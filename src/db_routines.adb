--  Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
--  This file is free software: you may copy, redistribute and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 2 of the License, or (at your
--  option) any later version.
--
--  This file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Calendar.Formatting;
with Ada.Exceptions;
with AdaBase;
with AdaBase.Results;
with AdaBase.Results.Sets;
with AdaBase.Statement;
with Ada.Calendar; use Ada.Calendar;
with GNAT.Source_Info;
with GNATCOLL.Traces.Syslog;
with Linux_GPIO;

package body DB_Routines is
   DR      : AdaBase.Driver.Base.MySQL.MySQL_Driver;

   function DB_Connect return Local_Defs.Trilean is
      delay_time_max   : constant Duration := 65536.0;
      delay_time       : Duration := 1.0;
      delay_time_count : Duration := 0.0;
      target_time      : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      DB_Disconnect;
      delay_loop :
      loop
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Contacting DB, delaying until " & Ada.Calendar.Formatting.Image (target_time) & " - " & GNAT.Source_Info.Source_Location);
         delay until target_time;

         if Linux_GPIO.Monitor_CTRL_C_Is_Called = True or else (DB_Connect_Private = Local_Defs.TFalse and then delay_time_count >= delay_time_max) then
            return Local_Defs.TFalse;
         end if;

         if DB_Connect_Private = Local_Defs.TTrue then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Connected - " & GNAT.Source_Info.Source_Location);
            return Local_Defs.TTrue;
         end if;

         if delay_time_count = delay_time then
            delay_time := delay_time * 2.0;
            delay_time_count := 0.0;
         else
            delay_time := delay_time + 1.0;
         end if;

         target_time := target_time + delay_time;
         delay_time_count := delay_time_count + 1.0;
      end loop delay_loop;
   end DB_Connect;

   function DB_Connect_Private return Local_Defs.Trilean is
   begin
      DR.basic_connect (database => Ada.Strings.Unbounded.To_String (Database),
                        username => Ada.Strings.Unbounded.To_String (DB_User),
                        password => Ada.Strings.Unbounded.To_String (DB_Pass),
                        hostname => Ada.Strings.Unbounded.To_String (DB_Host),
                        port     => DB_Port);

      return Local_Defs.TTrue;

   exception
      when E : others => GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         return Local_Defs.TBroken;
   end DB_Connect_Private;

   procedure DB_Disconnect is
   begin
      DR.disconnect;
   end DB_Disconnect;

   function Get_Restart_Count (Secs : out Interfaces.C.long; Nano_Secs : out Interfaces.C.long; Count : out Integer; Hourly : out Integer; Daily : out Integer; Restart_Mode : out Character) return Boolean is
      Row           : AdaBase.Results.Sets.Datarow;
      Get_Count_SQL : constant String := "SELECT seconds, nanoseconds, count, hourly, daily, mode FROM restart WHERE seconds=(SELECT MAX(seconds) FROM restart ORDER BY seconds, nanoseconds, count DESC LIMIT 1) ORDER BY seconds DESC, nanoseconds DESC, count DESC LIMIT 1";
   begin
      if DB_Connect /= Local_Defs.TTrue then
         return False;
      end if;

      declare
         STMT : Stmt_Type_Local := DR.query (Get_Count_SQL);
      begin
         Row      := STMT.fetch_next;
      end;

      Secs         := Interfaces.C.long (Row.column (1).as_byte8);
      Nano_Secs    := Interfaces.C.long (Row.column (2).as_byte8);
      Count        := Integer   (Row.column (3).as_byte8);
      Hourly       := Integer   (Row.column (4).as_byte8);
      Daily        := Integer   (Row.column (5).as_byte8);
      Restart_Mode :=            Row.column (6).as_string (1);
      return True;
   exception
      when E : others => GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         return False;
   end Get_Restart_Count;

   function Insert_30Min_Count (Secs : Interfaces.C.long; Nano_Secs : Interfaces.C.long; Count : Integer) return Boolean is
      Insert_30Min_Count_SQL : constant String := "INSERT INTO hourly VALUES (" & Secs'Image & "," & Nano_Secs'Image & "," & Count'Image & ")";
   begin
      declare
         STMT : Stmt_Type_Local := DR.prepare (Insert_30Min_Count_SQL);
      begin
         if STMT.execute then
            DR.commit;
         else
            DR.rollback;
            return False;
         end if;
      end;

      return True;
   exception
      when E : others => GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         return False;
   end Insert_30Min_Count;

   function Insert_Count (Secs : Interfaces.C.long; Nano_Secs : Interfaces.C.long; Count : Integer) return Boolean is
      Insert_Count_SQL : constant String := "INSERT INTO count VALUES (" & Secs'Image & "," & Nano_Secs'Image & "," & Count'Image & ")";
   begin
      declare
         STMT : Stmt_Type_Local := DR.prepare (Insert_Count_SQL);
      begin
         if STMT.execute then
            DR.commit;
         else
            DR.rollback;
            return False;
         end if;
      end;

      return True;
   exception
      when E : others => GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         return False;
   end Insert_Count;

   function Insert_Daily_Count (Secs : Interfaces.C.long; Nano_Secs : Interfaces.C.long; Count : Integer) return Boolean is
      Insert_Daily_Count_SQL : constant String := "INSERT INTO daily VALUES (" & Secs'Image & "," & Nano_Secs'Image & "," & Count'Image & ")";
   begin
      declare
         STMT : Stmt_Type_Local := DR.prepare (Insert_Daily_Count_SQL);
      begin
         if STMT.execute then
            DR.commit;
         else
            DR.rollback;
            return False;
         end if;
      end;

      return True;
   exception
      when E : others => GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         return False;
   end Insert_Daily_Count;

   function Insert_Restart_Count (Secs : Interfaces.C.long; Nano_Secs : Interfaces.C.long; Count : Integer; Hourly : Integer; Daily : Integer; Restart_Mode : Character) return Boolean is
      Insert_Restart_SQL : constant String := "INSERT INTO restart VALUES (" & Secs'Image & "," & Nano_Secs'Image & "," & Count'Image & "," & Hourly'Image & "," & Daily'Image & ",'" & Restart_Mode & "')";
   begin
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "INSERT INTO restart VALUES (" & Secs'Image & "," & Nano_Secs'Image & "," & Count'Image & "," & Hourly'Image & "," & Daily'Image & ",'" & Restart_Mode & "')");
      declare
         STMT : Stmt_Type_Local := DR.prepare (Insert_Restart_SQL);
      begin
         if STMT.execute then
            DR.commit;
         else
            DR.rollback;
            return False;
         end if;
      end;

      return True;
   exception
      when E : others => GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "WTF!! rivet " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
         return False;
   end Insert_Restart_Count;

   procedure Set_Account_Details (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306) is
   begin
      Database := DB;
      DB_Host  := Host;
      DB_User  := User;
      DB_Pass  := Pass;
      DB_Port  := Port;
   end Set_Account_Details;
end DB_Routines;
