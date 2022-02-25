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

with Interfaces.C;
with GNAT.Ctrl_C;
with GNAT.Source_Info;
with GNATCOLL.Traces.Syslog;
with Counter;
with DB_Update_Task;
with DB_Routines;
with Linux_GPIO;
with List_Handlers;
with List_Structure;
with Periodic_30m_Pulse;
with Signal_Handler_Package;
with Smartmeter_Pulse;
with Unix;
with Ada.Directories;          use Ada.Directories;
with Ada.Strings.Unbounded;
with GNAT.Command_Line;
with Config_Handler;

pragma Unreserve_All_Interrupts;

procedure Smartpower is
   type Pulse_30M_Task_Type        is new Periodic_30m_Pulse.Pulse;
   type Smartmeter_Pulse_Task_Type is new Smartmeter_Pulse.Pulse;
   type DB_Update_Task_Type        is new DB_Update_Task.Update;
   DBUpdate_Task   : DB_Update_Task_Type;
   Pulse_30M_Task  : Pulse_30M_Task_Type;
   Smartmeter_Task : Smartmeter_Pulse_Task_Type;
   S               : Interfaces.C.long := 0;
   N               : Interfaces.C.long := 0;
   C               : Integer := 0;
   H               : Integer := 0;
   D               : Integer := 0;
   M               : Character  := ' ';
   TS              : Unix.ts;
   Tmp_Restart     : List_Structure.Pulse_Restart_Record;
   Tmp_Period      : Duration;
   XML_Settings    : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("/etc/smartpower.xml");
   CMD_Args        : exception;
   Program_Failure : exception;
begin
   GNATCOLL.Traces.Syslog.Register_Syslog_Stream;
   GNATCOLL.Traces.Syslog.Openlog (GNAT.Source_Info.File, GNATCOLL.Traces.Syslog.None, GNATCOLL.Traces.Syslog.Daemon);
   GNAT.Ctrl_C.Install_Handler (Linux_GPIO.Monitor_CTRL_C_Called'Access);

   loop
      case GNAT.Command_Line.Getopt ("i:") is
         when 'i' =>
            XML_Settings := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Command_Line.Parameter);
         when others =>
            exit;
      end case;
   end loop;

   if not Ada.Directories.Exists (Ada.Strings.Unbounded.To_String (XML_Settings)) then
      raise CMD_Args with "Problem reading " & Ada.Strings.Unbounded.To_String (XML_Settings);
   end if;

   if Ada.Directories.Kind (Ada.Strings.Unbounded.To_String (XML_Settings)) /= Ada.Directories.Ordinary_File then
      raise CMD_Args with "Config file does not appear to be an ordinary file";
   end if;

   Config_Handler.Set_Config_Name (XML_Settings);
   Config_Handler.Load_Config;

   if DB_Routines.Get_Restart_Count (S, N, C, H, D, M) = False then
      return;
   end if;

   Unix.Clock_Gettime (Unix.CLOCK_REALTIME, TS'Address);
   GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "S: " & S'Image & ", N:" & N'Image & ", C: " & C'Image & ", H: " & H'Image & ", D: " & D'Image & ", M: " & M & " " & GNAT.Source_Info.Source_Location);
   Pulse_30M_Task.Start;
   Smartmeter_Task.Start (27, 22, Linux_GPIO.GPIOEVENT_REQUEST_BOTH_EDGES, S, C, H, D);
   Tmp_Restart.Seconds     := TS.TV_Sec;
   Tmp_Restart.Nanoseconds := TS.TV_NSec;
   Tmp_Restart.Count       := Counter.Secure.Get_Count;
   Tmp_Restart.Hourly      := Counter.Secure.Get_Count_30min;
   Tmp_Restart.Daily       := Counter.Secure.Get_Count_Day;
   Tmp_Restart.Mode        := '1';
   List_Handlers.Push (Tmp_Restart);
   DBUpdate_Task.Start;
   Signal_Handler_Package.Signal_Handler.Setup;

   while not Signal_Handler_Package.Exit_Program loop
      if Pulse_30M_Task'Terminated then
         Smartmeter_Task.Kill_Me;
         raise Program_Failure;
      end if;

      if Smartmeter_Task'Terminated then
         Pulse_30M_Task.Kill_Me;
         raise Program_Failure;
      end if;

      delay 0.1;
   end loop;

   if not Pulse_30M_Task'Terminated then
      Pulse_30M_Task.Kill_Me;
   end if;

   if not Smartmeter_Task'Terminated then
      Smartmeter_Task.Kill_Me;
   end if;

   Unix.Clock_Gettime (Unix.CLOCK_REALTIME, TS'Address);
   Tmp_Restart.Seconds     := TS.TV_Sec;
   Tmp_Restart.Nanoseconds := TS.TV_NSec;
   Tmp_Restart.Count       := Counter.Secure.Get_Count;
   Tmp_Restart.Hourly      := Counter.Secure.Get_Count_30min;
   Tmp_Restart.Daily       := Counter.Secure.Get_Count_Day;
   Tmp_Restart.Mode        := '0';
   GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Pushing Tmp_Restart - " & Tmp_Restart.Mode);
   List_Handlers.Push (Tmp_Restart);
   Tmp_Period := 1.0;

   check_pulse_data : loop
      if not List_Handlers.Is_Pulse_Data_Available then
         exit check_pulse_data;
      end if;

      if Tmp_Period > 65536.0 then
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Unable to write to database pulse data, shutting down anyway - " & GNAT.Source_Info.Source_Location);
         exit check_pulse_data;
      end if;

      delay Tmp_Period;
      Tmp_Period := Tmp_Period * 2.0;
   end loop check_pulse_data;

   if Tmp_Period <= 65536.0 then
      Tmp_Period := 1.0;

      check_30min_data : loop
         if not List_Handlers.Is_Pulse_30Min_Data_Available then
            exit check_30min_data;
         end if;

         if Tmp_Period > 65536.0 then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Unable to write to database pulse 30min data, shutting down anyway - " & GNAT.Source_Info.Source_Location);
            exit check_30min_data;
         end if;

         delay Tmp_Period;
         Tmp_Period := Tmp_Period * 2.0;
      end loop check_30min_data;
   end if;

   if Tmp_Period <= 65536.0 then
      Tmp_Period := 1.0;

      check_restart_data : loop
         if not List_Handlers.Is_Pulse_Restart_Data_Available then
            exit check_restart_data;
         end if;

         if Tmp_Period > 65536.0 then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Unable to write to database restart data, shutting down anyway - " & GNAT.Source_Info.Source_Location);
            exit check_restart_data;
         end if;

         delay Tmp_Period;
         Tmp_Period := Tmp_Period * 2.0;
      end loop check_restart_data;
   end if;

   if not DBUpdate_Task'Terminated then
      DBUpdate_Task.Kill_Me;
   end if;

   DB_Routines.DB_Disconnect;
end Smartpower;
