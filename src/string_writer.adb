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

with Ada.Strings.Unbounded;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Source_Info;
with GNATCOLL.Terminal;
with GNATCOLL.Traces.Syslog;
with Interfaces; use Interfaces;
with Unix;

package body String_Writer is
   Output_File : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Null_FD;
   Res         : Boolean;

   procedure Close_File is
   begin
      GNAT.OS_Lib.Close (Output_File, Res);

      if not Res then
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Close error on log file." & GNAT.Source_Info.Source_Location);
      end if;
   end Close_File;

   procedure Open_File is
      FName       : Ada.Strings.Unbounded.Unbounded_String;
      TermInfo    : GNATCOLL.Terminal.Terminal_Info;
   begin
      if Unix.getuid = 0 then
         FName := Ada.Strings.Unbounded.To_Unbounded_String ("/root/power_smart.txt");
      else
         FName := Ada.Strings.Unbounded.To_Unbounded_String ("/home/pi/power_smart.txt");
      end if;

      GNATCOLL.Terminal.Init_For_File   (TermInfo, Colors => GNATCOLL.Terminal.Yes);
      GNATCOLL.Terminal.Init_For_Stdout (TermInfo, Colors => GNATCOLL.Terminal.Yes);

      --  Can't be bothered fixing this...
      if TermInfo.Has_ANSI_Colors then
         null;
      end if;

      if GNAT.OS_Lib.Is_Write_Accessible_File (Ada.Strings.Unbounded.To_String (FName)) then
         Output_File := GNAT.OS_Lib.Open_Append (Ada.Strings.Unbounded.To_String (FName), GNAT.OS_Lib.Binary);
      else
         Output_File := GNAT.OS_Lib.Create_File (Ada.Strings.Unbounded.To_String (FName), GNAT.OS_Lib.Binary);
      end if;
   end Open_File;

   protected body Secure is
      procedure Write (S : String) is
         Ret : Integer;
      begin
         if Output_File = GNAT.OS_Lib.Null_FD then
            Open_File;
         end if;

         Ret := GNAT.OS_Lib.Write (Output_File, S'Address, S'Length);

         if Ret /= S'Length then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Did not write all data to file." & GNAT.Source_Info.Source_Location);
         end if;
      end Write;
   end Secure;
end String_Writer;
