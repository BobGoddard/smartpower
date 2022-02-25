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

with Ada.Calendar;
with Ada.Calendar.Formatting;
with GNAT.Source_Info;
with GNATCOLL.Traces.Syslog;
with DB_Routines;
with Linux_GPIO;
with List_Handlers;
with List_Structure;
with Local_Defs; use Local_Defs;

package body DB_Update_Task is

   task body Update is
      NTime        : Ada.Calendar.Time;
      NSecs        : Ada.Calendar.Formatting.Second_Duration;
      Data_Period  : List_Structure.Pulse_Record;
      Data_30min   : List_Structure.Pulse_30Min_Record;
      Data_Daily   : List_Structure.Pulse_Daily_Record;
      Data_Restart : List_Structure.Pulse_Restart_Record;
      Do_Exit      : Boolean := False;
      Res          : Boolean;
      Res_Conn     : Local_Defs.Trilean;
   begin
      select
         accept Start;
      end select;

      busy : loop
         select
            accept Kill_Me do
               NTime := Ada.Calendar.Clock;
               NSecs := Ada.Calendar.Formatting.Sub_Second (NTime);
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Task killme - " & Ada.Calendar.Formatting.Image (NTime) & " " & NSecs'Image & " - " & GNAT.Source_Info.Source_Location);
               Do_Exit := True;
            end Kill_Me;
         else
            if List_Handlers.Is_Pulse_Data_Available then
               Data_Period := List_Handlers.Pop_Pulse;
               Res := DB_Routines.Insert_Count (Data_Period.Seconds, Data_Period.Nanoseconds, Data_Period.Count);

               if Res = False then
                  DB_Routines.DB_Disconnect;
                  Res_Conn := DB_Routines.DB_Connect;
                  if Res_Conn = TFalse or else Res_Conn = TBroken then
                     Linux_GPIO.Monitor_CTRL_C_Called;
                     exit busy;
                  end if;
               end if;
            end if;

            if List_Handlers.Is_Pulse_30Min_Data_Available then
               Data_30min := List_Handlers.Pop_Pulse_30Min;
               Res := DB_Routines.Insert_30Min_Count (Data_30min.Seconds, Data_30min.Nanoseconds, Data_30min.Count);

               if Res = False then
                  DB_Routines.DB_Disconnect;
                  Res_Conn := DB_Routines.DB_Connect;
                  if Res_Conn = TFalse or else Res_Conn = TBroken then
                     Linux_GPIO.Monitor_CTRL_C_Called;
                     exit busy;
                  end if;
               end if;
            end if;

            if List_Handlers.Is_Pulse_Daily_Data_Available then
               Data_Daily := List_Handlers.Pop_Pulse_Daily;
               Res := DB_Routines.Insert_Daily_Count (Data_Daily.Seconds, Data_Daily.Nanoseconds, Data_Daily.Count);

               if Res = False then
                  DB_Routines.DB_Disconnect;
                  Res_Conn := DB_Routines.DB_Connect;
                  if Res_Conn = TFalse or else Res_Conn = TBroken then
                     Linux_GPIO.Monitor_CTRL_C_Called;
                     exit busy;
                  end if;
               end if;
            end if;

            if List_Handlers.Is_Pulse_Restart_Data_Available then
               Data_Restart := List_Handlers.Pop_Pulse_Restart;
               Res := DB_Routines.Insert_Restart_Count (Data_Restart.Seconds, Data_Restart.Nanoseconds, Data_Restart.Count, Data_Restart.Hourly, Data_Restart.Daily, Data_Restart.Mode);

               if Res = False then
                  DB_Routines.DB_Disconnect;
                  Res_Conn := DB_Routines.DB_Connect;
                  if Res_Conn = TFalse or else Res_Conn = TBroken then
                     Linux_GPIO.Monitor_CTRL_C_Called;
                     exit busy;
                  end if;
               end if;
            end if;
         end select;

         if Do_Exit then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Task exiting - " & GNAT.Source_Info.Source_Location);
            exit busy;
         end if;

         delay 0.9;
      end loop busy;
   end Update;
end DB_Update_Task;
