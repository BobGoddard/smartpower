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

with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Conversions;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with GNAT.Calendar.Time_IO;
with GNAT.Source_Info;
with GNATCOLL.Traces.Syslog;
with GNATCOLL.Terminal;
with Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with CP_Cap; use CP_Cap;
with Conversions;
with Counter;
with List_Handlers;
with List_Structure;
with String_Writer;
with Unix; use Unix;
with Various;

package body Periodic_30m_Pulse is

   task body Pulse is
      Do_Exit          : Boolean := False;
      NTime            : Ada.Calendar.Time;
      NSecs            : Ada.Calendar.Formatting.Second_Duration;
      T_Long           : constant Duration := 30.0 * 60.0;
      T_Short          : constant Duration := 1.0;
      Base_Time        : Ada.Calendar.Time;
      Current_Time     : Ada.Calendar.Time;
      Target_Time      : Ada.Calendar.Time;
      Sec_Unix         : Interfaces.C.long;
      TM_Details       : Unix.tm;
      TS_Current       : Unix.ts;
      HType            : String := " ";
      TZone_Offset     : Interfaces.C.long;
      terminfo         : GNATCOLL.Terminal.Terminal_Info;
      Count            : Various.Counter_Type       := 0;
      Count_30mins     : Various.Counter_30min_Type := 0;
      Count_Day        : Various.Counter_Day_Type   := 0;
      Count_30Min_kWh  : Float;
      Count_Day_kWh    : Float;
      Time_Image       : constant GNAT.Calendar.Time_IO.Picture_String := "%a %b %d %T %Y ";
      Current_Duration : Duration;
      Data             : List_Structure.Pulse_30Min_Record;
      Data_Daily       : List_Structure.Pulse_Daily_Record;
   begin
      select
         accept Start;
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Task started - " & GNAT.Source_Info.Source_Location);
      end select;

      Current_Time := Ada.Calendar.Clock;
      Sec_Unix     := Ada.Calendar.Conversions.To_Unix_Time (Current_Time);
      Current_Time := Ada.Calendar.Conversions.To_Ada_Time (Sec_Unix);
      Base_Time    := Ada.Calendar.Conversions.To_Ada_Time (Sec_Unix - Interfaces.C.long (Integer (Sec_Unix) mod Integer (T_Long)));
      Current_Time := Current_Time - T_Short * 10.0;
      Target_Time  := Base_Time + T_Long;
      GNATCOLL.Terminal.Init_For_Stdout (terminfo, Colors => GNATCOLL.Terminal.Yes);

      busy : loop
         select
            accept Kill_Me  do
               NTime := Ada.Calendar.Clock;
               NSecs := Ada.Calendar.Formatting.Sub_Second (NTime);
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Task killme - " & Ada.Calendar.Formatting.Image (NTime) & " " & NSecs'Image & " - " & GNAT.Source_Info.Source_Location);
               Do_Exit := True;
            end Kill_Me;
         else
            if Current_Time = Target_Time then
               TZone_Offset := Interfaces.C.long (Ada.Calendar.Time_Zones.UTC_Time_Offset (Current_Time) * 60);
               Unix.Clock_Gettime (Unix.CLOCK_REALTIME, TS_Current'Address);
               Unix.localtime_r (TS_Current'Address, TM_Details'Address);
               Current_Duration := Ada.Calendar.Conversions.To_Duration (TS_Current.TV_Sec, TS_Current.TV_NSec);

               if (TS_Current.TV_Sec + TZone_Offset) mod (24 * 60 * 60) = 0 then
                  Counter.Secure.Get (Count, Count_30mins, Count_Day, Reset_Count_Day => True);
                  Data_Daily.Seconds     := TS_Current.TV_Sec;
                  Data_Daily.Nanoseconds := TS_Current.TV_NSec;
                  Data_Daily.Count       := Count_Day;
                  List_Handlers.Push (Data_Daily);
                  HType := "D";
               elsif (TS_Current.TV_Sec + TZone_Offset) mod (30 * 60) = 0 then
                  Counter.Secure.Get (Count, Count_30mins, Count_Day, Reset_Count_30min => True);
                  if (TS_Current.TV_Sec + TZone_Offset) mod (60 * 60) = 0 then
                     HType := "H";
                  else
                     HType := "3";
                  end if;
               else
                  GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Unknown timezone information - Interfaces.C.long (TZone_Offset): " & TZone_Offset'Image & GNAT.Source_Info.Source_Location);
                  HType := "?";
               end if;

               Count_30Min_kWh := Float (Count_30mins) / 3200.0;
               Count_Day_kWh   := Float (Count_Day) / 3200.0;
               String_Writer.Secure.Write (CP_SAVE & CP_COL66 & CP_CAP_4 &
                                           HType &
                                           "   COUNTER: "        & Ada.Strings.Fixed.Overwrite ("         ", 9 - Count'Image'Length + 1, Count'Image)  &
                                           ",  SECS: "           & Current_Duration'Image &
                                           ",  DATE: "           & GNAT.Calendar.Time_IO.Image (Current_Time, Time_Image) &
                                                                   Interfaces.C.Strings.Value (TM_Details.tm_zone) &
                                                                   TZone_Offset'Image &
                                           ",  TOTAL IN 30MIN: " & Count_30mins'Image & " (" & Conversions.Float_To_String (Count_30Min_kWh, 7) & "kWh)" &
                                           ",  TOTAL IN DAY:   " & Count_Day'Image & " (" & Conversions.Float_To_String (Count_Day_kWh, 7) & "kWh)" &
                                           CP_RESETCAP & ASCII.LF & CP_RESTORE);
               Target_Time := Target_Time + T_Long;
               Data.Seconds := TS_Current.TV_Sec;
               Data.Nanoseconds := TS_Current.TV_NSec;
               Data.Count := Count_30mins;
               List_Handlers.Push (Data);
            else
               Current_Time := Current_Time + T_Short;
               delay until Current_Time;
            end if;
         end select;

         if Do_Exit then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Task exiting - " & GNAT.Source_Info.Source_Location);
            exit busy;
         end if;
      end loop busy;
   exception
      when E : others => GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "What the heck happened? - " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
   end Pulse;
end Periodic_30m_Pulse;
