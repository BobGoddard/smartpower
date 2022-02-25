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
with Ada.Calendar.Conversions;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Calendar.Time_IO;
with GNAT.Source_Info;
with Interfaces; use Interfaces;
with Interfaces.C.Strings;
with GNATCOLL.Traces.Syslog;
with CP_Cap;
with Counter;
with List_Handlers;
with List_Structure;
with String_Writer;
with Unix;

package body Smartmeter_Pulse is

   task body Pulse is
      subtype Ping_Type is Integer range 1 .. 32;
      Ping                  :          Ping_Type;
      LPin                  :          Linux_GPIO.Pin_Num;
      Num_Lines             : constant Interfaces.Unsigned_32 := 1;
      IOCTL_FD_Interrupt    :          Linux_GPIO.FD_Type;
      IOCTL_FD_LED          :          Linux_GPIO.FD_Type;
      LDev_Name             : constant String := "/dev/gpiochip0";
      Lines                 :          Linux_GPIO.Line_Offsets_Array;
      Flags                 : constant Linux_GPIO.Flags_Type := Linux_GPIO.GPIOHANDLE_REQUEST_OUTPUT;
      Handle_Data           : aliased  Linux_GPIO.GPIO_Handle_Data;
      Label_Pulse           : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("Smartmeter pulse");
      Label_Pin             : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("Smartmeter LED pin");
      Event_Data            : aliased  Linux_GPIO.GPIO_Event_Data;
      Do_Exit               :          Boolean := False;
      Count                 :          Various.Counter_Type;
      Current_Duration      :          Duration;
      Previous_Duration     :          Duration;
      Diff_Duration         :          Duration;
      TM_Details            :          Unix.tm; --  Only to get timezone abbreviation
      TS_Current            :          Unix.ts;
      Current_Time          :          Ada.Calendar.Time;
      NSecs                 :          Ada.Calendar.Formatting.Second_Duration;
      Time_Image            : constant GNAT.Calendar.Time_IO.Picture_String := "%a %b %d %T %Y ";
      Initial_30min         :          Various.Counter_30min_Type;
      Initial_Daily         :          Various.Counter_Day_Type;
      Data                  :          List_Structure.Pulse_Record;
      LEdge                 :          Linux_GPIO.GPIO_REQUEST_EDGE_TYPE;
      Signal                :          Boolean := False;
      Float_String          :          String := "9223372036854775808.123456789";
      Float_Unbounded       :          Ada.Strings.Unbounded.Unbounded_String;
   begin
      GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Smartmeter pulse task started." & GNAT.Source_Info.Source_Location);
      select
         accept Start (Pin             : Linux_GPIO.Pin_Num;
                       Pin_Red         : Linux_GPIO.Pin_Num;
                       Edge            : Linux_GPIO.GPIO_REQUEST_EDGE_TYPE;
                       Initial_Seconds : Interfaces.C.long;
                       Initial_Count_l : Various.Counter_Type;
                       Initial_30min_l : Various.Counter_30min_Type;
                       Initial_Daily_l : Various.Counter_Day_Type) do
            LPin                      := Pin;
            Lines (0)                 := Pin_Red;
            Handle_Data.Values (0)    := 0;
            LEdge                     := Edge;
            TS_Current.TV_Sec         := Initial_Seconds;
            TS_Current.TV_NSec        := 0;
            Count                     := Initial_Count_l;
            Initial_30min             := Initial_30min_l;
            Initial_Daily             := Initial_Daily_l;
         end Start;
      end select;

      Counter.Secure.Initialise_Counters (Count, Initial_30min, Initial_Daily);
      Previous_Duration := Ada.Calendar.Conversions.To_Duration (TS_Current.TV_Sec, TS_Current.TV_NSec);
      Linux_GPIO.Monitor_Device_Event_Open (LDev_Name, LPin, LEdge, Label_Pulse, IOCTL_FD_Interrupt);
      Linux_GPIO.Monitor_Device_Request_Open (LDev_Name, Lines, Num_Lines, Flags, Handle_Data, Label_Pin, IOCTL_FD_LED);

      Ping := Count mod 32 + 1;

      busy : loop
         select
            accept Kill_Me do
               Current_Time := Ada.Calendar.Clock;
               NSecs := Ada.Calendar.Formatting.Sub_Second (Current_Time);
               GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Task killme - " & Ada.Calendar.Formatting.Image (Current_Time) & " " & NSecs'Image & " - " & GNAT.Source_Info.Source_Location);
               Do_Exit := True;
            end Kill_Me;
         else
            Linux_GPIO.Monitor_Wait_For_Signal (Interfaces.C.int (IOCTL_FD_Interrupt), Event_Data);

            if Event_Data.ID = Linux_GPIO.GPIOEVENT_EVENT_RISING_EDGE then
               Counter.Secure.Increment_Counters;
               String_Writer.Secure.Write ("1");
               Handle_Data.Values (0) := 1;
               Linux_GPIO.Monitor_Set_Pins (IOCTL_FD_LED, Handle_Data);
               Signal := True;
            else
               Handle_Data.Values (0) := 0;
               Linux_GPIO.Monitor_Set_Pins (IOCTL_FD_LED, Handle_Data);
               Signal := False;

               if Ping = Ping_Type'Last then
                  Ping := Ping_Type'First;
               else
                  Ping := Ping + 1;
               end if;

               if Ping mod 4 = 0 then
                  String_Writer.Secure.Write (CP_Cap.CP_CAP_1 & "0" & CP_Cap.CP_RESETCAP);
                  Count := Counter.Secure.Get_Count;
                  Unix.Clock_Gettime (Unix.CLOCK_REALTIME, TS_Current'Address);
                  Data.Seconds      := TS_Current.TV_Sec;
                  Data.Nanoseconds  := TS_Current.TV_NSec;
                  Data.Count        := Count;
                  List_Handlers.Push (Data);

                  if Ping = Ping_Type'Last then
                     Unix.localtime_r (TS_Current'Address, TM_Details'Address);
                     Current_Duration := Ada.Calendar.Conversions.To_Duration (TS_Current.TV_Sec, TS_Current.TV_NSec);
                     Diff_Duration    := Current_Duration - Previous_Duration;
                     Current_Time     := Ada.Calendar.Conversions.To_Ada_Time (TS_Current.TV_Sec);

                     String_Writer.Secure.Write (CP_Cap.CP_COL70 & CP_Cap.CP_CAP_2);
                     String_Writer.Secure.Write ("COUNTER: " & Ada.Strings.Fixed.Overwrite ("         ", 9 - Count'Image'Length + 1, Count'Image));
                     String_Writer.Secure.Write (",  SECS: " & Current_Duration'Image);
                     String_Writer.Secure.Write (",  DATE: " & GNAT.Calendar.Time_IO.Image (Current_Time, Time_Image));
                     String_Writer.Secure.Write (Interfaces.C.Strings.Value (TM_Details.tm_zone));
                     String_Writer.Secure.Write (Interfaces.C.long (Ada.Calendar.Time_Zones.UTC_Time_Offset (Current_Time) * 60)'Image);
                     String_Writer.Secure.Write (",  DIFF: ");
                     Ada.Float_Text_IO.Put (Float_String, Float (Diff_Duration), Aft => 9, Exp => 0);
                     Float_Unbounded := Ada.Strings.Unbounded.Trim (Ada.Strings.Unbounded.To_Unbounded_String (Float_String), Ada.Strings.Left);

                     if Ada.Strings.Unbounded.Length (Float_Unbounded) < 15 then
                        Float_Unbounded := Ada.Strings.Unbounded.Head (Ada.Strings.Unbounded.To_Unbounded_String ("                 "), 15 - Ada.Strings.Unbounded.Length (Float_Unbounded)) & Float_Unbounded;
                     end if;

                     String_Writer.Secure.Write (Ada.Strings.Unbounded.To_String (Float_Unbounded));
                     String_Writer.Secure.Write (CP_Cap.CP_RESETCAP & ASCII.LF);
                     Previous_Duration := Current_Duration;
                  end if;
               else
                  String_Writer.Secure.Write ("0");
               end if;
            end if;
         end select;

         if Do_Exit and not Signal then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "Exit called on Smartmeter_Pulse." & GNAT.Source_Info.Source_Location);
            Linux_GPIO.Monitor_Device_Close (IOCTL_FD_Interrupt);
            Linux_GPIO.Monitor_Device_Close (IOCTL_FD_LED);
            exit busy;
         end if;
      end loop busy;

   exception
      when E : others => GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "What the heck just happened? - " & GNAT.Source_Info.Source_Location & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E) & ", Diff: " & Diff_Duration'Image);
   end Pulse;
end Smartmeter_Pulse;
