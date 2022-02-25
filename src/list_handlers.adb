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

with Ada.Exceptions;
with GNAT.Source_Info;
with GNATCOLL.Traces.Syslog;

package body List_Handlers is
   no_data : exception;

   function Is_Pulse_30Min_Data_Available return Boolean is
   begin
      if Pulse_30Min_Record_Queue.Current_Use = 0 then
         return False;
      end if;

      return True;
   end Is_Pulse_30Min_Data_Available;

   function Is_Pulse_Daily_Data_Available return Boolean is
   begin
      if Pulse_Daily_Record_Queue.Current_Use = 0 then
         return False;
      end if;

      return True;
   end Is_Pulse_Daily_Data_Available;

   function Is_Pulse_Data_Available return Boolean is
   begin
      if Pulse_Record_Queue.Current_Use = 0 then
         return False;
      end if;

      return True;
   end Is_Pulse_Data_Available;

   function Is_Pulse_Restart_Data_Available return Boolean is
   begin
      if Pulse_Restart_Record_Queue.Current_Use = 0 then
         return False;
      end if;

      return True;
   end Is_Pulse_Restart_Data_Available;

   function Pop_Pulse return List_Structure.Pulse_Record is
      Tmp : List_Structure.Pulse_Record;
   begin
      if Pulse_Record_Queue.Current_Use = 0 then
         raise no_data;
      end if;

      Pulse_Record_Queue.Dequeue (Tmp);
      return Tmp;
   exception
      when error : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "pop exception raised " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (error));
         raise;
   end Pop_Pulse;

   function Pop_Pulse_30Min return List_Structure.Pulse_30Min_Record is
      Tmp : List_Structure.Pulse_30Min_Record;
   begin
      if Pulse_30Min_Record_Queue.Current_Use = 0 then
         raise no_data;
      end if;

      Pulse_30Min_Record_Queue.Dequeue (Tmp);
      return Tmp;
   exception
      when error : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "pop exception raised " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (error));
         raise;
   end Pop_Pulse_30Min;

   function Pop_Pulse_Daily return List_Structure.Pulse_Daily_Record is
      Tmp : List_Structure.Pulse_Daily_Record;
   begin
      if Pulse_Daily_Record_Queue.Current_Use = 0 then
         raise no_data;
      end if;

      Pulse_Daily_Record_Queue.Dequeue (Tmp);
      return Tmp;
   exception
      when error : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "pop exception raised " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (error));
         raise;
   end Pop_Pulse_Daily;

   function Pop_Pulse_Restart return List_Structure.Pulse_Restart_Record is
      Tmp : List_Structure.Pulse_Restart_Record;
   begin
      if Pulse_Restart_Record_Queue.Current_Use = 0 then
         raise no_data;
      end if;

      Pulse_Restart_Record_Queue.Dequeue (Tmp);
      return Tmp;
   exception
      when error : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "pop exception raised " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (error));
         raise;
   end Pop_Pulse_Restart;

   procedure Push (Data : List_Structure.Pulse_Record) is
   begin
      Pulse_Record_Queue.Enqueue (Data);
   exception
      when error : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "push exception raised " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (error));
         raise;
   end Push;

   procedure Push (Data : List_Structure.Pulse_30Min_Record) is
   begin
      Pulse_30Min_Record_Queue.Enqueue (Data);
   exception
      when error : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "push exception raised " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (error));
         raise;
   end Push;

   procedure Push (Data : List_Structure.Pulse_Daily_Record) is
   begin
      Pulse_Daily_Record_Queue.Enqueue (Data);
   exception
      when error : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "push exception raised " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (error));
         raise;
   end Push;

   procedure Push (Data : List_Structure.Pulse_Restart_Record) is
   begin
      Pulse_Restart_Record_Queue.Enqueue (Data);
   exception
      when error : others =>
         GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, "push exception raised " & GNAT.Source_Info.Source_Location & " " & Ada.Exceptions.Exception_Name (error));
         raise;
   end Push;
end List_Handlers;
