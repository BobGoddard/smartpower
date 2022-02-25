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

with Ada.Containers.Synchronized_Queue_Interfaces; use Ada.Containers;
with Ada.Containers.Unbounded_Synchronized_Queues;
with List_Structure;

package List_Handlers is
   package Pulse_Record_Map_Interface         is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type     => List_Structure.Pulse_Record);
   package Pulse_Record_Map                   is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Pulse_Record_Map_Interface);

   package Pulse_30Min_Record_Map_Interface   is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type     => List_Structure.Pulse_30Min_Record);
   package Pulse_30Min_Record_Map             is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Pulse_30Min_Record_Map_Interface);

   package Pulse_Daily_Record_Map_Interface   is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type     => List_Structure.Pulse_Daily_Record);
   package Pulse_Daily_Record_Map             is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Pulse_Daily_Record_Map_Interface);

   package Pulse_Restart_Record_Map_Interface is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type     => List_Structure.Pulse_Restart_Record);
   package Pulse_Restart_Record_Map           is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Pulse_Restart_Record_Map_Interface);

   function Is_Pulse_30Min_Data_Available   return Boolean;
   function Is_Pulse_Data_Available         return Boolean;
   function Is_Pulse_Daily_Data_Available   return Boolean;
   function Is_Pulse_Restart_Data_Available return Boolean;

   function Pop_Pulse         return List_Structure.Pulse_Record;
   function Pop_Pulse_30Min   return List_Structure.Pulse_30Min_Record;
   function Pop_Pulse_Daily   return List_Structure.Pulse_Daily_Record;
   function Pop_Pulse_Restart return List_Structure.Pulse_Restart_Record;

   procedure Push (Data : List_Structure.Pulse_Record);
   procedure Push (Data : List_Structure.Pulse_30Min_Record);
   procedure Push (Data : List_Structure.Pulse_Daily_Record);
   procedure Push (Data : List_Structure.Pulse_Restart_Record);
private
   Pulse_Record_Queue         : Pulse_Record_Map.Queue;
   Pulse_30Min_Record_Queue   : Pulse_30Min_Record_Map.Queue;
   Pulse_Daily_Record_Queue   : Pulse_Daily_Record_Map.Queue;
   Pulse_Restart_Record_Queue : Pulse_Restart_Record_Map.Queue;
end List_Handlers;
