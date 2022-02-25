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

with Various;

package Counter is
   protected Secure is
      procedure Get           (C : out Various.Counter_Type; C30 : out Various.Counter_30min_Type; CDay : out Various.Counter_Day_Type; Reset_Count_30min : Boolean := False; Reset_Count_Day : Boolean := False);
      function  Get_Count       return Various.Counter_Type;
      function  Get_Count_30min return Various.Counter_30min_Type;
      function  Get_Count_Day   return Various.Counter_Day_Type;
      procedure Increment_Counters;
      procedure Initialise_Counters (Current : Various.Counter_Type := 0; Current_30min : Various.Counter_30min_Type := 0; Current_daily : Various.Counter_Day_Type := 0);
   private
      Count       : Various.Counter_Type;
      Count_30min : Various.Counter_30min_Type;
      Count_Day   : Various.Counter_Day_Type;
   end Secure;
end Counter;
