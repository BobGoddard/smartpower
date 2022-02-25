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
with Linux_GPIO;
with Various;

package Smartmeter_Pulse is
   task type Pulse is
      entry Start (Pin : Linux_GPIO.Pin_Num; Pin_Red : Linux_GPIO.Pin_Num; Edge : Linux_GPIO.GPIO_REQUEST_EDGE_TYPE; Initial_Seconds : Interfaces.C.long; Initial_Count_l : Various.Counter_Type; Initial_30min_l : Various.Counter_30min_Type; Initial_Daily_l : Various.Counter_Day_Type);
      entry Kill_Me;
   end Pulse;
end Smartmeter_Pulse;
