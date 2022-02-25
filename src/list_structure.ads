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

package List_Structure is
   type Pulse_Record is record
      Seconds     : Interfaces.C.long;
      Nanoseconds : Interfaces.C.long;
      Count       : Integer;
   end record;

   type Pulse_30Min_Record is record
      Seconds     : Interfaces.C.long;
      Nanoseconds : Interfaces.C.long;
      Count       : Integer;
   end record;

   type Pulse_Daily_Record is record
      Seconds     : Interfaces.C.long;
      Nanoseconds : Interfaces.C.long;
      Count       : Integer;
   end record;

   type Pulse_Restart_Record is record
      Seconds     : Interfaces.C.long;
      Nanoseconds : Interfaces.C.long;
      Count       : Integer;
      Hourly      : Integer;
      Daily       : Integer;
      Mode        : Character;
   end record;
end List_Structure;

--  MariaDB [power_smart]> describe restart;
--  +-------------+------------+------+-----+---------+-------+
--  | Field       | Type       | Null | Key | Default | Extra |
--  +-------------+------------+------+-----+---------+-------+
--  | seconds     | bigint(20) | NO   | PRI | NULL    |       |
--  | nanoseconds | bigint(20) | NO   | PRI | NULL    |       |
--  | count       | bigint(20) | NO   |     | NULL    |       |
--  | hourly      | int(11)    | NO   |     | NULL    |       |
--  | daily       | int(11)    | NO   |     | NULL    |       |
--  | mode        | char(1)    | NO   |     | NULL    |       |
--  +-------------+------------+------+-----+---------+-------+
--  6 rows in set (0.00 sec)
--
--  MariaDB [power_smart]> describe count;
--  +-------------+------------+------+-----+---------+-------+
--  | Field       | Type       | Null | Key | Default | Extra |
--  +-------------+------------+------+-----+---------+-------+
--  | seconds     | bigint(20) | NO   | PRI | NULL    |       |
--  | nanoseconds | bigint(20) | NO   | PRI | NULL    |       |
--  | count       | bigint(20) | NO   |     | NULL    |       |
--  +-------------+------------+------+-----+---------+-------+
