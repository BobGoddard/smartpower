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

with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded;

package Conversions is
   function Aft_Float_To_Integer (f : Float; A : Integer) return Integer;
   function Float_To_String      (f : Float; A : Integer) return String;
   function Float_To_UnBounded   (f : Float; A : Integer) return Ada.Strings.Unbounded.Unbounded_String;
   function Integer_To_String    (I : Integer)            return String;
   procedure Time_Diff (SecsCur  :     Integer; NSecsCur  :     Ada.Calendar.Formatting.Second_Duration;
                        SecsPre  :     Integer; NSecsPre  :     Ada.Calendar.Formatting.Second_Duration;
                        Secs_Res : out Integer; NSecs_Res : out Ada.Calendar.Formatting.Second_Duration);
end Conversions;
