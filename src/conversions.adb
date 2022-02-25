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

with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings;
with Ada.Calendar;

package body Conversions is
   function Aft_Float_To_Integer (f : Float; A : Integer) return Integer is
      US    : Ada.Strings.Unbounded.Unbounded_String;
      S     : String := "                          ";
      LSet1 : constant Ada.Strings.Maps.Character_Set := To_Set (' ');
      RSet1 : constant Ada.Strings.Maps.Character_Set := To_Set ("0");
      RSet2 : constant Ada.Strings.Maps.Character_Set := To_Set ('.');
   begin
      Ada.Float_Text_IO.Put (S, f, Aft => A, Exp => 0);
      US := Ada.Strings.Unbounded.To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet1);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet2);
      return 44; --  Ada.Strings.Unbounded.To_String (US);
   end Aft_Float_To_Integer;

   function Float_To_String (f : Float; A : Integer) return String is
      US : Ada.Strings.Unbounded.Unbounded_String;
      S  : String := "                          ";
      LSet1 : constant Ada.Strings.Maps.Character_Set := To_Set (' ');
      RSet1 : constant Ada.Strings.Maps.Character_Set := To_Set ("0");
      RSet2 : constant Ada.Strings.Maps.Character_Set := To_Set ('.');
   begin
      Ada.Float_Text_IO.Put (S, f, Aft => A, Exp => 0);
      US := Ada.Strings.Unbounded.To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet1);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet2);
      return Ada.Strings.Unbounded.To_String (US);
   end Float_To_String;

   function Float_To_UnBounded (f : Float; A : Integer) return Ada.Strings.Unbounded.Unbounded_String is
      US    : Ada.Strings.Unbounded.Unbounded_String;
      S     : String := "                          ";
      LSet1 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (' ');
      RSet1 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("0");
      RSet2 : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ('.');
   begin
      Ada.Float_Text_IO.Put (S, f, Aft => A, Exp => 0);
      US := Ada.Strings.Unbounded.To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet1);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet2);
      return US;
   end Float_To_UnBounded;

   function Integer_To_String (I : Integer) return String is
      US : Ada.Strings.Unbounded.Unbounded_String;
      S  : String := "                          ";
   begin
      Ada.Integer_Text_IO.Put (S, I);
      US := Ada.Strings.Unbounded.To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, Ada.Strings.Both);
      return Ada.Strings.Unbounded.To_String (US);
   end Integer_To_String;

   procedure Time_Diff (SecsCur : Integer; NSecsCur : Ada.Calendar.Formatting.Second_Duration; SecsPre : Integer; NSecsPre : Ada.Calendar.Formatting.Second_Duration; Secs_Res : out Integer; NSecs_Res : out Ada.Calendar.Formatting.Second_Duration) is
   begin
      Secs_Res := SecsCur - SecsPre;

      if NSecsCur >= NSecsPre then
         NSecs_Res := NSecsCur - NSecsPre;
      else
         Secs_Res := Secs_Res - 1;
         NSecs_Res := 1.0 - NSecsPre + NSecsCur;
      end if;
   end Time_Diff;
end Conversions;
