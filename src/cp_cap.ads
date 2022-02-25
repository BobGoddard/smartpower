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

package CP_Cap is

   RESET       : constant String := "0";
   BRIGHT      : constant String := "1";
   DIM         : constant String := "2";
   UNDERLINE   : constant String := "3";
   BLINK       : constant String := "4";
   REVERSED    : constant String := "7";
   HIDDEN      : constant String := "8";

   FOREGROUND  : constant String := "30";
   BACKGROUND  : constant String := "40";

   FBLACK       : constant String := "30";
   FRED         : constant String := "31";
   FGREEN       : constant String := "32";
   FYELLOW      : constant String := "33";
   FBLUE        : constant String := "34";
   FMAGENTA     : constant String := "35";
   FCYAN        : constant String := "36";
   FWHITE       : constant String := "37";

   BBLACK       : constant String := "40";
   BRED         : constant String := "41";
   BGREEN       : constant String := "42";
   BYELLOW      : constant String := "43";
   BBLUE        : constant String := "44";
   BMAGENTA     : constant String := "45";
   BCYAN        : constant String := "46";
   BWHITE       : constant String := "47";

   ESC_SEQ     : constant String := ASCII.ESC & "[";
   CP_SAVE     : constant String := ESC_SEQ   & "s";
   CP_RESTORE  : constant String := ESC_SEQ   & "u";
   CP_RESETCAP : constant String := ESC_SEQ   & "0m";
   CP_COL66    : constant String := ESC_SEQ   & "66G";
   CP_COL70    : constant String := ESC_SEQ   & "70G";
   CP_CAP_1    : constant String := ESC_SEQ   & BRIGHT & ";" & FCYAN   & ";" & BBLUE    & "m";
   CP_CAP_2    : constant String := ESC_SEQ   & BRIGHT & ";" & FCYAN   & ";" & BMAGENTA & "m";
   CP_CAP_3    : constant String := ESC_SEQ   & BRIGHT & ";" & FCYAN   & ";" & BGREEN   & "m";
   CP_CAP_4    : constant String := ESC_SEQ   & BRIGHT & ";" & FYELLOW & ";" & BMAGENTA & "m";

end CP_Cap;
