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

with System;
with GNAT.OS_Lib;
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

package Unix is
   subtype Ptr_Array is Interfaces.C.Strings.chars_ptr_array (0 .. 1);
   subtype clockid_t is Interfaces.C.int range 0 .. 7;
   type Ptr_Ptr_Array is access all Ptr_Array;

   CLOCK_REALTIME           : constant clockid_t := 0;
   CLOCK_REALTIME_COARSE    : constant clockid_t := 1;
   CLOCK_MONOTONIC          : constant clockid_t := 2;
   CLOCK_MONOTONIC_COARSE   : constant clockid_t := 3;
   CLOCK_MONOTONIC_RAW      : constant clockid_t := 4;
   CLOCK_BOOTTIME           : constant clockid_t := 5;
   CLOCK_PROCESS_CPUTIME_ID : constant clockid_t := 6;
   CLOCK_THREAD_CPUTIME_ID  : constant clockid_t := 7;

   type tm is record
      tm_sec    : Interfaces.C.int;
      tm_min    : Interfaces.C.int;
      tm_hour   : Interfaces.C.int;
      tm_day    : Interfaces.C.int;
      tm_mon    : Interfaces.C.int;
      tm_year   : Interfaces.C.int;
      tm_wday   : Interfaces.C.int;
      tm_yday   : Interfaces.C.int;
      tm_isdst  : Interfaces.C.int;
      tm_gmtoff : Interfaces.C.long;
      tm_zone   : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, tm);

   type ts is record
      TV_Sec    : Interfaces.C.long;
      TV_NSec   : Interfaces.C.long;
   end record;

   procedure localtime_r (T : System.Address; TM_Struct : System.Address);
   pragma Import (C, localtime_r, "localtime_r");

   procedure tzset;
   pragma Import (C, tzset, "tzset");

   function tzname return Ptr_Ptr_Array;
   pragma Import (C, tzname, "__get_tzname");

   procedure print_escaped_string (s : Interfaces.C.Strings.chars_ptr; F : GNAT.OS_Lib.File_Descriptor);
   pragma Import (C, print_escaped_string, "print_escaped_string");

   procedure Clock_Gettime (Clk_ID : clockid_t; TS_Struct : System.Address);
   pragma Import (C, Clock_Gettime, "clock_gettime");

   function getuid return Interfaces.Integer_64;
   pragma Import (C, getuid, "getuid");

   function chown (pathname : Interfaces.C.Strings.chars_ptr; owner : Interfaces.Integer_32; group : Interfaces.Integer_32) return Interfaces.Integer_32;
   pragma Import (C, chown, "chown");
end Unix;
