/*
** This module is part of EFCP.
**

     Copyright 2007 Shmuel Kliger
     Weizmann Institute of Science, Rehovot, Israel

** EFCP is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** EFCP is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
** GNU General Public License for more details.
** 
** You should have received a copy of the GNU General Public License
** along with EFCP; if not, see:

       http://www.gnu.org/licenses

** or write to:



       Free Software Foundation, Inc.
       51 Franklin Street, Fifth Floor
       Boston, MA 02110-1301 USA

       contact: bill@wisdom.weizmann.ac.il

**
*/

-includable(true).
-language(nil).

FCP_ctl_AnyEscape                       => 0.
FCP_ctl_InfoOffsetEscape                => 1.
FCP_ctl_LabelOffsetEscape               => 2.
FCP_ctl_StringOffsetEscape              => 3.
FCP_ctl_IntegerWordValueEscape          => 4.
FCP_ctl_TupleWordArityEscape            => 5.
FCP_ctl_RegisterEscape                  => 6.
FCP_ctl_ProcessArgEscape                => 7.
FCP_ctl_ModuleNameOffsetEscape          => 8.
FCP_ctl_PrcdrInfoEscape                 => 9.
FCP_ctl_IndexedArgEscape                => 10.
FCP_ctl_ProceduralEscape                => 12.
FCP_ctl_IterativeEscape                 => 13.
FCP_ctl_RealValueEscape                 => 16.
FCP_ctl_MinorItemSize                   => 2.
FCP_ctl_RoundUp                         => 2.
FCP_ctl_BranchAddressSize               => 4.
FCP_ctl_RescaleOffset                   => 1.
FCP_ctl_MinBranchAddress                => -8388607.
FCP_ctl_MaxBranchAddress                => 8388607.
FCP_ctl_MajorAllignment                 => 4.
FCP_ctl_MajorItemSize                   => 4.
FCP_ctl_StringHdrSize                   => 8.
FCP_ctl_RealSize                        => 8.
FCP_ctl_TupleAritySize                  => 4.
FCP_ctl_IntegerSize                     => 4.
FCP_ctl_StringOffsetSize                => 4.
