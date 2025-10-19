/*
** This module is part of EFCP.
**

     Copyright 2007 William Silverman
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

/*
       Transformer for Stochastic Psi Calculus procedures.
       June 2000
*/

-language([evaluate, compound, colon]).
-export(transform/5).
-mode(trust).

-include(spi_constants).

/*
** Transform/5
**
** Transform spifcp module to compound Fcp.
**
** Input:
**
**   Attributes1 - Source attributes.
**   Source      - SpiFcp code, minus attributes.
**
** Output:
**
**   Attributes2 - Attributes1 augmented by exported Fcp procedures.
**   Compound    - Compound Fcp code.
**   Errors      - Diagnostics in the form:  Name - comment(Argument)
**
** The actual transformation is performed in biospi.
*/

transform(Attributes1, Source, Attributes2, Compound, Errors) :-
  biospi#transform(Attributes1, Source, Attributes2, Compound, Errors, spifcp).
