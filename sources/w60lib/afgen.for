*
*     Copyright 1988, 2013 Alterra, Wageningen-UR
*
*     Licensed under the EUPL, Version 1.1 or as soon they
*     will be approved by the European Commission - subsequent
*     versions of the EUPL (the "Licence");
*     You may not use this work except in compliance with the
*     Licence.
*     You may obtain a copy of the Licence at:
*
*     https://joinup.ec.europa.eu/software/page/eupl
*
*     Unless required by applicable law or agreed to in
*     writing, software distributed under the Licence is
*     distributed on an "AS IS" basis,
*     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
*     express or implied.
*     See the Licence for the specific language governing
*     permissions and limitations under the Licence.
*
      REAL FUNCTION AFGEN (TABLE,ILTAB,X)

*     Chapter 16 in documentation WOFOST Version 4.1 (1988)

*     This function allows linear interpolation in table TABLE
*     for a given value of the independent variable X.

*     AFGEN - function name, result of the interpolation      O
*     TABLE - a one-dimensional array with paired data        I
*             x1, y1, x2, y2, etc.
*     ILTAB - length of TABLE (number of elements)            I
*     X     - value at which interpolation should take place  I

*     Called by routines CROPSI, NUTRIE, SOILRD, STDAY, SUBSOL,
*     WATGW, WATFD

*     Author: C. Rappoldt, January 1986, revised June 1990

      IMPLICIT REAL(A-Z)
      INTEGER I, ILTAB
      REAL TABLE(ILTAB)
**
      SAVE
      IF(TABLE(1).GE.X)  GOTO 40
      DO 10 I=3,ILTAB-1,2
      IF(TABLE(I).GE.X) GOTO 30
      IF(TABLE(I).LT.TABLE(I-2)) GOTO 20
10    CONTINUE
*     table fully filled, argument larger then last X in table
      AFGEN = TABLE(ILTAB)
      RETURN
*     table partly filled, argument larger then last X in table
20    AFGEN=TABLE(I-1)
      RETURN
*     argument between first and last X in table, interpolation
30    SLOPE=(TABLE(I+1)-TABLE(I-1))/(TABLE(I)-TABLE(I-2))
      AFGEN=TABLE(I-1) + (X-TABLE(I-2))*SLOPE
      RETURN
*     argument less or equal to first X in table
40    AFGEN=TABLE(2)
      RETURN
      END
