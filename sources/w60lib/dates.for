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
      SUBROUTINE DATES (ACTION,IYEAR,IDAY,IMONTH)

*     Converts either a year and day number to a month number 
*     and a day number within that month (ACTION='TO_MONTHS')
*     or the inverse, from a day number in a certain month to
*     a day number within the year (ACTION='TO_YEAR'). Leap years are
*     recognised.

*     ACTION - Type of action to undertake                             I
*     IYEAR  - Year number                                             I
*     IDAY   - Day number (counted from 1st or Jan or 1st of month)   I/O
*     IMONTH - Month number                                           I/O

*     Must be linked with object library TTUTIL.

*     Author: Daniel van Kraalingen
*     Date  : July 1992

*     formal parameters
      INTEGER IYEAR, IDAY, IMONTH
      CHARACTER*(*) ACTION

**    local parameters
      LOGICAL LEAP
      INTEGER IMOTB1(12), IMOTB2(12), IDM1(365), IDM2(366)
      SAVE

*     arrays are filled with data for normal year (1) or leap year (2)

      DATA IMOTB1 /0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IMOTB2 /0,31,60,91,121,152,182,213,244,274,305,335/
      DATA IDM1 /31*1,28*2,31*3,30*4,31*5,30*6,
     &           31*7,31*8,30*9,31*10,30*11,31*12/
      DATA IDM2 /31*1,29*2,31*3,30*4,31*5,30*6,
     &           31*7,31*8,30*9,31*10,30*11,31*12/

*     determine if IYEAR is leap year
      IF (MOD (IYEAR,4).EQ.0.AND.IYEAR.NE.1000) THEN
         LEAP = .TRUE.
      ELSE
         LEAP = .FALSE.
      END IF

      IF (ACTION.EQ.'TO_MONTHS') THEN
*        convert day number counted from 1st of Jan to 1st of month
         IF (.NOT.LEAP) THEN
            IMONTH = IDM1(IDAY)
            IDAY   = IDAY-IMOTB1(IMONTH)            
         ELSE
            IMONTH = IDM2(IDAY)
            IDAY   = IDAY-IMOTB2(IMONTH)            
         END IF
      ELSE IF (ACTION.EQ.'TO_YEAR') THEN
*        convert day number counted from 1st of month to 1st of Jan
         IF (.NOT.LEAP) THEN
            IDAY = IMOTB1(IMONTH)+IDAY
         ELSE
            IDAY = IMOTB2(IMONTH)+IDAY
         END IF
      ELSE
         CALL ERROR ('DATES','unkown action')
      END IF

      RETURN
      END
