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
      SUBROUTINE CONVR2 (ACTION,IVAR2,
     &                   SSD,RAD,TMN,TMX,HUM,WIN,RAI)

*     Converts weather data from 'repaired DBMETEO' files to appropriate 
*     units, and vice versa. The data type is converted from integer to 
*     real, or back, depending on the ACTION required.
*     The method by which data are stored in the repaired weather files 
*     is with INTEGER*2 variables to reduce file size. 
*     The routine can undertake two different actions
*     1) convert 'repaired DBMETO' INTEGER*2 values to REAL values
*        needed after reading a record (ACTION='DECODE_REPAIRED');
*     2) convert REAL values to INTEGER*2 values, needed before writing 
*        a record in the 'repaired DBMETEO' file (ACTION='CODE_REPAIRED').

*     ACTION - Conversion to undertake                               I
*     IVAR2  - INTEGER*2 array with coded weather variables from
*              'repaired DEMETEO file'                               I
*     SSD    - Daily sunshine duration (h/d)                         O
*     RAD    - Daily shortwave radiation (kJ/m2/d)                   O
*     TMN    - Daily average minimum temperature (Celsius)           O
*     TMX    - Daily average maximum temperature (Celsius)           O
*     HUM    - Daily average vapour pressure (kPa)                   O
*     WIN    - Daily average windspeed (m/s)                         O
*     RAI    - Daily rainfall (mm/d)                                 O
*
*     must be linked with object library TTUTIL

*     Author: Daniel van Kraalingen
*     Date  : April 1991, modified by Kees van Diepen, September 1992

*     formal parameters
      IMPLICIT REAL (A-Z)
      INTEGER*2 IVAR2(7)
      CHARACTER*(*) ACTION
**
      SAVE

      IF (ACTION.EQ.'DECODE_REPAIRED') THEN
         SSD = REAL (IVAR2(1))/10.
         IF (IVAR2(2).NE.-99) THEN
            RAD = REAL (IVAR2(2))*10.
         ELSE
            RAD = -99.
         END IF
         TMN = REAL (IVAR2(3))/10.
         TMX = REAL (IVAR2(4))/10.
         HUM = REAL (IVAR2(5))/100.
         WIN = REAL (IVAR2(6))/10.
         RAI = REAL (IVAR2(7))/10.

      ELSE IF (ACTION.EQ.'CODE_REPAIRED') THEN
         IVAR2(1) = NINT (SSD*10.)
         IF (RAD.GT.-98.) THEN
            IVAR2(2) = NINT (RAD/10.)
         ELSE
            IVAR2(2) = -99
         END IF
         IVAR2(3) = NINT (TMN*10.)
         IVAR2(4) = NINT (TMX*10.)
         IVAR2(5) = NINT (HUM*100.)
         IVAR2(6) = NINT (WIN*10.)
         IVAR2(7) = NINT (RAI*10.)
      ELSE
         CALL ERROR ('CONVR2','unknown action')
      END IF

      RETURN
      END
