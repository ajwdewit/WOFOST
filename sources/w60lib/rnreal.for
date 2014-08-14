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
      SUBROUTINE RNREAL (IURA, RAFILE, IYEAR, IDAY, RAIN)
      IMPLICIT REAL(A-Z)

*     Reads rainfall data from an external file called RAFILE.
*     The file should contain the rainfall data for the requested year.

*     IURA   - Unit number by which rainfall data file can be opened I
*     RAFILE - String containing name of data file                   I
*     IYEAR  - Year for which data are requested                     I
*     IDAY   - Day for which data are requested                      I
*     RAIN   - Amount of rain found on file                          O
*
*     Must be linked with object library TTUTIL.

*     Author: Daniel van Kraalingen
*     Date  : April 1991

*     formal parameters
      INTEGER IYEAR,IDAY,IURA
      CHARACTER*(*) RAFILE

**    local parameters
      INTEGER IYEARO, IYSER(70), INY, I1, I2, I3, IC
      REAL RSERIE(366)
      CHARACTER*80 RAFILO
      LOGICAL OPNFIL
      SAVE

      DATA RAFILO /' '/, OPNFIL /.FALSE./

*     test if the new file name is different from the file name during
*     the previous call, if so, open new data file

      IF (RAFILE.NE.RAFILO) THEN
         IF (OPNFIL) CLOSE (IURA)
         CALL FOPENG (IURA,RAFILE,'OLD','FS',0,' ')
         OPNFIL = .TRUE.
         RAFILO = RAFILE
         IYEARO = -99
      END IF

*     test if requested year is different from current year
*     if so, the available years are read, the column number searched
*     where the requested data are and data are read from that column

      IF (IYEAR.NE.IYEARO) THEN
         REWIND (IURA)
         CALL MOFILP (IURA)

*        read series with years from first non-comment line into IYSER
         READ (IURA,*) INY,(IYSER(I1),I1=1,INY)

*        find which column (IC) with rainfall data on the file is the
*        requested year
         IC = 1
10       IF (IYEAR.NE.IYSER(IC)) THEN
            IC = IC+1
            IF (IC.GT.70) CALL ERROR ('RNREAL','rain year not found')
         GOTO 10
         END IF

*        read file and put requested column in array RSERIE
         DO 20 I1=1,366
            IF (IC.EQ.1) THEN
               READ (IURA,*) I2,RSERIE(I1)
            ELSE
               READ (IURA,*) I2,(DUMMY,I3=1,IC-1),RSERIE(I1)
            END IF
            IF (I1.NE.I2) CALL ERROR ('RNREAL',
     &                    'error in day numbers on file')
20       CONTINUE
         IYEARO = IYEAR
      END IF

*     find rainfall
      RAIN = RSERIE(IDAY)

      RETURN
      END
