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
      SUBROUTINE RDMINF (METINF, IU1, STNAMA, LATSTA, ALTSTA,
     &                   IMNST , INSTAT)

*     Reads in meteo.inf file and returns stations names, latitudes
*     and altitudes.

*     METINF - File name of index of reference weather file            I
*     IU1    - Unit number by which index file can be opened           I
*     STNAMA - Array with station names                                O
*     LATSTA - Array with latitudes                                    O
*     ALTSTA - Array with altitudes                                    O
*     IMNST  - Maximum number of stations that can be read             I
*     INSTAT - Number of stations found on index file                  O

*     Must be linked with object library TTUTIL.

*     Author: Daniel van Kraalingen
*     Date  : July 1992

*     formal parameters
      IMPLICIT REAL (A-Z)
      INTEGER IU1, IMNST, INSTAT
      REAL LATSTA(IMNST), ALTSTA(IMNST)
      CHARACTER*(*) METINF
      CHARACTER*8 STNAMA(IMNST)

**    local parameters
      INTEGER IL1, ILEN, IOS, IREC, IDUM
      CHARACTER LINE*80
      SAVE

*     open meteo.inf file
      CALL LOWERC (METINF)
      IL1 = ILEN (METINF)
      WRITE (*,'(2A)') ' Opening input file : ',METINF(1:IL1)
*     record length in bytes : 80
      CALL FOPENG  (IU1,METINF,'OLD','FD',80,' ')

*     read in meteostation data into arrays until end_of_file
      IOS  = 0
      IREC = 1
10    IF (IOS.EQ.0) THEN
         READ (IU1,'(A)',REC=IREC,IOSTAT=IOS) LINE
         IF (IOS.EQ.0) THEN
            STNAMA(IREC) = '000'//LINE(2:6)
            READ (LINE(41:46),'(F6.2)') LATSTA(IREC)
            READ (LINE(53:59),'(F7.1)') ALTSTA(IREC)
            IREC = IREC+1
            IF (IREC.GT.IMNST) CALL ERROR ('RDRFIN',
     &        'array sizes not sufficient to hold meteo station data')
         ELSE
            IREC = IREC-1
         END IF
      GOTO 10
      END IF

      CLOSE (IU1)
      INSTAT = IREC

      RETURN
      END
