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
      SUBROUTINE REPRD (DBRDIR,
     &                  STNAM, IU0, IYEAR, IDAY,
     &                  LAT, ELEV, TMIN,TMAX,AVRAD,VAPOUR,WIND,RAIN)
      IMPLICIT REAL (A-Z)

*     Reads weather data from 'repaired DBMETEO' files.
*     This routine automatically detects changes in input data such
*     as weather station, year etc.
* Note: value of SSD is not returned
* Note: acronyms for variables different from elsewhere

*     DBRDIR - Directory where weather data are                      I
*     STNAM  - Station name (8 char WMO number of weather station)   I
*     IU0    - Unit number by which data file may be opened          I
*     IYEAR  - Requested year of weather data                        I
*     IDAY   - Requested day number                                  I
*     LAT    - Latitude of the station                               O
*     ELEV   - Altitude of the station                               O
*     TMIN   - Daily minimum temperature (Celsius)                   O
*     TMAX   - Daily maximum temperature (Celsius)                   O
*     AVRAD  - Daily shortwave radiation (MJ/m2/d)                   O
*     VAPOUR - Daily average vapour pressure (kPa)                   O
*     WIND   - Daily average wind speed (m/s)                        O
*     RAIN   - Daily total rainfall (mm/d)                           O

*     Must be linked with object library TTUTIL.

*     Author: Daniel van Kraalingen
*     Date  : June 1992

*     formal parameters
      CHARACTER*(*) DBRDIR, STNAM
      INTEGER IU0, IYEAR, IDAY

**    local parameters
      CHARACTER CLFILE*80, STNAMO*8, METINF*80
      INTEGER I1, IOS, IREC, IYEARF, IDAYF, IMOF
      INTEGER IFINDC, INSTAT
      LOGICAL OPNF, RDREF

*     special declarations
      INTEGER*2 IVAR2(7)
      BYTE IMO1, IDAY1

*     arrays to hold contents of index file of reference weather
      INTEGER IMNST
      PARAMETER (IMNST=500)
      REAL LATSTA(IMNST), ALTSTA(IMNST)
      CHARACTER*8 STNAMA(IMNST)

      SAVE

      DATA OPNF /.FALSE./, RDREF /.FALSE./
      DATA STNAMO /' '/

*     open new file if new weather directory, station name or data format
*     is requested

      IF (STNAM.NE.STNAMO) THEN
         IF (.NOT.RDREF) THEN
*           open index file once
            METINF = DBRDIR//'meteo.inf'
            CALL LOWERC (METINF)

*           (IU0 free after call to RDRFIN)
            CALL RDMINF (METINF,IU0,STNAMA,
     &                   LATSTA,ALTSTA,IMNST,INSTAT)
            RDREF = .TRUE.
         END IF

*        find requested station in the list, check if found
         I1 = IFINDC (STNAMA,IMNST,1,INSTAT,STNAM)
         IF (I1.EQ.0) CALL ERROR ('REPRD','station not in list')
         LAT  = LATSTA (I1)
         ELEV = ALTSTA (I1)

*        close data file if it is open
         IF (OPNF) CLOSE (IU0)

*        'repaired DBMETEO' format
         CLFILE = DBRDIR//'w'//STNAM(4:8)//'.rep'
         CALL LOWERC (CLFILE)

*        open unformatted, direct access data file
*        record length in bytes : 20
         CALL FOPENG (IU0,CLFILE,'OLD','UD',20,' ')

         STNAMO = STNAM
         OPNF   = .TRUE.
         IYEARF = -99
         IDAYF  = -99
         IREC   = 0
      END IF

*     find requested year and day if day and year during previous call
*     are different

      IOS = 0
      IF (IDAYF.LT.IDAY) IREC = IREC+1
10    IF (IOS.EQ.0.AND.(IYEAR.NE.IYEARF.OR.IDAY.NE.IDAYF)) THEN
*        repaired dbmeteo format
         READ (IU0,REC=IREC,IOSTAT=IOS) IYEARF,IMO1,IDAY1,IVAR2

*        test if read statement was successful
         IF (IOS.EQ.0) THEN
*           convert bytes to normal integers
            IMOF = IMO1
            IDAYF = IDAY1
*           convert month and day in month to day in year
            CALL DATES ('TO_YEAR',IYEARF,IDAYF,IMOF)
            IF (IYEAR.GT.IYEARF) THEN
*              if requested year is greater than year number that was
*              just read, the next record should be read
               IREC = IREC+1
            ELSE IF (IYEAR.EQ.IYEARF) THEN
               IF (IDAY.GT.IDAYF) THEN
*                 if requested day is greater than day number that was
*                 just read, the next record should be read
                  IREC = IREC+1
               ELSE IF (IDAY.LT.IDAYF) THEN
*                 if requested day is smaller than day number that was
*                 just read, jump 30 records up unless near the top of
*                 the file
                  IF (IREC.GT.30) THEN
                     IREC = IREC-30
                  ELSE
                     IREC = IREC-1
                  END IF
               END IF
            ELSE IF (IYEAR.LT.IYEARF) THEN
*              if requested year is smaller than year number that was
*              just read, jump 150 records up unless near the top of
*              the file
               IF (IREC.GT.150) THEN
                  IREC = IREC-150
               ELSE
                  IREC = IREC-1
               END IF
            END IF
*           stop reading if top of file is reached
            IF (IREC.LT.1) IOS = 1
         END IF

      GOTO 10
      END IF

      IF (IOS.NE.0) CALL ERROR
     &   ('REPRD','requested weather data not found')

*     convert coded integers to real values
      CALL CONVR2 ('DECODE_REPAIRED',IVAR2,
     &             SSD,RAD,TMIN,TMAX,HUM,WIND,RAIN)

      AVRAD  = RAD/1.E3
      VAPOUR = HUM*10.

      RETURN
      END
