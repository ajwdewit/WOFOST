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
      SUBROUTINE CLIMRD (CLFILE, IRNDAT, RSETRG, RSETRD,
     &                   IYEAR , IDAY  , IUWE  , LAT   , ELEV,
     &                   COEFA , COEFB ,
     &                   TMIN  , TMAX  , AVRAD , VAPOUR, WIND, RAIN)

*     +--------------------------------------------------------------+
*     | Version:      1.1											   |
*     | Date:         22-September-1997							   |
*     | Author:       Tamme van der Wal							   |
*     |               Group Software Engineering					   |
*     | Reason:       Because of un-use of W60MENU.DAT angstrom      |
*     |               coefficients need to be read in here           |
*     | Modification: COEFA and COEFB are read from datafile and     |
*     |               passed through through the parameters          |
*     +--------------------------------------------------------------+
*     Reads weather data from WOFOST format files including the 
*     generation of rainfall. The routine searches the file for the
*     requested year and reads weather data, latitude and altitude of
*     the site. Years with weather data must be in consecutive order on
*     file.

*     CLFILE - string containing directory and filename              I
*     IRNDAT - rainfall option (generated or distributed)            I
*     RSETRG - flags if reset of rainfall generation is required     I
*     RSETRD - flags if reset of rainfall distribution is required   I
*     IYEAR  - year for which data are requested                     I
*     IDAY   - day for which data are requested                      I
*     IUWE   - unit number with which file can be opened             I
*     LAT    - latitude of the site                                  O
*     ELEV   - altitude of the site                                  O
*     COEFA  - Angstom coefficient a                                 O
*     COEFB  - Angstom coefficient b                                 O
*     TMIN   - minimum temperature (units as on file)                O
*     TMAX   - maximum temperature (units as on file)                O
*     AVRAD  - daily shortwave irradiation (units as on file)        O
*     VAPOUR - vapour pressure (units as on file)                    O
*     WIND   - daily average windspeed (units as on file)            O
*     RAIN   - daily rainfall (units as on file)                     O
*
*     Must be linked with object library TTUTIL.

*     Author: Daniel van Kraalingen
*     Date  : April 1990

*     formal parameters
      IMPLICIT REAL(A-Z)
      INTEGER IRNDAT, IYEAR, IDAY, IUWE
	REAL    LAT, ELEV, COEFA, COEFB
      CHARACTER*(*) CLFILE
      LOGICAL RSETRG, RSETRD

**    local parameters
      REAL DAYNM1(28), DAYNM2(28)
      REAL TMINTB(28), TMAXTB(28), IRRATB(28)
      REAL VAPPTB(28), WINDTB(28), RAINTB(12),RAIND(12),RSERIE(366)
      CHARACTER*80 LINE, CLFILO
      INTEGER ILYEAR, I1, I2, IOS, ILDAY
      LOGICAL OPNFIL,LEAP
      SAVE

*     table of day numbers for normal and leap years
      DATA DAYNM1 /1.,0.,15.,0.,45.,0.,74.,0.,105.,0.,135.,0.,166.,0.,
     &        196.,0.,227.,0.,258.,0.,288.,0.,319.,0.,349.,0.,365.,0./
      DATA DAYNM2 /1.,0.,15.,0.,45.,0.,75.,0.,106.,0.,136.,0.,167.,0.,
     &        197.,0.,228.,0.,259.,0.,289.,0.,320.,0.,350.,0.,366.,0./

      DATA CLFILO /' '/, OPNFIL /.FALSE./

*     open new file if new name is different from name of previous call,
*     set start values of local year and day variables

      IF (CLFILE.NE.CLFILO) THEN
         IF (OPNFIL) CLOSE (IUWE)
         CALL FOPENG (IUWE,CLFILE,'OLD','FS',0,' ')
         OPNFIL = .TRUE.
         CLFILO = CLFILE
         CALL MOFILP (IUWE)
         ILYEAR = -99
         ILDAY  = -99
      END IF

      IF (IYEAR.NE.ILYEAR) THEN

*        requested year does not match year during previous call

*        assign new day numbers to arrays, these will be used for
*        interpolation later
         LEAP = IYEAR.GT.1500.AND.MOD (IYEAR,4).EQ.0
         DO 10 I1=1,27,2
            IF (LEAP) THEN
               DAYNUM = DAYNM2(I1)
            ELSE
               DAYNUM = DAYNM1(I1)
            END IF
            TMINTB(I1) = DAYNUM
            TMAXTB(I1) = DAYNUM
            IRRATB(I1) = DAYNUM
            VAPPTB(I1) = DAYNUM
            WINDTB(I1) = DAYNUM
10       CONTINUE

         IF (IYEAR.LT.ILYEAR) THEN
*           start at top of file if requested year is less than year
*           of previous call
            REWIND (IUWE)
            CALL MOFILP (IUWE)
         END IF

*        read next lines on file, during normal situations when the 
*        requested year (if changed) is one higher than the old year and
*        the year number on file is one higher than in the previous
*        block, these read statements are sufficient to find the
*        requested year

         READ (IUWE,'(A)') LINE
         READ (IUWE,*) ILYEAR,LAT,ELEV, COEFA, COEFB

*        if year on file does not match requested year, start search
*        for requested year
20       IF (IYEAR.NE.ILYEAR) THEN
            DO 30 I1=1,13
               READ (IUWE,'(A)',IOSTAT=IOS) LINE
               IF (IOS.NE.0) THEN
                  WRITE (*,*) IYEAR
                  CALL ERROR 
     &            ('CLIMRD','cannot find requested year')
                END IF
30          CONTINUE
            READ (IUWE,*) ILYEAR,LAT,ELEV, COEFA, COEFB
         GOTO 20
         END IF

*        requested year is found at this point, read weather data

         DO 40 I1=1,12
            I2 = I1*2+2
            READ (IUWE,*,IOSTAT=IOS) TMINTB(I2),TMAXTB(I2),IRRATB(I2),
     &            VAPPTB(I2),WINDTB(I2),RAINTB(I1),RAIND(I1)
            IF (IOS.NE.0) CALL ERROR
     &         ('CLIMRD','error while reading climate data')
40       CONTINUE

* 6.5    inserting monthly values in AFGEN tables
         TMINTB(2)  = (TMINTB(4)+TMINTB(26))/2.
         TMINTB(28) = TMINTB(2)
         TMAXTB(2)  = (TMAXTB(4)+TMAXTB(26))/2.
         TMAXTB(28) = TMAXTB(2)
         IRRATB(2)  = (IRRATB(4)+IRRATB(26))/2.
         IRRATB(28) = IRRATB(2)
         VAPPTB(2)  = (VAPPTB(4)+VAPPTB(26))/2.
         VAPPTB(28) = VAPPTB(2)
         WINDTB(2)  = (WINDTB(4)+WINDTB(26))/2.
         WINDTB(28) = WINDTB(2)
      END IF

*     interpolate weather variables, (this was previously done by
*     subroutine interp)

      RIDAY  = REAL (IDAY)
      TMIN   = AFGEN (TMINTB, 28, RIDAY)
      TMAX   = AFGEN (TMAXTB, 28, RIDAY)
      AVRAD  = AFGEN (IRRATB, 28, RIDAY)
      VAPOUR = AFGEN (VAPPTB, 28, RIDAY)
      WIND   = AFGEN (WINDTB, 28, RIDAY)

*     generated rainfall dependent on type of weather data (either monthly
*     climate averages or monthly year averages)

      IF (IRNDAT.EQ.0) THEN
         IF (IDAY.LT.ILDAY.OR.RSETRG)
     &      CALL RNGEN (RSETRG,RAINTB,RAIND,RSERIE)
         RAIN = RSERIE(IDAY)
      ELSE IF (IRNDAT.EQ.1) THEN
         IF (IDAY.LT.ILDAY.OR.RSETRD)
     &      CALL RNDIS (RSETRD,IYEAR,RAINTB,RAIND,RSERIE)
         RAIN = RSERIE(IDAY)
      ELSE IF (IRNDAT.EQ.2) THEN
         RAIN = -99.
      ELSE
         CALL ERROR ('CLIMRD','illegal rainfall option')
      END IF

*     update local day
      ILDAY  = IDAY

      RETURN
      END
