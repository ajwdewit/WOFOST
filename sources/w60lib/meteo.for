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
      SUBROUTINE METEO (
     &   IYEAR , IDAY  , IWEATH, WTRDIR, CNTR  ,
     &   GEODIR, DRVDIR, RUNDIR, DBRDIR,
     &   ISTN  , CLFILE, IUWE,
     &   IRNDAT, RSETRG, RSETRD, IURA  , IYEARR, RAFILE,
     &   LAT   , AVRAD , TMIN  , TMAX  , RAIN  , WIND  , VAPOUR,
     &   E0    , ES0   , ET0)

*     +-----------------------------------------------------------------+
*     | Version:      1.1                                               |
*     | Date:         22-September-1997                                 |
*     | Author:       Tamme van der Wal                                 |
*     |               Group Software Engineering                        |
*     | Reason:       un-use of W60MENU.DAT                             |
*     | Modification: Call to CLIMRD extended with COEFA and COEFB      |
*     +-----------------------------------------------------------------+
*     | Version:      1.2                                               |
*     | Date:         24 September 1997                                 |
*     | Author:       Tamme van der Wal                                 |
*     |               Group Software Engineering                        |
*     | Reason:       Angstrom coefficient read in from weather file    |
*     | Modification: COEFA and COEFB removed from interface            |
*     +-----------------------------------------------------------------+ 
*     Provides the calling program with weather data (including
*     rainfall) and evapotranspiration values. Different file formats
*     can be used depending on the value of IWEATH. Random rainfall
*     generation or distrution takes place depending on IWEATH.
*     Depending on the value of IWEATH, different variables are used
*     to define the data file be used:
*     IWEATH =  0,1: CLFILE
*            =    2: WTRDIR, CNTR and ISTN
*     IYEAR  - Year for which weather data are requested             I
*     IDAY   - Day for which weather data are requested              I
*     IWEATH - The type of weather format that is requested          I
*               0: monthly weather data, WOFOST format (requires 
*                  rainfall distribution)
*               1: monthly climate data, WOFOST format (requires 
*                  rainfall generation)
*               2: daily weather data, CABO format
*     WTRDIR - Directory where weather data are stored (see note)    I
*     CNTR   - Country name of weather data                          I
*     GEODIR - Directory of current year weather data                I
*     DRVDIR - Directory of index file of reference weather and
*              current year grid data (DRVDIR=derived data dir.)     I
*     DBMDIR - Directory of dbmeteo weather                          I
*     ISTN   - Station number of weather data                        I
*     CLFILE - Name of climate file                                  I
*     IUWE   - Unit number by which weather data file can be opened  I
*     IRNDAT - Determines how rainfall data should be obtained       I
*              0: use RNGEN, rain generator, reset through RSETRG
*              1: use RNDIS, rain distributor, reset through RSETRD
*              2: use RNREAL, reads daily rain data from other file
*              3: use rainfall from station
*     RSETRG - Reset rainfall generator   (only valid if IRNDAT=0)   I
*     RSETRD - Reset rainfall distributor (only valid if IRNDAT=1)   I
*              Next three variables are only valid if IRNDAT=2
*     IURA   - Unit number by which external rainfall file can be
*              opened                                                I
*     IYEARR - Year for which rainfall data are requested            I
*     RAFILE - File from which rainfall data is to be obtained       I
*     LAT    - Latitude of the site                                  O
*     AVRAD  - Daily shortwave radiation (J/m2/d)                    O
*     TMIN   - Daily minimum temperature (Celsius)                   O
*     TMAX   - Daily maximum temperature (Celsius)                   O
*     RAIN   - Daily rainfall (cm)                                   O
*     WIND   - Daily average windspeed (m/s)                         O
*     VAPOUR - Daily average vapour pressure (mbar/hPa)              O
*     E0     - Evaporation of open water (cm/d)                      O
*     ES0    - Evapotranspiration of wet soil (cm/d)                 O
*     ET0    - Evapotranspiration of short grass (cm/d)              O

*     Must be linked with object library TTUTIL.

*     Author: Daniel van Kraalingen
*     Date  : April 1991


*     formal parameters
      IMPLICIT REAL (A-Z)
      INTEGER IYEAR, IDAY, IWEATH, ISTN, IUWE, IRNDAT , IYEARR, IURA
      INTEGER ILEN
      CHARACTER*250 LOGFILE
      CHARACTER*(*) WTRDIR, CNTR, CLFILE, RAFILE
      CHARACTER*(*) GEODIR, DRVDIR, RUNDIR, DBRDIR

      LOGICAL RSETRG, RSETRD

**    local parameters
      INTEGER ISTAT
      REAL COEFA, COEFB
      SAVE

      IF (IWEATH.EQ.0.OR.IWEATH.EQ.1) THEN

*        monthly climate averages or monthly year averages from WOFOST
*        format, routine has one unit permanently in use (IUWE)

         CALL CLIMRD (CLFILE, IRNDAT, RSETRG, RSETRD,
     &                 IYEAR, IDAY  , IUWE  , LAT   , ELEV,
     &                 COEFA, COEFB , 
     &                 TMIN , TMAX  , AVRAD , VAPOUR, WIND, RAIN)

      ELSE IF (IWEATH.EQ.2) THEN

*        daily data from cabo system (see van Kraalingen et al. for
*        description of this system)

*        find and load station (STINFO does nothing if the station is
*        the same !)

*        unit numbers 92 and 91 are permanently in use
         LOGFILE = RUNDIR(1:ILEN(RUNDIR))//'weather.log'
         CALL STINFO (1101 , WTRDIR, LOGFILE , CNTR, ISTN , IYEAR,
     &                ISTAT, LONGIE, LAT , ELEV, COEFA, COEFB)
         IF (ISTAT.GT.0) WRITE (*,'(A,I7)')
     &      ' WARNING from STINFO: ISTAT = ',ISTAT 

*        get data
         CALL WEATHR (IDAY,ISTAT,AVRAD,TMIN,TMAX,VAPOUR,WIND,RAIN)

* HB : 5-8-2001 WHEN LINE OF DATA IS MISSING FOR RAINFALL -99 IS RETURNED
         IF (ABS(99.0+RAIN).LT.0.0001) THEN
            RAIN = 0.0
         END IF	    

         IF (ISTAT.GT.0) WRITE (*,'(A,I7)')
     &      ' WARNING from WEATHR: ISTAT = ',ISTAT 

*        adjust units (AVRAD to J/m2/day, VAPOUR from kPa to hPa/mbar)
         AVRAD  = AVRAD*1000.
         VAPOUR = VAPOUR*10.
      ELSE
	  CALL ERROR ('METEO','chosen weather option not supported')
      END IF

*     get rain from external file if necessary
      IF (IRNDAT.EQ.2) CALL RNREAL (IURA, RAFILE, IYEARR, IDAY, RAIN)

*     calculate potential rates of water loss
      CALL PENMAN (IDAY, LAT  , ELEV , COEFA , COEFB,
     &             TMIN, TMAX , AVRAD, VAPOUR, WIND,
     &             E0  , ES0  , ET0)

*     convert units
      E0   = E0 /10.
      ES0  = ES0/10.
      ET0  = ET0/10.
      RAIN = RAIN/10.

      RETURN
      END
