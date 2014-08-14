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
      SUBROUTINE ASTRO (IDAY,LAT,AVRAD,
     &                  DAYL,DAYLP,SINLD,COSLD,DIFPP,ATMTR,DSINBE)

*  Purpose: This subroutine calculates astronomic daylength,
*           diurnal radiation characteristics such as the atmospheric
*           transmission, diffuse radiation etc.. This routine has
*           been modified so that it uses arrays to hold some input
*           output variables for faster processing 

*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)
*  name   type meaning                                    units  class
*  ----   ---- -------                                    -----  -----
*  IDAY    I4  Day number (Jan 1st = 1)                      -      I
*  LAT     R4  Latitude of the site                       degrees   I
*  AVRAD   R4  Daily shortwave radiation                  J m-2 d-1 I
*  DAYL    R4  Astronomical daylength (base = 0 degrees)     h      O
*  DAYLP   R4  Astronomical daylength (base =-4 degrees)     h      O
*  SINLD   R4  Seasonal offset of sine of solar height       -      O
*  COSLD   R4  Amplitude of sine of solar height             -      O
*  DIFPP   R4  Diffuse irradiation perpendicular to direction of     
*              light                                      J m-2 s-1 O
*  ATMTR   R4  Daily atmospheric transmission                -      O
*  DSINBE  R4  Daily total of effective solar height         s      O

*  FATAL ERROR CHECKS: none
*  SUBROUTINES and FUNCTIONS called : none
*  FILE usage : none

*  Authors: Daniel van Kraalingen
*  Date   : April 1991

*  Modification: Include checks for 0<=daylength<=24 hour
*                Remove caching of results
*  Author      : Allard de Wit
*  Date        : January 2011

*     formal parameters
      IMPLICIT REAL(A-Z)
      INTEGER IDAY

      PARAMETER (PI=3.1415926, ANGLE=-4., RAD=0.0174533)

*     Error check on latitude
      IF (ABS (LAT).GT.90.) CALL FATALERR
     &   ('ASTRO','LAT > 90 or LAT < -90')

*     Declination and solar constant for this day
      DEC = -ASIN(SIN(23.45*RAD)*COS(2.*PI*(REAL(IDAY)+10.)/365.))
      SC  = 1370.*(1.+0.033*COS(2.*PI*REAL(IDAY)/365.))

*     calculation of daylength from intermediate variables
*     SINLD, COSLD and AOB
      SINLD = SIN(RAD*LAT)*SIN(DEC)
      COSLD = COS(RAD*LAT)*COS(DEC)
      AOB = SINLD/COSLD

*     For very high latitudes and days in summer and winter a limit is  
*     inserted to avoid math errors when daylength reaches 24 hours in 
*     summer or 0 hours in winter.

*     Calculate solution for base=0 degrees
      IF (ABS(AOB).LE.1.0) THEN
         DAYL  = 12.0*(1.+2.*ASIN(AOB)/PI)
*        integrals of sine of solar height
         DSINB  = 3600.*(DAYL*SINLD+24.*COSLD*SQRT(1.-AOB**2)/PI)
         DSINBE = 3600.*(DAYL*(SINLD+0.4*(SINLD**2+COSLD**2*0.5))+
     &            12.*COSLD*(2.+3.*0.4*SINLD)*SQRT(1.-AOB**2)/PI)
      ELSE
         IF (AOB.GT.1.0)  DAYL = 24.0
         IF (AOB.LT.-1.0) DAYL =  0.0
*        integrals of sine of solar height	
         DSINB  = 3600.*(DAYL*SINLD)
         DSINBE = 3600.*(DAYL*(SINLD+0.4*(SINLD**2+COSLD**2*0.5)))
      ENDIF

*     Calculate solution for base=-4 (ANGLE) degrees
      AOB_CORR = (-SIN(ANGLE*RAD)+SINLD)/COSLD
      IF (ABS(AOB_CORR).LE.1.0) THEN 
         DAYLP = 12.0*(1.+2.*ASIN(AOB_CORR)/PI)
      ELSE
         IF (AOB_CORR.GT.1.0)  DAYLP = 24.0
         IF (AOB_CORR.LT.-1.0) DAYLP =  0.0
      ENDIF

*     extraterrestrial radiation and atmospheric transmission
      ANGOT  = SC*DSINB
*     Check for DAYL=0 as in that case the angot radiation is 0 as well
      IF (DAYL.GT.0.0) THEN
          ATMTR = AVRAD/ANGOT
      ELSE
          ATMTR = 0
      ENDIF

*     estimate fraction diffuse irradiation
      IF (ATMTR.GT.0.75) FRDIF = 0.23
      IF (ATMTR.LE.0.75.AND.ATMTR.GT.0.35) 
     &  FRDIF = 1.33-1.46*ATMTR
      IF (ATMTR.LE.0.35.AND.ATMTR.GT.0.07) 
     &  FRDIF = 1.-2.3*(ATMTR-0.07)**2
      IF (ATMTR.LE.0.07) FRDIF = 1.

      DIFPP = FRDIF*ATMTR*0.5*SC

      RETURN
      END
