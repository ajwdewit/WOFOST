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
      SUBROUTINE PENMAN (IDAY,
     &      LAT, ELEV, ANGSTA, ANGSTB, TMIN, TMAX, AVRAD, VAP, WIND2,
     &      E0, ES0, ET0)

*     Chapter 10 in documentation WOFOST Version 4.1 (1988)

*     This routine calculates the potential evapo(transpi)ration
*     rates from a free water surface (E0), a bare soil surface
*     (ES0), and a crop canopy (ET0) in mm/d. For these calculations
*     the analysis by Penman is followed (Frere and Popov, 1979;
*     Penman, 1948, 1956, and 1963).
*     Subroutines and functions called: ASTRO, LIMIT.

*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)
*  name   type meaning                                      units  class
*  ----   ---- -------                                      -----  -----
*  IDAY    I4  Day number (Jan 1st = 1)                       -      I
*  LAT     R4  Latitude of the site                        degrees   I
*  ELEV    R4  Elevation above sea level                      m      I
*  ANGSTA  R4  Empirical constant in Angstrom formula         -      I
*  ANGSTB  R4  Empirical constant in Angstrom formula         -      I
*  TMIN    R4  Minimum temperature                            C      I
*  TMAX    R4  Maximum temperature                            C      I
*  AVRAD   R4  Daily shortwave radiation                   J m-2 d-1 I
*  VAP     R4  24 hour average vapour pressure               hPa     I
*  WIND2   R4  24 hour average windspeed at 2 meter          m/s     I
*  E0      R4  Penman potential evaporation from a free 
*              water surface                                mm/d     O
*  ES0     R4  Penman potential evaporation from a moist 
*              bare soil surface                            mm/d     O
*  ET0     R4  Penman potential transpiration from a crop 
*              canopy                                       mm/d     O

*  FATAL ERROR CHECKS (execution terminated, message): none
*  WARNINGS: none
*  SUBROUTINES and FUNCTIONS called: none
*  FILE usage: none

*     Authors: I.G.A.M. Noy and C.A. van Diepen, September 1986
*     revised van Kraalingen, April, van Diepen, October 1991
*     Added checks to avoid negative evaporation, Allard de Wit, Sep 2011

*     declarations
      IMPLICIT REAL(A-Z)
      INTEGER IDAY
**
      SAVE

*     psychrometric instrument constant (mbar/Celsius-1)
*     albedo for water surface, soil surface and canopy
*     latent heat of evaporation of water (J/kg=J/mm)
*     Stefan Boltzmann constant (J/m2/d/K4)
      PARAMETER (PSYCON=0.67, REFCFW=0.05, REFCFS=0.15, REFCFC=0.25)
      PARAMETER (LHVAP=2.45E6, STBC=4.9E-3)

*10.2 preparatory calculations
*     mean daily temperature and temperature difference (Celsius)
*     coefficient Bu in wind function, dependent on temperature
*     difference
      TMPA  = (TMIN+TMAX)/2.
      TDIF  = TMAX - TMIN
      BU    = 0.54 + 0.35 * LIMIT (0.,1.,(TDIF-12.)/4.)

*     barometric pressure (mbar)
*     psychrometric constant (mbar/Celsius)
      PBAR  = 1013.*EXP (-0.034*ELEV/(TMPA+273.))
      GAMMA = PSYCON*PBAR/1013.


*     saturated vapour pressure according to equation of Goudriaan
*     (1977) derivative of SVAP with respect to temperature, i.e. 
*     slope of the SVAP-temperature curve (mbar/Celsius);
*     measured vapour pressure not to exceed saturated vapour pressure

      SVAP  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
      DELTA = 238.102*17.32491*SVAP/(TMPA+238.102)**2
      VAP   = MIN (VAP,SVAP)

*     the expression n/N (RELSSD) from the Penman formula is estimated
*     from the Angstrom formula: RI=RA(A+B.n/N) -> n/N=(RI/RA-A)/B,
*     where RI/RA is the atmospheric transmission obtained by a CALL
*     to ASTRO:

      CALL ASTRO (IDAY, LAT,   AVRAD,
     &            DAYL, DAYLP, SINLD, COSLD, DIFPP, ATMTR, DSINBE)
      RELSSD = LIMIT (0.,1.,(ATMTR-ABS(ANGSTA))/ABS(ANGSTB))

*     Terms in Penman formula, for water, soil and canopy

*     net outgoing long-wave radiation (J/m2/d) acc. to Brunt (1932)
      RB  = STBC*(TMPA+273.)**4*(0.56-0.079*SQRT(VAP))*(0.1+0.9*RELSSD)

*     net absorbed radiation, expressed in mm/d
      RNW = (AVRAD*(1.-REFCFW)-RB)/LHVAP
      RNS = (AVRAD*(1.-REFCFS)-RB)/LHVAP
      RNC = (AVRAD*(1.-REFCFC)-RB)/LHVAP

*     evaporative demand of the atmosphere (mm/d)
      EA  = 0.26 * MAX (0.,(SVAP-VAP)) * (0.5+BU*WIND2)
      EAC = 0.26 * MAX (0.,(SVAP-VAP)) * (1.0+BU*WIND2)

*     Penman formula (1948)
      E0  = (DELTA*RNW+GAMMA*EA)/(DELTA+GAMMA)
      ES0 = (DELTA*RNS+GAMMA*EA)/(DELTA+GAMMA)
      ET0 = (DELTA*RNC+GAMMA*EAC)/(DELTA+GAMMA)

*     Ensure reference evaporation >= 0.
      E0  = MAX(0., E0)
      ES0 = MAX(0., ES0)
      ET0 = MAX(0., ET0)

      RETURN
      END
