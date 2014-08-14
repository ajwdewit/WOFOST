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
      SUBROUTINE TOTASS (DAYL  , AMAX , EFF  , LAI , KDIF, AVRAD, DIFPP,
     &                   DSINBE, SINLD, COSLD, DTGA)

*  Purpose: This routine calculates the daily total gross CO2
*           assimilation by performing a Gaussian integration over
*           time. At three different times of the day, irradiance is
*           computed and used to calculate the instantaneous canopy
*           assimilation, whereafter integration takes place. More
*           information on this routine is given by Spitters et al.
*           (1988).

*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)
*  name   type meaning                                    units  class
*  ----   ---- -------                                    -----  -----
*  DAYL    R4  Astronomical daylength (base = 0 degrees)     h      O
*  AMAX    R4  Assimilation rate at light saturation      kg CO2/   I
*                                                        ha leaf/h   
*  EFF     R4  Initial light use efficiency              kg CO2/J/  I
*                                                        ha/h m2 s   
*  LAI     R4  Leaf area index                             ha/ha    I
*  KDIF    R4  Extinction coefficient for diffuse light             I
*  AVRAD   R4  Daily shortwave radiation                  J m-2 d-1 I
*  DIFPP   R4  Diffuse irradiation perpendicular to direction of
*              light                                      J m-2 s-1 O
*  DSINBE  R4  Daily total of effective solar height         s      O
*  SINLD   R4  Seasonal offset of sine of solar height       -      O
*  COSLD   R4  Amplitude of sine of solar height             -      O
*  DTGA    R4  Daily total gross assimilation           kg CO2/ha/d O

*  FATAL ERROR CHECKS: none
*  SUBROUTINES and FUNCTIONS called : ASSIM
*  FILE usage : none

*  Authors: Daniel van Kraalingen 
*  Date   : April 1991

      IMPLICIT REAL(A-Z)
      INTEGER I1
      REAL XGAUSS(3), WGAUSS(3)
      PARAMETER (PI=3.1415926)
      SAVE
**
*     Gauss points and weights are stored in an array
      DATA XGAUSS /0.1127017, 0.5000000, 0.8872983/
      DATA WGAUSS /0.2777778, 0.4444444, 0.2777778/

*     calculation of assimilation is done only when it will not be zero
*     (AMAX >0, LAI >0)
      DTGA  = 0.
      IF (AMAX.GT.0..AND.LAI.GT.0.) THEN
         DO 10 I1=1,3
            HOUR   = 12.0+0.5*DAYL*XGAUSS(I1)
            SINB   = MAX (0.,SINLD+COSLD*COS(2.*PI*(HOUR+12.)/24.))
            PAR    = 0.5*AVRAD*SINB*(1.+0.4*SINB)/DSINBE
            PARDIF = MIN (PAR,SINB*DIFPP)
            PARDIR = PAR-PARDIF
            CALL ASSIM (AMAX,EFF,LAI,KDIF,SINB,PARDIR,PARDIF,FGROS)
            DTGA = DTGA+FGROS*WGAUSS(I1)
10       CONTINUE
         DTGA = DTGA*DAYL
      END IF

      RETURN
      END
