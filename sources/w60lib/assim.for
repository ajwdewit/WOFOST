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
      SUBROUTINE ASSIM (AMAX,EFF,LAI,KDIF,SINB,PARDIR,PARDIF,FGROS)

*     Chapter 13 in documentation WOFOST Version 4.1 (1988)

*     This routine calculates the gross CO2 assimilation rate of
*     the whole crop, FGROS, by performing a Gaussian integration
*     over depth in the crop canopy. At three different depths in
*     the canopy, i.e. for different values of LAI, the
*     assimilation rate is computed for given fluxes of photosynthe-
*     tically active radiation, whereafter integration over depth
*     takes place. More information on this routine is given by
*     Spitters et al. (1988). The input variables SINB, PARDIR
*     and PARDIF are calculated in routine TOTASS.

*     Subroutines and functions called: none.
*     Called by routine TOTASS.

*     Author: D.W.G. van Kraalingen, 1986

**    
*13.1 declarations
      IMPLICIT REAL(A-Z)
      INTEGER I
      REAL XGAUSS(3), WGAUSS(3)
      SAVE

*     initialize GAUSS array and scattering coefficient
      DATA XGAUSS /0.1127017, 0.5000000, 0.8872983/
      DATA WGAUSS /0.2777778, 0.4444444, 0.2777778/
      DATA SCV /0.2/

*13.2 extinction coefficients KDIF,KDIRBL,KDIRT
      REFH   = (1.-SQRT(1.-SCV))/(1.+SQRT(1.-SCV))
      REFS   = REFH*2./(1.+1.6*SINB)
      KDIRBL = (0.5/SINB)*KDIF/(0.8*SQRT(1.-SCV))
      KDIRT  = KDIRBL*SQRT(1.-SCV)

*13.3 three-point Gaussian integration over LAI
      FGROS  = 0.
      DO 10 I=1,3
      LAIC   = LAI*XGAUSS(I)
*     absorbed diffuse radiation (VISDF),light from direct
*     origine (VIST) and direct light(VISD)
      VISDF  = (1.-REFS)*PARDIF*KDIF  *EXP (-KDIF  *LAIC)
      VIST   = (1.-REFS)*PARDIR*KDIRT *EXP (-KDIRT *LAIC)
      VISD   = (1.-SCV) *PARDIR*KDIRBL*EXP (-KDIRBL*LAIC)
*     absorbed flux in W/m2 for shaded leaves and assimilation
      VISSHD = VISDF+VIST-VISD
      FGRSH  = AMAX*(1.-EXP (-VISSHD*EFF/MAX(2.0,AMAX)))
*     direct light absorbed by leaves perpendicular on direct
*     beam and assimilation of sunlit leaf area
      VISPP  = (1.-SCV)*PARDIR/SINB
      IF (VISPP.LE.0.) THEN
         FGRSUN = FGRSH
      ELSE
         FGRSUN = AMAX*(1.-(AMAX-FGRSH)
     &          *(1.-EXP (-VISPP*EFF/MAX(2.0,AMAX)))/ (EFF*VISPP))
      END IF
*     fraction of sunlit leaf area (FSLLA) and local
*     assimilation rate (FGL)
      FSLLA  = EXP (-KDIRBL*LAIC)
      FGL    = FSLLA*FGRSUN+(1.-FSLLA)*FGRSH
*     integration
      FGROS  = FGROS+FGL*WGAUSS(I)
10    CONTINUE

      FGROS  = FGROS*LAI
      RETURN
      END
