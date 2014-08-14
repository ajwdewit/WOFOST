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
      REAL FUNCTION SWEAF (ET0, CGNR)

*     Chapter 20 in documentation WOFOST Version 4.1 (1988)

*     The fraction of easily available soil water between
*     field capacity and wilting point is a function of the
*     potential evapotranspiration rate (for a closed canopy)
*     in cm/day, ET0, and the crop group number, CGNR (from
*     1 (=drought-sensitive) to 5 (=drought-resistent)). The
*     function SWEAF describes this relationship given in tabular
*     form by Doorenbos & Kassam (1979) and by Van Keulen & Wolf
*     (1986; p.108, table 20).

*     Must be linked with object library TTUTIL.

*     Authors: D.M. Jansen and C.A. van Diepen, October 1986.

      IMPLICIT REAL(A-Z)
**
      SAVE

      DATA A /0.76/,B /1.5/
*     curve for CGNR 5, and other curves at fixed distance below it

      SWEAF = 1./(A+B*ET0) - (5.-CGNR)*0.10

*     correction for lower curves (CGNR less than 3)
      IF (CGNR.LT.3.) SWEAF = SWEAF + (ET0-0.6)/(CGNR*(CGNR+3.))
      SWEAF = LIMIT (0.10, 0.95, SWEAF)

      RETURN
      END
