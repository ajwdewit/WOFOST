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
      SUBROUTINE CROPNO (RDI,E0,ES0,RD,EVWMX,EVSMX,TRA)
C ----------------------------------------------------------------------
C --- Date                : 27/01/98
C --- Purpose             : Fake crop
C ----------------------------------------------------------------------
      IMPLICIT NONE
      
      REAL RDI,E0,ES0,RD,EVWMX,EVSMX,TRA
      REAL WLV1,WST1,WSO1,LAI1,DVS1,TSUM,TRA1,GASS1
      REAL MRES1,DMI1,TAGP1
      INTEGER IDOST, IDWST, IDOSJ, IDWSJ

*     data block with simulation results
*     daily output of crop variables from CROPNO to WOFOST.OUT
*     only relevant for variable TRA1 which is a copy of TRA
      COMMON /CROPDO/ WLV1,WST1,WSO1,LAI1,DVS1,TSUM,TRA1,GASS1,
     &                MRES1,DMI1,TAGP1,IDOST, IDWST, IDOSJ, IDWSJ

C ----------------------------------------------------------------------
      RD    = RDI
      EVWMX = E0
      EVSMX = ES0       
      TRA   = 0.0
      TRA1  = TRA
 
      RETURN
      END
