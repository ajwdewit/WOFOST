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
      SUBROUTINE ROOTD
     &   (ITASK, DELT , IWB   , IZT, FR, RRI, IAIRDU,
     &    RDI  , RDMCR, RDMSOL, ZTI, ZT, RDM, RD)

*     In this routine the depth of the root zone is calculated for each
*     day of the crop cycle
*     Called by WOFOST
*     ITASK = 1, initialization
*     ITASK = 2, rate calculation
*     ITASK = 3, integration
*     ITASK = 4, finish section

*     Author: C.A. van Diepen, September 1988

*-----------------------------------------------------------------------
*     declarations
*-----------------------------------------------------------------------

      IMPLICIT REAL(A-Z)
      INTEGER IAIRDU, ITASK, IWB, IZT, ITOLD
**
      SAVE

      DATA ITOLD /4/

*     return immediately when previous task was initialization

      IF (ITASK.EQ.3.AND.ITOLD.EQ.1) THEN
         ITOLD = ITASK
         RETURN
      END IF

      IF (ITASK.EQ.1) THEN

*        initial and maximum rooting depth
         RD = RDI
         IF (IWB.EQ.0) THEN
            RDM  = MAX (RDI,RDMCR)
            RDMO = 0.
         END IF
         IF (IWB.EQ.1) RDM = MAX (RDI, MIN (RDMSOL,RDMO,RDMCR))

         IF (IZT.EQ.0) ZT = 999.
         IF (IZT.EQ.1) ZT = ZTI

      ELSE IF (ITASK.EQ.2) THEN

*        root growth RR in cm (is not considered as a rate!)
         RR = MIN (RDM-RD,RRI*DELT)
         IF (FR.LE.0.) RR=0.
*        with groundwater, root growth zero nearby groundwater
         IF (IAIRDU.EQ.0 .AND. ZT-RD.LT.10.) RR=0.

      ELSE IF (ITASK.EQ.3) THEN

*        new depth of rootzone
         RD = RD+RR

      ELSE IF (ITASK.EQ.4) THEN

*        save final rooting depth under potential production
*        needed for calculation water-limited production
         IF (IWB.EQ.0) RDMO = RD
      END IF

      ITOLD = ITASK

      RETURN
      END
