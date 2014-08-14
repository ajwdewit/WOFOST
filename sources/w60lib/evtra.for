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
      SUBROUTINE EVTRA (IWB  , IOX , IAIRDU, KDIF , CFET , DEPNR,
     &                  E0   , ES0 , ET0   , LAI  , SM   , SM0  ,
     &                  SMFCF, SMW , CRAIRC, EVWMX, EVSMX, TRAMX,
     &                  TRA  , IDOS, IDWS)

*     This routine calculates for a given crop cover the maximum
*     evaporation rate from a shaded wet soil surface and from a shaded
*     water surface, and the maximum and actual crop transpiration rate.

*     Called by CROPSI.

*     Author: C.A. van Diepen, April 1989

*     declarations

      IMPLICIT REAL (A-Z)
      INTEGER IAIRDU, IDOS, IDWS, IOX, IWB
**
      SAVE

      DATA DSOS /0./

*     extinction coefficient for total global radiation
      KGLOB = 0.75*KDIF

*     crop specific correction on potential transpiration rate
      ET0 = CFET*ET0

*     maximum evaporation and transpiration rates

      EKL   = EXP (-KGLOB*LAI)
      EVWMX = E0*EKL
      EVSMX = MAX (0., ES0*EKL)
      TRAMX = MAX (0.0001, ET0*(1.-EKL))

*     actual transpiration rate

      IF (IWB.EQ.0) THEN
         TRA  = TRAMX
         IDOS = 0
         IDWS = 0
      ELSE
*        calculation critical soil moisture content
         SWDEP = SWEAF (ET0,DEPNR)
         SMCR = (1.-SWDEP)*(SMFCF-SMW)+SMW
*        reduction in transpiration in case of water shortage
         RFWS = LIMIT (0.,1.,(SM-SMW)/(SMCR-SMW))

*        reduction in transpiration in case of oxygen shortage
*        for non-rice crops, and possibly deficient land drainage
         IF (IAIRDU.EQ.0 .AND. IOX.EQ.1) THEN
*           critical soil moisture content for aeration
            SMAIR = SM0 - CRAIRC
*           count days since start oxygen shortage (up to 4 days)
            IF (SM.GE.SMAIR) DSOS = MIN ((DSOS+1.),4.)
            IF (SM.LT.SMAIR) DSOS = 0.
*           maximum reduction reached after 4 days
            RFOSMX = LIMIT (0.,1.,(SM0-SM)/(SM0-SMAIR))
            RFOS   = RFOSMX + (1.-DSOS/4.)*(1.-RFOSMX)

*!old       IF (IAIRDU.EQ.0) RFOS = LIMIT(0.,1.,((SM0-0.05)-SM)/0.05)

*           for rice, or non-rice crops grown on perfectly drained land
         ELSE IF (IAIRDU.EQ.1 .OR. IOX.EQ.0) THEN
            RFOS = 1.
         END IF

         TRA = RFWS * RFOS * TRAMX
*        counting number of stress days
         IDOS = 0
         IDWS = 0
         IF (RFOS.LT.1.) IDOS = 1
         IF (RFWS.LT.1.) IDWS = 1
      END IF

      RETURN
      END
