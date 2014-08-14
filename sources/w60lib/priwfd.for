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
      SUBROUTINE PRIWFD (ITASK , IYEAR , IDAY  ,
     &                   IUOUT , IBAL  , IOX   , IFUNRN,
     &                   SM0   , SMFCF , SMW   , SMLIM , RDM   , WAV,
     &                   NOTINF, RD    , SSMAX , RDMSOL)

*     Prints on unit IUOUT part of WOFOST.OUT (standard output table
*     showing variables of crop growth and waterbalance at interval of
*     PRDEL days) relative to water-limited production calculated on
*     the basis of soil water balance routine WATFD (free drainage).

*     Author: C.A. van Diepen, February 1989, February 1993
*     +-----------------------------------------------------------------+
*     | Version:      1.2                                               |
*     | Date:         24 July 1997                                      |
*     | Author:       Tamme van der Wal                                 |
*     |               Group Software Engineering                        |
*     | Reason:       Adaptation of soil water balance calculations     |
*     | Modification: Addition of SMLIM in output                       |
*     +-----------------------------------------------------------------+ 
*     | MODIFICATION                    								|
*     | Date:         02-06-1998										|
*     | Author:       Hendrik Boogaard								|
*     | Reason:       Removal of IUSCR								|
*     +---------------------------------------------------------------+


*     declarations
      IMPLICIT REAL(A-Z)
      INTEGER IBAL,IDANTX,IDAY,IDHALT, IDOSJ, IDOST
      INTEGER IDWSJ,IDWST,IFUNRN,IOX,IUOUT
      INTEGER ITASK,IYEAR
      REAL    SMLIM

      COMMON/WBALFD/ TRATFD,EVWT,EVST,TSR,RAINT,WDRT,TOTINF,TOTIRR,
     &               PERCT,SSI,SSFIN,WI,W,WLOWI,WLOW,WBALRT,WBALTT,
     &               LOSST,MWC,TWE
*     daily output of crop variables from CROPSI to WOFOST.OUT
      COMMON /CROPDO/WLV,WST,WSO,LAI,DVS,TSUM,TRA,
     &               GASS,MRES,DMI,TAGP,
     &               IDOST, IDWST, IDOSJ, IDWSJ
*     final output of crop variables from CROPSI to WOFOST.OUT
      COMMON /CROPFO/IDANTX,IDHALT,TWRT,TWLV,TWST,TWSO,TAGPX,
     &               GASST,MREST,HINDEX,TRC,TRAT

*     daily output of water variables from WATFD to WOFOST.OUT
      COMMON /WFDDO/RAINT1,EVW,EVS,SM,SS,WWLOW

*     subtotals (are set at zero after printing)
      COMMON/SUBTOT/ TRAJ, EVSJ, EVWJ, RAINJ, RINFJ, RIRRJ, DWJ,
     &               PERCJ, LOSSJ, DWLOWJ, WWLOWJ,
     &               CRJ, DMAXJ, DZJ

* N.B TRATFD is a copy of TRAT calculated in WATGFD
*     TRAT is a copy of TRAT calculated in CROPSI  (So TRAT = TRATFD).
*     TAGP is a copy of TAGP calculated for each timestep in CROPSI.
*     TAGPX is a copy of the final TAGP calculated in CROPSI.
*     IDANTX is a copy of IDANTH
**
      CHARACTER NTEXT*17
      SAVE

      IF (ITASK.EQ.1) THEN

*-----------------------------------------------------------------------
*     print heading of output table
*-----------------------------------------------------------------------

*        data for heading of output table for single run

         IF (IOX.EQ.0) THEN
            WRITE (IUOUT,'(/,1X,A,/,1X,A)')
     &       'WATER LIMITED CROP PRODUCTION WITHOUT GROUNDWATER',
     &       '================================================='
         ELSE
            WRITE (IUOUT,'(/,1X,A,/,1X,A)')
     &       'WATER AND O2 LIMITED CROP PRODUCTION WITHOUT GROUNDWATER',
     &       '========================================================'
         END IF

         IF (IFUNRN.EQ.0) NTEXT = 'fixed fraction   '
         IF (IFUNRN.EQ.1) NTEXT = 'variable fraction'

         WRITE (IUOUT,'(1X,A,3X,A,F4.0,3X,A,F4.2)')
     &     NTEXT,' RDMso=',RDMSOL,'NOTinf=',NOTINF

         WRITE (IUOUT,'(1X,4(A,F5.3),A,F4.0,A,F5.1,A,F4.1)')
     &     'SM0=',SM0,' SMFC=',SMFCF,' SMW=',SMW,' SMLIM=',SMLIM,
     &     '    RDM=',RDM,' WAV=',WAV,' SSmax=',SSMAX

         WRITE (IUOUT,'(/,1X,2A)')
     &     'YEAR  DAY   WLV   WST    WSO   TAGP   LAI  RD    SM ',
     &     'RESRV AVAIL RAIN TRA EVA wet dry'

         WRITE (IUOUT,'(/,1X,2A)')
     &     '          kg/ha kg/ha  kg/ha  kg/ha m2/m2  cm vol.fr ',
     &     ' cm    cm    mm mm/d mm/d  days'

      ELSE IF (ITASK.EQ.2) THEN

*-----------------------------------------------------------------------
*     print daily crop and water variables
*-----------------------------------------------------------------------

         EVAP = EVW + EVS
         WAVAIL = (SM-SMW)*RD
         WRESRV = WWLOW-RDM*SMW

*        print selected daily crop and soil water variables
         WRITE (IUOUT,
     &      '(I6,I4,4F7.0,F6.2,F5.0,F6.3,2F5.1,F6.0,2F5.2,2I3)')
     &      IYEAR,IDAY,WLV,WST,WSO,TAGP,LAI,RD,SM,WRESRV,WAVAIL,
     &       10.*RAINT1,10.*TRA,10.*EVAP,IDOSJ,IDWSJ


*        resetting subtotals
         IDOSJ = 0
         IDWSJ = 0

*        resetting subtotals of waterbalance
         TRAJ   = 0.
         EVSJ   = 0.
         EVWJ   = 0.
         RAINJ  = 0.
         RINFJ  = 0.
         RIRRJ  = 0.
         DWJ    = 0.
         PERCJ  = 0.
         LOSSJ  = 0.
         DWLOWJ = 0.
         WWLOWJ = 0.
         CRJ    = 0.
         DMAXJ  = 0.
         DZJ    = 0.

      ELSE IF (ITASK.EQ.3) THEN
*        no action
         CONTINUE

      ELSE IF (ITASK.EQ.4) THEN

*-----------------------------------------------------------------------
*     print final results
*-----------------------------------------------------------------------
*        print summary results on one line of output table

         WRITE (IUOUT,'(/,1X,A,56X,A,/,1X,2A)')
     &     'SUMMARY :','stress days',
     &     'HALT ANTH TWRT   TWLV   TWST   TWSO   ',
     &     'TAGP HINDEX TRC  GASST  MREST wet dry'

         WRITE (IUOUT,'(2I4,5F7.0,F6.2,F6.1,2F7.0,2I4)')
     &     IDHALT, IDANTX, TWRT, TWLV, TWST, TWSO,
     &     TAGPX,HINDEX,TRC,GASST,MREST,IDOST,IDWST

         IF (IBAL.GT.0) THEN

*           print out desired water balances
*           water balance for the whole system (1d column soil and atm)

            IF (IBAL.EQ.1 .OR. IBAL.EQ.3) THEN
               WRITE (IUOUT,'(/,A,/, 3(A,F6.1),/,3(A,F6.1),/, 
     &                               2(A,F6.1),/,2(A,F6.1) )') 
     & '       WATER BALANCE WHOLE SYSTEM (1 DIMENS. COLUMN ; cm)',
     & ' init max root zone  ',(WI+WLOWI),'  final max root zone ',
     &   (W+WLOW), '    change:', (W+WLOW-WI-WLOWI),
     & '  init surf storage  ', SSI      ,'  final surf storage  ',
     &   SSFIN,    '    change:', (SSFIN-SSI),
     & '         irrigation  ',TOTIRR    ,'  evap water surface  ',EVWT,
     & '           rainfall  ',RAINT     ,'  evap soil surface   ',EVST

               WRITE (IUOUT,'(27X, 2(A,F6.1),/, 
     &                      2(27X,A,F6.1,/), 3(A,F6.1) )')
     & '  transpiration       ',TRATFD,
     &             '  to atmos:',(EVWT+EVST+TRATFD),
     & '  surface runoff      ',TSR,
     & '  lost to deep soil   ',LOSST,
     & '  TOTAL INIT + IN    ', (WI+WLOWI+SSI+TOTIRR+RAINT),
     & '  TOTAL FINAL + OUT   ',
     &    (W+WLOW+SSFIN+EVWT+EVST+TRATFD+TSR+LOSST),
     & '  checksum:',WBALTT
            END IF

*           water balance for the root zone
            IF (IBAL.EQ.2 .OR. IBAL.EQ.3) THEN

                WRITE (IUOUT,'(/,A,3(/,A,F6.1,A,F6.1))')
     &'               WATER BALANCE ROOT ZONE',
     &' initial water stock ',WI,    '  final water stock   ',W,
     &'        infiltration ',TOTINF,'  evap soil surface   ',EVST,
     &' added by root growth',WDRT,  '      transpiration   ',TRATFD
     
               WRITE (IUOUT,'(35X,A,F6.1,/,3(A,F6.1))')
     &   'percolation   ',PERCT,  
     &   ' TOTAL INIT + IN     ',(WI+TOTINF+WDRT),
     &   '  FINAL + OUT         ', (W+EVST+TRATFD+PERCT),
     &   '  checksum:',WBALRT
            END IF

         END IF
      END IF

      RETURN
      END
