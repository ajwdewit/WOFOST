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
      SUBROUTINE WATFD (ITASK , DELT  , IDEM  , IDHALT,
     &                  SOFILE, IUSO  , IUOUT , IULOG ,
     &                  RDM   , RD    , IAIRDU, IFUNRN,
     &                  SSI   , SMLIM , SSMAX , WAV   ,
     &                  NOTINF, EVWMX , EVSMX , TRA   ,
     &                  SMW   , CRAIRC, SM    , RAIN  ,
     &                  SM0   , SMFCF)

*     In routine WATFD the simulation of the soil water balance is
*     performed for FREELY DRAINING soils in the water-limited
*     production situation.
*     WATFD is called by WOFSIM.

* VARIABLE TYPE Description                                      Units   I/O

*  ITASK   I*4  flag to control task to be performed                       I
*               1, initialization
*               2, rate calculation, save dynamic output
*               3, integration, summation
*               4, finish section, save final output
*  DELT    R*4  time step = 1 day                                  d       I
*  IDEM    I*4  day of emergence (date 1-366)                              I
*  IDHALT  I*4  day that simulation is halted (date 1-366)                 I
*  SOFILE CH*(*) name of soil data file                                    I
*  IUSO    I*4  unit nr on which soil data file is opened                  I
*  IUOUT   I*4  unit nr on which output file is opened                     I
*  IULOG   I*4  unit nr on which log file is opened                        I
*  RDM     R*4  maximum rooting depth                              cm      I
*  RD      R*4  rooting depth                                      cm      I
*  IAIRDU  I*4  indicates presence(1) or absence(0) of airducts            I
*               in the roots. 1= can tolerate waterlogging 
*  IFUNRN  I*4  flag indicating the way to calculate the                   I
*               non-infiltrating fraction of rainfall:
*               0. fraction is fixed at NOTINF 
*               1. fraction depends on NOTINF and on daily rainfall 
*               as given by NINFTB.
*  SSI     R*4  initial surface storage                            cm      I
*  SMLIM   R*4  max. initial soil moisture in topsoil              cm      I
*  SSMAX   R*4  maximum surface storage                            cm      I
*  WAV     R*4  initial (at emergence) amount of water in excess of        I
*               wilting point, but not exceeding field capacity    cm
*  NOTINF  R*4  if FUNRAI=0 non-infiltrating fraction of rainfall          I
*               if FUNRAI=1 maximum non-infiltrating fraction 
*
*  EVWMX   R*4  maximum evaporation rate from shaded water surface cm d-1  I
*  EVSMX   R*4  maximum evaporation rate from shaded soil surface  cm d-1  I
*  TRA     R*4  actual transpiration rate                          cm d-1  I
*               Note: TRA is calculated in EVTRA called by CROPSI
*  SMW     R*4  soil moisture content at wilting point           cm3 cm-3  O
*  CRAIRC  R*4  critical air content                             cm3 cm-3  O
*  SM      R*4  actual soil moisture content                     cm3 cm-3  O
*  RAIN    R*4  daily rainfall                                     cm d-1  I
*  SM0     R*4  soil porosity                                    cm3 cm-3  O
*  SMFCF   R*4  soil moisture content at field capacity          cm3 cm-3  O


*     Author: C.A. van Diepen, February 1989, revised July 1990

*     Subroutine WATFD is derived from subroutine APPLE of WOFOST Version 4.1.

*     The purpose of the soil water balance calculations is to estimate the 
*     daily value of the soil moisture content. The soil moisture content 
*     influences soil moisture uptake and crop transpiration.
*     In the initial section soil physical data are read from file SOFILE, 
*     and all rates and sum totals are set at zero.
*     The dynamic calculations are carried out in two sections, one for the 
*     calculation of rates of change per timestep (delt = 1 day) and one 
*     for the calculation of summation variables and state variables.
*     The water balance is driven by rainfall, possibly buffered as surface 
*     storage, and evapotranspiration.
*     The processes considered are infiltration, soil water retention, 
*     percolation (here conceived as downward water flow from rooted zone 
*     to second layer), and the loss of water beyond the maximum root zone. 

*     The textural profile of the soil is conceived as homogeneous.
*     Initially the soil profile consists of two layers, the actually rooted 
*     soil and the soil immediately below the rooted zone until the maximum
*     rooting depth (soil and crop dependent). The extension of the root zone 
*     from initial rooting depth to maximum rooting depth is described in 
*     subroutine ROOTD. Its effect on the soil moisture content is accounted 
*     for in this subroutine WATFD.
*     From the moment that the maximum rooting depth is reached the soil 
*     profile is described as a one layer system.
*     The dynamic output is written to file by subroutine PRIWFD. 
*     +-----------------------------------------------------------------+
*     | Version:      1.2                                               |
*     | Date:         24 July 1997                                      |
*     | Author:       Tamme van der Wal                                 |
*     |               Group Software Engineering                        |
*     | Reason:       Adaptation of soil water balance calculations     |
*     | Modification: Addition of 2 new variables in site.dat           |
*     |               which need to be passed to WATFD and WATGW through|
*     |               WOFSIM. Variable SSI was already passed, variable |
*     |               SMLIM is added to parameter list                  |
*     |               Changed lines indicated with *!!                  |
*     +-----------------------------------------------------------------+ 
*     | MODIFICATION													  |
*     | Date:         15-06-1998										  |
*     | Author:       Hendrik Boogaard								  |
*     | Reason:       Error determination WLOW + limit on determination |
*     |               on amount of water in root zone                   |
*     | 1) limits of WLOW between 0.0 and SM0 in stead of 0.0 and SMFCF |
*     | 2) control whether W is less then 0.0 in integration            |
*     |    only can happen in dry start conditions; if W less than 0.0  |
*     |    then w = 0.0 and EVST is adapted                             |
*     +-----------------------------------------------------------------+ 


*     declarations
      IMPLICIT REAL(A-Z)
      INTEGER IAIRDU, IDHALT, IUSO, IUOUT,IFUNRN, ILSM, ITASK
      INTEGER IDEM,ITOLD,IULOG
      CHARACTER SOFILE*(*)
	REAL SSI, SMLIM

*     daily output of water variables from WATFD
      COMMON /WFDDO/ RAINT1,EVW1,EVS1,SM1,SS1,WWLOW1

*     output variables for summary water balance 
      COMMON/WBALFD/ TRATX,EVWTX,EVSTX,TSRX,RAINTX,WDRTX,TINFX,TIRRX,
     &               PERCTX,SSIX,SSX,WIX,WX,WLOWIX,WLOWX,WBRTX,WBTOTX,
     &               LOSSTX,
     &               MWCX,TWEX
*     subtotals (are set at zero after printing)
      COMMON/SUBTOT/ TRAJ, EVSJ, EVWJ, RAINJ, RINJ, RIRRJ, DWJ,
     &               PERCJ, LOSSJ, DWLOWJ, WWLOWJ,
     &               CRJ, DMAXJ, DZJ

*     multiplier for non-infiltrating fraction of rainfall as a function 
*     of daily rainfall
      REAL NINFTB(20)
**
*     help variable in integration
      REAL W_NEW

      SAVE

*     infiltration parameters WOFOST41
*     DATA NINFTB/0.0,0.00, 0.5,0.12, 1.0,0.29,
*     &              2.0,0.71, 3.0,0.91, 7.0,1.00, 8*0./
*     infiltration parameters WOFOST_WRR
      DATA NINFTB/0.0,0.0, 0.5,0.0, 1.5,1.0, 14*0./

      DATA ITOLD /4/

*     immediate return of execution when previous task was initialization
      IF (ITASK.EQ.3.AND.ITOLD.EQ.1) THEN
         ITOLD = ITASK
         RETURN
      END IF

      IF (ITASK.EQ.1) THEN

*-----------------------------------------------------------------------
*     initial section
*-----------------------------------------------------------------------

*        old rooting depth
         RDOLD = RD

*        read required set of soil physical data
*        n.b. S0, CONTAB and PFTAB are not used in WATFD

*        initialize soil data reading
         CALL RDINIT (IUSO, IULOG, SOFILE)

         CALL RDSREA ('SOPE', SOPE)
         CALL RDSREA ('KSUB', KSUB)
         CALL RDSREA ('SMFCF', SMFCF)
         CALL RDSREA ('SMW', SMW)
         CALL RDSREA ('SM0', SM0)
         CALL RDSREA ('CRAIRC', CRAIRC)
         CALL RDSREA ('K0',K0)
*!!      Check whether SMLIM is within boundaries (TvdW, 24-jul-97)
         IF (SMLIM.LT.SMW) SMLIM = SMW
	   IF (SMLIM.GT.SM0) SMLIM = SM0
*!!      User in NOT notified of changes in SMLIM


*!       CALL RDAREA ('SMTAB',SMTAB,30,ILSM)
*        end of soil input section

* 4.4    calculations
*        soil moisture retention data
*        field capacity, wilting point and porosity
*!         SMFCF = AFGEN (SMTAB, ILSM, LOG10 (200.))
*!         SMW   = AFGEN (SMTAB, ILSM, LOG10 (16000.))
*!         SM0   = AFGEN (SMTAB, ILSM, -1.)
         SS = SSI

*        initial moisture content in rooted zone
*!!      original:
*!!         SMLIM = SMFCF
*!!      now SMLIM is input variable in SITE.DAT  (TvdW, 24-jul-97)
         
         IF (IAIRDU.EQ.1) SMLIM = SM0
         SM = LIMIT (SMW, SMLIM, SMW+WAV/RD)
*        initial amount of moisture in rooted zone
         W  = SM*RD
         WI = W
*        soil evaporation, days since last rain
         DSLR = 1.
         IF (SM.LE.(SMW+0.5*(SMFCF-SMW))) DSLR=5.
*        amount of moisture between rooted zone and max.rooting depth
*!!      SMLIM replaced by SMFCF TvdW 24-jul-97
*!!         WLOW  = LIMIT (0., SMLIM*(RDM-RD), WAV+RDM*SMW-W)
         WLOW  = LIMIT (0., SM0*(RDM-RD), WAV+RDM*SMW-W)
         WLOWI = WLOW
         WWLOW = W + WLOW

*        all summation variables of the water balance are set at zero.
         TRAT   = 0.
         EVST   = 0.
         EVWT   = 0.
         TSR    = 0.
         RAINT  = 0.
         WDRT   = 0.
         TOTINF = 0.
         TOTIRR = 0.
         SUMSM  = 0.
         PERCT  = 0.
         LOSST  = 0.

*        all subtotals (over print interval) are set at zero
         TRAJ  = 0.
         EVSJ  = 0.
         EVWJ  = 0.
         RAINJ = 0.
         RINJ  = 0.
         RIRRJ = 0.
         DWJ   = 0.
         PERCJ = 0.
         LOSSJ = 0.
         DWLOWJ = 0.
         WWLOWJ = 0.
         CRJ    = 0.
         DMAXJ  = 0.
         DZJ    = 0.

*        all rates are set at zero
         EVS   = 0.
         EVW   = 0.
         RAIN  = 0.
         RIN   = 0.
         RIRR  = 0.
         DW    = 0.
         PERC  = 0.
         LOSS  = 0.
         DWLOW = 0.

      ELSE IF (ITASK.EQ.2) THEN

*-----------------------------------------------------------------------
*     rates of the water balance
*-----------------------------------------------------------------------

*        actual transpiration rate
*        N.B.: transpiration rate is calculated in EVTRA called by CROPSI
*        actual evaporation rates ...
         EVW = 0.
         EVS = 0.
*        ... from surface water if surface storage more than 1 cm, ...
         IF (SS.GT.1.) THEN
            EVW = EVWMX
*           ... else from soil surface
         ELSE
*!!in oude WOFOST:   evs = evsmx * limit(0.,1.,(sm-smw/3.)/(sm0-smw/3.))
            IF (RIN.GE.1.) THEN
               EVS  = EVSMX
               DSLR = 1.
            ELSE
               DSLR   = DSLR+1.
               EVSMXT = EVSMX*(SQRT (DSLR)-SQRT(DSLR-1.))
               EVS    = MIN (EVSMX, EVSMXT+RIN)
            END IF
         END IF

*        preliminary infiltration rate
         IF (SS.LE.0.1) THEN
*           without surface storage
            IF (IFUNRN.EQ.0) RINPRE = (1.-NOTINF)*RAIN+RIRR+SS/DELT
            IF (IFUNRN.EQ.1) RINPRE = (1.-NOTINF*AFGEN
     &         (NINFTB,20,RAIN))*RAIN+RIRR+SS/DELT
         ELSE
*           with surface storage, infiltration limited by SOPE
*!!         Next line replaced TvdW 24-jul-97
*!!            AVAIL  = SS+(RAIN+RIRR-EVW)*DELT
            AVAIL = SS + (RAIN * (1.-NOTINF)+RIRR-EVW) * DELT

            RINPRE = MIN (SOPE*DELT, AVAIL)/DELT
         END IF


*        percolation

*        equilibrium amount of soil moisture in rooted zone
         WE = SMFCF * RD
*        percolation from rooted zone to subsoil equals amount of
*        excess moisture in rooted zone (not to exceed conductivity)
         PERC1 = LIMIT (0., SOPE, (W - WE)/DELT - TRA - EVS)

*        loss of water at the lower end of the maximum root zone
*        equilibrium amount of soil moisture below rooted zone
         WELOW = SMFCF * (RDM - RD)
         LOSS  = LIMIT (0., KSUB, (WLOW - WELOW)/DELT + PERC1 )
*        for rice water losses are limited to K0/20
         IF (IAIRDU.EQ.1) LOSS = MIN (LOSS, 0.05*K0)

*        percolation not to exceed uptake capacity of subsoil
         PERC2 = ((RDM -RD) * SM0 - WLOW) / DELT + LOSS
         PERC  = MIN (PERC1,PERC2)

*        adjustment of infiltration rate
         RIN = MIN (RINPRE,(SM0-SM)*RD/DELT + TRA + EVS + PERC)

*        rates of change in amounts of moisture W and WLOW
         DW    = - TRA - EVS - PERC + RIN
         DWLOW = PERC - LOSS

*--------------
*        output
*--------------
*        output to WOFOST.OUT of daily soil water variables
         RAINT1 = RAINT
         EVW1   = EVW
         EVS1   = EVS
         SM1    = SM
         SS1    = SS
         WWLOW1 = WWLOW

      ELSE IF (ITASK.EQ.3) THEN

*-----------------------------------------------------------------------
*       dynamic calculations
*       integrals of the water balance:  summation and state variables
*-----------------------------------------------------------------------

*        total transpiration
         TRAT = TRAT + TRA*DELT
*        total evaporation from surface water layer and/or soil
         EVWT = EVWT + EVW*DELT
         EVST = EVST + EVS*DELT

*        totals for rainfall, irrigation and infiltration
         RAINT  = RAINT + RAIN*DELT
         TOTINF = TOTINF + RIN*DELT
         TOTIRR = TOTIRR + RIRR*DELT

*        surface storage and runoff
         SSPRE = SS + (RAIN+RIRR-EVW-RIN)*DELT
         SS    = MIN (SSPRE, SSMAX)
         TSR   = TSR + (SSPRE-SS)

*        amount of water in rooted zone
         W_NEW = W + DW*DELT
	   IF (W_NEW.LT.0.0) THEN
           EVST = EVST + W_NEW
		 W = 0.0
         ELSE
	     W = W_NEW
	   END IF

*        total percolation and loss of water by deep leaching
         PERCT = PERCT + PERC*DELT
         LOSST = LOSST + LOSS*DELT

*        amount of water in unrooted, lower part of rootable zone
         WLOW = WLOW + DWLOW*DELT
*        total amount of water in the whole rootable zone
         WWLOW = W + WLOW

*---------------------------------------------------------
*        output variables for water balance in WOFOST.OUX
*---------------------------------------------------------
         TRATX  = TRAT
         EVWTX  = EVWT
         EVSTX  = EVST
         TSRX   = TSR
         RAINTX = RAINT
         WDRTX  = WDRT
         TINFX  = TOTINF
         TIRRX  = TOTIRR
         PERCTX = PERCT
         WX     = W
         LOSSTX = LOSST

*-----------------------------------------------------
*        integration of subtotals over print interval
*-----------------------------------------------------
         TRAJ   = TRAJ + TRA*DELT
         EVWJ   = EVWJ + EVW*DELT
         EVSJ   = EVSJ + EVS*DELT
         RAINJ  = RAINJ + RAIN*DELT
         RINJ   = RINJ + RIN*DELT
         RIRRJ  = RIRRJ + RIRR*DELT
         DWJ    = DWJ + DW*DELT
         PERCJ  = PERCJ + PERC*DELT
         LOSSJ  = LOSSJ + LOSS*DELT
         DWLOWJ = DWLOWJ + DWLOW*DELT
         WWLOWJ = DWJ + DWLOWJ

         IF (W_NEW.LT.0.0) THEN
           EVSJ = EVSJ - W_NEW
	   END IF

*----------------------------------------------
*        change of rootzone subsystem boundary
*----------------------------------------------
*        calculation of amount of soil moisture in new rootzone
         IF (RD-RDOLD.GT.0.001) THEN
*           water added to root zone by root growth, in cm
            WDR  = WLOW*(RD-RDOLD)/(RDM-RDOLD)
            WLOW = WLOW - WDR

*           total water addition to rootzone by root growth
            WDRT = WDRT + WDR
*           amount of soil moisture in extended rootzone
            W = W + WDR
         END IF

*        mean soil moisture content in rooted zone
         SM = W/RD
*        calculating mean soil moisture content over growing period
         SUMSM = SUMSM + SM*DELT
*        save rooting depth
         RDOLD = RD

      ELSE IF (ITASK.EQ.4) THEN

*-----------------------------------------------------------------------
*     finish section
*-----------------------------------------------------------------------

*        mean water content rooting zone during crop growth and total
*        water content of the potentially rooted zone at end of simulation
         MWC = SUMSM/MAX(1.,REAL (MOD((365+IDHALT-IDEM),365)))
         TWE = W+WLOW

*        checksums waterbalance for system without groundwater
         WBALRT = TOTINF + WI + WDRT - EVST - TRAT - PERCT - W
         WBALTT = SSI + RAINT + TOTIRR + WI - W + WLOWI - WLOW - TRAT
     &     - EVWT - EVST - TSR - LOSST - SS

*        output variables for summary water balance in WOFOST.OUT
         TRATX  = TRAT
         EVWTX  = EVWT
         EVSTX  = EVST
         TSRX   = TSR
         RAINTX = RAINT
         WDRTX  = WDRT
         TINFX  = TOTINF
         TIRRX  = TOTIRR
         PERCTX = PERCT
         SSIX   = SSI
         SSX    = SS 
         WIX    = WI
         WX     = W
         WLOWIX = WLOWI
         WLOWX  = WLOW
         WBRTX  = WBALRT
         WBTOTX = WBALTT
         LOSSTX = LOSST

         MWCX = MWC
         TWEX = TWE
      END IF

      ITOLD = ITASK

      RETURN
      END
