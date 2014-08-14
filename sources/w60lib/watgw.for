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
      SUBROUTINE WATGW
     &      (ITASK , DELT  , IDEM , IDHALT, TERMNL,
     &       SOFILE, IUSO  , IUOUT, IULOG , IDRAIN, RD    ,
     &       IAIRDU, IFUNRN, SSI  , SMLIM , SSMAX , ZTI   , DD, 
     &       NOTINF, EVWMX , EVSMX , TRA  , SMW   , CRAIRC, ZT,
     &       SM,     RAIN,   SM0  , SMFCF)

*     In routine WATGW the simulation of the soil water balance is
*     performed for soils influenced by the presence of groundwater. Two 
*     situations are distinguished: with or without artificial drainage.
*     The soil water balance is calculated for a cropped field in the 
*     water-limited production situation. 
*     WATGW is called by WOFSIM.

* VARIABLE TYPE Description                                      Units   I/O

*  ITASK   I*4  flag to control task to be performed                       I
*               1, initialization
*               2, rate calculation, save dynamic output
*               3, integration, summation
*               4, finish section, save final output
*  DELT    R*4  time step (= 1 day)                                d       I
*  IDEM    I*4  day of emergence (date 1-366)                              I
*  IDHALT  I*4  day that simulation is halted (date 1-366)                 I
*  TERMNL  L*4  signals death of crop caused by prolonged waterlogging     O
*  SOFILE CH*(*) name of soil data file                                    I
*  IUSO    I*4  unit nr on which soil data file is opened                  I
*  IUOUT   I*4  unit nr on which output file is opened                     I
*  IULOG   I*4  unit nr on which log file is opened                        I
*  IDRAIN  I*4  indicates presence (1) or absence (0) of drains            I
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
*  ZTI     R*4  initial depth of groundwater table                 cm      I
*  DD      R*4  effective depth of drains (drainage base)          cm      I
*  NOTINF  R*4  if FUNRAI=0 non-infiltrating fraction of rainfall          I
*               if FUNRAI=1 maximum non-infiltrating fraction 
*  EVWMX   R*4  maximum evaporation rate from shaded water surface cm d-1  I
*  EVSMX   R*4  maximum evaporation rate from shaded soil surface  cm d-1  I
*  TRA     R*4  actual transpiration rate                          cm d-1  I
*               Note: TRA is calculated in EVTRA called by CROPSI
*  SMW     R*4  soil moisture content at wilting point           cm3 cm-3  O
*  CRAIRC  R*4  critical air content                             cm3 cm-3  O
*  ZT      R*4  actual depth of groundwater table                   cm     O
*  SM      R*4  actual soil moisture content                     cm3 cm-3  O
*  RAIN    R*4  daily rainfall                                     cm d-1  I
*  SM0     R*4  soil porosity                                    cm3 cm-3  O
*  SMFCF   R*4  soil moisture content at field capacity          cm3 cm-3  O

*     Author: C.A. van Diepen, February 1989, revised July 1990

*     Subroutine WATGW is derived from subroutine APPLE of WOFOST Version 4.1.

*     The purpose of the calculations is to estimate the daily value of the
*     mean soil moisture content. The soil moisture content influences soil 
*     moisture uptake and crop transpiration.

*     In the initial section soil physical data are read from file SOFILE, 
*     and all rates and sum totals are set at zero.
*     The dynamic calculations are carried out in two sections, marked by 
*     an ITASK value. Under ITASK=2 the rates of change per timestep 
*     (delt = 1 day) are calculated and under ITASK=3 the summation and state 
*     variables are calculated.
*     The water balance is driven by rainfall, possibly buffered as surface 
*     storage, and evapotranspiration.
*     The processes considered are infiltration, soil water retention, 
*     the steady state flow between the rootzone and the groundwater table 
*     (upward flow is accounted for as capillary rise, downward flow as
*     percolation), and drainage rate. An irrigation term is included but not 
*     used. The resulting groundwater depth, moisture and air contents in the
*     root zone are calculated.
*     
*     The textural profile of the soil is conceived as homogeneous.
*     Three soil depth zones are distinguished: the rooted zone between 
*     soil surface and actual rooting depth, the zone between rooting 
*     depth and groundwater, and the zone below groundwater level to a 
*     reference depth of XDEF=16000 cm, which is used as a formal system 
*     boundary. Soil moisture between groundwater and root zone is assumed to 
*     be in equilibrium with groundwater (in fact a contradiction with 
*     capillary rise or percolation). A makeshift approach is applied when 
*     the groundwater table rises into the rooted zone. Then two zones are 
*     distinguished within the rooted zone, a saturated lower part and an 
*     unsaturated upper part. 
*     The extension of the root zone from initial rooting depth to maximum 
*     rooting depth (crop dependent) is described in subroutine ROOTD. 
*     The dynamic output is written to file by subroutine PRIWGW. 
*     +-----------------------------------------------------------------+
*     | Version:      1.2                                               |
*     | Date:         24 July 1997                                      |
*     | Author:       Tamme van der Wal                                 |
*     |               Group Software Engineering                        |
*     | Reason:       Adaptation of soil water balance calculations     |
*     |               Inclusions of methods of WOFOST4.4                |
*     | Modification: Addition of 2 new variables in site.dat           |
*     |               which need to be passed to WATFD and WATGW through|
*     |               WOFSIM. Variable SSI was already passed, variable |
*     |               SMLIM is added to parameter list                  |
*     +-----------------------------------------------------------------+ 

*     declarations
      IMPLICIT REAL(A-Z)
      INTEGER I, I2, I20, I3, IAIRDU, IDHALT, IDRAIN,IFUNRN
      INTEGER ILCON,ILSM, IUSO, IUOUT, ITASK, IDEM, ITOLD, IULOG
	REAL    SSI, SMLIM
*     declarations
      CHARACTER SOFILE*(*)

*     output variables for summary water balance in WOFOST.OUT
      COMMON/WBALGW/ TRATX,EVWTX,EVSTX,TSRX,RAINTX,WDRTX,TINFX,TIRRX,
     &               PERCTX,SSIX,SSX,WIX,WX,WZIX,WZX,WBRTX,WBTOTX,
     &               CRTX,DRAITX
*     daily output of water variables from WATGW to WOFOST.OUT
      COMMON /WGWDO/ RAINT1,EVW1,EVS1,SM1,SS1,ZT1

*     tables with physical soil data
*     soil moisture content as a function of pF, and pF as function of soil m.
      REAL SMTAB(30), PFTAB(30)
*     10-log soil hydraulic conductivity as a function of pF
      REAL CONTAB(30)
*     cumulative amount of air as a function of height above groundwater
      REAL SDEFTB(30)
*     height above groundwater as a function of cumulative amount of air 
      REAL DEFDTB(30)
      REAL PGAU(3),WGAU(3)
*     multiplier for non-infiltrating fraction of rainfall as a function 
*     of daily rainfall
      REAL NINFTB(20)
      LOGICAL TERMNL
**
      SAVE
*     maximum depth of ground water table, lower system boundary, serves as 
*     reference depth for amount of water and air in the soil profile
*!!!      DATA XDEF/16000./
      DATA XDEF/1000./
*     mathematical parameters
      DATA PGAU/ 0.1127016654, 0.5, 0.8872983346/
      DATA WGAU/ 0.2777778, 0.4444444, 0.2777778/

*     infiltration parameters WOFOST41
*      DATA NINFTB/0.0,0.00, 0.5,0.12, 1.0,0.29,
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

*----------------------------------------------------------------------
*     initial section
*----------------------------------------------------------------------
*        read required set of soil physical data

*        initialize soil data reading
         CALL RDINIT (IUSO, IULOG, SOFILE)
         CALL RDSREA ('SOPE', SOPE)
         CALL RDSREA ('KSUB', KSUB)
*!       CALL RDSREA ('SMFCF', SMFCF)
*!       CALL RDSREA ('SMW', SMW)
*!       CALL RDSREA ('SM0', SM0)
         CALL RDSREA ('CRAIRC', CRAIRC)
*!       CALL RDSREA ('K0',K0)
         CALL RDAREA ('CONTAB',CONTAB,30,ILCON)
         CALL RDAREA ('SMTAB',SMTAB,30,ILSM)
*        end of soil input section

* 4.4    calculations
*        soil moisture retention data
*        field capacity, wilting point and porosity
         SMFCF = AFGEN (SMTAB, ILSM, LOG10(200.))
         SMW = AFGEN (SMTAB, ILSM, LOG10(16000.))
         SM0 = AFGEN (SMTAB, ILSM, -1.)

         K0 = 10.**AFGEN (CONTAB, ILCON,-1.)

         DO 10 I20=2,ILSM,2
            PFTAB(ILSM+1-I20) = SMTAB(I20)
            PFTAB(ILSM+2-I20) = SMTAB(I20-1)
10       CONTINUE

         RTDF = 0.
*        old rooting depth
         RDOLD = RD

*------------------------------------------------------
*        soil air volume above watertable at equilibrium
*------------------------------------------------------
*        table SDEFTB is calculated, containing the cumulative amount of
*        air as a function of height above groundwater under equilibrium
*        conditions. the table is calculated for the following values of
*        height (= matric head) : 0,2,4,8,16,32,64,128,256,.....16384.
*        method is 3 point gaussian integration of SM on each interval.
*        Table DEFDTB is the inverse function of SDEFTB.

         MH0 = 0.
         MH1 = 2.
         SDEFTB(1) = 0.
         SDEFTB(2) = 0.
         DEFDTB(1) = 0.
         DEFDTB(2) = 0.
         DO 20 I=2,15
            I2 = 2*I
            SDEFTB(I2-1) = MH1
            SDEFTB(I2)   = SDEFTB(I2-2)
            DO 30 I3=1,3
               SDEFTB(I2) = SDEFTB(I2) + WGAU(I3)*(MH1-MH0)*
     &         (SM0-AFGEN (SMTAB,ILSM,LOG10 (MH0+(MH1-MH0)*PGAU(I3))))
30          CONTINUE
            DEFDTB(I2-1) = SDEFTB(I2)
            DEFDTB(I2)   = SDEFTB(I2-1)
            MH0 = MH1
            MH1 = 2*MH1
20       CONTINUE

*-----------------------------------------------------
*        initial state variables of the water balance
*-----------------------------------------------------
         SS = SSI

         ZT = LIMIT (0.1, XDEF, ZTI)
         IF (IDRAIN.EQ.1) ZT = MAX (ZT, DD)

*        amount of air in soil below rooted zone
         SUBAIR = AFGEN (SDEFTB, 30, ZT-RD)
*        amount of moisture in soil below rooted zone
         WZ  = (XDEF-RD)*SM0 - SUBAIR
         WZI = WZ
*        equilibrium amount of soil moisture in rooted zone
         WE = SM0*RD + SUBAIR - AFGEN (SDEFTB,30,ZT)
*        equilibrium amount of moisture above drains up to the surface
         WEDTOT = SM0*DD - AFGEN (SDEFTB,30,DD)

*        initial moisture content in rooted zone
         IF (ZT.LT.RD+100.) THEN
*           groundwater in or close to rootzone
            W = WE
         ELSE
*           groundwater well below rootzone
            W = SMFCF*RD
         END IF
         SM  = W/RD
         WI  = W
*        soil evaporation, days since last rain
         DSLR = 1.
         IF (SM.LE.AFGEN (SMTAB,ILSM,3.0)) DSLR = 5.

*----------------------------------------------------------
*        all summation variables are initially set at zero
*----------------------------------------------------------
         TRAT   = 0.
         EVST   = 0.
         EVWT   = 0.
         TSR    = 0.
         CRT    = 0.
         PERCT  = 0.
         RAINT  = 0.
         WDRT   = 0.
         TOTINF = 0.
         TOTIRR = 0.
         SUMSM  = 0.
         DRAINT = 0.

*--------------------------------------------
*        all rates are initially set at zero
*--------------------------------------------
         EVS  = 0.
         EVW  = 0.
         RAIN = 0.
         RIN  = 0.
         RIRR = 0.
         DW   = 0.
         PERC = 0.
         CR   = 0.
         DMAX = 0.
         DZ   = 0.

      ELSE IF (ITASK.EQ.2) THEN

*-----------------------------------------------------------------------
*        rates of the water balance
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
*! in WOFOST 4.1 :   evs = evsmx * limit(0.,1.,(sm-smw/3.)/(sm0-smw/3.))
            IF (RIN.GE.1.) THEN
               EVS = EVSMX
               DSLR = 1.
            ELSE
               DSLR = DSLR+1.
               EVSMXT = EVSMX*(SQRT (DSLR)-SQRT (DSLR-1.))
               EVS = MIN (EVSMX, EVSMXT+RIN)
            END IF
         END IF

*        preliminary infiltration rate
         IF (SS.GT.0.1) THEN
*           with surface storage, infiltration limited by SOPE
*!!         Next line replaced TvdW 24-jul-97
*!!            AVAIL  = SS+(RAIN+RIRR-EVW)*DELT
            AVAIL = SS + (RAIN * (1.-NOTINF)+RIRR-EVW) * DELT

            RINPRE = MIN (SOPE*DELT,AVAIL)/DELT
         ELSE
*           without surface storage
            IF (IFUNRN.EQ.0) RINPRE=(1.-NOTINF)*RAIN+RIRR+SS/DELT
            IF (IFUNRN.EQ.1)
     &      RINPRE=(1.-NOTINF*AFGEN (NINFTB,20,RAIN))*RAIN+RIRR+SS/DELT
         END IF

*        indicator for groundwater table within (-) or below (+) rootzone
         ZTMRD = ZT-RD

*        capillary flow through the lower root zone boundary
*        no capillary flow if groundwater table is within rooted zone
         CR   = 0.
         PERC = 0.

         IF (ZTMRD.GT.0.) THEN
*           groundwater table below rooted zone:
*           equilibrium amount of soil moisture in rooted zone
            WE = SM0*RD + SUBAIR - AFGEN (SDEFTB,30,ZT)
*           soil suction
            PF = AFGEN (PFTAB,ILSM,SM)
*           calculate capillary flow
            CALL SUBSOL(PF,ZTMRD,FLOW,CONTAB,ILCON)
*           flow is accounted for as capillary rise or percolation
            IF (FLOW.GE.0.) CR = MIN (FLOW,MAX (WE-W,0.)/DELT)
            IF (FLOW.LE.0.) PERC = -1. * MAX (FLOW,MIN (WE-W,0.)/DELT)
*           hypothesis : for rice percolation is limited to K0/20
            IF (IAIRDU.EQ.1) PERC = MIN (PERC,0.05*K0)
         END IF

*        drainage rate
         IF (IDRAIN.EQ.1 .AND. ZT.LT.DD) THEN
*           capacity of artificial drainage system
            DR1 = 0.2*K0
            IF (ZTMRD.LE.0.) THEN
*              ground water above drains and within rootzone
               DR2  = MAX (0.,W + MAX (0.,DD-RD)*SM0 - WEDTOT) / DELT
               DMAX = MIN (DR1,DR2)
            ELSE
*              groundwater above drains and below root zone ; available
*              is the difference between equilibrium water above drains
*              and equilibrium water above groundwater level (both until
*              root zone).
               DR2  = (AFGEN (SDEFTB,30,DD-RD) - SUBAIR) / DELT
               DMAX = MIN (DR1,DR2)
            END IF
         ELSE
*           no drainage if drains are absent or if groundwater is below drains
            DMAX = 0.
         END IF

*        change in the groundwater depth
*        and adjustment of infiltration rate

         IF (ZTMRD .LE. 0.) THEN
*           groundwater table within rootzone
*           air concentration above groundwater in cm3/cm3
            IF (ZT.GE.0.1) AIRC=(RD*SM0-W)/ZT
*           infiltration rate not to exceed available soil air volume
            PERC = DMAX
            RIN = MIN (RINPRE,AIRC*ZT/DELT + TRA + EVS + PERC)
            DZ = (TRA+EVS+PERC-RIN)/AIRC
*           check if groundwater table stays within rooted zone
            IF (DZ*DELT .GT. RD-ZT) THEN
*              groundwater table will drop below rooted zone;
*              in order to maintain a stable moisture content in the rooted
*              zone during this transition, water is recovered from the subsoil.
*              In the water balance of the rooted zone this amount of water is
*              accounted for as CR (capillary rise).
               CR = (DZ*DELT-(RD-ZT))*AIRC/DELT
*              new equilibrium groundwater depth, based on the soil water
*              deficit
               DZ = (AFGEN (DEFDTB,30,CR*DELT) + RD-ZT)/DELT
            END IF
         ELSE
*           groundwater table below rootzone
            DEF1 = SUBAIR + (DMAX + CR - PERC)*DELT
*           groundwater not to exceed RD in current time step
            IF (DEF1.LT.0.) PERC = PERC + DEF1/DELT
            DZ = (AFGEN (DEFDTB,30,DEF1) + RD - ZT) /DELT
*           infiltration rate not to exceed available soil air volume
            RIN = MIN (RINPRE,(SM0-SM-0.0004)*RD/DELT+TRA+EVS+PERC-CR)
         END IF

*        rate of change in amount of moisture in the root zone
         DW = - TRA - EVS - PERC + CR + RIN

*---------------
*        output
*---------------
*        output to WOFOST.OUT of daily soil water variables
         RAINT1 = RAINT
         EVW1   = EVW
         EVS1   = EVS
         SM1    = SM
         SS1    = SS
         ZT1    = ZT

      ELSE IF (ITASK.EQ.3) THEN

*----------------------------------------------------------------------
*     dynamic calculations
*     integrals of the water balance:  summation and state variables
*----------------------------------------------------------------------

*        transpiration
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
         SS    = MIN (SSPRE,SSMAX)
         TSR   = TSR + (SSPRE-SS)
*        amount of water in rooted zone
         W = W + DW*DELT

*        total capillary rise or percolation
         CRT   = CRT + CR*DELT
         PERCT = PERCT + PERC*DELT
*        total drainage
         DRAINT = DRAINT + DMAX * DELT
*        groundwater depth
         ZT = ZT + DZ*DELT
*        amount of air and water below rooted zone
         SUBAIR = AFGEN (SDEFTB,30,ZT-RDOLD)
         WZ = (XDEF-RDOLD)*SM0 - SUBAIR

*---------------------------------------------
*        change of rootzone subsystem boundary
*---------------------------------------------
*        calculation of amount of soil moisture in new rootzone
         IF (RD-RDOLD.GT.0.001) THEN
*           save old value SUBAIR, new values SUBAIR and WZ
            SUBAI0 = SUBAIR
            SUBAIR = AFGEN (SDEFTB,30,ZT-RD)
            WZ = (XDEF-RD)*SM0 - SUBAIR
*           water added to rooted zone by root growth
            WDR = SM0*(RD-RDOLD) - (SUBAI0-SUBAIR)
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

*------------------------------
*        check on waterlogging
*------------------------------
*        finish conditions due to lasting lack of oxygen in root zone
*        (non-rice crops only)
         IF (IAIRDU.EQ.0 .AND. RTDF.GE.10.) THEN
            TERMNL = .TRUE.
*           message on output and simulation will be stopped
            WRITE (IUOUT,'(A)') ' Crop failure due to waterlogging !'
         ELSE
            IF (ZT.LT.10.) RTDF = RTDF+1.
            IF (ZT.GE.10.) RTDF = 0.
         END IF
      ELSE IF (ITASK.EQ.4) THEN

*-----------------------------------------------------------------------
*        finish section
*-----------------------------------------------------------------------

*        mean water content rooting zone during crop growth and total
*        water content of the potentially rooted zone at end of simulation
*         TWE = W 

*        checksums waterbalance for system with groundwater
         WBALRT = TOTINF + CRT + WI - W + WDRT - EVST - TRAT - PERCT
         WBALTT = SSI + RAINT + TOTIRR + WI - W + WZI - WZ - TRAT
     &            - EVWT - EVST - TSR - DRAINT - SS

*        output variables for WOFOST.OUT
         TRATX  = TRAT
         EVWTX  = EVWT
         EVSTX  = EVST
         SSIX   = SSI
         SSX    = SS
         TSRX   = TSR
         RAINTX = RAINT
         WDRTX  = WDRT
         TINFX  = TOTINF
         TIRRX  = TOTIRR
         PERCTX = PERCT
         WIX    = WI
         WX     = W
         WZIX   = WZI
         WZX    = WZ
         WBRTX  = WBALRT
         WBTOTX = WBALTT
         CRTX   = CRT
         DRAITX = DRAINT

      END IF

      ITOLD = ITASK

      RETURN
      END
