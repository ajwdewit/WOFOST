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
      SUBROUTINE CROPSI
     &         (ITASK, IDAY  , DELT , TIME , IDEM, DOANTH, IDHALT,
     &         TERMNL, ISTATE, IWB  , IOX  ,
     &         LAT   , AVRAD , TMIN , TMAX , E0  , ES0, ET0,
     &         CRFILE, IUPL  , IUOUT, IULOG,
     &         SM    , SM0   , SMFCF, SMW  , CRAIRC,
     &         EVWMX , EVSMX , TRA  , FR   , RRI   , IAIRDU,
     &         RDI   , RDMCR)

*      SUBROUTINE CROPSI
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)
*  name   type meaning                                    units  class
*  ----   ---- -------                                    -----  -----
*  ITASK   I4  Task that subroutine should perform           -      I
*  IDAY    I4  Day number (Jan 1st = 1)                      -      I
*  DELT    R4  Time step of integration (default 1 day)      d      I
*  TIME    R4  Day number controlled by the TIMER routine           I
*  IDEM    I4  Day of emergence                                     I/O
*  DOANTH  R4  Day of anthesis                                      O
*  IDHALT  I4  Day that simulation is halted                        I/O
*  TERMNL  L4  Flag to indicate to stop the simulation              I/O
*  ISTATE  I4                                                       
*  IWB     I4  flag controlling the calculation of 
*              potential or water-limited yield (0 or 1)     -      I
*  IOX     I4  flag controlling the calculation of 
*              water-limited yield without or with 
*              accounting for oxygen shortage in root zone   -      I
*  LAT     R4  Latitude of the site                         degr    I
*  AVRAD   R4  Daily short wave radiation                   J/m2/d  I
*  TMIN    R4  minimum daily air temperature                _C      I
*  TMAX    R4  maximum daily air temperature                _C      I
*  E0      R4  potential evaporation rate from water surf   cm d-1  I
*  ES0     R4  potential evaporation rate from soil surf    cm d-1  I
*  ET0     R4  potential evapotranspiration rate            cm d-1  I
*  CRFILE  C   name of input file with crop data                    I
*  IUPL    I4  unit of input file with crop data                    I
*  IUOUT   I4  unit of output file with simulation results          I
*  IULOG   I4  unit of log file                                     I
*  SM      R4  soil moisture content in the rooted zone    cm3 cm-3 I
*  SM0     R4  soil porosity, saturated moisture content   cm3 cm-3 I
*  SMFCF   R4  soil moisture content at field capacity     cm3 cm-3 I
*  SMW     R4  soil moisture content at wilting point      cm3 cm-3 I
*  CRAIRC  R4  critical air content in the rootzone        cm3 cm-3 I
*  EVWMX   R4  maximum evaporation rate from water surface cm d-1   I
*  EVSMX   R4  maximum evaporation rate from soil surface  cm d-1   I or O
*  TRA     R4  crop transpiration rate                     cm d-1   I or O
*  FR      R4  fraction of dry matter increase 
*              partitioned to the roots                      -      O
*  RRI     R4  root length growth rate                     cm d-1   O
*  IAIRDU  I4  indicates presence (1) or absence (0) of 
*              airducts in the roots.                               O
*              (1) means tolerating water-saturated soils
*  RDI     R4  initial rooting depth                         cm     O
*  RDMCR   R4  crop-dependent maximum rooting depth          cm     O
*   
*     In this routine the simulation of the potential or the
*     water-limited crop growth is performed.
*     Called by WOFSIM to perform a specific task. 
*     Four different tasks are distinguished :
*     ITASK = 1, initialization
*     ITASK = 2, rate calculation
*     ITASK = 3, integration
*     ITASK = 4, finish section
 
*     Author: C.A. van Diepen, February 1989, revised July 1990
*     December 1992
 
*     Modifications since WOFOST Version 4.1:
*     - 3 extra parameters for the description of exponential growth 
*     of young leaves : RGRLAI  LAIEM   DVSEND
*     - simulation of crop emergence 
 
*     Must be linked with object library TTUTIL.
*     Subroutines and functions called: AFGEN, ASTRO, EVTRA, TOTASS.
*     +--------------------------------------------------------------+
*     | Version:      1.2                                            |
*     | Date:         25-July-1997                                   |
*     | Author:       Tamme van der Wal                              |
*     |               Group Software Engineering                     |
*     | Reason:       Changes in crop parameters KDIF, EFF and SSA   |
*     |               which are now functions of DVS or DTEMP        |
*     | Modification: KDIFTB, EFFTB and SSATB are now read in from   |
*     |               data files and KDIF, EFF and SSA are calculated|
*     |               using the AFGEN function.                      |
*     +--------------------------------------------------------------+
*     | Version:      1.3                                            |
*     | Date:         25-July-1997                                   |
*     | Author:       Tamme van der Wal                              |
*     |               Group Software Engineering                     |
*     | Reason:       New parameter is read in: DVSI                 |
*     +--------------------------------------------------------------+
*    | MODIFICATION                                                  |
*    | author:       Hendrik Boogaard                                |
*    | date:         02-06-1998                                      |
*    | reason:       - FSEOPT                                        |
*    | modification:                                                 |
*    | - FSEOPT: - read data about crop tables that have to be       |
*    |             assigned to optimize parameters (IPARTB, NCTBx,   |
*    |             ICTBx, IICTBx) from crop file                     |
*    |           - call of APTOCTB that assigns the indices of the   |
*    |             crop tables to the parameters used in FSEOPT      |
*    |           - adjustement of calling CROPSI (MODE, OUTPUT)      |
*    |           - filling of matrices with simulation data on       |
*    |             similar timesteps as observations                 |
*    +----------------------------------------------------------------+

** 2.2 declarations

      IMPLICIT REAL(A-Z)
      INTEGER I1,I2,IAIRDU,IUPL,IUOUT,IULOG
      INTEGER IDANTH,IDEM,IDANTX,IDAY,IDDRY,IDFLPP,IDFLWL,IDHALT
      INTEGER IDHALX,IDOS,IDOSJ,IDOST,IDSL,IDWET,IDWS
      INTEGER IDWSJ,IDWST,ILAMAX,ILSSA, ILEFF, ILKDIF, ISTATE
      INTEGER ILDTSM,ILFL,ILFO,ILFR,ILFS,ILRDRR,ILRDRS,ILRFSE
      INTEGER ILSLA,ILTMNF,ILTMPF,ILVOLD,IOX,ITASK,ITOLD,IWB

*     data block with simulation results
*     daily output of crop variables from CROPSI to WOFOST.OUT
      COMMON /CROPDO/ WLV1,WST1,WSO1,LAI1,DVS1,TSUM,TRA1,GASS1,
     &                MRES1,DMI1,TAGP1,IDOST, IDWST, IDOSJ, IDWSJ
*     final output of crop variables from CROPSI to WOFOST.OUT
      COMMON /CROPFO/ IDANTX,IDHALX,TWRTX,TWLVX,TWSTX,TWSOX,TAGPX,
     &                GASSTX,MRESTX,HINDXX,TRCX,TRATX
*     final results for yield statistics and calculation of
*     nutrient-limited production from CROPSI to MAIN

*     DATA BLOCK WITH CROP SIMULATION RESULTS
*     final results from subroutine CROPSI needed for yield statistics,
*     calculation of nutrient-limited production and reporting
      COMMON /CRPSI/ YLVPP, YSTPP , YSOPP, HIPP  , RATPP, IDFLPP,
     &               DURPP, TRATPP, TRCPP, YRTPP , YAGPP, GASPP ,
     &               RESPP, LAMXPP, YLVWL, YSTWL , YSOWL, HIWL  ,
     &               RATWL, IDFLWL, DURWL, TRATWL, TRCWL, YRTWL ,
     &               YAGWL, GASWL , RESWL, LAMXWL, IDWET, IDDRY

*     declarations
      CHARACTER CRFILE*(*)
      REAL AMAXTB(30), DTSMTB(30), FRTB(30), FLTB(30)
      REAL FSTB(30), FOTB(30), RDRRTB(30), RDRSTB(30)
      REAL RFSETB(30), SLATB(30), TMNFTB(30), TMPFTB(30)
      REAL KDIF, EFF, SSA
      REAL DVSI
      REAL SSATB(30), KDIFTB(30), EFFTB(30)
      REAL LV(366), SLA(366), LVAGE(366),TMNSAV(7)

      LOGICAL DOANTH, TERMNL

      SAVE

      DATA ITOLD /4/

      IF (ITASK.EQ.3.AND.ITOLD.EQ.1) THEN
         ITOLD = ITASK
         RETURN
      END IF

      IF (ITASK.EQ.1) THEN

*        initialize crop data reading
         CALL RDINIT (IUPL, IULOG, CRFILE)
         CALL RDSINT ('IDSL',IDSL)
         CALL RDSREA ('DLO',DLO)
         CALL RDSREA ('DLC',DLC)
         CALL RDSREA ('TSUM1',TSUM1)
         CALL RDSREA ('TSUM2',TSUM2)
         CALL RDAREA ('DTSMTB',DTSMTB,30,ILDTSM)
         CALL RDSREA ('DVSI', DVSI)
         CALL RDSREA ('DVSEND',DVSEND)
         CALL RDSREA ('TDWI',TDWI)
         CALL RDSREA ('RGRLAI',RGRLAI)
         CALL RDAREA ('SLATB',SLATB,30,ILSLA)
         CALL RDSREA ('SPA',SPA)
         CALL RDAREA ('SSATB',SSATB,30,ILSSA)
         CALL RDSREA ('SPAN',SPAN)
         CALL RDSREA ('TBASE',TBASE)
         CALL RDAREA ('KDIFTB',KDIFTB,30,ILKDIF)
         CALL RDAREA ('EFFTB' ,EFFTB ,30,ILEFF )
         CALL RDAREA ('AMAXTB',AMAXTB,30,ILAMAX)
         CALL RDAREA ('TMPFTB',TMPFTB,30,ILTMPF)
         CALL RDAREA ('TMNFTB',TMNFTB,30,ILTMNF)
         CALL RDSREA ('CVL',CVL)
         CALL RDSREA ('CVO',CVO)
         CALL RDSREA ('CVR',CVR)
         CALL RDSREA ('CVS',CVS)
         CALL RDSREA ('Q10',Q10)
         CALL RDSREA ('RML',RML)
         CALL RDSREA ('RMO',RMO)
         CALL RDSREA ('RMR',RMR)
         CALL RDSREA ('RMS',RMS)
         CALL RDAREA ('RFSETB',RFSETB,30,ILRFSE)
         CALL RDAREA ('FRTB',FRTB,30,ILFR)
         CALL RDAREA ('FLTB',FLTB,30,ILFL)
         CALL RDAREA ('FSTB',FSTB,30,ILFS)
         CALL RDAREA ('FOTB',FOTB,30,ILFO)
         CALL RDSREA ('PERDL',PERDL)
         CALL RDAREA ('RDRRTB',RDRRTB,30,ILRDRR)
         CALL RDAREA ('RDRSTB',RDRSTB,30,ILRDRS)
         CALL RDSREA ('CFET',CFET)
         CALL RDSREA ('DEPNR',DEPNR)
         CALL RDSINT ('IAIRDU',IAIRDU)
         CALL RDSREA ('RDI',RDI)
         CALL RDSREA ('RRI',RRI)
         CALL RDSREA ('RDMCR',RDMCR)

*        emergence parameters
         CALL RDSREA ('TBASEM',TBASEM)
         CALL RDSREA ('TEFFMX',TEFFMX)
         CALL RDSREA ('TSUMEM',TSUMEM)

* 2.6    initial crop conditions at emergence or transplanting

* 2.6.1  initial values of crop parameters

         IDANTH = -99
         DOANTH = .FALSE.
         DVS    = DVSI
         TSUM   = 0.
         FR = AFGEN (FRTB, ILFR, DVS)
         FL = AFGEN (FLTB, ILFL, DVS)
         FS = AFGEN (FSTB, ILFS, DVS)
         FO = AFGEN (FOTB, ILFO, DVS)
         SLA(1)   = AFGEN (SLATB, ILSLA, DVS)
         LVAGE(1) = 0.
         ILVOLD   = 1

*        number of stress days
         IDOST = 0
         IDWST = 0
*        subtotals (over print interval) number of stress days
         IDOSJ = 0
         IDWSJ = 0
         IDOS  = 0
         IDWS  = 0

* 2.6.2  initial state variables of the crop

         DWRT = 0.
         DWLV = 0.
         DWST = 0.
         DWSO = 0.

         IF (ISTATE.EQ.3) THEN
*           emergence
            WRT  = FR*TDWI
            TADW = (1.-FR)*TDWI
            WST  = FS*TADW
            WSO  = FO*TADW
            WLV  = FL*TADW
            LAIEM = WLV*SLA(1)
            LV(1)  = WLV
            LASUM  = LAIEM
            LAIEXP = LAIEM
            LAIMAX = LAIEM
            SSA = AFGEN ( SSATB, ILSSA, DVS )
            LAI = LASUM+SSA*WST+SPA*WSO
         ELSE
            WRT  = 0.
            TADW = 0. 
            WST  = 0.
            WSO  = 0.
            WLV    = 0.
            LV(1)  = 0.
            LASUM  = 0.
            LAIEXP = 0.
            LAIMAX = 0.
            LAI    = 0.
         END IF

* 2.6.3  initial summation variables of the crop

         TAGP= WLV + WST +WSO
         GASST = 0.
         MREST = 0.
         TRAT  = 0.

         TMINRA = 0.
         DO 10 I1=1,7
            TMNSAV(I1) = -99.
10       CONTINUE
*        initialization of emergence parameters
         TSUME  = 0.
         DTSUME = 0.
         
      ELSE IF (ITASK.EQ.2) THEN

*-----------------------------------------------------------------------
*        dynamic calculations : rates of change of the crop variables
*-----------------------------------------------------------------------

*        average temperature and average daytemperature
         TEMP  = (TMIN+TMAX)/2.
         DTEMP = (TMAX+TEMP)/2.

*        seven day running average of minimum temperature
         IF (ISTATE.GE.3) THEN
*           shift minimum temperatures to the left
            DO 20 I1=1,6
               TMNSAV(I1) = TMNSAV(I1+1)
20          CONTINUE
            TMNSAV(7) = TMIN

*           calculate new average minimum temperature
            TMINRA = 0.
            I2 = 0
            DO 30 I1=1,7
               IF (TMNSAV(I1).NE.-99.) THEN
                  TMINRA = TMINRA+TMNSAV(I1)
                  I2 = I2+1
               END IF
30          CONTINUE
            TMINRA = TMINRA/REAL(I2)
         END IF

         IF (ISTATE.LT.3) THEN
*           model was started at sowing
            DTSUME = LIMIT (0., TEFFMX-TBASEM, TEMP-TBASEM)
            DTSUM = 0.
            DVR = 0.
         ELSE
*           emergence has taken place
* 2.19      phenological development rate photoperiodic daylength
            CALL ASTRO (IDAY,LAT,AVRAD,
     &                  DAYL,DAYLP,SINLD,COSLD,DIFPP,ATMTR,DSINBE)

*           increase in temperature sum
            DTSUM = AFGEN (DTSMTB, ILDTSM, TEMP)
            IF (DVS.LT.1.) THEN
*              effects of daylength and temperature on development during
*              vegetative phase
               DVRED = 1.
               IF (IDSL.GE.1) DVRED = LIMIT(0.,1.,(DAYLP-DLC)/(DLO-DLC))
               DVR = DVRED*DTSUM/TSUM1
            ELSE
*              development during generative phase
               DVR = DTSUM/TSUM2
            END IF
         END IF

* 2.20   daily dry matter production

*        gross assimilation and correction for sub-optimum
*        average day temperature

         AMAX = AFGEN (AMAXTB, ILAMAX, DVS)
         AMAX = AMAX * AFGEN (TMPFTB, ILTMPF, DTEMP)
         KDIF = AFGEN ( KDIFTB, ILKDIF, DVS  )
         EFF  = AFGEN ( EFFTB , ILEFF , DTEMP) 
         CALL TOTASS (DAYL , AMAX  , EFF, LAI  , KDIF, AVRAD, 
     &                DIFPP, DSINBE, SINLD, COSLD, DTGA)

*        correction for low minimum temperature potential
*        assimilation in kg CH2O per ha

         DTGA  = DTGA * AFGEN (TMNFTB, ILTMNF, TMINRA)
         PGASS = DTGA * 30./44.

*        (evapo)transpiration rates
         CALL EVTRA (IWB  , IOX , IAIRDU, KDIF , CFET , DEPNR,
     &               E0   , ES0 , ET0   , LAI  , SM   , SM0  ,
     &               SMFCF, SMW , CRAIRC, EVWMX, EVSMX, TRAMX,
     &               TRA  , IDOS, IDWS)

*        water stress reduction
         GASS = PGASS * TRA/TRAMX

*        respiration and partitioning of carbohydrates between growth 
*        and maintenance respiration
         RMRES = (RMR*WRT+RML*WLV+RMS*WST+RMO*WSO)*
     &           AFGEN (RFSETB, ILRFSE, DVS)
         TEFF  = Q10**((TEMP-25.)/10.)
         MRES  = MIN (GASS, RMRES*TEFF)
         ASRC  = GASS-MRES

*        DM partitioning factors, and dry matter increase
         FR  = AFGEN (FRTB, ILFR, DVS)
         FL  = AFGEN (FLTB, ILFL, DVS)
         FS  = AFGEN (FSTB, ILFS, DVS)
         FO  = AFGEN (FOTB, ILFO, DVS)
         CVF = 1./((FL/CVL+FS/CVS+FO/CVO)*(1.-FR)+FR/CVR)
         DMI = CVF*ASRC

*        check on partitioning
         FCHECK = FR+(FL+FS+FO)*(1.-FR) - 1.
         IF (ABS (FCHECK).GT.0.0001) THEN
            WRITE (IUOUT,'(A,I3,/,3(A,G12.5),/,2(A,G12.5))') 
     &         ' Error in partitioning functions on day ',IDAY,
     &         ' FCHECK = ',FCHECK,' FR = ',FR,' FL = ',FL,
     &         ' FS = ',FS,' FO = ',FO
            CALL ERROR ('CROPSI',
     &                  'partitioning error, see output file')
         END IF

*        check on carbon balance
         CCHECK = (GASS-MRES-(FR+(FL+FS+FO)*(1.-FR))*DMI/CVF)
     &          /MAX (0.0001,GASS)
         IF (ABS (CCHECK).GT.0.0001) THEN
            WRITE (IUOUT,'(A,I3,/,3(A,G12.5),/,A,4G12.5,/,2(A,G12.5))')
     &        ' Carbon flows nog balanced on day ',IDAY,
     &        ' CCHECK = ',CCHECK,' GASS = ',GASS,' MRES = ',MRES,
     &        ' FR,L,S,O = ',FR,FL,FS,FO,' DMI = ',DMI,' DVF = ',CVF
            CALL ERROR ('CROPSI',
     &                  'carbon balance error, see output file')
         END IF

* 2.21   growth rate by plant organ
* 2.21.1 growth rate roots and aerial parts

         ADMI = (1.-FR)*DMI
         GRRT = FR*DMI
         DRRT = WRT*AFGEN (RDRRTB, ILRDRR, DVS)
         GWRT = GRRT-DRRT

* 2.21.2 growth rate leaves

*        weight of new leaves
         GRLV = FL*ADMI

*        death of leaves due to water stress or high LAI
         DSLV1 = WLV*(1.-TRA/TRAMX)*PERDL
         LAICR = 3.2/KDIF
         DSLV2 = WLV*LIMIT (0., 0.03, 0.03*(LAI-LAICR)/LAICR)
         DSLV  = MAX (DSLV1, DSLV2)

*        determine extra death due to exceeding of life span of leaves
*        leaf death is imposed on array until no more leaves have to 
*        die or all leaves are gone

         REST = DSLV*DELT
         I1   = ILVOLD

100      IF (REST.GT.LV(I1).AND.I1.GE.1) THEN
            REST = REST-LV(I1)
            I1   = I1-1
            GOTO 100
         END IF

*        check if some of the remaining leaves are older than SPAN,
*        sum their weights

         DALV = 0.
         IF (LVAGE(I1).GT.SPAN.AND.REST.GT.0.AND.I1.GE.1) THEN
            DALV = LV(I1)-REST
            REST = 0.
            I1   = I1-1
         END IF

110      IF (I1.GE.1.AND.LVAGE(I1).GT.SPAN) THEN
            DALV = DALV+LV(I1)
            I1   = I1-1
         GOTO 110
         END IF

         DALV = DALV/DELT

*        death rate leaves and growth rate living leaves
         DRLV = DSLV+DALV

*        physiologic ageing of leaves per time step
         FYSDEL = MAX (0., (TEMP-TBASE)/(35.-TBASE))
         SLAT   = AFGEN (SLATB, ILSLA, DVS)

*        leaf area not to exceed exponential growth curve
         IF (LAIEXP.LT.6.) THEN
            DTEFF  = MAX (0.,TEMP-TBASE)
*            GLAIEX = LAIEXP*EXP(RGRLAI*DTEFF)
            GLAIEX=LAIEXP*RGRLAI*DTEFF
*           source-limited increase in leaf area
            GLASOL = GRLV*SLAT
*           sink-limited increase in leaf area
            GLA    = MIN (GLAIEX, GLASOL)
*           adjustment of specific leaf area of youngest leaf class
            IF (GRLV.GT.0.) SLAT = GLA/GRLV
         END IF

* 2.21.3 growth rate stems

         GRST = FS*ADMI
         DRST = AFGEN(RDRSTB, ILRDRS, DVS)*WST
         GWST = GRST-DRST

* 2.21.4 growth rate storage organs

         GWSO = FO*ADMI
         DRSO = 0.

      ELSE IF (ITASK.EQ.3) THEN

*-----------------------------------------------------------------------
*        dynamic calculations : integrals of the crop
*-----------------------------------------------------------------------

         IF (ISTATE.LT.3) THEN
            TSUME = TSUME+DTSUME*DELT
*           emergence test
            IF (TSUME.GE.TSUMEM) THEN
* 2.6.2        initial state variables of the crop

               WRT  = FR*TDWI
               TADW = (1.-FR)*TDWI
               WST  = FS*TADW
               WSO  = FO*TADW

               WLV    = FL*TADW
               LAIEM  = WLV*SLA(1)
               LV(1)  = WLV
               LASUM  = LAIEM
               LAIEXP = LAIEM
               LAIMAX = LAIEM
               SSA    = AFGEN(SSATB,ILSSA, DVS)
               LAI    = LASUM+SSA*WST+SPA*WSO

               ISTATE = 3
            END IF
         END IF

*        phenological development stage and temperature sum
         DVS  = DVS+DVR*DELT
         TSUM = TSUM+DTSUM*DELT

*        save date of anthesis, adjust development stage
         IF (DVS.GE.1. .AND. IDANTH.EQ.-99) THEN
            IDANTH = INT (TIME)-IDEM
            DVS    = 1.
            DOANTH = .TRUE.
         END IF

*        leaf death is imposed on array until no more leaves have to 
*        die or all leaves are gone

         DSLVT = DSLV*DELT
         I1    = ILVOLD
120      IF (DSLVT.GT.0.AND.I1.GE.1) THEN
            IF (DSLVT.GE.LV(I1)) THEN
               DSLVT  = DSLVT-LV(I1)
               LV(I1) = 0.
               I1 = I1-1
            ELSE
               LV(I1) = LV(I1)-DSLVT
               DSLVT  = 0.
            END IF
         GOTO 120
         END IF

130      IF (LVAGE(I1).GE.SPAN.AND.I1.GE.1) THEN
            LV(I1) = 0.
            I1     = I1-1
         GOTO 130
         END IF

         ILVOLD = I1

*        shifting of contents, integration of physiological age
         DO 140 I1=ILVOLD,1,-1
            LV(I1+1)    = LV(I1)
            SLA(I1+1)   = SLA(I1)
            LVAGE(I1+1) = LVAGE(I1)+FYSDEL*DELT
140      CONTINUE
         ILVOLD = ILVOLD+1

*        new leaves in class 1
         LV(1)    = GRLV*DELT
         SLA(1)   = SLAT
         LVAGE(1) = 0.

*        calculation of new leaf area and weight
         LASUM = 0.
         WLV   = 0.
         DO 150 I1=1,ILVOLD
            LASUM = LASUM+LV(I1)*SLA(I1)
            WLV   = WLV+LV(I1)
150      CONTINUE

         LAIEXP = LAIEXP+GLAIEX*DELT

*        dry weight of living plant organs and total above ground biomass
         WRT  = WRT+GWRT*DELT
         WST  = WST+GWST*DELT
         WSO  = WSO+GWSO*DELT
         TADW = WLV+WST+WSO

*        dry weight of dead plant organs
         DWRT = DWRT+DRRT*DELT
         DWLV = DWLV+DRLV*DELT
         DWST = DWST+DRST*DELT
         DWSO = DWSO+DRSO*DELT

*        dry weight of dead and living plant organs
         TWRT = WRT+DWRT
         TWLV = WLV+DWLV
         TWST = WST+DWST
         TWSO = WSO+DWSO
         TAGP = TWLV+TWST+TWSO

*        total gross assimilation and maintenance respiration
         GASST = GASS + GASST
         MREST = MRES + MREST
*        leaf area index
         SSA    = AFGEN(SSATB,ILSSA, DVS)
         LAI    = LASUM+SSA*WST+SPA*WSO
         LAIMAX = MAX (LAI,LAIMAX)
*        transpiration (same cumulation as in soil water routine)
         TRAT = TRA + TRAT

*        counting number of oxygen- and water-stress days
         IDOST = IDOST + IDOS
         IDWST = IDWST + IDWS
*        subtotals (over print interval) number of stress days
         IDOSJ = IDOSJ + IDOS
         IDWSJ = IDWSJ + IDWS

* 2.13   crop finish conditions

         IF (ILVOLD.GT.365) THEN
*           message on output and simulation will be stopped
            WRITE (IUOUT,'(A)') ' number of leaf classes exceeds 365 !'
            TERMNL = .TRUE.
         END IF
         IF (DVS.GE.DVSEND) THEN
            TERMNL = .TRUE.
         END IF
         IF (LAI.LE.0.002.AND.DVS.GT.0.5) THEN
            WRITE (IUOUT,'(A)') ' no living leaves (anymore)'
            TERMNL = .TRUE.
         END IF

      ELSE IF (ITASK.EQ.4) THEN

* 2.24   simulation halted
         IDHALT = IDAY

*        save final output variables: growth duration and development
*        stage, grain straw ratio, harvest index, transpiration coefficient

         RATIO  = TWSO/(TWLV+TWST)
         HINDEX = TWSO/TAGP
         TRC    = 100000.*TRAT/TAGP

*        summary results to WOFOST.OUT:
         IDANTX = IDANTH
         TWRTX  = TWRT
         TWLVX  = TWLV
         TWSTX  = TWST
         TWSOX  = TWSO
         TAGPX  = TAGP
         GASSTX = GASST
         MRESTX = MREST
         HINDXX = HINDEX
         TRCX   = TRC
         TRATX  = TRAT
         IDHALX = IDHALT

         IF (IWB.EQ.0) THEN
*           saving some simulation results potential production
*           output to MAIN program needed for calculation nutrient-limited
*           production and for reports
            YLVPP = TWLV
            YSTPP = TWST
            YSOPP = TWSO
            DURPP = REAL (MOD((365+IDHALT-IDEM),365))
*           output to MAIN program needed for WOFOST.OUT report
            HIPP  = HINDEX
            RATPP = RATIO
*           output to MAIN program needed for ad hoc reports
            TRATPP = TRAT
            TRCPP  = TRC
            IDFLPP = IDANTH
            YRTPP  = TWRT
            YAGPP  = TAGP
            GASPP  = GASST
            RESPP  = MREST
            LAMXPP = LAIMAX
         ELSE
*           saving some simulation results water-limited production

*           output to MAIN program needed for yield statistics, for calculation
*           nutrient-limited production and for reports
            YLVWL = TWLV
            YSTWL = TWST
            YSOWL = TWSO
*           output to MAIN program for yield statistics and WOFOST.OUT report
            HIWL  = HINDEX
            RATWL = RATIO
*           output to MAIN program needed for ad hoc reports
            IDFLWL = IDANTH
            DURWL  = REAL (MOD((365+IDHALT-IDEM),365))
            TRATWL = TRAT
            TRCWL  = TRC
            YRTWL  = TWRT
            YAGWL  = TAGP
            GASWL  = GASST
            RESWL  = MREST
            LAMXWL = LAIMAX
            IDWET  = IDOST
            IDDRY  = IDWST
         END IF

      END IF

*     output to WOFOST.OUT of daily crop variables
      WLV1  = WLV
      WST1  = WST
      WSO1  = WSO
      LAI1  = LAI
      DVS1  = DVS
      TRA1  = TRA
      GASS1 = GASS
      MRES1 = MRES
      DMI1  = DMI
      TAGP1 = TAGP

      ITOLD = ITASK

      RETURN
      END
