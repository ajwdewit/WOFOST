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
      SUBROUTINE STATWP (INIWP, TERMNL,
     &      RUNNAM, CLMNAM, CLFILE, RAINAM, RAFILE, CRPNAM, CRFILE, 
     &      SOLNAM, SOFILE, VRSION, RQUIRD,
     &      FILRER, FILWPS, 
     &      IUOUT,  IUOF,  IUOSUM, IUSTPP, IUSTWP, 
     &      MODE, ISET, ISTSET, INSETS, INYR,  INYEAR,   INR, INYRG,  
     &      IWB, IOX, IASYR, IASYRR, IRNDAT, 
     &      ISTCHO, IDSOW, IDESOW, IDLSOW, IDEM, 
     &      RDMSOL, NOTINF,IZT)

*-----------------------------------------------------------------------
*      STATWP : statistics of production, limited by availability 
*      of water.
*      Subroutine for statistical treatment of yield variables, simulated  
*      in a series of simulation runs with WOFOST, using the soil water 
*      balance model WATFD (free drainage).
*      The yield data refer to one crop and either a time series for one 
*      weather station (daily weather), or a series over different 
*      stations or station-years (mean monthly weather).
*-----------------------------------------------------------------------
*     +---------------------------------------------------------------+
*     | MODIFICATION                    								|
*     | Date:         02-06-1998										|
*     | Author:       Hendrik Boogaard								|
*     | Reason:       Removal of IUSCR + removal of ISOIL and ICROP	|
*     +---------------------------------------------------------------+


*     declarations, common blocks
      IMPLICIT REAL(A-H,J-Z)
      IMPLICIT INTEGER(I)
      LOGICAL INIWP, TERMNL, RQUIRD
      LOGICAL OPSTWP
      CHARACTER*(*) FILRER, FILWPS
      CHARACTER*(*) CRFILE, CRPNAM
      CHARACTER*(*) SOFILE, SOLNAM
      CHARACTER*(*) CLFILE, CLMNAM
      CHARACTER*(*) RAINAM, RAFILE
      CHARACTER*(*) MODE, RUNNAM, VRSION

      INTEGER IUOUT, IUOF,  IUOSUM, IUSTPP, IUSTWP
      INTEGER ISET, ISTSET, INSETS, INYR,  INYEAR,   INR, INYRG
      INTEGER IWB, IOX, IASYR, IASYRR, IRNDAT
      INTEGER ISTCHO, IDSOW, IDESOW, IDLSOW, IDEM
      INTEGER IZT


*      DATA BLOCK WITH CROP SIMULATION RESULTS
*      final results from subroutine CROPSI needed for yield statistics, 
*      calculation of nutrient-limited production and reporting
*       INCLUDE 'WRROUT.CMN'
       COMMON/WRROUT/ MYLVPP,MYSTPP,MYSOPP,VCYPP,
     &       MYLVWP,MYSTWP,MYSOWP,VCYWP,
     &       MYLVXP,MYSTXP,MYSOXP,VCYXP

      INTEGER IDDRY, IDFLPP, IDFLWL, IDWET  
      REAL LAMXPP, LAMXWL
      COMMON /CRPSI/ YLVPP, YSTPP , YSOPP, HIPP  , RATPP, IDFLPP,
     &               DURPP, TRATPP, TRCPP, YRTPP , YAGPP, GASPP ,
     &               RESPP, LAMXPP, YLVWL, YSTWL , YSOWL, HIWL  ,
     &               RATWL, IDFLWL, DURWL, TRATWL, TRCWL, YRTWL ,
     &               YAGWL, GASWL , RESWL, LAMXWL, IDWET, IDDRY

**          
*      output variables for summary water balance in WOFOST.OUT
       COMMON/WBALFD/ TRATF,EVWTF,EVSTF,TSRF,RAINTF,WDRTF,TINFF,TIRRF,
     &     PERCTF,SSIF,SSF,WIF,WF,WLOWIF,WLOWF,WBRTF,WBTOTF,
     &     LOSSTF, MWCF,TWEF

*      output variables for summary water balance in WOFOST.OUT
       COMMON/WBALGW/ TRATG,EVWTG,EVSTG,TSRG,RAINTG,WDRTG,TINFG,TIRRG,
     &     PERCTG,SSIG,SSFING,WIG,WFING,WZIG,WZFING,WBRTG,WBTOTG,
     &     CRTG,DRAITG

*     local parameters
      CHARACTER*15 GRONDW
      INTEGER ISUMEM

       SAVE

       DATA OPSTWP /.FALSE./

       IF (.NOT.INIWP) GOTO 100
       INIWP = .FALSE.

       IF (OPSTWP) GOTO 11
*         open file 
          CALL LOWERC (FILWPS)
          CALL FOPENG (IUSTWP,FILWPS,'NEW','FS',0,'DEL')
          OPSTWP = .TRUE.
11     CONTINUE       

*      variables in WRROUT.CMN
       MYLVWP = 0.
       MYSTWP = 0.
       MYSOWP = 0.
       VCYWP = 0.
*      initialization number of runs and means
       IWRUN = 0
       MSOW = 0.
       MIDEM = 0.
       MYLV = 0.
       MYST = 0.
       MYSO = 0.
       MYAG = 0.
       MLAI = 0.
       MHI = 0.
       MRAT = 0.
       MRAG = 0.
       MRYL = 0.
       MEVS = 0.
       MTRA = 0.
       MTRC = 0.
       MLOS = 0.
       MDUR = 0.
       MRAIN = 0.
       MDEL = 0.
*      initialization sums
       SUMSOW = 0.
       ISUMEM = 0
       SUMYLV = 0.
       SUMYST = 0.
       SUMYSO = 0.
       SUMYAG = 0.
       SUMLAI = 0.
       SUMHI  = 0.
       SUMRAT = 0.
       SUMRAG = 0.
       SUMRYL = 0.
       SUMEVS = 0.
       SUMTRA = 0.
       SUMTRC = 0.
       SUMLOS = 0.
       SUMDUR = 0.
*      mean water content, total water content at end, and rain 
       SUMMWC = 0.
       SUMTWE = 0.
       SUMRN  = 0.
       SUMDEL = 0.

*      initialization square sums 
       S2MYLV = 0.
       S2MYST = 0.
       S2MYSO = 0.
       S2MYAG = 0.
       S2MLAI = 0.
       S2MHI  = 0.
       S2MRAT = 0.
       S2MRAG = 0.
       S2MRYL = 0.
       S2MEVS = 0.
       S2MTRA = 0.
       S2MTRC = 0.
       S2MLOS = 0.
       S2MDUR = 0.
       S2MMWC = 0.
       S2MTWE = 0.
       S2MRN  = 0.
       S2MDEL = 0.

*     initialization standard deviations
       SDYLV = 0.
       SDYST = 0.
       SDYSO = 0.
       SDYAG = 0.
       SDLAI = 0.
       SDHI  = 0.
       SDEVS = 0.
       SDTRA = 0.
       SDTRC = 0.
       SDLOS = 0.
       SDDUR = 0.
       SDMWC = 0.
       SDTWE = 0.
       SDRAIN = 0.
       SDDEL = 0.
*     initialization variation coefficients
       VCYLV = 0.
       VCYST = 0.
       VCYSO = 0.
       VCYAG = 0.
       VCLAI = 0.
       VCHI  = 0.
       VCEVS = 0.
       VCTRA = 0.
       VCTRC = 0.
       VCLOS = 0.
       VCDUR = 0.
*!       VCMWC = 0.
*!       VCTWE = 0.
       VCRAIN = 0.
       VCDEL = 0.

*______print subheadings of output files

       IF (ISET.EQ.ISTSET .OR. (INYR.EQ.1 .AND. INYEAR.GT.3)) THEN
          IDETAI = 2
          CALL PRHEAD 
     &    (MODE,   RUNNAM, 
     &     FILWPS, FILRER, VRSION, CLMNAM, CLFILE, CRPNAM, CRFILE, 
     &     RAFILE, RAINAM, SOLNAM, SOFILE,
     &     IDETAI, ISET, ISTSET, INSETS, INYR,  INYEAR,   INR, INYRG, 
     &     IUOUT,  IUOF,  IUOSUM, IUSTPP, IUSTWP, 
     &     IWB, IOX, IASYR, IASYRR, IRNDAT, 
     &     ISTCHO, IDEM, IDESOW, IDLSOW)

          IF(IZT.EQ.1)THEN
            GRONDW='    GROUNDWATER'
          ELSE
            GRONDW=' NO GROUNDWATER'
         END IF
         WRITE (IUSTWP,'(A)') GRONDW

       WRITE (IUSTWP,3406)
3406   FORMAT('**',/,'**',/,'** SUMMARY WATER-LIMITED PRODUCTION',/,
     &       '** YEAR RUNNAM SET SOW EM ',
     &       ' DUR   TWLV   TWST   TWSO    TAGP   LAIM HINDEX ',
     & ' TRC RAINT DELWAT  TRAT EVSOL  LOSST  TSR  RYLD  RAGP')
       ENDIF

*-----------------------------------------------------------------------
100    CONTINUE

*      extra output variable
       RESIDU = YLVWL + YSTWL

*      avoid dividing by zero in case of no yield in potential situation
       IF(YAGPP.GE.0.01)THEN
       RELAGP = YAGWL/YAGPP*100.
       ELSE
       RELAGP = 0.
       END IF     
       IF(YSOWL.GE.0.01)THEN
       RELYLD = YSOWL/YSOPP*100.
       ELSE
       RELYLD = 0.
       END IF

*      output of WATFD
       IF (IZT.EQ.0) THEN
         TRAT = TRATF
         EVST = EVSTF
         TSR = TSRF
         RAINT = RAINTF
         PERCT = PERCTF
         LOSST = LOSSTF
         DELWAT = WF + WLOWF - (WIF + WLOWIF)
         MWC = MWCF
         TWE = TWEF
       ELSEIF (IZT.EQ.1) THEN
         TRAT = TRATG
         EVST = EVSTG
         TSR = TSRG
         RAINT = RAINTG
         PERCT = PERCTG
         LOSST = DRAITG
         DELWAT = WFING + WZFING - (WIG + WZIG)
       ENDIF

*      one line of output for simulation results per growing season
*      print final yield, harvest index, transpiration coefficient
*      and accumulated terms of the water balance
       WRITE(IUSTWP,3407) 
     $           IASYR,RUNNAM,ISET,IDSOW,IDEM, DURWL,
     $           YLVWL,YSTWL,YSOWL,YAGWL,LAMXWL,HIWL,TRCWL,
     $           RAINT,DELWAT,TRAT,EVST,LOSST,TSR,RELYLD,RELAGP
3407   FORMAT(1X,I4,1X,A6,1X,3I4,F6.0,2F7.0,2F8.0,2F6.2,F6.0,8F6.1)

*      summing the water-limited yields
*      counting the WATER LIMITED runs and summing the relevant statistical
*      variables
       IWRUN  = IWRUN+1
       SUMSOW = SUMSOW + IDSOW
       ISUMEM = ISUMEM + IDEM 
       SUMYLV = SUMYLV + YLVWL
       SUMYST = SUMYST + YSTWL
       SUMYSO = SUMYSO + YSOWL
       SUMYAG = SUMYAG + YAGWL
       SUMLAI = SUMLAI + LAMXWL
       SUMHI  = SUMHI  + HIWL 
       SUMRAT = SUMRAT + RATWL 
       SUMRAG = SUMRAG + RELAGP 
       SUMRYL = SUMRYL + RELYLD 
       SUMEVS = SUMEVS + EVST
       SUMTRA = SUMTRA + TRAT
       SUMTRC = SUMTRC + TRCWL
       SUMLOS = SUMLOS + LOSST
       SUMDUR = SUMDUR + DURWL 
       SUMMWC = SUMMWC + MWC 
       SUMTWE = SUMTWE + TWE 
       SUMRN  = SUMRN  + RAINT  
       SUMDEL = SUMDEL + DELWAT

*      Square sums for the same variables
       S2MYLV = S2MYLV + YLVWL*YLVWL
       S2MYST = S2MYST + YSTWL*YSTWL
       S2MYSO = S2MYSO + YSOWL*YSOWL
       S2MYAG = S2MYAG + YAGWL*YAGWL
       S2MLAI = S2MLAI + LAMXWL*LAMXWL
       S2MHI  = S2MHI  + HIWL*HIWL
       S2MRAT = S2MRAT + RATWL*RATWL 
       S2MRAG = S2MRAG + RELAGP*RELAGP 
       S2MRYL = S2MRYL + RELYLD*RELYLD 
       S2MEVS = S2MEVS + EVST*EVST
       S2MTRA = S2MTRA + TRAT*TRAT
       S2MTRC = S2MTRC + TRCWL*TRCWL
       S2MLOS = S2MLOS + LOSST*LOSST
       S2MDUR = S2MDUR + DURWL*DURWL
       S2MMWC = S2MMWC + MWC*MWC
       S2MTWE = S2MTWE + TWE*TWE
       S2MRN  = S2MRN  + RAINT*RAINT
       S2MDEL = S2MDEL + DELWAT*DELWAT

       IF (.NOT.TERMNL) RETURN

*      statistical elaboration of the results

*      means
       MSOW = SUMSOW/IWRUN
       MIDEM = ISUMEM/IWRUN
       MYLV = SUMYLV/IWRUN 
       MYST = SUMYST/IWRUN
       MYSO = SUMYSO/IWRUN
       MYAG = SUMYAG/IWRUN 
       MLAI = SUMLAI/IWRUN 
       MHI = SUMHI/IWRUN
       MRAG = SUMRAG/IWRUN 
       MRYL = SUMRYL/IWRUN 
       MEVS = SUMEVS/IWRUN 
       MTRA = SUMTRA/IWRUN 
       MTRC = SUMTRC/IWRUN 
       MLOS = SUMLOS/IWRUN 
       MDUR = SUMDUR/IWRUN 
       MMWC = SUMMWC/IWRUN
       MTWE = SUMTWE/IWRUN 
       MRAIN  = SUMRN/IWRUN  
       MDEL  = SUMDEL/IWRUN  

       IF (IWRUN.LT.3) GOTO 40
*      standard deviations

*      numeric calculations not allowed to gain negative result

       SDYLV = SQRT(MAX(0.,(S2MYLV - SUMYLV*SUMYLV/IWRUN)/(IWRUN-1)))
       SDYST = SQRT(MAX(0.,(S2MYST - SUMYST*SUMYST/IWRUN)/(IWRUN-1)))
       SDYSO = SQRT(MAX(0.,(S2MYSO - SUMYSO*SUMYSO/IWRUN)/(IWRUN-1)))
       SDYAG = SQRT(MAX(0.,(S2MYAG - SUMYAG*SUMYAG/IWRUN)/(IWRUN-1)))
       SDLAI = SQRT(MAX(0.,(S2MLAI - SUMLAI*SUMLAI/IWRUN)/(IWRUN-1)))
       SDHI  = SQRT(MAX(0.,(S2MHI  - SUMHI *SUMHI/IWRUN)/(IWRUN-1)))
       SDRAG = SQRT(MAX(0.,(S2MRAG - SUMRAG*SUMRAG/IWRUN)/(IWRUN-1)))
       SDRYL = SQRT(MAX(0.,(S2MRYL - SUMRYL*SUMRYL/IWRUN)/(IWRUN-1)))
       SDEVS = SQRT(MAX(0.,(S2MEVS - SUMEVS*SUMEVS/IWRUN)/(IWRUN-1)))
       SDTRA = SQRT(MAX(0.,(S2MTRA - SUMTRA*SUMTRA/IWRUN)/(IWRUN-1)))
       SDTRC = SQRT(MAX(0.,(S2MTRC - SUMTRC*SUMTRC/IWRUN)/(IWRUN-1)))
       SDLOS = SQRT(MAX(0.,(S2MLOS - SUMLOS*SUMLOS/IWRUN)/(IWRUN-1)))
       SDDUR = SQRT(MAX(0.,(S2MDUR - SUMDUR*SUMDUR/IWRUN)/(IWRUN-1)))
       SDMWC = SQRT(MAX(0.,(S2MMWC - SUMMWC*SUMMWC/IWRUN)/(IWRUN-1)))
       SDTWE = SQRT(MAX(0.,(S2MTWE - SUMTWE*SUMTWE/IWRUN)/(IWRUN-1)))
       SDRAIN = SQRT(MAX(0.,(S2MRN - SUMRN*SUMRN/IWRUN)/(IWRUN-1)))
       SDDEL = SQRT(MAX(0.,(S2MDEL - SUMDEL*SUMDEL/IWRUN)/(IWRUN-1)))

*      variation coefficients in %
       IF (MYLV.GE.0.1) VCYLV = 100.*SDYLV/MYLV
       IF (MYST.GE.0.1) VCYST = 100.*SDYST/MYST
       IF (MYSO.GE.0.1) VCYSO = 100.*SDYSO/MYSO
       IF (MYAG.GE.0.1) VCYAG = 100.*SDYAG/MYAG
       IF (MLAI.GE.0.1) VCLAI = 100.*SDLAI/MLAI
       IF (MHI.GE.0.1)  VCHI  = 100.*SDHI/MHI
       IF (MEVS.GE.0.1) VCEVS = 100.*SDEVS/MEVS
       IF (MTRA.GE.0.1) VCTRA = 100.*SDTRA/MTRA
       IF (MTRC.GE.0.1) VCTRC = 100.*SDTRC/MTRC
       IF (MLOS.GE.0.1) VCLOS = 100.*SDLOS/MLOS
       IF (MDUR.GE.0.1) VCDUR = 100.*SDDUR/MDUR
*!       VCMWC = 100.*SDMWC/MMWC
*!       VCTWE = 100.*SDTWE/MTWE
       IF (MRAIN.GE.0.1) VCRAIN = 100.*SDRAIN/MRAIN
       VCDEL = 100.*SDDEL/MAX(0.01,ABS(MDEL))

40     CONTINUE
*      results on file
          WRITE(IUSTWP,3408) 
     $     IWRUN,MDUR,MYLV,MYST,MYSO,MYAG,MLAI,MHI,MTRC,
     $     MRAIN,MDEL,MTRA,MEVS,MLOS,MRYL,MRAG
          WRITE(IUSTWP,3409) SDDUR,SDYLV,SDYST,SDYSO,SDYAG,SDLAI,SDHI,
     $     SDTRC,SDRAIN,SDDEL,SDTRA,SDEVS,SDLOS,SDRYL,SDRAG              
          WRITE(IUSTWP,3410) VCDUR,VCYLV,VCYST,VCYSO,VCYAG,VCLAI,VCHI,
     $     VCTRC,VCRAIN,VCDEL,VCTRA,VCEVS,VCLOS

       IF (RQUIRD) THEN
*      results on summary file
       WRITE (IUOSUM,'(1X,2A,1X,I2,3F6.0,
     &  3F7.0,F6.2,F7.0,3F6.2,2F7.2,F6.0,F5.0)')
     &  'WP ',RUNNAM,IZT,MSOW,MIDEM,MDUR,
     &  MYLV,MYST,MYSO,VCYSO,MYAG,VCYAG,MLAI,MHI,MRYL,MRAG,
     &  MTRC,RDMSOL
       END IF

*      variables in WRROUT.CMN
       MYLVWP = MYLV
       MYSTWP = MYST
       MYSOWP = MYSO
       VCYWP = VCYSO
*!!!       IF (ISTATO.EQ.1) CLOSE (IUSTWP)
       RETURN

*-----------------------------------------------------------------------
*      format statements
*-----------------------------------------------------------------------

3408    FORMAT(/,/,1X,'MEANS over ',I2,' years :',4X,
     &     F5.0,2F7.0,2F8.0,2F6.2,F6.0,5F6.1,6X,2F6.1)
3409    FORMAT(1X,'STANDARD DEVIATIONS     :',F5.0,
     &     2F7.0,2F8.0,2F6.2,F6.0,5F6.1,6X,2F6.2)
3410    FORMAT(1X,'VARIATION COEFFICIENTS :',F5.2,1X,
     &     2F7.2,2F8.2,2F6.2,6F6.1)

       END
