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
      SUBROUTINE STATPP (INIPP, TERMNL,
     &      RUNNAM, CLMNAM, CLFILE, RAINAM, RAFILE, CRPNAM, CRFILE, 
     &      SOLNAM, SOFILE, VRSION, RQUIRD,
     &      FILRER, FILPPS, 
     &      IUOUT,  IUOF,  IUOSUM, IUSTPP, IUSTWP, 
     &      MODE, ISET, ISTSET, INSETS, INYR,  INYEAR,   INR, INYRG,  
     &      IWB, IOX, IASYR, IASYRR, IRNDAT, 
     &      ISTCHO, IDSOW, IDESOW, IDLSOW, IDEM, INDTEM, 
     &      IZT)

*-----------------------------------------------------------------------
*     Statistics of potential production.
*     Subroutine for statistical treatment of yield variables, simulated  
*     in a series of simulation runs with WOFOST
*     The yield data refer to one crop and either a time series for one 
*     weather station (daily weather), or a series over different 
*     stations or station-years (mean monthly weather).
*-----------------------------------------------------------------------
*     +---------------------------------------------------------------+
*     | MODIFICATION                    								|
*     | Date:         02-06-1998										|
*     | Author:       Hendrik Boogaard								|
*     | Reason:       Removal of IUSCR + removal of ICROP and ISOIL	|
*     +---------------------------------------------------------------+

*     declarations, common blocks
      IMPLICIT REAL(A-H,J-Z)
      IMPLICIT INTEGER(I)
      LOGICAL INIPP, TERMNL, RQUIRD
      LOGICAL OPSTPP
      CHARACTER*(*) FILRER, FILPPS
      CHARACTER*(*) CRFILE, CRPNAM
      CHARACTER*(*) SOFILE, SOLNAM
      CHARACTER*(*) CLFILE, CLMNAM
      CHARACTER*(*) RAINAM, RAFILE
      CHARACTER*(*) MODE, RUNNAM, VRSION

      INTEGER IUOUT, IUOF,  IUOSUM, IUSTPP, IUSTWP
      INTEGER ISET, ISTSET, INSETS, INYR,  INYEAR,   INR, INYRG
      INTEGER IWB, IOX, IASYR, IASYRR, IRNDAT
      INTEGER ISTCHO, IDSOW, IDESOW, IDLSOW, IDEM, INDTEM 
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
*      output water balance variables from subroutine WATPP
       COMMON/WATPPO/ TRATX,EVWTPP,EVSTPP

*     local parameters
      INTEGER ISUMEM, IDFLWR 

       SAVE

       DATA OPSTPP /.FALSE./

       IF (.NOT.INIPP) GOTO 100
       INIPP = .FALSE.

       IF (OPSTPP) GOTO 11
*      open output file for potential production statistics
          CALL LOWERC (FILPPS)
          CALL FOPENG (IUSTPP,FILPPS,'NEW','FS',0,'DEL')
       OPSTPP = .TRUE.
11     CONTINUE       

*      variables in WRROUT.CMN
       MYLVPP = 0.
       MYSTPP = 0.
       MYSOPP = 0.
       VCYPP = 0.

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
       MEVS = 0.
       MTRA = 0.
       MTRC = 0.
       MDUR = 0.

*      initialization sums
       SUMSOW = 0.
       ISUMEM = 0
       SUMYLV = 0.
       SUMYST = 0.
       SUMYSO = 0.
       SUMYAG = 0.
       SUMLAI = 0.
       SUMHI = 0.
       ISUMEM = 0
       SUMRAT = 0.
       SUMEVS = 0.
       SUMTRA = 0.
       SUMTRC = 0.
       SUMDUR = 0.

*     initialization square sums 
       S2MYLV = 0.
       S2MYST = 0.
       S2MYSO = 0.
       S2MYAG = 0.
       S2MLAI = 0.
       S2MHI = 0.
       S2MRAT = 0.
       S2MEVS = 0.
       S2MTRA = 0.
       S2MTRC = 0.
       S2MDUR = 0.

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
       SDDUR = 0.
  
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
       VCDUR = 0.


*______print subheadings of output files

       IF (ISET.EQ.ISTSET .OR. (INYR.EQ.1 .AND. INYEAR.GE.3)) THEN
           IDETAI = 2
           CALL PRHEAD (MODE, RUNNAM, 
     &     FILPPS, FILRER, VRSION, CLMNAM, CLFILE, CRPNAM, CRFILE, 
     &     RAFILE, RAINAM, SOLNAM, SOFILE,
     &     IDETAI,ISET, ISTSET, INSETS, INYR,  INYEAR,   INR, INYRG,  
     &     IUOUT, IUOF,  IUOSUM, IUSTPP, IUSTWP, 
     &     IWB, IOX, IASYR, IASYRR, IRNDAT, 
     &     ISTCHO, IDEM, IDESOW, IDLSOW)


       WRITE (IUSTPP,3304)
3304   FORMAT('**',/,'**',/,'** SUMMARY POTENTIAL PRODUCTION',/,
     &          '** YR RUNNAM  SET SOW --> EM ANT FLWR',
     &          ' DUR HALT  TWRT   TWLV   TWST   TWSO',
     & '   TAGP   LAIM HINDEX TRC    GASST  MREST TRANSP EVSOL')
      ENDIF


*-----------------------------------------------------------------------
100    CONTINUE

*      formal parameter INDTEM has no value (should be known from WOFSIM)
       IF (IDSOW.GT.0) THEN
          INDTEM = MOD((365+IDEM-IDSOW),365)
       ELSE
          INDTEM = 0
       ENDIF
*      extra output variables : dates of flowering and end of season
       IDFLWR = MOD ((IDEM + IDFLPP),365)
       DHALT = REAL (MOD((IDEM + DURPP),365.))

*      one line of output for simulation results per growing season
       WRITE(IUSTPP,3305) 
     &           IASYR,RUNNAM,ISET,IDSOW,INDTEM,IDEM,IDFLPP,
     &           IDFLWR, DURPP,DHALT,YRTPP,YLVPP,YSTPP,YSOPP,
     &           YAGPP,LAMXPP,HIPP,TRCPP,GASPP,RESPP,TRATPP,EVSTPP
3305   FORMAT
     &  (1X,I4,1X,A6,1X,2I4,I3,3I4,2F5.0,5F7.0,2F6.2,F5.0,2F8.0,2F6.1)


*      summing the potential yields
*      counting the potential runs  and summing the relevant statistical
*      variables
       IWRUN  = IWRUN+1
       SUMSOW = SUMSOW + IDSOW
       ISUMEM = ISUMEM + IDEM
       SUMYLV = SUMYLV + YLVPP
       SUMYST = SUMYST + YSTPP
       SUMYSO = SUMYSO + YSOPP
       SUMYAG = SUMYAG + YAGPP
       SUMLAI = SUMLAI + LAMXPP
       SUMHI  = SUMHI  + HIPP
       SUMRAT = SUMRAT + RATPP
       SUMEVS = SUMEVS + EVSTPP
       SUMTRA = SUMTRA + TRATPP
       SUMTRC = SUMTRC + TRCPP
       SUMDUR = SUMDUR + DURPP

*      Square sums for the same variables

       S2MYLV = S2MYLV + YLVPP*YLVPP
       S2MYST = S2MYST + YSTPP*YSTPP
       S2MYSO = S2MYSO + YSOPP*YSOPP
       S2MYAG = S2MYAG + YAGPP*YAGPP
       S2MLAI = S2MLAI + LAMXPP*LAMXPP
       S2MHI  = S2MHI  + HIPP*HIPP
       S2MRAT = S2MRAT + RATPP*RATPP
       S2MEVS = S2MEVS + EVSTPP*EVSTPP
       S2MTRA = S2MTRA + TRATPP*TRATPP
       S2MTRC = S2MTRC + TRCPP*TRCPP
       S2MDUR = S2MDUR + DURPP*DURPP

       IF (.NOT.TERMNL) RETURN

*      Statistical elaboration of the results

*      means
       MSOW = SUMSOW/IWRUN
       MIDEM = ISUMEM/IWRUN
       MYLV = SUMYLV/IWRUN 
       MYST = SUMYST/IWRUN
       MYSO = SUMYSO/IWRUN
       MYAG = SUMYAG/IWRUN 
       MLAI = SUMLAI/IWRUN 
       MHI = SUMHI/IWRUN
       MEVS = SUMEVS/IWRUN 
       MTRA = SUMTRA/IWRUN 
       MTRC = SUMTRC/IWRUN 
       MDUR = SUMDUR/IWRUN 

       IF (IWRUN.LT.3) GOTO 40
*      standard deviations
*       numeric calculations not allowed to gain negative result
       
       SDYLV = SQRT(MAX(0.,(S2MYLV - SUMYLV*SUMYLV/IWRUN)/(IWRUN-1)))
       SDYST = SQRT(MAX(0.,(S2MYST - SUMYST*SUMYST/IWRUN)/(IWRUN-1)))
       SDYSO = SQRT(MAX(0.,(S2MYSO - SUMYSO*SUMYSO/IWRUN)/(IWRUN-1)))
       SDYAG = SQRT(MAX(0.,(S2MYAG - SUMYAG*SUMYAG/IWRUN)/(IWRUN-1)))
       SDLAI = SQRT(MAX(0.,(S2MLAI - SUMLAI*SUMLAI/IWRUN)/(IWRUN-1)))
       SDHI  = SQRT(MAX(0.,(S2MHI  - SUMHI *SUMHI/IWRUN)/(IWRUN-1)))
       SDEVS = SQRT(MAX(0.,(S2MEVS - SUMEVS*SUMEVS/IWRUN)/(IWRUN-1)))
       SDTRA = SQRT(MAX(0.,(S2MTRA - SUMTRA*SUMTRA/IWRUN)/(IWRUN-1)))
       SDTRC = SQRT(MAX(0.,(S2MTRC - SUMTRC*SUMTRC/IWRUN)/(IWRUN-1)))
       SDDUR = SQRT(MAX(0.,(S2MDUR - SUMDUR*SUMDUR/IWRUN)/(IWRUN-1)))

*      variation coefficients in %
       IF (MYLV.GE.0.1) VCYLV = 100.*SDYLV/MYLV
       IF (MYST.GE.0.1) VCYST = 100.*SDYST/MYST
       IF (MYSO.GE.0.1) VCYSO = 100.*SDYSO/MYSO
       IF (MYAG.GE.0.1) VCYAG = 100.*SDYAG/MYAG
       IF (MLAI.GE.0.1) VCLAI = 100.*SDLAI/MLAI
       IF (MHI .GE.0.1) VCHI  = 100.*SDHI/MHI
       IF (MEVS.GE.0.1) VCEVS = 100.*SDEVS/MEVS
       IF (MTRA.GE.0.1) VCTRA = 100.*SDTRA/MTRA
       IF (MTRC.GE.0.1) VCTRC = 100.*SDTRC/MTRC
       IF (MDUR.GE.0.1) VCDUR = 100.*SDDUR/MDUR

40     CONTINUE

*      results on file
       WRITE(IUSTPP,3308) 
     &    IWRUN,MDUR,MYLV,MYST,MYSO,MYAG,MLAI,MHI,MTRC,MTRA,MEVS
       WRITE(IUSTPP,3309) 
     &    SDDUR,SDYLV,SDYST,SDYSO,SDYAG,SDLAI,SDHI,SDTRC,SDTRA,SDEVS
       WRITE(IUSTPP,3310) 
     &    VCDUR,VCYLV,VCYST,VCYSO,VCYAG,VCLAI,VCHI,VCTRC,VCTRA,VCEVS

       IF (RQUIRD) THEN
*      results on summary files
       WRITE (IUOSUM,'(1X,2A,1X,I2,3F6.0,
     &  3F7.0,F6.2,F7.0,3F6.2)')
     &  'PP ',RUNNAM,IZT,MSOW,MIDEM,
     &  MDUR,MYLV,MYST,MYSO,VCYSO,MYAG,VCYAG,MLAI,MHI
       END IF

*      variables in WRROUT.CMN
       MYLVPP = MYLV
       MYSTPP = MYST
       MYSOPP = MYSO
       VCYPP = VCYSO

*!!!!       IF (ISTATO.EQ.1) CLOSE (IUSTPP)
       RETURN

*-----------------------------------------------------------------------
*      format statements
*-----------------------------------------------------------------------

3308    FORMAT('**',/,'**',/,'** MEANS over ',I2,' years : ',11X,
     &     F5.0,12X,4F7.0,2F6.2,F5.0,16X,F6.2,F6.1)
3309    FORMAT('** STANDARD DEVIATIONS : ',10X,F6.0,12X,
     &     4F7.0,2F6.2,F5.1,16X,F6.2,F6.1)
3310    FORMAT('** VARIATION COEFFICIENTS  : ',5X,F6.1,13X,
     &     4F7.2,2F6.2,F6.2,14X,2F7.2)
       END
