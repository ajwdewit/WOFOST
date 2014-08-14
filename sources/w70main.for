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
      PROGRAM W70MAIN

*     Is the driving routine for running the WOFOST crop growth 
*     simulation model.
*     The FSE system for reading data has been applied for the 
*     standard crop data (directory CROPD), standard physical soil 
*     data (directory SOILD), location-specific data on agrohydrological 
*     conditions (file SITE.DAT on directory RUNIO) and run-specific 
*     information on crop calendar, weather data specification and 
*     kind of output (file TIMER.DAT on directory RUNIO).
*     All these data can be varied across successive model runs by 
*     specifying them in the file RERUNS.DAT on directory RUNIO.

*    +----------------------------------------------------------------+
*    |MODE   - Type of calculation that should be carried out:        |
*    |         NO_RERUN: Runs are performed with just the data from   |
*    |                   the files RUNOPT.DAT, TIMER.DAT and SITE.DAT |
*    |         RERUN:    Runs are performed with data from the files  |
*    |                   RUNOPT.DAT, TIMER.DAT,SITE.DAT and RERUNS.DAT|
*    +----------------------------------------------------------------+
*     Subroutine called: 
*      - from library TTUTIL: ERROR

*     Author: Kees van Diepen and Daniel van Kraalingen
*     Date  : October 1992
*    +----------------------------------------------------------------+
*    | version:      1.2                                              |
*    | author:       Tamme van der Wal                                |
*    | date:         23 July 1997                                     |
*    | reason:       improvement of driver routines; getting rid of   |
*    |               redundant user-interface. All required parameters|
*    |               are now read in through files.                   |
*    | modification: removal of user-interactive questions and replace|
*    |               this by reading in from file. 					|
*    |               W60MAIN, WOF60 and SELOUT are now integrated in  |
*    |               one routine W61MAIN                              |
*    +----------------------------------------------------------------+
*    | MODIFICATION                                                   |
*    | author:       Hendrik Boogaard                                 |
*    | date:         02-06-1998                                       |
*    | reason:       - FSEOPT                                         |
*    |               - variables removed                              |
*    |               - earlier start waterbalance                     |
*    | modification:                                                  |
*    | - variables                                                    |
*    |           - FILSUM (runopt.dat                                 |
*    |           - IOUT (timer file) output to                        |
*    |           + statements with opening detailed output (PRDEL>0)  |
*    |             or opening log file (PRDEL=1) improved             |
*    |           - IUSCR/WOFOLD/OPNOUF removed                        |
*    | - earlier start water balance                                  |
*    |           - variable ISDAY and NOCROP introduced	              |
*    |            ISDAY from timer file = start waterbalance          |
*    |            NOCROP logical, indicates wether there's crop       |
*    |           - adjustement of call WOFSIM (ISDAY, NOCROP)         |
*    |           - determination if there's no crop when simulating   |
*    |             the water limited production (isday.ne.idem)       |
*    +----------------------------------------------------------------+
*    | MODIFICATION                                                   |
*    | author:       Hendrik Boogaard                                 |
*    | date:         15-06-1998                                       |
*    | reason:       error determination end data                     |
*    |               removal ICROP,ISOIL                              |
*    |               adjustement control when to write summary nutrie.|
*    | modification:                                                  |
*    | part of source code was diseappeared, variable IENCHO has to   |
*    | used for determining right values for IDURMX and IENDAY        |
*    +----------------------------------------------------------------+
*    | MODIFICATION                                                   |
*    | author:       Allard de Wit                                    |
*    | date:         07-01-2011                                       |
*    | reason:       Clean up of code, allowing platform independent  |
*    |               compilation (windows, linux, Mac OSX)            |
*    |               Removal of FSEOPT from source tree               |
*    | modification:                                                  |
*    | All filenames changed to lowercase, changes to source files    |
*    | to take into account lower case names for the include files.   |
*    | FSEOPT was removed from the source tree as it is strongly      |
*    | outdated and hardly ever used.                                 |
*    | CRPNAM and SOLNAM are now read from the crop and soil files    |
*    +----------------------------------------------------------------+


      IMPLICIT NONE
      INTEGER IOPT2, ILEN, IL1
      INTEGER IUT, IULOG
      CHARACTER MODE*8
      COMMON /CGMSYS/ DIRDAT
      CHARACTER*250  DIRDAT

      CHARACTER TMPSTR*250
      CHARACTER*250 RUNFIL, FIXNAM, FILSUM
**
      CHARACTER*250  DBMDIR,DBRDIR,WTRDIR,SOLDIR,CRPDIR,CLMDIR
      CHARACTER*250  RUNDIR,OUTDIR,DCGDIR,DRVDIR,GEODIR,TMPDIR
      LOGICAL NEWPP, INIT, NOCROP 

      INTEGER IDFLPP, IDFLWL, IDWET, IDDRY
      COMMON /CRPSI/ YLVPP, YSTPP , YSOPP, HIPP  , RATPP, IDFLPP,
     &               DURPP, TRATPP, TRCPP, YRTPP , YAGPP, GASPP ,
     &               RESPP, LAMXPP, YLVWL, YSTWL , YSOWL, HIWL  ,
     &               RATWL, IDFLWL, DURWL, TRATWL, TRCWL, YRTWL ,
     &               YAGWL, GASWL , RESWL, LAMXWL, IDWET, IDDRY
**
*     crop
      CHARACTER*250 CRFILE, CRPNAM

*     soil
      INTEGER IZT, IFUNRN, IOX, IWB, IBAL, IDRAIN
      INTEGER IOXPP, IOXWL
      CHARACTER*250 SOFILE, SOLNAM
      REAL SSI, SMLIM 

*     weather
      INTEGER ISYR, IASYR, IWEATH, ISTN, ISDAY
      CHARACTER*250 CLFILE, CLMNAM, CNTR

*     rain
      INTEGER IRNDAT, ISYRR, IASYRR, IYEARR, IROK(0:5,0:3)
      INTEGER INYRG
      LOGICAL RSETRG, RSETRD
      CHARACTER*250 RAINAM, RAFILE
      REAL NOTINF

*     nutrients
      REAL NBASE, NREC, PBASE, PREC, KBASE, KREC
      REAL NBAS, PBAS, KBAS    

*     States and rates
      REAL YLVWLT, YSTWLT, YSOWLT, HIWLT, RATWLT
      REAL YLVWL, YSTWL, YSOWL, HIWL, RATWL, MYLVWL, MYSTWL, MYSOWL
      REAL MHIWL, MRATWL
      REAL YLVPP, YSTPP, YSOPP, DURPP
      REAL LVBAS1, STBAS1, SOBAS1
      REAL RATPP, RATBAS, HIPP, HIBAS    
      REAL NFERTO, NFERTW, PFERTO, PFERTW, KFERTO, KFERTW
      REAL DD, ZTI, SSMAX, RDMSOL
      REAL LAMXWL, RESWL, GASWL, YAGWL, YRTWL, TRCWL, TRATWL, DURWL
      REAL LAMXPP, RESPP, GASPP, YAGPP, YRTPP, TRCPP, TRATPP, WAV 

*     run control
      INTEGER ISER, IREP, IREQ
      LOGICAL OKNOWN, SERIES, REPORT, RQUIRD
      LOGICAL INIPP,  INIWP, LASTPP, LASTWP

*     choices start and end days
      INTEGER       ISTCHO, IDAYB, IDEM, IDSOW, IDESOW, IDLSOW, ISTATE
      INTEGER       IENCHO, IDAYEN, IDURMX

*     unit numbers for screen, output file, plant, soil, weather,
*     rain, reruns, batch and temporary files
      INTEGER       IUOF, IUPL, IUSO, IUWE, IURA, IURE
      INTEGER       IUOUT, IUSTPP, IUSTWP, IUOSUM

*     run control and miscellaneous
      INTEGER       IPRODL, IWRUN, I1, I2, I3
      INTEGER       ISTSET, ISET, ILN39
      INTEGER       INSETS, INR, INYR, INYEAR
      INTEGER       INDTEM, IDETAI
      INTEGER       ILRUN, ILFIX, ILOUT, IL2
      LOGICAL       OPNST
      REAL          PRDEL

*     file names
      CHARACTER*250 WOFOUT, WOFRER, TIMFIL, SITFIL, SUMFIL

*     full file names including directory name
      CHARACTER*250 FILRER, FILPPS, FILWPS
      CHARACTER     VRSION*80, RUNNAM*6

*     command line handling
      INTEGER       ARGCNT

      SAVE

*     +----------------------------------------------------------------+
*     | Data statements                                                |
*     +----------------------------------------------------------------+

*     default data
      DATA IPRODL /2/, IOXPP /0/, IOXWL /0/

*     this array defines the valid weather and rainfall options, weather
*     options horizontal, rainfall options vertical, 0=illegal option,
*     1=valid option

      DATA IROK /0, 1, 0, 0, 0, 0,
     &           1, 1, 0, 0, 0, 0,
     &           1, 1, 1, 1, 1, 1,
     &           0, 0, 1, 1, 1, 1/

      DATA RUNFIL /'runopt.dat'/
      DATA VRSION /'**WOFOST version 7.1.7, release September 2013'/
*     unit numbers
      DATA IUOF /50/, IUPL /55/, IUSO /60/, IUWE /65/
      DATA IUSTPP /33/, IUSTWP /34/, IUOSUM /39/
*     ATTENTION: units 91 and 92 are used by cabo weather system
*     Unit numbers > 99 not allowed! 

      DATA IURA /70/, IURE /75/, IULOG /80/, IUT /86/
      DATA INIT /.FALSE./
      DATA RQUIRD /.FALSE./
      DATA SERIES /.FALSE./, OKNOWN /.FALSE./
      DATA INIPP /.FALSE./, INIWP /.FALSE./
      DATA REPORT /.FALSE./, WOFRER /'reruns.dat'/
      DATA TIMFIL /'timer.dat'/,  SITFIL /'site.dat'/

      DATA OPNST /.FALSE./

*     +---------------------------------------------------------------+
*     |  Command line handling                                        |
*     +---------------------------------------------------------------+

      ARGCNT = COMMAND_ARGUMENT_COUNT() 

      IF (ARGCNT.EQ.0) THEN
        DIRDAT = 'direct.ini'
      ELSE IF (ARGCNT.EQ.1) THEN
        CALL GET_COMMAND_ARGUMENT(1, DIRDAT)
        DIRDAT = TRIM(DIRDAT)
      ELSE
        CALL ERROR ('W70MAIN','Invalid command line')
      END IF

*     +---------------------------------------------------------------+
*     |  Collect input data                                           |
*     +---------------------------------------------------------------+

* ---- Read directory for outputfiles and derived data from input file
* ---- unit number 10 (and 11) can be used freely here because
* ---- this is a main program, and unit 10 (and 11) are 'released' also.

      IL1 = ILEN(DIRDAT)
      CALL RDINIT (10,0,DIRDAT(1:IL1))
      CALL RDSCHA ('DBMDIR',DBMDIR)
      CALL RDSCHA ('DBRDIR',DBRDIR)
      CALL RDSCHA ('WTRDIR',WTRDIR)
      CALL RDSCHA ('SOLDIR',SOLDIR)
      CALL RDSCHA ('CRPDIR',CRPDIR)
      CALL RDSCHA ('CLMDIR',CLMDIR)
      CALL RDSCHA ('RUNDIR',RUNDIR)
      CALL RDSCHA ('OUTDIR',OUTDIR)
      CALL RDSCHA ('DRVDIR',DRVDIR)
      CALL RDSCHA ('DCGDIR',DCGDIR)
      CALL RDSCHA ('GEODIR',GEODIR)
      CALL RDSCHA ('TMPDIR',TMPDIR)
      CLOSE (10)

* ----determine lengths of directory strings
      IF (ILEN(DBMDIR).EQ.0) THEN
            CALL ERROR ('W70MAIN', 'DBMDIR is empty')
      ELSEIF (ILEN(DBRDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'DBRDIR is empty')
      ELSEIF (ILEN(WTRDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'WTRDIR is empty')
      ELSEIF (ILEN(SOLDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'SOLDIR is empty')
      ELSEIF (ILEN(CRPDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'CRPDIR is empty')
      ELSEIF (ILEN(CLMDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'CLMDIR is empty')
      ELSEIF (ILEN(RUNDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'RUNDIR is empty')
      ELSEIF (ILEN(OUTDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'OUTDIR is empty')
      ELSEIF (ILEN(DCGDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'DCGDIR is empty')
      ELSEIF (ILEN(DRVDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'DRVDIR is empty')
      ELSEIF (ILEN(GEODIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'GEODIR is empty')
      ELSEIF (ILEN(TMPDIR).EQ.0) THEN
             CALL ERROR ('W70MAIN', 'TMPDIR is empty')
      ENDIF

* ----Open logfile
      TMPSTR = RUNDIR(1:ILEN(RUNDIR))//'wofost.log'
      CALL LOWERC (TMPSTR)
      CALL FOPENG (IULOG,TMPSTR,'NEW','FS',0,'DEL')
      INIT   = .TRUE.

* ----specification of 'RUNOPT' group of input data 
      ILRUN = ILEN (RUNDIR)
      IF (ILRUN.EQ.0) THEN
            TMPSTR = RUNFIL
          ELSE
            TMPSTR = RUNDIR(1:ILRUN)//RUNFIL(1:ILEN(RUNFIL))
      ENDIF

      CALL LOWERC (TMPSTR)
* ----analyse RUNOPT input file 
      CALL RDINIT (11, IULOG, TMPSTR)
* ----get values from file
      CALL RDSINT ('IOPT2' , IOPT2 )
      CALL RDSINT ('IPRODL' , IPRODL )
      CALL RDSINT ('IOXWL'  , IOXWL  )
      CALL RDSCHA ('TIMFIL' , TIMFIL )
      CALL RDSCHA ('SITFIL' , SITFIL )
      CALL RDSCHA ('WOFRER' , WOFRER )
      CALL RDSCHA ('WOFOUT' , WOFOUT )
      CALL RDSINT ('SERIES' , ISER   )
      CALL RDSINT ('REPORT' , IREP   )
      CALL RDSCHA ('FIXNAM' , FIXNAM )
      CALL RDSINT ('RQUIRD' , IREQ   )

      CLOSE (11)
                   
      IF (IOPT2.EQ.2) THEN
        MODE = 'NO_RERUN'
      ELSE IF (IOPT2.EQ.4) THEN
        MODE = 'RERUN'
      ELSE IF (IOPT2.NE.2 .AND. IOPT2.NE.4) THEN
        CALL ERROR ('W70MAIN', 'Wrong run option, IOPT2')
      END IF 
        
      WRITE (*,'(/,1X,A,/)') VRSION

      IF (ISER.EQ.1) THEN 
        SERIES = .TRUE. 
      ELSE 
        SERIES = .FALSE. 
      ENDIF
      IF (IREP.EQ.1) THEN 
        REPORT = .TRUE. 
      ELSE 
        REPORT = .FALSE. 
      ENDIF
      IF (IREQ.EQ.1) THEN 
        RQUIRD = .TRUE. 
      ELSE 
        RQUIRD = .FALSE. 
      ENDIF

* ----Setting of run control parameters as dictated by MODE
      IF (MODE.EQ.'NO_RERUN') THEN
* ------No usage of rerun file
        ISTSET = 0
        INSETS = 0
      ELSE IF (MODE.EQ.'RERUN') THEN
* ------mode requires rerun file
        ISTSET = 1
        FILRER = RUNDIR(1:ILEN(RUNDIR))//WOFRER
        CALL LOWERC (FILRER)
* ------find the number of rerun sets
        CALL RDSETS (IURE, IULOG, FILRER, INSETS)
        IF (INSETS.EQ.0) CALL ERROR ('W70MAIN',
     &     'rerun file does not exist or is empty')
      END IF

*     +----------------------------------------------------------------+
*     | Major loop over all sets of input data                         |
*     +----------------------------------------------------------------+
      DO 20 ISET = ISTSET,INSETS

*       select default run (ISET=0) or rerun set (ISET>0)
        CALL RDFROM (ISET,.TRUE.)

*       +--------------------------------------------------------------+
*       | input section                                                |
*       +--------------------------------------------------------------+

* ------specification of 'timer' group of input data 
        TMPSTR = RUNDIR(1:ILEN(RUNDIR))//TIMFIL
        CALL LOWERC (TMPSTR)
*       analyse timer input file 
        CALL RDINIT (IUT, IULOG, TMPSTR)
*       get values from file
        CALL RDSCHA ('RUNNAM', RUNNAM)
        CALL RDSCHA ('CRFILE' ,CRFILE)
        TMPSTR = CRFILE
        I1 = ILEN (TMPSTR)
        CRFILE = CRPDIR(1:ILEN(CRPDIR))//TMPSTR(1:I1)
        CALL LOWERC (CRFILE)
        CALL RDSINT ('IWEATH', IWEATH)
        CALL RDSCHA ('CLFILE', CLFILE)
*       put directory before file name
        TMPSTR = CLFILE
        I1 = ILEN (TMPSTR)
        IF (IWEATH.EQ.0.OR.IWEATH.EQ.1) THEN
            CLFILE = CLMDIR(1:ILEN(CLMDIR))//TMPSTR(1:I1)
        ELSEIF (IWEATH.EQ.2) THEN
            CLFILE = WTRDIR(1:ILEN(WTRDIR))//TMPSTR(1:I1)
              I2 = SCAN(TMPSTR(1:I1), '0123456789')
              I3 = SCAN(TMPSTR(1:I1), '.')
              CNTR = TMPSTR(1:(I2-1))
            IF (I2.EQ.(I3-1)) THEN
              READ(TMPSTR(I2:(I3-1)),'(i1)') ISTN 
            ELSEIF (I2.EQ.(I3-2)) THEN 
              READ(TMPSTR(I2:(I3-1)),'(i2)') ISTN 
            ELSEIF (I2.EQ.(I3-3)) THEN 
              READ(TMPSTR(I2:(I3-1)),'(i3)') ISTN 
            ELSE 
              CALL ERROR ('W70MAIN','Station number > 3 numbers')
            ENDIF
        ELSEIF (IWEATH.EQ.3) THEN
            CLFILE = DBMDIR(1:ILEN(DBMDIR))//TMPSTR(1:I1)
        ELSEIF (IWEATH.EQ.4) THEN
            CLFILE = DBRDIR(1:ILEN(DBRDIR))//TMPSTR(1:I1)
        ELSEIF (IWEATH.EQ.5) THEN
            CLFILE = DRVDIR(1:ILEN(DRVDIR))//TMPSTR(1:I1)
        ENDIF
        CALL LOWERC (CLFILE)
        CALL RDSINT ('ISYR'  , ISYR  )
        CALL RDSINT ('INYEAR', INYEAR)
        CALL RDSINT ('IRNDAT', IRNDAT)
        CALL RDSINT ('ISYRR' , ISYRR )
        IF (IRNDAT.EQ.2) THEN
            CALL RDSCHA ('RAFILE', RAFILE)
            TMPSTR = RAFILE
            I1 = ILEN (TMPSTR)
            RAFILE = CLMDIR(1:ILEN(CLMDIR))//TMPSTR(1:I1)
            CALL LOWERC (RAFILE)
        ELSE
            ISYRR = -999
        END IF
        CALL RDSINT ('INYRG' , INYRG )
        CALL RDSINT ('ISTCHO', ISTCHO)
        CALL RDSINT ('IDEM'  , IDEM  )
        CALL RDSINT ('IDSOW' , IDSOW )
        CALL RDSINT ('IDESOW', IDESOW)
        CALL RDSINT ('IDLSOW', IDLSOW)
        CALL RDSINT ('IENCHO', IENCHO)
        CALL RDSINT ('IDAYEN', IDAYEN)
        CALL RDSINT ('IDURMX', IDURMX)
        CALL RDSREA ('PRDEL' , PRDEL )
        CALL RDSINT ('IBAL'  , IBAL  )
        CALL RDSCHA ('CLMNAM', CLMNAM)
        CALL RDSCHA ('RAINAM', RAINAM)
        CALL RDSINT ('ISDAY', ISDAY) 

* ------check rainfall choice against weather choice, only certain
* ------combinations are allowed, these are coded in array IROK
        IF (IPRODL.GT.1 .OR. ISTCHO.EQ.2) THEN
          IF (IRNDAT.LT.0.OR.IRNDAT.GT.3) THEN
            CALL ERROR ('W70MAIN','IRNDAT out of range 0 to 3')
          ELSE IF (IROK(IWEATH,IRNDAT).EQ.0) THEN
            CALL ERROR ('W70MAIN','illegal IWEATH-IRNDAT combination')
          END IF
        END IF

* ------specification of 'site' group of input data 
        TMPSTR = RUNDIR(1:ILEN(RUNDIR))//SITFIL
        CALL LOWERC (TMPSTR)
* ------analyse site input file 
        CALL RDINIT (IUT, IULOG, TMPSTR)
* ------get values from file
        CALL RDSCHA ('SOFILE', SOFILE)
        TMPSTR = SOFILE
        I1 = ILEN (TMPSTR)
        SOFILE = SOLDIR(1:ILEN(SOLDIR))//TMPSTR(1:I1)
        CALL LOWERC (SOFILE)
        CALL RDSREA ('SSMAX' , SSMAX )
        CALL RDSINT ('IZT'   , IZT   )
        CALL RDSREA ('WAV'   , WAV   )
        CALL RDSREA ('ZTI'   , ZTI   )
        CALL RDSINT ('IDRAIN', IDRAIN)
        CALL RDSREA ('DD'    , DD    )
        CALL RDSREA ('RDMSOL', RDMSOL)
        CALL RDSINT ('IFUNRN', IFUNRN)
        CALL RDSREA ('NOTINF', NOTINF)
        CALL RDSREA ('NBASE' , NBASE )
        CALL RDSREA ('NREC'  , NREC  )
        CALL RDSREA ('PBASE' , PBASE )
        CALL RDSREA ('PREC'  , PREC  )
        CALL RDSREA ('KBASE' , KBASE )
        CALL RDSREA ('KREC'  , KREC  )
        CALL RDSREA ('SMLIM' , SMLIM )
        CALL RDSREA ('SSI'   , SSI   )

*       Read names from crop and soil files
        CALL RDINIT (IUPL, IULOG, CRFILE)
        CALL RDSCHA ('CRPNAM', CRPNAM)
        CALL RDINIT (IUSO, IULOG, SOFILE)
        CALL RDSCHA ('SOLNAM', SOLNAM)
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*       output options 
*-----------------------------------------------------------------------
        IF (.NOT.OKNOWN) THEN
*-----------------------------------------------------------------------
*         Standard detailed output file
*-----------------------------------------------------------------------
          IUOUT  = IUOF
          IF (PRDEL.EQ.0.) THEN
            WRITE (*,'(/,A,/)')
     &            ' a log-file w60.log will be opened' 
            WOFOUT = OUTDIR(1:ILEN(OUTDIR))//'w60.log'
* --------- open log file
          ELSE
            IF (ILEN(WOFOUT).GT.0) THEN 
                TMPSTR = WOFOUT(1:ILEN(WOFOUT))
            ELSE 
               CALL ERROR('W70MAIN-SELOUT','WOFOUT')
            ENDIF
            I1 = ILEN (TMPSTR)
            WOFOUT = OUTDIR(1:ILEN(OUTDIR))//TMPSTR(1:I1)
          ENDIF

*         open detailed output file
          CALL LOWERC (WOFOUT)
          CALL FOPENG (IUOUT,WOFOUT,'NEW','FS',0,'DEL')
     
*         output option: yield statistics per series of years
* -----   Attention: changed situation due to revision for wofost 7!
*         Watch the logicals!
*
          IF (SERIES) THEN
*           output files (.PPS & .WPS) receive fixed name 
            IF (.NOT. OPNST) THEN
              IF (REPORT) THEN
                CALL LOWERC (FIXNAM)
                ILFIX = ILEN(FIXNAM)
                  ILOUT = ILEN(OUTDIR)
                FILPPS = OUTDIR(1:ILOUT)//FIXNAM(1:ILFIX)//'.PPS'
                FILWPS = OUTDIR(1:ILOUT)//FIXNAM(1:ILFIX)//'.WPS'
                CALL LOWERC (FILPPS)
                CALL LOWERC (FILWPS)

                IL1 = MAX (1,ILEN (FILPPS))
                IL2 = MAX (1,ILEN (FILWPS))
                WRITE (*,'(/,15X,A,/,2(1X,A,/))') 
     &                ' Results to file(s) ',FILPPS(1:IL1),FILWPS(1:IL2)
                OPNST = .TRUE.
              END IF
            END IF
          END IF
* ------- Attention: changed situation due to revision for wofost 7!
*         Watch the logicals!
*
          IF (RQUIRD) THEN
            SUMFIL = FIXNAM(1:ILFIX)//'.sum'
            IL1 = ILEN (SUMFIL)
            FILSUM = OUTDIR(1:ILEN(OUTDIR))//SUMFIL(1:IL1)
            CALL LOWERC (FILSUM)
            WRITE (*,'(2A)') ' Opening file: ',FILSUM
            CALL FOPENG (IUOSUM,FILSUM,'NEW','FS',0,'DEL')
            WRITE (IUOSUM,'(2A,/,2A,/,2A,/,A,/,4A)')
     &          '** FILE: ',FILSUM,
     &          '** WITH SUMMARY OF LONG TERM SIMULATED YIELD',
     &          ' STATISTICS, POTENTIAL AND WATER-LIMITED PRODUCTION',
     &          '** CALCULATED WITH ',VRSION,'**',
     &          '**PS RUNNAM IZT',
     &          ' SOW  EM    DUR   TWLV   TWST   TWSO ',
     &          ' var%  TAGP   var%   LAIM HINDEX  RYLD   RAGP',
     &          ' TRC RDMSOL'
            ILN39 = -1
          END IF
          OKNOWN = .TRUE.
        END IF
*       Now output formats known

* ------end of input section

        IF (ISET.EQ.ISTSET) WRITE (*,'(/,A,/)')
     &         ' Crop growth simulation started, please wait ...'

*=======================================================================
*       Actual execution of crop growth simulation model
*=======================================================================

*       nested loops are marked by label
*  60   over successive years : for each year a potential run is 
*       followed by a water-limited run.
*  70   over different randomized rainfall distributions : 
*       for each year a potential run is followed by a number (1 or more, 
*       user-defined) of water-limited runs. 

*       reset rainfall generator and always create at least one
*       potential production run
        RSETRG = .TRUE.
        RSETRD = .TRUE.
        NEWPP  = .TRUE.

*       initialization number of water limited runs and yield totals
        IWRUN  = 0
        YLVWLT = 0.
        YSTWLT = 0.
        YSOWLT = 0.
        HIWLT  = 0.
        RATWLT = 0.
        
        INIPP = .TRUE.
        INIWP = .TRUE.


*-----------------------------------------------------------------------
*       make repeated runs for successive years 
        DO 60 INYR = 1,INYEAR

          IASYR = ISYR+INYR-1

          NEWPP  = .TRUE.

*-----------------------------------------------------------------------
*         make repeated runs with randomized rainfall 
*         (only if chosen, for IRNDAT values 0 and 1)
          DO 70 INR=1,INYRG

            IASYRR = ISYRR+INR-1

*=======================================================================
*           Production level 1 : 
*                                   simulation of potential crop growth 
*-----------------------------------------------------------------------

*-----------preparation
*           potential production always when the variable sowing date 
*           option has been chosen, also when a totally new run has
*           been defined

            IF (ISTCHO.EQ.2.OR.NEWPP) THEN

*             set run parameters
              NOCROP = .FALSE.
              IOX  = IOXPP
              IWB  = 0

*             set dates and ISTATE according to start option
              IF (ISTCHO.EQ.0) THEN
*               fixed emergence date (=model starts at emergence)
                IDAYB  = IDEM
                IDSOW  = -99
                IDESOW = -99
                IDLSOW = -99
                ISTATE = 3
              ELSE IF (ISTCHO.EQ.1) THEN
*               fixed sowing date (=model starts at sowing)
                IDAYB  = IDSOW
                IDEM   = -99
                IDESOW = -99
                IDLSOW = -99
                ISTATE = 1
              ELSE IF (ISTCHO.EQ.2) THEN
*               variable sowing (=model starts 10 days prior to 
*               earliest possible sowing date
                IDAYB  = MAX (1, IDESOW-10)
                IDEM   = -99
                IDSOW  = -99
                ISTATE = 0
              END IF

*             set dates and ISTATE according to start option
              IF (IENCHO.EQ.1) THEN
                IF (IDAYEN.LT.0) IDAYEN = 200
                IDURMX = -99
              ELSEIF (IENCHO.EQ.2) THEN
                IF (IDURMX.LT.0) IDURMX = 200
                IDAYEN = -99
              ENDIF

*-------------write headers to output file only when PRDEL > 0

              IF (PRDEL.GT.0.) THEN
                IDETAI = 1
                CALL PRHEAD (MODE  , RUNNAM,
     &                       WOFOUT, FILRER, VRSION, 
     &                       CLMNAM, CLFILE, CRPNAM, CRFILE, 
     &                       RAFILE, RAINAM, SOLNAM, SOFILE,
     &                       IDETAI, ISET  , ISTSET, INSETS, INYR,
     &                       INYEAR, INR   , INYRG , IUOUT ,   
     &                       IUOF  , IUOSUM, IUSTPP, IUSTWP, 
     &                       IWB   , IOX   , IASYR , IASYRR, IRNDAT, 
     &                       ISTCHO, IDEM  , IDESOW, IDLSOW)
              END IF

*-------------dynamic simulation of potential crop growth
              CALL WOFSIM ( 
     &             IUWE  , IWEATH, WTRDIR, CNTR  , ISTN  , CLFILE,
     &             IURA  , IRNDAT, IASYRR, RAFILE, RSETRG, RSETRD,
     &             IUPL  , CRFILE, CRPNAM,
     &             IUSO  , SOFILE,
     &             GEODIR, DRVDIR, RUNDIR, DBRDIR,  
     &             IASYR , IDAYB , IDAYEN, IDURMX, PRDEL ,
     &             IDEM  , IDSOW , IDESOW, IDLSOW, ISTATE, 
     &             IYEARR, IUOUT , IULOG ,
     &             IWB   , IOX   , IZT   , IBAL  ,
     &             IFUNRN, SSI   , SMLIM , SSMAX , WAV   , ZTI,
     &             IDRAIN, DD    , RDMSOL, NOTINF, 
     &             ISDAY, NOCROP)
*     end:   adaption for FSEOPT (IOBSD,INOD)

              NEWPP = .FALSE.

              IF (PRDEL.GT.0.) WRITE (IUOUT,'(A)') '1'

*-------------summary output and statistics of simulated potential yields

*             if final run for statistical treatment
              LASTPP = .FALSE.
*             non generated rainfall
              IF (INYEAR.GT.2 .AND. INYR.EQ.INYEAR) LASTPP =.TRUE.

              IF (SERIES .AND. REPORT) THEN

                CALL STATPP (INIPP, LASTPP,
     &               RUNNAM, CLMNAM, CLFILE, RAINAM, RAFILE, 
     &               CRPNAM, CRFILE, SOLNAM, SOFILE, VRSION, RQUIRD,
     &               FILRER, FILPPS, 
     &               IUOUT,  IUOF,  IUOSUM, IUSTPP, IUSTWP, 
     &               MODE, ISET, ISTSET, INSETS, INYR, INYEAR,INR,INYRG,  
     &               IWB, IOX, IASYR, IASYRR, IRNDAT, 
     &               ISTCHO, IDSOW, IDESOW, IDLSOW, IDEM, INDTEM, 
     &               IZT)


                    IF (LASTPP) ILN39 = ILN39 +2
                    IF (ISTCHO.EQ.0.AND.INR.EQ.1.AND.IRNDAT.EQ.0)
     &              LASTPP=.FALSE.
              END IF
*           end if goes back to "if (istcho.eq.2.or.newpp) then"
            END IF
            IF (IPRODL.EQ.1) GOTO 70

*=======================================================================
*              Production level 2: 
*                              simulation of water-limited crop growth 
*-----------------------------------------------------------------------

*           set run parameters
            IOX  = IOXWL
            IWB  = 1

*           start water-limited run at emergence date of potential run
            IDAYB  = IDEM
C ---       set nocrop condition
            IF (ISDAY.NE.IDEM) NOCROP = .TRUE.
            ISTATE = 3

*-----------write headers to output file only when PRDEL > 0

            IF (PRDEL.GT.0.) THEN
              IDETAI = 1
              CALL PRHEAD (MODE  , RUNNAM, WOFOUT, FILRER, VRSION,
     &                        CLMNAM, CLFILE, CRPNAM, CRFILE, 
     &                        RAFILE, RAINAM, SOLNAM, SOFILE,
     &                        IDETAI, ISET  , ISTSET, INSETS, INYR,  
     &                        INYEAR, INR   , INYRG , IUOUT , 
     &                        IUOF  , IUOSUM, IUSTPP, IUSTWP, 
     &                        IWB   , IOX   , IASYR , IASYRR, IRNDAT, 
     &                        ISTCHO, IDEM  , IDESOW, IDLSOW)
            END IF

*-----------dynamic simulation of water-limited crop growth
            CALL WOFSIM (IUWE  , IWEATH, 
     &                   WTRDIR, CNTR  , ISTN  , CLFILE, IURA  , 
     &                   IRNDAT, IASYRR, RAFILE, RSETRG, RSETRD,
     &                   IUPL  , CRFILE, CRPNAM, IUSO  , SOFILE,
     &                   GEODIR, DRVDIR, RUNDIR, DBRDIR,  
     &                   IASYR , IDAYB , IDAYEN, IDURMX, PRDEL,
     &                   IDEM  , IDSOW , IDESOW, IDLSOW, ISTATE, 
     &                   IYEARR, IUOUT , IULOG , IWB   , 
     &                   IOX   , IZT   , IBAL  , IFUNRN, SSI   , 
     &                   SMLIM , SSMAX , WAV   , ZTI   , IDRAIN, 
     &                   DD    , RDMSOL, NOTINF, 
     &                   ISDAY, NOCROP)

*           summing the water-limited yields
            IWRUN  = IWRUN + 1
            YLVWLT = YLVWLT + YLVWL
            YSTWLT = YSTWLT + YSTWL
            YSOWLT = YSOWLT + YSOWL
            HIWLT  = HIWLT  + HIWL
            RATWLT = RATWLT + RATWL

*           if final run for statistical treatment
            LASTWP = .FALSE.
            IF ((INYEAR.GT.2 .OR. INYRG.GT.2) .AND. 
     &            (INYR .EQ. INYEAR .AND. INR .EQ. INYRG)) THEN
              LASTWP  =.TRUE.
            ENDIF

*-----------summary output and statistics of simulated water-limited yields
            IF (SERIES .AND. REPORT )
     &        CALL STATWP (INIWP , LASTWP, RUNNAM, CLMNAM, CLFILE, 
     &                     RAINAM, RAFILE, CRPNAM, CRFILE, SOLNAM, 
     &                     SOFILE, VRSION, RQUIRD, FILRER, FILWPS,
     &                     IUOUT , IUOF  , IUOSUM, IUSTPP, 
     &                     IUSTWP, MODE  , ISET  , ISTSET, INSETS, 
     &                     INYR  , INYEAR, INR   , INYRG , IWB   ,  
     &                     IOX   , IASYR , IASYRR, IRNDAT, 
     &                     ISTCHO, IDSOW , IDESOW, IDLSOW, IDEM  , 
     &                     RDMSOL, NOTINF,IZT)
*         end of loop across generated rainfall
70        CONTINUE
*       end of loop across successive years
60      CONTINUE

        IF (IPRODL.LE.2) GOTO 80

*-----------------------------------------------------------------------
*       Production level 3 :
*                      calculation of nutrient-limited crop production
*-----------------------------------------------------------------------

*       Mean water-limited yields (used in NUTRIE)
        MYLVWL = YLVWLT/IWRUN
        MYSTWL = YSTWLT/IWRUN
        MYSOWL = YSOWLT/IWRUN
        MHIWL  = HIWLT /IWRUN 
        MRATWL = RATWLT/IWRUN

*       nutrient calculations
        CALL NUTRIE (CRFILE,IUPL,IUOUT,IULOG,
     &       NBASE,PBASE,KBASE,NREC,PREC,KREC,
     &       YLVPP,YSTPP,YSOPP,DURPP,
     &       MYLVWL,MYSTWL,MYSOWL,
     &       NBAS,PBAS,KBAS,
     &       LVBAS1,STBAS1,SOBAS1,
     &       NFERTO,NFERTW,PFERTO,PFERTW,KFERTO,KFERTW,
     &       RATBAS,HIBAS)


80      CONTINUE   

* 3.6   output table on yield levels and nutrient requirements
*       only when PRDEL > 0
        IF (IPRODL.EQ.3 .AND. PRDEL.GT.0.) THEN
          WRITE (IUOUT,'(A)') '1'
          WRITE (IUOUT,'(/,/,A,/,A,F5.0,A,F5.0,A,F5.0)') 
     &        ' SUMMARY CROP PRODUCTION AND NUTRIENT REQUIREMENTS',
     &        ' Nbas =',NBAS,', Pbas =',PBAS,', Kbas =',KBAS
          WRITE (IUOUT,'(A,F5.2,A,F5.2,A,F5.2,/)') 
     &        ' Nrec =',NREC,', Prec =',PREC,', Krec =',KREC
          WRITE (IUOUT,'(2A,/,2(16X,A,/),2A)')
     &        ' ===================================================',
     &        '===================',
     &      '| Potential       | Nutrient limited| Water limited   |',
     &      '| Crop production | Crop production | Crop production |',
     &        ' ___________________________________________________',
     &        '___________________'
          WRITE (IUOUT,'(2(A,3(5X,F7.0,5X,A),/),A,3(5X,F7.0,5X,A))')
     &        ' Leaves         |', YLVPP,'|',LVBAS1,'|',MYLVWL,'|',
     &        ' Stems          |', YSTPP,'|',STBAS1,'|',MYSTWL,'|',
     &        ' Storage organ  |', YSOPP,'|',SOBAS1,'|',MYSOWL,'|'

          WRITE (IUOUT,'(A,3(5X,F9.2,3X,A),/,A,3(5X,F9.2,3X,A))')
     &        ' Ratio SO/straw |', RATPP,'|',RATBAS,'|',MRATWL,'|',
     &        ' Harvest index  |',  HIPP,'|', HIBAS,'|', MHIWL,'|'

          WRITE (IUOUT,'(3(A,5X,F8.1,4X,A,10X,A,6X,A,5X,F8.1,4X,A,/))')
     &        ' Fertilizer N   |',NFERTO,'|','-','|',NFERTW,'|',
     &        ' Fertilizer P   |',PFERTO,'|','-','|',PFERTW,'|',
     &        ' Fertilizer K   |',KFERTO,'|','-','|',KFERTW,'|'
        END IF

*       short output when no other output is generated
      
        WRITE (*,'(2A)') ' RUNNAM =',RUNNAM
      

*     end of loop across rerun sets
20    CONTINUE



*-----------------------------------------------------------------------
*     some files are closed here 
*-----------------------------------------------------------------------

*     delete the temporary file with rerun sets (only when this file
*     was used)
      IF (INSETS.GT.0) CLOSE (IURE, STATUS='DELETE')


*     delete all .tmp files
      CALL RDDTMP (10)

      CLOSE(IULOG, STATUS='DELETE')

      END
