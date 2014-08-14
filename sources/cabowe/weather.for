* ----------------------------------------------------------
* weather -- get weathr data
* ----------------------------------------------------------
* PURPOSE
*       WEATHER makes 6 measurements (irradiation, minus temperature,
*       maximum temperature, early morning vapour pressure, mean
*       windspeed and precipitation) available for planth growth
*       modeling programs and other programs requiring a realistic
*       "weather environment" on a daily basis.
* AUTHOR
*       M Verbeek
* DESIGN
*       M Verbeek, D v Kraalingen, C ten Cate.
* (c)
*       Centre for AgroBiological and Soil Fertility Research (AB-DLO)
*       P.O. Box 14
*       6700 AA  Wageningen
*       The Netherlands
* VERSION
*       1
*       2 : MAXSTN (maximum station code) is 999 in WSDAOP
*       3 : removed string concatenation "//" write variable list
*           (this is was not standard f77)
*       4 : -Astronomic formulas now work from pole to pole instead of
*            from polar circle to polar circle (DvK)
*           -corrected bug when year was less than 1000 on PC (DvK)
*           -converted PRINT * to WRITE (*, (DvK)
*      4.1: - cleaned up the source code (extracted include files)
*           - removed wsilen and wsstar internal routines, replaced
*             by ilen and istart from ttutil (DvK).
*      4.2: - changed slightly the definition of 4th and 5th number
*             on the location line. Both >0: radiation column is inter-
*             preted as sunshine hours using 4th and 5th number as A and
*             B values in the Angstrom formula. Values are returned
*             unchanged through STINFO. Values Both <=0: radiation
*             column is interpreted as kJ/m2/s. Sign is changed
*             to positive and values are returned through STINFO (DvK).
*      4.3: - Millennium adaptation: years less than 1900 are
*             interpreted as the request to read .AVG files, containing
*             average weather data for which no year applies. Reading
*             of data from 2000 and beyond takes place from files
*             with extension .000, .001 etc. (DvK)
*           - Removed bug when A and B were less than zero, (indicating
*             radiation values) the values were checked as if they were
*             valid A and B values
* ------------------------------------------------------------- *

* ------------------------------------------------------------- *
* stinfo -- (re)set weather system parameters
* ------------------------------------------------------------- *
* PURPOSE
*
*       set (or reset) default values for location of data
*       files and the name of the log file to IPATH and ILOG.
*       Return information about data: the coordinates and altitude
*       of the weather station and in what whay the irradiation
*       data where obtained.
* PARAMETERS
*       name   type        description
*       ----   ----        ---------------------------------------
*       --- in  ---
*       IFLAG   int        output flags
*       IPATH   C*(*)      PATH to data files
*       ILOG    C*(*)      name of logfile
*       ICNT    C*(*)      name of country
*       ISTN    int        code for station
*       IYEAR   int        year of measurments
*        --- out ---
*       LON     real       longitude
*       LAT     real       latitude
*       ALT     real       altitude
*       A       real       first parameter of radiation conversion
*       B       real       second parameter of radiation conversion
* DEFAULTS
*       PATH:   ' '        Default directory
*       LOG:    ' '        Default name = "weather.log"
*       others: -          no defaults
* RETURNS
* ------------------------------------------------------------- *

      SUBROUTINE STINFO (xFLAG,xPATH,xLOG,xCNT,xSTN,xYEAR,
     &                   xSTAT,xLON,xLAT,xALT,xA,xB)

      IMPLICIT NONE

*     formal parameters
      CHARACTER*(*)     xPATH, xLOG, xCNT
      INTEGER           xFLAG, xSTN, xYEAR
      INTEGER           xSTAT
      REAL              xLON, xLAT, xALT, xA, xB

*     local variables
      include 'wscfil.inc'
      include 'wsnfil.inc'
      include 'wsnbuf.inc'

      INTEGER    FSCLSD
      INTEGER    MAXCNT
      CHARACTER  DEFLOG*20
      PARAMETER  (DEFLOG = 'weather.log',
     &            MAXCNT =  6,
     &            FSCLSD = -1)

*     external
      INTEGER  ILEN
      LOGICAL  WSDAOP, WSATTR, WSRDDA, WSFLGS

      INTEGER   FLAG
*     default logfile name
      LOGICAL   FIRST, DUMMY

*     data unit
      INTEGER   DUNIT

*     datafile identification
      CHARACTER BCNT*(MAXCNT)
      CHARACTER NEWLOG*(MAXFNM)
      INTEGER   BSTN, BYEAR

      SAVE

      DATA FIRST /.TRUE./
*                 nonsense value
      DATA FLAG   /-54321/
      DATA BCNT   /' '/
      DATA BSTN   /-1/
      DATA BYEAR  /-1/

      IF (FIRST) THEN
          FIRST = .FALSE.
*         init of variables in common done here
          cPATH  = '<no path>'
          cFNAME = '<no file>'
          cLOG   = DEFLOG
          cLUNIT = FSCLSD
       END IF

*     magic value in STERR if exit ok: STERR = 12345,
*     on error: = -1
      cSTERR = -1

*     check IFLAG, if changed, reset FLAGS
      IF (xFLAG .NE. FLAG) THEN
         IF (.NOT. WSFLGS (xFLAG, xSTAT)) THEN
*           set flag to maximum output and continue
            FLAG  = 1111
            DUMMY = WSFLGS (FLAG, xSTAT)
         ELSE
            FLAG = xFLAG
         END IF
      END IF

*     if xLOG = ' ' the logfile has the default name
      IF (ILEN(xLOG) .EQ. 0) THEN
         NEWLOG = DEFLOG
      ELSE
         NEWLOG = xLOG
      END IF

*     see if new log file requested; close old logfile
      IF (NEWLOG .NE. cLOG) THEN
         IF (cLUNIT .NE. FSCLSD) THEN
            CLOSE(cLUNIT)
            cLUNIT = FSCLSD
         END IF
         cLOG = NEWLOG
      END IF

*     return NIL value's on error
      xLON = -199.
      xLAT = -99.
      xALT = -99.
      xA   = -99.
      xB   = -99.

*     if datafile id or PATH changed, initialise buffer
      IF  (xCNT.NE.BCNT .OR. xSTN.NE.BSTN .OR.
     &     xYEAR.NE.BYEAR .OR. xPATH .NE. cPATH) THEN
*        open data file, checks parameters
         IF (.NOT. WSDAOP (xPATH,xCNT,xSTN,xYEAR,xSTAT,DUNIT)) RETURN
*        get station location and conversion factors
         IF (.NOT. WSATTR (DUNIT,xSTAT)) RETURN
*        get data in buffer
         IF (.NOT. WSRDDA (DUNIT,xSTAT)) RETURN
*        store *valid* parameters in buffer variables
         cPATH = xPATH
         BCNT  = xCNT
         BYEAR = xYEAR
         BSTN  = xSTN

*        close data file
         CLOSE (DUNIT)
         DUNIT = FSCLSD
      END IF

      xLON = cLONG
      xLAT = cLAT
      xALT = cALT
      xA   = cA
      xB   = cB

      xSTAT  = 0
      cSTERR = 12345

      RETURN
      END

* -------------------------------------------------------------------
* weathr -- Get weathr data for IDAY from data file opend with STINFO
* -------------------------------------------------------------------
* SUBROUTINE WEATHR
*       Returns weather data for a day specified in IDAY from a
*       data file opened with STINFO.
* PARAMETERS
*       name     type     description
*       --- in  ---
*       IDAY     int      day for which weather data are returned
*       --- out ---
*       STAT     int      return status: negative = error,
*                                        zero     = ok,
*                                        positive = warning.
*       RIRRAD    real    irradiation
*       RTMIN     real    minus temperature
*       RTMAX     real    maximum temperature
*       REMPR     real    early morning vapour pressure
*       RMWIND    real    mean wind speed
*       RPRECI    real    precipitation
* DESCRIPTION
*       WEATHR reads six "weather parameters" from the buffer filled by
*       STINFO: irradiation, lowest temperature, highest temperature,
*       early morning vapour pressure and precipitation for the
*       specified IDAY.
* STATUS
*       ISTAT is zero when all data are available.
*       The data returned by WEATHR may or may not be original
*       measurements: the status of these values is returned in
*       ISTAT. Each digit in ISTAT represents one status value for
*       one measurement. The order of digits is the same as in
*       the parameter list.  ISTAT is negative when data are missing
*       or an exception has occured.
* DIGIT FLAGS in ISTAT
*       1   value is an original measurement
*       2   value is an interpolation
*       3   value is an estimate
*       4   value is missing
* EXAMPLES
*       0        All values are OK
*       -444441  All values except precipitation are missing
*       3111111  Irradiation is an estimate.
*       2222223  All values are interpolations, precipiation is an
*                estimate.
*       -1       WEATHR called with wrong day number.
* -------------------------------------------------------------- *

      SUBROUTINE WEATHR (IDAY,
     &                   STAT, RIRRAD,RTMIN,RTMAX,REMPR,RMWIND,RPRECI)

      IMPLICIT NONE

*     formal parameters
      INTEGER IDAY
      REAL      RIRRAD, RTMIN, RTMAX, REMPR, RMWIND, RPRECI
      INTEGER   STAT

*     local variables
      include 'wscfil.inc'
      include 'wsnfil.inc'
      include 'wsnbuf.inc'

      INTEGER   ILEN

*     DF: Digit Flags returned in ISTAT
*     DFINT : interpolated
*     DFEST : estimated
*     DFUNDE: undefined
      INTEGER    DFINT, DFEST, DFUNDE
      PARAMETER (DFINT  = 2,
     &           DFEST  = 3,
     &           DFUNDE = 4)

*     order of measurements in buffer (and file)
      INTEGER    NIRRAD, NTMIN, NTMAX, NEMPR, NMWIND, NPRECI
      PARAMETER (NIRRAD = 1,
     &           NTMIN  = 2,
     &           NTMAX  = 3,
     &           NEMPR  = 4,
     &           NMWIND = 5,
     &           NPRECI = 6)

*     wrong day number
      INTEGER SPDAY
*     size of message for WSMESS
      INTEGER MAXMSG
*     internal write wrong
      INTEGER SSWEAT
*     weathr called after error from STINFO or STINFO not called
      INTEGER SOINIT
      PARAMETER (SOINIT = -21,
     &           SSWEAT = -902,
     &           MAXMSG = 100,
     &           SPDAY = -1)

      INTEGER ATTI
      LOGICAL ISMISS, ISESTI, ISINTP
      CHARACTER MSG*(MAXMSG), MTYPE*(MAXMSG), HLP*10

      SAVE

*     return NIL values on error
      RIRRAD = -99.
      RTMIN  = -99.
      RTMAX  = -99.
      REMPR  = -99.
      RMWIND = -99.
      RPRECI = -99.

*     signal from STINFO (12345 "magic value": successful exit)
      IF (cSTERR .NE. 12345) THEN
         STAT = SOINIT
         CALL WSMESS(
     &        'Error in WEATHR: called after error from STINFO or '//
     &        'STINFO not called', STAT)
         RETURN
      END IF

*     check day
      IF ((IDAY .LT. 1) .OR. (IDAY .GT. NRDAYS)) THEN
         STAT = SPDAY
         IF (cERRFL) THEN
            CALL WSITOA(IDAY,HLP)
            MSG = 'Error in WEATHR: called with wrong day: '//HLP
            CALL WSMESS(MSG, STAT)
         END IF
         RETURN
      END IF

*     read data from buf, keep track of missing data
      STAT   = 111111
      ISMISS = .FALSE.
      ISINTP = .FALSE.
      ISESTI = .FALSE.

      ATTI = cBFATTR(IDAY, NIRRAD)
      IF (ATTI .NE. 1) THEN
          IF (ATTI .EQ. DFUNDE) ISMISS = .TRUE.
          IF (ATTI .EQ. DFEST) ISESTI = .TRUE.
          IF (ATTI .EQ. DFINT) ISINTP = .TRUE.
          STAT = STAT + (100000 * (ATTI-1))
      END IF
      RIRRAD = cBFVALS(IDAY, NIRRAD)

      ATTI = cBFATTR(IDAY, NTMIN)
      IF (ATTI .NE. 1) THEN
          IF (ATTI .EQ. DFUNDE) ISMISS = .TRUE.
          IF (ATTI .EQ. DFEST) ISESTI = .TRUE.
          IF (ATTI .EQ. DFINT) ISINTP = .TRUE.
          STAT = STAT + (10000 * (ATTI-1))
      END IF
      RTMIN = cBFVALS(IDAY, NTMIN)

      ATTI = cBFATTR(IDAY, NTMAX)
      IF (ATTI.NE. 1) THEN
          IF (ATTI .EQ. DFUNDE) ISMISS = .TRUE.
          IF (ATTI .EQ. DFEST) ISESTI = .TRUE.
          IF (ATTI .EQ. DFINT) ISINTP = .TRUE.
          STAT = STAT + (1000 * (ATTI-1))
      END IF
      RTMAX = cBFVALS(IDAY, NTMAX)

      ATTI = cBFATTR(IDAY, NEMPR)
      IF (ATTI.NE. 1) THEN
          IF (ATTI .EQ. DFUNDE) ISMISS = .TRUE.
          IF (ATTI .EQ. DFEST) ISESTI = .TRUE.
          IF (ATTI .EQ. DFINT) ISINTP = .TRUE.
          STAT = STAT + (100 * (ATTI-1))
      END IF
      REMPR = cBFVALS(IDAY, NEMPR)

      ATTI = cBFATTR(IDAY, NMWIND)
      IF (ATTI .NE. 1) THEN
          IF (ATTI .EQ. DFUNDE) ISMISS = .TRUE.
          IF (ATTI .EQ. DFEST) ISESTI = .TRUE.
          IF (ATTI .EQ. DFINT) ISINTP = .TRUE.
          STAT = STAT + (10 * (ATTI-1))
      END IF
      RMWIND = cBFVALS(IDAY, NMWIND)

      ATTI = cBFATTR(IDAY, NPRECI)
      IF (ATTI .NE. 1) THEN
          IF (ATTI .EQ. DFUNDE) ISMISS = .TRUE.
          IF (ATTI .EQ. DFEST) ISESTI = .TRUE.
          IF (ATTI .EQ. DFINT) ISINTP = .TRUE.
          STAT = STAT + (ATTI-1)
      END IF
      RPRECI = cBFVALS(IDAY, NPRECI)

*     check result, create message if necessary and
*         requested.
*         messages:   missing        (printed if ERRFL on)
*                     no missing:    (printed if WARNFL on)
*                       1 -   estimated (not interpolated)
*                       2 -   estimated and interpolated
*                       3 -   interpolated (not estimated)
      IF (STAT .EQ. 111111) THEN
         STAT = 0
*        nothing to report
         RETURN
      ELSE IF (ISMISS) THEN
*        missing data
         STAT = -STAT
         IF (cERRFL) THEN
            WRITE (MSG, FMT='(3A,I3,1A,I7)', ERR=990)
     &         'Error in WEATHR: missing data, in: ',
     &         cFNAME(1:ILEN (cFNAME)),
     &         ' day:', IDAY, ' attr.:', STAT
            CALL WSMESS (MSG, STAT)
         END IF
      ELSE
*        warning
         IF (cWARNFL) THEN
            MTYPE = ' '
            IF ((ISESTI) .AND. (.NOT. ISINTP)) THEN
               MTYPE = 'Warning in WEATHR: estimated'
            ELSE IF ((ISESTI) .AND. (ISINTP)) THEN
               MTYPE = 'Warning in WEATHR: estimated and interpolated'
            ELSE
               MTYPE = 'Warning in WEATHR: interpolated'
            END IF
            WRITE (MSG, FMT='(4A,I3,1A,I6)', ERR=990)
     &          MTYPE(1:ILEN(MTYPE)), ' data, in:',
     &          cFNAME(1:ILEN(cFNAME)), ' day: ', IDAY,
     &         ' attr.:', STAT
            CALL WSMESS(MSG, STAT)
         END IF
      END IF

      RETURN

990   CONTINUE
      STAT = SSWEAT
      CALL WSMESS ('Internal error in WEATHR: ', STAT )
      RETURN
      END

      SUBROUTINE SETAB (xA,xB)
      IMPLICIT NONE

*     formal parameters
      REAL xA,xB

*     local variables
      include 'wsnbuf.inc'

      INTEGER WSCAB
      INTEGER RAD_TYPE_TMP
      SAVE

      IF (cRAD_TYPE.EQ.2) THEN
*        do only work when file was based on sunshine hours
         RAD_TYPE_TMP = WSCAB (xA,xB)
         IF (RAD_TYPE_TMP.NE.2) CALL FATALERR
     &      ('SETAB','invalid A and/or B value')
         cA = xA
         cB = xB
         CALL WSCONI
      ELSE
         CALL FATALERR ('SETAB',
     &   'datafile did not include sunshine hours')
      END IF

      RETURN
      END

      INTEGER FUNCTION WSCAB (xA,xB)

*     checks whether xA and xB are valid Angstrom A and B values
*     and returns whether xA and xB values point to measurements in
*     in energy units (a and b <= 0) or in sunshine hours (a and b > 0)
*     in the first case WSCAB returns a 1, otherwise it returns a 2

*     Modified by Allard de Wit (2012/AUG/9):
*     Check on values of Angstrom AB parameters is now always done, 
*     also when they are negative and not used because absolute
*     radiation values are provided.

*     formal parameters
      REAL xA,xB

*     local variables
      REAL A,B,SUM_AB
      REAL MIN_A,MAX_A
      REAL MIN_B,MAX_B
      REAL MIN_SUM_AB,MAX_SUM_AB
      PARAMETER (MIN_A=0.1, MAX_A=0.4)
      PARAMETER (MIN_B=0.3, MAX_B=0.7)
      PARAMETER (MIN_SUM_AB=0.6,MAX_SUM_AB=0.9)

      INTEGER WSCAB_L
      SAVE

      A = xA
      B = xB

*     use sign for first determination
      IF (A.EQ.0..AND.B.EQ.0.) THEN
*        radiation is in kJ/m2/s
         WSCAB_L = 1
      ELSE IF (A.LT.0..AND.B.LT.0.) THEN
*        radiation is in kJ/m2/s
         WSCAB_L = 1
      ELSE IF (A.GT.0..AND.B.GT.0.) THEN
*        radiation is in hours sunshine per day
         WSCAB_L = 2
      ELSE
         CALL FATALERR ('WSCAB','invalid A and/or B value')
      END IF

*     check now the absolute values
      A = ABS (A)
      IF (A.LT.MIN_A.OR.A.GT.MAX_A) CALL FATALERR
     &      ('WSCAB','invalid Angstrom A value')

      B = ABS (B)
      IF (B.LT.MIN_B.OR.B.GT.MAX_B) CALL FATALERR
     &      ('WSCAB','invalid Angstrom B value')

      SUM_AB = A+B
      IF (SUM_AB.LT.MIN_SUM_AB.OR.SUM_AB.GT.MAX_SUM_AB) CALL FATALERR
     &      ('WSCAB','invalid Angstrom A and/or B value')

      WSCAB = WSCAB_L

      RETURN
      END

* ------------------------------------------------------------- *
* wsflgs -- set system flags
* ------------------------------------------------------------- *
* DESCRIPTION
*       Verify and set output flags to NEWFLG, store separate
*       flag values in common field OF
*       Initializes itself when called first time.
* ------------------------------------------------------------- *

      LOGICAL FUNCTION WSFLGS (xNEWFLG, xSTAT)

      IMPLICIT NONE
*     formal parameters
      INTEGER   xNEWFLG,xSTAT

*     local variables
      include 'wsnfil.inc'

*     Status value : Parameter FLAG wrong
      INTEGER   SPFLAG
      PARAMETER (SPFLAG = -2)

*     copy of flag
      INTEGER   F
*     "value" of flag  (1000,100,10,1)
      INTEGER   FI
*     local copy of flag list (OF), OF is set after
*     every thing is checked, to avoid inconsistencies.
      LOGICAL   LOF(NOF)

      INTEGER   I
      CHARACTER MSG*80
      LOGICAL   FIRST

      SAVE

      DATA FIRST /.TRUE./

      WSFLGS = .FALSE.
      F      = xNEWFLG

      IF (FIRST) THEN
*        initialise output flags (warning/error for stdout/file)
         DO I = 1, NOF
            cOF(I) = .FALSE.
         END DO
*        send errors to output to start with
         cOF(OFOUTF) = .TRUE.
         cERRFL      = .TRUE.
         cWARNFL     = .FALSE.
      END IF

      WRITE (MSG, '(A29,I10)' ) 'Error in STINFO: wrong flag: ',xNEWFLG
      IF (F .LT. 0) THEN
         xSTAT = SPFLAG
         CALL WSMESS( MSG, xSTAT )
         RETURN
      END IF

*     loop for flags
*     flag "1000" is OFLOGW: warnings to log file
*     flag "   1" is OFOUTF: fatals to output
      FI = 1000
      DO I = OFLOGW, OFOUTF, -1
         IF ((F - FI) .GE. 0) THEN
            F = F - FI
            IF (F .GT. FI) THEN
*               not 1
                xSTAT = SPFLAG
                CALL WSMESS( MSG, xSTAT )
                RETURN
            END IF
*           enable flag I
            LOF(I) = .TRUE.
         ELSE
*           disable flag I
            LOF(I) = .FALSE.
         END IF
         FI = FI/10
      END DO

*     New flag ok, make it final.
      DO I=1, NOF
         cOF(I) = LOF(I)
      END DO

      cERRFL  = (cOF(OFOUTF) .OR. cOF(OFLOGF))
      cWARNFL = (cOF(OFOUTW) .OR. cOF(OFLOGW))

      WSFLGS = .TRUE.

      RETURN
      END

* ------------------------------------------------------------- *
* wsdaop -- open new data file, close current (if any)
* ------------------------------------------------------------- *
* DESCRIPTION
*       Checks parameters, constructs a file name , and opens the
*       file.
* REMARKS
*       Although this routine is written in standard fortran, file-
*       name construction is tricky: correct filename syntax is
*       is system dependend.
* Modified: MAXSTN from 99 to 999
* ------------------------------------------------------------- *

      LOGICAL FUNCTION WSDAOP (IPATH, CNT, STN, YEAR,
     &                         STAT,  DUNIT)

      IMPLICIT NONE

*     formal parameters
      CHARACTER*(*) IPATH,CNT
      INTEGER   STN,YEAR,STAT,DUNIT

*     local variables

      include 'wscfil.inc'

      INTEGER   ILEN
      LOGICAL   WSOPEN

      INTEGER SPYEAR,   ! year is wrong
     &        SPCNT,    ! country is wrong
     &        SPSTN,    ! station is wrong
     &        SFDATA,   ! can't open data file
     &        SSDAOP,   ! internal error in wsdaop
     &        SSOPEN,   ! internal error in wsopen
     &        MAXCNT,   ! maximum size of country
     &        MAXYR,    ! maximum year
     &        MINYR,    ! minimum year
     &        MAXSTN    ! maximum for station code

      PARAMETER (
     &      SPYEAR = -3    ,
     &      SPCNT  = -4    ,
     &      SPSTN  = -5    ,
     &      SFDATA = -12   ,
     &      SSOPEN = -903  ,
     &      SSDAOP = -900  ,
     &      MAXCNT = 50    ,
     &      MAXYR  = 2100  ,
     &      MINYR  = 1000  ,
     &      MAXSTN = 999   )

      INTEGER       SIZE,LCNT,IOSS,IL1,IL2
      CHARACTER*4   S
      CHARACTER*200 MSG,HMSG

*     data file name, including path
      CHARACTER*(MAXFNM) FNDAT, LFNAME, HFNAME

      SAVE

      WSDAOP = .FALSE.
      LFNAME = ' '

*     check parameters
*     year is 3 digits only
      IF ((YEAR .LT. MINYR) .OR. (YEAR .GT. MAXYR)) THEN
          MSG= 'Error in STINFO: year: "'
          CALL WSITOA(YEAR, S)
          HMSG = MSG
          IL1 = ILEN(HMSG)
          IL2 = ILEN(S)
          MSG = HMSG(1:IL1)//S(1:IL2)//'" is out of range'
          STAT = SPYEAR
          CALL WSMESS( MSG, STAT )
          RETURN
      END IF

      LCNT = ILEN(CNT)

*     size of country
      IF ((LCNT .LT. 1) .OR. (LCNT .GT. MAXCNT)) THEN
         STAT = SPCNT
         CALL WSMESS(
     &      'Error in STINFO: wrong string size for country', STAT )
         RETURN
      END IF

*     station
      IF ((STN .LT. 0) .OR. (STN .GT. MAXSTN)) THEN
         STAT = SPSTN
         CALL WSMESS(
     &      'Error in STINFO: station code out of range', STAT )
         RETURN
      END IF

*     construct name of file (without path), put in LFNAME
*     country part
      LFNAME = CNT(1:LCNT)
      SIZE   = LCNT

*     get station number part of filename
      CALL WSITOA(STN, S)
      HFNAME = LFNAME
      LFNAME = HFNAME(1:SIZE)//S
      SIZE   = ILEN(LFNAME)

*     year comes in extension part of filename
      HFNAME = LFNAME
      LFNAME = HFNAME(1:SIZE) // '.'
      SIZE = SIZE + 1

*     get year
      S = ' '
      HFNAME = LFNAME
      IF (YEAR.GE.1900) THEN
         WRITE(S, ERR= 900, FMT='(I4.4)') YEAR
         LFNAME = HFNAME(1:SIZE) // S(2:4)
      ELSE
         S = 'AVG'
         LFNAME = HFNAME(1:SIZE) // S(1:3)
      END IF
      SIZE = SIZE + 3

*     add path to name
      IF (ILEN(IPATH) .NE. 0) THEN
          FNDAT = IPATH(1:ILEN(IPATH))  // LFNAME(1:SIZE)
      ELSE
*         allow for "default directory"
          FNDAT = LFNAME
      END IF
      SIZE = ILEN(FNDAT)

*     open file for reading
      IF (.NOT. WSOPEN(FNDAT(1:SIZE), 'r', DUNIT, STAT, IOSS)) THEN
*        failed, report if not internal error
         IF (STAT .EQ. SSOPEN) RETURN
         CALL WSITOA(IOSS, S)
         MSG = 'Error in STINFO: cannot open: "'//FNDAT(1:SIZE)//
     &         '" (system status ='//S(1:ILEN(S))//')'
         STAT = SFDATA
         CALL WSMESS(MSG, STAT)
         RETURN
      END IF

*     make path/filename global for error messages
      cFNAME = LFNAME
      cPATH  = IPATH
      WSDAOP = .TRUE.

      RETURN

*     Internal write errors

900   CONTINUE
      STAT = SSDAOP
      CALL WSMESS( 'Internal error in STINFO', STAT )

      RETURN
      END

* ------------------------------------------------------------- *
* wsattr -- get attributes of data for weather station
* ------------------------------------------------------------- *
* DESCRIPTION
*       This routine is called after the file is opened.
*       Comment lines are read until a data line is seen. This line
*       must contain 5 (no more, no less) REAL values which
*       represent (in  this order): longitude, latitude and
*       altitude and irradiaton conversion factors A and B.
*       If no conversion is needed (data are already in irradiation
*       per square meter) then A and B must be zero.
* ------------------------------------------------------------- *

      LOGICAL FUNCTION WSATTR (DUNIT,STAT)

      IMPLICIT NONE

*     formal parameters
      INTEGER   DUNIT,STAT

*     local variables
      include 'wsnbuf.inc'

*     maximum size of input string
      INTEGER   MAXSTR
*     comment char in column 1
      CHARACTER CHRCOM*1
*     signal comment seen
      INTEGER   SMCOMM
*     signal unexpected end of data file
      INTEGER   SFDEOF
*     signal read error in data file
      INTEGER   SFDERR

      INTEGER WSCAB

      PARAMETER (
     &          CHRCOM = '*',
     &          SMCOMM =   1,
     &          SFDEOF = -14,
     &          SFDERR = -15,
     &          MAXSTR = 132)

      CHARACTER*(MAXSTR) S

*     local copies of location parms
      SAVE

      WSATTR = .FALSE.

*     loop until location data line is read
100   CONTINUE
*        read a string
         READ( UNIT=DUNIT, FMT='(A)', END=900, ERR=910 ) S
         IF(S(1:1) .EQ. CHRCOM) THEN
*            print if warn-flag(s) is (are) on
             STAT = SMCOMM
             CALL WSMESS( S, STAT)
*            if not equal to old value: error in wsmess
             IF (STAT .NE. SMCOMM) RETURN
             STAT = 0
             GOTO 100
         END IF
*     end of loop

*     not comment, get data again
      BACKSPACE (UNIT=DUNIT)
      READ (DUNIT,*,END=900,ERR=920) cLONG,cLAT,cALT,cA,cB

*     determine if sunshine hours were used
      cRAD_TYPE = WSCAB (cA,cB)

      IF (cRAD_TYPE.EQ.2) THEN
*        radiation was in sunshine hours, change sign of A and B
         cA = ABS (cA)
         cB = ABS (cB)
      END IF

      WSATTR = .TRUE.

      RETURN

* ------------------------------------------------------------- *
*     read errors:

900   CONTINUE
      STAT = SFDEOF
      CALL WSMESS( 'Error in STINFO: unexpected end of file.', STAT )
      RETURN

910   CONTINUE
      STAT = SFDERR
      CALL WSMESS( 'Error in STINFO: unexpected read error.', STAT )
      RETURN

920   CONTINUE
      STAT = SFDERR
      CALL WSMESS(
     &    'Error in STINFO: incorrect geografical data line', STAT )
      RETURN

      END

* ------------------------------------------------------------- *
* wsrdda -- read data in buffer
* ------------------------------------------------------------- *
* DESCRIPTION
*       WSRDDA reads the actual weather data, i.e. the second part
*       of the datafile. First the buffer is filled with the
*       value for "undefined". Then the data are read.
*       The actual reading is done in WSGREC (Get RECord) which
*       returns data values and attributes if an attribute line
*       preceded the data record.
*       After reading the available data, the buffer is scanned
*       for undefined values. These are replaced with an
*       interpolated value (if possible). If data are in sunshine hours
*       then a conversion to irradiation per square meter is
*       is performed.
* ------------------------------------------------------------- *

      LOGICAL FUNCTION WSRDDA (xDUNIT,xSTAT)

      IMPLICIT NONE

*     formal parameters
      INTEGER xDUNIT,xSTAT

*     local variables

      include 'wsnbuf.inc'

      REAL      VALUND
      PARAMETER (VALUND = -99.)

*     --- DF: Digit Flags returned in ISTAT
*     --- DFINT : interpolated
*     --- DFUNDE: undefined
      INTEGER    DFOK, DFINT, DFUNDE
      PARAMETER (DFOK = 1,
     &           DFINT = 2,
     &           DFUNDE = 4)

      LOGICAL   WSGREC

      INTEGER   TODAY,I,COL
*     list of data for day
      REAL      DATLST(NRDP)
*     list of attributes for day
      INTEGER   ATTLST(NRDP)
*     attributes set in data file with attribute line.
      INTEGER   ATTSET(NRDP)
      REAL      BVAL
      REAL      SLOPE
*     in missing part of column
      LOGICAL   INMIS
*     start of missing part of column
      INTEGER   START

      SAVE

*     initialize data buffer
      DO TODAY = 1, NRDAYS
         DO I = 1, NRDP
            cBFVALS(TODAY,I) = VALUND
            cBFATTR(TODAY,I) = DFUNDE
         END DO
      END DO

*     initialize attribute-set list to default
      DO I=1, NRDP
         ATTSET(I) = DFOK
      END DO

*     fill buffer, loop until eof or all days read
200   CONTINUE
*        get a line with NRDP data
         IF (.NOT. WSGREC (xDUNIT, NRDP, ATTSET,
     &                     xSTAT,  TODAY, DATLST, ATTLST)) GOTO 300
         DO COL=1, NRDP
            cBFVALS(TODAY, COL) = DATLST(COL)
            cBFATTR(TODAY, COL) = ATTLST(COL)
         END DO
         GOTO 200
*        end read loop

300   CONTINUE

*     abnormal exit if status unequal to eof
      IF (xSTAT .NE. 0) RETURN

*     see if interpolations needed (attributes undefined)
*     don't interpolate for rainfall: last column
      DO COL=1, NRDP-1
         INMIS = .FALSE.
         DO TODAY=1, NRDAYS

            IF ((cBFATTR(TODAY,COL) .EQ. DFUNDE) .AND.
     &          (.NOT. INMIS)) THEN
*              beginning of missing part
               INMIS = .TRUE.
               START = TODAY

            ELSE IF (INMIS .AND. (cBFATTR(TODAY,COL) .NE. DFUNDE)) THEN
*              at end of missing part
*              if not from day 1 interpolate
               INMIS = .FALSE.
               IF (START .NE. 1) THEN
*                 compute slope
                  SLOPE = (cBFVALS(TODAY, COL) - cBFVALS(START-1,COL))/
     &                    (TODAY-START+1)
                  BVAL = cBFVALS(START-1,COL)
                  DO I=START, TODAY-1
                     cBFVALS(I, COL) = BVAL + SLOPE*REAL(I-START+1)
                     cBFATTR(I, COL) = DFINT
                  END DO
               END IF
            END IF
         END DO
*        we don't have to check for missing at end (inmis=.true.)
*        because values are already set to "undefined"
      END DO

*     if data in hours sun, convert to irradiation
      IF (cRAD_TYPE.EQ.2) THEN
*        preserve sunshine hours in separate column
         DO TODAY=1,NRDAYS
            cBFSUN(TODAY) = cBFVALS(TODAY,1)
         END DO
         CALL WSCONI
      END IF

      WSRDDA = .TRUE.

      RETURN
      END

* ------------------------------------------------------------- *
* wsgrec -- get a record: NDPR data with atributes
* ------------------------------------------------------------- *
* DESCRIPTION
*       Datafiles are devided in to parts: a comment section
*       and a data section. This function reads the data section.
*       Two types of data lines are possible: normal data lines
*       and Attribute lines. Attribute lines are marked by the
*       special station value -999 (year and day are ignored
*       but a dummy value must be present). The parameter ATTSET
*       holds the attributes from the last attribute line. The values
*       in ATTLST may differ from ATTSET because data can be missing.
* PARAMETERS
*       name    class  type   description
*       --------------------------------------------------------
*       dunit   in     int    unit of data file
*       attset  in/out int()  attribute's set from last att. line.
*       stat    out    int    exit status (see below)
*       dayrd   out    int    day read
*       datlst  out    real() array (NRDP elements) with data points
*       attlst  out    int()  array (NRDP elements) with attributes
* RETURNS in STAT
*       0       signals success
*       other   signals failure (message is printed)
* RETURNS
*       .FALSE. when done : STAT = 0
*               on error  : STAT = exit error code
*       .TRUE.  got data, more lines expected.
* ------------------------------------------------------------- *

      LOGICAL FUNCTION WSGREC (DUNIT,NRDP,ATTSET,
     &                         STAT, DAYRD, DATLST, ATTLST)

      IMPLICIT NONE
*     formal parameters
      INTEGER   DUNIT,NRDP,ATTSET(NRDP),DAYRD,ATTLST(NRDP)
      REAL      DATLST(NRDP)

*     local variables
      REAL      VALUND
      INTEGER   SFDERR
      INTEGER   SFDEOF

      PARAMETER (
     &          SFDERR = -15       ,
     &          SFDEOF = -14       ,
     &          VALUND = -99.      )
*     DF: Digit Flags returned in ISTAT
*     DFUNDE: undefined
      INTEGER    DFUNDE
      PARAMETER (DFUNDE = 4)
      INTEGER   STAT, I
      INTEGER   YEARRD, STNRD

      SAVE

      WSGREC = .FALSE.
      READ(UNIT=DUNIT,FMT=*,END=900,ERR=910)
     &             STNRD, YEARRD, DAYRD, DATLST

*     Is it a line with data or attributes?
      IF (STNRD .EQ. -999) THEN
*        got line with attributes
         DO I=1, NRDP
            ATTSET(I) = NINT(DATLST(I))
         END DO
*        also get next line: MUST have data for these attributes
         READ (UNIT=DUNIT,FMT=*,END=920,ERR=910)
     &              STNRD, YEARRD, DAYRD, DATLST
      END IF

*     set attributes
      DO I=1, NRDP
         IF (DATLST(I) .LE. VALUND) THEN
            ATTLST(I) = DFUNDE
         ELSE
            ATTLST(I) = ATTSET(I)
         END IF
      END DO

      WSGREC = .TRUE.
      STAT = 0

      RETURN

* ------------------------------------------------------------- *
*     labels for read
900   CONTINUE
*     eof
      STAT = 0
      RETURN

910   CONTINUE
      STAT =SFDERR
      CALL WSMESS('Error in STINFO: incorrect data file', STAT )
      RETURN

920   CONTINUE
      STAT = SFDEOF
      CALL WSMESS('Error in STINFO: unexpected end of file', STAT )
      RETURN

      END

* ------------------------------------------------------------- *
* wsconi -- convert hours sun to irradation
* ------------------------------------------------------------- *
* PUPOSE
*       convert hours sun measurements to irradiation
* DESCRIPTION
*       WSCONI tests if A and B are not zero the measurements
*       are in hours sun and have to be converted to irradiation
*       per square meter.
* ------------------------------------------------------------- *
      SUBROUTINE WSCONI

      IMPLICIT NONE

*     formal parameters
*     <none>

*     local variables

      include 'wsnbuf.inc'

*     PI and conversion factor from degrees to radians
      REAL      PI, RAD
*     undefined data
      REAL      VALUND
      PARAMETER (PI      = 3.141592654,
     &           RAD     = 0.017453292,
     &           VALUND  = -99.0)

*     length of sun day for particalar latitude.
      REAL      DAYL(NRDAYS)
*     angot for this latitude
      REAL      ANGOT(NRDAYS)
*     last used latitude
      REAL      LAT
      REAL      DAY, DSINB, SC, DEC, SINLD, COSLD, AOB, ZZCOS, ZZA
      INTEGER   IDAY

      SAVE

      DATA LAT /100./

      IF (cLAT .NE. LAT) THEN
*        new latitude, recompute DAYL and ANGOT.
         LAT = cLAT

         DO IDAY=1, 366

            DAY = REAL (IDAY)

*           declination of the sun as function of daynumber (DAY)
            DEC = -ASIN (SIN(23.45*RAD)*COS(2.*PI*(DAY+10.)/365.))

*           SINLD, COSLD and AOB are intermediate variables
            SINLD = SIN (RAD*LAT)*SIN (DEC)
            COSLD = COS (RAD*LAT)*COS (DEC)
            AOB   = SINLD/COSLD

            IF (AOB.LT.-1.) THEN
               DAYL(IDAY)  = 0.
               ZZCOS       = 0.
            ELSE IF (AOB.GT.1.) THEN
               DAYL(IDAY)  = 24.
               ZZCOS       =  0.
            ELSE
               DAYL(IDAY)  = 12.*(1.+2.*ASIN (AOB)/PI)
               ZZA         = PI*(12.+DAYL(IDAY))/24.
               ZZCOS       = COS (ZZA)
            END IF

*           integral of sine of solar elevation
            DSINB  = 2.*3600.*(DAYL(IDAY)*0.5*SINLD-12.*COSLD*ZZCOS/PI)

*           solar constant (SC) and daily extraterrestrial
*           radiation (ANGOT)
            SC = 1370.*(1.+0.033*COS(2.*PI*DAY/365.))
            ANGOT(IDAY) = SC*DSINB/1000.
         END DO
      END IF

*     replace hours sun with radiation from daylength and Angot value
      DO IDAY=1,366
            IF (cBFVALS(IDAY,1).NE.VALUND .AND.
     &         DAYL(IDAY).GT.0.) THEN
               cBFVALS(IDAY,1) =
     &           ANGOT(IDAY)*(cA+cB*cBFSUN(IDAY)/DAYL(IDAY))
            ELSE
               cBFVALS(IDAY,1) = 0.
            END IF
      END DO

      RETURN
      END

* ------------------------------------------------------------- *
* wsmess -- (don't) print message on output and/or logfile
* ------------------------------------------------------------- *
* PARAMETERS
*       name    class   type    description
*       instr   in      char*   message to print
*       stat    in/out  integ   status value
* PURPOSE
*       WSMESS is used for printing (or not if flags are
*       disabled) warnings and/or error messages to the
*       standard output and/or log file.
* REMARK
*       STAT is written if WSMESS exits with an error.
* STATUS CODES
*        lowest highest description
*        -------------------------------------------------
*         <<    -111114 data are missing, print this code
*       -111111      -1 error detected (e.g. wrong parameters)
*        1       1      (SMCOMM): print a comment line
*        2       111111 not used
*        111112  333333 warning, print this code
* REMARKS
*       WSMESS is designed so that it is most efficient
*       if all output is disabled.
* -------------------------------------------------------------- *

      SUBROUTINE WSMESS (xINSTR, xSTAT)

      IMPLICIT NONE

*     formal parameters
      CHARACTER*(*)  xINSTR
      INTEGER        xSTAT

*     local variables

      include 'wsnfil.inc'
      include 'wscfil.inc'

      INTEGER    ILEN
      LOGICAL    WSOPEN

*     signal comment
      INTEGER    SMCOMM
*     write to (open) logfile failed
      INTEGER    SFLOGW
*     open of logfile failed: unknown mode.
      INTEGER    SSOPEN
*     unknown status code passed to WSMESS
      INTEGER    SSUSTA
*     maximum size for messages
      INTEGER    MAXMSG
*     special unit nbr: not a unit number (file is closed)
      INTEGER    FSCLSD
*     can't open log file
      INTEGER    SFLOGF
*     STINFO not called, or wrong initialisation
      INTEGER    SOINIT

      PARAMETER (SOINIT = -21 ,
     &           SFLOGF = -13 ,
     &           FSCLSD = -1  ,
     &           MAXMSG = 100 ,
     &           SSOPEN = -903,
     &           SFLOGW = -16 ,
     &           SSUSTA = -904,
     &           SMCOMM = 1)

      INTEGER    IOSS
      CHARACTER  MSG*(MAXMSG)
      CHARACTER  S*10
      LOGICAL    ISERR
*     status of WSMESS (used with WSOPEN)
      INTEGER    MSTAT

      SAVE

*     get level of status: warning or fatal-error
      ISERR = xSTAT .LT. 0

      IF (xSTAT .EQ. SOINIT) THEN
*        error caused by NOT calling STINFO, must print to output
         WRITE (*,*) xINSTR(1:ILEN(xINSTR))
         RETURN
      END IF

*     create a message if output/logfile enabled
      IF (ISERR) THEN
         IF (cERRFL) THEN
*            error output enabled, create message
             MSG = ' '
             IF (xSTAT .LT. -111111)  THEN
*               missing data
                MSG = xINSTR
             ELSE IF (xSTAT .GT. -900) THEN
*               some kind of error detected: report INSTR
                MSG = xINSTR
             ELSE
*               internal error, unknown status
                WRITE (*,*) 'Internal error in WEATHR/STINFO: ',
     &                   SSUSTA
                xSTAT = SSUSTA
                RETURN
             END IF
         ELSE
*            error output disabled, nothing to do
             RETURN
         END IF

*        warning
      ELSE
         IF (cWARNFL) THEN
*           warning output enabled
            MSG = ' '
            IF (xSTAT .GT. 111111) THEN
*              interpolated or artificial data
               MSG = xINSTR
            ELSE IF (xSTAT .EQ. SMCOMM) THEN
*              comment line read, copy to MSG
               MSG = xINSTR
            ELSE
*               internal error, unknown status
                WRITE (*,*) 'Internal error in WEATHR/STINFO: ',
     &                       SSUSTA
                xSTAT = SSUSTA
                RETURN
            END IF

         ELSE
*           warning output disabled, nothing to do
            RETURN
         END IF
      END IF

*     write message to output and/or logfile
      IF (ISERR) THEN
         IF (cOF(OFOUTF)) WRITE (*,*) MSG(1:ILEN(MSG))
         IF (cOF(OFLOGF)) THEN
*           open logfile if closed
            IF (cLUNIT .EQ. FSCLSD) THEN
               IF (.NOT. WSOPEN(cLOG, 'w', cLUNIT, MSTAT, IOSS))
     &                   GOTO 920
            END IF
            WRITE(cLUNIT, '(1X,A)', ERR=990, IOSTAT=IOSS)
     &            MSG(1:ILEN(MSG))
         END IF

      ELSE
         IF (cOF(OFOUTW)) WRITE (*,*) MSG(1:ILEN(MSG))
         IF (cOF(OFLOGW)) THEN
            IF (cLUNIT .EQ. FSCLSD) THEN
               IF (.NOT. WSOPEN(cLOG, 'w', cLUNIT, MSTAT, IOSS))
     &                   GOTO 920
            END IF
            WRITE (cLUNIT,'(1X,A)', ERR=990, IOSTAT=IOSS)
     &             MSG(1:ILEN(MSG))
         END IF
      END IF

      RETURN

*     log open failed: write to output instead
920   CONTINUE
*     If not internal error in WSOPEN print a message and put
*     code number in STAT. Internal errors are handled in WSOPEN.
      IF (MSTAT .EQ. SSOPEN) THEN
         xSTAT = MSTAT

         RETURN

      END IF
      xSTAT = SFLOGF
      CALL WSITOA (IOSS, S)
      MSG = 'Error in WEATHR/STINFO: cannot open logfile:"' //
     &      cLOG(1:ILEN(cLOG)) // '"' //
     &      ', (system status =' // S(1:ILEN(S)) // ')'
      WRITE (*,*) MSG

      RETURN

990   CONTINUE
      xSTAT = SFLOGW
      CALL WSITOA (IOSS, S)
      MSG = 'Error in WEATHR/STINFO: cannot write to logfile:"'//
     &      cLOG(1:ILEN(cLOG)) // '"' //
     &      ', (system status =' // S(1:ILEN(S)) // ')'
      WRITE (*,*) MSG

      RETURN
      END

* ------------------------------------------------------------- *
* wsopen -- open file NAME with MODE, return unit and status
* ------------------------------------------------------------- *
* DESCRIPTION
*       WSOPEN opens the file NAME with access mode MODE.
*       A unit number is assigned to UNUM for accessing the file.
* MODES
*       The following modes are defined:
*       'r'     open file for reading. On multi user systems
*               shareable files can be read.
*       'w'     the file is opend for writing. If the file exists
*               the old version is deleted first.
* RETURNS
*       The function result is .TRUE. if successfull.
*
*       The parameter STAT is used to return errors caused by
*       wrong arguments. IOSS is used to return errors from the
*       environment, e.g. trying to open a file for reading that
*       doesn't exist. These codes are highly system/compiler
*       dependend.
* ------------------------------------------------------------- *

      LOGICAL FUNCTION WSOPEN (NAME, MODE,
     &                         FUNIT, STAT, IOSS)

      IMPLICIT NONE

*     formal parameters
      CHARACTER*(*) NAME*(*)
      CHARACTER    MODE*1
      INTEGER      FUNIT,STAT,IOSS

*     local variables
      INTEGER      ILEN
      INTEGER      FSCLSD
      INTEGER      SSOPEN
      PARAMETER (SSOPEN = -903     ,
     &           FSCLSD = -1       )
      INTEGER   LNAME
      LOGICAL   EXST

      SAVE

      WSOPEN = .FALSE.
      IOSS   = 0
      STAT   = 0

*     get unit number
      CALL WSFUN (FUNIT)
      LNAME = ILEN(NAME)

*     read mode
*     open readonly, sequential, existing (old) file
      IF (MODE(1:1) .EQ. 'r') THEN
* Normal open statement
         OPEN(UNIT=FUNIT,
     &        STATUS='OLD',
     &        IOSTAT=IOSS,
     &        FILE=NAME(1:LNAME))
* VAX / VMS / ALPHA adaptation
c         OPEN(UNIT=FUNIT,
c     &        STATUS='OLD',
c     &        IOSTAT=IOSS,
c     &        FILE=NAME(1:LNAME),READONLY)
         IF (IOSS .NE. 0) THEN
            FUNIT = FSCLSD
            RETURN
         ELSE
            WSOPEN = .TRUE.
            RETURN
         END IF

*     write mode
*     open new file for sequential writing
      ELSE IF (MODE(1:1) .EQ. 'w') THEN
*        to be safe: first delete file if it exists
         INQUIRE(FILE=NAME(1:LNAME), EXIST=EXST)
         IF (EXST) THEN
            OPEN(UNIT=FUNIT,
     &           FILE=NAME(1:LNAME),
     &           STATUS='OLD',
     &           IOSTAT=IOSS )
            CLOSE(UNIT=FUNIT, STATUS='DELETE')
         END IF
*        now we can open with 'NEW'
         OPEN(UNIT=FUNIT,
     &        FILE = NAME(1:LNAME),
     &        STATUS = 'NEW',
     &        IOSTAT = IOSS )
         IF (IOSS .NE. 0) THEN
            FUNIT = FSCLSD
            RETURN
         ELSE
            WSOPEN = .TRUE.
            RETURN
         END IF

      ELSE
           STAT = SSOPEN
           FUNIT = FSCLSD
*          must print to output (can't call wsmess)
           WRITE (*,*)
     &       'Error in WEATHER/STINFO: internal error, code:', STAT
           RETURN
      END IF
      END

* ------------------------------------------------------------- *
* wsfun -- get free unit number
* ------------------------------------------------------------- *
* DESCRIPTION
*       wsfun scans for unused unitnumbers, starting with 92
*       down to 40.
*       The numbers are checked with INQUIRE, so that numbers
*       are returned to the "free-list" by using standard CLOSE.
* RETURNS
*        -1 in FD if no valid unit number found.
* BUGS
*       Inconsequent use causes subtle bugs.
* ------------------------------------------------------------- *
      SUBROUTINE WSFUN (FD)

      IMPLICIT NONE

*     formal parameters
      INTEGER FD

*     local variables

*     lowest,highest unit number opened:
      INTEGER   LOWUNT, MAXUNT
      PARAMETER (LOWUNT = 40,
     &           MAXUNT = 92)

      LOGICAL   ISOPEN
      SAVE

*     find unused unit number
      DO FD = MAXUNT, LOWUNT, -1
         INQUIRE(UNIT=FD, OPENED=ISOPEN)
         IF (.NOT. ISOPEN) RETURN
      END DO

      FD = -1

      RETURN
      END

* ------------------------------------------------------------- *
* wsitoa -- convert integer to (left adjusted) ascii in string  *
* ------------------------------------------------------------- *
* DESCRIPTION                                                   *
*       The integer I is written in STRING starting at the      *
*       first postion.                                          *
* ------------------------------------------------------------- *
      SUBROUTINE WSITOA (I, STRING)

      IMPLICIT NONE

*     formal parameters
      INTEGER   I
      CHARACTER STRING*(*)

*     local variables
      INTEGER   ILEN, ISTART

      CHARACTER S*80
      INTEGER   IS
      INTEGER   IE
      SAVE

      WRITE (S, '(I20)' )  I

      IS = ISTART(S)
      IE = ILEN(S)

      STRING = S(IS:IE)

      RETURN
      END
