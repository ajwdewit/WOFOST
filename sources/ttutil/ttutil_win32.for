**==ADDINF.FOR  
      SUBROUTINE ADDINF (STRING,SIGLEN,I,FORM)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING,FORM
      INTEGER SIGLEN,I
 
**    local variables
      CHARACTER*30 TMPR
      CHARACTER*30 TMPFORM
      INTEGER IL
      SAVE
 
      TMPFORM = '('//FORM//',A)'
      WRITE (TMPR,TMPFORM) I,'|'
      IL = INDEX (TMPR,'|')-1
 
      IF (SIGLEN+IL.GT.LEN (STRING)) CALL FATALERR
     &   ('ADDREF','internal error')
 
      STRING(SIGLEN+1:SIGLEN+IL) = TMPR(1:IL)
      SIGLEN = SIGLEN+IL
 
      RETURN
      END
**==ADDINT.FOR  
      SUBROUTINE ADDINT (STRING,SIGLEN,I)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
      INTEGER SIGLEN,I
 
**    local variables
      CHARACTER*11 TMP
      INTEGER ISTART,IS,IL,L,ILEN
      SAVE
 
      WRITE (TMP,'(I11)') I
      IS = ISTART (TMP)
      IL = ILEN (TMP)
      L  = IL-IS+1
      IF (SIGLEN+L.GT.LEN (STRING)) CALL FATALERR
     &   ('ADDINT','internal error')
      STRING(SIGLEN+1:SIGLEN+L) = TMP(IS:IL)
      SIGLEN = SIGLEN+L
 
      RETURN
      END
**==ADDREA.FOR  
      SUBROUTINE ADDREA (STRING,SIGLEN,R,FORM)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING,FORM
      INTEGER SIGLEN
      REAL R
 
**    local variables
      CHARACTER*30 TMPR
      CHARACTER*30 TMPFORM
      INTEGER IS,IL1,IL2,ISTART,ILEN
      SAVE
 
      TMPFORM = '('//FORM//',A)'
      WRITE (TMPR,TMPFORM) R,'|'
      IL2 = INDEX (TMPR,'|')-1
      IS  = ISTART (TMPR)
      IL1 = ILEN (TMPR(1:IL2))
 
      IF (SIGLEN+(IL1-IS+1).GT.LEN (STRING)) CALL FATALERR
     &   ('ADDREA','internal error')
 
      STRING(SIGLEN+1:SIGLEN+IL1-IS+1) = TMPR(IS:IL1)
      SIGLEN = SIGLEN+IL1-IS+1
 
      RETURN
      END
**==ADDREF.FOR  
      SUBROUTINE ADDREF (STRING,SIGLEN,R,FORM)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING,FORM
      INTEGER SIGLEN
      REAL R
 
**    local variables
      CHARACTER*30 TMPR
      CHARACTER*30 TMPFORM
      INTEGER IL
      SAVE
 
      TMPFORM = '('//FORM//',A)'
      WRITE (TMPR,TMPFORM) R,'|'
      IL = INDEX (TMPR,'|')-1
 
      IF (SIGLEN+IL.GT.LEN (STRING)) CALL FATALERR
     &   ('ADDREF','internal error')
 
      STRING(SIGLEN+1:SIGLEN+IL) = TMPR(1:IL)
      SIGLEN = SIGLEN+IL
 
      RETURN
      END
**==ADDSTF.FOR  
      SUBROUTINE ADDSTF (STRING,SIGLEN,TMP)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING,TMP
      INTEGER SIGLEN
 
**    local variables
      INTEGER L
      SAVE
 
      L = LEN (TMP)
      IF (L.EQ.0) CALL FATALERR ('ADDSTF','internal error')
 
      IF (SIGLEN+L.GT.LEN (STRING)) CALL FATALERR
     &   ('ADDSTF','internal error')
 
      STRING(SIGLEN+1:SIGLEN+L) = TMP
      SIGLEN = SIGLEN+L
 
      RETURN
      END
**==ADDSTR.FOR  
      SUBROUTINE ADDSTR (STRING,SIGLEN,TMP)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING,TMP
      INTEGER SIGLEN
 
**    local variables
      INTEGER ISTART,IS,IL,L,ILEN
      SAVE
 
      IS = ISTART (TMP)
      IL = ILEN (TMP)
      IF (IL.EQ.0) CALL FATALERR 
     &   ('ADDSTR','Cannot add zero-length string')
 
      L  = IL-IS+1
      IF (SIGLEN+L.GT.LEN (STRING)) CALL FATALERR
     &   ('ADDSTR','String too long after concatenation')
 
      STRING(SIGLEN+1:SIGLEN+L) = TMP(IS:IL)
      SIGLEN = SIGLEN+L
 
      RETURN
      END
**==AFINVS.FOR  
      SUBROUTINE AFINVS (FUN,ILF,FUNINV,ILFI,EXIST)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILF,ILFI
      REAL FUN,FUNINV
      DIMENSION FUN(2,ILF/2),FUNINV(2,ILFI/2)
      LOGICAL EXIST
 
**    local variables
      INTEGER I,IFROM
      REAL HELP1,HELP2
      LOGICAL INCR,INCR1,EQUAL,MATCH
      LOGICAL DONE
      SAVE
 
      DATA DONE /.FALSE./
 
      IF (.NOT.DONE) THEN
         WRITE (*,'(A)')
     &     ' MESSAGE from AFINVS: This routine will be',
     &     ' removed in future versions of TTUTIL.',
     &     ' We are sorry for the trouble, Kees & Daniel.'
         DONE = .TRUE.
      END IF
 
*     error checks input table and array lengths
      IF  (MOD(ILF,2).NE.0) CALL FATALERR('AFINVS','array size is odd')
      IF (MOD(ILFI,2).NE.0) CALL FATALERR('AFINVS','array size is odd')
      IF (ILF.NE.ILFI)      CALL FATALERR('AFINVS','ILF not equal ILFI')
      DO 10 I=2,ILF/2
         IF (FUN(1,I).LE.FUN(1,I-1))
     $    CALL FATALERR ('AFINVS','input X values not increase')
10    CONTINUE
 
*     inverse exists table should increase or decrease
      EXIST = .TRUE.
      INCR  = (FUN(2,2).GT.FUN(2,1))
      I     = 3
20    IF (I.LE.ILF/2 .AND. EXIST) THEN
         INCR1 = (FUN(2,I).GT.FUN(2,I-1))
         MATCH = ((INCR1.AND.INCR) .OR. .NOT.(INCR1.OR.INCR))
         EQUAL = (FUN(2,I).EQ.FUN(2,I-1))
         EXIST = .NOT.EQUAL .AND. MATCH
         I = I + 1
      GOTO 20
      END IF
 
      IF (.NOT.EXIST) RETURN
 
*     exchange X and Y
      DO 30 I=1,ILF/2
         HELP1 = FUN(1,I)
         FUNINV(1,I) = FUN(2,I)
         FUNINV(2,I) = HELP1
30    CONTINUE
 
      IF (ILF.GE.4 .AND. .NOT.INCR) THEN
*        table decreases: reverse order of points
         DO 40 I=1,ILF/4
            IFROM = ILF/2 - I + 1
            HELP1 = FUN(1,I)
            HELP2 = FUN(2,I)
            FUNINV(1,I) = FUN(1,IFROM)
            FUNINV(2,I) = FUN(2,IFROM)
            FUNINV(1,IFROM) = HELP1
            FUNINV(2,IFROM) = HELP2
40       CONTINUE
      END IF
 
      RETURN
      END
**==AMBUSY.FOR  
      SUBROUTINE AMBUSY (ITASK,MODULE,ICODE)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK,ICODE
      CHARACTER*(*) MODULE
 
**    local variables and function types
      INTEGER IFINDC,IFND,ILMAX,IN,ISTORE
      CHARACTER*6 MODLIS,LMODNM
      PARAMETER (ILMAX=10)
      DIMENSION ISTORE(0:ILMAX),MODLIS(ILMAX)
 
      SAVE
      DATA ISTORE/0,ILMAX*0/,IN/0/
 
*     local module name
      CALL UPPERC (MODULE)
      LMODNM = MODULE
      IFND   = IFINDC (MODLIS,ILMAX,1,IN,LMODNM)
 
      IF (ITASK.EQ.1) THEN
*        enter input code
         IF (IFND.EQ.0) THEN
*           add module to list
            IN = IN + 1
            IF (IN.GT.ILMAX) CALL FATALERR ('AMBUSY','too many names')
            MODLIS(IN) = LMODNM
            IFND = IN
         END IF
 
*        write input code in list
         ISTORE(IFND) = ICODE
 
      ELSE IF (ITASK.EQ.2) THEN
*        return input code or zero for unknown name
         ICODE = ISTORE (IFND)
 
      ELSE
*        comment
         CALL FATALERR ('AMBUSY','illegal ITASK value')
      END IF
 
      RETURN
      END
**==CHKTSK.FOR  
      SUBROUTINE CHKTSK (MODULE, IULOG, ITOLD, ITASK)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IULOG, ITOLD, ITASK
      CHARACTER*(*) MODULE
 
**    local parameters
      INTEGER ITABLE(4,4), ISTAT, IMES
      CHARACTER MESSAG(10)*58
      SAVE
 
*     The messages that can be generated and the severity (warning
*     or error) are coded in a table.
*     Positive values: WARNING
*     Negative values: ERROR
*     ITASK horizontal, ITOLD, vertical
      DATA ITABLE /1,  0, 10,  7,
     &             2,  3,  0,  0,
     &             2,  0, -5,  8,
     &             0, -4, -6, -9/
 
      DATA MESSAG/
     &   'Repeated initialization !                                 ',
     &   'No Terminal call done before initialization !             ',
     &   'Repeated rate calculation !                               ',
     &   'Rate calculation impossible after terminal call !         ',
     &   'Repeated integration not allowed !                        ',
     &   'Integration impossible after terminal call !              ',
     &   'Terminal call without simulation !                        ',
     &   'Terminal call after integration, some output may be lost !',
     &   'Repeated terminal call not allowed !                      ',
     &   'Integration after initialization, no rates available !    '/
 
      IF (ITASK.LT.1.OR.ITASK.GT.4) CALL FATALERR (MODULE,'wrong ITASK')
      IF (ITOLD.LT.1.OR.ITOLD.GT.4) CALL FATALERR (MODULE,'wrong ITOLD')
 
      ISTAT = ITABLE(ITASK, ITOLD)
      IMES  = ABS (ISTAT)
 
      IF (ISTAT.LT.0) THEN
         CALL FATALERR (MODULE, MESSAG(IMES))
      ELSE IF (ISTAT.GT.0) THEN
         WRITE (*,'(4A)')
     &     ' WARNING from ',MODULE,': ',MESSAG(IMES)
         IF (IULOG.NE.0) WRITE (IULOG,'(4A)')
     &     ' WARNING from ',MODULE,': ',MESSAG(IMES)
      END IF
 
      RETURN
      END
**==COPFL2.FOR  
      SUBROUTINE COPFL2 (IIN, FILE, IOUT, HEADER)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IIN, IOUT
      CHARACTER*(*) FILE
      LOGICAL HEADER
 
**    local variables
      INTEGER IOS, I1, IL, ILEN
      LOGICAL LOGTMP, FLEXIST
      CHARACTER LINE*255, LINE2*72
      SAVE
 
 
*     check on output file
      INQUIRE (UNIT=IOUT,OPENED=LOGTMP)
      IF (.NOT.LOGTMP) CALL FATALERR ('COPFL2','output file not open')
 
*     check on input file name
      IL = ILEN (FILE)
      IF (IL.EQ.0) CALL FATALERR ('COPFL2','no input file name')
 
      LOGTMP = FLEXIST (FILE(1:IL))
      IF (LOGTMP) THEN
         CALL FOPENG (IIN, FILE(1:IL), 'OLD','FS',0,' ')
 
         IF (HEADER) THEN
            DO I1=2,71
               LINE2(I1:I1) = '-'
            END DO
            LINE2(1:1) = '*'
            LINE2(72:72) = '*'
 
            WRITE (IOUT, '(A)')   LINE2
            WRITE (IOUT, '(2A,T72,A)')
     &        '* Contents of input file: ',FILE(1:IL),'*'
            WRITE (IOUT, '(A,/)') LINE2
         END IF
 
         IOS = 0
10       IF (IOS.EQ.0) THEN
            READ (IIN, '(A)', IOSTAT=IOS) LINE
            IF (IOS.EQ.0) THEN
               I1 = MAX (1, ILEN (LINE))
               WRITE (IOUT, '(A)') LINE(1:I1)
            END IF
         GOTO 10
         END IF
 
         CLOSE (IIN)
      END IF
 
      RETURN
      END
**==DECCHK.FOR  
      LOGICAL FUNCTION DECCHK (STRING)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
 
*     Local variables
      INCLUDE 'rdndec.gin'
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
      CALL RDERRI (' ',0,0,.FALSE.,.FALSE.)
 
      CALL PARSWORD (STRING,TTYPE,IB,IE,NERR,NWAR)
 
      DECCHK = NERR.EQ.0.AND.TTYPE.EQ.TFLOAT
 
      RETURN
      END
**==DECINT.FOR  
      SUBROUTINE DECINT (IWAR,STRING,VALUE)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IWAR, VALUE
      CHARACTER*(*) STRING
 
**    local variables + functions used
      INCLUDE 'rdndec.gin'
      INCLUDE 'rddecinf.inc'
      INTEGER TTYPE, IB, IE, NERR, NWAR
 
      SAVE
 
      CALL RDERRI (' ',0,0,.FALSE.,.FALSE.)
 
      CALL PARSWORD (STRING,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0.AND.TTYPE.EQ.TINTEG) THEN
*        floating point value
         IWAR  = 0
         VALUE = VINT
      ELSE
         IWAR  = 1
         VALUE = 0
      END IF
 
      RETURN
      END
**==DECREA.FOR  
      SUBROUTINE DECREA (IWAR,STRING,VALUE)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IWAR
      REAL VALUE
      CHARACTER*(*) STRING
 
**    local variables + functions used
      INCLUDE 'rdndec.gin'
      INCLUDE 'rddecinf.inc'
      INTEGER TTYPE, IB, IE, NERR, NWAR
 
      SAVE
 
      CALL RDERRI (' ',0,0,.FALSE.,.FALSE.)
 
      CALL PARSWORD (STRING,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0.AND.TTYPE.EQ.TFLOAT) THEN
*        floating point value
         IWAR  = 0
         VALUE = REAL(VFLOAT)
      ELSE
         IWAR  = 1
         VALUE = 0.0
      END IF
 
      RETURN
      END
**==DECREC.FOR  
      SUBROUTINE DECREC (RECORD,ILX,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILX
      CHARACTER*(*) RECORD
      REAL X
      DIMENSION X(ILX)
 
**    local variables
      INTEGER I,ICHR,IFND,IW1,IW2,IWAR
      CHARACTER CHAR*1
      LOGICAL SEPAR
      SAVE
 
*     initial
      IFND = 0
      IW1  = 0
      IW2  = 0
      ICHR = LEN (RECORD)
 
*     loop over all characters
      DO 10 I=1,ICHR
         CHAR  = RECORD(I:I)
         SEPAR = (CHAR.EQ.' ' .OR. CHAR.EQ.',')
 
*        start of word
         IF (IW1.EQ.0 .AND. .NOT.SEPAR) IW1 = I
 
         IF (IW1.GT.0) THEN
            IF (SEPAR) THEN
*              end of word found
               IW2 = I - 1
            ELSE IF (I.EQ.ICHR) THEN
*              end of line still reading digits
               IW2 = I
            END IF
         END IF
 
         IF (IW2.GT.0) THEN
*           decode number
            IFND = IFND + 1
            CALL DECREA (IWAR,RECORD(IW1:IW2),X(IFND))
            IF  (IWAR.NE.0)  GOTO 20
            IF (IFND.EQ.ILX) RETURN
            IW1 = 0
            IW2 = 0
         END IF
10    CONTINUE
 
20    CONTINUE
      IF (IFND.EQ.0) THEN
         CALL FATALERR ('DECREC','Record empty')
      ELSE
         WRITE (*,'(1X,A,I3,A,/,1X,A)')
     $    'Cannot decode as',ILX,' numbers record:',RECORD(1:ICHR)
         CALL FATALERR ('DECREC','Execution terminated')
      END IF
      END
**==DELFIL.FOR  
      SUBROUTINE DELFIL (FILE_NAME,NOT_EX_ERR)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) FILE_NAME
      LOGICAL NOT_EX_ERR
 
**    Local variables
      LOGICAL THERE
      INTEGER UN,GETUN,IL,IOS,ILEN
      CHARACTER*132 LFNAME
      SAVE
 
      IL = ILEN (FILE_NAME)
      IF (IL.EQ.0) CALL FATALERR ('DELFIL','empty file name')
 
      LFNAME = FILE_NAME
      CALL FLNAME (LFNAME)
      INQUIRE (FILE=LFNAME(1:IL),EXIST=THERE)
 
      IF (THERE) THEN
 
         UN = GETUN (10,99)
         OPEN (UN,FILE=LFNAME(1:IL),STATUS='OLD',
     &         ACCESS='SEQUENTIAL',FORM='UNFORMATTED',IOSTAT=IOS)
         IF (IOS.NE.0) CALL FATALERR ('DELFIL','cannot open file')
 
         CLOSE (UN,STATUS='DELETE',IOSTAT=IOS)
         IF (IOS.NE.0) CALL FATALERR ('DELFIL','cannot open file')
 
      ELSE IF (.NOT.THERE.AND.NOT_EX_ERR) THEN
 
         CALL FATALERR ('DELFIL','file to be deleted does not exist')
 
      END IF
 
      RETURN
      END
**==DTARDP.FOR  
      SUBROUTINE DTARDP (DATEA,FSEC,DPDTTM)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER DATEA(6)
      REAL FSEC
      DOUBLE PRECISION DPDTTM
 
**    Local variables
      INTEGER ITASK
      LOGICAL ERR
      CHARACTER*80 MESSAG
      SAVE
 
      ITASK = 1
      CALL DTSYS (ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)
 
      IF (ERR) THEN
         DPDTTM = 0.D0
         CALL FATALERR ('DTARDP',MESSAG)
      END IF
 
      RETURN
      END
 
**==DTDPAR.FOR  
      SUBROUTINE DTDPAR (DPDTTM,DATEA,FSEC)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER DATEA(6)
      REAL FSEC
      DOUBLE PRECISION DPDTTM
 
**    Local variables
      INTEGER ITASK,I1
      LOGICAL ERR
      CHARACTER*80 MESSAG
      SAVE
 
      ITASK = 2
      CALL DTSYS (ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)
 
      IF (ERR) THEN
*        reset everything
         DO 10 I1=1,6
            DATEA(I1) = 0
10       CONTINUE
         FSEC = 0.
         CALL FATALERR ('DTDPAR',MESSAG)
      END IF
 
      RETURN
      END
**==DTDPST.FOR  
      SUBROUTINE DTDPST (FORM,DPDTTM,STRNG)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) FORM, STRNG
      DOUBLE PRECISION DPDTTM
 
**    Local variables
      INTEGER DATEA(6)
      REAL FSEC,FSEC2
 
      INTEGER MNW
      PARAMETER (MNW=20)
      INTEGER IWBEG(MNW),IWEND(MNW)
 
      INTEGER I1,I2,IFNDO,IS,IE,ILTMPS,ILFORM,IC,IFND
      INTEGER IFINDG,ILEN,ILSTR,IDUM
      LOGICAL CHRNOW,WASCHR,FSFND
      CHARACTER LFORM*80
 
*     declaration of strings to hold formatted values, note
*     the string STR should be declared at the maximum length of
*     YR, MO etc. and MONN
      CHARACTER YR*4,MO*2,DAY*2,HR*2,MN*2,SEC*2,FS*8
      CHARACTER TMPS*9
 
*     declare edit names
      INTEGER NEDNAM
      PARAMETER (NEDNAM=9)
      CHARACTER EDNAM(NEDNAM)*8
 
*     declare month names
      CHARACTER MONN(12)*9
      SAVE
 
      DATA  EDNAM /'YEAR','MONTH','DAY',
     &             'HOUR','MINUTE','SECONDS','FSECONDS',
     &             'MONTHST','MONTHLT'/
 
      DATA MONN /'January','February','March','April',
     &           'May','June','July','August',
     &           'September','October','November','December'/
 
*     convert double precision time to array
      CALL DTDPAR (DPDTTM,DATEA,FSEC)
 
*     initial
      DO 10 I1=1,MNW
         IWBEG(I1) = 0
         IWEND(I1) = 0
10    CONTINUE
 
*     make format string local and convert to uppercase
      ILFORM = ILEN (FORM)
      IF (ILFORM.EQ.0) CALL FATALERR ('DTDPST','empty format string')
      IF (ILFORM.GT.LEN(FORM)) CALL FATALERR ('DTDPST',
     &    'format string too long')
      LFORM = ' '
      LFORM = FORM(1:ILFORM)
      CALL UPPERC (LFORM)
 
*     search through format specification
      IFND   = 0
      WASCHR = .FALSE.
      DO 20 I1=1,ILFORM
         IC = ICHAR (LFORM(I1:I1))
*        all letters A-Z and a-z are valid
         CHRNOW = (IC.GE.65.AND.IC.LE.90).OR.(IC.GE.97.AND.IC.LE.122)
 
         IF (.NOT.WASCHR .AND. CHRNOW) THEN
*           start new edit descriptor
            IFND = IFND + 1
            IWBEG(IFND) = I1
         ELSE IF (WASCHR.AND..NOT.CHRNOW) THEN
*           end of edit descriptor found
            IWEND(IFND) = I1 - 1
            IF (IFND.EQ.MNW) CALL FATALERR ('DTDPST',
     &         'date/time format too long')
         END IF
 
         WASCHR = CHRNOW
 
20    CONTINUE
      IF (WASCHR) IWEND(IFND) = ILFORM
 
*     search string for FSEC edit descriptor, set FSFND to .true. when
*     found
      FSFND = .FALSE.
      DO 30 I1=1,IFND
         CALL SFINDG (EDNAM,NEDNAM,1,NEDNAM,
     &                LFORM(IWBEG(I1):IWEND(I1)),2,
     &                IFINDG,IDUM)
         IF (IFINDG.EQ.7) FSFND = .TRUE.
30    CONTINUE
 
*     write date values on strings
      WRITE (YR ,'(I4.4)') DATEA(1)
      WRITE (MO ,'(I2.2)') DATEA(2)
      WRITE (DAY,'(I2.2)') DATEA(3)
      WRITE (HR ,'(I2.2)') DATEA(4)
      WRITE (MN ,'(I2.2)') DATEA(5)
      IF (FSFND) THEN
*        fractional seconds found in format string, do not round
         WRITE (SEC,'(I2.2)') DATEA(6)
 
*        round to six digit accuracy
         FSEC2 = REAL (INT (FSEC*1000000.))/1000000.
         WRITE (FS ,'(F8.6)') FSEC2
         IF (FS(1:1).EQ.' ') FS(1:1) = '0'
      ELSE
*        fractional seconds not found in format string,
*        round seconds off
         I2 = DATEA(6)
         IF (FSEC.GE.0.5) I2 = I2+1
         WRITE (SEC,'(I2.2)') I2
         FS = ' '
      END IF
 
      STRNG  = ' '
      ILSTR  = LEN (STRNG)
      ILTMPS = 0
      IS     = 0
      IE     = 0
      IF (IWBEG(1).GT.1) THEN
*        separators present at beginning of format string
         IS = IE+1
         IE = IS+(IWBEG(1)-1)-1
         IF (IE.GT.ILSTR) CALL FATALERR
     &      ('DTDPST','output string too small')
         STRNG(IS:IE) = LFORM(1:IWBEG(1)-1)
      END IF
 
*     go through edit descriptors, look them up in the list of valid
*     descriptors write values onto output string and add separators in
*     between
      IFINDG = 6
      DO 40 I1=1,IFND
 
*        search in list of edit descriptors
         IFNDO = IFINDG
         CALL SFINDG (EDNAM,NEDNAM,1,NEDNAM,
     &                LFORM(IWBEG(I1):IWEND(I1)),2,
     &                IFINDG,IDUM)
 
         IF (IFINDG.EQ.1) THEN
*           year descriptor found
            TMPS = YR
         ELSE IF (IFINDG.EQ.2) THEN
*           month descriptor found
            TMPS = MO
         ELSE IF (IFINDG.EQ.3) THEN
*           day descriptor found
            TMPS = DAY
         ELSE IF (IFINDG.EQ.4) THEN
*           hour descriptor found
            TMPS = HR
         ELSE IF (IFINDG.EQ.5) THEN
*           minute descriptor found
            TMPS = MN
         ELSE IF (IFINDG.EQ.6) THEN
*           seconds descriptor found
            TMPS = SEC
         ELSE IF (IFINDG.EQ.7) THEN
*           fractional seconds descriptor found
 
*           check if FSEC was preceded by nothing or a SEC descriptor
            IF (IFNDO.NE.6) CALL FATALERR ('DTDPST',
     &      'Fractional seconds not allowed after previous descriptor')
 
*           remove zero and dot when the FSEC edit descriptor was
*           preceded by a dot
 
            I2 = ILEN (FS)
            IF (IE.GE.1) THEN
               IF (STRNG(IE:IE).EQ.'.') THEN
                  TMPS = FS(3:I2)
               ELSE
                  TMPS = FS(1:I2)
               END IF
            ELSE
               TMPS = FS(1:I2)
            END IF
 
         ELSE IF (IFINDG.EQ.8) THEN
*           MON_ST found, use month in short text
*           notation (JAN, FEB etc.)
            TMPS = MONN(DATEA(2))(1:3)
         ELSE IF (IFINDG.EQ.9) THEN
*           MON_ST found, use month in long text notation (JANUARY etc.)
            TMPS = MONN(DATEA(2))
         ELSE
            WRITE (*,*) 'Edit descriptor: ',LFORM(IWBEG(I1):IWEND(I1))
            CALL FATALERR ('DTDPST','Date/time edit descriptor unknown')
         END IF
 
*        write to output string
         ILTMPS = ILEN (TMPS)
         IS     = IE+1
         IE     = IS+ILTMPS-1
         IF (IE.GT.ILSTR) CALL FATALERR
     &      ('DTDPST','output string too small')
         STRNG(IS:IE) = TMPS(1:ILTMPS)
 
         IF (I1.LT.IFND) THEN
*           add separators
            IS = IE+1
            IE = IS+(IWBEG(I1+1)-IWEND(I1)-1)-1
            IF (IE.GT.ILSTR) CALL FATALERR
     &         ('DTDPST','output string too small')
            STRNG(IS:IE) = LFORM(IWEND(I1)+1:IWBEG(I1+1)-1)
         ELSE IF (IWEND(IFND).LT.ILFORM) THEN
*           add separators if format string does not end
*           at last separator
            IS = IE+1
            IE = IS+(ILFORM+1-IWEND(I1)-1)-1
            IF (IE.GT.ILSTR) CALL FATALERR
     &         ('DTDPST','output string too small')
            STRNG(IS:IE) = LFORM(IWEND(I1)+1:ILFORM)
         END IF
40    CONTINUE
 
      RETURN
      END
**==DTFSECMP.FOR
      INTEGER FUNCTION DTFSECMP (IYEAR1,IDOY1,IYEAR2,IDOY2)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IYEAR1, IDOY1, IYEAR2, IDOY2
 
**    no local variables
      SAVE
 
      IF (IYEAR1.LT.1500.OR.IYEAR2.LT.1500.OR.IYEAR1.EQ.IYEAR2) THEN
         IF (IDOY2.LT.IDOY1) THEN
            DTFSECMP = -1
         ELSE IF (IDOY2.GT.IDOY1) THEN
            DTFSECMP = 1
         ELSE
            DTFSECMP = 0
         END IF
      ELSE IF (IYEAR2.LT.IYEAR1) THEN
         IF (IYEAR2.LT.IYEAR1) THEN
            DTFSECMP = -1
         ELSE IF (IYEAR2.GT.IYEAR1) THEN
            DTFSECMP = 1
         END IF
      END IF
 
      RETURN
      END
**==DTFSEDP.FOR 
      SUBROUTINE DTFSEDP (IYEAR,IDOY,DOY,DPDTTM)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IYEAR,IDOY
      REAL DOY
      DOUBLE PRECISION DPDTTM
 
**    Local variables
      DOUBLE PRECISION TMP_D
      INTEGER TMP_I(6)
      REAL FSEC
      SAVE
 
      TMP_I(1) = IYEAR
      TMP_I(2) = 1
      TMP_I(3) = 1
      TMP_I(4) = 0
      TMP_I(5) = 0
      TMP_I(6) = 0
      FSEC     = 0.
 
      CALL DTARDP (TMP_I,FSEC,TMP_D)
      DPDTTM = TMP_D+DBLE (IDOY-1)+DBLE (MAX (DOY-REAL (IDOY),0.))
 
      RETURN
      END
**==DTLEAP.FOR  
      LOGICAL FUNCTION DTLEAP (YEAR)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER YEAR
 
**    no local variables
      SAVE
 
      DTLEAP = MOD (YEAR,4).EQ.0
      IF (MOD (YEAR,100).EQ.0) DTLEAP = .FALSE.
      IF (MOD (YEAR,400).EQ.0) DTLEAP = .TRUE.
 
      RETURN
      END
 
**==DTNOW.FOR   
      SUBROUTINE DTNOW (DATEA)
 
*     returns system date and time,
*     dummy routine
 
      IMPLICIT NONE
 
*     formal parameters
      INTEGER DATEA(6)
 
*     local parameters
      SAVE
 
      DATEA(1) = 1900
      DATEA(2) = 1
      DATEA(3) = 1
      DATEA(4) = 0
      DATEA(5) = 0
      DATEA(6) = 0
 
      RETURN
      END
**==DTSYS.FOR   
      SUBROUTINE DTSYS (ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK, DATEA(6)
      LOGICAL ERR
      REAL FSEC
      DOUBLE PRECISION DPDTTM
      CHARACTER*(*) MESSAG
 
**    Local variables
      INTEGER I1, NZERO, DAYMAX, NDAYS
      INTEGER IMOTB1(12),IMOTB2(12)
      DOUBLE PRECISION DTMP, DTMAX
      LOGICAL DTLEAP
      INTEGER INDAY, ISECS
      DOUBLE PRECISION FRDAY, DSECS, FSECS
 
*     The absolute day number belonging to a certain day is
*     the number of days passed since 1-jan-0001. Hence, the
*     absolute day number of 1-jan-0001 is 0. The absolute
*     day number belonging to 1-jan-0002 is 365.
*     The number of days N calculated for a certain year Y is
*     given by N = Y*365 + Y/4 - Y/100 + Y/400 - 1.
*     The 1 subtracted simplifies the use of DOY numbers.
*     For 1-jan-0002 for instance DOY=1. If this number is added 
*     to the N = 364 calculated for the passed year, year 1, we 
*     have again the absolute day number belonging to 1-jan-0002.
*     The offset value used in the TTUTIL day counting should
*     give the number 1 to 1-jan-1900, which implies:
*     ttutiloffset = 1899 * 365 + 1899/4 - 1899/100 + 1899/400 - 1.
*     Dates below 1-jan-0001 and above 31-dec-9999 are illegal.
 
      INTEGER OFFSET
      PARAMETER (OFFSET = 693594)
      INTEGER N400, N100, N4, N1, LEFT, LASTY, DLAST
      SAVE
 
      DATA IMOTB1 /0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IMOTB2 /31,28,31,30,31,30,31,31,30,31,30,31/
 
      ERR    = .FALSE.
      MESSAG = ' '
 
*     value corresponding to 31-dec-9999 24:00
      DTMAX = DBLE (9999*365 + 9999/4 - 9999/100 + 9999/400 - OFFSET)
 
      IF (ITASK.EQ.1) THEN
 
*        convert from array to double precision real
         NZERO  = 0
         DO 10 I1=1,3
            IF (DATEA(I1).EQ.0) NZERO = NZERO+1
10       CONTINUE
 
         IF (NZERO.GT.0.AND.NZERO.LT.3) THEN
            ERR    = .TRUE.
            MESSAG = 'year 0 or partial date not allowed'
            RETURN
         END IF
 
*        days for the year
         IF (DATEA(1).GE.1) THEN
*           absolute day number belonging to 31 december previous year
            LASTY = DATEA(1) - 1
            INDAY = LASTY*365 + LASTY/4 - LASTY/100 + LASTY/400 - 1
         ELSE IF (DATEA(1).EQ.0) THEN
*           set date to TTUTIL offset value
            INDAY = OFFSET
         ELSE
            ERR    = .TRUE.
            MESSAG = 'illegal year'
            RETURN
         END IF
 
*        count days in month
         IF (DATEA(2).NE.0) THEN
            IF (DATEA(2).GE.1.AND.DATEA(2).LE.12) THEN
               IF (DATEA(2).GE.2) THEN
                  INDAY = INDAY+IMOTB1(DATEA(2))
                  IF (DTLEAP(DATEA(1)).AND.DATEA(2).GT.2)
     &                INDAY = INDAY + 1
               END IF
            ELSE
               ERR    = .TRUE.
               MESSAG = 'month invalid'
               RETURN
            END IF
         END IF
 
*        count days in day
         IF (DATEA(3).NE.0) THEN
            DAYMAX = IMOTB2(DATEA(2))
            IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) DAYMAX = DAYMAX+1
 
            IF (DATEA(3).GE.1.AND.DATEA(3).LE.DAYMAX) THEN
               INDAY = INDAY + DATEA(3)
            ELSE
               ERR    = .TRUE.
               MESSAG = 'day invalid'
               RETURN
            END IF
         END IF
 
*        subtract TTUTIL offset value and convert
         DTMP = DBLE(INDAY-OFFSET)
 
         IF (DATEA(4).GE.0) THEN
*           important: hours can be more than 24 !!
            DTMP = DTMP+DBLE (DATEA(4))/24.D0
         ELSE
            ERR    = .TRUE.
            MESSAG = 'hour invalid'
            RETURN
         END IF
 
         IF (DATEA(5).GE.0.AND.DATEA(5).LE.59) THEN
            DTMP = DTMP+DBLE (DATEA(5))/1440.D0
         ELSE
            ERR    = .TRUE.
            MESSAG = 'minute invalid'
            RETURN
         END IF
 
         IF (DATEA(6).GE.0.AND.DATEA(6).LE.59) THEN
            DTMP = DTMP+DBLE (DATEA(6))/86400.D0
         ELSE
            ERR    = .TRUE.
            MESSAG = 'seconds invalid'
            RETURN
         END IF
 
         IF (FSEC.GE.0.AND.FSEC.LT.1.) THEN
            DTMP = DTMP+DBLE (FSEC)/86400.D0
         ELSE
            ERR    = .TRUE.
            MESSAG = 'fractional seconds invalid'
            RETURN
         END IF
 
         IF (DTMP .GE. DTMAX) THEN
            ERR    = .TRUE.
            MESSAG = 'year above 9999'
            RETURN
         END IF
 
         DPDTTM = DTMP
 
      ELSE IF (ITASK.EQ.2) THEN
 
*        convert from double precision real to date array and
*        fractional seconds
 
         IF (DPDTTM .GE. DTMAX) THEN
            ERR    = .TRUE.
            MESSAG = 'year above 9999'
            RETURN
         END IF
 
*        get integer and fractional day
         IF (DPDTTM .GE. 0.0D0) THEN
            INDAY = INT(DPDTTM)
            FRDAY = DPDTTM - DBLE(INDAY)
         ELSE
            INDAY = -INT(ABS(DPDTTM))
            FRDAY = ABS(DPDTTM) - DBLE(ABS(INDAY))
            IF (FRDAY .GT. 0.0D0) THEN
*              there is a fractional day ; get it right
               FRDAY = 1.0D0 - FRDAY
               INDAY = INDAY - 1
            END IF
         END IF
 
*        get fractional day in seconds
         DSECS = 86400.D0 * FRDAY
         ISECS = INT(DSECS)
         FSECS = DSECS - DBLE(ISECS)
 
*        if the fractional seconds are close to 1.0 ?
         IF (FSECS .GT. 0.999999D0) THEN
*           regard as 1.0
            FSECS = 0.0D0
            ISECS = ISECS + 1
            IF (ISECS.EQ.86400) THEN
*              regard as a day
               ISECS = 0
               INDAY = INDAY + 1
            END IF
         END IF
 
*        now we have integer days and integer seconds for the day
*        corrected for a nearly finished second
*        now the date calculation can be done in integer arithmetic
 
*        alogorithm from FSE 4 calendar routine
         INDAY = INDAY + OFFSET
 
         IF (INDAY.LT.0) THEN
            ERR    = .TRUE.
            MESSAG = 'date before 1-jan-0001'
            RETURN
         END IF
 
*        number of 400 year periods and number of days left
         N400 = INDAY / 146097                  
         LEFT = INDAY - N400 * 146097
 
*        number of 100 year periods (max 3) and the number of days left
         N100 = MIN (3, LEFT / 36524)
         LEFT = LEFT - N100 * 36524
 
*        number of 4 year periods and the number of days left
         N4   = LEFT / 1461
         LEFT = LEFT - N4 * 1461
 
*        number of 1 year periods (max 3) and the number of days left
         N1   = MIN (3, LEFT / 365)
         LEFT = LEFT - N1 * 365
 
*        current year number is all years passed plus 1
         DATEA(1) = n400*400 + n100*100 + n4*4 + n1 + 1
 
*        find day number in year (DOY) by subtracting last years
         LASTY = DATEA(1) - 1
         DLAST = LASTY*365 + LASTY/4 - LASTY/100 + LASTY/400 - 1
         INDAY = INDAY - DLAST
 
*        find out month number
         DATEA(2) = 1
         NDAYS = IMOTB2(DATEA(2))
         IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) NDAYS = NDAYS+1
 
20       IF (INDAY.GT.NDAYS) THEN
            INDAY    = INDAY-NDAYS
            DATEA(2) = DATEA(2)+1
            NDAYS    = IMOTB2(DATEA(2))
            IF (DTLEAP (DATEA(1)).AND.DATEA(2).EQ.2) NDAYS = NDAYS+1
         GOTO 20
         END IF
 
*        day number in month
         DATEA(3) = INDAY
 
*        hh:mm:ss
         DATEA(4) = MOD(ISECS,86400) / 3600
         DATEA(5) = MOD(ISECS,3600) / 60
         DATEA(6) = MOD(ISECS,60)
 
*        fractional seconds
         FSEC = REAL (FSECS)
 
      ELSE
         CALL FATALERR ('DTSYS','unknown task')
      END IF
 
      RETURN
      END
**==ENTCHA.FOR  
      SUBROUTINE ENTCHA (QUEST,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) QUEST,X

**    local variables
      INTEGER LQ,LX,IOS
 
*     functions used
      INTEGER ILEN
      SAVE
 
*     string lengths
      LQ = ILEN (QUEST)
      LX = LEN (X)
 
*     ask the question and read the answer
10    CONTINUE
      WRITE (*,'(A50,A,$)') QUEST(1:LQ),': '
      READ (*,'(A)',IOSTAT=IOS) X(1:LX)
 
      IF (IOS.LT.0) THEN
         STOP ' End_Of_File detected ; program STOP'
 
      ELSE IF (IOS.GT.0) THEN
*        error during READ : give message and try again
         WRITE (*,'(/,A,/,A,/)')
     $      ' Enter a CHARACTER string !!',
     $      ' Use <CTRL> Z  to STOP (<Command>. on Mac)'
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTDCH.FOR  
*     ---------------------------------------------
*     enter string with default
*     ---------------------------------------------
      SUBROUTINE ENTDCH (QUEST,SDEF,S)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) QUEST,SDEF,S
 
**    local variables + functions called
      INTEGER IL,ILEN
      CHARACTER INPUT*40
      SAVE
 
      CALL ENTHLP (QUEST,SDEF,INPUT)
 
*     length of input string
      IL = ILEN (INPUT)
      IF (IL.EQ.0) THEN
*        default
         S = SDEF
      ELSE
*        new string supplied
         S = INPUT
      END IF
 
      RETURN
      END
**==ENTDDO.FOR  
*     ---------------------------------------------
*     enter double precision with default
*     ---------------------------------------------
      SUBROUTINE ENTDDO (QUEST,XDEF,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      DOUBLE PRECISION XDEF,X
      CHARACTER*(*) QUEST
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
*     value info
      INCLUDE 'rddecinf.inc'
 
**    local variables + functions called
      CHARACTER INPUT*40, HULP*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
*     write default to string
      WRITE (HULP,'(1P,G20.8)') XDEF
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTHLP (QUEST,HULP,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TFLOAT) THEN
*           floating point value
            X = VFLOAT
         ELSE IF (TTYPE .EQ. TSPACE) THEN
*           default
            X = XDEF
         ELSE
*           other data type
            CALL RDERR (2,'Not a floating point value')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTDIN.FOR  
*     ---------------------------------------------
*     enter integer with default
*     ---------------------------------------------
      SUBROUTINE ENTDIN (QUEST,IXDEF,IX)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IXDEF,IX
      CHARACTER*(*) QUEST
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
*     value info
      INCLUDE 'rddecinf.inc'
 
**    local variables + functions called
      CHARACTER INPUT*40, HULP*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
*     write default to string
      WRITE (HULP,'(I16)') IXDEF
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTHLP (QUEST,HULP,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TINTEG) THEN
*           integer
            IX = VINT
         ELSE IF (TTYPE .EQ. TSPACE) THEN
*           default
            IX = IXDEF
         ELSE
*           other data type
            CALL RDERR (2,'Not an INTEGER value')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTDOU.FOR  
*     ---------------------------------------------
*     enter double precision
*     ---------------------------------------------
      SUBROUTINE ENTDOU (QUEST,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER QUEST*(*)
      DOUBLE PRECISION X
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
*     value info
      INCLUDE 'rddecinf.inc'
 
**    local variables
      CHARACTER INPUT*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTCHA (QUEST,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TFLOAT) THEN
*           floating point value
            X = VFLOAT
         ELSE
*           other data type
            CALL RDERR (2,'Not a floating point value')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTDRE.FOR  
*     ---------------------------------------------
*     enter real with default
*     ---------------------------------------------
      SUBROUTINE ENTDRE (QUEST,XDEF,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL XDEF,X
      CHARACTER*(*) QUEST
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
*     value info
      INCLUDE 'rddecinf.inc'
 
**    local variables + functions called
      CHARACTER INPUT*40, HULP*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
*     write default to string
      WRITE (HULP,'(1P,G14.5)') XDEF
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTHLP (QUEST,HULP,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TFLOAT) THEN
*           floating point value
            X = REAL(VFLOAT)
         ELSE IF (TTYPE .EQ. TSPACE) THEN
*           default
            X = XDEF
         ELSE
*           other data type
            CALL RDERR (2,'Not a REAL value')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTDTI.FOR  
*     ---------------------------------------------
*     enter date/time with default
*     ---------------------------------------------
      SUBROUTINE ENTDTI (QUEST,DTDEF,DT)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      DOUBLE PRECISION DTDEF,DT
      CHARACTER*(*) QUEST
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
*     value info
      INCLUDE 'rddecinf.inc'
 
**    local variables + functions called
      CHARACTER INPUT*40, HULP*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
*     write default to string
      CALL DTDPST ('YEAR/MONTHLT/DAY_HOUR:MIN:SEC.FSEC',DTDEF,HULP)
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTHLP (QUEST,HULP,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TTIME) THEN
*           floating point value
            DT = VTIME
         ELSE IF (TTYPE .EQ. TSPACE) THEN
*           default
            DT = DTDEF
         ELSE
*           other data type
            CALL RDERR (2,'Not a date/time')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTDYN.FOR  
*     ---------------------------------------------
*     enter Yes/No with default
*     ---------------------------------------------
      SUBROUTINE ENTDYN (QUEST,YNDEF,YN)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER QUEST*(*)
      LOGICAL YNDEF,YN
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
**    local variables
      CHARACTER INPUT*40,HULP*1,YES*3,NO*3
      INTEGER TTYPE, IB, IE, NERR, NWAR, IL
      SAVE
 
      DATA YES/'YES'/, NO/'NO '/
 
*     write default to string
      IF (YNDEF) THEN
         HULP = 'Y'
      ELSE
         HULP = 'N'
      END IF
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTHLP (QUEST,HULP,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IL = IE-IB+1
         IF (TTYPE .EQ. TIDENT .AND. IL.LE.3) THEN
            CALL UPPERC (INPUT(IB:IE))
            IF (INPUT(IB:IE).EQ.YES(1:IL)) THEN
*              yes
               YN = .True.
            ELSE IF (INPUT(IB:IE).EQ.NO(1:IL)) THEN
*              no
               YN = .False.
            ELSE
*              wrong
               CALL RDERR (2,'This is not Yes or No')
               GOTO 10
            END IF
         ELSE IF (TTYPE .EQ. TSPACE) THEN
*           default
            YN = YNDEF
         ELSE
*           other data type
            CALL RDERR (2,'This is not Yes or No')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTHLP.FOR  
*     ---------------------------------------------
*     question with default string, returns typed string
*     ---------------------------------------------
      SUBROUTINE ENTHLP (QUEST,SDEF,INPUT)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) QUEST,SDEF,INPUT
 
**    local variables + functions called
      INTEGER IP1,IP2,ISTART,ILEN,LQ
      CHARACTER RECORD*80,SDLOC*80,VRAAG*50
      SAVE
 
*     local copy default
      SDLOC = SDEF
      IP1 = ISTART (SDLOC)
      IP2 = ILEN (SDLOC)
      IP1 = MAX (1,IP1)
      IP2 = MAX (1,IP2)
 
*     question length limited by available space
      LQ = ILEN (QUEST)
      LQ = MAX (1,LQ)
      LQ = MIN (LQ,47)
 
*     ask the question and read the answer
      VRAAG(1:LQ) = QUEST(1:LQ)
*     concatenation with local string (IBM !!)
      IF (LQ+IP2-IP1+5.LE.50) THEN
*        question and default on one line
         RECORD = VRAAG(1:LQ)//' ['//SDLOC(IP1:IP2)//']'
      ELSE
*        question on separate line
         WRITE (*,'(A48)') VRAAG(1:LQ)
         RECORD = ' ['//SDLOC(IP1:IP2)//']'
      END IF
      CALL ENTCHA (RECORD,INPUT)
 
      RETURN
      END
**==ENTINT.FOR  
*     ---------------------------------------------
*     enter integer
*     ---------------------------------------------
      SUBROUTINE ENTINT (QUEST,IX)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER QUEST*(*)
      INTEGER IX
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
*     value info
      INCLUDE 'rddecinf.inc'
 
**    local variables
      CHARACTER INPUT*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTCHA (QUEST,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TINTEG) THEN
*           integer
            IX = VINT
         ELSE
*           other data type
            CALL RDERR (2,'Not an INTEGER value')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTREA.FOR  
*     ---------------------------------------------
*     enter real
*     ---------------------------------------------
      SUBROUTINE ENTREA (QUEST,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER QUEST*(*)
      REAL X
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
*     value info
      INCLUDE 'rddecinf.inc'
 
**    local variables
      CHARACTER INPUT*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTCHA (QUEST,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TFLOAT) THEN
*           floating point value
            X = REAL(VFLOAT)
         ELSE
*           other data type
            CALL RDERR (2,'Not a REAL value')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTTIM.FOR  
*     ---------------------------------------------
*     enter date/time
*     ---------------------------------------------
      SUBROUTINE ENTTIM (QUEST,DT)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      DOUBLE PRECISION DT
      CHARACTER*(*) QUEST
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
*     value info
      INCLUDE 'rddecinf.inc'
 
**    local variables + functions called
      CHARACTER INPUT*40
      INTEGER TTYPE, IB, IE, NERR, NWAR
      SAVE
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTCHA (QUEST,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IF (TTYPE .EQ. TTIME) THEN
*           floating point value
            DT = VTIME
         ELSE
*           other data type
            CALL RDERR (2,'Not a date/time')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==ENTYNO.FOR  
*     ---------------------------------------------
*     enter Yes/No
*     ---------------------------------------------
      SUBROUTINE ENTYNO (QUEST,YN)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER QUEST*(*)
      LOGICAL YN
 
*     integer codes for tokens
      INCLUDE 'rdndec.gin'
 
**    local variables
      CHARACTER INPUT*40,YES*3,NO*3
      INTEGER TTYPE, IB, IE, NERR, NWAR, IL
      SAVE
 
      DATA YES/'YES'/, NO/'NO '/
 
*     ask the question and read the answer
10    CONTINUE
      CALL RDERRI ('Keyboard',0,0,.TRUE.,.FALSE.)
      CALL ENTCHA (QUEST,INPUT)
      CALL PARSWORD (INPUT,TTYPE,IB,IE,NERR,NWAR)
 
      IF (NERR.EQ.0) THEN
         IL = IE-IB+1
         IF (TTYPE .EQ. TIDENT .AND. IL.LE.3) THEN
            CALL UPPERC (INPUT(IB:IE))
            IF (INPUT(IB:IE).EQ.YES(1:IL)) THEN
*              yes
               YN = .True.
            ELSE IF (INPUT(IB:IE).EQ.NO(1:IL)) THEN
*              no
               YN = .False.
            ELSE
*              wrong
               CALL RDERR (2,'This is not Yes or No')
               GOTO 10
            END IF
         ELSE
*           other data type
            CALL RDERR (2,'This is not Yes or No')
            GOTO 10
         END IF
      ELSE
         GOTO 10
      END IF
 
      RETURN
      END
**==EXTENS.FOR  
      SUBROUTINE EXTENS (FILEIN,NEWEXT,ICHECK,FILEOU)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ICHECK
      CHARACTER*(*) FILEIN,FILEOU,NEWEXT
 
**    local variables + function called
      INTEGER I,ILEXT1,ILEXT2,ILFIL,IP,LOUT,IS,ILEN
      CHARACTER*1 OLD,NEW,CHR
      CHARACTER*132 FILBUF
      LOGICAL CHECK, HAAK, FOUND
      SAVE
 
*     remove point from new extension
      IF (NEWEXT(1:1).NE.'.') THEN
         IS = 1
      ELSE
         IS = 2
      END IF
 
*     length of input filename and new extension
      ILFIL  = ILEN (FILEIN)
      ILEXT2 = ILEN (NEWEXT) - IS + 1
      FILBUF = NEWEXT
      LOUT   = LEN (FILEOU)
      IF (ILFIL.EQ.0)  CALL FATALERR ('EXTENS','no filename supplied')
 
*     initialize search for "no extension"
      IP     = ILFIL + 1
      ILEXT1 = 0
      HAAK   = .FALSE.
      FOUND  = .FALSE.
 
*     search for point from the end on
      I = ILFIL
10    IF (I.GT.0 .AND..NOT.(FOUND.OR.HAAK)) THEN
*        check single character
         CHR  = FILEIN(I:I)
*        end of directory string on UNIX, Macintosh, MS-DOS and VAX
         HAAK = CHR.EQ.CHAR(47) .OR. CHR.EQ.CHAR(58) .OR.
     $          CHR.EQ.CHAR(92) .OR. CHR.EQ.CHAR(93)
 
         IF (CHR.EQ.'.') THEN
*           a (valid) point is found
            IP = I
            ILEXT1 = ILFIL - IP
            FOUND  = .TRUE.
         END IF
 
         I = I - 1
      GOTO 10
      END IF
 
*     check and set output name in uppercase
      IF (IP.GT.132) THEN
         WRITE (*,'(1X,2A)') 'Input file name:',FILEIN
         CALL FATALERR ('EXTENS','input filename too long')
      END IF
      FILBUF = ' '
      IF (IP.GE.2) FILBUF = FILEIN(1:IP-1)
      CALL UPPERC (FILBUF)
*     set point
      IF (ILEXT2.GT.0) FILBUF(IP:IP) = '.'
 
*     if old and new extension are equally long ; check !!
      CHECK = (ILEXT1.EQ.ILEXT2) .AND. (ICHECK.NE.0)
 
      DO 20 I=IP+1,IP+ILEXT2
         IF (I.GT.132) THEN
            WRITE (*,'(1X,2A)') 'Input file name:',FILEIN
            CALL FATALERR ('EXTENS','new filename too long')
         END IF
 
*        get character new extension and put into output
         NEW = NEWEXT(I-IP+IS-1:I-IP+IS-1)
         CALL UPPERC (NEW)
         FILBUF(I:I) = NEW
 
         IF (CHECK) THEN
*           get character old extension for comparison
            OLD = FILEIN(I:I)
            CALL UPPERC (OLD)
            IF (OLD.NE.NEW) CHECK=.FALSE.
         END IF
20    CONTINUE
 
      IF (CHECK) THEN
         WRITE (*,'(1X,2A)') 'Input file name:',FILEIN
         CALL FATALERR ('EXTENS','old and new extension equal')
      END IF
 
      IF (LOUT.LT.IP+ILEXT2) THEN
         WRITE (*,'(1X,2A)') 'Input file name:',FILEIN
         CALL FATALERR ('EXTENS','output string too short for new name')
      END IF
 
      FILEOU = FILBUF(1:IP+ILEXT2)
      RETURN
      END
**==FATALERR.FOR
      SUBROUTINE FATALERR (MODULE,MESSAG)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) MODULE, MESSAG
 
**    local variables
      INTEGER I,IL1,IL2,ILEN
      CHARACTER*1 DUMMY
      SAVE
 
*     fudge construction to fool the compiler about the return statement
      I = 0
      IF (I.EQ.0) THEN
         IL1 = ILEN (MODULE)
         IL2 = ILEN (MESSAG)
         IF (IL1.EQ.0.AND.IL2.EQ.0) THEN
            WRITE (*,'(A)')
     &      ' Fatal execution error, press <Enter>'
         ELSE IF (IL1.GT.0.AND.IL2.EQ.0) THEN
            WRITE (*,'(3A)')
     &      ' Fatal execution error in ',MODULE(1:IL1),', press <Enter>'
         ELSE
            WRITE (*,'(4A,/,A)')
     &      ' ERROR in ',MODULE(1:IL1),': ',MESSAG(1:IL2),
     &      ' Press <Enter>'
         END IF
*         READ (*,'(A)') DUMMY
         STOP
      END IF
 
      RETURN
      END
**==FCNSW.FOR   
      REAL FUNCTION FCNSW (X1,X2,X3,X4)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X1,X2,X3,X4
 
**    Local variables
      SAVE
 
      IF (X1.LT.0.) THEN
         FCNSW = X2
      ELSE IF (X1.EQ.0.) THEN
         FCNSW = X3
      ELSE
         FCNSW = X4
      END IF
 
      RETURN
      END
**==FLEXIST.FOR 
      LOGICAL FUNCTION FLEXIST (FILE_NAME)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) FILE_NAME
 
**    Local variables
      CHARACTER*132 FILE_NAME_L
      LOGICAL THERE
      INTEGER IL,ILEN
      SAVE
 
      FILE_NAME_L = FILE_NAME
      CALL FLNAME (FILE_NAME_L)
      IL = ILEN (FILE_NAME_L)
 
      INQUIRE (FILE=FILE_NAME_L(1:IL),EXIST=THERE)
      FLEXIST = THERE
 
      RETURN
      END
**==FLNAME.FOR  
      SUBROUTINE FLNAME (STRING)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
 
**    local variables
      SAVE
 
      CALL LOWERC (STRING)
 
      RETURN
      END
**==FOPENG.FOR  
      SUBROUTINE FOPENG (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IUNIT,IRECL
      CHARACTER*(*) FILNAM, STATUS, PRIV, TYPE
 
**    local variables
      INTEGER ILF,ILFIL,ITMP,IWL,IOS
      CHARACTER CHOICE*1,CREAT*4,EXT*3,FFF*11,FTYPE*4
      CHARACTER LFNAME*132,LPRIV*3,LSTAT*3,LTYPE*2
      LOGICAL DELOLD,KNPRIV,OPENU,OPENF,SEQ,THERE,OK
 
*     function
      INTEGER ILEN
      SAVE
 
*     initialize error variables
      IOS = 0
 
*     make filename local and prepare for operating system
      CALL STR_COPY (FILNAM,LFNAME,OK)
      IF (.NOT.OK) THEN
         WRITE (*,'(/,A)')
     &     'ERROR in FOPENG: File name too long for internal buffer'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
 
      CALL FLNAME (LFNAME)
      ILFIL = ILEN (LFNAME)
 
*     check unit number
*     -----------------
*     unit number has proper value and is free ?
      IF (IUNIT.LT.10.OR.IUNIT.GT.99) THEN
         WRITE (*,'(/,A)')
     &     ' ERROR in FOPENG: Unit number is < 10 or > 99 !'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
 
      INQUIRE (UNIT=IUNIT,OPENED=OPENU)
      IF (OPENU) THEN
*        unit number is in use, get the connected filename
         INQUIRE (UNIT=IUNIT, NAME=LFNAME)
         ILF = ILEN (LFNAME)
         WRITE (*,'(/,A,/,1X,A)')
     &    ' ERROR in FOPENG: Unit number is already in use for file',
     &      LFNAME(1:ILF)
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
 
*     check status
*     ------------
*     get local copies of status, type and privilege
      LSTAT  = STATUS
      CALL UPPERC (LSTAT)
 
*     Old is always readonly
      IF (LSTAT.EQ.'RDO') LSTAT = 'OLD'
 
*     simple value check
      IF (ILEN (STATUS).NE.3.OR.
     &   (LSTAT.NE.'OLD'.AND.LSTAT.NE.'NEW')) THEN
         WRITE (*,'(/,A)') ' ERROR in FOPENG: illegal file status'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
 
*     check type
*     ----------
      LTYPE  = TYPE
      CALL UPPERC (LTYPE)
 
      IF (ILEN (TYPE).NE.2) THEN
*        TYPE should be a two character code
         WRITE (*,'(/,A)') ' ERROR in FOPENG: Illegal TYPE description'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
 
      ELSE IF (INDEX ('FUB',LTYPE(1:1)).EQ.0.AND.
     &         INDEX ('FUB',LTYPE(2:2)).EQ.0) THEN
         WRITE (*,'(/,A)') ' ERROR in FOPENG: No FORM specified'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
 
      ELSE IF (INDEX ('SD',LTYPE(1:1)).EQ.0.AND.
     &         INDEX ('SD',LTYPE(2:2)).EQ.0) THEN
         WRITE (*,'(/,A)') ' ERROR in FOPENG: No ACCESS specified'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
 
*     get form and check
*     ------------------
      IF (INDEX (LTYPE,'F').GT.0) THEN
*        formatted file
         FFF = 'FORMATTED'
      ELSE IF (INDEX (LTYPE,'U').GT.0) THEN
*        unformatted file
         FFF = 'UNFORMATTED'
      ELSE IF (INDEX (LTYPE,'B').GT.0) THEN
*        binary form accepted by compilers:
*        MS Fortran 5.1 on PC
*        MS Powerstation 1.0 on PC
*        Dec Fortran 90 vs 5 on PC
*        Absoft ProFortran 77 on Macintosh
*        Absoft ProFortran 90 on Macintosh
         FFF = 'BINARY'
      END IF
 
      SEQ = INDEX (LTYPE,'S') .GT. 0
 
*     check record length
*     -------------------
      IF ((.NOT.SEQ .AND. IRECL.LE.0).OR.
     &         (SEQ .AND. IRECL.NE.0)) THEN
         WRITE (*,'(/,A)') ' ERROR in FOPENG: Illegal record length'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
 
*     check privilege
*     ---------------
      LPRIV  = PRIV
      CALL UPPERC (LPRIV)
 
      IF (LSTAT.EQ.'OLD'.AND.LPRIV.NE.' ') THEN
         CALL WARNING ('FOPENG',
     &   'status OLD does not require a privilege')
      ELSE IF (LSTAT.EQ.'NEW') THEN
         KNPRIV = LPRIV.EQ.'DEL' .OR. LPRIV.EQ.'NOD' .OR. LPRIV.EQ.'UNK'
 
         IF (ILEN (PRIV).NE.3.OR..NOT.KNPRIV) THEN
            WRITE (*,'(/,A)') ' ERROR in FOPENG: illegal privilege'
            CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
         END IF
      END IF
 
*     check filename and file status
*     ------------------------------
      THERE  = .FALSE.
      OPENF  = .FALSE.
      IF (ILFIL.EQ.0) THEN
         WRITE (*,'(/,A)') ' ERROR in FOPENG: zero length file name'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      ELSE
         INQUIRE (FILE=LFNAME(1:ILFIL),EXIST=THERE,IOSTAT=IOS)
 
         IF (IOS.NE.0) THEN
*           error from system, probably from file name
            WRITE (*,'(/,2A)')
     &       ' ERROR in FOPENG: INQUIRE for file existence not',
     &       ' successfull, check IOS value'
            CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
         END IF
 
         IF (THERE) INQUIRE (FILE=LFNAME(1:ILFIL),OPENED=OPENF)
 
         IF (THERE.AND.OPENF) THEN
            WRITE (*,'(/,A)') ' ERROR in FOPENG: File is already open'
            CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
         END IF
      END IF
 
      IF (LSTAT.EQ.'OLD'.AND..NOT.THERE) THEN
         WRITE (*,'(/,A)') ' ERROR in FOPENG: File does not exist'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
 
      IF (LSTAT.EQ.'NEW' .AND. THERE) THEN
*        action depends on privilege
         DELOLD = .FALSE.
 
         IF (LPRIV.EQ.'UNK') THEN
*           interactive choice
            ITMP = 0
10          IF (THERE) THEN
               WRITE (*,'(3A)')
     &           ' File ',LFNAME(1:ILFIL),' already exists'
20             CONTINUE
               CALL ENTDCH ('Overwrite (Y/N)','N',CHOICE)
               CALL UPPERC (CHOICE)
 
               IF (CHOICE.EQ.'Y') THEN
*                 delete old file
                  DELOLD = .TRUE.
               ELSE IF (CHOICE.EQ.'N') THEN
*                 old file not deleted, suggest new file name
                  ITMP = ITMP + 1
                  WRITE (EXT,'(I3.3)') ITMP
                  CALL EXTENS (FILNAM,EXT,0,LFNAME)
                  CALL ENTDCH ('Enter new file name',LFNAME,LFNAME)
                  ILFIL = ILEN (LFNAME)
                  THERE = .FALSE.
                  INQUIRE (FILE=LFNAME(1:ILFIL),EXIST=THERE,IOSTAT=IOS)
                  IF (IOS.NE.0) THEN
*                    error from system, probably from file name
                     WRITE (*,'(/,2A)')
     &               ' ERROR in FOPENG: INQUIRE for file existence not',
     &                ' successfull, check IOS value'
                      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,
     &                             PRIV,IOS)
                  END IF
                  GOTO 10
               ELSE
*                 illegal choice
                  GOTO 20
               END IF
            END IF
 
         ELSE IF (LPRIV.EQ.'DEL') THEN
*           delete old file
            DELOLD  = .TRUE.
 
         ELSE IF (LPRIV.EQ.'NOD') THEN
            WRITE (*,'(/,A)')
     &       ' ERROR in FOPENG: Existing file cannot be deleted'
            CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
         END IF
 
         IF (DELOLD) THEN
*           old file should be deleted
*           delete file as sequential unformatted file ; this does not
*           lead to an error message for direct access files on the Vax
            OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS='OLD',
     &            ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
            CLOSE (IUNIT,STATUS='DELETE',IOSTAT=IOS)
 
            IF (IOS.NE.0) THEN
*              error from system
               WRITE (*,'(/,A)')
     &          ' ERROR in FOPENG: existing file cannot be deleted'
               CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
            END IF
         END IF
      END IF
 
*     no errors, open file, !!! MACHINE DEPENDENT !!!
      ILF = ILEN (FFF)
      CALL LOWERC (LFNAME)
 
*     ===================================================
*     ==== Atari, MS-DOS, Unix, Absoft MacFortran II ====
*     ===================================================
      IWL = IRECL
      IF (SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
     &   ACCESS='SEQUENTIAL',FORM=FFF(1:ILF),IOSTAT=IOS)
      IF (.NOT.SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
     &   ACCESS='DIRECT',FORM=FFF(1:ILF),RECL=IWL,IOSTAT=IOS)
 
*     ===================================
*     ======= Fortran on Macintosh ======
*     === Language Systems Fortran 77 ===
*     === Absoft ProFortran 77 and 90 ===
*     ===================================
c      IWL = IRECL
c      IF (FFF.EQ.'FORMATTED') THEN
c*        sequential, formatted files are created as EDIT files
c         CREAT = 'EDIT'
c         FTYPE = 'TEXT'
c      ELSE
c*        some special creator/type combinations depend on extension
c         EXT = ' '
c         IF (ILFIL.GE.4) THEN
c*           get extension
c            IF (LFNAME(ILFIL-3:ILFIL-3).EQ.'.') THEN
c               EXT = LFNAME(ILFIL-2:ILFIL)
c            END IF
c         END IF
c
c         IF (EXT.EQ.'tif') THEN
c*           are created as NIH-Image files
c            CREAT = 'Imag'
c            FTYPE = 'TIFF'
c         ELSE IF (EXT.EQ.'lut') THEN
c*           are created as NIH-Image LUT file (colour table)
c            CREAT = 'Imag'
c            FTYPE = 'ICOL'
c         ELSE IF (EXT.EQ.'pic') THEN
c*           are created as Graphic Converter PICT
c            CREAT = 'GKON'
c            FTYPE = 'PICT'
c         ELSE
c*           otherwise default creator
c            CREAT = 'MPS '
c            FTYPE = 'OBJ '
c         END IF
c      END IF
c
c      IF (SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &   ACCESS='SEQUENTIAL',FORM=FFF(1:ILF),CREATOR=CREAT,
c     &   FILETYPE=FTYPE,IOSTAT=IOS)
c      IF (.NOT.SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &   ACCESS='DIRECT',FORM=FFF(1:ILF),RECL=IWL,CREATOR=CREAT,
c     &   FILETYPE=FTYPE,IOSTAT=IOS)
 
*     =====================================
*     ======= VAX ============= VAX =======
*     =====================================
c      IF (.NOT.SEQ) THEN
c         IF (FFF.EQ.'FORMATTED') THEN
c            IWL = IRECL
c         ELSE IF (FFF.EQ.'UNFORMATTED') THEN
c            IWL = 1 + (IRECL-1)/4
c         END IF
c      END IF
 
c*     normal open
c      IF (SEQ .AND. FFF.EQ.'FORMATTED'.AND.LSTAT.EQ.'NEW') THEN
c*        sequential formatted file with max record length 4096
c         OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &    ACCESS='SEQUENTIAL',FORM='FORMATTED',
c     &    BLOCKSIZE=4096,RECL=4096,IOSTAT=IOS,
c     &    CARRIAGECONTROL='LIST')
c      ELSE IF (SEQ .AND. FFF.EQ.'FORMATTED'.AND.LSTAT.EQ.'OLD') THEN
c*        sequential formatted file with max record length 4096
c         OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &    ACCESS='SEQUENTIAL',FORM='FORMATTED',
c     &    BLOCKSIZE=4096,RECL=4096,IOSTAT=IOS)
c      ELSE IF (SEQ .AND. FFF.EQ.'UNFORMATTED') THEN
c*        sequential unformatted file
c         OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &    ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
c     &    BLOCKSIZE=4096,IOSTAT=IOS)
c      ELSE IF (.NOT.SEQ) THEN
c*        direct access file
c         OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
c     &    ACCESS='DIRECT',FORM=FFF(1:ILF),RECL=IWL,
c     &    BLOCKSIZE=4096,IOSTAT=IOS)
c      END IF
 
      IF (IOS.NE.0) THEN
*        error from system
         WRITE (*,'(/,A)')
     &    ' ERROR in FOPENG: system error while trying to open file'
         CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
 
      RETURN
      END
 
      SUBROUTINE FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
 
*     error routine of fopeng
 
      IMPLICIT NONE
 
*     Formal parameters
      INTEGER IUNIT,IRECL,IOS
      CHARACTER*(*) FILNAM, STATUS, PRIV, TYPE
 
*     Local variables
      INTEGER ILFIL,ILEN
      SAVE
 
*     supply info on arguments
      ILFIL = MAX (ILEN (FILNAM),1)
      WRITE (*,'(/,A,/,A,I5,3(/,2A),/,A,I4,/,2A)')
     & ' Arguments of the CALL to FOPENG leading to this error:',
     & '   Unit             = ',IUNIT,
     & '   File name        =    ',FILNAM(1:ILFIL),
     & '   File status      =    ',STATUS,
     & '   File type        =    ',TYPE,
     & '   Record length    = ',IRECL,
     & '   Delete privilege =    ',PRIV
      IF (IOS.NE.0) THEN
         WRITE (*,'(A,I6,A)')
     & '   IOSTAT           = ',IOS,
     & ' <-- system I/O status code'
      END IF
      CALL FATALERR (' ',' ')
 
      RETURN
      END
**==FOPENS.FOR  
      SUBROUTINE FOPENS (IUNIT,FILNAM,STATUS,PRIV)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IUNIT
      CHARACTER*(*) FILNAM, STATUS, PRIV
 
**    local variables
      INTEGER IRECL
      CHARACTER*2 TYPE
      SAVE
 
*     call FOPENG for a formatted, sequential file
      TYPE  = 'FS'
      IRECL = 0
      CALL FOPENG (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV)
 
      RETURN
      END
**==GETREC.FOR  
      SUBROUTINE GETREC (IUNIT,RECORD,EOF)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IUNIT
      CHARACTER RECORD*(*)
      LOGICAL EOF
 
**    local variables
      INTEGER ISTAT
      LOGICAL STAR
      SAVE
 
*     repeat until non-comment record or EOF is found
      STAR = .TRUE.
      EOF = .FALSE.
10    IF (STAR .AND. .NOT.EOF) THEN
 
         READ (IUNIT,'(A)',IOSTAT=ISTAT) RECORD
         EOF = ISTAT.LT.0 .OR. RECORD(1:1).EQ.CHAR(26)
         STAR = RECORD(1:1).EQ.'*' .OR. RECORD(1:2).EQ.' *'
 
      GOTO 10
      END IF
 
      RETURN
      END
**==GETUN.FOR   
      INTEGER FUNCTION GETUN (IST,IEND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IST,IEND
 
**    Local variables
      INTEGER I1
      LOGICAL OPEN
      SAVE
 
      IF (IST.GT.IEND.OR.
     &    IST.LT.10.OR.IST.GT.99.OR.IEND.LT.10.OR.IEND.GT.99)
     &   CALL FATALERR ('GETUN','unit number outside range')
 
      GETUN = -1
      I1    = IST
      OPEN  = .TRUE.
 
10    IF (OPEN.AND.I1.NE.IEND) THEN
         INQUIRE (UNIT=I1,OPENED=OPEN)
         IF (.NOT.OPEN) GETUN = I1
         I1 = I1+1
      GOTO 10
      END IF
 
      IF (OPEN) CALL FATALERR ('GETUN','cannot find free unit number')
 
      RETURN
      END
**==GETUN2.FOR  
      INTEGER FUNCTION GETUN2 (IST,IEND,NUM)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IST,IEND,NUM
 
**    Local variables
      INTEGER UNIT1,UNIT2
      LOGICAL OPEN_FIRST,FOUND,TMP,OPEN_REST
      SAVE
 
      IF (IST.GT.IEND.OR.
     &    IST.LT.10.OR.IST.GT.99.OR.IEND.LT.10.OR.IEND.GT.99)
     &   CALL FATALERR ('GETUN2','unit number range invalid')
      IF (NUM.LT.1) CALL FATALERR ('GETUN2','invalid value of NUM')
 
      GETUN2 = -1
      UNIT1  = IST
      FOUND  = .FALSE.
 
10    IF (.NOT.FOUND.AND.UNIT1.NE.IEND) THEN
         INQUIRE (UNIT=UNIT1,OPENED=OPEN_FIRST)
C        OPEN_FIRST = TEST (UNIT1)
         IF (.NOT.OPEN_FIRST) THEN
*           first file in requested range is not open, see if
*           rest is open
            IF (NUM.GE.2) THEN
*              search for remaining open units
               OPEN_REST = .FALSE.
               DO UNIT2=UNIT1+1,UNIT1+NUM-1
                  INQUIRE (UNIT=UNIT2,OPENED=TMP)
C                 TMP = TEST (UNIT2)
                  IF (TMP) OPEN_REST = .TRUE.
               END DO
 
               IF (OPEN_REST) THEN
*                 there is a file open in the rest of the range
                  UNIT1 = UNIT1+NUM
               ELSE
*                 no open file found, routine was succesfull
                  FOUND = .TRUE.
               END IF
            ELSE
               FOUND = .TRUE.
            END IF
         ELSE
            UNIT1 = UNIT1+1
         END IF
      GOTO 10
      END IF
 
      IF (.NOT.FOUND) CALL FATALERR
     &   ('GETUN2','cannot find free unit number')
 
      GETUN2 = UNIT1
 
      RETURN
      END
**==IFINDC.FOR  
      INTEGER FUNCTION IFINDC (NAMLIS,ILDEC,IST,IEND,NAME)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC, IST, IEND
      CHARACTER*(*) NAMLIS, NAME
      DIMENSION NAMLIS(ILDEC)
 
**    local variables
      INTEGER IN, IM, IL1, IL2, STEP
      CHARACTER*1 C
      SAVE
 
*     error check
      IL1 = LEN (NAMLIS(1))
      IL2 = LEN (NAME)
      IF (IL1.NE.IL2) CALL FATALERR ('IFINDC','string size mismatch')
      IF (IST.LT.1.OR.IST.GT.ILDEC.OR.
     &    IEND.LT.0.OR.IEND.GT.ILDEC) CALL FATALERR ('IFINDC',
     &   'search outside array bounds')
 
      IF (IEND.EQ.0) THEN
         IFINDC = 0
         RETURN
      END IF
 
      STEP = 1
      IF (IEND.LT.IST) STEP = -1
 
      C  = NAME(1:1)
      IM = 0
 
      DO IN=IST,IEND,STEP
         IF (C.EQ.NAMLIS(IN)(1:1)) THEN
            IF (NAME.EQ.NAMLIS(IN)) THEN
               IM = IN
               GOTO 10
            END IF
         END IF
      END DO
 
10    CONTINUE
      IFINDC = IM
 
      RETURN
      END
**==IFINDI.FOR  
      INTEGER FUNCTION IFINDI (ILIS,ILDEC,IST,IEND,IINP)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILIS,ILDEC,IST,IEND,IINP
      DIMENSION ILIS(ILDEC)
 
**    local variables
      INTEGER IN, IM, STEP
      SAVE
 
*     error check
      IF (IST.LT.1.OR.IST.GT.ILDEC.OR.
     &    IEND.LT.0.OR.IEND.GT.ILDEC) CALL FATALERR ('IFINDI',
     &   'search outside array bounds')
 
      IF (IEND.EQ.0) THEN
         IFINDI = 0
         RETURN
      END IF
 
      STEP = 1
      IF (IEND.LT.IST) STEP = -1
 
      IM = 0
 
      DO IN=IST,IEND,STEP
         IF (IINP.EQ.ILIS(IN)) THEN
            IM = IN
            GOTO 10
         END IF
      END DO
 
10    CONTINUE
      IFINDI = IM
 
      RETURN
      END
**==ILEN.FOR    
      INTEGER FUNCTION ILEN (STRING)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
 
**    local variables
      INTEGER I, L
      SAVE
 
      L = LEN (STRING)
 
      DO 10 I=L,1,-1
         IF (STRING(I:I).NE.' ') THEN
            ILEN = I
            RETURN
         END IF
10    CONTINUE
 
      ILEN = 0
 
      RETURN
      END
**==INSW.FOR    
      REAL FUNCTION INSW (X1,X2,X3)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X1,X2,X3
 
**    local variables
      SAVE
 
      IF (X1.LT.0.) THEN
         INSW = X2
      ELSE
         INSW = X3
      END IF
 
      RETURN
      END
**==INTGRL.FOR  
      REAL FUNCTION INTGRL (STATE, RATE, DELT)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL STATE, RATE, DELT
 
**    local variables
      SAVE
 
      INTGRL = STATE + RATE * DELT
 
      RETURN
      END
 
**==ISTART.FOR  
      INTEGER FUNCTION ISTART (STRING)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
 
**    local variables
      INTEGER I, L
      SAVE
 
      L = LEN (STRING)
 
      DO 10 I=1,L
         IF (STRING(I:I).NE.' ') THEN
            ISTART = I
            RETURN
         END IF
10    CONTINUE
 
      ISTART = 0
 
      RETURN
      END
**==IUNIFL.FOR  
      INTEGER FUNCTION IUNIFL (LOWB,UPPB)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER LOWB, UPPB
 
**    Local variables
      REAL UNIFL, DIFF, RAN
      LOGICAL DRAW
      SAVE
 
      DIFF   = 1./(1.+REAL(UPPB)-REAL(LOWB))
 
      DRAW = .TRUE.
10    IF (DRAW) THEN
         RAN  = UNIFL()
         DRAW = RAN.EQ.1.
      GOTO 10
      END IF
 
      IUNIFL = LOWB+INT(RAN/DIFF)
c      write (*,*) lowb,uppb,iunifl
      RETURN
      END
**==LIMIT.FOR   
      REAL FUNCTION LIMIT (MIN,MAX,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL MIN,MAX,X
 
**    local parameters
      CHARACTER MESSAG*52
      SAVE
 
      IF (MAX.LT.MIN) THEN
*        minimum is larger than maximum, should generate an error
         WRITE (MESSAG, '(2(A,G11.5))')
     &   'argument error, MIN = ',MIN,', MAX = ',MAX
         CALL FATALERR ('LIMIT',MESSAG)
      END IF
 
      IF (X.LT.MIN) THEN
*        X below allowed range ; return lower bound
         LIMIT = MIN
 
      ELSE IF (X.LE.MAX) THEN
*        X in range ; return X
         LIMIT = X
 
      ELSE
*        X above allowed range ; return upper bound
         LIMIT = MAX
 
      END IF
 
      RETURN
      END
**==LINT.FOR    
      REAL FUNCTION LINT (TABLE,ILTAB,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILTAB
      REAL TABLE(ILTAB), X
 
**    local variables
      INTEGER I1, ILT, IUP
      REAL SLOPE, TINY
      PARAMETER (TINY=1.E-7)
      LOGICAL ERR, WARN
      SAVE

*     initialize
      ERR  = .FALSE.
      WARN = .FALSE.

*     check on value of ILTAB
      IF (MOD(ILTAB,2).NE.0 .OR. ILTAB.LE.2) THEN
         WRITE (*,'(A)')
     $    ' Number of elements in LINT table not correct !'
         ERR = .TRUE.
 
      ELSE
         IUP = 0
         DO 10 I1=3,ILTAB,2
*           check on ascending order of X-values in function
            IF (TABLE(I1).LE.TABLE(I1-2)) THEN
               WRITE (*,'(2A,I4)')
     $          ' X-coordinates in LINT table not in',
     $          ' ascending order at point',I1
               ERR = .TRUE.
            END IF
            IF (IUP.EQ.0 .AND. TABLE(I1).GE.X) IUP = I1
10       CONTINUE
      END IF
 
      IF (.NOT.ERR .AND. X.LT.TABLE(1)) THEN
         IUP = 3
         IF ((TABLE(1)-X) .GT. ABS(X)*TINY) THEN
            WARN = .TRUE.
            WRITE (*,'(A,G13.5)')
     $       ' WARNING in LINT: X-value below defined region at X=',X
         END IF
      ELSE IF (.NOT.ERR .AND. X.GT.TABLE(ILTAB-1)) THEN
         IUP = ILTAB-1
         IF ((X-TABLE(ILTAB-1)) .GT. ABS(X)*TINY) THEN
            WARN = .TRUE.
            WRITE (*,'(A,G13.5)')
     $       ' WARNING in LINT: X-value above defined region at X=',X
         END IF
      END IF
 
      IF (WARN.OR.ERR) THEN
         ILT = MIN (ILTAB/2, 15)
         WRITE (*,'(A,I4,/,A,I2,A)')
     $     ' Number of table elements is',ILTAB,
     $     ' First ',ILT,' pairs are:'
         IF (ILT.GT.0) WRITE (*,'(2G13.5)')
     $      (TABLE(I1),TABLE(I1+1),I1=1,2*ILT,2)
         IF (ERR) CALL FATALERR ('LINT','execution terminated')
      END IF
 
*     interpolation and extrapolation
      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      LINT  = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE
 
      RETURN
      END
**==LINT2.FOR   
      REAL FUNCTION LINT2 (TABNAM,TABLE,ILTAB,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) TABNAM
      INTEGER ILTAB
      REAL TABLE(ILTAB), X
 
**    local variables
      INTEGER I1, IUP, IL, ILEN
      REAL SLOPE, TINY
      PARAMETER (TINY=1.E-7)
      LOGICAL ERR
      SAVE
 
*     initialize
      ERR  = .FALSE.
 
*     check on value of ILTAB
      IF (MOD(ILTAB,2).NE.0 .OR. ILTAB.LE.2) THEN
         IL = MAX (1, ILEN (TABNAM))
         WRITE (*,'(2A,/,A)')
     &    ' Number of elements in interpolation table: ',TABNAM(1:IL),
     &    ' not correct !'
         ERR = .TRUE.
 
      ELSE
         IUP = 0
         DO 10 I1=3,ILTAB,2
*           check on ascending order of X-values in function
            IF (TABLE(I1).LE.TABLE(I1-2)) THEN
               IL = MAX (1, ILEN (TABNAM))
               WRITE (*,'(2A,/,A,I4)')
     &          ' X-coordinates in interpolation table: ',TABNAM(1:IL),
     &          ' not in ascending order at point',I1
               ERR = .TRUE.
            END IF
            IF (IUP.EQ.0 .AND. TABLE(I1).GE.X) IUP = I1
10       CONTINUE
      END IF
 
      IF (.NOT.ERR .AND. X.LT.TABLE(1)) THEN
         IUP = 3
         IF ((TABLE(1)-X) .GT. ABS(X)*TINY) THEN
            IL = MAX (1, ILEN (TABNAM))
            WRITE (*,'(A,G13.5,/,2A)')
     &       ' WARNING in LINT2: X-value below defined region at X=',X,
     &       ' in interpolation table: ',TABNAM(1:IL)
         END IF
      ELSE IF (.NOT.ERR .AND. X.GT.TABLE(ILTAB-1)) THEN
         IUP = ILTAB-1
         IF ((X-TABLE(ILTAB-1)) .GT. ABS(X)*TINY) THEN
            IL = MAX (1, ILEN (TABNAM))
            WRITE (*,'(A,G13.5,/,2A)')
     &       ' WARNING in LINT2: X-value above defined region at X=',X,
     &       ' in interpolation table: ',TABNAM(1:IL)
         END IF
      END IF
 
      IF (ERR) CALL FATALERR ('LINT2',' ')
 
*     interpolation and extrapolation
      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      LINT2 = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE
 
      RETURN
      END
**==LOWERC.FOR  
      SUBROUTINE LOWERC (STRING)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
 
**    local variables
      INTEGER I,IC,L
      SAVE
 
      L = LEN (STRING)
      DO 10 I=1,L
*        convert uppercase letters
         IC = ICHAR(STRING(I:I))
         IF (IC.GE.65.AND.IC.LE.90) STRING(I:I) = CHAR(IC+32)
10    CONTINUE
 
      RETURN
      END
**==MOVAVR.FOR  
      SUBROUTINE MOVAVR (ITASK,NAME,IP,IN,OUT)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK,IP
      REAL IN, OUT
      CHARACTER*(*) NAME
 
**    local variables
      INTEGER MNP,MNNAM,NNAM
      PARAMETER (MNP=100,MNNAM=10)
      REAL ARR(MNP,MNNAM)
      INTEGER NPA(MNNAM)
      CHARACTER*31 NAMESA(MNNAM),LNAME
      INTEGER I1,N,IFINDC,NP
      SAVE
 
      IF (ITASK.EQ.1) THEN
 
*        initialize storage arrays
 
         DO 10 I1=1,MNNAM
            NPA(I1)    = 0
            NAMESA(I1) = ' '
10       CONTINUE
         NNAM = 0
      ELSE
 
*        calculate running average
 
*        look up NAME in list of names
         IF (IP.GT.MNP) CALL FATALERR ('MOVAVR',
     &      'maximum number of points exceeded')
         LNAME = NAME
         N = IFINDC (NAMESA,MNNAM,1,NNAM,LNAME)
         IF (N.EQ.0) THEN
*           NAME was not found in list of names, add to list
            NNAM = NNAM+1
            IF (NNAM.GT.MNNAM) CALL FATALERR ('MOVAVR','too many names')
            N = NNAM
            NAMESA(N) = LNAME
         END IF
 
*        shift old values for NAME
         NP = MIN (NPA(N)+1, IP)
 
         DO 20 I1=NP,2,-1
            ARR(I1,N) = ARR(I1-1,N)
20       CONTINUE
 
*        add new value to array
         ARR(1,N) = IN
 
*        calculate running average by summing and dividing
         OUT = 0.
         DO 30 I1=1,NP
            OUT = OUT+ARR(I1,N)
30       CONTINUE
         OUT    = OUT/REAL (NP)
         NPA(N) = NP
      END IF
 
      RETURN
      END
**==NOTNUL.FOR  
      REAL FUNCTION NOTNUL (X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X
 
**    local variables
      SAVE
 
      IF (X.NE.0.) THEN
         NOTNUL = X
      ELSE
         NOTNUL = 1.
      END IF
 
      RETURN
      END
**==OUTAR2.FOR  
      SUBROUTINE OUTAR2 (NAME, ARRAY, LDEC, UDEC, I1, I2)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) NAME
      INTEGER LDEC,UDEC,I1,I2
      REAL ARRAY(LDEC:UDEC)
 
**    local variables
      CHARACTER IND(0:99)*5, TEMP*36
      INTEGER I3, IL, STEP, ILEN
      LOGICAL INIT
      SAVE
 
      DATA INIT /.FALSE./
 
*     initialize character array of subscripts
      IF (.NOT.INIT) THEN
         DO 10 I3=0,99
            IF (I3.GE.0.AND.I3.LE.9)
     &          WRITE (IND(I3),'(A,I1,A)') '(',I3,')'
            IF (I3.GT.9.AND.I3.LE.99)
     &          WRITE (IND(I3),'(A,I2,A)') '(',I3,')'
10       CONTINUE
         INIT = .TRUE.
      END IF
 
      IF (I1.LT.LDEC.OR.I1.GT.UDEC.OR.
     &    I2.LT.LDEC.OR.I2.GT.UDEC) THEN
         WRITE (*,*) 'ERROR in OUTAR2: ',
     &               'output range of ',NAME,' outside declared size'
         CALL FATALERR (' ',' ')
      END IF
 
      STEP = 1
      IF (I2.LT.I1) STEP = -1
 
      IL = ILEN (NAME)
 
*     dump names and values from start element to finish element
 
      DO 20 I3=I1,I2,STEP
 
         IF (I3.GE.0.AND.I3.LE.9) THEN
*           index was created in local character array,
*           concatenation with name
            TEMP = NAME(1:IL)//IND(I3)
         ELSE IF (I3.GE.10.AND.I3.LE.99) THEN
*           index was created in local character array,
*           concatenation with name
            TEMP = NAME(1:IL)//IND(I3)
         ELSE
*           index was not created in local character array,
*           index should be prepared
            IF (I3.GE.100.AND.I3.LE.999) THEN
               WRITE (TEMP,'(2A,I3,A)') NAME(1:IL),'(',I3,')'
            ELSE IF (I3.GE.-99.AND.I3.LE.-10) THEN
               WRITE (TEMP,'(2A,I3,A)') NAME(1:IL),'(',I3,')'
            ELSE IF (I3.GE.-9.AND.I3.LE.-1) THEN
               WRITE (TEMP,'(2A,I2,A)') NAME(1:IL),'(',I3,')'
            ELSE
               CALL FATALERR ('OUTAR2', 'subscript out of range')
            END IF
         END IF
 
         IF (I1.NE.I2) THEN
*           range of array is wanted (not a single element)
*           add symbol to make clear begin and end of array
            IF (TEMP(36:36).NE.' ') CALL FATALERR
     &         ('OUTAR2','string too long')
            IF (I3.EQ.I1) THEN
*              element is beginning of array
               TEMP(36:36) = 'B'
            ELSE IF (I3.EQ.I2) THEN
*              element is end of array
               TEMP(36:36) = 'E'
            ELSE
*              element is somewhere in array
               TEMP(36:36) = '.'
            END IF
         END IF
 
*        send array element to OUTDAT
         CALL OUTDAT (2, 0, TEMP, ARRAY(I3))
 
20    CONTINUE
 
      RETURN
      END
**==OUTCOM.FOR  
      SUBROUTINE OUTCOM (STR)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STR
 
**    local variables and used functions
      INTEGER ICOM, NCOM, I1, IL, ILU2, IFINDC, ILEN
      PARAMETER (NCOM=25)
      CHARACTER*80 COMMNT(NCOM), TMP
      LOGICAL OPEN
      SAVE
 
      DATA ICOM /0/, ILU2 /0/
 
      IF (STR.EQ.'<INIT$$$>') THEN
         ICOM = 0
      ELSE IF (STR.EQ.'<PRINT$$$>') THEN
         IF (ICOM.GT.0) THEN
            CALL AMBUSY (2,'OUTDAT',ILU2)
            IF (ILU2.EQ.0) CALL FATALERR ('OUTCOM',
     &         'No unit number for output file')
            INQUIRE (UNIT=ILU2, OPENED=OPEN)
            IF (.NOT.OPEN) CALL FATALERR
     &         ('OUTCOM','Output file not open')
            DO 10 I1=1,ICOM
               IL = ILEN (COMMNT(I1))
               WRITE (ILU2,'(2A)') '* ',COMMNT(I1)(1:IL)
10          CONTINUE
         END IF
      ELSE
         IF (ICOM.LT.NCOM) THEN
            IL  = ILEN (STR)
            TMP = STR
            I1  = IFINDC (COMMNT, NCOM, 1, ICOM, TMP)
            IF (IL.GT.0 .AND. I1.EQ.0) THEN
               ICOM = ICOM+1
               COMMNT(ICOM) = STR
            END IF
         ELSE
            CALL FATALERR ('OUTCOM','Too many comment lines')
         END IF
      END IF
 
      RETURN
      END
**==OUTDAT.FOR  
      SUBROUTINE OUTDAT (ITASK, IUNIT, RN, R)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK, IUNIT
      REAL R
      CHARACTER*(*) RN
 
**    local variables
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     A D J U S T A B L E   P A R A M E T E R S                       *
*     =========================================                       *
*     IMNC1  = maximum number of columns of dependent variables in an *
*              output block, if increased, also increase LINE*x, x    *
*              should be at least 14+IMNC1*13,                        *
*     NAMES_MXN = maximum number of names of dependent variables,     *
*                 can be increased without problems,                  *
*     VARL_M = maximum length of a variable, remainder is removed     *
*     Warning: do not change the maximum length of names of variables,*
*              currently set to 11 !!                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
*     AVN    - Array of variable names
*     IAVNL  - Array of lengths of AVN
*     AVV    - Array with values of AVN
*     ASELN  - Array with selected variable names
*     SEL    - Array with flags whether variables from AVN should be
*              printed
*     FND    - Array with counts how many times a variable is seen at
*              itask=2
*     MGIVEN - Array of flags whether messages about repeated input
*              of a variable is given
*     BLK    - Assigned block number, 0 if not assigned
*     COL_WIDTH_MNN
*            - maximum width of a Y column, excluding spaces
*     SEQ    - SEQ(1) = element of AVN that is first in
*                       current block,
*              SEQ(2) = element of AVN that is second in
*                       current block etc.
*     EXTRA_SP - Extra width of columns
*     EXTRA_SPX- Extra width for X columns
*     NAMES_IB_MXN_4 - Maximum number of names in block in itask=4
*                      (determined by paper width)
*     NAMES_IB_MXN_5 - Maximum number of names in block in itask=5
*     NAMES_IB_MXN_7 - Maximum number of names in block in itask=7
*     NAMES_IB_MXN_8 - Maximum number of names in block in itask=8
*     NAMES_IB_MXN_9 - Maximum number of names in block in itask=9
 
*     SEQ2     - Array with numbers that link to selected arrays
 
*     Maximum length of a variable name is 36 characters (31 for name,
*     5 for possible index e.g. '(134)')
 
      INTEGER NAMES_MXN, VARL_M
      INTEGER NAMES_IB_MXN_4,NAMES_IB_MXN_5
      INTEGER NAMES_IB_MXN_7,NAMES_IB_MXN_8,NAMES_IB_MXN_9
      PARAMETER (NAMES_IB_MXN_4=8)
      PARAMETER (NAMES_IB_MXN_5=100)
      PARAMETER (NAMES_IB_MXN_7=8)
      PARAMETER (NAMES_IB_MXN_8=NAMES_IB_MXN_5)
      PARAMETER (NAMES_IB_MXN_9=NAMES_IB_MXN_5)
      PARAMETER (NAMES_MXN=500,VARL_M=36)
      CHARACTER*(VARL_M) AVN(NAMES_MXN)
      INTEGER            IAVNL(NAMES_MXN)
      CHARACTER*(VARL_M) ASELN(NAMES_MXN)
      INTEGER            BLK(NAMES_MXN)
      LOGICAL            MGIVEN(NAMES_MXN)
      INTEGER            SEQ2(NAMES_MXN)
      INTEGER            FND(NAMES_MXN)
      CHARACTER*(VARL_M) LXN, LN
      REAL               AVV(NAMES_IB_MXN_5)
      LOGICAL            FNDA(NAMES_IB_MXN_5)
      INTEGER            SEQ(NAMES_IB_MXN_5)
      INTEGER            EXTRA_SP(NAMES_IB_MXN_5),EXTRA_SPX
 
      INTEGER COL_WIDTH_MNN,LINE_LEN,CENTRE
      INTEGER LINE_LEN_4
      INTEGER LINE_LEN_5
      INTEGER LINE_LEN_7
      INTEGER LINE_LEN_8
      INTEGER LINE_LEN_9
 
      PARAMETER (COL_WIDTH_MNN=12,CENTRE=8)
      PARAMETER (LINE_LEN_4=
     &    COL_WIDTH_MNN+2+NAMES_IB_MXN_4*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_5=
     &    COL_WIDTH_MNN+2+NAMES_IB_MXN_5*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_7=
     &    COL_WIDTH_MNN+2+NAMES_IB_MXN_7*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_8=LINE_LEN_5)
      PARAMETER (LINE_LEN_9=LINE_LEN_5)
 
*     declaration of longest line, others should fit into this one
      CHARACTER*(LINE_LEN_5) LINE
 
*     number of different variable names that have been found in a
*     particular run
      INTEGER IFND
 
*     IFND2 is the TRUE number of variables to be printed in a
*     particular block
      INTEGER IFND2
 
*     flag whether block is full with columns
      LOGICAL FULL_BLOCK
 
*     Length of LINE
      INTEGER LINE_L
 
*     Flag whether extra spaces between columns are necessary
      LOGICAL EXTRA_F
 
**    other local parameters
      INTEGER ILU1, ILU2, ICHECK, IOS, INSEL, ISEL, ISAVE
      INTEGER IRUN1, IRUN2, IRUN3, IRUN4, IRUN5, ILEN
      INTEGER ITOLD, ILTASK, INAME, IBLOK, ILXN, IR
      INTEGER ISREC, IREC, IEREC, IB, I1, I2, I3, I4, I5, I6, IFINDC
      REAL LV, LVO
      CHARACTER CHR*1, RUNTYP*1, RUNDUM*1, TEXT(4:9)*18, COMMCHR*1
      LOGICAL OPEND, OK, YFND, RECOVR, SELECTED, FIRST8, FIRST9
      CHARACTER*80 SPACE
 
*     uncomment for mac absoft fortran compilation
*      CHARACTER EOFCHR*1
 
      SAVE
 
*     uncomment for mac absoft fortran compilation
*      DATA EOFCHR /'Z'/
 
      DATA TEXT /'Table output','Spreadsheet output',
     &           '2 column output','ICASA output',
     &           'End of run output','Greenery output'/
 
      DATA ITOLD /-1/, IRUN1 /0/, IRUN2 /0/, INSEL /0/, RECOVR /.FALSE./
      DATA FIRST8 /.TRUE./
      DATA FIRST9 /.TRUE./
 
      IF (ITASK.EQ.1) THEN
 
*        unit number and status check:
*        =============================
*        unit number must be > 0 at first call, may be zero or equal
*        to value at first call
 
         IF (ITOLD.EQ.-1) THEN
 
            IF (IUNIT.EQ.0) CALL FATALERR
     &         ('OUTDAT','no unit number supplied')
            ILU2 = IUNIT

         ELSE
 
            IF (IUNIT.NE.0.AND.IUNIT.NE.ILU2) CALL FATALERR
     &         ('OUTDAT','change of unit number not allowed')
 
            IF (ITOLD.EQ.1) THEN
 
*              repeated initialization is taking place
 
               WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &           ' ignoring repeated initialization'
               WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &           ' ignoring repeated initialization'
               RETURN
 
            ELSE IF (ITOLD.EQ.2) THEN
 
               CONTINUE
 
            ELSE IF (ITOLD.EQ.3) THEN
 
*              during a previous call, one or more variables were
*              selected, this selection is discarded after
*              initialization
 
               WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &           ' selected variables discarded'
               WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &           ' selected variables discarded'
 
            ELSE IF (ITOLD.GE.4) THEN
 
               IF (RECOVR) CALL FATALERR ('OUTDAT',
     &            'normal OUTDAT call impossible after recovery')
 
            END IF
         END IF
 
*        make unit number for temporary file local and make unit number
*        available also for OUTPLT
         ILU1 = ILU2+1
         CALL AMBUSY (1,'OUTDAT',ILU2)
 
*        open file check:
*        see if units ILU2 and ILU1 are open. if not open here
 
         INQUIRE (UNIT=ILU2, OPENED=OPEND)
         IF (.NOT.OPEND) CALL FOPENG (ILU2,'RES.DAT','NEW','SF',0,'DEL')
 
*        see if unit ILU1 is open, if open use it, if not, open using
*        default output file name
         INQUIRE (UNIT=ILU1, OPENED=OPEND)
         IF (.NOT.OPEND) THEN
*           for normal compilers
            CALL FOPENG (ILU1,'RES.BIN','NEW','UD',48,'DEL')
 
*           reset number of runs in RES.BIN file, first record to
*           write to and start record of set
            IRUN4 = 0
            IREC  = 1
            ISREC = 0
         ELSE
*           temporary file is open
            IF (ITOLD.EQ.-1) CALL FATALERR ('OUTDAT',
     &      'temporary file may not be opened outside OUTDAT')
         END IF
 
*        initialize routine that writes comment lines to output file
         CALL OUTCOM ('<INIT$$$>')
 
*        reset arrays with names
         DO I1=1,NAMES_MXN
            AVN(I1)    = ' '
            FND(I1)    = 0
            MGIVEN(I1) = .FALSE.
         END DO
 
*        find out if initialization if generated by reruns
         CALL AMBUSY (2,'RDFROM',IRUN2)
         IF (IRUN2.EQ.0) THEN
*           run number not obtained from RDFROM or first run
            IRUN3  = IRUN1
            RUNTYP = 'N'
         ELSE
*           run number obtained from RDFROM
            IRUN3  = IRUN2
            RUNTYP = 'R'
         END IF
 
*        increase run number and number of runs in file
         IRUN1 = IRUN1+1
         IRUN4 = IRUN4+1
 
*        write total number of runs in RES.BIN file to RES.BIN file
         WRITE (ILU1,REC=1)
     &     '-',IRUN4,'....................................',0.
 
*        update pointer record from previous set only if it is not
*        the first initialization to the same RES.BIN file
 
         IF (IRUN4.GE.2) WRITE (ILU1,REC=ISREC)
     &      ' ',IREC,'....................................',0.
         IREC  = IREC+1
         ISREC = IREC
 
*        check on length of name
         IF (LEN (RN).LE.VARL_M) THEN
*           length of name is ok
            LXN = RN
         ELSE
*           length of name is not ok
            I1 = ILEN (RN)
            WRITE (*,'(2A,/,2A)')
     &        ' WARNING from OUTDAT: variable ',RN(1:I1),
     &        ' is too long for OUTDAT, it is truncated to: ',
     &          RN(1:VARL_M)
            WRITE (ILU2,'(2A,/,2A)')
     &        ' WARNING from OUTDAT: variable ',RN(1:I1),
     &        ' is too long for OUTDAT, it is truncated to: ',
     &          RN(1:VARL_M)
            LXN = RN(1:VARL_M)
         END IF
 
         CALL UPPERC (LXN)
         ILXN  = ILEN (LXN)
 
         WRITE (ILU1, REC=IREC)
     &     ' ',-99,'....................................',0.
         IREC  = IREC+1
         WRITE (ILU1, REC=IREC) RUNTYP,IRUN3,LXN,R
 
*        uncomment for mac absoft fortran compilation
*         WRITE (ILU1, REC=IREC+1)
*     &     EOFCHR,0,'....................................',0.
 
         IFND  = 0
         INSEL = 0
         ISAVE = 0
         SPACE = ' '
 
      ELSE IF (ITASK.EQ.2) THEN
 
*        dump variable to file
 
*        check status first
         IF (ITOLD.GE.3) CALL FATALERR
     &          ('OUTDAT','Initialization not done')
 
*        check on length of name
         IF (LEN (RN).LE.VARL_M) THEN
*           length of name is ok
            LN = RN
         ELSE
*           length of name is not ok
            I1 = ILEN (RN)
            WRITE (*,'(2A,/,2A)')
     &        ' WARNING from OUTDAT: variable ',RN(1:I1),
     &        ' is too long for OUTDAT, it is truncated to: ',
     &          RN(1:VARL_M)
            WRITE (ILU2,'(2A,/,2A)')
     &        ' WARNING from OUTDAT: variable ',RN(1:I1),
     &        ' is too long for OUTDAT, it is truncated to: ',
     &          RN(1:VARL_M)
            LN = RN(1:VARL_M)
         END IF
 
*        values and names are written to file
 
         CALL UPPERC (LN)
 
         OK    = .TRUE.
         INAME = 0
 
         IF (LN.EQ.LXN) THEN
            DO I1=1,IFND
               FND(I1) = 0
            END DO
            ISAVE = 0
         ELSE
*           variable is not the independent variable, look in list
 
*           increase new try pointer value, if at end of list reset
            I1 = ISAVE+1
            IF (I1.GT.IFND) I1 = 1
 
            IF (I1.GT.ISAVE) THEN
*              search to end of list from position of previous call
30             IF (I1.LE.IFND.AND.INAME.EQ.0) THEN
                  IF (LN.EQ.AVN(I1)) INAME = I1
                  I1 = I1+1
               GOTO 30
               END IF
            END IF
 
            IF (INAME.EQ.0) THEN
*              match not found to end of list, try beginning
               I1 = 1
35             IF (I1.LE.ISAVE.AND.INAME.EQ.0) THEN
                  IF (LN.EQ.AVN(I1)) INAME = I1
                  I1 = I1+1
               GOTO 35
               END IF
            END IF
 
            IF (INAME.EQ.0) THEN
*              name not found in list, add to list
               IFND = IFND+1
               IF (IFND.GT.NAMES_MXN) CALL FATALERR
     &            ('OUTDAT','too many variables for output')
               AVN(IFND)  = LN
               INAME      = IFND
               FND(INAME) = 1
            ELSE
*              name found in list
               FND(INAME) = FND(INAME)+1
               IF (FND(INAME).EQ.2) THEN
*                 variable supplied twice, prevent writing to file
                  OK = .FALSE.
                  IF (.NOT.MGIVEN(INAME)) THEN
                     MGIVEN(INAME) = .TRUE.
*                    give warning the second time a variable is entered
*                    do not give warnings on third, fourth etc.
                     I1 = ILEN (LN)
                     WRITE (*,'(4A)')
     &                 ' WARNING from OUTDAT: variable ',LN(1:I1),
     &                 ' supplied without new independent ',
     &                   LXN(1:ILXN)
                     WRITE (ILU2,'(4A)')
     &                 ' WARNING from OUTDAT: variable ',LN(1:I1),
     &                 ' supplied without new independent ',
     &                   LXN(1:ILXN)
                  END IF
               END IF
            END IF
         END IF
 
         IF (OK) THEN
            IREC = IREC+1
 
            WRITE (ILU1,REC=IREC) ' ',INAME, LN, R
 
*           uncomment for mac absoft fortran compilation
*            WRITE (ILU1,REC=IREC+1) EOFCHR,INAME, LN, R
         END IF
 
         ISAVE = INAME
 
      ELSE IF (ITASK.EQ.3) THEN
 
*        ---------------------------------------
*        selection of output variables for table
*        ---------------------------------------
 
         IF (ITOLD.EQ.1.OR.ITOLD.EQ.99) THEN
            WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &        ' wrong task preceeds selection of output'
            WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &        ' wrong task preceeds selection of output'
            RETURN
         ELSE IF (ITOLD.EQ.2.OR.(ITOLD.GE.4.AND.ITOLD.LE.16)) THEN
*           first time
            INSEL = 0
         END IF
 
         LN = RN
         CALL UPPERC (LN)
 
*        lookup name in list
         IF (INSEL.EQ.0) THEN
            I1 = 0
         ELSE
            I1 = IFINDC (ASELN,INSEL,1,INSEL,LN)
         END IF
 
         IF (I1.EQ.0) THEN
*           name not found in list, add
            INSEL = INSEL+1
            IF (INSEL.GT.NAMES_MXN) CALL FATALERR
     &         ('OUTDAT','too many variables selected')
            ASELN(INSEL) = LN
         END IF
 
      ELSE IF ((ITASK.GE.4 .AND.ITASK.LE.9).OR.
     &         (ITASK.GE.14.AND.ITASK.LE.19)) THEN
 
*        --------------------------
*        writing of output table(s)
*        --------------------------
 
*        error checks on old task
         IF (ITOLD.EQ.-1) THEN
*           recovery of output file, open RES.BIN file
            RECOVR = .TRUE.
            IF (IUNIT.LE.10) CALL FATALERR
     &         ('OUTDAT','illegal unit number')
            ILU2 = IUNIT
            ILU1 = ILU2+1
            CALL FOPENG (ILU1,'RES.BIN','OLD','UD',48,' ')
 
*           check if output file is open, make new one if it is not
 
            INQUIRE (UNIT=ILU2, OPENED=OPEND)
            IF (.NOT.OPEND) CALL FOPENG
     &         (ILU2,'RECOVER.DAT','NEW','SF',0,'DEL')
 
*           find out if end record of last set is in the file
            READ (ILU1,REC=1) RUNTYP,IRUN5
            IF (RUNTYP.EQ.'-') THEN
*              write end record of last set in the file
 
*              go to start of last set first
               IEREC = 1
               DO I1=1,IRUN5-1
                  ISREC = IEREC+1
                  READ (ILU1,REC=ISREC) RUNDUM,IEREC
               END DO
 
*              read rest until end_of_file
               IOS = 0
               IR = IEREC+2
45             IF (IOS.EQ.0) THEN
                  READ (ILU1,REC=IR,IOSTAT=IOS) RUNDUM
 
*                 uncomment for mac absoft fortran compilation
*                  IF (RUNDUM.EQ.EOFCHR) IOS = -1
 
                  IF (IOS.EQ.0) IR = IR+1
 
               GOTO 45
               END IF
               WRITE (ILU1, REC=IEREC+1)
     &           ' ',IR-1 ,'....................................',0.
               WRITE (ILU1, REC=1)
     &           '+',IRUN5,'....................................',0.
            END IF
 
         ELSE IF (ITOLD.EQ.ITASK) THEN
 
            WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &        ' new output and previous output have same format'
            WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &        ' new output and previous output have same format'
 
         ELSE IF (ITOLD.EQ.99) THEN
 
            WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &        ' output table cannot be created, no RES.BIN file'
            WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &        ' output table cannot be created, no RES.BIN file'
            RETURN
 
         ELSE IF (ITOLD.EQ.3.OR.ITOLD.EQ.2.OR.ITOLD.EQ.1) THEN
 
*           previous call was with itask=2, or itask=1
            IEREC = IREC
            WRITE (ILU1, REC=ISREC)
     &        ' ',IREC ,'....................................',0.
            READ  (ILU1, REC=1)     RUNDUM,IRUN5
            WRITE (ILU1, REC=1)
     &        '+',IRUN5,'....................................',0.
 
         END IF
 
         ILTASK = MOD (ITASK,10)
 
         IF (ITASK.GE.14) THEN
*           whole RES.BIN file should be converted
            I6 = 1
         ELSE
*           only last set in RES.BIN file should be converted
            I6 = IRUN5
         END IF
 
         DO I5=I6,IRUN5
 
         IF (ITASK.GE.14.OR.ITOLD.EQ.-1) THEN
*           read names from file into AVN array of the set which is
*           converted into an output table. This is not necessary if
*           only the last set has to be converted and this last set
*           was just finished with an itask=2 call (name of independent
*           variable and array AVN are already known !!!).
*           search for first record of set first
            IEREC = 1
            DO I1=1,I5
               ISREC = IEREC+1
               READ (ILU1,REC=ISREC) RUNDUM,IEREC
            END DO
 
*           read names and determine total number of names
*           read name of independent variable
*           determine length of name of independent variable
 
            READ (ILU1,REC=ISREC+1) RUNTYP, IRUN3, LXN
 
            ILXN  = ILEN (LXN)
            IFND  = 0
 
            DO IR=ISREC+2,IEREC
               READ (ILU1,REC=IR) RUNDUM, INAME, LN
               IFND = MAX (INAME, IFND)
               IF (INAME.GT.0) AVN(INAME) = LN
            END DO
         END IF
 
*        assign each selected variable a block number, this is based
*        on the length of the name of the selected variable, when
*        a new name does not fit anymore on the record, a new block is
*        generated
         FULL_BLOCK = .TRUE.
         IBLOK      = 0
         I2         = 1+MAX (COL_WIDTH_MNN+1,ILXN)
 
         IF (ILTASK.EQ.4) THEN
            LINE_LEN = LINE_LEN_4
         ELSE IF (ILTASK.EQ.5) THEN
            LINE_LEN = LINE_LEN_5
         ELSE IF (ILTASK.EQ.6) THEN
            LINE_LEN = LINE_LEN_4
         ELSE IF (ILTASK.EQ.7) THEN
            LINE_LEN = LINE_LEN_7
         ELSE IF (ILTASK.EQ.8) THEN
            LINE_LEN = LINE_LEN_8
         ELSE IF (ILTASK.EQ.9) THEN
            LINE_LEN = LINE_LEN_9
         END IF
 
*        assign block numbers to variables
         DO I1=1,IFND
 
            IF (INSEL.EQ.0) THEN
*              no variables were selected, select each one
               SELECTED = .TRUE.
            ELSE
               ISEL = IFINDC (ASELN,INSEL,1,INSEL,AVN(I1))
               IF (ISEL.GT.0) THEN
*                 variable found in list of selected variables
                  SELECTED = .TRUE.
               ELSE
*                 variable not found in list of selected variables
                  SELECTED = .FALSE.
               END IF
            END IF
 
            IF (SELECTED) THEN
 
               IF (FULL_BLOCK) THEN
*                 previous block full
                  IBLOK      = IBLOK+1
                  FULL_BLOCK = .FALSE.
               END IF
 
*              variable selected
               IAVNL(I1) = ILEN (AVN(I1)(1:35))
               IF (IAVNL(I1).EQ.0) THEN
                  WRITE (*,'(A)')
     &              ' WARNING from OUTDAT: zero length variable name'
                  WRITE (ILU2,'(A)')
     &              ' WARNING from OUTDAT: zero length variable name'
                  IAVNL(I1) = 1
               END IF
 
               IF (ILTASK.EQ.6) THEN
                  FULL_BLOCK   = .TRUE.
               ELSE
                  I3 = 1+MAX (COL_WIDTH_MNN,IAVNL(I1))
                  IF (I2+I3.GT.LINE_LEN) THEN
*                    new variable exceeds output file record,
*                    increase block number etc.
                     IBLOK = IBLOK+1
                     I2    = 1+MAX (COL_WIDTH_MNN+1,ILXN)
                  END IF
                  I2 = I2+I3
                  FULL_BLOCK = .FALSE.
               END IF
               BLK(I1) = IBLOK
            ELSE
               BLK(I1) = 0
            END IF
         END DO
 
*        check number of found blocks
         IF (IBLOK.EQ.0) THEN
            WRITE (*,'(A)')
     &        ' WARNING from OUTDAT: no output values given'
            WRITE (ILU2,'(/,A)')
     &        ' WARNING from OUTDAT: no output values given'
         ELSE IF (IBLOK.GT.1.AND.ILTASK.EQ.8) THEN
            CALL FATALERR ('OUTDAT',
     &      'more than one block with end of run option')
         END IF
 
*        write comment header stuff
*        --------------------------
 
         IF (ILTASK.EQ.4.OR.ILTASK.EQ.5.OR.ILTASK.EQ.7) THEN
 
*           display comment header for normal, tab-delimited output
*           and ICASA output
 
            COMMCHR = '*'
            IF (ILTASK.EQ.7) COMMCHR = '!'
 
*           write line to mark start of new run
            WRITE (ILU2,'(A,76A1)') COMMCHR,('-',I1=1,76)
 
*           write header to output file and possible extra
*           comment lines
 
            IF (IRUN3.EQ.0) THEN
               WRITE (ILU2,'(2A)')
     &        COMMCHR,' Output table number  :  0 (=first output table)'
            ELSE IF (RUNTYP.EQ.'R') THEN
               WRITE (ILU2,'(2A,I5)')
     &         COMMCHR,' Output from rerun set:',IRUN3
            ELSE IF (RUNTYP.EQ.'N') THEN
               WRITE (ILU2,'(2A,I5)')
     &         COMMCHR,' Output table number  :',IRUN3
            ELSE
               CALL FATALERR ('OUTDAT','unknown run type')
            END IF
 
            WRITE (ILU2,'(3A,/,3A)')
     &        COMMCHR,' Output table format  : ',TEXT(ILTASK),
     &        COMMCHR,' ',RN
 
            IF (ILTASK.NE.7) THEN
*              instruct OUTCOM to write comment lines to output file
               CALL OUTCOM ('<PRINT$$$>')
            END IF
 
*           do not write OUTCOM lines with other output formats,
*           because they will be written with an asterisk instead
*           of with an exclamation mark
 
         ELSE IF (ILTASK.EQ.8) THEN
 
*           write comment header for end of run output only once
            IF (FIRST8) THEN
*              write line to mark start of run
               WRITE (ILU2,'(A,76A1)') '*',('-',I1=1,76)
 
               WRITE (ILU2,'(2A)')
     &         '* Output table format  : ',TEXT(ILTASK)
*              instruct OUTCOM to write comment lines to output file
               CALL OUTCOM ('<PRINT$$$>')
            END IF
         ELSE IF (ILTASK.EQ.9) THEN
*           do not write a comment header for Greenery output
            CONTINUE
         END IF
 
         DO IB=1,IBLOK
 
*           search stuff for block number IB
            IFND2 = 0
            DO I1=1,IFND
               IF (BLK(I1).EQ.IB) THEN
*                 variable is in current block, add to list
*                 put pointer in long list
                  IFND2      = IFND2+1
                  SEQ(IFND2) = I1
                  SEQ2(I1)   = IFND2
               END IF
            END DO
 
*           header with variable names is written dependent on ITASK
            IF (ILTASK.EQ.4.OR.ILTASK.EQ.7) THEN
 
               WRITE (ILU2,'(A)') ' '
               CHR = ' '
 
*              write name of independent variable
*              ----------------------------------
 
*              initialize and add leading space
               LINE    = ' '
               LINE_L  = 1
 
               IF (ILTASK.EQ.4) THEN
*                 normal table format
                  IF (ILXN.LE.CENTRE+1) THEN
*                    variable name fits left of centre point
                     I3 = LINE_L+1+CENTRE+1-ILXN
                     I4 = LINE_L+CENTRE+1
                     LINE(I3:I4) = LXN(1:ILXN)
                     LINE_L = LINE_L+COL_WIDTH_MNN+1
                     EXTRA_SPX = 0
                  ELSE
*                    variable name extends beyond the centre point
                     LINE(LINE_L+1:LINE_L+ILXN) = LXN(1:ILXN)
                     EXTRA_SPX = MAX (ILXN-COL_WIDTH_MNN+1,0)
                     LINE_L = LINE_L+COL_WIDTH_MNN+1+EXTRA_SPX
                  END IF
               ELSE IF (ILTASK.EQ.7) THEN
*                 ICASA format
                  LINE   = '@'
                  LINE_L = 1
                  CALL ADDSTR (LINE,LINE_L,LXN)
                  CALL ADDSTR (LINE,LINE_L,'>')
                  LINE_L    = 14
                  EXTRA_SPX = 0
               END IF
 
*              set flag whether extra space is required while writing
               EXTRA_F = EXTRA_SPX.GT.0
 
*              write names of dependent variables centered above column
               DO I1=1,IFND2
                  I2 = SEQ(I1)
 
*                 add separating space between variable names
                  LINE_L = LINE_L+1
 
*                 variable should appear in this block
                  IF (IAVNL(I2).LE.CENTRE) THEN
*                    variable name fits to the left of centre point
                     I3 = LINE_L+1+CENTRE-IAVNL(I2)
                     I4 = LINE_L+CENTRE
 
                     LINE(I3:I4) = AVN(I2)(1:IAVNL(I2))
                     LINE_L = LINE_L+COL_WIDTH_MNN
                     EXTRA_SP(I1) = 0
                  ELSE
*                    variable name extends beyond centre point
                     LINE(LINE_L+1:LINE_L+IAVNL(I2)) =
     &                  AVN(I2)(1:IAVNL(I2))
                     EXTRA_SP(I1) = MAX (IAVNL(I2)-COL_WIDTH_MNN,0)
                     LINE_L = LINE_L+COL_WIDTH_MNN+EXTRA_SP(I1)
                  END IF
 
                  IF (EXTRA_SP(I1).GT.0) EXTRA_F = .TRUE.
 
               END DO
 
*              write line to file
               WRITE (ILU2,'(A)') LINE(1:LINE_L)
               WRITE (ILU2,'(A)') ' '
 
*              add a single space to all columns
               EXTRA_SPX = EXTRA_SPX+1
               DO I1=1,IFND2
                  EXTRA_SP(I1) = EXTRA_SP(I1)+1
               END DO
 
            ELSE IF (ILTASK.EQ.5.OR.(ILTASK.EQ.8.AND.FIRST8)) THEN
 
*              write tab delimited variable header
               CHR = CHAR (9)
 
*              write tabs between the names, allow no spaces !!
               LINE   = ' '
               LINE_L = 0
               CALL ADDSTR (LINE,LINE_L,LXN)
               DO I1=1,IFND2
                  CALL ADDSTR (LINE,LINE_L,CHR)
                  CALL ADDSTR (LINE,LINE_L,
     &                         AVN(SEQ(I1))(1:IAVNL(SEQ(I1))))
               END DO
 
               EXTRA_F = .FALSE.
 
*              write line to file
               WRITE (ILU2,'(A)') ' '
               WRITE (ILU2,'(A)') LINE(1:LINE_L)
               WRITE (ILU2,'(A)') ' '
 
               IF (ILTASK.EQ.8.AND.FIRST8) FIRST8 = .FALSE.
 
            ELSE IF (ILTASK.EQ.6) THEN
 
*              two column output
 
               CHR = ' '
               WRITE (ILU2,'(4A,/,A,/,1X,4A)')
     &           '* ',LXN(1:ILXN),CHR,AVN(SEQ(1))(1:IAVNL(SEQ(1))),
     &           ' 1 1 1',
     &           AVN(SEQ(1))(1:IAVNL(SEQ(1))),'(',LXN(1:ILXN),')'
 
               EXTRA_F = .FALSE.
 
            ELSE IF (ILTASK.EQ.9.AND.FIRST9) THEN
 
*              write tab delimited variable header
               CHR = ','
 
*              write tabs between the names, allow no spaces !!
               LINE   = ' '
               LINE_L = 0
               CALL ADDSTR (LINE,LINE_L,LXN)
               DO I1=1,IFND2
                  CALL ADDSTR (LINE,LINE_L,CHR)
                  CALL ADDSTR (LINE,LINE_L,
     &                         AVN(SEQ(I1))(1:IAVNL(SEQ(I1))))
               END DO
 
               EXTRA_F = .FALSE.
 
*              write line to file
               WRITE (ILU2,'(A)') LINE(1:LINE_L)
 
               IF (ILTASK.EQ.9.AND.FIRST9) FIRST9 = .FALSE.
 
            END IF
 
*           initialize output
            YFND = .FALSE.
 
*           only end of run values, search for last output set
*           (this part was taken from crettp routine of ttselect)
 
            IF (ILTASK.EQ.8) THEN
               LN = ' '
               IR = IEREC
26             IF (LN.NE.LXN.AND.IREC.GT.ISREC) THEN
                  IR = IR-1
                  READ (ILU1,REC=IR) RUNDUM, INAME, LN
               GOTO 26
               END IF
               ISREC = IR-1
            END IF
 
            DO IR=ISREC+1,IEREC
*              read next record
               READ (ILU1,REC=IR) RUNDUM, INAME, LN, LV
*              see if variable name is the independent variable
               IF (LN.EQ.LXN) THEN
                  IF (YFND) THEN
                     IF (ICHECK.EQ.IFND2) THEN
*                       no missing variables, write line
*                       directly to file
                        IF (ILTASK.GE.4.AND.ILTASK.LE.8) THEN
                           IF (.NOT.EXTRA_F) THEN
*                             no extra spaces required,
*                             no missing variables
                              WRITE (ILU2,'(1X,1P,G13.6,255(A,G12.5))')
     &                               LVO,(CHR,AVV(I1),I1=1,IFND2)
                           ELSE
*                             extra spaces required
                              WRITE (ILU2,'(A,1P,G13.6,255(A,G12.5))')
     &                           SPACE(1:EXTRA_SPX),LVO,
     &                          (SPACE(1:EXTRA_SP(I1)),AVV(I1),
     &                          I1=1,IFND2)
                           END IF
                        ELSE IF (ILTASK.EQ.9) THEN
                           LINE   = ' '
                           LINE_L = 0
                           CALL ADDINT (LINE,LINE_L,INT (LVO))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(1)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(2)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(3)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           DO I1=4,IFND2
                              CALL ADDREA (LINE,LINE_L,AVV(I1),
     &                                     '1P,G12.5')
                              CALL ADDSTR (LINE,LINE_L,',')
                           END DO
                           WRITE (ILU2,'(A)') LINE(1:LINE_L)
                        ELSE
                           CALL FATALERR ('OUTDAT','internal error')
                        END IF
                     ELSE
*                       one or more missing values found write to string
*                       LINE first
                        IF (ILTASK.GE.4.AND.ILTASK.LE.8) THEN
* no new indent level !
                        IF (.NOT.EXTRA_F) THEN
*                          no extra spaces required between columns
                           LINE   = ' '
                           LINE_L = 1
                           CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
                           DO I1=1,IFND2
                              CALL ADDSTF (LINE,LINE_L,CHR)
                              IF (FNDA(I1)) THEN
*                                value was found
                                 CALL ADDREF (LINE,LINE_L,
     &                              AVV(I1),'1P,G12.5')
                              ELSE
*                                value was not found
                                 LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
                                 LINE_L = LINE_L+COL_WIDTH_MNN
                              END IF
                           END DO
                        ELSE
*                          extra spaces required between columns
                           LINE   = ' '
                           LINE_L = EXTRA_SPX
                           CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
                           DO I1=1,IFND2
                              LINE_L = LINE_L+EXTRA_SP(I1)
                              IF (FNDA(I1)) THEN
*                                value was found
                                 CALL ADDREF (LINE,LINE_L,
     &                              AVV(I1),'1P,G12.5')
                              ELSE
*                                value was not found
                                 LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
                                 LINE_L = LINE_L+COL_WIDTH_MNN
                              END IF
                           END DO
                        END IF
* end of no new indent level !
                        ELSE IF (ILTASK.EQ.9) THEN
                           LINE   = ' '
                           LINE_L = 0
                           CALL ADDINT (LINE,LINE_L,INT (LVO))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(1)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(2)))
                           CALL ADDSTR (LINE,LINE_L,',')
                           CALL ADDINT (LINE,LINE_L,INT (AVV(3)))
                           DO I1=4,IFND2
                              CALL ADDSTR (LINE,LINE_L,',')
                              IF (FNDA(I1)) THEN
*                                value was found
                                 CALL ADDREA (LINE,LINE_L,
     &                              AVV(I1),'1P,G12.5')
                              ELSE
*                                value was not found
                                 CALL ADDSTR (LINE,LINE_L,'-')
                              END IF
                           END DO
                        ELSE
                           CALL FATALERR ('OUTDAT','internal error')
                        END IF
 
                        WRITE (ILU2,'(A)') LINE(1:LINE_L)
                     END IF
*                    initialize search for 'Y' values
                     YFND = .FALSE.
                  END IF
 
*                 reinitialize things
                  DO I1=1,IFND2
                     FNDA(I1) = .FALSE.
                     AVV(I1)  = -99.
                  END DO
                  ICHECK = 0
                  LVO = LV
               ELSE
*                 record contains 'Y' ; check names I2...I3
                  IF (BLK(INAME).EQ.IB) THEN
                     AVV(SEQ2(INAME))  = LV
                     YFND              = .TRUE.
                     ICHECK            = ICHECK+1
                     FNDA(SEQ2(INAME)) = .TRUE.
                  END IF
               END IF
            END DO
 
*           write last line at E_O_F
*           unfortunately this section must be identical to the
*           previous one
            IF (YFND) THEN
               IF (ICHECK.EQ.IFND2) THEN
*                 no missing variables, write line
*                 directly to file
                  IF (ILTASK.GE.4.AND.ILTASK.LE.8) THEN
                     IF (.NOT.EXTRA_F) THEN
*                       no extra spaces required,
*                       no missing variables
                        WRITE (ILU2,'(1X,1P,G13.6,255(A,G12.5))')
     &                         LVO,(CHR,AVV(I1),I1=1,IFND2)
                     ELSE
*                       extra spaces required
                        WRITE (ILU2,'(A,1P,G13.6,255(A,G12.5))')
     &                     SPACE(1:EXTRA_SPX),LVO,
     &                    (SPACE(1:EXTRA_SP(I1)),AVV(I1),
     &                    I1=1,IFND2)
                     END IF
                  ELSE IF (ILTASK.EQ.9) THEN
                     LINE   = ' '
                     LINE_L = 0
                     CALL ADDINT (LINE,LINE_L,INT (LVO))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(1)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(2)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(3)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     DO I1=4,IFND2
                        CALL ADDREA (LINE,LINE_L,AVV(I1),
     &                               '1P,G12.5')
                        CALL ADDSTR (LINE,LINE_L,',')
                     END DO
                     WRITE (ILU2,'(A)') LINE(1:LINE_L)
                  ELSE
                     CALL FATALERR ('OUTDAT','internal error')
                  END IF
               ELSE
*                 one or more missing values found write to string
*                 LINE first
                  IF (ILTASK.GE.4.AND.ILTASK.LE.8) THEN
* no new indent level !
                  IF (.NOT.EXTRA_F) THEN
*                    no extra spaces required between columns
                     LINE   = ' '
                     LINE_L = 1
                     CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
                     DO I1=1,IFND2
                        CALL ADDSTF (LINE,LINE_L,CHR)
                        IF (FNDA(I1)) THEN
*                          value was found
                           CALL ADDREF (LINE,LINE_L,
     &                        AVV(I1),'1P,G12.5')
                        ELSE
*                          value was not found
                           LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
                           LINE_L = LINE_L+COL_WIDTH_MNN
                        END IF
                     END DO
                  ELSE
*                    extra spaces required between columns
                     LINE   = ' '
                     LINE_L = EXTRA_SPX
                     CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
                     DO I1=1,IFND2
                        LINE_L = LINE_L+EXTRA_SP(I1)
                        IF (FNDA(I1)) THEN
*                          value was found
                           CALL ADDREF (LINE,LINE_L,
     &                        AVV(I1),'1P,G12.5')
                        ELSE
*                          value was not found
                           LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
                           LINE_L = LINE_L+COL_WIDTH_MNN
                        END IF
                     END DO
                  END IF
* end of no new indent level !
                  ELSE IF (ILTASK.EQ.9) THEN
                     LINE   = ' '
                     LINE_L = 0
                     CALL ADDINT (LINE,LINE_L,INT (LVO))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(1)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(2)))
                     CALL ADDSTR (LINE,LINE_L,',')
                     CALL ADDINT (LINE,LINE_L,INT (AVV(3)))
                     DO I1=4,IFND2
                        CALL ADDSTR (LINE,LINE_L,',')
                        IF (FNDA(I1)) THEN
*                          value was found
                           CALL ADDREA (LINE,LINE_L,
     &                        AVV(I1),'1P,G12.5')
                        ELSE
*                          value was not found
                           CALL ADDSTR (LINE,LINE_L,'-')
                        END IF
                     END DO
                  ELSE
                     CALL FATALERR ('OUTDAT','internal error')
                  END IF
 
                  WRITE (ILU2,'(A)') LINE(1:LINE_L)
               END IF
            END IF
         END DO
 
*        add some blank lines if table or spreadsheet output was
*        choosen
 
         IF (ILTASK.EQ.4.OR.ILTASK.EQ.5) WRITE (ILU2,'(/,/,/,1X)')
 
         END DO
 
      ELSE IF (ITASK.EQ.99) THEN
 
*        this option deletes the temporary file
         IF (ITOLD.EQ.99) CALL FATALERR ('OUTDAT',
     &       'Temporary file already deleted')
 
         CLOSE (ILU1, STATUS='DELETE')
 
      ELSE IF (ITASK.EQ.0) THEN
*        allow zero task, (occurs with ipform=0 in rkdriv and
*        eudriv drivers)
         CONTINUE
      ELSE
         CALL FATALERR ('OUTDAT','wrong ITASK')
      END IF
 
*     do not save task when it was a dummy task
      IF (ITASK.NE.0) ITOLD = ITASK
 
      RETURN
      END
**==OUTPLT.FOR  
      SUBROUTINE OUTPLT (ITASK, RN)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK
      CHARACTER*(*) RN
 
**    local variables
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*     A D J U S T A B L E   P A R A M E T E R                         *
*     =======================================                         *
*     IMNNAM = maximum number of names of dependent variables to be   *
*              plotted, if increased, also the DATA MARK statement    *
*              should be adjusted.                                    *
*     Warning: do not change the maximum length of names of variables,*
*              currently set to 11 !!                                 *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      INTEGER IMNNAM
      PARAMETER (IMNNAM=25)
      REAL AVMIN(IMNNAM), AVMAX(IMNNAM)
      CHARACTER*36 AVN(IMNNAM),LXN,LN
      CHARACTER*1 MARK(IMNNAM)
      LOGICAL FOUND(IMNNAM)
 
*     normal local parameters
      INTEGER IN, ILU1, ILU2, I, I1, I2, I3, I4, I5, I6, IY, ILTASK
      INTEGER IRUN, IWRITE, IFINDC, IOS, IEND, ISREC, IEREC, IREC
      INTEGER ILEN
      REAL LV, LVO, VMIN, VMAX, WIDTH
      CHARACTER*109 RECORD, REC1, REC2
      CHARACTER LXV*13, CHR*1, RUNTYP*1,RUNDUM*1
      LOGICAL FNDONE, OPEND, YFND
 
*     uncomment for mac absoft fortran compilation
*      CHARACTER EOFCHR*1
 
      SAVE
 
*     uncomment for mac absoft fortran compilation
*      DATA EOFCHR /'Z'/
 
      DATA IN /0/, ILU2 /0/
      DATA MARK /'1','2','3','4','5','6','7','8','9','0',
     &           'A','B','C','D','E','F','G','H','I','J',
     &           'K','L','M','N','P'/
 
*     'define variable name' option
 
      IF (ITASK.EQ.1) THEN
 
*        place new name in array of names, check for duplicates
*        and number of variables
 
         LXN = RN
         CALL UPPERC (LXN)
         I4 = IFINDC (AVN, IMNNAM, 1, IN, LXN)
         IF (I4.EQ.0) THEN
*           new variable found
            IN = IN+1
            IF (IN.GT.IMNNAM)
     &         CALL FATALERR ('OUTPLT',
     &         'Too many variables to be plotted')
            AVN(IN) = LXN
         END IF
 
      ELSE IF ((ITASK.GE.4.AND.ITASK.LE.7).OR.
     &         (ITASK.GE.14.AND.ITASK.LE.17)) THEN
 
         IF (IN.EQ.0) THEN
            WRITE (*,'(A)')
     &        ' WARNING from OUTPLT: Initialization not done'
            RETURN
         END IF
 
         ILTASK = MOD (ITASK,10)
 
*        try to get unit number from OUTDAT
         CALL AMBUSY (2,'OUTDAT',ILU2)
 
         IF (ILU2.EQ.0) THEN
*           no unit number obtained from OUTDAT
*           recover of output file without creation of an output table !
            ILU2 = 20
            ILU1 = ILU2+1
            CALL FOPENG (ILU1,'RES.BIN','OLD','UD',45,' ')
 
*           check if output file is open, else delete old one and
*           create new one
            INQUIRE (UNIT=ILU2, OPENED=OPEND)
            IF (.NOT.OPEND) CALL FOPENG
     &         (ILU2,'RES.DAT','NEW','SF',0,'DEL')
         END IF
         ILU1 = ILU2+1
 
*        find out if end record of last set is in the file
         READ (ILU1,REC=1) RUNTYP,IRUN
         IF (RUNTYP.EQ.'-') THEN
*           goto start of last set first
            IEREC = 1
            DO 10 I1=1,IRUN-1
               ISREC = IEREC+1
               READ (ILU1,REC=ISREC) RUNDUM,IEREC
10          CONTINUE
 
*           read rest until end_of_file
            IOS = 0
            IREC = IEREC+2
20          IF (IOS.EQ.0) THEN
               READ (ILU1,REC=IREC,IOSTAT=IOS) RUNDUM
 
*              uncomment for mac absoft fortran compilation
*               IF (RUNDUM.EQ.EOFCHR) IOS = -1
 
               IF (IOS.EQ.0) IREC = IREC+1
 
            GOTO 20
            END IF
 
            WRITE (ILU1, REC=IEREC+1) ' ',IREC-1,'...........',0.
            WRITE (ILU1, REC=1)       '+',IRUN  ,'...........',0.
         END IF
 
         IF (ITASK.GE.14) THEN
            I6 = 1
         ELSE
            I6 = IRUN
         END IF
 
         DO 30 I5=I6,IRUN
 
*        read names from file
*        search for first record of set first
         IEREC = 1
         DO 40 I1=1,I5
            ISREC = IEREC+1
            READ (ILU1,REC=ISREC) RUNDUM,IEREC
40       CONTINUE
 
         READ (ILU1,REC=ISREC+1) RUNTYP, IRUN, LXN
 
*        set minimum and maximum values to zero, single variables
*        are used for common scaling
         VMIN = 0.
         VMAX = 0.
         DO 50 I1=1,IMNNAM
            AVMIN(I1) = 0.
            AVMAX(I1) = 0.
            FOUND(I1) = .FALSE.
50       CONTINUE
 
*        determine minimum and maximum values from the file
*        and the number of records
         FNDONE = .FALSE.
         DO 60 IREC=ISREC+1,IEREC
 
            READ (ILU1,REC=IREC) RUNDUM, I1, LN, LV
 
            I = IFINDC (AVN, IMNNAM, 1, IN, LN)
            IF (I.GT.0) THEN
               IF (.NOT.FOUND(I)) THEN
                  AVMIN(I) = LV
                  AVMAX(I) = LV
                  FOUND(I) = .TRUE.
               END IF
               IF (.NOT.FNDONE) THEN
                  VMIN = LV
                  VMAX = LV
                  FNDONE = .TRUE.
               END IF
               IF (LV.LT.AVMIN(I)) AVMIN(I) = LV
               IF (LV.GT.AVMAX(I)) AVMAX(I) = LV
               IF (LV.LT.VMIN) VMIN = LV
               IF (LV.GT.VMAX) VMAX = LV
            END IF
60       CONTINUE
 
*        prepare records
         REC1 = ' '
         REC2 = ' '
 
         IF (ILTASK.EQ.4 .OR. ILTASK.EQ.5) THEN
*           wide format plots
            DO 70 I2=2,108
               REC1(I2:I2) = '-'
70          CONTINUE
            REC1(1:1) = 'I'
            REC1(109:109) = 'I'
 
            DO 80 I2=1,109,18
               REC2(I2:I2) = 'I'
80          CONTINUE
            IEND = 109
            WIDTH = 108.
         ELSE
*           small format plots
            DO 90 I2=2,64
               REC1(I2:I2) = '-'
90          CONTINUE
            REC1(1:1) = 'I'
            REC1(65:65) = 'I'
 
            DO 100 I2=1,65,16
               REC2(I2:I2) = 'I'
100         CONTINUE
            IEND  = 65
            WIDTH = 64.
         END IF
 
*        write header to output file
         WRITE (ILU2,'(/,14X,A)') RN
         IF (IRUN.EQ.0) THEN
            WRITE (ILU2,'(14X,A)')
     &        'Output plot number   :  0 (=first output plot)'
         ELSE IF (RUNTYP.EQ.'R') THEN
            WRITE (ILU2,'(14X,A,I4)')
     &        'Output from rerun set:',IRUN
         ELSE IF (RUNTYP.EQ.'N') THEN
            WRITE (ILU2,'(14X,A,I4)')
     &        'Output plot number   :',IRUN
         ELSE
            CALL FATALERR ('OUTPLT','unknown run type')
         END IF
 
         WRITE (ILU2,'(/,14X,4A,/,14X,4A)')
     &       'Variable  ','Marker  ','Minimum value  ','Maximum value',
     &       '--------  ','------  ','-------------  ','-------------'
 
*        write name, marker, minimum and maximum value
*        dependent on individual or common scaling
 
         DO 110 I=1,IN
            IF (FOUND(I)) THEN
               IF (AVMIN(I).NE.AVMAX(I)) THEN
                  IF (ILEN (AVN(I)).LE.11) THEN
                     WRITE (ILU2,'(14X,A11,2X,A,6X,G11.4,4X,G11.4)')
     &               AVN(I),MARK(I),AVMIN(I),AVMAX(I)
                  ELSE
                     WRITE (ILU2,'(14X,A,/,27X,A,6X,G11.4,4X,G11.4)')
     &               AVN(I),MARK(I),AVMIN(I),AVMAX(I)
                  END IF
               ELSE
                  IF (ILEN (AVN(I)).LE.11) THEN
                     WRITE (ILU2,'(14X,A11,2X,A,6X,G11.4,4X,G11.4,A)')
     &               AVN(I),MARK(I),AVMIN(I),AVMAX(I),
     &               ' no range to plot'
                  ELSE
                     WRITE (ILU2,'(14X,A,/,27X,A,6X,G11.4,4X,G11.4,A)')
     &               AVN(I),MARK(I),AVMIN(I),AVMAX(I),
     &               ' no range to plot'
                  END IF
               END IF
            ELSE
               WRITE (*,'(4A)') ' WARNING from OUTPLT: ',
     &           'variable ', AVN(I), 'not in set'
               WRITE (ILU2,'(4A)') ' WARNING from OUTPLT: ',
     &           'variable ', AVN(I), 'not in set'
            END IF
110      CONTINUE
 
*        add common scaling text and values if necessary
         IF (ILTASK.EQ.5 .OR. ILTASK.EQ.7) THEN
            WRITE (ILU2,'(/,14X,A,5X,G11.4,4X,G11.4)')
     &        'Scaling: Common', VMIN, VMAX
         ELSE
            WRITE (ILU2,'(/,14X,A)')
     &        'Scaling: Individual'
         END IF
 
*        write name of independent variable
         WRITE (ILU2,'(/,3X,A,/)') LXN
 
*        read file again and plot
         YFND = .FALSE.
 
*        set number of plot lines written to file to zero
*        start plotting
 
         IWRITE = 0
         RECORD = REC1
 
         DO 120 I2=1,IMNNAM
            FOUND(I2) = .FALSE.
120      CONTINUE
 
         DO 130 IREC=ISREC+1,IEREC
            READ (ILU1,REC=IREC) RUNDUM, I2, LN, LV
 
*              new name is independent name
               IF (LN.EQ.LXN) THEN
                  IF (YFND) THEN
 
                     WRITE (LXV,'(1P,G13.6)') LVO
                     WRITE (ILU2,'(2(1X,A))') LXV, RECORD(1:IEND)
                     IWRITE = IWRITE+1
 
*                    take start record dependent on number of
*                    lines written
 
                     IF (MOD (IWRITE,10).EQ.0) THEN
                        RECORD = REC1
                     ELSE
                        RECORD = REC2
                     END IF
 
                     YFND = .FALSE.
 
                     DO 140 I2=1,IMNNAM
                        FOUND(I2) = .FALSE.
140                  CONTINUE
 
                  END IF
                  LVO = LV
 
               ELSE
 
*                 Y name found
                  I = IFINDC (AVN, IMNNAM, 1, IN, LN)
                  IF (I.GT.0) THEN
                     IF (.NOT.FOUND(I)) THEN
                        YFND = .TRUE.
                        FOUND(I) = .TRUE.
 
*                       if individual scaling was choosen reset
*                       variables
 
                        IF (ILTASK.EQ.4 .OR. ILTASK.EQ.6) THEN
                           VMAX = AVMAX(I)
                           VMIN = AVMIN(I)
                        END IF
 
*                       plot only if there is a range
                        IF (VMAX.NE.VMIN) THEN
                           IY = 1+NINT(WIDTH*(LV-VMIN)/(VMAX-VMIN))
                           CHR = RECORD(IY:IY)
                           IF (CHR.EQ.' '.OR.
     &                         CHR.EQ.'I'.OR.
     &                         CHR.EQ.'-') THEN
                              RECORD(IY:IY) = MARK(I)
                           ELSE
                              RECORD(IY:IY) = '*'
                           END IF
                        END IF
                     ELSE
                        I2 = ILEN (LN)
                        I3 = ILEN (LXN)
                        WRITE (*,'(4A)')
     &                   ' WARNING from OUTPLT: variable ',LN(1:I2),
     &                   ' occurs twice at same value of ',LXN(1:I3)
                        WRITE (ILU2,'(4A)')
     &                   ' WARNING from OUTPLT: variable ',LN(1:I2),
     &                   ' occurs twice at same value of ',LXN(1:I3)
                     END IF
                  END IF
               END IF
130      CONTINUE
 
*        plot last line
 
         WRITE (LXV,'(1P,G13.6)') LVO
         IF (YFND) WRITE (ILU2,'(2(1X,A))') LXV, RECORD(1:IEND)
         WRITE (ILU2,'(/,/,/,A)') ' '
 
30       CONTINUE
 
*        set number of variables to zero
         IN = 0
      ELSE
         CALL FATALERR ('OUTPLT','Wrong ITASK')
      END IF
 
      RETURN
      END
**==OUTSEL.FOR  
      SUBROUTINE OUTSEL (PRSEL,IMNPRS,INPRS,IPFORM,MESSAG)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IMNPRS,INPRS,IPFORM
      CHARACTER*(*) PRSEL,MESSAG
      DIMENSION PRSEL(IMNPRS)
 
**    local variables and function types
      INTEGER IPRS
      LOGICAL TABGEN
      SAVE
 
 
*     checks
      IF (.NOT.((IPFORM.GE. 4 .AND. IPFORM.LE. 6) .OR.
     $          (IPFORM.GE.14 .AND. IPFORM.LE.16)) )
     $ CALL FATALERR ('OUTSEL','Illegal value of IPFORM')
      IF (INPRS.GT.IMNPRS) CALL FATALERR
     $   ('OUTSEL','illegal value INPRS')
 
*     output table(s) to file
      TABGEN = .FALSE.
      DO 10 IPRS=0,INPRS
         IF (IPRS.GT.0) THEN
*           check element of output table list
            TABGEN = PRSEL(IPRS).EQ.'<TABLE>'
            IF (.NOT.TABGEN) CALL OUTDAT (3,0,PRSEL(IPRS),0.0)
         END IF
 
*        write formatted table
         IF (TABGEN.OR.IPRS.EQ.INPRS) CALL OUTDAT (IPFORM,0,MESSAG ,0.)
10    CONTINUE
 
      RETURN
      END
**==PARSWORD.FOR
*     ---------------------------------------------
*     ---------------------------------------------
      SUBROUTINE PARSWORD (XSTRING,TTYPE,IB,IE,NERR,NWAR)
 
*     Finds and decodes 1 word with leading and/or trailing spaces
*     decoded values are passed to calling program via  RDDECINF.INC
*     If there is more than a single word an error message is given
*
*     XSTRING - character string                               I
*     TTYPE   - returned token type according to RDNDEC.GIN    O
*               if only spaces are parsed then TTYPE -> TSPACE
*     IB      - begin of word pointer                          O
*     IE      - end of word pointer                            O
*     NERR    - >0 there is an error                           O
*     NWAR    - >0 there is a warning                          O
 
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XSTRING
      INTEGER TTYPE, IB, IE, NERR, NWAR
 
*     buffer for RDLEX routine
      INCLUDE 'rdrecinf.inc'
 
*     token information
      INCLUDE 'lextokin.inc'
 
*     integer codes for tokens (TSPACE is used below)
      INCLUDE 'rdndec.gin'
 
*     error messages
      INCLUDE 'rderrinf.inc'
 
*     local variables
      INTEGER ILS
      SAVE
 
*     functions called
      INTEGER ILEN
 
*     write string in buffer under line number 0
      ILS    = ILEN (XSTRING)
      STBUF  = XSTRING
      STBLEN = LEN(XSTRING)
      RECNO  = 0
 
*     parse
      CALL RDLEX (1)
      CALL RDLEX (2)
      CALL RDLEX (3)
 
      IF (TOKEN.EQ.TSPACE .AND. IP.LT.ILS) THEN
*        there are leading spaces ; another time
         CALL RDLEX (3)
      END IF
 
*     returned
      TTYPE = TOKEN
      IB    = ISP
      IE    = IP
 
*     error check
      IF (IP.LT.ILS) THEN
         ISP = IP+1
         IP  = ILS
         CALL RDERR (2,'This is not a single item')
      END IF
 
      NERR  = INERR
      NWAR  = INWAR
 
      RETURN
      END
**==RCHRSRC.FOR 
      INTEGER FUNCTION RCHRSRC (CHARS,STRING,POSBEG,POSEND)
      IMPLICIT NONE
 
*     this routine searches for the first occurrence of one of the
*     characters of CHARS in STRING in reverse order ! (Contrary to
*     the intrinsic function INDEX it does not look for a match of
*     the whole string.
 
*     formal parameters
      CHARACTER*(*) STRING, CHARS
      INTEGER POSBEG, POSEND
 
*     local parameters
      INTEGER POS
      CHARACTER*1 CHR
      LOGICAL FND
      SAVE
 
      POS = POSEND
 
      FND = .FALSE.
10    IF (.NOT.FND.AND.POS.GE.POSBEG) THEN
         CHR = STRING(POS:POS)
         FND = INDEX (CHARS,CHR).NE.0
         IF (.NOT.FND) POS = POS-1
      GOTO 10
      END IF
 
      RCHRSRC = POS
 
      RETURN
      END
**==RDACHA.FOR  
      SUBROUTINE RDACHA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME,X
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80          CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDACHA',0,0,' ',IS,XNAME,'C',
     $             D,R,I,X,L,1,1,1,ILDEC,1,IFND,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDADOR.FOR  
      SUBROUTINE RDADOR (XNAME,XMIN,XMAX,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      DOUBLE PRECISION X,XMIN,XMAX
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
       
*     other
      INTEGER IS,I1,ILX,ILEN
      DOUBLE PRECISION XL
      CHARACTER Q*1, LXNAME*31
      SAVE
 
      DATA Q /''''/
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDADOR',0,0,' ',IS,XNAME,'D',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IFND,
     $             DM,RM,IM,CM,LM)
 
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
 
      DO 10 I1=1,IFND
         XL = X(I1)
         IF (XMAX.LT.XMIN) THEN
            WRITE (*,'(1X,4A)')
     &        'ERROR in RDADOR: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            CALL FATALERR (' ',' ')
         ELSE IF ((XL.LT.XMIN.OR.XL.GT.XMAX).AND.XL.NE.DM) THEN
            WRITE (*,'(1X,4A,I3,3A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &        'ERROR in RDADOR: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            CALL FATALERR (' ',' ')
         END IF
10    CONTINUE
 
      RETURN
      END
**==RDADOU.FOR  
      SUBROUTINE RDADOU (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      DOUBLE PRECISION X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDADOU',0,0,' ',IS,XNAME,'D',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IFND,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDAINR.FOR  
      SUBROUTINE RDAINR (XNAME,XMIN,XMAX,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      INTEGER X,XMIN,XMAX
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER               IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS,I1,ILX,ILEN
      INTEGER XL
      CHARACTER Q*1, LXNAME*31
      SAVE
 
      DATA Q /''''/
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDAINR',0,0,' ',IS,XNAME,'I',
     $             D,R,X,C,L,1,1,ILDEC,1,1,IFND,
     $             DM,RM,IM,CM,LM)
 
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
 
      DO 10 I1=1,IFND
         XL = X(I1)
         IF (XMAX.LT.XMIN) THEN
            WRITE (*,'(1X,4A)')
     &        'ERROR in RDAINR: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            CALL FATALERR (' ',' ')
         ELSE IF ((XL.LT.XMIN.OR.XL.GT.XMAX).AND.XL.NE.IM) THEN
            WRITE (*,'(1X,4A,I3,3A,/,T19,A,I8,A,I8,A,I8,A)')
     &        'ERROR in RDAINR: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            CALL FATALERR (' ',' ')
         END IF
10    CONTINUE
 
      RETURN
      END
**==RDAINT.FOR  
      SUBROUTINE RDAINT (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND,X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER               IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDAINT',0,0,' ',IS,XNAME,'I',
     $             D,R,X,C,L,1,1,ILDEC,1,1,IFND,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDALOG.FOR  
      SUBROUTINE RDALOG (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      LOGICAL X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL               LM
 
*     other
      INTEGER IS
      SAVE
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDALOG',0,0,' ',IS,XNAME,'L',
     $             D,R,I,C,X,1,1,1,1,ILDEC,IFND,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDAREA.FOR  
      SUBROUTINE RDAREA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      REAL X
      DIMENSION X(ILDEC)
      CHARACTER*(*) XNAME
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL                  RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDAREA',0,0,' ',IS,XNAME,'R',
     $             D,X,I,C,L,1,ILDEC,1,1,1,IFND,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDARER.FOR  
      SUBROUTINE RDARER (XNAME,XMIN,XMAX,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      REAL X,XMIN,XMAX
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL                  RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS,I1,ILX,ILEN
      REAL XL
      CHARACTER Q*1, LXNAME*31
      SAVE
 
      DATA Q /''''/
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDARER',0,0,' ',IS,XNAME,'R',
     $             D,X,I,C,L,1,ILDEC,1,1,1,IFND,
     $             DM,RM,IM,CM,LM)
 
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
 
      DO 10 I1=1,IFND
         XL = X(I1)
         IF (XMAX.LT.XMIN) THEN
            WRITE (*,'(1X,4A)')
     &        'ERROR in RDARER: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            CALL FATALERR (' ',' ')
         ELSE IF ((XL.LT.XMIN.OR.XL.GT.XMAX).AND.XL.NE.RM) THEN
            WRITE (*,'(1X,4A,I3,3A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &        'ERROR in RDARER: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            CALL FATALERR (' ',' ')
         END IF
10    CONTINUE
 
      RETURN
      END
**==RDATIM.FOR  
      SUBROUTINE RDATIM (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      DOUBLE PRECISION X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
*     the number of values is set by RDDATA
      CALL RDDATA (6,'RDATIM',0,0,' ',IS,XNAME,'T',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IFND,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDDATA.FOR  
      SUBROUTINE RDDATA (ITASK,CALPRG,IUNIT,IULOG,FILNAM,IS,
     $    XNAME,VARTYP,DX,RX,IX,CX,LX,
     $    NDDEC,NRDEC,NIDEC,NCDEC,NLDEC,NREQ,
     $    xDM,xRM,xIM,xCM,xLM)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK,IUNIT,IULOG,IS
      INTEGER NDDEC,NRDEC,NIDEC,NCDEC,NLDEC,NREQ
      DOUBLE  PRECISION DX(NDDEC),xDM
      REAL    RX(NRDEC),xRM
      INTEGER IX(NIDEC),xIM
      CHARACTER*250     xCM
      LOGICAL LX(NLDEC),xLM
      CHARACTER*(*) CALPRG,CX(NCDEC),FILNAM,XNAME,VARTYP*1
 
**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
 
*     read rddata common block
      INCLUDE 'rddata.inc'
 
*     some other variables on rerun and data values
      INTEGER IUNREP,IULR,INR,ITR,IRREP,PNTRNX,ILR
      INTEGER IUNDAT,IULD,IND,ITD,IRDAT,PNTDNX,ILD
      INTEGER INSETS,ISLOC,IPP,IRCHK,IDUM,IPS
      CHARACTER RFIL*250,DFIL*250,REPTMP*250,DATTMP*250
      DIMENSION IRCHK(ILNREP)
      LOGICAL LOGR,REPL,LOGD,INIDAT
 
*     filename list
      INTEGER INLIST,INFILS
      CHARACTER*250 FILLIS,TMPLIS
      DIMENSION FILLIS(INFDEC),TMPLIS(INFDEC)
 
*     rerun file buffers lying over each other
      INTEGER          IRBUF(0:ILBUF/IIL-1)
      DOUBLE PRECISION RRBUF(0:ILBUF/IRL-1)
      LOGICAL          LRBUF(0:ILBUF/ILL-1)
      EQUIVALENCE (IRBUF,RRBUF,LRBUF)
 
*     data file buffers lying over each other
      INTEGER          IDBUF(0:ILBUF/IIL-1)
      DOUBLE PRECISION RDBUF(0:ILBUF/IRL-1)
      LOGICAL          LDBUF(0:ILBUF/ILL-1)
      EQUIVALENCE (IDBUF,RDBUF,LDBUF)
 
*     other local variables
      INTEGER I,J,IBN,IEL,N,ISX,ILX,IMES,IWN,IREC,ILTMP,IRECL
      INTEGER NDEC,NELMT,IXL,IT1,IT2,IWAR,IVT,IV1,IV2,IVL,IVP,IPN
      INTEGER IUL,ISTREC,ILA,ILT,ILW,IPNT,IREP,IPC,IXLMAX,ITMP1
      DOUBLE PRECISION AD
      CHARACTER ERRTXT*26,MESSAG*42,LXNM31*31,ONFILE*9,GETSCA*2,SCARR*6
      CHARACTER BYTE*1,PPP*3,VALUE*12,LINE*76,ASKED*16,REQTYP*1,GETONF*5
      CHARACTER WHAT*35,TARGET*10,RECORD*79,ARROW*7
      DIMENSION MESSAG(28),ONFILE(5),ASKED(6),REQTYP(6),RECORD(4)
      DIMENSION SCARR(2)
      LOGICAL OPUNIT,OVERWR,EMPTY,THERE,FATAL,FOUND,ON,OFF
      LOGICAL SINGLE,UNKNOW,FIXED,TEMP,GIVEN,FILLED,REPMES,LOGF
      LOGICAL INIT,DREAD,PARSE
      DIMENSION IPC(0:IIL-1)
 
*     variables to hold missing values
      REAL RM_D, RM
      INTEGER IM_D, IM
      DOUBLE PRECISION DM_D, DM, TM_D, TM
      CHARACTER*250 CM_D, CM
      LOGICAL LM_D, LM
 
*     used functions
      INTEGER IFINDC, ISTART, ILEN
      LOGICAL FLEXIST
 
      SAVE
      DATA IUNDAT/0/, IUNREP/0/
      DATA ONFILE /'CHARACTER','INTEGER  ','REAL     ',
     $             'LOGICAL  ','DATE_TIME'/
      DATA GETONF /'CIFLT'/
      DATA ASKED  /'CHARACTER       ','DOUBLE PRECISION',
     $             'INTEGER         ','REAL            ',
     $             'LOGICAL         ','DATE_TIME       '/
      DATA GETSCA /'sa'/
      DATA SCARR  /'scalar','array'/
      DATA REQTYP /'C','F','I','F','L','T'/
      DATA ISLOC/0/, INIDAT/.FALSE./, REPL/.FALSE./, THERE/.FALSE./
      DATA ON/.TRUE./, OFF/.FALSE./, INFILS/0/, GIVEN/.FALSE./
      DATA ERRTXT /'Routine RDDATA, called by '/
      DATA INIT /.FALSE./
      DATA RM_D /-99.99/ , IM_D /-99/, DM_D /-99.99D0/,
     &     CM_D /'- MISSING -'/, LM_D /.FALSE./, TM_D /-99.99D0/
      DATA (MESSAG(I),I=1,14)
     $/'Variable name too long                    ',
     $ 'Under ITASK=5 declared length should be 1 ',
     $ 'Requested number of values not positive   ',
     $ 'Variable name not in data file            ',
     $ 'On data file this is a CHARACTER string   ',
     $ 'On data file this variable is INTEGER     ',
     $ 'On data file this variable is REAL        ',
     $ 'On data file this variable is LOGICAL     ',
     $ 'On data file this is a DATE_TIME          ',
     $ 'On data file this variable is an array    ',
     $ 'On data file this variable is scalar      ',
     $ 'Array on data file not of requested size  ',
     $ 'Array on data file too large              ',
     $ 'The rerun file contains a CHARACTER string'/
      DATA (MESSAG(I),I=15,28)
     $/'The rerun file contains an INTEGER        ',
     $ 'The rerun file contains a REAL variable   ',
     $ 'The rerun file contains a LOGICAL variable',
     $ 'The rerun file contains a DATE_TIME       ',
     $ 'On rerun file this variable is an array   ',
     $ 'On rerun file this variable is scalar     ',
     $ 'Array on rerun file not of requested size ',
     $ 'Array on rerun file too large to fit      ',
     $ 'Single precision underflow on data file   ',
     $ 'Single precision overflow on data file    ',
     $ 'String(s) on data file too long           ',
     $ 'Single precision underflow on rerun file  ',
     $ 'Single precision overflow on rerun file   ',
     $ 'String(s) on rerun file too long          '/
 
 
      IF (.NOT.INIT) THEN
*        initialize ; calculate values of array IPC
         IPC(IIL-1) = 1
         DO 5 I=IIL-2,0,-1
            IPC(I) = 256 * IPC(I+1)
5        CONTINUE
 
*        calculate IXLMAX as the maximum type length
         IXLMAX = MAX(IIL,IRL,ILL)
 
*        assign default missing values
         RM = RM_D
         IM = IM_D
         DM = DM_D
         CM = CM_D
         LM = LM_D
         TM = TM_D
 
*        copy set number to common block
         ISCOM  = 0
 
         INFDAT = 0
         INFREP = 0
 
         INIT = .TRUE.
      END IF
 
      IF (ITASK.EQ.1) THEN
*        read rerun file
         IF (REPL) THEN
*           there may be a previous TMP file open
            INQUIRE (UNIT=IUNREP, OPENED=OPUNIT)
            IF (OPUNIT) CLOSE (IUNREP)
         END IF
 
         I = ILEN (FILNAM)
         EMPTY = I.EQ.0
         IF (.NOT.EMPTY) THERE = FLEXIST (FILNAM)
 
         IF (EMPTY .OR. .NOT.THERE) THEN
*           disactivate replacements
            REPL  = OFF
            IS    = 0
            ISLOC = 0
 
*           delete swap files if they exist, release units
            CALL SWPI4 (3,'RD\IARREP',0,0,0,0)
            CALL SWPI4 (3,'RD\IPTREP',0,0,0,0)
 
            CALL AMBUSY (1,'RDFROM',ISLOC)
            RETURN
         END IF
 
*        set (new) local unit number + local logfile unit
         IUNREP = IUNIT
         LOGR   = IULOG.GT.0
         IULR   = IULOG
 
*        get local copy of filename
         RFIL = FILNAM
         ILR  = ILEN (RFIL)
         CALL UPPERC (RFIL)
         CALL EXTENS (RFIL,'TMP',1,REPTMP)
 
*        analyze file and check for errors
         CALL RDINDX (IWAR,IUNREP,ON,ON,LOGR,IULR,FILNAM,REPTMP,
     $    REPLIS,REPTYP,REPARR,ILNREP,
     $    IARREP,IPTREP,ILPREP,INFREP,INSETS)
         IF (IWAR.GT.0) THEN
            WRITE (*,'(/,/,3A,I4)')
     $      ' Number of errors in rerun file ',RFIL(1:ILR),':',IWAR
            CALL FATALERR (' ',' ')
         END IF
 
*        reset set number and record number ; set replace flag
         ISLOC  = 0
         IRREP  = 0
         REPL   = ON
 
*        message to AMBUSY ; set 0 selected
         CALL AMBUSY (1,'RDFROM',ISLOC)
 
*        return number of sets
         IS = INSETS
 
         IF (INSETS.GT.0) THEN
*           report to logfile
            IF (LOGR) THEN
               WRITE (IULR,'(/,1X,3A,I5,A,I3,A,/,1X,A,/,1X,A)')
     $          'File ',RFIL(1:ILR),' contains',INSETS,' set(s) of',
     $           INFREP,' variable(s):',
     $          'type sc/ar variable name',
     $          '---- ----- -------------'
               DO 10 I=1,INFREP
                  LXNM31 = REPLIS(I)
                  WRITE (IULR,'(2X,A,5X,A,3X,A)') 
     $             REPTYP(I),REPARR(I),REPLIS(I)
10             CONTINUE
 
               WRITE (IULR,'(/,1X,A)')
     $          '= Data file contents (set 0) selected ='
            END IF
 
*           reset replace counters
            DO 20 I=1,INFREP
               IRCHK(I) = 0
20          CONTINUE
         END IF
 
*        copy set number to common block
         ISCOM  = ISLOC
 
      ELSE IF (ITASK.EQ.2) THEN
*        change set number
         IF (IS.EQ.ISLOC) RETURN
         IF (.NOT.REPL) CALL FATALERR ('RDFROM','no rerun file active')
         FATAL = NREQ.GT.0
 
         IF (ISLOC.GE.1) THEN
*           check variable use of previous set
            DO 30 I=1,INFREP
               IF (IRCHK(I).EQ.0) THEN
                  ILX = ILEN (REPLIS(I))
                  IF (LOGR) WRITE (IULR,'(3X,2A)')
     $             REPLIS(I)(1:ILX),'   !!!!!!   NOT USED   !!!!!!'
 
                  IF (FATAL) THEN
                     WRITE (*,'(/,1X,A,I5,A,I5,3A,/,1X,3A,I5,A)')
     $                '- Moving from set',ISLOC,' to set',IS,' of ',
     $                RFIL(1:ILR),':','  Variable ',
     $                REPLIS(I)(1:ILX),' of set',ISLOC,' was NOT USED'
                     CALL FATALERR (' ',' ')
                  END IF
               END IF
               IRCHK(I) = 0
30          CONTINUE
         END IF
 
*        change set
         IF (IS.GE.0 .AND. IS.LE.INSETS) THEN
            ISLOC = IS
         ELSE
            CALL FATALERR ('RDFROM','illegal set number requested')
         END IF
 
*        message to AMBUSY ; set IS selected
         CALL AMBUSY (1,'RDFROM',IS)
 
*        report to logfile
         IF (IS.EQ.0 .AND. LOGR) WRITE (IULR,'(/,1X,A)')
     $    '= Data file contents (set 0) selected ='
         IF (IS.GE.1 .AND. LOGR) WRITE (IULR,'(/,1X,A,I5,3A)')
     $    '= Set',IS,' of rerun file ',RFIL(1:ILR),' selected ='
 
*        copy set number to common block
         ISCOM = ISLOC
 
      ELSE IF (ITASK.EQ.3 .OR. ITASK.EQ.4) THEN
*        initialize data file
         IF (INIDAT) THEN
*           close old unit if left open
            INQUIRE (UNIT=IUNDAT, OPENED=OPUNIT)
            IF (OPUNIT) CLOSE(IUNDAT)
         END IF
 
*        local unit number + local logfile unit
         IUNDAT = IUNIT
         LOGD   = IULOG.GT.0
         IULD   = IULOG
 
*        get local copy of filename
         DFIL = FILNAM
         ILD  = ILEN (DFIL)
         CALL UPPERC (DFIL)
*        position of last part of filename for logfile output
         IPS = MAX (ILD-11,1)
 
*        file name in list ?
         INLIST = IFINDC (FILLIS,INFDEC,1,INFILS,DFIL)
 
         IF (INLIST.EQ.0.AND.INFILS.LT.INFDEC) THEN
 
*           new file name does not occur in list and there
*           is place to store it
            INFILS         = INFILS + 1
            FILLIS(INFILS) = DFIL
 
            PARSE = .TRUE.
 
*           build tmp filename
            DATTMP = 'C:/TEMP/RD$'
            ITMP1 = 11
            CALL ADDINF (DATTMP,ITMP1,INFILS,'I5.5')
            CALL ADDSTR (DATTMP,ITMP1,'.TMP')
            TMPLIS(INFILS) = DATTMP
 
         ELSE IF (INLIST.EQ.0.AND.INFILS.GE.INFDEC) THEN
 
            PARSE = .TRUE.
 
*           default tmp file name
            DATTMP = 'C:/TEMP/RD$00000.TMP'
 
*           filename not in list, but list is full
            IF (.NOT.GIVEN.AND.ITASK.EQ.3) THEN
*              the new name is not included
               WRITE (*,'(9(/,2A),2(/,A))')
     $          ' MESSAGE from RDDATA: After reading with RDINIT',
     $          ' the data',' file ',DFIL(IPS:ILD),' the list of',
     $          ' file names appears to be full. Hence, the name',
     $          ' of the analyzed file cannot be included in the',
     $          ' list, which',' implies that RDINIT will not',
     $          ' recover the file contents from',' temporary file',
     $          ' when you initialize the same file once again.',
     $          ' When you, in a rerun loop for instance,',
     $          ' repeatedly use this',' file, the data will be',
     $          ' parsed again and again. To avoid that,',
     $          ' use RDPARS for files you initialize only once',
     $          ' and/or increase',' PARAMETER INFDEC in RDDATA.',
     $          ' This message will not be repeated for other files.'
               GIVEN = .TRUE.
            END IF
 
         ELSE IF (INLIST.GT.0) THEN
*           filename was found in list, what to do ?
            DATTMP = TMPLIS(INLIST)
            IF (ITASK.EQ.3) THEN
*              call was from rdinit, recover when tmp file exists,
*              parse when it does not exist
               IF (FLEXIST (DATTMP)) THEN
                  PARSE = .FALSE.
               ELSE
                  PARSE = .TRUE.
               END IF
            ELSE IF (ITASK.EQ.4) THEN
*              call was from rdpars, parse anyway
               PARSE = .TRUE.
            END IF
         END IF
 
         IF (PARSE) THEN
*           analyze file and check for errors
            CALL RDINDX (IWAR,IUNDAT,OFF,ON,LOGD,IULD,FILNAM,DATTMP,
     $       DATLIS,DATTYP,DATARR,ILNDAT,
     $       IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)
            IF (IWAR.GT.0) THEN
               WRITE (*,'(/,/,3A,I4)')
     $         ' Number of errors in data file ',DFIL(1:ILD),':',IWAR
               CALL FATALERR (' ',' ')
            ELSE
*              save index
               IDUM = 1
               CALL RDTMP2 (1,IUNDAT,DATLIS,
     $          DATTYP,DATARR,ILNDAT,IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)
               IF (LOGD) WRITE (IULD,'(3A,I4,A)')
     $          ' Data file   ',DFIL(IPS:ILD),
     $          ' with',INFDAT,' variables parsed by RDINDX'
            END IF
 
         ELSE
*           filename previously used ; open available temporary
            IRECL = ILBUF + IIL
            CALL FOPENG (IUNIT,DATTMP,'OLD','UD',IRECL,' ')
 
*           recover pointer info
            CALL RDTMP2 (2,IUNDAT,DATLIS,
     $       DATTYP,DATARR,ILNDAT,IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)
 
            IF (LOGD) WRITE (IULD,'(1X,3A)') 'Contents of ',
     $       DFIL(1:ILD),' recovered from TMP file'
         END IF
 
*        reset record number and set flags
         IRDAT  = 0
         INIDAT = ON
         REPMES = OFF
 
*        when the rerun facility is active
*        check type and array/scalar consistency  
         IF (REPL) THEN
            DO 60 I=1,INFDAT
               LXNM31 = DATLIS(I)
               INR = IFINDC (REPLIS,ILNREP,1,INFREP,LXNM31)
               IF (INR.GT.0) THEN
                  IF (DATTYP(I).NE.'-' .AND. REPTYP(INR).NE.'-') THEN
*                    typed on either datafile or rerun file
                     IF (DATTYP(I).NE.REPTYP(INR)) THEN
*                       type inconsistency
                        ITD = INDEX (GETONF,DATTYP(I))
                        ITR = INDEX (GETONF,REPTYP(INR))
                        ILX = ILEN (LXNM31)
                        IT1 = ILEN (ONFILE(ITD))
                        IT2 = ILEN (ONFILE(ITR))
 
                        WRITE (*,'(/,1X,6A,/,5A)')
     $                   ONFILE(ITD)(1:IT1),' variable ',LXNM31(1:ILX),
     $                   ' on data file ',DFIL(1:ILD),' occurs',
     $                   ' on rerun file ',RFIL(1:ILR),' as ',
     $                     ONFILE(ITR)(1:IT2),' variable'
                        CALL FATALERR (' ',' ')
                     END IF
                  END IF
 
                  IF (DATARR(I).NE.REPARR(INR)) THEN
*                    type inconsistency
                     ITD = INDEX (GETSCA,DATARR(I))
                     ITR = INDEX (GETSCA,REPARR(INR))
                     ILX = ILEN (LXNM31)
                     IT1 = ILEN (SCARR(ITD))
                     IT2 = ILEN (SCARR(ITR))
 
                     WRITE (*,'(/,1X,6A,/,5A)')
     $                SCARR(ITD)(1:IT1),' variable ',LXNM31(1:ILX),
     $                ' on data file ',DFIL(1:ILD),' occurs',
     $                ' on rerun file ',RFIL(1:ILR),' as ',
     $                  SCARR(ITR)(1:IT2),' variable'
                     CALL FATALERR (' ',' ')
                  END IF
               END IF
60          CONTINUE
         END IF
 
 
      ELSE IF (ITASK.EQ.5 .OR. ITASK.EQ.6 .OR. ITASK.EQ.7) THEN
*        get data ; error checks
         IF (.NOT.INIDAT) CALL FATALERR ('RDDATA','no data file active')
 
*        classify call
         SINGLE = ITASK.EQ.5
         UNKNOW = ITASK.EQ.6
         FIXED  = ITASK.EQ.7
 
*        copy values for missing data to external variables
         xDM = DM
         xRM = RM
         xIM = IM
         xCM = CM
         xLM = LM
 
         IF (VARTYP.EQ.'C') THEN
*           get character variable
            IVT   = 1
            NDEC  = NCDEC
            IXL   = 1
            NELMT = LEN(CX(1))
         ELSE IF (VARTYP.EQ.'D') THEN
*           get double precision variable
            IVT  = 2
            NDEC = NDDEC
            IXL  = IRL
         ELSE IF (VARTYP.EQ.'I') THEN
*           get integer variable
            IVT  = 3
            NDEC = NIDEC
            IXL  = IIL
         ELSE IF (VARTYP.EQ.'R') THEN
*           get real variable
            IVT  = 4
            NDEC = NRDEC
            IXL  = IRL
         ELSE IF (VARTYP.EQ.'L') THEN
*           get logical variable
            IVT  = 5
            NDEC = NLDEC
            IXL  = ILL
         ELSE IF (VARTYP.EQ.'T') THEN
*           get date_time
            IVT   = 6
            NDEC  = NDDEC
            IXL   = IRL
         END IF
 
*        get local copy of significant part of name in uppercase and
*        the length of the name in ILX
         ISX    = MAX (1,ISTART (XNAME))
         ILX    = MAX (1,ILEN (XNAME))
         LXNM31 = XNAME(ISX:ILX)
         CALL UPPERC (LXNM31)
 
*        check data file index
         IND  = IFINDC (DATLIS,ILNDAT,1,INFDAT,LXNM31)
 
*        check rerun file for overwrite facility
         INR = 0
*        variable occurs on active rerun file ?
         IF (REPL) INR = IFINDC (REPLIS,ILNREP,1,INFREP,LXNM31)
*        overwrite data file value(s) ?
         OVERWR = INR.GT.0 .AND. ISLOC.GT.0
 
*        reset error ; length of name
         IMES   = 0
 
         IF (ILX.GT.31) THEN
*           requested name too long
            IMES = 1
         ELSE IF (SINGLE .AND. NDEC.NE.1) THEN
*           under ITASK=5 declared length should be 1
            IMES = 2
         ELSE IF (FIXED .AND. NREQ.LE.0) THEN
*           requested number of values is not positive
            IMES = 3
         ELSE IF (IND.EQ.0) THEN
*           name does not occur in data file
            IMES = 4
         ELSE IF (DATTYP(IND).NE.'-' .AND.
     $            DATTYP(IND).NE.REQTYP(IVT)) THEN
*           variable type mismatch ; error 5,6,7,8 or 9
            IMES = 4 + INDEX (GETONF,DATTYP(IND))
         ELSE
*           number of values on file N
            N = IARDAT(IND)
 
*           error checks
            IF (SINGLE .AND. DATARR(IND).EQ.'a') THEN
               IMES = 10
            ELSE IF ((UNKNOW .OR. FIXED) .AND. DATARR(IND).EQ.'s') THEN
               IMES = 11
            ELSE IF (FIXED .AND. N.NE.NREQ .AND. .NOT.OVERWR) THEN
               IMES = 12
            ELSE IF (N.GT.NDEC) THEN
               IMES = 13
            END IF
 
            IF (IMES.EQ.0 .AND. INR.GT.0) THEN
*              variable occurs on active rerun file ; type checking,
*              also in case set 0 is selected and OVERWR is .false.
               IF (REPTYP(INR).NE.'-' .AND.
     $             REPTYP(INR).NE.REQTYP(IVT)) THEN
*                 variable type mismatch ; error 14,15,16,17 or 18
                  IMES = 13 + INDEX (GETONF,REPTYP(INR))
               END IF
            END IF
 
            IF (IMES.EQ.0 .AND. OVERWR) THEN
*              the other checks only in case of overwriting
*              get byte pointer and array length N
               IPP = (ISLOC-1) * INFREP + INR
               IF (IPP.LE.ILPREP) THEN
                  N = IARREP(IPP)
               ELSE
                  CALL SWPI4 (2,'RD\IARREP',0,ILPREP,IPP,N)
               END IF
 
*              error checks
               IF (SINGLE .AND. REPARR(INR).EQ.'a') THEN
                  IMES = 19
               ELSE IF ((UNKNOW .OR. FIXED) .AND. 
     $          REPARR(INR).EQ.'s') THEN
                  IMES = 20
               ELSE IF (FIXED .AND. N.NE.NREQ) THEN
                  IMES = 21
               ELSE IF (N.GT.NDEC) THEN
                  IMES = 22
               END IF
            END IF
         END IF
 
         IF (IMES.EQ.0) THEN
            IF (.NOT.OVERWR) THEN
*              read data from normal data file ; initial value pointer
               IPNT = IPTDAT(IND)
               IEL  = 0
 
80             IF (IEL.LT.N) THEN
*                 read repeat value
                  IREC = 1 + IPNT/ILBUF
 
                  IF (IREC.NE.IRDAT) THEN
*                    read new record from file
                     READ (IUNDAT,REC=IREC) RDBUF,PNTDNX
                     IRDAT = IREC
                  END IF
 
*                 set position of repeat value
                  IWN = MOD(IPNT,ILBUF) / IIL
*                 get repeat value
                  IREP = IDBUF(IWN)
*                 after repeater set pointer to first data item
                  IPNT = IPNT + MAX(IXL,IIL)
*                 end of record buffer reached ?
                  IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX
 
                  IF (IREP.GT.0) THEN
*                    read data item(s) until complete value found
                     FOUND  = OFF
                     J      = 0
 
70                   IF (.NOT.FOUND) THEN
*                       read data item
                        IREC = 1 + IPNT/ILBUF
 
                        IF (IREC.NE.IRDAT) THEN
*                          read new record from file
                           READ (IUNDAT,REC=IREC) RDBUF,PNTDNX
                           IRDAT = IREC
                        END IF
 
*                       read value ; position in record
                        IWN = MOD(IPNT,ILBUF) / IXL
 
*                       get data item ; FOUND flags complete value
                        IF (VARTYP.EQ.'R') THEN
*                          single precision real
C                      write (*,*) rdbuf(iwn)
                           AD = ABS(RDBUF(IWN))
                           IF (AD.EQ.0.0D0 .OR.
     $                      (AD.GT.1.0D-38 .AND. AD.LT.1.0D+38)) THEN
*                             accept single precision value
                              DO 400 I=IEL+1,IEL+IREP
                                 RX(I) = REAL (RDBUF(IWN))
C                      write (*,*) rx(i)
400                           CONTINUE
                           ELSE IF (AD.LT.1.0D-38) THEN
*                             underflow
                              IMES = 23
                           ELSE  IF (AD.GT.1.0D+38) THEN
*                             overflow
                              IMES = 24
                           END IF
                           FOUND = ON
                        ELSE IF (VARTYP.EQ.'I') THEN
*                          integer
                           DO 401 I=IEL+1,IEL+IREP
                              IX(I) = IDBUF(IWN)
401                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'D' .OR. VARTYP.EQ.'T') THEN
*                          double precision real or date_time
                           DO 404 I=IEL+1,IEL+IREP
                              DX(I) = RDBUF(IWN)
404                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'L') THEN
*                          logical
                           DO 405 I=IEL+1,IEL+IREP
                              LX(I) = LDBUF(IWN)
405                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'C') THEN
*                          character string
                           IBN  = MOD(IWN,IIL)
                           BYTE = CHAR(MOD(IDBUF(IWN/IIL)/IPC(IBN),256))
                           IF (BYTE.NE.CHAR(0)) THEN
                              IF (J.EQ.0) THEN
*                                first character found ; empty CX
                                 DO 403 I=IEL+1,IEL+IREP
                                    CX(I) = ' '
403                              CONTINUE
                              END IF
                              J = J + 1
                              IF (J.LE.NELMT) THEN
*                                accept byte
                                 DO 402 I=IEL+1,IEL+IREP
                                    CX(I)(J:J) = BYTE
402                              CONTINUE
                              ELSE
*                                error
                                 IMES = 25
                              END IF
                           ELSE
                              FOUND = ON
                           END IF
                        END IF
 
*                       next position on file
                        IPNT = IPNT + IXL
*                       end of record buffer reached ?
                        IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX
 
*                       value found ; set element counter
                        IF (FOUND) IEL=IEL+IREP
                     GOTO 70
                     END IF
 
                  ELSE
*                    missing value: do not read from file
                     DO 408 I=IEL+1,IEL-IREP
                        IF (VARTYP.EQ.'R') THEN
*                          default single precision real
                           RX(I) = RM
                        ELSE IF (VARTYP.EQ.'I') THEN
*                          default integer
                           IX(I) = IM
                        ELSE IF (VARTYP.EQ.'D') THEN
*                          default double precision real
                           DX(I) = DM
                        ELSE IF (VARTYP.EQ.'C') THEN
*                          default character string
                           CX(I) = CM
                        ELSE IF (VARTYP.EQ.'L') THEN
*                          default logical
                           LX(I) = LM
                        ELSE IF (VARTYP.EQ.'T') THEN
*                          default date_time
                           DX(I) = TM
                        END IF
408                  CONTINUE
 
*                    increase element counter
                     IEL=IEL-IREP
                  END IF
 
*                 next repeater + value at multiple of IXLMAX bytes
                  IPNT = IXLMAX * ((IXLMAX-1 + IPNT)/IXLMAX)
*                 end of record buffer reached ?
                  IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX
               GOTO 80
               END IF
 
            ELSE
*              read data from rerun file ; initial value pointer
               IF (IPP.LE.ILPREP) THEN
                  IPNT = IPTREP(IPP)
               ELSE
                  CALL SWPI4 (2,'RD\IPTREP',0,ILPREP,IPP,IPNT)
               END IF
 
               IEL  = 0
 
100            IF (IEL.LT.N) THEN
*                 read repeat value
                  IREC = 1 + IPNT/ILBUF
 
                  IF (IREC.NE.IRREP) THEN
*                    read new record from file
                     READ (IUNREP,REC=IREC) RRBUF,PNTRNX
                     IRREP = IREC
                  END IF
 
*                 set position of repeat value
                  IWN = MOD(IPNT,ILBUF) / IIL
*                 get repeat value
                  IREP = IRBUF(IWN)
*                 after repeater set pointer to first data item
                  IPNT = IPNT + MAX(IXL,IIL)
*                 end of record buffer reached ?
                  IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX
 
                  IF (IREP.GT.0) THEN
*                    read data item(s) until complete value found
                     DREAD  = .TRUE.
                     FOUND  = OFF
                     J      = 0
 
90                   IF (.NOT.FOUND) THEN
*                       read data item
                        IREC = 1 + IPNT/ILBUF
 
                        IF (IREC.NE.IRREP) THEN
*                          read new record from file
                           READ (IUNREP,REC=IREC) RRBUF,PNTRNX
                           IRREP = IREC
                        END IF
 
*                       read value ; position in record
                        IWN = MOD(IPNT,ILBUF) / IXL
 
*                       get data item ; FOUND flags complete value
                        IF (VARTYP.EQ.'R') THEN
*                          single precision real
                           AD = ABS(RRBUF(IWN))
                           IF (AD.EQ.0.0D0 .OR.
     $                      (AD.GT.1.0D-38 .AND. AD.LT.1.0D+38)) THEN
*                             accept single precision value
                              DO 500 I=IEL+1,IEL+IREP
                                 RX(I) = REAL (RRBUF(IWN))
500                           CONTINUE
                           ELSE IF (AD.LT.1.0D-38) THEN
*                             underflow
                              IMES = 26
                           ELSE  IF (AD.GT.1.0D+38) THEN
*                             overflow
                              IMES = 27
                           END IF
                           FOUND = ON
                        ELSE IF (VARTYP.EQ.'I') THEN
*                          integer
                           DO 501 I=IEL+1,IEL+IREP
                              IX(I) = IRBUF(IWN)
501                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'D' .OR. VARTYP.EQ.'T') THEN
*                          double precision real or date_time
                           DO 504 I=IEL+1,IEL+IREP
                              DX(I) = RRBUF(IWN)
504                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'L') THEN
*                          logical
                           DO 505 I=IEL+1,IEL+IREP
                              LX(I) = LRBUF(IWN)
505                        CONTINUE
                           FOUND   = ON
                        ELSE IF (VARTYP.EQ.'C') THEN
*                          character string
                           IBN  = MOD(IWN,IIL)
                           BYTE = CHAR(MOD(IRBUF(IWN/IIL)/IPC(IBN),256))
                           IF (BYTE.NE.CHAR(0)) THEN
                              IF (J.EQ.0) THEN
*                                first character found ; empty CX
                                 DO 503 I=IEL+1,IEL+IREP
                                    CX(I) = ' '
503                              CONTINUE
                              END IF
                              J = J + 1
                              IF (J.LE.NELMT) THEN
*                                accept byte
                                 DO 502 I=IEL+1,IEL+IREP
                                    CX(I)(J:J) = BYTE
502                              CONTINUE
                              ELSE
*                                error
                                 IMES = 28
                              END IF
                           ELSE
                              FOUND = ON
                           END IF
                        END IF
 
*                       next position on file
                        IPNT = IPNT + IXL
*                       end of record buffer reached ?
                        IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX
 
*                       value found ; set element counter
                        IF (FOUND) IEL=IEL+IREP
                     GOTO 90
                     END IF
 
                  ELSE
*                    missing value: do not read from file
                     DREAD = .FALSE.
                     DO 508 I=IEL+1,IEL-IREP
                        IF (VARTYP.EQ.'R') THEN
*                          default single precision real
                           RX(I) = RM
                        ELSE IF (VARTYP.EQ.'I') THEN
*                          default integer
                           IX(I) = IM
                        ELSE IF (VARTYP.EQ.'D') THEN
*                          default double precision real
                           DX(I) = DM
                        ELSE IF (VARTYP.EQ.'C') THEN
*                          default character string
                           CX(I) = CM
                        ELSE IF (VARTYP.EQ.'L') THEN
*                          default logical
                           LX(I) = LM
                        ELSE IF (VARTYP.EQ.'T') THEN
*                          default date_time
                           DX(I) = TM
                        END IF
508                  CONTINUE
 
*                    increase element counter
                     IEL=IEL-IREP
                  END IF
 
*                 next repeater + value at multiple of IXLMAX bytes
                  IPNT = IXLMAX * ((IXLMAX-1 + IPNT)/IXLMAX)
*                 end of record buffer reached ?
                  IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX
               GOTO 100
               END IF
 
*              count overwriting and write message
               IRCHK(INR) = IRCHK(INR) + 1
               IF (LOGR.AND.IMES.EQ.0) THEN
*                 write report to logfile
                  IF (.NOT.REPMES) THEN
                     WRITE (IULR,'(3X,2A,I5,A)')
     $                'Variable values have been overwritten by ',
     $                'rerun set',ISLOC,':'
                     REPMES = ON
                  END IF
 
                  IF (DREAD) THEN
*                    data have been read
                     ARROW = ' ----->'
                  ELSE
*                    missing value
                     ARROW = ' -DEF->'
                  END IF
 
                  WRITE (LINE,'(3X,2A)') LXNM31(1:ILX),ARROW
                  PPP    = ' '
                  I      = 0
                  IVP    = ILEN(LINE)
                  IVL    =  LEN(LINE) - 1
                  FILLED = OFF
 
110               IF (I.LT.N .AND. .NOT.FILLED) THEN
*                    write next value to help string
                     IF (VARTYP.EQ.'D'.OR.VARTYP.EQ.'T') THEN
                        WRITE (VALUE,'(1P,G12.4)') DX(I+1)
                     ELSE IF (VARTYP.EQ.'R') THEN
                        WRITE (VALUE,'(1P,G12.4)') RX(I+1)
                     ELSE IF (VARTYP.EQ.'I') THEN
                        WRITE (VALUE,'(I12)') IX(I+1)
                     ELSE IF (VARTYP.EQ.'C') THEN
                        J = ILEN (CX(I+1))
                        J = MIN (J,10)
                        VALUE = CHAR(39)//CX(I+1)(1:J)//CHAR(39)
                     ELSE IF (VARTYP.EQ.'L') THEN
                        IF (LX(I+1)) THEN
                           VALUE = '.TRUE.'
                        ELSE
                           VALUE = '.FALSE.'
                        END IF
                     END IF
 
*                    length of text and position of last character
                     IV1 = ISTART (VALUE)
                     IV2 = ILEN (VALUE)
                     IPN = IVP + 2 + IV2 - IV1
 
                     FILLED = IPN.GT.IVL
                     IF (.NOT.FILLED) THEN
*                       add value to message
                        LINE(IVP+2:IPN) = VALUE(IV1:IV2)
                        I   = I + 1
                        IVP = IPN
                        IF (I.LT.N) THEN
*                          add comma
                           IVP = IVP + 1
                           LINE(IVP:IVP) = ','
                        END IF
                     END IF
                  GOTO 110
                  END IF
 
*                 write replacement message to log file
                  IF (I.LT.N) PPP='...'
                  WRITE (IULR,'(2A)') LINE(1:IVP),PPP
               END IF
            END IF
 
*           return number of values on file when requested to do so
            IF (UNKNOW) NREQ = N
         END IF
 
         IF (IMES.GT.0) THEN
*           error message
            LOGF = (REPL.AND.LOGR) .OR. LOGD
            IF (REPL.AND.LOGR) THEN
*              message to rerun logfile
               IUL = IULR
            ELSE IF (LOGD) THEN
*              message to data file logfile
               IUL = IULD
            END IF
 
            IF (SINGLE) THEN
*              a single value into a non-array variable
               WHAT   = 'a single value'
               TARGET = ' variable '
               ISTREC = 2
               RECORD(1) = ' '
            ELSE IF (UNKNOW) THEN
*              requested number of values unspecified
               WHAT   = 'an unspecified number of values'
               TARGET = ' array '
               ISTREC = 1
            ELSE IF (FIXED) THEN
*              number of values specified
               WRITE (WHAT,'(A,I6,A)') 'precisely',NREQ,' values'
               TARGET = ' array '
               ISTREC = 1
            END IF
            ILA = ILEN (ASKED(IVT))
            ILW = ILEN (WHAT)
            ILT = ILEN (TARGET) + 1
 
*           error message
            RECORD(ISTREC) =
     $       ERRTXT//CALPRG//', attempts to read'
            RECORD(ISTREC+1) =
     $       WHAT(1:ILW)//' from file '//DFIL(1:ILD)
            RECORD(ISTREC+2) =
     $       'into '//ASKED(IVT)(1:ILA)//TARGET(1:ILT)//LXNM31(1:ILX)
            IF (.NOT.SINGLE) WRITE (RECORD(4),'(A,I5,A)')
     $       '(with declared length',NDEC,') :'
            WRITE (*,'(4(/,1X,A))') RECORD
            IF (LOGF) WRITE (IUL,'(6(/,1X,A))')
     $       'INPUT ERROR','===========',RECORD
 
*           in case of rerun value
            IF (OVERWR) THEN
               RECORD(1) =
     $    '   | RDDATA was instructed to replace data file contents by'
               WRITE (RECORD(2),'(A,I5,3A)')
     $    '   | value(s) in set',ISLOC,' of rerun file ',RFIL(1:ILR),'.'
               RECORD(3) =
     $    '   | Data are replaced only if variable type is correct '
               RECORD(4) =
     $    '   | on both data file and rerun file.'
               WRITE (*,'(1X,A,3(/,1X,A))') RECORD
               IF (LOGF) WRITE (IUL,'(1X,A,3(/,1X,A))') RECORD
            END IF
 
*           stop
            IF (LOGF) WRITE (IUL,'(1X,2A)')
     $       'ERROR in RDDATA: ',MESSAG(IMES)
            CALL FATALERR ('RDDATA',MESSAG(IMES))
         END IF
 
 
      ELSE IF (ITASK.EQ.8) THEN
*        check presence of variable name on data file ; error check
         IF (.NOT.INIDAT) CALL FATALERR ('RDINQR','no data file active')
 
*        get local copy of significant part of name in uppercase
         ISX    = MAX (1,ISTART (XNAME))
         ILX    = MAX (1,ILEN (XNAME))
         LXNM31 = XNAME(ISX:ILX)
         CALL UPPERC (LXNM31)
 
*        check data file index
         IS  = IFINDC (DATLIS,ILNDAT,1,INFDAT,LXNM31)
 
 
      ELSE IF (ITASK.EQ.9) THEN
*        delete all known "TMP files"
         IRECL = ILBUF + IIL
 
         IF (INIDAT) THEN
*           close old unit if left open
            INQUIRE (UNIT=IUNDAT, OPENED=OPUNIT)
            IF (OPUNIT) CLOSE(IUNDAT)
         END IF
 
         IF (REPL) THEN
*           close old unit if left open
            INQUIRE (UNIT=IUNREP, OPENED=OPUNIT)
            IF (OPUNIT) CLOSE (IUNREP)
         END IF
 
         IF (INIDAT) THEN
*           close temporary data files
            IF (LOGD) WRITE (IULD,'(1X,A)') ' '
            DO 120 I=1,INFILS
*              get name of temporary file ; it exists ?
               CALL EXTENS (FILLIS(I),'TMP',1,DATTMP)
               ILTMP = ILEN (DATTMP)
               TEMP  = FLEXIST (DATTMP(1:ILTMP))
 
               IF (TEMP) THEN
                  CALL FOPENG (IUNIT,DATTMP,'OLD','UD',IRECL,' ')
                  CLOSE (IUNIT,STATUS='DELETE')
                  IF (LOGD) WRITE (IULD,'(1X,3A)') 'Temporary file ',
     $             DATTMP(1:ILTMP),' deleted by RDDTMP'
               END IF
120         CONTINUE
 
*           disable variable reading
            INIDAT = OFF
         END IF
 
         IF (REPL) THEN
*           delete rerun TMP file if it exists
            CALL EXTENS (RFIL,'TMP',1,REPTMP)
            ILTMP = ILEN (REPTMP)
            TEMP  = FLEXIST (REPTMP(1:ILTMP))
            IF (TEMP) THEN
               CALL FOPENG (IUNIT,REPTMP,'OLD','UD',IRECL,' ')
               CLOSE (IUNIT,STATUS='DELETE')
               IF (LOGR) WRITE (IULR,'(1X,3A)') 'Temporary file ',
     $          REPTMP(1:ILTMP),' deleted by RDDTMP'
            END IF
 
*           disable reruns
            REPL = OFF
            CALL AMBUSY (1,'RDFROM',0)
         END IF
 
      ELSE IF (ITASK.EQ.10) THEN
*        set return value for missing values
         IF (VARTYP.EQ.'R') THEN
*           default single precision real
            RM = RX(1)
         ELSE IF (VARTYP.EQ.'I') THEN
*           default integer
            IM = IX(1)
         ELSE IF (VARTYP.EQ.'D') THEN
*           default double precision real
            DM = DX(1)
         ELSE IF (VARTYP.EQ.'C') THEN
*           default character string
            CM = CX(1)
         ELSE IF (VARTYP.EQ.'L') THEN
*           default logical
            LM = LX(1)
         ELSE IF (VARTYP.EQ.'T') THEN
*           default date_time
            TM = DX(1)
         END IF
 
      ELSE IF (ITASK.EQ.11) THEN
 
*        assign default missing values
         RM = RM_D
         IM = IM_D
         DM = DM_D
         CM = CM_D
         LM = LM_D
         TM = TM_D
 
      ELSE
         CALL FATALERR ('RDDATA','illegal ITASK value')
      END IF
 
      RETURN
      END
**==RDDTMP.FOR  
      SUBROUTINE RDDTMP (IUNIT)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IUNIT
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
      CALL RDDATA (9,'RDDTMP',IUNIT,0,C,IS,C,C,D,R,I,C,L,1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDERR.FOR   
      SUBROUTINE RDERR (ITASK,MESSAG)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK
      CHARACTER*(*) MESSAG
 
**    Include files
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rdtblinf.inc'
      INCLUDE 'rdfilinf.inc'
      INCLUDE 'rdstainf.inc'
      INCLUDE 'rderrinf.inc'
      INCLUDE 'lextokin.inc'
 
*     local variables
      CHARACTER*30 PTRSTR, ERBUF
      CHARACTER LMESS*40, FORM1*26
      PARAMETER (FORM1='(1X,I4,1X,A,1X,3A,/,T49,A)')
      INTEGER I, I1, I2, IH, ILEN
      LOGICAL THERE
 
      SAVE
 
      IF (INERR.EQ.0 .AND. INWAR.EQ.0) THEN
*        first message received ; file handling
         IF (TOSCR) WRITE (*,'(/,1X,2A,/,2(/,T2,A,T7,A,T48,A))')
     $    'Error(s) or Warning(s) while reading from ',
     $     FILEIN(1:ILFILE),
     $    'Line','Error text','Somewhere here',
     $    '====','==========','=============='
         IF (TOLOG) THEN
*           logfile present ?
            INQUIRE (UNIT=IULOG,OPENED=THERE)
            IF (.NOT.THERE)
     $       CALL FOPENS (IULOG,'RDINDX.LOG','NEW','DEL')
            WRITE (IULOG,'(/,1X,2A,/,2(/,T2,A,T7,A,T48,A))')
     $        'Error(s) or Warning(s) while reading from ',
     $         FILEIN(1:ILFILE),
     $        'Line','Error text','Somewhere here',
     $        '====','==========','=============='
         END IF
      END IF
 
      IF (ITASK.EQ.2 .OR. ITASK.EQ.3) THEN
*        write message with pointer string
         IF (ITASK.EQ.2) INERR = INERR + 1
         IF (ITASK.EQ.3) INWAR = INWAR + 1
 
*        truncate message
         LMESS = MESSAG
         CALL UPPERC (LMESS(1:1))
 
*        display line with pointer string
         I2 = MIN (IP+5,STBLEN-1)
 
         IF (I2.GE.1) THEN
            I1     = MAX (I2-(LEN(ERBUF)-1),1)
            ERBUF  = STBUF(I1:I2)
            PTRSTR = ' '
            DO 10 I=I1,I2
               IH = I-I1+1
               IF (I.EQ.ISP .OR. I.EQ.IP) THEN
                  PTRSTR(IH:IH) = '^'
               ELSE IF (I.GT.ISP .AND. I.LT.IP) THEN
                  IF (IH.GT.1) THEN
                     PTRSTR(IH:IH) = '-'
                  ELSE
                     PTRSTR(IH:IH) = '<'
                  END IF
               END IF
10          CONTINUE
            IF (TOSCR) WRITE (*,FORM1)
     &          RECNO,LMESS,'[',ERBUF(1:I2-I1+1),']',PTRSTR(1:I2-I1+1)
            IF (TOLOG) WRITE (IULOG,FORM1)
     &          RECNO,LMESS,'[',ERBUF(1:I2-I1+1),']',PTRSTR(1:I2-I1+1)
         ELSE
            IF (TOSCR) WRITE (*    ,FORM1) RECNO,LMESS
            IF (TOLOG) WRITE (IULOG,FORM1) RECNO,LMESS
         END IF
 
      ELSE IF (ITASK.EQ.4) THEN
*        global error ; message includes line number
         INERR = INERR + 1
 
         I1 = ILEN (TBLNAM(ICOL))
         I2 = ILEN (MESSAG)
 
         IF (TOSCR) WRITE (*,'(1X,I5,4A,/)')
     $      TBLLIN(ICOL),'  Variable ',
     $      TBLNAM(ICOL)(1:I1),': ',MESSAG(1:I2)
         IF (TOLOG) WRITE (IULOG,'(1X,I5,4A,/)')
     $      TBLLIN(ICOL),'  Variable ',
     $      TBLNAM(ICOL)(1:I1),': ',MESSAG(1:I2)
 
      ELSE
         CALL FATALERR ('RDERR','internal error')
      END IF
 
      RETURN
      END
**==RDERRI.FOR  
      SUBROUTINE RDERRI (xFILEIN,xIUDF,xIULOG,xTOSCR,xTOLOG)
 
      IMPLICIT NONE
 
*     Formal parameters
      CHARACTER*(*) xFILEIN
      INTEGER       xIUDF,xIULOG
      LOGICAL       xTOSCR, xTOLOG
 
*     Local variables
      INCLUDE 'rdfilinf.inc'
      INCLUDE 'rdstainf.inc'
      INCLUDE 'rderrinf.inc'
 
      INTEGER ILEN
 
      SAVE
 
      FILEIN = xFILEIN
      ILFILE = ILEN (FILEIN)
      IUDF   = xIUDF
      IULOG  = xIULOG
      TOSCR  = xTOSCR
      TOLOG  = xTOLOG
      INERR   = 0
      INWAR   = 0
 
      RETURN
      END
**==RDFCHA.FOR  
      SUBROUTINE RDFCHA (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
      CHARACTER*(*) XNAME,X
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80          CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
      CALL RDDATA (7,'RDFCHA',0,0,' ',IS,XNAME,'C',
     $             D,R,I,X,L,1,1,1,ILDEC,1,IVALS,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDFDOR.FOR  
      SUBROUTINE RDFDOR (XNAME,XMIN,XMAX,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      DOUBLE PRECISION X,XMIN,XMAX
      INTEGER ILDEC,IVALS
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS,I1,ILX,ILEN
      DOUBLE PRECISION XL
      CHARACTER Q*1, LXNAME*31
      SAVE
 
      DATA Q /''''/
 
      CALL RDDATA (7,'RDFDOR',0,0,' ',IS,XNAME,'D',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IVALS,
     $             DM,RM,IM,CM,LM)
 
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
 
      DO 10 I1=1,IVALS
         XL = X(I1)
         IF (XMAX.LT.XMIN) THEN
            WRITE (*,'(1X,4A)')
     &        'ERROR in RDFDOR: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            CALL FATALERR (' ',' ')
         ELSE IF ((XL.LT.XMIN.OR.XL.GT.XMAX).AND.XL.NE.DM) THEN
            WRITE (*,'(1X,4A,I3,3A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &        'ERROR in RDFDOR: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            CALL FATALERR (' ',' ')
         END IF
10    CONTINUE
 
      RETURN
      END
**==RDFDOU.FOR  
      SUBROUTINE RDFDOU (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
      DOUBLE PRECISION X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
      CALL RDDATA (7,'RDFDOU',0,0,' ',IS,XNAME,'D',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IVALS,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDFINR.FOR  
      SUBROUTINE RDFINR (XNAME,XMIN,XMAX,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
      INTEGER X,XMIN,XMAX
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER               IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS,I1,ILX,ILEN
      INTEGER XL
      CHARACTER Q*1, LXNAME*31
      SAVE
 
      DATA Q /''''/
 
      CALL RDDATA (7,'RDFINR',0,0,' ',IS,XNAME,'I',
     $             D,R,X,C,L,1,1,ILDEC,1,1,IVALS,
     $             DM,RM,IM,CM,LM)
 
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
 
      DO 10 I1=1,IVALS
         XL = X(I1)
         IF (XMAX.LT.XMIN) THEN
            WRITE (*,'(1X,4A)')
     &        'ERROR in RDFINR: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            CALL FATALERR (' ',' ')
         ELSE IF ((XL.LT.XMIN.OR.XL.GT.XMAX).AND.XL.NE.IM) THEN
            WRITE (*,'(1X,4A,I3,3A,/,T19,A,I8,A,I8,A,I8,A)')
     &        'ERROR in RDFINR: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            CALL FATALERR (' ',' ')
         END IF
10    CONTINUE
 
      RETURN
      END
**==RDFINT.FOR  
      SUBROUTINE RDFINT (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS,X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER               IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
      CALL RDDATA (7,'RDFINT',0,0,' ',IS,XNAME,'I',
     $             D,R,X,C,L,1,1,ILDEC,1,1,IVALS,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDFLOG.FOR  
      SUBROUTINE RDFLOG (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
      LOGICAL X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL               LM
 
*     other
      INTEGER IS
      SAVE
 
      CALL RDDATA (7,'RDFLOG',0,0,' ',IS,XNAME,'L',
     $             D,R,I,C,X,1,1,1,1,ILDEC,IVALS,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDFREA.FOR  
      SUBROUTINE RDFREA (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
      REAL X
      DIMENSION X(ILDEC)
      CHARACTER*(*) XNAME
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL                  RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
      CALL RDDATA (7,'RDFREA',0,0,' ',IS,XNAME,'R',
     $             D,X,I,C,L,1,ILDEC,1,1,1,IVALS,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDFRER.FOR  
      SUBROUTINE RDFRER (XNAME,XMIN,XMAX,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X,XMIN,XMAX
      INTEGER ILDEC,IVALS
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL                  RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS,I1,ILX,ILEN
      REAL XL
      CHARACTER Q*1, LXNAME*31
      SAVE
 
      DATA Q /''''/
 
      CALL RDDATA (7,'RDFRER',0,0,' ',IS,XNAME,'R',
     $             D,X,I,C,L,1,ILDEC,1,1,1,IVALS,
     $             DM,RM,IM,CM,LM)
 
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
 
      DO 10 I1=1,IVALS
         XL = X(I1)
         IF (XMAX.LT.XMIN) THEN
            WRITE (*,'(1X,4A)')
     &        'ERROR in RDFRER: Range not valid of identifier ',
     &         Q,LXNAME(1:ILX),Q
            CALL FATALERR (' ',' ')
         ELSE IF (XL.LT.XMIN.OR.XL.GT.XMAX) THEN
            WRITE (*,'(1X,4A,I3,3A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &        'ERROR in RDFRER: Range error of identifier ',
     &         Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &        'value =',XL,', range = [',XMIN,',',XMAX,']'
            CALL FATALERR (' ',' ')
         END IF
10    CONTINUE
 
      RETURN
      END
**==RDFROM.FOR  
      SUBROUTINE RDFROM (IS,FATAL)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IS
      LOGICAL FATAL
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IFATAL
      SAVE
 
      IF (FATAL) THEN
         IFATAL = 1
      ELSE
         IFATAL = 0
      END IF
 
*     call for (new) set
      CALL RDDATA (2,'RDFROM',0,0,' ',IS,' ',' ',D,R,I,C,L,
     $             1,1,1,1,1,IFATAL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDFTIM.FOR  
      SUBROUTINE RDFTIM (XNAME,X,ILDEC,IVALS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IVALS
      DOUBLE PRECISION X
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
 
**    local variables ; dummy set
      DOUBLE PRECISION      DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IS
      SAVE
 
      CALL RDDATA (7,'RDFTIM',0,0,' ',IS,XNAME,'T',
     $             X,R,I,C,L,ILDEC,1,1,1,1,IVALS,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDINAR.FOR  
      LOGICAL FUNCTION RDINAR (VAR_NAME)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) VAR_NAME
 
**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
 
      INCLUDE 'rddata.inc'
 
*     Local variables
      INTEGER I1,IFINDC
      CHARACTER*1  SCAR
      CHARACTER*31 VAR_NAME_UC
      SAVE
 
      VAR_NAME_UC = VAR_NAME
      CALL UPPERC (VAR_NAME_UC)
 
      IF (ISCOM.EQ.0) THEN
*        return data types from active data file
 
         IF (INFDAT.EQ.0) CALL FATALERR
     &      ('RDINAR','no data file active')
 
         I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
         IF (I1.EQ.0) CALL FATALERR ('RDINAR','variable not found')
         SCAR = DATARR(I1)
 
      ELSE IF (ISCOM.GT.0) THEN
 
*        return data types from rerun file
 
         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINAR','no rerun file active')
 
         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
         IF (I1.EQ.0) CALL FATALERR ('RDINAR','variable not found')
         SCAR = REPARR(I1)
 
      END IF
 
      RDINAR = SCAR .EQ. 'a'
 
      RETURN
      END
**==RDINDT.FOR  
      SUBROUTINE RDINDT (VAR_NAME,DATA_TYPE)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) VAR_NAME, DATA_TYPE
 
**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
      INCLUDE 'rddata.inc'
 
*     Local variables
      INTEGER I1,IFINDC
      CHARACTER*31 VAR_NAME_UC
      SAVE
 
      VAR_NAME_UC = VAR_NAME
      CALL UPPERC (VAR_NAME_UC)
 
      IF (ISCOM.EQ.0) THEN
*        return data types from active data file
 
         IF (INFDAT.EQ.0) CALL FATALERR ('RDINDT','no data file active')
 
         I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
         IF (I1.EQ.0) CALL FATALERR ('RDINDT','variable not found')
         DATA_TYPE = DATTYP(I1)
 
      ELSE IF (ISCOM.GT.0) THEN
 
*        return data types from rerun file
 
         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINDT','no rerun file active')
 
         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
         IF (I1.GT.0) THEN
            DATA_TYPE = REPTYP(I1)
         ELSE
            I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
            IF (I1.EQ.0) CALL FATALERR ('RDINDT','variable not found')
            DATA_TYPE = DATTYP(I1)
         END IF
      END IF
 
      RETURN
      END
**==RDINDX.FOR  
      SUBROUTINE RDINDX (IERR,IUNIT,SETS,TOSCRX,TOLOGX,IUL,DATFIL,
     $   TMPFIL,NAMLIS,VARTYP,VARRAY,ILIND,
     $   IARLEN,INDPNT,ILPNT,INFND,INSETS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IERR,IUNIT,IUL,ILIND,IARLEN,INDPNT,ILPNT,INFND,INSETS
      CHARACTER DATFIL*(*),TMPFIL*(*),NAMLIS*(*),VARTYP*1,VARRAY*1
      DIMENSION NAMLIS(ILIND),VARTYP(ILIND),VARRAY(ILIND)
      DIMENSION IARLEN(ILPNT),INDPNT(ILPNT)
      LOGICAL SETS,TOSCRX,TOLOGX
 
**    include files
      INCLUDE 'rderrinf.inc'
      INCLUDE 'rdtblinf.inc'
      INCLUDE 'rdfilinf.inc'
      INCLUDE 'rdrecinf.inc'
 
*     local variables
      INTEGER IDUM, NROW, INFLD, IVOLD, LNAME
      CHARACTER CDUM*1
      LOGICAL EOF, STCOMP
 
*     functions called
      INTEGER IFINDC, ILEN
 
      SAVE
 
      IDUM = 0
 
*     maximum variable name length
      INLMX = LEN (NAMLIS(1))
      IF (INLMX.GT.31) CALL FATALERR
     &   ('RDINDX','variable names too long')
 
*     file handling to common, initialize error system
      IUDF   = IUNIT + 1
      CALL RDERRI (DATFIL,IUDF,IUL,TOSCRX,TOLOGX)
      ILFILE = ILEN (DATFIL)
 
*     initialize record read system
      CALL RECREAD_INIT (IUDF,DATFIL)
      RECNO = 0
 
*     initialize Lexical analysis system
      CALL RDLEX (1)
 
      CALL RDTMP1 (1,IUNIT,TMPFIL)
 
*     initialize loop over tables
      INFLD  = 0
      INFND  = 0
      INSETS = 0
      EOF    = .FALSE.
      STCOMP = .TRUE.
 
 
10    IF (.NOT.EOF) THEN
*        analyze next table
         CALL RDSCTB (EOF)
 
*        number of rows
         NROW = NVAL / MAX (1,NCOL)
 
*        check names and types
         DO 20 ICOL=1,INAMES
*           increase output field counter
            INFLD = INFLD + 1
 
*           name previously used ?
            LNAME = IFINDC
     $       (NAMLIS,ILIND,1,INFND,TBLNAM(ICOL)(1:INLMX))
 
            IF (LNAME.EQ.0 .AND. INSETS.LE.1) THEN
*              accept new name in index
               INFND = INFND + 1
               IF (INSETS.EQ.0) INSETS = 1
               IF (INFND.GT.ILIND) THEN
*                 too many different names on file
                  CALL RDERR (4,'exceeded maximum number of names')
                  IF (SETS) WRITE (*,'(/,2A,/,A)')
     $            ' Too many names occur in the sets of rerun file ',
     $              DATFIL(1:ILFILE),
     $            ' Increase the value of ILNREP in RDMACHIN.INC.'
                  IF (.NOT.SETS) WRITE (*,'(/,2A,/,A)')
     $            ' Too many names occur in the data file ',
     $              DATFIL(1:ILFILE),
     $            ' Increase the value of ILNDAT in RDMACHIN.INC.'
                  CALL FATALERR (' ',' ')
               END IF
               NAMLIS(INFND) = TBLNAM(ICOL)
               VARTYP(INFND) = ' '
               VARRAY(INFND) = ' '
               IVOLD = INFND
               LNAME = INFND
 
            ELSE IF (LNAME.GT.0 .AND. SETS) THEN
*              variable name was previously found
               IF (LNAME.EQ.1 .AND. STCOMP) THEN
*                 start of new set
                  INSETS = INSETS + 1
               ELSE IF (LNAME.NE.IVOLD+1) THEN
*                 error in variable name order
                  CALL RDERR (4,'wrong name order in set')
               END IF
               STCOMP = LNAME.EQ.INFND
               IVOLD  = LNAME
            ELSE IF (LNAME.GT.0) THEN
*              old name illegal (no sets)
               CALL RDERR (4,'name already used')
            ELSE
*              new name illegal (in set)
               CALL RDERR (4,'name does not occur in previous set(s)')
            END IF
 
*           array length
            IF (.NOT.SETS) THEN
               IARLEN(INFLD) = NROW
               INDPNT(INFLD) = TBLPNT(ICOL)
            ELSE
               IF (INFLD.LE.ILPNT) THEN
                  IARLEN(INFLD) = NROW
                  INDPNT(INFLD) = TBLPNT(ICOL)
               ELSE
                  CALL SWPI4 (1,'RD\IARREP',IUNIT+2,
     &                        ILPNT,INFLD,NROW)
                  CALL SWPI4 (1,'RD\IPTREP',IUNIT+3,
     &                        ILPNT,INFLD,TBLPNT(ICOL))
               END IF
            END IF
 
            IF (VARTYP(LNAME) .EQ. ' ') THEN
*              variable not yet typed ; accept column type and array flag
               VARTYP(LNAME) = TBLTYP(ICOL)
               VARRAY(LNAME) = TBARAY
            ELSE IF (TBLTYP(ICOL).NE.' ') THEN
*              variable and column are both typed ; check type and array flag
               IF (TBLTYP(ICOL).NE.VARTYP(LNAME))
     $          CALL RDERR (4,'data type is not consistent')
               IF (VARRAY(LNAME).NE.TBARAY) 
     $          CALL RDERR (4,'scalar / array inconsistency')
            END IF
20       CONTINUE
      GOTO 10
      END IF
 
*     final call's to RDTMP1 (delete TMP in case of errors)
      CALL RDTMP1 (9,IDUM,CDUM)
 
*     set number of errors
      IERR = INERR
 
      RETURN
      END
**==RDINIT.FOR  
      SUBROUTINE RDINIT (IUNIT,IULOG,DATFIL)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IUNIT,IULOG
      CHARACTER*(*) DATFIL
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
*     initialize
      IL = 0
      CALL RDDATA (3,'RDINIT',IUNIT,IULOG,DATFIL,IS,' ',' ',
     $             D,R,I,C,L,1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDINLV.FOR  
      SUBROUTINE RDINLV (SETFLAG,VARLIS,VARLIS_MN,VARLIS_AN)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      LOGICAL SETFLAG
      INTEGER VARLIS_MN,VARLIS_AN
      CHARACTER*(*) VARLIS(VARLIS_MN)
 
**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
      INCLUDE 'rddata.inc'
 
*     Local variables
      INTEGER I1,I2,I3,ILEN
      SAVE
 
      IF (.NOT.SETFLAG.OR.ISCOM.EQ.0) THEN
 
*        return list of names from active datafile
 
         IF (INFDAT.EQ.0) CALL FATALERR ('RDINLV','no data file active')
         IF (INFDAT.GT.VARLIS_MN) CALL FATALERR
     &      ('RDINLV','external list does not have enough elements')
 
         I3 = LEN (VARLIS(1))
 
         DO I1=1,INFDAT
            I2 = ILEN (DATLIS (I1))
            IF (I2.LE.I3) THEN
*              name in internal table not longer than external string
               VARLIS(I1) = DATLIS(I1)(1:I2)
            ELSE
               CALL FATALERR ('RDINLV',
     &         'variable name to long for external list')
            END IF
         END DO
 
         VARLIS_AN = INFDAT
 
      ELSE IF (SETFLAG) THEN
 
*        return list of names from rerun file
 
         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINLV','no rerun file active')
         IF (INFREP.GT.VARLIS_MN) CALL FATALERR
     &      ('RDINLV','external list does not have enough elements')
 
         I3 = LEN (VARLIS(1))
 
         DO I1=1,INFREP
            I2 = ILEN (REPLIS (I1))
            IF (I2.LE.I3) THEN
*              name in internal table not longer than external string
               VARLIS(I1) = REPLIS(I1)(1:I2)
            ELSE
               CALL FATALERR ('RDINLV',
     &         'variable name to long for external list')
            END IF
         END DO
 
         VARLIS_AN = INFREP
 
      END IF
 
      RETURN
      END
**==RDINNE.FOR  
      SUBROUTINE RDINNE (VAR_NAME,NO_EL)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER NO_EL
      CHARACTER*(*) VAR_NAME
 
**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
      INCLUDE 'rddata.inc'
 
*     Local variables
      INTEGER I1,IFINDC,IP
      CHARACTER*31 VAR_NAME_UC
      SAVE
 
      VAR_NAME_UC = VAR_NAME
      CALL UPPERC (VAR_NAME_UC)
 
      IF (ISCOM.EQ.0) THEN
 
*        return number of elements from active datafile
 
         IF (INFDAT.EQ.0) CALL FATALERR ('RDINNE','no data file active')
 
         I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
         IF (I1.EQ.0) CALL FATALERR ('RDINNE','variable not found')
         IF (DATARR(I1).EQ.'s') CALL FATALERR ('RDINNE',
     $    'variable on data file is scalar')
 
         NO_EL = IARDAT(I1)
 
      ELSE IF (ISCOM.GT.0) THEN
 
*        return number of elements from active set in rerun file
 
         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINNE','no rerun file active')
 
         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
 
         IF (I1.GT.0) THEN
*           variable in rerun list, get number of elements from rerun
*           file
            IF (REPARR(I1).EQ.'s') CALL FATALERR ('RDINNE',
     $       'variable on rerun file is scalar')
 
            IP = INFREP*(ISCOM-1)+I1
            IF (IP.LE.ILPREP) THEN
               NO_EL = IARREP(IP)
            ELSE
               CALL SWPI4 (2,'RD\IARREP',-99,ILPREP,IP,NO_EL)
            END IF
         ELSE
*           variable not in rerun list, get number of elements from
*           normal data file
            I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
            IF (I1.EQ.0) CALL FATALERR ('RDINNE','variable not found')
            IF (DATARR(I1).EQ.'s') CALL FATALERR ('RDINNE',
     $       'variable on data file is scalar')
            NO_EL = IARDAT(I1)
         END IF
      END IF
 
      RETURN
      END
**==RDINQR.FOR  
      LOGICAL FUNCTION RDINQR (XNAME)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IFLAG,IL
      SAVE
 
*     inquire call
      CALL RDDATA (8,'RDINQR',0,0,C,IFLAG,XNAME,C,D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
*     set flag
      RDINQR = IFLAG.GT.0
 
      RETURN
      END
**==RDINQR2.FOR 
      LOGICAL FUNCTION RDINQR2 (VAR_NAME)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) VAR_NAME
 
**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
 
      INCLUDE 'rddata.inc'
 
*     Local variables
      INTEGER I1,IFINDC
      CHARACTER*31 VAR_NAME_UC
      SAVE
 
      VAR_NAME_UC = VAR_NAME
      CALL UPPERC (VAR_NAME_UC)
 
      IF (ISCOM.EQ.0) THEN
*        return data types from active data file
 
         IF (INFDAT.EQ.0) CALL FATALERR
     &      ('RDINQR2','no data file active')
 
         I1 = IFINDC (DATLIS,ILNDAT,1,INFDAT,VAR_NAME_UC)
 
      ELSE IF (ISCOM.GT.0) THEN
 
*        return data types from rerun file
 
         IF (INFREP.EQ.0) CALL FATALERR
     &      ('RDINQR2','no rerun file active')
 
         I1 = IFINDC (REPLIS,ILNREP,1,INFREP,VAR_NAME_UC)
 
      END IF
 
      RDINQR2 = I1.GT.0
 
      RETURN
      END
**==RDLEX.FOR   
      SUBROUTINE RDLEX (ITASK)
      IMPLICIT NONE
 
*     Formal parameters
      INTEGER ITASK
 
**    define long buffer to hold byte information
      INCLUDE 'lextokin.inc'
      INCLUDE 'rdrecinf.inc'
 
*     Syntax table declarations
      INCLUDE 'rdjdec.gin'
      INCLUDE 'rdndec.gin'
 
*     local variables
      INTEGER CODES(RECLEN)
      INTEGER I1, I2, ISTATE, TMPP
      INTEGER MNSTCK,NSTCK,ISTCK
      PARAMETER (MNSTCK=200)
      INTEGER BPSTCK(MNSTCK),EPSTCK(MNSTCK),TKSTCK(MNSTCK)
 
*     values
      INCLUDE 'rddecinf.inc'
      INTEGER INSTCK(MNSTCK)
      DOUBLE PRECISION RESTCK(MNSTCK),TISTCK(MNSTCK)
      LOGICAL LOSTCK(MNSTCK)
      CHARACTER*1 VTSTCK(MNSTCK)
 
      SAVE
 
*     Jump table data
      INCLUDE 'rdjdat.gin'
      INCLUDE 'rdndat.gin'
 
      IF (ITASK.EQ.1) THEN
*        initialize
*        formal parameters
         ISP    = 0
         IP     = 0
         TOKEN  = -1
 
*        internal variables
         ISTCK  = 0
         NSTCK  = 0
         RETURN
 
      END IF
 
      IF (ITASK.EQ.2) THEN
*        prepare record for parsing
 
*        add carriage return code for end_of_line character to the
*        right of rightmost character
         STBLEN         = STBLEN+1
         STBUF2(STBLEN) = CHAR (13)
 
*        significant record should be available, convert every
*        character in record to corresponding code
 
         DO I2=1,STBLEN
            ASCII(I2) = ICHAR (STBUF2(I2))
            CODES(I2) = ITYPE(ASCII(I2))
         END DO
 
*        common variables
         ISP    = 0
         IP     = 0
         TOKEN  = 0
 
*        internal variables
         ISTCK  = 0
         NSTCK  = 0
 
      ELSE IF (ITASK.EQ.3.AND.ISTCK.EQ.NSTCK) THEN
 
*        lexical analysis must move forward and this part of the buffer
*        has not yet been analysed
 
*        increase number of stack elements, anticipating on the
*        token to be found
         NSTCK  = NSTCK+1
         IF (NSTCK.GT.MNSTCK) CALL FATALERR
     &      ('LEXPARSE','Internal error, too many stack elements')
         ISP = IP+1
         IF (ISP.LT.STBLEN) THEN
*           pointer has not progressed until end_of_line
            TOKEN = 0
            TMPP  = ISP
            DO I1=1,NJMP
               ISTATE = JMPPTR(I1)
               IP     = ISP
70             IF (ISTATE.GT.0) THEN
                  ISTATE = LEXJTB(ISTATE,CODES(IP))
                  IP     = IP+1
               GOTO 70
               END IF
 
               IF (ISTATE.LT.0) THEN
*                 jumptable jumping has stopped at valid parser
                  IP = IP-2
                  IF (IP.GE.TMPP) THEN
*                    parsing has progressed further than any previous
*                    parser
                     TMPP  = IP
                     TOKEN = ABS (ISTATE)
                  END IF
               END IF
 
            END DO
 
            IP = TMPP
 
*           decoding section
            IF (TOKEN.EQ.TFLOAT) THEN
               CALL RDDECD (ISP,IP)
               RESTCK(NSTCK) = VFLOAT
               VTSTCK(NSTCK) = 'F'
               VTYPE         = 'F'
            ELSE IF (TOKEN.EQ.TINTEG) THEN
               CALL RDDECI (ISP,IP)
               INSTCK(NSTCK) = VINT
               VTSTCK(NSTCK) = 'I'
               VTYPE         = 'I'
            ELSE IF (TOKEN.EQ.TLOGIC) THEN
               CALL RDDECL (ISP,IP)
               LOSTCK(NSTCK) = VLOGIC
               VTSTCK(NSTCK) = 'L'
               VTYPE         = 'L'
            ELSE IF (TOKEN.EQ.TTIME) THEN
               CALL RDDECT (ISP,IP)
               TISTCK(NSTCK) = VTIME
               VTSTCK(NSTCK) = 'T'
               VTYPE         = 'T'
            ELSE IF (TOKEN.EQ.TSTRNG) THEN
               VTSTCK(NSTCK) = 'C'
               VTYPE         = 'C'
            ELSE IF (TOKEN.EQ.TMISS) THEN
               VTYPE         = '-'
            ELSE
               VTSTCK(NSTCK) = ' '
               VTYPE         = ' '
            END IF
 
         ELSE
            TOKEN = TEOL
            IP    = ISP
         END IF
 
         BPSTCK(NSTCK) = ISP
         EPSTCK(NSTCK) = IP
         TKSTCK(NSTCK) = TOKEN
         ISTCK         = NSTCK
 
      ELSE IF (ITASK.EQ.3.AND.ISTCK.LT.NSTCK) THEN
 
*        lexical analysis can move forward but token has already been
*        found prior
         ISTCK  = ISTCK+1
 
*        (this list is identical to the list in the next section)
         ISP    = BPSTCK(ISTCK)
         IP     = EPSTCK(ISTCK)
         TOKEN  = TKSTCK(ISTCK)
 
         VINT   = INSTCK(ISTCK)
         VFLOAT = RESTCK(ISTCK)
         VLOGIC = LOSTCK(ISTCK)
         VTIME  = TISTCK(ISTCK)
         VTYPE  = VTSTCK(ISTCK)
 
      ELSE IF (ITASK.EQ.4) THEN
 
*        lexical analysis must move back, but then
*        part of buffer has already been analysed
 
         ISTCK = ISTCK-1
         IF (ISTCK.GE.1) THEN
*           backwarding yields a token
 
*           (this list is identical to the previous list)
            ISP    = BPSTCK(ISTCK)
            IP     = EPSTCK(ISTCK)
            TOKEN  = TKSTCK(ISTCK)
 
            VINT   = INSTCK(ISTCK)
            VFLOAT = RESTCK(ISTCK)
            VLOGIC = LOSTCK(ISTCK)
            VTIME  = TISTCK(ISTCK)
            VTYPE  = VTSTCK(ISTCK)
 
         ELSE IF (ISTCK.EQ.0) THEN
*           parsing has arrived at the point prior to the first
*           character flag that beginning of line is reached
            ISP    = 0
            IP     = 0
            TOKEN  = 0
         ELSE
*           backwarding goes too far back
            CALL FATALERR ('RDLEX   ','Internal error, empty stack')
         END IF
      ELSE IF (ITASK.EQ.5) THEN
*        set backup pointer to beginning of record
         ISTCK  = 0
         ISP    = 0
         IP     = 0
         TOKEN  = 0
      END IF
 
      RETURN
      END
 
      SUBROUTINE RDDECD (IB,IE)
 
*     PURPOSE:
*           SUBROUTINE to decode reals. Reals are decoded into a double
*     precision variable ranges are: exponent +/- 38, up to 15
*     digits (whether or not significant)
 
*     Double precision
*     ================
*     VAX (D_Floating package, default)
*     appr. 0.29d-38 to 1.7d+38, 16 digits accuracy
 
*     MS-Fortran (IEEE)
*     2.225073858507201d-308 to 1.797693134862316d+308,
*     15 digits accuracy
 
*     LS-Fortran (Mac)
*     2.3d-308 to 1.7d+308, 15 digits accuracy
 
*     Single precision
*     ================
*     VAX (D_Floating package, default)
*     appr. 0.29e-38 to 1.7e+38, 7 digits accuracy
 
*     MS-Fortran (IEEE)
*     1.1754944e-38 to 3.4028235e+38, 6 digits accuracy
 
*     LS-Fortran
*     1.2e-38 to 3.4e38, appr. 7 digits accuracy
 
*     INPUT_ARGUMENTS:
*     IB -
*     IE -
 
*     AUTHOR(S)   : Daniel van Kraalingen
*     E-MAIL      : d.w.g.vankraalingen@ab.dlo.nl
*     VERSION     : 1.0
*     DATE        : 30-September-1997
*     ORGANISATION:
*     Research Institute for Agrobiology and Soil Fertility (AB-DLO).
*     P.O. Box 14, 6700 AA, Wageningen, The Netherlands.
 
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IB, IE
 
**    Include file
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rddecinf.inc'
 
*     maximum number of significant digits
      INTEGER SIGDGM
      PARAMETER (SIGDGM=15)
 
*     maximum absolute value of exponent
      INTEGER EXPOM
      PARAMETER (EXPOM=38)
 
*     Local variables
      INTEGER I2,SIGN,SIGNE,ITMP,IDOT,ICHR,EXPO,IDIG(48:57),DIST,SIGDIG
      LOGICAL EXPFND,SGDFND
      INTEGER MULLB, MULUB
      PARAMETER (MULLB=-15, MULUB=15)
      DOUBLE PRECISION MULTIA(MULLB:MULUB),DDIG(48:57), MULTI
 
      SAVE
 
      DATA DDIG /0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0/
      DATA IDIG /0,1,2,3,4,5,6,7,8,9/
      DATA MULTIA /1.D-15, 1.D-14, 1.D-13, 1.D-12, 1.D-11,
     &             1.D-10, 1.D-9 , 1.D-8 , 1.D-7 , 1.D-6 ,
     &             1.D-5 , 1.D-4 , 1.D-3 , 1.D-2 , 1.D-1 ,
     &             0.D+0 ,
     &             1.D+0 , 1.D+1 , 1.D+2 , 1.D+3 , 1.D+4 ,
     &             1.D+5 , 1.D+6 , 1.D+7 , 1.D+8 , 1.D+9 ,
     &             1.D+10, 1.D+11, 1.D+12, 1.D+13, 1.D+14/
 
      IF (STBUF2(IB).EQ.'-') THEN
*        minus sign found, shift start of decoding one to the right
         SIGN = -1
         ITMP = IB+1
      ELSE IF (STBUF2(IB).EQ.'+') THEN
*        plus sign found, also shift start of decoding one to the right
         SIGN = +1
         ITMP = IB+1
      ELSE
*        no sign found, default sign is positive
         SIGN = +1
         ITMP = IB
      END IF
 
*     search for dot, if not found, search subsequently for E,e,D and d
*     to enable e.g. 3e4 to be decoded as a real
      IDOT = INDEX (STBUF(IB:IE),'.')
      IF (IDOT.EQ.0) THEN
         IDOT = INDEX (STBUF(IB:IE),'E')
         IF (IDOT.EQ.0) THEN
            IDOT = INDEX (STBUF(IB:IE),'e')
            IF (IDOT.EQ.0) THEN
               IDOT = INDEX (STBUF(IB:IE),'D')
               IF (IDOT.EQ.0) THEN
                  IDOT = INDEX (STBUF(IB:IE),'d')
                  IF (IDOT.EQ.0) THEN
C                     write (*,*) stbuf(ib:ie)
                     CALL FATALERR ('RDDECD','internal error')
                  END IF
               END IF
            END IF
         END IF
      END IF
      IDOT = IB+IDOT-1
 
*     set exponent_found and significant_digit_found
      EXPFND = .FALSE.
      SGDFND = .FALSE.
      SIGDIG = 0
      VFLOAT = 0.D0
 
*     go to end of string, if exponent character is found, jump out
*     before the end of the string
 
      DO 10 I2=ITMP,IE
 
         ICHR = ASCII(I2)
 
         IF (ICHR.GE.49.AND.ICHR.LE.57) THEN
*           digit found
 
*           start counting digits on the first non zero
            IF (SGDFND) THEN
               SIGDIG = SIGDIG+1
            ELSE
               SGDFND = .TRUE.
               SIGDIG = 1
            END IF
 
            DIST = IDOT-I2
            IF (DIST.GE.MULLB.AND.DIST.LE.MULUB) THEN
*              digit number not outside range
               VFLOAT = VFLOAT+MULTIA(DIST)*DDIG(ICHR)
            ELSE IF (DIST.GT.MULUB) THEN
               MULTI  = 10.D0**(DIST-1)
               VFLOAT = VFLOAT+MULTI*DDIG(ICHR)
            ELSE IF (DIST.LT.MULLB) THEN
               MULTI  = 10.D0**DIST
               VFLOAT = VFLOAT+MULTI*DDIG(ICHR)
            END IF
         ELSE IF (ICHR.EQ.48) THEN
            IF (SGDFND) SIGDIG = SIGDIG+1
         ELSE IF (STBUF2(I2).EQ.'.') THEN
*           dot found, do nothing
            CONTINUE
         ELSE IF (INDEX ('EeDd',STBUF2(I2)).NE.0) THEN
*           exponent character found, shift start of exponent decoding
*           one to the right
            ITMP   = I2+1
            EXPFND = .TRUE.
            GOTO 20
         END IF
10    CONTINUE
 
20    CONTINUE
 
      IF (EXPFND) THEN
*        exponent found, process sign and value
         IF (STBUF2(ITMP).EQ.'-') THEN
            SIGNE = -1
            ITMP  = ITMP+1
         ELSE IF (STBUF2(ITMP).EQ.'+') THEN
            SIGNE = +1
            ITMP  = ITMP+1
         ELSE
            SIGNE = +1
         END IF
         EXPO = 0
         DO 30 I2=ITMP,IE
            EXPO = EXPO*10+IDIG(ASCII(I2))
30       CONTINUE
      END IF
 
*     check whether sign before number was found
      IF (SIGN.EQ.-1) VFLOAT = -VFLOAT
 
      IF (EXPFND) THEN
*        exponent found, see if it within the valid range
         IF (EXPO.LE.EXPOM) THEN
            VFLOAT = VFLOAT*10.D0**(SIGNE*EXPO)
         ELSE
            CALL RDERR (2,'Exponent outside limits')
         END IF
      END IF
 
      IF (SIGDIG.GT.SIGDGM)
     &   CALL RDERR (3,
     & 'Many digits, possible loss of accuracy, even with RD*DOU calls')
 
      RETURN
      END
 
      SUBROUTINE RDDECI (IB,IE)
 
*     PURPOSE:
*           SUBROUTINE to decode integers, range:
*     -2147483647 < value < +2147483647
 
*     INPUT_ARGUMENTS:
*     IB -
*     IE -
 
*     AUTHOR(S)   : Daniel van Kraalingen
*     E-MAIL      : d.w.g.vankraalingen@ab.dlo.nl
*     VERSION     : 1.0
*     DATE        : 30-September-1997
*     ORGANISATION:
*     Research Institute for Agrobiology and Soil Fertility (AB-DLO).
*     P.O. Box 14, 6700 AA, Wageningen, The Netherlands.
 
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IB,IE
 
**    Include file
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rddecinf.inc'
 
*     Local variables
      INTEGER ISPTMP,I2, SIGN, NDIG
      INTEGER DIGITS(48:57), IVAL
      LOGICAL OVERFL
      SAVE
 
      DATA DIGITS /0,1,2,3,4,5,6,7,8,9/
 
*     decode an integer, it must be a valid integer !! (is
*     already check by RDLEX)
 
*     find out if digits begin at the first character or not
      IF (STBUF2(IB).EQ.'-') THEN
         ISPTMP = IB+1
         SIGN   = -1
      ELSE IF (STBUF2(IB).EQ.'+') THEN
         ISPTMP = IB+1
         SIGN   = 1
      ELSE
         ISPTMP = IB
         SIGN   = 1
      END IF
 
*     calculate number of digits
      NDIG  = IE-ISPTMP+1
 
*     check for overflow
      IF (NDIG.LT.10) THEN
         OVERFL = .FALSE.
      ELSE IF (NDIG.EQ.10) THEN
         OVERFL = LGT (STBUF(ISPTMP:IE),'2147483647')
      ELSE
         OVERFL = .TRUE.
      END IF
 
      IF (.NOT.OVERFL) THEN
         IVAL = DIGITS(ASCII(ISPTMP))
         DO 10 I2=ISPTMP+1,IE
            IVAL = IVAL*10+DIGITS(ASCII(I2))
10       CONTINUE
         VINT = IVAL*SIGN
      ELSE
*        write overflow message
         VINT = 99
         CALL RDERR (2,'Integer value too large')
      END IF
 
      RETURN
      END
 
**    USAGE:
 
      SUBROUTINE RDDECL (IB,IE)
 
*     PURPOSE:
*           SUBROUTINE to decode logicals
 
*     INPUT_ARGUMENTS:
*     IB -
*     IE -
 
*     AUTHOR(S)   : Daniel van Kraalingen
*     E-MAIL      : d.w.g.vankraalingen@ab.dlo.nl
*     VERSION     : 1.0
*     DATE        : 30-September-1997
*     ORGANISATION:
*     Research Institute for Agrobiology and Soil Fertility (AB-DLO).
*     P.O. Box 14, 6700 AA, Wageningen, The Netherlands.
 
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IB, IE
 
**    Include files
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rddecinf.inc'
 
*     Local variables
      CHARACTER LOGSTR*7
      SAVE
 
      IF (IE-IB+1.LE.7) THEN
         LOGSTR = STBUF(IB:IE)
         CALL UPPERC (LOGSTR)
         IF (LOGSTR.EQ.'.TRUE.') THEN
            VLOGIC = .TRUE.
         ELSE IF (LOGSTR.EQ.'.FALSE.') THEN
            VLOGIC = .FALSE.
         ELSE
            CALL RDERR (2,'not a logical value')
         END IF
      ELSE
         CALL RDERR (2,'not a logical value')
      END IF
 
      RETURN
      END
 
**    USAGE:
 
      SUBROUTINE RDDECT (IB,IE)
 
*     PURPOSE:
*           SUBROUTINE to decode date/time
 
*     INPUT_ARGUMENTS:
*     IB -
*     IE -
 
*     AUTHOR(S)   : Daniel van Kraalingen
*     E-MAIL      : d.w.g.vankraalingen@ab.dlo.nl
*     VERSION     : 1.0
*     DATE        : 30-September-1997
*     ORGANISATION:
*     Research Institute for Agrobiology and Soil Fertility (AB-DLO).
*     P.O. Box 14, 6700 AA, Wageningen, The Netherlands.
 
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IB, IE
 
**    Include files
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rddecinf.inc'
 
*     Local variables
      INTEGER INSEP,MNSEP
      PARAMETER (MNSEP=7)
      INTEGER I1, I2, IPSEP(0:MNSEP), DATEA(6),IFINDC
      INTEGER YRSEQ, DAYSEQ, HRSEQ
      CHARACTER MONN1(12)*3, MONN2(12)*9, MONTMP*9, MESSAG*80
      LOGICAL MONNUM,ERR
      REAL FSEC
      SAVE
 
      DATA MONN1 /'JAN','FEB','MAR','APR','MAY','JUN',
     &            'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA MONN2 /'JANUARY','FEBRUARY','MARCH','APRIL',
     &            'MAY','JUNE','JULY','AUGUST',
     &            'SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER'/
 
*     SYNTAX CHECK NOG VIA VLAG AAN KUNNEN ZETTEN
 
*     initialize local storage
      DATEA(1) = 1900
      DATEA(2) = 1
      DATEA(3) = 1
      DATEA(4) = 0
      DATEA(5) = 0
      DATEA(6) = 0
      FSEC     = 0.
 
*     determine position of separators
      I1       = IB
      IPSEP(0) = IB-1
      INSEP    = 0
 
10    IF (I1.LE.IE.AND.INSEP.LT.MNSEP-1) THEN
         IF (INDEX ('-/:_.',STBUF2(I1)).NE.0) THEN
*           separator found
            INSEP = INSEP+1
            IPSEP(INSEP) = I1
         END IF
         I1 = I1+1
      GOTO 10
      END IF
 
*     add one separator at the end of the string
      IPSEP(INSEP+1) = IE+1
 
*     determine if a date is present
 
      IF (INDEX ('/-',STBUF2(IPSEP(1))).NE.0) THEN
 
*        date should be present
*        determine whether it is 1994-12-31 or 31-dec-1994 format by che
*        if there are digits between the 1st and 2nd separator
         MONNUM = .TRUE.
         DO 20 I1=IPSEP(1)+1,IPSEP(2)-1
            I2 = ICHAR(STBUF2(I1))
            IF (MONNUM.AND.(I2.LT.48.OR.I2.GT.57)) MONNUM = .FALSE.
20       CONTINUE
         IF (MONNUM) THEN
*           numeric month, expecting yy-mm-dd format
            YRSEQ  = 1
            DAYSEQ = 3
            CALL RDDECI(IPSEP(1)+1,IPSEP(2)-1)
            DATEA(2) = VINT
         ELSE
*           nonnumeric month, expecting dd-mm-yy format
            MONTMP = STBUF(IPSEP(1)+1:IPSEP(2)-1)
            CALL UPPERC (MONTMP)
            IF (IPSEP(2)-IPSEP(1)-1.EQ.3) THEN
*              expecting 3 letter month name
               I1 = IFINDC (MONN1,12,1,12,MONTMP(1:3))
            ELSE IF (IPSEP(2)-IPSEP(1)-1.GT.3) THEN
*              expecting full month name
               I1 = IFINDC (MONN2,12,1,12,MONTMP)
            ELSE
*              name has illegal length
               CALL RDERR (2,'length of month not correct')
            END IF
 
            IF (I1.GT.0) THEN
               DATEA(2) = I1
            ELSE
               CALL RDERR (2,'illegal month name')
            END IF
 
*           year should be in first position, day in last
            YRSEQ  = 3
            DAYSEQ = 1
 
         END IF
 
*        process year
         CALL RDDECI (IPSEP(YRSEQ-1)+1,IPSEP(YRSEQ)-1)
         DATEA(1) = VINT
 
*        process day number
         CALL RDDECI (IPSEP(DAYSEQ-1)+1,IPSEP(DAYSEQ)-1)
         DATEA(3) = VINT
 
      END IF
 
*     determine if a time is present and if so where it is present
 
      IF (STBUF2(IPSEP(1)).EQ.':') THEN
*        no date part before time part
         HRSEQ    = 1
         DATEA(1) = 0
         DATEA(2) = 0
         DATEA(3) = 0
      ELSE IF (INSEP.GE.3) THEN
*        date part precedes time part
         HRSEQ = 4
      ELSE
*        no time part at all
         HRSEQ = 0
      END IF
 
      IF (HRSEQ.GT.0) THEN
 
*        time is present, hour and minute should always be there
 
*        process hour
         CALL RDDECI (IPSEP(HRSEQ-1)+1,IPSEP(HRSEQ)-1)
         DATEA(4) = VINT
 
*        process minute
         CALL RDDECI (IPSEP(HRSEQ)+1,IPSEP(HRSEQ+1)-1)
         DATEA(5) = VINT
 
         IF (INSEP.GE.HRSEQ+1) THEN
 
*           integer part of seconds should also be there
            CALL RDDECI(IPSEP(HRSEQ+1)+1,IPSEP(HRSEQ+2)-1)
            DATEA(6) = VINT
 
            IF (INSEP.GE.HRSEQ+2.AND.
     &          IPSEP(HRSEQ+3)-IPSEP(HRSEQ+2).GT.1) THEN
*              fractional part of seconds should also be there
*              include decimal point !
               CALL RDDECD(IPSEP(HRSEQ+2),IPSEP(HRSEQ+3)-1)
               FSEC = REAL (VFLOAT)
            END IF
 
         END IF
      END IF
 
*     decode date to double precision real
      CALL DTSYS (1,DATEA,FSEC,VTIME,ERR,MESSAG)
      IF (ERR) THEN
         VTIME = 0.D0
         CALL RDERR (2,MESSAG)
      END IF
 
      RETURN
      END
**==RDMCHA.FOR  
      SUBROUTINE RDMCHA (X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80          CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
      CALL RDDATA (10,'RDMCHA',0,0,' ',IS,' ','C',D,R,I,X,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDMDEF.FOR  
      SUBROUTINE RDMDEF
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
*     none
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
      CALL RDDATA (11,'RDMDEF',0,0,' ',IS,' ','R',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDMDOU.FOR  
      SUBROUTINE RDMDOU (X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      DOUBLE PRECISION X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
      D(1) = X
      CALL RDDATA (10,'RDMDOU',0,0,' ',IS,' ','D',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDMINT.FOR  
      SUBROUTINE RDMINT (X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
      I(1) = X
      CALL RDDATA (10,'RDMINT',0,0,' ',IS,' ','I',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDMLOG.FOR  
      SUBROUTINE RDMLOG (X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      LOGICAL X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
      L(1) = X
      CALL RDDATA (10,'RDMLOG',0,0,' ',IS,' ','L',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDMREA.FOR  
      SUBROUTINE RDMREA (X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
      R(1) = X
      CALL RDDATA (10,'RDMREA',0,0,' ',IS,' ','R',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDMTIM.FOR  
      SUBROUTINE RDMTIM (X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      DOUBLE PRECISION X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
      D(1) = X
      CALL RDDATA (10,'RDMTIM',0,0,' ',IS,' ','T',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDPARS.FOR  
      SUBROUTINE RDPARS (IUNIT,IULOG,DATFIL)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IUNIT,IULOG
      CHARACTER*(*) DATFIL
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
*     initialize
      IL = 0
      CALL RDDATA (4,'RDPARS',IUNIT,IULOG,DATFIL,IS,' ',' ',
     $             D,R,I,C,L,1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDSCHA.FOR  
      SUBROUTINE RDSCHA (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME,X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      LOGICAL          L(1),LM
      CHARACTER*80          CM
 
*     other
      INTEGER IL,IS
      SAVE
 
*     read a single CHARACTER value (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSCHA',0,0,' ',IS,XNAME,'C',D,R,I,X,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      RETURN
      END
**==RDSCTB.FOR  
      SUBROUTINE RDSCTB (EOF)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      LOGICAL EOF
 
**    include files
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rdtblinf.inc'
      INCLUDE 'rdndec.gin'
      INCLUDE 'rddecinf.inc'
      INCLUDE 'lextokin.inc'
 
*     lexical analysis
      INTEGER TNEXT
 
*     state machine
*     Syntax check state machine
*     --------------------------
*    -1 - error in name part
*     0 - error in value part
*     1 - looking for table ; expecting identifier
*     2 - first identifier found
*     3 - first identifier + EOL
*     4 - name separator found
*     5 - next identifier found
*     6 - next identifier + EOL
*     7 - equal sign found
*     8 - integer found
*     9 - multiplier * found
*     A - string found
*     B - concatenator // found
*     C - other value or missing value or value after * found
*     D - value separator found, there must be another value
*     E - any value + space
*     F - value + EOL, on next line no comma, can be new table
*     G - End_Of_Table
*
*        STATUS              + - 1 2 3 4 5 6 7 8 9 A B C D E F G
*      WORD                  -----------------------------------
*     0 - unknown            + - + + + + + + - - - - - - - - - -
*     1 - TEOI   - EOF       G G G + + + + + - G - G - G - G G -
*     2 - TEOL   - EOL       6 F 1 3 3 4 6 6 7 F 9 F B F D F F -
*     3 - TTIME  - time      + C + + + + + C C - C - - - C C C -
*     4 - TINTEG - integer   + 8 + + + + + 8 8 - C - - - 8 8 8 -
*     5 - TFLOAT - real      + C + + + + + C C - C - - - C C C -
*     6 - TSPACE - space     + - 1 2 3 4 5 6 7 E 9 E B E D E F -
*     7 - TCOMMA - comma     4 D + 4 + + 4 + - D - D - D - D - -
*     8 - TEQUAL - equal =   7 - + 7 + + 7 + - - - - - - - - - -
*     9 - TSEMIC - semicol   G G + + + + + + - G - G - G - G G -
*    10 - TMULT  - *         + - + + + + + + - 9 - - - - - - - -
*    11 - TLPAR  - (         + - + + + + + + - - - - - - - - - -
*    12 - TRPAR  - )         + - + + + + + + - - - - - - - - - -
*    13 - TSCONC - //        + - + + + + + + - - - B - - - - - -
*    14 - TCMMNT - !         + - 1 2 3 4 5 6 7 8 9 A B C D E F -
*    15 - TIDENT - name      5 G 2 5 5 5 5 5 - - - - - - - - G -
*    16 - TSTRNG - string    + A + + + + + A A - A - A - A A A -
*    17 - TLOGIC - logical   + C + + + + + C C - C - - - C C C -
*    18 - TMISS  - missing   + C + + + + + C C - C - - - C C C -
 
*     IS     - status of parser
*     ISOLD  - previous parser status
*     EOTAB  - End_Of_Table status
*     JUMPTB - jumptable
*     NINPUT - number of parser input codes
*     NSTATS - number of parser states
      INTEGER NINPUT, NSTATS, EOTAB
      PARAMETER (NSTATS=16, NINPUT=18, EOTAB=16)
      INTEGER IS,ISOLD,JUMPTB
      DIMENSION JUMPTB(-1:NSTATS,0:NINPUT)
 
*     error messages
      CHARACTER MESSAG*35
      DIMENSION MESSAG(NSTATS-1)
 
*     others
*     IDUM   - dummy integer
*     CDUM   - dummy string
*     IREP   - repeat value
*     COLNUM - current column number
*     TBCOMP - flags a complete table
*     NEWREC - Whether new record should be read from input file
*     IWAR   - Warning whether record that was read was too long
*              for input buffer
      INTEGER IDUM, IREP, COLNUM, IWAR
      CHARACTER CDUM*1,CTRLZ*1
      LOGICAL TBCOMP, ON, OFF, NEWREC
      PARAMETER (ON=.TRUE., OFF=.FALSE.)
 
      SAVE
      INCLUDE 'rdndat.gin'
 
      DATA JUMPTB/
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $ 16,16,16,-1,-1,-1,-1,-1, 0,16, 0,16, 0,16, 0,16,16, 0,
     $  6,15, 1, 3, 3, 4, 6, 6, 7,15, 9,15,11,15,13,15,15, 0,
     $ -1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $ -1, 8,-1,-1,-1,-1,-1, 8, 8, 0,12, 0, 0, 0, 8, 8, 8, 0,
     $ -1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $ -1, 0, 1, 2, 3, 4, 5, 6, 7,14, 9,14,11,14,13,14,15, 0,
     $  4,13,-1, 4,-1,-1, 4,-1, 0,13, 0,13, 0,13, 0,13, 0, 0,
     $  7, 0,-1, 7,-1,-1, 7,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $ 16,16,-1,-1,-1,-1,-1,-1, 0,16, 0,16, 0,16, 0,16,16, 0,
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0,
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $ -1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0,11, 0, 0, 0, 0, 0, 0,
     $ -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15, 0,
     $  5,16, 2, 5, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0,16, 0,
     $ -1,10,-1,-1,-1,-1,-1,10,10, 0,10, 0,10, 0,10,10,10, 0,
     $ -1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $ -1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0/
 
      DATA MESSAG/
     $ 'Variable name expected             ',
     $ 'Next variable name or = expected   ',
     $ 'Without =, a next name is expected ',
     $ 'Next variable name expected        ',
     $ 'Next name, = or value on next line ',
     $ 'Next name or first value expected  ',
     $ 'Value expected                     ',
     $ 'Illegal after INTEGER value        ',
     $ 'After * a value is expected        ',
     $ 'Illegal after CHARACTER string     ',
     $ 'After // next string expected      ',
     $ 'No proper value separation         ',
     $ 'There must be another value        ',
     $ 'After a value this is illegal      ',
     $ 'New variable or next value expected'/
 
*     initialize output parameter
      EOF = OFF
 
*     initialize table info common variables
      TBLERR = OFF
      NAMERR = OFF
      TBARAY = 's'
      INAMES = 0
      NCOL   = 0
      NVAL   = 0
 
*     initialize local variables for table parsing
      TBCOMP = ON
      IREP   = 1
      IS     = 1
      IDUM   = 0
      CTRLZ  = CHAR (26)
 
*     new table
      CALL RDTMP1 (2,IDUM,CDUM)
 
10    IF (IS.NE.EOTAB) THEN
*        save current status ; next TOKEN ; jump
         NEWREC = TOKEN.EQ.TEOL.OR.TOKEN.EQ.-1
20       IF (NEWREC) THEN
            CALL RECREAD (STBUF,RECLEN-1,STBLEN,EOF,IWAR)
            IF (.NOT.EOF) EOF = STBUF(1:1).EQ.CTRLZ
            IF (.NOT.EOF) THEN
               RECNO  = RECNO+1
               NEWREC = STBUF2(1).EQ.'*'.OR.
     &                  STBUF2(1).EQ.'!'.OR.
     &                  STBUF2(1).EQ.'['.OR.
     &                  STBLEN.EQ.0
 
               IF (.NOT.NEWREC) THEN
*                 check for truncation of records
                  IF (IWAR.EQ.1) THEN
                     ISP = STBLEN-1
                     IP  = STBLEN-1
                     CALL RDERR (2,'Input record truncated')
                  END IF
 
                  CALL RDLEX (2)
               END IF
            ELSE
               NEWREC = .FALSE.
            END IF
         GOTO 20
         END IF
 
         IF (.NOT.EOF) THEN
            CALL RDLEX (3)
         ELSE
            CALL RECREAD_TERM
            RECNO = 0
            STBUF  = ' '
            STBLEN = 0
            ISP    = 1
            IP     = 1
            TOKEN  = TEOI
         END IF
 
         ISOLD = IS
         IS    = JUMPTB (ISOLD,TOKEN)
 
c         write (30,'(a,i2,A8,a,i2)')
c     $    ' rdsctb: ',token, tokenn(token),' -> state ',is
 
         IF (ISOLD.GT.0 .AND. IS.LE.0) THEN
*           syntax error
            TBLERR = ON
            IF (IS.EQ.-1) NAMERR = ON
            IF (IS.EQ. 0) IREP = 1
 
*           state machine error message
            IF (TOKEN.NE.TEOI) CALL RDERR (2,MESSAG(ISOLD))
            IF (TOKEN.EQ.TEOI) CALL RDERR (2,'Unexpected End_Of_File')
         END IF
 
         IF (TOKEN.EQ.TIDENT .AND. IS.NE.EOTAB) THEN
 
            IF (NCOL.GE.NCOLMX) THEN
*              too many variables in table ; stop
               CALL RDERR (2,'Too many columns in table')
               WRITE (*,'(/,A,I3,2A,/,2A)')
     $          ' Table has more than',NCOLMX,' columns.',
     $          ' Increase the value',' of NCOLMX in the',
     $          ' subroutines RDSCTB and RDTMP1 (both!!).'
               CALL FATALERR ('RDSCTB','too many columns')
            ELSE IF (IP-ISP+1.GT.INLMX) THEN
*              name too long
               CALL RDERR (2,'Variable name too long')
               NAMERR = ON
               TBLERR = ON
            ELSE
*              accept new variable name
               NCOL = NCOL + 1
               IF (.NOT.NAMERR) INAMES = INAMES + 1
 
*              set name, line number and type
               TBLNAM(NCOL) = STBUF(ISP:IP)
               CALL UPPERC (TBLNAM(NCOL))
               TBLLIN(NCOL) = RECNO
               TBLTYP(NCOL) = '-'
 
*              get field pointer
               CALL RDTMP1 (4,TBLPNT(NCOL),CDUM)
            END IF
 
         ELSE IF (TOKEN.EQ.TSTRNG .OR. TOKEN.EQ.TINTEG .OR.
     $            TOKEN.EQ.TFLOAT .OR. TOKEN.EQ.TTIME  .OR.
     $            TOKEN.EQ.TLOGIC .OR. TOKEN.EQ.TMISS) THEN
 
            IF (TOKEN.EQ.TINTEG) THEN
*              check for repeat value
               CALL RDLEX (3)
               TNEXT = TOKEN
               CALL RDLEX (4)
            END IF
 
            IF (TOKEN.EQ.TINTEG .AND. TNEXT.EQ.TMULT) THEN
*              get integer value as repeat value
               IREP = VINT
 
               IF (IS.GT.0 .AND. NCOL.GT.1) THEN
*                 repeater in table is illegal
                  CALL RDERR (2,'Repeat value not allowed in table')
                  TBLERR = ON
               ELSE IF (IREP.LE.0) THEN
*                 illegal repeat value
                  CALL RDERR (2,'Repeat value should be positive')
                  TBLERR = ON
               ELSE
*                 variable is an array, also if IREP=1
                  TBARAY = 'a'
               END IF
 
            ELSE IF (ISOLD.EQ.11) THEN
*              string concatenates with previous string ?
               CALL RDTMP1 (7,IDUM,CDUM)
 
            ELSE
*              all other cases, write value to file buffer
               CALL RDTMP1 (6,IREP,CDUM)
 
*              check data type of written value ; get column number
               NVAL   = NVAL + IREP
               COLNUM = 1 + MOD (NVAL-1,MAX(1,NCOL))
 
               IF (TOKEN.NE.TMISS) THEN
*                 type checking
                  IF (TBLTYP(COLNUM).EQ.'-') THEN
*                    set column data type
                     TBLTYP(COLNUM) = VTYPE
 
                  ELSE IF (
     $             VTYPE.NE.TBLTYP(COLNUM).AND..NOT.NAMERR) THEN
*                    inconsistent type
                     IF (NCOL.LE.1) THEN
*                       simple message
                        CALL RDERR (2,'Inconsistent variable type')
                        TBLERR = ON
                     ELSE IF (.NOT.TBLERR) THEN
*                       no syntax errors but types inconsistent
                        CALL RDERR (2,'Column type inconsistent ')
                        TBLERR = ON
                     END IF
                  END IF
               END IF
 
*              reset repeat value
               IREP = 1
 
*              table complete ; last column ?
               TBCOMP = COLNUM.EQ.NCOL
            END IF
         END IF
      GOTO 10
      END IF
 
*     check completeness of table
      IF (.NOT.TBCOMP .AND. .NOT.TBLERR) THEN
         CALL RDERR (2,'Previous table is incomplete')
         TBLERR = ON
      END IF
 
      IF (TOKEN.EQ.TIDENT) THEN
*        end of table detected by begin of next ; step backward LEX
         CALL RDLEX (4)
      ELSE IF (TOKEN.EQ.TEOI) THEN
*        set End_Of_File
         EOF = ON
      END IF
 
*     if NVAL > 1, there may be an array of values, there may
*     also be a table with 1 or more rows. In all cases the
*     variable(s) are classified as array variables, also if there
*     is just a single row. during the parsing above there the use of
*     a repeat value was already detected.
      IF (NVAL.GT.1) TBARAY = 'a'
 
*     terminate table
      CALL RDTMP1 (8,IDUM,CDUM)
 
      RETURN
      END
**==RDSDOR.FOR  
      SUBROUTINE RDSDOR (XNAME,XMIN,XMAX,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      DOUBLE PRECISION X, XMIN, XMAX
      CHARACTER*(*) XNAME
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS,ILX,ILEN
      CHARACTER Q*1,LXNAME*31
      SAVE
 
      DATA Q /''''/
 
*     read a single DOUBLE PRECISION number
*     (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSDOR',0,0,' ',IS,XNAME,'D',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      X      = D(1)
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
      IF (XMAX.LT.XMIN) THEN
         WRITE (*,'(1X,4A)')
     &     'ERROR in RDSDOR: Range not valid of identifier ',
     &      Q,LXNAME(1:ILX),Q
         CALL FATALERR (' ',' ')
      ELSE IF ((X.LT.XMIN.OR.X.GT.XMAX).AND.X.NE.DM) THEN
         WRITE (*,'(1X,5A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &     'ERROR in RDSDOR: Range error of identifier ',
     &      Q,LXNAME(1:ILX),Q,',',
     &     'value =',X,', range = [',XMIN,',',XMAX,']'
         CALL FATALERR (' ',' ')
      END IF
 
      RETURN
      END
**==RDSDOU.FOR  
      SUBROUTINE RDSDOU (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      DOUBLE PRECISION X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
*     read a single DOUBLE PRECISION value (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSDOU',0,0,' ',IS,XNAME,'D',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      X = D(1)
 
      RETURN
      END
**==RDSETS.FOR  
      SUBROUTINE RDSETS (IUNIT,IULOG,SETFIL,INS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IUNIT,IULOG,INS
      CHARACTER*(*) SETFIL
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS,ILF,ILEN
      CHARACTER*250 SCRREC
      LOGICAL WGIVEN
      INTEGER SCRREC_L
      SAVE
 
      DATA WGIVEN /.FALSE./
 
*     analyse rerun file
      IL = 0
      CALL RDDATA (1,'RDSETS',IUNIT,IULOG,SETFIL,INS,' ',' ',
     $             D,R,I,C,L,1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
*     message to AMBUSY ; set 0 selected
      IS = 0
      CALL AMBUSY (1,'RDFROM',IS)
 
*     messages to screen
      IF (IULOG.GT.0 .AND. INS.GT.0) THEN
         ILF = ILEN (SETFIL)
         WRITE (*,'(1X,A)')
     $    'Message from RDSETS: A logfile report is written'
         SCRREC_L = 21
         SCRREC = 'about the use of the '
         CALL ADDINT (SCRREC,SCRREC_L,INS)
         CALL ADDSTF (SCRREC,SCRREC_L,' parameter sets on ')
         CALL ADDSTR (SCRREC,SCRREC_L,SETFIL(1:ILF))
         WRITE (*,'(1X,A)') SCRREC(1:SCRREC_L)
         WGIVEN = .FALSE.
      ELSE IF (INS.GT.0.AND..NOT.WGIVEN) THEN
         WRITE (*,'(1X,2A)') 'WARNING from RDSETS: ',
     $    'no logfile is used !!'
         WGIVEN = .TRUE.
      END IF
 
      RETURN
      END
**==RDSINR.FOR  
      SUBROUTINE RDSINR (XNAME,XMIN,XMAX,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER X, XMIN, XMAX
      CHARACTER*(*) XNAME
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS,ILX,ILEN
      CHARACTER Q*1,LXNAME*31
      SAVE
 
      DATA Q /''''/
 
*     read a single INTEGER number (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSINR',0,0,' ',IS,XNAME,'I',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      X      = I(1)
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
      IF (XMAX.LT.XMIN) THEN
         WRITE (*,'(1X,4A)')
     &     'ERROR in RDSINR: Range not valid of identifier ',
     &      Q,LXNAME(1:ILX),Q
         CALL FATALERR (' ',' ')
      ELSE IF ((X.LT.XMIN.OR.X.GT.XMAX).AND.X.NE.IM) THEN
         WRITE (*,'(1X,5A,/,T19,A,I8,A,I8,A,I8,A)')
     &     'ERROR in RDSINR: Range error of identifier ',
     &      Q,LXNAME(1:ILX),Q,',',
     &     'value =',X,', range = [',XMIN,',',XMAX,']'
         CALL FATALERR (' ',' ')
      END IF
 
      RETURN
      END
**==RDSINT.FOR  
      SUBROUTINE RDSINT (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER X
      CHARACTER*(*) XNAME
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
*     read a single INTEGER number (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSINT',0,0,' ',IS,XNAME,'I',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      X = I(1)
 
      RETURN
      END
**==RDSLOG.FOR  
      SUBROUTINE RDSLOG (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      LOGICAL X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
*     read a single LOGICAL value (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSLOG',0,0,' ',IS,XNAME,'L',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      X = L(1)
 
      RETURN
      END
**==RDSREA.FOR  
      SUBROUTINE RDSREA (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      REAL X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
*     read a single REAL number (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSREA',0,0,' ',IS,XNAME,'R',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      X = R(1)
 
      RETURN
      END
**==RDSRER.FOR  
      SUBROUTINE RDSRER (XNAME,XMIN,XMAX,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X, XMIN, XMAX
      CHARACTER*(*) XNAME
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS,ILX,ILEN
      CHARACTER Q*1,LXNAME*31
      SAVE
 
      DATA Q /''''/
 
*     read a single REAL number (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSRER',0,0,' ',IS,XNAME,'R',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      X     = R(1)
 
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
      IF (XMAX.LT.XMIN) THEN
         WRITE (*,'(1X,4A)')
     &     'ERROR in RDSRER: Range not valid of identifier ',
     &      Q,LXNAME(1:ILX),Q
         CALL FATALERR (' ',' ')
      ELSE IF ((X.LT.XMIN.OR.X.GT.XMAX).AND.X.NE.RM) THEN
         WRITE (*,'(1X,5A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &     'ERROR in RDSRER: Range error of identifier ',
     &      Q,LXNAME(1:ILX),Q,',',
     &     'value =',X,', range = [',XMIN,',',XMAX,']'
         CALL FATALERR (' ',' ')
      END IF
 
      RETURN
      END
**==RDSTIM.FOR  
      SUBROUTINE RDSTIM (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      DOUBLE PRECISION X
 
**    local variables ; dummy set
      DOUBLE PRECISION D(1),DM
      REAL             R(1),RM
      INTEGER          I(1),IM
      CHARACTER*80     C(1),CM
      LOGICAL          L(1),LM
 
*     other
      INTEGER IL,IS
      SAVE
 
*     read a single TIME value (value of IL is arbitrary) !!
      IL = 0
      CALL RDDATA (5,'RDSTIM',0,0,' ',IS,XNAME,'T',D,R,I,C,L,
     $             1,1,1,1,1,IL,
     $             DM,RM,IM,CM,LM)
 
      X = D(1)
 
      RETURN
      END
**==RDTMP1.FOR  
      SUBROUTINE RDTMP1 (ITASK,INT1,STRG)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK,INT1
      CHARACTER*(*) STRG
 
**    machine constants and buffer length
      INCLUDE 'rdmachin.inc'
 
*     includes
      INCLUDE 'rdrecinf.inc'
      INCLUDE 'rderrinf.inc'
      INCLUDE 'lextokin.inc'
      INCLUDE 'rddecinf.inc'
 
*     temporary file
*     --------------
*     IUN    - unit number used to open random access file for I/O
*     TOPREC - the highest record number in use
      INTEGER IUN, TOPREC
 
*     table variables in this subroutine
*     ----------------------------------
*     NCOLMX - maximum number of columns in table
*     TBLWID - number of columns in table
*     TBLREC - current writing record for TBLWID columns
*     TBLFBT - points to free byte for TBLWID columns
*     ICOL   - column number for current value
*
*     IBUF   - buffer array with INTEGER values
*     RBUF   - buffer array with REAL values, equivalenced with IBUF !!
*     LBUF   - buffer array with LOGICAL's, equivalenced with IBUF !!
*
*     IXL    - number of bytes used per item (4,8 or 1 for R,I and C)
*     IWN    - word number in buffer, calculated from pointer and IXL
*     IBN    - byte number in integer word
*     IW     - integer element in case of C type, where IWN in bytes
*
      INTEGER NCOLMX, TBLWID, TBLREC, TBLFBT, ICOL
      PARAMETER (NCOLMX=40)
      DIMENSION TBLFBT(NCOLMX), TBLREC(NCOLMX)
 
      INTEGER IXL, IWN, NELMT, IBN, IW
      INTEGER           IBUF(0:ILBUF/IIL-1,NCOLMX)
      DOUBLE PRECISION  RBUF(0:ILBUF/IRL-1,NCOLMX)
      LOGICAL           LBUF(0:ILBUF/ILL-1,NCOLMX)
      EQUIVALENCE (IBUF,RBUF,LBUF)
 
*     other
      INTEGER I,J,K,ITOLD,NUL,ILRTOP,IRECL,IBYTE,IREP
      INTEGER IPC, IXLMAX
      LOGICAL INIT,CONCAT,REPEAT,WRTDAT
      DIMENSION IPC(0:IIL-1)
 
      SAVE
      DATA INIT /.FALSE./, NUL/0/, ITOLD /0/
 
 
      IF (.NOT.INIT) THEN
*        initialize ; calculate values of array IPC
         IPC(IIL-1) = 1
         DO 10 I=IIL-2,0,-1
            IPC(I) = 256 * IPC(I+1)
10       CONTINUE
 
*        calculate IXLMAX as the maximum type length
         IXLMAX = MAX(IIL,IRL,ILL)
 
*        the number of the last double precision number on a record
         ILRTOP = ILBUF/IRL-1
 
         INIT  = .TRUE.
      END IF
 
 
      IF (ITASK.EQ.1) THEN
*        initialize TMP file writing ; open direct access file
         IRECL = ILBUF + IIL
         IUN   = INT1
         CALL FOPENG (IUN,STRG,'NEW','UD',IRECL,'DEL')
 
*        first free record
         TOPREC    = 2
*        initialize first value_record
         TBLREC(1) = TOPREC
         TBLFBT(1) = 0
 
 
      ELSE IF (ITASK.EQ.2) THEN
*        new table
         IF (.NOT.(ITOLD.EQ.1 .OR. ITOLD.EQ.8))
     $    CALL FATALERR ('RDTMP1','Internal_1')
 
*        reset table width
         TBLWID = 0
 
 
      ELSE IF (ITASK.EQ.4) THEN
*        initialize field for next column ; return pointer
         IF (INERR.EQ.0) THEN
*           increase table width and column number
            TBLWID = TBLWID + 1
            ICOL   = TBLWID
 
            IF (TBLWID.GT.NCOLMX) CALL FATALERR ('RDTMP1','Internal_2')
 
*           set record pointer
            IF (TBLWID.EQ.1) THEN
*              set pointer at multiple of IXLMAX
               TBLFBT(1) = IXLMAX * ((IXLMAX-1 + TBLFBT(1))/IXLMAX)
               IF (TBLFBT(1).EQ.ILBUF) THEN
*                 write full buffer to file
                  WRITE (IUN,REC=TBLREC(1)) (RBUF(K,1),K=0,ILRTOP),NUL
*                 get number of free record
                  TOPREC = TOPREC + 1
*                 set column buffer number and buffer pointer
                  TBLREC(1) = TOPREC
                  TBLFBT(1) = 0
               END IF
            ELSE
*              set pointer to start of new record
               TOPREC = TOPREC + 1
               TBLREC(TBLWID) = TOPREC
               TBLFBT(TBLWID) = 0
            END IF
 
*           pointer belonging to field
            INT1 = ILBUF * (TBLREC(TBLWID)-1) + TBLFBT(TBLWID)
         END IF
 
 
      ELSE IF (ITASK.EQ.6 .OR. ITASK.EQ.7) THEN
*        write value buffer to record buffer ; get input
         CONCAT = ITASK.EQ.7
         IREP   = INT1
 
*        data type output
         STRG = ' '
 
*        check for string type
         IF (CONCAT.AND.VTYPE.NE.'C')
     $      CALL FATALERR ('RDTMP1','Internal_3')
 
         IF (INERR.EQ.0) THEN
            IF (CONCAT) THEN
*              column number unchanged ; settings for string
c      write (30,'(a,i5)') ' concatenate on column:', icol
               NELMT = IP - ISP
               IXL   = 1
*              decrease pointer to overwrote CHAR(0)
               TBLFBT(ICOL) = TBLFBT(ICOL) - 1
*              do not write repeat value
               REPEAT = .FALSE.
            ELSE
*              column number
               ICOL = 1 + MOD (ICOL,TBLWID)
c      write (30,'(a,i3)') 'rdtmp1 writes column',ICOL
 
*              number of elements NELMT and their length IXL
               IF (VTYPE.EQ.'I') THEN
*                 integer
                  NELMT = 1
                  IXL   = IIL
               ELSE IF (VTYPE.EQ.'F') THEN
*                 real
                  NELMT = 1
                  IXL   = IRL
               ELSE IF (VTYPE.EQ.'C') THEN
*                 character
                  NELMT = IP - ISP
                  IXL   = 1
               ELSE IF (VTYPE.EQ.'L') THEN
*                 logical
                  NELMT = 1
                  IXL   = ILL
               ELSE IF (VTYPE.EQ.'T') THEN
*                 date_time
                  NELMT = 1
                  IXL   = IRL
               ELSE IF (VTYPE.EQ.'-') THEN
*                 missing value ; mirror IREP
                  NELMT = 1
                  IXL   = ILL
                  IREP  = -1 * IREP
               ELSE
*                 error
                  CALL FATALERR ('RDTMP1','illegal variable type')
               END IF
 
*              initialize pointer to multiple of IXLMAX bytes
               TBLFBT(ICOL) = IXLMAX* ((IXLMAX-1 + TBLFBT(ICOL))/IXLMAX)
*              write repeat value first
               REPEAT = .TRUE.
            END IF
 
*           write repeat value and NELMT data elements
            J = 1
            WRTDAT = .TRUE.
80          IF (WRTDAT) THEN
*              record filled ?
               IF (TBLFBT(ICOL).GE.ILBUF) THEN
*                 get number of free record
                  TOPREC = TOPREC + 1
c      write (30,'(a,i3,a,i3)') ' ----', TBLREC(ICOL),' to',TOPREC
*                 write buffer to file with pointer to new
                  WRITE (IUN,REC=TBLREC(ICOL))
     $             (RBUF(K,ICOL),K=0,ILRTOP),(TOPREC-1)*ILBUF
*                 reset column buffer number
                  TBLREC(ICOL) = TOPREC
*                 reset record pointer
                  TBLFBT(ICOL) = 0
               END IF
 
               IF (REPEAT) THEN
*                 write repeat value as integer
                  IBUF(TBLFBT(ICOL)/IIL,ICOL) = IREP
c       write (30,'(i3,4(A,I6))')
c     $  ICOL,': repeat value ',irep,
c     $ ' TBLFBT=',TBLFBT(icol),' record=',tblrec(icol),
c     $ ' word=',tblfbt(icol) / iil
*                 use at least IXLMAX bytes for this integer
                  IF (IREP.GT.0) THEN
*                    use as many bytes as datatype, but at least IIL
                     TBLFBT(ICOL) = TBLFBT(ICOL) + MAX(IXL,IIL)
*                    disable repeat reading
                     REPEAT = .FALSE.
                  ELSE
*                    missing value, use IXLMAX bytes, don't write data
                     TBLFBT(ICOL) = TBLFBT(ICOL) + IXLMAX
                     WRTDAT = .FALSE.
                  END IF
               GOTO 80
               END IF
 
*              word number
               IWN  = TBLFBT(ICOL) / IXL
 
c        write (30,'(i3,4(A,I6))')
c     $  ICOL,': toprec=',toprec,
c     $ ' TBLFBT=',TBLFBT(ICOL),' record=',TBLREC(ICOL),
c     $ ' word=',iwn
*              write data item in buffer
               IF (VTYPE.EQ.'F') THEN
*                 floating point
                  RBUF(IWN,ICOL) = VFLOAT
               ELSE IF (VTYPE.EQ.'I') THEN
*                 integer
                  IBUF(IWN,ICOL) = VINT
               ELSE IF (VTYPE.EQ.'L') THEN
*                 logical
                  LBUF(IWN,ICOL) = VLOGIC
               ELSE IF (VTYPE.EQ.'T') THEN
*                 date_time
                  RBUF(IWN,ICOL) = VTIME
               ELSE IF (VTYPE.EQ.'C') THEN
*                 string code in integer ; byte number and integer
                  IBN = MOD(IWN,IIL)
                  IW  = IWN/IIL
                  IF (J.LT.NELMT) THEN
*                    take character from line buffer
                     IBYTE = ASCII(ISP+J)
                  ELSE
*                    last byte is zero
                     IBYTE = 0
                  END IF
                  IF (IBN.EQ.0) THEN
*                    start new integer word
                     IBUF(IW,ICOL) = IPC(IBN) * IBYTE
                  ELSE
*                    other bytes
                     IBUF(IW,ICOL) = IBUF(IW,ICOL) + IPC(IBN) * IBYTE
                  END IF
               END IF
 
*              increase buffer pointer
               TBLFBT(ICOL) = TBLFBT(ICOL) + IXL
 
*              next data item
               J = J + 1
               WRTDAT = J.LE.NELMT
c      write (30,'(2a,i6)') '--- after writing:',
c     $ 'TBLFBT(icol) = ',TBLFBT(icol)
            GOTO 80
            END IF
         END IF
 
 
      ELSE IF (ITASK.EQ.8) THEN
*        terminate table ; flush column buffers
         IF (INERR.EQ.0) THEN
            DO 50 I=1,TBLWID
               IF (TBLREC(I).LT.TOPREC) THEN
*                 write buffer which will not be used
c                  write (30,'(a,i5)')
c     $              ' eot: write record',TBLREC(I)
                  WRITE (IUN,REC=TBLREC(I)) (RBUF(K,I),K=0,ILRTOP),NUL
               ELSE IF (I.GT.1) THEN
*                 buffer 1 becomes the top record
                  DO 40 K=0,ILRTOP
                     RBUF(K,1) = RBUF(K,I)
40                CONTINUE
                  TBLREC(1) = TOPREC
                  TBLFBT(1) = TBLFBT(I)
               END IF
50          CONTINUE
         END IF
 
 
      ELSE IF (ITASK.EQ.9) THEN
*        terminal call ; delete TMP file ?
         IF (ITOLD.NE.8) CALL FATALERR ('RDTMP1','Internal_4')
         IF (INERR.EQ.0) THEN
*           flush buffer 1
c            write (30,'(a,i5)') ' terminal: dump record',TBLREC(1)
            WRITE (IUN,REC=TBLREC(1)) (RBUF(K,1),K=0,ILRTOP),NUL
 
*           write info for RDTMP2 to first record
            WRITE (IUN,REC=1) TOPREC,ILBUF,IIL
 
         ELSE
*           delete TMP file
            CLOSE (IUN,STATUS='DELETE')
         END IF
      ELSE
*        unknown task
         CALL FATALERR ('RDTMP1','Internal_5')
      END IF
 
      ITOLD = ITASK
 
      RETURN
      END
**==RDTMP2.FOR  
      SUBROUTINE RDTMP2 (ITASK,IUNIT,NAMLIS,VARTYP,VARRAY,ILIND,
     $                   IARLEN,INDPNT,ILPNT,INFND,INSETS)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK,IUNIT,ILIND,IARLEN,INDPNT,ILPNT,INFND,INSETS
      CHARACTER NAMLIS*(*),VARTYP*1,VARRAY*1
      DIMENSION NAMLIS(ILIND),VARTYP(ILIND),VARRAY(ILIND)
      DIMENSION IARLEN(ILPNT),INDPNT(ILPNT)
 
**    local variables and function types
      INTEGER INL,IREC,ILBUF,IIL,INFLD,J,J1,J2
      SAVE
 
*     length of variable name
      INL = LEN (NAMLIS(1))
 
      IF (ITASK.EQ.1) THEN
*        read last record used from record 1
         READ (IUNIT,REC=1) IREC,ILBUF,IIL
 
*        write header of temporary with last datarecord used
         WRITE (IUNIT,REC=1) IREC,ILBUF,IIL,INFND,INSETS
         INFLD = INSETS * INFND
 
*        write variable types and names
         J2 = 0
10       IF (J2.LT.INFND) THEN
            J1 = J2 + 1
            J2 = MIN (J2+ILBUF/(2+INL), INFND)
            IREC = IREC + 1
            WRITE (IUNIT,REC=IREC) 
     $       (VARTYP(J),VARRAY(J),NAMLIS(J),J=J1,J2)
         GOTO 10
         END IF
 
*        write array length and pointer for all fields
         J2 = 0
20       IF (J2.LT.INFLD) THEN
            J1 = J2 + 1
            J2 = MIN (J2+ILBUF/(2*IIL), INFLD)
            IREC = IREC + 1
            WRITE (IUNIT,REC=IREC) (IARLEN(J),INDPNT(J),J=J1,J2)
         GOTO 20
         END IF
 
 
      ELSE IF (ITASK.EQ.2) THEN
*        read index from file ; recover pointer info from first record
         READ (IUNIT,REC=1) IREC,ILBUF,IIL,INFND,INSETS
         INFLD = INSETS * INFND
 
*        read variable types and names
         J2 = 0
30       IF (J2.LT.INFND) THEN
            J1 = J2 + 1
            J2 = MIN (J2+ILBUF/(2+INL), INFND)
            IREC = IREC + 1
            READ (IUNIT,REC=IREC) 
     $       (VARTYP(J),VARRAY(J),NAMLIS(J),J=J1,J2)
         GOTO 30
         END IF
 
*        read array lengths and pointers
         J2 = 0
40       IF (J2.LT.INFLD) THEN
            J1 = J2 + 1
            J2 = MIN (J2+ILBUF/(2*IIL), INFLD)
            IREC = IREC + 1
            READ (IUNIT,REC=IREC) (IARLEN(J),INDPNT(J),J=J1,J2)
         GOTO 40
         END IF
      END IF
 
 
      RETURN
      END
**==REAAND.FOR  
      REAL FUNCTION REAAND (X1, X2)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X1, X2
 
**    local variables
      SAVE
 
      IF (X1.GT.0. .AND. X2.GT.0.) THEN
         REAAND = 1.
      ELSE
         REAAND = 0.
      END IF
 
      RETURN
      END
**==REANOR.FOR  
      REAL FUNCTION REANOR (X1, X2)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL X1, X2
 
**    local variables
      SAVE
 
      IF (X1.LE.0. .AND. X2.LE.0.) THEN
         REANOR = 1.
      ELSE
         REANOR = 0.
      END IF
 
      RETURN
      END
**==RECREAD.FOR 
      SUBROUTINE RECREAD (STBUF,RECLEN,STBLEN,EOF,IWAR)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STBUF
      LOGICAL EOF
      INTEGER RECLEN,STBLEN,IWAR
 
**    Local variables
      INCLUDE 'recread.inc'
      INTEGER F_BUF_LEN,IOS
 
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('RECREAD','system not initialized')
      IWAR = 0
 
      READ (L_UNIT,'(A)',IOSTAT=IOS) F_BUF
      EOF = IOS.NE.0
 
      IF (.NOT.EOF) THEN
*        Determine significant length
         DO F_BUF_LEN=F_BUF_DEC_LEN,1,-1
            IF (F_BUF(F_BUF_LEN:F_BUF_LEN).NE.' ') GOTO 20
         END DO
 
         F_BUF_LEN = 0
 
20       CONTINUE
 
         IF (F_BUF_LEN.GT.0.AND.F_BUF_LEN.LE.RECLEN) THEN
*           Input record fits on user record
            STBUF  = F_BUF(1:F_BUF_LEN)
            STBLEN = F_BUF_LEN
         ELSE IF (F_BUF_LEN.EQ.0) THEN
*           Empty input record
            STBUF  = ' '
            STBLEN = 0
         ELSE
*           Input record too long
            STBUF  = F_BUF(1:RECLEN)
            IWAR   = 1
            STBLEN = RECLEN
         END IF
      ELSE
         CLOSE (L_UNIT)
         FILE_CLOSED = .TRUE.
         F_BUF_LEN = 0
         STBUF     = ' '
         STBLEN    = 0
         INIT      = .FALSE.
      END IF
 
      RETURN
      END
**==RECREADI.FOR
      SUBROUTINE RECREAD_INIT (UNIT,INPUT_FILE)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) INPUT_FILE
      INTEGER UNIT
 
**    Include files
      INCLUDE 'recread.inc'
 
      SAVE
 
      L_UNIT = UNIT
      CALL FOPENS (L_UNIT,INPUT_FILE,'RDO',' ')
 
      FILE_CLOSED = .FALSE.
      INIT        = .TRUE.
 
      RETURN
      END
 
      BLOCK DATA RECREAD_DATA
      IMPLICIT NONE
      INCLUDE 'recread.inc'
      SAVE
      DATA INIT /.FALSE./
      END
**==RECREADT.FOR
      SUBROUTINE RECREAD_TERM
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
 
**    common block
      INCLUDE 'recread.inc'
 
      SAVE
 
      IF (.NOT.FILE_CLOSED) THEN
         CLOSE (L_UNIT)
         FILE_CLOSED = .TRUE.
      END IF
 
      RETURN
      END
**==REMOVE.FOR  
      SUBROUTINE REMOVE (STRING,CHR)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER STRING*(*),CHR*1
 
**    local variable
      INTEGER I
      SAVE
 
 
10    I = INDEX (STRING,CHR)
      IF (I.GT.0) THEN
         STRING(I:I) = ' '
         GOTO 10
      END IF
 
      RETURN
      END
**==SFINDG.FOR  
      SUBROUTINE SFINDG (NAMLIS,ILDEC,IST,IEND,NAME,ISTYPE,
     &                   IFINDG,IMATCH)
 
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC, IST, IEND, ISTYPE, IFINDG, IMATCH
      CHARACTER*(*) NAMLIS(ILDEC), NAME
 
**    local variables
      INTEGER IN, IM, IL1, IL3, INCR, ILEN
      SAVE
 
      IL1 = LEN (NAMLIS(1))
      IL3 = ILEN (NAME)
 
*     error check
      IF (IL3.GT.IL1) CALL FATALERR ('SFINDG','search string too long')
 
      IF (IEND.GT.IST) THEN
*        search from low to high elements
         IF (IST.LT.1.OR.IEND.GT.ILDEC)
     &      CALL FATALERR ('SFINDG','search outside array bounds')
         INCR = +1
      ELSE IF (IEND.LT.IST) THEN
*        search from high to low elements
         IF (IEND.LT.1.OR.IST.GT.ILDEC)
     &      CALL FATALERR ('SFINDG','search outside array bounds')
         INCR = -1
      END IF
 
      IFINDG = 0
      DO IN=IST,IEND,INCR
         IL1 = ILEN(NAMLIS(IN))
 
*        check length of strings
         IF (IL3.LE.IL1) THEN
*           string that is searched is not longer than string
*           to search in
            IM  = INDEX (NAMLIS(IN)(1:IL1),NAME(1:IL3))
         ELSE
*           string that is searched is longer than string
*           to search in, a match cannot occur and also
*           is is risky to call INDEX in that situation
            IM = 0
         END IF
 
         IF (IM.GT.0) THEN
            IF (ISTYPE.EQ.1) THEN
*              string should match exactly
               IF (IM.EQ.1.AND.IL3.EQ.IL1) THEN
                  IFINDG = IN
                  GOTO 30
               END IF
            ELSE IF (ISTYPE.EQ.2) THEN
*              beginning should match
               IF (IM.EQ.1) THEN
                  IFINDG = IN
                  GOTO 30
               END IF
            ELSE IF (ISTYPE.EQ.3) THEN
*              end should match
               IF (IM.EQ.IL1-IL3+1) THEN
                  IFINDG = IN
                  GOTO 30
               END IF
            ELSE IF (ISTYPE.EQ.4) THEN
*              match position not specified
               IFINDG = IN
               GOTO 30
            END IF
         END IF
      END DO
 
      IM = 0
 
30    CONTINUE
 
      IMATCH = IM
 
      RETURN
      END
**==SORTCH.FOR  
      SUBROUTINE SORTCH (N,ARRIN,INDX,Q)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER N,INDX
      CHARACTER*(*) ARRIN,Q
      DIMENSION ARRIN(N),INDX(N)
 
**    local parameters
      INTEGER I,J,L,IR,INDXT
      SAVE
 
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      IF (N.EQ.1) RETURN
 
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(LLT(ARRIN(INDX(J)),ARRIN(INDX(J+1))))J=J+1
          ENDIF
          IF(LLT(Q,ARRIN(INDX(J))))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END
**==SORTIN.FOR  
      SUBROUTINE SORTIN (N,ARRIN,INDX)
      IMPLICIT NONE
 
*     formal parameters
      INTEGER N,ARRIN,Q,INDX
      DIMENSION ARRIN(N),INDX(N)
 
*     local parameters
      INTEGER I,J,L,IR,INDXT
 
      SAVE
 
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      IF (N.EQ.1) RETURN
 
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END
**==STR_COPY.FOR
      SUBROUTINE STR_COPY (SOURCE_S,TARGET_S,OK)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) SOURCE_S,TARGET_S
      LOGICAL OK
 
**    Local variables
      INTEGER ILS,ILT,ILEN
      SAVE
 
      OK  = .TRUE.
      ILS = MAX (1,ILEN (SOURCE_S))
      ILT = LEN (TARGET_S)
 
      IF (ILS.GT.ILT) THEN
         OK  = .FALSE.
         ILS = ILT
      END IF
 
      TARGET_S = SOURCE_S(1:ILS)
 
      RETURN
      END
**==SWPI4.FOR   
      SUBROUTINE SWPI4 (ITASK,NAME,IUNIT,DECL,IP,VAL)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK,IUNIT,DECL,IP
      CHARACTER*(*) NAME
 
**    Local variables
 
*     Status array
*     MNA - Maximum number of names that can be swapped
*     ANAME - Array with names of swap arrays
*     AUNIT - Array with units for file i/o used for swap arrays
*     NREC  - Array with number of records per swap array
*     DECLA - Array with declared sizes of swap array
 
      INTEGER MNA
      PARAMETER (MNA=10)
      CHARACTER*62 ANAME(MNA),LNAME,EMPTY
      INTEGER AUNIT(MNA),NREC(MNA),DECLA(MNA)
 
*     Miscellaneous
      INTEGER I1,IN,IFINDC,RECN,TYPLEN
      CHARACTER MODNAM*6
      LOGICAL UNITOP
 
*     Data type specific
      INTEGER VAL,ZERO
      SAVE
 
*     Data type specific
      DATA ZERO /0/, TYPLEN /4/, MODNAM /'SWPI4'/
 
      DATA ANAME /MNA*' '/,EMPTY /' '/
 
C      write (*,*) itask,name,decl,ip
 
*     lookup name in name array
      LNAME = NAME
      IF (LNAME.EQ.' ') CALL FATALERR (MODNAM,'empty array name')
      IN    = IFINDC (ANAME,MNA,1,MNA,LNAME)
 
      IF (ITASK.EQ.1) THEN
*        store value
 
         IF (IN.EQ.0) THEN
*           array name unknown, set up for new name
 
            IF (DECL.LT.1) CALL FATALERR (MODNAM,
     &         'illegal declared length')
            IF (IUNIT.LT.10.OR.IUNIT.GT.99) CALL FATALERR
     &         (MODNAM,'illegal unit number')
 
            INQUIRE (UNIT=IUNIT,OPENED=UNITOP)
            IF (UNITOP) CALL FATALERR (MODNAM,'unit is already in use')
 
            IN = IFINDC (ANAME,MNA,1,MNA,EMPTY)
            IF (IN.EQ.0) CALL FATALERR
     &         (MODNAM,'internal name array full')
 
            ANAME(IN) = LNAME
 
            DECLA(IN) = DECL
 
            NREC(IN)  = 0
            AUNIT(IN) = IUNIT
            OPEN (AUNIT(IN),STATUS='SCRATCH',
     &            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=TYPLEN)
         END IF
 
         IF (IP.LE.DECLA(IN)) CALL FATALERR (MODNAM,
     &      'pointer less than declared value')
 
         RECN = IP-DECLA(IN)
 
         IF (RECN.GT.NREC(IN)) THEN
            DO 10 I1=NREC(IN)+1,RECN-1
               WRITE (AUNIT(IN),REC=I1) ZERO
10          CONTINUE
            NREC(IN) = RECN
         END IF
 
         WRITE (AUNIT(IN),REC=RECN) VAL
 
      ELSE IF (ITASK.EQ.2) THEN
 
*        read value
         IF (IN.GT.0) THEN
            RECN = IP-DECLA(IN)
            IF (RECN.GE.1.AND.RECN.LE.NREC(IN)) THEN
*              array value has been stored on file
               READ (AUNIT(IN),REC=RECN) VAL
            ELSE IF (RECN.GT.NREC(IN)) THEN
*              read is beyond last record
               CALL WARNING (MODNAM,'returning uninitialized element')
               VAL = ZERO
            ELSE IF (RECN.LT.1) THEN
*              error in calling swap routine
               CALL FATALERR (MODNAM,
     &         'array index below declared length')
            END IF
         ELSE
            CALL FATALERR (MODNAM,'unknown array name')
         END IF
 
      ELSE IF (ITASK.EQ.3) THEN
 
*        delete array, close and delete swap file, ignore arrays
*        not known to this routine
         IF (IN.GT.0) THEN
            ANAME(IN) = ' '
            CLOSE (AUNIT(IN))
            AUNIT(IN) = 0
         END IF
      ELSE
         CALL FATALERR (MODNAM,'illegal itask')
      END IF
 
      RETURN
      END
**==TIMER2.FOR  
      SUBROUTINE TIMER2 (ITASK, STTIME, DELT, PRDEL , FINTIM, IYEAR,
     &                   TIME , DOY   , IDOY, TERMNL, OUTPUT)
 
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ITASK, IYEAR, IDOY
      REAL STTIME, DELT, PRDEL, FINTIM, TIME, DOY
      LOGICAL TERMNL, OUTPUT
 
**    local variables
      INTEGER I, ITOLD, ICOUNT, IT, IS, IFINT, IPRINT, ILDOY, ILYEAR
      REAL LDELT, LTIME, LSTTIM, TINY, R1, DIFF
      PARAMETER (TINY=5.E-5)
      LOGICAL PROUT
      SAVE
 
      DATA ITOLD /4/
 
      IF (ITASK.EQ.1) THEN
 
*        test absolute values
         IF (IYEAR.LT.1000.OR.IYEAR.GT.2100)
     &      CALL FATALERR ('TIMER2', 'IYEAR < 1000 or IYEAR > 2100')
 
         IF (MOD (IYEAR,4).NE.0) THEN
            IF (STTIME.LT.(1.-TINY).OR.STTIME.GT.(365.+TINY))
     &         CALL FATALERR ('TIMER2', 'STTIME < 1 or > 365')
         ELSE
            IF (STTIME.LT.(1.-TINY).OR.STTIME.GT.(366.+TINY))
     &         CALL FATALERR ('TIMER2', 'STTIME < 1 or > 366')
         END IF
 
         IF (FINTIM.LT.STTIME) CALL FATALERR
     &      ('TIMER2', 'FINTIM < STTIME')
 
         IF (DELT.LE.0.) THEN
            CALL FATALERR ('TIMER2', 'DELT <= 0')
         ELSE IF (DELT.GT.0..AND.DELT.LT.1.) THEN
            R1 = 1./DELT
            IT = NINT (1./DELT)
            IS = 1
         ELSE IF (DELT.EQ.1.) THEN
            R1 = 1.
            IT = 1
            IS = 1
         ELSE IF (DELT.GT.1.) THEN
            R1 = DELT
            IT = 1
            IS = NINT (DELT)
         END IF
 
         IF (PRDEL.LT.0.) THEN
            CALL FATALERR ('TIMER2', 'PRDEL <= 0')
         ELSE IF (PRDEL.EQ.0.) THEN
*           suppress output when prdel = 0
            PROUT = .FALSE.
         ELSE
            PROUT = .TRUE.
         END IF
 
*        check multiples
         IF (ABS (R1-NINT (R1)).GT.TINY) CALL FATALERR
     &      ('TIMER2', 'DELT incorrect')
         IF (PROUT.AND.ABS (PRDEL-DELT*NINT (PRDEL/DELT)).GT.TINY)
     &      CALL FATALERR ('TIMER2', 'PRDEL not a multiple of DELT')
         IF (ABS (STTIME-NINT (STTIME)).GT.TINY) CALL FATALERR
     &      ('TIMER2', 'STTIME not an integer value')
         IF (PROUT) IPRINT = NINT (PRDEL/DELT)
         IFINT  = NINT ((FINTIM-STTIME)/DELT)
         DIFF   = (FINTIM-(STTIME+REAL (IFINT)*DELT))/DELT
         IF (DIFF.GT.0.01) IFINT = IFINT+1
 
         ICOUNT = 0
 
*        assign to local variables
 
         LTIME  = STTIME
         LSTTIM = STTIME
         LDELT  = DELT
         ILDOY  = NINT (STTIME)
         ILYEAR = IYEAR
 
*        global variables
 
         TIME   = STTIME
         IDOY   = ILDOY
         DOY    = REAL (ILDOY)
         TERMNL = .FALSE.
         IF (PROUT) THEN
            OUTPUT = .TRUE.
         ELSE
            OUTPUT = .FALSE.
         END IF
 
      ELSE IF (ITASK.EQ.2) THEN
 
         IF (ITOLD.EQ.4) CALL FATALERR
     &      ('TIMER2','initialization required')
 
         IF (TIME.NE.LTIME) CALL FATALERR
     &      ('TIMER2', 'TIME was changed illegally')
         IF (IDOY.NE.ILDOY) CALL FATALERR
     &      ('TIMER2', 'IDOY was changed illegally')
 
         IF (ICOUNT.LT.IFINT.AND..NOT.TERMNL) THEN
 
            ICOUNT = ICOUNT+1
            LTIME  = LSTTIM+REAL (ICOUNT)*LDELT
 
            IF (MOD (ICOUNT, IT).EQ.0) THEN
               DO 10 I=1,IS
                  ILDOY = ILDOY+1
                  IF (ILDOY.EQ.366) THEN
                     IF (ILYEAR.LT.1500) THEN
                        ILDOY  = 1
                     ELSE
                        IF (MOD (ILYEAR,4).NE.0) THEN
                           ILDOY = 1
                           ILYEAR = ILYEAR+1
                        END IF
                     END IF
                  ELSE IF (ILDOY.EQ.367) THEN
                     ILYEAR = ILYEAR+1
                     ILDOY  = 1
                  END IF
10             CONTINUE
               DOY   = REAL (ILDOY)
            ELSE
               DOY   = REAL (ILDOY)+MOD (LTIME, 1.)
            END IF
 
            OUTPUT = .FALSE.
            IF (PROUT) THEN
               IF (MOD(ICOUNT,IPRINT).EQ.0.OR.ICOUNT.GE.IFINT)
     &              OUTPUT = .TRUE.
            END IF
 
            TIME  = LTIME
            IDOY  = ILDOY
            IYEAR = ILYEAR
         ELSE
            TERMNL = .TRUE.
            IF (PROUT) OUTPUT = .TRUE.
         END IF
 
      ELSE
         CALL FATALERR ('TIMER2','wrong ITASK')
      END IF
 
      ITOLD = ITASK
 
      RETURN
      END
**==TTUVER.FOR  
      SUBROUTINE TTUVER (xMIN_V,xCUR_V)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      REAL xMIN_V,xCUR_V
 
**    Local variables
      REAL CUR_V
      PARAMETER (CUR_V=4.11)
 
      IF (xMIN_V.GT.CUR_V) THEN
         WRITE (*,'(1X,A,/,1X,A,F5.2,/,1X,A,F5.2,A)')
     &     'This program is not linked with the minimal TTUTIL version',
     &     'This is TTUTIL version:',CUR_V,
     &     'At least version      :',xMIN_V,' is required'
         CALL FATALERR ('TTUVER',' ')
      END IF
 
      xCUR_V = CUR_V
 
      RETURN
      END
**==UNIFL.FOR   
      REAL FUNCTION UNIFL()
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
*     none
 
**    local variables
      INTEGER JX,K
      DIMENSION JX(3)
      LOGICAL INIT
      SAVE
      DATA INIT/.FALSE./
 
      IF (.NOT.INIT) THEN
*        initialize generator ; see for seeds Bratley (1983)
         JX(2) = 1122334455
         JX(3) = 1408222472
         INIT = .TRUE.
      END IF
 
*     get next term in first stream = 40014 * JX(2) mod 2147483563
      K = JX(2) / 53668
      JX(2) = 40014 * (JX(2) - K * 53668)  -  K * 12211
      IF (JX(2).LT.0)  JX(2) = JX(2) + 2147483563
 
*     get next term in the second stream = 40692 * JX(3) mod 2147483399
      K = JX(3) / 52774
      JX(3) = 40692 * (JX(3) - K * 52774)  -  K * 3791
      IF (JX(3).LT.0)  JX(3) = JX(3) + 2147483399
 
*     set JX(1) = ((JX(3) + 2147483562 - JX(2)) mod 2147483562) + 1
      K = JX(3) - JX(2)
      IF (K.LE.0)  K = K + 2147483562
      JX(1) = K
 
*     put it on the interval (0,1)
      UNIFL = K * 4.656613E-10
 
      RETURN
      END
**==UPPERC.FOR  
      SUBROUTINE UPPERC (STRING)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) STRING
 
**    local variables
      INTEGER I,IC,L
      SAVE
 
      L = LEN (STRING)
      DO 10 I=1,L
*        convert lowercase letters
         IC = ICHAR(STRING(I:I))
         IF (IC.GE.97.AND.IC.LE.122) STRING(I:I) = CHAR(IC-32)
10    CONTINUE
 
      RETURN
      END
**==USEDUN.FOR  
      SUBROUTINE USEDUN (IST,IEND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER IST,IEND
 
**    Local variables
      INTEGER ISTEP,UNIT,IL,ILEN
      LOGICAL OPEN,FOUND
      CHARACTER*80 FILE
      SAVE
 
      ISTEP = 1
      IF (IST.GT.IEND) ISTEP = -1
      IF (IST.LT.10.OR.IST.GT.99.OR.IEND.LT.10.OR.IEND.GT.99)
     &   CALL FATALERR ('GETUN','unit number outside range')
 
      FOUND = .FALSE.
      DO UNIT=IST,IEND,ISTEP
         INQUIRE (UNIT=UNIT,OPENED=OPEN)
         IF (OPEN) THEN
            FILE = ' '
            INQUIRE (UNIT=UNIT,NAME=FILE)
            IL = ILEN (FILE)
            WRITE (*,'(A,I3,2A)')
     &        ' WARNING from USEDUN: Unit: ',UNIT,
     &        ' is in use for file: ',FILE(1:IL)
            FOUND = .TRUE.
         END IF
      END DO
 
      IF (.NOT.FOUND) WRITE (*,*) 'USEDUN: no open units found'
 
      RETURN
      END
**==VER4_11.FOR 
      SUBROUTINE VER4_11
      CONTINUE
      RETURN
      END
**==WARNING.FOR 
      SUBROUTINE WARNING (MODULE,MESSAG)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS
      CHARACTER*(*) MODULE, MESSAG
 
**    local variables
      INTEGER IL1,IL2,ILEN
      CHARACTER*1 DUMMY
      SAVE
 
      IL1 = ILEN (MODULE)
      IL2 = ILEN (MESSAG)
 
      IF (IL1.EQ.0.AND.IL2.EQ.0) THEN
         WRITE (*,'(A)')
     &   ' WARNING from unspecified source'
      ELSE IF (IL1.GT.0.AND.IL2.EQ.0) THEN
         WRITE (*,'(2A)')
     &   ' WARNING from ',MODULE(1:IL1)
      ELSE
         WRITE (*,'(4A)')
     &   ' WARNING from ',MODULE(1:IL1),': ',MESSAG(1:IL2)
      END IF
 
      READ (*,'(A)') DUMMY
 
      RETURN
      END
**==WORDS.FOR   
      SUBROUTINE WORDS (RECORD,ILW,SEPARS,IWBEG,IWEND,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILW,IWBEG,IWEND,IFND
      DIMENSION IWBEG(ILW),IWEND(ILW)
      CHARACTER*(*) RECORD,SEPARS
 
**    local variables
      INTEGER I,ICHR
      CHARACTER CHAR*1
      LOGICAL SEPAR,GOING
      SAVE
 
 
*     initial
      ICHR = LEN (RECORD)
      IFND = 0
 
      DO 10 I=1,ILW
         IWBEG(I) = 0
         IWEND(I) = 0
10    CONTINUE
      GOING = .FALSE.
 
*     search through record
      DO 20 I=1,ICHR
         CHAR  = RECORD(I:I)
         SEPAR = (INDEX(SEPARS,CHAR).NE.0)
 
         IF (.NOT.SEPAR .AND. .NOT.GOING) THEN
*           start new word
            IFND = IFND + 1
            IWBEG(IFND) = I
            GOING = .TRUE.
         END IF
 
         IF (SEPAR .AND. GOING) THEN
*           end of word found
            IWEND(IFND) = I - 1
            IF (IFND.EQ.ILW) RETURN
            GOING = .FALSE.
         END IF
 
20    CONTINUE
      IF (GOING) IWEND(IFND)=ICHR
 
      RETURN
      END
**==WRACHA.FOR  
      SUBROUTINE WRACHA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME,X(ILDEC)
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1, I2, ILEN
      INTEGER NCOL
      PARAMETER (NCOL=1)
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRACHA','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRACHA','error')
 
      XNAME_L = ILEN (XNAME)
 
      IF (IFND.EQ.1) THEN
         WRITE (UNIT,'(1X,5A)')
     &         XNAME(1:XNAME_L),' = 1*',Q,X(1),Q
      ELSE
         WRITE (UNIT,'(1X,2A,30A)')
     &         XNAME(1:XNAME_L),' = ',Q,X(1),Q,
     &         (',',Q,X(I1),Q,I1=2,MIN (NCOL, IFND))
 
         IF (IFND.GT.NCOL) THEN
*           remaining data have to be written
            DO I1=NCOL+1,IFND,NCOL
               WRITE (UNIT,'(1X,2A,30A)')
     &            SPACE(1:XNAME_L),'   ',Q,X(I1),Q,
     &            (',',Q,X(I2),Q,I2=I1+1,MIN (NCOL+I1-1, IFND))
            END DO
         END IF
      END IF
 
      RETURN
      END
**==WRADOU.FOR  
      SUBROUTINE WRADOU (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      INTEGER ILDEC,IFND
      DOUBLE PRECISION X(ILDEC)
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1, I2, ILEN
      INTEGER NCOL
      PARAMETER (NCOL=3)
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRADOU','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRADOU','error')
 
      XNAME_L = ILEN (XNAME)
 
      IF (IFND.EQ.1) THEN
         WRITE (UNIT,'(1X,2A,1P,G23.16)')
     &         XNAME(1:XNAME_L),' = 1*',X(1)
      ELSE
         WRITE (UNIT,'(1X,2A,1P,G23.16,10(A,G23.16))')
     &      XNAME(1:XNAME_L),' = ',X(1),
     &      (',',X(I1),I1=2,MIN (NCOL, IFND))
 
         IF (IFND.GT.NCOL) THEN
*           remaining data have to be written
            DO I1=NCOL+1,IFND,NCOL
               WRITE (UNIT,'(1X,2A,1P,G23.16,10(A,G23.16))')
     &            SPACE(1:XNAME_L),'   ',
     &            X(I1),(',',X(I2),I2=I1+1,MIN (NCOL+I1-1, IFND))
            END DO
         END IF
      END IF
 
      RETURN
      END
**==WRAINT.FOR  
      SUBROUTINE WRAINT (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      INTEGER ILDEC,IFND
      INTEGER X(ILDEC)
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1, I2, ILEN
      INTEGER NCOL
      PARAMETER (NCOL=3)
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRAINT','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRAINT','error')
 
      XNAME_L = ILEN (XNAME)
 
      IF (IFND.EQ.1) THEN
         WRITE (UNIT,'(1X,2A,I11)')
     &         XNAME(1:XNAME_L),' = 1*',X(1)
      ELSE
         WRITE (UNIT,'(1X,2A,I11,10(A,I11))')
     &      XNAME(1:XNAME_L),' = ',X(1),
     &      (',',X(I1),I1=2,MIN (NCOL, IFND))
 
         IF (IFND.GT.NCOL) THEN
*           remaining data have to be written
            DO I1=NCOL+1,IFND,NCOL
               WRITE (UNIT,'(1X,2A,I11,10(A,I11))')
     &            SPACE(1:XNAME_L),'   ',
     &            X(I1),(',',X(I2),I2=I1+1,MIN (NCOL+I1-1, IFND))
            END DO
         END IF
      END IF
 
      RETURN
      END
**==WRALOG.FOR  
      SUBROUTINE WRALOG (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME
      LOGICAL X(ILDEC)
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1, ILEN
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRALOG','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRALOG','error')
 
      XNAME_L = ILEN (XNAME)
 
      IF (IFND.EQ.1) THEN
         IF (X(1)) THEN
            WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = 1*.TRUE.'
         ELSE
            WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = 1*.FALSE.'
         END IF
      ELSE
         IF (X(1)) THEN
            WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = .TRUE.'
         ELSE
            WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = .FALSE.'
         END IF
 
         IF (IFND.GT.1) THEN
*           remaining data have to be written
            DO I1=2,IFND
               IF (X(I1)) THEN
                  WRITE (UNIT,'(1X,2A)')  SPACE(1:XNAME_L),'   .TRUE.'
               ELSE
                  WRITE (UNIT,'(1X,2A)')  SPACE(1:XNAME_L),'   .FALSE.'
               END IF
            END DO
         END IF
      END IF
 
      RETURN
      END
**==WRAREA.FOR  
      SUBROUTINE WRAREA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      INTEGER ILDEC,IFND
      REAL X(ILDEC)
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1, I2, ILEN
      INTEGER NCOL
      PARAMETER (NCOL=3)
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRAREA','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRAREA','error')
 
      XNAME_L = ILEN (XNAME)
 
      IF (IFND.EQ.1) THEN
         WRITE (UNIT,'(1X,2A,1P,G13.6)')
     &         XNAME(1:XNAME_L),' = 1*',X(1)
      ELSE
         WRITE (UNIT,'(1X,2A,1P,G13.6,10(A,G13.6))')
     &      XNAME(1:XNAME_L),' = ',X(1),
     &      (',',X(I1),I1=2,MIN (NCOL, IFND))
 
         IF (IFND.GT.NCOL) THEN
*           remaining data have to be written
            DO I1=NCOL+1,IFND,NCOL
               WRITE (UNIT,'(1X,2A,1P,G13.6,10(A,G13.6))')
     &            SPACE(1:XNAME_L),'   ',
     &            X(I1),(',',X(I2),I2=I1+1,MIN (NCOL+I1-1, IFND))
            END DO
         END IF
      END IF
 
      RETURN
      END
**==WRATIM.FOR  
      SUBROUTINE WRATIM (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      INTEGER ILDEC,IFND
      DOUBLE PRECISION X(ILDEC)
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1, STRING_L, ILEN
      CHARACTER*80 STRING
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRATIM','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRATIM','error')
 
      DO I1=1,IFND
         CALL DTDPST ('YEAR-MONTHST-DAY_HOUR:MINUTE:SECONDS.FSECONDS',
     &                 X(I1),STRING)
         STRING_L = ILEN (STRING)
         IF (IFND.EQ.1) THEN
            XNAME_L = ILEN (XNAME)
            WRITE (UNIT,'(1X,3A)')
     &         XNAME(1:XNAME_L),' = 1*',STRING(1:STRING_L)
         ELSE IF (I1.EQ.1) THEN
            XNAME_L = ILEN (XNAME)
            WRITE (UNIT,'(1X,3A)')
     &         XNAME(1:XNAME_L),' = ',STRING(1:STRING_L)
         ELSE
            WRITE (UNIT,'(1X,3A)')
     &         SPACE(1:XNAME_L),'   ',STRING(1:STRING_L)
         END IF
      END DO
 
      RETURN
      END
**==WRINIT.FOR  
      SUBROUTINE WRINIT (UNIT_X,FILE)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      INTEGER UNIT_X
      CHARACTER*(*) FILE
 
**    Include files
      INCLUDE 'wr_sys.inc'
 
*     Local variables
      LOGICAL OPEN
      INTEGER IL, ILEN
      SAVE
 
      Q    = CHAR (39)
      UNIT = UNIT_X
 
      IL   = ILEN (FILE)
      IF (IL.EQ.0) CALL FATALERR ('WRINIT','empty file name')
 
      INQUIRE (UNIT=UNIT,OPENED=OPEN)
      IF (.NOT.OPEN) CALL FOPENS (UNIT,FILE,'NEW','DEL')
      INIT = .TRUE.
 
      RETURN
      END
 
      BLOCK DATA WRINIT_DATA
      IMPLICIT NONE
 
      INCLUDE 'wr_sys.inc'
      SAVE
      DATA INIT /.FALSE./
      DATA SPACE /' '/
      END
 
**==WRSCHA.FOR  
      SUBROUTINE WRSCHA (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME,X
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER X_L, ILEN
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRSCHA','system not initialized')
 
      X_L = MAX (1,ILEN (X))
      WRITE (UNIT,'(1X,5A)') XNAME,' = ',Q,X(1:X_L),Q
 
      RETURN
      END
**==WRSDOU.FOR  
      SUBROUTINE WRSDOU (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      DOUBLE PRECISION X
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRSDOU','system not initialized')
      WRITE (UNIT,'(1X,2A,1P,G23.16)') XNAME,' = ',X
 
      RETURN
      END
**==WRSINT.FOR  
      SUBROUTINE WRSINT (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      INTEGER X
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRSINT','system not initialized')
      WRITE (UNIT,'(1X,2A,I11)') XNAME,' = ',X
 
      RETURN
      END
**==WRSLOG.FOR  
      SUBROUTINE WRSLOG (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      LOGICAL X
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRSLOG','system not initialized')
 
      IF (X) THEN
         WRITE (UNIT,'(1X,2A)') XNAME,' = .TRUE.'
      ELSE
         WRITE (UNIT,'(1X,2A)') XNAME,' = .FALSE.'
      END IF
 
      RETURN
      END
**==WRSREA.FOR  
      SUBROUTINE WRSREA (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      REAL X
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRSREA','system not initialized')
      WRITE (UNIT,'(1X,2A,1P,G13.6)') XNAME,' = ',X
 
      RETURN
      END
 
**==WRSTIM.FOR  
      SUBROUTINE WRSTIM (XNAME,X)
      IMPLICIT NONE
 
*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      DOUBLE PRECISION X
 
**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      CHARACTER*80 STRING
      INTEGER STRING_L, ILEN
      SAVE
 
      IF (.NOT.INIT) CALL FATALERR ('WRSTIM','system not initialized')
 
      CALL DTDPST ('YEAR-MONTHST-DAY_HOUR:MINUTE:SECONDS.FSECONDS',
     &              X,STRING)
      STRING_L = ILEN (STRING)
      WRITE (UNIT,'(1X,3A)') XNAME,' = ',STRING(1:STRING_L)
 
      RETURN
      END
