*     --------- COMMON: WSNFIL  ------------------------------- *
*                       Weathr Subsystem Num  FILe flags and logf. unit.
*     WARNFL  - .TRUE. if to create warings (screen and/or file)
*     ERRFL   - .TRUE. if to create error messages  ( " )

*     OF      - Output File flags
*     OFxxxW  - warning flag,   OFxxxF - fatal error flag
*     OFLOGx  - logfile flag,   OFOUTx - output (screen) flag
*     WARNFL  - .TRUE. if to create warings (screen and/or file)
*     ERRFL   - .TRUE. if to create error messages  ( " )

      INTEGER    OFOUTF,OFOUTW,OFLOGF,OFLOGW
      PARAMETER (OFOUTF = 1,
     &           OFOUTW = 2,
     &           OFLOGF = 3,
     &           OFLOGW = 4)

      INTEGER    NOF
      PARAMETER  (NOF=4)

      LOGICAL    cOF(NOF)
*                unit of logfile
      INTEGER    cLUNIT
*                error flag from STINFO to WEATHR (12345 = ok)
      INTEGER    cSTERR
      LOGICAL    cWARNFL, cERRFL

      COMMON /WSNFIL/ cOF, cLUNIT, cWARNFL, cERRFL, cSTERR
