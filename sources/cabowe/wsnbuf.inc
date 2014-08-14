*     --------- COMMON: WSNBUF ------------------------------- *
*                       Weathr Subsystem Num BUFfer

*     BFATTR = attributes of data: DFINT, DFEST, DFUNDE
*     BFVALS = values

      INTEGER    NRDAYS, NRDP
      PARAMETER (NRDP   = 6,
     &           NRDAYS = 366)

      REAL       cLONG,cLAT,cALT,cA,cB
      REAL       cBFVALS(NRDAYS,NRDP),cBFSUN(NRDAYS)
      INTEGER    cBFATTR(NRDAYS,NRDP)
      INTEGER    cRAD_TYPE

      COMMON /WSNBUF/ cLONG,cLAT,cALT,cA,cB,
     &                cBFVALS,cBFSUN,cBFATTR,cRAD_TYPE
