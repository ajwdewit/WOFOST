*     --------- COMMON: WSCFIL -------------------------------- *
*                       Weathr Subsystem Char FILe names
*                       note: MAXFNM depends on system.
      INTEGER    MAXFNM
      PARAMETER (MAXFNM = 256)

      CHARACTER*(MAXFNM) cPATH, cFNAME, cLOG
      COMMON   /WSCFIL/  cPATH, cFNAME, cLOG
