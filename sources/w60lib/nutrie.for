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
      SUBROUTINE NUTRIE (CRFILE, IUPL,  IUOUT,  IULOG,
     &                  NBASE,  PBASE,  KBASE,  NREC,   PREC,   KREC,
     &                  LVOPT,  STOPT,  SOOPT,  DUROPT,
     &                  TWLVW,  TWSTW,  TWSOW, 
     &                  NBAS,   PBAS,   KBAS,   LVBAS1, STBAS1, SOBAS1,
     &                  NFERTO, NFERTW, PFERTO, PFERTW, KFERTO, KFERTW,
     &                  RATBAS, HIBAS)

*     Chapter 3 in documentation WOFOST Version 4.1 (1988)

*     This routine calculates nutrient requirements and nutrient
*     limited yields according to the QUEFTS-system (Janssen et
*     al., 1988, and Janssen and Wolf, 1988) and generates output.

*     Subroutines and functions called: AFGEN.
*     Uses results of WOFOST crop growth simulation model.
*     Should be called after calculation of waterlimited production  
*     by WOFOST is finished
*     Normally long term mean water limited yield is used as input.
*     Should be linked to TTUTIL library.

*     FORMAL PARAMETERS
*     Name    Type Description                              Units  I/O
*     ------  --  ----------------------------------------  ------ ---
*     CRFILE  C   name of crop (=plant) data file                    I
*     IUPL    I   iunit number for plant data file                   I
*     IUOUT   I   iunit number of output file for information on
*                 possible syntax errors in plant data file          I
*     IULOG   I   iunit number of log file for information on        I
*
*     Base soil supply of nutrients for standard crop with 
*     growth duration of 120 days : 
*     NBASE   R   base soil supply of nitrogen for ...      kg ha-1  I
*     PBASE   R   base soil supply of phosphorus for ...    kg ha-1  I
*     KBASE   R   base soil supply of potassium for ...     kg ha-1  I
*
*     Recovery fractions of applied fertilizer :
*     NREC    R   recovery fraction of applied nitrogen       -      I
*     PREC    R   recovery fraction of applied phosphorus     -      I
*     KREC    R   recovery fraction of applied potassium      -      I

*     Amounts of crop biomass are totals of dry-matter accumulated 
*     over growing season, and may be annual values or long term 
*     average values (e.g. in case of multiple water-limited 
*     production runs): 
*     LVOPT   R   potential production of leaves            kg ha-1  I
*     STOPT   R   potential production of stems             kg ha-1  I
*     SOOPT   R   potential production of storage organs    kg ha-1  I
*     DUROPT  R   Duration of crop growth cycle pot. prod.  days     I
*     TWLVW   R   total dry weight of leaves (dead and living;
*                 under water-limited production situation  kg ha-1  I
*     TWSTW   R   total dry weight of stems (dead and living; 
*                 under water-limited production situation  kg ha-1  I
*     TWSOW   R   total dry weight of storage organs
*                 under water-limited production situation  kg ha-1  I

*     Actual soil supply of nutrients is function of crop 
*     growth duration :
*     NBAS    R   actual soil supply of nitrogen            kg ha-1  O
*     PBAS    R   actual soil supply of phosphorus          kg ha-1  O
*     KBAS    R   actual soil supply of potassium           kg ha-1  O

*     LVBAS1  R   nutrient-limited mass of leaves           kg ha-1  O
*     STBAS1  R   nutrient-limited mass of stems            kg ha-1  O
*     SOBAS1  R   nutrient-limited yield (of storage organs)kg ha-1  O
*     NFERTO  R   required fertilizer nitrogen for 
*                 potential crop yield                      kg ha-1  O
*     NFERTW  R   required fertilizer nitrogen for 
*                 water-limited crop yield                  kg ha-1  O
*     PFERTO  R   required fertilizer phosphorus for
*                 potential crop yield                      kg ha-1  O
*     PFERTW  R   required fertilizer phosphorus for
*                 water-limited crop yield                  kg ha-1  O
*     KFERTO  R   required fertilizer potassium for 
*                 potential crop yield                      kg ha-1  O
*     KFERTW  R   required fertilizer potassium for 
*                 water-limited crop yield                  kg ha-1  O
*     RATBAS  R   storage organs - vegetative organs ratio for
*                 nutrient-limited crop production          kg kg-1  O
*     HIBAS   R   harvest index of nutrient limited production   
*                 (calculated from the sum of living and dead
*                 plant material)                           kg kg-1  O

*     Author: Joost Wolf, January 1987, 
*             revised by Kees van Diepen, August 1992
*    +----------------------------------------------------------------+
*    | MODIFICATION                                                   |
*    | author:       Hendrik Boogaard                                 |
*    | date:         15-06-1998                                       |
*    | reason:       - bug variable name          					|
*    | modification:                                                  |
*    | PUPT should be NPUPT (under crops nitrogen uptake)  			|
*    +----------------------------------------------------------------+


* 3.1 declarations
      IMPLICIT REAL(A-Z)
*     formal parameters
      CHARACTER*(*) CRFILE
      INTEGER IUPL, IUOUT, IULOG
      REAL NBASE,  PBASE,  KBASE,  NREC,   PREC,   KREC
      REAL LVOPT,  STOPT,  SOOPT,  DUROPT
      REAL TWLVW,  TWSTW,  TWSOW
      REAL NBAS,   PBAS,   KBAS,   LVBAS1, STBAS1, SOBAS1
      REAL NFERTO, NFERTW, PFERTO, PFERTW, KFERTO, KFERTW
      REAL RATBAS, HIBAS
**    local variables
      INTEGER IORGAN
*     plant nutrient parameters (formerly in COMMON BLOCK)
      REAL NMINSO, NMINVE, NMAXSO, NMAXVE
      REAL PMINSO, PMINVE, PMAXSO, PMAXVE
      REAL KMINSO, KMINVE, KMAXSO, KMAXVE, YZERO, NFIX
 
      REAL UPTB(14)
      SAVE
 
*     fraction uptake from soil supply as function of length of season,
*     standard season is 120 days 
      DATA UPTB/0.,0.,40.,0.4,80.,0.7,120.,1.,240.,1.6,360.,2.,1000.,2./

*     initialize crop data reading
*     read minimum and maximum concentration of NPK-nutrients 
*     in vegetative organs and in storage organs (kg kg-1)

      CALL RDINIT(IUPL,IUOUT,CRFILE)
      CALL RDSREA('NMINSO',NMINSO)
      CALL RDSREA('NMINVE',NMINVE)
      CALL RDSREA('NMAXSO',NMAXSO)
      CALL RDSREA('NMAXVE',NMAXVE)
      CALL RDSREA('PMINSO',PMINSO)
      CALL RDSREA('PMINVE',PMINVE)
      CALL RDSREA('PMAXSO',PMAXSO)
      CALL RDSREA('PMAXVE',PMAXVE)
      CALL RDSREA('KMINSO',KMINSO)
      CALL RDSREA('KMINVE',KMINVE)
      CALL RDSREA('KMAXSO',KMAXSO)
      CALL RDSREA('KMAXVE',KMAXVE)
*     maximum amount of vegetative organs at zero yield of storage organs 
      CALL RDSREA('YZERO',YZERO)
*     fraction of crop's nitrogen uptake supplied by biological fixation 
      CALL RDSREA('NFIX',NFIX)

*     end of crop input section
 
      NFERTO = 0.
      NFERTW = 0.
      PFERTO = 0.
      PFERTW = 0.
      KFERTO = 0.
      KFERTW = 0.
 
* 3.2 base nutrient supply of soil during growing season
      NBAS = NBASE*AFGEN(UPTB,14,DUROPT)
      PBAS = PBASE*AFGEN(UPTB,14,DUROPT)
      KBAS = KBASE*AFGEN(UPTB,14,DUROPT)
*     nutrient uptake at zero yield
      NZERO = YZERO*(1.-NFIX)*(NMAXVE+2.*NMINVE)/3.
      PZERO = YZERO*(PMAXVE+2.*PMINVE)/3.
      KZERO = YZERO*(KMAXVE+2.*KMINVE)/3.
 
* 3.3 nutrient requirements for potential and waterlimited
*     yields at balanced concentrations
      NBALVE = (NMAXVE+NMINVE)/2.
      NBALSO = (NMAXSO+NMINSO)/2.
      PBALVE = (PMAXVE+PMINVE)/2.
      PBALSO = (PMAXSO+PMINSO)/2.
      KBALVE = (KMAXVE+KMINVE)/2.
      KBALSO = (KMAXSO+KMINSO)/2.
      NREQO  = (NBALVE*(LVOPT+STOPT)+NBALSO*SOOPT)*(1.-NFIX)
      NREQW  = (NBALVE*(TWLVW+TWSTW)+NBALSO*TWSOW)*(1.-NFIX)
      PREQO  = PBALVE*(LVOPT+STOPT)+PBALSO*SOOPT
      PREQW  = PBALVE*(TWLVW+TWSTW)+PBALSO*TWSOW
      KREQO  = KBALVE*(LVOPT+STOPT)+KBALSO*SOOPT
      KREQW  = KBALVE*(TWLVW+TWSTW)+KBALSO*TWSOW
*     nutrient requirements at accumulated and diluted nutrient conc.
      NREQA  = (NMAXVE*(LVOPT+STOPT)+NMAXSO*SOOPT)*(1.-NFIX)
      NREQD  = (NMINVE*(LVOPT+STOPT)+NMINSO*SOOPT)*(1.-NFIX)
      PREQA  = PMAXVE*(LVOPT+STOPT)+PMAXSO*SOOPT
      PREQD  = PMINVE*(LVOPT+STOPT)+PMINSO*SOOPT
      KREQA  = KMAXVE*(LVOPT+STOPT)+KMAXSO*SOOPT
      KREQD  = KMINVE*(LVOPT+STOPT)+KMINSO*SOOPT
*     fertilizer requirements
      NFERTO = MAX(0.,(NREQO-NBAS)/NREC)
      NFERTW = MAX(0.,(NREQW-NBAS)/NREC)
      PFERTO = MAX(0.,(PREQO-PBAS)/PREC)
      PFERTW = MAX(0.,(PREQW-PBAS)/PREC)
      KFERTO = MAX(0.,(KREQO-KBAS)/KREC)
      KFERTW = MAX(0.,(KREQW-KBAS)/KREC)
*     plant organ
      IF ( SOOPT.GT.200. ) THEN
         YLDO   = SOOPT
         IORGAN = 3
      ELSE IF ( STOPT.LE.200. ) THEN
         YLDO   = MAX(5.,LVOPT)
         IORGAN = 1
      ELSE
         YLDO   = STOPT
         IORGAN = 2
      END IF
      IF ( IORGAN.NE.3 ) THEN
         NZERO  = 0.
         PZERO  = 0.
         KZERO  = 0.
      END IF
 
* 3.4 yield /nutrient uptake ratios at accumulated
*     and diluted N-, P-, and K-concentrations in the plant parts
      YNRATD = YLDO/MAX(0.1,NREQD-NZERO)
      YNRATA = YLDO/MAX(0.1,NREQA-NZERO)
      YPRATD = YLDO/MAX(0.1,PREQD-PZERO)
      YPRATA = YLDO/MAX(0.1,PREQA-PZERO)
      YKRATD = YLDO/MAX(0.1,KREQD-KZERO)
      YKRATA = YLDO/MAX(0.1,KREQA-KZERO)
*     crop's nitrogen uptake from soil
      IF ( NBAS.LE.NZERO+YPRATA*(PBAS-PZERO)/YNRATD ) THEN
         NPUPT = NBAS
      ELSE IF ( NBAS.LT.(NZERO+(PBAS-PZERO)*(2.*YPRATD/YNRATA-YPRATA/
     &          YNRATD)) ) THEN
         NPUPT = NBAS-0.25*(NBAS-NZERO-YPRATA*(PBAS-PZERO)/YNRATD)
     &           **2./((YPRATD/YNRATA-YPRATA/YNRATD)*(PBAS-PZERO))
      ELSE
         NPUPT  = NZERO+YPRATD*(PBAS-PZERO)/YNRATA
      END IF
      IF ( NBAS.LE.NZERO+YKRATA*(KBAS-KZERO)/YNRATD ) THEN
         NKUPT = NBAS
      ELSE IF ( NBAS.LT.(NZERO+(KBAS-KZERO)*(2.*YKRATD/YNRATA-YKRATA/
     &          YNRATD)) ) THEN
         NKUPT = NBAS-0.25*(NBAS-NZERO-YKRATA*(KBAS-KZERO)/YNRATD)
     &           **2./((YKRATD/YNRATA-YKRATA/YNRATD)*(KBAS-KZERO))
      ELSE
         NKUPT = NZERO+YKRATD*(KBAS-KZERO)/YNRATA
      END IF
      NPUPT = MAX(0.,NPUPT)
      NKUPT = MAX(0.,NKUPT)
      NUPT  = MIN(NPUPT,NKUPT)
*     crop's phosphorus uptake from soil
      IF ( PBAS.LE.PZERO+YNRATA*(NBAS-NZERO)/YPRATD ) THEN
         PNUPT = PBAS
      ELSE IF ( PBAS.LT.(PZERO+(NBAS-NZERO)*(2.*YNRATD/YPRATA-YNRATA/
     &          YPRATD)) ) THEN
         PNUPT = PBAS-0.25*(PBAS-PZERO-YNRATA*(NBAS-NZERO)/YPRATD)
     &           **2./((YNRATD/YPRATA-YNRATA/YPRATD)*(NBAS-NZERO))
      ELSE
         PNUPT = PZERO+YNRATD*(NBAS-NZERO)/YPRATA
      END IF
      IF ( PBAS.LE.PZERO+YKRATA*(KBAS-KZERO)/YPRATD ) THEN
         PKUPT = PBAS
      ELSE IF ( PBAS.LT.(PZERO+(KBAS-KZERO)*(2.*YKRATD/YPRATA-YKRATA/
     &          YPRATD)) ) THEN
         PKUPT = PBAS-0.25*(PBAS-PZERO-YKRATA*(KBAS-KZERO)/YPRATD)
     &           **2./((YKRATD/YPRATA-YKRATA/YPRATD)*(KBAS-KZERO))
      ELSE
         PKUPT = PZERO+YKRATD*(KBAS-KZERO)/YPRATA
      END IF
      PNUPT = MAX(0.,PNUPT)
      PKUPT = MAX(0.,PKUPT)
      PUPT  = MIN(PNUPT,PKUPT)
*     crop's potassium uptake from soil
      IF ( KBAS.LE.KZERO+YNRATA*(NBAS-NZERO)/YKRATD ) THEN
         KNUPT = KBAS
      ELSE IF ( KBAS.LT.(KZERO+(NBAS-NZERO)*(2.*YNRATD/YKRATA-YNRATA/
     &          YKRATD)) ) THEN
         KNUPT = KBAS-0.25*(KBAS-KZERO-YNRATA*(NBAS-NZERO)/YKRATD)
     &           **2./((YNRATD/YKRATA-YNRATA/YKRATD)*(NBAS-NZERO))
      ELSE
         KNUPT = KZERO+YNRATD*(NBAS-NZERO)/YKRATA
      END IF
      IF ( KBAS.LE.KZERO+YPRATA*(PBAS-PZERO)/YKRATD ) THEN
         KPUPT = KBAS
      ELSE IF ( KBAS.LT.(KZERO+(PBAS-PZERO)*(2.*YPRATD/YKRATA-YPRATA/
     &          YKRATD)) ) THEN
         KPUPT = KBAS-0.25*(KBAS-KZERO-YPRATA*(PBAS-PZERO)/YKRATD)
     &           **2./((YPRATD/YKRATA-YPRATA/YKRATD)*(PBAS-PZERO))
      ELSE
         KPUPT = KZERO+YPRATD*(PBAS-PZERO)/YKRATA
      END IF
      KNUPT = MAX(0.,KNUPT)
      KPUPT = MAX(0.,KPUPT)
      KUPT  = MIN(KNUPT,KPUPT)
*     N-,P-, and K-limited yields at accumulated
*     and diluted nutrient concentration
      YNA = YNRATA*MAX(0.,NUPT-NZERO)
      YND = YNRATD*MAX(0.,NUPT-NZERO)
      YPA = YPRATA*MAX(0.,PUPT-PZERO)
      YPD = YPRATD*MAX(0.,PUPT-PZERO)
      YKA = YKRATA*MAX(0.,KUPT-KZERO)
      YKD = YKRATD*MAX(0.,KUPT-KZERO)
 
* 3.5 estimation of yield (EYNP,EYPN) from N- and P-limited yields
      LYD = MIN(YND,YPD,YKD,YLDO)
      IF ( YND.LE.YPA ) THEN
         EYNP = YND
         IF ( YND.GT.LYD ) EYNP = LYD
         GOTO 100
      ELSE IF ( YNA.GE.LYD.OR.YND.LE.LYD ) THEN
         IF ( YNA.LT.LYD.AND.YND.EQ.LYD ) THEN
            LYD = MIN(YPD,YKD,YLDO)
         ELSE
            EYNP = LYD
            GOTO 100
         END IF
      END IF
      EYNP = YPA+2.*(LYD-YPA)*(NUPT-NZERO-YPA/YNRATD)
     &       /(LYD/YNRATA-YPA/YNRATD)-(LYD-YPA)*(NUPT-NZERO-YPA/YNRATD)
     &       **2./(LYD/YNRATA-YPA/YNRATD)**2.
      IF ( YPA.GT.LYD ) EYNP = LYD
100   LYD = MIN(YND,YPD,YKD,YLDO)
      IF ( YPD.LE.YNA ) THEN
         EYPN = YPD
         IF ( YPD.GT.LYD ) EYPN = LYD
         GOTO 200
      ELSE IF ( YPA.GE.LYD.OR.YPD.LE.LYD ) THEN
         IF ( YPA.LT.LYD.AND.YPD.EQ.LYD ) THEN
            LYD = MIN(YND,YKD,YLDO)
         ELSE
            EYPN = LYD
            GOTO 200
         END IF
      END IF
      EYPN = YNA+2.*(LYD-YNA)*(PUPT-PZERO-YNA/YPRATD)
     &       /(LYD/YPRATA-YNA/YPRATD)-(LYD-YNA)*(PUPT-PZERO-YNA/YPRATD)
     &       **2./(LYD/YPRATA-YNA/YPRATD)**2.
      IF ( YNA.GT.LYD ) EYPN = LYD
*     estimation of yield (EYNK, EYKN) from N- and K-limited yields
200   LYD = MIN(YND,YPD,YKD,YLDO)
      IF ( YND.LE.YKA ) THEN
         EYNK = YND
         IF ( YND.GT.LYD ) EYNK = LYD
         GOTO 300
      ELSE IF ( YNA.GE.LYD.OR.YND.LE.LYD ) THEN
         IF ( YNA.LT.LYD.AND.YND.EQ.LYD ) THEN
            LYD = MIN(YPD,YKD,YLDO)
         ELSE
            EYNK = LYD
            GOTO 300
         END IF
      END IF
      EYNK = YKA+2.*(LYD-YKA)*(NUPT-NZERO-YKA/YNRATD)
     &       /(LYD/YNRATA-YKA/YNRATD)-(LYD-YKA)*(NUPT-NZERO-YKA/YNRATD)
     &       **2./(LYD/YNRATA-YKA/YNRATD)**2.
      IF ( YKA.GT.LYD ) EYNK = LYD
300   LYD = MIN(YND,YPD,YKD,YLDO)
      IF ( YKD.LE.YNA ) THEN
         EYKN = YKD
         IF ( YKD.GT.LYD ) EYKN = LYD
         GOTO 400
      ELSE IF ( YKA.GE.LYD.OR.YKD.LE.LYD ) THEN
         IF ( YKA.LT.LYD.AND.YKD.EQ.LYD ) THEN
            LYD = MIN(YND,YPD,YLDO)
         ELSE
            EYKN = LYD
            GOTO 400
         END IF
      END IF
      EYKN = YNA+2.*(LYD-YNA)*(KUPT-KZERO-YNA/YKRATD)
     &       /(LYD/YKRATA-YNA/YKRATD)-(LYD-YNA)*(KUPT-KZERO-YNA/YKRATD)
     &       **2./(LYD/YKRATA-YNA/YKRATD)**2.
      IF ( YNA.GT.LYD ) EYKN = LYD
*     estimation of yield (EYPK, EYKP) from P- and K-limited yields
400   LYD = MIN(YND,YPD,YKD,YLDO)
      IF ( YPD.LE.YKA ) THEN
         EYPK = YPD
         IF ( YPD.GT.LYD ) EYPK = LYD
         GOTO 500
      ELSE IF ( YPA.GE.LYD.OR.YPD.LE.LYD ) THEN
         IF ( YPA.LT.LYD.AND.YPD.EQ.LYD ) THEN
            LYD = MIN(YND,YKD,YLDO)
         ELSE
            EYPK = LYD
            GOTO 500
         END IF
      END IF
      EYPK = YKA+2.*(LYD-YKA)*(PUPT-PZERO-YKA/YPRATD)
     &       /(LYD/YPRATA-YKA/YPRATD)-(LYD-YKA)*(PUPT-PZERO-YKA/YPRATD)
     &       **2./(LYD/YPRATA-YKA/YPRATD)**2.
      IF ( YKA.GT.LYD ) EYPK = LYD
500   LYD = MIN(YND,YPD,YKD,YLDO)
      IF ( YKD.LE.YPA ) THEN
         EYKP = YKD
         IF ( YKD.GT.LYD ) EYKP = LYD
         GOTO 600
      ELSE IF ( YKA.GE.LYD.OR.YKD.LE.LYD ) THEN
         IF ( YKA.LT.LYD.AND.YKD.EQ.LYD ) THEN
            LYD = MIN(YND,YPD,YLDO)
         ELSE
            EYKP = LYD
            GOTO 600
         END IF
      END IF
      EYKP = YPA+2.*(LYD-YPA)*(KUPT-KZERO-YPA/YKRATD)
     &       /(LYD/YKRATA-YPA/YKRATD)-(LYD-YPA)*(KUPT-KZERO-YPA/YKRATD)
     &       **2./(LYD/YKRATA-YPA/YKRATD)**2.
      IF ( YPA.GT.LYD ) EYKP = LYD
*     estimation of yield (YE) from N-, P-, and K-limited yields
600   LYD = MIN(YND,YPD,YKD,YLDO)
      YE = MIN(LYD,(EYNP+EYPN+EYNK+EYKN+EYPK+EYKP)/6.)
*     nutrient-limited yields of plant organs
      IF ( IORGAN.EQ.3 ) THEN
         SOBAS1 = YE
         LVBAS1 = (LVOPT/(LVOPT+STOPT)
     &            *((STOPT+LVOPT-YZERO)*YE/SOOPT+YZERO))
         IF ( YE.LE.0. ) 
     &   LVBAS1 = LVOPT/(LVOPT+STOPT)
     &            *MIN(YZERO,NUPT/(0.333*(NMAXVE+2.*NMINVE)*(1.-NFIX)),
     &                 PUPT/(0.333*(PMAXVE+2.*PMINVE)),
     &                 KUPT/(0.333*(KMAXVE+2.*KMINVE)))
         STBAS1 = (STOPT/LVOPT)*LVBAS1
      ELSE IF ( IORGAN.NE.2 ) THEN
         LVBAS1 = YE
         STBAS1 = (STOPT/LVOPT)*LVBAS1
         SOBAS1 = (SOOPT/LVOPT)*LVBAS1
      ELSE
         STBAS1 = YE
         LVBAS1 = (LVOPT/STOPT)*STBAS1
         SOBAS1 = (SOOPT/STOPT)*STBAS1
      END IF
      RATBAS = SOBAS1/MAX(1.,LVBAS1+STBAS1)
      HIBAS  = SOBAS1/MAX(1.,LVBAS1+STBAS1+SOBAS1)
 
      RETURN
*     end of subroutine NUTRIE
      END
