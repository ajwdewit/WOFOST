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
      SUBROUTINE PRIJRC 
     &    (ITASK,IUNIT,IDAY,IYEAR,OUTDIR,FILGIS,IWB,RUNNAM)

*     formal parameters
      INTEGER ITASK, IUNIT, IYEAR, IDAY, IWB
      CHARACTER*(*) OUTDIR, FILGIS, RUNNAM

*     common block
      INTEGER IDOST,IDWST,IDOSJ,IDWSJ
      REAL LAI, MRES
      COMMON /CROPDO/WLV,WST,WSO,LAI,DVS,TSUM,TRA,
     &               GASS,MRES,DMI,TAGP,
     &               IDOST, IDWST, IDOSJ, IDWSJ

      INTEGER IDDRY, IDFLPP, IDFLWL, IDWET  
      REAL LAMXPP, LAMXWL
      COMMON /CRPSI/ YLVPP, YSTPP , YSOPP, HIPP  , RATPP, IDFLPP,
     &               DURPP, TRATPP, TRCPP, YRTPP , YAGPP, GASPP ,
     &               RESPP, LAMXPP, YLVWL, YSTWL , YSOWL, HIWL  ,
     &               RATWL, IDFLWL, DURWL, TRATWL, TRCWL, YRTWL ,
     &               YAGWL, GASWL , RESWL, LAMXWL, IDWET, IDDRY

*!!!  IEDEC is forced number of last decade in the year for which 
*!!   output is printed (its function (temporarily?) removed in feb 93)
*!!   ISHIFT is number of decades before harvest date to be printed (now: =3)
*!!   Crossing year boundary is now accepted by the program, but might lead to 
*!!   some irregularity in output, may be problems for aggregation procedure.
*!!   IDATMP (day number) is removed from output
*!!   regular output to file 'cisr.dec' (crop interim simulation results per
*!!   decade), binary file. For checking a formatted file can be used. This 
*!!   requires commenting and decommenting source lines for naming, opening 
*!!   and writing.
*!!   Record length of file 'Unform Seq' not relevant for open statement
**    local parameters
      INTEGER I1, I2, IPP, IPW, IDEC, IEDEC, IP1, IP2, ISHIFT, ILU
      INTEGER ILEN, ILGIS
      LOGICAL INIT, LEAP
      CHARACTER TMPSTR*80, FILFMT*80, LRUNNM*6

*     adjustable arrays
      INTEGER IMNDEC
      PARAMETER (IMNDEC=36)
      INTEGER ID1(IMNDEC), ID2(IMNDEC), IDAYS1(365), IDAYS2(366)
      INTEGER IDATMP(IMNDEC), IYTMP(IMNDEC), IDETMP(IMNDEC)
      REAL OUTPP(IMNDEC,4), OUTWP(IMNDEC,3)
      SAVE

*     start day of decades, normal and leap year
      DATA ID1 /10,20,31,41,51,59,69,79,90,100,110,120,130,140,
     &          151,161,171,181,191,201,212,222,232,243,253,263,
     &          273,283,293,304,314,324,334,344,354,365/
      DATA ID2 /10,20,31,41,51,60,70,80,91,101,111,121,131,141,
     &          152,162,172,182,192,202,213,223,233,244,254,264,
     &          274,284,294,305,315,325,335,345,355,366/

      DATA INIT /.FALSE./, ISHIFT /3/
*!!!      DATA    IEDEC /24/

      IF (ITASK.EQ.1) THEN
*        initialization

         IF (.NOT.INIT) THEN
*           open output files
            TMPSTR = OUTDIR//FILGIS
            CALL LOWERC (TMPSTR)
            ILGIS = ILEN(TMPSTR)
            CALL FOPENG (IUNIT,TMPSTR(1:ILGIS),'NEW','US',0,'DEL')  

            CALL EXTENS (TMPSTR,'FMT',0,FILFMT)
            CALL LOWERC (FILFMT)
            CALL FOPENG (IUNIT+1,FILFMT,'NEW','FS',0,'DEL')
*           set arrays to zero
            DO 10 I1=1,365
               IDAYS1(I1) = 0
               IDAYS2(I1) = 0
10          CONTINUE
            IDAYS2(366) = 0

*           initialize decade numbers
            DO 20 I1=1,IMNDEC
               IDAYS1(ID1(I1)) = I1
               IDAYS2(ID2(I1)) = I1
20          CONTINUE

*           make unit number local to this routine
            ILU  = IUNIT
            INIT = .TRUE.
         END IF

*        initialize only when potential production
         IF (IWB.EQ.0) THEN
            LRUNNM = RUNNAM
            IPP    = 0
            IPW    = 0
         END IF

      ELSE IF (ITASK.EQ.2) THEN
*        store data in arrays

*        test if year is leap year
         IF (MOD (IYEAR,4).NE.0) THEN
            IDEC = IDAYS1(IDAY)
         ELSE
            IDEC = IDAYS2(IDAY)
         END IF

         IF (IDEC.NE.0) THEN
*           day is decade boundary

            IF (IWB.EQ.0) THEN

*              store data for potential production

               IPP = IPP+1
               IF (IPP.GT.IMNDEC) CALL ERROR
     &            ('PRIJRC','local array too small')
               OUTPP(IPP,1) = WSO
               OUTPP(IPP,2) = TAGP
               OUTPP(IPP,3) = LAI
               OUTPP(IPP,4) = DVS
               IDATMP(IPP)  = IDAY
               IYTMP(IPP)   = IYEAR
*!!               IF (IYEAR.NE.IYTMP(1)) CALL ERROR
*!!     &            ('PRIJRC','year boundary passed')
               IDETMP(IPP)  = IDEC

            ELSE

*              store data for water limited production for all decades 
*              of the simulated growth season

               IPW = IPW+1
               IF (IPW.GT.IMNDEC) CALL ERROR
     &            ('PRIJRC','local array too small')
               IF (IDATMP(IPW).NE.IDAY.OR.IYTMP(IPW).NE.IYEAR)
     &            CALL ERROR ('PRIJRC','day and year not corresponding')
              
               OUTWP(IPW,1) = WSO
               OUTWP(IPW,2) = TAGP
               OUTWP(IPW,3) = LAI

            END IF
         END IF

      ELSE IF (ITASK.EQ.4) THEN

*!!         IF (IYEAR.NE.IYTMP(1)) CALL ERROR
*!!     &      ('PRIJRC','year boundary passed')

*        test if year is leap year
         IF (MOD (IYEAR,4).NE.0) THEN
            LEAP = .FALSE.
         ELSE
            LEAP = .TRUE.
         END IF

         IF (IWB.EQ.0) THEN

*           fill potential production arrays to end of year with final values
*           (for preparation of output only!)
            I1 = IPP+1
            I2 = IDETMP(IPP)+1
30          IF (I1.LE.IMNDEC.AND.I2.LE.36) THEN
               OUTPP(I1,1) = WSO
               OUTPP(I1,2) = TAGP
               OUTPP(I1,3) = 0.
               OUTPP(I1,4) = DVS
               IYTMP(I1)  = IYEAR
               IDETMP(I1) = I2
               IF (.NOT.LEAP) THEN
                  IDATMP(I1) = ID1(I2)
               ELSE
                  IDATMP(I1) = ID2(I2)
               END IF
               I1 = I1+1
               I2 = I2+1
            GOTO 30
            END IF

         ELSE IF (IWB.EQ.1) THEN

*           fill water-limited production arrays to end of year 
*           with final values

            I1 = IPW+1
            I2 = IDETMP(IPW)+1
40          IF (I1.LE.IMNDEC.AND.I2.LE.36) THEN
               OUTWP(I1,1) = WSO
               OUTWP(I1,2) = TAGP
               OUTWP(I1,3) = 0.
               I1 = I1+1
               I2 = I2+1
            GOTO 40
            END IF

*           write values to file

*           determine starting array element from where writing should start
*           (limit at one)
            IP1 = MIN (IPP,IPW)-ISHIFT
            IP1 = MAX (IP1,1)

*!!           determine finish array element from where writing should end
*!!            IF (IDETMP(IPP).GT.IEDEC.OR.IDETMP(IPW).GT.IEDEC)
*!!     &         WRITE (*,'(A)')
*!!     &         ' WARNING from PRIJRC: growing season ends after IEDEC'

*           write to file until end decade
            I1 = IP1
*           IPP and IWP are last decade before final day (ITASK=4). 
*           so final values are at the end of the next decade 
            I2 = MAX (IPP,IPW) + 1
*!!50          IF (I1.LE.IMNDEC.AND.IDETMP(I1).LE.IEDEC) THEN
50          IF (I1.LE.I2) THEN

*             output to binary file for selected decades
              WRITE (ILU)
     &           LRUNNM,IYTMP(I1),IDETMP(I1),
     &           OUTPP(I1,1),OUTWP(I1,1),OUTPP(I1,2),OUTWP(I1,2),
     &           OUTPP(I1,3),OUTWP(I1,3),OUTPP(I1,4)

*             output to formatted file only for final decade
              IF (I1.EQ.I2) THEN
                 WRITE (ILU+1,'(1X,A,I5,I3,4F8.0,4F6.2)')
     &           LRUNNM,IYTMP(I1),IDETMP(I1),
     &           OUTPP(I1,1),OUTWP(I1,1),OUTPP(I1,2),OUTWP(I1,2),
     &           OUTPP(I1,3),OUTWP(I1,3),OUTPP(I1,4),LAMXWL
               ENDIF

            I1 = I1+1
            GOTO 50
            END IF
         ELSE
            CALL ERROR ('PRIJRC','production level not known')
         END IF
      END IF

C!! Old WRITE statement (with idatmp and on formatted file):
C!!               WRITE (ILU,'(A,I5,2I4,4F7.0,3F5.2)')
C!!     &           LRUNNM,IYTMP(I1),IDETMP(I1),IDATMP(I1),
C!!     &           OUTPP(I1,1),OUTWP(I1,1),OUTPP(I1,2),OUTWP(I1,2),
C!!     &           OUTPP(I1,3),OUTWP(I1,3),OUTPP(I1,4)

      RETURN
      END
