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
      SUBROUTINE PRHEAD 
     &    (MODE, RUNNAM, 
     &     OUTFIL, FILRER, VRSION, CLMNAM, CLFILE, CRPNAM, CRFILE, 
     &     RAFILE, RAINAM, SOLNAM, SOFILE,
     &     IDETAI, ISET,   ISTSET, INSETS, INYR,   INYEAR, INR, INYRG,
     &     IUOUT,  IUOF,   IUOSUM, IUSTPP, IUSTWP, 
     &     IWB,    IOX,    IASYR,  IASYRR, IRNDAT, 
     &     ISTCHO, IDEM,   IDESOW, IDLSOW)

*     Write headers to output file 
*     Authors : Daniel van Kraalingen & Kees van Diepen,
*     Date    : February 1993
*     +---------------------------------------------------------------+
*     | Version:      1.2                     						|
*     | Date:         25-July-1997									|
*     | Author:       Tamme van der Wal								|
*     | Reason:       Reduction of possible MODEs to two				|
*     | Modification: PRHEAD now reacts on MODE: "RERUN"				|
*     +---------------------------------------------------------------+
*     | MODIFICATION                    								|
*     | Date:         02-06-1998										|
*     | Author:       Hendrik Boogaard								|
*     | Reason:       Removal of IUSCR								|
*     +---------------------------------------------------------------+

*     formal parameters
      CHARACTER*(*) MODE  , RUNNAM
      CHARACTER*(*) OUTFIL, FILRER, VRSION
      CHARACTER*(*) CLMNAM, CLFILE, CRPNAM, CRFILE
      CHARACTER*(*) RAFILE, RAINAM, SOLNAM, SOFILE
      INTEGER IDETAI, ISET, INSETS, IWB, IOX, IUOUT, IUOF
      INTEGER INR, INYR,  INYRG,  IASYR, IASYRR, IRNDAT
      INTEGER ISTCHO, IDEM, IDESOW, IDLSOW

**    local parameters

      CHARACTER*40 PSTEXT
      INTEGER ILEN, IL1, IL2

      IF (IWB.EQ.0) PSTEXT = ' POTENTIAL ' 
      IF (IWB.EQ.1 .AND. IOX.EQ.0) 
     &              PSTEXT = ' WATER-LIMITED ' 
      IF (IWB.EQ.1 .AND. IOX.EQ.1) 
     &              PSTEXT = ' WATER- AND OXYGEN LIMITED ' 

      IF (IDETAI.EQ.1) IUWRIT = IUOUT
      IF (IDETAI.EQ.2 .AND. IWB.EQ.0) IUWRIT = IUSTPP 
      IF (IDETAI.EQ.2 .AND. IWB.EQ.1) IUWRIT = IUSTWP 
      IF (IDETAI.EQ.3) IUWRIT = IUOSUM

      IF (IDETAI.EQ.1 .AND. IWB.EQ.0) THEN
         IF (IUOUT.EQ.IUOF .AND. INYR.GT.1) 
     &                               WRITE (IUWRIT,'(A)') '1'
      ENDIF

      IL1 = ILEN (VRSION)
      WRITE (IUWRIT,'(//,1X,2A,/)') VRSION(1:IL1)
      IF (IDETAI.EQ.1) WRITE (IUWRIT,'(1X,2A)') 'RUNNAM -> ',RUNNAM


      IL1 = ILEN (OUTFIL)
      WRITE (IUWRIT,'(2A)') ' OUTPUT -> file: ',OUTFIL(1:IL1)

      IF (IDETAI.EQ.1) THEN
         IF (MODE.EQ.'RERUN') THEN
            IL1 = ILEN (FILRER)
            WRITE (IUWRIT,'(2A,/,2(A,I3),A)')
     &         ' RERUNS -> file: ',FILRER(1:IL1),
     &         '                 with rerun set nr.',ISET,' out of ',
     &         INSETS,' sets'
         ELSE
            WRITE (IUWRIT,'(A)') ' RERUNS -> no reruns'
         END IF
      END IF


      IF (IDETAI.EQ.2) THEN
         IL1 = ILEN (PSTEXT) 
         WRITE (IUWRIT,'(2(A,/),4A)')
     &         '**','**','** RESULTS OF CROP GROWTH',
     &    ' SIMULATION MODEL WOFOST: SUMMARY',  
     &    PSTEXT(1:IL1),' PRODUCTION'

*!!!          IL1 = ILEN (WOSTAT)
*!!!         WRITE (IUSTPP,'(2A,/,2A)')
*!!!     &   ' WOFOST output file: ',WOSTAT(1:IL1),' Run name: ',RUNNAM

      ENDIF
       
      IL1 = ILEN (CLMNAM)
      IL2 = ILEN (CLFILE)
      WRITE (IUWRIT,'(/,2A,/,2A,A,I4)')
     &        ' WEATHER-> name: ',CLMNAM(1:IL1),
     &        '           file: ',CLFILE(1:IL2),'   start year: ',IASYR

      IF (IRNDAT.EQ.0) THEN
         WRITE (IUWRIT,'(2A,I3,A,I3)') 
     &                  ' RAIN   -> randomized by generator',
     &                  ': rain series ',
     &                 INR,' of total ',INYRG 
      ELSE IF (IRNDAT.EQ.1) THEN
         WRITE (IUWRIT,'(2A,I3,A,I3)') 
     &                  ' RAIN   ->  distributed',
     &                  ': with rain series nr. ',
     &                   INR,' of total ',INYRG
      ELSE IF (IRNDAT.EQ.2) THEN
         IL1 = ILEN (RAINAM)
         IL2 = ILEN (RAFILE)
         WRITE (IUWRIT,'(2A,/,2A,/,A,I4)')
     &                ' RAIN   -> name: ',RAINAM(1:IL1),
     &                '           file: ',RAFILE(1:IL2),
     &                '           year: ',IASYRR
      ELSE IF (IRNDAT.EQ.3) THEN
         WRITE (IUWRIT,'(2A)')' RAIN   ->', 
     &               ' belonging to weather station'
      END IF
      IL1 = ILEN (CRPNAM)
      IL2 = ILEN (CRFILE)
      WRITE (IUWRIT,'(2A,/,2A)')
     &             ' CROP   -> name: ',CRPNAM(1:IL1),
     &             '           file: ',CRFILE(1:IL2)

      IL1 = ILEN (SOLNAM)
      IL2 = ILEN (SOFILE)
      WRITE (IUWRIT,'(2A,/,2A)')
     &             ' SOIL   -> name: ',SOLNAM(1:IL1),
     &             '           file: ',SOFILE(1:IL2)

      IF (IDETAI.EQ.1) THEN
         IF (ISTCHO.EQ.0) THEN
            WRITE (IUWRIT,'(A,I3,/)')
     &               ' START  -> fixed emergence date IDEM =',IDEM 
         ELSE IF (ISTCHO.EQ.1) THEN
            WRITE (IUWRIT,'(A)') ' START  -> fixed sowing date '
         ELSE IF (ISTCHO.EQ.2) THEN
            WRITE (IUWRIT,'(A,I3,A,I3,A,I3)')
     &                ' START  -> sowing date variable between day nr',
     &                IDESOW,' and ', IDLSOW,
     &                ';   start calculations =', IDESOW-10
         END IF

      ENDIF

      END 
