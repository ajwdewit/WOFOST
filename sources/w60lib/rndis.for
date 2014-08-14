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
      SUBROUTINE RNDIS (RSETRD,IYEAR,RAINTB,RAIND,RSERIE)

*     This routine calculates the daily rainfall (RSERIE) by
*     distributing the given monthly rainfall (RAINTB) over a
*     defined number of randomly chosen rainy days.
*     Monthly rain and number of rainy days equal the given values.
*     Revision includes a minor modification in the random number
*     call and modification to distribute rain in leap years.

*     Subroutines and functions called: GAMMA2, RANDOM

*  FATAL ERROR CHECKS (execution terminated, message):
*  number of rainy days too large

*     Author: C.A. van Diepen, july 1989, revised April 1991

*     +-----------------------------------------------------------------+ 
*     | Version:      1.1                                               |
*     | Date:         2 OCTOBER 1997                                    |
*     | Author:       Tamme van der Wal                                 |
*     |               Group Software Engineering                        |
*     | Reason:       RAIND() in datafiles can be < 1                   |
*     | Modification: IRND = MAX (RAIND, 1)                             |
*     +-----------------------------------------------------------------+ 

*     declarations
      IMPLICIT REAL(A-Z)
      INTEGER IDAY, IDAYNR, IMON, IN, IND, INDMON, INRD, IR
      INTEGER ILDAYS(13), ILDAY1(13), ILDAY2(13), IYEAR
      REAL RAINTB(12), RAIND(12), RSERIE(366), GAMDIS(31)
      LOGICAL RSETRD, LEAP
**
      SAVE

*     number of last day previous month for normal years
      DATA ILDAY1 /  0,  31,  59,  90, 120, 151, 181,
     &             212, 243, 273, 304, 334, 365/

*     number of last day previous month for leap years
      DATA ILDAY2 /  0,  31,  60,  91, 121, 152, 182,
     &             213, 244, 274, 305, 335, 366/

*     load array ILDAY depending on whether IYEAR is leap year or normal
*     year
      LEAP = MOD (IYEAR,4).EQ.0
      DO 5 IMON=1,13
         IF (LEAP) THEN
            ILDAYS(IMON) = ILDAY2(IMON)
         ELSE
            ILDAYS(IMON) = ILDAY1(IMON)
         END IF
5     CONTINUE

*     reset FUNCTION RANDOM if required
      IF (RSETRD) THEN
         XRAND  = RANDOM(1,1)
         XRAND  = RANDOM(2,1)
         RSETRD = .FALSE.
      END IF

*-----------------------------------
* 9.2 calculation of daily rainfall
*-----------------------------------
      DO 10 IDAY=1,366
         RSERIE(IDAY) = 0.
10    CONTINUE

      DO 100 IMON=1,12
*!!!         INRD = INT(RAIND(IMON))
         INRD = MAX(INT(RAIND(IMON)),1)
         GAMSUM = 0.
         DO 20 IND = 1,31
            GAMDIS(IND) = 0.
20       CONTINUE
         INDMON = ILDAYS(IMON+1) - ILDAYS(IMON)
*        the number of rainy days not to exceed length of month
         IF (INRD.GT.INDMON)
     &     CALL ERROR ('RNDIS','number of rainy days too large')
         MEANRN = RAINTB(IMON) / MAX (INRD,1)
         IF (MEANRN .LT. 1.20) THEN
*           in case of a low mean daily rainfall
            DO 25 IR = 1,INRD
               GAMDIS(IR) = MEANRN
25          CONTINUE
            CORFAC = 1.
         ELSE
*           gamma distribution parameters for current month
            BETA = -2.16 + 1.83 * MEANRN
            ALFA = MIN  (MEANRN/BETA,0.999)
            DO 30 IR = 1,INRD
               GAMDIS(IR) = GAMMA2(ALFA,BETA,1)
               GAMSUM = GAMSUM + GAMDIS(IR)
30          CONTINUE
*           correction factor to keep amount of monthly rainfall unchanged
            CORFAC = RAINTB(IMON)/GAMSUM
         END IF

         DO 40 IN = 1,INRD
            GAMDIS(IN) = CORFAC*GAMDIS(IN)
44          XRAND  = RANDOM(2,1)
            XDAYNR = XRAND*INDMON + 1.
            IDAYNR = INT(XDAYNR) + ILDAYS(IMON)
            IF (RSERIE(IDAYNR) .GT. 0.) GOTO 44
            RSERIE(IDAYNR) = GAMDIS(IN)
40       CONTINUE
100   CONTINUE

      RETURN
      END
