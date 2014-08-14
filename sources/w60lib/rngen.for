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
      SUBROUTINE RNGEN (RSETRG,RAINTB,RAIND,RSERIE)

*     Chapter 9 in documentation WOFOST Version 4.1 (1988)

*     This routine generates one year of daily rainfall data (RSERIE)
*     on the basis of the given long term average monthly rainfall
*     (RAINTB) and number of rainy days (RAIND) (wet is >0. or >.1 mm).
*     The method follows the proposal of Shu Geng et al. (1986).
*     The procedure is a combination of a Markov chain and a gamma
*     distribution function.
*     The Markov chain uses the transitional probabilities of a wet day
*     after a dry day. The natural clustering of rainy days can be
*     described by a Markov parameter value of 0.75.
*     In the absence of clustering Markov=1.00. Check the value of Markov!
*     The parameters alfa and beta of the gamma distribution function
*     are derived from the mean rainfall per wet day.
*     The resulting number of rainy days and monthly totals of the generated
*     daily rainfall data are different from the given mean monthly values.
*     Note : reset of RANDOM here in stead of auto-reset
*     Revision includes a minor modification in the random number
*     call.

*     Subroutines and functions called: GAMMA2, RANDOM.

*     Author: C. Rappoldt, October 1986, revised April 1991
**
*----------------------------------
* 9.1 declarations and preparation
*----------------------------------
      IMPLICIT REAL(A-Z)
      INTEGER ID1, ID2, IDAY, IMON, INDMON, IP, ILDAYS(13)
      LOGICAL RSETRG
      REAL RAINTB(12), RAIND(12), RSERIE(366)
      SAVE

*     last day of preceding month
      DATA ILDAYS /  0,  31,  59,  90, 120, 151, 181,
     &             212, 243, 273, 304, 334, 365/
      DATA MARKOV /0.75/, A /2.16/, B /1.83/

      MLIMIT = 0.999*A/(0.999*B-1.0)
      IF (RSETRG) THEN
         IP = 0
         RAND = RANDOM (1,2)
         RAND = RANDOM (2,2)
         RSETRG = .FALSE.
      END IF

*     set array with rainfall to zero
      DO 10 IDAY = 1,365
         RSERIE(IDAY) = 0.
10    CONTINUE

*------------------------------------------------------
* 9.2  calculation of daily rainfall over twelve months
*------------------------------------------------------

      DO 50 IMON=1,12
         ID1 = ILDAYS(IMON)+1
         ID2 = ILDAYS(IMON+1)
         INDMON = ID2 - ILDAYS(IMON)
*        amount of rain per wet day
         RNPWD = RAINTB(IMON) / MAX (RAIND(IMON),0.01)

*        calculation of alfa and beta
         IF (RNPWD.GE.MLIMIT) THEN
*           regression equation is valid
            BETA = B*RNPWD - A
            ALFA = RNPWD/BETA
         ELSE IF (RNPWD.GT.0.0) THEN
*           adaptation to small beta
            ALFA = 0.999
            BETA = RNPWD/ALFA
         ELSE
*           no rainfall
            IP = 0
            GOTO 50
         END IF

*        conditional rainfall probabilities
         PWD = MARKOV * RAIND(IMON) / INDMON
         PWW = (1.00-MARKOV) + PWD

*        generation
         DO 40 IDAY=ID1,ID2
            RAND = RANDOM(2,2)
            IF (IP.EQ.0 .AND. RAND.LE.PWD) GOTO 30
            IF (IP.EQ.1 .AND. RAND.LE.PWW) GOTO 30
*           no rain
            IP = 0
            GOTO 40
30          CONTINUE
*           in case of rainfall
            IP = 1
*           amount of rainfall
            RSERIE (IDAY) = GAMMA2(ALFA,BETA,2)
40       CONTINUE
50    CONTINUE
      RETURN
      END
