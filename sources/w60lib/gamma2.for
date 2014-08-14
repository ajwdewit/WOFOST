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
      REAL FUNCTION GAMMA2 (ALFA,BETA,ISET)

*     Chapter 17 in documentation WOFOST Version 4.1 (1988)

*     This function calculates gamma-distributed numbers for the
*     specified gamma distribution parameters BETA and ALFA.
*     The algorithm used is derived from that of Berman (1971).

*     Subroutines and functions called: RANDOM.
*     Called by routine RNGEN and RNDIS.

*     Author: C. Rappoldt, modified by D. van Kraalingen,

**
      SAVE

      AA = 1./ALFA
      AB = 1./(1.-ALFA)
      TR1 = EXP (-18.42/AA)
      TR2 = EXP (-18.42/AB)
10    RN1 = RANDOM (2,ISET)
      RN2 = RANDOM (2,ISET)
      IF ((RN1-TR1) .GT. 0.) GOTO 20
      S1 = 0.
      GOTO 30
20    CONTINUE
      S1 = RN1**AA
30    CONTINUE
      IF ((RN2-TR2) .GT. 0.) GOTO 40
      S2 = 0.
      GOTO 50
40    CONTINUE
      S2 = RN2**AB
50    CONTINUE
      S12 = S1 + S2
      IF (S12-1.) 60,60,10
60    Z = S1/S12
      RN3 = RANDOM (2,ISET)
      GAMMA2 = -Z*ALOG (RN3)*BETA
      RETURN
      END
