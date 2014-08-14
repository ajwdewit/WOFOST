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
      SUBROUTINE SUBSOL (PF,D,FLOW,CONTAB,ILCON)

*     Chapter 15 in documentation WOFOST Version 4.1 (1988)

*     This routine calculates the rate of capillary flow or
*     percolation between groundwater table and root zone. The
*     stationary flow is found by integration of
*            dZ = K.d(MH)/(K + FLW)   ,where
*     Z= height above groundwater, MH= matric head, K= conductivity and
*     FLW= chosen flow. In an iteration loop the correct flow is found.
*     The integration goes at most over four intervals : [0,45],[45,170],
*     [170,330] and [330, MH-rootzone] (last one on logarithmic scale).

*     Subroutines and functions called: AFGEN.
*     Called by routine WATGW.

*     Author: C. Rappoldt, January 1986, revised June 1990

*15.1 declarations and constants
      IMPLICIT REAL(A-Z)
      INTEGER I1, I2, I3, IINT, ILCON, IMAX
      REAL CONTAB(ILCON)
      REAL START(4),PFSTAN(9),PGAU(3),WGAU(3)
      REAL DEL(4),PFGAU(12),HULP(12),CONDUC(12)
**
      SAVE

      DATA ELOG10/2.302585/,PGAU/.1127016654,.5,.8872983346/
      DATA WGAU/.2777778,.4444444,.2777778/
      DATA START/0.,45.,170.,330./,LOGST4/2.518514/
      DATA PFSTAN/.705143,1.352183,1.601282,1.771497,2.031409,
     &           2.192880,2.274233,2.397940,2.494110/

*15.2 calculation of matric head and check on small pF
      PF1 = PF
      D1  = D
      MH  = EXP (ELOG10*PF1)
      IF (PF1.LE.0.) GOTO 90
      IINT = 0

*15.3 number and width of integration intervals
      DO 10 I1=1,4
         IF(I1.LE.3) DEL(I1) = MIN (START(I1+1),MH)-START(I1)
         IF(I1.EQ.4) DEL(I1) = PF1-LOGST4
         IF(DEL(I1).LE.0.) GOTO 20
         IINT = IINT+1
10    CONTINUE

*15.4 preparation of three-point Gaussian integration
20    DO 50 I1=1,IINT
         DO 50 I2=1,3
         I3 = 3*(I1-1)+I2
         IF (I1.EQ.IINT) GOTO 30
*        the three points in the full-width intervals are standard
         PFGAU(I3) = PFSTAN(I3)
         GOTO 40
30       CONTINUE
*        the three points in the last interval are calculated
         IF(IINT.LE.3) PFGAU(I3) =
     &              LOG10 (START(IINT)+PGAU(I2)*DEL(IINT))
         IF(IINT.EQ.4) PFGAU(I3) = LOGST4+PGAU(I2)*DEL(IINT)
40       CONTINUE
*        variables needed in the loop below
         CONDUC(I3) = EXP (ELOG10*AFGEN (CONTAB,ILCON,PFGAU(I3)))
         HULP(I3)   = DEL(I1)*WGAU(I2)*CONDUC(I3)
         IF(I3.GT.9) HULP(I3) = HULP(I3)*ELOG10*EXP (ELOG10*PFGAU(I3))
50    CONTINUE

*15.5 setting upper and lower limit
      FU =  1.27
      FL = -1.*EXP (ELOG10*AFGEN (CONTAB, ILCON, PF1))
      IF (MH.LE.D1) FU = 0.
      IF (MH.GE.D1) FL = 0.
      IF (MH.EQ.D1) GOTO 80

*15.6 Iteration loop
      IMAX = 3*IINT
      DO 70 I1=1,15
         FLW = (FU+FL)/2.
         DF  = (FU-FL)/2.
         IF ((DF.LT.0.01).AND.((DF/ABS(FLW)).LT.0.1)) GOTO 80
         Z = 0.
         DO 60 I2=1,IMAX
            Z = Z+HULP(I2)/(CONDUC(I2)+FLW)
60       CONTINUE
         IF (Z.GE.D1) FL = FLW
         IF (Z.LE.D1) FU = FLW
70    CONTINUE

*15.7 output
80    FLOW = (FU+FL)/2.
      RETURN

*15.8 in case of small matric head
90    K0   = EXP ( ELOG10 * AFGEN (CONTAB,ILCON,-1.) )
      FLOW = K0*(MH/D-1.)

      RETURN
      END
