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
      REAL FUNCTION RANDOM (ITASK, ISET)

*     Chapter 19 in documentation WOFOST Version 4.1 (1988)

*     This function generates pseudo-random numbers uniformly
*     distributed between 0 and 1.

*     Author: C. Rappoldt, revised by D. van Kraalingen, April 1991


*     Revision : auto-reset statement removed (IL=1),
*                different sets of independent random numbers can be
*                generated through the pointer ISET,
*                reset or a particular set is through ITASK=1,
*                random number for a set is obtained through ITASK=2, 
*                (ITASK=1, should be done first before any ITASK=2)
*     The algorithm used is developed by Wichman and Hill (1982).

*     formal parameters
      INTEGER ITASK, ISET

**    local parameters
      PARAMETER (INSET=10, IXS=2316, IYS=4836, IZS=10248)
      INTEGER IX(INSET), IY(INSET), IZ(INSET)
      LOGICAL INITS(INSET), INIT
      SAVE

      DATA INIT /.FALSE./

*     set all sets to uninitialized
      IF (.NOT.INIT) THEN
         DO 10 I1=1,INSET
            INITS(I1) = .FALSE.
10       CONTINUE
         INIT = .TRUE.
      END IF

*     check if set pointer is within bounds
      IF (ISET.LT.1.OR.ISET.GT.INSET) CALL ERROR
     &   ('RANDOM', 'illegal random set')

      IF (ITASK.EQ.1) THEN
*        initialize a set
         IX(ISET) = IXS
         IY(ISET) = IYS
         IZ(ISET) = IZS
         INITS(ISET) = .TRUE.
         RANDOM = 0.
      ELSE IF (ITASK.EQ.2) THEN
*        get random number
         IF (.NOT.INITS(ISET)) THEN
            CALL ERROR ('RANDOM','set not initialized')
         ELSE
            IX(ISET) = 171*MOD(IX(ISET),177)- 2*(IX(ISET)/177)
            IY(ISET) = 172*MOD(IY(ISET),176)-35*(IY(ISET)/176)
            IZ(ISET) = 170*MOD(IZ(ISET),178)-63*(IZ(ISET)/178)
            IF(IX(ISET).LT.0) IX(ISET) = IX(ISET)+30269
            IF(IY(ISET).LT.0) IY(ISET) = IY(ISET)+30307
            IF(IZ(ISET).LT.0) IZ(ISET) = IZ(ISET)+30323
            RANDOM = MOD (REAL(IX(ISET))/30269.+REAL(IY(ISET))/30307.+
     &               REAL(IZ(ISET))/30323.,1.)
         END IF
      ELSE
         CALL ERROR ('RANDOM','wrong ITASK')
      END IF

      RETURN
      END
