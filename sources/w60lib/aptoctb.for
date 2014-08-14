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
	SUBROUTINE APTOCTB(NCTB,ICTB,IICTB,PARX,DTSMTB,KDIFTB,EFFTB,
     &                   AMAXTB,TMPFTB,TMNFTB,RDRRTB,RDRSTB,SLATB,
     &                   SSATB,RFSETB,FRTB,FLTB,FSTB,FOTB)
*
*  Subroutine: Assignment Parameters TO Crop TaBles
*  Content: This subroutine assigns a parameter (PARx) to one of the
*           indexes of a table crop parameter. The parameter PARx is
*           used in FSEOPT. The reason for this rather work around
*           solution is that FSEOPT can not handle table (arrays) in
*           the optimalization.
*
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)
*  name   type meaning                                    units  class
*  ----   ---- -------                                    -----  -----
*  NCTB    C   Name of Crop TaBle to be optimized            -      I
*  ICTB    I4  Index of Crop TaBle to be optimized           -      I
*  IICTB   I4  Number of indexes to be optimized with same   -      I
*              parameter, started with ICTB
*  PARX    R   Parameter in FSEOPT that has to be assigned to
*              one or more indexes of a crop table                  I
*  DTSMTB  R   Daily increase in thermal time, function of temp.   I/O
*  KDIFTB  R   Extinction coefficient for d..., function of DVS    I/O
*  EFFTB   R   Initial light-use efficiency of.., function of temp.I/O
*  AMAXTB  R   Maximum leaf CO2 assimilation .., function of DVS   I/O
*  TMPFTB  R   Reduction factor of AMAX, function of temp.         I/O
*  TMNFTB  R   Reduction factor of gross assim.., function of temp.I/O
*  RDRRTB  R   Relative death rate of roots, function of DVS       I/O
*  RDRSTB  R   Relative death rate of stems, function of DVS       I/O
*  SLATB   R   Specific leaf area, function of DVS                 I/O
*  SSATB   R   Specific stem area, function of DVS                 I/O
*  RFSETB  R   Reduction factor for senescence, function of DVS    I/O
*  FRTB    R   Fraction of above dry ma. to roots, function of DVS I/O
*  FLTB    R   Fraction of above dry ma. to leaves, function of DVS I/O
*  FSTB    R   Fraction of above dry ma. to stems, function of DVS I/O
*  FOTB    R   Fraction of above dry ma. to storage, function of DVS I/O
* 
*     Author: H.L. Boogaard, June 1998
*
*     +--------------------------------------------------------------+
*     | Version:      1.1											   |
*     | Date:         02-June-1998								   |
*     | Author:       Hendrik Boogaard							   |
*     |               Land evaluation method					       |
*     | Reason:                                                      |
*     | Modification:     				                           |
*     +--------------------------------------------------------------+

*     declarations
      IMPLICIT NONE
*     - formal variables
      CHARACTER NCTB*6
	INTEGER   ICTB, IICTB
	REAL      PARX
      REAL      DTSMTB(30),KDIFTB(30),EFFTB(30),AMAXTB(30),TMPFTB(30)
	REAL      TMNFTB(30),RDRRTB(30),RDRSTB(30),SLATB(30),SSATB(30)
      REAL      RFSETB(30),FRTB(30),FLTB(30),FSTB(30),FOTB(30)      

*     - local	
      INTEGER     I

*     start
	IF (ICTB.LE.0 .OR. ICTB.GT.30) THEN
         CALL ERROR ('APTOCTB',
     &               'FSEOPT: wrong index: ICTBx')			          
      ENDIF

      IF ((ICTB+(IICTB-1)*2).GT.30 .OR. IICTB.LT.1) THEN
         CALL ERROR ('APTOCTB',
     &               'FSEOPT: wrong number of indices: IICTBx')			          
	ENDIF

	IF (NCTB.EQ.'DTSMTB') THEN
         DO 10 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  DTSMTB(I) = PARX
10       CONTINUE
      ELSEIF (NCTB.EQ.'KDIFTB') THEN
         DO 11 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  KDIFTB(I) = PARX
11       CONTINUE
      ELSEIF (NCTB.EQ.'EFFTB') THEN
         DO 12 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  EFFTB(I) = PARX
12       CONTINUE
      ELSEIF (NCTB.EQ.'AMAXTB') THEN
         DO 13 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  AMAXTB(I) = PARX
13       CONTINUE
      ELSEIF (NCTB.EQ.'TMPFTB') THEN
         DO 14 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  TMPFTB(I) = PARX
14       CONTINUE
      ELSEIF (NCTB.EQ.'TMNFTB') THEN
         DO 15 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  TMNFTB(I) = PARX
15       CONTINUE
      ELSEIF (NCTB.EQ.'RDRRTB') THEN
         DO 16 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  RDRRTB(I) = PARX
16       CONTINUE
      ELSEIF (NCTB.EQ.'RDRSTB') THEN
         DO 17 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  RDRSTB(I) = PARX
17       CONTINUE
      ELSEIF (NCTB.EQ.'SLATB') THEN
         DO 18 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  SLATB(I) = PARX
18       CONTINUE
      ELSEIF (NCTB.EQ.'SSATB') THEN
         DO 19 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  SSATB(I) = PARX
19       CONTINUE
      ELSEIF (NCTB.EQ.'RFSETB') THEN
         DO 20 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  RFSETB(I) = PARX
20       CONTINUE
      ELSEIF (NCTB.EQ.'FRTB') THEN
         DO 21 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  FRTB(I) = PARX
21       CONTINUE
      ELSEIF (NCTB.EQ.'FLTB') THEN
         DO 22 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  FLTB(I) = PARX
22       CONTINUE
      ELSEIF (NCTB.EQ.'FSTB') THEN
         DO 23 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  FSTB(I) = PARX
23       CONTINUE
      ELSEIF (NCTB.EQ.'FOTB') THEN
         DO 24 I=ICTB,ICTB+(IICTB-1)*2,+2
      	  FOTB(I) = PARX
24       CONTINUE
      ELSE
         CALL ERROR ('APTOCTB',
     &               'FSEOPT: wrong name for crop table: NCTBx')			  
      ENDIF    
     	
		 
      RETURN
	END
