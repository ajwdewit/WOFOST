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
      SUBROUTINE PRIWPP (ITASK, IYEAR, IDAY, IDSEM,
     &                   IUOUT, RD)

*     print part of WOFOST.OUT (standard output table for single
*     run) on potential production

*     Author: C.A. van Diepen, September 1988, revised June 1991

*     declarations
      IMPLICIT REAL(A-Z)
      INTEGER IDSEM,IDANTX,IYEAR, IDAY,IDHALT, IDOSJ, IDOST
      INTEGER IDWSJ,IDWST,IUOUT,ITASK
*     daily output of crop variables from CROPSI to WOFOST.OUT
      COMMON /CROPDO/WLV,WST,WSO,LAI,DVS,TSUM,TRA,
     &              GASS,MRES,DMI,TAGP,
     &             IDOST, IDWST, IDOSJ, IDWSJ
*     final output of crop variables from CROPSI to WOFOST.OUT
      COMMON /CROPFO/IDANTX,IDHALT,TWRT,TWLV,TWST,TWSO,TAGPX,
     &               GASST,MREST,HINDEX,TRC,TRAT

*     TAGP is a copy of TAGP calculated for each timestep in CROPSI.
*     TAGPX is a copy of the final TAGP calculated in CROPSI.
*     IDANTX is a copy of IDANTH
**
      SAVE

      IF (ITASK.EQ.1) THEN

*-----------------------------------------------------------------------
*     print heading of output table
*-----------------------------------------------------------------------

         WRITE (IUOUT, '(1X,A,/,1X,A,/,/,1X,2A)')
     &     'POTENTIAL CROP PRODUCTION',
     &     '=========================',
     &     'YEAR  DAY IDSEM DVS  TSUM   WLV    WST    WSO    TAGP   ',
     &     'LAI  TRA   GASS   MRES    DMI'

         WRITE (IUOUT,'(/,1X,2A)')
     &     '                    degrd kg/ha  kg/ha  kg/ha   kg/ha ',
     &     'm2/m2   mm/d CH2O CH2O  kg/ha/d'


      ELSE IF (ITASK.EQ.2) THEN

*-----------------------------------------------------------------------
*     print daily crop and water variables
*-----------------------------------------------------------------------

*        print selected daily crop and soil water variables
         WRITE (IUOUT,'(I6,2I4,F6.2,F6.0,4F7.0,F6.2,F5.2,3F7.1)')
     &    IYEAR,IDAY,IDSEM,DVS,TSUM,WLV,WST,WSO,TAGP,LAI,10.*TRA,GASS,
     &    MRES,DMI

      ELSE IF (ITASK.EQ.3) THEN
*        no action
         CONTINUE
      ELSE IF (ITASK.EQ.4) THEN

*-----------------------------------------------------------------------
*     print final results
*-----------------------------------------------------------------------

*        print summary results on one line of output table
         WRITE (IUOUT,'(/,A,/,1X,2A)') ' SUMMARY:',
     &     'HALT  ANTH TWRT    TWLV    TWST    TWSO    ',
     &     'TAGP HINDEX TRANSP TRC  GASST  MREST'
         WRITE (IUOUT,'(2I4,5F8.0,F7.2,F6.1,F5.0,2F7.0)')
     &     IDHALT, IDANTX, TWRT, TWLV, TWST, TWSO, TAGPX, HINDEX, TRAT,
     &     TRC, GASST, MREST
*!!naar main:         WRITE (IUOUT,'(A)') '1'

      END IF
      RETURN
      END
