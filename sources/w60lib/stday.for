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
      SUBROUTINE STDAY
     &      (ITASK, CRPNAM, SOFILE, IUSO  , IUOUT , IULOG, RAIN, ES0,
     &       IDAY , IDESOW, IDLSOW, ISTATE, COSUT)

*  N.B. special feature added for WRR : COSUT.
*      Called by subroutine WOFOST
*      ITASK = 1, initialization
*      ITASK = 2, calculations
*      ITASK = 3, no action
*      ITASK = 4, no action
*  Subroutine STDAY determines sowing date
*  For ISTCHO = 2  a suitable sowing date is calculated, and the
*  corresponding date of emergence.
*  First the workability of the soil over the period before sowing is
*  checked on the basis of soil and crop dependent criteria.
*  The workability model imitates the results of the hydraulic model of
*  WIND (1972), published in ICW Nota 867 (VAN WIJK AND FEDDES,1975).
*  Soil EC-3 in the present model corresponds with Wind's loam with a drain
*  depth of 150 cm.
*-----------------------------------------------------------------------
*  CAPRMX     maximum upward flow into plow layer
*  CAPRFU(10) upward flow as a function of negative values of WEXC, when
*              topsoil is drier than field capacity
*  COSUT      counts times that sowing date equals latest sowing date
*              (indicator for the suitability of a soil for a specific crop).
*  DEFLIM     minimum required soil moisture deficit in plow layer
*              for occurrence of workable day (workability criterion)
*  EVS        daily evaporation from bare soil surface
*  IDAY       julian date
*  IDFWOR     first workable day
*  IDESOW     earliest sowing date
*  IDLSOW     latest sowing date
*  IDSOW      sowing date
*  ILWPER     length of workable period, should be 3 for sowing
*             (sowing criterion)
*  RAIN       daily rainfall
*  SEEP       daily seepage from plow layer
*  SMDEF      estimated soil moisture deficit in plow layer
*  SPADS   SPAC  topsoil seepage parameter for deep seedbed (potato)
*  SPASS   SPAC  topsoil seepage parameter for shallow seedbed
*  SPODS   SPOC  topsoil seepage parameter for deep seedbed (potato)
*  SPOSS   SPOC  topsoil seepage parameter for shallow seedbed
*  WEXC       excess amount of water in plow layer

*  Author : Kees van Diepen, April 1989, revised August 1990

      IMPLICIT REAL(A-Z)

*     formal parameters
      INTEGER ITASK, IUSO, IUOUT, IDAY, IDESOW, IDLSOW, ISTATE, IULOG
      CHARACTER*(*) CRPNAM, SOFILE

**    local parameters
      INTEGER IDFWOR, ILWPER, ITOLD
      REAL CAPRFU(10)
      SAVE

      DATA CAPRFU/-0.50,0.50, 0.00,0.20, 0.10,0.15, 0.40,0.10,
     &            1.00,0.05/
      DATA ITOLD /4/

*     immediate return of execution when previous task was initialization
      IF (ITASK.EQ.3.AND.ITOLD.EQ.1) THEN
         ITOLD = ITASK
         RETURN
      END IF

*-----------------------------------------------------------------------
*     initial section
*-----------------------------------------------------------------------

      IF (ITASK.EQ.1) THEN

*        initialization of the workability model
*        initialize soil data reading

         CALL RDINIT (IUSO, IULOG, SOFILE)
*        reading of soil data (single reals and arrays)
         CALL RDSREA ('SPADS' , SPADS )
         CALL RDSREA ('SPODS' , SPODS )
         CALL RDSREA ('SPASS' , SPASS )
         CALL RDSREA ('SPOSS' , SPOSS )
         CALL RDSREA ('DEFLIM', DEFLIM)
*        end of soil input section

         IF (CRPNAM(2:3).EQ.'BI') THEN
*           two seepage parameters for deep seedbed (potato)
            SPAC = SPADS
            SPOC = SPODS
         ELSE
*           two seepage parameters for shallow seedbed (all other crops)
            SPAC = SPASS
            SPOC = SPOSS
            DEFLIM = 0.
         END IF

         WEXC   = 2.
*RAIN=0?
*         RAIN  = 0.
         EVS    = 0.
         CAPRMX = 0.
         SEEP   = 0.
         IDFWOR = -99
         ILWPER = 0

*     end of initial section
      ELSE IF (ITASK.EQ.2) THEN
         CONTINUE
      ELSE IF (ITASK.EQ.3) THEN

*----------------------
*        before sowing
*----------------------
*        workability is assessed until sowing date is found
         IF (ISTATE.EQ.0) THEN
*           evaporation from soil surface
            IF (WEXC.GE.0.5) THEN
               CAPRMX = 0.
               EVS    = ES0
            ELSE
*              maximum capillary rise to surface (cf. Ritchie)
               CAPRMX = AFGEN (CAPRFU, 10, -WEXC)
               EVS    = MIN (ES0, CAPRMX+RAIN)
            END IF

            WEXC = MAX (-1., WEXC+RAIN-EVS)

*           seepage
            IF (WEXC.GT.0.) THEN
               SEEP = MIN (WEXC*SPAC+SPOC,WEXC)
               WEXC = WEXC-SEEP
            END IF

*           criterion for workable day
            IF (WEXC.LE.DEFLIM) THEN
               ILWPER = ILWPER+1
            ELSE
               ILWPER = 0
            END IF

*           first workable day? (for output only)
            IF (IDFWOR.EQ.-99 .AND. ILWPER.GE.1) IDFWOR = IDAY

*           criterion for sowing
            IF ((IDAY.GE.IDESOW .AND. ILWPER.GE.3)
     &               .OR. IDAY.EQ.IDLSOW) THEN
               ISTATE = 1
*              indicator for sowing at latest sowing date
               IF (IDAY.EQ.IDLSOW)THEN
                  COSUT = 1.
               ELSE
                  COSUT = 0.
               END IF
            END IF
         ELSE
            CALL ERROR ('STDAY','sowing has already taken place')
         END IF
      ELSE IF (ITASK.EQ.4) THEN
         CONTINUE
      END IF

      ITOLD = ITASK

      RETURN
      END
