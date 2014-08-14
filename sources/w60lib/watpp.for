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
      SUBROUTINE WATPP
     &             (ITASK , DELT, SOFILE, IUSO, IUOUT, IULOG,
     &              IAIRDU, SM0 , SMFCF , SMW , EVWMX, EVSMX, TRA, SM)


*     In this routine the variables of the soil water balance in the
*     potential production situation are calculated. The purpose is to 
*     quantify the crop water requirements for continuous growth without 
*     drought stress. It is assumed that the soil is permanently at field 
*     capacity.


* VARIABLE TYPE Description                                      Units   I/O

*  ITASK   I*4  flag to control task to be performed                       I
*               1, initialization
*               2, rate calculation, save dynamic output
*               3, integration, summation
*               4, finish section, save final output
*  DELT    R*4  time step (= 1 day)                                d       I
*  SOFILE CH*(*) name of soil data file                                    I
*  IUSO    I*4  unit nr on which soil data file is opened                  I
*  IUOUT   I*4  unit nr on which output file is opened                     I
*  IULOG   I*4  unit nr on which log file is opened                        I
*  IAIRDU  I*4  indicates presence(1) or absence(0) of airducts            I
*               in the roots. 1= can tolerate waterlogging 
*  SM0     R*4  soil porosity                                    cm3 cm-3  O
*  SMFCF   R*4  soil moisture content at field capacity          cm3 cm-3  O
*  SMW     R*4  soil moisture content at wilting point           cm3 cm-3  O
*  EVWMX   R*4  maximum evaporation rate from shaded water surface cm d-1  I
*  EVSMX   R*4  maximum evaporation rate from shaded soil surface  cm d-1  I
*  TRA     R*4  actual transpiration rate                          cm d-1  I
*               Note: TRA is calculated in EVTRA called by CROPSI
*  SM      R*4  actual soil moisture content                     cm3 cm-3  O

*     Author: C.A. van Diepen, February 1989, revised July 1990

*     Subroutine WATPP is derived from subroutine APPLE of WOFOST Version 4.1.
*     WATPP is called by WOFSIM.

*     declarations
      IMPLICIT REAL(A-Z)
      INTEGER IAIRDU, IUSO, IUOUT, ILSM, ITASK, ITOLD, IULOG
      CHARACTER SOFILE*(*)
      REAL SMTAB(30)

*     summary output variables 
      COMMON/WATPPO/ TRATX,EVWTX,EVSTX
**
      SAVE

      DATA ITOLD /4/

*     immediate return of execution when previous task was initialization
      IF (ITASK.EQ.3.AND.ITOLD.EQ.1) THEN
         ITOLD = ITASK
         RETURN
      END IF

      IF (ITASK.EQ.1) THEN

*----------------------------------------------------------------------
*     initial section
*----------------------------------------------------------------------
*        read required minimum set of soil physical data
*        SMFCF,SMW, and SM0 only

*        initialize soil data reading
         CALL RDINIT (IUSO, IULOG, SOFILE)

*        reading of soil data (single reals and arrays)
         CALL RDSREA ('SMFCF', SMFCF)
         CALL RDSREA ('SMW', SMW)
         CALL RDSREA ('SM0', SM0)
*!!      CALL RDAREA ('SMTAB',SMTAB,30,ILSM)
*        end of soil input section

*        soil moisture retention data
*        field capacity, wilting point and porosity
*!!!*         SMFCF = AFGEN (SMTAB, ILSM, LOG10(200.))
*!!!*         SMW   = AFGEN (SMTAB, ILSM, LOG10(16000.))
*!!!*         SM0   = AFGEN (SMTAB, ILSM, -1.)
         SM    = SMFCF

*        all summation variables of the water balance are set at zero.
         TRAT = 0.
         EVST = 0.
         EVWT = 0.

*        all rates are set at zero
         EVS = 0.
         EVW = 0.

      ELSE IF (ITASK.EQ.2) THEN

*-----------------------------------------------------------------------
*        rates of the water balance
*-----------------------------------------------------------------------

*        N.B.: transpiration rate is calculated in EVTRA called by CROPSI
*        evaporation rate from soil (non-rice) or water surface (rice)
         EVS = 0.
         EVW = 0.
         IF (IAIRDU.EQ.0) EVS = EVSMX*(SMFCF-SMW/3.)/(SM0-SMW/3.)
         IF (IAIRDU.EQ.1) EVW = EVWMX

      ELSE IF (ITASK.EQ.3) THEN

*-----------------------------------------------------------------------
*        dynamic calculations
*        integrals of the water balance:  summation and state variables
*-----------------------------------------------------------------------

*        total transpiration
         TRAT = TRAT + TRA*DELT
*        total evaporation from surface water layer and/or soil
         EVWT = EVWT + EVW*DELT
         EVST = EVST + EVS*DELT

*        soil permanently at field capacity under potential production :
         SM   = SMFCF

      ELSE IF (ITASK.EQ.4) THEN

*-----------------------------------------------------------------------
*        finish section
*-----------------------------------------------------------------------
*        output variables for summary in MAIN program
         TRATX = TRAT
         EVWTX = EVWT
         EVSTX = EVST

      END IF

      ITOLD = ITASK

      RETURN
      END
