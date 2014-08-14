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
      SUBROUTINE WOFSIM (
     &           IUWE  , IWEATH, WTRDIR, CNTR  , ISTN  , CLFILE,
     &           IURA  , IRNDAT, ISYRR , RAFILE, RSETRG, RSETRD,
     &           IUPL  , CRFILE, CRPNAM,
     &           IUSO  , SOFILE,
     &           GEODIR, DRVDIR, RUNDIR, DBRDIR,  
     &           ISYR  , IDAYB , IDAYEN, IDURMX, PRDEL,
     &           IDEM  , IDSOW , IDESOW, IDLSOW, ISTATE, 
     &           IYEARR, IUOUT , IULOG ,
     &           IWB   , IOX   , IZT   , IBAL  ,
     &           IFUNRN, SSI   , SMLIM , SSMAX , WAV   , ZTI,
     &           IDRAIN, DD    , RDMSOL, NOTINF, 
     &           ISDAY, NOCROP)

*     This routine organizes the simulation of crop growth and of soil
*     water conditions, the generation of weather data and the writing
*     of reports by successive calls to the relevant weather, crop, soil
*     water and printing subroutines, in that order.

*     MODE   - Type of calculation that should be carried out within
*              the loop across the stations

*     Authors: C.A. van Diepen and C. Rappoldt, September 1988.
*     revised : C.A. van Diepen, July 1990, revised: D. van Kraalingen
*     April 1991.

*     WOFSIM calls subroutines under a task number to perform a 
*     specific task. Four different tasks are distinguished :
*      ITASK = 1, initialization 
*      ITASK = 2, generation of driving variables, rate calculation and 
*                 daily output
*      ITASK = 3, integration
*      ITASK = 4, finish section, including final output
*     During program execution the value of the task number changes.
*     When a given task is completed by all subroutines, the next task
*     is specified in WOFSIM.
*     After the initialization, the dynamic calculations are performed
*     as repetitative executions of tasks 2 and 3, until the crop growth
*     ends. 
*     The crop and water routines take specific action under each task. 
*     The weather routine takes action under tasks 1 and 3 only. 
*     The print routine takes action under tasks 1, 3 and 4 only. 

*     The subroutine consists of three dynamic loops. The number of loops
*     that is carried out depends on the start day options as set by the user
*     in the main program. The first loop is executed if the model has to find
*     a suitable sowing date before sowing can take place. This is done by
*     repetitive calls to STDAY. The second loop is able to simulate crop emer-
*     gence by repetitive calls to CROPSI. The third loop is the actual crop
*     simulation, starting from emergence. Summary:

*     Variable sowing date: 1st, 2nd 3rd loop (find sowing date, find emergence
*                           date, do crop simulation),
*     Fixed sowing date   : 2nd, 3rd loop (find emergence date, do crop
*                           simulation)
*     Fixed emergence date: 3rd loop only (do crop simulation)

*     All loops are controlled by the TIMER2 routine and need weather
*     data.
*     Crop growth is calculated for a situation defined by the crop 
*     cultivar, the date of emergence, the weather, and soil moisture 
*     conditions during the growth period. 

*     There are three standard options for the simulation of the soil 
*     water balance: 
*     WATPP for potential production, 
*     WATFD for water-limited production on freely draining soils, and 
*     WATGW for water-limited production on soils with influence of a
*     groundwater table in the root zone. 
*     In dependence of the soil waterbalance option, WOFSIM calculates 
*     the maximum obtainable crop yield under unlimited water supply,
*     or under rainfed cropping on a site with or without influence 
*     of a groundwater table in the root zone. 
*     The simulations of crop and soil water systems are performed 
*     synchronously in timesteps of one day.
*     +-----------------------------------------------------------------+
*     | Version:      1.2                                               |
*     | Date:         24 July 1997                                      |
*     | Author:       Tamme van der Wal                                 |
*     |               Group Software Engineering                        |
*     | Reason:       Adaptation of soil water balance calculations     |
*     | Modification: Addition of 2 new variables in site.dat           |
*     |               which need to be passed to WATFD and WATGW through|
*     |               WOFSIM. Variable SSI was already passed, variable |
*     |               SMLIM is added to parameter list                  |
*     +-----------------------------------------------------------------+ 
*     +-----------------------------------------------------------------+
*     | Version:      1.2                                               |
*     | Date:         24 July 1997                                      |
*     | Author:       Tamme van der Wal                                 |
*     |               Group Software Engineering                        |
*     | Reason:       TOGIS is no longer used                           |
*     | Modification: Removal of TOGIS logical in WOFSIM                |
*     +---------------------------------------------------------------+
*     | Version:      1.3                                             |
*     | Date:         24 September 1997                               |
*     | Author:       Tamme van der Wal                               |
*     |               Group Software Engineering                      |
*     | Reason:       Angstrom coefficient read in from weather file  |
*     | Modification: COEFA and COEFB removed from interface          |
*    +----------------------------------------------------------------+
*    | MODIFICATION                                                   |
*    | author:       Hendrik Boogaard                                 |
*    | date:         02-06-1998                                       |
*    | reason:       - FSEOPT                                         |
*    |               - variables removed                              |
*    |               - earlier start waterbalance                     |
*    | modification:                                                  |
*    | - FSEOPT: - conversion year and day after call timer           |
*    |           - call of OBSINI (initialization observations)       |
*    |           - adjustement of calling WOFSIM (INOD, IOBSD)        |
*    |           - adjustement of calling CROPSI (MODE, OUTPUT)       |
*    |           - determination if output, only when there's an ob-  |
*    |             vation.                                            |
*    | - variables removed:                                           |
*    |           - IUSCR removed                                      |
*    | - earlier start water balance                                  |
*    |           - variable ISDAY and NOCROP introduced               |
*    |           - adjustement of call WOFSIM (ISDAY, NOCROP)         |
*    |           - IDAYB becomes ISDAY when NOCROP is true            |
*    |           - writing ISDAY to detailed output                   |
*    |           - introduction of subroutine CROPNO: simulates the   |
*    |             waterbalance when there's no crop                  |
*    |           - logical NOCROP controls the calling of CROPNO,     |
*    |             determines the variable OUTPUT, controls the       |
*    |             assignment of ISDAY to IDAYB etc.                  |
*    |           - introduction of statements that change NOCROP and  |
*    |             and OUTPUT when emergence is reached               |                                                       |
*    +----------------------------------------------------------------+
*    | MODIFICATION                                                   |
*    | author:       Hendrik Boogaard                                 |
*    | date:         15-06-1998                                       |
*    | reason:       - bug initialization timer                       |
*    |               - bug end of NOCROP condition                    |
*    | modification:                                                  |
*    | in water limited prodution when iday reaches idem TIMER is not |
*    | initialized again                                              |
*    | in water limited production when iday reaches idem you only    |
*    | initialize once by adding NOCROP in IF-statement               |
*    +----------------------------------------------------------------+
*    | MODIFICATION                                                   |
*    | author:       Allard de Wit                                    |
*    | date:         25-02-2011                                       |
*    | reason:       - removed FSEOPT from source tree                |
*    +----------------------------------------------------------------+


      IMPLICIT REAL (A-Z)
*     formal parameters
      INTEGER ISYR  , IDAYB , IDAYEN, IDURMX
      INTEGER IDEM  , IDSOW , IDESOW, IDLSOW
      INTEGER ISTATE, IYEARR, IUOUT , IUWE
      INTEGER IWEATH, ISTN  , IURA  , IRNDAT, ISYRR , IUPL , IULOG
      INTEGER IUSO  , IWB   , IOX   , IZT   , IBAL
      INTEGER IFUNRN, IDRAIN, ISDAY
      REAL    SSI, SMLIM

      CHARACTER*(*) WTRDIR,CNTR,CLFILE,RAFILE,CRFILE,CRPNAM,SOFILE
      CHARACTER*(*) GEODIR,DRVDIR,RUNDIR,DBRDIR

      LOGICAL RSETRG, RSETRD, NOCROP

**    local parameters
      INTEGER IAIRDU, IDHALT, IDSEM, ITASK  , IDOLD, INDAYS
      INTEGER IYEAR,  IDAY
      LOGICAL DOANTH, TERMNL, OUTPUT, INICRP
      REAL    LIFEMX

      SAVE

      IUNITD = IUPL
      IUNITL = IULOG

      IYEAR  = ISYR
      IYEARR = ISYRR

      IDSEM = 0
      DELT  = 1.

      INICRP = .FALSE.
      TERMNL = .FALSE.

      ITASK  = 1

*     initialize day counting and meteo data
*     if start day of waterbalance (ISDAY) is greater then start day
*     crop (IDAYB), then assume that the start day of the water balance
*     is in the previous year. Adjust IYEAR and FINTIM accordingly
      IF (NOCROP) THEN
        IF (ISDAY.GT.IDAYB) THEN
            FINTIM = REAL(IDAYB) + (365.-(ISDAY-IDAYB)+1) + 365.
            IDAYB = ISDAY
            IYEAR = IYEAR-1
        ELSE 
            FINTIM = REAL (IDAYB)+365.
            IDAYB = ISDAY
        END IF
      ELSE
        FINTIM = REAL (IDAYB)+365.
      END IF

      CALL TIMER2 (ITASK, REAL(IDAYB),  DELT, PRDEL , FINTIM,
     &             IYEAR, TIME, DAY   , IDAY, TERMNL, OUTPUT)

*      IF (NOCROP) THEN
*        OUTPUT = .FALSE.
*      ENDIF

      CALL METEO (IYEAR , IDAY  , IWEATH, WTRDIR, CNTR ,
     &            GEODIR, DRVDIR, RUNDIR, DBRDIR,
     &            ISTN  , CLFILE, IUWE  , IRNDAT, RSETRG,
     &            RSETRD, IURA  , IYEARR, RAFILE, 
     &            LAT   , AVRAD , TMIN  , TMAX  , RAIN  , WIND ,
     &            VAPOUR, E0    , ES0   , ET0)

*-----------------------------------------------------------------------
*     find date of sowing if model starts at sowing
*-----------------------------------------------------------------------

      IF (ISTATE.EQ.0) THEN
         ITASK = 1
         CALL STDAY (ITASK , CRPNAM, SOFILE, IUSO, IUOUT, IULOG,
     &               RAIN  , ES0   , IDAY, IDESOW,
     &               IDLSOW, ISTATE, COSUT)

10       IF (.NOT.TERMNL.AND.ISTATE.NE.1) THEN
            ITASK  = 3
            CALL STDAY (ITASK , CRPNAM, SOFILE, IUSO, IUOUT, IULOG,
     &                  RAIN  , ES0   , IDAY, IDESOW,
     &                  IDLSOW, ISTATE, COSUT)

            IF (ISTATE.EQ.1) IDSOW = IDAY

            CALL METEO (IYEAR , IDAY  , IWEATH, WTRDIR, CNTR  ,
     &                  GEODIR, DRVDIR, RUNDIR, DBRDIR,
     &                  ISTN  , CLFILE, IUWE  , IRNDAT, RSETRG,
     &                  RSETRD, IURA  , IYEARR, RAFILE, 
     &                  LAT   , AVRAD , TMIN  , TMAX  , RAIN  , WIND ,
     &                  VAPOUR, E0    , ES0   , ET0)
            ITASK  = 2
            IDOLD = IDAY
            CALL TIMER2 (ITASK, REAL(IDAYB),  DELT, PRDEL , FINTIM,
     &                   IYEAR, TIME, DAY   , IDAY, TERMNL, OUTPUT)

            IF (IDAY.LT.IDOLD) IYEARR = IYEARR+1
            GOTO 10
         END IF
*        sowing date is known at this point
      END IF


*-----------------------------------------------------------------------
*     loop until emergence takes place, initialize CROPSI first
*-----------------------------------------------------------------------

      IF (.NOT.TERMNL .AND. ISTATE.EQ.1) THEN
         ITASK = 1
*        initialize day counting and meteo data 
         FINTIM = REAL (IDSOW)+365.
         CALL TIMER2 (ITASK, REAL(IDSOW),  DELT, PRDEL , FINTIM,
     &                IYEAR, TIME, DAY   , IDAY, TERMNL, OUTPUT)

         CALL CROPSI
     &        (ITASK, IDAY  , DELT , TIME, IDEM, DOANTH, IDHALT,
     &        TERMNL, ISTATE, IWB  , IOX ,
     &        LAT   , AVRAD , TMIN , TMAX, E0  , ES0, ET0,
     &        CRFILE, IUPL  , IUOUT, IULOG,
     &        SM    , SM0   , SMFCF, SMW , CRAIRC,
     &        EVWMX , EVSMX , TRA  , FR  , RRI   ,IAIRDU,
     &        RDI   , RDMCR)

         INICRP = .TRUE.
         ISTATE = 2

20       IF (.NOT.TERMNL.AND. ISTATE.NE.3) THEN
            ITASK = 3

            CALL CROPSI
     &           (ITASK, IDAY  , DELT , TIME, IDEM, DOANTH, IDHALT,
     &           TERMNL, ISTATE, IWB  , IOX ,
     &           LAT   , AVRAD , TMIN , TMAX, E0  , ES0, ET0,
     &           CRFILE, IUPL  , IUOUT, IULOG,
     &           SM    , SM0   , SMFCF, SMW , CRAIRC,
     &           EVWMX , EVSMX , TRA  , FR  , RRI   ,IAIRDU,
     &           RDI   , RDMCR)

            ITASK = 2

            CALL METEO (IYEAR , IDAY  , IWEATH, WTRDIR, CNTR  ,
     &                  GEODIR, DRVDIR, RUNDIR, DBRDIR,
     &                  ISTN  , CLFILE, IUWE  , IRNDAT, RSETRG,
     &                  RSETRD, IURA  , IYEARR, RAFILE, 
     &                  LAT   , AVRAD , TMIN  , TMAX  , RAIN  , WIND ,
     &                  VAPOUR, E0    , ES0   , ET0)

            IF (ISTATE.EQ.3) THEN
               IDEM = IDAY
            ELSE
               CALL CROPSI
     &           (ITASK, IDAY  , DELT , TIME, IDEM, DOANTH, IDHALT,
     &            TERMNL, ISTATE, IWB  , IOX ,
     &            LAT   , AVRAD , TMIN , TMAX, E0  , ES0, ET0,
     &            CRFILE, IUPL  , IUOUT, IULOG,
     &            SM    , SM0   , SMFCF, SMW , CRAIRC,
     &            EVWMX , EVSMX , TRA  , FR  , RRI   ,IAIRDU,
     &            RDI   , RDMCR)

               IDOLD = IDAY
               CALL TIMER2 (ITASK, REAL(IDSOW), DELT, PRDEL , FINTIM,
     &                      IYEAR, TIME, DAY  , IDAY, TERMNL, OUTPUT)
               IF (IDAY.LT.IDOLD) IYEARR = IYEARR+1

            END IF
            GOTO 20
         END IF
*        emergence date should be known at this point\
      END IF


*-----------------------------------------------------------------------
*     loop until crop ripening takes place, initialize CROPSI if 
*     necessary (only when model is started at emergence)
*-----------------------------------------------------------------------

      IF (.NOT.TERMNL .AND. ISTATE.EQ.3) THEN
         ITASK = 1
         IF (PRDEL.GT.0 .AND. IWB.EQ.0) THEN
          WRITE (IUOUT,'(5X,A,5X,A,I3,5X,A,I3,/)') 
     &     'start waterbalance = -99','sowing date = ',IDSOW,
     &     'emergence date = ',IDEM
         ELSEIF (PRDEL.GT.0 .AND. IWB.EQ.1) THEN
          WRITE (IUOUT,'(5X,A,I3,A,I4,A,2X,A,I3,2X,A,I3,/)') 
     &     'start waterbalance = ',ISDAY,' (',IYEAR,')',
     &     'sowing date = ',IDSOW, 'emergence date = ',IDEM
         ENDIF
*           re-initialize day counting at emergence
*           determine maximum duration of crop cycle from emergence
         IF (IYEAR.LT.1500 .OR. MOD(IYEAR,4).NE.0) THEN
            INDAYS=365
         ELSEIF ( MOD(IYEAR,4).EQ.0) THEN
            INDAYS=366
         ENDIF

         IF (IDURMX.GT.0 .AND. IDAYEN.LT.0) THEN
            LIFEMX = REAL(IDURMX)
         ELSEIF (IDURMX.LT.0. .AND. IDAYEN.GT.0) THEN
            LIFEMX = REAL(MOD((INDAYS+IDAYEN-IDEM),INDAYS))
         ELSEIF (IDURMX.GT.0. .AND. IDAYEN.GT.0) THEN
            LIFEMX = REAL(MIN
     &              (MOD((INDAYS+IDAYEN-IDEM),INDAYS),IDURMX))
         ELSE
            CALL ERROR 
     &      ('WOFSIM',' negative values IDURMX and IDAYEN')
         ENDIF

         FINTIM = LIFEMX+REAL (IDEM)

         IF (.NOT.NOCROP) THEN
            CALL TIMER2 (ITASK, REAL(IDEM), DELT, PRDEL , FINTIM,
     &                   IYEAR, TIME, DAY , IDAY, TERMNL, OUTPUT)
         ENDIF

         IF (.NOT.INICRP) THEN
            CALL CROPSI
     &           (ITASK , IDAY  , DELT , TIME, IDEM, DOANTH, IDHALT,
     &            TERMNL, ISTATE, IWB  , IOX ,
     &            LAT   , AVRAD , TMIN , TMAX, E0  , ES0, ET0,
     &            CRFILE, IUPL  , IUOUT, IULOG,
     &            SM    , SM0   , SMFCF, SMW , CRAIRC,
     &            EVWMX , EVSMX , TRA  , FR  , RRI   ,IAIRDU,
     &            RDI   , RDMCR)
            INICRP = .TRUE.
         END IF

         CALL ROOTD
     &       (ITASK, DELT , IWB   , IZT, FR, RRI, IAIRDU,
     &        RDI  , RDMCR, RDMSOL, ZTI, ZT, RDM, RD)

         ISTATE = 4

         IF (IWB.EQ.0)
     &      CALL WATPP (ITASK , DELT, SOFILE, IUSO, IUOUT, IULOG,
     &                  IAIRDU, SM0 , SMFCF , SMW , EVWMX, EVSMX,
     &                  TRA   , SM)

         IF (IWB.EQ.1 .AND. IZT.EQ.0)
     &      CALL WATFD (ITASK , DELT  , IDEM  , IDHALT,
     &                  SOFILE, IUSO  , IUOUT , IULOG , RDM   , RD,
     &                  IAIRDU, IFUNRN, SSI   , SMLIM , SSMAX , WAV ,
     &                  NOTINF, EVWMX , EVSMX , TRA   , SMW ,
     &                  CRAIRC, SM    , RAIN  , SM0   , SMFCF)

         IF (IWB.EQ.1 .AND. IZT.EQ.1)
     &      CALL WATGW (ITASK , DELT  , IDEM  , IDHALT,
     &                  TERMNL, SOFILE, IUSO  , IUOUT , IULOG ,IDRAIN,
     &                  RD    , IAIRDU, IFUNRN, SSI   , SMLIM ,SSMAX ,
     &                  ZTI   , DD    , NOTINF, EVWMX , EVSMX ,
     &                  TRA   , SMW   , CRAIRC, ZT    , SM,  
     &                  RAIN  , SM0   , SMFCF)

         IF (PRDEL.GT.0..AND.IWB.EQ.0)
     &      CALL PRIWPP (ITASK, IYEAR , IDAY, IDSEM, IUOUT, RD)

         IF (PRDEL.GT.0..AND.IWB.EQ.1 .AND. IZT.EQ.0)
     &      CALL PRIWFD (ITASK , IYEAR , IDAY  ,
     &                   IUOUT , IBAL  , IOX   , IFUNRN,
     &                   SM0   , SMFCF , SMW   , SMLIM , RDM   , WAV,
     &                   NOTINF, RD    , SSMAX , RDMSOL)

         IF (PRDEL.GT.0..AND.IWB.EQ.1 .AND. IZT.EQ.1)
     &      CALL PRIWGW (ITASK , IYEAR , IDAY  , IDSEM,
     &                   IUOUT , IBAL  , IFUNRN, IDRAIN,
     &                   SM0   , SMFCF , SMW  , SMLIM , RDM   , ZTI,
     &                   DD    , NOTINF, RD   , SSMAX , RDMSOL)

30       IF (.NOT.TERMNL) THEN
            ITASK = 3
          IF (NOCROP) THEN
               CALL CROPNO (RDI,E0,ES0,RD,EVWMX,EVSMX,TRA)     
            ELSE
               CALL CROPSI
     &           (ITASK , IDAY  , DELT , TIME, IDEM, DOANTH, IDHALT,
     &            TERMNL, ISTATE, IWB  , IOX ,
     &            LAT   , AVRAD , TMIN , TMAX, E0  , ES0, ET0,
     &            CRFILE, IUPL  , IUOUT, IULOG,
     &            SM    , SM0   , SMFCF, SMW , CRAIRC,
     &            EVWMX , EVSMX , TRA  , FR  , RRI   ,IAIRDU,
     &            RDI   , RDMCR)

               CALL ROOTD
     &           (ITASK, DELT , IWB   , IZT, FR, RRI, IAIRDU,
     &            RDI  , RDMCR, RDMSOL, ZTI, ZT, RDM, RD)
            ENDIF

            IF (IWB.EQ.0)
     &         CALL WATPP (ITASK , DELT, SOFILE, IUSO, IUOUT, IULOG,
     &                     IAIRDU, SM0 , SMFCF , SMW , EVWMX, EVSMX,
     &                     TRA   , SM)
  
            IF (IWB.EQ.1 .AND. IZT.EQ.0)
     &         CALL WATFD (ITASK , DELT  , IDEM  , IDHALT,
     &                     SOFILE, IUSO  , IUOUT , IULOG , RDM   , RD, 
     &                     IAIRDU, IFUNRN, SSI   , SMLIM , SSMAX , WAV ,
     &                     NOTINF, EVWMX , EVSMX , TRA   , SMW ,
     &                     CRAIRC, SM    , RAIN  , SM0   , SMFCF)

            IF (IWB.EQ.1 .AND. IZT.EQ.1)
     &         CALL WATGW (ITASK , DELT  , IDEM  , IDHALT,
     &                     TERMNL, SOFILE, IUSO  , IUOUT ,IULOG,IDRAIN,
     &                     RD    , IAIRDU, IFUNRN, SSI   ,SMLIM,SSMAX ,
     &                     ZTI   , DD    , NOTINF, EVWMX , EVSMX ,
     &                     TRA   , SMW   , CRAIRC, ZT    , SM,
     &                     RAIN  ,SM0    , SMFCF)

            ITASK = 2

            CALL METEO (IYEAR , IDAY  , IWEATH, WTRDIR, CNTR  ,
     &                  GEODIR, DRVDIR, RUNDIR, DBRDIR,
     &                  ISTN  , CLFILE, IUWE  , IRNDAT, RSETRG,
     &                  RSETRD, IURA  , IYEARR, RAFILE, 
     &                  LAT   , AVRAD , TMIN  , TMAX  , RAIN  , WIND ,
     &                  VAPOUR, E0    , ES0   , ET0)
     
            IF (NOCROP) THEN
               CALL CROPNO (RDI,E0,ES0,RD,EVWMX,EVSMX,TRA)  
            ELSE
               CALL CROPSI
     &             (ITASK, IDAY  , DELT , TIME, IDEM, DOANTH, IDHALT,
     &              TERMNL, ISTATE, IWB  , IOX ,
     &              LAT   , AVRAD , TMIN , TMAX, E0  , ES0, ET0,
     &              CRFILE, IUPL  , IUOUT, IULOG,
     &              SM    , SM0   , SMFCF, SMW , CRAIRC,
     &              EVWMX , EVSMX , TRA  , FR  , RRI   ,IAIRDU,
     &              RDI   , RDMCR)
               CALL ROOTD
     &             (ITASK, DELT , IWB   , IZT, FR, RRI, IAIRDU,
     &              RDI  , RDMCR, RDMSOL, ZTI, ZT, RDM, RD)
            ENDIF

            IF (IWB.EQ.0)
     &         CALL WATPP (ITASK , DELT, SOFILE, IUSO, IUOUT, IULOG,
     &                     IAIRDU, SM0 , SMFCF , SMW , EVWMX, EVSMX,
     &                     TRA   , SM)

            IF (IWB.EQ.1 .AND. IZT.EQ.0)
     &         CALL WATFD (ITASK , DELT  , IDEM  , IDHALT,
     &                     SOFILE, IUSO  , IUOUT , IULOG , RDM , RD,
     &                     IAIRDU, IFUNRN, SSI   , SMLIM , SSMAX , WAV ,
     &                     NOTINF, EVWMX , EVSMX , TRA   , SMW ,
     &                     CRAIRC, SM    , RAIN  , SM0   , SMFCF)

            IF (IWB.EQ.1 .AND. IZT.EQ.1)
     &         CALL WATGW (ITASK , DELT  , IDEM  , IDHALT,
     &                     TERMNL, SOFILE, IUSO  , IUOUT ,IULOG,IDRAIN,
     &                     RD    , IAIRDU, IFUNRN, SSI   ,SMLIM,SSMAX,
     &                     ZTI   , DD    , NOTINF, EVWMX , EVSMX ,
     &                     TRA   , SMW   , CRAIRC, ZT    , SM,
     &                     RAIN  ,SM0    , SMFCF)

*           print standard output report (WOFOST.OUT):
            IF (PRDEL  .GT.0.     .AND.
     &         (OUTPUT .OR.TERMNL .OR.
     &          IDAY   .EQ.IDEM   .OR. DOANTH)) THEN
               IF (IWB.EQ.0)
     &            CALL PRIWPP (ITASK, IYEAR, IDAY, IDSEM, IUOUT, RD)

               IF (IWB.EQ.1 .AND. IZT.EQ.0)
     &            CALL PRIWFD (ITASK , IYEAR , IDAY  ,
     &                         IUOUT , IBAL  , IOX   , IFUNRN,
     &                         SM0   , SMFCF , SMW   , SMLIM , RDM, WAV,
     &                         NOTINF, RD    , SSMAX , RDMSOL)

               IF (IWB.EQ.1 .AND. IZT.EQ.1)
     &            CALL PRIWGW (ITASK , IYEAR , IDAY  , IDSEM,
     &                         IUOUT , IBAL  , IFUNRN, IDRAIN,
     &                         SM0   , SMFCF , SMW   , SMLIM , RDM, ZTI,
     &                         DD    , NOTINF, RD    , SSMAX , RDMSOL)
               DOANTH = .FALSE.
            END IF

            IDOLD = IDAY
            CALL TIMER2 (ITASK, REAL(IDEM), DELT, PRDEL , FINTIM,
     &                   IYEAR, TIME, DAY , IDAY, TERMNL, OUTPUT)

            IF (IDAY.LT.IDOLD) IYEARR = IYEARR+1
            IDSEM = INT (TIME)-IDEM

C ---       switch off nocrop condition and reinitialize day counting
            IF (IDAY.EQ.IDEM .AND. NOCROP) THEN
              NOCROP = .FALSE.
              ITASK = 1
              IF (IYEAR.LT.1500 .OR. MOD(IYEAR,4).NE.0) THEN
                INDAYS=365
              ELSEIF ( MOD(IYEAR,4).EQ.0) THEN
                INDAYS=366
              ENDIF
              IF (IDURMX.GT.0 .AND. IDAYEN.LT.0) THEN
                LIFEMX = REAL(IDURMX)
              ELSEIF (IDURMX.LT.0. .AND. IDAYEN.GT.0) THEN
                LIFEMX = REAL(MOD((INDAYS+IDAYEN-IDEM),INDAYS))
              ELSEIF (IDURMX.GT.0. .AND. IDAYEN.GT.0) THEN
                LIFEMX = REAL(MIN
     &              (MOD((INDAYS+IDAYEN-IDEM),INDAYS),IDURMX))
              ENDIF
              FINTIM = LIFEMX+REAL (IDEM)
              CALL TIMER2 (ITASK, REAL(IDEM), DELT, PRDEL , FINTIM,
     &                     IYEAR, TIME, DAY , IDAY, TERMNL, OUTPUT)

            ENDIF

*           IF (NOCROP) THEN
*             OUTPUT =.FALSE.
*           ENDIF

         GOTO 30
         END IF
      END IF

      ITASK = 4

      CALL CROPSI
     &         (ITASK, IDAY  , DELT , TIME, IDEM, DOANTH, IDHALT,
     &          TERMNL, ISTATE, IWB  , IOX ,
     &          LAT   , AVRAD , TMIN , TMAX, E0  , ES0, ET0,
     &          CRFILE, IUPL  , IUOUT, IULOG,
     &          SM    , SM0   , SMFCF, SMW , CRAIRC,
     &          EVWMX , EVSMX , TRA  , FR  , RRI   ,IAIRDU,
     &          RDI   , RDMCR)

      CALL ROOTD
     &     (ITASK, DELT , IWB   , IZT, FR, RRI, IAIRDU,
     &      RDI  , RDMCR, RDMSOL, ZTI, ZT, RDM, RD)

      IF (IWB.EQ.0)
     &   CALL WATPP (ITASK , DELT, SOFILE, IUSO, IUOUT, IULOG,
     &               IAIRDU, SM0 , SMFCF , SMW , EVWMX, EVSMX,
     &               TRA   , SM)

      IF (IWB.EQ.1 .AND. IZT.EQ.0)
     &   CALL WATFD (ITASK , DELT  , IDEM  , IDHALT,
     &               SOFILE, IUSO  , IUOUT , IULOG , RDM , RD, 
     &               IAIRDU, IFUNRN, SSI   , SMLIM , SSMAX , WAV ,
     &               NOTINF, EVWMX , EVSMX , TRA   , SMW ,
     &               CRAIRC, SM    , RAIN  , SM0   , SMFCF)

      IF (IWB.EQ.1 .AND. IZT.EQ.1)
     &   CALL WATGW (ITASK , DELT  , IDEM  , IDHALT,
     &               TERMNL, SOFILE, IUSO  , IUOUT , IULOG , IDRAIN,
     &               RD    , IAIRDU, IFUNRN, SSI   , SMLIM , SSMAX ,
     &               ZTI   , DD    , NOTINF, EVWMX , EVSMX ,
     &               TRA   , SMW   , CRAIRC, ZT    , SM, 
     &               RAIN  , SM0   , SMFCF)

      IF (PRDEL.GT.0..AND.IWB.EQ.0)
     &   CALL PRIWPP (ITASK, IYEAR, IDAY, IDSEM, IUOUT, RD)

      IF (PRDEL.GT.0..AND.IWB.EQ.1 .AND. IZT.EQ.0)
     &   CALL PRIWFD (ITASK , IYEAR , IDAY  ,
     &                IUOUT , IBAL  , IOX   , IFUNRN,
     &                SM0   , SMFCF , SMW   , SMLIM , RDM   , WAV,
     &                NOTINF, RD    , SSMAX , RDMSOL)

      IF (PRDEL.GT.0..AND.IWB.EQ.1 .AND. IZT.EQ.1)
     &   CALL PRIWGW (ITASK , IYEAR , IDAY  , IDSEM,
     &                IUOUT , IBAL  , IFUNRN, IDRAIN,
     &                SM0   , SMFCF , SMW   , SMLIM , RDM   , ZTI,
     &                DD    , NOTINF, RD    , SSMAX , RDMSOL)


      RETURN
      END
