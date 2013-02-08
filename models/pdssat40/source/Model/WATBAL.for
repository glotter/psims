C=======================================================================
C  COPYRIGHT 1998-2005 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  WATBAL, Subroutine, J.T. Ritchie
C  Calculates water balance components.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1989 GH  Written
C  10/18/1995 GH  Standardized for CERES and CROPGRO applications.
C  04/03/1996 GH  Split to handle different hydrology options
C  04/03/1996 GH  Added erosion options
C  07/10/1996 GH  Separated irrigation from precipiation prior to runoff
C               calculations
C  10/03/1997 CHP Re-write WATBAL to modular format and rename SOILWAT
C  11/21/1997 CHP Merged Subroutine RITCHIE with WATBAL
C  09/01/1999 GH  Incorporated into CROPGRO
C  01/13/2000 NBP Removed FDINT.
C  04/10/2001 GH  Relocated OPWBAL
C  06/21/2001 GH  Seasonal initialization for ROOTWU
C  09/17/2001 CHP Added KCAN, PORMIN, RWUEP1, RWUMX to input variables.
C                   (Now imported from PLANT modules.)
C  11/09/2001 WMB/CHP Split WATBAL into WATBAL and SPAM modules.
C  06/12/2002 CHP/US  Added flooded field options
!  10/24/2005 CHP Put weather variables in constructed variable. 
C-----------------------------------------------------------------------
C  Called by: SOIL module
C  Calls:     SNOWFALL, IPWBAL, WBSUM, WTDEPT,        (File WBSUBS.FOR)
C             UPFLOW                                  (File WBSUBS.FOR)
C             INFIL   (File INFIL.FOR)
C             OPWBAL  (File OPWBAL.FOR)
C             RNOFF   (File RNOFF.FOR)
C             SATFLO  (File SATFLO.FOR)
C=======================================================================

      SUBROUTINE WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX, WEATHER,         !Input
     &    FLOODWAT,                                       !I/O
     &    DRN, FLOW, SNOW, SW, SWDELTS, SWDELTU, WINF)    !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE FloodModule
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  ISWWAT

      INTEGER DYNAMIC, L, NLAYR

      REAL CN, CRAIN, DRAIN, ES, EXCS, IRRAMT
      REAL PINF, RAIN, RUNOFF
      REAL SNOW, SWCON, TDRAIN, TMAX, TRUNOF
      REAL TSW, TSWINI, WATAVL, WINF, WTDEP

      REAL, DIMENSION(NL) :: DLAYR, DRN, DS, DUL, FLOW, LL
      REAL, DIMENSION(NL) :: SAT, SW, SW_AVAIL, SWCN
      REAL, DIMENSION(NL) :: SWDELTS, SWDELTU, SWDELTX

!     Flood management variables:
      REAL FLOOD, INFILT, PUDPERC
C-DDD REAL SWTOT1, SWTOT2, DIFFSW
      LOGICAL PUDDLED, BUNDED
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType)  CONTROL
      TYPE (SoilType)     SOILPROP
      TYPE (SwitchType)   ISWITCH
      TYPE (FloodWatType) FLOODWAT
      TYPE (WeatherType)  WEATHER
      
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC

      CN     = SOILPROP % CN     
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS     
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SWCN   = SOILPROP % SWCN   
      SWCON  = SOILPROP % SWCON  

      ISWWAT = ISWITCH % ISWWAT

      FLOOD   = FLOODWAT % FLOOD
      PUDDLED = FLOODWAT % PUDDLED
      PUDPERC = FLOODWAT % PUDPERC
      BUNDED  = FLOODWAT % BUNDED

      RAIN = WEATHER % RAIN
      TMAX = WEATHER % TMAX

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Call IPWBAL to read in values from input file
      CALL IPWBAL (CONTROL, LL, NLAYR,                    !Input
     &    SW, WTDEP)                                      !Output

!     Call daily water values check
      CALL SWCheck(CONTROL, 
     &  DLAYR, LL, NLAYR, SAT, SW)

!     Save infiltration and runoff information for floodwater calcs
      FLOODWAT % INFILT = 0.0
      FLOODWAT % RUNOFF = 0.0

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN
        IF (CONTROL%MULTI .GT. 1) THEN
        !Re-read initial conditions if multi-season run
          CALL IPWBAL (CONTROL, LL, NLAYR,                !Input
     &      SW, WTDEP)                                    !Output
        ENDIF

!       Initialize summary variables
        CALL WBSUM(
     &    NLAYR, DRAIN, RAIN, RUNOFF, DLAYR, SW,          !Input
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI,             !Output
     &    SEASINIT)                                       !Control

!       Call OPWBAL to write headers to output file
        CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL,             !Input
     &    NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input
      ENDIF

!     Initialize snow accumulation
      CALL SNOWFALL (SEASINIT,
     &    TMAX, RAIN,                                     !Input
     &    SNOW, WATAVL)                                   !Output

!     Water balance output initialization
      CALL Wbal(CONTROL, ISWITCH, 
     &    DRAIN, ES, FLOODWAT, IRRAMT, RAIN, RUNOFF, SNOW,
     &    SWDELTS, SWDELTU, SWDELTX, DLAYR, NLAYR,
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)

      DRAIN = 0.0
      DRN   = 0.0
      FLOW  = 0.0
      WINF  = 0.0

!     Set process rates to zero.
      SWDELTS = 0.0
      SWDELTX = 0.0
      SWDELTU = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      !Convert snowfall into precip ( = rain)
      !This is needed for winter crops even if water not simulated.
      IF (TMAX .LE. 1.0 .OR. SNOW .GT. 0.001) THEN
        CALL SNOWFALL (RATE,
     &    TMAX, RAIN,                                 !Input
     &    SNOW, WATAVL)                               !Output
      ELSE
        WATAVL = RAIN
      ENDIF

!     Rates not calculated unless water switch is on.
      IF (ISWWAT .NE. 'Y') RETURN

!     Set process rates to zero.
      SWDELTS = 0.0
      SWDELTX = 0.0
      SWDELTU = 0.0

      PINF   = 0.0
      WINF   = 0.0
      INFILT = 0.0
      EXCS   = 0.0
      DRAIN  = 0.0
!--------------------------------------------------------------------
      !Puddled conditions added for RICE routines. - Puddled implies
      !  bunded conditions exist and soil has been modified to minimize
      !  percolation.  Perc rates read from irrigation data.
      IF (PUDDLED) THEN
      !Puddled field - fill soil profile and limit drainage to PUDPERC
       
        !Water available for infiltration   
        WINF = MAX(0.0, FLOOD + IRRAMT + RAIN)

        !First fill soil profile
        INFILT = 0.0
        DO L = 1, NLAYR
          SWDELTS(L) = SAT(L) - SW(L) 
          INFILT = INFILT + SWDELTS(L) * DLAYR(L) * 10.  !(in mm)
          IF (INFILT .GT. WINF) THEN
            SWDELTS(L) = SWDELTS(L) - (INFILT - WINF) / DLAYR(L) / 10.0
            INFILT = WINF
            EXIT
          ENDIF
        ENDDO

        !Drainage, if any, limited to PUDPERC 
        DRAIN = MIN(PUDPERC, WINF - INFILT)
        INFILT = INFILT + DRAIN
        DO L = 1, NLAYR
          DRN(L) = DRAIN   !used for soil N fluxes
        ENDDO

!--------------------------------------------------------------------
      ELSE
      !Non-puddled, flooded or non-flooded field
        PINF = 0.0
        WINF = 0.0
        INFILT = 0.0

        IF (BUNDED) THEN  !potential for flooding exists
          !No runoff from field with bund (RICE)
          RUNOFF = 0.0
          WINF = MAX(0.0, FLOOD + IRRAMT + RAIN)  !(mm)

        ELSE      !upland field
          !Not bunded, flooded conditions not possible
          !Calculate runoff by Williams-SCS curve number (CN) technique.
          CALL RNOFF(
     &      CN, LL, SAT, SW, WATAVL,                      !Input
     &      RUNOFF)                                       !Output
          WINF = WATAVL - RUNOFF + IRRAMT     !(mm)
        ENDIF

!       Potential for infilitration
        PINF = WINF * 0.1                       !(cm)

!       Call INFIL to calculate infiltration rates on days with irrigation
!         or rainfall.  Call SATFLO on days with no irrigation or rain
!         to calculate saturated flow.
        IF (PINF .GT. 0.0001) THEN
          CALL INFIL(
     &      DLAYR, DUL, NLAYR, PINF, SAT, SW, SWCN, SWCON,!Input
     &      DRAIN, DRN, EXCS, SWDELTS)                    !Output

          INFILT = 0.0
          DO L = 1, NLAYR
            INFILT = INFILT + SWDELTS(L) * DLAYR(L) * 10.  !(in mm)
          ENDDO
          INFILT = INFILT + DRAIN

          !Excess water not infiltrated is added to overland runoff. 
          !If bunded, excess water is accounted for in INFILT variable.
          IF (EXCS > 0 .AND. .NOT. BUNDED) THEN
            RUNOFF = RUNOFF + EXCS * 10.0
          ENDIF

        ELSE
          CALL SATFLO(
     &      DLAYR, DUL, NLAYR, SAT, SW, SWCN, SWCON,      !Input
     &      DRAIN, DRN, SWDELTS)                          !Output
        ENDIF

      ENDIF   !End of IF block for PUDDLED conditions

      IF (FLOOD .LE. 0.0) THEN
!       Calculate the availability of soil water for use in UPFLOW.
        DO L = 1, NLAYR
          SW_AVAIL(L) = MAX(0.0, SW(L) + SWDELTS(L))
        ENDDO

C       Calculate upward movement of water due to evaporation and root 
C       extraction (based on yesterday's values) for each soil layer.
        CALL UPFLOW(    
     &    NLAYR, DLAYR, DUL, LL, SAT, SW, SW_AVAIL,       !Input
     &    FLOW, SWDELTU)                                  !Output
      ENDIF

!     Save infiltration and runoff information for floodwater calcs
      FLOODWAT % INFILT = INFILT
      FLOODWAT % RUNOFF = RUNOFF

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN

C-DDD   CALL SUMSW(NLAYR, DLAYR, SW, SWTOT1)

!       Perform integration of soil water fluxes
!       Subtract soil evaporation from layer 1
        SW(1) = SW(1) - 0.1 * ES / DLAYR(1)

!       Perform integration of soil water fluxes
        DO L = 1, NLAYR
          SW(L) = SW(L) + SWDELTS(L) + SWDELTU(L) + SWDELTX(L)
        ENDDO

C-DDD   CALL SUMSW(NLAYR, DLAYR, SW, SWTOT2)
C-DDD   DIFFSW = SWTOT2 - SWTOT1

!       Perform daily summation of water balance variables.
        CALL WBSUM(
     &    NLAYR, DRAIN, RAIN, RUNOFF, DLAYR, SW,          !Input
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI,             !Output
     &    INTEGR)                                         !Control

!       Call daily water values check
        CALL SWCheck(CONTROL, 
     &    DLAYR, LL, NLAYR, SAT, SW)

        IF (FLOOD .LE. 0.0) THEN
C       Calculate soil water table depth
          CALL WTDEPT(
     &    NLAYR, DLAYR, DS, DUL, SAT, SW,                 !Input
     &    WTDEP)                                          !Output
        ENDIF                   
      ENDIF                   

      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL,             !Input
     &    NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input

!***********************************************************************
!***********************************************************************
!     OUTPUT - Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL,             !Input
     &    NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input

!     Water balance daily output 
      CALL Wbal(CONTROL, ISWITCH, 
     &    DRAIN, ES, FLOODWAT, IRRAMT, RAIN, RUNOFF, SNOW,
     &    SWDELTS, SWDELTU, SWDELTX, DLAYR, NLAYR,
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)

!***********************************************************************
!***********************************************************************
!     FINAL - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
      CALL OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL,             !Input
     &    NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input

!     Water balance seasonal output 
      CALL Wbal(CONTROL, ISWITCH, 
     &    DRAIN, ES, FLOODWAT, IRRAMT, RAIN, RUNOFF, SNOW,
     &    SWDELTS, SWDELTU, SWDELTX, DLAYR, NLAYR,
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END !SUBROUTINE WATBAL

C=====================================================================
      SUBROUTINE SWCheck(CONTROL, 
     &    DLAYR, LL, NLAYR, SAT, SW)
!     Daily check of soil water content values.

!     ------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE

      CHARACTER*6, PARAMETER :: ERRKEY = "WATBAL"
      CHARACTER*78 MESSAGE(10)    !Up to 10 lines of text to be output
      INTEGER NLAYR, L
      INTEGER YRDOY, YR, DOY

      REAL SWEF, LL1
      REAL DLAYR(NL), LL(NL), SAT(NL), SW(NL)

      TYPE (ControlType) CONTROL
      YRDOY  = CONTROL % YRDOY
!     ------------------------------------------------------------------

      SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2
      DO L = 1, NLAYR
        IF (L .EQ. 1) THEN
          LL1 = LL(1) * SWEF
        ELSE
          LL1 = LL(L)
        ENDIF

!       Check for soil water content below acceptable limit
        IF (LL1 - SW(L) .GT. 0.001) THEN
!         Print out-of-range values
          CALL YR_DOY(YRDOY, YR, DOY)
          WRITE(MESSAGE(1), 110) YR, DOY, L 
          WRITE(MESSAGE(2), 120) SW(L), LL1
  110     FORMAT('Day ',I4,1X,I3, ', Layer',I3,':')
  120     FORMAT('Computed soil water content of ', F6.4,
     &      ' is below lower limit of ',F6.4, '.')
          CALL WARNING(2, ERRKEY, MESSAGE)

!       Check for soil water greater than saturation in layer 1.
        ELSEIF ((SW(L) - SAT(L)) .GT. 0.001) THEN
!         Print out-of-range values.
          CALL YR_DOY(YRDOY, YR, DOY)
          WRITE(MESSAGE(1), 130) YR, DOY, L 
          WRITE(MESSAGE(2), 140) SW(L), SAT(L)
  130     FORMAT('Day ',I4,1X,I3, ', Layer',I3,':')
  140     FORMAT('Computed soil water content of ', F6.4,
     &      ' is above saturated limit of ',F6.4, '.')
          CALL WARNING(2, ERRKEY, MESSAGE)

        ENDIF
      ENDDO

!--------------------------------------------------------------------
      RETURN
      END SUBROUTINE SWCheck
C=====================================================================

C=====================================================================
C-DDD  SUBROUTINE SUMSW(NLAYR, DLAYR, SW, SWTOT)
C-DDD
C-DDD  USE MODULEDEFS
C-DDD
C-DDD  INTEGER L, NLAYR
C-DDD  REAL SW(NL), DLAYR(NL)
C-DDD  REAL SWTOT
C-DDD
C-DDD  SWTOT = 0.0
C-DDD  DO L = 1, NLAYR
C-DDD    SWTOT = SWTOT + SW(L) * DLAYR(L)
C-DDD  ENDDO
C-DDD  SWTOT = SWTOT * 10.0
C-DDD
C-DDD  END SUBROUTINE SUMSW
C=====================================================================

C=====================================================================
!     VARIABLE DEFINITIONS: (updated 12 Feb 2004)
!-----------------------------------------------------------------------
! BUNDED      Logical variable indicating if field is currently bunded 
! CN          Runoff Curve Number - measure of runoff potential based on 
!               soil type and current soil water content. 
! CONTROL     Composite variable containing variables related to control 
!               and/or timing of simulation.    See Appendix A. 
! CRAIN       Cumulative precipitation (mm)
! DLAYR(L)    Thickness of soil layer L (cm)
! DRAIN       Drainage rate from soil profile (mm/d)
! DRN(L)      Drainage rate through soil layer L (cm/d)
! DS(L)       Cumulative depth in soil layer L (cm)
! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil 
!               layer L (cm3[water]/cm3[soil])
! DYNAMIC     Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!               INTEGR, OUTPUT, or FINAL 
! ES          Actual soil evaporation rate (mm/d)
! EXCS        Excess water to be added to runoff (cm/d)
! FLOOD       Current depth of flooding (mm)
! FLOODWAT    Composite variable containing information related to bund 
!               management. Structure of variable is defined in 
!               ModuleDefs.for. 
! FLOW(L)     Movement of water between unsaturated soil layers due to soil 
!               evaporation: + = upward, -- = downward (cm/d)
! INFILT      Infiltration rate (mm / d)
! IRRAMT      Irrigation amount for today (mm / d)
! ISWWAT      Water simulation control switch (Y or N) 
! LL(L)       Volumetric soil water content in soil layer L at lower limit
!              (cm3 [water] / cm3 [soil])
! NLAYR       Actual number of soil layers 
! PINF        Potential water available for infiltration (cm)
! PUDDLED     Logical variable indicating whether lowland field has been 
!               puddled 
! PUDPERC     Potential percolation rate for puddled field (mm/d)
! RAIN        Precipitation depth for current day (mm)
! RUNOFF      Calculated runoff (mm/d)
! SAT(L)      Volumetric soil water content in layer L at saturation
!              (cm3 [water] / cm3 [soil])
! SNOW        Snow accumulation (mm)
! SW(L)       Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! SW_AVAIL(L) Soil water content in layer L available for evaporation, 
!               plant extraction, or movement through soil
!               (cm3 [water] / cm3 [soil])
! SWCN(L)     Saturated hydraulic conductivity in layer L (cm/hr)
! SWCON       Soil water conductivity constant; whole profile drainage rate 
!               coefficient (1/d)
! SWDELTS(L)  Change in soil water content due to drainage in layer L
!              (cm3 [water] / cm3 [soil])
! SWDELTU(L)  Change in soil water content due to evaporation and/or upward 
!               flow in layer L (cm3 [water] / cm3 [soil])
! SWDELTX(L)  Change in soil water content due to root water uptake in 
!               layer L (cm3 [water] / cm3 [soil])
! TDRAIN      Cumulative daily drainage from profile (mm)
! TMAX        Maximum daily temperature (°C)
! TRUNOF      Cumulative runoff (mm)
! TSW         Total soil water in profile (cm)
! TSWINI      Initial soil water content (cm)
! WATAVL      Water available for infiltration or runoff (rainfall plus 
!               irrigation) (mm/d)
! WINF        Water available for infiltration - rainfall minus runoff plus 
!               net irrigation (mm / d)
! WTDEP       Depth to water table (cm)
!-----------------------------------------------------------------------
!     END SUBROUTINE WATBAL
C=====================================================================

