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
C  SPAM, Subroutine
C  Calculates soil-plant-atmosphere interface energy balance components.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  11/09/2001 WMB/CHP Split WATBAL into WATBAL and SPAM.
C  02/06/2003 KJB/CHP Added KEP, EORATIO inputs from plant routines.
C  06/19/2003 CHP Added KTRANS - used instead of KEP in TRANS.
C  04/01/2004 CHP/US Added Penman - Meyer routine for potential ET
!  10/24/2005 CHP Put weather variables in constructed variable. 
C-----------------------------------------------------------------------
C  Called by: Main
C  Calls:     XTRACT, OPSPAM    (File SPSUBS.FOR)
C             PETPEN  (File PET.FOR)
C             PETDYN  (File PET.FOR)
C             PETPT   (File PET.FOR)
C             PSE     (File SOILEV.FOR)
C             ROOTWU  (File ROOTWU.FOR)
C             SOILEV  (File SOILEV.FOR)
C             TRANS   (File TRANS.FOR)
C=======================================================================

      SUBROUTINE SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, PORMIN, RLV,    !Input
     &    RWUMX, SOILPROP, SW, SWDELTS, SWDELTU,          !Input
     &    WEATHER, WINF, XHLAI,                           !Input
     &    FLOODWAT,                                       !Input/Output
     &    EO, EOP, ES, SRFTEMP, ST, SWDELTX, TRWUP)       !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types,
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE FloodModule

      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETW, ISWWAT
      CHARACTER*1  MEEVP, MEPHO
      CHARACTER*2  CROP

      INTEGER DYNAMIC, L, NLAYR
!      INTEGER RUN; REAL KCAN, KEP     !TEMP CHP

      REAL CANHT, CLOUDS, CO2, SRAD, TAVG, TDEW,
     &    TMAX, TMIN, WINDSP, XHLAI
      REAL CEF, CEO, CEP, CES, CET, EO, EP, ES, ET,
     &    SUMES1, SUMES2, T, TA, TGROAV,
     &    TRWU, TRWUP, U
      REAL EOS, EOP, WINF, SALB, ET_ALB
      REAL XLAT, TAV, TAMP, SRFTEMP
      REAL EORATIO, KSEVAP, KTRANS, SATFAC

      REAL DLAYR(NL), DUL(NL), LL(NL), RLV(NL), RWU(NL),
     &    SAT(NL), ST(NL), SW(NL), SWDELTS(NL),
     &    SWDELTU(NL), SWDELTX(NL), SW_AVAIL(NL)

!     Species-dependant variables imported from PLANT module:
      REAL PORMIN, RWUMX

!     Flood management variables:
      REAL EF, FLOOD

      REAL TGRO(TS)

!     Needed to send ET to OPSTRESS
      INTERFACE 
        SUBROUTINE OPSTRESS(C, I, E, P, W)
          USE ModuleDefs
          TYPE (ControlType), Intent(IN)           :: C
          CHARACTER*1,        Intent(IN), Optional :: I
          REAL,               Intent(IN), Optional :: E
          TYPE (PlStresType), Intent(IN), Optional :: P
          TYPE (WeatherType), Intent(IN), Optional :: W
        END SUBROUTINE OPSTRESS
      END INTERFACE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType) SOILPROP
      TYPE (SwitchType) ISWITCH
      TYPE (FloodWatType) FLOODWAT
      TYPE (WeatherType)  WEATHER

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC

      DLAYR  = SOILPROP % DLAYR
      DUL    = SOILPROP % DUL
      LL     = SOILPROP % LL
      NLAYR  = SOILPROP % NLAYR
      SALB   = SOILPROP % SALB
      SAT    = SOILPROP % SAT
      U      = SOILPROP % U

      ISWWAT = ISWITCH % ISWWAT
      IDETW  = ISWITCH % IDETW
      MEEVP  = ISWITCH % MEEVP
      MEPHO  = ISWITCH % MEPHO

      FLOOD  = FLOODWAT % FLOOD

      CLOUDS = WEATHER % CLOUDS
      CO2    = WEATHER % CO2
      SRAD   = WEATHER % SRAD  
      TA     = WEATHER % TA    
      TAMP   = WEATHER % TAMP  
      TAV    = WEATHER % TAV   
      TAVG   = WEATHER % TAVG  
      TDEW   = WEATHER % TDEW  
      TGRO   = WEATHER % TGRO   !I/O 
      TGROAV = WEATHER % TGROAV !I/O
      TMAX   = WEATHER % TMAX  
      TMIN   = WEATHER % TMIN  
      WINDSP = WEATHER % WINDSP
      XLAT   = WEATHER % XLAT  

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      IF (MEPHO .EQ. 'L' .OR. MEEVP .EQ. 'Z') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!      IF (ISWWAT .EQ. 'Y') THEN
        CALL STEMP(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output
!      ENDIF

!     ---------------------------------------------------------
      IF (ISWWAT .EQ. 'Y' .AND. MEEVP .NE. 'Z') THEN
        CALL ROOTWU(SEASINIT,
     &      DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,!Input
     &      RWU, SATFAC, TRWUP)                           !Output

!       Initialize soil evaporation and plant transpiration variables
        CALL SOILEV(SEASINIT,
     &    DLAYR, DUL, EOS, LL, SW, SW_AVAIL, U, WINF,     !Input
     &    ES, SUMES1, SUMES2, T)                          !Output

!       Initialize plant transpiration variables
        CALL TRANS(DYNAMIC, 
     &    CO2, CROP, EO, ES, KTRANS, TAVG, WINDSP, XHLAI, !Input
!    &    RUN, KCAN, KEP,     !TEMP CHP
     &    EOP)                                            !Output
      ENDIF

!     ---------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (MEPHO .EQ. 'L' .OR. MEEVP .EQ. 'Z') THEN
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF
      ENDIF

!     Call OPSPAM to open and write headers to output file
      IF (IDETW .EQ. 'Y') THEN
        CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT,
     &    CEP, CES, CET, EF, EO, EP, ES, ET, TMAX, TMIN, SRAD)    !, ST)
      ENDIF

      TRWU = 0.0

      EF   = 0.0
      EO   = 0.0
      EP   = 0.0
      ES   = 0.0
      ET   = 0.0

      CEF  = 0.0
      CEO  = 0.0
      CEP  = 0.0
      CES  = 0.0
      CET  = 0.0

      SWDELTX = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      CALL STEMP(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output

!-----------------------------------------------------------------------
!     POTENTIAL ROOT WATER UPTAKE
!-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN
!       Calculate the availability of soil water for use in SOILEV.
        DO L = 1, NLAYR
          SW_AVAIL(L) = MAX(0.0, SW(L) + SWDELTS(L) + SWDELTU(L))
        ENDDO

!       These processes are done by ETPHOT for hourly (Zonal) energy
!       balance method.
        IF (MEEVP .NE. 'Z') THEN
C       Calculate potential root water uptake rate for each soil layer
C       and total potential water uptake rate.
          IF (XHLAI .GT. 0.0) THEN
            CALL ROOTWU(RATE,
     &      DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,!Input
     &      RWU, SATFAC, TRWUP)                           !Output
          ELSE
            RWU   = 0.0
            TRWUP = 0.0
          ENDIF

!-----------------------------------------------------------------------
!         POTENTIAL EVAPOTRANSPIRATION
!-----------------------------------------------------------------------
          IF (FLOOD .GT. 0.0) THEN
            ! Set SALB to 0.08 under flooded conditions
            ! US - change to 0.05 Feb2004
            ET_ALB = 0.05
          ELSE
            ET_ALB = SALB
          ENDIF

          SELECT CASE (MEEVP)
!         ------------------------
          !FAO Penman-Monteith (FAO-56) potential evapotranspiration, 
!             with KC = 1.0
          CASE ('F')
            CALL PETPEN(
     &        CLOUDS, EORATIO, ET_ALB, SRAD, TAVG, TDEW,  !Input
     &        TMAX, TMIN, WINDSP, XHLAI,                  !Input
     &        EO)                                         !Output

!         ------------------------
          !Dynamic Penman-Monteith, pot. evapotranspiration, with
!             dynamic input of LAI, crop height effects on Ra and Rs
          CASE ('D')
            CALL PETDYN(
     &        CANHT, CLOUDS, ET_ALB, SRAD, TAVG, TDEW,    !Input
     &        TMAX, TMIN, WINDSP, XHLAI,                  !Input
     &        EO)                                         !Output
!         ------------------------
          !FAO Penman (FAO-24) potential evapotranspiration
          CASE ('P')
            CALL PETPNO(
     &        CLOUDS, ET_ALB, SRAD, TAVG, TDEW,           !Input
     &        TMAX, TMIN, WINDSP, XHLAI,                  !Input
     &        EO)                                         !Output
!         ------------------------
          !Penman - Meyer routine for estimation of Et in Southern NSW
          CASE ('M')
            CALL PETMEY(CONTROL, 
     &        TAVG, WINDSP, SRAD, TDEW, XHLAI, ET_ALB,    !Input
     &        EO)                                         !Output
!         ------------------------
          !Observed Potential ET from Weather file (Future)
          CASE ('O')
          !    EO = EOMEAS
!         ------------------------
          !Priestly-Taylor potential evapotranspiration
          CASE DEFAULT !Default - MEEVP = 'R' 
            CALL PETPT(
     &        ET_ALB, SRAD, TMAX, TMIN, XHLAI,          !Input
     &        EO)                                       !Output
!         ------------------------
          END SELECT

!-----------------------------------------------------------------------
!         POTENTIAL SOIL EVAPORATION
!-----------------------------------------------------------------------
          CALL PSE(
     &      EO, KSEVAP, XHLAI,                            !Input
     &      EOS)                                          !Output

C         Calculate actual soil (or flood) evaporation
          IF (FLOOD .GT. 0.0) THEN
            CALL FLOOD_EVAP(XHLAI, EO, EF)
            ES = 0.0
          ELSE
            CALL SOILEV(RATE,
     &      DLAYR, DUL, EOS, LL, SW, SW_AVAIL, U, WINF,   !Input
     &      ES, SUMES1, SUMES2, T)                        !Output
            EF = 0.0
          ENDIF

!-----------------------------------------------------------------------
!         ACTUAL TRANSPIRATION
!-----------------------------------------------------------------------
          IF (XHLAI .GT. 0.0) THEN
            IF (FLOOD .GT. 0.0) THEN
              !Use flood evaporation rate
              CALL TRANS (RATE, 
     &          CO2, CROP, EO, EF, KTRANS, TAVG, WINDSP, XHLAI, !Input
     &          EOP)                                            !Output
            ELSE
              !Use soil evaporation rate
              CALL TRANS(RATE, 
     &          CO2, CROP, EO, ES, KTRANS, TAVG, WINDSP, XHLAI, !Input
     &          EOP)                                            !Output
            ENDIF
          ELSE
            EOP = 0.0
          ENDIF

          IF (XHLAI .GT. 1.E-4 .AND. EOP .GT. 1.E-4) THEN
            !These calcs replace the old SWFACS subroutine
            !Stress factors now calculated as needed in PLANT routines.
            EP = MIN(EOP, TRWUP*10.)
          ELSE
            EP = 0.0
          ENDIF
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
!     ALTERNATE CALL TO ENERGY BALANCE ROUTINES
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (MEEVP .EQ. 'Z' .OR.
     &        (MEPHO .EQ. 'L' .AND. XHLAI .GT. 0.0)) THEN
          !ETPHOT called for photosynthesis only
          !    (MEPHO = 'L' and MEEVP <> 'Z')
          !or for both photosynthesis and evapotranspiration
          !   (MEPHO = 'L' and MEEVP = 'Z').
          CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
        ENDIF

!-----------------------------------------------------------------------
!       ACTUAL ROOT WATER EXTRACTION
!-----------------------------------------------------------------------
        IF (ISWWAT .EQ. 'Y') THEN
!         Adjust available soil water in layer 1.
          SW_AVAIL(1) = MAX(0.0, SW_AVAIL(1) - 0.1 * ES / DLAYR(1))

C         Calculate actual soil water uptake and transpiration rates
          CALL XTRACT(
     &      NLAYR, DLAYR, LL, SW, SW_AVAIL, TRWUP,        !Input
     &      EP, RWU,                                      !Input/Output
     &      SWDELTX, TRWU)                                !Output
        ENDIF   !ISWWAT = 'Y'
      ENDIF

!     Transfer computed value of potential floodwater evaporation to
!     flood variable.
      FLOODWAT % EF = EF

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN
!       Perform daily summation of water balance variables.
        ET  = ES  + EP + EF
        CEF = CEF + EF
        CEO = CEO + EO
        CEP = CEP + EP
        CES = CES + ES
        CET = CET + ET
      ENDIF

      IF (IDETW .EQ. 'Y') THEN
        CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT,
     &    CEP, CES, CET, EF, EO, EP, ES, ET, TMAX, TMIN, SRAD)    !, ST)
      ENDIF

!***********************************************************************
!***********************************************************************
!     OUTPUT - daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
!     Flood water evaporation can be modified by Paddy_Mgmt routine.
      EF = FLOODWAT % EF

!      IF (ISWWAT .EQ. 'Y') THEN
        CALL STEMP(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output
!      ENDIF

      CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT,
     &    CEP, CES, CET, EF, EO, EP, ES, ET, TMAX, TMIN, SRAD)    !, ST)

      IF (CROP .NE. 'FA' .AND. MEPHO .EQ. 'L') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

      CALL OPSTRESS(CONTROL, E=ET)

!     Transfer data to Water Balance routine
      CALL GetPutSPAM ("PUT", CEO, CEP, CES, EP)

!***********************************************************************
!***********************************************************************
!     FINAL - seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
      CALL OPSPAM(CONTROL, ISWITCH, FLOODWAT,
     &    CEP, CES, CET, EF, EO, EP, ES, ET, TMAX, TMIN, SRAD)    !, ST)

!      IF (ISWWAT .EQ. 'Y') THEN
        CALL STEMP(CONTROL, ISWITCH,
     &    SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
     &    SRFTEMP, ST)                                    !Output
!      ENDIF

      IF (MEPHO .EQ. 'L') THEN
        CALL ETPHOT(CONTROL, ISWITCH,
     &    PORMIN, RLV, RWUMX, SOILPROP, ST, SW,           !Input
     &    WEATHER, XHLAI,                                 !Input
     &    EOP, EP, ES, RWU, TRWUP)                        !Output
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      WEATHER % TGRO   = TGRO   !I/O 
      WEATHER % TGROAV = TGROAV !I/O

      RETURN
      END SUBROUTINE SPAM

!-----------------------------------------------------------------------
!     VARIABLE DEFINITIONS: (updated 12 Feb 2004)
!-----------------------------------------------------------------------
! CANHT       Canopy height (m)
! CEF         Cumulative seasonal evaporation from floodwater surface (mm)
! CEO         Cumulative potential evapotranspiration (mm)
! CEP         Cumulative transpiration (mm)
! CES         Cumulative evaporation (mm)
! CET         Cumulative evapotranspiration (mm)
! CLOUDS      Relative cloudiness factor (0-1) 
! CO2         Atmospheric carbon dioxide concentration
!              (µmol[CO2] / mol[air])
! CONTROL     Composite variable containing variables related to control 
!               and/or timing of simulation.    See Appendix A. 
! CROP        Crop identification code 
! DLAYR(L)    Thickness of soil layer L (cm)
! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil 
!               layer L (cm3[water]/cm3[soil])
! DYNAMIC     Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!               INTEGR, OUTPUT, or FINAL 
! EF          Evaporation rate from flood surface (mm / d)
! EO          Potential evapotranspiration rate (mm/d)
! EOP         Potential plant transpiration rate (mm/d)
! EORATIO     Ratio of increase in potential evapotranspiration with 
!               increase in LAI (up to LAI=6.0) for use with FAO-56 Penman 
!               reference potential evapotranspiration. 
! EOS         Potential rate of soil evaporation (mm/d)
! EP          Actual plant transpiration rate (mm/d)
! ES          Actual soil evaporation rate (mm/d)
! ET          Actual evapotranspiration rate (mm/d)
! FLOOD       Current depth of flooding (mm)
! FLOODWAT    Composite variable containing information related to bund 
!               management. Structure of variable is defined in 
!               ModuleDefs.for. 
! IDETW       Y=detailed water balance output, N=no detailed output 
! ISWITCH     Composite variable containing switches which control flow of 
!               execution for model.  The structure of the variable 
!               (SwitchType) is defined in ModuleDefs.for. 
! ISWWAT      Water simulation control switch (Y or N) 
! KSEVAP      Light extinction coefficient used for computation of soil 
!               evaporation 
! KTRANS      Light extinction coefficient used for computation of plant 
!               transpiration 
! LL(L)       Volumetric soil water content in soil layer L at lower limit
!              (cm3 [water] / cm3 [soil])
! MEEVP       Method of evapotranspiration ('P'=Penman, 
!               'R'=Priestly-Taylor, 'Z'=Zonal) 
! MEPHO       Method for photosynthesis computation ('C'=Canopy or daily, 
!               'L'=hedgerow or hourly) 
! NLAYR       Actual number of soil layers 
! PORMIN      Minimum pore space required for supplying oxygen to roots for 
!               optimal growth and function (cm3/cm3)
! RLV(L)      Root length density for soil layer L (cm[root] / cm3[soil])
! RWU(L)      Root water uptake from soil layer L (cm/d)
! RWUMX       Maximum water uptake per unit root length, constrained by 
!               soil water (cm3[water] / cm [root])
! SALB        Bare soil albedo (fraction)
! SAT(L)      Volumetric soil water content in layer L at saturation
!              (cm3 [water] / cm3 [soil])
! SATFAC      Root length weighted soil water excess stress factor ( 0 = no 
!               stress; 1 = saturated stress ) 
! SOILPROP    Composite variable containing soil properties including bulk 
!               density, drained upper limit, lower limit, pH, saturation 
!               water content.  Structure defined in ModuleDefs. 
! SRAD        Solar radiation (MJ/m2-d)
! SRFTEMP     Temperature of soil surface litter (°C)
! ST(L)       Soil temperature in soil layer L (°C)
! SUMES1      Cumulative soil evaporation in stage 1 (mm)
! SUMES2      Cumulative soil evaporation in stage 2 (mm)
! SW(L)       Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! SW_AVAIL(L) Soil water content in layer L available for evaporation, 
!               plant extraction, or movement through soil
!               (cm3 [water] / cm3 [soil])
! SWDELTS(L)  Change in soil water content due to drainage in layer L
!              (cm3 [water] / cm3 [soil])
! SWDELTU(L)  Change in soil water content due to evaporation and/or upward 
!               flow in layer L (cm3 [water] / cm3 [soil])
! SWDELTX(L)  Change in soil water content due to root water uptake in 
!               layer L (cm3 [water] / cm3 [soil])
! T           Number of days into Stage 2 evaporation (WATBAL); or time 
!               factor for hourly temperature calculations 
! TA          Daily normal temperature (°C)
! TAMP        Amplitude of temperature function used to calculate soil 
!               temperatures (°C)
! TAV         Average annual soil temperature, used with TAMP to calculate 
!               soil temperature. (°C)
! TAVG        Average daily temperature (°C)
! TDEW        Dew point temperature (°C)
! TGRO(I)     Hourly canopy temperature (°C)
! TGROAV      Average daily canopy temperature (°C)
! TMAX        Maximum daily temperature (°C)
! TMIN        Minimum daily temperature (°C)
! TRWU        Actual daily root water uptake over soil profile (cm/d)
! TRWUP       Potential daily root water uptake over soil profile (cm/d)
! U           Evaporation limit (cm)
! WINDSP      Wind speed at 2m (km/d)
! WINF        Water available for infiltration - rainfall minus runoff plus 
!               net irrigation (mm / d)
! XHLAI       Healthy leaf area index (m2[leaf] / m2[ground])
! XLAT        Latitude (deg.)
!-----------------------------------------------------------------------
!     END SUBROUTINE SPAM
!-----------------------------------------------------------------------

