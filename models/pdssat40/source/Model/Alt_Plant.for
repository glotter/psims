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
C  Alt_Plant, Subroutine
C
C  This routine calls plant growth routines for all non-cropgro crops.
C
C  These plant growth routines are handled differently by the main 
C     program than those of CROPGRO crops.  
C
C     Standardized output values are initialized by this routine and 
C     may or may not be computed by each individual plant growth module. 
C 
C     As additional crops are added, all will be called by this routine 
C     rather than by the main program.  
C-----------------------------------------------------------------------
C  Revision history
C
C  09/23/2001 CHP Written for modular pototo model to be incorporated 
C                   into CROPGRO. (SUBSTOR)
C  01/20/2002 WDB/CHP Added Ceres-Maize (MAIZE)
C  01/30/2002 LAH/CHP Added Wheat model (CSCERES)
C  04/17/2002 GH  Redefine module calls based on module name
C  07/15/2002 CHP Added rice module (US)
C  08/06/2002 WDB/CHP Added Ceres-Millet and Ceres-Sorghum
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  06/19/2003 CHP Added KTRANS (= KCAN + 0.15) for export to 
C                   transpiration routine.
C  07/08/2003 CHP Export KSEVAP for use in soil evaporation routine. 
C  10/08/2004 CHP Added GetPut_Iswitch call to push switch information
C                   into constructed variable which is accessible to
C                   all modules.
C  10/08/2004 CHP Removed some unused variables.
!  10/24/2005 CHP Put weather variables in constructed variable. 
C=======================================================================

      SUBROUTINE Alt_PLANT(CONTROL, ISWITCH, 
     &      EOP, FLOODWAT, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,                                 !Input
     &      FLOODN,                                       !I/O
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI)              !Output

C-----------------------------------------------------------------------
!     The following models are currently supported:
!         'MZCER' - CERES Maize
!         'RICER' - CERES Rice
!         'WHCER' - CERES Wheat
!         'PTSUB' - SUBSTOR Potato
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
! Each plant module must compute SATFAC, SWFAC, and TURFAC
C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE FloodModule    ! which contain control information, soil
                         ! parameters, hourly weather data and flooded
                         ! conditions.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  MEEVP, RNMODE
      CHARACTER*6  ERRKEY
      PARAMETER (ERRKEY = 'PLANTS')
      CHARACTER*8  MODEL
      CHARACTER*78 MESSAGE(10)    !Up to 10 lines of text to be output

      INTEGER DYNAMIC
      INTEGER RUN, NVALP0
      INTEGER YREND, YREMRG, MDATE, YRPLT
      INTEGER STGDOY(20)

      REAL CANHT, CO2, DAYL, EOP, EORATIO
      REAL KCAN, KEP, KSEVAP, KTRANS, NSTRES
      REAL PORMIN, RWUEP1, RWUMX, SRFTEMP
      REAL SNOW, TMAX, TMIN, TRWUP, TWILEN, XLAI, XHLAI

      REAL, DIMENSION(2)  :: HARVFRAC
      REAL, DIMENSION(NL) :: NH4, NO3, RLV
      REAL, DIMENSION(NL) :: ST, SW, UNO3, UNH4

      LOGICAL FixCanht
c-----------------------------------------------------------------------
C         Variables needed to run ceres maize.....W.D.B. 12-20-01
      CHARACTER*2 CROP 
      REAL    SRAD

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType)  CONTROL
      TYPE (SwitchType)   ISWITCH
      TYPE (SoilType)     SOILPROP
      TYPE (ResidueType)  HARVRES
      TYPE (ResidueType)  SENESCE
      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType)   FLOODN
      TYPE (WeatherType)  WEATHER

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      MODEL   = CONTROL % MODEL
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

      MEEVP  = ISWITCH % MEEVP

      CO2    = WEATHER % CO2   
      DAYL   = WEATHER % DAYL  
      SRAD   = WEATHER % SRAD  
      TMAX   = WEATHER % TMAX  
      TMIN   = WEATHER % TMIN  
      TWILEN = WEATHER % TWILEN

!***********************************************************************
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Alt-Plant crops can not use MEPHO = 'L' or MEEVP = 'Z' at
!       this time.  If Species files are modified for these options
!       in the future, we need to make this check on a crop by crop basis.
!     The plant routines do not use these codes, but the SPAM module
!       does and it will bomb when species parameters are not found.
      IF (ISWITCH % MEPHO .EQ. 'L') THEN
        ISWITCH % MEPHO = 'C'
!       Put ISWITCH data where it can be retreived 
!         by other modules as needed.
        CALL GETPUT_ISWITCH('PUT',ISWITCH)

!       Write message to WARNING.OUT file
        WRITE(MESSAGE(1),110) 
        WRITE(MESSAGE(2),120) CROP
        WRITE(MESSAGE(3),130) 
        CALL WARNING(3, ERRKEY, MESSAGE)
      ENDIF

  110 FORMAT('You have specified use of the Leaf-level photosynthesis')
  120 FORMAT('option, which is not available for crop ', A2, '.')
  130 FORMAT('Canopy photosynthesis option will be used.')

      IF (ISWITCH % MEEVP .EQ. 'Z') THEN
!       Default to Priestly-Taylor potential evapotranspiration
        ISWITCH % MEEVP = 'R'
!       Put ISWITCH data where it can be retreived 
!         by other modules as needed.
        CALL GETPUT_ISWITCH('PUT',ISWITCH)

!       Write message to WARNING.OUT file
        WRITE(MESSAGE(1),210) 
        WRITE(MESSAGE(2),220) CROP
        WRITE(MESSAGE(3),230) 
        WRITE(MESSAGE(4),240) 
        CALL WARNING(4, ERRKEY, MESSAGE)
      ENDIF
      
  210 FORMAT('You have specified use of the Zonal evapotranspiration')
  220 FORMAT('option, which is not available for crop ', A2, '.')
  230 FORMAT('The Priestly-Taylor potential evapo-transpiration ')
  240 FORMAT('option will be used.')

!---------------------------------------------------------------------
!     Print warning if "dynamic ET" routine is used - need canopy height
!     Need to modify this crop code list as canopy height routines are added.
      IF (MEEVP .EQ. 'D' .AND. INDEX('RIWHMLMZSGPTBA',CROP) .GT. 0) THEN
        WRITE(MESSAGE(1),310) 
        WRITE(MESSAGE(2),320) 
        WRITE(MESSAGE(3),330) CROP
        WRITE(MESSAGE(4),340) 
        CALL WARNING(4, ERRKEY, MESSAGE)
        !Trigger to set canopy height upon emergence
        FixCanht = .TRUE.
      ENDIF

  310 FORMAT('You are using the Penman-Monteith potential evapo-')
  320 FORMAT('transpiration method, which requires canopy height.')
  330 FORMAT('The ',A2,' crop routine does not calculate this ')
  340 FORMAT('value.  A default value will be used.')

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Initialize output variables.
!     Each plant routine may or may not re-compute these values.
      CANHT    = 0.0
      EORATIO  = 1.0
      KCAN     = 0.85
      KEP      = 1.0
      KSEVAP   = -99.
      KTRANS   = 1.0
      MDATE    = -99
      NSTRES   = 1.0
      NVALP0   = 10000
      PORMIN   = 0.02
      RLV      = 0.0
      RWUEP1   = 1.5
      RWUMX    = 0.03
      SENESCE % ResWt  = 0.0
      SENESCE % ResLig = 0.0
      SENESCE % ResE   = 0.0
      STGDOY   = 9999999
      UNH4     = 0.0
      UNO3     = 0.0
      XHLAI    = 0.0
      XLAI     = 0.0
      YREMRG   = -99

!     If this is not a sequenced run, don't use any previously calculated
!       harvest residue.
!     This should be done by each plant routine, but in case not:
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        HARVRES % RESWT  = 0.0
        HARVRES % RESLIG = 0.0
        HARVRES % RESE   = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0

!-----------------------------------------------------------------------
      ENDIF   !End of dynamic loop prior to calls to crop models
!-----------------------------------------------------------------------


!***********************************************************************
!***********************************************************************
!     Call crop models for all values of DYNAMIC:
!-----------------------------------------------------------------------
!     Potato 
      IF (INDEX(MODEL,'PTSUB') > 0) THEN 
        CALL SUBSTOR(CONTROL, ISWITCH, 
     &    CO2, EOP, HARVFRAC, NH4, NO3, SOILPROP, SRAD,   !Input
     &    ST, SW, TMAX, TMIN, TRWUP, TWILEN, YREND, YRPLT,!Input
     &    CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV,     !Output
     &    RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)       !Output

        IF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     Maize 
      ELSEIF (INDEX(MODEL,'MZCER') .GT. 0) THEN
        CALL MZ_CERES (CONTROL, ISWITCH,                  !Input
     &    CO2, DAYL, EOP, HARVFRAC, NH4, NO3, SNOW,       !Input
     &    SOILPROP, SRAD, SW, TMAX, TMIN, TRWUP,          !Input
     &    TWILEN, YREND, YRPLT,                           !Input
     $    CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,       !Output
     &    PORMIN, RLV, RWUMX,SENESCE, STGDOY, UNH4,       !Output
     &    UNO3, XLAI)                                     !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
!          KTRANS = KCAN + 0.15        !Or use KEP here??
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP        
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     Sorghum 
      ELSEIF (INDEX(MODEL,'SGCER') .GT. 0) THEN
        CALL SG_CERES (CONTROL, ISWITCH, 
     &     CO2, DAYL, EOP, HARVFRAC, NH4, NO3,            !Input
     &     SNOW, SOILPROP, SRAD, SW, TMAX, TMIN,          !Input
     &     TRWUP, TWILEN, YREND, YRPLT,                   !Input
     $     CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV,    !Output
     &     RWUMX, SENESCE, STGDOY, UNO3, UNH4, XLAI,      !Output
     &     KCAN, KEP)                                     !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
!          KTRANS = KCAN + 0.15        !Or use KEP here??
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     Millet 
      ELSEIF (INDEX(MODEL,'MLCER') .GT. 0) THEN
        CALL ML_CERES (CONTROL, ISWITCH, 
     &     CO2, DAYL, EOP, HARVFRAC, NH4, NO3,            !Input
     &     SNOW, SOILPROP, SRAD, SW, TMAX, TMIN,          !Input
     &     TRWUP, TWILEN, YREND, YRPLT,                   !Input
     $     CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV,    !Output
     &     RWUMX, SENESCE, STGDOY, UNO3, UNH4, XLAI,      !Output
     &     KCAN, KEP)                                     !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
!          KTRANS = KCAN + 0.15        !Or use KEP here??
          KTRANS = KEP        !KJB/WDB/CHP 10/22/2003
          KSEVAP = KEP
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     Wheat and Barley 
      ELSEIF (INDEX(MODEL,'WHCER') .GT. 0) THEN
        CALL CSCERES_Interface (CONTROL, ISWITCH,          !Input
     &     CO2, DAYL, EOP, YREND, NH4, NO3, SNOW, SOILPROP,!Input
     &     SRAD, SRFTEMP, ST, SW, TMAX, TMIN, TRWUP, TWILEN,!Input
     &     CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,PORMIN,!Output
     &     RLV, RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)  !Output

        IF (DYNAMIC .EQ. SEASINIT) THEN
          KTRANS = KEP
          KSEVAP = KEP
        ELSEIF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

!     Rice 
      ELSEIF (INDEX(MODEL,'RICER') .GT. 0) THEN
        CALL RICE(CONTROL, ISWITCH,
     &    CO2, DAYL, EOP, FLOODWAT, HARVFRAC, NH4, NO3,   !Input
     &    SOILPROP, SRAD, ST, SW, TMAX, TMIN, TRWUP,      !Input
     &    TWILEN, YRPLT,                                  !Input
     &    FLOODN,                                         !I/O
     &    CANHT, HARVRES, XLAI, MDATE, NSTRES, PORMIN,    !Output
     &    RWUEP1, RWUMX, RLV, SENESCE, STGDOY, UNH4, UNO3)!Output

        IF (DYNAMIC .EQ. INTEGR) THEN
          XHLAI = XLAI
        ENDIF

      ENDIF

!***********************************************************************
!***********************************************************************
!     Processing after calls to crop models:
!-----------------------------------------------------------------------
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
! Zero the value of HARVRES composite variable here 
!!!NOTE: At this time, the variable has already been used to 
!     initialize soil properties for this season.
!  This should be done by each plant routine, but in case not:
        HARVRES % RESWT  = 0.0
        HARVRES % RESLIG = 0.0
        HARVRES % RESE   = 0.0

!***********************************************************************
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     Set default canopy height upon emergence (or first day with 
!       LAI.  Should actually set these defaults within each
!       crop routine.
        IF (FixCanht .AND. (XLAI .GT. 0.001 .OR. XHLAI .GT. 0.001)) THEN
          CANHT = 0.5   
          FixCanht = .FALSE.
        ENDIF

!-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------

!***********************************************************************
      RETURN
      END SUBROUTINE Alt_Plant

!===========================================================================
! Variable listing for Alt_Plant - updated 08/18/2003
! --------------------------------------------------------------------------
! CANHT     Canopy height (m)
! CO2       Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
! CONTROL   Composite variable containing variables related to control 
!             and/or timing of simulation.  The structure of the variable 
!             (ControlType) is defined in ModuleDefs.for. 
! CROP      Crop identification code 
! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or FINAL 
! EOP       Potential plant transpiration rate (mm/d)
! EORATIO   Ratio of increase in EO with increase in LAI (up to LAI=6.0) 
!             for use with FAO-56 Penman reference EO. 
! ERRKEY    Subroutine name for error file 
! FIXCANHT  Logical variable, =TRUE if default canopy height is to be set 
!             by Alt_Plant routine upon emergence 
! FLOODN    Composite variable which contains flood nitrogen mass and 
!             concentrations. Structure of variable is defined in 
!             ModuleDefs.for. (var.)
! FLOODWAT  Composite variable containing information related to bund 
!             management. Structure of variable is defined in ModuleDefs.for. 
! HARVFRAC  Two-element array containing fractions of (1) yield harvested 
!             and (2) by-product harvested (fraction)
! HARVRES   Composite variable containing harvest residue amounts for total 
!             dry matter, lignin, and N amounts.  Structure of variable is 
!             defined in ModuleDefs.for. 
! ISWITCH   Composite variable containing switches which control flow of 
!             execution for model.  The structure of the variable 
!             (SwitchType) is defined in ModuleDefs.for. 
! KCAN      Canopy light extinction coefficient for daily PAR, for 
!             equidistant plant spacing, modified when in-row and between 
!             row spacing are not equal 
! KEP       Energy extinction coefficient for partitioning EO to EP 
! KSEVAP    Light extinction coefficient used for computation of soil 
!             evaporation 
! KTRANS    Light extinction coefficient used for computation of plant 
!             transpiration 
! MDATE     Harvest maturity date (YYYYDDD)
! MEEVP     Method of evapotranspiration ('P'=Penman, 'R'=Priestly-Taylor, 
!             'Z'=Zonal) 
! MESSAGE   Text array containing information to be written to WARNING.OUT 
!             file. 
! MODEL     Name of CROPGRO executable file 
! NH4(L)    Ammonium N in soil layer L (µg[N] / g[soil])
! NL        Maximum number of soil layers = 20 
! NO3(L)    Nitrate in soil layer L (µg[N] / g[soil])
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! NVALP0    Set to 100,000 in PHENOLOG, used for comparison of times of 
!             plant stages (d)
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RUN       Change in date between two observations for linear 
!             interpolation 
! RWUEP1    Threshold for reducing leaf expansion compared w/ ratio of 
!             TRWU/EP1 (total potential daily root water uptake/ actual 
!             transpiration) 
! RWUMX     Maximum water uptake per unit root length, constrained by soil 
!             water (cm3[water] / cm [root])
! SENESCE   Composite variable containing data about daily senesced plant 
!             matter. Structure of variable is defined in ModuleDefs.for 
! SNOW      Snow accumulation (mm)
! SOILPROP  Composite variable containing soil properties including bulk 
!             density, drained upper limit, lower limit, pH, saturation 
!             water content.  Structure defined in ModuleDefs. 
! SRAD      Solar radiation (MJ/m2-d)
! ST(L)     Soil temperature in soil layer L (°C)
! STGDOY(I) Day when plant stage I occurred (YYYYDDD)
! SW(L)     Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! TMAX      Maximum daily temperature (°C)
! TMIN      Minimum daily temperature (°C)
! TRWUP     Potential daily root water uptake over soil profile (cm/d)
! TWILEN    Daylength from twilight to twilight (h)
! UNH4(L)   Rate of root uptake of NH4, computed in NUPTAK
!            (kg [N] / ha - d)
! UNO3(L)   Rate of root uptake of NO3, computed in NUPTAK (kg [N] / ha -d)
! XHLAI     Healthy leaf area index (m2[leaf] / m2[ground])
! XLAI      Leaf area (one side) per unit of ground area
!            (m2[leaf] / m2[ground])
! YREMRG    Day of emergence (YYYYDDD)
! YREND     Date for end of season (usually harvest date) (YYYYDDD)
! YRPLT     Planting date (YYYYDDD)
!===========================================================================
