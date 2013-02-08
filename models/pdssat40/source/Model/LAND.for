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
C  LAND UNIT Module. G.Hoogenboom, J.W.Jones, C.Porter
C-----------------------------------------------------------------------
C  Land Unit Module.  Provides the interface between soil, weather
C  and crops.  Based on the original CROPGRO routine
C=======================================================================
C  REVISION       HISTORY
C  12/01/2001 CHP Written.
C  12/12/2001 GH  Rename to Land
!  10/24/2005 CHP Put weather variables in constructed variable. 
C=======================================================================     
C-----------------------------------------------------------------------
C    CROP should be modified when model versions change
C     or when a crop specific model is created.
C
C     CROP   = CR Generic CRopgro model     Version 3.90 (2002)
C     CROP   = BN for CROPGRO - DRY BEAN    Version 3.9  (2002)
C     CROP   = PN for CROPGRO - PEANUT      Version 3.9  (2002)
C     CROP   = SB for CROPGRO - SOYBEAN     Version 3.9  (2002)
C     CROP   = FA for CROPGRO - FALLOW      Version 3.9  (2002)
C     CROP   = TM for CROPGRO - TOMATO      Version 3.9  (2002)
C     CROP   = PR for CROPGRO - PEPPER      Version 3.9  (2002)
C     CROP   = PE for CROPGRO - PEA         Version 3.9  (2002)
C     CROP   = CH for CROPGRO - CHICKPEA    Version 3.9  (2002)
C     CROP   = PP for CROPGRO - PIGEONPEA   Version 3.9  (2002)
C     CROP   = C3 for CROPGRO - C4 CROPS    Version 3.9  (2002)
C     CROP   = C4 for CROPGRO - C3 CROPS    Version 3.9  (2002)
C     CROP   = G0 for CROPGRO - GRASS-0     Version 3.9  (2002)
C     CROP   = G1 for CROPGRO - GRASS-1     Version 3.9  (2002)
C     CROP   = G2 for CROPGRO - GRASS-2     Version 3.9  (2002)
C     CROP   = G3 for CROPGRO - GRASS-3     Version 3.9  (2002)
C     CROP   = G4 for CROPGRO - GRASS-4     Version 3.9  (2002)
C     CROP   = G5 for CROPGRO - GRASS-5     Version 3.9  (2002)
C     CROP   = G6 for CROPGRO - GRASS-6     Version 3.9  (2002)
C     CROP   = G7 for CROPGRO - GRASS-7     Version 3.9  (2002)
C     CROP   = G8 for CROPGRO - GRASS-8     Version 3.9  (2002)
C     CROP   = BR for CROPGRO - Brachiaria
C                               decumbens   Version 3.9  (2002)
C     CROP   = VB for CROPGRO - VELVETBEAN  Version 3.9  (2002)
C     CROP   = FB for CROPGRO - FABA BEAN   Version 3.9  (2002)
C
C-GH  Need to add other DSSAT-CSM crops

C     CROP   = BA for CSCERES - BARLEY      Version 3.9  (2002)
C     CROP   = CB for CROPGRO - CABBAGE     Version 3.9  (2002)
C     CROP   = CS for CSSIM   - CASSAVA     Version 3.9  (2002)
C     CROP   = CO for CROPGRO - COTTON      Version 3.9  (2002)
C     CROP   = CP for CROPGRO - COWPEA      Version 3.9  (2002)
C     CROP   = CT for CROPGRO - CITRUS      Version 3.9  (2002)
C     CROP   = MZ for GECER   - MAIZE       Version 3.9  (2002)
C     CROP   = ML for GECER   - MILLET      Version 3.9  (2002)
C     CROP   = PI for PIAL    - PINEAPPLE   Version 3.9  (2002)
C     CROP   = PT for SUBSTOR - POTATO      Version 3.9  (2002)
C     CROP   = RI for RICER   - RICE        Version 3.9  (2002)
C     CROP   = SC for SCCAN   - SUGARCANE   Version 3.9  (2002)
C     CROP   = SG for GECER   - SORGHUM     Version 3.9  (2002)
C     CROP   = SU for SUOIL   - SUNFLOWER   Version 3.9  (2002)
C     CROP   = TM for CROPGRO - TOMATO      Version 3.9  (2002)
C     CROP   = TN for ARGRO   - TANIER      Version 3.9  (2002)
C     CROP   = TR for ARGRO   - TARO        Version 3.9  (2002)
C     CROP   = WH for CSCERES - WHEAT       Version 3.9  (2002)
C=======================================================================
      SUBROUTINE LAND(CONTROL, ISWITCH, 
     &                YRPLT, MDATE, YREND)
      
C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE FloodModule    ! which contain control information, soil
                         ! parameters, hourly weather data and flooded
                         ! conditions.

      IMPLICIT NONE
      SAVE
C-----------------------------------------------------------------------
C     Crop, Experiment, Command line Variables
C-----------------------------------------------------------------------
      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY
      PARAMETER   (ERRKEY = 'LAND  ')
      CHARACTER*8  MODEL
      CHARACTER*30 FILEIO
      
C-----------------------------------------------------------------------
C     Date / Timing / Sequencing Variables
C-----------------------------------------------------------------------
      INTEGER      DYNAMIC, RUN, YRSIM, YRDOY

C-----------------------------------------------------------------------
C     Input and Output Handling
C-----------------------------------------------------------------------
      CHARACTER*1  IDETS, IPLTI

C-----------------------------------------------------------------------
C     Weather module Variables
C-----------------------------------------------------------------------
      TYPE (WeatherType)  WEATHER

C-----------------------------------------------------------------------
C     Soil Processes Module Variables 
C-----------------------------------------------------------------------
      REAL         SNOW, WINF
      REAL         NH4(NL), NO3(NL), ST(NL)
      REAL         SW(NL), SWDELTS(NL), SWDELTU(NL)
      TYPE (SoilType) SOILPROP    !type defined in ModuleDefs

C-----------------------------------------------------------------------
C     Soil - Plant - Atmosphere Module Variables
C-----------------------------------------------------------------------
      REAL         EO, EOP, ES, SRFTEMP, TRWUP
      REAL         SWDELTX(NL)

C-----------------------------------------------------------------------
C     PLANT Module Variables
C-----------------------------------------------------------------------
      INTEGER      MDATE
      INTEGER      STGDOY(20)
      REAL         CANHT, EORATIO, NSTRES, PORMIN, RWUMX
      REAL         XHLAI, XLAI
      REAL         KSEVAP, KTRANS
      REAL         UNH4(NL), UNO3(NL), RLV(NL)
      Type (ResidueType) HARVRES  !type defined in ModuleDefs
      Type (ResidueType) SENESCE  
      
C-----------------------------------------------------------------------
C     Operations Management Module Variables 
C-----------------------------------------------------------------------
      INTEGER     YREND, YRDIF, YRPLT
      REAL        IRRAMT
      REAL, DIMENSION(2) :: HARVFRAC   !Harvest & byproduct fractions

C-----------------------------------------------------------------------
!!     Temporary timer function
!!     Date / time variables
!      INTEGER DATE_TIME(8)
!!      date_time(1)  The 4-digit year  
!!      date_time(2)  The month of the year  
!!      date_time(3)  The day of the month  
!!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  
!      REAL TIME0, TIME1, DELTA_TIME
C-----------------------------------------------------------------------

C     Define constructed variable types based on definitions in
C     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     The variable "FLOODWAT" is of type "FloodWatType".
      TYPE (FloodWatType) FLOODWAT
      
!     The variable "FloodN" is of type "FloodNType".
      TYPE (FloodNType) FloodN

C     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      IPLTI  = ISWITCH % IPLTI

      YRDIF = 0
      
C***********************************************************************
C***********************************************************************
C     Run Initialization - Called once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!!     Temporary timer function
!      !Get initial time
!      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!!     Convert time to seconds
!      TIME0 = DATE_TIME(7) 
!     &      + DATE_TIME(8) / 1000.  
!     &      + DATE_TIME(6) * 60.  
!     &      + DATE_TIME(5) * 3600.

C-----------------------------------------------------------------------
C     Read switches from FILEIO
C-----------------------------------------------------------------------
      CALL IPIBS (CONTROL, ISWITCH,                       
     &            CROP, IDETS, MODEL)                     !Output

C-----------------------------------------------------------------------
C     Read input parameters for weather routines
C-----------------------------------------------------------------------
      CALL WEATHR(CONTROL, ISWITCH, WEATHER)

C-----------------------------------------------------------------------
C     Read initial soil data 
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    EO, ES, FLOODWAT, HARVRES, IRRAMT, NSTRES,      !Input
     &    SENESCE, SRFTEMP, ST, SWDELTX, UNH4, UNO3,      !Input
     &    WEATHER, XHLAI, YREND, YRPLT,                   !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, SNOW, SOILPROP, SW, SWDELTS,          !Output
     &    SWDELTU, WINF)                                  !Output

C-----------------------------------------------------------------------
C     Read initial management operations data 
C-----------------------------------------------------------------------
!      CALL MGMTOPS(CONTROL, ISWITCH, 
!     &    SOILPROP, ST, STGDOY, SW,                       !Input
!     &    YREND, HBPC, HPC, IRRAMT, YRDIF, MDATE, YRPLT)  !Output

C-----------------------------------------------------------------------
C     Read initial soil-plant-atmosphere data
C-----------------------------------------------------------------------
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, PORMIN, RLV,    !Input
     &    RWUMX, SOILPROP, SW, SWDELTS, SWDELTU,          !Input
     &    WEATHER, WINF, XHLAI,                           !Input
     &    FLOODWAT,                                       !Input/Output
     &    EO, EOP, ES, SRFTEMP, ST, SWDELTX, TRWUP)       !Output

C-----------------------------------------------------------------------
C     Read initial plant module data
C-----------------------------------------------------------------------
!      IF (CROP .EQ. 'FA') THEN
!        RETURN

      IF (INDEX('GROgro', MODEL(3:5)) .GT. 0) THEN 
!      ELSEIF (INDEX('GROgro', MODEL(3:5)) .GT. 0) THEN 
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI)                !Output

      ELSE
        CALL Alt_PLANT(CONTROL, ISWITCH,  
     &      EOP, FLOODWAT, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,                                 !Input
     &      FLOODN,                                       !I/O
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI)              !Output
      ENDIF
C-----------------------------------------------------------------------
C     Initialize summary.out information
C-----------------------------------------------------------------------
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
        CALL OPSUM (CONTROL, ISWITCH, YRPLT)
!      ENDIF

C*********************************************************************** 
C*********************************************************************** 
C     SEASONAL INITIALIZATION
C*********************************************************************** 
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Set planting date, adjust operations dates for seasonal or 
C     sequenced runs.
C-----------------------------------------------------------------------
      CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, SOILPROP, ST, STGDOY, SW, WEATHER,    !Input
     &    YREND, HARVFRAC, IRRAMT, MDATE, YRPLT)          !Output

C-----------------------------------------------------------------------
      IF (YRPLT .LT. YRSIM .AND. CROP .NE. 'FA' .AND. IPLTI .NE. 'A')
     &    THEN
         CALL ERROR(ERRKEY,2,' ',0)
      ENDIF

C-----------------------------------------------------------------------
C     Call WEATHR for initialization - reads first day of weather
C     data for use in soil N and soil temp initialization.
C-----------------------------------------------------------------------
      CALL WEATHR(CONTROL, ISWITCH, WEATHER)

C-----------------------------------------------------------------------
C     Seasonal initialization for soil-plant-atmosphere processes
!     chp moved this before SOIL, so soil temp is available 
C-----------------------------------------------------------------------
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, PORMIN, RLV,    !Input
     &    RWUMX, SOILPROP, SW, SWDELTS, SWDELTU,          !Input
     &    WEATHER, WINF, XHLAI,                           !Input
     &    FLOODWAT,                                       !Input/Output
     &    EO, EOP, ES, SRFTEMP, ST, SWDELTX, TRWUP)       !Output

C-----------------------------------------------------------------------
C     Seasonal initialization for soil processes
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    EO, ES, FLOODWAT, HARVRES, IRRAMT, NSTRES,      !Input
     &    SENESCE, SRFTEMP, ST, SWDELTX, UNH4, UNO3,      !Input
     &    WEATHER, XHLAI, YREND, YRPLT,                   !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, SNOW, SOILPROP, SW, SWDELTS,          !Output
     &    SWDELTU, WINF)                                  !Output

C-----------------------------------------------------------------------
C     Initialize PLANT routines (including phenology and pest)
C-----------------------------------------------------------------------
      IF (INDEX('GROgro', MODEL(3:5)) .GT. 0) THEN 
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI)                !Output

      ELSE
        CALL Alt_PLANT(CONTROL, ISWITCH,  
     &      EOP, FLOODWAT, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,                                 !Input
     &      FLOODN,                                       !I/O
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI)              !Output
      ENDIF

C-----------------------------------------------------------------------
C     Initialize summary output file - possible output from 
C     various modules.
C-----------------------------------------------------------------------
      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
        CALL OPSUM (CONTROL, ISWITCH, YRPLT)
      ENDIF

C***********************************************************************
C***********************************************************************
C     DAILY RATE CALCULATIONS
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C     Call WEATHER Subroutine to input weather data and to
C     calculate hourly radiation and air temperature values
C     Note: First day of weather has already been read by 
C       initialization call to WEATHR.
C-----------------------------------------------------------------------
      CALL WEATHR(CONTROL, ISWITCH, WEATHER)

C-----------------------------------------------------------------------
C     Call Operations Management module to determine today's 
C     applications of irrigation, tillage, etc.
C-----------------------------------------------------------------------
      CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, SOILPROP, ST, STGDOY, SW, WEATHER,    !Input
     &    YREND, HARVFRAC, IRRAMT, MDATE, YRPLT)          !Output

C-----------------------------------------------------------------------
C     Call Soil processes module to determine today's rates of 
C     change of soil properties.
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    EO, ES, FLOODWAT, HARVRES, IRRAMT, NSTRES,      !Input
     &    SENESCE, SRFTEMP, ST, SWDELTX, UNH4, UNO3,      !Input
     &    WEATHER, XHLAI, YREND, YRPLT,                   !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, SNOW, SOILPROP, SW, SWDELTS,          !Output
     &    SWDELTU, WINF)                                  !Output

C-----------------------------------------------------------------------
C     Call Soil-plant-atmosphere module to determine today's
C     rates of evapotranspiration.
C-----------------------------------------------------------------------
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, PORMIN, RLV,    !Input
     &    RWUMX, SOILPROP, SW, SWDELTS, SWDELTU,          !Input
     &    WEATHER, WINF, XHLAI,                           !Input
     &    FLOODWAT,                                       !Input/Output
     &    EO, EOP, ES, SRFTEMP, ST, SWDELTX, TRWUP)       !Output

C-----------------------------------------------------------------------
C     Call PLANT Subroutine to calculate crop growth and
C     development rates.
C     Skip plant growth and development routines for fallow runs
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. 
     &    YRDOY .GE. YRPLT .AND. YRPLT .NE. -99) THEN
        IF (INDEX('GROgro', MODEL(3:5)) .GT. 0) THEN 
        CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI)                !Output

        ELSE
          CALL Alt_PLANT(CONTROL, ISWITCH,  
     &      EOP, FLOODWAT, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,                                 !Input
     &      FLOODN,                                       !I/O
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI)              !Output
        ENDIF
      ENDIF

C***********************************************************************
C     DAILY INTEGRATION 
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR) THEN
C***********************************************************************
C     Integrate soil state variables
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    EO, ES, FLOODWAT, HARVRES, IRRAMT, NSTRES,      !Input
     &    SENESCE, SRFTEMP, ST, SWDELTX, UNH4, UNO3,      !Input
     &    WEATHER, XHLAI, YREND, YRPLT,                   !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, SNOW, SOILPROP, SW, SWDELTS,          !Output
     &    SWDELTU, WINF)                                  !Output

C-----------------------------------------------------------------------
C     Compute cumulative totals for soil-plant-atmosphere processes
C-----------------------------------------------------------------------
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, PORMIN, RLV,    !Input
     &    RWUMX, SOILPROP, SW, SWDELTS, SWDELTU,          !Input
     &    WEATHER, WINF, XHLAI,                           !Input
     &    FLOODWAT,                                       !Input/Output
     &    EO, EOP, ES, SRFTEMP, ST, SWDELTX, TRWUP)       !Output

C-----------------------------------------------------------------------
C     Call Plant module to integrate daily plant processes and update
C     plant state variables.
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA' .AND. 
     &        YRDOY .GE. YRPLT .AND. YRPLT .NE. -99) THEN
        IF (INDEX('GROgro', MODEL(3:5)) .GT. 0) THEN 
          CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI)                !Output

        ELSE
          CALL Alt_PLANT(CONTROL, ISWITCH,  
     &      EOP, FLOODWAT, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,                                 !Input
     &      FLOODN,                                       !I/O
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI)              !Output
        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Call Operations Management module to check for harvest end, 
C     accumulate variables.
C-----------------------------------------------------------------------
      CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, SOILPROP, ST, STGDOY, SW, WEATHER,    !Input
     &    YREND, HARVFRAC, IRRAMT, MDATE, YRPLT)          !Output

C***********************************************************************
C***********************************************************************
C     Daily Output
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN

      !IF (YRDOY > 2006264 .AND. YRDOY < 2007169) THEN
      !  WRITE(*,*) YRDOY
      !ENDIF

C     Call WEATHER module for daily output
      CALL WEATHR(CONTROL, ISWITCH, WEATHER)

C-----------------------------------------------------------------------
C     Call soil processes module for daily printout.
C-----------------------------------------------------------------------
        CALL SOIL(CONTROL, ISWITCH, 
     &    EO, ES, FLOODWAT, HARVRES, IRRAMT, NSTRES,      !Input
     &    SENESCE, SRFTEMP, ST, SWDELTX, UNH4, UNO3,      !Input
     &    WEATHER, XHLAI, YREND, YRPLT,                   !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, SNOW, SOILPROP, SW, SWDELTS,          !Output
     &    SWDELTU, WINF)                                  !Output

C-----------------------------------------------------------------------
C     Call soil-plant-atmosphere module for daily printout.
C-----------------------------------------------------------------------
        CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, PORMIN, RLV,    !Input
     &    RWUMX, SOILPROP, SW, SWDELTS, SWDELTU,          !Input
     &    WEATHER, WINF, XHLAI,                           !Input
     &    FLOODWAT,                                       !Input/Output
     &    EO, EOP, ES, SRFTEMP, ST, SWDELTX, TRWUP)       !Output

C-----------------------------------------------------------------------
C     Call plant module for daily printout.
C-----------------------------------------------------------------------
!      IF (YRDOY .GE. YRPLT .AND. YRPLT .NE. -99) THEN
        IF (CROP .EQ. 'FA') THEN
!          CALL YR_DOY(YRDOY, YEAR, DOY)
!          IF (DOY .EQ. 1 .OR. MDATE .EQ. YRDOY) THEN
!            WRITE(*,20)YEAR, MDATE
!   20       FORMAT(' YEAR = ',I5, 3X, ' MDATE = ',I5)
!          ENDIF
          RETURN

        ELSEIF (INDEX('GROgro', MODEL(3:5)) .GT. 0) THEN 
          CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI)                !Output

        ELSE
          CALL Alt_PLANT(CONTROL, ISWITCH,  
     &      EOP, FLOODWAT, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,                                 !Input
     &      FLOODN,                                       !I/O
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI)              !Output
        ENDIF
!      ENDIF
C-----------------------------------------------------------------------
C       Call management operations module for daily printout.
C-----------------------------------------------------------------------
        CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, SOILPROP, ST, STGDOY, SW, WEATHER,    !Input
     &    YREND, HARVFRAC, IRRAMT, MDATE, YRPLT)          !Output

C*********************************************************************** 
C***********************************************************************
C     Seasonal Output
C*********************************************************************** 
      ELSE IF (DYNAMIC .EQ. FINAL) THEN

C     Call WEATHER module to close current weather file 
      CALL WEATHR(CONTROL, ISWITCH, WEATHER)

C     Print seasonal summaries and close files.
      CALL SOIL(CONTROL, ISWITCH, 
     &    EO, ES, FLOODWAT, HARVRES, IRRAMT, NSTRES,      !Input
     &    SENESCE, SRFTEMP, ST, SWDELTX, UNH4, UNO3,      !Input
     &    WEATHER, XHLAI, YREND, YRPLT,                   !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, SNOW, SOILPROP, SW, SWDELTS,          !Output
     &    SWDELTU, WINF)                                  !Output

      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, PORMIN, RLV,    !Input
     &    RWUMX, SOILPROP, SW, SWDELTS, SWDELTU,          !Input
     &    WEATHER, WINF, XHLAI,                           !Input
     &    FLOODWAT,                                       !Input/Output
     &    EO, EOP, ES, SRFTEMP, ST, SWDELTX, TRWUP)       !Output

      !IF (CROP .NE. 'FA') THEN
        IF (INDEX('GROgro', MODEL(3:5)) .GT. 0) THEN 
          CALL CROPGRO(CONTROL, ISWITCH, 
     &    EOP, YREND, HARVFRAC, NH4, NO3, SOILPROP,       !Input
     &    ST, SW, TRWUP, WEATHER, YRPLT,                  !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,     !Output
     &    STGDOY, UNH4, UNO3, XHLAI, XLAI)                !Output

        ELSE
          CALL Alt_PLANT(CONTROL, ISWITCH,  
     &      EOP, FLOODWAT, HARVFRAC, NH4, NO3, SNOW,      !Input
     &      SOILPROP, SRFTEMP, ST, SW, TRWUP, WEATHER,    !Input
     &      YREND, YRPLT,                                 !Input
     &      FLOODN,                                       !I/O
     &      CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,      !Output
     &      MDATE, NSTRES, PORMIN, RLV, RWUMX, SENESCE,   !Output
     &      STGDOY, UNH4, UNO3, XHLAI, XLAI)              !Output
        ENDIF
      !ENDIF

C     Call management operations module for seasonal printout.
      CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, SOILPROP, ST, STGDOY, SW, WEATHER,    !Input
     &    YREND, HARVFRAC, IRRAMT, MDATE, YRPLT)          !Output

C-----------------------------------------------------------------------
C     Seasonal Output
C     Call end of season and summary output subroutines
C-----------------------------------------------------------------------
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
        CALL OPSUM (CONTROL, ISWITCH, YRPLT)
!      ENDIF

!!     Temporary timer function
!      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      
!!     Convert time to seconds
!      TIME1 = DATE_TIME(7) 
!     &      + DATE_TIME(8) / 1000.  
!     &      + DATE_TIME(6) * 60.  
!     &      + DATE_TIME(5) * 3600.
!      DELTA_TIME = TIME1 - TIME0
!      WRITE(200,'(1X,"RUN",I3,F10.3)') RUN, DELTA_TIME
!      TIME0 = TIME1

      CALL WARNING(0,'ENDRUN',FILEIO)
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
      RETURN
      END SUBROUTINE LAND 
