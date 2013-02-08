C=======================================================================
C  COPYRIGHT 1998-2005 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C  SOIL, Subroutine
C-----------------------------------------------------------------------
C  Soil Processes subroutine.  Calls the following modules:
C     SOILDYN  - integrates soil properties variables
C     STEMP    - soil temperature
C     WATBAL   - soil water balance
C     NTRANS, CENTURY - Soil N and C balance
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/02/2001 CHP Written
C  04/20/2002 GH  Modified for crop rotations
!  10/24/2005 CHP Put weather variables in constructed variable. 
!  10/25/2005 CHP Removed NTRANS_OLD module
C=====================================================================

      SUBROUTINE SOIL(CONTROL, ISWITCH, 
     &    EO, ES, FLOODWAT, HARVRES, IRRAMT, NSTRES,      !Input
     &    SENESCE, SRFTEMP, ST, SWDELTX, UNH4, UNO3,      !Input
     &    WEATHER, XHLAI, YREND, YRPLT,                   !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, SNOW, SOILPROP, SW, SWDELTS,          !Output
     &    SWDELTU, WINF)                                  !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE FloodModule

      IMPLICIT NONE

      CHARACTER*1  MESOM, ISWWAT
      CHARACTER*6, PARAMETER :: ERRKEY = 'SOIL  '

      INTEGER DYNAMIC, YREND
      INTEGER YRPLT, RUN

      REAL EO, ES, IRRAMT
      REAL NSTRES
      REAL SNOW
      REAL WINF

      REAL, DIMENSION(NL) :: DRN, FLOW
      REAL, DIMENSION(NL) :: NH4, NO3
      REAL, DIMENSION(NL) :: ST, SW, SWDELTS, SWDELTX, SWDELTU
      REAL, DIMENSION(NL) :: UNH4, UNO3

      REAL SRFTEMP
      CHARACTER*1 ISWNIT, RNMODE

!     Added for flooded N routines
      REAL XHLAI

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType)  CONTROL
      TYPE (SwitchType)   ISWITCH
      TYPE (SoilType)     SOILPROP
      Type (ResidueType)  HARVRES
      Type (ResidueType)  SENESCE
      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType)   FLOODN
      TYPE (WeatherType)  WEATHER

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

      ISWNIT = ISWITCH % ISWNIT
      ISWWAT = ISWITCH % ISWWAT
      MESOM  = ISWITCH % MESOM

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!      IF (RUN .LE. 1 .OR. (RNMODE .NE. 'Q' .AND. RUN .GT. 1)) THEN
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN .LE. 1) THEN
        CALL SOILDYN(CONTROL, MESOM, SOILPROP)

C-------------------------------------------------------------------------
C         Read input parameters for Soil water balance routines
C-------------------------------------------------------------------------
        CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX, WEATHER,         !Input
     &    FLOODWAT,                                       !I/O
     &    DRN, FLOW, SNOW, SW, SWDELTS, SWDELTU, WINF)    !Output

C-------------------------------------------------------------------------
C       Read input parameters for soil nitrogen routines
C       Note:  these routines are called to input data even if
C         soil N processes are not simulated.
C-------------------------------------------------------------------------
        IF (MESOM .EQ. 'G') THEN
        !Call CERES-based (Godwin) soil nitrogen module to read 
        !input data.   
          CALL NTRANS (CONTROL, ISWITCH, 
     &    DRN, ES, FLOODWAT, FLOW, HARVRES, NSTRES,       !Input
     &    SENESCE, SOILPROP, ST, SW, UNH4, UNO3,          !Input
     &    WEATHER, XHLAI, YRPLT,                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3)                                       !Output

        ELSEIF (MESOM .EQ. 'P') THEN
        !Call CENTURY-baeed (Parton) soil nitrogen module for 
        ! data input
          CALL CENTURY(CONTROL, ISWITCH, 
     &    DRN, EO, FLOW, HARVRES, NSTRES, SENESCE,        !Input
     &    SOILPROP, SRFTEMP, ST, SW, UNH4, UNO3, WINF,    !Input
     &    YREND, YRPLT,                                   !Input
     &    NH4, NO3)                                       !Output
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
C       Soil water balance initialization
C-----------------------------------------------------------------------
      CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX, WEATHER,         !Input
     &    FLOODWAT,                                       !I/O
     &    DRN, FLOW, SNOW, SW, SWDELTS, SWDELTU, WINF)    !Output

C-----------------------------------------------------------------------
C       Soil nitrogen initialization
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y') THEN
        IF (MESOM .EQ. 'G') THEN
          !Godwin (Ceres-based) soil N module
          CALL NTRANS (CONTROL, ISWITCH, 
     &    DRN, ES, FLOODWAT, FLOW, HARVRES, NSTRES,       !Input
     &    SENESCE, SOILPROP, ST, SW, UNH4, UNO3,          !Input
     &    WEATHER, XHLAI, YRPLT,                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3)                                       !Output

        ELSEIF (MESOM .EQ. 'P') THEN
          !Parton (Century-based) soil N module
          CALL CENTURY(CONTROL, ISWITCH, 
     &    DRN, EO, FLOW, HARVRES, NSTRES, SENESCE,        !Input
     &    SOILPROP, SRFTEMP, ST, SW, UNH4, UNO3, WINF,    !Input
     &    YREND, YRPLT,                                   !Input
     &    NH4, NO3)                                       !Output
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Rate Calculations 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C  Call WATBAL Subroutine to calculate evapotranspiration
C     and soil water balance
C-----------------------------------------------------------------------
!      IF (ISWWAT .EQ. 'Y') THEN
        CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX, WEATHER,         !Input
     &    FLOODWAT,                                       !I/O
     &    DRN, FLOW, SNOW, SW, SWDELTS, SWDELTU, WINF)    !Output
!      ENDIF

C-----------------------------------------------------------------------
C  Call NTRANS Subroutine to calculate nitrogen transformations in the
C      soil
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y') THEN
        IF (MESOM .EQ. 'G') THEN
        ! Call Godwin soil nitrogen module
          CALL NTRANS (CONTROL, ISWITCH, 
     &    DRN, ES, FLOODWAT, FLOW, HARVRES, NSTRES,       !Input
     &    SENESCE, SOILPROP, ST, SW, UNH4, UNO3,          !Input
     &    WEATHER, XHLAI, YRPLT,                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3)                                       !Output

        ELSEIF (MESOM .EQ. 'P') THEN
        ! Call Parton soil nitrogen module
          CALL CENTURY(CONTROL, ISWITCH, 
     &    DRN, EO, FLOW, HARVRES, NSTRES, SENESCE,        !Input
     &    SOILPROP, SRFTEMP, ST, SW, UNH4, UNO3, WINF,    !Input
     &    YREND, YRPLT,                                   !Input
     &    NH4, NO3)                                       !Output
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN

C-----------------------------------------------------------------------
      CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX, WEATHER,         !Input
     &    FLOODWAT,                                       !I/O
     &    DRN, FLOW, SNOW, SW, SWDELTS, SWDELTU, WINF)    !Output

C-----------------------------------------------------------------------
C  Call NTRANS Subroutine to calculate nitrogen transformations in the
C      soil
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y') THEN
        IF (MESOM .EQ. 'G') THEN
        ! Call Godwin soil nitrogen module
          CALL NTRANS (CONTROL, ISWITCH, 
     &    DRN, ES, FLOODWAT, FLOW, HARVRES, NSTRES,       !Input
     &    SENESCE, SOILPROP, ST, SW, UNH4, UNO3,          !Input
     &    WEATHER, XHLAI, YRPLT,                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3)                                       !Output

        ELSEIF (MESOM .EQ. 'P') THEN
        ! Call Parton soil nitrogen module
          CALL CENTURY(CONTROL, ISWITCH, 
     &    DRN, EO, FLOW, HARVRES, NSTRES, SENESCE,        !Input
     &    SOILPROP, SRFTEMP, ST, SW, UNH4, UNO3, WINF,    !Input
     &    YREND, YRPLT,                                   !Input
     &    NH4, NO3)                                       !Output
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
C     Call DAilY OutPut subroutines
C-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN
        CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX, WEATHER,         !Input
     &    FLOODWAT,                                       !I/O
     &    DRN, FLOW, SNOW, SW, SWDELTS, SWDELTU, WINF)    !Output
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
        IF (MESOM .EQ. 'G') THEN
          CALL NTRANS (CONTROL, ISWITCH, 
     &    DRN, ES, FLOODWAT, FLOW, HARVRES, NSTRES,       !Input
     &    SENESCE, SOILPROP, ST, SW, UNH4, UNO3,          !Input
     &    WEATHER, XHLAI, YRPLT,                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3)                                       !Output

        ELSEIF (MESOM .EQ. 'P') THEN
!           CENTURY-based SOM/residue module.
          CALL CENTURY(CONTROL, ISWITCH, 
     &    DRN, EO, FLOW, HARVRES, NSTRES, SENESCE,        !Input
     &    SOILPROP, SRFTEMP, ST, SW, UNH4, UNO3, WINF,    !Input
     &    YREND, YRPLT,                                   !Input
     &    NH4, NO3)                                       !Output
        ENDIF
      ENDIF


!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
      IF (ISWWAT .EQ. 'Y') THEN
        CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX, WEATHER,         !Input
     &    FLOODWAT,                                       !I/O
     &    DRN, FLOW, SNOW, SW, SWDELTS, SWDELTU, WINF)    !Output
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
        IF (MESOM .EQ. 'G') THEN
          CALL NTRANS (CONTROL, ISWITCH, 
     &    DRN, ES, FLOODWAT, FLOW, HARVRES, NSTRES,       !Input
     &    SENESCE, SOILPROP, ST, SW, UNH4, UNO3,          !Input
     &    WEATHER, XHLAI, YRPLT,                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3)                                       !Output

        ELSEIF (MESOM .EQ. 'P') THEN
!         CENTURY-based SOM/residue module.
          CALL CENTURY(CONTROL, ISWITCH, 
     &    DRN, EO, FLOW, HARVRES, NSTRES, SENESCE,        !Input
     &    SOILPROP, SRFTEMP, ST, SW, UNH4, UNO3, WINF,    !Input
     &    YREND, YRPLT,                                   !Input
     &    NH4, NO3)                                       !Output
        ENDIF
      ENDIF

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************

      RETURN
      END !SUBROUTINE SOIL

C=======================================================================
