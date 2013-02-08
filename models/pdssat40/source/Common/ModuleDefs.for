!=======================================================================
C  MODULE ModuleDefs
C  11/01/2001 CHP Written
C  06/15/2002 CHP Added flood-related data constructs 
C  03/12/2003 CHP Added residue data construct
C  05/08/2003 CHP Added version information
C  09/03/2004 CHP Modified GetPut_Control routine to store entire
C                   CONTROL variable. 
C             CHP Added GETPUT_ISWITCH routine to store ISWITCH.
C             CHP Added TRTNO to CONTROL variable.
!  06/14/2005 CHP Added FILEX to CONTROL variable.
!  10/24/2005 CHP Put weather variables in constructed variable. 
!             CHP Added PlantStres environmental stress variable
!=======================================================================

      MODULE ModuleDefs
!     Contains defintion of derived data types and constants which are 
!     used throughout the model.
      SAVE
!=======================================================================
!     Global CSM Version Number
      TYPE VersionType
        INTEGER :: Major = 4
        INTEGER :: Minor = 0  
        INTEGER :: Sub   = 2
        INTEGER :: Build = 0  
      END TYPE VersionType
      TYPE (VersionType) Version

!     Version history:
!       4.0.2.0 chp 08/11/2005 Release
!       4.0.2.1 chp 08/02/2005 Pre-release
!       4.0.2.0 chp 01/14/2005 
!       4.0.1.1 chp 12/16/2004 Minor updates, Hawaii
!       4.0.1.0 chp 01/28/2004 Release Version 
!       4.0.1.1 chp 01/07/2004
!       4.0.1.0 chp 10/14/2003
!       4.0.6   chp 08/29/2003
!       4.0.5   chp 08/12/2003
!       4.0.4   chp 07/22/2003
!       4.0.3   chp 06/27/2003
!       4.0.2   chp 06/04/2003
!       4.0.1   chp 05/08/2003

!=======================================================================

!     Global constants
      INTEGER, PARAMETER :: 
     &    NL       = 20,  !Maximum number of soil layers 
     &    TS       = 24,  !Number of hourly time steps per day
     &    NAPPL    = 250, !Maximum number of applications or operations
     &    NCOHORTS = 300, !Maximum number of cohorts
     &    NELEM    = 1,   !Number of elements modeled (currently only N)

         !Dynamic variable values
     &    RUNINIT  = 1, 
     &    INIT     = 2,  !Will take the place of RUNINIT & SEASINIT
                         !     (not fully implemented)
     &    SEASINIT = 2, 
     &    RATE     = 3,
     &    EMERG    = 3,  !Used for some plant processes.  
     &    INTEGR   = 4,  
     &    OUTPUT   = 5,  
     &    FINAL    = 6

!=======================================================================

!     Data construct for control variables
      TYPE ControlType
        CHARACTER (len=1)  MESIC, RNMODE
        CHARACTER (len=2)  CROP
        CHARACTER (len=8)  MODEL
        CHARACTER (len=12) FILEX
        CHARACTER (len=30) FILEIO
        INTEGER      DAS, DYNAMIC, FROP, LUNIO, MULTI, REPNO, RUN
        INTEGER      TRTNO, YRDOY, YRSIM, NYRS, YRDIF
      END TYPE ControlType

!=======================================================================
!     Data construct for control switches
      TYPE SwitchType
        CHARACTER (len=1) IDETC, IDETD, IDETG, IDETL, IDETN, IDETO
        CHARACTER (len=1) IDETR, IDETS, IDETW
        CHARACTER (len=1) IHARI, IPLTI, IIRRI
        CHARACTER (len=1) ISWCHE, ISWDIS, ISWNIT, ISWSYM, ISWTIL, ISWWAT
        CHARACTER (len=1) MEEVP, MEPHO, MESOM
        CHARACTER (len=1) IFERI, IRESI
        INTEGER NSWI
      END TYPE SwitchType

!Other switches and methods used by model:
! ISWPHO, ISWPOT, MELI, MEINF, MEHYD, NSWI, IOX, IDETP, IDETR- not used
! IDETH - only used in MgmtOps
! MEWTH - only used in WEATHR

!=======================================================================
!     Data construct for weather variables
      TYPE WeatherType
!       Weather station information
        REAL REFHT, WINDHT, XLAT

!       Daily weather data.
        REAL CLOUDS, CO2, DAYL, PAR, RAIN, SNDN, SNUP, SRAD, 
     &    TAMP, TA, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY,      
     &    TMAX, TMIN, TWILEN, WINDSP
            
!       Hourly weather data
        REAL
     &    AZZON(TS), BETA(TS), FRDIFP(TS), FRDIFR(TS),
     &    PARHR(TS), RADHR(TS), RHUMHR(TS),
     &    TAIRHR(TS), TGRO(TS), WINDHR(TS)
      END TYPE WeatherType

!=======================================================================
!     Data construct for soil variables
      TYPE SoilType
        INTEGER NLAYR
        REAL CN, DMOD, SALB, SLPF, SWCON, U
        REAL, DIMENSION(NL) :: ADCOEF, BD, CEC, CLAY, DLAYR, DS
        REAL, DIMENSION(NL) :: DUL, LL, OC, PH, PHKCL, SAND
        REAL, DIMENSION(NL) :: SAT, SILT, STONES, SWCN, TOTN, WR
      !These variables could be made available if needed elsewhere.
      !  They are currently read by SOILDYN module.
      !  CHARACTER*5 SLTXS
      !  CHARACTER*11 SLSOUR
      !  CHARACTER*50 SLDESC, TAXON
      END TYPE SoilType

!=======================================================================
!   Data construct for residue (harvest residue, senesced matter, etc.)
      TYPE ResidueType
        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha
        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha
        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha (E=N, P, S,...)
      END TYPE ResidueType

!=======================================================================
!     Data construct for oxidation layer
      TYPE OxLayerType
        INTEGER IBP
        REAL    OXU, OXH4, OXN3   
        REAL    OXLT, OXMIN4, OXMIN3
        REAL    DLTOXU, DLTOXH4, DLTOXN3
        REAL    ALGACT
        LOGICAL DAILY, UNINCO
      END TYPE OxLayerType

!======================================================================
!     Plant stresses for environmental stress summary
      Type PlStresType
        INTEGER NSTAGES   !# of stages (max 5)
        CHARACTER(len=23) StageName(5)
        REAL TURFAC, SWFAC, NSTRES, AGEFAC
        LOGICAL ACTIVE(5)
      End Type

!======================================================================
      CONTAINS

      SUBROUTINE GETPUT_CONTROL(CODE, CONTROL_ARG)
!     Transfers CONTROL variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (ControlType) CONTROL_ARG, CONTROL_SAVE

      SELECT CASE(CODE)
        CASE('GET','get')
          CONTROL_ARG = CONTROL_SAVE
        CASE('PUT','put')
          CONTROL_SAVE = CONTROL_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_CONTROL

!======================================================================

      SUBROUTINE GETPUT_ISWITCH(CODE, ISWITCH_ARG)
!     Transfers ISWITCH variable data where needed

      IMPLICIT NONE
      CHARACTER (len=3)  CODE
      TYPE (SwitchType) ISWITCH_ARG, ISWITCH_SAVE

      SELECT CASE(CODE)
        CASE('GET')
          ISWITCH_ARG = ISWITCH_SAVE
        CASE('PUT')
          ISWITCH_SAVE = ISWITCH_ARG
      END SELECT
      
      RETURN
      END SUBROUTINE GETPUT_ISWITCH

!======================================================================
      END MODULE ModuleDefs


!======================================================================
!     Paddy Managment routines.
!======================================================================
      MODULE FloodModule
!=======================================================================
!     Data construct for flood data. 
      Type FloodWatType
        !From IRRIG
        LOGICAL BUNDED        
        INTEGER NBUND         
        REAL ABUND            
        REAL PUDPERC, PERC    

        !From Paddy_Mgmt
        INTEGER YRDRY, YRWET  
        REAL FLOOD, FRUNOFF   
        REAL TOTBUNDRO        
        LOGICAL PUDDLED       

        REAL CEF, EF          !From SPAM
        REAL INFILT, RUNOFF   !From WATBAL
      End Type FloodWatType

      Type FloodNType
        INTEGER NDAT
        REAL    FLDH4C, FLDN3C                !Flood N concentrations
        REAL    FLDU, FLDN3, FLDH4            !Flood N mass (kg/ha)
        REAL    FRNH4U, FRNO3U                !Flood N uptake (kg/ha)
        REAL    DLTFUREA, DLTFNH4, DLTFNO3    !Flood N flux (kg/ha)
      End Type FloodNType

      END MODULE FloodModule
!======================================================================

