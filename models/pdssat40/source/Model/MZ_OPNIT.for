C=======================================================================
C  MZ_OPNIT, Subroutine, G. Hoogenboom, J.W. Jones
C-----------------------------------------------------------------------
C  Generates output file for daily plant nitrogen variables
C     This routine is used for maize, sorghum and millet.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990     Written
C  09/21/1998 CHP Split off from OPDAY.FOR file
C  05/11/1999 GH  Incorporated in CROPGRO
C  12/18/2001 WDB Revised for modular CERES
C  08/20/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called by: MAIZE, SG_CERES, ML_CERES
C  Calls:     None
!=======================================================================
      SUBROUTINE MZ_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      INTEGER DAS, DYNAMIC, MDATE, NOUTDN, YRSIM

      CHARACTER*1  IDETG, RNMODE
      CHARACTER*6, PARAMETER :: ERRKEY = 'MZ_OPN'
      CHARACTER*12 OUTPN
      CHARACTER*30 FILEIO

      INTEGER DAP, DOY, ERRNUM, FROP, L, NLAYR, RUN
      INTEGER TIMDIF, YEAR, YRDOY, YRPLT

      REAL PCNL
      REAL WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG
      REAL WTNUP,WTNLF,WTNST,PCNST,PCNRT     
      REAL CUMSENSURFN, CUMSENSOILN   !cumul. senes. N soil and surface

      LOGICAL FEXIST, FIRST

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
      TYPE (ResidueType) SENESCE

      IDETG   = ISWITCH % IDETG
      IF (IDETG .EQ. 'N') RETURN

      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

!-----------------------------------------------------------------------
      IF(DYNAMIC.EQ.RUNINIT) THEN

          OUTPN = 'PlantN.OUT'
          CALL GETLUN('OUTPN', NOUTDN)

      ENDIF

!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN

!     Initialize daily plant nitrogen output file
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDN, FILE = OUTPN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.  
        ELSE
          OPEN (UNIT = NOUTDN, FILE = OUTPN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDN,'("*PLANT N OUTPUT FILE")')
          FIRST = .TRUE.  
        ENDIF

        CALL HEADER(SEASINIT, FILEIO, NOUTDN, RUN)
        WRITE (NOUTDN,230)
  230   FORMAT('@YEAR DOY   DAS   DAP',
     &        '   CNAD   GNAD   VNAD   GN%D   VN%D   NUPC',
     &        '   LNAD   SNAD   LN%D   SN%D   RN%D   SNN0C   SNN1C')

        !cumul. senes. N soil and surface
        CUMSENSURFN = 0.0
        CUMSENSOILN = 0.0   

      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      IF (DYNAMIC .EQ. OUTPUT) THEN

!         Calculate cumulative N senesenced
          CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
          DO L = 1, NLAYR
            CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
          ENDDO

      !-----------------------------------------------------------------
      !   CHECK FOR OUTPUT FREQUENCY
      !-----------------------------------------------------------------
        IF (YRDOY .GE. YRPLT .AND. YRPLT .GT. 0)
     &          THEN

          DAP = MAX(0, TIMDIF(YRPLT, YRDOY))
          DAS = MAX(0, TIMDIF(YRSIM, YRDOY))

          IF ((MOD(DAS,FROP) .EQ. 0)     !Daily output every FROP days,
     &      .OR. (YRDOY .EQ. YRPLT)         !on planting date, and
     &      .OR. (YRDOY .EQ. MDATE)) THEN   !at harvest maturity 

            CALL YR_DOY(YRDOY, YEAR, DOY)

!           Print 
            WRITE (NOUTDN,300) YEAR, DOY, DAS, DAP,
     &       (WTNCAN*10.0), (WTNSD*10.0), (WTNVEG*10.0),
     &        PCNGRN, PCNVEG, (WTNUP*10.0),
     &       (WTNLF*10.0), (WTNST*10.0), PCNL, PCNST, PCNRT,
     &        CUMSENSURFN, CUMSENSOILN
 300        FORMAT (1X,I4,1X,I3.3,2(1X,I5),3(1X,F6.1),2(1X,F6.2),
     &        4(1X,F6.1),2(1X,F6.2), 2F8.2)
          ENDIF
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      IF (DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
        CLOSE (NOUTDN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE MZ_OPNIT
!=======================================================================


!=======================================================================
!       Variable definitions for OPGROW
!-----------------------------------------------------------------------
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! CROP    Crop identification code 
! DWNOD   Current nodule mass (g[nodule] / m2)
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or FINAL 
! ENAME   Experiment description 
! EXPER   Experiment code (prefix of input files) 
! HI      Ratio of seed weight (SDWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[seed] / g[tops])
! HIP     Ratio of pod weight (PODWT) to weight of above-ground portion of 
!           plant (TOPWT) (g[pods] / g[tops])
! MODEL   Name of CROPGRO executable file 
! NL      maximum number of soil layers = 20 
! NOUTDN  Unit number for NITROGEN.OUT output file 
! RUN    Report number for sequenced or multi-season runs 
! NSTRES  Nitrogen stress factor (1=no stress, 0=max stress) 
! OUTG    Growth output file name (typically 'GROWTH.OUT') 
! PCNL    Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PODNO   Total number of pods (#/m2)
! PODWT   Dry mass of seeds plus shells, including C and N
!           (g[pods] / m2[ground])
! PODWTD  Mass of detached pods (g[pods] / m2[ground])
! RLV(L)  Root length density for soil layer L ((cm root / cm3 soil))
! RSTAGE  Number of RSTAGES which have occurred. 
! RTDEP   Root depth (cm)
! RTWT    Dry mass of root tissue, including C and N (g[root] / m2[ground])
! SATFAC  Root length weighted soil water excess stress factor ( 0 = no 
!           stress; 1 = saturated stress ) 
! SDSIZE  Average mass of seeds (mg / seed)
! SDWT    Dry mass of seed tissue, including C and N (g[seed] / m2[ground])
! SEEDNO  Total number of seeds (#/m2)
! SHELLW  Shell mass (g[shell] / m2)
! SHELPC  Percentage of pod mass that is seeds (g[seed]/g[pods] * 100%)
! SLA     Specific leaf area (cm2[leaf] / m2[ground])
! STMWT   Dry mass of stem tissue, including C and N (g[stem] / m2[ground)
! SWFAC   Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!           0.0=max stress 
! TITLET  Description of treatment for this simulation 
! TOPWT   Total weight of above-ground portion of crop, including pods
!           (g[tissue] / m2)
! TRTNO   Treatment number 
! TURFAC  Water stress factor for expansion (0 - 1) 
! VSTAGE  Number of nodes on main stem of plant 
! WTCO    Cumulative losses of plant tissue (g[tissue] / m2)
! WTLF    Dry mass of leaf tissue including C and N (g[leaf] / m2[ground])
! WTLO    Cumulative leaf losses (g[leaf] / m2)
! WTSO    Cumulative stem losses (g[stem] / m2)
! XLAI    Leaf area (one side) per unit of ground area
!           (m2[leaf] / m2[ground])
! YRDOY   Current day of simulation (YYDDD)
! YRPLT   Planting date (YYDDD)
!***********************************************************************
!       END SUBROUTINE MZ_OPNIT
!=======================================================================

