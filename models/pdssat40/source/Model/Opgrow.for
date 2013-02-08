C=======================================================================
C  OPGROW, Subroutine, G. Hoogenboom, J.W. Jones
C-----------------------------------------------------------------------
C  Generates output file for daily growth variables
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 GH  Written
C  09/21/1998 CHP Split off from OPDAY.FOR file
C  05/11/1999 GH  Incorporated in CROPGRO
C  06/19/2001 GH  Modified output format
C  08/20/2002 GH  Modified for Y2K
C  07/08/2003 CHP Changed senescence output.
C-----------------------------------------------------------------------
C  Called by: PLANT
C  Calls:     None
!=======================================================================
      SUBROUTINE OPGROW(CONTROL, ISWITCH, 
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,  
     &    GRWRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES,  
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, 
     &    PODNO, PODWT, PODWTD, RHOL, RHOS, RLV, RSTAGE, RTDEP, 
     &    RTWT, SATFAC, SDWT, SEEDNO, SENESCE, SLA, STMWT, SWFAC, 
     &    TGRO, TGROAV, TOPWT, TOTWT, TURFAC, VSTAGE, WTLF, 
     &    WTNCAN, WTNLF, WTNST, WTNSD, WTNUP, WTNFX, XLAI, YRPLT)
     &    
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  IDETG
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPGROW'
!      CHARACTER*8  FNAME
      CHARACTER*12 OUTG, OUTPC, OUTPN
      CHARACTER*30 FILEIO

      INTEGER DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP, I, L
      INTEGER NLAYR, NOUTDG, NOUTPC, NOUTPN, RUN, RSTAGE
      INTEGER TIMDIF, YEAR, YRDOY, MDATE, YRPLT, YRSIM

      REAL CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD
      REAL GROWTH, GRWRES, HI, HIP, MAINR, NSTRES
      REAL PCLSD, PCCSD, PCNL, PG, PODNO, PODWT, PODWTD
      REAL RHOL, RHOS, RTDEP, RTWT, STMWT, SDWT, SEEDNO
      REAL SDSIZE, SHELLW, SHELPC, SLA
      REAL SATFAC, SWFAC, TGROAV, TOPWT, TOTWT, TURFAC
      REAL VSTAGE, WTLF, XLAI

      REAL PCCSDP, PCLSDP, PCNLP, PCNRTP, PCNSDP
      REAL PCNSHP, PCNSTP, RHOLP, RHOSP, SLAP

      REAL RLV(NL)
      REAL TGRO(TS)

      REAL WTNCAN,WTNLF,WTNST,WTNSD,WTNUP,WTNFX
      REAL WTNVEG,PCNVEG,NFIXN
      REAL PCNST,PCNRT,PCNSH,PCNSD

      REAL CUMSENSURF,  CUMSENSOIL  !cumul. senesced matter, soil & surf
      REAL CUMSENSURFN, CUMSENSOILN !cumul. senes. N soil and surface

      LOGICAL FEXIST, FIRST

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
      TYPE (ResidueType) SENESCE

!     No output for fallow crop
      CROP    = CONTROL % CROP
      IDETG   = ISWITCH % IDETG
      IF (CROP .EQ. 'FA' .OR. IDETG .EQ. 'N') RETURN

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!      IF (IDETG .EQ. 'Y') THEN
        OUTG  = 'PlantGro.OUT'
        CALL GETLUN('OUTG',  NOUTDG)

        OUTPN  = 'PlantN.OUT  '
        CALL GETLUN('OUTPN', NOUTPN)

        OUTPC  = 'PlantC.OUT  '
        CALL GETLUN('OUTPC', NOUTPC)
!      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!       Initialize daily growth output file
        INQUIRE (FILE = OUTG, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTDG, FILE = OUTG, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDG,'("*GROWTH ASPECTS OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, FILEIO, NOUTDG, RUN)
        WRITE (NOUTDG,210)
  210   FORMAT('@YEAR DOY   DAS   DAP',
     &         '   L#SD   GSTD   LAID   LWAD   SWAD   GWAD',
     &         '   RWAD   CWAD   G#AD    GWGD   HIAD   PWAD',
     &         '   P#AD   WSPD   WSGD   NSTD   EWSD    LN%D',
     &         '   SH%D   HIPD   PWDD   PWTD   SLAD   CHTD',
     &         '   CWID   NWAD   RDPD   RL1D   RL2D   RL3D',
     &         '   RL4D   RL5D   RL6D   RL7D   RL8D   RL9D',
!CHP &         '  RL10   CDAD   LDAD   SDAD')
     &         '   RL10   SNW0C   SNW1C')

!-----------------------------------------------------------------------
!       Initialize daily plant nitrogen output file
        INQUIRE (FILE = OUTPN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTPN, FILE = OUTPN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTPN,'("*PLANT N OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, FILEIO, NOUTPN, RUN)

        WRITE (NOUTPN,230)
  230   FORMAT('@YEAR DOY   DAS   DAP',
     &      '    CNAD    GNAD    VNAD    GN%D    VN%D     NFXC    NUPC',
     &      '    LNAD    SNAD    LN%D    SN%D    SHND',
!CHP &      '  RN%D  NFXD')
     &      '   RN%D   NFXD   SNN0C   SNN1C')

!-----------------------------------------------------------------------
!       Initialize daily plant carbon output file
        INQUIRE (FILE = OUTPC, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTPC, FILE = OUTPC, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          FIRST = .FALSE.
        ELSE
          OPEN (UNIT = NOUTPC, FILE = OUTPC, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTPC,'("*PLANT C OUTPUT FILE")')
          FIRST = .TRUE.
        ENDIF

        !Write headers
        CALL HEADER(SEASINIT, FILEIO, NOUTPC, RUN)

        WRITE (NOUTPC,250)
  250   FORMAT('@YEAR DOY   DAS   DAP   TWAD    PHAD',
     &      '    CMAD    CGRD    GRAD    MRAD    CHAD   CL%D   CS%D',
     &      '   TGNN   TGAV    GN%D    GL%D    GC%D')

        CUMSENSURF  = 0.0
        CUMSENSOIL  = 0.0
        CUMSENSURFN = 0.0
        CUMSENSOILN = 0.0   

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
C   CHECK FOR OUTPUT FREQUENCY
C-----------------------------------------------------------------------
      IF (YRDOY .LT. YRPLT .OR. YRPLT .LT. 0) RETURN

!     Accumulate senesced matter for surface and soil.
      CUMSENSURF  = CUMSENSURF  + SENESCE % ResWt(0) 
      CUMSENSURFN = CUMSENSURFN + SENESCE % ResE(0,1) 
      DO L = 1, NLAYR
        CUMSENSOIL  = CUMSENSOIL  + SENESCE % ResWt(L)
        CUMSENSOILN = CUMSENSOILN + SENESCE % ResE(L,1)
      ENDDO

      IF ((MOD(DAS,FROP) .EQ. 0)          !Daily output every FROP days,
     &  .OR. (YRDOY .EQ. YRPLT)           !on planting date, and
     &  .OR. (YRDOY .EQ. MDATE)) THEN     !at harvest maturity 

!       Print 
        DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
        CALL YR_DOY(YRDOY, YEAR, DOY) 

        IF (IDETG .EQ. 'Y') THEN

          !Prior to emergence, do not report %N, %C values
!Hold off on this until GBuild can handle -99 values
!          IF (DAS .GE. NVEG0) THEN
            RHOLP  = RHOL * 100.
            RHOSP  = RHOS * 100.
            SLAP   = SLA
            PCNLP  = PCNL
            PCNSTP = PCNST
            PCNRTP = PCNRT
!          ELSE
!            RHOLP  = -99.
!            RHOSP  = -99.
!            SLAP   = -99.
!            PCNLP  = -99.
!            PCNSTP = -99.
!            PCNRTP = -99.
!          ENDIF

          !Prior to grain growth, do not report % compositions of seed
!Hold off on this until GBuild can handle -99 values
!          IF (SDWT .GT. 0.0001) THEN
            PCNSDP = PCNSD
            PCLSDP = PCLSD
            PCCSDP = PCCSD
            PCNSHP = PCNSH
!          ELSE
!            PCNSDP = -99.
!            PCLSDP = -99.
!            PCCSDP = -99.
!            PCNSHP = -99.
!          ENDIF

          IF (PODWT .GT. 0.1) THEN
            SHELPC = SDWT*100./PODWT
          ELSE
            SHELPC = 0.0
          ENDIF

          SHELPC = MIN(SHELPC,99.99)
          SHELLW = PODWT - SDWT       !Not used

          IF (SEEDNO .GT. 0.0) THEN
            SDSIZE = SDWT/SEEDNO*1000
          ELSE
            SDSIZE = 0.0
          ENDIF

          IF (TOPWT .GT. 0. .AND. SDWT .GE. 0.) THEN
            HI = SDWT/TOPWT
          ELSE
            HI = 0.
          ENDIF

          IF (TOPWT .GT. 0. .AND. PODWT .GE. 0.) THEN
            HIP = PODWT/TOPWT
          ELSE
            HIP = 0.
          ENDIF

          WRITE (NOUTDG,310)YEAR, DOY, DAS, DAP, VSTAGE, RSTAGE, XLAI,
     &        NINT(WTLF*10.), NINT(STMWT*10.), NINT(SDWT*10.),
     &        NINT(RTWT*10.), NINT(TOPWT*10.), NINT(SEEDNO), SDSIZE, HI,
     &        NINT(PODWT*10.), NINT(PODNO), (1.-SWFAC), (1.-TURFAC),
     &        (1.-NSTRES), SATFAC, PCNLP, SHELPC, HIP, NINT(PODWTD*10.),
     &        NINT((PODWTD+PODWT)*10.), SLAP, CANHT, CANWH, (DWNOD*10.),
     &        (RTDEP/100.), (RLV(I),I=1,10),
     &        NINT(CUMSENSURF), NINT(CUMSENSOIL)
  310       FORMAT (1X,I4,1X,I3.3,2(1X,I5),
     &        1X,F6.1,1X,I6,1X,F6.2,6(1X,I6),
     &        1X,F7.1,1X,F6.3,2(1X,I6),4(1X,F6.3),1X,F7.2,2(1X,F6.2),
     &        2(1X,I6),1X,F6.1,2(1X,F6.2),1X,F6.1,11(1X,F6.2),
     &        2(1X,I7))  !3(1X,I6))

C-----------------------------------------------------------------------
          WTNVEG  = (WTNLF + WTNST)
      
          IF ((WTLF+STMWT).GT. 0.0001) THEN
            PCNVEG = (WTNLF+WTNST)/(WTLF+STMWT)*100.
          ELSE
            !PCNVEG = -99.    !Wait for GBuild fix for -99's
            PCNVEG = 0.
          ENDIF

          WRITE (NOUTPN,410) YEAR, DOY, DAS, DAP, (WTNCAN*10), 
     &       (WTNSD*10), (WTNVEG*10), PCNSDP, PCNVEG, (WTNFX*10),
     &       (WTNUP*10), (WTNLF*10), (WTNST*10), PCNLP, PCNSTP,
     &       PCNSHP, PCNRTP, NFIXN*10, CUMSENSURFN, CUMSENSOILN
  410     FORMAT(1X,I4,1X,I3.3,2(1X,I5),3(1X,F7.1),2(1X,F7.2),1X,
     &       2(1X,F7.1),2(1X,F7.1),2(1X,F7.2),1X,F7.1,2(1X,F6.1),
     &       2(1X,F7.2))

C-----------------------------------------------------------------------
          WRITE (NOUTPC,510) YEAR, DOY, DAS, DAP,
     &        NINT(TOTWT*10), PG, CMINEA, GROWTH,
     &        GRWRES, MAINR, (CADLF + CADST), RHOLP, RHOSP,
     &        TGRO(12), TGROAV, PCNSDP, PCLSDP, PCCSDP
  510     FORMAT(1X,I4,1X,I3.3,2(1X,I5),1X,I6,6(1X,F7.2),2(1X,F6.1),
     &        2(1X,F6.1),3(1X,F7.2))
        ENDIF       
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
        !Close daily output files.
        CLOSE (NOUTDG)
        CLOSE (NOUTPN)
        CLOSE (NOUTPC)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END ! SUBROUTINE OPGROW
!=======================================================================


!=======================================================================
!       Variable definitions for OPGROW
!-----------------------------------------------------------------------
! CANHT   Canopy height (m)
! CANWH   Canopy width normal to row (m)
! DAP     Number of days after planting (d)
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
! MULTI   Current seasonal simulation (=1 for first or only simulation, 
!           =NYRS for last simulation 
! NL      maximum number of soil layers = 20 
! NOUTDG  Unit number for growth output file 
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
!       END SUBROUTINE OPGROW
!=======================================================================

