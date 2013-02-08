C=======================================================================
C  COPYRIGHT 1998-2005 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=====================================================================
C  MgmtOps, Subroutine
C-----------------------------------------------------------------------
C  Operations Management subroutine.  Calls all operations modules:
C     AUTPLT   - automatic planting
C     AUTHAR   - automatic harvest
C     TILLAGE  - tillage
C     Chemical - Chemical applications
C     IRRIG    - irrigation applications
C  Eventually, move fertilizer placement and residue placement modules here.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/15/2001 CHP Written
C  04/16/2002 GH  Adjustment for sequence analysis
C  08/01/2002 CHP Merged RUNINIT and SEASINIT into INIT section
!  10/24/2005 CHP Put weather variables in constructed variable. 
C=====================================================================

      SUBROUTINE MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, SOILPROP, ST, STGDOY, SW, WEATHER,    !Input
     &    YREND, HARVFRAC, IRRAMT, MDATE, YRPLT)          !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE FloodModule
      IMPLICIT NONE
      SAVE

      CHARACTER*1  IHARI, IIRRI, IPLTI, ISWCHE, RNMODE
      CHARACTER*1  IDETO, ISWTIL, ISWWAT
      CHARACTER*2  CROP
      CHARACTER*6, PARAMETER :: ERRKEY = 'MGMTOP'

      INTEGER DAP, DYNAMIC    !, FOUND
      INTEGER YREND, IDATE, ISIM               !, LINC
      INTEGER NAP, NCHEM                 !, LUNIO
      INTEGER NHAR, NTIL, RUN, TIMDIF
      INTEGER YRDIF, YRDOY, MDATE, YRO, YRPLT, YRS, YRSIM
      INTEGER HDATE(3)
      INTEGER STGDOY(20)
      INTEGER TILLDATE(NAPPL)

      REAL IRRAMT, TOTIR
      REAL HPC(3), HBPC(3), HARVFRAC(2)
      REAL, DIMENSION(NL) :: DLAYR, DUL, LL, ST, SW
      REAL TILLDEP(NAPPL), TILLMIX(NAPPL)

!     Variables added for flooded conditions
      INTEGER NBUND
      REAL FLOOD, RAIN

      !Variables needed to call IPAHAR for sequenced runs:
      INTEGER HDLAY, HLATE, HSTG(3)
      REAL    SWPLTL, SWPLTH, SWPLTD

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 2
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "SOILPROP" is of type "SoilType".
      TYPE (SoilType) SOILPROP

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType) FLOODN
      TYPE (WeatherType)  WEATHER

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP  
      DYNAMIC = CONTROL % DYNAMIC 
      RUN     = CONTROL % RUN   
      YRDOY   = CONTROL % YRDOY   
      YRSIM   = CONTROL % YRSIM   
      YRDIF   = CONTROL % YRDIF
      RNMODE  = CONTROL % RNMODE

      DLAYR  = SOILPROP % DLAYR  
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      
      ISWWAT = ISWITCH % ISWWAT
      ISWCHE = ISWITCH % ISWCHE
      ISWTIL = ISWITCH % ISWTIL
      IPLTI  = ISWITCH % IPLTI
      IIRRI  = ISWITCH % IIRRI
      IHARI  = ISWITCH % IHARI
      IDETO  = ISWITCH % IDETO

      FLOOD = FLOODWAT % FLOOD
      RAIN  = WEATHER  % RAIN

C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
C-----------------------------------------------------------------------
C     For sequenced runs, YRDIF (the time difference in years to adjust
C       dates for the new season), is calculated based on HDATE (if 
C       IHARI = 'D') or on YRPLT
C-----------------------------------------------------------------------
      IF (RUN .EQ. 1) THEN
        YRDIF = 0
      ELSEIF (RNMODE .EQ. 'Q') THEN  
        IF (CROP .EQ. 'FA' .AND. IHARI .NE. 'D') THEN
          CALL YR_DOY(YRSIM, YRS, ISIM)
C         YRDIF based on HDATE
!         Call IPAHAR to read HDATE for YRDIF calculation.
          CALL IPAHAR(CONTROL,
     &        HPC, HBPC, HDATE, HDLAY, HLATE, HSTG,
     &        NHAR, SWPLTL, SWPLTH, SWPLTD)
          CALL YR_DOY(HDATE(1), YRO, IDATE)
          YRDIF    =  YRS - YRO
          HDATE(1) = (YRO + YRDIF) * 1000 + IDATE
          IF (HDATE(1) .LT. YRSIM) THEN
            YRDIF = YRDIF + 1
            HDATE(1) = (YRO + YRDIF) * 1000 + IDATE
          ENDIF
        CONTROL % YRDIF   = YRDIF
        ENDIF 
      ENDIF       

C-----------------------------------------------------------------------
C     Call modules to modify dates for seasonal or sequenced runs.
C-----------------------------------------------------------------------
!      IF (CROP .NE. 'FA') THEN
        CALL AUTPLT (CONTROL, ISWWAT,
     &    DLAYR, DUL, FLOOD, IDETO, IPLTI, LL, ST, SW,    !Input
     &    MDATE, YRPLT)                                   !Output
!      ENDIF
C-----------------------------------------------------------------------
C     Adjust harvest dates for seasonal or sequenced runs.
C     For potato, sets harvest date.
C-----------------------------------------------------------------------
      CALL AUTHAR(CONTROL, ISWWAT,
     &    DLAYR, DUL, IDETO, IHARI, LL, STGDOY,           !Input
     &    SW, MDATE, YRPLT,                               !Input
     &    YREND, HARVFRAC, HDATE, NHAR)                   !Output
    
      IF (ISWCHE .EQ. 'Y') THEN
        CALL Chemical(CONTROL, ISWITCH, NCHEM)
      ENDIF

      IF (ISWTIL .EQ. 'Y') THEN
        CALL TILLAGE(CONTROL, NTIL, TILLDATE, TILLDEP, TILLMIX)
      ENDIF

      CALL IRRIG(CONTROL, ISWITCH,
     &    RAIN, SOILPROP, SW, MDATE, YRPLT,               !Input
     &    FLOODWAT, IIRRI, IRRAMT, NAP, TOTIR)            !Output

      NBUND = FLOODWAT % NBUND
      !IF (NBUND .GT. 0) THEN
          !initialize flood management variables.
          CALL PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                                   !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 
      !ENDIF

!      CALL OpMgmt(CONTROL, ISWITCH,
!     &    IIRRI, NAP, NTIL, NCHEM, TOTIR, YRPLT)

C***********************************************************************
C***********************************************************************
C     Rate Calculations 
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
C  Call AUTPLT subroutine if automatic planting on, determine YRPLT
C-----------------------------------------------------------------------
      DAP = MAX0 (0, TIMDIF (YRPLT, YRDOY))

      IF (DAP .EQ. 0 .AND. IPLTI .EQ. 'A' .AND. CROP .NE. 'FA') THEN
        CALL AUTPLT (CONTROL, ISWWAT,
     &    DLAYR, DUL, FLOOD, IDETO, IPLTI, LL, ST, SW,    !Input
     &    MDATE, YRPLT)                                   !Output
      ENDIF

      IF (INDEX('AFRDPW',IIRRI) .GT. 0 .AND. ISWWAT .EQ. 'Y') THEN
!       Calculate irrigation depth for today
        CALL IRRIG(CONTROL, ISWITCH,
     &    RAIN, SOILPROP, SW, MDATE, YRPLT,               !Input
     &    FLOODWAT, IIRRI, IRRAMT, NAP, TOTIR)            !Output
      ELSE
          IRRAMT = 0.0
      ENDIF

      IF (ISWCHE .EQ. 'Y' .AND. NCHEM .GT. 0 .) THEN
        CALL Chemical(CONTROL, ISWITCH, NCHEM)
      ENDIF

      IF (ISWTIL .EQ. 'Y' .AND. NTIL .GT. 0) THEN
        CALL TILLAGE(CONTROL, NTIL, TILLDATE, TILLDEP, TILLMIX)
      ENDIF

C***********************************************************************
C***********************************************************************
C     Daily integration
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN

C-----------------------------------------------------------------------
C     Call AUTHAR subroutine to check harvest switch and
C     determine YREND
C-----------------------------------------------------------------------
!     Calculate cumulative irrigation
      IF (INDEX('AFRDPW',IIRRI) .GT. 0 .AND. ISWWAT .EQ. 'Y') THEN
        CALL IRRIG(CONTROL, ISWITCH,
     &    RAIN, SOILPROP, SW, MDATE, YRPLT,               !Input
     &    FLOODWAT, IIRRI, IRRAMT, NAP, TOTIR)            !Output
      ENDIF

      IF (NBUND .GT. 0) THEN
        !Determine flood depth today.
        CALL PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                            !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 
      ENDIF

      CALL AUTHAR(CONTROL, ISWWAT, 
     &    DLAYR, DUL, IDETO, IHARI, LL, STGDOY,           !Input
     &    SW, MDATE, YRPLT,                               !Input
     &    YREND, HARVFRAC, HDATE, NHAR)                   !Output

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
!  Variables which need to be written to output files:
!     AMIR   - OPOPS
!     EFFIRR - WBAL 
!     IIRRI  - OPOPS
!     JULAPL - OPOPS
!     NAP    - OPHARV, OPOPS, OPWBAL
!     TOTIR  - OPDAY, OPHARV, OPWBAL, WBAL

!-----------------------------------------------------------------------
!      CALL OpMgmt(CONTROL, ISWITCH,
!     &    IIRRI, NAP, NTIL, NCHEM, TOTIR, YRPLT)

      IF (NBUND .GT. 0) THEN
        CALL PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                                   !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 
      ENDIF

!***********************************************************************
!***********************************************************************
!     FINAL - seasonal output and close files
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------

!      CALL OpMgmt(CONTROL, ISWITCH,
!     &    IIRRI, NAP, NTIL, NCHEM, TOTIR, YRPLT)

      IF (NBUND .GT. 0) THEN
        CALL PADDY_MGMT (CONTROL, ISWITCH,
     &    IRRAMT, RAIN,                                   !Input
     &    FLOOD, FLOODWAT, FLOODN)                        !Output 
      ENDIF

!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved as real numbers for placement in real array.
!        LABEL(1)  = 'HBPC'; VALUE(1)  = HBPC(1)
!        LABEL(2)  = 'HPC '; VALUE(2)  = HPC(1)
!        LABEL(3)  = 'IR#M'; VALUE(3)  = FLOAT(NAP)
!        LABEL(4)  = 'IRCM'; VALUE(4)  = TOTIR
        LABEL(1)  = 'IR#M'; VALUE(1)  = FLOAT(NAP)
        LABEL(2)  = 'IRCM'; VALUE(2)  = TOTIR

        !Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************

      RETURN
      END !SUBROUTINE MGMTOPS

C=======================================================================

C=======================================================================
C  OpMgmt, Subroutine, C.H.Porter 
C  Generates output for Management Operations Module.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  08/01/2002 CHP Written
!  03/04/2003 CHP Disable routine for DS4 release - to be enabled when
!                   additional management operations can be added to
!                   output file (fertilizer, residue, tillage, etc.)
C-----------------------------------------------------------------------
C  Called from:   MgmtOps
C  Calls:         None
C=======================================================================
      SUBROUTINE OpMgmt(CONTROL, ISWITCH,
     &    IIRRI, NAP, NTIL, NCHEM, TOTIR, YRPLT)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  ISWCHE, IDETW, ISWWAT, ISWTIL, IIRRI
      CHARACTER*6,  PARAMETER :: ERRKEY = 'OPMGMT'
      CHARACTER*11, PARAMETER :: OUTM = 'MgmtOps.OUT'

      CHARACTER*30 FILEIO

      INTEGER DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP, DLUN
      INTEGER RUN, YEAR, YRDOY, YRPLT
      INTEGER NCHEM, NAP, NTIL, TIMDIF

      REAL TOTIR

      LOGICAL DPRINT, FEXIST

C-----------------------------------------------------------------------
C     Define constructed variable types based on definitions in
C     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      DAS     = CONTROL % DAS 
      DYNAMIC = CONTROL % DYNAMIC 
      FILEIO  = CONTROL % FILEIO  
      FROP    = CONTROL % FROP  

      RUN     = CONTROL % RUN    
      YRDOY   = CONTROL % YRDOY   

      ISWCHE  = ISWITCH % ISWCHE
      IDETW   = ISWITCH % IDETW
      ISWTIL  = ISWITCH % ISWTIL
      ISWWAT  = ISWITCH % ISWWAT

!***********************************************************************
!***********************************************************************
!     Input and Initialization
!***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
!-----------------------------------------------------------------------
C     Determine whether there will be anything to print out in
C         daily 'Managmnt.out' file.
C-----------------------------------------------------------------------
      DPRINT = .FALSE.

!     If Chemical apps, trigger printout      
      IF (ISWCHE .EQ. 'Y' .AND. NCHEM .GT. 0) DPRINT = .TRUE. 

!     If irrigation apps, trigger printout  
      IF (IDETW .EQ. 'Y' .AND. ISWWAT .EQ. 'Y' .AND. 
     &          INDEX('AFPWRD',IIRRI) .GT. 0) DPRINT = .TRUE.

!     If Tillage apps, trigger printout  
      IF (ISWTIL .EQ. 'Y' .AND. NTIL .GT. 0)  DPRINT = .TRUE.

!     If daily printout is needed, open file.
      IF (DPRINT) THEN
        INQUIRE (FILE = OUTM, EXIST = FEXIST)
        IF (FEXIST) THEN      
          !MgmtOps.out file has already been created for this run.
          OPEN (UNIT=DLUN, FILE=OUTM, STATUS='OLD',
     &      IOSTAT = ERRNUM, POSITION='APPEND')
        ELSE                  
!         Get unit number for daily and seasonal irrigation output files
          CALL GETLUN('OUTM', DLUN)
          OPEN (UNIT=DLUN, FILE=OUTM, STATUS='NEW',
     &      IOSTAT = ERRNUM)
          WRITE(DLUN,'("*MANAGEMENT OPERATIONS DAILY OUTPUT FILE")')
        ENDIF

        CALL HEADER(SEASINIT, FILEIO, DLUN, RUN)
        WRITE(DLUN,150)
  150   FORMAT('@YEAR DOY   DAS   DAP  IR#C  IRRC')
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
C  Generate output for file MgmtOps.OUT
C-----------------------------------------------------------------------
      IF (DPRINT) THEN
        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS,FROP) .EQ. 0) .OR.
     &      (DYNAMIC .EQ. FINAL  .AND. MOD(DAS,FROP) .NE. 0)) THEN

          IF (YRPLT .GT. 0) THEN
            DAP = MAX(0,TIMDIF(YRPLT,YRDOY))
          ELSE
            DAP = 0
          ENDIF
          CALL YR_DOY(YRDOY, YEAR, DOY) 

          !Print daily output
          WRITE(DLUN,'(1X,I4,1X,I3.3,4(1X,I5))') 
     &        YEAR, DOY, DAS, DAP, NAP, NINT(TOTIR)
        ENDIF

        IF (DYNAMIC .EQ. FINAL) THEN
          !Close daily output file
          CLOSE (DLUN)
        ENDIF
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OpMgmt
!***********************************************************************
