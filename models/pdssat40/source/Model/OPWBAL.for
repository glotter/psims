C=======================================================================
C  OPWBAL, Subroutine, C.H.Porter from Soil Water portions of OPDAY
C  Generates output for daily soil water data
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  04/13/1998 CHP Written
C  06/19/2001 GH  Modified output format
C  08/20/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Called from:   WATBAL
C  Calls:         None
C=======================================================================
      SUBROUTINE OPWBAL(CONTROL, ISWITCH, 
     &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL,             !Input
     &    NLAYR, SW, TDRAIN, TRUNOF, WTDEP)               !Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE FloodModule    ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1 IDETW, ISWWAT, RNMODE
      CHARACTER*12 OUTWAT
      PARAMETER (OUTWAT = 'SoilWat.OUT ')
      CHARACTER*30 FILEIO

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I
      INTEGER L, NAVWB, NAP, NLAYR, NOUTDW, RUN
      INTEGER YEAR, YRDOY, REPNO

      REAL AVWTD, CRAIN, IRRAMT, PESW, TDRAIN, TLL
      REAL TOTBUNDRO, TOTIR, TRUNOF, TSW, WTDEP
      REAL, DIMENSION(NL) :: DLAYR, LL, SW

      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 4
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH
      TYPE (FloodWatType) FLOODWAT

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      YRDOY   = CONTROL % YRDOY

      IDETW   = ISWITCH % IDETW
      ISWWAT  = ISWITCH % ISWWAT

      TOTBUNDRO = FLOODWAT % TOTBUNDRO

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .AND. IDETW .NE. 'N') THEN
!-----------------------------------------------------------------------
C   Generate headings for output file
C-----------------------------------------------------------------------
      CALL GETLUN('OUTWAT', NOUTDW)
      INQUIRE (FILE = OUTWAT, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTDW, FILE = OUTWAT, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTDW, FILE = OUTWAT, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(NOUTDW,'("*SOIL WATER DAILY OUTPUT FILE")')
      ENDIF

C-----------------------------------------------------------------------
C     Variable heading for WATER.OUT
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, FILEIO, NOUTDW, REPNO)
        ELSE
          CALL HEADER(SEASINIT, FILEIO, NOUTDW, RUN)
        ENDIF
        WRITE (NOUTDW,1120)
 1120   FORMAT('@YEAR DOY   DAS',
     &    '  SWXD   ROFC   DRNC   PREC  IR#C  IRRC  DTWT   SW1D   SW2D',
     &    '   SW3D   SW4D   SW5D   SW6D   SW7D   SW8D   SW9D   SW10')
      ENDIF

C-----------------------------------------------------------------------
C   Set initial values to calculate average values
C-----------------------------------------------------------------------
      NAVWB = 0
      AVWTD = 0.
      TOTIR = 0.
      NAP   = 0

!***********************************************************************
!***********************************************************************
!     Daily integration
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
C-----------------------------------------------------------------------
C   Summations for calculation of average values per print interval
C-----------------------------------------------------------------------
      NAVWB  = NAVWB  + 1
      AVWTD  = AVWTD  + WTDEP
      
      IF (IRRAMT .GT. 0.0) THEN
        TOTIR = TOTIR + IRRAMT
        NAP  = NAP + 1
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
      TLL = 0.0
      TSW = 0.0
      DO L = 1, NLAYR
        TLL = TLL + LL(L) * DLAYR(L)
        TSW = TSW + SW(L) * DLAYR(L)
      ENDDO
      PESW = MAX(0.0, TSW - TLL)

C   Calculate average values as a function of the output interval
C-----------------------------------------------------------------------
      IF (IDETW .EQ. 'Y' .AND. ISWWAT .EQ. 'Y') THEN
 
C-----------------------------------------------------------------------
C  Generate output for file WATER.OUT
C-----------------------------------------------------------------------
        !Print every FROP days.
        IF ((DYNAMIC .EQ. OUTPUT .AND. MOD(DAS, FROP) .EQ. 0) .OR. 
        !Also print on last day if not already done.
     &      (DYNAMIC .EQ. FINAL  .AND. MOD(DAS, FROP) .NE. 0)) THEN

          CALL YR_DOY(YRDOY, YEAR, DOY) 
          AVWTD = AVWTD / NAVWB

          WRITE (NOUTDW,1300)YEAR, DOY,MOD(DAS,100000), 
     &      NINT(PESW*10),NINT(TRUNOF),NINT(TDRAIN),NINT(CRAIN),
     &      NAP, NINT(TOTIR),
     &      NINT(AVWTD),(SW(I),I=1,10)
 1300       FORMAT(1X,I4,1X,I3.3,2(1X,I5),3(1X,I6),3(1X,I5),
     &      10(1X,F6.3))

          NAVWB = 0
          AVWTD = 0.
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     FINAL - Sesaonal Output
!***********************************************************************
        IF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
          !IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!           Store Summary.out labels and values in arrays to send to
!           OPSUM routines for printing.  Integers are temporarily 
!           saved aS real numbers for placement in real array.
            LABEL(1)  = 'PRCM'; VALUE(1)  = CRAIN
            LABEL(2)  = 'ROCM'; VALUE(2)  = TRUNOF + TOTBUNDRO
            LABEL(3)  = 'DRCM'; VALUE(3)  = TDRAIN
            LABEL(4)  = 'SWXM'; VALUE(4)  = PESW*10.

            !Send labels and values to OPSUM
            CALL SUMVALS (SUMNUM, LABEL, VALUE) 

            !Close daily output files.
            CLOSE (NOUTDW)
          !ENDIF
        ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE OPWBAL
!***********************************************************************
!-----------------------------------------------------------------------
!     OPWBAL VARIABLE DEFINITIONS:  updated 2/19/2004
!-----------------------------------------------------------------------
! AVWTD     Average water table depth since last printout (cm)
! CONTROL   Composite variable containing variables related to control 
!             and/or timing of simulation.    See Appendix A. 
! CRAIN     Cumulative precipitation (mm)
! DAS       Days after start of simulation (d)
! DLAYR(L)  Thickness of soil layer L (cm)
! DOY       Current day of simulation (d)
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or FINAL 
! ERRNUM    Error number for input 
! FEXIST    Logical variable 
! FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
! FLOODWAT  Composite variable containing information related to bund 
!             management. Structure of variable is defined in ModuleDefs.for. 
! FROP      Frequency of output (d)
! IDETW     Y=detailed water balance output, N=no detailed output 
! IRRAMT    Irrigation amount for today (mm / d)
! ISWITCH   Composite variable containing switches which control flow of 
!             execution for model.  The structure of the variable 
!             (SwitchType) is defined in ModuleDefs.for. 
! ISWWAT    Water simulation control switch (Y or N) 
! LABEL(I)  Array of labels for variables sent to OPSUM for summary 
!             printout; corresponds to VALUE array which stores values of 
!             variables being sent. 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!            (cm3 [water] / cm3 [soil])
! NAP       Number of irrigation applications 
! NAVWB     Number of days since last printout (d)
! NLAYR     Actual number of soil layers 
! NOUTDW    Unit number for water balance output file 
! PESW      Potential extractable soil water (= SW - LL) summed over root 
!             depth (cm)
! REPNO     Replication number for current simulation 
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RUN       Change in date between two observations for linear 
!             interpolation 
! SW(L)     Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! TDRAIN    Cumulative daily drainage from profile (mm)
! TLL       Total soil water in the profile at the lower limit of 
!             plant-extractable water (cm)
! TOTBUNDRO Cumulative seasonal flood runoff over bund (mm)
! TOTIR     Total seasonal irrigation (mm)
! TRUNOF    Cumulative runoff (mm)
! TSW       Total soil water in profile (cm)
! VALUE(I)  Array of values of variables sent to OPSUM for summary 
!             printout; corresponds to LABEL array which identifies 
!             variables being sent. (varies)
! WTDEP     Depth to water table (cm)
! YEAR      Year of current date of simulation 
! YRDOY     Current day of simulation (YYYYDDD)
!-----------------------------------------------------------------------
!     END OPWBAL Subroutine
!-----------------------------------------------------------------------
