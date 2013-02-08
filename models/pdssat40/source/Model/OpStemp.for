C=======================================================================
C  OPSTEMP, Subroutine, C.H.Porter 
C  Generates output for daily soil temperature data
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  11/01/2001 CHP Written
C  06/07/2002 GH  Modified for crop rotations
C-----------------------------------------------------------------------
C  Called from:   STEMP
C  Calls:         None
C=======================================================================
      SUBROUTINE OPSTEMP(CONTROL, ISWITCH, DOY, ST)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1  RNMODE, IDETW
      CHARACTER*12 OUTT
      CHARACTER*30 FILEIO

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I
      INTEGER NOUTDT, RUN, YEAR, YRDOY, REPNO
      REAL ST(NL)

      LOGICAL FEXIST

!-----------------------------------------------------------------------
!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
!-----------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

      IDETW   = ISWITCH % IDETW
      IF (IDETW .NE. 'Y') RETURN

      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FROP    = CONTROL % FROP
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN

      CALL GETLUN('OUTT',NOUTDT)
!     Open the output files
      OUTT = 'SoilTemp.OUT'
      INQUIRE (FILE = OUTT, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT=NOUTDT, FILE=OUTT, STATUS='OLD',
     &    IOSTAT = ERRNUM, POSITION='APPEND')
        !IF (RNMODE .NE. 'Q') THEN
        !ENDIF
      ELSE
        OPEN (UNIT=NOUTDT, FILE=OUTT, STATUS='NEW',
     &    IOSTAT = ERRNUM)
 !      Write headers info to daily output file
        WRITE(NOUTDT,'("*SOIL TEMPERATURE OUTPUT FILE (DAILY)")')
      ENDIF

C-----------------------------------------------------------------------
C     Variable heading for SoilTemp.OUT
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN

        !For first run of a sequenced run, use replicate
        ! number instead of run number in header.
        IF (RNMODE .EQ. 'Q') THEN
          CALL HEADER(SEASINIT, FILEIO, NOUTDT, REPNO)
        ELSE
          CALL HEADER(SEASINIT, FILEIO, NOUTDT, RUN)
        ENDIF

        WRITE (NOUTDT,120)
  120   FORMAT('@YEAR DOY   DAS',
     &  '  TS1D  TS2D  TS3D  TS4D  TS5D  TS6D  TS7D  TS8D  TS9D  TS10')
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily Output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
      IF (MOD(DAS, FROP) .EQ. 0) THEN
C       Generate output for file SoilTemp.OUT
        CALL YR_DOY(YRDOY, YEAR, DOY)
        WRITE (NOUTDT,300) YEAR, DOY, DAS, (ST(I),I=1,10)
  300   FORMAT(1X,I4,1X,I3.3,1X,I5,10(1X,F5.1))
      ENDIF

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
      CLOSE (NOUTDT)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE OPSTEMP
!***********************************************************************
