C=======================================================================
C  WARNING, Subroutine, C.H.PORTER
C  Writes warning messages to Warning.OUT file
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/21/2002 CHP Written
C  09/03/2004 CHP Modified call to GETPUT_CONTROL 
C  03/22/2005 CHP Added option to suppress Warning.OUT messages with 
C                 IDETL = 0 (zero).
!  05/04/2005 CHP Added date to warning message.
C=======================================================================

      SUBROUTINE WARNING (COUNT, ERRKEY, MESSAGE)

!     FILEIO and RUN needed to generate header for WARNING.OUT file

      USE ModuleDefs
      IMPLICIT NONE
CFANG FOR WARNING.OUT
      SAVE
      CHARACTER*(*) ERRKEY
      CHARACTER*11, PARAMETER :: WarnOut = 'Warning.OUT'
      CHARACTER*1   IDETL
      CHARACTER*30  FILEIO
      CHARACTER*78  MESSAGE(*)

      INTEGER COUNT, DOY, I, LUN, OLDRUN, RUN, YEAR, YRDOY
      LOGICAL FIRST, FEXIST, FOPEN

      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH

      DATA FIRST /.TRUE./
      DATA OLDRUN /0/

!-----------------------------------------------------------------------
      CALL GETPUT_CONTROL('GET', CONTROL)
      FILEIO = CONTROL % FILEIO
      RUN    = CONTROL % RUN
      YRDOY  = CONTROL % YRDOY
      
      IF (INDEX(ERRKEY,'ENDRUN') <= 0) THEN
!       First time routine is called to print, open file.
!       File will remain open until program execution is stopped.
        IF (FIRST) THEN

!         Check for IDETL = '0' (zero) --> suppress output
          CALL GETPUT_ISWITCH('GET', ISWITCH)
          IDETL = ISWITCH % IDETL
          IF (IDETL == '0') RETURN

          CALL GETLUN('OUTWARN', LUN)
          INQUIRE (FILE = WarnOut, EXIST = FEXIST)
          IF (FEXIST) THEN
            INQUIRE (FILE = WarnOut, OPENED = FOPEN)
            IF (.NOT. FOPEN) THEN
              OPEN (UNIT=LUN, FILE=WarnOut, STATUS='OLD',
     &            POSITION='APPEND')
            ENDIF
          ELSE
            OPEN (UNIT=LUN, FILE=WarnOut, STATUS='NEW')
            WRITE(LUN,'("*WARNING DETAIL FILE")')
          ENDIF

          IF (INDEX(ERRKEY,'HEADER') <= 0 .AND.
     &        INDEX(ERRKEY,'CROPD')  <= 0 .AND.
     &        INDEX(ERRKEY,'INPUTX') <= 0) THEN
            WRITE(LUN,'(/,78("*"))')
            CALL HEADER(SEASINIT, FILEIO, LUN,RUN)
            FIRST = .FALSE.
            OLDRUN = RUN
          ENDIF
        ENDIF

!       Suppress Warning.OUT if IDETL = '0' (zero)
        IF (IDETL == '0') RETURN

        IF (COUNT > 0) THEN
          !Print header if this is a new run.
          !Suppress header if the error occurred in CROPD, which is 
          !  called by the header printing routine.
          IF (OLDRUN .NE. RUN .AND. RUN .NE. 0 .AND. FILEIO .NE. "")THEN
            IF (INDEX(ERRKEY,'HEADER') <= 0 .AND.
     &          INDEX(ERRKEY,'CROPD')  <= 0 .AND.
     &          INDEX(ERRKEY,'INPUTX') <= 0) THEN
              CALL HEADER(SEASINIT,FILEIO,LUN,RUN)
            ENDIF
          ENDIF
          OLDRUN = RUN

!         Print the warning.  Message is sent from calling routine as text.
          CALL YR_DOY(YRDOY, YEAR, DOY)
          WRITE(LUN,'(/,1X,A,"  YEAR DOY = ",I4,1X,I3)')ERRKEY,YEAR,DOY
          DO I = 1, COUNT
            WRITE(LUN,'(1X,A78)') MESSAGE(I)
          ENDDO
  !        WRITE(LUN, '(/)')
        ENDIF

      ELSE    !ERRKEY = 'ENDRUN' -> End of season
        FIRST = .TRUE.
        CLOSE(LUN)
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE WARNING
