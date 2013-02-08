C=======================================================================
C  TILLAGE, Subroutine, C.H. Porter
C  Tillage routine reads tillage parameters.
C  Will be replaced by a functioning tillage module in upcoming versions.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/17/2001 CHP  Written
C  04/16/2002 GH   Modified for sequence analysis
C  08/01/2002 CHP  Merged RUNINIT and SEASINIT into INIT section
C  08/20/2002 GH   Modified for Y2K
C  08/12/2003 CHP  Added I/O error checking
C-----------------------------------------------------------------------
C  Called : Main
C  Calls  : 
C=======================================================================

      SUBROUTINE TILLAGE(CONTROL, NTIL, TILLDATE, TILLDEP, TILLMIX)

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1 RNMODE
      CHARACTER*5 TIMPL(NAPPL)
      CHARACTER*6 ERRKEY, SECTION
      PARAMETER (ERRKEY = 'TILLAG')
      CHARACTER*30 FILEIO
      CHARACTER*90 CHAR

      INTEGER DYNAMIC, ERRNUM, FOUND, I, IDATE, LNUM
      INTEGER MULTI, NTIL, YR, YRDIF, YRSIM
      INTEGER TILLDATE(NAPPL)
      INTEGER LUNIO

      REAL TILLDEP(NAPPL), TILLMIX(NAPPL)

!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      YRSIM   = CONTROL % YRSIM
      YRDIF   = CONTROL % YRDIF
      RNMODE  = CONTROL % RNMODE

C***********************************************************************
C***********************************************************************
C    Input and Initialization 
C***********************************************************************
      IF (DYNAMIC .EQ. INIT) THEN
C-----------------------------------------------------------------------
C     Read FILEIO
C-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

C-----------------------------------------------------------------------
C     Read Tillage Section
C-----------------------------------------------------------------------
      SECTION = '*TILLA'
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        NTIL  = 0
        DO I = 1,NAPPL
!          READ(LUNIO,3106,IOSTAT=ERRNUM,ERR=3107)
!     &        TILLDATE(I), TIMPL(I), TDEP(I)     !TDEP not used
! 3106     FORMAT(3X,I7,1X,A5,1X,F5.0)
          READ(LUNIO,'(3X,I7,1X,A90)',ERR=30,END=30) TILLDATE(I), CHAR
          LNUM = LNUM + 1

          READ(CHAR,'(A5,1X,F5.0)',IOSTAT=ERRNUM) TIMPL(I), TILLDEP(I) 
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)
          TILLMIX(I) = 100.   !Mixing percentage
          NTIL  = NTIL  + 1
        ENDDO
   30   CONTINUE
      ENDIF

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'Q' .AND. TILLDATE(1) .LT. YRSIM) THEN
        DO I = 1, NTIL
          CALL YR_DOY(TILLDATE(I),YR,IDATE)
          TILLDATE(I) = (YR + YRDIF) * 1000 + IDATE
        END DO
      ENDIF

C-----------------------------------------------------------------------
C     Adjust for multi year runs
C-----------------------------------------------------------------------
      IF (MULTI .GT. 1) THEN
        DO I = 1, NTIL
          CALL YR_DOY(TILLDATE(I),YR,IDATE)
          TILLDATE(I) = (YR + MULTI - 1) * 1000 + IDATE
        END DO
      ENDIF

C***********************************************************************
C***********************************************************************
C     END OF DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C***********************************************************************
      RETURN

      END !SUBROUTINE TILLAGE

!========================================================================
! Tillage variable definitiions
!========================================================================
! DYNAMIC     Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!               INTEGR, OUTPUT, or FINAL 
! ERRKEY      Subroutine name for error file 
! ERRNUM      Error number for input 
! FILEIO      Filename for input file (e.g., IBSNAT35.INP) 
! FOUND       Indicator that good data was read from file by subroutine 
!               FIND (0 - End-of-file encountered, 1 - NAME was found) 
! ISECT       Indicator of completion of IGNORE routine: 0 - End of file 
!               encountered, 1 - Found a good line to read, 2 - End of 
!               Section in file encountered denoted by * in column 1.  
! LNUM        Current line number of input file 
! LUNIO       Logical unit number for FILEIO 
! SECTION     Section name in input file 
!========================================================================
!     End TILLAGE module
!========================================================================
