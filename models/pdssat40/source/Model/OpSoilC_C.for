C=======================================================================
C  OPSOILC_C, Subroutine, C.H.Porter 
C  Generates output for daily soil carbon data for Century routine.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  12/03/2002 CHP Written
!  12/01/2005 CHP Report total C for surface and soil as OCAM
!  12/12/2005 CHP Add OCTAM and ONTAM variables
C-----------------------------------------------------------------------
C  Called from:   Century
C  Calls:         None
C=======================================================================

      SUBROUTINE OPSOILC_C(CONTROL, ISWITCH, 
     &    CUMRES, CUMRESE, DSNC, NLAYR, SomLitC, SomLitE, 
     &    SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)
C-------------------------------------------------------------------
C
C  Soil Carbon Aspects OUTPUT File
C
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1  IDETC, RNMODE
      CHARACTER*9  OUTSC
      CHARACTER*30 FILEIO

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, L
      INTEGER NLAYR, NOUTDC, RUN, YEAR, YRDOY, REPNO

      REAL CUMRES, DSNC, INTERPOLATE
      REAL SCDD, SNDD, SOCD, SOND
      REAL TSOMC, TLITC
      REAL, DIMENSION(NELEM) :: CUMRESE, TSOME, TLITE
      REAL, DIMENSION(NL) :: HUMC, HUMN
      REAL, DIMENSION(0:NL) :: SSOMC, SomLitC
      REAL, DIMENSION(0:NL,NELEM) :: SSOME, SomLitE

      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 5
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      IDETC  = ISWITCH % IDETC

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for SoilC.OUT
C-----------------------------------------------------------------------
      IF (IDETC .EQ. 'Y') THEN
        OUTSC = 'SoilC.OUT'
        CALL GETLUN('OUTSC', NOUTDC)
        INQUIRE (FILE = OUTSC, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDC, FILE = OUTSC, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDC, FILE = OUTSC, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDC,'("*SOIL CARBON DAILY OUTPUT FILE")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, FILEIO, NOUTDC, REPNO)
          ELSE
            CALL HEADER(SEASINIT, FILEIO, NOUTDC, RUN)
          ENDIF

          WRITE (NOUTDC,200)
  200     FORMAT('@YEAR DOY   DAS',
     &  '    OMAC    SCDD    SOCD    SC0D    SCTD   SOMCT    LCTD',
     &  '    ONAC    SNDD    SOND    SN0D    SNTD   SOMNT    LNTD')
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      IF (IDETC == 'Y' .AND. MOD(DAS, FROP) == 0) THEN
        CALL YR_DOY(YRDOY, YEAR, DOY) 

        DO L = 1, NLAYR
          HUMC(L) = SSOMC(L)
          HUMN(L) = SSOME(L,1)  
        ENDDO
        SCDD = INTERPOLATE (HUMC, DSNC)
        SNDD = INTERPOLATE (HUMN, DSNC)

        SOCD = (SomLitC(0) + TSOMC + TLITC)   !/1000.
        SOND = SomLitE(0,1) + TSOME(1) + TLITE(1)

        WRITE(NOUTDC,400) YEAR, DOY, DAS,  
     &    NINT(CumRes), NINT(SCDD), NINT(SOCD), NINT(SomLitC(0)),
     &    NINT(TSOMC+TLITC), NINT(TSOMC), TLITC,
     &    CUMRESE(1), NINT(SNDD), NINT(SOND), SomLitE(0,1), 
     &    NINT(TSOME(1)+TLITE(1)), NINT(TSOME(1)), TLITE(1)
      ENDIF

  400 FORMAT(1X,I4,1X,I3.3,1X,I5,
     &    6I8, F8.1,
     &    F8.1, 2I8, F8.1, 2I8, F8.1)

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
!       Store Summary.out labels and values in arrays to send to
!       OPSUM routines for printing.  Integers are temporarily 
!       saved aS real numbers for placement in real array.
        LABEL(1)  = 'RECM '; VALUE(1)  = CUMRES
!        LABEL(2)  = 'OCAM'; VALUE(2)  = TSOMC/1000.
!        12/01/2005 Wageningen CHP Report total C surface and soil -- 
!          this is important for C sequestration studies.
!        LABEL(2)  = 'OCAM'; VALUE(2) = (TSOMC + TLITC +SOMLITC(0))/1000.
!       12/12/2005 CHP Add OCTAM and ONTAM variables
        LABEL(2)  = 'OCTAM'; VALUE(2) = SomLitC(0) + TSOMC + TLITC
        LABEL(3)  = 'OCAM '; VALUE(3) = TSOMC + TLITC 
        LABEL(4)  = 'ONTAM'; VALUE(4) = SomLitE(0,1) +TSOME(1) +TLITE(1)
        LABEL(5)  = 'ONAM '; VALUE(5) = TSOME(1) + TLITE(1) 

        !Send labels and values to OPSUM
        CALL SUMVALS (SUMNUM, LABEL, VALUE) 

      !Close daily output files.
        IF (IDETC .EQ. 'Y') CLOSE(NOUTDC)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPSOILC_C
C-------------------------------------------------------------------
C
