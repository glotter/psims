!***********************************************************************
!  OpSoilN_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Produces the soil nitrogen output file.
!
!  REVISION HISTORY
!  ........ ...  Written.
! 08/20/2002 GH  Modified for Y2K
!
!  Called : CENTURY
!  Calls  : 
!***********************************************************************

      SUBROUTINE OpSoilN_C(CONTROL, ISWITCH, 
     &    AMTNIT, NAPNIT, NH4, NO3, TLCH, TNH4, 
     &    TNH4NO3, TNO3, TSOME, 
     &    TNOX, TIMMOBILIZE, TMINERALIZE, TNITRIFY)
!-------------------------------------------------------------------
!
!  Soil Nitrogen Aspects OUTPUT File
!
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1 IDETN, ISWNIT, ISWWAT, RNMODE
      CHARACTER*9  OUTSN
      CHARACTER*30 FILEIO

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I
      INTEGER NAPNIT, NOUTDN, RUN, YEAR, YRDOY, REPNO

      REAL AMTNIT
      REAL TLCH, TNH4, TNH4NO3, TNO3
      REAL NO3(NL), NH4(NL)
      REAL TSOME(3)

      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 4
      CHARACTER*4, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

      REAL TMINERALIZE    !Mineralization / day
      REAL TIMMOBILIZE    !Immobilizatino / day
      REAL TNITRIFY       !Nitrification / day
      REAL TNOX           !Denitrification / day

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY

      IDETN  = ISWITCH % IDETN
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      IF (ISWWAT .EQ. 'N' .OR. ISWNIT .EQ. 'N') RETURN

!***********************************************************************
!***********************************************************************
!     Run initialization - run once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN
        OUTSN = 'SoilN.OUT'
        CALL GETLUN('OUTSN', NOUTDN)
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Variable heading for SoilN.OUT
!-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN
        INQUIRE (FILE = OUTSN, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDN, FILE = OUTSN, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDN, FILE = OUTSN, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDN,'("*SOIL NITROGEN DAILY OUTPUT FILE")')
        ENDIF

        IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
          !For first run of a sequenced run, use replicate
          ! number instead of run number in header.
          IF (RNMODE .EQ. 'Q') THEN
            CALL HEADER(SEASINIT, FILEIO, NOUTDN, REPNO)
          ELSE
            CALL HEADER(SEASINIT, FILEIO, NOUTDN, RUN)
          ENDIF

          WRITE (NOUTDN,100)
  100     FORMAT('@YEAR DOY   DAS',
     &   '  NAPC  NI#M    NLCC   NIAD    NOAD   NITD   NHTD',
     &   '  NI1D  NI2D  NI3D  NI4D  NI5D  NI6D  NI7D  NI8D  NI9D  NI10',
     &   '  NH1D  NH2D  NH3D  NH4D  NH5D  NH6D  NH7D  NH8D  NH9D  NH10',
     &   '    NMNC    NITC    NDNC    NIMC    AMLC')

        CALL YR_DOY(YRDOY, YEAR, DOY) 
          WRITE (NOUTDN,310) YEAR, DOY-1, -1, NINT(AMTNIT), 
     &       NAPNIT, TLCH, TNH4NO3, NINT(TSOME(1)), TNO3, TNH4, 
     &       (NO3(I),I=1,10), (NH4(I),I=1,10),
     &       TMINERALIZE, TNITRIFY, TNOX, TIMMOBILIZE, 0.0
        ENDIF 
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (MOD(DAS, FROP) .EQ. 0 .AND. IDETN .EQ. 'Y') THEN
        CALL YR_DOY(YRDOY, YEAR, DOY) 
        WRITE (NOUTDN,310) YEAR, DOY, DAS, NINT(AMTNIT), 
     &       NAPNIT, TLCH, TNH4NO3, NINT(TSOME(1)), TNO3, TNH4, 
     &       (NO3(I),I=1,10), (NH4(I),I=1,10),
     &       TMINERALIZE, TNITRIFY, TNOX, TIMMOBILIZE, 0.0
  310   FORMAT(1X,I4,1X,I3.3,3(1X,I5),1X,F7.1,1X,F6.1,I8,2F7.1,
     &       15(1X,F5.1),5(1X,F5.2), 5F8.2)
      ENDIF

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
      !Also print on last day if not already done.
      IF (MOD(DAS, FROP) .NE. 0 .AND. IDETN .EQ. 'Y') THEN
        CALL YR_DOY(YRDOY, YEAR, DOY) 
          WRITE (NOUTDN,310) YEAR, DOY, DAS, NINT(AMTNIT), 
     &       NAPNIT, TLCH, TNH4NO3, NINT(TSOME(1)), TNO3, TNH4, 
     &       (NO3(I),I=1,10), (NH4(I),I=1,10),
     &       TMINERALIZE, TNITRIFY, TNOX, TIMMOBILIZE, 0.0
      ENDIF

      !Write end of season summary info for SUMMARY.OUT file
      !Scratch file has been opened by subroutine OPSUM, so
      !just need to retrieve correct unit number.
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!         Store Summary.out labels and values in arrays to send to
!         OPSUM routines for printing.  Integers are temporarily 
!         saved aS real numbers for placement in real array.
          LABEL(1)  = 'NI#M'; VALUE(1)  = FLOAT(NAPNIT)
          LABEL(2)  = 'NICM'; VALUE(2)  = AMTNIT
          LABEL(3)  = 'NLCM'; VALUE(3)  = TLCH
          LABEL(4)  = 'NIAM'; VALUE(4)  = TNH4NO3
!          LABEL(5)  = 'RECM'; VALUE(5)  = CUMRES
!          LABEL(5)  = 'ONAM'; VALUE(5)  = TSOME(1)
!          LABEL(7)  = 'OCAM'; VALUE(7)  = TSOMC/1000.

          !Send labels and values to OPSUM
          CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

      !Close daily output files.
      IF (IDETN .EQ. 'Y') CLOSE(NOUTDN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE OpSoilN_C
!-------------------------------------------------------------------

