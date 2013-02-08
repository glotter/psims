C=======================================================================
C  OPSOILNC, Subroutine, C.H.Porter from Soil Nitrogen and Carbon 
C     portions of OPDAY
C  Generates output for daily soil Nitrogen and Carbon data
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  11/16/2001 CHP Written
C  06/07/2002 GH  Modified for crop rotations
C  08/20/2002 GH  Modified for Y2K
!  12/01/2005 CHP Report total C for surface and soil as OCAM
!  12/12/2005 CHP Add OCTAM and ONTAM variables
!                 Added surface residues to output.
C-----------------------------------------------------------------------
C  Called from:   NTRANS
C  Calls:         None
C=======================================================================

      SUBROUTINE OpSoilNC(CONTROL, ISWITCH, 
     &    AMTNIT, CUMRES, DSNC, HUMC, HUMN, NAPNIT, NH4, NO3, 
     &    RESLEFT, RESLEFTN,
     &    TFOM, TFON, THUMC, THUMN, TLCH, TNH4, TNH4NO3, TNO3, 
     &    TOTAML, TMINERALIZE, TIMMOBILIZE, TNITRIFY, TNOX)
C-------------------------------------------------------------------
C
C  Soil Nitrogen Aspects OUTPUT File
C  Soil Carbon Aspects OUTPUT File
C
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------

      CHARACTER*1  IDETC, IDETN, ISWNIT, ISWWAT, RNMODE
      CHARACTER*9  OUTSN, OUTSC
      CHARACTER*30 FILEIO

      INTEGER DAS, DOY, DYNAMIC, ERRNUM, FROP, I, REPNO
      INTEGER NAPNIT, NOUTDC, NOUTDN, RUN, YEAR, YRDOY

      REAL AMTNIT, CUMRES, DSNC, INTERPOLATE
      REAL RESLEFT, RESLEFTN
      REAL SCDD, SNDD, TFOM, TFON, TOTAML
      REAL THUMC, THUMN, TLCH, TNH4, TNH4NO3, TNO3
      REAL HUMC(NL), HUMN(NL), NO3(NL), NH4(NL) !, SNI(NL)
      REAL OCTAM, ONTAM

      LOGICAL FEXIST

!     Arrays which contain data for printing in SUMMARY.OUT file
      INTEGER, PARAMETER :: SUMNUM = 9
      CHARACTER*5, DIMENSION(SUMNUM) :: LABEL
      REAL, DIMENSION(SUMNUM) :: VALUE

      REAL TMINERALIZE    !Mineralization / day
      REAL TIMMOBILIZE    !Immobilization / day
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
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      IDETC  = ISWITCH % IDETC
      IDETN  = ISWITCH % IDETN
      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT

      IF (ISWWAT .EQ. 'N' .OR. ISWNIT .EQ. 'N') THEN
        RETURN
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Variable heading for SoilN.OUT
C-----------------------------------------------------------------------
      IF (IDETN .EQ. 'Y') THEN
        OUTSN = 'SoilN.OUT'
        CALL GETLUN('OUTSN', NOUTDN)
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
        ENDIF 
      ENDIF

C-----------------------------------------------------------------------
C     Variable heading for CARBON.OUT
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
  200     FORMAT('@YEAR DOY   DAS    SOCD   OMAC',
     &      '   TFOM   HUM1   HUM2   HUM3   HUM4  HUM5  TFON',
     &      '  NHU1  NHU2  NHU3  NHU4  NHU5  SNDD  SCDD')
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      OCTAM = THUMC + (TFOM + RESLEFT) * 0.4
      ONTAM = THUMN + TFON + RESLEFTN

      IF (MOD(DAS, FROP) .EQ. 0) THEN

        CALL YR_DOY(YRDOY, YEAR, DOY) 

        IF (IDETN .EQ. 'Y') THEN
          WRITE (NOUTDN,310) YEAR, DOY, DAS, NINT(AMTNIT), 
     &       NAPNIT, TLCH, TNH4NO3, NINT(THUMN), TNO3, TNH4, 
     &       (NO3(I),I=1,10), (NH4(I),I=1,10),
     &       TMINERALIZE, TNITRIFY, TNOX, TIMMOBILIZE, TOTAML
  310     FORMAT(1X,I4,1X,I3.3,3(1X,I5),1X,F7.1,1X,F6.1,I8,2F7.1,
     &       15(1X,F5.1),5(1X,F5.2), 5F8.2)
        ENDIF

        IF (IDETC .EQ. 'Y') THEN
          SNDD = INTERPOLATE (HUMN, DSNC)
          SCDD = INTERPOLATE (HUMC, DSNC)
          WRITE(NOUTDC,400) YEAR, DOY, DAS, NINT(THUMC),
     &        NINT(CUMRES), NINT(TFOM), (NINT(HUMC(I)),I=1,5), 
     &        NINT(TFON), (NINT(HUMN(I)),I=1,5), NINT(SNDD), NINT(SCDD)
  400     FORMAT(1X,I4,1X,I3.3,1X,I5,I8,6(1X,I6),17(1X,I5))
        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
      !Write end of season summary info for SUMMARY.OUT file
      !Scratch file has been opened by subroutine OPSUM, so
      !just need to retrieve correct unit number.
!      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
!         Store Summary.out labels and values in arrays to send to
!         OPSUM routines for printing.  Integers are temporarily 
!         saved aS real numbers for placement in real array.
          LABEL(1)  = 'NI#M '; VALUE(1)  = FLOAT(NAPNIT)
          LABEL(2)  = 'NICM '; VALUE(2)  = AMTNIT
          LABEL(3)  = 'NLCM '; VALUE(3)  = TLCH
          LABEL(4)  = 'NIAM '; VALUE(4)  = TNH4NO3
          LABEL(5)  = 'RECM '; VALUE(5)  = CUMRES
!          LABEL(6)  = 'ONAM'; VALUE(6)  = THUMN
!          LABEL(7)  = 'OCAM'; VALUE(7)  = THUMC/1000.
!         12/01/2005 Wageningen CHP Report total C surface
!             and soil -- this is important for C sequestration
!             studies.
!          LABEL(7)  = 'OCAM'; VALUE(7)  = (THUMC + TFOM)/1000.
!         12/12/2005 CHP Add OCTAM and ONTAM variables
          LABEL(6) = 'OCTAM'; VALUE(6) = OCTAM
          LABEL(7) = 'OCAM '; VALUE(7) = THUMC + (TFOM) * 0.4 
          LABEL(8) = 'ONTAM'; VALUE(8) = ONTAM
          LABEL(9) = 'ONAM '; VALUE(9) = THUMN + TFON
          !Send labels and values to OPSUM
          CALL SUMVALS (SUMNUM, LABEL, VALUE) 
!      ENDIF

      !Close daily output files.
      IF (IDETN .EQ. 'Y') CLOSE(NOUTDN)
      IF (IDETC .EQ. 'Y') CLOSE(NOUTDC)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE OpSoilNC
C-------------------------------------------------------------------
C
