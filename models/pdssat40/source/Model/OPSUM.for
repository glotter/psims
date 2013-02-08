C=======================================================================
C  OPSUM, Subroutine G. Hoogenboom, J. W. Jones
C  Generates output for seasonal data.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1990 GH  Written
C  11/02/1999 CHP Changed TSOC to THUMC, TSIN to THUMN, AMTRES to CUMRES 
C  07/01/2000 GH  Eliminated common block statements
C  01/09/2002 CHP Added SumModule to store SUMMARY.OUT variables
C  05/08/2003 CHP Added version and date-time stamp.
C  08/12/2003 CHP Added I/O error checking
C  09/10/2003 CHP Changed Evaluate.out headers to have "S" (simulated) 
C                    and "M" (measured) suffixes rather than "P" (predicted) 
C                    and "O" (observed)
C  01/04/2005 CHP Changed screen output to HWAH instead of HWAM
C  02/04/2005 CHP Added new variables to Summary.out: WSTA, SLNO, PWAM, LAIX,
C                   HIAM, EPCM, ESCM
!  08/12/2005 CHP Changed P variable headers
!  12/12/2005 CHP Add OCTAM and ONTAM variables
C=======================================================================

      MODULE SumModule
!     This module defines variables which are printed to SUMMARY.OUT file.

!     Data construct for summary.out data. Used only by SUMVAL and OPSUM.
      Type SummaryType
        INTEGER ADAT, MDAT, DWAP, CWAM
        INTEGER HWAM
        INTEGER HNUMAM, NFXM, NUCM, CNAM, GNAM
        INTEGER NAP, IRCM, ETCM
        INTEGER PRCM, ROCM, DRCM, SWXM
        INTEGER NINUMM, NICM, NLCM, NIAM, RECM, ONAM, OCAM
        INTEGER PINUMM, PICM, PUPC, SPAM
        REAL HWAH, HWUM, BWAH, HNUMUM 

!       Added 2/6/2005 for v4.0.2.0
        REAL LAIX, HIAM
        INTEGER PWAM, EPCM, ESCM

!       Added 12/12/2005 
        INTEGER OCTAM, ONTAM

      End Type SummaryType

      Integer, Parameter :: EvaluateNum = 40

      Type EvaluateType
        INTEGER COUNT
        CHARACTER*4,  DIMENSION(EvaluateNum) :: OLAP
        CHARACTER*8,  DIMENSION(EvaluateNum) :: Simulated, Measured
        CHARACTER*35, DIMENSION(EvaluateNum) :: DESCRIP
      End Type EvaluateType

      Type (SummaryType) SUMDAT
      Type (EvaluateType) EvaluateData

      End Module SumModule
C=======================================================================


C=======================================================================
      SUBROUTINE OPSUM (CONTROL, ISWITCH, YRPLT) 

C-----------------------------------------------------------------------
      USE SumModule     
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETL, IDETO, IDETS, RNMODE
      CHARACTER*2  CROP, CG, CR_LAST
      CHARACTER*6  SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPSUM '
      CHARACTER*8  EXPER, FLDNAM
      CHARACTER*12 OUTS, SEVAL
      PARAMETER (OUTS = 'Summary.OUT')
      CHARACTER*25 TITLET
      CHARACTER*30 FILEIO
      CHARACTER*60 ENAME

      INTEGER ADAT, CNAM, CRPNO, CWAM, DNR1, DNR7, DRCM, DWAP, DYNAMIC
      INTEGER ERRNUM, ETCM, FOUND, GNAM, HNUMAM, HWAM
      INTEGER I, IRCM, LUNIO, LINC, LNUM, MDAT, NAP, NINUMM
      INTEGER NFXM, NIAM, NICM, NLCM, NLINES  !, NNAPHO
      INTEGER NOUTDS, NUCM, NYRS, PRCM, RECM, ROCM, ONAM, OCAM
      INTEGER ROTNO, ROTOPT, RUN, SLUN, SWXM, TIMDIF, TRTNO, YRPLT
      INTEGER YRSIM, YRDOY
      INTEGER RUN2
      INTEGER PINUMM, PICM, PUPC, SPAM    !P data

      REAL BWAH, HNUMUM, HWAH, HWUM   !, HBPC, HPC

!     Added 2/6/2005 for v4.0.2.0
      REAL LAIX, HIAM
      INTEGER PWAM, EPCM, ESCM
      CHARACTER* 8 WSTA
      CHARACTER*10 SLNO

!     Added 12/12/2005 
      INTEGER OCTAM, ONTAM

      LOGICAL FEXIST

!     Evaluate.OUT variables:
      INTEGER COUNT   !Number of observations for this crop
      CHARACTER*4,  DIMENSION(EvaluateNum) :: OLAP    !Labels
      CHARACTER*8,  DIMENSION(EvaluateNum) :: Measured, Simulated
      CHARACTER*35, DIMENSION(EvaluateNum) :: DESCRIP !Descriptions

!     Date and time stamp variables
      INTEGER       DATE_TIME(8)
      CHARACTER*3   MON(12)
      DATA MON /'Jan','Feb','Mar','Apr','May','Jun','Jul'
     &         ,'Aug','Sep','Oct','Nov','Dec'/
      DATA CR_LAST /'  '/
!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      NYRS    = CONTROL % NYRS

      IDETS   = ISWITCH % IDETS
      IDETO   = ISWITCH % IDETO
      IDETL   = ISWITCH % IDETL

C***********************************************************************
C***********************************************************************
C     Run initialization - run once per simulation
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      date_time(1)  The 4-digit year  
!      date_time(2)  The month of the year  
!      date_time(3)  The day of the month  
!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  
      
      OPEN (UNIT=LUNIO, FILE = FILEIO, STATUS = 'OLD', 
     &  IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      LNUM = 0
C-----------------------------------------------------------------------
C      Find and Read Experimental Details Section
C-----------------------------------------------------------------------
      SECTION = '*EXP.D'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(3X,A8,1X,A2,1X,A60)',IOSTAT=ERRNUM) EXPER,CG,ENAME
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF
C-----------------------------------------------------------------------
C       Find and Read TREATMENTS Section
C-----------------------------------------------------------------------
      SECTION = '*TREAT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(I3,I2,2(1X,I1),1X,A25)',IOSTAT=ERRNUM) 
     &      TRTNO, ROTNO, ROTOPT, CRPNO, TITLET
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C       Find and read Cultivar Section
C-----------------------------------------------------------------------
      SECTION = '*CULTI'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(3X,A2)',IOSTAT=ERRNUM) CROP
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

C-----------------------------------------------------------------------
C    Find and read Field Section
C-----------------------------------------------------------------------
      SECTION = '*FIELD'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO,'(3X,A8,1X,A8,49X,A10)',IOSTAT=ERRNUM) 
     &        FLDNAM, WSTA, SLNO
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF

      CLOSE (LUNIO)

C     Get unit number for SUMMARY.OUT file
      CALL GETLUN('OUTS', NOUTDS)

!-----------------------------------------------------------------------
!     Get unit number for EVALUATE.OUT file
      SEVAL = 'Evaluate.OUT'
      CALL GETLUN('SEVAL', SLUN)

      EvaluateData % Simulated = '     -99'
      EvaluateData % Measured  = '     -99'

C***********************************************************************
C***********************************************************************
C     Seasonal initialization - run once per season
C***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
C     Initialize temporary file which will store variables needed by
C       OPSUM.  This file will be written to by various modules and
C       deleted upon closing.
C-----------------------------------------------------------------------
C     Initialize OPSUM variables.
      SUMDAT % ADAT = 0
      SUMDAT % MDAT = 0
      SUMDAT % DWAP = 0
      SUMDAT % CWAM = 0
      SUMDAT % HWAM = 0
      SUMDAT % HWUM = 0.0
      SUMDAT % NAP  = 0
      SUMDAT % IRCM = 0
      SUMDAT % PRCM = 0
      SUMDAT % ETCM = 0
      SUMDAT % ROCM = 0
      SUMDAT % DRCM = 0
      SUMDAT % SWXM = 0
      SUMDAT % NICM = 0
      SUMDAT % NFXM = 0
      SUMDAT % NUCM = 0
      SUMDAT % NLCM = 0
      SUMDAT % NIAM = 0
      SUMDAT % CNAM = 0
      SUMDAT % GNAM = 0
      SUMDAT % RECM = 0
      SUMDAT % OCTAM= 0
      SUMDAT % OCAM = 0
      SUMDAT % ONTAM= 0
      SUMDAT % ONAM = 0

      SUMDAT % HWAH = 0.0
      SUMDAT % BWAH = 0.0
      SUMDAT % HNUMUM = 0.0
      SUMDAT % HNUMAM = 0
      SUMDAT % NINUMM = 0

      SUMDAT % PWAM = 0
      SUMDAT % LAIX = 0.0
      SUMDAT % HIAM = 0.0
      SUMDAT % EPCM = 0
      SUMDAT % ESCM = 0

!     P data - CHP added 8/12/2005
      SUMDAT % PINUMM = 0
      SUMDAT % PICM = 0
      SUMDAT % PUPC = 0
      SUMDAT % SPAM = 0

!***********************************************************************
!***********************************************************************
!    Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
      ADAT = SUMDAT % ADAT    !Anthesis Date
      MDAT = SUMDAT % MDAT    !Physiological Maturity Date
      DWAP = SUMDAT % DWAP    !Planting Material Weight (kg/ha)
      CWAM = SUMDAT % CWAM    !Tops Weight at Maturity (kg/ha)
      HWAM = SUMDAT % HWAM    !Yield at Maturity (kg/ha)
      HWUM = SUMDAT % HWUM    !Unit Weight at Maturity (mg/unit)
      NAP  = SUMDAT % NAP     !Irrigation Applications (no.)
      IRCM = SUMDAT % IRCM    !Season Irrigation (mm)
      PRCM = SUMDAT % PRCM    !Season Precipitation (mm)
      ETCM = SUMDAT % ETCM    !Season Evapo-transpiration (mm)
      ROCM = SUMDAT % ROCM    !Season Surface Runoff (mm)
      DRCM = SUMDAT % DRCM    !Season Vertical Drainage (mm)
      SWXM = SUMDAT % SWXM    !Extractable Water at Maturity (mm)
      NICM = SUMDAT % NICM    !Inorganic N applied (kg N/ha)
      NFXM = SUMDAT % NFXM    !N Fixed (kg/ha)
      NUCM = SUMDAT % NUCM    !N uptake (kg/ha)
      NLCM = SUMDAT % NLCM    !N leached (kg/ha)
      NIAM = SUMDAT % NIAM    !Inorganic N at maturity (kg N/ha)
      CNAM = SUMDAT % CNAM    !Tops N at Maturity (kg/ha)
      GNAM = SUMDAT % GNAM    !Grain N at Maturity (kg/ha)
      RECM = SUMDAT % RECM    !Residue Applied (kg/ha)
      OCTAM= SUMDAT % OCTAM   !Organic C at maturity, soil & surf (kg/h)
      OCAM = SUMDAT % OCAM    !Organic soil C at maturity (kg/ha)
      ONTAM= SUMDAT % ONTAM   !Organic N at maturity, soil & surf (kg/h)
      ONAM = SUMDAT % ONAM    !Organic soil N at maturity (kg/ha)

!      HBPC = SUMDAT % HBPC    !Percent by-product
!      HPC  = SUMDAT % HPC     !Percent harvested
      HWAH = SUMDAT % HWAH    !Yield at Harvest (kg/ha)
      BWAH = SUMDAT % BWAH    !By-product (kg/ha)
      HNUMUM = SUMDAT % HNUMUM    !Number at Maturity (no./unit)
      HNUMAM = SUMDAT % HNUMAM    !Number at Maturity (no./m2)
      NINUMM = SUMDAT % NINUMM    !Nitrogen Applications (no.)

      PINUMM = SUMDAT % PINUMM    !Number of P applications   !PI#M
      PICM   = SUMDAT % PICM      !P applied (kg/ha)          !PICM
!     CHP changed this from "Tops P at maturity" to "cumul P uptake"
      PUPC   = SUMDAT % PUPC      !Cumul P uptake (kg[P]/ha)  !PUPC
      SPAM   = SUMDAT % SPAM      !Soil P at maturity (kg/ha) !SPAM

      PWAM = SUMDAT % PWAM 
      LAIX = SUMDAT % LAIX 
      HIAM = SUMDAT % HIAM 
      EPCM = SUMDAT % EPCM 
      ESCM = SUMDAT % ESCM 

C-------------------------------------------------------------------
C
C  Simulation Summary File
C
C-------------------------------------------------------------------
      IF (INDEX('AY',IDETS) .GT. 0) THEN
        INQUIRE (FILE = OUTS, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDS, FILE = OUTS, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDS, FILE = OUTS, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)

!         Version information stored in ModuleDefs.for
          WRITE (NOUTDS,300) EXPER,CG,ENAME,Version,
     &        Mon(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1), 
     &             DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  300     FORMAT (
     &    '*SUMMARY : ',A8,A2,1X,A60,1X,
     &    'DSSAT Cropping System Model Ver. ',I1,'.',I1,'.',I1,'.',I3.3,
     &    5X,A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2,//,
     &    '!IDENTIFIERS................ TREATMENT.......... ',
     &    'SITE INFORMATION............ ',
     &    'DATES..................................  ',
     &    'DRY WEIGHT, YIELD AND YIELD COMPONENTS................',
     &    '...............  ',
     &    'WATER...............................................  ',
     &    'NITROGEN......................................  ',
     &    'ORGANIC MATTER....................  ',
     &    'PHOSPHORUS............')

          WRITE (NOUTDS,400)
  400     FORMAT ('@   RUNNO   TRNO R# O# C# CR TNAM                ',
     & 'FNAM     WSTA.... SOIL_ID...  ',
     & '  SDAT    PDAT    ADAT    MDAT    HDAT',
     & '  DWAP  CWAM  HWAM  HWAH   BWAH  PWAM',
     & '    HWUM  H#AM    H#UM  HIAM  LAIX',
     & '  IR#M  IRCM  PRCM  ETCM  EPCM  ESCM  ROCM  DRCM  SWXM',
     & '  NI#M  NICM  NFXM  NUCM  NLCM  NIAM  CNAM  GNAM',
!     & '  RECM   ONAM  OCAM  PO#M  POCM  CPAM  SPAM')
!     & '  RECM   ONAM  OCAM  PI#M  PICM  PUPC  SPAM')   !chp 08/12/2005
!     & '  RECM   ONAM    OCAM  PI#M  PICM  PUPC  SPAM') !chp 11/29/2005
     & '  RECM  ONTAM   ONAM   OCTAM    OCAM',           !chp 12/12/2005
     & '  PI#M  PICM  PUPC  SPAM')
        ENDIF

        WRITE (NOUTDS,500) RUN, TRTNO, ROTNO, ROTOPT, CRPNO, 
     &    CROP, TITLET(1:19), FLDNAM, WSTA, SLNO,
     &    YRSIM, YRPLT, ADAT, MDAT, YRDOY, 
     &    DWAP, CWAM, HWAM, NINT(HWAH), NINT(BWAH*10.), PWAM, 
     &    HWUM, HNUMAM, HNUMUM, HIAM, LAIX, 
     &    NAP, IRCM, PRCM, ETCM, EPCM, ESCM, ROCM, DRCM, SWXM, 
     &    NINUMM, NICM, NFXM, NUCM, NLCM, NIAM, CNAM, GNAM, 
     &    RECM,    ! ONAM, OCAM, 
     &    ONTAM, ONAM, OCTAM, OCAM,
!     &    NNAPHO, NINT(TOTPH), NINT(TOTPUP), NINT(TPLEFT) !, NUCMF
     &    PINUMM, PICM, PUPC, SPAM        !P data

  500  FORMAT (I9,1X,I6,3(I3),
     &  1X,A2,1X,A19,1X,A8,1X,A8,1X,A10,
     &  5(1X,I7),4(1X,I5),1X,I6,1X,I5,
     &  1X,F7.4,1X,I5,1X,F7.1, F6.2, F6.1,
!     &  18(1X,I5),1X,I6,F8.3,4(1X,I5))
     &  18(1X,I5),2(1X,I6),2(1X,I7),4(1X,I5))

        CLOSE (NOUTDS)
      ENDIF
C-------------------------------------------------------------------
C     Screen output for multi-season runs:
C     Was OPBAT subroutine
C-------------------------------------------------------------------
      IF (INDEX('NQSABCGF',RNMODE) .GT. 0 .OR. NYRS .GT. 1) THEN
          NLINES = RUN - 1
        IF (RUN .EQ. 1) THEN
          CALL CLEAR
        ENDIF
        NLINES = MOD(NLINES,20)
        IF (NLINES .EQ. 0) THEN
          IF (RUN .LT. 1000) THEN
            WRITE(*,600)
            WRITE(*,610)
          ELSE
            RUN2 = (RUN / 1000) * 1000
            WRITE(*,602)
            WRITE(*,612) RUN2
          ENDIF
        ENDIF
  600 FORMAT('RUN    TRT FLO MAT TOPWT SEEDW  RAIN  TIRR',
     &                    '   CET  PESW  TNUP  TNLF   TSON TSOC')
  602 FORMAT('RUN+   TRT FLO MAT TOPWT SEEDW  RAIN  TIRR',
     &                    '   CET  PESW  TNUP  TNLF   TSON TSOC')
  610 FORMAT('           dap dap kg/ha kg/ha    mm    mm    mm    mm',
     &                                     ' kg/ha kg/ha  kg/ha t/ha')
  612 FORMAT (I5, '      dap dap kg/ha kg/ha    mm    mm    mm    mm',
     &                                     ' kg/ha kg/ha  kg/ha t/ha')

        DNR1 = TIMDIF(YRPLT, ADAT)
        IF (DNR1 .LT. 0 .OR. YRPLT .LT. 0 .OR. DNR1 .GT. 999) THEN
          DNR1 = -99
        ENDIF
        DNR7 = TIMDIF(YRPLT, MDAT)
        IF (DNR7 .LT. 0 .OR. YRPLT .LT. 0 .OR. DNR7 .GT. 999) THEN
          DNR7 = -99
        ENDIF
        WRITE (*,650) MOD(RUN,1000), CROP, MOD(TRTNO,1000),
!CHP &      DNR1, DNR7, CWAM, HWAM, PRCM, IRCM, ETCM, SWXM, NUCM, 
     &      DNR1, DNR7, CWAM, NINT(HWAH), PRCM, IRCM, ETCM, SWXM, NUCM, 
!     &      NIAM, ONAM, NINT(OCAM)
     &      NIAM, ONAM, NINT(OCAM/1000.)
  650   FORMAT(I3,1X,A2,I4,2(1X,I3),8(I6),I7,I5)
        NLINES=NLINES+1

      ENDIF

C-------------------------------------------------------------------
!     Write Evaluate.OUT file
      IF((IDETL .NE. 'N' .OR. INDEX('IAEBCGD',RNMODE) .GT. 0) .AND.
     &        (CROP .NE. 'WH' .AND. CROP .NE. 'BA')) THEN

        COUNT     = EvaluateData % COUNT
        OLAP      = EvaluateData % OLAP
        DESCRIP   = EvaluateData % DESCRIP
        Simulated = EvaluateData % Simulated
        Measured  = EvaluateData % Measured

!       Open or create Evaluate.out file
        INQUIRE (FILE = SEVAL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = SLUN, FILE = SEVAL, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = SLUN, FILE = SEVAL, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          CALL DATE_AND_TIME (VALUES=DATE_TIME)
          WRITE (SLUN,700) EXPER, CG, ENAME, Version,
     &        Mon(DATE_TIME(2)), DATE_TIME(3), DATE_TIME(1), 
     &        DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
  700     FORMAT ('*EVALUATION : ',A8,A2,1X,A60,1X,
     &    'DSSAT Cropping System Model Ver. ',I1,'.',I1,'.',I1,'.',I3.3,
     &     5X,A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
        ENDIF

!       Write headers if new crop is being processed
        IF (CROP .NE. CR_LAST) THEN
          WRITE(SLUN,'(/,"@RUN EXCODE     TN RN CR",3X,A5,79(3X,A5))')   
     &        (OLAP(I)//"S", OLAP(I)//"M",I = 1, COUNT)
          CR_LAST = CROP
        ENDIF

!       Write evaluation data
        WRITE(SLUN,750) RUN, EXPER, CG, TRTNO, ROTNO, CROP, 
     &            (Simulated(I), Measured(I), I= 1,COUNT)
  750   FORMAT(I4,1X,A8,A2,I3,I3,1X,A2,80A8)
        CLOSE(SLUN)
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPSUM
C=======================================================================


C=======================================================================
C  SUMVALS, Subroutine C. H. Porter
C  Obtains and stores Summary.out values
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/09/2002 CHP Written
C=======================================================================
      SUBROUTINE SUMVALS (NUMBER, LABEL, VALUE) 

C-----------------------------------------------------------------------
      USE SumModule     
      IMPLICIT NONE

      INTEGER I, NUMBER
      CHARACTER*(*), DIMENSION(NUMBER) :: LABEL
      REAL, DIMENSION(NUMBER) :: VALUE

C-----------------------------------------------------------------------
      DO I = 1, NUMBER
        SELECT CASE(TRIM(LABEL(I)))

        !From OPHARV:
        CASE ('ADAT'); SUMDAT % ADAT   = NINT(VALUE(I))
        CASE ('MDAT'); SUMDAT % MDAT   = NINT(VALUE(I))
        CASE ('DWAP'); SUMDAT % DWAP   = NINT(VALUE(I))
        CASE ('CWAM'); SUMDAT % CWAM   = NINT(VALUE(I))
        CASE ('HWAM'); SUMDAT % HWAM   = NINT(VALUE(I))
        CASE ('HWAH'); SUMDAT % HWAH   = VALUE(I) !Float
        CASE ('BWAH'); SUMDAT % BWAH   = VALUE(I) !Float
        CASE ('HWUM'); SUMDAT % HWUM   = VALUE(I) !Float
        CASE ('H#AM'); SUMDAT % HNUMAM = NINT(VALUE(I))
        CASE ('H#UM'); SUMDAT % HNUMUM = VALUE(I) !Float
        CASE ('NFXM'); SUMDAT % NFXM   = NINT(VALUE(I))
        CASE ('NUCM'); SUMDAT % NUCM   = NINT(VALUE(I))
        CASE ('CNAM'); SUMDAT % CNAM   = NINT(VALUE(I))
        CASE ('GNAM'); SUMDAT % GNAM   = NINT(VALUE(I))
        CASE ('PWAM'); SUMDAT % PWAM   = NINT(VALUE(I)) 
        CASE ('LAIX'); SUMDAT % LAIX   = VALUE(I) !Float
        CASE ('HIAM'); SUMDAT % HIAM   = VALUE(I) !Float

        !From MgmtOps:
        CASE ('IR#M'); SUMDAT % NAP    = NINT(VALUE(I))
        CASE ('IRCM'); SUMDAT % IRCM   = NINT(VALUE(I))

        !From OPSPAM:
        CASE ('ETCM'); SUMDAT % ETCM = NINT(VALUE(I))
        CASE ('EPCM'); SUMDAT % EPCM = NINT(VALUE(I)) 
        CASE ('ESCM'); SUMDAT % ESCM = NINT(VALUE(I)) 

        !From OPWBAL:
        CASE ('PRCM'); SUMDAT % PRCM = NINT(VALUE(I))
        CASE ('ROCM'); SUMDAT % ROCM = NINT(VALUE(I))
        CASE ('DRCM'); SUMDAT % DRCM = NINT(VALUE(I))
        CASE ('SWXM'); SUMDAT % SWXM = NINT(VALUE(I))

        !From OpSoilNC:
        CASE ('NI#M'); SUMDAT % NINUMM = NINT(VALUE(I))
        CASE ('NICM'); SUMDAT % NICM   = NINT(VALUE(I))
        CASE ('NLCM'); SUMDAT % NLCM   = NINT(VALUE(I))
        CASE ('NIAM'); SUMDAT % NIAM   = NINT(VALUE(I))
        CASE ('RECM'); SUMDAT % RECM   = NINT(VALUE(I))
!        CASE ('ONAM'); SUMDAT % ONAM   = NINT(VALUE(I))
!        CASE ('OCAM'); SUMDAT % OCAM   = NINT(VALUE(I))
!        CASE ('OCAM'); SUMDAT % OCAM   = VALUE(I) !11/29/05

        CASE ('OCTAM');SUMDAT % OCTAM  = NINT(VALUE(I)) !12/12/05
        CASE ('OCAM'); SUMDAT % OCAM   = NINT(VALUE(I)) !12/12/05
        CASE ('ONTAM');SUMDAT % ONTAM  = NINT(VALUE(I)) !12/12/05
        CASE ('ONAM'); SUMDAT % ONAM   = NINT(VALUE(I)) !12/12/05

        !From OpSoilP_C
        CASE ('PI#M'); SUMDAT % PINUMM = NINT(VALUE(I))
        CASE ('PICM'); SUMDAT % PICM   = NINT(VALUE(I))
        CASE ('PUPC'); SUMDAT % PUPC   = NINT(VALUE(I))
        CASE ('SPAM'); SUMDAT % SPAM   = NINT(VALUE(I))

        END SELECT
      ENDDO

      RETURN
      END SUBROUTINE SUMVALS

C=======================================================================


C=======================================================================
C  EvaluateDat, Subroutine C. H. Porter
C  Obtains and stores Summary.out values
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/21/2002 CHP Written
C=======================================================================
      SUBROUTINE EvaluateDat (COUNT, Measured, Simulated, DESCRIP, OLAP) 

C-----------------------------------------------------------------------
      USE SumModule     
      IMPLICIT NONE

      INTEGER I, COUNT
      CHARACTER*4,  DIMENSION(EvaluateNum) :: OLAP
      CHARACTER*8,  DIMENSION(EvaluateNum) :: Measured, Simulated
      CHARACTER*35, DIMENSION(EvaluateNum) :: DESCRIP

C-----------------------------------------------------------------------
      EvaluateData % COUNT = MIN(COUNT, EvaluateNum)
      DO I = 1, COUNT
        EvaluateData % Measured(I)  = Measured(I)
        EvaluateData % Simulated(I) = Simulated(I)
        EvaluateData % DESCRIP(I)   = DESCRIP(I)
        EvaluateData % OLAP(I)      = OLAP(I)
      ENDDO

      RETURN
      END SUBROUTINE EvaluateDat

C=======================================================================
