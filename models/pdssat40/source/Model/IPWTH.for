C=======================================================================
C  IPWTH, Subroutine, N.B. Pickering, 08/30/91
C  Input weather data and check for errors.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/30/1991 NBP Written
C  07/01/1999 CHP Modified for modular format
C  09/02/1999 GH  Incorporated into CROPGRO
C  03/15/2000 GH  Modular model revisited
C  06/06/2002 GH  Modified for crop rotations
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added warning messages for use of default TAMP and TAV
C  08/12/2003 CHP Added I/O error checking
C  01/14/2005 CHP Modified checks for TAMP, TAV
C  02/08/2005 CHP Added check for weather file name when first day of
C                   a sequence occurs on Jan 1.
C-----------------------------------------------------------------------
C  Called by: WEATHR
C  Calls:     None
C=======================================================================

      SUBROUTINE IPWTH(CONTROL,
     &    CCO2, FILEW, MEWTH, PAR, PATHWT,                !Output
     &    RAIN, REFHT, RHUM, RSEED1, SRAD,                !Output
     &    TAMP, TAV, TDEW, TMAX, TMIN, WINDHT,            !Output
     &    WINDSP, XELEV, XLAT, XLONG,                     !Output
     &    DYNAMIC)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  BLANK, MEWTH, RNMODE
      CHARACTER*4  INSI
      CHARACTER*6  SECTION, ERRKEY
      CHARACTER*12 FILEW
      CHARACTER*30 FILEIO
!      CHARACTER*78 MESSAGE(10)
      CHARACTER*80 PATHWT, HDRFMT, LINE, RECFMT
      CHARACTER*92 FILEWW

      INTEGER ERRNUM,YRSIM,LINWTH,LUNWTH
      INTEGER TIMDIF,YRDOY,YRDOYW,YRDOYY,WYEAR,RUN
      INTEGER INCYD, YRSIMY, YRSIMMY, YRSIM2Y,YRSIMM2Y, YEAR, DOY
      INTEGER FOUND,PATHL,YRSIMM
      INTEGER LNUM, ERR, LUNIO, RSEED1
      INTEGER DYNAMIC
      INTEGER NYEAR, MULTI, YR, ISIM, YRDOYWY, WYR, NWYRS

      REAL
     &  XELEV,PAR,RAIN,REFHT,SRAD,TAV,TAMP,TDEW,TMAX,TMIN,WINDHT,
     &  WINDSP,XLAT,XLONG,CCO2,RHUM

      LOGICAL FEXIST, FIRST

      PARAMETER
     &  ( HDRFMT = '(2X,A4,2(1X,F8.3),1X,F5.0,4(1X,F5.1),1X,F5.0)',
     &    RECFMT = '(I5,8(1X,F5.0))',
     &    ERRKEY = 'IPWTH ')
      PARAMETER (BLANK = ' ')

C     The variable "CONTROL" is of constructed type "ControlType" as 
C     defined in ModuleDefs.for, and contains the following variables.
C     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL

      FILEIO  = CONTROL % FILEIO  
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI   
      RUN     = CONTROL % RUN    
      RNMODE  = CONTROL % RNMODE  
      YRDOY   = CONTROL % YRDOY   
      YRSIM   = CONTROL % YRSIM   

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
!-----------------------------------------------------------------------
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
      IF (RNMODE .NE. 'Q' .OR. RUN. EQ. 1) THEN
        OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
        READ (LUNIO,'(11(/),15X,A12,1X,A80)',IOSTAT=ERR) FILEW, PATHWT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,12)

        REWIND (LUNIO)
        SECTION = '*SIMUL'
        CALL FIND(LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEW,LNUM)
        READ(LUNIO,'(41X,I5)',IOSTAT=ERR) RSEED1
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM+1)
        READ (LUNIO,'(/,19X,A1)',IOSTAT=ERR) MEWTH
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM+3)

        CLOSE (LUNIO)

        CALL GETLUN('FILEW', LUNWTH)
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN

!-----------------------------------------------------------------------
      IF (MULTI .GT. 1) THEN
        OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)
        READ (LUNIO,'(11(/),15X,A12,1X,A80)',IOSTAT=ERR) FILEW, PATHWT
        IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,12)
        CLOSE (LUNIO)

        NYEAR = (ICHAR(FILEW(7:7)) - 48)*10 + (ICHAR(FILEW(8:8)) - 48)
        WYEAR = (ICHAR(FILEW(5:5)) - 48)*10 + (ICHAR(FILEW(6:6)) - 48)
        IF (NYEAR .EQ. 1) THEN
          PATHL  = INDEX(PATHWT,BLANK)
          WYEAR = MOD((WYEAR + MULTI - 1),100)
          WRITE(FILEW(5:6),'(I2.2)') WYEAR
          IF (PATHL .LE. 1) THEN
            FILEWW = FILEW
          ELSE
            FILEWW = PATHWT(1:(PATHL-1)) // FILEW
          ENDIF
          INQUIRE (FILE = FILEWW,EXIST = FEXIST)
          IF (.NOT. FEXIST) CALL ERROR (ERRKEY,1,FILEW,0)
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
C     First simulation day.  - Original IPWTH code
!-----------------------------------------------------------------------
C        YRSIMM = MOD(YRSIM,100000)
        YRSIMM = YRSIM
        CALL YR_DOY(YRSIM,YR,ISIM)
        IF (ISIM .GT. 1) THEN
          YRSIMY = INCYD(YRSIM,-1)
C         YRSIMMY = MOD(YRSIMY,100000)
          YRSIMMY = YRSIMY
        ELSE
          YRSIMY  = YRSIM
          YRSIMMY = YRSIMM
        ENDIF
        IF (ISIM .GT. 2) THEN
          YRSIM2Y  = INCYD(YRSIM,-2)
C         YRSIMM2Y = MOD(YRSIM2Y,100000)
          YRSIMM2Y = YRSIM2Y
        ELSE
          YRSIM2Y  = YRSIMY
          YRSIMM2Y = YRSIMMY
        ENDIF
         
!       This fixes a problem when the first day of a sequence is Jan 1.
        IF (INDEX('FQ',RNMODE) > 0 .AND. ISIM == 1) THEN
!         Sequenced run - check for first day of year -- may need to increment weather file
          READ(FILEW(5:6),'(I2.2)') WYEAR !Starting year in weather file
          READ(FILEW(7:8),'(I2.2)') NWYRS !Number of years in file
          CALL FULLYEAR(WYEAR*1000+1, WYR, DOY)
          IF (YR > WYR+NWYRS-1) THEN
            WYEAR = WYEAR + NWYRS
            WRITE(FILEW(5:6),'(I2.2)') MOD(WYEAR,100)
          ENDIF
        ENDIF

        LINWTH = 0
        PATHL  = INDEX(PATHWT,BLANK)
        IF (PATHL .LE. 1) THEN
          FILEWW = FILEW
        ELSE
          FILEWW = PATHWT(1:(PATHL-1)) // FILEW
        ENDIF
        OPEN (LUNWTH,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)

C       Read in weather file header.
  320   CONTINUE
        CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
        IF (FOUND .EQ. 2) GO TO 320
        IF (FOUND .EQ. 0) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
        READ (LINE,HDRFMT,IOSTAT=ERRNUM)
     &    INSI,XLAT,XLONG,XELEV,TAV,TAMP,REFHT,WINDHT,CCO2
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)

! 11/12/2005 CHP -- Move checks for TAV and TAMP to WEATHR routine
!         so they are available for weather generator routines also.
C       Substitute default values if TAV or TAMP are missing.  Write a
C         message to the WARNING.OUT file.
! 10/27/2005 CHP The checks for TAV and TAMP were being done in the 
!         STEMP routine, overriding this check. STEMP used .LE. instead 
!         of .LT. and the results were very different for some experiments 
!         which do not supply TAV and TAMP (UFMA8301.PNX, CLMO8501.SBX, 
!         NCCL8801.SBX, GALN0201.COX, others).
!         So -- leave LE for now.
!        IF (TAV  .LE. 0.0) THEN       
!!        IF (TAV  .LT. 0.0) THEN       
!          TAV = 20.0
!          WRITE(MESSAGE(1), 100)
!          WRITE(MESSAGE(2), 120) TAV
!          WRITE(MESSAGE(3), 130)
!          CALL WARNING (3, ERRKEY, MESSAGE)
!        ENDIF
!        IF (TAMP .LE. 0.0) THEN
!!        IF (TAMP .LT. 0.0) THEN
!          TAMP = 5.0
!          WRITE(MESSAGE(1), 110)
!          WRITE(MESSAGE(2), 120) TAMP
!          WRITE(MESSAGE(3), 130)
!          CALL WARNING (3, ERRKEY, MESSAGE)
!        ENDIF
!
!  100 FORMAT
!     &   ('Value of TAV, average annual soil temperature, is missing.')
!  110 FORMAT('Value of TAMP, amplitude of soil temperature function,',
!     &            ' is missing.')
!  120 FORMAT('A default value of', F5.1, 'ºC is being used for this',
!     &            ' simulation,')
!  130 FORMAT('which may produce undesirable results.')

C       Substitute default values if REFHT or WINDHT are missing.
        IF (REFHT .LE. 0.) REFHT = 1.5
        IF (WINDHT .LE. 0.) WINDHT = 2.0

C       Read in first weather record.
        CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
        IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
        READ (LINE,RECFMT,IOSTAT=ERRNUM) YRDOYW
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)
        CALL Y2K_DOYW(MULTI, RNMODE, YRDOYWY, YRDOYW)
!chp 9/21/2005        CALL Y2K_DOY(YRDOYW)
        YRDOYWY = YRDOYW
        IF (YRDOYW .GT. YRSIMM) THEN
          CALL ERROR(ERRKEY,1,FILEW,LINWTH)
        ENDIF
        CALL YR_DOY(YRDOYW, YEAR, DOY)
        IF (YRDOYW .NE. YRSIMM2Y .OR. DOY .LE. 2) THEN
          BACKSPACE(LUNWTH)
          LINWTH = LINWTH - 1
        ENDIF
C       BACKSPACE(LUNWTH)

C       Read in weather records until day before simulation.
        DO WHILE (YRDOYW .LT. YRSIMM2Y)
          CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
          IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
          READ (LINE,RECFMT,IOSTAT=ERRNUM) YRDOYW
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)
          CALL Y2K_DOYW(MULTI, RNMODE, YRDOYWY, YRDOYW)
          YRDOYWY = YRDOYW
        ENDDO
        CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
        READ (LINE,RECFMT,IOSTAT=ERRNUM) YRDOYW,SRAD,TMAX,TMIN,
     &           RAIN,TDEW,WINDSP,PAR,RHUM
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)

!       GH added error checking 10/31/05
        IF (SRAD .LT. 0. .OR. RAIN .LT. 0.)
     &    CALL ERROR(ERRKEY,3,FILEW,LINWTH)
        IF (NINT(TMAX * 100.) .EQ. 0 .AND. NINT(TMIN * 100.) .EQ. 0)
     &    CALL ERROR(ERRKEY,4,FILEW,LINWTH)
        IF (ABS(TMAX - TMIN) .LT. 0.005) 
     &    CALL ERROR(ERRKEY,5,FILEW,LINWTH)
        IF (TMAX .LT. TMIN)
     &    CALL ERROR(ERRKEY,6,FILEW,LINWTH)

        CALL Y2K_DOYW(MULTI, RNMODE, YRDOYWY, YRDOYW)
        YRDOYWY = YRDOYW
!CHP 2/9/06        IF (YRDOYW .EQ. YRSIMM) THEN
        IF (YRDOYW .EQ. YRSIMM .OR. ISIM .EQ. 1) THEN
          BACKSPACE(LUNWTH)
          LINWTH = LINWTH - 1
        ENDIF         
      
      FIRST = .TRUE.      !Next record read is first of season

!***********************************************************************
!***********************************************************************
!     RATE (Daily input of weather data)
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------

C     Remember yesterday's date to check for sequential data.
      YRDOYY = YRDOY

C     Read in today's data and check.
      CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
      IF (FOUND .EQ. 0) THEN
         CLOSE(LUNWTH)
         READ(FILEW(5:6),'(I2.2)') WYEAR
         WYEAR = WYEAR + 1
         WRITE(FILEW(5:6),'(I2.2)') MOD(WYEAR,100)
         LINWTH = 0
         PATHL  = INDEX(PATHWT,BLANK)
         IF (PATHL .LE. 1) THEN
            FILEWW = FILEW
         ELSE
            FILEWW = PATHWT(1:(PATHL-1)) // FILEW
         ENDIF

         OPEN (LUNWTH,FILE=FILEWW,STATUS='OLD',IOSTAT=ERRNUM)
         IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)

C        Read in weather file header.
  500    CONTINUE
         CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
         IF (FOUND .EQ. 2) GO TO 500
         IF (FOUND .EQ. 0) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
C        Read in first weather record.
         CALL IGNORE(LUNWTH,LINWTH,FOUND,LINE)
         IF (FOUND .NE. 1) CALL ERROR(ERRKEY,-1,FILEW,LINWTH)
      ENDIF

      READ (LINE,RECFMT,IOSTAT=ERRNUM) YRDOYW,SRAD,TMAX,TMIN,
     &  RAIN,TDEW,WINDSP,PAR,RHUM
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEW,LINWTH)
      CALL Y2K_DOYW(MULTI, RNMODE, YRDOYWY, YRDOYW)
      YRDOYWY = YRDOYW
C-----------------------------------------------------------------------
C     Error checking
C-----------------------------------------------------------------------
!     YRDOY has not been calculated yet for first record read
      IF (.NOT. FIRST) THEN       
        IF (TIMDIF(YRDOYY,YRDOYW) .GT. 1)
     &    CALL ERROR(ERRKEY,2,FILEW,LINWTH)
      ELSE
        FIRST = .FALSE.     
      ENDIF

!###  IF (SRAD .LE. 0. .OR. RAIN .LT. 0.)   !SRAD=0 gives error.
      IF (SRAD .LT. 0. .OR. RAIN .LT. 0.)
     &  CALL ERROR(ERRKEY,3,FILEW,LINWTH)
      IF (NINT(TMAX * 100.) .EQ. 0 .AND. NINT(TMIN * 100.) .EQ. 0)
     &  CALL ERROR(ERRKEY,4,FILEW,LINWTH)
      IF (ABS(TMAX - TMIN) .LT. 0.005) 
     &  CALL ERROR(ERRKEY,5,FILEW,LINWTH)
      IF (TMAX .LT. TMIN)
     &  CALL ERROR(ERRKEY,6,FILEW,LINWTH)

C-----------------------------------------------------------------------
C     Substitute default values if TDEW or WINDSP are missing.
C-----------------------------------------------------------------------
      IF (TDEW .LE. 0.0)  TDEW = TMIN
      IF (WINDSP .LE. 0.0) WINDSP = 86.4

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
      CLOSE (LUNWTH)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END
!     END SUBROUTINE IPWTH
C=======================================================================

! IPWTH Variables

!-----------------------------------------------------------------------
! BLANK   blank character 
! CCO2    Atmospheric CO2 concentration read from input file (ppm)
! DYNAMIC Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, INTEGR, 
!           OUTPUT, or FINAL 
! ERRKEY  Subroutine name for error file 
! ERRNUM  Error number for input 
! FILEIO  Filename for input file (e.g., IBSNAT35.INP) 
! FILEW   Weather data file 
! FILEWW  Pathname plus filename for weather file (e.g. UFGA7801.WTH) 
! FIRST   Indicates first call to subroutine (true or false) 
! FOUND   Indicator that good data was read from file by subroutine FIND  
!           (0- End-of-file encountered, 1 - NAME was found) 
! HDRFMT  format for weather data header information 
! INCYD   Function subroutine increases/decreases date (YRDOY) 
!           based on variable INC. 
! INSI    Location code for weather data 
! ISIM    Day portion of Julian date 
! LINE    Record of data read from file 
! LINWTH  Current line read from weather file 
! LNUM    Current line number of input file 
! LUNIO   Logical unit number for FILEIO 
! LUNWTH  Unit number for weather file 
! MEWTH   Switch for method of obtaining weather data-- 'G' or 'M'- read 
!           weather data file 'S'- read SIMMETEO inputs and generate 
!           weather data 'W'- read WGEN inputs and generate weather data 
! MULTI   Current simulation year (=1 for first or single simulation, =NYRS 
!           for last seasonal simulation) 
! NYEAR   Numeric value of 7th and 8th characters in FILEW, i.e.,  
! PAR     Daily photosynthetically active radiation or photon flux density
!           (moles[quanta]/m2-d)
! PATHL   Number of characters in path name (path plus filename for FILEC) 
! PATHWT  Directory path for weather file 
! RAIN    Precipitation depth for current day (mm)
! RECFMT  Format for weather record 
! REFHT   Reference height for wind speed (m)
! RHUM    Relative humidity (%)
! RSEED1  Random number generator seed- user input 
! SECTION Section name in input file 
! SRAD    Solar radiation (MJ/m2-d)
! TAMP    Amplitude of temperature function used to calculate soil 
!           temperatures (°C)
! TAV     Average annual soil temperature, used with TAMP to calculate soil 
!           temperature. (°C)
! TDEW    Dewpoint temperature (°C)
! TIMDIF  Integer function which calculates the number of days between two 
!           Julian dates (da)
! TMAX    Maximum daily temperature (°C)
! TMIN    Minimum daily temperature (°C)
! WINDHT  Reference height for wind speed (m)
! WINDSP  Wind speed (km/d)
! WYEAR   Weather year; current year for weather data 
! XELEV   Field elevation (not used) (m)
! XLAT    Latitude (deg.)
! XLONG   Longitude (deg.)
! YR      Year portion of date 
! YR_DOY  Function subroutoine converts date in YYDDD format to integer 
!           year (YY) and day (DDD). 
! YRDOY   Current day of simulation (YYDDD)
! YRDOYW  Julian date read from weather file (YYDDD)
! YRDOYY  Date read from weather file yesterday (YYDDD)
! YRSIM   Start of simulation date (YYDDD)
! YRSIMM  Beginning date of simulation (YYDDD)
! YRSIMMY Day before simulation date (YYDDD format) 
! YRSIMY  Day before simulation date (YYDDD format) 
C=======================================================================
