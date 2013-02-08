C=======================================================================
C  UTILS, File, G. Hoogenboom, P.W. Wilkens and B. Baer
C  General utility functions
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/30/1998 GH  Combine UTILS based on UPCASE, VERIFY, TABEX, and CLEAR
C  07/01/2000 GH  Added SWAP
C=======================================================================

C=======================================================================
C  UPCASE, Function
C
C  Function to return the upper case of a lower case letter.  Otherwise
C  returns the same character
C-----------------------------------------------------------------------
C  Revision history
C
C  05/15/1992 BDB Written
C  05/28/1993 PWW Header revision and minor changes   
C-----------------------------------------------------------------------
C  INPUT  : INCHAR
C
C  LOCAL  :
C
C  OUTPUT : INCHAR
C-----------------------------------------------------------------------
C  Called : INPUT READS IPEXP JULIAN
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  INCHAR :
C  CHAVAL :
C=======================================================================

      CHARACTER*1 FUNCTION UPCASE (INCHAR)

      IMPLICIT  NONE

      CHARACTER INCHAR*1
      INTEGER   CHAVAL

      CHAVAL = ICHAR(INCHAR)

      IF ((CHAVAL .LE. 122) .AND. (CHAVAL .GE. 97)) THEN
         UPCASE = CHAR(CHAVAL-32)
       ELSE
         UPCASE = INCHAR
      ENDIF

      END FUNCTION UPCASE

C=======================================================================
C  VERIFY, Subroutine
C
C  I/O utility routine
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : LINE VALUE FLAG
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : IPECO IPSOIL IPVAR SECLI  SECROP SEFERT SEFREQ SEHARV SEINIT
C           SEIRR SEPLT  SERES SESOIL SEVAR  SEWTH  IPEXP  SETIME
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C=======================================================================

      SUBROUTINE VERIFY (LINE,VALUE,FLAG)

      IMPLICIT    NONE

      CHARACTER*1 DIGITS(13),LINE(80)
      INTEGER     JKNEW,JSIGNR,JTIME,JVALUE,NC,NDECML,I,J,JSIGN
      REAL        TFRAC,VALUE,FLAG

      DATA DIGITS/'0','1','2','3','4','5','6','7','8','9',
     &            '+','-','.'/

      FLAG   = -1.0
      JKNEW  =    0
      JSIGNR =    1
      JTIME  =    0
      JVALUE =    0
      NC     =    0
      NDECML =   -1
      TFRAC  =  0.0
      VALUE  =  0.0
C
C     Check for CR/LF
C
      DO I = 1, 80
         IF (LINE(I) .NE. ' ') GO TO 100
      END DO
      !
      ! Nothing entered .. spaces or ÄÄÙ .. set FLAG to 1.0 and exit
      !
      FLAG = 1.0
      GO TO 1300

  100 NC     = NC + 1
      IF (NC .GT. 80) GO TO 1300
      IF (LINE(NC) .EQ. ' ') GO TO 100
      DO I = 1, 13
         J = I
         IF (LINE(NC) .EQ. DIGITS(I)) GO TO 300
      END DO
      GO TO 1200
  300 IF (J .LE. 10) GO TO 600
      IF (J .EQ. 13) GO TO 500
      JSIGN  = 1
      IF (J .EQ. 12) JSIGN = -1

C-----------------------------------------------------------------------
C***    IF SIGN IS REPEATED
C-----------------------------------------------------------------------

      IF (JKNEW .GT. 0) GO TO 1200
      JKNEW = 1
C-----------------------------------------------------------------------
C
C***    SIGN APPEARS AFTER THE DECIMAL POINT
C
C-----------------------------------------------------------------------
      IF (NDECML) 400,1200,900
  400 JSIGNR = JSIGN
      GO TO 900
  500 JKNEW  = 1
C-----------------------------------------------------------------------
C***    DECIMAL REPEATED
C-----------------------------------------------------------------------

      IF (NDECML .GE. 0) GO TO 1200
      NDECML = 0
      GO TO 900
  600 J = J - 1
      JKNEW = 1
      IF (NDECML) 700,800,900
  700 JVALUE = JVALUE*10 + J
      GO TO 900
  800 JTIME = JTIME + 1
      TFRAC = TFRAC + FLOAT(J)/(10.**JTIME)
  900 VALUE = FLOAT(JSIGNR*JVALUE)+FLOAT(JSIGNR)*TFRAC
 1000 NC    = NC + 1
      IF (NC .GT. 80) GO TO 1300
      IF (LINE(NC) .EQ. ' ') GO TO 1000
      DO I = 1, 13
         J = I
         IF (LINE(NC) .EQ. DIGITS(I)) GO TO 300
      END DO
 1200 FLAG = 1.0

 1300 DO I = 1, 80
         LINE(I) = ' '
      END DO

      END SUBROUTINE VERIFY

C=======================================================================
C  TABEX, Function
C
C  Look up utility routine
C-----------------------------------------------------------------------
      FUNCTION TABEX(VAL,ARG,DUMMY,K)

      IMPLICIT NONE

      INTEGER K,J

      REAL VAL(K),ARG(K),DUMMY,TABEX

           DO 100  J = 2,K
           IF (DUMMY .GT. ARG(J)) GO TO 100
           GO TO 200
  100      CONTINUE
      J = K
  200 TABEX = (DUMMY-ARG(J-1))*(VAL(J)-VAL(J-1))/(ARG(J)-ARG(J-1))+VAL
     &     (J-1)

      END FUNCTION TABEX

C=======================================================================
C  CLEAR, Subroutine
C
C  Clears the screen using ANSI codes, sets color to white on blue
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : SESOIL SESIM SERES SEPLT SENSDM SENS SEFREQ SEWTH SEFERT SECLI
C           SEVAR SETIME IPVAR IPSOIL WTHMDI OPHARV OPDAY IPEXP SEIRR SEINIT
C           SEHARV SECROP INVAR INPUT
C
C  Calls  : None
C-----------------------------------------------------------------------

C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE CLEAR

      IMPLICIT  NONE

      !CHARACTER ESC
      !ESC = CHAR(27)
C      WRITE  (*,100) ESC
      WRITE  (*,100) 
C 100   FORMAT (1X,A1,'[2J',//////)
100   FORMAT (20/)

      END SUBROUTINE CLEAR

C=======================================================================
C  HOME, Subroutine
C
C  Moves cursor to 0,0 ANSI and clears the screen
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      2- 9-93
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : ERROR AUTPLT AUTHAR INPUT
C
C  Calls  : CLEAR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE HOME

      IMPLICIT  NONE

      CHARACTER ESC

      ESC = CHAR(27)

      WRITE (*,100) ESC
      WRITE (*,200) ESC

100   FORMAT (1X,A1,'[2J')
200   FORMAT (1X,A1,'[0;0H')

      END SUBROUTINE HOME

C=======================================================================
C  CURPOS, Subroutine
C
C  Moves cursor to specified row
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                     P.W.W.      2- 9-93
C  2. Header revision and minor changes           P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : ESC
C
C  OUTPUT : None
C-----------------------------------------------------------------------
C  Called : INTRO
C
C  Calls  : NONE
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  ESC    : ESC key codes
C=======================================================================

      SUBROUTINE CURPOS (LINE)

      IMPLICIT  NONE

      CHARACTER ESC,LINE*2

      ESC = CHAR(27)

      WRITE (*,200) ESC,LINE

200   FORMAT (1X,A1,'[',A2,';0H')

      END SUBROUTINE CURPOS

C=======================================================================
C  CURV, Function
C
C  Function to interpolate between four data points.
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWJ Written
C  01/12/1999 GH  Added to UTILS routine
C  10/08/2004 CHP Changed criteria for XM from .GT. to .GE. 
C-----------------------------------------------------------------------
C  INPUT  :
C
C  LOCAL  :
C
C  OUTPUT : INCHAR
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  CTYPE  :
C  CURV   :
C  XB     :
C  XM     :
C=======================================================================
      FUNCTION CURV(CTYPE,XB,X1,X2,XM,X)

      IMPLICIT NONE

      CHARACTER*3 CTYPE
      REAL CURV,XB,X1,X2,XM,X

      CURV = 1.0
      IF (CTYPE .EQ. 'NON' .OR. CTYPE .EQ. 'non') RETURN

C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'LIN' .OR. CTYPE .EQ. 'lin') THEN
        CURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CURV = (X-XB)/(X1-XB)
        IF(X .GE. X1 .AND. X .LE. X2)CURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)CURV = 1.0 - (X-X2)/(XM-X2)
        CURV = MAX(CURV,0.0)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------

C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'QDR' .OR. CTYPE .EQ. 'qdr') THEN
        CURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CURV = 1. -((X1-X)/(X1-XB))**2
        IF(X .GE. X1 .AND. X .LE. X2)CURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)CURV = 1. - ((X-X2)/(XM-X2))**2
        CURV = MAX(CURV,0.0)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------

C-------------------------------------------------------------------------------
C     Curve type INL is the inverse linear with a minimum for use in photoperiod
C     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'INL' .OR. CTYPE .EQ. 'inl') THEN
        CURV = 1.0
        IF(X .GT. X1 .AND. X .LT. X2)CURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
!        IF(X .GT. X2) CURV = XM
        IF(X .GE. X2) CURV = XM       !CHP per Stu Rymph 10/8/2004
        CURV = MAX(CURV,XM)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------
C     Curve type SHO for use with short day plants.
C     The curve is the inverse linear with a minimum for use in photoperiod
C     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'SHO' .OR. CTYPE .EQ. 'sho') THEN
        IF (X .LE. X1) THEN
           CURV = 1.0
        ELSE IF ((X .GT. X1) .AND. (X .LT. X2)) THEN
           CURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
        ELSE IF (X .GE. X2) THEN
          CURV = XM
        ENDIF
        CURV = MAX(CURV,XM)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------
C     Curve type LON for use with long day plants.
C     The curve is the inverse linear with a minimum for use in photoperiod
C     In this case, XM is the lowest relative rate, X1 and X2 are critical dayl
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'LON' .OR. CTYPE .EQ. 'lon') THEN
        IF (X .LT. X2) THEN
           CURV = XM
        ELSE IF ((X .GE. X2) .AND. (X .LT. X1)) THEN
           CURV = 1.-(1.-XM)*((X1-X)/(X1-X2))
        ELSE
           CURV = 1.0
        ENDIF
        CURV = MAX(CURV,XM)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------
C
C-------------------------------------------------------------------------------
      IF(CTYPE .EQ. 'SIN' .OR. CTYPE .EQ. 'sin') THEN
        CURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)
     &   CURV = 0.5*(1.+COS(2.*22./7.*(X-X1)/(2.*(X1-XB))))
        IF(X .GE. X1 .AND. X .LE. X2)CURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)
     &   CURV = 0.5*(1.+COS(2.*22./7.*(X2-X)/(2.*(XM-X2))))
        CURV = MAX(CURV,0.0)
        CURV = MIN(CURV,1.0)
      ENDIF
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
      END FUNCTION CURV

C=======================================================================
C  SWAP
C
C  Subroutine to swap values among different variables
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/90 JWH Written
C  07/01/00 GH  Added to UTILS routine
C-----------------------------------------------------------------------

      SUBROUTINE SWAP(A, B, C, D, E, F)

      IMPLICIT NONE

      INTEGER A, B, C, D, E, F, I

      I = A
      A = B
      B = I
      I = C
      C = D
      D = I
      I = E
      E = F
      F = I
      RETURN
      END SUBROUTINE SWAP
C-----------------------------------------------------------------------


C=======================================================================
C  GETLUN, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Assigns unique output file unit numbers to input and output files
C     based on file variable name.  If valid file variable name is not
C     specified, unit numbers are assigned incrementally starting with
C     unit 90.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/17/2001 CHP Written.
C-----------------------------------------------------------------------
! Called by: IRRIG, OPWBAL, OPGROW, . . . 
! Calls: None
C========================================================================

      SUBROUTINE GETLUN(FileVarName, LUN)

!-----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*), INTENT(IN) :: FileVarName
      INTEGER, INTENT(OUT) :: LUN

      LOGICAL FEXIST, FPRINT(200)
      INTEGER COUNTER, Length, I, OUTLUN, StartLun
      CHARACTER*30 SaveName(200)

      DATA StartLun /90/
      DATA COUNTER /0/
      DATA FPRINT /200*.FALSE./
      DATA OUTLUN /83/    !List.OUT - list of unit assignments

!-----------------------------------------------------------------------
!     On first call to subroutine, open new file to record
!     input and output file information.
CFANG      INQUIRE (FILE = 'LIST.OUT', EXIST = FEXIST)
      INQUIRE (FILE = 'List.OUT', EXIST = FEXIST)
      IF (FEXIST) THEN
C-JED   OPEN (UNIT = OUTLUN, FILE = 'List.OUT', STATUS = 'OLD',
        OPEN (UNIT = OUTLUN, FILE = 'List.OUT', STATUS = 'OLD',
     &        ACTION = 'WRITE',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = OUTLUN, FILE = 'List.OUT', STATUS = 'NEW')
        WRITE(OUTLUN,10)
   10   FORMAT('*Summary of files opened during simulation',
     &      //,'Unit  File',/'Num.  Variable Name')
      ENDIF

!-----------------------------------------------------------------------
      Length = Len(Trim(FileVarName))

      SELECT CASE (FileVarName(1:Length))

! Input Files (Units 8 through 29):
      CASE ('FILEA');   LUN = 8   !observed time series data
      CASE ('FILEC', 'FILEE', 'FINPUT');  LUN = 10          
                  !*.spe, *.eco, miscellaneous input files
      CASE ('FILEW');   LUN = 11  !*.wth - weather files
      CASE ('FILEP');   LUN = 12  !*.pst - pest files
      CASE ('FILESS');  LUN = 13  !SOILN980.SOL
      CASE ('BATCH');   LUN = 14  !Batch run input file
      CASE ('ERRORX');  LUN = 15  !Model.err
      CASE ('FILETL');       LUN = 16  !Tillage.sol
      CASE ('PFILE');   LUN = 17  !Phosphorus input files
      CASE ('FILEIO');       LUN = 21  !temp. input file; dssat40.inp
      CASE ('DTACDE');  LUN = 22  !DATA.CDE
      CASE ('FILETMP'); LUN = 23  !Tony Hunt temp file

! Daily Output Files (Units 30 through 49):    FName Code:
      CASE ('OUTM');    LUN = 30  !MgmtOps.OUT    
      CASE ('OUTWTH');  LUN = 31  !Weather.OUT
      CASE ('OUTG');    LUN = 32  !PlantGro.OUT
      CASE ('OUTPN');   LUN = 33  !PlantN.OUT
      CASE ('OUTPC');   LUN = 34  !PlantC.OUT
      CASE ('OUTD');    LUN = 35  !Pest.OUT
      CASE ('OUTT');    LUN = 36  !SoilTemp.OUT
      CASE ('OUTWAT');  LUN = 37  !SoilWat.OUT
      CASE ('OUTSN');   LUN = 38  !SoilN.OUT
      CASE ('OUTSC');   LUN = 39  !SoilC.OUT
      CASE ('OUTSP');   LUN = 40  !SoilP.OUT
      CASE ('OUTSPAM'); LUN = 41  !ET.OUT
      CASE ('OUTSOMC'); LUN = 42  !SOMLITC.OUT
      CASE ('OUTETP');  LUN = 43  !ETPhot.OUT
      CASE ('OUTFLD');  LUN = 44  !FloodW.OUT
      CASE ('OUTCH');   LUN = 45  !Chemical.OUT
      CASE ('FLDN');    LUN = 46  !FloodN.OUT
      CASE ('OUTSOMN'); LUN = 47  !SOMLITN.OUT
      CASE ('OUTP');    LUN = 48  !PlantP.OUT
      CASE ('OUTRSTG'); LUN = 49  !Rstages.OUT

! Daily Information files: (Units 50 through 59)
      CASE ('SLDET');   LUN = 50  !Somlit1.OUT
      CASE ('OUTWARN'); LUN = 51  !Warning.OUT
      CASE ('WORK.OUT');LUN = 52  !Work.OUT - for CSCERES
      CASE ('ERRORO');  LUN = 53  !Error.OUT - echo of screen errors

! Seasonal output files (Units 60 through 79):
      CASE ('SOUTM');   LUN = 60  !MgmtOpsSum.OUT
      CASE ('SOUTWTH'); LUN = 61  !WeatherSum.OUT
      CASE ('SOUTG');   LUN = 62  !PlantSum.OUT
      !
      CASE ('SOUTSPAM');LUN = 65  !SPAMSum.OUT
      CASE ('SOUTR');   LUN = 66  !Operat.OUT
      CASE ('SOUTE');   LUN = 67  !Environ.OUT
      CASE ('SEVAL');   LUN = 68  !Evaluate.OUT
      CASE ('PNBAL');   LUN = 70  !PlantNbal.OUT
      CASE ('PCBAL');   LUN = 71  !PlantCbal.OUT
      CASE ('SNBAL');   LUN = 72  !SoilNbal.OUT
      CASE ('SCBAL');   LUN = 73  !SoilCbal.OUT
      CASE ('PPBAL');   LUN = 74  !PlantPbal.OUT
      CASE ('SPBAL');   LUN = 75  !SoilPbal.OUT

! Composite output files (Units 80 through 89):
      CASE ('OUTO');    LUN = 80  !Overview.OUT
      CASE ('OUTS');    LUN = 81  !Summary.OUT
      CASE ('SWBAL');   LUN = 82  !SoilWatbal.OUT
      !RESERVE UNIT 83 FOR LIST.OUT   
      CASE ('LIST');    LUN = 83  !List.OUT (list of unit assignments)
      CASE ('OUTBAT');  LUN = 84  !TEMP.BAT file for DOS commands
      CASE ('TEMP');    LUN = 85  !TEMP file
      CASE ('OUTLST');  LUN = 86  !OUTPUT.LST list of output files

      CASE ('OUTSOMP'); LUN = 87  !SOMLITP.OUT

! Files not covered above will be assigned numbers
!     incrementally starting with unit number 90.
      CASE DEFAULT
        !First check to see if a unit number has already been
        !assigned to this FileVarName.  If so, assign same LUN.
        DO I = StartLun, StartLun + Counter
          IF (FileVarName .EQ. TRIM(SaveName(I))) THEN
            LUN = I
            EXIT 
          ENDIF
        ENDDO

        !Assign a unique unit number to this FileVarName
        IF (I .GT. StartLun + Counter) THEN
          LUN = StartLun + COUNTER
          COUNTER = COUNTER + 1
        ENDIF
        
      END SELECT 

!     Print to 'LIST.OUT' file each file assigned a unit number
!     (only print the first time a unit is assigned)
!       OUTPUT.LST - ICASA format headers, etc.
!     Save FileVarName in case it is used again.
      IF (.NOT. FPRINT(LUN)) THEN
C        PRINT *, 'FileVarName = ', FileVarName
        WRITE(OUTLUN,'(I4,2X,A)') LUN, FileVarName
        FPRINT(LUN) = .TRUE.
        SaveName(LUN) = FileVarName
      ENDIF

      CLOSE(OUTLUN)

      RETURN
      END SUBROUTINE GETLUN
C=======================================================================


C=======================================================================
C  HEADER, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Writes simulation header info to file.

!     Currently, the routine reads header info from a file (written 
!     previously by input module or elsewhere in the code).  Probably
!     should generate the header info here.

C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/18/2001 CHP Written.
C  08/12/2003 CHP Added I/O error checking
C-----------------------------------------------------------------------
! Called by: IRRIG, OPWBAL, OPGROW, . . . 
! Calls: None
C========================================================================

      SUBROUTINE HEADER(DYNAMIC, FILEIO, LUNDES, RUN)

!     Reads up to 100 lines of header information and prints to output
!       files as needed.
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*2 CROP
      CHARACTER*6 SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'HEADER'
      CHARACTER*8 EXPER, MODEL
      CHARACTER*16 CROPD
      CHARACTER*25 TITLET
      CHARACTER*30 FILEIO
      CHARACTER*60 ENAME
      CHARACTER*78 MSG(3)
      CHARACTER*80 HEADER1(100), HEADER2  !Simulation header

      INTEGER DYNAMIC, ERRNUM, FOUND, I, LINC, LNUM, LUNDES
      INTEGER LUNIO, NH1, RUN, PREV_RUN, TRTNO
      INTEGER LUNSRC   !Source file unit number

      INTEGER istat

      DATA PREV_RUN /0/
      
      logical fexist, fopened, ERR, FIRST
      integer unumber

!-----------------------------------------------------------------------
      IF (PREV_RUN .EQ. 0) THEN
        !Do once.
        CALL GETLUN('FILEIO', LUNIO)
        CALL GETLUN('FINPUT', LUNSRC)
      ENDIF

!     If this is a new run, then read header information from HEADER.OUT
!       and FILEIO.
      IF (RUN .NE. PREV_RUN .AND. RUN .NE. 0) THEN
        NH1 = 0   
        OPEN (UNIT = LUNSRC, FILE = 'HEADER.OUT', STATUS = 'OLD',
     &        IOSTAT = ERRNUM)
        IF (ERRNUM .EQ. 0) THEN
          I = 0
C-JED     DO WHILE (.NOT. EOF (LUNSRC))
          DO WHILE (istat.EQ.0)
            I = I + 1
            READ(LUNSRC,'(A80)',ERR=50, iostat=istat) HEADER1(I)
          ENDDO
   50     CONTINUE
          NH1 = I   !Number of simulation header lines
        ELSE
          WRITE(MSG(1),"('*RUN',I4)") RUN
          MSG(2) = ' HEADER.OUT file does not exist or could not be'
     &                    // ' opened.'
          MSG(3) = ' Output file headers cannot be generated.'
          CALL WARNING(3, ERRKEY, MSG)
        ENDIF
        CLOSE (LUNSRC)

        CROP = '  '
        CROPD = '          '

!     The following code was added temporarily to fix
!     a problem where fileio is opened on unit 0 on the first call
!     to the subroutine.
        inquire (file = fileio, exist = fexist)

        if (fexist) then
          inquire (file = fileio, opened = fopened)
          if (fopened) then
            inquire (file = fileio, number = unumber)
            close(unumber)
          endif

!-----------------------------------------------------------------------
!       On first call, get new header information from FILEIO.  
!-----------------------------------------------------------------------
          ERR = .FALSE.
          OPEN (UNIT=LUNIO, FILE=FILEIO, STATUS='OLD', IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) THEN
            ERR = .TRUE.
            GOTO 150
          ENDIF

          REWIND (LUNIO)
          LNUM = 0

C-----------------------------------------------------------------------
C         Read Model and Species FILE name and path
C-----------------------------------------------------------------------
          READ (LUNIO,'(//, 15X,A8)',IOSTAT=ERRNUM) MODEL
          LNUM = LNUM + 3
          IF (ERRNUM .NE. 0) ERR = .TRUE.

!-----------------------------------------------------------------------
C        Find and Read Experimental Details Section
C-----------------------------------------------------------------------
          SECTION = '*EXP.D'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            ERR = .TRUE.
          ELSE
            READ(LUNIO, 120,IOSTAT=ERRNUM) EXPER, ENAME; LNUM = LNUM + 1
  120       FORMAT(3X,A8,4X,A60)
            IF (ERRNUM .NE. 0) ERR = .TRUE.
          ENDIF
C-----------------------------------------------------------------------
C       Find and Read TREATMENTS Section
C-----------------------------------------------------------------------
          SECTION = '*TREAT'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            ERR = .TRUE.
          ELSE
            READ(LUNIO, 130,IOSTAT=ERRNUM) TRTNO, TITLET; LNUM = LNUM +1
  130       FORMAT(I3,6X,A25)

            IF (ERRNUM .NE. 0) ERR = .TRUE.
          ENDIF

!-----------------------------------------------------------------------
!       Find and read Cultivar Section
!-----------------------------------------------------------------------
          SECTION = '*CULTI'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
            ERR = .TRUE.
          ELSE
            READ(LUNIO,140,IOSTAT=ERRNUM) CROP ; LNUM = LNUM + 1
  140       FORMAT(3X,A2)
            IF (ERRNUM .NE. 0) ERR = .TRUE.
          ENDIF

          CLOSE (LUNIO)
  150     CALL GET_CROPD(CROP, CROPD)
        else
          ERR = .TRUE.
        endif

        IF (ERR) FIRST = .TRUE.
        PREV_RUN = RUN
      ENDIF

!     Write Model info and date-time stamp to all headers
      IF (NH1 .GT. 0) WRITE(LUNDES,'(/,A80)') HEADER1(1)

!***********************************************************************
!***********************************************************************
!     Run Initialization
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Write simulation header info to destination file.
      DO I = 2, NH1
        IF (HEADER1(I)(1:4) .EQ. '*RUN') THEN
!         Update run number (needed for multiple-year runs)
          HEADER2 = HEADER1(I)
          WRITE(HEADER1(I),'(A5,I3,A72)')HEADER2(1:5),RUN,HEADER2(9:80)
        ENDIF
        WRITE(LUNDES,'(A80)',ERR=200) HEADER1(I)
      ENDDO
  200 CONTINUE  

!***********************************************************************
!***********************************************************************
!     Seasonal initialization
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Write seasonal header information to destination file.
      IF (ERR) THEN
        !Errors detected in FILEIO info
        WRITE(LUNDES,1000) MOD(RUN,1000)
 1000   FORMAT(/,'*RUN ',I3,/)

        IF (FIRST) THEN
          FIRST = .FALSE.
          WRITE(MSG(1),"('*RUN',I4)") RUN
          MSG(2) = ' Invalid format in FILEIO.'
          MSG(3) = ' Output file headers cannot be generated.'
          CALL WARNING(3, ERRKEY, MSG)
        ENDIF

      ELSE    !No errors
        WRITE (LUNDES,1100) MOD(RUN,1000), TITLET,
     &        MODEL, CROPD, EXPER, CROP, ENAME, TRTNO, TITLET
 1100   FORMAT (/,'*RUN ',I3,8X,':',A25,/,
     &        1X,'MODEL',10X,':',1X,A8,' - ',A16,/,
     &        1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,4X,A47,/,
     &        1X,'TREATMENT',I3, 3X,':',A25,/)
      ENDIF

!***********************************************************************
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      RETURN
      END SUBROUTINE HEADER

!=======================================================================


C=======================================================================
C  GET_CROPD, Subroutine, C. H. Porter
C-----------------------------------------------------------------------
C  Assigns text description of crop based on 2 letter crop code.

C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/19/2002 CHP Written.
C  08/22/2003 CHP revised to read crop descriptions from DETAIL.CDE
C  11/23/2004 CHP Increased length of PATHX (path for executable) to 120.
C========================================================================

      SUBROUTINE GET_CROPD(CROP, CROPD)
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*2  CROP, CROPID
      CHARACTER*6  SECTION, ERRKEY
      CHARACTER*10 FILECDE
      CHARACTER*16 CROPD
      CHARACTER*30 CRDESC
      CHARACTER*48 DATAX
      CHARACTER*78 MSG(3)
      PARAMETER (ERRKEY = 'CROPD')
      CHARACTER*120 PATHX

      INTEGER FOUND, ERR, LNUM, LUN
      INTEGER IPX

      LOGICAL FEXIST

      DATA FILECDE /'DETAIL.CDE'/
C-----------------------------------------------------------------------
      CROPD = '          '
      DATAX = FILECDE
      INQUIRE (FILE = DATAX, EXIST = FEXIST)

      IF (.NOT. FEXIST) THEN
!       File does not exist in data directory, check directory
!         with executable.
        CALL GETARG(0,PATHX)
C-JED   CALL GETARG(0,PATHX,IPX)
        IPX = INDEX(PATHX,' ')
        IF (IPX .NE. 0) THEN
          IPX = IPX - 1
        ENDIF
        DATAX = PATHX(1:(IPX-12)) // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF        

      IF (.NOT. FEXIST) THEN
!       Last, check for file in C:\DSSAT4 directory
        DATAX = "../" // FILECDE
        INQUIRE (FILE = DATAX, EXIST = FEXIST)
      ENDIF

      IF (FEXIST) THEN
        CALL GETLUN('DTACDE',LUN)
        OPEN (LUN, FILE=DATAX, STATUS = 'OLD',IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR(ERRKEY, ERR, DATAX, 0)
        SECTION = '*Crop '
        CALL FIND(LUN,SECTION,LNUM,FOUND)
        IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, DATAX, LNUM)
C-JED   DO WHILE (.NOT. EOF(LUN))
        DO WHILE (err.eq.0)
          READ(LUN,'(A2,7X,A30)',IOSTAT=ERR) CROPID, CRDESC
          LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY, ERR, DATAX, LNUM)
          IF (CROPID .EQ. CROP) THEN
            CROPD = CRDESC(1:16)
            EXIT
          ENDIF
        ENDDO
        CLOSE(LUN)
      ELSE
!       Detail.CDE file is missing -- stop program with message.
        CALL ERROR(ERRKEY, 29, FILECDE, 0)
      ENDIF

      IF (CROPD .EQ. '          ') THEN
        WRITE(MSG(1),11) CROP , FILECDE
   11   FORMAT('Crop code ',A2, ' could not be found in file: ',A)
        CALL WARNING(1, ERRKEY, MSG)
      ENDIF

!Previous code:
!      SELECT CASE (CROP)
!      CASE ('BA'); CROPD = 'BARLEY    '
!      CASE ('BN'); CROPD = 'DRY BEAN  '
!      CASE ('BR'); CROPD = 'BRACHIARIA'
!      CASE ('C3'); CROPD = 'C3-CROPS  '
!      CASE ('C4'); CROPD = 'C4-CROPS  '
!      CASE ('CB'); CROPD = 'CABBAGE   '
!      CASE ('CS'); CROPD = 'CASSAVA   '
!      CASE ('CH'); CROPD = 'CHICKPEA  '
!      CASE ('CO'); CROPD = 'COTTON    '
!      CASE ('CP'); CROPD = 'COWPEA    '
!      CASE ('CT'); CROPD = 'CITRUS    '
!      CASE ('FA'); CROPD = 'FALLOW    '
!      CASE ('FB'); CROPD = 'FABA BEAN '
!      CASE ('G0'); CROPD = 'BAHIA     '
!      CASE ('G1'); CROPD = 'GRASS-1   '
!      CASE ('G2'); CROPD = 'GRASS-2   '
!      CASE ('G3'); CROPD = 'GRASS-3   '
!      CASE ('G4'); CROPD = 'GRASS-4   '
!      CASE ('G5'); CROPD = 'GRASS-5   '
!      CASE ('G6'); CROPD = 'GRASS-6   '
!      CASE ('G7'); CROPD = 'GRASS-7   '
!      CASE ('G8'); CROPD = 'GRASS-8   '
!      CASE ('MZ'); CROPD = 'MAIZE     '
!      CASE ('ML'); CROPD = 'MILLET    '
!      CASE ('PE'); CROPD = 'PEA       '
!      CASE ('PI'); CROPD = 'PINEAPPLE '
!      CASE ('PN'); CROPD = 'PEANUT    '
!      CASE ('PP'); CROPD = 'PIGEONPEA '
!      CASE ('PR'); CROPD = 'PEPPER    '
!      CASE ('PT'); CROPD = 'POTATO    '
!      CASE ('RI'); CROPD = 'RICE      '
!      CASE ('SB'); CROPD = 'SOYBEAN   '
!      CASE ('SC'); CROPD = 'SUGARCANE '
!      CASE ('SG'); CROPD = 'SORGHUM   '
!      CASE ('SU'); CROPD = 'SUNFLOWER '
!      CASE ('TM'); CROPD = 'TOMATO    '
!      CASE ('TN'); CROPD = 'TANIER    '
!      CASE ('TR'); CROPD = 'TARO      '
!      CASE ('VB'); CROPD = 'VELVETBEAN'
!      CASE ('WH'); CROPD = 'WHEAT     '
!      END SELECT

      RETURN
      END SUBROUTINE GET_CROPD
C=======================================================================


C=======================================================================
C  INTERPOLATE, Subroutine
C
C  Interpolates and coagulates across/between layers
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P. Wilkens  2-8-93
C-----------------------------------------------------------------------
C  INPUT  : NOUTDM,DAP,YRDOY,PCINPD,PG,GROWTH,MAINR,GRWRES,CADLF,CADST,
C           CMINEA,THUMC,CUMRES,TOTWT,RHOL,RHOS,PGNOON,PCINPN
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  :
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      REAL FUNCTION INTERPOLATE (ARRVAR, DEPTH)

      IMPLICIT  NONE

      REAL      ARRVAR(20),X(10),DEPTH,TOTALVAL,TOTDEPTH,DIFF
      INTEGER   I

      DATA X   /5.,10.,15.,15.,15.,30.,30.,30.,30.,30./

      IF (DEPTH .LE. X(1)) THEN
         !
         ! Depth is <= to 5 cm
         !
         INTERPOLATE = ARRVAR(1)
         RETURN
      ENDIF

      TOTDEPTH = 0.0
      TOTALVAL = 0.0

      DO I = 1, 10
         IF (TOTDEPTH + X(I) .LE. DEPTH) THEN
           !
           ! We have not yet reached the queried depth
           !
           TOTDEPTH = TOTDEPTH + X(I)
           TOTALVAL = TOTALVAL + ARRVAR(I)*X(I)
!           IF (TOTDEPTH .EQ. DEPTH) THEN
           IF (ABS(TOTDEPTH - DEPTH) .LT. 0.0005) THEN
              INTERPOLATE = TOTALVAL / TOTDEPTH
              EXIT
           ENDIF
         ELSE
           !
           ! Ended up in the middle of a layer .. mix it
           !
           DIFF     = DEPTH - TOTDEPTH
           TOTDEPTH = DEPTH
           TOTALVAL = TOTALVAL + ARRVAR(I)*DIFF
           INTERPOLATE = TOTALVAL / TOTDEPTH
           EXIT
         ENDIF
      END DO

      END FUNCTION INTERPOLATE
C=======================================================================

C=======================================================================
C  OPCLEAR, Subroutine C.H. Porter
C  Delete *.OUT files 
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  07-22-2002 CHP Written.
C  03/07/2005 CHP Read previous output file names from OUTPUT.LST 
C                 rather than delete all *.OUT files.
C=======================================================================
      SUBROUTINE OPCLEAR

      IMPLICIT NONE

      INTEGER, PARAMETER :: MaxFiles = 31

      CHARACTER*14, DIMENSION(MaxFiles) :: FileName
      CHARACTER*2,  DIMENSION(MaxFiles) :: OPCODE
      CHARACTER*50, DIMENSION(MaxFiles) :: Comment
      
      INTEGER ERR, I, LUN

!     Get list of possible output file names
      CALL OUTFILES(FileName, OPCODE, Comment)

!     Find available logical unit for temporary file open commands.
      CALL GETLUN("OPCLR", LUN)

!     Loop thru files, if it exists, delete it.
      DO I = 1, MaxFiles
        OPEN (FILE=trim(FileName(I)), UNIT=LUN, STATUS='OLD',IOSTAT=ERR)
        IF (ERR == 0) CLOSE(LUN,STATUS='DELETE')
      ENDDO

      RETURN
      END SUBROUTINE OPCLEAR
C=======================================================================


C=======================================================================
C  OPNAMES, Subroutine C.H. Porter
C  Use alternate output filenames if FNAME is set.
C  File commands are written to a DOS batch file so the screen output 
C    can be controlled.
C  To expand the list of files: Increase MaxFiles, Add filename and 
C    character code to list.
C  Write names of output files to FileNames.OUT file
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  07-22-2002 CHP Written.
!  01-11-2005 CHP Added a few more output files to the list.
C=======================================================================
      SUBROUTINE OPNAMES(FNAME)

C-JED USE IFPORT
      IMPLICIT NONE

      SAVE
      INTEGER i, COUNT, LUNLST, LUNTMP, MaxFiles, SYS
      INTEGER FNUM
      PARAMETER (MaxFiles = 31)

      CHARACTER*8  FNAME
      CHARACTER*12  TempName
      CHARACTER*14, DIMENSION(MaxFiles) :: FileName
      CHARACTER*2,  DIMENSION(MaxFiles) :: OPCODE
      CHARACTER*50, DIMENSION(MaxFiles) :: Comment
      CHARACTER*80 BatchCommand
      CHARACTER*80 Chmod_sh

      LOGICAL FOPEN, FEXIST

C-----------------------------------------------------------------------
!     If FNAME = 'OVERVIEW', then keep default names
!     If FNAME not equal to 'OVERVIEW': 
!       Assign output file names based on FNAME.

!     The following files will be copied to a file of the form 
!     FNAME.ccO, 
!     where FNAME is read from FILEIO and sent from main program, 
!           cc  = Alternate filename character code (see below).

!-----------------------------------------------------------------------
      CALL OUTFILES(FileName, OPCODE, Comment)

!     Open OUTPUT.LST file - list of new output files
      CALL GETLUN("OUTO", LUNLST)
      OPEN (LUNLST, FILE="OUTPUT.LST", STATUS = 'REPLACE')

      WRITE(LUNLST,10)
   10 FORMAT('*Output file list',
     &    //,'@FILENAME        DESCRIPTION')

      IF (FNAME .EQ. 'OVERVIEW') THEN
!       Keep filenames, just write to OUTPUT.LST file
        DO i = 1, MaxFiles
          !Check if FileName(i) exists, if not, go on to the next file
          INQUIRE (FILE = Trim(FileName(i)), EXIST = FEXIST)
          IF (.NOT. FEXIST) CYCLE   
          WRITE(LUNLST,'(A14,3X,A50)') FileName(i), Comment(i)
        Enddo

      ELSE
!       Re-name output files based on FNAME
!       Open temporary batch file
        CALL GETLUN("OUTBAT", LUNTMP)
        OPEN (LUNTMP, FILE="TEMP.BAT", STATUS = 'REPLACE')
        COUNT = 0

        DO i = 1, MaxFiles
          !Check if FileName(i) exists, if not, go on to the next file
          INQUIRE (FILE = Trim(FileName(i)), EXIST = FEXIST)
          IF (.NOT. FEXIST) CYCLE   

          !Determine new file name and store as TempName
          IF (OPCODE(i) /= '  ') THEN
            TempName = FNAME // '.' // 'O' // OPCODE(i) 
            WRITE(LUNLST,'(A12,5X,A50)') TempName, Comment(i)

            !Check if TempName exists, if so, delete it.
            INQUIRE (FILE = TempName, EXIST = FEXIST)
            IF (FEXIST) THEN   
              BatchCommand = 'rm -f ' // TempName
              WRITE(LUNTMP, '(A50)') BatchCommand
              COUNT = COUNT + 1
            ENDIF

            !Copy from default filename into new filename 
            BatchCommand = 'cp  -f ' // FileName(i) // ' '  
     &                             // TempName // ' '
            WRITE(LUNTMP,'(A50)') BatchCommand

            !Delete old file
            BatchCommand = 'rm -f ' // FileName(i)
            WRITE(LUNTMP,'(A50)') BatchCommand
            WRITE(LUNTMP, '(" ")')
            COUNT = COUNT + 2

            !If file was left open, close it now.
            INQUIRE(FILE=FILENAME(I), OPENED=FOPEN)
            IF (FOPEN) THEN
              INQUIRE(FILE=FILENAME(I), NUMBER=FNUM)
              CLOSE(FNUM)
            ENDIF

          ELSE
!           Don't rename
            !Check if FileName(i) exists, if not, go on to the next file
            INQUIRE (FILE = Trim(FileName(i)), EXIST = FEXIST)
            IF (FEXIST) THEN
              WRITE(LUNLST,'(A14,3X,A50)') FileName(i), Comment(i)
            ENDIF
          ENDIF 
        Enddo

        CLOSE (LUNTMP)

        IF (COUNT > 0) THEN
!         Run batch file - direct output to TEMP.BAK file
          BatchCommand = "sh TEMP.BAT >TEMP.BAK"
          Chmod_sh = "chmod 755 TEMP.BAT"
          SYS = SYSTEM( Chmod_sh )
          SYS = SYSTEM(BatchCommand)
  
!         Delete TEMP.BAT file
          OPEN (LUNTMP, FILE = "TEMP.BAT", STATUS = 'UNKNOWN')
          CLOSE (LUNTMP, STATUS = 'DELETE')
  
!         Delete TEMP.BAK file
          OPEN (LUNTMP, FILE = "TEMP.BAK", STATUS = 'UNKNOWN')
          CLOSE (LUNTMP, STATUS = 'DELETE')
        ELSE
!         Close empty batch file
          CLOSE (LUNTMP, STATUS = 'DELETE')
        ENDIF

      ENDIF
      CLOSE (LUNLST)

      RETURN
      END SUBROUTINE OPNAMES
C=======================================================================

C=======================================================================
C  ALIN, Function
C
C  Linear intepolation routine
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2. Header revision and minor changes           P.W.W.      8-7-93
C-----------------------------------------------------------------------
C  INPUT  : TABX,TABY,N
C
C  LOCAL  : I
C
C  OUTPUT : XVAL
C-----------------------------------------------------------------------
C  Called : NFACTO
C
C  Calls  : None
C=======================================================================

      REAL FUNCTION ALIN (TABX,TABY,N,XVAL)

      IMPLICIT  NONE

      INTEGER   N,I
      REAL      TABX,TABY,XVAL

      DIMENSION TABX(N),TABY(N)

      IF (XVAL .LE. TABX(1)) THEN
         ALIN = TABY(1)
         RETURN
      ENDIF
      IF (XVAL .GE. TABX(N)) THEN
         ALIN = TABY(N)
         RETURN
      ENDIF

      DO I = 2, N
        IF (XVAL .LE. TABX(I)) EXIT
      END DO

      ALIN = (XVAL-TABX(I-1))*(TABY(I)-TABY(I-1))/
     &            (TABX(I)-TABX(I-1))+TABY(I-1)

      RETURN    
      END FUNCTION ALIN 
C=======================================================================


C=======================================================================
C  OUTFILES, Subroutine C.H. Porter
C  Lists output files for use by other routines
!  08/16/2005 CHP These alternate names must be coordinated with the 
!                 DSSAT shell. Do not change arbitrarily.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  03/09/2005 CHP Written.
C=======================================================================
      SUBROUTINE OUTFILES(FileName, OPCODE, Comment)

      IMPLICIT NONE
      SAVE

      INTEGER MaxFiles
      PARAMETER (MaxFiles = 31)

      CHARACTER*14, DIMENSION(MaxFiles) :: FileName
      CHARACTER*2,  DIMENSION(MaxFiles) :: OPCODE
      CHARACTER*50, DIMENSION(MaxFiles) :: Comment

      OPCODE   = '  '
      FileName = '              '
      Comment  = '                                                  '
!                          Default     Alt. filename       
!                  output filename    character code
!     -----------------------------  ---------------
      FileName(1)  = 'PlantGro.OUT  '; OPCODE(1)  = 'PG'
      FileName(2)  = 'PlantN.OUT    '; OPCODE(2)  = 'PN'
      FileName(3)  = 'PlantC.OUT    '; OPCODE(3)  = 'PC'
      FileName(4)  = 'SoilN.OUT     '; OPCODE(4)  = 'SN'
      FileName(5)  = 'SoilC.OUT     '; OPCODE(5)  = 'SC'

      FileName(6)  = 'SoilTemp.OUT  '; OPCODE(6)  = 'TS'
      FileName(7)  = 'SoilWat.OUT   '; OPCODE(7)  = 'SW'
      FileName(8)  = 'ET.OUT        '; OPCODE(8)  = 'EB'
      FileName(9)  = 'ETPhot.OUT    '; OPCODE(9)  = 'PH'
      FileName(10) = 'Pest.OUT      '; OPCODE(10) = 'PS'

      FileName(11) = 'MgmtOps.OUT   '; OPCODE(11) = 'MO'
      FileName(12) = 'FloodW.OUT    '; OPCODE(12) = 'FL'
      FileName(13) = 'Weather.OUT   '; OPCODE(13) = 'WE'
      FileName(14) = 'OVERVIEW.OUT  '; OPCODE(14) = 'OV'
      FileName(15) = 'Summary.OUT   '; OPCODE(15) = 'SU'
      FileName(16) = 'Evaluate.OUT'  ; OPCODE(16) = 'EV'

!     FUTURE:
      FileName(17) = 'Chemical.OUT  '; OPCODE(17) = 'CH'
      FileName(18) = 'SoilP.OUT     '; OPCODE(18) = 'SP'
      FileName(19) = 'Operat.OUT    '; OPCODE(19) = 'OP'
      FileName(20) = 'Environ.OUT   '; OPCODE(20) = 'EN'

!     New: 1/1/2005  
      FileName(21) = 'SOMLITC.OUT   '; OPCODE(21) = 'LC'
      FileName(22) = 'SOMLITN.OUT   '; OPCODE(22) = 'LN'
      FileName(23) = 'SOMLITP.OUT   '; OPCODE(23) = 'LP'
      FileName(24) = 'FloodN.OUT    '; OPCODE(24) = 'FN'
      FileName(25) = 'PlantP.OUT    '; OPCODE(25) = 'PP'

!     Added 3/9/2005
      FileName(26) = 'PlantNBal.OUT' ; OPCODE(26) = '  '
      FileName(27) = 'SoilNBal.OUT'  ; OPCODE(27) = '  '
      FileName(28) = 'SoilWatBal.OUT'; OPCODE(28) = '  '
      FileName(29) = 'SoilCBal.OUT'  ; OPCODE(29) = '  '
      FileName(30) = 'Warning.OUT'   ; OPCODE(30) = '  '
      FileName(31) = 'ERROR.OUT'     ; OPCODE(31) = '  '

!     ------------------------------------------------------------------
      Comment(1)  = 'Daily plant growth output file                    '
      Comment(2)  = 'Daily plant nitrogen output file                  '
      Comment(3)  = 'Daily plant carbon output file                    '
      Comment(4)  = 'Daily soil nitrogen output file                   '
      Comment(5)  = 'Daily soil carbon output file                     '

      Comment(6)  = 'Daily soil temperature output file                '
      Comment(7)  = 'Daily soil water output file                      '
      Comment(8)  = 'Daily soil-plant-atmosphere output file           '
      Comment(9)  = 'Daily evapotranspiration output file              '
      Comment(10) = 'Daily pest and disease damage output file         '

      Comment(11) = 'Daily management operations output file           '
      Comment(12) = 'Daily flood management output file                '
      Comment(13) = 'Daily weather output file                         '
      Comment(14) = 'Seasonal overview output file                     '
      Comment(15) = 'Seasonal summary output file                      '
      Comment(16) = 'Evaluation output file (simulated vs. measured)   '

      Comment(17) = 'Daily chemical applications output file           '
      Comment(18) = 'Daily soil phosphorus output file                 '
      Comment(19) = 'Operations summary output file                    '
      Comment(20) = 'Environmental modifications output file           '

      Comment(21) = 'Soil organic matter - carbon output file          '
      Comment(22) = 'Soil organic matter - nitrogen output file        '
      Comment(23) = 'Soil organic matter - phosphorus output file      '
      Comment(24) = 'Flood water nitrogen output file                  '
      Comment(25) = 'Daily plant phosphorus output                     '

      Comment(26) = 'Plant nitrogen balance                            '
      Comment(27) = 'Seasonal soil nitrogen balance                    '
      Comment(28) = 'Seasonal soil water balance                       '
      Comment(29) = 'Seasonal soil carbon balance                      '
      Comment(30) = 'Warning messages                                  '
      Comment(31) = 'Error messages                                    '

      RETURN
      END SUBROUTINE OUTFILES

      INTEGER FUNCTION SafeLenString (STRING)

      IMPLICIT  NONE

      CHARACTER(len=*) STRING
      CHARACTER(len=1) CHAR
      INTEGER   I, Length, CHARVAL

      Length = LEN(STRING)
      DO I = 1,Length
        CHAR = STRING(I:I)
        CHARVAL = ICHAR(CHAR)
        IF ((CHARVAL < 66) .OR.
     &      (CHARVAL > 90 .AND. CHARVAL < 98) .OR.
     &      (CHARVAL > 122)) THEN
          CYCLE
        ELSE
          SafeLenString = I
          EXIT
        ENDIF
      ENDDO

      RETURN
      END FUNCTION
C=======================================================================

!=======================================================================
!  LenString, Function
!
!  Function to return the length of a character string, excluding 
!     trailing blanks.  This is the same as the fortran function
!     LEN.  When the string is read from an ASCII file, the LEN
!     function does not recognize the blanks as blanks.  Same for 
!     functions ADJUSTL, ADJUSTR, LEN_TRIM.
!-----------------------------------------------------------------------
!  Revision history
!
!  10/24/2005 CHP Written
!=======================================================================

      INTEGER FUNCTION LenString (STRING)

      IMPLICIT  NONE

      CHARACTER(len=*) STRING
      CHARACTER(len=1) CHAR
      INTEGER   I, Length, CHARVAL

      Length = LEN(STRING)
      DO I = Length, 1, -1
        CHAR = STRING(I:I)
        CHARVAL = ICHAR(CHAR)
        IF ((CHARVAL < 66) .OR.
     &      (CHARVAL > 90 .AND. CHARVAL < 98) .OR.
     &      (CHARVAL > 122)) THEN
          CYCLE
        ELSE
          LenString = I
          EXIT
        ENDIF
      ENDDO

      RETURN
      END FUNCTION LenString

!=======================================================================
