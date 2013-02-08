C=======================================================================
C  COPYRIGHT 1998-2005 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C                      USDA-ARS U.S Water Conservation Laboratory
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C
C     CROPPING SYSTEM MODEL Version 4.0
C
C     Decision Support System for Agrotechnology Transfer (DSSAT)
C
C     August 31, 2005  CSM Version 4.0
C
C     Gerrit Hoogenboom, J.W. Jones, Cheryl Porter, K.J. Boote, 
C
C     Bill Batchelor, Tony Hunt, Arjan Gijsman
C
C     Paul Wilkens, Upendra Singh, Jeff W. White
C
C=======================================================================
C
C=======================================================================
C  REVISION       HISTORY
C  11/04/2001 GH  Written.
C  12/12/2001 GH  Rename to CSM and integrate with Land/CROPGRO routines
C  01/13/2002 CHP Add debug mode
C  02/02/2002 GH  Revise driver for argument calls
C  04/20/2002 GH  Revisions for sequence analysis
C  06/10/2002 GH  Revisions for outputs of sequence analysis
C  06/11/2002 GH  Modified for Y2K
C  07/22/2002 CHP Added calls to OPCLEAR and OPNAMES
C  11/25/2002 GH  Upgrade to CSM Version 3.9, 020 for December Workshop
C  08/12/2003 CHP Added I/O error checking
C  03/31/2004 GH  Upgrade to CSM Version 4.0, 040 for March 31 Release
C  09/03/2004 CHP Added GetPut_Control call to push control information
C                   into constructed variable which is accessible to
C                   all modules.  Added TRTNO to CONTROL variable.
C  11/23/2004 CHP Increased length of PATHX (path for executable) to 120.
C  02/08/2005 CHP Changed criteria for ending a sequenced run.
!  06/14/2005 CHP Added FILEX to CONTROL variable, read FILEX from FILEIO
C=======================================================================
      PROGRAM CSM

C_JED USE DFPORT

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.

      IMPLICIT NONE
      SAVE
C-----------------------------------------------------------------------
      CHARACTER*1   ANS,RNMODE,BLANK,UPCASE
      CHARACTER*6   ERRKEY,FINDCH,TRNARG
      CHARACTER*8   FNAME
      CHARACTER*100  DSCSM,INPUT,FILEB,FILEX
      CHARACTER*18  RUNARG
      CHARACTER*30  FILEARG,FILEIO,FILEIOH
      CHARACTER*80  CHARTEST
      CHARACTER*120 INPUTX
      CHARACTER*120  PATHX

      INTEGER       YRDOY,YRSIM,YRPLT,MDATE,YREND,YR,ISIM
      INTEGER       MULTI,NYRS,INCYD,YEAR,DOY,DAS,TIMDIF
      INTEGER       ERRNUM,LUNIO,TRTALL,TRTNO,EXPNO,I,RUN
      INTEGER       IP,IPX, YRSIM_SAVE, YRDIF, YRDOY_END
      INTEGER       LUNBIO,LINBIO,ISECT,IFIND,LN
      INTEGER       NREPS, REPNO

      INTEGER       SafeLenString
      LOGICAL       FEXIST, DONE

      PARAMETER (ERRKEY = 'CSM   ')      
      PARAMETER (BLANK  = ' ')

C     Define constructed variable types based on definitions in
C     ModuleDefs.for.

C     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

C     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType) ISWITCH

C-----------------------------------------------------------------------
      DONE = .FALSE.

      CALL GETLUN('FILEIO', LUNIO)
      FILEIO = 'DSSAT40.INP'

C-----------------------------------------------------------------------
C    Get argument from runtime module to determine path of the EXE files
C-----------------------------------------------------------------------
      CALL GETARG(0,PATHX)
      IPX = LNBLNK(PATHX)
      CALL GETARG(1,RNMODE)
      CALL GETARG(2,FILEARG)
      CALL GETARG(3,TRNARG)
C     CALL GETARG(4,RUNARG)
C-----------------------------------------------------------------------

C     RNMODE:  
C      I - Interactive mode.  Use model interface for exp. & trtno.
C      A - Run all treatments.  User specifies fileX on the command
C          line and the model runs all treatments
C      B - Batch mode. User defines fileX and treatment numbers in 
C          Batch file
C      E - Sensitivity analysis.  User defines fileX and treatment
C          number in Batch file 
C      D - Debug mode.  Model skips input module and reads temp
C          file from the command line
C      N - Seasonal analysis. Use Batch file to define experiment and 
C          treatments 
C      Q - Sequence analysis. Use Batch file to define experiment
C      S - Spatial.  Use Batch file to define experiment
C      G - Gene based model.  Use Batch file to define experiment
C      F - Farm model.  Use Batch file to define experiment
C      C - Command line mode.  Use input from the command line.
C-----------------------------------------------------------------------
      RNMODE = UPCASE(RNMODE)
      IF (INDEX('IANQGSFDBEC',RNMODE) .EQ. 0) THEN
         RNMODE = 'I'
      ENDIF
      IF (INDEX('AC',RNMODE).GT. 0) THEN
         READ(FILEARG(1:12),'(A12)') FILEX
      ELSE IF (INDEX('BNQSFE',RNMODE) .GT. 0) THEN
         READ(FILEARG(1:12),'(A12)') FILEB
      ELSE IF (INDEX('D',RNMODE) .GT. 0) THEN    
         READ(FILEARG(1:30),'(A30)') FILEIO
      ENDIF

      !Delete existing output files
      CALL OPCLEAR
C-----------------------------------------------------------------------
C    Delete previouse copies of temporary input file
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'D') THEN
       INQUIRE (FILE = FILEIO,EXIST = FEXIST)
       IF (FEXIST) THEN
         OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
         CLOSE (LUNIO,STATUS = 'DELETE')
       ENDIF
       LN = LEN(TRIM(FILEIO))
       FILEIOH = FILEIO
       WRITE(FILEIOH(LN:LN),'(A1)') 'H'
       INQUIRE (FILE = FILEIOH,EXIST = FEXIST)
       IF (FEXIST) THEN
         OPEN (LUNIO, FILE = FILEIOH,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
         CLOSE (LUNIO,STATUS = 'DELETE')
       ENDIF

C-----------------------------------------------------------------------
C    Create path for input module
C-----------------------------------------------------------------------
        print *, "PATHX = ", PATHX
        print *, "IPX = ", IPX

c        INPUT = 'FOO_INPUT'
c        INPUTX = 'input'

       IF (PATHX(IPX-3:IPX-3) .EQ. '.') THEN
         DSCSM = PATHX((IPX-11):IPX)
         INPUT(1:12) = 'MINPT'//DSCSM(6:12)
         INPUTX = PATHX(1:(IPX-12)) // INPUT // ' '
       ELSE
         DSCSM = PATHX((IPX-7):IPX) // '.EXE'
         INPUT(1:12) = 'MINPT'//DSCSM(6:12)
         INPUTX = PATHX(1:(IPX-8)) // INPUT // ' '
         IPX = IPX + 4
       ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Temporarily fix input file name for debugging purposes:
!     To use these Debug lines of code (letter D in column 1) with CVF:
!     1) Go to pull down menu Project -> Settings -> Fortran (Tab) ->
!       Debug (Category) -> Check box for Compile Debug(D) Lines
!     2)  Specify location of input module here:
C-DDD    INQUIRE (FILE = INPUTX,EXIST = FEXIST)
C-DDD    IF (.NOT. FEXIST) THEN
C-DDD      INPUTX = 'C:\dssat4\minpt040.exe'
C-DDD      IPX = LEN(TRIM(INPUTX))
C-DDD    ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        INQUIRE (FILE = INPUTX,EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          CALL ERROR (ERRKEY,27,INPUTX,0)
        ENDIF
C-----------------------------------------------------------------------
C    Open BATCH file
C-----------------------------------------------------------------------
        IF (INDEX('NQSFBE',RNMODE) .GT. 0) THEN
           CALL GETLUN('BATCH ', LUNBIO)
           FINDCH='*BATCH'
           OPEN (LUNBIO, FILE = FILEB,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,28,FILEB,LINBIO)
           CALL FIND (LUNBIO,FINDCH,LINBIO,IFIND)
           IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
        ENDIF
      ENDIF 
C-----------------------------------------------------------------------
C    Set run number and replication number
C-----------------------------------------------------------------------
      RUN   = 0
      REPNO = 1
      CONTROL % REPNO = REPNO

C*********************************************************************** 
C*********************************************************************** 
C     RUN INITIALIZATION
C***********************************************************************
      RUN_LOOP: DO WHILE (.NOT. DONE)
      YREND = -99
      RUN = RUN + 1
      IF ((INDEX('NSFB',RNMODE) .GT. 0) .OR. (INDEX('E',RNMODE) .GT.
     &     0 .AND. RUN .EQ. 1)) THEN
        CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST)
        IF (ISECT .EQ. 1) THEN
          READ (CHARTEST,100,IOSTAT=ERRNUM)  FILEX, TRTNO
 100      FORMAT (A12,I6)
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
          WRITE(TRNARG(1:6),'(I6)') TRTNO
        ELSE
          DONE = .TRUE.
          GO TO 2000
        ENDIF
      ENDIF
      IF (INDEX('Q',RNMODE) .GT. 0) THEN
        CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST)
        IF (ISECT .EQ. 0 .OR. RUN .EQ. 1) THEN
          REWIND(LUNBIO)
          CALL FIND (LUNBIO,FINDCH,LINBIO,IFIND)
          CALL IGNORE (LUNBIO,LINBIO,ISECT,CHARTEST)
        ENDIF
        READ (CHARTEST,100,IOSTAT=ERRNUM)  FILEX, TRTNO         
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,26,FILEB,LINBIO)
        WRITE(TRNARG(1:6),'(I6)') TRTNO
      ENDIF
      WRITE(RUNARG(1:18),'(I18)') RUN
C-----------------------------------------------------------------------
C    Run INPUT module
C-----------------------------------------------------------------------
      IF (RNMODE .NE. 'D') THEN
         IPX = LEN_TRIM(INPUTX)
         print *, 'INPUTX = ', INPUTX(1:IPX)
         print *, 'IPX=', IPX
         print *, 'FILEIO = ', FILEIO
C         INPUTX = INPUTX(1:IPX) // BLANK // TRIM(FILEIO) //
         INPUTX = '../MINPT040.EXE ' // BLANK // TRIM(FILEIO) //
     &           BLANK  // RNMODE // BLANK // RUNARG // BLANK //
     &           TRIM(FILEX) // BLANK // TRNARG
         print *,'COMMANDLINE=', INPUTX
C       INPUTX = "/devsandbox/hfsfc/CSM/Input/Input_exe DSSAT40.INP"

       I = SYSTEM(INPUTX)

       print *, 'INPUTX = ', INPUTX
       print *, 'I = ', I

       IF (I .GT. 0) THEN
         CALL ERROR('INPUTX',0,INPUTX,0)
         !STOP 'Could not run INPUT module'
       ENDIF
      ELSE
       FILEX = '            '    !Debug mode - no FILEX
      ENDIF
C-----------------------------------------------------------------------
C    Check to see if the temporary file exists
C-----------------------------------------------------------------------
      print *, 'FILEIO = ', FILEIO
      INQUIRE (FILE = FILEIO,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
        CALL ERROR(ERRKEY,2,FILEIO,LUNIO)
      ENDIF

      OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,0)
      READ (LUNIO,300,IOSTAT=ERRNUM) EXPNO,TRTNO,TRTALL
 300  FORMAT(36X,3(1X,I5))
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,1)
      READ (LUNIO,'(//,15X,A12)',IOSTAT=ERRNUM) FILEX
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,1)
      IF (RUN .EQ. 1) THEN
        READ(LUNIO,'(8(/),15X,A8)',IOSTAT=ERRNUM) FNAME    
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,13)
        READ(LUNIO,400,IOSTAT=ERRNUM) NYRS, NREPS, YRSIM
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,15)
 400    FORMAT(/,15X,I5,1X,I5,7X,I7)
      ELSE IF (RNMODE .NE. 'Q') THEN
        READ(LUNIO,500,IOSTAT=ERRNUM) NYRS, NREPS, YRSIM
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,15)
 500    FORMAT(10(/),15X,I5,1X,I5,7X,I7)
      ENDIF
      CLOSE(LUNIO)

      IF (NYRS > 1) THEN
        YRSIM_SAVE = YRSIM
      ENDIF

      IF (INDEX('FQ',RNMODE) .GT. 0) THEN
        IF (RUN .EQ. 1) THEN
          CALL YR_DOY(YRSIM,YR,ISIM)
!          YEAR_END = YR + NYRS
          YRDOY_END = (YR + NYRS) * 1000 + ISIM
          YRDOY_END = INCYD(YRDOY_END, -1)
        ENDIF
        NYRS  = 1
      ENDIF

      IF (RNMODE .NE. 'Q' .OR. RUN .EQ. 1) THEN
        YRDOY = YRSIM
      ENDIF
      IF (INDEX('FQ',RNMODE).GT. 0 .AND. RUN .GT. 1) THEN
         YRSIM = INCYD(YRDOY,1)
      ENDIF
       
      MULTI  = 0
      YRDIF  = 0
       
      CONTROL % FILEIO  = FILEIO
      CONTROL % FILEX   = FILEX
      CONTROL % NYRS    = NYRS
      CONTROL % MULTI   = MULTI
      CONTROL % RNMODE  = RNMODE
      CONTROL % RUN     = RUN
      CONTROL % TRTNO   = TRTNO
      CONTROL % YRDIF   = YRDIF
      CONTROL % YRDOY   = YRDOY
      CONTROL % YRSIM   = YRSIM
      CONTROL % DYNAMIC = RUNINIT
      CALL GETPUT_CONTROL('PUT', CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)
            
C*********************************************************************** 
C*********************************************************************** 
C-----------------------------------------------------------------------
C     BEGINNING of SEASONAL SIMULATION loop
C-----------------------------------------------------------------------
C     SEASONAL INITIALIZATION
C*********************************************************************** 
      SEAS_LOOP: DO WHILE (MULTI .NE. NYRS)
C***********************************************************************
      IF (NYRS .GT. 1) THEN 
        MULTI = MULTI + 1
      ELSE
        MULTI = 1
      ENDIF
      IF (MULTI .GT. 1) THEN
        RUN   = RUN + 1
        YRSIM = YRSIM_SAVE
        CALL YR_DOY(YRSIM,YR,ISIM)
        YRSIM = (YR + MULTI - 1) * 1000 + ISIM
        YREND = -99
      ENDIF
      IF (RNMODE .NE. 'Q' .OR. RUN .GT. 1) THEN
        YRDOY = YRSIM
      ENDIF
      
      CONTROL % RUN     = RUN
      CONTROL % YRSIM   = YRSIM
      CONTROL % YRDOY   = YRDOY
      CONTROL % MULTI   = MULTI
      CONTROL % DYNAMIC = SEASINIT
      CALL GETPUT_CONTROL('PUT', CONTROL)
   
      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

      YRDOY = INCYD(YRDOY,-1)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     BEGINNING of DAILY SIMULATION loop
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DAY_LOOP: DO WHILE (YRDOY .GT. YREND)
C-----------------------------------------------------------------------
C     Increment day (YRDOY)
C-----------------------------------------------------------------------
      YRDOY = INCYD(YRDOY,1)

C-----------------------------------------------------------------------
C     Calculate days after simulation (DAS) 
C-----------------------------------------------------------------------
      CALL YR_DOY(YRDOY,YEAR,DOY)
c     IF (RNMODE .NE. 'Q') THEN
         DAS   = MAX(0,TIMDIF(YRSIM,YRDOY))
c     ELSE
c        DAS   = MAX(0,TIMDIF(YRSIM_SAVE,YRDOY))
c     ENDIF
      CONTROL % YRDOY   = YRDOY
      CONTROL % DAS     = DAS
C*********************************************************************** 
C     RATE CALCULATIONS
C*********************************************************************** 
      CONTROL % DYNAMIC = RATE
      CALL GETPUT_CONTROL('PUT', CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C*********************************************************************** 
C     INTEGRATION 
C*********************************************************************** 
      CONTROL % DYNAMIC = INTEGR
      CALL GETPUT_CONTROL('PUT', CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C*********************************************************************** 
C     OUTPUT
C*********************************************************************** 
      CONTROL % DYNAMIC = OUTPUT
      CALL GETPUT_CONTROL('PUT', CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C***********************************************************************
      ENDDO DAY_LOOP   !End of daily loop
C-----------------------------------------------------------------------
C     END of DAILY SIMULATION loop
C----------------------------------------------------------------------
C*********************************************************************** 
C     FINAL 
C*********************************************************************** 
      CONTROL % DYNAMIC = FINAL
      CALL GETPUT_CONTROL('PUT', CONTROL)

      CALL LAND(CONTROL, ISWITCH, 
     &          YRPLT, MDATE, YREND)

C-----------------------------------------------------------------------
      ENDDO SEAS_LOOP  
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     END of SEASONAL SIMULATION loop
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C    Check to see if all treatments have been run for RNMODE = 'A'
C-----------------------------------------------------------------------
      I = INDEX('A', RNMODE)
      IF (INDEX('A',RNMODE) .GT. 0 .AND. TRTNO .GE. TRTALL) THEN
         DONE = .TRUE.
      
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      ELSE IF (INDEX('GDC',RNMODE) .GT. 0) THEN
        DONE = .TRUE.
!      ELSE IF (INDEX('FQ',RNMODE).GT. 0 .AND. YEAR .GE. YEAR_END)  THEN
      ELSE IF (INDEX('FQ',RNMODE).GT. 0 .AND. YRDOY .GE. YRDOY_END) THEN
        REPNO = REPNO + 1
        CONTROL % REPNO = REPNO
        IF (REPNO .GT. NREPS) THEN
          DONE = .TRUE.
        ELSE
          RUN = 0
        ENDIF
      ELSE IF (INDEX('IE',RNMODE) .GT. 0) THEN
        WRITE(*,1700)
 1700   FORMAT(/,1X,'Do you want to run more simulations ? ',
     &         /,1X,'Y or N ? [Default = "N"] ===> ',$)
        READ (5,1800) ANS
 1800   FORMAT(A1)
        ANS = UPCASE(ANS)
        IF (ANS .NE. 'Y') DONE = .TRUE.
      ENDIF

 2000 CONTINUE
      END DO RUN_LOOP 

      !Change output file names if FNAME set
      CALL OPNAMES(FNAME)

      END PROGRAM CSM  ! PROGRAM

!===========================================================================
! Variable listing for main program
! ---------------------------------
! BLANK   Blank character 
! CONTROL Composite variable containing variables related to control and/or 
!           timing of simulation.  The structure of the variable 
!           (ControlType) is defined in ModuleDefs.for. 
! DAS     Days after start of simulation (d)
! DONE    Logical variable. TRUE if all runs have been completed. FALSE 
!           otherwise. 
! DOY     Current day of simulation (d)
! DSCSM   Name of CSM model executable (i.e., DSCSM040.EXE)
! ERRKEY  Subroutine name for error file 
! ERRNUM  Error number for input 
! EXPNO   Experiment number 
! FEXIST  Logical variable 
! FILEARG Run-time argument which contains name of input file (either 
!           FILEIO, FILEB or FILEX depending on run mode). 
! FILEB   Name of batch file (i.e., D4batch.dv4) 
! FILEIO  Filename for input file (e.g., IBSNAT35.INP) 
! FILEX   Experiment file, e.g., UFGA7801.SBX 
! FNAME   Output file name, usually 'OVERVIEW' 
! I       Loop counter 
! INPUT   Name of input module executable (i.e., MINPT040.EXE) 
! INPUTX  Command line for system call to run input module. 
! IP      Return status of GETARG command 
! IPX     Length of path plus filename for CSM executable 
! ISECT   Indicator of completion of IGNORE routine: 0 - End of file 
!           encountered, 1 - Found a good line to read, 2 - End of Section 
!           in file encountered denoted by * in column 1. 
! ISIM    Day portion of Julian date 
! ISWITCH Composite variable containing switches which control flow of 
!           execution for model.  The structure of the variable 
!           (SwitchType) is defined in ModuleDefs.for. 
! LN      Pest number 
! LUNIO   Logical unit number for FILEIO 
! MDATE   Harvest maturity date (YYYYDDD)
! MULTI   Current simulation year (=1 for first or single simulation, =NYRS 
!           for last seasonal simulation) 
! NREPS   Number of replications for sequenced simulation 
! NYRS    Number of years of simulations to perform for multi-season run 
!           (each with identical intitial conditions, but different weather 
!           years) 
! REPNO   Replication number for current simulation 
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RUN     Change in date between two observations for linear interpolation 
! TRTNO   Treatment number being simulated (from FILEX) 
! YEAR    Year of current date of simulation 
! YR      Year portion of date 
! YRDIF   Increment in years which must be added to operations dates for 
!           seasonal or sequenced simulations (yr)
! YRDOY   Current day of simulation (YYYYDDD)
! YREND   Date for end of season (usually harvest date) (YYYYDDD)
! YRPLT   Planting date (YYYYDDD)
! YRSIM   Start of simulation date (YYYYDDD)
!===========================================================================
