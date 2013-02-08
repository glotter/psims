C=======================================================================
C  OPVIEW, Subroutine C.H. Porter
C  Prints overview file based on data provided by various crop models.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/01/2002 CHP Written based on OPHARV subroutine.
C  04/16/2002 GH  Added control block 
C  04/18/2002 GH  Modified DAP variable
C  08/12/2003 CHP Added I/O error checking
!  10/24/2005 CHP Added environmental & stress factors to Overview.OUT
C=======================================================================

      SUBROUTINE OPVIEW(CONTROL,  
     &    BIOMAS, COUNT, DESCRIP, IDETO, LFNUM, 
     &    Measured, PlantStres, Simulated, STGDOY, 
     &    STNAME, WTNCAN, XLAI, YIELD, YRPLT)

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1 IDETO, RNMODE, ANS
      CHARACTER*2 CROP
      CHARACTER*3 RMM
      CHARACTER*6 SECTION
      CHARACTER*6, PARAMETER :: ERRKEY = 'OPVIEW'
!      CHARACTER*8 FNAME
      CHARACTER*10 STNAME(20)
      CHARACTER*16 CROPD
      CHARACTER*12 FILEA, OUTO
      CHARACTER*25 TITLET
      CHARACTER*30 FILEIO

      INTEGER DAP, DOY, DSTRES, DYNAMIC, ERRNUM, FOUND
      INTEGER I, IPX, ISENS, LINC, LNUM, LUNIO
      INTEGER NOUTDO, NYRS, RUN, TIMDIF, TRTNO
      INTEGER YIELD, YR, YRDOY, YRPLT
      INTEGER STGDOY(20)
      INTEGER Length, LenString

      REAL AVDSTR, AVNSTR, BIOMAS, LFNUM, NSTRES
      REAL STRESN, STRESS, TURFAC, WTN, WTNCAN, XLAI

      LOGICAL FEXIST

!     Describe observed and Simulated data as character strings.
      INTEGER COUNT                           !Number of values
      CHARACTER*8 Simulated(40), Measured(40) !Values
      CHARACTER*35 DESCRIP(40)                !Descriptions

      CHARACTER*64 TITLE(3)
      DATA TITLE /
     &'        CROP GROWTH   BIOMASS         LEAF   CROP N     STRESS ',
     &'   DATE  AGE STAGE      kg/ha    LAI   NUM  kg/ha  %   H2O    N',
     &' ------  --- ---------- -----  -----  ----  ---  ---  ----  ----'
!               DAP STNAME(I) BIOMAS   XLAI LFNUM WTNCAN WTN AVDSTR AVNSTR
     &/

      INTERFACE 
        SUBROUTINE OPSTRESS(C, I, E, P, W)
          USE ModuleDefs
          TYPE (ControlType), Intent(IN)           :: C
          CHARACTER*1,        Intent(IN), Optional :: I
          REAL,               Intent(IN), Optional :: E
          TYPE (PlStresType), Intent(IN), Optional :: P
          TYPE (WeatherType), Intent(IN), Optional :: W
        END SUBROUTINE OPSTRESS
      END INTERFACE

      TYPE (ControlType) CONTROL
      TYPE (PlStresType) PlantStres

!-----------------------------------------------------------------------
!     Don't print for fallow
      CROP    = CONTROL % CROP
      IF (CROP .EQ. 'FA') RETURN

      NYRS    = CONTROL % NYRS
      RNMODE  = CONTROL % RNMODE
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      DYNAMIC = CONTROL % DYNAMIC

      NSTRES  = PlantStres % NSTRES
      TURFAC  = PlantStres % TURFAC

C***********************************************************************
C***********************************************************************
C     RUN INITIALIZATION
C***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!     Read FILEIO
      CALL GETLUN('FILEIO', LUNIO)
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)
      LNUM = 0
C-----------------------------------------------------------------------
C    Read FILE names and paths
C-----------------------------------------------------------------------
      READ (LUNIO,'(55X,I5)', IOSTAT=ERRNUM) ISENS ; LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      READ (LUNIO,'(2/,15X,A12,1X,A80)', IOSTAT=ERRNUM) FILEA
      LNUM = LNUM + 3    
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

C-----------------------------------------------------------------------
C       Find and Read TREATMENTS Section
C-----------------------------------------------------------------------
      SECTION = '*TREAT'
      CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
      IF (FOUND .EQ. 0) THEN
        CALL ERROR(SECTION, 42, FILEIO, LNUM)
      ELSE
        READ(LUNIO, '(I3,6X,A25)', IOSTAT=ERRNUM) TRTNO, TITLET
        LNUM = LNUM + 1    
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
      ENDIF
      
      CLOSE (LUNIO)

C-----------------------------------------------------------------------
      IF (IDETO .EQ. 'Y') THEN
!       Get unit number for OVERVIEW.OUT
        OUTO  = 'OVERVIEW.OUT'
        CALL GETLUN('OUTO', NOUTDO)
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      IF (IDETO .EQ. 'Y') THEN
        INQUIRE (FILE = OUTO, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = NOUTDO, FILE = OUTO, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = NOUTDO, FILE = OUTO, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(NOUTDO,'("*SIMULATION OVERVIEW FILE")')
        ENDIF
        CALL HEADER(RUNINIT, FILEIO, NOUTDO, RUN)
      ENDIF

C-----------------------------------------------------------------------
C    WRITE HEADER TO SCREEN and overview.out file
C-----------------------------------------------------------------------
      IF (INDEX('IDE',RNMODE) .GT. 0 .AND. NYRS .LE. 1) THEN
        WRITE(*,3050) RUN, TITLET, TITLE
 3050   FORMAT(
     &    '*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES',
     &    //,' RUN NO.',I6,4X,A25,/,3(/,A64))
      ENDIF

      IF (IDETO .EQ. 'Y') THEN
        WRITE (NOUTDO,3055) RUN,TITLET, (TITLE(I), I=1,3)
 3055   FORMAT(
     &    '*SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES',
     &    //,' RUN NO.',I6,4X,A25,/,3(/,A64))
      ENDIF

!     Initialize average stresses
      DSTRES = 0
      STRESS = 0.0
      STRESN = 0.0

      WTN = 0.0
      AVDSTR = 0.0
      AVNSTR = 0.0

      CALL OPSTRESS(CONTROL, I=IDETO, P=PlantStres)

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
C-----------------------------------------------------------------------
!     Accumulate water and N stresses since last stage printout
      DSTRES = DSTRES + 1
      STRESS = STRESS + TURFAC
      STRESN = STRESN + NSTRES      

!     At each plant stage, print growth data.
      DO I = 1, 20
        IF (YRDOY .EQ. STGDOY(I)) THEN

          !Compute average stresses
          IF (DSTRES .GE. 1) THEN
            AVDSTR = 1.0 - STRESS / DSTRES
            AVNSTR = 1.0 - STRESN / DSTRES
            DSTRES = 0
            STRESS = 0.0
            STRESN = 0.0
          ELSE
            AVDSTR = 0.0
            AVNSTR = 0.0
          ENDIF

          IF (BIOMAS .GT. 0.0) THEN
            WTN = WTNCAN*10. / BIOMAS * 100.
          ELSE
            WTN = 0.0
          ENDIF

          IF (YRPLT .GT. 0) THEN
            DAP = MAX(0,TIMDIF(YRPLT, YRDOY))
          ELSE
            DAP = 0
          ENDIF
          CALL YR_DOY (YRDOY, YR, DOY)
          CALL NAILUJ (DOY, YR, RMM, IPX)

          IF (INDEX('IDE',RNMODE) .GT. 0 .AND. NYRS .LE. 1) THEN
            WRITE(*,4600) IPX, RMM, DAP, STNAME(I), NINT(BIOMAS), 
     &        XLAI, LFNUM, NINT(WTNCAN*10.), WTN, AVDSTR, AVNSTR
 4600       FORMAT (1X,I2,1X,A3,1X,I4,1X,A10,I6,1X,F6.2,1X,F5.1,
     &                1X,I4,1X,F4.1,1X,F5.2,1X,F5.2)
          ENDIF

          IF (IDETO .EQ. 'Y') THEN
            WRITE(NOUTDO,4600) IPX, RMM, DAP, STNAME(I), NINT(BIOMAS), 
     &        XLAI, LFNUM, NINT(WTNCAN*10.), WTN, AVDSTR, AVNSTR
          ENDIF
        ENDIF
      ENDDO

      CALL OPSTRESS(CONTROL, P=PlantStres)

!***********************************************************************
!***********************************************************************
!     Seasonal Output 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
      CALL GET_CROPD(CROP, CROPD)
      CROPD = ADJUSTR(CROPD)

!     Plant overview data:
        IF (INDEX('IDE',RNMODE) .GT. 0 .AND. NYRS .LE. 1) THEN  
          WRITE(*,
     &          '(/,1X,"Please press < ENTER > key to continue ",2X,$)')
          READ  (*, *)
          CALL CLEAR

          WRITE(*,100)
          DO I = 1, COUNT

                !Don't allow blank Simulated value,
            IF (((Simulated(I) .NE. ' ') .AND.            !<----------!
                                                                    ! !
                !or -99 Simulated value,                            !A!
C-GH &          (INDEX(Simulated(I),'-99') .EQ. 0) .AND.            !L!
C Need to keep -99 values                                           !L!
     &          (INDEX(Simulated(I),'-99') .EQ. 0) .AND.            ! !
c-chp If Simulated values are -99, then value doesn't exist and     !O!
c      we shouldn't print it. Maize has some variables that are     !F!
c      included in OLAB array (like pod date, etc.) that            ! !
c      really don't make sense for maize.  We should probably       !T!
c      modify the OLAB array to only include values that are        !H!
c      needed. - chp 4/11/03                                        !E!
c      Values of -99 for Measured data are still printed.           !S!
                                                                    !E!
                !or blank description.                              ! !
     &          (DESCRIP(I)(1:1) .NE. ' '))    !<---------------------!

     &        .OR.                             !<---------------------!
C-CHP  OK - compromise - keep Simulated=-99 for physiological maturity!
!                                               and anthesis date     !
     &          ((INDEX(Simulated(I),'-99') .EQ. 0) .AND.    ! OR THIS!
     &           ((INDEX(DESCRIP(I),'Phys') .NE. 0) .OR.   !AND EITHER!
     &            (INDEX(DESCRIP(I),'Anthesis') .NE. 0))))   !OF THESE!
     &        THEN                             !<---------------------!
                                               
              WRITE(*,200) DESCRIP(I), Simulated(I), Measured(I)
            ENDIF
          ENDDO
          WRITE(*,250) RUN,TITLET
          READ  (5,'(A1)') ANS
          CALL CLEAR
          WRITE(*,300) CROPD, YIELD
        ENDIF

  100     FORMAT(
     &    //,"*MAIN GROWTH AND DEVELOPMENT VARIABLES",//,   
     &       "@",5X,"VARIABLE",T41,"SIMULATED     MEASURED",/,  
     &           6X,"--------",T41,"---------     --------")  
  200     FORMAT(6X,A35,A8,5X,A8)
  250     FORMAT ('*RUN ',I6,4X,': ',A22,
     &          '... Press < ENTER > key to continue',$)

        IF (IDETO .EQ. 'Y') THEN
          WRITE(NOUTDO,100)
          DO I = 1, COUNT

                !Don't allow blank Simulated value,
            IF (((Simulated(I) .NE. ' ') .AND.            !<----------!
                !or -99 predicted value,                            !T!
C-GH &          (INDEX(Simulated(I),'-99') .EQ. 0) .AND.            !H!
C Need to keep -99 values                                           !E!
                                                                    !S!
     &          (INDEX(Simulated(I),'-99') .EQ. 0) .AND.            !E!
                                                                    ! !
                !or blank description.                              !3!
     &          (DESCRIP(I)(1:1) .NE. ' '))    !<---------------------!
                  
     &        .OR.                                                 !OR!
C-CHP                                          !<---------------------!
c OK - compromise - keep Simulated=-99 for physiological maturity     !
!                                               and anthesis date     !
     &          ((INDEX(Simulated(I),'-99') .NE. 0) .AND.    ! OR THIS!
     &           ((INDEX(DESCRIP(I),'Phys') .NE. 0) .OR.   !AND EITHER!
     &            (INDEX(DESCRIP(I),'Anthesis') .NE. 0))))   !OF THESE!
     &        THEN                             !<---------------------!
                                               
              WRITE(NOUTDO,200) DESCRIP(I), Simulated(I), Measured(I)
            ENDIF
          ENDDO
        ENDIF

        CALL OPSTRESS(CONTROL, P=PlantStres)
 
        IF (IDETO .EQ. 'Y') THEN
          Length = LenString(CROPD)
          WRITE(NOUTDO,300) CROPD(1:Length), YIELD
          WRITE(NOUTDO,'(96("*"))')
  300     FORMAT(/,10X,A," YIELD : ",I8," kg/ha    [Dry weight] ",/)
        ENDIF

        CLOSE (NOUTDO)
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE OPVIEW
C=======================================================================
