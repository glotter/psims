!=======================================================================
!  FPLACE_C, Subroutine for CENTURY-based SOM/residue module.
!  Determines fertilizer placement (restructured from NTRANS)
!-----------------------------------------------------------------------
!  Revision history
!  Written
!  08/08/1992 WTB Modified for version 3 input/output (matches LeGRO)  
!  02/18/1993 PWW Header revision and minor changes   
!  05/20/1999 CHP Modular format
!  09/16/1999 CHP Return changes to SNH4, SNO3, UREA as DLTSNH4, 
!                 DLTSNO3, DLTUREA  (integration performed in NTRANS)
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!                 modules with CHP's modular format.
!  03/01/2000 AJG Added link between fertilizer incorporation and
!                 surface-residue incorporation. Renamed:
!                 IFTYPE to FERTYP, M to FERTYPE, FERCOD to FERMET.
!  04/16/2002  GH Adjusted for crop rotations
!  08/17/2002  GH Modified for Y2K  
!  08/12/2003 CHP Added I/O error checking
!  02/23/2004 AJG Changed FTYPEN for automatic fertilization.
!
!-----------------------------------------------------------------------
!  Called : CENTURY
!  Calls  : Function IDLAYR
!=======================================================================

      SUBROUTINE FPLACE_C (CONTROL,
     &  DLAYR, NLAYR, NSTRES, YRPLT,                      !Input
     &  DISTURBDEPTH, DISTURBEND, DISTURBNUM,             !Input/Output
     &  DLTSNO3, DLTSNH4, DLTUREA,                        !Input/Output
     &  AMTNIT, ANFER, CONTAINSN, DOINCORPOR,             !Output
     &  FDAY, FERDEPTH, FERMET, FERMIXPERC,               !Output
     &  FERTYP, FERTYPE, IFERI, IUOF, IUON,               !Output
     &  NAPNIT, NFERT)                                    !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      LOGICAL CONTAINSN, DOINCORPOR, IUON

      CHARACTER*1  IFERI, RNMODE
      CHARACTER*2  FERTYPEN
      CHARACTER*5  FERMETHOD, FERMET(NAPPL)
      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*30 FILEIO 
      CHARACTER*90 CHAR
      PARAMETER (ERRKEY = 'FPLACE')

      INTEGER DAP, DISTURBNUM, DOY, DYNAMIC, ERRNUM, FERTYPE,
     &  FOUND, FTYPEN, I, IDATE, IDLAYR, INCYD, IUOF, K, KMAX,
     &  L, LINC, LNUM, LUNIO, MULTI, NAPNIT, NFERT, NLAYR, 
     &  TIMDIF, YEAR, YR, YRDIF, YRDNIT, YRDOY, YRPLT, YRSIM

      INTEGER DISTURBEND(NAPPL*3), FDAY(NAPPL), FERTYP(NAPPL)

      REAL AMTNIT, DSOILN, FERDEPCUM, FERDEPTH, FERMIXPERC,
     &  FERNIT, NSTRES, SOILNC, SOILNX

      REAL ANFER(NAPPL), DISTURBDEPTH(NAPPL*3), DLAYR(NL), 
     &      DLTSNH4(NL), DLTSNO3(NL), DLTUREA(NL), FERDEP(NAPPL),
     &      PROF(NL)

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      YRDIF   = CONTROL % YRDIF
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!     Seasonal initialization - run once per season
!***********************************************************************
!      IF (DYNAMIC .EQ. RUNINIT) THEN
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------

!     Read FPLACE data from FILEIO.
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
      LNUM = 0

!     Find SIMULATION CONTROL Section.
      SECTION = '*SIMUL'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND .EQ. 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(///, 31X, A1)', IOSTAT = ERRNUM) IFERI
        LNUM = LNUM + 4
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
      ENDIF   !End of IF block on FOUND.

!     ------------------------------------------------------------------
!     Find AUTOMATIC MANAGEMENT Section
!     ------------------------------------------------------------------
      SECTION = '!AUTOM'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND .EQ. 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(//,14X,3F6.0,4X,A2)',IOSTAT=ERRNUM) 
     &        DSOILN,SOILNC,SOILNX, FERTYPEN
        LNUM = LNUM + 3
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
        READ(FERTYPEN,'(I2)',IOSTAT=ERRNUM) FTYPEN
        IF (ERRNUM .NE. 0) FTYPEN = 1
      ENDIF

!     ------------------------------------------------------------------
!     Find FERTILIZER Section
!     ------------------------------------------------------------------
      SECTION = '*FERTI'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND .EQ. 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        NFERT = 0

        DO I = 1, NAPPL
!          READ (LUNIO, 80, IOSTAT = ERRNUM, ERR = 90, END = 90) 
!     &      FDAY(I), FERTYP(I), FERMET(I), FERDEP(I), ANFER(I)
!   80     FORMAT(3X,I7,3X,I3,1X,A5,2F6.0)
          READ (LUNIO, '(3X,I7,A90)', ERR = 90, END = 90) FDAY(I), CHAR
          LNUM = LNUM + 1

          READ(CHAR,'(3X,I3,1X,A5,2F6.0)',IOSTAT=ERRNUM)
     &      FERTYP(I), FERMET(I), FERDEP(I), ANFER(I)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)
          NFERT = NFERT + 1
        ENDDO   !End of DO loop on I=1,NAPPL.
   90   CONTINUE
      ENDIF   !End of IF block on FOUND.

!     Close input file.
      CLOSE (LUNIO)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!     Initialize to zero.
      AMTNIT = 0.
      NAPNIT = 0

!-----------------------------------------------------------------------
!     Adjust for multi year runs
!-----------------------------------------------------------------------
      IF (MULTI .GT. 1 .AND. NFERT .GT. 0 .AND. IFERI .NE. 'D') THEN
!       Open the FILEIO input file.
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)

!       If the file can't be found, call an error.
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)

        SECTION = '*FERTI'

!       Find the line number from where to start reading.
        CALL FIND (LUNIO, SECTION, LNUM, FOUND)

!       If the residue section can't be found, call an error, or else
!       read the input data.
        IF (FOUND .EQ. 0) THEN
          CALL ERROR (SECTION, 42, FILEIO, LNUM)
        ELSE
          DO I = 1, NFERT
            READ(LUNIO,'(3X,I7)',IOSTAT=ERRNUM,ERR=5010,END=5010)FDAY(I)
          ENDDO   !End of DO loop on I=1,NFERT.

!         Continue here after jumping out of the DO loop with an error
!         (thus the end of the residue section was reached).
5010      CONTINUE

          IF (ERRNUM .NE. 0) BACKSPACE(LUNIO)
        ENDIF   !End of IF block on FOUND.

!       Close input file.
        CLOSE (LUNIO)

!       Adjust dates for seasonal runs.
        DO I = 1, NFERT
          CALL YR_DOY (FDAY(I), YR, IDATE)
          FDAY(I) = (YR + MULTI - 1) * 1000 + IDATE
        ENDDO   !End of DO loop on I=1,NFERT.
      ENDIF

!-----------------------------------------------------------------------
!     Adjust for crop rotations
!-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'Q') THEN
        IF (NFERT .GT. 0 .AND. FDAY(1) .LT. YRSIM .AND. 
     &    IFERI .NE. 'D') THEN
          DO I = 1, NFERT
            CALL YR_DOY (FDAY(I), YR, IDATE)
            FDAY(I) = (YR + YRDIF) * 1000 + IDATE
          ENDDO   !End of DO loop on I=1,NFERT.
        ENDIF   !End of IF block on NFERT and FDAY
      ENDIF   !End of IF block on RNMODE

!     ------------------------------------------------------------------
      YRDNIT = 0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
      CALL YR_DOY (YRDOY, YEAR, DOY)
      DAP = MAX0 (0, TIMDIF (YRPLT, YRDOY))

!     Initialize for the INCORPOR call in NTRANS.
      FERMIXPERC = 0.
      FERDEPTH = 0.

      FERNIT = 0.

!     ------------------------------------------------------------------
!     Fertilize on specified dates (YYDDD format)
!     ------------------------------------------------------------------
      IF (NFERT .GT. 0 .AND. IFERI .EQ. 'R') THEN
        DO I = 1, NFERT
          IF (YRDOY .EQ. FDAY(I)) THEN
            FERTYPE = FERTYP(I)

!           Some of the fertilizer types in fileX do not contain N,
!           though one may indicate that they do. This gives an
!           N-balance error. Set here a flag to prevent this and print a
!           warning to Warning.OUT.
            CONTAINSN = .TRUE.
            IF (FERTYPE .EQ. 13 .OR. FERTYPE .EQ. 14 .OR.
     &        FERTYPE .EQ. 15 .OR. FERTYPE .EQ. 16 .OR.
     &        FERTYPE .EQ. 18 .OR. FERTYPE .EQ. 20 .OR.
     &        FERTYPE .EQ. 21 .OR. FERTYPE .EQ. 22 .OR.
     &        FERTYPE .EQ. 24 .OR. FERTYPE .EQ. 26) THEN
              CONTAINSN = .FALSE.
            ENDIF   !End of IF block on FERTYPE.

            IF (CONTAINSN) THEN
              NAPNIT   = NAPNIT + 1
              AMTNIT   = AMTNIT + ANFER(I)
              FERNIT   = ANFER(I)
              FERDEPTH = FERDEP(I)
              FERMETHOD = FERMET(I)

!AJG: Why this??? Potassium Nitrate is corrected as Ammonium poly-phosphate, but NO3 and NH4
!are dealt with quite differently (e.g. leaching)!
              IF (FERTYPE .EQ. 17) THEN
                FERTYPE = 12
              ENDIF   !End of IF block on FERTYPE.

              GO TO 100
            ENDIF   !End of IF block on CONTAINSN.

          ELSEIF (FDAY(I) .GT. YRDOY) THEN
            GO TO 100                        
          ENDIF   !End of IF block on YRDOY>FDAY.
        ENDDO   !End of DO loop on I=1,NFERT.

!     ------------------------------------------------------------------
!     Fertilize on specified days (DDD format)
!     ------------------------------------------------------------------
      ELSE IF (NFERT .GT. 0 .AND. IFERI .EQ. 'D') THEN
        DO I = 1, NFERT
          IF (DAP .EQ. FDAY(I)) THEN
            FERTYPE   = FERTYP(I)

!           Some of the fertilizer types in fileX do not contain N,
!           though one may indicate that they do. This gives an
!           N-balance error. Set here a flag to prevent this and print a
!           warning to Warning.OUT.
            CONTAINSN = .TRUE.
            IF (FERTYPE .EQ. 13 .OR. FERTYPE .EQ. 14 .OR.
     &        FERTYPE .EQ. 15 .OR. FERTYPE .EQ. 16 .OR.
     &        FERTYPE .EQ. 18 .OR. FERTYPE .EQ. 20 .OR.
     &        FERTYPE .EQ. 21 .OR. FERTYPE .EQ. 22 .OR.
     &        FERTYPE .EQ. 24 .OR. FERTYPE .EQ. 26) THEN
              CONTAINSN = .FALSE.
            ENDIF   !End of IF block on FERTYPE.

            IF (CONTAINSN) THEN
!             FDAY(I)   = YRDOY
              NAPNIT    = NAPNIT + 1
              AMTNIT    = AMTNIT + ANFER(I)
              FERNIT    = ANFER(I)
              FERDEPTH  = FERDEP(I)
              FERMETHOD = FERMET(I)

!AJG: Why this??? Potassium Nitrate is corrected as Ammonium poly-phosphate, but NO3 and NH4
!are dealt with quite differently (e.g. leaching)!
              IF (FERTYPE .EQ. 17) THEN
                FERTYPE = 12
              ENDIF   !End of IF block on FERTYPE.

              GO TO 100                         
            ENDIF   !End of IF block on CONTAINSN.

          ELSE IF (FDAY(I) .GT. DAP) THEN
            GO TO 100                         
          ENDIF   !End of IF block on DAP>FDAY
        END DO   !End of DO loop on I=1,NFERT

!     ------------------------------------------------------------------
!     Automatic fertilization routine
!     ------------------------------------------------------------------
      ELSE IF (IFERI .EQ. 'A') THEN
        IF ((1. - NSTRES) * 100. .GT. SOILNC .AND.
     &    YRDOY .GT. (YRDNIT + 1)) THEN

!         The model only deals with N fertilizers.
          CONTAINSN = .TRUE.

!         Use the fertilizer type as set in the AUTOMATIC MANAGEMENT of fileX.
!         section of fileX.
          FERTYPE = FTYPEN

!AJG: Why this??? Potassium Nitrate is corrected as Ammonium poly-phosphate, but NO3 and NH4
!are dealt with quite differently (e.g. leaching)!
          IF (FERTYPE .EQ. 17) THEN
            FERTYPE = 12
          ENDIF   !End of IF block on FERTYPE.

!         Some of the fertilizer types in fileX do not contain N,
!         though one may indicate that they do. This gives an
!         N-balance error. Set here a flag to prevent this and print a
!         warning to Warning.OUT.
          IF (FERTYPE .EQ. 13 .OR. FERTYPE .EQ. 14 .OR.
     &      FERTYPE .EQ. 15 .OR. FERTYPE .EQ. 16 .OR.
     &      FERTYPE .EQ. 18 .OR. FERTYPE .EQ. 20 .OR.
     &      FERTYPE .EQ. 21 .OR. FERTYPE .EQ. 22 .OR.
     &      FERTYPE .EQ. 24 .OR. FERTYPE .EQ. 26) THEN
            CONTAINSN = .FALSE.
          ENDIF   !End of IF block on FERTYPE.

!         Apply the N.
          IF (CONTAINSN) THEN
            NAPNIT    = NAPNIT + 1
            AMTNIT    = AMTNIT + SOILNX
            FERNIT    = SOILNX
            FERDEPTH  = DSOILN
            FERMETHOD = FERMET(I)
            YRDNIT    = YRDOY
          ENDIF   !End of IF block on CONTAINSN.
        ENDIF   !End of IF block on NSTRES.
      ENDIF   !End of IF block on NFERT and IFERI.

100   CONTINUE

      IF (FERNIT .GT. 0.) THEN
        DO L = 1, NLAYR
          PROF(L) = 0.
        END DO   !End of DO loop on L.

        KMAX = IDLAYR (NLAYR, DLAYR, FERDEPTH)
        FERDEPCUM = 0.

        IF (KMAX .EQ. 1) THEN
          PROF(1) = 1.

!       Topsoil layer incorporation
        ELSE
          FERDEPCUM = DLAYR(1)
          PROF(1)   = DLAYR(1) / FERDEPTH

          DO L = 2, KMAX
            FERDEPCUM = FERDEPCUM + DLAYR(L)
            IF (FERDEPTH .LE. FERDEPCUM) THEN
              PROF(L) = (FERDEPTH - (FERDEPCUM - DLAYR(L))) / FERDEPTH
            ELSE
              PROF(L) = DLAYR(L) / FERDEPTH
            ENDIF   !End of IF block on FERDEPTH.
          ENDDO   !End of DO loop on L.
        ENDIF   !End of IF block on KMAX.

!       If fertilizer is incorporated, the SOM/litter decomposition rate
!       is increased for 30 days as a result of soil disturbance. Set
!       the end date for this, and the depth of the disturbed soil
!       layers for which the increased decomposition rate holds. In
!       NTRANS this will lead to setting a flag that enhances the
!       decomposition rate.
        IF (FERDEPTH .GT. 0.) THEN
          DISTURBNUM = DISTURBNUM + 1
          DISTURBEND(DISTURBNUM) = INCYD (YRDOY, 30)
          DISTURBDEPTH(DISTURBNUM) = FERDEPTH

!         Set a flag for incorporation of surface residues (done in
!         NTRANS) with the fertilizer incorporation. For initialization,
!         this is not needed, because there is no surface litter yet;
!         for carry-over residues, this should be done by a tillage
!         event.
          DOINCORPOR = .TRUE.

!         Set the percentage of the surface residues that will be
!         incorporated with the fertilizer incorporation. Set to zero
!         if superficially applied or with irrigation water, and set
!         to 100 if incorporated or deeply placed.
          IF (FERMETHOD .EQ. 'AP002' .OR. FERMETHOD .EQ. 'AP004' .OR.
     &      FERMETHOD .EQ. 'AP019' .OR. FERMETHOD .EQ. 'AP020') THEN
            FERMIXPERC = 100.
          ELSE
            FERMIXPERC = 0.
          ENDIF   !End of IF block on FERMETHOD.
        ENDIF   !End of IF block on FERDEPTH.

!       ===================================================
!       º    Fertilizer types as given in appendix 4,     º
!       º    Technical Report 1,IBSNAT (1986).            º
!       º                                                 º
!       º      1   = Ammonium Nitrate                     º
!       º      2   = Ammonium Sulphate                    º
!       º      3   = Ammonium Nitrate Sulphate            º
!       º      4   = Anhydrous Ammonia                    º
!       º      5   = Urea                                 º
!       º      51  = Urea Super Granule                   º
!       º      6   = Diammonium Phosphate                 º
!       º      7   = Monoammonium Phosphate               º
!       º      8   = Calcium Nitrate                      º
!       º      9   = Aqua Ammonia                         º
!       º     10   = Urea Ammonium Nitrate                º
!       º     11   = Calcium Ammonium Nitrate             º
!       º     12   = Ammonium poly-phosphate              º
!       º     13   = Single super phosphate               º
!       º     14   = Triple super phosphate               º
!       º     15   = Liquid phosphoric acid               º
!       º     16   = Potassium chloride                   º
!       º     17   = Potassium Nitrate                    º
!       º     18   = Potassium sulfate                    º
!       º     19   = Urea super granules                  º
!       º     20   = Dolomitic limestone                  º
!       º     21   = Rock phosphate                       º
!       º     22   = Calcitic limestone                   º
!       º     24   = Rhizobium                            º
!       º     26   = Calcium hydroxide                    º
!       º  19-22   = Reserved for control release fert.   º
!       ===================================================

!       ----------------------------------------------------------------
!       FERTYPE = 5
!       ----------------------------------------------------------------
        IF (FERTYPE .EQ. 5) THEN
          DO K = 1, KMAX
            DLTUREA(K) = DLTUREA(K) + FERNIT * PROF(K)
          END DO   !End of DO loop on K.
          IUON = .TRUE.

!         All the urea is assumed to have hydrolyzed 21 days after
!         the urea application; this happens on day IUOF. The IUON
!         flag will then be set to false (is done in SOILNI or
!         NTRANS).
          IUOF = DOY + 21

          IF (DOY .GT. 344) THEN
            IUOF = 21 - (365 - DOY)
          ENDIF   !End of IF block on DOY.

!       ----------------------------------------------------------------
!       FERTYPE = 2, 4, 6, 7, 9 or 11
!       ----------------------------------------------------------------
        ELSEIF (FERTYPE .EQ. 2 .OR. FERTYPE .EQ. 4 .OR.
     &    FERTYPE .EQ. 6 .OR. FERTYPE .EQ. 7 .OR. FERTYPE .EQ. 9 .OR.
     &    FERTYPE .EQ. 11) THEN
          DO K = 1, KMAX
            DLTSNH4(K) = DLTSNH4(K) + FERNIT * PROF(K)
          ENDDO   !End of DO loop on K.

!       ----------------------------------------------------------------
!       FERTYPE = 1 or 3
!       ----------------------------------------------------------------
        ELSEIF (FERTYPE .EQ. 1 .OR. FERTYPE .EQ. 3) THEN
          DO K = 1, KMAX
            DLTSNH4(K) = DLTSNH4(K) + 0.5 * FERNIT * PROF(K)
            DLTSNO3(K) = DLTSNO3(K) + 0.5 * FERNIT * PROF(K)
          ENDDO   !End of DO loop on K.

!       ----------------------------------------------------------------
!       FERTYPE = 8 or 12
!       ----------------------------------------------------------------
        ELSEIF (FERTYPE .EQ. 8 .OR. FERTYPE .EQ. 12) THEN
          DO K = 1, KMAX
            DLTSNO3(K) = DLTSNO3(K) + FERNIT * PROF(K)
          ENDDO   !End of DO loop on K.

!       ----------------------------------------------------------------
!       FERTYPE = 10
!       ----------------------------------------------------------------
        ELSEIF (FERTYPE .EQ. 10) THEN
          DO K = 1, KMAX
            DLTSNO3(K) = DLTSNO3(K)  + FERNIT * 0.25 * PROF(K)
            DLTSNH4(K) = DLTSNH4(K)  + FERNIT * 0.25 * PROF(K)
            DLTUREA(K) = DLTUREA(K) + FERNIT * 0.50 * PROF(K)
          ENDDO   !End of DO loop on K.

!         All the urea is assumed to have hydrolyzed 21 days after
!         the urea application; this happens on day IUOF. The IUON
!         flag will then be set to false (is done in SOILNI or
!         NTRANS).
          IUON = .TRUE.
          IUOF = DOY + 21

          IF (DOY .GT. 344) IUOF = 21 - (365 - DOY)
        ENDIF   !End of IF block on FERTYPE.
      ENDIF   !End of IF block on FERNIT.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END   !SUBROUTINE FPLACE_C

!=======================================================================
! FPLACE_C Variables
!-----------------------------------------------------------------------
! AMTNIT       Cumulative amount of N in fertilizer applications (kg[N]/ha)
! ANFER(I)   Amount of nitrogen in fertilizer applied in Ith application
!                (kg[N]/ha)
! CONTAINSN    Flag to indicate whether the fertilizer type contains N (-)
! DAP          Days after planting (d)
! DISTURBDEPTH Depth of the disturbed soil layers (cm)
! DISTURBEND   Date (YRDOY) on which the 30-day decomposition-rate enhancement
!                after soil disturbance ends (-)
! DISTURBNUM   Counter for the number of soil disturbances (-)
! DLAYR(L)  Soil thickness in layer L (cm)
! DLTSNH4(L)   Rate of change of ammonium content in soil layer L
!                (kg[N]/ha/d)
! DLTSNO3(L)   Rate of change of nitrate content in soil layer L
!                (kg[N]/ha/d)
! DLTUREA(L)   Rate of change of urea content in soil layer L (kg[N]/ha/d)
! DOINCORPOR   Flag set to TRUE for decomposition-rate enhancement after soil
!                disturbance. (-)
! DOY        Current day of simulation (d)
! DSOILN     Fertilizer depth for automatic fertilizer option (cm)
! DYNAMIC      Controls module sequence: DYNAMIC = RUNINIT, SEASINIT, RATE, 
!              INTEGR, OUTPUT, or FINAL 
! FDAY(I)    Julian date for Ith fertilizer application (YYDDD)
! FERDEP(I)  Fertilizer depth for application I (cm)
! FERDEPCUM    Cumulative soil depth (cm)
! FERDEPTH   Fertilizer depth on current day of simulation (cm)
! FERMET(I)    Fertilizer application method for Ith application 
! FERMETHOD    Fertilizer application method for current application 
! FERMIXPERC   Percentage of the surface residues that will be incorporated
!                with incorporation of newly applied fertilizers. (-)
! FERNIT     Amount of nitrogen in fertilizer applied on current day of 
!              simulation (kg [N] / ha)
! FERTYP(I)    Fertilizer type for the Ith application 
! FERTYPE    Fertilizer type for current application 
! FILEIO       Filename for INP file (e.g., IBSNAT35.INP) 
! FTYPEN     Fertilizer types 
! IDATE        Day portion of fertilizer application date (d)
! IDLAYR     Soil layer where fertilizer is placed 
! IFERI        Fertilizer switch (A = automatic, R = on specified dates 
!                (YYDDD format), D = on specified days (DDD) format.
! INCYD        
! IUOF         Critical Julian day when all urea is assumed to be
!              hydrolyzed (assumed to occur 21 days after the urea
!              application) (da)
! IUON       Flag indicating presence of urea (true or false) 
! K            Loop counter                                    
! KMAX       Maximum soil depth for fertilizer application (cm)
! LUNIO      Logical unit number for FILEIO 
! MULTI        Current simulation of multi-year simulation                       
! NAPNIT     Current number of fertilizer applications applied. 
! NFERT        Number of observed fertilizer applications                        
! NL           Maximum number of soil layers                                     
! NLAYR        Number of soil layers                                             
! NSTRES       Nitrogen stress factor (1 = no stress, 0 = max stress)                
! PROF(L)      Proportion of soil destined fertilizer for layer L
!                (fraction)
! SOILNC     Critical threshold of soil nitrogen content to trigger 
!                automatic fertilization. Measured as % of N supply to N 
!                demand 
! SOILNX     Amount of N to be applied for automatic fertilization
!                (kg[N]/ha)
! TIMDIF     Integer function which calculates the number of days between 
!                two Julian dates (da)
! YR           Year portion of date (for observed fertilizer or residue  
!                application dates)
! YRDIF      Function subroutine which calculates number of days between 
!                two dates (da)
! YRDNIT     Date of last automatic fertilizer application (YYDDD)
! YRDOY      Current day of simulation (YYDDD)
! YRPLT      Planting date (YYDDD)
! YRSIM      Start of simulation date (YYDDD)
!======================================================================
