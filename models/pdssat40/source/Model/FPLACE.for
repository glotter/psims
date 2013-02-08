C=======================================================================
C  FPLACE, Subroutine
C  Determines fertilizer placement (restructured from NTRANS)
C-----------------------------------------------------------------------
C  Revision history
C  Written
C  08/08/1992 WTB Modified for version 3 input/output (matches LeGRO)  
C  02/18/1993 PWW Header revision and minor changes   
C  05/20/1999 CHP Modular format
C  09/16/1999 CHP Return changes to SNH4, SNO3, UREA as DLTSNH4, 
C                 DLTSNO3, DLTUREA  (integration performed in NTRANS)
C  03/16/2000 GH  Incorporated in CROPGRO
C  04/06/2000 AJG Renamed IFTYPE to FERTYP, M to FERTYPE, FERCOD to FERMET
C  07/01/2000 GH  Renamed FERDEP to FERDEPTH, DFERT to FERDEP, 
C                 CD to CUMDEP
C  04/16/2002 GH  Adjusted for crop rotations
C  08/19/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C  10/28/2004 CHP Fixed problem with multiple applications on same day.
C  01/22/2005 CHP/US Fixed problem with fertilizer distribution for 
C                      deep placement.
C  03/17/2005 CHP Moved fertilizer distribution routine to separate 
C                 subroutine to accommodate multiple applications on a
C                 single day with different fertilizer types and depths.
C-----------------------------------------------------------------------
C  Called : NTRANS
C  Calls  : Function IDLAYR
C=======================================================================

      SUBROUTINE FPLACE (CONTROL,
     &  DLAYR, FLOOD, NLAYR, NSTRES, YRPLT,               !Input
     &  DLTSNO3, DLTSNH4, DLTUREA, OXLAYR, FLOODN,        !Input/Output
     &  AMTNIT, ANFER, FDAY, FERMET, FERTYPE,             !Output
     &  IFERI, IUOF, IUON, LFD10, NAPNIT, NFERT)          !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE FloodModule

      IMPLICIT  NONE
      SAVE

      LOGICAL IUON

      CHARACTER*1  IFERI, RNMODE
      CHARACTER*2  FERTYPEN
      CHARACTER*5  FERMET(NAPPL)
      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*30 FILEIO 
      CHARACTER*90 CHAR
      PARAMETER (ERRKEY = 'FPLACE')

      INTEGER DAP, ERRNUM, FOUND, FTYPEN
      INTEGER I, IDATE, IUOF, LINC, LNUM, LUNIO
      INTEGER FERTYPE, MULTI, NAPNIT, NFERT, NLAYR
      INTEGER TIMDIF, YEAR, YRDIF, YRDNIT, YRDOY, YRPLT, YRSIM
      INTEGER FDAY(NAPPL), FERTYP(NAPPL)
      INTEGER DYNAMIC

      REAL AMTNIT, DSOILN, FERDEPTH, FERNIT
      REAL NSTRES, SOILNC, SOILNX
      REAL DLAYR(NL), DLTSNO3(NL), DLTSNH4(NL), DLTUREA(NL)
      REAL ANFER(NAPPL), FERDEP(NAPPL)

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.

      TYPE (ControlType) CONTROL
      TYPE (FloodNType)  FloodN
      TYPE (OxLayerType) OxLayr

!     Added for RICE model:
      LOGICAL UNINCO
      INTEGER LFD10, METFER
      REAL FLOOD

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

C***********************************************************************
C***********************************************************************
C     Seasonal initialization - run once per season
C***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
C-----------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      YRDIF   = CONTROL % YRDIF
      YRSIM   = CONTROL % YRSIM

!     Read FPLACE data from FILEIO.
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
      LNUM = 0

!     Find SIMULATION CONTROL Section.
      SECTION = '*SIMUL'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND == 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(///, 31X, A1)', IOSTAT = ERRNUM) IFERI
        LNUM = LNUM + 4
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
      ENDIF

!     ------------------------------------------------------------------
!     Find AUTOMATIC MANAGEMENT Section
!     ------------------------------------------------------------------
      SECTION = '!AUTOM'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND == 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        READ (LUNIO,'(//,14X,3F6.0,4X,A2)',IOSTAT=ERRNUM) 
     &        DSOILN,SOILNC,SOILNX, FERTYPEN
        LNUM = LNUM + 3
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
        READ(FERTYPEN,'(I2)',IOSTAT=ERRNUM) FTYPEN
        IF (ERRNUM .NE. 0) FTYPEN = 1
      ENDIF

!###AJG  Needs an automatic P fertilizer option in fileX ???

!     ------------------------------------------------------------------
!     Find FERTILIZER Section
!     ------------------------------------------------------------------
      SECTION = '*FERTI'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

      IF (FOUND == 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
        NFERT = 0

        DO I = 1, NAPPL
          READ (LUNIO, '(3X,I7,A90)', ERR = 90, END = 90) FDAY(I), CHAR
          LNUM = LNUM + 1

          READ(CHAR,'(3X,I3,1X,A5,2F6.0)',IOSTAT=ERRNUM)
     &      FERTYP(I), FERMET(I), FERDEP(I), ANFER(I)
          IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)

!         The number of fertilizer applications to be done in this run.
          NFERT = NFERT + 1
        ENDDO
   90   CONTINUE
      ENDIF

      CLOSE (LUNIO)

C***********************************************************************
C***********************************************************************
C     Seasonal initialization - run once per season
C***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
C-----------------------------------------------------------------------
      AMTNIT = 0.             !from INPLNT
      NAPNIT = 0              !from INPLNT
      LFD10 = 0.0 !Check for sequenced runs
C-----------------------------------------------------------------------
C     Adjust for multi year runs
C-----------------------------------------------------------------------
      IF (MULTI .GT. 1 .AND. NFERT .GT. 0 .AND. IFERI .NE. 'D') THEN
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)

        SECTION = '*FERTI'
        CALL FIND (LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) THEN
          CALL ERROR (SECTION, 42, FILEIO, LNUM)
        ELSE
          DO I = 1, NFERT
            READ(LUNIO,'(3X,I7)',IOSTAT=ERRNUM,ERR=5010,END=5010)FDAY(I)
          ENDDO
5010      CONTINUE
        ENDIF
        CLOSE (LUNIO)

!       Adjust dates for seasonal runs.
        DO I = 1, NFERT
          CALL YR_DOY (FDAY(I), YEAR, IDATE)
          FDAY(I) = (YEAR + MULTI - 1) * 1000 + IDATE
        ENDDO
      ENDIF

C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'Q') THEN
        IF (NFERT .GT. 0 .AND. FDAY(1) .LT. YRSIM .AND. 
     &          IFERI .NE. 'D') THEN
          DO I = 1, NFERT
            CALL YR_DOY (FDAY(I), YEAR, IDATE)
            FDAY(I) = (YEAR + YRDIF) * 1000 + IDATE
          ENDDO
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
      YRDNIT = 0
      UNINCO = .FALSE.

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN
!-----------------------------------------------------------------------
      FERNIT = 0.

!-----------------------------------------------------------------------
!     Fertilize on specified dates (YYDDD format)
!-----------------------------------------------------------------------
      IF (NFERT .GT. 0 .AND. IFERI .EQ. 'R') THEN
        DO I = 1, NFERT
          IF (YRDOY .EQ. FDAY(I)) THEN
            NAPNIT   = NAPNIT + 1
            AMTNIT   = AMTNIT + ANFER(I)
            FERNIT   = ANFER(I)
            FERDEPTH = FERDEP(I)
C-----------------------------------------------------------------------
C              Convert character codes for fertilizer method into
C              the special numerical code the rice model uses
C-----------------------------------------------------------------------
            READ (FERMET(I)(4:5),'(I2)') METFER
            FERTYPE  = FERTYP(I)
            IF (FERTYPE .EQ. 17) THEN
              FERTYPE = 12
            ENDIF

            IF (FERNIT > 1.E-3) THEN
              CALL FertApply(
     &          DLAYR, FERDEPTH, FERNIT, FERTYPE, FLOOD,  !Input
     &          METFER, NLAYR, YRDOY,                     !Input
     &          FloodN, OxLayr, DLTSNH4, DLTSNO3, DLTUREA,!I/O
     &          IUOF, IUON, LFD10, UNINCO)                !Output
            ENDIF

          ELSE IF (FDAY(I) .GT. YRDOY) THEN
            EXIT                        
          ENDIF
        END DO

!-----------------------------------------------------------------------
!     Fertilize on specified days (DDD format)
!-----------------------------------------------------------------------
      ELSE IF (NFERT .GT. 0 .AND. IFERI .EQ. 'D') THEN
        DAP = MAX (0, TIMDIF(YRPLT, YRDOY))
        DO I = 1, NFERT
          IF ((FDAY(I) .NE. 0 .AND. DAP .EQ. FDAY(I)) .OR.
     &        (FDAY(I) .EQ. 0 .AND. YRDOY .EQ. YRPLT)) THEN
C           FDAY(I)  = YRDOY
            NAPNIT   = NAPNIT + 1
            AMTNIT   = AMTNIT + ANFER(I)
            FERNIT   = ANFER(I)
            FERDEPTH = FERDEP(I)
            READ (FERMET(I)(4:5),'(I2)') METFER
            FERTYPE  = FERTYP(I)
            IF (FERTYPE .EQ. 17) THEN
              FERTYPE = 12
            ENDIF

            IF (FERNIT > 1.E-3) THEN
              CALL FertApply(
     &          DLAYR, FERDEPTH, FERNIT, FERTYPE, FLOOD,  !Input
     &          METFER, NLAYR, YRDOY,                     !Input
     &          FloodN, OxLayr, DLTSNH4, DLTSNO3, DLTUREA,!I/O
     &          IUOF, IUON, LFD10, UNINCO)                !Output
            ENDIF

          ELSE IF (FDAY(I) .GT. DAP) THEN
            EXIT                         
          ENDIF
        END DO

!-----------------------------------------------------------------------
!     Automatic fertilization routine
!-----------------------------------------------------------------------
      ELSE IF (IFERI .EQ. 'A') THEN
        IF ((1. - NSTRES) * 100. .GT. SOILNC .AND.
     &    YRDOY .GT. (YRDNIT + 1)) THEN
          NAPNIT   = NAPNIT + 1
          AMTNIT   = AMTNIT + SOILNX
          FERNIT   = SOILNX
          FERDEPTH = DSOILN
          YRDNIT   = YRDOY
          METFER = 1

          FERTYPE = FTYPEN
          IF (FERTYPE .EQ. 17) THEN
            FERTYPE = 12
          ENDIF

          IF (FERNIT > 1.E-3) THEN
            CALL FertApply(
     &        DLAYR, FERDEPTH, FERNIT, FERTYPE, FLOOD,    !Input
     &        METFER, NLAYR, YRDOY,                       !Input
     &        FloodN, OxLayr, DLTSNH4, DLTSNO3, DLTUREA,  !I/O
     &        IUOF, IUON, LFD10, UNINCO)                  !Output
          ENDIF

        ENDIF
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      OxLayr % UNINCO  = UNINCO

      RETURN
      END SUBROUTINE FPLACE


C=======================================================================
C  FertApply, Subroutine
C
C  Distributes N fertilizer constituents to soil layers
C-----------------------------------------------------------------------
C  Revision history
C
C  03/17/2005 CHP pulled N fertilizer distribution from FPLACE 
C=======================================================================
      SUBROUTINE FertApply(
     &    DLAYR, FERDEPTH, FERNIT, FERTYPE, FLOOD,        !Input
     &    METFER, NLAYR, YRDOY,                           !Input
     &    FloodN, OxLayr, DLTSNH4, DLTSNO3, DLTUREA,      !I/O
     &    IUOF, IUON, LFD10, UNINCO)                      !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule
      IMPLICIT NONE
      SAVE

      LOGICAL IUON

      CHARACTER*6, PARAMETER :: ERRKEY = 'FPLACE'
      INTEGER DOY
      INTEGER I, IDLAYR, IUOF, K, KMAX, L
      INTEGER FERTYPE, NLAYR
      INTEGER YEAR, YRDOY

      REAL CUMDEP, FERDEPTH, FERNIT
      REAL DLAYR(NL), PROF(NL), DLTSNO3(NL), DLTSNH4(NL), DLTUREA(NL)

!     Added for RICE model:
      LOGICAL DAILY, UNINCO
      INTEGER INCYD, KAPPM, KD, LFD10, METFER
      REAL FME(10)    !Fertilizer mixing efficiency
      REAL DLTFUREA, DLTFNO3, DLTFNH4 
      CHARACTER*78  MESSAGE(10)
      REAL FLOOD, DLTOXU, DLTOXH4, DLTOXN3

      TYPE (FloodNType)  FloodN
      TYPE (OxLayerType) OxLayr

      DLTFUREA = FloodN % DLTFUREA
      DLTFNH4  = FloodN % DLTFNH4
      DLTFNO3  = FloodN % DLTFNO3

      DLTOXU  = OxLayr % DLTOXU
      DLTOXH4 = OxLayr % DLTOXH4
      DLTOXN3 = OxLayr % DLTOXN3
      DAILY   = OxLayr % DAILY

!-----------------------------------------------------------------------
      DATA FME/0.0,0.15,0.30,0.45,0.60,0.75,0.90,0.92,0.95,1.0/
!               1    2    3    4    5    6    7    8    9   10  
!       METFER 10   11   12   13   14   15   16   17   18   19
C
C           MIXING EFFICIENCY BASED ON BURESH ET AL.
C
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
C
C     Uses mixing efficiency (FME) input to determine where fertilizer goes.
C     If mixing efficiency is zero all fertilizer is in floodwater.
C     If mixing efficiency is 1.0 all fertilizer retained in soil.
C     Function IDLAYR is used to identify layers for deep placement.
C     Routine assumes uniform incorporation within a layer for deep
C     point placed sources.
C
C     For all BI treatments, the routine will distribute the soil fraction
C     of the fertilizer over the layers encompassed by the incorporation
C     depth.
C
C     Need to make provision for USG as a source
!-----------------------------------------------------------------------
!      IF (FERNIT .GT. 0.) THEN
        KMAX = 1
        PROF = 0.

        DAILY = .FALSE.
!        LFD10 = DOY + 10
        LFD10 = INCYD(YRDOY, 10)  !Use YRDOY format, increment 10 days

         !
         ! Adjust KAPPM based on METhod of FERtilization
         !
         SELECT CASE (METFER)
           CASE (0)
             KAPPM = 10                  ! Applied when required
           CASE (1,3)
             KAPPM = 1                   ! No incorporation  (0.0)
           CASE (2)
             KAPPM = 10                  ! Completely incorporated
           CASE (4:9)
             KAPPM = 10                  ! Assume 100% for other's
           CASE (10:19)
             KAPPM = METFER - 9          ! Range from 0.15 to 0.95
           CASE DEFAULT
             KAPPM = 10                  ! Should not be here
         END SELECT

        IF (KAPPM <= 9) THEN
            UNINCO = .TRUE.
         ENDIF

! @CDE  FME  I DISTR   DESCRIPTION                                            
! AP001   0  1 Surface Broadcast, not incorporated                                          
! AP002 100 10 Layers  Broadcast, incorporated                                
! AP003   0  1 Surface Banded on surface                                      
! AP004 100 10 Deep    Banded beneath surface                                 
! AP005                Applied in irrigation water                            
! AP006                Foliar spray                                           
! AP007 100 10 Deep    Bottom of hole                                         
! AP008                On the seed                                            
! AP009 100 10 Deep    Injected                                               
! AP011   0  1 Surface Broadcast on flooded/saturated soil, none in soil      
! AP012  15  2 Layers  Broadcast on flooded/saturated soil, 15% in soil       
! AP013  30  3 Layers  Broadcast on flooded/saturated soil, 30% in soil           
! AP014  45  4 Layers  Broadcast on flooded/saturated soil, 45% in soil       
! AP015  60  5 Layers  Broadcast on flooded/saturated soil, 60% in soil       
! AP016  75  6 Layers  Broadcast on flooded/saturated soil, 75% in soil       
! AP017  90  7 Layers  Broadcast on flooded/saturated soil, 90% in soil       
! AP018  92  8 Layers  Band on saturated soil,2cm flood, 92% in soil          
! AP019  95  9 Deep    Deeply placed urea super granules/pellets, 95% in soil 
! AP020 100 10 Deep    Deeply placed urea super granules/pellets, 100% in soil

        SELECT CASE (METFER)
          CASE (1,3,11)
!           Surface placement
            KMAX = 1
            PROF(1) = 1.0

          CASE (2,5,6,12:18)  !Do 5 and 6 belong here?
C            These treatments are incorporated
C            Remainder of fertilizer (FME) distributed over layers
             KMAX = IDLAYR (NLAYR,DLAYR,FERDEPTH)
             CUMDEP   = 0.0
            IF (KMAX == 1) THEN

C              Incorporation shallow - only surface layer
               PROF(1) = 1.0
             ELSE
               CUMDEP = DLAYR(1)
               PROF(1)   = DLAYR(1) / FERDEPTH

               DO L = 2, KMAX
                 CUMDEP = CUMDEP + DLAYR(L)
                IF (FERDEPTH <= CUMDEP) THEN
                   PROF(L) = (FERDEPTH - (CUMDEP - DLAYR(L))) / FERDEPTH
                 ELSE
                   PROF(L) = DLAYR(L) / FERDEPTH
                 ENDIF
               END DO
             ENDIF

          CASE (4,7,8,9,19,20)
C           This is deep placement
C           All fertilizer placed in layer KD with (PROF = 1.0)
            KD = IDLAYR (NLAYR,DLAYR,FERDEPTH)
            IF (KD == 1) THEN
              CALL YR_DOY(YRDOY, YEAR, DOY)
              WRITE (MESSAGE(1),1000) YEAR, DOY,  FERTYPE,  FERDEPTH
              WRITE (MESSAGE(2),1001)
              WRITE (MESSAGE(3),1002)
 1000         FORMAT('Day ',I4,1X,I3,'. Fertilizer type ',I3,
     &            '. Depth ', F5.2)
 1001         FORMAT('Deep placement of fertilizer could not ',
     &            'be accomodated.')
 1002         FORMAT('Model is forcing deep placement into ',
     &            'second layer.')
              CALL WARNING(3, ERRKEY, MESSAGE)
              KD = 2
            ENDIF
            PROF(KD) = 1.0
            KMAX     = KD
         END SELECT

        IF (KMAX > 1 .AND. FME(KAPPM) < 0.95) THEN
           DO I = 1, KMAX
             SELECT CASE (I)
               CASE (1)
                IF (PROF(I)*1.2 < 1.0) THEN
                   PROF(I) = PROF(I)*1.2
                 ENDIF

               CASE (2)
                 PROF(I)   = 1.0 - PROF(I-1)

               CASE (3)
                 PROF(I)   = PROF(I-1) - PROF(I)
                 PROF(I)   = AMAX1 (PROF(I),0.0)
                 PROF(I-1) = PROF(I-1) - PROF(I)

               CASE DEFAULT
                 PROF(I)   = 0.0
             END SELECT
           END DO
         ENDIF

         IF (FERTYPE .EQ. 51) THEN
            FERTYPE = 19
         ENDIF

         SELECT CASE (FERTYPE)

           CASE (1,3,11)
C            Ammonium nitrate
C                                                     
             DLTFNH4 = DLTFNH4 + FERNIT * 0.5 * (1.0 - FME(KAPPM))
             DLTFNO3 = DLTFNO3 + FERNIT * 0.5 * (1.0 - FME(KAPPM))
             DO K = 1, KMAX
               DLTSNH4(K) = DLTSNH4(K) + 0.5 *FERNIT *FME(KAPPM)*PROF(K)
               DLTSNO3(K) = DLTSNO3(K) + 0.5 *FERNIT *FME(KAPPM)*PROF(K)
             END DO

           CASE (2,4,6,7,9,12)
C            Ammonium source
C
             DLTFNH4 = DLTFNH4 + FERNIT * (1.0-FME(KAPPM))
             DO K = 1, KMAX
               DLTSNH4(K) = DLTSNH4(K) + FERNIT * FME(KAPPM) * PROF(K)
             END DO

           CASE (5,19)
C            Urea source - switch on hydrolysis
C
             DLTFUREA = DLTFUREA + FERNIT * (1.0 - FME(KAPPM))
             DO K = 1, KMAX
               DLTUREA(K) = DLTUREA(K) + FERNIT * FME(KAPPM) * PROF(K)
             END DO
C            All the urea is assumed to have hydrolyzed 21 days after
C            the urea application; this happens on day IUOF. The IUON
C            flag will then be set to false (is done in SOILNI or
C            NTRANS).
             IUON = .TRUE.
             CALL YR_DOY (YRDOY, YEAR, DOY)
             IUOF = DOY + 21
             IF (DOY .GT. 344) THEN
                IUOF = 21 - (365-DOY)
             ENDIF

           CASE (8,17)
C            Nitrate source
C
             DLTFNO3 = DLTFNO3 + FERNIT * (1.0 - FME(KAPPM))
             DO K = 1, KMAX
               DLTSNO3(K) = DLTSNO3(K) + FERNIT * FME(KAPPM) * PROF(K)
             END DO

           CASE (10)
C            UREA Ammonium Nitrate
C
             DLTFNO3  = DLTFNO3  + FERNIT * 0.25 * (1.0 - FME(KAPPM))
             DLTFNH4  = DLTFNH4  + FERNIT * 0.25 * (1.0 - FME(KAPPM))
             DLTFUREA = DLTFUREA + FERNIT * 0.50 * (1.0 - FME(KAPPM))
             DO K = 1, KMAX
               DLTSNO3(K) = DLTSNO3(K) + FERNIT *0.25*FME(KAPPM)*PROF(K)
               DLTSNH4(K) = DLTSNH4(K) + FERNIT *0.25*FME(KAPPM)*PROF(K)
               DLTUREA(K) = DLTUREA(K) + FERNIT *0.50*FME(KAPPM)*PROF(K)
             END DO

!            All the urea is assumed to have hydrolyzed 21 days after
!            the urea application; this happens on day IUOF. The IUON
!            flag will then be set to false (is done in SOILNI or
!            NTRANS).
             IUON = .TRUE.
             CALL YR_DOY (YRDOY, YEAR, DOY)
             IUOF = DOY + 21

             IF (DOY .GT. 344) THEN
               IUOF = 21 - (365 - DOY)
             ENDIF

           CASE (13,14,15,16,18)
C            The rest!!
C
             RETURN
         END SELECT

         IF (FLOOD .EQ. 0.0) THEN
            DLTOXU  = DLTOXU  + DLTFUREA
            DLTOXH4 = DLTOXH4 + DLTFNH4
            DLTOXN3 = DLTOXN3 + DLTFNO3

            DLTUREA(1) = DLTUREA(1) + DLTFUREA
            DLTSNH4(1) = DLTSNH4(1) + DLTFNH4
            DLTSNO3(1) = DLTSNO3(1) + DLTFNO3
            DLTFUREA= 0.0
            DLTFNH4 = 0.0
            DLTFNO3 = 0.0
        ENDIF
!      ENDIF

      FloodN % DLTFUREA = DLTFUREA
      FloodN % DLTFNH4  = DLTFNH4
      FloodN % DLTFNO3  = DLTFNO3

      OxLayr % DLTOXU  = DLTOXU
      OxLayr % DLTOXH4 = DLTOXH4
      OxLayr % DLTOXN3 = DLTOXN3
      OxLayr % DAILY   = DAILY

      RETURN
      END SUBROUTINE FertApply
C=======================================================================

C=======================================================================
C  IDLAYR, Function
C
C  Determines layer where fertilizer is placed
C-----------------------------------------------------------------------
C  Revision history
C
C  02/08/93 PWW Written
C  02/08/93 PWW Header revision and minor changes 
C-----------------------------------------------------------------------
C  INPUT  : NLAYR FLAYR FDEPTH
C
C  LOCAL  : L
C
C OUTPUT : IDLAYR
C-----------------------------------------------------------------------
C  Called : FPLACE
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  NLAYR  : Number of layers in soil
C  DLAYR(): Thickness increment of soil layer L - cm
C  L      : Loop counter
C  FDEPTH : Fertilizer depth (cm)
C  DEPTH  : Depth to the bottom of a layer from the surface (cm)
C=======================================================================

      INTEGER FUNCTION IDLAYR (NLAYR,DLAYR,FDEPTH)

      IMPLICIT  NONE

      INTEGER   NLAYR,L
      DIMENSION DLAYR (NLAYR)

      REAL      FDEPTH,DLAYR,DEPTH

      DEPTH  = 0.0
      IDLAYR = 1
      DO L = 1, NLAYR
         DEPTH  = DEPTH + DLAYR (L)
         IF (FDEPTH .LE. DEPTH) THEN
            IDLAYR = L
            GO TO 10
         ENDIF
      END DO

      IDLAYR = NLAYR

   10 CONTINUE
      RETURN
      END !FUNCTION SUBROUTINE IDLAYR

!=======================================================================
! FPLACE and IDLAYR Variables - updated 08/18/2003
!-----------------------------------------------------------------------
! AMTNIT     Cumulative amount of N in fertilizer applications
!             (kg [N] / ha)
! ANFER(I)   Amount of nitrogen in fertilizer applied in Ith application
!             (kg [N] / ha)
! CHAR       Contains the contents of last record read 
! CONTROL    Composite variable containing variables related to control 
!              and/or timing of simulation.  The structure of the variable 
!              (ControlType) is defined in ModuleDefs.for. 
! CUMDEP     Cumulative depth of soil profile (cm)
! DAILY      Logical variable to determine whether daily or hourly flood 
!              chemistry or oxidation layer calculations are done. 
! DAP        Number of days after planting (d)
! DEPTH      Depth to the bottom of a layer from the surface (cm)
! DLAYR(L)   Thickness of soil layer L (cm)
! DLTFNH4    Rate of change of ammonium in floodwater (kg [N] / ha / d)
! DLTFNO3    Rate of change of nitrate in floodwater (kg [N] / ha / d)
! DLTFUREA   Rate of change of urea in floodwater (kg [N] / ha / d)
! DLTOXH4    Rate of change of ammonium in oxidation layer
!             (kg [N] / ha / d)
! DLTOXN3    Rate of change of nitrate in oxidation layer (kg [N] / ha / d)
! DLTOXU     Rate of change of urea in oxidation layer (kg [N] / ha / d)
! DLTSNH4(L) Rate of change of ammonium in soil layer L (kg [N] / ha / d)
! DLTSNO3(L) Rate of change of nitrate in soil layer L (kg [N] / ha / d)
! DLTUREA(L) Rate of change of urea content in soil layer L
!             (kg [N] / ha / d)
! DOY        Current day of simulation (d)
! DSOILN     Fertilizer depth for automatic fertilizer option (cm)
! DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!              INTEGR, OUTPUT, or FINAL 
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FDAY(I)    Julian date for Ith fertilizer application (YYYYDDD)
! FDEPTH     Fertilizer depth (cm)
! FERDEP(I)  Fertilizer depth for application I (cm)
! FERDEPTH   Fertilizer depth on current day of simulation (cm)
! FERMET(I)  Fertilizer method for Ith application 
! FERNIT     Amount of nitrogen in fertilizer applied on current day of 
!              simulation (kg [N] / ha)
! FERTYP(I)  Type of fertilizer used for Ith application 
! FERTYPE    Fertilizer type for current application 
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FLOOD      Current depth of flooding (mm)
! FLOODN     Composite variable which contains flood nitrogen mass and 
!              concentrations. Structure of variable is defined in 
!              ModuleDefs.for. (var.)
! FME        Fertilizer mixing efficiency (fraction)
! FOUND      Indicator that good data was read from file by subroutine FIND 
!              (0 - End-of-file encountered, 1 - NAME was found) 
! FTYPEN     Fertilizer types 
! I          Loop counter 
! IDATE      Day of irrigation or fertilizer application (d)
! IDLAYR     Soil layer where fertilizer is placed 
! IFERI      Fertilizer switch (A= automatic, R= on specified dates 
!              (YYYYDDD format), D = on specified days after planting (DDD) 
!              format. 
! IUOF       Critical Julian day when all urea is assumed to be hydrolyzed 
!              (this is assumed to occur 21 days after the urea application)
!              (d)
! IUON       Flag indicating presence of urea (true or false) 
! K          Loop counter/dummy variable 
! KAPPM      Index for fertilizer placement characteristics; determines 
!              mixing efficiency, placement depth 
! KD         Soil layer in which fertilizer is placed 
! KMAX       Maximum soil depth for fertilizer application (cm)
! LFD10      Date, 10 days after last fertilization.  Used to determine 
!              whether hourly flood chemistry computations will be done 
!              (see DAILY variable). (YYYYDDD)
! LINC       Line number of input file 
! LNUM       Current line number of input file 
! LUNIO      Logical unit number for FILEIO 
! MESSAGE    Text array containing information to be written to WARNING.OUT 
!              file. 
! METFER     Numerical code for fertilizer method 
! MULTI      Current simulation year (=1 for first or single simulation, 
!              =NYRS for last seasonal simulation) 
! NAPNIT     Current number of fertilizer applications applied. 
! NAPPL      Maximum number of applications (for fertilizer, irrigation, 
!              etc.) 
! NFERT      Total number of observed fertilizer applications 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NSTRES     Nitrogen stress factor (1=no stress, 0=max stress) 
! OXLAYR     Composite variable which contains data about oxidation layer.  
!              See ModuleDefs.for for structure. 
! PROF(L)    Proportion of soil destined fertilizer for layer L (fraction)
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! SECTION    Section name in input file 
! SOILNC     Critical threshold of soil nitrogen content to trigger 
!              automatic fertilization.  Measured as a percentage of N 
!              supply to N demand (%)
! SOILNX     Amount of N to be applied for automatic fertilization
!             (kg [N] / ha)
! UNINCO     Logical variable indicating whether fertilizer is fully 
!              incorporated; if true, ignore oxidation layer 
! YEAR       Year of current date of simulation 
! YRDIF      Increment in years which must be added to operations dates for 
!              seasonal or sequenced simulations (yr)
! YRDNIT     Date of last automatic fertilizer application (YYYYDDD)
! YRDOY      Current day of simulation (YYYYDDD)
! YRPLT      Planting date (YYYYDDD)
! YRSIM      Start of simulation date (YYYYDDD)
!======================================================================
