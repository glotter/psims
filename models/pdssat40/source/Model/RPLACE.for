C**********************************************************************
C  RPLACE, Subroutine
C
C  Purpose: Deals with residue placement.
C
C  Revision   History
C  11/21/1995 PWW Written.
C  06/08/1999 CHP Modified for modular format
C  06/09/1999 AJG Completely revised the soil N and SOM module, and made
C               a new SOM module based on the CENTURY model.
C  06/09/1999 AJG Canged variable names: AMTRES to CUMRES,
C                 METRES to RESTYPE;RESAMT to TOPRES, RESAPP to RESSOL &
C                 RESSRF; RESCOD to RESTYP; RINP to RIP; RMET to  RESMET
C  03/17/2000 GH  Incorporated in CROPGRO
C  07/01/2000 GH  Changed variable names RESDEP to RESDEPTH;
C                 DEPRES to RESDEP
C  04/20/2002 GH  Adjusted for crop rotations
C  06/11/2002 GH  Modified for Y2K
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  08/12/2003 CHP Added I/O error checking
C  10/28/2004 CHP Added fix to allow multiple applications in one day.
C
C               Note comments in code about previous crop code, etc.
C               Develop mechanism to handle surface residue   
C-----------------------------------------------------------------------
C  Called : NTRANS
C  Calls  : ERROR, FIND, IPSOIL, YR_DOY
C=======================================================================

      SUBROUTINE RPLACE (CONTROL, ISWITCH, 
     &    DLAYR, HARVRES, NLAYR, SENESCE, YRPLT,           !Input
     &    DLTFON, DLTFPOOL,                               !I/O
     &    CUMRES, CUMRESN, CUMSENN, DMINR, DSNC,          !Output
     &    PRCEL, PRCHO, PRLIG, RCN, RDCEL,                !Output
     &    RDCHO, RDLIG, RESLEFT, RESLEFTN, RESNIT)        !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE

      CHARACTER*1 IRESI, RNMODE
      CHARACTER*5 RESTYPE  
      CHARACTER*5 RESTYP(NAPPL), RESMET(NAPPL)
      CHARACTER*6 ERRKEY, SECTION
      CHARACTER*30 FILEIO
      CHARACTER*90 CHAR
      PARAMETER (ERRKEY = 'RPLACE')

      INTEGER, PARAMETER :: SRFC = 0  !Surface layer
      INTEGER, PARAMETER :: N = 1     !N component of elements array

      INTEGER DAP, DYNAMIC, ERRNUM, FOUND, I, IDATE,
     &  IOUT, L, LINC, LNUM, LUNIO, MULTI, NAPRES,
     &  NLAYR, NRESAP, NRESDL, RUN, TIMDIF, YR,
     &  YRDIF, YRDOY, YRPLT, YRSIM

      INTEGER RESDAY(NAPPL)

      REAL ADD, CUMRES, CUMRESN, CUMSENN
      REAL DEPTH, DMINR, DRESMG, DSNC, FR
      REAL HOLD, ICREN, ICRES, ICRID, ICRIP, PRCEL, PRCHO
      REAL PRLIG, RCN, RDCEL, RDCHO, RDLIG, RESDEPTH
      REAL RESLEFT, RESLEFTN, RESNIT, RESSOL, RESSRF
      REAL RESSRFN, RESSOLN

      REAL DLAYR(NL), DLTFON(NL), DLTFPOOL(NL,3)

      REAL RESDEP(NAPPL), RESIDUE(NAPPL), RESN(NAPPL)
      REAL RESP(NAPPL), RIP(NAPPL)

      !Plant senesced matter(surface-soil, carbon-lignin-nitrogen)
      REAL SenWt(0:NL)        !kg[dry matter]/ha
      REAL SenLig(0:NL)       !kg[lignin]/ha
      REAL SenE(0:NL,NELEM)   !kg[E]/ha (E=N, P, S,...)
      REAL SEN_PRCEL, SEN_PRCHO, SEN_PRLIG

!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      IRESI   = ISWITCH % IRESI

      SenWt  = SENESCE % ResWt
      SenLig = SENESCE % ResLig
      SenE   = SENESCE % ResE
      
!-----------------------------------------------------------------------
!     Residue types                                                    
!     1 = Crop residue                                                 
!     2 = Green manure                                                 
!     3 = Barnyard manure                                              
!     4 = Liquid manure                                                
!                                                                     
!     Residue Application (IRESI)                                             
!     A = Auto residue for multiyear                                   
!     N = No residue                                                   
!     R = On reported dates                                            
!     D = As reported, in DAP                                          
!     F = Auto, with fixed amounts 

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      RUN     = CONTROL % RUN
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      YRDIF   = CONTROL % YRDIF
      YRSIM   = CONTROL % YRSIM
!     ------------------------------------------------------------------
!     Read RPLACE data from FILEIO
!     ------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
      LNUM =0

!     ------------------------------------------------------------------
!     Find AUTOMATIC MANAGEMENT Section
!     ------------------------------------------------------------------
      SECTION = '!AUTOM'
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!     If the residue section can't be found, call an error, or else
!     read the input data.
      IF (FOUND .EQ. 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
!       Read the number of days after planting that residues are
!       applied (with automatic application) and the residue
!       application depth.
        READ (LUNIO,'(///,20X,I6,F6.0)', IOSTAT = ERRNUM) NRESDL, DRESMG
        LNUM = LNUM + 4
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
      ENDIF

!     ----------------------------------------------------------------
!     Find INITIAL CONDITIONS Section   ***Added from INRES***
!     ----------------------------------------------------------------
      SECTION = '*INITI'
!     Find the line number from where to start reading.
      CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
!     If the residue section can't be found, call an error, or else
!     read the input data.
      IF (FOUND .EQ. 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
!       Read the initial shoot residue. The root residues are dealt
!       with in subroutine SOILNI.
        READ (LUNIO, 10, IOSTAT = ERRNUM) ICRES, ICREN, ICRIP, ICRID
10      FORMAT (46X, 2F6.0, 6X, 2F6.0)
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
      ENDIF
      IF (ICRID .LT. 0.01) ICRIP = 0.
!     ------------------------------------------------------------------
!     Find RESIDUE Section
!     ------------------------------------------------------------------
!     Only read the residue section if residues are going to be
!     applied.
      IF (IRESI .NE. 'N') THEN
        SECTION = '*RESID'

!       Find the line number from where to start reading.
        CALL FIND (LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC

!       If the residue section can't be found, call an error, or else
!       read the input data.
        IF (FOUND .EQ. 0) THEN
          CALL ERROR (SECTION, 42, FILEIO, LNUM)
        ELSE
!         Initialize the number of residue applications to be done.
          NRESAP = 0

          DO I = 1, NAPPL
!           Read the residue application parameters.
!           Note: RESP is not currently used in CROPGRO.  

!            READ (LUNIO, 86, IOSTAT = ERRNUM, ERR = 87, END=87) 
!     &        RESDAY(I), RESTYP(I), RESIDUE(I), RESN(I), RESP(I), 
!     &        RIP(I), RESDEP(I), RESMET(I)
!86          FORMAT (3X, I7, 1X, A5, 3F6.0, 6X, 2F6.0, 1X, A5)
            READ (LUNIO,'(3X,I7,1X,A90)',ERR=87,END=87) RESDAY(I), CHAR
            LNUM = LNUM + 1

            READ (CHAR, '(A5,3F6.0,6X,2F6.0,1X,A5)', IOSTAT = ERRNUM) 
     &        RESTYP(I), RESIDUE(I), RESN(I), RESP(I), 
     &        RIP(I), RESDEP(I), RESMET(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)

            IF (RIP(I) .LT. 0.001) THEN
              RIP(I) = 0.0
            ENDIF

!           If the residue code does not start with 'RE'.
            IF (RESTYP(I)(1:2) .NE. 'RE') RESTYP(I)(1:2) = 'RE'
            NRESAP = NRESAP + 1
          ENDDO

!         Continue here after jumping out of the DO loop with an error
!         (thus the end of the residue section was reached).
87        CONTINUE
        ENDIF   !End of IF block on FOUND=0.
      ENDIF   !End of IF block on IRESI.

!     Close the input file.
      CLOSE (LUNIO)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!     Initialize the number of residue applications done.
      NAPRES  = 0              !from INPLNT

!     Initialize the cumulative residue applications to zero. Don't
!     sum up the residues present at initialization in CUMRES,
!     because they are not considered "Residue Application" but
!     "Initial Conditions".
      CUMRES      = 0.
      CUMRESN = 0.
      !HARVRESN = 0.
      CUMSENN  = 0.

!     ------------------------------------------------------------------
!     Not a sequenced run.  
!     Initialization for a single-season run, a multiple single-season
!     run (= seasonal), or the first year of a sequential run.
!     ------------------------------------------------------------------
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN

!       Initialize the surface-deposited residues to zero.
        RESLEFT  = 0.
        RESLEFTN = 0.

!       Distribute the initial shoot residue over soil and surface.
        RESSOL = ICRES * ICRIP / 100.
        RESSRF = ICRES - RESSOL

!       Set the initial shoot residue N concentration.
!       Save as kg [N] / ha. - CHP 8/13/02
        !RESNIT = ICREN    !%
        RESSRFN = ICREN / 100. * RESSRF   !kg[N]/ha
        RESSOLN = ICREN / 100. * RESSOL

!       Set the initial shoot residue incorporation depth.
        RESDEPTH = ICRID

!       Set the initial shoot residue type to a standard value, so
!       that the residue parameters are set. 
!       TEMPORARY: this should be set via fileX.
!       METRES = 'RE001'   !name change
        RESTYPE = 'RE001'

!       Save harvest residue for use in N balance
        HARVRES % RESWT(0)  = RESSRF
        HARVRES % RESE(0,1) = RESSRFN 

      ELSE
C-----------------------------------------------------------------------
C     Adjust for crop rotations
C-----------------------------------------------------------------------
!       Add the shoot residue from the previous season to the soil.
!       Carry-over root residues (TRTRES) have already been dealt with
!       in the INIT section of SOILNI.
!       NB: This should be split over soil and surface.
C-----------------------------------------------------------------------
        RESSRF = HARVRES % RESWT(SRFC)
C-----------------------------------------------------------------------
!      N in harvest residue from previous crop.
C-----------------------------------------------------------------------
        IF (RESSRF .GT. 0.0) THEN
          RESSRFN = HARVRES % RESE(SRFC,N)
        ELSE
          RESSRFN = 0.0
        ENDIF
C-----------------------------------------------------------------------
!       Set the residue incorporation depth.
!       NB: This should be made flexible.
C-----------------------------------------------------------------------
        RESDEPTH = 20.0
C-----------------------------------------------------------------------
!       Set the residue type, so that the residue parameters are set. 
!       NB: This should be made dependent on the previous crop.
C-----------------------------------------------------------------------
        RESTYPE = 'RE001'

C-----------------------------------------------------------------------
!       Increment day of year and adjust all date values for sequenced
!       runs (Moved from DATECS). Do this only if the number of
!       residue applications to be done is > 0, and the residue
!       application method is 'As reported', and the residue-application
!       day is before the start day of the new season.
C-----------------------------------------------------------------------
        IF (NRESAP .GT. 0 .AND. RESDAY(1) .LT. YRSIM .AND. 
     &      IRESI .NE. 'D') THEN
          DO I = 1, NRESAP
            CALL YR_DOY(RESDAY(I),YR,IDATE)
            RESDAY(I) = (YR + YRDIF) * 1000 + IDATE
          ENDDO
        ENDIF
      ENDIF   

C-----------------------------------------------------------------------
      IF (MULTI .GT. 1 .AND. NRESAP .GT. 0 .AND. IRESI .NE. 'D') THEN
C-----------------------------------------------------------------------        
!       Each season of a seasonal run, the YR number is increased by
!       one. The residue application dates thus have to be updated.
C-----------------------------------------------------------------------
        DO I = 1, NRESAP
          CALL YR_DOY (RESDAY(I), YR, IDATE)
          RESDAY(I) = (YR + MULTI - 1) * 1000 + IDATE
        ENDDO
      ENDIF

!     Call IPSOIL and get the indicated parameters for this
!     crop residue, including RDECR(1,3) and the pool sizes
!     for CHO, cellulose, and lignin
      CALL IPSOIL (CONTROL,'RE001',                   !Input
     &  DMINR, DSNC, PRCEL, PRCHO, PRLIG,             !Output
     &  RCN, RDCEL, RDCHO, RDLIG)                     !Output

      SEN_PRCEL = PRCEL
      SEN_PRCHO = PRCHO
      SEN_PRLIG = PRLIG

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE .OR. DYNAMIC .EQ. FINAL) THEN
!     ------------------------------------------------------------------
!     Set back to zero every time the rate section is entered.
      RESSRF = 0.0
      RESSOL  = 0.0
      RESNIT  = 0.0
      RESSRFN = 0.0
      RESSOLN = 0.0

      IF (SenWt(SRFC) .GT. 0.0) THEN
        !senesced plant matter on surface kg/ha
        !convert from C to biomass units with 0.40
        RESSRF = SenWt(SRFC)  !kg[dry weight]/ha
        !N in senesced plant matter on surface
        RESSRFN = SenE(SRFC,N) !kg[N]/ha
        !senesced plant matter in soil
        CUMSENN = CUMSENN + RESSRFN
      ENDIF

!     Add senesced material to soil layers
      DO L = 1, NLAYR
        DLTFON(L) = DLTFON(L) + SenE(L,N)
        ADD = SenWt(L)              !kg[dry matter]/ha
        DLTFPOOL(L,1) = DLTFPOOL(L,1) + ADD * SEN_PRCHO 
        DLTFPOOL(L,2) = DLTFPOOL(L,2) + ADD * SEN_PRCEL 
        DLTFPOOL(L,3) = DLTFPOOL(L,3) + ADD * SEN_PRLIG 
        CUMSENN = CUMSENN + SenE(L,N)
      ENDDO
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
      IF (NRESAP .GT. 0) THEN
        DAP = MAX(0, TIMDIF(YRPLT,YRDOY))
        DO I = 1, NRESAP

              !Residue application on a specified date.
          IF ((IRESI .EQ. 'R' .AND. YRDOY .EQ. RESDAY(I)) .OR.
              !Residue application on a specified day after planting.
     &        (IRESI .EQ. 'D' .AND. RESDAY(I) .NE. 0 .AND. 
     &                                          DAP .EQ. RESDAY(I)) .OR. 
     &        (IRESI .EQ. 'D' .AND. RESDAY(I) .EQ. 0 .AND. 
     &                                            YRDOY .EQ. YRPLT) .OR. 
              !Automatic multiyear residue application.
     &        (IRESI .EQ. 'A' .AND. DAP .EQ. NRESDL)) THEN

            !Apply residue today
            !-------------------
!           Increase number of residue applications done.
            NAPRES  = NAPRES + 1

!           If the residue depth is zero, the incorporation percentage
!           should also be zero.
            SELECT CASE(IRESI)
            CASE ('R', 'D')
              IF (RESDEP(I) .LT. 0.001) RIP(I) = 0.
            CASE ('A')
              IF (DRESMG .LT. 0.001) RIP(I) = 0.
            END SELECT

!           Divide the residue over soil and surface.
            RESSOL  = RESSOL + RESIDUE(I) * RIP(I) / 100.
            RESSRF  = RESSRF + RESIDUE(I) * (1.0 - RIP(I) / 100.)

!           Set the residue N concentration.
!            RESNIT  = RESNIT + RESN(I)
!           Additional N added: 
            RESNIT  = RESN(I) / 100. * RESIDUE(I)    !kg[N]/ha
            RESSOLN = RESSOLN + RESNIT * RIP(I) / 100.
            RESSRFN = RESSRFN + RESNIT * (1.0 - RIP(I) / 100.)

!           Set the residue incorporation depth.
            RESDEPTH  = RESDEP(I)

!           Set the type of residue.
            RESTYPE = RESTYP(I)

!           Sum the residue to the total amount added.
            CUMRES  = CUMRES + RESIDUE(I) 
            CUMRESN = CUMRESN + RESNIT        !kg[N]/ha

!           !Jump out of loop to apply residue.
            !EXIT  

!         If it is not yet a residue application day, jump out of loop 
          ELSEIF ((IRESI .EQ. 'R' .AND. RESDAY(I) .GT. YRDOY) .OR.
     &            (IRESI .EQ. 'D' .AND. RESDAY(I) .GT. DAP)) THEN
            EXIT 
          ENDIF   !End of IF block on YRDOY and RESDAY
        END DO   !End of loop on NRESAP.
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!   PERFORM ADDITIONAL CALCULATIONS FOR INITIALIZATION AND RATE SECTIONS
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. RATE .OR. 
     &        DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
!     ----------------------------------------------------------------
!     C and N contribution of the applied residue.
!     ----------------------------------------------------------------
!     Sum up all the residues that were not incorporated 
      RESLEFT  = RESLEFT + RESSRF
      RESLEFTN = RESLEFTN + RESSRFN    !kg[N]/ha

!     Soil-deposited residues
      IF (RESSOL .GT. 0.001 .AND. RESDEPTH .GT. 0.001) THEN 

!       Call IPSOIL and get the indicated parameters for this
!       residue type, including RDECR(1,3) and the pool sizes
!       for CHO, cellulose, and lignin
        CALL IPSOIL (CONTROL, RESTYPE,                    !Input
     &    DMINR, DSNC, PRCEL, PRCHO, PRLIG,               !Output
     &    RCN, RDCEL, RDCHO, RDLIG)                       !Output

!       Set the starting depth for counting the soil layers to zero.
        DEPTH = 0.0

!       Initialize a flag that determines when to jump out of the DO
!       loop.
        IOUT  = 1

        DO L = 1, NLAYR
!         Set the depth of the upper limit of the layer.
          HOLD  = DEPTH

!         Calculate the depth of the bottom of the layer.
          DEPTH = DEPTH + DLAYR(L)

!         If the residue application depth is less than the depth of
!         the bottom of the layer.
          IF (RESDEPTH .LE. DEPTH) THEN
!           Calculate the fraction of the residue that is to be
!           added to this layer
              FR = (RESDEPTH - HOLD) / RESDEPTH

!           Set a flag to jump out of the DO loop.
            IOUT = 2
          ELSE
!           If the residue application depth is greater than the
!           depth of bottom of the layer, calculate the fraction of
!           the residue that is to be added to this layer.
              FR = DLAYR(L) / RESDEPTH
          ENDIF   !End of IF block on RESDEPTH =< DEPTH.

!         Add the residue to the carbohydrate, cellulose and lignin
!         pools of the layer.
          ADD = RESSOL * FR

!         Add the residue to the DLTxxx rate variables.
          DLTFON(L) = DLTFON(L) + RESSOLN * FR  !kg[N]/ha
          DLTFPOOL(L,1) = DLTFPOOL(L,1) + ADD * PRCHO 
          DLTFPOOL(L,2) = DLTFPOOL(L,2) + ADD * PRCEL 
          DLTFPOOL(L,3) = DLTFPOOL(L,3) + ADD * PRLIG 

!         If there are no more soil layers over which to distribute
!         the residue, jump out of the DO loop. 
          IF (IOUT .EQ. 2) THEN
            EXIT
          ENDIF
        ENDDO   !End of loop on soil layers.
      ENDIF   !End of IF block on RESSOL and RESDEPTH.

!***********************************************************************
!***********************************************************************
!     END OF SECOND DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

  300 RETURN
      END !SUBROUTINE RPLACE

!=======================================================================
! RPLACE Variables - updated 08/20/2003
!-----------------------------------------------------------------------
! ADD        Amount of residue carbon to be added to FOM in the soil 
!              layer under consideration (kg [dry matter] / ha)
! CHAR       Contains the contents of last record read 
! CONTROL    Composite variable containing variables related to control 
!              and/or timing of simulation.  The structure of the 
!              variable (ControlType) is defined in ModuleDefs.for. 
! CUMRES     Cumulative amount of residue application (kg [res] / ha)
! CUMRESN    Cumulative amount of N in residue application (kg [N] / ha)
! CUMSENN    Cumulative N in senesced plant matter added to soil and 
!              surface (kg [N] / ha)
! DAP        Number of days after planting (d)
! DEPTH      Depth to the bottom of a layer from the surface (cm)
! DLAYR(L)   Thickness of soil layer L (cm)
! DLTFON(L)  Rate of change of N in fresh organic residue  in soil layer 
!              L (kg [N] / ha / d)
! DLTFPOOL   Rate of change of FOM pool (FPOOL) in soil layer L 
!   (L,J)           (J=1=carbohydrate, 2=cellulose, 3=lignin)
!              (kg [residue pool] / ha / d)
! DMINR      Maximum decomposition rate constant of stable organic 
!              matter (d-1)
! DRESMG     Residue application depth (with the option of automatic 
!              residue application) (cm)
! DSNC       Depth to which C and N are integrated across all soil 
!              layers for output in CARBON.OUT (cm)
! DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!              INTEGR, OUTPUT, or FINAL 
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FOUND      Indicator that good data was read from file by subroutine 
!              FIND (0 - End-of-file encountered, 1 - NAME was found) 
! FR         Residue fraction to be added to a soil layer (fraction)
! HARVRES    Composite variable containing harvest residue amounts for 
!              total dry matter, lignin, and N amounts.  Structure of 
!              variable is defined in ModuleDefs.for. 
! HOLD       Amount of water a soil layer will hold above it's present 
!              level, used to calculate downward flow; also, temporary 
!              variable/ intermediate calculation (cm)
! ICREN      Initial conditions residue N concentration (%)
! ICRES      Initial conditions residue application
!             (kg [dry matter] / ha)
! ICRID      Initial conditions residue incorporation depth (cm)
! ICRIP      Initial conditions residue incorporation percentage (%)
! IDATE      Day of irrigation or fertilizer application (d)
! IOUT       Flag parameter that determines when to jump out of a DO 
!              loop (1 or 2) 
! IRESI      Residue application method. A=Automatic residue application 
!              at a certain days after planting for multiple years; N=No 
!              residue; R=On reported dates; D=As reported, in DAP; 
!              F=Auto, with fixed amounts 
! ISWITCH    Composite variable containing switches which control flow 
!              of execution for model.  The structure of the variable 
!              (SwitchType) is defined in ModuleDefs.for. 
! LINC       Line number of input file 
! LNUM       Current line number of input file 
! LUNIO      Logical unit number for FILEIO 
! MULTI      Current simulation year (=1 for first or single simulation, 
!              =NYRS for last seasonal simulation) 
! N          Constant = 1, representing nitrogen component of array 
! NAPPL      Maximum number of applications (for fertilizer, irrigation, 
!              etc.) 
! NAPRES     Number of residue applications which have been applied 
! NELEM      Number of elements simulated in soil processes model 
!              (currently = 1) 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NRESAP     Number of observed residue applications 
! NRESDL     Number of days after planting that residues are applied 
!              (with the option of automatic residue application) (d)
! PRCEL      Cellulose fraction of the residue (fraction)
! PRCHO      Carbohydrate fraction of the residue (fraction)
! PRLIG      Lignin fraction of the residue (fraction)
! RCN        C/N ratio of initial root residue (kg [C] / kg [N])
! RDCEL      Maximum decomposition rate of cellulose (fraction / day)
! RDCHO      Maximum decomposition rate of carbohydrates
!             (fraction / day)
! RDLIG      Maximum decomposition rate of lignin (fraction / day)
! RESDAY(I)  Date of Ith residue application (YYYYDDD)
! RESDEP(I)  Incorporation depth of newly added residues for application 
!              I (cm)
! RESDEPTH   Incorporation depth of newly added residues (cm)
! RESIDUE(I) Amount of residue applied for Ith application
!             (kg [dry matter] / ha)
! RESLEFT    Residue material which is left on top of soil and not 
!              incorporated (kg[residue]/ha)
! RESLEFTN   N in residue which is not incorporated (kg[N]/ha)
! RESMET(I)  Residue incorporation method for Ith application (Not used)
!             
! RESN(I)    N concentration of the residue for Ith application (%)
! RESNIT     N in current residue application (kg[N]/ha)
! RESP(I)    P concentration of the residue for Ith application (%)
! RESSOL     Amount of residue applied to the soil
!             (kg [dry matter] / ha)
! RESSOLN    N in portion of residue incorporated into soil (kg[N]/ha)
! RESSRF     Residue left on surface of soil (kg [residue] / ha)
! RESSRFN    N in portion of residue left on surface (unincorporated)
!             (kg[N]/ha)
! RESTYP(I)  FileX code for residue type for Ith residue application; 
!              not used yet) (1=crop residue, 2=green manure, 3=barnyard 
!              manure, 4=liquid manure) 
! RESTYPE    Residue type for current application 
! RIP(I)     Residue incorporation percentage for application I (%)
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, 
!             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
! RUN        Change in date between two observations for linear 
!              interpolation 
! SECTION    Section name in input file 
! SENE(L,E)  Element component of senesced material, soil layer L, 
!              element E (1=N, 2=P, 3=K) (kg[E]/ha)
! SENESCE    Composite variable containing data about daily senesced 
!              plant matter. Structure of variable is defined in 
!              ModuleDefs.for 
! SENLIG(L)  Lignin component of senesced material, soil layer L
!             (kg[lignin]/ha)
! SENWT      Leaf senescence due to N mobilization
!             (g[leaf] / m2[ground])
! SEN_PRCEL  Cellulose fraction in senesced material (fraction)
! SEN_PRCHO  Carbohydrate fraction in senesced material (fraction)
! SEN_PRLIG  Lignin fraction in senesced material (fraction)
! SRFC       Constant = 0, representing surface layer in array 
! YR         Year portion of date 
! YRDIF      Increment in years which must be added to operations dates 
!              for seasonal or sequenced simulations (yr)
! YRDOY      Current day of simulation (YYYYDDD)
! YRPLT      Planting date (YYYYDDD)
! YRSIM      Start of simulation date (YYYYDDD)
!=======================================================================
