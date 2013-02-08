!***********************************************************************
!  RPLACE_C, Subroutine for CENTURY-based SOM/residue module.
!
!  Purpose: Deals with residue placement.
!
!  Revision       History
!  11/21/1995 PWW Written.
!  06/08/1999 CHP Modified for modular format
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!                 a new SOM module based on the CENTURY model.
!                 Also changed variable names:
!                 OLD      NEW                OLD      NEW
!                 ------   ------             ------   ------ 
!                 AMTRES   CUMRES             RESCOD   RESTYP
!                 DEPRES   RESDEP             RESDEP   RESDEPTH 
!                 METRES   RESTYPE            RESNIT   RESECONC
!                 RESAMT   TOPRES             RINP     RIP
!                 RESAPP   RESSOL, RESSRF     RMET     RESMET
!  06/21/1999 CHP Modular format.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!                 modules with CHP's modular format.
!  04/20/2002 GH  Adjusted for crop rotations
!  06/11/2002 GH  Modified for Y2K
!  11/11/2002 AJG Added DLAYR to the PARTIT_C parameter string.
!  08/12/2003 CHP Added I/O error checking
!  11/14/2003 CHP Added call to WARNING for high residue N concentrataions.
!  07/01/2005 CHP Fixed a problem with initialization of harvest residues for 
!                 sequenced runs.
!
!  Called : NTRANS_C
!  Calls  : ERROR, FIND, IPSOIL, PARTIT_C, YR_DOY
!***********************************************************************

      SUBROUTINE RPLACE_C (CONTROL, ISWITCH, 
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRMETI,     !Input
     &    FRMETS, HARVRES, NLAYR, RESDAX,                 !Input
     &    YRDIF, YRPLT,                                   !Input

     &    CUMRES, CUMRESE, DISTURBDEPTH, DISTURBEND,      !Input/Output
     &    DISTURBNUM, DLTLIGC, DLTMETABC, DLTMETABE,      !Input/Output
     &    DLTSNH4, DLTSNO3, DLTSTRUCC, DLTSTRUCE,         !Input/Output

     &    ADDMETABEFLAG, DOINCORPOR, FRMETFLAG,           !Output
     &    RESDEPTH, RESMIXPERC, RESECONC)                !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE
!-----------------------------------------------------------------------

      LOGICAL ADDMETABEFLAG, DOINCORPOR, FRMETFLAG

      CHARACTER*1 IRESI, RNMODE
      CHARACTER*5 RESTYPE  
      CHARACTER*5 RESTYP(NAPPL), RESMET(NAPPL)
      CHARACTER*6 ERRKEY, SECTION
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(10)
      CHARACTER*90 CHAR
      PARAMETER (ERRKEY = 'RPLACE')

      INTEGER DAP, DISTURBNUM, DYNAMIC, ERRNUM,
     &  FOUND, I, IDATE, INCYD, IOUT, L, LAYER,
     &  LINC, LNUM, LUNIO, MULTI, N, NAPRES, NLAYR,
     &  NRESAP, NRESDL, RUN, SRFC,
     &  TIMDIF, YR, YRDIF, YRDOY, YRPLT, YRSIM
      PARAMETER (N = 1)
      PARAMETER (SRFC = 0)

      INTEGER DISTURBEND(NAPPL*3), RESDAY(NAPPL)

      REAL CUMRES, DEPTH, DRESMG, FR, FRMETI,
     &  FRMETS, HOLD, ICREN, ICRES, ICRID, ICRIP, PRLIG,
     &  RDUMMY1, RDUMMY2, RDUMMY3, RDUMMY4, RDUMMY5,
     &  RDUMMY6, RDUMMY7, RDUMMY8, RESDAX, RESDEPTH,
     &  RESSOL, RESSRF, RESMIXPERC

      REAL CEDAM(3), CESTR(3), CUMRESE(3), DISTURBDEPTH(NAPPL*3),
     &  DLAYR(NL), DLTLIGC(0:NL), DLTMETABC(0:NL),
     &  DLTSNH4(NL), DLTSNO3(NL), DLTSTRUCC(0:NL),
     &  FRDAE(3), FRLRES(0:NL), RESC(0:NL),
     &  RESDEP(NAPPL), RESECONC(3), RESIDUE(NAPPL), RESN(NAPPL),
     &  RESP(NAPPL), RESSOLE(3), RESSRFE(3), RIP(NAPPL),
     &  SNH4(NL)

      REAL AMINRL(NL,3), DLTMETABE(0:NL,3), DLTSTRUCE(0:NL,3),
     &  RESCE(0:NL,3)

!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES

      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      IRESI   = ISWITCH % IRESI

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


!     Set back to zero every time the RPLACE_C is entered.
      RESSOL = 0.
      RESSRF = 0.
      RESECONC = 0.
      DO L = 0, NLAYR
        RESC(L) = 0.
      END DO

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
      YRSIM   = CONTROL % YRSIM
!     ------------------------------------------------------------------
!     Read RPLACE data from FILEIO
!     ------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)

!     If the file can't be found, call an error.
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)

!     ------------------------------------------------------------------
!     Find AUTOMATIC MANAGEMENT Section
!     ------------------------------------------------------------------
      SECTION = '!AUTOM'

!     Find the line number from where to start reading.
      CALL FIND (LUNIO, SECTION, LNUM, FOUND)

!     If the residue section can't be found, call an error, or else
!     read the input data.
      IF (FOUND .EQ. 0) THEN
        CALL ERROR (SECTION, 42, FILEIO, LNUM)
      ELSE
!       Read the number of days after planting that residues are
!       applied (with automatic application) and the residue
!       application depth.
        READ (LUNIO, '(///, 20X, I6, F6.0)',IOSTAT=ERRNUM) NRESDL,DRESMG
        LNUM = LNUM + 4
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
      ENDIF

!     ----------------------------------------------------------------
!     Find INITIAL CONDITIONS Section
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
!       with in subroutine SOILNI_C.
        READ (LUNIO, 10,IOSTAT=ERRNUM) ICRES, ICREN, ICRIP, ICRID
10      FORMAT (46X, 2F6.0, 6X, 2F6.0)
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
      ENDIF

!     If the residue depth is zero, the incorporation percentage
!     should also be zero.
      IF (ICRID .LT. 0.001) ICRIP = 0.

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

!           If the residue code does not start with 'RE'.
            IF (RESTYP(I)(1:2) .NE. 'RE') RESTYP(I)(1:2) = 'RE'
            NRESAP = NRESAP + 1
          ENDDO

!         Continue here after jumping out of the DO loop with an error
!         (thus the end of the residue section was reached).
87        CONTINUE
        ENDIF   !End of IF block on FOUND .EQ. 0.
      ENDIF   !End of IF block on IRESI .NE. 'N'.

!     Close the input file.
      CLOSE (LUNIO)

!     Initialize the number of residue applications done.
      NAPRES  = 0

!     Not a sequenced run.  
!     Initialization for a single-season run, a multiple single-season
!     run (= seasonal), or the first year of a sequential run.
!     ------------------------------------------------------------------
      IF (RUN == 1 .OR. INDEX ('QF', RNMODE) <= 0) THEN

!       Distribute the initial shoot residue over soil and surface.
        RESSOL = ICRES * ICRIP / 100.
        RESSRF = ICRES - RESSOL

!       Set the initial shoot residue N concentration as a fraction (not %).
        RESECONC(N) = ICREN / 100.
        RESSRFE(N) = RESECONC(N) * RESSRF

!       Check for residues with an high or low N concentrations.
        IF ((RESSOL + RESSRF) .GT. 0.0) THEN
          IF (RESECONC(N) .GT. 0.04) THEN
            WRITE (MSG(1), 400) YRDOY, RESECONC(N)*100.
            CALL WARNING (1, ERRKEY, MSG)
          ELSEIF (RESECONC(N) .LT. 0.00001) THEN
            WRITE (MSG(1), 401) YRDOY, RESECONC(N)*100.
            CALL WARNING (1, ERRKEY, MSG)
          ENDIF
        ENDIF
  400 FORMAT('The residue N concentration on day ', I7,
     &           ' is quite high: ', F8.2, '%.') 
  401 FORMAT('The residue N concentration on day ', I7,
     &           ' is improbably low: ', F8.2, '%.') 

!       FRLRES(SRFC) = PRLIG

!       Set the initial shoot residue incorporation depth.
        RESDEPTH = ICRID

!       Set variables for the call to subroutine INCORPOR.
        RESMIXPERC = ICRIP

!       Set the initial shoot residue type to a standard value, so
!       that the residue parameters are set. 
!       TEMPORARY: this should be set via fileX.
        RESTYPE = 'RE001'

      ELSE
!-----------------------------------------------------------------------
!     Adjust for crop rotations
!-----------------------------------------------------------------------
!       Add the shoot residue from the previous season to the soil.
!       Carry-over root residues (TRTRES) have already been dealt with
!       in the INIT section of SOILNI_C.
!       NB: This should be split over soil and surface.
!-----------------------------------------------------------------------
        RESSRF = HARVRES % RESWT(0)
        RESSRFE(N) = HARVRES % RESE(0,N)
        IF (RESSRF > 1.E-5) THEN
          RESECONC(N) = RESSRFE(N) / RESSRF
        ELSE
          RESECONC(N) = 0.
        ENDIF

!-----------------------------------------------------------------------
!      N and lignin in harvest residue from previous crop.
!-----------------------------------------------------------------------
        IF (RESSRF .GT. 0.) THEN
          RESCE(SRFC,N) = RESSRF * 0.4 / RESSRFE(N)
          FRLRES(SRFC)  = HARVRES % ResLig(0) / RESSRF  
        ELSE
          RESCE(SRFC,N) = 0.
          FRLRES(SRFC)  = 0.
        ENDIF

!-----------------------------------------------------------------------
!       Set the residue type, so that the residue parameters are set. 
!       NB: This should be made dependent on the previous crop.
!-----------------------------------------------------------------------
        RESTYPE = 'RE001'

!-----------------------------------------------------------------------
!       Increment day of year and adjust all date values for sequenced
!       runs (Moved from DATECS). Do this only if the number of
!       residue applications to be done is > 0, and the residue
!       application method is 'As reported', and the residue-application
!       day is before the start day of the new season.
!-----------------------------------------------------------------------
        IF (NRESAP .GT. 0 .AND. RESDAY(1) .LT. YRSIM .AND. 
     &      IRESI .NE. 'D') THEN
          DO I = 1, NRESAP
            CALL YR_DOY (RESDAY(I),YR,IDATE)
            RESDAY(I) = (YR + YRDIF) * 1000 + IDATE
          ENDDO
        ENDIF   !End of IF block on NRESAP, RESDAY and IRESI.
      ENDIF   !End of IF block on RNMODE

!-----------------------------------------------------------------------
!     Adjust for seasonal runs
!-----------------------------------------------------------------------
!     Seasonal run - MULTI > 1
!     The data input section is only called once, though for a
!     seasonal run some variables/parameters have to be set back to
!     their initial value each season. Do that here.
!     ------------------------------------------------------------------
!     For a seasonal run, open the input file, if the number of
!     residue applications to be done is > 0, and the residue
!     application method is 'As reported in DAP'.
!-----------------------------------------------------------------------
      IF (MULTI .GT. 1 .AND. NRESAP .GT. 0 .AND. IRESI .NE. 'D') THEN
        DO I = 1, NRESAP
          CALL YR_DOY (RESDAY(I), YR, IDATE)
          RESDAY(I) = (YR + MULTI - 1) * 1000 + IDATE
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
!     *** Note that SEASINIT continues after RATE ***
!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
      DAP = MAX0 (0, TIMDIF (YRPLT, YRDOY))

!     ----------------------------------------------------------------
!     Residue application on a specified date.
!     ----------------------------------------------------------------
      IF (NRESAP .GT. 0) THEN
        DO I = 1, NRESAP

!         Residue application on a specified date.
          IF ((IRESI .EQ. 'R' .AND. YRDOY .EQ. RESDAY(I)) .OR.
              !Residue application on a specified day after planting.
     &        (IRESI .EQ. 'D' .AND. DAP   .EQ. RESDAY(I)) .OR. 
              !Automatic multiyear residue application.
     &        (IRESI .EQ. 'A' .AND. DAP   .EQ. NRESDL)) THEN

!           Apply residue today
!           -------------------
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
            RESSOL  = RESIDUE(I) * RIP(I) / 100.
            RESSRF  = RESIDUE(I) * (1.0 - RIP(I) / 100.)

!           Set the residue N concentration as a fraction (not %).
            RESECONC(N) = RESN(I) / 100.

!           Residues with an unlikely high N concentration.
            IF ((RESSOL + RESSRF) .GT. 0.0) THEN
              IF (RESECONC(N) .GT. 0.04) THEN
                WRITE (MSG(1), 400) YRDOY, RESECONC(N)*100.
                CALL WARNING (1, ERRKEY, MSG)
!             Or too low an N concentration.
              ELSEIF (RESECONC(N) .LT. 0.00001) THEN
                WRITE (MSG(1), 401) YRDOY, RESECONC(N)*100.
                CALL WARNING (1, ERRKEY, MSG)
              ENDIF
            ENDIF

            RESSOLE(N) = RESIDUE(I) * RESECONC(N) * RIP(I) / 100.
            RESSRFE(N) = RESIDUE(I) * RESECONC(N) * (1.0 - RIP(I)/100.)
            IF (RESECONC(N) > 1.E-4) THEN
              RESCE(SRFC,N) = 0.40 / RESECONC(N)
            ELSE
              RESCE(SRFC,N) = 10.
            ENDIF

!           Set the residue incorporation depth.
            RESDEPTH  = RESDEP(I)

!           Set variables for the call to subroutine INCORPOR.
            RESMIXPERC = RIP(I)

!           Set the type of residue.
            RESTYPE = RESTYP(I)

!           Sum the residue to the total amount added.
            CUMRES  = CUMRES + RESIDUE(I) 
            CUMRESE(N) = CUMRESE(N) + RESIDUE(I) * RESECONC(N)

!           Jump out of loop to apply residue.
            EXIT  

!         If it is not yet a residue application day, jump out of loop to
!         apply the residue.
          ELSEIF ((IRESI .EQ. 'R' .AND. RESDAY(I) .GT. YRDOY) .OR.
     &            (IRESI .EQ. 'D' .AND. RESDAY(I) .GT. DAP)) THEN
            EXIT 
          ENDIF   !End of IF block on YRDOY and RESDAY
        END DO   !End of loop on NRESAP.
      ENDIF
!***********************************************************************
!***********************************************************************
!     END OF FIRST DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

!***********************************************************************
!***********************************************************************
!   PERFORM ADDITIONAL CALCULATIONS FOR INITIALIZATION AND RATE SECTIONS
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. RATE) THEN
!     ----------------------------------------------------------------
!     C and N contribution of the applied residue.
!     ----------------------------------------------------------------
!     -----------------------
!     Surface-applied residue
!     -----------------------
      IF (RESSRF > 1.E-5) THEN
        RESC(SRFC) = RESSRF * 0.40

!       Calculate the C and N contribution of the residue that is newly
!       added to the surface layer. Set the residue's C content 
!       (= 40% of the residue amount), C/E ratio and lignin conc.
        IF (RESSRFE(N) .GT. 0) THEN
          IF (RESECONC(N) .GT. 0.) THEN
            RESCE(SRFC,N) = 0.40 / RESECONC(N)
          ELSE
            RESCE(SRFC,N) = 40.0
          ENDIF
        ENDIF

!       Call IPSOIL to get the lignin concentration.
!       The residue is given a default value of 10% lignin, which can be
!       overwritten in IPSOIL (if the residue type was specified).
        PRLIG = 0.10
        CALL IPSOIL (
     &    CONTROL, RESTYPE,                               !Input
     &    RDUMMY1, RDUMMY2, RDUMMY3, RDUMMY4, PRLIG,      !Output
     &    RDUMMY5, RDUMMY6, RDUMMY7, RDUMMY8)             !Output

!       For carry-over residue the lignin concentration has been set
!       already in SEASINIT.
        FRLRES(SRFC) = PRLIG

!       If L is transferred from NTRANS via the subroutine's parameter
!       string, it goes wrong, because L is the DO loop counter.
!       Therefore, L is first copied to LAYER.
        LAYER = SRFC

!       Distribute the newly added residues over the SRFC structural
!       and metabolic residue pools.
        CALL PARTIT_C (
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRLRES,     !Input
     &    FRMETI, FRMETS, LAYER, RESC,                    !Input
     &    RESCE, RESDAX, SNH4,                            !Input
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,         !I/O
     &    DLTSNO3, DLTSTRUCC, DLTSTRUCE,                  !I/O
     &    ADDMETABEFLAG, FRMETFLAG)                       !Output
      ENDIF   !End of IF block on RESSRF>0.001

!     ------------
!     Soil residue
!     ------------
!     If there are soil-deposited residues, which are incorporated (and
!     not left on top of the soil, which would make them not soil-
!     deposited).
      IF (RESSOL .GT. 0.001 .AND. RESDEPTH .GT. 0.001) THEN 

!       If some of the newly applied residue is worked into the soil,
!       then the surface litter and SOM1 that were already present will
!       also be worked in. Set a flag to activate this in subroutine
!       NTRANS. For initialization, this is not needed, because there
!       is no surface litter yet; for carry-over residues, this should
!       be done by a tillage event.
        IF (DYNAMIC .EQ. 3) DOINCORPOR = .TRUE.

!       Due to soil disturbance, the decomposition rate will speed up
!       for 30 days. Set the end date for the decomposition enhancement,
!       and the depth of the disturbed soil layers for which the
!       increased decomposition rate holds.
        DISTURBNUM = DISTURBNUM + 1
        IF (DYNAMIC .EQ. 2) THEN
!         If getting here for initialization, then YRDOR has not yet
!         been set, so use YRSIM.
          DISTURBEND(DISTURBNUM) = INCYD (YRSIM, 30)
        ELSE
          DISTURBEND(DISTURBNUM) = INCYD (YRDOY, 30)
        ENDIF
        DISTURBDEPTH(DISTURBNUM) = RESDEPTH

!       Set the starting depth for counting the soil layers over which
!       to distribute the soil residue to zero.
        DEPTH = 0.0

!       Initialize a flag that determines when to jump out of the DO
!       loop.
        IOUT  = 1

!       Call IPSOIL to get the lignin concentration.
!       The residue is given a default value of 10% lignin, which can be
!       overwritten in IPSOIL (if the residue type was specified).
        PRLIG = 0.10
        CALL IPSOIL (
     &    CONTROL, RESTYPE,                                 !Input
     &    RDUMMY1, RDUMMY2, RDUMMY3, RDUMMY4, PRLIG,        !Output
     &    RDUMMY5, RDUMMY6, RDUMMY7, RDUMMY8)               !Output

!       For carry-over residue the lignin concentration has been set
!       already in SEASINIT.
        FRLRES(SRFC) = PRLIG

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
          ENDIF   !End of IF block on RESDEPTH=<DEPTH.

          RESC(L) = RESSOL * FR * 0.40

          IF (RESECONC(N) > 0.) THEN
            RESCE(L,N) = 0.40 / RESECONC(N)
          ELSE
            RESCE(L,N) = 0.0
          ENDIF

!         If L is transferred from NTRANS via the subroutine's parameter
!         string, it goes wrong, because L is the DO loop counter.
!         Therefore, L is first copied to LAYER.
          LAYER = L

!         Distribute the newly added residues over the soil 
!         structural and metabolic residue pools.
          CALL PARTIT_C (
     &      AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRLRES,   !Input
     &      FRMETI, FRMETS, LAYER, RESC,                  !Input
     &      RESCE, RESDAX, SNH4,                          !Input
     &      DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,       !Input/Output
     &      DLTSNO3, DLTSTRUCC, DLTSTRUCE,                !Input/Output
     &      ADDMETABEFLAG, FRMETFLAG)                     !Output

!         If there are no more soil layers over which to distribute
!         the residue, jump out of the DO loop. 
          IF (IOUT .EQ. 2) GO TO 200
        ENDDO   !End of loop on soil layers.

!       Continue here after jumping out of the DO loop.
200     CONTINUE
      ENDIF   !End of IF block on RESSOL and RESDEPTH

!***********************************************************************
!***********************************************************************
!     END OF SECOND DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END   !SUBROUTINE RPLACE_C

!=======================================================================
! RPLACE_C Variables
!-----------------------------------------------------------------------
! ADD        Amount of residue carbon to be added to FOM in the soil 
!                layer under consideration (kg [C] / ha)
! ADDMETABEFLAG 
! CEDAM        
! CESTR        
! CUMRES       Cumulative amount of residue application (kg[residue]/ ha)
! CUMRESE      
! DAP          Days after planting (d)
! DEPTH      Depth to the bottom of a layer from the surface (cm)
! DISTURBDEPTH 
! DISTURBEND   
! DISTURBNUM   
! DLAYR(L)     Thickness of soil layer L (cm)
! DLTLIGC      
! DLTMETABC    
! DLTMETABE    
! DLTSNH4       
! DLTSNO3       
! DLTSTRUCC    
! DLTSTRUCE    
! DOINCORPOR   
! DRESMG     Residue application depth (with the option of automatic 
!              residue application) (cm)
!                for output in CARBON.OUT (cm)
! DUMMY1..8    
! FILEIO       Filename for INP file (e.g., IBSNAT35.INP) 
! FR           Residue fraction to be added to the soil layer (fraction)
! FRDAE,       
! FRMETFLAG    
! FRMETI       
! FRMETS       
! HOLD         Temporary variable / intermediate calculation 
! ICREN      Initial conditions residue N concentration (%)
! ICRES        Initial conditions residue application (kg [residue] /ha)
! ICRID      Initial conditions residue incorporation depth (cm)
! ICRIP      Initial conditions residue incorporation percentage (%)
! IDATE        Day portion of fertilizer application date (d)
! INCYD        
! IOUT       Flag parameter that determines when to jump out of a DO 
!                loop (range: 1 or 2) 
! IRESI        Residue application method. A = Automatic application 
!                on a certain day after planting for multiple years;
!                N = No residue; R = On reported dates; D = As reported,
!                in DAP; F = Auto, with fixed amounts
! LUNIO      Logical unit number for FILEIO 
! MULTI        Current simulation of multi-year simulation                     
! NAPRES     Number of residue applications which have been applied
! NELEM        
! NL           Maximum number of soil layers                                   
! NLAYR        Number of soil layers                                           
! NRESAP     Number of observed residue applications 
! NRESDL     Number of days after planting that residues are applied 
!              (with the option of automatic residue application) (d)
! PRCEL      Cellulose fraction of the residue (fraction)
! PRCHO      Carbohydrate fraction of the residue (fraction)
! PRLIG      Lignin fraction of the residue (fraction)
! RESDAX       
! RESDAY(I)    Date of residue application (YYDDD)
! RESDEP(I)    Incorporation depth of newly added residues (cm)
! RESDEPTH     Incorporation depth of current-day's residue application
!                (cm)
! RESIDUE(I)   Amount of residue applied (kg [residue] / ha)
! RESMET(I)    Residue application method (not used yet in the model)          
! RESMIXPERC   
! RESN(I)    N concentration of the residue for Ith application (%)
! RESECONC(IEL) Nutrient concentration of the residue for current application (kg/ha)
! RESP(I)    P concentration of the residue (%)
! RESSOL     Amount of residue applied to the soil
!                kg[dry matter] / ha)
! RESTYP(I)    Residue type for Ith application
! RESTYPE      Residue type for the current application day
! RIP(I)       Residue incorporation percentage (%)
! TIMDIF       Integer function which calculates the number of days 
!                between two Julian dates (d) 
! YR           Year portion of date (for observed fertilizer or residue 
!                application dates)
! YRDIF        Function subroutine which calculates number of days 
!                between two dates (d)
! YRDOY      Current day of simulation (YYDDD)
! YRPLT      Planting date (YYDDD)
! YRSIM      Start of simulation date (YYDDD)
!=======================================================================
