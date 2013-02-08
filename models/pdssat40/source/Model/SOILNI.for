C***********************************************************************
C  SOILNI, Subroutine
C
C  Purpose: Do soil N initialization.
C-----------------------------------------------------------------------
C  REVISION HISTORY 
C  02/08/1993 PWW Header revision and minor changes.
C  02/20/1996 GH  Written.
C  02/26/1998 WTB Fixed HUMC/HUMN calculations.
C  06/09/1999 AJG Completely revised the soil N and SOM module, and made
C               a new SOM module based on the CENTURY model.
C               Also changed variable names:
C         OLD         NEW       
C         -------     -------   
C         CNI         NITCAPY
C         HUM         HUMC            
C         NHUM        HUMN
C         RNKG        ICRTN, TRTRESN
C         TFY         TFNITY          
C         WFY         WFNITY
C         WRESR       ICRT, TRTRES
C  06/21/1999 CHP Modular format
C  03/16/2000 GH  Incorporated in CROPGRO
C  06/11/2002 GH  Modified for Y2K
C  08/12/2003 CHP Added I/O error checking
C                 No re-initialization done for sequenced runs
C
C-----------------------------------------------------------------------
C  Called : NTRANS
C  Calls  : ERROR, FIND
C=======================================================================

      SUBROUTINE SOILNI(CONTROL, 
     &    HARVRES, PRCEL, PRCHO, PRLIG, RCN,              !Input
     &    SOILPROP, ST, SW,                               !Input
     &    CNRAT, FAC, FOM, FON, FPOOL, HUMC,              !Output
     &    HUMN, IUOF, IUON, NH4, NITCAPY, NO3,            !Output
     &    PHN, SNH4, SNO3, TFNITY, UREA, WFNITY)          !Output
      
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE

      LOGICAL   IUON

      CHARACTER*1 RNMODE 
      CHARACTER*6 ERRKEY, SECTION
      CHARACTER*30 FILEIO
      CHARACTER*78 MESSAGE(4)
      PARAMETER (ERRKEY = 'SOILNI')

      INTEGER ERRNUM, FOUND, IUOF, L, LNUM
      INTEGER LUNIO, MULTI, NLAYR
      INTEGER DYNAMIC, RUN

      REAL DEPMAX, DEPTH, FACTOR, ICNOD, ICRT, ICRTN, PRCEL
      REAL PRCHO, PRLIG, RCN, WSUM

      REAL BD(NL), CNRAT(NL), DLAYR(NL), DS(NL)
      REAL DUL(NL), FAC(NL), FOM(NL), FON(NL), HUMC(NL)
      REAL LL(NL), HUMN(NL), NH4(NL), NITCAPY(NL), NO3(NL)
      REAL OC(NL), PH(NL), PHN(NL), SAT(NL), SNH4(NL)
      REAL SNO3(NL), ST(NL), SW(NL), TFNITY(NL), TOTN(NL) 
      REAL UREA(NL), WFNITY(NL), WRN(NL)

      REAL FPOOL(NL,3)
      REAL TOT_HRESWT

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.

!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      Type (ResidueType) HARVRES

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DUL    = SOILPROP % DUL    
      DS     = SOILPROP % DS     
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      OC     = SOILPROP % OC     
      PH     = SOILPROP % PH     
      SAT    = SOILPROP % SAT    
      TOTN   = SOILPROP % TOTN    

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!     Seasonal initialization - run once per season
!***********************************************************************
!      IF (DYNAMIC .EQ. RUNINIT) THEN
      IF (DYNAMIC .EQ. SEASINIT) THEN     !Combine RUNINIT and SEASINIT
!     ----------------------------------------------------------------
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
!       --------------------------------------------------------------
!       Open the FILEIO input file.
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)
!       --------------------------------------------------------------
!       Find and Read INITIAL CONDITIONS Section.
!       Read root residue from previous crop and initial soil N values
!       --------------------------------------------------------------
        SECTION = '*INITI'
        CALL FIND (LUNIO, SECTION, LNUM, FOUND)
        IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO, LNUM)

!       Read the weight of root residues and nodules from the
!       previous crop. 
        READ (LUNIO, '(16X, 2F6.0)', IOSTAT = ERRNUM) ICRT, ICNOD
        LNUM = LNUM + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)

        IF (ICRT .LT. 0.) ICRT = 0.
        IF (ICNOD .LT. 0.) ICNOD = 0.

        DO L = 1, NLAYR
          LNUM = LNUM + 1
          READ(LUNIO, 100, IOSTAT=ERRNUM) NH4(L),NO3(L)
100       FORMAT (14X, 2 (1X, F5.1))
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
        ENDDO
        CLOSE (LUNIO)

        DEPMAX = DS(NLAYR)
!      ENDIF   !End of IF block for non-sequenced runs

!     --------------------------------------------------------------
!     Initialize various soil environmental parameters.
!     Non-sequenced runs only
!     --------------------------------------------------------------
!      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        DO L = 1, NLAYR
!         Calculate the pH factor for the nitrification.
          IF (PH (L) .LT. 6.0) THEN
            PHN(L) = (PH(L) - 4.5) / 1.5
          ELSEIF (PH (L) .GT. 8.0) THEN
            PHN(L) = 9.0 - PH(L)
          ELSE
            PHN(L) = 1.0
          ENDIF
          PHN(L) = AMAX1 (PHN(L), 0.0)

!         Calculate the conversion factor FAC to switch from kg[N] / ha
!         to ug[N] / g[soil].
          FAC(L) = 10. / (BD(L) * DLAYR(L))
        ENDDO
!      ENDIF   !End of non-sequenced run initialization

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN  
!        !Non-sequenced runs
!!       Set the flag for the presence of urea to false.
        IUON = .FALSE.
        IUOF = 0

        DO L = 1, NLAYR
!         --------------------------------------------------------------
!         Initialize various soil environmental parameters.
!         --------------------------------------------------------------
!         Calculate yesterday's soil water factor. When calculating
!         the nitrification, WFNITY will be compared with today's soil
!         water factor, and the maximum will apply.
          IF (SW(L) .GT. DUL(L)) THEN
            WFNITY(L) = 1.0 - ((SW(L) - DUL(L)) / (SAT(L) - DUL(L)))
          ELSE
            WFNITY(L) = (SW(L) - LL(L)) / DUL(L)
          ENDIF
          WFNITY(L) = AMAX1 (WFNITY(L), 0.0)

!         Calculate yesterday's soil temperature factor. When
!         calculating the nitrification, TFNITY will be compared with
!         today's soil temperature factor, and the maximum will apply.
          IF (ST(L) .LT. 5.0) THEN
            TFNITY(L) = 0.0
          ELSE
            TFNITY(L) = 0.0009766 * ST(L) * ST(L)
          ENDIF

!         Initialize the index that indicates the capacity for
!         nitrification to proceed. This index will be recalculated
!         every day.
          NITCAPY(L) = 0.1

!         Calculate the conversion factor FAC to switch from kg[N] / ha
!         to ug[N] / g[soil].
          FAC(L) = 10. / (BD(L) * DLAYR(L))

!         --------------------------------------------------------------
!         Initialize soil mineral nitrogen and urea.
!         --------------------------------------------------------------
!         Limit NO3 and NH4 to >=0.01.
          NO3(L)  = AMAX1 (NO3(L), 0.01)
          NH4(L)  = AMAX1 (NH4(L), 0.01)

!         Convert the N concentrations to kg[N] / ha per soil layer.
          SNO3(L)    = NO3(L) / FAC(L)
          SNH4(L)    = NH4(L) / FAC(L)

!         Initialize urea.
          UREA(L) = 0.0

!         --------------------------------------------------------------
!         Initialize soil organic matter and residue pools.
!         --------------------------------------------------------------
!         Calculate the stable organic matter pool HUMC in kg[C] / ha.
          HUMC(L) = OC(L) * 1000. * BD(L) * DLAYR(L)

!         If the soil N concentration is known, calculate the humus N
!         from the total soil N (= organic + inorganic).
          IF (TOTN(L) .GT. 0.001) THEN
            HUMN(L) = TOTN(L)* DLAYR(L) * BD(L) * 1.E03 
!           Humus C:N ratio.
            CNRAT(L) = HUMC(L) / HUMN(L)

!           Limit C:N ratio to a minimum of 10.
            IF (CNRAT(L) .LT. 10.0) THEN
              WRITE(MESSAGE(1),105) 
              WRITE(MESSAGE(2),110) L, CNRAT(L) 
              WRITE(MESSAGE(3),115) 
              WRITE(MESSAGE(4),120) 
              CALL WARNING(4, ERRKEY, MESSAGE)

  105 FORMAT('Warning: The initialized C:N ratio of the soil organic ')
  110 FORMAT('matter (humus) in layer ',I3,' is unlikely: ', F8.1)
  115 FORMAT('A C:N ratio of 10.0 will be used to calculate initial ',
     &                'soil organic N.')
  120 FORMAT('Please check your SOIL.SOL file.' )

              CNRAT(L) = 10.0
              HUMN(L) = HUMC(L) / CNRAT(L)
            ENDIF

          ELSE
!           If soil N is not known, assume a humus C/N ratio of 10.
            CNRAT(L) = 10.0
            HUMN(L) = HUMC(L) / CNRAT(L)
          ENDIF   !End of IF block on TOTN.

          HUMN(L) = AMAX1 (HUMN(L), 0.01)

!         Residue pools.
          FOM(L)     = 0.0
          FON(L)     = 0.0
          FPOOL(L,1) = 0.0
          FPOOL(L,2) = 0.0
          FPOOL(L,3) = 0.0
        END DO   !End of soil layer loop.
C
C       Calculate N contributions
C       RCN is now read in from SOILN980.SOL .. was set to 40.0
        IF (RCN .LE. 0) THEN  !TEMP - CHP
          RCN = 40.0          !ERROR NEEDS TO BE DEBUGGED
        ENDIF                 !RCN = 0 HERE WITH TONY'S WHEAT FILES
        ICRTN = ICRT * 0.40 / RCN

      ENDIF  !End of RUN if-construct

!-----------------------------------------------------------------------
!     This section done for both sequenced and non-sequenced runs.
!-----------------------------------------------------------------------
!     Distribute the root residues over the soil layers according to a
!     default exponential function.
      WSUM  = 0.0
      DEPTH = 0.0
      DO L = 1, NLAYR
         DEPTH  = DEPTH + DLAYR(L)
         WRN(L) = EXP(-3.0 * DEPTH / DEPMAX)
         WSUM   = WSUM + WRN(L)
      END DO

      DO L = 1, NLAYR
        IF (WSUM .GT. 0.0) THEN
          FACTOR = WRN(L) / WSUM
        ELSE
          FACTOR = 0.0
        ENDIF

!       Add the initial or carry-over root and nodule residues to the 
!       fresh-organic-matter pool.
        IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
          !Initialize from FILEIO
          FOM(L) = FOM(L) + (ICRT + ICNOD) * FACTOR
          FON(L) = FON(L) + (ICRTN + 0.3 / 6.25 * ICNOD) * FACTOR

          !Save root residue for use in N balance
          HARVRES % RESWT(L)  = (ICRT + ICNOD) * FACTOR
          HARVRES % RESE(L,1) = (ICRTN + 0.3 / 6.25 * ICNOD) * FACTOR
        ELSE
          !Initialize with harvest residues from previous crop
          FOM(L) = FOM(L) + HARVRES % RESWT(L)
          FON(L) = FON(L) + HARVRES % RESE(L,1)
        ENDIF

        FPOOL(L,1) = FOM(L) * PRCHO   !was 0.20
        FPOOL(L,2) = FOM(L) * PRCEL   !was 0.70
        FPOOL(L,3) = FOM(L) * PRLIG   !was 0.10
      ENDDO   !End of soil layer loop.

!     Temp code for debugging:
      TOT_HRESWT = HARVRES % RESWT(0)
      DO L = 1, NLAYR
        TOT_HRESWT = TOT_HRESWT + HARVRES % RESWT(L)
      ENDDO
!      print *, tot_hreswt

!     ------------------------------------------------------------------
!     Initial shoot residues.
!     ------------------------------------------------------------------
!     The initial or carry-over shoot residues are handled in the
!     subroutine RPLACE, because they may be incorporated (partly).

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END !SUBROUTINE SOILNI

!=======================================================================
! SOILNI Variables
!
! BD(L)      Bulk density, soil layer L (g [soil] / cm3 [soil])
! CNRAT(L)   C/N ratio of humus or humus pool in soil layer L
!              (kg [C] / kg [N])
! DEPMAX     Maximum depth of reported soil layers (cm)
! DEPTH      Depth to the bottom of a layer from the surface (cm)
! DLAYR(L)   Soil thickness in layer L (cm)
! DMOD       Factor to adjust the mineralization rate for certain atypical 
!              soils (range 0-1) 
! DOY        Current day of simulation (d)
! DS(L)      Cumulative depth in soil layer L (cm)
! DUL(L)     Volumetric soil water content at Drained Upper Limit in soil 
!              layer L (cm3[water]/cm3[soil])
! DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!              INTEGR, OUTPUT, or FINAL 
! ERRKEY     Subroutine name for error file 
! ERRNUM     Error number for input 
! FAC(L)     Conversion factor to switch from kg [N] / ha to µg [N] / g 
!              [soil] for soil layer L 
! FACTOR     Weighting factor for residue contribution to soil layers.  
!              Decreases exponentially with depth (fraction)
! FILEIO     Filename for input file (e.g., IBSNAT35.INP) 
! FOM(L)     Fresh organic residue in soil layer L (kg [dry matter] / ha)
! FON(L)     Nitrogen in fresh organic matter in soil layer L (kg [N] / ha)
! FOUND      Indicator that good data was read from file by subroutine FIND 
!              (0 - End-of-file encountered, 1 - NAME was found) 
! FPOOL(L,J) FOM pool in soil layer L: J=1:carbohydrate, 2:cellulose, 
!              3:lignin (kg [residue pool] / ha)
! HUMC(L)    Carbon in stable organic matter (humus) (kg [C] / ha)
! HUMN(L)    Nitrogen in stable organic matter (humus) (kg [N] / ha)
! ICNOD      Initial mass of nodule residues in soil (left-over from 
!              previous crop) (kg [residue] /ha)
! ICRT       Initial mass of root residues in soil (left-over from previous 
!              crop) (kg [residue] /ha)
! ICRTN      Initial N content of root residues (kg [N] /ha)
! IUOF       Critical Julian day when all urea is assumed to be hydrolyzed 
!              (this is assumed to occur 21 days after the urea application)
!              (d)
! IUON       Flag indicating presence of urea (true or false) 
! LL(L)      Volumetric soil water content in soil layer L at lower limit
!              (cm3 [water] / cm3 [soil])
! LNUM       Current line number of input file 
! LUNIO      Logical unit number for FILEIO 
! MULTI      Current simulation year (=1 for first or single simulation, 
!              =NYRS for last seasonal simulation) 
! NH4(L)     Ammonium N in soil layer L (µg[N] / g[soil])
! NITCAPY(L) Previous day's nitrification potential in soil layer L, 
!              indicating whether there may be a lag phase for 
!              nitrification to proceed (range: 0-1) 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! NO3(L)     Nitrate in soil layer L (µg[N] / g[soil])
! OC(L)      Organic carbon content of layer (%)
! PH(L)      pH in soil layer L 
! PHN(L)     Factor for pH effect on nitrification rate (range 0-1) for 
!              soil layer L 
! PRCEL      Cellulose fraction of the residue (fraction)
! PRCHO      Carbohydrate fraction of the residue (fraction)
! PRLIG      Lignin fraction of the residue (fraction)
! RCN        C/N ratio of initial root residue (kg [C] / kg [N])
! SAT(L)     Volumetric soil water content in layer L at saturation
!              (cm3 [water] / cm3 [soil])
! SECTION    Section name in input file 
! SNH4(L)    Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)    Total extractable nitrate N in soil layer L (kg [N] / ha)
! ST(L)      Soil temperature in soil layer L (°C)
! SW(L)      Volumetric soil water content in layer L
!              (cm3 [water] / cm3 [soil])
! TFNITY(L)  Yesterday’s soil temperature factor for nitrification (range 
!              0-1) 
! TOTN(L)    Total N in soil (%)
! UREA(L)    Amount of urea in soil layer L (kg [N] / ha)
! WFNITY(L)  Yesterday’s soil water factor for nitrification rate (range 
!              0-1) 
! WRN(L)     Exponential root distribution factor for soil layer L for 
!              distribution of initial root residues 
! WSUM       Sum of WRN (root distribution) through soil profile 
!=======================================================================
