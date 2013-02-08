!***********************************************************************
!  SOILNI_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Do soil N initialization.
!
!  REVISION HISTORY
!  02/08/1993 PWW Header revision and minor changes.
!  02/20/1996 GH  Written.
!  02/26/1998 WTB/AJG  Fixed HUMC/HUMN calculations.
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!               a new SOM module based on the CENTURY model.
!               Also changed variable names:
!               OLD       NEW                  OLD       NEW
!               ------    ------               ------    ------ 
!               AINH4     TNH4                 TIFOM     TFOM
!               AINO3     TNO3                 TIFON     TFON
!               ANH4      TNH4                 TSOC      THUMC
!               ANO3      TNO3                 TSON      THUMN
!               CNI       NITRIFCAPY           WFY       WFNITY
!               HUM       HUMC                 WRESR     ICRT, TRTRES
!               NHUM      HUMN                 
!               RNKG      ICRTE, TRTRESE      
!               TFY       TFNITY
!  06/21/1999 CHP Modular format.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!                modules with CHP's modular structure.
!  06/11/2002 GH Modifield for Y2K
!  11/11/2002 AJG Added DLAYR to the PARTIT_C parameter string.
!  08/12/2003 CHP Added I/O error checking
!                 No re-initialization done for sequenced runs.
!  07/21/2005 CHP Fixed initialization problem with harvest residue
!
!  Called: CENTURY
!  Calls : ERROR, FIND, OPSOMLIT_C, PARTIT_C, SOMFIX_C, 
!          SOMLITPRINT_C, TSOMLIT_C
!***********************************************************************

      SUBROUTINE SOILNI_C (CONTROL, 
     &  IDETL, SOILPROP, ST, SW,                          !Input

     &  CUMRES, CUMRESE,                                  !Input/output

     &  ADDMETABEFLAG, AMINRL, ACCCO2, ACCMNR, CEDAM,     !Output
     &  CES1, CES1M, CES1T, CES1X, CES2, CES21I,          !Output
     &  CES21M, CES21S, CES21T, CES21X, CES2LI, CES2LM,   !Output
     &  CES2LS, CES2LX, CES3, CES3M, CES3T, CES3X,        !Output
     &  CESTR, CO2MET, CO2S1, CO2S2, CO2S3,               !Output
     &  CO2STR, CULMETQ, CULS1Q, CULS2Q, CULS3Q,          !Output
     &  CULSTRQ, DECMET, DECS1, DECS2, DECS3, DECSTR,     !Output
     &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,           !Output
     &  DLTSNO3, DLTSTRUCC, DLTSTRUCE, DSNC, FAC, FRDAE,  !Output
     &  FRMETFLAG, FRMETI, FRMETS, HARVRES, IUOF, IUON,   !Output
     &  LIGC, LIGSTR, LITC, LITE, METABC, METABE,         !Output
     &  NH4, NITRIFCAPY, NO3, PHFLAG, PHN, RESDAX,        !Output
     &  S1S3, S2S3, SNH4, SNO3, SOM1C, SOM1E, SOM2C,      !Output
     &  SOM2E, SOM3C, SOM3E, SOMFRACFLAG, SSOMC, SSOME,   !Output
     &  STRUCC, STRUCE, TFNITY, TMETABC, TMETABE, TNH4,   !Output
     &  TNO3, TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM3C,     !Output
     &  TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE, TXS1,     !Output
     &  UREA, WFNITY, WRN)                                !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      INTEGER DYNAMIC, ERRNUM, RUN,
     &  FOUND, IEL, IUOF, L, LAYER, LNUM, LUNIO, 
     &  MULTI, N, NLAYR, YRDOY
      PARAMETER (N = 1)

      REAL CO2S1I, CO2S1S, CO2S2, CO2S3, CULMETQ,
     &  CULS1Q, CULS2Q, CULS3Q, CULSTRQ, CUMRES, DEPMAX, DEPTH,
     &  DSNC, FACTOR, FRMETI, FRMETS, ICNOD, ICRT,
     &  PRLIG, RCN, RESDAX, RDUMMY1, RDUMMY2, RDUMMY3,
     &  RDUMMY4, RDUMMY5, RDUMMY6, RDUMMY7, S1S3I, S1S3S,
     &  S2S3I, S2S3S, TLITC, TMETABC, TNH4, TNO3,  
     &  TSOM1C, TSOM2C, TSOM3C, TSOMC,
     &  TSTRUCC, TXS1I, TXS1S, WSUM

      REAL ACCCO2(0:1), BD(NL), CEDAM(3), CESTR(3),
     &  CLAY(NL), CO2MET(0:1), CO2S1(0:NL), CUMRESE(3),
     &  DECMET(0:1), DECS1(0:1), DECS2(1), DECS3(1), DECSTR(0:1),
     &  DLAYR(NL), DLTLIGC(0:NL), DLTMETABC(0:NL), DLTSNH4(NL),
     &  DLTSNO3(NL), DLTSTRUCC(0:NL), DS(NL), DUL(NL), FAC(NL),
     &  FRDAE(3), FRLRES(0:NL), ICRTE(3), LIGSTR(0:1), LITC(0:NL), 
     &  LL(NL), METABC(0:NL), NH4(NL), NITRIFCAPY(NL), NO3(NL), OC(NL),
     &  PH(NL), PHN(NL), RESC(0:NL), S1S3(NL),
     &  S2S3(NL), SAND(NL), SAT(NL), SNH4(NL), SNO3(NL),
     &  SOM1C(0:NL), SOM2C(NL), SOM3C(NL), SSOMC(0:NL), ST(NL),
     &  STRUCC(0:NL), SW(NL), TFNITY(NL), TLITE(3), TMETABE(3),
     &  TOTN(NL), TSOM1E(3), TSOM2E(3), TSOM3E(3), TSOME(3),
     &  TSTRUCE(3), TXS1(NL), UREA(NL), WFNITY(NL), WRN(NL)

      REAL ACCMNR(0:NL,3), AMINRL(NL,3), CES1(0:NL,3), CES1M(0:1,3),
     &  CES1T(0:1,3), CES1X(0:1,3), CES2(NL,3), CES21I(0:0,3),
     &  CES21M(0:1,3), CES21S(0:1,3), CES21T(1,3), CES21X(0:1,3),
     &  CES2LI(0:1,3), CES2LM(0:1,3), CES2LS(0:1,3), CES2LX(0:1,3),
     &  CES3(NL,3), CES3M(1,3), CES3T(1,3), CES3X(1,3),
     &  CO2STR(0:1,2), DLTMETABE(0:NL,3), DLTSTRUCE(0:NL,3), LIGC(0:NL),
     &  LITE(0:NL,3), METABE(0:NL,3), RESCE(0:NL,3), RESE(NL,3),
     &  SOM1E(0:NL,3), SOM2E(NL,3), SOM3E(NL,3), 
     &  SSOME(0:NL,3), STRUCE(0:NL,3)
      REAL SomLitC(0:NL), SomLitE(0:NL,NELEM)

      CHARACTER*1 IDETL, RNMODE
      CHARACTER*5  RESTYPE
      CHARACTER*6  ERRKEY, SECTION
      CHARACTER*30 FILEIO
      PARAMETER (ERRKEY = 'SOILNI')

      LOGICAL ADDMETABEFLAG, FRMETFLAG, IUON, PHFLAG
      LOGICAL  SOMFRACFLAG(NL)

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (ResidueType) HARVRES

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      BD     = SOILPROP % BD     
      CLAY   = SOILPROP % CLAY    
      DLAYR  = SOILPROP % DLAYR  
      DUL    = SOILPROP % DUL    
      DS     = SOILPROP % DS     
      LL     = SOILPROP % LL     
      OC     = SOILPROP % OC     
      PH     = SOILPROP % PH     
      NLAYR  = SOILPROP % NLAYR  
      SAND   = SOILPROP % SAND   
      SAT    = SOILPROP % SAT    
      TOTN   = SOILPROP % TOTN    

!***********************************************************************
!***********************************************************************
!     Run initialization - Called once per simulation.
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!       ----------------------------------------------------------------
!       Done only for non-sequenced runs.
        IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
!         Read from FILEIO (moved from IPIBS)
!         --------------------------------------------------------------
!         Open the FILEIO input file.
          OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT = ERRNUM)

!         If the file can't be found, call an error.
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)

!         --------------------------------------------------------------
!         Find and Read INITIAL CONDITIONS Section
!         --------------------------------------------------------------
          SECTION = '*INITI'

!         Find the line number from where to start reading.
          CALL FIND (LUNIO, SECTION, LNUM, FOUND)

!         If the Initial Conditions section can't be found, call an
!         error, or else read the input data.
          IF (FOUND .EQ. 0) CALL ERROR (SECTION, 42, FILEIO, LNUM)

!         Read the weight of root residues and nodules from the
!         previous crop. The shoot residues are dealt with in
!         subroutine RPLACE, because they may be incorporated
!         (partly).
          READ (LUNIO,'(16X, 2F6.0)', IOSTAT = ERRNUM) ICRT, ICNOD
          LNUM = LNUM + 1
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
          IF (ICRT .LT. 0.) ICRT = 0.
          IF (ICNOD .LT. 0.) ICNOD = 0.

          DO L = 1, NLAYR
!           Read the layer depth DS and its NH4 and NO3 concentration.
            READ (LUNIO, 100, IOSTAT = ERRNUM) NH4(L), NO3(L)
100         FORMAT (14X, 2(1X, F5.1))
            LNUM = LNUM + 1
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
          ENDDO

!         Continue here after jumping out of the DO loop with an error
!         (thus the end of the Initial Conditions section is reached).
200       CONTINUE

!         Calculate the initial SOM pools.
          CALL SOMINIT_C (CONTROL,
     &    BD, CES1, CES1M, CES1X, CES2, CES21M, CES21X,   !Input
     &    CES3, CES3M, CES3X, CLAY, CO2S1I, CO2S1S,       !Input
     &    DLAYR, NLAYR, OC, S1S3I, S1S3S, S2S3I, S2S3S,   !Input
     &    SAND, TOTN, TXS1I, TXS1S,                       !Input
     &    ACCMNR, ACCCO2, CO2S1, S1S3, S2S3,              !Output
     &    SOM1C, SOM1E, SOM2C, SOM2E, SOM3C,              !Output
     &    SOM3E, SOMFRACFLAG, TXS1)                       !Output
        ENDIF   !End of IF block for RUN=1.

!       Close the input file.
        CLOSE (LUNIO)

!       ----------------------------------------------------------------
!       Find and Read Residue Lignin and Root C:N ratio and
!       integration depth.
!       ----------------------------------------------------------------
!       Get the root C:N ratio and the residue lignin concentration from
!       SOILN980.SOL. Set the residue type to a default value. Read the
!       depth to which C and N are integrated for output purposes. Set
!       the residue type to a default value. Replace those variables
!       that are not used by a dummy (RDUMMY because it are REAL
!       variables).
!This lignin value should be set in fileX for the initial residues.
        RESTYPE = 'RE001'
        CALL IPSOIL (CONTROL,RESTYPE,                     !Input
     &    RDUMMY1, DSNC, RDUMMY3, RDUMMY4, PRLIG,         !Output
     &    RCN, RDUMMY5, RDUMMY6, RDUMMY7)                 !Output

!       ----------------------------------------------------------------
!       Read the fixed (site- and crop-independent) parameters for the 
!       CENTURY-based SOM/litter module.
!       ----------------------------------------------------------------
        CALL SOMFIX_C (CONTROL, 
     &    CEDAM, CES1, CES1M, CES1T, CES1X, CES2,         !Output
     &    CES21I, CES21M, CES21S, CES21T, CES21X,         !Output
     &    CES2LI, CES2LM, CES2LS, CES2LX, CES3, CES3M,    !Output
     &    CES3T, CES3X, CESTR, CO2MET, CO2S1,             !Output
     &    CO2S1I, CO2S1S, CO2S2, CO2S3, CO2STR,           !Output
     &    CULMETQ, CULS1Q, CULS2Q, CULS3Q, CULSTRQ,       !Output
     &    DECMET, DECS1, DECS2, DECS3, DECSTR,            !Output
     &    FRDAE, FRMETI, FRMETS, LIGSTR, RESDAX,          !Output
     &    S1S3I, S1S3S, S2S3I, S2S3S, TXS1I, TXS1S)       !Output

!       Close the input file.
        CLOSE (LUNIO)

!       ----------------------------------------------------------------
!       Write output headers in OPSOMLIT.OUT.
!       ----------------------------------------------------------------
        CALL OPSOMLIT_C (CONTROL, IDETL, 
     &  ACCCO2, CUMRES, CUMRESE, LITC, LITE, METABC,      !Input
     &  METABE, SOILPROP, SOM1C, SOM1E, SOM2C, SOM2E,     !Input
     &  SOM3C, SOM3E, SomLitC, SomLitE, STRUCC, STRUCE,   !Input
     &  TLITC, TLITE, TMETABC, TMETABE, TSOM1C,           !Input
     &  TSOM1E, TSOM2C, TSOM2E, TSOM3C, TSOM3E,           !Input
     &  TSOMC, TSOME, TSTRUCC, TSTRUCE)                   !Input

!     --------------------------------------------------------------
!     Initialize various soil environmental parameters.
!     Non-sequenced runs only.
!     --------------------------------------------------------------
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        DO L = 1, NLAYR
!         Set bulk density to an average value, if not given.
!         This is done in Tillage.for now. - chp 10/3/01
!          IF (BD(L) .LT. 0.001) THEN
!            BD(L) = 1.2
!          ENDIF

!         Set the soil pH to an average value, if not given.
          IF (PH(L) .LE. 1.0 .OR. PH(L) .GT. 10.0) THEN
            PH(L) = 7.0

!           Set a flag to get a warning in NCHECK that the pH was
!           changed.
            PHFLAG = .TRUE.
          ENDIF

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
        ENDDO   !End of soil layer loop.
      ENDIF   !End of non-sequenced run initialization 

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!     Set the flag for the presence of urea to false.
      IUON = .FALSE.
      IUOF = 0

!     07/21/2005 CHP moved from RPLACE_C
      CUMRES  = 0.
      CUMRESE = 0.

      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
!       ----------------------------------------------------------------
!       Non-sequenced runs.
!       ----------------------------------------------------------------

        IF (MULTI .GT. 1) THEN
!         --------------------------------------------------------------
!         Re-read initial conditions from FILEIO for multi-year 
!         (seasonal)analysis.
!         --------------------------------------------------------------
          OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, 0)

!         --------------------------------------------------------------
!         Find and Re-read INITIAL CONDITIONS Section
!         --------------------------------------------------------------
          SECTION = '*INITI'

!         Find the line number from where to start reading.
          CALL FIND (LUNIO, SECTION, LNUM, FOUND)

!         If the Initial Conditions section can't be found, call an
!         error, or else read the input data.
          IF (FOUND .EQ. 0) THEN
            CALL ERROR (SECTION, 42, FILEIO, LNUM)
          ELSE
!           Read the weight of root residues and nodules from the
!           previous crop. The shoot residues are dealt with in
!           subroutine RPLACE, because they may be incorporated
!           (partly).
            READ (LUNIO,'(16X, 2F6.0)', IOSTAT = ERRNUM) ICRT, ICNOD
            LNUM = LNUM + 1
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, FILEIO, LNUM)
            IF (ICRT .LT. 0.) ICRT = 0.
            IF (ICNOD .LT. 0.) ICNOD = 0.

            DO L = 1, NLAYR
!             Read the layer depth and its NH4 and NO3 concentration.
              READ (LUNIO, 100, IOSTAT = ERRNUM) NH4(L), NO3(L)
              LNUM = LNUM + 1
              IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY, ERRNUM, FILEIO,LNUM)
            ENDDO

!           Continue here after jumping out of the DO loop with an
!           error (thus the end of the Initial Conditions section was
!           reached).
700         CONTINUE
          ENDIF   !End of IF block on FOUND.

!         Close the input file.
          CLOSE (LUNIO)

!         --------------------------------------------------------------
!         Re-read the fixed (site- and crop-independent) parameters for
!         the CENTURY-based SOM/litter module.
!         --------------------------------------------------------------
!         Almost all SOMFIX_C parameters remain unchanged (thus no need
!         to re-read), but the file also has the initial C:E ratios of
!         the SOM pools, which use the same variables that lateron are
!         used for the actual C:E ratio (and thus have changed).
          CALL SOMFIX_C (CONTROL, 
     &      CEDAM, CES1, CES1M, CES1T, CES1X, CES2,       !Output
     &      CES21I, CES21M, CES21S, CES21T, CES21X,       !Output
     &      CES2LI, CES2LM, CES2LS, CES2LX, CES3, CES3M,  !Output
     &      CES3T, CES3X, CESTR, CO2MET, CO2S1,           !Output
     &      CO2S1I, CO2S1S, CO2S2, CO2S3, CO2STR,         !Output
     &      CULMETQ, CULS1Q, CULS2Q, CULS3Q, CULSTRQ,     !Output
     &      DECMET, DECS1, DECS2, DECS3, DECSTR,          !Output
     &      FRDAE, FRMETI, FRMETS, LIGSTR, RESDAX,        !Output
     &      S1S3I, S1S3S, S2S3I, S2S3S, TXS1I, TXS1S)     !Output

!         Re-read residue lignin and root C:N ratio.
          RESTYPE = 'RE001'
          CALL IPSOIL (CONTROL,RESTYPE,                   !Input
     &      RDUMMY1, RDUMMY2, RDUMMY3, RDUMMY4, PRLIG,    !Output
     &      RCN, RDUMMY5, RDUMMY6, RDUMMY7)               !Output
        ENDIF   !End of IF block on MULTI.
        
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
            TFNITY(L) = 0.0009766 * ST(L) ** 2.
          ENDIF

!         Initialize the index NITRIFCAPY which indicates the capacity 
!         for nitrification to proceed. This index will be recalculated
!         every day.
          NITRIFCAPY(L) = 0.1

!         Calculate the conversion factor FAC to switch from kg[N] / ha
!         to ug[N] / g[soil].
          FAC(L) = 10. / (BD(L) * DLAYR(L))

!         --------------------------------------------------------------
!         Initialize soil mineral nitrogen and urea.
!         --------------------------------------------------------------
!         Limit NO3 and NH4 to >=1.E-6.
          NO3(L)  = AMAX1 (NO3(L), 1.E-6)
          NH4(L)  = AMAX1 (NH4(L), 1.E-6)

!         Convert the N concentrations to kg[N] / ha per soil layer.
          SNO3(L) = NO3(L) / FAC(L)
          SNH4(L) = NH4(L) / FAC(L)

!         Prepare soil N or P data for the CENTURY-based SOM and litter
!         model.
!To be done for P also.
          DO IEL = 1, NELEM
            AMINRL(L,IEL) = SNH4(L) + SNO3(L)
          END DO   !End of IEL loop.

!         Initialize urea.
          UREA(L) = 0.0
        END DO   !End of soil layer loop.

        TNH4 = 0.
        TNO3 = 0.

!       ----------------------------------------------------------------
!       Initialize soil organic matter and residue pools.
!       ----------------------------------------------------------------
!       Initialize to zero. Not needed for the SOM pools, which get a
!       value in SOMINIT.
        DO L = 0, NLAYR   !With SRFC layer.
          METABC(L) = 0.
          STRUCC(L) = 0. 
          LIGC(L) = 0.
          DO IEL = 1, NELEM
            METABE(L,IEL) = 0.
            STRUCE(L,IEL) = 0. 
          END DO 
        END DO

        TLITC = 0.
        DO IEL = 1, NELEM
          TLITE(IEL) = 0.
        END DO 

!       Set the initial value of CES1, CES2, CES3 the same for every
!       layer instead of using CES1(SOIL,N) etc. Take a backward DO loop, so
!       that it doesn't overwrite CES1(SOIL,N) by CES1(1,N).
        DO L = NLAYR, 0, -1
          DO IEL = 1, 3
            IF (L == 0) THEN
              CES1(0,IEL) = CES1(0,IEL)
	      ELSE
              CES1(L,IEL) = CES1(1,IEL)
              CES2(L,IEL) = CES2(1,IEL)
              CES3(L,IEL) = CES3(1,IEL)
            ENDIF   !End of IF block on L==SRFC.
          ENDDO   !End of DO loop on IEL.
        ENDDO   !End of DO loop on L.

!       Calculate the initial SOM pools.
        CALL SOMINIT_C (CONTROL,
     &    BD, CES1, CES1M, CES1X, CES2, CES21M, CES21X,   !Input
     &    CES3, CES3M, CES3X, CLAY, CO2S1I, CO2S1S,       !Input
     &    DLAYR, NLAYR, OC, S1S3I, S1S3S, S2S3I, S2S3S,   !Input
     &    SAND, TOTN, TXS1I, TXS1S,                       !Input
     &    ACCMNR, ACCCO2, CO2S1, S1S3, S2S3,              !Output
     &    SOM1C, SOM1E, SOM2C, SOM2E, SOM3C,              !Output
     &    SOM3E, SOMFRACFLAG, TXS1)                       !Output

!       Calculate root N contributions.
!       RCN is now read in from SOILN980.SOL .. was set to 40.0
        DO IEL = 1, NELEM
          ICRTE(N) = ICRT * 0.40 / RCN
!...To be done for P also...
        END DO

!      ELSE
!!    chp 07/14/2003 There should be no initialization needed for 
!!        sequenced runs, except to modify management dates and to 
!!        zero some cumulative variables.
!!
!!       ----------------------------------------------------------------
!!       Sequenced runs.
!!       ----------------------------------------------------------------
!!       For sequenced runs, there may be urea present.  If so, set flag 
!!       (IUON) to true.  All the urea is assumed to have hydrolyzed 21 
!!       days after the urea application; on day IUOF.
!        DO L = 1, NLAYR
!          IF (UREA(L) .GT. 0.001) THEN
!            IUON = .TRUE.
!            IUOF = DOY + 21
!
!!           If not all urea has hydrolyzed this year, transfer the
!!           remaining days to next year.
!            IF (DOY .GT. 344) IUOF = 21 - (365 - DOY)
!
!!           Jump out of the DO loop if there was still urea in any
!!           layer.
!            GOTO 800
!          ENDIF
!        ENDDO
!
!!       Continue here after jumping out of the DO loop.
!800     CONTINUE
      ENDIF  !End of IF block on RNMODE.

!     Get detailed SOM output. Do this here and not only after adding
!     root and shoot residue, because otherwise one cannot check the
!     SOM content against the SOIL.SOL values (if residues and
!     SOM1(SRFC) are incorporated). First update the total amount of
!     SOM (all pools together).
      CALL TSOMLIT_C (
     &  METABC, METABE, NLAYR, SOM1C, SOM1E, SOM2C,       !Input
     &  SOM2E, SOM3C, SOM3E, STRUCC, STRUCE,              !Input
     &  LITC, LITE, SSOMC, SSOME, TLITC, TLITE,           !Output
     &  TMETABC, TMETABE, SomLitC, SomLitE,               !Output
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM3C,           !Output
     &  TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)           !Output

!     Get detailed SOM and litter output for checking the SOM
!     initialization (litter has not yet been set and will be printed
!     from CENTURY section SEASINIT). Use RUN to prevent it from printing
!     again with a SEASINIT of a follow-up year in a sequential run.
      IF (IDETL .EQ. 'D' .AND. RUN == 1) THEN
        CALL SOMLITPRINT_C (CONTROL,
     &    DLAYR, LITC, LITE, METABC, METABE, NLAYR,       !Input
     &    SOM1C, SOM1E, SOM2C, SOM2E, SOM3C, SOM3E,       !Input
     &    SSOMC, SSOME, STRUCC, STRUCE, TLITC, TLITE,     !Input
     &    TMETABC, TMETABE, TSOM1C, TSOM1E, TSOM2C,       !Input
     &    TSOM2E, TSOM3C, TSOM3E, TSOMC, TSOME, TSTRUCC,  !Input
     &    TSTRUCE)                                        !Input
      ENDIF

!     Write headers and explanation of abbreviations for SOMLIT.OUT
!     (output file with the standard DSSAT output format, i.e. each
!     line represents one day).
      CALL OPSOMLIT_C (CONTROL, IDETL, 
     &  ACCCO2, CUMRES, CUMRESE, LITC, LITE, METABC,      !Input
     &  METABE, SOILPROP, SOM1C, SOM1E, SOM2C, SOM2E,     !Input
     &  SOM3C, SOM3E, SomLitC, SomLitE, STRUCC, STRUCE,   !Input
     &  TLITC, TLITE, TMETABC, TMETABE, TSOM1C,           !Input
     &  TSOM1E, TSOM2C, TSOM2E, TSOM3C, TSOM3E,           !Input
     &  TSOMC, TSOME, TSTRUCC, TSTRUCE)                   !Input

!     ------------------------------------------------------------------
!     This section done for both sequenced (RNMODE='Q') and non-sequenced
!     (RUN=1) runs.
!     ------------------------------------------------------------------

!     ------------------------------------------------------------------
!     Initial root residues.
!     ------------------------------------------------------------------
!     Distribute the root residues over the soil layers according to a
!     default exponential function.
      WSUM  = 0.0
      DEPTH = 0.0
      DEPMAX = DS(NLAYR)
      DO L = 1, NLAYR
        DEPTH  = DEPTH + DLAYR (L)
        WRN(L) = EXP (-3.0 * DEPTH / DEPMAX)
        WSUM   = WSUM + WRN(L)
      END DO

      DO L = 1, NLAYR
        IF (WSUM .GT. 0.0) THEN
          FACTOR = WRN(L) / WSUM
        ELSE
          FACTOR = 0.0
        ENDIF

!       Add the root + nodule residues to the residue pool, which then
!       in subroutine PARTIT will be distributed over the structural and
!       metabolic pools.
        IF ((RUN .EQ. 1 .OR. INDEX('QF', RNMODE) .LE. 0) .AND.
     &    (ICRTE(N) .GT. 0. .OR. ICNOD .GT. 0.) .AND.
     &    FACTOR .GT. 0.) THEN

!         Initial root + nodule residue C and C/E ratio. Give nodules
!         a protein concentration of 30% (see PRONOD in species files).
!...To be done for P also...

          RESC(L) = (ICRT + ICNOD) * 0.40 * FACTOR
          RESCE(L,N) = RESC(L) / ((ICRTE(N) + 0.3 / 6.25 * ICNOD) *
     &      FACTOR)

!         Lignin concentration of root residue.
!Should be made flexible via fileX.
          FRLRES(L) = 0.10

!       Carry-over root residue C and C/E ratio.
        ELSEIF (INDEX ('QF',RNMODE) .GT. 0) THEN
          RESC(L) = RESC(L) + 0.4 * HARVRES % RESWT(L)

!         7/21/2005 CHP Don't add harvest residue in with organic matter
!             applications.
!          CUMRES  = CUMRES + HARVRES % RESWT(L)
          DO IEL = 1, NELEM
!chp 7/1/2005 RESE(L,IEL) = RESE(L,IEL) + HARVRES % RESE(L,IEL)
            RESE(L,IEL) = HARVRES % RESE(L,IEL)
!            CUMRESE(IEL) = CUMRESE(IEL) + RESE(L,IEL)
            IF (RESE(L,IEL) > 0.) RESCE(L,IEL) = RESC(L) / RESE(L,IEL)
          ENDDO

!         Lignin concentration of root residue.
          IF (HARVRES % ResWT(L) > 0) THEN
            FRLRES(L) = HARVRES % ResLig(L) / HARVRES % ResWT(L)
          ENDIF
        ENDIF   !End of IF block on RUN .EQ. 1 .OR. ....


!       If L is transferred from SOILNI via the subroutine's parameter
!       string, it goes wrong, because L is the DO loop counter.
!       Therefore, L is first copied to LAYER.
        LAYER = L

        CALL PARTIT_C (
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRLRES,     !Input
     &    FRMETI, FRMETS, LAYER, RESC,                    !Input
     &    RESCE, RESDAX, SNH4,                            !Input
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,         !Input/Output
     &    DLTSNO3, DLTSTRUCC, DLTSTRUCE,                  !Input/Output
     &    ADDMETABEFLAG, FRMETFLAG)                       !Output
      ENDDO   !End of soil layer loop.

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

      RETURN
      END   !Subroutine SOILNI_C
