!=======================================================================
!  CENTURY, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Determines N transformations
!-----------------------------------------------------------------------
!  Revision       History
!  . . . . . .    Written
!  08/08/1992 WTB Modified for version 3 input/output (matches LeGRO).
!  02/08/1993 PWW Header revision and minor changes. 
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!               a new SOM module based on the CENTURY model.
!               Also changed variable names:
!               OLD      NEW                OLD      NEW
!               ------   ------             ------   ------ 
!               A        NITRIFLIMIT        RHMIN    HUMFRAC * HUMN
!               AINO3    TNO3               RMET     RESMET
!               ANH4     TNH4               RNAC     IMMOB
!               ANO3     TNO3               RNKG     TRTRESE
!               B2       SNH4_AVAIL         RNTRF    NITRIF
!               BB       NITRIFFRAC,NITRIF  RP2      NITRIFCAP
!               CNI      NITRIFCAPY         SANC     NITRIFCAPNH4
!               DNRATE   DENITRIF           SARNC    NITRIFCAPNH4Y
!               ELNC     TODAYINDEX         SWF      WFUREA
!               FERCOD   FERMET             TF       TFNIT,TFSOM,TFUREA
!               FT       TFDENIT            TFY      TFNITY
!               FW       WFDENIT            TIFOM    TFOM
!               G1       DECFACT            TIFON    TFON
!               G1 * X   FOMFRAC            TOTIN    SNH4NO3(array!)
!               GRCOM    FOMFRAC * FOM      TSIN     TNH4NO3
!               GRNOM    FOMFRAC * FON      TSOC     THUMC
!               HUM      HUMC               TSON     THUMN
!               IFTYPE   FERTYP             WFD      WFNIT
!               ISFLAG   NSOURCE            WFY      WFNITY
!               INH4     TNH4               X        FOMFRAC
!               MF       WFSOM              XT       TFNITLAG
!               NHUM     HUMN               XW       WFNITLAG
!               RESCOD   RESTYP
!  06/21/1999 CHP Modular format. 
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  04/20/2002 GH  Adjusted for crop rotations
!  04/25/2002 AJG Lots of small changes and linked SoilCBal.
!  11/11/2002 AJG Added DLAYR to LITDEC_C and SOMDEC_C.
!  07/17/2003 CHP Added call to TILLAGE for tillage data - removed
!                   direct reads of tillage info from FILEIO
!                 Added some initialization statements
!  11/14/2003 CHP Re-set value of DISTURBNUM for sequenced runs.
!  11/18/2003 AJG/UPS Corrected and added explanation on CW and DENITRIF.
!  10/18/2005 CHP Modified SSOMC and SSOME to keep surface component.
!-----------------------------------------------------------------------
!  Called: SOIL
!  Calls : DECRAT_C, FPLACE_C, IMMOBLIMIT_C, INCORPOR_C, 
!          LITDEC_C, NCHECK_C, NFLUX, NUPDATE_C, OPSOMLIT_C, RPLACE_C, 
!          SOILCBAL_C, SOILNI_C, SOMDEC_C, SOMLITPRINT_C, TSOMLIT_C,
!          YR_DOY
!=======================================================================
      SUBROUTINE Century (CONTROL, ISWITCH, 
     &    DRN, EO, FLOW, HARVRES, NSTRES, SENESCE,        !Input
     &    SOILPROP, SRFTEMP, ST, SW, UNH4, UNO3, WINF,    !Input
     &    YREND, YRPLT,                                   !Input
     &    NH4, NO3)                                       !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      INTEGER DISTURBENDMAX, DISTURBNUM, DOY, DYNAMIC,
     &  FERTYPE, YREND, I,
     &  IEL, INCYD, IUOF, L, LAYER, LAYER1,
     &  LIG, N, NAPNIT, NFERT,
     &  NLAYR, NONLIG, NSOURCE, NTIL,
     &  RUN, SOIL, SRFC,
     &  YEAR, YRDOY, YRPLT, YRDIF
      PARAMETER (SRFC = 0, SOIL = 1)
      PARAMETER (NONLIG = 1, LIG = 2)
      PARAMETER (N = 1)

      INTEGER DISTURBEND(NAPPL*3), FDAY(NAPPL), FERTYP(NAPPL), 
     &  TILLDATE(NAPPL)
      INTEGER DEND(NAPPL*3), DNUM
      REAL DDEP(NAPPL*3)  

      REAL AD, AK, AMTNIT, CO2S2, CO2S3, CULMETQ, CULS1Q,
     &  CULS2Q, CULS3Q, CULSTRQ, CUMRES, CW, DENITRIF,
     &  DEPTH, DISTURBDEPTHMAX, DLAG, DMOD,
     &  DSNC, EO, FERDEPTH, FERMIXPERC, FRMETI, FRMETS,
     &  NITRIF, NITRIFCAP, NITRIFCAPNH4, NITRIFCAPNH4Y,
     &  NITRIFFRAC, NITRIFLIMIT, NSTRES, RESDAX,
     &  RESDEPTH, RESMIXPERC, SENESSUMC, SENESSUMN, 
     &  SNH4_AVAIL, SNO3_AVAIL, SRFTEMP, SWEF, TFDENIT, TFNIT,
     &  TFNITLAG, TFUREA, TILLDEPTH, TILLMIXPERC, TLCH, TLITC,
     &  TIMMOBILIZE, TMETABC, TMINERALIZE, TNITRIFY, TSOM1C, TSOM2C,
     &  TSOM3C, TSOMC, TSTRUCC, TNH4, TNH4NO3,
     &  TNO3, TNOX, TODAYINDEX, UHYDR, WFDENIT, WFNIT, WFNITLAG,
     &  WFSOM, WFUREA, WINF, XL, XMIN, WTNUP

      REAL ACCCO2(0:1), ADCOEF(NL), ANFER(NAPPL), BD(NL),
     &  CEDAM(3), CESTR(3), CFMETS1(0:NL), CFS1S2(0:NL),
     &  CFS1S3(NL), CFS2S1(NL), CFS2S3(NL), CFS3S1(NL),
     &  CFSTRS1(0:NL), CFSTRS2(0:NL), CLAY(NL),
     &  CO2FMET(0:NL), CO2FS1(0:NL), CO2FS2(NL), CO2FS3(NL),
     &  CO2MET(0:1), CO2S1(0:NL), CUMRESE(3), DECMET(0:1),
     &  DECS1(0:1), DECS2(1), DECS3(1), DECSTR(0:1),
     &  DEFAC(0:NL), DISTURBDEPTH(NAPPL*3), DLAYR(NL),DLTLIGC(0:NL),
     &  DLTMETABC(0:NL), DLTSNH4(NL), DLTSNO3(NL),
     &  DLTSOM1C(0:NL), DLTSOM2C(NL), DLTSOM3C(NL), DLTSTRUCC(0:NL),
     &  DLTUREA(NL), DRN(NL), DUL(NL), FAC(NL), FLOW(NL), FRDAE(3),
     &  FRLSTR(0:NL), HARVRESE(3), LIGC(0:NL), LIGSTR(0:1), LITC(0:NL),
     &  LL(NL), METABC(0:NL), NH4(NL), NITRIFCAPY(NL),
     &  NO3(NL), PH(NL), PHN(NL), RESECONC(3), 
     &  S1S3(NL), S2S3(NL), SAT(NL), SILT(NL), SNH4(NL),
     &  SNO3(NL), SOM1C(0:NL), SOM2C(NL), SOM3C(NL), SSOMC(0:NL), 
     &  ST(NL), STRUCC(0:NL), SW(NL), TFNITY(NL), TILLDEP(NAPPL), 
     &  TILLMIX(NAPPL), TLITE(3), TMETABE(3),  
     &  TSOM1E(3), TSOM2E(3), TSOM3E(3), TSOME(3), TSTRUCE(3), TXS1(NL), 
     &  UNH4(NL), UNO3(NL), UPPM(NL), UREA(NL), WFNITY(NL), WRN(NL)

      REAL ACCMNR(0:NL,3), AMINRL(NL,3),
     &  CES1(0:NL,3), CES1M(0:1,3), CES1T(0:1,3),
     &  CES1X(0:1,3), CES2(NL,3), CES21I(0:0,3),
     &  CES21M(0:1,3), CES21S(0:1,3), CES21T(1,3),
     &  CES21X(0:1,3), CES2LI(0:1,3), CES2LM(0:1,3),
     &  CES2LS(0:1,3), CES2LX(0:1,3), CES3(NL,3),
     &  CES3M(1,3), CES3S(1,3), CES3T(1,3), CES3X(1,3),
     &  CO2FSTR(0:NL,2), CO2STR(0:1,2),
     &  DLTMETABE(0:NL,3), DLTSOM1E(0:NL,3),
     &  DLTSOM2E(NL,3), DLTSOM3E(NL,3), 
     &  DLTSTRUCE(0:NL,3), EFMETS1(0:NL,3), EFS1S2(0:NL,3),
     &  EFS1S3(NL,3), EFS2S1(NL,3), EFS2S3(NL,3),
     &  EFS3S1(NL,3), EFSTRS1(0:NL,3), EFSTRS2(0:NL,3),
     &  IMMMETS1(0:NL,3), IMMS1S2(0:NL,3), IMMS1S3(NL,3),
     &  IMMS2S1(NL,3), IMMS2S3(NL,3), IMMS3S1(NL,3),
     &  IMMSTRS1(0:NL,3), IMMSTRS2(0:NL,3),
     &  LITE(0:NL,3), METABE(0:NL,3), MINERALIZE(0:NL,3),
     &  MNRMETS1(0:NL,3), MNRS1S2(0:NL,3), MNRS1S3(NL,3),
     &  MNRS2S1(NL,3), MNRS2S3(NL,3), MNRS3S1(NL,3),
     &  MNRSTRS1(0:NL,3), MNRSTRS2(0:NL,3), SOM1E(0:NL,3),
     &  SOM2E(NL,3), SOM3E(NL,3), SSOME(0:NL,3), STRUCE(0:NL,3) 
      REAL SNI(NL) !CHP temp
      REAL SomLitC(0:NL), SomLitE(0:NL,NELEM)

      CHARACTER*1  IDETL, IFERI, IRESI, RNMODE, ISWTIL
      CHARACTER*2  CROP
      CHARACTER*5  FERMET(NAPPL)
      CHARACTER*6  ERRKEY

      PARAMETER (ERRKEY = 'NTRANS')

      LOGICAL ADDMETABEFLAG, CONTAINSN, DOINCORPOR, FRMETFLAG, IUON,
     &  PHFLAG
      LOGICAL DOCULT(0:NL), SOMFRACFLAG(NL)

      DATA CONTAINSN /.TRUE./ 
      DATA ADDMETABEFLAG /.FALSE./
      DATA FRMETFLAG /.FALSE./
      DATA PHFLAG /.FALSE./

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      Type (ResidueType) HARVRES
      TYPE (ResidueType) SENESCE  !Structure defined in ModuleDefs.for

!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDIF   = CONTROL % YRDIF
      YRDOY   = CONTROL % YRDOY

!     Check that this list is complete and that all are needed.
      ADCOEF = SOILPROP % ADCOEF 
      BD     = SOILPROP % BD     
      CLAY   = SOILPROP % CLAY    
      DLAYR  = SOILPROP % DLAYR  
      DMOD   = SOILPROP % DMOD    
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      PH     = SOILPROP % PH     
      SAT    = SOILPROP % SAT    
      SILT   = SOILPROP % SILT    
  
      IDETL  = ISWITCH % IDETL
      IRESI  = ISWITCH % IRESI
      IFERI  = ISWITCH % IFERI
      ISWTIL = ISWITCH % ISWTIL

!     ------------------------------------------------------------------
!     Split YRDOY into YR and DOY.
      CALL YR_DOY (YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION - CALLED ONCE PER SIMULATION
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!     ------------------------------------------------------------------

        CALL SOILNI_C (CONTROL, 
     &    IDETL, SOILPROP, ST, SW,                        !Input
     &    CUMRES, CUMRESE,                                !Input/output
     &    ADDMETABEFLAG, AMINRL, ACCCO2, ACCMNR, CEDAM,   !Output
     &    CES1, CES1M, CES1T, CES1X, CES2, CES21I,        !Output
     &    CES21M, CES21S, CES21T, CES21X, CES2LI, CES2LM, !Output
     &    CES2LS, CES2LX, CES3, CES3M, CES3T, CES3X,      !Output
     &    CESTR, CO2MET, CO2S1, CO2S2, CO2S3,             !Output
     &    CO2STR, CULMETQ, CULS1Q, CULS2Q, CULS3Q,        !Output
     &    CULSTRQ, DECMET, DECS1, DECS2, DECS3, DECSTR,   !Output
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,         !Output
     &    DLTSNO3, DLTSTRUCC, DLTSTRUCE, DSNC, FAC, FRDAE,!Output
     &    FRMETFLAG, FRMETI, FRMETS, HARVRES, IUOF, IUON, !Output
     &    LIGC, LIGSTR, LITC, LITE, METABC, METABE,       !Output
     &    NH4, NITRIFCAPY, NO3, PHFLAG, PHN, RESDAX,      !Output
     &    S1S3, S2S3, SNH4, SNO3, SOM1C, SOM1E, SOM2C,    !Output
     &    SOM2E, SOM3C, SOM3E, SOMFRACFLAG, SSOMC, SSOME, !Output
     &    STRUCC, STRUCE, TFNITY, TMETABC, TMETABE, TNH4, !Output
     &    TNO3, TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM3C,   !Output
     &    TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE, TXS1,   !Output
     &    UREA, WFNITY, WRN)                              !Output

        CALL FPLACE_C (CONTROL,
     &    DLAYR, NLAYR, NSTRES, YRPLT,                    !Input
     &    DISTURBDEPTH, DISTURBEND, DISTURBNUM,           !Input/Output
     &    DLTSNO3, DLTSNH4, DLTUREA,                      !Input/Output
     &    AMTNIT, ANFER, CONTAINSN, DOINCORPOR,           !Output
     &    FDAY, FERDEPTH, FERMET, FERMIXPERC,             !Output
     &    FERTYP, FERTYPE, IFERI, IUOF, IUON,             !Output
     &    NAPNIT, NFERT)                                  !Output

        CALL RPLACE_C (CONTROL, ISWITCH, 
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRMETI,     !Input
     &    FRMETS, HARVRES, NLAYR, RESDAX,                 !Input
     &    YRDIF, YRPLT,                                   !Input
     &    CUMRES, CUMRESE, DISTURBDEPTH, DISTURBEND,      !Input/Output
     &    DISTURBNUM, DLTLIGC, DLTMETABC, DLTMETABE,      !Input/Output
     &    DLTSNH4, DLTSNO3, DLTSTRUCC, DLTSTRUCE,         !Input/Output
     &    ADDMETABEFLAG, DOINCORPOR, FRMETFLAG,           !Output
     &    RESDEPTH, RESMIXPERC,  RESECONC)                !Output

        CALL OpSoilN_C(CONTROL, ISWITCH, 
     &    AMTNIT, NAPNIT, NH4, NO3, TLCH, TNH4, 
     &    TNH4NO3, TNO3, TSOME, 
     &    TNOX, TIMMOBILIZE, TMINERALIZE, TNITRIFY)

!     Called from SOILNI_C - probably should move call here - it would
!     be cleaner to call from the same place every time.
!        CALL OPSOMLIT_C (CONTROL, IDETL, 
!     &    ACCCO2, CUMRES, CUMRESE, LITC, LITE, METABC,        !Input
!     &    METABE, SOM1C, SOM1E, SOM2C, SOM2E, SOM3C,          !Input
!     &    SOM3E, SSOMC, SSOME, STRUCC, STRUCE, TLITC,         !Input
!     &    TLITE, TMETABC, TMETABE, TSOM1C, TSOM1E,            !Input
!     &    TSOM2C, TSOM2E, TSOM3C, TSOM3E, TSOMC, TSOME,       !Input
!     &    TSTRUCC, TSTRUCE)                                   !Input

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION: run once per season.
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!       Initialize.
        TNOX = 0.0
        TLCH = 0.0
        !Initialize uptake variables here, or they will have residual
        !  value on first day of multi-season runs.
        UNH4  = 0.0
        UNO3  = 0.0
        WTNUP = 0.0

        TMINERALIZE = 0.0
        TIMMOBILIZE = 0.0
        TNITRIFY    = 0.0

!       Initialize soil C and N process rates for this time step. First
!       the variables that exist for both SRFC and soil layers, then
!       those that exist only for the soil layers.
        DO L = 0, NLAYR   !With SRFC layer.
          DLTMETABC(L) = 0.0
          DLTSTRUCC(L) = 0.0
          DLTLIGC(L)   = 0.0
          DLTSOM1C(L)  = 0.0

          DO IEL = 1, NELEM
            DLTMETABE(L,IEL) = 0.0
            DLTSTRUCE(L,IEL) = 0.0
            DLTSOM1E(L,IEL)  = 0.0
          END DO

          IF (L .NE. SRFC) THEN
            DLTSNO3(L)  = 0.0
            DLTSNH4(L)  = 0.0
            DLTUREA(L)  = 0.0
            DLTSOM2C(L) = 0.0
            DLTSOM3C(L) = 0.0

            DO IEL = 1, NELEM
              DLTSOM2E(L,IEL)  = 0.0
              DLTSOM3E(L,IEL)  = 0.0
            ENDDO
          ENDIF   !End of IF block on L.NE.SRFC.
        ENDDO   !End of soil layer loop.

      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
!       Variables for setting the enhanced SOM/litter decomposition
!       rate after soil disturbance (DOCULT flag).
        DISTURBDEPTHMAX = 0.
        DISTURBNUM = 0
        DISTURBENDMAX = 0
        DISTURBEND = 0
        DO L = 1, NLAYR
          DOCULT(L) = .FALSE.
        ENDDO
      ELSE
!       For sequenced runs, need to purge the DISTURB arrays of old data
!       First copy data to temporary arrays, then go through and keep
!         only current dates.  This is needed because in long sequences 
!         the number of disturbances can get very large 
!         (potentially DISTURBNUM = # sequences * NAPPL * 3).
!       CHP 11/12/2003

        !Copy data to temporary arrays
        DNUM = DISTURBNUM
        DEND = DISTURBEND
        DDEP = DISTURBDEPTH

        !Zero out disturbance arrays
        DISTURBNUM = 0
        DISTURBEND = 0
        DISTURBDEPTH = 0.0

        !Copy back to disturbance arrays only current data
        DO I = 1, DNUM
          IF (DEND(I) .GT. YRDOY) THEN
            DISTURBNUM = DISTURBNUM + 1
            DISTURBEND(DISTURBNUM) = DEND(I)
            DISTURBDEPTH(DISTURBNUM) = DDEP(I)
          ENDIF
        ENDDO
      ENDIF

!       Set initial SOM and nitrogen conditions for each soil layer.
        CALL SOILNI_C (CONTROL, 
     &    IDETL, SOILPROP, ST, SW,                        !Input
     &    CUMRES, CUMRESE,                                !Input/output
     &    ADDMETABEFLAG, AMINRL, ACCCO2, ACCMNR, CEDAM,   !Output
     &    CES1, CES1M, CES1T, CES1X, CES2, CES21I,        !Output
     &    CES21M, CES21S, CES21T, CES21X, CES2LI, CES2LM, !Output
     &    CES2LS, CES2LX, CES3, CES3M, CES3T, CES3X,      !Output
     &    CESTR, CO2MET, CO2S1, CO2S2, CO2S3,             !Output
     &    CO2STR, CULMETQ, CULS1Q, CULS2Q, CULS3Q,        !Output
     &    CULSTRQ, DECMET, DECS1, DECS2, DECS3, DECSTR,   !Output
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,         !Output
     &    DLTSNO3, DLTSTRUCC, DLTSTRUCE, DSNC, FAC, FRDAE,!Output
     &    FRMETFLAG, FRMETI, FRMETS, HARVRES, IUOF, IUON, !Output
     &    LIGC, LIGSTR, LITC, LITE, METABC, METABE,       !Output
     &    NH4, NITRIFCAPY, NO3, PHFLAG, PHN, RESDAX,      !Output
     &    S1S3, S2S3, SNH4, SNO3, SOM1C, SOM1E, SOM2C,    !Output
     &    SOM2E, SOM3C, SOM3E, SOMFRACFLAG, SSOMC, SSOME, !Output
     &    STRUCC, STRUCE, TFNITY, TMETABC, TMETABE, TNH4, !Output
     &    TNO3, TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM3C,   !Output
     &    TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE, TXS1,   !Output
     &    UREA, WFNITY, WRN)                              !Output

!       Initialize the decomposition rate parameter, as a function
!       of the environmental conditions.
        CALL DECRAT_C (CONTROL, 
     &    BD, CLAY, DLAYR, DUL, EO, L, LL, NLAYR,         !Input
     &    SAT, SILT, SRFTEMP, ST, SW, WINF,               !Input
     &    DEFAC)                                          !Output

!       Set the fertilizer application dates.
        CALL FPLACE_C (CONTROL,
     &    DLAYR, NLAYR, NSTRES, YRPLT,                    !Input
     &    DISTURBDEPTH, DISTURBEND, DISTURBNUM,           !Input/Output
     &    DLTSNO3, DLTSNH4, DLTUREA,                      !Input/Output
     &    AMTNIT, ANFER, CONTAINSN, DOINCORPOR,           !Output
     &    FDAY, FERDEPTH, FERMET, FERMIXPERC,             !Output
     &    FERTYP, FERTYPE, IFERI, IUOF, IUON,             !Output
     &    NAPNIT, NFERT)                                  !Output

!       Add initial or carry-over shoot residue.
        CALL RPLACE_C (CONTROL, ISWITCH, 
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRMETI,     !Input
     &    FRMETS, HARVRES, NLAYR, RESDAX,                 !Input
     &    YRDIF, YRPLT,                                   !Input
     &    CUMRES, CUMRESE, DISTURBDEPTH, DISTURBEND,      !Input/Output
     &    DISTURBNUM, DLTLIGC, DLTMETABC, DLTMETABE,      !Input/Output
     &    DLTSNH4, DLTSNO3, DLTSTRUCC, DLTSTRUCE,         !Input/Output
     &    ADDMETABEFLAG, DOINCORPOR, FRMETFLAG,           !Output
     &    RESDEPTH, RESMIXPERC,  RESECONC)                !Output

        CALL SENESADD_C (SEASINIT,
     &    AMINRL, CEDAM, CESTR, CROP, DLAYR, FRDAE,       !Input
     &    FRMETI, FRMETS, NLAYR, RESDAX, SENESCE,         !Input
     &    SNH4,                                           !Input
     &    ADDMETABEFLAG, DLTLIGC, DLTMETABC,              !Output
     &    DLTMETABE, DLTSNH4, DLTSNO3, DLTSTRUCC,         !Output
     &    DLTSTRUCE, FRMETFLAG, SENESSUMC, SENESSUMN)     !Output

!       The SOM variables were initialized as state variables, but the
!       litter variables as rate variables (because of RPLACE call).
!       Integrate the rate variables for the initialization, so that
!       output on litter initialization can be obtained.
        TNO3 = 0.0
        TNH4 = 0.0
        DO L = 0, NLAYR
          METABC(L) = METABC(L) + DLTMETABC(L)
          STRUCC(L) = STRUCC(L) + DLTSTRUCC(L)
          !LIGC(L)   = LIGC(L)   + DLTLIGC(L)
          LIGC(L)   = AMIN1(LIGC(L) + DLTLIGC(L), STRUCC(L))

          DO IEL = 1, NELEM
            METABE(L,IEL) = METABE(L,IEL) + DLTMETABE(L,IEL)
            STRUCE(L,IEL) = STRUCE(L,IEL) + DLTSTRUCE(L,IEL) 
          END DO

          IF (L .NE. SRFC) THEN
            UREA(L) = UREA(L)  + DLTUREA(L)
            SNO3(L) = SNO3(L)  + DLTSNO3(L)
            SNH4(L) = SNH4(L)  + DLTSNH4(L)

!           Sum across the soil profile.
            TNH4    = TNH4 + SNH4(L)
            TNO3    = TNO3 + SNO3(L)

            DO IEL = 1, NELEM
              AMINRL(L,IEL) = SNH4(L) + SNO3(L)
              !To be done for P also.
            END DO
          ENDIF   !End of IF block on L .NE. SRFC.

!         Calculate FRLSTR from LIGC. (NOT FOR SEQUENCED RUNS!!)
          IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
            IF (STRUCC(L) .GT. 0.001) THEN
              FRLSTR(L) = LIGC(L) / STRUCC(L)
            ELSE
              FRLSTR(L) = 0.0
            ENDIF
          ENDIF
        END DO   !End of soil layer loop.

!       Total soil mineral N.
        TNH4NO3 = TNH4 + TNO3

!       Take the soil-profile sum of the SOM and litter variables.
        CALL TSOMLIT_C (
     &    METABC, METABE, NLAYR, SOM1C, SOM1E, SOM2C,     !Input
     &    SOM2E, SOM3C, SOM3E, STRUCC, STRUCE,            !Input
     &    LITC, LITE, SSOMC, SSOME, TLITC, TLITE,         !Output
     &    TMETABC, TMETABE, SomLitC, SomLitE,       !Output
     &    TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM3C,         !Output
     &    TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)         !Output

!       Get detailed SOM and litter output for checking the litter
!       initialization (SOM initialization was printed from SOILNI_C
        IF (IDETL .EQ. 'D') THEN
          CALL SOMLITPRINT_C (CONTROL,
     &      DLAYR, LITC, LITE, METABC, METABE, NLAYR,     !Input
     &      SOM1C, SOM1E, SOM2C, SOM2E, SOM3C, SOM3E,     !Input
     &      SSOMC, SSOME, STRUCC, STRUCE, TLITC, TLITE,   !Input
     &      TMETABC, TMETABE, TSOM1C, TSOM1E, TSOM2C,     !Input
     &      TSOM2E, TSOM3C, TSOM3E, TSOMC, TSOME,         !Input
     &      TSTRUCC, TSTRUCE)                             !Input
        ENDIF

!       Check for impossible SOM/residue values and other warnings.
        CALL NCHECK_C (CONTROL, 
     &  ADDMETABEFLAG, FRLSTR,                            !Input
     &  FRMETFLAG, IFERI, IRESI, METABC, METABE, NLAYR,   !Input
     &  PHFLAG, SNH4, SNO3, SOM1C, SOM1E, SOM2C, SOM2E,   !Input
     &  SOM3C, SOM3E, SOMFRACFLAG, STRUCC, STRUCE, UREA)  !Input

        SWEF = 0.9 - 0.00038 * (DLAYR(1) - 30.) ** 2.

        CALL OpSoilN_C(CONTROL, ISWITCH, 
     &    AMTNIT, NAPNIT, NH4, NO3, TLCH, TNH4, 
     &    TNH4NO3, TNO3, TSOME, 
     &    TNOX, TIMMOBILIZE, TMINERALIZE, TNITRIFY)

        CALL OPSOILC_C(CONTROL, ISWITCH, 
     &    CUMRES, CUMRESE, DSNC, NLAYR, SomLitC, SomLitE, 
     &    SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)

!       Soil carbon balance.
        CALL SOILCBAL_C (CONTROL, ISWITCH, 
     &    ACCCO2, CUMRES, LITC, SENESSUMC,                !Input
     &    SOM1C, TLITC, TSOMC, YRDOY)                     !Input

        CALL SoilNBal_C(CONTROL, ISWITCH,
     &    AMTNIT, CUMRESE, HARVRES, HARVRESE, LITE, NLAYR,!Input
     &    SENESSUMN, SOM1E, TLCH, TLITE, TNH4, TNO3,      !Input
     &    TNOX, TSOME, TSOM1E, TSOM2E, TSOM3E, WTNUP)     !Input

        IF (ISWTIL .EQ. 'Y') THEN
!         Get tillage info for this season
          CALL TILLAGE(CONTROL, NTIL, TILLDATE, TILLDEP, TILLMIX)
        ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
!       Set TILLDEPTH to zero at the start of a new day.
        TILLDEPTH = 0.

!       Initialize soil C and N process rates for this time step. First
!       the variables that exist for both SRFC and soil layers, then
!       those that exist only for the soil layers.
        DO L = 0, NLAYR   !With SRFC layer.
          DLTMETABC(L) = 0.0
          DLTSTRUCC(L) = 0.0
          DLTLIGC(L)   = 0.0
          DLTSOM1C(L)  = 0.0

          DO IEL = 1, NELEM
            DLTMETABE(L,IEL) = 0.0
            DLTSTRUCE(L,IEL) = 0.0
            DLTSOM1E(L,IEL)  = 0.0
          END DO

          IF (L .NE. SRFC) THEN
            DLTSNO3(L)  = 0.0
            DLTSNH4(L)  = 0.0
            DLTUREA(L)  = 0.0
            DLTSOM2C(L) = 0.0
            DLTSOM3C(L) = 0.0
            FAC(L) = 10.0/(BD(L)*DLAYR(L))    !recalculate daily

            DO IEL = 1, NELEM
              DLTSOM2E(L,IEL)  = 0.0
              DLTSOM3E(L,IEL)  = 0.0
            ENDDO
          ENDIF   !End of IF block on L.NE.SRFC.
        ENDDO   !End of soil layer loop.

        IF (INDEX ('ARD',IFERI) .GT. 0) THEN
          CALL FPLACE_C (CONTROL,
     &      DLAYR, NLAYR, NSTRES, YRPLT,                  !Input
     &      DISTURBDEPTH, DISTURBEND, DISTURBNUM,         !Input/Output
     &      DLTSNO3, DLTSNH4, DLTUREA,                    !Input/Output
     &      AMTNIT, ANFER, CONTAINSN, DOINCORPOR,         !Output
     &      FDAY, FERDEPTH, FERMET, FERMIXPERC,           !Output
     &      FERTYP, FERTYPE, IFERI, IUOF, IUON,           !Output
     &      NAPNIT, NFERT)                                !Output
        ENDIF

        IF (INDEX ('ARD',IRESI) .GT. 0) THEN
          CALL RPLACE_C (CONTROL, ISWITCH, 
     &      AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRMETI,   !Input
     &      FRMETS, HARVRES, NLAYR, RESDAX,               !Input
     &      YRDIF, YRPLT,                                 !Input
     &      CUMRES, CUMRESE, DISTURBDEPTH, DISTURBEND,    !Input/Output
     &      DISTURBNUM, DLTLIGC, DLTMETABC, DLTMETABE,    !Input/Output
     &      DLTSNH4, DLTSNO3, DLTSTRUCC, DLTSTRUCE,       !Input/Output
     &      ADDMETABEFLAG, DOINCORPOR, FRMETFLAG,         !Output
     &      RESDEPTH, RESMIXPERC,  RESECONC)              !Output
        ENDIF

!       Add the senesced plant material to the soil as residues.
        CALL SENESADD_C (RATE,
     &    AMINRL, CEDAM, CESTR, CROP, DLAYR, FRDAE,       !Input
     &    FRMETI, FRMETS, NLAYR, RESDAX, SENESCE, SNH4,   !Input
     &    ADDMETABEFLAG, DLTLIGC, DLTMETABC,              !Output
     &    DLTMETABE, DLTSNH4, DLTSNO3, DLTSTRUCC,         !Output
     &    DLTSTRUCE, FRMETFLAG, SENESSUMC, SENESSUMN)     !Output

!       If DOY=IUOF (has been set in FPLACE_C), then all the urea has
!       hydrolyzed already.
        IF (DOY .EQ. IUOF) THEN
          DO L = 1, NLAYR
            DLTSNH4(L) = DLTSNH4(L) + UREA(L)
            DLTUREA(L) = DLTUREA(L) - UREA(L)
          END DO
          IUON = .FALSE.
        ENDIF

!       ----------------------------------------------------------------
!       Loop through soil layers for rate calculations
!       ----------------------------------------------------------------
        DO L = 0, NLAYR   !With SRFC layer.

          IF (L .NE. SRFC) THEN
!           ------------------------------------------------------------
!           Environmental limitation factors for the soil processes.
!           ------------------------------------------------------------
            IF (SW(L) .LE. DUL(L)) THEN
              IF (L .EQ. 1) THEN
!               For the topsoil layer, the lowest possible volumetric
!               water content (air-dry water content) may be lower than
!               the lower limit, because this layer may dry out due to
!               water evaporation at the soil surface.
                AD = LL(L) * SWEF
              ELSE
!               Set the air dry water content equal to the lower limit.
                AD  = LL(L)
              ENDIF   !End of IF block on L=1

!             Soil water factors WFSOM and WFNIT. The CENTURY-based SOM
!             module does not use WFSOM (because it calculates it
!             differently in subroutine DECRAT), but needs it for
!             calculating some other variables.
              WFSOM = (SW(L) - AD) / (DUL(L) - AD)
              WFNIT = WFSOM

            ELSE   !If SW(L) .GT. DUL(L)
!             If the soil water content is higher than the drained upper 
!             limit (field capacity), calculate the excess water as
!             fraction of the maximum excess (i.e. when saturated).
              XL = (SW(L) - DUL(L)) / (SAT(L) - DUL(L))

!             Soil water factors WFSOM and WFNIT. The CENTURY-based SOM
!             module does not use WFSOM (because it calculates it
!             differently in subroutine DECRAT), but needs it for
!             calculating some other variables.
              WFSOM = 1.0 - 0.5 * XL
              WFNIT = 1.0 - XL
            ENDIF   !End of IF block on SW vs. DUL.

!           Limit the soil water factors between 0 and 1.
            WFSOM = AMAX1 (AMIN1 (WFSOM, 1.), 0.)
            WFNIT = AMAX1 (AMIN1 (WFNIT, 1.), 0.)

!           Calculate the soil temperature factor for the nitrification.
            TFNIT = (ST(L) - 5.) / 30.
            TFNIT = AMAX1 (AMIN1 (TFNIT, 1.), 0.)
            IF (ST(L) .LT. 5.) TFNIT = 0.

!           Calculate the soil water factor for the urea hydrolysis, and
!           limit it between 0 and 1.
            WFUREA = WFSOM + 0.20
            WFUREA = AMAX1 (AMIN1 (WFUREA, 1.), 0.)

!           Calculate the soil temperature factor for the urea
!           hydrolysis.
            TFUREA = (ST(L) / 40.) + 0.20
            TFUREA = AMAX1 (AMIN1 (TFUREA, 1.), 0.)

!           Water factor for denitrification: only if SW > DUL.
            WFDENIT = 1. - (SAT(L) - SW(L)) / (SAT(L) - DUL(L))
            WFDENIT = AMAX1 (AMIN1 (WFDENIT, 1.), 0.)

!           Temperature factor for denitrification.
            TFDENIT = 0.1 * EXP (0.046 * ST(L))
            TFDENIT = AMAX1 (AMIN1 (TFDENIT, 1.), 0.)

!           ------------------------------------------------------------
!           UREA hydrolysis
!           ------------------------------------------------------------
            IF (IUON) THEN
!             Calculate the maximum hydrolysis rate of urea.
              AK = -1.12 + 1.31 * SSOMC(L) * 1.E-4 * FAC(L) + 0.203 *
     &          PH(L) - 0.155 * SSOMC(L)  * 1.E-4 * FAC(L) * PH(L)
              AK = AMAX1 (AK, 0.25)

!             Calculate the amount of urea that hydrolyses.
              UHYDR = AK * AMIN1 (WFUREA, TFUREA) * UREA(L)
              IF (UHYDR .GT. UREA(L)) THEN
                UHYDR = UREA(L)
              ENDIF
              DLTUREA(L) = DLTUREA(L) - UHYDR
              DLTSNH4(L) = DLTSNH4(L) + UHYDR
            ENDIF   !End of IF block on IUON. 

!           ------------------------------------------------------------
!           Nitrification section.
!           ------------------------------------------------------------
!           Effect of ammonium availability on the nitrification
!           capacity index. 
            NITRIFCAPNH4 = 1.0 - EXP (-0.01363 * SNH4(L))

!           Today's environmental index that determines how yesterday's
!           nitrification capacity index affects today's nitrification
!           capacity index.
            TODAYINDEX = AMIN1 (TFNIT, WFNIT, NITRIFCAPNH4)

!           Nitrification capacity index based on the conditions during
!           the last two days (NITRIFCAPY was set yesterday).
            NITRIFCAP = NITRIFCAPY(L) * EXP (2.302 * TODAYINDEX)
            NITRIFCAP = AMAX1 (NITRIFCAP, 0.05)
            NITRIFCAP = AMIN1 (NITRIFCAP, 1.00)

!           The most limiting environmental index of the nitrification.
            NITRIFLIMIT = AMIN1 (NITRIFCAP, WFNIT, TFNIT, PHN(L))

!           The fraction of NH4 that will nitrify, using a Michaelis-
!           Menten function (see McLaren 1970).
            NITRIFFRAC = (NITRIFLIMIT * 40.0 * NH4(L) / (NH4(L) + 90.0))

!           Maximum 80% of ammonium is allowed to nitrify in one day.
            NITRIFFRAC = AMIN1 (NITRIFFRAC, 0.80)

!           Ensure that some ammonium is retained in the layer (set to
!           zero).
!           XMIN = 0.5 / FAC(L)
            XMIN = 0.

!           Make sure that no more NH4 is nitrified than there is, 
!           taking into account what has already been removed by other 
!           processes (thus use only negative DLTSNH4 values). This is a 
!           protection against negative values at the integration step.
            SNH4_AVAIL = SNH4(L) + AMIN1 (DLTSNH4(L), 0.) - XMIN

!           Do the nitrification.
            NITRIF = NITRIFFRAC * (SNH4_AVAIL - XMIN)
            NITRIF = AMIN1 (NITRIFFRAC * SNH4(L), SNH4_AVAIL)
!           NITRIF = AMAX1 (NITRIF, 0.)

!           Update the rate variables, reducing the NH4 pool by the
!           nitrification and adding it to the NO3 pool.
            DLTSNH4(L) = DLTSNH4(L) - NITRIF
            DLTSNO3(L) = DLTSNO3(L) + NITRIF
            TNITRIFY   = TNITRIFY   + NITRIF

!           Effect of ammonium availability on tomorrow's nitrification
!           capacity index.
!           Note: the original model used 0.1363 instead of 0.01363;
!           this was an error [AJG].
            NITRIFCAPNH4Y = 1.0 - EXP (-0.01363 * SNH4(L))

!           Lag phase in nitrification is based on the conditions of the
!           last two days. Compare today's water and temperature factor
!           with yesterday's values, and take the least limiting, so as
!           to prevent that a single day of low temperature or water
!           stress would substantially reduce the nitrification.
            WFNITLAG = AMAX1 (WFNIT, WFNITY(L))
            TFNITLAG = AMAX1 (TFNIT, TFNITY(L))

!           Set the effect of today's nitrification capacity index on
!           tomorrow's nitrification capacity index.
            NITRIFCAPY(L) = NITRIFCAP * AMIN1 (WFNITLAG, TFNITLAG,
     &        NITRIFCAPNH4Y)
            NITRIFCAPY(L) = AMAX1 (NITRIFCAPY(L), 0.05)

!           Set the effect of today's water and temperature factor for
!           tomorrow.
            WFNITY(L) = WFNIT
            TFNITY(L) = TFNIT

!           ------------------------------------------------------------
!           Denitrification section
!           ------------------------------------------------------------
!           Denitrification only occurs if there is nitrate, SW > DUL
!           and soil temperature > 5.
            IF (NO3(L) .GT. 1.0 .AND. SW(L) .GT. DUL(L) .AND.
     &        ST(L) .GE. 5.0) THEN

!             Lag factor that can be used to reduce the denitrification
!             rate. Temporarily set to 1.
              DLAG = 1.0

!             Water extractable soil carbon: estimated according to
!             Rolston et al. 1980, as cited in Godwin & Jones 1991 (ASA
!             monograph #31): CW = 24.5 + available carbon from SOM +
!             fresh C from carbohydrates. For the CENTURY-based module,
!             the carbohydrate C pool is calculated as 20% of the litter C.
!             NB: DSSAT3.5 has a factor 0.4 for converting FPOOL to carbon
!             units, but this is not needed here, as LITC has those units.
              !CW = 24.5 + (0.0031 * SSOMC(L) + 0.2 * LITC(L)) * FAC(L)

!     ----------------------------------------------------------------
              !CHP changed 1/14/2004 per email from UPS / AJG
              CW = 24.5 + 0.0031 * (SSOMC(L) + 0.2 * LITC(L)) * FAC(L)
!     ----------------------------------------------------------------

!             Denitrification rate. The difference of 1.E-5 vs. 1.E-4 and
!             the missing BD and DLAYR are due to a division by FAC.
              DENITRIF = 6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * 
     &          TFDENIT * DLAG / FAC(L)
              DENITRIF = AMAX1 (DENITRIF, 0.0)

!             The minimum amount of NO3 that stays behind in the soil
!             and cannot denitrify is XMIN.
!             XMIN    = 0.25 / FAC(L)
              XMIN    = 0.      !AJG

!             Check that no more NO3 denitrifies than there is, taking
!             into account what has already been removed by other
!             processes (thus use only negative DLTSNO3 values). This is
!             a protection against negative values during integration.
              SNO3_AVAIL = SNO3(L) + AMIN1 (DLTSNO3(L), 0.) - XMIN

!             Take the minimum of the calculated denitrification and the
!             amount of NO3 available for denitrification. 
              DENITRIF  = AMIN1 (DENITRIF, SNO3_AVAIL)

!             Reduce soil NO3 by the amount denitrified and add this to
!             the NOx pool
              DLTSNO3(L) = DLTSNO3(L) - DENITRIF
              TNOX       = TNOX       + DENITRIF
            ENDIF   !End of IF block on denitrification.
          ENDIF   !End of IF block on L .NE. SRFC.

!         --------------------------------------------------------------
!         Litter + SOM decay
!         --------------------------------------------------------------
!         If L is transferred from NTRANS via the subroutine's parameter
!         string, it goes wrong, because L is the DO loop counter.
!         Therefore, L is first copied to LAYER.
          LAYER = L

!         Calculate the decomposition rate parameter, as a function
!         of the environmental conditions.
          CALL DECRAT_C (CONTROL, 
     &      BD, CLAY, DLAYR, DUL, EO, L, LL, NLAYR,       !Input
     &      SAT, SILT, SRFTEMP, ST, SW, WINF,             !Input
     &      DEFAC)                                        !Output

!         Though AMINRL is in the subroutine parameter string, the SRFC
!         layer does not use AMINRL in LITDEC and SOMDEC. Thus there is
!         no need to accommodate the fact that the SRFC layer does not
!         have mineral E and uses the AMINRL of soil layer 1.

!         Do the litter decomposition with the CENTURY-based litter
!         decomposition model.
          CALL LITDEC_C (
     &      AMINRL, CES1, CES1M, CES1T, CES1X,            !Input
     &      CES2LI, CES2LM, CES2LS, CES2LX,               !Input
     &      CO2MET, CO2STR, CULMETQ, CULSTRQ,             !Input
     &      DECMET, DECSTR, DEFAC, DLAYR, DOCULT,         !Input
     &      FRLSTR, L, LIGSTR, METABC, METABE,            !Input
     &      STRUCC, STRUCE,                               !Input
     &      CFMETS1, CFSTRS1, CFSTRS2, CO2FMET,           !Output
     &      CO2FSTR,EFMETS1, EFSTRS1, EFSTRS2,            !Output
     &      IMMMETS1, IMMSTRS1,IMMSTRS2, MNRMETS1,        !Output
     &      MNRSTRS1, MNRSTRS2,                           !Output
     &      RATE)                                         !Control

!         Do the SOM decomposition with the CENTURY-based SOM model.
          CALL SOMDEC_C (
     &      AMINRL, CES1, CES1M, CES1T, CES1X,            !Input
     &      CES21I, CES21M, CES21S, CES21T,               !Input
     &      CES21X, CES3, CES3M, CES3S, CES3T, CES3X,     !Input
     &      CO2S1, CO2S2, CO2S3, CULS1Q, CULS2Q,          !Input
     &      CULS3Q, DECS1, DECS2, DECS3, DEFAC, DLAYR,    !Input
     &      DMOD, DOCULT, L, S1S3, S2S3, SOM1C,           !Input
     &      SOM1E, SOM2C, SOM2E, SOM3C, SOM3E, TXS1,      !Input
     &      CFS1S2, CFS1S3, CFS2S1, CFS2S3, CFS3S1,       !Output
     &      CO2FS1, CO2FS2, CO2FS3, EFS1S2, EFS1S3,       !Output
     &      EFS2S1, EFS2S3, EFS3S1, IMMS1S2, IMMS1S3,     !Output
     &      IMMS2S1, IMMS2S3, IMMS3S1, MNRS1S2, MNRS1S3,  !Output
     &      MNRS2S1, MNRS2S3, MNRS3S1,                    !Output
     &      RATE)                                         !Control
        END DO   !End of soil layer loop

!       ----------------------------------------------------------------
!       Limit immobilization to the NH4+NO3 that is available.
!       ----------------------------------------------------------------
!       In LITDEC and SOMDEC, the decomposition was calculated under
!       unrestrained conditions concerning mineral E. This means that
!       the immobilization may take away more mineral E than there is
!       in a soil layer. The E immobilization thus has to be limited,
!       taking into account all other processes that affect mineral E
!       (thus, for N, that affect DLTSNH4 and DLTSNO3).
        CALL IMMOBLIMIT_C (
     &    AMINRL, CFMETS1, CFS1S2, CFS1S3, CFS2S1,        !Input
     &    CFS2S3, CFS3S1, CFSTRS1, CFSTRS2, CO2FMET,      !Input
     &    CO2FS1, CO2FS2, CO2FS3, CO2FSTR, DLTSNH4,       !Input
     &    DLTSNO3, EFMETS1, EFS1S2, EFS1S3, EFS2S1,       !Input
     &    EFS2S3, EFS3S1, EFSTRS1, EFSTRS2, IMMMETS1,     !Input
     &    IMMS1S2, IMMS1S3, IMMS2S1, IMMS2S3, IMMS3S1,    !Input
     &    IMMSTRS1, IMMSTRS2, LIGC, METABC, METABE,       !Input
     &    MNRMETS1, MNRS1S2, MNRS1S3, MNRS2S1,            !Input
     &    MNRS2S3, MNRS3S1, MNRSTRS1, MNRSTRS2, NLAYR,    !Input
     &    SOM1C, SOM1E, SOM2C, SOM2E, SOM3C, SOM3E,       !Input
     &    STRUCC, STRUCE,                                 !Input
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSOM1C,        !Input/Output
     &    DLTSOM1E, DLTSOM2C, DLTSOM2E, DLTSOM3C,         !Input/Output
     &    DLTSOM3E, DLTSTRUCC, DLTSTRUCE,                 !Input/Output
     &    MINERALIZE)                                     !Output

!       ----------------------------------------------------------------
!       Distribute mineralization and immobilization over NH4 and NO3.
!       ----------------------------------------------------------------
!Do for P also.
        DO L = 0, NLAYR
          IF (L .EQ. SRFC) THEN
!           Add the mineralization in the SRFC layer to layer 1, because
!           the SRFC layer does not have a mineral E pool. Don't modify
!           L, but create a new variable LAYER1.
            LAYER1 = 1

!           In NUPDATE, MINERALIZE is not an array => has to be
!           transferred as a scalar.
            CALL NUPDATE_C (
     &        LAYER1, SNH4,                               !Input
     &        DLTSNH4, DLTSNO3, MINERALIZE(SRFC,N),       !Input/Output
     &        TMINERALIZE, TIMMOBILIZE)                   !Output
          ELSE
            CALL NUPDATE_C (
     &        L, SNH4,                                    !Input
     &        DLTSNH4, DLTSNO3, MINERALIZE(L,N),          !Input/Output
     &        TMINERALIZE, TIMMOBILIZE)                   !Output
          ENDIF
        END DO   !End of soil layer loop

!       ----------------------------------------------------------------
!       Downward and upward N movement with the water flow.
!       ----------------------------------------------------------------
        IF (IUON) THEN
          NSOURCE = 1   !Urea.
          CALL NFLUX (
     &      ADCOEF, BD, DLAYR, DRN, DUL, FLOW, NLAYR,     !Input
     &      UREA, NSOURCE, SW,                            !Input
     &      DLTUREA, TLCH)                                !Output
        ENDIF

        NSOURCE = 2   !NO3.
        CALL NFLUX (
     &    ADCOEF, BD, DLAYR, DRN, DUL, FLOW, NLAYR,       !Input
     &    SNO3, NSOURCE, SW,                              !Input
     &    DLTSNO3, TLCH)                                  !Output

!       ----------------------------------------------------------------
!       Surface litter incorporation due to tillage or incorporation of 
!       residue or fertilizer.
!       ----------------------------------------------------------------
!       With tillage or incorporation of residue or fertilizer, the
!       surface litter that was already present will also be
!       incorporated. Because changes to SOM and litter pools have only
!       been calculated as rate variables (DLTxxx), the state variables
!       have not changed yet. So, incorporation of the original pools
!       can be done here.
        DO I = 1, NTIL
!         Determine whether tillage occurs and if so, set the depth
!         and incorporation percentage.
          IF (YRDOY .EQ. TILLDATE(I)) THEN
            TILLDEPTH   = TILLDEP(I)
            TILLMIXPERC = TILLMIX(I)
            DOINCORPOR  = .TRUE.

!           The decomposition rate is increased for 30 days (starting
!           tomorrow) as a result of soil disturbance. Set the start
!           and end date. Increase the soil-disturbance number by one.
            DISTURBNUM = DISTURBNUM + 1
            DISTURBEND(DISTURBNUM) = INCYD (YRDOY, 30)
            DISTURBDEPTH(DISTURBNUM) = TILLDEPTH 
          ENDIF
        END DO   !End of NTIL loop.

        IF (DOINCORPOR) THEN
!         Take the maximum of the soil- and surface-residue mixing
!         variables (depth and percentage).
          TILLDEPTH = AMAX1 (TILLDEPTH, RESDEPTH, FERDEPTH)
          TILLMIXPERC = AMAX1 (TILLMIXPERC, RESMIXPERC, FERMIXPERC)

          CALL INCORPOR_C (
     &      CFMETS1, CFS1S2, CFSTRS1, CFSTRS2, CO2FMET,   !Input
     &      CO2FSTR, CO2FS1, DLAYR, EFMETS1, EFS1S2,      !Input
     &      EFSTRS1, EFSTRS2, LIGC, IMMMETS1, IMMSTRS1,   !Input
     &      METABC, METABE, MNRMETS1, MNRSTRS1,           !Input
     &      MNRSTRS2, MNRS1S2, NLAYR, SOM1C,              !Input
     &      SOM1E, STRUCC, STRUCE, TILLDEPTH,             !Input
     &      TILLMIXPERC,                                  !Input
     &      DLTLIGC, DLTMETABC, DLTMETABE, DLTSOM1C,      !Output
     &      DLTSOM1E, DLTSTRUCC, DLTSTRUCE,               !Output
     &      RATE)                                         !Control

!         Set DOINCORPOR back to FALSE.
          DOINCORPOR = .FALSE.
        ENDIF   !End of IF block on DOINCORPOR.

!       Determine the maximum soil depth for which the decomposition-
!       rate enhancement holds.
        DO I = 1, DISTURBNUM
          IF (YRDOY .LE. DISTURBEND(I)) THEN
!           If the new tillage event starts before the decomposition-
!           rate enhancing effect of a previous tillage event is
!           terminated, then take the deepest soil-disturbance depth
!           of the two. 
            DISTURBDEPTHMAX = AMAX1 (DISTURBDEPTHMAX, DISTURBDEPTH(I))

!           Take the highest DISTURBEND. This is going to determine
!           until when the decomposition-rate enhancing effect holds.
            DISTURBENDMAX = MAX0 (DISTURBENDMAX, DISTURBEND(I))

          ELSEIF (YRDOY .GT. DISTURBEND(I) .AND.
     &      DISTURBEND(I) .NE. 0) THEN
!           If the decomposition-rate enhancing effect of a previous
!           tillage event is terminated, set the depth back to zero, so
!           that another - possibly shallower - event can take over.
!           DISTURBEND = 0 means that an empty array element is reached.
            DISTURBDEPTHMAX = 0.

          ELSEIF (DISTURBEND(I) .EQ. 0) THEN
!           If an empty DISTURBEND array element is reached, then
!           there are no further DISTURBDEPTH data anymore to compare,
!           so jump out of the DO loop.
            GOTO 300
          ENDIF   !End of IF block on DISTURBEND.
        END DO   !End of DISTURBNUM loop.

300     CONTINUE

!       ----------------------------------------------------------------
!       Decomposition-rate enhancement after soil disturbance.
!       ----------------------------------------------------------------
        DO L = 0, NLAYR
!         Set a flag that speeds up the decomposition rate for thirty
!         days, from tomorrow onwards. Only applies to the layers that
!         were disturbed.
          IF (YRDOY .LE. DISTURBENDMAX) THEN
            IF (L .EQ. SRFC) THEN
              DEPTH = 0.
              DOCULT(SRFC) = .TRUE.
            ELSE
              DEPTH = DEPTH + DLAYR(L)

              IF (DEPTH .LE. DISTURBDEPTHMAX) THEN
                DOCULT(L) = .TRUE.
              ELSE
                DOCULT(L) = .FALSE.
              ENDIF
            ENDIF   !End of IF block on soil layers.
          ELSE
              DOCULT(L) = .FALSE.
          ENDIF   !End of IF block on YRDOY .LE. DISTURBENDMAX
        END DO   !End of soil layer loop.

!       Check for impossible SOM/residue values and other warnings.
        CALL NCHECK_C (CONTROL, 
     &  ADDMETABEFLAG, FRLSTR,                            !Input
     &  FRMETFLAG, IFERI, IRESI, METABC, METABE, NLAYR,   !Input
     &  PHFLAG, SNH4, SNO3, SOM1C, SOM1E, SOM2C, SOM2E,   !Input
     &  SOM3C, SOM3E, SOMFRACFLAG, STRUCC, STRUCE, UREA)  !Input

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION
!     First limit immobilization to what is available in the soil.
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!       In SENESADD the senesced material is stored in a temporary
!       variable (KEEPSHOOTC, KEEPROOTC, etc.) until it reaches a
!       certain value. This is to prevent the model from transferring
!       every day a few milligrams. But at the end of the season
!       everything has to be transferred, including what is in these
!       temporary variables. That is done on day YREND. But YREND can
!       be set in the INTEGR section of AUTHAR, so add this last bit
!       here.
        IF (YRDOY .EQ. YREND) THEN
          CALL SENESADD_C (INTEGR,
     &      AMINRL, CEDAM, CESTR, CROP, DLAYR, FRDAE,     !Input
     &      FRMETI, FRMETS, NLAYR, RESDAX, SENESCE,       !Input
     &      SNH4,                                         !Input
     &      ADDMETABEFLAG, DLTLIGC, DLTMETABC,            !Output
     &      DLTMETABE, DLTSNH4, DLTSNO3, DLTSTRUCC,       !Output
     &    DLTSTRUCE, FRMETFLAG, SENESSUMC, SENESSUMN)     !Output
        ENDIF

!       Set to zero before summing up across the soil profile.
        TNH4 = 0.
        TNO3 = 0.

!       -----------------------------------------
!       Loop through soil layers for integration.
!       -----------------------------------------
!       Integrate soil C and E process rates for this time step. First
!       the variables that exist for both SRFC and soil layers, then
!       those that exist only for the soil layers.
        DO L = 0, NLAYR
          METABC(L) = METABC(L) + DLTMETABC(L)
          STRUCC(L) = STRUCC(L) + DLTSTRUCC(L)
          LIGC(L)   = LIGC(L)   + DLTLIGC(L)
          SOM1C(L)  = SOM1C(L)  + DLTSOM1C(L) 

          DO IEL = 1, NELEM
            METABE(L,IEL) = METABE(L,IEL) + DLTMETABE(L,IEL)
            STRUCE(L,IEL) = STRUCE(L,IEL) + DLTSTRUCE(L,IEL) 
            SOM1E(L,IEL)  = SOM1E(L,IEL)  + DLTSOM1E(L,IEL)
          END DO

          IF (L .NE. SRFC) THEN
            SOM2C(L) = SOM2C(L) + DLTSOM2C(L)
            SOM3C(L) = SOM3C(L) + DLTSOM3C(L)
            DO IEL = 1, NELEM
              SOM2E(L,IEL) = SOM2E(L,IEL) + DLTSOM2E(L,IEL)
              SOM3E(L,IEL) = SOM3E(L,IEL) + DLTSOM3E(L,IEL)
            END DO

            UREA(L)  = UREA(L)  + DLTUREA(L)

            SNO3(L)  = SNO3(L)  + DLTSNO3(L) - UNO3(L)
            SNH4(L)  = SNH4(L)  + DLTSNH4(L) - UNH4(L)
            SNI(L)   = SNO3(L) + SNH4(L)

!           Though UREA, SNO3, SNH4 should not go negative here, it
!           yet sometimes happens by 0.000001. This gives a crash.
!           So set them to zero.
            IF (UREA(L) .LT. 0.) UREA(L) = 0.
            IF (SNO3(L) .LT. 0.) SNO3(L) = 0.
            IF (SNH4(L) .LT. 0.) SNH4(L) = 0.

!           Conversions.
            DO IEL = 1, NELEM
              AMINRL(L,IEL) = SNH4(L) + SNO3(L)
              !To be done for P also.
            END DO

            NO3(L)  = SNO3(L) * FAC(L)
            NH4(L)  = SNH4(L) * FAC(L)
            UPPM(L) = UREA(L) * FAC(L)

!           Sum across the soil profile.
            TNH4    = TNH4 + SNH4(L)
            TNO3    = TNO3 + SNO3(L)
            WTNUP   = WTNUP + (UNO3(L) + UNH4(L)) / 10.    !g[N]/m2
          ENDIF   !End of IF block on L .NE. SRFC

!         Calculate FRLSTR from LIGC: only if new residue was added.
          IF (STRUCC(L) .GT. 0.0001) THEN
            FRLSTR(L) = LIGC(L) / STRUCC(L)
          ELSE
            FRLSTR(L) = 0.0
          ENDIF
        ENDDO   !End of soil layer loop.

!       ----------------------------------------
!       Accumulators for mineralization and CO2.
!       ----------------------------------------
        DO L = 0, NLAYR
          IF (L .EQ. SRFC) THEN
            DO IEL = 1, NELEM
!             Accumulate mineralized E of the whole simulation period.
              ACCMNR(SRFC,IEL) = ACCMNR(SRFC,IEL) + MINERALIZE(SRFC,IEL)
            END DO   !End of IEL loop

          ELSEIF (L .EQ. 1) THEN
            DO IEL = 1, NELEM
!             For layer 1, subtract the E mineralization by the SRFC
!             layer, because this E also adds to the topsoil layer but
!             is accumulated separately.
              ACCMNR(1,IEL) = ACCMNR(1,IEL) + MINERALIZE(1,IEL) -
     &          MINERALIZE(SRFC,IEL)
            END DO   !End of IEL loop

          ELSE
            DO IEL = 1, NELEM
              ACCMNR(L,IEL) = ACCMNR(L,IEL) + MINERALIZE (L,IEL)
            END DO   !End of IEL loop
          ENDIF

!         Accumulate CO2 of the whole simulation period.
          IF (L .EQ. SRFC) THEN
            ACCCO2(SRFC) = ACCCO2(SRFC) + CO2FMET(SRFC) +
     &        CO2FSTR(SRFC,LIG) + CO2FSTR(SRFC,NONLIG) + CO2FS1(SRFC)
          ELSE
            ACCCO2(SOIL) = ACCCO2(SOIL) + CO2FMET(L) + CO2FSTR(L,LIG) +
     &        CO2FSTR(L,NONLIG) + CO2FS1(L) + CO2FS2(L) + CO2FS3(L)
          ENDIF
        END DO   !End of soil layer loop.

!       Total soil mineral N.
        TNH4NO3 = TNH4 + TNO3

!       Update total amount of SOM and litter (all pools together).
        CALL TSOMLIT_C (
     &    METABC, METABE, NLAYR, SOM1C, SOM1E, SOM2C,     !Input
     &    SOM2E, SOM3C, SOM3E, STRUCC, STRUCE,            !Input
     &    LITC, LITE, SSOMC, SSOME, TLITC, TLITE,         !Output
     &    TMETABC, TMETABE, SomLitC, SomLitE,       !Output
     &    TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM3C,         !Output
     &    TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)         !Output

!       Check for impossible SOM/residue values and other warnings.
        CALL NCHECK_C (CONTROL, 
     &  ADDMETABEFLAG, FRLSTR,                            !Input
     &  FRMETFLAG, IFERI, IRESI, METABC, METABE, NLAYR,   !Input
     &  PHFLAG, SNH4, SNO3, SOM1C, SOM1E, SOM2C, SOM2E,   !Input
     &  SOM3C, SOM3E, SOMFRACFLAG, STRUCC, STRUCE, UREA)  !Input

!       Soil carbon balance.
        CALL SOILCBAL_C (CONTROL, ISWITCH, 
     &    ACCCO2, CUMRES, LITC, SENESSUMC,                !Input
     &    SOM1C, TLITC, TSOMC, YRDOY)                     !Input

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
!       Get detailed SOM and litter output.
        IF (IDETL .EQ. 'D') THEN
          CALL SOMLITPRINT_C (CONTROL,
     &      DLAYR, LITC, LITE, METABC, METABE, NLAYR,     !Input
     &      SOM1C, SOM1E, SOM2C, SOM2E, SOM3C, SOM3E,     !Input
     &      SSOMC, SSOME, STRUCC, STRUCE, TLITC, TLITE,   !Input
     &      TMETABC, TMETABE, TSOM1C, TSOM1E, TSOM2C,     !Input
     &      TSOM2E, TSOM3C, TSOM3E, TSOMC, TSOME,         !Input
     &      TSTRUCC, TSTRUCE)                             !Input
        ENDIF

        CALL OPSOMLIT_C (CONTROL, IDETL, 
     &  ACCCO2, CUMRES, CUMRESE, LITC, LITE, METABC,      !Input
     &  METABE, SOILPROP, SOM1C, SOM1E, SOM2C, SOM2E,     !Input
     &  SOM3C, SOM3E, SomLitC, SomLitE, STRUCC, STRUCE,   !Input
     &  TLITC, TLITE, TMETABC, TMETABE, TSOM1C,           !Input
     &  TSOM1E, TSOM2C, TSOM2E, TSOM3C, TSOM3E,           !Input
     &  TSOMC, TSOME, TSTRUCC, TSTRUCE)                   !Input

        CALL OpSoilN_C(CONTROL, ISWITCH, 
     &    AMTNIT, NAPNIT, NH4, NO3, TLCH, TNH4, 
     &    TNH4NO3, TNO3, TSOME, 
     &    TNOX, TIMMOBILIZE, TMINERALIZE, TNITRIFY)

        CALL OPSOILC_C(CONTROL, ISWITCH, 
     &    CUMRES, CUMRESE, DSNC, NLAYR, SomLitC, SomLitE, 
     &    SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)

        CALL SoilNBal_C(CONTROL, ISWITCH,
     &    AMTNIT, CUMRESE, HARVRES, HARVRESE, LITE, NLAYR,!Input
     &    SENESSUMN, SOM1E, TLCH, TLITE, TNH4, TNO3,      !Input
     &    TNOX, TSOME, TSOM1E, TSOM2E, TSOM3E, WTNUP)     !Input

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
!     ------------------------------------------------------------------
!       Close output files.
        IF (IDETL .EQ. 'D') THEN
          CALL SOMLITPRINT_C (CONTROL,
     &      DLAYR, LITC, LITE, METABC, METABE, NLAYR,     !Input
     &      SOM1C, SOM1E, SOM2C, SOM2E, SOM3C, SOM3E,     !Input
     &      SSOMC, SSOME, STRUCC, STRUCE, TLITC, TLITE,   !Input
     &      TMETABC, TMETABE, TSOM1C, TSOM1E, TSOM2C,     !Input
     &      TSOM2E, TSOM3C, TSOM3E, TSOMC, TSOME,         !Input
     &      TSTRUCC, TSTRUCE)                             !Input
        ENDIF

        CALL OPSOMLIT_C (CONTROL, IDETL, 
     &  ACCCO2, CUMRES, CUMRESE, LITC, LITE, METABC,      !Input
     &  METABE, SOILPROP, SOM1C, SOM1E, SOM2C, SOM2E,     !Input
     &  SOM3C, SOM3E, SomLitC, SomLitE, STRUCC, STRUCE,   !Input
     &  TLITC, TLITE, TMETABC, TMETABE, TSOM1C,           !Input
     &  TSOM1E, TSOM2C, TSOM2E, TSOM3C, TSOM3E,           !Input
     &  TSOMC, TSOME, TSTRUCC, TSTRUCE)                   !Input

        CALL OpSoilN_C(CONTROL, ISWITCH, 
     &    AMTNIT, NAPNIT, NH4, NO3, TLCH, TNH4, 
     &    TNH4NO3, TNO3, TSOME, 
     &    TNOX, TIMMOBILIZE, TMINERALIZE, TNITRIFY)

        CALL OPSOILC_C(CONTROL, ISWITCH, 
     &    CUMRES, CUMRESE, DSNC, NLAYR, SomLitC, SomLitE, 
     &    SSOMC, SSOME, TLITC, TLITE, TSOMC, TSOME)

!      IF (ISWNIT .EQ. 'Y' .AND. IDETN .EQ. 'Y') THEN
        CALL SoilNBal_C(CONTROL, ISWITCH,
     &    AMTNIT, CUMRESE, HARVRES, HARVRESE, LITE, NLAYR,!Input
     &    SENESSUMN, SOM1E, TLCH, TLITE, TNH4, TNO3,      !Input
     &    TNOX, TSOME, TSOM1E, TSOM2E, TSOM3E, WTNUP)     !Input
!      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END   SUBROUTINE Century

!***********************************************************************
! NTRANS Variables
!
! A           Index for the most limiting environmental factor of the 
!               nitrification  (range 0-1)
! AD          Lowest possible volumetric water content of a soil layer 
!               when it is air  dry. For the top soil layer AD may be 
!               lower than the lower limit LL, because this layer may 
!               dry out due to water evaporation at the soil surface.
!               (cm3[H2O] / cm3[soil]) 
! ADCOEF(L)   Anion adsorption coefficient for reduced anion (nitrate) 
!               flow in variable-charge soils (ADCOEF = 0 implies no 
!               anion retention)
! AK          Maximum hydrolysis rate of urea (i.e. proportion of urea 
!               that will hydrolyze in 1 day under optimum conditions). 
!               AK >= 0.25.
! AMTNIT      Cumulative amount of N in fertilizer applications
!              (kg[N]/ha)
! ANFER(I)    Amount of nitrogen in fertilizer applied in Ith
!               application (kg[N] / ha)
! BD(L)       Bulk density (g[soil] / cm3[soil])
! CUMRES      Cumulative amount of residue application (kg [res] / ha)
! CW          Water extractable SOM carbon (ug [C] / g[soil])
! DENITRIF    Denitrification rate (kg [N] / ha)
! DEPMAX      Maximum depth of reported soil layers (cm)
! DLAG        Factor which can be used to reduce denitrification rate 
!               (currently set to 1.0)
! DLAYR(L)    Soil thickness in layer L (cm)
! DLTSNH4(L)  Rate of change of ammonium content in soil layer L
!               (kg[N]/ha/d)
! DLTSNO3(L)  Rate of change of nitrate content in soil layer L
!               (kg[N]/ha/d)
! DLTUREA(L)  Rate of change of urea content in soil layer L
!               (kg[N]/ha/d)
! DMOD        Factor to adjust the mineralization rate RHMIN for certain  
!               atypical soils (range 0-1)
! DOY         Current day of simulation (d)
! DRN(L)      Drainage rate through soil layer L (cm/d)
! DSNC        Depth to which C and N are integrated across all soil 
!               layers for output in CARBON.OUT (cm) 
! DUL(L)      Volumetric soil water content at drained upper limit in 
!               layer L (cm3/cm3)
! FAC(L)      Conversion factor to switch from kg [N]/ha to
!               ug[N]/g[soil]
! FDAY(I)     Julian date for Ith fertilizer application (YYDDD)
! FERMET(I)   Fertilizer code for Ith application         
! FERTYP(I)   Type of fertilizer application for Ith application 
! FILEIO      Filename for INP file (e.g., IBSNAT35.INP)  
! FLOW(L)     Volume of water moving from layer by unsaturated flow: 
!               + = upward, - = downward (cm/d)
! IFERI       Fertilizer switch (A = automatic, R = on specified dates
!               (YYDDD format), D = on specified days (DDD format) 
! IRESI       Residue application method. A = Automatic application 
!               at a certain day after planting for multiple years;
!               N = No residue; R = On reported dates; D = As reported,
!               in DAP; F = Auto with fixed amounts
! IUOF        Critical Julian day when all urea is assumed to have
!              hydrolyzed (occurs 21 days after the urea application)(d)
! IUON        Flag indicating presence of urea (true or false)       
! LL(L)       Volumetric soil water content in layer L at lower limit 
!               (cm3/cm3)
! MULTI       Current simulation of multi-year simulation       
! NAPNIT      Current number of fertilizer applications applied.
! NFERT       Number of observed fertilizer applications        
! NH4(L)      Ammonium nitrate in soil layer L (µg[N] / g[soil])
! NITRIF Nitrification rate (kg [N] / ha)
! NITRIFCAP   Nitrification capacity index based on the conditions 
!               during the immediate past, i.e. last two days(range 0-1)
! NITRIFCAPY  Yesterday's day's nitrification potential, indicating
!             whether there may be a lag phase for nitrification to
!             proceed (range 0-1)
! NITRIFCAPNH4 Ammonium concentration factor for nitrification: effect
!             of supply of ammonium on nitrification capacity
!             (range 0-1)
! NITRIFCAPNH4Y Effect of supply of ammonium on the reduction of
!               nitrification capacity for the next day (range 0-1)
! NITRIFFRAC  Fraction of NH4 that will nitrify; also: Maximum 
!               nitrification rate (fraction or kg[N] / (ha - d)) 
! NL          Maximum number of soil layers       
! NLAYR       Number of soil layers               
! NNOM        Net N release from all SOM sources (kg [N] / ha)
! NO3(L)      Nitrate in soil layer L (ug [N] / g[soil])
! NRESAP      Number of observed residue applications              
! NSTRES      Nitrogen stress factor (1=no stress, 0=max stress)   
! OC(L)       Organic carbon content of layer (%)
! PH(L)       pH in soil layer L  
! PHN(L)      Factor for pH effect on nitrification rate (range 0-1) for 
!               soil layer L 
! RCN         C/N ratio of initial root residue           
! RESAMT      Residue carryover from previous crop for sequenced runs 
!               (kg[residue]/ha)
! RESTYP(I)   FileX code for residue type (not used yet)  
! RESDAY(I)   Date of residue application (YYDDD)
! RESIDUE(I)  Amount of residue applied (kg [residue] /ha)
! RESECONC    Nutrient concentration of the residue for current application (%)
! RESMET(I)   Residue incorporation method (Not used) 
! SAT(L)      Volumetric soil water content in layer L at saturation
!             (cm3[H2O] / cm3[soil])
! SNH4(L)     Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNH4_AVAIL  Maximum amount of NH4 that may nitrify (kg [N] / (ha -d))
! SNO3(L)     Total extractable nitrate N in soil layer L (kg [N] / ha)
! ST(L)       Soil temperature by soil layer (oC)
! SW(L)       Today's Volumetric Soil Water Content in layer L (cm3/cm3)
! SWEF        Soil water evaporation fraction (fraction of lower limit 
!               to which evaporation can reduce soil water (fraction) 
! TNH4        Total extractable ammonium N in soil profile (kg [N] / ha)
! TNH4I       Total extractable ammonium N in soil profile at beginning 
!               of simulation (kg [N] / ha)
! TNO3        Total extractable nitrate N in soil profile (kg [N] / ha)
! TNO3I       Total extractable nitrate N in soil profile at beginning 
!               of the simulation (kg [N] / ha)
! TODAYINDEX  Environmental limit on nitrification capacity: depends on  
!               water, temperature and NH4 level (range 0-1)
! WFUREA      Reduction of urea hydrolysis due to soil water content (no 
!               reduction for SW = DUL, 20% reduction for SW = LL, 70% 
!               reduction for SW = SAT (fraction) 
! TFDENIT     Soil temperature factor for denitrification rate         
! TFNIT       Soil temperature factor for nitrification (range 0 - 1)
! TFNITY(L)   Yesterday's soil temperature factor for nitrification
!               (range 0-1)    
! TFSOM       Soil temperature factor for FOM decay (range 0 - 1)
! TFUREA      Soil temperature factor for urea hydrolysis (range 0 - 1)
! TLCH        Total N leached from soil (kg [N] / ha)
! TNOX        Denitrification across the total soil profile (SRFC not) 
!               adding to the nitrous oxide (NOx) pool of the air
!               (kg[N]/ha)  
! TNH4NO3     Total inorganic N in the whole profile (kg[N]/ha)
! TRTRES      Weight of root residues from previous crop
!               (kg[dry matter]/ha)
! TRTRESE(N)  N content of root residues from previous crop (=RTWTNL) 
!               (kg[N]/ha)
! UHYDR       Rate of urea hydrolysis (kg N / (ha - d))
! UNH4(L)     Rate of root uptake of NH4, computed in NUPTAK (kg[N]/ha)
! UNO3(L)     Rate of root uptake of NO3, computed in NUPTAK (kg[N]/ha)
! UPPM(L)     Urea concentration of a soil layer (ug [N] / g[soil])
! UREA(L)     Amount of urea in soil layer L (kg [N] / ha)
! WFDENIT     Soil water content factor for denitrification rate  
! WFNIT       Soil water factor for nitrification rate (range 0-1)             
! WFNITY(L)   Yesterday's soil water factor for nitrification rate
!               (range 0-1) 
! WFSOM       Reduction factor for FOM decay based on soil water content 
!               (no reduction for SW = DUL, 100% reduction for SW = LL, 
!               50% reduction for SW = SAT) (fraction) 
! XMIN        Minimum allowable NH4 or NO3 value in soil (kg [N] / ha)
! TFNITX      Temperature effect on nitrification capacity  
! WFNITX      Water effect on nitrification capacity        
! YEAR        Year of current date of simulation            
! YRDOY       Current day of simulation (YYDDD)
! YRPLT       Planting date (YYDDD)
!***********************************************************************
