C=======================================================================
C  COPYRIGHT 1998-2005 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  NTRANS, Subroutine
C
C  Determines N transformations
C-----------------------------------------------------------------------
C  Revision history
C  . . . . . .  Written
C  08-08-1992 WTB Modified for version 3 input/output (matches LeGRO).
C  02-08-1993 PWW Header revision and minor changes. 
C  06/09/1999 AJG Completely revised the soil N and SOM module, and made
C               a new SOM module based on the CENTURY model.
C               Also changed variable names:
C               OLD      NEW                OLD      NEW
C               ------   ------             ------   ------ 
C               A        NITLIM             RHMIN    HUMFRAC * HUMN     
C               AINO3    TNO3               RMET     RESMET
C               ANH4     TNH4               RNAC     IMMOB
C               ANO3     TNO3               RNKG     TRTRESN
C               B2       SNH4_AVAIL         RNTRF    NITRIF
C               BB       NITFRAC,NITRIF     RP2      NITCAP
C               CNI      NITCAPY            SANC     NITCAPA
C               DNRATE   DENITRIF           SARNC    NITCAPAY
C               ELNC     TDINDEX            SWF      WFUREA
C               FERCOD   FERMET             TF       TFNIT,TFSOM,TFUREA
C               FT       TFDENIT            TFY      TFNITY
C               FW       WFDENIT            TIFOM    TFOM
C               G1       DECFACT            TIFON    TFON
C               G1 * X   FOMFRAC            TOTIN    SNH4NO3(array!)
C               GRCOM    FOMFRAC * FOM      TSIN     TNH4NO3
C               GRNOM    FOMFRAC * FON      TSOC     THUMC
C               HUM      HUMC               TSON     THUMN
C               IFTYPE   FERTYP             WFD      WFNIT
C               INH4     TNH4               WFY      WFNITY
C               ISFLAG   NSOURCE            X        FOMFRAC
C               MF       WFSOM              XT       TFNITLAG
C               NHUM     HUMN               XW       WFNITLAG
C               RESCOD   RESTYP
C  06/21/1999 CHP Modular format.
C  03/29/2000 GH  Corrections for SARNC, CW, and DNRATE
C  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
C               modules with CHP's modular structure.
C  03/17/2000 GH  Incorporated in CROPGRO 
C  06/12/2002 UPS/CHP Added modifications for flooded conditions
C                   Added ammonia volatilization (OXLAYER) and replaced
C                     nitrification section based on Gilmour
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  11/18/2003 CHP Changed calculation of CW based on email from UPS
C  04/20/2004 US  Modified DLAG, removed IFDENIT
!  10/24/2005 CHP Put weather variables in constructed variable. 
C-----------------------------------------------------------------------
C  Called : CROPGRO
C  Calls  : FPLACE, IPSOIL, NCHECK, NFLUX, RPLACE,
C           SOILNI, YR_DOY
C=======================================================================

      SUBROUTINE NTRANS (CONTROL, ISWITCH, 
     &    DRN, ES, FLOODWAT, FLOW, HARVRES, NSTRES,       !Input
     &    SENESCE, SOILPROP, ST, SW, UNH4, UNO3,          !Input
     &    WEATHER, XHLAI, YRPLT,                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3)                                       !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE FloodModule
      IMPLICIT  NONE
      SAVE
!-----------------------------------------------------------------------
      LOGICAL IUON

      CHARACTER*1  IFERI, IRESI
      CHARACTER*5  FERMET(NAPPL)
      
      INTEGER DOY, DYNAMIC, IUOF, L
      INTEGER NAPNIT, NFERT, NLAYR
      INTEGER NSOURCE, YEAR, YRPLT   

      INTEGER FDAY(NAPPL)

      REAL AD, AK, AMTNIT, CNR, CNRF, CUMRES, CUMRESN, CUMSENN
      REAL CW, DECFACT, DENITRIF, DMINR, DMOD, DSNC    !REVISED-US
      REAL FOMFRAC, HUMFRAC, IMMOB, NFAC, NITRIF
      REAL NNOM
      REAL NSTRES, PRCEL, PRCHO, PRLIG, RCN, RDCEL, RDCHO, RDLIG
      REAL REDFACT, RESLEFT, RESLEFTN, RESNIT
      REAL SNH4_AVAIL, NI_AVAIL, SNO3_AVAIL 
      REAL SWEF, TFDENIT, TFNIT, TFOM, TFON, TFSOM, TFUREA
      REAL THUMC, THUMN, TLCH, TNH4, TNH4NO3, TNO3, TNOX
      REAL UHYDR      !, TOPRES, TOPRESN
      REAL WFDENIT, WFNIT, WFSOM, WFUREA, XL, XMIN
      REAL WTNUP, TUREA, ALGFIX
       
      REAL TFPOOL(3)
      REAL ANFER(NAPPL)

      REAL ADCOEF(NL), BD(NL), CNRAT(NL), DLAYR(NL) 
      REAL DLTFON(NL), DLTHUMC(NL), DLTHUMN(NL)
      REAL DLTSNH4(NL), DLTSNO3(NL), DLTUREA(NL), DRN(NL), DUL(NL)
      REAL FAC(NL), FLOW(NL), FOM(NL), FON(NL), HUMC(NL), HUMN(NL)
      REAL LL(NL), NH4(NL), SNH4NO3(NL), NITCAPY(NL), NO3(NL)
      REAL PH(NL), PHN(NL), SAT(NL), SNH4(NL), SNO3(NL), ST(NL), SW(NL)
      REAL TFNITY(NL), UNH4(NL), UNO3(NL), UPPM(NL), UREA(NL)
      REAL WFNITY(NL), SNI(NL)

      REAL DLTFPOOL(NL,3), FPOOL(NL,3)

      !Plant senesced matter(surface-soil, carbon-lignin-nitrogen)
      REAL SenWt(0:NL)        !kg[dry matter]/ha
      !REAL SenLig(0:NL)       !kg[lignin]/ha
      !REAL SenE(0:NL,NELEM)   !kg[E]/ha (E=N, P, S,...)
      REAL SENSUM

!     Variables added for flooded conditions analysis:
      TYPE (FloodWatType) FLOODWAT    
      TYPE (FloodNType)   FloodN        
      TYPE (OxLayerType)  OXLAYR   

      INTEGER NBUND, NSWITCH
      INTEGER FERTYPE  !This will come from FPLACE
      INTEGER DLAG(NL), LFD10  !REVISED-US
      REAL ALI, FLOOD, ES, TMAX, TMIN, SRAD, XHLAI
      REAL PHMIN, DMOD1, CMF, REQN, TKELVIN, TFACTOR, WFPL, WF2
      REAL PHFACT, T2, TLAG, ARNTRF   !, DNFRATE
      REAL CUMFNRO
      REAL BD1
      REAL TOTAML, TOTFLOODN

!      REAL GRNOM1, GRNOM2, DINT

      REAL TMINERALIZE    !Cumulative Mineralization 
      REAL TIMMOBILIZE    !Cumulative Immobilizatino 
      REAL TNITRIFY       !Cumulative Nitrification 

      LOGICAL DAILY

!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (SoilType)    SOILPROP
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC

      IFERI   = ISWITCH % IFERI
      IRESI   = ISWITCH % IRESI

      ADCOEF = SOILPROP % ADCOEF 
      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DMOD   = SOILPROP % DMOD    
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      PH     = SOILPROP % PH     
      SAT    = SOILPROP % SAT    

      NSWITCH = ISWITCH % NSWI

      NBUND = FLOODWAT % NBUND
      FLOOD = FLOODWAT % FLOOD

      SenWt  = SENESCE % ResWt
      !SenLig = SENESCE % ResLig
      !SenE   = SENESCE % ResE

      SRAD = WEATHER % SRAD
      TMAX = WEATHER % TMAX
      TMIN = WEATHER % TMIN

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!     ------------------------------------------------------------------

! To be done: SOILN980.SOL has several residue codes. So why here fix
! it at RE001? => change into RESTYPE
      CALL IPSOIL (
     &    CONTROL,'RE001',                                !Input
     &    DMINR, DSNC, PRCEL, PRCHO, PRLIG,               !Output
     &    RCN, RDCEL, RDCHO, RDLIG)                       !Output

      CALL FPLACE (CONTROL,
     &  DLAYR, FLOOD, NLAYR, NSTRES, YRPLT,               !Input
     &  DLTSNO3, DLTSNH4, DLTUREA, OXLAYR, FLOODN,        !Input/Output
     &  AMTNIT, ANFER, FDAY, FERMET, FERTYPE,             !Output
     &  IFERI, IUOF, IUON, LFD10, NAPNIT, NFERT)          !Output
     
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
        IUOF   = 0
        TNOX   = 0.0            !from INPLNT
        TLCH   = 0.0            !from INPLNT
        WTNUP  = 0.0
        CMF    = 1.0
        TFNITY = 0.0

        DO L = 1, NLAYR
          DLTSNO3(L)    = 0.0     
          DLTSNH4(L)    = 0.0      
          DLTUREA(L)    = 0.0
!         DLTFOM(L)     = 0.0
          DLTFON(L)     = 0.0
          DLTFPOOL(L,1) = 0.0
          DLTFPOOL(L,2) = 0.0
          DLTFPOOL(L,3) = 0.0
          DLTHUMC(L)    = 0.0
          DLTHUMN(L)    = 0.0
          DLAG(L)       = 0   !REVISED-US
          !Initialize uptake variables here, or they will have residual
          !  value on first day of multi-season runs.
          UNH4(L)       = 0.0
          UNO3(L)       = 0.0
        ENDDO

!      Set initial SOM and nitrogen conditions for each soil layer.
        CALL SOILNI(CONTROL, 
     &    HARVRES, PRCEL, PRCHO, PRLIG, RCN,              !Input
     &    SOILPROP, ST, SW,                               !Input
     &    CNRAT, FAC, FOM, FON, FPOOL, HUMC,              !Output
     &    HUMN, IUOF, IUON, NH4, NITCAPY, NO3,            !Output
     &    PHN, SNH4, SNO3, TFNITY, UREA, WFNITY)          !Output

!       Set the fertilizer application dates.
        CALL FPLACE (CONTROL,
     &    DLAYR, FLOOD, NLAYR, NSTRES, YRPLT,             !Input
     &    DLTSNO3, DLTSNH4, DLTUREA, OXLAYR, FLOODN,      !Input/Output
     &    AMTNIT, ANFER, FDAY, FERMET, FERTYPE,           !Output
     &    IFERI, IUOF, IUON, LFD10, NAPNIT, NFERT)        !Output

!       Add initial or carry-over shoot residue.
        CALL RPLACE(CONTROL, ISWITCH, 
     &    DLAYR, HARVRES, NLAYR, SENESCE, YRPLT,          !Input
     &    DLTFON, DLTFPOOL,                               !I/O
     &    CUMRES, CUMRESN, CUMSENN, DMINR, DSNC,          !Output
     &    PRCEL, PRCHO, PRLIG, RCN, RDCEL,                !Output
     &    RDCHO, RDLIG, RESLEFT, RESLEFTN, RESNIT)        !Output

        CALL NCHECK(CONTROL, 
     &    CNRAT, FOM, FON, FPOOL, HUMC, HUMN, IFERI,      !Input
     &    IRESI, NLAYR, RESNIT, SNH4, SNO3, UREA)         !Input

        SWEF = 0.9-0.00038*(DLAYR(1)-30.)**2

!     Initialize flooded N if flooding is a possibility.
      CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTFON, DLTSNH4, DLTSNO3, DLTUREA,FLOODN,OXLAYR,!I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output

!     Initialize TOTAML
      CALL OXLAYER(CONTROL,
     &    BD1, ES, FAC, FERTYPE, FLOODWAT, LFD10,         !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, DAILY, TOTAML)                             !Output

      TMINERALIZE = 0.0
      TIMMOBILIZE = 0.0
      TNITRIFY    = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
!     Initialize Soil N process rates for this time step.
      DO L = 1, NLAYR
        DLTSNO3(L) = -UNO3(L)  
        DLTSNH4(L) = -UNH4(L)  
        FAC(L) = 10.0/(BD(L)*DLAYR(L))    !recalculate daily
      ENDDO
      DLTUREA  = 0.0
      DLTFON   = 0.0
      DLTFPOOL = 0.0
      DLTFPOOL = 0.0
      DLTFPOOL = 0.0
      DLTHUMC  = 0.0
      DLTHUMN  = 0.0

!     Fertilizer placement
      IF (INDEX('ARD',IFERI) .GT. 0) THEN
        CALL FPLACE (CONTROL,
     &  DLAYR, FLOOD, NLAYR, NSTRES, YRPLT,               !Input
     &  DLTSNO3, DLTSNH4, DLTUREA, OXLAYR, FLOODN,        !Input/Output
     &  AMTNIT, ANFER, FDAY, FERMET, FERTYPE,             !Output
     &  IFERI, IUOF, IUON, LFD10, NAPNIT, NFERT)          !Output
      ENDIF

!     Call Residue Placement routine if residue records have
!       been read, or if plant senesced matter is present.
      SENSUM = SUM(SenWt)
      IF (INDEX('ARD',IRESI) .GT. 0 .OR. SENSUM .GT. 0.0) THEN
        CALL RPLACE (CONTROL, ISWITCH, 
     &    DLAYR, HARVRES, NLAYR, SENESCE, YRPLT,          !Input
     &    DLTFON, DLTFPOOL,                               !I/O
     &    CUMRES, CUMRESN, CUMSENN, DMINR, DSNC,          !Output
     &    PRCEL, PRCHO, PRLIG, RCN, RDCEL,                !Output
     &    RDCHO, RDLIG, RESLEFT, RESLEFTN, RESNIT)        !Output

        CALL NCHECK(CONTROL, 
     &    CNRAT, FOM, FON, FPOOL, HUMC, HUMN, IFERI,      !Input
     &    IRESI, NLAYR, RESNIT, SNH4, SNO3, UREA)         !Input
      ENDIF

!     Calculate rates of change of flood N components and interaction
!         with top soil layer.
      IF (NBUND > 0) THEN
        CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTFON, DLTSNH4, DLTSNO3, DLTUREA,FLOODN,OXLAYR,!I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
      ENDIF
      
!     This needs to be called from NTRANS for upland cases, too.
      IF (FLOOD .LE. 0.0) THEN
        CALL OXLAYER(CONTROL,
     &    BD1, ES, FAC, FERTYPE, FLOODWAT, LFD10,         !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, DAILY, TOTAML)                             !Output
      ENDIF

      DMOD1 = DMOD   !used for alt. value of DMOD

      IF (FERTYPE .EQ. 5) THEN !This condition was in RICE
!     ------------------------------------------------------------------
!       If DOY=IUOF (has been set in FPLACE), then all the urea has
!       hydrolyzed already.
        CALL YR_DOY (CONTROL%YRDOY, YEAR, DOY)
        IF (DOY .EQ. IUOF) THEN
          DO L = 1, NLAYR
            DLTSNH4(L) = DLTSNH4(L) + UREA(L)
            DLTUREA(L) = DLTUREA(L) - UREA(L)
          END DO
          IUON = .FALSE.
        ENDIF
      ENDIF

!     ------------------------------------------------------------------
!     Loop through soil layers for rate calculations
!     ------------------------------------------------------------------
      DO L = 1, NLAYR
!       ----------------------------------------------------------------
!       Environmental limitation factors for the soil processes.
!       ----------------------------------------------------------------
        IF (SW(L) .LE. DUL(L)) THEN
          IF (L .EQ. 1) THEN
!           For the topsoil layer, the lowest possible volumetric water
!           content (air-dry water content) may be lower than the lower
!           limit, because this layer may dry out due to water
!           evaporation at the soil surface.
            AD = LL(L) * SWEF
          ELSE
!           Set the air dry water content equal to the lower limit.
            AD  = LL(L)
          ENDIF

!         Soil water factors WFSOM and WFNIT.
          WFSOM = (SW(L) - AD) / (DUL(L) - AD)
          WFNIT = WFSOM
        ELSE
!         If the soil water content is higher than the drained upper 
!         limit (field capacity), calculate the excess water as fraction
!         of the maximum excess (i.e. when saturated).
          XL = (SW(L) - DUL(L)) / (SAT(L) - DUL(L))

!         Soil water factors WFSOM and WFNIT.
          WFSOM = 1.0 - 0.5 * XL
          WFNIT = 1.0 - XL
        ENDIF   !End of IF block on SW vs. DUL.

!       PH factor (from RICE model)
        IF (FLOOD .GT. 0.0) THEN
           WFSOM    = 0.75
           PHMIN = 1.00
         ELSE
           PHMIN = PH(L)/3.0 - 1.1
        ENDIF

!       Limit the soil water factors between 0 and 1.
        WFSOM = MAX (MIN (WFSOM, 1.), 0.)
        WFNIT = MAX (MIN (WFNIT, 1.), 0.)
        PHMIN = MAX (MIN (PHMIN, 1.), 0.)


        !IF (FERTYPE .EQ. 19) THEN
        !   CRF = 0.5
        !ELSE
        !  CRF = 1.0
        !ENDIF

!       Calculate the soil temperature factor for the SOM
!       decomposition model.
        TFSOM = (ST(L) - 5.) / 30.
        TFSOM = MAX (MIN (TFSOM, 1.), 0.)
        IF (ST(L) .LT. 5.) TFSOM = 0.

!       Calculate the soil temperature factor for the nitrification.
        TFNIT = TFSOM

!       Calculate the soil temperature factor for the urea hydrolysis.
        TFUREA = (ST(L) / 40.) + 0.20
        TFUREA = MAX (MIN (TFUREA, 1.), 0.)

!       Temperature factor for denitrification.
        TFDENIT = 0.1 * EXP (0.046 * ST(L))
        TFDENIT = MAX (MIN (TFDENIT, 1.), 0.)

!-----------------------------------------------------------------------
!       UREA hydrolysis
!-----------------------------------------------------------------------
        IF (IUON) THEN
!         Calculate the maximum hydrolysis rate of urea.
!         AK = -1.12 + 1.31 * OC(L) + 0.203 * PH(L) - 0.155 *
!    &         OC(L) * PH(L)
          AK = -1.12 + 1.31 * HUMC(L) * 1.E-4 * FAC(L) + 0.203 * PH(L)
     &              - 0.155 * HUMC(L) * 1.E-4 * FAC(L) * PH(L)
          AK = AMAX1 (AK, 0.25)

!         Calculate the soil water factor for the urea hydrolysis, and
!         limit it between 0 and 1.
          IF (FLOOD .GT. 0.0) THEN
             WFUREA = 1.0
          ELSE
            WFUREA = WFSOM + 0.20
            WFUREA = MAX (MIN (WFUREA, 1.), 0.)
          ENDIF

!         Calculate the amount of urea that hydrolyses.
          UHYDR = AK * MIN (WFUREA, TFUREA) * 
     &            (UREA(L)+DLTUREA(L))
          UHYDR = AMIN1 (UHYDR, UREA(L))

          DLTUREA(L) = DLTUREA(L) - UHYDR 
          DLTSNH4(L) = DLTSNH4(L)  + UHYDR 
        ENDIF   !End of IF block on IUON.

!-----------------------------------------------------------------------
!       FOM + SOM decay
!-----------------------------------------------------------------------
!       Total amount of soil extractable mineral N 
!       SNH4NO3(L) = SNO3(L) + SNH4(L) - 0.5 / FAC(L)
        SNH4NO3(L) = SNO3(L) + SNH4(L)   !No XMIN needed
        SNH4NO3(L) = AMAX1 (SNH4NO3(L), 0.0)

!       -----------------
!       FOM decomposition
!       -----------------
!       Only do FOM decomposition if there is anything to decompose.
        IF (FOM(L) .GE. 0.001) THEN

          CMF = WFSOM + 0.25                 !From RICE model
          CMF = MAX (MIN (CMF, 1.), 0.)

!         C/N ratio of the fresh organic residue in the soil, including
!         the mineral N that is present in the soil.
          CNR = (0.4 * FOM(L)) / (FON(L) + SNH4NO3(L))

!         C/N ratio factor with a critical C:N ratio of 25.
          CNRF = EXP (-0.693 * (CNR - 25.) / 25.0)
          CNRF = AMIN1 (CNRF, 1.0)

!         Decomposition factor as function of temperature, water and
!         C/N ratio.
          IF (FOM(L) .LT. 5.0) THEN
            DECFACT = 0.0
          ELSE
            DECFACT = TFSOM * WFSOM * CNRF
          ENDIF

!         FOM fraction that decomposes, summed across the three pools.
          FOMFRAC = DECFACT * (FPOOL(L,1) * RDCHO + 
     &      FPOOL(L,2) * RDCEL + FPOOL(L,3) * RDLIG) / FOM(L)

C         Add different switch point for immobln under flooded conditions
C         REQN = N Req. ---> To decompose 1g of FOM (GRCOM) will have to
C         recruit (NREQ-N CONC) g of N
          IF (FLOOD .GT. 0.0) THEN
            REQN = 0.01
          ELSE
            REQN = 0.02
          ENDIF

!         N immobilization by decomposing residue only occurs if its N%
!         (FON/FOM) is less than 2%. This value results from the carbon
!         conversion efficiency of microbes and their C:N ratio.
!          IMMOB = FOMFRAC * FOM(L) * (0.02 - FON(L) / FOM(L))
          IMMOB = FOMFRAC * FOM(L) * (REQN - FON(L) / FOM(L))
          IMMOB = AMAX1 (IMMOB, 0.0)

!         Limit the immobilization to the available mineral N in the
!         soil. Also take into account the NH4 and NO3 that was already
!         removed by other processes (thus only take the negative value
!         of DLTSNH4 and DLTSNO3).
!          NI_AVAIL = AMAX1(0.0, SNH4NO3(L) + MIN (DLTSNH4(L), 0.) +
!     &      MIN (DLTSNO3(L), 0.))
          NI_AVAIL = AMAX1(0.0, SNH4NO3(L) + DLTSNH4(L) + DLTSNO3(L))

!         Limit the FOM decomposition if there is not enough mineral N.
          IF (IMMOB .GT. NI_AVAIL) THEN
            REDFACT = NI_AVAIL / IMMOB
            IMMOB = NI_AVAIL
            FOMFRAC = FOMFRAC * REDFACT
            DECFACT = DECFACT * REDFACT
          ENDIF

!         The FOM fraction that decomposes. 
          DLTFPOOL(L,1) = DLTFPOOL(L,1) - FPOOL(L,1) * DECFACT * RDCHO
          DLTFPOOL(L,2) = DLTFPOOL(L,2) - FPOOL(L,2) * DECFACT * RDCEL
          DLTFPOOL(L,3) = DLTFPOOL(L,3) - FPOOL(L,3) * DECFACT * RDLIG
          DLTFON(L) = DLTFON(L) + IMMOB - FOMFRAC * FON(L)

        ELSE   !If there is no FOM.
          DECFACT = 0.0
          FOMFRAC = 0.0
          IMMOB   = 0.0
          CMF     = 1.0
        ENDIF   !End of IF block on FOM.

!       -------------------
!       Humus decomposition
!       -------------------
!       The humus fraction that decomposes only depends on the
!       environmental conditions and the decomposition constant; the C:N
!       ratio is not important.

!     FROM RICE:
!         RHMIN  = NHUM(L)*DMINR*TF*CMF*DMOD1*PHMIN
!             RHMIN -> HUMFRAC * HUMN
!             NHUM  -> HUMN
!             TF    -> TFNIT,TFSOM,TFUREA
!         HUM(L) = HUM(L)- RHMIN * CNRAT(L) + 0.2* GRNOM* 10.0
!             HUM   -> HUMC
!             GRNOM -> FOMFRAC * FON

!        HUMFRAC = DMINR * TFSOM * WFSOM * DMOD
        HUMFRAC = DMINR * TFSOM * CMF * DMOD1 * PHMIN

!     On 02/21/03 CHP removed the following code because it was
!         causing initialization problems, and may not be 
!         working correctly.
!     This doesn't make much sense. 
!     Only on first day will GRNOM2 have a value of 0, when L=1.  
!     Thereafter, GRNOM2 = GRNOM1 = GRNOM
!     Furthermore, DMOD1 is not used again until next layer.
!         IF (L .EQ. 1) THEN
!            GRNOM1 = FOMFRAC * FON(L)     !GRNOM
!          ELSEIF (L .EQ. 2) THEN
!            GRNOM2 = FOMFRAC * FON(L)     !GRNOM
!         ENDIF
!
!         IF (IMMOB .LE. 0.002 .AND. SNH4NO3(L) .LE. 5.0) THEN
!            IF (FLOOD .LE. 0) THEN
!              DINT = 0.3
!            ELSE
!              DINT  = 0.5
!            ENDIF
!            DMOD1 = DINT+(GRNOM1+GRNOM2)*0.65
!            DMOD1 = AMIN1 (DMOD1,DMOD)
!          ELSE
!            DMOD1 = DMOD
!         ENDIF

!       The reduction in the HUMN pool due to its decomposition is
!       calculated from the amount of N mineralized from the humus and
!       assuming that 20% of N from FON becomes stabilized as HUMN.
!       It takes with it an amount of C to give the new humus a C:N
!       ratio of 10. 
        DLTHUMC(L) = DLTHUMC(L) - HUMFRAC * HUMC(L) + 0.2 * FOMFRAC *
     &    FON(L) * 10.
        DLTHUMN(L) = DLTHUMN(L) - HUMFRAC * HUMN(L) + 0.2 * FOMFRAC * 
     &    FON(L)

!       Net N release from all SOM and FOM sources
        NNOM  = 0.8 * FOMFRAC * FON(L) + HUMFRAC * HUMN(L) - IMMOB

!       XMIN  = 0.5 / FAC(L)
        XMIN = 0.             !AJG

!-----------------------------------------------------------------------
!       Net mineralization rate.
!-----------------------------------------------------------------------
!       If the net N release from all SOM sources (NNOM) is positive,
!       add the mineralized N to the NH4 pool.
        IF (NNOM .GE. 0.0) THEN
          DLTSNH4(L) = DLTSNH4(L) + NNOM
          TMINERALIZE = TMINERALIZE + NNOM
          NNOM = 0.

        ELSE
!         If NNOM is < 0, there is immobilization. If the N demand
!         of the immobilization is greater than the amount of NH4
!         available, take all NH4, leaving behind a minimum amount of
!         NH4 equal to XMIN (NNOM is negative!).

          IF (ABS(NNOM) .GT. (SNH4(L) - XMIN)) THEN
            NNOM = NNOM + SNH4(L) - XMIN
            DLTSNH4(L) = DLTSNH4(L) + XMIN - SNH4(L)
            TIMMOBILIZE = TIMMOBILIZE + (SNH4(L) - XMIN)

            SNO3_AVAIL = SNO3(L) + DLTSNO3(L)
            IF (ABS(NNOM) .GT. (SNO3_AVAIL - XMIN)) THEN
              !Not enough SNO3 to fill remaining NNOM, leave residual
              NNOM = NNOM + SNO3_AVAIL - XMIN
              DLTSNO3(L) = DLTSNO3(L) + XMIN - SNO3_AVAIL
            ELSE
!             Get the remainder of the immobilization from nitrate (NNOM
!             is negative!)
              DLTSNO3(L) = DLTSNO3(L) + NNOM
              NNOM = 0.
            ENDIF

          ELSE
!           Reduce soil NH4 by the immobilization (NNOM is
!           negative!).
            DLTSNH4(L) = DLTSNH4(L) + NNOM
            TIMMOBILIZE = TIMMOBILIZE - NNOM
            NNOM = 0.0
          ENDIF   !End of IF block on ABS(NNOM).
        ENDIF   !End of IF block on NNOM.

!-----------------------------------------------------------------------
!       Nitrification section
!-----------------------------------------------------------------------
C        Nitrification section based on Gilmour
         TKELVIN = ST(L) + 273.0
         TFACTOR = EXP(-6572 / TKELVIN + 21.4)  !6602
!         WFPL    = WFP(L)    !not used
         WFPL = SW(L) / SAT(L)
         IF (SW(L) .GT. DUL(L)) THEN
           WF2 = -2.5 * WFPL + 2.55
         ELSE
           IF (WFPL .GT. 0.4) THEN
             WF2 = 1.0
           ELSE
             WF2 = 3.15 * WFPL - 0.1
           ENDIF
         ENDIF
         WF2 = AMAX1 (WF2, 0.0)
         WF2 = AMIN1 (1.0, WF2)

         PHFACT  = AMIN1 (1.0, 0.33 * PH(L) - 1.36)
!         NH4(L)  = SNH4(L)*FAC(L)

         IF (FLOOD .GT. 0.0) THEN
            PHFACT = 1.0
            WF2    = 0.0     
            TFNITY(L) = 0.0
         ENDIF

         T2   = AMAX1 (0.0,(TFNITY(L) - 1.0))
         TLAG = 0.075 * T2**2
         TLAG = AMIN1 (TLAG,1.0)
         IF (NBUND. LE. 0.) THEN
             TLAG = 1.0
         ENDIF
!         NITRIF = AMIN1(1.0, TFACTOR * WF2 * PHFACT * TLAG) * NH4(L)
         NFAC = AMAX1(0.0, AMIN1(1.0, TFACTOR * WF2 * PHFACT * TLAG))
         NITRIF = NFAC * NH4(L)

         IF (NSWITCH .EQ. 5) THEN
           ARNTRF = 0.0
         ELSE
           ARNTRF  = NITRIF * FAC(L)
         ENDIF

         IF (NH4(L).LE. 0.01) THEN
           TFNITY(L) = 0.0
         ELSE
           IF (SW(L) .LT. SAT(L)) THEN
             TFNITY(L) = TFNITY(L) + 1.0
           ENDIF
         ENDIF

!         XMIN = SMIN4(L)
         XMIN = 0.0
!         SNH4_AVAIL = SNH4(L) + DLTSNH4(L)
         SNH4_AVAIL = AMAX1(0.0, SNH4(L) + DLTSNH4(L) - XMIN)
         !SNH4_AVAIL = SNH4(L) + MIN (DLTSNH4(L), 0.) - UNH4(L) - XMIN
!         ARNTRF = MIN(ARNTRF, SNH4_AVAIL - XMIN)
         ARNTRF = MIN(ARNTRF, SNH4_AVAIL)

         DLTSNO3(L) = DLTSNO3(L) + ARNTRF
         DLTSNH4(L) = DLTSNH4(L) - ARNTRF
         TNITRIFY   = TNITRIFY   + ARNTRF

!-----------------------------------------------------------------------
!       Denitrification section
!-----------------------------------------------------------------------
!       Denitrification only occurs if there is nitrate, SW > DUL and
!       soil temperature > 5.
        IF (NO3(L) .GT. 0.01 .AND. SW(L) .GT. DUL(L) .AND.
     &       ST(L) .GE. 5.0) THEN


!         Water extractable soil carbon: estimated according to
!         Rolston et al. 1980, as cited in Godwin & Jones 1991 (ASA
!         monograph #31). Calculate carbohydrate carbon as 40% of the
!         carbohydrate pool.
C-UPS     Corrected per e-mail 03/29/00
          !CW = 24.5 + 0.0031 * (HUMC(L) + 0.4 * FPOOL(L,1)) * FAC(L)

!     ----------------------------------------------------------------
!11/18/2003 UPS: THE NEW NTRANS SHOULD READ: 
!         CW = 24.5 + {0.0031 * HUMC(L) + 0.4 * FPOOL(L,1)} * FAC(L) 
!            = 24.5 + AVAILABLE CARBON FROM HUMIC FRACTION + FRESH C from  CARBOHYDRATE POOL 

!NOTES: 1. ONLY THE HUMIC C IS MULTIPLIED BY 0.0031 
!       2. SOILC IN GODWIN&JONES INCLUDED BOTH HUMIC C AND FRESH (LITTER POOL C) 
!       3. WE ARE USING ONLY THE CARBOHYDRATE POOL (*0.4 TO C) = ALL AVAILABLE 
!       4. FPOOL is still kg of Organic matter/ha (and not kg C/ha)?? 

!     SO WE NEED TO FIX BOTH DSSAT4 AND GODWIN AND SINGH EQN TO THE ABOVE. 

!          CW = 24.5 + (0.0031 * HUMC(L) + 0.4 * FPOOL(L,1)) * FAC(L) 

!     The above removed on 1/14/2004 as per email from AJG and UPS

!     ----------------------------------------------------------------
!     DENITRIFICATION - CORRECTIONS - 13 Jan 2004 (US)
          CW = 24.5 + 0.0031 * (HUMC(L) + 0.4 * FPOOL(L,1)) * FAC(L)
!
!     The DENITRIF or DNRATE calculations are identical in NTRANS 
!             (DSSAT4) and Godwin and Singh:
!
!     DENITRIF = {6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * TFDENIT * DLAG }/ FAC(L)
!     -- AS in NTRANS (DSSAT4)
!
!         = {in concentration unit}/FAC
!         = kg N (denitrified)
!         = {in conc unit }/(10/BD*DLAYR)
!
!     DENITRIF = 6.0*1.E-05 * CW * NO3(L) * WFDENIT * TFDENIT * DLAG*BD(L)*DLAYR(L)
!     -- AS in GODWIN & SINGH
!
!     NOTE: CW is in concentration unit!!  Extractable C concentration.
!
!     CW = (SOILC*FAC(L))*0.0031 + 24.5
!
!       where SOILC = HUMC(L) + 0.4 * FOM(L) in kg/ha - Origianl definition
!        Later corrected to:
!        SOILC = HUMC(L) + 0.4 * FPOOL(L,1)  -- because only carbohydrate 
!             pool from the FOM is assumed to provide labile/extractable C.
! 
!     CW   = ({HUMC(L) + 0.4 * FPOOL(L,1)} * FAC(L))*0.0031 + 24.5
!
!     The equations in Godwin and Jones as well as Godwin and Singh are incorrect!!
                
!     ----------------------------------------------------------------

!         Water factor for denitrification: only if SW > DUL.
          WFDENIT = 1. - (SAT(L) - SW(L)) / (SAT(L) - DUL(L))
          WFDENIT = MAX (MIN (WFDENIT, 1.), 0.)

          IF (WFDENIT .GT. 0.0) THEN
             DLAG(L) = DLAG(L) + 1
           ELSE
             DLAG(L) = 0
          ENDIF

          IF (DLAG(L) .LT. 5) THEN
             WFDENIT = 0.0
          ENDIF

!         Denitrification rate
C-UPS     Corrected per e-mail 03/29/00
!         DLAG REMOVED REVISED-US 4/20/2004
          DENITRIF = 6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * 
     &                 TFDENIT / FAC(L)       
          DENITRIF = AMAX1 (DENITRIF, 0.0)

!         The minimum amount of NO3 that stays behind in the soil and 
!         cannot denitrify is XMIN.
!         XMIN    = 0.25 / FAC(L)
          XMIN    = 0.       !AJG

!         Check that no more NO3 denitrifies than there is, taking
!         into account what has already been removed by other
!         processes (thus use only negative DLTSNO3 values). This is a
!         protection against negative values at the integration step.
          SNO3_AVAIL = SNO3(L) + MIN (DLTSNO3(L), 0.) - XMIN

!         Take the minimum of the calculated denitrification and the
!         amount of NO3 available for denitrification. 
          DENITRIF  = MIN (DENITRIF, SNO3_AVAIL)

C         If flooded, lose all nitrate --------REVISED-US
!          IF (FLOOD .GT. 0.0) THEN
!            !DNFRATE = SNO3(L) - 0.5/FAC(L)        !XMIN?, SNO3_AVAIL?
!            DNFRATE = SNO3_AVAIL - 0.5/FAC(L)        !XMIN?, SNO3_AVAIL?
!          ELSE
!            DNFRATE = 0.0
!          ENDIF

!chp 4/20/2004          DENITRIF = AMAX1 (DENITRIF, DNFRATE)
          DENITRIF = AMAX1 (DENITRIF, 0.0)
          IF (NSWITCH .EQ. 6) THEN
            DENITRIF = 0.0
          ENDIF

!         Reduce soil NO3 by the amount denitrified and add this to
!         the NOx pool
          DLTSNO3(L) = DLTSNO3(L) - DENITRIF
          TNOX       = TNOX       + DENITRIF

        ELSE
!         IF SW, ST OR NO3 FALL BELOW CRITICAL IN ANY LAYER RESET LAG EFFECT.
          DLAG(L) = 0      !REVISED-US
        ENDIF   !End of IF block on denitrification.
      END DO   !End of soil layer loop.

!     ------------------------------------------------------------------
!     Downward and upward N movement with the water flow.
!     ------------------------------------------------------------------
      IF (IUON) THEN
        NSOURCE = 1    !Urea.
        CALL NFLUX (
     &    ADCOEF, BD, DLAYR, DRN, DUL, FLOW, NLAYR,       !Input
     &    UREA, NSOURCE, SW,                              !Input
     &    DLTUREA, TLCH)                                  !Output
      ENDIF

      NSOURCE = 2   !NO3.
      CALL NFLUX (
     &  ADCOEF, BD, DLAYR, DRN, DUL, FLOW, NLAYR,         !Input
     &  SNO3, NSOURCE, SW,                                !Input
     &  DLTSNO3, TLCH)                                    !Output

!***********************************************************************
!***********************************************************************
!     END OF FIRST DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION (also performed for seasonal initialization)
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT .OR. DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------

      IF (DYNAMIC .EQ. INTEGR) THEN
!       Update flood N components.
        IF (NBUND > 0) THEN
          CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTFON, DLTSNH4, DLTSNO3, DLTUREA,FLOODN,OXLAYR,!I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
        ENDIF
      
        !Update oxidation layer variables
        CALL OXLAYER(CONTROL,
     &    BD1, ES, FAC, FERTYPE, FLOODWAT, LFD10,         !Input
     &    NSWITCH, SNH4, SNO3, SOILPROP, SRAD, ST,        !Input
     &    SW, TMAX, TMIN, UREA, XHLAI,                    !Input
     &    DLTSNH4, DLTSNO3, DLTUREA, OXLAYR,              !I/O
     &    ALI, DAILY, TOTAML)                             !Output
      ENDIF

!     Loop through soil layers for integration
      DO L = 1, NLAYR
!        SNO3(L) = SNO3(L) + DLTSNO3(L) - UNO3(L)
!        SNH4(L) = SNH4(L) + DLTSNH4(L) - UNH4(L)
        SNO3(L) = SNO3(L) + DLTSNO3(L)    !plant uptake added to DLTSNO3
        SNH4(L) = SNH4(L) + DLTSNH4(L)    !plant uptake added to DLTSNH4

        UREA(L) = UREA(L) + DLTUREA(L)
        HUMC(L) = HUMC(L) + DLTHUMC(L)
        HUMN(L) = HUMN(L) + DLTHUMN(L)
        FPOOL(L,1) = FPOOL(L,1) + DLTFPOOL(L,1)
        FPOOL(L,2) = FPOOL(L,2) + DLTFPOOL(L,2)
        FPOOL(L,3) = FPOOL(L,3) + DLTFPOOL(L,3)
        FON(L)     = FON(L)     + DLTFON(L)

!       Underflow trapping
        IF (SNO3(L) .LT. 0.00001) SNO3(L) = 0.0
        IF (SNH4(L) .LT. 0.00001) SNH4(L) = 0.0
        IF (UREA(L) .LT. 0.00001) UREA(L) = 0.0
        IF (HUMC(L) .LT. 0.00001) HUMC(L) = 0.0
        IF (HUMN(L) .LT. 0.00001) HUMN(L) = 0.0
        IF (FON(L)  .LT. 0.00001) FON(L)  = 0.0
        IF (FPOOL(L,1) .LT. 0.00001) FPOOL(L,1) = 0.0
        IF (FPOOL(L,2) .LT. 0.00001) FPOOL(L,2) = 0.0
        IF (FPOOL(L,3) .LT. 0.00001) FPOOL(L,3) = 0.0

        FOM(L) = FPOOL(L,1) + FPOOL(L,2) + FPOOL(L,3)

!       Conversions.
        NO3(L)  = SNO3(L) * FAC(L)
        NH4(L)  = SNH4(L) * FAC(L)
        UPPM(L) = UREA(L) * FAC(L)

      ENDDO

!     Call NCHECK to check for and fix negative values.
      CALL NCHECK (CONTROL, 
     &    CNRAT, FOM, FON, FPOOL, HUMC, HUMN, IFERI,      !Input
     &    IRESI, NLAYR, RESNIT, SNH4, SNO3, UREA)         !Input

!     Soil profile accumulations.
      TNH4   = 0.0
      TNO3   = 0.0
      TFON   = 0.0
      TFOM   = 0.0
      THUMC  = 0.0
      THUMN  = 0.0
      TUREA  = 0.0
      TFPOOL(1) = 0.0
      TFPOOL(2) = 0.0
      TFPOOL(3) = 0.0
      DO L = 1, NLAYR
        TNH4  = TNH4  + SNH4(L)
        TNO3  = TNO3  + SNO3(L)
        TFON  = TFON  + FON(L)
        TFOM  = TFOM  + FOM(L)
        THUMC = THUMC + HUMC(L)
        THUMN = THUMN + HUMN(L)
        TUREA = TUREA + UREA(L)
        TFPOOL(1) = TFPOOL(1) + FPOOL(L,1)
        TFPOOL(2) = TFPOOL(2) + FPOOL(L,2)
        TFPOOL(3) = TFPOOL(3) + FPOOL(L,3)
        SNI(L) = SNO3(L) + SNH4(L)
        WTNUP = WTNUP + (UNO3(L) + UNH4(L)) / 10.    !g[N]/m2 cumul.
      ENDDO

      TNH4NO3 = TNH4 + TNO3

      IF (DYNAMIC .EQ. SEASINIT) THEN
        CALL SoilNBal (CONTROL, ISWITCH, 
     &    ALGFIX, AMTNIT, CUMFNRO, CUMRESN, CUMSENN, HARVRES,!Input
     &    NBUND, NLAYR, RESLEFTN, TFON, THUMN, TLCH, TNH4,!Input
     &    TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP)    !Input

        CALL OpSoilNC(CONTROL, ISWITCH, 
     &    AMTNIT, CUMRES, DSNC, HUMC, HUMN, NAPNIT, NH4, NO3, 
     &    RESLEFT, RESLEFTN,
     &    TFOM, TFON, THUMC, THUMN, TLCH, TNH4, TNH4NO3, TNO3, 
     &    TOTAML, TMINERALIZE, TIMMOBILIZE, TNITRIFY, TNOX)
      ENDIF

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
C     Write daily output
      CALL OpSoilNC(CONTROL, ISWITCH, 
     &    AMTNIT, CUMRES, DSNC, HUMC, HUMN, NAPNIT, NH4, NO3, 
     &    RESLEFT, RESLEFTN,
     &    TFOM, TFON, THUMC, THUMN, TLCH, TNH4, TNH4NO3, TNO3, 
     &    TOTAML, TMINERALIZE, TIMMOBILIZE, TNITRIFY, TNOX)

      IF (NBUND > 0) THEN
        CALL FLOOD_CHEM(CONTROL, ISWITCH, 
     &    FLOODWat, LFD10, SOILPROP, SNO3, SNH4, UREA,    !Input
     &    SRAD, SW, TMAX, TMIN, XHLAI,                    !Input
     &    DLTFON, DLTSNH4, DLTSNO3, DLTUREA,FLOODN,OXLAYR,!I/O
     &    ALGFIX, BD1, CUMFNRO, TOTAML, TOTFLOODN)        !Output
      ENDIF

      CALL SoilNBal (CONTROL, ISWITCH, 
     &    ALGFIX, AMTNIT, CUMFNRO, CUMRESN, CUMSENN, HARVRES,!Input
     &    NBUND, NLAYR, RESLEFTN, TFON, THUMN, TLCH, TNH4,!Input
     &    TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP)    !Input

C***********************************************************************
C***********************************************************************
C     END OF SECOND DYNAMIC IF CONSTRUCT
C***********************************************************************
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END !SUBROUTINE NTRANS

!=======================================================================
! NTRANS Variables - updated 08/20/2003
!-----------------------------------------------------------------------
! AD            Lowest possible volumetric water content of a soil layer 
!                 when it is air dry. For the top soil layer AD may be 
!                 lower than the lower limit LL, because this layer may dry 
!                 out due to water evaporation at the soil surface.
!                 (cm3 [H2O]/ cm3 [soil])
! ADCOEF(L)     Anion adsorption coefficient for soil layer L;  for reduced 
!                 anion (nitrate) flow in variable-charge soils (ADCOEF = 0 
!                 implies no anion retention) (cm3 (H2O] / g [soil])
! AK            Maximum hydrolysis rate of urea (i.e. proportion of urea 
!                 that will hydrolyze in 1 day under optimum conditions). 
!                 AK >= 0.25. Also: Today's value of the nitrification 
!                 potential, calculated from the previous day’s value (d-1)
! ALGFIX        N in algae (kg [N] / ha)
! ALI            
! AMTNIT        Cumulative amount of N in fertilizer applications
!                (kg [N] / ha)
! ANFER(I)      Amount of nitrogen in fertilizer applied in Ith application
!                (kg [N] / ha)
! ARNTRF        Daily nitrification rate (kg [N] / ha / d)
! BD(L)         Bulk density, soil layer L (g [soil] / cm3 [soil])
! BD1           Bulk density of oxidized layer (g [soil] / cm3 [soil])
! CMF           Affect of soil water content on decomposition of fresh 
!                 organic matter 
! CNR           C/N ratio of FOM (kg [C] / kg [N])
! CNRAT(L)      C/N ratio of humus or humus pool in soil layer L
!                (kg [C] / kg [N])
! CNRF          C/N ratio factor: the effect of C/N ratio on SOM 
!                 decomposition (range 0-1) 
! CONTROL       Composite variable containing variables related to control 
!                 and/or timing of simulation.  The structure of the 
!                 variable (ControlType) is defined in ModuleDefs.for. 
! CUMFNRO       Cumulative N lost in runoff over bund (kg [N] / ha)
! CUMRES        Cumulative amount of residue application (kg [res] / ha)
! CUMRESN       Cumulative amount of N in residue application (kg [N] / ha)
! CUMSENN       Cumulative N in senesced plant matter added to soil and 
!                 surface (kg [N] / ha)
! CW            Water extractable SOM carbon (µg [C] / g [soil])
! DAILY         Logical variable to determine whether daily or hourly flood 
!                 chemistry or oxidation layer calculations are done. 
! DECFACT       Decay rate of a FOM pool (fraction / day)
! DENITRIF      Denitrification rate (kg [N] / ha / d)
! DLAG(L)       Number of days with soil water content greater than the 
!                 drained upper limit for soil layer L.  For 
!                 denitrification to occur, DLAG must be greater than 4 (4 days
!                 lag period before denitrification begins). 
! DLAYR(L)      Thickness of soil layer L (cm)
! DLTFON(L)     Rate of change of N in fresh organic residue  in soil layer 
!                 L (kg [N] / ha / d)
! DLTFPOOL(L,J) Rate of change of FOM pool (FPOOL) in soil layer L 
!                 (J=1=carbohydrate, 2=cellulose, 3=lignin)
!                 (kg [residue pool] / ha / d)
! DLTHUMC(L)    Rate of change of Carbon in stable organic matter (humus) 
!                 in soil layer L (kg [C] / ha - d)
! DLTHUMN(L)    Change in nitrogen in stable organic matter (humus) in soil 
!                 layer L (kg[N]/ha-d)
! DLTSNH4(L)    Rate of change of ammonium in soil layer L
!                (kg [N] / ha / d)
! DLTSNO3(L)    Rate of change of nitrate in soil layer L (kg [N] / ha / d)
! DLTUREA(L)    Rate of change of urea content in soil layer L
!                (kg [N] / ha / d)
! DMINR         Maximum decomposition rate constant of stable organic 
!                 matter (d-1)
! DMOD          Factor to adjust the mineralization rate for certain 
!                 atypical soils (range 0-1) 
! DNFRATE       Maximum denitrification rate (kg [N] / ha / d)
! DOY           Current day of simulation (d)
! DRN(L)        Drainage rate through soil layer L (cm/d)
! DSNC          Depth to which C and N are integrated across all soil 
!                 layers for output in CARBON.OUT (cm)
! DUL(L)        Volumetric soil water content at Drained Upper Limit in 
!                 soil layer L (cm3[water]/cm3[soil])
! DYNAMIC       Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!                 INTEGR, OUTPUT, or FINAL 
! ES            Actual soil evaporation rate (mm/d)
! FAC(L)        Conversion factor to switch from kg [N] / ha to µg [N] / g 
!                 [soil] for soil layer L 
! FDAY(I)       Julian date for Ith fertilizer application (YYYYDDD)
! FERMET(I)     Fertilizer method for Ith application 
! FERTYPE       Fertilizer type for current application 
! FLOOD         Current depth of flooding (mm)
! FLOODN        Composite variable which contains flood nitrogen mass and 
!                 concentrations. Structure of variable is defined in 
!                 ModuleDefs.for. (var.)
! FLOODWAT      Composite variable containing information related to bund 
!                 management. Structure of variable is defined in 
!                 ModuleDefs.for. 
! FLOW(L)       Movement of water between unsaturated soil layers due to 
!                 soil evaporation: + = upward, -- = downward (cm/d)
! FOM(L)        Fresh organic residue in soil layer L
!                (kg [dry matter] / ha)
! FOMFRAC       Fraction of carbohydrate, cellulose, or lignin in FOM, 
!                 before the decomposition is done (fraction)
! FON(L)        Nitrogen in fresh organic matter in soil layer L
!                (kg [N] / ha)
! FPOOL(L,J)    FOM pool in soil layer L: J=1:carbohydrate, 2:cellulose, 
!                 3:lignin (kg [residue pool] / ha)
! HARVRES       Composite variable containing harvest residue amounts for 
!                 total dry matter, lignin, and N amounts.  Structure of 
!                 variable is defined in ModuleDefs.for. 
! HUMC(L)       Carbon in stable organic matter (humus) (kg [C] / ha)
! HUMFRAC       Humus fraction that decomposes (fraction)
! HUMN(L)       Nitrogen in stable organic matter (humus) (kg [N] / ha)
! IFERI         Fertilizer switch (A= automatic, R= on specified dates 
!                 (YYYYDDD format), D = on specified days after planting 
!                 (DDD) format. 
! IMMOB         N immobilization rate associated with residue decomposition
!                (kg [N] / (ha - d))
! IRESI         Residue application method. A=Automatic residue application 
!                 at a certain days after planting for multiple years; N=No 
!                 residue; R=On reported dates; D=As reported, in DAP; 
!                 F=Auto, with fixed amounts 
! ISWITCH       Composite variable containing switches which control flow 
!                 of execution for model.  The structure of the variable 
!                 (SwitchType) is defined in ModuleDefs.for. 
! IUOF          Critical Julian day when all urea is assumed to be 
!                 hydrolyzed (this is assumed to occur 21 days after the 
!                 urea application) (d)
! IUON          Flag indicating presence of urea (true or false) 
! LFD10         Date, 10 days after last fertilization.  Used to determine 
!                 whether hourly flood chemistry computations will be done 
!                 (see DAILY variable). (YYYYDDD)
! LL(L)         Volumetric soil water content in soil layer L at lower 
!                 limit (cm3 [water] / cm3 [soil])
! NAPNIT        Current number of fertilizer applications applied. 
! NAPPL         Maximum number of applications (for fertilizer, irrigation, 
!                 etc.) 
! NBUND         Number of bund height records 
! NFERT         Total number of observed fertilizer applications 
! NH4(L)        Ammonium N in soil layer L (µg[N] / g[soil])
! NITCAPY(L)    Previous day's nitrification potential in soil layer L, 
!                 indicating whether there may be a lag phase for 
!                 nitrification to proceed (range: 0-1) 
! NITRIF        Nitrification rate (kg [N] / ha - d)
! NI_AVAIL      N available for immobilization (kg [N] / ha)
! NL            Maximum number of soil layers = 20 
! NLAYR         Actual number of soil layers 
! NNOM          Net mineral N release from all SOM sources (kg [N] / ha)
! NO3(L)        Nitrate in soil layer L (µg[N] / g[soil])
! NSOURCE       Flag for N source (1 = urea, 2 = NO3) 
! NSTRES        Nitrogen stress factor (1=no stress, 0=max stress) 
! NSWITCH       Nitrogen switch - can be used to control N processes (0-No 
!                 N simulated, 1-N simulated, 5-Nitrification off, 
!                 6-denitrification off, 7-floodwater loss off, 8-drainage 
!                 losses off, 9-leaching off, 10-runoff losses off 
! OXLAYR        Composite variable which contains data about oxidation 
!                 layer.  See ModuleDefs.for for structure. 
! PH(L)         pH in soil layer L 
! PHFACT        Effect of pH on various soil N processes; value varies with 
!                 routine 
! PHMIN         Effect of pH on decomposition of fresh organic matter in 
!                 soil 
! PHN(L)        Factor for pH effect on nitrification rate (range 0-1) for 
!                 soil layer L 
! PRCEL         Cellulose fraction of the residue (fraction)
! PRCHO         Carbohydrate fraction of the residue (fraction)
! PRLIG         Lignin fraction of the residue (fraction)
! RCN           C/N ratio of initial root residue (kg [C] / kg [N])
! RDCEL         Maximum decomposition rate of cellulose (fraction / day)
! RDCHO         Maximum decomposition rate of carbohydrates
!                (fraction / day)
! RDLIG         Maximum decomposition rate of lignin (fraction / day)
! REDFACT       Reduction factor for FOM decomposition and for N leaching 
! REQN          Threshhold N content of fresh organic matter, below which N 
!                 immobilization will occur during decomposition 
! RESLEFT       Residue material which is left on top of soil and not 
!                 incorporated (kg[residue]/ha)
! RESLEFTN      N in residue which is not incorporated (kg[N]/ha)
! RESNIT        N in current residue application (kg[N]/ha)
! SAT(L)        Volumetric soil water content in layer L at saturation
!                (cm3 [water] / cm3 [soil])
! SENESCE       Composite variable containing data about daily senesced 
!                 plant matter. Structure of variable is defined in 
!                 ModuleDefs.for 
! SENSUM        Mass of senesced plant matter today (kg / ha)
! SENWT         Leaf senescence due to N mobilization
!                (g[leaf] / m2[ground])
! SNH4(L)       Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNH4NO3(L)    Total inorganic N in layer L (kg [N] / ha)
! SNH4_AVAIL    Maximum amount of NH4 that may nitrify (kg [N] / (ha - d))
! SNI           Total inorganic N in soil profile (kg [N] / ha)
! SNO3(L)       Total extractable nitrate N in soil layer L (kg [N] / ha)
! SNO3_AVAIL    NO3 available for denitrification (kg [N] / ha)
! SOILPROP      Composite variable containing soil properties including 
!                 bulk density, drained upper limit, lower limit, pH, 
!                 saturation water content.  Structure defined in ModuleDefs. 
! SRAD          Solar radiation (MJ/m2-d)
! ST(L)         Soil temperature in soil layer L (°C)
! SW(L)         Volumetric soil water content in layer L
!                (cm3 [water] / cm3 [soil])
! SWEF          Soil water evaporation fraction; fraction of lower limit 
!                 content to which evaporation can reduce soil water 
!                 content in top layer (fraction)
! T2            Temperature factor for nitrification 
! TFACTOR       Temperature factor for nitrification 
! TFDENIT       Temperature factor for denitrification rate (range 0-1) 
! TFNIT         Soil temperature factor for nitrification (range 0-1) 
! TFNITY(L)     Yesterday’s soil temperature factor for nitrification 
!                 (range 0-1) 
! TFOM          Sum of FOM (fresh organic matter) across the total soil 
!                 profile (kg [dry matter] / ha)
! TFON          Sum of FON (N in fresh organic matter) across the total 
!                 soil profile (kg [N] / ha)
! TFPOOL(J)     Total of FOM pool summed over soil profile: 
!                 J=1:carbohydrate, 2:cellulose, 3:lignin
!                 (kg [residue pool] / ha)
! TFSOM         Soil temperature factor for SOM decomposition (range 0-1) 
! TFUREA        Soil temperature factor for urea hydrolysis (range 0-1) 
! THUMC         Total C in stable organic matter (HUMC) in soil profile
!                (kg [C] / ha)
! THUMN         Total N in stable organic matter (HUMN) in soil profile
!                (kg [N] / ha)
! TIMMOBILIZE   Cumulative N immoblized (kg [N] / ha)
! TKELVIN       Soil temperature (oK)
! TLAG          Temperature factor for nitrification (0-1) 
! TLCH          Total N leached from soil (kg [N] / ha)
! TMAX          Maximum daily temperature (°C)
! TMIN          Minimum daily temperature (°C)
! TMINERALIZE   Cumulative mineralization (kg [N] / ha)
! TNH4          Total extractable ammonium N in soil profile (kg [N] / ha)
! TNH4NO3       Total amount of inorganic N (NH4 and NO3) across soil 
!                 profile (kg [N] / ha)
! TNITRIFY      Cumulative nitrification (kg [N] / ha)
! TNO3          Total extractable nitrate N in soil profile (kg [N] / ha)
! TNOX          Denitrification across the total soil profile  adding to 
!                 the nitrous oxide (NOx) pool of the air (kg [N] / ha)
! TOTAML        Cumulative ammonia volatilization (kg [N] / ha)
! TOTFLOODN     Current N in flood water (kg [N] / ha)
! TUREA         Total urea in soil profile (kg [N] / ha)
! UHYDR         Rate of urea hydrolysis (kg [N] / ha - d)
! UNH4(L)       Rate of root uptake of NH4, computed in NUPTAK
!                (kg [N] / ha - d)
! UNO3(L)       Rate of root uptake of NO3, computed in NUPTAK
!                (kg [N] / ha -d)
! UPPM(L)       Urea concentration of a soil layer (µg [N] / g [soil])
! UREA(L)       Amount of urea in soil layer L (kg [N] / ha)
! WF2           Water factor for nitrification 
! WFDENIT       Soil water factor for denitrification rate (range 0-1) 
! WFNIT         Soil water factor for nitrification rate (range 0-1) 
! WFNITY(L)     Yesterday’s soil water factor for nitrification rate (range 
!                 0-1) 
! WFPL          Water factor for nitrification 
! WFSOM         Reduction factor for FOM decay based on soil water content 
!                 (no reduction for SW = DUL, 100% reduction for SW = LL, 
!                 50% reduction for SW = SAT) 
! WFUREA        Reduction of urea hydrolysis due to soil water content 
!                 (range = 0-1; no reduction for SW = DUL, 20% reduction 
!                 for SW = LL, 70% reduction for SW = SAT (fraction)
! WTNUP         Cumulative N uptake (g[N] / m2)
! XHLAI         Healthy leaf area index (m2[leaf] / m2[ground])
! XL            Excess water (above DUL) as a fraction of the maximum 
!                 amount of excess water (i.e. saturated). (fraction)
! XMIN          Amount of NH4 that cannot be immobilized but stays behind 
!                 in soil as NH4; Also, Amount of NO3 that cannot denitrify 
!                 but stays behind in the soil as NO3 (kg [N] / ha)
! YEAR          Year of current date of simulation 
! YRPLT         Planting date (YYYYDDD)
!***********************************************************************
