C=======================================================================
C  COPYRIGHT 1998-2005 Iowa State University, Ames, Iowa
C                      University of Florida, Gainesville, Florida
C                      The University of Georgia, Griffin, Georgia
C                      International Fertilizer Development Center
C                      University of Guelph
C  ALL RIGHTS RESERVED
C=======================================================================
C======================================================================
C  Various MAIZE Subroutines
C
C  Maize growth routine that coordinates calling of GROSUB, PHENOL and
C  ROOTGR. Prepared for the modular version of Generic CERES.
C
C----------------------------------------------------------------------
C  Revision history
C
C  03/25/2001 WDB Written                         
C  12/01/2001 WDB Took out all crops except maize 
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
!  12/17/2004 CHP Modified HRESCeres call for harvest residue
C  02/04/2005 CHP Added PODWT to Summary.out output
C----------------------------------------------------------------------
C  Called : Alt_Plant
C----------------------------------------------------------------------


      SUBROUTINE MZ_CERES (CONTROL, ISWITCH,              !Input
     &    CO2, DAYL, EOP, HARVFRAC, NH4, NO3, SNOW,       !Input
     &    SOILPROP, SRAD, SW, TMAX, TMIN, TRWUP,          !Input
     &    TWILEN, YREND, YRPLT,                           !Input
     $    CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,       !Output
     &    PORMIN, RLV, RWUMX,SENESCE, STGDOY, UNH4,       !Output
     &    UNO3, XLAI)                                     !Output

      USE ModuleDefs

      IMPLICIT NONE
      SAVE

!----------------------------------------------------------------------

      REAL            AGEFAC    
      REAL            APTNUP       
      REAL            BD(20)     
      REAL            BIOMAS  
      REAL            BWAH  
      REAL            CANHT        
      REAL            CANNAA    
      REAL            CANWAA   
      REAL            CANWH   
      REAL            CARBO      
!      REAL            CNSD1    
!      REAL            CNSD2    
      REAL            CO2      
      CHARACTER*2     CROP   
      REAL            CUMDEP      
      REAL            CUMDTT    
      REAL            CUMPH     
      REAL            DAYL 
      REAL            DEPMAX     
      REAL            DLAYR(20) 
      REAL            DM         
      INTEGER         DOY 
      REAL            DS(20)     
      REAL            DTT      
      REAL            DUL(20)    
      INTEGER         DYNAMIC    
      REAL            EARS    
      REAL            EARWT      
      REAL            EOP           
      CHARACTER*6     ERRKEY  
      REAL            ESW(20)     
      CHARACTER*30    FILEIO
      INTEGER         FROP    
      REAL            G3       
      REAL            GNP     
      REAL            GNUP      
      REAL            GPP    
      REAL            GPSM     
      REAL            GRNWT     
      REAL            GRAINN   
      REAL            GRORT    
      INTEGER         YREND   
      REAL            HARVFRAC(2)
      REAL            HI               
      REAL            HIP            
      INTEGER         I          
      INTEGER         ICSDUR 
      CHARACTER*1     IDETO 
      CHARACTER*1     IPLTI     
      INTEGER         ISDATE        
      INTEGER         ISTAGE  
      CHARACTER*1     ISWWAT
      CHARACTER*1     ISWNIT  
      CHARACTER*1     ISWDIS
      REAL            LAI         
      INTEGER         LEAFNO 
      CHARACTER*1     IDETS    
      INTEGER         IDURP    
      REAL            LFWT      
      REAL            LL(20)       
      INTEGER         LUNIO  
      REAL            KCAN
      REAL            KEP
      REAL            MAXLAI     
      INTEGER         MDATE 
      CHARACTER*10    MZSTGNAM(20) 
      REAL            NH4(20)    
      INTEGER         NLAYR  
      REAL            NO3(20)  
      INTEGER         NOUTDO  
      REAL            NSTRES 
      REAL            P3          
      REAL            P5          
      REAL            PCNGRN   
      REAL            PCNVEG     
      REAL            PCNL   
      REAL            PCNSD    
      REAL            PCNST     
      REAL            PCNRT
      REAL            PDWI 
      REAL            SLPF    
      REAL            PGRORT  
      REAL            PHINT    
      REAL            PLA     
      REAL            PLAG    
      REAL            PODNO      
      REAL            PODWT   
      REAL            PORMIN  
      REAL            PLTPOP    
      REAL            PTF        
      REAL            RCNP        
      REAL            RLV(20)   
      REAL            RLWR      
      CHARACTER*1     RNMODE   
      REAL            ROOTN          
      REAL            ROWSPC     
      INTEGER         RSTAGE   
      REAL            RTDEP      
      REAL            RTWO        
      REAL            RTWT     
      REAL            RTWTO     
      REAL            RUE
      INTEGER         RUN  
      REAL            RWUEP1   
      REAL            RWUMX   
      REAL            SAT(20)  
      REAL            SATFAC    
      REAL            SDEPTH    
      REAL            SDSIZE    
      REAL            SDSZ      
      REAL            SDWT 
      REAL            SEEDNO   
      REAL            SENLA  
      REAL            SDWTAH  
      REAL            SHELPC    
      REAL            SHF(20)  
      REAL            SI1(6)     
      REAL            SI3(6)   
      REAL            SKERWT    
      REAL            SLA      
      REAL            SNOW          
      REAL            SRAD     
      REAL            STOVER    
      REAL            STOVN      
      REAL            STOVWT  
      INTEGER         STGDOY(20)   
      CHARACTER*10    STNAME(20)      
      REAL            STMWT     
      REAL            STMWTO       
      REAL            SUMDTT     
      REAL            SUMP        
      REAL            SW(20)     
      REAL            SWFAC    
      REAL            TANC     
      REAL            TLNO 
      REAL            TMIN    
      REAL            TMAX        
      REAL            TOPWT      
      REAL            TOTNUP  
      REAL            TRNU    
      REAL            TRWUP     
      REAL            TURFAC 
      REAL            TWILEN
      REAL            UNH4(20)      
      REAL            UNO3(20)    
      REAL            VSTAGE   
      REAL            VMNC     
      REAL            WTCO    
      REAL            WTLF    
      REAL            WTLO   
      REAL            WTSO     
      REAL            WTNCAN      
      REAL            WTNVEG     
      REAL            WTNLF 
      REAL            WTNSD 
      REAL            WTNST    
      REAL            WTNUP   
      REAL            XHLAI     
      REAL            XGNP      
      REAL            XLAI          
      REAL            XN        
      REAL            XNTI     
      REAL            XSTAGE   
      INTEGER         YR   
      REAL            YIELD    
      INTEGER         YRDOY  
      INTEGER         YREMRG   
      INTEGER         YRPLT 
      INTEGER         YRSIM    

!     Added by W.D.B. for pest damage at CiMMIT 4/14/2001

      REAL    AREALF,CLW,CSW,LAGSD,LNGPEG
      REAL    SLDOT,SSDOT,WLFDOT
      REAL    PHTIM(365)
      REAL    WTSD(NCOHORTS), SDNO(NCOHORTS)
      REAL    WTSHE(NCOHORTS), SHELN(NCOHORTS)
      REAL    SDDES(NCOHORTS)
      REAL    SWIDOT,WSHIDT,ASMDOT,DISLA,NPLTD,PPLTD
      REAL    WLIDOT,WRIDOT,WSIDOT
      INTEGER NR2

!     ------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE

      PARAMETER       (ERRKEY='MAIZE') 

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      CROP    = CONTROL % CROP
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      FILEIO  = CONTROL % FILEIO
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS     
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SHF    = SOILPROP % WR
      SLPF   = SOILPROP % SLPF   

      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT
      ISWDIS = ISWITCH % ISWDIS
      IPLTI  = ISWITCH % IPLTI
      IDETO  = ISWITCH % IDETO
      IDETS  = ISWITCH % IDETS

      DATA MZSTGNAM /
     &  'End Juveni',   !1
     &  'Floral Ini',   !2
     &  '75% Silkin',   !3
     &  'Beg Gr Fil',   !4
     &  'End Gr Fil',   !5
     &  'Maturity  ',   !6
     &  'Sowing    ',   !7
     &  'Germinate ',   !8
     &  'Emergence ',   !9
     &  '          ',   !10
     &  '          ',   !11
     &  '          ',   !12
     &  '          ',   !13
     &  'Start Sim ',   !14
     &  'End Sim   ',   !15
     &  'Harvest   ',   !16
     &  '          ',   !17
     &  '          ',   !18
     &  '          ',   !19
     &  'Harvest   '/   !20


C----------------------------------------------------------------------
C
C              Code for all Dynamic Variables
C
C----------------------------------------------------------------------

      CALL YR_DOY(YRDOY, YR, DOY)

C----------------------------------------------------------------------
C
C              DYNAMIC = RUNINIT
C
C----------------------------------------------------------------------

      IF(DYNAMIC.EQ.RUNINIT) THEN
          !Define names for growth stages for screen output in CROPGRO. 
          !Not used anywhere else in CERES
          DO I = 1, 20
              STNAME(I) = '          '
              STNAME(I) = MZSTGNAM (I)    
          END DO
          
          STGDOY(14) = YRSIM

          CALL GETLUN('OUTO', NOUTDO)
          CALL GETLUN('FILEIO', LUNIO)

                

          !-------------------------------------------------------------
          !Call phenology routine
          !-------------------------------------------------------------
          CALL MZ_PHENOL(DYNAMIC,ISWWAT,FILEIO,NOUTDO,IDETO,      !C
     &    CUMDEP,DAYL,DLAYR,IDURP,LEAFNO,LL,NLAYR,PLTPOP,SDEPTH,  !I
     &    SI1,SI3,SNOW, SRAD,SUMP,SW,TMAX,TMIN, TWILEN,           !I
     &    XN,YRDOY,YRSIM,                                         !I
     &    CUMDTT,DTT,EARS,GPP,ISDATE, ISTAGE,MDATE,STGDOY,SUMDTT, !O
     &    XNTI,TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP,P3)                !O

          !-------------------------------------------------------------
          !Call growth routine
          !-------------------------------------------------------------
          CALL MZ_GROSUB (NOUTDO,ISWNIT,ISWWAT,IDETO,DYNAMIC,
     &  STGDOY,YRDOY,DOY,PLTPOP,PHINT,ROWSPC,PORMIN,
     &  P5,G3,SUMDTT,DTT,CUMPH,ISTAGE,
     &  ICSDUR,SWFAC,TURFAC,
     &  LFWT,LAI,GRNWT,XN,TMIN,TMAX,SRAD,CO2,GPSM,
     &  GPP,GRORT,PTF,BIOMAS,PLA,SENLA,RTWT,EARWT,
     &  TLNO,CARBO,STMWT,XNTI,PLAG,EARS,
     &  SUMP,LEAFNO,IDURP,DM,
     &  NSTRES,AGEFAC,GRAINN,ROOTN,STOVN,PDWI,STOVWT,
     &  PGRORT,RCNP,VMNC,GNP,
     &  TANC,TRNU,NLAYR,RLV,NO3,NH4,
     &  UNO3,UNH4,XSTAGE,SW,LL,SAT,DLAYR,SHF, !CNSD1,CNSD2,
     &  YIELD,SKERWT,CANNAA,CANWAA,XHLAI,EOP,TRWUP,RWUEP1,
     &  WTNCAN,WTNSD,WTNVEG,PCNVEG,WTNUP,
     &  WTNLF,WTNST,MDATE, 
     &  APTNUP, GNUP, MAXLAI, STOVER, TOTNUP, XGNP,XLAI,
     &  SHELPC,SDSIZE,HI,HIP,TOPWT,SLA,STMWTO,RTWO,PODWT,
     &  SDWT,SEEDNO,PODNO,WTLF,RTWTO,PCNL,PCNSD,PCNST,PCNRT,
     &  CANHT,CANWH,VSTAGE,RSTAGE,PCNGRN,SATFAC,
     &  AREALF,WLIDOT,WSIDOT,WRIDOT,SWIDOT,PPLTD,ASMDOT,FILEIO,
     &  RLWR,SDSZ,RWUMX,BD,RUE, SENESCE, SLPF, P3)


          !-------------------------------------------------------------
          !Call Root routine
          !-------------------------------------------------------------

          CALL MZ_ROOTGR (DYNAMIC,ISWNIT,                         !C
     &        CUMDEP,CUMDTT,DEPMAX,DLAYR,DTT,ESW,GRORT,ISTAGE,    !I
     %        LL,DUL,NO3,NH4,NLAYR,PLTPOP,PORMIN,RLWR,SAT,SDEPTH, !I
     %        SHF,STGDOY,SW,SWFAC,YRDOY,                          !I
     %        RTDEP,RLV)            

          CALL MZ_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)

          CALL MZ_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)
       
          CALL MZ_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC,                           !Input
     &    WTNCAN, WTNUP, XGNP, XLAI, XN, YIELD, YRPLT,    !Input
     &    BWAH, SDWTAH)                                   !Output

        !IF (ISWDIS.EQ.'Y') THEN
         CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
        !ENDIF
C-----------------------------------------------------------------------
C                     DYNAMIC = SEASINIT
C-----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.SEASINIT) THEN

C-----------------------------------------------------------------------
C     Subroutine IPPARM reads FILEP, the PEST progress file.
C-----------------------------------------------------------------------
          IF (ISWDIS.EQ.'Y') THEN
          CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
          ENDIF
      
          DO I = 1, 20
              STNAME(I) = '          '
              STNAME(I) = MZSTGNAM (I)    
          END DO
          
          STGDOY(14) = YRSIM


          CALL MZ_PHENOL(DYNAMIC,ISWWAT,FILEIO,NOUTDO,IDETO,      !C
     &    CUMDEP,DAYL,DLAYR,IDURP,LEAFNO,LL,NLAYR,PLTPOP,SDEPTH,  !I
     &    SI1,SI3,SNOW, SRAD,SUMP,SW,TMAX,TMIN, TWILEN,           !I
     &    XN,YRDOY,YRSIM,                                         !I
     &    CUMDTT,DTT,EARS,GPP,ISDATE, ISTAGE,MDATE,STGDOY,SUMDTT, !O
     &    XNTI,TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP,P3)                !O


          CALL MZ_GROSUB (NOUTDO,ISWNIT,ISWWAT,IDETO,DYNAMIC,
     &  STGDOY,YRDOY,DOY,PLTPOP,PHINT,ROWSPC,PORMIN,
     &  P5,G3,SUMDTT,DTT,CUMPH,ISTAGE,
     &  ICSDUR,SWFAC,TURFAC,
     &  LFWT,LAI,GRNWT,XN,TMIN,TMAX,SRAD,CO2,GPSM,
     &  GPP,GRORT,PTF,BIOMAS,PLA,SENLA,RTWT,EARWT,
     &  TLNO,CARBO,STMWT,XNTI,PLAG,EARS,
     &  SUMP,LEAFNO,IDURP,DM,
     &  NSTRES,AGEFAC,GRAINN,ROOTN,STOVN,PDWI,STOVWT,
     &  PGRORT,RCNP,VMNC,GNP,
     &  TANC,TRNU,NLAYR,RLV,NO3,NH4,
     &  UNO3,UNH4,XSTAGE,SW,LL,SAT,DLAYR,SHF, !CNSD1,CNSD2,
     &  YIELD,SKERWT,CANNAA,CANWAA,XHLAI,EOP,TRWUP,RWUEP1,
     &  WTNCAN,WTNSD,WTNVEG,PCNVEG,WTNUP,
     &  WTNLF,WTNST,MDATE, 
     &  APTNUP, GNUP, MAXLAI, STOVER, TOTNUP, XGNP,XLAI,
     &  SHELPC,SDSIZE,HI,HIP,TOPWT,SLA,STMWTO,RTWO,PODWT,
     &  SDWT,SEEDNO,PODNO,WTLF,RTWTO,PCNL,PCNSD,PCNST,PCNRT,
     &  CANHT,CANWH,VSTAGE,RSTAGE,PCNGRN,SATFAC,
     &  AREALF,WLIDOT,WSIDOT,WRIDOT,SWIDOT,PPLTD,ASMDOT,FILEIO,
     &  RLWR,SDSZ,RWUMX,BD,RUE, SENESCE, SLPF, P3)
                          

          CALL MZ_ROOTGR (DYNAMIC,ISWNIT,                         !C
     &        CUMDEP,CUMDTT,DEPMAX,DLAYR,DTT,ESW,GRORT,ISTAGE,    !I
     %        LL,DUL,NO3,NH4,NLAYR,PLTPOP,PORMIN,RLWR,SAT,SDEPTH, !I
     %        SHF,STGDOY,SW,SWFAC,YRDOY,                          !I
     %        RTDEP,RLV)            

          CALL MZ_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)

          CALL MZ_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)


          CALL MZ_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC,                           !Input
     &    WTNCAN, WTNUP, XGNP, XLAI, XN, YIELD, YRPLT,    !Input
     &    BWAH, SDWTAH)                                   !Output

C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
C                 DYNAMIC = RATE
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.RATE) THEN


        IF (ISWDIS.EQ.'Y') THEN
          CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
        ENDIF
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
C                 DYNAMIC = INTEGRATE
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN

        IF (ISWDIS.EQ.'Y') THEN
          CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
        ENDIF

          !------------------------------------------------------------
          !        Call MZ_PHENOL
          !------------------------------------------------------------
        IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 7) THEN               
          IF (CROP .NE. 'FA') THEN
            CALL MZ_PHENOL(DYNAMIC,ISWWAT,FILEIO,NOUTDO,IDETO,    !C
     &    CUMDEP,DAYL,DLAYR,IDURP,LEAFNO,LL,NLAYR,PLTPOP,SDEPTH,  !I
     &    SI1,SI3,SNOW, SRAD,SUMP,SW,TMAX,TMIN, TWILEN,           !I
     &    XN,YRDOY,YRSIM,                                         !I
     &    CUMDTT,DTT,EARS,GPP,ISDATE, ISTAGE,MDATE,STGDOY,SUMDTT, !O
     &    XNTI,TLNO,XSTAGE,YREMRG,RUE,KCAN,KEP,P3)                !O
          ENDIF
        ENDIF
          !------------------------------------------------------------
          !Call MZ_GROSUB
          !------------------------------------------------------------
        IF (ISTAGE .LT. 6) THEN    
          CALL MZ_GROSUB (NOUTDO,ISWNIT,ISWWAT,IDETO,DYNAMIC,
     &      STGDOY,YRDOY,DOY,PLTPOP,PHINT,ROWSPC,PORMIN,
     &      P5,G3,SUMDTT,DTT,CUMPH,ISTAGE,
     &      ICSDUR,SWFAC,TURFAC,
     &      LFWT,LAI,GRNWT,XN,TMIN,TMAX,SRAD,CO2,GPSM,
     &      GPP,GRORT,PTF,BIOMAS,PLA,SENLA,RTWT,EARWT,
     &      TLNO,CARBO,STMWT,XNTI,PLAG,EARS,
     &      SUMP,LEAFNO,IDURP,DM,
     &      NSTRES,AGEFAC,GRAINN,ROOTN,STOVN,PDWI,STOVWT,
     &      PGRORT,RCNP,VMNC,GNP,
     &      TANC,TRNU,NLAYR,RLV,NO3,NH4,
     &      UNO3,UNH4,XSTAGE,SW,LL,SAT,DLAYR,SHF, !CNSD1,CNSD2,
     &      YIELD,SKERWT,CANNAA,CANWAA,XHLAI,EOP,TRWUP,RWUEP1,
     &      WTNCAN,WTNSD,WTNVEG,PCNVEG,WTNUP,
     &      WTNLF,WTNST,MDATE, 
     &      APTNUP, GNUP, MAXLAI, STOVER, TOTNUP, XGNP,XLAI,
     &      SHELPC,SDSIZE,HI,HIP,TOPWT,SLA,STMWTO,RTWO,PODWT,
     &      SDWT,SEEDNO,PODNO,WTLF,RTWTO,PCNL,PCNSD,PCNST,PCNRT,
     &      CANHT,CANWH,VSTAGE,RSTAGE,PCNGRN,SATFAC,
     &      AREALF,WLIDOT,WSIDOT,WRIDOT,SWIDOT,PPLTD,ASMDOT,FILEIO,
     &      RLWR,SDSZ,RWUMX,BD,RUE, SENESCE, SLPF, P3)
        ELSE
          UNO3 = 0.0
          UNH4 = 0.0
        ENDIF

          !------------------------------------------------------------
          !        Call MZ_ROOTGR
          !------------------------------------------------------------
          IF (ISWWAT .EQ. 'Y') THEN

            DEPMAX = DS(NLAYR)
            CALL MZ_ROOTGR (DYNAMIC,ISWNIT,                       !C
     &        CUMDEP,CUMDTT,DEPMAX,DLAYR,DTT,ESW,GRORT,ISTAGE,    !I
     %        LL,DUL,NO3,NH4,NLAYR,PLTPOP,PORMIN,RLWR,SAT,SDEPTH, !I
     %        SHF,STGDOY,SW,SWFAC,YRDOY,                          !I
     %        RTDEP,RLV)            
          ENDIF

C----------------------------------------------------------------------
C ---------------------------------------------------------------------
C
C                         DYNAMIC = OUTPUT 
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.OUTPUT) THEN
        IF (YRDOY .EQ. YREND) THEN
          STGDOY(16) = YREND
        ENDIF

        IF (YRDOY .GE. YRPLT) THEN
          CALL MZ_GROSUB (NOUTDO,ISWNIT,ISWWAT,IDETO,DYNAMIC,
     &  STGDOY,YRDOY,DOY,PLTPOP,PHINT,ROWSPC,PORMIN,
     &  P5,G3,SUMDTT,DTT,CUMPH,ISTAGE,
     &  ICSDUR,SWFAC,TURFAC,
     &  LFWT,LAI,GRNWT,XN,TMIN,TMAX,SRAD,CO2,GPSM,
     &  GPP,GRORT,PTF,BIOMAS,PLA,SENLA,RTWT,EARWT,
     &  TLNO,CARBO,STMWT,XNTI,PLAG,EARS,
     &  SUMP,LEAFNO,IDURP,DM,
     &  NSTRES,AGEFAC,GRAINN,ROOTN,STOVN,PDWI,STOVWT,
     &  PGRORT,RCNP,VMNC,GNP,
     &  TANC,TRNU,NLAYR,RLV,NO3,NH4,
     &  UNO3,UNH4,XSTAGE,SW,LL,SAT,DLAYR,SHF, !CNSD1,CNSD2,
     &  YIELD,SKERWT,CANNAA,CANWAA,XHLAI,EOP,TRWUP,RWUEP1,
     &  WTNCAN,WTNSD,WTNVEG,PCNVEG,WTNUP,
     &  WTNLF,WTNST,MDATE, 
     &  APTNUP, GNUP, MAXLAI, STOVER, TOTNUP, XGNP,XLAI,
     &  SHELPC,SDSIZE,HI,HIP,TOPWT,SLA,STMWTO,RTWO,PODWT,
     &  SDWT,SEEDNO,PODNO,WTLF,RTWTO,PCNL,PCNSD,PCNST,PCNRT,
     &  CANHT,CANWH,VSTAGE,RSTAGE,PCNGRN,SATFAC,
     &  AREALF,WLIDOT,WSIDOT,WRIDOT,SWIDOT,PPLTD,ASMDOT,FILEIO,
     &  RLWR,SDSZ,RWUMX,BD,RUE, SENESCE, SLPF, P3)

        ENDIF   

      CALL MZ_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)
      CALL MZ_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)

      CALL MZ_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC,                           !Input
     &    WTNCAN, WTNUP, XGNP, XLAI, XN, YIELD, YRPLT,    !Input
     &    BWAH, SDWTAH)                                   !Output

      IF (ISWDIS.EQ.'Y') THEN
        CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &    PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &    SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
      ENDIF
C----------------------------------------------------------------------
C ---------------------------------------------------------------------
C
C                         DYNAMIC = FINAL 
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      ELSEIF(DYNAMIC.EQ.FINAL) THEN

        CALL MZ_GROSUB (NOUTDO,ISWNIT,ISWWAT,IDETO,DYNAMIC,
     &  STGDOY,YRDOY,DOY,PLTPOP,PHINT,ROWSPC,PORMIN,
     &  P5,G3,SUMDTT,DTT,CUMPH,ISTAGE,
     &  ICSDUR,SWFAC,TURFAC,
     &  LFWT,LAI,GRNWT,XN,TMIN,TMAX,SRAD,CO2,GPSM,
     &  GPP,GRORT,PTF,BIOMAS,PLA,SENLA,RTWT,EARWT,
     &  TLNO,CARBO,STMWT,XNTI,PLAG,EARS,
     &  SUMP,LEAFNO,IDURP,DM,
     &  NSTRES,AGEFAC,GRAINN,ROOTN,STOVN,PDWI,STOVWT,
     &  PGRORT,RCNP,VMNC,GNP,
     &  TANC,TRNU,NLAYR,RLV,NO3,NH4,
     &  UNO3,UNH4,XSTAGE,SW,LL,SAT,DLAYR,SHF, !CNSD1,CNSD2,
     &  YIELD,SKERWT,CANNAA,CANWAA,XHLAI,EOP,TRWUP,RWUEP1,
     &  WTNCAN,WTNSD,WTNVEG,PCNVEG,WTNUP,
     &  WTNLF,WTNST,MDATE, 
     &  APTNUP, GNUP, MAXLAI, STOVER, TOTNUP, XGNP,XLAI,
     &  SHELPC,SDSIZE,HI,HIP,TOPWT,SLA,STMWTO,RTWO,PODWT,
     &  SDWT,SEEDNO,PODNO,WTLF,RTWTO,PCNL,PCNSD,PCNST,PCNRT,
     &  CANHT,CANWH,VSTAGE,RSTAGE,PCNGRN,SATFAC,
     &  AREALF,WLIDOT,WSIDOT,WRIDOT,SWIDOT,PPLTD,ASMDOT,FILEIO,
     &  RLWR,SDSZ,RWUMX,BD,RUE, SENESCE, SLPF, P3)

        CALL MZ_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)

        CALL MZ_OPNIT(CONTROL, ISWITCH, 
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)

        CALL MZ_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC,                           !Input
     &    WTNCAN, WTNUP, XGNP, XLAI, XN, YIELD, YRPLT,    !Input
     &    BWAH, SDWTAH)                                   !Output

        CALL HRes_Ceres(CONTROL,
     &    CROP, GRNWT, HARVFRAC, NLAYR, PLTPOP, PODWT,    !Input
     &    RLV, ROOTN, RTWT, SENESCE, STOVN, STOVWT, WTNSD,!Input
     &    HARVRES)                                        !Output

        !Set senescence variable to zero for next season
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0

      ENDIF

      RETURN
      END SUBROUTINE MZ_CERES

C----------------------------------------------------------------------
C
C           Variable Definitions
C
C----------------------------------------------------------------------

! AGEFAC      !Nitrogen stress factor affecting cell expansion 
! ANO3(20)    !Total extractable nitrate N in soil profile (kg N/ha)    
! ANH4(20)    !Total extractable ammonium N in soil profile (kg N/ha) 
! APTNUP      !Nitrogen in stover (above ground biomass) kg N/ha
! AREALF     Area of leaves (one side) per unit ground area
!              (cm2[leaf] / m2[ground])
! ASMDOT     Daily assimilative damage (g[CH2O] /m2 / d)
! BD(20)      !Bulk density of soil layer, g soil/cm3 soil
! BIOMAS      !Above ground biomass, g/m2  
! CANHT       !Canopy height, m (currently not calculated)
! CANNAA      !Stover N at anthesis, g N/M2    
! CANWAA      !Canopy weight at anthesis, g/m2  
! CANWH            !Canopy width, m (currently not calculated)
! CARBO       !Daily biomass production, g/plant/day  
! CLW        Cumulative leaf growth (g[leaf]/m2)
! CNSD1       !Cum. water stress on photosynthesis during growth stage    
! CNSD2       !Cumulative water stress on growth during growth stage  
! CO2         !Atmospheric CO2, ppm  
! CO2X(10)    !CO2 effect on photosynthesis, X axis is CO2 level, ppm
! CO2Y(10)    !CO2 effect on photosynthesis, Y axis is relative effect 
! CROP        !Two character representation of crop (ie. MZ)
! CSW        Cumulative stem growth (g[stem]/m2)
! CTYPE       !Crop type (Always 1 for maize)
! CUMDEP      !Cum. soil depth, cm 
! CUMDTT      !Cum. growing degree days, degrees C    
! CUMPH       !Cum. phyllochron intervals, or fully expanded leaves 
! DAP         !Days after planting
! DEPMAX      !Depth of soil, cm
! DISLA       Diseased leaf area (cm2[leaf]/m2[ground]/d)
! DLAYR(L)    Soil thickness in layer L (cm)
! DM          !Total above ground biomass, kg/ha   
! DOY         !Day of year
! DS(20)      !Depth of soil in layer, cm  
! DTT         !Growing degree days today, degrees C    
! DUL(20)     !Drained upper limite, cm3/cm3 
! DYNAMIC     !Main control variable to tell each module which section of code to run
! EARS        !Ears per plant, computed here and used in grosub.  
! EARWT       !Ear weight, g/ear!
! EOP         !Potential plant transpiration, mm/day
! ERRKEY      !Key or error messages
! ERRNUM      !Error number to print to screen in case of error
! ESW(20)     !Extractable soil water, cm3 water/cm3 soil  
! FAC(20)     !Factor that convergs mg elemental N/kg soil to kg N/ha for soil layer L
! FILEC       !Path and name of species file (ie. *.spe)
! FILEIO      !Name if input file (ie. DSSAT40.INP)               
! FOUND        !Flag used to warn user if information in an input file does not exist 
! G2          !Maximum kernel number (kernels/plant)
! G3          !Potential kernel growth rate, mg/kernel/day 
! GNP         !Nitrogen concentration in new grain growth, gN/g dry matter
! GNUP        !Total grain N uptake, kg N/ha
! GPP         !Grain number per plant, grains/plant 
! GPSM        !Grain numbers, grains/m2 
! GRNWT       !Grain weight, g/plan
! GRAINN      !Grain nitrogen content, g N/plant   
! GRORT       !Root growth rate, g/plant/day
! YREND       !Year and day of year for harvest
! HI          !Harvest index, Seed weight/above ground plant weight
! HIP         !Harvest index of ear, (g ear + seed weight)/(g above ground plant weight)
! I           !Index counter
! ICSDUR      !Calendar day accumulator for each growth stage 
! IDETO       !Switch for printing overview.out file
! IPLTI       !
! ISDATE      !Year and day of year for end of leaf growth
! ISTAGE      !Growth stage
! ISWDIS      !Disease switch
! ISWWAT      !Water balance switch
! ISWNIT      !Nitrogen balance switch 
! LAGSD      Not used in CERES except to pass 0 into pest.for
! LAI         !Leaf area index, cm2/cm2 
! LEAFNO      !Number of oldest leaf per plant (same as XN)
! IDETS       !Code to generate several output files
! L           !Index counter
! LINC        !Line number of input file
! LNGPEG      Not used in CERES except to pass 0 into pest.for
! IDURP       !Duration of ISTAGE 4, calendar days
! LFWT        !Leaf weight, g/plant   
! LL(20)      !Volumetric lower limit of soil water holding capacity, cm3 water/cm3 soil
! LNUM        !Line number in an input datafile
! LUNIO       !Logical unit number for file
! MAXLAI      !Maximum leaf area index, m2/m2
! MDATE       !Maturity data, year and day of year
! MZSTGNAM(20)!Array containing names of various growth stages
! NH4(20)     !Ammonium in soil layer L, mg elemental N/kg soil 
! NLAYR       !Number of soil layers
! NPEST       !Number of pests defined in the crop pest file
! NPLTD      Number of plants destroyed (#/m2/d)
! NO3(20)     !Nitrate in soil layer L (mg elemental N/kg soil) 
! NOUTDO   !Output file name
! NSTRES      !Nitrogen stress factor affecting growth (0-1) 
! P1          !Growing degree days (base 8C) from seedling emergence to end of Juvenile phase    
! P2          !Photoperiod sensitivity coefficient, 1/hr    
! P5          !Cumulative growing degree days (Base 8C) from silking to physioligical maturity
! PATHCR  !Path to species filee
! PCNGRN      !Nitrogen content in grain, %
! PCNVEG      !Percent nitrogen in vegetative tissue (leaf and stem), kg N/ha  
! PCNL        !Percent nitrogen in leaf tissue, %
! PCNSD       !Percent nitrogen in seeds, %
! PCNST       !Percent of nitrogen in stems, %
! PCNRT       !Percent of nitrogen in roots, %
! PCPID(40,6) !Pest coupling point identification code for pest i and couping point j
! PCTID(I)   Pest damage characterization method for pest I: (1) absolute 
!              daily damage rate, (2) percent observed damage, (3) daily 
!              percent damage rate
! PDCF1(I,J) Pest damage coefficient asscociated with pest I, coupling 
!              point J 
! PID(I)     Pest identification header from FILE
! PDWI        !Potential increment in new shoot growth, g/plant 
! PL(I)             Pest level for pest I today

! PGRORT      !Potential increment in new root growth, g/plant
! PHINT       !Phylochron interval. Number of GDD for new leaf emergence, degrees C 
! PHTIM      Not used in CERES except to pass 0 into pest.for
! PLA         !Plant leaf area, cm2/plant 
! PLAG        !Leaf area growth, cm2/plant  
! PLTPOP      !Plant Population, Pl/m  
! PODNO       !Ear number, #/m2
! PODWT       !Pod (ear) weight, g/m2
! PORMIN      !Minimum pore volume before soil water saturation effects growth (not used), cm3/cm3    
! PPLTD      Percent plants destroyed  (%/m2/d)
! PTF         !Ratio of above ground biomass to total biomass 
! RCNP        !Root critical nitrogen concentration, g N/g root dry weight 
! RLV(20)     !Root length volume of soil layer, cm3 root/cm3 soil
! RLWR        !Root length to weight ration, cm/g   
! RNMODE    !Run mode
! ROOTN       !Root nitrogen content, g N/plant   
! ROPT        !Second optimum temperature for development from species file, 
! ROWSPC      !Row spacing, cm  
! RSTAGE        !Growth stage (same as ISTAGE), 1-6.
! RTDEP       !Root depth, cm 
! RTWO        !Root weight, g/m2
! RTWT        !Root weight, g/plant 
! RTWTO       !Root weight, g/m2  (same as RTWO)
! RUN      !Run number
! RWUEP1      !Factor to modify water stress for cell expansion (in species file), mm/day
! RWUMX       !Maximum root water uptake parameter from species file  
! SAT(20)     !Saturated water holding capacity for soil layer, cm3 water/cm3 soil
! SATFAC      !Reduction of growth due to water logging (0-1.0)
! SDDES(J)   Number of seeds destroyed today in cohort J when shells are 
!              not destroyed (#/m2/day)
! SDEPTH      !Sowing depth, cm        
! SDNO(J)    Need to figure out how to use this array to pass in seed number???
! SDSIZE      !Average seed size, mg/seed
! SDSZ        !Maximum potential seed size, mg/kernel
! SDWT        !Seed (grain) weight, g/m2
! SECTION !Variable indicating which section to look for in an input file
! SEEDNO           !Seed number, #/m2
! SENLA       !Normal leaf senescence today, cm2/plant 
! SHELPC           !Shelling percentage (grain weight/ear weight)
! SHELN(J)   Number of shells for cohort J (#/m2)
! SHF(20)     !Soil hospitality factor, unitless    
! SI1(6)      !Water stress during a growth stage used for output           
! SI3(6)      !Nitrogen stress during a growth stage used for output
! SKERWT      !Weight per kernel, g/kernel 
! SLA              !Specific leaf area, cm2/g
! SLDOT      Defoliation due to daily leaf senescence (g/m2/day)
! SNOW             !Snow accumulation today, mm
! SRAD        !Total solar radiation today, MJ/m2  
! SSDOT      Not used in CERES except to pass 0 into pest.for
! STOVER           !Stover weight (leaf+stem), kg/ha
! STOVN       !Nitrogen content in stover, g N/plant 
! STOVWT      !Stover weight (Stem + leaf), g/plant 
! STGDOY(20)   !Array storing the dates that different growth stages occurred
! STNAME(20)      !Array containing names of various growth stages
! STMWT       !Stem weight, g/plant
! STMWTO           !Stem weight, g/m2
! SUMDTT      !Sum of growing degree days since stage initiation, C  
! SUMP        !Cumulative plant growth during ISTAGE 4, g/plant 
! SW(20)      !Volumetric soil water content of soil layer, cm3 water/cm3 soil   
! SWFAC       !Soil water stress effect on growth (0-1), 1 is no stress, 0 is full stress
! SWIDOT     Daily seed mass damage (g/m2/day)
! TANC        !Nitrogen content in above ground biomass, decimal   
! TBASE       !Base temperature below which no development occurs, C    
! TLNO        !Total number of leaves that the plant produces
! TMIN        !Minimum temperature today, C    
! TMAX        !Maximum temperature today, C 
! TOPT        !Optimum temperature for development (from species file), C
! TOPWT            !Total above ground biomass, g/m2
! TOTNUP           !Total shoot N uptake at maturity, kg N/ha
! TRNU        !Total potential root nitrogen uptake, kg N/ha 
! TRTNO    !Treatment number of current simulation
! TRWUP            !Total root water uptake, mm/day
! TURFAC      !Soil water stress effecting cell expansion
! UNH4(20)         !Plant uptake of ammonium from layer (kg N/ha/day)
! UNO3(20)         !Plant uptake of nitrate from a layer (kg N/ha/day)
! VSTAGE           !Vegetative growth stage (number of leaves)
! VMNC        !Plant vegetative minimum nitrogen concentration, g N/g plant 
! WLFDOT     Leaf weight losses due to freezing (g[leaf]/m2-d)
! WLIDOT     Daily pest or freeze damage to leaf mass (g/m2/day)
! WMODB*1 !Switch to indicate if weather has been modified in file X (Warning****** not passed in***)
! WRIDOT     Daily pest damage to root mass (g/m2/day)
! WSHIDT     Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WSIDOT     Daily pest damage to stem mass (g/m2/day)
! WTCO        !Cumulative loss of plant tissue, g/m2  (not computed)  
! WTLF        !Leaf weight, g/m2
! WTLO        !Cumulative loss of leaf tissue, g/m2 (not computed)    
! WTSD(J)    Seed mass  for cohort J (g/m2)
! WTSO        !Cumulative stem loss of stem tissue, g/m2 (not computed) 
! WTHADJ(2,8) !Used on mz_phenol but not passed into maize.for ???????  
! WTNCAN      !Weight of nitrogen in above ground biomass (stem, leaf, grain), kg N/ha    
! WTNVEG      !Weight of nitrogen in vegetative tissue, kg N/ha    
! WTNLF       !Weight of nitrogen in leaf tissue, kg N/h    
! WTNSD       !Weight of nitrogen in seed, g[N] / m2[ground]
! WTNST       !Weight of nitrogen in stem tissue, kg N/ha     
! WTNUP       !Total nitrogen uptake, g/m2  
! WTSHE(J)   Shell mass  for cohort J (g/m2)
! XHLAI       !Healthy leaf area, cm2/cm2  
! XGNP        !Nitrogen content of grain, %
! XLAI             !Leaf area index, m2/m2
! XLAT        !Latutude  
! XN          !Number of oldest expanding leaf    
! XNTI        !Number of leaves at tassel initiation
! XSTAGE      !Non-integer growth stage indicator    
! YR       !Year
! YIELD       !Yield in kg/ha at 0% moisture content
! YRDOY    !Current year and day of year
! YREMRG   !Year and day of year of emergence 
! YRPLT    !Year and day of year of planting
! YRSIM    !Year and day of year of first day of simulation


