C=======================================================================
C  COPYRIGHT 1998-2005 Iowa State University, Ames, Iowa
C                      University of Florida, Gainesville, Florida
C                      The University of Georgia, Griffin, Georgia
C                      International Fertilizer Development Center
C                      University of Guelph
C  ALL RIGHTS RESERVED
C=======================================================================
C======================================================================
C  Main CERES-Sorghum CSM Routine
C
C  This subroutine coordinates calling of SG_GROSUB, SG_PHENOL
C  and SG_ROOTGR. It was rewritten based on the Generic Ceres v3.5 
C  source code distributed with DSSAT v3.5. 
C----------------------------------------------------------------------
C  Revision history
C
C  07/15/2002 WDB Written 
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  08/12/2003 CHP Added I/O error checking
!  12/17/2004 CHP Modified HRESCeres call for harvest residue
C----------------------------------------------------------------------
C
C  Called : Alt_Plant
C
C  Calls  : SG_GROSUB, SG_PHENOL, SG_ROOTS
C----------------------------------------------------------------------

      SUBROUTINE SG_CERES (CONTROL, ISWITCH, 
     &     CO2, DAYL, EOP, HARVFRAC, NH4, NO3,            !Input
     &     SNOW, SOILPROP, SRAD, SW, TMAX, TMIN,          !Input
     &     TRWUP, TWILEN, YREND, YRPLT,                   !Input
     $     CANHT, HARVRES, MDATE, NSTRES, PORMIN, RLV,    !Output
     &     RWUMX, SENESCE, STGDOY, UNO3, UNH4, XLAI,KCAN,KEP) !Output

      USE ModuleDefs

      IMPLICIT NONE
      SAVE
!----------------------------------------------------------------------
!      Programming Notes  W.D.B
!
!      Note 1: Currently the weather data modification from file X is 
!      not being passed in. Is this a problem, or is the modified Tmax, 
!      Tmin, etc being passed in?
!
!      Note 2: Currently, the ecotype file is hard coded. This needs to 
!              be changed to read ecotype file name from dssat40.inp
!              as soon as the CSM writes this name in the .inp file.
!
!----------------------------------------------------------------------

      REAL            AGEFAC   
      REAL            APTNUP       
      REAL            BD(20)     
      REAL            BIOMAS
      CHARACTER*1     BLANK  
      PARAMETER (BLANK=' ')
      REAL            BWAH
      CHARACTER*255   C255
      REAL            CANHT        
      REAL            CANNAA    
      REAL            CANWAA   
      REAL            CANWH   
      REAL            CARBO      
      REAL            CNSD1    
      REAL            CNSD2    
      REAL            CO2      
      REAL            CO2X(10)  
      REAL            CO2Y(10)
      CHARACTER*2     CROP   
      INTEGER         CTYPE    
      REAL            CUMDEP      
      REAL            CUMDTT    
      REAL            CUMPH   
      REAL            DAYL   
      REAL            DEPMAX     
      REAL            DGET
      REAL            DJTI
      REAL            DLAYR(20) 
      REAL            DM         
      INTEGER         DOY 
      REAL            DS(20)     
      REAL            DSGT
      REAL            DTT      
      REAL            DUL(20)    
      INTEGER         DYNAMIC     
      REAL            EARWT 
      CHARACTER*16    ECONAM
      CHARACTER*6     ECOTYP     
      REAL            EOP           
      CHARACTER*6     ERRKEY
      INTEGER         ERRNUM    
      REAL            ESW(20)       
      CHARACTER*12    FILEC   
      CHARACTER*92    FILEGC
      CHARACTER*30    FILEIO     
      INTEGER         FOUND 
      INTEGER         FROP    
      REAL            G2       
      REAL            GDDE         
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
      REAL            KCAN
      REAL            KEP
      REAL            LAI         
      INTEGER         LEAFNO 
      CHARACTER*1     IDETS              
      REAL            LFWT      
      REAL            LL(20)    
      INTEGER         LINC    
      INTEGER         LNUM    
      INTEGER         LUNECO   
      INTEGER         LUNIO  
      REAL            MAXLAI     
      INTEGER         MDATE 
 
      REAL            NH4(20)    
      INTEGER         NLAYR  
      REAL            NO3(20)  
      INTEGER         NOUTDO  
      REAL            NSTRES     
      REAL            P1                
      REAL            P5          
      CHARACTER*80    PATHCR
      INTEGER         PATHL
      REAL            PCNGRN   
      REAL            PCNVEG     
      REAL            PCNL      
      REAL            PCNST     
      REAL            PCNRT   
      REAL            PDWI     
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
      REAL            ROPT     
      REAL            ROWSPC     
      INTEGER         RSTAGE   
      REAL            RTDEP             
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
      REAL            SDWT 
      REAL            SDWTAH
      CHARACTER*6     SECTION
      REAL            SEEDNO   
      REAL            SENLA    
      CHARACTER*10    SGSTGNAM(20)
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
      REAL            SW(20)     
      REAL            SWCG
      REAL            SWFAC    
      REAL            TANC    
      REAL            TBASE   
      REAL            TLNO 
      REAL            TMIN    
      REAL            TMAX   
      REAL            TOPT      
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
C------------------------------------------------------------------------
C  VARIABLES REQUIRED FOR PHENOL
C------------------------------------------------------------------------

      REAL BIOMS2
      REAL CSD1
      REAL CSD2
      REAL EMAT
      REAL G1
      INTEGER IDUR1
      INTEGER IPRINT
      REAL P2O
      REAL P2R
      REAL P3
      REAL P4
      REAL P9
      REAL PANWT
      REAL PGRNWT
      REAL SI2(6)
      REAL SI4(6)
      REAL SIND
      REAL TEMPCR
      REAL TPSM

C -------------------------------------------------------------------
C VARIABLES ONLY USED IN SG_PHASEI
C------------------------------------------------------------------------

      REAL BIOMS1
      REAL GROLF
      REAL GROSTM
      REAL LWMIN
      REAL MGROLF
      REAL MGROPAN
      REAL MGROSTM
      REAL MLFWT
      REAL MPANWT
      REAL MSTMWT
      REAL PAF
      REAL PGC
      REAL PLAN
      REAL PLAO
      REAL PLATO
      REAL PLAY
      REAL PLAMX
      REAL RANC
      REAL RWU(20)
      REAL SEEDRV
      REAL SLAN
      REAL SUMRTR
      REAL SWMAX
      REAL SWMIN
      REAL TCARBO
      REAL TCNP
      REAL TDUR
      REAL TGROLF
      REAL TGROPAN
      REAL TGROSTM
      REAL TILN
      REAL TILSW
      REAL TLFWT
      REAL TMNC
      REAL TPANWT
      REAL TSIZE
      REAL TSTMWT
      REAL VANC
!     REAL WSTR1
C-----------------------------------------------------------------------
C  VARIABLES NEEDED IN SG_GROSUB 
C-----------------------------------------------------------------------
      REAL LAT
      REAL NDEF3
      REAL NFAC
      REAL SLW
      REAL TEMF
      REAL TEMPM
      REAL TMFAC1(8)

C-----------------------------------------------------------------------
C  VARIABLES FROM SPECIES FILE
C-----------------------------------------------------------------------

      CHARACTER*80    C80
      CHARACTER*6     ECONO
      INTEGER         ERR 
      PARAMETER       (ERRKEY='SG_CERES')
      CHARACTER*12    FILES
      CHARACTER*12    FILEE
      CHARACTER*92    FILECC
      REAL            FSLFN
      REAL            FSLFW
      INTEGER         ISECT
      INTEGER         LUNCRP
      REAL            PARSR
      CHARACTER*80    PATHSR
      CHARACTER*80    PATHER
      REAL            SLPF
      REAL            PRFTC(4)
      REAL            RGFIL(4)
      CHARACTER*6     VARNO
      CHARACTER*16    VRNAME

!     ------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE

C      PARAMETER       (ERRKEY='MAIZE') 

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
      SLPF   = SOILPROP % SLPF 
      SAT    = SOILPROP % SAT    
      SHF    = SOILPROP % WR

      ISWWAT = ISWITCH % ISWWAT
      ISWNIT = ISWITCH % ISWNIT
      ISWDIS = ISWITCH % ISWDIS
      IPLTI  = ISWITCH % IPLTI
      IDETO  = ISWITCH % IDETO
      IDETS  = ISWITCH % IDETS

      DATA SGSTGNAM/
     &  'End Juveni',   !1
     &  'Floral Ini',   !2
     &  'End Lf Gro',   !3
     &  'End Pan Gr',   !4
     &  'End Mn Fil',   !5
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
     &  'Anthesis  ',   !16
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
              STNAME(I) = SGSTGNAM (I)    
          END DO
          
          STGDOY(14) = YRSIM

          CALL GETLUN('OUTO', NOUTDO)
          CALL GETLUN('FILEIO', LUNIO)

          CALL SG_GROSUB (
     &      AGEFAC, BIOMAS, CARBO, CNSD1,CNSD2, CO2X, CO2Y, 
     &      CO2, CSD2, CUMDTT, CUMPH, DLAYR,DM, DTT,  
     &      GPP, GRAINN, GROLF, GRORT, GROSTM, ICSDUR, ISTAGE, 
     &      ISWNIT, ISWWAT, LAI, LAT, LEAFNO, LFWT, LL, LWMIN, NDEF3, 
     &      NFAC, NLAYR, NH4,NSTRES, NO3, P1, P3, P4, P5, PAF, PANWT, 
     &      PDWI, PGC, PGRORT, PHINT, PLA, PLAN, PLAG, PLAO, PLATO, 
     &      PLAY, PLTPOP, PTF, RANC, RCNP, RLV,ROOTN, ROWSPC, RTWT, 
     &      SAT,SEEDRV, SENLA, SHF, SLAN, SLW, SRAD, 
     &      STMWT, STOVN, STOVWT, SW, SWMAX, SWMIN, SUMDTT, SUMRTR, 
     &      SWFAC, TANC, TBASE, TCNP,TEMF, TEMPM, TDUR, TILN, 
     &      TMAX, TMFAC1, TMIN, TMNC, TRNU,TSIZE, TURFAC,
     &      XN,XSTAGE, EOP, TRWUP, RWUEP1,DYNAMIC,UNO3,UNH4,BD,
     &      PRFTC,RGFIL,PORMIN,PARSR,RUE,SLPF,SATFAC,FSLFW,FSLFN,
     &      ASMDOT,WLIDOT,WSIDOT,WRIDOT,PPLTD,SWIDOT,ISWDIS,SENESCE)

         !Output routines, MZ_OPGROW and MZ_OPNIT are used for 
         !  maize, sorghum and millet.
          CALL MZ_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)

          CALL MZ_OPNIT(CONTROL, ISWITCH,     !Yes, MZ is correct!
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)
       

          CALL SG_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC, WTNCAN, WTNUP, XGNP,      !Input
     &    XLAI, XN, YIELD, YRPLT,                         !Input
     &    BWAH, SDWTAH)                                   !Output

          !IF (ISWDIS.EQ.'Y') THEN
             CALL PEST(CONTROL, ISWITCH, 
     &       AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, CARBO,    !Input
     &       PHTIM, PLTPOP, RTWTO, SLA, SLDOT, SOILPROP,     !Input
     &       SSDOT, STMWTO, TOPWT, WLFDOT, WTLF, YRPLT,      !Input
     &       RLV, SDNO, SHELN, SWIDOT,                       !I/O
     &       VSTAGE, WSHIDT, WTSD, WTSHE,                    !I/O
     &       ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &       SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
          !ENDIF

C--------------------------------------------------------------------
C                     DYNAMIC = SEASINIT
C--------------------------------------------------------------------

      ELSEIF(DYNAMIC.EQ.SEASINIT) THEN

         SWFAC  = 1.0
         TURFAC = 1.0
         NSTRES = 1.0
         SATFAC = 0.0
         WTNUP  = 0.0
         WTNCAN = 0.0
         EARWT  = 0.0

      !--------------------------------------------------------------
      !                   READ INPUT FILES
      !--------------------------------------------------------------

          !----------------------------------------------------------
          !     Read input file name (ie. DSSAT40.INP) and path
          !----------------------------------------------------------
          CALL GETLUN('FILEIO', LUNIO)
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)  
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,0)

          READ(LUNIO,50,IOSTAT=ERR) FILES, PATHSR; LNUM = 7
   50     FORMAT(//////,15X,A12,1X,A80)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

          READ(LUNIO,51,IOSTAT=ERR) FILEE, PATHER; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          READ(LUNIO,51,IOSTAT=ERR) FILEC, PATHCR; LNUM = LNUM + 1
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
   51     FORMAT(15X,A12,1X,A80)

          !----------------------------------------------------------
          !   Read Planting Details Section
          !----------------------------------------------------------
          SECTION = '*PLANT'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
              CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
              READ(LUNIO,60,IOSTAT=ERR) PLTPOP,ROWSPC,SDEPTH
 60           FORMAT(25X,F5.2,13X,F5.2,7X,F5.2)
              LNUM = LNUM + 1
              IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF

          !----------------------------------------------------------
          !          Read crop cultivar coefficients
          !----------------------------------------------------------
          SECTION = '*CULTI'
          CALL FIND(LUNIO, SECTION, LINC, FOUND) ; LNUM = LNUM + LINC
          IF (FOUND .EQ. 0) THEN
              CALL ERROR(SECTION, 42, FILEIO, LNUM)
          ELSE
              READ (LUNIO,1800,IOSTAT=ERR) VARNO,VRNAME,ECONO,
     %                   P1,P2O,P2R,P5,G1,G2
!CHP 1800          FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2))    
1800          FORMAT (A6,1X,A16,1X,A6,1X,6F6.0)    
              LNUM = LNUM + 1
              IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

              READ (LUNIO,'(F6.2)',IOSTAT=ERR) PHINT
              LNUM = LNUM + 1
              IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
          ENDIF

          CLOSE(LUNIO)

C         ***********************************************************
C         ***********************************************************
C 
C                             READ SPECIES FILE
C
C         ***********************************************************
C         ***********************************************************

          FILECC =  TRIM(PATHSR) // FILES
          CALL GETLUN('FILEC', LUNCRP)
          OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
          IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,0)

          !----------------------------------------------------------
          !       Find and Read TEMPERATURE Section
          !----------------------------------------------------------
          SECTION = '*TEMPE'
          CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
          IF (FOUND .EQ. 0) THEN
             CALL ERROR(SECTION, 42, FILECC, LNUM)
          ELSE
             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR)
     &            PRFTC(1),PRFTC(2),PRFTC(3),PRFTC(4)
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(7X,4(1X,F5.2))',IOSTAT=ERR) RGFIL(1),
     &            RGFIL(2),RGFIL(3),RGFIL(4)
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          ENDIF
          REWIND(LUNCRP)

          !----------------------------------------------------------
          !         Find and Read PHOTOSYNTHESIS section
          !----------------------------------------------------------
          SECTION = '*PHOTO'
          CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
          IF (FOUND .EQ. 0) THEN
              CALL ERROR(SECTION, 42, FILECC, LNUM)
          ELSE
              CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
              READ(C80,'(8X,F6.3))',IOSTAT=ERR) PARSR
              IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

              CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
              READ(C80,'(7X,10(1X,F5.0))',IOSTAT=ERR) CO2X(1),CO2X(2),
     &             CO2X(3), CO2X(4),CO2X(5), CO2X(6),CO2X(7),CO2X(8),
     &             CO2X(9),CO2X(10)
              IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

              CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
              READ(C80,'(7X,10(1X,F5.2))',IOSTAT=ERR) CO2Y(1),CO2Y(2),
     &             CO2Y(3), CO2Y(4),CO2Y(5), CO2Y(6),CO2Y(7),CO2Y(8),
     &             CO2Y(9),CO2Y(10)
              IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

           ENDIF
           REWIND(LUNCRP)

           !---------------------------------------------------------
           !        Find and Read Stress Response
           !---------------------------------------------------------
           SECTION = '*STRES'
           CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
           IF (FOUND .EQ. 0) THEN
              CALL ERROR(SECTION, 42, FILECC, LNUM)
           ELSE
              CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
              READ(C80,'(9X,F6.3))',IOSTAT=ERR) FSLFW
              IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(9X,F6.3))',IOSTAT=ERR) FSLFN
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          ENDIF
          REWIND(LUNCRP)

          !----------------------------------------------------------
          !        Find and Read Seed Growth Parameters
          !----------------------------------------------------------
          SECTION = '*SEED '
          CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
          IF (FOUND .EQ. 0) THEN
             CALL ERROR(SECTION, 42, FILECC, LNUM)
          ELSE
             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(8X,F6.3))',IOSTAT=ERR) DSGT
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(8X,F6.3))',IOSTAT=ERR) DGET
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(8X,F6.3))',IOSTAT=ERR) SWCG
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          ENDIF    
          REWIND(LUNCRP)

          !----------------------------------------------------------
          !        Find and Read Root parameters
          !----------------------------------------------------------
          SECTION = '*ROOT '
          CALL FIND(LUNCRP, SECTION, LNUM, FOUND)
          IF (FOUND .EQ. 0) THEN
             CALL ERROR(SECTION, 42, FILECC, LNUM)
          ELSE
             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(9X,F6.3))',IOSTAT=ERR) PORMIN
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(9X,F6.3))',IOSTAT=ERR) RWUMX
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(9X,F6.3))',IOSTAT=ERR) RLWR
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)

             CALL IGNORE(LUNCRP,LNUM,ISECT,C80)
             READ(C80,'(9X,F6.3))',IOSTAT=ERR) RWUEP1
             IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILECC,LNUM)
          ENDIF

          CLOSE (LUNCRP)


C         ***********************************************************
C         ***********************************************************
C 
C                             READ ECOTYPE FILE
C
C         ***********************************************************
C         ***********************************************************


      !--------------------------------------------------------------
      !        Open Ecotype File FILEE
      !--------------------------------------------------------------
        LNUM = 0
        PATHL  = INDEX(PATHER,BLANK)
        IF (PATHL .LE. 1) THEN
          FILEGC = FILEE
        ELSE
          FILEGC = PATHER(1:(PATHL-1)) // FILEE
        ENDIF

          ! Note: This file is hard coded until Gerrit and Cheryl
          !       get a version of CSM built that places the name
          !       of the ecotype file in DSSAT40.INP.
        !------------------------------------------------------------
        !     Read Ecotype Parameter File
        !------------------------------------------------------------
        CALL GETLUN('FILEE', LUNECO)
        OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERRNUM)
        IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,0)
        ECOTYP = '      '
        LNUM = 0

        DO WHILE (ECOTYP .NE. ECONO)
  
          CALL IGNORE(LUNECO, LNUM, ISECT, C255)
          IF (ISECT .EQ. 1 .AND. C255(1:1) .NE. ' ' .AND.
     &          C255(1:1) .NE. '*') THEN
C-GH           READ(C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,TBASE,TOPT,
C-GH &            ROPT,DJTI,GDDE,RUE,KCAN
             READ(C255,3100,IOSTAT=ERRNUM) ECOTYP,ECONAM,TBASE,TOPT,
     &            ROPT,DJTI,GDDE,RUE,KCAN,P3,P4

C-GH 3100         FORMAT (A6,1X,A16,1X,7(1X,F5.1))
3100         FORMAT (A6,1X,A16,1X,7(1X,F5.1),2(1X,F5.0))
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEE,LNUM)
          ELSEIF (ISECT .EQ. 0) THEN
            CALL ERROR(ERRKEY,7,FILEE,LNUM)
          ENDIF
        ENDDO
        CLOSE (LUNECO)
        KEP = KCAN/(1-0.07)*(1-0.25)


         CALL SG_GROSUB (
     &      AGEFAC, BIOMAS, CARBO, CNSD1,CNSD2, CO2X, CO2Y, 
     &      CO2, CSD2, CUMDTT, CUMPH, DLAYR,DM, DTT,  
     &      GPP, GRAINN, GROLF, GRORT, GROSTM, ICSDUR, ISTAGE, 
     &      ISWNIT, ISWWAT, LAI, LAT, LEAFNO, LFWT, LL, LWMIN, NDEF3, 
     &      NFAC, NLAYR, NH4,NSTRES, NO3, P1, P3, P4, P5, PAF, PANWT, 
     &      PDWI, PGC, PGRORT, PHINT, PLA, PLAN, PLAG, PLAO, PLATO, 
     &      PLAY, PLTPOP, PTF, RANC, RCNP, RLV,ROOTN, ROWSPC, RTWT, 
     &      SAT,SEEDRV, SENLA, SHF, SLAN, SLW, SRAD, 
     &      STMWT, STOVN, STOVWT, SW, SWMAX, SWMIN, SUMDTT, SUMRTR, 
     &      SWFAC, TANC, TBASE, TCNP,TEMF, TEMPM, TDUR, TILN, 
     &      TMAX, TMFAC1, TMIN, TMNC, TRNU,TSIZE, TURFAC,
     &      XN,XSTAGE, EOP, TRWUP, RWUEP1,DYNAMIC,UNO3,UNH4,BD,
     &      PRFTC,RGFIL,PORMIN,PARSR,RUE,SLPF,SATFAC,FSLFW,FSLFN,
     &      ASMDOT,WLIDOT,WSIDOT,WRIDOT,PPLTD,SWIDOT,ISWDIS,SENESCE)

      !---------------------------------------------------------------
      !   Subroutine IPPARM reads FILEP, the PEST progress file.
      !---------------------------------------------------------------
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
              STNAME(I) = SGSTGNAM (I)    
          END DO
          
          DO I=1,NLAYR
          ESW(I) = DUL(I) - LL(I)
          END DO

          STGDOY(14) = YRSIM




      !--------------------------------------------------------------
      !             Initialize Variables
      !--------------------------------------------------------------
          CTYPE = 1 

         !Output routines, MZ_OPGROW and MZ_OPNIT are used for 
         !  maize, sorghum and millet.
          CALL MZ_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)

          CALL MZ_OPNIT(CONTROL, ISWITCH,     !Yes, MZ is correct!
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)


          CALL SG_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC, WTNCAN, WTNUP, XGNP,      !Input
     &    XLAI, XN, YIELD, YRPLT,                         !Input
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
          !        Call SG_PHENOL
          !------------------------------------------------------------
        IF (YRDOY .EQ. YRPLT .OR. ISTAGE .NE. 7) THEN               
          IF (CROP .NE. 'FA') THEN

          IF(YRDOY.EQ.YRPLT) ISTAGE = 7

          CALL SG_PHENOL (APTNUP,BIOMAS,BIOMS2,
     &    CSD1, CSD2,
     &    CNSD1, CNSD2, CTYPE, CUMDEP, DLAYR, DTT, 
     &    EMAT, G1, G2, GNUP, GPP, GPSM, GRAINN, GRNWT, ICSDUR, IDETO,
     &    IDUR1, IPRINT, ISDATE, ISTAGE, ISWNIT, ISWWAT,
     &    LAI, LEAFNO, LL, MAXLAI, MDATE, NOUTDO, NLAYR, 
     &    P1, P2O, P2R, P3, P4, P5, P9, PANWT, PGRNWT, PHINT,
     &    PLTPOP, ROPT, RTDEP, SDEPTH, SI1, SI2, SI3,
     &    SI4, SIND, SKERWT, SNOW, SRAD, STGDOY,STMWT, STOVER, STOVN, 
     &    SUMDTT, SW, TANC, TBASE, TEMPCR, TMAX, TMIN, 
     &    TOPT,TOTNUP, TPSM, YIELD, YRDOY,XGNP, XSTAGE,  
C         Variables passed through PHENOL to phasei but not used in phenol
     &    AGEFAC, BIOMS1, CUMDTT, CUMPH, GROLF,
     &    GRORT, GROSTM, LFWT, LWMIN, MGROLF, MGROPAN, MGROSTM, 
     &    MLFWT, MPANWT, MSTMWT, NSTRES, PAF, 
     &    PGC, PLA, PLAN, PLAO, PLATO, PLAY, PLAMX, PTF, RANC, 
     &    RLV, ROOTN, RTWT, RWU, SEEDRV, SENLA, SLAN, 
     &    STOVWT, SUMRTR, SWMAX, SWMIN, TCARBO, TCNP, TDUR, TGROLF,
     &    TGROPAN, TGROSTM, TILN, TILSW, TLFWT, TLNO, TMNC,
     &    TPANWT, TSIZE, TSTMWT, VANC, VMNC,  
     &    XNTI,SWFAC,TURFAC,DGET,SWCG,DJTI, 
     &    DAYL, TWILEN, CANWAA, CANNAA)

          ENDIF
        ENDIF

          !------------------------------------------------------------
          !Call SG_GROSUB
          !------------------------------------------------------------
          IF (ISTAGE .LT. 6) THEN 
 

          CALL SG_GROSUB (
     &      AGEFAC, BIOMAS, CARBO, CNSD1,CNSD2, CO2X, CO2Y, 
     &      CO2, CSD2, CUMDTT, CUMPH, DLAYR,DM, DTT,  
     &      GPP, GRAINN, GROLF, GRORT, GROSTM, ICSDUR, ISTAGE, 
     &      ISWNIT, ISWWAT, LAI, LAT, LEAFNO, LFWT, LL, LWMIN, NDEF3, 
     &      NFAC, NLAYR, NH4,NSTRES, NO3, P1, P3, P4, P5, PAF, PANWT, 
     &      PDWI, PGC, PGRORT, PHINT, PLA, PLAN, PLAG, PLAO, PLATO, 
     &      PLAY, PLTPOP, PTF, RANC, RCNP, RLV,ROOTN, ROWSPC, RTWT, 
     &      SAT,SEEDRV, SENLA, SHF, SLAN, SLW, SRAD, 
     &      STMWT, STOVN, STOVWT, SW, SWMAX, SWMIN, SUMDTT, SUMRTR, 
     &      SWFAC, TANC, TBASE, TCNP,TEMF, TEMPM, TDUR, TILN, 
     &      TMAX, TMFAC1, TMIN, TMNC, TRNU,TSIZE, TURFAC,
     &      XN,XSTAGE, EOP, TRWUP, RWUEP1,DYNAMIC,UNO3,UNH4,BD,
     &      PRFTC,RGFIL,PORMIN,PARSR,RUE,SLPF,SATFAC,FSLFW,FSLFN,
     &      ASMDOT,WLIDOT,WSIDOT,WRIDOT,PPLTD,SWIDOT,ISWDIS,SENESCE)
          ENDIF

          XLAI = LAI

          !------------------------------------------------------------
          !        Call SG_ROOTGR
          !------------------------------------------------------------
          IF (ISWWAT .EQ. 'Y') THEN

          DEPMAX = DS(NLAYR)
          CALL SG_ROOTGR (
     &    CUMDEP, CUMDTT, DEPMAX, DLAYR, DTT,
     &    ESW, GRORT, ISWNIT, LL, NH4, NLAYR, NO3, 
C     &    PLTPOP, RLV, RLWR, RTDEP, SHF, SW, SWFAC)
     & PLTPOP, PORMIN,RLV, RLWR, RTDEP, SAT,SHF, SW, SWFAC)

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
          STGDOY(20) = YREND
        ENDIF

!        IF (YRDOY .GE. YRPLT) THEN
!
!        ENDIF   

      !--------------------------------------------------------------
      !      Compute Biomass variables needed for output routines
      !--------------------------------------------------------------
          TOPWT = (LFWT+STMWT+EARWT)*PLTPOP
          DM = BIOMAS * 10
          STOVWT = LFWT + STMWT
          PTF = (LFWT + STMWT + EARWT)*PLTPOP

          VSTAGE = XN                   !V-stage
          IF (LEAFNO .GT. 0.0) THEN
              RSTAGE = ISTAGE           !R-stage
          ELSE
              RSTAGE = 0
          ENDIF

          WTLF = LFWT*PLTPOP      !g/m2
          STMWTO = STMWT * PLTPOP !g/m2
          RTWTO = RTWT*PLTPOP     !g/m2

!     CHP 02/04/2005 
!          PODWT = EARWT * PLTPOP  !g/m2
          PODWT = PANWT * PLTPOP  !g/m2

c Original 10/1/03          SDWT = PANWT*PLTPOP     !g/m2
          SDWT = PANWT*PLTPOP*0.8

          GPSM = GPP * PLTPOP      !SEED NO/M2
          SEEDNO = GPSM           !SEED NO/M2

          IF(GPP.GT.0.0) THEN
              PODNO = SEEDNO/GPP   !POD (EAR) NO/M2
          ELSE
              PODNO = 0.0
          ENDIF

          IF(WTLF.GT.0) THEN
             SLA = LAI*10000/WTLF    !CM2/G
          ELSE
             SLA = 0
          ENDIF

          TOPWT = BIOMAS
          XLAI = LAI
          XHLAI = LAI
          SHELPC = 0.0
         
          IF(SEEDNO.GT.0) THEN
             SDSIZE = SDWT/SEEDNO*1000
          ELSE
             SDSIZE = 0.0
          ENDIF

          IF(TOPWT.GT.0.AND.SDWT.GE.0) THEN
             HI = SDWT/TOPWT
          ELSE
             HI=0.0
          ENDIF

          IF(TOPWT.GT.0.AND.PODWT.GE.0) THEN
             HIP = PODWT/TOPWT
          ELSE
             HIP = 0.0
          ENDIF

      !--------------------------------------------------------------
      !      Compute Nitrogen variables needed for output routines
      !--------------------------------------------------------------

          WTNCAN = (STOVN+GRAINN)*PLTPOP

          IF( (LFWT+STMWT).GT.0.0) THEN
             WTNLF = STOVN*(LFWT/STOVWT)*PLTPOP
             WTNST = STOVN * (STMWT/(LFWT+STMWT))*PLTPOP
          ELSE
             WTNLF = 0.0
             WTNST = 0.0
          ENDIF

          WTNSD = GRAINN * PLTPOP
c         WTNRT = ROOTN * PLTPOP
          WTNVEG = WTNLF + WTNST
          IF (SDWT.GT.0.0) THEN
             PCNGRN = WTNSD/SDWT*100
          ELSE
             PCNGRN = 0.0
          ENDIF

          IF((WTLF+STMWT).GT.0.0) THEN
             PCNVEG = (WTNLF+WTNST)/(WTLF+(STMWT*PLTPOP))*100
          ELSE
             PCNVEG = 0.0
          ENDIF
      
         WTNUP = WTNUP + TRNU * PLTPOP

         IF(LFWT.GT.0.0.AND.PLTPOP.GT.0.0) THEN
            PCNL = WTNLF/(LFWT*PLTPOP)*100
         ELSE
            PCNL = 0.0
         ENDIF

         IF(STMWT.GT.0.0.AND.PLTPOP.GT.0.0) THEN
            PCNST = WTNST/(STMWT*PLTPOP)*100
         ELSE
            PCNST = 0.0
         ENDIF

         IF(RTWT.GT.0.0) THEN
            PCNRT = ROOTN/RTWT*100
         ELSE
            PCNRT = 0.0
         ENDIF

         !Output routines, MZ_OPGROW and MZ_OPNIT are used for 
         !  maize, sorghum and millet.
         CALL MZ_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)

         CALL MZ_OPNIT(CONTROL, ISWITCH,      !Yes, MZ is correct!
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)

         CALL SG_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC, WTNCAN, WTNUP, XGNP,      !Input
     &    XLAI, XN, YIELD, YRPLT,                         !Input
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
        !Output routines, MZ_OPGROW and MZ_OPNIT are used for 
        !  maize, sorghum and millet.
        CALL MZ_OPGROW(CONTROL, ISWITCH,  
     &    CANHT, CANWH, HI, HIP, MDATE, NLAYR, NSTRES, PCNL, PLTPOP,
     &    PODNO, PODWT, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDSIZE, 
     &    SDWT, SEEDNO, SENESCE, SHELPC, SLA, STMWTO, SWFAC, TOPWT, 
     &    TURFAC, VSTAGE, WTCO, WTLF, WTLO, WTSO, XLAI, YRPLT)

        CALL MZ_OPNIT(CONTROL, ISWITCH,       !Yes, MZ is correct!
     &    YRPLT, MDATE, NLAYR, SENESCE,
     &    WTNCAN,WTNSD,WTNVEG,PCNGRN,PCNVEG,
     &    WTNUP,WTNLF,WTNST,PCNL,PCNST,PCNRT)

        CALL SG_OPHARV(CONTROL, 
     &    AGEFAC, APTNUP, BIOMAS, CANNAA, CANWAA, GNUP,   !Input
     &    GPP, GPSM, HARVFRAC, IDETO, IDETS, IPLTI,       !Input
     &    ISDATE, ISTAGE, MAXLAI, MDATE, NSTRES, PODWT,   !Input
     &    SDWT, SEEDNO, SENESCE, SKERWT, STGDOY, STOVER,  !Input
     &    SWFAC, TOPWT, TURFAC, WTNCAN, WTNUP, XGNP,      !Input
     &    XLAI, XN, YIELD, YRPLT,                         !Input
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
      END SUBROUTINE SG_CERES

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
! CSW         !Cumulative stem growth (g[stem]/m2)
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
! LNGPEG      Not used in CERES except to pass 0 into pest.for
! IDURP       !Duration of ISTAGE 4, calendar days
! LFWT        !Leaf weight, g/plant   
! LL(20)      !Volumetric lower limit of soil water holding capacity, cm3 water/cm3 soil
! LNUM        !Line number in an input datafile
! LUNIO       !Logical unit number for file
! MAXLAI      !Maximum leaf area index, m2/m2
! MDATE       !Maturity data, year and day of year

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
! SGSTGNAM(20)!Array containing names of various growth stages
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

