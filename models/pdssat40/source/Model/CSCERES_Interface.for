C=======================================================================
C  CSCERES_Interface, Subroutine
C  DSSAT interface for CSCERES Wheat growth routine.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  04/16/2002 LAH/CHP Written.
C  03-12-2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C=======================================================================
      SUBROUTINE CSCERES_Interface (CONTROL, ISWITCH,      !Input
     &     CO2, DAYL, EOP, YREND, NH4, NO3, SNOW, SOILPROP,!Input
     &     SRAD, SRFTEMP, ST, SW, TMAX, TMIN, TRWUP, TWILEN,!Input
     &     CANHT, HARVRES, KCAN, KEP, MDATE, NSTRES,PORMIN,!Output
     &     RLV, RWUMX, SENESCE, STGDOY, UNH4, UNO3, XLAI)  !Output

      USE ModuleDefs
      IMPLICIT NONE
      SAVE

      CHARACTER*1   IDETG, IDETL, IDETO, ISWWAT, ISWNIT, RNMODE
      CHARACTER*2   CROP
      CHARACTER*30  FILEIO
      CHARACTER*78  MESSAGE(10)
      CHARACTER*120 FILEIOCS

      INTEGER DYNAMIC, RUN, TN, RUNI, RN, ON
      INTEGER REP, STEP, CN, YRHAR, YREND, YRDOY
      INTEGER MDATE, L, NLAYR
      INTEGER MULTI, FROP, SN, YEAR, DOY
      INTEGER STGDOY(20), YEARPLT

      REAL WUPT, EOP, TRWUP, SRAD, TMAX, TMIN, CO2
      REAL SNOW, KCAN, KEP, DEPMAX
      REAL NSTRES, XLAI, LAI, NFP
      REAL DAYL, TWILEN, PORMIN, RAIN, RWUMX, SRFTEMP
      REAL CANHT, EO, WINDSP

      REAL, DIMENSION(NL) :: BD, DLAYR, DS, DUL, LL
      REAL, DIMENSION(NL) :: NH4, NO3, RLV, SAT, SHF
      REAL, DIMENSION(NL) :: ST, SW, UNO3, UNH4, UH2O
      REAL, DIMENSION(0:NL) :: SENC, SENN, SENLIG
      REAL, DIMENSION(0:NL) :: CRESC, CRESN, CRESLIG
      REAL, DIMENSION(0:NL) :: SOILTEMP

!     Harvest residue variables 0 = surface
!      INTEGER NELEM       !Number of elements
!      REAL HResWt(0:NL)   !Residue mass (kg[dry matter]/ha)
!      REAL HResLig(0:NL)  !Residue lignin (kg[lignin]/ha)
!      REAL HResE(0:NL,3)  !Residue element components (kg[E]/ha)
!      REAL HARVSUMN       !Total N in harvest residue (kg[N]/ha)
!      INTEGER, PARAMETER :: N = 1, SRFC = 0
!      REAL SENSUMN, sensumntot

      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP
      TYPE (SwitchType)  ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      CROP    = CONTROL % CROP
      FROP    = CONTROL % FROP
      MULTI   = CONTROL % MULTI
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

      ISWWAT  = ISWITCH % ISWWAT
      ISWNIT  = ISWITCH % ISWNIT
      IDETG   = ISWITCH % IDETG
      IDETL   = ISWITCH % IDETL
      IDETO   = ISWITCH % IDETO

      BD     = SOILPROP % BD     
      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS    
      DUL    = SOILPROP % DUL    
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SHF    = SOILPROP % WR

      YRHAR = YREND
      WUPT  = TRWUP

      DEPMAX = DS(NLAYR)

      FILEIOCS(1:30) = FILEIO

      CALL YR_DOY(YRDOY, YEAR, DOY)

!     Print warning if Century soil N routine used
      IF (DYNAMIC .EQ. RUNINIT .AND. ISWITCH % MESOM .EQ. 'P') THEN
        WRITE(MESSAGE(1),100) 
        WRITE(MESSAGE(2),110)  
        WRITE(MESSAGE(3),120) 
        CALL WARNING(3, "WHCER ", MESSAGE)
      ENDIF

  100 FORMAT('You have selected the Century soil nutrient model. ')
  110 FORMAT('The wheat routines have not been calibrated for ')
  120 FORMAT('use with this model.' )

      IF (DYNAMIC .EQ. RUNINIT .OR. DYNAMIC .EQ. SEASINIT) THEN
        TN = 0
        RN = 0
        SN = 0
        ON = 0
        CN = 0  !Crop component
        REP = 1
        STEP = 1
        RUNI = 1

        !Variables not needed
        RAIN = -99.0
        WINDSP = -99.0
        EO = -99.0  
        SOILTEMP(0) = -99.0
        DO l = 1, NL
          SOILTEMP(L) = -99.0
          UH2O(L)=-99.0
        ENDDO

        ! To avoid warning
        DAYL = DAYL 

!      sensumntot = 0.0
      ELSE
        SOILTEMP(0) = SRFTEMP
        DO L = 1, NLAYR
          SOILTEMP(L) = ST(L)
        ENDDO
      ENDIF


C-----------------------------------------------------------------------
      CALL CSCER040 (FILEIOCS, RUN, TN, RN, RNMODE,        !Command line
     & ISWWAT, ISWNIT, IDETO, IDETG, IDETL, FROP,          !Controls
     & SN, ON, RUNI, REP, YEAR, DOY, STEP, CN,             !Run+loop
     & SRAD, TMAX, TMIN, CO2, RAIN,                        !Weather
     & TWILEN, WINDSP, SOILTEMP, EO,                       !Weather
     & NLAYR, DLAYR, DEPMAX, LL, DUL, SAT, BD, SHF,        !Soil states
     & SNOW, SW, NO3, NH4,                                 !H2o,N states
     & YEARPLT,                                            !Pl.date
     & EOP, TRWUP,                                         !Resources
     & LAI, KCAN, KEP,                                     !States
     & RLV, NFP, PORMIN, RWUMX, CANHT,                     !States
     & UNO3, UNH4, UH2O,                                   !Uptake
     & SENC, SENN, SENLIG,                                 !Senescence
     & CRESC, CRESN, CRESLIG,                              !Residue
     & STGDOY,                                             !Stage dates
     & DYNAMIC)                                            !Control 

!      KCAN   = KPAR
!      KEP    = KSRAD
      XLAI   = LAI
      NSTRES = NFP

      IF (STGDOY(11).EQ.YRDOY) THEN
        MDATE = YRDOY
        YREND = YRDOY
      ENDIF 

      if (dynamic .eq. integr) then
        DO L=0, NLAYR
          SENESCE % ResWt(L)  = (SENC(L) + CRESC(L)) / 0.40
          SENESCE % ResLig(L) = SENLIG(L) + CRESLIG(L)
          SENESCE % ResE(L,1) = SENN(L) + CRESN(L)
        ENDDO
      endif
                      
      IF (YREND.EQ.YRDOY .AND. DYNAMIC.EQ.INTEGR) THEN 
        !Transfer harvest residue from senescence variable to 
        !harvest residue variable on day of harvest.
        HARVRES = SENESCE
        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
      ELSE
        MDATE = -99
      ENDIF

      RETURN
      END SUBROUTINE CSCERES_Interface
