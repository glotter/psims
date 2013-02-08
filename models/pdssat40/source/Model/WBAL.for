C=====================================================================
C  WBAL, Subroutine, Gerrit Hoogenboom
C  Seasonally: Provides output Water balance.  Prints file SoilWatBal.OUT
!  Data is obtained from WATBAL, SPAM and IRRIG modules daily.  
!  Data from SPAM and IRRIG are sent via GETPUT routines.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  02/20/1996 GH  Written
C  09/02/1999 CHP Added daily soil water content check
C  08/20/2002 GH  Modified for Y2K
!-----------------------------------------------------------------------
!  Called by: WATBAL
C=====================================================================
      SUBROUTINE Wbal(CONTROL, ISWITCH, 
     &    DRAIN, ES, FLOODWAT, IRRAMT, RAIN, RUNOFF, SNOW,
     &    SWDELTS, SWDELTU, SWDELTX, DLAYR, NLAYR,
     &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)
!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      USE FloodModule
      IMPLICIT NONE
      SAVE

      CHARACTER*1 IDETL, IDETW, ISWWAT
      CHARACTER*14, PARAMETER :: SWBAL = 'SoilWatBal.OUT'
      CHARACTER*30 FILEIO
      INTEGER DAS, DOY, DYNAMIC, LUNWBL
      INTEGER RUN, YEAR, YRSIM, YRDOY, NBUND
      INTEGER YR1, DY1, YR2, DY2

      REAL CEO, CEP, CES, CRAIN, EFFIRR 
      REAL TDRAIN, TOTIR, TRUNOF, TSW, TSWINI
      REAL WBALAN

!     Temporary daily balance
      REAL, DIMENSION(NL) :: DLAYR, SWDELTS, SWDELTX, SWDELTU
      REAL SWDELTSTOT, SWDELTUTOT, SWDELTXTOT
      REAL IRRAMT, ES, EF, RAIN, RUNOFF, TOTEFFIRR
      REAL DRAIN, EP, TSWY    !, INFILT
      REAL CEF, FLOOD, FLOODI, TOTBUNDRO, FRUNOFF, FLOODY
      REAL SNOW, SNOWI, SNOWY, CUMWBAL    !, CINF
      INTEGER NLAYR, L

      LOGICAL FEXIST

!     The variable "ISWITCH" is of type "SwitchType".
      TYPE (ControlType) CONTROL
      TYPE (SwitchType) ISWITCH
      TYPE (FloodWatType) FLOODWAT

!     ------------------------------------------------------------------
      IDETW   = ISWITCH % IDETW
      ISWWAT  = ISWITCH % ISWWAT
      IF (IDETW .EQ. 'N' .OR. ISWWAT .EQ. 'N') RETURN
!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM
      DAS     = CONTROL % DAS
      IDETW   = ISWITCH % IDETW
      IDETL   = ISWITCH % IDETL
      ISWWAT  = ISWITCH % ISWWAT

      EF        = FLOODWAT % EF    
      CEF       = FLOODWAT % CEF    
      FLOOD     = FLOODWAT % FLOOD    
      NBUND     = FLOODWAT % NBUND    
      TOTBUNDRO = FLOODWAT % TOTBUNDRO
      FRUNOFF   = FLOODWAT % FRUNOFF
      !INFILT    = FLOODWAT % INFILT

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN

!     Open output file
      CALL GETLUN('SWBAL', LUNWBL)
      INQUIRE (FILE = SWBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNWBL, FILE = SWBAL, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = LUNWBL, FILE = SWBAL, STATUS = 'NEW')
        WRITE(LUNWBL,'("*WATER BALANCE OUTPUT FILE")')
      ENDIF

      CALL HEADER(SEASINIT, FILEIO, LUNWBL, RUN)

      IF (IDETL .EQ. 'D') THEN
        !Write header for daily output
        WRITE (LUNWBL,1120)
 1120   FORMAT('@YEAR DOY   DAS',
     & '    SWTD    FWTD    SNTD',                    !State variables
     & '   IRRD   PRED',                              !Inflows
!     & '   INFD',            !Exchange between flood and soil water
     & '   DRND   ROFD   FROD   ESAD   EPAD   EFAD',  !Outflows
     & '    WBAL   CUMWBAL',                          !Balance
     & '       TOTS   TOTU   TOTX')   !Changes to soil water
      ENDIF

      TSWY   = TSWINI
      FLOODI = FLOOD
      FLOODY = FLOOD

      !CINF = 0.0
      SNOWI = SNOW
      SNOWY = SNOW

      CUMWBAL = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (IDETL .EQ. 'D') THEN
!       Transfer data from constructed variable to local variables
        CALL GetPutSPAM ("GET", CEO, CEP, CES, EP)
        CALL GetPutIRRIG("GET", EFFIRR, TOTIR)
        CALL YR_DOY(YRDOY, YEAR, DOY) 

!       Change in storage = Inflows - Outflows
!       Balance = Inflows - Outflows - Change in storage
        WBALAN = 
     &         + IRRAMT + RAIN                !Inflows
     &         - DRAIN - RUNOFF - FRUNOFF     !Outflows
     &         - ES - EP - EF                 !Outflows
     &         - (TSW * 10.) + (TSWY * 10.)   !Change in soil water 
     &         - FLOOD       + FLOODY         !Change in flood water 
     &         - SNOW        + SNOWY          !Change in snow accum.

        CUMWBAL = CUMWBAL + WBALAN

        SWDELTSTOT = 0.0
        SWDELTUTOT = 0.0
        SWDELTXTOT = 0.0
        DO L = 1, NLAYR
          SWDELTSTOT = SWDELTSTOT + SWDELTS(L) * DLAYR(L)
          SWDELTUTOT = SWDELTUTOT + SWDELTU(L) * DLAYR(L)
          SWDELTXTOT = SWDELTXTOT + SWDELTX(L) * DLAYR(L)
        ENDDO

        WRITE (LUNWBL,1300) YEAR, DOY, DAS, 
     &    (TSW * 10.), FLOOD, SNOW,                   !State variables
     &    IRRAMT, RAIN,                               !Inflows
!     &    INFILT,                 !Exchange between flood and soil water
     &    DRAIN, RUNOFF, FRUNOFF, ES, EP, EF,         !Outflows
     &    WBALAN, CUMWBAL                             !Balance
     &    ,SWDELTSTOT*10., SWDELTUTOT*10., SWDELTXTOT*10.
! 1300   FORMAT(1X,I4,1X,I3.3,1X,I5,3F8.2, 9F7.2, F8.2, F10.2,4X,3F7.2)
 1300   FORMAT(1X,I4,1X,I3.3,1X,I5,3F8.2, 8F7.2, F8.2, F10.2,4X,3F7.2)

        !Save values for comparison tomorrow
        TSWY   = TSW
        FLOODY = FLOOD
        SNOWY  = SNOW


      ENDIF
      
!      CINF = CINF + INFILT

!***********************************************************************
!***********************************************************************
!     FINAL - Seasonal output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
C-----------------------------------------------------------------------
      YRSIM   = CONTROL % YRSIM
      CALL YR_DOY(YRSIM, YR1, DY1)
      CALL YR_DOY(YRDOY, YR2, DY2)

      CALL GetPutSPAM ("GET", CEO, CEP, CES, EP)
      CALL GetPutIRRIG("GET", EFFIRR, TOTIR)

      IF (EFFIRR .GT. 0.0) THEN
        TOTEFFIRR = EFFIRR * TOTIR
      ELSE
        TOTEFFIRR = TOTIR
      ENDIF

      WRITE (LUNWBL,320)
  320 FORMAT(/,5X,'WATER BALANCE PARAMETERS',
     &       /,5X,'========================',T48,'--mm--')
      WRITE (LUNWBL,400)
     &                   YR1, DY1, TSWINI*10,
     &                   YR2, DY2, TSW*10, TOTEFFIRR,
     &                   CRAIN, TDRAIN, TRUNOF,
     &                   CES, CEP, CEO
  400 FORMAT(
     &       /,5X,'Soil H20 (start) on Year/day',I5,'/',I3.3,T44,F10.2,
     &       /,5X,'Soil H20 (final) on Year/day',I5,'/',I3.3,T44,F10.2,
     &       /,5X,'Effective Irrigation',T44,F10.2,
     &       /,5X,'Precipitation',T44,F10.2,
     &       /,5X,'Drainage',T44,F10.2,
     &       /,5X,'Runoff',T44,F10.2,
     &       /,5X,'Soil Evaporation',T44,F10.2,
     &       /,5X,'Transpiration',T44,F10.2,
     &       /,5X,'Potential ET',T44,F10.2)

      WBALAN = (TOTEFFIRR) + CRAIN - TDRAIN - TRUNOF - 
     &            CES - CEP - (TSW * 10) + (TSWINI * 10)

      IF (SNOW > 0.0 .OR. SNOWI > 0.0) THEN
        WRITE(LUNWBL, 420) SNOWI, SNOW
  420   FORMAT(/,5X,'Initial snow accumulation ', T44, F10.2,
     &         /,5X,'Final snow accumulation ',   T44, F10.2)
        WBALAN = WBALAN - SNOW + SNOWI
      ENDIF

      IF (NBUND .GT. 0) THEN
!        WRITE (LUNWBL,450) FLOODI, FLOOD, CINF, CEF, TOTBUNDRO
        WRITE (LUNWBL,450) FLOODI, FLOOD, CEF, TOTBUNDRO
  450   FORMAT(
     &       /,5X,'Initial flood depth     ',T44,F10.2,
     &       /,5X,'Final flood depth       ',T44,F10.2,
!     &       /,5X,'Cumulative infiltration ',T44,F10.2,
     &       /,5X,'Flood pool evaporation  ',T44,F10.2,
     &       /,5X,'Runoff over bund        ',T44,F10.2)
        WBALAN = WBALAN - FLOOD + FLOODI - CEF - TOTBUNDRO
      ENDIF

      WRITE  (LUNWBL,500) WBALAN
  500 FORMAT(/,5X,'Final Balance ',T42,F12.4,/)

      CLOSE(LUNWBL)       

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE Wbal
C=======================================================================


C=======================================================================
      SUBROUTINE GetPutSPAM(GETPUT, CEO, CEP, CES, EP)
!     Transfer data from SPAM module to WBAL.      
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE

      CHARACTER*3 GETPUT
      REAL CEO, CEP, CES, EP

      !Data which comes from SPAM module
      Type SpamType
        REAL CEO, CEP, CES, EP
      End Type SpamType
      Type (SpamType) SpamData

!-----------------------------------------------------------------------
      IF (GETPUT .EQ. 'GET') THEN
        !Retreive data
        CEO = SpamData % CEO 
        CEP = SpamData % CEP 
        CES = SpamData % CES 
        EP  = SpamData % EP 

      ELSEIF (GETPUT .EQ. 'PUT') THEN
        !Store data
        SpamData % CEO = CEO 
        SpamData % CEP = CEP 
        SpamData % CES = CES 
        SpamData % EP  = EP
      ENDIF

      RETURN
      END SUBROUTINE GetPutSPAM
C=======================================================================

C=======================================================================
      SUBROUTINE GetPutIRRIG(GETPUT, EFFIRR, TOTIR)
!     Transfer data from Irrigation module to WBAL.      
!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE

      CHARACTER*3 GETPUT
      REAL EFFIRR, TOTIR

      !Data which comes from IRRIG module
      Type IrrigType
        REAL EFFIRR, TOTIR
      End Type IrrigType
      Type (IrrigType) IrrigData

!-----------------------------------------------------------------------
      IF (GETPUT .EQ. 'GET') THEN
        !Retreive data
        EFFIRR = IrrigData % EFFIRR 
        TOTIR  = IrrigData % TOTIR 

      ELSEIF (GETPUT .EQ. 'PUT') THEN
        !Store data
        IrrigData % EFFIRR = EFFIRR 
        IrrigData % TOTIR  = TOTIR 
      ENDIF      

      RETURN
      END SUBROUTINE GetPutIRRIG
C=======================================================================


C=====================================================================
!     WBAL VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! CEO      Cumulative potential evapotranspiration (mm)
! CEP      Cumulative transpiration (mm)
! CES      Cumulative evaporation (mm)
! CRAIN    Cumulative precipitation (mm)
! DEFICIT  Amount by which the allowable minimum soil water content in top 
!            layer exceeds the actual calculated soil water content (cm3/cm3)
! DLAYR(L)  Soil thickness in layer L (cm)
! DYNAMIC  Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!            INTEGR, OUTPUT, or FINAL 
! EFFIRR   Irrigation application efficiency (cm/cm)
! ES       Actual soil evaporation rate (mm/d)
! EXPER    Experiment code (prefix of input files) 
! FIRST    Indicates first call to subroutine (true or false)
! LL(L)    Volumetric soil water content in soil layer L at lower limit
!            (cm3/cm3)
! LUNWARN  Logical unit number for Warning.OUT file 
! LUNWBL   Logical unit number for WBAL.OUT file 
! NL       Maximum number of soil layers = 20 
! NLAYR    Actual number of soil layers 
! SAT(L)   Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SW(L)    Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! SWEF     Soil water evaporation fraction; fraction of lower limit content 
!            to which evaporation can reduce soil water content in top layer
!            (fraction)
! TDRAIN   Cumulative daily drainage from profile (mm)
! TOTIR    Total seasonal irrigation (mm)
! TRTNO    Treatment number 
! TRUNOF   Cumulative runoff (mm)
! TSW      Total soil water in profile (cm)
! TSWINI   Initial soil water content (cm)
! WBALAN   Seasonal water balance (should equal 0.0) (mm)
! YRDOY    Current day of simulation (YYDDD)
! YRSIM    Start of simulation date (YYDDD)
!-----------------------------------------------------------------------
!     END SUBROUTINE WBAL
C=======================================================================
