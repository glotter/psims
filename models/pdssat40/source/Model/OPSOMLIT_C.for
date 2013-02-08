!***********************************************************************
!  OPSOMLIT_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Prints daily output of SOM and litter variables with all
!           variables on one line per day (easy for importing in a
!           spreadsheet). See SOMLITPRINT for layer-by-layer output.
!
!  Revision history:
!  01/01/2000 AJG written
!  06/23/2003 CHP split into two files and only print layers 0 thru 5.  
!                 Most text editors can't handle wide files.
!  10/17/2005 CHP Revised output to include all surface pools and total 
!                 SOM + LIT C and N
!                 Added total soil C at 0-20cm and 20-40cm depth.
!
!  Called: CENTURY
!  Calls: --
!***********************************************************************

      SUBROUTINE OPSOMLIT_C (CONTROL, IDETL, 
     &  ACCCO2, CUMRES, CUMRESE, LITC, LITE, METABC,      !Input
     &  METABE, SOILPROP, SOM1C, SOM1E, SOM2C, SOM2E,     !Input
     &  SOM3C, SOM3E, SomLitC, SomLitE, STRUCC, STRUCE,   !Input
     &  TLITC, TLITE, TMETABC, TMETABE, TSOM1C,           !Input
     &  TSOM1E, TSOM2C, TSOM2E, TSOM3C, TSOM3E,           !Input
     &  TSOMC, TSOME, TSTRUCC, TSTRUCE)                   !Input

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------

      CHARACTER*1 IDETL, RNMODE
      CHARACTER*8 LayerText(5)
      CHARACTER*11 OUTSOMC, OUTSOMN
      CHARACTER*30 FILEIO

      INTEGER DAS, DYNAMIC, FROP, L
      INTEGER NOUTDC, NOUTDN, RUN, YRDOY, YEAR, DOY, REPNO
      INTEGER, PARAMETER :: N  = 1
      INTEGER, PARAMETER :: SRFC = 0, SOIL = 1
      INTEGER ZT(5), ZB(5), NLAYR
      REAL, DIMENSION(5) :: SL, S1, S2, S3, LIT, MET, STR

      REAL CUMRES, TLITC, TMETABC, TSOM1C, TSOM2C,
     &  TSOM3C, TSOMC, TSTRUCC
      REAL SOC_20CM, SOC_20CM_P, SOC_40CM, SOC_40CM_P, FRAC
      REAL SOIL_20CM, SOIL_40CM

      REAL ACCCO2(0:1), CUMRESE(3), TLITE(3), TMETABE(3), 
     &  TSOM1E(3), TSOM2E(3), TSOM3E(3), TSOME(3), TSTRUCE(3)

      REAL LITC(0:NL), METABC(0:NL), SOM1C(0:NL), SOM2C(NL), SOM3C(NL),
     &  STRUCC(0:NL)  !, SSOMC(0:NL)
      REAL BD(NL), DLAYR(NL), DS(NL)

      REAL LITE(0:NL,3), METABE(0:NL,3), SOM1E(0:NL,3), SOM2E(NL,3),
     &  SOM3E(NL,3), STRUCE(0:NL,3)   !, SSOME(0:NL,3)
      REAL SomLitC(0:NL), SomLitE(0:NL,NELEM)

      LOGICAL FEXIST

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType)    SOILPROP

!     IDETL = 'N' or '0' (zero) -- supress output
      IF (INDEX('N0',IDETL) > 0) RETURN

!     Transfer values from constructed data types into local variables.
      DAS     = CONTROL % DAS
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      FROP    = CONTROL % FROP
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      REPNO   = CONTROL % REPNO
      YRDOY   = CONTROL % YRDOY

      BD    = SOILPROP % BD
      DS    = SOILPROP % DS
      DLAYR = SOILPROP % DLAYR
      NLAYR = SOILPROP % NLAYR

!***********************************************************************
!***********************************************************************
!     INPUT + RUNINIT
!***********************************************************************
      IF (DYNAMIC == RUNINIT) THEN
!     ------------------------------------------------------------------
!     SOM daily output file
      OUTSOMC = 'SOMLITC.OUT'
      OUTSOMN = 'SOMLITN.OUT'
      CALL GETLUN('OUTSOMC', NOUTDC)
      CALL GETLUN('OUTSOMN', NOUTDN)

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
      ELSEIF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
!     Open SOMC output file and print headers.
      INQUIRE (FILE = OUTSOMC, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTDC, FILE = OUTSOMC, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTDC, FILE = OUTSOMC, STATUS = 'NEW')
        WRITE(NOUTDC,'("*SOM C DAILY OUTPUT FILE")')
      ENDIF

!     Open SOM-N output file and print headers.
      INQUIRE (FILE = OUTSOMN, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = NOUTDN, FILE = OUTSOMN, STATUS = 'OLD',
     &    POSITION = 'APPEND')
      ELSE
        OPEN (UNIT = NOUTDN, FILE = OUTSOMN, STATUS = 'NEW')
        WRITE(NOUTDN,'("*SOM N DAILY OUTPUT FILE")')
      ENDIF

      IF (RNMODE .NE. 'Q' .OR. RUN == 1) THEN
!       Write headers for variable names.
!       NB: Switched off: Writing headers every season interupts the file
!       and makes it more difficult to handle it in a graphics program.

        !For first run of a sequenced run, use replicate
        ! number instead of run number in header.
        IF (RNMODE == 'Q') THEN
          CALL HEADER(SEASINIT, FILEIO, NOUTDC, REPNO)
          CALL HEADER(SEASINIT, FILEIO, NOUTDN, REPNO)
        ELSE
          CALL HEADER(SEASINIT, FILEIO, NOUTDC, RUN)
          CALL HEADER(SEASINIT, FILEIO, NOUTDN, RUN)
        ENDIF

!       Establish soil layer depths for headers
        DO L = 1, 5
          SELECT CASE(L)
          CASE (1)
            ZT(1) = 0
            ZB(1) = NINT(DS(1))
            WRITE(LayerText(L),'(5X,I1,"-",I1)') ZT(L), ZB(L)
          CASE (2)
            ZT(L) = ZB(L-1)
            ZB(L) = NINT(DS(L))
            WRITE(LayerText(L),'(4X,I1,"-",I2)') ZT(L), ZB(L)
          CASE (3,4)
            ZT(L) = ZB(L-1)
            ZB(L) = NINT(DS(L))
            WRITE(LayerText(L),'(3X,I2,"-",I2)') ZT(L), ZB(L)
          CASE(5) !Layer 5 includes all soil layers below the 4th
            ZT(5) = ZB(4)
            ZB(5) = DS(NLAYR)
            WRITE(LayerText(L),'(2X,I2,"-",I3)') ZT(L), ZB(L)
          END SELECT
        ENDDO

        WRITE (NOUTDC, 100) (LayerText(L), L=1,5),(LayerText(L), L=1,5),
     &                      (LayerText(L), L=1,5),(LayerText(L), L=1,5),
     &                      (LayerText(L), L=1,5),(LayerText(L), L=1,5),
     &                      (LayerText(L), L=1,5)
        WRITE (NOUTDC, 200)

        WRITE (NOUTDN, 110) (LayerText(L), L=1,5),(LayerText(L), L=1,5),
     &                      (LayerText(L), L=1,5),(LayerText(L), L=1,5),
     &                      (LayerText(L), L=1,5),(LayerText(L), L=1,5),
     &                      (LayerText(L), L=1,5)
        WRITE (NOUTDN, 210)

      ENDIF

!***********************************************************************
!***********************************************************************
!     OUTPUT
!***********************************************************************
      ELSEIF (DYNAMIC == OUTPUT .AND. (MOD(DAS,FROP) == 0)) THEN
!     ------------------------------------------------------------------
!     Write daily output on SOM and litter.
      CALL YR_DOY(YRDOY, YEAR, DOY) 

!     Carbon
!     Layer 5 includes all SOM and Lit C to bottom of profile
      SL = 0.; S1 = 0.; S2 = 0.; S3 = 0.; LIT = 0.; MET = 0.; STR = 0.
      DO L = 1, NLAYR
        SELECT CASE(L)
        CASE(1:5)
          SL(L)  = SomLitC(L)
          S1(L)  = SOM1C(L)
          S2(L)  = SOM2C(L)
          S3(L)  = SOM3C(L)
          LIT(L) = LITC(L)
          MET(L) = METABC(L)
          STR(L) = STRUCC(L)
        CASE(6:)
          SL(5)  = SL(5)  + SomLitC(L)
          S1(5)  = S1(5)  + SOM1C(L)
          S2(5)  = S2(5)  + SOM2C(L)
          S3(5)  = S3(5)  + SOM3C(L)
          LIT(5) = LIT(5) + LITC(L)
          MET(5) = MET(5) + METABC(L)
          STR(5) = STR(5) + STRUCC(L)
        END SELECT
      ENDDO

!     Calculate sample carbon from 0-20 cm and from 20-40 cm
!       in kg/ha and percent
      SOC_20CM = SOM1C(0)+ SOM1C(1) + SOM2C(1) + SOM3C(1)
      SOIL_20CM = DLAYR(1) * BD(1) * 1.E5

      SOC_40CM = 0.0
      SOIL_40CM = 0.0

      DO L = 2, NLAYR
        IF (DS(L) <= 20.) THEN
!         Entire layer is in top 20 cm
          SOC_20CM = SOC_20CM + SOM1C(L) + SOM2C(L) + SOM3C(L)
          SOIL_20CM = SOIL_20CM + DLAYR(L) * BD(L) * 1.E5

        ELSEIF (DS(L-1) < 20.) THEN
!         A portion (FRAC) of layer is in top 20 cm
          FRAC = (20. - DS(L-1)) / DLAYR(L)
          SOC_20CM = SOC_20CM + FRAC * (SOM1C(L) + SOM2C(L) + SOM3C(L))
          SOIL_20CM = SOIL_20CM + FRAC * DLAYR(L) * BD(L) * 1.E5

          IF (DS(L) < 40.) THEN
!           The remaining portion (1 - FRAC) is between 20-40cm
            SOC_40CM  = (1. - FRAC) * (SOM1C(L) + SOM2C(L) + SOM3C(L))
            SOIL_40CM = (1. - FRAC) * DLAYR(L) * BD(L) * 1.E5

          ELSE
!           Part of the remaining portion is between 20-40 cm
            FRAC = 20. / DLAYR(L)
            SOC_40CM  = FRAC * (SOM1C(L) + SOM2C(L) + SOM3C(L))
            SOIL_40CM = FRAC * DLAYR(L) * BD(L) * 1.E5
          ENDIF

        ELSEIF (DS(L) <= 40.) THEN
!         The entire layer is between 20-40 cm
          SOC_40CM = SOC_40CM + SOM1C(L) + SOM2C(L) + SOM3C(L)
          SOIL_40CM = SOIL_40CM + DLAYR(L) * BD(L) * 1.E5

        ELSEIF (DS(L-1) < 40.) THEN
!         A portion (FRAC) of layer is between 20-40 cm
          FRAC = (40. - DS(L-1)) / DLAYR(L)
          SOC_40CM = SOC_40CM + FRAC * (SOM1C(L) + SOM2C(L) + SOM3C(L))
          SOIL_40CM = SOIL_40CM + FRAC * DLAYR(L) * BD(L) * 1.E5
        ENDIF
      ENDDO

      SOC_20CM_P = SOC_20CM / SOIL_20CM * 100.
      SOC_40CM_P = SOC_40CM / SOIL_40CM * 100.

      WRITE (NOUTDC, 300) YEAR, DOY, DAS,  
     &    NINT(SOC_20CM), SOC_20CM_P, 
     &    NINT(SOC_40CM), SOC_40CM_P,
     &    NINT(SomLitC(0)+TSOMC+TLITC), NINT(SomLitC(0)), 
     &               NINT(TSOMC+TLITC), (NINT(SL(L)), L=1, 5),
     &    NINT(SOM1C(0)), NINT(TSOM1C), (NINT(S1(L)), L=1, 5),
     &                    NINT(TSOM2C), (NINT(S2(L)), L=1, 5), 
     &                    NINT(TSOM3C), (NINT(S3(L)), L=1, 5),
     &    NINT(LITC(0)),  NINT(TLITC),  (NINT(LIT(L)),L=1, 5), 
     &    NINT(METABC(0)),NINT(TMETABC),(NINT(MET(L)),L=1, 5),
     &    NINT(STRUCC(0)),NINT(TSTRUCC),(NINT(STR(L)),L=1, 5), 
     &    NINT(CUMRES), NINT(ACCCO2(SRFC)), NINT(ACCCO2(SOIL))
  300 FORMAT (1X,I4,1X,I3.3,1X,I5, 2(I8,F8.2), I10, 60(I8))

!     Nitrogen
!     Layer 5 includes all SOM and Lit N to bottom of profile
      SL = 0.; S1 = 0.; S2 = 0.; S3 = 0.; LIT = 0.; MET = 0.; STR = 0.
      DO L = 1, NLAYR
        SELECT CASE(L)
        CASE(1:5)
          SL(L)  = SomLitE(L,N)
          S1(L)  = SOM1E(L,N)
          S2(L)  = SOM2E(L,N)
          S3(L)  = SOM3E(L,N)
          LIT(L) = LITE(L,N)
          MET(L) = METABE(L,N)
          STR(L) = STRUCE(L,N)
        CASE(6:)
          SL(5)  = SL(5)  + SomLitE(L,N)
          S1(5)  = S1(5)  + SOM1E(L,N)
          S2(5)  = S2(5)  + SOM2E(L,N)
          S3(5)  = S3(5)  + SOM3E(L,N)
          LIT(5) = LIT(5) + LITE(L,N)
          MET(5) = MET(5) + METABE(L,N)
          STR(5) = STR(5) + STRUCE(L,N)
        END SELECT
      ENDDO

      WRITE (NOUTDN, 310) YEAR, DOY, DAS, 
     &    NINT(SomLitE(0,N)+TSOME(N)+TLITE(N)), NINT(SomLitE(0,N)), 
     &           NINT(TSOME(N)+TLITE(N)), (NINT(SL(L)), L=1,5),
     &    NINT(SOM1E(0,N)), NINT(TSOM1E(N)),(NINT(S1(L)), L=1, 5), 
     &    NINT(TSOM2E(N)),    (NINT(S2(L)), L=1, 5), 
     &    NINT(TSOM3E(N)),    (NINT(S3(L)), L=1, 5), 
     &    LITE(0,N),   TLITE(N),   (LIT(L), L=1, 5), 
     &    METABE(0,N), TMETABE(N), (MET(L), L=1, 5), 
     &    STRUCE(0,N), TSTRUCE(N), (STR(L), L=1, 5), 
     &    NINT(CUMRESE(N))
  310 FORMAT (1X,I4,1X,I3.3,1X,I5, 27I8, 21F8.2, I8)

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSEIF (DYNAMIC == FINAL) THEN
!     ------------------------------------------------------------------
        CLOSE (NOUTDN)
        CLOSE (NOUTDC)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

100   FORMAT(
     &'!',434(' '),'Cumul.',/,'!',T17,
     &'<---- SOM C in top layers ----> <------------ Total Soil ',
     &'Organic Carbon (SOM + LIT) ------------> ',
     &'<--------- Active Soil Organic Carbon (SOM1) ---------> ',
     &'<---- Intermediate Soil Organic C (SOM2) -----> ',
     &'<------- Passive Soil Organic C (SOM3) -------> ',
     &'<------------------ Litter Carbon --------------------> ',
     &'<-------------- Metabolic Litter Carbon --------------> ',
     &'<------------- Structural Litter Carbon --------------> ',
     &' Resid.  <-Accum. CO2->',/,'!',T17,
     &'20cm-kg  20cm-% 40cm-kg  40cm-%     Total',
     &' Surface    Soil',5A8,   !Total  
     &' Surface    Soil',5A8,   !Active
     &        '    Soil',5A8,   !Intermediate
     &        '    Soil',5A8,   !Passive
     &' Surface    Soil',5A8,   !Litter
     &' Surface    Soil',5A8,   !Metabolic
     &' Surface    Soil',5A8,   !Structural
     &' (kg/ha) Surface    Soil')

110   FORMAT(
     &'!',400(' '),'Cumul.',/,'!',T17,
     &'<---------------- Total Soil Organic Nitrogen ----------',
     &'------> ',
     &'<-------- Active Soil Organic Nitrogen (SOM1) --------> ',
     &'<---- Intermediate Soil Organic N (SOM2) -----> ',
     &'<------- Passive Soil Organic N (SOM3) -------> ',
     &'<----------------- Litter Nitrogen -------------------> ',
     &'<------------- Metabolic Litter Nitrogen -------------> ',
     &'<------------ Structural Litter Nitrogen -------------> ',
     &' Res. N',/,'!',T16,'   Total',
     &' Surface    Soil',5A8,   !Total    
     &' Surface    Soil',5A8,   !Active
     &        '    Soil',5A8,   !Intermediate
     &        '    Soil',5A8,   !Passive
     &' Surface    Soil',5A8,   !Litter
     &' Surface    Soil',5A8,   !Metabolic
     &' Surface    Soil',5A8,   !Structural
     &' (kg/ha)')

200     FORMAT ('@YEAR DOY   DAS',
     &    '  SCS20D  SC%20D  SCS40D  SC%40D      SOCD',
     &    '    SC0D    SCTD    SC1D    SC2D    SC3D    SC4D   SC5+D',
     &    '   S1C0D   S1CTD   S1C1D   S1C2D   S1C3D   S1C4D  S1C5+D',
     &            '   S2CTD   S2C1D   S2C2D   S2C3D   S2C4D  S2C5+D',
     &            '   S3CTD   S3C1D   S3C2D   S3C3D   S3C4D  S3C5+D',
     &    '    LC0D    LCTD    LC1D    LC2D    LC3D    LC4D   LC5+D',
     &    '   MEC0D   MECTD   MEC1D   MEC2D   MEC3D   MEC4D  MEC5+D',
     &    '   STC0D   STCTD   STC1D   STC2D   STC3D   STC4D  STC5+D',
     &    '    RESC   CO20C   CO2SC')

210     FORMAT ('@YEAR DOY   DAS    SOND',
     &    '    SN0D    SNTD    SN1D    SN2D    SN3D    SN4D   SN5+D',
     &    '   S1N0D   S1NTD   S1N1D   S1N2D   S1N3D   S1N4D  S1N5+D',
     &            '   S2NTD   S2N1D   S2N2D   S2N3D   S2N4D  S2N5+D',
     &            '   S3NTD   S3N1D   S3N2D   S3N3D   S3N4D  S3N5+D',
     &    '    LN0D    LNTD    LN1D    LN2D    LN3D    LN4D   LN5+D',
     &    '   MEN0D   MENTD   MEN1D   MEN2D   MEN3D   MEN4D  MEN5+D',
     &    '   STN0D   STNTD   STN1D   STN2D   STN3D   STN4D  STN5+D',
     &    '   RESNC')

      RETURN
      END SUBROUTINE OPSOMLIT_C

!100   FORMAT (
!     &'!Explanation of variable abbreviations (units: all in kg/ha)',/,
!     &'!',78('='),/,
!     &'! SNTD      = soil organic carbon (all SOM pools), summed ',
!     &    'across the whole soil profile.',/,
!     &'! SN1D..5D  = soil organic N (all SOM pools) of soil layer 1..5.'
!     &    ,//,
!     &'! S1NTD     = active soil organic N (SOM1), summed across',
!     &    ' the whole soil profile.',/,
!     &'! S1N0D     = active soil organic N (SOM1) of the surface layer.'
!     &    ,/,
!     &'! S1N1D..5D = active soil organic N (SOM1) of soil layers 1..5.',
!     &    //,
!     &'! S2NTD     = intermediate soil organic N (SOM2), summed across',
!     &    ' the whole soil profile.',/,
!     &'! S2N1D..5D = intermediate soil organic N (SOM2) of soil ',
!     &    'layer 1..5.',//,
!     &'! S3NTD     = passive soil organic N (SOM3), summed across the ',
!     &    'whole soil profile.',/,
!     &'! S3N1D..5D = passive soil organic N (SOM3) of soil layer 1..5.',
!     &    //,
!     &'! LNTD      = litter N, summed across the whole soil profile.',/,
!     &'! LN0D      = litter N of the surface layer.',/,
!     &'! LN1D..5D  = litter N of soil layers 1..5.',//,
!     &'! MENTD     = metabolic litter N, summed across the whole soil ',
!     &    'profile.',/,
!     &'! MEN0D     = metabolic litter N of the surface layer.',/,
!     &'! MEN1D..5D = metabolic litter N of soil layer 1..5.',//,
!     &'! STNTD     = structural litter N, summed across the whole soil',
!     &    ' profile.',/,
!     &'! STN0D     = structural litter N of the surface layer.',/,
!     &'! STN1D..5D = structural litter N of soil layer 1..5.',//,
!     &'! RECNC     = N in cumulative residues applied during the run.'/)
!
!200   FORMAT (
!     &'!Explanation of variable abbreviations (units: all in kg/ha', 
!     &    'except SCTD in tons/ha)',/,
!     &'!',78('='),/,
!     &'! SCTD      = soil organic carbon (all SOM pools), summed ',
!     &    'across the whole soil profile.',/,
!     &'! SC1D..5D  = soil organic carbon (all SOM pools) of soil layer',
!     &    ' 1..5.',//,
!     &'! S1CTD     = active soil organic carbon (SOM1), summed across ',
!     &    'the whole soil profile.',/,
!     &'! S1C0D     = active soil organic carbon (SOM1) of the surface ',
!     &    'layer.',/,
!     &'! S1C1D..5D = active soil organic carbon (SOM1) of soil layer ',
!     &    '1..5.',//,
!     &'! S2CTD     = intermediate soil organic carbon (SOM2), summed ',
!     &    'across the whole soil profile.',/,
!     &'! S2C1D..5D = intermediate soil organic carbon (SOM2) of soil ',
!     &    'layer 1..5.',//,
!     &'! S3CTD     = passive soil organic carbon (SOM3), summed ',
!     &    'across the whole soil profile.',/,
!     &'! S3C1D..5D = passive soil organic carbon (SOM3) of soil layer ',
!     &    '1..5.',//,
!     &'! LCTD      = litter carbon, summed across the whole soil ',
!     &    'profile.',/,
!     &'! LC0D      = litter carbon of the surface layer.',/,
!     &'! LC1D..5D  = litter carbon of soil layer 1..5.',//,
!     &'! MECTD     = metabolic litter carbon, summed across the whole ',
!     &    'soil profile.',/,
!     &'! MEC0D     = metabolic litter carbon of the surface layer.',/,
!     &'! MEC1D..5D = metabolic litter carbon of soil layer 1..5.',//,
!     &'! STCTD     = structural litter carbon, summed across the ',
!     &    'whole soil profile.',/,
!     &'! STC0D     = structural litter carbon of the surface layer.',/,
!     &'! STC1D..5D = structural litter carbon of soil layer 1..5.',//,
!     &'! RECC      = Cumulative residues applied during the run.',/,
!     &'! TCO2C     = Accumulated surface CO2 (kg/ha)  ',/,
!     &'! SCO2C     = Accumulated soil CO2 (kg/ha)   ',/)     

