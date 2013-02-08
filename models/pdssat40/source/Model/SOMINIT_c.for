!***********************************************************************
!  SOMINIT_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine initializes variables and does
!           calculations that only have to be done once at the start
!           of a cycle. Only for CENTURY-based SOM model.
!
!  Revision history:
!  ........       Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG Revised and linked to DSSAT.
!  02/25/2003 CHP Revised initialization when TOTN is limiting.
!  03/26/2003 GH  Modified file name
!  08/12/2003 CHP Added I/O error checking
!                 Write to Warning.out file in addition to screen.
!  09/01/2003 CHP Further revised SOM initialization when TOTN value
!                   is limiting.
!  11/02/2003 AJG Corrected some errors and added explanatory text.
!
!  Called: SOILNI_C
!  Calls : UPCASE
!***********************************************************************


      SUBROUTINE SOMINIT_C (CONTROL,
     &    BD, CES1, CES1M, CES1X, CES2, CES21M, CES21X,   !Input
     &    CES3, CES3M, CES3X, CLAY, CO2S1I, CO2S1S,       !Input
     &    DLAYR, NLAYR, OC, S1S3I, S1S3S, S2S3I, S2S3S,   !Input
     &    SAND, TOTN, TXS1I, TXS1S,                       !Input
     &    ACCMNR, ACCCO2, CO2S1, S1S3, S2S3,              !Output
     &    SOM1C, SOM1E, SOM2C, SOM2E, SOM3C,              !Output
     &    SOM3E, SOMFRACFLAG, TXS1)                       !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE

      INTEGER CHARPOSITION, DYNAMIC, ERRNUM, I, IEL, INIUSE, L, LUN, N, 
     &  NLAYR, SOIL, SRFC, LUNIO, LNUM
      PARAMETER (SRFC = 0, SOIL = 1) 
      PARAMETER (N = 1)

      REAL CO2S1I, CO2S1S, S1S3I, S1S3S, S2S3I,
     &  S2S3S, TXS1I, TXS1S
      REAL SSOME_DEFAULT(0:NLAYR,3), SOMFRACTOT, RATIO, DIFF
      REAL MAX_SOM1E, MIN_SOM1E
      REAL MAX_SOM2E, MIN_SOM2E
      REAL MAX_SOM3E, MIN_SOM3E

!     Variable NL defined in ModuleDefs.for
      REAL ACCCO2(0:1), BD(NL), CLAY(NL), CO2S1(0:NL),
     &  DLAYR(NL), OC(NL), S1S3(NL), S2S3(NL), SAND(NL),
     &  SOM1C(0:NL), SOM2C(NL), SOM3C(NL), SOM1FRAC(NL), SOM2FRAC(NL),
     &  SOM3FRAC(NL), SSOMC(0:NL), TOTN(NL), TXS1(NL)
      REAL ACCMNR(0:NL,3), 
     &  CES1(0:NL,3), CES1M(0:1,3), CES1X(0:1,3), 
     &  CES2(NL,3),   CES21M(1,3),  CES21X(1,3), 
     &  CES3(NL,3),   CES3M(1,3),   CES3X(1,3), 
     &  SOM1E(0:NL,3), SOM2E(NL,3), SOM3E(NL,3), SSOME(0:NL,3)

      CHARACTER*1 UPCASE, USERINPUT
      CHARACTER*5 USERINPUT_LONG
      CHARACTER*6 DUMMY
      CHARACTER*6,  PARAMETER :: ERRKEY  = 'SOMINI'
      CHARACTER*18, PARAMETER :: SOMFILE = 'SOMFRACTIONS_C.SOL'
      CHARACTER*30  FILEIO
      CHARACTER*72 PATHSOL
      CHARACTER*78 MSG(7)
      CHARACTER*90 SOMPF

      CHARACTER*1, PARAMETER :: BLANK = ' '
      INTEGER LENGTH

      LOGICAL SOMFRACFLAG(NL), FEXIST

!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC == RUNINIT) THEN
!     ------------------------------------------------------------------
!       Initialize.
        DO L = 1, NLAYR
          SOMFRACFLAG(L) = .FALSE.
        END DO

!       Open FILEIO to get path to soils directory
!         and user input SOM1, SOM2 and SOM3 initial fractions
        OPEN (UNIT = LUNIO, FILE = FILEIO, STATUS='OLD', IOSTAT=ERRNUM)
        IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,0)
        REWIND (LUNIO)
        READ(LUNIO, '(10/,28X,A72)', IOSTAT=ERRNUM) PATHSOL
        LNUM = 11
        IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

        CLOSE(LUNIO)

        LENGTH  = INDEX (PATHSOL,BLANK)
        IF (LENGTH == 0) THEN
          SOMPF = SOMFILE
        ELSE
          SOMPF = TRIM(PATHSOL) // TRIM(SOMFILE)
        ENDIF

!       Open the SOMFRACTIONS_C.SOL file to read the user-supplied
!       (if any) fractions of SOM1C, SOM2C, SOM3C.
        INQUIRE(FILE = SOMPF, EXIST = FEXIST)
        IF (.NOT. FEXIST) THEN
          CALL ERROR(ERRKEY, 1, SOMFILE, 0)
        ENDIF

        CALL GETLUN('FINPUT', LUN)
        LNUM = 0
        OPEN (UNIT = LUN, FILE = SOMPF, STATUS = 'OLD', IOSTAT=ERRNUM)
        IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

!       Skip the blank lines and text lines.
        DO I = 1, 14
          LNUM = LNUM + 1
          READ (LUN,*, IOSTAT=ERRNUM)
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)
        END DO

!-GH    Need to add better error checking
!       Read the USERINPUT flag and SOM fractions.
        READ (LUN,'(19X,A5)', IOSTAT=ERRNUM) USERINPUT_LONG
        LNUM = LNUM + 1
        IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

        READ (LUN,'(19X,I5)', IOSTAT=ERRNUM) INIUSE
        LNUM = LNUM + 1
        IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

!       Trim USERINPUT_LONG to one character, removing the blanks.
        IF (INDEX(USERINPUT_LONG, 'Y') > 0) THEN   
          CHARPOSITION = INDEX(USERINPUT_LONG, 'Y')
          USERINPUT = USERINPUT_LONG(CHARPOSITION:CHARPOSITION)
        ELSEIF (INDEX(USERINPUT_LONG, 'y') > 0) THEN 
          CHARPOSITION = INDEX(USERINPUT_LONG, 'y')
          USERINPUT = USERINPUT_LONG(CHARPOSITION:CHARPOSITION)
        ELSEIF (INDEX(USERINPUT_LONG, 'N') > 0) THEN  
          CHARPOSITION = INDEX(USERINPUT_LONG, 'N')
          USERINPUT = USERINPUT_LONG(CHARPOSITION:CHARPOSITION)
        ELSEIF (INDEX(USERINPUT_LONG, 'n') > 0) THEN  
          CHARPOSITION = INDEX(USERINPUT_LONG, 'n')
          USERINPUT = USERINPUT_LONG(CHARPOSITION:CHARPOSITION)
        ELSE
!         USERINPUT should be 'Y', 'y', 'N', or 'n'.
          MSG(1) = '************************************************'
          MSG(2) = ' The USERINPUT flag in the SOMFRACTIONS.SOM file'
          MSG(3) = ' should be Y or N, but has a different value.'
          MSG(4) = ' Please correct this and run the model again.'
          MSG(5) = ' '
          MSG(6) = ' Program terminated.'
          MSG(7) = '************************************************'
          CALL WARNING(7, ERRKEY, MSG)
          DO I=1,7
            WRITE(*,*) MSG(I)
          ENDDO
          STOP
        ENDIF

!       Convert to capital letter.
        USERINPUT = UPCASE(USERINPUT)

        IF (USERINPUT == 'Y') THEN
          READ (LUN,*, IOSTAT=ERRNUM)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM) DUMMY, SOM1FRAC(1), SOM1FRAC(2), 
     &      SOM1FRAC(3), SOM1FRAC(4), SOM1FRAC(5), SOM1FRAC(6), 
     &      SOM1FRAC(7), SOM1FRAC(8), SOM1FRAC(9), SOM1FRAC(10)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM) DUMMY, SOM2FRAC(1), SOM2FRAC(2), 
     &      SOM2FRAC(3), SOM2FRAC(4), SOM2FRAC(5), SOM2FRAC(6), 
     &      SOM2FRAC(7), SOM2FRAC(8), SOM2FRAC(9), SOM2FRAC(10)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM) DUMMY, SOM3FRAC(1), SOM3FRAC(2), 
     &      SOM3FRAC(3), SOM3FRAC(4), SOM3FRAC(5), SOM3FRAC(6), 
     &      SOM3FRAC(7), SOM3FRAC(8), SOM3FRAC(9), SOM3FRAC(10)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM) DUMMY, SOM1FRAC(11), SOM1FRAC(12), 
     &      SOM1FRAC(13), SOM1FRAC(14), SOM1FRAC(15), SOM1FRAC(16), 
     &      SOM1FRAC(17), SOM1FRAC(18), SOM1FRAC(19), SOM1FRAC(20)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM) DUMMY, SOM2FRAC(11), SOM2FRAC(12), 
     &      SOM2FRAC(13), SOM2FRAC(14), SOM2FRAC(15), SOM2FRAC(16), 
     &      SOM2FRAC(17), SOM2FRAC(18), SOM2FRAC(19), SOM2FRAC(20)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

          READ (LUN,*, IOSTAT=ERRNUM) DUMMY, SOM3FRAC(11), SOM3FRAC(12), 
     &      SOM3FRAC(13), SOM3FRAC(14), SOM3FRAC(15), SOM3FRAC(16), 
     &      SOM3FRAC(17), SOM3FRAC(18), SOM3FRAC(19), SOM3FRAC(20)
          LNUM = LNUM + 1
          IF (ERRNUM /= 0) CALL ERROR(ERRKEY,ERRNUM,SOMFILE,LNUM)

!         Check whether the fractions sum up to 1.0 (with REAL variables
!         one can only check whether they are between certain limits).
          DO L = 1, NLAYR
            IF (SOM1FRAC(L) + SOM2FRAC(L) + SOM3FRAC(L) < 0.99 .OR. 
     &        SOM1FRAC(L) + SOM2FRAC(L) + SOM3FRAC(L) > 1.01) THEN
              SOMFRACFLAG(L) = .TRUE.
            ENDIF
          END DO
        ENDIF

        CLOSE(LUN)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------

!       Initialize N mineralization accumulators.
        DO L = 0, NLAYR
          DO IEL = 1, NELEM
            ACCMNR(L,IEL) = 0.
          END DO
        END DO

!       Initialize accumulators for microbially respired CO2.
        ACCCO2(SRFC) = 0.
        ACCCO2(SOIL) = 0.

        DO L = 0, NLAYR
!         --------------------------------------------------------------
!         Initialize the SOM pools using Burke's equations.
!         --------------------------------------------------------------
!         Burke's equation for estimating the SOM pool distribution
!         depends on the previous long-term use of the soil. This value
!         is set in Initial Conditions under Previous Crop.

!         Burke's equations only apply to soil layers, so the
!         surface SOM1C is just set to a default value. The various
!         SOM pools are calculated from SSOMC(L).
          IF (L == SRFC) THEN
            SOM1C(SRFC) = 100.

          ELSE   !Soil layers.
!           If the user did not supply the SOM1, SOM2 and SOM3 fractions,
!           use default values, depending on the long-term history of the soil.
            IF (USERINPUT == 'N') THEN
!             Grassland soils.
              IF (INIUSE == 1) THEN
                SOM1FRAC(L) = 0.02
                SOM2FRAC(L) = 0.64
                SOM3FRAC(L) = 0.34
!             Cultivated soils.
              ELSEIF (INIUSE == 2) THEN
                SOM1FRAC(L) = 0.02
                SOM2FRAC(L) = 0.54
                SOM3FRAC(L) = 0.44
              ENDIF
            ENDIF

!           Sum the three SOM fractions and ensure they sum up to 1.0.
            SOMFRACTOT = SOM1FRAC(L) + SOM2FRAC(L) + SOM3FRAC(L)

!           Fractions need to sum to 1.0 - adjust 
            IF (ABS(SOMFRACTOT - 1.0) > 0.0001) THEN
              SOM1FRAC(L) = 1.0 / SOMFRACTOT * SOM1FRAC(L)
              SOM2FRAC(L) = 1.0 / SOMFRACTOT * SOM2FRAC(L)
              SOM3FRAC(L) = 1.0 / SOMFRACTOT * SOM3FRAC(L)
            ENDIF

!           Calculate the total SOMC.
            SSOMC(L) = OC(L) * BD(L) * DLAYR(L) * 1000.

!           Give each SOM pool it share of the total SOMC.
            SOM1C(L) = SSOMC(L) * SOM1FRAC(L)
            SOM2C(L) = SSOMC(L) * SOM2FRAC(L)
            SOM3C(L) = SSOMC(L) * SOM3FRAC(L)
          ENDIF   !End of IF block on L=SRFC

!         ---------------------------------------------------------------
!         Calculate the E content of surface SOM1, soil SOM1, SOM2, SOM3.
!         ---------------------------------------------------------------
!!To be done for P also.
          DO IEL = 1, NELEM
!           Surface layer.
            IF (L == SRFC) THEN
              SOM1E(SRFC,IEL) = SOM1C(SRFC) / CES1(SRFC,IEL)

            ELSEIF (L /= SRFC) THEN
!             Soil layers: before calculating the SOME content of the
!             three  SOM pools with the TOTN values in SOIL.SOL, first
!             calculate what they would be with the default C:E ratios.
              SOM1E(L,IEL) = SOM1C(L) / CES1(L,IEL)   
              SOM2E(L,IEL) = SOM2C(L) / CES2(L,IEL)
              SOM3E(L,IEL) = SOM3C(L) / CES3(L,IEL)
              SSOME_DEFAULT(L,N) =
     &            SOM1E(L,IEL) + SOM2E(L,IEL) + SOM3E(L,IEL)
            ENDIF

!           If there are data on the total soil N content of the layer,
!           use these to initialize SOM N.
            IF (L /= SRFC .AND. IEL == N .AND. TOTN(L) >= 0.001) THEN

!             Total SOM N available based on TOTN:
              SSOME(L,N) = TOTN(L) * BD(L) * DLAYR(L) * 1000.
              
!             If SSOME according to TOTN is less than what is would be
!             with the default C:N ratios, proportion all three pools by
!             the ratio of SSOME as per TOTN to the values calculated
!             from default C:N ratios. because if the E content is too low.
!             it may give negative values later in the code.
              IF (SSOME(L,N) < SSOME_DEFAULT(L,N)) THEN

!               TOTN will limit org. N in this layer. Decrease all the SOM-N
!               pools with the same multiplier.
                RATIO = SSOME(L,N) / SSOME_DEFAULT(L,N)
                SOM1E(L,N) = SOM1E(L,N) * RATIO
                SOM2E(L,N) = SOM2E(L,N) * RATIO
                SOM3E(L,N) = SOM3E(L,N) * RATIO

!               -----
!               SOM3E
!               -----
!               Limit the soil SOM3E between minimum and maximum values.
                MAX_SOM3E = SOM3C(L) / CES3M(SOIL,N)
                MIN_SOM3E = SOM3C(L) / CES3X(SOIL,N)

                IF (SOM3E(L,N) > MAX_SOM3E) THEN
                  DIFF = SOM3E(L,N) - MAX_SOM3E
                  SOM3E(L,N) = MAX_SOM3E
                ELSEIF (SOM3E(L,N) < MIN_SOM3E) THEN
                  DIFF = SOM3E(L,N) - MIN_SOM3E
                  SOM3E(L,N) = MIN_SOM3E
                ELSE
                  DIFF = 0.0
                ENDIF

!               -----
!               SOM2E
!               -----
!               After adjusting SOM3E, give the remaining N to the next
!               most-stable pool: SOM2.
                IF (ABS(DIFF) > 0.001) THEN
                  SOM2E(L,N) = SOM2E(L,N) + DIFF
                ENDIF

!               Limit SOM2E between minimum and maximum values.
                MAX_SOM2E = SOM2C(L) / CES21M(SOIL,N)
                MIN_SOM2E = SOM2C(L) / CES21X(SOIL,N)

                IF (SOM2E(L,N) > MAX_SOM2E) THEN
                  DIFF = SOM2E(L,N) - MAX_SOM2E
                  SOM2E(L,N) = MAX_SOM2E
                ELSEIF (SOM2E(L,N) < MIN_SOM2E) THEN
                  DIFF = SOM2E(L,N) - MIN_SOM2E
                  SOM2E(L,N) = MIN_SOM2E
                ELSE
                  DIFF = 0.0
                ENDIF

!               -----
!               SOM1E
!               -----
!               After adjusting SOM3E and SOM2E, give the remaining N to
!               the last pool: SOM1.
                IF (ABS(DIFF) > 0.001) THEN
                  SOM1E(L,N) = SOM1E(L,N) + DIFF
                ENDIF

!               Limit soil SOM1E between minimum and maximum values.
                MAX_SOM1E = SOM1C(L) / CES1M(SOIL,N)
                MIN_SOM1E = SOM1C(L) / CES1X(SOIL,N)

                IF (SOM1E(L,N) > MAX_SOM1E) THEN
                  DIFF = SOM1E(L,N) - MAX_SOM1E
                  SOM1E(L,N) = MAX_SOM1E
                ELSEIF (SOM1E(L,N) < MIN_SOM1E) THEN
                  DIFF = SOM1E(L,N) - MIN_SOM1E
                  SOM1E(L,N) = MIN_SOM1E
                ELSE
                  DIFF = 0.0
                ENDIF

!               ---------------------------------------------------------
!               Write a warning message if total SOM N had to be adjusted
!               from the TOTN value.
                IF (ABS(DIFF) > 0.001) THEN
                  WRITE(MSG(1),100) TOTN(L), L
                  WRITE(MSG(2),101) 
                  CALL WARNING(2, ERRKEY, MSG)
                ENDIF

              ELSE   !SSOME(L,N) >= SSOME_DEFAULT(L,N)
!               Default C:E ratios will limit org. N in this layer - ignore TOTN
                SSOME(L,N) = SSOME_DEFAULT(L,N)
                WRITE(MSG(1),100) TOTN(L), L
                WRITE(MSG(2),101)
                CALL WARNING(2, ERRKEY, MSG)
              ENDIF   !End IF (L /= SRFC) section.

100           FORMAT("Reported intial value of total N (",F5.2,
     &            "% in layer", I2,") was not used ")
101           FORMAT("because resulting C:N ratios were not within"
     &            " range of acceptable values.")

!           If there are no data on the total soil N content, use the
!           values calculated from default C:N ratios.
            ELSEIF (L /= SRFC .AND. 
     &               IEL == N .AND. TOTN(L) < 0.001) THEN
               SSOME(L,N) = SSOME_DEFAULT(L,N)
            ENDIF   !End of IF block on L=SRFC and TOTN.

!           Recalculate C:E ratios and sum of SOME pools,
            IF (L /= SRFC) THEN
              CES1(L,N) = SOM1C(L) / SOM1E(L,N)
              CES2(L,N) = SOM2C(L) / SOM2E(L,N)
              CES3(L,N) = SOM3C(L) / SOM3E(L,N)
              SSOME(L,N) = SOM1E(L,N) + SOM2E(L,N) + SOM3E(L,N)

              IF (SOM1E(L,N) < 0. .OR. SOM2E(L,N) < 0. .OR.
     &              SOM3E(L,N) < 0.) THEN
!               Print a warning.
                WRITE(MSG(1),
     &            '("SOM1/2/3 < 0, probably due to an unbalance")')
                WRITE(MSG(2),
     &            '("between SLOC and SLNI in the SOIL.SOL file.")')
                CALL WARNING(2, ERRKEY, MSG)
              ENDIF

!!             Calculate C:N ratio of SOM2.
!              IF (SOM2E(L,N) > 0.0001) THEN
!                CNRAT(L) = SOM2C(L) / SOM2E(L,N)
!              ELSE
!                CNRAT(L) = 10.0
!                SOM2E(L,N) = SOM2C(L) / CNRAT(L)
!              ENDIF
!              CNRAT = CES2(L,N)

            ENDIF   !End IF (L /= SRFC) section.
          END DO   !End of IEL loop.


!         --------------------------------------------------------------
!         Some soil-texture-dependent variables that affect the SOM
!         decomposition; the SRFC layer does not have texture.
!         --------------------------------------------------------------
          IF (L /= SRFC) THEN
!           Calculate the effect of soil texture on the soil-microbe
!           (i.e. soil SOM1) decomposition rate.
            TXS1(L)  = TXS1I + TXS1S * SAND(L) / 100.

!           Calculate the C fraction lost as CO2, when soil SOM1
!           decomposes into SOM2.
            CO2S1(L) = CO2S1I + CO2S1S * SAND(L) / 100.

!           Calculate the C fraction transferred to SOM3 when soil SOM1
!           or SOM2 decomposes.
            S1S3(L)  = S1S3I + S1S3S * CLAY(L) / 100.
            S2S3(L)  = S2S3I + S2S3S * CLAY(L) / 100.
          ENDIF   !End of IF block on L /= SRFC
        END DO   !End of soil layer loop.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!     ------------------------------------------------------------------
      RETURN
      END   !SUBROUTINE SOMINIT_C

