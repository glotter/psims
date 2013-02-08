C***********************************************************************
C  SoilNBal, Subroutine
C 
C  Purpose: Provides output N balance for plant growth processes 
C     (file PlantN.bal).  Based on old NBAL.FOR, which combined
C     plant and soil N balances.
C
C  REVISION   HISTORY
C  01/01/1995 WTB Written.
C  06/09/1999 AJG Completely revised the soil N and SOM module, and made
C                 a new SOM module based on the CENTURY model.
C                 Also changed the following variable names:
C                  OLD       NEW                  OLD       NEW
C                 ------    ------               ------    ------ 
C                  ANH4      TNH4                 TSON      THUMN
C                  ANH4I     TNH4I                TIFON     TFON
C                  ANO3      TNO3                 TIFONI    TFONI
C                  ANO3I     TNO3I
C  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
C                 modules with CHP's modular structure.
C  03/16/2000 GH  Checked the new modular CROPGRO.
C  06/19/2001 GH  Modified output
C  03/07/2001 CHP Split plant and soil N balances.
C  09/11/2002 CHP Added daily balance when IDETL = 'D'
C***********************************************************************

      SUBROUTINE SoilNBal (CONTROL, ISWITCH, 
     &    ALGFIX, AMTNIT, CUMFNRO, CUMRESN, CUMSENN, HARVRES,!Input
     &    NBUND, NLAYR, RESLEFTN, TFON, THUMN, TLCH, TNH4,!Input
     &    TNO3, TNOX, TOTAML, TOTFLOODN, TUREA, WTNUP)    !Input

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      CHARACTER*1  IDETN, IDETL, ISWNIT
      CHARACTER*12, PARAMETER :: SNBAL = 'SoilNbal.OUT'
      CHARACTER*30 FILEIO

      INTEGER DAS, DYNAMIC, ERRNUM, YRDOY, YRDOYI
      INTEGER YRSIM, RUN, LUNSNC, L, NLAYR, NBUND
      INTEGER YEAR, DOY, YRI, DOYI

      REAL AMTNIT, HARVRESN, RESLEFTN,
     &  RESLEFTNI, CUMRESN, CUMSENN, TALLN, TALLNI, TFON,
     &  TFONI, THUMN, THUMNI, TLCH, TNH4, TNH4I,
     &  TNO3, TNO3I,  TNOX, TORGN, TORGNI,
     &  WTNUP, TUREA, ALGFIX
      REAL TOTAML, CUMFNRO, TOTFLOODN, TOTFLOODNI

      REAL TLCHI, TNOXI, WTNUPI
      REAL HResSurf, HResSoil

      LOGICAL FEXIST

      REAL RESNTODAY, SENNTODAY, LCHTODAY, NOXTODAY
      REAL WTNUPTODAY, AMLTODAY, FNROTODAY, AMTNITTODAY
      REAL CUMRESNY, CUMSENNY, TLCHY, TNOXY, WTNUPY, FLOODNTODAY
      REAL TOTAMLY, CUMFNROY, AMTNITY, FLOODNY
      REAL TOTSTATE, TOTADD, TOTSUB, DAILYBALANCE, TOTSTATY, CUMBAL

!     ------------------------------------------------------------------
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE (ResidueType) HARVRES

!     Return if detail not requested.
      IDETL   = ISWITCH % IDETL
      IDETN   = ISWITCH % IDETN
      ISWNIT  = ISWITCH % ISWNIT
      IF (IDETL  .EQ. 'N' .OR. 
     &    IDETL  .EQ. '0' .OR.    !zero
     &    IDETN  .EQ. 'N' .OR. 
     &    ISWNIT .EQ. 'N') RETURN

      DYNAMIC = CONTROL % DYNAMIC
      DAS     = CONTROL % DAS
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

C***********************************************************************
C***********************************************************************
C     Seasonal Initialization phase
C***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
      !Initialize output file
      CALL GETLUN('SNBAL', LUNSNC)
      INQUIRE (FILE = SNBAL, EXIST = FEXIST)
      IF (FEXIST) THEN
        OPEN (UNIT = LUNSNC, FILE = SNBAL, STATUS = 'OLD',
     &    IOSTAT = ERRNUM, POSITION = 'APPEND')
        WRITE(LUNSNC,'(/,80("="))') 
      ELSE
        OPEN (UNIT = LUNSNC, FILE = SNBAL, STATUS = 'NEW',
     &    IOSTAT = ERRNUM)
        WRITE(LUNSNC,'("*SOIL N BALANCE - CERES SOIL N&C ROUTINES")')
      ENDIF

      CALL HEADER(SeasInit, FILEIO, LUNSNC, RUN)

!       Sum all the soil organic N pools.
        TORGNI = THUMN + TFON + RESLEFTN

        IF (RUN > 1) THEN
!         Harvest residue plus left-over senesced material from 
!         previous crop.
          HResSurf = HARVRES % ResE(0,1)   !surface res N kg[N]/ha
          HResSoil = 0.0                   !soil res N kg[N]/ha
          DO L = 1,NLAYR
            HResSoil = HResSoil + HARVRES % ResE(L,1)
          ENDDO
          HARVRESN = HResSurf + HResSoil
        ELSE
          HResSurf = 0.0
          HResSoil = 0.0 
          HARVRESN = 0.0
        ENDIF

!       Initial value for surface litter.
        RESLEFTNI = RESLEFTN - HResSurf

!       Initial value for soil litter. Because 
!       HARVRESN has been added to FON (and thus is part of TFON),
!       remove it here, so as to present them separately.
        TFONI = MAX(0.0, TFON - HResSoil)

!       Initial value for SOM N, summed across all soil layers.
        THUMNI = THUMN

!       Initial value for extractable N summed across all soil layers.
        TNO3I  = TNO3
        TNH4I  = TNH4

!       Initial value for leached N and denitrified N.
        TLCHI  = TLCH
        TNOXI  = TNOX

!       Initial value for N uptake from soil.
        WTNUPI = WTNUP*10      ! Move multiplier to TALLNI
        WTNUPI = WTNUP

!       Sum the initial value of all abiotic N pools (soil, air)
!       SOM and N uptake (WTNUPI multiplied by 10 to convert
!       from g/m2 to kg/ha).
!       TALLNI = TORGNI + TNO3I + TNH4I + TLCHI + TNOXI + WTNUPI * 10.
        TALLNI = TORGNI + TNO3I + TNH4I

        TOTFLOODNI = 0.0

!       Set starting day of the simulation.
        YRDOYI = YRSIM

!     If detailed printout requested, print daily soil N balance
      IF (IDETL .EQ. 'D') THEN
!       Cumulative values (yesterday)
        CUMRESNY = 0.0
        CUMSENNY = 0.0
        TLCHY    = 0.0
        TNOXY    = 0.0
        WTNUPY   = 0.0
        TOTAMLY  = 0.0
        CUMFNROY = 0.0
        AMTNITY  = 0.0
        FLOODNY  = 0.0
        CUMBAL   = 0.0
!        TOTSTATY = TOTSTATY + HARVRESN

        TOTSTATY = THUMN + TFON + TNO3 + TNH4 + RESLEFTN + TUREA
        
        WRITE(LUNSNC,10)
   10     FORMAT(17X,' ------------------  STATE VARIABLES TODAY  ---',
     &    '----------------   ---- ADDED TODAY ----     ---------- ',
     &    'REMOVED TODAY ----------     DAILY     CUMUL',/,
     &    '@YEAR DOY  DAS      HUMN     FON     NO3     NH4   TUREA',
     &    '    RESN  FLOODN  ALGAEN    RESN    SENN    FERT     LCH',
     &    '     NOX     NUP     AML     NRO   BALANCE   BALANCE')
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!     ------------------------------------------------------------------
      IF (IDETL .EQ. 'D') THEN
        !Compute daily rates from cumulative values
        RESNTODAY = CUMRESN - CUMRESNY
        SENNTODAY = CUMSENN - CUMSENNY
        LCHTODAY  = TLCH - TLCHY
        NOXTODAY = TNOX - TNOXY
        WTNUPTODAY = (WTNUP - WTNUPY) * 10.
        AMLTODAY = TOTAML - TOTAMLY
        FNROTODAY = CUMFNRO - CUMFNROY
        AMTNITTODAY = AMTNIT - AMTNITY
        FLOODNTODAY = TOTFLOODN - FLOODNY


        TOTSTATE = THUMN + TFON + TNO3 + TNH4 + RESLEFTN + ALGFIX
     &                + TOTFLOODN + TUREA
        TOTADD   = RESNTODAY + SENNTODAY + AMTNITTODAY
        TOTSUB   = LCHTODAY  + NOXTODAY  + WTNUPTODAY + AMLTODAY + 
     &                FNROTODAY
        DAILYBALANCE = TOTSTATE - TOTSTATY - TOTADD + TOTSUB
        CUMBAL   = CUMBAL + DAILYBALANCE

        CALL YR_DOY(YRDOY, YEAR, DOY)
!       Write daily output to SOILNBAL.OUT.
        WRITE (LUNSNC,50) YEAR, DOY, DAS, THUMN, TFON, TNO3,
     &      TNH4, TUREA, RESLEFTN, TOTFLOODN, ALGFIX, RESNTODAY, 
     &      SENNTODAY, AMTNITTODAY, LCHTODAY, NOXTODAY, WTNUPTODAY, 
     &      AMLTODAY, FNROTODAY, DAILYBALANCE, CUMBAL
   50     FORMAT(I5, I4.3, I5, 1X, F9.2, 15F8.3, 2F10.3)

!       Save today's cumulative values for use tomorrow
        CUMRESNY = CUMRESN
        CUMSENNY = CUMSENN
        TLCHY    = TLCH
        TNOXY    = TNOX
        WTNUPY   = WTNUP
        TOTAMLY  = TOTAML
        CUMFNROY = CUMFNRO
        AMTNITY  = AMTNIT
        TOTSTATY = TOTSTATE
      ENDIF

!***********************************************************************
!***********************************************************************
!     Output phase
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
!     ------------------------------------------------------------------
!     N balance will be off by the amount of N uptake on the last day 
!       of season because this amount has been added to the N uptake by
!       plant, but has not been subtracted from soil.  This is because
!       soil processes are computed before plant processes.
!     May want to subtract this amount from balance?

        CALL YR_DOY(YRDOYI, YRI, DOYI)
        CALL YR_DOY(YRDOY, YEAR, DOY)
!       Add the fertilizer N to the initial N pool. Also add the N from
!       organic residues applied during the growth period and sum this
!       with the initial TALLNI to make the balance fit with the final
!       TALLN. SEEDNI is not needed, because for the plant the NBAL
!       only deals with N uptake from the soil.
        TALLNI  = TALLNI + AMTNIT + CUMRESN + CUMSENN !Initial + add

!       Sum all the soil organic N pools.
        TORGN = THUMN + TFON + RESLEFTN

!       Sum the initial value of all abiotic N pools (soil, air,
!       fertilizer), SOM and N uptake (WTNUP multiplied by 10 to
!       convert from g/m2 to kg/ha). Deduct the N in senesced material
!       from the N removed by the plant, because senesced material has
!       been returned to the soil.
        TALLN = TORGN + TNO3 + TNH4 + TUREA +        !State end of day
     &          TLCH + TNOX + WTNUP * 10. + TOTAML   !Losses

!       Write output to NBAL.OUT.
        WRITE (LUNSNC,100) YRI, DOYI, YEAR, DOY

        WRITE (LUNSNC, 200) THUMNI, THUMN, TFONI, TFON, 
     &    RESLEFTNI, RESLEFTN, TNO3I, TNO3, TNH4I, TNH4, 0.0, TUREA

        IF (NBUND .GT. 0) THEN
          WRITE(LUNSNC,300) TOTFLOODNI, TOTFLOODN, 0.0, ALGFIX
          TALLN = TALLN + TOTFLOODN + ALGFIX
        ENDIF

        WRITE (LUNSNC,600) HARVRESN, CUMRESN, 
     &    CUMSENN, AMTNIT, TLCH, TNOX, WTNUP * 10., TOTAML

        IF (NBUND .GT. 0) THEN
          TALLN = TALLN + CUMFNRO     !Factor in runoff over bund
          WRITE(LUNSNC,700) CUMFNRO
        ENDIF

        WRITE(LUNSNC,800) TALLNI, TALLN

        CLOSE (UNIT = LUNSNC)

100     FORMAT (//,T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     &    /,T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &    /,'SOIL N BALANCE',T49,'-----------kg N/ha-----------')

200     FORMAT (/, 3X, 'Soil Humus N',    T48, F10.2, 10X, F10.2,
     &     /, 3X, 'Soil Litter N',        T48, F10.2, 10X, F10.2,
     &     /, 3X, 'N in Surface Residue', T48, F10.2, 10X, F10.2,
     &     /, 3X, 'Soil NO3',             T48, F10.2, 10X, F10.2,
     &     /, 3X, 'Soil NH4',             T48, F10.2, 10X, F10.2,
     &     /, 3X, 'Soil Urea',            T48, F10.2, 10X, F10.2)

300     FORMAT (3X,'Flood water N',       T48, F10.2, 10X, F10.2,
     &     /, 3X, 'Algae N',              T48, F10.2, 10X, F10.2)

600     FORMAT (
     &     /, 3X, 'Added to / Removed from Soil:'
     &     /, 3X, 'N in Harvest Residues from Previous Crop',
     &                                            T48, F10.2,
     &     /, 3X, 'N from Organic Applications',  T48, F10.2,
     &     /, 3X, 'N from senesced plant matter', T48, F10.2, 
     &     /, 3X, 'Fertilizer N',                 T48, F10.2,
     &     /, 3X, 'Leached NO3',                  T68, F10.2,
     &     /, 3X, 'N Denitrified',                T68, F10.2,
     &     /, 3X, 'N Uptake From Soil',           T68, F10.2,
     &     /, 3X, 'Ammonia volatilization',       T68, F10.2)

700     FORMAT (3X,'N in runoff over bund',       T68, F10.2)

800     FORMAT (/,3X, 'Total N balance',  T48, F10.2, 10X,F10.2)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilNBal


!     Save old formats - good for debugging
!200     FORMAT (/, 3X, 'Soil Humus N (THUMN)',  T48, F10.2, 10X, F10.2,
!     &     /, 3X, 'Soil Litter N (TFON)',       T48, F10.2, 10X, F10.2,
!     &     /, 3X, 'N in Surface Residue (RESLEFTN)', 
!     &                                          T48, F10.2, 10X, F10.2,
!     &     /, 3X, 'Soil NO3 (TNO3)',            T48, F10.2, 10X, F10.2,
!     &     /, 3X, 'Soil NH4 (TNH4)',            T48, F10.2, 10X, F10.2)
!
!300     FORMAT (3X,'Flood water N (TOTFLOODN)', T48, F10.2, 10X, F10.2)
!
!600     FORMAT (
!     &     /, 3X, 'Added to / Removed from Soil:'
!     &     /, 3X, 'N in Harvest Residues from Previous Crop (HARVRESN)',
!     &     /, 3X,                                           T48, F10.2,
!     &     /, 3X, 'N from Organic Applications (CUMRESN)' , T48, F10.2,
!     &     /, 3X, 'N from senesced plant matter (CUMSENN)', T48, F10.2, 
!     &     /, 3X, 'Fertilizer N (AMTNIT)',                  T48, F10.2,
!     &     /, 3X, 'Leached NO3 (TLCH)',                     T68, F10.2,
!     &     /, 3X, 'N Denitrified (TNOX)',                   T68, F10.2,
!     &     /, 3X, 'N Uptake From Soil (WTNUP*10.)',         T68, F10.2,
!     &     /, 3X, 'Ammonia volatilization (TOTAML)',        T68, F10.2)
!
!700     FORMAT (3X,'N in runoff over bund (CUMFNRO)',       T68, F10.2)

