!***********************************************************************
!  SOILNBAL_C, Subroutine for CENTURY-based SOM/residue module.
!
!  Purpose: Provides output N balance (file SoilNBal.OUT)
!
!  REVISION   HISTORY
!  01/01/1995 WTB Written.
!  06/09/1999 AJG Completely revised the soil N and SOM module, and made
!                 a new SOM module based on the CENTURY model.
!                 Also changed the following variable names:
!                 OLD       NEW                  OLD       NEW
!                 ------    ------               ------    ------ 
!                 ANH4      TNH4                 TSON      THUMN
!                 ANH4I     TNH4I                TIFON     TFON
!                 ANO3      TNO3                 TIFONI    TFONI
!                 ANO3I     TNO3I
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!                 modules with CHP's modular structure.
!  03/16/2000 GH  Checked the new modular CROPGRO.
!  06/19/2001 GH  Modified output
!  04/22/2002 AJG Removed the crop-related code, which has become a
!                 separate subroutine PlantNBal.
!  10/29/2002 AJG Getting the balance OK for sequential runs also.
!  07/01/2005 CHP Fixed initialization problem with harvest residue.
!
!  Called: CENTURY
!  Calls : --
!***********************************************************************

      SUBROUTINE SoilNBal_C (CONTROL, ISWITCH,
     &  AMTNIT, CUMRESE, HARVRES, HARVRESE, LITE, NLAYR,  !Input
     &  SENESSUMN, SOM1E, TLCH, TLITE, TNH4, TNO3,        !Input
     &  TNOX, TSOME, TSOM1E, TSOM2E, TSOM3E, WTNUP)       !Input

!***********************************************************************

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL FEXIST

      CHARACTER*1  IDETN, IDETL, ISWNIT
      CHARACTER*12, PARAMETER :: SNBAL = 'SoilNbal.OUT'
      CHARACTER*30  FILEIO

      INTEGER DAS, DOY, DOY1, DOY2, DYNAMIC, ERRNUM, L, LUNSNC, N,
     &  NLAYR, RUN, SRFC, YR, YR1, YR2, YRDOY, YRDOYI, YRSIM
      PARAMETER (N = 1)
      PARAMETER (SRFC = 0)

      REAL AMTNIT, AMTNITI, AMTNITTODAY, AMTNITY, CUMRESNY,
     &  CUMBALE(N), LCHTODAY, NOXTODAY, RESNTODAY,
     &  SENESSUMN, SENESSUMNY, SENNTODAY, TALLN, TALLNI, TLCH, 
     &  TLCHY, TNH4, TNH4I, TNO3, TNO3I, TNOX, TNOXY,
     &  TORGEI(N), TOTADDE(N), 
     &  WTNUP, WTNUPTODAY, WTNUPY

      REAL CUMRESE(3), HARVRESE(3), HResSurfE(3), HResSoilE(3),
     &  TLITE(3), TLITEI(3), TORGE(3),  TOTSOILE(3), TOTSOILEI(3),
     &  TOTSUBE(3),TSOME(3), TSOMEI(3), TSOM1E(3),
     &  TSOM1EI(3), TSOM2E(3), TSOM2EI(3), TSOM3E(3), TSOM3EI(3)

      REAL LITE(0:NL,3), LITEI(0:NL,3), SOM1E(0:NL,3), SOM1EI(0:NL,3)


!     The variable "CONTROL" is of type "ControlType".
      TYPE (ControlType) CONTROL

C     The variable "ISWITCH" is of type "SwitchType".
      TYPE (SwitchType)  ISWITCH
      TYPE (ResidueType) HARVRES

!     ------------------------------------------------------------------
!     Return if detail not requested.
      IDETL   = ISWITCH % IDETL
      IDETN   = ISWITCH % IDETN
      ISWNIT  = ISWITCH % ISWNIT
      IF (IDETL  .EQ. 'N' .OR. 
     &    IDETL  .EQ. '0' .OR.    !zero
     &    IDETN  .EQ. 'N' .OR. 
     &    ISWNIT .EQ. 'N') RETURN

!     ------------------------------------------------------------------
      DYNAMIC = CONTROL % DYNAMIC
      DAS     = CONTROL % DAS
      FILEIO  = CONTROL % FILEIO
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization phase
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!       Initialize output file
        CALL GETLUN('SNBAL', LUNSNC)
        INQUIRE (FILE = SNBAL, EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUNSNC, FILE = SNBAL, STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
          WRITE(LUNSNC,'(/,80("*"))') 
        ELSE
          OPEN (UNIT = LUNSNC, FILE = SNBAL, STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
          WRITE(LUNSNC,'("*SOIL N BALANCE - CENTURY ROUTINES")')
        ENDIF

        CALL HEADER (SEASINIT, FILEIO, LUNSNC, RUN)

!       Initial value for SOM N, summed across all soil layers (not the
!       SRFC layer.
        TSOM1EI(N) = TSOM1E(N)
        TSOM2EI(N) = TSOM2E(N)
        TSOM3EI(N) = TSOM3E(N)
        TSOMEI(N)  = TSOME(N)

!       Initial value for extractable N summed across all soil layers.
        TNO3I  = TNO3
        TNH4I  = TNH4

        IF (RUN > 1) THEN
!         Harvest residue plus left-over senesced material from previous
!         crop.
          HResSurfE(N) = HARVRES % ResE(0,1)   !surface res N kg[N]/ha
          HResSoilE(N) = 0.0                    !soil res N kg[N]/ha
          DO L = 1, NLAYR
            HResSoilE(N) = HResSoilE(N) + HARVRES % ResE(L,1)
          ENDDO
          HARVRESE(N) = HResSurfE(N) + HResSoilE(N)
        ELSE
          HResSurfE(n) = 0.0
          HResSoilE(N) = 0.0 
          HARVRESE(N)  = 0.0
        ENDIF

!       Initial value for soil litter (not surface litter).
!       Subtract residue leftover from previous crop from soil 
!         litter because this was added in previously.
        TLITEI(N) = TLITE(N) - HResSoilE(N)

!       Initial value for surface litter and surface SOM1.
!       Subtract residue leftover from previous crop from surface 
!       litter because this was added in previously.
        LITEI(SRFC,N) = LITE(SRFC,N) - HResSurfE(N)
        SOM1EI(SRFC,N) = SOM1E(SRFC,N)

!       Sum all the soil organic N pools.
        TORGEI(N) = TSOMEI(N) + TLITEI(N) +LITEI(SRFC,N) +SOM1EI(SRFC,N)
!     &    + HResSurfE(N) + HResSoilE(N)

!       Total organic and inorganic N in the soil.
        TOTSOILEI(N) = TORGEI(N) + TNO3I + TNH4I

!       Initial value for leached N and denitrified N.
!       TLCHI  = TLCH
!       TNOXI  = TNOX

!       Initial value for N2 fixed.
!       WTNFXI = WTNFX

!       Initial value for N uptake from soil.
!       WTNUPI = WTNUP

!       Sum the initial value of all abiotic N pools (soil, air)
!       SOM and N uptake (WTNUPI multiplied by 10 to convert
!       from g/m2 to kg/ha).
!       TALLNI = TORGEI(N) + TNO3I + TNH4I + TLCHI + TNOXI + WTNUPI * 10.
!    &    + HARVRESE(N)
        TALLNI = TORGEI(N) + TNO3I + TNH4I !+HResSurfE(N) + HResSoilE(N)

!       Set starting day of the simulation.
        YRDOYI = YRSIM

!     If detailed printout requested, print daily soil N balance
      IF (IDETL .EQ. 'D') THEN

!       Cumulative values (yesterday)
        CUMRESNY   = 0.0
        SENESSUMNY = 0.0
        TLCHY      = 0.0
        TNOXY      = 0.0
        WTNUPY     = 0.0
        AMTNITY    = 0.0
        TOTADDE(N) = 0.0
        TOTSUBE(N) = 0.0
        
        WRITE(LUNSNC,10)
   10   FORMAT(
     &    15X,'  ----------------  STATE VARIABLES TODAY  ---------',
     &    '------   -------- ADDED TODAY --------   ------- REMOVED',
     &    ' TODAY -------   BALANCE',/,
     &    '@YEAR DOY   DAS    TSOMN   TLITN  SRFLIT SRFSOM1     NO3',
     &    '     NH4     SOIL    RESN    SENN    FERT   TOTAL     LCH',
     &    '     NOX     NUP   TOTAL  FROM DAY=0')
      ENDIF

!***********************************************************************
!***********************************************************************
!     Daily output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!     ------------------------------------------------------------------
      IF (IDETL .EQ. 'D') THEN
!       Compute daily rates from cumulative values
        RESNTODAY = CUMRESE(N) - CUMRESNY
        SENNTODAY = SENESSUMN - SENESSUMNY
        AMTNITTODAY = AMTNIT - AMTNITY
        LCHTODAY  = TLCH - TLCHY
        NOXTODAY  = TNOX - TNOXY
        WTNUPTODAY = (WTNUP - WTNUPY) * 10.

!       Total soil organic E and combined with the mineral E.
        TORGE(N) = TSOME(N) + TLITE(N) + LITE(SRFC,N) + SOM1E(SRFC,N)
        TOTSOILE(N) = TORGE(N) + TNO3 + TNH4

!       Additions to and removals from the field.
        TOTADDE(N)  = TOTADDE(N) + AMTNITTODAY + SENNTODAY + RESNTODAY
        TOTSUBE(N) = TOTSUBE(N) + LCHTODAY + NOXTODAY +
     &    WTNUPTODAY

!       What has been added to the field is already part of TOTSOILE,
!       but is additional to the initial N, so that TOTADDE has to be 
!       added to TOTSOILEI. Similarly, TOTSUBE has to be subtracted.
        CUMBALE(N) = TOTSOILE(N) - (TOTSOILEI(N) + TOTADDE(N)
     &    - TOTSUBE(N)) - HARVRESE(N)

        CALL YR_DOY(YRDOY, YR, DOY)

!       Write daily output to SOILNBAL.OUT.
        WRITE (LUNSNC,50) YR, DOY, DAS, TSOME(N), TLITE(N),
     &    LITE(SRFC,N), SOM1E(SRFC,N), TNO3, TNH4, TOTSOILE(N), 
     &    RESNTODAY, SENNTODAY, AMTNITTODAY, TOTADDE(N), 
     &    LCHTODAY, NOXTODAY, WTNUPTODAY, TOTSUBE(N), CUMBALE(N)
     &    
   50   FORMAT(1X, I4, 1X, I3, 1X, I5, 1X, 6F8.2, 1X, 9F8.2, F10.2)

!       Save today's cumulative values for use tomorrow
        CUMRESNY = CUMRESE(N)
        SENESSUMNY = SENESSUMN
        TLCHY    = TLCH
        TNOXY    = TNOX
        WTNUPY   = WTNUP
        AMTNITY  = AMTNIT
      ENDIF

!***********************************************************************
!***********************************************************************
!     Output phase
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
!     ------------------------------------------------------------------

!       Sum all the soil organic N pools.
        TORGE(N) = TSOME(N) + TLITE(N) + LITE(SRFC,N) + SOM1E(SRFC,N)

!       Add the fertilizer N to the initial N pool. Also add the N from
!       organic residues applied during the growth period and sum this
!       with the initial TALLNI to make the balance fit with the final
!       TALLN. Senesced material is included in the final litter pools,
!       and thus can be considered as input on the initial soil-N 
!       content. SEEDNI is not needed, because for the plant the NBAL
!       only deals with N uptake from the soil.
        AMTNITI = AMTNIT
        TALLNI  = TALLNI + AMTNITI + HARVRESE(N) + CUMRESE(N) +SENESSUMN

!       Sum the initial value of all abiotic N pools (soil, air,
!       fertilizer), SOM and N uptake (WTNUP multiplied by 10 to
!       convert from g/m2 to kg/ha).
!       Removed TFERT; AJG.
        TALLN = TORGE(N) + TNO3 + TNH4 + TLCH + TNOX + WTNUP * 10.

!       Write end-of-season output.
        CALL YR_DOY(YRDOYI, YR1, DOY1)
        CALL YR_DOY(YRDOY,  YR2, DOY2)
        WRITE (LUNSNC,100) YR1, DOY1, YR2, DOY2

        WRITE (LUNSNC,200) TSOM1EI(N), TSOM1E(N),
     &    TSOM2EI(N), TSOM2E(N), TSOM3EI(N), TSOM3E(N), TLITEI(N),
     &    TLITE(N), LITEI(SRFC,N), LITE(SRFC,N), SOM1EI(SRFC,N),
     &    SOM1E(SRFC,N)

        WRITE (LUNSNC,300) TORGEI(N), TORGE(N), TNO3I, TNO3, TNH4I,
     &    TNH4, (TNO3I + TNH4I), (TNO3 + TNH4),
     &    HARVRESE(N), CUMRESE(N), AMTNITI, SENESSUMN, TLCH, TNOX, 
     &    WTNUP * 10., TALLNI, TALLN

        CLOSE (UNIT = LUNSNC)

100     FORMAT (//,T42,'INITIAL Year/Doy', T64, 'FINAL Year/Doy', 
     &    /,T49, I5,'/',I3.3, T69,I5,'/',I3.3,
     &    /,'SOIL N BALANCE',T49,'-----------kg N/ha-----------')

200     FORMAT ('  SOIL & SURFACE ORGANIC MATTER N',
     &    /, 3X, 'Active (= microbial) SOM N', T48, F10.2, 10X, F10.2,
     &    /, 3X, 'Intermediate SOM N',         T48, F10.2, 10X, F10.2,
     &    /, 3X, 'Passive SOM N',              T48, F10.2, 10X, F10.2,
     &    /, 3X, 'Soil Litter N',              T48, F10.2, 10X, F10.2,
     &    /, 3X, 'N in Litter on Top of the Soil', T48, F10.2,
     &    10X, F10.2,
     &    /, 3X, 'N in Microbes on Top of the Soil', T48, F10.2,
     &    10X, F10.2,
     &    /, 3X, T48,'  --------',T68,'  --------')

300     FORMAT (3X, 'Total Organic N in Soil and Surface Layers',
     &                       T48, F10.2, 10X, F10.2,
     &    //,'  SOIL INORGANIC N',
     &    /,3X, 'Soil NO3', T48, F10.2, 10X, F10.2,
     &    /, 3X, 'Soil NH4', T48, F10.2, 10X, F10.2,
     &    /, 3X, T48,'  --------',T68,'  --------',
     &    /, 3X, 'Total Inorganic N in Soil', T48, F10.2, 10X, F10.2,
     &    //,'  ADDITIONS AND REMOVALS:'
     &    /, 3X, 'N in Harvest Residues from Previous Crop',
     &    /, 3X, '  (sequential runs only)', T48, F10.2,
     &    /, 3X, 'N from Organic Applications', T48, F10.2,
     &    /, 3X, 'Fertilizer N', T48, F10.2,
     &    /, 3X, 'N in returned senesced material', T48, F10.2,
     &    /, 3X, 'Leached NO3', T48, 20X, F10.2,
     &    /, 3X, 'N Denitrified', T48, 20X, F10.2,
     &    /, 3X, 'N Uptake From Soil', T48, 20X, F10.2,
     &    /, 3X, T48,'  --------',T68,'  --------',
     &    /, '  TOTAL N BALANCE', T48, F10.2, 10X, F10.2)

!***********************************************************************
!***********************************************************************
!     End of DYNAMIC IF construct
!***********************************************************************
      END IF

      RETURN
      END SUBROUTINE SoilNBal_C
