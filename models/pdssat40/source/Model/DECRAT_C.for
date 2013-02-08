!***********************************************************************
!  DECRAT_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine calculates the effect of soil temperature
!           and soil water content on the decomposition rate
!           parameter.
!
!  Revision history:
!  ........ Parton et al.  Written for CENTURY model; the soil water
!                          section was written in the C language for
!                          the daily version of CENTURY.
!  01/01/99 AJG  Revised and linked to DSSAT.
!  25/05/99 AJG  Converted the soil water section to FORTRAN and
!                linked it to DSSAT.
!  01/01/00 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!
!  Called: CENTURY
!  Calls : Function ATANF_C
!***********************************************************************

      SUBROUTINE DECRAT_C (CONTROL, 
     &  BD, CLAY, DLAYR, DUL, EO, L, LL, NLAYR,           !Input
     &  SAT, SILT, SRFTEMP, ST, SW, WINF,                 !Input
     &  DEFAC)                                            !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      CHARACTER TEXTURE*6

      INTEGER DYNAMIC, L, NLAYR, SRFC, YRDOY, YRSIM, NSWI
      PARAMETER (SRFC = 0)

      REAL ATANF_C, BASE1, BASE2, DBLVAL, E1, E2, EO, EOY, NORMALIZER,
     &  SRFTEMP, TFSOM, WATERFILLED, WFSOM, WINF, WINFY, SWEF, XL, RWC

      REAL A(NL), AD(NL), B(NL), BD(NL), C(NL), CLAY(NL), D(NL),
     &  DEFAC(0:NL), DLAYR(NL), LL(NL), SILT(NL), ST(NL), SW(NL),
     &  SWY(NL), DUL(NL), SAT(NL)

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

!***********************************************************************
!***********************************************************************
!     RUNINIT + SEASONAL INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
        DO L = 1, NLAYR
!         Set the soil texture class: coarse if sand >30%, fine if
!         clay >30%, otherwise medium.
!         NB: Original settings have clay=<30 and clay+silt=<70 as 
!         critical limits. But that is too wide (it would include loam
!         and part of silt loam).
          IF (CLAY(L) .LT. 20. .AND. CLAY(L) + SILT(L) .LT. 50.) THEN
            TEXTURE = 'COARSE'
          ELSEIF (CLAY(L) .GT. 20. .AND. CLAY(L) + SILT(L) .GT. 50.)THEN
            TEXTURE = 'FINE'
          ELSE
            TEXTURE = 'MEDIUM'
          ENDIF

!         Set texture-dependent parameters.
          IF (TEXTURE .EQ. 'COARSE') THEN
            A(L) = 0.55   !A = water-filled pore fraction where WFSOM=1
            B(L) = 1.70
            C(L) = -0.007
            D(L) = 3.22
          ELSEIF (TEXTURE .EQ. 'MEDIUM') THEN
            A(L) = 0.60
            B(L) = 1.27
            C(L) = 0.0012
            D(L) = 2.84
          ELSEIF (TEXTURE .EQ. 'FINE') THEN
            A(L) = 0.60
            B(L) = 1.27
            C(L) = 0.0012
            D(L) = 2.84
          ENDIF   !End of IF block on TEXTURE.
        END DO   !End of soil layer loop.

        EOY = 0.0

!***********************************************************************
!***********************************************************************
!     RATE
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
!       Set the initialization values for WFSOM of the first day. This
!       can't be done in SEASINIT, because the variables don't have a
!       value then.
        IF (YRDOY .EQ. YRSIM) THEN
          WINFY  = WINF
          EOY    = EO
          SWY(1) = SW(1)
        ENDIF

!       ----------------------------------------------------------------
!       Soil temperature factor.
!       ----------------------------------------------------------------
!       Calculate the effect of temperature on the decomposition rate
!       parameter. The normalizer is the value of the numerator at 30
!       degrees Celsius.
        NORMALIZER = ATANF_C (30.)
        IF (L .EQ. SRFC) THEN
          TFSOM = ATANF_C (SRFTEMP) 
     &      / NORMALIZER
        ELSE
          TFSOM = ATANF_C (ST(L)) 
     &      / NORMALIZER
        ENDIF

!       Limit TFSOM between >= 0.01 and =< 1.
        TFSOM = AMAX1 (TFSOM, 0.01)
        TFSOM = AMIN1 (TFSOM, 1.)

!       ----------------------------------------------------------------
!       Soil water factor.
!       ----------------------------------------------------------------
!       The soil layers use SW for calculating WFSOM, thus today's rain
!       or irrigation will be reflected in tomorrow's WFSOM (after the
!       integration of SW). But the surface layer uses WINF, which means
!       that today's rain+irrigation will be reflected in today's WFSOM.
!       The SRFC and soil layers and thus out of phase by one day.
!       Therefore apply yesterday's rain+irrigation to the SRFC layer.
        IF (L .EQ. SRFC) THEN

!         Calculate the effect of soil water on the decomposition rate
!         parameter. For the SRFC layer, the water factor is made
!         dependent on the ratio of rain+irrigation (corrected for
!         runoff) and PET. The available water in the topsoil is also
!         added as 'buffer' for days without rain or irrigation
!         (otherwise there would be no decomposition at all on such
!         days). Rain+irrigation will then be reflected both in SW and
!         in WINF, which is double. So only use SW if there was hardly
!         any rain or irrigation (< 10 mm).
          IF (EOY .EQ. 0) THEN    !On first call EO and EOY are zero
            DBLVAL = -8.5         !CHP added fix 
          ELSE
            IF (WINF .GT. 10.) THEN
              DBLVAL = -8.5 * WINFY / EOY

            ELSE
!           NB: SW(1) can be lower than LL(1) due to evaporation from
!           the soil surface (the factor SWEF). In that case don't use
!           SW(1)-LL(1), because it would be negative.
              IF (SW(1) .LT. LL(1)) THEN
                DBLVAL = -8.5 * WINFY  / EOY
              ELSE
                DBLVAL = -8.5 *(WINFY + DLAYR(1) *(SWY(1) - LL(1)))/EOY
              ENDIF
            ENDIF
          ENDIF

!         Underflow trap
          IF (DBLVAL .GT. -20.) THEN
            WFSOM = 1. / (1. + 30. * EXP (DBLVAL))
          ELSE
            WFSOM = 1.0
          ENDIF

!         Set tomorrow's values.
          WINFY  = WINF
          EOY    = EO
          SWY(1) = SW(1)

        ELSE   !Soil layers

!==============================================================================
!     NSWI - water factor effect on SOM decomposition
!!! Use option 2 - we will get rid of other code.

         ! NSWI = 1      !Arjan's original
         ! NSWI = 2      !Old CERES v3.5 code
         ! NSWI = 3      !Original century

          !Default to method 2
          !IF (NSWI .LT. 1 .OR. NSWI .GT. 3) THEN 

      !**************************
      !**************************
            NSWI = 2
      !**************************
      !**************************
         !ENDIF

          SELECT CASE (NSWI)

!==============================================================================
          CASE (1)
!         Convert the volumetric water content to water-filled pore
!         fraction. Don't use here the measured SAT values, but do it
!         according to the theory that SAT = 1 -(BD / particle density).
          WATERFILLED = SW(L) / (1. - BD(L) / 2.65)
          WATERFILLED = AMAX1 (WATERFILLED, 0.)
          WATERFILLED = AMIN1 (WATERFILLED, 1.)

!         The water factor gives the effect of soil water conditions on 
!         the decomposition rate.
!         NB: For soils that are water saturated for very long periods
!         (e.g. swamps) the water factor should be lower than estimated
!         here.
          BASE1 = (WATERFILLED - B(L)) / (A(L) - B(L))
          BASE2 = (WATERFILLED - C(L)) / (A(L) - C(L))
          E1 = D(L) * (B(L) - A(L)) / (A(L) - C(L))
          E2 = D(L)

          WFSOM = (BASE1 ** E1) * (BASE2 ** E2)

          CASE (2)
!==============================================================================
!Old CERES v3.5 code:
          AD  = LL(L)
          IF (L .EQ. 1) THEN
            SWEF = 0.9 - 0.00038 * (DLAYR(1) - 30.) ** 2
            AD(L) = LL(L) * SWEF
          ENDIF

          WFSOM  = (SW(L) - AD(L)) / (DUL(L) - AD(L))
!         WFNIT = WFSOM

          IF (SW(L) .GT. DUL(L)) THEN
            XL = (SW(L) - DUL(L)) / (SAT(L) - DUL(L))
            WFSOM = 1.0 - 0.5 * XL
!           WFNIT = 1.0 - XL
          ENDIF
 
!==============================================================================

          CASE (3)

          RWC = (sw(L) - LL(L)) / (DUL(L) - LL(L))
          WFSOM = 1. / (1. + 4.0 * EXP (-6.0 * RWC))

          END SELECT
!==============================================================================

        ENDIF   !End of IF block on L=SRFC vs. soil layers.

!       For the SRFC layer, WFSOM cannot go below 0.03226 (if
!       DBLVAL=0). Use that also for the soil layers as minimum, or
!       the strange situation may occur that the minimum WFSOM for the
!       SRFC layer is higher than for layer 1).
        WFSOM = AMAX1 (WFSOM, 0.03226)
        WFSOM = AMIN1 (WFSOM, 1.)

!       Combine the effects of temperature, moisture and anaerobic
!       conditions.
        DEFAC(L) = TFSOM * WFSOM

!       Limit DEFAC to >= 0 and =< 1.
        DEFAC(L) = AMAX1 (DEFAC(L), 0.)
        DEFAC(L) = AMIN1 (DEFAC(L), 1.)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!     ------------------------------------------------------------------
      RETURN
      END   !SUBROUTINE DECRAT_C


!**********************************************************************
!  ATANF_C, function for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This routine is functionally equivalent to the routine of
!           the same name, described in the publication: "Some graphs
!           and their functional forms." by William Parton and Georgo.
!           Innis 1972 - Technical Report no. 153, Natural Resource
!           Ecology Lab., Colorado State University, Fort Collins, Co.
!
!  Revision history:
!  ........ Parton et al.  Written for CENTURY model.
!  26/05/99 AJG  Linked to DSSAT.
!
!  Called: DECRAT_C
!  Calls : --
!**********************************************************************

      REAL FUNCTION ATANF_C (TEMPERATURE)

      REAL PI, TEMPERATURE
      REAL TEFF(4)

      DATA TEFF /15.7, 11.4, 29.7, 0.0309/

      PI = 3.1415927
      ATANF_C = TEFF(2) + (TEFF(3) / PI) * ATAN (PI * TEFF(4) *
     &  (TEMPERATURE - TEFF(1)))

      RETURN
      END   !Function ATANF_C

!***********************************************************************
! DECRAT_C variables:
!
! A, B        Intermediate parameter (--)
! BASE1       Parameter used for soil-water factor for SOM/residue decomposition (-)
! BASE2       Parameter used for soil-water factor for SOM/residue decomposition (-)
! BD(L)       Bulk density (g[soil] / cm3[soil])
! C, D        Intermediate parameter (-)
! DBLVAL      Parameter used for soil-water factor for SOM/residue decomposition (-)
! DEFAC(L)    Decomposition rate factor (temperature and soil-water effect) (-)
! DLAYR(L)  Soil thickness in layer L (cm)
! E1, E2      Intermediate parameter (-)
! EO          Potential evapotranspiration (mm)
! EOY         Yesterday’s value of the potential evapotranspiration (mm)
! LL(L)       Volumetric soil water content in layer L at lower limit
!               (cm3[water] / cm3[soil])
! NORMALIZER  Temperature factor for SOM/residue decomposition at 30 degrees
!               Celsius (reference value). (-)
! PI          The number Pi (3.1415927) (-)
! SRFC        Identifier for the litter surface layer on top of the soil (SRFC=0)  (-)
! SRFTEMP     Temperature of the surface layer on top of the soil (degrees Celsius)
! ST(L)       Soil temperature by soil layer (oC)
! SW(L)       Today's Volumetric Soil Water Content in layer L (cm3/cm3)
! SWY(L)      Yesterday's value of the soil water content (cm3[water] / cm3[soil])
! TEFF(I)     Parameters in the calculation of the effect of soil temperature on
!               SOM decomposition (oC)
! TEMPERATURE Soil temperature (oC)
! TFSOM       Temperature factor for SOM/residue decomposition (-)
! WATERFILLED Water-filled porosity. (cm3[water] / cm3[soil pores])
! WFSOM       Water factor for SOM/residue decomposition (-)
! WINF        The amount of water that infiltrates (mm)
! WINFY       Yesterday’s value of the amount of water that infiltrates (mm)
!***********************************************************************
