!***********************************************************************
!  INCORPOR_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine takes care of incorporation of surface
!           litter into the soil, both for situations in which
!           incorporation is done immediately when residue is applied
!           or when incorporation is done at a later time. This
!           subroutine only works when the CENTURY-based SOM model has
!           been chosen, because otherwise surface litter is not being
!           simulated.
!
!  Revision history:
!  01/01/99 AJG  Written
!
!  Called: CENTURY
!  Calls : --
!***********************************************************************

      SUBROUTINE INCORPOR_C (
     &  CFMETS1, CFS1S2, CFSTRS1, CFSTRS2, CO2FMET,       !Input
     &  CO2FSTR, CO2FS1, DLAYR, EFMETS1, EFS1S2,          !Input
     &  EFSTRS1, EFSTRS2, LIGC, IMMMETS1, IMMSTRS1,       !Input
     &  METABC, METABE, MNRMETS1, MNRSTRS1,               !Input
     &  MNRSTRS2, MNRS1S2, NLAYR, SOM1C,                  !Input
     &  SOM1E, STRUCC, STRUCE, TILLDEPTH,                 !Input
     &  TILLMIXPERC,                                      !Input

     &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSOM1C,          !Output
     &  DLTSOM1E, DLTSTRUCC, DLTSTRUCE,                   !Output

     &  DYNAMIC)                                          !Control

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL, NELEM defined in ModuleDefs.for

      IMPLICIT NONE
      SAVE

      INTEGER DYNAMIC, IEL, IOUT, L, LIG, NLAYR, NONLIG,
     &  SRFC
      PARAMETER (NONLIG = 1, LIG = 2)
      PARAMETER (SRFC = 0)

      REAL DEPTH, FR, HOLD, TILLDEPTH, TILLMIXPERC

      REAL CFMETS1(0:NL), CFS1S2(0:NL), CFSTRS1(0:NL), CFSTRS2(0:NL),
     &  CO2FMET(0:NL), CO2FS1(0:NL), DLAYR(NL),
     &  DLTLIGC(0:NL), DLTMETABC(0:NL), DLTSOM1C(0:NL), DLTSTRUCC(0:NL),
     &  LIGC(0:NL), METABC(0:NL), SOM1C(0:NL), STRUCC(0:NL)

      REAL CO2FSTR(0:NL,2), DLTMETABE(0:NL,3), DLTSOM1E(0:NL,3),
     &  DLTSTRUCE(0:NL,3), EFMETS1(0:NL,3), EFS1S2(0:NL,3),
     &  EFSTRS1(0:NL,3), EFSTRS2(0:NL,3), IMMMETS1(0:NL,3),
     &  IMMSTRS1(0:NL,3), METABE(0:NL,3), MNRMETS1(0:NL,3),
     &  MNRSTRS1(0:NL,3), MNRSTRS2(0:NL,3), MNRS1S2(0:NL,3),
     &  SOM1E(0:NL,3), STRUCE(0:NL,3)


!***********************************************************************
!***********************************************************************
!     RATE
!***********************************************************************
      IF (DYNAMIC .EQ. RATE) THEN
!       ----------------------------------------------------------------
!       If residues had been applied earlier on top of the soil surface,
!       but are only now incorporated into the soil, then they have been
!       divided already in metabolic and structural material. These are
!       now thus distributed over the soil layers.


!       If the tillage depth is zero, the incorporation percentage
!       should also be zero.
        IF (TILLDEPTH .LT. 0.001) TILLMIXPERC = 0.

!       Set the starting depth for counting the layers to zero.
        DEPTH = 0.

!       Initialize a flag that determines when to jump out of the DO
!       loop.
        IOUT = 1

        DO L = 1, NLAYR
!         Set the depth of the upper limit of the layer.
          HOLD = DEPTH

!         Calculate the depth of the bottom of the layer.
          DEPTH = DEPTH + DLAYR(L)

!         If the residue application depth is less than the depth of
!         the bottom of the layer.
          IF (TILLDEPTH .LE. DEPTH) THEN

!           Calculate the fraction of the residue that is to be added
!           to this layer.
            IF (TILLDEPTH .GT. 0.) FR = (TILLDEPTH - HOLD) / TILLDEPTH

!           Set a flag to jump out of the DO loop.
            IOUT = 2
          ELSE
!           If the residue application depth is greater than the
!           depth of bottom of the layer, calculate the fraction of
!           the residue that is to be added to this layer.
            IF (TILLDEPTH .GT. 0.) FR = DLAYR(L) / TILLDEPTH
          ENDIF

!         Add the newly incorporated material to the litter pools of
!         the layer. Don't incorporate what has already decomposed
!         (though SOM1(SRFC) receives material from decomposed METAB and
!         STRUC).
          DLTMETABC(L) = DLTMETABC(L) + (METABC(SRFC) - CFMETS1(SRFC) -
     &      CO2FMET(SRFC)) * FR * TILLMIXPERC / 100.
          DLTSTRUCC(L) = DLTSTRUCC(L) + (STRUCC(SRFC) - CFSTRS1(SRFC) -
     &      CFSTRS2(SRFC) - CO2FSTR(SRFC,NONLIG) - CO2FSTR(SRFC,LIG)) *
     &      FR * TILLMIXPERC / 100.
          DLTLIGC(L)   = DLTLIGC(L) + (LIGC(SRFC) - CFSTRS2(SRFC) -
     &      CO2FSTR(SRFC,LIG)) * FR * TILLMIXPERC / 100.
          DLTSOM1C(L)  = DLTSOM1C(L) + (SOM1C(SRFC) - CFS1S2(SRFC) -
     &      CO2FS1(SRFC) + CFMETS1(SRFC) + CFSTRS1(SRFC)) * FR *
     &      TILLMIXPERC / 100.

          DO IEL = 1, NELEM
            DLTMETABE(L,IEL) = DLTMETABE(L,IEL) + (METABE(SRFC,IEL) -
     &        EFMETS1(SRFC,IEL) - MNRMETS1(SRFC,IEL)) * FR *
     &        TILLMIXPERC / 100.
            DLTSTRUCE(L,IEL) = DLTSTRUCE(L,IEL) + (STRUCE(SRFC,IEL) -
     &        EFSTRS1(SRFC,IEL) - EFSTRS2(SRFC,IEL) - MNRSTRS1(SRFC,IEL)
     &        - MNRSTRS2(SRFC,IEL)) * FR * TILLMIXPERC / 100.
            DLTSOM1E(L,IEL)  = DLTSOM1E(L,IEL) + (SOM1E(SRFC,IEL) -
     &        EFS1S2(SRFC,IEL) - MNRS1S2(SRFC,IEL) + EFMETS1(SRFC,IEL) +
     &        EFSTRS1(SRFC,IEL) + IMMMETS1(SRFC,IEL) +
     &        IMMSTRS1(SRFC,IEL)) * FR * TILLMIXPERC / 100.
          END DO   !End of IEL loop.

!         If there are no more soil layers over which to distribute
!         the residue, jump out of the DO loop. 
          IF (IOUT .EQ. 2) GOTO 100
        END DO   !End of layer loop.

!       Continue here after jumping out of the DO loop.
100     CONTINUE

!       Correct the SRFC litter pools.
        DLTMETABC(SRFC) = DLTMETABC(SRFC) - (METABC(SRFC) -
     &    CFMETS1(SRFC) - CO2FMET(SRFC)) * TILLMIXPERC / 100.
        DLTSTRUCC(SRFC) = DLTSTRUCC(SRFC) - (STRUCC(SRFC) -
     &    CFSTRS1(SRFC) - CFSTRS2(SRFC) - CO2FSTR(SRFC,NONLIG) -
     &    CO2FSTR(SRFC,LIG)) * TILLMIXPERC / 100.
        DLTLIGC(SRFC)   = DLTLIGC(SRFC) - (LIGC(SRFC) - CFSTRS2(SRFC) -
     &      CO2FSTR(SRFC,LIG)) * TILLMIXPERC / 100.
        DLTSOM1C(SRFC)  = DLTSOM1C(SRFC)  - (SOM1C(SRFC) - CFS1S2(SRFC)
     &    - CO2FS1(SRFC) + CFMETS1(SRFC) + CFSTRS1(SRFC)) *
     &    TILLMIXPERC / 100.

        DO IEL = 1, NELEM
          DLTMETABE(SRFC,IEL) = DLTMETABE(SRFC,IEL) - (METABE(SRFC,IEL)
     &      - EFMETS1(SRFC,IEL) - MNRMETS1(SRFC,IEL)) *
     &      TILLMIXPERC / 100.
          DLTSTRUCE(SRFC,IEL) = DLTSTRUCE(SRFC,IEL) - (STRUCE(SRFC,IEL)
     &      - EFSTRS1(SRFC,IEL) - EFSTRS2(SRFC,IEL) - MNRSTRS1(SRFC,IEL)
     &      - MNRSTRS2(SRFC,IEL)) * TILLMIXPERC / 100.
          DLTSOM1E(SRFC,IEL)  = DLTSOM1E(SRFC,IEL) - (SOM1E(SRFC,IEL) -
     &      EFS1S2(SRFC,IEL) - MNRS1S2(SRFC,IEL) + EFMETS1(SRFC,IEL) +
     &      EFSTRS1(SRFC,IEL) + IMMMETS1(SRFC,IEL) +
     &      IMMSTRS1(SRFC,IEL)) * TILLMIXPERC / 100.
        END DO   !End of IEL loop.


!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!     ------------------------------------------------------------------
      RETURN
      END   !SUBROUTINE INCORPOR_C.

!***********************************************************************
! INCORPOR_C variables:
!
! CFMETS1     C flow from the metabolic pool to SOM1 (-)
! CFS1S2      C flow from SOM1 to SOM2  (kg[C] / ha)
! CFSTRS1     C flow from the structural pool to SOM1 (kg[C] / ha)
! CFSTRS2     C flow from the structural pool to SOM2 (kg[C] / ha)
! CO2FMET     CO2 flow that accompanies the C flow out of the metabolic pool
!               (kg[C] / ha)
! CO2FS1      CO2 flow that accompanies the C flow out of SOM1l (kg[C] / ha)
! CO2FSTR     CO2 flow that accompanies the C flow out of the structural pool
!               (kg[C] / ha)
! DLAYR       Thickness of soil layer L (cm)
! DLTLIGC     Rate variable for the change in lignin C (kg[C] / ha)
! DLTMETABC   Rate variable for the change in metabolic C (kg[C] / ha)
! DLTMETABE   Rate variable for the change in metabolic E (kg[E] / ha)
! DLTSOM1C    Rate variable for the change in SOM1 C (kg[C] / ha)
! DLTSOM1E    Rate variable for the change in SOM1 E (kg[E] / ha)
! DLTSTRUCC   Rate variable for the change in structural C (kg[C] / ha)
! DLTSTRUCE   Rate variable for the change in structural E (kg[E] / ha)
! DYNAMIC     Controls module sequence: DYNAMIC =RUNINIT, SEASINIT, RATE, 
!               INTEGR, OUTPUT, or FINAL
! EFMETS1     E flow from soil or soil or surface metabolic residue to soil
!               or surface SOM1 (kg[E] / ha)
! EFS1S2      E flow from soil or surface SOM1 to SOM2 (kg[E] / ha)
! EFSTRS1     E flow from soil or surface structural residue to soil or
!               surface SOM1 (kg[E] / ha)
! EFSTRS2     E flow from soil or soil or surface structural residue to SOM2
!               (kg[E] / ha)
! IMMMETS1    Immobilization of E during the flow from soil or surface metabolic
!               residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS1    Immobilization of E during the flow from soil or surface structural
!               residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS2    Immobilization of E during the flow from soil or surface structural
!               residue to SOM2  (kg[E] / ha)
! METABC      Metabolic litter C pool (kg[C] / ha)
! METABE      Metabolic litter E pool (kg[E] / ha)
! MNRMETS1    Mineralization of E during the flow from soil or surface metabolic
!               residue to soil or surface SOM1  (kg[E] / ha)
! MNRS1S2     Mineralization of E during the flow from SOM1 to SOM2 (kg[E] / ha)
! MNRSTRS1    Mineralization of E during the flow from soil or surface structural
!               to soil or surface SOM1  (kg[E] / ha)
! MNRSTRS2    Mineralization of E during the flow from soil or surface structural
!               residue to SOM2  (kg[E] / ha)
! NELEM       Number of elements: 1 = N, 2 = N+P, 3 = N+P+S (-)
! NLAYR       Number of soil layers (-)
! METABC      Soil or surface metabolic residue carbon content (units: kg[C] / ha)
! METABE      Soil or surface metabolic residue E content (units: kg[E] / ha)
! SOM1C       Soil or surface SOM1 carbon content (units: kg[C] / ha)
! SOM1E       Soil or surface SOM1 E content (units: kg[E] / ha)
! SOM2C       SOM2 carbon content (units: kg[C] / ha)
! SOM2E       SOM2 E content (units: kg[E] / ha)
! SOM3C       SOM3 carbon content (units: kg[C] / ha)
! SOM3E       SOM3 E content (units: kg[E] / ha)
! STRUCC      Soil or surface structural residue carbon content (units: kg[C] / ha)
! STRUCE      Soil or surface structural residue E content (units: kg[E] / ha)
! TILLDEPTH   Tillage depth of current tillage event.(cm)
! TILLMIXPERC Percentage of the surface residues that will be incorporated with the
!               current tillage event (%)
!***********************************************************************
