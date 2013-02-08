!***********************************************************************
!  TSOMLIT_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Sum all SOM pools and litter pools for the whole soil
!           profile.
!
!  REVISION HISTORY
!  06/09/1999 AJG Written
!  01/01/2000 AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular structure.
!  Called : CENTURY, SOILNI_C
!  Calls  : --
!***********************************************************************

      SUBROUTINE TSOMLIT_C (
     &  METABC, METABE, NLAYR, SOM1C, SOM1E, SOM2C,       !Input
     &  SOM2E, SOM3C, SOM3E, STRUCC, STRUCE,              !Input
     &  LITC, LITE, SSOMC, SSOME, TLITC, TLITE,           !Output
     &  TMETABC, TMETABE, SomLitC, SomLitE,               !Output
     &  TSOM1C, TSOM1E, TSOM2C, TSOM2E, TSOM3C,           !Output
     &  TSOM3E, TSOMC, TSOME, TSTRUCC, TSTRUCE)           !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      INTEGER IEL, L, NLAYR, SRFC
      PARAMETER (SRFC = 0)

      REAL TLITC, TMETABC, TSOM1C, TSOM2C, TSOM3C, TSOMC, TSTRUCC
      REAL LITC(0:NL), METABC(0:NL), SOM1C(0:NL), SOM2C(NL), SOM3C(NL),
     &  SSOMC(0:NL), STRUCC(0:NL), TLITE(3), TMETABE(3), TSOM1E(3),
     &  TSOM2E(3), TSOM3E(3), TSOME(3), TSTRUCE(3)
      REAL SomLitC(0:NL), SomLitE(0:NL,NELEM)
      REAL LITE(0:NL,3), METABE(0:NL,3), SOM1E(0:NL,3), SOM2E(NL,3),
     &  SOM3E(NL,3), SSOME(0:NL,3), STRUCE(0:NL,3)

!     ------------------------------------------------------------------
!     Initialize to zero before summing up across all soil layers.
      TLITC   = 0.
      TMETABC = 0.
      TSTRUCC = 0.

      TSOMC   = 0.
      TSOM1C  = 0.
      TSOM2C  = 0.
      TSOM3C  = 0.

      TLITE   = 0.
      TMETABE = 0.
      TSTRUCE = 0.

      TSOME   = 0.
      TSOM1E  = 0.
      TSOM2E  = 0.
      TSOM3E  = 0.

!     ---------------------------------------------------------------
!     Compute surface totals
      LITC(SRFC) = STRUCC(SRFC) + METABC(SRFC)
      SSOMC(SRFC) = SOM1C(SRFC)
      SomLitC(SRFC) = SSOMC(SRFC) + LITC(SRFC)
      DO IEL = 1, NELEM
        LITE(SRFC,IEL) = STRUCE(SRFC,IEL) + METABE(SRFC,IEL)
        SSOME(SRFC,IEL) = SOM1E(SRFC,IEL)
        SomLitE(SRFC,IEL) = SSOME(SRFC,IEL) + LITE(SRFC,IEL)
      END DO

!     ---------------------------------------------------------------
!     Calculate the combined structural-plus-metabolic C and E, and
!     sum across the whole profile (not SRFC layer).
      DO L = 1, NLAYR
        LITC(L) = STRUCC(L) + METABC(L)
        TMETABC = TMETABC + METABC(L)
        TSTRUCC = TSTRUCC + STRUCC(L)
        TLITC   = TLITC   + LITC(L)

        DO IEL = 1, NELEM
          LITE(L,IEL)  = STRUCE(L,IEL) + METABE(L,IEL)
          TMETABE(IEL) = TMETABE(IEL)  + METABE(L,IEL)
          TSTRUCE(IEL) = TSTRUCE(IEL)  + STRUCE(L,IEL)
          TLITE(IEL)   = TLITE(IEL)    + LITE(L,IEL)
        END DO   !End of DO loop on IEL
      END DO   !End of DO loop on L

!     Calculate the total SOM C and E in the soil layers, and sum
!     across the whole profile (not SRFC layer).
      DO L = 1, NLAYR
        SSOMC(L) = SOM1C(L) + SOM2C(L) + SOM3C(L)
        TSOMC    = TSOMC  + SSOMC(L)
        TSOM1C   = TSOM1C + SOM1C(L)
        TSOM2C   = TSOM2C + SOM2C(L)
        TSOM3C   = TSOM3C + SOM3C(L)
        SomLitC(L) = SSOMC(L) + LITC(L)

        DO IEL = 1, NELEM
          SSOME(L,IEL) = SOM1E(L,IEL) + SOM2E(L,IEL) + SOM3E(L,IEL)
          TSOME(IEL)   = TSOME(IEL)  + SSOME(L,IEL)
          TSOM1E(IEL)  = TSOM1E(IEL) + SOM1E(L,IEL)
          TSOM2E(IEL)  = TSOM2E(IEL) + SOM2E(L,IEL)
          TSOM3E(IEL)  = TSOM3E(IEL) + SOM3E(L,IEL)
          SomLitE(L,IEL) = SSOME(L,IEL) + LITE(L,IEL)
        ENDDO   !End of DO loop on IEL.
      ENDDO   !End of DO loop on L.

!     ------------------------------------------------------------------
      RETURN
      END SUBROUTINE TSOMLIT_C
