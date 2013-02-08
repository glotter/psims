C=======================================================================
C  HRes_Ceres, Subroutine
C-----------------------------------------------------------------------
C  Determines harvest residue at end of season for Ceres crops.
C     Maize, millet, sorghum, rice, wheat(?).
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  08/16/2002 CHP Written based on old OPSEQ routine
C  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
C                   as defined in ModuleDefs.for
C  12/16/2004 CHP/KJB/LAH changed residue to account for harvest
C                   fractions.
!  07/21/2005 CHP Use PODWT instead of SDWT to calculated unharvested
!                   plant mass.
!  08/02/2005 CHP Only compute for sequenced runs.  Set to zero otherwise.
!  08/11/2005 CHP Further revise harvest residue computations - all
!                 cob/chaff assumed to be left in field.  Byproduct 
!                 harvested is STOVWT * Harvest %.
C=======================================================================

      SUBROUTINE HRes_Ceres(CONTROL,
     &  CROP, GRNWT, HARVFRAC, NLAYR, PLTPOP, PODWT,      !Input
     &  RLV, ROOTN, RTWT, SENESCE, STOVN, STOVWT, WTNSD,  !Input
     &  HARVRES)                                          !Output

      USE ModuleDefs
      IMPLICIT NONE
      SAVE

!     Input variables
      CHARACTER*2 CROP
      INTEGER NLAYR
      REAL GRNWT, PLTPOP, PODWT, ROOTN, RTWT, STOVN, STOVWT, WTNSD
      REAL TOPRES, GRNRES, COBRES
      REAL HARVFRAC(2)
      REAL, DIMENSION(NL) :: RLV

!     Output variables
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      TYPE (ControlType) CONTROL

!     Local Variables
      INTEGER IEL, L
      REAL PRLIG, TRTRES, TRTRESE(3), TRLV
      INTEGER, PARAMETER :: N = 1, SRFC = 0

!     Extra variables from call to IPSOIL
      REAL DMINR, DSNC, PRCEL, PRCHO, RCN, RDCEL, RDCHO, RDLIG

!     Harvest residue variables 0 = surface
      REAL HResWt(0:NL)   !Residue mass (kg[dry matter]/ha)
      REAL HResLig(0:NL)  !Residue lignin (kg[lignin]/ha)
      REAL HResE(0:NL,NELEM)  !Residue element components (kg[E]/ha)

C-----------------------------------------------------------------------
      HResWt  = 0.0   
      HResLig = 0.0
      HResE   = 0.0

!     No residue left in fallow field.  
!     Residue only computed for sequenced runs.
      IF (CROP /= 'FA' .AND. INDEX('QF',CONTROL%RNMODE) > 0) THEN

C-----------------------------------------------------------------------
!       By-product not harvested:
        TOPRES = AMAX1(0.0, STOVWT * PLTPOP * 10. * (1. - HARVFRAC(2)))
!        kg/ha               g/pl    pl/m2              

!       Grain not harvested
        GRNRES = AMAX1(0.0, GRNWT * PLTPOP * 10. * (1. - HARVFRAC(1)))
!        kg/ha               g/pl   pl/m2  
            
!       Add cob (chaff) weight -- 100%.
        COBRES = AMAX1(0.0, (PODWT - GRNWT * PLTPOP)  * 10.)
!        kg/ha                g/m2    g/pl    pl/m2

        HResWt(SRFC) = TOPRES + GRNRES + COBRES

        IF (STOVWT > 1.E-5) THEN
          HResE(SRFC,N) = STOVN * TOPRES / STOVWT
!           kg[N]/ha   =(g[N]/pl) * (kg[DW]/ha) / (g[DW]/pl)         
     &                  + WTNSD * 10. * (1. - HARVFRAC(1))
!                        g[N]/m2    
        ELSE
          HResE(SRFC,N) = WTNSD * 10. * (1. - HARVFRAC(1))
!           kg[N]/ha     g[N]/m2    
        ENDIF

C-------------------------------------------------------------------------
!       Distribute root+nodule residues by layer, according to the root
!       length distribution (NB: this may give very wrong results if the
!       roots vary in thickness by layer, but root weight is not available
!       by layer).
        TRLV = 0.
        DO L = 1, NLAYR
          TRLV = TRLV + RLV(L)
        END DO

!       Root + nodule residues.
!       -----------------------
!       Total root residues (whole profile) equal root weight 
        TRTRES = RTWT * PLTPOP * 10.              !kg/ha
!             g/plant * plants/m2

!       N in root residues.
        TRTRESE(N) = ROOTN * PLTPOP * 10.       !kg/ha
!                 g[N]/plant * plants/m2  

!       Senescence has been added daily (subroutine SENESADD), so no need
!       to add it here as WTRO and WTNOO, as in the CERES-based module.
        IF (TRLV > 1.E-6) THEN
          DO L = 1, NLAYR
            HResWt(L) = TRTRES * RLV(L) / TRLV
            DO IEL = 1, NELEM
              HResE(L,IEL) = TRTRESE(IEL) * RLV(L) / TRLV
            END DO   !End of IEL loop.
          END DO   !End of soil layer loop.
        ENDIF

C-------------------------------------------------------------------------
!       Lignin fraction in soil and surface senesced matter:

!       Call IPSOIL and get the indicated parameters for this
!       crop residue, including RDECR(1,3) and the pool sizes
!       for CHO, cellulose, and lignin
        CALL IPSOIL (CONTROL,'RE001',                   !Input
     &    DMINR, DSNC, PRCEL, PRCHO, PRLIG,             !Output
     &    RCN, RDCEL, RDCHO, RDLIG)                     !Output

        !Multiply biomass array by % lignin to get lignin component
        HResLig = HResWt * PRLIG / 100.

C-------------------------------------------------------------------------
        !Add in last day of senesced plant material (not added in soil
        !  module because it is computed after soil integration.

! chp 08/11/2005
!       Senesced leaf and stem may be harvested for byproduct.
        HResWt(SRFC)  = HResWt(SRFC)  + SENESCE % ResWt(SRFC) 
     &                    * (1. - HARVFRAC(2))
        HResLig(SRFC) = HResLig(SRFC) + SENESCE % ResLig(SRFC)
     &                    * (1. - HARVFRAC(2))
        HResE(SRFC,N) = HResE(SRFC,N) + SENESCE % ResE(SRFC,N)
     &                    * (1. - HARVFRAC(2))

        DO L = 1, NLAYR
          HResWt(L)  = HResWt(L)  + SENESCE % ResWt(L)
          HResLig(L) = HResLig(L) + SENESCE % ResLig(L)
          HResE(L,N) = HResE(L,N) + SENESCE % ResE(L,N)
        ENDDO

      ENDIF   !Crop .NE. 'FA'
C-----------------------------------------------------------------------
!     Transfer results to constructed variable
      HARVRES % ResWt  = HResWt
      HARVRES % ResLig = HResLig
      HARVRES % ResE   = HResE

      RETURN
      END SUBROUTINE HRes_Ceres

