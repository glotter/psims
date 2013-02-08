!***********************************************************************
!  NUPDATE_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Update the soil nitrogen variables for mineralization,
!           immobilization and (de)nitrification.
!
!  Revision history:
!  ........ ...  Written.
!  01/01/99 AJG  Separated this module from NTRANS.
!  01/01/00 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!
!  Called: CENTURY, PARTIT_C
!  Calls : --
!***********************************************************************

      SUBROUTINE NUPDATE_C (
     &  L, SNH4,                                          !Input
     &  DLTSNH4, DLTSNO3, MINERALIZE,                     !Input/Output
     &  TMINERALIZE, TIMMOBILIZE)                         !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
!     NL defined in ModuleDefs.for

      IMPLICIT  NONE
      SAVE

      INTEGER L

      REAL MINERALIZE   !In IMMOBLIMIT and NTRANS this is an array!
      REAL SNH4_AVAIL, XMIN
      REAL DLTSNH4(NL), DLTSNO3(NL), SNH4(NL)
      REAL TMINERALIZE, TIMMOBILIZE

!     ------------------------------------------------------------------
!     Distribute mineralization and immobilization over NH4 and NO3.
!     XMIN is the minimum amount of NH4 that remains in the soil.
!     XMIN = 0.5 / FAC(L)
      XMIN = 0.           !AJG

!     If the net N release from all SOM sources (MINERALIZE) is
!     positive, add the mineralized N to the NH4 pool.
      IF (MINERALIZE .GE. 0.) THEN
        DLTSNH4(L) = DLTSNH4(L) + MINERALIZE
        TMINERALIZE = TMINERALIZE + MINERALIZE

!     If MINERALIZE is < 0, there is immobilization.
      ELSE
!       Make sure that no more NH4 is immobilized than there is,
!       taking into account what has already been removed by other
!       processes (thus use only negative DLTSNH4 values). This is a
!       protection against negative values at the integration step.
        TIMMOBILIZE = TIMMOBILIZE + ABS(MINERALIZE)
        SNH4_AVAIL = SNH4(L) + AMIN1 (DLTSNH4(L), 0.) - XMIN

        IF (ABS (MINERALIZE) .GT. SNH4_AVAIL) THEN

!         If the N demand of the immobilization is greater than the
!         amount of NH4 available, take all NH4 (MINERALIZE is
!         negative!).
          MINERALIZE = MINERALIZE + SNH4_AVAIL
          DLTSNH4(L) = DLTSNH4(L) - SNH4_AVAIL

!         Get the remainder of the immobilization from nitrate
!         (MINERALIZE is negative!)
          DLTSNO3(L) = DLTSNO3(L) + MINERALIZE

        ELSE
!         Reduce soil NH4 by the immobilization (MINERALIZE is
!         negative!).
          DLTSNH4(L) = DLTSNH4(L) + MINERALIZE
        ENDIF   !End of IF block on ABS(MINERALIZE).
      ENDIF   !End of IF block on MINERALIZE>=0.

      RETURN
      END   !SUBROUTINE NUPDATE_C

