C=======================================================================
C  RNOFF, Subroutine, J.T. Ritchie
C  Calculate runoff by Williams-SCS curve number (CN) technique.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  01/01/1989 JR  Written
C  10/01/1987 GH  Correction of RUNOFF calc. according to SCS curve no.
C  12/05/1993 NBP Made into subroutine
C  07/12/1996 GH  Changed Precipitation to RAIN
C  10/11/1997 CHP Updated for modular format.
C  07/01/1997 BDB Changed CN and SMX initialization (removed CN1, CN2,
C                 CN3, WF, WX, XX) from old INSOIL code
C  07/01/1997 BDB Simplified CN calculations (Removed C1, C2, C3, WF, DUL
C                  and made SMX input variables) (old WBSUBS code)
C  09/01/1999  GH Incorporated into CROPGRO
!-----------------------------------------------------------------------
!  Called by: WATBAL
!  Calls:     None
C=======================================================================
      SUBROUTINE RNOFF( 
     &    CN, LL, SAT, SW, WATAVL,                        !Input
     &    RUNOFF)                                         !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE

      CHARACTER*6 ERRKEY
      PARAMETER (ERRKEY = 'RNOFF')

      REAL CN
      REAL PB, WATAVL, SMX
      REAL RUNOFF, SWABI

!     Max number of soil layers defined in ModuleDefs.for
      REAL LL(NL), SAT(NL), SW(NL)

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
C     Runoff is related to the average soil water content of the top
C     two layers of soil
      IF (WATAVL .GT. 0.001) THEN
        SMX = 254.0 * (100.0/CN - 1.0)
        SWABI = 0.15 * ((SAT(1) - SW(1)) / (SAT(1) - LL(1) * 0.5) +
     &                (SAT(2) - SW(2)) / (SAT(2) - LL(2) * 0.5))
        SWABI = MAX(0.0, SWABI)
        PB = WATAVL - SWABI * SMX
        IF (PB .GT. 0) THEN
          RUNOFF = PB**2/(WATAVL + (1.0-SWABI) * SMX)
        ELSE
          RUNOFF = 0.0
        END IF
      ELSE
        RUNOFF = 0.0
      ENDIF

!***********************************************************************
      RETURN
      END !SUBROUTINE RNOFF

!-----------------------------------------------------------------------
!     Variable definitions for RNOFF Module (updated 2/12/2004)
!-----------------------------------------------------------------------
! CN       Runoff Curve Number - measure of runoff potential based on soil 
!            type and current soil water content. 
! LL(L)    Volumetric soil water content in soil layer L at lower limit
!           (cm3 [water] / cm3 [soil])
! PB       Determines threshold amount of rainfall that will occur before 
!            runoff starts (mm/d)
! RUNOFF   Calculated runoff (mm/d)
! SAT(L)   Volumetric soil water content in layer L at saturation
!           (cm3 [water] / cm3 [soil])
! SMX      Soil storage available for surface water based on CN formula
!           (mm)
! SW(L)    Volumetric soil water content in layer L
!           (cm3 [water] / cm3 [soil])
! SWABI(L) A soil water abstraction index, a unitless indicator of the soil 
!            water condition at the time of a rainfall event.  This affects 
!            the intercept of the runoff axis when runoff starts to 
!            occur--later when drier and sooner when wetter. 
! WATAVL   Water available for infiltration or runoff (rainfall plus 
!            irrigation) (mm/d)
!-----------------------------------------------------------------------
!     END SUBROUTINE RNOFF
!-----------------------------------------------------------------------


