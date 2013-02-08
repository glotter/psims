C=======================================================================
C  ROOTWU, Subroutine, J.T. Ritchie
C  Calculates root water uptake rate for each soil layer and total rate.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1989 JR  Written
C  12/05/1993 NBP Made into subroutine.
C  01/18/1996 JWJ Added flooding effect on water uptake
C  01/06/1996 GH  Added soil water excess stress
C  10/10/1997 CHP Updated for modular format.
C  09/01/1999 GH  Incorporated in CROPGRO
C  01/10/2000 NBP Added SAVE for stored variables and set SWCON2=RWU=0.0
C  01/12/2000 NBP Removed FILECC from input
C  01/25/2000 NBP Added IOSTAT to READ statements to set ERRNUM.  Cleaned.
C  06/21/2001 GH  Added seasonal initialiation
C  09/17/2001 CHP Input PORMIN and RWUMX from Plant module.
C                   
C-----------------------------------------------------------------------
C Called by: WATBAL
C Calls:     None
C=======================================================================
      SUBROUTINE ROOTWU(DYNAMIC,
     &    DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW,  !Input
     &    RWU, SATFAC, TRWUP)                             !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
C-----------------------------------------------------------------------
      INTEGER DYNAMIC

      INTEGER L, NLAYR

      REAL SWEXF, TRWUP       !, SUMEX, SUMRL
      REAL SWCON1, SWCON3, PORMIN, RWUMX
      REAL SUMEX, SUMRL, SATFAC
      REAL DLAYR(NL), LL(NL), RLV(NL), RWU(NL)
      REAL SAT(NL), SW(NL), SWCON2(NL), TSS(NL)

      PARAMETER (SWCON1 = 1.32E-3)
      PARAMETER (SWCON3 = 7.01)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
C     Compute SWCON2 for each soil layer.  Adjust SWCON2 for extremely
C     high LL to avoid water uptake limitations.
!-----------------------------------------------------------------------
      DO L = 1, NL             
        SWCON2(L) = 0.0        
        RWU(L) = 0.0           
        TSS(L) = 0.0
      ENDDO                    
      TRWUP  = 0.0
      SUMEX  = 0.0
      SUMRL  = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      TRWUP  = 0.0
      SUMEX  = 0.0
      SUMRL  = 0.0

      DO L = 1,NLAYR
        SWCON2(L) = 120. - 250. * LL(L)
        IF (LL(L) .GT. 0.30) SWCON2(L) = 45.0

        RWU(L) = 0.0   !WDB - CIMMYT 2002
      ENDDO

      DO L = 1,NLAYR
        IF (RLV(L) .LE. 0.00001 .OR. SW(L) .LE. LL(L)) THEN
          RWU(L) = 0.
        ELSE
          RWU(L) = SWCON1*EXP(MIN((SWCON2(L)*(SW(L)-LL(L))),40.))/
     &      (SWCON3-ALOG(RLV(L)))

C-----------------------------------------------------------------------
C           PORMIN = MINIMUM PORE SPACE  REQUIRED FOR SUPPLYING OXYGEN
C                TO ROOTS FOR OPTIMAL GROWTH AND FUNCTION
C     TSS(L) = number of days soil layer L has been saturated
C-----------------------------------------------------------------------
          IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
             TSS(L) = 0.
          ELSE
             TSS(L) = TSS(L) + 1.
          ENDIF
C-----------------------------------------------------------------------
C           Delay of 2 days after soil layer is saturated before root
C           water uptake is affected
C-----------------------------------------------------------------------
          IF (TSS(L) .GT. 2.) THEN
             SWEXF = (SAT(L)-SW(L))/PORMIN
             SWEXF = MAX(SWEXF,0.0)
          ELSE
             SWEXF = 1.0
          ENDIF
          SWEXF = MIN(SWEXF,1.0)
          RWU(L) = MIN(RWU(L),RWUMX*SWEXF)
          RWU(L) = MIN(RWU(L),RWUMX)
        ENDIF
        RWU(L) = RWU(L)*DLAYR(L)*RLV(L)
        TRWUP  = TRWUP + RWU(L)
        SUMEX  = SUMEX + DLAYR(L)*RLV(L)*(1.0 - SWEXF)
        SUMRL  = SUMRL + DLAYR(L)*RLV(L)
      ENDDO

C-----------------------------------------------------------------------
C SATFAC = Root length weighted soil water excess stress factor
C          ( 0 = no stress; 1.0 = saturated stress )
C SUMEX  = Sum over all layers of water excess factor times depth
C          times root length density
C SUMRL  = Sum of root length density (integral over depth)
C-----------------------------------------------------------------------
      IF (SUMRL .GT. 0.0) THEN
         SATFAC = SUMEX/SUMRL
      ELSE
         SATFAC = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END !SUBROUTINE ROOTWU

!-----------------------------------------------------------------------
!     ROOTWU VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DLAYR(L)  Soil thickness in layer L (cm)
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or FINAL 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             (cm3/cm3)
! NL        Maximum number of soil layers = 20 
! NLAYR     Actual number of soil layers 
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! RLV(L)    Root length density for soil layer L ((cm root / cm3 soil))
! RWU(L)    Root water uptake from soil layer L (cm/d)
! RWUMX     Maximum water uptake per unit root length, constrained by soil 
!             water (cm3[water] / cm [root])
! SAT(L)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SATFAC    Root length weighted soil water excess stress factor ( 0 = no 
!             stress; 1 = saturated stress ) 
! SUMEX     Sum of water excess factor times depth times root length 
!             density 
! SUMRL     Sum of root length density (integrated over depth) 
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWCON1    Constant used in determining root water uptake 
! SWCON2(L) Variable used in determining root water uptake, dependant on 
!             lower limit in layer L 
! SWCON3    Constant used in determining root water uptake 
! SWEXF     Excess water stress factor for layer with deepest roots (0-1) 
! TRWUP     Total potential daily root water uptake (cm/d)
! TSS(L)    Number of days soil layer L has been saturated (d)
!-----------------------------------------------------------------------
!     END SUBROUTINE ROOTWU
!-----------------------------------------------------------------------
