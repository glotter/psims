!***********************************************************************
! IMMOBLIMIT_C, subroutine for CENTURY-based SOM/residue module of DSSAT
!
!  Purpose: Limit the immobilization, when summing up all the E flows
!           would take away more mineral E than there is in a soil
!           layer.
! 
!  Revision history:
!  01/01/2000 AJG  Written.
!  06/23/2003 AJG  Included a 'fake' integration and a reduction factor
!                  to ensure that no state variables go negative in the
!                  real integration.
!  11/14/2003 CHP  Added checks for zero divides

!  Called: CENTURY
!  Calls : --
!***********************************************************************

      SUBROUTINE IMMOBLIMIT_C (
     &  AMINRL, CFMETS1, CFS1S2, CFS1S3, CFS2S1,          !Input
     &  CFS2S3, CFS3S1, CFSTRS1, CFSTRS2, CO2FMET,        !Input
     &  CO2FS1, CO2FS2, CO2FS3, CO2FSTR, DLTSNH4,         !Input
     &  DLTSNO3, EFMETS1, EFS1S2, EFS1S3, EFS2S1,         !Input
     &  EFS2S3, EFS3S1, EFSTRS1, EFSTRS2, IMMMETS1,       !Input
     &  IMMS1S2, IMMS1S3, IMMS2S1, IMMS2S3, IMMS3S1,      !Input
     &  IMMSTRS1, IMMSTRS2, LIGC, METABC, METABE,         !Input
     &  MNRMETS1, MNRS1S2, MNRS1S3, MNRS2S1,              !Input
     &  MNRS2S3, MNRS3S1, MNRSTRS1, MNRSTRS2, NLAYR,      !Input
     &  SOM1C, SOM1E, SOM2C, SOM2E, SOM3C, SOM3E,         !Input
     &  STRUCC, STRUCE,                                   !Input

     &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSOM1C,          !Input/Output
     &  DLTSOM1E, DLTSOM2C, DLTSOM2E, DLTSOM3C,           !Input/Output
     &  DLTSOM3E, DLTSTRUCC, DLTSTRUCE,                   !Input/Output

     &  MINERALIZE)                                       !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL, NELEM defined in ModuleDefs.for

      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------

      INTEGER IEL, L, LIG, N, NLAYR, NONLIG, P, SRFC
      PARAMETER (NONLIG = 1, LIG = 2) 
      PARAMETER (N = 1, P = 2)
      PARAMETER (SRFC = 0)

      LOGICAL FLAGSTRS1(0:NL), FLAGSTRS2(0:NL), FLAGMETS1(0:NL),
     &  FLAGS1S2(0:NL), FLAGS1S3(NL), FLAGS2S1(NL), FLAGS2S3(NL),
     &  FLAGS3S1(NL)

      REAL CFS1S2OLD, CFS1S3OLD, CFS2S1OLD, CFS2S3OLD, REDUCF, XMIN
      PARAMETER (XMIN = 0.1)

      REAL CFMETS1(0:NL), CFS1S2(0:NL), CFS1S3(NL),
     &  CFS2S1(NL), CFS2S3(NL), CFS3S1(NL),
     &  CFSTRS1(0:NL), CFSTRS2(0:NL), CO2FMET(0:NL),
     &  CO2FS1(0:NL), CO2FS2(NL), CO2FS3(NL),
     &  DLTLIGC(0:NL), DLTMETABC(0:NL), DLTSNH4(NL), DLTSNO3(NL),
     &  DLTSOM1C(0:NL), DLTSOM2C(NL), DLTSOM3C(NL),
     &  DLTSTRUCC(0:NL), LIGC(0:NL), METABC(0:NL), REDUCFACTMIN(NL),
     &  SOM1C(0:NL), SOM2C(NL), SOM3C(NL), STRUCC(0:NL)

      REAL AMINRL(NL,3), CO2FSTR(0:NL,2), DLTMETABE(0:NL,3),
     &  DLTSOM1E(0:NL,3), DLTSOM2E(NL,3), DLTSOM3E(NL,3),
     &  DLTSTRUCE(0:NL,3), EFMETS1(0:NL,3), EFS1S2(0:NL,3),
     &  EFS1S3(NL,3), EFS2S1(NL,3), EFS2S3(NL,3), EFS3S1(NL,3),
     &  EFSTRS1(0:NL,3), EFSTRS2(0:NL,3),  IMMMETS1(0:NL,3),
     &  IMMS1S2(0:NL,3), IMMS1S3(NL,3), IMMS2S1(NL,3),
     &  IMMS2S3(NL,3), IMMS3S1(NL,3), IMMSTRS1(0:NL,3),
     &  IMMSTRS2(0:NL,3), IMMSUMNET(0:NL,3), METABE(0:NL,3), 
     &  MINERALIZE(0:NL,3), MNRSTRS1(0:NL,3), MNRSTRS2(0:NL,3),
     &  MNRMETS1(0:NL,3), MNRS1S2(0:NL,3), MNRS1S3(NL,3),
     &  MNRS2S1(NL,3), MNRS2S3(NL,3), MNRS3S1(NL,3),
     &  REDUCFACT(NL,3), SOM1E(0:NL,3), SOM2E(NL,3), SOM3E(NL,3), 
     &  STRUCE(0:NL,3), TAKEOFF(NL,3)


!***********************************************************************
      DO L = 0, NLAYR   !Including SRFC layer.
!       --------------------------
!       Every-day initializations.
!       --------------------------
!       Set the flags that indicate whether the immobilization has to
!       be limited to false.
        FLAGSTRS1(L)  = .FALSE.
        FLAGSTRS2(L)  = .FALSE.
        FLAGMETS1(L)  = .FALSE.
        FLAGS1S2(L)   = .FALSE.
        IF (L .NE. SRFC) THEN
          FLAGS1S3(L) = .FALSE.
          FLAGS2S1(L) = .FALSE.
          FLAGS2S3(L) = .FALSE.
          FLAGS3S1(L) = .FALSE.
      
          DO IEL = 1, NELEM
!           Initialize REDUCFACT to the value that holds if the
!           immobilization does not have to be limited, the N removal
!           by other processes than SOM and litter decomposition to
!           zero, and the sum variables to zero.
            REDUCFACT(L,IEL) = 1.
            TAKEOFF(L,IEL)   = 0.
            IMMSUMNET(L,IEL) = 0.
          END DO
        ENDIF

!       ------------------------------------
!       Total N uptake by various processes.
!       ------------------------------------
        DO IEL = 1, NELEM

!         E removal from soil by plant uptake, leaching, nitrification,
!         denitrification. Because the SRFC layer uses the mineral E of
!         layer 1, TAKEOFF has no value for the SRFC layer.
          IF (L .NE. SRFC) THEN
            IF (IEL .EQ. N .AND. DLTSNH4(L) + DLTSNO3(L) .LT. 0.) THEN
              TAKEOFF(L,N) = ABS (DLTSNH4(L) + DLTSNO3(L))
!            ELSEIF (IEL .EQ. P .AND. ........) THEN
!To be used for future P version....
            ENDIF
          ENDIF

          IF (L .EQ. SRFC) THEN
!           Determine the sum of all net-immobilizing flows, because it
!           are only those pools for which the flows may have to be
!           limited. This sum is different from the overall net
!           immobilization of the layer. Take the flow from the
!           structural pool to SOM1 and SOM2 together, because these
!           flows are linked.
            IMMSUMNET(SRFC,IEL) = 
     &        MAX (IMMMETS1(SRFC,IEL) - MNRMETS1(SRFC,IEL), 0.) +
     &        MAX (IMMSTRS1(SRFC,IEL) + IMMSTRS2(SRFC,IEL) - 
     &        MNRSTRS1(SRFC,IEL) - MNRSTRS2(SRFC,IEL), 0.) +
     &        MAX (IMMS1S2(SRFC,IEL) - MNRS1S2(SRFC,IEL), 0.)

          ELSE 
!           Ditto for each of the soil layers.
            IMMSUMNET(L,IEL)  =  
     &        MAX (IMMMETS1(L,IEL) - MNRMETS1(L,IEL), 0.) +
     &        MAX (IMMSTRS1(L,IEL) + IMMSTRS2(L,IEL) -
     &        MNRSTRS1(L,IEL) - MNRSTRS2(L,IEL), 0.) +
     &        MAX (IMMS1S2(L,IEL) - MNRS1S2(L,IEL), 0.) +
     &        MAX (IMMS1S3(L,IEL) - MNRS1S3(L,IEL), 0.) +
     &        MAX (IMMS2S1(L,IEL) - MNRS2S1(L,IEL), 0.) +
     &        MAX (IMMS2S3(L,IEL) - MNRS2S3(L,IEL), 0.) +
     &        MAX (IMMS3S1(L,IEL) - MNRS3S1(L,IEL), 0.)
          ENDIF   !End of IF block on SRFC layer vs. other layers. 
        END DO   !End of IEL loop.
      END DO   !End of layer loop.

!     ------------------------------------------------------------------
!     Reduction factor for immobilization.
!     ------------------------------------------------------------------
!     If the total E immobilization and other E takeoff is greater than
!     the amount of E available in the soil, reduce the SOM and litter
!     decomposition by a reduction factor, so that:
!     Amount of E needed for immobilization = amount of E available.
!     Or phrased differently:
!     REDUCFACT * (net immobilization) = (AMINRL - XMIN) - TAKEOFF
!     A minimum amount of XMIN has to stay behind to prevent slightly
!     negative AMINRL due to inaccuracies with REAL variables.
!     All immobilizing flows in the layer are then reduced by the same
!     reduction factor, without making a distinction which flow may be
!     more important. The mineralization flows remain untouched: they
!     don't remove mineral E, but only add to the soil.

      DO L = 1, NLAYR  !SRFC layer not needed: uses AMINRL from layer 1.
        DO IEL = 1, NELEM
!         The SRFC layer uses the mineral E of soil layer 1. Calculate
!         the reduction factor to limit the immobilization from layer 1;
!         this factor holds for both the SRFC layer and for layer 1.
!         Due to inaccuracy in real variables, REDUCFACT may still take
!         more AMINRL than there is if AMINRL is very small (though the
!         difference is very, very small). This could result in negative
!         AMINRL. Keep therefore a minimum amount of AMINRL: XMIN.
          IF (L .EQ. 1) THEN
            IF (IMMSUMNET(SRFC,IEL) + IMMSUMNET(1,IEL) .GT. 
     &        AMINRL(1,IEL) - TAKEOFF(1,IEL) - XMIN) THEN

!             Calculate the reduction factor for layer 1
              REDUCFACT(1,IEL) = (AMINRL(1,IEL) - TAKEOFF(1,IEL) - XMIN)
     &          / (IMMSUMNET(SRFC,IEL) + IMMSUMNET(1,IEL))
            ENDIF
          ELSE
!           Ditto for the other soil layers.
            IF (IMMSUMNET(L,IEL) .GT. 
     &        AMINRL(L,IEL) - TAKEOFF(L,IEL) -XMIN) THEN
              REDUCFACT(L,IEL) = (AMINRL(L,IEL) - TAKEOFF(L,IEL) - XMIN)
     &          / (IMMSUMNET(L,IEL))
            ENDIF
          ENDIF

!         If there is hardly any mineral N, don't allow immobilization.
          IF (AMINRL(L,IEL) .LT. 1.E-6) REDUCFACT(L,IEL) = 0.

!         Limit between >= 0 and =<1.
          REDUCFACT(L,IEL) = AMAX1 (REDUCFACT(L,IEL), 0.) 
          REDUCFACT(L,IEL) = AMIN1 (REDUCFACT(L,IEL), 1.)
 
!         If IMMSUMNET(L,IEL) equals zero, then the REDUCFACT is 
!         -infinity. In the next statement this would lead to setting
!         it to zero,while it really should be set to 1.0, because
!         there is no net immobilization.
          IF (IMMSUMNET(L,IEL) .LT. 0.00001) REDUCFACT(L,IEL) = 1.
        END DO   !End of IEL loop.

!       Determine the minimum of the reduction factors for N and P.
        IF (L .NE. SRFC .AND. NELEM .EQ. 1) THEN
          REDUCFACTMIN(L) = REDUCFACT(L,N)
!For future P version.
!       ELSEIF (L .NE. SRFC .AND. NELEM .EQ. 2) THEN
!         REDUCFACTMIN(L) = MIN (REDUCFACT(L,N), REDUCFACT(L,P))
        ENDIF
      END DO   !End of layer loop.

!     ------------------------------------------------------------------
!     Limit the immobilization.
!     ------------------------------------------------------------------
!     Because the reduction factor only applies to immobilizing flows
!     and not to mineralizing flows, it has to be determined whether a
!     flow is immobilizing for either the N or P. It could be that N is
!     not immobilizing while P is (or vice versa), but because they
!     affect the same decomposition process, both the N and P flow then
!     have to be corrected by the reduction factor. Therefor, do an IEL
!     loop that sets a flag for flows of which any of these two is
!     immobilizing. Then apply the reduction factor to the C, N and P
!     flows from that pool.

      DO L = 0, NLAYR   !Including SRFC layer.
        DO IEL = 1, NELEM
!         ----------
!         SRFC LAYER
!         ----------
          IF (L .EQ. SRFC) THEN
!           Flow from metabolic litter to SOM1.
            IF (IMMMETS1(SRFC,IEL) - MNRMETS1(SRFC,IEL) .GT. 0.) THEN
              FLAGMETS1(SRFC) = .TRUE.
            ENDIF

!           The lignin and non-lignin fraction of the structural litter
!           decompose together. So if one has to be reduced, the other
!           one has to go also.
            IF ((IMMSTRS1(SRFC,IEL) - MNRSTRS1(SRFC,IEL) .GT. 0.) .OR. 
     &        (IMMSTRS2(SRFC,IEL) - MNRSTRS2(SRFC,IEL) .GT. 0.)) THEN
              FLAGSTRS1(SRFC) = .TRUE.
              FLAGSTRS2(SRFC) = .TRUE.
            ENDIF

!           Flow from SOM1 to SOM2.
            IF (IMMS1S2(SRFC,IEL) - MNRS1S2(SRFC,IEL) .GT. 0.) THEN
              FLAGS1S2(SRFC) = .TRUE.
            ENDIF

          ELSE
!           -----------
!           SOIL LAYERS
!           -----------
!           --------------------------------------------
!           Flow out of metabolic and structural litter.
!           --------------------------------------------
!           Flow from metabolic litter to SOM1.
            IF (IMMMETS1(L,IEL) - MNRMETS1(L,IEL) .GT. 0.) THEN
              FLAGMETS1(L) = .TRUE.
            ENDIF

!           The lignin and non-lignin fraction of the structural litter
!           decompose together. So if one has to be reduced, the other
!           one has to go also.
            IF ((IMMSTRS1(L,IEL) - MNRSTRS1(L,IEL) .GT. 0.) .OR.
     &        (IMMSTRS2(L,IEL) - MNRSTRS2(L,IEL) .GT. 0.)) THEN
              FLAGSTRS1(L) = .TRUE.
              FLAGSTRS2(L) = .TRUE.
            ENDIF

!           -----------------
!           Flow out of SOM1.
!           -----------------
!           Flow from SOM1 to SOM2.
            IF (IMMS1S2(L,IEL) - MNRS1S2(L,IEL) .GT. 0.) THEN
              FLAGS1S2(L) = .TRUE.
            ENDIF

!           Flow from SOM1 to SOM3.
            IF (IMMS1S3(L,IEL) - MNRS1S3(L,IEL) .GT. 0.) THEN
              FLAGS1S3(L) = .TRUE.
            ENDIF

!           -----------------
!           Flow out of SOM2.
!           -----------------
!           Flow from SOM2 to SOM1
            IF (IMMS2S1(L,IEL) - MNRS2S1(L,IEL) .GT. 0.) THEN
              FLAGS2S1(L) = .TRUE.
            ENDIF

!           Flow from SOM2 to SOM3.
            IF (IMMS2S3(L,IEL) - MNRS2S3(L,IEL) .GT. 0.) THEN
              FLAGS2S3(L) = .TRUE.
            ENDIF

!           -----------------
!           Flow out of SOM3.
!           -----------------
!           Flow from SOM3 to SOM1.
            IF (IMMS3S1(L,IEL) - MNRS3S1(L,IEL).GT. 0.) THEN
              FLAGS3S1(L) = .TRUE.
            ENDIF
          ENDIF   !End of IF block on SRFC layer vs. other layers.
        END DO   !End of IEL loop

!       ----------------------------------------------------------------
!       Apply the reduction factor.
!       ----------------------------------------------------------------
        IF (L .EQ. SRFC) THEN

!         --------------------------------------
!         Flow out of SRFC litter and SRFC SOM1.
!         --------------------------------------
!         Flow from metabolic litter to SOM1.
          IF (FLAGMETS1(SRFC)) THEN
            CFMETS1(SRFC) = CFMETS1(SRFC) * REDUCFACTMIN(1)
            CO2FMET(SRFC) = CO2FMET(SRFC) * REDUCFACTMIN(1)

            DO IEL = 1, NELEM
              EFMETS1(SRFC,IEL)  = EFMETS1(SRFC,IEL) * REDUCFACTMIN(1) 
              IMMMETS1(SRFC,IEL) = IMMMETS1(SRFC,IEL) * REDUCFACTMIN(1) 
              MNRMETS1(SRFC,IEL) = MNRMETS1(SRFC,IEL) * REDUCFACTMIN(1)
            END DO
          ENDIF

!         The lignin and non-lignin fraction of the structural litter
!         decompose together. So if one has to be reduced, the other
!         one has to go also.
          IF (FLAGSTRS1(SRFC) .OR. FLAGSTRS2(SRFC)) THEN
            CFSTRS1(SRFC)        = CFSTRS1(SRFC) * REDUCFACTMIN(1)
            CFSTRS2(SRFC)        = CFSTRS2(SRFC) * REDUCFACTMIN(1)
            CO2FSTR(SRFC,NONLIG) = CO2FSTR(SRFC,NONLIG) *
     &        REDUCFACTMIN(1)
            CO2FSTR(SRFC,LIG)    = CO2FSTR(SRFC,LIG) * REDUCFACTMIN(1)

            DO IEL = 1, NELEM
              EFSTRS1(SRFC,IEL)  = EFSTRS1(SRFC,IEL) * REDUCFACTMIN(1)
              EFSTRS2(SRFC,IEL)  = EFSTRS2(SRFC,IEL) * REDUCFACTMIN(1) 
              IMMSTRS1(SRFC,IEL) = IMMSTRS1(SRFC,IEL) * REDUCFACTMIN(1)
              IMMSTRS2(SRFC,IEL) = IMMSTRS2(SRFC,IEL) * REDUCFACTMIN(1) 
              MNRSTRS1(SRFC,IEL) = MNRSTRS1(SRFC,IEL) * REDUCFACTMIN(1)
              MNRSTRS2(SRFC,IEL) = MNRSTRS2(SRFC,IEL) * REDUCFACTMIN(1)
            END DO
          ENDIF

!         Flow from SOM1 to SOM2.
          IF (FLAGS1S2(SRFC)) THEN
            CFS1S2(SRFC) = CFS1S2(SRFC) * REDUCFACTMIN(1)
            CO2FS1(SRFC) = CO2FS1(SRFC) * REDUCFACTMIN(1)

            DO IEL = 1, NELEM
              EFS1S2(SRFC,IEL)  = EFS1S2(SRFC,IEL) * REDUCFACTMIN(1) 
              IMMS1S2(SRFC,IEL) = IMMS1S2(SRFC,IEL) * REDUCFACTMIN(1)
              MNRS1S2(SRFC,IEL) = MNRS1S2(SRFC,IEL) * REDUCFACTMIN(1)
            END DO
          ENDIF

        ELSE   !Soil layers.
!         -------------------------------------------------
!         Flow out of soil metabolic and structural litter.
!         -------------------------------------------------
!         Flow from metabolic litter to SOM1.
          IF (FLAGMETS1(L)) THEN
            CFMETS1(L) = CFMETS1(L) * REDUCFACTMIN(L)
            CO2FMET(L) = CO2FMET(L) * REDUCFACTMIN(L)

            DO IEL = 1, NELEM
              EFMETS1(L,IEL)  = EFMETS1(L,IEL) * REDUCFACTMIN(L) 
              IMMMETS1(L,IEL) = IMMMETS1(L,IEL) * REDUCFACTMIN(L)
              MNRMETS1(L,IEL) = MNRMETS1(L,IEL) * REDUCFACTMIN(L)
            END DO
          ENDIF

!         The lignin and non-lignin fraction of the structural litter
!         decompose together. So if one has to be reduced, the other
!         one has to go also.
          IF(FLAGSTRS1(L) .OR. FLAGSTRS2(L)) THEN
            CFSTRS1(L)        = CFSTRS1(L) * REDUCFACTMIN(L)
            CFSTRS2(L)        = CFSTRS2(L) * REDUCFACTMIN(L)
            CO2FSTR(L,NONLIG) = CO2FSTR(L,NONLIG) * REDUCFACTMIN(L)
            CO2FSTR(L,LIG)    = CO2FSTR(L,LIG) * REDUCFACTMIN(L)

            DO IEL = 1, NELEM
              EFSTRS1(L,IEL)  = EFSTRS1(L,IEL) * REDUCFACTMIN(L)
              EFSTRS2(L,IEL)  = EFSTRS2(L,IEL) * REDUCFACTMIN(L) 
              IMMSTRS1(L,IEL) = IMMSTRS1(L,IEL) * REDUCFACTMIN(L)
              IMMSTRS2(L,IEL) = IMMSTRS2(L,IEL) * REDUCFACTMIN(L)
              MNRSTRS1(L,IEL) = MNRSTRS1(L,IEL) * REDUCFACTMIN(L)
              MNRSTRS2(L,IEL) = MNRSTRS2(L,IEL) * REDUCFACTMIN(L)
            END DO
          ENDIF

!         -----------------
!         Flow out of SOM1.
!         -----------------
!         Flow from SOM1 to SOM2.
          IF (FLAGS1S2(L)) THEN
!           Keep the original C flow in a temporary variable, because it
!           is still needed for the calculation of the reduced CO2 flow.
!           Then apply the reduction factor.
            CFS1S2OLD = CFS1S2(L)
            CFS1S2(L) = CFS1S2(L) * REDUCFACTMIN(L)

            DO IEL = 1, NELEM
              EFS1S2(L,IEL)  = EFS1S2(L,IEL) * REDUCFACTMIN(L) 
              IMMS1S2(L,IEL) = IMMS1S2(L,IEL) * REDUCFACTMIN(L)
              MNRS1S2(L,IEL) = MNRS1S2(L,IEL) * REDUCFACTMIN(L)
            END DO
          ENDIF

!         Flow from SOM1 to SOM3.
          IF (FLAGS1S3(L)) THEN
!           Keep the original C flow in a temporary variable, because it
!           is still needed for the calculation of the reduced CO2 flow.
!           Then apply the reduction factor.
            CFS1S3OLD = CFS1S3(L)
            CFS1S3(L) = CFS1S3(L) * REDUCFACTMIN(L)

            DO IEL = 1, NELEM
              EFS1S3(L,IEL)  = EFS1S3(L,IEL) * REDUCFACTMIN(L) 
              IMMS1S3(L,IEL) = IMMS1S3(L,IEL) * REDUCFACTMIN(L) 
              MNRS1S3(L,IEL) = MNRS1S3(L,IEL) * REDUCFACTMIN(L)
            END DO
          ENDIF

!         Reduce the CO2 flow out of SOM1.
          IF (FLAGS1S2(L) .AND. FLAGS1S3(L)) THEN
!           If both flows have to be reduced, apply the full reduction
!           factor.
            CO2FS1(L) = CO2FS1(L) * REDUCFACTMIN(L)

          ELSEIF (FLAGS1S2(L) .AND. .NOT. FLAGS1S3(L)) THEN
!           If only one of the two flows has to be reduced, reduce the
!           CO2 flow according to the relative SOM reduction.
            CO2FS1(L) = (CFS1S2(L) + CFS1S3(L)) / (CFS1S2OLD +
     &        CFS1S3(L)) * CO2FS1(L)

          ELSEIF (.NOT. FLAGS1S2(L) .AND. FLAGS1S3(L)) THEN
!           If only one of the two flows has to be reduced, reduce the
!           CO2 flow according to the relative SOM reduction.
            CO2FS1(L) = (CFS1S2(L) + CFS1S3(L)) / (CFS1S2(L) +
     &        CFS1S3OLD) * CO2FS1(L)
          ENDIF

!         -----------------
!         Flow out of SOM2.
!         -----------------
!         Flow from SOM2 to SOM1.
          IF (FLAGS2S1(L)) THEN
!           Keep the original C flow in a temporary variable, because it
!           is still needed for the calculation of the reduced CO2 flow.
!           Then apply the reduction factor.
            CFS2S1OLD = CFS2S1(L)
            CFS2S1(L) = CFS2S1(L) * REDUCFACTMIN(L)

            DO IEL = 1, NELEM
              EFS2S1(L,IEL)  = EFS2S1(L,IEL) * REDUCFACTMIN(L) 
              IMMS2S1(L,IEL) = IMMS2S1(L,IEL) * REDUCFACTMIN(L) 
              MNRS2S1(L,IEL) = MNRS2S1(L,IEL) * REDUCFACTMIN(L)
            END DO
          ENDIF

!         Flow from SOM2 to SOM3.
          IF (FLAGS2S3(L)) THEN
!           Keep the original C flow in a temporary variable, because it
!           is still needed for the calculation of the reduced CO2 flow.
!           Then apply the reduction factor.
            CFS2S3OLD = CFS2S3(L)
            CFS2S3(L) = CFS2S3(L) * REDUCFACTMIN(L)

            DO IEL = 1, NELEM
              EFS2S3(L,IEL)  = EFS2S3(L,IEL) * REDUCFACTMIN(L) 
              IMMS2S3(L,IEL) = IMMS2S3(L,IEL) * REDUCFACTMIN(L) 
              MNRS2S3(L,IEL) = MNRS2S3(L,IEL) * REDUCFACTMIN(L)
            END DO
          ENDIF

!         Reduce the CO2 flow out of SOM2 according to the ratio of
!         the flows to SOM1 and SOM3 (depending on which of the two
!         flows is immobilizing).
          IF (FLAGS2S1(L) .AND. FLAGS2S3(L)) THEN
!           If both flows have to be reduced, apply the full reduction
!           factor.
            CO2FS2(L) = CO2FS2(L) * REDUCFACTMIN(L)

          ELSEIF (FLAGS2S1(L) .AND. .NOT. FLAGS2S3(L)) THEN
!           If only one of the two flows has to be reduced, reduce the
!           CO2 flow according to the relative SOM reduction.
            CO2FS2(L) = (CFS2S1(L) + CFS2S3(L)) / (CFS2S1OLD +
     &        CFS2S3(L)) * CO2FS2(L)

          ELSEIF (.NOT. FLAGS2S1(L) .AND. FLAGS2S3(L)) THEN
!           If only one of the two flows has to be reduced, reduce the
!           CO2 flow according to the relative SOM reduction.
            CO2FS2(L) = (CFS2S1(L) + CFS2S3(L)) / (CFS2S1(L) +
     &        CFS2S3OLD) * CO2FS2(L)
          ENDIF

!         -----------------
!         Flow out of SOM3.
!         -----------------
!         Flow from SOM3 to SOM1.
          IF (FLAGS3S1(L)) THEN
            CFS3S1(L) = CFS3S1(L) * REDUCFACTMIN(L)
            CO2FS3(L) = CO2FS3(L) * REDUCFACTMIN(L)

            DO IEL = 1, NELEM
              EFS3S1(L,IEL)  = EFS3S1(L,IEL) * REDUCFACTMIN(L) 
              IMMS3S1(L,IEL) = IMMS3S1(L,IEL) * REDUCFACTMIN(L)
              MNRS3S1(L,IEL) = MNRS3S1(L,IEL) * REDUCFACTMIN(L)
            END DO
          ENDIF
        ENDIF   !End of IF block on SRFC layer vs. other layers.

!       --------------------------------------------------------------
!       Combine all the fluxes in DLTxxx rate variables.
!       --------------------------------------------------------------
!       --------------------------------
!       Metabolic and structural litter.
!       --------------------------------
        DLTMETABC(L) = DLTMETABC(L) - CFMETS1(L) - CO2FMET(L)
        DLTSTRUCC(L) = DLTSTRUCC(L) - CFSTRS1(L) - CFSTRS2(L) -
     &    CO2FSTR(L,NONLIG) - CO2FSTR(L,LIG)

!       The material that flows from structural to SOM2 is lignin.
        DLTLIGC(L) = DLTLIGC(L) - CFSTRS2(L) - CO2FSTR(L,LIG)

!       Include the E flow from pool A to B, plus the E that is
!       mineralized. E from immobilization adds to the receiving pool;
!       the providing pool has nothing to do with the immobilization.
        DO IEL = 1, NELEM
          DLTMETABE(L,IEL) = DLTMETABE(L,IEL) - EFMETS1(L,IEL)
!           Subtract mineralization.
     &      - MNRMETS1(L,IEL)

          DLTSTRUCE(L,IEL) = DLTSTRUCE(L,IEL) - EFSTRS1(L,IEL) -
     &      EFSTRS2(L,IEL)
!           Subtract mineralization.
     &      - MNRSTRS1(L,IEL) - MNRSTRS2(L,IEL)
        END DO   !End of IEL loop

!       ----
!       SOM1
!       ----
        IF (L .EQ. SRFC) THEN
          DLTSOM1C(SRFC) = DLTSOM1C(SRFC) + CFMETS1(SRFC) +
     &      CFSTRS1(SRFC) - CFS1S2(SRFC) - CO2FS1(SRFC)

          DO IEL = 1, NELEM
            DLTSOM1E(SRFC,IEL) = DLTSOM1E(SRFC,IEL) + EFMETS1(SRFC,IEL)
     &        + EFSTRS1(SRFC,IEL) - EFS1S2(SRFC,IEL)
!             Subtract mineralization.
     &        - MNRS1S2(SRFC,IEL)
!             Add immobilization.
     &        + IMMMETS1(SRFC,IEL) + IMMSTRS1(SRFC,IEL)
            END DO

        ELSE   !Soil layers.

          DLTSOM1C(L) = DLTSOM1C(L) + CFMETS1(L) + CFSTRS1(L) -
     &      CFS1S2(L) - CFS1S3(L) - CO2FS1(L) + CFS2S1(L) + CFS3S1(L)

          DO IEL = 1, NELEM
            DLTSOM1E(L,IEL) = DLTSOM1E(L,IEL) + EFMETS1(L,IEL) +
     &        EFSTRS1(L,IEL) - EFS1S2(L,IEL) - EFS1S3(L,IEL) +
     &        EFS2S1(L,IEL) + EFS3S1(L,IEL)
!             Subtract mineralization.
     &        - MNRS1S2(L,IEL) - MNRS1S3(L,IEL)
!             Add immobilization.
     &        + IMMMETS1(L,IEL) + IMMSTRS1(L,IEL) + IMMS2S1(L,IEL) 
     &        + IMMS3S1(L,IEL)
          END DO   !End of IEL loop

!         ----
!         SOM2
!         ----
!         SOM2 of layer 1 gets input from the SRFC layer also.
          IF (L .EQ. 1) THEN
            DLTSOM2C(L) = DLTSOM2C(L) + CFSTRS2(SRFC) + CFSTRS2(L) +
     &        CFS1S2(SRFC) + CFS1S2(L) - CFS2S1(L) - CFS2S3(L) -
     &         CO2FS2(L)
          ELSE
            DLTSOM2C(L) = DLTSOM2C(L) + CFSTRS2(L) + CFS1S2(L) -
     &         CFS2S1(L) - CFS2S3(L) - CO2FS2(L)
          ENDIF

          DO IEL = 1, NELEM
            IF (L .EQ. 1) THEN
              DLTSOM2E(L,IEL) = DLTSOM2E(L,IEL) + EFSTRS2(SRFC,IEL) +
     &          EFSTRS2(L,IEL) + EFS1S2(SRFC,IEL) + EFS1S2(L,IEL) -
     &          EFS2S1(L,IEL) - EFS2S3(L,IEL)
!               Subtract mineralization.
     &          - MNRS2S1(L,IEL) - MNRS2S3(L,IEL)
!               Add immobilization.
     &          + IMMSTRS2(SRFC,IEL) + IMMSTRS2(L,IEL)
     &          + IMMS1S2(SRFC,IEL) + IMMS1S2(L,IEL)
            ELSE
              DLTSOM2E(L,IEL) = DLTSOM2E(L,IEL) + EFSTRS2(L,IEL) +
     &          EFS1S2(L,IEL) - EFS2S1(L,IEL) - EFS2S3(L,IEL)
!               Subtract mineralization.
     &          - MNRS2S1(L,IEL) - MNRS2S3(L,IEL)
!               Add immobilization.
     &          + IMMSTRS2(L,IEL) + IMMS1S2(L,IEL)
            ENDIF
          END DO   !End of IEL loop

!         ----
!         SOM3
!         ----
          DLTSOM3C(L) = DLTSOM3C(L) + CFS1S3(L) + CFS2S3(L) -
     &      CFS3S1(L) - CO2FS3(L)

          DO IEL = 1, NELEM
            DLTSOM3E(L,IEL) = DLTSOM3E(L,IEL) + EFS1S3(L,IEL) +
     &        EFS2S3(L,IEL) - EFS3S1(L,IEL)
!             Subtract mineralization.
     &        - MNRS3S1(L,IEL)
!             Add immobilization.
     &        + IMMS1S3(L,IEL) + IMMS2S3(L,IEL)
          END DO   !End of IEL loop
        ENDIF   !End of IF block on L=SRFC vs. other soil layers

!AJG 06/22/2003
!       --------------------------------------------------------------
!       Check that no state variables can go negative in the
!       integration by doing here a 'fake' integration.
!       --------------------------------------------------------------
!       Set the reDuction factor back to 1, each day for each layer.
        REDUCF = 1.

!       --------------------------------------------------------------
!       Litter pools.
!       --------------------------------------------------------------
!       Take for all the litter pools the smallest of all the
!       reduction factors.
        IF (METABC(L) + DLTMETABC(L) < 0. .AND. DLTMETABC(L) > 0.) THEN
          REDUCF = ABS (METABC(L) / DLTMETABC(L))
        ENDIF

        IF (STRUCC(L) + DLTSTRUCC(L) < 0. .AND. DLTSTRUCC(L) > 0.) THEN
          REDUCF = MIN (REDUCF, ABS (STRUCC(L) / DLTSTRUCC(L)))
        ENDIF

        IF (LIGC(L) + DLTLIGC(L) < 0. .AND. DLTLIGC(L) > 0.) THEN
          REDUCF = MIN (REDUCF, ABS (LIGC(L) / DLTLIGC(L)))
        ENDIF

!       Apply the reduction factor for the litter pools.
        DLTMETABC(L) = REDUCF * DLTMETABC(L)
        DLTSTRUCC(L) = REDUCF * DLTSTRUCC(L)
        DLTLIGC(L)   = REDUCF * DLTLIGC(L)

!       Do the same for E.
        DO IEL = 1, NELEM
          IF (METABE(L,IEL) + DLTMETABE(L,IEL) < 0. .AND. 
     &            METABE(L,IEL) > 0.) THEN
            REDUCF = ABS (DLTMETABE(L,IEL) / METABE(L,IEL))
          ENDIF

          IF (STRUCE(L,IEL) + DLTSTRUCE(L,IEL) < 0. .AND. 
     &            STRUCE(L,IEL) > 0.) THEN
            REDUCF = MIN (REDUCF, 
     &        ABS (DLTSTRUCE(L,IEL) / STRUCE(L,IEL)))
          ENDIF
        END DO   !End DO IEL

!       --------------------------------------------------------------
!       SOM pools.
!       --------------------------------------------------------------
!       The SOM pools have each their own reduction factor.
        IF (SOM1C(L) + DLTSOM1C(L) < 0.) DLTSOM1C(L) = -SOM1C(L)

        IF (L .NE. 0) THEN
          IF (SOM2C(L) + DLTSOM2C(L) < 0.) DLTSOM2C(L) = -SOM2C(L)
          IF (SOM3C(L) + DLTSOM3C(L) < 0.) DLTSOM3C(L) = -SOM3C(L)
        ENDIF

!       Do the same for E.
        DO IEL = 1, NELEM
          IF (METABE(L,IEL) + DLTMETABE(L,IEL) < 0. .AND.  
     &            METABE(L,IEL) > 0.) THEN
            REDUCF = DLTMETABE(L,IEL) / METABE(L,IEL) 
          ENDIF

          IF (STRUCE(L,IEL) + DLTSTRUCE(L,IEL) < 0. .AND.  
     &            STRUCE(L,IEL) > 0.) THEN
            REDUCF = MIN (REDUCF, 
     &        ABS (DLTSTRUCE(L,IEL) / STRUCE(L,IEL)))
          ENDIF

          IF (SOM1E(L,IEL) + DLTSOM1E(L,IEL) < 0.) THEN
            DLTSOM1E(L,IEL) = -SOM1E(L,IEL)
          ENDIF

          IF (L .NE. 0) THEN
            IF (SOM2E(L,IEL) + DLTSOM2E(L,IEL) < 0.) THEN
              DLTSOM2E(L,IEL) = -SOM2E(L,IEL)
            ENDIF
            IF (SOM3E(L,IEL) + DLTSOM3E(L,IEL) < 0.) THEN
              DLTSOM3E(L,IEL) = -SOM3E(L,IEL)
            ENDIF
          ENDIF
        END DO   !End DO IEL

!       -------------------------------------------------------
!       Total E mineralization or immobilization in this layer.
!       -------------------------------------------------------
!       The MINERALIZE variable used here sums up the mineralization
!       from where it originated, not where it ended up. Thus for the
!       SRFC layer, it goes to MINERALIZE(SRFC,IEL) and not to the
!       layer-1 variable. In the NUPDATE subroutine (called from
!       NTRANS), it is handled where the E goes to.
        DO IEL = 1, NELEM
          IF (L .EQ. SRFC) THEN
            MINERALIZE(SRFC,IEL) = MNRMETS1(SRFC,IEL) + 
     &        MNRSTRS1(SRFC,IEL) + MNRSTRS2(SRFC,IEL) +
     &        MNRS1S2(SRFC,IEL) - IMMMETS1(SRFC,IEL) - 
     &        IMMSTRS1(SRFC,IEL) - IMMSTRS2(SRFC,IEL) -
     &        IMMS1S2(SRFC,IEL)

          ELSE
            MINERALIZE(L,IEL) = MNRMETS1(L,IEL) + MNRSTRS1(L,IEL) +
     &        MNRSTRS2(L,IEL) + MNRS1S2(L,IEL) + MNRS1S3(L,IEL) +
     &        MNRS2S1(L,IEL) + MNRS2S3(L,IEL) + MNRS3S1(L,IEL) -
     &        IMMMETS1(L,IEL) - IMMSTRS1(L,IEL) - IMMSTRS2(L,IEL) -
     &        IMMS1S2(L,IEL) - IMMS1S3(L,IEL) - IMMS2S1(L,IEL) -
     &        IMMS2S3(L,IEL) - IMMS3S1(L,IEL)
          ENDIF
        END DO   !End of IEL loop.
      END DO   !End of layer loop

!***********************************************************************
!***********************************************************************
!     END
!***********************************************************************
      RETURN
      END    !Subroutine IMMOBLIMIT_C

!***********************************************************************
! IMMOBLIMIT_C variables:
!
! CFMETS1     C flow from the metabolic pool to SOM1 (-)
! CFS1S2      C flow from SOM1 to SOM2  (kg[C] / ha)
! CFS1S2OLD   C flow from SOM1 to SOM2 before limiting it because of
!               shortage of E available for immobilization. (kg[C] / ha)
! CFS1S3      C flow from SOM1 to SOM3 (kg[C] / ha)
! CFS1S3OLD   C flow from SOM1 to SOM3 before limiting it because of
!               shortage of E available for immobilization. (kg[C] / ha)
! CFS2S1      C flow from SOM2 to SOM1 (kg[C] / ha)
! CFS2S1OLD   C flow from SOM2 to SOM1 before limiting it because of
!               shortage of E available for immobilization. (kg[C] / ha)
! CFS2S3      C flow from SOM2 to SOM3 (kg[C] / ha)
! CFS2S3OLD   C flow from SOM2 to SOM3 before limiting it because of
!               shortage of E available for immobilization. (kg[C] / ha)
! CFSTRS1     C flow from the structural pool to SOM1 (kg[C] / ha)
! CFSTRS2     C flow from the structural pool to SOM2 (kg[C] / ha)
! CO2FMET     CO2 flow that accompanies the C flow out of the metabolic pool
!               (kg[C] / ha)
! CO2FS1      CO2 flow that accompanies the C flow out of SOM1l (kg[C] / ha)
! CO2FS2      CO2 flow that accompanies the C flow out of SOM12 (kg[C] / ha)
! CO2FS3      CO2 flow that accompanies the C flow out of SOM3l (kg[C] / ha)
! CO2FSTR     CO2 flow that accompanies the C flow out of the structural pool
!               (kg[C] / ha)
! DLTLIGC     Rate variable for the change in lignin C (kg[C] / ha)
! DLTMETABC   Rate variable for the change in metabolic C (kg[C] / ha)
! DLTMETABE   Rate variable for the change in metabolic E (kg[E] / ha)
! DLTSNH4     Rate variable for the change in SNH4 (kg[N] / ha)
! DLTSNO3     Rate variable for the change in SNO3 (kg[N] / ha)
! DLTSOM1C    Rate variable for the change in SOM1 C (kg[C] / ha)
! DLTSOM1E    Rate variable for the change in SOM1 E (kg[E] / ha)
! DLTSOM2C    Rate variable for the change in SOM2 C (kg[C] / ha)
! DLTSOM2E    Rate variable for the change in SOM2 E (kg[E] / ha)
! DLTSOM3C    Rate variable for the change in SOM3 C (kg[C] / ha)
! DLTSOM3E    Rate variable for the change in SOM3 E (kg[E] / ha)
! DLTSTRUCC   Rate variable for the change in structural C (kg[C] / ha)
! DLTSTRUCE   Rate variable for the change in structural E (kg[E] / ha)
! DLTUREA     Rate variable for the change in urea N  (kg[N] / ha)
! EFMETS1     E flow from soil or soil or surface metabolic residue to soil
!               or surface SOM1 (kg[E] / ha)
! EFS1S2      E flow from soil or surface SOM1 to SOM2 (kg[E] / ha)
! EFS1S3      E flow from soil SOM1 to SOM3 (kg[E] / ha)
! EFS2S1      E flow from SOM2 to SOM1 (kg[E] / ha)
! EFS2S3      E flow from SOM2 to SOM3 (kg[E] / ha)
! EFS3S1      E flow from SOM3 to SOM1 (kg[E] / ha)
! EFSTRS1     E flow from soil or surface structural residue to soil or
!               surface SOM1 (kg[E] / ha)
! EFSTRS2     E flow from soil or soil or surface structural residue to SOM2
!               (kg[E] / ha)
! IMMMETS1    Immobilization of E during the flow from soil or surface metabolic
!               residue to soil or surface SOM1  (kg[E] / ha)
! IMMOB       Immobilization of E  (kg[E] / ha)
! IMMS1S2     Immobilization of E during the flow from soil or surface SOM1 to
!               SOM2 (kg[E] / ha)
! IMMS1S3     Immobilization of E during the flow from SOM1 to SOM3 (kg[E] / ha)
! IMMS2S1     Immobilization of E during the flow from SOM2 to SOM1 (kg[E] / ha)
! IMMS2S3     Immobilization of E during the flow from SOM2 to SOM3 (kg[E] / ha)
! IMMS3S1     Immobilization of E during the flow from SOM3 to SOM1 (kg[E] / ha)
! IMMSTRS1    Immobilization of E during the flow from soil or surface structural
!               residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS2    Immobilization of E during the flow from soil or surface structural
!               residue to SOM2  (kg[E] / ha)
! IMMSUMNET   Net immobilization of E by all flows in a layer (kg[E] / ha)
! MINERALIZE  Mineralization of E  (kg[E] / ha)
! MNRMETS1    Mineralization of E during the flow from soil or surface metabolic
!               residue to soil or surface SOM1  (kg[E] / ha)
! MNRS1S2     Mineralization of E during the flow from SOM1 to SOM2 (kg[E] / ha)
! MNRS1S3     Mineralization of E during the flow from SOM1 to SOM3 (kg[E] / ha)
! MNRS2S1     Mineralization of E during the flow from SOM2 to SOM1 (kg[E] / ha)
! MNRS2S3     Mineralization of E during the flow from SOM2 to SOM3 (kg[E] / ha)
! MNRS3S1     Mineralization of E during the flow from SOM3 to SOM1 (kg[E] / ha)
! MNRSTRS1    Mineralization of E during the flow from soil or surface structural
!               to soil or surface SOM1  (kg[E] / ha)
! MNRSTRS2    Mineralization of E during the flow from soil or surface structural
!               residue to SOM2  (kg[E] / ha)
! NELEM       Number of elements: 1 = N, 2 = N+P, 3 = N+P+S (-)
! NLAYR       Number of soil layers (-)
!***********************************************************************
