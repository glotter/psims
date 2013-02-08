!***********************************************************************
!  SOMDEC_C, subroutine
!
!  Purpose: This subroutine deals with the carbon and nutrient flows
!          between the different SOM pools (not: the flow out
!          of the litter; see LITDEC), as a consequence of SOM
!          decomposition.
!
!  Revision history:
!  ........ Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG  Revised, added explanatory text and linked to DSSAT.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  11/11/2002 AJG Corrected the use of CES21T and CES3T.
!  10/31/2003 AJG Fixed some remaining comparisons that were done with
!                 the amount of E instead of its concentration and added
!                 some explanatory text.
!
!  Called: CENTURY
!  Calls : EFLOW_C
!***********************************************************************

      SUBROUTINE SOMDEC_C (
     &  AMINRL, CES1, CES1M, CES1T, CES1X,                !Input
     &  CES21I, CES21M, CES21S, CES21T,                   !Input
     &  CES21X, CES3, CES3M, CES3S, CES3T, CES3X,         !Input
     &  CO2S1, CO2S2, CO2S3, CULS1Q, CULS2Q,              !Input
     &  CULS3Q, DECS1, DECS2, DECS3, DEFAC, DLAYR,        !Input
     &  DMOD, DOCULT, L, S1S3, S2S3, SOM1C,               !Input
     &  SOM1E, SOM2C, SOM2E, SOM3C, SOM3E, TXS1,          !Input

     &  CFS1S2, CFS1S3, CFS2S1, CFS2S3, CFS3S1,           !Output
     &  CO2FS1, CO2FS2, CO2FS3, EFS1S2, EFS1S3,           !Output
     &  EFS2S1, EFS2S3, EFS3S1, IMMS1S2, IMMS1S3,         !Output
     &  IMMS2S1, IMMS2S3, IMMS3S1, MNRS1S2, MNRS1S3,      !Output
     &  MNRS2S1, MNRS2S3, MNRS3S1,                        !Output

     &  DYNAMIC)                                          !Control

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
!     The SRFC layer has array index '0' and the soil layers '1' to
!     '20'. If there is no distinction between the different soil
!     layers, array index SOIL (which equals '1') is used. AMINRL,
!     SOM2, SOM3 and the parameters related to these pools do not
!     exist for the surface layer. The SRFC layer uses the AMINRL
!     from the topsoil layer.

      LOGICAL DOCULT(0:20)

      INTEGER DYNAMIC, IEL, L, SOIL, SRFC
      PARAMETER (SRFC = 0, SOIL = 1)

      REAL CEADD, CES1S, CO2S2, CO2S3, CULS1Q,
     &  CULS2Q, CULS3Q, DMOD, RDUMMY, TCFLOW
      PARAMETER (RDUMMY = -99.)

      REAL CFS1S2(0:NL), CFS1S3(NL), CFS2S1(NL), CFS2S3(NL),
     &  CFS3S1(NL), CO2FS1(0:NL), CO2FS2(NL), CO2FS3(NL),
     &  CO2S1(0:NL), CULS1(0:NL), CULS2(NL), CULS3(NL),
     &  DECS1(0:1), DECS2(1), DECS3(1), DEFAC(0:NL), DLAYR(NL),
     &  S1S3(NL), S2S3(NL), SOM1C(0:NL), SOM2C(NL), SOM3C(NL),
     &  TXS1(NL)

      REAL AMINRL(NL,3), CES1(0:NL,3), CES1M(0:1,3),
     &  CES1T(0:1,3), CES1X(0:1,3), CES21(0:NL,3),
     &  CES21I(0:0,3), CES21M(0:1,3), CES21S(0:1,3), CES21T(1,3),
     &  CES21X(0:1,3), CES3(NL,3), CES3M(1,3), CES3S(1,3),
     &  CES3T(1,3), CES3X(1,3), EFS1S2(0:NL,3), EFS1S3(NL,3),
     &  EFS2S1(NL,3), EFS2S3(NL,3), EFS3S1(NL,3), IMMS1S2(0:NL,3),
     &  IMMS1S3(NL,3), IMMS2S1(NL,3), IMMS2S3(NL,3), IMMS3S1(NL,3),
     &  MNRS1S2(0:NL,3), MNRS1S3(NL,3), MNRS2S1(NL,3), MNRS2S3(NL,3),
     &  MNRS3S1(NL,3), SOM1E(0:NL,3), SOM2E(NL,3), SOM3E(NL,3)


!***********************************************************************
!***********************************************************************
!     RATE
!***********************************************************************
      IF (DYNAMIC == RATE) THEN
!     ------------------------------------------------------------------

        IF (L == SRFC) THEN
!         --------------------------------------------------------------
!         Daily initializations.
!         --------------------------------------------------------------
!         Set all the SRFC flows to zero. 
          CFS1S2(SRFC)  = 0.
          DO IEL = 1, NELEM
            EFS1S2(SRFC,IEL)  = 0. 
            IMMS1S2(SRFC,IEL) = 0.
            MNRS1S2(SRFC,IEL) = 0.
          END DO
          CO2FS1(SRFC)  = 0.

!         Set effect of cultivation on decomposition rate.
          IF (DOCULT(SRFC)) THEN
            CULS1(SRFC) = CULS1Q
          ELSE
            CULS1(SRFC) = 1.
          ENDIF

!         --------------------------------------------------------------
!         C/E ratio of new SOM2 from decomposing surface SOM1.
!         --------------------------------------------------------------
          DO IEL = 1, NELEM
!           Calculate C/E ratios for the flow from surface SOM1 to
!           SOM2 (layer 1). The C/E of new SOM2 equals C/E of old
!           surface SOM1 plus a factor that depends on the C/E of
!           SOM1.
            IF (SOM1E(SRFC,IEL) > 0.00001) THEN
              CEADD = CES21I(SRFC,IEL) + CES21S(SRFC,IEL) *
     &          (SOM1C(SRFC) / SOM1E(SRFC,IEL) - CES1M(SRFC,IEL))
              CES21(SRFC,IEL) = SOM1C(SRFC) / SOM1E(SRFC,IEL) + CEADD
            ELSE
              CEADD = 0.0
              CES21(SRFC,IEL) = 0.0
            ENDIF

!           Make sure that the new C/E ratio is >= the minimum value
!           allowed and =< the maximum value allowed.
            CES21(SRFC,IEL) = AMAX1 (CES21(SRFC,IEL), CES21M(SRFC,IEL))
            CES21(SRFC,IEL) = AMIN1 (CES21(SRFC,IEL), CES21X(SRFC,IEL))
          END DO   !End of IEL loop.

!         --------------------------------------------------------------
!         Decomposition of surface SOM1 to SOM2, and to CO2.
!         --------------------------------------------------------------
!         If there is surface SOM1.
          IF (SOM1C(SRFC) > 1.E-06) THEN

!           Calculate the total C flow out of the surface SOM1 (the SRFC
!           layer is not affected by DMOD).
            TCFLOW = SOM1C(SRFC) * DEFAC(SRFC) * DECS1(SRFC) *
     &        CULS1(SRFC)
            IF (TCFLOW > SOM1C(SRFC)) TCFLOW = SOM1C(SRFC)

!           Calculate the CO2 respiration associated with the C flow
!           from surface SOM1 to SOM2 (layer 1).
            CO2FS1(SRFC) = TCFLOW * CO2S1(SRFC)

!           Correct the C flow from surface SOM1 to SOM2 for the CO2
!           lost to respiration.
            CFS1S2(SRFC) = TCFLOW - CO2FS1(SRFC)

            DO IEL = 1, NELEM
!             Do the E flow associated with the C flow from surface SOM1
!             to SOM2, and also do the E mineralization that goes with
!             this flow and with the CO2 respiration.

!For P, don't repeat the mineralization due to CO2 respiration!! -> 
!Use a dummy.
              CALL EFLOW_C (
     &          SOM1C(SRFC), SOM1E(SRFC,IEL),             !Input
     &          CES21(SRFC,IEL), CFS1S2(SRFC),            !Input
     &          CO2FS1(SRFC),                             !Input
     &          EFS1S2(SRFC,IEL), IMMS1S2(SRFC,IEL),      !Output
     &          MNRS1S2(SRFC,IEL))                        !Output
            END DO
          ENDIF  !End of surface SOM1 section.

        ELSE     !Continue with soil layers.

!         --------------------------------------------------------------
!         Daily initializations.
!         --------------------------------------------------------------
!         Set all the soil flows to zero.
          CFS1S2(L) = 0.
          CFS1S3(L) = 0.
          CFS2S1(L) = 0.
          CFS2S3(L) = 0.
          CFS3S1(L) = 0.
          DO IEL = 1, NELEM
            EFS1S2(L,IEL) = 0.
            EFS1S3(L,IEL) = 0.
            EFS2S1(L,IEL) = 0.
            EFS2S3(L,IEL) = 0.
            EFS3S1(L,IEL) = 0. 
            IMMS1S2(L,IEL) = 0.
            IMMS1S3(L,IEL) = 0.
            IMMS2S1(L,IEL) = 0.
            IMMS2S3(L,IEL) = 0.
            IMMS3S1(L,IEL) = 0.
            MNRS1S2(L,IEL) = 0.
            MNRS1S3(L,IEL) = 0.
            MNRS2S1(L,IEL) = 0.
            MNRS2S3(L,IEL) = 0.
            MNRS3S1(L,IEL) = 0.
          END DO
          CO2FS1(L) = 0.
          CO2FS2(L) = 0.
          CO2FS3(L) = 0.

!         Set effect of cultivation on decomposition rate.
          IF (DOCULT(L)) THEN
            CULS1(L) = CULS1Q
            CULS2(L) = CULS2Q
            CULS3(L) = CULS3Q
          ELSE
            CULS1(L) = 1.
            CULS2(L) = 1.
            CULS3(L) = 1.
          ENDIF


          DO IEL = 1, NELEM
!           ------------------------------------------------------------
!           C/E ratio of new soil SOM1 from decomposing SOM2 or SOM3.
!           ------------------------------------------------------------
            IF (AMINRL(L,IEL) <= 1.E-06) THEN
!             If there is no mineral E available in the soil for
!             absorption by the decomposing SOM2 or SOM3, set the C/E
!             ratio of the new soil SOM1 to the maximum value allowed.
              CES1(L,IEL) = CES1X(SOIL,IEL)

!           Compare the AMINRL with CES1T in a 1-cm thick soil layer by
!           dividing both by the layer thickness (20 cm in CENTURY).
            ELSEIF (AMINRL(L,IEL) / DLAYR(L) >
     &        CES1T(SOIL,IEL) / 20.) THEN
!             If the amount of mineral E available in the soil for
!             absorption by the decomposing SOM2 or SOM3 is greater than
!             the critical value, set the C/E ratio of the new soil SOM1
!             to the minimum value allowed.
              CES1(L,IEL) = CES1M(SOIL,IEL)

!           Otherwise, interpolate between the minimum and maximum C/E
!           ratio depending on the amount of mineral E available in
!           the soil for absorption by the decomposing SOM2 or SOM3.
            ELSE
!             Calculate CES1S to a 1-cm thick soil layer by dividing
!             CES1T by the layer thickness (20 cm in CENTURY).
              CES1S = -(CES1X(SOIL,IEL) - CES1M(SOIL,IEL)) / 
     &          (CES1T(SOIL,IEL) / 20.)

!             Set CES1.
              CES1(L,IEL) = CES1X(SOIL,IEL) + CES1S * 
     &           (AMINRL(L,IEL) / DLAYR(L))

!             Make sure that the new C/E ratio is >= the minimum value
!             allowed and =< the maximum value allowed.
              CES1(L,IEL) = AMAX1 (CES1(L,IEL), CES1M(SOIL,IEL))
              CES1(L,IEL) = AMIN1 (CES1(L,IEL), CES1X(SOIL,IEL))
            ENDIF  !End of IF block on CES1.

!           ------------------------------------------------------------
!           C/E ratio of new SOM2 from decomposing soil SOM1.
!           ------------------------------------------------------------
!           NB: The original CENTURY model only deals with a 20-cm-thick 
!           soil layer, while DSSAT has many layers of variable thickness. 
!           The parameter CES21T (a 'fixed' parameter) is in kg[N]/ha
!           and the comparison it is used for depends on the layer
!           thickness. It is therefore recalculated to a 1-cm-thick layer.
            IF (AMINRL(L,IEL) <= 1.E-06) THEN
!             If there is no mineral E available in the layer for
!             absorption by the decomposing soil SOM1, set the C/E ratio
!             of the new SOM2 to the maximum value allowed.
              CES21(L,IEL) = CES21X(SOIL,IEL)

!           Compare the AMINRL with CES21T in a 1-cm thick soil layer by
!           dividing both by the layer thickness (20 cm in CENTURY).
            ELSEIF (AMINRL(L,IEL) / DLAYR(L) > 
     &        CES21T(SOIL,IEL) / 20.) THEN
!             If the amount of mineral E available in the layer for
!             absorption by the decomposing soil SOM1 is greater than
!             the critical value, set the C/E ratio of the new SOM2 to
!             the minimum value allowed.
              CES21(L,IEL) = CES21M(SOIL,IEL)

            ELSE
!             Otherwise, interpolate between the minimum and maximum
!             C/E, depending on the amount of mineral E available in 
!             the layer for absorption by the decomposing soil SOM1.
              CES21S(SOIL,IEL) = -(CES21X(SOIL,IEL) - CES21M(SOIL,IEL))
     &          / (CES21T(SOIL,IEL) / 20.)
              CES21(L,IEL) = CES21X(SOIL,IEL) + CES21S(SOIL,IEL) *
     &          AMINRL(L,IEL) / DLAYR(L)

!             Make sure that the new C/E ratio is >= the minimum value
!             allowed and =< the maximum value allowed.
              CES21(L,IEL) = AMAX1 (CES21(L,IEL), CES21M(SOIL,IEL))
              CES21(L,IEL) = AMIN1 (CES21(L,IEL), CES21X(SOIL,IEL))
            ENDIF  !End of IF block on CES21.

!           ------------------------------------------------------------
!           C/E ratio of new SOM3 from decomposing soil SOM1 or SOM2.
!           ------------------------------------------------------------
!           NB: The original CENTURY model only deals with a 20-cm-thick 
!           soil layer, while DSSAT has many layers of variable thickness. 
!           The parameter CES3T (a 'fixed' parameter) is in kg[N]/ha
!           and the comparison it is used for depends on the layer
!           thickness. It is therefore recalculated to a 1-cm-thick layer.
            IF (AMINRL(L,IEL) <= 1.E-06) THEN
!             If there is no mineral E available in the layer for
!             absorption by the decomposing soil SOM1 or SOM2, set the
!             C/E ratio of the new SOM3 to the maximum value allowed.
              CES3(L,IEL) = CES3X(SOIL,IEL)

!           Compare the AMINRL with CES1T in a 1-cm thick soil layer by
!           dividing both by the layer thickness (20 cm in CENTURY).
            ELSEIF (AMINRL(L,IEL) / DLAYR(L) > 
     &        CES3T(SOIL,IEL) / 20.) THEN
!             If the amount of mineral E available in the layer for
!             absorption by the decomposing soil SOM1 or SOM2 is greater
!             than the critical value, set the C/E ratio of the new SOM3
!             to the minimum value allowed.
              CES3(L,IEL) = CES3M(SOIL,IEL)

            ELSE
!             Otherwise, interpolate between the minimum and maximum C/E
!             ratio depending on the amount of mineral E available in
!             the layer for absorption by the decomposing soil SOM1 or
!             SOM2.
              CES3S(SOIL,IEL) = -(CES3X(SOIL,IEL) - CES3M(SOIL,IEL)) /
     &          (CES3T(SOIL,IEL) / 20.)
              CES3(L,IEL) = CES3X(SOIL,IEL) + CES3S(SOIL,IEL) *
     &          AMINRL(L,IEL) / DLAYR(L)

!             Make sure that the new C/E ratio is >= the minimum value
!             allowed and =< the maximum value allowed.
              CES3(L,IEL) = AMAX1 (CES3(L,IEL), CES3M(SOIL,IEL))
              CES3(L,IEL) = AMIN1 (CES3(L,IEL), CES3X(SOIL,IEL))
            ENDIF   !End of IF block on CES3.
          END DO   !End of loop on C/E ratios.


!         --------------------------------------------------------------
!         Decomposition of soil SOM1 to SOM2 + SOM3, and to CO2.
!         --------------------------------------------------------------
          IF (SOM1C(L) > 1.E-06) THEN

!           Calculate the total C flow out of soil SOM1.
            TCFLOW = SOM1C(L) * DEFAC(L) * DECS1(SOIL) * CULS1(L) *
     &        TXS1(L) * DMOD
            IF (TCFLOW > SOM1C(L)) TCFLOW = SOM1C(L)

!           Calculate the CO2 respiration associated with the C flow
!           from soil SOM1 to SOM2 and SOM3.
            CO2FS1(L) = TCFLOW * CO2S1(L)

!           Calculate the C flow from soil SOM1 to SOM3 (the fraction
!           to SOM3 is texture dependent).
!           NB: In contrast to other C flows, TCFLOW is not first
!           reduced by the CO2 loss before dividing it over CFS1S2 and
!           CFS1S3, because the CO2 loss only refers to microbial
!           processes.
            CFS1S3(L) = TCFLOW * S1S3(L)

!           Calculate the C flow from soil SOM1 to SOM2: SOM2 gets
!           what is left over of TCFLOW.
            CFS1S2(L) = TCFLOW - CO2FS1(L) - CFS1S3(L)

            DO IEL = 1, NELEM
!             Do the E flow associated with the C flow from soil SOM1
!             to SOM2, and also do the E mineralization that goes with
!             this flow and with the CO2 respiration.
              CALL EFLOW_C (
     &          SOM1C(L), SOM1E(L,IEL),                   !Input
     &          CES21(L,IEL), CFS1S2(L),                  !Input
     &          CO2FS1(L),                                !Input
     &          EFS1S2(L,IEL), IMMS1S2(L,IEL),            !Output
     &          MNRS1S2(L,IEL))                           !Output

!             Do the E flow associated with the C flow from soil SOM1
!             to SOM3, and also do the E mineralization that goes with
!             this flow.
!             NB: The E mineralization that goes with the CO2 flow has
!             already been dealt with when handling the flow from SOM1
!             to SOM2, so put here a dummy with value -99.
              CALL EFLOW_C (
     &          SOM1C(L), SOM1E(L,IEL),                   !Input
     &          CES3(L,IEL), CFS1S3(L),                   !Input
     &          RDUMMY,                                   !Input
     &          EFS1S3(L,IEL), IMMS1S3(L,IEL),            !Output
     &          MNRS1S3(L,IEL))                           !Output
            END DO   !End of IEL loop.
          ENDIF  !End of soil SOM1 section

!         --------------------------------------------------------------
!         Decomposition of SOM2 to SOM3 + soil SOM1 and to CO2.
!         --------------------------------------------------------------
          IF (SOM2C(L) > 1.E-06) THEN

!           Calculate the total C flow out of SOM2.
            TCFLOW = SOM2C(L) * DEFAC(L) * DECS2(SOIL) * CULS2(L) *
     &        DMOD
            IF (TCFLOW > SOM2C(L)) TCFLOW = SOM2C(L)

!           Calculate the CO2 respiration associated with the C flow
!           from SOM2 to soil SOM1 + SOM3.
            CO2FS2(L) = TCFLOW * CO2S2

!           Calculate the C flow from SOM2 to SOM3.
!           NB: In contrast to other C flows, TCFLOW is not first
!           reduced by the CO2 loss before dividing it over CFS2S3 and
!           CFS2S1, because the CO2 loss only refers to microbial
!           processes.
            CFS2S3(L) = TCFLOW * S2S3(L)

!           Calculate the C flow from SOM2 to soil SOM1.
            CFS2S1(L) = TCFLOW - CO2FS2(L) - CFS2S3(L)

            DO IEL = 1, NELEM
!             Do the E flow associated with the C flow from SOM2 to
!             soil SOM1, and also do the E mineralization that goes
!             with this flow and with the CO2 respiration.
              CALL EFLOW_C (
     &          SOM2C(L), SOM2E(L,IEL),                   !Input
     &          CES1(L,IEL), CFS2S1(L),                   !Input
     &          CO2FS2(L),                                !Input
     &          EFS2S1(L,IEL), IMMS2S1(L,IEL),            !Output
     &          MNRS2S1(L,IEL))                           !Output

!             Ditto for the flow from SOM2 to SOM3.
!             NB: The E mineralization that goes with the CO2 flow has
!             already been dealt with when handling the flow from SOM2
!             to soil SOM1, so put here a dummy with value -99.
              CALL EFLOW_C (
     &          SOM2C(L), SOM2E(L,IEL),                   !Input
     &          CES3(L,IEL), CFS2S3(L),                   !Input
     &          RDUMMY,                                   !Input
     &          EFS2S3(L,IEL), IMMS2S3(L,IEL),            !Output
     &          MNRS2S3(L,IEL))                           !Output
            END DO   !End of IEL loop.
          ENDIF  !End of SOM2 section

!         --------------------------------------------------------------
!         Decomposition of SOM3 to soil SOM1, and to CO2.
!         --------------------------------------------------------------
          IF (SOM3C(L) > 1.E-06) THEN

!           Calculate the total C flow out of SOM3C.
            TCFLOW = SOM3C(L) * DEFAC(L) * DECS3(SOIL) * CULS3(L) * DMOD
            IF (TCFLOW > SOM3C(L)) TCFLOW = SOM3C(L)

!           Calculate the CO2 respiration associated with the C flow
!           from SOM3 to soil SOM1.
            CO2FS3(L) = TCFLOW * CO2S3

!           Correct the C flow from SOM3 to soil SOM1 for the CO2 lost
!           to respiration.
            CFS3S1(L) = TCFLOW - CO2FS3(L)

            DO IEL = 1, NELEM
!             Do the E flow associated with the C flow from SOM3 to soil
!             SOM1, and also do the E mineralization that goes with this
!             flow and with the CO2 respiration.
              CALL EFLOW_C (
     &          SOM3C(L), SOM3E(L,IEL),                   !Input
     &          CES1(L,IEL), CFS3S1(L),                   !Input
     &          CO2FS3(L),                                !Input
     &          EFS3S1(L,IEL), IMMS3S1(L,IEL),            !Output
     &          MNRS3S1(L,IEL))                           !Output
            END DO
          ENDIF  !End of SOM3 section.
        ENDIF   !End of SRFC vs. Soil layers.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

      RETURN
      END    !Subroutine SOMDEC_C
