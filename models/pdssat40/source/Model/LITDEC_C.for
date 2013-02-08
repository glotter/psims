!***********************************************************************
!  LITDEC_C, subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine calculates the decomposition of soil- 
!           and surface-deposited residues.
!
!  Revision history:
!  ........ Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG  Revised, added explanatory text and linked to DSSAT.
!  01/01/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  11/11/2002 AJG Corrected the use of CES1T for layer thickness.
!  06/23/2003 AJG Limit the amount of lignin that decomposes.
!  10/31/2003 AJG Changed the correction of 06/23/2003 and replaced GT,
!                 GE, LT, etc by >, >=, < etc.
!
!  Called: CENTURY
!  Calls : EFLOW_C
************************************************************************

      SUBROUTINE LITDEC_C (
     &  AMINRL, CES1, CES1M, CES1T, CES1X,                !Input
     &  CES2LI, CES2LM, CES2LS, CES2LX,                   !Input
     &  CO2MET, CO2STR, CULMETQ, CULSTRQ,                 !Input
     &  DECMET, DECSTR, DEFAC, DLAYR, DOCULT,             !Input
     &  FRLSTR, L, LIGSTR, METABC, METABE,                !Input
     &  STRUCC, STRUCE,                                   !Input

     &  CFMETS1, CFSTRS1, CFSTRS2, CO2FMET,               !Output
     &  CO2FSTR,EFMETS1, EFSTRS1, EFSTRS2,                !Output
     &  IMMMETS1, IMMSTRS1,IMMSTRS2, MNRMETS1,            !Output
     &  MNRSTRS1, MNRSTRS2,                               !Output

     &  DYNAMIC)                                          !Control

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL DOCULT(0:20)

      INTEGER DYNAMIC, IEL, L, LIG, NONLIG, 
     &  SOIL, SRFC

      PARAMETER (NONLIG = 1, LIG = 2)
      PARAMETER (SRFC = 0, SOIL = 1)

      REAL CEADD, CES1S, CULMETQ,
     &  CULSTRQ, FREMET, FRESTR, TCFLOW

      REAL CFMETS1(0:NL), CFSTRS1(0:NL),
     &  CFSTRS2(0:NL), CO2FMET(0:NL), CO2MET(0:1), 
     &  CULMET(0:NL), CULSTR(0:NL), DECMET(0:1),
     &  DECSTR(0:1), DEFAC(0:NL), DLAYR(NL), FRLSTR(0:NL),
     &  LIGSTR(0:1), METABC(0:NL), STRUCC(0:NL)

      REAL AMINRL(NL,3), CES1(0:NL,3), CES1M(0:1,3),
     &  CES1T(0:1,3), CES1X(0:1,3), CES2L(0:NL,3),
     &  CES2LI(0:1,3), CES2LM(0:1,3), CES2LS(0:1,3),
     &  CES2LX(0:1,3), CO2FSTR(0:NL,2), CO2STR(0:1,2), EFMETS1(0:NL,3),
     &  EFSTRS1(0:NL,3), EFSTRS2(0:NL,3), METABE(0:NL,3),
     &  IMMMETS1(0:NL,3), IMMSTRS1(0:NL,3), IMMSTRS2(0:NL,3),
     &  MNRMETS1(0:NL,3), MNRSTRS1(0:NL,3), MNRSTRS2(0:NL,3),
     &  STRUCE(0:NL,3)

!     The SRFC layer has array index '0' and the soil layers '1' to
!     '20'. If there is no distinction between the different soil
!     layers, array index SOIL (which equals '1') is used. AMINRL,
!     SOM2, SOM3 and the parameters related to these pools do not
!     exist for the surface layer. The SRFC layer uses the mineral N
!     from the topsoil layer.

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
!         Set all the flows to zero. 
          CFMETS1(SRFC) = 0.
          CFSTRS1(SRFC) = 0.
          CFSTRS2(SRFC) = 0.
          DO IEL = 1, NELEM
            EFMETS1(SRFC,IEL) = 0.
            EFSTRS1(SRFC,IEL) = 0.
            EFSTRS2(SRFC,IEL) = 0. 
            IMMMETS1(SRFC,IEL) = 0.
            IMMSTRS1(SRFC,IEL) = 0.
            IMMSTRS2(SRFC,IEL) = 0. 
            MNRMETS1(SRFC,IEL) = 0.
            MNRSTRS1(SRFC,IEL) = 0.
            MNRSTRS2(SRFC,IEL) = 0.
          END DO
          CO2FMET(SRFC) = 0.
          CO2FSTR(SRFC,LIG) = 0.
          CO2FSTR(SRFC,NONLIG) = 0.

!         Set effect of cultivation on decomposition rate.
          IF (DOCULT(SRFC)) THEN
            CULSTR(SRFC) = CULSTRQ
            CULMET(SRFC) = CULMETQ
          ELSE
            CULSTR(SRFC) = 1.
            CULMET(SRFC) = 1.
          ENDIF

!         --------------------------------------------------------------
!         Decomposition of surface metabolic residue.
!         --------------------------------------------------------------
!         If there is surface metabolic residue.
          IF (METABC(SRFC) > 1.E-06) THEN

!           ------------------------------------------------------------
!           C/E ratio of surface SOM1 from surface metabolic.
!           ------------------------------------------------------------
            DO IEL = 1, NELEM
!             If the E concentration of the decomposing surface
!             metabolic residue is greater than a threshold value, set
!             the C/E ratio of the new surface SOM1 to the minimum value
!             allowed.
              FREMET = METABE(SRFC,IEL) / (METABC(SRFC) * 2.5)

              IF (FREMET > CES1T(SRFC,IEL)) THEN
                CES1(SRFC,IEL) = CES1M(SRFC,IEL)
              ELSE
!               Otherwise, interpolate between the minimum and maximum
!               C/E ratio, depending on the E concentration of the
!               decomposing surface metabolic residue.
                CES1S = (CES1M(SRFC,IEL) - CES1X(SRFC,IEL)) /
     &            CES1T(SRFC,IEL)
                CES1(SRFC,IEL) = CES1X(SRFC,IEL) + FREMET * CES1S

!               Make sure that the new C/E ratio is >= the minimum value
!               allowed and =< the maximum value allowed.
                CES1(SRFC,IEL) = AMAX1 (CES1(SRFC,IEL), CES1M(SRFC,IEL))
                CES1(SRFC,IEL) = AMIN1 (CES1(SRFC,IEL), CES1X(SRFC,IEL))
              ENDIF
            END DO   !End of IEL loop.

!           ------------------------------------------------------------
!           C flow from surface metabolic to surface SOM1 and CO2.
!           ------------------------------------------------------------
!           Calculate the total C flow out of the surface metabolic
!           residue.
            TCFLOW = METABC(SRFC) * DEFAC(SRFC) * DECMET(SRFC)
            IF (TCFLOW > METABC(SRFC)) TCFLOW = METABC(SRFC)

!           Calculate the CO2 respiration associated with the C flow
!           from surface metabolic residue to surface SOM1.
            CO2FMET(SRFC) = TCFLOW * CO2MET(SRFC)

!           Correct the C flow from surface metabolic residue to
!           surface SOM1 for the CO2 lost to respiration.
            CFMETS1(SRFC) = TCFLOW - CO2FMET(SRFC)

!           ------------------------------------------------------------
!           E flow from surface metabolic.
!           ------------------------------------------------------------
            DO IEL = 1, NELEM
!             Do the E flow associated with the C flow from surface
!             metabolic to surface SOM1, and also do the E
!             mineralization flow that accompanies this C flow.
              CALL EFLOW_C (
     &          METABC(SRFC), METABE(SRFC,IEL),           !Input
     &          CES1(SRFC,IEL), CFMETS1(SRFC),            !Input
     &          CO2FMET(SRFC),                            !Input
     &          EFMETS1(SRFC,IEL), IMMMETS1(SRFC,IEL),    !Output
     &          MNRMETS1(SRFC,IEL))                       !Output
            END DO
          ENDIF  !End of SRFC metabolic.

!         --------------------------------------------------------------
!         Decomposition of surface structural residue to surface SOM1
!         and SOM2.
!         --------------------------------------------------------------
!         If there is surface structural residue.
          IF (STRUCC(SRFC) > 1.E-06) THEN

!           ------------------------------------------------------------
!           C/E ratio of surface SOM1 and SOM2 from surface structural.
!           ------------------------------------------------------------
            DO IEL = 1, NELEM
!             E concentration of the decomposing surface structural
!             residue
              FRESTR = STRUCE(SRFC,IEL) / (STRUCC(SRFC) * 2.5)

!             If the E concentration of the decomposing surface
!             structural residue is greater than a threshold value, set
!             the C/E ratio of the new surface SOM1 to the minimum value
!             allowed.
              IF (FRESTR > CES1T(SRFC,IEL)) THEN
                CES1(SRFC,IEL) = CES1M(SRFC,IEL)
              ELSE
!               Otherwise, interpolate between the minimum and maximum 
!               C/E ratio, depending on the E concentration of the 
!               decomposing surface structural residue.
                CES1S = (CES1M(SRFC,IEL) - CES1X(SRFC,IEL)) /
     &            CES1T(SRFC,IEL)
                CES1(SRFC,IEL) = CES1X(SRFC,IEL) + FRESTR * CES1S

!               Make sure that the new C/E ratio is >= the minimum value
!               allowed and =< the maximum value allowed.
                CES1(SRFC,IEL) = AMAX1 (CES1(SRFC,IEL), CES1M(SRFC,IEL))
                CES1(SRFC,IEL) = AMIN1 (CES1(SRFC,IEL), CES1X(SRFC,IEL))
              ENDIF

!             Calculate the C/E ratio for new SOM2 from decomposing
!             lignin surface structural residue. This is set equal to
!             the C/E of the surface SOM1 that is newly formed from the
!             non-lignin structural residue, plus a factor that depends
!             on the C/E of SOM1.
              CEADD = CES2LI(SRFC,IEL) + CES2LS(SRFC,IEL) *
     &          (CES1(SRFC,IEL) - CES1M(SRFC,IEL))
              CES2L(SRFC,IEL) = CES1(SRFC,IEL) + CEADD

!             Make sure that the C/E ratio of the new SOM2 from
!             decomposing lignin surface structural residue is greater
!             than the minimum and less than the maximum value allowed.
              CES2L(SRFC,IEL) = AMAX1 (CES2L(SRFC,IEL),CES2LM(SRFC,IEL))
              CES2L(SRFC,IEL) = AMIN1 (CES2L(SRFC,IEL),CES2LX(SRFC,IEL))
            END DO   !End of IEL loop.

!           ------------------------------------------------------------
!           C flow from surface structural to surface SOM1, SOM2 and
!           CO2.
!           ------------------------------------------------------------
!           Calculate the total C flow out of the surface structural
!           residue. The lignin fraction flows to SOM2 of soil layer 1;
!           non-lignin flows to surface SOM1.
            TCFLOW = STRUCC(SRFC) * DEFAC(SRFC) * DECSTR(SRFC) *
     &        EXP(-LIGSTR(SRFC)* FRLSTR(SRFC))
            IF (TCFLOW > STRUCC(SRFC)) TCFLOW = STRUCC(SRFC)

!           Let the surface structural lignin flow to SOM2.
            CFSTRS2(SRFC) = TCFLOW * FRLSTR(SRFC)

!           Calculate the respiration associated with the surface
!           structural lignin flow to SOM2.
            CO2FSTR(SRFC,LIG) = CFSTRS2(SRFC) * CO2STR(SRFC,LIG)

!           Correct the C flow from surface structural lignin to SOM2
!           for the CO2 lost to respiration.
            CFSTRS2(SRFC) = CFSTRS2(SRFC) - CO2FSTR(SRFC,LIG)

! AJG removed on 11/05/2003
!!AJG 06/23/2003
!!           Check that no more lignin decomposes than there is.
!            IF (CFSTRS2(SRFC) > FRLSTR(SRFC) * STRUCC(SRFC)) THEN
!!             Keep the old value for correcting TCFLOW.
!              KEEPOLD = CFSTRS2(SRFC)
!
!!             Limit CFSTRS2 and redo calculations of CO2FSTR.
!              CFSTRS2(SRFC) = FRLSTR(SRFC) * STRUCC(SRFC)
!              CO2FSTR(SRFC,LIG) = CFSTRS2(SRFC) * CO2STR(SRFC,LIG)
!              CFSTRS2(SRFC) = CFSTRS2(SRFC) - CO2FSTR(SRFC,LIG)
!
!!             Limit TCFLOW with the lignin correction.
!              TCFLOW = TCFLOW - KEEPOLD + CFSTRS2(SRFC)
!            ENDIF

!           Calculate the gross C flow from surface structural non-
!           lignin into surface SOM1.
            CFSTRS1(SRFC) = TCFLOW - CFSTRS2(SRFC) - CO2FSTR(SRFC,LIG)

!           Calculate the respiration associated with the flow from
!           surface structural non-lignin into surface SOM1.
            CO2FSTR(SRFC,NONLIG) = CFSTRS1(SRFC) * CO2STR(SRFC,NONLIG)

!           Correct the C flow from surface structural non-lignin to
!           surface SOM1 for the CO2 lost to respiration.
            CFSTRS1(SRFC) = CFSTRS1(SRFC) - CO2FSTR(SRFC,NONLIG)

!           ------------------------------------------------------------
!           E flow from SRFC structural lignin and non-lignin.
!           ------------------------------------------------------------
            DO IEL = 1, NELEM
!             Do the E flow associated with the C flow from surface
!             structural non-lignin to surface SOM1, and also do the E
!             mineralization flow that accompanies this C flow.
              CALL EFLOW_C (
     &          STRUCC(SRFC), STRUCE(SRFC,IEL),           !Input
     &          CES1(SRFC,IEL), CFSTRS1(SRFC),            !Input
     &          CO2FSTR(SRFC,NONLIG),                     !Input
     &          EFSTRS1(SRFC,IEL), IMMSTRS1(SRFC,IEL),    !Output
     &          MNRSTRS1(SRFC,IEL))                       !Output

!             Do the E flow associated with the C flow from surface
!             structural lignin to SOM2, and also do the E
!             mineralization flow that accompanies this C flow.
              CALL EFLOW_C (
     &          STRUCC(SRFC), STRUCE(SRFC,IEL),           !Input
     &          CES2L(SRFC,IEL), CFSTRS2(SRFC),           !Input
     &          CO2FSTR(SRFC,LIG),                        !Input
     &          EFSTRS2(SRFC,IEL), IMMSTRS2(SRFC,IEL),    !Output
     &          MNRSTRS2(SRFC,IEL))                       !Output
            END DO
          ENDIF  !End of SRFC structural

        ELSE   !Do soil layers.
!         --------------------------------------------------------------
!         Daily initializations.
!         --------------------------------------------------------------
!         Set all the flows to zero. 
          CFMETS1(L) = 0.
          CFSTRS1(L) = 0.
          CFSTRS2(L) = 0.
          DO IEL = 1, NELEM
            EFMETS1(L,IEL) = 0.
            EFSTRS1(L,IEL) = 0.
            EFSTRS2(L,IEL) = 0. 
            IMMMETS1(L,IEL) = 0.
            IMMSTRS1(L,IEL) = 0.
            IMMSTRS2(L,IEL) = 0. 
            MNRMETS1(L,IEL) = 0.
            MNRSTRS1(L,IEL) = 0.
            MNRSTRS2(L,IEL) = 0.
          END DO
          CO2FMET(L) = 0.
          CO2FSTR(L,LIG) = 0.
          CO2FSTR(L,NONLIG) = 0.

!         Set effect of cultivation on decomposition rate.
          IF (DOCULT(L)) THEN
            CULSTR(L) = CULSTRQ
            CULMET(L) = CULMETQ
          ELSE
            CULSTR(L) = 1.
            CULMET(L) = 1.
          ENDIF

!         --------------------------------------------------------------
!         Decomposition of soil metabolic residue
!         --------------------------------------------------------------
!         If there is soil metabolic residue.
          IF (METABC(L) > 1.E-06) THEN

!           ------------------------------------------------------------
!           C/E ratio of soil SOM1 from soil metabolic.
!           ------------------------------------------------------------
            DO IEL = 1, NELEM
!             NB: The original CENTURY model only deals with a 20-cm-thick 
!             soil layer, while DSSAT has many layers of variable thickness. 
!             The parameter CES1T (a 'fixed' parameter) is in kg[N]/ha
!             and the comparison it is used for depends on the layer
!             thickness. It is therefore recalculated to a 1-cm-thick layer.
              IF (AMINRL(L,IEL) <= 1.E-06) THEN
!               If no E available in the soil for immobilization by the
!               residue, set the C/E ratio to the maximum value allowed.
                CES1(L,IEL) = CES1X(SOIL,IEL)

!             Compare the AMINRL with CES1T in a 1-cm thick soil layer by
!             dividing both by the layer thickness (20 cm in CENTURY).
              ELSEIF (AMINRL(L,IEL) / DLAYR(L) > 
     &          CES1T(SOIL,IEL) / 20.) THEN
!               If the amount of E available in the soil for
!               immobilization by the residue is more than the critical
!               value, set the C/E ratio to the minimum value allowed.
                CES1(L,IEL) = CES1M(SOIL,IEL)
              ELSE

!               Otherwise, interpolate between the minimum and maximum
!               C/E ratio, depending on the amount of E available in the
!               soil for absorption by the residue.
                CES1S = -(CES1X(SOIL,IEL) - CES1M(SOIL,IEL)) /
     &            (CES1T(SOIL,IEL) / 20.)
                CES1(L,IEL) = CES1X(SOIL,IEL) + CES1S *
     &             AMINRL(L,IEL) / DLAYR(L)

!               Make sure that the new C/E ratio is >= the minimum value
!               allowed and =< the maximum value allowed.
                CES1(L,IEL) = AMAX1 (CES1(L,IEL), CES1M(SOIL,IEL))
                CES1(L,IEL) = AMIN1 (CES1(L,IEL), CES1X(SOIL,IEL))
              ENDIF   !End of CES1 section.
            END DO   !End of IEL loop.

!           ------------------------------------------------------------
!           C flow from soil metabolic to soil SOM1, and CO2.
!           ------------------------------------------------------------
!           Calculate the total C flow out of the soil metabolic
!           residue.
            TCFLOW = METABC(L) * DEFAC(L) * DECMET(SOIL) * CULMET(L)
            IF (TCFLOW > METABC(L)) TCFLOW = METABC(L)

!           Calculate the CO2 respiration associated with the C flow
!           from soil metabolic residue to soil SOM1.
            CO2FMET(L) = TCFLOW * CO2MET(SOIL)

!           Correct the C flow from soil metabolic residue to soil SOM1
!           for the CO2 lost to respiration.
            CFMETS1(L) = TCFLOW - CO2FMET(L)

!           ------------------------------------------------------------
!           E flow from soil metabolic.
!           ------------------------------------------------------------
            DO IEL = 1, NELEM
!             Do the E flow associated with the C flow from soil
!             metabolic to soil SOM1, and also do the E
!             mineralization flow that accompanies this C flow.
              CALL EFLOW_C (
     &          METABC(L), METABE(L,IEL),                 !Input
     &          CES1(L,IEL), CFMETS1(L),                  !Input
     &          CO2FMET(L),                               !Input
     &          EFMETS1(L,IEL), IMMMETS1(L,IEL),          !Output
     &          MNRMETS1(L,IEL))                          !Output
            END DO
          ENDIF   !End of soil metabolic.

!         --------------------------------------------------------------
!         Decomposition of soil structural residue.
!         --------------------------------------------------------------
!         If there is soil structural residue.
          IF (STRUCC(L) > 1.E-06) THEN

!           ------------------------------------------------------------
!           C/E ratio of soil SOM1 and SOM2 from soil structural.
!           ------------------------------------------------------------
            DO IEL = 1, NELEM
!             NB: The original CENTURY model only deals with a 20-cm-thick 
!             soil layer, while DSSAT has many layers of variable thickness. 
!             The parameter CES1T (a 'fixed' parameter) is in kg[N]/ha
!             and the comparison it is used for depends on the layer
!             thickness. It is therefore recalculated to a 1-cm-thick layer.

!             If the amount of E available in the soil for immobilization
!             by the residue is more than the critical value, set the C/E 
!             ratio to the minimum value allowed.
              IF (AMINRL(L,IEL) / DLAYR(L) > 
     &          CES1T(SOIL,IEL) / 20.) THEN
                CES1(L,IEL) = CES1M(SOIL,IEL)

              ELSE
!               Otherwise, interpolate between the minimum and maximum
!               C/E ratio, depending on the amount of E available in the
!               soil for absorption by the residue.
                CES1S = -(CES1X(SOIL,IEL) - CES1M(SOIL,IEL)) /
     &            (CES1T(SOIL,IEL) / 20.)
                CES1(L,IEL) = CES1X(SOIL,IEL) + CES1S *
     &            AMINRL(L,IEL) / DLAYR(L)

!               Make sure that the new C/E ratio is >= the minimum value
!               allowed and =< the maximum value allowed.
                CES1(L,IEL) = AMAX1 (CES1(L,IEL), CES1M(SOIL,IEL))
                CES1(L,IEL) = AMIN1 (CES1(L,IEL), CES1X(SOIL,IEL))
              ENDIF

!             Calculate the C/E ratio for new SOM2 from decomposing
!             lignin soil structural residue. This is set equal to
!             the C/E of the soil SOM1 that is newly formed from the
!             non-lignin soil residue, plus a factor that depends
!             on the C/E of SOM1.
              CEADD = CES2LI(SOIL,IEL) + CES2LS(SOIL,IEL) *
     &         (CES1(L,IEL) - CES1M(SOIL,IEL))
              CES2L(L,IEL) = CES1(L,IEL) + CEADD

!             Make sure that the new C/E ratio is >= the minimum value
!             allowed and =< the maximum value allowed.
              CES2L(L,IEL) = AMAX1 (CES2L(L,IEL), CES2LM(SOIL,IEL))
              CES2L(L,IEL) = AMIN1 (CES2L(L,IEL), CES2LX(SOIL,IEL))
            END DO   !End of IEL loop.

!           ------------------------------------------------------------
!           C flow from soil structural to soil SOM1, SOM2, and CO2.
!           ------------------------------------------------------------
!           Calculate the total C flow out of the soil structural
!           residue.
!             CHP 2/5/2004 added condition to prevent underflows
            IF ((LIGSTR(SOIL) * FRLSTR(L)) .LT. 40.) THEN
              TCFLOW = STRUCC(L) * DEFAC(L) * DECSTR(SOIL) *
     &           EXP(-LIGSTR(SOIL) * FRLSTR(L)) * CULSTR(L)
            ELSE
              TCFLOW = 0.0
            ENDIF

!           Check that no more C decomposes than there is.
            IF (TCFLOW > STRUCC(L)) TCFLOW = STRUCC(L)

!           ------------------------------------------------------------
!           Flow of soil structural lignin to SOM2; non-lignin soil
!           structural residue goes to soil SOM1.
!           ------------------------------------------------------------
!           Let the soil structural lignin flow to SOM2.
            CFSTRS2(L) = TCFLOW * FRLSTR(L)

!           Calculate the respiration associated with the soil
!           structural lignin flow to SOM2.
            CO2FSTR(L,LIG) = CFSTRS2(L) * CO2STR(SOIL,LIG)

!           Correct the C flow from soil structural lignin to SOM2 for
!           the CO2 lost to respiration.
            CFSTRS2(L) = CFSTRS2(L) - CO2FSTR(L,LIG)

! AJG removed on 11/05/2003
!!AJG 06/23/2003
!!           Check that no more lignin decomposes than there is.
!            IF (CFSTRS2(L) > FRLSTR(L) * STRUCC(L)) THEN
!!             Keep the old value for correcting TCFLOW.
!              KEEPOLD = CFSTRS2(L)
!
!!             Limit CFSTRS2 and redo calculations of CO2FSTR.
!              CFSTRS2(L) = FRLSTR(L) * STRUCC(L)
!              CO2FSTR(L,LIG) = CFSTRS2(L) * CO2STR(SOIL,LIG)
!              CFSTRS2(L) = CFSTRS2(L) - CO2FSTR(L,LIG)
!
!!             Limit TCFLOW with the lignin correction.
!              TCFLOW = TCFLOW - KEEPOLD + CFSTRS2(L)
!            ENDIF

!           Calculate the gross C flow from soil structural non-lignin
!           into soil SOM1.
            CFSTRS1(L) = TCFLOW - CFSTRS2(L) - CO2FSTR(L,LIG)

!           Calculate the respiration associated with the flow from
!           soil structural non-lignin into soil SOM1.
            CO2FSTR(L,NONLIG) = CFSTRS1(L) * CO2STR(SOIL,NONLIG)

!           Correct the C flow from soil structural non-lignin to soil
!           SOM1 for the CO2 lost to respiration.
            CFSTRS1(L) = CFSTRS1(L) - CO2FSTR(L,NONLIG)

!           ------------------------------------------------------------
!           E flow from soil structural lignin and non-lignin
!           ------------------------------------------------------------
            DO IEL = 1, NELEM
!             Do the E flow associated with the C flow from soil
!             structural non-lignin to SOM1, and also do the E
!             mineralization flow that accompanies this C flow.
              CALL EFLOW_C (
     &          STRUCC(L), STRUCE(L,IEL),                   !Input
     &          CES1(L,IEL), CFSTRS1(L),                    !Input
     &          CO2FSTR(L,NONLIG),                          !Input
     &          EFSTRS1(L,IEL), IMMSTRS1(L,IEL),            !Output
     &          MNRSTRS1(L,IEL))                            !Output

!             Do the E flow associated with the C flow from soil
!             structural lignin to SOM2, and also do the E
!             mineralization flow that accompanies this C flow.
              CALL EFLOW_C (
     &          STRUCC(L), STRUCE(L,IEL),                   !Input
     &          CES2L(L,IEL), CFSTRS2(L),                   !Input
     &          CO2FSTR(L,LIG),                             !Input
     &          EFSTRS2(L,IEL), IMMSTRS2(L,IEL),            !Output
     &          MNRSTRS2(L,IEL))                            !Output
            END DO
          ENDIF   !End of soil structural
        ENDIF   !End of SRFC vs. soil layers.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

      RETURN
      END    !Subroutine LITDEC_C

!***********************************************************************
! LITDEC_C variables:
!
! AMINRL(LYR,IEL)       Available mineral E in a soil layer (kg[E] / ha)
! CEADD(IEL)            Addition factor for the C/E ratio of newly formed SOM2, coming
!                         from surface structural material or surface SOM1
!                         (kg[C] / kg[E])
! CES1(LYR,IEL)         C/E ratio for newly formed soil or surface SOM1, coming from
!                         soil or surface residue (kg[C] / kg[E])
!                         Also: initial value of the C/E ratio of soil or surface SOM1
!                         (kg[C] / kg[E])
! CES1M(SOIL/SRFC,IEL)  Minimum allowable C/E ratio for newly formed soil or 
!                         surface SOM1, coming from soil or surface residue
!                         (kg[C] / kg[E])
! CES1S(SOIL/SRFC,IEL)  Slope value for the calculation of the C/E ratio of newly
!                         formed SOM1, coming from soil or surface residue
!                         (SOIL: kg[C].ha / kg2[E]; SRFC: kg[C].kg[DM] / kg2[E])
! CES1T(SOIL,IEL)       Threshold value of the soil mineral E content, above which
!                         the C/E ratio of newly formed soil SOM1 equals
!                         CES1M(SOIL,IEL) (kg[E] / ha)
! CES1T(SRFC,IEL)       Threshold value of the E concentration of decomposing surface
!                         residue, above which the C/E ratio of newly formed surface
!                         SOM1 equals CES1M(SRFC,IEL) (kg[E] / kg[DM])
! CES1X(SOIL/SRFC,IEL)  Maximum allowable C/E ratio for newly formed soil or surface
!                          SOM1, coming from soil or surface residue (kg[C] / kg[E])
! CES2L(LYR,IEL)        C/E ratio for newly formed SOM2, coming from soil or surface
!                          structural lignin residue (kg[C] / kg[E])
! CES2LI(SRFC,IEL)      Intercept value for the calculation of the C/E ratio of newly
!                          formed SOM2, coming from surface structural lignin residue 
!                          (kg[C] / kg[E])
! CES2LM(SRFC,IEL)      Minimum allowable C/E ratio for newly formed SOM2, coming from
!                          surface structural lignin residue (kg[C] / kg[E])
! CES2LS(SRFC,IEL)      Slope value for the calculation of the C/E ratio of newly formed
!                         SOM2, coming from surface structural lignin residue (units?)
! CES2LX(SOIL/SRFC,IEL) Maximum allowable C/E ratio for newly formed SOM2, coming from
!                         soil or surface structural lignin residue (kg[C] / kg[E])
! CFMETS1               C flow from the metabolic pool to SOM1 (-)
! CFSTRS1               C flow from the structural pool to SOM1 (kg[C] / ha)
! CFSTRS2               C flow from the structural pool to SOM2 (kg[C] / ha)
! CO2FMET               CO2 flow that accompanies the C flow out of the metabolic pool
!                         (kg[C] / ha)
! CO2FSTR               CO2 flow that accompanies the C flow out of the structural pool
!                         (kg[C] / ha)
! CO2MET(SOIL/SRFC)     C fraction lost to CO2 respiration when soil or surface metabolic
!                         residue decomposes to soil or surface SOM1 (units?)
! CO2STR(SOIL/SRFC)     C fraction lost to CO2 when soil or surface structural non-lignin
!                         residue decomposes to soil of surface SOM1 (units?)
! CULMET(LYR)           Effect of cultivation on the decomposition rate of soil or surface
!                         metabolic residue. Is set to CULME$ when cultivation occurs and
!                         functions as a multiplier on the decomposition rate (-)
! CULMETQ               Effect of cultivation on the decomposition rate of soil or surface
!                         metabolic residue (-)
! CULSTR(LYR)           Effect of cultivation on decomposition rate of soil structural
!                         residue. Is set to CULST$ when cultivation occurs and functions
!                         as a multiplier on the decomposition rate (-)
! CULSTRQ               Effect of cultivation on the decomposition rate of soil structural
!                         residue (-)
! DECMET(SOIL/SRFC)     Maximum decomposition rate of soil or surface metabolic residue
!                         under optimal conditions (but without increased decomposition 
!                         due to soil disturbance; see CULME) (1/d)
! DECSTR(SOIL/SRFC)     Maximum decomposition rate of soil or surface structural residue
!                         under optimal conditions (but without increased decomposition
!                         due to soil disturbance; see CULST) (1/d)
! DEFAC(LYR)            Decomposition factor that represents the effect of temperature
!                         and low soil water conditions on the decomposition rate parameter
!                         functions as a multiplier on the maximum decomposition rate
!                         (DECMET, DECSTR, DECS1, DECS2, DECS3) (range 0-1) (-)
! DYNAMIC               Controls module sequence: DYNAMIC =RUNINIT, SEASINIT, RATE, 
!                         INTEGR, OUTPUT, or FINAL
! EFMETS1               E flow from soil or soil or surface metabolic residue to soil
!                         or surface SOM1 (kg[E] / ha)
! EFSTRS1               E flow from soil or surface structural residue to soil or
!                         surface SOM1 (kg[E] / ha)
! EFSTRS2               E flow from soil or soil or surface structural residue to SOM2
!                         (kg[E] / ha)
! FREMET                E concentration (fraction) of decomposing metabolic residue
!                         (kg[E] / kg[DM])
! FRESTR                E concentration (fraction) of decomposing structural residue
!                         (kg[E] / kg[DM])
! FRLSTR(LYR)           Lignin concentration (fraction) of the structural soil or
!                         surface residue that was already in the system (kg[lignin] / kg[DM])
! IEL                   Element number. 1 = N; 2 = P; 3 = S (-)
! IMMMETS1              Immobilization of E during the flow from soil or surface metabolic
!                         residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS1              Immobilization of E during the flow from soil or surface structural
!                         residue to soil or surface SOM1  (kg[E] / ha)
! IMMSTRS2              Immobilization of E during the flow from soil or surface structural
!                         residue to SOM2  (kg[E] / ha)
! LIG                   Array index indicating whether the variable refers to the lignin
!                         component of the soil or surface structural residue pool or
!                         the non-lignin component (see NONLIG). LIG = 1 (-)
! LIGSTR(SOIL/SRFC)     Effect of lignin on the decomposition rate of soil or surface
!                         structural residue (-)
! METABC                Soil or surface metabolic residue carbon content (kg[C] / ha)
! METABE                Soil or surface metabolic residue E content (kg[E] / ha)
! MNRMETS1              Mineralization of E during the flow from soil or surface metabolic
!                         residue to soil or surface SOM1  (kg[E] / ha)
! MNRSTRS1              Mineralization of E during the flow from soil or surface structural
!                         to soil or surface SOM1  (kg[E] / ha)
! MNRSTRS2              Mineralization of E during the flow from soil or surface structural
!                         residue to SOM2  (kg[E] / ha)
! NELEM                 Number of elements: 1 = N, 2 = N+P, 3 = N+P+S (-)
! NL                    Maximum number of soil layers used in the array definitions (-)
! NONLIG                Array index indicating whether the variable refers to the lignin
!                         component of the soil or surface structural residue pool (see LIG)
!                         or the non-lignin component. NONLIG = 2 (-)
! STRUCC                Soil or surface structural residue carbon content (kg[C] / ha)
! STRUCE                Soil or surface structural residue E content (kg[E] / ha)
! SOIL                  Identifier for the soil layer as a whole (i.e. no individual layers)
!                       . SOIL = 1 (-)
! SRFC                  Identifier for the surface layer. SRFC = 0 (-)
! TCFLOW                Total amount of C flowing from one residue or SOM pool to another
!                         (kg[N] / ha)
!***********************************************************************
