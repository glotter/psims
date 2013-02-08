!***********************************************************************
!  SENESADD_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Add the senesced material daily to the residue pools for
!           the CENTURY-based SOM model.
!
!  REVISION HISTORY
!  01/01/1999 AJG Written.
!  03/26/2000 CHP/AJG Integrated the CENTURY-based and CERES-based SOM
!               modules with CHP's modular format.
!  03/12/2003 CHP Changed senescence variable to composite (SENESCE)
!                   as defined in ModuleDefs.for
!
!  Called: NTRANS_C
!  Calls : PARTIT_C
!***********************************************************************

      SUBROUTINE SENESADD_C (DYNAMIC, 
     &  AMINRL, CEDAM, CESTR, CROP, DLAYR, FRDAE,         !Input
     &  FRMETI, FRMETS, NLAYR, RESDAX, SENESCE, SNH4,     !Input
     &  ADDMETABEFLAG, DLTLIGC, DLTMETABC,                !Output
     &  DLTMETABE, DLTSNH4, DLTSNO3, DLTSTRUCC,           !Output
     &  DLTSTRUCE, FRMETFLAG, SENESSUMC, SENESSUMN)       !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      LOGICAL ADDMETABEFLAG, FRMETFLAG

      CHARACTER*2  CROP

      INTEGER DYNAMIC, L, LAYER, NLAYR 
      INTEGER, PARAMETER :: SRFC = 0, N = 1

      REAL ADDC, ADDLIG, ADDN, FRMETI, FRMETS, RESDAX,
     &  SENESSUMC, SENESSUMN
      REAL CEDAM(NELEM), CESTR(NELEM), DLAYR(NL), DLTLIGC(0:NL),
     &  DLTMETABC(0:NL), DLTMETABE(0:NL,NELEM), 
     &  DLTSTRUCC(0:NL), FRDAE(NELEM),
     &  FRLRES(0:NL), RESC(0:NL), RESE(0:NL,NELEM)
      REAL AMINRL(NL,NELEM), DLTSTRUCE(0:NL,NELEM), RESCE(0:NL,NELEM)

      REAL, DIMENSION(NL) :: SNH4, DLTSNH4, DLTSNO3
!-----------------------------------------------------------------------
!     Constructed variables are defined in ModuleDefs.
      TYPE (ResidueType) SENESCE  

!***********************************************************************
!***********************************************************************
!     RUN INITIALIZATION - CALLED ONCE PER SIMULATION
!***********************************************************************
      IF (DYNAMIC == SEASINIT) THEN
!     ------------------------------------------------------------------
!       Initialize the cumulative senescence for the new season at zero.
        SENESSUMC = 0.
        SENESSUMN = 0.

!***********************************************************************
!***********************************************************************
!     RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC == RATE) THEN
!     ------------------------------------------------------------------
!       If it is a fallow treatment, there is no senescence.
        IF (CROP == 'FA') RETURN

!       -----------------
!       Shoot senescence.
!       -----------------
!       Senesced leaves+stems+shells+seeds. The organ damage may be due
!       age senescence, water-stress damage, freeze damage and pest
!       damage; for some organs all of these hold, but for others not.
!       All of these, minus the pest damage (this does not add to the
!       soil), should be included.
        RESE = SENESCE % RESE
!       The senescence variables are calculated in the plant growth modules.
        ADDC   = SENESCE % ResWt(0) * 0.40            !kg[C]/ha
        ADDLIG = SENESCE % ResLig(0)                  !kg[lignin]/ha
        ADDN   = SENESCE % ResE(0,N)                  !kg[N]/ha

!       Residue C to be added.
        RESC(SRFC) = ADDC

!       Calculate senesced-leaf C/E ratio.
        IF (ADDN > 1E-5) THEN
          RESCE(SRFC,N) = ADDC / ADDN
        ELSE
          RESCE(SRFC,N) = 0.0
        ENDIF

!         Set the lignin concentration of the senesced material.
        IF (ADDC > 1E-5) THEN
          FRLRES(SRFC) = ADDLIG / (ADDC * 2.5)
        ELSE
          FRLRES(SRFC) = 0.0
        ENDIF

!       If L is transferred to PARTIT via the subroutine's parameter
!       string, it may go wrong because L is the DO loop counter.
!       Therefore, L is first copied to LAYER.
        LAYER = SRFC

!       Add the senesced material to the structural and metabolic
!       residue pools.
        CALL PARTIT_C (
     &    AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRLRES,   !Input
     &    FRMETI, FRMETS, LAYER, RESC,                  !Input
     &    RESCE, RESDAX, SNH4,                          !Input
     &    DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,       !Input/Output
     &    DLTSNO3, DLTSTRUCC, DLTSTRUCE,                !Input/Output
     &    ADDMETABEFLAG, FRMETFLAG)                     !Output

!       Sum up all C, N and P in senesced parts added to the soil for
!       the C, N and P balance.
        SENESSUMC = SENESSUMC + ADDC
        SENESSUMN = SENESSUMN + ADDN

!       Set back to zero after the residue has been added.
        ADDC = 0.
        ADDN = 0.

!       -------------------------
!       Root + nodule senescence.
!       -------------------------
!       Total root length is only calculated in ROOTDM (a pest routine),
!       which is not used if the pest option is not selected. So do it
!       here also.
        DO L = 1, NLAYR
!         Senesced roots and nodules are not calculated by soil layer,
!         so distribute them according to the root distribution.
          ADDC   = SENESCE % ResWt(L) * 0.40            !kg[C]/ha
          ADDLIG = SENESCE % ResLig(L)                  !kg[lignin]/ha
          ADDN   = SENESCE % ResE(L,N)                  !kg[N]/ha

!           Multiply by 10 to convert from g/m2 (plant) to
!           kg/ha (soil).
          RESC(L)    = ADDC
          IF (ADDN > 1E-5) THEN
            RESCE(L,N) = RESC(L) / ADDN
          ELSE
            RESCE(L,N) = 0.0
          ENDIF

!         Set the lignin concentration of the senesced material.
          IF (ADDC > 1E-5) THEN
            FRLRES(L) = ADDLIG / (ADDC * 2.5)
          ELSE
            FRLRES(L) = 0.0
          ENDIF

!         If L is transferred via the subroutine's 
!         parameter string, it may go wrong, because L is the
!         DO loop counter. Therefore, L is first copied to LAYER.
          LAYER = L

!         Add the senesced material to the structural and metabolic
!         residue pools.
          CALL PARTIT_C (
     &      AMINRL, CEDAM, CESTR, DLAYR, FRDAE,       !Input
     &      FRLRES, FRMETI, FRMETS, LAYER,            !Input
     &      RESC, RESCE, RESDAX, SNH4,                !Input
     &      DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,   !Input/Output
     &      DLTSNO3, DLTSTRUCC, DLTSTRUCE,            !Input/Output
     &      ADDMETABEFLAG, FRMETFLAG)                 !Output

!         Sum up all C, N and P in senesced parts added to the soil for
!         the C, N and P balance.
          SENESSUMC = SENESSUMC + ADDC
          SENESSUMN = SENESSUMN + ADDN

!         Set back to zero after the residue has been added.
          ADDC = 0.
          ADDN = 0.
        END DO   !End of soil layer loop.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

      RETURN
      END Subroutine SENESADD_C

