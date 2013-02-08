!***********************************************************************
!  PARTIT_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: This subroutine calculates the amount of carbon,
!           nutrients and lignin in newly added residues that goes
!           to the structural residue pool, and the amount that goes
!           to the metabolic residue pool.
!
!  Revision history:
!  ........       Parton et al.  Written for CENTURY model.
!  01/01/1999 AJG Revised and linked to DSSAT.
!  11/11/2002 AJG Corrected RESDAX for layer thickness.
!  03/26/2003 GH  Modified file name
!  01/19/2006 CHP Added checks to prevent negative N
!
!  Called: INSEQ_C, RPLACE_C, SENESADD_C, SOILNI_C
!  Calls : NUPDATE_C
!***********************************************************************
      SUBROUTINE PARTIT_C (
     &  AMINRL, CEDAM, CESTR, DLAYR, FRDAE, FRLRES,       !Input
     &  FRMETI, FRMETS, LAYER, RESC,                      !Input
     &  RESCE, RESDAX, SNH4,                              !Input

     &  DLTLIGC, DLTMETABC, DLTMETABE, DLTSNH4,           !Input/Output
     &  DLTSNO3, DLTSTRUCC, DLTSTRUCE,                    !Input/Output

     &  ADDMETABEFLAG, FRMETFLAG)                         !Output

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL, NELEM defined in ModuleDefs.for

      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------
      LOGICAL ADDMETABEFLAG, FRMETFLAG

      INTEGER IEL, L, LAYER, N, SRFC
      PARAMETER (N = 1)
      PARAMETER (SRFC = 0)

!     MINERALIZE is a local variable here (though it also exists
!     elsewhere).
      REAL ADDLIGC, ADDMETABC, ADDMETABE, ADDSTRUCC, ADDSTRUCE,
     &  DIRABS, FRMET, FRMETI, FRMETS, FRNRES, LNRES, MINERALIZE,
     &  RESDAX

      REAL CEDAM(3), CESTR(3), DLAYR(NL), DLTLIGC(0:NL),
     &  DLTMETABC(0:NL), DLTSNH4(NL), DLTSNO3(NL), DLTSTRUCC(0:NL),
     &  FRDAE(3), FRLRES(0:NL), RESC(0:NL), SNH4(NL)

      REAL AMINRL(NL,3), AMINRL_AVAIL(NL,3), DLTMETABE(0:NL,3),
     &  DLTSTRUCE(0:NL,3), RESCE(0:NL,3), RESE(0:NL,3)

      REAL TMINERALIZE, TIMMOBILIZE


!     Copy LAYER back to L.
      L = LAYER

!     If no new residue has been applied, go back.
      IF (RESC(L) .LT. 0.001) RETURN

      DO IEL = 1, NELEM
        IF (RESCE(L,IEL) > 0.0) THEN
!         Calculate the amount of E in the newly added residue.
          RESE(L,IEL) = RESC(L) / RESCE(L,IEL)
        ELSE
          RESE(L,IEL) = RESC(L) / 40.0  
        ENDIF
      ENDDO

!     ----------------------------------------------------------------
!     Direct absorption of E by the residue, starting with a temporary
!     integration of NH4 and NO3.
!     ----------------------------------------------------------------
!     For the SRFC layer there is no direct absorption as the contact
!     of the litter with the soil is limited.
      IF (L .NE. SRFC) THEN

!       The integration of the DLTxxx variables will be done elsewhere
!       at the end of the time step. But because residue partitioning
!       done here depends on the amount of N absorbed by the residue,
!       the AMINRL available for direct absorption should be determined
!       here by a temporary integration of DLTSNH4 and DLTSNO3 into a
!       variable AMINRL_AVAIL (only use negative DLTSNH4 and DLTSNO3
!       values). This is a protection against negative values at the
!       integration step.
        DO IEL = 1, NELEM
          IF (IEL .EQ. N) THEN
            AMINRL_AVAIL(L,IEL) = AMINRL(L,IEL) +
     &        AMIN1 (DLTSNH4(L), 0.) + AMIN1 (DLTSNO3(L), 0.)
          ENDIF
!To be done for P also.
          AMINRL_AVAIL(L,IEL) = AMAX1 (AMINRL_AVAIL(L,IEL), 0.)
        END DO

        DO IEL = 1, NELEM
!         NB: The original CENTURY model only deals with a 20-cm-thick 
!         soil layer, while DSSAT has many layers of variable thickness. 
!         The parameter RESDAX (a 'fixed' parameter) is in kg[N]/ha
!         and the comparison it is used for depends on the layer
!         thickness. It is therefore recalculated to a 1-cm-thick layer.

!         Calculate the direct absorption of E by the newly added
!         residue. This equals the fraction of E available for
!         absorption (FRDAE) times the amount of mineral E in the soil
!         layer (AMINRL_AVAIL) times a reduction factor that depends on
!         the amount of residue present (RESC/RESDAX).
          DIRABS = FRDAE(IEL) * AMINRL_AVAIL(L,IEL) / DLAYR(L) * 
     &      AMIN1 ((RESC(L) / DLAYR(L)) / (RESDAX / 20.), 1.)

!         Calculate the C/E ratio after the direct absorption.
          RESCE(L,IEL) = RESC(L) / (RESE(L,IEL) + DIRABS)

!         Limit the RESCE after direct absorption so that the C/E ratio
!         of the residue does not become lower than CEDAM. The original
!         residue may have already a C/E ratio < CEDAM (eg. with high-N
!         legume residues). In that case take the original C/E as
!         critical limit.
          RESCE(L,IEL) = AMIN1 (RESC(L) / RESE(L,IEL),
     &      AMAX1 (CEDAM(IEL), RESCE(L,IEL)))

!         Do the direct absorption.
          DIRABS = RESC(L) / RESCE(L,IEL) - RESE(L,IEL)

!         DIRABS should be >= 0 and =< AMINRL_AVAIL.
          DIRABS = AMIN1 (DIRABS, AMINRL_AVAIL(L,IEL))
          DIRABS = AMAX1 (DIRABS, 0.)

!         Add the E from direct absorption to the E pool of the newly
!         added residue
          RESE(L,IEL) = RESE(L,IEL) + DIRABS

!         Reduce the soil mineral E pool by the direct absorption (by
!         setting DLTSNH4 and DLTSNO3). Use the MINERALIZE variable,
!         doing as if what is absorbed were immobilization.
          MINERALIZE = -DIRABS
          CALL NUPDATE_C (
     &      L, SNH4,                                      !Input
     &      DLTSNH4, DLTSNO3, MINERALIZE,                 !Input/Output
     &      TMINERALIZE, TIMMOBILIZE)                     !Output
!Do for P also.
        END DO   !End or IEL loop.
      ENDIF   !End of IF block on L=SRFC

!     ------------------------------------------------------------------
!     Partition the residue into structural and metabolic.
!     ------------------------------------------------------------------
!     Calculate the N fraction of the newly added residue.
      FRNRES = RESE(L,N) / (RESC(L) * 2.5)

!     Calculate the lignin/nitrogen ratio of newly added residue.
      IF (FRNRES .GT. 0.0001) THEN
        LNRES = FRLRES(L) / FRNRES
      ELSE 
        LNRES = 0.0
      ENDIF

!     The fraction of the newly added residue that goes to metabolic
!     depends on the L/N ratio; the rest goes to structural.
      FRMET = FRMETI - FRMETS * LNRES

!     Make sure that the fraction of the newly added residue that goes
!     to structural is not less than its lignin fraction (thus: at
!     least all the lignin has to go to structural; the rest may go to
!     metabolic).
      IF ((1. - FRMET) .LT. FRLRES(L)) FRMET = (1. - FRLRES(L))

!     Error check. Set a flag for printing a warning in NCHECK.
      IF (FRMET .LT. 0) THEN
        FRMETFLAG = .TRUE.
        FRMET = 0.20
      ELSEIF (FRMET .GT. 1.) THEN
        FRMETFLAG = .TRUE.
        FRMET = 1.0 
      ENDIF

!     Make sure at least 20% of the newly added residue goes to
!     metabolic.
      IF (FRMET .LT. 0.20) FRMET = 0.20

!     Calculate the amount of C added to the metabolic pool.
      ADDMETABC = RESC(L) * FRMET
      IF (ADDMETABC .LT. 0.) ADDMETABC = 0.

!     Calculate the amount of C added to the structural pool.
      ADDSTRUCC = RESC(L) - ADDMETABC
      IF (ADDSTRUCC .LT. 0.) ADDSTRUCC = 0.

!     Calculate the amount of lignin C in the newly added residue.
      ADDLIGC = FRLRES(L) * RESC(L)

!     Partition the newly added residue E over the structural and
!     metabolic pools. The C/E ratio of the structural pool is a
!     "fixed" parameter (from the SOMFIX file).
      DO IEL = 1, NELEM
        ADDSTRUCE = ADDSTRUCC / CESTR(IEL)
        ADDMETABE = RESE(L,IEL) - ADDSTRUCE

!       With residues that have a very low E concentration, the
!       structural residue still gets the C/E ratio as set in
!       SOMFIX.SOL. This may result in ADDSTRUCE being greater than
!       RESE, and thus negative ADDMETABE. Set a flag for signaling this
!       and printing a warning in NCHECK.
        IF (ADDMETABE .LT. -0.001) THEN
          ADDMETABEFLAG = .TRUE.
          ADDMETABE = 0.0
          ADDSTRUCE = RESE(L,IEL)
        ENDIF

!       Add the newly added residue pools to the DLT residue pools of
!       earlier residue additions during this time step. Do carbon only 
!       for IEL=N; otherwise it would be added several times if NELEM>1.
        IF (IEL .EQ. N) THEN
          DLTSTRUCC(L) = DLTSTRUCC(L) + ADDSTRUCC
          DLTMETABC(L) = DLTMETABC(L) + ADDMETABC
          DLTLIGC(L) = DLTLIGC(L) + ADDLIGC
        ENDIF

        DLTSTRUCE(L,IEL) = DLTSTRUCE(L,IEL) + ADDSTRUCE
        DLTMETABE(L,IEL) = DLTMETABE(L,IEL) + ADDMETABE
      END DO   !End of IEL loop.

!     Set back to zero. 
      RESC(L)   = 0.
      ADDMETABC = 0.
      ADDSTRUCC = 0.
      DO IEL = 1, NELEM
        RESE(L,IEL) = 0.
        ADDMETABE   = 0.
        ADDSTRUCE   = 0. 
      END DO

!***********************************************************************
!***********************************************************************
!     END
!***********************************************************************

!     ------------------------------------------------------------------
      RETURN
      END   !SUBROUTINE PARTIT_C


