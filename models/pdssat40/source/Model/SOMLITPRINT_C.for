!***********************************************************************
!  SOMLITPRINT_C, for CENTURY-based SOM/residue module of DSSAT.
!
!  Purpose: Prints daily output of SOM and litter variables in a
!           layer-by-layer structure (easy to read, but hard to import
!           in a spreadsheet; for that, see OPSOMLIT).
!
!  Revision history:
!  06/09/1999 AJG Written
!  01/01/2000 AJG Integrated the CENTURY-based and CERES-based SOM
!                  modules with CHP's modular structure.
!  11/05/2002 AJG Adapted for output from seqwuential runs.
!
!  Called: CENTURY
!  Calls: --
!***********************************************************************
      SUBROUTINE SOMLITPRINT_C (CONTROL,
     &  DLAYR, LITC, LITE, METABC, METABE, NLAYR,         !Input
     &  SOM1C, SOM1E, SOM2C, SOM2E, SOM3C, SOM3E,         !Input
     &  SSOMC, SSOME, STRUCC, STRUCE, TLITC, TLITE,       !Input
     &  TMETABC, TMETABE, TSOM1C, TSOM1E, TSOM2C,         !Input
     &  TSOM2E, TSOM3C, TSOM3E, TSOMC, TSOME, TSTRUCC,    !Input
     &  TSTRUCE)                                          !Input


!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT  NONE
      SAVE
!     ------------------------------------------------------------------
!     The variable FIRSTTIME controls that it only prints the SOM 
!     initialization with a call from SOILNI_C on the first day and not 
!     again with a call from CENTURY, which does the litter initialization.
      LOGICAL FIRSTTIME

      CHARACTER*1 RNMODE
      CHARACTER*11 ERRKEY
      PARAMETER (ERRKEY = 'SOMLITPRINT')

      INTEGER DYNAMIC, ERRNUM, L, LUN, N, NLAYR, RUN, SRFC, YRDOY
      PARAMETER (N = 1)
      PARAMETER (SRFC = 0)

      REAL TLITC, TMETABC, TSOM1C, TSOM2C, TSOM3C, TSOMC, TSTRUCC

      REAL TLITE(3), TMETABE(3), TSOM1E(3), TSOM2E(3), TSOM3E(3),
     &  TSOME(3), TSTRUCE(3)

      REAL DLAYR(NL), LITC(0:NL), METABC(0:NL), SOM1C(0:NL), SOM2C(NL),
     &  SOM3C(NL), SSOMC(0:NL), STRUCC(0:NL)

      REAL LITE(0:NL,3), METABE(0:NL,3), SOM1E(0:NL,3), SOM2E(NL,3),
     &  SOM3E(NL,3), SSOME(0:NL,3), STRUCE(0:NL,3)

      DATA FIRSTTIME /.TRUE./

      LOGICAL FEXIST

!     Constructed variables are defined in ModuleDefs.
      TYPE (ControlType) CONTROL

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!       Open output file.
        CALL GETLUN('SLDET', LUN)
        INQUIRE (FILE = 'SOMLIT1.OUT', EXIST = FEXIST)
        IF (FEXIST) THEN
          OPEN (UNIT = LUN, FILE = 'SOMLIT1.OUT', STATUS = 'OLD',
     &      IOSTAT = ERRNUM, POSITION = 'APPEND')
        ELSE
          OPEN (UNIT = LUN, FILE = 'SOMLIT1.OUT', STATUS = 'NEW',
     &      IOSTAT = ERRNUM)
        ENDIF

!       If the file can't be found, call an error.
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY, ERRNUM, 'SOMLITPRINT', 0)

!       ----------------------------------------------------------------
!       SOM INITIALIZATION OUTPUT
!       ----------------------------------------------------------------
        IF (RUN .EQ. 1 .OR. INDEX ('QF',RNMODE) .LE. 0) THEN

!         Write output on SOM initialization.
          IF (FIRSTTIME) THEN
            WRITE (LUN,'(101(''*''))')
            WRITE (LUN,'(A)') 'SOM initialization.'
            WRITE (LUN,'(101(''*''))')

            DO L = 0, NLAYR
              IF (L .EQ. SRFC) THEN
!               Write headers of the various pools.
                WRITE (LUN,100) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &            'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &            'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &            'LITE(N)', 'STRUCE(N)', 'METABE(N)'
100             FORMAT (A2, 4X, A5, 2X, 14(A9, 2X))

                WRITE (LUN,200) L, SOM1C(SRFC), LITC(SRFC), 
     &            STRUCC(SRFC), METABC(SRFC), SOM1E(SRFC,N),
     &            LITE(SRFC,N), STRUCE(SRFC,N), METABE(SRFC,N)
200             FORMAT (I2, 20X, F11.2, 22X, 3F11.2, 11X, F11.2, 22X,
     &            3F11.2)

              ELSE
!               Write SOM and litter pool sizes (litter is still zero).
                WRITE (LUN,300) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &            SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &            SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &            LITE(L,N), STRUCE(L,N), METABE(L,N)
300             FORMAT (I2, F9.0, 14(F11.2))

                IF (L .EQ. NLAYR) WRITE (LUN,400) TSOMC, TSOM1C, TSOM2C,
     &            TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &            TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N),TMETABE(N)
400             FORMAT ('Soil', 7X, 14(F11.2), /)
              ENDIF
            END DO

            FIRSTTIME = .FALSE.

!         --------------------------------------------------------------
!         LITTER INITIALIZATION OUTPUT
!         --------------------------------------------------------------
          ELSE   !IF .NOT. FIRSTTIME
            WRITE (LUN,'(101(''*''))')
!           Write header of output on litter initialization.
            WRITE (LUN,500)
500         FORMAT ('Litter initialization ',
     &       '(for a sequential run, harvest residues of the previous ',
     &       'season are added here).')

            WRITE (LUN,600)
600         FORMAT ('If the litter has been incorporated, then SOM1 of',
     &        ' layer 0 will also be incorporated. *')
            WRITE (LUN,'(101(''*''))')

!           Set FIRSTTIME back to true, so that with a seasonal run the
!           output will start from the beginning.
!           FIRSTTIME = .TRUE.

            DO L = 0, NLAYR
              IF (L .EQ. 0) THEN
!               Write headers of the various pools.
                WRITE (LUN,100) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &            'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &            'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &            'LITE(N)', 'STRUCE(N)', 'METABE(N)'
                WRITE (LUN,200) L, SOM1C(SRFC), LITC(SRFC),
     &            STRUCC(SRFC),  METABC(SRFC), SOM1E(SRFC,N),
     &            LITE(SRFC,N), STRUCE(SRFC,N), METABE(SRFC,N)
              ELSE
!               Write SOM and litter pool sizes.
                WRITE (LUN,300) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &            SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &            SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &            LITE(L,N), STRUCE(L,N), METABE(L,N)

                IF (L .EQ. NLAYR) WRITE (LUN,400) TSOMC, TSOM1C, TSOM2C,
     &            TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &            TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N),TMETABE(N)
              ENDIF
            END DO
          ENDIF
        ENDIF

!***********************************************************************
!     OUTPUT (Daily, not just initialization!)
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
!     ------------------------------------------------------------------
!       Write header on daily output.
        WRITE (LUN,'(A7, I7)') 'YRDOY =', YRDOY

        DO L = 0, NLAYR
          IF (L .EQ. 0) THEN
!           Write headers of the various pools.
            WRITE (LUN,100) 'L', 'DLAYR', 'SSOMC', 'SOM1C',
     &        'SOM2C', 'SOM3C', 'LITC', 'STRUCC', 'METABC',
     &        'SSOME(N)', 'SOM1E(N)', 'SOM2E(N)', 'SOM3E(N)',
     &        'LITE(N)', 'STRUCE(N)', 'METABE(N)'
            WRITE (LUN,200) L, SOM1C(SRFC), LITC(SRFC), STRUCC(SRFC), 
     &        METABC(SRFC), SOM1E(SRFC,N), LITE(SRFC,N),
     &        STRUCE(SRFC,N), METABE(SRFC,N)
          ELSE
!           Write SOM and litter pool sizes.
            WRITE (LUN,300) L, DLAYR(L), SSOMC(L), SOM1C(L),
     &        SOM2C(L), SOM3C(L), LITC(L), STRUCC(L), METABC(L),
     &        SSOME(L,N), SOM1E(L,N), SOM2E(L,N), SOM3E(L,N),
     &        LITE(L,N), STRUCE(L,N), METABE(L,N)
            IF (L .EQ. NLAYR) WRITE (LUN,400) TSOMC, TSOM1C, TSOM2C,
     &        TSOM3C, TLITC, TSTRUCC, TMETABC, TSOME(N), TSOM1E(N),
     &        TSOM2E(N), TSOM3E(N), TLITE(N), TSTRUCE(N),TMETABE(N)
          ENDIF
        END DO 

!***********************************************************************
!***********************************************************************
!     FINAL
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. FINAL) THEN
!     ------------------------------------------------------------------
        CLOSE (LUN)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!     ------------------------------------------------------------------
      RETURN
      END   !SUBROUTINE SOMLITPRINT_C

