!=======================================================================
!  NCHECK_C, Subroutine for CENTURY-based SOM/residue module of DSSAT.
!  Checks for negative values of soil N and prints report if found.
!-----------------------------------------------------------------------
!  Revision history
!  12/22/1999 CHP Written
!  02/04/2000 AJG Modified for CENTURY-based SOM/residue module.
!  11/14/2003 CHP Removed call to WARNING for Residue N values.  
!
!  Called: CENTURY
!  Calls : NWRITE_C
!-----------------------------------------------------------------------

      SUBROUTINE NCHECK_C (CONTROL, 
     &  ADDMETABEFLAG, FRLSTR,                            !Input
     &  FRMETFLAG, IFERI, IRESI, METABC, METABE, NLAYR,   !Input
     &  PHFLAG, SNH4, SNO3, SOM1C, SOM1E, SOM2C, SOM2E,   !Input
     &  SOM3C, SOM3E, SOMFRACFLAG, STRUCC, STRUCE, UREA)  !Input

!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         !which contain control information, soil
                         !parameters, hourly weather data.
      IMPLICIT NONE
!     ------------------------------------------------------------------
!     NELEM and NL defined in ModuleDefs.for.
      INTEGER DYNAMIC, IEL, L, 
     &   MULTI, N, NLAYR, RUN, SRFC, YRDOY
      PARAMETER (N = 1, SRFC = 0)

      REAL FRLSTR(0:NL), METABC(0:NL), SNH4(NL),
     &  SNO3(NL), SOM1C(0:NL), SOM2C(NL), SOM3C(NL),
     &  STRUCC(0:NL), UREA(NL)
      REAL METABE(0:NL,3), SOM1E(0:NL,3), SOM2E(NL,3),
     &  SOM3E(NL,3), STRUCE(0:NL,3)

      CHARACTER*1 IFERI, IRESI, RNMODE
      CHARACTER*6, PARAMETER :: ERRKEY = "NCHECK"
      CHARACTER*30 FILEIO
      CHARACTER*78 MSG(10)

      LOGICAL ADDMETABEFLAG, FRMETFLAG, PHFLAG
      LOGICAL  SOMFRACFLAG(NL)

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO 
      MULTI   = CONTROL % MULTI
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Rate / initialization section - Warning messages for soil texture,
!     SOM C+N, Residue C+N and C:N ratio.
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!       Check initial values only on the first season of a sequential
!       run (RUN=1) or seasonal run (MULTI=1).
        IF (RUN .EQ. 1 .OR. (MULTI .LE. 1 .AND. 
     &    INDEX('QF',RNMODE) .LE. 0)) THEN
!         Warning that the initial SOM1+SOM2+SOM3 don't sum up to 100%.
          DO L = 1, NLAYR
            IF (SOMFRACFLAG(L)) THEN
              WRITE (MSG(1), 150) L
              WRITE (MSG(2), 151) 
              WRITE (MSG(3), 152) 
              WRITE (MSG(4), 153)
              CALL WARNING (3, ERRKEY, MSG)
            ENDIF   !End of IF block on SOMFRACFLAG.
          END DO

150       FORMAT('The initial fractions of SOM1+SOM2+SOM3 in layer ',
     &      I2,' do not sum up to 1.0.')
151       FORMAT('The SOM output in the files SOMLIT.OUT and ',
     &     'SOMLIT1.OUT may be wrong.')
152       FORMAT('Please check your SOMFRACTIONS_C.SOL file.')
153       FORMAT('Values will be adjusted to sum to 1.0.')

!         Warning that the pH was set to a different value.
          IF (PHFLAG) THEN
            WRITE (MSG(1), 200)
            WRITE (MSG(2), 201)
            WRITE (MSG(3), 202)
            CALL WARNING (3, ERRKEY, MSG)
          ENDIF   !End of IF block on PHFLAG.

200       FORMAT('The pH of a certain soil layer at initialization')
201       FORMAT('was outside the range pH=1-10, which is unlikely.')
202       FORMAT('It was set by DSSAT to pH=7.') 
    
!!         Check for unlikely C:N ratio's in th initial SOM. The CNRAT
!!         variable is based on SOM2 (see SOMINIT).
!          DO L = 1, NLAYR
!            IF (CNRAT(L) .LT. 8. .OR. CNRAT(L) .GT. 25.) THEN
!              WRITE (MSG(1), 300) 
!              WRITE (MSG(2), 301) L
!              WRITE (MSG(3), 302) CNRAT(L)
!              WRITE (MSG(4), 303)
!              WRITE (MSG(5), 304)
!              CALL WARNING (5, ERRKEY, MSG)
!            ENDIF   !End of IF block on CNRAT.
!          END DO   !End of soil layer loop.

300       FORMAT ('The C/N ratio of the soil organic matter',
     &          ' pool of intermediate age (SOM2)')
301       FORMAT ('in layer ', I2, ', as calculated during the',
     &            ' initialization from the SLNI value and')
302       FORMAT ('the fraction of SOM2, is unlikely: ', F8.1)
303       FORMAT ('Please check your SOIL.SOL file for the ratio of',
     &            ' SLOC and SLNI -- it should')
304       FORMAT ('be between ca. 5 and 15 -- and check your',
     &            ' SOMFRACTIONS_C.SOL file')
305       FORMAT ('(unlikely SOM pool distribution?).')



!     CHP 11/13/2003 Put this in RPLACE_C
!     RESECONC stays the same until next residue application and so
!       message is printed every day.
!!         Initial residues with an unlikely high N concentration.
!          IF (RESECONC(N) .GT. 0.04) THEN
!            WRITE (MSG(1), 400) YRDOY, RESECONC(N)*100.
!            CALL WARNING (1, ERRKEY, MSG)
!!         Or too low a N concentration.
!          ELSEIF (RESECONC(N) .LT. 0.00001) THEN
!            WRITE (MSG(1), 401) YRDOY, RESECONC(N)*100.
!            CALL WARNING (1, ERRKEY, MSG)
!          ENDIF   !End of IF block on RESECONC.
!
!400       FORMAT(' The residue N concentration on day ', I7,
!     &           ' is quite high: ', F8.2, '%.') 
!401       FORMAT(' The residue N concentration on day ', I7,
!     &           ' is improbably low: ', F8.2, '%.') 

!         Check for invalid fertilizer option.
          IF (INDEX ('ARDN', IFERI) .EQ. 0) THEN
            WRITE (MSG(1), 500) IFERI
            WRITE (MSG(2), 501) 
            CALL WARNING (2, ERRKEY, MSG)
          ENDIF   !End of IF block on IFERI.

500       FORMAT('The fertilizer option, "', A1, '" is not',
     &            ' currently supported.')
501       FORMAT('No fertilizer applications were done.')

!         Check for invalid residue option.
          IF (INDEX ('ARDN', IRESI) .EQ. 0) THEN
            WRITE (MSG(1), 600) IRESI
            WRITE (MSG(2), 601) 
            CALL WARNING (2, ERRKEY, MSG)
          ENDIF   !End of IF block on IRESI.

600       FORMAT('The residue option, "', A1, '" is not',
     &            ' currently supported.')
601       FORMAT('No residue applications were done.')
        ENDIF   !End of IF block on RUN, RNMODE, MULTI.

!***********************************************************************
!***********************************************************************
!     Rate section - Warning messages for Residue N 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
!     This is not needed anymore.
!!       Check for a fertilizer option that should not have N.
!        IF (.NOT. CONTAINSN) THEN
!          WRITE (MSG(1), 700) FERTYPE, YRDOY
!          WRITE (MSG(2), 701) 
!          CALL WARNING (2, ERRKEY, MSG)
!!         Set back to true.
!          CONTAINSN = .TRUE.
!        ENDIF   !End of IF block on CONTAINSN.
!
!700   FORMAT('The fertilizer type, "FE0', I2, '" on day ', I7,
!     &      ' should not contain N,')
!701   FORMAT('though you indicated that it does.',
!     &       ' This may affect the N balance.')

!     CHP 11/13/2003 Put this in RPLACE_C
!     RESECONC stays the same until next residue application and so
!       message is printed every day.
!!       Residues with an unlikely high N concentration.
!        IF (RESECONC(N) .GT. 0.04) THEN
!          WRITE (MSG(1), 400) YRDOY, RESECONC(N)*100.
!          CALL WARNING (1, ERRKEY, MSG)
!!       Or too low an N concentration.
!        ELSEIF (RESECONC(N) .LT. 0.00001) THEN
!          WRITE (MSG(1), 401) YRDOY, RESECONC(N)*100.
!          CALL WARNING (1, ERRKEY, MSG)
!        ENDIF

!       Warning that C:E ratio in subroutine PARTIT is unlikely wide.
        IF (FRMETFLAG) THEN
          WRITE (MSG(1), 800) 
          WRITE (MSG(2), 801) YRDOY
          WRITE (MSG(3), 802) 
          WRITE (MSG(4), 803) 
          CALL WARNING (4, ERRKEY, MSG)
!         Set back to FALSE; otherwise it prints the warning every day.
          FRMETFLAG = .FALSE.
        ENDIF   !End of IF block on FRMETFLAG

800   FORMAT('The fraction of metabolic material in newly applied',
     &    ' residue')
801   FORMAT('or senesced biomass that was added to the soil,',
     &    ' on day ', I7)
802   FORMAT('is <0 or >1. This may give a negative structural',
     &    ' fraction.') 
803   FORMAT('This is due to an incorrect lignin or N concentration.')

!       Warning that C:E ratio in subroutine PARTIT is unlikely wide.
        IF (ADDMETABEFLAG) THEN
          WRITE (MSG(1), 900) 
          WRITE (MSG(2), 901) 
          WRITE (MSG(3), 902) YRDOY
          WRITE (MSG(4), 903) 
          WRITE (MSG(5), 904) 
          WRITE (MSG(6), 905) 
          CALL WARNING (6, ERRKEY, MSG)
!         Set back to FALSE; otherwise it prints the warning every day.
          ADDMETABEFLAG = .FALSE.
        ENDIF   !End of IF block on ADDMETABEFLAG

900   FORMAT('The carbon/nutrient ratio (C/N or C/P) of the residue') 
901   FORMAT('or senesced biomass that was added to the soil, on day')
902   FORMAT(I7,' is wider than the common range to which the CESTR')
903   FORMAT('parameter in the file SOMFIX.SOL applies. You have a') 
904   FORMAT('residue with an extremely low nutrient concentration.')  
905   FORMAT('Please check your data and/or adapt the CESTR value.')  

***********************************************************************
!***********************************************************************
!     INTEGRATE
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!     ------------------------------------------------------------------
        DO L = 0, NLAYR   !Including SRFC layer.
!         Check for negative SOM/residue and soil-N values.
          IF (L .GT. SRFC) THEN
            IF (SNO3(L) .LT. -0.001) CALL NWRITE_C(YRDOY, L, SNO3(L), 1)
            IF (SNH4(L) .LT. -0.001) CALL NWRITE_C(YRDOY, L, SNH4(L), 2)
            IF (UREA(L) .LT. -0.001) CALL NWRITE_C(YRDOY, L, UREA(L), 3)
          ENDIF

          IF (METABC(L) .LT. -0.001) CALL NWRITE_C(YRDOY,L, METABC(L),4)
          IF (STRUCC(L) .LT. -0.001) CALL NWRITE_C(YRDOY,L, STRUCC(L),5)
          IF (FRLSTR(L) .LT. -0.001 .OR. FRLSTR(L) .GT. 1.) 
     &                            CALL NWRITE_C (YRDOY, L, FRLSTR(L), 6)

          IF (SOM1C(L) .LT. -0.001) CALL NWRITE_C(YRDOY, L, SOM1C(L), 7)
          IF (L .GT. SRFC) THEN
            IF (SOM2C(L) .LT. -0.001) CALL NWRITE_C(YRDOY,L, SOM2C(L),8)
            IF (SOM3C(L) .LT. -0.001) CALL NWRITE_C(YRDOY,L, SOM3C(L),9)
          ENDIF

          DO IEL = 1, NELEM
            IF (METABE(L,IEL) .LT. -0.001) 
     &        CALL NWRITE_C (YRDOY, L, METABE(L,IEL), 10)
            IF (STRUCE(L,IEL) .LT. -0.001) 
     &        CALL NWRITE_C (YRDOY, L, STRUCE(L,IEL), 11)

            IF (SOM1E(L,IEL)   .LT. -0.001) 
     &        CALL NWRITE_C (YRDOY, L, SOM3E(L,IEL), 12)
            IF (L .GT. SRFC) THEN
              IF (SOM2E(L,IEL) .LT. -0.001) 
     &          CALL NWRITE_C (YRDOY, L, SOM2E(L,IEL), 13)
              IF (SOM3E(L,IEL) .LT. -0.001) 
     &          CALL NWRITE_C (YRDOY, L, SOM3E(L,IEL), 14)
            ENDIF
          ENDDO   !End of IEL loop.
        ENDDO   !End of soil layer loop.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

!-----------------------------------------------------------------------
      RETURN
      END   !SUBROUTINE NCHECK_C


!=======================================================================
!  NWRITE_C, Subroutine for CENTURY-based SOM/residue module.
!
!  Writes negative values of soil N to Warning.OUT file
!-----------------------------------------------------------------------
!  Revision history
!  03/16/00 CHP written.
!  03/28/00 AJG Modified for CENTURY-based SOM/residue module.
!-----------------------------------------------------------------------
      SUBROUTINE NWRITE_C (YRDOY, L, VALUE, CODE)

!     ------------------------------------------------------------------
      IMPLICIT NONE 

      INTEGER CODE, L, YRDOY
      REAL VALUE
      CHARACTER*78 MSG(10)
      LOGICAL SEND

!     ------------------------------------------------------------------
      SEND = .FALSE.
      SELECT CASE (CODE)
        CASE (1)
          WRITE (MSG(1), 10) YRDOY, L
          WRITE (MSG(2), 100) VALUE
10          FORMAT('Negative soil-N value on day ', I7, ' in layer ',
     &        I3)
100         FORMAT('Nitrate (SNO3) = ', F10.3, ' kg[N]/ha')
            SEND = .TRUE.

        CASE (2)
          WRITE (MSG(1), 10) YRDOY, L
          WRITE (MSG(2), 120) VALUE
120       FORMAT('Ammonium (SNH4) = ', F10.3, ' kg[N]/ha')
          SEND = .TRUE.

        CASE (3)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 135) VALUE
130       FORMAT(
     &      'Negative SOM/residue value on day ', I7, ' in layer ',
     &      I3)
135       FORMAT ('Urea = ',F10.3,' kg[N]/ha')
          SEND = .TRUE.

        CASE (4)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 140) VALUE
140       FORMAT ('Metabolic carbon (METABC) = ', F10.3, ' kg[C]/ha')
          SEND = .TRUE.
        
        CASE (5)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 150) VALUE
150       FORMAT('Structural carbon (STRUCC) = ', F10.3, ' kg[C]/ha')
          SEND = .TRUE.

        CASE (6)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 160) VALUE
160       FORMAT ('Structural lignin (FRLSTR) = ', F10.3, '%')
          SEND = .TRUE.

        CASE (7)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 170) VALUE
170       FORMAT ('SOM1 carbon (SOM1C) = ', F10.3, ' kg[C]/ha')
          SEND = .TRUE.

        CASE (8)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 180) VALUE
180       FORMAT ('SOM2 carbon (SOM2C) = ', F10.3, 'kg[C]/ha')
          SEND = .TRUE.

        CASE (9)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 190) VALUE
190       FORMAT ('SOM3 carbon (SOM3C) = ', F10.3, 'kg[C]/ha')
          SEND = .TRUE.

        CASE (10)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 200) VALUE
200       FORMAT ('Metabolic Nitrogen (METABE(N)) = ', F10.3,
     &      'kg[N]/ha')
          SEND = .TRUE.

        CASE (11)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 210) VALUE
210       FORMAT('Structural Nitrogen (STRUCE(N)) = ', F10.3,
     &      'kg[N]/ha')
          SEND = .TRUE.

        CASE (12)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 220) VALUE
220       FORMAT('SOM1 Nitrogen (SOM1E(N)) = ', F10.3, 'kg[N]/ha')
          SEND = .TRUE.

        CASE (13)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 230) VALUE
230       FORMAT('SOM2 Nitrogen (SOM2E(N)) = ', F10.3, 'kg[N]/ha')
          SEND = .TRUE.

        CASE (14)
          WRITE (MSG(1), 130) YRDOY, L
          WRITE (MSG(2), 240) VALUE
240       FORMAT('SOM3 Nitrogen (SOM3E(N)) = ', F10.3, 'kg[N]/ha')
        SEND = .TRUE.

      END SELECT

      IF (SEND) CALL WARNING (2, "NCHECK", MSG)

      RETURN
      END   !SUBROUTINE NWRITE_C


