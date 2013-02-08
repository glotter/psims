C=======================================================================
C  LMATCH, Subroutine
C
C  Converts input soil layer data into fixed output soil layers
C  
C-----------------------------------------------------------------------
C  Revision history
C
C  05/07/1991 JWJ Written 
C  05/28/1993 PWW Header revision and minor changes  
C  08/27/2004 CHP A value of -99 for missing data is preserved in all 
C                 layers containing any portion of missing layer data.
C
C-----------------------------------------------------------------------
C  INPUT  : NLAYRI,DI,VI,DLAYR
C
C  LOCAL  : J,K,L,VS,ZIL,ZOL,SUMZ,SUMV,ZT,ZB
C
C  OUTPUT : DS,VS
C-----------------------------------------------------------------------
C  Called : IPSOIL IPSLIN
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      SUBROUTINE LMATCH (NLAYRI,DLAYR,VI,NLAYRO,DS)

      USE ModuleDefs
      IMPLICIT NONE

      INTEGER  J,K,L,NLAYRI,NLAYRO

      REAL     VI(NL),DS(NL),VS(NL),DLAYR(NL)
      REAL     ZIL,ZOL,SUMZ,SUMV,ZT,ZB

      LOGICAL MISSING

      IF (NLAYRI .LT. 1) RETURN

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------
C     This subroutine assumes that DLAYR(L) values are depths to the
C     bottom of layer L
      DO L = 1, NL
        VS(L) = 0.0
      END DO

      K   = 1
      ZIL = 0.0
      ZOL = 0.0

      LOOP1: DO L = 1, NL
        SUMZ = 0.0
        SUMV = 0.0

        MISSING = .FALSE.

        LOOP2: DO WHILE (.TRUE.)
          ZT   = MAX (ZOL,ZIL)
          ZB   = MIN (DS(L),DLAYR(K))
          SUMZ = SUMZ + (ZB - ZT)
          SUMV = SUMV + VI(K)*(ZB - ZT)
          IF (ABS(VI(K) + 99.) < 0.001 .AND. ZB > ZT) THEN
            !missing data if VI=-99
            MISSING = .TRUE.
          ENDIF

          IF (DS(L) .LT. DLAYR(K)) EXIT LOOP2
C
C         Either establish last layer or go to next input layer
C
          IF (K .EQ. NLAYRI) GOTO 20
C
C         Go to next input layer to add characteristics
C
          ZIL = DLAYR(K)
          K   = K + 1
          IF (K .GT. NL) THEN
            WRITE (*,15) K
   15       FORMAT(' More than NL layers in soil profile : ',I3,/,
     &              ' Please fix soil profile.')
            STOP
          ENDIF
        END DO LOOP2

   10   VS(L) = VI(K)
        IF (SUMZ .GT. 0.0) THEN
          VS(L) = SUMV/SUMZ
        ENDIF
        IF (MISSING) THEN
          VS(L) = -99.0
        ENDIF
        ZOL = DS(L)
      END DO LOOP1

      RETURN
C
C     Set last layer characteristics, and depth of profile
C
   20 VS(L) = VI(K)
      IF (ABS(SUMV + 99.0) > 0.001) THEN
        IF (SUMZ .GT. 0.0) THEN
          VS(L) = SUMV/SUMZ
        ENDIF
      ELSE
        VS(L) = -99.0
      ENDIF

      NLAYRO = L
      DO J = 1, NLAYRO
         VI(J) = VS(J)
      END DO

      RETURN
      END SUBROUTINE LMATCH


C=======================================================================
C  LYRSET, Subroutine
C
C  Converts input soil layer data into fixed output soil layers
C  Created by J. W. Jones to create fixed increments for soil
C-----------------------------------------------------------------------
C  Revision history
C
C  05/07/1991 JWJ Written 
C  05/28/1993 PWW Header revisions
C
C-----------------------------------------------------------------------
C  INPUT  : NLAYRI,DI,ZLAYR,DS
C
C  LOCAL  : J,K,L,ZIL,ZOL,DI
C
C  OUTPUT : NLAYRO,DS,VS
C-----------------------------------------------------------------------
C  Called : IPSOIL
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE LYRSET (NLAYRI,ZLAYR,NLAYRO,DS,DLAYR,DEPMAX)

      USE ModuleDefs
      IMPLICIT NONE

      INTEGER  J,K,L,NLAYRI,NLAYRO

      REAL     DI(NL),DS(NL),ZLAYR(NL),DLAYR(NL)
      REAL     ZIL,ZOL,DEPMAX

C*----------------------------------------------------------------------
C     Definition of layers and soil depths moved to IPEXP
C*----------------------------------------------------------------------

C
C     This subroutine assumes that DI(L) values are depths to the
C     bottom of layer L
C
      DO L = 1, NL
         DI(L) = ZLAYR(L)
      END DO

      K = 1

      DO L = 1, NL
         DO WHILE (.TRUE.)
            IF (DS(L) .LT. DI(K)) GOTO 10
C
C           Either establish last layer or go to next input layer
C
            IF (K .EQ. NLAYRI) GOTO 20
C
C           Go to next input layer to add characteristics
C
            ZIL = DI(K)
            K   = K + 1
            IF (K .GT. NL) THEN
                WRITE (*,*) 'K = ',K
                pause
            ENDIF
         END DO
 10      CONTINUE
         ZOL = DS(L)
      END DO
C
C     Set last layer characteristics, and depth of profile
C
 20   CONTINUE
      DS(L)    = DI(K)
      NLAYRO   = L
      DLAYR(1) = DS(1)

      IF (NLAYRO .GT. 1) THEN
         DO J = 2, NLAYRO
            DLAYR(J) = DS(J) - DS(J - 1)
         END DO
         IF (DLAYR (NLAYRO) .LT. DLAYR(NLAYRO - 1) .AND.
     &       DLAYR (NLAYRO) .LT. 15.0) THEN
             DLAYR (NLAYRO)   = (DLAYR(NLAYRO) + DLAYR(NLAYRO-1))/2
             DLAYR (NLAYRO-1) =  DLAYR(NLAYRO)
             DS (NLAYRO-1)    = DS (NLAYRO-2)  + DLAYR(NLAYRO-1)
         ENDIF
      ENDIF

      DEPMAX = DS(NLAYRO)

      RETURN
      END SUBROUTINE LYRSET
C=======================================================================


C=======================================================================
C  LYRSET2, Subroutine
C
C  Converts input soil layer data into fixed output soil layers
C  Created by J. W. Jones to create fixed increments for soil
C  Alternate routine written by CHP
C-----------------------------------------------------------------------
C  Revision history
C
C  10/29/2003 CHP Written based on LYRSET
C=======================================================================

      SUBROUTINE LYRSET2 (NLAYRI, ZLAYR,                  !Input
     &            DS, NLAYRO, DLAYR, DEPMAX)              !Output

      USE ModuleDefs
      IMPLICIT NONE

      INTEGER  L, M, NLAYRI, NLAYRO

      REAL DS(NL), ZLAYR(NL),DLAYR(NL)
      REAL DEPMAX, CUMDEP, THICKNESS

      INTENT(IN)  :: NLAYRI, ZLAYR
      INTENT(OUT) :: DS, NLAYRO, DLAYR, DEPMAX
C----------------------------------------------------------------------
!     Redistribute soil layers.  Keep top three layers at fixed depths.
      DS(1) =  5. 
      DS(2) = 15. 
      DS(3) = 30. 

!     Remaining soil layers set based on thicknesses read from file.
!     Split into 2, 3 or 4 equal layers if thickness > 15 cm.
!     ZLAYR values are depths to bottom of soil layer (cm).
      CUMDEP = 0.0
      M = 4
      DO L = 1, NLAYRI

        CUMDEP = ZLAYR(L)
        THICKNESS = CUMDEP - DS(M-1)

        IF (CUMDEP .LE. DS(3)) THEN
          !Within top 3 fixed depth layers, depths already set.
          CYCLE

        ELSEIF (THICKNESS .LT. 5.0) THEN
          !Add to next layer if thin
          CYCLE

        ELSEIF ((CUMDEP .LT. 90. .AND. THICKNESS .LE. 15.0) .OR.
     &          (CUMDEP .GE. 90. .AND. THICKNESS .LE. 30.0)) THEN
          !Single layer
          DS(M) = CUMDEP
          M = M + 1

        ELSEIF ((CUMDEP .LT. 90. .AND. THICKNESS .LE. 30.0) .OR.
     &          (CUMDEP .GE. 90. .AND. THICKNESS .LE. 60.0)) THEN
          !Split into two equal layers
          DS(M)   = CUMDEP - NINT(THICKNESS * 0.50)
          DS(M+1) = CUMDEP
          M = M + 2

        ELSEIF ((CUMDEP .LT. 90. .AND. THICKNESS .LE. 45.0) .OR.
     &          (CUMDEP .GE. 90. .AND. THICKNESS .LE. 90.0)) THEN
          !Split into three equal layers
          DS(M)   = CUMDEP - NINT(THICKNESS * 0.66667)
          DS(M+1) = CUMDEP - NINT(THICKNESS * 0.33333)
          DS(M+2) = CUMDEP
          M = M + 3

        ELSE
          !Split into four equal layers
          DS(M)   = CUMDEP - NINT(THICKNESS * 0.75)
          DS(M+1) = CUMDEP - NINT(THICKNESS * 0.50)
          DS(M+2) = CUMDEP - NINT(THICKNESS * 0.25)
          DS(M+3) = CUMDEP
          M = M + 4
        ENDIF
      ENDDO

      NLAYRO = M - 1
      DLAYR(1) = DS(1)
      DO L = 2, NLAYRO
        DLAYR(L) = DS(L) - DS(L-1)
      ENDDO

      DEPMAX = DS(NLAYRO)

      RETURN
      END SUBROUTINE LYRSET2
C=======================================================================
