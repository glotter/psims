C=======================================================================
C  IPSOIL, Subroutine
C
C  Soil selection
C-----------------------------------------------------------------------
C  Revision history
C
C  06/21/1991 JWJ 
C  05/28/1993 PWW Header revision and minor changes
C  12/01/1993 WTB Modified for soil P
C  12/12/2000 GH  Modified format
C  07/01/2003 GH  Add error checking codes for surface values
!  08/23/2005 CHP Fixed some error checking for soil water parameters.
C-----------------------------------------------------------------------
C  INPUT  : RNMODE,FILES,PATHSL,ISWWAT
C
C  LOCAL  :
C
C  OUTPUT : NSENS
C-----------------------------------------------------------------------
C  Called : SESOIL INPUT
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSOIL (RNMODE,FILES,PATHSL,NSENS,ISWWAT)

      USE ModuleDefs
      IMPLICIT NONE

      INCLUDE 'COMSOI.BLK'

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS,ISWWAT,UPCASE
      CHARACTER*5   MH(NL)
      CHARACTER*6   ERRKEY
      CHARACTER*12  FILES
      CHARACTER*80  PATHSL
      CHARACTER*92  FILESS
      CHARACTER*255 C255

      INTEGER I,J,JJ,P,NLAYRI,NLAYRO,LINSOL,ISECT
      INTEGER NSENS,NLSOIL,NLOOP,ERRNUM,LUNSL,PATHL

!     05/27/2004 CHP Added these variables to COMSOI.BLK
!      REAL    PTERMA(NL),PTERMB(NL),EXK(NL),EXMG(NL),EXNA(NL),EXTS(NL)
!      REAL    SLEC(NL),ZLYR(NL),ZZLYR(NL)
      REAL    ZLYR(NL),ZZLYR(NL)
      REAL    FLAG,SL,SLDP

      PARAMETER (ERRKEY = 'IPSOIL')
      PARAMETER (LUNSL  = 12)
      PARAMETER (BLANK = ' ')

!-----------------------------------------------------------------------
!     No soil file read - default conditions
      IF (SLNO(1:3) .EQ. '   ' .OR. SLNO(1:3) .EQ. '-99' .OR.
     &    SLNO(8:10) .EQ. '-99' .OR. SLNO(8:10) .EQ. '   ') THEN
         SLPF  = 1.0
         NLAYR = 1

!     Read soil file
       ELSE
         LINSOL = 0
         PATHL  = INDEX(PATHSL,BLANK)
         IF (PATHL .LE. 1) THEN
            FILESS = FILES
          ELSE
            FILESS = PATHSL(1:(PATHL-1)) // FILES
         ENDIF
         OPEN (LUNSL, FILE = FILESS,STATUS = 'OLD',IOSTAT=ERRNUM)
         IF (ERRNUM .NE. 0) THEN
            CALL ERROR(ERRKEY,ERRNUM,FILES,0)
         END IF

!-----------------------------------------------------------------------
!     Sensitivity Analysis - soil selection
         IF (NSENS .EQ. 1 ) THEN
            NLOOP  = 0
            NLSOIL = 0
            DO I = 1, 80
               LINE(I) = ' '
            END DO
            I  = 1
            IF (INDEX('IE',RNMODE) .GT. 0) THEN
               CALL CLEAR
               WRITE (*, 5130)
            ENDIF
 10         CONTINUE
            CALL IGNORE(LUNSL, LINSOL, ISECT, C255)
            IF ( ISECT .EQ. 0 ) GO TO 111
            IF (C255(1:1) .NE. '*') GO TO 10
            IF (C255(2:5) .EQ. 'SOIL') GO TO 10

            READ (C255,5030,IOSTAT=ERRNUM) PEDON,SLSOUR,SLTXS,
     &             SLDP,SLDESC
            IF (ERRNUM .NE. 0) THEN
               CALL ERROR (ERRKEY,ERRNUM,FILES,LINSOL)
            ENDIF
            DO P = 1, 10
             PEDON(P:P) = UPCASE(PEDON(P:P))
             SLNO(P:P)  = UPCASE(SLNO(P:P))
            END DO
            IF (INDEX('IE',RNMODE) .GT. 0) WRITE(*, 5140) I,SLDESC,PEDON
            IF (PEDON  .EQ. SLNO) NLSOIL = I
C
C           Write out pause statement every 15 lines
C
            IF ((MOD(I,15) .EQ. 0).AND.(INDEX('IE',RNMODE) .GT. 0)) THEN
               WRITE (*,5000)
               READ  (5,'(A1)') ANS
            END IF
            I  = I + 1

            GOTO 10
 111        CONTINUE

            NLOOP = NLOOP + 1
            IF (NLOOP .GT. 25) THEN
               CALL ERROR (ERRKEY,1,FILES,LINSOL)
            ENDIF
            LINE(1) = ' '

            IF ((INDEX('IE',RNMODE) .GT. 0) .AND. NLSOIL .EQ. 0) THEN
               WRITE (*,950) SLNO
            ENDIF
            IF (INDEX('IE',RNMODE) .GT. 0) WRITE(*, 5160) NLSOIL
            READ (5,'(80A1)') LINE
            CALL VERIFY (LINE,SL,FLAG)

            IF (SL .LE. 0) THEN
               SL  = NLSOIL
             ELSEIF ((FLAG .GT. 0) .OR. (SL .GT. (I-1))) THEN
               WRITE (*,5101) (I-1)
               GO TO 111
             ELSEIF (SL .NE. NINT(SL)) THEN
               WRITE (*,5102)
               GO TO 111
             ELSEIF (SL .GT. 0.) THEN
               NLSOIL = NINT(SL)
             ELSE
               CALL ERROR(ERRKEY,3,FILES,LINSOL)
            ENDIF

            REWIND (LUNSL)
         ENDIF     !End of sensitivity selection

!-----------------------------------------------------------------------
!     Find correct soil within soil file
         I = 0
 5024    CONTINUE
         I = I + 1
 5025    CONTINUE
         CALL IGNORE (LUNSL,LINSOL,ISECT,C255)
         IF ( ISECT .EQ. 0 ) THEN     !end of file
            CLOSE(LUNSL)
            IF (FILES(1:4) .EQ. 'SOIL') THEN
               FILES(1:8) = SLNO(1:2)//'.SOL  '
               PATHL  = INDEX(PATHSL,BLANK)
               IF (PATHL .LE. 1) THEN
                 FILESS = FILES
               ELSE
                 FILESS = PATHSL(1:(PATHL-1)) // FILES
               ENDIF
               OPEN (LUNSL, FILE = FILESS,STATUS = 'OLD',IOSTAT=ERRNUM)
               IF (ERRNUM .NE. 0) THEN
                  CALL ERROR(ERRKEY,ERRNUM,FILES,0)
               ENDIF
               GO TO 5025
            ENDIF
            IF (ISWWAT .EQ. 'N') THEN
              SLPF  = 1.0
              NLAYR = 1
              RETURN
            ELSE
              CALL ERROR (ERRKEY,4,FILES,LINSOL)
            ENDIF
         ENDIF
         IF (C255(1:1) .NE. '*') GO TO 5025
         IF (C255(2:5) .EQ. 'SOIL') GO TO 5025
         READ (C255,5030,IOSTAT=ERRNUM) PEDON,SLSOUR,SLTXS,SLDP,SLDESC
         IF (ERRNUM .NE. 0) THEN
            CALL ERROR (ERRKEY,ERRNUM,FILES,LINSOL)
         ENDIF
         DO P = 1, 10
           PEDON(P:P) = UPCASE(PEDON(P:P))
           SLNO(P:P)  = UPCASE(SLNO(P:P))
         END DO

         IF (((PEDON .NE. SLNO) .AND. (NSENS .EQ. 0))  .OR.
     &         ((I .LT. NLSOIL) .AND. (NSENS .EQ. 1))) GO TO 5024

!-----------------------------------------------------------------------
!        Found correct soil
!-----------------------------------------------------------------------
         CALL IGNORE (LUNSL, LINSOL, ISECT, C255)
         IF (ISECT .NE. 1) CALL ERROR (ERRKEY,4,FILES,LINSOL)
         READ (C255,5035,IOSTAT=ERRNUM) SSITE,SCOUNT,SLAT,SLONG,TAXON
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILES,LINSOL)
         CALL IGNORE (LUNSL,LINSOL,ISECT,C255)
         IF (ISECT .NE. 1) CALL ERROR (ERRKEY,4,FILES,LINSOL)
         READ (C255,5040,IOSTAT=ERRNUM) SCOM,SALB,U,SWCON,CN2,SLNF,SLPF,
     &        SMHB,SMPX,SMKE,SGRP
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILES,LINSOL)
         !IF (U .LE. 0.0) THEN
         !   CALL ERROR (ERRKEY,9,FILES,LINSOL)
         !ENDIF
         IF (SWCON .LT. 0.0) THEN
            CALL ERROR (ERRKEY,10,FILES,LINSOL)
         ENDIF
         IF (CN2 .LE. 0.0) THEN
            CALL ERROR (ERRKEY,11,FILES,LINSOL)
         ENDIF
         !IF (SALB .LE. 0.0) THEN
         !   CALL ERROR (ERRKEY,12,FILES,LINSOL)
         !ENDIF

C
C        Now read layer information
C
         DO J = 1, 1000
            IF (J .GT. NL) CALL ERROR (ERRKEY,2,FILES,LINSOL)
            CALL IGNORE (LUNSL,LINSOL,ISECT,C255)
C
C           Check for last layer (Character in 1ST Column will be the next
C           soil
C
            IF ((C255(1:1) .EQ. '*').OR.(ISECT .NE. 1)) GOTO 55
C
C           Check for second tier, the depth to the layer will be less     
C           than the depth to the previous layer                        
                                                                        
            READ(C255,5080,IOSTAT=ERRNUM) ZLYR(J)                       
            IF ((J .GT. 1) .AND. (ZLYR(J) .LE. ZLYR(J-1))) GOTO 75      
                                                                        
            READ (C255,5080,IOSTAT=ERRNUM) ZLYR(J),MH(J),
     &           LL(J),DUL(J),SAT(J),SHF(J),SWCN(J),BD(J),
     &           OC(J),CLAY(J),SILT(J),STONES(J),TOTN(J),
     &           PH(J),PHKCL(J),CEC(J),ADCOEF(J)
            IF (ERRNUM .NE. 0) THEN
               CALL ERROR (ERRKEY,ERRNUM,FILES,LINSOL)
            ENDIF
            IF (ISWWAT .NE. 'N') THEN
!CHP 5/26/04  IF (SAT(J) .LT. DUL(J)) THEN
              IF (DUL(J) - SAT(J) .GT. 1.E-4) THEN
                CALL ERROR (ERRKEY,7,FILES,LINSOL)
              ENDIF
!CHP 5/26/04  IF (DUL(J) .LT. LL(J)) THEN
              IF ((LL(J) - DUL(J)) .GT. 1.E-4) THEN
                 CALL ERROR (ERRKEY,8,FILES,LINSOL)
              ENDIF
              IF (DUL(J) .LT. 1.E-3) THEN
                 CALL ERROR (ERRKEY,13,FILES,LINSOL)
              ENDIF
!CHP 5/26/04  IF (SAT(J) .EQ. DUL(J)) THEN
              IF (ABS(SAT(J) - DUL(J)) .LT. 1.E-2) THEN
                 SAT(J) = DUL(J) + 0.01
              ENDIF
!CHP 5/26/04  IF (DUL(J) .EQ.  LL(J)) THEN
              IF (ABS(DUL(J) -  LL(J)) .LT. 1.E-2) THEN
                 LL(J) = DUL(J) - 0.01
              ENDIF   
            ENDIF
         END DO

 75      CONTINUE

         JJ = 1
         DO JJ = 1, 200
            IF (JJ .GT. NL) THEN
               CALL ERROR (ERRKEY,2,FILES,LINSOL)
            ENDIF
C
C           Check for last layer (Character in 1ST Column will be the next
C           soil
C
            IF ((C255(1:1) .EQ. '*').OR.(ISECT .NE. 1)) GOTO 85

            READ (C255,5090,IOSTAT=ERRNUM) ZZLYR(JJ),
     &          EXTP(JJ),TOTP(JJ),ORGP(JJ),CACO(JJ),EXTAL(JJ),
     &          EXTFE(JJ),EXTMN(JJ),TOTBAS(JJ),PTERMA(JJ),
     &          PTERMB(JJ),EXK(JJ),EXMG(JJ),EXNA(JJ),EXTS(JJ),SLEC(JJ)
            IF (ERRNUM .NE. 0) THEN
               CALL ERROR(ERRKEY,ERRNUM,FILES,LINSOL)
            ENDIF
            IF (ZLYR(JJ) .NE. ZZLYR(JJ)) THEN
               CALL ERROR (ERRKEY,5,FILES,LINSOL)
            ENDIF
            CALL IGNORE (LUNSL, LINSOL, ISECT, C255)                    
         END DO

 85      CONTINUE
         IF (JJ .NE. J) THEN
            CALL ERROR (ERRKEY,6,FILES,LINSOL)
         ENDIF
C
C        Found last layer for this soil
C
 55      CONTINUE
         NLAYRI = J - 1
         
!     CHP 05/21/2004
!     Fix for deep layer soils -- set bottom of 20th layer to depth of profile
         DS(NL) = MAX(DS(NL), ZLYR(NLAYRI))

         SLNO = PEDON
         CLOSE (LUNSL)

         CALL LYRSET (NLAYRI, ZLYR, NLAYR, DS, DLAYR, DEPMAX)

!        First tier soils data
         CALL LMATCH (NLAYRI, ZLYR, LL,    NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, DUL,   NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, SAT,   NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, SHF,   NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, SWCN,  NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, BD,    NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, OC,    NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, CLAY,  NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, SILT,  NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, STONES,NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, TOTN,  NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, PH,    NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, PHKCL, NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, CEC,   NLAYRO, DS)
         CALL LMATCH (NLAYRI, ZLYR, ADCOEF,NLAYRO, DS)

!        Second tier soils data
         CALL LMATCH (NLAYRI,ZZLYR, EXTP,  NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, TOTP  ,NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, ORGP,  NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, CACO,  NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, EXTAL, NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, EXTFE, NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, EXTMN, NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, TOTBAS,NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, PTERMA,NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, PTERMB,NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, EXK,   NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, EXMG,  NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, EXNA,  NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, EXTS,  NLAYRO, DS)
         CALL LMATCH (NLAYRI,ZZLYR, SLEC,  NLAYRO, DS)

C        Add other variables depending on model capabilities
C
      
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  950 FORMAT (/,5X,' SOIL ',A10,' is not in the SOIL file SOIL.SOL !')
 5000 FORMAT (/, '  More.... press < ENTER > key ')
 5030 FORMAT (1X,A10, 2X, A11,1X,A5,1X,F5.0,1X,A50)
 5035 FORMAT (2(1X,A11),2(1X,F8.3),1X,A50)
 5040 FORMAT (1X,A5,6(1X,F5.0),4(1X,A5))
 5080 FORMAT (1X,F5.0,1X,A5,29(1X,F5.0))
 5090 FORMAT (1X,F5.0,18(1X,F5.0))
 5101 FORMAT (10X,'ERROR! Soil Selection must be between 1 and ',I3,/)
 5102 FORMAT (10X,'ERROR! Soil Selection must be an INTEGER value',/)
 5130 FORMAT (T25, 'SOILS IN THE DATA BASE', /,T3, 'REF', T25,
     &        22('='),/,T3, 'NO.', T7, 'TAXONOMY NAME', T67,
     &        'PEDON NUMBER', /T2, 4('-'), 1X, 50('-'), T67, 12('-'))
 5140 FORMAT (I4,') ',A50, T67, A10)
 5160 FORMAT (/,6X,'SELECTED SOIL TYPE ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?    --->',2X,' ',$)

      END SUBROUTINE IPSOIL
