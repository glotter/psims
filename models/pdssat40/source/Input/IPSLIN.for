C=======================================================================
C  IPSLIN, Subroutine
C
C  Reads soil initial conditions from FILEX file
C  Read initial values of rapidly changing soil variables,
C  and convert values to standard soil depths
C-----------------------------------------------------------------------
C  Revision history
C
C  06/21/1991 JWJ Written
C  05/28/1993 PWW Header revision and minor changes
C  12/01/1993 WTB Modifed to read soil test P
C  08/19/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  INPUT  : FILEX,LNIC,NLAYR,DUL,SWINIT,PEDON,SLNO
C
C  LOCAL  : LN,NLAYRI,NLAYRO,DS,DLAYRI,LINEXP,ISECT,CHARTEST,LUNEXP
C
C  OUTPUT : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,INO3,INH4,SWINIT
C-----------------------------------------------------------------------
C  Called : SESOIL INPUT
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSLIN (FILEX,LNIC,NLAYR,DUL,YRIC,PRCROP,WRESR,
     &        WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,SWINIT,INH4,INO3,
     &        ISWWAT,ISWNIT,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID,YRSIM)

      USE ModuleDefs
      IMPLICIT     NONE

      CHARACTER*1  ISWWAT,ISWNIT
      CHARACTER*2  PRCROP
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*10 PEDON,SLNO
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER      I,LN,LUNEXP,NLAYR,NLAYRI,NLAYRO,LINEXP,ISECT,LNIC,
     &             YRIC,ERRNUM,IFIND,YRSIM
      REAL         DS(NL),DLAYRI(NL),SWINIT(NL)
      REAL         DUL(NL),WRESR,WRESND,EFINOC,EFNFIX,INO3(NL),INH4(NL)
      REAL         ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID

      PARAMETER   (LUNEXP = 16)
      PARAMETER   (ERRKEY = 'IPSLIN')

      FINDCH = '*INITI'
C
C     Set default initial conditions in case they are missing
C
      YRIC = 0
      PRCROP = '  '
      WRESR  = 0.0
C-GH  WRESR  = 1.0
      WRESND = 0.0
      EFINOC = 1.0
      EFNFIX = 1.0
      ICWD   = 0.0
      ICRES  = 0.0
      ICREN  = 0.0
      ICREP  = 0.0
      ICRIP  = 100.0
      ICRID  = 0.0
C-GH	ICRID  = 15.0
C
C     Need to modify these default values
C
      DO I = 1, NLAYR
         SWINIT(I) = DUL(I)
         INO3(I)   = 0.1
         INH4(I)   = 0.1
      END DO

      IF (PEDON .NE. SLNO) RETURN
      IF (LNIC  .LE. 0)    RETURN

      OPEN (LUNEXP,FILE = FILEX,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,0)

      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50   CONTINUE
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

      IF (ISECT .EQ. 1) THEN
          READ (CHARTEST,55,IOSTAT=ERRNUM) LN
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
          IF (LN .NE. LNIC) GO TO 50
          READ (CHARTEST,55,IOSTAT=ERRNUM) LN,PRCROP,YRIC,WRESR,
     &       WRESND,EFINOC,EFNFIX,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID

          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
       ELSE
         CALL ERROR (ERRKEY,2,FILEX,LINEXP)
      ENDIF
      
	IF (YRIC .LT. 0) THEN
	  YRIC = YRSIM
      ENDIF
      CALL  Y2K_DOY (YRIC)
      IF (ISWNIT .EQ. 'Y') THEN
         WRESR = MAX(WRESR,0.0)
C-GH	   IF (WRESR  .LT. 1.0) WRESR  = 1.0
         IF (WRESND .LT. 0.0) WRESND = 0.0
         IF (EFINOC .LT. 0.0) EFINOC = 1.0
         IF (EFNFIX .LT. 0.0) EFNFIX = 1.0
         ICWD  = MAX(ICWD,0.0)
C-PW     ICRES = MAX(ICRES,10.0)
         ICRES = MAX(ICRES,0.0)
         ICREN = MAX(ICREN,0.0)
         ICREP = MAX(ICREP,0.0)
         ICRID = MAX(ICRID,0.0)
C-GH	   ICRID = MAX(ICRID,15.0)
         IF (ICRIP  .LT. 0.0) ICRIP  = 100.0
      ENDIF

      NLAYRI = 1
C
C     Read layer information for the correct IC level number
C
 70   CONTINUE
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (LN .NE. LNIC) GO TO 70
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,DLAYRI(NLAYRI),
     &             SWINIT(NLAYRI),INH4(NLAYRI),INO3(NLAYRI)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (ISWWAT .NE. 'N') THEN
            IF (SWINIT(NLAYRI) .GT. 0.75) THEN
               CALL ERROR (ERRKEY,10,FILEX,LINEXP)
            ENDIF
            IF (SWINIT(NLAYRI) .LT. 0.00) THEN
                SWINIT(NLAYRI) = DUL(NLAYRI)
            ENDIF
         ENDIF

         IF (ISWNIT .EQ. 'Y') THEN
            IF (INH4(NLAYRI) .LT.   0.0) THEN
               INH4(NLAYRI) = 0.01
            ENDIF
            IF (INH4(NLAYRI) .GT. 100.0) THEN
               CALL ERROR (ERRKEY,11,FILEX,LINEXP)
            ENDIF
            IF (INO3(NLAYRI) .LT.   0.0) THEN
               INO3(NLAYRI) = 0.01
            ENDIF
            IF (INO3(NLAYRI) .GT. 100.0) THEN
               CALL ERROR (ERRKEY,12,FILEX,LINEXP)
            ENDIF
         ENDIF

         NLAYRI = NLAYRI + 1
         GO TO 70

      ENDIF

      CLOSE (LUNEXP)
      NLAYRI = NLAYRI - 1

      CALL LMATCH (NLAYRI,DLAYRI,SWINIT,NLAYRO,DS)
      CALL LMATCH (NLAYRI,DLAYRI,INH4,  NLAYRO,DS)
      CALL LMATCH (NLAYRI,DLAYRI,INO3,  NLAYRO,DS)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,3X,A2,1X,I5,10(1X,F5.0))
 60   FORMAT (I3,F5.0,3(1X,F5.0))

      END SUBROUTINE IPSLIN

C=======================================================================
C  IPSLAN, Subroutine
C
C  Reads soil analysis data from FILEX file
C  Read initial values of soil variables, such as fertility,
C  that change at a medium rate, adjust soil depths to standard
C  Created 01-JUL-91, J. Jones to read new IBSNAT formats
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       J. W. Jones 7-01-91
C  2  Modified by
C  3. Modifed to read soil test P                   W.D.B      11-18-92
C  4. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : FILEX,LNSA,PEDON,SLNO,BD,OC,PH
C
C  LOCAL  : LUNEXP,LN,NLAYRI,NLAYRO,DS,SABL,LINEXP,ISECT,CHARTEST,
C           SADM,SAOC,SANI,SAPHW,SAPHB,SAPX,SAKE,
C           SADAT,SMDM,SMOC,SMNI,SMHW,SMHB,SMPX,SMKE
C
C  OUTPUT : BD,OC,PH,EXTP
C-----------------------------------------------------------------------
C  Called : SESOIL INPUT
C
C  Calls  : FIND IGNORE ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSLAN (FILEX,LNSA,BD,OC,PH,PEDON,SLNO,DS,EXTP,TOTN)

      USE ModuleDefs
      IMPLICIT     NONE

      CHARACTER*5  SMKE,SMHB,SMPX
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*10 SLNO,PEDON
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST*80

      INTEGER      LN,LUNEXP,NLAYRI,NLAYRO,LINEXP,ISECT,LNSA
      INTEGER      ERRNUM,SADAT,IFIND,L

      REAL         SABL(NL),SADM(NL),SAOC(NL),SANI(NL),SAPHW(NL)
      REAL         SAPX(NL),SAKE(NL),BD(NL),OC(NL),DS(NL),SAPHB(NL)
      REAL         PH(NL),EXTP(NL),TOTN(NL)

      PARAMETER   (LUNEXP = 16)
      PARAMETER   (ERRKEY = 'IPSLAN')

                   FINDCH = '*SOIL '

      IF (LNSA  .LE. 0)    RETURN
      IF (PEDON .NE. SLNO) RETURN

      OPEN (LUNEXP,FILE = FILEX,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,0)

      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)

 50   CONTINUE
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,55,IOSTAT=ERRNUM) LN
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (LN .NE. LNSA) GO TO 50

         READ (CHARTEST,55,IOSTAT=ERRNUM) LN,SADAT,SMHB,SMPX,SMKE
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
       ELSE
         CALL ERROR (ERRKEY,2,FILEX,LINEXP)
      ENDIF
	CALL Y2K_DOY (SADAT)
C
C     Read layer information for the correct soil analysis number
C
      NLAYRI = 1
 70   CONTINUE
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)

      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IF (LN .NE. LNSA) GO TO 70

         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,SABL(NLAYRI),
     &         SADM (NLAYRI),SAOC(NLAYRI),SANI(NLAYRI),SAPHW(NLAYRI),
     &         SAPHB(NLAYRI),SAPX(NLAYRI),SAKE(NLAYRI)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)

         IF (SADM(NLAYRI) .GT. 10.0) THEN
            CALL ERROR (ERRKEY,10,FILEX,LINEXP)
         ENDIF
         IF (SAOC(NLAYRI) .GT. 100.0) THEN
            CALL ERROR (ERRKEY,11,FILEX,LINEXP)
         ENDIF
         IF (SANI(NLAYRI) .GT. 10.0) THEN
            CALL ERROR (ERRKEY,12,FILEX,LINEXP)
         ENDIF
         NLAYRI = NLAYRI + 1
         GO TO 70
      ENDIF

      CLOSE (LUNEXP)
      NLAYRI = NLAYRI - 1

      IF (SADM(1) .GT.  0.0) THEN
        CALL LMATCH (NLAYRI, SABL, SADM,  NLAYRO, DS)
      ENDIF
      IF (SAOC(1) .GT.  0.0) THEN
        CALL LMATCH (NLAYRI, SABL, SAOC,  NLAYRO, DS)
      ENDIF
      IF (SANI(1) .GT.  0.0) THEN
        CALL LMATCH (NLAYRI, SABL, SANI,  NLAYRO, DS)
      ENDIF
      IF (SAPHW(1) .GT.  0.0) THEN
        CALL LMATCH (NLAYRI, SABL, SAPHW, NLAYRO, DS)
      ENDIF
      IF (SAPHB(1) .GT.  0.0) THEN
        CALL LMATCH (NLAYRI, SABL, SAPHB, NLAYRO, DS)
      ENDIF
      IF (SAPX(1) .GT.  0.0) THEN
        CALL LMATCH (NLAYRI, SABL, SAPX,  NLAYRO, DS)
      ENDIF
      IF (SAKE(1) .GT.  0.0) THEN
        CALL LMATCH (NLAYRI, SABL, SAKE,  NLAYRO, DS)
      ENDIF
C
C     Use soil analysis values for NLAYRO layers for which data were
C          in the experimental field; for other layers the values from
C          the soil profile will be retained and used. Only replace
C          those values if data were available (i.e., not < 0.)
C
      DO L = 1, NLAYRO
         IF (SADM(L) .GT. 0.0) BD(L)    = SADM(L)
         IF (SAOC(L) .GT. 0.0) OC(L)    = SAOC(L)
         IF (SANI(L) .GT. 0.0) TOTN(L)  = SANI(L)
         IF (SAPHW(L) .GT. 0.0) PH(L)   = SAPHW(L)
         IF (SAPX(L)  .GT. 0.0) EXTP(L) = SAPX(L)
      ENDDO
C
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

 55   FORMAT (I3,2X,I3,3(1X,A5))
 60   FORMAT (I3,F5.0,7(1X,F5.0))

      END SUBROUTINE IPSLAN

!
