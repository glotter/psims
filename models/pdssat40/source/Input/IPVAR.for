C=======================================================================
C  IPVAR, Subroutine
C
C  Reads in genetic information for crop
C-----------------------------------------------------------------------
C  Revision       History
C  01/01/1990 GH  Written
C  05/28/1993 PWW Header revision and minor changes  
C  12/14/2000 GH  Version correction
C-----------------------------------------------------------------------
C  INPUT  : FILEG,NSENS,VARNO,VARTY,VRNAME,PATHGE,ECONO
C
C  LOCAL  : LINE,BLANK,ANS,ERRKEY,C255,FILEGG,ERRNUM,LINVAR,LUNVAR,I,ISECT,
C           PATHL,NLOOP,NLVAR,FLAG,VAR
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : SEVAR SENS INPUT
C
C  Calls  : ERROR CLEAR IGNORE VERIFY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      SUBROUTINE IPVAR (FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,
     &                  PATHGE,ECONO,CROP)

      IMPLICIT NONE

      INCLUDE 'COMGEN.BLK'

      CHARACTER*1   LINE(80),RNMODE,BLANK,ANS
      CHARACTER*2   CROP
      CHARACTER*6   VARTY,VARNO,ERRKEY,ECONO
      CHARACTER*12  FILEG
      CHARACTER*16  VRNAME
      CHARACTER*80  PATHGE
      CHARACTER*92  FILEGG
      CHARACTER*255 C255

      INTEGER       I,L,NSENS,NLVAR,LUNVAR,LINVAR,ISECT,NLOOP
      INTEGER       ERRNUM,PATHL
      REAL          FLAG,VAR

      PARAMETER (LUNVAR = 19)
      PARAMETER (ERRKEY = 'IPVAR ')
      PARAMETER (BLANK  = ' ')

      PATHL  = INDEX (PATHGE,BLANK)

      IF (PATHL .LE. 1) THEN
         FILEGG = FILEG
       ELSE
         FILEGG = PATHGE(1:(PATHL-1)) // FILEG
      ENDIF
C-----------------------------------------------------------------------
C    Read Cultivar Specific Genetics/Cultivar Parameter File
C-----------------------------------------------------------------------

      OPEN (LUNVAR,FILE = FILEGG,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,0)
      IF (NSENS .EQ. 1) THEN
         I  = 1
         NLOOP = 0
         IF (INDEX('IE',RNMODE) .GT. 0) THEN
            CALL CLEAR
            WRITE (*,100)
         ENDIF
  200    CONTINUE
         CALL IGNORE (LUNVAR,LINVAR,ISECT,C255)
         IF (ISECT .EQ. 0) GO TO 211
         IF (ISECT .EQ. 2) GO TO 200
         IF (C255(1:1) .EQ. ' ' .OR. C255(1:1) .EQ. '*' 
     &   .OR. C255(1:1) .EQ. '$') GO TO 200
         READ (C255, 700, IOSTAT=ERRNUM) VARTY,VRNAME,ECONO
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,LINVAR)
         IF (INDEX('IE',RNMODE) .GT. 0) THEN
            WRITE (*,750) I,VARTY,VRNAME,ECONO,ECONO(3:4)
         ENDIF
         IF (VARTY .EQ. VARNO) NLVAR = I
C
C        Write Pause Statement Every 15 lines
C
         IF (MOD(I,15) .EQ. 0 .AND. INDEX('IE',RNMODE) .GT. 0) THEN
            WRITE (*,300)
            READ  (5,'(A1)') ANS
         ENDIF
         I  = I + 1
         GOTO 200

  211    CONTINUE
         NLOOP = NLOOP + 1
         IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,FILEG,LINVAR)
         LINE(1) = ' '
         IF (INDEX('IE',RNMODE) .GT. 0) WRITE (*,1300) NLVAR
         READ (5, 1400) LINE
         CALL VERIFY (LINE,VAR,FLAG)
         IF (VAR .LE. 0) THEN
            VAR = NLVAR
          ELSE IF ((FLAG .GT. 0) .OR. (VAR .GT. (I-1))) THEN
            WRITE (*,1200) I -1
            GO TO 211
          ELSE IF (VAR .NE. NINT(VAR)) THEN
            WRITE (*,1201)
            GO TO 211
          ELSE IF (VAR .GT. 0) THEN
            NLVAR = NINT(VAR)
          ELSE
            GO TO 211
         ENDIF

         REWIND (LUNVAR)
      ENDIF

      I = 0

 2010 CONTINUE
      I = I + 1
 2000 CONTINUE
      CALL IGNORE (LUNVAR, LINVAR, ISECT, C255)
      IF (ISECT .EQ. 0) CALL ERROR (ERRKEY,2,FILEG,LINVAR)
      IF (ISECT .EQ. 2) GO TO 2000
      IF (C255(1:1) .EQ. ' ' .OR. C255(1:1) .EQ. '*') GO TO 2000

!     CROPGRO crops
      IF (INDEX ('BNPNSBTMPECHPPPRC3C4G0G1G2G3G4G5G6G7G8BRVBCPCBFBCOCT',
     &            CROP) .GT. 0) THEN
         READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,CSDVAR,
     &        PPSEN,PH2T5,PHTHRS(6),PHTHRS(8),PHTHRS(10),PHTHRS(13),
     &        LFMAX,SLAVAR,SIZELF,XFRUIT,WTPSD,SFDUR,SDPDVR,PODUR
         IF (LFMAX  .LE. 0) CALL ERROR (ERRKEY,22,FILEG,LINVAR)
         IF (SLAVAR .LE. 0) CALL ERROR (ERRKEY,23,FILEG,LINVAR)
         IF (SIZELF .LE. 0) CALL ERROR (ERRKEY,23,FILEG,LINVAR)
         IF (XFRUIT .LE. 0) CALL ERROR (ERRKEY,24,FILEG,LINVAR)
         IF (WTPSD  .LE. 0) CALL ERROR (ERRKEY,25,FILEG,LINVAR)
         IF (SDPDVR .LE. 0) CALL ERROR (ERRKEY,26,FILEG,LINVAR)
         IF (SFDUR  .LE. 0) CALL ERROR (ERRKEY,27,FILEG,LINVAR)
         IF (PODUR  .LE. 0) CALL ERROR (ERRKEY,28,FILEG,LINVAR)

       ELSEIF (INDEX ('MZMLSGWHBA',CROP) .GT. 0) THEN
         !Maize
         IF (CROP .EQ. 'MZ') THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P5,G2,G3,PHINT
         !Wheat
          ELSE IF (CROP .EQ. 'WH') THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1V,P1D,P5,G1,G2,G3,PHINT
C-GH     &            P1V,P1D,P5,G1,G2,G3,PHINT,GPROT,LT50H,P1
         !Sorghum
          ELSE IF (CROP .EQ. 'SG') THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G2,PHINT
         !Millet
          ELSE IF (CROP .EQ. 'ML') THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2O,P2R,P5,G1,G4,PHINT
         !Barley
          ELSE IF (CROP .EQ. 'BA') THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1V,P1D,P5,G1,G2,G3,PHINT
         ENDIF

      !Potato
       ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
            READ (C255, 800,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            G2,G3,G4,PD,P2,TC
      !Cassava
       ELSEIF (INDEX ('CS',CROP) .GT. 0) THEN
            READ (C255, 900,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &           (GCOEFF(L),L=1,15)
      !Rice
       ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
            READ (C255,950,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2R,P5,P2O,G1,G2,G3,G4

      !Sugarcane
       ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
            READ (C255,1000,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,RATPOT,LFMAX,G1,PI1,PI2,DTTPI
      !Sunflower
       ELSEIF (INDEX ('SU',CROP) .GT. 0) THEN
            READ (C255,1100,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P2,P5,G2,G3,O1
      !Pineapple
       ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
            READ (C255,1150,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P2,P3,P4,G2,G3,PHINT
      !Taro, tanier
       ELSEIF (INDEX ('TRTN',CROP) .GT. 0) THEN
            READ (C255,1175,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            P1,P3,P4,P5,G3,G4,PHINT,PCINT,PCGRD
      !Cotton
       ELSEIF (CROP .EQ. 'CO') THEN
            READ (C255,1185,IOSTAT=ERRNUM) VARTY,VRNAME,ECONO,
     &            SCPB,RESPC,SQCON,FCUT,FLAI,DDISQ
      ENDIF

      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEG,LINVAR)
      IF (((VARTY .NE. VARNO) .AND. (NSENS .EQ. 0)) .OR.
     &        ((I .LT. NLVAR) .AND. (NSENS .EQ. 1))) GO TO 2010

      VARNO = VARTY
      CLOSE (LUNVAR)

      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT (T30,'VARIETY SELECTION',/,T30,'=================',
     &     //,T43,'ECOTYPE',2X,'MATURITY',
     &      /,2X,'NO.',1X,'ENTRY',3X,'VARIETY',22X,'GROUP',5X,'GROUP',
     &      /,2X,'---',1X,'------',2X,20('-'),8X,'-------',2X,
     &           '--------')
  300 FORMAT (/,'  More.... press < ENTER > key')
  700 FORMAT (A6,1X,A16,1X,A6,6X,A2)
  750 FORMAT (I4,') ',A6,2X,A16,13X,A6,6X,A2)
c 800 FORMAT (A6,1X,A16,1X,A6,15(1X,F5.0))
  800 FORMAT (A6,1X,A16,1X,A6,15(F6.0))
  900 FORMAT (A6,1X,A16,1X,A6,2(F6.1),F6.2,2(F6.1),F6.2,F6.0,
     &        F6.3,F6.2,5(F6.0))
  950 FORMAT (A6,1X,A16,1X,A6,5(F6.1),F6.4,2(F6.2))
 1000 FORMAT (A6,1X,A16,1X,A6,1X,7(F6.1))
c1100 FORMAT (A6,1X,A16,1X,A6,1X,F6.2,F8.4,F7.2,F8.2,F7.3,F4.0)
 1100 FORMAT (A6,1X,A16,1X,A6,1X,F5.1,1X,F5.2,1X,F5.1,1X,F5.0,1X,F5.2,
     &        1X,F5.0)
 1150 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.1,F6.0,F6.1,F6.2,F6.1)
 1175 FORMAT (A6,1X,A16,1X,A6,4(F6.0),2(F6.2),3(F6.1))
 1185 FORMAT (A6,1X,A16,1X,A6,6(F6.0))
 1200 FORMAT (6X,'ERROR! Variety Selection must be between 1 & ',I3,/)
 1201 FORMAT (6X,'ERROR! Variety Selection must be an INTEGER value',/)
 1300 FORMAT (/,6X,'VARIETY SELECTED ===>',1X,I4,
     &        /,6X,'NEW SELECTION ?  --->',3X,' ',$)
 1400 FORMAT (80A1)

      END SUBROUTINE IPVAR
