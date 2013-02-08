C=======================================================================
C  IPEXP, Subroutine
C
C  Determines experiment and treatment selection
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 JWW Written
C  05/28/1993 PWW Header revision and minor changes            
C  12/10/1995 NBP Soil top 2 layers=10 cm, error check on MEEVP & MEEVP
C  09/30/1997 GH  Added cotton                                 
C  01/19/1998 PWW Added species file for Ceres and OilCrop 
C  05/06/1998 GH  Changed date and version number to v3.5, May 15, 1998  
C  08/17/1999 GH  Added cabbage  
C  09/20/2000 GH  Replaced G9 with BR for Brachiaria decumbens
C  09/20/2000 GH  Changed MESOM for 'Parton and Godwin options
C  09/20/2000 GH  Changed to version 3.90 (990)
C  11/03/2001 GH  Add CASUPRO
C  12/12/2001 GH  Extract model information
C  01/30/2002 GH  Modify inputs for the wheat model
C  06/07/2002 GH  Modify dates for Y2K
C  11/21/2002 GH  Modify rotation options and reps
C-----------------------------------------------------------------------
C  INPUT  : MODEL,RUN,DS,SLNO,LNIC,LNSA,NYRS,VARNO,CROP,PATHMO,WMODI
C           FROP,SLTX
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR IGNORE VERIFY CLEAR FIND IPCUL PATH IPPLNT IPFLD IPSIM
C           YR_DOY IPENV IPHAR IPIRR IPRES IPFERT
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPEXP (MODEL,RUN,DS,SLNO,NYRS,VARNO,
     &           CROP,WMODI,FROP,TRTN,EXPP,EXPN,TITLET,TRTALL,
     &           TRNARG,IIRV,FTYPEN,CHEXTR,
     &           NFORC,PLTFOR,NDOF,PMTYPE,INPUT,MODELV,
     &           LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
     &           LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES, CRMODEL)


      USE ModuleDefs
      IMPLICIT NONE

      INCLUDE 'COMIBS.BLK'
      INCLUDE 'COMSWI.BLK'

      CHARACTER* 1 LINE(80),BLANK
      CHARACTER* 1 WMODI,ANS
      CHARACTER* 2 CROP
      CHARACTER* 3 TRNARG
      CHARACTER* 3 PROCOD,ALN(13),ALLN
      CHARACTER* 4 WSTA1
	CHARACTER* 5 CRMODEL
      CHARACTER* 6 VARNO,MODELV,ERRKEY,FINDCH
      CHARACTER* 7 FILELS
      CHARACTER*10 CROPS(45),SLNO
      CHARACTER*12 MODEL,NAMEF,INPUT
      CHARACTER*25 TITLET
      CHARACTER*42 CHEXTR(200)
      CHARACTER*80 CHARTEST
      CHARACTER*92 FILELL

      INTEGER I,L,NLOOP,LINF,ISECT,LUNEXP,LUNLST
	INTEGER LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,LNCHE,LNCU
      INTEGER LNHAR,LNENV,LNTIL,LNSIM,LINEXP
      INTEGER NYRS,FROP,EXPN,EXPP,TRTN,ERRNUM,IFIND,FTYPEN
      INTEGER PATHL,RUN,CRID,ISIM,TRTALL,IIRV(NAPPL)
      INTEGER NFORC,NDOF,PMTYPE,YR

      INTEGER M   
                  

      LOGICAL FEXIST

      REAL    FLAG,EXP,TRT,DS(NL),PLTFOR

C-----------------------------------------------------------------------

      PARAMETER (LUNEXP = 16)
      PARAMETER (LUNLST = 17)
      PARAMETER (ERRKEY = 'IPEXP ')
      PARAMETER (BLANK = ' ')
                 FINDCH = '*TREAT'
C-----------------------------------------------------------------------

      DATA CROPS/'DRY BEAN  ','PEANUT    ','SOYBEAN   ','COWPEA    ',
     &           'PEA       ','CHICKPEA  ','PIGEONPEA ','PEPPER    ',
     &           'RICE      ','FALLOW    ','MAIZE     ','WHEAT     ',
     &           'MILLET    ','SORGHUM   ','BARLEY    ','CASSAVA   ',
     &           'POTATO    ','TOMATO    ','C3-CROPS  ','C4-CROPS  ',
     &           'BAHIA     ','GRASS-1   ','GRASS-2   ','GRASS-3   ',
     &           'GRASS-4   ','GRASS-5   ','GRASS-6   ','GRASS-7   ',
     &           'GRASS-8   ','BRACHIARIA','SUGARCANE ','AROIDS    ',
     &           'SUNFLOWER ','PINEAPPLE ','TARO      ','TANIER    ',
     &           'COTTON    ','VELVETBEAN','CABBAGE   ','FABA BEAN ',
     &           'CITRUS    ','          ','          ','          ',
     &           '          ' /

C-----------------------------------------------------------------------
      FILELS = 'EXP.LST'

      
C-----------------------------------------------------------------------
C     Set depths of individual soil layers
C-----------------------------------------------------------------------
C     This subroutine assumes that DS(L) values are depths to the bottom
C     of layer L
C
C     DS(L) can be interactively modified in the sensitivity analysis

!     M = Soil layer at which profile transitions from 
!             30 cm to 60 cm thickness.
      M = 18  !soil layers 18, 19, 20 with thickness of 60 cm.

      DS(1) =  5.
      DS(2) = 15.
      DS(3) = 30.
      DS(4) = 45.
      DS(5) = 60.

      DO L = 6, M-1
         DS(L) = DS(L - 1) + 30.
      END DO

      DO L = M, NL
         DS(L) = DS(L - 1) + 60.
      END DO

C-----------------------------------------------------------------------
      NLOOP = 0
      IF (RUN .EQ. 1) THEN
         EXPN   = 1
         EXPP   = 0
         TRTN   = 1
         TRTALL = 999
       ELSE
         EXPN   = EXPP
      ENDIF

      IF (RNMODE .EQ. 'I') THEN
         OPEN (LUNLST, FILE = FILELS,STATUS = 'OLD',IOSTAT=ERRNUM)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,0)

         WRITE (*,200)
         I = 0
  300    CONTINUE
         I = I + 1
         LINF = 0
  350    CONTINUE
         CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
         IF (ISECT .EQ. 2) GO TO 350

         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,410,IOSTAT=ERRNUM) EXPER,CG,ENAME
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,LINEXP)
            IF (MOD(I,16) .EQ. 0) THEN
               WRITE (*,600)
               READ (5,'(A1)') ANS
            ENDIF
	      READ(EXPER(5:6),'(I2)') YR
	      IF (YR .GE. 10) THEN
            WRITE (*,500) I,CG,ENAME(1:45),EXPER(1:2),EXPER(3:4),
     &                    EXPER(5:6),EXPER(7:8)
          ELSE
	        WRITE (*,501) I,CG,ENAME(1:45),EXPER(1:2),EXPER(3:4),
     &                      EXPER(5:6),EXPER(7:8)
	      ENDIF

          ELSE
            GO TO 800
         ENDIF

         GO TO 300
  800    CONTINUE
         REWIND (LUNLST)

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

  850    CONTINUE
         LINE(1) = ' '
         NLOOP = NLOOP + 1
         IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,2,FILELS,0)
         WRITE (*,1000) EXPN
         READ  (5,1100) LINE
         CALL VERIFY (LINE,EXP,FLAG)

         IF (EXP .LE. 0.0) THEN
            EXP = EXPN
          ELSEIF ((FLAG .GT. 0) .OR. (EXP .GT. (I-1))) THEN
            WRITE (*,1101) (I-1)
            GO TO 850
          ELSEIF (EXP .NE. NINT(EXP)) THEN
            WRITE (*,1102)
            GO TO 850
          ELSEIF (EXP .GT. 0.0) THEN
            EXPN = NINT(EXP)
          ELSE
            CALL ERROR (ERRKEY,2,FILELS,0)
         ENDIF
C
C     Establish the name of the experiment input file
C
        I = 0
 950    CONTINUE
        I = I + 1
 975    CONTINUE
        CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
        IF (ISECT .EQ. 2) GO TO 975
        READ (CHARTEST,410,IOSTAT=ERRNUM) EXPER,CG,ENAME
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,LINEXP)
        IF (I .LT. EXPN) GO TO 950
        CLOSE (LUNLST)
	  FILEX(1:12) = EXPER//'.'//CG//'X'
	ELSE
	  READ(FILEX(10:11),'(A2)') CG
	  READ(FILEX(1:8),'(A8)') EXPER
	ENDIF

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        
      FILEA(1:12) = EXPER//'.'//CG//'A'
      FILET(1:12) = EXPER//'.'//CG//'T'

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      NLOOP = 0
      I     = 0

      OPEN (LUNEXP,FILE = FILEX,STATUS = 'OLD',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,0)
	READ(LUNEXP,1500) ENAME
 1500 FORMAT(25X,A60)
      IF (RNMODE .EQ. 'I') CALL CLEAR
      IF (EXPN .NE. EXPP) THEN
         TRTN = 1
      ENDIF
      IF (RNMODE .EQ. 'I' .OR. RNMODE .EQ. 'A' .AND.
     &   TRTALL .EQ. 999) THEN
         IF (RNMODE .EQ. 'I') WRITE (*,2300) ENAME(1:40)
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF(IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 2400    CONTINUE
         I = I + 1
         CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,54,IOSTAT=ERRNUM) (ALN(L),L=1,13)
   54             FORMAT (34X,13A3)
            DO L=1,13
              ALLN = ALN(L)
              IF (ALLN(3:3) .EQ. '?') THEN
                 ERRNUM = 99
                 CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
              ENDIF
            ENDDO
            IF (RNMODE .EQ. 'Q') THEN
              READ (CHARTEST,56,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,
     &              TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &              LNCHE,LNTIL,LNENV,LNHAR,LNSIM
            ELSE 
	        READ (CHARTEST,55,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,
     &              TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &              LNCHE,LNTIL,LNENV,LNHAR,LNSIM
	      ENDIF
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (MOD(I,16) .EQ. 0 .AND. RNMODE .EQ. 'I') THEN
               WRITE (*,600)
               READ (5,'(A1)') ANS
            ENDIF
	      READ(EXPER(5:6),'(I2)') YR
	      IF (YR .GE. 10) THEN
            IF (RNMODE .EQ. 'I') WRITE (*,2600) I,TITLET,
     &          EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8),TRTNO
          ELSE
	        IF (RNMODE .EQ. 'I') WRITE (*,2601) I,TITLET,
     &            EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8),TRTNO
	      ENDIF
          ELSE
            GO TO 2700
         ENDIF
         GO TO 2400
 2700    CONTINUE
        
         TRTALL = I - 1
         IF (RNMODE .EQ. 'A') TRTN = 1
C-GH     IF (RNMODE .EQ. 'I') 
C-GH     &   WRITE (*,2650) I,EXPER(1:2),EXPER(3:4),EXPER(5:6),EXPER(7:8)
         
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
 2750    CONTINUE
         NLOOP = NLOOP + 1
         LINE(1) = ' '
         IF (NLOOP .GT. 25) CALL ERROR(ERRKEY,3,FILEX,LINEXP)
         IF (RNMODE .EQ. 'I') THEN
           WRITE (*,2900) TRTN
C
C        Read the correct treatment number
C
           READ (5,1100) LINE
           CALL VERIFY (LINE,TRT,FLAG)
         ENDIF
         IF (TRT .LE. 0.0) THEN
            TRT = TRTN
C-GH      ELSEIF (TRT .EQ. (TRTALL+1) .AND. RNMODE .EQ. 'I') THEN
C-GH        RNMODE = 'A'
C-GH        TRTN   = 1
          ELSEIF ((FLAG .GT. 0) .OR. (TRT .GT. I)) THEN
            WRITE (*,2751) (I-1)
            GO TO 2750
          ELSEIF (TRT .NE. NINT(TRT)) THEN
            WRITE(*,2752)
            GO TO 2750
          ELSEIF (TRT .GT. 0.) THEN
            TRTN = NINT(TRT)
          ELSE
            CALL ERROR (ERRKEY,3,FILEX,LINEXP)
         ENDIF
       ELSEIF (INDEX ('NQGSFBEC',RNMODE) .GT. 0) THEN
         READ (TRNARG(1:3),'(I3)') TRTN
         I = 999
       ELSEIF (INDEX ('A',RNMODE) .GT. 0) THEN
         TRTN = TRTN + 1
      ENDIF
	
C-----------------------------------------------------------------------
C     Find treatment number and appropriate levels
C-----------------------------------------------------------------------
      REWIND (LUNEXP)
      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)

      I = 0
 50   CONTINUE
      I = I + 1
      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
	IF (RNMODE .EQ. 'Q') THEN
        READ (CHARTEST,56,IOSTAT=ERRNUM) TRTNO,ROTNO,ROTOPT,CRPNO,
     &     TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &     LNCHE,LNTIL,LNENV,LNHAR,LNSIM
	ELSE
        READ (CHARTEST,55,IOSTAT=ERRNUM) TRTNO,ROTNO,ROTOPT,CRPNO,
     &     TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &     LNCHE,LNTIL,LNENV,LNHAR,LNSIM
      ENDIF
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
C     IF (I .LT. TRTN) GO TO 50
      IF ((INDEX('BEDNSGFC',RNMODE) .GT. 0 .AND. TRTN .NE. TRTNO)
     & .OR. (INDEX('Q',RNMODE) .GT. 0 .AND. TRTN .NE. ROTNO)
     & .OR. (INDEX('AI',RNMODE) .GT. 0 .AND. I .LT. TRTN))
     &    GO TO 50

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

c      IF (RNMODE .EQ. 'I' .OR. RNMODE .EQ. 'A' .AND. TRTN .EQ. 1) THEN
c         CALL CLEAR
c         WRITE(*,3450)
c      ENDIF

C-----------------------------------------------------------------------
C     Call input section for cultivar selection
C-----------------------------------------------------------------------

      CALL IPCUL (LUNEXP,FILEX,LNCU,CROP,VARNO)
      IF (CROP   .EQ. '  ') CALL ERROR (ERRKEY,10,FILEX,LINEXP)
      IF (VARNO  .EQ. '  ') CALL ERROR (ERRKEY,11,FILEX,LINEXP)

C-----------------------------------------------------------------------
C    Select Model Name and Path
C-----------------------------------------------------------------------
C    MODEL and CROP should be modified when model versions change
C     or when a crop specific model is created.
C
C     GRO  - generic cropGRO model Version 4.00 (2003)
C     CROP = BN for CROPGRO - DRY BEAN    Version 4.0 (2003)
C     CROP = PN for CROPGRO - PEANUT      Version 4.0 (2003)
C     CROP = SB for CROPGRO - SOYBEAN     Version 4.0 (2003)
C     CROP = FA for CROPGRO - FALLOW      Version 4.0 (2003)
C     CROP = TM for CROPGRO - TOMATO      Version 4.0 (2003)
C     CROP = PR for CROPGRO - PEPPER      Version 4.0 (2003)
C     CROP = PE for CROPGRO - PEA         Version 4.0 (2003)
C     CROP = CH for CROPGRO - CHICKPEA    Version 4.0 (2003)
C     CROP = PP for CROPGRO - PIGEONPEA   Version 4.0 (2003)
C     CROP = VB for CROPGRO - VELVETBEAN  Version 4.0 (2003)
C     CROP = CP for CROPGRO - COWPEA      Version 4.0 (2003)
C     CROP = CB for CROPGRO - CABBAGE     Version 4.0 (2003)
C     CROP = C3 for CROPGRO - C4 CROPS    Version 4.0 (2003)
C     CROP = C4 for CROPGRO - C3 CROPS    Version 4.0 (2003)
C     CROP = G0 for CROPGRO - BAHIA       Version 4.0 (2003)
C     CROP = G1 for CROPGRO - GRASSES     Version 4.0 (2003)
C     CROP = G2 for CROPGRO - GRASSES     Version 4.0 (2003)
C     CROP = G3 for CROPGRO - GRASSES     Version 4.0 (2003)
C     CROP = G4 for CROPGRO - GRASSES     Version 4.0 (2003)
C     CROP = G5 for CROPGRO - GRASSES     Version 4.0 (2003)
C     CROP = G6 for CROPGRO - GRASSES     Version 4.0 (2003)
C     CROP = G7 for CROPGRO - GRASSES     Version 4.0 (2003)
C     CROP = G8 for CROPGRO - GRASSES     Version 4.0 (2003)
C     CROP = BR for CROPGRO - Brachiaria
C                               decumbens Version 4.0 (2003)
C     CROP = FB for CROPGRO - FABA BEAN   Version 4.0 (2003)
C     CROP = CO for CROPGRO - COTTON      Version 4.0 (2003)
C     CROP = CT for CROPGRO - CITRUS      Version 4.0 (2003)
C
C     CER  - generic CEReal model Version 4.0 (2003)
C
C     CROP = MZ for CERES - Maize   Version 4.0 (2003)
C     CROP = WH for CERES - Wheat   Version 4.0 (2003)
C     CROP = BA for CERES - Barley  Version 4.0 (2003)
C     CROP = ML for CERES - Millet  Version 4.0 (2003)
C     CROP = SG for CERES - Sorghum Version 4.0 (2003)
C     CROP = RI for CERES - Rice    Version 4.00 (2003)
C
C     SIM  - generic cropSIM model Version 4.0 (2003)
C     CROP = CS for CROPSIM Cassava Version 4.0 (2003)
C
C     SUB  - generic SUBstor model Version 4.0 (2003)
C     CROP = PT for SUBSTOR Potato Version 4.0 (2003)
C
C     CAN  - CANegro model Version 4.0 (2003)
C     CROP = SC for Sugarcane Version 4.0 (2003)
C
C     OIL  - OILcrop model Version 4.0 (98.0)
C     CROP = SU for Sunflower Version 4.0 (2003)
C
C     ALO  - ALOha model Version 4.0 (2003)
C     CROP = PI for Pineapple Version 4.0 (2003)
C
C     ARO  - AROid model Version 4.0 (2003)
C     CROP = TR for Taro Version 4.0 (2003)
C     CROP = TN for Tanier Version 4.0 (2003)
C
C     COT  - COTton model Version 4.0 (2003)
C     CROP = CO for Cotton Version 4.0 (2003)
C
C     CSP  - CaSuPro Cane and Sucrose Production ModelVersion 4.0 (2003)
C     CROP = SC for CROPSIM Cassava Version 4.0 (2003)
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C     Call IPSIM
C-----------------------------------------------------------------------

      CALL IPSIM (LUNEXP,LNSIM,TITSIM,NYRS,RUN,NREPSQ,ISIMI,PWDINF,
     &     PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,PTX,PTTN,DSOIL,THETAC,
     &     IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,SOILNX,NEND,RIP,NRESDL,
     &     DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,RSEED1,LINEXP,AIRAMT,
     &     EFFIRR,CROP,FROP,CRMODEL)
      
      MODEL  = CRMODEL // INPUT(6:8)
	MODELV = MODEL(3:8)

      IF (CROP .NE. 'FA') THEN
C-----------------------------------------------------------------------
C        Select crop parameter input file
C-----------------------------------------------------------------------
         IF ((INDEX('GROgroSIMsim',MODELV(1:3)) .GT. 0) .OR.
     &      (INDEX('ALOaloCERcerSUBsubOILoil',MODELV(1:3)) .GT. 0) .OR.
     &      (INDEX('CSPcsp',MODELV(1:3)) .GT.0) .OR. 
     &      (INDEX('CSMcsm',MODELV(1:3)) .GT.0)) THEN
           FILEC(1:12) = CROP//MODELV//'.SPE'
           INQUIRE (FILE = FILEC,EXIST = FEXIST)
           IF (.NOT. FEXIST) THEN
              CALL PATH('CRD',DSSATP,PATHCR,1,NAMEF)
            ELSE
              PATHCR = BLANK
           ENDIF
         ENDIF
C-----------------------------------------------------------------------
C        Select genetic parameter input file
C
C        READ ???????1.CUL if RNMODE = G ;
C        READ ???????0.CUL for other modes;
C-----------------------------------------------------------------------
         FILEG(1:12) = CROP//MODELV//'.CUL'
         IF (RNMODE .EQ. 'G') THEN
            WRITE(FILEG(8:8),'(A1)') '1'
         ENDIF
         INQUIRE (FILE = FILEG,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            CALL PATH('CRD',DSSATP,PATHGE,1,NAMEF)
          ELSE
            PATHGE = BLANK
         ENDIF

C-----------------------------------------------------------------------
C        Select ecotype parameter input file
C-----------------------------------------------------------------------
         FILEE(1:12) = CROP//MODELV//'.ECO'
         INQUIRE (FILE = FILEE,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
             CALL PATH ('CRD',DSSATP,PATHEC,1,NAMEF)
           ELSE
              PATHEC = BLANK
         ENDIF
        
C-----------------------------------------------------------------------
C        Select pest parameter input file
C-----------------------------------------------------------------------

         FILEP(1:12) = CROP//MODELV//'.PST'
         INQUIRE (FILE = FILEP,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            CALL PATH('PSD',DSSATP,PATHPE,1,NAMEF)
          ELSE
            PATHPE = BLANK
         ENDIF

C-----------------------------------------------------------------------
C        End of IF NOT fallow
C-----------------------------------------------------------------------
      ENDIF

      IF (CROP .EQ. 'BN') CRID = 1
      IF (CROP .EQ. 'PN') CRID = 2
      IF (CROP .EQ. 'SB') CRID = 3
      IF (CROP .EQ. 'CP') CRID = 4
      IF (CROP .EQ. 'PE') CRID = 5
      IF (CROP .EQ. 'CH') CRID = 6
      IF (CROP .EQ. 'PP') CRID = 7
      IF (CROP .EQ. 'PR') CRID = 8
      IF (CROP .EQ. 'RI') CRID = 9
      IF (CROP .EQ. 'FA') CRID = 10
      IF (CROP .EQ. 'MZ') CRID = 11
      IF (CROP .EQ. 'WH') CRID = 12
      IF (CROP .EQ. 'ML') CRID = 13
      IF (CROP .EQ. 'SG') CRID = 14
      IF (CROP .EQ. 'BA') CRID = 15
      IF (CROP .EQ. 'CS') CRID = 16
      IF (CROP .EQ. 'PT') CRID = 17
      IF (CROP .EQ. 'TM') CRID = 18
      IF (CROP .EQ. 'C3') CRID = 19
      IF (CROP .EQ. 'C4') CRID = 20
      IF (CROP .EQ. 'G0') CRID = 21
      IF (CROP .EQ. 'G1') CRID = 22
      IF (CROP .EQ. 'G2') CRID = 23
      IF (CROP .EQ. 'G3') CRID = 24
      IF (CROP .EQ. 'G4') CRID = 25
      IF (CROP .EQ. 'G5') CRID = 26
      IF (CROP .EQ. 'G6') CRID = 27
      IF (CROP .EQ. 'G7') CRID = 28
      IF (CROP .EQ. 'G8') CRID = 29
      IF (CROP .EQ. 'BR') CRID = 30
      IF (CROP .EQ. 'SC') CRID = 31
      IF (CROP .EQ. 'SU') CRID = 33
      IF (CROP .EQ. 'PI') CRID = 34
      IF (CROP .EQ. 'TR') CRID = 35
      IF (CROP .EQ. 'TN') CRID = 36
      IF (CROP .EQ. 'CO') CRID = 37
      IF (CROP .EQ. 'VB') CRID = 38
      IF (CROP .EQ. 'CB') CRID = 39
      IF (CROP .EQ. 'FB') CRID = 40
      IF (CROP .EQ. 'CT') CRID = 41

      CROPD = CROPS(CRID)

      REWIND(LUNEXP)

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      CALL IPPLNT (LUNEXP,FILEX,LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,
     &     SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &     YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      CALL IPFLD (LUNEXP,FILEX,LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
     &     SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,
     &     XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS)

C-----------------------------------------------------------------------
C     Select soil profile input file
C       1. SOIL.SOL
C       2. ??.SOL where ?? = Institute ID from Soil Profile Number
C       3. From C:\DSSAT3\DSSATPRO.FLE  SOIL.SOL
C       4. From C:\DSSAT3\DSSATPRO.FLE  ??.SOL
C-----------------------------------------------------------------------

      FILES = 'SOIL.SOL'
      INQUIRE (FILE = FILES,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
         FILES(1:8) = SLNO(1:2)//'.SOL  '
         INQUIRE (FILE = FILES,EXIST = FEXIST)
         IF (.NOT. FEXIST) THEN
            PROCOD = 'SLD'
            CALL PATH (PROCOD,DSSATP,PATHSL,1,NAMEF)
            PATHL  = INDEX(PATHSL,BLANK)
            FILES = 'SOIL.SOL'
            FILELL = PATHSL(1:(PATHL-1)) // FILES
            INQUIRE (FILE = FILELL,EXIST = FEXIST)
            IF (.NOT. FEXIST) THEN
               FILES(1:8) = SLNO(1:2)//'.SOL  '
            ENDIF
         ENDIF
       ELSE
         PATHSL = BLANK
      ENDIF

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (ISIMI .EQ. 'S') THEN
         IF (YRSIM .LT. 0) THEN
           YRSIM = YRPLT
         ENDIF
      ELSE IF (ISIMI .EQ. 'P') THEN
           YRSIM = YRPLT
      ELSE IF (ISIMI .EQ. 'E') THEN
           YRSIM = IEMRG
           YRPLT = IEMRG
      ENDIF
      IF (CROP .EQ. 'FA' .AND. YRPLT .EQ. YRSIM) THEN
         YRSIM = YRSIM - 1
      ENDIF
      CALL YR_DOY (YRSIM,YEAR,ISIM)

C-----------------------------------------------------------------------
C     Now establish the weather file FILEW as WSTA + .WT?  where ? :
C
C          M = observed data
C          G = generated data
C          S = interactively generated
C-----------------------------------------------------------------------

      IF (MEWTH .EQ. 'G') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
            IF (YEAR .LT. 2000) THEN
	        YR = YEAR - 1900
	      ELSE IF (YEAR .LT. 3000) THEN
	        YR = YEAR - 2000
            ENDIF
            WRITE (FILEW(1:12),75) WSTA,YR,'01.WTG'
          ELSE
            WRITE (FILEW(1:12),76) WSTA,WSTA1,'.WTG'
         ENDIF
         PROCOD = 'WGD'
       ELSEIF (MEWTH .EQ. 'S' .OR. MEWTH .EQ. 'W') THEN
         WRITE (FILEW(1:12),77) WSTA,'.CLI    '
         PROCOD = 'CLD'
       ELSEIF (MEWTH .EQ. 'M') THEN
         IF (WSTA1(4:4) .EQ. BLANK) THEN
	     IF (YEAR .LT. 2000) THEN
	       YR = YEAR - 1900
	     ELSE IF (YEAR .LT. 3000) THEN
	       YR = YEAR - 2000
            ENDIF
           WRITE (FILEW(1:12),75) WSTA,YR,'01.WTH'
          ELSE
            WRITE(FILEW(1:12),76) WSTA,WSTA1,'.WTH'
         ENDIF
         PROCOD = 'WED'
       ELSE
         CALL ERROR (ERRKEY,22,FILEX,LINEXP)
      ENDIF

      INQUIRE (FILE = FILEW,EXIST = FEXIST)
      IF (.NOT. FEXIST) THEN
         CALL PATH(PROCOD,DSSATP,PATHWT,1,NAMEF)
       ELSE
         PATHWT = BLANK
      ENDIF

C-----------------------------------------------------------------------
C     Build output files.
C
C     Generic output file names with extension 'OUT' are overwritten
C     at the start of each simulation.
C
C     IOX = 'Y' creates experiment specific output file names
C-----------------------------------------------------------------------
      IF (IOX .EQ. 'Y') THEN
         WRITE (OUTO(1:12),80) EXPER,'.',CG,'O'
       ELSE
         OUTO  = 'OVERVIEW.OUT'
      ENDIF

C-----------------------------------------------------------------------
C     Call IPENV
C-----------------------------------------------------------------------
      CALL IPENV (FILEX,LNENV,LUNEXP,CO2ADJ,CO2FAC,DAYADJ,
     &     DAYFAC,DPTADJ,DPTFAC,NEV,PRCADJ,PRCFAC,RADADJ,RADFAC,
     &     TMADJ,TMFAC,TXADJ,TXFAC,WMDATE,WMODI,WNDADJ,WNDFAC,
     &     WTHADJ)

C-----------------------------------------------------------------------
C     Call IPHAR
C-----------------------------------------------------------------------
      CALL IPHAR (LUNEXP,FILEX,LNHAR,HDATE,HSTG,HCOM,HSIZ,HPC,
     &     NHAR,IHARI,YRSIM,CROP,HBPC)

C-----------------------------------------------------------------------
C     Call IPIRR
C-----------------------------------------------------------------------
      CALL IPIRR (LUNEXP,FILEX,LNIR,YRSIM,ISWWAT,
     &     NIRR,EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,IAMEX,LNSIM,
     &     NAPW,TOTAPW,AIRAMX,IDLAPL,IRRCOD,AMT,IIRV,IIRRI)

C-----------------------------------------------------------------------
C     Call IPFERT
C-----------------------------------------------------------------------
      CALL IPFERT (LUNEXP,FILEX,LNFER,YRSIM,ISWNIT,ISWPHO,
     &     NFERT,FDAY,IFTYPE,FERCOD,DFERT,ANFER,APFER,AKFER,ACFER,
     &     AOFER,FOCOD,TOTNAP,IFERI,ISWWAT,LNSIM)

C-----------------------------------------------------------------------
C     Call IPRES
C-----------------------------------------------------------------------
      CALL IPRES (LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,
     &     RINP,DEPRES,RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,
     &     ISWPHO,ISWPOT,IRESI,ISWWAT,RMET,LNSIM)

C-----------------------------------------------------------------------
C     Call IPCHEM - Chemical applications
C-----------------------------------------------------------------------
      CALL IPCHEM (LUNEXP,FILEX,LNCHE,YRSIM,ISWWAT,NCHEM,CDATE,
     &    CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,LNSIM,CHEXTR)

C-----------------------------------------------------------------------
C     Call IPTILL - Tillage operations
C-----------------------------------------------------------------------
      CALL IPTILL (LUNEXP,FILEX,LNTIL,YRSIM,ISWTIL,NTIL,TDATE,
     &    TIMPL,TDEP,LNSIM)

      CLOSE(LUNEXP)
      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

   55 FORMAT (I3,I1,2(1X,I1),1X,A25,14I3)
   56 FORMAT (2I2,2(1X,I1),1X,A25,14I3)

   75 FORMAT (A4,I2.2,A6)
   76 FORMAT (3A4)
   77 FORMAT (A4,A8)
   80 FORMAT (A8,A1,A2,A1)
  200 FORMAT (T57,'INST.',T64,'SITE',T70,'YEAR',T75,'EXPT.',
     &  /,T7,'CROP EXPERIMENTAL CASE STUDIES',T58,'ID',T65,'ID',
     &  T76,'NO',/T7,4('-'),1X,31('-'),T57,'----',
     &    T64,'----',T70,'----',T75,'----')
  410 FORMAT (3X,A8,1X,A2,2X,A60)
  500 FORMAT (1X,I3,'.',2X,A2,2X,A45,1X,A2,5X,A2,3X,'19',A2,2X,A2)
  501 FORMAT (1X,I3,'.',2X,A2,2X,A45,1X,A2,5X,A2,3X,'20',A2,2X,A2)
  600 FORMAT (/,'  More.... press < ENTER > key',$)
 1000 FORMAT (/,6X,'EXPERIMENT SELECTED ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?     --->',2X,' ',$)
 1100 FORMAT (80A1)
 1101 FORMAT (10X,'ERROR! Experiment Selection must be between 1',
     &            ' and ',I3,/)
 1102 FORMAT (10X,'ERROR! Experiment Selection must be an',
     &            ' INTEGER value',/)
 2300 FORMAT (T47,'INST.',T54,'SITE',T60,'YEAR',T66,'EXPT.',
     &  T72,'TRT.',/,T7,A40,T48,'ID',T55,'ID',T67,'NO',
     &  T73,'NO', /,T7,37('-'),T47,'----',
     &  T54,'----',T60,'----',T66,'----',T72,'----')
 2600 FORMAT (1X,I3,'.',1X,A25,16X,A2,5X,A2,3X,'19',A2,3X,A2,3X,I3)
 2601 FORMAT (1X,I3,'.',1X,A25,16X,A2,5X,A2,3X,'20',A2,3X,A2,3X,I3)
 2650 FORMAT (1X,I3,'.',1X,'RUN ALL TREATMENTS',23X,
     &        A2,5X,A2,3X,'19',A2,3X,A2,4X,I2)
 2751 FORMAT (10X,'ERROR! Treatment Selection must be between 1',
     &            ' and ',I3,/)
 2752 FORMAT (10X,'ERROR! Treatment Selection must be an INTEGER',/)
 2900 FORMAT (/,6X,'TREATMENT SELECTED ===>',1X,I3,
     &        /,6X,'NEW SELECTION ?    --->',2X,' ',$)
 3450 FORMAT (//////,15X,' Reading Data.  Please be patient.',/,
     &               15X,' Do not touch the keyboard !',/,16X,33('='))

      END

C=======================================================================
C  IPPLNT, Subroutine
C
C  Reads parameters related to planting operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       J.W.J       4-21-91
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNPLT
C
C  LOCAL  : LN
C
C  OUTPUT : IPLT,PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,PLTPOP,PLANTS,
C           SDAGE,ATEMP,PLPH,IEMRG
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPPLNT (LUNEXP,FILEX,LNPLT,PLME,PLDS,ROWSPC,AZIR,CROP,
     &     SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
     &     YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,IPLTI)

      IMPLICIT NONE

      CHARACTER*1   PLME,PLDS,IPLTI
      CHARACTER*2   CROP
      CHARACTER*6   ERRKEY,FINDCH
      CHARACTER*12  FILEX
      CHARACTER*110 CHARTEST

      INTEGER   LUNEXP,LNPLT,IEMRG,LN,LINEXP,ISECT,IFIND,ERRNUM
      INTEGER   IPLT,YRPLT,YR,NFORC,NDOF,PMTYPE

      REAL      ROWSPC,AZIR,SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP
      REAL      PLPH,SPRLAP,PLTFOR

      PARAMETER (ERRKEY='IPPLNT')
                 FINDCH='*PLANT'
      LINEXP = 0

      IF (LNPLT .GT. 0) THEN
         CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
C
C           Actual read statement for Planting inputs
C
            READ (CHARTEST,60,IOSTAT=ERRNUM) LN,YRPLT,IEMRG,PLANTS,
     &      PLTPOP,PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,
     &      PLPH,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE
C New variables for pineapple
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
	      CALL Y2K_DOY(YRPLT)
	      CALL Y2K_DOY(IEMRG)
            CALL YR_DOY (YRPLT,YR,IPLT)
          ELSE
            CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         IF (LN .NE. LNPLT) GO TO 50
	   IF (IPLTI .EQ. 'R') THEN
           IF ((YRPLT .LT. 1 .OR. YRPLT .GT. 9999999)
     &       .AND. IEMRG .LT. 1) THEN
              CALL ERROR (ERRKEY,10,FILEX,LINEXP)
	     ENDIF
         ENDIF
         IF (PLTPOP .LE. 0.0 .AND. PLANTS .GT. 0.0) THEN
            PLTPOP = PLANTS
         ENDIF
         IF (PLTPOP .LE. 0.0 .OR. PLTPOP .GT. 999.) THEN
            CALL ERROR (ERRKEY,11,FILEX,LINEXP)
         ENDIF
         IF ((ROWSPC .GT. -90. .AND. ROWSPC .LE. 0.0)
     &      .OR. ROWSPC .GT. 99999.) THEN
            CALL ERROR (ERRKEY,12,FILEX,LINEXP)
         ENDIF
         IF ((AZIR .GT. -90. .AND. AZIR .LT. 0.0)
     &      .OR. AZIR .GT. 99999.) THEN
            CALL ERROR (ERRKEY,13,FILEX,LINEXP)
         ENDIF
         IF (SDEPTH .LE. 0.0 .OR. SDEPTH .GT. 100.0)
     &       THEN
            CALL ERROR (ERRKEY,14,FILEX,LINEXP)
         ENDIF
         IF ((INDEX('PT',CROP)) .GT. 0) THEN
           IF (SPRLAP .LE. 0.0) THEN
              CALL ERROR (ERRKEY,16,FILEX,LINEXP)
           ENDIF
           IF (SDWTPL .LE. 0.0) THEN
              CALL ERROR (ERRKEY,17,FILEX,LINEXP)
           ENDIF
         ENDIF

         IF (INDEX('TSPNRCB',PLME) .LE. 0) 
     &                    CALL ERROR (ERRKEY,19,FILEX,LINEXP)
         IF (INDEX('RI',CROP) .LE. 0) THEN    !CHP ADDED
           IF ((INDEX('T',PLME)) .GT. 0) THEN
             IF (SDWTPL .LT. 0.0) THEN
               CALL ERROR (ERRKEY,18,FILEX,LINEXP)
             ENDIF
           ENDIF
         ENDIF
      ENDIF

      REWIND (LUNEXP)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,I5,1X,I5,2(1X,F5.0),2(5X,A1),8(1X,F5.0),I6,F6.0,2I6)

      END

C=======================================================================
C  IPFLD, Subroutine
C
C  Reads parameters related to field operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       J.W.J       4-21-91
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNFLD
C
C  LOCAL  : LN
C
C  OUTPUT : FLDNAM,WSTA,SLNO,SLOPE,DFDRN,FLDD,SFDRN,SLTX,FLST,FILEW,FLOB
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPFLD (LUNEXP,FILEX,LNFLD,FLDNAM,WSTA,WSTA1,SLNO,
     &           SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP,
     &           XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS)

      IMPLICIT NONE

      CHARACTER*1  UPCASE
      CHARACTER*4  WSTA,WSTA1,HFNDCH
      CHARACTER*5  DFDRN,FLST,SLTX
      CHARACTER*6  ERRKEY,FINDCH
      CHARACTER*8  FLDNAM
      CHARACTER*10 SLNO
      CHARACTER*12 FILEX
      CHARACTER*80 CHARTEST

      INTEGER LUNEXP,LNFLD,LN,LINEXP,ISECT,IFIND,ERRNUM,I

      REAL    FLDD,SFDRN,FLOB,SLDP,SLOPE
      REAL    XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS

      PARAMETER (ERRKEY='IPFLD ')
                 FINDCH='*FIELD'
      LINEXP = 0

      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50   CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
      IF (ISECT .EQ. 1) THEN
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,FLDNAM,WSTA,WSTA1,SLOPE,
     &                     FLOB,DFDRN,FLDD,SFDRN,FLST,SLTX,SLDP,SLNO
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
       ELSE
         CALL ERROR (ERRKEY,2,FILEX,LINEXP)
      ENDIF
      IF (LN .NE. LNFLD) GO TO 50
      DO I = 1, 4
        WSTA(I:I)  = UPCASE(WSTA(I:I))
        WSTA1(I:I) = UPCASE(WSTA1(I:I))
      END DO
      IF (WSTA(1:3) .EQ. '-99' .AND. SLNO(1:3) .EQ. '-99') THEN
        CLOSE(LUNEXP)
        STOP
      ENDIF

      IF (WSTA .EQ. '    ') THEN
         CALL ERROR (ERRKEY,10,FILEX,LINEXP)
      ENDIF
      IF (SLNO .EQ. '          ') THEN
         CALL ERROR(ERRKEY,11,FILEX,LINEXP)
      ENDIF
      IF (SLOPE .LT. 0.0) THEN
         SLOPE = 0.0
      ENDIF
      IF (SFDRN .LE. 0.0) THEN
        SFDRN = 100.
      ENDIF

C
C    New section
C
C    Find header and read second line of field information
C
      HFNDCH='SLAS'
      CALL HFIND(LUNEXP,HFNDCH,LINEXP,IFIND)
      IF (IFIND .EQ. 1) THEN
 70     CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
        IF (ISECT .EQ. 1) THEN
           READ (CHARTEST,80,IOSTAT=ERRNUM) LN,
     &                     XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS
           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         ELSE
           CALL ERROR (ERRKEY,2,FILEX,LINEXP)
         ENDIF
         IF (LN .NE. LNFLD) GO TO 70
      ENDIF
      IF (AREA .LE. 0.0) AREA = 1.0
      IF (FLWR .LE. 0.0) FLWR = 1.0
      IF (SLEN .LE. 0.0) SLEN = SQRT(AREA*FLWR*10000.0)

C
C    End New section

      REWIND(LUNEXP)

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

 60   FORMAT (I3,A8,1X,2A4,1X,F5.0,1X,F5.0,1X,A5,2(1X,F5.0),
     &         2(1X,A5),1X,F5.0,1X,A10)
 80   FORMAT (I3,2(F15.0,1X),F9.0,1X,F17.0,3(1X,F5.0))

      END

C=======================================================================
C  IPSIM, Subroutine
C
C  Reads parameters related to field operation from FILEX file
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       J.W.J       4-21-91
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       5-28-93
C  11/19/2003 CHP Added check for MEPHO and incompatible models.
C-----------------------------------------------------------------------
C  INPUT  : LUNEXP,FILEX,LNSIM
C
C  LOCAL  : LN
C
C  OUTPUT : NYRS,NREPSQ,ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWPOT,ISWDIS,MEWTH,
C           MESIC,MELI,MEEVP,MEINF,MEPHO,ISIMI,ISIM,IPLTI,IIRRI,IFERI,
C           IRESI,IHARI,IOX,IDETO,IDETS,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
C           PWDINF,PWDINL,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN,DSOILX,THETACX,
C           IEPTX,IOFFX,IAMEX,DSOILN,SOILNC,SOILNX,NEND,RIP,NRESDL,
C           DRESMG,HDLAY,HLATE
C-----------------------------------------------------------------------
C  Called : IPEXP
C
C  Calls  : ERROR IGNORE FIND YR_DOY
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE IPSIM (LUNEXP,LNSIM,TITSIM,NYRS,RUN,NREPSQ,
     & ISIMI,PWDINF,PWDINL,SWPLTL,NCODE,SWPLTH,SWPLTD,YEAR,
     & PTX,PTTN,DSOIL,THETAC,IEPT,IOFF,IAME,DSOILN,SOILNC,YRSIM,
     & SOILNX,NEND,RIP,NRESDL,DRESMG,HDLAY,HLATE,HPP,HRP,FTYPEN,
     & RSEED1,LINEXP,AIRAMT,EFFIRR,CROP,FROP,CRMODEL)

      IMPLICIT NONE

      INCLUDE 'COMSWI.BLK'

      CHARACTER*1   UPCASE,ISIMI
      CHARACTER*2   CROP
      CHARACTER*5   NEND,NCODE,IOFF,IAME,CRMODEL
      CHARACTER*6   ERRKEY,FINDCH
      CHARACTER*25  TITSIM
      CHARACTER*78  MSG(2)
      CHARACTER*128 CHARTEST

      INTEGER LNSIM,LUNEXP,ISECT,LINEXP,ISIM,NYRS,NREPSQ,FROP
      INTEGER PWDINF,PWDINL,HLATE,HDLAY,NRESDL
      INTEGER IFIND,LN,ERRNUM,FTYPEN,YRSIM,YEAR,RUN,RSEED1,RRSEED1,I

      REAL DSOIL,THETAC,DSOILN,SOILNC,SOILNX,SWPLTL,SWPLTH,SWPLTD
      REAL PTX,PTTN,DRESMG,RIP,IEPT,HPP,HRP,AIRAMT,EFFIRR

      PARAMETER (ERRKEY='IPSIM ')
                 FINDCH='*SIMUL'

      IF (LNSIM .EQ. 0) THEN
         LNSIM   = 0
         NYRS    = 1
         NREPSQ  = 1
         ISIMI   = 'S'
         YRSIM   = -99
         RSEED1  = 2150
         ISWWAT  = 'N'
         ISWNIT  = 'N'
         NSWITCH = 0
         ISWSYM  = 'N'
         ISWPHO  = 'N'
         ISWPOT  = 'N'
         ISWDIS  = 'N'
         ISWCHE  = 'N'
         ISWTIL  = 'N'
         MEWTH   = 'M'
         MESIC   = 'M'
         MELI    = 'E'
         MEEVP   = 'R'
         MEINF   = 'S'
         MEPHO   = 'C'
         MEHYD   = 'R'
         NSWITCH =  0
         MESOM   = 'G'
         IPLTI   = 'R'
         IIRRI   = 'N'
         IFERI   = 'N'
         IRESI   = 'N'
         IHARI   = 'M'
         IOX     = 'N'
         FROP    =  3
         IDETO   = 'Y'
         IDETS   = 'Y'
         IDETG   = 'Y'
         IDETN   = 'N'
         IDETC   = 'N'
         IDETW   = 'N'
         IDETP   = 'N'
         IDETD   = 'N'
         IDETL   = 'N'
         IDETH   = 'N'
         IDETR   = 'R'
         EFFIRR  = 1.00
         THETAC  = 75.0
         IEPT    = 100.0
         DSOIL   = 30.0
         DSOILN  = 30.0
         AIRAMT  = 10.0
         IOFF    = 'GS000'
         IAME    = 'IR001'
	   CRMODEL = '     '
       ELSE
 40      CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
         IF (IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
 50      CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (LN .NE. LNSIM) GO TO 50
            READ (CHARTEST,55,IOSTAT=ERRNUM) LN,NYRS,NREPSQ,ISIMI,
     &            YRSIM,RRSEED1,TITSIM,CRMODEL
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (INDEX('G',RNMODE) .GT. 0) NYRS = 1
	      IF ((RNMODE .NE. 'Q') .OR. (RNMODE .EQ. 'Q'
     &       .AND. RUN .EQ. 1)) THEN
	         RSEED1 = RRSEED1
               IF (RSEED1 .LE. 0) THEN
                 RSEED1 = 2150
	         ENDIF
            ENDIF
            CALL Y2K_DOY (YRSIM)
            CALL YR_DOY (YRSIM,YEAR,ISIM)
          ELSE
            BACKSPACE (LUNEXP)
            GO TO 40
         ENDIF
C
C        Read SECOND line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,ISWWAT,ISWNIT,ISWSYM,
     &        ISWPHO,ISWPOT,ISWDIS,ISWCHE,ISWTIL
         IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
         ISWWAT = UPCASE(ISWWAT)
         ISWNIT = UPCASE(ISWNIT)
         ISWSYM = UPCASE(ISWSYM)
         ISWPHO = UPCASE(ISWPHO)
         ISWPOT = UPCASE(ISWPOT)
         ISWDIS = UPCASE(ISWDIS)
	   ISWCHE = UPCASE(ISWCHE)
	   ISWTIL = UPCASE(ISWTIL)
         IF (INDEX ('BNSBPNPECHPPVBCPCBFB',CROP) .EQ. 0) THEN
            ISWSYM = 'N'
         ENDIF
         IF (ISWCHE .EQ. ' ') THEN
            ISWCHE = 'N'
         ENDIF
         IF (ISWTIL .EQ. ' ') THEN
            ISWTIL = 'N'
         ENDIF
         IF (ISWWAT .EQ. 'N') THEN
            IF (ISWNIT .EQ. 'Y') ISWNIT = 'N'
            IF (ISWTIL .EQ. 'Y') ISWTIL = 'N'
            IF (ISWCHE .EQ. 'Y') ISWCHE = 'N'
         ENDIF
C
C        Read THIRD line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,61,IOSTAT=ERRNUM) LN,MEWTH,MESIC,
     &        MELI,MEEVP,MEINF,MEPHO,MEHYD,NSWITCH,MESOM
         IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
         MEWTH = UPCASE(MEWTH)
         MESIC = UPCASE(MESIC)
         MELI  = UPCASE(MELI)
         MEEVP = UPCASE(MEEVP)
         MEINF = UPCASE(MEINF)
         MEPHO = UPCASE(MEPHO)
!         IF (INDEX('OPG',MESOM) .EQ. 0) THEN
         IF (INDEX('PG',MESOM) .EQ. 0) THEN
            MESOM = 'G'
         ENDIF
         MESOM = UPCASE(MESOM)
         IF (MEEVP .EQ. 'Z' .AND. MEPHO .NE. 'L')
     &     CALL ERROR(ERRKEY,3,' ',0)
         IF (MEHYD .EQ. ' ') THEN
            MEHYD = 'R'
         ENDIF
         MEHYD = UPCASE(MEHYD)

      IF (NSWITCH .LE. 0 .AND. ISWNIT .EQ. 'Y') THEN
        NSWITCH = 1
      ENDIF
C
C        Read FOURTH line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         READ (CHARTEST,60,IOSTAT=ERRNUM) LN,IPLTI,IIRRI,
     &        IFERI,IRESI,IHARI
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
         IPLTI = UPCASE(IPLTI)
         IIRRI = UPCASE(IIRRI)
         IFERI = UPCASE(IFERI)
         IRESI = UPCASE(IRESI)
         IHARI = UPCASE(IHARI)

      IF ((INDEX('CSPT',CROP)) .GT. 0) THEN
        IF (IHARI .EQ. 'A') THEN
           CALL ERROR (ERRKEY,4,FILEX,LINEXP)
        ENDIF
      ENDIF
      IF ((INDEX('PT',CROP)) .GT. 0) THEN
        IF (IPLTI .EQ. 'A') THEN
           CALL ERROR (ERRKEY,5,FILEX,LINEXP)
        ENDIF
      ENDIF

C
C        Read FIFTH line of simulation control
C
         CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (RUN .EQ. 1) THEN
            READ (CHARTEST,65,IOSTAT=ERRNUM) LN,IOX,IDETO,
     &      IDETS,FROP,IDETG,IDETC,IDETW,IDETN,IDETP,IDETD,
     &      IDETL,IDETH,IDETR
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IOX   = UPCASE(IOX)
            IDETO = UPCASE(IDETO)
            IDETS = UPCASE(IDETS)
            IDETG = UPCASE(IDETG)
            IDETC = UPCASE(IDETC)
            IDETW = UPCASE(IDETW)
            IDETN = UPCASE(IDETN)
            IDETP = UPCASE(IDETP)
            IDETD = UPCASE(IDETD)
            IF (IDETL .EQ. ' ') THEN
               IDETL = 'N'
            ENDIF
            IDETL = UPCASE(IDETL)
            IF (IDETH .EQ. ' ') THEN
               IDETH = 'N'
            ENDIF
            IDETH = UPCASE(IDETH)
            IF (IDETR .EQ. ' ') THEN
               IDETR = 'N'
            ENDIF
            IDETR = UPCASE(IDETR)
            IF (FROP .LE. 0) FROP = 10
         ENDIF
C
C        Read SIXTH line of simulation control
C
         CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
         IF (ISECT .EQ. 1) THEN
            READ (CHARTEST,66,IOSTAT=ERRNUM) LN,PWDINF,PWDINL,
     &           SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            IF (PWDINF .LT. 1000) PWDINF = YEAR * 1000 + PWDINF
            IF (PWDINL .LT. 1000) PWDINL = YEAR * 1000 + PWDINL
	      CALL Y2K_DOY (PWDINF)
	      CALL Y2K_DOY (PWDINL)
C
C           Read SEVENTH line of simulation control
C
            CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,67,IOSTAT=ERRNUM) LN,DSOIL,THETAC,
     &           IEPT,IOFF,IAME,AIRAMT,EFFIRR
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
C
C           Read EIGHTH line of simulation control
C
            CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,67,IOSTAT=ERRNUM) LN,DSOILN,SOILNC,
     &           SOILNX,NCODE,NEND
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
            READ (NCODE,70,IOSTAT=ERRNUM) FTYPEN
C
C           Read NINTH line of simulation control
C
            CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,68,IOSTAT=ERRNUM) LN,RIP,NRESDL,DRESMG
            IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEX,LINEXP)
C
C           Read TENTH line of simulation control
C
            CALL IGNORE(LUNEXP,LINEXP,ISECT,CHARTEST)
            READ (CHARTEST,66,IOSTAT=ERRNUM) LN,HDLAY,HLATE,
     &           HPP,HRP
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
	      CALL Y2K_DOY (HLATE)
            IF (HPP   .LT. 0.0)  HPP   = 100.
            IF (HRP   .LT. 0.0)  HRP   = 0.0
          ELSE
            PWDINF  =   1
            PWDINL  =   366
            SWPLTL  =   1.0
            SWPLTH  =   100.0
            SWPLTD  =   200.0
            PTX     =   50.0
            PTTN    =   1.0
            DSOIL   =   200.0
            THETAC  =   10.0
            IEPT    =   100.0
            IOFF    =   ' '
            HPP     =   100.0
            HRP     =     0.0
         ENDIF
 120     CONTINUE
      ENDIF
      DO I=1,5
        CRMODEL(I:I)= UPCASE(CRMODEL(I:I))
	ENDDO

	IF (CRMODEL .EQ. '     '    .OR. CRMODEL(3:5) .NE. 'CER' .OR. 
     &    CRMODEL(3:5) .NE. 'GRO' .OR. CRMODEL(3:5) .NE. 'SIM' .OR.
     &    CRMODEL(3:5) .NE. 'CSM' .OR. CRMODEL(3:5) .NE. 'SUB' .OR.
     &    CRMODEL(3:5) .NE. 'CAN' .OR. CRMODEL(3:5) .NE. 'OIL' .OR.
     &    CRMODEL(3:5) .NE. 'ALO') THEN
         IF (INDEX ('SBPNBNCHPPPEVBTMPRCBFACPFB',CROP) .GT. 0) THEN
           CRMODEL = 'CRGRO'
         ELSEIF (INDEX ('C3C4G0G1G2G3G4G5G6G7BR',CROP) .GT. 0) THEN
           CRMODEL = 'CRGRO'
         ELSEIF (INDEX ('COCT',CROP) .GT. 0) THEN
           CRMODEL = 'CRGRO'       
!         ELSEIF (INDEX ('BASGML',CROP) .GT. 0) THEN
!           CRMODEL = 'GECER'
         ELSEIF (INDEX ('SG',CROP) .GT. 0) THEN
           CRMODEL = 'SGCER'
         ELSEIF (INDEX ('ML',CROP) .GT. 0) THEN
           CRMODEL = 'MLCER'
         ELSEIF (INDEX ('MZ',CROP) .GT. 0) THEN
           CRMODEL = 'MZCER'
         ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
           CRMODEL = 'RICER'
         ELSEIF (INDEX ('CS',CROP) .GT. 0) THEN
           CRMODEL = 'CSSIM'
c	   ELSEIF (INDEX ('WH',CROP) .GT. 0) THEN
c           CRMODEL = 'WHCSM'
	   ELSEIF (INDEX ('BAWH',CROP) .GT. 0) THEN
           CRMODEL = 'WHCER'
         ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
           CRMODEL = 'PTSUB'
         ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
           CRMODEL = 'SCCAN'
!           CRMODEL = 'SCCSP'
         ELSEIF (INDEX ('SU',CROP) .GT. 0) THEN
           CRMODEL = 'SUOIL'
         ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
           CRMODEL = 'PIALO'
         ELSEIF (INDEX ('TNTA',CROP) .GT. 0) THEN
           CRMODEL = 'ARSUB'
	   ELSE
	     WRITE (*,260)
 260       FORMAT( ' No models have currently been defined for this',
     &             ' crop. !',/,
     &            ' Please contact the CSM Model developers for'
     &            ' additional information.')
	     STOP
         ENDIF
      ENDIF   

      IF (MEPHO .EQ. 'L' .AND. CRMODEL .NE. 'CRGRO') THEN
        MEPHO = 'C'
        WRITE(MSG(1),80)
        WRITE (MSG(2),81) CRMODEL
        CALL WARNING(2, "IPEXP ", MSG)

   80 FORMAT('Photosynthesis method (PHOTO in FILEX) has been changed')
   81 FORMAT('from "L" to "C" for compatibility with crop model, '
     &            ,A5,'.') 

      ENDIF

      REWIND (LUNEXP)
      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  55  FORMAT (I3,11X,2(1X,I5),5X,A1,1X,I5,1X,I5,1X,A25,1X,A5)
  60  FORMAT (I3,11X,8(5X,A1))
  61  FORMAT (I3,11X,7(5X,A1),5X,I1,5X,A1)
  65  FORMAT (I3,11X,3(5X,A1),4X,I2,9(5X,A1))
  66  FORMAT (I3,11X,2(1X,I5),5(1X,F5.0))
  67  FORMAT (I3,11X,3(1X,F5.0),2(1X,A5),1X,F5.0,1X,F5.0)
  68  FORMAT (I3,11X,1X,F5.0,1X,I5,1X,F5.0)
  70  FORMAT (3X,I2)

      END
