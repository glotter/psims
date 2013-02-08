C=======================================================================
C  OPTEMPY2K, Subroutine
C
C  Determines experiment and treatment selection
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1990 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  06/07/2002 GH  Modifed for Y2K Output
!  06/15/2005 CHP Modified output for sequenced runs (no soils output, 
!                 except for 1st run of sequence).
C-----------------------------------------------------------------------
C  INPUT  : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
C           TOTN,NYRS,VARNO,VRNAME,CROP,MODEL,PATHMO,ECONO,FROP,RUN,FILEIO
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : INPUT
C
C  Calls  : ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPTEMPY2K (
     & YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
     & NYRS,VARNO,VRNAME,CROP,MODEL,RUN,FILEIO,EXPN,ECONO,FROP,TRTALL,
     & TRTN,CHEXTR,NFORC,PLTFOR,NDOF,PMTYPE,ISENS)

      USE ModuleDefs
      IMPLICIT NONE

      INCLUDE 'COMIBS.BLK'
      INCLUDE 'COMSOI.BLK'
      INCLUDE 'COMSWI.BLK'
      INCLUDE 'COMGEN.BLK'

      CHARACTER*2  CROP,PRCROP
      CHARACTER*6  VARNO,ECONO
      CHARACTER*9  ERRKEY
      CHARACTER*12 MODEL
      CHARACTER*16 VRNAME
      CHARACTER*30 FILEIO
      CHARACTER*42 CHEXTR(NAPPL)

      INTEGER NYRS,RUN,I,L,EXPN,LUNIO,LINIO,ERRNUM,FROP,YRIC,TRTALL
      INTEGER TRTN,NFORC,NDOF,PMTYPE,ISENS

      REAL    PLTFOR
      REAL    SWINIT(NL),WRESR,WRESND,EFINOC,EFNFIX,INO3(NL),INH4(NL)

      PARAMETER (LUNIO = 21)
      PARAMETER (ERRKEY = 'OPTEMPY2K')
      LINIO  = 0

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,0)
C-----------------------------------------------------------------------
C     Write temp. required variables on top of file
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2000) RNMODE,RUN,EXPN,TRTN,TRTALL,ISENS
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*FILES              '
      LINIO = LINIO + 1
      WRITE (LUNIO,2040,IOSTAT=ERRNUM) MODEL
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2050,IOSTAT=ERRNUM) FILEX
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2100,IOSTAT=ERRNUM) FILEA
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2200,IOSTAT=ERRNUM) FILET
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2300,IOSTAT=ERRNUM) FILEC,PATHCR
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2400,IOSTAT=ERRNUM) FILEE,PATHEC
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2500,IOSTAT=ERRNUM) FILEG,PATHGE
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2600,IOSTAT=ERRNUM) FILEP,PATHPE
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2700,IOSTAT=ERRNUM) FILES,PATHSL
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2800,IOSTAT=ERRNUM) FILEW,PATHWT
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,2900,IOSTAT=ERRNUM) OUTO(1:8)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*SIMULATION CONTROL '
      LINIO = LINIO + 1
      WRITE (LUNIO,900,IOSTAT=ERRNUM) NYRS,NREPSQ,ISIMI,YRSIM,RSEED1,
     &       TITSIM,MODEL(1:5)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,910,IOSTAT=ERRNUM) ISWWAT,ISWNIT,ISWSYM,ISWPHO,
     &       ISWPOT,ISWDIS,ISWCHE,ISWTIL
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,915,IOSTAT=ERRNUM) MEWTH,MESIC,MELI,MEEVP,
     & MEINF,MEPHO,MEHYD,NSWITCH,MESOM
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,910,IOSTAT=ERRNUM) IPLTI,IIRRI,IFERI,IRESI,IHARI
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,920,IOSTAT=ERRNUM) IOX,IDETO,IDETS,FROP,IDETG,IDETC,
     &       IDETW,IDETN,IDETP,IDETD,IDETL,IDETH,IDETR
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'!AUTOMATIC MANAGEM  '
      LINIO = LINIO + 1
      WRITE (LUNIO,930,IOSTAT=ERRNUM) PWDINF,PWDINL,SWPLTL,SWPLTH,
     &       SWPLTD,PTX,PTTN
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,940,IOSTAT=ERRNUM) DSOIL,THETAC,IEPT,IOFF,IAME,
     &       AIRAMT,EFFIRR
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,940,IOSTAT=ERRNUM) DSOILN,SOILNC,SOILNX,NCODE,NEND
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,950,IOSTAT=ERRNUM) RIP,NRESDL,DRESMG
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,930,IOSTAT=ERRNUM) HDLAY,HLATE,HPP,HRP
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*EXP.DETAILS        '
      LINIO = LINIO + 1
      WRITE (LUNIO,50,IOSTAT=ERRNUM)EXPN,EXPER,CG,ENAME
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*TREATMENTS         '
      LINIO = LINIO + 1
      WRITE (LUNIO,55,IOSTAT=ERRNUM) TRTNO,ROTNO,ROTOPT,CRPNO,TITLER
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*CULTIVARS          '
      LINIO = LINIO + 1
      WRITE (LUNIO,56,IOSTAT=ERRNUM) CROP,VARNO,VRNAME
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*FIELDS             '
      LINIO = LINIO + 1
      WRITE (LUNIO,59,IOSTAT=ERRNUM) FLDNAM,FILEW(1:8),SLOPE,FLOB,DFDRN,
     &       FLDD,SFDRN,FLST,SLTX,SLDP,SLNO
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
      WRITE (LUNIO,60,IOSTAT=ERRNUM) XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*INITIAL CONDITIONS '
      LINIO = LINIO + 1
      EFINOC = MAX(EFINOC,-9.0)
      EFNFIX = MAX(EFNFIX,-9.0)
      ICREN  = MAX(ICREN,-9.0)
      ICREP  = MAX(ICREP,-9.0)
      WRITE (LUNIO,61,IOSTAT=ERRNUM) PRCROP,YRIC,WRESR,WRESND,EFINOC,
     &       EFNFIX,ICWD,INT(ICRES),ICREN,ICREP,ICRIP,ICRID
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      DO I = 1, NLAYR
         LINIO = LINIO + 1
         WRITE (LUNIO,62,IOSTAT=ERRNUM) DS(I),SWINIT(I),INH4(I),INO3(I)
         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
      END DO
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*PLANTING DETAILS   '
      LINIO = LINIO + 1
      IF ((INDEX('PI',CROP)) .GT. 0) THEN
         WRITE (LUNIO,70,IOSTAT=ERRNUM) YRPLT,IEMRG,PLANTS,PLTPOP,PLME,
     &          PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,PLPH,SPRLAP,
     &          NFORC,PLTFOR,NDOF,PMTYPE
      ELSE
         WRITE (LUNIO,70,IOSTAT=ERRNUM) YRPLT,IEMRG,PLANTS,PLTPOP,PLME,
     &          PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,SDAGE,ATEMP,PLPH,SPRLAP
      ENDIF
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*IRRIGATION         '
      LINIO = LINIO + 1
      WRITE (LUNIO,75,IOSTAT=ERRNUM) EFFIRX,DSOILX,THETCX,IEPTX,IOFFX,
     &       IAMEX,AIRAMX
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (NIRR .GT. 0) THEN
         DO I = 1, NIRR
            LINIO = LINIO + 1
            WRITE (LUNIO,76,IOSTAT=ERRNUM) IDLAPL(I),IRRCOD(I),AMT(I)!,
     &             !IIRV(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*FERTILIZERS        '
      IF (NFERT .GT. 0) THEN
         DO I = 1, NFERT
            LINIO = LINIO + 1
            WRITE (LUNIO,77,IOSTAT=ERRNUM)FDAY(I),IFTYPE(I),FERCOD(I),
     &          DFERT(I),ANFER(I),APFER(I),AKFER(I),ACFER(I),AOFER(I),
     &          FOCOD(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*RESIDUES           '
      IF (NARES .GT. 0) THEN
         DO I = 1,NARES
            LINIO = LINIO + 1
            WRITE (LUNIO,79,IOSTAT=ERRNUM)RESDAY(I),RESCOD(I),
     &         INT(RESIDUE(I)),RESN(I),RESP(I),RESK(I),RINP(I),
     &         DEPRES(I),RMET(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF
C-----------------------------------------------------------------------
C     Chemicals ....
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*CHEMICALS          '
      IF (NCHEM .GT. 0) THEN
         DO I = 1,NCHEM
            LINIO = LINIO + 1
            WRITE (LUNIO,78,IOSTAT=ERRNUM) CDATE(I),CHCOD(I),
     &         CHAMT(I),CHMET(I),CHDEP(I),CHT(I),CHEXTR(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF

C-----------------------------------------------------------------------
C    Tillage  ....
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*TILLAGE            '
      IF (NTIL .GT. 0) THEN
         DO I = 1,NTIL
            LINIO = LINIO + 1
            WRITE (LUNIO,80,IOSTAT=ERRNUM)TDATE(I),TIMPL(I),TDEP(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*ENVIRONMENT        '
      IF (NEV .GT. 0) THEN
         DO I = 1,NEV
            LINIO = LINIO + 1
            IF (DAYFAC(I) .EQ. 'M' .AND. DAYADJ(I) .LE. 10.0) THEN
            WRITE (LUNIO,91,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ELSE IF (RADFAC(I) .EQ. 'M' .AND. RADADJ(I) .LE. 10.0) THEN
            WRITE (LUNIO,92,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ELSE IF (PRCFAC(I) .EQ. 'M' .AND. PRCADJ(I) .LE. 10.0) THEN
            WRITE (LUNIO,93,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ELSE IF ((TXFAC(I) .EQ. 'R' .AND. TXADJ(I) .LE. -10.0) .OR.
     &              (TMFAC(I) .EQ. 'R' .AND. TMADJ(I) .LE. -10.0)) THEN
            WRITE (LUNIO,94,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ELSE
            WRITE (LUNIO,90,IOSTAT=ERRNUM)
     &            WMDATE(I),DAYFAC(I),DAYADJ(I),RADFAC(I),
     &            RADADJ(I),TXFAC(I),TXADJ(I),TMFAC(I),TMADJ(I),
     &            PRCFAC(I),PRCADJ(I),CO2FAC(I),INT(CO2ADJ(I)),
     &            DPTFAC(I),DPTADJ(I),WNDFAC(I),WNDADJ(I)
            ENDIF
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      LINIO = LINIO + 1
      WRITE (LUNIO,40)'*HARVEST            '
      IF (NHAR .GT. 0) THEN
         DO I = 1,NHAR
            LINIO = LINIO + 1
            WRITE (LUNIO,100,IOSTAT=ERRNUM)HDATE(I),HSTG(I),HCOM(I),
     &             HSIZ(I),HPC(I),HBPC(I)
            IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
         END DO
      ENDIF   !End of non-sequence soils write
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
!     SOIL DATA
!     Do not need soil input for sequenced runs (except first year)
      IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN
        LINIO = LINIO + 1
        WRITE (LUNIO,40)'*SOIL               '
        LINIO = LINIO + 1
        WRITE (LUNIO,960,IOSTAT=ERRNUM) SLNO,SLSOUR,SLTX,SLDP,SLDESC
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        LINIO = LINIO + 1
        WRITE (LUNIO,970,IOSTAT=ERRNUM) SSITE,SCOUNT,SLAT,SLONG,TAXON
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        WRITE (LUNIO,980,IOSTAT=ERRNUM) SCOM,SALB,U,SWCON,CN2,SLNF,SLPF,
     &         SMHB,SMPX,SMKE,SGRP
        LINIO = LINIO + 1
        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        DO I = 1, NLAYR
          LINIO = LINIO + 1
          IF (TOTN(I) .LT. -9.0) THEN
            TOTN(I) = -9.0
          ENDIF
          IF (BD(I) .LT. -9.0) THEN
            BD(I) = -9.0
          ENDIF
          IF (OC(I) .LT. -9.0) THEN
            OC(I) = -9.0
          ENDIF
          IF (SWCN(I) .GT. 0.0 .AND. SWCN(I) .LT. 10.0 ) THEN
            WRITE (LUNIO,989,IOSTAT=ERRNUM) DS(I),
     &             LL(I),DUL(I),SAT(I),SHF(I),SWCN(I),BD(I),
     &             OC(I),CLAY(I),SILT(I),STONES(I),TOTN(I),
     &             PH(I),PHKCL(I),CEC(I),ADCOEF(I)
          ELSE
            WRITE (LUNIO,990,IOSTAT=ERRNUM) DS(I),
     &             LL(I),DUL(I),SAT(I),SHF(I),SWCN(I),BD(I),
     &             OC(I),CLAY(I),SILT(I),STONES(I),TOTN(I),
     &             PH(I),PHKCL(I),CEC(I),ADCOEF(I)
          ENDIF
          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
        END DO
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
        LINIO = LINIO + 1
        WRITE (LUNIO,40)'                    '
        DO I = 1, NLAYR
           LINIO = LINIO + 1
!       05/27/2004 CHP added all 2nd tier data.  We may need to revise
!       output format as we start using these data.
              WRITE (LUNIO,991,IOSTAT=ERRNUM) 
     &          DS(I), EXTP(I), TOTP(I), ORGP(I), CACO(I), EXTAL(I), 
     &          EXTFE(I), EXTMN(I), TOTBAS(I), PTERMA(I), PTERMB(I), 
     &          EXK(I), EXMG(I), EXNA(I), EXTS(I), SLEC(I)
           IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
        END DO
        LINIO = LINIO + 1
      ENDIF   !End of non-sequence soils write
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
         WRITE (LUNIO,40)'*CULTIVAR           '
         LINIO = LINIO + 1
         IF (INDEX('PNSBBNTMPECHPPPRC3C4G0G1G2G3',CROP) .GT. 0) THEN
            WRITE (LUNIO,1500,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,CSDVAR,
     &           PPSEN,PH2T5,PHTHRS(6),PHTHRS(8),PHTHRS(10),PHTHRS(13),
     &           LFMAX,SLAVAR,SIZELF,XFRUIT,WTPSD,SFDUR,SDPDVR,PODUR
          ELSEIF (INDEX('G4G5G6G7G8BRVBCPCBFBCOCT',CROP) .GT. 0) THEN
            WRITE (LUNIO,1500,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,CSDVAR,
     &           PPSEN,PH2T5,PHTHRS(6),PHTHRS(8),PHTHRS(10),PHTHRS(13),
     &           LFMAX,SLAVAR,SIZELF,XFRUIT,WTPSD,SFDUR,SDPDVR,PODUR
          ELSEIF (INDEX('CS',CROP) .GT. 0) THEN
            WRITE (LUNIO,1600,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &            (GCOEFF(L),L=1,15)
          ELSEIF (INDEX ('MZMLSGWHBA',CROP) .GT. 0) THEN
            IF (CROP .EQ. 'MZ') THEN
               WRITE (LUNIO,1800,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               P1,P2,P5,G2,G3,PHINT
             ELSE IF (CROP .EQ. 'WH') THEN
               WRITE (LUNIO,1700,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               P1V,P1D,P5,G1,G2,G3,PHINT
             ELSE IF (CROP .EQ. 'SG') THEN
               WRITE (LUNIO,1900,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               P1,P2O,P2R,P5,G1,G2,PHINT
             ELSE IF (CROP .EQ. 'ML') THEN
               WRITE (LUNIO,1950,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               P1,P2O,P2R,P5,G1,G4,PHINT
             ELSE IF (CROP .EQ. 'BA') THEN
               WRITE (LUNIO,1700,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               P1V,P1D,P5,G1,G2,G3,PHINT
            ENDIF
          ELSEIF (INDEX ('PT',CROP) .GT. 0) THEN
               WRITE (LUNIO,1400,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &               G2,G3,G4,PD,P2,TC
          ELSEIF (INDEX ('RI',CROP) .GT. 0) THEN
            WRITE (LUNIO,1985,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &             P1,P2R,P5,P2O,G1,G2,G3,G4
          ELSEIF (INDEX ('SC',CROP) .GT. 0) THEN
            WRITE (LUNIO,1000,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &             P1,RATPOT,LFMAX,G1,PI1,PI2,DTTPI
          ELSEIF (INDEX ('SU',CROP) .GT. 0) THEN
            WRITE (LUNIO,1960,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &             P1,P2,P5,G2,G3,O1
          ELSEIF (INDEX ('PI',CROP) .GT. 0) THEN
            WRITE (LUNIO,1970,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &            P2,P3,P4,G2,G3,PHINT
          ELSEIF (INDEX ('TRTN',CROP) .GT. 0) THEN
            WRITE (LUNIO,1975,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
     &             P1,P3,P4,P5,G3,G4,PHINT,PCINT,PCGRD
c          ELSEIF (INDEX ('CO',CROP) .GT. 0) THEN
c            WRITE (LUNIO,1995,IOSTAT=ERRNUM) VARNO,VRNAME,ECONO,
c     &             SCPB,RESPC,SQCON,FCUT,FLAI,DDISQ
         ENDIF
      ENDIF

      CLOSE (LUNIO)
      RETURN

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

   40 FORMAT (A20)
   50 FORMAT (I3,A8,1X,A2,1X,A60)
   55 FORMAT (I3,I2,2(1X,I1),1X,A25)
   56 FORMAT (3X,A2,1X,A6,1X,A16)
   59 FORMAT (3X,A8,1X,A8,1X,F5.1,1X,F5.0,1X,A5,2(1X,F5.0),
     &        2(1X,A5),1X,F5.0,1X,A10)
   60 FORMAT (3X,2(F15.5,1X),F9.2,1X,F17.1,1X,F5.0,2(1X,F5.1))
C  61 FORMAT (3X,A2,4X,I5,2(1X,F5.0),2(1X,F5.2),1X,F5.1,1X,F5.0,
C    &        2(1X,F5.2),2(1X,F5.0))
C-Y2K 61 FORMAT (3X,A2,4X,I5,2(1X,F5.0),2(1X,F5.2),1X,F5.1,1X,I5,
C-Y2K &        2(1X,F5.2),2(1X,F5.0))
   61 FORMAT (3X,A2,4X,I7,2(1X,F5.0),2(1X,F5.2),1X,F5.1,1X,I5,
     &        2(1X,F5.2),2(1X,F5.0))
   62 FORMAT (3X,F5.0,1X,F5.3,2(1X,F5.1))
C-Y2K 70 FORMAT (3X,I5,1X,I5,2(1X,F5.1),2(5X,A1),2(1X,F5.0),1X,F5.1,
C-Y2K &        2(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6)
   70 FORMAT (3X,I7,1X,I7,2(1X,F5.1),2(5X,A1),2(1X,F5.0),1X,F5.1,
     &        2(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6)
   75 FORMAT (2X,1X,F5.3,3(1X,F5.0),2(1X,A5),1X,F5.1)
   76 FORMAT (3X,I7,1X,A5,1X,F5.1)
!chp   76 FORMAT (3X,I7,1X,A5,1X,F5.1,4X,I2)
C-Y2K 76 FORMAT (3X,I5,1X,A5,1X,F5.1,4X,I2)
   77 FORMAT (3X,I7,2(1X,A5),6(1X,F5.0),1X,A5)
C-Y2K 77 FORMAT (3X,I5,2(1X,A5),6(1X,F5.0),1X,A5)
   78 FORMAT (3X,I7,1X,A5,1X,F5.2,1X,A5,1X,F5.1,1X,A5,A42)
!  78 FORMAT (3X,I7,1X,A5,1X,F5.2,1X,A5,1X,F5.2,1X,A5,A42)
C-Y2K 78 FORMAT (3X,I5,1X,A5,1X,F5.2,1X,A5,1X,F5.2,1X,A5,A42)
c  78 FORMAT (3X,I5,1X,A5,1X,F5.2,1X,A5,1X,F5.2,1X,A5)
   79 FORMAT (3X,I7,1X,A5,1X,I5,3(1X,F5.2),2(1X,F5.0),1X,A5)
C-Y2K79 FORMAT (3X,I5,1X,A5,1X,I5,3(1X,F5.2),2(1X,F5.0),1X,A5)
   80 FORMAT (3X,I7,1X,A5,1X,F5.1)
C-Y2K 80 FORMAT (3X,I5,1X,A5,1X,F5.1)
   91 FORMAT (3X,I7,1X,A1,F4.2,4(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1))
   92 FORMAT (3X,I7,1X,A1,F4.1,1X,A1,F4.2,3(1X,A1,F4.1),1X,A1,I4,
     &       2(1X,A1,F4.1))
   93 FORMAT (3X,I7,4(1X,A1,F4.1),1X,A1,F4.2,1X,A1,I4,
     &       2(1X,A1,F4.1))
   94 FORMAT (3X,I7,2(1X,A1,F4.1),2(1X,A1,F4.0),1X,A1,F4.1,1X,A1,I4,
     &       2(1X,A1,F4.1))
   90 FORMAT (3X,I7,5(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1))
  100 FORMAT (3X,I7,3(1X,A5),2(1X,F5.0))
C-Y2K 100 FORMAT (3X,I5,3(1X,A5),2(1X,F5.0))
  900 FORMAT (14X,I6,1X,I5,5X,A1,1X,I7,1X,I5,1X,A25,1X,A5)
C-Y2K 900 FORMAT (18X,I2,2X,I4,5X,A1,2(1X,I5),1X,A25,1X,A5)
  910 FORMAT (14X,8(5X,A1))
  915 FORMAT (14X,7(5X,A1),5X,I1,5X,A1)
  920 FORMAT (14X,3(5X,A1),4X,I2,9(5X,A1))
C-Y2K 930 FORMAT (14X,2(1X,I5),5(1X,F5.0))
  930 FORMAT (14X,2(1X,I7),5(1X,F5.0))
  940 FORMAT (14X,3(1X,F5.0),2(1X,A5),1X,F5.1,1X,F5.3)
  950 FORMAT (15X,F5.0,1X,I5,1X,F5.0)
  960 FORMAT (1X,A10,2X,A11,1X,A5,1X,F5.0,1X,A50)
  970 FORMAT (2(1X,A11),2(F8.3),1X,A50)
  980 FORMAT (1X,A5,1X,F5.2,1X,F5.1,1X,F5.2,1X,F5.0,2(1X,F5.2),4(1X,A5))
C-GH  989 FORMAT (1X,F5.0,6X,4(1X,F5.3),1X,F5.3,2(1X,F5.2),3(1X,F5.1),
C     &        1X,F5.2,19(1X,F5.1))
  989 FORMAT (1X,F5.0,6X,4(1X,F5.3),1X,F5.3,2(1X,F5.2),3(1X,F5.1),
     &        F6.3,19F6.2)
C-GH  990 FORMAT (1X,F5.0,6X,4(1X,F5.3),1X,F5.1,2(1X,F5.2),3(1X,F5.1),
C     &        1X,F5.2,19(1X,F5.1))
  990 FORMAT (1X,F5.0,6X,4(1X,F5.3),1X,F5.1,2(1X,F5.2),3(1X,F5.1),
     &        F6.3,19F6.2)
  991 FORMAT (1X,F5.0,18(1X,F5.1))
 1000 FORMAT (A6,1X,A16,1X,A6,1X,7(F6.1))
 1400 FORMAT (A6,1X,A16,1X,A6,1X,F6.0,F6.1,F6.2,3(F6.1))
 1500 FORMAT (A6,1X,A16,1X,A6,F6.2,F6.3,5F6.2,F6.3,2F6.1,F6.2,
     &        F6.3,3F6.2)
 1600 FORMAT (A6,1X,A16,1X,A6,2(F6.1),F6.2,2(F6.1),F6.2,F6.0,
     &        F6.3,F6.2,6(F6.0))
 1700 FORMAT (A6,1X,A16,1X,A6,1X,5(F6.1),F6.2, F6.1)
 1800 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2))
 1900 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.2,3(F6.1),F6.2)
 1950 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.2,2(F6.1),3(F6.2))
c1960 FORMAT (A6,1X,A16,1X,A6,1X,F6.2,F8.4,F7.2,F8.2,F7.3,F4.0)
 1960 FORMAT (A6,1X,A16,1X,A6,1X,F5.1,1X,F5.2,1X,F5.1,1X,F5.0,1X,F5.2,
     &        1X,F5.0)
 1970 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.1,F6.0,F6.1,F6.2,F6.1)
 1975 FORMAT (A6,1X,A16,1X,A6,4(F6.0),2(F6.2),3(F6.1))
 1985 FORMAT (A6,1X,A16,1X,A6,5(F6.1),F6.4,2(F6.2))
 1995 FORMAT (A6,1X,A16,1X,A6,F6.1,3(F6.3),F6.2,F6.1)
 2000 FORMAT ('*MODEL INPUT FILE   ',9X,A1,5(1X,I5))
 2040 FORMAT ('MODEL          ',A12,1X,A80)
 2050 FORMAT ('FILEX          ',A12,1X,A80)
 2100 FORMAT ('FILEA          ',A12,1X,A80)
 2200 FORMAT ('FILET          ',A12,1X,A80)
 2300 FORMAT ('SPECIES        ',A12,1X,A80)
 2400 FORMAT ('ECOTYPE        ',A12,1X,A80)
 2500 FORMAT ('CULTIVAR       ',A12,1X,A80)
 2600 FORMAT ('PESTS          ',A12,1X,A80)
 2700 FORMAT ('SOILS          ',A12,1X,A80)
 2800 FORMAT ('WEATHER        ',A12,1X,A80)
 2900 FORMAT ('OUTPUT         ',A8)

      END SUBROUTINE OPTEMPY2K
      
