!  Generic read routines
!  File contains utilities for generic reading of standard files

!  NOTE: Need to get array sizes with: ASIZE=UBOUND(ARRAYOUT,1)

!    XREADT   - Subroutine to read line of text from X-file
!    XREADC   - Subroutine to read character from X-file
!    XREADR   - Subroutine to read real value from X-file
!    XREADI   - Subroutine to read integer value from X-file
!    XREADCA  - Subroutine to read array of characters from X-file
!    XREADRA  - Subroutine to read array of real values from X-file
!    XREADIA  - Subroutine to read array of integer values from X-file

!    AREADT   - Subroutine to read line of text from agronomy file
!    AREADC   - Subroutine to read character from agronomy file
!    AREADR   - Subroutine to read real value from agronomy file
!    AREADI   - Subroutine to read integer value from agronomy file

!    CUREADT  - Subroutine to read line of text from cultivar file
!    CUREADC  - Subroutine to read character from cultivar file
!    CUREADR  - Subroutine to read real value from cultivar file

!    ECREADT  - Subroutine to read line(s) of text from ecotype file
!    ECREADC  - Subroutine to read character from ecotype file
!    ECREADR  - Subroutine to read real value from ecotype file

!    SPREADT  - Subroutine to read line of text from species file
!    SPREADC  - Subroutine to read character from species file
!    SPREADR  - Subroutine to read real value from species file
!    SPREADRA - Subroutine to read array of real value from species file
!    SPREADIA - Subroutine to read array of integers from species file
!    SPREADCA - Subroutine to read array of characters from species file

!    ARAYREAD - Subroutine to read internal array and return string

!    AMAKEABV - Subroutine to make array of abbreviations with synonyms
!    AMAKE1   - Subroutine to make array when data in columns,1 level
!    AMAKER0  - Subroutine to make array when data in rows and 0 levels
!    AMAKE5   - Subroutine to make array when data in columns,5 levels

!    SLREADT  - Subroutine to read line of text from soil file
!    SLREADC  - Subroutine to read character from soil file
!    SLREADR  - Subroutine to read real value from soil file
!    SLREADCA - Subroutine to read array of characters from soil file
!    SLREADRA - Subroutine to read array of real values from soil file

!    SLLAYERS - Function to calculate the number of layers in a soil
!    SLDEPTH  - Function to calculate the depth of a soil profile

!    SMREADT  - Subroutine to read line of text from SOM file
!    SMREADC  - Subroutine to read character from SOM file
!    SMREADR  - Subroutine to read real value from SOM file
!    SMREADRA - Subroutine to read array of real value from SOM file
!    SMREADR2 - Subroutine to read 2 dim array,real value from SOM file

!    WTHDATA  - Subroutine to find the name and location of weather data

!    TDETAILS - Subroutine to find number of components, options, etc..


!    ECREADRA - Subroutine to read real array from eco file (String<25)

!    Following no longer used but retained for possible later use
!    E1READT  - Subroutine to read line of text from 'soils' type eco file
!    E1READC  - Subroutine to read character from 'soils' type eco file
!    E1READR  - Subroutine to read real value from 'soils' type eco file

!    S1READT  - Subroutine to read line of text from old species file
!    S1READC  - Subroutine to read character from old species file
!    S1READR  - Subroutine to read real value from old species file
!    S1READRA - Subroutine to read array of real value from old species file
!    S1READIA - Subroutine to read array of integers from old species file
!    S1READCA - Subroutine to read array of characters from old species file


!  1. Added SAVE statements and case sensitivity for
!       output file names for LINUX portability   L.A.H.    17-12-04
!-----------------------------------------------------------------------


      SUBROUTINE XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,
     & TXTO)

      IMPLICIT NONE; SAVE

      CHARACTER*(*)   FILENAME, CODE, TXTO
!chp1/30/2006      CHARACTER*600   DATASTD, ABVSTD
      CHARACTER*1000  DATASTD, ABVSTD
      CHARACTER*254   FILENEW, FILEOLD, ABVLINE, DATALINE
      CHARACTER*254   TL2541, TL2542, DATARRAY(500), ABVARRAY(1000)
      CHARACTER*128   ARG
      CHARACTER*100   DATAROC
      CHARACTER*132   ABVFILE
      CHARACTER*80    TL0801
      CHARACTER*64    CTRDIRFL
      CHARACTER*60    ENAME, ERRLINES, ERRLINE
      CHARACTER*52    TEXTSTD
      CHARACTER*30    TL30
      CHARACTER*30    LEVELNEW, LEVELOLD, LEVELTMP, LEVELSTD, LE
      CHARACTER*12    COEFFC, ABVC, COEFFCTM
      CHARACTER*10    EXPER, TL10FROMI, LEVEL, TL10
      CHARACTER*4     COEFFCHK
      CHARACTER*2     CU,FL,SA,IC,MP,MI,MF,MR,MC,MT,ME,MH,SM,AM,FI
      CHARACTER*1     TL1UPCASE, TEXT, TR, CTR, FTYPE, COLON
      CHARACTER*1     CSWOUT
      INTEGER         COL2TMP, COL3TMP, COL4TMP, COL1, COL1R, L
      INTEGER         COL2, COL3, COL4, COLRN, STARNUM, STARTRE
      INTEGER         COL1P, COL1RP, COL2P, COL3P, COL4P, XREADNUM
      INTEGER         FILENUM, TVILENT, TVI1, TVI2, TVI0, TVI3, TVI4
      INTEGER         STARTCOL, LEVELLEN, FILELEN, LINE, LENTMP
      INTEGER         TVINSTR, ABVNUM, DATANUM, LENSTD, ABVLEN
      INTEGER         DATANUMS, RETURNUM, ARGLEN, ABVSYN, FNUMWRK
      LOGICAL         FFLAG, FOPEN

      PARAMETER  (LENSTD = 13)

!      SAVE            FILENEW

      CSWOUT = 'N'

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      CALL LTRIM2(FILENAME,filenew)
      FILELEN=TVILENT(FILENEW)
      FILELEN=MIN(254,FILELEN)
      FILENEW=' '
      FILENEW(1:FILELEN)=FILENAME(1:FILELEN)

      IF (FILENEW(1:FILELEN) .EQ. FILEOLD(1:FILELEN)) THEN
        IF(COL1.EQ.COL1P)THEN
        IF(COL1R.EQ.COL1RP)THEN
          IF(COL2.EQ.COL2P)THEN
            IF(COL3.EQ.COL3P)THEN
              IF(COL4.EQ.COL4P)THEN
                GO TO 5555
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        ENDIF
      ENDIF

!     02/01/2005 LAH added new code to find SN 
      ! Pre-processor to find TN and SN if not provided (DS4 files)
      TVI1 = TVILENT(FILENEW)
      IF (FILENEW(TVI1-2:TVI1).EQ.'INH') THEN
        OPEN(filenum,FILE=filenew,STATUS='OLD')
        DO WHILE (TL10(1:6).NE.'*TREAT')
          READ (FILENUM,'(A10)') TL10
        ENDDO
        READ (FILENUM,'(A10)') TL10
        READ (FILENUM,'(A10)') TL10
        READ (TL10,'(I3)') COL1
        READ (TL10,'(3X,I3)') COL2
        CLOSE (filenum)
        COL1R = 0
        COL3 = 1
        COL4 =1
      ENDIF

!     CHP 9/19/2005 Changed .LE. to .LT. at PW's request
!      IF (COL1.LE.0) THEN
      IF (COL1.LT.0) THEN
        WRITE (*,*) '  Treatment # for first treatment <= 0 !'
        WRITE (*,*) '  Program will have to stop!'
        WRITE (*,*) '  Check WORK.OUT for details of run'
        WRITE(fnumwrk,*)'  Treatment # for first treatment <= 0 !'
        STOP ' '
      ENDIF

      WRITE(fnumwrk,*)' '
      WRITE(fnumwrk,*)'FILE ',FILENAME(1:FILELEN)
      IF (COL1R.LT.0) COL1R = 0
      WRITE(fnumwrk,*)' TREATMENT: ',COL1
      WRITE(fnumwrk,*)' REPLICATE: ',COL1R
      WRITE(fnumwrk,*)' SN,ON,CN (as read): ',COL2,COL3,COL4
      IF (COL2.LE.0) COL2 = 1
      IF (COL3.LE.0) COL3 = 1
      IF (COL4.LE.0) COL4 = 1
      WRITE(fnumwrk,*)' SN,ON,CN (as used): ',COL2,COL3,COL4

      LEVEL=TL10FROMI(COL1)
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN) = LEVEL(1:LEVELLEN)
      LEVELNEW=LEVELSTD(1:LEVELLEN)

      FTYPE='*'

      OPEN(filenum,FILE=filename(1:FILELEN),STATUS='OLD')

      tl0801=' '
      DATANUM=0
      LINE=0
      TVI0=0
      RETURNUM=0
      TR='N'
      CTR='N'
      STARTCOL = 1
      ABVLINE=' '

      XREADNUM = 0

 4444 CONTINUE

      STARTRE = 0
      STARNUM = 0

 8888 CONTINUE

      XREADNUM = XREADNUM + 1

      DO
        tl2541=' '
        READ(filenum,'(A254)',END=9996,ERR=9999)tl2541

        IF(tl2541(1:1).EQ.'$')ftype='$'

        IF(TVILENT(tl2541).LT.3)GOTO 999
        IF(tl2541(1:1).EQ.'!')GOTO 999

        COLON = 'N'
        IF(tl2541(1:4).EQ.'*EXP'.OR.
     &   tl2541(1:1).EQ.'$'.AND.tl2541(8:10).eq.'EXP' .OR.
     &   tl2541(1:1).EQ.'$'.AND.tl2541(8:10).eq.'DET')THEN
          IF(tl2541(1:8).NE.'$CONTROL')THEN
            DO TVI1=1,30
              IF(COLON.EQ.'Y'.AND.TL2541(TVI1:TVI1).NE.' ')EXIT
              IF(TL2541(TVI1:TVI1).EQ.':')COLON='Y'
              TL2541(TVI1:TVI1)=' '
            ENDDO
            CALL LTRIM(TL2541)
            READ (TL2541,'(A10,A60)')EXPER,ENAME
            CALL Ltrim(ENAME)
            DATANUM=DATANUM+1
            DATARRAY(DATANUM)='EXPER '//' '//exper
            DATANUM=DATANUM+1
            DATARRAY(DATANUM)='ENAME '//' '//ename
          ENDIF
          GO TO 999
        ENDIF

        IF(tl2541(1:1).EQ.'$')GOTO 999
        IF(tl2541(1:3).EQ.'*EX')GOTO 999

        IF(TR.EQ.'Y')THEN
          RETURNUM=RETURNUM+1
          IF (RETURNUM .GT. 250) THEN
            WRITE(fnumwrk,*)' Problem reading treatment! '
            WRITE(fnumwrk,*)' Treatment,component,option,species: ',
     &      COL1,COL2,COL3,COL4
            WRITE (*,*) ' Problem reading treatment! '
            WRITE (*,*) ' Treatment,component,option,species: ',
     &      COL1,COL2,COL3,COL4
            WRITE (*,*) ' Program will have to stop'
            WRITE (*,*) ' Check WORK.OUT for details of run'
            STOP ' '
          ENDIF
        ENDIF

        tl30 = ' '
        TL30 = TL2541(1:30)
        CALL Ltrim(tl30)
        tl30(1:1) = Tl1upcase(tl30(1:1))
        tl30(2:2) = Tl1upcase(tl30(2:2))
        IF (TL30(2:2).EQ.' ') TL30(3:3)=' '

        IF (tl2541(1:1).EQ.'*') THEN
          IF (tl2541(1:8).NE.'*CONTROL') THEN
           IF (TL2541(2:4).EQ.'EXP') GO TO 999
           IF (TL2541(2:4).NE.'GEN') STARNUM = STARNUM + 1

            ! Following is to accomodate files with treatment group
            ! low in file
            IF (STARNUM .EQ. STARTRE+1 .AND. STARNUM .GT. 2) THEN
              ! Following is necessary to stop entering
              ! when treatments at end of file
              IF (XREADNUM .EQ. 1) THEN
                CLOSE (FILENUM)
                OPEN (filenum,FILE=filename(1:FILELEN),STATUS='OLD')

                tl0801=' '
                DATANUM=0
                LINE=0
                TVI0=0
                RETURNUM=0
                TR='N'
                CTR='N'
                STARTCOL = 1
                ABVLINE=' '
                GO TO 8888
              ENDIF
            ENDIF

           IF(TL2541(2:4).EQ.'TRE')THEN
             TR='Y'
             STARTCOL = 5
             IF (STARTRE.EQ.0) STARTRE = STARNUM
           ELSEIF(TL2541(2:4).EQ.'GEN')THEN
             TR='N'
             STARTCOL = 1
           ELSE
             TR='N'
             STARTCOL = 2
           ENDIF
           LE=' '
           IF(TL2541(2:4).EQ.'GEN')LE(1:2)='00'
           IF(TL2541(2:4).EQ.'TRE')LE(1:LEVELLEN)=LEVELSTD(1:LEVELLEN)
           IF(TL2541(2:4).EQ.'CUL')LE(1:2)=CU
           IF(TL2541(2:4).EQ.'FIE')LE(1:2)=FL
           IF(TL2541(2:4).EQ.'SOI')LE(1:2)=SA
           IF(TL2541(2:4).EQ.'INI')LE(1:2)=IC
           IF(TL2541(2:4).EQ.'PLA')LE(1:2)=MP
           IF(TL2541(2:4).EQ.'IRR')LE(1:2)=MI
           IF(TL2541(2:4).EQ.'FER')LE(1:2)=MF
           IF(TL2541(2:4).EQ.'RES')LE(1:2)=MR
           IF(TL2541(2:4).EQ.'CHE')LE(1:2)=MC
           IF(TL2541(2:4).EQ.'TIL')LE(1:2)=MT
           IF(TL2541(2:4).EQ.'ENV')LE(1:2)=ME
           IF(TL2541(2:4).EQ.'HAR')LE(1:2)=MH
           IF(TL2541(2:4).EQ.'FIL')LE(1:2)=FI
           IF(TL2541(2:4).EQ.'SIM')THEN
             LE(1:2)=SM
             IF(SM(1:1).NE.'0')CTR='Y'
           ENDIF
           IF(TL2541(2:4).EQ.'AUT')LE(1:2)=AM
          ENDIF

          GO TO 999
        ENDIF

        IF(tl2541(1:1).EQ.'@')THEN
          ! Strip off leading @
          tl2541(1:1)=' '

          ! Following code introduced to move header across.
          ! Maybe necessary in case first column is character field
          ! eg General section, but not for data fields where
          ! first column must be numeric!!!
          TVI2 = 0
          TVI3 = 0
          DO tvi1 = 1,30
            IF(tl2541(tvi1:tvi1).EQ.' ' .AND. TVI2.GT.0)THEN
              TVI3=TVI1-1
              EXIT
            ENDIF
            IF(tl2541(tvi1:tvi1).NE.' '.AND. TVI2.EQ.0)TVI2=TVI1
          ENDDO
          TL10(1:(TVI3-TVI2+1))=TL2541(TVI2:TVI3)
          DO TVI1=1,TVI3
            TL2541(TVI1:TVI1)=' '
          ENDDO
          TL2541(1:(TVI3-TVI2+1))=TL10(1:(TVI3-TVI2+1))

          ! Strip out leading periods
          ABVLEN=TVILENT(TL2541)
          DO TVI1=1,ABVLEN-1
            IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
          ENDDO
          IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
            ABVLINE(1:1)=' '
            ABVLINE=tl2541
            ABVSTD=' '
            CALL STANDARD(ABVLINE,ABVSTD,'13')
            ABVNUM=TVINSTR(ABVSTD)
            ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE = 40
            IF(ABVNUM.GT.40)ABVNUM=40
            LINE=1
            GO TO 999
          ENDIF
        ENDIF

        DATALINE=tl2541

        IF(ABVNUM.EQ.1)THEN
          IF(line.EQ.1)THEN
            datanum=datanum+1
            CALL LTRIM(abvline)
            CALL LTRIM(dataline)
            DATARRAY(datanum)=abvline(1:6)//' '//dataline(1:248)
            LINE=LINE+1
          ELSE
            CALL LTRIM(dataline)
            tvi1=TVILENT(DATARRAY(datanum))
            tvi2=TVILENT(dataline)
            IF(tvi1+tvi2.GT.241)tvi2=254-tvi1
            DATARRAY(datanum)=
     &      DATARRAY(datanum)(1:tvi1)//' '//dataline(1:tvi2)
          ENDIF
          GO TO 999
        ENDIF

        DATASTD=' '
        TEXTSTD=' '
        IF(TVILENT(abvline).LT.3)THEN
          WRITE(fnumwrk,*)' Header line not found! '
          WRITE(fnumwrk,*)' Possibly wrong file type!'
          WRITE(fnumwrk,*)' Header line: ',abvline(1:60)
          WRITE(fnumwrk,*)' Data line  : ',dataline(1:60)
          WRITE (*,*) ' Header line not found! '
          WRITE (*,*) ' Possibly wrong file type!'
          WRITE (*,*) ' Header line: ',abvline(1:60)
          WRITE (*,*) ' Data line  : ',dataline(1:60)
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check WORK.OUT for details of run'
          STOP ' '
        ENDIF

        CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'13')
        ! Below is for debugging purposes only
        ! WRITE(fnumwrk,*)' ABVLINE  ',abvline(1:60)
        ! WRITE(fnumwrk,*)' ABVSTD   ',abvstd(1:60)
        ! WRITE(fnumwrk,*)' DATALINE ',dataline(1:60)
        ! WRITE(fnumwrk,*)' DATASTD  ',datastd(1:60)

         IF(le(1:2).NE.'00')THEN
           leveltmp=' '
           READ(DATASTD,'(A13)',ERR=7777)leveltmp(1:13)
           CALL Ltrim(leveltmp)
           IF(leveltmp(1:1).EQ.'0')THEN
             leveltmp(1:1)=' '
             IF(leveltmp(2:2).EQ.'0')THEN
               leveltmp(2:2)=' '
               IF(leveltmp(3:3).EQ.'0')THEN
                 leveltmp(3:3)=' '
               ENDIF
             ENDIF
           ENDIF
           CALL Ltrim(leveltmp)
         ENDIF

        IF(TR.EQ.'Y')THEN
          DATAROC=' '
          IF(ftype.EQ.'*')THEN
            DATAROC(1:52)=DATASTD(1:52)
          ELSEIF(ftype.EQ.'$')THEN
            READ(DATASTD,'(13X,I13)',ERR=7777)COLRN
            IF (COLRN.NE.COL1R)GO TO 999
            DATAROC(1:13)=DATASTD(1:13)
            DATAROC(14:52)=DATASTD(26:65)
          ENDIF
          READ(DATAROC,'(13X,I13)',ERR=7777)COL2TMP
          IF (COL2TMP.EQ.0)COL2TMP=COL2
          IF (COL2TMP.NE.COL2)GO TO 999
          READ(DATAROC,'(26X,I13)',ERR=7777)COL3TMP
          IF (COL3TMP.EQ.0)COL3TMP=COL3
          IF (COL3TMP.NE.COL3)GO TO 999
          READ(DATAROC,'(39X,I13)',ERR=7777)COL4TMP
          IF (COL4TMP.EQ.0)COL4TMP=COL4
          IF (COL4TMP.NE.COL4)GO TO 999
        ENDIF

        IF (LE .NE. '00') THEN
          IF (LE(1:1).EQ.'0') LE(1:1) = ' ' ! NEW DEC 03
          CALL Ltrim(le)                    ! NEW DEC 03
          LENTMP=TVILENT(LE)
          ! Below is for debugging
          ! Write (fnumwrk,*)' Leveltmp ie what read ',leveltmp
          ! Write (fnumwrk,*)' Le ie what needed     ',le
          IF(LEVELTMP(1:LENTMP).NE.LE(1:LENTMP))GO TO 999
          IF(LEVELTMP(LENTMP+1:LENTMP+1).NE.' ')GO TO 999
        ENDIF

        TEXT='N'

        DO TVI1=1,ABVNUM-(STARTCOL-1)
          IF(TVI1.EQ.1)THEN
            IF(LINE.EQ.1)THEN
              datanums=datanum
            ELSE
              datanum=datanums
            ENDIF
          ENDIF
          datanum=datanum+1
          TVI2 = 1 + ((STARTCOL-1)*LENSTD) + (TVI1-1)*LENSTD
          TVI4=TVI2 + (LENSTD-1)
          COEFFC=DATASTD(TVI2:TVI4)
          ABVC=ABVSTD(TVI2:TVI4)
          ! Below are for debugging
          ! Write(fnumwrk,*)' abvc   ',abvc
          ! Write(fnumwrk,*)' coeffc ',coeffc
          COEFFCTM=COEFFC
          CALL LTRIM(COEFFCTM)
          IF (TVILENT(COEFFCTM).LT.1)
     &      COEFFC = '-99         '
          CALL LTRIM(TEXTSTD)
          IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3) = '-99'
          COEFFCHK=COEFFCTM(1:4)

          IF (LINE.EQ.1) THEN
            IF(COEFFCHK.EQ.'TEXT')THEN
              DATARRAY(datanum)=abvc//' '//TEXTSTD
            ELSE
              DATARRAY(datanum)=abvc//' '//coeffc
            ENDIF
            IF(TVI1.EQ.(ABVNUM-(STARTCOL-1)))LINE=LINE+1
          ELSE
            LENTMP=TVILENT(DATARRAY(datanum))
            IF(COEFFCHK.EQ.'TEXT')THEN
              TL2541=DATARRAY(datanum)(1:LENTMP)//' '//TEXTSTD
              DATARRAY(datanum)=TL2541
            ELSE
              TL2541=
     &        DATARRAY(datanum)(1:LENTMP)//' '//coeffc
              DATARRAY(datanum)=TL2541
            ENDIF
          ENDIF

          IF(ABVC(1:3).EQ.'CU ')CU=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'FL ')FL=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'SA ')SA=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'IC ')IC=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'MP '.OR.ABVC(1:3).EQ.'PL ')MP=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'MI '.OR.ABVC(1:3).EQ.'IR ')MI=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'MF '.OR.ABVC(1:3).EQ.'FE ')MF=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'MR '.OR.ABVC(1:3).EQ.'RE ')MR=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'MC '.OR.ABVC(1:3).EQ.'CH ')MC=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'MT '.OR.ABVC(1:3).EQ.'TI ')MT=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'ME '.OR.ABVC(1:3).EQ.'EN ')ME=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'ME '.OR.ABVC(1:3).EQ.'EM ')ME=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'MH '.OR.ABVC(1:3).EQ.'HA ')MH=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'SM ')THEN
            SM=COEFFC(1:2)
            FI=COEFFC(1:2)
          ENDIF
          IF(ABVC(1:3).EQ.'AM ')AM=COEFFC(1:2)
          IF(ABVC(1:3).EQ.'FI ')FI=COEFFC(1:2)

        ENDDO

  999   CONTINUE

      END DO

 9996 CONTINUE

      ! Following is to accomodate files with treatment group
      ! at the end of the file
      IF (STARNUM.EQ.STARTRE .AND. XREADNUM.EQ.1) THEN
        WRITE(fnumwrk,*)' Treatments at end of file!'
        WRITE(fnumwrk,*)' Will return to top of file'
        CLOSE (FILENUM)
        OPEN (filenum,FILE=filename(1:FILELEN),STATUS='OLD')
        tl0801=' '
        DATANUM=0
        LINE=0
        TVI0=0
        RETURNUM=0
        TR='N'
        CTR='N'
        STARTCOL = 1
        ABVLINE=' '
        GO TO 8888
      ENDIF
 9998 CONTINUE
 9999 CONTINUE

      CLOSE(filenum)

      IF (CTR.EQ.'N')THEN
        arg = ' '
        tvi2 = 0
        tvi3 = 0
        CALL GETARG (0,arg)
        DO tvi1 = 1,arglen
          IF (arg(tvi1:tvi1).EQ.'/') tvi2=tvi1
          IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1-1
        ENDDO
        IF (tvi3.EQ.0) THEN
          tvi3 = arglen
        ENDIF
        ctrdirfl=ARG(1:TVI3)//'.CTR'
        ! Change control file name from module specific to general
        DO L = LEN(TRIM(CTRDIRFL)),1,-1
          IF (CTRDIRFL(L:L).EQ.'/') EXIT
        ENDDO
        IF (L.GT.1) THEN
          ctrdirfl = CTRDIRFL(1:L-1)//'/'//'CROPSIM.CTR'
        ELSE
          ctrdirfl(1:11) = 'CROPSIM.CTR'
        ENDIF

        INQUIRE(FILE=ctrdirfl,EXIST=fflag)
        IF(.NOT.fflag)THEN
          WRITE (fnumwrk,*) ' No control file: ',ctrdirfl(1:60)
          WRITE (*,*) ' No control file: ',ctrdirfl(1:60)
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
        OPEN(filenum,FILE=ctrdirfl,STATUS='OLD')
        WRITE(fnumwrk,*)' CONTROLS FROM: ',ctrdirfl(1:60)
        STARTCOL=2
        CTR='Y'
        TR='N'
        LE='1 '
        GO TO 4444
      ENDIF

      CLOSE(filenum)

      DO TVI3 = 1,DATANUM
        WRITE(fnumwrk,'(I6,A2,A60)')tvi3,'  ',datarray(TVI3)(1:60)
      ENDDO

      IF (DATANUM > 0 .AND. DATARRAY(1)(1:6) .EQ. '      ')THEN
        WRITE (fnumwrk,*) ' Problem reading: ',filename(1:55)
        WRITE (fnumwrk,*) ' First array element: ',datarray(1)(1:50)
        WRITE (fnumwrk,*) ' 2nd array element: ',datarray(2)(1:50)
        WRITE (fnumwrk,*) ' Number in array: ',datanum
        WRITE (*,*) ' Problem reading: ',filename(1:55)
        WRITE (*,*) ' First array element: ',datarray(1)(1:50)
        WRITE (*,*) ' 2nd array element: ',datarray(2)(1:50)
        WRITE (*,*) ' Number in array: ',datanum
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      abvfile=' '
      abvfile(1:10)='DETAIL.CDE'
      INQUIRE(FILE=abvfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
        CALL GETARG(0,arg)
        DO tvi1 = 1,arglen
          IF(arg(tvi1:tvi1).EQ.'/')tvi2=tvi1
        ENDDO
        abvfile=ARG(1:TVI2)//'DETAIL.CDE'
        INQUIRE(FILE=abvfile,EXIST=fflag)
      ENDIF
      IF(fflag)THEN
        ! Read abbreviation file
        IF (CSWOUT.EQ.'Y')
     &  WRITE(fnumwrk,*)' Reading abbrev file: ',abvfile(1:55)
        CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
      ELSE
        IF (CSWOUT.EQ.'Y')
     &  WRITE(fnumwrk,*)' No abbreviation file: ',abvfile(1:55)
        abvsyn=0
      ENDIF

 5555 CONTINUE

      LEVELOLD = ' '
      FILEOLD = ' '
      LEVELOLD = LEVELNEW
      FILEOLD=' '
      FILEOLD(1:FILELEN) = FILENEW(1:FILELEN)

      COL1P=COL1
      COL1RP=COL1R
      COL2P=COL2
      COL3P=COL3
      COL4P=COL4

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)

!      WRITE(FNUMWRK,'(A2,*)'  ',CODE,' ',TXTO(1:40)

      RETURN

 7776 CONTINUE
      ERRLINES = TL2542(1:60)
      ERRLINE = TL2541(1:60)
      GO TO 7778

 7777 CONTINUE

      ERRLINES = DATALINE(1:60)
      ERRLINE = DATASTD(1:60)

 7778 CONTINUE
      WRITE(fnumwrk,*)' Problem in reading treatments in X-file file!'
      WRITE(fnumwrk,*)' Original line was: ',errline(1:50)
      WRITE(fnumwrk,*)' Standard line was: ',errlines(1:50)
      WRITE(fnumwrk,*)' Maybe that numbers run together!'
      WRITE(fnumwrk,*)' Or dots in header line over numeric fields!'
      WRITE (*,*) ' Problem in reading treatments in X-file file!'
      WRITE (*,*) ' Original line was: ',errline(1:50)
      WRITE (*,*) ' Standard line was: ',errlines(1:50)
      WRITE (*,*) ' Maybe that numbers run together!'
      WRITE (*,*) ' Or dots in header line over numeric fields!'
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details of run'
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,
     & charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)  FILENAME, CODE, CHAROUT
      CHARACTER*254  TL2541, TXTO
      CHARACTER*100  CODENEW
      CHARACTER*20   COEFF
      CHARACTER*1    FOUND
      INTEGER        CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
      INTEGER        TVI3, CODECLEN, COL1, COL1R, COL2, COL3, COL4

      CALL XREADT (FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3 = 1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE

      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CHAROUT(1:3)='-99'
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADR(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,
     & valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)  FILENAME, CODE
      CHARACTER*30   CHAROUT
      REAL           VALUEOUT, TVRFROMC
      INTEGER        COL1, COL1R, COL2, COL3, COL4

      CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,charout)
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADI(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,
     & valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)  FILENAME, CODE
      CHARACTER*30   CHAROUT
      INTEGER        COL1, COL1R, COL2, COL3, COL4
      INTEGER        VALUEOUT, YEAR, DAY, CSYEARDOY
      REAL           VALUETMP, TVRFROMC

      CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,charout)

      ! Following is to remove leading characters if present
      IF(ICHAR(charout(1:1)).GT.64)charout(1:1)=' '
      IF(ICHAR(charout(2:2)).GT.64)charout(2:2)=' '

      VALUETMP=TVRFROMC(Charout(1:12))
      VALUEOUT=NINT(VALUETMP)

      ! Following is to return yeardoy instead of yrdoy
      IF (VALUEOUT.GT.0 .AND. VALUEOUT.LE.99365) THEN
        IF (CODE.EQ.'SDATE') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'PDATE') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'ICDAT') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'IDATE') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'FDATE') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'RDATE') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'PFRST') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'PLAST') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'CDATE') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'TDATE') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'ODATE') VALUEOUT = CSYEARDOY (valueout)
        IF (CODE.EQ.'HDATE') VALUEOUT = CSYEARDOY (valueout)
      ENDIF

      ! Following is to read years and days instead of date
      IF(CODE.EQ.'ICDAT'.AND.VALUEOUT.EQ.-99)THEN
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICDAY',charout)
        VALUETMP=TVRFROMC(Charout(1:12))
        IF (VALUETMP.LE.0)
     &  CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICDY',charout)
        VALUETMP=TVRFROMC(Charout(1:12))
        DAY=NINT(VALUETMP)
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'ICYR',charout)
        VALUETMP=TVRFROMC(Charout(1:12))
        YEAR=NINT(VALUETMP)
        VALUEOUT=YEAR*1000+DAY
      ENDIF
      IF(CODE.EQ.'PDATE'.AND.VALUEOUT.EQ.-99)THEN
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'PLDAY',charout)
        VALUETMP=TVRFROMC(Charout(1:12))
        DAY=NINT(VALUETMP)
        CALL XREADC(FILENAME,COL1,COL1R,COL2,COL3,COL4,'PLYR',charout)
        VALUETMP=TVRFROMC(Charout(1:12))
        YEAR=NINT(VALUETMP)
        VALUEOUT=YEAR*1000+DAY
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADCA(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,
     & AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)   FILENAME, CODE, AOUTSIZE
      CHARACTER*(*)   ARRAYOUT(*)
      CHARACTER*354   TL3541
      CHARACTER*254   TL2541,TXTO
      CHARACTER*100   CODENEW
      INTEGER         TVIFROMC
      INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE
      INTEGER         COL1, COL1R, COL2, COL3, COL4, LENARVAR

      CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      SIZE = TVIFROMC(AOUTSIZE)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=' '
      ENDDO

      LENARVAR = LEN(arrayout(1))

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,SIZE
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+(LENARVAR))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADRA(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,
     & AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)   FILENAME, CODE, AOUTSIZE
      CHARACTER*354   TL3541
      CHARACTER*254   TL2541,TXTO
      CHARACTER*100   CODENEW
      REAL            TVR1, TVRFROMC, ARRAYOUT(*)
      INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE
      INTEGER         COL1, COL1R, COL2, COL3, COL4


      CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      TVR1 = TVRFROMC(AOUTSIZE)
      SIZE = NINT(TVR1)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,SIZE
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE XREADIA(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,
     & AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)   FILENAME, CODE, AOUTSIZE
      CHARACTER*354   TL3541, TL3542
      CHARACTER*254   TL2541, TXTO, TXTO2, TL2542
      CHARACTER*100   CODENEW
      REAL            TVR1, VALUETMP, TVRFROMC
      INTEGER         TVIFROMC, YEAR, COL1, COL1R, COL2, COL3, COL4
      INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE, ARRAYOUT(*)
      INTEGER         CSYEARDOY

      CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      TVR1 = TVRFROMC(AOUTSIZE)
      SIZE = NINT(TVR1)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'8')
      DO L = 1,SIZE
         L2 = (L-1)*8 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         IF(ICHAR(tl3541(L2:L2)).GT.64)tl3541(L2:L2)=' '
         IF(ICHAR(tl3541(L2+1:L2+1)).GT.64)tl3541(L2+1:L2+1)=' '
         VALUETMP=TVRFROMC(TL3541(L2:L2+6))
         ARRAYOUT(L)=NINT(VALUETMP)
         ! Following is to return yeardoy instead of yrdoy
         IF (ARRAYOUT(L).GT.0 .AND. ARRAYOUT(L).LE.99365) THEN
           IF (CODE.EQ.'IDATE') ARRAYOUT(L) = CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'FDATE') ARRAYOUT(L) = CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'RDATE') ARRAYOUT(L) = CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'TDATE') ARRAYOUT(L) = CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'ODATE') ARRAYOUT(L) = CSYEARDOY (arrayout(l))
           IF (CODE.EQ.'HDATE') ARRAYOUT(L) = CSYEARDOY (arrayout(l))
         ENDIF
      END DO
 1000 CONTINUE

      IF(ARRAYOUT(1).EQ.-99)THEN
        IF(CODE.EQ.'FDATE' .OR.
     &     CODE.EQ.'IDATE' .OR.
     &     CODE.EQ.'WMDAT' .OR.
     &     CODE.EQ.'ODATE') THEN
          IF(CODE.EQ.'FDATE')THEN
            CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,'FEDAY',TXTO)
            CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,'FEYR',TXTO2)
          ELSEIF(CODE.EQ.'IDATE')THEN
            CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,'IRDAY',TXTO)
            CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,'IRYR',TXTO2)
          ELSEIF(CODE.EQ.'ODATE' .OR. CODE.EQ.'WMDATE')THEN
            CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,'EMDAY',TXTO)
            CALL XREADT(FILENAME,COL1,COL1R,COL2,COL3,COL4,'EMYR',TXTO2)
          ENDIF
          IF (TXTO(1:3).EQ.'-99') GO TO 1001
          TL2541=TXTO
          TL2542=TXTO2
          CALL Standard(TL2541,TL3541,'8')
          CALL Standard(TL2542,TL3542,'8')
          DO L = 1,SIZE
            L2 = (L-1)*8 + 1
            IF (TL3541(L2:L2).EQ.'!')GO TO 1001
            IF (TL3541(L2:L2).EQ.' ')GO TO 1001
            IF(ICHAR(tl3541(L2:L2)).GT.64)tl3541(L2:L2)=' '
            IF(ICHAR(tl3541(L2+1:L2+1)).GT.64)tl3541(L2+1:L2+1)=' '
            ARRAYOUT(L)=TVIFROMC(TL3541(L2:L2+6))
            IF (TL3542(L2:L2).EQ.'!')GO TO 1001
            IF (TL3542(L2:L2).EQ.' ')GO TO 1001
            IF(ICHAR(tl3542(L2:L2)).GT.64)tl3542(L2:L2)=' '
            IF(ICHAR(tl3542(L2+1:L2+1)).GT.64)tl3542(L2+1:L2+1)=' '
            YEAR=TVIFROMC(TL3542(L2:L2+6))
            ARRAYOUT(L)=YEAR*1000+ARRAYOUT(L)
          END DO
 1001     CONTINUE
        ENDIF
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AREADT(FILENAME,LEVELI,RN,SN,ON,CN,CODE,TXTO)

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)   FILENAME, CODE, TXTO
      CHARACTER*254   FILENEW, FILEOLD, DATARRAY(500), ABVARRAY(1000)
      CHARACTER*128   ARG
      character*100   CODENEW
      CHARACTER*132   ABVFILE
      CHARACTER*93    ADIRFILE
      CHARACTER*12    LEVELP,LEVELSTD, SYNCODE
      CHARACTER*10    TL10FROMI, LEVEL
      INTEGER         TVILENT, TVI1, TVI2
      INTEGER         LEVELLEN, FILELEN, LEVELI, LEVELIP
      INTEGER         DATANUM, LENSTD, CODELEN, ARGLEN
      INTEGER         CODENUM, ABVSYN, FNUMWRK
      INTEGER         RN, SN, ON, CN
      LOGICAL         FFLAG, FOPEN

!      SAVE            FILENEW

      ! File
      CALL LTRIM2(FILENAME,filenew)
      FILELEN=TVILENT(FILENEW)
      FILELEN=MIN(254,FILELEN)
      FILENEW=' '
      FILENEW(1:FILELEN)=FILENAME(1:FILELEN)

      ! Level
      LENSTD = 12
      LEVEL=TL10FROMI(LEVELI)
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN) = LEVEL(1:LEVELLEN)

      ! Code
      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)

      CODENUM=1
      SYNCODE=' '

      ! Check if have already read file
      IF(FILENEW.EQ.FILEOLD)THEN
        IF(LEVELI.EQ.LEVELIP)THEN
          GOTO 5555
        ENDIF
      ENDIF

      ! Check A-file name
      ADIRFILE=' '
      ADIRFILE=FILENAME(1:FILELEN-1)//'A'

      CALL AMAKE5(ADIRFILE,LEVEL,RN,SN,ON,CN,DATARRAY,datanum)

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF

      abvfile=' '
      abvfile(1:8)='DATA.CDE'
      INQUIRE(FILE=abvfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
        CALL GETARG(0,arg)
        DO tvi1 = 1,arglen
          IF(arg(tvi1:tvi1).EQ.'/')tvi2=tvi1
        ENDDO
        abvfile=ARG(1:TVI2)//'DATA.CDE'
        INQUIRE(FILE=abvfile,EXIST=fflag)
      ENDIF
      IF(fflag)THEN
        ! Read abbreviation file
        WRITE(fnumwrk,*)' Reading abbrev file: ',abvfile(1:55)
        CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
      ELSE
        WRITE(fnumwrk,*)' No abbreviation file: ',abvfile(1:55)
        abvsyn=0
      ENDIF

 5555 CONTINUE

      FILEOLD = ' '
      FILEOLD(1:FILELEN) = FILENEW(1:FILELEN)
      levelp=' '
      levelp(1:levellen)=levelstd(1:levellen)
      LEVELIP=LEVELI

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      ! WRITE(FNUMWRK,*)'  AREAD ',CODE,' ',TXTO(1:40)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AREADC(FILENAME,LEVELI,RN,SN,ON,CN,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)  CODE,CHAROUT,FILENAME
      CHARACTER*254  TL2541, TXTO
      CHARACTER*100  CODENEW
      CHARACTER*20   COEFF
      CHARACTER*1    FOUND
      INTEGER        CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
      INTEGER        TVI3, LEVELI, CODECLEN, RN, SN, ON, CN

      CALL AREADT(FILENAME,LEVELI,RN,SN,ON,CN,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3 = 1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CHAROUT=' '
         CHAROUT(1:3)='-99'
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AREADR(FILENAME,LEVELI,RN,SN,ON,CN,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)  CODE,FILENAME
      CHARACTER*30   CHAROUT
      INTEGER        LEVELI, RN, SN, ON, CN
      REAL           VALUEOUT, TVRFROMC

      CALL AREADC(FILENAME,LEVELI,RN,SN,ON,CN,CODE,charout)
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AREADI(FILENAME,LEVELI,RN,SN,ON,CN,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)  CODE,FILENAME
      CHARACTER*30   CHAROUT
      INTEGER        LEVELI, VALUEOUT, RN, SN, ON, CN
      REAL           VALUETMP, TVRFROMC

      CALL AREADC(FILENAME,LEVELI,RN,SN,ON,CN,CODE,charout)
      VALUETMP=TVRFROMC(Charout(1:12))
      VALUEOUT=NINT(VALUETMP)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CUREADT(CUDIRFLE,LEVEL,CODE,TXTO)

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)   CODE, TXTO, cudirfle, LEVEL
      CHARACTER*254   DATARRAY(500), ABVARRAY(1000)
      character*100   codenew
      CHARACTER*132   ABVFILE
      CHARACTER*93    CUDIRFLP, CUDIRFLN
      CHARACTER*10    LEVELP,LEVELSTD
      INTEGER         TVILENT
      INTEGER         LEVELLEN, FILELEN, DATANUM, LENSTD
      INTEGER         CODELEN, FNUMWRK, ABVSYN
      LOGICAL         FOPEN

!      SAVE            CUDIRFLP

      LENSTD = 12

      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN) = LEVEL(1:LEVELLEN)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      CALL LTRIM2(CUDIRFLE,cudirfln)
      FILELEN=TVILENT(CUDIRFLN)
      FILELEN=MIN(93,FILELEN)
      CUDIRFLN=' '
      CUDIRFLN(1:FILELEN)=CUDIRFLE(1:FILELEN)

      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD

      ! Check if have already read file
      IF(CUDIRFLN.EQ.CUDIRFLP)THEN
        IF(LEVEL(1:LEVELLEN).EQ.LEVELP(1:LEVELLEN))THEN
          GOTO 5555
        ENDIF
      ENDIF

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF

      CALL AMAKE1(CUDIRFLN,LEVELSTD,DATARRAY,datanum)
      IF (DATANUM .LE. 0) THEN
        WRITE (fnumwrk,*) ' Problem reading cultivar file'
        WRITE (fnumwrk,*) ' File was: ',cudirfle(1:60)
        WRITE (*,*) ' Problem reading cultivar file'
        WRITE (*,*) ' File was: ',cudirfle(1:60)
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      ! No abbreviation file. Headers must be standard!
      abvfile=' '
      abvsyn=0

 5555 CONTINUE

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      ! WRITE(FNUMWRK,*)'  CUREAD ',CODE,' ',TXTO(1:40)

      CUDIRFLP=' '
      CUDIRFLP(1:FILELEN)=CUDIRFLN(1:FILELEN)

      levelp=' '
      levelp(1:levellen)=levelstd(1:levellen)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CUREADC(CUDIRFLE,LEVEL,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)  CODE, CHAROUT, LEVEL, CUDIRFLE
      CHARACTER*254  TL2541, TXTO
      CHARACTER*100  CODENEW
      CHARACTER*20   COEFF
      CHARACTER*1    FOUND
      INTEGER        CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
      INTEGER        TVI3, CODECLEN, FNUMWRK
      LOGICAL        FOPEN

      CALL CUREADT(CUDIRFLE,LEVEL,CODE,TXTO)
      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3 = 1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CALL Getlun('WORK.OUT',fnumwrk)
         INQUIRE (FILE='Work.out',OPENED=fopen)
         IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
         WRITE (fnumwrk,*) ' Problem reading cultivar file'
         WRITE (fnumwrk,*) ' File was: ',cudirfle(1:60)
         WRITE (fnumwrk,*) ' Could not find code: ',Code
         WRITE (*,*) ' Problem reading cultivar file'
         WRITE (*,*) ' File was: ',cudirfle(1:60)
         WRITE (*,*) ' Could not find code: ',Code
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check WORK.OUT for details of run'
         STOP ' '
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CUREADR(CUDIRFLE,LEVEL,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) CODE,LEVEL,CUDIRFLE
      CHARACTER*30  CHAROUT
      REAL          VALUEOUT,TVRFROMC

      CALL CUREADC(CUDIRFLE,LEVEL,CODE,charout)
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE ECREADT(ECDIRFLE,LEVEL,CODE,TXTO)

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) CODE, TXTO, ECDIRFLE, LEVEL
      CHARACTER*254 DATARRAY(500), ABVARRAY(1000)
      character*100 CODENEW
      CHARACTER*132 ABVFILE
      CHARACTER*93  ECDIRFLT, ECDIRFLP
      CHARACTER*10  LEVELP, LEVELSTD
      INTEGER       TVILENT
      INTEGER       LEVELLEN, FILELEN, DATANUM, LENSTD
      INTEGER       CODELEN, FNUMWRK, ABVSYN
      LOGICAL       FOPEN

!      SAVE          ECDIRFLT

      CALL LTRIM2(ECDIRFLE,ecdirflt)
      FILELEN=TVILENT(ECDIRFLT)
      FILELEN=MIN(93,FILELEN)
      ECDIRFLT=' '
      ECDIRFLT(1:FILELEN)=ECDIRFLE(1:FILELEN)

      LENSTD = 12

      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN) = LEVEL(1:LEVELLEN)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      IF(ECDIRFLT(1:FILELEN).EQ.ECDIRFLP(1:FILELEN))THEN
        IF(LEVEL(1:LEVELLEN).EQ.LEVELP(1:LEVELLEN))GO TO 5555
      ENDIF

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF

      CALL AMAKE1(ECDIRFLT,LEVELSTD,DATARRAY,datanum)
      IF (DATANUM .LE. 0)THEN
        WRITE (fnumwrk,*) ' Problem reading: ',ECDIRFLT(1:55)
        WRITE (fnumwrk,*) ' Likely that level not found.'
        WRITE (*,*) ' Problem reading: ',ECDIRFLT(1:55)
        WRITE (*,*) ' Likely that level not found.'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      ! No abbreviation file. Headers must be standard!
      abvfile=' '
      abvsyn=0

 5555 CONTINUE

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      ! WRITE(FNUMWRK,*)'  ECREAD ',CODE,' ',TXTO(1:40)

      ECDIRFLP=' '
      ECDIRFLP(1:FILELEN)=ECDIRFLT(1:FILELEN)
      levelp(1:levellen)=levelstd(1:levellen)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE ECREADC(ECDIRFLE,LEVEL,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)  CODE,CHAROUT,LEVEL,ECDIRFLE
      CHARACTER*254  TL2541, TXTO
      CHARACTER*100  CODENEW
      CHARACTER*40   COEFF
      CHARACTER*1    FOUND
      INTEGER        CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
      INTEGER        TVI3, CODECLEN, FNUMWRK
      LOGICAL        FOPEN

      CALL ECREADT(ECDIRFLE,LEVEL,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,40
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3 = 1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.40)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:40)=COEFF
        ENDIF
       ELSE
         CALL Getlun('WORK.OUT',fnumwrk)
         INQUIRE (FILE='Work.out',OPENED=fopen)
         IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
         WRITE (fnumwrk,*) ' Could not find code: ',Code
         WRITE (fnumwrk,*) ' File was: ',ECDIRFLE(1:60)
         WRITE (*,*) ' Could not find code: ',Code
         WRITE (*,*) ' File was: ',ECDIRFLE(1:60)
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check WORK.OUT for details of run'
         STOP ' '
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE ECREADR(ECDIRFLE,LEVEL,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) CODE,LEVEL,ECDIRFLE
      CHARACTER*30  CHAROUT
      REAL          VALUEOUT, TVRFROMC

      CALL ECREADC(ECDIRFLE,LEVEL,CODE,charout)
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END


!-----------------------------------------------------------------------

      SUBROUTINE SPREADT(SPDIRFLE,CODE,TXTO)

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) SPDIRFLE, CODE, TXTO
      CHARACTER*254 DATARRAY(500), TL2541, ABVLINE, DATALINE
      CHARACTER*254 ABVARRAY(1000)
!chp1/30/2006      CHARACTER*600 DATASTD, ABVSTD
      CHARACTER*1000 DATASTD, ABVSTD
      CHARACTER*132 ABVFILE
      CHARACTER*100 CODENEW
      CHARACTER*93  SPDIRFLT, SPDIRFLP
      CHARACTER*52  TEXTSTD, tl0521
      CHARACTER*30  TL30
      CHARACTER*12  COEFFC(200), ABVC(200), COEFFCTM, SYNCODE
      CHARACTER*4   COEFFCHK
      CHARACTER*1   TEXT, BLANK, CSWOUT
      INTEGER       TVILENT, TVI1, TVI2, TVI0, TVI3, TVI4
      INTEGER       FILENUM, FILELEN, LINE, LENTMP
      INTEGER       TVINSTR, ABVNUM, LENSTD, ABVLEN, DATANUM
      INTEGER       CODENUM, ABVSYN, FNUMWRK, L, CODELEN
      LOGICAL       FOPEN

      PARAMETER     (BLANK = ' ')

!      SAVE          SPDIRFLT

      CSWOUT = 'Y'

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      LENSTD = 12
      CODENUM=1
      SYNCODE=' '

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      CALL LTRIM2(SPDIRFLE,spdirflt)
      FILELEN=TVILENT(SPDIRFLT)
      FILELEN=MIN(93,FILELEN)
      SPDIRFLT=' '
      SPDIRFLT(1:FILELEN)=SPDIRFLE(1:FILELEN)

      IF(SPDIRFLE(1:FILELEN).EQ.SPDIRFLP(1:FILELEN))GO TO 5555

      OPEN(filenum,FILE=SPDIRFLE(1:FILELEN),STATUS='OLD')

      DO L = 1,500
        DATARRAY = ' '
      ENDDO
      DATANUM = 0
      ABVNUM = 0
      LINE=0
      TVI0=0
      TVI1=0

      DO

        tl2541=' '
        tl30=' '
        LINE=LINE+1
        DO
          READ(filenum,'(A254)',END=999,ERR=9999)tl2541
          IF(tl2541(1:1).EQ.'*')GOTO 888
          IF(tl2541(1:1).EQ.'$')GOTO 888
          IF(tl2541(1:1).EQ.'!')GOTO 888
          IF(tvilent(tl2541).LT.6)GOTO 888
          IF(tl2541(1:1).EQ.'@')THEN
            ! Strip out leading periods
            ABVLEN=TVILENT(TL2541)
            DO TVI1=1,ABVLEN-1
              IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
            ENDDO
            IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
              TVI0=TVI0+ABVNUM
              ABVLINE=tl2541
              LINE=1
            ENDIF
            GO TO 888
          ENDIF
          EXIT
  888     CONTINUE
        END DO

        DATALINE=tl2541

        TEXT='N'

        ABVSTD=' '
        DATASTD=' '
        tl0521=' '
        TEXTSTD=' '
        COEFFC(TVI0+1)=' '

        CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'12')
        IF(ABVLINE(1:1).EQ.'@')ABVLINE(1:1)=' '
        CALL STANDARD(ABVLINE,ABVSTD,'12')
        ABVNUM=TVINSTR(ABVSTD)
        ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE = 40
        IF(ABVNUM.GT.40)ABVNUM=40
        DO TVI1=1,ABVNUM
          TVI2= 1 + (TVI1-1)*LENSTD
          TVI4=TVI2 + (LENSTD-1)
          COEFFC(TVI0+TVI1)=DATASTD(TVI2:TVI4)
          ABVC(TVI0+TVI1)=ABVSTD(TVI2:TVI4)
          COEFFCTM=COEFFC(TVI0+TVI1)
          CALL LTRIM(COEFFCTM)
          IF (TVILENT(COEFFCTM).LT.1)COEFFC(TVI0+TVI1) = '-99         '
          CALL LTRIM(TEXTSTD)
          IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3) = '-99'
          COEFFCHK=COEFFCTM(1:4)
          IF (LINE.EQ.1) THEN
           IF(COEFFCHK.EQ.'TEXT')THEN
            datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//TEXTSTD
           ELSE
            datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//coeffc(TVI0+tvi1)
           ENDIF
          ELSE
           LENTMP=TVILENT(DATARRAY(TVI0+TVI1))
           IF(COEFFCHK.EQ.'TEXT')THEN
            TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//TEXTSTD
            datarray(TVI0+tvi1)=TL2541
           ELSE
            TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//coeffc(TVI0+tvi1)
            datarray(TVI0+tvi1)=TL2541
           ENDIF
          ENDIF
        ENDDO

        datanum=tvi0+abvnum

      ENDDO

  999 CONTINUE

      CLOSE(FILENUM)

      IF (CSWOUT.EQ.'Y') THEN
        WRITE(fnumwrk,*)' '
        WRITE(fnumwrk,*)'FILE ',SPDIRFLE(1:FILELEN)
        DO TVI3 = 1,DATANUM
          WRITE(fnumwrk,'(I6,A2,A60)')tvi3,'  ',datarray(TVI3)(1:60)
        ENDDO
      ENDIF

      ! No abbreviation file. Headers must be standard!
      abvfile=' '
      abvsyn=0

 5555 CONTINUE

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)

      SPDIRFLP=' '
      SPDIRFLP(1:FILELEN)=SPDIRFLT(1:FILELEN)

      RETURN

 9999 CONTINUE
      CLOSE (filenum)
      WRITE (fnumwrk,*)' Problem reading: ',spdirfle(1:50)
      WRITE (*,*) ' Problem reading: ',spdirfle(1:50)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details of run'
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADC(SPDIRFLE,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) SPDIRFLE, CODE, CHAROUT
      CHARACTER*254 TL2541, TXTO
      CHARACTER*100 CODENEW
      CHARACTER*20  COEFF
      CHARACTER*1   FOUND
      INTEGER       CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
      INTEGER       TVI3, CODECLEN, FNUMWRK
      LOGICAL       FOPEN

      CALL SPREADT(SPDIRFLE,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3 = 1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CALL Getlun('WORK.OUT',fnumwrk)
         INQUIRE (FILE='Work.out',OPENED=fopen)
         IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
         WRITE (fnumwrk,*) ' Could not find code: ',Code
         WRITE (fnumwrk,*) ' File was: ',spdirfle(1:50)
         WRITE (*,*) ' Could not find code: ',Code
         WRITE (*,*) ' File was: ',spdirfle(1:50)
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check WORK.OUT for details of run'
         STOP ' '
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADR(SPDIRFLE,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) SPDIRFLE, CODE
      CHARACTER*30  CHAROUT
      REAL          VALUEOUT, TVRFROMC

      CALL SPREADC(SPDIRFLE,CODE,charout)
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADCA(SPDIRFLE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) SPDIRFLE, CODE, AOUTSIZE, ARRAYOUT(*)
      CHARACTER*354 TL3541
      CHARACTER*254 TL2541,TXTO
      CHARACTER*100 CODENEW
      REAL          TVR1, TVRFROMC
      INTEGER       CODELEN, TVI1, TVILENT, L, L2, SIZE, LENARVAR

      CALL SPREADT(SPDIRFLE,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      TVR1 = TVRFROMC(AOUTSIZE)
      SIZE = NINT(TVR1)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=' '
      ENDDO

      LENARVAR = LEN(arrayout(1))

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,SIZE
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+LENARVAR)
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADRA(SPDIRFLE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) SPDIRFLE, CODE, AOUTSIZE
      CHARACTER*354 TL3541
      CHARACTER*254 TL2541,TXTO
      CHARACTER*100 CODENEW
      REAL          TVR1, TVRFROMC, ARRAYOUT(*)
      INTEGER       CODELEN, TVI1, TVILENT, L, L2, SIZE

      CALL SPREADT(SPDIRFLE,CODE,TXTO)
      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      TVR1 = TVRFROMC(AOUTSIZE)
      SIZE = NINT(TVR1)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,SIZE
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SPREADIA(SPDIRFLE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) SPDIRFLE, CODE, AOUTSIZE
      CHARACTER*354 TL3541
      CHARACTER*254 TL2541,TXTO
      CHARACTER*100 CODENEW
      INTEGER       TVIFROMC, ARRAYOUT(*)
      INTEGER       CODELEN, TVI1, TVILENT, L, L2, SIZE

      CALL SPREADT(SPDIRFLE,CODE,TXTO)
      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      SIZE = TVIFROMC(AOUTSIZE)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,SIZE
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVIFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) CODE, DATARRAY(500),TXTO
      CHARACTER*254 TL2541, TL2542, ABVARRAY(1000)
      CHARACTER*100 CODENEW
      CHARACTER*12  SYNCODE
      CHARACTER*1   ABVFOUND, FOUND
      INTEGER       DATANUM, TVILENT, ABVSYN
      INTEGER       TVI1, CODENUM, FNUMWRK, LENTEXT, CODELEN
      INTEGER       LENTMP, LENTMP2, TVI2, TVI3, TVIABV
      LOGICAL       FOPEN

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF

      SYNCODE=' '
      CODENUM=1

 5556 CONTINUE

      IF(TVILENT(SYNCODE).LT.1)THEN
        CALL LTRIM2(CODE,codenew)
      ELSE
        CALL LTRIM2(SYNCODE,codenew)
      ENDIF
      CODELEN=Tvilent(CODENEW)

      FOUND='N'
      DO TVI1 = 1,DATANUM
        TL2541=' '
        TL2541=DATARRAY(TVI1)

        ! Remove trailing blanks
        IF(TL2541((CODELEN+1):(CODELEN+1)).EQ.'.')THEN
          DO TVI2 = CODELEN+1,CODELEN+20
            IF(TL2541(TVI2:TVI2).EQ.' ')EXIT
            TL2541(TVI2:TVI2)=' '
          ENDDO
        ENDIF

        IF(CODENEW(1:CODELEN).EQ.TL2541(1:CODELEN).AND.
     &     TL2541((CODELEN+1):(CODELEN+1)).EQ.' ')THEN
          DO TVI2=CODELEN,100
            IF(TL2541(TVI2:TVI2).EQ.' ')EXIT
          ENDDO
          DO TVI3 = TVI2, 100
            IF (TL2541(TVI3:TVI3).NE.' ')EXIT
          ENDDO
          LENTMP=TVILENT(TL2541)
          LENTMP2=LENTMP-TVI3+1
          TL2542(1:LENTMP2)=TL2541(TVI3:LENTMP)
          IF(LENTMP2.GT.0)FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENTEXT=LEN(TXTO)
        IF (LENTEXT.LE.LENTMP2) THEN
          TXTO = TL2542(1:LENTEXT)
        ELSE
          TXTO = ' '
          TXTO(1:LENTMP2) = TL2542(1:LENTMP2)
        ENDIF
      ELSE
        IF(ABVSYN.GT.0)THEN
          IF(CODENUM.LE.1)THEN
            ABVFOUND='N'
            DO TVIABV = 1,ABVSYN
              CALL LTRIM(ABVARRAY(TVIABV))
              IF(CODENEW(1:CODELEN).EQ.ABVARRAY(TVIABV)(1:CODELEN))THEN
                ABVFOUND='Y'
                GO TO 8000
              ENDIF
            ENDDO
          ENDIF
 8000     CONTINUE
          IF(ABVFOUND.EQ.'Y')THEN
            CODENUM=CODENUM+1
            CALL Getstr(ABVARRAY(TVIABV),CODENUM,SYNCODE)
            IF(TVILENT(SYNCODE).GT.0.AND.SYNCODE(1:3).NE.'-99')THEN
              WRITE(fnumwrk,*)' Abbreviations  ',abvarray(tviabv)(1:55)
              WRITE(fnumwrk,*)' Trying synonym ',syncode
              GO TO 5556
            ELSE
              WRITE(fnumwrk,*)' Nothing found for ',Code
            ENDIF
          ENDIF
        ENDIF

        TXTO = ' '
        TXTO(1:3)='-99'

      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AMAKEABV(ABVFILE,abvarray,abvsyn)

      IMPLICIT NONE
      SAVE

      CHARACTER*254 TL2541, ABVARRAY(1000)
      CHARACTER*132 ABVFILE
      CHARACTER*1   CSWOUT
      INTEGER       TVILENT, ABVSYN, ABVLEN, FILENUM
      INTEGER       FNUMWRK, FILELEN, TVI1
      LOGICAL       FOPEN

      CSWOUT = 'N'

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      FILELEN=TVILENT(abvfile)
      OPEN(filenum,FILE=abvfile(1:FILELEN),STATUS='OLD')
      abvsyn=0
      DO
        tl2541=' '
        READ(filenum,'(A254)',END=7776,ERR=7777)tl2541
        IF(TVILENT(tl2541).LT.3)GOTO 777
        IF(tl2541(1:1).EQ.'!')GOTO 777
        IF(tl2541(1:1).EQ.'@')GOTO 777
        abvlen=tvilent(tl2541)
        IF(abvlen.GT.81)THEN
          abvsyn=abvsyn+1
          IF (abvsyn .EQ. 1000) THEN
            WRITE (fnumwrk,*) ' Abbreviation number is at the limit'
            WRITE (fnumwrk,*) ' If abbreviation # cannot be reduced,'
            WRITE (fnumwrk,*) ' the program will have to be changed'
            WRITE (*,*) ' Abbreviation number is at the limit'
            WRITE (*,*) ' If abbreviation # cannot be reduced,'
            WRITE (*,*) ' the program will have to be changed'
            WRITE (*,*) ' Program will have to stop'
            WRITE (*,*) ' Check WORK.OUT for details of run'
            STOP ' '
          ENDIF
          DO tvi1=1,9
            IF(tl2541(tvi1:tvi1).EQ.' ')EXIT
          ENDDO
          abvarray(abvsyn)=tl2541(1:tvi1)//'  '//tl2541(81:abvlen)
        ENDIF
 777    CONTINUE
      ENDDO
 7776 CONTINUE
 7777 CONTINUE

      CLOSE(filenum)

      IF (CSWOUT.EQ.'Y')
     & WRITE(fnumwrk,*)' Abbreviations with synonym ',abvsyn

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AMAKE1(ADIRFILE,LEVEL,DATARRAY,datanum)

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)   ADIRFILE
      CHARACTER*1000  DATASTD, ABVSTD
      CHARACTER*254   TL2541, ABVLINE, DATALINE, DATARRAY(500)
      CHARACTER*52    TEXTSTD
      CHARACTER*30    TL30, LEVELTMP
      CHARACTER*24    COEFFC, ABVC, COEFFCTM
      CHARACTER*10    LEVEL
      CHARACTER*4     COEFFCHK
      CHARACTER*1     TL1UPCASE, TEXT
      INTEGER         FILENUM, TVILENT, TVI1, TVI2, TVI4
      INTEGER         STARTCOL, LEVELLEN, FILELEN, LENTMP
      INTEGER         TVINSTR, ABVNUM, DATANUM, LENSTD, ABVLEN
      INTEGER         DATANUMS, FNUMWRK, LINE
      LOGICAL         FOPEN

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      ! File
      FILELEN=TVILENT(ADIRFILE)

      ! Level
      LENSTD = 24
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      OPEN(filenum,FILE=ADIRFILE,STATUS='OLD')

      WRITE(fnumwrk,*)'FILE ',ADIRFILE(1:FILELEN)
      WRITE(fnumwrk,*)' LEVEL ',LEVEL(1:LEVELLEN)

      DATANUM=0
      STARTCOL = 1
      ABVLINE=' '

        DO
          tl2541=' '
          READ(filenum,'(A254)',END=9996,ERR=9999)tl2541

          IF(TVILENT(tl2541).LT.3)GOTO 999
          IF(tl2541(1:1).EQ.'!')GOTO 999
          IF(tl2541(1:1).EQ.'$')GOTO 999

          tl30=' '
          TL30=TL2541(1:30)
          CALL Ltrim(tl30)
          tl30(1:1)=Tl1upcase(tl30(1:1))
          tl30(2:2)=Tl1upcase(tl30(2:2))
          IF(TL30(2:2).EQ.' ')TL30(3:3)=' '

          IF(tl2541(1:1).EQ.'*')THEN
            IF(tl2541(2:4).NE.'COE')THEN
              IF(tl2541(2:4).NE.'GEN')startcol=2
            ENDIF
            GO TO 999
          ENDIF

          IF(tl2541(1:1).EQ.'@')THEN
            ! Strip off leading @
            tl2541(1:1)=' '
            ! Strip out leading periods
            ABVLEN=TVILENT(TL2541)
            DO TVI1=1,ABVLEN-1
              IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
            ENDDO
            IF (ABVLINE(2:40).NE.TL2541(2:40))THEN
              ABVLINE(1:1)=' '
              ABVLINE=tl2541
              ABVSTD=' '
              CALL STANDARD(ABVLINE,ABVSTD,'24')
              ABVNUM=TVINSTR(ABVSTD)
              ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE = 40
              IF(ABVNUM.GT.40)ABVNUM=40
              LINE=1
              GO TO 999
            ENDIF
          ENDIF

          DATALINE=tl2541

          IF(ABVNUM.EQ.1)THEN
            IF(line.EQ.1)THEN
              datanum=datanum+1
              CALL LTRIM(abvline)
              CALL LTRIM(dataline)
              DATARRAY(datanum)=abvline(1:6)//' '//dataline(1:248)
              LINE=LINE+1
            ELSE
              CALL LTRIM(dataline)
              tvi1=TVILENT(DATARRAY(datanum))
              tvi2=TVILENT(dataline)
              IF(tvi1+tvi2.GT.241)tvi2=254-tvi1
              DATARRAY(datanum)=
     &        DATARRAY(datanum)(1:tvi1)//' '//dataline(1:tvi2)
            ENDIF
            GO TO 999
          ENDIF

          DATASTD=' '
          TEXTSTD=' '
          CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'24')

          leveltmp=' '
          READ(DATASTD,'(a24)')leveltmp(1:24)

          IF(LEVELTMP(1:LEVELLEN).NE.LEVEL(1:LEVELLEN))GO TO 999
          IF(LEVELTMP(LEVELLEN+1:LEVELLEN+1).NE.' ')GO TO 999

          TEXT='N'

          DO TVI1=1,ABVNUM-(STARTCOL-1)
            IF(TVI1.EQ.1)THEN
              IF(LINE.EQ.1)THEN
                datanums=datanum
              ELSE
                datanum=datanums
              ENDIF
            ENDIF
            datanum=datanum+1
            TVI2 = 1 + ((STARTCOL-1)*LENSTD) + (TVI1-1)*LENSTD
            TVI4=TVI2 + (LENSTD-1)
            COEFFC=DATASTD(TVI2:TVI4)
            ABVC=ABVSTD(TVI2:TVI4)
            COEFFCTM=COEFFC
            CALL LTRIM(COEFFCTM)
            IF (TVILENT(COEFFCTM).LT.1)
     &        COEFFC = '-99         '
            CALL LTRIM(TEXTSTD)
            IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3) = '-99'
            COEFFCHK=COEFFCTM(1:4)

            IF (LINE.EQ.1) THEN
              IF(COEFFCHK.EQ.'TEXT')THEN
                DATARRAY(datanum)=abvc//' '//TEXTSTD
              ELSE
                DATARRAY(datanum)=abvc//' '//coeffc
              ENDIF
              IF(TVI1.EQ.(ABVNUM-(STARTCOL-1)))LINE=LINE+1
            ELSE
              LENTMP=TVILENT(DATARRAY(datanum))
              IF(COEFFCHK.EQ.'TEXT')THEN
                TL2541=DATARRAY(datanum)(1:LENTMP)//' '//TEXTSTD
                DATARRAY(datanum)=TL2541
              ELSE
                TL2541=
     &          DATARRAY(datanum)(1:LENTMP)//' '//coeffc
                DATARRAY(datanum)=TL2541
              ENDIF
            ENDIF

          ENDDO

  999     CONTINUE

        END DO

 9996 CONTINUE
 9998 CONTINUE
 9999 CONTINUE

      CLOSE(filenum)

      DO TVI1 = 1,DATANUM
        WRITE(fnumwrk,'(I6,A2,A60)')tvi1,'  ',DATARRAY(TVI1)(1:60)
      ENDDO

      IF (DATANUM .LE. 0)THEN
        WRITE(fnumwrk,*)' Problem in AMAKE1!'
        WRITE(fnumwrk,*)' Likely that level not found.'
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AMAKER0(SPDIRFLT,datarray,datanum)

      IMPLICIT NONE
      SAVE

      CHARACTER*254    DATARRAY(500), TL2541
      CHARACTER*93     SPDIRFLT
      CHARACTER*80     TL0801
      INTEGER          DATANUM, FILENUM, TVI1, SIZE, FNUMWRK
      LOGICAL          FFLAG, FOPEN

      INTEGER istat

      SIZE = 500

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      OPEN(filenum,FILE=spdirflt,STATUS='OLD')
      tl0801=' '

      WRITE(fnumwrk,*)'FILE ',SPDIRFLT(1:50)

      TVI1 = 0
      FFLAG = .FALSE.
      DO WHILE(.NOT.fflag)
        READ(filenum,'(A254)',END=9996,ERR=9999,iostat=istat)tl2541
        IF (tl2541(1:1).NE.'$') THEN
          IF (tl2541(1:1).NE.'*') THEN
            IF (tl2541(1:1).NE.'@') THEN
              IF (tl2541(1:1).NE.'!') THEN
                IF (tl2541(1:6).NE.'      ') THEN
                  TVI1=TVI1+1
                  IF(TVI1.GE.SIZE)THEN
                    WRITE(fnumwrk,*)
     &              ' Too many coefficients in file: ',spdirflt
                    WRITE(fnumwrk,*)' Size permitted is :',size
                    WRITE(*,*)
     &               ' Too many coefficients in file: ',spdirflt(1:40)
                    WRITE (*,*) ' Size permitted is :',size
                    WRITE (*,*) ' Program will have to stop'
                    WRITE (*,*) ' Check WORK.OUT for details of run'
                    STOP ' '
                  ENDIF
                  CALL LTRIM(TL2541)
                  datarray(tvi1)=tl2541
                  WRITE(fnumwrk,'(A2,I4,A2,A60)')
     &             '  ',TVI1,'  ',TL2541(1:60)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF (istat.ne.0)fflag = .TRUE.
      ENDDO

 9996 CONTINUE
 9998 CONTINUE
 9999 CONTINUE

      CLOSE(FILENUM)

      DATARRAY(TVI1+1)='-9999'
      datanum=tvi1

      IF (DATARRAY(1) .EQ. '      ')THEN
        WRITE (fnumwrk,*) ' Problem reading: ',spdirflt(1:60)
        WRITE (fnumwrk,*) ' Datanum: ',datanum
        WRITE (fnumwrk,*) ' Datarray:'
        DO TVI1 = 1, DATANUM
          WRITE(fnumwrk,*)datarray(tvi1)
        ENDDO
        WRITE (*,*) ' Problem reading: ',spdirflt(1:60)
        WRITE (*,*) ' Datanum: ',datanum
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE AMAKE5(ADIRFILE,LEVEL,RN,SN,ON,CN,DATARRAY,datanum)

      IMPLICIT NONE
      SAVE

      CHARACTER*(*)   ADIRFILE
!CHP 1/30/2006      CHARACTER*600   DATASTD, ABVSTD
      CHARACTER*1000   DATASTD, ABVSTD
      CHARACTER*254   TL2541, ABVLINE, DATALINE, DATARRAY(500)
      CHARACTER*52    TEXTSTD
      CHARACTER*30    TL30, LEVELTMP
      CHARACTER*12    COEFFC, ABVC, COEFFCTM, VALUE
      CHARACTER*10    LEVEL
      CHARACTER*4     COEFFCHK
      CHARACTER*1     TL1UPCASE, TEXT
      INTEGER         FILENUM, TVILENT, TVI1, TVI2, TVI4
      INTEGER         STARTCOL, LEVELLEN, FILELEN, LENTMP
      INTEGER         TVINSTR, ABVNUM, DATANUM, LENSTD, ABVLEN
      INTEGER         DATANUMS, FNUMWRK, LINE, TVICOLNM, TVIFROMC
      INTEGER         RNCOL, SNCOL, ONCOL, CNCOL, RN, SN, ON, CN
      INTEGER         RNVAL, SNVAL, ONVAL,CNVAL
      LOGICAL         FOPEN

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      ! Currently SN, ON, AND CN are not currently used .. but will be!
      ! To avoid warning the following dummy statements ahve been used
      TVI1 = SN
      TVI1 = ON
      TVI1 = CN
      TVI1 = 0

      ! File
      FILELEN=TVILENT(ADIRFILE)

      ! Level
      LENSTD = 12
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      OPEN(filenum,FILE=ADIRFILE,STATUS='OLD')

      WRITE(fnumwrk,*)'FILE ',ADIRFILE(1:FILELEN)
      WRITE(fnumwrk,*)' LEVEL ',LEVEL(1:LEVELLEN)

      DATANUM=0
      STARTCOL = 1
      ABVLINE=' '

        DO
          tl2541=' '
          READ(filenum,'(A254)',END=9996,ERR=9999)tl2541

          IF(TVILENT(tl2541).LT.3)GOTO 999
          IF(tl2541(1:1).EQ.'!')GOTO 999
          IF(tl2541(1:1).EQ.'$')GOTO 999

          tl30=' '
          TL30=TL2541(1:30)
          CALL Ltrim(tl30)
          tl30(1:1)=Tl1upcase(tl30(1:1))
          tl30(2:2)=Tl1upcase(tl30(2:2))
          IF(TL30(2:2).EQ.' ')TL30(3:3)=' '

          IF(tl2541(1:1).EQ.'*')THEN
            IF(tl2541(2:4).NE.'GEN')startcol=2
            GO TO 999
          ENDIF

          IF(tl2541(1:1).EQ.'@')THEN
            ! Strip off leading @
            tl2541(1:1)=' '
            ! Strip out leading periods
            ABVLEN=TVILENT(TL2541)
            DO TVI1=1,ABVLEN-1
              IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
            ENDDO
            IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
              ABVLINE(1:1)=' '
              ABVLINE=tl2541
              ABVSTD=' '
              CALL STANDARD(ABVLINE,ABVSTD,'12')
              ABVNUM=TVINSTR(ABVSTD)
              ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE = 40
              IF(ABVNUM.GT.40)ABVNUM=40
              LINE=1
              GO TO 999
            ENDIF
          ENDIF

          DATALINE=tl2541

          IF(ABVNUM.EQ.1)THEN
            IF(line.EQ.1)THEN
              datanum=datanum+1
              CALL LTRIM(abvline)
              CALL LTRIM(dataline)
              DATARRAY(datanum)=abvline(1:6)//' '//dataline(1:248)
              LINE=LINE+1
            ELSE
              CALL LTRIM(dataline)
              tvi1=TVILENT(DATARRAY(datanum))
              tvi2=TVILENT(dataline)
              IF(tvi1+tvi2.GT.241)tvi2=254-tvi1
              DATARRAY(datanum)=
     &        DATARRAY(datanum)(1:tvi1)//' '//dataline(1:tvi2)
            ENDIF
            GO TO 999
          ENDIF

          DATASTD=' '
          TEXTSTD=' '
          CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'12')

          leveltmp=' '
          READ(DATASTD,'(a12)')leveltmp(1:12)

          IF(LEVELTMP(1:LEVELLEN).NE.LEVEL(1:LEVELLEN))GO TO 999
          IF(LEVELTMP(LEVELLEN+1:LEVELLEN+1).NE.' ')GO TO 999

          ! NOTE. Not working for SN,ON,CN
          RNCOL=Tvicolnm(ABVLINE,'RN ')
          SNCOL=Tvicolnm(ABVLINE,'SN ')
          ONCOL=Tvicolnm(ABVLINE,'ON ')
          CNCOL=Tvicolnm(ABVLINE,'CN ')
          IF (RNCOL.GT.0)THEN
            CALL Getstr(datastd,rncol,value)
            RNVAL = Tvifromc(value)
            IF (RNVAL.NE.RN) GO TO 999
          ENDIF
          IF (SNCOL.GT.0)THEN
            CALL Getstr(datastd,sncol,value)
            SNVAL = Tvifromc(value)
            ! IF (SNVAL.NE.SN) GO TO 999
          ENDIF
          IF (ONCOL.GT.0)THEN
            CALL Getstr(datastd,oncol,value)
            ONVAL = Tvifromc(value)
            !IF (ONVAL.NE.ON) GO TO 999
          ENDIF
          IF (CNCOL.GT.0)THEN
            CALL Getstr(datastd,cncol,value)
            CNVAL = Tvifromc(value)
            ! IF (CNVAL.NE.CN) GO TO 999
          ENDIF

          TEXT='N'

          DO TVI1=1,ABVNUM-(STARTCOL-1)
            IF(TVI1.EQ.1)THEN
              IF(LINE.EQ.1)THEN
                datanums=datanum
              ELSE
                datanum=datanums
              ENDIF
            ENDIF
            datanum=datanum+1
            TVI2 = 1 + ((STARTCOL-1)*LENSTD) + (TVI1-1)*LENSTD
            TVI4=TVI2 + (LENSTD-1)
            COEFFC=DATASTD(TVI2:TVI4)
            ABVC=ABVSTD(TVI2:TVI4)
            COEFFCTM=COEFFC
            CALL LTRIM(COEFFCTM)
            IF (TVILENT(COEFFCTM).LT.1)
     &        COEFFC = '-99         '
            CALL LTRIM(TEXTSTD)
            IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3) = '-99'
            COEFFCHK=COEFFCTM(1:4)

            IF (LINE.EQ.1) THEN
              IF(COEFFCHK.EQ.'TEXT')THEN
                DATARRAY(datanum)=abvc//' '//TEXTSTD
              ELSE
                DATARRAY(datanum)=abvc//' '//coeffc
              ENDIF
              IF(TVI1.EQ.(ABVNUM-(STARTCOL-1)))LINE=LINE+1
            ELSE
              LENTMP=TVILENT(DATARRAY(datanum))
              IF(COEFFCHK.EQ.'TEXT')THEN
                TL2541=DATARRAY(datanum)(1:LENTMP)//' '//TEXTSTD
                DATARRAY(datanum)=TL2541
              ELSE
                TL2541=
     &          DATARRAY(datanum)(1:LENTMP)//' '//coeffc
                DATARRAY(datanum)=TL2541
              ENDIF
            ENDIF

          ENDDO

  999     CONTINUE

        END DO

 9996 CONTINUE
 9998 CONTINUE
 9999 CONTINUE

      CLOSE(filenum)

      DO TVI1 = 1,DATANUM
        WRITE(fnumwrk,'(I6,A2,A60)')tvi1,'  ',DATARRAY(TVI1)(1:60)
      ENDDO

      IF (DATANUM .LE. 0)THEN
        WRITE(fnumwrk,*)' Problem reading: ',ADIRFILE
        WRITE(fnumwrk,*)' Likely that level not found.'
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADT(SLDIRFLE,SLCODE,CODE,TXTO)

      IMPLICIT NONE
      SAVE

      CHARACTER*(*) SLDIRFLE, SLCODE, CODE, TXTO
      CHARACTER*254 DATARRAY(500), TL2541, ABVLINE, DATALINE
      CHARACTER*254 ABVARRAY(1000)
!chp1/30/2006      CHARACTER*600 DATASTD, ABVSTD
      CHARACTER*1000 DATASTD, ABVSTD
      CHARACTER*128 ARG
      CHARACTER*132 ABVFILE
      CHARACTER*80  TL0801
      CHARACTER*120 CFGDFILE
      CHARACTER*52  TEXTSTD, tl0521
      CHARACTER*30  TL30
      CHARACTER*12  COEFFC(200), ABVC(200), COEFFCTM, SYNCODE
      CHARACTER*11  GROUP
      CHARACTER*10  SLCODEP
      CHARACTER*4   COEFFCHK
      CHARACTER*1   TL1UPCASE, TEXT, BLANK
      INTEGER       TVILENT, TVI1, TVI2, TVI0, TVI3, TVI4
      INTEGER       FILENUM, GROUPLEN, FILELEN, LINE, LENTMP
      INTEGER       TVINSTR, ABVNUM, LENSTD, ABVLEN, DATANUM
      INTEGER       ARGLEN, CODENUM, ABVSYN, FNUMWRK, L
      LOGICAL       FFLAG,FOPEN

      PARAMETER     (BLANK = ' ')

!      SAVE          FNUMWRK
!      SAVE          SLCODEP


      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      arg = ' '
      tvi2 = 0
      tvi3 = 0
      CALL GETARG (0,arg)
      IF (arglen.GT.100) ARGLEN = TVILENT(ARG)
      DO tvi1 = 1,arglen
        IF (arg(tvi1:tvi1).EQ.'/') tvi2=tvi1
        IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1-1
      ENDDO
      IF (tvi3.EQ.0) tvi3 = arglen
      cfgdfile=ARG(1:TVI3)//'.CFG'
      ! Change cfg file name from module specific to general
      DO L = LEN(TRIM(CFGDFILE)),1,-1
        IF (CFGDFILE(L:L).EQ.'/') EXIT
      ENDDO
      IF (L.GT.1) THEN
        cfgdfile = CFGDFILE(1:L-1)//'/'//'CROPSIM.CFG'
      ELSE
        cfgdfile(1:11) = 'CROPSIM.CFG'
      ENDIF

      abvfile=ARG(1:TVI2)//'SOIL.CDE'

      LENSTD = 12
      CODENUM=1
      SYNCODE=' '

      IF (LEN(TRIM(SLCODE)).LT.3 .OR. SLCODE(1:3).EQ.'   ') THEN
        WRITE (fnumwrk,*) ' No soil code in soil reads!! '
        WRITE (*,*) ' No soil code in soil reads!! '
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      IF(SLCODE(1:10).EQ.SLCODEP(1:10))GO TO 5555
      WRITE(fnumwrk,*)' SOIL FILENAME ',SLDIRFLE(1:60)
      WRITE(fnumwrk,*)' SOIL CODE     ',SLCODE
      WRITE(fnumwrk,*)' '

      FILELEN=TVILENT(SLDIRFLE)

      GROUP(1:1)='*'
      GROUP(2:11)=SLCODE(1:10)
      GROUPLEN=TVILENT(GROUP)

      OPEN(filenum,FILE=SLDIRFLE(1:FILELEN),STATUS='OLD')

      tl0801=' '
      DO WHILE(tl0801(1:GROUPLEN).NE.GROUP(1:GROUPLEN))
        READ(filenum,'(A80)',END=9998,ERR=9999)tl0801
        IF (tl0801(2:6).EQ.'SOIL:')THEN
          tl0801(1:5) = '     '
          tl0801(6:6) = '*'
        ENDIF
        CALL Ltrim(tl0801)
        tl0801(1:1)=Tl1upcase(tl0801(1:1))
        tl0801(2:2)=Tl1upcase(tl0801(2:2))
        IF(tl0801(1:GROUPLEN).EQ.GROUP(1:GROUPLEN))EXIT
      END DO

      WRITE(fnumwrk,*)'FILE ',SLDIRFLE(1:FILELEN)
      WRITE(fnumwrk,*)' GROUP ',GROUP(1:GROUPLEN)


      DO L = 1,500
        DATARRAY = ' '
      ENDDO
      DATANUM = 0
      ABVNUM = 0
      LINE=0
      TVI0=0
      TVI1=0

      DO

        tl2541=' '
        tl30=' '
        LINE=LINE+1
        DO
          READ(filenum,'(A254)',END=999,ERR=9999)tl2541
          IF(tl2541(1:1).EQ.'*')GOTO 999
          IF(tl2541(1:1).EQ.'!')GOTO 888
          IF(tvilent(tl2541).LT.3)GOTO 888
          IF(tl2541(1:1).EQ.'@')THEN
            ! Strip out leading periods
            ABVLEN=TVILENT(TL2541)
            DO TVI1=1,ABVLEN-1
              IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
            ENDDO
            IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
              TVI0=TVI0+ABVNUM
              ABVLINE=tl2541
              LINE=1
            ENDIF
            GO TO 888
          ENDIF
          EXIT
  888     CONTINUE
        END DO

        DATALINE=tl2541

        TEXT='N'

        ABVSTD=' '
        DATASTD=' '
        tl0521=' '
        TEXTSTD=' '
        COEFFC(TVI0+1)=' '

        CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'12')
        IF(ABVLINE(1:1).EQ.'@')ABVLINE(1:1)=' '
        CALL STANDARD(ABVLINE,ABVSTD,'12')
        ABVNUM=TVINSTR(ABVSTD)
        ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE = 40
        IF(ABVNUM.GT.40)ABVNUM=40
        DO TVI1=1,ABVNUM
          TVI2= 1 + (TVI1-1)*LENSTD
          TVI4=TVI2 + (LENSTD-1)
          COEFFC(TVI0+TVI1)=DATASTD(TVI2:TVI4)
          ABVC(TVI0+TVI1)=ABVSTD(TVI2:TVI4)
          COEFFCTM=COEFFC(TVI0+TVI1)
          CALL LTRIM(COEFFCTM)
          IF (TVILENT(COEFFCTM).LT.1)COEFFC(TVI0+TVI1) = '-99         '
          CALL LTRIM(TEXTSTD)
          IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3) = '-99'
          COEFFCHK=COEFFCTM(1:4)
          IF (LINE.EQ.1) THEN
           IF(COEFFCHK.EQ.'TEXT')THEN
            datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//TEXTSTD
           ELSE
            datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//coeffc(TVI0+tvi1)
           ENDIF
          ELSE
           LENTMP=TVILENT(DATARRAY(TVI0+TVI1))
           IF(COEFFCHK.EQ.'TEXT')THEN
            TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//TEXTSTD
            datarray(TVI0+tvi1)=TL2541
           ELSE
            TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//coeffc(TVI0+tvi1)
            datarray(TVI0+tvi1)=TL2541
           ENDIF
          ENDIF
        ENDDO

        datanum=tvi0+abvnum

      ENDDO

  999 CONTINUE

      CLOSE(FILENUM)

      DO TVI3 = 1,datanum
        WRITE(fnumwrk,'(I6,A2,A60)')tvi3,'  ',datarray(TVI3)(1:60)
      enddo

      SLCODEP(1:10)=SLCODE(1:10)

      abvfile=' '
      abvfile(1:8)='SOIL.CDE'
      INQUIRE(FILE=abvfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
        CALL GETARG(0,arg)
        DO tvi1 = 1,arglen
          IF(arg(tvi1:tvi1).EQ.'/')tvi2=tvi1
        ENDDO
        abvfile=ARG(1:TVI2)//'SOIL.CDE'
        INQUIRE(FILE=abvfile,EXIST=fflag)
      ENDIF
      IF(fflag)THEN
        ! Read abbreviation file
        WRITE(fnumwrk,*)' Reading abbrev file: ',abvfile(1:55)
        CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
      ELSE
        WRITE(fnumwrk,*)' No abbreviation file: ',abvfile(1:55)
        abvsyn=0
      ENDIF

 5555 CONTINUE
      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
      !WRITE(FNUMWRK,*)'  SLREAD ',CODE,' ',TXTO(1:40)

      RETURN

 9998 CONTINUE
      CLOSE(filenum)
      WRITE (fnumwrk,*) ' Did not find soil: ',slcode
      WRITE (fnumwrk,*) ' File: ',sldirfle(1:50)
      WRITE (*,*) ' Did not find soil: ',slcode
      WRITE (*,*) ' File: ',sldirfle(1:50)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details of run'
      STOP ' '

 9999 CONTINUE
      CLOSE(filenum)
      WRITE (fnumwrk,*) ' Problem reading: ',sldirfle(1:50)
      WRITE (*,*) ' Problem reading: ',sldirfle(1:50)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details of run'
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADC(SLDIRFLE,SLCODE,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE; SAVE

      CHARACTER*(*)  SLDIRFLE, SLCODE, CODE, CHAROUT
      CHARACTER*254  TL2541, TXTO
      CHARACTER*100  CODENEW
      CHARACTER*20   COEFF
      CHARACTER*1    FOUND
      INTEGER        CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
      INTEGER        TVI3, CODECLEN, FNUMWRK
      LOGICAL        FOPEN

      CALL SLREADT(SLDIRFLE,SLCODE,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3 = 1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CALL Getlun('WORK.OUT',fnumwrk)
         INQUIRE (FILE='Work.out',OPENED=fopen)
         IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
         WRITE (fnumwrk,*) ' Could not find code: ',Code
         WRITE (fnumwrk,*) ' File was: ',sldirfle(1:50)
         WRITE (*,*) ' Could not find code: ',Code
         WRITE (*,*) ' File was: ',sldirfle(1:50)
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check WORK.OUT for details of run'
         STOP ' '
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADR(SLDIRFLE,SLCODE,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE; SAVE

      CHARACTER*(*)  SLDIRFLE, SLCODE, CODE
      CHARACTER*30   CHAROUT
      REAL           VALUEOUT, TVRFROMC

      CALL SLREADC(SLDIRFLE,SLCODE,CODE,charout)
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADCA(SLDIRFLE,SLCODE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE; SAVE

      CHARACTER*(*)   SLDIRFLE, SLCODE, CODE, AOUTSIZE, ARRAYOUT(*)
      CHARACTER*354   TL3541
      CHARACTER*254   TL2541,TXTO
      CHARACTER*100   CODENEW
      REAL            TVR1, TVRFROMC
      INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE, LENARVAR

      CALL SLREADT(SLDIRFLE,SLCODE,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      TVR1 = TVRFROMC(AOUTSIZE)
      SIZE = NINT(TVR1)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=' '
      ENDDO

      LENARVAR = LEN(arrayout(1))

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,SIZE
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+LENARVAR)
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SLREADRA(SLDIRFLE,SLCODE,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE; SAVE

      CHARACTER*(*)   SLDIRFLE, SLCODE, CODE, AOUTSIZE
      CHARACTER*354   TL3541
      CHARACTER*254   TL2541,TXTO
      CHARACTER*100   CODENEW
      REAL            TVR1, TVRFROMC, ARRAYOUT(*)
      INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE, I

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)
      IF(CODENEW(1:5).EQ.'DLAYR')THEN
        CALL SLREADT(SLDIRFLE,SLCODE,'SLB',TXTO)
        CODELEN=3
      ELSE
        CALL SLREADT(SLDIRFLE,SLCODE,CODE,TXTO)
        CALL LTRIM2(CODE,codenew)
        CODELEN=Tvilent(CODEnew)
      ENDIF

      TVR1 = TVRFROMC(AOUTSIZE)
      SIZE = NINT(TVR1)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,SIZE
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      IF(CODENEW(1:5).EQ.'DLAYR')THEN
        DO I = SIZE,2,-1
          IF (ARRAYOUT(I) .GT. ARRAYOUT(I-1)) THEN
            ARRAYOUT(I) = ARRAYOUT(I) - ARRAYOUT(I-1)
          ENDIF
        ENDDO
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION SLLAYERS(ARRAY,ASIZE)

      ! Returns number of layers in soil from array of layer bases

      IMPLICIT NONE; SAVE

      CHARACTER*(*)  ASIZE
      REAL           ARRAY(*), TVR1, TVRFROMC
      INTEGER        SIZE, SLLAYERS, I

      TVR1 = TVRFROMC(ASIZE)
      SIZE = INT(TVR1)

      SLLAYERS=1
      DO I = 2, SIZE
        IF (ARRAY(I) .GT. ARRAY(I-1)) THEN
          SLLAYERS = SLLAYERS + 1
        ELSE
          EXIT
        ENDIF
      ENDDO

      IF (ARRAY(1).LE.0.0) SLLAYERS = 0

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION NUMLAYER(ARRAY,ASIZE)

      ! Returns number of layers in soil profile from array of depths

      IMPLICIT NONE; SAVE

      CHARACTER*(*)  ASIZE
      REAL           ARRAY(*), TVR1, TVRFROMC
      INTEGER        SIZE, NUMLAYER, I

      TVR1 = TVRFROMC(ASIZE)
      SIZE = INT(TVR1)

      NUMLAYER=1
      DO I = 2, SIZE
        IF (ARRAY(I).GT.0.0) THEN
          NUMLAYER = NUMLAYER + 1
        ELSE
          EXIT
        ENDIF
      ENDDO

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION SLDEPTH(ARRAY,ASIZE)

      ! Returns depth of soil profile

      IMPLICIT NONE; SAVE

      CHARACTER*(*)  ASIZE
      REAL           ARRAY(*), TVR1, TVRFROMC
      INTEGER        SIZE, SLDEPTH, I

      TVR1 = TVRFROMC(ASIZE)
      SIZE = NINT(TVR1)

      SLDEPTH=ARRAY(1)
      DO I = 2, SIZE
        IF (ARRAY(I) .GT. ARRAY(I-1)) THEN
          SLDEPTH = SLDEPTH + ARRAY(I)
        ENDIF
      ENDDO

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADT(MODEL,CODE,TXTO)

      IMPLICIT NONE; SAVE

      CHARACTER*(*)   TXTO, CODE
      CHARACTER*254   DATARRAY(500), ABVARRAY(1000)
      CHARACTER*128   ARG
      character*100   CODENEW
      CHARACTER*132   ABVFILE
      CHARACTER*93    SMDIRFLE
      CHARACTER*12    SMFILE
      CHARACTER*8     MODEL, MODELP
      INTEGER         CODELEN, TVI2, TVI3, DATANUM
      INTEGER         TVILENT, TVI1
      INTEGER         ARGLEN, FNUMWRK, ABVSYN
      LOGICAL         FFLAG, FOPEN

!      SAVE            MODELP

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)

      IF(MODEL.EQ.MODELP)THEN
        GOTO 5555
      ENDIF

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF

      ! FILE NAME
      CALL GETARG(0,arg)
      DO tvi1 = 1,arglen
        IF(arg(tvi1:tvi1).EQ.'/')tvi2=tvi1
        IF(arg(tvi1:tvi1).EQ.'.')tvi3=tvi1
      ENDDO
      smfile=model(1:8)//'.SOM'
      ! Establish location
      INQUIRE(FILE=smfile,EXIST=fflag)
      IF (fflag) THEN
        smdirfle=smfile
      ELSE
        smdirfle=ARG(1:TVI2)//SMFILE
        INQUIRE(FILE=smdirfle,EXIST=fflag)
        IF (.NOT.fflag) THEN
          WRITE (fnumwrk,*) ' SOM coefficients file not found!!'
          WRITE (fnumwrk,*) ' File sought was: ',smdirfle(1:50)
          WRITE (*,*) ' SOM coefficients file not found!!'
          WRITE (*,*) ' File sought was: ',smdirfle(1:50)
          WRITE (*,*) ' Program will have to stop'
          WRITE (*,*) ' Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
      ENDIF

      CALL AMAKER0(SMDIRFLE,datarray,datanum)
      IF (DATANUM .LE. 0)THEN
        WRITE (fnumwrk,*) ' Problem reading: ',SMDIRFLE(1:50)
        WRITE (fnumwrk,*) ' Likely that level not found.'
        WRITE (*,*) ' Problem reading: ',SMDIRFLE(1:50)
        WRITE (*,*) ' Likely that level not found.'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      abvfile=' '
      abvfile(1:10)='SOM.CDE'
      INQUIRE(FILE=abvfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
        CALL GETARG(0,arg)
        DO tvi1 = 1,arglen
          IF(arg(tvi1:tvi1).EQ.'/')tvi2=tvi1
        ENDDO
        abvfile=ARG(1:TVI2)//'SOM.CDE'
        INQUIRE(FILE=abvfile,EXIST=fflag)
      ENDIF
      IF(fflag)THEN
        ! Read abbreviation file
        WRITE(fnumwrk,*)' Reading abbrev file: ',abvfile(1:55)
        CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
      ELSE
        WRITE(fnumwrk,*)' No abbreviation file: ',abvfile(1:55)
        abvsyn=0
      ENDIF

 5555 CONTINUE

      CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)

      modelp=model

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADC(MODEL,CODE,charout)

      ! Returns one character string for particular code

      IMPLICIT NONE; SAVE

      CHARACTER*(*)  CODE, CHAROUT, MODEL
      CHARACTER*254  TXTO, TL2541
      CHARACTER*100  CODENEW
      CHARACTER*20   COEFF
      CHARACTER*1    FOUND
      INTEGER        CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
      INTEGER        TVI3, CODECLEN, FNUMWRK
      LOGICAL        FOPEN

      CALL SMREADT(MODEL,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODENEW)
      CODECLEN=LEN(CHAROUT)

      DO TVI1=1,CODECLEN
        CHAROUT(TVI1:TVI1)=' '
      ENDDO

      DO TVI1=1,20
        Coeff(TVI1:TVI1)=' '
      ENDDO

      FOUND='N'
      STARTPOS=1
      ENDPOS=0

      TL2541=TXTO
      CALL Ltrim(TL2541)

      DO TVI3 = 1, 100
        IF(TL2541(TVI3:TVI3).EQ.' ')THEN
          ENDPOS=TVI3
          FOUND='Y'
          GO TO 1000
        ENDIF
      ENDDO
 1000 CONTINUE
      IF(FOUND.EQ.'Y')THEN
        LENGTH=ENDPOS-STARTPOS+1
        COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
        CHAROUT=' '
        IF (CODECLEN.LT.20)THEN
          CHAROUT=COEFF(1:CODECLEN)
        ELSE
          CHAROUT(1:20)=COEFF
        ENDIF
       ELSE
         CALL Getlun('WORK.OUT',fnumwrk)
         INQUIRE (FILE='Work.out',OPENED=fopen)
         IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
         WRITE (fnumwrk,*) ' Could not find code: ',Code
         WRITE (fnumwrk,*) ' Working with SOM parameter file'
         WRITE (*,*) ' Could not find code: ',Code
         WRITE (*,*) ' Working with SOM parameter file'
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check WORK.OUT for details of run'
         STOP ' '
       ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADR(MODEL,CODE,valueout)

      ! Returns one real value for particular code

      IMPLICIT NONE; SAVE

      CHARACTER*(*) CODE, MODEL
      CHARACTER*30  CHAROUT
      REAL          VALUEOUT, TVRFROMC

      CALL SMREADC(MODEL,CODE,charout)
      VALUEOUT=TVRFROMC(Charout(1:12))

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADRA(MODEL,CODE,AOUTSIZE,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE; SAVE

      CHARACTER*(*)   MODEL, CODE, AOUTSIZE
      CHARACTER*354   TL3541
      CHARACTER*254   TL2541,TXTO
      CHARACTER*100   CODENEW
      REAL            TVR1, TVRFROMC, ARRAYOUT(*)
      INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE

      CALL SMREADT(MODEL,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      TVR1 = TVRFROMC(AOUTSIZE)
      SIZE = NINT(TVR1)

      DO TVI1 = 1,SIZE
        ARRAYOUT(TVI1)=-99
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,SIZE
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE SMREADR2(MODEL,CODE,DIMVAL,arrayout)

      ! Returns ARRAY for particular code

      IMPLICIT NONE; SAVE

      CHARACTER*(*)   CODE, DIMVAL, MODEL
      CHARACTER*354   TL3541
      CHARACTER*254   TL2541,TXTO
      CHARACTER*100   CODENEW
      INTEGER         CODELEN, TVI1, TVILENT, L, L2, ARRAYVAL, TVI2
      REAL            ARRAYOUT(0:20,3), TVRFROMC, DIMVALR

      CALL SMREADT(MODEL,CODE,TXTO)

      CALL LTRIM2(CODE,codenew)
      CODELEN=Tvilent(CODEnew)

      DIMVALR=TVRFROMC(DIMVAL)
      IF(DIMVALR.LT.2.0)THEN
        ARRAYVAL=NINT(10.0*(DIMVALR-1.0))
      ELSE
        ARRAYVAL=NINT(10.0*(DIMVALR-2.0))
      ENDIF

      DO TVI1 = 0,20
        DO TVI2 = 1,3
          ARRAYOUT(TVI1,tvi2)=-99
        enddo
      ENDDO

      TL2541=TXTO
      CALL Standard(TL2541,TL3541,'7')
      DO L = 1,3
         L2 = (L-1)*7 + 1
         IF (TL3541(L2:L2).EQ.'!')GO TO 1000
         IF (TL3541(L2:L2).EQ.' ')GO TO 1000
         IF(DIMVALR.LT.2)THEN
           ARRAYOUT(ARRAYVAL,L)=TVRFROMC(TL3541(L2:L2+5))
         ELSE
           ARRAYOUT(L,ARRAYVAL)=TVRFROMC(TL3541(L2:L2+5))
         ENDIF
      END DO
 1000 CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE WTHDATA(FILEIO,WTHSTA,FTYPE,wthdirfl)

      IMPLICIT NONE; SAVE

      CHARACTER*(*) WTHSTA
      CHARACTER*128 ARG
      CHARACTER*120 CFGDFILE, FILEIO
      CHARACTER*80  PATHWTH
      CHARACTER*64  WTHDIRFL
      CHARACTER*12  WTHFLE, WTHTFLE
      CHARACTER*4   TL4
      CHARACTER*3   CFGCODE,FTYPE
      CHARACTER*1   WTHFLAG
      INTEGER       TVI1, TVI2, TVI3, TVILENT, ARGLEN, L
      INTEGER       FNUMWRK, FILENUM
      LOGICAL       FFLAG, FOPEN

      tvi2 = 0

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      WRITE(FNUMWRK,*)' WTHSTA ',WTHSTA

      arg = ' '
      tvi2 = 0
      tvi3 = 0
      arglen = 0
      CALL GETARG (0,arg)
      DO tvi1 = 1,arglen
        IF (arg(tvi1:tvi1).EQ.'/') tvi2=tvi1
        IF (arg(tvi1:tvi1).EQ.'.') tvi3=tvi1-1
      ENDDO
      IF (tvi3.EQ.0) tvi3 = arglen
      cfgdfile=ARG(1:TVI3)//'.CFG'
      ! Change cfg file name from module specific to general
      DO L = LEN(TRIM(CFGDFILE)),1,-1
        IF (CFGDFILE(L:L).EQ.'/') EXIT
      ENDDO
      IF (L.GT.1) THEN
        cfgdfile = CFGDFILE(1:L-1)//'/'//'CROPSIM.CFG'
      ELSE
        cfgdfile(1:11) = 'CROPSIM.CFG'
      ENDIF

      DO L = LEN(TRIM(FILEIO)),1,-1
        IF (FILEIO(L:L).EQ.'/') EXIT
      ENDDO
      IF (L.GT.1) THEN
        PATHWTH=' '
        PATHWTH = FILEIO(1:L-1)
      ELSE
        PATHWTH=' '
      ENDIF

      cfgcode='WED'
      tl4='.'//FTYPE
      wthdirfl=' '

      ! Specific file
      tvi1=Tvilent(wthsta)
      tvi1=MIN(8,tvi1)
      wthtfle=wthsta(1:tvi1)//tl4

      ! Determine if 'special' weather file
      wthflag = 'N'
      IF (WTHSTA(TVI1-1:TVI1).NE.'01') wthflag = 'Y'

      ! General file
      tvi1=MIN(4,tvi1)
      wthfle=wthsta(1:tvi1)//tl4

      tvi1=Tvilent(wthfle)
      IF(tvi1.LT.6)THEN
       WRITE (fnumwrk,*) ' No weather file nor station defined!! '
       WRITE (*,*) ' No weather file nor station defined!! '
       WRITE (*,*) ' Program will have to stop'
       WRITE (*,*) ' Check WORK.OUT for details of run'
       STOP ' '
      ENDIF

      IF (WTHFLAG.EQ.'Y') THEN
        WTHDIRFL = FILEIO(1:L-1)//'/'//wthtfle
        INQUIRE(FILE=wthdirfl,EXIST=fflag)
        IF(fflag)THEN
          WRITE(fnumwrk,*)
     &     ' Specific file in working dir: ',wthdirfl(1:45)
          RETURN
        ELSE
          WRITE(fnumwrk,*)' Specific file not in working directory'
          WRITE(fnumwrk,*)' File sought was ',wthdirfl(1:60)
        ENDIF
      ENDIF

      WTHDIRFL = FILEIO(1:L-1)//'/'//wthfle
      INQUIRE(FILE=wthdirfl,EXIST=fflag)

      IF(.NOT.fflag)THEN
       WRITE(fnumwrk,*)' General file not in working directory'
       WRITE(fnumwrk,*)' File sought was ',wthdirfl(1:60)
       WTHDIRFL = FILEIO(1:L-1)//'/'//wthtfle
       INQUIRE(FILE=wthdirfl,EXIST=fflag)
       IF(fflag)THEN
         WRITE(fnumwrk,*)
     &    ' Specific file in working dir: ',wthdirfl(1:45)
       ELSE
         WRITE(fnumwrk,*)' Specific file not in working directory'
         WRITE(fnumwrk,*)' File sought was ',wthdirfl(1:60)
       ENDIF
      ENDIF

      IF (.NOT.fflag) THEN
       WRITE(fnumwrk,*)' Checking configured directories'
       WRITE(fnumwrk,*)' Config.file: ',cfgdfile(1:60)
       CALL Finddir(filenum,cfgdfile,cfgcode,wthfle,wthdirfl)
       INQUIRE(FILE=wthdirfl,EXIST=fflag)
       IF (.NOT.fflag) THEN
         CALL Finddir(filenum,cfgdfile,cfgcode,wthtfle,wthdirfl)
         INQUIRE(FILE=wthdirfl,EXIST=fflag)
         IF (.NOT.fflag) THEN
           WRITE (fnumwrk,*) ' Weather file not found!'
           WRITE (fnumwrk,*) ' Station was     : ',wthfle
           WRITE (fnumwrk,*) ' Station+year was: ',wthtfle
           WRITE (*,*) ' Weather file not found!'
           WRITE (*,*) ' Station was     : ',wthfle
           WRITE (*,*) ' Station+year was: ',wthtfle
           WRITE (*,*) ' Program will have to stop'
           WRITE (*,*) ' Check WORK.OUT for details of run'
           STOP ' '
         ENDIF
       ENDIF
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE TDETAILS(FILENAME,TN,RN,snnum,onnum,cnnum)

      IMPLICIT NONE; SAVE

      INTEGER       snx
      PARAMETER     (snx=10)
      INTEGER       onx
      PARAMETER     (onx=3)

      CHARACTER*(*)   FILENAME
      CHARACTER*600   DATATMP
      CHARACTER*254   TL2541
      CHARACTER*80    tl0801, DATASTD
      CHARACTER*30    TL30
      character*25    dataline
      CHARACTER*20    GROUP, LEVELSTD
      CHARACTER*10    LEVEL, TL10FROMI
      CHARACTER*1     TL1UPCASE, FTYPE
      INTEGER         FILENUM, GROUPLEN, TVILENT
      INTEGER         LEVELLEN, FILELEN, LENSTD
      INTEGER         FNUMWRK, I, L
      INTEGER         CNNUM(SNX,ONX), SNNUM, ONNUM(SNX)
      INTEGER         SN, ON(SNX), CN(SNX,ONX), TN, RN, COLRN
      LOGICAL         FOPEN

      IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='Work.out',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
      ENDIF
      IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
     & CALL GETLUN('FILETMP',FILENUM)

      FTYPE ='*' ! Default file type
      GROUP ='*TREATMENTS'

      LENSTD = 12

      FILELEN=TVILENT(FILENAME)

      GROUPLEN=TVILENT(GROUP)

      OPEN(filenum,FILE=filename(1:FILELEN),STATUS='OLD')

      tl0801=' '
      DO WHILE(tl0801(1:GROUPLEN).NE.GROUP(1:GROUPLEN))
        READ(filenum,'(A80)',END=9996,ERR=9999)tl0801
        CALL Ltrim(tl0801)
        tl0801(1:1)=Tl1upcase(tl0801(1:1))
        tl0801(2:2)=Tl1upcase(tl0801(2:2))
        IF(tl0801(1:1).EQ.'$')ftype='$'
      END DO

      LEVEL=TL10FROMI(TN)
      LEVELLEN=TVILENT(LEVEL)
      IF(LEVELLEN.GT.LENSTD)LEVELLEN=LENSTD
      LEVELSTD='      '
      LEVELSTD(1:LEVELLEN) = LEVEL(1:LEVELLEN)

      SNNUM=1
      DO I=1,SNX
        onnum(i)=1
      ENDDO
      DO I=1,SNX
        DO L=1,ONX
          CNNUM(I,L)=1
        ENDDO
      ENDDO

      tl2541=' '

      DO
        DO WHILE(tl30(1:levellen).NE.levelstd(1:levellen))
          READ(filenum,'(A254)',END=9996,ERR=9999)tl2541
          IF(tl2541(1:1).EQ.'*')GOTO 999
          TL30=TL2541(1:30)
          CALL Ltrim(tl30)
          IF(tl30(1:1).EQ.'0')THEN
            tl30(1:1)=' '
            IF(tl30(2:2).EQ.'0')THEN
              tl30(2:2)=' '
              IF(tl30(3:3).EQ.'0')THEN
                tl30(3:3)=' '
              ENDIF
            ENDIF
          ENDIF
          CALL Ltrim(tl30)
          tl30(1:1)=Tl1upcase(tl30(1:1))
          tl30(2:2)=Tl1upcase(tl30(2:2))
        END DO
        DATALINE=tl2541(1:20)//'     '
        CALL Standard(dataline,datatmp,'6')

        IF(ftype.EQ.'*')THEN
          DATASTD(1:48)=DATATMP(1:48)
        ELSEIF(ftype.EQ.'$')THEN
          READ(DATATMP,'(6X,I6)')COLRN
          IF (COLRN.NE.RN) GOTO 998
          DATASTD(1:48)=DATATMP(7:54)
        ENDIF

        READ(DATASTD,'(6X,I6)')sn
        IF(SN.LE.0)SN=1
        IF (SN.GT.0)THEN
          READ(DATASTD,'(12X,I6)')on(sn)
          IF(ON(SN).LE.0)ON(SN)=1
          IF(ON(SN).GT.0)THEN
            READ(DATASTD,'(18X,I6)')cn(sn,on(sn))
            IF(CN(SN,ON(SN)).LE.0)cn(sn,on(sn))=1
          ENDIF
        ENDIF
        snnum=MAX(snnum,sn)
        onnum(sn)=MAX(onnum(sn),on(sn))
        cnnum(sn,on(sn))=MAX(cnnum(sn,on(sn)),cn(sn,on(sn)))

  998   CONTINUE

        READ(filenum,'(A254)',END=9996,ERR=9999)tl2541
        IF(tl2541(1:1).EQ.'*')GOTO 999
        TL30=TL2541(1:30)
        CALL Ltrim(tl30)
        IF(tl30(1:1).EQ.'0')THEN
          tl30(1:1)=' '
          IF(tl30(2:2).EQ.'0')THEN
            tl30(2:2)=' '
            IF(tl30(3:3).EQ.'0')THEN
              tl30(3:3)=' '
            ENDIF
          ENDIF
        ENDIF
        CALL Ltrim(tl30)
        tl30(1:1)=Tl1upcase(tl30(1:1))
        tl30(2:2)=Tl1upcase(tl30(2:2))
      ENDDO

  999 CONTINUE

 9996 CONTINUE
 9998 CONTINUE
 9999 CONTINUE

      CLOSE(filenum)

      IF (SN .LE. 0)THEN
        WRITE (fnumwrk,*) ' Problem in TDETAILS!'
        WRITE (fnumwrk,*) ' Could not read: ',filename(1:60)
        WRITE (*,*) ' Problem in TDETAILS!'
        WRITE (*,*) ' Could not read: ',filename(1:60)
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      RETURN

      END


!-----------------------------------------------------------------------
!
!     SUBROUTINE ECREADRA(ECDIRFLE,LEVEL,CODE,AOUTSIZE,arrayout)
!
!     ! Returns real array for particular code. Not now used
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*)  CODE,LEVEL,ECDIRFLE,AOUTSIZE
!     CHARACTER*254  TXTO
!     CHARACTER*1    POS1DONE
!     REAL           TVRFROMC,TVR1,ARRAYOUT(*)
!     INTEGER        TVI1, L, SIZE
!     INTEGER        ARRAYNUM, POSCOMMA
!
!     TXTO = ' '
!     CALL ECREADC(ECDIRFLE,LEVEL,CODE,txto)
!
!     TVR1 = TVRFROMC(AOUTSIZE)
!     SIZE = NINT(TVR1)
!
!     DO TVI1 = 1,SIZE
!       ARRAYOUT(TVI1)=-99
!     ENDDO
!
!     POS1DONE = 'N'
!     ARRAYNUM = 1
!     POSCOMMA = 0
!     DO L = 1,24
!        IF (TXTO(L:L).EQ.','.OR.TXTO(L:L).EQ.' ') THEN
!          IF (POS1DONE.EQ.'N') THEN
!            ARRAYOUT(1) = TVRFROMC(TXTO(1:L-1))
!            POS1DONE = 'Y'
!          ELSE
!            ARRAYOUT(ARRAYNUM) = TVRFROMC(TXTO(POSCOMMA+1:L-1))
!          ENDIF
!          IF (TXTO(L:L).EQ.'!') GO TO 1000
!          IF (TXTO(L:L).EQ.' ') GO TO 1000
!          ARRAYNUM = ARRAYNUM + 1
!          POSCOMMA = L
!        ENDIF
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READT(ECDIRFLE,ECCODE,CODE,TXTO)
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE, TXTO
!     CHARACTER*600 DATASTD, ABVSTD
!     CHARACTER*254 DATARRAY(500), TL2541, ABVLINE, DATALINE
!     CHARACTER*254 ABVARRAY(1000)
!     CHARACTER*132 ABVFILE
!     CHARACTER*128 ARG
!     CHARACTER*80  TL0801
!     CHARACTER*52  TEXTSTD, tl0521
!     CHARACTER*30  TL30
!     CHARACTER*12  COEFFC(200), ABVC(200), COEFFCTM, SYNCODE
!     CHARACTER*11  GROUP
!     CHARACTER*6   ECCODEP
!     CHARACTER*4   COEFFCHK
!     CHARACTER*1   TL1UPCASE, TEXT, BLANK, CSWOUT
!     INTEGER       TVILENT, TVI1, TVI2, TVI0, TVI3, TVI4
!     INTEGER       FILENUM, GROUPLEN, FILELEN, LINE, LENTMP
!     INTEGER       TVINSTR, ABVNUM, LENSTD, ABVLEN, DATANUM
!     INTEGER       ARGLEN, CODENUM, ABVSYN, FNUMWRK, L
!     LOGICAL       FFLAG,FOPEN
!
!     PARAMETER     (BLANK = ' ')
!
!!     SAVE          ECCODEP
!
!     CSWOUT = 'Y'
!
!     IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
!       CALL Getlun('WORK.OUT',fnumwrk)
!       INQUIRE (FILE='Work.out',OPENED=fopen)
!       IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
!     ENDIF
!     IF (FILENUM.LE.0.OR.FILENUM.GT.1000)
!    & CALL GETLUN('FILETMP',FILENUM)
!
!     LENSTD = 12
!     CODENUM=1
!     SYNCODE=' '
!
!     IF (LEN(TRIM(ECCODE)).LT.3 .OR. ECCODE(1:3).EQ.'   ') THEN
!       WRITE(fnumwrk,*)' No ecotype code in ecotype reads!! '
!       STOP
!     ENDIF
!
!     IF(ECCODE(1:6).EQ.ECCODEP(1:6))GO TO 5555
!
!     FILELEN=TVILENT(ECDIRFLE)
!
!     GROUP(1:1)='*'
!     GROUP(2:7)=ECCODE(1:6)
!     GROUPLEN=TVILENT(GROUP)
!
!     OPEN(filenum,FILE=ECDIRFLE(1:FILELEN),STATUS='OLD')
!
!     tl0801=' '
!     DO WHILE(tl0801(1:GROUPLEN).NE.GROUP(1:GROUPLEN))
!       READ(filenum,'(A80)',END=9998,ERR=9999)tl0801
!       IF (tl0801(2:9).EQ.'ECOTYPE:')THEN
!         tl0801(1:8) = '       '
!         tl0801(9:9) = '*'
!       ENDIF
!       CALL Ltrim(tl0801)
!       tl0801(1:1)=Tl1upcase(tl0801(1:1))
!       tl0801(2:2)=Tl1upcase(tl0801(2:2))
!       IF(tl0801(1:GROUPLEN).EQ.GROUP(1:GROUPLEN))EXIT
!     END DO
!
!     DO L = 1,500
!       DATARRAY = ' '
!     ENDDO
!     DATANUM = 0
!     ABVNUM = 0
!     LINE=0
!     TVI0=0
!     TVI1=0
!
!     DO
!
!       tl2541=' '
!       tl30=' '
!       LINE=LINE+1
!       DO
!         READ(filenum,'(A254)',END=999,ERR=9999)tl2541
!         IF(tl2541(1:1).EQ.'*')GOTO 999
!         IF(tl2541(1:1).EQ.'!')GOTO 888
!         IF(tvilent(tl2541).LT.3)GOTO 888
!         IF(tl2541(1:1).EQ.'@')THEN
!           ! Strip out leading periods
!           ABVLEN=TVILENT(TL2541)
!           DO TVI1=1,ABVLEN-1
!             IF(TL2541(TVI1:TVI1+1).EQ.' .')TL2541(TVI1:TVI1+1)='  '
!           ENDDO
!           IF (ABVLINE(2:30).NE.TL2541(2:30))THEN
!             TVI0=TVI0+ABVNUM
!             ABVLINE=tl2541
!             LINE=1
!           ENDIF
!           GO TO 888
!         ENDIF
!         EXIT
! 888     CONTINUE
!       END DO
!
!       DATALINE=tl2541
!
!       TEXT='N'
!
!       ABVSTD=' '
!       DATASTD=' '
!       tl0521=' '
!       TEXTSTD=' '
!       COEFFC(TVI0+1)=' '
!
!       CALL STANDC(ABVLINE,DATALINE,DATASTD,TEXTSTD,'12')
!       IF(ABVLINE(1:1).EQ.'@')ABVLINE(1:1)=' '
!       CALL STANDARD(ABVLINE,ABVSTD,'12')
!       ABVNUM=TVINSTR(ABVSTD)
!       ! NB MAXIMUM NUMBER OF VARIABLES ALONG LINE = 40
!       IF(ABVNUM.GT.40)ABVNUM=40
!       DO TVI1=1,ABVNUM
!         TVI2= 1 + (TVI1-1)*LENSTD
!         TVI4=TVI2 + (LENSTD-1)
!         COEFFC(TVI0+TVI1)=DATASTD(TVI2:TVI4)
!         ABVC(TVI0+TVI1)=ABVSTD(TVI2:TVI4)
!         COEFFCTM=COEFFC(TVI0+TVI1)
!         CALL LTRIM(COEFFCTM)
!         IF (TVILENT(COEFFCTM).LT.1)COEFFC(TVI0+TVI1) = '-99         '
!         CALL LTRIM(TEXTSTD)
!         IF (TVILENT(TEXTSTD).LT.1)TEXTSTD(1:3) = '-99'
!         COEFFCHK=COEFFCTM(1:4)
!         IF (LINE.EQ.1) THEN
!          IF(COEFFCHK.EQ.'TEXT')THEN
!           datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//TEXTSTD
!          ELSE
!           datarray(TVI0+tvi1)=abvc(TVI0+tvi1)//' '//coeffc(TVI0+tvi1)
!          ENDIF
!         ELSE
!          LENTMP=TVILENT(DATARRAY(TVI0+TVI1))
!          IF(COEFFCHK.EQ.'TEXT')THEN
!           TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//TEXTSTD
!           datarray(TVI0+tvi1)=TL2541
!          ELSE
!           TL2541=datarray(tvi0+tvi1)(1:LENTMP)//' '//coeffc(TVI0+tvi1)
!           datarray(TVI0+tvi1)=TL2541
!          ENDIF
!         ENDIF
!       ENDDO
!
!       datanum=tvi0+abvnum
!
!     ENDDO
!
! 999 CONTINUE
!
!     CLOSE(FILENUM)
!
!     IF (CSWOUT.EQ.'Y') THEN
!       WRITE(fnumwrk,*)' '
!       WRITE(fnumwrk,*)'FILE ',ECDIRFLE(1:FILELEN)
!       WRITE(fnumwrk,*)' GROUP ',GROUP(1:GROUPLEN)
!       DO TVI3 = 1,DATANUM
!         WRITE(fnumwrk,'(I6,A2,A60)')tvi3,'  ',datarray(TVI3)(1:60)
!       ENDDO
!     ENDIF
!
!     arg = ' '
!     abvfile=' '
!     abvfile(1:11)='ECCOEFF.CDE'
!     INQUIRE(FILE=abvfile,EXIST=fflag)
!     IF(.NOT.fflag)THEN
!       CALL GETARG(0,arg)
!       DO tvi1 = 1,arglen
!         IF(arg(tvi1:tvi1).EQ.'/')tvi2=tvi1
!       ENDDO
!       abvfile=ARG(1:TVI2)//'ECCOEFF.CDE'
!       INQUIRE(FILE=abvfile,EXIST=fflag)
!     ENDIF
!     IF(fflag)THEN
!       ! Read abbreviation file
!       WRITE(fnumwrk,*)'  Reading abbrev file: ',abvfile(1:55)
!       CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
!     ELSE
!       WRITE(fnumwrk,*)'  No abbreviation file: ',abvfile(1:55)
!       abvsyn=0
!     ENDIF
!
!     ECCODEP(1:6)=ECCODE(1:6)
!
!5555 CONTINUE
!
!     CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
!     IF (CSWOUT.EQ.'Y') WRITE(FNUMWRK,*)'  E1READ ',CODE,' ',TXTO(1:40)
!
!     RETURN
!
!9998 CONTINUE
!     WRITE(fnumwrk,*)'  Did not find ecotype: ',ECCODE
!     WRITE(fnumwrk,*)'  File: ',ECDIRFLE(1:50)
!     CLOSE(filenum)
!     STOP
!
!9999 CONTINUE
!     WRITE(fnumwrk,*)'  Problem reading: ',ECDIRFLE(1:50)
!     CLOSE(filenum)
!     STOP
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READC(ECDIRFLE,ECCODE,CODE,charout)
!
!     ! Returns one character string for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE, CHAROUT
!     CHARACTER*254 TL2541, TXTO
!     CHARACTER*100 CODENEW
!     CHARACTER*20  COEFF
!     CHARACTER*1   FOUND
!     INTEGER       CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
!     INTEGER       TVI3, CODECLEN, FNUMWRK
!     LOGICAL       FOPEN
!
!     CALL E1READT(ECDIRFLE,ECCODE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODENEW)
!     CODECLEN=LEN(CHAROUT)
!
!     DO TVI1=1,CODECLEN
!       CHAROUT(TVI1:TVI1)=' '
!     ENDDO
!
!     DO TVI1=1,20
!       Coeff(TVI1:TVI1)=' '
!     ENDDO
!
!     FOUND='N'
!     STARTPOS=1
!     ENDPOS=0
!
!     TL2541=TXTO
!     CALL Ltrim(TL2541)
!
!     DO TVI3 = 1, 100
!       IF(TL2541(TVI3:TVI3).EQ.' ')THEN
!         ENDPOS=TVI3
!         FOUND='Y'
!         GO TO 1000
!       ENDIF
!     ENDDO
!1000 CONTINUE
!     IF(FOUND.EQ.'Y')THEN
!       LENGTH=ENDPOS-STARTPOS+1
!       COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
!       CHAROUT=' '
!       IF (CODECLEN.LT.20)THEN
!         CHAROUT=COEFF(1:CODECLEN)
!       ELSE
!         CHAROUT(1:20)=COEFF
!       ENDIF
!      ELSE
!        CALL Getlun('WORK.OUT',fnumwrk)
!        INQUIRE (FILE='Work.out',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
!        WRITE(FNUMWRK,*)'  Could not find code: ',Code
!        STOP
!      ENDIF
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READR(ECDIRFLE,ECCODE,CODE,valueout)
!
!     ! Returns one real value for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE
!     CHARACTER*30  CHAROUT
!     REAL          VALUEOUT, TVRFROMC
!
!     CALL E1READC(ECDIRFLE,ECCODE,CODE,charout)
!     VALUEOUT=TVRFROMC(Charout(1:12))
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READCA(ECDIRFLE,ECCODE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE, AOUTSIZE, ARRAYOUT(*)
!     CHARACTER*354 TL3541
!     CHARACTER*254 TL2541,TXTO
!     CHARACTER*100 CODENEW
!     REAL          TVR1, TVRFROMC
!     INTEGER       CODELEN, TVI1, TVILENT, L, L2, SIZE, LENARVAR
!
!     CALL E1READT(ECDIRFLE,ECCODE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     TVR1 = TVRFROMC(AOUTSIZE)
!     SIZE = NINT(TVR1)
!
!     DO TVI1 = 1,SIZE
!       ARRAYOUT(TVI1)=' '
!     ENDDO
!
!     LENARVAR = LEN(arrayout(1))
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L = 1,SIZE
!        L2 = (L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+LENARVAR)
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE E1READRA(ECDIRFLE,ECCODE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*) ECDIRFLE, ECCODE, CODE, AOUTSIZE
!     CHARACTER*354 TL3541
!     CHARACTER*254 TL2541,TXTO
!     CHARACTER*100 CODENEW
!     REAL          TVR1, TVRFROMC, ARRAYOUT(*)
!     INTEGER       CODELEN, TVI1, TVILENT, L, L2, SIZE
!
!     CALL E1READT(ECDIRFLE,ECCODE,CODE,TXTO)
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     TVR1 = TVRFROMC(AOUTSIZE)
!     SIZE = NINT(TVR1)
!
!     DO TVI1 = 1,SIZE
!       ARRAYOUT(TVI1)=-99
!     ENDDO
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L = 1,SIZE
!        L2 = (L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READT(SPDIRFLE,CODE,TXTO)
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*)   TXTO, CODE, SPDIRFLE
!     CHARACTER*254   DATARRAY(500), ABVARRAY(1000)
!     CHARACTER*128   ARG
!     character*100   CODENEW
!     CHARACTER*132   ABVFILE
!     CHARACTER*93    SPDIRFLT, SPDIRFLP
!     INTEGER         CODELEN, TVI2, FILELEN, DATANUM
!     INTEGER         TVILENT, TVI1, ARGLEN
!     INTEGER         FNUMWRK, ABVSYN
!     LOGICAL         FFLAG, FOPEN
!
!!     SAVE            SPDIRFLT
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     CALL LTRIM2(SPDIRFLE,spdirflt)
!     FILELEN=TVILENT(SPDIRFLT)
!     FILELEN=MIN(93,FILELEN)
!     SPDIRFLT=' '
!     SPDIRFLT(1:FILELEN)=SPDIRFLE(1:FILELEN)
!
!     IF(SPDIRFLE(1:FILELEN).EQ.SPDIRFLP(1:FILELEN))GO TO 5555
!
!     CALL AMAKER0(SPDIRFLT,datarray,datanum)
!     IF (DATANUM .LE. 0)THEN
!       WRITE(fnumwrk,*)'  Problem reading: ',SPDIRFLT
!       WRITE(fnumwrk,*)'  Likely that level not found.'
!       STOP
!     ENDIF
!
!     IF (fnumwrk.LE.0.OR.fnumwrk.GT.1000) THEN
!       CALL Getlun('WORK.OUT',fnumwrk)
!       INQUIRE (FILE='Work.out',OPENED=fopen)
!       IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
!     ENDIF
!
!     abvfile=' '
!     abvfile(1:10)='GCOEFF.CDE'
!     INQUIRE(FILE=abvfile,EXIST=fflag)
!     IF(.NOT.fflag)THEN
!       CALL GETARG(0,arg)
!       DO tvi1 = 1,arglen
!         IF(arg(tvi1:tvi1).EQ.'/')tvi2=tvi1
!       ENDDO
!       abvfile=ARG(1:TVI2)//'GCOEFF.CDE'
!       INQUIRE(FILE=abvfile,EXIST=fflag)
!       IF(fflag)THEN
!         ! Read abbreviation file
!         WRITE(fnumwrk,*)'  Reading abbrev file: ',abvfile(1:55)
!         CALL AMAKEABV(ABVFILE,abvarray,abvsyn)
!       ELSE
!         WRITE(fnumwrk,*)'  No abbreviation file: ',abvfile(1:55)
!         abvsyn=0
!       ENDIF
!     ENDIF
!
!5555 CONTINUE
!
!     CALL ARAYREAD(CODE,DATARRAY,DATANUM,ABVARRAY,ABVSYN,TXTO)
!     ! WRITE(FNUMWRK,*)'  S1READ ',CODE,' ',TXTO(1:40)
!
!     SPDIRFLP=' '
!     SPDIRFLP(1:FILELEN)=SPDIRFLT(1:FILELEN)
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READC(SPDIRFLE,CODE,charout)
!
!     ! Returns one character string for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*)  SPDIRFLE
!     CHARACTER(*)   CODE, CHAROUT
!     CHARACTER*254  TXTO, TL2541
!     CHARACTER*100  CODENEW
!     CHARACTER*20   COEFF
!     CHARACTER*1    FOUND
!     INTEGER        CODELEN, TVI1, TVILENT, STARTPOS, ENDPOS, LENGTH
!     INTEGER        TVI3, CODECLEN, FNUMWRK
!     LOGICAL        FOPEN
!
!     CALL S1READT(SPDIRFLE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODENEW)
!     CODECLEN=LEN(CHAROUT)
!
!     DO TVI1=1,CODECLEN
!       CHAROUT(TVI1:TVI1)=' '
!     ENDDO
!
!     DO TVI1=1,20
!       Coeff(TVI1:TVI1)=' '
!     ENDDO
!
!     FOUND='N'
!     STARTPOS=1
!     ENDPOS=0
!
!     TL2541=TXTO
!     CALL Ltrim(TL2541)
!
!     DO TVI3 = 1, 100
!       IF(TL2541(TVI3:TVI3).EQ.' ')THEN
!         ENDPOS=TVI3
!         FOUND='Y'
!         GO TO 1000
!       ENDIF
!     ENDDO
!1000 CONTINUE
!     IF(FOUND.EQ.'Y')THEN
!       LENGTH=ENDPOS-STARTPOS+1
!       COEFF(1:LENGTH)=TL2541(STARTPOS:ENDPOS)
!       CHAROUT=' '
!       IF (CODECLEN.LT.20)THEN
!         CHAROUT=COEFF(1:CODECLEN)
!       ELSE
!         CHAROUT(1:20)=COEFF
!       ENDIF
!      ELSE
!        CALL Getlun('WORK.OUT',fnumwrk)
!        INQUIRE (FILE='Work.out',OPENED=fopen)
!        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
!        WRITE(fnumwrk,*)'  Could not find code: ',Code
!        STOP
!      ENDIF
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READR(SPDIRFLE,CODE,valueout)
!
!     ! Returns one real value for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*)  SPDIRFLE, CODE
!     CHARACTER*30   CHAROUT
!     REAL           VALUEOUT, TVRFROMC
!
!     CALL S1READC(SPDIRFLE,CODE,charout)
!     VALUEOUT=TVRFROMC(Charout(1:12))
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READRA(SPDIRFLE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*)   SPDIRFLE, CODE, AOUTSIZE
!     CHARACTER*354   TL3541
!     CHARACTER*254   TL2541,TXTO
!     CHARACTER*100   CODENEW
!     REAL            TVR1, TVRFROMC, ARRAYOUT(*)
!     INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE
!
!     CALL S1READT(SPDIRFLE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     TVR1 = TVRFROMC(AOUTSIZE)
!     SIZE = NINT(TVR1)
!
!     DO TVI1 = 1,SIZE
!       ARRAYOUT(TVI1)=-99
!     ENDDO
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L = 1,SIZE
!        L2 = (L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)=TVRFROMC(TL3541(L2:L2+5))
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READIA(SPDIRFLE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*)   SPDIRFLE, CODE, AOUTSIZE
!     CHARACTER*354   TL3541
!     CHARACTER*254   TL2541,TXTO
!     CHARACTER*100   CODENEW
!     INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE, ARRAYOUT(*)
!     INTEGER         TVIFROMC
!
!     CALL S1READT(SPDIRFLE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     SIZE = TVIFROMC(AOUTSIZE)
!
!     DO TVI1 = 1,SIZE
!       ARRAYOUT(TVI1)='-99'
!     ENDDO
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L = 1,SIZE
!        L2 = (L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)=TVIFROMC(TL3541(L2:L2+5))
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
!
!-----------------------------------------------------------------------
!
!     SUBROUTINE S1READCA(SPDIRFLE,CODE,AOUTSIZE,arrayout)
!
!     ! Returns ARRAY for particular code
!
!     IMPLICIT NONE; SAVE
!
!     CHARACTER*(*)   SPDIRFLE, CODE, AOUTSIZE, ARRAYOUT(*)
!     CHARACTER*354   TL3541
!     CHARACTER*254   TL2541,TXTO
!     CHARACTER*100   CODENEW
!     REAL            TVR1, TVRFROMC
!     INTEGER         CODELEN, TVI1, TVILENT, L, L2, SIZE, LENARVAR
!
!     CALL S1READT(SPDIRFLE,CODE,TXTO)
!
!     CALL LTRIM2(CODE,codenew)
!     CODELEN=Tvilent(CODEnew)
!
!     TVR1 = TVRFROMC(AOUTSIZE)
!     SIZE = NINT(TVR1)
!
!     DO TVI1 = 1,SIZE
!       ARRAYOUT(TVI1)=' '
!     ENDDO
!
!     LENARVAR = LEN(arrayout(1))
!
!     TL2541=TXTO
!     CALL Standard(TL2541,TL3541,'7')
!     DO L = 1,SIZE
!        L2 = (L-1)*7 + 1
!        IF (TL3541(L2:L2).EQ.'!')GO TO 1000
!        IF (TL3541(L2:L2).EQ.' ')GO TO 1000
!        ARRAYOUT(L)(1:LENARVAR)=TL3541(L2:L2+LENARVAR)
!     END DO
!1000 CONTINUE
!
!     RETURN
!
!     END
