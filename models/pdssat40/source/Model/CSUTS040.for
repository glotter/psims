!  Utility routines and functions

      ! Subroutines: Calendar, Csopline, Csdaylen, Csincmth, Csyr_doy
      !              Emod
      !              Finddir, Fvcheck
      !              Getpos, Getstri, Getstrr, Getstr, Getstr1
      !              Linechar, Linevals, Ltrim, Ltrim2
      !              Standard, Standc, Start
      !              Ucase, Csclear5

      ! Functions    Csdoc, Csyeardoy, Csydoy, Cstabex,
      !              Csincdat, Csendyr, Csvpsat, Csmthend, Csmthmid,
      !              Cstimdif
      !              Dapcalc
      !              Tfac4
      !              Tvicolnm, Tvilent, Tvinstr, Tvifromc, Tvrfromc
      !              Tl1upcase, Tl10fromi
      !              Yval, Yvalxy, Yval1

      ! Following are used in the weather routine.
      ! Subroutines  Csfind, Csignore
      ! Function     Csupcase

      ! Following is used in the Legumes module.
      ! Function     Cscurv

!-----------------------------------------------------------------------

      SUBROUTINE Calendar(iyr,doy,DOM,MONTH)

      ! Provide calendar date for specified day of year

      IMPLICIT NONE

      CHARACTER*3 mon(12),month
      INTEGER     dom,doy,idim(12),i,mo,iyr

      DATA Mon/'Jan','Feb','Mar','Apr','May','Jun','Jul'
     X ,'Aug','Sep','Oct','Nov','Dec'/

      IF (doy.GT.366 .OR. iyr.GT.3000) THEN
        dom = 0
        month = '-99'
        RETURN
      ENDIF

      DO i=1,12
       idim(i)=31
      ENDDO
      idim(4)=30
      idim(6)=30
      idim(9)=30
      idim(11)=30
      idim(2)=28
      IF(MOD(iyr,4).EQ.0)idim(2)=29
      mo=1
      dom=31
      DO WHILE(dom.LT.doy)
       mo=mo+1
       IF (mo.GT.12) THEN
         dom=0
         month='-99'
         RETURN
       ENDIF
       dom=dom+idim(mo)
      END DO
      dom=doy-dom+idim(mo)
      month=mon(mo)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Csopline(opline,opreal)

      ! Creates an output line from a real variable

      IMPLICIT NONE

      CHARACTER*(*) opline
      REAL          opreal

      IF (OPREAL.EQ.0.00) THEN
        WRITE (OPLINE,'(1X,F5.1)') opreal
      ELSEIF (OPREAL.GT.0.00 .AND. OPREAL.LE.0.099) THEN
        WRITE (OPLINE,'(1X,F5.4)') opreal
      ELSEIF (OPREAL.GT.0.099 .AND. OPREAL.LE.0.99) THEN
        WRITE (OPLINE,'(1X,F5.3)') opreal
      ELSEIF (OPREAL.GT.0.99 .AND. OPREAL.LE.9.99) THEN
        WRITE (OPLINE,'(1X,F5.2)') opreal
      ELSEIF (OPREAL.GT.9.99 .AND. OPREAL.LE.99.99) THEN
        WRITE (OPLINE,'(1X,F5.1)') opreal
      ELSEIF (OPREAL.GT.99.99 .AND. OPREAL.LT.999.9)THEN
        WRITE (OPLINE,'(1X,F5.1)') OPREAL
      ELSE
        WRITE (OPLINE,'(1X,I5)') NINT(OPREAL)
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSDAYLEN (DAY,XLAT,DAYL,DAYLC)

      IMPLICIT    NONE

      INTEGER DAY
      REAL  HEMIS, LAT, PI, CFDATR, CFDGTR, CFRATH, ANGLE, RDATE
      REAL  DECLIN, HAS, SISI, COCO, DAYLC, DOY, XLAT, DAYL

      DOY = FLOAT(DAY)

      ! Algorithm from Simtag model

      IF (XLAT .LT. 0.0) THEN
        HEMIS = 1.0
        LAT = ABS(XLAT)
      ELSE
        HEMIS = 0.0
        LAT = XLAT
      ENDIF
      PI=3.141593
      CFDATR=2.*PI/365.
      CFDGTR=2.*PI/360.
      CFRATH=24./(2.*PI)
      ANGLE=96.
      RDATE=DOY*CFDATR
      DECLIN=0.397-22.980*COS(RDATE)+3.631*SIN(RDATE)-0.388*COS(2*RDATE)
     *+0.039*SIN(2*RDATE)-0.160*COS(3*RDATE)
      IF(HEMIS.EQ.1.) DECLIN=-DECLIN
      COCO=COS(LAT*CFDGTR)*COS(DECLIN*CFDGTR)
      SISI=SIN(LAT*CFDGTR)*SIN(DECLIN*CFDGTR)
   3  HAS=ACOS(AMAX1(-1.,(COS(ANGLE*CFDGTR)-SISI)/COCO))
      DAYL=2.*HAS*CFRATH
      IF(ANGLE.LT.96.) GO TO 4
      DAYLC=DAYL
      ANGLE=90.+50./60.
      GO TO 3
   4  CONTINUE

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSINCMTH(YR,MTH,INC)

      ! Increases/decreases month,adjusts YR based on INC (ABS(INC)<=12)

      IMPLICIT NONE

      INTEGER YR,MTH,INC

      MTH = MTH + INC
      IF (MTH .GT. 12) THEN
        YR = YR + 1
        MTH = MTH - 12
      ELSE IF (MTH .LT. 1) THEN
        YR = YR - 1
        MTH = MTH + 12
      ENDIF

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSYR_DOY(YRDOY,YEAR,DOY)

      !  Converts YRDOY (or YEARDOY) to YEAR and DOY.

      IMPLICIT NONE

      INTEGER DOY,YEAR,YRDOY,YR,YRDOYWRK

      YRDOYWRK = YRDOY
      IF(YRDOY.GE.2000000)YRDOYWRK=YRDOY-2000000
      IF(YRDOY.GE.1900000)YRDOYWRK=YRDOY-1900000

      IF(YRDOY.LE.0)THEN
        YEAR  = - 99
        DOY = - 99
      ELSE
        YR  = INT(YRDOYWRK / 1000)
        DOY = YRDOYWRK - YR * 1000
      ENDIF

      IF (YR.GE.19) THEN
        YEAR=1900+YR
      ELSEIF (YR.GE.0 .AND. YR.LT.19) THEN
        YEAR=2000+YR
      ENDIF

      END
!-----------------------------------------------------------------------

      SUBROUTINE EMOD(FACADJ,FAC,ADJ,NEV)

      ! Generates values for environmental adjustments from input string

      IMPLICIT NONE

      CHARACTER*(*)  facadj(10), fac(10)
      REAL           adj(10)
      INTEGER        i,l,tvilent,nev,fnumwrk
      LOGICAL        fopen

      DO I = 1,NEV
       CALL ltrim(facadj(i))
       L=Tvilent(facadj(i))
       IF(l.LT.2)THEN
         fac(i)='0'
         adj(i)=-99
       ELSEIF(l.EQ.2)THEN
         fac(i)=facadj(i)(1:1)
         READ(facadj(i),'(1x,F1.0))')adj(i)
       ELSEIF(l.EQ.3)THEN
         IF(facadj(i)(1:1).EQ.'-')THEN
           fac(i)='0'
           adj(i)=-99
         ELSE
           fac(i)=facadj(i)(1:1)
           READ(facadj(i),'(1x,F2.0))')adj(i)
         ENDIF
       ELSEIF(l.EQ.4)THEN
         fac(i)=facadj(i)(1:1)
         READ(facadj(i),'(1x,F3.0))')adj(i)
       ELSEIF(l.EQ.5)THEN
         fac(i)=facadj(i)(1:1)
         READ(facadj(i),'(1x,F4.0))')adj(i)
       ELSEIF(l.GT.5)THEN
         CALL Getlun('WORK.OUT',fnumwrk)
         INQUIRE (FILE='WORK.OUT',OPENED=fopen)
         IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
         WRITE(fnumwrk,*)' Problem reading Environmental Modifications!'
         WRITE (*,*) ' Problem reading Environmental Modifications!'
         WRITE (*,*) ' Program will have to stop'
         WRITE (*,*) ' Check WORK.OUT for details of run'
         STOP ' '
       ENDIF
      ENDDO

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Finddir(fnumcfg,cfgdfile,cfgcode,fname,FDIRNAME)

      ! Returns full name (inc directory) for specified file
      ! Uses cfg.file

      IMPLICIT NONE

      CHARACTER*(80) tline
      CHARACTER*(2)  tl2
      CHARACTER*(37) tl37
      CHARACTER*(*)  cfgdfile,cfgcode,fname,fdirname
      INTEGER        fnumcfg,l,tvilent,loop,fnumwrk
      LOGICAL        fflag,fopen

      fdirname=' '
      fdirname='-99'

      IF (FNUMWRK.LE.0.OR.FNUMWRK.GE.1000) THEN
        CALL Getlun ('WORK.OUT',fnumwrk)
        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
      ENDIF

      INQUIRE(FILE=cfgdfile,EXIST=fflag)
      IF(.NOT.fflag)THEN
        WRITE(fnumwrk,*)' No configuration file: ',cfgdfile(1:50)
        RETURN
      ENDIF

      OPEN(fnumcfg,FILE=cfgdfile)

      DO loop=1,2000
       READ(fnumcfg,'(A80)',ERR=9999,END=10000)tline
       IF(tline(1:6).EQ.'*DIREC')EXIT
      ENDDO

      DO loop=1,2000
       READ(fnumcfg,'(A80)',ERR=9999,END=10000)tline
       IF(tline(1:1).EQ.'*')THEN
        CLOSE(fnumcfg)
        fdirname=' '
        fdirname(1:3)='-99'
        RETURN
       ENDIF
       IF(tline(1:3).EQ.cfgcode(1:3))THEN
        CALL Getstr(tline,2,tl2)
        CALL Getstr(tline,3,tl37)
        l=Tvilent(tl37)
        IF(tl37(l:l).NE.'/'.AND.l.LT.37)THEN
         tl37(l+1:l+1)='/'
         l=l+1
        ENDIF
        fdirname=tl2//tl37(1:l)//fname
        WRITE(fnumwrk,*)' Looking for: ',fdirname(1:60)
        CALL Ucase(fdirname)
        INQUIRE(FILE=fdirname,EXIST=fflag)
        IF(fflag)EXIT
       ENDIF
      ENDDO
      CLOSE(fnumcfg)
      RETURN

9999  WRITE (fnumwrk,*) ' Error in finddir!'
      WRITE (*,*) ' Error in finddir!'
      GOTO 10001
10000 WRITE (fnumwrk,*) ' End of configuration file in finddir!'
      WRITE (*,*) ' End of configuration file in finddir!'
10001 CONTINUE
      WRITE(fnumwrk,*)' Looking for code: ',cfgcode
      WRITE(fnumwrk,*)' Looking for file: ',fname
      WRITE (*,*) ' Looking for code: ',cfgcode
      WRITE (*,*) ' Looking for file: ',fname
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details of run'
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE FVCHECK(DIRFILE,VCODE)

      ! Checks file version (ie.compares what needed in model with what
      ! in header of file.

      IMPLICIT NONE

      CHARACTER*(*)  DIRFILE, VCODE
      CHARACTER*80   TLINE
      CHARACTER*1    COLON
      INTEGER        L,FNUMWRK,FNUMTMP
      LOGICAL        FFLAG,FOPEN

      IF (FNUMWRK.LE.0.OR.FNUMWRK.GE.1000) THEN
        CALL Getlun ('WORK.OUT',fnumwrk)
        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
      ENDIF
      IF (FNUMTMP.LE.0.OR.FNUMTMP.GE.1000)
     & CALL Getlun ('FVCHECK',fnumtmp)

      INQUIRE (FILE = dirfile,EXIST = fflag)
      IF (.NOT.fflag) THEN
        WRITE (fnumwrk,*) ' Could not find input file! '
        WRITE (fnumwrk,*) ' File was: ',dirfile(1:60)
        WRITE (fnumwrk,*) ' Version code sought was: ',vcode
        WRITE (*,*) ' Could not find input file! '
        WRITE (*,*) ' File was: ',dirfile(1:60)
        WRITE (*,*) ' Version code sought was: ',vcode
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ELSE
        COLON = 'N'
        OPEN (UNIT = FNUMTMP,FILE = DIRFILE)
        READ(FNUMTMP,'(A80)')TLINE
        DO L = 1,50
          IF (COLON.EQ.'Y' .AND. TLINE(L:L).NE.' ') EXIT
          IF (TLINE(L:L).EQ.':' .OR. TLINE(L:L).EQ.' ') COLON='Y'
        ENDDO
        CLOSE(FNUMTMP)
        IF (TLINE(L:L+7) .NE. VCODE) THEN
          WRITE (fnumwrk,*)' Input file not correct version!'
          WRITE (fnumwrk,*)' File was: ',dirfile(1:60)
          WRITE (fnumwrk,*)' File version: ',tline(L:L+7)
          WRITE (fnumwrk,*)' Needed version: ',vcode
          WRITE (*,*)' Input file not correct version!'
          WRITE (*,*)' File was: ',dirfile(1:60)
          WRITE (*,*)' File version: ',tline(L:L+7)
          WRITE (*,*)' Needed version: ',vcode
          WRITE (*,*)' Program will have to stop'
          WRITE (*,*)' Check WORK.OUT for details of run'
          STOP ' '
        ENDIF
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Getpos(tline,number,SPOS)

      ! Returns start position for specified string in line

      IMPLICIT NONE

      CHARACTER*(*)  tline
      CHARACTER*(30) value
      INTEGER        number,spos,epos

      CALL Getstr1(tline,number,value,spos,epos)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Getstri(tline,number,VALUEI)

      ! Returns integer for specifed position in line

      IMPLICIT NONE

      CHARACTER*(*)  tline
      CHARACTER*(10) value
      INTEGER        number,epos,spos,valuei,tvifromc

      SAVE

      CALL Getstr1(tline,number,value,spos,epos)
      valuei = TVIFROMC(value)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Getstrr(tline,number,VALUEI)

      ! Returns real for specified position in line

      IMPLICIT NONE

      CHARACTER*(*)  tline
      CHARACTER*(10) value
      INTEGER        number,epos,spos
      REAL           valuei,tvrfromc

      SAVE

      CALL Getstr1(tline,number,value,spos,epos)
      valuei = TVRFROMC(value)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Getstr(tline,number,VALUE)

      ! Returns variable string for specifed position in line

      IMPLICIT NONE

      CHARACTER*(*) tline,value
      INTEGER       number,epos,spos

      SAVE

      CALL Getstr1(tline,number,value,spos,epos)

      RETURN
      END

!-----------------------------------------------------------------------

      SUBROUTINE Getstr1(tline,number,VALUE,SPOS,EPOS)

      ! Returns variable string for specifed position in line

      IMPLICIT NONE

      CHARACTER*(*) tline, value
      INTEGER       number,pos1,pos2,i,loc,switch,spos,epos,tvinstr,l

      SAVE

      value=' '

      IF(number.LE.0)THEN
       value='-99'
       RETURN
      ENDIF

      i=tvinstr(tline)
      l=LEN(TRIM(tline))
      IF(number.GT.i)THEN
       value='-99'
       spos=-99
       epos=-99
       RETURN
      ENDIF

  10  CONTINUE
      pos1=0
      pos2=0
      switch=0
      loc=0
      i=0
      DO WHILE (i.LE.l)
       i=i+1
       IF(i.LE.l .AND. tline(i:i).NE.' ')THEN
        IF(switch.EQ.0)THEN
         switch=1
        ELSE
         switch=2
        ENDIF
        IF(pos1.NE.0)THEN
         pos2=i
        ELSE
         pos1=i
         pos2=i
        ENDIF
       ELSE
        IF(number.EQ.loc.AND.(switch.EQ.1.OR.switch.EQ.2))THEN
         value=tline(pos1:pos2)
         spos=pos1
         epos=pos2
        ENDIF
        switch=0
        pos1=0
       ENDIF
       IF(switch.EQ.1)THEN
        loc=loc+1
       ENDIF
      ENDDO

      IF(number.EQ.2.AND.value.EQ.'=')THEN
       number=3
       GOTO 10
      ENDIF

      IF(value.EQ.' ') value='-99'
      IF(value.EQ.'.') value='-99'

      RETURN

      END

!-----------------------------------------------------------------------
      SUBROUTINE Linechar(tline,ARRAY)

      ! Reads a character string and returns individual components

      IMPLICIT NONE

      INTEGER tvicolnm,ncol,l,headfound,istart,iend,i,l1,loop,ineg
      INTEGER LENLINE,TVILENT,LENLINET
      CHARACTER tline*(*)
      CHARACTER datatmp*31
      CHARACTER*10 array(20)

      Lenline=Len(tline)
      Lenlinet=Tvilent(tline)
      Lenline = Min(lenline,lenlinet+2)

      l=0
      i=0
      DO loop=1,20
       array(loop)=' '
      ENDDO
      ncol=0
      tvicolnm=0
      headfound=0

      DO WHILE (i.LE.lenline)
       istart=0
       iend=0
       i=l
       DO WHILE (istart.LT.1)
        IF(ineg.EQ.0)THEN
         i=i+1
        ELSEIF(ineg.GT.0)THEN
         i=i
         ineg=0
        ENDIF
        IF(i.GT.LENLINE) GOTO 9999
        IF(tline(i:i).EQ.'-'.OR.tline(i:i).NE.' ')THEN
         istart=i
         EXIT
        ENDIF
       ENDDO
       i=istart
       DO WHILE (iend.LT.1)
        i=i+1
        IF(i.GT.LENLINE)EXIT
        IF(tline(i:i).EQ.'-'.OR.tline(i:i).EQ.' ')THEN
         IF(tline(i:i).EQ.'-') ineg=1
         iend=i-1
         EXIT
        ENDIF
       ENDDO
       l=i

       IF(i.GT.lenline)EXIT
       ncol=ncol+1
       IF(ncol.GT.20)EXIT

       l1=(iend-istart+1)
       datatmp='                                '
       datatmp(1:l1)=tline(istart:iend)

       IF(ncol.GT.0)array(ncol)=datatmp(1:10)

      END DO

9999  CONTINUE

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE Linevals(tline,ARRAY)

      ! Reads a character string and returns values to max of 20 items

      IMPLICIT NONE

      INTEGER tvicolnm,ncol,l,headfound,istart,iend,i,l1,loop,ineg
      INTEGER LENLINE,TVILENT,LENLINET,ICHAR,I2
      CHARACTER tline*(*)
      CHARACTER datatmp*31,vartype*3
      REAL array(20),valuetmp

      Lenline=Len(tline)
      Lenlinet=Tvilent(tline)
      Lenline = Min(lenline,lenlinet+2)

      ! Remove flags (eg.A,N) and old values after flag
      I=0
      VARTYPE = 'CHA'
      DO WHILE (I.LE.LENLINE)
        I=I+1
        IF (ICHAR(TLINE(I:I)).LE.57.AND.TLINE(I:I).NE.' ')VARTYPE='NUM'
        IF (VARTYPE.EQ.'NUM' .AND. ICHAR(TLINE(I:I)).GT.57) THEN
          TLINE(I:I) = ' '
          DO I2 = 1,10
            IF (I+I2.GT.LENLINE) EXIT
            IF (TLINE((I+I2):(I+I2)).EQ.' ') THEN
              VARTYPE = 'CHA'
              EXIT
            ENDIF
            TLINE((I+I2):(I+I2)) =  ' '
          ENDDO
        ENDIF
      ENDDO

      l=0                                              ! Find column #
      i=0
      DO loop=1,20
       array(loop)=0.0
      ENDDO
      ncol=0
      tvicolnm=0
      headfound=0

      DO WHILE (i.LE.lenline)
       istart=0
       iend=0
       i=l
       DO WHILE (istart.LT.1)
        IF(ineg.EQ.0)THEN
         i=i+1
        ELSEIF(ineg.GT.0)THEN
         i=i
         ineg=0
        ENDIF
        IF(i.GT.LENLINE) GOTO 9999
        IF(tline(i:i).EQ.'-'.OR.tline(i:i).NE.' ')THEN
         istart=i
         EXIT
        ENDIF
       ENDDO
       i=istart
       DO WHILE (iend.LT.1)
        i=i+1
        IF(i.GT.LENLINE)EXIT
        IF(tline(i:i).EQ.'-'.OR.tline(i:i).EQ.' ')THEN
         IF(tline(i:i).EQ.'-') ineg=1
         iend=i-1
         EXIT
        ENDIF
       ENDDO
       l=i

       IF(i.GT.lenline)EXIT
       ncol=ncol+1
       IF(ncol.GT.20)EXIT

       l1=(iend-istart+1)
       datatmp='                                '
       datatmp(1:l1)=tline(istart:iend)
       READ(datatmp,300,ERR=301)valuetmp
300    FORMAT(F10.0)
       GOTO 302
301    CONTINUE
       datatmp(l1:l1)=' '
       READ(datatmp,300,ERR=303)valuetmp
       GOTO 302
303    CONTINUE
       valuetmp=-99
302    CONTINUE
       IF(ncol.GT.0)array(ncol)=valuetmp

      END DO

9999  CONTINUE

      RETURN
      END

!-----------------------------------------------------------------------

      SUBROUTINE Ltrim(tchar)

      ! Left trims a character variable

      IMPLICIT NONE

      CHARACTER*(*) tchar
      CHARACTER*500 tchar2
      INTEGER       l,i,k,l2,tvilent,lentc,fnumwrk
      LOGICAL       fopen

      lentc = LEN(tchar)
      IF (lentc.GT.500) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='WORK.OUT',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
        WRITE (fnumwrk,*)
     &   ' Problem in Ltrim! Input line longer than allowed!'
        WRITE (*,*)
     &   ' Problem in Ltrim! Input line longer than allowed!'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      i=1
      l=Tvilent(tchar)
      l2=Len(tchar)
      IF(l.LE.0)RETURN
      IF(l.GT.1)THEN
       DO i=1,l
        IF(tchar(i:i).NE.' ')EXIT
       ENDDO
      ENDIF
      k=l-(i-1)

      tchar2=' '
      tchar2(1:k)=tchar(i:l)
      tchar = ' '

      tchar(1:k)=tchar2(1:k)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Ltrim2(tchar,tchar2)

      ! Left trims a character variable - used when string passed

      IMPLICIT NONE

      CHARACTER(*)  tchar, tchar2
      INTEGER       l,i,k,l2,tvilent

      i=1
      l=Tvilent(tchar)
      l2=Len(tchar)
      IF(l.LE.0)RETURN
      IF(l.GT.1)THEN
       DO i=1,l
        IF(tchar(i:i).NE.' ')EXIT
       ENDDO
      ENDIF
      k=l-(i-1)

      tchar2=' '
      tchar2(1:k)=tchar(i:l)

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Standard(tlinein,TLINEOUT,widthc)

      ! Standardises variables along a string to widthc spaces each

      IMPLICIT NONE

      CHARACTER*(*)  tlinein,tlineout,widthc
      INTEGER        i,j,l,itmp,tvilent,blanks,width,tvifromc
      INTEGER        lenin,lenout,fnumwrk
      LOGICAL        fopen

      IF (FNUMWRK.LE.0.OR.FNUMWRK.GE.1000) THEN
        CALL Getlun ('WORK.OUT',fnumwrk)
        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
      ENDIF

      width=0
      width=Tvifromc(widthc)

      tlineout=' '
      i=0
      l=0
      j=Tvilent(tlinein)
      lenin=LEN(tlinein)
      lenout=LEN(tlineout)
      blanks=0

      DO WHILE(l.LT.j)
       l=l+1
       blanks=blanks+1
       IF(blanks.GT.50)EXIT
       IF(tlinein(l:l).NE.CHAR(32))THEN
        IF(tlinein(l:l).NE.CHAR(0))THEN
        blanks=0
        itmp=0
        DO WHILE(tlinein(l:l).NE.' ')
         IF(tlinein(l:l).EQ.CHAR(0))EXIT
         IF(tlinein(l:l).EQ.CHAR(32))EXIT
         blanks=blanks+1
         IF(blanks.GT.50)EXIT
         i=i+1
         IF(l.EQ.lenin)GOTO 9998
         itmp=itmp+1
         tlineout(i:i)=tlinein(l:l)
         l=l+1
         IF(tlinein(l:l).EQ.CHAR(0))EXIT
         IF(itmp.EQ.width-1)THEN
          DO WHILE(tlinein(l:l).NE.' ')
           IF(tlinein(l:l).EQ.CHAR(0))EXIT
           IF(tlinein(l:l).EQ.CHAR(32))EXIT
           l=l+1
          ENDDO
          EXIT
         ENDIF
        ENDDO
        DO WHILE(itmp.LT.width)
         i=i+1
         IF(i.EQ.lenout)GOTO 9999
         itmp=itmp+1
         tlineout(i:i)=' '
        ENDDO
       ENDIF
       ENDIF
       IF(tlinein(l:l).EQ.CHAR(0))EXIT
      ENDDO

      RETURN

 9998 WRITE(fnumwrk,*)
     & ' Problem in standard! End of line, or no blank!'
      WRITE (fnumwrk,*) ' Linein : ',tlinein(1:65)
      WRITE (fnumwrk,*) ' Lineout: ',tlineout(1:65)
      WRITE (*,*)
     & ' Problem in standard! End of line, or no blank!'
      WRITE (*,*) ' Linein : ',tlinein(1:65)
      WRITE (*,*) ' Lineout: ',tlineout(1:65)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details of run'
      STOP ' '
 9999 WRITE(fnumwrk,*)
     & ' Problem in standard!! Line too long adding blanks! '
      WRITE(fnumwrk,*) ' Linein : ',tlinein(1:65)
      WRITE(fnumwrk,*) ' Lineout: ',tlineout(1:65)
      WRITE(*,*)
     & ' Problem in standard!! Line too long adding blanks! '
      WRITE (*,*) ' Linein : ',tlinein(1:65)
      WRITE (*,*) ' Lineout: ',tlineout(1:65)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details of run'
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE Standc(atlinein,tlineini,TLINEOUT,TCHAROUT,widthc)

      ! Removes one string of text from below data code with dots, or
      ! at end of line, then standardises variables to 'widthc' spaces
      ! each. Returns both text from below code with dots, or at end
      ! (tcharout) and standardised line.
      ! NB 'TEXT' iserted in place of string so that variable position
      ! along line is not changed.
      ! NB April 2002 gave specific string lengths to tlineout,tcharout

      ! NB Tcharout is limited to 52 characters. This may truncate
      ! a long string, as for example with soil classification from
      ! soil files.

      IMPLICIT NONE

      CHARACTER*(*)  tlineini, atlinein, widthc
      CHARACTER*1000 tlinetmp, atline, tlinein, tlineout
      CHARACTER*52   tcharout
      CHARACTER*1    dottype
      INTEGER        i,j,k,l,itmp,tvilent,blanks,width,start,enddot
      INTEGER        tvifromc,lendots,lenout,lenafter,kk,lendata
      INTEGER        lentchar,fnumwrk
      LOGICAL        fopen

      dottype='I'    ! Dots inside line

      IF (FNUMWRK.LE.0.OR.FNUMWRK.GE.1000) THEN
        CALL Getlun ('WORK.OUT',fnumwrk)
        INQUIRE (FILE = 'WORK.OUT',OPENED = fopen)
        IF (.NOT.fopen) OPEN (UNIT = fnumwrk,FILE = 'WORK.OUT')
      ENDIF

      j=Tvilent(atlinein)
      IF(j.LT.3)THEN
        WRITE(fnumwrk,*)' Abbreviation line not found! Wrong file type?'
        WRITE(fnumwrk,*)' Abvr.line: ',atlinein(1:60)
        WRITE(fnumwrk,*)' Data line: ',tlineini(1:60)
        WRITE (*,*) ' Abbreviation line not found! Wrong file type?'
        WRITE (*,*) ' Abvr.line: ',atlinein(1:60)
        WRITE (*,*) ' Data line: ',tlineini(1:60)
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF
      atline = ' '
      IF (j.LE.10) THEN
        atline(1:1) = atlinein(1:1)
        atline(3:J+1) = atlinein(2:j)
      ELSE
        atline(1:j) = atlinein(1:j)
      ENDIF

      j=Tvilent(tlineini)
      IF(j.LT.3)THEN
        WRITE (fnumwrk,*)
     &   ' Data line sent for standardising is too short!'
        WRITE (fnumwrk,*)' Presumably is wrong file type!'
        WRITE (*,*)
     &   ' Data line sent for standardising is too short!'
        WRITE (*,*) ' Presumably is wrong file type!'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF
      tlinein=' '
      tlinein=tlineini(1:j)

      kk=LEN(tlineout)

      width=0
      width=Tvifromc(widthc)

      ! Establish position of dotted variable
      j=Tvilent(atline)
      start=0
      enddot=0
      DO l=2,j
       IF(atline(l:l).NE.' '.AND.atline(l-1:l-1).EQ.' ')start=l
       IF(atline(l:l).EQ.' '.AND.atline(l-1:l-1).EQ.'.')THEN
        enddot=l-1
        EXIT
       ENDIF
      ENDDO

      IF(enddot.EQ.0)THEN
       enddot=Tvilent(tlinein)
       dottype='E'   ! Dots at end of line
      ENDIF

      ! Create new lineout
      tlineout=' '
      tcharout=' '
      j=Tvilent(tlinein)
      IF(start.GT.1)tlineout(1:start-1)=tlinein(1:start-1)
      IF(enddot.NE.0)THEN
       lendots=enddot-start+1
       lenafter=j-enddot
       IF(lenafter.GT.0.AND.dottype.EQ.'I')THEN
        tlineout(start:start+5)=' TEXT '
        tlineout((start+6):((start+6)+lenafter))=tlinein(enddot+1:j)
       ENDIF
       IF(dottype.EQ.'E')THEN
        IF(start.GT.1)THEN
          IF(tlineout(start-1:start-1).NE.' ')start=start-1
          IF(tlineout(start-1:start-1).NE.' ')start=start-1
          tlineout(1:start-1)=tlinein(1:start-1)
          tlineout(start:start+5)=' TEXT '
          lendata=Tvilent(tlinein)
          lentchar=MIN(52,lendata-start+1)
          tcharout(1:lentchar)=tlinein(start:start+lentchar-1)
         ELSE
          tlineout=tlinein
         ENDIF
       ENDIF
      ENDIF

      ! Create output character string
      IF(enddot.GT.start+1)THEN
       i=MIN0(52,enddot-start+1)
       i=MAX0(i,1)
       tcharout(1:i)=tlinein(start:enddot)
       CALL ltrim(tcharout)
      ENDIF

      tlinetmp=' '
      lenout=Tvilent(tlineout)
      tlinetmp(1:lenout)=tlineout(1:lenout)
      tlineout=' '

      i=0
      l=0
      j=Tvilent(tlinetmp)
      k=LEN(tlinetmp)
      kk=LEN(tlineout)
      blanks=0

      DO WHILE(l.LT.j)
       l=l+1
       blanks=blanks+1
       IF(blanks.GT.50)EXIT
       IF(tlinetmp(l:l).NE.CHAR(32))THEN
        IF(tlinetmp(l:l).NE.CHAR(0))THEN
        blanks=0
        itmp=0
        DO WHILE(tlinetmp(l:l).NE.' ')
         IF(tlinetmp(l:l).EQ.CHAR(0))EXIT
         IF(tlinetmp(l:l).EQ.CHAR(32))EXIT
         blanks=blanks+1
         IF(blanks.GT.50)EXIT
         i=i+1
         IF(i.EQ.k)GOTO 9998
         itmp=itmp+1
         tlineout(i:i)=tlinetmp(l:l)
         l=l+1
         IF(tlinetmp(l:l).EQ.CHAR(0))EXIT
         IF(itmp.EQ.width-1)EXIT
        ENDDO
        DO WHILE(itmp.LT.width)
         i=i+1
         IF(i.EQ.k)RETURN
         IF(i.EQ.kk)RETURN
         itmp=itmp+1
         tlineout(i:i)=' '
        ENDDO
       ENDIF
       ENDIF
       IF(tlinetmp(l:l).EQ.CHAR(0))EXIT
      ENDDO

      RETURN

 9998 WRITE (fnumwrk,*) ' Problem in STANDC!! Did not find blank! '
      WRITE (fnumwrk,*) ' Linein : ',tlinein(1:65)
      WRITE (fnumwrk,*) ' Lineout: ',tlineout(1:65)
      WRITE (*,*) ' Problem in STANDC!! Did not find blank! '
      WRITE (*,*) ' Linein : ',tlinein(1:65)
      WRITE (*,*) ' Lineout: ',tlineout(1:65)
      WRITE (*,*) ' Program will have to stop'
      WRITE (*,*) ' Check WORK.OUT for details of run'
      STOP ' '

      END

!-----------------------------------------------------------------------

      SUBROUTINE Start(tchar,K)

      ! Finds where first character on line is positioned

      IMPLICIT NONE

      CHARACTER*(*) tchar
      INTEGER       l,i,k,tvilent

      k=0
      l=Tvilent(tchar)
      IF(l.LE.0)RETURN
      IF(l.GT.1)THEN
       DO i=1,l
        IF(tchar(i:i).NE.' ')EXIT
       ENDDO
      ENDIF
      k=i

      RETURN

      END

!-----------------------------------------------------------------------

      SUBROUTINE Ucase(inchar)

      ! Returns the upper case of a string

      IMPLICIT  NONE

      CHARACTER inchar*(*)
      CHARACTER tl1upcase*1
      INTEGER   l,i,tvilent

      CALL ltrim(inchar)
      i=Tvilent(inchar)

      DO l=1,i
       inchar(l:l)=Tl1upcase(inchar(l:l))
      ENDDO

      END

!-----------------------------------------------------------------------

      SUBROUTINE CSCLEAR5

       WRITE  (*,'(/////)')

      END

!-----------------------------------------------------------------------

      INTEGER FUNCTION CSDOC(YR,DOY)

      ! Calculates day-of-century (01/01/1901 : CSDOC = 1)

      IMPLICIT NONE

      INTEGER DOY,NLEAP,YR

      NLEAP = (YR-1)/4
      CSDOC = NLEAP*366 + (YR-NLEAP-1)*365 + DOY

      END

!-----------------------------------------------------------------------

      FUNCTION CSYEARDOY(YRDOY)

      ! Converts YRDOY to YEARDOY

      IMPLICIT NONE

      INTEGER DOY,YR,YRDOY,CSYEARDOY

      IF (YRDOY.GT.0 .AND. YRDOY.LE.99365) THEN
        YR  = INT(YRDOY / 1000)
        DOY = YRDOY - YR * 1000
        IF (YR .LE. 10) THEN
          CSYEARDOY = (2000 + YR) * 1000 + DOY
        ELSE
          CSYEARDOY = (1900 + YR) * 1000 + DOY
        ENDIF
      ELSE
        CSYEARDOY = YRDOY
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------

      INTEGER FUNCTION CSYDOY(YEAR,DOY)

      ! Converts YR and DOY to YRDOY.

      IMPLICIT NONE

      INTEGER DOY,YR,year

      IF(year.GE.2000)THEN
        YR=YEAR-2000
      ELSEIF(year.GE.1900)THEN
        YR=YEAR-1900
      ELSE
        YR=YEAR
      ENDIF

      CSYDOY = YR*1000 + DOY

      END

!-----------------------------------------------------------------------
      FUNCTION CSTABEX(VAL,ARG,DUMMY,K)

      !  Looks up data from 'table'

      IMPLICIT NONE

      INTEGER K,J,FNUMWRK
      LOGICAL FOPEN

      REAL VAL(K),ARG(K),DUMMY,CSTABEX

      DO 100  J = 2,K
        IF (DUMMY .GT. ARG(J)) GO TO 100
        GO TO 200
  100 CONTINUE
      J = K
      IF (ARG(J)-ARG(J-1).EQ.0.0) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='WORK.OUT',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
        WRITE (fnumwrk,*) ' Divisor in CSTABEX = 0.0!!'
        WRITE (*,*) ' Divisor in CSTABEX = 0.0!!'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF
  200 CSTABEX = (DUMMY-ARG(J-1))*(VAL(J)-VAL(J-1))/(ARG(J)-ARG(J-1))+VAL
     &     (J-1)

      END

!-----------------------------------------------------------------------

      INTEGER FUNCTION CSINCDAT(ADATE, DELTA)

      ! Increases or decrease date (no restriction to 365 days)

      IMPLICIT NONE

      INTEGER NDYR, AYR, ADOY, ADATE, DELTA, CSENDYR, CSYDOY
      EXTERNAL CSENDYR, CSYDOY

      CALL CSYR_DOY(ADATE, AYR, ADOY)
      NDYR = CSENDYR(AYR)
      ADOY = ADOY + DELTA
  100 CONTINUE
      IF (ADOY .GT. NDYR) THEN
        AYR = AYR + 1
        ADOY = ADOY - NDYR
        GO TO 100
      END IF
  200 IF (ADOY .LE. 0) THEN
        AYR = AYR - 1
        NDYR = CSENDYR(AYR)
        ADOY = ADOY + NDYR
        GO TO 200
      END IF
      CSINCDAT = AYR*1000+ADOY

      RETURN

      END

!-----------------------------------------------------------------------

      INTEGER FUNCTION CSENDYR(YR)

      ! Computes end-of-year (365 or 366) depending if leap year.

      IMPLICIT NONE

      INTEGER YR

      IF (MOD(YR,4) .EQ. 0) THEN
        CSENDYR = 366
      ELSE
        CSENDYR = 365
      ENDIF

      END

!-----------------------------------------------------------------------

      REAL FUNCTION CSVPSAT(T)

      ! Calculates saturated vapor pressure of air (Tetens, 1930).

      IMPLICIT NONE

      REAL T

      CSVPSAT = 610.78 * EXP(17.269*T/(T+237.30))

      END

!-----------------------------------------------------------------------

      INTEGER FUNCTION CSMTHEND(YR,MTH)

      ! Calculates day-of-year that is end of month.

      IMPLICIT NONE

      INTEGER MTH,MEND(12),YR
      LOGICAL LEAPYR

      DATA MEND/31,59,90,120,151,181,212,243,273,304,334,365/

      LEAPYR = MOD(YR,4) .EQ. 0
      IF (LEAPYR .AND. MTH.GE.2) THEN
        CSMTHEND = MEND(MTH) + 1
      ELSE
        CSMTHEND = MEND(MTH)
      ENDIF

      END

!-----------------------------------------------------------------------

      INTEGER FUNCTION CSMTHMID(YR,MTH)

      ! Calculates day-of-year that is midpoint of month.

      IMPLICIT NONE

      INTEGER MTH,YR,MIDPT(12)
      LOGICAL LEAPYR

      DATA MIDPT/16,46,75,106,136,167,197,228,259,289,320,350/

      LEAPYR = MOD(YR,4) .EQ. 0
      IF (LEAPYR .AND. MTH.GE.2) THEN
        CSMTHMID = MIDPT(MTH) + 1
      ELSE
        CSMTHMID = MIDPT(MTH)
      ENDIF

      END

!-----------------------------------------------------------------------

      INTEGER FUNCTION CSTIMDIF(YRDOY1IN,YRDOY2IN)

      ! Calculates time difference between two YRDOY or YEARDOY dates

      IMPLICIT NONE

      INTEGER CSDOC,DOY1,DOY2,YR1,YR2,YRDOY1,YRDOY2,YRDOY1IN,YRDOY2IN

      YRDOY1 = YRDOY1IN
      YRDOY2 = YRDOY2IN

      ! Simple time difference two days in the same year attempted first
      CSTIMDIF = YRDOY2 - YRDOY1

      ! If time difference involves year change, use CSDOC calculations.
      IF (CSTIMDIF .GT. 365 .OR. CSTIMDIF .LT. -365) THEN
        IF (YRDOY1.GT.2000000) YRDOY1 = YRDOY1-2000000
        IF (YRDOY1.GT.1900000) YRDOY1 = YRDOY1-1900000
        YR1  = INT(YRDOY1 / 1000)
        DOY1 = YRDOY1 - YR1 * 1000
        IF (YR1.LT.10) YR1 = YR1+100
        IF (YRDOY2.GT.2000000) YRDOY2 = YRDOY2-2000000
        IF (YRDOY2.GT.1900000) YRDOY2 = YRDOY2-1900000
        YR2  = INT(YRDOY2 / 1000)
        DOY2 = YRDOY2 - YR2 * 1000
        IF (YR2.LT.10) YR2 = YR2+100
        CSTIMDIF = CSDOC(YR2,DOY2) - CSDOC(YR1,DOY1)
      ENDIF

      END

!-----------------------------------------------------------------------

      FUNCTION Dapcalc(datein,pyrin,pdoyin)

      ! Calculates days after planting from date and planting DATA

      IMPLICIT NONE

      INTEGER dapcalc,datein,year,day,yeartmp,dateinwr
      INTEGER pdoy,pdoyin,pyr,pyrin

      IF(datein.LT.-90 .OR. pyrin.LT.-90 .OR. pdoyin.LT.-90)THEN
       dapcalc=-99
       RETURN
      ENDIF

      dapcalc=-99

      DATEINWR = DATEIN
      IF(DATEIN.GT.2000000)DATEINWR=DATEIN-2000000
      IF(DATEIN.GT.1900000)DATEINWR=DATEIN-1900000

      pdoy=pdoyin
      IF(pyrin.GT.99)THEN
       pyr=MOD(pyrin,100)
      ELSE
       pyr=pyrin
      ENDIF
      IF(pyr.LT.20)THEN
       pyr=2000+pyr
      ELSE
       pyr=1900+pyr
      ENDIF

      IF(dateinwr.GT.1000)THEN
       day=MOD(dateinwr,1000)
       yeartmp=(dateinwr-day)/1000
       IF(yeartmp.GT.99)THEN
        year=MOD(yeartmp,1000)
        IF(year.GT.99)year=MOD(year,100)
       ELSE
        year=yeartmp
       ENDIF
      ELSE
       day=dateinwr
       year=-99
      ENDIF

      IF(year.GT.-99)THEN
       IF(year.LT.20)THEN
        year=2000+year
       ELSE
        year=1900+year
       ENDIF
      ENDIF

      IF(year.EQ.-99)THEN
       IF(day.LE.pdoy)THEN
        IF(MOD(pyr,4).EQ.0)THEN
         dapcalc=(366-pdoy)+day
        ELSE
         dapcalc=(365-pdoy)+day
        ENDIF
       ELSE
        dapcalc=day-pdoy
       ENDIF
      ENDIF

      IF(year.NE.-99)THEN
       IF(year.EQ.pyr)THEN
        dapcalc=day-pdoy
       ELSEIF(year.EQ.pyr+1)THEN
        IF(MOD(pyr,4).EQ.0)THEN
         dapcalc=(366-pdoy)+day
        ELSE
         dapcalc=(365-pdoy)+day
        ENDIF
       ELSEIF(year.EQ.pyr+2)THEN
        IF(MOD(pyr,4).EQ.0)THEN
         dapcalc=(366-pdoy)+365+day
        ELSE
         IF(MOD(pyr+1,4).EQ.0)THEN
          dapcalc=(365-pdoy)+366+day
         ELSE
          dapcalc=(365-pdoy)+365+day
         ENDIF
        ENDIF
       ENDIF
      ENDIF

      IF(dapcalc.LT.0)dapcalc=-99

      RETURN

      END

C-------------------------------------------------------------------------------
      FUNCTION Tfac4(tcard,temp,TUNIT)

      ! Calculate temp factor and thermal units from cardinal temps

      REAL tfac4,tcard(4),temp,tunit

      IF (temp.LE.tcard(1)) THEN
        tfac4 = 0.0
      ELSEIF (temp.GT.tcard(1) .AND. temp.LE.tcard(2)) THEN
        tfac4 = (temp-tcard(1))/(tcard(2)-tcard(1))
      ELSEIF (temp.GT.tcard(2) .AND. temp.LE.tcard(3)) THEN
        tfac4 = 1.0
      ELSEIF (temp.GT.tcard(3) .AND. temp.LE.tcard(4)) THEN
        tfac4 = 1.0 - ((temp-tcard(3))/(tcard(4)-tcard(3)))
      ELSEIF (temp.GT.tcard(4)) THEN
        tfac4 = 0.0
      ENDIF

      tfac4 = AMAX1(0.0,AMIN1(1.0,tfac4))
      tunit = tfac4 * (tcard(2)-tcard(1))

      RETURN
      END

!-----------------------------------------------------------------------

      FUNCTION Tvicolnm(tl1801,header)

      ! Reads a character string and returns column # for identifier

      IMPLICIT NONE

      CHARACTER header*(*)
      CHARACTER tl1801*180
      CHARACTER headtmp*31
      INTEGER tvicolnm,ncol,l,headfound,istart,iend,i,lhead,tvilent

      lhead=Tvilent(header)
      l=0                                              ! Find column #
      ncol=0
      tvicolnm=0
      headfound=0
      headtmp='                               '

      IF(tl1801(1:1).EQ.'@')tl1801(1:1)=' '

      DO WHILE(headfound.LT.1.AND.l.LT.170)
       istart=0
       iend=0
       i=l
       DO WHILE (istart.LT.1)
        i=i+1
        IF(i.GE.170)THEN
         istart=i
         EXIT
        ENDIF
        IF(tl1801(i:i).NE.' ')THEN
         istart=i
         EXIT
        ENDIF
       ENDDO
       i=istart
       DO WHILE (iend.LT.1)
        i=i+1
        IF(i.GE.170)THEN
         iend=i
         EXIT
        ENDIF
        IF(tl1801(i:i).EQ.' ')THEN
         iend=i-1
         EXIT
        ENDIF
       ENDDO
       l=i
       IF((iend-istart).GT.0.OR.iend.EQ.istart)ncol=ncol+1
       IF(iend-istart.LT.30)THEN
        headtmp=tl1801(istart:iend)
       ELSE
        headtmp=tl1801(istart:istart+30)
       ENDIF
       IF(headtmp(1:lhead).EQ.header)THEN
        headfound=1
        EXIT
       ENDIF
      END DO

      IF(headfound.GT.0)tvicolnm=ncol

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Tvilent(tchar)

      ! Returns the string length after trimming off trailing blanks

      IMPLICIT NONE

      CHARACTER   tchar*(*)
      INTEGER     tvilent
      INTEGER     i,length,l

      l=LEN(tchar)
      length=l
      DO i=l,1,-1
       IF(tchar(i:i).NE.CHAR(32))THEN
        IF(tchar(i:i).NE.CHAR(0))EXIT
       ENDIF
       IF(tchar(i:i).EQ.CHAR(32).OR.tchar(i:i).EQ.CHAR(0))THEN
        length=length-1
       ENDIF
      ENDDO
      tvilent=length

      END

!-----------------------------------------------------------------------

      FUNCTION Tvinstr(tline)

      ! Returns number of strings in a line

      IMPLICIT NONE

      CHARACTER*(*) tline
      INTEGER       tvinstr,i,n,tint,tvilent,switch,psw

      n=0
      psw=0
      tint=Tvilent(tline)
      DO i=1,tint
       IF(tline(i:i).EQ.' ')THEN
        switch=0
       ELSE
        switch=1
       ENDIF
       IF(switch.GT.psw) n=n+1
       psw=switch
      ENDDO
      tvinstr=n

      END

!-----------------------------------------------------------------------

      FUNCTION Tvifromc(charnum)

      ! Change character string to an integer

      IMPLICIT NONE

      CHARACTER  charnum*(*),newchar*10
      INTEGER    number,pos,i,lenchar,tvifromc,tvilent,fnumwrk
      LOGICAL    fopen

      lenchar=Tvilent(charnum)
      IF(charnum(lenchar:lenchar).EQ.'r'.OR.
     X charnum(lenchar:lenchar).EQ.'R')THEN
       i=lenchar-1
       charnum=charnum(1:i)
      ENDIF
      lenchar=Tvilent(charnum)
      IF(lenchar.EQ.0)THEN
       tvifromc=-99
       RETURN
      ENDIF
      DO i=1,lenchar
       IF(charnum(i:i).NE.' '.OR.charnum(i:i).NE.'0')THEN
        pos=i
        newchar=charnum(pos:lenchar)
        EXIT
       ENDIF
      ENDDO
      lenchar=Tvilent(newchar)
      IF(lenchar.EQ.1)THEN
       READ(newchar,'(i1)',ERR=9991)number
      ELSEIF(lenchar.EQ.2)THEN
       READ(newchar,'(i2)',ERR=9991)number
      ELSEIF(lenchar.EQ.3)THEN
       READ(newchar,'(i3)',ERR=9991)number
      ELSEIF(lenchar.EQ.4)THEN
       READ(newchar,'(i4)',ERR=9991)number
      ELSEIF(lenchar.EQ.5)THEN
       READ(newchar,'(i5)',ERR=9991)number
      ELSEIF(lenchar.EQ.6)THEN
       READ(newchar,'(i6)',ERR=9991)number
      ELSEIF(lenchar.EQ.7)THEN
       READ(newchar,'(i7)',ERR=9991)number
      ELSEIF(lenchar.EQ.8)THEN
       READ(newchar,'(i8)',ERR=9991)number
      ELSEIF(lenchar.EQ.9)THEN
       READ(newchar,'(i9)',ERR=9991)number
      ELSEIF(lenchar.EQ.10)THEN
       READ(newchar,'(i10)',ERR=9991)number
      ELSEIF(lenchar.EQ.11)THEN
       READ(newchar,'(i11)',ERR=9991)number
      ELSEIF(lenchar.EQ.12)THEN
       READ(newchar,'(i12)',ERR=9991)number
      ELSEIF(lenchar.GE.12)THEN
       CALL Getlun('WORK.OUT',fnumwrk)
       INQUIRE (FILE='WORK.OUT',OPENED=fopen)
       IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
       WRITE(fnumwrk,*)
     &  ' Cannot convert due to too large number: ',newchar
      ENDIF
      tvifromc=number
      RETURN
 9991 CONTINUE
      tvifromc=-99

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Tvrfromc(charnum)

      ! Change character string to a real

      IMPLICIT NONE

      CHARACTER  charnum*(*),newchar*10
      INTEGER    pos,i,lenchar,tvilent,tvifromc,fnumwrk
      REAL       tvrfromc,number
      LOGICAL    fopen

      lenchar=Tvilent(charnum)
      IF(charnum(lenchar:lenchar).EQ.'r'.OR.
     X charnum(lenchar:lenchar).EQ.'R')THEN
       i=lenchar-1
       charnum=charnum(1:i)
      ENDIF
      lenchar=Tvilent(charnum)
      IF(lenchar.EQ.0)THEN
       tvifromc=-99
       RETURN
      ENDIF
      DO i=1,lenchar
       IF(charnum(i:i).NE.' '.OR.charnum(i:i).NE.'0')THEN
        pos=i
        newchar=charnum(pos:lenchar)
        EXIT
       ENDIF
      ENDDO
      lenchar=Tvilent(newchar)
      IF(lenchar.EQ.1)THEN
       READ(newchar,'(F1.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.2)THEN
       READ(newchar,'(F2.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.3)THEN
       READ(newchar,'(F3.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.4)THEN
       READ(newchar,'(F4.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.5)THEN
       READ(newchar,'(F5.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.6)THEN
       READ(newchar,'(F6.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.7)THEN
       READ(newchar,'(F7.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.8)THEN
       READ(newchar,'(F8.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.9)THEN
       READ(newchar,'(F9.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.10)THEN
       READ(newchar,'(F10.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.11)THEN
       READ(newchar,'(F11.0)',ERR=9991)number
      ELSEIF(lenchar.EQ.12)THEN
       READ(newchar,'(F12.0)',ERR=9991)number
      ELSEIF(lenchar.GE.12)THEN
       CALL Getlun('WORK.OUT',fnumwrk)
       INQUIRE (FILE='WORK.OUT',OPENED=fopen)
       IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
       WRITE(fnumwrk,*)
     &  ' Cannot convert due to too large number: ',newchar
      ENDIF
      tvrfromc=number
      RETURN
 9991 CONTINUE
      tvrfromc=-99.0

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Tl1upcase(inchar)

      ! Returns the upper case of a lower case letter.

      IMPLICIT  NONE

      CHARACTER inchar*1
      CHARACTER tl1upcase*1
      INTEGER   chaval

      chaval=Ichar(inchar)

      IF((chaval.LE.122).AND.(chaval.GE.97))THEN
       tl1upcase=CHAR(chaval-32)
      ELSE
       tl1upcase=inchar
      ENDIF

      END

!-----------------------------------------------------------------------

      FUNCTION tl10fromi(number)

      ! Changes integer to character

      IMPLICIT NONE

      CHARACTER tl10fromi*(*)
      CHARACTER tchar*(80)
      INTEGER number,digitnew(20),numb,newn,i,tvilent,lentvc,lentchar,l

      IF(number.LT.10)THEN
       tl10fromi=CHAR(48+number)
      ELSE
       tl10fromi=' '
       DO i=1,80
        tchar(i:i)=' '
       ENDDO

       numb=0
       newn=number
       DO WHILE (newn.GE.10)
        numb=numb+1
        digitnew(numb)=MOD(newn,10)
        newn=newn/10
       ENDDO
       numb=numb+1
       digitnew(numb)=newn

       DO L=NUMB,1,-1
        tchar=CHAR(48+digitnew(l))
        CALL Ltrim(tchar)
        lentchar=Tvilent(tchar)
        tchar=tchar(1:lentchar)
        CALL Ltrim(tl10fromi)
        lentvc=Tvilent(tl10fromi)
        IF(lentvc.GT.0)THEN
         tl10fromi=tl10fromi(1:lentvc)//tchar(1:lentchar)
        ELSE
         tl10fromi=tchar(1:lentchar)
        ENDIF
       ENDDO
      ENDIF
      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Yval(xyvals,xval)

      ! Y value corresponding to input x

      IMPLICIT NONE

      INTEGER i,l,n,fnumwrk
      REAL    xyvals(10,2),xval,xyvalprv,yval
      LOGICAL fopen

      xyvalprv=-9999.9
      n=0

      DO l=1,10
       n=n+1
       IF(xyvals(l,1).EQ.-99.0)THEN
         n=l-1
        EXIT
       ENDIF
       IF(xyvals(l,1).LE.0.0)THEN
         IF(xyvals(l,1).EQ.xyvalprv)THEN
          n=l-2
          EXIT
         ENDIF
       ENDIF
       xyvalprv=xyvals(l,1)
      ENDDO

      IF (n.LE.0) THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='WORK.OUT',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
        WRITE(fnumwrk,*)' An array entered the Yval function with no  '
        WRITE(fnumwrk,*)' values. This would cause an error in output '
        WRITE(fnumwrk,*)' values, and may also cause the computer to'
        WRITE(fnumwrk,*)' hang. Please check.'
        WRITE (*,*) ' An array entered the Yval function with no  '
        WRITE (*,*) ' values. This would cause an error in output '
        WRITE (*,*) ' values, and may also cause the computer to'
        WRITE (*,*) ' to hang.'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      IF(xval.LE.xyvals(1,1))THEN
       Yval=xyvals(1,2)
       RETURN
      ENDIF
      IF(xval.GE.xyvals(n,1))THEN
       Yval=xyvals(n,2)
       RETURN
      ENDIF
      DO i=2,n
       IF(xval.LE.xyvals(i,1))EXIT
      ENDDO
      Yval=(xval-xyvals(i-1,1))*(xyvals(i,2)-xyvals(i-1,2))/
     X (xyvals(i,1)-xyvals(i-1,1))+xyvals(i-1,2)
      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Yvalxy(xarray,yarray,xval)

      ! Y value corresponding to input x using two input arrays

      IMPLICIT NONE

      REAL    xarray(10),yarray(10),yvalxy,xyvalprv,xval
      INTEGER n,l,i,fnumwrk
      LOGICAL fopen

      xyvalprv=-9999.9
      n=0

      DO l=1,10
       n=n+1
       IF(xarray(l).EQ.-99.0)THEN
         n=l-1
        EXIT
       ENDIF
       IF(xarray(l).LE.0.0)THEN
         IF(xarray(l).EQ.xyvalprv)THEN
          n=l-2
          EXIT
         ENDIF
       ENDIF
       xyvalprv=xarray(l)
      ENDDO

      IF(n.LE.0)THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='WORK.OUT',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
        WRITE(fnumwrk,*)' An array entered the Yvalxy function with no'
        WRITE(fnumwrk,*)' values. This would cause an error in output '
        WRITE(fnumwrk,*)' values, and may also  cause the computer to'
        WRITE(fnumwrk,*)' hang. Please check.'
        WRITE (*,*) ' An array entered the Yvalxy function with no  '
        WRITE (*,*) ' values. This would cause an error in output '
        WRITE (*,*) ' values, and may also cause the computer to'
        WRITE (*,*) ' to hang.'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      IF(xval.LE.xarray(1))THEN
       Yvalxy=yarray(1)
       RETURN
      ENDIF

      IF(xval.GE.xarray(n))THEN
       Yvalxy=yarray(n)
       RETURN
      ENDIF

      DO i=2,n
       IF(xval.LE.xarray(i))EXIT
      ENDDO
      Yvalxy=(xval-xarray(i-1))*(yarray(i)-yarray(i-1))/
     X (xarray(i)-xarray(i-1))+yarray(i-1)

      RETURN

      END

!-----------------------------------------------------------------------

      FUNCTION Yval1(yvals,lowerc,upperc,xval)

      ! Y value corresponding to input x from array where x values
      ! are given at values that equal the indices of the y array.

      IMPLICIT NONE

      INTEGER      l,n,lower,upper,xvali,tvifromc,adjust,fnumwrk
      INTEGER      loweradj,upperadj,xvaliadj,lowerpos,upperpos
      REAL         yvals(*),xval,yval1,xvalfr
      CHARACTER(*) lowerc,upperc
      LOGICAL      fopen

      LOWER = TVIFROMC(LOWERC)
      UPPER = TVIFROMC(UPPERC)
      XVALI = INT(XVAL)
      XVALFR = XVAL - FLOAT(XVALI)

      ! Adjust for offsett
      ADJUST = 1 - LOWER
      LOWERADJ = LOWER + ADJUST
      UPPERADJ = UPPER + ADJUST
      XVALIADJ = INT(XVAL) + ADJUST

      n=0
      lowerpos = 0
      upperpos = 0
      DO l=LOWERADJ,UPPERADJ
       IF (yvals(l).NE.-99.0) THEN
         n = n+1
         IF (LOWERPOS.LE.0) lowerpos = l
       ELSE
         IF (LOWERPOS.GT.0) THEN
           IF (UPPERPOS.LE.0) UPPERPOS = L-1
         ENDIF
       ENDIF
      ENDDO
      IF (UPPERPOS.EQ.0) UPPERPOS=UPPERADJ

      IF(n.LE.0)THEN
        CALL Getlun('WORK.OUT',fnumwrk)
        INQUIRE (FILE='WORK.OUT',OPENED=fopen)
        IF (.NOT.fopen) OPEN(UNIT=fnumwrk,FILE='WORK.OUT')
        WRITE(fnumwrk,*)' An array entered the Yval1 function with no  '
        WRITE(fnumwrk,*)' values. This would cause an error in output '
        WRITE(fnumwrk,*)' values, and may also cause the computer to'
        WRITE(fnumwrk,*)' hang. Please check.'
        WRITE (*,*) ' An array entered the Yval1 function with no  '
        WRITE (*,*) ' values. This would cause an error in output '
        WRITE (*,*) ' values, and may also cause the computer to'
        WRITE (*,*) ' to hang.'
        WRITE (*,*) ' Program will have to stop'
        WRITE (*,*) ' Check WORK.OUT for details of run'
        STOP ' '
      ENDIF

      IF (XVALIADJ.LT.LOWERPOS) THEN
        YVAL1 = YVALS(LOWERPOS)
      ELSEIF (XVALIADJ.GE.LOWERPOS .AND. XVALIADJ.LT.UPPERPOS) THEN
        YVAL1 = YVALS(XVALIADJ)
     &        + XVALFR*(YVALS(XVALIADJ+1)-YVALS(XVALIADJ))
      ELSE
        YVAL1 = YVALS(UPPERPOS)
      ENDIF

      RETURN

      END

!-----------------------------------------------------------------------
      SUBROUTINE CSFIND(LUNUM,NAME,LNUM,FOUND)

      IMPLICIT NONE

      INTEGER   FOUND,I,LNUM,LUNUM
      CHARACTER SECTION*6,NAME*6,CSUPCASE*1

      FOUND = 0
      LNUM  = 1

      DO I = 1, LEN(NAME)
         NAME(I:I) = CSUPCASE(NAME(I:I))
      END DO

      ! Loop to read through data file.

   10 IF (.TRUE.) THEN
         READ(LUNUM,'(A)',END=20) SECTION
         DO I = 1,LEN(SECTION)
            SECTION(I:I) = CSUPCASE(SECTION(I:I))
         END DO


         IF (NAME .EQ. SECTION) then
            ! String found, set FOUND to 1, and exit loop.
            FOUND = 1
            GOTO 20

          ELSE
            ! String not found, set FOUND to 0.
            FOUND = 0
         ENDIF

         LNUM = LNUM + 1
         GOTO 10
      ENDIF

   20 RETURN
      END


!-----------------------------------------------------------------------
      SUBROUTINE CSIGNORE(LUN,LINEXP,ISECT,CHARTEST)

      IMPLICIT NONE

      CHARACTER BLANK*(80),CHARTEST*(*)
      INTEGER   LUN,LINEXP,ISECT

      DATA BLANK/'                                                    '/

      ISECT = 1
 30   READ(LUN,'(A)',ERR=70,END=70)CHARTEST
      LINEXP = LINEXP + 1
      ! Check to see if all of this section has been read
      IF(CHARTEST(1:1) .EQ. '*' )THEN
         ! End of section encountered
         ISECT = 2
         RETURN
      ENDIF

      ! Check for blank lines and comments (denoted by ! in column 1)
      IF(CHARTEST(1:1).NE.'!' .AND. CHARTEST(1:1).NE.'@') THEN
         IF(CHARTEST(1:80).NE.BLANK)THEN
            ! FOUND A GOOD LINE TO READ
            RETURN
         ENDIF
      ENDIF
      GO TO 30
      ! To read the next line
 70   ISECT = 0
      RETURN
      END

!-----------------------------------------------------------------------
      CHARACTER*1 FUNCTION CSUPCASE (INCHAR)

      IMPLICIT  NONE

      CHARACTER INCHAR*1
      INTEGER   CHAVAL

      CHAVAL = ICHAR(INCHAR)

      IF ((CHAVAL .LE. 122) .AND. (CHAVAL .GE. 97)) THEN
         CSUPCASE = CHAR(CHAVAL-32)
       ELSE
         CSUPCASE = INCHAR
      ENDIF

      END   !FUNCTION CSUPCASE


!-----------------------------------------------------------------------
      FUNCTION CSCURV(CTYPE,XB,X1,X2,XM,X)

      IMPLICIT NONE

      CHARACTER*3 CTYPE
      REAL CSCURV,XB,X1,X2,XM,X

      CSCURV = 1.0
      IF (CTYPE .EQ. 'NON' .OR. CTYPE .EQ. 'non') RETURN

      IF(CTYPE .EQ. 'LIN' .OR. CTYPE .EQ. 'lin') THEN
        CSCURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CSCURV = (X-XB)/(X1-XB)
        IF(X .GE. X1 .AND. X .LE. X2)CSCURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)CSCURV = 1.0 - (X-X2)/(XM-X2)
        CSCURV = MAX(CSCURV,0.0)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      IF(CTYPE .EQ. 'QDR' .OR. CTYPE .EQ. 'qdr') THEN
        CSCURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)CSCURV = 1. -((X1-X)/(X1-XB))**2
        IF(X .GE. X1 .AND. X .LE. X2)CSCURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)CSCURV = 1. - ((X-X2)/(XM-X2))**2
        CSCURV = MAX(CSCURV,0.0)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      ! Type INL is the inverse linear with a minimum for use in
      ! photoperiod calculations. In this case, XM is the lowest
      ! relative rate, X1 and X2 are critical dayl

      IF(CTYPE .EQ. 'INL' .OR. CTYPE .EQ. 'inl') THEN
        CSCURV = 1.0
        IF(X.GT.X1 .AND. X.LT.X2)CSCURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
        IF(X .GT. X2) CSCURV = XM
        CSCURV = MAX(CSCURV,XM)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      ! Type SHO for use with short day plants.
      ! The curve is the inverse linear with a minimum for use in
      ! photoperiod calculations. In this case, XM is the lowest
      ! relative rate, X1 and X2 are critical dayl

      IF(CTYPE .EQ. 'SHO' .OR. CTYPE .EQ. 'sho') THEN
        IF (X .LE. X1) THEN
           CSCURV = 1.0
        ELSE IF ((X .GT. X1) .AND. (X .LT. X2)) THEN
           CSCURV = 1.-(1.-XM)*((X-X1)/(X2-X1))
        ELSE IF (X .GE. X2) THEN
          CSCURV = XM
        ENDIF
        CSCURV = MAX(CSCURV,XM)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      ! Curve type LON for use with long day plants.
      ! Type is the inverse linear with a minimum for use in
      ! photoperiod calculations. In this case, XM is the
      ! lowest relative rate, X1 and X2 are critical dayl

      IF(CTYPE .EQ. 'LON' .OR. CTYPE .EQ. 'LON') THEN
        IF (X .LT. X2) THEN
           CSCURV = XM
        ELSE IF ((X .GE. X2) .AND. (X .LT. X1)) THEN
           CSCURV = 1.-(1.-XM)*((X1-X)/(X1-X2))
        ELSE
           CSCURV = 1.0
        ENDIF
        CSCURV = MAX(CSCURV,XM)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      IF(CTYPE .EQ. 'SIN' .OR. CTYPE .EQ. 'sin') THEN
        CSCURV = 0.
        IF(X .GT. XB .AND. X .LT. X1)
     &   CSCURV = 0.5*(1.+COS(2.*22./7.*(X-X1)/(2.*(X1-XB))))
        IF(X .GE. X1 .AND. X .LE. X2)CSCURV = 1.
        IF(X .GT. X2 .AND. X .LT. XM)
     &   CSCURV = 0.5*(1.+COS(2.*22./7.*(X2-X)/(2.*(XM-X2))))
        CSCURV = MAX(CSCURV,0.0)
        CSCURV = MIN(CSCURV,1.0)
      ENDIF

      END   !FUNCTION CURV

