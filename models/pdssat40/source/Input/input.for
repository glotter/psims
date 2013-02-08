C=======================================================================
C  COPYRIGHT 1998-2005 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      International Fertilizer Development Center, AL    
C                      Iowa State University, Iowa
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  INPUT, Main program
C
C  INPUT MODULE FOR DSSAT MODELS,  DSSAT v4.02
C
C  August 31, 2005               Gerrit Hoogenboom
C
C
C  Reads FileX, includes sensitivity analysis and writes a
C  temporary output file for input by the crop models
C-----------------------------------------------------------------------
C  Revision history
C
C 04/26/1993 GH  Written
C 05/28/1993 PWW Header revision and minor changes
C 01/12/1993 WTB Modified for soil P model                     .
C 06/12/1994 GH  Set to 1994 version (MINPT940.EXE)
C 02/15/1995 GH  Set to 1995 version (MINPT950.EXE)
C 03/25/1996 GH  Modified to add OILCROP Sunflower model
C 15/01/1996 GH  Add DSSAT v3.1 file structure for fileX
C 12/31/1996 GH  Modified to add chickpea and pigeonpea
C 01/03/1997 GH  Modified to add ALOHA Pineapple model
C 01/19/1997 GH  Modified to add pepper
C 09/29/1997 GH  Modified to add cotton and CSCOT model
C 05/06/1998 GH  Changed to May 15, 1998, DSSAT v3.5
C 05/07/1998 GH  Modified to add velvetbean
C 09/11/1998 GH  Modified to add cowpea
C 05/23/1999 GH  Changed to May 31, 1999, DSSAT v3.51
C 08/17/1999 GH  Modified to add cabbage
C 07/03/2000 GH  Modified for CVF compiler and modular CROPGRO
C 09/20/2000 GH  Modifed to add Brachiaria decumbens
C 09/20/2000 GH  Changed to September 20, 2000, DSSAT v3.7
C 11/04/2001 GH  Added CASUPRO model
C 12/13/2001 GH  Modified to fit with the CSM model
C 01/30/2002 GH  Modified for the new wheat model
C 04/15/2002 GH  Modified for sequence analysis
C 04/20/2002 GH  Modified temporary output file
C 06/06/2002 GH  Modified for Y2K output
C 12/25/2002 GH  Changed to December, 2002. CSM v3.9
C 03/31/2004 GH  Official release DSSAT v4.0, CSM040
C 08/31/2005 GH  Official release DSSAT Version 4.0.2.0, CSM040
C-----------------------------------------------------------------------
C  INPUT  : None
C
C  LOCAL  : WMODI,WMODB,CROP,PRCROP,VARNO,VARTY,ERRKEY,ECOTYP,ECONO,
C           MODEL,FILEIO,ECONAM,VRNAME,TITLER,PATHMO,NLOOP,FROP,FTYPEN,RUN
C           LNSA,LNIC,LUNIO,NYRS,ERRNUM,ENDSIM,SENSMS,NSENS,YRIC,SEQNO
C           IVRTEM,IVRGRP,IPLT,ISIM,BEXIST,WRESR,WRESND,TSOC,PM06
C           PM09,SWINIT(20),INO3(20),INH4(20),TOTN(20),EFINOC,EFNFIX
C           AINO3,AINH4,TNMIN,ANO3,ANH4,TSWINI,ESW(20),SW(20),TLL,TSW,TDUL
C           TSAT,TPESW,CUMDEP,PESW,CO2,CLDVAR,THVAR,SDPRO,TRIFOL,SIZELF
C           THRESH,LNGSH,RHGHT,RWIDTH
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called :
C
C  Calls  : ERROR CLEAR INTRO IPEXP IPSOIL IPVAR IPECO IPSLIN IPSLAN
C           SENS INSOIL WEATHR 
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================
      PROGRAM INPUT_PROGRAM

      USE ModuleDefs
      IMPLICIT NONE

      INCLUDE 'COMSOI.BLK'
      INCLUDE 'COMIBS.BLK'
      INCLUDE 'COMSWI.BLK'

      CHARACTER*  1 WMODI
      CHARACTER*  2 CROP,PRCROP
      CHARACTER*  3 TRNARG
      CHARACTER*  5 CRMODEL
      CHARACTER*  6 VARNO,VARTY,ERRKEY,ECOTYP,ECONO,MODELV
      CHARACTER* 12 MODEL,INPUT
      CHARACTER* 16 ECONAM,VRNAME
	CHARACTER* 18 RUNARG
      CHARACTER* 25 TITLET
	CHARACTER* 30 FILEIO
      CHARACTER* 42 CHEXTR(200)
      CHARACTER*120 INPUTX
      CHARACTER*120 WTHSTR

      INTEGER       NLOOP,FROP,FTYPEN,RUN,IIRV(NAPPL)
      INTEGER       LUNIO,NYRS,ERRNUM,NSENS,YRIC
      INTEGER       IVRGRP,IPLT,ISIM,EXPP,EXPN,TRTN,TRTALL
      INTEGER       NFORC,NDOF,PMTYPE,ISENS
	INTEGER       LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE
	INTEGER       LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES

      INTEGER       IP,IPX
C-SUN INTEGER       LNBLNK

      LOGICAL       FEXIST,INITIAL

      REAL          WRESR,WRESND,TSOC,SWINIT(NL),CO2
      REAL          INO3(NL),INH4(NL),EFINOC,EFNFIX
      REAL          AINO3,AINH4,TNMIN,ANO3,ANH4,TSWINI
      REAL          ESW(NL),SW(NL),TLL,TSW,TDUL,TSAT,TPESW,CUMDEP,PESW
      REAL          PLTFOR

      PARAMETER (ERRKEY = 'INPUT ')
      PARAMETER (LUNIO  = 21)

C-----------------------------------------------------------------------
C     Get argument from runtime module to determine path and run mode
C-----------------------------------------------------------------------
C   Fortran Compaq Visual Fortran
C-----------------------------------------------------------------------
      CALL GETARG (0,INPUTX)
      IPX = LNBLNK(INPUTX)
      CALL PATHD  (DSSATP,INPUTX,IPX)
      CALL GETARG (1,FILEIO)
      CALL GETARG (2,RNMODE)
      CALL GETARG (3,RUNARG)
      CALL GETARG (4,FILEX)
      CALL GETARG (5,TRNARG)
	
C-----------------------------------------------------------------------
C   SUN Fortran 
C-----------------------------------------------------------------------
C-SUN CALL GETARG (0,INPUTX)
C-SUN IP = LNBLNK(INPUTX)
C-SUN CALL PATHD  (DSSATP,INPUTX,IP)
C-SUN CALL GETARG (1,MODEL)
C-SUN CALL GETARG (2,FILEIO)

C-SUN CALL GETARG (4,RNMODE)
C-SUN CALL GETARG (5,EXPARG)
C-SUN CALL GETARG (6,TRNARG)
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INPUT = INPUTX((IPX-11):IPX)
      READ(RUNARG(1:18),'(I18)') RUN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Temporarily fix input file name for debugging purposes:
!     To use these Debug lines of code (letter D in column 1) with CVF:
!     1) Go to pull down menu Project -> Settings -> Fortran (Tab) ->
!       Debug (Category) -> Check box for Compile Debug(D) Lines
!     2)  Specify name of input module here:
C-DDD INPUT = "MINPT040.EXE"
C-DDD DSSATP = 'C:\DSSAT4\DSSATPRO.FLE'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C-----------------------------------------------------------------------
C    Initialize and delete previous copy of FILEIO
C-----------------------------------------------------------------------
      INQUIRE (FILE = FILEIO,EXIST = FEXIST)
      IF (FEXIST) THEN
          OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
	    READ (LUNIO,40) EXPP,TRTN,TRTALL
          READ (LUNIO,70,IOSTAT=ERRNUM) IOX,IDETO,IDETS,FROP,IDETG,
     &            IDETC,IDETW,IDETN,IDETP,IDETD,IDETL,IDETH,IDETR
          CLOSE (LUNIO,STATUS = 'DELETE')
      ENDIF

C-----------------------------------------------------------------------
C     BEGINNING of READING INPUT files
C-----------------------------------------------------------------------
      IF (RNMODE .EQ. 'I' .AND. RUN .EQ. 1) THEN
        CALL CLEAR
        CALL INTRO
      ENDIF
      NSENS  = 0
      ISENS  = 0

C-----------------------------------------------------------------------
C     Call IPEXP
C-----------------------------------------------------------------------
      CALL IPEXP (MODEL,RUN,DS,SLNO,NYRS,VARNO,CROP,WMODI,
     &     FROP,TRTN,EXPP,EXPN,TITLET,TRTALL,TRNARG,IIRV,
     &     FTYPEN,CHEXTR,NFORC,PLTFOR,NDOF,PMTYPE,INPUT,MODELV,
     &     LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
     &     LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES, CRMODEL)

C-----------------------------------------------------------------------
C     Call IPSOIL
C-----------------------------------------------------------------------
      CALL IPSOIL (RNMODE,FILES,PATHSL,NSENS,ISWWAT)

C-----------------------------------------------------------------------
C     Call IPVAR 
C-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        CALL IPVAR (FILEG,NSENS,RNMODE,VARNO,VARTY,VRNAME,PATHGE,
     &              ECONO,CROP)
c 	  IF (INDEX('GROCSMCAN',MODELV(1:3)) .GT. 0) THEN 
c            CALL IPECO (FILEE,NSENS,RNMODE,PATHEC,ECOTYP,ECONAM,
c     &                 ECONO,IVRGRP,MODEL)
c	  ELSE 
c	      ECONAM = '                '
c        ENDIF
      ENDIF

C-----------------------------------------------------------------------
C     Call IPSLIN to read initial soil conditions
C-----------------------------------------------------------------------
      IF (ISWWAT .NE. 'N' .AND. MESIC .EQ. 'M') THEN
         CALL IPSLIN (FILEX,LNIC,NLAYR,DUL,YRIC,PRCROP,WRESR,
     &        WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,SWINIT,INH4,INO3,
     &        ISWWAT,ISWNIT,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID,YRSIM)
         IF (ISIMI .EQ. 'I') THEN
           IF (YRIC .LT. YRSIM .AND. YRIC .GT. 0) THEN
             YRSIM = YRIC
             CALL YR_DOY (YRSIM,YEAR,ISIM)
             IF (MEWTH .EQ. 'M' .OR. MEWTH .EQ. 'G') THEN
                WRITE (FILEW(5:6),'(I2)') YEAR
             ENDIF
           ENDIF
         ENDIF
	
C-----------------------------------------------------------------------
C        Call IPSLAN to read soil analysis information
C-----------------------------------------------------------------------
         IF (ISWNIT .EQ. 'Y') THEN
            CALL IPSLAN (FILEX,LNSA,BD,OC,PH,PEDON,SLNO,DS,EXTP,TOTN)
         ENDIF
      ENDIF
	
C-----------------------------------------------------------------------
C        Sensitivity Analysis Section
C-----------------------------------------------------------------------
      IF (INDEX('IE',RNMODE) .GT. 0 .AND. NYRS .EQ. 1) THEN
	   IF (INDEX('I',RNMODE) .GT. 0) THEN
           NLOOP = 0
  300      CONTINUE
           NLOOP = NLOOP + 1
           IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
           CALL CLEAR
           WRITE (*,400)
           READ (5,'(I2)',ERR = 300) NSENS
	   ELSE
	     NSENS = 1
	   ENDIF
         IF (NSENS .EQ. 1) THEN
            INITIAL = (ISWWAT .EQ.'N')
            CALL SENS (NSENS,VARNO,VARTY,VRNAME,FTYPEN,LNIC,LNSA,
     &        WRESR,WRESND,ISIM,NYRS,IPLT,WMODI,ECONO,ECONAM,ECOTYP,
     &        PRCROP,SWINIT,INO3,INH4,RUN,FROP,YRIC,EFINOC,EFNFIX,
     &        CROP,IVRGRP,ISENS,MODELV, CRMODEL)
            IF (INITIAL) THEN
               IF ((ISWNIT .EQ. 'Y') .OR. (ISWWAT .NE.'N')) THEN
                  NSENS = 0
                  CALL IPSOIL (RNMODE,FILES,PATHSL,NSENS,ISWWAT)
                  CALL IPSLIN (FILEX,LNIC,NLAYR,DUL,YRIC,PRCROP,
     &                 WRESR,WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,
     &                 SWINIT,INH4,INO3,ISWWAT,ISWNIT,
     &                 ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID,YRSIM)
                  CALL IPSLAN (FILEX,LNSA,BD,OC,PH,PEDON,SLNO,
     &                         DS,EXTP,TOTN)
                  NSENS = 1
               ENDIF
            ENDIF
         ENDIF
         WRITE (*,1000) RUN
         READ (5,'(A25)') TITLER
         IF (TITLER .EQ. '                         ') THEN
            TITLER = TITLET
         ENDIF
       ELSE
         TITLER = TITLET
      ENDIF
      
C-----------------------------------------------------------------------
C     Call INSOIL to calculate initial conditions for each soil layer
C-----------------------------------------------------------------------

      CALL INSOIL (ISWWAT,ISWNIT,AINO3,ANO3,AINH4,ANH4,TNMIN,
     &  SWINIT,TSWINI,NLAYR,DUL,LL,ESW,DLAYR,SAT,SW,TLL,TDUL,
     &  TSAT,TPESW,CUMDEP,PESW,TSW,BD,INO3,INH4,TSOC,OC,PH,
     &  RESN,RESP,RESIDUE,RINP,DEPRES,ICRES,ICREN,ICREP,ICRIP,
     &  ICRID,NARES,YRSIM,RESAMT,RESDAY,SLTX,SLTXS,TOTN)
      
C-----------------------------------------------------------------------
C     Call WEATHR to set CO2 conditions and weather parameter modifications
C-----------------------------------------------------------------------

      CALL WEATHR (CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,PRCADJ,
     &     PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,TXADJ,TXFAC,WMODI,WNDADJ,
     &     WNDFAC,WTHADJ,CO2,WTHSTR,NEV)
      
C-----------------------------------------------------------------------
C     Write temporary output files for runtime modules
C-----------------------------------------------------------------------
C     Write DSSAT Format Version 4 Output file for input by Version 4
C     
C-----------------------------------------------------------------------
      
        CALL OPTEMPY2K(YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,
     &            SWINIT,INH4,INO3,NYRS,VARNO,VRNAME,CROP,MODEL,
     &            RUN,FILEIO,EXPN,ECONO,FROP,TRTALL,TRTN,
     &            CHEXTR,NFORC,PLTFOR,NDOF,PMTYPE,ISENS)
      
        CALL OPTEMPXY2K (YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,
     &           SWINIT,INH4,INO3,NYRS,VARNO,VRNAME,CROP,
     &           FILEIO,FROP,MODEL,ECONO,
     &           LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
     &           LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
     &           NFORC,PLTFOR,PMTYPE,NDOF,CHEXTR)

C-----------------------------------------------------------------------
C     Write DSSAT Format Version 4 Output files
C-----------------------------------------------------------------------
      
      CALL OPGEN (CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,
     &     TSWINI,RUN,MODEL,CROP,CROPD,TITLET,ECONO,VARTY,
     &     ESW,SWINIT,INO3,INH4,TSOC,WTHSTR,NYRS)

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------

  40  FORMAT (36X,3(1X,I5))
  70  FORMAT (17(/),14X,3(5X,A1),4X,I2,9(5X,A1))
 400  FORMAT (/////,5X,'What Would You Like To Do ?',
     &            //,1X,' 0. Run Simulation.',
     &             /,1X,' 1. Select Sensitivity Analysis Options.',
     &            //,1X,'    CHOICE ?   [ Default = 0 ] ===> ',$)
 1000 FORMAT (/,5X,'Please enter Run',I3,' name : ===> ',$)

      END PROGRAM INPUT_PROGRAM
