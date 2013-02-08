C=======================================================================
C  OPHEAD, Subroutine
C
C  Prints inputs for the simulation to the screen
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1993 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C  05/28/1993 PWW Added switch block, etc.
C  04/01/1996 GH  Added comsoi.blk and comibs.blk
C  12/11/2000 GH  Minor output fixes due to 80 character stringlimit
C  01/29/2002 CHP OPHEAD writes to generic HEADER.OUT file for use
C                   by model output routines.
C  08/19/2002 GH  Modified for Y2K
C  05/08/2003 CHP Added version information and date-time stamp to header
C
C-----------------------------------------------------------------------
C  INPUT  : WSTA,PEDON,TRTNO,CUMDEP,NFERT,TOTNAP,NARES,RESAMT,NAPW,TOTAPW,
C           SLDESC,TPESW,ROWSPC,PLTPOP,VRNAME,AINO3,AINH4,YEAR,SLTX,LUNOV,
C           YRSIM,YRPLT,SOILNX,DSOILN,SOILNC,ECONAM,RUN,MODEL,
C           EXPER,CROP,CROPD,DSOIL,THETAC,TITLET
C
C  LOCAL  :
C
C  OUTPUT :
C-----------------------------------------------------------------------
C  Called : OPDAY
C
C  Calls  : YR_DOY NAILUJ WTHSUM
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  HDLAY  :
C=======================================================================

      SUBROUTINE OPHEAD (LUNOV,CUMDEP,TPESW,VRNAME,AINO3,AINH4,
     &     ECONO,RUN,MODEL,TITLET,WTHSTR)

      USE ModuleDefs
      IMPLICIT NONE

      INCLUDE 'COMSWI.BLK'
      INCLUDE 'COMSOI.BLK'
      INCLUDE 'COMIBS.BLK'

      CHARACTER*3   RMP,RMS
      CHARACTER*6   ECONO
      CHARACTER*12  MODEL
      CHARACTER*16  VRNAME
      CHARACTER*25  TITLET
      CHARACTER*120 WTHSTR

      INTEGER       IDYP,IDYS,IPYRP,IPYRS,NNFERT
      INTEGER       NNAPW,ISIM,IPLT,LUNOV,RUN

      REAL          AINH4,AINO3,CUMDEP,TPESW

	INTEGER       DATE_TIME(8)
      CHARACTER*3   MON(12)

      DATA MON /'Jan','Feb','Mar','Apr','May','Jun','Jul'
     &         ,'Aug','Sep','Oct','Nov','Dec'/

	CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      date_time(1)  The 4-digit year  
!      date_time(2)  The month of the year  
!      date_time(3)  The day of the month  
!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  
      
!     Version information stored in ModuleDefs.for
	WRITE (LUNOV,100) Version, Mon(DATE_TIME(2)), DATE_TIME(3), 
     &    DATE_TIME(1), DATE_TIME(5), DATE_TIME(6), DATE_TIME(7)
!chp      WRITE (LUNOV,200) RUN,TITLER
      WRITE (LUNOV,200) MOD(RUN,1000),TITLER
      WRITE (LUNOV,300) MODEL(1:8),CROPD,EXPER,CG,ENAME(1:50),
!chp     &                  TRTNO,TITLET,CROPD,VRNAME,ECONO
     &            MOD(TRTNO,1000),TITLET,CROPD,VRNAME,ECONO

      CALL YR_DOY (YRSIM,IPYRS,ISIM)
      CALL NAILUJ (ISIM,IPYRS,RMS,IDYS)
      WRITE (LUNOV,400) RMS,IDYS,IPYRS
      CALL YR_DOY (YRPLT,IPYRP,IPLT)

      IF (IPLT .LE. 366 .AND. IPLTI .EQ. 'R' ) THEN
         CALL NAILUJ (IPLT,IPYRP,RMP,IDYP)
	   WRITE (LUNOV,450) RMP,IDYP,IPYRP,PLTPOP,ROWSPC
       ELSE
         WRITE (LUNOV,475) PLTPOP,ROWSPC
      ENDIF

      WRITE (LUNOV,500) WSTA, YEAR
      WRITE (LUNOV,600) PEDON,SLTX,SLDESC

      IF (ISWWAT .NE. 'N') THEN
!        CHP 08/12/2005 Don't report initial conditions for
!             sequenced runs.
         IF (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1) THEN
           WRITE (LUNOV,625) NINT(CUMDEP),TPESW*10,AINO3,AINH4
         ENDIF

         IF (IIRRI .EQ. 'R' .OR. IIRRI .EQ. 'D') THEN
            IF (IIRRI .EQ. 'R') THEN
               WRITE (LUNOV,650)
             ELSE IF (IIRRI .EQ. 'D') THEN
               WRITE (LUNOV,655)
            ENDIF
            IF (TOTAPW .EQ. 0 .AND. NAPW .GE. 1) THEN
C-GH           NNAPW = 0
               NNAPW = NAPW
             ELSE
               NNAPW = NAPW
            ENDIF
            WRITE (LUNOV,660) NINT(TOTAPW),NNAPW
          ELSE IF (IIRRI .EQ. 'A') THEN
            WRITE (LUNOV,670)
            WRITE (LUNOV,680) DSOIL,THETAC
          ELSE IF (IIRRI .EQ. 'F') THEN
            WRITE (LUNOV,685)
            WRITE (LUNOV,686) DSOIL,THETAC
          ELSE IF (IIRRI .EQ. 'N') THEN
            WRITE (LUNOV,690)
            WRITE (LUNOV,700)
         ENDIF
       ELSE IF (ISWWAT .EQ. 'N') THEN
         WRITE (LUNOV,710)
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
         IF (ISWSYM .EQ. 'Y') THEN
            WRITE (LUNOV,720)
          ELSE IF (ISWSYM .EQ. 'U') THEN
            WRITE (LUNOV,730)
          ELSE IF (ISWSYM .EQ. 'N') THEN
            WRITE (LUNOV,740)
         ENDIF
       ELSE
         WRITE (LUNOV,750)
      ENDIF

      IF (ISWNIT .EQ. 'Y') THEN
         IF (IFERI .EQ. 'R' .OR. IFERI .EQ. 'D') THEN
            IF (TOTNAP .EQ. 0 .AND. NFERT .GE. 1) THEN
               NNFERT = 0
             ELSE
               NNFERT = NFERT
            ENDIF
            WRITE (LUNOV,800) NINT(TOTNAP),NNFERT
          ELSE IF (IFERI .EQ. 'A') THEN
            WRITE (LUNOV,810) SOILNX,DSOILN,SOILNC
          ELSE IF (IFERI .EQ. 'N') THEN
            WRITE (LUNOV,820)
         ENDIF
         WRITE (LUNOV,1000) NINT(ICRES),NINT(RESAMT),NARES
      ENDIF

      WRITE (LUNOV,1200) WTHSTR(1:60),WTHSTR(61:120)
      WRITE (LUNOV,1300) ISWWAT,ISWNIT,ISWSYM,ISWPHO,ISWDIS
      WRITE (LUNOV,1350) MEPHO,MEEVP,MEINF,MEHYD,MESOM
      WRITE (LUNOV,1400) IPLTI,IIRRI,IFERI,IRESI,IHARI,MEWTH

C-----------------------------------------------------------------------
C     Format Strings
C-----------------------------------------------------------------------

  100 FORMAT ("*DSSAT Cropping System Model Ver. ",I1,".",I1,".",I1,
     &     ".",I3.3,15X,A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2)
  200 FORMAT (/,'*RUN ',I3,8X,': ',A25)
  300 FORMAT (1X,'MODEL',10X,':',1X,A8,' - ',A10,/,
     &        1X,'EXPERIMENT',5X,':',1X,A8,1X,A2,1X,A50,/,
     &        1X,'TREATMENT',I3, 3X,':',1X,A25,//,
     &        1X,'CROP',11X,':',1X,A10,5X,'CULTIVAR :',A17,1X,
     &        'ECOTYPE :',A6)
  400 FORMAT (1X,'STARTING DATE  :',1X,A3,1X,I2,1X,I4)
  450 FORMAT (1X,'PLANTING DATE  :',1X,A3,1X,I2,1X,I4,4X,
     &       'PLANTS/m2 :',F5.1,5X,'ROW SPACING :',F5.0,'cm ')
  475 FORMAT (1X,'PLANTING DATE  :',1X,'AUTOMATIC PLANTING',1X,
     &       'PLANTS/m2 :',F5.1,5X,'ROW SPACING :',F5.0,'cm ')
  500 FORMAT (1X,'WEATHER',8X,':',1X,A4,3X,I4)
  600 FORMAT (1X,'SOIL',11X,':',1X,A10,5X,'TEXTURE : ',A5,' - ',A25)
  625 FORMAT (1X,'SOIL INITIAL C ',':',1X,'DEPTH:',I3,'cm',1X,
     &     'EXTR. H2O:',F5.1,'mm  NO3:',F5.1,'kg/ha  NH4:',F5.1,'kg/ha')
  650 FORMAT (1X,'WATER BALANCE',2X,':',1X,'IRRIGATE ON',
     &           ' REPORTED DATE(S)')
  655 FORMAT (1X,'WATER BALANCE',2X,':',1X,'IRRIGATE ON REPORTED',
     &           ' DAP')
  660 FORMAT (1X,'IRRIGATION',5X,':',1X,I8,' mm IN ',I5,' APPLICATIONS')
  670 FORMAT (1X,'WATER BALANCE',2X,':',1X,
     &           'AUTOMATIC IRRIGATION - REFILL PROFILE')
  680 FORMAT (1X,'IRRIGATION',5X,': AUTOMATIC - PLANTING -> MATURITY ',
     &           '[ SOIL DEPTH:',F5.2,'m',1X,F3.0,'%]')
  685 FORMAT (1X,'WATER BALANCE',2X,':',1X,
     &           'AUTOMATIC IRRIGATION - FIXED AMOUNT')
  686 FORMAT (1X,'IRRIGATION',5X,': AUTOMATIC - PLANTING -> MATURITY ',
     &           '[ SOIL DEPTH:',F5.2,'m',1X,F3.0,'%]')
  690 FORMAT (1X,'WATER BALANCE',2X,':',1X,'RAINFED')
  700 FORMAT (1X,'IRRIGATION',5X,':',1X,'NOT IRRIGATED')
  710 FORMAT (1X,'WATER BALANCE',2X,':',1X,'NOT SIMULATED ;',
     &           ' NO H2O-STRESS',/,1X,'IRRIGATION',5X,':')
  720 FORMAT (1X,'NITROGEN BAL.',2X,':',1X,
     &           'SOIL-N, N-UPTAKE & DYNAMIC N-FIXATION SIMULATION')
  730 FORMAT (1X,'NITROGEN BAL.',2X,':',1X,
     &           'SOIL-N, N-UPTAKE & UNLIMITED N-FIXATION SIMULATION')
  740 FORMAT (1X,'NITROGEN BAL.',2X,':',1X,
     &           'SOIL-N & N-UPTAKE SIMULATION; NO N-FIXATION')
  750 FORMAT (1X,'NITROGEN BAL.',2X,':',1X,
     &           'NOT SIMULATED ; NO N-STRESS',/,1X,
     &           'N-FERTILIZER',3X,':',/,1X,'RESIDUE/MANURE',1X,':')
  800 FORMAT (1X,'N-FERTILIZER',3X,':',1X,I8,' kg/ha IN ',I5,
     &           ' APPLICATIONS')
  810 FORMAT (1X,'N-FERTILIZER',3X,':',1X,'AUTO APPLICATIONS ',F5.0,
     &           ' kg/ha AT ',F6.2,' cm AND',F5.2,' % STRESS')
  820 FORMAT (1X,'N-FERTILIZER',3X,':',1X,'NO N-FERTILIZER APPLIED')
 1000 FORMAT (1X,'RESIDUE/MANURE',1X,':',1X,'INITIAL : ',I5,' kg/ha ;',
     &        I8,' kg/ha IN ',I5,
     &           ' APPLICATIONS')
 1200 FORMAT (1X,'ENVIRONM. OPT. :',1X,A60,/,18X,A60)
 1300 FORMAT (1X,'SIMULATION OPT : WATER',3X,':',A1,2X,'NITROGEN:',A1,
     & 2X,'N-FIX:',A1,2X,'PHOSPH :',A1,2X,'PESTS  :',A1)
 1350 FORMAT (18X,'PHOTO',3X,':',A1,2X,'ET      :',A1,
     & 2X,'INFIL:',A1,2X,'HYDROL :',A1,2X,'SOM    :',A1)
 1400 FORMAT (1X,'MANAGEMENT OPT : PLANTING:',A1,2X,'IRRIG',3X,':',A1,
     & 2X,'FERT :',A1,2X,'RESIDUE:',A1,2X,'HARVEST:',A1,2X,'WTH:',A1,/)

      RETURN
      END SUBROUTINE OPHEAD
