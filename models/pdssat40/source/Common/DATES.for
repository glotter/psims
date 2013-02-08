C=======================================================================
C  DATES, File, Nigel Pickering, G. Hoogenboom, P.W. Wilkens and B. Baer
C  General functions related to time and date calculations
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/13/1991 NBP Developed
C  01/30/1998 GH  Modified ENDYR function for leap year
C  01/30/1998 GH  Added NAILUJ routine
C  02/02/1998 GH  Integer in YR_DOY
C  07/01/2000 GH  Added INCDAT
C  06/09/2002 GH  Modified for Y2K
!  10/11/2005 CHP Fix problem in Y2K_DOYW, sequenced runs spanning Y2K 
C=======================================================================
C=======================================================================
C  DOYC, Integer function, N.B. Pickering, 09/13/91
C  Calculates "TRUE" julian day, assuming that all leap years are 
C  divisible by 4. This is incorrect for certain years.
C-----------------------------------------------------------------------
C  09/13/1991 NBP Devfeloped
C  11/24/2002 GH  Modified for Y2K
C-----------------------------------------------------------------------
C  Input : YR,DOY
C  Output: DOC
C  Local : NLEAP
C=======================================================================

      INTEGER FUNCTION DOYC(YR,DOY)

      IMPLICIT NONE
      INTEGER DOY,NLEAP,YR

      NLEAP = INT((YR-1)/4)
      DOYC = NLEAP*366 + (YR-NLEAP-1)*365 + DOY

      END FUNCTION DOYC

C=======================================================================
C  YR_DOY, Subroutine, N.B. Pickering, 09/13/91
C  Converts YRDOY to YR and DOY.
C-----------------------------------------------------------------------
C  Input : YRDOY
C  Output: YR,DOY
C=======================================================================

      SUBROUTINE YR_DOY(YRDOY,YR,DOY)

      IMPLICIT NONE

      INTEGER DOY,YR,YRDOY

      YR  = INT(YRDOY / 1000)
      DOY = YRDOY - YR * 1000

      END SUBROUTINE YR_DOY

C=======================================================================
C  Y2K_DOY, Subroutine, Gerrit Hoogenboom 6/7/2002
C  Converts YRDOY to Y2K
C-----------------------------------------------------------------------
C  Input : YRDOY
C  Output: YRDOY
C=======================================================================

      SUBROUTINE Y2K_DOY(YRDOY)

      IMPLICIT NONE

      INTEGER DOY,YR,YRDOY

      IF (YRDOY .LE. 99365) THEN
        YR  = INT(YRDOY / 1000)
        DOY = YRDOY - YR * 1000
        IF (YRDOY .GT. 0) THEN
          IF (YR .LE. 10) THEN
            YRDOY = (2000 + YR) * 1000 + DOY
          ELSE
            YRDOY = (1900 + YR) * 1000 + DOY
          ENDIF
        ENDIF
      ENDIF
        
      END SUBROUTINE Y2K_DOY

C=======================================================================
C  Y2K_DOYW, Subroutine, C. Porter, 02/05/2004
C  Converts YRDOYW to Y2K for weather files
C  If this is a sequenced run, then days are forced to be sequential
C    when going from year 2010 to 2011,
C    and from 1999 to 2000.
C-----------------------------------------------------------------------
C  Input : RNMODE, YRDOYWY, YRDOYW
C  Output: YRDOYW
C=======================================================================

      SUBROUTINE Y2K_DOYW(MULTI, RNMODE, YRDOYWY, YRDOYW)

      USE ModuleDefs
      IMPLICIT NONE

      CHARACTER*1 RNMODE
      INTEGER MULTI
      INTEGER DOY, YR, YRDOYW
      INTEGER DOYY, YRY, YRDOYWY, YRINC
      Type (ControlType) CONTROL

      DATA YRY /0/

      IF (YRDOYW .LE. 99365) THEN
        YR  = INT(YRDOYW / 1000)
        DOY = YRDOYW - YR * 1000

        IF (YRDOYW .GT. 0) THEN
!          IF (YR .LE. 10 .OR. YRY .GE. 2000) THEN
            YR = 2000 + YR
!          ELSE
!            YR = 1900 + YR
!          ENDIF
          YRDOYW = YR * 1000 + DOY
        ENDIF
      ENDIF

!CHP 10/11/2005 Fix problem with sequenced runs spanning Y2K
      IF (INDEX('QFNS',RNMODE) .GT. 0 .OR. MULTI .GT. 1) THEN
        CALL YR_DOY(YRDOYWY, YRY, DOYY)
        CALL GETPUT_CONTROL('GET', CONTROL)
        IF (CONTROL % RUN > 1) THEN
          YRINC = YR - YRY
!          IF (YRINC .EQ. 100 .OR. YRINC .EQ. 101) THEN
          IF (YRINC .GT. 1) THEN
            YR = YR - 100
            YRDOYW = YR * 1000 + DOY
          ENDIF
        ENDIF
      ENDIF 

      RETURN
      END SUBROUTINE Y2K_DOYW

C=======================================================================
C  YDOY, Integer Function, N.B. Pickering, 09/13/91
C  Converts YR and DOY to YRDOY.
C-----------------------------------------------------------------------
C  Input : YR,DOY
C  Output: YRDOY
C=======================================================================

      INTEGER FUNCTION YDOY(YR,DOY)

      IMPLICIT NONE
      INTEGER DOY,YR

      YDOY = YR * 1000 + DOY
      
      END FUNCTION YDOY

C=======================================================================
C  TIMDIF, Integer function, N.B. Pickering, 09/13/91
C  Calculates the time difference between two YRDOY dates (days).
C-----------------------------------------------------------------------
C  Input : YRDOY1,YRDOY2
C  Output: DAYDIF
C=======================================================================

      INTEGER FUNCTION TIMDIF(YRDOY1,YRDOY2)

      IMPLICIT NONE
      INTEGER DOYC,DOY1,DOY2,YR1,YR2,YRDOY1,YRDOY2

C     Simple time difference of two days in the same year attempted first.

      TIMDIF = YRDOY2 - YRDOY1

C     If time difference involves a year change, use DOC calculations.

      IF (TIMDIF .GT. 365 .OR. TIMDIF .LT. -365) THEN
        CALL YR_DOY(YRDOY1,YR1,DOY1)
        CALL YR_DOY(YRDOY2,YR2,DOY2)
        TIMDIF = DOYC(YR2,DOY2) - DOYC(YR1,DOY1)
      ENDIF

      END FUNCTION TIMDIF

C=======================================================================
C  MTHEND, Integer Function, N.B. Pickering, 06/05/92
C  Calculates day-of-year that is end of month.
C-----------------------------------------------------------------------
C  Input : MTH,YR
C  Output: MTHEND
C  Local : MEND,LEAPYR
C=======================================================================

      INTEGER FUNCTION MTHEND(YR,MTH)

      IMPLICIT NONE
      INTEGER MTH,MEND(12),YR
      LOGICAL LEAPYR
      DATA MEND/31,59,90,120,151,181,212,243,273,304,334,365/

      LEAPYR = ((MOD(YR,4) .EQ. 0 .AND. MOD(YR,100) .NE. 0) .OR.
     &         (MOD(YR,400) .EQ. 0))
      IF (LEAPYR .AND. MTH.GE.2) THEN
        MTHEND = MEND(MTH) + 1
      ELSE
        MTHEND = MEND(MTH)
      ENDIF

      END FUNCTION MTHEND

C=======================================================================
C  MTHMID, Integer Function, N.B. Pickering, 06/05/92
C  Calculates day-of-year that is midpoint of month.
C-----------------------------------------------------------------------
C  Input : MTH,YR
C  Output: MTHMID
C  Local : MAXDOY,MBEG,MEND,LEAPYR,MTHBEG,MTHEND
C=======================================================================

      INTEGER FUNCTION MTHMID(YR,MTH)

      IMPLICIT NONE
      INTEGER MTH,YR
      LOGICAL LEAPYR
      INTEGER MIDPT(12)
      DATA MIDPT/16,46,75,106,136,167,197,228,259,289,320,350/

      LEAPYR = ((MOD(YR,4) .EQ. 0 .AND. MOD(YR,100) .NE. 0) .OR.
     &         (MOD(YR,400) .EQ. 0))
      IF (LEAPYR .AND. MTH.GE.2) THEN
        MTHMID = MIDPT(MTH) + 1
      ELSE
        MTHMID = MIDPT(MTH)
      ENDIF

      END FUNCTION MTHMID

C=======================================================================
C  INCYD, Integer Function, N.B. Pickering, 06/05/92
C  Increases/decreases YRDOY based on INC (ABS(INC)<=365).
C-----------------------------------------------------------------------
C  Input : YRDOY
C  Output: INCYD
C  Local : YDEND,YR,DOY
C=======================================================================

      INTEGER FUNCTION INCYD(YRDOY,INC)

      IMPLICIT NONE
      INTEGER ENDYR,INC,NDYR,YRDOY,YR,DOY,YDOY

      CALL YR_DOY(YRDOY,YR,DOY)
      NDYR = ENDYR(YR)
      DOY = DOY + INC
      IF (DOY .GT. NDYR) THEN
        YR = YR + 1
        DOY = DOY - NDYR
      ELSE IF (DOY .LE. 0) THEN
        YR = YR - 1
        NDYR = ENDYR(YR)
        DOY = NDYR + DOY
      ENDIF
      INCYD = YDOY(YR,DOY)
      
      END FUNCTION INCYD

C=======================================================================
C  INCDAT, Integer Function,J.Hansen
C  Similar to INCYD without the restriction that DELTA <= 365.
C-----------------------------------------------------------------------
C  Input : YRDOY(ADATE)
C  Output: INCDAT
C  Local : NDYR,AYR,ADOY,DELTA,ENDYR,YDOY
C-----------------------------------------------------------------------

      INTEGER FUNCTION INCDAT(ADATE, DELTA)

      IMPLICIT NONE
      INTEGER NDYR, AYR, ADOY, ADATE, DELTA, ENDYR, YDOY
      EXTERNAL ENDYR, YDOY

      CALL YR_DOY(ADATE, AYR, ADOY)
      NDYR = ENDYR(AYR)
      ADOY = ADOY + DELTA
  100 CONTINUE
      IF (ADOY .GT. NDYR) THEN
        AYR = AYR + 1
        ADOY = ADOY - NDYR
        GO TO 100
      END IF
  200 IF (ADOY .LE. 0) THEN
        AYR = AYR - 1
        NDYR = ENDYR(AYR)
        ADOY = ADOY + NDYR
        GO TO 200
      END IF
      INCDAT = YDOY(AYR, ADOY)

      RETURN
      END FUNCTION INCDAT


C=======================================================================
C  INCMTH, Integer Function, N.B. Pickering, 06/05/92
C  Increases/decreases month and adjusts YR based on INC (ABS(INC)<=12).
C-----------------------------------------------------------------------
C  Input : YR,MTH,SIGN
C  Output: INCMTH
C  Local :
C=======================================================================

      SUBROUTINE INCMTH(YR,MTH,INC)

      INTEGER YR,MTH,INC

      MTH = MTH + INC
      IF (MTH .GT. 12) THEN
        YR = YR + 1
        MTH = MTH - 12
      ELSE IF (MTH .LT. 1) THEN
        YR = YR - 1
        MTH = MTH + 12
      ENDIF

      END SUBROUTINE INCMTH

C=======================================================================
C  YDEND, Integer Function, N.B. Pickering, 06/05/92
C  Computes end-of-year in YRDOY format.  Input can be YRDOY or YR.
C-----------------------------------------------------------------------
C  Input : YRDOY or YR
C  Output: YDEND
C  Local : DOY,NDYR,YR
C=======================================================================

      INTEGER FUNCTION YDEND(YRDOY)

      IMPLICIT NONE
      INTEGER ENDYR,YRDOY,YR,YDOY

      IF (YRDOY/1000 .NE. 0) THEN
        YR = YRDOY / 1000
      ELSE
        YR = YRDOY
      ENDIF
      YDEND = YDOY(YR,ENDYR(YR))

      END FUNCTION YDEND

C=======================================================================
C  ENDYR, Integer Function, N.B. Pickering, 06/05/92
C  Computes end-of-year (365 or 366) depending if leap year.
C-----------------------------------------------------------------------
C  Input : YR
C  Output: ENDYR
C  Local :
C=======================================================================

      INTEGER FUNCTION ENDYR(YR)

      INTEGER YR
      IF ((MOD(YR,4) .EQ. 0 .AND. MOD(YR,100) .NE. 0) .OR.
     &  (MOD(YR,400) .EQ. 0))  THEN
        ENDYR = 366
      ELSE
        ENDYR = 365
      ENDIF

      END FUNCTION ENDYR

C=======================================================================
C  JULIAN, Function
C
C  Determines day, month
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : NDAY,RMON,NYRCHK
C
C  LOCAL  : RNAME,UPCASE,I,JCOUNT,MON
C
C  OUTPUT : JULIAN
C-----------------------------------------------------------------------
C  Called : SEHARV SEPLT SETIME
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  RMON   :
C  RNAME():
C  UPCASE :
C  I      :
C  JCOUNT :
C  NDAY   :
C  NYRCHK :
C  MON()  :
C=======================================================================

      INTEGER FUNCTION JULIAN (NDAY,RMON,YR)

      IMPLICIT    NONE

      CHARACTER*3 RMON,RNAME(12)
      CHARACTER*1 UPCASE

      INTEGER     I,JCOUNT,NDAY,YR,MON(12)

      DATA MON   /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA RNAME /'JAN','FEB','MAR','APR','MAY','JUN',
     &            'JUL','AUG','SEP','OCT','NOV','DEC'/

      IF ((MOD(YR,4) .EQ. 0 .AND. MOD(YR,100) .NE. 0) .OR.
     &  (MOD(YR,400) .EQ. 0))  THEN
         MON(2) = 29
       ELSE
         MON(2) = 28
      END IF
      DO I = 1, 3
         RMON(I:I) = UPCASE(RMON(I:I))
      END DO
      DO JCOUNT = 1, 12
         IF (RMON .EQ. RNAME(JCOUNT)) GO TO 200
      END DO

C-----------------------------------------------------------------------
C     Month name cannot be recognized
C-----------------------------------------------------------------------

      JULIAN = 370
      RETURN
  200 JULIAN = 0

      DO 400 JCOUNT = 1, 12
           IF (RMON .EQ. RNAME(JCOUNT))
     &          GO TO 300
           JULIAN = JULIAN + MON(JCOUNT)
           GO TO 400
  300      JULIAN = JULIAN + NDAY
           RETURN
  400 CONTINUE

      END FUNCTION JULIAN


C=======================================================================
C  NAILUJ, Subroutine
C
C  Determines Julian date
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : JULD,NYRCHK
C
C  LOCAL  : RNAME(),NSUM,JCOUNT,NDIF,MON()
C
C  OUTPUT : NDAY RMON
C-----------------------------------------------------------------------
C  Called : SEHARV SENS SEPLT SETIME OPDAY OPHEAD
C
C  Calls  : None
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  RMON   :
C  RNAME():
C  NSUM   :
C  JCOUNT :
C  NDIF   :
C  JULD   :
C  NYRCHK :
C  NDAY   :
C  MON()  :
C=======================================================================

      SUBROUTINE NAILUJ (JULD,YR,RMON,NDAY)

      IMPLICIT    NONE

      CHARACTER*3 RMON,RNAME(12)
      INTEGER     NSUM,JCOUNT,NDIF,JULD,YR,NDAY,MON(12)

      DATA MON   /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA RNAME /'JAN','FEB','MAR','APR','MAY','JUN',
     &            'JUL','AUG','SEP','OCT','NOV','DEC'/

      IF ((MOD(YR,4) .EQ. 0 .AND. MOD(YR,100) .NE. 0) .OR.
     &  (MOD(YR,400) .EQ. 0))  THEN
         MON(2) = 29
       ELSE
         MON(2) = 28
      ENDIF
      NSUM = 0

      DO JCOUNT = 1, 12
         NDIF = JULD - NSUM
         IF (NDIF .LE. MON(JCOUNT)) THEN
            NDAY = NDIF
            RMON = RNAME(JCOUNT)
            RETURN
         ENDIF
         NSUM = NSUM + MON(JCOUNT)
      END DO

      RETURN

      END SUBROUTINE NAILUJ



C=======================================================================
C  FullYear, Subroutine
C
C  Converts 2 digit year to 4 digit year. On first call per simulation, 
C    assumes that years 10 and lower refer to 2010 and years 11 and higher
C    refer to 1911.  After the first call, years are converted based on
C    previous years (i.e., year 2011 follows year 2010.)
C    
C-----------------------------------------------------------------------
C  Revision history
C
C  02/07/2002 CHP Written
C  11/24/2002 GH  Modified Y2K to 10
C=======================================================================
      SUBROUTINE FullYear (YRDOY, YEAR, DOY)

      IMPLICIT    NONE

      INTEGER CENTURY, DOY
      INTEGER YEAR, YRDOY, YR
      LOGICAL FIRST
      DATA FIRST /.TRUE./

      IF (YRDOY .LE. 99365) THEN
        YR  = INT(YRDOY / 1000)
        DOY = YRDOY - YR * 1000

        IF (FIRST) THEN
          IF (YR .LE. 10) THEN
            CENTURY = 2000
          ELSE
            CENTURY = 1900
          ENDIF
          FIRST = .FALSE.
        ENDIF

!       This will work if the YRDOY after 99365 is 100001.
         YEAR = CENTURY + YR

      ENDIF 
      RETURN
      END SUBROUTINE FullYear
