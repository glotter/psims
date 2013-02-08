C=======================================================================
C  PATH, Subroutine
C
C  Program to read the PATH from DSSATPRO.FLE
C-----------------------------------------------------------------------
C  Revision history
C
C  01/01/1991 GH  Written
C  05/28/1993 PWW Header revision and minor changes
C-----------------------------------------------------------------------
C  INPUT  : PROCOD,PFLAG
C
C  LOCAL  : BLANK ERRKEY FILEPR LINE LUNPR ERRNUM PATHL I K FEXIST
C
C  OUTPUT : PATHC,NAMEF
C-----------------------------------------------------------------------
C  Called : SECLI SEWTH IPEXP
C
C  Calls  : ERROR
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  BLANK  :
C  PROCOD :
C  ERRKEY :
C  FILEPR :
C  LINE   :
C  PATHC  :
C  LUNPR  :
C  ERRNUM :
C  PATHL  :
C  I,K    :
C  PFLAG  :
C  FEXIST :
C=======================================================================

      SUBROUTINE PATH (PROCOD,DSSATP,PATHC,PFLAG,NAMEF)

      IMPLICIT     NONE

      CHARACTER*1  BLANK
      CHARACTER*3  PROCOD
      CHARACTER*6  ERRKEY
      CHARACTER*12 NAMEF
      CHARACTER*102 DSSATP
      CHARACTER*80 LINE,PATHC

      INTEGER      LUNPR,ERRNUM,PATHL,I,K,PFLAG
      LOGICAL      FEXIST

      PARAMETER (LUNPR  = 15)
      PARAMETER (ERRKEY = 'PATH  ')
      PARAMETER (BLANK  = ' ')

      PATHC = BLANK

      INQUIRE (FILE = DSSATP,EXIST = FEXIST)
      IF (FEXIST) THEN
         OPEN (LUNPR,FILE=DSSATP,STATUS = 'OLD',IOSTAT=ERRNUM)
      ENDIF
      IF (.NOT. FEXIST .OR. ERRNUM .NE. 0) RETURN

      DO I = 1, 500
         READ (LUNPR,'(A80)',IOSTAT=ERRNUM) LINE
         IF (LINE(1:3) .EQ. PROCOD) THEN
C            PATHC  = LINE(5:6) // LINE(8:80)
            PATHC  = LINE(8:80)
            PATHL  = INDEX (PATHC,BLANK)
C            WRITE (PATHC(PATHL:PATHL),'(A1)') '\'
            WRITE (PATHC(PATHL:PATHL),'(A1)') '/'
            NAMEF  = PATHC (PATHL+1:PATHL+13)
            IF (PATHL .LT. 80) THEN
               DO K = (PATHL+1),80
                  IF (PATHC(K:K) .NE. BLANK) THEN
                     PATHC(K:K) = BLANK
                  ENDIF
               END DO
            ENDIF
            CLOSE (LUNPR)
            RETURN
         ENDIF
      END DO

      IF (PFLAG .EQ. 1) THEN
         WRITE(*,600) PROCOD
 600     FORMAT(' CODE ',A3,' not found in file DSSATPRO.FLE')
         CALL ERROR (ERRKEY,1,DSSATP,I)
      ENDIF

      CLOSE (LUNPR)
      END

C=======================================================================
C  PATHD, Subroutine
C
C  Program to read the PATH of DSSATPRO.FLE
C    Assume in current directory first.
C    If not in current directory, DSSATPRO.FLE is stored with the EXEs
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H         8-10-93
C  2  Modified by
C  3. Header revision and minor changes             P.W.W.      5-28-93
C-----------------------------------------------------------------------
C  INPUT  : INPUTX
C
C  LOCAL  : BLANK ERRKEY FILEPR LINE LUNPR ERRNUM PATHL I K FEXIST
C
C  OUTPUT : DSSATP
C-----------------------------------------------------------------------
C  Called : INPUT
C
C-----------------------------------------------------------------------
C                         DEFINITIONS
C
C  FEXIST :
C=======================================================================

      SUBROUTINE PATHD (DSSATP,INPUTX,IP)

      IMPLICIT     NONE

      CHARACTER*12 DSSATF
      CHARACTER*102 DSSATP    !,INPUTX
      CHARACTER*120 INPUTX

      INTEGER      I
      INTEGER*2    IP

      LOGICAL      FEXIST

      PARAMETER (DSSATF = 'DSSATPRO.FLE')

      IP = LNBLNK(INPUTX)
      DSSATP(1:12) = DSSATF
      INQUIRE (FILE = DSSATP,EXIST = FEXIST)
	IF (.NOT. FEXIST .AND. IP .GT. 12) THEN
         DO I = IP, 0, -1
            IF (INPUTX(I:I) .EQ. '/') GO TO 10
         END DO
   10    CONTINUE
         DSSATP(1:I+12) = INPUTX(1:I) // DSSATF
      ENDIF
      END
