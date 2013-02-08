C=======================================================================
C  IPSOIL, Subroutine
C
C  Read soil parameters from SOILN040.SOL
C-----------------------------------------------------------------------
C  Revision history
C  06/15/1994 PWW Written
C  02/07/1993 PWW Header revision and minor changes
C  02/07/1993 PWW Added switch common block, restructured
C  06/09/1999 CHP Modular format
C  03/16/2000 GH  Incorporated in CROPGRO
C                 Note, file name should be dynamically created based on
C                 model name
C  03/26/2003 GH  Modified file name and location (SOL directory)
C  08/12/2003 CHP Added I/O error checking
C  08/22/2003 CHP Changed to read file once only and save arrays of data.
C-----------------------------------------------------------------------
C  Called : CROPGRO
C=======================================================================

      SUBROUTINE IPSOIL (CONTROL, RESTYPE,                !Input
     &    DMINR, DSNC, PRCEL, PRCHO, PRLIG,               !Output
     &    RCN, RDCEL, RDCHO, RDLIG)                       !Output

C-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT     NONE
      SAVE

      CHARACTER*1,  PARAMETER :: BLANK = ' '
      CHARACTER*6,  PARAMETER :: ERRKEY = 'IPSOIL'
      CHARACTER*5   RESTYPE
      CHARACTER*10  ACRO(12)
      CHARACTER*12  FILESS
      CHARACTER*30  FILEIO
      CHARACTER*72  PATHSOL
      CHARACTER*78  MSG(3)
      CHARACTER*84  SOILNF
      CHARACTER*180 CHAR

      INTEGER ISECT, J, LUNSOL, ERR, LNUM, LUNIO, LENGTH
      INTEGER, PARAMETER :: NR=9    !Maximum # of residue types

      REAL DMINR, RCN, DSNC, RDCHO, RDCEL, RDLIG, PRCHO, PRCEL, PRLIG
      REAL ADMINR, ARCN, ADSNC                        !Saved values
      REAL, DIMENSION(NR) :: ARDCHO, ARDCEL, ARDLIG   !Saved values
      REAL, DIMENSION(NR) :: APRCHO, APRCEL, APRLIG   !Saved values
      CHARACTER*5, DIMENSION(NR) :: ARESTYPE          !Saved values

      TYPE (ControlType) CONTROL

      LOGICAL FIRST, FEXIST, EROR

      DATA FIRST /.TRUE./
      DATA ACRO/'DMINR     ','RCN      ','DSNC      ','RE001     ',
     &          'RE002     ','RE003    ','RE004     ','RE005     ',
     &          'RE006     ','RE007    ','RE008     ','RE009     '/

C-----------------------------------------------------------------------
!     On first call to routine, read file and save values in arrays for
!       subsequent retrieval.
      IF (FIRST) THEN
        FIRST = .FALSE.
        EROR = .FALSE.

!       Assign default values
        ADMINR = 8.3E-05
        ARCN   = 40.0
        ADSNC  = 20.0
        ARDCHO = 0.2
        ARDCEL = 0.05
        ARDLIG = 0.0095
        APRCHO = 0.20  !PRCHO, PRCEL, and PRLIG are proportions of
        APRCEL = 0.70  !carbohydrate, cellulose and lignin in residue
        APRLIG = 0.10  !and must sum to 1.0.
        DO J = 1, 9
          ARESTYPE(J) = ACRO(J+3)(1:5)
        ENDDO

        FILEIO  = CONTROL % FILEIO
        LUNIO   = CONTROL % LUNIO
        FILESS = 'SOILN040.SOL'
C-----------------------------------------------------------------------
C       Open FILEIO to get path to soils directory
C-----------------------------------------------------------------------
        OPEN (UNIT = LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERR)
        IF (ERR .NE. 0) CALL ERROR (ERRKEY, ERR, FILEIO, 0)
        REWIND (LUNIO)
        READ(LUNIO, '(10(/),28X,A72)', IOSTAT=ERR) PATHSOL; LNUM = 11
        IF (ERR .NE. 0) CALL ERROR (ERRKEY, ERR, FILEIO, LNUM)
        CLOSE(LUNIO)

        LENGTH  = INDEX (PATHSOL,BLANK)
        IF (LENGTH .EQ. 0) THEN
          SOILNF = FILESS
        ELSE
          SOILNF = TRIM(PATHSOL) // TRIM(FILESS)
        ENDIF

        INQUIRE (FILE=SOILNF, EXIST=FEXIST)
        IF (FEXIST) THEN
C-----------------------------------------------------------------------
C       Open the SOILN040.SOL file 
C-----------------------------------------------------------------------
          CALL GETLUN('FILESS', LUNSOL)
          OPEN (LUNSOL,FILE = SOILNF, STATUS = 'OLD', IOSTAT=ERR)
          IF (ERR .EQ. 0) THEN 
C-----------------------------------------------------------------------
C     Read Soil Parameters from SOILN040.SOL
C-----------------------------------------------------------------------
            DO WHILE (ERR.eq.0)
              CALL IGNORE(LUNSOL,LNUM,ISECT,CHAR)
              IF ( ISECT.EQ.0 ) THEN
                 GOTO 200
              ENDIF
C              print *, 'CHAR = ', CHAR
              DO J = 1, 12
                IF (CHAR(4:13) .EQ. ACRO(J)) THEN
                  IF (J .EQ. 1) THEN
                    READ (CHAR(15:26),'(E12.0)',IOSTAT=ERR) ADMINR
                    IF (ERR .NE. 0) ADMINR = 8.3E-05
                    EXIT
                  ELSEIF (J .EQ. 2) THEN
                    READ (CHAR(15:24),'(F9.0)',IOSTAT=ERR) ARCN
                    IF (ERR .NE. 0) ARCN = 40.0
                    EXIT
                  ELSEIF (J .EQ. 3) THEN
                    READ (CHAR(15:24),'(F9.0)',IOSTAT=ERR) ADSNC
                    IF (ERR .NE. 0) ADSNC = 20.0
                    EXIT
                  ELSEIF (J .GE. 4) THEN
                    READ (CHAR(4:62),'(A5,6X,6F8.4)',IOSTAT=ERR)
     &                ARESTYPE(J-3),ARDCHO(J-3),ARDCEL(J-3),ARDLIG(J-3),
     &                              APRCHO(J-3),APRCEL(J-3),APRLIG(J-3)
                    IF (ERR .NE. 0) THEN
                      ARESTYPE(J-3) = ACRO(J)(1:5)
                      ARDCHO(J-3) = 0.2
                      ARDCEL(J-3) = 0.05
                      ARDLIG(J-3) = 0.0095
                      APRCHO(J-3) = 0.20  
                      APRCEL(J-3) = 0.70  
                      APRLIG(J-3) = 0.10  
                    ENDIF
                    EXIT
                  ENDIF
                ENDIF
              ENDDO
              IF (ERR .NE. 0) EROR = .TRUE.
            ENDDO
          ELSE
            !File could not be opened
            EROR = .TRUE.
            ERR = 20
          ENDIF
        ELSE
          !File does not exist
          EROR = .TRUE.
          ERR = 40
        ENDIF

  200   CONTINUE
        CLOSE (LUNSOL)

C-----------------------------------------------------------------------
!       Re-assign default values to arrays if problems with reading file
        IF (EROR) THEN
          SELECT CASE(ERR)
            CASE (20)
              MSG(1) = 'Error opening file: ' 
            CASE (40)
              MSG(1) = 'File does not exist: '
            CASE DEFAULT
              MSG(1) = 'Error reading file: '
          END SELECT
          MSG(2) = SOILNF
          MSG(3) = 'Default residue characteristics will be used.'
          CALL WARNING(3,ERRKEY,MSG)
        ENDIF     
      ENDIF       !End of FIRST block

C-----------------------------------------------------------------------
!     Assign values for current RESTYPE
      DMINR = ADMINR
      RCN   = ARCN  
      DSNC  = ADSNC 

      DO J = 1, NR
        IF (RESTYPE .EQ. ARESTYPE(J)) THEN
          RDCHO = ARDCHO(J)
          RDCEL = ARDCEL(J)
          RDLIG = ARDLIG(J)
          PRCHO = APRCHO(J)
          PRCEL = APRCEL(J)
          PRLIG = APRLIG(J)
          EXIT
        ENDIF
      ENDDO

C-----------------------------------------------------------------------
!     RESTYPE not found - assign default values and give warning
      IF (J .GT. NR) THEN
        DMINR = 8.3E-05
        RCN   = 40.0
        DSNC  = 20.0
        RDCHO = 0.2
        RDCEL = 0.05
        RDLIG = 0.0095
        PRCHO = 0.20    
        PRCEL = 0.70    
        PRLIG = 0.10    

        WRITE(MSG(1),1210) RESTYPE
 1210   FORMAT('User specified residue type: ',A5,' not valid.')
        MSG(2) = 'Default residue characteristics will be used.'
        CALL WARNING(2,ERRKEY,MSG)
      ENDIF

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE IPSOIL

C=======================================================================

! IPSOIL and SCREATE Variables
!
! ACRO(I) List of variables names used in SOILN980.SOL file which contains 
!           coefficients needed for simulating the decomposition of soil 
!           organic matter and organic matter 
! CHAR    Contains the contents of last record read 
! DMINR   Maximum decomposition rate constant of stable organic matter
!           (d-1)
! DSNC    Depth to which C and N are integrated across all soil layers for 
!           output in CARBON.OUT (cm)
! ERR     Error code for file operation 
! FILESS  Path plus filename for species file (*.spe) 
! I       Loop counter 
! J       Loop counter 
! LUNSOL  Logical unit number for SOILN980.SOL 
! PRCEL   Cellulose fraction of the residue (fraction)
! PRCHO   Carbohydrate fraction of the residue (fraction)
! PRLIG   Lignin fraction of the residue (fraction)
! RCN     C/N ratio of initial root residue (kg [C] / kg [N])
! RDCEL   Maximum decomposition rate of cellulose (fraction / day)
! RDCHO   Maximum decomposition rate of carbohydrates (fraction / day)
! RDLIG   Maximum decomposition rate of lignin (fraction / day)
! RESTYPE Residue application method 
! T(I)    Text array with explanatory text of the SOILN980.SOL file 
C=======================================================================

