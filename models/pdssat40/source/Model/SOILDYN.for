C=======================================================================
C  COPYRIGHT 1998-2005 The University of Georgia, Griffin, Georgia
C                      University of Florida, Gainesville, Florida
C                      Iowa State University, Ames, Iowa
C                      International Center for Soil Fertility and 
C                       Agricultural Development, Muscle Shoals, Alabama
C                      University of Guelph, Guelph, Ontario
C  ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  SOILDYN, Subroutine, C.H. Porter
C  Soil dynamics routine computes and distributes soil parameters.
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  10/29/2001 CHP Written
C  08/12/2003 CHP Added I/O error checking
!  10/29/2004 CHP Program to halt for missing WR data (per GH).
!  02/01/2005 CHP Added initializations and revised checks.
C-----------------------------------------------------------------------
C  Called : Main
C  Calls  : 
C=======================================================================

      SUBROUTINE SOILDYN(CONTROL, MESOM, SOILPROP)

C-----------------------------------------------------------------------
!     Note: some routines (nitrogen routines and others?) read and modify 
!     BD at beginning of routine.  Get rid of all of these.  Also check for
!     reading soil parameters from file.  All modules should now get soil
!     values passed from tillage.  CHP 10/3/01

      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE
      SAVE

      LOGICAL NOTEXTURE, NO_OC, PHFLAG
      CHARACTER*1 MESOM, RNMODE
      CHARACTER*5 SLTXS
      CHARACTER*6 ERRKEY, SECTION
      PARAMETER (ERRKEY = 'SOILDY')
      CHARACTER*10 SLNO
      CHARACTER*11 SLSOUR
      CHARACTER*30 FILEIO
      CHARACTER*50 SLDESC, TAXON
      CHARACTER*78 MSG(10)
      CHARACTER*200 CHAR

      INTEGER DYNAMIC, ERRNUM, FOUND, L, LNUM
      INTEGER Length, LUNIO, NLAYR, REPNO, RUN

      REAL CN, DMOD, SLPF, SALB, SLDP, SWCON, TEMP, U

      REAL, DIMENSION(NL) :: ADCOEF, BD, CLAY, DLAYR, DS, DUL, LL
      REAL, DIMENSION(NL) :: OC, PH, SAND, SAT, SILT, SWCN, TOTN, WR

!     Not currently used:
      REAL, DIMENSION(NL) :: CEC, PHKCL, STONES

!     The variable "SOILPROP" is of constructed type "SoilType" as 
!     defined in ModuleDefs.for.  At the end of this routine, local
!     variables will be copied into the constructed variable, SOILPROP.
      TYPE (SoilType) SOILPROP

!     The variable "CONTROL" is of constructed type "ControlType" as 
!     defined in ModuleDefs.for, and contains the following variables.
!     The components are copied into local variables for use here.
      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      LUNIO   = CONTROL % LUNIO
      REPNO   = CONTROL % REPNO
      RNMODE  = CONTROL % RNMODE
      RUN     = CONTROL % RUN

!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT .AND.
!-----------------------------------------------------------------------
!     Skip initialization for sequenced runs:
     &   (INDEX('FQ',RNMODE) .LE. 0 .OR. RUN == 1)) THEN

!     Initialize soils variables
      SLTXS  = '     '
      SLSOUR = '           '
      SLDESC = '                                                 '
      TAXON  = '                                                 '

      SLDP   = -99.
      SALB   = -99.
      DMOD   = -99.
      SLPF   = -99.
      CLAY   = -99.
      SILT   = -99.
      SAND   = 0.
      STONES = -99.
      OC     = -99.
      PH     = -99.
      BD     = -99.
      LL     = -99.
      DUL    = -99.
      SAT    = -99.
      SWCN   = -99.
      PHKCL  = -99.
      CEC    = -99.

      U      = -99.
      SWCON  = -99.
      CN     = -99.
      WR     = -99.
      TOTN   = -99.
      ADCOEF = 0.

!-----------------------------------------------------------------------
!     Read FILEIO
      OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
      IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,0)

!-----------------------------------------------------------------------
C     Find and Read Soil Profile Section.
      SECTION = '*SOIL '
      CALL FIND(LUNIO, SECTION, LNUM, FOUND)
      IF (FOUND .EQ. 0) CALL ERROR(SECTION, 42, FILEIO, LNUM)

      READ(LUNIO, 60, IOSTAT=ERRNUM) SLNO, SLSOUR, SLTXS, SLDP, SLDESC
   60 FORMAT (1X,A10, 2X, A11,1X,A5,1X,F5.0,1X,A50)
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)
      READ(LUNIO, 70, IOSTAT=ERRNUM) TAXON ; LNUM = LNUM + 1
   70 FORMAT (41X,A50)
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)
      
      READ(LUNIO, 80, IOSTAT=ERRNUM) SALB, U, SWCON, CN, DMOD, SLPF
   80 FORMAT (7X,F5.2,1X,F5.1,1X,F5.2,1X,F5.0,2(1X,F5.2))
      LNUM = LNUM + 1
      IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)

        !First tier soils data:
      NLAYR = 0
      DO L = 1, NL
        READ(LUNIO,'(F6.0,6X,A200)',IOSTAT=ERRNUM) DS(L), CHAR
        IF (ERRNUM .NE. 0) EXIT
        IF (L .GT. 1) THEN
          IF (DS(L) .LT. DS(L-1)) EXIT
        ENDIF
        LNUM = LNUM + 1

        READ(CHAR,'(15F6.0)',IOSTAT=ERRNUM) 
     &      LL(L), DUL(L), SAT(L), WR(L), SWCN(L), BD(L),
     &      OC(L), CLAY(L), SILT(L), STONES(L), TOTN(L),
     &      PH(L), PHKCL(L), CEC(L), ADCOEF(L)

        IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LNUM)
        NLAYR = NLAYR + 1
      ENDDO

      CLOSE (LUNIO)

C-----------------------------------------------------------------------
!     Check validity of input values:

      IF (SALB < 1.E-4) THEN
        SALB = 0.15
        WRITE(MSG(1),400) SALB
  400   FORMAT('Soil albedo has been set to ',F5.1,'.')
        CALL WARNING(1, ERRKEY, MSG)
      ENDIF

      IF (SLPF < 1.E-4) THEN
        SLPF = 1.0
      ENDIF

C     Initialize curve number (according to J.T. Ritchie) 1-JUL-97 BDB
      IF (CN .LE. 25.0 .OR. CN .GT. 98.0) THEN
        TEMP = CN
        IF (CN .LE. 0.0) THEN
          CN = 80.
        ENDIF
        CN = AMIN1 (CN,98.0)
        CN = AMAX1 (CN,25.0)
        WRITE(MSG(1),500) TEMP, CN
  500   FORMAT('Surface runoff curve number has been changed from ',
     &    F5.1,' to ',F5.1,'.')
        CALL WARNING(1, ERRKEY, MSG)
      ENDIF

      IF (U < 1.E-4) THEN
        U = 6.0
        WRITE(MSG(1),505) U
  505   FORMAT('Stage 1 soil evaporation limit has been set to ',
     &     F5.1,'.')
        CALL WARNING(1, ERRKEY, MSG)
      ENDIF

      IF (SWCON < 1.E-4) THEN
        SWCON = 0.25
        WRITE(MSG(1),510) SWCON
  510   FORMAT('Soil water conductivity constant has been set to ',
     &    F5.2,' (fraction/day).')
        CALL WARNING(1, ERRKEY, MSG)
      ENDIF

      PHFLAG = .FALSE.

C-----------------------------------------------------------------------
      MSG(3) = ' '
      DO L = 1, NLAYR
!       Calculate layer thicknesses
        IF (L .EQ. 1) THEN
          DLAYR(1) = DS(1)
        ELSE
          DLAYR(L) = DS(L) - DS(L-1)
        ENDIF

!       Default of 1.0 probably not the best, but better than zero.
        !10/29/2004 CHP changed from .LE. to .LT.
        IF (WR(L) .LT. 0.0) THEN
          !WR(L) = 1.0
          !Missing_Data = .TRUE.
          !10/29/2004 CHP per GH
          !Do not allow missing data.
          CALL ERROR(ERRKEY,11,FILEIO,0)
        ENDIF

!       Set bulk density to an average value, if not given.
        IF (BD(L) .LT. 0.001) THEN
          BD(L) = 1.2             
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '  BD'
          ELSE
            IF (INDEX(MSG(3),'BD') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', BD'
            ENDIF
          ENDIF 
        ENDIF

!       Set the soil pH to an average value, if not given.
        IF (PH(L) .LE. 1.0 .OR. PH(L) .GT. 10.0) THEN
          PH(L) = 7.0
          IF (Length < 2) THEN
            MSG(3) = '  pH'
          ELSE
            IF (INDEX(MSG(3),'pH') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', pH'
            ENDIF
          ENDIF 
        ENDIF

        IF (CEC(L) .LE. 0.0) THEN
          CEC(L) = 15.5
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '  CEC'
          ELSE
            IF (INDEX(MSG(3),'CEC') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', CEC'
            ENDIF
          ENDIF
        ENDIF
         
        IF (STONES(L) .GT. 100.0 .OR. STONES(L) .LT. 0.0) THEN
          STONES(L) = 0.0
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '  STONES'
          ELSE
            IF (INDEX(MSG(3),'STONES') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', STONES'
            ENDIF
          ENDIF
        ENDIF

        IF (ADCOEF(L) .LE. 0.0) THEN
          ADCOEF(L) = 0.0
          Length = LEN(TRIM(MSG(3)))
          IF (Length < 2) THEN
            MSG(3) = '  ADCOEF'
          ELSE
            IF (INDEX(MSG(3),'ADCOEF') < 1) THEN
              MSG(3) = TRIM(MSG(3)) // ', ADCOEF'
            ENDIF
          ENDIF
        ENDIF
      ENDDO   !End of soil layer loop.

!      IF (INDEX('QFN',RNMODE) .LE. 0 .OR. 
!     &            (RUN .EQ. 1 .AND. REPNO .EQ. 1)) THEN
!       chp 2/8/06 This message is confusing to users.
!        IF (LEN(TRIM(MSG(3))) > 1) THEN
!!         Print message for missing or invalid data
!          MSG(1)='The soil file has missing or invalid data. '
!          MSG(2)='Default values have been used for these variables:'
!          MSG(4)='These defaults may or may not be important to your' //
!     &      ' results.'
!          CALL WARNING(4, ERRKEY, MSG)
!        ENDIF
!  
!!      Warning that the pH was set to a different value.
!        IF (PHFLAG) THEN
!          WRITE (MSG(1), 600)
!          WRITE (MSG(2), 601)
!          WRITE (MSG(3), 602)
!  600     FORMAT('The pH of one or more soil layers at initialization')
!  601     FORMAT('was outside the range pH=1-10, which is unlikely.')
!  602     FORMAT('It was set by DSSAT to pH=7.') 
!          CALL WARNING (3, ERRKEY, MSG)
!        ENDIF   !End of IF block on PHFLAG.
!      ENDIF

!     Add additional checks for Century SOM model:
      IF (MESOM .EQ. 'P') THEN
        NOTEXTURE = .FALSE.
        NO_OC     = .FALSE.

!       Check for soil texture validity.
        DO L = 1, NLAYR
!         Soil texture check for invalid soil texture. Treat as if 
!         no texture has been provided.
          IF (CLAY(L) + SILT(L) + SAND(L) .GT. 100.001) THEN
            CLAY(L) = -99.
            SILT(L) = -99.
          ENDIF

!         If no texture data is provided, set to average values.
          IF (CLAY(L) .LE. -0.001 .OR. CLAY(L) .GT. 100.001) THEN
            CLAY(L) = 30.

!           If silt is given, but clay not, the default clay value may
!           sum to >100%.
            IF (CLAY(L) + SILT(L) .GT. 100.) CLAY(L) = 100. - SILT(L)

            NOTEXTURE = .TRUE.
          ENDIF

          IF (SILT(L) .LE. -0.001 .OR. SILT(L) .GT. 100.001) THEN
            SILT(L) = 30.

!           If clay is given, but silt not, the default silt value may
!           sum to >100%.
            IF (CLAY(L) + SILT(L) .GT. 100.) SILT(L) = 100. - CLAY(L)
            NOTEXTURE = .TRUE.
          ENDIF

!         Clay and silt data are given, but sand not.
          SAND(L) = 100. - CLAY(L) - SILT(L)
          SAND(L) = AMAX1 (SAND(L), 0.)

          IF (OC(L) .LT. 1.E-3) THEN
            NO_OC = .TRUE.
          ENDIF

        ENDDO   !End of soil layer loop

        IF (INDEX('QFN',RNMODE) .LE. 0 .OR. 
     &            (RUN .EQ. 1 .AND. REPNO .EQ. 1)) THEN
!       Texture data missing - write message to WARNING.OUT file.
        IF (NOTEXTURE) THEN
            MSG(1)='The CENTURY-based soil-organic-matter module needs'
            MSG(2)='soil texture data, but the specified soil profile '
            MSG(3)='does not have those for some layers. The model will'
            MSG(4)='run with the default 30% clay, 30% silt, 40% sand. '
            MSG(5)='This may give incorrect results.'
        CALL WARNING(5, ERRKEY, MSG)
        ENDIF

!       Organic C data missing - write message to WARNING.OUT file 
!         and end program.
        IF (NO_OC) THEN
            MSG(1)='The CENTURY-based soil-organic-matter module needs'
            MSG(2)='soil organic carbon data, but the specified soil '
            MSG(3)='does not have that data for some layers.'
        MSG(4)='Model run will stop.'
        CALL WARNING(4, ERRKEY, MSG)
        CALL ERROR(ERRKEY,10,FILEIO,0)
        ENDIF
        ENDIF     !End of IF block on RNMODE
      ENDIF     !End of IF block on MESOM
!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************

!     Copy soil parameters to soil variable type, which is defined in
!     ModuleDefs.for.  
      SOILPROP % ADCOEF = ADCOEF 
      SOILPROP % BD     = BD     
      SOILPROP % CEC    = CEC    
      SOILPROP % CLAY   = CLAY   
      SOILPROP % CN     = CN     
      SOILPROP % DLAYR  = DLAYR  
      SOILPROP % DMOD   = DMOD        !formerly SLNF   
      SOILPROP % DS     = DS     
      SOILPROP % DUL    = DUL    
      SOILPROP % LL     = LL     
      SOILPROP % NLAYR  = NLAYR
      SOILPROP % OC     = OC     
      SOILPROP % PH     = PH     
      SOILPROP % PHKCL  = PHKCL  
      SOILPROP % SALB   = SALB   
      SOILPROP % SAND   = SAND   
      SOILPROP % SAT    = SAT    
      SOILPROP % SLPF   = SLPF  
      SOILPROP % SILT   = SILT   
      SOILPROP % STONES = STONES 
      SOILPROP % SWCN   = SWCN   
      SOILPROP % SWCON  = SWCON  
      SOILPROP % TOTN   = TOTN   
      SOILPROP % U      = U     
      SOILPROP % WR     = WR     

      RETURN
      END SUBROUTINE SOILDYN

!========================================================================
! SoilDyn variable definitiions - updated 08/20/2003
!========================================================================
! ADCOEF(L) Anion adsorption coefficient for soil layer L;  for reduced 
!             anion (nitrate) flow in variable-charge soils (ADCOEF = 0 
!             implies no anion retention) (cm3 (H2O] / g [soil])
! BD(L)     Bulk density, soil layer L (g [soil] / cm3 [soil])
! CEC(L)    Cation exchange capacity, soil layer L (cmol kg-1)
! CHAR      Contains the contents of last record read 
! CLAY(L)   Percentage of clay in soil layer L 
! CN        Runoff Curve Number - measure of runoff potential based on soil 
!             type and current soil water content. 
! CONTROL   Composite variable containing variables related to control 
!             and/or timing of simulation.  The structure of the variable 
!             (ControlType) is defined in ModuleDefs.for. 
! DLAYR(L)  Thickness of soil layer L (cm)
! DMOD      Factor to adjust the mineralization rate for certain atypical 
!             soils (range 0-1) 
! DS(L)     Cumulative depth in soil layer L (cm)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3[water]/cm3[soil])
! DYNAMIC   Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!             INTEGR, OUTPUT, or FINAL 
! ERRKEY    Subroutine name for error file 
! ERRNUM    Error number for input 
! FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
! FOUND     Indicator that good data was read from file by subroutine FIND 
!             (0 - End-of-file encountered, 1 - NAME was found) 
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!            (cm3 [water] / cm3 [soil])
! LNUM      Current line number of input file 
! LUNIO     Logical unit number for FILEIO 
! MESOM     Method for soil N computations ('G'=Godwin or Ceres-based, 
!             'P'=Parton or Century-based (future)) 
! MSG       Text array containing information to be written to WARNING.OUT 
!             file. 
! NL        Maximum number of soil layers = 20 
! NLAYR     Actual number of soil layers 
! NOTEXTURE Logical variable which indicates whether soil texture data is 
!             available 
! OC(L)     Organic carbon content of layer (%)
! PH(L)     pH in soil layer L 
! PHKCL(L)  pH in buffer, soil layer L 
! SALB      Bare soil albedo (fraction)
! SAT(L)    Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SECTION   Section name in input file 
! SILT(L)   Percentage of silt in soil layer L 
! SLDESC    Soil description or local classification 
! SLDP      Soil profile depth (cm)
! SLNO      Soil identifier (Institute + Site + Year + Soil) 
! SLPF      Soil photosynthesis factor, 0 to 1 scale 
! SLSOUR    Souce of soil information 
! SLTXS     Soil texture code 
! SOILPROP  Composite variable containing soil properties including bulk 
!             density, drained upper limit, lower limit, pH, saturation 
!             water content.  Structure defined in ModuleDefs. 
! STONES(L) Coarse fraction (>2 mm) (%)
! SWCN(L)   Saturated hydraulic conductivity in layer L (cm/hr)
! SWCON     Soil water conductivity constant; whole profile drainage rate 
!             coefficient (1/d)
! TAXON     Soil family, SCS system 
! TOTN(L)   Total N in soil (%)
! U         Evaporation limit (cm)
! WR(L)     Root hospitality factor, used to compute root distribution 
!========================================================================
!     End SOILDYN module
!========================================================================
