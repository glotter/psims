!=======================================================================
!  OpStress, Subroutine
!
!  Tracks weather parameters as a function of critical growth stages.
!     CONTROL is a required variable
!     Other variables are optional.
!     ISWITCH, PlantStres are sent from OPHARV routines
!     WEATHER sent from weather module
!     ET can be sent from SPAM routine, but it is not currently printed.
!-----------------------------------------------------------------------
!  Revision history
!  10/20/2005 CHP Written based on CROPGRO v3.5 routines SUMWTH and OPSTRS
!-----------------------------------------------------------------------

      SUBROUTINE OPSTRESS(CONTROL, IDETO,   
     &    ET, PlantStres, WEATHER)

      USE ModuleDefs
      IMPLICIT  NONE
      SAVE

!-----------------------------------------------------------------------
      INTEGER, PARAMETER :: MaxStag = 5 !max # of stages output

!     Interface variables
      TYPE (ControlType), Intent(IN)           :: CONTROL
      CHARACTER*1,        Intent(IN), Optional :: IDETO
      REAL,               Intent(IN), Optional :: ET
      TYPE (PlStresType), Intent(IN), Optional :: PlantStres
      TYPE (WeatherType), Intent(IN), Optional :: WEATHER

!     Local variables
      CHARACTER*1 IDETO_SAVE, RNMODE
      CHARACTER*2 CROP
      CHARACTER*12, PARAMETER :: OUTO = 'OVERVIEW.OUT'
      CHARACTER*23 STAG(MaxStag)

      INTEGER DYNAMIC, I, NOUTDO, NYRS, STTOT
      INTEGER, DIMENSION(MaxStag) :: NNR

      REAL TMAX, TMIN, RAIN, DAYL, SRAD, TURFAC, SWFAC, NSTRES, AGEFAC
      REAL ET_L
      REAL, DIMENSION(MaxStag) :: TMAXR, TMINR, RAINR, DAYLR, RADR, CETR
      REAL, DIMENSION(MaxStag) :: TURFR, SWFCR, NSTRR, AGEFR  !, RADTR

      LOGICAL FIRST, FEXIST
      DATA FIRST /.TRUE./

!-----------------------------------------------------------------------
      IF (FIRST) THEN
        IDETO_SAVE = 'N'
        CALL GETLUN('OUTO', NOUTDO)

        FIRST = .FALSE.

      ENDIF

      DYNAMIC = CONTROL % DYNAMIC

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
      CROP   = CONTROL % CROP
      NYRS   = CONTROL % NYRS
      RNMODE = CONTROL % RNMODE

      IF (PRESENT(IDETO)) THEN
        IDETO_SAVE = IDETO
      ELSE
        IDETO_SAVE = 'N'
      ENDIF

      IF (PRESENT(PlantStres)) THEN
        STTOT = PlantStres % NSTAGES
        STAG  = PlantStres % StageName
      ELSE
        STTOT = 0
        STAG = '                       '
      ENDIF

!     Zero averages in arrays
      TMAXR = 0.0
      TMINR = 0.0
      RAINR = 0.0
      DAYLR = 0.0
      RADR  = 0.0
      !RADTR = 0.0
      CETR  = 0.0
      TURFR = 0.0
      SWFCR = 0.0
      NSTRR = 0.0
      AGEFR = 0.0
      NNR   = 0.0

      ET_L   = 0.0
      TURFAC = 1.0 
      SWFAC  = 1.0  
      NSTRES = 1.0 
      AGEFAC = 1.0

!***********************************************************************
!***********************************************************************
!     RATE
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     If weather data is sent, routine was called by weather module and
!     data need to be accumulated.
      IF (PRESENT(WEATHER)) THEN
        TMAX = WEATHER % TMAX
        TMIN = WEATHER % TMIN
        RAIN = WEATHER % RAIN
        DAYL = WEATHER % DAYL
        SRAD = WEATHER % SRAD 
      ENDIF

!***********************************************************************
!***********************************************************************
!     DAILY OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!-----------------------------------------------------------------------
      IF (PRESENT(ET)) THEN
        ET_L = ET
      ENDIF

      IF (PRESENT(PlantStres)) THEN
        TURFAC = PlantStres % TURFAC 
        SWFAC  = PlantStres % SWFAC  
        NSTRES = PlantStres % NSTRES 
        AGEFAC = PlantStres % AGEFAC 

        DO I = 1, PlantStres % NSTAGES
          IF (PlantStres % ACTIVE(I)) THEN
            NNR(I) = NNR(I) + 1
            TMAXR(I) = TMAXR(I) + TMAX
            TMINR(I) = TMINR(I) + TMIN
            RAINR(I) = RAINR(I) + RAIN
            DAYLR(I) = DAYLR(I) + DAYL
            RADR (I) = RADR (I) + SRAD 
            CETR(I)  = CETR(I)  + ET_L
            TURFR(I) = TURFR(I) + TURFAC 
            SWFCR(I) = SWFCR(I) + SWFAC  
            NSTRR(I) = NSTRR(I) + NSTRES 
            AGEFR(I) = AGEFR(I) + AGEFAC
          ENDIF
        ENDDO

      ENDIF
!***********************************************************************
!***********************************************************************
!     SEASONAL OUTPUT
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. FINAL) THEN
!-----------------------------------------------------------------------
      IF (IDETO_SAVE .EQ. 'Y' .AND. STTOT > 0) THEN
        INQUIRE (FILE = OUTO, EXIST = FEXIST)
        IF (.NOT. FEXIST) RETURN !ERROR?

        OPEN (UNIT = NOUTDO, FILE = OUTO, STATUS = 'OLD',
     &    POSITION = 'APPEND')
        WRITE(NOUTDO,500)
      ENDIF

      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
        WRITE (*,500)
      ENDIF

      DO I = 1, STTOT
        IF (NNR(I) > 0) THEN
          TMAXR(I) = TMAXR(I)/ NNR(I)
          TMINR(I) = TMINR(I)/ NNR(I)
          DAYLR(I) = DAYLR(I)/ NNR(I)
          !RADTR(I) = RADR(I)
          RADR (I) = RADR(I) / NNR(I)
          TURFR(I) = 1 - (TURFR(I) / NNR(I))
          SWFCR(I) = 1 - (SWFCR(I) / NNR(I))
          NSTRR(I) = 1 - (NSTRR(I) / NNR(I))
          AGEFR(I) = 1 - AMIN1(1.0, (AGEFR(I) / NNR(I)))
        ENDIF

        IF (IDETO_SAVE .EQ. 'Y') THEN
          IF (RAINR(I) < 1000.) THEN
            WRITE(NOUTDO,600) STAG(I), NNR(I), 
     &        TMAXR(I), TMINR(I), RADR(I), DAYLR(I), RAINR(I), CETR(I),
     &        SWFCR(I), TURFR(I), AGEFR(I), NSTRR(I)
  600       FORMAT(1X,A23,I5,3F6.1,F7.2,2F7.1,4F7.3)
          ELSE
            WRITE(NOUTDO,610) STAG(I), NNR(I), 
     &        TMAXR(I), TMINR(I), RADR(I), DAYLR(I), 
     &        NINT(RAINR(I)), NINT(CETR(I)),
     &        SWFCR(I), TURFR(I), AGEFR(I), NSTRR(I)
  610       FORMAT(1X,A23,I5,3F6.1,F7.2,2I7,4F7.3)
          ENDIF
        ENDIF

        IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
          IF (RAINR(I) < 1000.) THEN
            WRITE(*,600) STAG(I), NNR(I), 
     &        TMAXR(I), TMINR(I), RADR(I), DAYLR(I), RAINR(I), CETR(I),
     &        SWFCR(I), TURFR(I), AGEFR(I), NSTRR(I)
          ELSE
            WRITE(*,610) STAG(I), NNR(I), 
     &        TMAXR(I), TMINR(I), RADR(I), DAYLR(I), 
     &        NINT(RAINR(I)), NINT(CETR(I)),
     &        SWFCR(I), TURFR(I), AGEFR(I), NSTRR(I)
          ENDIF
        ENDIF
      ENDDO

!      IF (IDETO_SAVE .EQ. 'Y' .AND. STTOT > 0) THEN
!        WRITE (NOUTDO,1100)
! 1100   FORMAT (/,67X,'(0.0 = Minimum Stress',/,
!     &            67X,' 1.0 = Maximum Stress)')
!      ENDIF

!      IF (RNMODE .EQ. 'I' .AND. NYRS .LE. 1) THEN
!        WRITE (*,1100)
!      ENDIF

!     Reset arrays for next run.
      TMAXR = 0.0
      TMINR = 0.0
      RAINR = 0.0
      DAYLR = 0.0
      RADR  = 0.0
      !RADTR = 0.0
      CETR  = 0.0
      TURFR = 0.0
      SWFCR = 0.0
      NSTRR = 0.0
      AGEFR = 0.0
      NNR   = 0

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
 500  FORMAT(//,'*ENVIRONMENTAL AND STRESS FACTORS',//,
     &' |-----Development Phase------|-------------Environment--------',
     &'------|---------Stress----------|',/,
     &30X,'|--------Average-------|---Cumulative--|  (0=Min, 1=Max Str',
     &'ess)  |',/,
     &25X,'Time  Temp  Temp Solar Photop         Evapo |----Water---|-',
     &'-Nitrogen--|',/,
     &25X,'Span   Max   Min   Rad  [day]   Rain  Trans  Photo',9X,'Pho',
     &'to',/,
     &25X,'days    øC    øC MJ/m2     hr     mm     mm  synth Growth  ',
     &'synth Growth',/,96('-'))


      RETURN

      END SUBROUTINE OPSTRESS
!=======================================================================


