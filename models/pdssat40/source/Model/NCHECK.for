C=======================================================================
C  NCHECK, Subroutine
C
C  Checks for negative values of soil N and prints report if found.
C-----------------------------------------------------------------------
C  Revision history
C  12/22/1999 CHP written
C  03/16/2000 GH  Incorporated in CROPGRO
C               Note: File names etc. should be dynamically created 
C               Check time stamp
C-----------------------------------------------------------------------
      SUBROUTINE NCHECK(CONTROL, 
     &    CNRAT, FOM, FON, FPOOL, HUMC, HUMN, IFERI,      !Input
     &    IRESI, NLAYR, RESNIT, SNH4, SNO3, UREA)         !Input

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
      IMPLICIT NONE 
!-----------------------------------------------------------------------
      CHARACTER*1 IFERI, IRESI
      CHARACTER*30 FILEIO
      CHARACTER*78  MESSAGE(10)

      INTEGER DYNAMIC
      INTEGER L, NLAYR, RUN, YRDOY

      REAL RESNIT
      REAL CNRAT(NL), FOM(NL), FON(NL), FPOOL(NL,3), HUMC(NL), 
     &    HUMN(NL), SNH4(NL), SNO3(NL), UREA(NL)

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO 
      RUN     = CONTROL % RUN
      YRDOY   = CONTROL % YRDOY

!***********************************************************************
!***********************************************************************
!     Initialization section - Warning messages for Residue N, 
!       C:N ratio, fertilizer option, and residue option.
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Check initial values on first run only of sequential or
!         seasonal simulations.
      IF (RUN .EQ. 1) THEN

!       Check for impossible values of CNRAT from SOILNI. 
        DO L = 1, NLAYR
          IF (CNRAT(L) .LT. 2. .OR. CNRAT(L) .GT. 50.) THEN
            WRITE(MESSAGE(1),100) 
            WRITE(MESSAGE(2),110) L, CNRAT(L) 
            WRITE(MESSAGE(3),120) 
            CALL WARNING(3, "NCHECK", MESSAGE)
          ENDIF 
        ENDDO

  100 FORMAT('Warning: The initialized C/N ratio of the soil organic ')
  110 FORMAT('matter (humus) in layer ',I3,' is unlikely: ', F8.1)
  120 FORMAT('Please check your SOIL.SOL file.' )

!       Check for residues with an unlikely high N concentration after
!         initialization. 
        IF (RESNIT .GT. 4.) THEN
          WRITE(MESSAGE(1),200) YRDOY
          WRITE(MESSAGE(2),210) RESNIT
          CALL WARNING(2, "NCHECK", MESSAGE)
        ENDIF

  200 FORMAT('Warning: The residue N concentration on day ', I7)
  210 FORMAT('is quite high: ', F6.2, '%.')

!       Check for invalid fertilizer option.
        IF (INDEX('ARDN',IFERI) .EQ. 0) THEN
          WRITE(MESSAGE(1),300) IFERI
          WRITE(MESSAGE(2),310) 
          CALL WARNING(2, "NCHECK", MESSAGE)
        ENDIF

  300 FORMAT(
     &    'Warning: The fertilizer option, "',A1,'" is not currently ')
  310 FORMAT('supported.  No fertilizer applications were added.')

!       Check for invalid residue option.
        IF (INDEX('ARDN',IRESI) .EQ. 0) THEN
          WRITE(MESSAGE(1),400) IFERI
          WRITE(MESSAGE(2),410) 
          CALL WARNING(2, "NCHECK", MESSAGE)
        ENDIF

  400 FORMAT(
     &    'Warning: The residue option, "',A1,'" is not currently ')
  410 FORMAT('supported.  No residue applications were added.')

      ENDIF

!***********************************************************************
!***********************************************************************
!     Rate section - Warning messages for Residue N 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
!     Check for residues with an unlikely high N concentration after 
!         rate calculation. 
      IF (RESNIT .GT. 4.) THEN
        WRITE(MESSAGE(1),200) YRDOY
        WRITE(MESSAGE(2),210) RESNIT
        CALL WARNING(2, "NCHECK", MESSAGE)
      ENDIF
      
!***********************************************************************
!***********************************************************************
!     Daily integration/output
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. INTEGR) THEN
!-----------------------------------------------------------------------
!     Check for negative soil N values
      DO L = 1, NLAYR
        IF (SNO3(L).LT. -0.001) CALL NWRITE(YRDOY,L,SNO3(L),1)
        IF (SNH4(L).LT. -0.001) CALL NWRITE(YRDOY,L,SNH4(L),2)
        IF (UREA(L).LT. -0.001) CALL NWRITE(YRDOY,L,UREA(L),3)
        IF (FOM(L) .LT. -0.001) CALL NWRITE(YRDOY,L,FOM(L), 4)
        IF (FON(L) .LT. -0.001) CALL NWRITE(YRDOY,L,FON(L), 5)
        IF (HUMC(L).LT. -0.001) CALL NWRITE(YRDOY,L,HUMC(L),6)
        IF (HUMN(L).LT. -0.001) CALL NWRITE(YRDOY,L,HUMN(L),7)
        IF (FPOOL(L,1) .LT. -0.001) CALL NWRITE(YRDOY, L, FPOOL(L,1), 8)
        IF (FPOOL(L,2) .LT. -0.001) CALL NWRITE(YRDOY, L, FPOOL(L,2), 9)
        IF (FPOOL(L,3) .LT. -0.001) CALL NWRITE(YRDOY, L, FPOOL(L,3),10)
      ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      RETURN
      END  !SUBROUTINE NCHECK

C=======================================================================
C  NWRITE, Subroutine
C
C  Writes negative values of soil N to Warning.OUT file
C-----------------------------------------------------------------------
C  Revision history
!  03/16/00 CHP written
!-----------------------------------------------------------------------
      SUBROUTINE NWRITE(YRDOY, L, VALUE, CODE)

!-----------------------------------------------------------------------
      IMPLICIT NONE 

      CHARACTER*78  MSG(10)
      INTEGER CODE, L, YRDOY
      REAL VALUE

!-----------------------------------------------------------------------
      WRITE(MSG(1),100) YRDOY, L
      WRITE(MSG(3),"('Value will be set to zero')")
  100 FORMAT('Negative soil N value on day ',I7,' in layer ',I3)

      SELECT CASE (CODE)
      CASE (1); WRITE(MSG(2),
     &  "('Nitrate (SNO3) = ',F10.3,'kg[N]/ha')") VALUE
      
      CASE (2); WRITE(MSG(2),
     &  "('Ammonium (SNH4) = ',F10.3,'kg[N]/ha')") VALUE

      CASE (3); WRITE(MSG(2),
     &  "('UREA = ',F10.3,'kg[N]/ha')") VALUE

      CASE (4); WRITE(MSG(2),
     &  "('Fresh organic matter (FOM) = ',F10.3,'kg[residue]/ha')")
     &   VALUE

      CASE (5); WRITE(MSG(2),
     &  "('N in residue (FON) = ',F10.3,'kg[N]/ha')") VALUE

      CASE (6); WRITE(MSG(2),
     &  "('C in humus (HUMC) = ',F10.3,'kg[C]/ha')") VALUE

      CASE (7); WRITE(MSG(2),
     &  "('N in humus (HUMN) = ',F10.3,'kg[N]/ha')") VALUE

      CASE (8); WRITE(MSG(2),
     &  "('FOM carbohydrate pool (FPOOL(L,1)) = ',F10.3,'kg/ha')") 
     &    VALUE

      CASE (9); WRITE(MSG(2),
     &  "('FOM cellulose pool (FPOOL(L,2)) = ',F10.3,'kg/ha')") VALUE

      CASE (10); WRITE(MSG(2),
     &  "('FOM lignin pool (FPOOL(L,3)) = ',F10.3,'kg/ha')") VALUE

      END SELECT

      CALL WARNING(3, "NCHECK", MSG)
      VALUE = 0.0

      RETURN
      END SUBROUTINE NWRITE

!==========================================================================
! NCHECK, NWRITE Variable list
!==========================================================================
! CNRAT(L)   C/N ratio of humus or humus pool in soil layer L
!              (kg [C] / kg [N])
! CODE       Code for negative soil nitrogen values to be written to 
!              warning file 
! DYNAMIC    Module control variable; =RUNINIT, SEASINIT, RATE, EMERG, 
!              INTEGR, OUTPUT, or FINAL 
! FOM(L)     Fresh organic residue in soil layer L (kg [dry matter] / ha)
! FON(L)     Nitrogen in fresh organic matter in soil layer L (kg [N] / ha)
! FPOOL(L,J) FOM pool in soil layer L: J=1:carbohydrate, 2:cellulose, 
!              3:lignin (kg [residue pool] / ha)
! HUMC(L)    Carbon in stable organic matter (humus) (kg [C] / ha)
! HUMN(L)    Nitrogen in stable organic matter (humus) (kg [N] / ha)
! IFERI      Fertilizer switch (A= automatic, R= on specified dates (YYDDD 
!              format), D = on specified days after planting (DDD) format. 
! IRESI      Residue application method. A=Automatic residue application at 
!              a certain days after planting for multiple years; N=No 
!              residue; R=On reported dates; D=As reported, in DAP; F=Auto, 
!              with fixed amounts 
! LUNWARN    Logical unit number for Warning.OUT file 
! NL         Maximum number of soil layers = 20 
! NLAYR      Actual number of soil layers 
! RESNIT     N concentration of the residue for current application (%)
! SNH4(L)    Total extractable ammonium N in soil layer L (kg [N] / ha)
! SNO3(L)    Total extractable nitrate N in soil layer L (kg [N] / ha)
! UREA(L)    Amount of urea in soil layer L (kg [N] / ha)
! VALUE      Value of variable written to warning file 
! YRDOY      Current day of simulation (YYDDD)
!==========================================================================
