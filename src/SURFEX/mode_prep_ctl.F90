MODULE MODE_PREP_CTL

!**** *MODE_PREP_CTL*  - Control PREP structure

!     Author. 
!     ------- 
!      Philippe Marguinaud *METEO FRANCE* 
!      Original : 01-10-2014
!
!
!     Modified.
!     ------------
!      B. Decharme 12/2020 : Add 4D field
!
!
!     Description.
!     ------------
!     This module implements a structure and methods to control PREP execution,
!     retrieve fields to be interpolated so that an external program may
!     interpol the fields itself (Fullpos), and pass back interpolated fields to
!     PREP.

USE MODI_ABOR1_SFX

IMPLICIT NONE

TYPE PREP_CTL_FLD
  CHARACTER (LEN=32) :: CLNAME = '', CLTYPE = '', CLMASK = ''
  REAL, POINTER :: ZFLD2 (:,:)   => NULL ()
  REAL, POINTER :: ZFLD3 (:,:,:) => NULL ()
  REAL, POINTER :: ZFLD4 (:,:,:,:) => NULL ()
  TYPE (PREP_CTL_FLD), POINTER :: NEXT => NULL ()
  TYPE (PREP_CTL_FLD), POINTER :: PREV => NULL ()
END TYPE PREP_CTL_FLD

TYPE PREP_CTL
  LOGICAL :: LSTEP0 = .FALSE. ! PREP step 0 (count fields to be interpolated)
  LOGICAL :: LSTEP1 = .FALSE. ! PREP step 1 (retrieve fields to be interpolated)
  LOGICAL :: LSTEP2 = .FALSE. ! PREP step 2 (pass interpolated fields to PREP and finish)

  LOGICAL :: LPART1 = .TRUE.  ! Invoke PREP_*_EXTERN routines
  LOGICAL :: LPART2 = .FALSE. ! Retrieve or pass back interpolated fields from/to PREP
  LOGICAL :: LPART3 = .TRUE.  ! Do interpolations
  LOGICAL :: LPART4 = .FALSE. ! Unused for now
  LOGICAL :: LPART5 = .TRUE.  ! Post-processing after interpolations
  LOGICAL :: LPART6 = .TRUE.  ! Post-processing after interpolations (higher level)
  TYPE (PREP_CTL_FLD), POINTER :: HEAD => NULL (), TAIL => NULL ()
END TYPE

INTERFACE PREP_CTL_INT_PART2
  MODULE PROCEDURE PREP_CTL_INT_PART2_2, PREP_CTL_INT_PART2_3, PREP_CTL_INT_PART2_4
END INTERFACE

INTERFACE PREP_CTL_INT_PART4
  MODULE PROCEDURE PREP_CTL_INT_PART4_2, PREP_CTL_INT_PART4_3, PREP_CTL_INT_PART4_4

END INTERFACE

CONTAINS

LOGICAL FUNCTION PREP_CTL_CAN (YDCTL)

! Returns true if we are not in a 2-part PREP

TYPE (PREP_CTL), INTENT (INOUT) :: YDCTL

TYPE (PREP_CTL) :: YLCTL

PREP_CTL_CAN = (YDCTL%LSTEP0 .EQV. YLCTL%LSTEP0) .AND. (YDCTL%LSTEP1 .EQV. YLCTL%LSTEP1) .AND. &
             & (YDCTL%LSTEP2 .EQV. YLCTL%LSTEP2) .AND. (YDCTL%LPART1 .EQV. YLCTL%LPART1) .AND. &
             & (YDCTL%LPART1 .EQV. YLCTL%LPART1) .AND. (YDCTL%LPART2 .EQV. YLCTL%LPART2) .AND. &
             & (YDCTL%LPART3 .EQV. YLCTL%LPART3) .AND. (YDCTL%LPART4 .EQV. YLCTL%LPART4) .AND. &
             & (YDCTL%LPART5 .EQV. YLCTL%LPART5) .AND. (YDCTL%LPART6 .EQV. YLCTL%LPART6) 
  

END FUNCTION PREP_CTL_CAN

SUBROUTINE PREP_CTL_COUNT (YDCTL, KCOUNT)

! Reckon the number of 2D fields in YDCTL

TYPE (PREP_CTL),     INTENT (INOUT) :: YDCTL
INTEGER,             INTENT (OUT)   :: KCOUNT

TYPE (PREP_CTL_FLD), POINTER        :: YLCFL

KCOUNT = 0

YLCFL => YDCTL%HEAD
DO WHILE (ASSOCIATED (YLCFL))
  IF (ASSOCIATED (YLCFL%ZFLD2)) THEN
    KCOUNT = KCOUNT + SIZE (YLCFL%ZFLD2, 2)
  ELSEIF (ASSOCIATED (YLCFL%ZFLD3)) THEN
    KCOUNT = KCOUNT + SIZE (YLCFL%ZFLD3, 2) * SIZE (YLCFL%ZFLD3, 3)
  ELSEIF (ASSOCIATED (YLCFL%ZFLD4)) THEN
    KCOUNT = KCOUNT + SIZE (YLCFL%ZFLD3, 2) * SIZE (YLCFL%ZFLD3, 3) * SIZE (YLCFL%ZFLD4, 4)
  ENDIF
  YLCFL => YLCFL%NEXT
ENDDO

END SUBROUTINE PREP_CTL_COUNT

SUBROUTINE PREP_CTL_FREE (YDCTL)

! Free a whole PREP_CTL structure

TYPE (PREP_CTL),     INTENT (INOUT) :: YDCTL

TYPE (PREP_CTL_FLD), POINTER        :: YLCFL

YLCFL => YDCTL%HEAD
DO WHILE (ASSOCIATED (YLCFL))
  IF (ASSOCIATED (YLCFL%ZFLD2)) THEN
    DEALLOCATE (YLCFL%ZFLD2)
  ELSEIF (ASSOCIATED (YLCFL%ZFLD3)) THEN
    DEALLOCATE (YLCFL%ZFLD3)
  ELSEIF (ASSOCIATED (YLCFL%ZFLD4)) THEN
    DEALLOCATE (YLCFL%ZFLD4)
  ENDIF
  YLCFL => YLCFL%NEXT
  DEALLOCATE (YDCTL%HEAD)
  YDCTL%HEAD => YLCFL
ENDDO

NULLIFY (YDCTL%HEAD, YDCTL%TAIL)

END SUBROUTINE PREP_CTL_FREE

SUBROUTINE PREP_CTL_PUSH (YDCTL, YDCFL)

! Add a new set of fields

TYPE (PREP_CTL),     INTENT (INOUT) :: YDCTL
TYPE (PREP_CTL_FLD), POINTER        :: YDCFL

IF (ASSOCIATED (YDCTL%TAIL)) THEN
  YDCTL%TAIL%NEXT => YDCFL
  YDCFL%PREV => YDCTL%TAIL
  YDCTL%TAIL => YDCFL
ELSE
  YDCTL%HEAD => YDCFL
  YDCTL%TAIL => YDCFL
ENDIF

NULLIFY (YDCFL)

END SUBROUTINE PREP_CTL_PUSH

SUBROUTINE PREP_CTL_SHIFT (YDCTL, YDCFL)

! Get back a set of fields

TYPE (PREP_CTL),     INTENT (INOUT) :: YDCTL
TYPE (PREP_CTL_FLD), POINTER        :: YDCFL

YDCFL => YDCTL%HEAD

IF (.NOT. ASSOCIATED (YDCFL)) THEN
  CALL ABOR1_SFX ('PREP_CTL_SHIFT: ATTEMPT TO SHIFT EMPTY FIELD LIST')
ENDIF

YDCTL%HEAD => YDCFL%NEXT

IF (ASSOCIATED (YDCTL%HEAD)) THEN
  YDCTL%HEAD%PREV => NULL ()
ENDIF

END SUBROUTINE PREP_CTL_SHIFT

SUBROUTINE PREP_CTL_INT_PART2_2 (YDCTL, CDSURF, CDTYPE, CDMASK, PFIELDIN)

! Retrieve/pass back fields

TYPE (PREP_CTL),   INTENT (INOUT) :: YDCTL
CHARACTER (LEN=*), INTENT (IN)    :: CDSURF
CHARACTER (LEN=*), INTENT (IN)    :: CDTYPE
CHARACTER (LEN=*), INTENT (IN)    :: CDMASK
REAL,              POINTER        :: PFIELDIN (:,:)

TYPE (PREP_CTL_FLD), POINTER :: YLCFL

INTEGER :: IREP

IF (YDCTL%LPART2) THEN
  IF (YDCTL%LSTEP0 .OR. YDCTL%LSTEP1) THEN
    ALLOCATE (YLCFL)
    YLCFL%CLTYPE = CDTYPE
    YLCFL%CLNAME = CDSURF
    YLCFL%CLMASK = CDMASK
    YLCFL%ZFLD2 => PFIELDIN
    NULLIFY (PFIELDIN)
    CALL PREP_CTL_PUSH (YDCTL, YLCFL)
  ELSEIF (YDCTL%LSTEP2) THEN
    CALL PREP_CTL_SHIFT (YDCTL, YLCFL)
    IF (YLCFL%CLNAME /= CDSURF .OR. YLCFL%CLTYPE /= CDTYPE) THEN
      CALL ABOR1_SFX ('PREP_CTL_INT_PART2: FIELD MISMATCH: EXPECTED '&
                    &//TRIM (CDSURF)//'/'//TRIM (CDTYPE)//', GOT '// &
                    &TRIM (YLCFL%CLNAME)//'/'//TRIM (YLCFL%CLTYPE))
    ENDIF
    PFIELDIN => YLCFL%ZFLD2
    NULLIFY (YLCFL%ZFLD2)
    DEALLOCATE (YLCFL)
  ENDIF
ENDIF

END SUBROUTINE PREP_CTL_INT_PART2_2

SUBROUTINE PREP_CTL_INT_PART4_2 (YDCTL, CDSURF, CDTYPE, CDMASK, PFIELDIN, PFIELDOUT)

TYPE (PREP_CTL),   INTENT (INOUT) :: YDCTL
CHARACTER (LEN=*), INTENT (IN)    :: CDSURF
CHARACTER (LEN=*), INTENT (IN)    :: CDTYPE
CHARACTER (LEN=*), INTENT (IN)    :: CDMASK
REAL,              POINTER        :: PFIELDIN  (:,:)
REAL,              POINTER        :: PFIELDOUT (:,:)

TYPE (PREP_CTL_FLD), POINTER :: YLCFL
INTEGER :: IREP, ILONG, IPOSEX
INTEGER (8) :: JDIM (2)

IF (YDCTL%LPART4) THEN
ENDIF

END SUBROUTINE PREP_CTL_INT_PART4_2

SUBROUTINE PREP_CTL_INT_PART2_3 (YDCTL, CDSURF, CDTYPE, CDMASK, PFIELDIN)

! Retrieve/pass back fields

TYPE (PREP_CTL),   INTENT (INOUT) :: YDCTL
CHARACTER (LEN=*), INTENT (IN)    :: CDSURF
CHARACTER (LEN=*), INTENT (IN)    :: CDTYPE
CHARACTER (LEN=*), INTENT (IN)    :: CDMASK
REAL,              POINTER        :: PFIELDIN (:,:,:)

TYPE (PREP_CTL_FLD), POINTER :: YLCFL

INTEGER :: IREP

IF (YDCTL%LPART2) THEN
  IF (YDCTL%LSTEP0 .OR. YDCTL%LSTEP1) THEN
    ALLOCATE (YLCFL)
    YLCFL%CLTYPE = CDTYPE
    YLCFL%CLNAME = CDSURF
    YLCFL%CLMASK = CDMASK
    YLCFL%ZFLD3 => PFIELDIN
    NULLIFY (PFIELDIN)
    CALL PREP_CTL_PUSH (YDCTL, YLCFL)
  ELSEIF (YDCTL%LSTEP2) THEN
    CALL PREP_CTL_SHIFT (YDCTL, YLCFL)
    IF (YLCFL%CLNAME /= CDSURF .OR. YLCFL%CLTYPE /= CDTYPE) THEN
      CALL ABOR1_SFX ('PREP_CTL_INT_PART2: FIELD MISMATCH: EXPECTED '&
                    &//TRIM (CDSURF)//'/'//TRIM (CDTYPE)//', GOT '// &
                    &TRIM (YLCFL%CLNAME)//'/'//TRIM (YLCFL%CLTYPE))
    ENDIF
    PFIELDIN => YLCFL%ZFLD3
    NULLIFY (YLCFL%ZFLD3)
    DEALLOCATE (YLCFL)
  ENDIF
ENDIF

END SUBROUTINE PREP_CTL_INT_PART2_3

SUBROUTINE PREP_CTL_INT_PART4_3 (YDCTL, CDSURF, CDTYPE, CDMASK, PFIELDIN, PFIELDOUT)

TYPE (PREP_CTL),   INTENT (INOUT) :: YDCTL
CHARACTER (LEN=*), INTENT (IN)    :: CDSURF
CHARACTER (LEN=*), INTENT (IN)    :: CDTYPE
CHARACTER (LEN=*), INTENT (IN)    :: CDMASK
REAL,              POINTER        :: PFIELDIN  (:,:,:)
REAL,              POINTER        :: PFIELDOUT (:,:,:)

INTEGER :: IREP, ILONG, IPOSEX
INTEGER (8) :: JDIM (3)
TYPE (PREP_CTL_FLD), POINTER :: YLCFL

IF (YDCTL%LPART4) THEN
ENDIF

END SUBROUTINE PREP_CTL_INT_PART4_3


SUBROUTINE PREP_CTL_INT_PART2_4 (YDCTL, CDSURF, CDTYPE, CDMASK, PFIELDIN)

! Retrieve/pass back fields

TYPE (PREP_CTL),   INTENT (INOUT) :: YDCTL
CHARACTER (LEN=*), INTENT (IN)    :: CDSURF
CHARACTER (LEN=*), INTENT (IN)    :: CDTYPE
CHARACTER (LEN=*), INTENT (IN)    :: CDMASK
REAL,              POINTER        :: PFIELDIN (:,:,:,:)

TYPE (PREP_CTL_FLD), POINTER :: YLCFL

INTEGER :: IREP

IF (YDCTL%LPART2) THEN
  IF (YDCTL%LSTEP0 .OR. YDCTL%LSTEP1) THEN
    ALLOCATE (YLCFL)
    YLCFL%CLTYPE = CDTYPE
    YLCFL%CLNAME = CDSURF
    YLCFL%CLMASK = CDMASK
    YLCFL%ZFLD4 => PFIELDIN
    NULLIFY (PFIELDIN)
    CALL PREP_CTL_PUSH (YDCTL, YLCFL)
  ELSEIF (YDCTL%LSTEP2) THEN
    CALL PREP_CTL_SHIFT (YDCTL, YLCFL)
    IF (YLCFL%CLNAME /= CDSURF .OR. YLCFL%CLTYPE /= CDTYPE) THEN
      CALL ABOR1_SFX ('PREP_CTL_INT_PART2: FIELD MISMATCH: EXPECTED '&
                    &//TRIM (CDSURF)//'/'//TRIM (CDTYPE)//', GOT '// &
                    &TRIM (YLCFL%CLNAME)//'/'//TRIM (YLCFL%CLTYPE))
    ENDIF
    PFIELDIN => YLCFL%ZFLD4
    NULLIFY (YLCFL%ZFLD4)
    DEALLOCATE (YLCFL)
  ENDIF
ENDIF

END SUBROUTINE PREP_CTL_INT_PART2_4

SUBROUTINE PREP_CTL_INT_PART4_4 (YDCTL, CDSURF, CDTYPE, CDMASK, PFIELDIN, PFIELDOUT)

TYPE (PREP_CTL),   INTENT (INOUT) :: YDCTL
CHARACTER (LEN=*), INTENT (IN)    :: CDSURF
CHARACTER (LEN=*), INTENT (IN)    :: CDTYPE
CHARACTER (LEN=*), INTENT (IN)    :: CDMASK
REAL,              POINTER        :: PFIELDIN  (:,:,:,:)
REAL,              POINTER        :: PFIELDOUT (:,:,:,:)

INTEGER :: IREP, ILONG, IPOSEX
INTEGER (8) :: JDIM (4)
TYPE (PREP_CTL_FLD), POINTER :: YLCFL

IF (YDCTL%LPART4) THEN
ENDIF

END SUBROUTINE PREP_CTL_INT_PART4_4

END MODULE

