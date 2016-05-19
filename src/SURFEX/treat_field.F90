!     #########
      SUBROUTINE TREAT_FIELD(HPROGRAM,HSCHEME,HFILETYPE,    &
                              HSUBROUTINE,HFILENAME,HFIELD,   &
                              PPGDARRAY,HSFTYPE               )  
!     ##############################################################
!
!!**** *TREAT_FIELD* chooses which treatment subroutine to use
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    11/09/95
!!
!!    Modification
!!    25/05/96    (V. Masson) remove useless case for HSUBROUTINE   
!!    29/11/2002  (D. Gazen)  add HSFTYPE argument + call to read_binllvfast routine
!!    03/2004     (V. MAsson) externalization
!!    04/2009     (B. Decharme) Special treatement for gaussian grid
!!    06/2009     (B. Decharme)  call Topographic index statistics calculation
!!    09/2010     (E. Kourzeneva) call reading of the lake database
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_LUOUT
USE MODI_READ_DIRECT
USE MODI_READ_DIRECT_GAUSS
USE MODI_READ_LATLON
USE MODI_READ_BINLLV
USE MODI_READ_BINLLVFAST
USE MODI_READ_ASCLLV
USE MODI_AVERAGE2_MESH
!
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_AVERAGE2_COVER
!
USE MODI_AVERAGE2_CTI
USE MODI_AVERAGE2_LDB
!
USE MODI_AVERAGE2_OROGRAPHY
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM      ! Type of program
 CHARACTER(LEN=6),  INTENT(IN) :: HSCHEME       ! Scheme treated
 CHARACTER(LEN=6),  INTENT(IN) :: HFILETYPE     ! Type of the data file
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
 CHARACTER(LEN=20), INTENT(IN) :: HFIELD        ! Name of the field.
REAL, DIMENSION(:), INTENT(INOUT), OPTIONAL :: PPGDARRAY ! field on MESONH grid
 CHARACTER(LEN=3),   INTENT(IN),    OPTIONAL :: HSFTYPE
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TREAT_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.     Selection of type of reading (and point by point treatment)
!            -----------------------------------------------------------
!
SELECT CASE (HFILETYPE)

   CASE ('DIRECT')
         IF(CGRID=="GAUSS     ")THEN
           CALL READ_DIRECT_GAUSS(HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD)
         ELSE
           CALL READ_DIRECT(HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD)
         ENDIF

   CASE ('BINLLV')
       CALL READ_BINLLV(HPROGRAM,HSUBROUTINE,HFILENAME)

   CASE ('BINLLF')
       CALL READ_BINLLVFAST(HPROGRAM,HSUBROUTINE,HFILENAME)

   CASE ('ASCLLV')
       CALL READ_ASCLLV(HPROGRAM,HSUBROUTINE,HFILENAME)

   CASE ('LATLON')
       CALL READ_LATLON(HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME)

   CASE DEFAULT
     CALL ABOR1_SFX('TREAT_FIELD: FILE TYPE NOT SUPPORTED: '//HFILETYPE)

END SELECT
!
!-------------------------------------------------------------------------------
!
!*    2.     Call to the adequate subroutine (global treatment)
!            --------------------------------------------------
!
SELECT CASE (HSUBROUTINE)

  CASE ('A_COVR')
    CALL AVERAGE2_COVER

  CASE ('A_OROG')
    CALL AVERAGE2_OROGRAPHY

  CASE ('A_CTI ')
    CALL AVERAGE2_CTI

  CASE ('A_LDBD')
    CALL AVERAGE2_LDB(PPGDARRAY,'D',1)

  CASE ('A_LDBS')
    CALL AVERAGE2_LDB(PPGDARRAY,'S',1)
    
  CASE ('A_MESH')
    IF (.NOT. PRESENT(PPGDARRAY)) THEN
      WRITE(ILUOUT,*) 'You asked to average a PGD field with A_MESH option,'
      WRITE(ILUOUT,*) 'but you did not give the array to store this field'
      CALL ABOR1_SFX('TREAT_FIELD: ARRAY IS MISSING')
    END IF
    CALL AVERAGE2_MESH(PPGDARRAY)

END SELECT
!
IF (LHOOK) CALL DR_HOOK('TREAT_FIELD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TREAT_FIELD
