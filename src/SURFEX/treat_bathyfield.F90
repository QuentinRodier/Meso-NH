!     #########
      SUBROUTINE TREAT_BATHYFIELD(HPROGRAM,HSCHEME,HFILETYPE,    &
                              HSUBROUTINE,HFILENAME,HNCVARNAME,   &
                              HFIELD, PPGDARRAY,HSFTYPE               )  
!     ##############################################################
!
!!**** *TREAT_BATHYFIELD* chooses which treatment subroutine to use to read 
!!                        the bathymetry
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
!!    C. Lebeaupin Brossier        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!    
!!    Original    01/2008
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_LUOUT
USE MODI_READ_DIRECT
USE MODI_READ_BINLLV
USE MODI_READ_BINLLVFAST
USE MODI_READ_ASCLLV
USE MODI_READ_NETCDF
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
USE MODI_AVERAGE2_OROGRAPHY
!
USE MODI_READ_DIRECT_GAUSS
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
 CHARACTER(LEN=28), INTENT(IN) :: HNCVARNAME    ! Name of the variable in netcdf file
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
IF (LHOOK) CALL DR_HOOK('TREAT_BATHYFIELD',0,ZHOOK_HANDLE)
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

   CASE ('NETCDF')
       CALL READ_NETCDF(HPROGRAM,HSUBROUTINE,HFILENAME,HNCVARNAME)

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

  CASE ('A_MESH')
    IF (.NOT. PRESENT(PPGDARRAY)) THEN
      WRITE(ILUOUT,*) 'You asked to average a PGD field with A_MESH option,'
      WRITE(ILUOUT,*) 'but you did not give the array to store this field'
      CALL ABOR1_SFX('TREAT_BATHYFIELD: PGD ARRAY IS MISSING')
    END IF
    CALL AVERAGE2_MESH(PPGDARRAY)

END SELECT
IF (LHOOK) CALL DR_HOOK('TREAT_BATHYFIELD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TREAT_BATHYFIELD
