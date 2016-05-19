!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 surfex 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_INI_SW_SETUP
!     ##########################
!
INTERFACE
!
    SUBROUTINE INI_SW_SETUP(HRAD,KSWB_MNH,PSW_BANDS)
!
CHARACTER(LEN=*), INTENT(IN)   :: HRAD      ! type of radiation scheme
INTEGER,          INTENT(IN)   :: KSWB_MNH  ! number of SW band
REAL,             DIMENSION(:) :: PSW_BANDS ! wavelength in the middle of each band
!
END SUBROUTINE INI_SW_SETUP
!
END INTERFACE
!
END MODULE MODI_INI_SW_SETUP
!
!
!   #######################################################################
    SUBROUTINE INI_SW_SETUP(HRAD,KSWB_MNH,PSW_BANDS)
!   #######################################################################
!
!!****  *INI_SW_SETUP * - initialisation for ECMWF radiation SW bands
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!! 
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/03/03
!!      modification : 01/09/03  Y. Seity, KSWB_MNH=6
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER(LEN=*), INTENT(IN)   :: HRAD      ! type of radiation scheme
INTEGER,          INTENT(IN)   :: KSWB_MNH  ! number of SW band
REAL,             DIMENSION(:) :: PSW_BANDS ! wavelength in the middle of each band
!
!*       0.2   declarations of local variables
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
SELECT CASE (HRAD)
  CASE ('ECMW')

! number of SW band used in MESONH between surface and radiation

    IF (KSWB_MNH  == 1) THEN
       PSW_BANDS = 0.7E-6
    ELSEIF (KSWB_MNH  == 6) THEN
       PSW_BANDS(1) = 0.2175E-6
       PSW_BANDS(2) = 0.345E-6
       PSW_BANDS(3) = 0.565E-6
       PSW_BANDS(4) = 0.94E-6
       PSW_BANDS(5) = 1.785E-6
       PSW_BANDS(6) = 3.19E-6
    ELSE
!callabortstop
CALL ABORT
       STOP     
    ENDIF

  CASE ('FIXE','TOPA','NONE')

! number of SW band used in MESONH between surface and radiation

    IF (KSWB_MNH  == 1) THEN
       PSW_BANDS = 0.7E-6
    ELSEIF (KSWB_MNH  == 6) THEN
       PSW_BANDS(1) = 0.2175E-6
       PSW_BANDS(2) = 0.345E-6
       PSW_BANDS(3) = 0.565E-6
       PSW_BANDS(4) = 0.94E-6
       PSW_BANDS(5) = 1.785E-6
       PSW_BANDS(6) = 3.19E-6
    ELSE
!callabortstop
CALL ABORT
       STOP     
    ENDIF

!
END SELECT
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_SW_SETUP
