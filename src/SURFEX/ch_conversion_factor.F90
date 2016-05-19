!     #########
      SUBROUTINE CH_CONVERSION_FACTOR(HCONVERSION,PRHOA)
!     #######################################
!
!!****  *CH_CONVERSION_FACTOR
!!
!!    PURPOSE
!!    -------
!     Determines the correct conversion factor to produce emissions in
!     Molec/m2/s
!
!!**  METHOD
!!    ------
!!    
!!    
!!    AUTHOR
!!    ------
!!	S.QUEGUINER 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        11/2011
!!      A. Alias        07/2013 add MODI_ABOR1_SFX
!!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_CSTS,       ONLY : XAVOGADRO, XMD
USE MODD_CH_SNAP_n,  ONLY : XCONVERSION
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
 CHARACTER(LEN=3),  INTENT(IN)  :: HCONVERSION ! Unit conversion code
REAL, DIMENSION(:),INTENT(IN)  :: PRHOA       ! air density
!
!*       0.2   declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CH_CONVERSION_FACTOR',0,ZHOOK_HANDLE)
!
! determine the conversion factor
XCONVERSION(:) = 1.
SELECT CASE (HCONVERSION)
  CASE ('MIX') ! flux given ppp*m/s,  conversion to molec/m2/s
  ! where 1 molecule/cm2/s = (224.14/6.022136E23) ppp*m/s
    XCONVERSION(:) = XAVOGADRO * PRHOA(:) / XMD
  CASE ('CON') ! flux given in molecules/cm2/s, conversion to molec/m2/s 
    XCONVERSION(:) =  1E4
  CASE ('MOL') ! flux given in microMol/m2/day, conversion to molec/m2/s  
    ! where 1 microMol/m2/day = (22.414/86.400)*1E-12 ppp*m/s
    XCONVERSION(:) = (22.414/86.400)*1E-12 * XAVOGADRO * PRHOA(:) / XMD
  CASE DEFAULT
    CALL ABOR1_SFX('CH_BUILDEMISSN: UNKNOWN CONVERSION FACTOR')
END SELECT
!
IF (LHOOK) CALL DR_HOOK('CH_CONVERSION_FACTOR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE CH_CONVERSION_FACTOR
