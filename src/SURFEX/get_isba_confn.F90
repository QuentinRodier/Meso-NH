!     ########################################
      SUBROUTINE GET_ISBA_CONF_n(KPATCH,KGROUND_LAYER,KSNOW_LAYER,KNBIOMASS,  &
                                   KNLITTER, KNLITTLEVS, KNSOILCARB)  
!     ########################################
!
!!****  *GET_ISBA_CONF_n* - routine to get some ISBA fields
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
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2008
!!      A.L. Gibelin 07/2009 : Dimensions for carbon options
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_n,     ONLY : NPATCH, NGROUND_LAYER, TSNOW, NNBIOMASS, &
                              NNLITTER, NNLITTLEVS, NNSOILCARB  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(OUT) :: KPATCH        ! number of patchs
INTEGER, INTENT(OUT) :: KGROUND_LAYER ! number of ground layers
INTEGER, INTENT(OUT) :: KSNOW_LAYER   ! number of snow layers
INTEGER, INTENT(OUT) :: KNBIOMASS     ! number of biomass pools
INTEGER, INTENT(OUT) :: KNLITTER      ! number of litter pools
INTEGER, INTENT(OUT) :: KNLITTLEVS    ! number of litter levels
INTEGER, INTENT(OUT) :: KNSOILCARB    ! number of soil carbon pools
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_ISBA_CONF_N',0,ZHOOK_HANDLE)
KPATCH = NPATCH
KGROUND_LAYER = NGROUND_LAYER
KSNOW_LAYER = TSNOW%NLAYER
KNBIOMASS = NNBIOMASS
KNLITTER = NNLITTER
KNLITTLEVS = NNLITTLEVS
KNSOILCARB = NNSOILCARB
IF (LHOOK) CALL DR_HOOK('GET_ISBA_CONF_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_ISBA_CONF_n
