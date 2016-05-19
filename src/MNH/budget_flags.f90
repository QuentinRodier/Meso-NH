!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 budget 2006/05/18 13:07:25
!-----------------------------------------------------------------
!##################
 MODULE MODI_BUDGET_FLAGS
!##################
!
INTERFACE
!
SUBROUTINE BUDGET_FLAGS(OUSERV, OUSERC, OUSERR,         &
                        OUSERI, OUSERS, OUSERG, OUSERH  )
!
!
LOGICAL, INTENT(IN) :: OUSERV    ! flag to use water vapor
LOGICAL, INTENT(IN) :: OUSERC    ! flag to use cloud
LOGICAL, INTENT(IN) :: OUSERR    ! flag to use rain
LOGICAL, INTENT(IN) :: OUSERI    ! flag to use ice
LOGICAL, INTENT(IN) :: OUSERS    ! flag to use snow
LOGICAL, INTENT(IN) :: OUSERG    ! flag to use graupel
LOGICAL, INTENT(IN) :: OUSERH    ! flag to use hail
!
END SUBROUTINE BUDGET_FLAGS
!
END INTERFACE
!
END MODULE MODI_BUDGET_FLAGS
!     ###################################################
SUBROUTINE BUDGET_FLAGS(OUSERV, OUSERC, OUSERR,         &
                        OUSERI, OUSERS, OUSERG, OUSERH  )
!     ###################################################
!
!!****  *BUDGET_FLAGS* - routine to initialize the flags to call the budget
!!                       routines
!!                           
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!       
!!     
!!
!!    EXTERNAL
!!    --------
!!      CART_COMPRESS 
!!      MASK_COMPRESS
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!      None
!!
!!    AUTHOR
!!    ------
!!  	V. Masson        * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/10/02
!!      
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BUDGET
USE MODD_NSV, ONLY : NSV
USE MODD_LES
!
IMPLICIT NONE
!  
!  
!*       0.1   Declarations of arguments :
!
LOGICAL, INTENT(IN) :: OUSERV    ! flag to use water vapor
LOGICAL, INTENT(IN) :: OUSERC    ! flag to use cloud
LOGICAL, INTENT(IN) :: OUSERR    ! flag to use rain
LOGICAL, INTENT(IN) :: OUSERI    ! flag to use ice
LOGICAL, INTENT(IN) :: OUSERS    ! flag to use snow
LOGICAL, INTENT(IN) :: OUSERG    ! flag to use graupel
LOGICAL, INTENT(IN) :: OUSERH    ! flag to use hail

!-------------------------------------------------------------------------------
!
LBUDGET_U  = (LBU_ENABLE .AND. LBU_RU  ) .OR.  LLES_CALL
LBUDGET_V  = (LBU_ENABLE .AND. LBU_RV  ) .OR.  LLES_CALL
LBUDGET_W  = (LBU_ENABLE .AND. LBU_RW  ) .OR.  LLES_CALL
LBUDGET_TH = (LBU_ENABLE .AND. LBU_RTH ) .OR.  LLES_CALL
LBUDGET_TKE= (LBU_ENABLE .AND. LBU_RTKE) .OR.  LLES_CALL
LBUDGET_RV = (LBU_ENABLE .AND. LBU_RRV ) .OR. (LLES_CALL .AND. OUSERV)
LBUDGET_RC = (LBU_ENABLE .AND. LBU_RRC ) .OR. (LLES_CALL .AND. OUSERC)
LBUDGET_RR = (LBU_ENABLE .AND. LBU_RRR ) .OR. (LLES_CALL .AND. OUSERR)
LBUDGET_RI = (LBU_ENABLE .AND. LBU_RRI ) .OR. (LLES_CALL .AND. OUSERI)
LBUDGET_RS = (LBU_ENABLE .AND. LBU_RRS ) .OR. (LLES_CALL .AND. OUSERS)
LBUDGET_RG = (LBU_ENABLE .AND. LBU_RRG ) .OR. (LLES_CALL .AND. OUSERG)
LBUDGET_RH = (LBU_ENABLE .AND. LBU_RRH ) .OR. (LLES_CALL .AND. OUSERH)
LBUDGET_SV = (LBU_ENABLE .AND. LBU_RSV ) .OR. (LLES_CALL .AND. NSV>0 )
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE BUDGET_FLAGS
