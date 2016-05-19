!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

!     #####################
      MODULE MODI_UPDATE_ll 
!     #####################
!
INTERFACE
!
!!     ##########################################
       SUBROUTINE UPDATE_HALO_ll( TPLIST, KINFO )
!!     ##########################################
!
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll
!
  TYPE(LIST_ll), POINTER :: TPLIST ! pointer to the list of fields to be updated
  INTEGER                :: KINFO  ! return status
!
       END SUBROUTINE UPDATE_HALO_ll
!
!!     ############################################
       SUBROUTINE UPDATE_1DHALO_ll( TPLIST, KINFO )
!!     ############################################
!
  USE MODD_ARGSLIST_ll, ONLY : LIST1D_ll
!
  TYPE(LIST1D_ll), POINTER :: TPLIST
  INTEGER, INTENT(OUT) :: KINFO
!
       END SUBROUTINE UPDATE_1DHALO_ll
!
!!     ############################################################
       SUBROUTINE UPDATE_BOUNDARIES_ll( HDIRECTION, TPLIST, KINFO )
!!     ############################################################
!
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll
!
  CHARACTER*2, INTENT(IN) :: HDIRECTION
  TYPE(LIST_ll), POINTER :: TPLIST ! pointer to the list of fields to be updated
  INTEGER                :: KINFO  ! return status
!
       END SUBROUTINE UPDATE_BOUNDARIES_ll
!
!
!!     ##################################################################
       SUBROUTINE INIT_HALO2_ll(TPHALO2LIST, KNBVAR, KDIMX, KDIMY, KDIMZ)
!!     ##################################################################
!
  USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
  TYPE(HALO2LIST_ll), POINTER :: TPHALO2LIST ! list of HALO2_lls
  INTEGER                     :: KNBVAR      ! number of HALO2_lls to allocate
  INTEGER        :: KDIMX, KDIMY, KDIMZ      ! dimensions of the HALO2_lls
!
       END SUBROUTINE INIT_HALO2_ll
!
!!     ########################################################
       SUBROUTINE UPDATE_HALO2_ll( TPLIST, TPLISTHALO2, KINFO )
!!     ########################################################
!
  USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll,LIST_ll
!
  TYPE(LIST_ll), POINTER      :: TPLIST      ! pointer to the list of 
                                             ! fields to be sent
  TYPE(HALO2LIST_ll), POINTER :: TPLISTHALO2 ! pointer to the list of
                                             ! halo2 to be received
  INTEGER                     :: KINFO       ! return status
!
       END SUBROUTINE UPDATE_HALO2_ll
!
END INTERFACE
!
END MODULE MODI_UPDATE_ll
