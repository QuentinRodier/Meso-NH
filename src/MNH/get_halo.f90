!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 newsrc 2007/03/01 13:18:33
!-----------------------------------------------------------------
!     ####################
      MODULE MODI_GET_HALO
!     ####################
!
INTERFACE
!
SUBROUTINE GET_HALO2(PSRC,TP_PSRC_HALO2_ll)
!
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC    ! variable at t
TYPE(HALO2LIST_ll), POINTER         :: TP_PSRC_HALO2_ll          ! halo2 for SRC
!
END SUBROUTINE GET_HALO2
!
SUBROUTINE GET_HALO(PSRC)
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC    ! variable at t
!
END SUBROUTINE GET_HALO
!
SUBROUTINE DEL_HALO2_ll(TPHALO2LIST)
!
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
TYPE(HALO2LIST_ll), POINTER :: TPHALO2LIST ! list of HALO2_lls
!
END SUBROUTINE DEL_HALO2_ll
!
END INTERFACE
!
END MODULE MODI_GET_HALO         
!
!     ###########################################
      SUBROUTINE GET_HALO2(PSRC,TP_PSRC_HALO2_ll)
!     ###########################################
!
USE MODE_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll, HALO2LIST_ll
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC    ! variable at t
TYPE(HALO2LIST_ll), POINTER         :: TP_PSRC_HALO2_ll          ! halo2 for SRC
!
INTEGER                          :: IIU,IJU,IKU            ! domain sizes
TYPE(LIST_ll)     , POINTER      :: TZ_PSRC_ll               ! halo
INTEGER                          :: IERROR                 ! error return code 
!
IIU = SIZE(PSRC,1)
IJU = SIZE(PSRC,2)
IKU = SIZE(PSRC,3)
!
NULLIFY( TZ_PSRC_ll,TP_PSRC_HALO2_ll)
CALL INIT_HALO2_ll(TP_PSRC_HALO2_ll,1,IIU,IJU,IKU)
!
CALL ADD3DFIELD_ll(TZ_PSRC_ll,PSRC)
CALL UPDATE_HALO_ll(TZ_PSRC_ll,IERROR)
CALL UPDATE_HALO2_ll(TZ_PSRC_ll,TP_PSRC_HALO2_ll,IERROR)
!
!   clean local halo list
!
CALL CLEANLIST_ll(TZ_PSRC_ll)
!
END SUBROUTINE GET_HALO2
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     #########################
      SUBROUTINE GET_HALO(PSRC)
!     #########################
!
USE MODE_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC    ! variable at t
!
TYPE(LIST_ll)     , POINTER      :: TZ_PSRC_ll               ! halo
INTEGER                          :: IERROR                 ! error return code 
!
NULLIFY( TZ_PSRC_ll)
!
CALL ADD3DFIELD_ll(TZ_PSRC_ll,PSRC)
CALL UPDATE_HALO_ll(TZ_PSRC_ll,IERROR)
CALL CLEANLIST_ll(TZ_PSRC_ll)
!
END SUBROUTINE GET_HALO
!-----------------------------------------------------------------------
!
!     ####################################
      SUBROUTINE DEL_HALO2_ll(TPHALO2LIST)
!     ####################################
!
!!****  *DEL_HALO2_ll* delete the second layer of the halo
!!
!!
!!    Purpose
!!    -------
!       The purpose of this routine is to deallocate the 
!     TPHALO2LIST variable which contains the second layer of the
!     halo for each variable.
! 
!!    Implicit Arguments
!!    ------------------
!     Module MODD_ARGSLIST_ll
!       type HALO2LIST_ll
!!
!!    Reference
!!    ---------
! 
!!    Author
!!    ------
!     J. Escobar                 * LA - CNRS *
!
!     Modification :
!     -------------
!     Juan  11/03/2010 : Memory Leak add DEALLOCATE(TZHALO2LIST%HALO2)     
! 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
  USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  TYPE(HALO2LIST_ll), POINTER :: TPHALO2LIST ! list of HALO2_lls
!
!
!*       0.2   Declarations of local variables :
!
  TYPE(HALO2LIST_ll), POINTER :: TZHALO2LIST
!
!-------------------------------------------------------------------------------
!
!*       1.    Deallocate the list of HALO2_lls
!
  TZHALO2LIST => TPHALO2LIST
!
  DO WHILE(ASSOCIATED(TZHALO2LIST))
!
    TPHALO2LIST => TZHALO2LIST%NEXT
    DEALLOCATE(TZHALO2LIST%HALO2%WEST)
    DEALLOCATE(TZHALO2LIST%HALO2%EAST)
    DEALLOCATE(TZHALO2LIST%HALO2%SOUTH)
    DEALLOCATE(TZHALO2LIST%HALO2%NORTH)
    DEALLOCATE(TZHALO2LIST%HALO2)
    DEALLOCATE(TZHALO2LIST)
    TZHALO2LIST => TPHALO2LIST
!
  ENDDO
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE DEL_HALO2_ll
