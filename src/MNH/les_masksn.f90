!MNH_LIC Copyright 2000-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ################
MODULE MODI_LES_MASKS_n
!      ################
!
!
!
INTERFACE LES_MASKS_n
!
      SUBROUTINE LES_MASKS_n(KTCOUNT)
!
INTEGER, INTENT(IN) :: KTCOUNT ! current model time-step
!
END SUBROUTINE LES_MASKS_n
!
END INTERFACE
!
END MODULE MODI_LES_MASKS_n

!     #######################
      SUBROUTINE  LES_MASKS_n(KTCOUNT)
!     #######################
!
!
!!****  *LES_MASKS_n* initializes the LES variables for
!!                    the current time-step of model _n
!!                         
!!
!!    PURPOSE
!!    -------
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
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         07/02/00
!!
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CONF_n
USE MODD_DYN_n
USE MODD_FIELD_n
USE MODD_LES
USE MODD_LES_n
USE MODD_TIME
USE MODD_TIME_n
!
use mode_datetime,       only: Datetime_distance
USE MODE_ll
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
INTEGER, INTENT(IN) :: KTCOUNT ! current model time-step
!
!
!       0.2  declaration of local variables
!
INTEGER :: IXOR_ll, IYOR_ll                    ! origine point coordinates
!                                              ! of current processor domain
!                                              ! on model domain on all
!                                              ! processors
INTEGER :: IIB_ll, IJB_ll                      ! SO point coordinates of
!                                              ! current processor phys. domain
!                                              ! on model domain on all
!                                              ! processors
INTEGER :: IIE_ll, IJE_ll                      ! NE point coordinates of
!                                              ! current processor phys. domain
!                                              ! on model domain on all
!                                              ! processors
INTEGER :: IIINF_MASK, IISUP_MASK              ! cart. mask local proc. limits
INTEGER :: IJINF_MASK, IJSUP_MASK              ! cart. mask local proc. limits
!
INTEGER :: JK                                  ! vertical loop counter
INTEGER :: IIB, IJB, IIE, IJE                  ! hor. indices
INTEGER :: IIU, IJU                            ! hor. indices
INTEGER :: IKU                                 ! ver. index
INTEGER :: IRR, IRRC, IRRR, IRRI, IRRS, IRRG   ! moist variables indices
INTEGER :: IMI                                 ! Current model index             
!
!-------------------------------------------------------------------------------
!
!*      1.   Does current time-step is a LES time-step?
!            -----------------------------------------
!
LLES_CALL= .FALSE.
!
IF (.NOT. LLES) RETURN
!
IF ( KTCOUNT>1 .AND. MOD (KTCOUNT-1,NLES_DTCOUNT)==0) LLES_CALL=.TRUE.
!
IF (.NOT. LLES_CALL) RETURN
!
NLES_TCOUNT = NLES_TCOUNT + 1
!
NLES_CURRENT_TCOUNT = NLES_TCOUNT
!
xles_dates(nles_tcount ) = tdtcur
call Datetime_distance( tdtseg, tdtcur, xles_times(nles_tcount ) )
!
!-------------------------------------------------------------------------------
!
CALL GET_OR_ll     ('B',IXOR_ll,IYOR_ll)
CALL GET_DIM_EXT_ll('B',IIU,IJU)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
!
IIB_ll=IXOR_ll+IIB-1
IJB_ll=IYOR_ll+IJB-1
IIE_ll=IXOR_ll+IIE-1
IJE_ll=IYOR_ll+IJE-1
!
IKU = SIZE(XTHT,3)
!
!-------------------------------------------------------------------------------
!
!*      2.   Definition of masks
!            -------------------
!
!*      2.1  Cartesian (sub-)domain (on local processor)
!            ----------------------
!
ALLOCATE(LLES_CURRENT_CART_MASK(IIU,IJU,NLES_K))
!
IMI = GET_CURRENT_MODEL_INDEX()
!
IIINF_MASK = MAX(IIB, NLESn_IINF(IMI)-(IIB_ll-1-JPHEXT))
IJINF_MASK = MAX(IJB, NLESn_JINF(IMI)-(IJB_ll-1-JPHEXT))
IISUP_MASK = MIN(IIE, NLESn_ISUP(IMI)-(IIB_ll-1-JPHEXT))
IJSUP_MASK = MIN(IJE, NLESn_JSUP(IMI)-(IJB_ll-1-JPHEXT))
!
LLES_CURRENT_CART_MASK(:,:,:) = .FALSE.
LLES_CURRENT_CART_MASK(IIINF_MASK:IISUP_MASK,IJINF_MASK:IJSUP_MASK,:) = .TRUE.
!
CLES_CURRENT_LBCX(:) = CLES_LBCX(:,IMI)
CLES_CURRENT_LBCY(:) = CLES_LBCY(:,IMI)
!
!
!*      2.2  Cloud masks
!            -----------
!
CALL LES_CLOUD_MASKS_n
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LES_MASKS_n   
