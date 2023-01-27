!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!#############################
MODULE MODI_INI_LES_CART_MASKn
!#############################
!
!
!
INTERFACE
!
      SUBROUTINE  INI_LES_CART_MASK_n(KMI, PXHAT_ll,PYHAT_ll,                &
                                     KLES_IINF,KLES_JINF,KLES_ISUP,KLES_JSUP)
!

INTEGER,            INTENT(IN)  :: KMI      ! son model index
REAL, DIMENSION(:), INTENT(IN)  :: PXHAT_ll ! son model X coordinate
REAL, DIMENSION(:), INTENT(IN)  :: PYHAT_ll ! son model X coordinate
INTEGER,            INTENT(OUT) :: KLES_IINF ! limits of the cartesian
INTEGER,            INTENT(OUT) :: KLES_JINF ! mask in son model
INTEGER,            INTENT(OUT) :: KLES_ISUP ! physical domain
INTEGER,            INTENT(OUT) :: KLES_JSUP !
!
END SUBROUTINE INI_LES_CART_MASK_n
!
END INTERFACE
!
END MODULE MODI_INI_LES_CART_MASKn
!
!     ########################################################################
      SUBROUTINE  INI_LES_CART_MASK_n(KMI, PXHAT_ll,PYHAT_ll,                &
                                      KLES_IINF,KLES_JINF,KLES_ISUP,KLES_JSUP)
!     ########################################################################
!
!
!!****  *INI_LES_CART_MASK_n* initializes the LES cartesian mask
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
!!      Modification     01/02/01 (D.Gazen) add module MODD_NSV for NSV variable
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!  P. Wautelet 19/10/2017: IO: removed extern_userio.f90
!  P. Wautelet 30/03/2021: budgets: LES cartesian subdomain limits are defined in the physical domain
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODE_ll
USE MODE_GATHER_ll 
USE MODE_MODELN_HANDLER
!
USE MODD_CONF
USE MODD_PARAMETERS
!
USE MODD_GRID_n
USE MODD_LES
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
INTEGER,            INTENT(IN)  :: KMI      ! son model index
REAL, DIMENSION(:), INTENT(IN)  :: PXHAT_ll ! son model X coordinate
REAL, DIMENSION(:), INTENT(IN)  :: PYHAT_ll ! son model X coordinate
INTEGER,            INTENT(OUT) :: KLES_IINF ! limits of the cartesian
INTEGER,            INTENT(OUT) :: KLES_JINF ! mask in son model
INTEGER,            INTENT(OUT) :: KLES_ISUP ! physical domain
INTEGER,            INTENT(OUT) :: KLES_JSUP !
!
!
!       0.2  declaration of local variables
!
!
INTEGER :: IIB_ll    ! son domain index
INTEGER :: IIE_ll    ! son domain index
INTEGER :: IJB_ll    ! son domain index
INTEGER :: IJE_ll    ! son domain index
!
INTEGER :: JI, JJ    ! loop counters
!
REAL    :: ZX, ZY    ! coordinates of mask boundaries
!
INTEGER :: IINFO_ll
!
REAL, DIMENSION(:), POINTER :: ZXHAT_ll ! father model coordinates
REAL, DIMENSION(:), POINTER :: ZYHAT_ll !
INTEGER :: IMI
!
IMI = GET_CURRENT_MODEL_INDEX()
!-------------------------------------------------------------------------------
!
!*      1.   Coordinate of father model
!            --------------------------
!
CALL GO_TOMODEL_ll(IMI, IINFO_ll)
ZXHAT_ll => XXHAT_ll
ZYHAT_ll => XYHAT_ll
!
CALL GO_TOMODEL_ll(KMI, IINFO_ll)
!
!-------------------------------------------------------------------------------
!
IIB_ll=1+JPHEXT
IIE_ll=SIZE(PXHAT_ll)-JPHEXT
IJB_ll=1+JPHEXT
IJE_ll=SIZE(PYHAT_ll)-JPHEXT
!
!-------------------------------------------------------------------------------
!
!*      2.   X limit of LES cartesian mask
!            -----------------------------
!
!* left limit
!
ZX = ZXHAT_ll(NLESn_IINF(IMI) + JPHEXT)
IF (PXHAT_ll(IIB_ll)>ZX) THEN
  KLES_IINF=1 ! father mask starts left of son domain
ELSE IF (PXHAT_ll(IIE_ll+1)<ZX) THEN
  CALL MASK_OVER_ALL_DOMAIN
  RETURN
ELSE
  DO JI=IIB_ll,IIE_ll
    IF (ABS(PXHAT_ll(JI)-ZX) <= (PXHAT_ll(JI+1)-PXHAT_ll(JI))/2. ) THEN
      KLES_IINF=JI-JPHEXT
    END IF
  END DO
END IF
!
!* right limit
!
ZX = ZXHAT_ll(NLESn_ISUP(IMI) + JPHEXT + 1)
IF (PXHAT_ll(IIB_ll)>ZX) THEN
  CALL MASK_OVER_ALL_DOMAIN
  RETURN
ELSE IF (PXHAT_ll(IIE_ll+1)<ZX) THEN
  KLES_ISUP=IIE_ll-JPHEXT ! father mask ends right of son domain
ELSE
  DO JI=IIB_ll,IIE_ll
    IF (ABS(PXHAT_ll(JI+1)-ZX) <= (PXHAT_ll(JI+1)-PXHAT_ll(JI))/2. ) THEN
      KLES_ISUP=JI-JPHEXT
    END IF
  END DO
END IF
!
!-------------------------------------------------------------------------------
!
!*      3.   Y limit of LES cartesian mask
!            -----------------------------
!
!* bottom limit
!
ZY = ZYHAT_ll(NLESn_JINF(IMI) + JPHEXT)
IF (PYHAT_ll(IJB_ll)>ZY) THEN
  KLES_JINF=1 ! father mask starts under the son domain
ELSE IF (PYHAT_ll(IJE_ll+1)<ZY) THEN
  CALL MASK_OVER_ALL_DOMAIN
  RETURN
ELSE
  DO JJ=IJB_ll,IJE_ll
    IF (ABS(PYHAT_ll(JJ)-ZY) <= (PYHAT_ll(JJ+1)-PYHAT_ll(JJ))/2. ) THEN
      KLES_JINF=JJ-JPHEXT
    END IF
  END DO
END IF
!
!* top limit
!
ZY = ZYHAT_ll(NLESn_JSUP(IMI) + JPHEXT + 1)
IF (PYHAT_ll(IJB_ll)>ZY) THEN
  CALL MASK_OVER_ALL_DOMAIN
  RETURN
ELSE IF (PYHAT_ll(IJE_ll+1)<ZY) THEN
  KLES_JSUP=IJE_ll-JPHEXT ! father mask ends over the son domain
ELSE
  DO JJ=IJB_ll,IJE_ll
    IF (ABS(PYHAT_ll(JJ+1)-ZY) <= (PYHAT_ll(JJ+1)-PYHAT_ll(JJ))/2. ) THEN
      KLES_JSUP=JJ-JPHEXT
    END IF
  END DO
END IF
!
!-------------------------------------------------------------------------------
!
  CONTAINS
!
  SUBROUTINE MASK_OVER_ALL_DOMAIN
    KLES_IINF=IIB_ll-JPHEXT ! father mask not in son domain, so all domain is taken
    KLES_ISUP=IIE_ll-JPHEXT
    KLES_JINF=IJB_ll-JPHEXT
    KLES_JSUP=IJE_ll-JPHEXT
  END SUBROUTINE MASK_OVER_ALL_DOMAIN
!
END SUBROUTINE INI_LES_CART_MASK_n   

