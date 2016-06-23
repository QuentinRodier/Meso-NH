!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################################################################
      SUBROUTINE GET_VEG_n(HPROGRAM, KI, U, I, PLAI, PVH)
!     #######################################################################
!
!!****  *GET_VEG_n* - gets some veg fields on atmospheric grid
!!
!!    PURPOSE
!!    -------
!!
!!    This program returns some veg variables needed by the atmosphere
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
!!	P. Aumond	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURF_PAR,         ONLY : XUNDEF
USE MODD_DATA_COVER_PAR

USE MODI_GET_LUOUT
USE MODI_VEGTYPE_TO_PATCH
!                                
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),   INTENT(IN)   :: HPROGRAM    
INTEGER,            INTENT(IN)   :: KI         ! number of points
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_t), INTENT(INOUT) :: I
!
REAL, DIMENSION(KI), INTENT(OUT) :: PVH    ! Tree height 
REAL, DIMENSION(KI), INTENT(OUT) :: PLAI   
!-------------------------------------------------------------------------------
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!  Arrays defined for each tile
!  
!
INTEGER                               :: JI,JJ           ! loop index over tiles
INTEGER                               :: ILUOUT       ! unit numberi
REAL, DIMENSION(U%NSIZE_FULL)      :: ZH_TREE_FULL, ZLAI_FULL
REAL, DIMENSION(U%NSIZE_NATURE)    :: ZH_TREE, ZLAI,ZWORK
INTEGER:: IPATCH_TRBE, IPATCH_TRBD, IPATCH_TEBE, IPATCH_TEBD, IPATCH_TENE, &
          IPATCH_BOBD, IPATCH_BONE, IPATCH_BOND
! 
!-------------------------------------------------------------------------------
!
!*   0. Logical unit for writing out
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*       1. Passage dur le masque global
!              -------------------------------


ZH_TREE_FULL(:) = 0.
ZLAI_FULL   (:) = XUNDEF

IPATCH_TRBE = VEGTYPE_TO_PATCH(NVT_TRBE, I%NPATCH)
IPATCH_TRBD = VEGTYPE_TO_PATCH(NVT_TRBD, I%NPATCH)
IPATCH_TEBE = VEGTYPE_TO_PATCH(NVT_TEBE, I%NPATCH)
IPATCH_TEBD = VEGTYPE_TO_PATCH(NVT_TEBD, I%NPATCH)
IPATCH_TENE = VEGTYPE_TO_PATCH(NVT_TENE, I%NPATCH)
IPATCH_BOBD = VEGTYPE_TO_PATCH(NVT_BOBD, I%NPATCH)
IPATCH_BONE = VEGTYPE_TO_PATCH(NVT_BONE, I%NPATCH)
IPATCH_BOND = VEGTYPE_TO_PATCH(NVT_BOND, I%NPATCH)


ZWORK(:) = I%XVEGTYPE(:,NVT_TRBE) + I%XVEGTYPE(:,NVT_TRBD) + I%XVEGTYPE(:,NVT_TEBE) + &
           I%XVEGTYPE(:,NVT_TEBD) + I%XVEGTYPE(:,NVT_TENE) + I%XVEGTYPE(:,NVT_BOBD) + &
           I%XVEGTYPE(:,NVT_BONE) + I%XVEGTYPE(:,NVT_BOND)

DO JJ=1,U%NSIZE_NATURE
  !
  IF (ZWORK(JJ)==0) THEN
    !
    ZH_TREE(JJ) = 0.
    ZLAI(JJ) = 0.
    !
  ELSE
    !
    ZH_TREE(JJ) = ( (I%XH_TREE(JJ,IPATCH_TRBE) * I%XVEGTYPE(JJ,NVT_TRBE) ) + &
                    (I%XH_TREE(JJ,IPATCH_TRBD) * I%XVEGTYPE(JJ,NVT_TRBD) ) + &
                    (I%XH_TREE(JJ,IPATCH_TEBE) * I%XVEGTYPE(JJ,NVT_TEBE) ) + &
                    (I%XH_TREE(JJ,IPATCH_TEBD) * I%XVEGTYPE(JJ,NVT_TEBD) ) + &
                    (I%XH_TREE(JJ,IPATCH_TENE) * I%XVEGTYPE(JJ,NVT_TENE) ) + &
                    (I%XH_TREE(JJ,IPATCH_BOBD) * I%XVEGTYPE(JJ,NVT_BOBD) ) + &
                    (I%XH_TREE(JJ,IPATCH_BONE) * I%XVEGTYPE(JJ,NVT_BONE) ) + &
                    (I%XH_TREE(JJ,IPATCH_BOND) * I%XVEGTYPE(JJ,NVT_BOND) ) &
                     ) / ZWORK(JJ) 

    ZLAI(JJ)  = ( I%XLAI(JJ,IPATCH_TRBE) * I%XVEGTYPE(JJ,NVT_TRBE) ) + &
                ( I%XLAI(JJ,IPATCH_TRBD) * I%XVEGTYPE(JJ,NVT_TRBD) ) + &
                ( I%XLAI(JJ,IPATCH_TEBE) * I%XVEGTYPE(JJ,NVT_TEBE) ) + &
                ( I%XLAI(JJ,IPATCH_TEBD) * I%XVEGTYPE(JJ,NVT_TEBD) ) + &
                ( I%XLAI(JJ,IPATCH_TENE) * I%XVEGTYPE(JJ,NVT_TENE) ) + &
                ( I%XLAI(JJ,IPATCH_BOBD) * I%XVEGTYPE(JJ,NVT_BOBD) ) + &
                ( I%XLAI(JJ,IPATCH_BONE) * I%XVEGTYPE(JJ,NVT_BONE) )+ &
                ( I%XLAI(JJ,IPATCH_BOND) * I%XVEGTYPE(JJ,NVT_BOND) )
    
    ZH_TREE_FULL(U%NR_NATURE(JJ)) = ZH_TREE(JJ)
    ZLAI_FULL   (U%NR_NATURE(JJ)) = ZLAI(JJ)
    !
  END IF
  !
END DO
!
ZLAI_FULL(:) = U%XNATURE(:) * ZLAI_FULL(:)
!
!*       2. Envoi les variables vers mesonH 
!             ------------------------------

IF ( SIZE(PVH) /= SIZE(ZH_TREE_FULL) ) THEN
  WRITE(ILUOUT,*) 'try to get VH field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PVH) :', SIZE(PVH)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XVH) :', SIZE(ZH_TREE_FULL)
  CALL ABOR1_SFX('GET_VHN: VH SIZE NOT CORRECT')
ELSE
  PVH = ZH_TREE_FULL
END IF
!
!==============================================================================
!
!-------------------------------------------------------------------------------
!
IF ( SIZE(PLAI) /= SIZE(ZLAI_FULL) ) THEN
  WRITE(ILUOUT,*) 'try to get LAI field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PLAI) :', SIZE(PLAI)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XLAI) :', SIZE(ZLAI_FULL)
  CALL ABOR1_SFX('GET_LAIN: LAI SIZE NOT CORRECT')
ELSE
  PLAI = ZLAI_FULL
END IF
!
!==============================================================================
!
!-------------------------------------------------------------------------------
!
!==============================================================================
!
END SUBROUTINE GET_VEG_n
