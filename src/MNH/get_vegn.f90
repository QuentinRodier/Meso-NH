!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ##########################
      MODULE MODI_GET_VEG_n
!     ##########################
INTERFACE
      SUBROUTINE GET_VEG_n(HPROGRAM, KI, PLAI, PVH)
!
CHARACTER(LEN=6),   INTENT(IN)            :: HPROGRAM    
INTEGER,            INTENT(IN)            :: KI         ! number of points

!    
REAL, DIMENSION(KI), INTENT(OUT) :: PVH     
REAL, DIMENSION(KI), INTENT(OUT) :: PLAI   
!
END SUBROUTINE GET_VEG_n
!
END INTERFACE
END MODULE MODI_GET_VEG_n
!     #######################################################################
      SUBROUTINE GET_VEG_n(HPROGRAM, KI, PLAI, PVH)
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
USE MODD_SURF_PAR,         ONLY : XUNDEF
USE MODD_DATA_COVER_PAR
USE MODD_SURF_ATM_n,     ONLY : CSEA,      CWATER,      CTOWN,      CNATURE,      &
                                XSEA,      XWATER,      XTOWN,      XNATURE,      &
                                NSIZE_SEA, NSIZE_WATER, NSIZE_TOWN, NSIZE_NATURE, &
                                NR_SEA,    NR_WATER,    NR_TOWN,    NR_NATURE,    &
                                NDIM_FULL, NSIZE_FULL,                            &
                                NDIM_NATURE, NDIM_SEA, NDIM_WATER, NDIM_TOWN
USE MODD_ISBA_n

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
REAL, DIMENSION(NSIZE_FULL)      :: ZH_TREE_FULL, ZLAI_FULL
REAL, DIMENSION(NSIZE_NATURE)    :: ZH_TREE, ZLAI,ZWORK
INTEGER:: IPATCH_TREE, IPATCH_EVER, IPATCH_CONI
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


ZH_TREE_FULL(:)=0
ZLAI_FULL(:)=XUNDEF

IPATCH_TREE=VEGTYPE_TO_PATCH(NVT_TREE, NPATCH)
IPATCH_EVER=VEGTYPE_TO_PATCH(NVT_EVER, NPATCH)
IPATCH_CONI=VEGTYPE_TO_PATCH(NVT_CONI, NPATCH)


ZWORK(:)=(XVEGTYPE(:,NVT_CONI)+&
          XVEGTYPE(:,NVT_EVER)+&
          XVEGTYPE(:,NVT_TREE))

DO JJ=1,NSIZE_NATURE
  IF (ZWORK(JJ)==0) THEN
          ZH_TREE(JJ) = 0.
          ZLAI(JJ) = 0.
  ELSE
         ZH_TREE(JJ) =(((XH_TREE(JJ,IPATCH_TREE)*XVEGTYPE(JJ,NVT_TREE))+&
                      (XH_TREE(JJ,IPATCH_EVER)*XVEGTYPE(JJ,NVT_EVER))+&
                      (XH_TREE(JJ,IPATCH_CONI)*XVEGTYPE(JJ,NVT_CONI)))/&
                      (ZWORK(JJ)))

         ZLAI(JJ)  = (((XLAI(JJ,IPATCH_EVER)*XVEGTYPE(JJ,NVT_EVER))+&
                       (XLAI(JJ,IPATCH_CONI)*XVEGTYPE(JJ,NVT_CONI))+&
                       (XLAI(JJ,IPATCH_TREE)*XVEGTYPE(JJ,NVT_TREE))))
    
          ZH_TREE_FULL(NR_NATURE(JJ)) = ZH_TREE(JJ)
          ZLAI_FULL(NR_NATURE(JJ)) = ZLAI(JJ)
  END IF
END DO

ZLAI_FULL(:)=XNATURE(:)*ZLAI_FULL(:)


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
