!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #################
      MODULE MODN_LBC_n
!     #################
!
!!****  *MODN_LBC$n* - declaration of namelist NAM_LBCn
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify the namelist  NAM_LBCn
!     which concerns the lateral boundary conditions of one nested model.         
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_LBC$n : contains declaration of lateral boundary conditions
!!        
!!        CLBCX   : X-direction LBC type at left(1) and right(2) boundaries 
!!        CLBCY   : Y-direction LBC type at left(1) and right(2) boundaries                              
!!        NLBLX   : X-direction characteristic large scale length at left(1) and 
!!                  right(2) boundaries ( number of delta x)     
!!        NLBLY   : Y-direction characteristic large scale length at left(1) and 
!!                  right(2) boundaries ( number of delta y)
!!        XCPHASE : prescribed value of the phase velocity if constant
!!        XCPHASE_PBL : prescribed value of the phase velocity in the PBL if constant
!!
!!    REFERENCE
!!    ---------
!!      Book2 of Meso-NH documentation (module MODD_LBCn)
!!          
!!    AUTHOR
!!    ------
!!	V. Ducrocq and J-P. Lafore    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    13/09/94                      
!!      Modification : 26/06/13 (C.Lac) Introduction of CPHASE_PBL
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_LBC_n, ONLY: &
         CLBCX_n => CLBCX, &
         CLBCY_n => CLBCY, &
         NLBLX_n => NLBLX, &
         NLBLY_n => NLBLY, &
         XCPHASE_n => XCPHASE, &
         XCPHASE_PBL_n => XCPHASE_PBL
!
IMPLICIT NONE
!
CHARACTER(LEN=4),SAVE, DIMENSION(2)  :: CLBCX
CHARACTER(LEN=4),SAVE, DIMENSION(2)  :: CLBCY
INTEGER,SAVE, DIMENSION(2)  :: NLBLX
INTEGER,SAVE, DIMENSION(2)  :: NLBLY
REAL,SAVE  :: XCPHASE
REAL,SAVE  :: XCPHASE_PBL
!
NAMELIST/NAM_LBCn/CLBCX,CLBCY,NLBLX,NLBLY,XCPHASE,XCPHASE_PBL
!
CONTAINS
!
SUBROUTINE INIT_NAM_LBCn
  CLBCX = CLBCX_n
  CLBCY = CLBCY_n
  NLBLX = NLBLX_n
  NLBLY = NLBLY_n
  XCPHASE = XCPHASE_n
  XCPHASE_PBL = XCPHASE_PBL_n
END SUBROUTINE INIT_NAM_LBCn

SUBROUTINE UPDATE_NAM_LBCn
  CLBCX_n = CLBCX
  CLBCY_n = CLBCY
  NLBLX_n = NLBLX
  NLBLY_n = NLBLY
  XCPHASE_n = XCPHASE
  XCPHASE_PBL_n = XCPHASE_PBL
END SUBROUTINE UPDATE_NAM_LBCn

END MODULE MODN_LBC_n
