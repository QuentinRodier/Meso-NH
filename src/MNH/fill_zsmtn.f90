!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:17
!-----------------------------------------------------------------
!     ######################
      MODULE MODI_FILL_ZSMTn
!     ######################
!
INTERFACE 
!
      SUBROUTINE FILL_ZSMT_n(HFIELD,PFIELD,KSON)
!
CHARACTER(LEN=6),         INTENT(IN) :: HFIELD ! name of the field to nest
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PFIELD
INTEGER,                  INTENT(IN) :: KSON   ! son model index
!
END SUBROUTINE FILL_ZSMT_n
!
END INTERFACE
!
END MODULE MODI_FILL_ZSMTn
!
!
!
!     ##########################################
      SUBROUTINE FILL_ZSMT_n(HFIELD,PFIELD,KSON)
!     ##########################################
!
!!****  *FILL_ZSMT_n* - fill the working array for nesting of pgd files
!!                          with KSON model index
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        12/01/05
!!      Modification    20/05/06 Remove Clark and Farley interpolation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_GRID_n,  ONLY : XZSMT
USE MODD_LUNIT_n, ONLY : CLUOUT
USE MODD_LBC_n,   ONLY : CLBCX,CLBCY
USE MODD_NESTING
USE MODD_PARAMETERS
!
USE MODI_INI_BIKHARDT_n
USE MODI_SPAWN_ZS
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
CHARACTER(LEN=6),     INTENT(IN)     :: HFIELD ! name of the field to fill
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PFIELD
INTEGER,              INTENT(IN) :: KSON   ! son model index
!
!*       0.2   declarations of local variables
!-------------------------------------------------------------------------------
INTEGER :: IMI ! current model index (DAD index)
!
! Dummy pointers needed to correct an ifort Bug
CHARACTER(LEN=4), DIMENSION(:), POINTER :: DPTR_CLBCX,DPTR_CLBCY
REAL, DIMENSION(:,:),  POINTER          :: DPTR_XZSMT
!
!*       1.    initializations
!              ---------------
!
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(KSON)
!
CALL INI_BIKHARDT_n(NDXRATIO_ALL(KSON),NDYRATIO_ALL(KSON),2)
!
!-------------------------------------------------------------------------------
!
!*       2.    interpolation of dad field
!              --------------------------
!
DPTR_CLBCX=>CLBCX
DPTR_CLBCY=>CLBCY
DPTR_XZSMT=>XZSMT
CALL SPAWN_ZS(NXOR_ALL(KSON),NXEND_ALL(KSON),NYOR_ALL(KSON),NYEND_ALL(KSON), &
              NDXRATIO_ALL(KSON),NDYRATIO_ALL(KSON),DPTR_CLBCX,DPTR_CLBCY,         &
              CLUOUT,PFIELD,DPTR_XZSMT,HFIELD                             )
!-------------------------------------------------------------------------------
!
CALL GOTO_MODEL(IMI)
!
END SUBROUTINE FILL_ZSMT_n
