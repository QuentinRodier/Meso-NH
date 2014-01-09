!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_nest_pgd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ########################
      MODULE MODI_DEFINE_MASK_n
!     ########################
!
!
INTERFACE 
!
      SUBROUTINE DEFINE_MASK_n
!
END SUBROUTINE DEFINE_MASK_n
!
END INTERFACE
!
END MODULE MODI_DEFINE_MASK_n
!
!
!     #########################
      SUBROUTINE DEFINE_MASK_n
!     #########################
!
!!****  *DEFINE_MASK_n* - allocates arrays for nesting of pgds
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
!!      Original        26/09/96
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_DIM_n
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_LUNIT
USE MODD_NESTING
USE MODD_NEST_PGD_n
!
USE MODE_FM
USE MODE_IO_ll
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
!
!*       0.2   declarations of local variables
!
INTEGER :: ILUOUT0
INTEGER :: IRESP
INTEGER :: ISON
INTEGER :: JLOOP
INTEGER :: IMI
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
IMI=GET_CURRENT_MODEL_INDEX()
!
ALLOCATE ( NNESTMASK (NIMAX+2*JPHEXT,NJMAX+2*JPHEXT,1+COUNT(NDAD(:)==IMI)))
ALLOCATE ( NSON      (                              1+COUNT(NDAD(:)==IMI)))
!
NNESTMASK(:,:,:) = 0
NSON(1) = IMI
!
ISON=1
DO JLOOP=1,NMODEL
  IF (NDAD(JLOOP)/=IMI) CYCLE
  ISON=ISON+1
  NSON(ISON)=JLOOP
  NNESTMASK(NXOR_ALL(JLOOP)+1:NXEND_ALL(JLOOP)-1,     &
            NYOR_ALL(JLOOP)+1:NYEND_ALL(JLOOP)-1, ISON) = 1
END DO
!
IF (ANY (SUM(NNESTMASK(:,:,:),DIM=3)>1) ) THEN
  WRITE(ILUOUT0,*) 'Two nested models in the same father are overlapping each other'
!callabortstop
  CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF
!
NNESTMASK(:,:,1) = 1.-SUM(NNESTMASK(:,:,:),DIM=3)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFINE_MASK_n
