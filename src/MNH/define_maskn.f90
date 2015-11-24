!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
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
!!   J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
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
USE MODE_SPLITTING_ll, ONLY : SPLIT2
USE MODD_VAR_ll, ONLY : NPROC, IP, YSPLITTING, NMNH_COMM_WORLD
USE MODD_STRUCTURE_ll, ONLY : ZONE_ll
USE MODE_TOOLS_ll, ONLY : INTERSECTION
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
INTEGER     :: IXOR_F, IYOR_F    ! origin of local father subdomain (global coord)
INTEGER     :: IXEND_F, IYEND_F    ! end of local father subdomain (global coord)
INTEGER     :: IXOR_C, IYOR_C    ! origin of intersection between son model and local father subdomain (global coord)
INTEGER     :: IXEND_C, IYEND_C    ! end of intersection between son model and local father subdomain (global coord)
TYPE(ZONE_ll), DIMENSION(:), ALLOCATABLE :: TZSPLITTING
TYPE(ZONE_ll) :: TZCOARSESONGLB ! global son domain in father grid
TYPE(ZONE_ll), DIMENSION(:), ALLOCATABLE :: TZCOARSESONLCL ! intersection of global son domain and local father subdomain
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
IMI=GET_CURRENT_MODEL_INDEX()
!
ALLOCATE ( NNESTMASK (NIMAX+2*JPHEXT,NJMAX+2*JPHEXT,1+COUNT(NDAD(:)==IMI)))
ALLOCATE ( NSON      (                              1+COUNT(NDAD(:)==IMI)))
!
! get splitting of father model
ALLOCATE(TZSPLITTING(NPROC))
ALLOCATE(TZCOARSESONLCL(NPROC))
CALL SPLIT2 ( NIMAX_ll, NJMAX_ll, 1, NPROC, TZSPLITTING, YSPLITTING )
! get coords of local father subdomain
IXOR_F = TZSPLITTING(IP)%NXOR-JPHEXT
IYOR_F = TZSPLITTING(IP)%NYOR-JPHEXT
IXEND_F = TZSPLITTING(IP)%NXEND-JPHEXT
IYEND_F = TZSPLITTING(IP)%NYEND-JPHEXT
!
TZCOARSESONGLB%NZOR = TZSPLITTING(IP)%NZOR    ! there is no splitting in Z direction
TZCOARSESONGLB%NZEND = TZSPLITTING(IP)%NZEND  ! there is no splitting in Z direction
TZCOARSESONGLB%NUMBER = TZSPLITTING(IP)%NUMBER
!
NNESTMASK(:,:,:) = 0
NSON(1) = IMI
!
ISON=1
DO JLOOP=1,NMODEL
  IF (NDAD(JLOOP)/=IMI) CYCLE
  ISON=ISON+1
  NSON(ISON)=JLOOP
  !
  !JUAN A REVOIR TODO_JPHEXT !!!
  ! <<<<<<< define_maskn.f90
  ! init global son zone in father grid coords
  !
  ! TZCOARSESONGLB%NXOR = NXOR_ALL(JLOOP)+1
  ! TZCOARSESONGLB%NYOR = NYOR_ALL(JLOOP)+1
  ! TZCOARSESONGLB%NXEND = NXEND_ALL(JLOOP)-1
  ! TZCOARSESONGLB%NYEND = NYEND_ALL(JLOOP)-1
  TZCOARSESONGLB%NXOR = NXOR_ALL(JLOOP)+JPHEXT
  TZCOARSESONGLB%NYOR = NYOR_ALL(JLOOP)+JPHEXT
  TZCOARSESONGLB%NXEND = NXEND_ALL(JLOOP)-JPHEXT
  TZCOARSESONGLB%NYEND = NYEND_ALL(JLOOP)-JPHEXT
  ! get the intersection  with local father subdomain -> TZCOARSESONLCL
  CALL INTERSECTION( TZSPLITTING, NPROC, TZCOARSESONGLB, TZCOARSESONLCL)
  IXOR_C = TZCOARSESONLCL(IP)%NXOR
  IXEND_C = TZCOARSESONLCL(IP)%NXEND
  IYOR_C = TZCOARSESONLCL(IP)%NYOR
  IYEND_C = TZCOARSESONLCL(IP)%NYEND
  IF ( IXEND_C/=0 .AND. IYEND_C/=0 ) THEN
    ! the intersection is non empty
    NNESTMASK( (IXOR_C-IXOR_F+1):(IXEND_C-IXOR_F+1), (IYOR_C-IYOR_F+1):(IYEND_C-IYOR_F+1), ISON) = 1
  ENDIF
!  NNESTMASK(NXOR_ALL(JLOOP)+1:NXEND_ALL(JLOOP)-1,     &
!            NYOR_ALL(JLOOP)+1:NYEND_ALL(JLOOP)-1, ISON) = 1
! =======
!  NNESTMASK(NXOR_ALL(JLOOP)+JPHEXT:NXEND_ALL(JLOOP)-JPHEXT,     &
!            NYOR_ALL(JLOOP)+JPHEXT:NYEND_ALL(JLOOP)-JPHEXT, ISON) = 1
! >>>>>>> 1.2.4.2.18.2.2.1
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
