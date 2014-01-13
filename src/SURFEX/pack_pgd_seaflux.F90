!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PACK_PGD_SEAFLUX(HPROGRAM,PSEABATHY)
!     ##############################################################
!
!!**** *PACK_PGD_SEAFLUX* packs SEAFLUX physiographic fields from all surface points to SEAFLUX points
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!
!!    P. Le Moigne        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2007
!!    Lebeaupin-B C. 01/2008 : include bathymetry
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SEAFLUX_n,          ONLY : XSST, XSEABATHY
USE MODD_SEAFLUX_GRID_n,     ONLY : NDIM, CGRID, XGRID_PAR
!
USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
!
USE MODI_GET_LUOUT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),        INTENT(IN) :: HPROGRAM  ! Type of program
REAL,    DIMENSION(:),   INTENT(IN) :: PSEABATHY ! bathymetry
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: ILUOUT ! output listing logical unit
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PACK_PGD_SEAFLUX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.      Number of points and packing
!             ----------------------------
!
 CALL GET_TYPE_DIM_n('SEA   ',NDIM)
ALLOCATE(IMASK(NDIM))
ILU=0
 CALL GET_SURF_MASK_n('SEA   ',NDIM,IMASK,ILU,ILUOUT)
!
!
!-------------------------------------------------------------------------------
!
!*    2.      Packing of fields
!             -----------------
!
ALLOCATE(XSEABATHY(NDIM))
 CALL PACK_SAME_RANK(IMASK,PSEABATHY(:),XSEABATHY(:))
IF (LHOOK) CALL DR_HOOK('PACK_PGD_SEAFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_PGD_SEAFLUX
