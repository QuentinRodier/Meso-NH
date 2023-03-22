!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_FX_SURF_ATM_n (DTCO, DGO, D, U, UG, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MIP_FX_SURF_ATM_n* - writes fixed surface diagnostics
!!
!!    PURPOSE
!!    -------
!!
!!    Global fixed fields
!!    
!!
!!**  METHOD
!!    ------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2016
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
USE MODD_DIAG_n,          ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURF_ATM_n,      ONLY : SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------

!
TYPE(DATA_COVER_t),    INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t),  INTENT(INOUT) :: DGO
TYPE(DIAG_t),          INTENT(INOUT) :: D
TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
CHARACTER(LEN=6),      INTENT(IN)    :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_FX_SURF_ATM_N',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Global fixed fields
!-------------------------------------------------------------------------------
!
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','WRITE','SURF_ATM.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Surface Altitude
!-------------------------------------------------------------------------------
!
YRECFM='orog'
YCOMMENT='surface altitude (m)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XOROG(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * area of grid cell
!-------------------------------------------------------------------------------
!
YRECFM='areacella'
YCOMMENT='area of grid cell (m2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,UG%G%XMESH_SIZE(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * fraction of the grid cell occupied by land
!-------------------------------------------------------------------------------
!
YRECFM='sftlf'
YCOMMENT='fraction of the grid cell occupied by land (-)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,U%XNATURE(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * fraction of the grid cell occupied by lake
!-------------------------------------------------------------------------------
!
YRECFM='sftlaf'
YCOMMENT='fraction of the grid cell occupied by lake (-)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,U%XWATER(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * fraction of the grid cell occupied by sea
!-------------------------------------------------------------------------------
!
YRECFM='sftsf'
YCOMMENT='fraction of the grid cell occupied by sea (-)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,U%XSEA(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * fraction of the grid cell occupied by town
!-------------------------------------------------------------------------------
!
YRECFM='sfttf'
YCOMMENT='fraction of the grid cell occupied by town (-)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,U%XTOWN(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_FX_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_MIP_FX_SURF_ATM_n
