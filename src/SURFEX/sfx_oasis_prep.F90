!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_PREP (I, UG, U, &
                           HPROGRAM)
!###################################################
!
!!****  *SFX_OASIS_PREP* - Prepare grid areas and mask file for SFX-OASIS coupling
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
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
!!      Original    10/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
!
!
!
USE MODN_SFX_OASIS
USE MODD_SFX_OASIS
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_GET_MESH_CORNER
USE MODI_UNPACK_SAME_RANK
USE MODI_SFX_OASIS_CHECK
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),        INTENT(IN) :: HPROGRAM    ! program calling surf. schemes
!
!
!*       0.2   Declarations of local parameter
!              -------------------------------
!
INTEGER,           PARAMETER  :: INC = 4    ! Number of grid-cell corners
!
 CHARACTER(LEN=4),  PARAMETER  :: YSFX_LAND = 'slan'
 CHARACTER(LEN=4),  PARAMETER  :: YSFX_QSB  = 'sdra'
 CHARACTER(LEN=4),  PARAMETER  :: YSFX_GW   = 'sgw '
 CHARACTER(LEN=4),  PARAMETER  :: YSFX_SEA  = 'ssea'
 CHARACTER(LEN=4),  PARAMETER  :: YSFX_LAKE = 'slak'
!
!*       0.3   Declarations of local variables
!              -------------------------------
!
REAL,    DIMENSION(U%NDIM_FULL)       :: ZGW        ! groundwater mask
REAL,    DIMENSION(U%NDIM_FULL)       :: ZMASK_LAND ! land-sea mask for rrm coupling
REAL,    DIMENSION(U%NDIM_FULL)       :: ZMASK_LAKE ! lake mask for ogcm coupling
REAL,    DIMENSION(U%NDIM_FULL)       :: ZMASK_SEA  ! sea-land mask for ogcm coupling
!
REAL,    DIMENSION(U%NDIM_FULL,1)     :: ZLON
REAL,    DIMENSION(U%NDIM_FULL,1)     :: ZLAT
REAL,    DIMENSION(U%NDIM_FULL,1)     :: ZAREA
INTEGER, DIMENSION(U%NDIM_FULL,1)     :: IMASK
!
REAL,    DIMENSION(U%NDIM_FULL,1,INC) :: ZCORNER_LON
REAL,    DIMENSION(U%NDIM_FULL,1,INC) :: ZCORNER_LAT
!
INTEGER, DIMENSION(2)          :: IVAR_SHAPE  ! indexes for the coupling field local dimension
!
INTEGER                        :: IPART_ID ! Local partition ID
INTEGER                        :: IERR     ! Error info
!
INTEGER                        :: ILUOUT, IFLAG
!
INTEGER                        :: JI, JC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_PREP',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
!
!*       0.     Initialize :
!               ------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL SFX_OASIS_CHECK(I, U, &
                     ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*       2.     Get grid definition :
!               ---------------------
!
 CALL GET_MESH_CORNER(UG, &
                     ILUOUT,ZCORNER_LAT(:,1,:),ZCORNER_LON(:,1,:))
!
ZLON(:,1)=UG%XLON(:)
ZLAT(:,1)=UG%XLAT(:)
!
IF(LCPL_GW.AND.I%LGW)THEN
  CALL UNPACK_SAME_RANK(U%NR_NATURE(:),I%XGW(:),ZGW(:))
  WHERE(ZGW(:)==XUNDEF)
        ZGW(:)=0.0
  ELSEWHERE(ZGW(:)>0.0)
        ZGW(:)=1.0
  ENDWHERE
ELSE
  ZGW(:) = 0.0
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     Comput masks :
!               --------------
!
ZMASK_LAND(:) = U%XNATURE(:)+U%XTOWN(:)
ZMASK_SEA (:) = U%XSEA   (:)
IF(U%CWATER=='FLAKE ')THEN
  ZMASK_LAKE(:) = U%XWATER (:)
ELSE
  ZMASK_LAKE(:) = XUNDEF
ENDIF
IF(LCPL_SEA.AND.LWATER)THEN
  ZMASK_SEA (:) = U%XSEA (:)+U%XWATER(:)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5.     Write grid definition :
!               -----------------------
!
!
!
 CALL OASIS_START_GRIDS_WRITING(IFLAG)
!
!*       1.1    Grid definition for Land surface :
!               ----------------------------------
!
IF(LCPL_LAND)THEN  
!
  ZAREA(:,1) = UG%XMESH_SIZE(:) * ZMASK_LAND(:)
  !0 = not masked ; 1 = masked
  WHERE(ZAREA(:,1)>0.0)
        IMASK(:,1) = 0
  ELSEWHERE
        IMASK(:,1) = 1
  ENDWHERE
  CALL OASIS_WRITE_GRID  (YSFX_LAND,U%NDIM_FULL,1,ZLON(:,:),ZLAT(:,:))  
  CALL OASIS_WRITE_CORNER(YSFX_LAND,U%NDIM_FULL,1,INC,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YSFX_LAND,U%NDIM_FULL,1,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YSFX_LAND,U%NDIM_FULL,1,IMASK(:,:))
!
  ZAREA(:,1) = UG%XMESH_SIZE(:) * ZMASK_LAND(:) * (1.0-ZGW(:))
  !0 = not masked ; 1 = masked
  WHERE(ZAREA(:,1)>0.0)
        IMASK(:,1) = 0
  ELSEWHERE
        IMASK(:,1) = 1
  ENDWHERE
  CALL OASIS_WRITE_GRID  (YSFX_QSB,U%NDIM_FULL,1,ZLON(:,:),ZLAT(:,:))  
  CALL OASIS_WRITE_CORNER(YSFX_QSB,U%NDIM_FULL,1,INC,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YSFX_QSB,U%NDIM_FULL,1,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YSFX_QSB,U%NDIM_FULL,1,IMASK(:,:))
!
ENDIF
!
! groundwater surface coupling case
!
IF(LCPL_GW)THEN       
  ZAREA(:,1) = UG%XMESH_SIZE(:) * ZGW(:)
  !0 = not masked ; 1 = masked
  WHERE(ZAREA(:,1)>0.0)
        IMASK(:,1) = 0
  ELSEWHERE
        IMASK(:,1) = 1
  ENDWHERE
  CALL OASIS_WRITE_GRID  (YSFX_GW,U%NDIM_FULL,1,ZLON(:,:),ZLAT(:,:))  
  CALL OASIS_WRITE_CORNER(YSFX_GW,U%NDIM_FULL,1,INC,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YSFX_GW,U%NDIM_FULL,1,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YSFX_GW,U%NDIM_FULL,1,IMASK(:,:))
ENDIF
!
!*       1.2    Grid definition for lake surface :
!               ----------------------------------
!
IF(LCPL_LAKE)THEN
  ZAREA(:,1) = UG%XMESH_SIZE(:) * ZMASK_LAKE(:)
  !0 = not masked ; 1 = masked
  WHERE(ZAREA(:,1)>0.0)
        IMASK(:,1) = 0
  ELSEWHERE
        IMASK(:,1) = 1
  ENDWHERE
  CALL OASIS_WRITE_GRID  (YSFX_LAKE,U%NDIM_FULL,1,ZLON(:,:),ZLAT(:,:))  
  CALL OASIS_WRITE_CORNER(YSFX_LAKE,U%NDIM_FULL,1,INC,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YSFX_LAKE,U%NDIM_FULL,1,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YSFX_LAKE,U%NDIM_FULL,1,IMASK(:,:))
ENDIF
!
!*       1.3    Grid definition for sea/water :
!               -------------------------------
!
IF(LCPL_SEA)THEN     
  ZAREA(:,1) = UG%XMESH_SIZE(:) * ZMASK_SEA(:)
  !0 = not masked ; 1 = masked
  WHERE(ZAREA(:,1)>0.0)
        IMASK(:,1) = 0
  ELSEWHERE
        IMASK(:,1) = 1
  ENDWHERE
  CALL OASIS_WRITE_GRID  (YSFX_SEA,U%NDIM_FULL,1,ZLON(:,:),ZLAT(:,:))  
  CALL OASIS_WRITE_CORNER(YSFX_SEA,U%NDIM_FULL,1,INC,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YSFX_SEA,U%NDIM_FULL,1,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YSFX_SEA,U%NDIM_FULL,1,IMASK(:,:))
ENDIF
!
 CALL OASIS_TERMINATE_GRIDS_WRITING()
!
 CALL OASIS_ENDDEF(IERR)
!
IF(IERR/=OASIS_OK)THEN
   WRITE(ILUOUT,*)'SFX_OASIS_PREP: OASIS enddef problem, err = ',IERR
   CALL ABOR1_SFX('SFX_OASIS_PREP: OASIS enddef problem')
ENDIF
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_PREP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_PREP
