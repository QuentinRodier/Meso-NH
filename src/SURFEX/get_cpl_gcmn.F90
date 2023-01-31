!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_CPL_GCM_n (U, USS, IM, &
                                HPROGRAM,KI,PRAIN,PSNOW,PZ0,PZ0H,PQSURF, &
                                POROMEA,POROSTD,POROANI,PORODIR,PSAND,PCLAY)
!     ######################################################
!
!!****  *GET_CPL_GCM_n* - routine to get physical fields   
!!                      for initialise ARPEGE/ALADIN run
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
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
!!      Original    04/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_SURF_ATM,   ONLY : LCPL_GCM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_CSTS, ONLY : XG, XPI
!
USE MODI_GET_LUOUT
!
USE MODI_GET_1D_MASK
USE MODI_GET_FRAC_n
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(SSO_t),        INTENT(INOUT) :: USS
!
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
!
CHARACTER(LEN=6),        INTENT(IN)  :: HPROGRAM
INTEGER,                 INTENT(IN)  :: KI       ! number of points
!
REAL, DIMENSION(KI),     INTENT(OUT) :: PRAIN    ! total rainfall rate (kg/m2/s)
REAL, DIMENSION(KI),     INTENT(OUT) :: PSNOW    ! total snowfall rate (kg/m2/s)
REAL, DIMENSION(KI),     INTENT(OUT) :: PZ0      ! roughness length for momentum (m)
REAL, DIMENSION(KI),     INTENT(OUT) :: PZ0H     ! roughness length for heat (m)
REAL, DIMENSION(KI),     INTENT(OUT) :: PQSURF   ! specific humidity at surface (kg/kg)
REAL, DIMENSION(KI),     INTENT(OUT) :: POROMEA  ! averaged orography (m2/s2)
REAL, DIMENSION(KI),     INTENT(OUT) :: POROSTD  ! subgrid-scale orography std deviation (m2/s2)
REAL, DIMENSION(KI),     INTENT(OUT) :: POROANI  ! subgrid-scale orography anisotropy (-)
REAL, DIMENSION(KI),     INTENT(OUT) :: PORODIR  ! subgrid-scale orography direction (rad)
REAL, DIMENSION(KI),     INTENT(OUT) :: PSAND    ! Sand fraction
REAL, DIMENSION(KI),     INTENT(OUT) :: PCLAY    ! Clay fraction
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
INTEGER :: KI_NATURE ! dimension of nature tile
INTEGER :: JI
INTEGER, DIMENSION(KI) :: IMASK
REAL, DIMENSION(KI)   :: ZSEA,ZWATER,ZNATURE,ZTOWN
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_CPL_GCM_N',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF(LCPL_GCM) THEN
!
  IF(SIZE(PZ0)/=SIZE(U%XZ0H))THEN
    WRITE(ILUOUT,*)'try to get Z0 field from atmospheric model, but size is not correct'
    WRITE(ILUOUT,*)'size of field expected by the atmospheric model (PZ0) :', SIZE(PZ0)
    WRITE(ILUOUT,*)'size of field in SURFEX                         (XZ0) :', SIZE(U%XZ0)
    CALL ABOR1_SFX('GET_CPL_GCMN: PZ0 ARGUMENT SIZE /= XZ0 MODULE SIZE')
  ENDIF
!
  PRAIN (:) = U%XRAIN
  PSNOW (:) = U%XSNOW
  PZ0   (:) = U%XZ0
  PZ0H  (:) = U%XZ0H
  PQSURF(:) = U%XQSURF
!
  POROMEA(:)= XG*USS%XAVG_ZS
!
  WHERE(USS%XSSO_STDEV(:)==XUNDEF)
    POROSTD(:)=0.0
  ELSEWHERE
    POROSTD(:)=XG*USS%XSSO_STDEV(:)
  ENDWHERE
!
  WHERE(USS%XSSO_ANIS(:)==XUNDEF)
    POROANI(:)=1.0
  ELSEWHERE
    POROANI(:)=USS%XSSO_ANIS(:)
  ENDWHERE
!
  WHERE(USS%XSSO_DIR(:)==XUNDEF)
    PORODIR(:)=0.0
  ELSEWHERE
    PORODIR(:)=USS%XSSO_DIR(:)*XPI/180.
  ENDWHERE
!
! Get sand and clay fraction for the aerosols scheme in arpege
! (as in get_surf_varn.F90)

  CALL GET_FRAC_n(U,HPROGRAM,KI,ZSEA,ZWATER,ZNATURE,ZTOWN)

  KI_NATURE = COUNT(ZNATURE (:) > 0.0)
  IMASK(:)=0
  CALL GET_1D_MASK(KI_NATURE, KI, ZNATURE, IMASK(1:KI_NATURE))
  
  PSAND(:)=0.0
  PCLAY(:)=0.0
  DO JI=1,KI_NATURE
    PSAND(IMASK(JI))=IM%K%XSAND(JI,1)
    PCLAY(IMASK(JI))=IM%K%XCLAY(JI,1)
  ENDDO

ELSE
!
  WRITE(ILUOUT,*)'LCPL_GCM must be TRUE when you use atmospheric model'
  CALL ABOR1_SFX('GET_CPL_GCMN: LCPL_GCM must be TRUE')
!  
ENDIF

!
IF (LHOOK) CALL DR_HOOK('GET_CPL_GCM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_CPL_GCM_n
