!##################
MODULE MODD_TEB_HYDRO_n
!##################
!
!!****  *MODD_TEB_HYDRO - declaration of ISBA scheme packed surface parameters for urban hydrology
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       09/2009
!!      V. Masson       06/2013  splits module in 4
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE TEB_HYDRO_t
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
! - Soil heat and water:
!
  REAL, POINTER, DIMENSION(:,:)  :: XWG_ROAD          ! soil volumetric water content profile                       (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XWGI_ROAD         ! soil liquid water equivalent volumetric ice content profile (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XWG_BLD           ! soil volumetric water content profile                       (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XWGI_BLD          ! soil liquid water equivalent volumetric ice content profile (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XCOND_ROAD        ! hydraulic conductivity of the compartment "road"            (m/s)
  REAL, POINTER, DIMENSION(:,:)  :: XCOND_BLD         ! hydraulic conductivity of the compartment "bld"             (m/s)
  REAL, POINTER, DIMENSION(:,:)  :: XCOND_GD          ! hydraulic conductivity of the compartment "garden"          (m/s)
  REAL, POINTER, DIMENSION(:)    :: XKSAT_ICE         ! hydraulic conductivity at saturation over frozen area       (m s-1)   
!                  
!-------------------------------------------------------------------------------
!
END TYPE TEB_HYDRO_t
!
TYPE TEB_HYDRO_NP_t
  !
  TYPE(TEB_HYDRO_t), POINTER :: AL(:) => NULL()
  !
END TYPE TEB_HYDRO_NP_t
!
CONTAINS
!
!
SUBROUTINE TEB_HYDRO_INIT(YTEB_HYDRO)
TYPE(TEB_HYDRO_t), INTENT(INOUT) :: YTEB_HYDRO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_HYDRO_N:TEB_HYDRO_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YTEB_HYDRO%XWG_ROAD)
  NULLIFY(YTEB_HYDRO%XWGI_ROAD)
  NULLIFY(YTEB_HYDRO%XWG_BLD)
  NULLIFY(YTEB_HYDRO%XWGI_BLD)
  NULLIFY(YTEB_HYDRO%XCOND_ROAD)
  NULLIFY(YTEB_HYDRO%XCOND_BLD)
  NULLIFY(YTEB_HYDRO%XCOND_GD)
  NULLIFY(YTEB_HYDRO%XKSAT_ICE)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_HYDRO_N:TEB_HYDRO_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_HYDRO_INIT


SUBROUTINE TEB_HYDRO_NP_INIT(YNTEB_HYDRO,KPATCH)
TYPE(TEB_HYDRO_NP_t), INTENT(INOUT) :: YNTEB_HYDRO
INTEGER, INTENT(IN) :: KPATCH
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_N:TEB_HYDRO_NP_INIT",0,ZHOOK_HANDLE)
IF (.NOT.ASSOCIATED(YNTEB_HYDRO%AL)) THEN
  ALLOCATE(YNTEB_HYDRO%AL(KPATCH))
  DO JP=1,KPATCH
    CALL TEB_HYDRO_INIT(YNTEB_HYDRO%AL(JP))
  ENDDO
ELSE
  DO JP=1,KPATCH
    CALL TEB_HYDRO_INIT(YNTEB_HYDRO%AL(JP))
  ENDDO
  DEALLOCATE(YNTEB_HYDRO%AL)        
ENDIF
IF (LHOOK) CALL DR_HOOK("MODD_TEB_HYDRO_N:TEB_HYDRO_NP_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_HYDRO_NP_INIT

END MODULE MODD_TEB_HYDRO_n
