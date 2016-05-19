!     #########
SUBROUTINE PREP_TEB_GARDEN_UNIF(KLUOUT,HSURF,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_GARDEN_UNIF* - prepares ISBA field from prescribed values
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
!
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP_TEB_GARDEN,ONLY : XHUG_SURF, XHUG_ROOT, XHUG_DEEP,       &
                                  XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,  &
                                  XTG_SURF, XTG_ROOT, XTG_DEEP,        &
                                  XWR_DEF  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
REAL, POINTER, DIMENSION(:,:,:) :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
INTEGER :: JV ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN_UNIF',0,ZHOOK_HANDLE)
SELECT CASE(HSURF)
!
!*      3.0    Orography
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(1,1,1))
    PFIELD = 0.
   
!
!*      3.1    Profile of soil relative humidity
!
  CASE('WG     ')
    ALLOCATE(PFIELD(1,3,NVEGTYPE))
    DO JV=1,NVEGTYPE
      PFIELD(:,1,JV) = XHUG_SURF
      PFIELD(:,2,JV) = XHUG_ROOT
      PFIELD(:,3,JV) = XHUG_DEEP
    END DO

!*      3.2    Profile of soil humidity for ice

  CASE('WGI    ')
    ALLOCATE(PFIELD(1,3,NVEGTYPE))
    DO JV=1,NVEGTYPE
      PFIELD(:,1,JV) = XHUGI_SURF
      PFIELD(:,2,JV) = XHUGI_ROOT
      PFIELD(:,3,JV) = XHUGI_DEEP
    END DO

!*      3.3    Profile of temperatures

  CASE('TG     ')
    ALLOCATE(PFIELD(1,3,NVEGTYPE))
    DO JV=1,NVEGTYPE
      PFIELD(:,1,JV) = XTG_SURF
      PFIELD(:,2,JV) = XTG_ROOT
      PFIELD(:,3,JV) = XTG_DEEP
    END DO

!*      3.4    Other quantities

  CASE('WR     ')
    ALLOCATE(PFIELD(1,1,NVEGTYPE))
    PFIELD = XWR_DEF

  CASE('LAI    ')
    ALLOCATE(PFIELD(1,1,NVEGTYPE))
    PFIELD = XUNDEF

END SELECT
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='UNIF  '
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN_UNIF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_GARDEN_UNIF
