!     #########
SUBROUTINE PREP_SNOW_BUFFER(HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_SNOW_BUFFER* - prepares snow field from operational BUFFER
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005
!!------------------------------------------------------------------
!
!
USE MODE_READ_BUFFER
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_BUFFER_GRID
USE MODI_SNOW_T_WLIQ_TO_HEAT
!
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_PREP_SNOW,      ONLY : XGRID_SNOW
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_GRID_BUFFER,    ONLY : NNI
USE MODD_SNOW_PAR,       ONLY : XANSMIN, XANSMAX, XRHOSMAX
USE MODD_CSTS,           ONLY : XTT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=10),   INTENT(IN) :: HSURF     ! type of field
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
TYPE (DATE_TIME)                :: TZTIME_BUFFER    ! current date and time
 CHARACTER(LEN=6)                :: YINMODEL       ! model from which GRIB file originates
REAL, DIMENSION(:),   POINTER   :: ZFIELD1D       ! field read
REAL, DIMENSION(:),   POINTER   :: ZHEAT          ! heat in snow
REAL, DIMENSION(:),   POINTER   :: ZRHO           ! density of snow
INTEGER                         :: JVEGTYPE       ! loop counter on vegtypes
INTEGER                         :: JLAYER         ! loop on snow fine grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Reading of grid
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_BUFFER',0,ZHOOK_HANDLE)
 CALL PREP_BUFFER_GRID(KLUOUT,YINMODEL,TZTIME_BUFFER)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of the physical field for urban areas
!              ---------------------------------------------
!
IF (HSURF(7:8)=='RO') THEN
  ! 
  SELECT CASE(HSURF(1:3))
    CASE('DEP','ALB','WWW')
      ALLOCATE(PFIELD(NNI,1,1))
    CASE('HEA','RHO')
      ALLOCATE(PFIELD(NNI,SIZE(XGRID_SNOW),1))
  END SELECT
  !
  PFIELD(:,:,:) = 0.
!
!-------------------------------------------------------------------------------------
!
!*      3.     Reading of the physical field for vegetated areas
!              -------------------------------------------------
!
ELSE
!
  SELECT CASE(HSURF(1:3))
!
!*      3.1    Total snow content (kg/m2)
!
  CASE('WWW')
     CALL READ_BUFFER_SNOW_VEG(KLUOUT,YINMODEL,ZFIELD1D)
     !
     ALLOCATE(PFIELD(NNI,1,NVEGTYPE))
     DO JVEGTYPE=1,NVEGTYPE
       PFIELD(:,1,JVEGTYPE)=ZFIELD1D(:)
     END DO
     DEALLOCATE(ZFIELD1D)
!
!
!*      3.2    Total snow depth (m)
!
  CASE('DEP')
     CALL READ_BUFFER_SNOW_VEG_DEPTH(KLUOUT,YINMODEL,ZFIELD1D)
     !
     ALLOCATE(PFIELD(NNI,1,NVEGTYPE))
     DO JVEGTYPE=1,NVEGTYPE
       PFIELD(:,1,JVEGTYPE)=ZFIELD1D(:)
     END DO
     DEALLOCATE(ZFIELD1D)
!
!
!*      3.3    Profile of heat in the snow
!
  CASE('HEA')
     !* read temperature
     CALL READ_BUFFER_TS(KLUOUT,YINMODEL,ZFIELD1D)
     WHERE (ZFIELD1D/=XUNDEF) ZFIELD1D(:) = MIN(ZFIELD1D,XTT)
     !* assumes no liquid water in the snow
     ALLOCATE(ZHEAT(SIZE(ZFIELD1D)))
     ALLOCATE(ZRHO (SIZE(ZFIELD1D)))
     ZRHO(:) = XRHOSMAX
     !
     CALL SNOW_T_WLIQ_TO_HEAT(ZHEAT,ZRHO,ZFIELD1D)
     !
     ALLOCATE(PFIELD(NNI,SIZE(XGRID_SNOW),NVEGTYPE))
     DO JVEGTYPE=1,NVEGTYPE
       DO JLAYER=1,SIZE(XGRID_SNOW)
         PFIELD(:,JLAYER,JVEGTYPE)=ZHEAT(:)
       END DO
     END DO
     DEALLOCATE(ZFIELD1D)
     DEALLOCATE(ZHEAT   )
     DEALLOCATE(ZRHO    )
!
!*      3.4    Albedo
!
  CASE('ALB')    
    ALLOCATE(PFIELD(NNI,1,NVEGTYPE))
    PFIELD = 0.5 * ( XANSMIN + XANSMAX )
!
!*      3.5    Density
!
  CASE('RHO')    
    ALLOCATE(PFIELD(NNI,SIZE(XGRID_SNOW),NVEGTYPE))
    PFIELD = XRHOSMAX

  END SELECT
  !
END IF
!
!-------------------------------------------------------------------------------------
!
!*      4.     Interpolation method
!              --------------------
!
CINTERP_TYPE='BUFFER'
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_BUFFER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SNOW_BUFFER
