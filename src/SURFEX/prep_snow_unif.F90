!     #########
SUBROUTINE PREP_SNOW_UNIF(KLUOUT,HSURF,PFIELD, TPTIME,  &
                          OSNOW_IDEAL,                  &
                          PUNIF_WSNOW, PUNIF_RSNOW,     &
                          PUNIF_TSNOW, PUNIF_ASNOW,     &
                          PUNIF_SG1SNOW, PUNIF_SG2SNOW, &
                          PUNIF_HISTSNOW,PUNIF_AGESNOW  )  
!     #################################################################################
!
!!****  *PREP_SNOW_UNIF* - prepares snow field from prescribed values
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
!!      M. Lafaysse adaptation with new snow age
!!------------------------------------------------------------------
!
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_PREP_SNOW,      ONLY : NGRID_LEVEL
!
USE MODI_SNOW_T_WLIQ_TO_HEAT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! output listing logical unit
 CHARACTER(LEN=10),  INTENT(IN)  :: HSURF     ! type of field
REAL, POINTER, DIMENSION(:,:,:) :: PFIELD    ! field to interpolate horizontally
TYPE(DATE_TIME),    INTENT(IN)  :: TPTIME    ! date and time
LOGICAL,            INTENT(IN)  :: OSNOW_IDEAL
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_WSNOW ! prescribed snow content (kg/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_RSNOW ! prescribed density (kg/m3)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_TSNOW ! prescribed temperature (K)
REAL,               INTENT(IN)  :: PUNIF_ASNOW ! prescribed albedo (-)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_SG1SNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_SG2SNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_HISTSNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_AGESNOW ! 
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTSNOW, ZRSNOW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_UNIF',0,ZHOOK_HANDLE)
!
IF (OSNOW_IDEAL) THEN
  ALLOCATE(PFIELD(1,SIZE(PUNIF_WSNOW),1))
  ALLOCATE(ZTSNOW(1,SIZE(PUNIF_WSNOW),1))
  ALLOCATE(ZRSNOW(1,SIZE(PUNIF_WSNOW),1))
ELSE
  ALLOCATE(PFIELD(1,NGRID_LEVEL,1))
  ALLOCATE(ZTSNOW(1,NGRID_LEVEL,1))
  ALLOCATE(ZRSNOW(1,NGRID_LEVEL,1))
ENDIF
!
!*      1.     No snow
!              -------
!
IF (ANY(PUNIF_RSNOW(:)==0. .AND. PUNIF_WSNOW(:)/=0.)) THEN 
  WRITE(KLUOUT,*)'XWSNOW/=0. AND RSNOW=0.'
  CALL ABOR1_SFX('PREP_SNOW_UNIF: WITH XWSNOW/=0., RSNOW MUST NOT BE 0.')
END IF
!
!*      2.     Snow prescribed
!              ---------------
!
SELECT CASE(HSURF(1:3))
!
  CASE('WWW')
    IF (OSNOW_IDEAL) THEN
      PFIELD(1,:,1) = PUNIF_WSNOW(:)
    ELSE
      PFIELD(1,:,1) = PUNIF_WSNOW(1)
    ENDIF
  CASE('RHO')
    IF (OSNOW_IDEAL) THEN
      PFIELD(1,:,1) = PUNIF_RSNOW(:)
    ELSE
      PFIELD(1,:,1) = PUNIF_RSNOW(1)
    ENDIF               
  CASE('ALB')          
    PFIELD = PUNIF_ASNOW
  CASE('DEP')
    IF (OSNOW_IDEAL) THEN
      PFIELD(1,:,1) = PUNIF_WSNOW(:)/PUNIF_RSNOW(:)
    ELSE
      PFIELD(1,:,1) = PUNIF_WSNOW(1)/PUNIF_RSNOW(1)
    ENDIF              
  CASE('HEA')
    IF (OSNOW_IDEAL) THEN
      ZRSNOW(1,:,1) = PUNIF_RSNOW(:)
      ZTSNOW(1,:,1) = PUNIF_TSNOW(:)
    ELSE
      ZRSNOW(1,:,1) = PUNIF_RSNOW(1)
      ZTSNOW(1,:,1) = PUNIF_TSNOW(1)
    ENDIF
    CALL SNOW_T_WLIQ_TO_HEAT(PFIELD,ZRSNOW,ZTSNOW)
  CASE('SG1')
    IF (OSNOW_IDEAL) THEN
      PFIELD(1,:,1) = PUNIF_SG1SNOW(:)
    ELSE
      PFIELD(1,:,1) = PUNIF_SG1SNOW(1)
    ENDIF 
  CASE('SG2')
    IF (OSNOW_IDEAL) THEN
      PFIELD(1,:,1) = PUNIF_SG2SNOW(:)
    ELSE
      PFIELD(1,:,1) = PUNIF_SG2SNOW(1)
    ENDIF 
  CASE('HIS')
    IF (OSNOW_IDEAL) THEN
      PFIELD(1,:,1) = PUNIF_HISTSNOW(:)
    ELSE
      PFIELD(1,:,1) = PUNIF_HISTSNOW(1)
    ENDIF     
  CASE('AGE')
    IF (OSNOW_IDEAL) THEN
      PFIELD(1,:,1) = PUNIF_AGESNOW(:)
    ELSE
      PFIELD(1,:,1) = PUNIF_AGESNOW(1)
    ENDIF           
  !
END SELECT
!
!*      2.     Interpolation method
!              --------------------
!
CINTERP_TYPE='UNIF  '
DEALLOCATE(ZTSNOW)
DEALLOCATE(ZRSNOW)
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_UNIF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SNOW_UNIF
