!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_SFX_LAND (I, U, &
                               OCPL_GW,OCPL_FLOOD,OCPL_CALVING,  &
                              PRUNOFF,PDRAIN,PCALVING,PRECHARGE, &
                              PSRCFLOOD            )  
!     ###############################################################################
!
!!****  *GET_SFX_LAND* - routine to get some land surface variables from surfex
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
!!      B. Decharme      *Meteo France*
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
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
!
USE MODI_UNPACK_SAME_RANK
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
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
LOGICAL,            INTENT(IN)  :: OCPL_GW     ! groundwater/surface key
LOGICAL,            INTENT(IN)  :: OCPL_FLOOD   ! flood key
LOGICAL,            INTENT(IN)  :: OCPL_CALVING ! calving key
!
REAL, DIMENSION(:), INTENT(OUT) :: PRUNOFF    ! Cumulated Surface runoff             (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PDRAIN     ! Cumulated Deep drainage              (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PCALVING   ! Cumulated Calving flux               (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PRECHARGE  ! Cumulated Recharge to groundwater    (kg/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSRCFLOOD  ! Cumulated freshwater flux            (kg/m2)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(I%XCPL_PFLOOD)) :: ZSRCFLOOD
!
REAL, DIMENSION(SIZE(PCALVING)) :: ZCALVING
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_SFX_LAND',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*       1.0   Initialization
!              --------------
!
PRUNOFF  (:) = XUNDEF
PDRAIN   (:) = XUNDEF
PCALVING (:) = XUNDEF
PRECHARGE(:) = XUNDEF
PSRCFLOOD(:) = XUNDEF
!
!*       2.0   Get variable over nature
!              ------------------------
!
IF(U%NSIZE_NATURE>0)THEN
!
! * surface runoff
!
  CALL UNPACK_SAME_RANK(U%NR_NATURE,I%XCPL_RUNOFF(:),PRUNOFF(:),XUNDEF)
  I%XCPL_RUNOFF (:) = 0.0
!
! * deep drainage
!
  CALL UNPACK_SAME_RANK(U%NR_NATURE,I%XCPL_DRAIN(:),PDRAIN(:),XUNDEF)
  I%XCPL_DRAIN(:) = 0.0
!
! * Calving flux
!
  IF(OCPL_CALVING)THEN
    CALL UNPACK_SAME_RANK(U%NR_NATURE,I%XCPL_ICEFLUX(:),PCALVING(:),XUNDEF)
    I%XCPL_ICEFLUX(:) = 0.0
  ELSEIF(I%LGLACIER)THEN
    I%XCPL_DRAIN  (:) = I%XCPL_DRAIN(:) + I%XCPL_ICEFLUX(:)
    I%XCPL_ICEFLUX(:) = 0.0
  ENDIF
!
! * groundwater recharge 
!
  IF(OCPL_GW)THEN
    CALL UNPACK_SAME_RANK(U%NR_NATURE,I%XCPL_RECHARGE(:),PRECHARGE(:),XUNDEF)
    I%XCPL_RECHARGE(:)=0.0
  ENDIF
!
! * floodplain source terms
!
  IF(OCPL_FLOOD)THEN
    ZSRCFLOOD  (:) = I%XCPL_PFLOOD(:)-I%XCPL_EFLOOD(:)-I%XCPL_IFLOOD(:)
    I%XCPL_PFLOOD(:) = 0.0
    I%XCPL_EFLOOD(:) = 0.0
    I%XCPL_IFLOOD(:) = 0.0
    CALL UNPACK_SAME_RANK(U%NR_NATURE,ZSRCFLOOD(:),PSRCFLOOD(:),XUNDEF)
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_SFX_LAND',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_SFX_LAND
