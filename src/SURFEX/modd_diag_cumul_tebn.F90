!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ############################
      MODULE MODD_DIAG_CUMUL_TEB_n
!     ############################
!
!!****  *MODD_DIAG_CUMUL_TEB - declaration of cumulated surface parameters for TEB scheme
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
!!      C de Munck   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       19/02/2013
!
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

!TYPE DIAG_CUMUL_TEB_OPTIONS_t
!------------------------------------------------------------------------------
!
!  LOGICAL :: LTEB_CUM   ! flag for cumulated terms of teb scheme
!
!------------------------------------------------------------------------------
!END TYPE DIAG_CUMUL_TEB_OPTIONS_t
!
TYPE DIAG_CUMUL_TEB_1P_t
!------------------------------------------------------------------------------
!* miscellaneous variables
!
  REAL, POINTER, DIMENSION(:)   :: XRUNOFFC_TOWN      ! cumulateda ggregated water runoff for town      (kg/m2 town/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFFC_GARDEN    ! cumulated water runoff for green areas          (kg/m2 garden/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFFC_ROAD      ! cumulated water runoff for roads                (kg/m2 road/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFFC_ROOF      ! cumulated aggregated water runoff for roofs     (kg/m2 roof/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFFC_STRLROOF  ! cumulated water runoff for structural roofs     (kg/m2 structural roof/s)
  REAL, POINTER, DIMENSION(:)   :: XRUNOFFC_GREENROOF ! cumulated water runoff for greenroof            (kg/m2 greenroof/s)
  REAL, POINTER, DIMENSION(:)   :: XDRAINC_GREENROOF  ! cumulated water vertical drainage for greenroof (kg/m2 greenroof/s)
  REAL, POINTER, DIMENSION(:)   :: XDRAINC_GARDEN     ! cumulated water vertical drainage for gardens   (kg/m2 garden/s)
  REAL, POINTER, DIMENSION(:)   :: XIRRIGC_GREENROOF  ! cumulated water supply from summer irrigation   (kg/m2 greenroof/s)
  REAL, POINTER, DIMENSION(:)   :: XIRRIGC_GARDEN     ! cumulated water supply from summer irrigation   (kg/m2 garden/s)
  REAL, POINTER, DIMENSION(:)   :: XIRRIGC_ROAD       ! cumulated water supply from summer irrigation   (kg/m2 road/s)
  !
  REAL, POINTER, DIMENSION(:)   :: XHVACC_COOL        ! cumulated en. consump. of the cooling system [W m-2(bld)]
  REAL, POINTER, DIMENSION(:)   :: XHVACC_HEAT        ! cumulated en. consump. of the heating system [W m-2(bld)]
  REAL, POINTER, DIMENSION(:)   :: XTHER_PROD_BLDC    ! cumulated en. product. of thermal      solar panels [W m-2(bld)]
  REAL, POINTER, DIMENSION(:)   :: XPHOT_PROD_BLDC    ! cumulated en. product. of photovoltaic solar panels [W m-2(bld)]
!
!------------------------------------------------------------------------------
END TYPE DIAG_CUMUL_TEB_1P_t

TYPE DIAG_CUMUL_TEB_t
  !
  TYPE(DIAG_CUMUL_TEB_1P_t), POINTER :: ALP(:) => NULL()
  TYPE(DIAG_CUMUL_TEB_1P_t), POINTER :: CUR => NULL()
  !
END TYPE DIAG_CUMUL_TEB_t




 CONTAINS
!
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!
!



!
SUBROUTINE DIAG_CUMUL_TEB_GOTO_PATCH(YDIAG_CUMUL_TEB,KTO_PATCH)
TYPE(DIAG_CUMUL_TEB_t), INTENT(INOUT) :: YDIAG_CUMUL_TEB
INTEGER, INTENT(IN) :: KTO_PATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Current patch is set to patch KTO_PATCH
IF (LHOOK) CALL DR_HOOK('MODD_DIAG_CUMUL_TEB_N:DIAG_CUMUL_TEB_GOTO_PATCH',0,ZHOOK_HANDLE)

YDIAG_CUMUL_TEB%CUR => YDIAG_CUMUL_TEB%ALP(KTO_PATCH)

IF (LHOOK) CALL DR_HOOK('MODD_DIAG_CUMUL_TEB_N:DIAG_CUMUL_TEB_GOTO_PATCH',1,ZHOOK_HANDLE)

END SUBROUTINE DIAG_CUMUL_TEB_GOTO_PATCH
!
!------------------------------------------------------------------------------
!
SUBROUTINE DIAG_CUMUL_TEB_INIT(YDIAG_CUMUL_TEB,KPATCH)
TYPE(DIAG_CUMUL_TEB_t), INTENT(INOUT) :: YDIAG_CUMUL_TEB
INTEGER, INTENT(IN) :: KPATCH
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_CUMUL_TEB_N:DIAG_CUMUL_TEB_INIT",0,ZHOOK_HANDLE)
  ALLOCATE(YDIAG_CUMUL_TEB%ALP(KPATCH))
  YDIAG_CUMUL_TEB%CUR => YDIAG_CUMUL_TEB%ALP(1)
DO JP=1,KPATCH
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XRUNOFFC_TOWN)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XRUNOFFC_GARDEN)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XRUNOFFC_ROAD)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XRUNOFFC_ROOF)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XRUNOFFC_STRLROOF)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XRUNOFFC_GREENROOF)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XDRAINC_GREENROOF)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XDRAINC_GARDEN)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XIRRIGC_GREENROOF)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XIRRIGC_GARDEN)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XIRRIGC_ROAD)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XHVACC_COOL)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XHVACC_HEAT)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XTHER_PROD_BLDC)
  NULLIFY(YDIAG_CUMUL_TEB%ALP(JP)%XPHOT_PROD_BLDC)
ENDDO 
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_CUMUL_TEB_N:DIAG_CUMUL_TEB_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_CUMUL_TEB_INIT
!
!------------------------------------------------------------------------------
!
!
!------------------------------------------------------------------------------

END MODULE MODD_DIAG_CUMUL_TEB_n
