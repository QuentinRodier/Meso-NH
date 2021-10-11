!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE INIT_DST_VEG (DSTK, U, &
                     HPROGRAM,  &    ! Program calling unit
                  KSIZE_P, & ! Number of nature points in a patch
                  KR_P, &    ! Mask from patch --> nature vectors
                  PVEGTYPE_PATCH  ) ! fraction (in a nature point) of a vegtype for a patch

!
USE MODD_DST_n, ONLY : DST_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_DST_SURF
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK
!
USE MODI_GET_LUOUT
USE MODI_GET_VEGTYPE_2_PATCH_MASK
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!PASSED VARIABLES
!
TYPE(DST_t), INTENT(INOUT) :: DSTK
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6), INTENT(IN)      :: HPROGRAM              !Passing unit
!
INTEGER, INTENT(IN) :: KSIZE_P
INTEGER, DIMENSION(:), INTENT(IN) :: KR_P
REAL, DIMENSION(:,:), INTENT(IN) :: PVEGTYPE_PATCH
!
!LOCAL VARIABLES
 CHARACTER(LEN=4)    :: CRGUNIT               ! type of log-normal geometric mean radius
INTEGER             :: JVEG                  ! Counter for vegetation classes
INTEGER             :: JVEG_IN               ! Vegetation index
INTEGER             :: JPATCH                ! Counter for patches
INTEGER             :: JMODE                 ! Counter for dust modes
INTEGER             :: JMODE_IDX             ! Index for dust modes
INTEGER             :: ILUOUT
INTEGER             :: ISIZE_LARGEST_DST
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!get output listing unit
IF (LHOOK) CALL DR_HOOK('INIT_DST_VEG',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!Allocate memory
!ALLOCATE(NVEGNO_DST)
!Set the number of classes that can emit dust (fxm: set this elsewhere)
NVEGNO_DST = 2
!
!Allocate memory for the vegtype-translator
ALLOCATE(DSTK%NVT_DST(NVEGNO_DST))
!
!Set the dust/vegtype translator vector
DSTK%NVT_DST(1)  = NVT_NO
DSTK%NVT_DST(2)  = NVT_ROCK
!
!Allocate memory for roughness lengths of erodible surfaces
ALLOCATE(DSTK%Z0_EROD_DST(NVEGNO_DST))
!
!Set the roughness lengths corresponding to erodible surfaces
!Smooth roughness length is given to 1.d-5 (dstmbl.f90)
DSTK%Z0_EROD_DST(1) = 30.d-6    !m (30 um) 
DSTK%Z0_EROD_DST(2) = 200.d-6   !m (200 um) 
!
!Allocate memory for dust emitter surface vectors in patch vectors
IF (.NOT.ASSOCIATED(DSTK%NSIZE_PATCH_DST)) ALLOCATE(DSTK%NSIZE_PATCH_DST(NVEGNO_DST))
!
DO JVEG = 1,NVEGNO_DST
  !Count all the points in the patch where you have dust emitter vegetation
  DSTK%NSIZE_PATCH_DST(JVEG) = COUNT(PVEGTYPE_PATCH(:,DSTK%NVT_DST(JVEG)) > 0.) 
ENDDO
!
!Find the largest dust emitter vector in any patch
!ALLOCATE (NSIZE_LARGEST_DST)
ISIZE_LARGEST_DST = 0
DO JVEG = 1,NVEGNO_DST
  ISIZE_LARGEST_DST = max(ISIZE_LARGEST_DST,DSTK%NSIZE_PATCH_DST(JVEG))
ENDDO
!
!Allocate memory for KR_PATCH_DST mask translate from patch vector to dust vector
ALLOCATE(DSTK%NR_PATCH_DST(ISIZE_LARGEST_DST,NVEGNO_DST))
!
!Initialize the mask array
DSTK%NR_PATCH_DST(:,:)=0
!
!Get values from the dust emitter vegetation mask
DO JVEG=1,NVEGNO_DST
  JVEG_IN = DSTK%NVT_DST(JVEG)          ! Get the real vegtype index
#ifdef RJ_OFIX
  CALL GET_VEGTYPE_2_PATCH_MASK(ILUOUT,    &
           DSTK%NSIZE_PATCH_DST(JVEG),             &!I Size of dust emitter vector
           KSIZE_P,                   &!I Size of patch vector
!RJ: attempt to make this call generic
           KR_P,&!I Mask from patch to nature
           PVEGTYPE_PATCH,                           &!I Fraction of vegtype of nature point within jpatch 
           DSTK%NR_PATCH_DST(:DSTK%NSIZE_PATCH_DST(JVEG),JVEG),  &!O Part of mask array to fill with values
           JVEG_IN                                  &!I Index of vegtype in question
             )  
#else
  CALL GET_VEGTYPE_2_PATCH_MASK(ILUOUT,    &
           DSTK%NSIZE_PATCH_DST(JVEG),             &!I Size of dust emitter vector
           KSIZE_P,                   &!I Size of patch vector
           KR_P,                              &!I Mask from patch to nature
           PVEGTYPE_PATCH,                           &!I Fraction of vegtype of nature point within jpatch 
           DSTK%NR_PATCH_DST(:DSTK%NSIZE_PATCH_DST(JVEG),JVEG),  &!O Part of mask array to fill with values
           JVEG_IN                                  &!I Index of vegtype in question
             )  
#endif
ENDDO    !Loop on veg-types
!
IF (LHOOK) CALL DR_HOOK('INIT_DST_VEG',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_DST_VEG

