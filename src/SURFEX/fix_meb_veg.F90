!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE FIX_MEB_VEG (DTI, IG, I, &
                              KPATCH)
!     ################################################################
!
!!    PURPOSE
!!    -------
!!
!!    A MEB routine:
!!
!!    For LMEB=true ...
!!
!!    ... XPAR_VEG/XDATA_VEG has to be set to >0 for those NVEGTYPE positions
!!    corresponding to the patches where TEMP_LMEB_PATCH = true.
!!    Otherwise, if XPAR_VEG/XDATA_VEG is 0 for these  NVEGTYPE positions
!!    PLAI will be set to undefined in CONVERT_PATCH_ISBA.
!!
!!    ... XPAR_VEG/XDATA_VEG has to be set to <1 for those NVEGTYPE positions
!!    corresponding to the patches where TEMP_LMEB_PATCH = true.
!!    Otherwise, if XPAR_VEG/XDATA_VEG is 1 for these  NVEGTYPE positions
!!    PALBNIR_SOIL will be set to undefined in CONVERT_PATCH_ISBA.
!!
!!    Therefore, XPAR_VEG/XDATA_VEG is set to 0.5 in the identified cases!
!!
!!
!!    METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    P. Samuelsson
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2013
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_DATA_COVER,     ONLY : XDATA_VEG
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_VEGTYPE_TO_PATCH 
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
!
INTEGER, INTENT(IN) :: KPATCH
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IPATCH
!
REAL, DIMENSION(IG%NDIM,DTI%NTIME)                          :: ZWORKPAR
REAL, DIMENSION(SIZE(XDATA_VEG,1),SIZE(XDATA_VEG,2)) :: ZWORKDATA
!
INTEGER :: JVEGTYPE! loop on vegtype
INTEGER ::  PATCH_LIST(NVEGTYPE)
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('FIX_MEB_VEG',0,ZHOOK_HANDLE)
!
DO JVEGTYPE=1,NVEGTYPE
  PATCH_LIST(JVEGTYPE) = VEGTYPE_TO_PATCH (JVEGTYPE, KPATCH)
ENDDO
!
DO JVEGTYPE=1,NVEGTYPE
  !
  ZWORKDATA(:,:)=XDATA_VEG(:,:,JVEGTYPE)
  IF (DTI%LDATA_VEG) ZWORKPAR(:,:)=DTI%XPAR_VEG(:,:,JVEGTYPE)
  !
  DO IPATCH=1,KPATCH
    IF(PATCH_LIST(JVEGTYPE)==IPATCH .AND. I%LMEB_PATCH(IPATCH))THEN
      ZWORKDATA(:,:)=0.5
      IF (DTI%LDATA_VEG) ZWORKPAR(:,:)=0.5
      EXIT
    ENDIF
  ENDDO
  !
  XDATA_VEG(:,:,JVEGTYPE)=ZWORKDATA(:,:)
  IF (DTI%LDATA_VEG) DTI%XPAR_VEG(:,:,JVEGTYPE)=ZWORKPAR(:,:)
  !
ENDDO
!
!
IF (LHOOK) CALL DR_HOOK('FIX_MEB_VEG',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!   
END SUBROUTINE FIX_MEB_VEG

