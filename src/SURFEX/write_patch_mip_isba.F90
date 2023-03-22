!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_PATCH_CMIP_ISBA(DUO, U, ID, IO, S, HPROGRAM, HRECFM, HCOMMENT, PWORK)
!     #################################
!
!!****  *WRITE_PATCH_CMIP_ISBA*
!!
!!    PURPOSE
!!    -------
!!
!!    Writes the ISBA patch diagnostic fields as specified by cmip
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      R. Séférian   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2016

!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIAG_n,         ONLY : DIAG_OPTIONS_t
USE MODD_SURF_ATM_n,     ONLY : SURF_ATM_t
USE MODD_SURFEX_n,       ONLY : ISBA_DIAG_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t
!
USE MODI_WRITE_SURF
USE MODI_PATCH_TO_TILE_FUNC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(ISBA_DIAG_t),    INTENT(INOUT) :: ID
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
!
CHARACTER(LEN=6),     INTENT(IN) :: HPROGRAM ! program calling
CHARACTER(LEN=14),    INTENT(IN) :: HRECFM
CHARACTER(LEN=100),   INTENT(IN) :: HCOMMENT
!
REAL, DIMENSION(:,:), INTENT(IN) :: PWORK
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(U%NSIZE_NATURE,7) :: ZWORKTILES
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=20) :: YREC           ! Name of the article to be write
INTEGER           :: ICH
LOGICAL           :: GWRITE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!CMIP patches :
!
!Patch 1 = Tree
!Patch 2 = Shrub (not yet in isba)
!Patch 3 = natural Grass
!Patch 4 = Crop
!Patch 5 = Pasture (not yet in isba)
!Patch 6 = no-vegetated
!Patch 7 = permanent snow and ice
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_PATCH_CMIP_ISBA',0,ZHOOK_HANDLE)
!
GWRITE = (ID%O%LPATCH_BUDGET.AND.(IO%NPATCH==12.OR.IO%NPATCH==19))
!
IF(.NOT.GWRITE)THEN
  IF (LHOOK) CALL DR_HOOK('WRITE_PATCH_CMIP_ISBA',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
ZWORKTILES(:,:)=PATCH_TO_TILE_FUNC(S%XPATCH(:,:),PWORK(:,:))
!
IF(HPROGRAM=='FA')THEN
  ICH=MIN(8,LEN_TRIM(HRECFM))
ELSE
  ICH=LEN_TRIM(HRECFM)
ENDIF
!
YREC=HRECFM(:ICH)//'Tree'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,ZWORKTILES(:,1),IRESP,HCOMMENT)
!
IF(IO%NPATCH==19)THEN
  YREC=HRECFM(:ICH)//'Shrub'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,ZWORKTILES(:,2),IRESP,HCOMMENT)
ENDIF
!
YREC=HRECFM(:ICH)//'Grass'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,ZWORKTILES(:,3),IRESP,HCOMMENT)
!
YREC=HRECFM(:ICH)//'Crop'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,ZWORKTILES(:,4),IRESP,HCOMMENT)
!
!YREC=HRECFM(:ICH)//'Pastur'
!CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,ZWORKTILES(:,5),IRESP,HCOMMENT)
!
YREC=HRECFM(:ICH)//'Bsoil'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,ZWORKTILES(:,6),IRESP,HCOMMENT)
!
YREC=HRECFM(:ICH)//'Is'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YREC,ZWORKTILES(:,7),IRESP,HCOMMENT)
!
!
IF (LHOOK) CALL DR_HOOK('WRITE_PATCH_CMIP_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_PATCH_CMIP_ISBA
