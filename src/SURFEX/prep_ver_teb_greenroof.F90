!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_TEB_GREENROOF (TGR, TGRO, TGRP, TOP)
!     #################################################################################
!
!!****  *PREP_VER_TEB_GREENROOF* - change in ISBA fields due to altitude change
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
!!     V. Masson  + A.Lemonsu & C.deMunck
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified by B. Decharme  (01/2009), Optional Arpege deep soil temperature initialization
!!------------------------------------------------------------------
!

!
!
USE MODD_TEB_GREENROOF_n, ONLY : TEB_GREENROOF_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
USE MODD_TEB_GREENROOF_PGD_n, ONLY : TEB_GREENROOF_PGD_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_ISBA_PAR,          ONLY : XWGMIN
USE MODD_SURF_PAR,          ONLY : XUNDEF
USE MODD_PREP,              ONLY : XZS_LS, XT_CLIM_GRAD
USE MODD_CSTS,              ONLY : XTT, XDAY, XLMTT, XRHOLW
!
USE MODE_THERMOS
USE MODI_PREP_VER_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!
TYPE(TEB_GREENROOF_t), INTENT(INOUT) :: TGR
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
TYPE(TEB_GREENROOF_PGD_t), INTENT(INOUT) :: TGRP
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
INTEGER                         :: JL        ! loop counter on layers
INTEGER                         :: IWORK     ! Work integer
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWGTOT    ! total water content
REAL, DIMENSION(:), ALLOCATABLE :: ZDW       ! variation of water in soil
REAL, DIMENSION(:), ALLOCATABLE :: ZZSFREEZE ! altitude where soil temperature equals XTT
INTEGER                         :: IDEEP_SOIL! layer corresponding to deep soil temperature
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWGI_CLIM_GRAD ! ice content vertical gradient
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTG_LS! temperature on initial orography
!
REAL                            :: ZGRADX = 5.E-4 ! slope of ice content gradient
REAL                            :: ZH0    = 5.E-1 ! constant used to define ice content gradient
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.0    Ice content climatologic gradient
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_TEB_GREENROOF',0,ZHOOK_HANDLE)
ALLOCATE(ZWGI_CLIM_GRAD (SIZE(TGR%CUR%XWG,1),SIZE(TGR%CUR%XWG,2)))
!
ZWGI_CLIM_GRAD(:,:) = ZGRADX * EXP( - TGRP%XDG(:,:) / ZH0 )
!-------------------------------------------------------------------------------------
!
!*      1.1    Temperature profile
!
ALLOCATE(ZTG_LS(SIZE(TGR%CUR%XTG,1),SIZE(TGR%CUR%XTG,2)))
ZTG_LS(:,:) = TGR%CUR%XTG(:,:)
!
  DO JL=1,SIZE(TGR%CUR%XTG,2)
    WHERE(TGR%CUR%XTG(:,JL)/=XUNDEF) &
      TGR%CUR%XTG(:,JL) = TGR%CUR%XTG(:,JL) + XT_CLIM_GRAD  * (TOP%XZS - XZS_LS)  
  END DO
!
!-------------------------------------------------------------------------------------
!
!*      1.2    Water and ice in the soil
!
ALLOCATE(ZZSFREEZE      (SIZE(TGR%CUR%XWG,1)))
ALLOCATE(ZWGTOT         (SIZE(TGR%CUR%XWG,1)))
ALLOCATE(ZDW            (SIZE(TGR%CUR%XWG,1)))
!
!* general case
!
IWORK=SIZE(TGR%CUR%XTG,2)
!
  !
  DO JL=1,IWORK
    !
    ZDW(:) = 0.
    ! altitude where deep soil freezes (diurnal surface response is not treated)
    ZZSFREEZE(:) = TOP%XZS + (XTT - TGR%CUR%XTG(:,JL)) / XT_CLIM_GRAD
    !
    WHERE(TGR%CUR%XTG(:,JL)/=XUNDEF) 
      !
      WHERE (ZTG_LS(:,JL) < XTT)
        !
        WHERE (TOP%XZS <= XZS_LS)
          !
          WHERE (TOP%XZS > ZZSFREEZE) 
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (TOP%XZS - XZS_LS)
          ELSEWHERE
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (ZZSFREEZE - XZS_LS) + ZGRADX * (TOP%XZS - ZZSFREEZE)
          ENDWHERE
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (TOP%XZS - XZS_LS)
          !
        ENDWHERE
        !
      ELSEWHERE
        !
        WHERE (TOP%XZS <= XZS_LS)
          !
          ZDW(:) = ZGRADX * (TOP%XZS - XZS_LS)
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (TOP%XZS - ZZSFREEZE)
          !
        END WHERE
        !
      END WHERE
      !
      ZWGTOT(:) = XUNDEF
      !
      WHERE(TGR%CUR%XWG(:,JL)/=XUNDEF)         
        ZWGTOT(:) = TGR%CUR%XWG(:,JL) + TGR%CUR%XWGI(:,JL)
      ENDWHERE 
      !
      WHERE(TGR%CUR%XWG(:,JL)/=XUNDEF)      
        TGR%CUR%XWGI(:,JL) = TGR%CUR%XWGI(:,JL) + ZDW(:)
        TGR%CUR%XWG (:,JL) = TGR%CUR%XWG (:,JL) - ZDW(:)
      ENDWHERE      
      !
      WHERE (TGR%CUR%XWGI(:,JL) < 0..AND.TGR%CUR%XWGI(:,JL)/=XUNDEF) 
        TGR%CUR%XWGI(:,JL) = 0.
        TGR%CUR%XWG (:,JL) = ZWGTOT(:)
      END WHERE
      !
      WHERE (TGR%CUR%XWG(:,JL) < XWGMIN.AND.TGR%CUR%XWG(:,JL)/=XUNDEF)
        TGR%CUR%XWG (:,JL) = XWGMIN
        TGR%CUR%XWGI(:,JL) = ZWGTOT(:) - XWGMIN
      END WHERE
      !
      WHERE(TGR%CUR%XWGI(:,JL) > 0..AND.TGR%CUR%XWGI(:,JL)/=XUNDEF)
        TGR%CUR%XTG(:,JL) = MIN(XTT,TGR%CUR%XTG(:,JL))
      ELSEWHERE
        TGR%CUR%XTG(:,JL) = MAX(XTT,TGR%CUR%XTG(:,JL))
      ENDWHERE
      !
    ENDWHERE
    !
  END DO
  !
!
!
DEALLOCATE(ZZSFREEZE     )
DEALLOCATE(ZWGI_CLIM_GRAD)
DEALLOCATE(ZWGTOT        )
DEALLOCATE(ZDW           )
!
!* masks where fields are not defined
WHERE (TGR%CUR%XTG(:,1:SIZE(TGR%CUR%XWG,2)) == XUNDEF)
  TGR%CUR%XWG (:,:) = XUNDEF
  TGR%CUR%XWGI(:,:) = XUNDEF
END WHERE
!
!-------------------------------------------------------------------------------------
!
IDEEP_SOIL = TGRO%NLAYER_GR
 CALL PREP_VER_SNOW(TGR%CUR%TSNOW,XZS_LS,TOP%XZS,SPREAD(ZTG_LS,3,1),SPREAD(TGR%CUR%XTG,3,1),IDEEP_SOIL)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Deallocation of large-scale orography
!
DEALLOCATE(ZTG_LS)
IF (LHOOK) CALL DR_HOOK('PREP_VER_TEB_GREENROOF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_TEB_GREENROOF
