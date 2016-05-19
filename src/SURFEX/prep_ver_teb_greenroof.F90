!     #########
SUBROUTINE PREP_VER_TEB_GREENROOF
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
USE MODD_TEB_n,             ONLY : XZS
USE MODD_TEB_GREENROOF_n,   ONLY : XTG, XWG, XWGI, XWSAT, TSNOW, &
                                   XDG, NLAYER_GR
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
ALLOCATE(ZWGI_CLIM_GRAD (SIZE(XWG,1),SIZE(XWG,2)))
!
ZWGI_CLIM_GRAD(:,:) = ZGRADX * EXP( - XDG(:,:) / ZH0 )
!-------------------------------------------------------------------------------------
!
!*      1.1    Temperature profile
!
ALLOCATE(ZTG_LS(SIZE(XTG,1),SIZE(XTG,2)))
ZTG_LS(:,:) = XTG(:,:)
!
  DO JL=1,SIZE(XTG,2)
    WHERE(XTG(:,JL)/=XUNDEF) &
      XTG(:,JL) = XTG(:,JL) + XT_CLIM_GRAD  * (XZS - XZS_LS)  
  END DO
!
!-------------------------------------------------------------------------------------
!
!*      1.2    Water and ice in the soil
!
ALLOCATE(ZZSFREEZE      (SIZE(XWG,1)))
ALLOCATE(ZWGTOT         (SIZE(XWG,1)))
ALLOCATE(ZDW            (SIZE(XWG,1)))
!
!* general case
!
IWORK=SIZE(XTG,2)
!
  !
  DO JL=1,IWORK
    !
    ZDW(:) = 0.
    ! altitude where deep soil freezes (diurnal surface response is not treated)
    ZZSFREEZE(:) = XZS + (XTT - XTG(:,JL)) / XT_CLIM_GRAD
    !
    WHERE(XTG(:,JL)/=XUNDEF) 
      !
      WHERE (ZTG_LS(:,JL) < XTT)
        !
        WHERE (XZS <= XZS_LS)
          !
          WHERE (XZS > ZZSFREEZE) 
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (XZS - XZS_LS)
          ELSEWHERE
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (ZZSFREEZE - XZS_LS) + ZGRADX * (XZS - ZZSFREEZE)
          ENDWHERE
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (XZS - XZS_LS)
          !
        ENDWHERE
        !
      ELSEWHERE
        !
        WHERE (XZS <= XZS_LS)
          !
          ZDW(:) = ZGRADX * (XZS - XZS_LS)
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL) * (XZS - ZZSFREEZE)
          !
        END WHERE
        !
      END WHERE
      !
      ZWGTOT(:) = XUNDEF
      !
      WHERE(XWG(:,JL)/=XUNDEF)         
        ZWGTOT(:) = XWG(:,JL) + XWGI(:,JL)
      ENDWHERE 
      !
      WHERE(XWG(:,JL)/=XUNDEF)      
        XWGI(:,JL) = XWGI(:,JL) + ZDW(:)
        XWG (:,JL) = XWG (:,JL) - ZDW(:)
      ENDWHERE      
      !
      WHERE (XWGI(:,JL) < 0..AND.XWGI(:,JL)/=XUNDEF) 
        XWGI(:,JL) = 0.
        XWG (:,JL) = ZWGTOT(:)
      END WHERE
      !
      WHERE (XWG(:,JL) < XWGMIN.AND.XWG(:,JL)/=XUNDEF)
        XWG (:,JL) = XWGMIN
        XWGI(:,JL) = ZWGTOT(:) - XWGMIN
      END WHERE
      !
      WHERE(XWGI(:,JL) > 0..AND.XWGI(:,JL)/=XUNDEF)
        XTG(:,JL) = MIN(XTT,XTG(:,JL))
      ELSEWHERE
        XTG(:,JL) = MAX(XTT,XTG(:,JL))
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
WHERE (XTG(:,1:SIZE(XWG,2)) == XUNDEF)
  XWG (:,:) = XUNDEF
  XWGI(:,:) = XUNDEF
END WHERE
!
!-------------------------------------------------------------------------------------
!
IDEEP_SOIL = NLAYER_GR
 CALL PREP_VER_SNOW(TSNOW,XZS_LS,XZS,SPREAD(ZTG_LS,3,1),SPREAD(XTG,3,1),IDEEP_SOIL)
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
