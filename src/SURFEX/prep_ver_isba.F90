!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_ISBA
!     #################################################################################
!
!!****  *PREP_VER_ISBA* - change in ISBA fields due to altitude change
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
!!      Modified by B. Decharme  (01/2009), Optional Arpege deep soil temperature initialization
!!      S. Riette   04/2010 Modification of XTG corrections after freezing
!!------------------------------------------------------------------
!

!
USE MODD_ISBA_n,          ONLY : XZS, XTG, XWG, XWGI, XWSAT, TSNOW, &
                                   CISBA, XDG, NGROUND_LAYER,         &
                                   LTEMP_ARP, NTEMPLAYER_ARP  
USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP,           ONLY : XZS_LS, XT_CLIM_GRAD
USE MODD_PREP_ISBA,      ONLY : LSNOW_IDEAL
USE MODD_CSTS,           ONLY : XTT, XDAY, XLMTT, XRHOLW
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
INTEGER                         :: JP        ! loop counter on patches
INTEGER                         :: IWORK     ! Work integer
!
REAL, DIMENSION(:), ALLOCATABLE :: ZWGTOT    ! total water content
REAL, DIMENSION(:), ALLOCATABLE :: ZDW       ! variation of water in soil
REAL, DIMENSION(:), ALLOCATABLE :: ZZSFREEZE ! altitude where soil temperature equals XTT
INTEGER                         :: IDEEP_SOIL! layer corresponding to deep soil temperature
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWGI_CLIM_GRAD ! ice content vertical gradient
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTG_LS! temperature on initial orography
!
REAL                            :: ZGRADX = 5.E-4 ! slope of ice content gradient
REAL                            :: ZH0    = 5.E-1 ! constant used to define ice content gradient
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.0    Ice content climatologic gradient
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_ISBA',0,ZHOOK_HANDLE)
ALLOCATE(ZWGI_CLIM_GRAD (SIZE(XWG,1),SIZE(XWG,2),SIZE(XWG,3)))
!
ZWGI_CLIM_GRAD(:,:,:) = ZGRADX * EXP( - XDG(:,:,:) / ZH0 )
!-------------------------------------------------------------------------------------
!
!*      1.1    Temperature profile
!
ALLOCATE(ZTG_LS(SIZE(XTG,1),SIZE(XTG,2),SIZE(XTG,3)))
ZTG_LS(:,:,:) = XTG(:,:,:)
!
DO JP=1,SIZE(XTG,3)
  DO JL=1,SIZE(XTG,2)
    WHERE(XTG(:,JL,JP)/=XUNDEF) &
      XTG(:,JL,JP) = XTG(:,JL,JP) + XT_CLIM_GRAD  * (XZS - XZS_LS)  
  END DO
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
IF(LTEMP_ARP)THEN
  IWORK=SIZE(XWG,2)
ELSE
  IWORK=SIZE(XTG,2)
ENDIF
!
DO JP=1,SIZE(XWG,3)
  !
  DO JL=1,IWORK
    !
    ZDW(:) = 0.
    ! altitude where deep soil freezes (diurnal surface response is not treated)
    ZZSFREEZE(:) = XZS + (XTT - XTG(:,JL,JP)) / XT_CLIM_GRAD
    !
    WHERE(XTG(:,JL,JP)/=XUNDEF) 
      !
      WHERE (ZTG_LS(:,JL,JP) < XTT)
        !
        WHERE (XZS <= XZS_LS)
          !
          WHERE (XZS > ZZSFREEZE) 
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL,JP) * (XZS - XZS_LS)
          ELSEWHERE
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL,JP) * (ZZSFREEZE - XZS_LS) + ZGRADX * (XZS - ZZSFREEZE)
          ENDWHERE
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL,JP) * (XZS - XZS_LS)
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
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL,JP) * (XZS - ZZSFREEZE)
          !
        END WHERE
        !
      END WHERE 
      !
      ZWGTOT(:) = XUNDEF
      !
      WHERE(XWG(:,JL,JP)/=XUNDEF)        
        ZWGTOT(:) = XWG(:,JL,JP) + XWGI(:,JL,JP)
      ENDWHERE
      !
      WHERE(XWG(:,JL,JP)/=XUNDEF)        
        XWGI(:,JL,JP) = XWGI(:,JL,JP) + ZDW(:)
        XWG (:,JL,JP) = XWG (:,JL,JP) - ZDW(:)
      ENDWHERE
      !
      WHERE (XWGI(:,JL,JP)<0.0.AND.XWGI(:,JL,JP)/=XUNDEF) 
        XWGI(:,JL,JP) = 0.
        XWG (:,JL,JP) = ZWGTOT(:)
      END WHERE
      !
      WHERE (XWG(:,JL,JP)<XWGMIN.AND.XWG(:,JL,JP)/=XUNDEF)
        XWG (:,JL,JP) = XWGMIN
        XWGI(:,JL,JP) = ZWGTOT(:) - XWGMIN
      END WHERE
      !
      WHERE(XWGI(:,JL,JP)>0.0.AND.XWGI(:,JL,JP)/=XUNDEF)
        XTG(:,JL,JP) = MIN(XTT,XTG(:,JL,JP))
      ELSEWHERE
        XTG(:,JL,JP) = MAX(XTT,XTG(:,JL,JP))
      ENDWHERE
      !
    END WHERE
    !
  END DO
  !
END DO
!
!* limits in force-restore case
!
IF (CISBA=='3-L') THEN 
  DO JP=1,SIZE(XWG,3)
     WHERE (XWGI(:,3,JP) /= XUNDEF)
       XWG (:,3,JP) = XWG(:,3,JP)+XWGI(:,3,JP)
       XWGI(:,3,JP) = 0.
       XTG (:,3,JP) = ZTG_LS(:,3,JP) + XT_CLIM_GRAD  * (XZS - XZS_LS)       
     END WHERE
     IF(LTEMP_ARP)THEN
        XTG (:,4:SIZE(XTG,2),JP) = ZTG_LS(:,4:SIZE(XTG,2),JP)
     ENDIF
  END DO
ELSEIF(CISBA=='2-L'.AND.LTEMP_ARP) THEN
  DO JP=1,SIZE(XWG,3)
     XTG (:,3:SIZE(XTG,2),JP) = ZTG_LS(:,3:SIZE(XTG,2),JP)
  END DO
END IF
!
DEALLOCATE(ZZSFREEZE)
DEALLOCATE(ZWGI_CLIM_GRAD)
DEALLOCATE(ZWGTOT   )
DEALLOCATE(ZDW      )
!
!* masks where fields are not defined
WHERE (XTG(:,1:SIZE(XWG,2),:) == XUNDEF)
  XWG (:,:,:) = XUNDEF
  XWGI(:,:,:) = XUNDEF
END WHERE
!
!-------------------------------------------------------------------------------------
!
!*      1.4    Snow variables
!
!* vertical shift
IF (.NOT.LSNOW_IDEAL) THEN
  IF (CISBA=='DIF') THEN
    IDEEP_SOIL = NGROUND_LAYER
  ELSE
    IDEEP_SOIL = 2
  END IF        
  CALL PREP_VER_SNOW(TSNOW,XZS_LS,XZS,ZTG_LS,XTG,IDEEP_SOIL)
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      2.     Deallocation of large-scale orography
!
DEALLOCATE(ZTG_LS)
IF (LHOOK) CALL DR_HOOK('PREP_VER_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_ISBA
