!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_ISBA (I)
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
!!      Y. Seity    02/2016 Add limits in Force-Restore case (WG2 contains WG1)
!!------------------------------------------------------------------
!

!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
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
!
TYPE(ISBA_t), INTENT(INOUT) :: I
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
ALLOCATE(ZWGI_CLIM_GRAD (SIZE(I%XWG,1),SIZE(I%XWG,2),SIZE(I%XWG,3)))
!
ZWGI_CLIM_GRAD(:,:,:) = ZGRADX * EXP( - I%XDG(:,:,:) / ZH0 )
!-------------------------------------------------------------------------------------
!
!*      1.1    Temperature profile
!
ALLOCATE(ZTG_LS(SIZE(I%XTG,1),SIZE(I%XTG,2),SIZE(I%XTG,3)))
ZTG_LS(:,:,:) = I%XTG(:,:,:)
!
DO JP=1,SIZE(I%XTG,3)
  DO JL=1,SIZE(I%XTG,2)
    WHERE(I%XTG(:,JL,JP)/=XUNDEF) &
      I%XTG(:,JL,JP) = I%XTG(:,JL,JP) + XT_CLIM_GRAD  * (I%XZS - XZS_LS)  
  END DO
END DO
!
!-------------------------------------------------------------------------------------
!
!*      1.2    Water and ice in the soil
!
ALLOCATE(ZZSFREEZE      (SIZE(I%XWG,1)))
ALLOCATE(ZWGTOT         (SIZE(I%XWG,1)))
ALLOCATE(ZDW            (SIZE(I%XWG,1)))
!
!* general case
!
IF(I%LTEMP_ARP)THEN
  IWORK=SIZE(I%XWG,2)
ELSE
  IWORK=SIZE(I%XTG,2)
ENDIF
!
DO JP=1,SIZE(I%XWG,3)
  !
  DO JL=1,IWORK
    !
    ZDW(:) = 0.
    ! altitude where deep soil freezes (diurnal surface response is not treated)
    ZZSFREEZE(:) = I%XZS + (XTT - I%XTG(:,JL,JP)) / XT_CLIM_GRAD
    !
    WHERE(I%XTG(:,JL,JP)/=XUNDEF) 
      !
      WHERE (ZTG_LS(:,JL,JP) < XTT)
        !
        WHERE (I%XZS <= XZS_LS)
          !
          WHERE (I%XZS > ZZSFREEZE) 
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL,JP) * (I%XZS - XZS_LS)
          ELSEWHERE
            ZDW(:) = ZWGI_CLIM_GRAD(:,JL,JP) * (ZZSFREEZE - XZS_LS) + ZGRADX * (I%XZS - ZZSFREEZE)
          ENDWHERE
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL,JP) * (I%XZS - XZS_LS)
          !
        ENDWHERE
        !
      ELSEWHERE
        !
        WHERE (I%XZS <= XZS_LS)
          !
          ZDW(:) = ZGRADX * (I%XZS - XZS_LS)
          !
        ELSEWHERE
          !
          ZDW(:) = ZWGI_CLIM_GRAD(:,JL,JP) * (I%XZS - ZZSFREEZE)
          !
        END WHERE
        !
      END WHERE 
      !
      ZWGTOT(:) = XUNDEF
      !
      WHERE(I%XWG(:,JL,JP)/=XUNDEF)        
        ZWGTOT(:) = I%XWG(:,JL,JP) + I%XWGI(:,JL,JP)
      ENDWHERE
      !
      WHERE(I%XWG(:,JL,JP)/=XUNDEF)        
        I%XWGI(:,JL,JP) = I%XWGI(:,JL,JP) + ZDW(:)
        I%XWG (:,JL,JP) = I%XWG (:,JL,JP) - ZDW(:)
      ENDWHERE
      !
      WHERE (I%XWGI(:,JL,JP)<0.0.AND.I%XWGI(:,JL,JP)/=XUNDEF) 
        I%XWGI(:,JL,JP) = 0.
        I%XWG (:,JL,JP) = ZWGTOT(:)
      END WHERE
      !
      WHERE (I%XWG(:,JL,JP)<XWGMIN.AND.I%XWG(:,JL,JP)/=XUNDEF)
        I%XWG (:,JL,JP) = XWGMIN
        I%XWGI(:,JL,JP) = ZWGTOT(:) - XWGMIN
      END WHERE
      !
      WHERE(I%XWGI(:,JL,JP)>0.0.AND.I%XWGI(:,JL,JP)/=XUNDEF)
        I%XTG(:,JL,JP) = MIN(XTT,I%XTG(:,JL,JP))
      ELSEWHERE
        I%XTG(:,JL,JP) = MAX(XTT,I%XTG(:,JL,JP))
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
IF (I%CISBA=='2-L'.OR.I%CISBA=='3-L') THEN
I%XWG(:,2,:)=MAX(I%XWG(:,1,:)*I%XDG(:,1,:),I%XWG(:,2,:)*I%XDG(:,2,:))/I%XDG(:,2,:)
I%XWGI(:,2,:)=MAX(I%XWGI(:,1,:)*I%XDG(:,1,:),I%XWGI(:,2,:)*I%XDG(:,2,:))/I%XDG(:,2,:)
ENDIF
IF (I%CISBA=='3-L') THEN 
  DO JP=1,SIZE(I%XWG,3)
     WHERE (I%XWGI(:,3,JP) /= XUNDEF)
       I%XWG (:,3,JP) = I%XWG(:,3,JP)+I%XWGI(:,3,JP)
       I%XWGI(:,3,JP) = 0.
       I%XTG (:,3,JP) = ZTG_LS(:,3,JP) + XT_CLIM_GRAD  * (I%XZS - XZS_LS)       
     END WHERE
     IF(I%LTEMP_ARP)THEN
        I%XTG (:,4:SIZE(I%XTG,2),JP) = ZTG_LS(:,4:SIZE(I%XTG,2),JP)
     ENDIF
  END DO
ELSEIF(I%CISBA=='2-L'.AND.I%LTEMP_ARP) THEN
  DO JP=1,SIZE(I%XWG,3)
     I%XTG (:,3:SIZE(I%XTG,2),JP) = ZTG_LS(:,3:SIZE(I%XTG,2),JP)
  END DO
END IF
!
DEALLOCATE(ZZSFREEZE)
DEALLOCATE(ZWGI_CLIM_GRAD)
DEALLOCATE(ZWGTOT   )
DEALLOCATE(ZDW      )
!
!* masks where fields are not defined
WHERE (I%XTG(:,1:SIZE(I%XWG,2),:) == XUNDEF)
  I%XWG (:,:,:) = XUNDEF
  I%XWGI(:,:,:) = XUNDEF
END WHERE
!
!-------------------------------------------------------------------------------------
!
!*      1.4    Snow variables
!
!* vertical shift
IF (.NOT.LSNOW_IDEAL) THEN
  IF (I%CISBA=='DIF') THEN
    IDEEP_SOIL = I%NGROUND_LAYER
  ELSE
    IDEEP_SOIL = 2
  END IF        
  CALL PREP_VER_SNOW(I%TSNOW,XZS_LS,I%XZS,ZTG_LS,I%XTG,IDEEP_SOIL)
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
