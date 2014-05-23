!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ################################################################################
SUBROUTINE SSO_Z0_FRICTION_n(PSEA,PUREF,PRHOA,PU,PV,PPEW_A_COEF,PPEW_B_COEF,PSFU,PSFV)
!     ################################################################################
!
!!****  *SSO_Z0_FRICTION_n * - Computes subgrid-scale orography friction
!                                  according to several options:
!                                CROUGH='Z01D' : orographic roughness length
!                                CROUGH='Z04D' : orographic roughness length
!                                                variable with wind direction
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
!!      Original    05/2010
!!      E. Martin   01/2012 Correction masque (compatibilité XUNDEF)
!!      B. Decharme 09/2012 new wind implicitation and sea fraction
!!      J. Escobar  05/2014 for bug with ifort/10, replace WHERE this IF
!----------------------------------------------------------------
!
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SURF_ATM,       ONLY : CIMPLICIT_WIND
USE MODD_CSTS,           ONLY : XKARMAN, XPI
USE MODD_SURF_ATM_SSO_n, ONLY : CROUGH, XZ0EFFJPDIR, XZ0REL, XFRACZ0,      &
                                XZ0EFFIP, XZ0EFFIM, XZ0EFFJP, XZ0EFFJM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)    :: PSEA      ! Sea fraction                          (-)
REAL, DIMENSION(:), INTENT(IN)    :: PUREF     ! Wind forcing height                   (m)
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(:), INTENT(IN)    :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(:), INTENT(IN)    :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(:), INTENT(IN)    :: PPEW_A_COEF! implicit coefficients                (m2s/kg)
REAL, DIMENSION(:), INTENT(IN)    :: PPEW_B_COEF! needed if HCOUPLING='I'              (m/s)
REAL, DIMENSION(:), INTENT(INOUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(:), INTENT(INOUT) :: PSFV      ! meridian momentum flux                (Pa)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PU))    :: ZWIND   ! wind strength (m/s)
REAL, DIMENSION(SIZE(PU))    :: ZDIR    ! wind direction (rad., clockwise)
REAL, DIMENSION(SIZE(PU))    :: ZALFA   ! angle between z0eff J axis and wind direction (rad., clockwise) 
REAL, DIMENSION(SIZE(PU))    :: ZCOS2, ZSIN2
REAL, DIMENSION(SIZE(PU))    :: ZZ0EFF  ! Momentum Roughness length
REAL, DIMENSION(SIZE(PU))    :: ZCD     ! drag coefficient
REAL, DIMENSION(SIZE(PU))    :: ZUSTAR2 ! square of friction velocity
REAL, DIMENSION(SIZE(PU))    :: ZSSO_SFU! zonal orographic momentum flux
REAL, DIMENSION(SIZE(PU))    :: ZSSO_SFV! meridian orographic momentum flux
LOGICAL, DIMENSION(SIZE(PU)) :: GMASK   ! mask where SSO exists

INTEGER                      :: II
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
!
!*      1.     roughness length formalism
!              --------------------------

!* wind strength
!
  IF (LHOOK) CALL DR_HOOK('SSO_Z0_FRICTION_N',0,ZHOOK_HANDLE)
  ZWIND(:) = SQRT(PU(:)**2+PV(:)**2)
!
!* wind direction
!
  ZDIR(:) = 0.
  WHERE (ZWIND(:)>0.)  ZDIR(:)=ATAN2(PU(:),PV(:))
!
!* default value
!
  GMASK(:)=(PSEA(:)/=1..AND. XZ0REL(:)/=0.)
  ZZ0EFF(:) = XUNDEF
!
!*      2.     Constant orographic roughness length
!              ------------------------------------
!
IF (CROUGH=="Z01D") ZZ0EFF(:) = XZ0REL(:)
!
!*      3.     Directionnal roughness length
!              -----------------------------
!
IF (CROUGH=="Z04D") THEN
   DO II=1,SIZE(GMASK)
      IF (GMASK(II)) THEN
         !
         ZALFA(II) = ZDIR(II) - XZ0EFFJPDIR(II) * XPI/180.
         !
         IF    (ZALFA(II)<=-XPI) THEN
            ZALFA(II) = ZALFA(II) + 2.*XPI
         ELSEIF(ZALFA(II)>  XPI) THEN
            ZALFA(II) = ZALFA(II) - 2.*XPI
         END IF
         !
         IF (ZALFA(II)>=-XPI.AND.ZALFA(II)<=XPI) THEN
            !
            ZSIN2(II) = SIN(ZALFA(II))**2
            ZCOS2(II) = COS(ZALFA(II))**2
            !
            IF (ZALFA(II)<0.) THEN
               ZZ0EFF(II)=XZ0EFFIM(II)*ZSIN2(II)
            ELSE
               ZZ0EFF(II)=XZ0EFFIP(II)*ZSIN2(II)
            END IF
            !
            IF (ZALFA(II)>=-XPI/2. .AND. ZALFA(II)<XPI/2.) THEN
               ZZ0EFF(II) = ZZ0EFF(II) + XZ0EFFJP(II)*ZCOS2(II)
            ELSE
               ZZ0EFF(II) = ZZ0EFF(II) + XZ0EFFJM(II)*ZCOS2(II)
            END IF
            !
         END IF
         !    
      END IF
   END DO
ENDIF
!
!*      4.     Friction coefficient
!              --------------------
!
ZCD    (:) = 0.
ZUSTAR2(:) = 0.
!
GMASK(:)=(GMASK(:).AND.ZZ0EFF(:)>0.)
!
WHERE (GMASK(:))
!
!* sets a limit to roughness length
  ZZ0EFF(:) = MIN(ZZ0EFF(:),PUREF(:)/XFRACZ0)
!
! neutral case
  ZCD(:) = (XKARMAN/LOG(PUREF(:)/ZZ0EFF(:)))**2
!
END WHERE
!
!*      5.     Friction due to orography
!              -------------------------
!
! Modify flux-form implicit coupling coefficients:
!
IF(CIMPLICIT_WIND=='OLD')THEN
! old implicitation
  ZUSTAR2(:) =  ZCD(:)*ZWIND(:)*PPEW_B_COEF(:)   &
             / (1.0-PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:))
ELSE
! new implicitation
  ZUSTAR2(:) = (ZCD(:)*ZWIND(:)*(2.*PPEW_B_COEF(:)-ZWIND(:))   )   &
             / (1.0-2.0*PRHOA(:)*ZCD(:)*ZWIND(:)*PPEW_A_COEF(:))
ENDIF
!
WHERE (GMASK(:))
!
  ZWIND(:) = PRHOA(:)*PPEW_A_COEF(:)*ZUSTAR2(:) + PPEW_B_COEF(:)
  ZWIND(:) = MAX(ZWIND(:),0.)
!
  WHERE(PPEW_A_COEF(:)/= 0.)
    ZUSTAR2(:) = MAX( ( ZWIND(:) - PPEW_B_COEF(:) ) / (PRHOA(:)*PPEW_A_COEF(:)), 0.)
  ENDWHERE
!
END WHERE
!
!*      6.     Projection of friction on wind components
!              -----------------------------------------
!
ZSSO_SFU (:) = 0.
ZSSO_SFV (:) = 0.
WHERE (ZWIND(:)>0.)
  ZSSO_SFU (:) = - PU(:)/ZWIND(:) * ZUSTAR2(:) * PRHOA(:)
  ZSSO_SFV (:) = - PV(:)/ZWIND(:) * ZUSTAR2(:) * PRHOA(:)
END WHERE
!
!*      7.     Adds orographic friction to other sources of friction
!              -----------------------------------------------------
!
PSFU(:) = PSFU(:) + ZSSO_SFU(:) * (1.0-PSEA(:))
PSFV(:) = PSFV(:) + ZSSO_SFV(:) * (1.0-PSEA(:))
!
IF (LHOOK) CALL DR_HOOK('SSO_Z0_FRICTION_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE SSO_Z0_FRICTION_n
