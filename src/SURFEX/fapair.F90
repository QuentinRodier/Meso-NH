!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######
SUBROUTINE FAPAIR(PABC, PIA, PLAI, PXMUS, PSSA_SUP, PSSA_INF, &
           PB_SUP, PB_INF, PALB_VEG, PALB_SOIL, OSHADE,            &
           PFAPR, PFAPR_BS, PLAI_EFF, PIACAN,                      &
           PIACAN_SHADE, PIACAN_SUNLIT, PFRAC_SUN                  )
!   #########################################################################
!
!!****  *FAPAIR*  
!!
!!    PURPOSE
!!    -------
!!    Calculates FAPAR and FAPIR of vegetation and bare soil.
!!              
!!**  METHOD
!!    ------
!!    Carrer et al. 
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    USE MODD_SURF_PAR
!!    USE MODD_CSTS
!!    USE MODI_CCETR_PAIR
!!
!!    REFERENCE
!!    ---------
!!     Carrer et al. ??
!!      
!!    AUTHOR
!!    ------
!!	D. Carrer          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/04/2011 
!!
!-------------------------------------------------------------------------------
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XI0
USE MODD_CO2V_PAR,   ONLY : XK_SUP, XK_INF, XXSI_SUP, XXSI_INF 
!
USE MODI_CCETR_PAIR  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!*       0.     DECLARATIONS
!               ------------
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN) :: PABC    ! abscissa needed for integration
!                                         ! of net assimilation and stomatal
!                                         ! conductance over canopy depth
REAL, DIMENSION(:), INTENT(IN) :: PIA     ! PIA   = absorbed PAR / PIR	                             
REAL, DIMENSION(:), INTENT(IN) :: PLAI    ! PLAI  = leaf area index
REAL, DIMENSION(:), INTENT(IN) :: PXMUS   ! cosine of solar zenith angle
REAL,               INTENT(IN) :: PSSA_SUP, PSSA_INF
REAL, DIMENSION(:), INTENT(IN) :: PB_SUP, PB_INF 
REAL, DIMENSION(:), INTENT(IN) :: PALB_VEG, PALB_SOIL
LOGICAL, DIMENSION(:), INTENT(IN) :: OSHADE   ! OSHADE = if 1 shading activated
!
REAL, DIMENSION(:), INTENT(OUT) :: PFAPR
REAL, DIMENSION(:), INTENT(OUT) :: PFAPR_BS
REAL, DIMENSION(:), OPTIONAL,   INTENT(OUT) :: PLAI_EFF
!
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: PIACAN ! PAR in the canopy at different gauss level
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: PIACAN_SHADE  ! PAR in the canopy at different gauss level
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: PIACAN_SUNLIT ! PAR in the canopy at different gauss level
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT) :: PFRAC_SUN     ! fraction of sunlit leaves
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PLAI))   :: ZXIA, ZXIA_SUP, ZKMUSP_SUP, ZKMUSP_INF
REAL, DIMENSION(SIZE(PLAI))   :: ZB_DR_SUP, ZB_DR_INF, ZOMEGA_DR_SUP, ZOMEGA_DR_INF, &
                                 ZOMEGA_DF_SUP, ZOMEGA_DF_INF
!                                ZXIA  = abs. radiation of vegetation
REAL, DIMENSION(SIZE(PLAI))   :: ZTR, ZFD_SKY, ZFD_VEG, ZFD_SUP, ZLAI_EFF0, ZLAI_EFF
!                                ZTR = transmittance       
!REAL, DIMENSION(SIZE(PLAI))    :: ZXIA_SUNLIT, ZXIA_SHADE, ZLAI_SUNLIT, ZLAI_SHADE
!                                 ZXIA_SUNLIT = absorbed PAR of sunlit leaves
!                                 ZXIA_SHADE = absorbed PAR of shaded leaves
!                                 ZLAI_SUNLIT = LAI of sunlit leaves
!                                 ZLAI_SHADE = LAI of shaded leaves
!REAL, DIMENSION(SIZE(PLAI))    :: ZRN_SUNLIT, ZRN_SHADE
REAL, DIMENSION(SIZE(PLAI),SIZE(PABC)) :: ZIACAN, ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN
REAL                           :: ZABC, ZWEIGHT, ZCOEF,  ZRATIO, ZTAU, ZSUP, ZINF, &
                                  ZSSA_SUP, ZSSA_INF, ZB_DF_SUP, ZB_DF_INF
!                                            ZABC    = abscissa needed for integration
!                                                     of net assimilation and stomatal 
!                                                     conductance over canopy depth 
!                                                     (working scalar)
INTEGER                        :: JINT, I ! index for loops
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FAPAIR',0,ZHOOK_HANDLE)
!
! initialisation
!
ZTR(:)      = 1.0
!
ZXIA_SUP(:) = 0.
!
ZFD_SKY(:) = 0.
ZFD_VEG(:)  = 0.
ZFD_SUP(:)  = 0.
!
!ZXIA_SUNLIT(:) = 0.
!ZXIA_SHADE(:)  = 0.
!ZLAI_SUNLIT(:) = 0.
!ZLAI_SHADE(:)  = 0.
!
ZLAI_EFF(:) = 0.
!
ZIACAN(:,:)        = 0.
ZIACAN_SUNLIT(:,:) = 0.
ZIACAN_SHADE(:,:)  = 0.
ZFRAC_SUN(:,:)     = 0.
!
PFAPR(:)    = 0.
PFAPR_BS(:) = 0.
!PRN_SHADE(:) = 0.
!PRN_SUNLIT(:) = 0.
!
!
ZTAU = EXP(-0.1) !  AOD is arbitrary fixed to low value...
!
! the global radiation is estimated as a fraction of 
DO I=1,SIZE(PIA)
  IF (PIA(I) > 0.) THEN
    ! estimate fraction of diffuse radiation by Erbs (1982)
    ! 0.48 : factor to convert from the shortwave to the  PAR band [0.4-0.7µm]
    ZRATIO = PIA(I)/0.48/XI0/PXMUS(I)
    IF (ZRATIO < 0.22) THEN
      ZFD_SKY(I) = (1 - 0.09*ZRATIO)
    ELSE IF (ZRATIO < 0.8) THEN
      ZFD_SKY(I) = (0.9511 + (-0.1604 + (4.388 + (-16.64 + 12.34*ZRATIO)*ZRATIO)*ZRATIO)*ZRATIO)
    ELSE
      !!$ PXFD_SKY(I) = PIA(I)*0.165  ! original Erbs formulation
      !if clear sky, the diffuse fraction depends on aerosol load
      ZFD_SKY(I) = (1. - ZTAU) /(1. - (1.-PXMUS(I))*ZTAU)
    ENDIF
  ENDIF
END DO
!
IF (PABC(SIZE(PABC)).GT.0.8) ZFD_VEG(:) = MIN(ZFD_SKY(:),1.)
!
! set param sup / inf
!
ZSSA_SUP = SQRT(1.-PSSA_SUP)
ZSSA_INF = SQRT(1.-PSSA_INF)
!
ZSUP = - 0.461 * XXSI_SUP + 3.8
ZINF = - 0.461 * XXSI_INF + 3.8
!
DO I=1,SIZE(PIA)
  IF (PIA(I).NE.0.) THEN
    ZKMUSP_SUP(I) = EXP(-XK_SUP*(ACOS(PXMUS(I)))**ZSUP)
    ZKMUSP_INF(I) = EXP(-XK_INF*(ACOS(PXMUS(I)))**ZINF)
    ! direct case
    ! Directional albedo of upper/lower layer
    ZB_DR_SUP(I) = 1.-(1.-ZSSA_SUP)/(1.+2.*PXMUS(I)*ZSSA_SUP) 
    ZB_DR_INF(I) = 1.-(1.-ZSSA_SUP)/(1.+2.*PXMUS(I)*ZSSA_INF)
    ! CLUMPING INDEX 
    ZOMEGA_DR_SUP(I) = 1. / (1.+ PB_SUP(I)*ZKMUSP_SUP(I))
    ZOMEGA_DR_INF(I) = 1. / (1.+ PB_INF(I)*ZKMUSP_INF(I))
    ! diffus case
    ! CLUMPING INDEX
    ZOMEGA_DF_SUP(I) = (1.+PB_SUP(I)/2.)/(1.+PB_SUP(I))
    ZOMEGA_DF_INF(I) = (1.+PB_INF(I)/2.)/(1.+PB_INF(I))
  ENDIF
ENDDO
!
ZB_DF_SUP = 1.-(1.-ZSSA_SUP)/(1.+ ZSSA_SUP)
ZB_DF_INF = 1.-(1.-ZSSA_INF)/(1.+ ZSSA_INF)
!
! Integration over the canopy: SIZE(PABC) increments
! are used to approximate the integral. And to calculate 
! absorded fluxes within the canopy and in the bare soil  
DO JINT = SIZE(PABC),1,-1
!
  ZABC = 1.
  IF (JINT.LT.SIZE(PABC)) ZABC = PABC(JINT+1)
  ZWEIGHT = ZABC - PABC(JINT)
  !
  IF (PABC(JINT).GT.0.8) THEN
    !  Compute transmittance of each level  
    CALL CCETR_PAIR (JINT, PABC(JINT), ZABC, PIA, PXMUS, ZB_DR_SUP, &
                     ZOMEGA_DR_SUP, ZOMEGA_DF_SUP, ZB_DF_SUP, PLAI, &
                     PALB_VEG, PALB_SOIL, ZFD_SKY, ZFD_VEG, ZTR,    &
                     ZXIA, ZLAI_EFF0              )
  ELSE
    CALL CCETR_PAIR (JINT, PABC(JINT), ZABC, PIA, PXMUS, ZB_DR_INF, &
                     ZOMEGA_DR_INF, ZOMEGA_DF_INF, ZB_DF_INF, PLAI, &
                     PALB_VEG, PALB_SOIL, ZFD_SKY, ZFD_VEG, ZTR,    &
                     ZXIA, ZLAI_EFF0              )
  ENDIF          
  ! 
  DO I=1,SIZE(PIA)
    !
    ZXIA(I)        = MAX(0.,ZXIA(I))
    ZIACAN(I,JINT) = MAX(0.,ZXIA(I)-ZXIA_SUP(I))
    ZXIA_SUP(I)    = ZXIA(I)
    !
    ZLAI_EFF0(I) = MAX(0.,ZLAI_EFF0(I))
    ZLAI_EFF(I)  = ZLAI_EFF(I) + ZLAI_EFF0(I)
    !
    !calculate a FAPAR/FAPIR of the entire canopy
    PFAPR(I)  = PFAPR(I) + ZIACAN(I,JINT)
    !
    !------------------------------------------------------
    ! If LSHADE=0 no shading, only sunlit leaves
    ! If LSHADE=1 shading
    ! PIACAN is used to calculate An of each level within the canopy in cotwores
    ! ZIACAN_SUNLIT used for net assimilation of a sunlit leave in COTWO
    ! ZIACAN_SHADE used  in A-gs for net assimilation of a shaded leave in COTWO
    IF (OSHADE(I)) THEN
      !
      !sunlit leaves
      !absorbed PAR of an equivalent canopy representative of the layer of leaves
      ZCOEF = (1.0-ZFD_SUP(I))/ZTR(I)+ ZFD_SUP(I)
      ZIACAN_SUNLIT(I,JINT) =             ZCOEF/(ZWEIGHT*MAX(0.0001,PLAI(I)))*ZIACAN(I,JINT)    
      !not sunlit leaves
      ZIACAN_SHADE(I,JINT)  = MAX(0.,ZFD_SUP(I)/(ZWEIGHT*MAX(0.0001,PLAI(I)))*ZIACAN(I,JINT))
      !
      !ZXIA_SUNLIT(I) = ZXIA_SUNLIT(I) + ZWEIGHT*ZTR(I)      *ZIACAN_SUNLIT(I,JINT)
      !ZLAI_SUNLIT(I) = ZLAI_SUNLIT(I) + ZWEIGHT*ZTR(I)*ZCOEF*PLAI(I)
      !
      !ZXIA_SHADE(I)  = ZXIA_SHADE(I)  + ZWEIGHT*(1-ZTR(I))           *ZIACAN_SHADE(I,JINT)
      !ZLAI_SHADE(I)  = ZLAI_SHADE(I)  + ZWEIGHT*(1-ZTR(I))*ZFD_SUP(I)*PLAI(I)
      !
      ZFRAC_SUN(I,JINT) = ZTR(I)  !fraction of sunlit leaves
      !      
    ELSE
      !
      ZIACAN_SUNLIT(I,JINT) = MAX(0.,ZIACAN(I,JINT)/(ZWEIGHT*MAX(0.0001,PLAI(I))))
      !ZLAI_SUNLIT(I) = ZLAI_SUNLIT(I) + ZWEIGHT*PLAI(I)
      !
    ENDIF
    !
    ZFD_SUP(I) = ZFD_VEG(I)
    !
    ENDDO
  !
END DO
!
!
WHERE (PIA(:).NE.0.)
  PFAPR(:) = PFAPR(:) / PIA(:)
  PFAPR_BS(:)=(1.-PALB_VEG(:))*(1-PALB_SOIL(:))*(1.+PALB_VEG(:)*PALB_SOIL(:))*ZTR(:)
  WHERE (PLAI(:).EQ.0) PFAPR_BS(:) = 1-PALB_SOIL(:)
END WHERE
!
!WHERE (ZLAI_SHADE(:) .NE.0.) ZRN_SHADE(:)  = ZXIA_SHADE(:) / ZLAI_SHADE(:)
!WHERE (ZLAI_SUNLIT(:).NE.0.) ZRN_SUNLIT(:) = ZXIA_SUNLIT(:)/ ZLAI_SUNLIT(:)
!
IF (PRESENT(PLAI_EFF))      PLAI_EFF      = ZLAI_EFF
IF (PRESENT(PIACAN))        PIACAN        = ZIACAN
IF (PRESENT(PIACAN_SUNLIT)) PIACAN_SUNLIT = ZIACAN_SUNLIT
IF (PRESENT(PIACAN_SHADE))  PIACAN_SHADE  = ZIACAN_SHADE
IF (PRESENT(PFRAC_SUN))     PFRAC_SUN     = ZFRAC_SUN
!
IF (LHOOK) CALL DR_HOOK('FAPAIR',1,ZHOOK_HANDLE)
!
END SUBROUTINE FAPAIR
