!!   ###############################
     SUBROUTINE CH_BVOCEM_n(PSW_FORBIO,PRHOA,PSFTS)
!!   ###############################
!!
!!***  *BVOCEM*
!! 
!!    PURPOSE
!!    -------
!!    Calculate the biogenic emission fluxes according to the 
!!    subgrid vegetation given by the soil interface
!!
!!    METHOD
!!    ------
!!
!!
!!    AUTHOR
!!    ------
!!    F. Solmon (LA) & V. Masson (CNRM)
!!    
!!    MODIFICATIONS
!!    -------------
!!    Original: 25/10/00
!!    P. Tulet  30/07/03 externalisation of biogenics fluxes (2D => 1D)
!!
!!
!!    EXTERNAL
!!    --------
USE MODI_VEGTYPE_TO_PATCH
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_GR_BIOG_n 
USE MODD_BVOC_PAR
USE MODD_CSTS,ONLY : XMD, XAVOGADRO
USE MODD_CO2V_PAR
USE MODD_SURF_PAR,ONLY:XUNDEF
USE MODD_ISBA_n, ONLY : NPATCH, CPHOTO, XPATCH, XVEGTYPE, XABC, XPOI, XTG
USE MODD_ISBA_PAR
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, NVT_TREE, NVT_CONI, NVT_EVER, &
                                NVT_GRAS, NVT_TROG, NVT_PARK, NVT_C3, NVT_C4,&
                                NVT_IRR
USE MODD_CH_ISBA_n,ONLY : NSV_CHSBEG, NSV_CHSEND, NBEQ, CSV
!!
!!
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

REAL, DIMENSION(:,:), INTENT(IN)    :: PSW_FORBIO
REAL, DIMENSION(:),   INTENT(IN)    :: PRHOA
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSFTS
!
!*       0.1  declaration of arguments
!
!*   0.1 Declaration of local variables
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1)) :: ZRAD_PAR,  ZLCOR_RAD
!                            PAR radiation in case of ISBA-STD use
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1)) :: ZFISO_FOR  , ZFMONO_FOR,   &
                                       ZFISO_GRASS, ZFMONO_GRASS, &
                                       ZFISO_CROP , ZFMONO_CROP     
!                                Fluxes coming from different landuse
REAL, DIMENSION(SIZE(PSW_FORBIO,1), NVEGTYPE) :: ZTCOR ,ZTCORM
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1),SIZE(XABC),NVEGTYPE) :: ZBVOCPAR 
!                                PAR at gauss level in micromolphot/m2/s
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1)) :: ZISOPOT, ZMONOPOT
!
INTEGER:: KNGAUSS     
!                        nbre of gauss level in integration
!                        index of patch corresponding to forest(+ligneaous)
INTEGER:: JPATCH, JSV
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N',0,ZHOOK_HANDLE)
!
!* 1. Contribution of forest and ligneous vegetation 
!   from ISOPOT and MONOPOT maps 
!   ------------------------------------------------
!
!* 1.0 Preliminary : patch index corresponding to forest
!
!2.Contribution of other types of vegetation than forest, consider the vegtype fraction in the pixel 
!------------------------------------------------------------------------------------------
!
!* 2.0 Preliminary : patch index corresponding to grassland, crops (C3+C4)
!
!1.1.1 Using ISBA_Ags explicit light attenuation 
! number of g Gauss level for the integration 
IF (CPHOTO/='NON') THEN
  KNGAUSS = SIZE(XABC)
ELSE
  !1.1.2 using isba std version 
  ZRAD_PAR (:)= 0.
  DO JPATCH = 1,NPATCH
    ZRAD_PAR (:)= ZRAD_PAR (:) +(PSW_FORBIO(:,JPATCH)*XPATCH(:,JPATCH) ) * XPARCF * 4.7 
  END DO
  ZLCOR_RAD (:) = ZLCOR_FUNC(ZRAD_PAR(:))
ENDIF
!  
!
 CALL BY_PATCH(NVT_TREE, ZTCOR(:,NVT_TREE), ZTCORM(:,NVT_TREE))
 CALL BY_PATCH(NVT_CONI, ZTCOR(:,NVT_CONI), ZTCORM(:,NVT_CONI))
 CALL BY_PATCH(NVT_EVER, ZTCOR(:,NVT_EVER), ZTCORM(:,NVT_EVER))
 CALL BY_PATCH(NVT_GRAS, ZTCOR(:,NVT_GRAS), ZTCORM(:,NVT_GRAS))
 CALL BY_PATCH(NVT_TROG, ZTCOR(:,NVT_TROG), ZTCORM(:,NVT_TROG))
 CALL BY_PATCH(NVT_PARK, ZTCOR(:,NVT_PARK), ZTCORM(:,NVT_PARK))
 CALL BY_PATCH(NVT_C3  , ZTCOR(:,NVT_C3)  , ZTCORM(:,NVT_C3)  )
 CALL BY_PATCH(NVT_C4  , ZTCOR(:,NVT_C4)  , ZTCORM(:,NVT_C4)  )
 CALL BY_PATCH(NVT_IRR , ZTCOR(:,NVT_IRR) , ZTCORM(:,NVT_IRR) )
!
!
ZISOPOT (:) = XISOPOT (:) / (XVEGTYPE(:,NVT_TREE) + XVEGTYPE(:,NVT_CONI) + XVEGTYPE(:,NVT_EVER))
ZMONOPOT(:) = XMONOPOT(:) / (XVEGTYPE(:,NVT_TREE) + XVEGTYPE(:,NVT_CONI) + XVEGTYPE(:,NVT_EVER)) 
 CALL BY_VEG(NVT_TREE, NVT_CONI, NVT_EVER, ZISOPOT, ZMONOPOT, ZFISO_FOR, ZFMONO_FOR)
!
ZISOPOT (:) = XISOPOT_GRASS
ZMONOPOT(:) = XMONOPOT_GRASS
 CALL BY_VEG(NVT_GRAS, NVT_TROG, NVT_PARK, ZISOPOT, ZMONOPOT, ZFISO_GRASS, ZFMONO_GRASS)
!
ZISOPOT (:) = XISOPOT_CROP
ZMONOPOT(:) = XMONOPOT_CROP
 CALL BY_VEG(NVT_C3, NVT_C4, NVT_IRR, ZISOPOT, ZMONOPOT, ZFISO_CROP, ZFMONO_CROP)
!
!---------------------------------------------------------------------------------------
!
!3.Summation of different contribution for fluxes 
!------------------------------------------------
!
!isoprene in ppp.m.s-1
XFISO (:)=(3.0012E-10/3600.) * ( ZFISO_FOR (:) + ZFISO_GRASS (:)+ ZFISO_CROP (:) ) + 1E-17
!monoterpenes
XFMONO(:)=(1.5006E-10/3600.) * ( ZFMONO_FOR(:) + ZFMONO_GRASS(:)+ ZFMONO_CROP(:) ) + 1E-17
!
! conversion in molecules/m2/s
!
XFISO(:)  = XFISO(:)  * XAVOGADRO * PRHOA(:) / XMD
XFMONO(:) = XFMONO(:) * XAVOGADRO * PRHOA(:) / XMD
!
DO JSV=NSV_CHSBEG,NSV_CHSEND
  IF (CSV(JSV) == "BIO") THEN
    ! RELACS CASE
    PSFTS(:,JSV) = PSFTS(:,JSV) + (XFISO(:) + XFMONO(:)) 
  ELSE IF (CSV(JSV) == "ISO" .OR. CSV(JSV) == "ISOP") THEN
    ! RACM CASE
    PSFTS(:,JSV) = PSFTS(:,JSV) + XFISO(:)  
  ELSE IF (CSV(JSV) == "API"  .OR. CSV(JSV) == "LIM" .OR. &
           CSV(JSV) == "BIOL" .OR. CSV(JSV) == "BIOH" ) THEN
    ! RACM CASE
    ! CACM or RELACS 2 CASE     
    PSFTS(:,JSV) = PSFTS(:,JSV) + 0.5 * XFMONO(:) 
  ENDIF
END DO
!
!**********************************************************************************
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N',1,ZHOOK_HANDLE)
CONTAINS
!
SUBROUTINE BY_PATCH(NVT_VEGTYPE,PTCOR,PTCORM)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: NVT_VEGTYPE
REAL, DIMENSION(:), INTENT(OUT) :: PTCOR
REAL, DIMENSION(:), INTENT(OUT) :: PTCORM
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1)) :: ZBVOCSG
REAL, DIMENSION(SIZE(PSW_FORBIO,1),SIZE(XABC)) :: ZBVOCPAR 
INTEGER:: IPATCH, JLAYER, IT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_PATCH',0,ZHOOK_HANDLE)
!
IPATCH = VEGTYPE_TO_PATCH(NVT_VEGTYPE, NPATCH)
!
PTCOR  (:) = 0.
PTCORM (:) = 0.
DO IT=1,SIZE(XTG,1)
  IF (XTG(IT,1,IPATCH).LE.1000.) THEN
    PTCORM(IT)=ZTCORM0_FUNC(XTG(IT,1,IPATCH))
    PTCOR (IT)=ZTCOR0_FUNC (XTG(IT,1,IPATCH))
  ENDIF
ENDDO
!
IF (CPHOTO/='NON') THEN
  !PAR over Forest canopies, in micro-molE.m-2.s-1 
  ZBVOCPAR(:,:) = XIACAN(:,:,IPATCH)*4.7
  !Calculation of radiative attenuation effect in the canopy on correction factor
  ZBVOCSG(:) = 0.
  DO JLAYER=1,KNGAUSS
    ZBVOCSG(:) = ZBVOCSG(:) + XPOI(JLAYER) * ZLCOR_FUNC(ZBVOCPAR(:,JLAYER)) 
  ENDDO
  PTCOR(:) = PTCOR(:) * ZBVOCSG(:)
ELSE
  PTCOR(:) = PTCOR(:) * XCANFAC * ZLCOR_RAD(:)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_PATCH',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_PATCH
!--------------------------------------------------------------------------
SUBROUTINE BY_VEG(NVT_V1, NVT_V2, NVT_V3, &
                  PISOPOT, PMONOPOT, PFISO, PFMONO)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: NVT_V1
INTEGER, INTENT(IN) :: NVT_V2
INTEGER, INTENT(IN) :: NVT_V3
REAL, DIMENSION(:), INTENT(IN) :: PISOPOT
REAL, DIMENSION(:), INTENT(IN) :: PMONOPOT
REAL, DIMENSION(:), INTENT(OUT) :: PFISO
REAL, DIMENSION(:), INTENT(OUT) :: PFMONO
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG',0,ZHOOK_HANDLE)
!
!isoprene flux 
!!
!! warning, XISOPOT external map accounts for the total forest fraction
WHERE ( XVEGTYPE(:,NVT_V1) + XVEGTYPE(:,NVT_V2) + XVEGTYPE(:,NVT_V3) > 0. )
  !
  PFISO(:) = PISOPOT(:) *                   &
     ( ZTCOR(:,NVT_V1) * XVEGTYPE(:,NVT_V1) &
      +ZTCOR(:,NVT_V2) * XVEGTYPE(:,NVT_V2) &
      +ZTCOR(:,NVT_V3) * XVEGTYPE(:,NVT_V3) )
  !
  PFMONO(:) = PMONOPOT(:) *                  &
     ( ZTCORM(:,NVT_V1) * XVEGTYPE(:,NVT_V1) &
      +ZTCORM(:,NVT_V2) * XVEGTYPE(:,NVT_V2) &
      +ZTCORM(:,NVT_V3) * XVEGTYPE(:,NVT_V3) )
             
  !
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_VEG
!--------------------------------------------------------------------------
FUNCTION ZLCOR_FUNC(ZX)

REAL, DIMENSION(:)          :: ZX
REAL, DIMENSION(SIZE(ZX))   :: ZLCOR_FUNC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZLCOR_FUNC',0,ZHOOK_HANDLE)
ZLCOR_FUNC(:)=0.
ZLCOR_FUNC(:) = ZX(:)*XISO_CL*XISO_ALF/(1+(XISO_ALF**2)*(ZX(:)**2))**0.5
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZLCOR_FUNC',1,ZHOOK_HANDLE)
!
END FUNCTION ZLCOR_FUNC
!---------------------------------------------------------------------------
FUNCTION ZTCOR0_FUNC(ZX)

REAL, PARAMETER             :: R   = 8.314
REAL          :: ZX
REAL   :: ZTCOR0_FUNC
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZTCOR0_FUNC',0,ZHOOK_HANDLE)
!
ZTCOR0_FUNC=0.
ZTCOR0_FUNC = EXP(XISO_CT1*(ZX-XISO_BTS)/(R*XISO_BTS*ZX))     &
          /(1+EXP(XISO_CT2*(ZX-XISO_BTM)/(R*XISO_BTS*ZX)))
       !
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZTCOR0_FUNC',1,ZHOOK_HANDLE)
END FUNCTION ZTCOR0_FUNC
!---------------------------------------------------------------------------
FUNCTION ZTCORM0_FUNC(ZX)

REAL           :: ZX
REAL  :: ZTCORM0_FUNC
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!      
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZTCORM0_FUNC',0,ZHOOK_HANDLE)
ZTCORM0_FUNC= 0.
ZTCORM0_FUNC = EXP(XMONO_BETA*(ZX-XMONO_T3))
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:ZTCORM0_FUNC',1,ZHOOK_HANDLE)
!
END FUNCTION ZTCORM0_FUNC
!
!---------------------------------------------------------------------------
!
END SUBROUTINE CH_BVOCEM_n
