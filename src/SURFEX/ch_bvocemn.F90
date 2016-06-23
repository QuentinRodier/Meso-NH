!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!   ###############################
     SUBROUTINE CH_BVOCEM_n (CHI, GB, I, &
                             PSW_FORBIO,PRHOA,PSFTS)
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
!!    R. Alkama 04/2012  add 7 new vegtype (19 instead 12)
!!
!!    EXTERNAL
!!    --------
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODI_VEGTYPE_TO_PATCH
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_BVOC_PAR
USE MODD_CSTS,ONLY : XMD, XAVOGADRO
USE MODD_CO2V_PAR
USE MODD_SURF_PAR,ONLY:XUNDEF
USE MODD_ISBA_PAR
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, NVT_TEBD, NVT_BONE, NVT_TRBE, &
                                NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                NVT_BOND, NVT_SHRB, NVT_BOGR, NVT_GRAS, & 
                                NVT_TROG, NVT_PARK, NVT_C3, NVT_C4,     &
                                NVT_IRR 
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

!
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(ISBA_t), INTENT(INOUT) :: I
!
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
REAL, DIMENSION(SIZE(PSW_FORBIO,1),SIZE(I%XABC),NVEGTYPE) :: ZBVOCPAR 
!                                PAR at gauss level in micromolphot/m2/s
!
REAL, DIMENSION(SIZE(PSW_FORBIO,1)) :: ZISOPOT, ZMONOPOT, ZRATIO
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
IF (I%CPHOTO/='NON') THEN
  KNGAUSS = SIZE(I%XABC)
ELSE
  !1.1.2 using isba std version 
  ZRAD_PAR (:)= 0.
  DO JPATCH = 1,I%NPATCH
    ZRAD_PAR (:)= ZRAD_PAR (:) +(PSW_FORBIO(:,JPATCH)*I%XPATCH(:,JPATCH) ) * XPARCF * 4.7 
  END DO
  ZLCOR_RAD (:) = ZLCOR_FUNC(ZRAD_PAR(:))
ENDIF
!  
!
 CALL BY_PATCH(NVT_TEBD, ZTCOR(:,NVT_TEBD), ZTCORM(:,NVT_TEBD))
 CALL BY_PATCH(NVT_BONE, ZTCOR(:,NVT_BONE), ZTCORM(:,NVT_BONE))
 CALL BY_PATCH(NVT_TRBE, ZTCOR(:,NVT_TRBE), ZTCORM(:,NVT_TRBE))
 CALL BY_PATCH(NVT_TRBD, ZTCOR(:,NVT_TRBD), ZTCORM(:,NVT_TRBD))
 CALL BY_PATCH(NVT_TEBE, ZTCOR(:,NVT_TEBE), ZTCORM(:,NVT_TEBE))
 CALL BY_PATCH(NVT_TENE, ZTCOR(:,NVT_TENE), ZTCORM(:,NVT_TENE))
 CALL BY_PATCH(NVT_BOBD, ZTCOR(:,NVT_BOBD), ZTCORM(:,NVT_BOBD))
 CALL BY_PATCH(NVT_BOND, ZTCOR(:,NVT_BOND), ZTCORM(:,NVT_BOND))
 CALL BY_PATCH(NVT_SHRB, ZTCOR(:,NVT_SHRB), ZTCORM(:,NVT_SHRB))
 CALL BY_PATCH(NVT_BOGR, ZTCOR(:,NVT_BOGR), ZTCORM(:,NVT_BOGR))
 CALL BY_PATCH(NVT_GRAS, ZTCOR(:,NVT_GRAS), ZTCORM(:,NVT_GRAS))
 CALL BY_PATCH(NVT_TROG, ZTCOR(:,NVT_TROG), ZTCORM(:,NVT_TROG))
 CALL BY_PATCH(NVT_PARK, ZTCOR(:,NVT_PARK), ZTCORM(:,NVT_PARK))
 CALL BY_PATCH(NVT_C3  , ZTCOR(:,NVT_C3)  , ZTCORM(:,NVT_C3)  )
 CALL BY_PATCH(NVT_C4  , ZTCOR(:,NVT_C4)  , ZTCORM(:,NVT_C4)  )
 CALL BY_PATCH(NVT_IRR , ZTCOR(:,NVT_IRR) , ZTCORM(:,NVT_IRR) )
!
!
ZRATIO (:) = I%XVEGTYPE(:,NVT_TEBD) + I%XVEGTYPE(:,NVT_BONE) + I%XVEGTYPE(:,NVT_TRBE) + &
             I%XVEGTYPE(:,NVT_TRBD) + I%XVEGTYPE(:,NVT_TEBE) + I%XVEGTYPE(:,NVT_TENE) + &
             I%XVEGTYPE(:,NVT_BOBD) + I%XVEGTYPE(:,NVT_BOND) + I%XVEGTYPE(:,NVT_SHRB)
!
WHERE (ZRATIO(:)/=0.)
  ZISOPOT (:) = GB%XISOPOT (:) / ZRATIO(:)
  ZMONOPOT(:) = GB%XMONOPOT(:) / ZRATIO(:) 
ELSEWHERE
  ZISOPOT (:) = 0.
  ZMONOPOT(:) = 0.
END WHERE
!                       
 CALL BY_VEG9(NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                    NVT_BOND, NVT_SHRB, ZISOPOT, ZMONOPOT, ZFISO_FOR, ZFMONO_FOR)
!
ZISOPOT (:) = XISOPOT_GRASS
ZMONOPOT(:) = XMONOPOT_GRASS
 CALL BY_VEG4(NVT_GRAS, NVT_TROG, NVT_PARK, NVT_BOGR, ZISOPOT, ZMONOPOT, ZFISO_GRASS, ZFMONO_GRASS)
!
ZISOPOT (:) = XISOPOT_CROP
ZMONOPOT(:) = XMONOPOT_CROP
 CALL BY_VEG3(NVT_C3, NVT_C4, NVT_IRR, ZISOPOT, ZMONOPOT, ZFISO_CROP, ZFMONO_CROP)
!
!---------------------------------------------------------------------------------------
!
!3.Summation of different contribution for fluxes 
!------------------------------------------------
!
!isoprene in ppp.m.s-1
GB%XFISO (:)=(3.0012E-10/3600.) * ( ZFISO_FOR (:) + ZFISO_GRASS(:) + ZFISO_CROP(:) ) + 1E-17
!monoterpenes
GB%XFMONO(:)=(1.5006E-10/3600.) * ( ZFMONO_FOR(:) + ZFMONO_GRASS(:)+ ZFMONO_CROP(:) ) + 1E-17
!
! conversion in molecules/m2/s
!
GB%XFISO(:)  = GB%XFISO(:)  * XAVOGADRO * PRHOA(:) / XMD
GB%XFMONO(:) = GB%XFMONO(:) * XAVOGADRO * PRHOA(:) / XMD
!
DO JSV=CHI%SVI%NSV_CHSBEG,CHI%SVI%NSV_CHSEND
  IF (CHI%SVI%CSV(JSV) == "BIO") THEN
    ! RELACS CASE
    PSFTS(:,JSV) = PSFTS(:,JSV) + (GB%XFISO(:) + GB%XFMONO(:)) 
  ELSE IF (CHI%SVI%CSV(JSV) == "ISO" .OR. CHI%SVI%CSV(JSV) == "ISOP") THEN
    ! RACM CASE
    PSFTS(:,JSV) = PSFTS(:,JSV) + GB%XFISO(:)  
  ELSE IF (CHI%SVI%CSV(JSV) == "API"  .OR. CHI%SVI%CSV(JSV) == "LIM" .OR. &
           CHI%SVI%CSV(JSV) == "BIOL" .OR. CHI%SVI%CSV(JSV) == "BIOH" ) THEN
    ! RACM CASE
    ! CACM or RELACS 2 CASE     
    PSFTS(:,JSV) = PSFTS(:,JSV) + 0.5 * GB%XFMONO(:) 
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
REAL, DIMENSION(SIZE(PSW_FORBIO,1),SIZE(I%XABC)) :: ZBVOCPAR 
INTEGER:: IPATCH, JLAYER, IT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_PATCH',0,ZHOOK_HANDLE)
!
IPATCH = VEGTYPE_TO_PATCH(NVT_VEGTYPE, I%NPATCH)
!
PTCOR  (:) = 0.
PTCORM (:) = 0.
DO IT=1,SIZE(I%XTG,1)
  IF (I%XTG(IT,1,IPATCH).LE.1000.) THEN
    PTCORM(IT)=ZTCORM0_FUNC(I%XTG(IT,1,IPATCH))
    PTCOR (IT)=ZTCOR0_FUNC (I%XTG(IT,1,IPATCH))
  ENDIF
ENDDO
!
IF (I%CPHOTO/='NON') THEN
  !PAR over Forest canopies, in micro-molE.m-2.s-1 
  ZBVOCPAR(:,:) = GB%XIACAN(:,:,IPATCH)*4.7
  !Calculation of radiative attenuation effect in the canopy on correction factor
  ZBVOCSG(:) = 0.
  DO JLAYER=1,KNGAUSS
    ZBVOCSG(:) = ZBVOCSG(:) + I%XPOI(JLAYER) * ZLCOR_FUNC(ZBVOCPAR(:,JLAYER)) 
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
SUBROUTINE BY_VEG3(NVT_V1, NVT_V2, NVT_V3, &
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
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG3',0,ZHOOK_HANDLE)
!
!isoprene flux 
!!
!! warning, XISOPOT external map accounts for the total forest fraction
WHERE ( I%XVEGTYPE(:,NVT_V1) + I%XVEGTYPE(:,NVT_V2) + I%XVEGTYPE(:,NVT_V3) > 0. )
  !
  PFISO(:) = PISOPOT(:) *                   &
     ( ZTCOR(:,NVT_V1) * I%XVEGTYPE(:,NVT_V1) &
      +ZTCOR(:,NVT_V2) * I%XVEGTYPE(:,NVT_V2) &
      +ZTCOR(:,NVT_V3) * I%XVEGTYPE(:,NVT_V3) )
  !
  PFMONO(:) = PMONOPOT(:) *                  &
     ( ZTCORM(:,NVT_V1) * I%XVEGTYPE(:,NVT_V1) &
      +ZTCORM(:,NVT_V2) * I%XVEGTYPE(:,NVT_V2) &
      +ZTCORM(:,NVT_V3) * I%XVEGTYPE(:,NVT_V3) )          
  !
ELSEWHERE
  !
  PFISO(:) = 0.
  PFMONO(:) = 0.
  !
END WHERE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG3',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_VEG3
!--------------------------------------------------------------------------
SUBROUTINE BY_VEG4(NVT_V1, NVT_V2, NVT_V3, NVT_V4,&
                  PISOPOT, PMONOPOT, PFISO, PFMONO)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: NVT_V1
INTEGER, INTENT(IN) :: NVT_V2
INTEGER, INTENT(IN) :: NVT_V3
INTEGER, INTENT(IN) :: NVT_V4
REAL, DIMENSION(:), INTENT(IN) :: PISOPOT
REAL, DIMENSION(:), INTENT(IN) :: PMONOPOT
REAL, DIMENSION(:), INTENT(OUT) :: PFISO
REAL, DIMENSION(:), INTENT(OUT) :: PFMONO
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG4',0,ZHOOK_HANDLE)
!
!isoprene flux 
!!
!! warning, XISOPOT external map accounts for the total forest fraction
WHERE ( I%XVEGTYPE(:,NVT_V1) + I%XVEGTYPE(:,NVT_V2) + I%XVEGTYPE(:,NVT_V3) &
       +I%XVEGTYPE(:,NVT_V4) > 0. )
  !
  PFISO(:) = PISOPOT(:) *                   &
     ( ZTCOR(:,NVT_V1) * I%XVEGTYPE(:,NVT_V1) &
      +ZTCOR(:,NVT_V2) * I%XVEGTYPE(:,NVT_V2) &
      +ZTCOR(:,NVT_V3) * I%XVEGTYPE(:,NVT_V3) &
      +ZTCOR(:,NVT_V4) * I%XVEGTYPE(:,NVT_V4) )
  !
  PFMONO(:) = PMONOPOT(:) *                  &
     ( ZTCORM(:,NVT_V1) * I%XVEGTYPE(:,NVT_V1) &
      +ZTCORM(:,NVT_V2) * I%XVEGTYPE(:,NVT_V2) &
      +ZTCORM(:,NVT_V3) * I%XVEGTYPE(:,NVT_V3) &
      +ZTCORM(:,NVT_V4) * I%XVEGTYPE(:,NVT_V4) )            
  !
ELSEWHERE
  !
  PFISO(:) = 0.
  PFMONO(:) = 0.
  !
END WHERE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG4',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_VEG4
!--------------------------------------------------------------------------
SUBROUTINE BY_VEG9(NVT_V1, NVT_V2, NVT_V3, NVT_V4, NVT_V5, NVT_V6,     &
               NVT_V7, NVT_V8, NVT_V9, PISOPOT, PMONOPOT, PFISO, PFMONO)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: NVT_V1
INTEGER, INTENT(IN) :: NVT_V2
INTEGER, INTENT(IN) :: NVT_V3
INTEGER, INTENT(IN) :: NVT_V4
INTEGER, INTENT(IN) :: NVT_V5
INTEGER, INTENT(IN) :: NVT_V6
INTEGER, INTENT(IN) :: NVT_V7
INTEGER, INTENT(IN) :: NVT_V8
INTEGER, INTENT(IN) :: NVT_V9
REAL, DIMENSION(:), INTENT(IN) :: PISOPOT
REAL, DIMENSION(:), INTENT(IN) :: PMONOPOT
REAL, DIMENSION(:), INTENT(OUT) :: PFISO
REAL, DIMENSION(:), INTENT(OUT) :: PFMONO
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG9',0,ZHOOK_HANDLE)
!
!isoprene flux 
!!
!! warning, XISOPOT external map accounts for the total forest fraction
WHERE ( I%XVEGTYPE(:,NVT_V1) + I%XVEGTYPE(:,NVT_V2) + I%XVEGTYPE(:,NVT_V3) &
       +I%XVEGTYPE(:,NVT_V4) + I%XVEGTYPE(:,NVT_V5) + I%XVEGTYPE(:,NVT_V6) &
       +I%XVEGTYPE(:,NVT_V7) + I%XVEGTYPE(:,NVT_V8) + I%XVEGTYPE(:,NVT_V9) > 0. )
  !
  PFISO(:) = PISOPOT(:) *                   &
     ( ZTCOR(:,NVT_V1) * I%XVEGTYPE(:,NVT_V1) &
      +ZTCOR(:,NVT_V2) * I%XVEGTYPE(:,NVT_V2) &
      +ZTCOR(:,NVT_V3) * I%XVEGTYPE(:,NVT_V3) &
      +ZTCOR(:,NVT_V4) * I%XVEGTYPE(:,NVT_V4) &
      +ZTCOR(:,NVT_V5) * I%XVEGTYPE(:,NVT_V5) &
      +ZTCOR(:,NVT_V6) * I%XVEGTYPE(:,NVT_V6) &
      +ZTCOR(:,NVT_V7) * I%XVEGTYPE(:,NVT_V7) &
      +ZTCOR(:,NVT_V8) * I%XVEGTYPE(:,NVT_V8) &
      +ZTCOR(:,NVT_V9) * I%XVEGTYPE(:,NVT_V9) )
  !
  PFMONO(:) = PMONOPOT(:) *                  &
     ( ZTCORM(:,NVT_V1) * I%XVEGTYPE(:,NVT_V1) &
      +ZTCORM(:,NVT_V2) * I%XVEGTYPE(:,NVT_V2) &
      +ZTCORM(:,NVT_V3) * I%XVEGTYPE(:,NVT_V3) &
      +ZTCORM(:,NVT_V4) * I%XVEGTYPE(:,NVT_V4) & 
      +ZTCORM(:,NVT_V5) * I%XVEGTYPE(:,NVT_V5) & 
      +ZTCORM(:,NVT_V6) * I%XVEGTYPE(:,NVT_V6) & 
      +ZTCORM(:,NVT_V7) * I%XVEGTYPE(:,NVT_V7) & 
      +ZTCORM(:,NVT_V8) * I%XVEGTYPE(:,NVT_V8) &
      +ZTCORM(:,NVT_V9) * I%XVEGTYPE(:,NVT_V9) ) 
  !
ELSEWHERE
  !
  PFISO(:) = 0.
  PFMONO(:) = 0.
  !
END WHERE
!
IF (LHOOK) CALL DR_HOOK('CH_BVOCEM_N:BY_VEG9',1,ZHOOK_HANDLE)
!
END SUBROUTINE BY_VEG9
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
