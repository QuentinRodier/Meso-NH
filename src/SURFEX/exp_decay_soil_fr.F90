!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########
      SUBROUTINE EXP_DECAY_SOIL_FR (HISBA, PF, PC1SAT, PC2REF, PD_G, PD_ICE, &
                                      PC4REF, PC3, PCONDSAT, PKSAT_ICE       )  
!     ##########################################################################
!
!!****  *EXP_DECAY_SOIL_FR*  
!!
!!    PURPOSE
!!    -------
!
!     We caculate the hydraulic coductivity decay factor for each FR-coefficients.
!     Also, we redefine the surface hydraulic coductivity at saturation for
!     convective precipitation parametrisation.
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/11/03 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,ONLY : XUNDEF
USE MODD_SGH_PAR, ONLY : X2                                
USE MODD_CSTS,    ONLY : XDAY
#ifdef TOPD
USE MODD_DUMMY_EXP_PROFILE,ONLY : XC_DEPTH_RATIO
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*)                  :: HISBA   ! hydrology/soil:
!                                            ! '2-L'  = single column
!                                            ! '3-L'  = root zone/baseflow layer
!                                            ! 'DIF'  = N-layer diffusion: Richard's Eq.
!
REAL, DIMENSION(:), INTENT(IN)    :: PF
!                                    PF = exponential decay factor (1/m)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PC2REF,PC1SAT
!                                    PC1SAT  = C1 at saturation
!                                    PC2REF  = reference value of C2
!
REAL, DIMENSION(:,:),INTENT(INOUT):: PCONDSAT
!                                    PCONDSAT  = hydraulic conductivity at saturation (m s-1)
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PD_G 
!                                    PD_G   = Depth of bottom of Soil layers (m)
!
REAL, DIMENSION(:), INTENT(IN)    :: PD_ICE 
!                                    PD_ICE = depth of the soil column for
!                                             fraction of frozen soil calculation (m)

REAL, DIMENSION(:,:),INTENT(INOUT):: PC3
!                                    PC3 = C3 coef with exponential decay of hydraulic soil profil 
!
REAL, DIMENSION(:), INTENT(INOUT) :: PC4REF
!                                    PC4REF = fiiting soil paramater for vertical diffusion (C4)
!                                             with exponential decay of hydraulic soil profil 
!
REAL, DIMENSION(:), INTENT(OUT)   :: PKSAT_ICE
!                                    PKSAT_ICE = hydraulic conductivity at saturation (m s-1)
!                                                on frozen soil depth (Horton calculation)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PF))         :: ZD_G_TOT, ZC_DEPTH, ZKSAT_NOEXP, ZC_DEPTH_RATIO
!                                    ZD_G_TOT = depth of the soil column (m)
!                                    ZC_DEPTH = assumed as the depth where the vertical 
!                                               satured hydraulic conductivities reach
!                                               the compacted value given in Clapp and
!                                               Hornberger. (m)
!                                               For ISBA-FR, we take the root depth.
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('EXP_DECAY_SOIL_FR',0,ZHOOK_HANDLE)
!
ZD_G_TOT(:) = PD_G(:,2)
IF(HISBA=='3-L')ZD_G_TOT(:) = PD_G(:,3)
!
ZKSAT_NOEXP(:) = PCONDSAT(:,2)
!
ZC_DEPTH_RATIO(:) = 1.
!
#ifdef TOPD
IF (ALLOCATED(XC_DEPTH_RATIO)) ZC_DEPTH_RATIO(:) = XC_DEPTH_RATIO(:)
#endif
!
WHERE(ZD_G_TOT(:)/=XUNDEF)
!
!compacted depth
!
ZC_DEPTH(:) = PD_G(:,2)*ZC_DEPTH_RATIO(:)
!ZC_DEPTH(:) = PD_G(:,2)
!
!surface hydraulic conductivity at saturation
!
PCONDSAT(:,1) = PCONDSAT(:,1)*EXP(PF(:)*ZC_DEPTH(:))
!
!mean hydraulic conductivity at saturation over the root zone
!   
PCONDSAT(:,2) = ZKSAT_NOEXP(:)*( EXP(PF(:)*ZC_DEPTH)-EXP(PF(:)*(ZC_DEPTH(:)-PD_G(:,2))) )   &
                  /(PF(:)*PD_G(:,2))
!   
!mean hydraulic conductivity at saturation over the first soil centimeters
!   
PKSAT_ICE(:) = ZKSAT_NOEXP(:)*( EXP(PF(:)*ZC_DEPTH)-EXP(PF(:)*(ZC_DEPTH(:)-PD_ICE(:))) )   &
                 /(PF(:)*PD_ICE(:))  
!
!decay factor for C1 coef
!   
PC1SAT(:) = PC1SAT(:)*SQRT( EXP(-PF(:)*ZC_DEPTH(:)) )
!
!decay factor for C2 coef 
!
PC2REF(:)=PC2REF(:)+( PCONDSAT(:,2)-ZKSAT_NOEXP(:) ) * XDAY/PD_G(:,2) 
!
!C3 coef with exponential decay in root soil layer 
!
PC3(:,1)=PC3(:,1)*( EXP(PF(:)*ZC_DEPTH(:))-EXP(PF(:)*(ZC_DEPTH(:)-PD_G(:,2))) ) / (PF(:)*PD_G(:,2))
!
ENDWHERE
!
IF(HISBA=='3-L')THEN
! 
   WHERE(PD_G(:,2)< ZD_G_TOT(:).AND.PD_G(:,2)/=XUNDEF)
!           
!  C3 coef with exponential decay in deep soil layer 
!
   PC3(:,2)=PC3(:,2)*( EXP(PF(:)*(ZC_DEPTH(:)-PD_G(:,2)))-EXP(PF(:)*(ZC_DEPTH(:)-ZD_G_TOT(:))) )      &
                       / (PF(:)*(ZD_G_TOT(:)-PD_G(:,2)))  
! 
!  decay factor for C4 coef
!      
   PC4REF(:)=PC4REF(:)*( EXP(PF(:)*(ZC_DEPTH(:)-PD_G(:,2)/X2))-EXP(PF(:)*(ZC_DEPTH(:)&
                         -((PD_G(:,2)+ZD_G_TOT(:))/2.))) ) * X2/(PF(:)*ZD_G_TOT(:))        
!
   ENDWHERE
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('EXP_DECAY_SOIL_FR',1,ZHOOK_HANDLE)
!
END SUBROUTINE EXP_DECAY_SOIL_FR
