!     #########
SUBROUTINE SOILSTRESS( HISBA, PF2,                                   &
                  PROOTFRAC, PWSAT, PWFC, PWWILT,                    &
                  PWG, PWGI, KWG_LAYER, PF2WGHT, PF5                 )  
!     ####################################################################
!
!!****  *SOILSTRESS*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the vegetation stress due to soil water
!         
!     
!!**  METHOD
!!    ------
!
!     Calculates the F2 coefficient.
!     
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    none
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!      
!!    AUTHOR
!!    ------
!!
!!	S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      13/03/95 
!!     (P.Jabouille)  13/11/96    mininum value for ZF1
!!     (V. Masson)    28/08/98    add PF2 for Calvet (1998) CO2 computations
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
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
 CHARACTER(LEN=*),     INTENT(IN)   :: HISBA   ! type of soil (Force-Restore OR Diffusion)
!                                             ! '2-L'
!                                             ! '3-L'
!                                             ! 'DIF'   ISBA-DF
!
REAL, DIMENSION(:,:), INTENT(IN) :: PROOTFRAC, PWSAT, PWFC, PWWILT,       &
                                      PWG, PWGI  
!                                     PROOTFRAC = cumulative root fraction (-)
!                                     PWFC      = field capacity profile (m3/m3)
!                                     PWWILT    = wilting point profile (m3/m3)
!                                     PWSAT     = porosity profile (m3/m3)
!                                     PWG       = soil liquid volumetric water content (m3/m3)
!                                     PWGI      = soil frozen volumetric water content (m3/m3)
!
INTEGER, DIMENSION(:), INTENT(IN) :: KWG_LAYER  
!                                    KWG_LAYER = Number of soil moisture layers (DIF option)
!
REAL, DIMENSION(:), INTENT(OUT)  :: PF2      ! water stress coefficient
!
REAL, DIMENSION(:), INTENT(OUT)  :: PF5      ! water stress coefficient for Hv (based on F2):
!                                            ! Verify that Etv=>0 as F2=>0
!
REAL, DIMENSION(:,:), INTENT(OUT):: PF2WGHT  ! water stress coefficient profile (ISBA-DF)
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PWFC,1)) ::  ZWFC_AVGZ, ZWSAT_AVGZ, ZWWILT_AVGZ
!	                           ZWFC_AVGZ   = field capacity averaged over entire soil column
!	                           ZWSAT_AVGZ  = porosity averaged over entire soil column
!	                           ZWWILT_AVGZ = wilting point averaged over entire soil column
!
! ISBA-DF:
!
REAL, DIMENSION(SIZE(PWG,1)) :: ZWSAT, ZWFC, ZWWILT
!                               ZWSAT     = ice-adjusted porosity profile (m3/m3)
!                               ZWFC      = ice-adjusted field capacity profile (m3/m3)
!                               ZWWILT    = ice-adjusted wilting point profile (m3/m3)
!
REAL    :: ZROOTFRACN
!          ZROOTFRACN = Normalized root fraction weights
!
INTEGER :: INI, INL, JJ, JL, IDEPTH
!
!
!*      0.3    declarations of local parameters:
!
REAL, PARAMETER                :: ZDENOM_MIN  = 1.E-6 ! minimum denominator: 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                                     ! numerical factor to prevent division by 0
!-------------------------------------------------------------------------------
!
!*       0.     Initialization of variables:
!               ----------------------------
!
IF (LHOOK) CALL DR_HOOK('SOILSTRESS',0,ZHOOK_HANDLE)
!
INI=SIZE(PWG,1)
INL=MAXVAL(KWG_LAYER(:))
!
PF2    (:)      = 0.0
PF2WGHT(:,:)    = 0.0
!
ZWFC_AVGZ(:)    = 0.
ZWSAT_AVGZ(:)   = 0.
ZWWILT_AVGZ(:)  = 0.
!
!-------------------------------------------------------------------------------
!
!*       2.     THE 'PF2' FACTOR
!               ----------------
!               This factor takes into account the effect
!               of the water stress on the surface
!               resistance
!
! - For humid soils (> WFC), this factor does not
!   increase the stomatal resistance                                   
! - The stomatal resistance should be large
!   when the soil is very dry (< WILT)
! - For intermediate soils it ranges (F2_min =< F2 <= 1):
!   where F2_min is a small numerical threshold
!
IF(HISBA =='DIF')THEN      
!
! If using the diffusion option, then calculate transpiration weights
! and the mean root-zone soil water stress factor F2:
!
!---------------------------------------------------------
! First layer
!---------------------------------------------------------
!
! Due to the presence of ice, modify soil parameters:
  ZWSAT (:) = PWSAT (:,1) - PWGI(:,1)
  ZWFC  (:) = PWFC  (:,1) * ZWSAT(:)/PWSAT(:,1)
  ZWWILT(:) = PWWILT(:,1) * ZWSAT(:)/PWSAT(:,1)
!
! Calculate the soil water stress factor for each layer:
  PF2WGHT(:,1) = (PWG(:,1)-ZWWILT(:))/(ZWFC(:)-ZWWILT(:))
  PF2WGHT(:,1) = MIN( 1.0, MAX( ZDENOM_MIN,PF2WGHT(:,1) ) ) 
!
! Normalize the transpiration weights by root fraction:
  PF2WGHT(:,1) = PROOTFRAC(:,1)*PF2WGHT(:,1)
!
! Net soil water stress for entire root zone:
  PF2(:) = PF2WGHT(:,1)
!
! Make sure weights stress factor is within limits
  PF2WGHT(:,1) = MAX(PF2WGHT(:,1),ZDENOM_MIN*PROOTFRAC(:,1))
!
!---------------------------------------------------------
! Other layers
!---------------------------------------------------------
!
  DO JL=2,INL
     DO JJ=1,INI
!
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<=IDEPTH)THEN
!
!       Due to the presence of ice, modify soil parameters:
        ZWSAT (JJ) = PWSAT (JJ,JL) - PWGI(JJ,JL)
        ZWFC  (JJ) = PWFC  (JJ,JL) * ZWSAT(JJ)/PWSAT(JJ,JL)
        ZWWILT(JJ) = PWWILT(JJ,JL) * ZWSAT(JJ)/PWSAT(JJ,JL)
!
!       Calculate the soil water stress factor for each layer:
        PF2WGHT(JJ,JL) = (PWG(JJ,JL)-ZWWILT(JJ))/(ZWFC(JJ)-ZWWILT(JJ))
        PF2WGHT(JJ,JL) = MIN( 1.0, MAX( ZDENOM_MIN,PF2WGHT(JJ,JL) ) ) 
!
!       Calculate normalized root fraction weights:
        ZROOTFRACN = PROOTFRAC(JJ,JL) - PROOTFRAC(JJ,JL-1)
!
!       Normalize the transpiration weights by root fraction:                                                
        PF2WGHT(JJ,JL) = ZROOTFRACN*PF2WGHT(JJ,JL)
!
!       Net soil water stress for entire root zone:
        PF2(JJ) = PF2(JJ) + PF2WGHT(JJ,JL)
!
!       Make sure weights stress factor is within limits
        PF2WGHT(JJ,JL) = MAX(PF2WGHT(JJ,JL),ZDENOM_MIN*ZROOTFRACN)
!        
      ENDIF
!
     ENDDO
  ENDDO
!
! Make sure average stress factor is within limits
  PF2(:) = MAX(ZDENOM_MIN, MIN(1.0, PF2(:)))
!
ELSE
!
! When using the Force-Restore (FR) soil option, the soil properties
! are assumed to be homogeneous in the vertical. Therefore, simply
! use the properties for the uppermost soil layer when defining
! soil properties for local computations.
!
! Due to the presence of ice, modify soil parameters:
!
   ZWSAT_AVGZ(:)  = PWSAT(:,1) - PWGI(:,2)
   ZWFC_AVGZ(:)   = PWFC(:,1)  *ZWSAT_AVGZ(:)/PWSAT(:,1)
   ZWWILT_AVGZ(:) = PWWILT(:,1)*ZWSAT_AVGZ(:)/PWSAT(:,1)
!
! Compute the water stress factor:
!
   PF2(:) = ( PWG(:,2)-ZWWILT_AVGZ(:) ) /  ( ZWFC_AVGZ(:)-ZWWILT_AVGZ(:))
   PF2(:) = MAX(ZDENOM_MIN , MIN(1.0, PF2(:)))
!
!
ENDIF
!
! Function to cause Etv to approach 0 as F2 goes to 0:
!
PF5(:) = MERGE(PF2(:),0.0,PF2(:)>ZDENOM_MIN)
!
IF (LHOOK) CALL DR_HOOK('SOILSTRESS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOILSTRESS
