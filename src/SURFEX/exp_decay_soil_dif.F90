!     #########################
SUBROUTINE EXP_DECAY_SOIL_DIF (PF,PD_G,KWG_LAYER,PDROOT,PCONDSAT)
!     ##########################################################
!
!!****  *EXP_DECAY_SOIL_DIF*  
!!
!!    PURPOSE
!!    -------
!
!     We caculate the hydraulic conductivity decay factor for each conductivity (for diffusion option).
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
!!	B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/11/03 
!-------------------------------------------------------------------------------
!
USE MODD_SGH_PAR, ONLY : X2,XF_DECAY
USE MODD_SURF_PAR,ONLY : XUNDEF, NUNDEF
!
!*      0.1    declarations of arguments
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL, DIMENSION(:), INTENT(IN)    :: PF
!                                    PF = exponential decay factor (1/m)
REAL, DIMENSION(:,:),INTENT(IN   ) :: PD_G          !layer depth
INTEGER, DIMENSION(:), INTENT(IN ) :: KWG_LAYER
REAL, DIMENSION(:),  INTENT(IN   ) :: PDROOT        !root depth
REAL, DIMENSION(:,:),INTENT(INOUT) :: PCONDSAT      !hydraulic conductivity at saturation (m s-1)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PD_G(:,:),1))                   :: ZC_DEPTH
REAL, DIMENSION(SIZE(PD_G(:,:),1),SIZE(PD_G(:,:),2)) :: ZD_MID
!
INTEGER                       :: I, INL, JL, INI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('EXP_DECAY_SOIL_DIF',0,ZHOOK_HANDLE)
!
INI=SIZE(PD_G(:,:),1)
INL=SIZE(PD_G(:,:),2)
!
!-------------------------------------------------------------------------------
!
!Mid point depth
!
ZD_MID(:,:)=XUNDEF
!
WHERE(PD_G(:,1)/=XUNDEF)ZD_MID(:,1)=PD_G(:,1)/X2
!
DO JL=2,INL
   DO I=1,INI
      IF(PD_G(I,JL)/=XUNDEF)THEN   
         ZD_MID(I,JL)=(PD_G(I,JL-1)+PD_G(I,JL))/X2
      ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!depth where the vertical satured hydraulic conductivities reach
!the compacted value given in Clapp and Hornberger (root depth)
!
ZC_DEPTH=0.0
!
DO I=1,INI
   IF(PDROOT(I)/=XUNDEF.AND.KWG_LAYER(I)/=NUNDEF)THEN
     ZC_DEPTH(I)=PDROOT(I)
   ENDIF
ENDDO
!
!-------------------------------------------------------------------------------
! Exponential conductivity of heach mid point layer 
!-------------------------------------------------------------------------------
!
DO JL=1,INL
  DO I=1,INI
    IF(ZD_MID(I,JL)/=XUNDEF) THEN
      IF (JL<=KWG_LAYER(I))THEN             
        PCONDSAT (I,JL) = PCONDSAT (I,JL) * EXP(PF(I)*(ZC_DEPTH(I)-ZD_MID(I,JL)))
      ELSE
        PCONDSAT (I,JL) = PCONDSAT (I,KWG_LAYER(I))
      ENDIF
    ELSE
      PCONDSAT (I,JL) = XUNDEF
    ENDIF
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('EXP_DECAY_SOIL_DIF',1,ZHOOK_HANDLE)
!
END SUBROUTINE EXP_DECAY_SOIL_DIF





