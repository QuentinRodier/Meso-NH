!#####################
MODULE MODI_ADV_BOUNDARIES
!#####################
!
INTERFACE
!
      SUBROUTINE ADV_BOUNDARIES ( HLBCX,HLBCY,PFIELD,PFIELDI,HFIELD )    
!
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN)      :: HLBCX,HLBCY   ! X and Y-direc. LBC type
REAL, DIMENSION(:,:,:),   INTENT(INOUT)         :: PFIELD                        
REAL, DIMENSION(:,:,:),   INTENT(IN), OPTIONAL  :: PFIELDI                        
CHARACTER(LEN=1),         INTENT(IN), OPTIONAL  :: HFIELD  ! Field type
!
END SUBROUTINE ADV_BOUNDARIES
!
END INTERFACE
!

END MODULE MODI_ADV_BOUNDARIES
!
!
!     ####################################################################
      SUBROUTINE ADV_BOUNDARIES ( HLBCX,HLBCY,PFIELD,PFIELDI,HFIELD )
!     ####################################################################
!
!!****  *ADV_BOUNDARIES* - routine to prepare the top and bottom Boundary Conditions 
!!
!!
!!    AUTHOR
!!    ------
!!	
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!         
USE MODD_PARAMETERS
USE MODE_ll
!
IMPLICIT NONE
!
!
!*       0.1   declarations of arguments
!
!
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN)      :: HLBCX,HLBCY   ! X and Y-direc. LBC type
REAL, DIMENSION(:,:,:),   INTENT(INOUT)         :: PFIELD                        
REAL, DIMENSION(:,:,:),   INTENT(IN), OPTIONAL  :: PFIELDI                        
CHARACTER(LEN=1),         INTENT(IN), OPTIONAL  :: HFIELD  ! Field type
!
!
!*       0.2   declarations of local variables
!
INTEGER             :: IKB       ! indice K Beginning in z direction
INTEGER             :: IKE       ! indice K End       in z direction 
INTEGER             :: IIU, IJU  ! Index End in X and Y directions
!
!-------------------------------------------------------------------------------
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS AND OTHER INDICES:
!              ----------------------------------------------
IKB = 1 + JPVEXT
IKE = SIZE(PFIELD,3) - JPVEXT
IIU=SIZE(PFIELD,1)
IJU=SIZE(PFIELD,2)
!
IF (SIZE(PFIELD)==0) RETURN
!-------------------------------------------------------------------------------
!
!*       2.    UPPER AND LOWER BC FILLING:   
!              ---------------------------
!
!*       2.1    COMPUTE THE FIELD EXTRAPOLATIONS AT THE GROUND
!
!
   IF (PRESENT(HFIELD) .AND. PRESENT(PFIELDI)) THEN
     IF (HFIELD=='W') &
     PFIELD  (:,:,IKB  )   = PFIELDI (:,:,IKB) 
   END IF
!
   PFIELD  (:,:,IKB-1)   = PFIELD  (:,:,IKB) 

!
!*       2.2    COMPUTE THE FIELD EXTRAPOLATIONS AT THE TOP
!
  PFIELD  (:,:,IKE+1)   = PFIELD  (:,:,IKE) 
!
!
!*       3.    LATERAL BC FILLING                                
!              ---------------------------
!
IF( PRESENT(PFIELDI) )  THEN
  IF (HLBCX(1)=='OPEN' .AND. LWEST_ll()) THEN
     PFIELD(1,:,:) = PFIELDI(1,:,:)
     IF (PRESENT(HFIELD)) THEN
       IF (HFIELD=='U') &
       PFIELD(2,:,:) = PFIELDI(2,:,:)
     END IF
  END IF
  IF (HLBCX(2)=='OPEN' .AND. LEAST_ll()) THEN
     PFIELD(IIU,:,:) = PFIELDI(IIU,:,:)
  END IF
  IF (HLBCY(1)=='OPEN' .AND. LSOUTH_ll()) THEN
     PFIELD(:,1,:) = PFIELDI(:,1,:)
     IF (PRESENT(HFIELD)) THEN
       IF (HFIELD=='V') &
       PFIELD(:,2,:) = PFIELDI(:,2,:)
     END IF
  END IF
  IF (HLBCY(2)=='OPEN' .AND. LNORTH_ll()) THEN
     PFIELD(:,IJU,:) = PFIELDI(:,IJU,:)
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ADV_BOUNDARIES
