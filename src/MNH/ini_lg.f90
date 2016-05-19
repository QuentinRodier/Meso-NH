!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##################
      MODULE MODI_INI_LG
!     ##################
INTERFACE
!
      SUBROUTINE INI_LG(PXHAT,PYHAT,PZZ,PSVM,PSVT,PLBXSVM,PLBYSVM)
!
REAL,DIMENSION(:),      INTENT(IN) :: PXHAT,PYHAT ! Positions x,y in the cartesian plane
REAL,DIMENSION(:,:,:), INTENT(IN)  :: PZZ         ! True altitude of the w grid-point
REAL,DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVM,PSVT        ! scalar var. at t-1 and t
REAL,DIMENSION(:,:,:,:), INTENT(INOUT) :: PLBXSVM,PLBYSVM  ! LB in x and y-dir.
!
END SUBROUTINE INI_LG
!
END INTERFACE
!
END MODULE MODI_INI_LG
!
!
!
!     ############################################################
      SUBROUTINE INI_LG(PXHAT,PYHAT,PZZ,PSVM,PSVT,PLBXSVM,PLBYSVM)
!     ############################################################
!
!!****  *INI_LG* - routine to initialize lagrangian variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to set or reset lagrangian variables
!       as the initial position of particles

!!**  METHOD
!!    ------
!     floating indices are used to set only SV variables corresponding to
!     lagrangian variables
!!      
!!    EXTERNAL
!!    --------   
!!      None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Module MODD_NSV : NSV_LGBED, NSV_LGEND
!!
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine INI_LG)
!!      
!!
!!    AUTHOR
!!    ------
!!	P. Jabouille / J Stein      * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        22/06/01 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------ 
!
!
USE MODD_NSV, ONLY : NSV_LGBEG,NSV_LGEND
USE MODE_ll
!
IMPLICIT NONE
!
!*       0.1   declarations of argument
!
REAL,DIMENSION(:),      INTENT(IN) :: PXHAT,PYHAT ! Positions x,y in the cartesian plane
REAL,DIMENSION(:,:,:), INTENT(IN)  :: PZZ         ! True altitude of the w grid-point
REAL,DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVM,PSVT        ! scalar var. at t-1 and t
REAL,DIMENSION(:,:,:,:), INTENT(INOUT) :: PLBXSVM,PLBYSVM  ! LB in x and y-dir.
!
!
!*       0.2   declarations of local variables
!                                                     
INTEGER      :: IIU,IJU,IKU ! Upper bounds
INTEGER      :: JI,JJ,JK    ! loop index
!
!-------------------------------------------------------------------------------
!
!*       1.    PROLOGUE
!              --------
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
IKU=SIZE(PZZ,3)
!
!*       2.    Compute initial position
!              ------------------------
!
DO JK=1,IKU
  DO JJ=1,IJU
    DO JI=1,IIU-1
      PSVM(JI,JJ,JK,NSV_LGBEG)=0.5*(PXHAT(JI)+PXHAT(JI+1))
    END DO
    PSVM(IIU,JJ,JK,NSV_LGBEG)=2.*PSVM(IIU-1,JJ,JK,NSV_LGBEG)-PSVM(IIU-2,JJ,JK,NSV_LGBEG)
  END DO
END DO
!
DO JK=1,IKU
  DO JI=1,IIU
    DO JJ=1,IJU-1
      PSVM(JI,JJ,JK,NSV_LGBEG+1)=0.5*(PYHAT(JJ)+PYHAT(JJ+1))
    END DO
    PSVM(JI,IJU,JK,NSV_LGBEG+1)=2.*PSVM(JI,IJU-1,JK,NSV_LGBEG+1)-PSVM(JI,IJU-2,JK,NSV_LGBEG+1)
  END DO
END DO
!
DO JI=1,IIU
  DO JJ=1,IJU
    DO JK=1,IKU-1
      PSVM(JI,JJ,JK,NSV_LGEND)=0.5*(PZZ(JI,JJ,JK)+PZZ(JI,JJ,JK+1))
    END DO
    PSVM(JI,JJ,IKU,NSV_LGEND)=2.*PSVM(JI,JJ,IKU-1,NSV_LGEND)-PSVM(JI,JJ,IKU-2,NSV_LGEND)
  END DO
END DO
!
PSVT(:,:,:,NSV_LGBEG:NSV_LGEND)=PSVM(:,:,:,NSV_LGBEG:NSV_LGEND)
!
!*       3.    SET LB
!              ------
!
IF (LWEST_ll()) PLBXSVM(1,:,:,NSV_LGBEG:NSV_LGEND)=PSVM(1,:,:,NSV_LGBEG:NSV_LGEND)
IF (LEAST_ll()) PLBXSVM(SIZE(PLBXSVM,1),:,:,NSV_LGBEG:NSV_LGEND)=PSVM(IIU,:,:,NSV_LGBEG:NSV_LGEND)
IF ( SIZE(PLBYSVM,1) >0 ) THEN
  IF(LSOUTH_ll()) PLBYSVM(:,1,:,NSV_LGBEG:NSV_LGEND)=PSVM(:,1,:,NSV_LGBEG:NSV_LGEND)
  IF(LNORTH_ll()) PLBYSVM(:,SIZE(PLBYSVM,2),:,NSV_LGBEG:NSV_LGEND)=PSVM(:,IJU,:,NSV_LGBEG:NSV_LGEND)
END IF
!
END SUBROUTINE INI_LG
