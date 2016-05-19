!     ################
MODULE MODD_PREP_FLAKE
!     ################
!
!!****  *MODD_PREP_FLAKE - declaration for field interpolations
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	S.Malardel    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/03
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
SAVE
!--------------------------------------------------------------------------
!
 CHARACTER(LEN=28) :: CFILE_FLAKE   ! input file name
 CHARACTER(LEN=6)  :: CTYPE         ! input file type
 CHARACTER(LEN=28) :: CFILEPGD_FLAKE   ! input file name
 CHARACTER(LEN=6)  :: CTYPEPGD         ! input file type
!
REAL              :: XTS_UNIF   !  uniform prescribed 
                                !  surface temperature for inland water
! Uniform prescribed of FLake variables:
REAL              :: XUNIF_T_SNOW    
REAL              :: XUNIF_T_ICE        
REAL              :: XUNIF_T_MNW        
REAL              :: XUNIF_T_WML        
REAL              :: XUNIF_T_BOT        
REAL              :: XUNIF_T_B1         
REAL              :: XUNIF_CT           
REAL              :: XUNIF_H_SNOW       
REAL              :: XUNIF_H_ICE        
REAL              :: XUNIF_H_ML            
REAL              :: XUNIF_H_B1      
!
LOGICAL           :: LCLIM_LAKE ! Do we want to use lake climate data? T if yes.
!
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP_FLAKE


