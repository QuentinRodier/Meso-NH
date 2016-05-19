!     #####################
      MODULE MODD_CH_SURF
!     ######################
!
!!
!!    PURPOSE
!!    -------
!  this module is for the surface scheme only     
!   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!
!!    AUTHOR
!!    ------
!!  P. Tulet  (16/01/01) *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER,PARAMETER :: JPEMISMAX_F = 10000
INTEGER,PARAMETER :: JPEMISMAX_S = 1000
INTEGER,PARAMETER :: JPSNAPMAX = 50
!
REAL, SAVE, DIMENSION(:),   ALLOCATABLE :: XSREALMASSMOLVAL ! final molecular
                                                            ! diffusivity value
REAL, SAVE, DIMENSION(:),   ALLOCATABLE :: XSREALREACTVAL   ! final chemical
                                                            ! reactivity factor
                                                            ! with biology
REAL, SAVE, DIMENSION(:,:), ALLOCATABLE :: XSREALHENRYVAL   ! chemical Henry
                                                            ! constant value
!
END MODULE MODD_CH_SURF


