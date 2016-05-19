!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######spl
      MODULE MODD_PGDZ0DWORK
!     ######################
!
!!****  *MODD_PGDZ0DWORK* - declaration of work arrays and variables
!!                          for directional z0 computation
!!
!!    PURPOSE
!!    -------  
!!
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
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/09/95                      
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!
!
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZMAXSSQ ! maximum of orography in a
!                                               ! subgrid square
LOGICAL, DIMENSION(:,:), ALLOCATABLE :: GSSQ    ! presence of data in a subgrid
!                                               ! square
INTEGER :: ISSQX                                ! number of subgrid squares in 
INTEGER :: ISSQY                                ! each direction in grid mesh
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_PGDZ0DWORK
