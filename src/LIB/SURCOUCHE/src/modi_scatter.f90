!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

!     ######spl
      MODULE MODI_SCATTER
!     #####################
!
INTERFACE
      SUBROUTINE SCATTER(P1,P2)
!
REAL, DIMENSION(:,:),  INTENT(IN)    :: P1
REAL, DIMENSION(:,:),  INTENT(OUT)   :: P2
!
END SUBROUTINE SCATTER
!
END INTERFACE
!
END MODULE MODI_SCATTER
