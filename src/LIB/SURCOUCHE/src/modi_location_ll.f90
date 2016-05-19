!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

!     #######################
      MODULE MODI_LOCATION_ll
!     #######################
!
INTERFACE
!
!!     ###########################################
       LOGICAL FUNCTION LNORTH_ll( K, HSPLITTING )
!!     ###########################################
!
  INTEGER, INTENT(IN), OPTIONAL     :: K ! number of the subdomain
  CHARACTER*1, INTENT(IN), OPTIONAL :: HSPLITTING ! kind of splitting
!
       END FUNCTION LNORTH_ll
!
!!     ##########################################
       LOGICAL FUNCTION LWEST_ll( K, HSPLITTING )
!!     ##########################################
!
  INTEGER, INTENT(IN), OPTIONAL     :: K ! number of the subdomain
  CHARACTER*1, INTENT(IN), OPTIONAL :: HSPLITTING ! kind of splitting
!
       END FUNCTION LWEST_ll
!
!!     ###########################################
       LOGICAL FUNCTION LSOUTH_ll( K, HSPLITTING )
!!     ###########################################
!
  INTEGER, INTENT(IN), OPTIONAL     :: K ! number of the subdomain
  CHARACTER*1, INTENT(IN), OPTIONAL :: HSPLITTING ! kind of splitting
!
       END FUNCTION LSOUTH_ll
!
!!     ##########################################
       LOGICAL FUNCTION LEAST_ll( K, HSPLITTING )
!!     ##########################################
!
  INTEGER, INTENT(IN), OPTIONAL     :: K ! number of the subdomain
  CHARACTER*1, INTENT(IN), OPTIONAL :: HSPLITTING ! kind of splitting
!
       END FUNCTION LEAST_ll
!
END INTERFACE
!
END MODULE MODI_LOCATION_ll
