!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2007/03/27 09:58:10
!-----------------------------------------------------------------
!     #################
      MODULE MODN_ADV_n
!     #################
!
!!****  *MODN_ADV$n* - declaration of namelist NAM_ADVn
!!
!!    PURPOSE
!!    -------     
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_ADV$n : contains declaration of scalar advection schemes
!!      parameters
!!
!!    REFERENCE
!!    ---------
!!          
!!    AUTHOR
!!    ------
!!	Vila, Lafore   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/10/95  (Vila, Lafore) Implementation scalar advection scheme
!!      C.Lac       24/04/06  Introduction of CUVW_ADV_SCHEME and
!!                            removal of CFV_ADV_SCHEME
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_ADV_n, ONLY: &
         CUVW_ADV_SCHEME_n => CUVW_ADV_SCHEME, &
         CMET_ADV_SCHEME_n => CMET_ADV_SCHEME, &
         CSV_ADV_SCHEME_n => CSV_ADV_SCHEME, &
         NLITER_n => NLITER
!
IMPLICIT NONE
!
CHARACTER(LEN=6)  :: CUVW_ADV_SCHEME
CHARACTER(LEN=6)  :: CMET_ADV_SCHEME
CHARACTER(LEN=6)  :: CSV_ADV_SCHEME
INTEGER  :: NLITER
!
NAMELIST/NAM_ADVn/CUVW_ADV_SCHEME,CMET_ADV_SCHEME,CSV_ADV_SCHEME,NLITER
!
CONTAINS
!
SUBROUTINE INIT_NAM_ADVn
  CUVW_ADV_SCHEME = CUVW_ADV_SCHEME_n
  CMET_ADV_SCHEME = CMET_ADV_SCHEME_n
  CSV_ADV_SCHEME = CSV_ADV_SCHEME_n
  NLITER = NLITER_n
END SUBROUTINE INIT_NAM_ADVn

SUBROUTINE UPDATE_NAM_ADVn
  CUVW_ADV_SCHEME_n = CUVW_ADV_SCHEME
  CMET_ADV_SCHEME_n = CMET_ADV_SCHEME
  CSV_ADV_SCHEME_n = CSV_ADV_SCHEME
  NLITER_n = NLITER
END SUBROUTINE UPDATE_NAM_ADVn

END MODULE MODN_ADV_n
