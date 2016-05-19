!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! ECMWF_RAD2 2003/02/19 13:36:36
!-----------------------------------------------------------------
MODULE PARKIND2
!
!     *** Define huge kinds for strong typing ***
!
IMPLICIT NONE
SAVE
!
!     Integer Kinds
!     -------------
!
INTEGER, PARAMETER :: JPIH = SELECTED_INT_KIND(18)
!
!     Real Kinds
!     ----------
!
#ifdef REALHUGE
INTEGER, PARAMETER :: JPRH = SELECTED_REAL_KIND(28,2400)
#else
INTEGER, PARAMETER :: JPRH = SELECTED_REAL_KIND(13,300)
#endif
!
END MODULE PARKIND2
