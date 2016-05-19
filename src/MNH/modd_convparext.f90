!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######spl
      MODULE MODD_CONVPAREXT
!     ######################
!
IMPLICIT NONE
!
INTEGER, SAVE :: JCVEXB ! start vertical computations at
                        ! 1 + JCVEXB = 1 + ( KBDIA - 1 )
INTEGER, SAVE :: JCVEXT ! limit vertical computations to
                        ! KLEV - JCVEXT = KLEV - ( KTDIA - 1 )
!
END MODULE MODD_CONVPAREXT
