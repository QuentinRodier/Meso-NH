!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 tools 2006/05/18 13:07:25
!-----------------------------------------------------------------
SUBROUTINE SECOND_MNH(XT)

IMPLICIT NONE
REAL           :: XT
REAL           :: ZT
call cpu_time(ZT)
XT=ZT
END SUBROUTINE SECOND_MNH

