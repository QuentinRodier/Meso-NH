FUNCTION INDEX1 (HNAME, HLIST) RESULT(KINDEX1)

!***********************************************************************
! Version "$Id: index1.f 45 2014-09-12 20:05:29Z coats $"
! EDSS/Models-3 I/O API.
! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
! (C) 2003-2010 Baron Advanced Meteorological Systems, LLC.
! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
! See file "LGPL.txt" for conditions of use.
!.........................................................................
!  INDEX1    subroutine body starts at line 53
!  INDEXINT1 subroutine body starts at line 99
!
!  FUNCTION:
!
!       Search for character-string or integer key NAME or IKEY in list NLIST
!       and return the subscript (1...N) at which it is found, or return 0
!       when not found in NLIST
!
!  PRECONDITIONS REQUIRED:
!       none
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!       none
!
!  REVISION HISTORY:
!    INDEX1:
!       5/1988   Modified for ROMNET
!       9/1994   Modified for Models-3 by CJC
!    INDEXINT1:
!       Prototype 11/2004 by CJC:  MODULE M3UTILIO for I/O API v3
!       Modified   3/2006 by CJC:  moved INDEXINT1() to file "index1.f"
!
!       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
!***********************************************************************

IMPLICIT NONE

!.......   Arguments and their descriptions:

CHARACTER(LEN=*), INTENT(IN) :: HNAME      !  Character string being searched for
CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HLIST  !  array to be searched
INTEGER :: KINDEX1

!.......   Local variable:

INTEGER :: JI   !  loop counter

!.....................................................................
!.......   begin body of INDEX1()

KINDEX1 = 0
!
DO JI = 1, SIZE(HLIST)
  IF ( HNAME.EQ.HLIST(JI) ) THEN    ! Found NAME in NLIST
    KINDEX1 = JI
    EXIT
  ENDIF
END DO

END FUNCTION INDEX1

! --=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


!FUNCTION INDEXINT1(KEY, KEYLIST ) RESULT(KINDEXINT1)
!
!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!!  Look up integer key IKEY in unsorted list <NLIST,KEYLIST>
!!  of integer keys.  Return the subscript at which IKEY
!!  occurs, or 0 in case of failure
!!
!!  PRECONDITIONS REQUIRED:
!!      none
!!
!!  REVISION  HISTORY:
!!      Prototype  11/2004 by CJC
!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!
!IMPLICIT NONE
!
!!!........  Arguments:
! 
!INTEGER, INTENT(IN) :: KEY
!INTEGER, DIMENSION(:), INTENT(IN) :: KEYLIST
!
!INTEGER :: KINDEXINT1
!
!!!........  Local Variables:
!
!INTEGER :: JI
!
!!!........  begin body ........................................
!
!KINDEXINT1 = 0
!
!DO JI = 1, SIZE(KEYLIST)
!  IF ( KEY .EQ. KEYLIST(JI) ) THEN
!    KINDEXINT1 = JI
!    EXIT
!  END IF
!END DO
!
!END FUNCTION INDEXINT1
