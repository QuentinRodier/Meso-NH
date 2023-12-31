! (C) Copyright 2014- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

SUBROUTINE GETMEMSTAT(KOUT, CDLABEL)

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPIB

! USE MPL_MODULE
USE MPL_MYRANK_MOD
USE MPL_NPROC_MOD
USE MPL_GATHERV_MOD

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN) :: KOUT
CHARACTER(LEN=*), INTENT(IN) :: CDLABEL
INTEGER(KIND=JPIM) :: I, IMYPROC, INPROC, IOFFSET
INTEGER(KIND=JPIM), PARAMETER :: JP_MEMKEYS = 5  ! pls. consult ifsaux/utilities/getmemvals.F90
INTEGER(KIND=JPIM) IMEMKEYS(JP_MEMKEYS)
INTEGER(KIND=JPIB) IMEMVALS(JP_MEMKEYS)
REAL(KIND=JPRB), ALLOCATABLE :: ZSEND(:), ZRECV(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: ICOUNTS(:)
CHARACTER(LEN=1) CLENV

CALL GET_ENVIRONMENT_VARIABLE("EC_PROFILE_MEM", CLENV) ! turn OFF by export EC_PROFILE_MEM=0

IF (KOUT >= 0 .AND. CLENV /= '0') THEN
  IMYPROC = MPL_MYRANK()
  INPROC  = MPL_NPROC()

  ALLOCATE(ZSEND(JP_MEMKEYS))
  ALLOCATE(ZRECV(JP_MEMKEYS * INPROC))
  ALLOCATE(ICOUNTS(INPROC))

!                 1=MAXHEAP, 2=MAXRSS, 3=CURRENTHEAP, 5=MAXSTACK, 6=PAGING
  IMEMKEYS(:) = (/1,         2,        3,             5,          6/) 
  CALL GETMEMVALS(JP_MEMKEYS, IMEMKEYS, IMEMVALS)

  ZSEND(:) = 0
  DO I=1,JP_MEMKEYS
    ZSEND(I) = IMEMVALS(I)
  ENDDO
  ZRECV(:) = -1

  ICOUNTS(:) = JP_MEMKEYS
  CALL MPL_GATHERV(ZSEND(:), KROOT=1, KRECVCOUNTS=ICOUNTS(:), &
                  &PRECVBUF=ZRECV, CDSTRING='GETMEMSTAT:')

  IF (IMYPROC == 1) THEN
     WRITE(KOUT,9000) TRIM(CDLABEL)
9000 FORMAT(/,"Memory Utilization Information (in bytes) : ",a,/,79("="),//,&
         &  "Node   Max heapsize   Max resident   Current heap      Max stack   I/O-paging #",/,&
         &  "====   ============   ============   ============   ============   ============",//)
     IOFFSET = 0
     DO I=1,INPROC
       IMEMVALS(:) = ZRECV(IOFFSET+1:IOFFSET+JP_MEMKEYS)
       WRITE(KOUT,'(I4,5(3X,I12))') I,IMEMVALS(:)
       IOFFSET = IOFFSET + JP_MEMKEYS
     ENDDO
     WRITE(KOUT,'(/,a,/)') 'End of Memory Utilization Information'
  ENDIF

  DEALLOCATE(ZSEND)
  DEALLOCATE(ZRECV)
  DEALLOCATE(ICOUNTS)

  CALL GETHEAPSTAT(KOUT, CDLABEL)
ENDIF
END SUBROUTINE GETMEMSTAT
