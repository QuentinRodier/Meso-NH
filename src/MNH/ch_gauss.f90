!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 chimie 2006/05/18 13:07:25
!-----------------------------------------------------------------
!!    ############################# 
      MODULE MODI_CH_GAUSS
!!    ############################# 

INTERFACE

SUBROUTINE CH_GAUSS(PIN,POUT,KDIM,KFAIL)
IMPLICIT NONE
INTEGER, INTENT(IN)                     :: KDIM   ! dimension of the matrix
INTEGER, INTENT(INOUT)                  :: KFAIL  ! error flag
REAL, DIMENSION(KDIM,KDIM), INTENT(IN)  :: PIN    ! matrix to be inverted
REAL, DIMENSION(KDIM,KDIM), INTENT(OUT) :: POUT   ! inverse matrix
END SUBROUTINE CH_GAUSS

END INTERFACE

END MODULE MODI_CH_GAUSS
!!    #############################
      SUBROUTINE CH_GAUSS(PIN,POUT,KDIM,KFAIL)
!!    #############################

!!****  *CH_GAUSS*-inversion of a squared matrix using pivot search

!!    PURPOSE
!!    -------
!       Inversion of a real squared matrix

!!    METHOD
!!    ------
!       This routine inverts the KDIMxKDIM matrix PIN using the Gauss-Jordan 
!     algorithm with pivot-element search and returns its inverse 
!     in the KDIMxKDIM matrix POUT. If the inversion fails (in the case where
!     the matrix PIN is singulary), the variable KFAIL will be set to 1.
!     If KFAIL is set to 0 on entry, the subroutine will stop. If 
!     KFAIL is negative, the program continues silently, otherwise an 
!     error message is printed.

!!    REFERENCE
!!    ---------
!     J. Stoer: Einf\"uhrung in die Numerische Mathematik I,
!     Heidelberger Taschenb\"ucher, Springer Verlag, Berlin, 1983.

!!    AUTHOR
!!    ------
!!    K. Suhre

!!    MODIFICATIONS
!!    -------------
!!    Original 24/02/95 (adapted from FORTRAN77 version in tools.k)
!!    27/02/95 (K. Suhre) put in some more array syntax

!!    EXTERNAL
!!    --------
!!    none

!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    none

!!    EXPLICIT ARGUMENTS
!!    ------------------
IMPLICIT NONE
INTEGER, INTENT(IN)                     :: KDIM   ! dimension of the matrix
INTEGER, INTENT(INOUT)                  :: KFAIL  ! error flag
REAL, DIMENSION(KDIM,KDIM), INTENT(IN)  :: PIN    ! matrix to be inverted
REAL, DIMENSION(KDIM,KDIM), INTENT(OUT) :: POUT   ! inverse matrix

!!    DECLARATION OF LOCAL CONSTANTS
!!    ------------------------------
REAL, PARAMETER            :: PPEPS = 1.0E-20 ! lower bound for pivot element

!!    DECLARATION OF LOCAL VARIABLES
!!    ------------------------------
INTEGER                    :: IPIVOT           ! current pivot element
INTEGER, DIMENSION(KDIM)   :: IORDER           ! to stock the permutations
INTEGER                    :: JI, JJ, JK, IMEM ! integer variables
INTEGER, DIMENSION(1)      :: IHELP            ! help array for MAXLOC
REAL, DIMENSION(KDIM,KDIM) :: ZWORK            ! work array
REAL                       :: ZMAX, ZMEM       ! real variables
REAL, DIMENSION(KDIM)      :: ZCOL             ! 1D work array

!!    EXECUTABLE STATEMENTS
!!    ---------------------

!*    copy PIN into workarray ZWORK
ZWORK(:,:) = PIN(:,:)

!*    initialize permutation array IORDER
IORDER = (/ (JJ,JJ = 1, KDIM) /)

!*    outer loop for matrix elimination
elimination_loop : DO JJ = 1, KDIM

!*    pivot search
  ZMAX = MAXVAL(ABS(ZWORK(JJ:KDIM,JJ)))
  IHELP = MAXLOC(ABS(ZWORK(JJ:KDIM,JJ)))
  IPIVOT = IHELP(1) + JJ - 1

!*    check for singulary matrix, print error message and stop 
!*    if this is requested by KFAIL (see above for possible values for KFAIL)
  error : IF (ZMAX.LE.PPEPS) THEN
    IF (KFAIL.GE.0) THEN
      PRINT *, "Error message from subroutine CH_GAUSS: ", &
	       "singulary matrix cannot be inverted!"
    ENDIF
    IF (KFAIL.EQ.0) THEN
       !callabortstop
      CALL ABORT
      STOP 1
    ENDIF
    KFAIL = 1
    RETURN
  ENDIF error

!*    interchange the lines
  IF (IPIVOT.GT.JJ) THEN
    ZCOL = ZWORK(JJ,:)
    ZWORK(JJ,:) = ZWORK(IPIVOT,:)
    ZWORK(IPIVOT,:) = ZCOL
    IMEM = IORDER(JJ)
    IORDER(JJ) = IORDER(IPIVOT)
    IORDER(IPIVOT) = IMEM
  ENDIF

!*    transformation
  ZMEM = 1.0/ZWORK(JJ,JJ)
  ZWORK(:,JJ) = ZMEM * ZWORK(:,JJ)
  ZWORK(JJ,JJ) = ZMEM

  DO JK = 1, JJ-1
    ZWORK(1:JJ-1,JK) = ZWORK(1:JJ-1,JK) - ZWORK(1:JJ-1,JJ)*ZWORK(JJ,JK)
    ZWORK(JJ+1:KDIM,JK) = ZWORK(JJ+1:KDIM,JK) - ZWORK(JJ+1:KDIM,JJ)*ZWORK(JJ,JK)
    ZWORK(JJ,JK) = -ZMEM * ZWORK(JJ,JK)
  ENDDO

  DO JK = JJ+1, KDIM
    ZWORK(1:JJ-1,JK) = ZWORK(1:JJ-1,JK) - ZWORK(1:JJ-1,JJ)*ZWORK(JJ,JK)
    ZWORK(JJ+1:KDIM,JK) = ZWORK(JJ+1:KDIM,JK) - ZWORK(JJ+1:KDIM,JJ)*ZWORK(JJ,JK)
    ZWORK(JJ,JK) = -ZMEM * ZWORK(JJ,JK)
  ENDDO

ENDDO elimination_loop

!*    change columns
DO JI = 1, KDIM
 ZCOL(IORDER(:)) = ZWORK(JI,:)
 POUT(JI,:) = ZCOL(:)
ENDDO

!* set KFAIL to success value 0
KFAIL = 0

RETURN
END SUBROUTINE CH_GAUSS
