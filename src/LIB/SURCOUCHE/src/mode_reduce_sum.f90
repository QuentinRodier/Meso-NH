!MNH_LIC Copyright 1998-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 26/04/2019: use modd_precision parameters for datatypes of MPI communications
!  P. Wautelet 21/06/2019: mode REDUCESUM_ll subroutine to mode_reduce_sum.f90 (to remove circular dependencies between modules)
!-----------------------------------------------------------------
module mode_reduce_sum

USE MODD_MPIF
use modd_precision, only: MNHINT_MPI, MNHREAL_MPI
USE MODD_VAR_ll,    ONLY: NMNH_COMM_WORLD
USE modd_repro_sum

implicit none

INTERFACE REDUCESUM_ll
   MODULE PROCEDURE REDUCE_SUM_0DD_ll, REDUCE_SUM_1DD_ll,                                      &
                    REDUCE_SUM_0D_ll,  REDUCE_SUM_1D_ll,  REDUCE_SUM_2D_ll,  REDUCE_SUM_3D_ll, &
                    REDUCE_SUM_I0D_ll, REDUCE_SUM_I1D_ll, REDUCE_SUM_I2D_ll, REDUCE_SUM_I3D_ll
END INTERFACE

contains

  SUBROUTINE INIT_DD(KINFO)
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: KINFO ! MPI return status
    !
    ! define the double-double for MPI
    !
    CALL MPI_TYPE_CONTIGUOUS(2, MNHREAL_MPI ,MNH_DOUBLE_DOUBLE , KINFO)
    CALL MPI_TYPE_COMMIT(MNH_DOUBLE_DOUBLE , KINFO)
    !
    ! define the double-double sum = MNH_SUM_DD  for MPI
    !
    CALL MPI_OP_CREATE(DDPDD, .TRUE., MNH_SUM_DD, KINFO)
    FIRST_CALL_DD = .FALSE.
    !
  END SUBROUTINE INIT_DD

  PURE SUBROUTINE DDPDD (dda, ddb, len, itype)
    !----------------------------------------------------------------------
    !
    ! Purpose:
    ! Modification of original codes written by David H. Bailey
    ! This subroutine computes ddb(i) = dda(i)+ddb(i)
    ! for use with MPI_*_REDUCE
    !
    !----------------------------------------------------------------------
    !
    ! Arguments
    !
    INTEGER, INTENT(in)                :: len       ! array length
    TYPE(DOUBLE_DOUBLE), INTENT(in)    :: dda(len)  ! input
    TYPE(DOUBLE_DOUBLE), INTENT(inout) :: ddb(len)  ! result
    INTEGER, INTENT(in)                :: itype     ! unused
    !
    ! Local workspace
    !
    REAL e, t1, t2
    INTEGER i
    !
    !-----------------------------------------------------------------------
    !
    DO i = 1, len
       !
       !   Compute dda + ddb using Knuth's trick.
       !
       t1 = dda(i)%R + ddb(i)%R
       e  = t1 - dda(i)%R
       t2 = ((ddb(i)%R - e) + (dda(i)%R - (t1 - e))) &
            + dda(i)%E + ddb(i)%E
       !
       !   The result is t1 + t2, after normalization.
       !
       ddb(i)%R = t1 + t2
       ddb(i)%E = t2 - ((t1 + t2) - t1)
    ENDDO

    RETURN

  END SUBROUTINE DDPDD

!     ########################################
      SUBROUTINE REDUCE_SUM_0DD_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_0DD_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the scalar argument PRES on processors.
!
!     REDUCE_SUM_0Q_ll is the routine for scalar REAL*16 argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_0Q_ll, each processor
!     computes its local sum PRES; in REDUCE_SUM_0Q_ll
!     we reduce this values and return the global sum
!     in the PRES variable  REAL*16.
!
!!    External
!!    --------
!
!!    Implicit Arguments
!!    ------------------
!
!
!!    Author
!!    ------
!     Ph. Kloos      * CNRM - CERFACS *
!
!!    Modifications
!!    -------------
!     Original 27/06/98
!     R. Guivarch 09/07/98 Same argument PRES INOUT
!
!-----------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  TYPE(DOUBLE_DOUBLE), INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  TYPE(DOUBLE_DOUBLE)  :: ZRES ! sum
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  IF (FIRST_CALL_DD) CALL INIT_DD(KINFO)
  ZRES%R = 0.0 ;  ZRES%E = 0.0
  CALL MPI_ALLREDUCE(PRES, ZRES, 1, MNH_DOUBLE_DOUBLE , &
                     MNH_SUM_DD, NMNH_COMM_WORLD, KINFO)

  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE REDUCE_SUM_0DD_ll
!
!     ########################################
      SUBROUTINE REDUCE_SUM_0D_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_0D_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the scalar argument PRES on processors.
!
!     REDUCE_SUM_0D_ll is the routine for scalar argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_0D_ll, each processor
!     computes its local sum PRES; in REDUCE_SUM_0D_ll
!     we reduce this values and return the global sum
!     in the PRES variable.
!
!!    External
!!    --------
!
!!    Implicit Arguments
!!    ------------------
!
!
!!    Author
!!    ------
!     Ph. Kloos      * CNRM - CERFACS *
!
!!    Modifications
!!    -------------
!     Original 27/06/98
!     R. Guivarch 09/07/98 Same argument PRES INOUT
!
!-----------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  REAL, INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  REAL :: ZRES ! Intermediate result
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  CALL MPI_ALLREDUCE(PRES, ZRES, 1, MNHREAL_MPI, &
                     MPI_SUM, NMNH_COMM_WORLD, KINFO)
!
  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE REDUCE_SUM_0D_ll
!
!     ########################################
      SUBROUTINE REDUCE_SUM_1DD_ll(PRES, KINFO)
!     ########################################
!

!!    Author
!!    ------
!     J.Escobar 22/10/2010
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
   TYPE(DOUBLE_DOUBLE), DIMENSION(:), INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  TYPE(DOUBLE_DOUBLE), DIMENSION(SIZE(PRES,1)) :: ZRES ! Intermediate sum

!
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  IF (FIRST_CALL_DD) CALL INIT_DD(KINFO)
  ZRES%R = 0.0 ;  ZRES%E = 0.0
  CALL MPI_ALLREDUCE(PRES, ZRES, SIZE(PRES), MNH_DOUBLE_DOUBLE , &
                     MNH_SUM_DD, NMNH_COMM_WORLD, KINFO)
PRES = ZRES
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE REDUCE_SUM_1DD_ll
!

!     ########################################
      SUBROUTINE REDUCE_SUM_1D_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_1D_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the each entry of the one-dimensional vector PRES
!     on all processors.
!
!     REDUCE_SUM_1D_ll is the routine for 1D argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_1D_ll, each processor
!     computes its local 1D sum PRES; in REDUCE_SUM_1D_ll
!     we reduce this values and return the global sum
!     in the PRES variable.
!
!!    External
!!    --------
!
!!    Implicit Arguments
!!    ------------------
!
!
!!    Author
!!    ------
!     Ph. Kloos      * CNRM - CERFACS *
!
!!    Modifications
!!    -------------
!     Original 27/06/98
!     R. Guivarch 09/07/98 Same argument PRES INOUT
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  REAL, DIMENSION(:), INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  REAL, DIMENSION(SIZE(PRES,1)) :: ZRES ! Intermediate sum
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  CALL MPI_ALLREDUCE(PRES, ZRES, SIZE(PRES,1), MNHREAL_MPI, &
                     MPI_SUM, NMNH_COMM_WORLD, KINFO)
!
  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE REDUCE_SUM_1D_ll
!
!     ########################################
      SUBROUTINE REDUCE_SUM_2D_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_2D_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the each entry of the two-dimensional vector PRES
!     on all processors.
!
!     REDUCE_SUM_2D_ll is the routine for 2D argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_2D_ll, each processor
!     computes its local 2D sum PRES; in REDUCE_SUM_2D_ll
!     we reduce this values and return the global sum
!     in the PRES variable.
!
!!    External
!!    --------
!
!!    Implicit Arguments
!!    ------------------
!
!
!!    Author
!!    ------
!     Ph. Kloos      * CNRM - CERFACS *
!
!!    Modifications
!!    -------------
!     Original 27/06/98
!     R. Guivarch 09/07/98 Same argument PRES INOUT
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  REAL, DIMENSION(:,:), INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  REAL, DIMENSION(SIZE(PRES,1),SIZE(PRES,2)) :: ZRES ! Intermediate sum
!
  INTEGER :: IDIM
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  IDIM = SIZE(PRES,1) * SIZE(PRES,2)
!
  CALL MPI_ALLREDUCE(PRES, ZRES, IDIM, MNHREAL_MPI, MPI_SUM, &
                     NMNH_COMM_WORLD, KINFO)
!
  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE REDUCE_SUM_2D_ll
!
!     ########################################
      SUBROUTINE REDUCE_SUM_3D_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_3D_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the each entry of the three-dimensional vector PRES
!     on all processors.
!
!     REDUCE_SUM_3D_ll is the routine for 3D argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_3D_ll, each processor
!     computes its local 3D sum PRES; in REDUCE_SUM_3D_ll
!     we reduce this values and return the global sum
!     in the PRES variable.
!
!!    External
!!    --------
!
!!    Implicit Arguments
!!    ------------------
!
!
!!    Author
!!    ------
!     Ph. Kloos      * CNRM - CERFACS *
!
!!    Modifications
!!    -------------
!     Original 27/06/98
!     R. Guivarch 09/07/98 Same argument PRES INOUT
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  REAL, DIMENSION(SIZE(PRES,1),SIZE(PRES,2),SIZE(PRES,3)) :: ZRES ! Intermediate
                                                                  ! sum
!
  INTEGER :: IDIM
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  IDIM = SIZE(PRES,1) * SIZE(PRES,2) * SIZE(PRES,3)
!
  CALL MPI_ALLREDUCE(PRES, ZRES, IDIM, MNHREAL_MPI, MPI_SUM, &
                     NMNH_COMM_WORLD, KINFO)
!
  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE REDUCE_SUM_3D_ll
!
!     ########################################
      SUBROUTINE REDUCE_SUM_I0D_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_I0D_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the scalar argument PRES on processors.
!
!     REDUCE_SUM_I0D_ll is the routine for integer scalar argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_I0D_ll, each processor
!     computes its local sum PRES; in REDUCE_SUM_I0D_ll
!     we reduce this values and return the global sum
!     in the PRES variable.
!
!!    External
!!    --------
!
!!    Author
!!    ------
!     D. Gazen  * L.A. *
!
!!    Modifications
!!    -------------
!     Original 4/09/2000
!
!-----------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  INTEGER, INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  INTEGER :: ZRES ! Intermediate result
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  CALL MPI_ALLREDUCE(PRES, ZRES, 1, MNHINT_MPI, &
                     MPI_SUM, NMNH_COMM_WORLD, KINFO)
!
  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE REDUCE_SUM_I0D_ll
!
!     ########################################
      SUBROUTINE REDUCE_SUM_I1D_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_I1D_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the each entry of the one-dimensional vector PRES
!     on all processors.
!
!     REDUCE_SUM_I1D_ll is the routine for integer 1D argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_I1D_ll, each processor
!     computes its local 1D sum PRES; in REDUCE_SUM_I1D_ll
!     we reduce this values and return the global sum
!     in the PRES variable.
!
!!    External
!!    --------
!
!!    Author
!!    ------
!     D. Gazen  * L.A. *
!
!!    Modifications
!!    -------------
!     Original 4/09/2000
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  INTEGER, DIMENSION(:), INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  INTEGER, DIMENSION(SIZE(PRES,1)) :: ZRES ! Intermediate sum
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  CALL MPI_ALLREDUCE(PRES, ZRES, SIZE(PRES,1), MNHINT_MPI, &
                     MPI_SUM, NMNH_COMM_WORLD, KINFO)
!
  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE REDUCE_SUM_I1D_ll
!
!     ########################################
      SUBROUTINE REDUCE_SUM_I2D_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_2D_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the each entry of the two-dimensional vector PRES
!     on all processors.
!
!     REDUCE_SUM_I2D_ll is the routine for integer 2D argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_I2D_ll, each processor
!     computes its local 2D sum PRES; in REDUCE_SUM_I2D_ll
!     we reduce this values and return the global sum
!     in the PRES variable.
!
!!    External
!!    --------
!
!!    Author
!!    ------
!     D. Gazen  * L.A. *
!
!!    Modifications
!!    -------------
!     Original 4/09/2000
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  INTEGER, DIMENSION(:,:), INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  INTEGER, DIMENSION(SIZE(PRES,1),SIZE(PRES,2)) :: ZRES ! Intermediate sum
!
  INTEGER :: IDIM
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  IDIM = SIZE(PRES,1) * SIZE(PRES,2)
!
  CALL MPI_ALLREDUCE(PRES, ZRES, IDIM, MNHINT_MPI, MPI_SUM, &
                     NMNH_COMM_WORLD, KINFO)
!
  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE REDUCE_SUM_I2D_ll
!
!     ########################################
      SUBROUTINE REDUCE_SUM_I3D_ll(PRES, KINFO)
!     ########################################
!
!!****  *REDUCE_SUM_I3D_ll*-
!
!!    Purpose
!!    -------
!     This routine calculates the sum of the values
!     of the each entry of the three-dimensional vector PRES
!     on all processors.
!
!     REDUCE_SUM_I3D_ll is the routine for 3D argument
!     of the generic routine REDUCESUM_ll.
!
!!    Method
!!    ------
!     Before the call to REDUCE_SUM_I3D_ll, each processor
!     computes its local 3D sum PRES; in REDUCE_SUM_I3D_ll
!     we reduce this values and return the global sum
!     in the PRES variable.
!
!!    External
!!    --------
!
!!    Author
!!    ------
!     D. Gazen  * L.A. *
!
!!    Modifications
!!    -------------
!     Original 4/09/2000
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
  INTEGER, DIMENSION(:,:,:), INTENT(INOUT) :: PRES ! sum
!
  INTEGER, INTENT(OUT) :: KINFO ! MPI return status
!
!*       0.2   Declarations of local variables :
!
  INTEGER, DIMENSION(SIZE(PRES,1),SIZE(PRES,2),SIZE(PRES,3)) :: ZRES ! Intermediate
                                                                  ! sum
!
  INTEGER :: IDIM
!
!-------------------------------------------------------------------------------
!
!*       1. CALL THE MPI_ALLREDUCE ROUTINE
!           ------------------------------
!
  IDIM = SIZE(PRES,1) * SIZE(PRES,2) * SIZE(PRES,3)
!
  CALL MPI_ALLREDUCE(PRES, ZRES, IDIM, MNHINT_MPI, MPI_SUM, &
                     NMNH_COMM_WORLD, KINFO)
!
  PRES = ZRES
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE REDUCE_SUM_I3D_ll
!
!-------------------------------------------------------------------------------
!
end module mode_reduce_sum
