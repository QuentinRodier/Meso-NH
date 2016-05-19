!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/06/27 13:52:59
!-----------------------------------------------------------------
!     ##################
      MODULE MODD_IO_SURF_MNH
!     ##################
!
!!****  *MODD_IO_SURF_MNH - 
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	S.Malardel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!
!
!*       0.   DECLARATIONS
!
IMPLICIT NONE
CHARACTER(LEN=28),SAVE :: CFILE       ! Name of the input FM-file
CHARACTER(LEN=28),SAVE :: COUTFILE    ! Name of the output FM-file
CHARACTER(LEN=28),SAVE :: COUT        ! Name of output_listing file
INTEGER                :: NLUOUT      ! output listing logical unit
CHARACTER(LEN=6),SAVE          :: CMASK
INTEGER, DIMENSION(:), POINTER :: NMASK=>NULL()     ! 1D mask to read only interesting surface
!                                           ! points on current processor
INTEGER, DIMENSION(:), POINTER :: NMASK_ALL=>NULL() ! 1D mask to read all surface points all processors
!
CHARACTER(LEN=5),SAVE          :: CACTION = '     '! action being done ('READ ','WRITE')
!
! number of points in each direction on current processor
INTEGER                              :: NIU,NJU
! indices of physical points in each direction on current processor
INTEGER                              :: NIB,NJB,NIE,NJE
! number of points in each direction on all processors
INTEGER                              :: NIU_ALL,NJU_ALL
! indices of physical points in each direction on all processors
INTEGER                              :: NIB_ALL,NJB_ALL,NIE_ALL,NJE_ALL
!
INTEGER                              :: NHALO = 0
! number of points added on each side (N,E,S,W) to the fields
! the HALO is added   when the field is read    (works only for grid coordinates)
!  note that at reading, this also modifies the numbers of points (IMAX, JMAX)
! the HALO is removed when the field is written (works for all fields)
!
END MODULE MODD_IO_SURF_MNH
