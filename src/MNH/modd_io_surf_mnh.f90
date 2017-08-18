!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
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
!!	M.Faivre 2014
!
!*       0.   DECLARATIONS
!
USE MODD_IO_ll, ONLY : TFILEDATA
USE MODD_PARAMETERS, ONLY: JPMODELMAX

IMPLICIT NONE

INTEGER                              :: NHALO = 0

TYPE IO_SURF_MNH_t
!$20140403 JUAN upgraded this modd to have // and mutlimodels use
!$20140403 cancel the SAVE in structure def as made in already // modd in MNH
!$
!CHARACTER(LEN=28),SAVE :: CFILE       ! Name of the input FM-file
!CHARACTER(LEN=28),SAVE :: COUTFILE    ! Name of the output FM-file
!CHARACTER(LEN=28),SAVE :: COUT        ! Name of output_listing file
!INTEGER                :: NLUOUT      ! output listing logical unit
!CHARACTER(LEN=6),SAVE          :: CMASK
CHARACTER(LEN=28)              :: CFILE       ! Name of the input FM-file
TYPE(TFILEDATA),POINTER        :: TPINFILE => NULL() ! Input FM-file
CHARACTER(LEN=28)              :: COUTFILE    ! Name of the output FM-file
CHARACTER(LEN=28)              :: COUT        ! Name of output_listing file
INTEGER                        :: NLUOUT      ! output listing logical unit
CHARACTER(LEN=6)               :: CMASK
INTEGER, DIMENSION(:), POINTER :: NMASK=>NULL()     ! 1D mask to read only interesting surface
!                                           ! points on current processor
INTEGER, DIMENSION(:), POINTER :: NMASK_ALL=>NULL() ! 1D mask to read all surface points all processors
!
CHARACTER(LEN=5)               :: CACTION = '     '! action being done ('READ ','WRITE')
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
!!INTEGER                              :: NHALO = 0
! number of points added on each side (N,E,S,W) to the fields
! the HALO is added   when the field is read    (works only for grid coordinates)
!  note that at reading, this also modifies the numbers of points (IMAX, JMAX)
! the HALO is removed when the field is written (works for all fields)
!
END type IO_SURF_MNH_t
!
TYPE(IO_SURF_MNH_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: IO_SURF_MNH_MODEL
!
!!!!!!!!!!!!!!!!!!!! LOCAL VARIABLE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
CHARACTER(LEN=28)     ,POINTER :: CFILE =>NULL()      ! Name of the input FM-file
TYPE(TFILEDATA)       ,POINTER :: TPINFILE => NULL()  ! Input FM-file
CHARACTER(LEN=28)     ,POINTER :: COUTFILE =>NULL()   ! Name of the output FM-file
CHARACTER(LEN=28)     ,POINTER :: COUT =>NULL()       ! Name of output_listing file
INTEGER               ,POINTER :: NLUOUT =>NULL()     ! output listing logical unit
CHARACTER(LEN=6)      ,POINTER :: CMASK =>NULL()
INTEGER, DIMENSION(:), POINTER :: NMASK=>NULL()     ! 1D mask to read only interesting surface
!                                           ! points on current processor
INTEGER, DIMENSION(:), POINTER :: NMASK_ALL=>NULL() ! 1D mask to read all surface points all processors
!
CHARACTER(LEN=5)      ,POINTER :: CACTION => NULL() ! action being done ('READ ','WRITE')
!
! number of points in each direction on current processor
INTEGER             , POINTER  :: NIU=>NULL(),NJU=>NULL()
! indices of physical points in each direction on current processor
INTEGER             , POINTER  :: NIB=>NULL(),NJB=>NULL(),NIE=>NULL(),NJE=>NULL()
! number of points in each direction on all processors
INTEGER             , POINTER  :: NIU_ALL=>NULL(),NJU_ALL=>NULL()
! indices of physical points in each direction on all processors
INTEGER             , POINTER  :: NIB_ALL=>NULL(),NJB_ALL=>NULL(),NIE_ALL=>NULL(),NJE_ALL=>NULL()
!
!$20140403 you hardly want to set the NHALO inside the structure since it
!$connects with NAMELIST PGDFILE makign things difficult
!$NHALO IS =1 whatever the model is !!
!!INTEGER             , POINTER  :: NHALO=>NULL()

CONTAINS

SUBROUTINE IO_SURF_MNH_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
! save curretnt state for allocated arrays
IO_SURF_MNH_MODEL(KFROM)%NMASK=>NMASK
IO_SURF_MNH_MODEL(KFROM)%NMASK_ALL=>NMASK_ALL

! current model is set for model KTO 
CFILE=>IO_SURF_MNH_MODEL(KTO)%CFILE
TPINFILE=>IO_SURF_MNH_MODEL(KTO)%TPINFILE
COUTFILE=>IO_SURF_MNH_MODEL(KTO)%COUTFILE
COUT=>IO_SURF_MNH_MODEL(KTO)%COUT
NLUOUT=>IO_SURF_MNH_MODEL(KTO)%NLUOUT
CMASK=>IO_SURF_MNH_MODEL(KTO)%CMASK
NMASK=>IO_SURF_MNH_MODEL(KTO)%NMASK
NMASK_ALL=>IO_SURF_MNH_MODEL(KTO)%NMASK_ALL
CACTION=>IO_SURF_MNH_MODEL(KTO)%CACTION
NIU=>IO_SURF_MNH_MODEL(KTO)%NIU
NJU=>IO_SURF_MNH_MODEL(KTO)%NJU
NIB=>IO_SURF_MNH_MODEL(KTO)%NIB
NJB=>IO_SURF_MNH_MODEL(KTO)%NJB
NIE=>IO_SURF_MNH_MODEL(KTO)%NIE
NJE=>IO_SURF_MNH_MODEL(KTO)%NJE
NIU_ALL=>IO_SURF_MNH_MODEL(KTO)%NIU_ALL
NJU_ALL=>IO_SURF_MNH_MODEL(KTO)%NJU_ALL
NIB_ALL=>IO_SURF_MNH_MODEL(KTO)%NIB_ALL
NJB_ALL=>IO_SURF_MNH_MODEL(KTO)%NJB_ALL
NIE_ALL=>IO_SURF_MNH_MODEL(KTO)%NIE_ALL
NJE_ALL=>IO_SURF_MNH_MODEL(KTO)%NJE_ALL
!!NHALO=>IO_SURF_MNH_MODEL(KTO)%NHALO
END SUBROUTINE IO_SURF_MNH_GOTO_MODEL

END MODULE MODD_IO_SURF_MNH
