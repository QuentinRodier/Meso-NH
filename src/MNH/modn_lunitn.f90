!MNH_LIC Copyright 1994-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###################
      MODULE MODN_LUNIT_n
!     ###################
!
!!****  *MODN_LUNIT$n* - declaration of namelist NAM_LUNITn
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify  the namelist NAM_LUNITn 
!     which contains names for Meso-NH files (FM-file)   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_LUNIT$n : contains declaration of logical unit names
!!            CINIFILE   : name of the FM-file used to initialize the model $n
!!            CCPLFILE   : Names of the FM-files used to couple the model 1
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_LUNITn)
!!          
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       1/06/94                      
!!      Modification  10/03/95 (I.Mallet)   add the coupling files names 
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAX
USE MODD_LUNIT_n,    ONLY: CINIFILE_n    => CINIFILE,    &
                           CINIFILEPGD_n => CINIFILEPGD, &
                           CCPLFILE_n    => CCPLFILE,    &
                           NPCPLFILEMAX
!
IMPLICIT NONE
!
SAVE
!
CHARACTER(LEN=NFILENAMELGTMAX)                            :: CINIFILE
CHARACTER(LEN=NFILENAMELGTMAX)                            :: CINIFILEPGD
CHARACTER(LEN=NFILENAMELGTMAX), DIMENSION(:), ALLOCATABLE :: CCPLFILE
!
NAMELIST/NAM_LUNITn/CINIFILE,CINIFILEPGD,CCPLFILE
!
CONTAINS
!
SUBROUTINE INIT_NAM_LUNITn( OWRITE )
  LOGICAL, OPTIONAL, INTENT(IN) :: OWRITE

  CHARACTER(LEN=NFILENAMELGTMAX), DIMENSION(:), ALLOCATABLE :: YCPLFILE_TMP
  LOGICAL :: GWRITE

  IF ( PRESENT(OWRITE) ) THEN
    GWRITE = OWRITE
  ELSE
    GWRITE = .FALSE.
  END IF

  CINIFILE = CINIFILE_n
  CINIFILEPGD = CINIFILEPGD_n

  ! Reallocate CCPLFILE_n to the NPCPLFILEMAX size (necessary to read again/decompact)
  ! if write: keep the compact version (in that case, we assume that CCPLFILE_n is already associated)
  IF ( .NOT.GWRITE) THEN
    IF ( ASSOCIATED(CCPLFILE_n) ) THEN
      ALLOCATE( YCPLFILE_TMP, SOURCE = CCPLFILE_n ) ! sourced allocation, YCPLFILE_TMP is allocated to a clone of CCPLFILE_N
      DEALLOCATE( CCPLFILE_n )
    ELSE
      ALLOCATE( CHARACTER(LEN=NFILENAMELGTMAX) :: YCPLFILE_TMP(NPCPLFILEMAX) )
      YCPLFILE_TMP(:) = ''
    END IF

    ALLOCATE( CHARACTER(LEN=NFILENAMELGTMAX) :: CCPLFILE_n(NPCPLFILEMAX) )
    CCPLFILE_n(1:SIZE(YCPLFILE_TMP)) = YCPLFILE_TMP(:)
    IF ( SIZE(YCPLFILE_TMP) < SIZE(CCPLFILE_n) ) CCPLFILE_n(SIZE(YCPLFILE_TMP)+1:) = ''
  END IF

  IF ( ALLOCATED( CCPLFILE ) ) DEALLOCATE( CCPLFILE )
  ALLOCATE( CCPLFILE, SOURCE = CCPLFILE_N ) ! sourced allocation, CCPLFILE is allocated to a clone of CCPLFILE_N
END SUBROUTINE INIT_NAM_LUNITn

SUBROUTINE UPDATE_NAM_LUNITn(KMI)
  USE MODD_DYN, ONLY: NCPL_NBR

  INTEGER, INTENT(IN) :: KMI ! Model number

  CHARACTER(LEN=NFILENAMELGTMAX), DIMENSION(:), ALLOCATABLE :: YCPLFILE_TMP
  INTEGER :: ICPL_NBR
  INTEGER :: JCI

  CINIFILE_n = CINIFILE
  CINIFILEPGD_n = CINIFILEPGD

  ! Compress the coupling file list + find the number of coupling files
  ICPL_NBR = 0
  DO JCI = 1, SIZE(CCPLFILE)
    IF ( LEN_TRIM(CCPLFILE(JCI)) /= 0 ) THEN
      ICPL_NBR = ICPL_NBR + 1
      IF ( JCI /= ICPL_NBR ) THEN
        CCPLFILE(ICPL_NBR) = CCPLFILE(JCI)
        CCPLFILE(JCI) = ''
      END IF
    END IF
  END DO

  ALLOCATE( CHARACTER(LEN=NFILENAMELGTMAX) :: YCPLFILE_TMP(ICPL_NBR) )
  YCPLFILE_TMP(:) = CCPLFILE(1:ICPL_NBR)
  CALL MOVE_ALLOC( FROM = YCPLFILE_TMP, TO = CCPLFILE )

  ! NCPL_NBR is only for the outermost model
  IF ( KMI == 1 ) NCPL_NBR = ICPL_NBR

  ! Reallocate CCPLFILE_n because CCPLFILE could have been reallocated (and resized/compacted)
  DEALLOCATE( CCPLFILE_n )
  ALLOCATE( CCPLFILE_n, SOURCE = CCPLFILE ) ! sourced allocation, CCPLFILE_n is allocated to a clone of CCPLFILE

END SUBROUTINE UPDATE_NAM_LUNITn

END MODULE MODN_LUNIT_n
