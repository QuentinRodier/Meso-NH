!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ################
      MODULE MODD_PREP_SEAFLUX
!     ################
!
!!****  *MODD_PREP_SEAFLUX - declaration for field interpolations
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
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
!!	S.Malardel    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/03
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
SAVE
!--------------------------------------------------------------------------
!
 CHARACTER(LEN=28) :: CFILE_SEAFLX   ! input file name
 CHARACTER(LEN=6)  :: CTYPE          ! input file type
 CHARACTER(LEN=28) :: CFILEPGD_SEAFLX   ! input file name
 CHARACTER(LEN=6)  :: CTYPEPGD          ! input file type
!
REAL              :: XSST_UNIF   !  uniform prescribed SST
!
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP_SEAFLUX


