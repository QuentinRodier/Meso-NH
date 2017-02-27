!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_MNHWRITE_ZS_DUMMY_n
!     ##########################
INTERFACE
      SUBROUTINE MNHWRITE_ZS_DUMMY_n(TPFILE)
!
USE MODD_IO_ll, ONLY: TFILEDATA
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
END SUBROUTINE MNHWRITE_ZS_DUMMY_n
!
END INTERFACE
END MODULE MODI_MNHWRITE_ZS_DUMMY_n
!
!     ###################################################
      SUBROUTINE MNHWRITE_ZS_DUMMY_n(TPFILE)
!     ###################################################
!
!!****  *MNHWRITE_ZS_WH_DUMMY_n* - writes zs and dummy surface fields.
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to write the LFIFM part of 
!!       physiographic data file with the FM routines.
!!
!!
!!**  METHOD
!!    ------
!!          
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   01/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_GR_FIELD_n, ONLY : XSSO_STDEV, XSSO_ANISOTROPY, XSSO_DIRECTION, XSSO_SLOPE, &
                            XAVG_ZS, XSIL_ZS, XMIN_ZS, XMAX_ZS
!
USE MODD_LUNIT_n,    ONLY : CLUOUT
USE MODD_PARAM_n,    ONLY : CSURF
USE MODD_IO_ll,      ONLY : TFILEDATA
!
USE MODI_WRITE_DUMMY_GR_FIELD_n
!
USE MODE_FMWRIT
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    ! at the open of the file in LFI  routines 
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string 
CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
!
!-------------------------------------------------------------------------------
!
!*       1.     Orography :
!               ---------
!
!***********************************!
! Already written (in write_lfifmn) !
!***********************************!
!
IF (CSURF /='EXTE') RETURN
!-------------------------------------------------------------------------------
!
!*       2.     Orographic characteristics :
!               --------------------------
!
CALL IO_WRITE_FIELD(TPFILE,'SSO_ANIS', CLUOUT,IRESP,XSSO_ANISOTROPY)
CALL IO_WRITE_FIELD(TPFILE,'SSO_SLOPE',CLUOUT,IRESP,XSSO_SLOPE)
CALL IO_WRITE_FIELD(TPFILE,'SSO_DIR',  CLUOUT,IRESP,XSSO_DIRECTION)
CALL IO_WRITE_FIELD(TPFILE,'AVG_ZS',   CLUOUT,IRESP,XAVG_ZS)
CALL IO_WRITE_FIELD(TPFILE,'SIL_ZS',   CLUOUT,IRESP,XSIL_ZS)
CALL IO_WRITE_FIELD(TPFILE,'MAX_ZS',   CLUOUT,IRESP,XMAX_ZS)
CALL IO_WRITE_FIELD(TPFILE,'MIN_ZS',   CLUOUT,IRESP,XMIN_ZS)
CALL IO_WRITE_FIELD(TPFILE,'SSO_STDEV',CLUOUT,IRESP,XSSO_STDEV)
!
!-------------------------------------------------------------------------------
!
!*      3.     Dummy fields
!              ------------
!
CALL WRITE_DUMMY_GR_FIELD_n(TPFILE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHWRITE_ZS_DUMMY_n
