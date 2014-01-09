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
      MODULE MODI_MNHREAD_ZS_DUMMY_n
!     ##########################
INTERFACE
      SUBROUTINE MNHREAD_ZS_DUMMY_n(HINIFILE)
!
CHARACTER (LEN=*),  INTENT(IN)   :: HINIFILE ! name of
                                             ! the initial file
!
END SUBROUTINE MNHREAD_ZS_DUMMY_n
!
END INTERFACE
!
END MODULE MODI_MNHREAD_ZS_DUMMY_n
!
!     ##########################################################################
      SUBROUTINE MNHREAD_ZS_DUMMY_n(HINIFILE)
!     ##########################################################################
!
!!****  *MNHREAD_ZS_DUMMY_n* - reads zs and dummy surface fields
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to read the LFIFM part of 
!!       physiographic data file with the FM routines.
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
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
!!      Original    01/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_LUNIT_n,    ONLY : CLUOUT
USE MODD_GRID_n,     ONLY : XZS
USE MODD_GR_FIELD_n, ONLY : XSSO_STDEV, XSSO_ANISOTROPY, XSSO_DIRECTION, XSSO_SLOPE, &
                            XAVG_ZS, XSIL_ZS, XMIN_ZS, XMAX_ZS
USE MODD_PARAM_n,    ONLY : CSURF
!
USE MODI_READ_DUMMY_GR_FIELD_n
!
USE MODE_ll
USE MODE_FMREAD
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
CHARACTER (LEN=*),  INTENT(IN)   :: HINIFILE ! name of
                                             ! the initial file
!

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    ! at the open of the file in LFI  routines 
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string 
!
CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
INTEGER           :: IIU            ! X array size
INTEGER           :: IJU            ! Y array size
!
!-------------------------------------------------------------------------------
!
!*       1.     READS IN THE LFI FILE
!	        ---------------------
!
!*       1.0    General information :
!               -------------------
!
!*       1.1    Dimensions :
!               ----------
!
!* x and y dimensions in the file
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
!
!
!*       1.3    Orography :
!               ---------
YRECFM='ZS'
IF (.NOT.(ASSOCIATED(XZS))) THEN
  ALLOCATE(XZS(IIU,IJU))
  CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XZS,IGRID,ILENCH,YCOMMENT,IRESP)
END IF
!
IF (CSURF /='EXTE') RETURN
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!*       2.1    Orographic characteristics :
!               --------------------------
!
YRECFM='SSO_STDEV'
IF (.NOT.(ASSOCIATED(XSSO_STDEV))) ALLOCATE(XSSO_STDEV(IIU,IJU))
CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XSSO_STDEV(:,:),IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='AVG_ZS'
IF (.NOT.(ASSOCIATED(XAVG_ZS))) ALLOCATE(XAVG_ZS(IIU,IJU))
CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XAVG_ZS(:,:),IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SIL_ZS'
IF (.NOT.(ASSOCIATED(XSIL_ZS))) ALLOCATE(XSIL_ZS(IIU,IJU))
CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XSIL_ZS(:,:),IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='MAX_ZS'
IF (.NOT.(ASSOCIATED(XMAX_ZS))) ALLOCATE(XMAX_ZS(IIU,IJU))
CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XMAX_ZS(:,:),IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='MIN_ZS'
IF (.NOT.(ASSOCIATED(XMIN_ZS))) ALLOCATE(XMIN_ZS(IIU,IJU))
CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XMIN_ZS(:,:),IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SSO_ANIS'
IF (.NOT.(ASSOCIATED(XSSO_ANISOTROPY))) ALLOCATE(XSSO_ANISOTROPY(IIU,IJU))
CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XSSO_ANISOTROPY(:,:),IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SSO_SLOPE'
IF (.NOT.(ASSOCIATED(XSSO_SLOPE))) ALLOCATE(XSSO_SLOPE(IIU,IJU))
CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XSSO_SLOPE(:,:),IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SSO_DIR'
IF (.NOT.(ASSOCIATED(XSSO_DIRECTION))) ALLOCATE(XSSO_DIRECTION(IIU,IJU))
CALL FMREAD(HINIFILE,YRECFM,CLUOUT,'XY',XSSO_DIRECTION(:,:),IGRID,ILENCH,YCOMMENT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*      3.     Dummy fields
!              ------------
!
CALL READ_DUMMY_GR_FIELD_n(HINIFILE,CLUOUT,1,IIU,1,IJU,.TRUE.)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHREAD_ZS_DUMMY_n
