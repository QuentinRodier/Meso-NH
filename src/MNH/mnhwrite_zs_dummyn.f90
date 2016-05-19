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
      SUBROUTINE MNHWRITE_ZS_DUMMY_n(HPROGRAM)
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
END SUBROUTINE MNHWRITE_ZS_DUMMY_n
!
END INTERFACE
END MODULE MODI_MNHWRITE_ZS_DUMMY_n
!
!     ###################################################
      SUBROUTINE MNHWRITE_ZS_DUMMY_n(HPROGRAM)
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
USE MODD_LUNIT,      ONLY : COUTFMFILE
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
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
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
YRECFM='SSO_ANIS'
YCOMMENT='X_Y_SSO_ANISOTROPY (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT,'XY',XSSO_ANISOTROPY,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SSO_SLOPE'
YCOMMENT='X_Y_SSO_SLOPE '
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT,'XY',XSSO_SLOPE,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SSO_DIR'
YCOMMENT='X_Y_SSO_DIR (degree)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT,'XY',XSSO_DIRECTION,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='AVG_ZS'
YCOMMENT='X_Y_AVG_ZS (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT,'XY',XAVG_ZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SIL_ZS'
YCOMMENT='X_Y_SIL_ZS (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT,'XY',XSIL_ZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='MAX_ZS'
YCOMMENT='X_Y_MAX_ZS (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT,'XY',XMAX_ZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='MIN_ZS'
YCOMMENT='X_Y_MIN_ZS (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT,'XY',XMIN_ZS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='SSO_STDEV'
YCOMMENT='X_Y_SSO_STDEV (m)'
IGRID=4
ILENCH=LEN(YCOMMENT)
CALL FMWRIT(COUTFMFILE,YRECFM,CLUOUT,'XY',XSSO_STDEV,IGRID,ILENCH,YCOMMENT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*      3.     Dummy fields
!              ------------
!
CALL WRITE_DUMMY_GR_FIELD_n(COUTFMFILE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHWRITE_ZS_DUMMY_n
