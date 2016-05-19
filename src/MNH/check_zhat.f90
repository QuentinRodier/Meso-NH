!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_real 2006/05/18 13:07:25
!-----------------------------------------------------------------
!#####################
MODULE MODI_CHECK_ZHAT
!#####################
INTERFACE
      SUBROUTINE CHECK_ZHAT(HFMFILE,HDAD_NAME)
!
CHARACTER(LEN=*),    INTENT(IN)    :: HFMFILE   ! name of the Mesonh input file
CHARACTER(LEN=*),    INTENT(INOUT) :: HDAD_NAME ! true name of the Mesonh input file
!
END SUBROUTINE CHECK_ZHAT
END INTERFACE
END MODULE MODI_CHECK_ZHAT
!     ########################################
      SUBROUTINE CHECK_ZHAT(HFMFILE,HDAD_NAME)
!     ########################################
!
!!****  *CHECK_ZHAT* - checks coherence between large scale and fine
!!                     vertical grids for nesting purposes
!! 
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!  1 Resolution ratios during previous spawning are read in FM file.
!!  2 The 2 orographies are averaged on the grid with coarser resolution.
!!  3 If the 2 orographies on coarse grids are identical, then DAD_NAME
!!    is kept; if not, it is initialized to ' ', and nesting wont be
!!    allowed between the output file and its father.
!!
!!    EXTERNAL
!!    --------
!!
!!    function FMLOOK  :to retrieve a logical unit number associated with a file
!!    function FMREAD
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB : verbosity level for output-listing
!!      Module MODD_LUNIT     : contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_GRID1 
!!         XZHAT
!!      Module MODD_DIM1 
!!         NKMAX
!!      Module MODD_PARAMETERS
!!         JPHEXT
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!	
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    24/09/96
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_FM
!
USE MODD_CONF           ! declaration modules
USE MODD_LUNIT
USE MODD_GRID_n
USE MODD_DIM_n
USE MODD_PARAMETERS
!
USE MODE_FMREAD
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
CHARACTER(LEN=*),    INTENT(IN)    :: HFMFILE   ! name of the Mesonh input file
CHARACTER(LEN=*),    INTENT(INOUT) :: HDAD_NAME ! true name of the Mesonh input file
!
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER             :: IKMAX                ! vertical dimension in input file
INTEGER             :: IMASDEV              ! Masdev version
REAL, DIMENSION(:), ALLOCATABLE :: ZZHAT    ! vertical grid in input file
LOGICAL             :: GSLEVE               ! flag for sleve coordinate
REAL                :: ZLEN1                ! Decay scale for smooth topography
REAL                :: ZLEN2                ! Decay scale for small-scale topography deviation
!
INTEGER             :: IRESP                ! return-code if problems occured
INTEGER             :: ILUOUT0              ! logical unit for file CLUOUT0
INTEGER             :: IGRID,ILENCH         !   File 
CHARACTER (LEN=16)  :: YRECFM               ! management
CHARACTER (LEN=100) :: YCOMMENT             ! variables 
LOGICAL             :: GTHINSHELL
!
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
!-------------------------------------------------------------------------------
!
!*            1. Reading grid and dimension
!                --------------------------
!
YRECFM='KMAX'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',IKMAX,IGRID,ILENCH,YCOMMENT,IRESP)
ALLOCATE(ZZHAT(IKMAX+2*JPVEXT))
YRECFM='ZHAT'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',ZZHAT,IGRID,ILENCH,YCOMMENT,IRESP)
YRECFM='THINSHELL'
CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',GTHINSHELL,IGRID,ILENCH,YCOMMENT,IRESP)
!
CALL FMREAD(HFMFILE,'MASDEV',CLUOUT0,'--',IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
IF (IMASDEV<=46) THEN
  GSLEVE = .FALSE.
ELSE
  YRECFM='SLEVE'
  CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',GSLEVE,IGRID,ILENCH,YCOMMENT,IRESP)
ENDIF
!
!*            2. Check dimensions
!                ----------------
!
IF ( IKMAX /= NKMAX ) THEN
  HDAD_NAME=' '
  WRITE (ILUOUT0,*) '********************************************************'
  WRITE (ILUOUT0,*) 'Vertical grid has a new number of levels; no nesting allowed'
  WRITE (ILUOUT0,*) '********************************************************'
  RETURN
END IF
!
!*            3. Check the vertical grid
!                -----------------------
!
IF ( ANY(ABS(XZHAT(:)-ZZHAT(:))>1.E-10) ) THEN
  HDAD_NAME=' '
  WRITE (ILUOUT0,*) '********************************************************'
  WRITE (ILUOUT0,*) 'Vertical grid has been changed; no nesting allowed'
  WRITE (ILUOUT0,*) '********************************************************'
END IF
!
!*            4. Check the thinshell approximation
!                ---------------------------------
!
IF ( (GTHINSHELL .OR. LTHINSHELL) .AND. (.NOT. GTHINSHELL .OR. .NOT. LTHINSHELL) ) THEN
  HDAD_NAME=' '
  WRITE (ILUOUT0,*) '********************************************************'
  WRITE (ILUOUT0,*) 'thinshell approximation changed; no nesting allowed'
  WRITE (ILUOUT0,*) '********************************************************'
END IF
!
!*            5. Check the type of vertical grid
!                -------------------------------
!
IF ( (GSLEVE .OR. LSLEVE) .AND. (.NOT. GSLEVE .OR. .NOT. LSLEVE) ) THEN
  HDAD_NAME=' '
  WRITE (ILUOUT0,*) '********************************************************'
  WRITE (ILUOUT0,*) 'type of grid (SLEVE or GAL-CHEN) changed; no nesting allowed'
  WRITE (ILUOUT0,*) '********************************************************'
END IF
!
!*            6. Check the SLEVE coordinate parameters
!                -------------------------------------
!
IF ( GSLEVE .AND. LSLEVE ) THEN
  YRECFM='LEN1'
  CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',ZLEN1,IGRID,ILENCH,YCOMMENT,IRESP)
  YRECFM='LEN2'
  CALL FMREAD(HFMFILE,YRECFM,CLUOUT0,'--',ZLEN2,IGRID,ILENCH,YCOMMENT,IRESP)
  IF (ZLEN1 /= XLEN1 .OR. ZLEN2 /= XLEN2) THEN
    HDAD_NAME=' '
    WRITE (ILUOUT0,*) '********************************************************'
    WRITE (ILUOUT0,*) 'Decay scales for SLEVE coordinate changed; no nesting allowed'
    WRITE (ILUOUT0,*) '********************************************************'
  END IF
END IF
!
DEALLOCATE(ZZHAT)
!-------------------------------------------------------------------------------
!
WRITE (ILUOUT0,*) 'Routine CHECK_ZHAT completed'
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CHECK_ZHAT
