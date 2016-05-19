!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 spawn 2006/05/23 15:34:13
!-----------------------------------------------------------------
!######################
MODULE MODI_SPAWN_GRID2
!######################
!
INTERFACE
!
     SUBROUTINE SPAWN_GRID2 (KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,         &
                             PLONOR,PLATOR,PXHAT,PYHAT,PZHAT,                 &
                             OSLEVE,PLEN1,PLEN2,                              &
                             PZS,PZSMT,PZS_LS,PZSMT_LS,                       &
                             TPDTMOD,TPDTCUR                                  )
!
USE MODD_TIME
!
!
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the model 2 domain, relative to model 1
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction Resolution ratio
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between model 2 and model 1
!
REAL,                 INTENT(INOUT) :: PLATOR            ! Latitude of the origine point
REAL,                 INTENT(INOUT) :: PLONOR            ! Longitude of the origine point
REAL, DIMENSION(:),   INTENT(INOUT) :: PXHAT,PYHAT,PZHAT ! positions x,y,z in the
                                     ! conformal plane or on the cartesian plane
LOGICAL,              INTENT(OUT)   :: OSLEVE            ! flag for SLEVE coordinate
REAL,                 INTENT(OUT)   :: PLEN1             ! Decay scale for smooth topography
REAL,                 INTENT(OUT)   :: PLEN2             ! Decay scale for small-scale topography deviation
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZS               ! orography
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZSMT             ! smooth orography
REAL, DIMENSION(:,:), INTENT(OUT)   :: PZS_LS            ! interpolated orography
REAL, DIMENSION(:,:), INTENT(OUT)   :: PZSMT_LS          ! interpolated smooth orography
!
!
TYPE (DATE_TIME),     INTENT(INOUT) :: TPDTMOD  ! Date and Time of MODel beginning
TYPE (DATE_TIME),     INTENT(INOUT) :: TPDTCUR  ! CURent date and time
!
END SUBROUTINE SPAWN_GRID2
!
END INTERFACE
!
END MODULE MODI_SPAWN_GRID2
!
!
!     #########################################################################
     SUBROUTINE SPAWN_GRID2 (KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,         &
                             PLONOR,PLATOR,PXHAT,PYHAT,PZHAT,                 &
                             OSLEVE,PLEN1,PLEN2,                              &
                             PZS,PZSMT,PZS_LS,PZSMT_LS,                       &
                             TPDTMOD,TPDTCUR                                  )
!     #########################################################################
!
!!****  *SPAWN_GRID2 * - subroutine to define spatial and temporal grid.
!!
!!    PURPOSE
!!    -------
!!
!!      This routine defines the information necessary to generate the model 2
!!    grid, consistently with the spawning model 1.
!!      The longitude and latitude of the model 2 origine are computed from
!!    the model 1. Then the grid in the conformal projection and terrain
!!    following coordinates (XHAT,YHAT and ZHAT) and orography, are interpolated
!!    from the model 1 grid and orography knowledge.
!!
!!      Date and time are set as for model 1.
!!
!!**  METHOD
!!    ------
!!
!!      The model 2 variables are transmitted by argument (P or K prefixes),
!!    while the ones of model 1 are declared through calls to MODD_...
!!    (X or N prefixes)
!!
!!      For the case where the resolution ratio between models is 1,
!!    the horizontal interpolation becomes a simple equality.
!!      For the general case where resolution ratio is not egal to one,
!!    grid and orography are interpolated as follows:
!!         - linear interpolation for XHAT and YHAT
!!         - identity for ZHAT (no vertical spawning)
!!            2 types of interpolations can be used:
!!                 1. Clark and Farley (JAS 1984) on 9 points
!!                 2. Bikhardt on 16 points
!!
!!    EXTERNAL
!!    --------
!!
!!      FMLOOK        : to recover a logical unit number
!!      Module MODE_TIME : contains SM_PRINT_TIME routine
!!      Routine BIKHARDT2     : to perform horizontal interpolations
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_PARAMETERS : contains parameters
!!      Module MODD_CONF       : contains models configuration
!!      Module MODD_GRID1      : contains grid variables
!!      Module MODD_TIME1      : contains date and time of model 1
!!                              and uses MODD_TIME
!!      Module MODD_GR_FIELD1  : contains surface variables
!!
!!      Module MODD_LUNIT2     : contains unit numbers of model 2 files
!!
!!    REFERENCE
!!    ---------
!!
!!       Book1 of the documentation
!!       PROGRAM SPAWN_GRID2 (Book2 of the documentation)
!!
!!
!!    AUTHOR
!!    ------
!!
!!       J.P. Lafore     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     12/01/95
!!      Modification 05/07/95  (Lafore) Different resolution ratio case introduction
!!      Modification 31/01/96  (Lafore) Update for MASDEV2_2 version and corrections
!!                                      for the different resolution ratio case
!!      Modification 19/02/96  (Lafore) introduction of the Bikhardt interpolation
!!      Modification 19/03/96  (Lafore) interpolation of surface variables
!!      Modification 10/06/96 (V.Masson) remove the loops in case of no resolution change
!!      Modification 10/06/96 (V.Masson) interpolation computations performed in
!!                                       independant routines
!!      Modification 19/06/96 (V.Masson) case of integer input land sea mask
!!      Modification 02/10/96 (V.Masson) iterative method for zs computation
!!      Modification 21/11/96 (Lafore)   move from BIKHARDT2 to BIKHARDT routine
!!      Modification 16/07/97 (V.Masson) bug in test of positivity for zs
!!      Modification 17/07/97 (V.Masson) purely interpolated zs (PZS_LS)
!!      Modification 10/10/97 (V.Masson) bug on boundaries for zs procedure
!!      Modification 20/04/99 (J. Stein) bug on the last point if the whole
!!                             domain is used (2D case along y for instance
!!      Modification 15/03/99 (V.Masson) cover types
!!      Modification 04/07/01 (J.Stein)  convergence test set to 1 millimeter for GRID_MODEL(1)%XZS
!!      Modification 05/09/05 (J. Escobar) change INTENT(OUT) --> INTENT(INOUT)
!!                             to avoid problem when Input parameter and GRID1 parameter
!!                             are exactly the same !!!
!!      Modification 20/05/06 Remove Clark and Farley interpolation
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_PARAMETERS       ! Declarative modules
USE MODD_CONF
!
USE MODD_GRID, ONLY: XLONORI,XLATORI 
USE MODD_GRID_n,    ONLY: GRID_MODEL
USE MODD_TIME_n,    ONLY: TIME_MODEL
USE MODD_LBC_n,     ONLY: LBC_MODEL
!
USE MODD_LUNIT_n
USE MODD_BIKHARDT_n
!
USE MODE_FM
USE MODE_IO_ll
USE MODE_TIME
USE MODE_GRIDPROJ
!
USE MODI_BIKHARDT
USE MODI_SPAWN_ZS
!
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the model 2 domain, relative to model 1
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction Resolution ratio
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between model 2 and model 1
!
REAL,                 INTENT(INOUT) :: PLATOR            ! Latitude of the origine point
REAL,                 INTENT(INOUT) :: PLONOR            ! Longitude of the origine point
REAL, DIMENSION(:),   INTENT(INOUT) :: PXHAT,PYHAT,PZHAT ! positions x,y,z in the
                                     ! conformal plane or on the cartesian plane
LOGICAL,              INTENT(OUT)   :: OSLEVE            ! flag for SLEVE coordinate
REAL,                 INTENT(OUT)   :: PLEN1             ! Decay scale for smooth topography
REAL,                 INTENT(OUT)   :: PLEN2             ! Decay scale for small-scale topography deviation
REAL, DIMENSION(:,:), INTENT(OUT) :: PZS               ! orography
REAL, DIMENSION(:,:), INTENT(OUT) :: PZSMT             ! smooth orography
REAL, DIMENSION(:,:), INTENT(OUT) :: PZS_LS            ! interpolated orography
REAL, DIMENSION(:,:), INTENT(OUT) :: PZSMT_LS          ! interpolated smooth orography
!
!
TYPE (DATE_TIME),     INTENT(INOUT) :: TPDTMOD  ! Date and Time of MODel beginning
TYPE (DATE_TIME),     INTENT(INOUT) :: TPDTCUR  ! CURent date and time
!
!*       0.2    Declarations of local variables for print on FM file
!
INTEGER :: ILUOUT   ! Logical unit number for the output listing
INTEGER :: IRESP    ! Return codes in FM routines
!
REAL :: ZPOND1,ZPOND2               ! interpolation coefficients
!
INTEGER             :: IIU       ! Upper dimension in x direction
INTEGER             :: IJU       ! Upper dimension in y direction
INTEGER             :: IIB       ! indice I Beginning in x direction
INTEGER             :: IJB       ! indice J Beginning in y direction
INTEGER             :: IIS,IJS   ! indices I and J in x and y dir. for scalars
INTEGER             :: JI,JEPSX  ! Loop index in x direction
INTEGER             :: JJ,JEPSY  ! Loop index in y direction
REAL, DIMENSION(:), ALLOCATABLE :: ZXHAT_EXTENDED, ZYHAT_EXTENDED
INTEGER             :: IXSIZE1,IYSIZE1    ! sizes of the XHAT and YHAT arrays
!
CHARACTER (LEN=40)  :: YTITLE    ! Title for time print
INTEGER  :: IMI
!-------------------------------------------------------------------------------
!
!
!*       1.    PROLOGUE:
!              ---------
! 
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(2)
!
!*       1.1   Interpolation method
!
!
!*       1.1   computes dimensions of arrays and other indices
!
IIU = SIZE(PXHAT)
IJU = SIZE(PYHAT)
IIB = 1+JPHEXT
IJB = 1+JPHEXT
!
!*       1.2  recovers logical unit number of output listing
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
!*       1.3  checks that model 2 domain is included in the one of model 1
!
IF ( (KXEND) > SIZE(GRID_MODEL(1)%XXHAT) )  THEN   
  WRITE(ILUOUT,FMT=*) 'SPAWN_MODEL2:  MODEL 2 DOMAIN OUTSIDE THE MODEL1 DOMAIN  ', &
                  ' KXOR = ', KXOR,' KXEND = ', KXEND,                         &
                  ' IIU of model1 = ',SIZE(GRID_MODEL(1)%XXHAT)
 !callabortstop
  CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF 
IF ( (KYEND) > SIZE(GRID_MODEL(1)%XYHAT) )  THEN  
  WRITE(ILUOUT,FMT=*) 'SPAWN_MODEL2:  MODEL 2 DOMAIN OUTSIDE THE MODEL1 DOMAIN  ', &
                  ' KYOR = ', KYOR,' KYEND = ', KYEND,                         &
                  ' IJU of model1 = ',SIZE(GRID_MODEL(1)%XYHAT)
 !callabortstop
  CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.    INITIALIZATION OF THE GRID OF MODEL 2:
!              --------------------------------------
!
PZHAT(:) = GRID_MODEL(1)%XZHAT(:) 
OSLEVE   = GRID_MODEL(1)%LSLEVE
PLEN1    = GRID_MODEL(1)%XLEN1
PLEN2    = GRID_MODEL(1)%XLEN2
!
IF (KDXRATIO == 1 .AND. KDYRATIO == 1 ) THEN
!
!*       2.1   special case of spawning - no change of resolution :
!
  PXHAT(:) = GRID_MODEL(1)%XXHAT(KXOR:KXEND)
  PYHAT(:) = GRID_MODEL(1)%XYHAT(KYOR:KYEND)
  PZS  (:,:) = GRID_MODEL(1)%XZS  (KXOR:KXEND,KYOR:KYEND)
  PZS_LS(:,:)= GRID_MODEL(1)%XZS  (KXOR:KXEND,KYOR:KYEND)
  PZSMT   (:,:) = GRID_MODEL(1)%XZSMT(KXOR:KXEND,KYOR:KYEND)
  PZSMT_LS(:,:) = GRID_MODEL(1)%XZSMT(KXOR:KXEND,KYOR:KYEND)
!
ELSE
!
!*       2.2  general case - change of resolution :
!
!*       2.2.1 linear interpolation for XHAT and YHAT
!
  IXSIZE1=SIZE(GRID_MODEL(1)%XXHAT)
  ALLOCATE(ZXHAT_EXTENDED(IXSIZE1+1))
  ZXHAT_EXTENDED(1:IXSIZE1)=GRID_MODEL(1)%XXHAT(:)
  ZXHAT_EXTENDED(IXSIZE1+1)=2.*GRID_MODEL(1)%XXHAT(IXSIZE1)-GRID_MODEL(1)%XXHAT(IXSIZE1-1)
  DO JEPSX = 1,KDXRATIO
    ZPOND2 = FLOAT(JEPSX-1)/FLOAT(KDXRATIO)
    ZPOND1 = 1.-ZPOND2
    DO JI = KXOR,KXEND
      IIS = IIB+JEPSX-1+(JI-KXOR-1)*KDXRATIO
!
      IF (1 <= IIS .AND. IIS <= IIU)                   &
      PXHAT(IIS) = ZPOND1*ZXHAT_EXTENDED(JI) +ZPOND2*ZXHAT_EXTENDED(JI+1)
    END DO
  END DO
  DEALLOCATE(ZXHAT_EXTENDED)
!
  IYSIZE1=SIZE(GRID_MODEL(1)%XYHAT)
  ALLOCATE(ZYHAT_EXTENDED(IYSIZE1+1))
  ZYHAT_EXTENDED(1:IYSIZE1)=GRID_MODEL(1)%XYHAT(:)
  ZYHAT_EXTENDED(IYSIZE1+1)=2.*GRID_MODEL(1)%XYHAT(IYSIZE1)-GRID_MODEL(1)%XYHAT(IYSIZE1-1)
  DO JEPSY = 1,KDYRATIO
    ZPOND2 = FLOAT(JEPSY-1)/FLOAT(KDYRATIO)
    ZPOND1 = 1.-ZPOND2
    DO JJ = KYOR,KYEND
      IJS = IJB+JEPSY-1+(JJ-KYOR-1)*KDYRATIO
!
      IF (1 <= IJS .AND. IJS <= IJU)                   &
      PYHAT(IJS) = ZPOND1*ZYHAT_EXTENDED(JJ) +ZPOND2*ZYHAT_EXTENDED(JJ+1)
    END DO
  END DO
  DEALLOCATE(ZYHAT_EXTENDED)
!
!
!*       2.2.2  interpolation of ZS performed later
!
END IF
!
PLONOR = XLONORI
PLATOR = XLATORI
!
!-------------------------------------------------------------------------------
!
!*       3.    INITIALIZATION OF ZS and ZSMT:
!              ------------------------------
!
CALL SPAWN_ZS(KXOR,KXEND,KYOR,KYEND,KDXRATIO,KDYRATIO,LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,CLUOUT,  &
              GRID_MODEL(1)%XZS,  PZS,  'ZS    ',PZS_LS)
CALL SPAWN_ZS(KXOR,KXEND,KYOR,KYEND,KDXRATIO,KDYRATIO,LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,CLUOUT,  &
              GRID_MODEL(1)%XZSMT,PZSMT,'ZSMT  ',PZSMT_LS)
!
!-------------------------------------------------------------------------------
!
!*       4.    INITIALIZATION OF MODEL 2 DATE AND TIME:
!              ----------------------------------------
!
TPDTMOD = TIME_MODEL(1)%TDTCUR
TPDTCUR = TIME_MODEL(1)%TDTCUR
!
YTITLE='OUTER MODEL : CURRENT DATE AND TIME '
CALL SM_PRINT_TIME(TIME_MODEL(1)%TDTCUR, CLUOUT, YTITLE)
YTITLE='SPAWNED MODEL : DATE AND TIME BEGINNING'
CALL SM_PRINT_TIME(TPDTMOD, CLUOUT, YTITLE)
YTITLE='SPAWNED MODEL : CURRENT DATE AND TIME '
CALL SM_PRINT_TIME(TPDTCUR, CLUOUT, YTITLE)
!
!-------------------------------------------------------------------------------
CALL GOTO_MODEL(IMI)
!
END SUBROUTINE SPAWN_GRID2
