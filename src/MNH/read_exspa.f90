!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 spawn 2006/06/27 14:37:30
!-----------------------------------------------------------------
!######################
MODULE MODI_READ_EXSPA
!######################
!
INTERFACE
!
      SUBROUTINE READ_EXSPA(HINIFILE,HINIFILEPGD,&
                            KXOR,KYOR,KXSIZE,KYSIZE,KDXRATIO,KDYRATIO,&
                            OBAL_ONLY,HDOMAIN,HSPAFILE,HSPANBR,       &
                            HDADINIFILE,HDADSPAFILE,HSONFILE)
!
!*       0.1    Dummy arguments
!
CHARACTER (LEN=28), INTENT(OUT) :: HINIFILE ! Name of the model 1 FM file
CHARACTER (LEN=28), INTENT(OUT) :: HINIFILEPGD
INTEGER,            INTENT(OUT) :: KXOR     !  horizontal position (i,j) of the ORigin
INTEGER,            INTENT(OUT) :: KYOR     ! of the model 2 domain, relative to model 1
INTEGER,            INTENT(OUT) :: KDXRATIO !  x and y-direction resolution RATIO
INTEGER,            INTENT(OUT) :: KDYRATIO !      between model 2 and model 1
INTEGER,            INTENT(OUT) :: KXSIZE   ! number of model 1 grid points in x and y-directions
INTEGER,            INTENT(OUT) :: KYSIZE   ! in the model 2 physical domain
!
CHARACTER (LEN=28), INTENT(OUT) :: HDOMAIN  ! input fm-file for grid definition
CHARACTER (LEN=28), INTENT(OUT) :: HSPAFILE ! possible name of the output FM-file
CHARACTER (LEN= 2), INTENT(OUT) :: HSPANBR  ! NumBeR associated to the SPAwned file
CHARACTER (LEN=28), INTENT(OUT) :: HSONFILE ! Name of the SON 1 file
CHARACTER (LEN=28), INTENT(OUT) :: HDADINIFILE ! name of the initial dad file
                                                  ! of the model 1 only for 
                                                  ! spawning 1 with GBAL_ONLY
CHARACTER (LEN=28), INTENT(OUT) :: HDADSPAFILE  ! name of the dad file of the                                                   ! model 2 only for spawning 1 
                                                  ! with GBAL_ONLY
LOGICAL,            INTENT(OUT) :: OBAL_ONLY      ! compute anelastique balance
                                                  ! without change in the file
                                                  ! definition
!
END SUBROUTINE READ_EXSPA
!
END INTERFACE
!
END MODULE MODI_READ_EXSPA
!
!
!     #########################################################################
      SUBROUTINE READ_EXSPA(HINIFILE,HINIFILEPGD, &
                            KXOR,KYOR,KXSIZE,KYSIZE,KDXRATIO,KDYRATIO,&
                            OBAL_ONLY,HDOMAIN,HSPAFILE,HSPANBR,       &
                            HDADINIFILE,HDADSPAFILE,HSONFILE)
!     #########################################################################
!
!!****  *READ_EXSPA * - subroutine to read spawning namelist
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!
!!
!!    EXTERNAL
!!    --------
!!
!!
!! 
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     19/03/96
!!      Modification 14/12/00  (P.Jabouille) do not close the EXSPA file
!!      Modification 15/10/01  (I.Mallet) allow namelists in different orders
!!      Modification 08/04/04  (G.Jaubert) spawning 1 for anelastic balance only
!!      Modification 07/07/05  (D.Barbary) spawn with 2 input files (father+son1)
!!      Modification 30/03/12  (S.Bielli) add NAM_NCOUT for netcdf output
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CONF
USE MODD_LUNIT_n, CINIFILE_n=>CINIFILE, CINIFILEPGD_n=>CINIFILEPGD
USE MODD_PARAMETERS
!
USE MODE_IO_ll
USE MODE_POS
USE MODE_MODELN_HANDLER
!
USE MODN_CONFIO
#ifdef MNH_NCWRIT
USE MODN_NCOUT
#endif
!
IMPLICIT NONE
!
!*       0.1    Dummy arguments
!
CHARACTER (LEN=28), INTENT(OUT) :: HINIFILE ! Name of the model 1 FM file
CHARACTER (LEN=28), INTENT(OUT) :: HINIFILEPGD
INTEGER,            INTENT(OUT) :: KXOR     !  horizontal position (i,j) of the ORigin
INTEGER,            INTENT(OUT) :: KYOR     ! of the model 2 domain, relative to model 1
INTEGER,            INTENT(OUT) :: KDXRATIO !  x and y-direction resolution RATIO
INTEGER,            INTENT(OUT) :: KDYRATIO !      between model 2 and model 1
INTEGER,            INTENT(OUT) :: KXSIZE   ! number of model 1 grid points in x and y-directions
INTEGER,            INTENT(OUT) :: KYSIZE   ! in the model 2 physical domain
!
CHARACTER (LEN=28), INTENT(OUT) :: HDOMAIN  ! input fm-file for grid definition
CHARACTER (LEN=28), INTENT(OUT) :: HSPAFILE ! possible name of the output FM-file
CHARACTER (LEN= 2), INTENT(OUT) :: HSPANBR  ! NumBeR associated to the SPAwned file
CHARACTER (LEN=28), INTENT(OUT) :: HSONFILE ! Name of the SON 1 file
CHARACTER (LEN=28), INTENT(OUT) :: HDADINIFILE ! name of the initial dad file
                                                  ! of the model 1 only for 
                                                  ! spawning 1 with GBAL_ONLY
CHARACTER (LEN=28), INTENT(OUT) :: HDADSPAFILE  ! name of the dad file of the                                                   ! model 2 only for spawning 1 
                                                  ! with GBAL_ONLY
LOGICAL,            INTENT(OUT) :: OBAL_ONLY      ! compute anelastique balance
                                                  ! without change in the file
                                                  ! definition
!
!
!*       0.2    Local variables
!
INTEGER :: IRESP    ! Return codes in FM routines
INTEGER :: ILUOUT   ! Logical unit number for the output listing 
INTEGER :: ILUSPA   ! Logical unit number for the EXSPA file
CHARACTER (LEN=32) :: YEXSPA          ! Name of the EXSPA file
LOGICAL :: GFOUND   ! Return code when searching namelist
!
!* prefixes in the namelists are such because of code history
!
INTEGER :: IXOR                !  horizontal position (i,j) of the ORigin
INTEGER :: IYOR                ! of the model 2 domain, relative to model 1
INTEGER :: IDXRATIO            !  x and y-direction resolution RATIO
INTEGER :: IDYRATIO            !      between model 2 and model 1
INTEGER :: IXSIZE,IYSIZE       ! number of model 1 grid points in x and y-directions
                               ! in the model 2 physical domain
LOGICAL :: GBAL_ONLY           !  compute only anelastique balance
CHARACTER (LEN=28) :: YDOMAIN = ' '   ! input fm-file for grid definition
CHARACTER (LEN=28) :: YSPAFILE = ' '  ! possible name of the output FM-file
CHARACTER (LEN= 2) :: YSPANBR = '00'  ! NumBeR associated to the SPAwned file
CHARACTER (LEN=28) :: YDADINIFILE = ' ' ! Name of dad model for model 1  
CHARACTER (LEN=28) :: YDADSPAFILE = ' '  ! Name of dad model for model 2 
CHARACTER(LEN=28) :: CINIFILE ! re-declaration because of namelist
CHARACTER(LEN=28) :: CINIFILEPGD ! re-declaration because of namelist

INTEGER :: IMI
CHARACTER (LEN=28) :: YSONFILE = ' '  ! Name of SON input file
!
!*       0.3    Namelist declarations
!
NAMELIST/NAM_GRID2_SPA/  IXOR,IYOR, &! horizontal position (i,j) of the origin 
                                     ! of the model 2 domain, relative to model 1
                         IDXRATIO,  &! x and y-direction resolution RATIO between
                         IDYRATIO,  &! model 2 (to be spawned) and model 1
                         IXSIZE,    &! number of grid meshes of model 1 covered
                         IYSIZE,    &! by physical model 2 in each direction
                         GBAL_ONLY   ! compute only anelastique balance
NAMELIST/NAM_LUNIT2_SPA/ CINIFILE,  &! In file name (model 1)
                         CINIFILEPGD,&! pgd file associated with In file name (model 1)
                         YDOMAIN,   &! fm file in which is read the grid
                                     ! (if used, previous namelist is obsolete)
                         YSPAFILE,  &! Name proposed for spawned file
                         YSPANBR,   &! NumBeR associated to the SPAwned file
                                     ! (used if YSPAFILE is not specified)
                         NVERB,     &! verbosity flag
                         YDADINIFILE, & ! Name of dad model for model 1 
                         YDADSPAFILE, & ! Name of dad model for model 2
                         YSONFILE ! Name of SON input file
!
!-------------------------------------------------------------------------------
!
!*       1.    initialize logical unit number of the EXSPA file :
!
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(2)
!
CLUOUT  = 'OUTPUT_LISTING2'
YEXSPA  = 'SPAWN1.nam'
CALL OPEN_ll(UNIT=ILUOUT,FILE=CLUOUT,IOSTAT=IRESP,FORM='FORMATTED',ACTION='WRITE', &
     MODE=GLOBAL)
CALL OPEN_ll(unit=ILUSPA,FILE=YEXSPA,iostat=IRESP,status="OLD",action='READ',  &
             form='FORMATTED',position="REWIND",mode=GLOBAL) 
!
!
!*       2.    read the EXSPA file :
!
IDXRATIO=NUNDEF
IDYRATIO=NUNDEF
IXSIZE=NUNDEF
IYSIZE=NUNDEF
IXOR=NUNDEF
IYOR=NUNDEF
GBAL_ONLY=.FALSE.
!
CALL POSNAM(ILUSPA,'NAM_GRID2_SPA',GFOUND,ILUOUT)
IF (GFOUND) READ(ILUSPA,NAM_GRID2_SPA)
CINIFILE=CINIFILE_n
CALL POSNAM(ILUSPA,'NAM_LUNIT2_SPA',GFOUND,ILUOUT)
IF (GFOUND) READ(ILUSPA,NAM_LUNIT2_SPA)
CINIFILE_n=CINIFILE
CINIFILEPGD_n=CINIFILEPGD
!!
CALL POSNAM(ILUSPA,'NAM_CONFIO',GFOUND,ILUOUT)
IF (GFOUND) READ(ILUSPA,NAM_CONFIO)
CALL SET_CONFIO_ll(LCDF4, LLFIOUT, LLFIREAD)
#ifdef MNH_NCWRIT
CALL POSNAM(ILUSPA,'NAM_NCOUT',GFOUND,ILUOUT)
IF (GFOUND) READ(ILUSPA,NAM_NCOUT)
#endif
CALL CLOSE_ll(YEXSPA)
!
!
!*       3.    model 1 and SON1 FM file name
!
HINIFILE = CINIFILE_n
HINIFILEPGD = CINIFILEPGD_n
HSONFILE = YSONFILE
!
!*       4.    CINIFILE value is also used for model 2 (cf SPAWN_MODEL2)
!
!
!
!*       5.    other file names
!
HDOMAIN  = YDOMAIN
HSPAFILE = YSPAFILE
HSPANBR  = YSPANBR
HDADINIFILE=YDADINIFILE
HDADSPAFILE=YDADSPAFILE
!
!*       6.    zoom characteristics
!
KDXRATIO = IDXRATIO
KDYRATIO = IDYRATIO
KXSIZE   = IXSIZE
KYSIZE   = IYSIZE
KXOR     = IXOR
KYOR     = IYOR
!
OBAL_ONLY=GBAL_ONLY
!-------------------------------------------------------------------------------
!
CALL GOTO_MODEL(IMI)
!
END SUBROUTINE READ_EXSPA

