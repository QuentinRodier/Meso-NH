! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ################
      PROGRAM SPAWNING
!     ################
!
!!****  *SPAWNING * -general monitor to spawn a model from an other one.
!!
!!    PURPOSE
!!    -------
!! 
!!      This program is the general monitor to spawn a model. Firstly, it calls
!!    the subroutine INIT, which performs the initialization of the model 1.
!!      The domain size and configuration of model 2 are initialized in
!!     INIT before the data structures of ComLib set up.
!!      Then, the program calls the subroutine SPAWN_MODEL2, which initializes
!!    by horizontal interpolation, the model 2 in a sub-domain of model 1, 
!!    and writes the resulting fields in a FM-file.
!!
!!
!!**  METHOD
!!    ------
!!     
!!
!!    EXTERNAL
!!    --------
!!
!!       subroutine INIT      : performs the initialization of the model 1
!!
!!       subroutine SPAWN_MODEL2 : performs horizontal interpolation and writes
!!                                 FM-file   
!!
!!
!! 
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    NONE
!!
!!    REFERENCE
!!    ---------
!!
!!       NONE
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
!!      Original    10/01/95
!!      Modification 29/01/96  (Lafore) Update for MASDEV2_2 version
!!      Modification 19/03/96  (Lafore) Spawning for surface fields
!!      Modification 24/10/96  (Masson) Initialization of outer points
!!      Modification 19/11/96  (Masson) Add deep convection
!!      Modification 03/11/97  (Lafore) update of call to BOUNDARIES
!!      Modification 03/06/98  (Stein ) update of call to BOUNDARIES
!!      Modification 15/03/99  (Masson) call to READ_EXSPA and program name
!!      Modification 15/07/99  (Jabouille) create MODD_SPAWN
!!      Modification 14/12/00  (Jabouille) add NAM_BLANK reading
!!      Modification 15/10/01  (Mallet) allow namelists in different orders
!!      Modification 07/07/05  (Barbary) spawn with 2 input files (father+son1)
!!                                      to keep finest fields of son1
!!      Modification 05/06     Remove EPS
!!      Modification 19/03/2008 (J.Escobar) rename INIT to INIT_MNH --> grib problem
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!  
!*       0.1    Declarative modules common to all the models
!
USE MODD_CONF
USE MODD_CST
USE MODD_DYN
USE MODD_GRID
USE MODD_LUNIT
USE MODD_PARAMETERS
USE MODD_REF
USE MODD_SPAWN
USE MODN_BLANK
USE MODD_NSV
!  
!*       0.2    Declarative modules of model 1
!
USE MODD_CONF_n
USE MODD_CURVCOR_n
USE MODD_DIM_n
USE MODD_DYN_n, LRES_n=>LRES, XRES_n=>XRES 
USE MODD_FIELD_n
USE MODD_LSFIELD_n
USE MODD_LBC_n
USE MODD_LUNIT_n
USE MODD_OUT_n
USE MODD_PARAM_n
USE MODD_REF_n
USE MODD_TIME_n
USE MODD_CH_MNHC_n
! 
USE MODE_IO_ll
USE MODE_ll
USE MODE_POS
USE MODE_FM
USE MODE_MODELN_HANDLER
!
USE MODI_SPAWN_MODEL2    
USE MODI_BOUNDARIES
!
USE MODI_VERSION
USE MODI_INIT_MNH
USE MODI_DEALLOC_SURFEX
!
!
IMPLICIT NONE
!
!*       0.3    Local variables
!
!
CHARACTER (LEN=28) :: YSONFILE = ' '  ! possible name of SON input FM-file
CHARACTER (LEN=28) :: YSPAFILE = ' '  ! possible name of the output FM-file
CHARACTER (LEN= 2) :: YSPANBR = '00'  ! NumBeR associated to the SPAwned file
INTEGER            :: IINFO_ll        ! return code of // routines
INTEGER :: IRESP                      ! Return codes in FM routines
INTEGER :: ILUSPA,ILUOUT              ! Logical unit number for the EXSPA file
CHARACTER (LEN=32) :: YEXSPA          ! Name of the EXSPA file
LOGICAL :: GFOUND                     ! Return code when searching namelist
!
LOGICAL :: LSPAWN_SURF = .TRUE.  ! .TRUE. : surface fields are spawned
LOGICAL                           :: LRES
REAL                              :: XRES
NAMELIST/NAM_SPAWN_SURF/LSPAWN_SURF, LRES, XRES  
!
!-------------------------------------------------------------------------------
!
! First Switch to model 1 variables
CALL GOTO_MODEL(1)
!
CALL VERSION
CPROGRAM='SPAWN '
CDOMAIN= ''
!
CALL INITIO_ll()
!-------------------------------------------------------------------------------
!
!*       1.    SPAWNING INITIALIZATION 
!              -----------------------
!
CALL READ_EXSPA(CINIFILE,CINIFILEPGD,&
                NXOR,NYOR,NXSIZE,NYSIZE,NDXRATIO,NDYRATIO, &
                LBAL_ONLY, &
                CDOMAIN,YSPAFILE,YSPANBR,CDADINIFILE,CDADSPAFILE,YSONFILE)
!
!-------------------------------------------------------------------------------
!
!*       2.    MODEL 1 INITIALIZATION
!              ----------------------
!
CALL INIT_MNH
!
CALL FMCLOS_ll(CINIFILE,'KEEP',CLUOUT,IRESP)
CALL FMCLOS_ll(CINIFILEPGD,'KEEP',CLUOUT,IRESP)
!
!*       3.    NAM_BLANK READING AND EXSPA file CLOSURE
!              ----------------------------------------
!
YEXSPA  = 'SPAWN1.nam'
CALL OPEN_ll(unit=ILUSPA,FILE=YEXSPA,iostat=IRESP,status="OLD",action='READ',  &
             form='FORMATTED',position="REWIND",mode=GLOBAL) 
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
CALL INIT_NMLVAR
CALL POSNAM(ILUSPA,'NAM_SPAWN_SURF',GFOUND)
IF (GFOUND) READ(UNIT=ILUSPA,NML=NAM_SPAWN_SURF)
CALL UPDATE_MODD_FROM_NMLVAR
CALL POSNAM(ILUSPA,'NAM_BLANK',GFOUND)
IF (GFOUND) READ(UNIT=ILUSPA,NML=NAM_BLANK)
!!$CALL CLOSE_ll(YEXSPA)
!
!-------------------------------------------------------------------------------
!
!*       4.    INITIALIZATION OF OUTER POINTS OF MODEL 1
!              -----------------------------------------
!
CALL BOUNDARIES                                                     & 
           (XTSTEP,CLBCX,CLBCY,NRR,NSV,1,                           &
            XLBXUM,XLBXVM,XLBXWM,XLBXTHM,XLBXTKEM,XLBXRM,XLBXSVM,   &
            XLBYUM,XLBYVM,XLBYWM,XLBYTHM,XLBYTKEM,XLBYRM,XLBYSVM,   &
            XLBXUS,XLBXVS,XLBXWS,XLBXTHS,XLBXTKES,XLBXRS,XLBXSVS,   &
            XLBYUS,XLBYVS,XLBYWS,XLBYTHS,XLBYTKES,XLBYRS,XLBYSVS,   &
            XRHODJ,                                                 &
            XUM, XVM, XWM, XTHM, XTKEM, XRM, XSVM,XSRCM,            &
            XUT, XVT, XWT, XTHT, XTKET, XRT, XSVT                   )
!
!-------------------------------------------------------------------------------
!
!*       5.    SPAWNING OF MODEL 2 FROM MODEL 1
!              --------------------------------
!
CALL GOTO_MODEL(2)
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
CALL INIT_NMLVAR
CALL POSNAM(ILUSPA,'NAM_SPAWN_SURF',GFOUND)
IF (GFOUND) READ(UNIT=ILUSPA,NML=NAM_SPAWN_SURF)
CALL UPDATE_MODD_FROM_NMLVAR
CALL GOTO_MODEL(1)
CALL CLOSE_ll(YEXSPA)
!
CALL GO_TOMODEL_ll(2,IINFO_ll)
!
CALL SPAWN_MODEL2 (NRR,NSV_USER,CTURB,CSURF,CCLOUD,                     &
                   CCHEM_INPUT_FILE,YSPAFILE,YSPANBR,YSONFILE,          &
                   CINIFILE, CINIFILEPGD, LSPAWN_SURF                   )
!
CALL DEALLOC_SURFEX
 !callabortstop
CALL CLOSE_ll(CLUOUT,IOSTAT=IRESP)
CALL END_PARA_ll(IINFO_ll)
!JUAN CALL ABORT
STOP

CONTAINS 

SUBROUTINE INIT_NMLVAR
LRES=LRES_n
XRES=XRES_n
END SUBROUTINE INIT_NMLVAR

SUBROUTINE UPDATE_MODD_FROM_NMLVAR
LRES_n=LRES
XRES_n=XRES
END SUBROUTINE UPDATE_MODD_FROM_NMLVAR
!
!-------------------------------------------------------------------------------
!
END PROGRAM SPAWNING  
