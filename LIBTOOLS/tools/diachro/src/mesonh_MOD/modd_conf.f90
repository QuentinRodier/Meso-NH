!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!     #################
      MODULE MODD_CONF
!     #################
!
!!****  *MODD_CONF* - declaration of configuration variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the variables
!     which concern the configuration of all models. For exemple, 
!     the type of geometry (Cartesian or conformal projection plane). 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_CONF)
!!      Technical Specifications Report of the Meso-NH (chapters 2 and 3)
!!       
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/05/94    
!!      J. Stein                      09/01/95   add the 1D switch    
!!      J. Stein and P. Jabouille     30/04/96   add the storage type         
!!      J.-P. Pinty                   13/02/96   add LFORCING switch
!!      J. Stein                      25/07/97   add the equation system switch    
!!      P. Jabouille                  07/05/98   add LPACK
!!      V. Masson                     18/03/98   add the VERSION switch
!!      V. Masson                     15/03/99   add PROGRAM swith
!!      P. Jabouille                  21/07/99   add NHALO and CSPLIT
!!      P. Jabouille                  26/06/01   lagrangian variables
!!      V. Masson                     09/07/01   add LNEUTRAL switch
!!      P. Jabouille                  18/04/02   add NBUGFIX and CBIBUSER
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
CHARACTER (LEN=5),SAVE :: CCONF  ! Configuration of models
                                 !  'START' for start configuration (variables
                                 ! at time t and t-dt are the same in the
                                 ! initial file)
                                 !  'RESTART' for restart configuration 
                                 ! (variables  at time t and t-dt are different)   
                                 !  'POST' for post-treatment configuration 
LOGICAL,SAVE      :: LTHINSHELL  ! Logical for thinshell approximation
                                 ! .TRUE.  = thinshell approximation
                                 ! .FALSE. = no thinshell approximation
LOGICAL,SAVE      :: LCARTESIAN  ! Logical for cartesian geometry :
                                 !  .TRUE.  = cartesian geometry 
                                 !  .FALSE. = conformal projection
LOGICAL,SAVE      :: L2D         ! Logical for 2D model version
                                 ! .TRUE.  = 2D model version
                                 ! .FALSE. = 3D model version
LOGICAL,SAVE      :: L1D         ! Logical for 1D model version
                                 ! .TRUE.  = 1D model version
                                 ! .FALSE. = 2D or 3D model version
LOGICAL,SAVE      :: LFLAT       ! Logical for zero ororography
                                 ! .TRUE.  = no orography (zs=0.)
                                 ! .FALSE. = orography  
INTEGER,SAVE      :: NMODEL      ! Number of nested models
INTEGER,SAVE      :: NVERB       ! Level of informations on output-listing
                                 !  0 for minimum of prints
                                 ! 5 for intermediate level of prints
                                 ! 10 for maximum of prints 
CHARACTER (LEN=5),SAVE :: CEXP   !  Experiment name
CHARACTER (LEN=5),SAVE :: CSEG   ! name of segment
CHARACTER (LEN=2),SAVE :: CSTORAGE_TYPE ! storage type for the informations 
                                 ! written in the FM files ( 'TT' if the MesoNH 
                                 ! prognostic fields are at the same instant;
                                 ! 'MT' if they are taken at two instants in
                                 ! succession; 'PG' for PGD files informations )
LOGICAL,SAVE :: LFORCING         ! Logical for forcing sources
                                 ! .TRUE.  = add forcing sources
                                 ! .FALSE. = no forcing fields
!
CHARACTER (LEN=3),SAVE :: CEQNSYS! EQuatioN SYStem resolved by the MESONH model
                                 ! 'LHE' Lipps and HEmler anelastic system
                                 ! 'DUR' approximated form of the DURran version
                                 ! of the anelastic sytem
                                 ! 'MAE' classical Modified Anelastic Equations
                                 ! but with not any approximation in the
                                 ! momentum equation
                                 ! 'FCE' fully compressible equations ( not
                                 ! yet developped )
LOGICAL,SAVE      :: LPACK       ! Logical to compress 1D or 2D FM files
!
!
INTEGER,SAVE :: NMASDEV           ! NMASDEV=XY corresponds to the masdevX_Y
INTEGER,SAVE :: NBUGFIX           ! NBUGFIX=n corresponds to the BUGn of masdevX_Y
CHARACTER(LEN=10),SAVE :: CBIBUSER! CBIBUSER is the name of the user binary library
!
CHARACTER(LEN=6),SAVE :: CPROGRAM ! CPROGRAM is the program currently running:
!                                 ! 'PGD   ','ADVPGD','NESPGD','REAL  ','IDEAL '
!                                 ! 'MESONH','SPAWN ','DIAG  '
!
INTEGER,SAVE      :: NHALO        ! Size of the halo for parallel distribution
!
CHARACTER (LEN=10),SAVE :: CSPLIT ! kind of domain splitting for parallel distribution
                                  !  "BSPLITTING","XSPLITTING","YSPLITTING"
LOGICAL,SAVE      :: LLG         ! Logical to use lagrangian variables
LOGICAL,SAVE      :: LINIT_LG    ! to reinitialize lagrangian variables
LOGICAL,SAVE      :: LNOMIXLG    ! to use turbulence for lagrangian variables
!
LOGICAL,SAVE      :: LNEUTRAL ! True if ref. theta field is uniform
!
END MODULE MODD_CONF
