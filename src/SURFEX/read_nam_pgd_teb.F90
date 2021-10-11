!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_NAM_PGD_TEB(HPROGRAM,KTEB_PATCH, HBEM,         &
                                  HCOOL_COIL, HHEAT_COIL, OAUTOSIZE, &
                                  HROAD_GRID, KROOF_LAYER,           &
                                  KWALL_LAYER, KFLOOR_LAYER,         &
                                  KMASS_LAYER, KBEMCOMP,KTIME_CHANGE,&
                                  OGREENROOF, HURBTREE, OURBHYDRO,   &
                                  OSOLAR_PANEL, OSPARTACUS, OEXPLW,  &
                                  OCHECK_TEB, OEPS_BDGT_GLOB,        &
                                  OEPS_BDGT_FAC, HTEST )
!
!     ##############################################################
!
!!**** *READ_NAM_PGD_TEB* reads namelist for TEB
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2005
!!       2008 B. Decharme : uniform value of subgrid drainage coefficient
!!    12/2008 E. Martin   : files of data for subgrid drainage 
!!                          and subgridrunoff
!!    06/2009 B. Decharme : files of data for topographic index
!!
!!      A. Lemonsu        07/2012         Key for greenroofs & greenwalls
!!      A. Lemonsu        07/2012         Key for urban hydrology
!!      E.Redon/A.Lemonsu 12/2015         Key for urban trees
!!      M. Goret          04/2017         Add NTIME_CHANGE
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! Type of program
CHARACTER(LEN=2), INTENT(IN) :: HTEST    ! must be equal to 'OK'  
!
INTEGER,           INTENT(OUT) :: KTEB_PATCH      ! number of patches
 CHARACTER(LEN=3), INTENT(OUT) :: HBEM            ! to use BEM
 CHARACTER(LEN=6), INTENT(OUT) :: HCOOL_COIL      ! type of cooling coil
 CHARACTER(LEN=6), INTENT(OUT) :: HHEAT_COIL      ! type of heating coil
LOGICAL,          INTENT(OUT) :: OAUTOSIZE        ! Flag to activate autosize calculations
 CHARACTER(LEN=6), INTENT(OUT) :: HROAD_GRID      ! vertical discretization for road soil column
INTEGER,          INTENT(OUT) :: KROOF_LAYER      ! number of roof layers
INTEGER,          INTENT(OUT) :: KWALL_LAYER      ! number of wall layers
INTEGER,          INTENT(OUT) :: KFLOOR_LAYER     ! number of floor layers
INTEGER,           INTENT(OUT) :: KMASS_LAYER     ! number of mass layers
INTEGER,           INTENT(OUT) :: KBEMCOMP        ! number of compartments in bem
INTEGER,           INTENT(OUT) :: KTIME_CHANGE    ! number of change of legal time during the simulation
LOGICAL,          INTENT(OUT) :: OGREENROOF       ! key for greenroof activation
 CHARACTER(LEN=4), INTENT(OUT) :: HURBTREE        ! To dealwith street trees or green walls
LOGICAL,          INTENT(OUT) :: OURBHYDRO        ! key for urban hydrology activation
LOGICAL,          INTENT(OUT) :: OSOLAR_PANEL     ! key for solar panel activation
LOGICAL,          INTENT(OUT) :: OSPARTACUS       ! key for SPARTACUS-Surface activation
LOGICAL,          INTENT(OUT) :: OEXPLW           ! key for explicit calculation of longwave radiation
LOGICAL,          INTENT(OUT) :: OCHECK_TEB      ! key for energy budget verification for TEB
 REAL,            INTENT(OUT) :: OEPS_BDGT_GLOB  ! Difference allowed in energy budget for TEB for global processes
 REAL,            INTENT(OUT) :: OEPS_BDGT_FAC !  Difference allowed in energy budget for TEB for facade processes
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                           :: ILUOUT    ! output listing logical unit
INTEGER                           :: ILUNAM    ! namelist file logical unit
LOGICAL                           :: GFOUND    ! flag when namelist is present
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                  :: NTEB_PATCH       ! number of patches
 CHARACTER(LEN=3)         :: CBEM             ! to use BEM
 CHARACTER(LEN=6)         :: CCOOL_COIL       ! type of cooling coil
 CHARACTER(LEN=6)         :: CHEAT_COIL       ! type of heating coil
LOGICAL                  :: LAUTOSIZE        ! Flag to activate autosize calculations
 CHARACTER(LEN=6)        :: CROAD_GRID       ! vertical discretization for road soil column
INTEGER                  :: NROOF_LAYER      ! number of roof layers
INTEGER                  :: NWALL_LAYER      ! number of wall layers
INTEGER                  :: NFLOOR_LAYER     ! number of floor layers
INTEGER                  :: NMASS_LAYER      ! number of floor layers
INTEGER                  :: NBEMCOMP         ! number of compartments in bem
INTEGER                  :: NTIME_CHANGE     ! number of time the legal time change during the simulation
LOGICAL                  :: LGREENROOF       ! key for greenroof activation
 CHARACTER(LEN=4)        :: CURBTREE         ! To deal with street trees or green walls
LOGICAL                  :: LURBHYDRO        ! key for urban hydrology activation
LOGICAL                  :: LSOLAR_PANEL     ! key for solar panel activation
LOGICAL                  :: LSPARTACUS       ! key for SPARTACUS-Surface activation
LOGICAL                  :: LEXPLW           ! key for explicit calculation of longwave radiation
LOGICAL                  :: LCHECK_TEB       ! key for energy budget verification for TEB
  REAL                   :: XEPS_BDGT_GLOB   ! Difference allowed in energy budget for TEB for global processes
  REAL                   :: XEPS_BDGT_FAC    !  Difference allowed in energy budget for TEB for facade processes
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_TEB/ NTEB_PATCH, CBEM, CCOOL_COIL, CHEAT_COIL, LAUTOSIZE,           &
                  CROAD_GRID, NFLOOR_LAYER, NMASS_LAYER, NBEMCOMP,               &
                  NROOF_LAYER, NWALL_LAYER, LGREENROOF, LURBHYDRO, LSOLAR_PANEL, &
                  LSPARTACUS, LEXPLW, CURBTREE, NTIME_CHANGE, LCHECK_TEB,        &
                  XEPS_BDGT_GLOB, XEPS_BDGT_FAC
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations of defaults
!             ---------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_TEB',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('READ_NAM_PGD_TEB: FATAL ERROR DURING ARGUMENT TRANSFER')
ENDIF
!
NTEB_PATCH         = 1
CBEM               = 'DEF'
LAUTOSIZE          =.FALSE.
CHEAT_COIL         ='IDEAL'
CCOOL_COIL         ='IDEAL'
CROAD_GRID         ='LOW5  '
NROOF_LAYER        = 5
NWALL_LAYER        = 5
NFLOOR_LAYER       = 5
NMASS_LAYER        = 5
NBEMCOMP           = 1
NTIME_CHANGE       = 0
LGREENROOF         = .FALSE.
LURBHYDRO          = .FALSE.
LSOLAR_PANEL       = .FALSE.
LSPARTACUS         = .FALSE.
LEXPLW             = .FALSE.
CURBTREE           = 'NONE'
LCHECK_TEB         = .TRUE.
XEPS_BDGT_GLOB     = 1.E-3
XEPS_BDGT_FAC      = 1.E-6
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
HROAD_GRID   = CROAD_GRID
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_TEB',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_TEB)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
KTEB_PATCH   = NTEB_PATCH           ! number of patches
HBEM         = CBEM
HCOOL_COIL   = CCOOL_COIL
HHEAT_COIL   = CHEAT_COIL
OAUTOSIZE    = LAUTOSIZE
IF (LEN_TRIM(CROAD_GRID)>0) HROAD_GRID   = CROAD_GRID
KROOF_LAYER  = NROOF_LAYER
KWALL_LAYER  = NWALL_LAYER
KFLOOR_LAYER = NFLOOR_LAYER
KMASS_LAYER  = NMASS_LAYER
KBEMCOMP     = NBEMCOMP
KTIME_CHANGE = NTIME_CHANGE
!
OGREENROOF   = LGREENROOF
OURBHYDRO    = LURBHYDRO
OSOLAR_PANEL = LSOLAR_PANEL
OSPARTACUS   = LSPARTACUS
OEXPLW       = LEXPLW
OCHECK_TEB  = LCHECK_TEB
OEPS_BDGT_GLOB = XEPS_BDGT_GLOB
OEPS_BDGT_FAC = XEPS_BDGT_FAC
!
HURBTREE     = CURBTREE
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_PGD_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_PGD_TEB
