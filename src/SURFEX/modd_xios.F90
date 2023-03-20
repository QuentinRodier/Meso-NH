!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_XIOS
!     ######################
!
!!****  *MODD_XIOS - nest for variables used in interfacing XIOS to Surfex / Arpege
!!
!!    PURPOSE
!!    -------
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
!!	S.Sénési   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       08/2015
!!      Modified       11/2016 R. Séférian add land-use tiles for CMIP6
!
!*       0.   DECLARATIONS
!             ------------
#ifdef WXIOS
USE XIOS ,    ONLY : XIOS_CONTEXT
TYPE(xios_context) :: TXIOS_CONTEXT                ! Xios context handle 
#else
INTEGER            :: TXIOS_CONTEXT                ! Fake Xios context handle 
#endif
!
!  Basic toggle
!
LOGICAL            :: LXIOS                        ! Do we use XIOS for outputing diags
!
!  Setup variables 
!
CHARACTER(LEN=6)   :: YXIOS_CONTEXT= "surfex"      ! Context name known to Xios (must match a context declared in xml file)
CHARACTER(LEN=14)  :: COUTPUT_DEFAULT="surfex_cselect" ! XIOS id for the file receiving all Surfex variables selected 
                                                   ! by CSELECT (could/should exist in xml configuration file)
LOGICAL            :: LALLOW_ADD_DIM=.FALSE.       ! allow multi-dimensional output ?
LOGICAL            :: LGRID_MODE=.FALSE.           ! Should we declare fields with a grid rather
                                                   ! than with a domain(+axis) 
INTEGER            :: NBASE_XIOS_FREQ=1            ! Base frequency for calling XIOS (unit=timestep)
LOGICAL            :: LXIOS_INVERT_LEVELS=.FALSE.  ! allow to invert atmospheric levels
!
!  Evolving variables
!
LOGICAL            :: LXIOS_DEF_CLOSED             ! Has the Surfex context definition already been closed ?
CHARACTER(LEN=6)   :: YXIOS_DOMAIN                 ! When writing diags using write_diag_surf, name of the current tile 
INTEGER            :: NTIMESTEP=-1                 ! Last value of timestep sent to XIOS 
INTEGER            :: NBLOCK=1                     ! Number of blocks in the MPI-task (NPROMA blocks when in 
                                                   ! Arpege). For xios_send_block
!
!  Names for various dimensions (this section could be move elsewhere when more 
!  output schemes will handle dimension names)
!
!  from atmosphere
CHARACTER(LEN=30) :: YSWBAND_DIM_NAME            ="swband"
CHARACTER(LEN=30) :: YATM_VAXIS_NAME             ="klev"
CHARACTER(LEN=30) :: YATM_VAXIS_HALF_NAME        ="klev_half"
CHARACTER(LEN=30) :: YATM_VAXIS_LL_NAME          ="klev_ll"
!
!  from surfex
CHARACTER(LEN=30) :: YPATCH_DIM_NAME             ="patch"
CHARACTER(LEN=30) :: YPATCHES_DIM_NAME           ="patches"
CHARACTER(LEN=30) :: YSNOW_LAYER_DIM_NAME        ="snow_layer"
CHARACTER(LEN=30) :: YGROUND_LAYER_DIM_NAME      ="ground_layer"
CHARACTER(LEN=30) :: YTG_LAYER_DIM_NAME          ="ground_layer_for_temperature"
CHARACTER(LEN=30) :: YSOIL_CARBON_POOL_DIM_NAME  ="soil_carbon_pools"
CHARACTER(LEN=30) :: YLANDUSE_TILES_DIM_NAME     ="landusetype4"
CHARACTER(LEN=30) :: YVEGTYPE_DIM_NAME           ="vegtype"
!
!--------------------------------------------------------------------------------
! Number of landuse patches
!--------------------------------------------------------------------------------
!
INTEGER, PARAMETER :: NLUT = 4
!
END MODULE MODD_XIOS
