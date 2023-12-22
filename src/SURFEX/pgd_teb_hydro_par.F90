!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_HYDRO_PAR (DTCO, UG, U, USS, DTH, KDIM, HPROGRAM)
!     ##############################################################

!!**** *PGD_TEB_HYDRO_PAR* monitor for averaging and interpolations of cover fractions
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
!!    A. Lemonsu       Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2009
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_DATA_TEB_HYDRO_n, ONLY : DATA_TEB_HYDRO_t
USE MODD_SURF_PAR, ONLY : NFILENAMELGTMAX, XUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_FIELD
USE MODI_ABOR1_SFX
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
INTEGER, INTENT(IN) :: KDIM
TYPE(DATA_TEB_HYDRO_t) :: DTH
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: ILUNAM ! namelist file  logical unit
LOGICAL :: GFOUND ! true if namelist is found
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!
! uniform value
!
! Additional parameters for urban hydrology (option LURBHYDRO)
REAL                                   :: XUNIF_DENS_WASTE     ! Wastewater sewer length density (-)
REAL                                   :: XUNIF_DENS_STORM     ! Stormwater sewer length density (-)
REAL                                   :: XUNIF_DSEWER         ! waste water sewer depth
REAL                                   :: XUNIF_WS_ROOF_MAX         ! Max. capacity of surface roof water storage 
REAL                                   :: XUNIF_WS_ROAD_MAX         ! Max. capacity of surface road water storage
REAL                                   :: XUNIF_IP_SEWER            ! Parameter for parasite infiltrations into sewer
REAL                                   :: XUNIF_CONNEX              ! Impervious surfaces connexion rate to the sewer
REAL                                   :: XUNIF_INFIL_ROAD          ! Water infiltration through the roads
REAL                                   :: XUNIF_URBDRAIN            ! Limitation of urban deep drainage (0-1)
!
! name of files containing data
!
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_DENS_WASTE    ! waste water sewer length
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_DENS_STORM    ! storm water sewer length
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_DSEWER        ! waste water sewer depth
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_WS_ROOF_MAX         ! Max. capacity of surface roof water storage
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_WS_ROAD_MAX         ! Max. capacity of surface road water storage
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_IP_SEWER            ! Parameter for parasite infiltrations into sewer
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_CONNEX              ! Impervious surfaces connexion rate to the sewer
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_INFIL_ROAD          ! Water infiltration through the roads
CHARACTER(LEN=NFILENAMELGTMAX)        :: CFNAM_URBDRAIN            ! Limitation of urban deep drainage (0-1)
!
! type of files containing data
!
CHARACTER(LEN=6)                      :: CFTYP_DENS_WASTE    ! waste water sewer length
CHARACTER(LEN=6)                      :: CFTYP_DENS_STORM    ! storm water sewer length
CHARACTER(LEN=6)                      :: CFTYP_DSEWER        ! waste water sewer depth
CHARACTER(LEN=6)                      :: CFTYP_WS_ROOF_MAX         ! Max. capacity of surface roof water storage 
CHARACTER(LEN=6)                      :: CFTYP_WS_ROAD_MAX         ! Max. capacity of surface road water storage
CHARACTER(LEN=6)                      :: CFTYP_IP_SEWER            ! Parameter for parasite infiltrations into sewer
CHARACTER(LEN=6)                      :: CFTYP_CONNEX              ! Impervious surfaces connexion rate to the sewer
CHARACTER(LEN=6)                      :: CFTYP_INFIL_ROAD          ! Water infiltration through the roads
CHARACTER(LEN=6)                      :: CFTYP_URBDRAIN            ! Limitation of urban deep drainage (0-1)
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_TEB_HYDRO/ XUNIF_DENS_WASTE, XUNIF_DENS_STORM, XUNIF_DSEWER,        &
                                XUNIF_WS_ROOF_MAX, XUNIF_WS_ROAD_MAX, XUNIF_IP_SEWER, &
                                XUNIF_CONNEX, XUNIF_INFIL_ROAD, XUNIF_URBDRAIN,       &
                                CFNAM_DENS_WASTE, CFNAM_DENS_STORM, CFNAM_DSEWER,     &
                                CFNAM_WS_ROOF_MAX, CFNAM_WS_ROAD_MAX, CFNAM_IP_SEWER, &
                                CFNAM_CONNEX, CFNAM_INFIL_ROAD, CFNAM_URBDRAIN,       &
                                CFTYP_DENS_WASTE, CFTYP_DENS_STORM, CFTYP_DSEWER,     &
                                CFTYP_WS_ROOF_MAX, CFTYP_WS_ROAD_MAX, CFTYP_IP_SEWER, &
                                CFTYP_CONNEX, CFTYP_INFIL_ROAD, CFTYP_URBDRAIN
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_HYDRO_PAR',0,ZHOOK_HANDLE)

XUNIF_DENS_WASTE   = XUNDEF
XUNIF_DENS_STORM   = XUNDEF
XUNIF_DSEWER       = XUNDEF
XUNIF_WS_ROOF_MAX  = 1.
XUNIF_WS_ROAD_MAX  = 1.
XUNIF_IP_SEWER     = 0.
XUNIF_CONNEX       = 1.
XUNIF_INFIL_ROAD   = 0.
XUNIF_URBDRAIN     = 0.
CFNAM_DENS_WASTE   = ''
CFNAM_DENS_STORM   = ''
CFNAM_DSEWER       = ''
CFNAM_WS_ROOF_MAX  = ''
CFNAM_WS_ROAD_MAX  = ''
CFNAM_IP_SEWER     = ''
CFNAM_CONNEX       = ''
CFNAM_INFIL_ROAD   = ''
CFNAM_URBDRAIN     = ''
CFTYP_DENS_WASTE   = '      '
CFTYP_DENS_STORM   = '      '
CFTYP_DSEWER       = '      '
CFTYP_WS_ROOF_MAX  = '      '
CFTYP_WS_ROAD_MAX  = '      '
CFTYP_IP_SEWER     = '      '
CFTYP_CONNEX       = '      '
CFTYP_INFIL_ROAD   = '      '
CFTYP_URBDRAIN     = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_TEB_HYDRO',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_TEB_HYDRO)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
! FOR THE TIME BEING, the hydrology fields are NOT in ecoclimap or ecoclimapSG
!
! Therefore, they MUST be provided by user
!
! No choice !!!!
!-------------------------------------------------------------------------------

DTH%LDATA_TEB_HYDRO = .TRUE.

ALLOCATE(DTH%XPAR_DENS_WASTE    (KDIM               )) 
ALLOCATE(DTH%XPAR_DENS_STORM    (KDIM               ))
ALLOCATE(DTH%XPAR_DSEWER        (KDIM               ))
ALLOCATE(DTH%XPAR_WS_ROOF_MAX   (KDIM               )) 
ALLOCATE(DTH%XPAR_WS_ROAD_MAX   (KDIM               ))
ALLOCATE(DTH%XPAR_IP_SEWER      (KDIM               ))
ALLOCATE(DTH%XPAR_CONNEX        (KDIM               )) 
ALLOCATE(DTH%XPAR_INFIL_ROAD    (KDIM               ))
ALLOCATE(DTH%XPAR_URBDRAIN      (KDIM               ))
!-------------------------------------------------------------------------------
! Additional fields for urban hydrology (LURBHYDRO)
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'DENS_WASTE: density of wastewater sewer length','TWN',CFNAM_DENS_WASTE,       &
                 CFTYP_DENS_WASTE,XUNIF_DENS_WASTE,DTH%XPAR_DENS_WASTE(:))  
!

 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'DENS_STORM: density of stormwater sewer length','TWN',CFNAM_DENS_STORM,       &
                 CFTYP_DENS_STORM,XUNIF_DENS_STORM,DTH%XPAR_DENS_STORM(:))  
! 
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'DSEWER: depth of waste water sewer','TWN',CFNAM_DSEWER,        &
                 CFTYP_DSEWER,XUNIF_DSEWER,DTH%XPAR_DSEWER(:))  
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'WS_ROOF_MAX: maximum water capacity of ROOF reservoir (mm)','TWN',CFNAM_WS_ROOF_MAX,       &
                 CFTYP_WS_ROOF_MAX,XUNIF_WS_ROOF_MAX,DTH%XPAR_WS_ROOF_MAX(:))  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'WS_ROAD_MAX: maximum water capacity of ROAD reservoir (mm)','TWN',CFNAM_WS_ROAD_MAX,       &
                 CFTYP_WS_ROAD_MAX,XUNIF_WS_ROAD_MAX,DTH%XPAR_WS_ROAD_MAX(:))  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'IP_SEWER: Parameter for parasite infiltrations into sewer (-)','TWN',CFNAM_IP_SEWER,       &
                 CFTYP_IP_SEWER,XUNIF_IP_SEWER,DTH%XPAR_IP_SEWER(:))  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'CONNEX: Impervious surfaces fraction connexion rate to the sewer (-)','TWN',CFNAM_CONNEX,       &
                 CFTYP_CONNEX,XUNIF_CONNEX,DTH%XPAR_CONNEX(:))  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'INFIL_ROAD: Water infiltration through roads (kg/m2/s)','TWN',CFNAM_INFIL_ROAD,       &
                 CFTYP_INFIL_ROAD,XUNIF_INFIL_ROAD,DTH%XPAR_INFIL_ROAD(:))  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'URBDRAIN: Limitation fraction of urban deep drainage (0-1)','TWN',CFNAM_URBDRAIN,       &
                 CFTYP_URBDRAIN,XUNIF_URBDRAIN,DTH%XPAR_URBDRAIN(:))  
!

IF (LHOOK) CALL DR_HOOK('PGD_TEB_HYDRO_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB_HYDRO_PAR
