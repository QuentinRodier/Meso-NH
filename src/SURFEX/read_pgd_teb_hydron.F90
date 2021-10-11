!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_HYDRO_n (DTCO, U, GCP, THP, DTH, KDIM, PD_ROAD,  KTEB_SOIL, HPROGRAM)
!     ################################################
!
!!****  *READ_PGD_TEB_HYDRO_n* - reads TEB HYDRO physiographic fields
!!                        
!!
!!    PURPOSE
!!    -------
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
!!      K.Chancibault/A.Lemonsu 01/2016   *Meteo France, IFSTTAR
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      01/2016
!!      C. de Munck   07/2020  Change to use of sewer length densities
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_TEB_HYDRO_n, ONLY : DATA_TEB_HYDRO_t
USE MODD_TEB_HYDRO_PGD_n,  ONLY : TEB_HYDRO_PGD_t
!
USE MODI_GET_LUOUT
USE MODI_SET_SURFEX_FILEIN
USE MODI_INIT_IO_SURF_n
USE MODI_READ_SURF
USE MODI_END_IO_SURF_n
USE MODI_READ_PGD_TEB_HYDRO_PAR_n
USE MODI_ABOR1_SFX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
!
TYPE(DATA_COVER_t),    INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(TEB_HYDRO_PGD_t), INTENT(INOUT) :: THP
TYPE(DATA_TEB_HYDRO_t),INTENT(INOUT) :: DTH
INTEGER,               INTENT(IN)    :: KDIM
REAL, DIMENSION(:,:),  INTENT(IN)    :: PD_ROAD     ! soil layer thickness (m)
INTEGER,               INTENT(IN)    :: KTEB_SOIL
!
 CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
INTEGER                       :: ILUOUT         ! output listing logical unit
INTEGER                       :: IRESP          ! IRESP  : return-code if a problem appears
INTEGER                       :: JI
 CHARACTER(LEN=12)            :: YRECFM         ! Name of the article to be read
REAL                          :: ZDEPTH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.    Reading of PGD file
!              --------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_HYDRO_N',0,ZHOOK_HANDLE)
!
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
! Not yet possible to initialize these quantities from ecoclimap or ecoclimapSG
!-------------------------------------------------------------------------------
! So the user must have specified the values of the parameters, and one read them here.
!
ALLOCATE(THP%XDENS_WASTE(KDIM)) 
ALLOCATE(THP%XDENS_STORM(KDIM)) 
ALLOCATE(THP%XDSEWER(KDIM))     
ALLOCATE(THP%XWS_ROOF_MAX(KDIM)) 
ALLOCATE(THP%XWS_ROAD_MAX(KDIM)) 
ALLOCATE(THP%XIP_SEWER(KDIM))     
ALLOCATE(THP%XCONNEX(KDIM)) 
ALLOCATE(THP%XINFIL_ROAD(KDIM)) 
ALLOCATE(THP%XURBDRAIN(KDIM))     
!
! Reading flag for user specified urban hydrology 
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ') ! change input file name to pgd name
 CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'TOWN  ','TEB   ','READ ')
 !
 YRECFM='LPAR_HYDRO'    
 CALL READ_SURF(HPROGRAM,YRECFM,DTH%LDATA_TEB_HYDRO,IRESP)
!
IF (DTH%LDATA_TEB_HYDRO) THEN
  CALL READ_PGD_TEB_HYDRO_PAR_n(DTCO, U, GCP, DTH, KDIM, HPROGRAM,'-')
  !
  THP%XDENS_WASTE  = DTH%XPAR_DENS_WASTE
  THP%XDENS_STORM  = DTH%XPAR_DENS_STORM 
  THP%XDSEWER      = DTH%XPAR_DSEWER
  THP%XWS_ROOF_MAX = DTH%XPAR_WS_ROOF_MAX 
  THP%XWS_ROAD_MAX = DTH%XPAR_WS_ROAD_MAX
  THP%XIP_SEWER    = DTH%XPAR_IP_SEWER
  THP%XCONNEX      = DTH%XPAR_CONNEX 
  THP%XINFIL_ROAD  = DTH%XPAR_INFIL_ROAD
  THP%XURBDRAIN    = DTH%XPAR_URBDRAIN
  !
END IF
!
 CALL END_IO_SURF_n(HPROGRAM)
!-------------------------------------------------------------------------------
!  Secondary PGD hydro parameters
!-------------------------------------------------------------------------------
!
!*   2.1     Allocations
!
ALLOCATE(THP%NLAYER_SEWER(KDIM))
!
THP%NLAYER_SEWER(:)   = 1
!
!*   2.2      Sewer layer
!
DO JI=1,KDIM
  ZDEPTH    = PD_ROAD(JI,THP%NLAYER_SEWER(JI))
  DO WHILE (ZDEPTH.LT.THP%XDSEWER(JI).AND.(THP%NLAYER_SEWER(JI).LT.KTEB_SOIL))
    THP%NLAYER_SEWER(JI) = THP%NLAYER_SEWER(JI) + 1
    ZDEPTH    = ZDEPTH + PD_ROAD(JI,THP%NLAYER_SEWER(JI))
  ENDDO
ENDDO
!
WHERE (THP%XDENS_WASTE==0.0 .AND. THP%XDENS_STORM==0.0) 
  THP%NLAYER_SEWER=0
ENDWHERE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_HYDRO_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_HYDRO_n
