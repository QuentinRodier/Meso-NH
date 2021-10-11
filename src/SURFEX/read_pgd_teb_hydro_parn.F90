!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_HYDRO_PAR_n (DTCO, U, GCP, DTH, KDIM, HPROGRAM, HDIRIN)
!     ################################################
!
!!****  *READ_PGD_TEB_HYDRO_PAR_n* - reads TEB HYDRO physiographic fields
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
!!      Original    01/2016
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_DATA_COVER_n,     ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,       ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_DATA_TEB_HYDRO_n, ONLY : DATA_TEB_HYDRO_t
!
USE MODI_READ_SURF
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
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
TYPE(DATA_COVER_t),     INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t),       INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t), INTENT(INOUT) :: GCP
TYPE(DATA_TEB_HYDRO_t), INTENT(INOUT) :: DTH
INTEGER,                INTENT(IN)    :: KDIM
!
 CHARACTER(LEN=6),      INTENT(IN)    :: HPROGRAM ! program calling
 CHARACTER(LEN=1),      INTENT(IN)    :: HDIRIN
 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                               :: IRESP    ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12)                     :: YRECFM   ! Name of the article to be read
CHARACTER(LEN=100)                    :: YCOMMENT ! Comment string
!
INTEGER :: ILUOUT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.    Reading of PGD file
!              --------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_HYDRO_PAR_N',0,ZHOOK_HANDLE)
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!
! Read density of wastewater sewer length
YRECFM='D_DENS_WASTE'
ALLOCATE(DTH%XPAR_DENS_WASTE(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_DENS_WASTE,HDIRIN)
!
! Read density of stormwater sewer length
YRECFM='D_DENS_STORM'
ALLOCATE(DTH%XPAR_DENS_STORM(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_DENS_STORM,HDIRIN)
!
! Read depth of waste water sewer length
YRECFM='D_DSEWER'
ALLOCATE(DTH%XPAR_DSEWER(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_DSEWER,HDIRIN)
!
! Read max capacity of surface roof water storage
YRECFM='D_WS_RF_MAX'
ALLOCATE(DTH%XPAR_WS_ROOF_MAX(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_WS_ROOF_MAX,HDIRIN)
!
! Read max capacity of surface road water storage
YRECFM='D_WS_RD_MAX'
ALLOCATE(DTH%XPAR_WS_ROAD_MAX(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_WS_ROAD_MAX,HDIRIN)
!
! Read parameter for parasite infiltrations into sewer
YRECFM='D_IP_SEWER'
ALLOCATE(DTH%XPAR_IP_SEWER(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_IP_SEWER,HDIRIN)
!
! Read impervious surfaces connexion rate to sewer
YRECFM='D_CONNEX'
ALLOCATE(DTH%XPAR_CONNEX(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_CONNEX,HDIRIN)
!
! Read parameter for water infiltration through roads
YRECFM='D_INFIL_RD'
ALLOCATE(DTH%XPAR_INFIL_ROAD(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_INFIL_ROAD,HDIRIN)
!
! Read parameter for limitation of deep drainage
YRECFM='D_URBDRAIN'
ALLOCATE(DTH%XPAR_URBDRAIN(KDIM))
 CALL READ_FIELD(YRECFM,DTH%XPAR_URBDRAIN,HDIRIN)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_HYDRO_PAR_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE READ_FIELD(HRECFM,PFIELD,YDIR)
!
USE MODI_HOR_INTERPOL
!
IMPLICIT NONE
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HRECFM ! name of record in the file
REAL, DIMENSION(KDIM), INTENT(OUT) :: PFIELD
 CHARACTER(LEN=1),     INTENT(IN)  :: YDIR
!
REAL, DIMENSION(KDIM)             :: ZF
REAL, DIMENSION(:,:), POINTER     :: ZIN
REAL, DIMENSION(KDIM,1)           :: ZOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_HYDRO_PAR_N:READ_FIELD',0,ZHOOK_HANDLE)
!
IF (YDIR=='A') THEN
  !
  CALL READ_SURF(HPROGRAM,HRECFM,ZF,IRESP,HDIR='A')
  ALLOCATE(ZIN(KDIM,1))
  ZIN(:,1) = ZF(:)
  CALL HOR_INTERPOL(DTCO, U,GCP,ILUOUT,ZIN,ZOUT)
  DEALLOCATE(ZIN)
  PFIELD(:) = ZOUT(:,1)
  !
ELSE
  CALL READ_SURF(HPROGRAM,HRECFM,PFIELD,IRESP)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_HYDRO_PAR_N:READ_FIELD',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_FIELD
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_HYDRO_PAR_n
