!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE UPDATE_ESM_SURF_ATM_n(HPROGRAM, KI, KSW, PZENITH, PSW_BANDS,   &
                                   PTRAD, PDIR_ALB, PSCA_ALB, PEMIS         )  
!     #################################################################################
!
!!****  *UPDATE_ESM_SURF_ATM_n * - Routine to update radiative properties in Earth
!!                                 System Model (SEA, WATER, NATURE, TOWN) after
!!                                 the call to OASIS coupler in order to close the
!!                                 energy budget between radiative scheme and surfex
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!-------------------------------------------------------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SURF_ATM_n,     ONLY : NSIZE_SEA, NSIZE_WATER, NSIZE_TOWN, NSIZE_NATURE, &
                                  NR_SEA,    NR_WATER,    NR_TOWN,    NR_NATURE,    &
                                  XSEA,      XWATER,      XTOWN,      XNATURE,      &
                                  CSEA,      CWATER,      CTOWN,      CNATURE  
!
USE MODD_ISBA_n,         ONLY : LFLOOD
!
USE MODD_DATA_COVER_PAR, ONLY : NTILESFC
!
USE MODI_AVERAGE_RAD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_UPDATE_ESM_ISBA_n
!
USE MODI_UPDATE_ESM_SEAFLUX_n
!
USE MODI_UPDATE_ESM_WATFLUX_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),       INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER,                INTENT(IN)  :: KI        ! number of points
INTEGER,                INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL, DIMENSION(KI),     INTENT(IN) :: PZENITH   ! zenithal angle       (radian from the vertical)
REAL, DIMENSION(KSW),    INTENT(IN) :: PSW_BANDS ! mean wavelength of each shortwave band (m)
!
REAL, DIMENSION(KI),    INTENT(OUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PEMIS     ! emissivity                            (-)
!
!*      0.2    declarations of local variables
!
INTEGER :: JTILE                        ! loop on type of surface
LOGICAL :: GNATURE, GTOWN, GWATER, GSEA ! .T. if the corresponding surface is represented
!
! Tile outputs:
!
REAL, DIMENSION(KI,NTILESFC) :: ZTRAD_TILE     ! radiative surface temperature
REAL, DIMENSION(KI,NTILESFC) :: ZEMIS_TILE     ! emissivity
REAL, DIMENSION(KI,NTILESFC) :: ZFRAC_TILE     ! fraction of each surface type
!
REAL, DIMENSION(KI,KSW,NTILESFC) :: ZDIR_ALB_TILE ! direct albedo
REAL, DIMENSION(KI,KSW,NTILESFC) :: ZSCA_ALB_TILE ! diffuse albedo
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
! Preliminaries: Tile related operations
!-------------------------------------------------------------------------------------
! FLAGS for the various surfaces:
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_SURF_ATM_N',0,ZHOOK_HANDLE)
GSEA      = (NSIZE_SEA    >0 .AND. CSEA/='NONE')
GWATER    = (NSIZE_WATER  >0 .AND. CWATER/='NONE')
GNATURE   = (NSIZE_NATURE >0 .AND. CNATURE/='NONE')
!
GTOWN     = NSIZE_TOWN   >0
IF(GTOWN)THEN
  CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: TOWN SCHEME NOT YET AVAILABLE FOR EARTH SYSTEM MODEL')
ENDIF
!
! Tile counter:
!
JTILE     = 0 
!
! Initialization: Outputs to atmosphere over each tile:
!
ZTRAD_TILE(:,:)       = XUNDEF
ZDIR_ALB_TILE(:,:,:)  = XUNDEF
ZSCA_ALB_TILE(:,:,:)  = XUNDEF
ZEMIS_TILE(:,:)       = XUNDEF
!
! Fractions for each tile:
!
ZFRAC_TILE(:,:)    = 0.0
!
!--------------------------------------------------------------------------------------
! Call arrange interfaces for sea, water, nature and town here...
!--------------------------------------------------------------------------------------
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! SEA Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GSEA)THEN
!
   ZFRAC_TILE(:,JTILE) = XSEA(:)
!
   CALL TREAT_SURF(NSIZE_SEA,NR_SEA,JTILE)   ! pack variables which are arguments to this routine
!
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! INLAND WATER Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GWATER)THEN
!
   ZFRAC_TILE(:,JTILE) = XWATER(:)
!
   CALL TREAT_SURF(NSIZE_WATER,NR_WATER,JTILE)  
!
ENDIF 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! NATURAL SURFACE Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
JTILE = JTILE + 1
!
IF(GNATURE)THEN
!
   ZFRAC_TILE(:,JTILE) = XNATURE(:)
!
   CALL TREAT_SURF(NSIZE_NATURE,NR_NATURE,JTILE)
!   
ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! URBAN Tile calculations:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Not yet implemented
!
!JTILE = JTILE + 1
!
!IF(GTOWN)THEN
!
!   ZFRAC_TILE(:,JTILE) = XTOWN(:)
!
!   CALL TREAT_SURF(NSIZE_TOWN,NR_TOWN,JTILE)  
!
!ENDIF 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Grid box average radiative properties:
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
 CALL AVERAGE_RAD(ZFRAC_TILE,                                           &
                   ZDIR_ALB_TILE, ZSCA_ALB_TILE, ZEMIS_TILE, ZTRAD_TILE, &
                   PDIR_ALB,      PSCA_ALB,      PEMIS,      PTRAD       )  
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_SURF_ATM_N',1,ZHOOK_HANDLE)
CONTAINS
!=======================================================================================
SUBROUTINE TREAT_SURF(KSIZE,KMASK,KTILE)
!
INTEGER, INTENT(IN)               :: KSIZE
INTEGER, INTENT(IN), DIMENSION(:) :: KMASK
INTEGER, INTENT(IN)               :: KTILE
!
REAL, DIMENSION(KSIZE) :: ZP_ZENITH   ! zenithal angle       (radian from the vertical)
!
REAL, DIMENSION(KSIZE)     :: ZP_TRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KSIZE,KSW) :: ZP_DIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(KSIZE,KSW) :: ZP_SCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KSIZE)     :: ZP_EMIS     ! emissivity
!
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! input arguments:
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_SURF_ATM_N:TREAT_SURF',0,ZHOOK_HANDLE)
!
ZP_TRAD    = XUNDEF
ZP_DIR_ALB = XUNDEF
ZP_SCA_ALB = XUNDEF
ZP_EMIS    = XUNDEF
!
DO JJ=1,KSIZE
  ZP_ZENITH(JJ)     = PZENITH     (KMASK(JJ))
ENDDO
!
!
IF (KTILE==1) THEN
  !
  IF (CSEA=='SEAFLX') THEN
    CALL UPDATE_ESM_SEAFLUX_n(HPROGRAM,NSIZE_SEA,KSW,ZP_ZENITH,ZP_DIR_ALB,ZP_SCA_ALB,ZP_EMIS,ZP_TRAD)
  ELSE
    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: SEA SCHEME MUST BE ACTIVATED FOR EARTH SYSTEM MODEL')
  ENDIF
  !
ELSEIF (KTILE==2) THEN
  !
  IF (CWATER=='WATFLX') THEN   
    CALL UPDATE_ESM_WATFLUX_n(HPROGRAM,NSIZE_WATER,KSW,ZP_ZENITH,ZP_DIR_ALB,ZP_SCA_ALB,ZP_EMIS,ZP_TRAD)
  ELSEIF (CWATER=='FLAKE ') THEN
    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: FLAKE SCHEME NOT YET AVAILABLE FOR EARTH SYSTEM MODEL')
  ELSE
    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: SEA SCHEME MUST BE ACTIVATED FOR EARTH SYSTEM MODEL')
  ENDIF
  !
ELSEIF (KTILE==3) THEN
  !          
  IF (CNATURE=='ISBA') THEN   
    CALL UPDATE_ESM_ISBA_n(HPROGRAM,NSIZE_NATURE,KSW,ZP_ZENITH,PSW_BANDS,ZP_DIR_ALB,ZP_SCA_ALB,ZP_EMIS,ZP_TRAD)
  ELSE
    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: NATURE SCHEME MUST BE ACTIVATED FOR EARTH SYSTEM MODEL')
  ENDIF
  !
!ELSEIF (KTILE==4) THEN
!  !
!  IF (CTOWN=='TEB   ') THEN   
!    CALL UPDATE_ESM_TEB_n(HPROGRAM,NSIZE_SEA,KSW,ZP_ZENITH,ZP_TRAD,ZP_DIR_ALB,ZP_SCA_ALB,ZP_EMIS)
!  ELSE
!    CALL ABOR1_SFX('UPDATE_ESM_SURF_ATM_n: TEB SCHEME MUST BE ACTIVATED FOR EARTH SYSTEM MODEL')
!  ENDIF
!  !        
ENDIF
!
DO JJ=1,KSIZE
   ZTRAD_TILE      (KMASK(JJ),KTILE)  = ZP_TRAD      (JJ)
   ZDIR_ALB_TILE   (KMASK(JJ),:,KTILE)= ZP_DIR_ALB   (JJ,:)
   ZSCA_ALB_TILE   (KMASK(JJ),:,KTILE)= ZP_SCA_ALB   (JJ,:)
   ZEMIS_TILE      (KMASK(JJ),KTILE)  = ZP_EMIS      (JJ)
ENDDO
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_SURF_ATM_N:TREAT_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_SURF
!=======================================================================================
!
END SUBROUTINE UPDATE_ESM_SURF_ATM_n


