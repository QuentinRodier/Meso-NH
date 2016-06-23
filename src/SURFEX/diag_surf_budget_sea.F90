!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE  DIAG_SURF_BUDGET_SEA(PTT , PSST, PRHOA, PSFTH, PSFTH_ICE,    &
                                 PSFTQ, PSFTQ_ICE,                       &
                                 PDIR_SW, PSCA_SW, PLW,                  &
                                 PDIR_ALB,PSCA_ALB,PALB_ICE,PEMIS, PTRAD,&
                                 PSFZON, PSFZON_ICE, PSFMER, PSFMER_ICE, &
                                 OHANDLE_SIC, PSIC, PTICE,               &
                                 PRN, PH, PLE, PLE_ICE, PGFLUX,             &
                                 PSWD, PSWU, PSWBD, PSWBU, PLWD, PLWU,   &
                                 PFMU, PFMV, PEVAP, PSUBL,               &
                                 PRN_ICE, PH_ICE, PGFLUX_ICE,            &
                                 PSWU_ICE, PSWBU_ICE, PLWU_ICE,          &
                                 PFMU_ICE, PFMV_ICE                      ) 


!     ###############################################################################
!
!!****  *DIAG_SURF_BUDGET_WATER * - Computes diagnostics over water
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!       S.Senesi    01/2014 : Handle fluxes on seaice
!!------------------------------------------------------------------
!

!
!
USE MODD_CSTS,           ONLY : XSTEFAN, XLSTT, XLVTT
USE MODD_WATER_PAR,      ONLY : XEMISWATICE, XALBSEAICE
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,               INTENT(IN) :: PTT       ! freezing temperature of water surface
REAL, DIMENSION(:), INTENT(IN) :: PSST      ! sea surface temperature (K)
REAL, DIMENSION(:), INTENT(IN) :: PRHOA     ! air density
REAL, DIMENSION(:), INTENT(IN) :: PSFTH     ! heat flux
REAL, DIMENSION(:), INTENT(IN) :: PSFTH_ICE ! heat flux on seaice
!
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ     ! water flux
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ_ICE ! water flux on seaice
!
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
!
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:)  ,INTENT(IN):: PALB_ICE  ! Seaice albedo 
REAL, DIMENSION(:), INTENT(IN) :: PEMIS     ! emissivity                            (-)
REAL, DIMENSION(:), INTENT(IN) :: PTRAD     ! radiative temperature                 (K)
!
REAL, DIMENSION(:), INTENT(IN) :: PSFZON    ! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFZON_ICE! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER    ! meridional friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER_ICE! meridional friction
!
LOGICAL, INTENT(IN)         :: OHANDLE_SIC  ! Do we weight seaice and open sea fluxes
REAL, DIMENSION(:), INTENT(IN) :: PSIC      ! Sea ice cover                         (-)
REAL, DIMENSION(:), INTENT(IN) :: PTICE     ! Sea ice temperature                   (°K)
!
REAL, DIMENSION(:), INTENT(OUT):: PRN       ! net radiation                         (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PH        ! sensible heat flux                    (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PLE       ! total latent heat flux                (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PLE_ICE      ! sublimation latent heat flux          (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PGFLUX    ! storage flux                          (W/m2)
!
REAL, DIMENSION(:),  INTENT(OUT):: PSWD     ! total incoming short wave radiation   (W/m2)
REAL, DIMENSION(:),  INTENT(OUT):: PSWU     ! total upward short wave radiation     (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT):: PSWBD   ! incoming short wave radiation by spectral band (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT):: PSWBU   ! upward  short wave radiation by spectral band (W/m2)
REAL, DIMENSION(:),  INTENT(OUT):: PLWD     ! Downward long wave radiation          (W/m2)
REAL, DIMENSION(:),  INTENT(OUT):: PLWU     ! Upward long wave radiation            (W/m2)
!
REAL, DIMENSION(:), INTENT(OUT):: PFMU      ! zonal wind stress
REAL, DIMENSION(:), INTENT(OUT):: PFMV      ! meridian wind stress
REAL, DIMENSION(:), INTENT(OUT):: PEVAP     ! total evaporation                     (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT):: PSUBL     ! sublimation                           (kg/m2/s)
! Fluxes on seaice
REAL, DIMENSION(:), INTENT(OUT):: PRN_ICE   ! net radiation                         (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PH_ICE    ! sensible heat flux                    (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PGFLUX_ICE! storage flux                          (W/m2)
! Continued
REAL, DIMENSION(:),  INTENT(OUT):: PSWU_ICE ! total upward short wave radiation (W/m2)
REAL, DIMENSION(:,:),INTENT(OUT):: PSWBU_ICE! upward  short wave radiation by spectral band (W/m2)
REAL, DIMENSION(:),  INTENT(OUT):: PLWU_ICE ! upward long wave radiation (W/m2)
REAL, DIMENSION(:),  INTENT(OUT):: PFMU_ICE ! zonal wind stress on sea-ice
REAL, DIMENSION(:),  INTENT(OUT):: PFMV_ICE ! meridian wind stress on sea-ice

!*      0.2    declarations of local variables
!
INTEGER                      :: I
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_SEA',0,ZHOOK_HANDLE)
!
ISWB = SIZE(PDIR_SW,2)
! 
!* total incoming and outgoing SW
!
DO JSWB=1,ISWB
  PSWBD(:,JSWB) = PDIR_SW(:,JSWB)                    + PSCA_SW(:,JSWB)
  PSWBU(:,JSWB) = PDIR_SW(:,JSWB) * PDIR_ALB(:,JSWB) + PSCA_SW(:,JSWB) * PSCA_ALB(:,JSWB) 
ENDDO
!
PSWD(:) = 0.
PSWU(:) = 0.
DO JSWB=1,ISWB
   PSWD(:)=PSWD(:)+PSWBD(:,JSWB)
   PSWU(:)=PSWU(:)+PSWBU(:,JSWB)
ENDDO
!
!*incoming outgoing LW
!
PLWD(:)=PLW(:)
PLWU(:)=PEMIS(:)*XSTEFAN*PTRAD(:)**4 + (1.-PEMIS(:))*PLW(:)
!
!* net radiation
!
PRN(:)    =   PSWD(:) - PSWU(:)     + PLWD(:) - PLWU    (:)
!
IF (.NOT.OHANDLE_SIC) THEN
  !
  !* sensible heat flux
  !
  PH     = PSFTH
  !
  !* latent heat flux
  !
  WHERE (PSST<PTT  )
     PLE    = PSFTQ * XLSTT
     PLE_ICE= PSFTQ * XLSTT
     PEVAP  = PSFTQ
     PSUBL  = PSFTQ
  ELSEWHERE
     PLE    = PSFTQ * XLVTT
     PLE_ICE= 0.0
     PEVAP  = PSFTQ
     PSUBL  = 0.0
  END WHERE
  !
  !* wind stress
  !
  PFMU = PSFZON
  PFMV = PSFMER
!
ELSE
  !
  !---------------------------------------------------------------------------- 
  ! Sea ice or mixed diag
  !---------------------------------------------------------------------------- 
  !
  !
  !* total incoming and outgoing SW
  !
  DO JSWB=1,ISWB
   PSWBU_ICE(:,JSWB) = (PDIR_SW(:,JSWB) + PSCA_SW(:,JSWB)) * PALB_ICE(:) 
  ENDDO
  !
  PSWU_ICE(:) = 0.
  DO JSWB=1,ISWB
     PSWU_ICE(:)=PSWU_ICE(:)+PSWBU_ICE(:,JSWB)
  ENDDO
  !
  !*incoming outgoing LW
  !
  PLWU_ICE(:)=XEMISWATICE*XSTEFAN*PTICE(:)**4 + (1.-XEMISWATICE)*PLW(:)
  !
  !* net radiation
  !
  PRN_ICE(:) =   PSWD(:) - PSWU_ICE(:) + PLWD(:) - PLWU_ICE(:)
  !
  !* sensible heat flux
  !
  PH     = (1 - PSIC) * PSFTH         + PSIC * PSFTH_ICE 
  PH_ICE =                                     PSFTH_ICE
  !
  !* latent heat flux
  !
  PLE     = (1 - PSIC) * PSFTQ * XLVTT + PSIC * PSFTQ_ICE * XLSTT
  PLE_ICE =                                     PSFTQ_ICE * XLSTT
  PEVAP   = (1 - PSIC) * PSFTQ         + PSIC * PSFTQ_ICE 
  PSUBL   =                              PSIC * PSFTQ_ICE 
  !
  !* ice storage flux
  !
   PGFLUX_ICE = PRN_ICE - PH_ICE - PLE_ICE
  !
  !* wind stress
  !
  PFMU = (1 - PSIC) * PSFZON + PSIC * PSFZON_ICE
  PFMU_ICE =                          PSFZON_ICE
  PFMV = (1 - PSIC) * PSFMER + PSIC * PSFMER_ICE
  PFMV_ICE =                          PSFMER_ICE
!  
ENDIF
!
!* total storage flux
!
PGFLUX = PRN - PH - PLE
!
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGET_SEA
