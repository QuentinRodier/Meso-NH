!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE  DIAG_SURF_BUDGET_SEA(D, DI, S, PTT, PRHOA, PSFTH, PSFTH_ICE,  &
                                 PSFTQ, PSFTQ_ICE, PDIR_SW, PSCA_SW, PLW, &
                                 PDIR_ALB, PSCA_ALB, PEMIS, PTRAD,        &
                                 PSFZON, PSFZON_ICE, PSFMER, PSFMER_ICE   ) 


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
!!      A.Voldoire  04/2018 : GELATO1D coupling and in regional model
!!------------------------------------------------------------------
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
USE MODD_DIAG_n, ONLY : DIAG_t
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
TYPE(DIAG_t),    INTENT(INOUT) :: D
TYPE(DIAG_t),    INTENT(INOUT) :: DI
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
REAL,               INTENT(IN) :: PTT       ! freezing temperature of water surface
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
REAL, DIMENSION(:), INTENT(IN) :: PEMIS     ! emissivity                            (-)
REAL, DIMENSION(:), INTENT(IN) :: PTRAD     ! radiative temperature                 (K)
!
REAL, DIMENSION(:), INTENT(IN) :: PSFZON    ! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFZON_ICE! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER    ! meridional friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER_ICE! meridional friction
!
!*      0.2    declarations of local variables
!
INTEGER                      :: JI, INI
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_SEA',0,ZHOOK_HANDLE)
!
INI  = SIZE(PDIR_SW,1)
ISWB = SIZE(PDIR_SW,2)
! 
!* total incoming and outgoing SW
!
DO JSWB=1,ISWB
   DO JI=1,INI
      D%XSWBD(JI,JSWB) = PDIR_SW(JI,JSWB)                     + PSCA_SW(JI,JSWB)
      D%XSWBU(JI,JSWB) = PDIR_SW(JI,JSWB) * PDIR_ALB(JI,JSWB) + PSCA_SW(JI,JSWB) * PSCA_ALB(JI,JSWB) 
   ENDDO
ENDDO
!
D%XSWD(:) = 0.
D%XSWU(:) = 0.
DO JSWB=1,ISWB
   DO JI=1,INI
      D%XSWD(JI) = D%XSWD(JI) + D%XSWBD(JI,JSWB)
      D%XSWU(JI) = D%XSWU(JI) + D%XSWBU(JI,JSWB)
   ENDDO
ENDDO
!
!*incoming outgoing LW
!
D%XLWD(:)=PLW(:)
D%XLWU(:)=PEMIS(:)*XSTEFAN*PTRAD(:)**4 + (1.-PEMIS(:))*PLW(:)
!
!* net radiation
!
D%XRN(:)    =   D%XSWD(:) - D%XSWU(:)     + D%XLWD(:) - D%XLWU    (:)
!
IF (.NOT.S%LHANDLE_SIC) THEN
  !
  !* heat flux
  !
  WHERE (S%XSST(:)<PTT  )
     D%XH    (:) = PSFTH(:)
     DI%XH   (:) = PSFTH(:)
     D%XLE   (:) = PSFTQ(:) * XLSTT
     D%XLEI  (:) = PSFTQ(:) * XLSTT
     DI%XLE  (:) = PSFTQ(:) * XLSTT
     DI%XLEI (:) = PSFTQ(:) * XLSTT
     D%XEVAP (:) = PSFTQ(:)
     DI%XEVAP(:) = PSFTQ(:)
     D%XSUBL (:) = PSFTQ(:)
     DI%XSUBL(:) = PSFTQ(:)
  ELSEWHERE
     D%XH    (:) = PSFTH(:)
     DI%XH   (:) = 0.0
     D%XLE   (:) = PSFTQ(:) * XLVTT
     D%XLEI  (:) = 0.0
     DI%XLE  (:) = 0.0
     DI%XLEI (:) = 0.0
     D%XEVAP (:) = PSFTQ(:)
     DI%XEVAP(:) = 0.0
     D%XSUBL (:) = 0.0
     DI%XSUBL(:) = 0.0
  END WHERE
  !
  !* wind stress
  !
  D%XFMU(:) = PSFZON(:)
  D%XFMV(:) = PSFMER(:)
  !
ELSE
  !
  !---------------------------------------------------------------------------- 
  ! Global mixed diag
  !---------------------------------------------------------------------------- 
  !
  !* sensible heat flux
  !
  D%XH(:) = (1.0-S%XSIC(:)) * PSFTH(:) + S%XSIC(:) * PSFTH_ICE(:) 
  !
  !* latent and sublimation heat fluxes
  !
  D%XLE (:) = (1.0-S%XSIC(:)) * PSFTQ(:) * XLVTT + S%XSIC(:) * PSFTQ_ICE(:) * XLSTT
  D%XLEI(:) =                                      S%XSIC(:) * PSFTQ_ICE(:) * XLSTT
  !
  !* evaporation and sublimation flux
  !
  D%XEVAP(:) = (1.0-S%XSIC(:)) * PSFTQ(:) + S%XSIC(:) * PSFTQ_ICE(:) 
  D%XSUBL(:) =                              S%XSIC(:) * PSFTQ_ICE(:) 
  !
  !* wind stress
  !
  D%XFMU(:) = (1.0-S%XSIC(:)) * PSFZON(:) + S%XSIC(:) * PSFZON_ICE(:)
  D%XFMV(:) = (1.0-S%XSIC(:)) * PSFMER(:) + S%XSIC(:) * PSFMER_ICE(:)
  !
  !---------------------------------------------------------------------------- 
  ! Sea ice diag (only)
  !---------------------------------------------------------------------------- 
  !
  !* incoming and outgoing SW
  !
  DO JSWB=1,ISWB
     DO JI=1,INI
        DI%XSWBU(JI,JSWB) = (PDIR_SW(JI,JSWB) + PSCA_SW(JI,JSWB)) * S%XICE_ALB(JI) 
     ENDDO
  ENDDO
  !
  DI%XSWU(:) = 0.
  DO JSWB=1,ISWB
     DO JI=1,INI
        DI%XSWU(JI) = DI%XSWU(JI) + DI%XSWBU(JI,JSWB)
     ENDDO
  ENDDO
  !
  !*incoming outgoing LW
  !
  DI%XLWU(:)=XEMISWATICE*XSTEFAN*S%XTICE(:)**4 + (1.-XEMISWATICE)*PLW(:)
  !
  !* net radiation
  !
  DI%XRN(:) =   D%XSWD(:) - DI%XSWU(:) + D%XLWD(:) - DI%XLWU(:)
  !
  !* sensible heat flux
  !
  DI%XH(:) = PSFTH_ICE(:)
  !
  !* latent heat flux
  !
  DI%XLE (:) = PSFTQ_ICE(:) * XLSTT
  DI%XLEI(:) = PSFTQ_ICE(:) * XLSTT
  !
  !* evaporation & sublimation fluxes
  !
  DI%XEVAP(:) = PSFTQ_ICE(:)
  DI%XSUBL(:) = PSFTQ_ICE(:) 
  !
  !* ice storage flux
  !
  DI%XGFLUX(:) = DI%XRN(:) - DI%XH(:) - DI%XLE(:)
  !
  !* wind stress
  !
  DI%XFMU(:) = PSFZON_ICE(:)
  DI%XFMV(:) = PSFMER_ICE(:)
  !
ENDIF
!
!* Global mixed total storage flux
!
D%XGFLUX = D%XRN - D%XH - D%XLE
!
!* Anthopogenic flux
!
D%XQF = 0.
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGET_SEA
