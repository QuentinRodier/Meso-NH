!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGET_WATER (PTT, PTS, PRHOA, PSFTH, PSFTQ,         &
                                           PDIR_SW, PSCA_SW, PLW,                &
                                           PDIR_ALB, PSCA_ALB, PEMIS, PTRAD,     &
                                           PSFZON, PSFMER,                       &
                                           PRN, PH, PLE, PLEI, PGFLUX,           &
                                           PSWD, PSWU, PSWBD, PSWBU, PLWD, PLWU, &
                                           PFMU, PFMV, PEVAP, PSUBL              )  
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
!                             Ts instead of Tsrad
!!------------------------------------------------------------------
!

!
!
USE MODD_CSTS,           ONLY : XSTEFAN, XLSTT, XLVTT, XCPD
!
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,               INTENT(IN) :: PTT       ! freezing temperature of water surface
REAL, DIMENSION(:), INTENT(IN) :: PTS       ! surface temperature (K)
REAL, DIMENSION(:), INTENT(IN) :: PRHOA     ! air density
REAL, DIMENSION(:), INTENT(IN) :: PSFTH     ! heat flux
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ     ! water flux
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                           !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN) :: PEMIS     ! emissivity                            (-)
REAL, DIMENSION(:), INTENT(IN) :: PSFZON    ! zonal friction
REAL, DIMENSION(:), INTENT(IN) :: PSFMER    ! meridional friction
!
REAL, DIMENSION(:), INTENT(OUT):: PRN       ! net radiation                         (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PH        ! sensible heat flux                    (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PLE       ! total latent heat flux                (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PLEI      ! sublimation latent heat flux          (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PGFLUX    ! storage flux                          (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PEVAP     ! total evaporation                     (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT):: PSUBL     ! sublimation                           (kg/m2/s)
!
REAL, DIMENSION(:,:), INTENT(OUT):: PSWBD  ! incoming short wave radiation by spectral band (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT):: PSWBU  ! upward  short wave radiation by spectral band (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PSWD     ! total incoming short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PSWU     ! total upward short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PLWD     ! Downward long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT):: PLWU     ! upward long wave radiation (W/m2)  
!
REAL, DIMENSION(:), INTENT(OUT):: PFMU     ! zonal friction
REAL, DIMENSION(:), INTENT(OUT):: PFMV     ! meridional friction
!
!*      0.2    declarations of local variables
!
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_WATER',0,ZHOOK_HANDLE)
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
PRN    =   PSWD(:) - PSWU(:) + PLWD(:) - PLWU(:)
!
!* sensible heat flux
!
PH     = PSFTH(:)
!
!* latent heat flux
!
WHERE (PTS<PTT  )
  PLE    = PSFTQ * XLSTT
  PLEI   = PSFTQ * XLSTT
  PEVAP  = PSFTQ
  PSUBL  = PSFTQ
ELSEWHERE
  PLE    = PSFTQ * XLVTT
  PLEI   = 0.0
  PEVAP  = PSFTQ
  PSUBL  = 0.0
END WHERE
!
!* storage flux
!
PGFLUX = PRN - PH - PLE
!
!* wind stress
!
PFMU = PSFZON
!
PFMV = PSFMER
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGET_WATER
