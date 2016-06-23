!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_SEA(HALB,PSST,PZENITH,PTT,PEMIS,PDIR_ALB,PSCA_ALB,  &
                          PDIR_ALB_ATMOS,PSCA_ALB_ATMOS,PEMIS_ATMOS,PTRAD,&
                          OHANDLE_SIC,PTICE,PSIC,PICE_ALB,PU,PV           )  
!     #######################################################################
!
!!****  *UPDATE_RAD_SEA * - update the radiative properties at time t+1 (see by the atmosphere) 
!                           in order to close the energy budget between surfex and the atmosphere
 
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
!!      Modified    03/2011 : E. Bazile (MK10) albedo from Marat Khairoutdinov
!!      Modified    01/2014 : S. Senesi : handle fractional seaice
!!      Modified    02/2014 : split from update_rad_seawat.F90
!!      Modified    01/2015 : introduce interactive ocean surface albedo (R.Séférian)
!!------------------------------------------------------------------
!
USE MODD_WATER_PAR, ONLY : XEMISWAT, XEMISWATICE, &
                           XALBWAT, XALBSCA_WAT,  &
                           XALBSEAICE
!
USE MODD_SFX_OASIS, ONLY : LCPL_SEA
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_MK10
USE MODI_ALBEDO_RS14
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=4),       INTENT(IN)  :: HALB
!
REAL, DIMENSION(:),     INTENT(IN)   :: PSST      ! Sea surface temperature
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL,                   INTENT(IN)   :: PTT       ! Sea/ice transition temperature (different according to sea or inland water)
!
REAL, DIMENSION(:),     INTENT(INOUT):: PDIR_ALB  ! Direct albedo at t+1 for the mix (open sea, seaice)
REAL, DIMENSION(:),     INTENT(INOUT):: PSCA_ALB  ! Diffuse albedo at t+1 for the mix (open sea, seaice)
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS     ! emissivity (sea water + sea ice) at t+1
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB_ATMOS ! Direct albedo at t+1 for the atmosphere
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB_ATMOS ! Diffuse albedo at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS_ATMOS    ! Emissivity at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PTRAD          ! radiative temp at t+1 for the atmosphere
!
LOGICAL,                INTENT(IN)   , OPTIONAL :: OHANDLE_SIC ! Do we weight seaice and open sea fluxes
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PTICE     ! Seaice surface temperature
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PSIC      ! Seaice cover
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PICE_ALB  ! Seaice albedo
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PU        ! zonal wind (m/s)
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PV        ! meridian wind (m/s)
!
!*      0.2    declarations of local variables
!
INTEGER :: JSWB
REAL, DIMENSION(SIZE(PZENITH)) :: ZALBDIR
REAL, DIMENSION(SIZE(PZENITH)) :: ZALBSCA
REAL, DIMENSION(SIZE(PZENITH)) :: ZWIND
LOGICAL :: GHANDLE_SIC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_SEA',0,ZHOOK_HANDLE)
!
ZALBDIR(:) = 0.
ZALBSCA(:) = 0.
!
IF (HALB=='TA96') THEN
!        
  ZALBDIR(:) = ALBEDO_TA96(PZENITH(:))
  ZALBSCA(:) = XALBSCA_WAT
!  
ELSEIF (HALB=='MK10') THEN
!        
  ZALBDIR(:) = ALBEDO_MK10(PZENITH(:))
  ZALBSCA(:) = XALBSCA_WAT
!  
ELSEIF (HALB=='RS14') THEN
!        
  IF (PRESENT(PU).AND.PRESENT(PV)) THEN
     ZWIND(:) = SQRT(PU(:)**2+PV(:)**2)
     CALL ALBEDO_RS14(PZENITH(:),ZWIND(:),ZALBDIR(:),ZALBSCA(:))
  ELSE
     ZALBDIR(:) = PDIR_ALB(:)
     ZALBSCA(:) = PSCA_ALB(:)
  ENDIF
!
ENDIF
!
IF (.NOT. PRESENT(OHANDLE_SIC)) THEN 
   GHANDLE_SIC=.FALSE.
ELSE 
   GHANDLE_SIC=OHANDLE_SIC
ENDIF
!
IF(LCPL_SEA)THEN !Earth System Model
!
!Sea and/or ice albedo already given by coupled seaice model
!Except for Taylor et al (1996) and MK10 formulation
!
  WHERE (PSST(:)>=PTT  )
    !* open water
    PEMIS   (:) = XEMISWAT
  ELSEWHERE
    !* sea ice
    PEMIS   (:) = XEMISWATICE
  END WHERE
  !
  IF (HALB=='TA96' .OR. HALB=='MK10' .OR. HALB=='RS14') THEN
    !* Taylor et al 1996
    !* open water
    WHERE (PSST(:)>=PTT) PDIR_ALB(:) = ZALBDIR(:)
    WHERE (PSST(:)>=PTT) PSCA_ALB(:) = ZALBSCA(:)
  ENDIF
  !
ELSEIF(GHANDLE_SIC) THEN 
  ! Returned values are an average of open sea and seaice properties
  ! weighted by the seaice cover
  PEMIS   (:) = ( 1 - PSIC(:)) * XEMISWAT    + PSIC(:) * XEMISWATICE
  IF (HALB=='UNIF') THEN
     PDIR_ALB(:) = ( 1 - PSIC(:)) * XALBWAT     + PSIC(:) * PICE_ALB(:)
     PSCA_ALB(:) = ( 1 - PSIC(:)) * XALBWAT     + PSIC(:) * PICE_ALB(:)
  ELSE IF (HALB=='TA96' .OR. HALB=='MK10' .OR. HALB=='RS14') THEN
     PDIR_ALB(:) = ( 1 - PSIC(:)) * ZALBDIR(:) + PSIC(:) * PICE_ALB(:)
     PSCA_ALB(:) = ( 1 - PSIC(:)) * ZALBSCA(:) + PSIC(:) * PICE_ALB(:)
  ENDIF
ELSE
  !
  IF (HALB=='UNIF') THEN
  !* uniform albedo
    WHERE (PSST(:)>=PTT  )
    !* open water
      PDIR_ALB  (:) = XALBWAT
      PSCA_ALB  (:) = XALBWAT
      PEMIS     (:) = XEMISWAT
    ELSEWHERE
    !* sea ice
      PDIR_ALB(:) = XALBSEAICE
      PSCA_ALB(:) = XALBSEAICE
      PEMIS   (:) = XEMISWATICE
    END WHERE
  !
  ELSE IF (HALB=='TA96' .OR. HALB=='MK10' .OR. HALB=='RS14') THEN
    !* Taylor et al 1996
    !
    WHERE (PSST(:)>=PTT)
    !* open water
      PDIR_ALB  (:) = ZALBDIR(:)
      PSCA_ALB  (:) = ZALBSCA(:)
      PEMIS     (:) = XEMISWAT
    ELSEWHERE
    !* sea ice
      PDIR_ALB(:) = XALBSEAICE
      PSCA_ALB(:) = XALBSEAICE
      PEMIS   (:) = XEMISWATICE
    END WHERE
    !
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------------
!
DO JSWB=1,SIZE(PDIR_ALB_ATMOS,2)
  PDIR_ALB_ATMOS(:,JSWB) = PDIR_ALB(:)
  PSCA_ALB_ATMOS(:,JSWB) = PSCA_ALB(:)
END DO
!
PEMIS_ATMOS(:) = PEMIS(:)
IF(GHANDLE_SIC) THEN 
   PTRAD(:) = (((1 - PSIC(:)) * XEMISWAT    * PSST (:)**4 + &
                     PSIC(:)  * XEMISWATICE * PTICE(:)**4)/ &
              PEMIS(:)) ** 0.25
ELSE
   PTRAD(:) = PSST (:)
END IF
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_RAD_SEA

