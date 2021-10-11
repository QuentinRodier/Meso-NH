!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ####################
      MODULE MODD_SSO_n
!     ######################
!
!!****  *MODD_SSO - declaration of surface parameters related to orography
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
!
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
!!      V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
! utilisé uniquement tel quel. 
TYPE SSO_t
!
!-----------------------------------------------------------------------------------------------------
!
! Type of roughness
!
 CHARACTER(LEN=4) :: CROUGH     ! type of orographic roughness
!                              ! 'NONE'
                               ! 'Z01D'
                               ! 'Z04D'
                               ! 'BE04'
!
  REAL, DIMENSION(:), POINTER   :: XZ0EFFJPDIR    ! heading of J direction (deg from N clockwise)
!
  REAL, DIMENSION(:), POINTER   :: XSSO_SLOPE         ! slope of S.S.O.
  REAL, DIMENSION(:), POINTER   :: XSSO_ANIS          ! anisotropy of S.S.O.
  REAL, DIMENSION(:), POINTER   :: XSSO_DIR           ! direction of S.S.O. (deg from N clockwise) 
  REAL, DIMENSION(:), POINTER   :: XSSO_STDEV         ! S.S.O. standard deviation           (m)
!
  REAL, DIMENSION(:), POINTER   :: XAVG_ZS        ! averaged orography                      (m)
  REAL, DIMENSION(:), POINTER   :: XSIL_ZS        ! silhouette orography                    (m)
  REAL, DIMENSION(:), POINTER   :: XMAX_ZS        ! maximum subgrid orography               (m)
  REAL, DIMENSION(:), POINTER   :: XMIN_ZS        ! minimum subgrid orography               (m)
  REAL, DIMENSION(:), POINTER   :: XAVG_SLO       ! averaged slope (rad)
  REAL, DIMENSION(:), POINTER   :: XSLOPE         ! mesh-scale slope angle (rad)
  REAL, DIMENSION(:), POINTER   :: XASPECT        ! mesh-scale slope aspect (rad)
  REAL, DIMENSION(:,:), POINTER :: XSLOPE_DIR     ! averaged slope in each aspect section (direction) (rad)
  REAL, DIMENSION(:,:), POINTER :: XFRAC_DIR      ! fraction of subrid scale pixels in each aspect section
  REAL, DIMENSION(:), POINTER   :: XSVF           ! sky-view factor 
  REAL, DIMENSION(:,:), POINTER :: XHMINS_DIR   ! minimum local horizon angle (sinus) in each aspect section
  REAL, DIMENSION(:,:), POINTER :: XHMAXS_DIR   ! maximum local horizon angle (sinus) in each aspect section
  REAL, DIMENSION(:,:), POINTER :: XSHA_DIR       ! A factor for local horizon angle in each aspect section
  REAL, DIMENSION(:,:), POINTER :: XSHB_DIR       ! B factor for local horizon angle in each aspect section
  INTEGER                       :: NSECTORS       ! number of aspect sectors
  LOGICAL                       :: LDSV           ! switch for orographic shadowing, sky view factor
  LOGICAL                       :: LDSH           ! switch for orographic shadowing, shadow factor
  LOGICAL                       :: LDSL           ! switch for orographic shadowing, slope factor
! Zo threshold
  REAL   :: XFRACZ0                                ! Z0=Min(Z0, Href/XFRACZ0)
  REAL   :: XCOEFBE                                ! Beljaars coefficient         
!
!-----------------------------------------------------------------------------------------------------
!
! Subgrid orography parameters
!
  REAL, DIMENSION(:), POINTER :: XAOSIP, XAOSIM, XAOSJP, XAOSJM
! directional A/S quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
  REAL, DIMENSION(:), POINTER :: XHO2IP, XHO2IM, XHO2JP, XHO2JM
! directional h/2 quantities in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
! They are used in soil routines to compute effective roughness length
!
  REAL, DIMENSION(:), POINTER   :: XZ0REL         ! relief roughness length     (m)
!
! utilisé par l'atmosphère 
!
  REAL, DIMENSION(:), POINTER :: XZ0EFFIP, XZ0EFFIM, XZ0EFFJP, XZ0EFFJM
! directional total roughness lenghts in 4 coordinate directions
! (IP: i index up;  IM: i index down;  JP: j index up;  JM: j index down)
!
END TYPE SSO_t
!
! utilisé par isba
!
TYPE SSO_NP_t
!
TYPE(SSO_t), DIMENSION(:), POINTER :: AL=>NULL()
!
END TYPE SSO_NP_t
!
CONTAINS
!
SUBROUTINE SSO_INIT(YSSO)
TYPE(SSO_t), INTENT(INOUT) :: YSSO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_SSO_N:SSO_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YSSO%XZ0EFFJPDIR)  
  NULLIFY(YSSO%XSSO_SLOPE)
  NULLIFY(YSSO%XSSO_ANIS)
  NULLIFY(YSSO%XSSO_DIR)
  NULLIFY(YSSO%XSSO_STDEV)
  NULLIFY(YSSO%XAVG_ZS)
  NULLIFY(YSSO%XSIL_ZS)
  NULLIFY(YSSO%XMAX_ZS)
  NULLIFY(YSSO%XMIN_ZS)
  NULLIFY(YSSO%XAVG_SLO)
  NULLIFY(YSSO%XSLOPE)
  NULLIFY(YSSO%XASPECT)
  NULLIFY(YSSO%XSLOPE_DIR)
  NULLIFY(YSSO%XFRAC_DIR)
  NULLIFY(YSSO%XSVF)
  NULLIFY(YSSO%XHMINS_DIR)
  NULLIFY(YSSO%XHMAXS_DIR)
  NULLIFY(YSSO%XSHA_DIR)
  NULLIFY(YSSO%XSHB_DIR)
  NULLIFY(YSSO%XAOSIP)
  NULLIFY(YSSO%XAOSIM)
  NULLIFY(YSSO%XAOSJP)
  NULLIFY(YSSO%XAOSJM)
  NULLIFY(YSSO%XHO2IP)
  NULLIFY(YSSO%XHO2IM)
  NULLIFY(YSSO%XHO2JP)
  NULLIFY(YSSO%XHO2JM)
  NULLIFY(YSSO%XZ0REL)
  NULLIFY(YSSO%XZ0EFFIP)
  NULLIFY(YSSO%XZ0EFFIM)
  NULLIFY(YSSO%XZ0EFFJP)
  NULLIFY(YSSO%XZ0EFFJM)
  
  YSSO%CROUGH=' '
  YSSO%XFRACZ0=2.
  YSSO%XCOEFBE=2.
  YSSO%NSECTORS=0
  YSSO%LDSV=.FALSE.
  YSSO%LDSL=.FALSE.
  YSSO%LDSH=.FALSE.

  IF (LHOOK) CALL DR_HOOK("MODD_SSO_N:SSO_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE SSO_INIT
!
SUBROUTINE SSO_NP_INIT(YSSO_NP,KPATCH)
TYPE(SSO_NP_t), INTENT(INOUT) :: YSSO_NP
INTEGER, INTENT(IN) :: KPATCH
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_SSO_N:SSO_NP_INIT",0,ZHOOK_HANDLE)
!
IF (ASSOCIATED(YSSO_NP%AL)) THEN
  DO JP = 1,KPATCH
    CALL SSO_INIT(YSSO_NP%AL(JP))
  ENDDO        
  DEALLOCATE(YSSO_NP%AL)
ELSE
  ALLOCATE(YSSO_NP%AL(KPATCH))
  DO JP = 1,KPATCH
    CALL SSO_INIT(YSSO_NP%AL(JP))
  ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK("MODD_SSO_N:SSO_NP_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE SSO_NP_INIT
!
END MODULE MODD_SSO_n
