!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/11/23 17:22:54
!-----------------------------------------------------------------
!     ########################  
      MODULE MODN_PARAM_ECRAD_n
!     ########################
!
!-------------------------------------------------------------------------------
USE PARKIND1 , ONLY : JPIM,JPRB
#ifdef MNH_ECRAD
#if ( VER_ECRAD == 140 )
USE MODD_RADIATIONS_n , ONLY : NSWB_MNH, NLWB_MNH
#endif
#endif
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAM_ECRAD_n, ONLY: &
         NSWSOLVER_n => NSWSOLVER, &
         NLWSOLVER_n => NLWSOLVER, &
         NLIQOPT_n => NLIQOPT, &
         NICEOPT_n => NICEOPT , &
         NRADLP_n  => NRADLP , &
         NRADIP_n  => NRADIP , &
         NGAS_n => NGAS, &
         NOVLP_n => NOVLP, &
         NREG_n => NREG, &
         XCLOUD_FRAC_STD_n => XCLOUD_FRAC_STD, &
         NLWSCATTERING_n => NLWSCATTERING, &
         NAERMACC_n => NAERMACC        
#ifdef MNH_ECRAD
#if ( VER_ECRAD == 140 )
USE MODD_PARAM_ECRAD_n, ONLY: &
         LSPEC_ALB_n => LSPEC_ALB, &
         LSPEC_EMISS_n => LSPEC_EMISS, &
       !  USER_ALB_DIFF_n => USER_ALB_DIFF, &
       !  USER_ALB_DIR_n => USER_ALB_DIR, &
       !  USER_EMISS_n => USER_EMISS
         SURF_TYPE_n => SURF_TYPE
#endif
#endif
!          EFF3D_n => EFF3D, &
!          SIDEM_n => SIDEM, &
!          LWCSCA_n => LWCSCA, &
!          LWASCA_n => LWASCA, &
!          PSRAD_n => PSRAD, &
!          SW_ML_E_n => SW_ML_E, &
!          LW_ML_E_n => LW_ML_E

!
IMPLICIT NONE

INTEGER(KIND=JPIM), SAVE :: NSWSOLVER
INTEGER(KIND=JPIM), SAVE :: NLWSOLVER
INTEGER(KIND=JPIM), SAVE :: NGAS
INTEGER(KIND=JPIM), SAVE :: NLIQOPT
INTEGER(KIND=JPIM), SAVE :: NICEOPT
INTEGER(KIND=JPIM), SAVE :: NOVLP
INTEGER(KIND=JPIM), SAVE :: NRADLP
INTEGER(KIND=JPIM), SAVE :: NRADIP
INTEGER(KIND=JPIM), SAVE :: NREG
REAL(KIND=JPRB), SAVE    :: XCLOUD_FRAC_STD
INTEGER(KIND=JPIM), SAVE :: NLWSCATTERING
INTEGER(KIND=JPIM), SAVE :: NAERMACC
#ifdef MNH_ECRAD
#if ( VER_ECRAD == 140 )
LOGICAL, SAVE :: LSPEC_ALB
LOGICAL, SAVE :: LSPEC_EMISS
!REAL(KIND=JPRB), ALLOCATABLE, SAVE    :: USER_ALB_DIFF(:)
!REAL(KIND=JPRB), ALLOCATABLE, SAVE    :: USER_ALB_DIR(:)
!REAL(KIND=JPRB), ALLOCATABLE, SAVE    :: USER_EMISS(:)
CHARACTER(LEN=4), SAVE   :: SURF_TYPE
#endif
#endif
! LOGICAL, SAVE :: EFF3D
! LOGICAL, SAVE :: SIDEM
! LOGICAL, SAVE :: LWCSCA
! LOGICAL, SAVE :: LWASCA
! LOGICAL, SAVE :: PSRAD
! LOGICAL, SAVE :: SW_ML_E
! LOGICAL, SAVE :: LW_ML_E
!
NAMELIST/NAM_PARAM_ECRADn/NSWSOLVER,NLWSOLVER,NRADLP,NRADIP,&
                          NLIQOPT,NICEOPT,NOVLP,NGAS,NREG,XCLOUD_FRAC_STD,&
                          NLWSCATTERING, NAERMACC &
#ifndef MNH_ECRAD
                          & 
#else
#if ( VER_ECRAD == 140 )
                          , LSPEC_ALB, LSPEC_EMISS, &
!                          USER_ALB_DIFF, USER_ALB_DIR, USER_EMISS, &
                          SURF_TYPE
#else
                          & 
#endif
#endif
!
CONTAINS
!
SUBROUTINE INIT_NAM_PARAM_ECRADn
  NSWSOLVER = NSWSOLVER_n
  NLWSOLVER = NLWSOLVER_n
  NLIQOPT = NLIQOPT_n
  NICEOPT = NICEOPT_n
  NOVLP = NOVLP_n
  NRADLP = NRADLP_n
  NRADIP = NRADIP_n
  NGAS = NGAS_n
  NREG = NREG_n
  XCLOUD_FRAC_STD = XCLOUD_FRAC_STD_n
  NLWSCATTERING = NLWSCATTERING_n
  NAERMACC = NAERMACC_n
#ifdef MNH_ECRAD
#if ( VER_ECRAD == 140 )
  LSPEC_ALB = LSPEC_ALB_n
  LSPEC_EMISS = LSPEC_EMISS_n
!  USER_ALB_DIFF = USER_ALB_DIFF_n
!  USER_ALB_DIR = USER_ALB_DIR_n
!  USER_EMISS = USER_EMISS_n
  SURF_TYPE = SURF_TYPE_n
#endif
#endif  
END SUBROUTINE INIT_NAM_PARAM_ECRADn

SUBROUTINE UPDATE_NAM_PARAM_ECRADn
  NSWSOLVER_n = NSWSOLVER
  NLWSOLVER_n = NLWSOLVER
  NLIQOPT_n = NLIQOPT
  NICEOPT_n = NICEOPT
  NOVLP_n = NOVLP  
  NRADLP_n = NRADLP
  NRADIP_n = NRADIP
  NGAS_n = NGAS
  NREG_n = NREG
  XCLOUD_FRAC_STD_n = XCLOUD_FRAC_STD
  NLWSCATTERING_n = NLWSCATTERING
  NAERMACC_n = NAERMACC
#ifdef MNH_ECRAD
#if ( VER_ECRAD == 140 )
  LSPEC_ALB_n = LSPEC_ALB
  LSPEC_EMISS_n = LSPEC_EMISS
!  USER_ALB_DIFF_n = USER_ALB_DIFF
!  USER_ALB_DIR_n = USER_ALB_DIR
!  USER_EMISS_n = USER_EMISS
  SURF_TYPE_n = SURF_TYPE
#endif
#endif  
END SUBROUTINE UPDATE_NAM_PARAM_ECRADn

END MODULE MODN_PARAM_ECRAD_n
