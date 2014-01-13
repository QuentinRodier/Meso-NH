!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODN_ASSIM
!     ##################
!
!!****  *MODN_ASSIM - declaration of keys for ISBA assimilation scheme (2DVAR, Bouyssel et al.)
!!
!!    PURPOSE
!!    -------
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
!!	L. Jarlan   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       23/02/05
!!      Merge assimilation   T. Aspelien met.no (04/2012)
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_ASSIM, ONLY:  LASSIM,CASSIM,CASSIM_ISBA,LPRINT,LAROME,LECSST,LAESST,LAESNM,LWATERTG2,&
                       LALADSURF,LREAD_SST_FROM_FILE,LEXTRAP_SEA,LEXTRAP_WATER,LEXTRAP_NATURE,&
                       NECHGU,RCLIMCA,RCLISST,SIGH2MO,SIGT2MO,SIGWGO,SIGWGB,SIGW2B,           &
                       LOBSWG,LOBS2M,LIMVEG,SPRECIP2,RTHR_QC,SIGWGO_MAX,RSCAL_JAC,            &
                       LPRT,LSIM,LBEV,LBFIXED,NOBSTYPE,YERROBS,INCO,IVAR,NVAR,XVAR_M,PREFIX_M,&
                       XSIGMA_M, TPRT_M, INCV, SCALE_Q   
!
IMPLICIT NONE
!
NAMELIST/NAM_ASSIM/LASSIM,CASSIM,CASSIM_ISBA,LPRINT,LAROME,LECSST,LAESST,LAESNM,LALADSURF,&
                   LREAD_SST_FROM_FILE,LEXTRAP_SEA,LEXTRAP_WATER,LEXTRAP_NATURE,LWATERTG2
NAMELIST/NAM_NACVEG/NECHGU,RCLIMCA,RCLISST,SIGH2MO,SIGT2MO,SIGWGO,SIGWGB,SIGW2B, &
                       LOBSWG,LOBS2M,LIMVEG,SPRECIP2,RTHR_QC,SIGWGO_MAX,RSCAL_JAC
NAMELIST/NAM_IO_VARASSIM/LPRT, LSIM, LBEV, LBFIXED
NAMELIST/NAM_OBS/NOBSTYPE, YERROBS, INCO
NAMELIST/NAM_VAR/IVAR, NVAR, XVAR_M, PREFIX_M, XSIGMA_M, TPRT_M, INCV, SCALE_Q
!
END MODULE MODN_ASSIM
