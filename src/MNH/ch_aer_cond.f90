!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/ch_aer_growth.f90,v $ $Revision: 1.1.4.1.2.1 $
! MASDEV4_7 chimie 2006/05/18 13:07:25
!-----------------------------------------------------------------
!!   #########################
     MODULE MODI_CH_AER_COND
!!   #########################
!!
INTERFACE
!!
SUBROUTINE CH_AER_COND(PM, PLNSIG, PRG, PPRESSURE, PTEMP, &
                       PDM3CDT, PDM6CDT                   )
IMPLICIT NONE
REAL, DIMENSION(:,:), INTENT(IN)    :: PM, PLNSIG, PRG
REAL, DIMENSION(:),   INTENT(IN)    :: PPRESSURE, PTEMP 
REAL, DIMENSION(:,:), INTENT(INOUT) :: PDM3CDT, PDM6CDT
END SUBROUTINE CH_AER_COND
!!
END INTERFACE
!!
END MODULE MODI_CH_AER_COND
!!
!!   #################################################################
     SUBROUTINE CH_AER_COND(PM, PLNSIG, PRG, PPRESSURE, PTEMP,       &
                            PDM3CDT, PDM6CDT                         )
!!   #################################################################
!!
!!   PURPOSE
!!   -------
!!
!!   This routine computes the condensated mass and tendencies.
!!   Note that dM0_cond/dt = 0 : The condensation doesn't create particles.
!!   Only moments 3 and 6 are computed.
!!   
!!   REFERENCE
!!   ---------
!!
!!   Method from CMAQ model:
!!
!!    Binkowski, F.S. and U. Shankar, The regional particulate matter
!!      model 1. Model description and preliminary results, J. Geophys.
!!      Res., Vol 100, No D12, 26101-26209, 1995. 
!! 
!!
!!    AUTHOR
!!    ------
!!    Joris Pianezze (2018) * LACy *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
! 
USE MODD_CH_AEROSOL
USE MODD_CST,       ONLY : XPI, XBOLTZ, XAVOGADRO
USE MODD_CONF,      ONLY : NVERB
!!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)       :: PM, PLNSIG, PRG
REAL, DIMENSION(:),   INTENT(IN)       :: PPRESSURE, PTEMP
REAL, DIMENSION(:,:), INTENT(INOUT)    :: PDM3CDT, PDM6CDT
!
!*       0.2   Declarations of local variables
!
INTEGER                                :: JI,JK
REAL                                   :: ZALPHA
REAL,   DIMENSION(SIZE(PM,1))          :: ZDIFFSULF, ZDIFFCORR, ZDV
REAL,   DIMENSION(SIZE(PM,1))          :: ZCBAR
REAL,   DIMENSION(SIZE(PM,1))          :: ZGNC3, ZGNC6, ZGFM3, ZGFM6
REAL,   DIMENSION(SIZE(PM,1),6,JPMODE) :: ZMOM
!
!
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZATION
!              --------------
!
ZALPHA    = 0.1
ZDIFFSULF = 9.36E-6                                            ! molecular diffusivity of sulfuric acid
ZDIFFCORR = (101325.0/PPRESSURE) * (PTEMP/273.15)**(1.75)          ! correction factor for atmospheric conditions
ZDV       = ZDIFFSULF * ZDIFFCORR                              ! corrected molecural diffusivity of sulfuric acid
ZCBAR     = SQRT(8.0*XBOLTZ*XAVOGADRO*PTEMP/(XPI*XH2SO4*1E-3)) ! molecular velocitie (temperature dependent)
! 
!
!-------------------------------------------------------------------------------
!
!*       2.    COMPUTE CONDENSATED MASS AND TENDENCIES
!              ---------------------------------------
!
DO JI=1,JPMODE
  !
  ! ZMOM = Equation (after integration) for every moment (JK order): Tulet 2005
  ! ZMOM = m**k.m**-3
  DO JK=1,6
    ZMOM(:,JK,JI) = PM(:,NM0(JI))*((PRG(:,JI)*1E-6)**JK)* &
                    EXP(((REAL(JK)**2)/2.)*(PLNSIG(:,JI)**2.0))             
  ENDDO
  !
  ZGNC3 = 2 * XPI * ZDV * ZMOM(:,1,JI)                    ! 3rd moment, near-continuum
  ZGNC6 = 2 * XPI * ZDV * ZMOM(:,4,JI)                    ! 6th moment, near-continuum
  ZGFM3 = (XPI / 4.0) * ZALPHA * ZCBAR * ZMOM(:,2,JI)     ! 3rd moment, free-molecular
  ZGFM6 = (XPI / 4.0) * ZALPHA * ZCBAR * ZMOM(:,5,JI)     ! 6th moment, free-molecular
  !
  PDM3CDT(:,JI) = ZGNC3 * ZGFM3 / ( ZGNC3 + ZGFM3 )       ! 3rd moment : m**3 / m**3 s
  PDM6CDT(:,JI) = ZGNC6 * ZGFM6 / ( ZGNC6 + ZGFM6 )       ! 6th moment : m**6 / m**3 s
  !
END DO
!
END SUBROUTINE CH_AER_COND
