!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
! $Source$
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_RESOLVED_CLOUD
!     ##########################
INTERFACE
      SUBROUTINE RESOLVED_CLOUD ( HCLOUD, HACTCCN, HSCONV, HMF_CLOUD,                  &
                                  KRR, KSPLITR, KSPLITG, KMI, KTCOUNT,                 &
                                  HLBCX, HLBCY, HFMFILE, HLUOUT, HRAD, HTURBDIM,       &
                                  OCLOSE_OUT, OSUBG_COND, OSIGMAS, HSUBG_AUCV,         &
                                  PTSTEP, PZZ, PRHODJ, PRHODREF, PEXNREF,              &
                                  PPABST, PTHT, PRT, PSIGS, PSIGQSAT, PMFCONV,         &
                                  PTHM, PRCM, PPABSM,                                  &
                                  PW_ACT, PTHS, PRS, PSVT, PSVS, PSRCS, PCLDFR,        &
                                  PCIT, OSEDIC, OACTIT, OSEDC, OSEDI,                  &
                                  ORAIN, OWARM, OHHONI, OCONVHG,                       &
                                  PCF_MF,PRC_MF, PRI_MF,                               &
                                  PINPRC,PINPRC3D,PINPRR,PINPRR3D, PEVAP3D,            &
                                  PINPRS,PINPRS3D,PINPRG,PINPRG3D,PINPRH,PINPRH3D,     &
                                  PSOLORG,PMI,                                         &
                                  PSPEEDC, PSPEEDR, PSPEEDS, PSPEEDG, PSPEEDH,         &
                                  PSEA,PTOWN          )   
!
CHARACTER(LEN=4),         INTENT(IN)   :: HCLOUD   ! kind of cloud
CHARACTER(LEN=4),         INTENT(IN)   :: HACTCCN  ! kind of CCN activation scheme
                                                   ! paramerization
CHARACTER(LEN=4),         INTENT(IN)   :: HSCONV   ! Shallow convection scheme
CHARACTER(LEN=4),         INTENT(IN)   :: HMF_CLOUD! Type of statistical cloud
INTEGER,                  INTENT(IN)   :: KRR      ! Number of moist variables
INTEGER,                  INTENT(IN)   :: KSPLITR  ! Number of small time step
                                       ! integrations for  rain sedimendation
INTEGER,                  INTENT(IN)   :: KSPLITG  ! Number of small time step
                                       ! integrations for  ice  sedimendation
INTEGER,                  INTENT(IN)   :: KMI      ! Model index
INTEGER,                  INTENT(IN)   :: KTCOUNT  ! Temporal loop counter
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX,HLBCY   ! X and Y-direc. LBC type
CHARACTER(LEN=*),         INTENT(IN)   :: HFMFILE  ! Name of the output FM-file
CHARACTER(LEN=*),         INTENT(IN)   :: HLUOUT   ! Output-listing name for
                                                   ! model n
CHARACTER*4,              INTENT(IN)   :: HRAD     ! Radiation scheme name
CHARACTER*4,              INTENT(IN)   :: HTURBDIM ! Dimensionality of the
                                                   ! turbulence scheme
LOGICAL,                  INTENT(IN)   :: OCLOSE_OUT ! Conditional closure of
                                                   ! the OUTPUT FM-file
LOGICAL,                  INTENT(IN)   :: OSUBG_COND ! Switch for Subgrid Cond.
LOGICAL,                  INTENT(IN)   :: OSIGMAS  ! Switch for Sigma_s:
                                        ! use values computed in CONDENSATION
                                        ! or that from turbulence scheme
CHARACTER(LEN=4),         INTENT(IN)   :: HSUBG_AUCV
                                        ! Kind of Subgrid autoconversion method
REAL,                     INTENT(IN)   :: PTSTEP ! Time step :XTSTEP in namelist
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PRHODJ  !Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PRHODREF! Reference dry air density
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PEXNREF ! Reference Exner function
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PPABST  ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT):: PRT     ! Moist variables at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PSIGS   ! Sigma_s at time t
REAL,                     INTENT(IN)   :: PSIGQSAT! coeff applied to qsat variance contribution
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PMFCONV ! convective mass flux
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PTHM    ! Theta at time t-Dt
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PPABSM   ! Pressure time t-Dt
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PRCM    ! Cloud water m.r. at time t-Dt
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_ACT ! W for CCN activation
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS  ! Theta source
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRS   ! Moist  variable sources
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVT  ! Scalar variable at time t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVS  ! Scalar variable sources
!
!
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSRCS ! Second-order flux
                                                 ! s'rc'/2Sigma_s2 at time t+1
                                                 ! multiplied by Lambda_3
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PCLDFR! Cloud fraction
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PCIT  ! Pristine ice number
                                                 ! concentration at time t
LOGICAL,                  INTENT(IN)    :: OSEDIC! Switch to activate the
                                                 ! cloud droplet sedimentation
                                                 ! for ICE3            
LOGICAL,                  INTENT(IN)    :: OACTIT ! Switch to activate the
                                                 ! activation through temp.
                                                 ! evolution in C2R2 and KHKO
LOGICAL,                  INTENT(IN)    :: OSEDC ! Switch to activate the
                                                 ! cloud droplet sedimentation
                                                 ! for C2R2 or KHKO
LOGICAL,                  INTENT(IN)    :: OSEDI ! Switch to activate the
                                                 ! cloud crystal sedimentation
LOGICAL,                  INTENT(IN)    :: ORAIN ! Switch to activate the
                                                 ! raindrop formation
LOGICAL,                  INTENT(IN)    :: OWARM ! Control of the rain formation
                                                 !  by slow warm microphysical
                                                 !         processes
LOGICAL,                  INTENT(IN)    :: OHHONI! enable haze freezing
LOGICAL,                  INTENT(IN)    :: OCONVHG! Switch for conversion from
                                                  ! hail to graupel
!
REAL, DIMENSION(:,:,:),     INTENT(IN)    :: PCF_MF! Convective Mass Flux Cloud fraction 
REAL, DIMENSION(:,:,:),     INTENT(IN)    :: PRC_MF! Convective Mass Flux liquid mixing ratio
REAL, DIMENSION(:,:,:),     INTENT(IN)    :: PRI_MF! Convective Mass Flux solid mixing ratio
!
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRC! Cloud instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRR! Rain instant precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRR3D ! sed flux of precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PEVAP3D  ! evap profile
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRS! Snow instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRG! Graupel instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRH! Hail instant precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRC3D ! sed flux of precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRS3D ! sed flux of precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRG3D ! sed flux of precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRH3D ! sed flux of precip
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSOLORG ![%] solubility fraction of soa
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PMI !
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDC ! Cloud sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDR ! Rain sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDS ! Snow sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDG ! Graupel sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDH ! Hail sedimentation speed
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PSEA      ! Land Sea mask
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PTOWN      ! Town fraction
!
END SUBROUTINE RESOLVED_CLOUD
END INTERFACE
END MODULE MODI_RESOLVED_CLOUD
!
!     ##########################################################################
      SUBROUTINE RESOLVED_CLOUD ( HCLOUD, HACTCCN, HSCONV, HMF_CLOUD,                  &
                                  KRR, KSPLITR, KSPLITG, KMI, KTCOUNT,                 &
                                  HLBCX, HLBCY, HFMFILE, HLUOUT, HRAD, HTURBDIM,       &
                                  OCLOSE_OUT, OSUBG_COND, OSIGMAS, HSUBG_AUCV,         &
                                  PTSTEP, PZZ, PRHODJ, PRHODREF, PEXNREF,              &
                                  PPABST, PTHT, PRT, PSIGS, PSIGQSAT, PMFCONV,         &
                                  PTHM, PRCM, PPABSM,                                  &
                                  PW_ACT, PTHS, PRS, PSVT, PSVS, PSRCS, PCLDFR,        &
                                  PCIT, OSEDIC, OACTIT, OSEDC, OSEDI,                  &
                                  ORAIN, OWARM, OHHONI, OCONVHG,                       &
                                  PCF_MF,PRC_MF, PRI_MF,                               &
                                  PINPRC,PINPRC3D,PINPRR,PINPRR3D, PEVAP3D,            &
                                  PINPRS,PINPRS3D,PINPRG,PINPRG3D,PINPRH,PINPRH3D,     &
                                  PSOLORG,PMI,                                         &
                                  PSPEEDC, PSPEEDR, PSPEEDS, PSPEEDG, PSPEEDH,         &
                                  PSEA,PTOWN          )   
!     ##########################################################################
!
!!****  * -  compute the  resolved clouds and precipitation
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the  microphysical sources
!!    related to the resolved clouds and precipitation
!!
!!
!!**  METHOD
!!    ------
!!      The main actions of this routine is to call the routines computing the
!!    microphysical sources. Before that:
!!        - it computes the real absolute pressure,
!!        - negative values of the current guess of all mixing ratio are removed.
!!          This is done by a global filling algorithm based on a multiplicative
!!          method (Rood, 1987), in order to conserved the total mass in the
!!          simulation domain.
!!        - Sources are transformed in physical tendencies, by removing the
!!          multiplicative term Rhod*J.
!!        - External points values are filled owing to the use of cyclic
!!          l.b.c., in order to performe computations on the full domain.
!!      After calling to microphysical routines, the physical tendencies are
!!    switched back to prognostic variables.
!!
!!
!!    EXTERNAL
!!    --------
!!      Subroutine FMLOOK: to recover the logical unit number linked to a FMfile
!!      Subroutine SLOW_TERMS: Computes the explicit microphysical sources
!!      Subroutine FAST_TERMS: Performs the saturation adjustment for l
!!      Subroutine RAIN_ICE  : Computes the explicit microphysical sources for i
!!      Subroutine ICE_ADJUST: Performs the saturation adjustment for i+l
!!      MIN_ll,SUM3D_ll : distributed functions equivalent to MIN and SUM
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS : contains declarations of parameter variables
!!         JPHEXT       : Horizontal external points number
!!         JPVEXT       : Vertical external points number
!!      Module MODD_CST
!!          XP00               ! Reference pressure
!!          XRD                ! Gaz  constant for dry air
!!          XCPD               ! Cpd (dry air)
!!
!!    REFERENCE
!!    ---------
!!
!!      Book1 and book2 of documentation ( routine RESOLVED_CLOUD )
!!
!!    AUTHOR
!!    ------
!!      E. Richard       * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/12/94
!!      Modifications: June 8, 1995 ( J.Stein )
!!                                   Cleaning to improve efficienty and clarity
!!                                  in agreement with the MESO-NH coding norm
!!                     March 1, 1996 ( J.Stein )
!!                                   store the cloud fraction
!!                     March 18, 1996 ( J.Stein )
!!                                   check that ZMASSPOS /= 0
!!                     Oct.  12, 1996 ( J.Stein )
!!                                   remove the negative values correction
!!                                   for the KES2 case
!!      Modifications: Dec 14, 1995 (J.-P. Pinty)
!!                                   Add the mixed-phase option
!!      Modifications: Jul 01, 1996 (J.-P. Pinty)
!!                                   Change arg. list in routine FAST_TERMS
!!      Modifications: Jan 27, 1997 (J.-P. Pinty)
!!                                   add W and SV in arg. list
!!      Modifications: March 23, 98 (E.Richard)
!!                                   correction of negative value based on
!!                                  rv+rc+ri and thetal or thetail conservation
!!      Modifications: April 08, 98 (J.-P. Lafore and V. Ducrocq )
!!                                  modify the  correction of negative values
!!      Modifications: June 08, 00  (J.-P. Pinty and J.-M. Cohard)
!!                                  add the C2R2 scheme
!!      Modifications: April 08, 01  (J.-P. Pinty)
!!                                  add the C3R5 scheme
!!      Modifications: July  21, 01  (J.-P. Pinty)
!!                                  Add OHHONI and PW_ACT (for haze freezing)
!!      Modifications: Sept 21, 01  (J.-P. Pinty)
!!                                  Add XCONC_CCN limitation
!!      Modifications: Nov  21, 02  (J.-P. Pinty)
!!                                  Add ICE4 and C3R5 options
!!                     June, 2005   (V. Masson)
!!                                  Technical change in interface for scalar arguments
!!      Modifications : March, 2006 (O.Geoffroy)
!!                                  Add KHKO scheme
!!      Modifications : March 2013  (O.Thouron)
!!                                  Add prognostic supersaturation
!!              July, 2015 (O.Nuissier/F.Duffourg) Add microphysics diagnostic for
!!                                      aircraft, ballon and profiler
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODE_ll
USE MODE_ll
USE MODE_FM
!
USE MODD_CONF
USE MODD_CST
USE MODD_PARAMETERS
USE MODD_PARAM_ICE,  ONLY : CSEDIM
USE MODD_RAIN_ICE_DESCR
USE MODD_PARAM_C2R2
USE MODD_BUDGET
USE MODD_NSV
USE MODD_CH_AEROSOL , ONLY : LORILAM
USE MODD_DUST , ONLY : LDUST
USE MODD_SALT , ONLY : LSALT
!
USE MODI_SLOW_TERMS
USE MODI_FAST_TERMS
USE MODI_ICE_ADJUST
USE MODI_RAIN_ICE
USE MODI_RAIN_C2R2_KHKO
USE MODI_ICE_C1R3
USE MODI_C2R2_ADJUST
USE MODI_KHKO_NOTADJUST
USE MODI_C3R5_ADJUST
USE MODI_SHUMAN
USE MODI_BUDGET
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
!
CHARACTER(LEN=4),         INTENT(IN)   :: HCLOUD   ! kind of cloud
                                                   ! paramerization
CHARACTER(LEN=4),         INTENT(IN)   :: HACTCCN  ! kind of CCN activation
CHARACTER(LEN=4),         INTENT(IN)   :: HSCONV   ! Shallow convection scheme
CHARACTER(LEN=4),         INTENT(IN)   :: HMF_CLOUD! Type of statistical cloud
INTEGER,                  INTENT(IN)   :: KRR      ! Number of moist variables
INTEGER,                  INTENT(IN)   :: KSPLITR  ! Number of small time step
                                       ! integrations for  rain sedimendation
INTEGER,                  INTENT(IN)   :: KSPLITG  ! Number of small time step
                                       ! integrations for  ice  sedimendation
INTEGER,                  INTENT(IN)   :: KMI      ! Model index
INTEGER,                  INTENT(IN)   :: KTCOUNT  ! Temporal loop counter
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX,HLBCY   ! X and Y-direc. LBC type
CHARACTER(LEN=*),         INTENT(IN)   :: HFMFILE  ! Name of the output FM-file
CHARACTER(LEN=*),         INTENT(IN)   :: HLUOUT   ! Output-listing name for
                                                   ! model n
CHARACTER*4,              INTENT(IN)   :: HRAD     ! Radiation scheme name
CHARACTER*4,              INTENT(IN)   :: HTURBDIM ! Dimensionality of the
                                                   ! turbulence scheme
LOGICAL,                  INTENT(IN)   :: OCLOSE_OUT ! Conditional closure of
                                                   ! the OUTPUT FM-file
LOGICAL,                  INTENT(IN)   :: OSUBG_COND ! Switch for Subgrid Cond.
LOGICAL,                  INTENT(IN)   :: OSIGMAS  ! Switch for Sigma_s:
                                        ! use values computed in CONDENSATION
                                        ! or that from turbulence scheme
CHARACTER(LEN=4),         INTENT(IN)   :: HSUBG_AUCV
                                        ! Kind of Subgrid autoconversion method
REAL,                     INTENT(IN)   :: PTSTEP ! Time step :XTSTEP in namelist
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PRHODJ  !Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PRHODREF! Reference dry air density
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PEXNREF ! Reference Exner function
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PPABST  ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT):: PRT     ! Moist variables at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PSIGS   ! Sigma_s at time t
REAL,                     INTENT(IN)   :: PSIGQSAT! coeff applied to qsat variance contribution
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PMFCONV ! convective mass flux
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PTHM    ! Theta at time t-Dt
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PPABSM   ! Pressure time t-Dt
REAL, DIMENSION(:,:,:),   INTENT(IN)   :: PRCM    ! Cloud water m.r. at time t-Dt
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_ACT ! W for CCN activation
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS  ! Theta source
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRS   ! Moist  variable sources
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVT  ! Scalar variable at time t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVS  ! Scalar variable sources
!
!
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSRCS ! Second-order flux
                                                 ! s'rc'/2Sigma_s2 at time t+1
                                                 ! multiplied by Lambda_3
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PCLDFR! Cloud fraction
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PCIT  ! Pristine ice number
                                                 ! concentration at time t
LOGICAL,                  INTENT(IN)    :: OSEDIC! Switch to activate the
                                                 ! cloud droplet sedimentation
                                                 ! for ICE3            
LOGICAL,                  INTENT(IN)    :: OACTIT ! Switch to activate the
                                                 ! activation through temp.
                                                 ! evolution in C2R2 and KHKO
LOGICAL,                  INTENT(IN)    :: OSEDC ! Switch to activate the
                                                 ! cloud droplet sedimentation
LOGICAL,                  INTENT(IN)    :: OSEDI ! Switch to activate the
                                                 ! cloud crystal sedimentation
LOGICAL,                  INTENT(IN)    :: ORAIN ! Switch to activate the
                                                 ! raindrop formation
LOGICAL,                  INTENT(IN)    :: OWARM ! Control of the rain formation
                                                 !  by slow warm microphysical
                                                 !         processes
LOGICAL,                  INTENT(IN)    :: OHHONI! enable haze freezing
LOGICAL,                  INTENT(IN)    :: OCONVHG! Switch for conversion from
                                                  ! hail to graupel
!
REAL, DIMENSION(:,:,:),     INTENT(IN)    :: PCF_MF! Convective Mass Flux Cloud fraction 
REAL, DIMENSION(:,:,:),     INTENT(IN)    :: PRC_MF! Convective Mass Flux liquid mixing ratio
REAL, DIMENSION(:,:,:),     INTENT(IN)    :: PRI_MF! Convective Mass Flux solid mixing ratio
!
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRC! Cloud instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRR! Rain instant precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRR3D ! sed flux of precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PEVAP3D  ! evap profile
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRS! Snow instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRG! Graupel instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRH! Hail instant precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRC3D ! sed flux of precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRS3D ! sed flux of precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRG3D ! sed flux of precip
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PINPRH3D ! sed flux of precip
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSOLORG ![%] solubility fraction of soa
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PMI !
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDC ! Cloud sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDR ! Rain sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDS ! Snow sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDG ! Graupel sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSPEEDH ! Hail sedimentation speed
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PSEA      ! Land Sea mask
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PTOWN      ! Town fraction
!
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JRR,JSV       ! Loop index for the moist and scalar variables
INTEGER :: IIB           !  Define the physical domain
INTEGER :: IIE           !
INTEGER :: IJB           !
INTEGER :: IJE           !
INTEGER :: IKB           !
INTEGER :: IKE           !
INTEGER :: IKU
INTEGER :: IINFO_ll      ! return code of parallel routine
INTEGER :: JK
!
!
!
REAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3)):: ZDZZ
REAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3)):: ZT,ZEXN,ZLV,ZLS,ZCPH
REAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3)):: ZCOR
                                    ! for the correction of negative rv
REAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3)):: ZZZ
                                    ! model layer height
REAL  :: ZMASSTOT                   ! total mass  for one water category
                                    ! including the negative values
REAL  :: ZMASSPOS                   ! total mass  for one water category
                                    ! after removing the negative values
REAL  :: ZRATIO                     ! ZMASSTOT / ZMASSCOR
!
INTEGER                               :: ISVBEG ! first scalar index for microphysics
INTEGER                               :: ISVEND ! last  scalar index for microphysics
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZSVT   ! scalar variable for microphysics only
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZSVS   ! scalar tendency for microphysics only
!
!------------------------------------------------------------------------------
!
!*       1.     PRELIMINARY COMPUTATIONS
!               ------------------------
!
IIB=1+JPHEXT
IIE=SIZE(PZZ,1) - JPHEXT
IJB=1+JPHEXT
IJE=SIZE(PZZ,2) - JPHEXT
IKB=1+JPVEXT
IKE=SIZE(PZZ,3) - JPVEXT
IKU=SIZE(PZZ,3)
!
IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'KHKO') THEN
  ISVBEG = NSV_C2R2BEG
  ISVEND = NSV_C2R2END
ELSE IF (HCLOUD == 'C3R5') THEN
  ISVBEG = NSV_C2R2BEG
  ISVEND = NSV_C1R3END
ELSE
  ISVBEG = 0
  ISVEND = 0
END IF
!
IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'C3R5' .OR. HCLOUD == 'KHKO') THEN
  ALLOCATE(ZSVT(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3),ISVEND - ISVBEG + 1))
  ALLOCATE(ZSVS(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3),ISVEND - ISVBEG + 1))
  ZSVT(:,:,:,:) = PSVT(:,:,:,ISVBEG:ISVEND)
  ZSVS(:,:,:,:) = PSVS(:,:,:,ISVBEG:ISVEND)
END IF
!
!*       2.     TRANSFORMATION INTO PHYSICAL TENDENCIES
!               ---------------------------------------
!
PTHS(:,:,:) = PTHS(:,:,:) / PRHODJ(:,:,:)
DO JRR = 1,KRR
  PRS(:,:,:,JRR)  = PRS(:,:,:,JRR) / PRHODJ(:,:,:)
END DO
!
IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'C3R5' .OR. HCLOUD == 'KHKO') THEN
  DO JSV = 1,SIZE(ZSVS,4)
    ZSVS(:,:,:,JSV) = ZSVS(:,:,:,JSV) / PRHODJ(:,:,:)
  ENDDO
ENDIF
!
!  complete the lateral boundaries to avoid possible problems
!
PTHS(IIB-1,:,:) = PTHS(IIB,:,:)
PTHS(IIE+1,:,:) = PTHS(IIE,:,:)
PTHS(:,IJB-1,:) = PTHS(:,IJB,:)
PTHS(:,IJE+1,:) = PTHS(:,IJE,:)
!
PRS(IIB-1,:,:,:) = PRS(IIB,:,:,:)
PRS(IIE+1,:,:,:) = PRS(IIE,:,:,:)
PRS(:,IJB-1,:,:) = PRS(:,IJB,:,:)
PRS(:,IJE+1,:,:) = PRS(:,IJE,:,:)
!
!  complete the physical boundaries to avoid some computations
!
IF(LWEST_ll()  .AND. HLBCX(1) /= 'CYCL')  PRT(IIB-1,:,:,2:) = 0.0
IF(LEAST_ll()  .AND. HLBCX(2) /= 'CYCL')  PRT(IIE+1,:,:,2:) = 0.0
IF(LSOUTH_ll() .AND. HLBCY(1) /= 'CYCL')  PRT(:,IJB-1,:,2:) = 0.0
IF(LNORTH_ll() .AND. HLBCY(2) /= 'CYCL')  PRT(:,IJE+1,:,2:) = 0.0
!
IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'C3R5' .OR. HCLOUD == 'KHKO') THEN
  ZSVS(IIB-1,:,:,:) = ZSVS(IIB,:,:,:)
  ZSVS(IIE+1,:,:,:) = ZSVS(IIE,:,:,:)
  ZSVS(:,IJB-1,:,:) = ZSVS(:,IJB,:,:)
  ZSVS(:,IJE+1,:,:) = ZSVS(:,IJE,:,:)
!
!  complete the physical boundaries to avoid some computations
!
  IF(LWEST_ll()  .AND. HLBCX(1) /= 'CYCL')  ZSVT(IIB-1,:,:,:) = 0.0
  IF(LEAST_ll()  .AND. HLBCX(2) /= 'CYCL')  ZSVT(IIE+1,:,:,:) = 0.0
  IF(LSOUTH_ll() .AND. HLBCY(1) /= 'CYCL')  ZSVT(:,IJB-1,:,:) = 0.0
  IF(LNORTH_ll() .AND. HLBCY(2) /= 'CYCL')  ZSVT(:,IJE+1,:,:) = 0.0
ENDIF
!
!  complete the vertical boundaries
!
PTHS(:,:,IKB-1) = PTHS(:,:,IKB)
PTHS(:,:,IKE+1) = PTHS(:,:,IKE)
!
PRS(:,:,IKB-1,:) = PRS(:,:,IKB,:)
PRS(:,:,IKE+1,:) = PRS(:,:,IKE,:)
!
PRT(:,:,IKB-1,:) = PRT(:,:,IKB,:)
PRT(:,:,IKE+1,:) = PRT(:,:,IKE,:)
!
IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'C3R5' .OR. HCLOUD == 'KHKO') THEN
  ZSVS(:,:,IKB-1,:) = ZSVS(:,:,IKB,:)
  ZSVS(:,:,IKE+1,:) = ZSVS(:,:,IKE,:)
  ZSVT(:,:,IKB-1,:) = ZSVT(:,:,IKB,:)
  ZSVT(:,:,IKE+1,:) = ZSVT(:,:,IKE,:)
ENDIF
!
! personal comment:  tranfering these variables to the
!                    microphysical routines would save
!                    computing time
!
ZEXN(:,:,:)= (PPABST(:,:,:)/XP00)**(XRD/XCPD)
ZT(:,:,:)= PTHT(:,:,:)*ZEXN(:,:,:)
ZLV(:,:,:)=XLVTT +(XCPV-XCL) *(ZT(:,:,:)-XTT)
ZLS(:,:,:)=XLSTT +(XCPV-XCI) *(ZT(:,:,:)-XTT)
ZCPH(:,:,:)=XCPD +XCPV*PRT(:,:,:,1)
!
!
!*       3.     REMOVE NEGATIVE VALUES
!               ----------------------
!
!*       3.1    Non local correction for precipitating species (Rood 87)
!
IF (HCLOUD == 'KESS' .OR. HCLOUD == 'ICE3'                       &
    .OR.  HCLOUD == 'C2R2' .OR. HCLOUD == 'C3R5' .OR. HCLOUD == 'KHKO') THEN
!
  DO JRR = 3,KRR
    SELECT CASE (JRR)
      CASE(3,5,6,7) ! rain, snow, graupel and hail

        IF ( MIN_ll( PRS(:,:,:,JRR), IINFO_ll) < 0.0 ) THEN
!
! compute the total water mass computation
!
          ZMASSTOT = MAX( 0. , SUM3D_ll( PRS(:,:,:,JRR), IINFO_ll ) )
!
! remove the negative values
!
          PRS(:,:,:,JRR) = MAX( 0., PRS(:,:,:,JRR) )
!
! compute the new total mass
!
          ZMASSPOS = MAX(XMNH_TINY,SUM3D_ll( PRS(:,:,:,JRR), IINFO_ll ) )
!
! correct again in such a way to conserve the total mass
!
          ZRATIO = ZMASSTOT / ZMASSPOS
          PRS(:,:,:,JRR) = PRS(:,:,:,JRR) * ZRATIO
!
        END IF
    END SELECT
  END DO
END IF
!
!*       3.2    Adjustement for liquid and solid cloud
!
SELECT CASE ( HCLOUD )
  CASE('KESS')
    WHERE (PRS(:,:,:,2) < 0.)
      PRS(:,:,:,1) = PRS(:,:,:,1) + PRS(:,:,:,2)
      PTHS(:,:,:) = PTHS(:,:,:) - PRS(:,:,:,2) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRS(:,:,:,2) = 0.0
    END WHERE
!
!
  CASE('C2R2','KHKO')                                 
    WHERE (PRS(:,:,:,2) < 0. .OR. ZSVS(:,:,:,2) < 0.)
      ZSVS(:,:,:,1) = 0.0
    END WHERE
    DO JSV = 2, 3
      WHERE (PRS(:,:,:,JSV) < 0. .OR. ZSVS(:,:,:,JSV) < 0.)
        PRS(:,:,:,1) = PRS(:,:,:,1) + PRS(:,:,:,JSV)
        PTHS(:,:,:) = PTHS(:,:,:) - PRS(:,:,:,JSV) * ZLV(:,:,:) /  &
             ZCPH(:,:,:) / ZEXN(:,:,:)
        PRS(:,:,:,JSV)  = 0.0
        ZSVS(:,:,:,JSV) = 0.0
      END WHERE
    ENDDO
! Commented 03/2013 O.Thouron 
! (at least necessary to be commented for supersaturation variable)
!  ZSVS(:,:,:,:) = MAX( 0.0,ZSVS(:,:,:,:) )
!
!
  CASE('ICE3','ICE4')
    WHERE (PRS(:,:,:,4) < 0.)
      PRS(:,:,:,1) = PRS(:,:,:,1) + PRS(:,:,:,4)
      PTHS(:,:,:) = PTHS(:,:,:) - PRS(:,:,:,4) * ZLS(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRS(:,:,:,4) = 0.
    END WHERE
!
!   cloud
    WHERE (PRS(:,:,:,2) < 0.)
      PRS(:,:,:,1) = PRS(:,:,:,1) + PRS(:,:,:,2)
      PTHS(:,:,:) = PTHS(:,:,:) - PRS(:,:,:,2) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRS(:,:,:,2) = 0.
    END WHERE
!
! if rc or ri are positive, we can correct negative rv
!   cloud
    WHERE ((PRS(:,:,:,1) <0.) .AND. (PRS(:,:,:,2)> 0.) )
      PRS(:,:,:,1) = PRS(:,:,:,1) + PRS(:,:,:,2)
      PTHS(:,:,:) = PTHS(:,:,:) - PRS(:,:,:,2) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRS(:,:,:,2) = 0.
    END WHERE
!   ice
    IF(KRR > 3) THEN
      WHERE ((PRS(:,:,:,1) < 0.).AND.(PRS(:,:,:,4) > 0.))
        ZCOR(:,:,:)=MIN(-PRS(:,:,:,1),PRS(:,:,:,4))
        PRS(:,:,:,1) = PRS(:,:,:,1) + ZCOR(:,:,:)
        PTHS(:,:,:) = PTHS(:,:,:) - ZCOR(:,:,:) * ZLS(:,:,:) /  &
             ZCPH(:,:,:) / ZEXN(:,:,:)
        PRS(:,:,:,4) = PRS(:,:,:,4) -ZCOR(:,:,:)
      END WHERE
    END IF
!
   CASE('C3R5')
    WHERE (PRS(:,:,:,2) < 0. .OR. ZSVS(:,:,:,2) < 0.)
      ZSVS(:,:,:,1) = 0.0
    END WHERE
    DO JSV = 2, 3
      WHERE (PRS(:,:,:,JSV) < 0. .OR. ZSVS(:,:,:,JSV) < 0.)
        PRS(:,:,:,1) = PRS(:,:,:,1) + PRS(:,:,:,JSV)
        PTHS(:,:,:) = PTHS(:,:,:) - PRS(:,:,:,JSV) * ZLV(:,:,:) /  &
             ZCPH(:,:,:) / ZEXN(:,:,:)
        PRS(:,:,:,JSV)  = 0.0
        ZSVS(:,:,:,JSV) = 0.0
      END WHERE
    ENDDO
    ZSVS(:,:,:,:) = MAX( 0.0,ZSVS(:,:,:,:) )
!   ice
    WHERE (PRS(:,:,:,4) < 0.)
      PRS(:,:,:,1) = PRS(:,:,:,1) + PRS(:,:,:,4)
      PTHS(:,:,:) = PTHS(:,:,:) - PRS(:,:,:,4) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRS(:,:,:,4)  = 0.0
      PSVS(:,:,:,4) = 0.0
    END WHERE
!   cloud
    WHERE (PRS(:,:,:,2) < 0.)
      PRS(:,:,:,1) = PRS(:,:,:,1) + PRS(:,:,:,2)
      PTHS(:,:,:) = PTHS(:,:,:) - PRS(:,:,:,2) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRS(:,:,:,2)  = 0.0
      PSVS(:,:,:,2) = 0.0
    END WHERE
    PSVS(:,:,:,:) = MAX( 0.0,PSVS(:,:,:,:) )
!
END SELECT
!
!
!*       3.3  STORE THE BUDGET TERMS
!            ----------------------
!
IF (LBUDGET_RV) CALL BUDGET (PRS(:,:,:,1) * PRHODJ(:,:,:), 6,'NEGA_BU_RRV')
IF (LBUDGET_RC) CALL BUDGET (PRS(:,:,:,2) * PRHODJ(:,:,:), 7,'NEGA_BU_RRC')
IF (LBUDGET_RR) CALL BUDGET (PRS(:,:,:,3) * PRHODJ(:,:,:), 8,'NEGA_BU_RRR')
IF (LBUDGET_RI) CALL BUDGET (PRS(:,:,:,4) * PRHODJ(:,:,:) ,9,'NEGA_BU_RRI')
IF (LBUDGET_RS) CALL BUDGET (PRS(:,:,:,5) * PRHODJ(:,:,:),10,'NEGA_BU_RRS')
IF (LBUDGET_RG) CALL BUDGET (PRS(:,:,:,6) * PRHODJ(:,:,:),11,'NEGA_BU_RRG')
IF (LBUDGET_RH) CALL BUDGET (PRS(:,:,:,7) * PRHODJ(:,:,:),12,'NEGA_BU_RRH')
IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)  * PRHODJ(:,:,:), 4,'NEGA_BU_RTH')
!

!*       3.4    Limitations of Na and Nc to the CCN max number concentration
!
! Commented by O.Thouron 03/2013
!IF ((HCLOUD == 'C2R2' .OR. HCLOUD == 'C3R5' .OR. HCLOUD == 'KHKO') &
!     .AND.(XCONC_CCN > 0)) THEN
!  IF ((HACTCCN /= 'ABRK')) THEN
!  ZSVT(:,:,:,1) = MIN( ZSVT(:,:,:,1),XCONC_CCN )
!  ZSVT(:,:,:,2) = MIN( ZSVT(:,:,:,2),XCONC_CCN )
!  ZSVS(:,:,:,1) = MIN( ZSVS(:,:,:,1),XCONC_CCN )
!  ZSVS(:,:,:,2) = MIN( ZSVS(:,:,:,2),XCONC_CCN )
!  END IF
!END IF
!
!
!-------------------------------------------------------------------------------
!
SELECT CASE ( HCLOUD )
  CASE ('REVE')
!
!*       4.     REVERSIBLE MICROPHYSICAL SCHEME
!               -------------------------------
!
    CALL FAST_TERMS ( KRR, KMI, HFMFILE, HLUOUT, HRAD, HTURBDIM,               &
                      HSCONV, HMF_CLOUD, OCLOSE_OUT, OSUBG_COND, PTSTEP,       &
                      PRHODJ, PSIGS, PPABST,                                   &
                      PCF_MF,PRC_MF,                                           &
                      PRVT=PRT(:,:,:,1), PRCT=PRT(:,:,:,2),                    &
                      PRVS=PRS(:,:,:,1), PRCS=PRS(:,:,:,2),                    &
                      PTHS=PTHS, PSRCS=PSRCS, PCLDFR=PCLDFR                    )
!
  CASE ('KESS')
!
!*       5.     KESSLER MICROPHYSICAL SCHEME
!               ----------------------------
!
!
!*       5.1    Compute the explicit microphysical sources
!
    CALL SLOW_TERMS ( KSPLITR, PTSTEP, KMI, HSUBG_AUCV,                       &
                      PZZ, PRHODJ, PRHODREF, PCLDFR,                          &
                      PTHT, PRT(:,:,:,1), PRT(:,:,:,2), PRT(:,:,:,3), PPABST, &
                      PTHS, PRS(:,:,:,1), PRS(:,:,:,2), PRS(:,:,:,3),         &
                      PINPRR, PINPRR3D, PEVAP3D                         )
!
!*       5.2    Perform the saturation adjustment
!
    CALL FAST_TERMS ( KRR, KMI, HFMFILE, HLUOUT, HRAD, HTURBDIM,               &
                      HSCONV, HMF_CLOUD, OCLOSE_OUT, OSUBG_COND, PTSTEP,       &
                      PRHODJ, PSIGS, PPABST,                                   &
                      PCF_MF,PRC_MF,                                           &
                      PRVT=PRT(:,:,:,1), PRCT=PRT(:,:,:,2),                    &
                      PRVS=PRS(:,:,:,1), PRCS=PRS(:,:,:,2), PRRS=PRS(:,:,:,3), &
                      PTHS=PTHS, PSRCS=PSRCS, PCLDFR=PCLDFR                    )
!
!
  CASE ('C2R2','KHKO')
!
!*       7.     2-MOMENT WARM MICROPHYSICAL SCHEME C2R2 or KHKO
!               ---------------------------------------
!
!
!*       7.1    Compute the explicit microphysical sources
!
!
    CALL RAIN_C2R2_KHKO ( HCLOUD, OACTIT, OSEDC, ORAIN, KSPLITR, PTSTEP, KMI,          &
                     HFMFILE, HLUOUT, OCLOSE_OUT, PZZ, PRHODJ, PRHODREF, PEXNREF, &
                     PPABST, PTHT, PRT(:,:,:,1), PRT(:,:,:,2),  PRT(:,:,:,3),     &
                     PTHM, PRCM, PPABSM,                                          &
                     PW_ACT, PTHS, PRS(:,:,:,1), PRS(:,:,:,2), PRS(:,:,:,3),      &
                     ZSVT(:,:,:,1), ZSVT(:,:,:,2), ZSVT(:,:,:,3),                 &
                     ZSVS(:,:,:,1), ZSVS(:,:,:,2), ZSVS(:,:,:,3),                 &
                     PINPRC, PINPRR, PINPRR3D, PEVAP3D ,                          &
                     PSVT(:,:,:,:), PSOLORG, PMI, HACTCCN           )
!
!
!*       7.2    Perform the saturation adjustment
!
   IF (LSUPSAT) THEN
    CALL KHKO_NOTADJUST (KRR, KTCOUNT,HFMFILE, HLUOUT, HRAD, OCLOSE_OUT,         &
                         PTSTEP, PRHODJ, PPABSM, PPABST, PRHODREF, PZZ,          &
                         PTHT,PRT(:,:,:,1),PRT(:,:,:,2),PRT(:,:,:,3),            &
                         PTHS,PRS(:,:,:,1),PRS(:,:,:,2),PRS(:,:,:,3),            &
                         ZSVS(:,:,:,2),ZSVS(:,:,:,1),                            &
                         ZSVS(:,:,:,4), PCLDFR, PSRCS                            )
!
   ELSE
    CALL C2R2_ADJUST ( KRR,HFMFILE, HLUOUT, HRAD,                              &
                       HTURBDIM, OCLOSE_OUT, OSUBG_COND, PTSTEP,               &
                       PRHODJ, PSIGS, PPABST,                                  &
                       PTHS=PTHS, PRVS=PRS(:,:,:,1), PRCS=PRS(:,:,:,2),        &
                       PCNUCS=ZSVS(:,:,:,1), PCCS=ZSVS(:,:,:,2),               &
                       PSRCS=PSRCS, PCLDFR=PCLDFR, PRRS=PRS(:,:,:,3)           )
!
   END IF
!
  CASE ('ICE3')
!
!*       9.     MIXED-PHASE MICROPHYSICAL SCHEME (WITH 3 ICE SPECIES)
!               -----------------------------------------------------
!
!
!*       9.1    Compute the explicit microphysical sources
!
!
    DO JK=IKB,IKE
      ZDZZ(:,:,JK)=PZZ(:,:,JK+1)-PZZ(:,:,JK)    
    ENDDO
    CALL RAIN_ICE ( OSEDIC,CSEDIM, HSUBG_AUCV, OWARM,1,IKU,1,            &
                    KSPLITR, PTSTEP, KMI, KRR,                           &
                    ZDZZ, PRHODJ, PRHODREF, PEXNREF, PPABST, PCIT,PCLDFR,&
                    PTHT, PRT(:,:,:,1), PRT(:,:,:,2),                    &
                    PRT(:,:,:,3), PRT(:,:,:,4),                          &
                    PRT(:,:,:,5), PRT(:,:,:,6),                          &
                    PTHS, PRS(:,:,:,1), PRS(:,:,:,2), PRS(:,:,:,3),      &
                    PRS(:,:,:,4), PRS(:,:,:,5), PRS(:,:,:,6),            &
                    PINPRC,PINPRC3D,PINPRR, PINPRR3D, PEVAP3D,           &
                    PINPRS,PINPRS3D, PINPRG,PINPRG3D, PSIGS,             &
                    PSPEEDC, PSPEEDR, PSPEEDS, PSPEEDG, PSPEEDH,         &
                    PSEA,PTOWN)
!
!*       9.2    Perform the saturation adjustment over cloud ice and cloud water
!
    ZZZ = MZF(1,IKU,1, PZZ )
    CALL ICE_ADJUST (1,IKU,1, KRR, KMI, HFMFILE, HLUOUT, HRAD, HTURBDIM,     &
                    OSUBG_COND, OSIGMAS, PTSTEP,PSIGQSAT,                    &
                    PRHODJ, PEXNREF,  PSIGS, PMFCONV, PPABST, ZZZ,           &
                    PCF_MF,PRC_MF,PRI_MF,                                    &   
                    PRVT=PRT(:,:,:,1), PRCT=PRT(:,:,:,2),                    &
                    PRVS=PRS(:,:,:,1), PRCS=PRS(:,:,:,2),                    &
                    PTHS=PTHS, PSRCS=PSRCS, PCLDFR=PCLDFR,                   &
                    PRRT=PRT(:,:,:,3), PRRS=PRS(:,:,:,3),                    &
                    PRIT=PRT(:,:,:,4), PRIS=PRS(:,:,:,4),                    &
                    PRST=PRT(:,:,:,5), PRSS=PRS(:,:,:,5),                    &
                    PRGT=PRT(:,:,:,6), PRGS=PRS(:,:,:,6)                     )
!
  CASE ('ICE4')
!
!*       10.    MIXED-PHASE MICROPHYSICAL SCHEME (WITH 4 ICE SPECIES)
!               -----------------------------------------------------
!
!
!*       10.1   Compute the explicit microphysical sources
!
!
    DO JK=IKB,IKE
      ZDZZ(:,:,JK)=PZZ(:,:,JK+1)-PZZ(:,:,JK)    
    ENDDO
    CALL RAIN_ICE ( OSEDIC,CSEDIM, HSUBG_AUCV, OWARM,1,IKU,1,             &
                    KSPLITR, PTSTEP, KMI, KRR,                            &
                    ZDZZ, PRHODJ, PRHODREF, PEXNREF, PPABST, PCIT, PCLDFR,&
                    PTHT, PRT(:,:,:,1), PRT(:,:,:,2),                     &
                    PRT(:,:,:,3), PRT(:,:,:,4),                           &
                    PRT(:,:,:,5), PRT(:,:,:,6),                           &
                    PTHS, PRS(:,:,:,1), PRS(:,:,:,2), PRS(:,:,:,3),       &
                    PRS(:,:,:,4), PRS(:,:,:,5), PRS(:,:,:,6),             &
                    PINPRC,PINPRC3D, PINPRR, PINPRR3D, PEVAP3D,           &
                    PINPRS,PINPRS3D, PINPRG,PINPRG3D, PSIGS,              &
                    PSPEEDC, PSPEEDR, PSPEEDS, PSPEEDG, PSPEEDH,          &
                    PSEA, PTOWN,                                          &
                    PRT(:,:,:,7),  PRS(:,:,:,7), PINPRH,PINPRH3D,OCONVHG  )

!
!*       10.2   Perform the saturation adjustment over cloud ice and cloud water
!
    ZZZ = MZF(1,IKU,1, PZZ )
    CALL ICE_ADJUST (1,IKU,1, KRR, KMI, HFMFILE, HLUOUT, HRAD, HTURBDIM,     &
                    OSUBG_COND, OSIGMAS, PTSTEP,PSIGQSAT,                    &
                    PRHODJ, PEXNREF, PSIGS, PMFCONV, PPABST, ZZZ,            &
                    PCF_MF,PRC_MF,PRI_MF,                                    &                     
                    PRVT=PRT(:,:,:,1), PRCT=PRT(:,:,:,2),                    &
                    PRVS=PRS(:,:,:,1), PRCS=PRS(:,:,:,2),                    &
                    PTHS=PTHS, PSRCS=PSRCS, PCLDFR=PCLDFR,                   &
                    PRRT=PRT(:,:,:,3), PRRS=PRS(:,:,:,3),                    &
                    PRIT=PRT(:,:,:,4), PRIS=PRS(:,:,:,4),                    &
                    PRST=PRT(:,:,:,5), PRSS=PRS(:,:,:,5),                    &
                    PRGT=PRT(:,:,:,6), PRGS=PRS(:,:,:,6),                    &
                    PRHT=PRT(:,:,:,7), PRHS=PRS(:,:,:,7)                     )
!
  CASE ('C3R5')
!
!*       11.    2-MOMENT MIXED-PHASE MICROPHYSICAL SCHEME (WITH 3 ICE SPECIES)
!               --------------------------------------------------------------
!
!
!*       11.1   Compute the explicit microphysical sources
!
    CALL RAIN_C2R2_KHKO ( HCLOUD, OACTIT, OSEDC, ORAIN, KSPLITR, PTSTEP, KMI,                  &
                     HFMFILE, HLUOUT, OCLOSE_OUT, PZZ, PRHODJ, PRHODREF, PEXNREF, &
                     PPABST, PTHT,                                                &
                     PRT(:,:,:,1), PRT(:,:,:,2),                                  &
                     PRT(:,:,:,3),                                                &
                     PTHM, PRCM, PPABSM,                                          &
                     PW_ACT, PTHS, PRS(:,:,:,1), PRS(:,:,:,2), PRS(:,:,:,3),      &
                     ZSVT(:,:,:,1), ZSVT(:,:,:,2), ZSVT(:,:,:,3),                 &
                     ZSVS(:,:,:,1), ZSVS(:,:,:,2), ZSVS(:,:,:,3),                 &
                     PINPRC, PINPRR, PINPRR3D, PEVAP3D,                           &
                     PSVT(:,:,:,:), PSOLORG, PMI, HACTCCN )
!
    CALL ICE_C1R3  ( OSEDI, OHHONI, KSPLITG, PTSTEP, KMI,                    &
                     PZZ, PRHODJ, PRHODREF, PEXNREF,                         &
                     PPABST, PW_ACT, PTHT,                            &
                     PRT(:,:,:,1), PRT(:,:,:,2), PRT(:,:,:,3),               &
                     PRT(:,:,:,4), PRT(:,:,:,5), PRT(:,:,:,6),               &
                     PTHS, PRS(:,:,:,1), PRS(:,:,:,2), PRS(:,:,:,3), &
                     PRS(:,:,:,4), PRS(:,:,:,5), PRS(:,:,:,6),               &
                                    ZSVT(:,:,:,2), ZSVT(:,:,:,3),            &
                                    ZSVT(:,:,:,4),                           &
                     ZSVS(:,:,:,1), ZSVS(:,:,:,2), ZSVS(:,:,:,3),            &
                     ZSVS(:,:,:,5), ZSVS(:,:,:,4),                           &
                     PINPRS, PINPRG                                          )
!
!
!*       11.2   Perform the saturation adjustment
!
    CALL C3R5_ADJUST ( KRR, KMI, HFMFILE, HLUOUT, HRAD,               &
                       HTURBDIM, OCLOSE_OUT, OSUBG_COND, PTSTEP,               &
                       PRHODREF, PRHODJ, PEXNREF, PSIGS, PPABST,       &
                       PRVT=PRT(:,:,:,1), PRCT=PRT(:,:,:,2), PRRT=PRT(:,:,:,3),&
                       PRIT=PRT(:,:,:,4), PRST=PRT(:,:,:,5), PRGT=PRT(:,:,:,6),&
                       PRVS=PRS(:,:,:,1), PRCS=PRS(:,:,:,2), PRRS=PRS(:,:,:,3),&
                       PRIS=PRS(:,:,:,4), PRSS=PRS(:,:,:,5), PRGS=PRS(:,:,:,6),&
                       PCCT=ZSVT(:,:,:,2), PCIT=ZSVT(:,:,:,4),                 &
                       PCNUCS=ZSVS(:,:,:,1), PCCS=ZSVS(:,:,:,2),               &
                       PINUCS=ZSVS(:,:,:,5), PCIS=ZSVS(:,:,:,4),               &
                       PTHS=PTHS, PSRCS=PSRCS, PCLDFR=PCLDFR                   )
!
END SELECT
!
!-------------------------------------------------------------------------------
!
!
!*      12.     SWITCH BACK TO THE PROGNOSTIC VARIABLES
!               ---------------------------------------
!
PTHS(:,:,:) = PTHS(:,:,:) * PRHODJ(:,:,:)
!
DO JRR = 1,KRR
  PRS(:,:,:,JRR)  = PRS(:,:,:,JRR) * PRHODJ(:,:,:)
END DO
!
IF (HCLOUD == 'C2R2' .OR. HCLOUD == 'C3R5' .OR. HCLOUD == 'KHKO') THEN
  DO JSV = 1,SIZE(ZSVS,4)
    PSVS(:,:,:,JSV+ISVBEG-1) = ZSVS(:,:,:,JSV) * PRHODJ(:,:,:)
  ENDDO
  DO JSV = 1,SIZE(ZSVT,4)
    PSVT(:,:,:,JSV+ISVBEG-1) = ZSVT(:,:,:,JSV)
  ENDDO
  DEALLOCATE(ZSVS)
  DEALLOCATE(ZSVT)
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE RESOLVED_CLOUD
