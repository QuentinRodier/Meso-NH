!MNH_LIC Copyright 1995-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!     ######spl
       MODULE MODI_RAIN_ICE
!      ####################
!
INTERFACE
      SUBROUTINE RAIN_ICE ( OSEDIC,HSEDIM, HSUBG_AUCV, OWARM, KKA, KKU, KKL,      &
                            KSPLITR, PTSTEP, KRR,                            &
                            PDZZ, PRHODJ, PRHODREF, PEXNREF, PPABST, PCIT, PCLDFR,&
                            PTHT, PRVT, PRCT, PRRT, PRIT, PRST, &
                            PRGT, PTHS, PRVS, PRCS, PRRS, PRIS, PRSS, PRGS, &
                            PINPRC,PINPRR, PINPRR3D, PEVAP3D,           &
                            PINPRS, PINPRG, PSIGS, PINDEP, PRAINFR, PSEA, PTOWN,  &
                            PRHT, PRHS, PINPRH, PFPR                        )
!
!
LOGICAL,                  INTENT(IN)    :: OSEDIC ! Switch for droplet sedim.
CHARACTER(LEN=4),         INTENT(IN)    :: HSEDIM ! Sedimentation scheme
CHARACTER(LEN=4),         INTENT(IN)    :: HSUBG_AUCV ! Switch for rc->rr Subgrid autoconversion
                                        ! Kind of Subgrid autoconversion method
LOGICAL,                  INTENT(IN)    :: OWARM   ! .TRUE. allows raindrops to
                                                   !   form by warm processes
                                                   !      (Kessler scheme)
!
INTEGER,                  INTENT(IN)    :: KKA   !near ground array index  
INTEGER,                  INTENT(IN)    :: KKU   !uppest atmosphere array index
INTEGER,                  INTENT(IN)    :: KKL   !vert. levels type 1=MNH -1=ARO
INTEGER,                  INTENT(IN)    :: KSPLITR ! Number of small time step 
                                      ! integration for  rain sedimendation
REAL,                     INTENT(IN)    :: PTSTEP  ! Double Time step
                                                   ! (single if cold start)
INTEGER,                  INTENT(IN)    :: KRR     ! Number of moist variable
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDZZ     ! Layer thikness (m)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF ! Reference Exner function
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST  ! absolute pressure at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCIT    ! Pristine ice n.c. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCLDFR  ! Cloud fraction
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRVT    ! Water vapor m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCT    ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRIT    ! Pristine ice m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRST    ! Snow/aggregate m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRGT    ! Graupel/hail m.r. at t
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PSIGS   ! Sigma_s at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS    ! Theta source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRCS    ! Cloud water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRRS    ! Rain water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRIS    ! Pristine ice m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRSS    ! Snow/aggregate m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRGS    ! Graupel m.r. source

!
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRC! Cloud instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINDEP  ! Cloud instant deposition
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRR! Rain instant precip
REAL, DIMENSION(:,:,:),INTENT(OUT)      :: PINPRR3D! Rain inst precip 3D
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PEVAP3D! Rain evap profile
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRS! Snow instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRG! Graupel instant precip
REAL, DIMENSION(:,:,:), INTENT(OUT)     :: PRAINFR! Rain fraction            
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PSEA ! Sea Mask
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PTOWN! Fraction that is town 
REAL, DIMENSION(:,:,:), OPTIONAL,  INTENT(IN)    :: PRHT    ! Hail m.r. at t
REAL, DIMENSION(:,:,:), OPTIONAL,  INTENT(INOUT) :: PRHS    ! Hail m.r. source
REAL, DIMENSION(:,:), OPTIONAL, INTENT(INOUT)     :: PINPRH! Hail instant precip
REAL, DIMENSION(:,:,:,:), OPTIONAL, INTENT(OUT)   :: PFPR ! upper-air precipitation fluxes
!
END SUBROUTINE RAIN_ICE
END INTERFACE
END MODULE MODI_RAIN_ICE
!     ######spl
      SUBROUTINE RAIN_ICE ( OSEDIC,HSEDIM, HSUBG_AUCV, OWARM, KKA, KKU, KKL,      &
                            KSPLITR, PTSTEP, KRR,                            &
                            PDZZ, PRHODJ, PRHODREF, PEXNREF, PPABST, PCIT, PCLDFR,&
                            PTHT, PRVT, PRCT, PRRT, PRIT, PRST, &
                            PRGT, PTHS, PRVS, PRCS, PRRS, PRIS, PRSS, PRGS, &
                            PINPRC,PINPRR, PINPRR3D, PEVAP3D,           &
                            PINPRS, PINPRG, PSIGS, PINDEP, PRAINFR, PSEA, PTOWN,  &
                            PRHT, PRHS, PINPRH, PFPR                        )
!     ######################################################################
!
!!****  * -  compute the explicit microphysical sources
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the slow microphysical sources
!!    which can be computed explicitly
!!
!!
!!**  METHOD
!!    ------
!!      The autoconversion computation follows Kessler (1969).
!!      The sedimentation rate is computed with a time spliting technique and
!!    an upstream scheme, written as a difference of non-advective fluxes. This
!!    source term is added to the future instant ( split-implicit process ).
!!      The others microphysical processes are evaluated at the central instant
!!    (split-explicit process ): autoconversion, accretion and rain evaporation.
!!      These last 3 terms are bounded in order not to create negative values
!!    for the water species at the future instant.
!!
!!    EXTERNAL
!!    --------
!!      None
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS
!!          JPHEXT       : Horizontal external points number
!!          JPVEXT       : Vertical external points number
!!      Module MODD_CONF :
!!          CCONF configuration of the model for the first time step
!!      Module MODD_CST
!!          XP00               ! Reference pressure
!!          XRD,XRV            ! Gaz  constant for dry air, vapor
!!          XMD,XMV            ! Molecular weight for dry air, vapor
!!          XCPD               ! Cpd (dry air)
!!          XCL                ! Cl (liquid)
!!          XCI                ! Ci (solid)
!!          XTT                ! Triple point temperature
!!          XLVTT              ! Vaporization heat constant
!!          XALPW,XBETAW,XGAMW ! Constants for saturation vapor pressure
!!                               function over liquid water
!!          XALPI,XBETAI,XGAMI ! Constants for saturation vapor pressure
!!                               function over solid ice
!!      Module MODD_BUDGET:
!!         NBUMOD       : model in which budget is calculated
!!         CBUTYPE      : type of desired budget
!!                          'CART' for cartesian box configuration
!!                          'MASK' for budget zone defined by a mask
!!                          'NONE'  ' for no budget
!!         NBUPROCCTR   : process counter used for each budget variable
!!         LBU_RTH      : logical for budget of RTH (potential temperature)
!!                        .TRUE. = budget of RTH
!!                        .FALSE. = no budget of RTH
!!         LBU_RRV      : logical for budget of RRV (water vapor)
!!                        .TRUE. = budget of RRV
!!                        .FALSE. = no budget of RRV
!!         LBU_RRC      : logical for budget of RRC (cloud water)
!!                        .TRUE. = budget of RRC
!!                        .FALSE. = no budget of RRC
!!         LBU_RRI      : logical for budget of RRI (cloud ice)
!!                        .TRUE. = budget of RRI
!!                        .FALSE. = no budget of RRI
!!         LBU_RRR      : logical for budget of RRR (rain water)
!!                        .TRUE. = budget of RRR
!!                        .FALSE. = no budget of RRR
!!         LBU_RRS      : logical for budget of RRS (aggregates)
!!                        .TRUE. = budget of RRS
!!                        .FALSE. = no budget of RRS
!!         LBU_RRG      : logical for budget of RRG (graupeln)
!!                        .TRUE. = budget of RRG
!!                        .FALSE. = no budget of RRG
!!
!!    REFERENCE
!!    ---------
!!
!!      Book1 and Book2 of documentation ( routine RAIN_ICE )
!!
!!    AUTHOR
!!    ------
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/11/95
!!      (J.Viviand) 04/02/97  debug accumulated prcipitation & convert
!!                            precipitation rate in m/s
!!      (J.-P. Pinty) 17/02/97  add budget calls
!!      (J.-P. Pinty) 17/11/97  set ice sedim. for cirrus ice, reset RCHONI
!!                              and RRHONG, reverse order for DEALLOCATE
!!      (J.-P. Pinty) 11/02/98  correction of the air dynamical viscosity and
!!                              add advance of the budget calls
!!      (J.-P. Pinty) 18/05/98  correction of the air density in the RIAUTS
!!                              process
!!      (J.-P. Pinty) 18/11/98  split the main routine
!!      (V. Masson)   18/11/98  bug in IVEC1 and IVEC2 upper limits
!!      (J. Escobar & J.-P. Pinty)
!!                    11/12/98  contains and rewrite count+pack
!!      (J. Stein & J.-P. Pinty)
!!                    14/10/99  correction for very small RIT
!!      (J. Escobar & J.-P. Pinty)
!!                    24/07/00  correction for very samll m.r. in
!!                              the sedimentation subroutine
!!      (M. Tomasini) 11/05/01  Autoconversion of rc into rr modification to take
!!                              into account the subgrid variance
!!                              (cf Redelsperger & Sommeria JAS 86)
!!      (G. Molinie)  21/05/99  bug in RRCFRIG process, RHODREF**(-1) missing
!!                              in RSRIMCG
!!      (G. Molinie & J.-P. Pinty)
!!                    21/06/99  bug in RACCS process
!!      (P. Jabouille) 27/05/04 safety test for case where esw/i(T)> pabs (~Z>40km)
!!      (J-.P. Chaboureau) 12/02/05  temperature depending ice-to-snow autocon-
!                              version threshold (Chaboureau and Pinty GRL 2006)
!!      (J.-P. Pinty) 01/01/O1  add the hail category and correction of the
!!                              wet growth rate of the graupeln
!!      (S.Remy & C.Lac) 06/06 Add the cloud sedimentation
!!      (S.Remy & C.Lac) 06/06 Sedimentation becoming the last process
!!      to settle the precipitating species created during the current time step
!!      (S.Remy & C.Lac) 06/06 Modification of the algorithm of sedimentation
!!      to settle n times the precipitating species created during Dt/n instead
!!      of Dt
!!      (C.Lac) 11/06 Optimization of the sedimentation loop for NEC
!!      (J.Escobar) 18/01/2008 Parallel Bug in Budget when IMICRO >= 1
!!                  --> Path inhibit this test by IMICRO >= 0 allway true
!!      (Y.Seity) 03/2008 Add Statistic sedimentation
!!      (Y.Seity) 10/2009 Added condition for the raindrop accretion of the aggregates
!!         into graupeln process (5.2.6) to avoid negative graupel mixing ratio
!!      (V.Masson, C.Lac) 09/2010 Correction in split sedimentation for
!!                                reproducibility
!!      (S. Riette) Oct 2010 Better vectorisation of RAIN_ICE_SEDIMENTATION_STAT
!!      (Y. Seity), 02-2012  add possibility to run with reversed vertical levels
!!      (L. Bengtsson), 02-2013 Passing in land/sea mask and town fraction in
!!                      order to use different cloud droplet number conc. over
!!                      land, sea and urban areas in the cloud sedimentation.
!!      (D. Degrauwe), 2013-11: Export upper-air precipitation fluxes PFPR.
!!      (S. Riette) Nov 2013 Protection against null sigma
!!      Juan 24/09/2012: for BUG Pgi rewrite PACK function on mode_pack_pgi
!!      (C. Lac) FIT temporal scheme : instant M removed
!!      (JP Pinty), 01-2014 : ICE4 : partial reconversion of hail to graupel
!!              July, 2015 (O.Nuissier/F.Duffourg) Add microphysics diagnostic for
!!                                      aircraft, ballon and profiler
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1
!!      C.Lac : 10/2016 : add droplet deposition
!!      C.Lac : 01/2017 : correction on droplet deposition
!!      J.Escobar : 10/2017 : for real*4 , limit exp() in RAIN_ICE_SLOW with XMNH_HUGE_12_LOG
!!      (C. Abiven, Y. Léauté, V. Seigner, S. Riette) Phasing of Turner rain subgrid param
!!      J.Escobar : 8/2018 : for real*4 , bis => limit exp() in RAIN_ICE_SLOW with XMNH_HUGE_12_LOG
!!      P.Wautelet 01/02/2019: add missing initialization for PFPR
!!                   02/2019 C.Lac add rain fraction as an output field
!!      J.Escobar 09/07/2019 : for reproductiblity MPPDB_CHECK , add missing LCHECK test in ZRHODJ de/allocate
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS
USE MODD_CST
USE MODD_CONF
USE MODD_RAIN_ICE_DESCR
USE MODD_RAIN_ICE_PARAM
USE MODD_PARAM_ICE
USE MODD_BUDGET
USE MODD_LES
USE MODI_BUDGET
USE MODI_GAMMA
USE MODE_FMWRIT
USE MODE_ll
USE MODE_MSG
!
#ifdef MNH_PGI
USE MODE_PACK_PGI
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
!
LOGICAL,                  INTENT(IN)    :: OSEDIC ! Switch for droplet sedim.
CHARACTER(LEN=4),         INTENT(IN)    :: HSEDIM ! Sedimentation scheme
CHARACTER(LEN=4),         INTENT(IN)    :: HSUBG_AUCV
                                        ! Kind of Subgrid autoconversion method
LOGICAL,                  INTENT(IN)    :: OWARM   ! .TRUE. allows raindrops to
                                                   !   form by warm processes
                                                   !      (Kessler scheme)
!
INTEGER,                  INTENT(IN)    :: KKA   !near ground array index
INTEGER,                  INTENT(IN)    :: KKU   !uppest atmosphere array index
INTEGER,                  INTENT(IN)    :: KKL   !vert. levels type 1=MNH -1=ARO
INTEGER,                  INTENT(IN)    :: KSPLITR ! Number of small time step
                                      ! integration for  rain sedimendation
REAL,                     INTENT(IN)    :: PTSTEP  ! Double Time step
                                                   ! (single if cold start)
INTEGER,                  INTENT(IN)    :: KRR     ! Number of moist variable
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDZZ    ! Layer thikness (m)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF ! Reference Exner function
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST  ! absolute pressure at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCIT    ! Pristine ice n.c. at t
REAL, DIMENSION(:,:,:),     INTENT(IN)  :: PCLDFR! Convective Mass Flux Cloud fraction
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRVT    ! Water vapor m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCT    ! Cloud water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRIT    ! Pristine ice m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRST    ! Snow/aggregate m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRGT    ! Graupel/hail m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PSIGS   ! Sigma_s at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS    ! Theta source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRCS    ! Cloud water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRRS    ! Rain water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRIS    ! Pristine ice m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRSS    ! Snow/aggregate m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRGS    ! Graupel m.r. source
!
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRC! Cloud instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINDEP  ! Cloud instant deposition
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRR! Rain instant precip
REAL, DIMENSION(:,:,:),INTENT(OUT)      :: PINPRR3D! Rain inst precip 3D
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PEVAP3D! Rain evap profile
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRS! Snow instant precip
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRG! Graupel instant precip
REAL, DIMENSION(:,:,:), INTENT(OUT)     :: PRAINFR! Rain fraction            
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PSEA ! Sea Mask
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PTOWN! Fraction that is town
REAL, DIMENSION(:,:,:), OPTIONAL,  INTENT(IN)    :: PRHT    ! Hail m.r. at t
REAL, DIMENSION(:,:,:), OPTIONAL,  INTENT(INOUT) :: PRHS    ! Hail m.r. source
REAL, DIMENSION(:,:), OPTIONAL, INTENT(INOUT)     :: PINPRH! Hail instant precip
REAL, DIMENSION(:,:,:,:), OPTIONAL, INTENT(OUT)   :: PFPR ! upper-air precipitation fluxes
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JK            ! Vertical loop index for the rain sedimentation
INTEGER :: JN            ! Temporal loop index for the rain sedimentation
INTEGER :: JJ            ! Loop index for the interpolation
INTEGER :: JI            ! Loop index for the interpolation
INTEGER :: IIB           !  Define the domain where is
INTEGER :: IIE           !  the microphysical sources have to be computed
INTEGER :: IJB           !
INTEGER :: IJE           !
INTEGER :: IKB,IKTB,IKT  !
INTEGER :: IKE,IKTE      !
!
REAL    :: ZTSPLITR      ! Small time step for rain sedimentation
!
!
INTEGER :: ISEDIMR,ISEDIMC, ISEDIMI, ISEDIMS, ISEDIMG, ISEDIMH, &
  INEGT, IMICRO ! Case number of sedimentation, T>0 (for HEN)
                ! and r_x>0 locations
INTEGER :: IGRIM, IGACC, IGDRY ! Case number of riming, accretion and dry growth
                               ! locations
INTEGER :: IGWET, IHAIL   ! wet growth locations and case number
LOGICAL, DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),SIZE(PEXNREF,3)) &
    :: GSEDIMR,GSEDIMC, GSEDIMI, GSEDIMS, GSEDIMG, GSEDIMH ! Test where to compute the SED processes
LOGICAL, DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),SIZE(PEXNREF,3)) &
                     :: GNEGT  ! Test where to compute the HEN process
LOGICAL, DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),SIZE(PEXNREF,3)) &
                     :: GMICRO ! Test where to compute all processes
LOGICAL, DIMENSION(:), ALLOCATABLE :: GRIM ! Test where to compute riming
LOGICAL, DIMENSION(:), ALLOCATABLE :: GACC ! Test where to compute accretion
LOGICAL, DIMENSION(:), ALLOCATABLE :: GDRY ! Test where to compute dry growth
LOGICAL, DIMENSION(:), ALLOCATABLE :: GWET  ! Test where to compute wet growth
LOGICAL, DIMENSION(:), ALLOCATABLE :: GHAIL ! Test where to compute hail growth
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2)):: GDEP
INTEGER, DIMENSION(:), ALLOCATABLE :: IVEC1,IVEC2       ! Vectors of indices for
                                ! interpolations
REAL,    DIMENSION(:), ALLOCATABLE :: ZVEC1,ZVEC2,ZVEC3 ! Work vectors for
                                ! interpolations
REAL,    DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),SIZE(PEXNREF,3))   &
                                  :: ZW ! work array
REAL,    DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),SIZE(PEXNREF,3))   &
      :: ZPRCS,ZPRRS,ZPRSS,ZPRGS,ZPRHS   ! Mixing ratios created during the time step
REAL,    DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),0:SIZE(PEXNREF,3)+1)   &
                                  :: ZWSED        ! sedimentation fluxes
REAL,    DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),0:SIZE(PEXNREF,3)+1)   &
                                  :: ZWSEDW1       ! sedimentation speed
REAL,    DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),0:SIZE(PEXNREF,3)+1)   &
                                  :: ZWSEDW2       ! sedimentation speed
REAL,    DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2))                   &
                                  :: ZCONC_TMP    ! Weighted concentration
REAL,    DIMENSION(SIZE(PEXNREF,1),SIZE(PEXNREF,2),SIZE(PEXNREF,3))   &
                                  :: ZT ! Temperature
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) ::  &
                                     ZRAY,   & ! Cloud Mean radius
                                     ZLBC,   & ! XLBC weighted by sea fraction
                                     ZFSEDC
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) :: ZHLC_HCF3D  ! HLCLOUDS cloud fraction in high water content part
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) :: ZHLC_LCF3D  ! HLCLOUDS cloud fraction in low water content part
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) :: ZHLC_HRC3D  ! HLCLOUDS cloud water content in high water content part
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) :: ZHLC_LRC3D  ! HLCLOUDS cloud water content in low water content
REAL, DIMENSION(:), ALLOCATABLE :: ZRVT    ! Water vapor m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRCT    ! Cloud water m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRRT    ! Rain water m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRIT    ! Pristine ice m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRST    ! Snow/aggregate m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRGT    ! Graupel m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRHT    ! Hail m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZCIT    ! Pristine ice conc. at t
!
REAL, DIMENSION(:), ALLOCATABLE :: ZRVS    ! Water vapor m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRCS    ! Cloud water m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRRS    ! Rain water m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRIS    ! Pristine ice m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRSS    ! Snow/aggregate m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRGS    ! Graupel m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRHS    ! Hail m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZTHS    ! Theta source
REAL, DIMENSION(:), ALLOCATABLE :: ZTHT    ! Potential temperature
REAL, DIMENSION(:), ALLOCATABLE :: ZTHLT   ! Liquid potential temperature
REAL, DIMENSION(:), ALLOCATABLE :: ZCRIAUTI ! Snow-to-ice autoconversion thres.
!
REAL, DIMENSION(:), ALLOCATABLE &
               :: ZRHODREF, & ! RHO Dry REFerence
                  ZRHODREFC,& ! RHO Dry REFerence
                  ZRHODREFR,& ! RHO Dry REFerence
                  ZRHODREFI,& ! RHO Dry REFerence
                  ZRHODREFS,& ! RHO Dry REFerence
                  ZRHODREFG,& ! RHO Dry REFerence
                  ZRHODREFH,& ! RHO Dry REFerence
                  ZRHODJ,   & ! RHO times Jacobian
                  ZZT,      & ! Temperature
                  ZPRES,    & ! Pressure
                  ZEXNREF,  & ! EXNer Pressure REFerence
                  ZZW,      & ! Work array
                  ZZW2,     & ! Work array
                  ZZW3,     & ! Work array
                  ZZW4,     & ! Work array
                  ZLSFACT,  & ! L_s/(Pi_ref*C_ph)
                  ZLVFACT,  & ! L_v/(Pi_ref*C_ph)
                  ZUSW,     & ! Undersaturation over water
                  ZSSI,     & ! Supersaturation over ice
                  ZLBDAR,   & ! Slope parameter of the raindrop  distribution
                  ZLBDAR_RF,& ! Slope parameter of the raindrop  distribution
                                 ! for the Rain Fraction part
                  ZLBDAS,   & ! Slope parameter of the aggregate distribution
                  ZLBDAG,   & ! Slope parameter of the graupel   distribution
                  ZLBDAH,   & ! Slope parameter of the hail      distribution
                  ZRDRYG,   & ! Dry growth rate of the graupeln
                  ZRWETG,   & ! Wet growth rate of the graupeln
                  ZAI,      & ! Thermodynamical function
                  ZCJ,      & ! Function to compute the ventilation coefficient
                  ZKA,      & ! Thermal conductivity of the air
                  ZDV,      & ! Diffusivity of water vapor in the air
                  ZSIGMA_RC,& ! Standard deviation of rc at time t
                  ZCF,      & ! Cloud fraction
                  ZRF,      & ! Rain fraction
                  ZHLC_HCF, & ! HLCLOUDS : fraction of High Cloud Fraction in grid
                  ZHLC_LCF, & ! HLCLOUDS : fraction of Low  Cloud Fraction in grid
                              !    note that ZCF = ZHLC_HCF + ZHLC_LCF
                  ZHLC_HRC, & ! HLCLOUDS : LWC that is High LWC in grid
                  ZHLC_LRC, & ! HLCLOUDS : LWC that is Low  LWC in grid
                              !    note that ZRC = ZHLC_HRC + ZHLC_LRC
                ZHLC_RCMAX, & ! HLCLOUDS : maximum value for RC in distribution
                  ZRCRAUTC, & ! RC value to begin rain formation =XCRIAUTC/RHODREF
             ZHLC_HRCLOCAL, & ! HLCLOUDS : LWC that is High LWC local in HCF
             ZHLC_LRCLOCAL, & ! HLCLOUDS : LWC that is Low  LWC local in LCF
                              !    note that ZRC/CF = ZHLC_HRCLOCAL+ ZHLC_LRCLOCAL
                              !                     = ZHLC_HRC/HCF+ ZHLC_LRC/LCF
                  ZCC,      & ! terminal velocity
                  ZFSEDC1D, & ! For cloud sedimentation
                  ZWLBDC,   & ! Slope parameter of the droplet  distribution
                  ZCONC,    & ! Concentration des aerosols
                  ZRAY1D,   & ! Mean radius
                  ZWLBDA      ! Libre parcours moyen
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZW1 ! Work arrays
REAL            :: ZTIMAUTIC,XDUMMY6,XDUMMY7
REAL            :: ZINVTSTEP
REAL, DIMENSION(SIZE(XRTMIN))     :: ZRTMIN
! XRTMIN = Minimum value for the mixing ratio
! ZRTMIN = Minimum value for the source (tendency)
!
INTEGER , DIMENSION(SIZE(GMICRO)) :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                           :: JL       ! and PACK intrinsics
REAL :: ZCOEFFRCM
!
!-------------------------------------------------------------------------------
!
!*       1.     COMPUTE THE LOOP BOUNDS
!               -----------------------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB=KKA+JPVEXT*KKL
IKE=KKU-JPVEXT*KKL
IKT=SIZE(PDZZ,3)
IKTB=1+JPVEXT
IKTE=IKT-JPVEXT
!
!
ZINVTSTEP=1./PTSTEP
!
!
!*       2.     COMPUTES THE SLOW COLD PROCESS SOURCES
!               --------------------------------------
!
CALL RAIN_ICE_NUCLEATION
!
!
!  optimization by looking for locations where
!  the microphysical fields are larger than a minimal value only !!!
!
GMICRO(:,:,:) = .FALSE.

 IF ( KRR == 7 ) THEN
  GMICRO(IIB:IIE,IJB:IJE,IKTB:IKTE) =                          &
                PRCT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(2) .OR. &
                PRRT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(3) .OR. &
                PRIT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(4) .OR. &
                PRST(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(5) .OR. &
                PRGT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(6) .OR. &
                PRHT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(7)
 ELSE IF( KRR == 6 ) THEN
  GMICRO(IIB:IIE,IJB:IJE,IKTB:IKTE) =                          &
                PRCT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(2) .OR. &
                PRRT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(3) .OR. &
                PRIT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(4) .OR. &
                PRST(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(5) .OR. &
                PRGT(IIB:IIE,IJB:IJE,IKTB:IKTE)>XRTMIN(6)
 END IF

IMICRO = COUNTJV( GMICRO(:,:,:),I1(:),I2(:),I3(:))
IF( IMICRO >= 0 ) THEN
  ALLOCATE(ZRVT(IMICRO))
  ALLOCATE(ZRCT(IMICRO))
  ALLOCATE(ZRRT(IMICRO))
  ALLOCATE(ZRIT(IMICRO))
  ALLOCATE(ZRST(IMICRO))
  ALLOCATE(ZRGT(IMICRO))
  IF ( KRR == 7 ) ALLOCATE(ZRHT(IMICRO))
  ALLOCATE(ZCIT(IMICRO))
  ALLOCATE(ZRVS(IMICRO))
  ALLOCATE(ZRCS(IMICRO))
  ALLOCATE(ZRRS(IMICRO))
  ALLOCATE(ZRIS(IMICRO))
  ALLOCATE(ZRSS(IMICRO))
  ALLOCATE(ZRGS(IMICRO))
  IF ( KRR == 7 ) ALLOCATE(ZRHS(IMICRO))
  ALLOCATE(ZTHS(IMICRO))
  ALLOCATE(ZTHT(IMICRO))
  ALLOCATE(ZTHLT(IMICRO))
  ALLOCATE(ZRHODREF(IMICRO))
  ALLOCATE(ZZT(IMICRO))
  ALLOCATE(ZPRES(IMICRO))
  ALLOCATE(ZEXNREF(IMICRO))
  ALLOCATE(ZSIGMA_RC(IMICRO))
  ALLOCATE(ZCF(IMICRO))
  ALLOCATE(ZRF(IMICRO))
  ALLOCATE(ZHLC_HCF(IMICRO))
  ALLOCATE(ZHLC_LCF(IMICRO))
  ALLOCATE(ZHLC_HRC(IMICRO))
  ALLOCATE(ZHLC_LRC(IMICRO))
  ALLOCATE(ZHLC_RCMAX(IMICRO))
  ALLOCATE(ZRCRAUTC(IMICRO))
  ALLOCATE(ZHLC_HRCLOCAL(IMICRO))
  ALLOCATE(ZHLC_LRCLOCAL(IMICRO))

  DO JL=1,IMICRO
    ZRVT(JL) = PRVT(I1(JL),I2(JL),I3(JL))
    ZRCT(JL) = PRCT(I1(JL),I2(JL),I3(JL))
    ZRRT(JL) = PRRT(I1(JL),I2(JL),I3(JL))
    ZRIT(JL) = PRIT(I1(JL),I2(JL),I3(JL))
    ZRST(JL) = PRST(I1(JL),I2(JL),I3(JL))
    ZRGT(JL) = PRGT(I1(JL),I2(JL),I3(JL))
    IF ( KRR == 7 ) ZRHT(JL) = PRHT(I1(JL),I2(JL),I3(JL))
    ZCIT(JL) = PCIT(I1(JL),I2(JL),I3(JL))
    ZCF(JL) = PCLDFR(I1(JL),I2(JL),I3(JL))
    IF ( HSUBG_AUCV == 'PDF ' .AND. CSUBG_PR_PDF == 'SIGM' ) THEN
      ZSIGMA_RC(JL) = PSIGS(I1(JL),I2(JL),I3(JL)) * 2.
!     ZSIGMA_RC(JL) = MAX(PSIGS(I1(JL),I2(JL),I3(JL)) * 2., 1.E-12)
    END IF
    ZRVS(JL) = PRVS(I1(JL),I2(JL),I3(JL))
    ZRCS(JL) = PRCS(I1(JL),I2(JL),I3(JL))
    ZRRS(JL) = PRRS(I1(JL),I2(JL),I3(JL))
    ZRIS(JL) = PRIS(I1(JL),I2(JL),I3(JL))
    ZRSS(JL) = PRSS(I1(JL),I2(JL),I3(JL))
    ZRGS(JL) = PRGS(I1(JL),I2(JL),I3(JL))
    IF ( KRR == 7 ) ZRHS(JL) = PRHS(I1(JL),I2(JL),I3(JL))
    ZTHS(JL) = PTHS(I1(JL),I2(JL),I3(JL))
!
    ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
    ZZT(JL) = ZT(I1(JL),I2(JL),I3(JL))
    ZTHT(JL) = PTHT(I1(JL),I2(JL),I3(JL))
    ZTHLT(JL) = ZTHT(JL) - XLVTT * ZTHT(JL) / XCPD / ZZT(JL) * ZRCT(JL)
    ZPRES(JL) = PPABST(I1(JL),I2(JL),I3(JL))
    ZEXNREF(JL) = PEXNREF(I1(JL),I2(JL),I3(JL))
  ENDDO
  ALLOCATE(ZZW(IMICRO))
  ALLOCATE(ZZW2(IMICRO))
  ALLOCATE(ZZW3(IMICRO))
  ALLOCATE(ZZW4(IMICRO))
  ALLOCATE(ZLSFACT(IMICRO))
  ALLOCATE(ZLVFACT(IMICRO))
    ZZW(:)  = ZEXNREF(:)*( XCPD+XCPV*ZRVT(:)+XCL*(ZRCT(:)+ZRRT(:)) &
                                    +XCI*(ZRIT(:)+ZRST(:)+ZRGT(:)) )
    ZLSFACT(:) = (XLSTT+(XCPV-XCI)*(ZZT(:)-XTT))/ZZW(:) ! L_s/(Pi_ref*C_ph)
    ZLVFACT(:) = (XLVTT+(XCPV-XCL)*(ZZT(:)-XTT))/ZZW(:) ! L_v/(Pi_ref*C_ph)
  ALLOCATE(ZUSW(IMICRO))
  ALLOCATE(ZSSI(IMICRO))
    ZZW(:) = EXP( XALPI - XBETAI/ZZT(:) - XGAMI*ALOG(ZZT(:) ) )
    ZSSI(:) = ZRVT(:)*( ZPRES(:)-ZZW(:) ) / ( (XMV/XMD) * ZZW(:) ) - 1.0
                                                      ! Supersaturation over ice
!
  ALLOCATE(ZLBDAR(IMICRO))
  ALLOCATE(ZLBDAR_RF(IMICRO))
  ALLOCATE(ZLBDAS(IMICRO))
  ALLOCATE(ZLBDAG(IMICRO))
  IF ( KRR == 7 ) ALLOCATE(ZLBDAH(IMICRO))
  ALLOCATE(ZRDRYG(IMICRO))
  ALLOCATE(ZRWETG(IMICRO))
  ALLOCATE(ZAI(IMICRO))
  ALLOCATE(ZCJ(IMICRO))
  ALLOCATE(ZKA(IMICRO))
  ALLOCATE(ZDV(IMICRO))
!
  IF ( KRR == 7 ) THEN
    ALLOCATE(ZZW1(IMICRO,7))
  ELSE IF( KRR == 6 ) THEN
    ALLOCATE(ZZW1(IMICRO,6))
  ENDIF
!
  IF (LBU_ENABLE .OR. LLES_CALL .OR. LCHECK ) THEN
    ALLOCATE(ZRHODJ(IMICRO))
    ZRHODJ(:) = PACK( PRHODJ(:,:,:),MASK=GMICRO(:,:,:) )
  END IF
!

  !Cloud water split between high and low content part is done here
  !according to autoconversion option
  ZRCRAUTC(:)   = XCRIAUTC/ZRHODREF(:) ! Autoconversion rc threshold
  IF (HSUBG_AUCV == 'NONE') THEN
    !Cloud water is entirely in low or high part
    WHERE (ZRCT(:) > ZRCRAUTC(:))
      ZHLC_HCF(:) = 1.
      ZHLC_LCF(:) = 0.0
      ZHLC_HRC(:) = ZRCT(:)
      ZHLC_LRC(:) = 0.0
      ZRF(:)      = 1.
    ELSEWHERE (ZRCT(:) > XRTMIN(2))
      ZHLC_HCF(:) = 0.0
      ZHLC_LCF(:) = 1.
      ZHLC_HRC(:) = 0.0
      ZHLC_LRC(:) = ZRCT(:)
      ZRF(:)      = 0.
    ELSEWHERE
      ZHLC_HCF(:) = 0.0
      ZHLC_LCF(:) = 0.0
      ZHLC_HRC(:) = 0.0
      ZHLC_LRC(:) = 0.0
      ZRF(:)      = 0.
    END WHERE

  ELSEIF (HSUBG_AUCV == 'CLFR') THEN
    !Cloud water is only in the cloudy part and entirely in low or high part
      WHERE (ZCF(:) > 0. .AND. ZRCT(:) > ZRCRAUTC(:)*ZCF(:))
        ZHLC_HCF(:) = ZCF(:)
        ZHLC_LCF(:) = 0.0
        ZHLC_HRC(:) = ZRCT(:)
        ZHLC_LRC(:) = 0.0
        ZRF(:)      = ZCF(:)
      ELSEWHERE (ZCF(:) > 0. .AND. ZRCT(:) > XRTMIN(2))
        ZHLC_HCF(:) = 0.0
        ZHLC_LCF(:) = ZCF(:)
        ZHLC_HRC(:) = 0.0
        ZHLC_LRC(:) = ZRCT(:)
        ZRF(:)      = 0.
      ELSEWHERE (ZCF(:) > 0.)
        ZHLC_HCF(:) = 0.0
        ZHLC_LCF(:) = 0.0
        ZHLC_HRC(:) = 0.0
        ZHLC_LRC(:) = 0.0
        ZRF(:)      = 0.
      ELSEWHERE
        ZHLC_HCF(:) = 0.0
        ZHLC_LCF(:) = 0.0
        ZHLC_HRC(:) = 0.0
        ZHLC_LRC(:) = 0.0
        ZRF(:)      = 0.
      END WHERE

  ELSEIF (HSUBG_AUCV == 'PDF ') THEN
    !Cloud water is split between high and low part according to a PDF
    !    'HLCRECTPDF'    : rectangular PDF form
    !    'HLCTRIANGPDF'  : triangular PDF form
    !    'HLCQUADRAPDF'  : second order quadratic PDF form
    !    'HLCISOTRIPDF'  : isocele triangular PDF
    !    'SIGM'          : Redelsperger and Sommeria (1986)

    IF ( CSUBG_PR_PDF == 'SIGM' ) THEN
      ! Redelsperger and Sommeria (1986) but organised according to Turner (2011, 2012)
      WHERE ( ZRCT(:) > ZRCRAUTC(:) + ZSIGMA_RC(:))
        ZHLC_HCF(:) = 1.
        ZHLC_LCF(:) = 0.0
        ZHLC_HRC(:) = ZRCT(:)
        ZHLC_LRC(:) = 0.0
        ZRF(:)      = 1.
      ELSEWHERE ( ZRCT(:) >  ( ZRCRAUTC(:) - ZSIGMA_RC(:) ) .AND. &
                & ZRCT(:) <= ( ZRCRAUTC(:) + ZSIGMA_RC(:) )       )
        ZHLC_HCF(:) = (ZRCT(:)+ZSIGMA_RC(:)-ZRCRAUTC(:))/ &
                     &(2.*ZSIGMA_RC(:))
        ZHLC_LCF(:) = MAX(0., ZCF(:)-ZHLC_HCF(:))
        ZHLC_HRC(:) = (ZRCT(:)+ZSIGMA_RC(:)-ZRCRAUTC(:))* &
                     &(ZRCT(:)+ZSIGMA_RC(:)+ZRCRAUTC(:))/ &
                     &(4.*ZSIGMA_RC(:))
        ZHLC_LRC(:) = MAX(0., ZRCT(:)-ZHLC_HRC(:))
        ZRF(:)      = ZHLC_HCF(:)
      ELSEWHERE ( ZRCT(:)>XRTMIN(2) .AND. ZCF(:)>0. )
        ZHLC_LCF(:) = 0.0
        ZHLC_LCF(:) = ZCF(:)
        ZHLC_HRC(:) = 0.0
        ZHLC_LRC(:) = ZRCT(:)
        ZRF(:)      = 0.
      ELSEWHERE
        ZHLC_HCF(:) = 0.0
        ZHLC_LCF(:) = 0.0
        ZHLC_HRC(:) = 0.0
        ZHLC_LRC(:) = 0.0
        ZRF(:)      = 0.
      END WHERE

    ! Turner (2011, 2012)
    ELSEIF ( CSUBG_PR_PDF== 'HLCRECTPDF' .OR. CSUBG_PR_PDF == 'HLCISOTRIPDF' .OR. &
           & CSUBG_PR_PDF == 'HLCTRIANGPDF' .OR. CSUBG_PR_PDF == 'HLCQUADRAPDF' ) THEN
      ! Calculate maximum value r_cM from PDF forms
      IF ( CSUBG_PR_PDF == 'HLCRECTPDF' .OR. CSUBG_PR_PDF == 'HLCISOTRIPDF' ) THEN
        ZCOEFFRCM = 2.0
      ELSE IF ( CSUBG_PR_PDF == 'HLCTRIANGPDF' ) THEN
        ZCOEFFRCM = 3.0
      ELSE IF ( CSUBG_PR_PDF == 'HLCQUADRAPDF' ) THEN
        ZCOEFFRCM = 4.0
      END IF
      WHERE (ZRCT(:).GT.0. .AND. ZCF(:).GT.0.)
        ZHLC_RCMAX(:) = ZCOEFFRCM * ZRCT(:) / ZCF(:)
      END WHERE

      ! Split available water and cloud fraction in two parts
      ! Calculate local mean values int he low and high parts for the 3 PDF forms:
      IF ( CSUBG_PR_PDF == 'HLCRECTPDF' ) THEN
        WHERE (ZRCT(:).GT.0. .AND. ZCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
          ZHLC_LRCLOCAL(:) = 0.5*ZRCRAUTC(:)
          ZHLC_HRCLOCAL(:) = ( ZHLC_RCMAX(:) + ZRCRAUTC(:)) / 2.0
        END WHERE
      ELSE IF ( CSUBG_PR_PDF == 'HLCTRIANGPDF' ) THEN
        WHERE (ZRCT(:).GT.0. .AND. ZCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
          ZHLC_LRCLOCAL(:) = ( ZRCRAUTC(:) *(3.0 * ZHLC_RCMAX(:) - 2.0 * ZRCRAUTC(:) ) ) &
                          / (3.0 * (2.0 * ZHLC_RCMAX(:) - ZRCRAUTC(:)  ) )
          ZHLC_HRCLOCAL(:) = (ZHLC_RCMAX(:) + 2.0*ZRCRAUTC(:)) / 3.0
        END WHERE
      ELSE IF ( CSUBG_PR_PDF == 'HLCQUADRAPDF' ) THEN
        WHERE (ZRCT(:).GT.0. .AND. ZCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
          ZHLC_LRCLOCAL(:) = (3.0 *ZRCRAUTC(:)**3 - 8.0 *ZRCRAUTC(:)**2 * ZHLC_RCMAX(:) &
                          + 6.0*ZRCRAUTC(:) *ZHLC_RCMAX(:)**2 ) &
                          / &
                          (4.0* ZRCRAUTC(:)**2 -12.0*ZRCRAUTC(:) *ZHLC_RCMAX(:) &
                          + 12.0 * ZHLC_RCMAX(:)**2 )
          ZHLC_HRCLOCAL(:) =  (ZHLC_RCMAX(:) + 3.0*ZRCRAUTC(:)) / 4.0
        END WHERE
      ELSE IF ( CSUBG_PR_PDF == 'HLCISOTRIPDF' ) THEN
        WHERE (ZRCT(:).GT.0. .AND. ZCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
          WHERE ( (ZRCT(:) / ZCF(:)).LE.ZRCRAUTC(:) )
            ZHLC_LRCLOCAL(:) = ( (ZHLC_RCMAX(:))**3 &
                             - (12.0 * (ZHLC_RCMAX(:))*(ZRCRAUTC(:))**2) &
                             + (8.0 * ZRCRAUTC(:)**3) ) &
                             / ( (6.0 * (ZHLC_RCMAX(:))**2) &
                             - (24.0 * (ZHLC_RCMAX(:)) * ZRCRAUTC(:)) &
                             + (12.0 * ZRCRAUTC(:)**2) )
            ZHLC_HRCLOCAL(:) = ( ZHLC_RCMAX(:) + 2.0 * ZRCRAUTC(:) ) / 3.0
          ELSEWHERE
            ZHLC_LRCLOCAL(:) = (2.0/3.0) * ZRCRAUTC(:)
            ZHLC_HRCLOCAL(:) = (3.0*ZHLC_RCMAX(:)**3 - 8.0*ZRCRAUTC(:)**3) &
                             / (6.0 * ZHLC_RCMAX(:)**2 - 12.0*ZRCRAUTC(:)**2)
          END WHERE
        END WHERE
      END IF

      ! Compare r_cM  to r_cR to know if cloud water content is high enough to split in two parts or not
      WHERE (ZRCT(:).GT.0. .AND. ZCF(:).GT.0. .AND. ZHLC_RCMAX(:).GT.ZRCRAUTC(:))
        ! Calculate final values for LCF and HCF:
        ZHLC_LCF(:) = ZCF(:) &
                       * ( ZHLC_HRCLOCAL - &
                       ( ZRCT(:) / ZCF(:) ) ) &
                       / (ZHLC_HRCLOCAL - ZHLC_LRCLOCAL)
        ZHLC_HCF(:) = MAX(0., ZCF(:) - ZHLC_LCF(:))
        !
        ! Calculate final values for LRC and HRC:
        ZHLC_LRC(:) = ZHLC_LRCLOCAL * ZHLC_LCF(:)
        ZHLC_HRC(:) = MAX(0., ZRCT(:) - ZHLC_LRC(:))
      ELSEWHERE (ZRCT(:).GT.0. .AND. ZCF(:).GT.0. .AND. ZHLC_RCMAX(:).LE.ZRCRAUTC(:))
        ! Put all available cloud water and his fraction in the low part
        ZHLC_LCF(:) = ZCF(:)
        ZHLC_HCF(:) = 0.0
        ZHLC_LRC(:) = ZRCT(:)
        ZHLC_HRC(:) = 0.0
      ELSEWHERE
        ZHLC_LCF(:) = 0.
        ZHLC_HCF(:) = 0.0
        ZHLC_LRC(:) = 0.
        ZHLC_HRC(:) = 0.0
      END WHERE

      ZRF(:)=ZHLC_HCF(:) !Precipitation fraction

    ELSE
      !wrong CSUBG_PR_PDF case
      WRITE(*,*) 'wrong CSUBG_PR_PDF case'
      CALL PRINT_MSG(NVERB_FATAL,'GEN','RAIN_ICE','')      
    ENDIF
  ELSE
    !wrong HSUBG_AUCV case
    WRITE(*,*)'wrong HSUBG_AUCV case'
    CALL PRINT_MSG(NVERB_FATAL,'GEN','RAIN_ICE','')  
  ENDIF

  !Diagnostic of precipitation fraction
  ZW(:,:,:) = 0.
  PRAINFR(:,:,:) = UNPACK( ZRF(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  CALL RAINFR_VERT(PRAINFR(:,:,:), PRRT(:,:,:))
  DO JL=1,IMICRO
    ZRF(JL)=PRAINFR(I1(JL),I2(JL),I3(JL))
  END DO
!
  CALL RAIN_ICE_SLOW
!
!-------------------------------------------------------------------------------
!
!
!*       3.     COMPUTES THE SLOW WARM PROCESS SOURCES
!               --------------------------------------
!
!*       3.1    compute the slope parameter Lbda_r
!
  !ZLBDAR will be used when we consider rain diluted over the grid box
  WHERE( ZRRT(:)>0.0 )
    ZLBDAR(:)  = XLBR*( ZRHODREF(:)*MAX( ZRRT(:),XRTMIN(3) ) )**XLBEXR
  END WHERE
  !ZLBDAR_RF will be used when we consider rain concentrated in its fraction
  WHERE( ZRRT(:)>0.0 .AND. ZRF(:)>0.0 )
    ZLBDAR_RF(:)  = XLBR*( ZRHODREF(:) *MAX( ZRRT(:)/ZRF(:)  , XRTMIN(3) ) )**XLBEXR
  ELSEWHERE
    ZLBDAR_RF(:)  = 0.
  END WHERE
!
  IF( OWARM ) THEN    !  Check if the formation of the raindrops by the slow
                      !  warm processes is allowed
    PEVAP3D(:,:,:)= 0.
    CALL RAIN_ICE_WARM
  END IF
!
!-------------------------------------------------------------------------------
!
!
!*       4.     COMPUTES THE FAST COLD PROCESS SOURCES FOR r_s
!               ----------------------------------------------
!
  CALL RAIN_ICE_FAST_RS
!
!-------------------------------------------------------------------------------
!
!
!*       5.     COMPUTES THE FAST COLD PROCESS SOURCES FOR r_g
!               ----------------------------------------------
!
  CALL RAIN_ICE_FAST_RG
!
!-------------------------------------------------------------------------------
!
!
!*       6.     COMPUTES THE FAST COLD PROCESS SOURCES FOR r_h
!               ----------------------------------------------
!
 IF ( KRR == 7 ) THEN
  CALL RAIN_ICE_FAST_RH
 END IF
!
!-------------------------------------------------------------------------------
!
!
!*       7.     COMPUTES SPECIFIC SOURCES OF THE WARM AND COLD CLOUDY SPECIES
!               -------------------------------------------------------------
!
  CALL RAIN_ICE_FAST_RI
!
!
!-------------------------------------------------------------------------------
!
!
!
  ZW(:,:,:) = PRVS(:,:,:)
  PRVS(:,:,:) = UNPACK( ZRVS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PRCS(:,:,:)
  PRCS(:,:,:) = UNPACK( ZRCS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PRRS(:,:,:)
  PRRS(:,:,:) = UNPACK( ZRRS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PRIS(:,:,:)
  PRIS(:,:,:) = UNPACK( ZRIS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PRSS(:,:,:)
  PRSS(:,:,:) = UNPACK( ZRSS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PRGS(:,:,:)
  PRGS(:,:,:) = UNPACK( ZRGS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  IF ( KRR == 7 ) THEN
    ZW(:,:,:) = PRHS(:,:,:)
    PRHS(:,:,:) = UNPACK( ZRHS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  END IF
  ZW(:,:,:) = PTHS(:,:,:)
  PTHS(:,:,:) = UNPACK( ZTHS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PCIT(:,:,:)
  PCIT(:,:,:) = UNPACK( ZCIT(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
!
  ZW(:,:,:) = PRAINFR(:,:,:)
  PRAINFR(:,:,:) = UNPACK( ZRF(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
!
  ZW(:,:,:) = 0.
  ZHLC_HCF3D(:,:,:) = UNPACK( ZHLC_HCF(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
!
  ZW(:,:,:) = 0.
  ZHLC_LCF3D(:,:,:) = UNPACK( ZHLC_LCF(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
!
  ZW(:,:,:) = 0.
  ZHLC_HRC3D(:,:,:) = UNPACK( ZHLC_HRC(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
!
  ZW(:,:,:) = 0.
  ZHLC_LRC3D(:,:,:) = UNPACK( ZHLC_LRC(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
!
!
  DEALLOCATE(ZZW1)
  DEALLOCATE(ZDV)
  DEALLOCATE(ZCJ)
  DEALLOCATE(ZRDRYG)
  DEALLOCATE(ZRWETG)
  DEALLOCATE(ZLBDAG)
  IF ( KRR == 7 ) DEALLOCATE(ZLBDAH)
  DEALLOCATE(ZLBDAS)
  DEALLOCATE(ZLBDAR)
  DEALLOCATE(ZLBDAR_RF)
  DEALLOCATE(ZSSI)
  DEALLOCATE(ZUSW)
  DEALLOCATE(ZLVFACT)
  DEALLOCATE(ZLSFACT)
  DEALLOCATE(ZZW)
  DEALLOCATE(ZZW2)
  DEALLOCATE(ZZW3)
  DEALLOCATE(ZZW4)
  DEALLOCATE(ZEXNREF)
  DEALLOCATE(ZPRES)
  DEALLOCATE(ZRHODREF)
  DEALLOCATE(ZZT)
  IF(LBU_ENABLE .OR. LLES_CALL .OR. LCHECK ) DEALLOCATE(ZRHODJ)
  DEALLOCATE(ZTHS)
  DEALLOCATE(ZTHT)
  DEALLOCATE(ZTHLT)
  IF ( KRR == 7 ) DEALLOCATE(ZRHS)
  DEALLOCATE(ZRGS)
  DEALLOCATE(ZRSS)
  DEALLOCATE(ZRIS)
  DEALLOCATE(ZRRS)
  DEALLOCATE(ZRCS)
  DEALLOCATE(ZRVS)
  DEALLOCATE(ZCIT)
  DEALLOCATE(ZRGT)
  IF ( KRR == 7 ) DEALLOCATE(ZRHT)
  DEALLOCATE(ZRST)
  DEALLOCATE(ZRIT)
  DEALLOCATE(ZRRT)
  DEALLOCATE(ZAI)
  DEALLOCATE(ZRCT)
  DEALLOCATE(ZKA)
  DEALLOCATE(ZRVT)
  DEALLOCATE(ZSIGMA_RC)
  DEALLOCATE(ZCF)
  DEALLOCATE(ZRF)
  DEALLOCATE(ZHLC_HCF)
  DEALLOCATE(ZHLC_LCF)
  DEALLOCATE(ZHLC_HRC)
  DEALLOCATE(ZHLC_LRC)
  DEALLOCATE(ZHLC_RCMAX)
  DEALLOCATE(ZRCRAUTC)
  DEALLOCATE(ZHLC_HRCLOCAL)
  DEALLOCATE(ZHLC_LRCLOCAL)
!
  ELSE
!
! Advance the budget calls
!
! Reordered for compability with flexible structures like in AROME

 ! rain_ice_slow
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'HON_BU_RTH')
 IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'HON_BU_RRC')
 IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'HON_BU_RRI')
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'SFR_BU_RTH')
 IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'SFR_BU_RRR')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'SFR_BU_RRG')
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'DEPS_BU_RTH')
 IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6,'DEPS_BU_RRV')
 IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'DEPS_BU_RRS')
 IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'AGGS_BU_RRI')
 IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'AGGS_BU_RRS')
 IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'AUTS_BU_RRI')
 IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'AUTS_BU_RRS')
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'DEPG_BU_RTH')
 IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6,'DEPG_BU_RRV')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'DEPG_BU_RRG')

 IF (OWARM) THEN ! rain_ice_warm
   IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'AUTO_BU_RRC')
   IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'AUTO_BU_RRR')
   IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'ACCR_BU_RRC')
   IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'ACCR_BU_RRR')
   IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'REVA_BU_RTH')
   IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6,'REVA_BU_RRV')
   IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'REVA_BU_RRR')
 ENDIF

 !rain_ice_fast_rs
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'RIM_BU_RTH')
 IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'RIM_BU_RRC')
 IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'RIM_BU_RRS')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'RIM_BU_RRG')
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'ACC_BU_RTH')
 IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'ACC_BU_RRR')
 IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'ACC_BU_RRS')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'ACC_BU_RRG')
 IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'CMEL_BU_RRS')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'CMEL_BU_RRG')

 !rain_ice_fast_rg
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'CFRZ_BU_RTH')
 IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'CFRZ_BU_RRR')
 IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'CFRZ_BU_RRI')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'CFRZ_BU_RRG')
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'WETG_BU_RTH')
 IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'WETG_BU_RRC')
 IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'WETG_BU_RRR')
 IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'WETG_BU_RRI')
 IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'WETG_BU_RRS')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'WETG_BU_RRG')
 IF (LBUDGET_RH) CALL BUDGET (PRHS(:,:,:)*PRHODJ(:,:,:),12,'WETG_BU_RRH')
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'DRYG_BU_RTH')
 IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'DRYG_BU_RRC')
 IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'DRYG_BU_RRR')
 IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'DRYG_BU_RRI')
 IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'DRYG_BU_RRS')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'DRYG_BU_RRG')
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'GMLT_BU_RTH')
 IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'GMLT_BU_RRR')
 IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'GMLT_BU_RRG')

 IF(KRR==7) THEN ! rain_ice_fast_rh
   IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'WETH_BU_RTH')
   IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'WETH_BU_RRC')
   IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'WETH_BU_RRR')
   IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'WETH_BU_RRI')
   IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'WETH_BU_RRS')
   IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'WETH_BU_RRG')
   IF (LBUDGET_RH) CALL BUDGET (PRHS(:,:,:)*PRHODJ(:,:,:),12,'WETH_BU_RRH')
   IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'HMLT_BU_RTH')
   IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8,'HMLT_BU_RRR')
   IF (LBUDGET_RH) CALL BUDGET (PRHS(:,:,:)*PRHODJ(:,:,:),12,'HMLT_BU_RRH')
 ENDIF

 !rain_ice_fast_ri
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'IMLT_BU_RTH')
 IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'IMLT_BU_RRC')
 IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'IMLT_BU_RRI')
 IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'BERFI_BU_RTH')
 IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'BERFI_BU_RRC')
 IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'BERFI_BU_RRI')
!
END IF
!
!-------------------------------------------------------------------------------
!
!*       8.     COMPUTE THE SEDIMENTATION (RS) SOURCE
!               -------------------------------------
!
!*       8.1    time splitting loop initialization
!
ZTSPLITR= PTSTEP / FLOAT(KSPLITR)
!
!
IF (HSEDIM == 'STAT') THEN
  CALL RAIN_ICE_SEDIMENTATION_STAT
ELSEIF (HSEDIM == 'SPLI') THEN
  CALL RAIN_ICE_SEDIMENTATION_SPLIT
ELSE
  WRITE(*,*) ' STOP'
  WRITE(*,*) ' NO SEDIMENTATION SCHEME FOR HSEDIM=',HSEDIM
  CALL PRINT_MSG(NVERB_FATAL,'GEN','RAIN_ICE','')  
END IF
!sedimentation of rain fraction
CALL RAINFR_VERT(PRAINFR, PRRS(:,:,:)*PTSTEP)

!
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
!
CONTAINS
!
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE RAIN_ICE_SEDIMENTATION_SPLIT
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
!
INTEGER , DIMENSION(SIZE(GSEDIMC)) :: IC1,IC2,IC3 ! Used to replace the COUNT
INTEGER , DIMENSION(SIZE(GSEDIMR)) :: IR1,IR2,IR3 ! Used to replace the COUNT
INTEGER , DIMENSION(SIZE(GSEDIMS)) :: IS1,IS2,IS3 ! Used to replace the COUNT
INTEGER , DIMENSION(SIZE(GSEDIMI)) :: II1,II2,II3 ! Used to replace the COUNT
INTEGER , DIMENSION(SIZE(GSEDIMG)) :: IG1,IG2,IG3 ! Used to replace the COUNT
INTEGER , DIMENSION(SIZE(GSEDIMH)) :: IH1,IH2,IH3 ! Used to replace the COUNT
INTEGER   :: ILENALLOCC,ILENALLOCR,ILENALLOCI,ILENALLOCS,ILENALLOCG,ILENALLOCH
INTEGER   :: ILISTLENC,ILISTLENR,ILISTLENI,ILISTLENS,ILISTLENG,ILISTLENH
INTEGER, ALLOCATABLE :: ILISTR(:),ILISTC(:),ILISTI(:),ILISTS(:),ILISTG(:),ILISTH(:)
! Optimization for NEC
!INTEGER, SAVE :: IOLDALLOCC = SIZE(PEXNREF,1)*SIZE(PEXNREF,2)*SIZE(PEXNREF,3)/10
!INTEGER, SAVE :: IOLDALLOCR = SIZE(PEXNREF,1)*SIZE(PEXNREF,2)*SIZE(PEXNREF,3)/10
!INTEGER, SAVE :: IOLDALLOCI = SIZE(PEXNREF,1)*SIZE(PEXNREF,2)*SIZE(PEXNREF,3)/10
!INTEGER, SAVE :: IOLDALLOCS = SIZE(PEXNREF,1)*SIZE(PEXNREF,2)*SIZE(PEXNREF,3)/10
!INTEGER, SAVE :: IOLDALLOCG = SIZE(PEXNREF,1)*SIZE(PEXNREF,2)*SIZE(PEXNREF,3)/10
!INTEGER, SAVE :: IOLDALLOCH = SIZE(PEXNREF,1)*SIZE(PEXNREF,2)*SIZE(PEXNREF,3)/10
INTEGER, SAVE :: IOLDALLOCC = 6000
INTEGER, SAVE :: IOLDALLOCR = 6000
INTEGER, SAVE :: IOLDALLOCI = 6000
INTEGER, SAVE :: IOLDALLOCS = 6000
INTEGER, SAVE :: IOLDALLOCG = 6000
INTEGER, SAVE :: IOLDALLOCH = 6000
!
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) :: ZCONC3D !  droplet condensation
!-------------------------------------------------------------------------------
!
!
!        O. Initialization of for sedimentation
!
IF (OSEDIC) PINPRC (:,:) = 0.
IF (LDEPOSC) PINDEP (:,:) = 0.
PINPRR (:,:) = 0.
PINPRR3D (:,:,:) = 0.
PINPRS (:,:) = 0.
PINPRG (:,:) = 0.
IF ( KRR == 7 ) PINPRH (:,:) = 0.
IF (PRESENT(PFPR)) PFPR(:,:,:,:) = 0.
!
!*       1. Parameters for cloud sedimentation
!
   IF (OSEDIC) THEN
    ZRAY(:,:,:)   = 0.
    ZLBC(:,:,:)   = XLBC(1)
    ZFSEDC(:,:,:) = XFSEDC(1)
    ZCONC3D(:,:,:)= XCONC_LAND
    ZCONC_TMP(:,:)= XCONC_LAND
    IF (PRESENT(PSEA)) THEN 
      ZCONC_TMP(:,:)=PSEA(:,:)*XCONC_SEA+(1.-PSEA(:,:))*XCONC_LAND

      DO JK=IKTB,IKTE
        ZLBC(:,:,JK)   = PSEA(:,:)*XLBC(2)+(1.-PSEA(:,:))*XLBC(1)
        ZFSEDC(:,:,JK) = (PSEA(:,:)*XFSEDC(2)+(1.-PSEA(:,:))*XFSEDC(1))
        ZFSEDC(:,:,JK) = MAX(MIN(XFSEDC(1),XFSEDC(2)),ZFSEDC(:,:,JK))
        ZCONC3D(:,:,JK)= (1.-PTOWN(:,:))*ZCONC_TMP(:,:)+PTOWN(:,:)*XCONC_URBAN
        ZRAY(:,:,JK)   = 0.5*((1.-PSEA(:,:))*GAMMA(XNUC+1.0/XALPHAC)/(GAMMA(XNUC)) + &
                PSEA(:,:)*GAMMA(XNUC2+1.0/XALPHAC2)/(GAMMA(XNUC2)))
      END DO
    ELSE
        ZCONC3D(:,:,:) = XCONC_LAND
        ZRAY(:,:,:)  = 0.5*(GAMMA(XNUC+1.0/XALPHAC)/(GAMMA(XNUC)))
    END IF
    ZRAY(:,:,:)      = MAX(1.,ZRAY(:,:,:))
    ZLBC(:,:,:)      = MAX(MIN(XLBC(1),XLBC(2)),ZLBC(:,:,:))
   ENDIF
!
!*       2.    compute the fluxes
!
!  optimization by looking for locations where
!  the precipitating fields are larger than a minimal value only !!!
!  For optimization we consider each variable separately

ZRTMIN(:)    = XRTMIN(:) * ZINVTSTEP
IF (OSEDIC) GSEDIMC(:,:,:) = .FALSE.
GSEDIMR(:,:,:) = .FALSE.
GSEDIMI(:,:,:) = .FALSE.
GSEDIMS(:,:,:) = .FALSE.
GSEDIMG(:,:,:) = .FALSE.
IF ( KRR == 7 ) GSEDIMH(:,:,:) = .FALSE.
!
ILENALLOCR = 0
IF (OSEDIC) ILENALLOCC = 0
ILENALLOCI = 0
ILENALLOCS = 0
ILENALLOCG = 0
IF ( KRR == 7 ) ILENALLOCH = 0
!
! ZPiS = Specie i source creating during the current time step
! PRiS = Source of the previous time step
!
IF (OSEDIC) THEN
  ZPRCS(:,:,:) = 0.0
  ZPRCS(:,:,:) = PRCS(:,:,:)-PRCT(:,:,:)* ZINVTSTEP
  PRCS(:,:,:)  = PRCT(:,:,:)* ZINVTSTEP
END IF
ZPRRS(:,:,:) = 0.0
ZPRSS(:,:,:) = 0.0
ZPRGS(:,:,:) = 0.0
IF ( KRR == 7 ) ZPRHS(:,:,:) = 0.0
!
ZPRRS(:,:,:) = PRRS(:,:,:)-PRRT(:,:,:)* ZINVTSTEP
ZPRSS(:,:,:) = PRSS(:,:,:)-PRST(:,:,:)* ZINVTSTEP
ZPRGS(:,:,:) = PRGS(:,:,:)-PRGT(:,:,:)* ZINVTSTEP
IF ( KRR == 7 ) ZPRHS(:,:,:) = PRHS(:,:,:)-PRHT(:,:,:)* ZINVTSTEP
PRRS(:,:,:)  = PRRT(:,:,:)* ZINVTSTEP
PRSS(:,:,:)  = PRST(:,:,:)* ZINVTSTEP
PRGS(:,:,:)  = PRGT(:,:,:)* ZINVTSTEP
IF ( KRR == 7 ) PRHS(:,:,:)  = PRHT(:,:,:)* ZINVTSTEP
!
! PRiS = Source of the previous time step + source created during the subtime
! step
!
DO JN = 1 , KSPLITR
  IF( JN==1 ) THEN
   IF (OSEDIC) PRCS(:,:,:) = PRCS(:,:,:) + ZPRCS(:,:,:)/KSPLITR
   PRRS(:,:,:) = PRRS(:,:,:) + ZPRRS(:,:,:)/KSPLITR
   PRSS(:,:,:) = PRSS(:,:,:) + ZPRSS(:,:,:)/KSPLITR
   PRGS(:,:,:) = PRGS(:,:,:) + ZPRGS(:,:,:)/KSPLITR
   IF ( KRR == 7 ) PRHS(:,:,:) = PRHS(:,:,:) + ZPRHS(:,:,:)/KSPLITR
   DO JK = IKTB , IKTE
     ZW(:,:,JK) =ZTSPLITR/(PRHODREF(:,:,JK)* PDZZ(:,:,JK))
   END DO
 ELSE
   IF (OSEDIC) PRCS(:,:,:) = PRCS(:,:,:) + ZPRCS(:,:,:)*ZTSPLITR
   PRRS(:,:,:) = PRRS(:,:,:) + ZPRRS(:,:,:)*ZTSPLITR
   PRSS(:,:,:) = PRSS(:,:,:) + ZPRSS(:,:,:)*ZTSPLITR
   PRGS(:,:,:) = PRGS(:,:,:) + ZPRGS(:,:,:)*ZTSPLITR
   IF ( KRR == 7 ) PRHS(:,:,:) = PRHS(:,:,:) + ZPRHS(:,:,:)*ZTSPLITR
 END IF
 !
 IF (OSEDIC) GSEDIMC(IIB:IIE,IJB:IJE,IKTB:IKTE) =                &
                  PRCS(IIB:IIE,IJB:IJE,IKTB:IKTE)>ZRTMIN(2)
 GSEDIMR(IIB:IIE,IJB:IJE,IKTB:IKTE) =                            &
                  PRRS(IIB:IIE,IJB:IJE,IKTB:IKTE)>ZRTMIN(3)
 GSEDIMI(IIB:IIE,IJB:IJE,IKTB:IKTE) =                            &
                  PRIS(IIB:IIE,IJB:IJE,IKTB:IKTE)>ZRTMIN(4)
 GSEDIMS(IIB:IIE,IJB:IJE,IKTB:IKTE) =                            &
                  PRSS(IIB:IIE,IJB:IJE,IKTB:IKTE)>ZRTMIN(5)
 GSEDIMG(IIB:IIE,IJB:IJE,IKTB:IKTE) =                            &
                  PRGS(IIB:IIE,IJB:IJE,IKTB:IKTE)>ZRTMIN(6)
 IF ( KRR == 7 ) GSEDIMH(IIB:IIE,IJB:IJE,IKTB:IKTE) =            &
                  PRHS(IIB:IIE,IJB:IJE,IKTB:IKTE)>ZRTMIN(7)
!
 IF (OSEDIC) ISEDIMC = COUNTJV( GSEDIMC(:,:,:),IC1(:),IC2(:),IC3(:))
 ISEDIMR = COUNTJV( GSEDIMR(:,:,:),IR1(:),IR2(:),IR3(:))
 ISEDIMI = COUNTJV( GSEDIMI(:,:,:),II1(:),II2(:),II3(:))
 ISEDIMS = COUNTJV( GSEDIMS(:,:,:),IS1(:),IS2(:),IS3(:))
 ISEDIMG = COUNTJV( GSEDIMG(:,:,:),IG1(:),IG2(:),IG3(:))
 IF ( KRR == 7 ) ISEDIMH = COUNTJV( GSEDIMH(:,:,:),IH1(:),IH2(:),IH3(:))
!
!*       2.1   for cloud
!
 IF (OSEDIC) THEN
  ZWSED(:,:,:) = 0.
  IF( JN==1 ) PRCS(:,:,:) = PRCS(:,:,:) * PTSTEP
  IF( ISEDIMC >= 1 ) THEN
    IF ( ISEDIMC .GT. ILENALLOCC ) THEN
      IF ( ILENALLOCC .GT. 0 ) THEN
        DEALLOCATE (ZRCS, ZRHODREFC, ILISTC,ZWLBDC,ZCONC,ZRCT,  &
                    ZZT,ZPRES,ZRAY1D,ZFSEDC1D,ZWLBDA,ZCC )
      END IF
      ILENALLOCC = MAX (IOLDALLOCC, 2*ISEDIMC )
      IOLDALLOCC = ILENALLOCC
      ALLOCATE(ZRCS(ILENALLOCC), ZRHODREFC(ILENALLOCC), ILISTC(ILENALLOCC), &
        ZWLBDC(ILENALLOCC), ZCONC(ILENALLOCC), ZRCT(ILENALLOCC), ZZT(ILENALLOCC), &
        ZPRES(ILENALLOCC), ZRAY1D(ILENALLOCC), ZFSEDC1D(ILENALLOCC), &
        ZWLBDA(ILENALLOCC), ZCC(ILENALLOCC)  )
    END IF
!
    DO JL=1,ISEDIMC
      ZRCS(JL) = PRCS(IC1(JL),IC2(JL),IC3(JL))
      ZRHODREFC(JL) =  PRHODREF(IC1(JL),IC2(JL),IC3(JL))
      ZWLBDC(JL) = ZLBC(IC1(JL),IC2(JL),IC3(JL))
      ZCONC(JL) = ZCONC3D(IC1(JL),IC2(JL),IC3(JL))
      ZRCT(JL) = PRCT(IC1(JL),IC2(JL),IC3(JL))
      ZZT(JL) = PTHT(IC1(JL),IC2(JL),IC3(JL))
      ZPRES(JL) = PPABST(IC1(JL),IC2(JL),IC3(JL))
      ZRAY1D(JL) = ZRAY(IC1(JL),IC2(JL),IC3(JL))
      ZFSEDC1D(JL) = ZFSEDC(IC1(JL),IC2(JL),IC3(JL))
    END DO
!
    ILISTLENC = 0
    DO JL=1,ISEDIMC
     IF( ZRCS(JL) .GT. ZRTMIN(2) ) THEN
       ILISTLENC = ILISTLENC + 1
       ILISTC(ILISTLENC) = JL
     END IF
    END DO
       DO JJ = 1, ILISTLENC
          JL = ILISTC(JJ)
          IF (ZRCS(JL) .GT. ZRTMIN(2) .AND. ZRCT(JL) .GT. XRTMIN(2)) THEN
            ZWLBDC(JL) = ZWLBDC(JL) * ZCONC(JL) / (ZRHODREFC(JL) * ZRCT(JL))
            ZWLBDC(JL) = ZWLBDC(JL)**XLBEXC
            ZRAY1D(JL) = ZRAY1D(JL) / ZWLBDC(JL) !! ZRAY : mean diameter=M(1)/2
            ZZT(JL)    = ZZT(JL) * (ZPRES(JL)/XP00)**(XRD/XCPD)
            ZWLBDA(JL) = 6.6E-8*(101325./ZPRES(JL))*(ZZT(JL)/293.15)
            ZCC(JL)    = XCC*(1.+1.26*ZWLBDA(JL)/ZRAY1D(JL)) !! XCC modified for cloud
            ZWSED (IC1(JL),IC2(JL),IC3(JL))= ZRHODREFC(JL)**(-XCEXVT +1 ) *   &
              ZWLBDC(JL)**(-XDC)*ZCC(JL)*ZFSEDC1D(JL) * ZRCS(JL)
          END IF
       END DO
  END IF
       DO JK = IKTB , IKTE
         PRCS(:,:,JK) = PRCS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
       END DO
       IF (PRESENT(PFPR)) THEN
         DO JK = IKTB , IKTE
           PFPR(:,:,JK,2)=ZWSED(:,:,JK)
         ENDDO
       ENDIF
      PINPRC(:,:) = PINPRC(:,:) + ZWSED(:,:,IKB) / XRHOLW / KSPLITR
      IF( JN==KSPLITR ) THEN
        PRCS(:,:,:) = PRCS(:,:,:) * ZINVTSTEP
      END IF
 END IF
!
!*       2.2   for rain
!
  IF( JN==1 ) PRRS(:,:,:) = PRRS(:,:,:) * PTSTEP
  ZWSED(:,:,:) = 0.
  IF( ISEDIMR >= 1 ) THEN
    IF ( ISEDIMR .GT. ILENALLOCR ) THEN
      IF ( ILENALLOCR .GT. 0 ) THEN
        DEALLOCATE (ZRRS, ZRHODREFR, ILISTR)
      END IF
      ILENALLOCR = MAX (IOLDALLOCR, 2*ISEDIMR )
      IOLDALLOCR = ILENALLOCR
      ALLOCATE(ZRRS(ILENALLOCR), ZRHODREFR(ILENALLOCR), ILISTR(ILENALLOCR))
    END IF
!
    DO JL=1,ISEDIMR
      ZRRS(JL) = PRRS(IR1(JL),IR2(JL),IR3(JL))
      ZRHODREFR(JL) =  PRHODREF(IR1(JL),IR2(JL),IR3(JL))
    END DO
!
    ILISTLENR = 0
    DO JL=1,ISEDIMR
     IF( ZRRS(JL) .GT. ZRTMIN(3) ) THEN
       ILISTLENR = ILISTLENR + 1
       ILISTR(ILISTLENR) = JL
     END IF
    END DO
       DO JJ = 1, ILISTLENR
          JL = ILISTR(JJ)
           ZWSED (IR1(JL),IR2(JL),IR3(JL))= XFSEDR  * ZRRS(JL)**XEXSEDR *   &
                                        ZRHODREFR(JL)**(XEXSEDR-XCEXVT)
       END DO
  END IF
       DO JK = IKTB , IKTE
         PRRS(:,:,JK) = PRRS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
       END DO
       IF (PRESENT(PFPR)) THEN
         DO JK = IKTB , IKTE
           PFPR(:,:,JK,3)=ZWSED(:,:,JK)
         ENDDO
       ENDIF
       PINPRR(:,:) = PINPRR(:,:) + ZWSED(:,:,IKB)/XRHOLW/KSPLITR
       PINPRR3D(:,:,:) = PINPRR3D(:,:,:) + ZWSED(:,:,1:IKT)/XRHOLW/KSPLITR
      IF( JN==KSPLITR ) THEN
        PRRS(:,:,:) = PRRS(:,:,:) * ZINVTSTEP
      END IF
!
!*       2.3   for pristine ice
!
  IF( JN==1 ) PRIS(:,:,:) = PRIS(:,:,:) * PTSTEP
  ZWSED(:,:,:) = 0.
  IF( ISEDIMI >= 1 ) THEN
    IF ( ISEDIMI .GT. ILENALLOCI ) THEN
      IF ( ILENALLOCI .GT. 0 ) THEN
        DEALLOCATE (ZRIS, ZRHODREFI, ILISTI)
      END IF
      ILENALLOCI = MAX (IOLDALLOCI, 2*ISEDIMI )
      IOLDALLOCI = ILENALLOCI
      ALLOCATE(ZRIS(ILENALLOCI), ZRHODREFI(ILENALLOCI), ILISTI(ILENALLOCI))
    END IF
!
    DO JL=1,ISEDIMI
      ZRIS(JL) = PRIS(II1(JL),II2(JL),II3(JL))
      ZRHODREFI(JL) =  PRHODREF(II1(JL),II2(JL),II3(JL))
    END DO
!
    ILISTLENI = 0
    DO JL=1,ISEDIMI
     IF( ZRIS(JL) .GT.  MAX(ZRTMIN(4),1.0E-7 )) THEN ! limitation of the McF&H formula
       ILISTLENI = ILISTLENI + 1
       ILISTI(ILISTLENI) = JL
     END IF
    END DO
       DO JJ = 1, ILISTLENI
          JL = ILISTI(JJ)
              ZWSED (II1(JL),II2(JL),II3(JL))= XFSEDI * ZRIS(JL) *  &
                               ZRHODREFI(JL)**(1.0-XCEXVT) * & !    McF&H
                               MAX( 0.05E6,-0.15319E6-0.021454E6* &
                               ALOG(ZRHODREFI(JL)*ZRIS(JL)) )**XEXCSEDI
       END DO
  END IF
       DO JK = IKTB , IKTE
         PRIS(:,:,JK) = PRIS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
       END DO
       IF (PRESENT(PFPR)) THEN
         DO JK = IKTB , IKTE
           PFPR(:,:,JK,4)=ZWSED(:,:,JK)
         ENDDO
       ENDIF
      IF( JN==KSPLITR ) THEN
        PRIS(:,:,:) = PRIS(:,:,:) * ZINVTSTEP
      END IF
!
!*       2.4   for aggregates/snow
!
  IF( JN==1 ) PRSS(:,:,:) = PRSS(:,:,:) * PTSTEP
  ZWSED(:,:,:) = 0.
  IF( ISEDIMS >= 1 ) THEN
    IF ( ISEDIMS .GT. ILENALLOCS ) THEN
      IF ( ILENALLOCS .GT. 0 ) THEN
        DEALLOCATE (ZRSS, ZRHODREFS, ILISTS)
      END IF
      ILENALLOCS = MAX (IOLDALLOCS, 2*ISEDIMS )
      IOLDALLOCS = ILENALLOCS
      ALLOCATE(ZRSS(ILENALLOCS), ZRHODREFS(ILENALLOCS), ILISTS(ILENALLOCS))
    END IF
!
    DO JL=1,ISEDIMS
      ZRSS(JL) = PRSS(IS1(JL),IS2(JL),IS3(JL))
      ZRHODREFS(JL) =  PRHODREF(IS1(JL),IS2(JL),IS3(JL))
    END DO
!
    ILISTLENS = 0
    DO JL=1,ISEDIMS
     IF( ZRSS(JL) .GT. ZRTMIN(5) ) THEN
       ILISTLENS = ILISTLENS + 1
       ILISTS(ILISTLENS) = JL
     END IF
    END DO
       DO JJ = 1, ILISTLENS
          JL = ILISTS(JJ)
             ZWSED (IS1(JL),IS2(JL),IS3(JL))= XFSEDS * ZRSS(JL)**XEXSEDS *  &
                                        ZRHODREFS(JL)**(XEXSEDS-XCEXVT)
       END DO
  END IF
       DO JK = IKTB , IKTE
         PRSS(:,:,JK) = PRSS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
       END DO
       IF (PRESENT(PFPR)) THEN
         DO JK = IKTB , IKTE
           PFPR(:,:,JK,5)=ZWSED(:,:,JK)
         ENDDO
       ENDIF
      PINPRS(:,:) = PINPRS(:,:) + ZWSED(:,:,IKB)/XRHOLW/KSPLITR
      IF( JN==KSPLITR ) THEN
        PRSS(:,:,:) = PRSS(:,:,:) * ZINVTSTEP
      END IF
!
!*       2.5   for graupeln
!
  ZWSED(:,:,:) = 0.
  IF( JN==1 ) PRGS(:,:,:) = PRGS(:,:,:) * PTSTEP
  IF( ISEDIMG >= 1 ) THEN
    IF ( ISEDIMG .GT. ILENALLOCG ) THEN
      IF ( ILENALLOCG .GT. 0 ) THEN
        DEALLOCATE (ZRGS, ZRHODREFG, ILISTG)
      END IF
      ILENALLOCG = MAX (IOLDALLOCG, 2*ISEDIMG )
      IOLDALLOCG = ILENALLOCG
      ALLOCATE(ZRGS(ILENALLOCG), ZRHODREFG(ILENALLOCG), ILISTG(ILENALLOCG))
    END IF
!
    DO JL=1,ISEDIMG
      ZRGS(JL) = PRGS(IG1(JL),IG2(JL),IG3(JL))
      ZRHODREFG(JL) =  PRHODREF(IG1(JL),IG2(JL),IG3(JL))
    END DO
!
    ILISTLENG = 0
    DO JL=1,ISEDIMG
     IF( ZRGS(JL) .GT. ZRTMIN(6) ) THEN
       ILISTLENG = ILISTLENG + 1
       ILISTG(ILISTLENG) = JL
     END IF
    END DO
       DO JJ = 1, ILISTLENG
          JL = ILISTG(JJ)
             ZWSED (IG1(JL),IG2(JL),IG3(JL))= XFSEDG  * ZRGS(JL)**XEXSEDG *   &
                                        ZRHODREFG(JL)**(XEXSEDG-XCEXVT)
       END DO
END IF
       DO JK = IKTB , IKTE
         PRGS(:,:,JK) = PRGS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
       END DO
       IF (PRESENT(PFPR)) THEN
         DO JK = IKTB , IKTE
           PFPR(:,:,JK,6)=ZWSED(:,:,JK)
         ENDDO
       ENDIF
       PINPRG(:,:) = PINPRG(:,:) + ZWSED(:,:,IKB)/XRHOLW/KSPLITR
      IF( JN==KSPLITR ) THEN
        PRGS(:,:,:) = PRGS(:,:,:) * ZINVTSTEP
      END IF
!
!*       2.6   for hail
!
 IF ( KRR == 7 ) THEN
  IF( JN==1 ) PRHS(:,:,:) = PRHS(:,:,:) * PTSTEP
  ZWSED(:,:,:) = 0.
  IF( ISEDIMH >= 1 ) THEN
    IF ( ISEDIMH .GT. ILENALLOCH ) THEN
      IF ( ILENALLOCH .GT. 0 ) THEN
        DEALLOCATE (ZRHS, ZRHODREFH, ILISTH)
      END IF
      ILENALLOCH = MAX (IOLDALLOCH, 2*ISEDIMH )
      IOLDALLOCH = ILENALLOCH
      ALLOCATE(ZRHS(ILENALLOCH), ZRHODREFH(ILENALLOCH), ILISTH(ILENALLOCH))
    END IF
!
    DO JL=1,ISEDIMH
      ZRHS(JL) = PRHS(IH1(JL),IH2(JL),IH3(JL))
      ZRHODREFH(JL) =  PRHODREF(IH1(JL),IH2(JL),IH3(JL))
    END DO
!
    ILISTLENH = 0
    DO JL=1,ISEDIMH
     IF( ZRHS(JL) .GT. ZRTMIN(7) ) THEN
       ILISTLENH = ILISTLENH + 1
       ILISTH(ILISTLENH) = JL
     END IF
    END DO
       DO JJ = 1, ILISTLENH
          JL = ILISTH(JJ)
             ZWSED (IH1(JL),IH2(JL),IH3(JL))= XFSEDH  * ZRHS(JL)**XEXSEDH *   &
                                        ZRHODREFH(JL)**(XEXSEDH-XCEXVT)
       END DO
  END IF
       DO JK = IKTB , IKTE
         PRHS(:,:,JK) = PRHS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
       END DO
       IF (PRESENT(PFPR)) THEN
         DO JK = IKTB , IKTE
           PFPR(:,:,JK,7)=ZWSED(:,:,JK)
         ENDDO
       ENDIF
       PINPRH(:,:) = PINPRH(:,:) + ZWSED(:,:,IKB)/XRHOLW/KSPLITR
      IF( JN==KSPLITR ) THEN
        PRHS(:,:,:) = PRHS(:,:,:) * ZINVTSTEP
      END IF
 END IF
!
END DO
!
IF (OSEDIC) THEN
   IF (ILENALLOCC .GT. 0) DEALLOCATE (ZRCS, ZRHODREFC,  &
  ILISTC,ZWLBDC,ZCONC,ZRCT, ZZT,ZPRES,ZRAY1D,ZFSEDC1D, ZWLBDA,ZCC)
END IF
IF (ILENALLOCR .GT. 0 ) DEALLOCATE(ZRHODREFR,ZRRS,ILISTR)
IF (ILENALLOCI .GT. 0 ) DEALLOCATE(ZRHODREFI,ZRIS,ILISTI)
IF (ILENALLOCS .GT. 0 ) DEALLOCATE(ZRHODREFS,ZRSS,ILISTS)
IF (ILENALLOCG .GT. 0 ) DEALLOCATE(ZRHODREFG,ZRGS,ILISTG)
IF (KRR == 7 .AND. (ILENALLOCH .GT. 0 )) DEALLOCATE(ZRHODREFH,ZRHS,ILISTH)
!
!*       2.3     budget storage
!
IF (LBUDGET_RC .AND. OSEDIC) &
                CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'SEDI_BU_RRC')
IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8 ,'SEDI_BU_RRR')
IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9 ,'SEDI_BU_RRI')
IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'SEDI_BU_RRS')
IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'SEDI_BU_RRG')
IF ( KRR == 7 .AND. LBUDGET_RH) &
                CALL BUDGET (PRHS(:,:,:)*PRHODJ(:,:,:),12,'SEDI_BU_RRH')
!
!
!
!*       2.4  DROPLET DEPOSITION AT THE 1ST LEVEL ABOVE GROUND
!
IF (LDEPOSC) THEN
  GDEP(:,:) = .FALSE.
  GDEP(IIB:IIE,IJB:IJE) =    PRCS(IIB:IIE,IJB:IJE,IKB) >0 
  WHERE (GDEP)
     PRCS(:,:,IKB) = PRCS(:,:,IKB) - XVDEPOSC * PRCT(:,:,IKB) / PDZZ(:,:,IKB)
     PINPRC(:,:) = PINPRC(:,:) + XVDEPOSC * PRCT(:,:,IKB) * PRHODREF(:,:,IKB) /XRHOLW 
     PINDEP(:,:) = XVDEPOSC * PRCT(:,:,IKB) * PRHODREF(:,:,IKB) /XRHOLW 
  END WHERE
END IF
!
!*       2.5     budget storage
!
IF ( LBUDGET_RC .AND. LDEPOSC ) &
   CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'DEPO_BU_RRC')
!

  END SUBROUTINE RAIN_ICE_SEDIMENTATION_SPLIT
!
!-------------------------------------------------------------------------------
!
 SUBROUTINE RAIN_ICE_SEDIMENTATION_STAT
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
!

REAL :: ZP1,ZP2,ZH,ZZWLBDA,ZZWLBDC,ZZCC
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2)) :: ZQP
INTEGER :: JI,JJ,JK
INTEGER :: JCOUNT, JL
INTEGER, DIMENSION(SIZE(PRHODREF,1)*SIZE(PRHODREF,2)) :: I1, I2
!
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) :: ZCONC3D !  droplet condensation
!-------------------------------------------------------------------------------
!
!
!
!*       1. Parameters for cloud sedimentation
!
  IF (OSEDIC) THEN
    ZRAY(:,:,:)   = 0.
    ZLBC(:,:,:)   = XLBC(1)
    ZFSEDC(:,:,:) = XFSEDC(1)
    ZCONC3D(:,:,:)= XCONC_LAND
    ZCONC_TMP(:,:)= XCONC_LAND
    IF (PRESENT(PSEA)) THEN
      ZCONC_TMP(:,:)=PSEA(:,:)*XCONC_SEA+(1.-PSEA(:,:))*XCONC_LAND

      DO JK=IKTB,IKTE
        ZLBC(:,:,JK)   = PSEA(:,:)*XLBC(2)+(1.-PSEA(:,:))*XLBC(1)
        ZFSEDC(:,:,JK) = (PSEA(:,:)*XFSEDC(2)+(1.-PSEA(:,:))*XFSEDC(1))
        ZFSEDC(:,:,JK) = MAX(MIN(XFSEDC(1),XFSEDC(2)),ZFSEDC(:,:,JK))
        ZCONC3D(:,:,JK)= (1.-PTOWN(:,:))*ZCONC_TMP(:,:)+PTOWN(:,:)*XCONC_URBAN
        ZRAY(:,:,JK)   = 0.5*((1.-PSEA(:,:))*GAMMA(XNUC+1.0/XALPHAC)/(GAMMA(XNUC)) + &
                PSEA(:,:)*GAMMA(XNUC2+1.0/XALPHAC2)/(GAMMA(XNUC2)))
      END DO
    ELSE
        ZCONC3D(:,:,:) = XCONC_LAND
        ZRAY(:,:,:)  = 0.5*(GAMMA(XNUC+1.0/XALPHAC)/(GAMMA(XNUC)))
    END IF
    ZRAY(:,:,:)      = MAX(1.,ZRAY(:,:,:))
    ZLBC(:,:,:)      = MAX(MIN(XLBC(1),XLBC(2)),ZLBC(:,:,:))
  ENDIF
  IF (LDEPOSC) PINDEP (:,:) = 0.
!
!*       2.    compute the fluxes
!


ZRTMIN(:)    = XRTMIN(:) * ZINVTSTEP
!
IF (OSEDIC) THEN
  ZPRCS(:,:,:) = 0.0
  ZPRCS(:,:,:) = PRCS(:,:,:)-PRCT(:,:,:)* ZINVTSTEP
  PRCS(:,:,:)  = PRCT(:,:,:)* ZINVTSTEP
END IF
ZPRRS(:,:,:) = 0.0
ZPRSS(:,:,:) = 0.0
ZPRGS(:,:,:) = 0.0
IF ( KRR == 7 ) ZPRHS(:,:,:) = 0.0
!
ZPRRS(:,:,:) = PRRS(:,:,:)-PRRT(:,:,:)* ZINVTSTEP
ZPRSS(:,:,:) = PRSS(:,:,:)-PRST(:,:,:)* ZINVTSTEP
ZPRGS(:,:,:) = PRGS(:,:,:)-PRGT(:,:,:)* ZINVTSTEP
IF ( KRR == 7 ) ZPRHS(:,:,:) = PRHS(:,:,:)-PRHT(:,:,:)* ZINVTSTEP
PRRS(:,:,:)  = PRRT(:,:,:)* ZINVTSTEP
PRSS(:,:,:)  = PRST(:,:,:)* ZINVTSTEP
PRGS(:,:,:)  = PRGT(:,:,:)* ZINVTSTEP
IF ( KRR == 7 ) PRHS(:,:,:)  = PRHT(:,:,:)* ZINVTSTEP
!
IF (OSEDIC) PRCS(:,:,:) = PRCS(:,:,:) + ZPRCS(:,:,:)
PRRS(:,:,:) = PRRS(:,:,:) + ZPRRS(:,:,:)
PRSS(:,:,:) = PRSS(:,:,:) + ZPRSS(:,:,:)
PRGS(:,:,:) = PRGS(:,:,:) + ZPRGS(:,:,:)
IF ( KRR == 7 ) PRHS(:,:,:) = PRHS(:,:,:) + ZPRHS(:,:,:)
DO JK = IKTB , IKTE
  ZW(:,:,JK) =PTSTEP/(PRHODREF(:,:,JK)* PDZZ(:,:,JK) )
END DO

!
!*       2.1   for cloud
!
 IF (OSEDIC) THEN
     PRCS(:,:,:) = PRCS(:,:,:) * PTSTEP
     ZWSED(:,:,:) = 0.
     ZWSEDW1(:,:,:) = 0.
     ZWSEDW2(:,:,:) = 0.

! calculation of P1, P2 and sedimentation flux
     DO JK = IKE , IKB, -1*KKL
       !estimation of q' taking into account incomming ZWSED
       ZQP(:,:)=ZWSED(:,:,JK+KKL)*ZW(:,:,JK)

       JCOUNT=COUNTJV2((PRCS(:,:,JK) > ZRTMIN(2) .AND. PRCT(:,:,JK) > ZRTMIN(2)) .OR. &
                       (ZQP(:,:) > ZRTMIN(2)),I1(:),I2(:))
       DO JL=1, JCOUNT
         JI=I1(JL)
         JJ=I2(JL)
         !calculation of w
         ! mars 2009 : ajout d'un test
         !IF ( PRCS(JI,JJ,JK) > ZRTMIN(2) ) THEN
         IF(PRCS(JI,JJ,JK) > ZRTMIN(2) .AND. PRCT(JI,JJ,JK) > ZRTMIN(2)) THEN
           ZZWLBDA=6.6E-8*(101325./PPABST(JI,JJ,JK))*(PTHT(JI,JJ,JK)/293.15)
           ZZWLBDC=(ZLBC(JI,JJ,JK)*ZCONC3D(JI,JJ,JK)  &
                &/(PRHODREF(JI,JJ,JK)*PRCT(JI,JJ,JK)))**XLBEXC
           ZZCC=XCC*(1.+1.26*ZZWLBDA*ZZWLBDC/ZRAY(JI,JJ,JK)) !! ZCC  : Fall speed
           ZWSEDW1 (JI,JJ,JK)=PRHODREF(JI,JJ,JK)**(-XCEXVT ) *   &
             &  ZZWLBDC**(-XDC)*ZZCC*ZFSEDC(JI,JJ,JK)
         ENDIF
         IF ( ZQP(JI,JJ) > ZRTMIN(2) ) THEN
           ZZWLBDA=6.6E-8*(101325./PPABST(JI,JJ,JK))*(PTHT(JI,JJ,JK)/293.15)
           ZZWLBDC=(ZLBC(JI,JJ,JK)*ZCONC3D(JI,JJ,JK)  &
                &/(PRHODREF(JI,JJ,JK)*ZQP(JI,JJ)))**XLBEXC
           ZZCC=XCC*(1.+1.26*ZZWLBDA*ZZWLBDC/ZRAY(JI,JJ,JK)) !! ZCC  : Fall speed
           ZWSEDW2 (JI,JJ,JK)=PRHODREF(JI,JJ,JK)**(-XCEXVT ) *   &
             &  ZZWLBDC**(-XDC)*ZZCC*ZFSEDC(JI,JJ,JK)
         ENDIF
       ENDDO

       DO JJ = IJB, IJE
         DO JI = IIB, IIE
           ZH=PDZZ(JI,JJ,JK)
           ZP1 = MIN(1., ZWSEDW1(JI,JJ,JK) * PTSTEP / ZH)
           ! mars 2009 : correction : ZWSEDW1 =>  ZWSEDW2
           !IF (ZWSEDW1(JI,JJ,JK) /= 0.) THEN
           IF (ZWSEDW2(JI,JJ,JK) /= 0.) THEN
             ZP2 = MAX(0.,1 -  ZH &
           &  / (PTSTEP*ZWSEDW2(JI,JJ,JK)) )
           ELSE
             ZP2 = 0.
           ENDIF
           ZWSED (JI,JJ,JK)=ZP1*PRHODREF(JI,JJ,JK)*&
           &ZH*PRCS(JI,JJ,JK)&
           &* ZINVTSTEP+ ZP2 * ZWSED (JI,JJ,JK+KKL)
         ENDDO
       ENDDO
     ENDDO

     DO JK = IKTB , IKTE
       PRCS(:,:,JK) = PRCS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
     END DO
     IF (PRESENT(PFPR)) THEN
       DO JK = IKTB , IKTE
         PFPR(:,:,JK,2)=ZWSED(:,:,JK)
       ENDDO
     ENDIF

     PINPRC(:,:) = ZWSED(:,:,IKB)/XRHOLW                        ! in m/s
     PRCS(:,:,:) = PRCS(:,:,:) * ZINVTSTEP
 ENDIF

!
!*       2.2   for rain
!

   PRRS(:,:,:) = PRRS(:,:,:) * PTSTEP
   ZWSED(:,:,:) = 0.
   ZWSEDW1(:,:,:) = 0.
   ZWSEDW2(:,:,:) = 0.

! calculation of ZP1, ZP2 and sedimentation flux
   DO JK = IKE , IKB, -1*KKL
     !estimation of q' taking into account incomming ZWSED
     ZQP(:,:)=ZWSED(:,:,JK+KKL)*ZW(:,:,JK)

     JCOUNT=COUNTJV2((PRRS(:,:,JK) > ZRTMIN(3)) .OR. &
                     (ZQP(:,:) > ZRTMIN(3)),I1(:),I2(:))
     DO JL=1, JCOUNT
       JI=I1(JL)
       JJ=I2(JL)
       !calculation of w
       IF ( PRRS(JI,JJ,JK) > ZRTMIN(3) ) THEN
         ZWSEDW1 (JI,JJ,JK)= XFSEDR *PRRS(JI,JJ,JK)**(XEXSEDR-1)* &
         PRHODREF(JI,JJ,JK)**(XEXSEDR-XCEXVT-1)
       ENDIF
       IF ( ZQP(JI,JJ) > ZRTMIN(3) ) THEN
         ZWSEDW2 (JI,JJ,JK)= XFSEDR *(ZQP(JI,JJ))**(XEXSEDR-1)* &
         PRHODREF(JI,JJ,JK)**(XEXSEDR-XCEXVT-1)
       ENDIF
     ENDDO
     DO JJ = IJB, IJE
       DO JI = IIB, IIE
         ZH=PDZZ(JI,JJ,JK)
         ZP1 = MIN(1., ZWSEDW1(JI,JJ,JK) * PTSTEP / ZH )
         IF (ZWSEDW2(JI,JJ,JK) /= 0.) THEN
           ZP2 = MAX(0.,1 -  ZH &
         & / (PTSTEP*ZWSEDW2(JI,JJ,JK)) )
         ELSE
           ZP2 = 0.
         ENDIF
         ZWSED (JI,JJ,JK)=ZP1*PRHODREF(JI,JJ,JK)*&
         &ZH*PRRS(JI,JJ,JK)&
         &* ZINVTSTEP+ ZP2 * ZWSED (JI,JJ,JK+KKL)
       ENDDO
     ENDDO
   ENDDO

   DO JK = IKTB , IKTE
     PRRS(:,:,JK) = PRRS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
   ENDDO
   IF (PRESENT(PFPR)) THEN
     DO JK = IKTB , IKTE
       PFPR(:,:,JK,3)=ZWSED(:,:,JK)
     ENDDO
   ENDIF
   PINPRR(:,:) = ZWSED(:,:,IKB)/XRHOLW                        ! in m/s
   PINPRR3D(:,:,:) = ZWSED(:,:,1:IKT)/XRHOLW                        ! in m/s
   PRRS(:,:,:) = PRRS(:,:,:) * ZINVTSTEP

!
!*       2.3   for pristine ice
!

   PRIS(:,:,:) = PRIS(:,:,:) * PTSTEP
   ZWSED(:,:,:) = 0.
   ZWSEDW1(:,:,:) = 0.
   ZWSEDW2(:,:,:) = 0.
! calculation of ZP1, ZP2 and sedimentation flux
   DO JK = IKE , IKB, -1*KKL
     !estimation of q' taking into account incomming ZWSED
     ZQP(:,:)=ZWSED(:,:,JK+KKL)*ZW(:,:,JK)

     JCOUNT=COUNTJV2((PRIS(:,:,JK) > MAX(ZRTMIN(4),1.0E-7 )) .OR. &
                     (ZQP(:,:) > MAX(ZRTMIN(4),1.0E-7 )),I1(:),I2(:))
     DO JL=1, JCOUNT
       JI=I1(JL)
       JJ=I2(JL)
       !calculation of w
       IF ( PRIS(JI,JJ,JK) > MAX(ZRTMIN(4),1.0E-7 ) ) THEN
         ZWSEDW1 (JI,JJ,JK)= XFSEDI *  &
         &  PRHODREF(JI,JJ,JK)**(XCEXVT) * & !    McF&H
         &  MAX( 0.05E6,-0.15319E6-0.021454E6* &
         &  ALOG(PRHODREF(JI,JJ,JK)*PRIS(JI,JJ,JK)) )**XEXCSEDI
       ENDIF
       IF ( ZQP(JI,JJ) > MAX(ZRTMIN(4),1.0E-7 ) ) THEN
         ZWSEDW2 (JI,JJ,JK)= XFSEDI *  &
         &  PRHODREF(JI,JJ,JK)**(XCEXVT) * & !    McF&H
         &  MAX( 0.05E6,-0.15319E6-0.021454E6* &
         &  ALOG(PRHODREF(JI,JJ,JK)*ZQP(JI,JJ)) )**XEXCSEDI
       ENDIF
     ENDDO
     DO JJ = IJB, IJE
       DO JI = IIB, IIE
         ZH=PDZZ(JI,JJ,JK)
         ZP1 = MIN(1., ZWSEDW1(JI,JJ,JK) * PTSTEP / ZH )
         IF (ZWSEDW2(JI,JJ,JK) /= 0.) THEN
           ZP2 = MAX(0.,1 - ZH  &
           &  / (PTSTEP*ZWSEDW2(JI,JJ,JK)) )
         ELSE
           ZP2 = 0.
         ENDIF
         ZWSED (JI,JJ,JK)=ZP1*PRHODREF(JI,JJ,JK)*&
         &ZH*PRIS(JI,JJ,JK)&
         &* ZINVTSTEP+ ZP2 * ZWSED (JI,JJ,JK+KKL)
       ENDDO
     ENDDO
   ENDDO

   DO JK = IKTB , IKTE
     PRIS(:,:,JK) = PRIS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
   ENDDO
   IF (PRESENT(PFPR)) THEN
     DO JK = IKTB , IKTE
       PFPR(:,:,JK,4)=ZWSED(:,:,JK)
     ENDDO
   ENDIF

   PRIS(:,:,:) = PRIS(:,:,:) * ZINVTSTEP


!
!*       2.4   for aggregates/snow
!

   PRSS(:,:,:) = PRSS(:,:,:) * PTSTEP
   ZWSED(:,:,:) = 0.
   ZWSEDW1(:,:,:) = 0.
   ZWSEDW2(:,:,:) = 0.

! calculation of ZP1, ZP2 and sedimentation flux
   DO JK = IKE , IKB, -1*KKL
     !estimation of q' taking into account incomming ZWSED
     ZQP(:,:)=ZWSED(:,:,JK+KKL)*ZW(:,:,JK)

     JCOUNT=COUNTJV2((PRSS(:,:,JK) > ZRTMIN(5)) .OR. &
                     (ZQP(:,:) > ZRTMIN(5)),I1(:),I2(:))
     DO JL=1, JCOUNT
       JI=I1(JL)
       JJ=I2(JL)
       !calculation of w
       IF (PRSS(JI,JJ,JK) > ZRTMIN(5) ) THEN
         ZWSEDW1(JI,JJ,JK)=XFSEDS*(PRSS(JI,JJ,JK))**(XEXSEDS-1)*&
         PRHODREF(JI,JJ,JK)**(XEXSEDS-XCEXVT-1)
       ENDIF
       IF ( ZQP(JI,JJ) > ZRTMIN(5) ) THEN
         ZWSEDW2(JI,JJ,JK)=XFSEDS*(ZQP(JI,JJ))**(XEXSEDS-1)*&
         PRHODREF(JI,JJ,JK)**(XEXSEDS-XCEXVT-1)
       ENDIF
     ENDDO
     DO JJ = IJB, IJE
       DO JI = IIB, IIE
         ZH=PDZZ(JI,JJ,JK)
         ZP1 = MIN(1., ZWSEDW1(JI,JJ,JK) * PTSTEP / ZH )
         IF (ZWSEDW2(JI,JJ,JK) /= 0.) THEN
           ZP2 = MAX(0.,1 - ZH&
          / (PTSTEP*ZWSEDW2(JI,JJ,JK)) )
         ELSE
           ZP2 = 0.
         ENDIF
         ZWSED (JI,JJ,JK)=ZP1*PRHODREF(JI,JJ,JK)*&
         &ZH*PRSS(JI,JJ,JK)&
         &* ZINVTSTEP+ ZP2 * ZWSED (JI,JJ,JK+KKL)
       ENDDO
     ENDDO
   ENDDO

   DO JK = IKTB , IKTE
     PRSS(:,:,JK) = PRSS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
   ENDDO
   IF (PRESENT(PFPR)) THEN
     DO JK = IKTB , IKTE
       PFPR(:,:,JK,5)=ZWSED(:,:,JK)
     ENDDO
   ENDIF

   PINPRS(:,:) = ZWSED(:,:,IKB)/XRHOLW                        ! in m/s

   PRSS(:,:,:) = PRSS(:,:,:) * ZINVTSTEP


!
!*       2.5   for graupeln
!

   PRGS(:,:,:) = PRGS(:,:,:) * PTSTEP
   ZWSED(:,:,:) = 0.
   ZWSEDW1(:,:,:) = 0.
   ZWSEDW2(:,:,:) = 0.

! calculation of ZP1, ZP2 and sedimentation flux
   DO JK = IKE,  IKB, -1*KKL
     !estimation of q' taking into account incomming ZWSED
     ZQP(:,:)=ZWSED(:,:,JK+KKL)*ZW(:,:,JK)

     JCOUNT=COUNTJV2((PRGS(:,:,JK) > ZRTMIN(6)) .OR. &
                     (ZQP(:,:) > ZRTMIN(6)),I1(:),I2(:))
     DO JL=1, JCOUNT
       JI=I1(JL)
       JJ=I2(JL)
       !calculation of w
       IF ( PRGS(JI,JJ,JK) > ZRTMIN(6) ) THEN
         ZWSEDW1 (JI,JJ,JK)= XFSEDG*(PRGS(JI,JJ,JK))**(XEXSEDG-1) * &
                                  PRHODREF(JI,JJ,JK)**(XEXSEDG-XCEXVT-1)
       ENDIF
       IF ( ZQP(JI,JJ) > ZRTMIN(6) ) THEN
         ZWSEDW2 (JI,JJ,JK)= XFSEDG*(ZQP(JI,JJ))**(XEXSEDG-1) * &
                                  PRHODREF(JI,JJ,JK)**(XEXSEDG-XCEXVT-1)
       ENDIF
     ENDDO
     DO JJ = IJB, IJE
       DO JI = IIB, IIE
         ZH=PDZZ(JI,JJ,JK)
         ZP1 = MIN(1., ZWSEDW1(JI,JJ,JK) * PTSTEP / ZH )
         IF (ZWSEDW2(JI,JJ,JK) /= 0.) THEN
           ZP2 = MAX(0.,1 - ZH &
         & / (PTSTEP*ZWSEDW2(JI,JJ,JK)) )
         ELSE
           ZP2 = 0.
         ENDIF
         ZWSED (JI,JJ,JK)=ZP1*PRHODREF(JI,JJ,JK)*&
         &ZH*PRGS(JI,JJ,JK)&
         &* ZINVTSTEP+ ZP2 * ZWSED (JI,JJ,JK+KKL)
       ENDDO
     ENDDO
   ENDDO

   DO JK = IKTB , IKTE
         PRGS(:,:,JK) = PRGS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
   ENDDO
   IF (PRESENT(PFPR)) THEN
     DO JK = IKTB , IKTE
       PFPR(:,:,JK,6)=ZWSED(:,:,JK)
     ENDDO
   ENDIF

   PINPRG(:,:) = ZWSED(:,:,IKB)/XRHOLW                        ! in m/s

   PRGS(:,:,:) = PRGS(:,:,:) * ZINVTSTEP

!
!*       2.6   for hail
!
 IF ( KRR == 7 ) THEN
     PRHS(:,:,:) = PRHS(:,:,:) * PTSTEP
     ZWSED(:,:,:) = 0.
     ZWSEDW1(:,:,:) = 0.
     ZWSEDW2(:,:,:) = 0.
! calculation of ZP1, ZP2 and sedimentation flux
     DO JK = IKE , IKB, -1*KKL
     !estimation of q' taking into account incomming ZWSED
     ZQP(:,:)=ZWSED(:,:,JK+KKL)*ZW(:,:,JK)

     JCOUNT=COUNTJV2((PRHS(:,:,JK)+ZQP(JI,JJ) > ZRTMIN(7)) .OR. &
                     (ZQP(:,:) > ZRTMIN(7)),I1(:),I2(:))
     DO JL=1, JCOUNT
       JI=I1(JL)
       JJ=I2(JL)
         !calculation of w
         IF ((PRHS(JI,JJ,JK)+ZQP(JI,JJ)) > ZRTMIN(7) ) THEN
           ZWSEDW1 (JI,JJ,JK)= XFSEDH  * (PRHS(JI,JJ,JK))**(XEXSEDH-1) *   &
                                    PRHODREF(JI,JJ,JK)**(XEXSEDH-XCEXVT-1)
         ENDIF
         IF ( ZQP(JI,JJ) > ZRTMIN(7) ) THEN
           ZWSEDW2 (JI,JJ,JK)= XFSEDH  * ZQP(JI,JJ)**(XEXSEDH-1) *   &
                                    PRHODREF(JI,JJ,JK)**(XEXSEDH-XCEXVT-1)
         ENDIF
       ENDDO
       DO JJ = IJB, IJE
         DO JI = IIB, IIE
           ZH=PDZZ(JI,JJ,JK)
           ZP1 = MIN(1., ZWSEDW1(JI,JJ,JK) * PTSTEP / ZH)
           IF (ZWSEDW2(JI,JJ,JK) /= 0.) THEN
             ZP2 = MAX(0.,1 - ZH &
           &  / (PTSTEP*ZWSEDW2(JI,JJ,JK)) )
           ELSE
             ZP2 = 0.
           ENDIF
           ZWSED (JI,JJ,JK)=ZP1*PRHODREF(JI,JJ,JK)*&
           &ZH*PRHS(JI,JJ,JK)&
           &* ZINVTSTEP+ ZP2 * ZWSED (JI,JJ,JK+KKL)
         ENDDO
       ENDDO
     ENDDO

     DO JK = IKTB , IKTE
       PRHS(:,:,JK) = PRHS(:,:,JK) + ZW(:,:,JK)*(ZWSED(:,:,JK+KKL)-ZWSED(:,:,JK))
     ENDDO
     IF (PRESENT(PFPR)) THEN
       DO JK = IKTB , IKTE
         PFPR(:,:,JK,7)=ZWSED(:,:,JK)
       ENDDO
     ENDIF

     PINPRH(:,:) = ZWSED(:,:,IKB)/XRHOLW                        ! in m/s

     PRHS(:,:,:) = PRHS(:,:,:) * ZINVTSTEP

 ENDIF
!

!
!*       2.3     budget storage
!
IF (LBUDGET_RC .AND. OSEDIC) &
                CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'SEDI_BU_RRC')
IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8 ,'SEDI_BU_RRR')
IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9 ,'SEDI_BU_RRI')
IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'SEDI_BU_RRS')
IF (LBUDGET_RG) CALL BUDGET (PRGS(:,:,:)*PRHODJ(:,:,:),11,'SEDI_BU_RRG')
IF ( KRR == 7 .AND. LBUDGET_RH) &
                CALL BUDGET (PRHS(:,:,:)*PRHODJ(:,:,:),12,'SEDI_BU_RRH')
!
!
!*       2.4  DROPLET DEPOSITION AT THE 1ST LEVEL ABOVE GROUND
!
IF (LDEPOSC) THEN
  GDEP(:,:) = .FALSE.
  GDEP(IIB:IIE,IJB:IJE) =    PRCS(IIB:IIE,IJB:IJE,IKB) >0 
  WHERE (GDEP)
     PRCS(:,:,IKB) = PRCS(:,:,IKB) - XVDEPOSC * PRCT(:,:,IKB) / PDZZ(:,:,IKB)
     PINPRC(:,:) = PINPRC(:,:) + XVDEPOSC * PRCT(:,:,IKB) * PRHODREF(:,:,IKB) /XRHOLW 
     PINDEP(:,:) = XVDEPOSC * PRCT(:,:,IKB) * PRHODREF(:,:,IKB) /XRHOLW 
  END WHERE
END IF
!
!*       2.5     budget storage
!
IF ( LBUDGET_RC .AND. LDEPOSC ) &
   CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'DEPO_BU_RRC')

!
  END SUBROUTINE RAIN_ICE_SEDIMENTATION_STAT
!
!-------------------------------------------------------------------------------
!

!
  SUBROUTINE RAIN_ICE_NUCLEATION
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
INTEGER , DIMENSION(SIZE(GNEGT))  :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                           :: JL       ! and PACK intrinsics
!
!-------------------------------------------------------------------------------
!
!
!  compute the temperature and the pressure
!
ZT(:,:,:) = PTHT(:,:,:) * ( PPABST(:,:,:) / XP00 ) ** (XRD/XCPD)
!
!  optimization by looking for locations where
!  the temperature is negative only !!!
!
GNEGT(:,:,:) = .FALSE.
GNEGT(IIB:IIE,IJB:IJE,IKTB:IKTE) = ZT(IIB:IIE,IJB:IJE,IKTB:IKTE)<XTT
INEGT = COUNTJV( GNEGT(:,:,:),I1(:),I2(:),I3(:))
IF( INEGT >= 1 ) THEN
  ALLOCATE(ZRVT(INEGT)) ;
  ALLOCATE(ZCIT(INEGT)) ;
  ALLOCATE(ZZT(INEGT))  ;
  ALLOCATE(ZPRES(INEGT));
  DO JL=1,INEGT
    ZRVT(JL) = PRVT(I1(JL),I2(JL),I3(JL))
    ZCIT(JL) = PCIT(I1(JL),I2(JL),I3(JL))
    ZZT(JL) = ZT(I1(JL),I2(JL),I3(JL))
    ZPRES(JL) = PPABST(I1(JL),I2(JL),I3(JL))
  ENDDO
  ALLOCATE(ZZW(INEGT))
  ALLOCATE(ZUSW(INEGT))
  ALLOCATE(ZSSI(INEGT))
    ZZW(:) = EXP( XALPI - XBETAI/ZZT(:) - XGAMI*ALOG(ZZT(:) ) )           ! es_i
    ZZW(:) = MIN(ZPRES(:)/2., ZZW(:))             ! safety limitation
    ZSSI(:) = ZRVT(:)*( ZPRES(:)-ZZW(:) ) / ( (XMV/XMD) * ZZW(:) ) - 1.0
                                                      ! Supersaturation over ice
    ZUSW(:) = EXP( XALPW - XBETAW/ZZT(:) - XGAMW*ALOG(ZZT(:) ) )          ! es_w
    ZUSW(:) = MIN(ZPRES(:)/2.,ZUSW(:))            ! safety limitation
    ZUSW(:) = ( ZUSW(:)/ZZW(:) )*( (ZPRES(:)-ZZW(:))/(ZPRES(:)-ZUSW(:)) ) - 1.0
                             ! Supersaturation of saturated water vapor over ice
!
!*       3.1     compute the heterogeneous nucleation source: RVHENI
!
!*       3.1.1   compute the cloud ice concentration
!
  ZZW(:) = 0.0
  ZSSI(:) = MIN( ZSSI(:), ZUSW(:) ) ! limitation of SSi according to SSw=0
  WHERE( (ZZT(:)<XTT-5.0) .AND. (ZSSI(:)>0.0) )
    ZZW(:) = XNU20 * EXP( XALPHA2*ZSSI(:)-XBETA2 )
  END WHERE
  WHERE( (ZZT(:)<=XTT-2.0) .AND. (ZZT(:)>=XTT-5.0) .AND. (ZSSI(:)>0.0) )
    ZZW(:) = MAX( XNU20 * EXP( -XBETA2 ),XNU10 * EXP( -XBETA1*(ZZT(:)-XTT) ) * &
                               ( ZSSI(:)/ZUSW(:) )**XALPHA1 )
  END WHERE
  ZZW(:) = ZZW(:) - ZCIT(:)
  IF( MAXVAL(ZZW(:)) > 0.0 ) THEN
!
!*       3.1.2   update the r_i and r_v mixing ratios
!
    ZZW(:) = MIN( ZZW(:),50.E3 ) ! limitation provisoire a 50 l^-1
    ZW(:,:,:) = UNPACK( ZZW(:),MASK=GNEGT(:,:,:),FIELD=0.0 )
    ZW(:,:,:) = MAX( ZW(:,:,:) ,0.0 ) *XMNU0/(PRHODREF(:,:,:)*PTSTEP)
    PRIS(:,:,:) = PRIS(:,:,:) + ZW(:,:,:)
    PRVS(:,:,:) = PRVS(:,:,:) - ZW(:,:,:)
    IF ( KRR == 7 ) THEN
        PTHS(:,:,:) = PTHS(:,:,:) + ZW(:,:,:)*(XLSTT+(XCPV-XCI)*(ZT(:,:,:)-XTT))   &
                 /( (XCPD + XCPV*PRVT(:,:,:) + XCL*(PRCT(:,:,:)+PRRT(:,:,:))   &
       + XCI*(PRIT(:,:,:)+PRST(:,:,:)+PRGT(:,:,:)+PRHT(:,:,:)))*PEXNREF(:,:,:) )
      ELSE IF( KRR == 6 ) THEN
        PTHS(:,:,:) = PTHS(:,:,:) + ZW(:,:,:)*(XLSTT+(XCPV-XCI)*(ZT(:,:,:)-XTT))   &
                 /( (XCPD + XCPV*PRVT(:,:,:) + XCL*(PRCT(:,:,:)+PRRT(:,:,:))   &
                   + XCI*(PRIT(:,:,:)+PRST(:,:,:)+PRGT(:,:,:)))*PEXNREF(:,:,:) )
    END IF
                                 ! f(L_s*(RVHENI))
    ZZW(:) = MAX( ZZW(:)+ZCIT(:),ZCIT(:) )
    PCIT(:,:,:) = MAX( UNPACK( ZZW(:),MASK=GNEGT(:,:,:),FIELD=0.0 ) , &
                       PCIT(:,:,:) )
  END IF
  DEALLOCATE(ZSSI)
  DEALLOCATE(ZUSW)
  DEALLOCATE(ZZW)
  DEALLOCATE(ZPRES)
  DEALLOCATE(ZZT)
  DEALLOCATE(ZCIT)
  DEALLOCATE(ZRVT)
END IF
!
!*       3.1.3   budget storage
!
IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'HENU_BU_RTH')
IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6,'HENU_BU_RRV')
IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'HENU_BU_RRI')
!
  END SUBROUTINE RAIN_ICE_NUCLEATION
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE RAIN_ICE_SLOW
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!
!*       3.2     compute the homogeneous nucleation source: RCHONI
!
  ZZW(:) = 0.0
  WHERE( (ZZT(:)<XTT-35.0) .AND. (ZRCT(:)>XRTMIN(2)) .AND. (ZRCS(:)>0.) )
    ZZW(:) = MIN( ZRCS(:),XHON*ZRHODREF(:)*ZRCT(:)       &
                                 *EXP( MIN(XMNH_HUGE_12_LOG,XALPHA3*(ZZT(:)-XTT)-XBETA3) ) )
                                 ! *EXP( XALPHA3*(ZZT(:)-XTT)-XBETA3 ) )
    ZRIS(:) = ZRIS(:) + ZZW(:)
    ZRCS(:) = ZRCS(:) - ZZW(:)
    ZTHS(:) = ZTHS(:) + ZZW(:)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(RCHONI))
  ENDWHERE
!
  IF (LBUDGET_TH) CALL BUDGET (                                                &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),  &
                                                              4,'HON_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                                &
                     UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),   &
                                                              7,'HON_BU_RRC')
  IF (LBUDGET_RI) CALL BUDGET (                                                &
                     UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),   &
                                                              9,'HON_BU_RRI')
!
!*       3.3     compute the spontaneous freezing source: RRHONG
!
  ZZW(:) = 0.0
  WHERE( (ZZT(:)<XTT-35.0) .AND. (ZRRT(:)>XRTMIN(3)) .AND. (ZRRS(:)>0.) )
    ZZW(:) = MIN( ZRRS(:),ZRRT(:)* ZINVTSTEP )
    ZRGS(:) = ZRGS(:) + ZZW(:)
    ZRRS(:) = ZRRS(:) - ZZW(:)
    ZTHS(:) = ZTHS(:) + ZZW(:)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(RRHONG))
  ENDWHERE
!
  IF (LBUDGET_TH) CALL BUDGET (                                                &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),  &
                                                              4,'SFR_BU_RTH')
  IF (LBUDGET_RR) CALL BUDGET (                                                &
                     UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),   &
                                                              8,'SFR_BU_RRR')
  IF (LBUDGET_RG) CALL BUDGET (                                                &
                     UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),   &
                                                             11,'SFR_BU_RRG')
!
!*       3.4    compute the deposition, aggregation and autoconversion sources
!
  ZKA(:) = 2.38E-2 + 0.0071E-2 * ( ZZT(:) - XTT )          ! k_a
  ZDV(:) = 0.211E-4 * (ZZT(:)/XTT)**1.94 * (XP00/ZPRES(:)) ! D_v
!
!*       3.4.1  compute the thermodynamical function A_i(T,P)
!*              and the c^prime_j (in the ventilation factor)
!

  ZAI(:) = EXP( XALPI - XBETAI/ZZT(:) - XGAMI*ALOG(ZZT(:) ) ) ! es_i
  ZAI(:) = ( XLSTT + (XCPV-XCI)*(ZZT(:)-XTT) )**2 / (ZKA(:)*XRV*ZZT(:)**2) &
                                 + ( XRV*ZZT(:) ) / (ZDV(:)*ZAI(:))
  ZCJ(:) = XSCFAC * ZRHODREF(:)**0.3 / SQRT( 1.718E-5+0.0049E-5*(ZZT(:)-XTT) )
!
!*       3.4.2  compute the riming-conversion of r_c for r_i production: RCAUTI
!
!  ZZW(:) = 0.0
!  ZTIMAUTIC = SQRT( XTIMAUTI*XTIMAUTC )
!  WHERE ( (ZRCT(:)>0.0) .AND. (ZRIT(:)>0.0) .AND. (ZRCS(:)>0.0) )
!    ZZW(:) = MIN( ZRCS(:),ZTIMAUTIC * MAX( SQRT( ZRIT(:)*ZRCT(:) ),0.0 ) )
!    ZRIS(:) = ZRIS(:) + ZZW(:)
!    ZRCS(:) = ZRCS(:) - ZZW(:)
!    ZTHS(:) = ZTHS(:) + ZZW(:)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(RCAUTI))
!  END WHERE
!
!*       3.4.3  compute the deposition on r_s: RVDEPS
!
  WHERE ( ZRST(:)>0.0 )
    ZLBDAS(:)  = MIN( XLBDAS_MAX,                                           &
                      XLBS*( ZRHODREF(:)*MAX( ZRST(:),XRTMIN(5) ) )**XLBEXS )
  END WHERE
  ZZW(:) = 0.0
  WHERE ( (ZRST(:)>XRTMIN(5)) .AND. (ZRSS(:)>0.0) )
    ZZW(:) = ( ZSSI(:)/(ZRHODREF(:)*ZAI(:)) ) *                               &
             ( X0DEPS*ZLBDAS(:)**XEX0DEPS + X1DEPS*ZCJ(:)*ZLBDAS(:)**XEX1DEPS )
    ZZW(:) =         MIN( ZRVS(:),ZZW(:)      )*(0.5+SIGN(0.5,ZZW(:))) &
                   - MIN( ZRSS(:),ABS(ZZW(:)) )*(0.5-SIGN(0.5,ZZW(:)))
    ZRSS(:) = ZRSS(:) + ZZW(:)
    ZRVS(:) = ZRVS(:) - ZZW(:)
    ZTHS(:) = ZTHS(:) + ZZW(:)*ZLSFACT(:)
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),  &
                                                              4,'DEPS_BU_RTH')
  IF (LBUDGET_RV) CALL BUDGET (                                                &
                 UNPACK(ZRVS(:),MASK=GMICRO(:,:,:),FIELD=PRVS)*PRHODJ(:,:,:),  &
                                                              6,'DEPS_BU_RRV')
  IF (LBUDGET_RS) CALL BUDGET (                                                &
                     UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),   &
                                                             10,'DEPS_BU_RRS')
!
!*       3.4.4  compute the aggregation on r_s: RIAGGS
!
  ZZW(:) = 0.0
  WHERE ( (ZRIT(:)>XRTMIN(4)) .AND. (ZRST(:)>XRTMIN(5)) .AND. (ZRIS(:)>0.0) )
    ZZW(:) = MIN( ZRIS(:),XFIAGGS * EXP( XCOLEXIS*(ZZT(:)-XTT) ) &
                                  * ZRIT(:)                      &
                                  * ZLBDAS(:)**XEXIAGGS          &
                                  * ZRHODREF(:)**(-XCEXVT)       )
    ZRSS(:)  = ZRSS(:)  + ZZW(:)
    ZRIS(:)  = ZRIS(:)  - ZZW(:)
  END WHERE
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              9,'AGGS_BU_RRI')
  IF (LBUDGET_RS) CALL BUDGET (                                                 &
                     UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             10,'AGGS_BU_RRS')
!
!*       3.4.5  compute the autoconversion of r_i for r_s production: RIAUTS
!
  ALLOCATE(ZCRIAUTI(IMICRO))
!  ZCRIAUTI(:)=MIN(XCRIAUTI,10**(0.06*(ZZT(:)-XTT)-3.5))
  ZCRIAUTI(:)=MIN(XCRIAUTI,10**(XACRIAUTI*(ZZT(:)-XTT)+XBCRIAUTI))
  ZZW(:) = 0.0
  WHERE ( (ZRIT(:)>XRTMIN(4)) .AND. (ZRIS(:)>0.0) )
    ZZW(:) = MIN( ZRIS(:),XTIMAUTI * EXP( XTEXAUTI*(ZZT(:)-XTT) ) &
                            * MAX( ZRIT(:)-ZCRIAUTI(:),0.0 ) )
    ZRSS(:)  = ZRSS(:)  + ZZW(:)
    ZRIS(:)  = ZRIS(:)  - ZZW(:)
  END WHERE
  DEALLOCATE(ZCRIAUTI)
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              9,'AUTS_BU_RRI')
  IF (LBUDGET_RS) CALL BUDGET (                                                 &
                     UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             10,'AUTS_BU_RRS')
!
!*       3.4.6  compute the deposition on r_g: RVDEPG
!
!
  WHERE ( ZRGT(:)>0.0 )
    ZLBDAG(:)  = XLBG*( ZRHODREF(:)*MAX( ZRGT(:),XRTMIN(6) ) )**XLBEXG
  END WHERE
  ZZW(:) = 0.0
  WHERE ( (ZRGT(:)>XRTMIN(6)) .AND. (ZRGS(:)>0.0) )
    ZZW(:) = ( ZSSI(:)/(ZRHODREF(:)*ZAI(:)) ) *                               &
             ( X0DEPG*ZLBDAG(:)**XEX0DEPG + X1DEPG*ZCJ(:)*ZLBDAG(:)**XEX1DEPG )
    ZZW(:) =         MIN( ZRVS(:),ZZW(:)      )*(0.5+SIGN(0.5,ZZW(:))) &
                   - MIN( ZRGS(:),ABS(ZZW(:)) )*(0.5-SIGN(0.5,ZZW(:)))
    ZRGS(:) = ZRGS(:) + ZZW(:)
    ZRVS(:) = ZRVS(:) - ZZW(:)
    ZTHS(:) = ZTHS(:) + ZZW(:)*ZLSFACT(:)
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                 &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                              4,'DEPG_BU_RTH')
  IF (LBUDGET_RV) CALL BUDGET (                                                 &
                 UNPACK(ZRVS(:),MASK=GMICRO(:,:,:),FIELD=PRVS)*PRHODJ(:,:,:),   &
                                                              6,'DEPG_BU_RRV')
  IF (LBUDGET_RG) CALL BUDGET (                                                 &
                     UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             11,'DEPG_BU_RRG')
!
  END SUBROUTINE RAIN_ICE_SLOW
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE RAIN_ICE_WARM
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!
!-------------------------------------------------------------------------------
!
!*       4.2    compute the autoconversion of r_c for r_r production: RCAUTR
!

    WHERE( ZRCS(:)>0.0 .AND. ZHLC_HCF(:).GT.0.0 )
      ZZW(:) = XTIMAUTC*MAX( ZHLC_HRC(:)/ZHLC_HCF(:)  - XCRIAUTC/ZRHODREF(:),0.0)
      ZZW(:) = MIN( ZRCS(:),ZHLC_HCF(:)*ZZW(:))
      ZRCS(:) = ZRCS(:) - ZZW(:)
      ZRRS(:) = ZRRS(:) + ZZW(:)
    END WHERE
!
      IF (LBUDGET_RC) CALL BUDGET (                                               &
                       UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                                7,'AUTO_BU_RRC')
      IF (LBUDGET_RR) CALL BUDGET (                                               &
                       UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                                8,'AUTO_BU_RRR')
!
!*       4.3    compute the accretion of r_c for r_r production: RCACCR
!
    IF (CSUBG_RC_RR_ACCR=='NONE') THEN
      !CLoud water and rain are diluted over the grid box
      WHERE( ZRCT(:)>XRTMIN(2) .AND. ZRRT(:)>XRTMIN(3) .AND. ZRCS(:)>0.0 )
        ZZW(:) = MIN( ZRCS(:), XFCACCR * ZRCT(:)                &
                 * ZLBDAR(:)**XEXCACCR    &
                 * ZRHODREF(:)**(-XCEXVT) )
        ZRCS(:) = ZRCS(:) - ZZW(:)
        ZRRS(:) = ZRRS(:) + ZZW(:)
      END WHERE

    ELSEIF (CSUBG_RC_RR_ACCR=='PRFR') THEN
      !Cloud water is concentrated over its fraction with possibly to parts with high and low content as set for autoconversion
      !Rain is concnetrated over its fraction
      !Rain in high content area fraction: ZHLC_HCF
      !Rain in low content area fraction:
      ! if ZRF<ZCF (rain is entirely falling in cloud): ZRF-ZHLC_HCF
      ! if ZRF>ZCF (rain is falling in cloud and in clear sky): ZCF-ZHLC_HCF
      ! => min(ZCF, ZRF)-ZHLC_HCF
      ZZW(:) = 0.
      WHERE( ZHLC_HRC(:)>XRTMIN(2) .AND. ZRRT(:)>XRTMIN(3) .AND. ZRCS(:)>0.0 &
            .AND. ZHLC_HCF(:)>0 )
        !Accretion due to rain falling in high cloud content
        ZZW(:) = XFCACCR * ( ZHLC_HRC(:)/ZHLC_HCF(:) )     &
               * ZLBDAR_RF(:)**XEXCACCR &
               * ZRHODREF(:)**(-XCEXVT) &
               * ZHLC_HCF
      END WHERE
      WHERE( ZHLC_LRC(:)>XRTMIN(2) .AND. ZRRT(:)>XRTMIN(3) .AND. ZRCS(:)>0.0 &
            .AND. ZHLC_LCF(:)>0 )
        !We add acrretion due to rain falling in low cloud content
        ZZW(:) = ZZW(:) + XFCACCR * ( ZHLC_LRC(:)/ZHLC_LCF(:) )     &
                        * ZLBDAR_RF(:)**XEXCACCR &
                        * ZRHODREF(:)**(-XCEXVT) &
                        * (MIN(ZCF(:), ZRF(:))-ZHLC_HCF(:))
      END WHERE
      ZZW(:)=MIN(ZRCS(:), ZZW(:))
      ZRCS(:) = ZRCS(:) - ZZW(:)
      ZRRS(:) = ZRRS(:) + ZZW(:)

    ELSE
      !wrong CSUBG_RC_RR_ACCR case
      WRITE(*,*) 'wrong CSUBG_RC_RR_ACCR case'
            CALL PRINT_MSG(NVERB_FATAL,'GEN','RAIN_ICE_WARM','')  
    ENDIF

    IF (LBUDGET_RC) CALL BUDGET (                                               &
                     UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              7,'ACCR_BU_RRC')
    IF (LBUDGET_RR) CALL BUDGET (                                               &
                     UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              8,'ACCR_BU_RRR')
!
!*       4.4    compute the evaporation of r_r: RREVAV
!
    ZZW(:) = 0.0

    IF (CSUBG_RR_EVAP=='NONE') THEN
      !Evaporation only when there's no cloud (RC must be 0)
       WHERE( (ZRRT(:)>XRTMIN(3)) .AND. (ZRCT(:)<=XRTMIN(2)) )
          ZZW(:)  = EXP( XALPW - XBETAW/ZZT(:) - XGAMW*ALOG(ZZT(:) ) ) ! es_w
          ZUSW(:) = 1.0 - ZRVT(:)*( ZPRES(:)-ZZW(:) ) / ( (XMV/XMD) * ZZW(:) )
                                                        ! Undersaturation over water
          ZZW(:) = ( XLVTT+(XCPV-XCL)*(ZZT(:)-XTT) )**2 / ( ZKA(:)*XRV*ZZT(:)**2 ) &
               + ( XRV*ZZT(:) ) / ( ZDV(:)*ZZW(:) )
          ZZW(:) = MIN( ZRRS(:),( MAX( 0.0,ZUSW(:) )/(ZRHODREF(:)*ZZW(:)) ) *      &
            ( X0EVAR*ZLBDAR(:)**XEX0EVAR+X1EVAR*ZCJ(:)*ZLBDAR(:)**XEX1EVAR ) )
          ZRRS(:) = ZRRS(:) - ZZW(:)
          ZRVS(:) = ZRVS(:) + ZZW(:)
          ZTHS(:) = ZTHS(:) - ZZW(:)*ZLVFACT(:)
       END WHERE

    ELSEIF (CSUBG_RR_EVAP=='CLFR' .OR. CSUBG_RR_EVAP=='PRFR') THEN
      !Evaporation in clear sky part
      !With CLFR, rain is diluted over the grid box
      !With PRFR, rain is concentrated in its fraction
      !Use temperature and humidity in clear sky part like Bechtold et al. (1993)
      IF (CSUBG_RR_EVAP=='CLFR') THEN
        ZZW4(:)=1. !Precipitation fraction
        ZZW3(:)=ZLBDAR(:)
      ELSE
        ZZW4(:)=ZRF(:) !Precipitation fraction
        ZZW3(:)=ZLBDAR_RF(:)
      ENDIF

      !ATTENTION
      !Il faudrait recalculer les variables ZKA, ZDV, ZCJ en tenant compte de la température T^u
      !Ces variables devraient être sorties de rain_ice_slow et on mettrait le calcul de T^u, T^s
      !et plusieurs versions (comme actuellement, en ciel clair, en ciel nuageux) de ZKA, ZDV, ZCJ dans rain_ice
      !On utiliserait la bonne version suivant l'option NONE, CLFR... dans l'évaporation et ailleurs

      WHERE(  (ZRRT(:)>XRTMIN(3)) .AND. ( ZZW4(:) > ZCF(:) ) )
        ! outside the cloud (environment) the use of T^u (unsaturated) instead of T
        ! Bechtold et al. 1993
        !
        ! T^u = T_l = theta_l * (T/theta)
        ZZW2(:) =  ZTHLT(:) * ZZT(:) / ZTHT(:)
        !
        ! es_w with new T^u
        ZZW(:)  = EXP( XALPW - XBETAW/ZZW2(:) - XGAMW*ALOG(ZZW2(:) ) )
        !
        ! S, Undersaturation over water (with new theta^u)
        ZUSW(:) = 1.0 - ZRVT(:)*( ZPRES(:)-ZZW(:) ) / ( (XMV/XMD) * ZZW(:) )
        !
        ZZW(:) = ( XLVTT+(XCPV-XCL)*(ZZW2(:)-XTT) )**2 / ( ZKA(:)*XRV*ZZW2(:)**2 ) &
               + ( XRV*ZZW2(:) ) / ( ZDV(:)*ZZW(:) )
        !
        ZZW(:) = MAX( 0.0,ZUSW(:) )/(ZRHODREF(:)*ZZW(:))  *      &
               ( X0EVAR*ZZW3(:)**XEX0EVAR+X1EVAR*ZCJ(:)*ZZW3(:)**XEX1EVAR )
        !
        ZZW(:) = MIN( ZRRS(:),  ZZW(:) *( ZZW4(:) - ZCF(:) ) )
        !
        ZRRS(:) = ZRRS(:) - ZZW(:)
        ZRVS(:) = ZRVS(:) + ZZW(:)
        ZTHS(:) = ZTHS(:) - ZZW(:)*ZLVFACT(:)
      END WHERE

    ELSE
      !wrong CSUBG_RR_EVAP case
      WRITE(*,*) 'wrong CSUBG_RR_EVAP case'
      CALL PRINT_MSG(NVERB_FATAL,'GEN','RAIN_ICE_WARM','')  
    END IF

    IF (LBUDGET_TH) CALL BUDGET (                                               &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                              4,'REVA_BU_RTH')
    IF (LBUDGET_RV) CALL BUDGET (                                               &
                 UNPACK(ZRVS(:),MASK=GMICRO(:,:,:),FIELD=PRVS)*PRHODJ(:,:,:),   &
                                                              6,'REVA_BU_RRV')
    IF (LBUDGET_RR) CALL BUDGET (                                               &
                     UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              8,'REVA_BU_RRR')
    ZW(:,:,:)=PEVAP3D(:,:,:)
    PEVAP3D(:,:,:)=UNPACK(ZZW(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:))
!
  END SUBROUTINE RAIN_ICE_WARM
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE RAIN_ICE_FAST_RS
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*       5.1    cloud droplet riming of the aggregates
!
  ZZW1(:,:) = 0.0
!
  ALLOCATE(GRIM(IMICRO))
! GRIM(:) = (ZRCT(:)>0.0) .AND. (ZRST(:)>0.0) .AND.            &
  GRIM(:) = (ZRCT(:)>XRTMIN(2)) .AND. (ZRST(:)>XRTMIN(5)) .AND.            &
                                (ZRCS(:)>0.0) .AND. (ZZT(:)<XTT)
  IGRIM = COUNT( GRIM(:) )
!
  IF( IGRIM>0 ) THEN
!
!        5.1.0  allocations
!
    ALLOCATE(ZVEC1(IGRIM))
    ALLOCATE(ZVEC2(IGRIM))
    ALLOCATE(IVEC1(IGRIM))
    ALLOCATE(IVEC2(IGRIM))
!
!        5.1.1  select the ZLBDAS
!
    ZVEC1(:) = PACK( ZLBDAS(:),MASK=GRIM(:) )
!
!        5.1.2  find the next lower indice for the ZLBDAS in the geometrical
!               set of Lbda_s used to tabulate some moments of the incomplete
!               gamma function
!
    ZVEC2(1:IGRIM) = MAX( 1.00001, MIN( FLOAT(NGAMINC)-0.00001,           &
                          XRIMINTP1 * LOG( ZVEC1(1:IGRIM) ) + XRIMINTP2 ) )
    IVEC2(1:IGRIM) = INT( ZVEC2(1:IGRIM) )
    ZVEC2(1:IGRIM) = ZVEC2(1:IGRIM) - FLOAT( IVEC2(1:IGRIM) )
!
!        5.1.3  perform the linear interpolation of the normalized
!               "2+XDS"-moment of the incomplete gamma function
!
    ZVEC1(1:IGRIM) =   XGAMINC_RIM1( IVEC2(1:IGRIM)+1 )* ZVEC2(1:IGRIM)      &
                     - XGAMINC_RIM1( IVEC2(1:IGRIM)   )*(ZVEC2(1:IGRIM) - 1.0)
    ZZW(:) = UNPACK( VECTOR=ZVEC1(:),MASK=GRIM,FIELD=0.0 )
!
!        5.1.4  riming of the small sized aggregates
!
    WHERE ( GRIM(:) )
      ZZW1(:,1) = MIN( ZRCS(:),                                &
                     XCRIMSS * ZZW(:) * ZRCT(:)                & ! RCRIMSS
                                      *   ZLBDAS(:)**XEXCRIMSS &
                                      * ZRHODREF(:)**(-XCEXVT) )
      ZRCS(:) = ZRCS(:) - ZZW1(:,1)
      ZRSS(:) = ZRSS(:) + ZZW1(:,1)
      ZTHS(:) = ZTHS(:) + ZZW1(:,1)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(RCRIMSS))
    END WHERE
!
!        5.1.5  perform the linear interpolation of the normalized
!               "XBS"-moment of the incomplete gamma function
!
    ZVEC1(1:IGRIM) =  XGAMINC_RIM2( IVEC2(1:IGRIM)+1 )* ZVEC2(1:IGRIM)      &
                    - XGAMINC_RIM2( IVEC2(1:IGRIM)   )*(ZVEC2(1:IGRIM) - 1.0)
    ZZW(:) = UNPACK( VECTOR=ZVEC1(:),MASK=GRIM,FIELD=0.0 )
!
!        5.1.6  riming-conversion of the large sized aggregates into graupeln
!
!
    WHERE ( GRIM(:) .AND. (ZRSS(:)>0.0) )
      ZZW1(:,2) = MIN( ZRCS(:),                     &
                   XCRIMSG * ZRCT(:)                & ! RCRIMSG
                           *  ZLBDAS(:)**XEXCRIMSG  &
                           * ZRHODREF(:)**(-XCEXVT) &
                           - ZZW1(:,1)              )
      ZZW1(:,3) = MIN( ZRSS(:),                         &
                       XSRIMCG * ZLBDAS(:)**XEXSRIMCG   & ! RSRIMCG
                               * (1.0 - ZZW(:) )/(PTSTEP*ZRHODREF(:)) )
      ZRCS(:) = ZRCS(:) - ZZW1(:,2)
      ZRSS(:) = ZRSS(:) - ZZW1(:,3)
      ZRGS(:) = ZRGS(:) + ZZW1(:,2)+ZZW1(:,3)
      ZTHS(:) = ZTHS(:) + ZZW1(:,2)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(RCRIMSG))
    END WHERE
    DEALLOCATE(IVEC2)
    DEALLOCATE(IVEC1)
    DEALLOCATE(ZVEC2)
    DEALLOCATE(ZVEC1)
  END IF
  IF (LBUDGET_TH) CALL BUDGET (                                               &
               UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                             4,'RIM_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                               &
                   UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             7,'RIM_BU_RRC')
  IF (LBUDGET_RS) CALL BUDGET (                                               &
                   UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                            10,'RIM_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (                                               &
                   UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                            11,'RIM_BU_RRG')
  DEALLOCATE(GRIM)
!
!*       5.2    rain accretion onto the aggregates
!
  ZZW1(:,2:3) = 0.0
  ALLOCATE(GACC(IMICRO))
   GACC(:) = (ZRRT(:)>XRTMIN(3)) .AND. (ZRST(:)>XRTMIN(5)) .AND.            &
                            (ZRRS(:)>0.0) .AND. (ZZT(:)<XTT)
  IGACC = COUNT( GACC(:) )
!
  IF( IGACC>0 ) THEN
!
!        5.2.0  allocations
!
    ALLOCATE(ZVEC1(IGACC))
    ALLOCATE(ZVEC2(IGACC))
    ALLOCATE(ZVEC3(IGACC))
    ALLOCATE(IVEC1(IGACC))
    ALLOCATE(IVEC2(IGACC))
!
!        5.2.1  select the (ZLBDAS,ZLBDAR) couplet
!
    ZVEC1(:) = PACK( ZLBDAS(:),MASK=GACC(:) )
    ZVEC2(:) = PACK( ZLBDAR(:),MASK=GACC(:) )
!
!        5.2.2  find the next lower indice for the ZLBDAS and for the ZLBDAR
!               in the geometrical set of (Lbda_s,Lbda_r) couplet use to
!               tabulate the RACCSS-kernel
!
    ZVEC1(1:IGACC) = MAX( 1.00001, MIN( FLOAT(NACCLBDAS)-0.00001,           &
                          XACCINTP1S * LOG( ZVEC1(1:IGACC) ) + XACCINTP2S ) )
    IVEC1(1:IGACC) = INT( ZVEC1(1:IGACC) )
    ZVEC1(1:IGACC) = ZVEC1(1:IGACC) - FLOAT( IVEC1(1:IGACC) )
!
    ZVEC2(1:IGACC) = MAX( 1.00001, MIN( FLOAT(NACCLBDAR)-0.00001,           &
                          XACCINTP1R * LOG( ZVEC2(1:IGACC) ) + XACCINTP2R ) )
    IVEC2(1:IGACC) = INT( ZVEC2(1:IGACC) )
    ZVEC2(1:IGACC) = ZVEC2(1:IGACC) - FLOAT( IVEC2(1:IGACC) )
!
!        5.2.3  perform the bilinear interpolation of the normalized
!               RACCSS-kernel
!
    DO JJ = 1,IGACC
      ZVEC3(JJ) =  (  XKER_RACCSS(IVEC1(JJ)+1,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                    - XKER_RACCSS(IVEC1(JJ)+1,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                          * ZVEC1(JJ) &
                 - (  XKER_RACCSS(IVEC1(JJ)  ,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                    - XKER_RACCSS(IVEC1(JJ)  ,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                          * (ZVEC1(JJ) - 1.0)
    END DO
    ZZW(:) = UNPACK( VECTOR=ZVEC3(:),MASK=GACC,FIELD=0.0 )
!
!        5.2.4  raindrop accretion on the small sized aggregates
!
    WHERE ( GACC(:) )
      ZZW1(:,2) =                                            & !! coef of RRACCS
              XFRACCSS*( ZLBDAS(:)**XCXS )*( ZRHODREF(:)**(-XCEXVT-1.) ) &
         *( XLBRACCS1/((ZLBDAS(:)**2)               ) +                  &
            XLBRACCS2/( ZLBDAS(:)    * ZLBDAR(:)    ) +                  &
            XLBRACCS3/(               (ZLBDAR(:)**2)) )/ZLBDAR(:)**4
      ZZW1(:,4) = MIN( ZRRS(:),ZZW1(:,2)*ZZW(:) )           ! RRACCSS
      ZRRS(:) = ZRRS(:) - ZZW1(:,4)
      ZRSS(:) = ZRSS(:) + ZZW1(:,4)
      ZTHS(:) = ZTHS(:) + ZZW1(:,4)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(RRACCSS))
    END WHERE
!
!        5.2.4b perform the bilinear interpolation of the normalized
!               RACCS-kernel
!
    DO JJ = 1,IGACC
      ZVEC3(JJ) =  (   XKER_RACCS(IVEC2(JJ)+1,IVEC1(JJ)+1)* ZVEC1(JJ)          &
                    -  XKER_RACCS(IVEC2(JJ)+1,IVEC1(JJ)  )*(ZVEC1(JJ) - 1.0) ) &
                                                                   * ZVEC2(JJ) &
                 - (   XKER_RACCS(IVEC2(JJ)  ,IVEC1(JJ)+1)* ZVEC1(JJ)          &
                    -  XKER_RACCS(IVEC2(JJ)  ,IVEC1(JJ)  )*(ZVEC1(JJ) - 1.0) ) &
                                                           * (ZVEC2(JJ) - 1.0)
    END DO
    ZZW1(:,2) = ZZW1(:,2)*UNPACK( VECTOR=ZVEC3(:),MASK=GACC(:),FIELD=0.0 )
                                                                       !! RRACCS!
!        5.2.5  perform the bilinear interpolation of the normalized
!               SACCRG-kernel
!
    DO JJ = 1,IGACC
      ZVEC3(JJ) =  (  XKER_SACCRG(IVEC2(JJ)+1,IVEC1(JJ)+1)* ZVEC1(JJ)          &
                    - XKER_SACCRG(IVEC2(JJ)+1,IVEC1(JJ)  )*(ZVEC1(JJ) - 1.0) ) &
                                                          * ZVEC2(JJ) &
                 - (  XKER_SACCRG(IVEC2(JJ)  ,IVEC1(JJ)+1)* ZVEC1(JJ)          &
                    - XKER_SACCRG(IVEC2(JJ)  ,IVEC1(JJ)  )*(ZVEC1(JJ) - 1.0) ) &
                                                          * (ZVEC2(JJ) - 1.0)
    END DO
    ZZW(:) = UNPACK( VECTOR=ZVEC3(:),MASK=GACC,FIELD=0.0 )
!
!        5.2.6  raindrop accretion-conversion of the large sized aggregates
!               into graupeln
!
    WHERE ( GACC(:) .AND. (ZRSS(:)>0.0) )
      ZZW1(:,2) = MAX( MIN( ZRRS(:),ZZW1(:,2)-ZZW1(:,4) ),0.0 )       ! RRACCSG
    END WHERE
    WHERE ( GACC(:) .AND. (ZRSS(:)>0.0) .AND. ZZW1(:,2)>0.0 )
      ZZW1(:,3) = MIN( ZRSS(:),XFSACCRG*ZZW(:)*                     & ! RSACCRG
            ( ZLBDAS(:)**(XCXS-XBS) )*( ZRHODREF(:)**(-XCEXVT-1.) ) &
           *( XLBSACCR1/((ZLBDAR(:)**2)               ) +           &
              XLBSACCR2/( ZLBDAR(:)    * ZLBDAS(:)    ) +           &
              XLBSACCR3/(               (ZLBDAS(:)**2)) )/ZLBDAR(:) )
      ZRRS(:) = ZRRS(:) - ZZW1(:,2)
      ZRSS(:) = ZRSS(:) - ZZW1(:,3)
      ZRGS(:) = ZRGS(:) + ZZW1(:,2)+ZZW1(:,3)
      ZTHS(:) = ZTHS(:) + ZZW1(:,2)*(ZLSFACT(:)-ZLVFACT(:)) !
                               ! f(L_f*(RRACCSG))
    END WHERE
    DEALLOCATE(IVEC2)
    DEALLOCATE(IVEC1)
    DEALLOCATE(ZVEC3)
    DEALLOCATE(ZVEC2)
    DEALLOCATE(ZVEC1)
  END IF
  DEALLOCATE(GACC)
  IF (LBUDGET_TH) CALL BUDGET (                                               &
               UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                             4,'ACC_BU_RTH')
  IF (LBUDGET_RR) CALL BUDGET (                                               &
                   UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             8,'ACC_BU_RRR')
  IF (LBUDGET_RS) CALL BUDGET (                                               &
                   UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                            10,'ACC_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (                                               &
                   UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                            11,'ACC_BU_RRG')
!
!*       5.3    Conversion-Melting of the aggregates
!
  ZZW(:) = 0.0
  WHERE( (ZRST(:)>XRTMIN(5)) .AND. (ZRSS(:)>0.0) .AND. (ZZT(:)>XTT) )
    ZZW(:) = ZRVT(:)*ZPRES(:)/((XMV/XMD)+ZRVT(:)) ! Vapor pressure
    ZZW(:) =  ZKA(:)*(XTT-ZZT(:)) +                                 &
               ( ZDV(:)*(XLVTT + ( XCPV - XCL ) * ( ZZT(:) - XTT )) &
                           *(XESTT-ZZW(:))/(XRV*ZZT(:))             )
!
! compute RSMLT
!
    ZZW(:)  = MIN( ZRSS(:), XFSCVMG*MAX( 0.0,( -ZZW(:) *             &
                           ( X0DEPS*       ZLBDAS(:)**XEX0DEPS +     &
                             X1DEPS*ZCJ(:)*ZLBDAS(:)**XEX1DEPS ) -   &
                                     ( ZZW1(:,1)+ZZW1(:,4) ) *       &
                              ( ZRHODREF(:)*XCL*(XTT-ZZT(:))) ) /    &
                                             ( ZRHODREF(:)*XLMTT ) ) )
!
! note that RSCVMG = RSMLT*XFSCVMG but no heat is exchanged (at the rate RSMLT)
! because the graupeln produced by this process are still icy!!!
!
    ZRSS(:) = ZRSS(:) - ZZW(:)
    ZRGS(:) = ZRGS(:) + ZZW(:)
  END WHERE
  IF (LBUDGET_RS) CALL BUDGET (                                                 &
                     UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             10,'CMEL_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (                                                 &
                     UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             11,'CMEL_BU_RRG')
!
  END SUBROUTINE RAIN_ICE_FAST_RS
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE RAIN_ICE_FAST_RG
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*       6.1    rain contact freezing
!
  ZZW1(:,3:4) = 0.0
  WHERE( (ZRIT(:)>XRTMIN(4)) .AND. (ZRRT(:)>XRTMIN(3)) .AND.  &
                             (ZRIS(:)>0.0) .AND. (ZRRS(:)>0.0) )
    ZZW1(:,3) = MIN( ZRIS(:),XICFRR * ZRIT(:)                & ! RICFRRG
                                    * ZLBDAR(:)**XEXICFRR    &
                                    * ZRHODREF(:)**(-XCEXVT) )
    ZZW1(:,4) = MIN( ZRRS(:),XRCFRI * ZCIT(:)                & ! RRCFRIG
                                    * ZLBDAR(:)**XEXRCFRI    &
                                    * ZRHODREF(:)**(-XCEXVT-1.) )
    ZRIS(:) = ZRIS(:) - ZZW1(:,3)
    ZRRS(:) = ZRRS(:) - ZZW1(:,4)
    ZRGS(:) = ZRGS(:) + ZZW1(:,3)+ZZW1(:,4)
    ZTHS(:) = ZTHS(:) + ZZW1(:,4)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*RRCFRIG)
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                 &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                              4,'CFRZ_BU_RTH')
  IF (LBUDGET_RR) CALL BUDGET (                                                 &
                     UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              8,'CFRZ_BU_RRR')
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              9,'CFRZ_BU_RRI')
  IF (LBUDGET_RG) CALL BUDGET (                                                 &
                     UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             11,'CFRZ_BU_RRG')
!
!*       6.2    compute the Dry growth case
!
  ZZW1(:,:) = 0.0
  WHERE( (ZRGT(:)>XRTMIN(6)) .AND. ((ZRCT(:)>XRTMIN(2) .AND. ZRCS(:)>0.0)) )
    ZZW(:) = ZLBDAG(:)**(XCXG-XDG-2.0) * ZRHODREF(:)**(-XCEXVT)
    ZZW1(:,1) = MIN( ZRCS(:),XFCDRYG * ZRCT(:) * ZZW(:) )             ! RCDRYG
  END WHERE
  WHERE( (ZRGT(:)>XRTMIN(6)) .AND. ((ZRIT(:)>XRTMIN(4) .AND. ZRIS(:)>0.0)) )
    ZZW(:) = ZLBDAG(:)**(XCXG-XDG-2.0) * ZRHODREF(:)**(-XCEXVT)
    ZZW1(:,2) = MIN( ZRIS(:),XFIDRYG * EXP( XCOLEXIG*(ZZT(:)-XTT) ) &
                                     * ZRIT(:) * ZZW(:) )             ! RIDRYG
  END WHERE
!
!*       6.2.1  accretion of aggregates on the graupeln
!
  ALLOCATE(GDRY(IMICRO))
  GDRY(:) = (ZRST(:)>XRTMIN(5)) .AND. (ZRGT(:)>XRTMIN(6)) .AND. (ZRSS(:)>0.0)
  IGDRY = COUNT( GDRY(:) )
!
  IF( IGDRY>0 ) THEN
!
!*       6.2.2  allocations
!
    ALLOCATE(ZVEC1(IGDRY))
    ALLOCATE(ZVEC2(IGDRY))
    ALLOCATE(ZVEC3(IGDRY))
    ALLOCATE(IVEC1(IGDRY))
    ALLOCATE(IVEC2(IGDRY))
!
!*       6.2.3  select the (ZLBDAG,ZLBDAS) couplet
!
    ZVEC1(:) = PACK( ZLBDAG(:),MASK=GDRY(:) )
    ZVEC2(:) = PACK( ZLBDAS(:),MASK=GDRY(:) )
!
!*       6.2.4  find the next lower indice for the ZLBDAG and for the ZLBDAS
!               in the geometrical set of (Lbda_g,Lbda_s) couplet use to
!               tabulate the SDRYG-kernel
!
    ZVEC1(1:IGDRY) = MAX( 1.00001, MIN( FLOAT(NDRYLBDAG)-0.00001,           &
                          XDRYINTP1G * LOG( ZVEC1(1:IGDRY) ) + XDRYINTP2G ) )
    IVEC1(1:IGDRY) = INT( ZVEC1(1:IGDRY) )
    ZVEC1(1:IGDRY) = ZVEC1(1:IGDRY) - FLOAT( IVEC1(1:IGDRY) )
!
    ZVEC2(1:IGDRY) = MAX( 1.00001, MIN( FLOAT(NDRYLBDAS)-0.00001,           &
                          XDRYINTP1S * LOG( ZVEC2(1:IGDRY) ) + XDRYINTP2S ) )
    IVEC2(1:IGDRY) = INT( ZVEC2(1:IGDRY) )
    ZVEC2(1:IGDRY) = ZVEC2(1:IGDRY) - FLOAT( IVEC2(1:IGDRY) )
!
!*       6.2.5  perform the bilinear interpolation of the normalized
!               SDRYG-kernel
!
    DO JJ = 1,IGDRY
      ZVEC3(JJ) =  (  XKER_SDRYG(IVEC1(JJ)+1,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                    - XKER_SDRYG(IVEC1(JJ)+1,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                         * ZVEC1(JJ) &
                 - (  XKER_SDRYG(IVEC1(JJ)  ,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                    - XKER_SDRYG(IVEC1(JJ)  ,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                         * (ZVEC1(JJ) - 1.0)
    END DO
    ZZW(:) = UNPACK( VECTOR=ZVEC3(:),MASK=GDRY,FIELD=0.0 )
!
    WHERE( GDRY(:) )
      ZZW1(:,3) = MIN( ZRSS(:),XFSDRYG*ZZW(:)                         & ! RSDRYG
                                      * EXP( XCOLEXSG*(ZZT(:)-XTT) )  &
                    *( ZLBDAS(:)**(XCXS-XBS) )*( ZLBDAG(:)**XCXG )    &
                    *( ZRHODREF(:)**(-XCEXVT-1.) )                    &
                         *( XLBSDRYG1/( ZLBDAG(:)**2              ) + &
                            XLBSDRYG2/( ZLBDAG(:)   * ZLBDAS(:)   ) + &
                            XLBSDRYG3/(               ZLBDAS(:)**2) ) )
    END WHERE
    DEALLOCATE(IVEC2)
    DEALLOCATE(IVEC1)
    DEALLOCATE(ZVEC3)
    DEALLOCATE(ZVEC2)
    DEALLOCATE(ZVEC1)
  END IF
!
!*       6.2.6  accretion of raindrops on the graupeln
!
  GDRY(:) = (ZRRT(:)>XRTMIN(3)) .AND. (ZRGT(:)>XRTMIN(6)) .AND. (ZRRS(:)>0.0)
  IGDRY = COUNT( GDRY(:) )
!
  IF( IGDRY>0 ) THEN
!
!*       6.2.7  allocations
!
    ALLOCATE(ZVEC1(IGDRY))
    ALLOCATE(ZVEC2(IGDRY))
    ALLOCATE(ZVEC3(IGDRY))
    ALLOCATE(IVEC1(IGDRY))
    ALLOCATE(IVEC2(IGDRY))
!
!*       6.2.8  select the (ZLBDAG,ZLBDAR) couplet
!
    ZVEC1(:) = PACK( ZLBDAG(:),MASK=GDRY(:) )
    ZVEC2(:) = PACK( ZLBDAR(:),MASK=GDRY(:) )
!
!*       6.2.9  find the next lower indice for the ZLBDAG and for the ZLBDAR
!               in the geometrical set of (Lbda_g,Lbda_r) couplet use to
!               tabulate the RDRYG-kernel
!
    ZVEC1(1:IGDRY) = MAX( 1.00001, MIN( FLOAT(NDRYLBDAG)-0.00001,           &
                          XDRYINTP1G * LOG( ZVEC1(1:IGDRY) ) + XDRYINTP2G ) )
    IVEC1(1:IGDRY) = INT( ZVEC1(1:IGDRY) )
    ZVEC1(1:IGDRY) = ZVEC1(1:IGDRY) - FLOAT( IVEC1(1:IGDRY) )
!
    ZVEC2(1:IGDRY) = MAX( 1.00001, MIN( FLOAT(NDRYLBDAR)-0.00001,           &
                          XDRYINTP1R * LOG( ZVEC2(1:IGDRY) ) + XDRYINTP2R ) )
    IVEC2(1:IGDRY) = INT( ZVEC2(1:IGDRY) )
    ZVEC2(1:IGDRY) = ZVEC2(1:IGDRY) - FLOAT( IVEC2(1:IGDRY) )
!
!*       6.2.10 perform the bilinear interpolation of the normalized
!               RDRYG-kernel
!
    DO JJ = 1,IGDRY
      ZVEC3(JJ) =  (  XKER_RDRYG(IVEC1(JJ)+1,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                    - XKER_RDRYG(IVEC1(JJ)+1,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                                  * ZVEC1(JJ) &
                 - (  XKER_RDRYG(IVEC1(JJ)  ,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                    - XKER_RDRYG(IVEC1(JJ)  ,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                         * (ZVEC1(JJ) - 1.0)
    END DO
    ZZW(:) = UNPACK( VECTOR=ZVEC3(:),MASK=GDRY,FIELD=0.0 )
!
    WHERE( GDRY(:) )
      ZZW1(:,4) = MIN( ZRRS(:),XFRDRYG*ZZW(:)                    & ! RRDRYG
                        *( ZLBDAR(:)**(-4) )*( ZLBDAG(:)**XCXG ) &
                               *( ZRHODREF(:)**(-XCEXVT-1.) )   &
                    *( XLBRDRYG1/( ZLBDAG(:)**2              ) + &
                       XLBRDRYG2/( ZLBDAG(:)   * ZLBDAR(:)   ) + &
                       XLBRDRYG3/(               ZLBDAR(:)**2) ) )
    END WHERE
    DEALLOCATE(IVEC2)
    DEALLOCATE(IVEC1)
    DEALLOCATE(ZVEC3)
    DEALLOCATE(ZVEC2)
    DEALLOCATE(ZVEC1)
  END IF
!
  ZRDRYG(:) = ZZW1(:,1) + ZZW1(:,2) + ZZW1(:,3) + ZZW1(:,4)
  DEALLOCATE(GDRY)
!
!*       6.3    compute the Wet growth case
!
  ZZW(:) = 0.0
  ZRWETG(:) = 0.0
  WHERE( ZRGT(:)>XRTMIN(6) )
    ZZW1(:,5) = MIN( ZRIS(:),                                    &
                ZZW1(:,2) / (XCOLIG*EXP(XCOLEXIG*(ZZT(:)-XTT)) ) ) ! RIWETG
    ZZW1(:,6) = MIN( ZRSS(:),                                    &
                ZZW1(:,3) / (XCOLSG*EXP(XCOLEXSG*(ZZT(:)-XTT)) ) ) ! RSWETG
!
    ZZW(:) = ZRVT(:)*ZPRES(:)/((XMV/XMD)+ZRVT(:)) ! Vapor pressure
    ZZW(:) =   ZKA(:)*(XTT-ZZT(:)) +                              &
             ( ZDV(:)*(XLVTT + ( XCPV - XCL ) * ( ZZT(:) - XTT )) &
                           *(XESTT-ZZW(:))/(XRV*ZZT(:))           )
!
! compute RWETG
!
    ZRWETG(:)=MAX( 0.0,                                               &
                 ( ZZW(:) * ( X0DEPG*       ZLBDAG(:)**XEX0DEPG +     &
                              X1DEPG*ZCJ(:)*ZLBDAG(:)**XEX1DEPG ) +   &
                 ( ZZW1(:,5)+ZZW1(:,6) ) *                            &
                 ( ZRHODREF(:)*(XLMTT+(XCI-XCL)*(XTT-ZZT(:)))   ) ) / &
                            ( ZRHODREF(:)*(XLMTT-XCL*(XTT-ZZT(:))) )   )
  END WHERE
!
!*       6.4    Select Wet or Dry case
!
   ZZW(:) = 0.0
  IF     ( KRR == 7 ) THEN
   WHERE( ZRGT(:)>XRTMIN(6) .AND. ZZT(:)<XTT                            &
                                        .AND.                          & ! Wet
                              ZRDRYG(:)>=ZRWETG(:) .AND. ZRWETG(:)>0.0 ) ! case
     ZZW(:) = ZRWETG(:) - ZZW1(:,5) - ZZW1(:,6) ! RCWETG+RRWETG
!
! limitation of the available rainwater mixing ratio (RRWETH < RRS !)
!
    ZZW1(:,7) = MAX( 0.0,MIN( ZZW(:),ZRRS(:)+ZZW1(:,1) ) )
    ZUSW(:)   = ZZW1(:,7) / ZZW(:)
    ZZW1(:,5) = ZZW1(:,5)*ZUSW(:)
    ZZW1(:,6) = ZZW1(:,6)*ZUSW(:)
    ZRWETG(:) = ZZW1(:,7) + ZZW1(:,5) + ZZW1(:,6)
!
    ZRCS(:) = ZRCS(:) - ZZW1(:,1)
    ZRIS(:) = ZRIS(:) - ZZW1(:,5)
    ZRSS(:) = ZRSS(:) - ZZW1(:,6)
!
! assume a linear percent of conversion of graupel into hail
!
    ZRGS(:) = ZRGS(:) + ZRWETG(:)                     !     Wet growth
    ZZW(:)  = ZRGS(:)*ZRDRYG(:)/(ZRWETG(:)+ZRDRYG(:)) !        and
    ZRGS(:) = ZRGS(:) - ZZW(:)                        !   partial conversion
    ZRHS(:) = ZRHS(:) + ZZW(:)                        ! of the graupel into hail
!
    ZRRS(:) = MAX( 0.0,ZRRS(:) - ZZW1(:,7) + ZZW1(:,1) )
    ZTHS(:) = ZTHS(:) + ZZW1(:,7)*(ZLSFACT(:)-ZLVFACT(:))
                                                 ! f(L_f*(RCWETG+RRWETG))
   END WHERE
   ELSE IF( KRR == 6 ) THEN
     WHERE( ZRGT(:)>XRTMIN(6) .AND. ZZT(:)<XTT                            &
                                        .AND.                          & ! Wet
                              ZRDRYG(:)>=ZRWETG(:) .AND. ZRWETG(:)>0.0 ) ! case
    ZZW(:)  = ZRWETG(:)
    ZRCS(:) = ZRCS(:) - ZZW1(:,1)
    ZRIS(:) = ZRIS(:) - ZZW1(:,5)
    ZRSS(:) = ZRSS(:) - ZZW1(:,6)
    ZRGS(:) = ZRGS(:) + ZZW(:)
!
    ZRRS(:) = ZRRS(:) - ZZW(:) + ZZW1(:,5) + ZZW1(:,6) + ZZW1(:,1)
    ZTHS(:) = ZTHS(:) + (ZZW(:)-ZZW1(:,5)-ZZW1(:,6))*(ZLSFACT(:)-ZLVFACT(:))
                                                 ! f(L_f*(RCWETG+RRWETG))
   END WHERE
 END IF
  IF (LBUDGET_TH) CALL BUDGET (                                                 &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                              4,'WETG_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                                 &
                     UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              7,'WETG_BU_RRC')
  IF (LBUDGET_RR) CALL BUDGET (                                                 &
                     UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              8,'WETG_BU_RRR')
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              9,'WETG_BU_RRI')
  IF (LBUDGET_RS) CALL BUDGET (                                                 &
                     UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             10,'WETG_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (                                                 &
                     UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             11,'WETG_BU_RRG')
  IF ( KRR == 7 ) THEN
    IF (LBUDGET_RH) CALL BUDGET (                                                 &
                     UNPACK(ZRHS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             12,'WETG_BU_RRH')
  END IF

!
  WHERE( ZRGT(:)>XRTMIN(6) .AND. ZZT(:)<XTT                            &
                                        .AND.                          &
                               ZRDRYG(:)<ZRWETG(:) .AND. ZRDRYG(:)>0.0 ) ! Dry
    ZRCS(:) = ZRCS(:) - ZZW1(:,1)
    ZRIS(:) = ZRIS(:) - ZZW1(:,2)
    ZRSS(:) = ZRSS(:) - ZZW1(:,3)
    ZRRS(:) = ZRRS(:) - ZZW1(:,4)
    ZRGS(:) = ZRGS(:) + ZRDRYG(:)
    ZTHS(:) = ZTHS(:) + (ZZW1(:,1)+ZZW1(:,4))*(ZLSFACT(:)-ZLVFACT(:)) !
                      ! f(L_f*(RCDRYG+RRDRYG))
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                    &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                              4,'DRYG_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                                 &
                     UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              7,'DRYG_BU_RRC')
  IF (LBUDGET_RR) CALL BUDGET (                                                 &
                     UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              8,'DRYG_BU_RRR')
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              9,'DRYG_BU_RRI')
  IF (LBUDGET_RS) CALL BUDGET (                                                 &
                     UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             10,'DRYG_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (                                                 &
                     UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             11,'DRYG_BU_RRG')
!
!      WHERE ( ZZT(:) > XTT ) ! RSWETG case only
!        ZRSS(:) = ZRSS(:) - ZZW1(:,6)
!        ZRGS(:) = ZRGS(:) + ZZW1(:,6)
!      END WHERE
!
!*       6.5    Melting of the graupeln
!
  ZZW(:) = 0.0
   WHERE( (ZRGT(:)>XRTMIN(6)) .AND. (ZRGS(:)>0.0) .AND. (ZZT(:)>XTT) )
    ZZW(:) = ZRVT(:)*ZPRES(:)/((XMV/XMD)+ZRVT(:)) ! Vapor pressure
    ZZW(:) =  ZKA(:)*(XTT-ZZT(:)) +                                 &
               ( ZDV(:)*(XLVTT + ( XCPV - XCL ) * ( ZZT(:) - XTT )) &
                           *(XESTT-ZZW(:))/(XRV*ZZT(:))             )
!
! compute RGMLTR
!
    ZZW(:)  = MIN( ZRGS(:), MAX( 0.0,( -ZZW(:) *                     &
                           ( X0DEPG*       ZLBDAG(:)**XEX0DEPG +     &
                             X1DEPG*ZCJ(:)*ZLBDAG(:)**XEX1DEPG ) -   &
                                     ( ZZW1(:,1)+ZZW1(:,4) ) *       &
                              ( ZRHODREF(:)*XCL*(XTT-ZZT(:))) ) /    &
                                             ( ZRHODREF(:)*XLMTT ) ) )
    ZRRS(:) = ZRRS(:) + ZZW(:)
    ZRGS(:) = ZRGS(:) - ZZW(:)
    ZTHS(:) = ZTHS(:) - ZZW(:)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(-RGMLTR))
  END WHERE
    IF (LBUDGET_TH) CALL BUDGET (                                                 &
                   UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                                4,'GMLT_BU_RTH')
    IF (LBUDGET_RR) CALL BUDGET (                                                 &
                       UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                                8,'GMLT_BU_RRR')
    IF (LBUDGET_RG) CALL BUDGET (                                                 &
                       UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                               11,'GMLT_BU_RRG')
!
  END SUBROUTINE RAIN_ICE_FAST_RG
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE RAIN_ICE_FAST_RH
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
  ALLOCATE(GHAIL(IMICRO))
  GHAIL(:) = ZRHT(:)>XRTMIN(7)
  IHAIL = COUNT(GHAIL(:))
!
  IF( IHAIL>0 ) THEN
!
!*       7.2    compute the Wet growth of hail
!
    WHERE ( GHAIL(:) )
      ZLBDAH(:)  = XLBH*( ZRHODREF(:)*MAX( ZRHT(:),XRTMIN(7) ) )**XLBEXH
    END WHERE
!
    ZZW1(:,:) = 0.0
    WHERE( GHAIL(:) .AND. ((ZRCT(:)>XRTMIN(2) .AND. ZRCS(:)>0.0)) )
      ZZW(:) = ZLBDAH(:)**(XCXH-XDH-2.0) * ZRHODREF(:)**(-XCEXVT)
      ZZW1(:,1) = MIN( ZRCS(:),XFWETH * ZRCT(:) * ZZW(:) )             ! RCWETH
    END WHERE
    WHERE( GHAIL(:) .AND. ((ZRIT(:)>XRTMIN(4) .AND. ZRIS(:)>0.0)) )
      ZZW(:) = ZLBDAH(:)**(XCXH-XDH-2.0) * ZRHODREF(:)**(-XCEXVT)
      ZZW1(:,2) = MIN( ZRIS(:),XFWETH * ZRIT(:) * ZZW(:) )             ! RIWETH
    END WHERE
!
!*       7.2.1  accretion of aggregates on the hailstones
!
    ALLOCATE(GWET(IMICRO))
    GWET(:) = GHAIL(:) .AND. (ZRST(:)>XRTMIN(5) .AND. ZRSS(:)>0.0)
    IGWET = COUNT( GWET(:) )
!
    IF( IGWET>0 ) THEN
!
!*       7.2.2  allocations
!
      ALLOCATE(ZVEC1(IGWET))
      ALLOCATE(ZVEC2(IGWET))
      ALLOCATE(ZVEC3(IGWET))
      ALLOCATE(IVEC1(IGWET))
      ALLOCATE(IVEC2(IGWET))
!
!*       7.2.3  select the (ZLBDAH,ZLBDAS) couplet
!
      ZVEC1(:) = PACK( ZLBDAH(:),MASK=GWET(:) )
      ZVEC2(:) = PACK( ZLBDAS(:),MASK=GWET(:) )
!
!*       7.2.4  find the next lower indice for the ZLBDAG and for the ZLBDAS
!               in the geometrical set of (Lbda_h,Lbda_s) couplet use to
!               tabulate the SWETH-kernel
!
      ZVEC1(1:IGWET) = MAX( 1.00001, MIN( FLOAT(NWETLBDAH)-0.00001,           &
                            XWETINTP1H * LOG( ZVEC1(1:IGWET) ) + XWETINTP2H ) )
      IVEC1(1:IGWET) = INT( ZVEC1(1:IGWET) )
      ZVEC1(1:IGWET) = ZVEC1(1:IGWET) - FLOAT( IVEC1(1:IGWET) )
!
      ZVEC2(1:IGWET) = MAX( 1.00001, MIN( FLOAT(NWETLBDAS)-0.00001,           &
                            XWETINTP1S * LOG( ZVEC2(1:IGWET) ) + XWETINTP2S ) )
      IVEC2(1:IGWET) = INT( ZVEC2(1:IGWET) )
      ZVEC2(1:IGWET) = ZVEC2(1:IGWET) - FLOAT( IVEC2(1:IGWET) )
!
!*       7.2.5  perform the bilinear interpolation of the normalized
!               SWETH-kernel
!
      DO JJ = 1,IGWET
        ZVEC3(JJ) = (  XKER_SWETH(IVEC1(JJ)+1,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                     - XKER_SWETH(IVEC1(JJ)+1,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                                   * ZVEC1(JJ) &
                   - ( XKER_SWETH(IVEC1(JJ)  ,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                     - XKER_SWETH(IVEC1(JJ)  ,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                          * (ZVEC1(JJ) - 1.0)
      END DO
      ZZW(:) = UNPACK( VECTOR=ZVEC3(:),MASK=GWET,FIELD=0.0 )
!
      WHERE( GWET(:) )
        ZZW1(:,3) = MIN( ZRSS(:),XFSWETH*ZZW(:)                       & ! RSWETH
                      *( ZLBDAS(:)**(XCXS-XBS) )*( ZLBDAH(:)**XCXH )  &
                         *( ZRHODREF(:)**(-XCEXVT-1.) )               &
                         *( XLBSWETH1/( ZLBDAH(:)**2              ) + &
                            XLBSWETH2/( ZLBDAH(:)   * ZLBDAS(:)   ) + &
                            XLBSWETH3/(               ZLBDAS(:)**2) ) )
      END WHERE
      DEALLOCATE(IVEC2)
      DEALLOCATE(IVEC1)
      DEALLOCATE(ZVEC3)
      DEALLOCATE(ZVEC2)
      DEALLOCATE(ZVEC1)
    END IF
!
!*       7.2.6  accretion of graupeln on the hailstones
!
    GWET(:) = GHAIL(:) .AND. (ZRGT(:)>XRTMIN(6) .AND. ZRGS(:)>0.0)
    IGWET = COUNT( GWET(:) )
!
    IF( IGWET>0 ) THEN
!
!*       7.2.7  allocations
!
      ALLOCATE(ZVEC1(IGWET))
      ALLOCATE(ZVEC2(IGWET))
      ALLOCATE(ZVEC3(IGWET))
      ALLOCATE(IVEC1(IGWET))
      ALLOCATE(IVEC2(IGWET))
!
!*       7.2.8  select the (ZLBDAH,ZLBDAG) couplet
!
      ZVEC1(:) = PACK( ZLBDAH(:),MASK=GWET(:) )
      ZVEC2(:) = PACK( ZLBDAG(:),MASK=GWET(:) )
!
!*       7.2.9  find the next lower indice for the ZLBDAH and for the ZLBDAG
!               in the geometrical set of (Lbda_h,Lbda_g) couplet use to
!               tabulate the GWETH-kernel
!
      ZVEC1(1:IGWET) = MAX( 1.00001, MIN( FLOAT(NWETLBDAG)-0.00001,           &
                            XWETINTP1H * LOG( ZVEC1(1:IGWET) ) + XWETINTP2H ) )
      IVEC1(1:IGWET) = INT( ZVEC1(1:IGWET) )
      ZVEC1(1:IGWET) = ZVEC1(1:IGWET) - FLOAT( IVEC1(1:IGWET) )
!
      ZVEC2(1:IGWET) = MAX( 1.00001, MIN( FLOAT(NWETLBDAG)-0.00001,           &
                            XWETINTP1G * LOG( ZVEC2(1:IGWET) ) + XWETINTP2G ) )
      IVEC2(1:IGWET) = INT( ZVEC2(1:IGWET) )
      ZVEC2(1:IGWET) = ZVEC2(1:IGWET) - FLOAT( IVEC2(1:IGWET) )
!
!*       7.2.10 perform the bilinear interpolation of the normalized
!               GWETH-kernel
!
      DO JJ = 1,IGWET
        ZVEC3(JJ) = (  XKER_GWETH(IVEC1(JJ)+1,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                     - XKER_GWETH(IVEC1(JJ)+1,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                                   * ZVEC1(JJ) &
                  - (  XKER_GWETH(IVEC1(JJ)  ,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                     - XKER_GWETH(IVEC1(JJ)  ,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                          * (ZVEC1(JJ) - 1.0)
      END DO
      ZZW(:) = UNPACK( VECTOR=ZVEC3(:),MASK=GWET,FIELD=0.0 )
!
      WHERE( GWET(:) )
        ZZW1(:,5) = MAX(MIN( ZRGS(:),XFGWETH*ZZW(:)                       & ! RGWETH
                      *( ZLBDAG(:)**(XCXG-XBG) )*( ZLBDAH(:)**XCXH )  &
                         *( ZRHODREF(:)**(-XCEXVT-1.) )               &
                         *( XLBGWETH1/( ZLBDAH(:)**2              ) + &
                            XLBGWETH2/( ZLBDAH(:)   * ZLBDAG(:)   ) + &
                            XLBGWETH3/(               ZLBDAG(:)**2) ) ),0. )
      END WHERE
      DEALLOCATE(IVEC2)
      DEALLOCATE(IVEC1)
      DEALLOCATE(ZVEC3)
      DEALLOCATE(ZVEC2)
      DEALLOCATE(ZVEC1)
    END IF
    DEALLOCATE(GWET)
!
!*       7.3    compute the Wet growth of hail
!
    ZZW(:) = 0.0
    WHERE( GHAIL(:) .AND. ZZT(:)<XTT )
      ZZW(:) = ZRVT(:)*ZPRES(:)/((XMV/XMD)+ZRVT(:)) ! Vapor pressure
      ZZW(:) = ZKA(:)*(XTT-ZZT(:)) +                                 &
                ( ZDV(:)*(XLVTT + ( XCPV - XCL ) * ( ZZT(:) - XTT )) &
                            *(XESTT-ZZW(:))/(XRV*ZZT(:))             )
!
! compute RWETH
!
      ZZW(:)  =  MAX(0.,  ( ZZW(:) * ( X0DEPH*       ZLBDAH(:)**XEX0DEPH +     &
                                X1DEPH*ZCJ(:)*ZLBDAH(:)**XEX1DEPH ) +   &
                   ( ZZW1(:,2)+ZZW1(:,3)+ZZW1(:,5) ) *                  &
                   ( ZRHODREF(:)*(XLMTT+(XCI-XCL)*(XTT-ZZT(:)))   ) ) / &
                         ( ZRHODREF(:)*(XLMTT-XCL*(XTT-ZZT(:))) ) )
!
      ZZW1(:,6) = MAX( ZZW(:) - ZZW1(:,2) - ZZW1(:,3) - ZZW1(:,5),0.) ! RCWETH+RRWETH
    END WHERE
    WHERE ( GHAIL(:) .AND. ZZT(:)<XTT  .AND. ZZW1(:,6)/=0.)
!
! limitation of the available rainwater mixing ratio (RRWETH < RRS !)
!
      ZZW1(:,4) = MAX( 0.0,MIN( ZZW1(:,6),ZRRS(:)+ZZW1(:,1) ) )
      ZUSW(:)   = ZZW1(:,4) / ZZW1(:,6)
      ZZW1(:,2) = ZZW1(:,2)*ZUSW(:)
      ZZW1(:,3) = ZZW1(:,3)*ZUSW(:)
      ZZW1(:,5) = ZZW1(:,5)*ZUSW(:)
      ZZW(:)    = ZZW1(:,4) + ZZW1(:,2) + ZZW1(:,3) + ZZW1(:,5)
!
!*       7.1.6  integrate the Wet growth of hail
!
      ZRCS(:) = ZRCS(:) - ZZW1(:,1)
      ZRIS(:) = ZRIS(:) - ZZW1(:,2)
      ZRSS(:) = ZRSS(:) - ZZW1(:,3)
      ZRGS(:) = ZRGS(:) - ZZW1(:,5)
      ZRHS(:) = ZRHS(:) + ZZW(:)
      ZRRS(:) = MAX( 0.0,ZRRS(:) - ZZW1(:,4) + ZZW1(:,1) )
      ZTHS(:) = ZTHS(:) + ZZW1(:,4)*(ZLSFACT(:)-ZLVFACT(:))
                           ! f(L_f*(RCWETH+RRWETH))
    END WHERE
  END IF
    IF (LBUDGET_TH) CALL BUDGET (                                                 &
                   UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),&
                                                                4,'WETH_BU_RTH')
    IF (LBUDGET_RC) CALL BUDGET (                                                 &
                       UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0), &
                                                                7,'WETH_BU_RRC')
    IF (LBUDGET_RR) CALL BUDGET (                                                 &
                       UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0), &
                                                                8,'WETH_BU_RRR')
    IF (LBUDGET_RI) CALL BUDGET (                                                 &
                       UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0), &
                                                                9,'WETH_BU_RRI')
    IF (LBUDGET_RS) CALL BUDGET (                                                 &
                       UNPACK(ZRSS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0), &
                                                               10,'WETH_BU_RRS')
    IF (LBUDGET_RG) CALL BUDGET (                                                 &
                       UNPACK(ZRGS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0), &
                                                               11,'WETH_BU_RRG')
    IF (LBUDGET_RH) CALL BUDGET (                                                 &
                       UNPACK(ZRHS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0), &
                                                               12,'WETH_BU_RRH')
!
!
! ici LRECONVH et un flag pour autoriser une reconversion partielle de
!la grele en gresil
!
!  IF( IHAIL>0  ) THEN
!
!UPG_CD
!
!
!*       7.45   Conversion of the hailstones into graupel
!
!    XDUMMY6=0.01E-3
!    XDUMMY7=0.001E-3
!    WHERE( ZRHT(:)<XDUMMY6 .AND. ZRCT(:)<XDUMMY7 .AND. ZZT(:)<XTT )
!      ZZW(:) = MIN( 1.0,MAX( 0.0,1.0-(ZRCT(:)/XDUMMY7) ) )
!
! assume a linear percent conversion rate of hail into graupel
!
!      ZZW(:)  = ZRHS(:)*ZZW(:)
!      ZRGS(:) = ZRGS(:) + ZZW(:)                      !   partial conversion
!      ZRHS(:) = ZRHS(:) - ZZW(:)                      ! of hail into graupel
!
!    END WHERE
!  END IF




  IF( IHAIL>0 ) THEN
!
!*       7.5    Melting of the hailstones
!
    ZZW(:) = 0.0
    WHERE( GHAIL(:) .AND. (ZRHS(:)>0.0) .AND. (ZZT(:)>XTT) )
      ZZW(:) = ZRVT(:)*ZPRES(:)/((XMV/XMD)+ZRVT(:)) ! Vapor pressure
      ZZW(:) = ZKA(:)*(XTT-ZZT(:)) +                              &
             ( ZDV(:)*(XLVTT + ( XCPV - XCL ) * ( ZZT(:) - XTT )) &
                             *(XESTT-ZZW(:))/(XRV*ZZT(:))         )
!
! compute RHMLTR
!
      ZZW(:)  = MIN( ZRHS(:), MAX( 0.0,( -ZZW(:) *                     &
                             ( X0DEPH*       ZLBDAH(:)**XEX0DEPH +     &
                               X1DEPH*ZCJ(:)*ZLBDAH(:)**XEX1DEPH ) -   &
                      ZZW1(:,6)*( ZRHODREF(:)*XCL*(XTT-ZZT(:))) ) /    &
                                               ( ZRHODREF(:)*XLMTT ) ) )
      ZRRS(:) = ZRRS(:) + ZZW(:)
      ZRHS(:) = ZRHS(:) - ZZW(:)
      ZTHS(:) = ZTHS(:) - ZZW(:)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(-RHMLTR))
    END WHERE
  END IF
  DEALLOCATE(GHAIL)
    IF (LBUDGET_TH) CALL BUDGET (                                                 &
                   UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),&
                                                                4,'HMLT_BU_RTH')
    IF (LBUDGET_RR) CALL BUDGET (                                                 &
                       UNPACK(ZRRS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0), &
                                                                8,'HMLT_BU_RRR')
    IF (LBUDGET_RH) CALL BUDGET (                                                 &
                       UNPACK(ZRHS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0), &
                                                               12,'HMLT_BU_RRH')
!
  END SUBROUTINE RAIN_ICE_FAST_RH
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE RAIN_ICE_FAST_RI
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*       7.1    cloud ice melting
!
  ZZW(:) = 0.0
  WHERE( (ZRIS(:)>0.0) .AND. (ZZT(:)>XTT) )
    ZZW(:)  = ZRIS(:)
    ZRCS(:) = ZRCS(:) + ZRIS(:)
    ZTHS(:) = ZTHS(:) - ZRIS(:)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(-RIMLTC))
    ZRIS(:) = 0.0
    ZCIT(:) = 0.0
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                 &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                              4,'IMLT_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                                 &
                     UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              7,'IMLT_BU_RRC')
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                              9,'IMLT_BU_RRI')
!
!*       7.2    Bergeron-Findeisen effect: RCBERI
!
  ZZW(:) = 0.0
  WHERE( (ZRCS(:)>0.0) .AND. (ZSSI(:)>0.0) .AND. &
         (ZRIT(:)>XRTMIN(4)) .AND. (ZCIT(:)>0.0)       )
    ZZW(:) = MIN(1.E8,XLBI*( ZRHODREF(:)*ZRIT(:)/ZCIT(:) )**XLBEXI) ! Lbda_i
    ZZW(:) = MIN( ZRCS(:),( ZSSI(:) / (ZRHODREF(:)*ZAI(:)) ) * ZCIT(:) * &
                  ( X0DEPI/ZZW(:) + X2DEPI*ZCJ(:)*ZCJ(:)/ZZW(:)**(XDI+2.0) ) )
    ZRCS(:) = ZRCS(:) - ZZW(:)
    ZRIS(:) = ZRIS(:) + ZZW(:)
    ZTHS(:) = ZTHS(:) + ZZW(:)*(ZLSFACT(:)-ZLVFACT(:)) ! f(L_f*(RCBERI))
  END WHERE
  IF (LBUDGET_TH) CALL BUDGET (                                                 &
                 UNPACK(ZTHS(:),MASK=GMICRO(:,:,:),FIELD=PTHS)*PRHODJ(:,:,:),   &
                                                             4,'BERFI_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                                 &
                     UNPACK(ZRCS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             7,'BERFI_BU_RRC')
  IF (LBUDGET_RI) CALL BUDGET (                                                 &
                     UNPACK(ZRIS(:)*ZRHODJ(:),MASK=GMICRO(:,:,:),FIELD=0.0),    &
                                                             9,'BERFI_BU_RRI')
!
  END SUBROUTINE RAIN_ICE_FAST_RI
!
SUBROUTINE RAINFR_VERT(ZPRFR, ZRR)

IMPLICIT NONE
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: ZPRFR !Precipitation fraction
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: ZRR !Rain field
!
!-------------------------------------------------------------------------------
INTEGER :: JI, JJ, JK
!
DO JI = IIB,IIE
   DO JJ = IJB, IJE
      ZPRFR(JI,JJ,IKE)=0.
      DO JK=IKE-KKL, IKB, -KKL
         IF (ZRR(JI,JJ,JK) .GT. XRTMIN(3)) THEN
            ZPRFR(JI,JJ,JK)=MAX(ZPRFR(JI,JJ,JK),ZPRFR(JI,JJ,JK+KKL))
            IF (ZPRFR(JI,JJ,JK)==0) THEN
               ZPRFR(JI,JJ,JK)=1.
            END IF
         ELSE
            ZPRFR(JI,JJ,JK)=0.
         END IF
      END DO
   END DO
END DO
!
!
END SUBROUTINE RAINFR_VERT 
!
!
!-------------------------------------------------------------------------------
!
!
  FUNCTION COUNTJV(LTAB,I1,I2,I3) RESULT(IC)
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
!
LOGICAL, DIMENSION(:,:,:) :: LTAB ! Mask
INTEGER, DIMENSION(:) :: I1,I2,I3 ! Used to replace the COUNT and PACK
INTEGER :: JI,JJ,JK,IC
!
!-------------------------------------------------------------------------------
!
IC = 0
DO JK = 1,SIZE(LTAB,3)
  DO JJ = 1,SIZE(LTAB,2)
    DO JI = 1,SIZE(LTAB,1)
      IF( LTAB(JI,JJ,JK) ) THEN
        IC = IC +1
        I1(IC) = JI
        I2(IC) = JJ
        I3(IC) = JK
      END IF
    END DO
  END DO
END DO
!
END FUNCTION COUNTJV
  FUNCTION COUNTJV2(LTAB,I1,I2) RESULT(IC)
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
!
LOGICAL, DIMENSION(:,:) :: LTAB ! Mask
INTEGER, DIMENSION(:) :: I1,I2 ! Used to replace the COUNT and PACK
INTEGER :: JI,JJ,IC
!
!-------------------------------------------------------------------------------
!
IC = 0
DO JJ = 1,SIZE(LTAB,2)
  DO JI = 1,SIZE(LTAB,1)
    IF( LTAB(JI,JJ) ) THEN
      IC = IC +1
      I1(IC) = JI
      I2(IC) = JJ
    END IF
  END DO
END DO
!
END FUNCTION COUNTJV2
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE RAIN_ICE
