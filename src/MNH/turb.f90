!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!    ################ 
     MODULE MODI_TURB  
!    ################ 
!
INTERFACE
!
      SUBROUTINE TURB(KKA, KKU, KKL, KMI,KRR,KRRL,KRRI,HLBCX,HLBCY,   &
                KSPLIT,KMODEL_CL, &
                OCLOSE_OUT,OTURB_FLX,OTURB_DIAG,OSUBG_COND,ORMC01,    &
                HTURBDIM,HTURBLEN,HTOM,HTURBLEN_CL,HCLOUD,PIMPL,      &
                PTSTEP,TPFILE,PDXX,PDYY,PDZZ,PDZX,PDZY,PZZ,           &
                PDIRCOSXW,PDIRCOSYW,PDIRCOSZW,PCOSSLOPE,PSINSLOPE,    &
                PRHODJ,PTHVREF,                                       &
                PSFTH,PSFRV,PSFSV,PSFU,PSFV,                          &
                PPABST,PUT,PVT,PWT,PTKET,PSVT,PSRCT,                  &
                PBL_DEPTH, PSBL_DEPTH,                                &
                PCEI,PCEI_MIN,PCEI_MAX,PCOEF_AMPL_SAT,                &
                PTHLT,PRT,                                            &
                PRUS,PRVS,PRWS,PRTHLS,PRRS,PRSVS,PRTKES,PRTKEMS,PSIGS,&
                PFLXZTHVMF,PWTH,PWRC,PWSV,PDYP,PTHP,PTR,PDISS,PLEM    )

!
USE MODD_IO_ll, ONLY: TFILEDATA
!
INTEGER,                INTENT(IN)   :: KKA           !near ground array index  
INTEGER,                INTENT(IN)   :: KKU           !uppest atmosphere array index
INTEGER,                INTENT(IN)   :: KKL           !vert. levels type 1=MNH -1=AR
INTEGER,                INTENT(IN)   :: KMI           ! model index number  
INTEGER,                INTENT(IN)   :: KRR           ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL          ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI          ! number of ice water var.
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
INTEGER,                INTENT(IN)   :: KSPLIT        ! number of time-splitting
INTEGER,                INTENT(IN)   :: KMODEL_CL     ! model number for cloud mixing length
LOGICAL,                INTENT(IN)   ::  OCLOSE_OUT   ! switch for syncronous
                                                      ! file opening
LOGICAL,                INTENT(IN)   ::  OTURB_FLX    ! switch to write the
                                 ! turbulent fluxes in the syncronous FM-file
LOGICAL,                INTENT(IN)   ::  OTURB_DIAG   ! switch to write some
                                 ! diagnostic fields in the syncronous FM-file
LOGICAL,                INTENT(IN)   ::  OSUBG_COND   ! switch for SUBGrid 
                                 ! CONDensation
LOGICAL,                INTENT(IN)   ::  ORMC01       ! switch for RMC01 lengths in SBL
CHARACTER*4           , INTENT(IN)      ::  HTURBDIM  ! dimensionality of the 
                                 ! turbulence scheme
CHARACTER*4           , INTENT(IN)   ::  HTURBLEN     ! kind of mixing length
CHARACTER*4           , INTENT(IN)   ::  HTOM         ! kind of Third Order Moment
CHARACTER*4           , INTENT(IN)   ::  HTURBLEN_CL  ! kind of cloud mixing length
                                                      ! surface friction flux
REAL,                   INTENT(IN)   ::  PIMPL        ! degree of implicitness
CHARACTER (LEN=4),      INTENT(IN)   ::  HCLOUD       ! Kind of microphysical scheme
REAL,                   INTENT(IN)   ::  PTSTEP       ! timestep 
TYPE(TFILEDATA),        INTENT(IN)   ::  TPFILE       ! Output file
!
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PDXX,PDYY,PDZZ,PDZX,PDZY
                                        ! metric coefficients
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PZZ       !  physical distance 
! between 2 succesive grid points along the K direction
REAL, DIMENSION(:,:),   INTENT(IN)      ::  PDIRCOSXW, PDIRCOSYW, PDIRCOSZW
! Director Cosinus along x, y and z directions at surface w-point
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PCOSSLOPE       ! cosinus of the angle
                                 ! between i and the slope vector
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PSINSLOPE       ! sinus of the angle
                                 ! between i and the slope vector
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PRHODJ    ! dry density * Grid size
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PTHVREF   ! Virtual Potential
                                        ! Temperature of the reference state
!
REAL, DIMENSION(:,:),   INTENT(IN)      ::  PSFTH,PSFRV,   &
! normal surface fluxes of theta and Rv 
                                            PSFU,PSFV
! normal surface fluxes of (u,v) parallel to the orography 
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PSFSV
! normal surface fluxes of Scalar var. 
!
!    prognostic variables at t- deltat
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PPABST      ! Pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PUT,PVT,PWT ! wind components
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTKET       ! TKE
REAL, DIMENSION(:,:,:,:), INTENT(IN) ::  PSVT        ! passive scal. var.
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PSRCT       ! Second-order flux
                      ! s'rc'/2Sigma_s2 at time t-1 multiplied by Lambda_3
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PBL_DEPTH  ! BL depth for TOMS
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PSBL_DEPTH ! SBL depth for RMC01
!
!
!    variables for cloud mixing length
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PCEI ! Cloud Entrainment instability
                                                 ! index to emphasize localy 
                                                 ! turbulent fluxes
REAL, INTENT(IN)      ::  PCEI_MIN ! minimum threshold for the instability index CEI
REAL, INTENT(IN)      ::  PCEI_MAX ! maximum threshold for the instability index CEI
REAL, INTENT(IN)      ::  PCOEF_AMPL_SAT ! saturation of the amplification coefficient
!   thermodynamical variables which are transformed in conservative var.
REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PTHLT       ! conservative pot. temp.
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRT         ! water var.  where 
                             ! PRT(:,:,:,1) is the conservative mixing ratio        
!
! sources of momentum, conservative potential temperature, Turb. Kin. Energy, 
! TKE dissipation
REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PRUS,PRVS,PRWS,PRTHLS,PRTKES
! Source terms for all water kinds, PRRS(:,:,:,1) is used for the conservative
! mixing ratio
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PRTKEMS
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRRS 
! Source terms for all passive scalar variables
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRSVS
! Sigma_s at time t+1 : square root of the variance of the deviation to the 
! saturation 
REAL, DIMENSION(:,:,:), INTENT(OUT)     ::  PSIGS
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PFLXZTHVMF 
!                                           MF contribution for vert. turb. transport
!                                           used in the buoy. prod. of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PWTH       ! heat flux
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PWRC       ! cloud water flux
REAL, DIMENSION(:,:,:,:),INTENT(OUT) :: PWSV       ! scalar flux
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PDYP  ! Dynamical production of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PTHP  ! Thermal production of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PTR   ! Transport production of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PDISS ! Dissipation of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PLEM  ! Mixing length

!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TURB
!
END INTERFACE
!
END MODULE MODI_TURB
!
!     #################################################################
      SUBROUTINE TURB(KKA,KKU,KKL,KMI,KRR,KRRL,KRRI,HLBCX,HLBCY,KSPLIT,KMODEL_CL, &
                OCLOSE_OUT,OTURB_FLX,OTURB_DIAG,OSUBG_COND,ORMC01,    &
                HTURBDIM,HTURBLEN,HTOM,HTURBLEN_CL,HCLOUD,PIMPL,      &
                PTSTEP,TPFILE,PDXX,PDYY,PDZZ,PDZX,PDZY,PZZ,           &
                PDIRCOSXW,PDIRCOSYW,PDIRCOSZW,PCOSSLOPE,PSINSLOPE,    &
                PRHODJ,PTHVREF,                                       &
                PSFTH,PSFRV,PSFSV,PSFU,PSFV,                          &
                PPABST,PUT,PVT,PWT,PTKET,PSVT,PSRCT,                  &
                PBL_DEPTH,PSBL_DEPTH,                                 &
                PCEI,PCEI_MIN,PCEI_MAX,PCOEF_AMPL_SAT,                &
                PTHLT,PRT,                                            &
                PRUS,PRVS,PRWS,PRTHLS,PRRS,PRSVS,PRTKES,PRTKEMS,PSIGS,&
                PFLXZTHVMF,PWTH,PWRC,PWSV,PDYP,PTHP,PTR,PDISS,PLEM    )
!     #################################################################
!
!
!!****  *TURB* - computes the turbulent source terms for the prognostic
!!               variables. 
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to compute the source terms in 
!!    the evolution equations due to the turbulent mixing. 
!!      The source term is computed as the divergence of the turbulent fluxes.
!!    The cartesian fluxes are obtained by a one and a half order closure, based
!!    on a prognostic equation for the Turbulence Kinetic Energy( TKE ). The 
!!    system is closed by prescribing a turbulent mixing length. Different 
!!    choices are available for this length. 
!
!!**  METHOD
!!    ------
!!    
!!      The dimensionality of the turbulence parameterization can be chosen by
!!    means of the parameter HTURBDIM:
!!           * HTURBDIM='1DIM' the parameterization is 1D but can be used in
!!    3D , 2D or 1D simulations. Only the sources associated to the vertical
!!    turbulent fluxes are taken into account.
!!           *  HTURBDIM='3DIM' the parameterization is fully 2D or 3D depending
!!    on the model  dimensionality. Of course, it does not make any sense to
!!    activate this option with a 1D model. 
!!
!!      The following steps are made:
!!      1- Preliminary computations.
!!      2- The metric coefficients are recovered from the grid knowledge.
!!      3- The mixing length is computed according to its choice:
!!           * HTURBLEN='BL89' the Bougeault and Lacarrere algorithm is used.
!!             The mixing length is given by the vertical displacement from its
!!             original level of an air particule having an initial internal
!!             energy equal to its TKE and stopped by the buoyancy forces.
!!             The discrete formulation is second order accurate.
!!           * HTURBLEN='DELT' the mixing length is given by the mesh size 
!!             depending on the model dimensionality, this length is limited 
!!             with the ground distance.
!!           * HTURBLEN='DEAR' the mixing length is given by the mesh size 
!!             depending on the model dimensionality, this length is limited 
!!             with the ground distance and also by the Deardorff mixing length
!!             pertinent in the stable cases.
!!           * HTURBLEN='KEPS' the mixing length is deduced from the TKE 
!!             dissipation, which becomes a prognostic variable of the model (
!!             Duynkerke formulation).   
!!      3'- The cloud mixing length is computed according to HTURBLEN_CLOUD
!!             and emphasized following the CEI index
!!      4- The conservative variables are computed along with Lv/Cp.
!!      5- The turbulent Prandtl numbers are computed from the resolved fields
!!         and TKE 
!!      6- The sources associated to the vertical turbulent fluxes are computed
!!      with a temporal scheme allowing a degree of implicitness given by 
!!      PIMPL, varying from PIMPL=0. ( purely explicit scheme) to PIMPL=1.
!!      ( purely implicit scheme)
!!      The sources associated to the horizontal fluxes are computed with a
!!      purely explicit temporal scheme. These sources are only computed when
!!      the turbulence parameterization is 2D or 3D( HTURBDIM='3DIM' ).
!!      7- The sources for TKE are computed, along with the dissipation of TKE 
!!      if HTURBLEN='KEPS'.
!!      8- Some turbulence-related quantities are stored in the synchronous 
!!      FM-file.
!!      9- The non-conservative variables are retrieved.  
!!    
!!      
!!      The saving of the fields in the synchronous FM-file is controlled by:
!!        * OTURB_FLX => saves all the turbulent fluxes and correlations
!!        * OTURB_DIAG=> saves the turbulent Prandtl and Schmidt numbers, the
!!                       source terms of TKE and dissipation of TKE 
!!
!!    EXTERNAL
!!    --------
!!      SUBROUTINE PRANDTL   : computes the turbulent Prandtl number
!!      SUBROUTINE TURB_VER  : computes the sources from the vertical fluxes
!!      SUBROUTINE TURB_HOR  : computes the sources from the horizontal fluxes
!!      SUBROUTINE TKE_EPS_SOURCES : computes the sources for  TKE and its
!!                                   dissipation
!!      SUBROUTINE BUDGET    : computes and stores the budgets
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!       MODD_PARAMETERS : JPVEXT  number of marginal vertical points
!!
!!       MODD_CONF      : CCONF model configuration (start/restart)
!!                        L1D   switch for 1D model version
!!                        L2D   switch for 2D model version
!!
!!       MODD_CST  : contains physical constants
!!                    XG   gravity constant
!!                    XRD  Gas constant for dry air
!!                    XRV  Gas constant for vapor
!!
!!       MODD_CTURB : contains turbulence scheme constants
!!                    XCMFS,XCED       to compute the dissipation mixing length
!!                    XTKEMIN  minimum values for the TKE 
!!                    XLINI,XLINF      to compute Bougeault-Lacarrere mixing 
!!                                     length
!!      Module MODD_BUDGET:
!!         NBUMOD  
!!         CBUTYPE 
!!         NBUPROCCTR 
!!         LBU_RU     
!!         LBU_RV     
!!         LBU_RW     
!!         LBU_RTH    
!!         LBU_RSV1   
!!         LBU_RRV    
!!         LBU_RRC    
!!         LBU_RRR    
!!         LBU_RRI    
!!         LBU_RRS    
!!         LBU_RRG    
!!         LBU_RRH    
!!
!!    REFERENCE
!!    ---------
!!      Book 2 of documentation (routine TURB)
!!      Book 1 of documentation (Chapter: Turbulence)
!!
!!    AUTHOR
!!    ------
!!      Joan Cuxart             * INM and Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         05/10/94
!!      Modifications: Feb 14, 1995 (J.Cuxart and J.Stein) 
!!                                  Doctorization and Optimization
!!      Modifications: March 21, 1995 (J.M. Carriere) 
!!                                  Introduction of cloud water
!!      Modifications: June   1, 1995 (J.Cuxart     ) 
!!                                  take min(Kz,delta)
!!      Modifications: June   1, 1995 (J.Stein J.Cuxart)
!!                                  remove unnecessary arrays and change Prandtl
!!                                  and Schmidt numbers localizations
!!      Modifications: July  20, 1995 (J.Stein) remove MODI_ground_ocean +
!!                                TZDTCUR + MODD_TIME because they are not used
!!                                change RW in RNP for the outputs
!!      Modifications: August 21, 1995 (Ph. Bougeault)   
!!                                  take min(K(z-zsol),delta)
!!      Modifications: Sept 14, 1995 (Ph Bougeault, J. Cuxart)
!!         second order BL89 mixing length computations + add Deardorff length 
!!         in the Delta case for stable cases
!!      Modifications: Sept 19, 1995 (J. Stein, J. Cuxart)
!!         define a DEAR case for the mixing length, add MODI_BUDGET and change
!!         some BUDGET calls, add LES tools
!!      Modifications: Oct  16, 1995 (J. Stein) change the budget calls
!!      Modifications: Feb  28, 1996 (J. Stein) optimization + 
!!                                              remove min(K(z-zsol),delta)+
!!                                              bug in the tangential fluxes 
!!      Modifications: Oct  16, 1996 (J. Stein) change the subgrid condensation
!!                                              scheme + temporal discretization
!!      Modifications: Dec  19, 1996 (J.-P. Pinty) update the budget calls
!!                     Jun  22, 1997 (J. Stein) use the absolute pressure and
!!                                  change the Deardorf length at the surface
!!      Modifications: Apr  27, 1997 (V. Masson) BL89 mix. length computed in
!!                                               a separate routine
!!                     Oct  13, 1999 (J. Stein)  switch for the tgt fluxes
!!                     Jun  24, 1999 (P Jabouille)  Add routine UPDATE_ROTATE_WIND
!!                     Feb  15, 2001 (J. Stein)  remove tgt fluxes
!!                     Mar 8,  2001 (V. Masson) forces the same behaviour near the surface
!!                                              for all mixing lengths
!!                     Nov 06, 2002 (V. Masson) LES budgets
!!                     Nov,    2002 (V. Masson) implement modifications of
!!                                              mixing and dissipative lengths
!!                                              near the surface (according
!!                                              Redelsperger et al 2001)
!!                     Apr,    2003 (V. Masson) bug in Blackadar length
!!                                              bug in LES in 1DIM case
!!                     Feb 20, 2003 (J.-P. Pinty) Add reversible ice processes
!!                     May,26  2004 (P Jabouille) coef for computing dissipative heating
!!                     Sept 2004 (M.Tomasini) Cloud Mixing length modification
!!                                            following the instability 
!!                                            criterium CEI calculated in modeln
!!                     May   2006    Remove KEPS
!!                     Sept.2006 (I.Sandu): Modification of the stability criterion for
!!                                 DEAR (theta_v -> theta_l)
!!                     Oct 2007 (J.Pergaud) Add MF contribution for vert. turb. transport
!!                     Oct.2009  (C.Lac) Introduction of different PTSTEP according to the
!!                              advection schemes
!!                     October 2009 (G. Tanguy) add ILENCH=LEN(YCOMMENT) after
!!                                              change of YCOMMENT
!!                     06/2011 (J.escobar ) Bypass Bug with ifort11/12 on  HLBCX,HLBC
!!                     2012-02 Y. Seity,  add possibility to run with reversed
!!                                          vertical levels
!!                     10/2012 (J. Colin) Correct bug in DearDoff for dry simulations
!!                     10/2012 J.Escobar Bypass PGI bug , redefine some allocatable array inplace of automatic
!!                     04/2016  (C.Lac) correction of negativity for KHKO
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!                     01/2018 (Q.Rodier) Introduction of RM17
!!                  03/2020 (B.Vie) : LIMA negativity checks after turbulence, advection and microphysics budgets 
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_PARAMETERS, ONLY: JPVEXT_TURB
USE MODD_CST
USE MODD_CTURB
USE MODD_CONF
USE MODD_BUDGET
USE MODD_IO_ll, ONLY: TFILEDATA
USE MODD_LES
USE MODD_NSV
USE MODD_PARAM_LIMA
!
USE MODI_GRADIENT_M
USE MODI_GRADIENT_U
USE MODI_GRADIENT_V
USE MODI_BL89
USE MODI_TURB_VER
USE MODI_ROTATE_WIND
USE MODI_TURB_HOR_SPLT 
USE MODI_TKE_EPS_SOURCES
USE MODI_SHUMAN
USE MODI_GRADIENT_M
USE MODI_BUDGET
USE MODI_LES_MEAN_SUBGRID
USE MODI_RMC01
USE MODI_GRADIENT_W
USE MODI_TM06
USE MODI_UPDATE_LM
USE MODI_GET_HALO
!
USE MODE_FIELD, ONLY: TFIELDDATA, TYPEREAL
USE MODE_FMWRIT
USE MODE_SBL
!
USE MODI_EMOIST
USE MODI_ETHETA
!
USE MODI_SECOND_MNH
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
!
INTEGER,                INTENT(IN)   :: KKA           !near ground array index  
INTEGER,                INTENT(IN)   :: KKU           !uppest atmosphere array index
INTEGER,                INTENT(IN)   :: KKL           !vert. levels type 1=MNH -1=ARO
INTEGER,                INTENT(IN)   :: KMI           ! model index number  
INTEGER,                INTENT(IN)   :: KRR           ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL          ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI          ! number of ice water var.
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
INTEGER,                INTENT(IN)   :: KSPLIT        ! number of time-splitting
INTEGER,                INTENT(IN)   :: KMODEL_CL     ! model number for cloud mixing length
LOGICAL,                INTENT(IN)   ::  OCLOSE_OUT   ! switch for syncronous
                                                      ! file opening
LOGICAL,                INTENT(IN)   ::  OTURB_FLX    ! switch to write the
                                 ! turbulent fluxes in the syncronous FM-file
LOGICAL,                INTENT(IN)   ::  OTURB_DIAG   ! switch to write some
                                 ! diagnostic fields in the syncronous FM-file
LOGICAL,                INTENT(IN)   ::  OSUBG_COND   ! switch for SUBGrid 
                                 ! CONDensation
LOGICAL,                INTENT(IN)   ::  ORMC01       ! switch for RMC01 lengths in SBL
CHARACTER*4           , INTENT(IN)      ::  HTURBDIM  ! dimensionality of the 
                                 ! turbulence scheme
CHARACTER*4           , INTENT(IN)   ::  HTURBLEN     ! kind of mixing length
CHARACTER*4           , INTENT(IN)   ::  HTOM         ! kind of Third Order Moment
CHARACTER*4           , INTENT(IN)   ::  HTURBLEN_CL  ! kind of cloud mixing length
REAL,                   INTENT(IN)   ::  PIMPL        ! degree of implicitness
CHARACTER (LEN=4),      INTENT(IN)   ::  HCLOUD       ! Kind of microphysical scheme
REAL,                   INTENT(IN)   ::  PTSTEP       ! timestep 
TYPE(TFILEDATA),        INTENT(IN)   ::  TPFILE       ! Output file
!
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PDXX,PDYY,PDZZ,PDZX,PDZY
                                        ! metric coefficients
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PZZ       !  physical distance 
! between 2 succesive grid points along the K direction
REAL, DIMENSION(:,:),   INTENT(IN)      ::  PDIRCOSXW, PDIRCOSYW, PDIRCOSZW
! Director Cosinus along x, y and z directions at surface w-point
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PCOSSLOPE       ! cosinus of the angle
                                 ! between i and the slope vector
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PSINSLOPE       ! sinus of the angle
                                 ! between i and the slope vector
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PRHODJ    ! dry density * Grid size
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PTHVREF   ! Virtual Potential
                                        ! Temperature of the reference state
!
REAL, DIMENSION(:,:),   INTENT(IN)      ::  PSFTH,PSFRV,   &
! normal surface fluxes of theta and Rv 
                                            PSFU,PSFV
! normal surface fluxes of (u,v) parallel to the orography
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PSFSV
! normal surface fluxes of Scalar var. 
!
!    prognostic variables at t- deltat
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PPABST      ! Pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PUT,PVT,PWT ! wind components
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PTKET       ! TKE
REAL, DIMENSION(:,:,:,:), INTENT(IN) ::  PSVT        ! passive scal. var.
REAL, DIMENSION(:,:,:),   INTENT(IN) ::  PSRCT       ! Second-order flux
                      ! s'rc'/2Sigma_s2 at time t-1 multiplied by Lambda_3
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PBL_DEPTH  ! BL height for TOMS
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PSBL_DEPTH ! SBL depth for RMC01
!
!    variables for cloud mixing length
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PCEI ! Cloud Entrainment instability
                                                 ! index to emphasize localy 
                                                 ! turbulent fluxes
REAL, INTENT(IN)      ::  PCEI_MIN ! minimum threshold for the instability index CEI
REAL, INTENT(IN)      ::  PCEI_MAX ! maximum threshold for the instability index CEI
REAL, INTENT(IN)      ::  PCOEF_AMPL_SAT ! saturation of the amplification coefficient
!
!   thermodynamical variables which are transformed in conservative var.
REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PTHLT       ! conservative pot. temp.
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRT         ! water var.  where 
                             ! PRT(:,:,:,1) is the conservative mixing ratio        
!
! sources of momentum, conservative potential temperature, Turb. Kin. Energy, 
! TKE dissipation
REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PRUS,PRVS,PRWS,PRTHLS,PRTKES
! Source terms for all water kinds, PRRS(:,:,:,1) is used for the conservative
! mixing ratio
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PRTKEMS
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRRS 
! Source terms for all passive scalar variables
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRSVS
! Sigma_s at time t+1 : square root of the variance of the deviation to the 
! saturation 
REAL, DIMENSION(:,:,:), INTENT(OUT)     ::  PSIGS
REAL, DIMENSION(:,:,:), INTENT(IN)      ::  PFLXZTHVMF 
!                                           MF contribution for vert. turb. transport
!                                           used in the buoy. prod. of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PWTH       ! heat flux
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PWRC       ! cloud water flux
REAL, DIMENSION(:,:,:,:),INTENT(OUT) :: PWSV       ! scalar flux
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PDYP  ! Dynamical production of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PTHP  ! Thermal production of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PTR   ! Transport production of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PDISS ! Dissipation of TKE
REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PLEM  ! Mixing length
!
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
REAL, ALLOCATABLE, DIMENSION(:,:,:) ::&
          ZCP,                        &  ! Cp at t-1
          ZEXN,                       &  ! EXN at t-1
          ZT,                         &  ! T at t-1
          ZLOCPEXNM,                  &  ! Lv/Cp/EXNREF at t-1
          ZLM,                        &  ! Turbulent mixing length
          ZLEPS,                      &  ! Dissipative length
          ZDP,ZTP, ZTRH,              &  ! Dynamic and Thermal Production of TKE
          ZATHETA,ZAMOIST,            &  ! coefficients for s = f (Thetal,Rnp)
          ZCOEF_DISS,                 &  ! 1/(Cph*Exner) for dissipative heating
          ZFRAC_ICE,                  &  ! ri fraction of rc+ri
          ZMWTH,ZMWR,ZMTH2,ZMR2,ZMTHR,&  ! 3rd order moments
          ZFWTH,ZFWR,ZFTH2,ZFR2,ZFTHR,&  ! opposite of verticale derivate of 3rd order moments
          ZTHLM, ZTR, ZDISS              ! initial potential temp.
REAL, ALLOCATABLE, DIMENSION(:,:,:,:) ::     &
          ZRM                            ! initial mixing ratio 
REAL, ALLOCATABLE, DIMENSION(:,:) ::  ZTAU11M,ZTAU12M,  &
                                                 ZTAU22M,ZTAU33M,  &
            ! tangential surface fluxes in the axes following the orography
                                                 ZUSLOPE,ZVSLOPE,  &
            ! wind components at the first mass level parallel 
            ! to the orography 
                                                 ZCDUEFF,          &
            ! - Cd*||u|| where ||u|| is the module of the wind tangential to 
            ! orography (ZUSLOPE,ZVSLOPE) at the surface.
                                                 ZUSTAR, ZLMO,     &
                                                 ZRVM, ZSFRV
            ! friction velocity, Monin Obuhkov length, work arrays for vapor
!
            ! Virtual Potential Temp. used
            ! in the Deardorff mixing length computation
REAL, DIMENSION(:,:,:), ALLOCATABLE  :: &  
          ZLVOCPEXNM,ZLSOCPEXNM,      &  ! Lv/Cp/EXNREF and Ls/Cp/EXNREF at t-1
          ZATHETA_ICE,ZAMOIST_ICE        ! coefficients for s = f (Thetal,Rnp)
!
REAL                :: ZEXPL        ! 1-PIMPL deg of expl.
REAL                :: ZRVORD       ! RV/RD
!
INTEGER             :: IKB,IKE      ! index value for the
! Beginning and the End of the physical domain for the mass points
INTEGER             :: IKT          ! array size in k direction
INTEGER             :: IKTB,IKTE    ! start, end of k loops in physical domain 
INTEGER             :: JRR,JK,JSV   ! loop counters
INTEGER             :: JI,JJ        ! loop counters
REAL                :: ZL0          ! Max. Mixing Length in Blakadar formula
REAL                :: ZALPHA       ! proportionnality constant between Dz/2 and 
!                                   ! BL89 mixing length near the surface
!
REAL :: ZTIME1, ZTIME2
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)):: ZTT,ZEXNE,ZLV,ZLS,ZCPH,ZCOR
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3))::  ZSHEAR, ZDUDZ, ZDVDZ
TYPE(TFIELDDATA) :: TZFIELD
!
!------------------------------------------------------------------------------------------
ALLOCATE (                                                        &
          ZCP(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),         &  
          ZEXN(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),        &  
          ZT(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),          &  
          ZLOCPEXNM(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),   & 
          ZLM(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),         & 
          ZLEPS(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),       &  
          ZDP(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),         &
          ZTP(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),         &
          ZTRH(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),        &  
          ZATHETA(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),     &
          ZAMOIST(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),     & 
          ZCOEF_DISS(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),  &  
          ZFRAC_ICE(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),   &  
          ZMWTH(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),       &
          ZMWR(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),        &
          ZMTH2(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),       &
          ZMR2(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),        &
          ZMTHR(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),       &  
          ZFWTH(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),       &
          ZFWR(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),        &
          ZFTH2(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),       &
          ZFR2(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),        &
          ZFTHR(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)),       &  
          ZTHLM(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3))        )                        

ALLOCATE ( ZRM(SIZE(PRT,1),SIZE(PRT,2),SIZE(PRT,3),SIZE(PRT,4))   )

ALLOCATE ( &
           ZTAU11M(SIZE(PTHLT,1),SIZE(PTHLT,2)),  &
           ZTAU12M(SIZE(PTHLT,1),SIZE(PTHLT,2)),  &
           ZTAU22M(SIZE(PTHLT,1),SIZE(PTHLT,2)),  &
           ZTAU33M(SIZE(PTHLT,1),SIZE(PTHLT,2)),  &
           ZUSLOPE(SIZE(PTHLT,1),SIZE(PTHLT,2)),  &
           ZVSLOPE(SIZE(PTHLT,1),SIZE(PTHLT,2)),  &
           ZCDUEFF(SIZE(PTHLT,1),SIZE(PTHLT,2)),  &
           ZUSTAR(SIZE(PTHLT,1),SIZE(PTHLT,2)),   &
           ZLMO(SIZE(PTHLT,1),SIZE(PTHLT,2)),     &
           ZRVM(SIZE(PTHLT,1),SIZE(PTHLT,2)),     &
           ZSFRV(SIZE(PTHLT,1),SIZE(PTHLT,2))     )

!------------------------------------------------------------------------------------------
!
!*      1.PRELIMINARIES
!         -------------
!
!*      1.1 Set the internal domains, ZEXPL 
!
!
IKT=SIZE(PTHLT,3)          
IKTB=1+JPVEXT_TURB              
IKTE=IKT-JPVEXT_TURB
IKB=KKA+JPVEXT_TURB*KKL
IKE=KKU-JPVEXT_TURB*KKL
!
ZEXPL = 1.- PIMPL
ZRVORD= XRV / XRD
!
!
ZTHLM(:,:,:) = PTHLT(:,:,:)
ZRM(:,:,:,:) = PRT(:,:,:,:)
!
!
!
!----------------------------------------------------------------------------
!
!*      2. COMPUTE CONSERVATIVE VARIABLES AND RELATED QUANTITIES
!          -----------------------------------------------------
!
!*      2.1 Cph at t
!
ZCP(:,:,:)=XCPD
!
IF (KRR > 0) ZCP(:,:,:) = ZCP(:,:,:) + XCPV * PRT(:,:,:,1)
DO JRR = 2,1+KRRL                          ! loop on the liquid components  
  ZCP(:,:,:)  = ZCP(:,:,:) + XCL * PRT(:,:,:,JRR)
END DO
!
DO JRR = 2+KRRL,1+KRRL+KRRI                ! loop on the solid components   
  ZCP(:,:,:)  = ZCP(:,:,:)  + XCI * PRT(:,:,:,JRR)
END DO
!
!*      2.2 Exner function at t
!
ZEXN(:,:,:) = (PPABST(:,:,:)/XP00) ** (XRD/XCPD)
!
!*      2.3 dissipative heating coeff a t
!
ZCOEF_DISS(:,:,:) = 1/(ZCP(:,:,:) * ZEXN(:,:,:)) 
!
!
ZFRAC_ICE(:,:,:) = 0.0
ZATHETA(:,:,:) = 0.0
ZAMOIST(:,:,:) = 0.0
!
IF (KRRL >=1) THEN
!
!*      2.4 Temperature at t
!
  ZT(:,:,:) =  PTHLT(:,:,:) * ZEXN(:,:,:)
!
!*       2.5 Lv/Cph/Exn
!
  IF ( KRRI >= 1 ) THEN 
    ALLOCATE(ZLVOCPEXNM(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)))
    ALLOCATE(ZLSOCPEXNM(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)))
    ALLOCATE(ZAMOIST_ICE(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)))
    ALLOCATE(ZATHETA_ICE(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)))
!
    CALL COMPUTE_FUNCTION_THERMO(XALPW,XBETAW,XGAMW,XLVTT,XCL,ZT,ZEXN,ZCP, &
                                 ZLVOCPEXNM,ZAMOIST,ZATHETA)
    CALL COMPUTE_FUNCTION_THERMO(XALPI,XBETAI,XGAMI,XLSTT,XCI,ZT,ZEXN,ZCP, &
                                 ZLSOCPEXNM,ZAMOIST_ICE,ZATHETA_ICE)
!
    WHERE(PRT(:,:,:,2)+PRT(:,:,:,4)>0.0)
      ZFRAC_ICE(:,:,:) = PRT(:,:,:,4) / ( PRT(:,:,:,2)+PRT(:,:,:,4) )
    END WHERE
!
    ZLOCPEXNM(:,:,:) = (1.0-ZFRAC_ICE(:,:,:))*ZLVOCPEXNM(:,:,:) &
                           +ZFRAC_ICE(:,:,:) *ZLSOCPEXNM(:,:,:)
    ZAMOIST(:,:,:) = (1.0-ZFRAC_ICE(:,:,:))*ZAMOIST(:,:,:) &
                         +ZFRAC_ICE(:,:,:) *ZAMOIST_ICE(:,:,:)
    ZATHETA(:,:,:) = (1.0-ZFRAC_ICE(:,:,:))*ZATHETA(:,:,:) &
                         +ZFRAC_ICE(:,:,:) *ZATHETA_ICE(:,:,:)

    DEALLOCATE(ZAMOIST_ICE)
    DEALLOCATE(ZATHETA_ICE)
  ELSE
    CALL COMPUTE_FUNCTION_THERMO(XALPW,XBETAW,XGAMW,XLVTT,XCL,ZT,ZEXN,ZCP, &
                                 ZLOCPEXNM,ZAMOIST,ZATHETA)
  END IF
!
!
  IF (OCLOSE_OUT .AND. OTURB_DIAG) THEN
    TZFIELD%CMNHNAME   = 'ATHETA'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'ATHETA'
    TZFIELD%CUNITS     = 'm'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = 'X_Y_Z_ATHETA'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZATHETA)
! 
    TZFIELD%CMNHNAME   = 'AMOIST'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'AMOIST'
    TZFIELD%CUNITS     = 'm'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = 'X_Y_Z_AMOIST'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZAMOIST)
  END IF
!
ELSE
  ZLOCPEXNM=0.
END IF              ! loop end on KRRL >= 1
!
! computes conservative variables
!
IF ( KRRL >= 1 ) THEN
  IF ( KRRI >= 1 ) THEN
    ! Rnp at t
    PRT(:,:,:,1)  = PRT(:,:,:,1)  + PRT(:,:,:,2)  + PRT(:,:,:,4)
    PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,2) + PRRS(:,:,:,4)
    ! Theta_l at t
    PTHLT(:,:,:)  = PTHLT(:,:,:)  - ZLVOCPEXNM(:,:,:) * PRT(:,:,:,2) &
                                  - ZLSOCPEXNM(:,:,:) * PRT(:,:,:,4)
    PRTHLS(:,:,:) = PRTHLS(:,:,:) - ZLVOCPEXNM(:,:,:) * PRRS(:,:,:,2) &
                                  - ZLSOCPEXNM(:,:,:) * PRRS(:,:,:,4)
  ELSE
    ! Rnp at t
    PRT(:,:,:,1)  = PRT(:,:,:,1)  + PRT(:,:,:,2) 
    PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,2)
    ! Theta_l at t
    PTHLT(:,:,:)  = PTHLT(:,:,:)  - ZLOCPEXNM(:,:,:) * PRT(:,:,:,2)
    PRTHLS(:,:,:) = PRTHLS(:,:,:) - ZLOCPEXNM(:,:,:) * PRRS(:,:,:,2)
  END IF
END IF
!
!----------------------------------------------------------------------------
!
!*      3. MIXING LENGTH : SELECTION AND COMPUTATION
!          -----------------------------------------
!
!
SELECT CASE (HTURBLEN)
!
!*      3.1 BL89 mixing length
!           ------------------

  CASE ('BL89')
    ZSHEAR=0.
    CALL BL89(KKA,KKU,KKL,PZZ,PDZZ,PTHVREF,ZTHLM,KRR,ZRM,PTKET,ZSHEAR,ZLM)
!
!*      3.2 RM17 mixing length
!           ------------------

  CASE ('RM17')
    ZDUDZ = MXF(MZF(1,KKU,1,GZ_U_UW(1,KKU,1,PUT,PDZZ)))
    ZDVDZ = MYF(MZF(1,KKU,1,GZ_V_VW(1,KKU,1,PVT,PDZZ)))
    ZSHEAR = SQRT(ZDUDZ*ZDUDZ + ZDVDZ*ZDVDZ)
    CALL BL89(KKA,KKU,KKL,PZZ,PDZZ,PTHVREF,ZTHLM,KRR,ZRM,PTKET,ZSHEAR,ZLM)
!
!*      3.3 Delta mixing length
!           -------------------
!
  CASE ('DELT')
    CALL DELT(ZLM)
!
!*      3.4 Deardorff mixing length
!           -----------------------
!
  CASE ('DEAR')
    CALL DEAR(ZLM)
!
!*      3.5 Blackadar mixing length
!           -----------------------
!
  CASE ('BLKR')
   ZL0 = 100.
   ZLM(:,:,:) = ZL0

   ZALPHA=0.5**(-1.5)
   !
   DO JK=IKTB,IKTE
     ZLM(:,:,JK) = ( 0.5*(PZZ(:,:,JK)+PZZ(:,:,JK+KKL)) - &
     & PZZ(:,:,KKA+JPVEXT_TURB*KKL) ) * PDIRCOSZW(:,:)
     ZLM(:,:,JK) = ZALPHA  * ZLM(:,:,JK) * ZL0 / ( ZL0 + ZALPHA*ZLM(:,:,JK) )
   END DO
!
   ZLM(:,:,IKTB-1) = ZLM(:,:,IKTB)
   ZLM(:,:,IKTE+1) = ZLM(:,:,IKTE)
!
!
!
END SELECT
!
!
!
!*      3.5 Mixing length modification for cloud
!           -----------------------
IF (KMODEL_CL==KMI .AND. HTURBLEN_CL/='NONE') CALL CLOUD_MODIF_LM

!
!*      3.6 Dissipative length
!           ------------------
!
ZLEPS(:,:,:)=ZLM(:,:,:)
!
!*      3.7 Correction in the Surface Boundary Layer (Redelsperger 2001)
!           ----------------------------------------
!
ZLMO=XUNDEF
IF (ORMC01) THEN
  ZUSTAR=(PSFU**2+PSFV**2)**(0.25)
  IF (KRR>0) THEN
    ZLMO=LMO(ZUSTAR,ZTHLM(:,:,IKB),ZRM(:,:,IKB,1),PSFTH,PSFRV)
  ELSE
    ZRVM=0.
    ZSFRV=0.
    ZLMO=LMO(ZUSTAR,ZTHLM(:,:,IKB),ZRVM,PSFTH,ZSFRV)
  END IF
  CALL RMC01(HTURBLEN,KKA,KKU,KKL,PZZ,PDXX,PDYY,PDZZ,PDIRCOSZW,PSBL_DEPTH,ZLMO,ZLM,ZLEPS)
END IF
!
!*      3.8 Mixing length in external points (used if HTURBDIM="3DIM")
!           ----------------------------------------------------------
!
IF (HTURBDIM=="3DIM") THEN
  CALL UPDATE_LM(HLBCX,HLBCY,ZLM,ZLEPS)
END IF
!----------------------------------------------------------------------------
!
!*      4. GO INTO THE AXES FOLLOWING THE SURFACE
!          --------------------------------------
!
!
!*      4.1 rotate the wind at time t
!
!
!
  IF (CPROGRAM=='AROME ') THEN
    ZUSLOPE=PUT(:,:,KKA)
    ZVSLOPE=PVT(:,:,KKA)
  ELSE
    CALL ROTATE_WIND(PUT,PVT,PWT,                       &
                     PDIRCOSXW, PDIRCOSYW, PDIRCOSZW,   &
                     PCOSSLOPE,PSINSLOPE,               &
                     PDXX,PDYY,PDZZ,                    &
                     ZUSLOPE,ZVSLOPE                    )
!
    CALL UPDATE_ROTATE_WIND(ZUSLOPE,ZVSLOPE)
  END IF
!
!
!*      4.2 compute the proportionality coefficient between wind and stress
!
  ZCDUEFF(:,:) =-SQRT ( (PSFU(:,:)**2 + PSFV(:,:)**2) /               &
                        (XMNH_TINY + ZUSLOPE(:,:)**2 + ZVSLOPE(:,:)**2 )   &
                      )
!
!*       4.6 compute the surface tangential fluxes
!
ZTAU11M(:,:) =2./3.*(  (1.+ (PZZ (:,:,IKB+KKL)-PZZ (:,:,IKB))  &
                           /(PDZZ(:,:,IKB+KKL)+PDZZ(:,:,IKB))  &
                       )   *PTKET(:,:,IKB)                   &
                     -0.5  *PTKET(:,:,IKB+KKL)                 &
                    )
ZTAU12M(:,:) =0.0
ZTAU22M(:,:) =ZTAU11M(:,:)
ZTAU33M(:,:) =ZTAU11M(:,:)
!
!*       4.7 third order terms in temperature and water fluxes and correlations
!            ------------------------------------------------------------------
!
!
ZMWTH = 0.     ! w'2th'
ZMWR  = 0.     ! w'2r'
ZMTH2 = 0.     ! w'th'2
ZMR2  = 0.     ! w'r'2
ZMTHR = 0.     ! w'th'r'

IF (HTOM=='TM06') THEN
  CALL TM06(KKA,KKU,KKL,PTHVREF,PBL_DEPTH,PZZ,PSFTH,ZMWTH,ZMTH2)
!
  ZFWTH = -GZ_M_W(KKA,KKU,KKL,ZMWTH,PDZZ)    ! -d(w'2th' )/dz
  !ZFWR  = -GZ_M_W(KKA,KKU,KKL,ZMWR, PDZZ)    ! -d(w'2r'  )/dz
  ZFTH2 = -GZ_W_M(KKA,KKU,KKL,ZMTH2,PDZZ)    ! -d(w'th'2 )/dz
  !ZFR2  = -GZ_W_M(KKA,KKU,KKL,ZMR2, PDZZ)    ! -d(w'r'2  )/dz
  !ZFTHR = -GZ_W_M(KKA,KKU,KKL,ZMTHR,PDZZ)    ! -d(w'th'r')/dz
!
  ZFWTH(:,:,IKTE:) = 0.
  ZFWTH(:,:,:IKTB) = 0.
  !ZFWR (:,:,IKTE:) = 0.
  !ZFWR (:,:,:IKTB) = 0.
  ZFWR  = 0.
  ZFTH2(:,:,IKTE:) = 0.
  ZFTH2(:,:,:IKTB) = 0.
  !ZFR2 (:,:,IKTE:) = 0.
  !ZFR2 (:,:,:IKTB) = 0.
  ZFR2  = 0.
  !ZFTHR(:,:,IKTE:) = 0.
  !ZFTHR(:,:,:IKTB) = 0.
  ZFTHR = 0.
ELSE
  ZFWTH = 0.
  ZFWR  = 0.
  ZFTH2 = 0.
  ZFR2  = 0.
  ZFTHR = 0.
ENDIF
!
!----------------------------------------------------------------------------
!
!*      5. TURBULENT SOURCES
!          -----------------
!
CALL TURB_VER(KKA,KKU,KKL,KRR, KRRL, KRRI,               &
          OCLOSE_OUT,OTURB_FLX,                          &
          HTURBDIM,HTOM,PIMPL,ZEXPL,                     &
          PTSTEP,TPFILE,                                 &
          PDXX,PDYY,PDZZ,PDZX,PDZY,PDIRCOSZW,PZZ,        &
          PCOSSLOPE,PSINSLOPE,                           &
          PRHODJ,PTHVREF,                                &
          PSFTH,PSFRV,PSFSV,PSFTH,PSFRV,PSFSV,           &
          ZCDUEFF,ZTAU11M,ZTAU12M,ZTAU33M,               &
          PUT,PVT,PWT,ZUSLOPE,ZVSLOPE,PTHLT,PRT,PSVT,    &
          PTKET,ZLM,ZLEPS,                               &
          ZLOCPEXNM,ZATHETA,ZAMOIST,PSRCT,ZFRAC_ICE,     &
          ZFWTH,ZFWR,ZFTH2,ZFR2,ZFTHR,PBL_DEPTH,         &
          PSBL_DEPTH,ZLMO,                               &
          PRUS,PRVS,PRWS,PRTHLS,PRRS,PRSVS,              &
          ZDP,ZTP,PSIGS,PWTH,PWRC,PWSV                   )
!

IF (LBUDGET_U) CALL BUDGET (PRUS,1,'VTURB_BU_RU')
IF (LBUDGET_V) CALL BUDGET (PRVS,2,'VTURB_BU_RV')
IF (LBUDGET_W) CALL BUDGET (PRWS,3,'VTURB_BU_RW')
IF (LBUDGET_TH)  THEN
  IF ( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET (PRTHLS+ ZLVOCPEXNM * PRRS(:,:,:,2) + ZLSOCPEXNM * PRRS(:,:,:,4),4,'VTURB_BU_RTH')
  ELSE IF ( KRRL >= 1 ) THEN
    CALL BUDGET (PRTHLS+ ZLOCPEXNM * PRRS(:,:,:,2),4,'VTURB_BU_RTH')
  ELSE
    CALL BUDGET (PRTHLS,4,'VTURB_BU_RTH')
  END IF
END IF
IF (LBUDGET_SV) THEN
  DO JSV = 1,NSV
    CALL BUDGET (PRSVS(:,:,:,JSV),JSV+12,'VTURB_BU_RSV')
  END DO
END IF
IF (LBUDGET_RV) THEN
  IF ( KRRI >= 1 .AND. KRRL >= 1) THEN
    CALL BUDGET (PRRS(:,:,:,1)-PRRS(:,:,:,2)-PRRS(:,:,:,4),6,'VTURB_BU_RRV')
  ELSE IF ( KRRL >= 1 ) THEN
    CALL BUDGET (PRRS(:,:,:,1)-PRRS(:,:,:,2),6,'VTURB_BU_RRV')
  ELSE 
    CALL BUDGET (PRRS(:,:,:,1),6,'VTURB_BU_RRV')
  END IF
END IF  
IF (LBUDGET_RC) CALL BUDGET (PRRS(:,:,:,2),7,'VTURB_BU_RRC')
IF (LBUDGET_RI) CALL BUDGET (PRRS(:,:,:,4),9,'VTURB_BU_RRI')
!
!
IF (HTURBDIM=='3DIM') THEN
    CALL TURB_HOR_SPLT(KSPLIT, KRR, KRRL, KRRI, PTSTEP,        &
          HLBCX,HLBCY,OCLOSE_OUT,OTURB_FLX,OSUBG_COND,         &
          TPFILE,                                              &
          PDXX,PDYY,PDZZ,PDZX,PDZY,PZZ,                        &
          PDIRCOSXW,PDIRCOSYW,PDIRCOSZW,                       &
          PCOSSLOPE,PSINSLOPE,                                 &
          PRHODJ,PTHVREF,                                      &
          PSFTH,PSFRV,PSFSV,                                   &
          ZCDUEFF,ZTAU11M,ZTAU12M,ZTAU22M,ZTAU33M,             &
          PUT,PVT,PWT,ZUSLOPE,ZVSLOPE,PTHLT,PRT,PSVT,          &
          PTKET,ZLM,ZLEPS,                                     &
          ZLOCPEXNM,ZATHETA,ZAMOIST,PSRCT,ZFRAC_ICE,           &
          ZDP,ZTP,PSIGS,                                       &
          ZTRH,                                                &
          PRUS,PRVS,PRWS,PRTHLS,PRRS,PRSVS                     )
END IF
!
!
IF (LBUDGET_U) CALL BUDGET (PRUS,1,'HTURB_BU_RU')
IF (LBUDGET_V) CALL BUDGET (PRVS,2,'HTURB_BU_RV')
IF (LBUDGET_W) CALL BUDGET (PRWS,3,'HTURB_BU_RW')
IF (LBUDGET_TH)  THEN
  IF ( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET (PRTHLS+ZLVOCPEXNM*PRRS(:,:,:,2)+ZLSOCPEXNM*PRRS(:,:,:,4) &
                                                  ,4,'HTURB_BU_RTH')
  ELSE IF ( KRRL >= 1 ) THEN
    CALL BUDGET (PRTHLS+ ZLOCPEXNM * PRRS(:,:,:,2),4,'HTURB_BU_RTH')
  ELSE
    CALL BUDGET (PRTHLS,4,'HTURB_BU_RTH')
  END IF
END IF
IF (LBUDGET_SV) THEN
  DO JSV = 1,NSV
    CALL BUDGET (PRSVS(:,:,:,JSV),JSV+12,'HTURB_BU_RSV')
  END DO
END IF
IF (LBUDGET_RV) THEN
  IF ( KRRI >= 1 .AND. KRRL >= 1) THEN
    CALL BUDGET (PRRS(:,:,:,1)-PRRS(:,:,:,2)-PRRS(:,:,:,4),6,'HTURB_BU_RRV')
  ELSE IF ( KRRL >= 1 ) THEN
    CALL BUDGET (PRRS(:,:,:,1)-PRRS(:,:,:,2),6,'HTURB_BU_RRV')
  ELSE 
    CALL BUDGET (PRRS(:,:,:,1),6,'HTURB_BU_RRV')
  END IF
END IF  
IF (LBUDGET_RC) CALL BUDGET (PRRS(:,:,:,2),7,'HTURB_BU_RRC')
IF (LBUDGET_RI) CALL BUDGET (PRRS(:,:,:,4),9,'HTURB_BU_RRI')
!
!----------------------------------------------------------------------------
!
!*      6. EVOLUTION OF THE TKE AND ITS DISSIPATION 
!          ----------------------------------------
!
!  6.1 Contribution of mass-flux in the TKE buoyancy production if 
!      cloud computation is not statistical 

       ZTP = ZTP + XG / PTHVREF * MZF(KKA,KKU,KKL, PFLXZTHVMF )

!  6.2 TKE evolution equation

CALL TKE_EPS_SOURCES(KKA,KKU,KKL,KMI,PTKET,ZLM,ZLEPS,ZDP,ZTRH,       &
                     PRHODJ,PDZZ,PDXX,PDYY,PDZX,PDZY,PZZ,            &
                     PTSTEP,PIMPL,ZEXPL,                             &
                     HTURBLEN,HTURBDIM,                              &
                     TPFILE,OCLOSE_OUT,OTURB_DIAG,                   &
                     ZTP,PRTKES,PRTKEMS,PRTHLS,ZCOEF_DISS,PTR,PDISS  )
!
PDYP = ZDP
PTHP = ZTP
!
IF (LBUDGET_TH)  THEN
  IF ( KRRI >= 1 .AND. KRRL >= 1 ) THEN
    CALL BUDGET (PRTHLS+ZLVOCPEXNM*PRRS(:,:,:,2)+ZLSOCPEXNM*PRRS(:,:,:,4) &
                                                ,4,'DISSH_BU_RTH')
  ELSE IF ( KRRL >= 1 ) THEN
    CALL BUDGET (PRTHLS+ZLOCPEXNM* PRRS(:,:,:,2),4,'DISSH_BU_RTH')
  ELSE
    CALL BUDGET (PRTHLS,4,'DISSH_BU_RTH')
  END IF
END IF
!
!----------------------------------------------------------------------------
!
!*      7. STORES SOME INFORMATIONS RELATED TO THE TURBULENCE SCHEME
!          ---------------------------------------------------------
!
PLEM = ZLM
!
IF ( OTURB_DIAG .AND. OCLOSE_OUT ) THEN
! 
! stores the mixing length
! 
  TZFIELD%CMNHNAME   = 'LM'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'LM'
  TZFIELD%CUNITS     = 'm'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = 'Mixing length'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZLM)
!
  IF (KRR /= 0) THEN
!
! stores the conservative potential temperature
!
    TZFIELD%CMNHNAME   = 'THLM'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'THLM'
    TZFIELD%CUNITS     = 'K'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = 'Conservative potential temperature'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    CALL IO_WRITE_FIELD(TPFILE,TZFIELD,PTHLT)
!
! stores the conservative mixing ratio
!
    TZFIELD%CMNHNAME   = 'RNPM'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'RNPM'
    TZFIELD%CUNITS     = 'kg kg-1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = 'Conservative mixing ratio'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    CALL IO_WRITE_FIELD(TPFILE,TZFIELD,PRT(:,:,:,1))
   END IF
END IF
!
!----------------------------------------------------------------------------
!
!*      8. RETRIEVE NON-CONSERVATIVE VARIABLES
!          -----------------------------------
!
IF ( KRRL >= 1 ) THEN
  IF ( KRRI >= 1 ) THEN
    PRT(:,:,:,1)  = PRT(:,:,:,1)  - PRT(:,:,:,2)  - PRT(:,:,:,4)
    PRRS(:,:,:,1) = PRRS(:,:,:,1) - PRRS(:,:,:,2) - PRRS(:,:,:,4)
    PTHLT(:,:,:)  = PTHLT(:,:,:)  + ZLVOCPEXNM(:,:,:) * PRT(:,:,:,2) &
                                  + ZLSOCPEXNM(:,:,:) * PRT(:,:,:,4)
    PRTHLS(:,:,:) = PRTHLS(:,:,:) + ZLVOCPEXNM(:,:,:) * PRRS(:,:,:,2) &
                                  + ZLSOCPEXNM(:,:,:) * PRRS(:,:,:,4)
!
    DEALLOCATE(ZLVOCPEXNM)
    DEALLOCATE(ZLSOCPEXNM)
  ELSE
    PRT(:,:,:,1)  = PRT(:,:,:,1)  - PRT(:,:,:,2) 
    PRRS(:,:,:,1) = PRRS(:,:,:,1) - PRRS(:,:,:,2)
    PTHLT(:,:,:)  = PTHLT(:,:,:)  + ZLOCPEXNM(:,:,:) * PRT(:,:,:,2)
    PRTHLS(:,:,:) = PRTHLS(:,:,:) + ZLOCPEXNM(:,:,:) * PRRS(:,:,:,2)
  END IF
END IF
!
SELECT CASE ( HCLOUD )
  CASE('ICE3','ICE4')
     ZEXNE(:,:,:)= (PPABST(:,:,:)/XP00)**(XRD/XCPD)
     ZTT(:,:,:)= PTHLT(:,:,:)*ZEXNE(:,:,:)
     ZLV(:,:,:)=XLVTT +(XCPV-XCL) *(ZTT(:,:,:)-XTT)
     ZLS(:,:,:)=XLSTT +(XCPV-XCI) *(ZTT(:,:,:)-XTT)
     ZCPH(:,:,:)=XCPD +XCPV*PRT(:,:,:,1)
    WHERE (PRRS(:,:,:,4) < 0.)
      PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,4)
      PRTHLS(:,:,:) = PRTHLS(:,:,:) - PRRS(:,:,:,4) * ZLS(:,:,:) /  &
           ZCPH(:,:,:) / ZEXNE(:,:,:)
      PRRS(:,:,:,4) = 0.
    END WHERE
!
!   cloud
    WHERE (PRRS(:,:,:,2) < 0.)
      PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,2)
      PRTHLS(:,:,:) = PRTHLS(:,:,:) - PRRS(:,:,:,2) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXNE(:,:,:)
      PRRS(:,:,:,2) = 0.
    END WHERE
!
! if rc or ri are positive, we can correct negative rv
!   cloud
    WHERE ((PRRS(:,:,:,1) <0.) .AND. (PRRS(:,:,:,2)> 0.) )
      PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,2)
      PRTHLS(:,:,:) = PRTHLS(:,:,:) - PRRS(:,:,:,2) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXNE(:,:,:)
      PRRS(:,:,:,2) = 0.
    END WHERE
!   ice
    IF(KRR > 3) THEN
      WHERE ((PRRS(:,:,:,1) < 0.).AND.(PRRS(:,:,:,4) > 0.))
        ZCOR(:,:,:)=MIN(-PRRS(:,:,:,1),PRRS(:,:,:,4))
        PRRS(:,:,:,1) = PRRS(:,:,:,1) + ZCOR(:,:,:)
        PRTHLS(:,:,:) = PRTHLS(:,:,:) - ZCOR(:,:,:) * ZLS(:,:,:) /  &
             ZCPH(:,:,:) / ZEXNE(:,:,:)
        PRRS(:,:,:,4) = PRRS(:,:,:,4) -ZCOR(:,:,:)
      END WHERE
    END IF
!
  CASE('C2R2','KHKO')
     ZEXNE(:,:,:)= (PPABST(:,:,:)/XP00)**(XRD/XCPD)
     ZTT(:,:,:)= PTHLT(:,:,:)*ZEXNE(:,:,:)
     ZLV(:,:,:)=XLVTT +(XCPV-XCL) *(ZTT(:,:,:)-XTT)
     ZLS(:,:,:)=XLSTT +(XCPV-XCI) *(ZTT(:,:,:)-XTT)
     ZCPH(:,:,:)=XCPD +XCPV*PRT(:,:,:,1)
!  CALL GET_HALO(PRRS(:,:,:,2))
!  CALL GET_HALO(PRSVS(:,:,:,2))
!  CALL GET_HALO(PRSVS(:,:,:,3))
     WHERE (PRRS(:,:,:,2) < 0. .OR. PRSVS(:,:,:,2) < 0.)
        PRSVS(:,:,:,1) = 0.0
     END WHERE
     DO JSV = 2, 3
        WHERE (PRRS(:,:,:,JSV) < 0. .OR. PRSVS(:,:,:,JSV) < 0.)
           PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,JSV)
           PRTHLS(:,:,:) = PRTHLS(:,:,:) - PRRS(:,:,:,JSV) * ZLV(:,:,:) /  &
                ZCPH(:,:,:) / ZEXNE(:,:,:)
           PRRS(:,:,:,JSV)  = 0.0
           PRSVS(:,:,:,JSV) = 0.0
        END WHERE
     END DO
     !
   CASE('LIMA')   
     ZEXNE(:,:,:)= (PPABST(:,:,:)/XP00)**(XRD/XCPD)
     ZTT(:,:,:)= PTHLT(:,:,:)*ZEXNE(:,:,:)
     ZLV(:,:,:)=XLVTT +(XCPV-XCL) *(ZTT(:,:,:)-XTT)
     ZLS(:,:,:)=XLSTT +(XCPV-XCI) *(ZTT(:,:,:)-XTT)
     ZCPH(:,:,:)=XCPD +XCPV*PRT(:,:,:,1)
! Correction where rc<0 or Nc<0
      IF (LWARM) THEN
         WHERE (PRRS(:,:,:,2) < XRTMIN(2)/PTSTEP .OR. PRSVS(:,:,:,NSV_LIMA_NC) < XCTMIN(2)/PTSTEP)
            PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,2)
            PRTHLS(:,:,:) = PRTHLS(:,:,:) - PRRS(:,:,:,2) * ZLV(:,:,:) /  &
                 ZCPH(:,:,:) / ZEXNE(:,:,:)
            PRRS(:,:,:,2)  = 0.0
            PRSVS(:,:,:,NSV_LIMA_NC) = 0.0
         END WHERE
      END IF
! Correction where rr<0 or Nr<0
      IF (LWARM .AND. LRAIN) THEN
         WHERE (PRRS(:,:,:,3) < XRTMIN(3)/PTSTEP .OR. PRSVS(:,:,:,NSV_LIMA_NR) < XCTMIN(3)/PTSTEP)
            PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,3)
            PRTHLS(:,:,:) = PRTHLS(:,:,:) - PRRS(:,:,:,3) * ZLV(:,:,:) /  &
                 ZCPH(:,:,:) / ZEXNE(:,:,:)
            PRRS(:,:,:,3)  = 0.0
            PRSVS(:,:,:,NSV_LIMA_NR) = 0.0
         END WHERE
      END IF
! Correction where ri<0 or Ni<0
      IF (LCOLD) THEN
         WHERE (PRRS(:,:,:,4) < XRTMIN(4)/PTSTEP .OR. PRSVS(:,:,:,NSV_LIMA_NI) < XCTMIN(4)/PTSTEP)
            PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,4)
            PRTHLS(:,:,:) = PRTHLS(:,:,:) - PRRS(:,:,:,4) * ZLS(:,:,:) /  &
                 ZCPH(:,:,:) / ZEXNE(:,:,:)
            PRRS(:,:,:,4)  = 0.0
            PRSVS(:,:,:,NSV_LIMA_NI) = 0.0
         END WHERE
      END IF
!
      PRSVS(:,:,:,:) = MAX( 0.0,PRSVS(:,:,:,:) )
      PRRS(:,:,:,:) = MAX( 0.0,PRRS(:,:,:,:) )
!
END SELECT
!
IF ((HCLOUD == 'ICE3') .OR. (HCLOUD == 'ICE4') .OR. (HCLOUD == 'KHKO') .OR. (HCLOUD == 'C2R2') .OR. (HCLOUD == 'LIMA') ) THEN
  IF (LBUDGET_TH) CALL BUDGET (PRTHLS(:,:,:), 4,'NETUR_BU_RTH')
  IF (LBUDGET_RV) CALL BUDGET (PRRS(:,:,:,1), 6,'NETUR_BU_RRV')
  IF (LBUDGET_RC) CALL BUDGET (PRRS(:,:,:,2), 7,'NETUR_BU_RRC')
  IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:,3), 8,'NETUR_BU_RRR')
  IF (LBUDGET_RI) CALL BUDGET (PRRS(:,:,:,4), 9,'NETUR_BU_RRI')
  IF (LBUDGET_RS) CALL BUDGET (PRRS(:,:,:,5),10,'NETUR_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (PRRS(:,:,:,6),11,'NETUR_BU_RRG')
  IF (LBUDGET_RH) CALL BUDGET (PRRS(:,:,:,7),12,'NETUR_BU_RRH')
END IF
IF (LBUDGET_SV .AND. (HCLOUD == 'LIMA')) THEN
   IF (LWARM) CALL BUDGET (PRSVS(:,:,:,NSV_LIMA_NC),12+NSV_LIMA_NC,'NETUR_BU_RSV')
   IF (LWARM.AND.LRAIN) CALL BUDGET (PRSVS(:,:,:,NSV_LIMA_NR),12+NSV_LIMA_NR,'NETUR_BU_RSV')
   IF (LCOLD) CALL BUDGET (PRSVS(:,:,:,NSV_LIMA_NI),12+NSV_LIMA_NI,'NETUR_BU_RSV')
   IF (NMOD_CCN.GE.1) THEN
      DO JI=1, NMOD_CCN
         CALL BUDGET ( PRSVS(:,:,:,NSV_LIMA_CCN_FREE+JI-1),12+NSV_LIMA_CCN_FREE+JI-1,'NETUR_BU_RSV') 
      END DO
   END IF
   IF (NMOD_IFN.GE.1) THEN
      DO JI=1, NMOD_IFN
         CALL BUDGET ( PRSVS(:,:,:,NSV_LIMA_IFN_FREE+JI-1),12+NSV_LIMA_IFN_FREE+JI-1,'NETUR_BU_RSV') 
      END DO
   END IF
END IF

!
!----------------------------------------------------------------------------
!
!*      9. LES averaged surface fluxes
!          ---------------------------
!
IF (LLES_CALL) THEN
  CALL SECOND_MNH(ZTIME1)
  CALL LES_MEAN_SUBGRID(PSFTH,X_LES_Q0)
  CALL LES_MEAN_SUBGRID(PSFRV,X_LES_E0)
  DO JSV=1,NSV
    CALL LES_MEAN_SUBGRID(PSFSV(:,:,JSV),X_LES_SV0(:,JSV))
  END DO
  CALL LES_MEAN_SUBGRID(PSFU,X_LES_UW0)
  CALL LES_MEAN_SUBGRID(PSFV,X_LES_VW0)
  CALL LES_MEAN_SUBGRID((PSFU*PSFU+PSFV*PSFV)**0.25,X_LES_USTAR)
!----------------------------------------------------------------------------
!
!*     10. LES for 3rd order moments
!          -------------------------
!
  CALL LES_MEAN_SUBGRID(ZMWTH,X_LES_SUBGRID_W2Thl)
  CALL LES_MEAN_SUBGRID(ZMTH2,X_LES_SUBGRID_WThl2)
  IF (KRR>0) THEN
    CALL LES_MEAN_SUBGRID(ZMWR,X_LES_SUBGRID_W2Rt)
    CALL LES_MEAN_SUBGRID(ZMTHR,X_LES_SUBGRID_WThlRt)
    CALL LES_MEAN_SUBGRID(ZMR2,X_LES_SUBGRID_WRt2)
  END IF
!
!----------------------------------------------------------------------------
!
!*     11. LES quantities depending on <w'2> in "1DIM" mode
!          ------------------------------------------------
!
  IF (HTURBDIM=="1DIM") THEN
    CALL LES_MEAN_SUBGRID(2./3.*PTKET,X_LES_SUBGRID_U2)
    X_LES_SUBGRID_V2 = X_LES_SUBGRID_U2
    X_LES_SUBGRID_W2 = X_LES_SUBGRID_U2
    CALL LES_MEAN_SUBGRID(2./3.*PTKET*MZF(KKA,KKU,KKL,&
               & GZ_M_W(KKA,KKU,KKL,PTHLT,PDZZ)),X_LES_RES_ddz_Thl_SBG_W2)
    IF (KRR>=1) &
    CALL LES_MEAN_SUBGRID(2./3.*PTKET*MZF(KKA,KKU,KKL,&
               & GZ_M_W(KKA,KKU,KKL,PRT(:,:,:,1),PDZZ)),X_LES_RES_ddz_Rt_SBG_W2)
    DO JSV=1,NSV
      CALL LES_MEAN_SUBGRID(2./3.*PTKET*MZF(KKA,KKU,KKL,&
 & GZ_M_W(KKA,KKU,KKL,PSVT(:,:,:,JSV),PDZZ)),X_LES_RES_ddz_Sv_SBG_W2(:,:,:,JSV))
    END DO
  END IF

!----------------------------------------------------------------------------
!
!*     12. LES mixing end dissipative lengths, presso-correlations
!          -------------------------------------------------------
!
  CALL LES_MEAN_SUBGRID(ZLM,X_LES_SUBGRID_LMix)
  CALL LES_MEAN_SUBGRID(ZLEPS,X_LES_SUBGRID_LDiss)
!
!* presso-correlations for subgrid Tke are equal to zero.
!
  ZLM = 0.
  CALL LES_MEAN_SUBGRID(ZLM,X_LES_SUBGRID_WP)
!
  CALL SECOND_MNH(ZTIME2)
  XTIME_LES = XTIME_LES + ZTIME2 - ZTIME1
END IF

!

!
!----------------------------------------------------------------------------
!
CONTAINS
!
!
!     ##############################################
      SUBROUTINE UPDATE_ROTATE_WIND(PUSLOPE,PVSLOPE)
!     ##############################################
!!
!!****  *UPDATE_ROTATE_WIND* routine to set rotate wind values at the border
!
!!    AUTHOR
!!    ------
!!
!!     P Jabouille   *CNRM METEO-FRANCE
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   24/06/99
!!      J.Escobar 21/03/2013: for HALOK comment all NHALO=1 test
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODE_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
USE MODD_CONF
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PUSLOPE,PVSLOPE
! tangential surface fluxes in the axes following the orography
!
!*       0.2   Declarations of local variables :
!
INTEGER             :: IIB,IIE,IJB,IJE ! index values for the physical subdomain
TYPE(LIST_ll), POINTER :: TZFIELDS_ll  ! list of fields to exchange
INTEGER                :: IINFO_ll     ! return code of parallel routine
!
!*        1  PROLOGUE
!
NULLIFY(TZFIELDS_ll)
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
!
!         2 Update halo if necessary
!
!!$IF (NHALO == 1) THEN
  CALL ADD2DFIELD_ll(TZFIELDS_ll,PUSLOPE)
  CALL ADD2DFIELD_ll(TZFIELDS_ll,PVSLOPE)
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
!!$ENDIF
!
!        3 Boundary conditions for non cyclic case
!
IF ( HLBCX(1) /= "CYCL" .AND. LWEST_ll()) THEN
  PUSLOPE(IIB-1,:)=PUSLOPE(IIB,:)
  PVSLOPE(IIB-1,:)=PVSLOPE(IIB,:)
END IF
IF ( HLBCX(2) /= "CYCL" .AND. LEAST_ll()) THEN
  PUSLOPE(IIE+1,:)=PUSLOPE(IIE,:)
  PVSLOPE(IIE+1,:)=PVSLOPE(IIE,:)
END IF
IF ( HLBCY(1) /= "CYCL" .AND. LSOUTH_ll()) THEN
  PUSLOPE(:,IJB-1)=PUSLOPE(:,IJB)
  PVSLOPE(:,IJB-1)=PVSLOPE(:,IJB)
END IF
IF(  HLBCY(2) /= "CYCL" .AND. LNORTH_ll()) THEN
  PUSLOPE(:,IJE+1)=PUSLOPE(:,IJE)
  PVSLOPE(:,IJE+1)=PVSLOPE(:,IJE)
END IF
!
END SUBROUTINE UPDATE_ROTATE_WIND
!
!     ########################################################################
      SUBROUTINE COMPUTE_FUNCTION_THERMO(PALP,PBETA,PGAM,PLTT,PC,PT,PEXN,PCP,&
                                         PLOCPEXN,PAMOIST,PATHETA            )
!     ########################################################################
!!
!!****  *COMPUTE_FUNCTION_THERMO* routine to compute several thermo functions
!
!!    AUTHOR
!!    ------
!!
!!     JP Pinty      *LA*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   24/02/03
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_CST
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments 
!
REAL,                   INTENT(IN)    :: PALP,PBETA,PGAM,PLTT,PC
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PT,PEXN,PCP
!
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PLOCPEXN
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PAMOIST,PATHETA
! 
!*       0.2   Declarations of local variables
!
REAL                :: ZEPS         ! XMV / XMD
REAL, DIMENSION(SIZE(PEXN,1),SIZE(PEXN,2),SIZE(PEXN,3)) :: ZRVSAT
REAL, DIMENSION(SIZE(PEXN,1),SIZE(PEXN,2),SIZE(PEXN,3)) :: ZDRVSATDT
!
!-------------------------------------------------------------------------------
!
  ZEPS = XMV / XMD
!
!*       1.1 Lv/Cph at  t
!
  PLOCPEXN(:,:,:) = ( PLTT + (XCPV-PC) *  (PT(:,:,:)-XTT) ) / PCP(:,:,:)
!
!*      1.2 Saturation vapor pressure at t
!
  ZRVSAT(:,:,:) =  EXP( PALP - PBETA/PT(:,:,:) - PGAM*ALOG( PT(:,:,:) ) )
!
!*      1.3 saturation  mixing ratio at t
!
  ZRVSAT(:,:,:) =  ZRVSAT(:,:,:) * ZEPS / ( PPABST(:,:,:) - ZRVSAT(:,:,:) )
!
!*      1.4 compute the saturation mixing ratio derivative (rvs')
!
  ZDRVSATDT(:,:,:) = ( PBETA / PT(:,:,:)  - PGAM ) / PT(:,:,:)   &
                 * ZRVSAT(:,:,:) * ( 1. + ZRVSAT(:,:,:) / ZEPS )
!
!*      1.5 compute Amoist
!
  PAMOIST(:,:,:)=  0.5 / ( 1.0 + ZDRVSATDT(:,:,:) * PLOCPEXN(:,:,:) )
!
!*      1.6 compute Atheta
!
  PATHETA(:,:,:)= PAMOIST(:,:,:) * PEXN(:,:,:) *                             &
        ( ( ZRVSAT(:,:,:) - PRT(:,:,:,1) ) * PLOCPEXN(:,:,:) /               &
          ( 1. + ZDRVSATDT(:,:,:) * PLOCPEXN(:,:,:) )        *               &
          (                                                                  &
           ZRVSAT(:,:,:) * (1. + ZRVSAT(:,:,:)/ZEPS)                         &
                        * ( -2.*PBETA/PT(:,:,:) + PGAM ) / PT(:,:,:)**2      &
          +ZDRVSATDT(:,:,:) * (1. + 2. * ZRVSAT(:,:,:)/ZEPS)                 &
                        * ( PBETA/PT(:,:,:) - PGAM ) / PT(:,:,:)             &
          )                                                                  &
         - ZDRVSATDT(:,:,:)                                                  &
        )
!
!*      1.7 Lv/Cph/Exner at t-1
!
  PLOCPEXN(:,:,:) = PLOCPEXN(:,:,:) / PEXN(:,:,:)
!
END SUBROUTINE COMPUTE_FUNCTION_THERMO
!
!     ####################
      SUBROUTINE DELT(PLM)
!     ####################
!!
!!****  *DELT* routine to compute mixing length for DELT case
!
!!    AUTHOR
!!    ------
!!
!!     M Tomasini      *Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   01/05
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!*       0.1   Declarations of dummy arguments 
!
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PLM
!
!*       0.2   Declarations of local variables
!
REAL                :: ZD           ! distance to the surface
!
!-------------------------------------------------------------------------------
!
DO JK = IKTB,IKTE ! 1D turbulence scheme
  PLM(:,:,JK) = PZZ(:,:,JK+KKL) - PZZ(:,:,JK)
END DO
PLM(:,:,KKU) = PLM(:,:,IKE)
PLM(:,:,KKA) = PZZ(:,:,IKB) - PZZ(:,:,KKA)
IF ( HTURBDIM /= '1DIM' ) THEN  ! 3D turbulence scheme
  IF ( L2D) THEN
    PLM(:,:,:) = SQRT( PLM(:,:,:)*MXF(PDXX(:,:,:)) ) 
  ELSE
    PLM(:,:,:) = (PLM(:,:,:)*MXF(PDXX(:,:,:))*MYF(PDYY(:,:,:)) ) ** (1./3.)
  END IF
END IF
!
!  mixing length limited by the distance normal to the surface 
!  (with the same factor as for BL89)
!
IF (.NOT. ORMC01) THEN
  ZALPHA=0.5**(-1.5)
  !
  DO JJ=1,SIZE(PUT,2)
    DO JI=1,SIZE(PUT,1)
      DO JK=IKTB,IKTE
        ZD=ZALPHA*(0.5*(PZZ(JI,JJ,JK)+PZZ(JI,JJ,JK+KKL))&
        -PZZ(JI,JJ,IKB)) *PDIRCOSZW(JI,JJ)
        IF ( PLM(JI,JJ,JK)>ZD) THEN
          PLM(JI,JJ,JK)=ZD
        ELSE
          EXIT
        ENDIF
      END DO
    END DO
  END DO
END IF
!
PLM(:,:,KKA) = PLM(:,:,IKB  )
PLM(:,:,KKU  ) = PLM(:,:,IKE)
!
END SUBROUTINE DELT
!
!     ####################
      SUBROUTINE DEAR(PLM)
!     ####################
!!
!!****  *DELT* routine to compute mixing length for DEARdorff case
!
!!    AUTHOR
!!    ------
!!
!!     M Tomasini      *Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   01/05
!!      I.Sandu (Sept.2006) : Modification of the stability criterion
!!                            (theta_v -> theta_l)
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!*       0.1   Declarations of dummy arguments 
!
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PLM
!
!*       0.2   Declarations of local variables
!
REAL                :: ZD           ! distance to the surface
REAL                :: ZVAR         ! Intermediary variable
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) ::   ZWORK2D
!
REAL, DIMENSION(SIZE(PTHLT,1),SIZE(PTHLT,2),SIZE(PTHLT,3)) ::     &
            ZDTHLDZ,ZDRTDZ,     &!dtheta_l/dz, drt_dz used for computing the stablity
!                                ! criterion 
            ZETHETA,ZEMOIST             !coef ETHETA and EMOIST
!----------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
!   initialize the mixing length with the mesh grid
DO JK = IKTB,IKTE ! 1D turbulence scheme
  PLM(:,:,JK) = PZZ(:,:,JK+KKL) - PZZ(:,:,JK)
END DO
PLM(:,:,KKU) = PLM(:,:,IKE)
PLM(:,:,KKA) = PZZ(:,:,IKB) - PZZ(:,:,KKA)
IF ( HTURBDIM /= '1DIM' ) THEN  ! 3D turbulence scheme
  IF ( L2D) THEN
    PLM(:,:,:) = SQRT( PLM(:,:,:)*MXF(PDXX(:,:,:)) )
  ELSE
    PLM(:,:,:) = (PLM(:,:,:)*MXF(PDXX(:,:,:))*MYF(PDYY(:,:,:)) ) ** (1./3.)
  END IF
END IF
!   compute a mixing length limited by the stability
!
ZETHETA(:,:,:) = ETHETA(KRR,KRRI,PTHLT,PRT,ZLOCPEXNM,ZATHETA,PSRCT)
ZEMOIST(:,:,:) = EMOIST(KRR,KRRI,PTHLT,PRT,ZLOCPEXNM,ZAMOIST,PSRCT)
!
! For dry simulations
IF (KRR>0) THEN
  DO JK = IKTB+1,IKTE-1
    DO JJ=1,SIZE(PUT,2)
      DO JI=1,SIZE(PUT,1)
        ZDTHLDZ(JI,JJ,JK)= 0.5*((PTHLT(JI,JJ,JK+KKL)-PTHLT(JI,JJ,JK    ))/PDZZ(JI,JJ,JK+KKL)+ &
                                (PTHLT(JI,JJ,JK    )-PTHLT(JI,JJ,JK-KKL))/PDZZ(JI,JJ,JK    ))
        ZDRTDZ(JI,JJ,JK) = 0.5*((PRT(JI,JJ,JK+KKL,1)-PRT(JI,JJ,JK    ,1))/PDZZ(JI,JJ,JK+KKL)+ &
                                (PRT(JI,JJ,JK    ,1)-PRT(JI,JJ,JK-KKL,1))/PDZZ(JI,JJ,JK    ))
        ZVAR=XG/PTHVREF(JI,JJ,JK)*                                                  &
             (ZETHETA(JI,JJ,JK)*ZDTHLDZ(JI,JJ,JK)+ZEMOIST(JI,JJ,JK)*ZDRTDZ(JI,JJ,JK))
        !
        IF (ZVAR>0.) THEN
          PLM(JI,JJ,JK)=MAX(XMNH_EPSILON,MIN(PLM(JI,JJ,JK), &
                        0.76* SQRT(PTKET(JI,JJ,JK)/ZVAR)))
        END IF
      END DO
    END DO
  END DO
ELSE
  DO JK = IKTB+1,IKTE-1
    DO JJ=1,SIZE(PUT,2)
      DO JI=1,SIZE(PUT,1)
        ZDTHLDZ(JI,JJ,JK)= 0.5*((PTHLT(JI,JJ,JK+KKL)-PTHLT(JI,JJ,JK    ))/PDZZ(JI,JJ,JK+KKL)+ &
                                (PTHLT(JI,JJ,JK    )-PTHLT(JI,JJ,JK-KKL))/PDZZ(JI,JJ,JK    ))
        ZVAR=XG/PTHVREF(JI,JJ,JK)*ZETHETA(JI,JJ,JK)*ZDTHLDZ(JI,JJ,JK)
        !
        IF (ZVAR>0.) THEN
          PLM(JI,JJ,JK)=MAX(XMNH_EPSILON,MIN(PLM(JI,JJ,JK), &
                        0.76* SQRT(PTKET(JI,JJ,JK)/ZVAR)))
        END IF
      END DO
    END DO
  END DO
END IF
!  special case near the surface 
ZDTHLDZ(:,:,IKB)=(PTHLT(:,:,IKB+KKL)-PTHLT(:,:,IKB))/PDZZ(:,:,IKB+KKL)
! For dry simulations
IF (KRR>0) THEN
  ZDRTDZ(:,:,IKB)=(PRT(:,:,IKB+KKL,1)-PRT(:,:,IKB,1))/PDZZ(:,:,IKB+KKL)
ELSE
  ZDRTDZ(:,:,IKB)=0
ENDIF
!
ZWORK2D(:,:)=XG/PTHVREF(:,:,IKB)*                                           &
            (ZETHETA(:,:,IKB)*ZDTHLDZ(:,:,IKB)+ZEMOIST(:,:,IKB)*ZDRTDZ(:,:,IKB))
WHERE(ZWORK2D(:,:)>0.)
  PLM(:,:,IKB)=MAX(XMNH_EPSILON,MIN( PLM(:,:,IKB),                 &
                    0.76* SQRT(PTKET(:,:,IKB)/ZWORK2D(:,:))))
END WHERE
!
!  mixing length limited by the distance normal to the surface (with the same factor as for BL89)
!
IF (.NOT. ORMC01) THEN
  ZALPHA=0.5**(-1.5)
  !
  DO JJ=1,SIZE(PUT,2)
    DO JI=1,SIZE(PUT,1)
      DO JK=IKTB,IKTE
        ZD=ZALPHA*(0.5*(PZZ(JI,JJ,JK)+PZZ(JI,JJ,JK+KKL))-PZZ(JI,JJ,IKB)) &
          *PDIRCOSZW(JI,JJ)
        IF ( PLM(JI,JJ,JK)>ZD) THEN
          PLM(JI,JJ,JK)=ZD
        ELSE
          EXIT
        ENDIF
      END DO
    END DO
  END DO
END IF
!
PLM(:,:,KKA) = PLM(:,:,IKB  )
PLM(:,:,IKE  ) = PLM(:,:,IKE-KKL)
PLM(:,:,KKU  ) = PLM(:,:,KKU-KKL)
!
END SUBROUTINE DEAR
!
!     #########################
      SUBROUTINE CLOUD_MODIF_LM
!     #########################
!!
!!*****CLOUD_MODIF_LM routine to:
!!       1/ change the mixing length in the clouds
!!       2/ emphasize the mixing length in the cloud
!!           by the coefficient ZCOEF_AMPL calculated here
!!             when the CEI index is above ZCEI_MIN.
!!
!!
!!      ZCOEF_AMPL ^
!!                 |
!!                 |
!!  ZCOEF_AMPL_SAT -                       ---------- Saturation
!!    (XDUMMY1)    |                      -
!!                 |                     -
!!                 |                    -
!!                 |                   -
!!                 |                  - Amplification
!!                 |                 - straight
!!                 |                - line
!!                 |               -
!!                 |              -
!!                 |             -
!!                 |            -
!!                 |           -
!!               1 ------------
!!                 |
!!                 |
!!               0 -----------|------------|----------> PCEI
!!                 0      ZCEI_MIN     ZCEI_MAX
!!                        (XDUMMY2)    (XDUMMY3)
!!
!!
!!
!!    AUTHOR
!!    ------
!!     M. Tomasini   *CNRM METEO-FRANCE
!!
!!    MODIFICATIONS
!!    -------------
!!     Original   09/07/04
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
REAL :: ZPENTE            ! Slope of the amplification straight line
REAL :: ZCOEF_AMPL_CEI_NUL! Ordonnate at the origin of the
                          ! amplification straight line
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZCOEF_AMPL
                          ! Amplification coefficient of the mixing length
                          ! when the instability criterium is verified 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZLM_CLOUD
                          ! Turbulent mixing length in the clouds
!
!-------------------------------------------------------------------------------
!
!*       1.    INITIALISATION
!              --------------
!
ZPENTE = ( PCOEF_AMPL_SAT - 1. ) / ( PCEI_MAX - PCEI_MIN ) 
ZCOEF_AMPL_CEI_NUL = 1. - ZPENTE * PCEI_MIN
!
ZCOEF_AMPL(:,:,:) = 1.
!
!*       2.    CALCULATION OF THE AMPLIFICATION COEFFICIENT
!              --------------------------------------------
!
! Saturation
!
WHERE ( PCEI(:,:,:)>=PCEI_MAX ) ZCOEF_AMPL(:,:,:)=PCOEF_AMPL_SAT
!
! Between the min and max limits of CEI index, linear variation of the
! amplification coefficient ZCOEF_AMPL as a function of CEI
!
WHERE ( PCEI(:,:,:) <  PCEI_MAX .AND.                                        &
        PCEI(:,:,:) >  PCEI_MIN      )                                       &
        ZCOEF_AMPL(:,:,:) = ZPENTE * PCEI(:,:,:) + ZCOEF_AMPL_CEI_NUL  
!
!
!*       3.    CALCULATION OF THE MIXING LENGTH IN CLOUDS
!              ------------------------------------------
!
IF (HTURBLEN_CL == HTURBLEN) THEN
  ZLM_CLOUD(:,:,:) = ZLM(:,:,:)
ELSE
  SELECT CASE (HTURBLEN_CL)
!
!*         3.1 BL89 mixing length
!           ------------------
  CASE ('BL89','RM17')
    ZSHEAR=0.
    CALL BL89(KKA,KKU,KKL,PZZ,PDZZ,PTHVREF,ZTHLM,KRR,ZRM,PTKET,ZSHEAR,ZLM_CLOUD)
!
!*         3.2 Delta mixing length
!           -------------------
  CASE ('DELT')
    CALL DELT(ZLM_CLOUD)
!
!*         3.3 Deardorff mixing length
!           -----------------------
  CASE ('DEAR')
    CALL DEAR(ZLM_CLOUD)
!
  END SELECT
ENDIF
!
!*       4.    MODIFICATION OF THE MIXING LENGTH IN THE CLOUDS
!              -----------------------------------------------
!
! Impression before modification of the mixing length
IF ( OTURB_DIAG .AND. OCLOSE_OUT ) THEN
  TZFIELD%CMNHNAME   = 'LM_CLEAR_SKY'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'LM_CLEAR_SKY'
  TZFIELD%CUNITS     = 'm'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = 'X_Y_Z_LM CLEAR SKY'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZLM)
ENDIF
!
! Amplification of the mixing length when the criteria are verified
!
WHERE (ZCOEF_AMPL(:,:,:) /= 1.) ZLM(:,:,:) = ZCOEF_AMPL(:,:,:)*ZLM_CLOUD(:,:,:)
!
! Cloud mixing length in the clouds at the points which do not verified the CEI
!
WHERE (PCEI(:,:,:) == -1.) ZLM(:,:,:) = ZLM_CLOUD(:,:,:)
!
!
!*       5.    IMPRESSION
!              ----------
!
IF ( OTURB_DIAG .AND. OCLOSE_OUT ) THEN
  TZFIELD%CMNHNAME   = 'COEF_AMPL'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'COEF_AMPL'
  TZFIELD%CUNITS     = '1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = 'X_Y_Z_COEF AMPL'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZCOEF_AMPL)
  !
  TZFIELD%CMNHNAME   = 'LM_CLOUD'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'LM_CLOUD'
  TZFIELD%CUNITS     = 'm'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = 'X_Y_Z_LM CLOUD'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZLM_CLOUD)
  !
ENDIF
!
END SUBROUTINE CLOUD_MODIF_LM
!
END SUBROUTINE TURB    
