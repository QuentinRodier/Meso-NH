!-----------------------------------------------------------------
!     #####################
      MODULE MODI_ADVECTION
!     #####################
!
INTERFACE
      SUBROUTINE ADVECTION (HUVW_ADV_SCHEME,HMET_ADV_SCHEME,HSV_ADV_SCHEME,    &
                           KLITER, HLBCX, HLBCY,KRR, KSV, KTCOUNT,             &
                           PTSTEP_MET, PTSTEP_SV,                              & 
                           PUM, PVM, PWM, PTHM, PRM, PTKEM, PSVM,              &
                           PUT, PVT, PWT, PTHT, PRT, PTKET, PSVT,              &
                           PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,               &
                           PRUS,PRVS, PRWS, PRTHS, PRRS, PRTKES, PRSVS,        &
                           TPHALO2MLIST, TPHALO2LIST, TPHALO2SLIST )
!
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER(LEN=6),         INTENT(IN)    :: HMET_ADV_SCHEME, & ! Control of the 
                                           HSV_ADV_SCHEME,  & ! scheme applied 
                                           HUVW_ADV_SCHEME     ! to the selected
                                                              ! variables 
!
INTEGER,                  INTENT(IN)    :: KLITER        ! Iteration number for
                                                         ! the MPDATA scheme
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
INTEGER,                  INTENT(IN)    :: KRR  ! Number of moist variables
INTEGER,                  INTENT(IN)    :: KSV  ! Number of Scalar Variables
INTEGER,                  INTENT(IN)    :: KTCOUNT  ! iteration count
REAL,                     INTENT(IN)    :: PTSTEP_MET !  Effective time step for
                                                ! meteorological scalar variables 
                                                ! (depending on advection scheme)
REAL,                     INTENT(IN)    :: PTSTEP_SV !  Effective time step for
                                                ! tracer scalar variables 
                                                ! (depending on advection scheme)
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHM, PTKEM
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRM , PSVM
                                                  ! Variables at t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUM, PVM, PWM
                                                  ! Variables at t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT , PVT  , PWT, PRHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT , PSVT
                                                  ! Variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ,PDZX,PDZY
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS , PRVS  , PRWS
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS, PRTKES
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS , PRSVS
                                                  ! Sources terms 
!
! halo lists for 4th order advection
TYPE(HALO2LIST_ll), POINTER :: TPHALO2MLIST ! momentum variables
TYPE(HALO2LIST_ll), POINTER :: TPHALO2LIST  ! meteorological scalar variables
TYPE(HALO2LIST_ll), POINTER :: TPHALO2SLIST ! tracer scalar variables
!
END SUBROUTINE ADVECTION
!
END INTERFACE
!
END MODULE MODI_ADVECTION 
!     ##########################################################################
      SUBROUTINE ADVECTION (HUVW_ADV_SCHEME,HMET_ADV_SCHEME,HSV_ADV_SCHEME,    &
                           KLITER, HLBCX, HLBCY,KRR, KSV, KTCOUNT,             &
                           PTSTEP_MET, PTSTEP_SV,                              & 
                           PUM, PVM, PWM, PTHM, PRM, PTKEM, PSVM,              &
                           PUT, PVT, PWT, PTHT, PRT, PTKET, PSVT,              &
                           PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,               &
                           PRUS,PRVS, PRWS, PRTHS, PRRS, PRTKES, PRSVS,        &
                           TPHALO2MLIST, TPHALO2LIST, TPHALO2SLIST )
!     ##########################################################################
!
!!****  *ADVECTION * - routine to call the specialized advection routines
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to control the advection routines.
!!    For that, it is first necessary to compute the metric coefficients
!!    and the contravariant components of the momentum.
!!
!!**  METHOD
!!    ------
!!      The advection of momenta is calculated using a centred (second order) 
!!    scheme. Three schemes can be used to calculated the advection of a 
!!    scalar: centred (2nd) (ADVECSCALAR), Flux-Corrected Transport Scalar
!!    (FCT_SCALAR) and a Multidimensional Positive Definite Advection Transport
!!    Algorithm (MPDATA).
!!      Once the scheme is selected, it is applied to the following group of
!!    variables: METeorologicals (temperature, water substances, TKE,
!!    dissipation TKE) and Scalar Variables. It is possible to select different
!!    advection schemes for each group of variables.
!!
!!    EXTERNAL
!!    --------
!!      Functions MXM,MYM,MZM  : computes the averages along the 3 directions
!!      CONTRAV              : computes the contravariant components.
!!      ADVECUVW             : computes the advection terms for momentum.
!!      ADVECSCALAR          : computes the advection terms for scalar fields.
!!      ADD3DFIELD_ll        : add a field to 3D-list
!!      ADVEC_4TH_ORDER      : 4th order advection scheme
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE
!!
!!    REFERENCE
!!    ---------
!!      Book1 and book2 ( routine ADVECTION )
!!
!!    AUTHOR
!!    ------
!!	J.-P. Pinty      * Laboratoire d'Aerologie*
!!	J.-P. Lafore     * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/07/94 
!!                  01/04/95 (Ph. Hereil J. Nicolau) add the model number
!!                  23/10/95 (J. Vila and JP Lafore) advection schemes scalar
!!                  16/01/97 (JP Pinty)              change presentation 
!!                  30/04/98 (J. Stein P Jabouille)  extrapolation for the cyclic
!!                                                   case and parallelisation
!!                  24/06/99 (P Jabouille)           case of NHALO>1
!!                  25/10/05 (JP Pinty)              4th order scheme
!!                  24/04/06 (C.Lac)                 Split scalar and passive
!!                                                   tracer routines
!!                  08/06    (T.Maric)               PPM scheme
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll, HALO2LIST_ll
USE MODD_CONF
USE MODD_BLANK
USE MODD_GRID_n
!
USE MODI_SHUMAN
USE MODI_CONTRAV
USE MODI_ADVECUVW
USE MODI_ADVECUVW_4TH
USE MODI_ADVECMET      
USE MODI_ADVECMET_4TH
USE MODI_FCT_MET   
USE MODI_MPDATA
USE MODI_ADVECSCALAR
USE MODI_ADVECSCALAR_4TH
USE MODI_FCT_SCALAR 
USE MODI_MPDATA_SCALAR
USE MODI_PPM_MET
USE MODI_PPM_SCALAR
!
!
!-------------------------------------------------------------------------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER(LEN=6),         INTENT(IN)    :: HMET_ADV_SCHEME, & ! Control of the 
                                           HSV_ADV_SCHEME,  & ! scheme applied 
                                           HUVW_ADV_SCHEME     ! to the selected
                                                              ! variables 
!
INTEGER,                  INTENT(IN)    :: KLITER        ! Iteration number for
                                                         ! the MPDATA scheme
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
INTEGER,                  INTENT(IN)    :: KRR  ! Number of moist variables
INTEGER,                  INTENT(IN)    :: KSV  ! Number of Scalar Variables
!
INTEGER,                  INTENT(IN)    :: KTCOUNT  ! iteration count
REAL,                     INTENT(IN)    :: PTSTEP_MET !  Effective time step for
                                                ! meteorological scalar variables 
                                                ! (depending on advection scheme)
REAL,                     INTENT(IN)    :: PTSTEP_SV !  Effective time step for
                                                ! tracer scalar variables 
                                                ! (depending on advection scheme)
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHM, PTKEM
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRM , PSVM
                                                  ! Variables at t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUM, PVM, PWM
                                                  ! Variables at t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT , PVT  , PWT
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET, PRHODJ
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT , PSVT
                                                  ! Variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ,PDZX,PDZY
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS , PRVS, PRWS
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS, PRTKES
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS , PRSVS
                                                  ! Sources terms 
!
!
!*       0.2   declarations of local variables
!
!
!  
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRUT 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRVT 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRWT
                                                  ! cartesian 
                                                  ! components of
                                                  ! momentum
!
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRUCT 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRVCT
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRWCT
                                                  ! contravariant
                                                  ! components
                                                  ! of momentum
!
INTEGER                     :: IINFO_ll    ! return code of parallel routine
TYPE(LIST_ll), POINTER      :: TZFIELDS_ll ! list of fields to exchange
! halo lists for 4th order advection
TYPE(HALO2LIST_ll), POINTER :: TPHALO2MLIST ! momentum variables
TYPE(HALO2LIST_ll), POINTER :: TPHALO2LIST  ! meteorological scalar variables
TYPE(HALO2LIST_ll), POINTER :: TPHALO2SLIST ! tracer scalar variables
INTEGER :: IKU
!
!-------------------------------------------------------------------------------
!
!
IKU=SIZE(XZHAT)
!*       1.     COMPUTES THE CONTRAVARIANT COMPONENTS
!	        -------------------------------------
!
ZRUT = PUT(:,:,:) * MXM(PRHODJ)
ZRVT = PVT(:,:,:) * MYM(PRHODJ)
ZRWT = PWT(:,:,:) * MZM(1,IKU,1,PRHODJ)
!
IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN                                      
  CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY, &
                ZRUCT,ZRVCT,ZRWCT,2)
ELSEIF (HUVW_ADV_SCHEME=='CEN4TH') THEN
  CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY, &
                ZRUCT,ZRVCT,ZRWCT,4)
ENDIF
!
NULLIFY(TZFIELDS_ll)
IF(NHALO == 1) THEN
  CALL ADD3DFIELD_ll(TZFIELDS_ll, ZRWCT)
  CALL ADD3DFIELD_ll(TZFIELDS_ll, ZRUCT)
  CALL ADD3DFIELD_ll(TZFIELDS_ll, ZRVCT)
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.     CALLS THE ADVECTION ROUTINES FOR THE MOMENTUM 
!	        ---------------------------------------------
!
! choose between 2nd and 4th order momentum advection.
IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN                                      
!
   CALL ADVECUVW (PUT,PVT,PWT,ZRUCT,ZRVCT,ZRWCT,PRUS,PRVS,PRWS)
!
ELSEIF (HUVW_ADV_SCHEME=='CEN4TH') THEN
! 
   CALL ADVECUVW_4TH ( HLBCX, HLBCY, ZRUCT, ZRVCT, ZRWCT,            &
                       PUT, PVT, PWT, PRUS, PRVS, PRWS, TPHALO2MLIST )                 
!
END IF
!
!-------------------------------------------------------------------------------
!
!*       3.     CALLS THE ADVECTION ROUTINES FOR THE METEOROLOGICAL SCALARS 
!	        -----------------------------------------------------------
!
!            3.1. 2nd order scheme
!
IF (HMET_ADV_SCHEME=='CEN2ND') THEN
!
   CALL ADVECMET (KRR, PTHT,PRT,PTKET,     &
                  ZRUCT,ZRVCT,ZRWCT,       &
                  PRTHS,PRRS,PRTKES        )
!
!             3.2. 4th order scheme
!
ELSEIF (HMET_ADV_SCHEME =='CEN4TH' ) THEN
!
   CALL ADVECMET_4TH (HLBCX,HLBCY, KRR,                &
                      ZRUCT, ZRVCT, ZRWCT,             &
                      PTHT, PTKET, PRT,                &
                      PRTHS, PRTKES, PRRS, TPHALO2LIST )
!
!             3.3. Flux-Corrected Transport scheme
!
ELSEIF ( HMET_ADV_SCHEME=='FCT2ND') THEN
!
   CALL FCT_MET  (HLBCX, HLBCY,KRR,                        &
                  PTSTEP_MET, PRHODJ, PTHM, PRM, PTKEM,    &
                  PTHT, PRT, PTKET,                        &
                  ZRUCT, ZRVCT, ZRWCT,                     &
                  PRTHS, PRRS, PRTKES                      )
!
!             3.4. MPDATA scheme
!
ELSEIF (HMET_ADV_SCHEME=='MPDATA') THEN
!
   CALL MPDATA (KLITER, HLBCX, HLBCY, KRR,                 &
                PTSTEP_MET, PRHODJ, PTHM, PRM, PTKEM,      &
                PTHT, PRT, PTKET,                          &
                ZRUCT, ZRVCT, ZRWCT,                       &
                PRTHS, PRRS, PRTKES                        )
!
!             3.5. PPM schemes
!   
ELSEIF (HMET_ADV_SCHEME(1:3)=='PPM') THEN
!
! extrapolate velocity field to t+dt/2 to use in forward in time PPM
! advection scheme
!
   ZRUT = (1.5*PUT(:,:,:) - 0.5*PUM(:,:,:))
   ZRVT = (1.5*PVT(:,:,:) - 0.5*PVM(:,:,:))
   ZRWT = (1.5*PWT(:,:,:) - 0.5*PWM(:,:,:))
! calculate Courant numbers
  IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN                                      
    CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY, &
                ZRUCT,ZRVCT,ZRWCT,2)
  ELSEIF (HUVW_ADV_SCHEME=='CEN4TH') THEN
    CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY, &
                ZRUCT,ZRVCT,ZRWCT,4)
  ENDIF
!
   ZRUCT = ZRUCT*PTSTEP_MET
   ZRVCT = ZRVCT*PTSTEP_MET
   ZRWCT = ZRWCT*PTSTEP_MET

   CALL PPM_MET   (HLBCX,HLBCY, KRR, KTCOUNT,                  &
                   ZRUCT, ZRVCT, ZRWCT, PTSTEP_MET, PRHODJ,    &
                   PTHT, PTKET, PRT, PRTHS, PRTKES, PRRS,      &
                   HMET_ADV_SCHEME                             )
!
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.     CALLS THE ADVECTION ROUTINES FOR TRACERS
!	        ----------------------------------------
!
!            4.1. 2nd order scheme
!
IF (HSV_ADV_SCHEME=='CEN2ND') THEN
!
   CALL ADVECSCALAR  (KSV, PSVT, ZRUCT,ZRVCT,ZRWCT,PRSVS )             
!
!             4.2. 4th order scheme
!
ELSEIF (HSV_ADV_SCHEME =='CEN4TH' ) THEN
!
   CALL ADVECSCALAR_4TH (HLBCX,HLBCY, KSV,          &
                         ZRUCT, ZRVCT, ZRWCT,       &
                         PSVT, PRSVS, TPHALO2SLIST   )           
!
!             4.3. Flux-Corrected Transport scheme
!
ELSEIF ( HSV_ADV_SCHEME=='FCT2ND') THEN
!
   CALL FCT_SCALAR  (HLBCX, HLBCY, KSV,             &
                     PTSTEP_SV, PRHODJ, PSVM,PSVT,  &
                     ZRUCT, ZRVCT, ZRWCT, PRSVS     ) 
!
!             4.4. MPDATA scheme
!
ELSEIF (HSV_ADV_SCHEME=='MPDATA') THEN
!
   CALL MPDATA_SCALAR ( KLITER, HLBCX, HLBCY, KSV,           &
                        PTSTEP_SV, PRHODJ, PSVM, PSVT,       &
                        ZRUCT, ZRVCT, ZRWCT,  PRSVS          )           
!
!             4.5. PPM schemes
!   
ELSEIF (HSV_ADV_SCHEME(1:3)=='PPM') THEN
!
! extrapolate velocity field to t+dt/2 to use in forward in time PPM
! advection scheme
!
   ZRUT = (1.5*PUT(:,:,:) - 0.5*PUM(:,:,:))
   ZRVT = (1.5*PVT(:,:,:) - 0.5*PVM(:,:,:))
   ZRWT = (1.5*PWT(:,:,:) - 0.5*PWM(:,:,:))
! calculate Courant numbers
  IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN                                      
    CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY, &
                ZRUCT,ZRVCT,ZRWCT,2)
  ELSEIF (HUVW_ADV_SCHEME=='CEN4TH') THEN
    CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY, &
                ZRUCT,ZRVCT,ZRWCT,4)
  ENDIF
!
   ZRUCT = ZRUCT*PTSTEP_SV
   ZRVCT = ZRVCT*PTSTEP_SV
   ZRWCT = ZRWCT*PTSTEP_SV

   CALL PPM_SCALAR(HLBCX,HLBCY, KSV, KTCOUNT,               &
                   ZRUCT, ZRVCT, ZRWCT, PTSTEP_SV, PRHODJ,  &
                   PSVT, PRSVS, HSV_ADV_SCHEME      )                     
!
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ADVECTION
