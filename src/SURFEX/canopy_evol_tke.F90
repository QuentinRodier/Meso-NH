!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE CANOPY_EVOL_TKE(KI,KLVL,PTSTEP,PRHOA,             &
                                   PZ,PZF,PDZ,PDZF,                  &
                                   PFORC_E,PDFORC_EDE,               &
                                   PU,PTH,PUW,PWTH,PWQ,PLEPS,PTKE    )  
!     #########################################
!
!!****  *CANOPY_EVOL_TKE* - evolution of wind in canopy 
!!                        
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2006 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_TRIDIAG_SURF
USE MODD_CANOPY_TURB, ONLY : XCED, XTKEMIN
USE MODD_CSTS,        ONLY : XG, XRD, XRV
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                  INTENT(IN)    :: KI        ! number of horizontal points
INTEGER,                  INTENT(IN)    :: KLVL      ! number of levels in canopy
REAL,                     INTENT(IN)    :: PTSTEP    ! time-step                             (s)
REAL, DIMENSION(KI),      INTENT(IN)    :: PRHOA     ! Air density                           (kg/m3)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PZ        ! Z at full levels                      (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PZF       ! Z at half levels                      (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZ       ! deltaZ between canopy half levels,
!                                                    ! located at full levels                (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZF      ! deltaZ between canopy (full) levels,
!                                                    ! located at half levels                (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PFORC_E   ! tendency of wind due to canopy drag   (m2/s3)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDFORC_EDE! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PU        ! wind speed at canopy levels           (m/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PTH       ! pot. temp. at canopy levels           (K)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PUW       ! turbulent flux (at half levels)       (m2/s2)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PWTH      ! w'Th' flux (at half levels)           (mK/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PWQ       ! w'q'  flux (at half levels)           (kg/m2/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PLEPS     ! dissipative length (full levels)      (m)
REAL, DIMENSION(KI,KLVL), INTENT(INOUT) :: PTKE      ! TKE at canopy levels   
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                    :: JLAYER   ! loop counter on layers
!
REAL, DIMENSION(KI,KLVL)   :: ZDUDZ    ! dU/dz at mid levels
REAL, DIMENSION(KI,KLVL)   :: ZDP      ! dynamical production at full levels
!                                      ! (at full levels)
REAL, DIMENSION(KI,KLVL)   :: ZTP      ! thermal   production at full levels
!                                      ! (at full levels)
REAL, DIMENSION(KI,KLVL)   :: ZDISS_O_TKE ! dissipation/TKE ratio at full levels
REAL, DIMENSION(KI,KLVL)   :: ZF       ! turbulent flux at mid levels
REAL, DIMENSION(KI,KLVL)   :: ZDFDDVDZ ! derivative of turbulent flux as a
!                                      ! function of vertical gradient of wind variable
!                                      ! (at mid levels)
REAL, DIMENSION(KI,KLVL)   :: ZEXT     ! external forcing at full levels
REAL, DIMENSION(KI,KLVL)   :: ZDEXTDV  ! derivative of external forcing as a
!                                      ! function of vertical variable
!                                      ! (at full levels)
REAL, DIMENSION(KI,KLVL)   :: ZTKE     ! TKE     at canopy levels (work var.)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*    1. initializations
!        ---------------
!
!* external forces
!
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_TKE',0,ZHOOK_HANDLE)
ZEXT = 0.
ZDEXTDV = 0.
!
!* turbulent flux
!
ZF       = 0.
ZDFDDVDZ = 0.
!
!-------------------------------------------------------------------------------
!
!*    2. wind vertical derivative mid layers (at half levels below full levels)
!        ------------------------
!
ZDUDZ(:,:) = -999.
DO JLAYER=2,KLVL
  ZDUDZ(:,JLAYER) = (PU(:,JLAYER) - PU(:,JLAYER-1)) / PDZF(:,JLAYER)
END DO
!-------------------------------------------------------------------------------
!
!*    3. Dynamical production of TKE (at full levels)
!        ---------------------------
!
ZDP(:,:) = -999.
!
!* first level using an extrapolation using a 1/z law
ZDP(:,1) = - PUW(:,2) * ZDUDZ(:,2) * (PZF(:,2)/PZ(:,1))

! other levels
DO JLAYER=2,KLVL-1
  ZDP(:,JLAYER) = - 0.5 * (PUW(:,JLAYER)   * ZDUDZ(:,JLAYER)  ) &
                    - 0.5 * (PUW(:,JLAYER+1) * ZDUDZ(:,JLAYER+1))  
END DO
!
!* upper level using an extrapolation using a 1/z law
ZDP(:,KLVL) = - PUW(:,KLVL) * ZDUDZ(:,KLVL) * (PZF(:,KLVL)/PZ(:,KLVL))
!
!* adds dynamical production in non-transport forces
ZEXT    = ZEXT    + ZDP
ZDEXTDV = ZDEXTDV + 0.
!
!-------------------------------------------------------------------------------
!
!*    4. Thermal production of TKE (at full levels)
!        -------------------------
!
ZTP(:,:) = -999.
!
! other levels
DO JLAYER=1,KLVL-1
  ZTP(:,JLAYER) = XG/PTH(:,JLAYER) * 0.5 * ( (PWTH(:,JLAYER)+ PWTH(:,JLAYER+1))          &
                                   + (XRV/XRD-1) * (PWQ(:,JLAYER) + PWQ(:,JLAYER)   )/PRHOA(:) )  
END DO
!
!* upper level
ZTP(:,KLVL) = XG/PTH(:,KLVL) * PWTH(:,KLVL)
!
!* adds dynamical production in non-transport forces
ZEXT    = ZEXT    + ZTP
ZDEXTDV = ZDEXTDV + 0.
!
!-------------------------------------------------------------------------------
!
!*    4. Dissipation/TKE ratio (to prepare implicitation of dissipation)
!        ---------------------
!
ZDISS_O_TKE = - XCED * SQRT(PTKE(:,:)) / PLEPS(:,:)
!
!* adds dissipation in non-transport forces
ZEXT    = ZEXT    +       ZDISS_O_TKE * PTKE(:,:)
ZDEXTDV = ZDEXTDV + 1.5 * ZDISS_O_TKE
!
!-------------------------------------------------------------------------------
!
!*    6. Adds Creation force due to canopy
!        ---------------------------------
!
!
ZEXT    = ZEXT    + PFORC_E(:,:)
ZDEXTDV = ZDEXTDV + PDFORC_EDE(:,:)


!-------------------------------------------------------------------------------
!
!*    7. Evolution of TKE due to Dyn prod, Dissipation and Drag production
!        -----------------------------------------------------------------
!
!* note that dissipation is implicited
!
 CALL TRIDIAG_SURF(PTKE,ZF,ZDFDDVDZ,ZEXT,ZDEXTDV,PTSTEP, &
                    PDZF,PDZ,ZTKE                         )  
!
!-------------------------------------------------------------------------------
!
!*    7. New value of TKE (at full levels)
!        ----------------
!
PTKE(:,:) = ZTKE(:,:)
!
!-------------------------------------------------------------------------------
!
!*    8. Security at all levels : set minimum threshold
!        ----------------------
!
PTKE(:,:) = MAX(PTKE,XTKEMIN)
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_TKE',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------
!
END SUBROUTINE CANOPY_EVOL_TKE
