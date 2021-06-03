!MNH_LIC Copyright 1994-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-------------------------------------------------------------------------------
!     #####################
      MODULE MODI_VISCOSITY
!     #####################
!
INTERFACE
!
    SUBROUTINE VISCOSITY(HLBCX, HLBCY, KRR, KSV, PNU, PPRANDTL,          &
                        OVISC_UVW, OVISC_TH, OVISC_SV, OVISC_R,         &
                        ODRAG,  &
                        PUT, PVT, PWT, PTHT, PRT, PSVT,                 &
                        PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,           &
                        PRUS, PRVS, PRWS, PRTHS, PRRS, PRSVS,PDRAG      )
!
     IMPLICIT NONE
!
!*       0.1   Declarations of arguments:
!
! X and Y lateral boundary conditions
     CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY 
!
     INTEGER, INTENT(IN) :: KRR      ! number of moist variables
     INTEGER, INTENT(IN) :: KSV      ! number of scalar variables
!
     REAL, INTENT(IN) :: PNU         ! viscosity coefficient
     REAL, INTENT(IN) :: PPRANDTL    ! Parandtl number
!
!
! logical switches
     LOGICAL, INTENT(IN) :: OVISC_UVW ! momentum
     LOGICAL, INTENT(IN) :: OVISC_TH  ! theta
     LOGICAL, INTENT(IN) :: OVISC_SV  ! scalar tracer
     LOGICAL, INTENT(IN) :: OVISC_R   ! moisture
     LOGICAL, INTENT(IN) :: ODRAG     ! noslip/freeslip 
!
!
! input variables at time t
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PUT
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PVT
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PWT
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PTHT
     REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRT
     REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PSVT
!
!
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PRHODJ ! rho_ref * Jacobian
!
      REAL, DIMENSION(:,:), INTENT(IN) :: PDRAG ! Array -1/1 defining where the no-slipcondition is applied
! metric coefficients
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDXX
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDYY
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZZ
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZX
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZY
!
! output source terms
     REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRUS, PRVS, PRWS
     REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRTHS
     REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS, PRSVS
!
   END SUBROUTINE VISCOSITY
!
END INTERFACE
!
END MODULE MODI_VISCOSITY
!
!-------------------------------------------------------------------------------
!
SUBROUTINE VISCOSITY(HLBCX, HLBCY, KRR, KSV, PNU, PPRANDTL,          &
                     OVISC_UVW, OVISC_TH, OVISC_SV, OVISC_R,         &
                     ODRAG,        &
                     PUT, PVT, PWT, PTHT, PRT, PSVT,                 &
                     PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,           &
                     PRUS, PRVS, PRWS, PRTHS, PRRS, PRSVS,PDRAG      )
!
!    IMPLICIT ARGUMENTS
!    ------------------ 
!      Module MODD_PARAMETERS: JPHEXT, JPVEXT
!      Module MODD_CONF: L2D
!
!!    AUTHOR
!!    ------
!!      Jeanne Colin                      * CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!      01/18 (C.Lac) Add budgets
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!  P. Wautelet 08/11/2019: corrected wrong budget name VISC_BU_RU -> VISC_BU_RTH
!  P. Wautelet    02/2020: use the new data structures and subroutines for budgets
!       02/21 (T.Nagel) Add adhesion condition in case of an IBM-obstacle at the domain top boundary
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
  USE MODD_ARGSLIST_ll, ONLY: LIST_ll
  use modd_budget,      only: lbudget_u,  lbudget_v,  lbudget_w,  lbudget_th, lbudget_rv,  lbudget_rc,  &
                              lbudget_rr, lbudget_ri, lbudget_rs, lbudget_rg, lbudget_rh,  lbudget_sv,  &
                              NBUDGET_U,  NBUDGET_V,  NBUDGET_W,  NBUDGET_TH, NBUDGET_RV,  NBUDGET_RC,  &
                              NBUDGET_RR, NBUDGET_RI, NBUDGET_RS, NBUDGET_RG, NBUDGET_RH,  NBUDGET_SV1, &
                              tbudgets
  USE MODD_CONF
  USE MODD_DRAG_n
  USE MODD_PARAMETERS
  USE MODD_VISCOSITY

  use mode_budget,      only: Budget_store_init, Budget_store_end
  USE MODE_ll

  USE MODI_SHUMAN
  USE MODI_LAP_M
!
!-------------------------------------------------------------------------------
!
  IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
! X and Y lateral boundary conditions
  CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY 
!
  INTEGER, INTENT(IN) :: KRR      ! number of moist variables
  INTEGER, INTENT(IN) :: KSV      ! number of scalar variables
!
  REAL, INTENT(IN) :: PPRANDTL    ! Parandtl number
  REAL, INTENT(IN) :: PNU         ! viscous diffusion rate 
!
! logical switches
     LOGICAL, INTENT(IN) :: OVISC_UVW ! momentum
     LOGICAL, INTENT(IN) :: OVISC_TH  ! theta
     LOGICAL, INTENT(IN) :: OVISC_SV  ! scalar tracer
     LOGICAL, INTENT(IN) :: OVISC_R   ! moisture
     LOGICAL, INTENT(IN) :: ODRAG     ! noslip/freeslip
!
! input variables at time t
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PUT
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PVT
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PWT
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PTHT
     REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRT
     REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PSVT
!
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PRHODJ ! rho_ref * Jacobian
!
!
REAL, DIMENSION(:,:), INTENT(IN) :: PDRAG ! Array -1/1 defining where the no-slip condition is applied

     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDXX
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDYY
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZZ
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZX
     REAL, DIMENSION(:,:,:), INTENT(IN) :: PDZY
!
! output source terms
     REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRUS, PRVS, PRWS
     REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRTHS
     REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS, PRSVS
!
!
!*       0.2   Declarations of local variables
!
  INTEGER :: IK ! counter
  INTEGER :: IKB, IKE
!
  REAL, DIMENSION(SIZE(PWT,1),SIZE(PWT,2),SIZE(PWT,3)) :: ZTMP ! temp storage
  REAL, DIMENSION(SIZE(PWT,1),SIZE(PWT,2),SIZE(PWT,3)) :: ZLAPu ! temp storage
  REAL, DIMENSION(SIZE(PWT,1),SIZE(PWT,2),SIZE(PWT,3)) :: ZLAPv ! temp storage
  REAL, DIMENSION(SIZE(PWT,1),SIZE(PWT,2),SIZE(PWT,3)) :: ZY1 ! temp storage
  REAL, DIMENSION(SIZE(PWT,1),SIZE(PWT,2),SIZE(PWT,3)) :: ZY2 ! temp storage
!
!
INTEGER                          :: IIU,IJU,IKU         ! I,J,K array sizes
!
INTEGER                          :: JI,JJ,JK  ! I loop index
INTEGER :: IINFO_ll
TYPE(LIST_ll), POINTER :: TZFIELDS_ll   ! list of fields to exchange
!
!
!-------------------------------------------------------------------------------
!
IIU=SIZE(PWT,1)
IJU=SIZE(PWT,2)
IKU=SIZE(PWT,3)

if ( lbudget_u  .and. ovisc_uvw ) call Budget_store_init( tbudgets(NBUDGET_U ), 'VISC', prus (:, :, :)    )
if ( lbudget_v  .and. ovisc_uvw ) call Budget_store_init( tbudgets(NBUDGET_V ), 'VISC', prvs (:, :, :)    )
if ( lbudget_w  .and. ovisc_uvw ) call Budget_store_init( tbudgets(NBUDGET_W ), 'VISC', prws (:, :, :)    )
if ( lbudget_th .and. ovisc_th  ) call Budget_store_init( tbudgets(NBUDGET_TH), 'VISC', prths(:, :, :)    )
if ( lbudget_rv .and. ovisc_r   ) call Budget_store_init( tbudgets(NBUDGET_RV), 'VISC', prrs (:, :, :, 1) )
if ( lbudget_rc .and. ovisc_r   ) call Budget_store_init( tbudgets(NBUDGET_RC), 'VISC', prrs (:, :, :, 2) )
if ( lbudget_rr .and. ovisc_r   ) call Budget_store_init( tbudgets(NBUDGET_RR), 'VISC', prrs (:, :, :, 3) )
if ( lbudget_ri .and. ovisc_r   ) call Budget_store_init( tbudgets(NBUDGET_RI), 'VISC', prrs (:, :, :, 4) )
if ( lbudget_rs .and. ovisc_r   ) call Budget_store_init( tbudgets(NBUDGET_RS), 'VISC', prrs (:, :, :, 5) )
if ( lbudget_rg .and. ovisc_r   ) call Budget_store_init( tbudgets(NBUDGET_RG), 'VISC', prrs (:, :, :, 6) )
if ( lbudget_rh .and. ovisc_r   ) call Budget_store_init( tbudgets(NBUDGET_RH), 'VISC', prrs (:, :, :, 7) )
if ( lbudget_sv .and. ovisc_sv  ) then
  do ik = 1, ksv
    call Budget_store_init( tbudgets(NBUDGET_SV1 - 1 + ik), 'VISC', prsvs(:, :, :, ik) )
  end do
end if

!*       1.    Viscous forcing for potential temperature
!	       -----------------------------------------
!
!
IF (OVISC_TH) THEN
!
!
      PRTHS = PRTHS + PNU/PPRANDTL * &
              LAP_M(HLBCX,HLBCY,PDXX,PDYY,PDZX,PDZY,PDZZ,PRHODJ,PTHT)
!
!
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.    Viscous forcing for moisture
!	       ----------------------------
!
IF (OVISC_R .AND. (SIZE(PRT,1) > 0)) THEN
!
!
     DO IK = 1, KRR
        PRRS(:,:,:,IK) = PRRS(:,:,:,IK) + PNU/PPRANDTL * &
             LAP_M(HLBCX,HLBCY,PDXX,PDYY,PDZX,PDZY,PDZZ,PRHODJ,PRT(:,:,:,IK))
     END DO
!
!
END IF
!
!-------------------------------------------------------------------------------
!
!*       3.    Viscous forcing for passive scalars
!	       -----------------------------------
!
IF (OVISC_SV .AND. (SIZE(PSVT,1) > 0)) THEN
!
!
      DO IK = 1, KSV
         PRSVS(:,:,:,IK) = PRSVS(:,:,:,IK) + PNU/PPRANDTL * &
              LAP_M(HLBCX,HLBCY,PDXX,PDYY,PDZX,PDZY,PDZZ,PRHODJ,PSVT(:,:,:,IK))
      END DO
!
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.    Viscous forcing for momentum
!	       ----------------------------
!
IF (OVISC_UVW) THEN
!
!*       4.1   U - component
!
!
      ZY1 = MXF(PUT)
      IF (ODRAG) THEN
         ZY1(:,:,1) = PDRAG * ZY1(:,:,2)
!!Add adhesion condition in case of an IBM-obstacle at the domain top boundary
!         ZY1(:,:,IKU) = PDRAG * ZY1(:,:,IKE)
      ENDIF
!
! 
       ZLAPu = LAP_M(HLBCX,HLBCY,PDXX,PDYY,PDZX,   &
                   PDZY,PDZZ,PRHODJ,ZY1)
!! Update halo to compute the source term
 NULLIFY(TZFIELDS_ll)
 CALL ADD3DFIELD_ll( TZFIELDS_ll, ZLAPu, 'VISCOSITY::ZLAPu' )
 CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
 CALL CLEANLIST_ll(TZFIELDS_ll)
!
 PRUS = PRUS + MXM(PNU*ZLAPu)
!
!*       4.2   V - component
!              -------------
!
  IF (.NOT. L2D) THEN

      ZY2 = MYF(PVT) 
      IF (ODRAG) THEN
        ZY2(:,:,1) = PDRAG * ZY2(:,:,2)
      ENDIF
!
      ZLAPv =  LAP_M(HLBCX,HLBCY,PDXX,PDYY,PDZX,   &
                     PDZY,PDZZ,PRHODJ,ZY2)
!! Update halo to compute the source term
!
 NULLIFY(TZFIELDS_ll)
 CALL ADD3DFIELD_ll( TZFIELDS_ll, ZLAPv, 'VISCOSITY::ZLAPv' )
 CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
 CALL CLEANLIST_ll(TZFIELDS_ll)
!
 PRVS = PRVS + MYM(PNU*ZLAPv)

ENDIF 

!
!*       4.3   W - component
!              -------------
!
   IKB = JPVEXT + 1
   IKE = SIZE(PWT,3) - JPVEXT

   ZTMP = MZF(PWT)
!
   IF (ODRAG) THEN
         WHERE (PDRAG==-1)
         ZTMP(:,:,IKB) = 0.
         ENDWHERE
   ENDIF
!
   DO IK = 1,JPVEXT
      ZTMP(:,:,IK) = ZTMP(:,:,IKB)
      ZTMP(:,:,IKE+IK) = ZTMP(:,:,IKE)
   END DO
!
   ZTMP = MZM( PNU * &
          LAP_M(HLBCX,HLBCY,PDXX,PDYY,PDZX,PDZY,PDZZ,PRHODJ,ZTMP) )
!
   DO IK = 1,JPVEXT
      ZTMP(:,:,IK) = ZTMP(:,:,IKB)
      ZTMP(:,:,IKE+IK) = ZTMP(:,:,IKE) 
   END DO
   PRWS = PRWS + ZTMP
!
!!! Debug provisoire dans le cas ou le noslip est applique jusqu'au bord de
!sortie de flux en OPEN
  IF ( LWEST_ll().AND.(ODRAG)) THEN
    IF ( MINVAL(PDRAG(IIU,:))== -1) THEN
              DO JK=1,IKU
                WHERE(PDRAG(IIU,:)== -1)
            PRUS(IIU,:,JK) = PRUS(IIU-1,:,JK)
            PRVS(IIU,:,JK) = PRVS(IIU-1,:,JK)
            PRWS(IIU,:,JK) = PRWS(IIU-1,:,JK)
                ENDWHERE
            END DO
   ENDIF
  ENDIF
END IF

if ( lbudget_u  .and. ovisc_uvw ) call Budget_store_end( tbudgets(NBUDGET_U ), 'VISC', prus (:, :, :)    )
if ( lbudget_v  .and. ovisc_uvw ) call Budget_store_end( tbudgets(NBUDGET_V ), 'VISC', prvs (:, :, :)    )
if ( lbudget_w  .and. ovisc_uvw ) call Budget_store_end( tbudgets(NBUDGET_W ), 'VISC', prws (:, :, :)    )
if ( lbudget_th .and. ovisc_th  ) call Budget_store_end( tbudgets(NBUDGET_TH), 'VISC', prths(:, :, :)    )
if ( lbudget_rv .and. ovisc_r   ) call Budget_store_end( tbudgets(NBUDGET_RV), 'VISC', prrs (:, :, :, 1) )
if ( lbudget_rc .and. ovisc_r   ) call Budget_store_end( tbudgets(NBUDGET_RC), 'VISC', prrs (:, :, :, 2) )
if ( lbudget_rr .and. ovisc_r   ) call Budget_store_end( tbudgets(NBUDGET_RR), 'VISC', prrs (:, :, :, 3) )
if ( lbudget_ri .and. ovisc_r   ) call Budget_store_end( tbudgets(NBUDGET_RI), 'VISC', prrs (:, :, :, 4) )
if ( lbudget_rs .and. ovisc_r   ) call Budget_store_end( tbudgets(NBUDGET_RS), 'VISC', prrs (:, :, :, 5) )
if ( lbudget_rg .and. ovisc_r   ) call Budget_store_end( tbudgets(NBUDGET_RG), 'VISC', prrs (:, :, :, 6) )
if ( lbudget_rh .and. ovisc_r   ) call Budget_store_end( tbudgets(NBUDGET_RH), 'VISC', prrs (:, :, :, 7) )
if ( lbudget_sv .and. ovisc_sv  ) then
  do ik = 1, ksv
    call Budget_store_end( tbudgets(NBUDGET_SV1 - 1 + ik), 'VISC', prsvs(:, :, :, ik) )
  end do
end if

END SUBROUTINE VISCOSITY
