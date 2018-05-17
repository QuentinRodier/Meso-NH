!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!    #######################
     MODULE MODI_TURB_HOR_UV
!    #######################
!
INTERFACE  
!
      SUBROUTINE TURB_HOR_UV(KSPLT,                                  &
                      OCLOSE_OUT,OTURB_FLX,                          &
                      TPFILE,                                        &
                      PK,PINV_PDXX,PINV_PDYY,PINV_PDZZ,PMZM_PRHODJ,  &
                      PDXX,PDYY,PDZZ,PDZX,PDZY,                      &
                      PDIRCOSZW,                                     &
                      PCOSSLOPE,PSINSLOPE,                           &
                      PRHODJ,                                        &
                      PCDUEFF,PTAU11M,PTAU12M,PTAU22M,PTAU33M,       &
                      PUM,PVM,PUSLOPEM,PVSLOPEM,                     &
                      PDP,                                           &
                      PRUS,PRVS                                      )
!
USE MODD_IO_ll, ONLY: TFILEDATA
!
INTEGER,                  INTENT(IN)    ::  KSPLT        ! split process index
LOGICAL,                  INTENT(IN)    ::  OCLOSE_OUT   ! switch for syncronous
                                                         ! file opening       
LOGICAL,                  INTENT(IN)    ::  OTURB_FLX    ! switch to write the
                                 ! turbulent fluxes in the syncronous FM-file
TYPE(TFILEDATA),          INTENT(IN)    ::  TPFILE       ! Output file
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PK          ! Turbulent diffusion doef.
                                                        ! PK = PLM * SQRT(PTKEM)
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PINV_PDXX   ! 1./PDXX
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PINV_PDYY   ! 1./PDYY
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PINV_PDZZ   ! 1./PDZZ
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PMZM_PRHODJ ! MZM(PRHODJ)
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PDXX, PDYY, PDZZ, PDZX, PDZY 
                                                         ! Metric coefficients
REAL, DIMENSION(:,:),     INTENT(IN)    ::  PDIRCOSZW
! Director Cosinus along z directions at surface w-point
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PCOSSLOPE       ! cosinus of the angle 
                                      ! between i and the slope vector
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PSINSLOPE       ! sinus of the angle 
                                      ! between i and the slope vector
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PRHODJ       ! density * grid volume
!
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PCDUEFF      ! Cd * || u || at time t
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTAU11M      ! <uu> in the axes linked 
       ! to the maximum slope direction and the surface normal and the binormal 
       ! at time t - dt
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTAU12M      ! <uv> in the same axes
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTAU22M      ! <vv> in the same axes
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTAU33M      ! <ww> in the same axes
!
! Variables at t-1
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PUM,PVM
REAL, DIMENSION(:,:),      INTENT(IN)   ::  PUSLOPEM     ! wind component along the 
                                     ! maximum slope direction
REAL, DIMENSION(:,:),      INTENT(IN)   ::  PVSLOPEM     ! wind component along the 
                                     ! direction normal to the maximum slope one
!
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PRUS, PRVS   ! var. at t+1 -split-
REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PDP          ! TKE production terms
!
!
END SUBROUTINE TURB_HOR_UV
!
END INTERFACE
!
END MODULE MODI_TURB_HOR_UV
!     ################################################################
      SUBROUTINE TURB_HOR_UV(KSPLT,                                  &
                      OCLOSE_OUT,OTURB_FLX,                          &
                      TPFILE,                                        &
                      PK,PINV_PDXX,PINV_PDYY,PINV_PDZZ,PMZM_PRHODJ,  &
                      PDXX,PDYY,PDZZ,PDZX,PDZY,                      &
                      PDIRCOSZW,                                     &
                      PCOSSLOPE,PSINSLOPE,                           &
                      PRHODJ,                                        &
                      PCDUEFF,PTAU11M,PTAU12M,PTAU22M,PTAU33M,       &
                      PUM,PVM,PUSLOPEM,PVSLOPEM,                     &
                      PDP,                                           &
                      PRUS,PRVS                                      )
!     ################################################################
!
!
!!****  *TURB_HOR* -routine to compute the source terms in the meso-NH
!!               model equations due to the non-vertical turbulent fluxes.
!!
!!    PURPOSE
!!    -------
!!
!!     see TURB_HOR
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!      Joan Cuxart             * INM and Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!                     Aug    , 1997 (V. Saravane) spliting of TURB_HOR
!!                     Nov  27, 1997 (V. Masson) clearing of the routine
!!                     Oct  18, 2000 (V. Masson) LES computations + LFLAT switch
!!                     Nov  06, 2002 (V. Masson) LES budgets
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CST
USE MODD_CONF
USE MODD_CTURB
USE MODD_IO_ll, ONLY: TFILEDATA
USE MODD_PARAMETERS
USE MODD_LES
!
USE MODE_FIELD, ONLY: TFIELDDATA, TYPEREAL
USE MODE_FMWRIT
!
USE MODI_GRADIENT_M
USE MODI_GRADIENT_U
USE MODI_GRADIENT_V
USE MODI_GRADIENT_W
USE MODI_SHUMAN 
USE MODI_COEFJ
USE MODI_LES_MEAN_SUBGRID
!
USE MODI_SECOND_MNH
!
IMPLICIT NONE
!
!
!*       0.1  declaration of arguments
!
!
!
INTEGER,                  INTENT(IN)    ::  KSPLT        ! split process index
LOGICAL,                  INTENT(IN)    ::  OCLOSE_OUT   ! switch for syncronous
                                                         ! file opening       
LOGICAL,                  INTENT(IN)    ::  OTURB_FLX    ! switch to write the
                                 ! turbulent fluxes in the syncronous FM-file
TYPE(TFILEDATA),          INTENT(IN)    ::  TPFILE       ! Output file
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PK          ! Turbulent diffusion doef.
                                                        ! PK = PLM * SQRT(PTKEM)
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PINV_PDXX   ! 1./PDXX
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PINV_PDYY   ! 1./PDYY
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PINV_PDZZ   ! 1./PDZZ
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PMZM_PRHODJ ! MZM(PRHODJ)
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PDXX, PDYY, PDZZ, PDZX, PDZY 
                                                         ! Metric coefficients
REAL, DIMENSION(:,:),     INTENT(IN)    ::  PDIRCOSZW
! Director Cosinus along z directions at surface w-point
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PCOSSLOPE       ! cosinus of the angle 
                                      ! between i and the slope vector
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PSINSLOPE       ! sinus of the angle 
                                      ! between i and the slope vector
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PRHODJ       ! density * grid volume
!
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PCDUEFF      ! Cd * || u || at time t
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTAU11M      ! <uu> in the axes linked 
       ! to the maximum slope direction and the surface normal and the binormal 
       ! at time t - dt
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTAU12M      ! <uv> in the same axes
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTAU22M      ! <vv> in the same axes
REAL, DIMENSION(:,:),   INTENT(IN)   ::  PTAU33M      ! <ww> in the same axes
!
! Variables at t-1
REAL, DIMENSION(:,:,:),   INTENT(IN)    ::  PUM,PVM
REAL, DIMENSION(:,:),      INTENT(IN)   ::  PUSLOPEM     ! wind component along the 
                                     ! maximum slope direction
REAL, DIMENSION(:,:),      INTENT(IN)   ::  PVSLOPEM     ! wind component along the 
                                     ! direction normal to the maximum slope one
!
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PRUS, PRVS   ! var. at t+1 -split-
REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PDP          ! TKE production terms
!
!
!
!*       0.2  declaration of local variables
!
REAL, DIMENSION(SIZE(PUM,1),SIZE(PUM,2),SIZE(PUM,3))       &
                                     :: ZFLX,ZWORK
    ! work arrays
!   
REAL, DIMENSION(SIZE(PUM,1),SIZE(PUM,2)) ::ZDIRSINZW 
      ! sinus of the angle between the vertical and the normal to the orography
INTEGER             :: IKB,IKE,IKU
                                    ! Index values for the Beginning and End
                                    ! mass points of the domain  
!
REAL, DIMENSION(SIZE(PUM,1),SIZE(PUM,2),SIZE(PUM,3))  :: GY_U_UV_PUM
REAL, DIMENSION(SIZE(PVM,1),SIZE(PVM,2),SIZE(PVM,3))  :: GX_V_UV_PVM
!
REAL :: ZTIME1, ZTIME2
TYPE(TFIELDDATA) :: TZFIELD
! ---------------------------------------------------------------------------
!
!*       1.   PRELIMINARY COMPUTATIONS
!             ------------------------
!
IKB = 1+JPVEXT               
IKE = SIZE(PUM,3)-JPVEXT    
IKU = SIZE(PUM,3)
!
ZDIRSINZW(:,:) = SQRT( 1. - PDIRCOSZW(:,:)**2 )
!
GX_V_UV_PVM = GX_V_UV(1,IKU,1,PVM,PDXX,PDZZ,PDZX)
IF (.NOT. L2D) GY_U_UV_PUM = GY_U_UV(1,IKU,1,PUM,PDYY,PDZZ,PDZY)
!
!
!*      12.   < U'V'>
!             -------
!
!
IF (.NOT. L2D) THEN
  ZFLX(:,:,:)= - XCMFS * MYM(MXM(PK)) *                           &
          (GY_U_UV_PUM + GX_V_UV_PVM)
ELSE
  ZFLX(:,:,:)= - XCMFS * MYM(MXM(PK)) *                           &
          (GX_V_UV_PVM)
END IF
!
ZFLX(:,:,IKE+1)= ZFLX(:,:,IKE)
!
!
! Compute the correlation at the first physical level with the following 
! hypothesis du/dz vary in 1/z and w=0 at the ground
ZFLX(:,:,IKB:IKB)   = - XCMFS * MYM(MXM(PK(:,:,IKB:IKB))) *  (     &
  ( DYM( PUM(:,:,IKB:IKB) )                                        &
   -MYM( (PUM(:,:,IKB+1:IKB+1)-PUM(:,:,IKB:IKB))                   &
        *(1./MXM(PDZZ(:,:,IKB+1:IKB+1))+1./MXM(PDZZ(:,:,IKB:IKB))))&
    *0.5*MXM((PDZY(:,:,IKB+1:IKB+1)+PDZY(:,:,IKB:IKB)))            &
  ) / MXM(PDYY(:,:,IKB:IKB))                                       &
 +( DXM( PVM(:,:,IKB:IKB) )                                        &
   -MXM( (PVM(:,:,IKB+1:IKB+1)-PVM(:,:,IKB:IKB))                   &
        *(1./MYM(PDZZ(:,:,IKB+1:IKB+1))+1./MYM(PDZZ(:,:,IKB:IKB))))&
    *0.5*MYM((PDZX(:,:,IKB+1:IKB+1)+PDZX(:,:,IKB:IKB)))            &
  ) / MYM(PDXX(:,:,IKB:IKB))                                 ) 
! 
! extrapolates this flux under the ground with the surface flux
ZFLX(:,:,IKB-1) =                                                           &
   PTAU11M(:,:) * PCOSSLOPE(:,:) * PSINSLOPE(:,:) * PDIRCOSZW(:,:)**2         &
  +PTAU12M(:,:) * (PCOSSLOPE(:,:)**2 - PSINSLOPE(:,:)**2) *                   &
                  PDIRCOSZW(:,:)**2                                           &
  -PTAU22M(:,:) * PCOSSLOPE(:,:) * PSINSLOPE(:,:)                             &
  +PTAU33M(:,:) * PCOSSLOPE(:,:) * PSINSLOPE(:,:) * ZDIRSINZW(:,:)**2         &
  -PCDUEFF(:,:) * (                                                           &
    2. * PUSLOPEM(:,:) * PCOSSLOPE(:,:) * PSINSLOPE(:,:) *                    &
          PDIRCOSZW(:,:) * ZDIRSINZW(:,:)                                     &
    +PVSLOPEM(:,:) * (PCOSSLOPE(:,:)**2 - PSINSLOPE(:,:)**2) * ZDIRSINZW(:,:) &
                   )
!  
ZFLX(:,:,IKB-1:IKB-1) = 2. * MXM( MYM( ZFLX(:,:,IKB-1:IKB-1) ) )  &
                   - ZFLX(:,:,IKB:IKB)
!     
! stores  <U V>
IF ( OCLOSE_OUT .AND. OTURB_FLX ) THEN
  TZFIELD%CMNHNAME   = 'UV_FLX'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'UV_FLX'
  TZFIELD%CUNITS     = 'm2 s-2'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = 'X_Y_Z_UV_FLX'
  TZFIELD%NGRID      = 5
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZFLX)
END IF
!
!
!
!computation of the source for rho*V due to this flux
IF (.NOT. LFLAT) THEN
  PRUS(:,:,:) = PRUS(:,:,:)                                &
              - DYF(ZFLX * MXM(MYM(PRHODJ) * PINV_PDYY) )         &
              + DZF(1,IKU,1, MYF( MZM(1,IKU,1,ZFLX)*MXM(PDZY/MZM(1,IKU,1,PDYY)))   &
                    * MXM(PMZM_PRHODJ * PINV_PDZZ) )
ELSE
  PRUS(:,:,:) = PRUS(:,:,:) - DYF(ZFLX * MXM(MYM(PRHODJ) * PINV_PDYY) )
END IF
!
!computation of the source for rho*V due to this flux
IF (.NOT. LFLAT) THEN
  PRVS(:,:,:) = PRVS(:,:,:)                             &
                - DXF(ZFLX * MYM(MXM(PRHODJ) * PINV_PDXX) )    &
                + DZF(1,IKU,1, MXF( MZM(1,IKU,1,ZFLX)*MYM(PDZX/MZM(1,IKU,1,PDXX))) & 
                      * MYM(PMZM_PRHODJ * PINV_PDZZ) )
ELSE
  PRVS(:,:,:) = PRVS(:,:,:) - DXF(ZFLX * MYM(MXM(PRHODJ) * PINV_PDXX) )
END IF
!
IF (KSPLT==1) THEN
  !
  !Contribution to the dynamic production of TKE:
  !
  IF (.NOT. L2D) THEN
    ZWORK(:,:,:) = - MXF( MYF( ZFLX *                                &
       (GY_U_UV_PUM + GX_V_UV_PVM) ) ) 
  ELSE
    ZWORK(:,:,:) = - MXF( MYF( ZFLX *                                &
       (GX_V_UV_PVM) ) )  
  ENDIF
  !
  ! evaluate the dynamic production at w(IKB+1) in PDP(IKB)
  !
  ZWORK(:,:,IKB:IKB) =  -                                             &
     MXF ( MYF( 0.5 * (ZFLX(:,:,IKB+1:IKB+1)+ZFLX(:,:,IKB:IKB)) ) )   &
   *(MXF ( MYF(                                                       &
       DYM( 0.5 * (PUM(:,:,IKB+1:IKB+1)+PUM(:,:,IKB:IKB))  )          &
      / MXM( 0.5*(PDYY(:,:,IKB:IKB)+PDYY(:,:,IKB+1:IKB+1)) )          &
      +DXM( 0.5 * (PVM(:,:,IKB+1:IKB+1)+PVM(:,:,IKB:IKB))  )          &
      / MYM( 0.5*(PDXX(:,:,IKB:IKB)+PDXX(:,:,IKB+1:IKB+1)) )          &
         )    )                                                       &  
    -MXF( (PUM(:,:,IKB+1:IKB+1)-PUM(:,:,IKB:IKB)) /                   &
              MXM(PDZZ(:,:,IKB+1:IKB+1)) * PDZY(:,:,IKB+1:IKB+1)      &
        ) / MYF(MXM( 0.5*(PDYY(:,:,IKB:IKB)+PDYY(:,:,IKB+1:IKB+1)) ) )&
    -MYF( (PVM(:,:,IKB+1:IKB+1)-PVM(:,:,IKB:IKB)) /                   &
              MYM(PDZZ(:,:,IKB+1:IKB+1)) * PDZX(:,:,IKB+1:IKB+1)      &
        ) / MXF(MYM( 0.5*(PDXX(:,:,IKB:IKB)+PDXX(:,:,IKB+1:IKB+1)) ) )&
    ) 
  !
  ! dynamic production 
  PDP(:,:,:) = PDP(:,:,:) + ZWORK(:,:,:)
  ! 
END IF
!
! Storage in the LES configuration
!
IF (LLES_CALL .AND. KSPLT==1) THEN
  CALL SECOND_MNH(ZTIME1)
  CALL LES_MEAN_SUBGRID( MXF(MYF(ZFLX)), X_LES_SUBGRID_UV ) 
  CALL LES_MEAN_SUBGRID( MXF(MYF(GY_U_UV(1,IKU,1,PUM,PDYY,PDZZ,PDZY)*ZFLX)), X_LES_RES_ddxa_U_SBG_UaU , .TRUE.)
  CALL LES_MEAN_SUBGRID( MXF(MYF(GX_V_UV(1,IKU,1,PVM,PDXX,PDZZ,PDZX)*ZFLX)), X_LES_RES_ddxa_V_SBG_UaV , .TRUE.)
  CALL SECOND_MNH(ZTIME2)
  XTIME_LES = XTIME_LES + ZTIME2 - ZTIME1
END IF
!
!
END SUBROUTINE TURB_HOR_UV
