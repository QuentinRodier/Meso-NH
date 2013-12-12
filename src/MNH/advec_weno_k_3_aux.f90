!     ##############################
      MODULE MODI_ADVEC_WENO_K_3_AUX
!     ##############################
!
INTERFACE
!
      SUBROUTINE ADVEC_WENO_K_3_UX(HLBCX,PSRC, PRUCT, PR, TPHALO2)
!
USE MODE_ll
USE MODD_LUNIT
USE MODD_CONF
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX  ! X direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on U grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRUCT ! contrav. comp. on MASS GRID
!
! output source term
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PR
TYPE(HALO2_ll), OPTIONAL, POINTER :: TPHALO2      ! halo2 for the field at t
!
END SUBROUTINE ADVEC_WENO_K_3_UX
!
!                    ----------------------------
!
      SUBROUTINE ADVEC_WENO_K_3_MX(HLBCX,PSRC, PRUCT, PR, TPHALO2)
!
USE MODE_ll
USE MODD_LUNIT
USE MODD_CONF
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX  ! X direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on U grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRUCT ! contrav. comp. on MASS GRID
!
! output source term
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PR
TYPE(HALO2_ll), OPTIONAL, POINTER :: TPHALO2      ! halo2 for the field at t
!
END SUBROUTINE ADVEC_WENO_K_3_MX
!
!                     ---------------------------
!
      SUBROUTINE ADVEC_WENO_K_3_VY(HLBCY,PSRC, PRVCT, PR, TPHALO2)
!
USE MODE_ll
USE MODD_LUNIT
USE MODD_CONF
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY  ! Y direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on U grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRVCT ! contrav. comp. on MASS GRID
!
!
! output source term
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PR
TYPE(HALO2_ll), OPTIONAL, POINTER :: TPHALO2      ! halo2 for the field at t
!
END SUBROUTINE ADVEC_WENO_K_3_VY
!
!                  ------------------------------
!
      SUBROUTINE ADVEC_WENO_K_3_MY(HLBCY,PSRC, PRVCT, PR, TPHALO2)
!
USE MODE_ll
USE MODD_LUNIT
USE MODD_CONF
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY  ! Y direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on U grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRVCT ! contrav. comp. on MASS GRID
!
! output source term
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PR
TYPE(HALO2_ll), OPTIONAL, POINTER :: TPHALO2      ! halo2 for the field at t
!
END SUBROUTINE ADVEC_WENO_K_3_MY
!
!                     -------------------------------
!
FUNCTION WENO_K_3_WZ(PSRC, PRWCT) RESULT(PR)
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on W grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRWCT ! contrav. comp. on MASS GRID
!
! output source term
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: PR
!
END FUNCTION WENO_K_3_WZ
!
!                      ------------------------------
!
FUNCTION WENO_K_3_MZ(PSRC, PRWCT) RESULT(PR)
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on MASS grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRWCT ! contrav. comp. on W grid
!
! output source term
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: PR
!
END FUNCTION WENO_K_3_MZ
!
END INTERFACE
!
END MODULE MODI_ADVEC_WENO_K_3_AUX
!
!-----------------------------------------------------------------------------
!
!     ############################################################
      SUBROUTINE ADVEC_WENO_K_3_UX(HLBCX,PSRC, PRUCT, PR, TPHALO2)
!     ############################################################
!!
!!**** Computes PRUCT * PUT. Upstream fluxes of U in X direction.  
!!     Input PUT is on U Grid 'ie' (i,j,k) based on UGRID reference
!!     Output PR is on mass Grid 'ie' (i+1/2,j,k) based on UGRID reference
!!              
!!    AUTHOR
!!    ------
!!    F. Visentin   *CNRS/LA*               
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
USE MODE_ll
USE MODD_LUNIT
USE MODD_CONF
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX  ! X direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on U grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRUCT ! contrav. comp. on MASS GRID
!
! output source term
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PR
TYPE(HALO2_ll), OPTIONAL, POINTER :: TPHALO2      ! halo2 for the field at t
!
!*       0.2   Declarations of local variables :
!
INTEGER :: IIB,IJB    ! Begining useful area in x,y,z directions
INTEGER :: IIE,IJE    ! End useful area in x,y,z directions
INTEGER:: IW,IE,IWF,IEF   ! Coordinate of third order diffusion area
!
INTEGER:: ILUOUT,IRESP   ! for prints
!
! intermediate reconstruction fluxes for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFPOS1, ZFPOS2, ZFPOS3
!
! intermediate reconstruction fluxes for negative wind case
! we need only one since ZFNEG2 = ZFPOS2
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFNEG1, ZFNEG2, ZFNEG3
!
! smoothness indicators for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBPOS1, ZBPOS2, ZBPOS3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBNEG1, ZBNEG2, ZBNEG3
!
! WENO weights
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: ZOMP1, ZOMP2, ZOMP3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: ZOMN1, ZOMN2, ZOMN3
!
! standard weights
!
REAL, PARAMETER :: ZGAMMA1 = 1./10.
REAL, PARAMETER :: ZGAMMA2 = 3./5.
REAL, PARAMETER :: ZGAMMA3 = 3./10.
REAL, PARAMETER :: ZGAMMA1_PRIM = 1./3.
REAL, PARAMETER :: ZGAMMA2_PRIM = 2./3.
!
REAL, PARAMETER :: ZEPS = 1.0E-15
!
!-----------------------------------------------------------------------------
!*       0.3.     COMPUTES THE DOMAIN DIMENSIONS
!                 ------------------------------
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
!-------------------------------------------------------------------------------
!
!*       0.4.   INITIALIZE THE FIELD 
!               ---------------------
!
PR(:,:,:) = 0.0
!
ZFPOS1  = 0.0
ZFPOS2  = 0.0
ZFPOS3  = 0.0
ZFNEG1  = 0.0
ZFNEG2  = 0.0
ZFNEG3  = 0.0
ZBPOS1  = 0.0
ZBPOS2  = 0.0
ZBPOS3  = 0.0
ZBNEG1  = 0.0
ZBNEG2  = 0.0
ZBNEG3  = 0.0
ZOMP1   = 0.0
ZOMP2   = 0.0
ZOMP3   = 0.0
ZOMN1   = 0.0
ZOMN2   = 0.0
ZOMN3   = 0.0 
!
!-------------------------------------------------------------------------------
!
SELECT CASE ( HLBCX(1) ) ! X direction LBC type: (1) for left side
!
!*       1.1    CYCLIC CASE IN THE X DIRECTION:
!
CASE ('CYCL')          ! In that case one must have HLBCX(1) == HLBCX(2)
!
  IF(NHALO == 1) THEN
    IW=IIB
    IE=IIE
  ELSE
    CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT,IRESP)
    WRITE(ILUOUT,*) 'ERROR : 3rd order advection in CYCLic case '
    WRITE(ILUOUT,*) 'cannot be used with NHALO=2'
    CALL ABORT
    STOP
  END IF
!
! r: many left cells in regard to 'i' cell for each stencil
!
! intermediate fluxes at the mass point on Ugrid u(i+1/2,j,k) for positive wind
! case (left to the right)
! (r=2 for the first stencil ZFPOS1, r=1 for the second ZFPOS2 and
!  r=0 for the last ZFPOS3)
!
  ZFPOS1(IW+1:IE-1,:,:) = 1./6 * (2.0*PSRC(IW-1:IE-3,:,:) - &
                    7.0*PSRC(IW:IE-2,:,:) + 11.0*PSRC(IW+1:IE-1,:,:))
  ZFPOS1(IW,       :,:) = 1./6 * (2.0*TPHALO2%WEST(:,:)   - &
                    7.0*PSRC(IW-1,   :,:) + 11.0*PSRC(IW,       :,:))
  ZFPOS1(IW-1,     :,:) = 0.5  * (3.0*PSRC(IW-1     ,:,:) - TPHALO2%WEST(:,:))
  ZFPOS1(IE,       :,:) = 0.5  * (3.0*PSRC(IE       ,:,:) - PSRC(IE-1,   :,:))
  ZFPOS1(IE+1,     :,:) = 0.5  * (3.0*PSRC(IE+1     ,:,:) - PSRC(IE,     :,:))
!
!
  ZFPOS2(IW:IE-1,:,:) = 1./6 * (-1.0*PSRC(IW-1:IE-2,:,:) + 5.0*PSRC(IW:IE-1,:,:) + 2.0*PSRC(IW+1:IE,:,:))
  ZFPOS2(IW-1,   :,:) = 0.5 * (PSRC(IW-1     ,:,:) + PSRC(IW,     :,:))
  ZFPOS2(IE,     :,:) = 0.5 * (PSRC(IE       ,:,:) + PSRC(IE+1,   :,:))
  ZFPOS2(IE+1,   :,:) = 0.5 * (PSRC(IE+1     ,:,:) + TPHALO2%EAST(:,:))
!
  ZFPOS3(IW:IE-1,:,:) = 1./6 * (2.0*PSRC(IW:IE-1,:,:) + 5.0*PSRC(IW+1:IE,:,:) &
                        - PSRC(IW+2:IE+1,:,:))
!
!
! r: many left cells in regard to 'i+1' cell for each stencil
! 
! intermediate flux at the mass point on Ugrid (i+1/2,j,k)=((i+1)-1/2,j,k) for 
! negative wind case (right to the left)
! (r=2 for the last stencil ZFNEG3=ZFPOS2, r=1 for the second ZFNEG2=ZFPOS3 
!  and r=0 for the first ZFNEG1)  
!
  ZFNEG1(IW:IE-2,:,:) = 1./6 * (11.0*PSRC(IW+1:IE-1,:,:) - &
                        7.0*PSRC(IW+2:IE,:,:) + 2.0*PSRC(IW+3:IE+1,:,:))
  ZFNEG1(IE-1,   :,:) = 1./6 * (11.0*PSRC(IE,       :,:) - &
                        7.0*PSRC(IE+1,   :,:) + 2.0*TPHALO2%EAST(:,:))
  ZFNEG1(IE,  :,:) = 0.5 * (3.0*PSRC(IE+1,:,:) - TPHALO2%EAST(:,:))
  ZFNEG1(IE+1,:,:) = - 999.
  ZFNEG1(IW-1,:,:) = 0.5 * (3.0*PSRC(IW,  :,:) - PSRC(IW+1,   :,:))
!
! 
  ZFNEG2(IW:IE-1,:,:) = 1./6 * (2.0*PSRC(IW:IE-1,:,:) +    &
                        5.0*PSRC(IW+1:IE,:,:) - PSRC(IW+2:IE+1,:,:))
  ZFNEG2(IE,  :,:) = 0.5 * (PSRC(IE,   :,:) + PSRC(IE+1,:,:))
  ZFNEG2(IE+1,:,:) = 0.5 * (PSRC(IE+1, :,:) + TPHALO2%EAST(:,:))
  ZFNEG2(IW-1,:,:) = 0.5 * (PSRC(IW-1, :,:) + PSRC(IW,:,:))
!
!
  ZFNEG3(IW:IE-1,:,:) = 1./6 * (-1.0*PSRC(IW-1:IE-2,:,:) + &
                        5.0*PSRC(IW:IE-1,:,:) + 2.0*PSRC(IW+1:IE,:,:))
!
! smoothness indicators for positive wind case
!
  ZBPOS1(IW+1:IE-1,:,:) = 13./12 * (PSRC(IW-1:IE-3,:,:) - 2.0*PSRC(IW:IE-2,:,:)&
                       + PSRC(IW+1:IE-1,:,:))**2 + 1./4 * (PSRC(IW-1:IE-3,:,:) &
                       - 4.0*PSRC(IW:IE-2,:,:) + 3.0*PSRC(IW+1:IE-1,:,:))**2
  ZBPOS1(IW,       :,:) = 13./12 * (TPHALO2%WEST(:,:) - 2.0*PSRC(IW-1,:,:) + &
                          PSRC(IW,:,:))**2 + 1./4 * (TPHALO2%WEST(:,:) -     &
                          4.0*PSRC(IW-1,:,:) + 3.0*PSRC(IW,:,:))**2
  ZBPOS1(IW-1,   :,:) = (PSRC(IW-1,:,:) - TPHALO2%WEST(:,:))**2
  ZBPOS1(IE,     :,:) = (PSRC(IE  ,:,:) - PSRC(IE-1,   :,:))**2
  ZBPOS1(IE+1,   :,:) = (PSRC(IE+1,:,:) - PSRC(IE,     :,:))**2
!
!
  ZBPOS2(IW:IE-1,:,:) = 13./12 * (PSRC(IW-1:IE-2,:,:) - 2.0*PSRC(IW:IE-1,:,:) +&
   PSRC(IW+1:IE,:,:))**2 + 1./4 * (PSRC(IW-1:IE-2,:,:) - PSRC(IW+1:IE,:,:))**2
  ZBPOS2(IW-1,:,:) = (PSRC(IW,  :,:) - PSRC(IW-1,:,:))**2
  ZBPOS2(IE,  :,:) = (PSRC(IE+1,:,:) - PSRC(IE,  :,:))**2
  ZBPOS2(IE+1,:,:) = (TPHALO2%EAST(:,:) - PSRC(IE+1,:,:))**2
!
!
  ZBPOS3(IW:IE-1,:,:) = 13./12 * (PSRC(IW:IE-1,:,:) - 2.0*PSRC(IW+1:IE,:,:) + &
   PSRC(IW+2:IE+1,:,:))**2 + 1./4 * ( 3.0*PSRC(IW:IE-1,:,:) -                 &
   4.0*PSRC(IW+1:IE,:,:) + PSRC(IW+2:IE+1,:,:))**2
!
! smoothness indicators for negative wind case
!       
  ZBNEG1(IW:IE-2,:,:) = 13./12 * (PSRC(IW+1:IE-1,:,:) - 2.0*PSRC(IW+2:IE,:,:) +&
              PSRC(IW+3:IE+1,:,:))**2 + 1./4 * ( 3.0*PSRC(IW+1:IE-1,:,:) -     &
              4.0*PSRC(IW+2:IE,:,:) + PSRC(IW+3:IE+1,:,:))**2
  ZBNEG1(IE-1,   :,:) = 13./12 * (PSRC(IE,:,:) - 2.0*PSRC(IE+1,:,:) +          &
              TPHALO2%EAST(:,:))**2 + 1./4 * ( 3.0*PSRC(IE,:,:) -              &
              4.0*PSRC(IE+1,:,:) + TPHALO2%EAST(:,:))**2
  ZBNEG1(IE,   :,:) = (PSRC(IE+1,   :,:) - TPHALO2%EAST(:,:))**2
  ZBNEG1(IE+1, :,:) = - 999.
  ZBNEG1(IW-1, :,:) = (PSRC(IW,  :,:) - PSRC(IW+1,   :,:))**2
!
!
  ZBNEG2(IW:IE-1,:,:) = 13./12 * (PSRC(IW:IE-1,:,:) - 2.0*PSRC(IW+1:IE,:,:) + &
   PSRC(IW+2:IE+1,:,:))**2 + 1./4 * (PSRC(IW:IE-1,:,:) - PSRC(IW+2:IE+1,:,:))**2
  ZBNEG2(IW-1,:,:) = (PSRC(IW-1,:,:) - PSRC(IW,  :,:))**2
  ZBNEG2(IE  ,:,:) = (PSRC(IE,  :,:) - PSRC(IE+1,:,:))**2
  ZBNEG2(IE+1,:,:) = (PSRC(IE+1,:,:) - TPHALO2%EAST(:,:))**2
!
!
  ZBNEG3(IW:IE-1,:,:) = 13./12 * (PSRC(IW-1:IE-2,:,:) - 2.0*PSRC(IW:IE-1,:,:) +&
                        PSRC(IW+1:IE,:,:))**2 + 1./4 * ( PSRC(IW-1:IE-2,:,:) - &
                        4.0*PSRC(IW:IE-1,:,:) + 3.0*PSRC(IW+1:IE,:,:))**2
!
! WENO weights
!
  ZOMP1(IW:IE-1,:,:) = ZGAMMA1 / (ZEPS + ZBPOS1(IW:IE-1,:,:))**2
  ZOMP2(IW:IE-1,:,:) = ZGAMMA2 / (ZEPS + ZBPOS2(IW:IE-1,:,:))**2
  ZOMP3(IW:IE-1,:,:) = ZGAMMA3 / (ZEPS + ZBPOS3(IW:IE-1,:,:))**2
  ZOMN1(IW:IE-1,:,:) = ZGAMMA1 / (ZEPS + ZBNEG1(IW:IE-1,:,:))**2
  ZOMN2(IW:IE-1,:,:) = ZGAMMA2 / (ZEPS + ZBNEG2(IW:IE-1,:,:))**2
  ZOMN3(IW:IE-1,:,:) = ZGAMMA3 / (ZEPS + ZBNEG3(IW:IE-1,:,:))**2
!
  ZOMP1(IW-1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IW-1,:,:))**2
  ZOMP2(IW-1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IW-1,:,:))**2
  ZOMP1(IE,  :,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IE,  :,:))**2
  ZOMP2(IE,  :,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IE,  :,:))**2
  ZOMP1(IE+1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IE+1,:,:))**2
  ZOMP2(IE+1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IE+1,:,:))**2
  ZOMN1(IW-1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IW-1,:,:))**2
  ZOMN2(IW-1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IW-1,:,:))**2
  ZOMN1(IE,  :,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IE,  :,:))**2
  ZOMN2(IE,  :,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IE,  :,:))**2
  ZOMN1(IE+1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IE+1,:,:))**2
  ZOMN2(IE+1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IE+1,:,:))**2
!
! WENO fluxes (5th order)
!
  PR(IW:IE-1,:,:) = (ZOMP2(IW:IE-1,:,:)/(ZOMP1(IW:IE-1,:,:)+ZOMP2(IW:IE-1,:,:)+&
                     ZOMP3(IW:IE-1,:,:)) * ZFPOS2(IW:IE-1,:,:) &
                   + ZOMP1(IW:IE-1,:,:)/(ZOMP1(IW:IE-1,:,:)+ZOMP2(IW:IE-1,:,:)+&
                     ZOMP3(IW:IE-1,:,:)) * ZFPOS1(IW:IE-1,:,:) & 
                   + ZOMP3(IW:IE-1,:,:)/(ZOMP1(IW:IE-1,:,:)+ZOMP2(IW:IE-1,:,:)+&
                     ZOMP3(IW:IE-1,:,:)) * ZFPOS3(IW:IE-1,:,:))&
                   * (0.5+SIGN(0.5,PRUCT(IW:IE-1,:,:)))        &
                  + (ZOMN2(IW:IE-1,:,:)/(ZOMN1(IW:IE-1,:,:)+ZOMN2(IW:IE-1,:,:)+&
                     ZOMN3(IW:IE-1,:,:)) * ZFNEG2(IW:IE-1,:,:) &
                   + ZOMN1(IW:IE-1,:,:)/(ZOMN1(IW:IE-1,:,:)+ZOMN2(IW:IE-1,:,:)+&
                     ZOMN3(IW:IE-1,:,:)) * ZFNEG1(IW:IE-1,:,:) &
                   + ZOMN3(IW:IE-1,:,:)/(ZOMN1(IW:IE-1,:,:)+ZOMN2(IW:IE-1,:,:)+&
                     ZOMN3(IW:IE-1,:,:)) * ZFNEG3(IW:IE-1,:,:))&
                   * (0.5-SIGN(0.5,PRUCT(IW:IE-1,:,:)))
!
! WENO fluxes (3rd order)
!
  PR(IW-1,:,:) = (ZOMN2(IW-1,:,:)/(ZOMN1(IW-1,:,:)+ZOMN2(IW-1,:,:)) * & 
                  ZFNEG2(IW-1,:,:)     &
               + (ZOMN1(IW-1,:,:)/(ZOMN1(IW-1,:,:)+ZOMN2(IW-1,:,:)) * &
                  ZFNEG1(IW-1,:,:))) * &
               (0.5-SIGN(0.5,PRUCT(IW-1,:,:)))                        &
               + (ZOMP2(IW-1,:,:)/(ZOMP1(IW-1,:,:)+ZOMP2(IW-1,:,:)) * &
               ZFPOS2(IW-1,:,:)     &
               + (ZOMP1(IW-1,:,:)/(ZOMP1(IW-1,:,:)+ZOMP2(IW-1,:,:)) * &
               ZFPOS1(IW-1,:,:))) * &
               (0.5+SIGN(0.5,PRUCT(IW-1,:,:)))
!
  PR(IE,  :,:) = (ZOMN2(IE,  :,:)/(ZOMN1(IE,  :,:)+ZOMN2(IE,  :,:)) * &
               ZFNEG2(IE,  :,:)     &
               + (ZOMN1(IE,  :,:)/(ZOMN1(IE,  :,:)+ZOMN2(IE,  :,:)) * &
               ZFNEG1(IE,  :,:))) * &
               (0.5-SIGN(0.5,PRUCT(IE,  :,:)))                        &
               + (ZOMP2(IE,  :,:)/(ZOMP1(IE,  :,:)+ZOMP2(IE,  :,:)) * &
               ZFPOS2(IE,  :,:)     &
               + (ZOMP1(IE,  :,:)/(ZOMP1(IE,  :,:)+ZOMP2(IE,  :,:)) * &
               ZFPOS1(IE,  :,:))) * &
               (0.5+SIGN(0.5,PRUCT(IE,  :,:)))
!
  PR(IE+1,:,:) = (ZOMN2(IE+1,:,:)/(ZOMN1(IE+1,:,:)+ZOMN2(IE+1,:,:)) * &
               ZFNEG2(IE+1,:,:)     &
               + (ZOMN1(IE+1,:,:)/(ZOMN1(IE+1,:,:)+ZOMN2(IE+1,:,:)) * &
               ZFNEG1(IE+1,:,:))) * &
               (0.5-SIGN(0.5,PRUCT(IE+1,:,:)))                        &
               + (ZOMP2(IE+1,:,:)/(ZOMP1(IE+1,:,:)+ZOMP2(IE+1,:,:)) * &
               ZFPOS2(IE+1,:,:)     &
               + (ZOMP1(IE+1,:,:)/(ZOMP1(IE+1,:,:)+ZOMP2(IE+1,:,:)) * &
               ZFPOS1(IE+1,:,:))) * &
               (0.5+SIGN(0.5,PRUCT(IE+1,:,:)))
!
!
!       OPEN, WALL, NEST CASE IN THE X DIRECTION
!
CASE ('OPEN','WALL','NEST')
!
  IW=IIB
  IE=IIE
!
!  LATERAL BOUNDARY CONDITIONS
!  AT THE PHYSICAL BORDER: USE A FIRST ORDER UPSTREAM WENO SCHEME AT THE POINTS: IW-1, 
! IE /AND/ A THIRD ORDER WENO SCHEME AT THE POINTS: IW, IE-1
!  AT THE PROC. BORDER: A THIRD ORDER UPSTREAM WENO SCHEME AT THE POINTS: IW-1, IE  /AND/ 
! A FIFTH ORDER WENO SCHEME AT THE POINTS: IW, IE-1
!
!   PHYSICAL BORDER (WEST)
!
  IF(LWEST_ll()) THEN
!
!   FISRT ORDER WENO SCHEME
!
    PR(IW-1,:,:) = PSRC(IW-1,:,:) * (0.5+SIGN(0.5,PRUCT(IW-1,:,:))) + &
                   PSRC(IW,:,:) * &
                   (0.5-SIGN(0.5,PRUCT(IW-1,:,:)))
!
!   THIRD ORDER WENO SCHEME
!
    ZFPOS1(IW,:,:) = 0.5  * (3.0*PSRC(IW,:,:) - PSRC(IW-1,:,:))
    ZFPOS2(IW,:,:) = 0.5 * (PSRC(IW     ,:,:) + PSRC(IW+1,:,:))
    ZBPOS1(IW,:,:) = (PSRC(IW,:,:) - PSRC(IW-1,:,:))**2
    ZBPOS2(IW,:,:) = (PSRC(IW+1,  :,:) - PSRC(IW,:,:))**2
!
    ZFNEG1(IW,:,:) = 0.5 * (3.0*PSRC(IW+1,:,:) - PSRC(IW+2,:,:))
    ZFNEG2(IW,:,:) = 0.5 * (PSRC(IW,      :,:) + PSRC(IW+1,:,:))
    ZBNEG1(IW,:,:) = (PSRC(IW+1,:,:) - PSRC(IW+2,:,:))**2
    ZBNEG2(IW,:,:) = (PSRC(IW,  :,:) - PSRC(IW+1,:,:))**2
!
    ZOMP1(IW,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IW,:,:))**2
    ZOMP2(IW,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IW,:,:))**2
    ZOMN1(IW,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IW,:,:))**2
    ZOMN2(IW,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IW,:,:))**2
!
    PR(IW,:,:) = (ZOMN2(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)) * &
      ZFNEG2(IW,:,:) +  &
       (ZOMN1(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)) * ZFNEG1(IW,:,:))) *  &
       (0.5-SIGN(0.5,PRUCT(IW,:,:))) +                                    &
       (ZOMP2(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)) * ZFPOS2(IW,:,:) +    &
       (ZOMP1(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)) * ZFPOS1(IW,:,:))) *  &
       (0.5+SIGN(0.5,PRUCT(IW,:,:)))
!
!    PROC. BORDER (WEST)
!
  ELSEIF(NHALO == 1) THEN
!
!   THIRD ORDER WENO SCHEME
!
    ZFPOS1(IW-1,:,:) = 0.5  * (3.0*PSRC(IW-1,:,:) - TPHALO2%WEST(:,:))
    ZFPOS2(IW-1,:,:) = 0.5 * (PSRC(IW-1,     :,:) + PSRC(IW,:,:))
    ZBPOS1(IW-1,:,:) = (PSRC(IW-1,:,:) - TPHALO2%WEST(:,:))**2
    ZBPOS2(IW-1,:,:) = (PSRC(IW,  :,:) - PSRC(IW-1,:,:))**2
!
    ZFNEG1(IW-1,:,:) = 0.5 * (3.0*PSRC(IW,:,:) - PSRC(IW+1,:,:))
    ZFNEG2(IW-1,:,:) = 0.5 * (PSRC(IW-1,  :,:) + PSRC(IW,  :,:))
    ZBNEG1(IW-1,:,:) = (PSRC(IW,  :,:) - PSRC(IW+1,:,:))**2
    ZBNEG2(IW-1,:,:) = (PSRC(IW-1,:,:) - PSRC(IW,  :,:))**2
!
    ZOMP1(IW-1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IW-1,:,:))**2
    ZOMP2(IW-1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IW-1,:,:))**2
    ZOMN1(IW-1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IW-1,:,:))**2
    ZOMN2(IW-1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IW-1,:,:))**2
!
    PR(IW-1,:,:) = (ZOMN2(IW-1,:,:)/(ZOMN1(IW-1,:,:)+ZOMN2(IW-1,:,:)) &
               * ZFNEG2(IW-1,:,:)  &
               + (ZOMN1(IW-1,:,:)/(ZOMN1(IW-1,:,:)+ZOMN2(IW-1,:,:)) * &
                 ZFNEG1(IW-1,:,:))) *&
               (0.5-SIGN(0.5,PRUCT(IW-1,:,:)))                        &
               + (ZOMP2(IW-1,:,:)/(ZOMP1(IW-1,:,:)+ZOMP2(IW-1,:,:)) * &
                 ZFPOS2(IW-1,:,:)    &
               + (ZOMP1(IW-1,:,:)/(ZOMP1(IW-1,:,:)+ZOMP2(IW-1,:,:)) * &
                 ZFPOS1(IW-1,:,:))) *&
               (0.5+SIGN(0.5,PRUCT(IW-1,:,:)))
!
!   FIFTH ORDER WENO SCHEME
!
    ZFPOS1(IW,:,:) = 1./6 * (2.0*TPHALO2%WEST(:,:) - 7.0*PSRC(IW-1,:,:) + &
                     11.0*PSRC(IW, :,:))
    ZFPOS2(IW,:,:) = 1./6 * (-1.0*PSRC(IW-1,  :,:) + 5.0*PSRC(IW,  :,:) + &
                     2.0*PSRC(IW+1,:,:))
    ZFPOS3(IW,:,:) = 1./6 * (2.0*PSRC(IW,     :,:) + 5.0*PSRC(IW+1,:,:) - &
                     PSRC(IW+2,:,:))
!
    ZFNEG1(IW,:,:) = 1./6 * (11.0*PSRC(IW+1,:,:) - 7.0*PSRC(IW+2,:,:) + &
                     2.0*PSRC(IW+3,:,:))
    ZFNEG2(IW,:,:) = 1./6 * ( 2.0*PSRC(IW,  :,:) + 5.0*PSRC(IW+1,:,:) - &
                     PSRC(IW+2,:,:))
    ZFNEG3(IW,:,:) = 1./6 * (-1.0*PSRC(IW-1,:,:) + 5.0*PSRC(IW,  :,:) + &
                     2.0*PSRC(IW+1,:,:))  
!
    ZBPOS1(IW,:,:) = 13./12 * (TPHALO2%WEST(:,:) - 2.0*PSRC(IW-1,:,:) + &
                     PSRC(IW,:,:))**2 + &
                     1./4 * (TPHALO2%WEST(:,:) - 4.0*PSRC(IW-1,:,:) + &
                     3.0*PSRC(IW,:,:))**2
    ZBPOS2(IW,:,:) = 13./12 * (PSRC(IW-1,:,:) - 2.0*PSRC(IW,:,:) + &
                     PSRC(IW+1,:,:))**2 + &
                     1./4 * (PSRC(IW-1,:,:) - PSRC(IW+1,:,:))**2
    ZBPOS3(IW,:,:) = 13./12 * (PSRC(IW,:,:) - 2.0*PSRC(IW+1,:,:) + &
                     PSRC(IW+2,:,:))**2 + &
                     1./4 * ( 3.0*PSRC(IW,:,:) - 4.0*PSRC(IW+1,:,:) + &
                     PSRC(IW+2,:,:))**2
!
    ZBNEG1(IW,:,:) = 13./12 * (PSRC(IW+1,:,:) - 2.0*PSRC(IW+2,:,:) + &
                     PSRC(IW+3,:,:))**2 + &
                     1./4 * ( 3.0*PSRC(IW+1,:,:) - 4.0*PSRC(IW+2,:,:) + &
                     PSRC(IW+3,:,:))**2
    ZBNEG2(IW,:,:) = 13./12 * (PSRC(IW,:,:) - 2.0*PSRC(IW+1,:,:) + &
                     PSRC(IW+2,:,:))**2 + &
                     1./4 * (PSRC(IW,:,:) - PSRC(IW+2,:,:))**2    
    ZBNEG3(IW,:,:) = 13./12 * (PSRC(IW-1,:,:) - 2.0*PSRC(IW,:,:) + &
                     PSRC(IW+1,:,:))**2 + &
                     1./4 * ( PSRC(IW-1,:,:) - 4.0*PSRC(IW,:,:) + &
                     3.0*PSRC(IW+1,:,:))**2
!
    ZOMP1(IW,:,:) = ZGAMMA1 / (ZEPS + ZBPOS1(IW,:,:))**2
    ZOMP2(IW,:,:) = ZGAMMA2 / (ZEPS + ZBPOS2(IW,:,:))**2
    ZOMP3(IW,:,:) = ZGAMMA3 / (ZEPS + ZBPOS3(IW,:,:))**2
    ZOMN1(IW,:,:) = ZGAMMA1 / (ZEPS + ZBNEG1(IW,:,:))**2
    ZOMN2(IW,:,:) = ZGAMMA2 / (ZEPS + ZBNEG2(IW,:,:))**2
    ZOMN3(IW,:,:) = ZGAMMA3 / (ZEPS + ZBNEG3(IW,:,:))**2
!
    PR(IW,:,:) = (ZOMP2(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)+ &
                  ZOMP3(IW,:,:)) * ZFPOS2(IW,:,:)      &
                   + ZOMP1(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)+ &
                   ZOMP3(IW,:,:)) * ZFPOS1(IW,:,:)   &
                   + ZOMP3(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)+ &
                   ZOMP3(IW,:,:)) * ZFPOS3(IW,:,:)) *&
                   (0.5+SIGN(0.5,PRUCT(IW,:,:)))                 &
                   + (ZOMN2(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)+&
                   ZOMN3(IW,:,:)) * ZFNEG2(IW,:,:)  &
                   + ZOMN1(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)+ &
                   ZOMN3(IW,:,:)) * ZFNEG1(IW,:,:)   &
                   + ZOMN3(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)+ &
                   ZOMN3(IW,:,:)) * ZFNEG3(IW,:,:)) *&
                   (0.5-SIGN(0.5,PRUCT(IW,:,:)))
!
  ENDIF
!
! PHYSICAL BORDER (EAST)
!
  IF(LEAST_ll()) THEN
    PR(IE,:,:) = PSRC(IE,:,:) * (0.5+SIGN(0.5,PRUCT(IE,:,:))) + &
                 PSRC(IE+1,:,:) * &
                 (0.5-SIGN(0.5,PRUCT(IE,:,:)))
!
    ZFPOS1(IE-1,:,:) = 0.5 * (3.0*PSRC(IE-1,:,:) - PSRC(IE-2,:,:))
    ZFPOS2(IE-1,:,:) = 0.5 * (PSRC(IE-1,    :,:) + PSRC(IE,  :,:))
    ZBPOS1(IE-1,:,:) = (PSRC(IE-1,:,:) - PSRC(IE-2,:,:))**2
    ZBPOS2(IE-1,:,:) = (PSRC(IE,  :,:) - PSRC(IE-1,:,:))**2
!
    ZFNEG1(IE-1,:,:) = 0.5 * (3.0*PSRC(IE,:,:) - PSRC(IE+1,:,:))
    ZFNEG2(IE-1,:,:) = 0.5 * (PSRC(IE-1,  :,:) + PSRC(IE,  :,:))
    ZBNEG1(IE-1,:,:) = (PSRC(IE,  :,:) - PSRC(IE+1,:,:))**2
    ZBNEG2(IE-1,:,:) = (PSRC(IE-1,:,:) - PSRC(IE,  :,:))**2
!
    ZOMP1(IE-1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IE-1,:,:))**2
    ZOMP2(IE-1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IE-1,:,:))**2
    ZOMN1(IE-1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IE-1,:,:))**2
    ZOMN2(IE-1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IE-1,:,:))**2
!    
      PR(IE-1,:,:) = (ZOMN2(IE-1,:,:)/(ZOMN1(IE-1,:,:)+ZOMN2(IE-1,:,:)) * &
               ZFNEG2(IE-1,:,:)&
               + (ZOMN1(IE-1,:,:)/(ZOMN1(IE-1,:,:)+ZOMN2(IE-1,:,:)) * &
               ZFNEG1(IE-1,:,:))) *&
               (0.5-SIGN(0.5,PRUCT(IE-1,:,:)))                        &
               + (ZOMP2(IE-1,:,:)/(ZOMP1(IE-1,:,:)+ZOMP2(IE-1,:,:)) * &
               ZFPOS2(IE-1,:,:)    &
               + (ZOMP1(IE-1,:,:)/(ZOMP1(IE-1,:,:)+ZOMP2(IE-1,:,:)) * &
               ZFPOS1(IE-1,:,:))) *&
               (0.5+SIGN(0.5,PRUCT(IE-1,:,:)))
!
! PROC. BORDER (EAST)
!
  ELSEIF(NHALO == 1) THEN
!
    ZFPOS1(IE,:,:) = 0.5 * (3.0*PSRC(IE,:,:) - PSRC(IE-1,:,:))
    ZFPOS2(IE,:,:) = 0.5 * (PSRC(IE,    :,:) + PSRC(IE+1,:,:))
    ZBPOS1(IE,:,:) = (PSRC(IE,  :,:) - PSRC(IE-1,:,:))**2
    ZBPOS2(IE,:,:) = (PSRC(IE+1,:,:) - PSRC(IE,  :,:))**2
!
    ZFNEG1(IE,:,:) = 0.5 * (3.0*PSRC(IE+1,:,:) - TPHALO2%EAST(:,:))
    ZFNEG2(IE,:,:) = 0.5 * (PSRC(IE,      :,:) + PSRC(IE+1,:,:))
    ZBNEG1(IE,:,:) = (PSRC(IE+1,:,:) - TPHALO2%EAST(:,:))**2
    ZBNEG2(IE,:,:) = (PSRC(IE,  :,:) - PSRC(IE+1,:,:))**2
!
    ZOMP1(IE,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IE,:,:))**2
    ZOMP2(IE,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IE,:,:))**2
    ZOMN1(IE,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IE,:,:))**2
    ZOMN2(IE,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IE,:,:))**2
!
    PR(IE,:,:) = (ZOMN2(IE,:,:)/(ZOMN1(IE,:,:)+ZOMN2(IE,:,:)) * ZFNEG2(IE,:,:)&
               + (ZOMN1(IE,:,:)/(ZOMN1(IE,:,:)+ZOMN2(IE,:,:)) * ZFNEG1(IE,:,:))) *&
               (0.5-SIGN(0.5,PRUCT(IE,:,:)))                                      &
               + (ZOMP2(IE,:,:)/(ZOMP1(IE,:,:)+ZOMP2(IE,:,:)) * ZFPOS2(IE,:,:)    &
               + (ZOMP1(IE,:,:)/(ZOMP1(IE,:,:)+ZOMP2(IE,:,:)) * ZFPOS1(IE,:,:))) *&
               (0.5+SIGN(0.5,PRUCT(IE,:,:)))
!
!
    ZFPOS1(IE-1,:,:) = 1./6 * (2.0 *PSRC(IE-3,:,:) - 7.0*PSRC(IE-2,:,:) + &
                              11.0*PSRC(IE-1,:,:))
    ZFPOS2(IE-1,:,:) = 1./6 * (-1.0*PSRC(IE-2,:,:) + 5.0*PSRC(IE-1,:,:) + &
                              2.0*PSRC(IE,:,:))
    ZFPOS3(IE-1,:,:) = 1./6 * (2.0 *PSRC(IE-1,:,:) + 5.0*PSRC(IE,  :,:) - &
                              PSRC(IE+1,  :,:))
!
    ZFNEG1(IE-1,:,:) = 1./6 * (11.0*PSRC(IE,:,:) - 7.0*PSRC(IE+1,:,:) + &
                              2.0*TPHALO2%EAST(:,:))
    ZFNEG2(IE-1,:,:) = 1./6 * (2.0*PSRC(IE-1,:,:) + 5.0*PSRC(IE,:,:) - &
                              PSRC(IE+1,:,:))
    ZFNEG3(IE-1,:,:) = 1./6 * (-1.0*PSRC(IE-2,:,:) + 5.0*PSRC(IE-1,:,:) + &
                              2.0*PSRC(IE,:,:))
!
    ZBPOS1(IE-1,:,:) = 13./12 * (PSRC(IE-3,:,:) - 2.0*PSRC(IE-2,:,:) + &
                       PSRC(IE-1,:,:))**2 + &
                       1./4 * (PSRC(IE-3,:,:) - 4.0*PSRC(IE-2,:,:) + &
                       3.0*PSRC(IE-1,:,:))**2
    ZBPOS2(IE-1,:,:) = 13./12 * (PSRC(IE-2,:,:) - 2.0*PSRC(IE-1,:,:) + &
                       PSRC(IE,:,:))**2 + &
                       1./4 * (PSRC(IE-2,:,:) - PSRC(IE,:,:))**2
    ZBPOS3(IE-1,:,:) = 13./12 * (PSRC(IE-1,:,:) - 2.0*PSRC(IE,:,:) + &
                       PSRC(IE+1,:,:))**2 + &
                       1./4 * ( 3.0*PSRC(IE-1,:,:) - 4.0*PSRC(IE,:,:) + &
                       PSRC(IE+1,:,:))**2!
    ZBNEG1(IE-1,:,:) = 13./12 * (PSRC(IE,:,:) - 2.0*PSRC(IE+1,:,:) + &
                       TPHALO2%EAST(:,:))**2 + &
                       1./4 * ( 3.0*PSRC(IE,:,:) - 4.0*PSRC(IE+1,:,:) + &
                       TPHALO2%EAST(:,:))**2
    ZBNEG2(IE-1,:,:) = 13./12 * (PSRC(IE-1,:,:) - 2.0*PSRC(IE,:,:) + &
                       PSRC(IE+1,:,:))**2 + &
                       1./4 * (PSRC(IE-1,:,:) - PSRC(IE+1,:,:))**2
    ZBNEG3(IE-1,:,:) = 13./12 * (PSRC(IE-2,:,:) - 2.0*PSRC(IE-1,:,:) + &
                       PSRC(IE,:,:))**2 + &
                       1./4 * ( PSRC(IE-2,:,:) - 4.0*PSRC(IE-1,:,:) + &
                       3.0*PSRC(IE,:,:))**2
!
    ZOMP1(IE-1,:,:) = ZGAMMA1 / (ZEPS + ZBPOS1(IE-1,:,:))**2
    ZOMP2(IE-1,:,:) = ZGAMMA2 / (ZEPS + ZBPOS2(IE-1,:,:))**2
    ZOMP3(IE-1,:,:) = ZGAMMA3 / (ZEPS + ZBPOS3(IE-1,:,:))**2
    ZOMN1(IE-1,:,:) = ZGAMMA1 / (ZEPS + ZBNEG1(IE-1,:,:))**2
    ZOMN2(IE-1,:,:) = ZGAMMA2 / (ZEPS + ZBNEG2(IE-1,:,:))**2
    ZOMN3(IE-1,:,:) = ZGAMMA3 / (ZEPS + ZBNEG3(IE-1,:,:))**2
!
      PR(IE-1,:,:) = (ZOMP2(IE-1,:,:)/(ZOMP1(IE-1,:,:)+ZOMP2(IE-1,:,:)+ &
                   ZOMP3(IE-1,:,:)) * ZFPOS2(IE-1,:,:)   &
                   + ZOMP1(IE-1,:,:)/(ZOMP1(IE-1,:,:)+ZOMP2(IE-1,:,:)+ &
                   ZOMP3(IE-1,:,:)) * ZFPOS1(IE-1,:,:)    &
                   + ZOMP3(IE-1,:,:)/(ZOMP1(IE-1,:,:)+ZOMP2(IE-1,:,:)+ &
                   ZOMP3(IE-1,:,:)) * ZFPOS3(IE-1,:,:)) * &
                   (0.5+SIGN(0.5,PRUCT(IE-1,:,:))) &
                  + (ZOMN2(IE-1,:,:)/(ZOMN1(IE-1,:,:)+ZOMN2(IE-1,:,:)+ &
                  ZOMN3(IE-1,:,:)) * ZFNEG2(IE-1,:,:)    &
                   + ZOMN1(IE-1,:,:)/(ZOMN1(IE-1,:,:)+ZOMN2(IE-1,:,:)+ &
                   ZOMN3(IE-1,:,:)) * ZFNEG1(IE-1,:,:)    &
                   + ZOMN3(IE-1,:,:)/(ZOMN1(IE-1,:,:)+ZOMN2(IE-1,:,:)+ &
                   ZOMN3(IE-1,:,:)) * ZFNEG3(IE-1,:,:)) * &
                   (0.5-SIGN(0.5,PRUCT(IE-1,:,:)))
!
  ENDIF
!
!      USE A FIFTH ORDER UPSTREAM WENO SCHEME ELSEWHERE (IW+1 --> IE-2) 
!
  ZFPOS1(IW+1:IE-2,:,:) = 1./6 * (2.0*PSRC(IW-1:IE-4,:,:) - &
   7.0*PSRC(IW:IE-3,:,:) + 11.0*PSRC(IW+1:IE-2,:,:))
  ZFPOS2(IW+1:IE-2,:,:) = 1./6 * (-1.0*PSRC(IW:IE-3,:,:) + &
   5.0*PSRC(IW+1:IE-2,:,:) + 2.0*PSRC(IW+2:IE-1,:,:))
  ZFPOS3(IW+1:IE-2,:,:) = 1./6 * (2.0*PSRC(IW+1:IE-2,:,:) + &
   5.0*PSRC(IW+2:IE-1,:,:) - PSRC(IW+3:IE,:,:))
!
  ZFNEG1(IW+1:IE-2,:,:) = 1./6 * (11.0*PSRC(IW+2:IE-1,:,:) - &
   7.0*PSRC(IW+3:IE,:,:) + 2.0*PSRC(IW+4:IE+1,:,:))
  ZFNEG2(IW+1:IE-2,:,:) = 1./6 * (2.0*PSRC(IW+1:IE-2,:,:) + &
   5.0*PSRC(IW+2:IE-1,:,:) - PSRC(IW+3:IE,:,:))
  ZFNEG3(IW+1:IE-2,:,:) = 1./6 * (-1.0*PSRC(IW:IE-3,:,:) + &
   5.0*PSRC(IW+1:IE-2,:,:) + 2.0*PSRC(IW+2:IE-1,:,:))
!
  ZBPOS1(IW+1:IE-2,:,:) = 13./12 * (PSRC(IW-1:IE-4,:,:) - &
    2.0*PSRC(IW:IE-3,:,:) + PSRC(IW+1:IE-2,:,:))**2 + &
    1./4 * (PSRC(IW-1:IE-4,:,:) - 4.0*PSRC(IW:IE-3,:,:) + &
    3.0*PSRC(IW+1:IE-2,:,:))**2
  ZBPOS2(IW+1:IE-2,:,:) = 13./12 * (PSRC(IW:IE-3,:,:) - &
    2.0*PSRC(IW+1:IE-2,:,:) + PSRC(IW+2:IE-1,:,:))**2 + &
    1./4 * (PSRC(IW:IE-3,:,:) - PSRC(IW+2:IE-1,:,:))**2
  ZBPOS3(IW+1:IE-2,:,:) = 13./12 * (PSRC(IW+1:IE-2,:,:) - &
    2.0*PSRC(IW+2:IE-1,:,:) + PSRC(IW+3:IE,:,:))**2 + &
    1./4 * ( 3.0*PSRC(IW+1:IE-2,:,:) - 4.0*PSRC(IW+2:IE-1,:,:) &
    + PSRC(IW+3:IE,:,:))**2
!
  ZBNEG1(IW+1:IE-2,:,:) = 13./12 * (PSRC(IW+2:IE-1,:,:) - &
    2.0*PSRC(IW+3:IE,:,:) + PSRC(IW+4:IE+1,:,:))**2 + &
    1./4 * ( 3.0*PSRC(IW+2:IE-1,:,:) - 4.0*PSRC(IW+3:IE,:,:) + &
    PSRC(IW+4:IE+1,:,:))**2
  ZBNEG2(IW+1:IE-2,:,:) = 13./12 * (PSRC(IW+1:IE-2,:,:) - &
    2.0*PSRC(IW+2:IE-1,:,:) + PSRC(IW+3:IE,:,:))**2 + &
    1./4 * (PSRC(IW+1:IE-2,:,:) - PSRC(IW+3:IE,:,:))**2
  ZBNEG3(IW+1:IE-2,:,:) = 13./12 * (PSRC(IW:IE-3,:,:) - &
    2.0*PSRC(IW+1:IE-2,:,:) + PSRC(IW+2:IE-1,:,:))**2 + &
    1./4 * ( PSRC(IW:IE-3,:,:) - 4.0*PSRC(IW+1:IE-2,:,:) + &
    3.0*PSRC(IW+2:IE-1,:,:))**2
!
  ZOMP1(IW+1:IE-2,:,:) = ZGAMMA1 / (ZEPS + ZBPOS1(IW+1:IE-2,:,:))**2
  ZOMP2(IW+1:IE-2,:,:) = ZGAMMA2 / (ZEPS + ZBPOS2(IW+1:IE-2,:,:))**2
  ZOMP3(IW+1:IE-2,:,:) = ZGAMMA3 / (ZEPS + ZBPOS3(IW+1:IE-2,:,:))**2
  ZOMN1(IW+1:IE-2,:,:) = ZGAMMA1 / (ZEPS + ZBNEG1(IW+1:IE-2,:,:))**2
  ZOMN2(IW+1:IE-2,:,:) = ZGAMMA2 / (ZEPS + ZBNEG2(IW+1:IE-2,:,:))**2
  ZOMN3(IW+1:IE-2,:,:) = ZGAMMA3 / (ZEPS + ZBNEG3(IW+1:IE-2,:,:))**2
!
    PR(IW+1:IE-2,:,:) = (ZOMP2(IW+1:IE-2,:,:)/(ZOMP1(IW+1:IE-2,:,:)+ &
      ZOMP2(IW+1:IE-2,:,:)+ &
      ZOMP3(IW+1:IE-2,:,:)) * ZFPOS2(IW+1:IE-2,:,:)  +                 &
      ZOMP1(IW+1:IE-2,:,:)/(ZOMP1(IW+1:IE-2,:,:)+ZOMP2(IW+1:IE-2,:,:)+ &
      ZOMP3(IW+1:IE-2,:,:)) * ZFPOS1(IW+1:IE-2,:,:)  +                 &
      ZOMP3(IW+1:IE-2,:,:)/(ZOMP1(IW+1:IE-2,:,:)+ZOMP2(IW+1:IE-2,:,:)+ &
      ZOMP3(IW+1:IE-2,:,:)) * ZFPOS3(IW+1:IE-2,:,:)) *                 &
      (0.5+SIGN(0.5,PRUCT(IW+1:IE-2,:,:))) +                           &
      (ZOMN2(IW+1:IE-2,:,:)/(ZOMN1(IW+1:IE-2,:,:)+ZOMN2(IW+1:IE-2,:,:)+&
      ZOMN3(IW+1:IE-2,:,:)) * ZFNEG2(IW+1:IE-2,:,:) +                  &
      ZOMN1(IW+1:IE-2,:,:)/(ZOMN1(IW+1:IE-2,:,:)+ZOMN2(IW+1:IE-2,:,:)+ &
      ZOMN3(IW+1:IE-2,:,:)) * ZFNEG1(IW+1:IE-2,:,:)   +                &
      ZOMN3(IW+1:IE-2,:,:)/(ZOMN1(IW+1:IE-2,:,:)+ZOMN2(IW+1:IE-2,:,:)+ &
      ZOMN3(IW+1:IE-2,:,:)) * ZFNEG3(IW+1:IE-2,:,:)) *                 &
      (0.5-SIGN(0.5,PRUCT(IW+1:IE-2,:,:)))
!
END SELECT
!
PR = PR * PRUCT
!
END SUBROUTINE ADVEC_WENO_K_3_UX
!
!------------------------------------------------------------------------------
!
!     ############################################################
      SUBROUTINE ADVEC_WENO_K_3_MX(HLBCX,PSRC, PRUCT, PR, TPHALO2)
!     ############################################################
!!
!!**** Computes PRUCT * PWT (or PRUCT * PVT). Upstream fluxes of W (or V)
!!     variables in X direction.  
!!     Input PWT is on W Grid 'ie' (i,j,k) based on WGRID reference
!!     Output PR is on mass Grid 'ie' (i-1/2,j,k) based on WGRID reference  
!!
!!    AUTHOR
!!    ------
!!    F. Visentin   *CNRS/LA*                
!!
!!    MODIFICATIONS
!!    -------------
!!
!------------------------------------------------------------------------------
!
USE MODE_ll
USE MODD_LUNIT
USE MODD_CONF
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX  ! X direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on U grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRUCT ! contrav. comp. on MASS GRID
!
! output source term
!
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PR
TYPE(HALO2_ll), OPTIONAL, POINTER :: TPHALO2      ! halo2 for the field at t
!
!*       0.2   Declarations of local variables :
!
INTEGER :: IIB,IJB    ! Begining useful area in x,y,z directions
INTEGER :: IIE,IJE    ! End useful area in x,y,z directions
INTEGER::  IW,IE   ! Coordinate of third order diffusion area
!
INTEGER:: ILUOUT,IRESP   ! for prints
!
!
! intermediate reconstruction fluxes for positive wind case
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFPOS1, ZFPOS2, ZFPOS3
!
! intermediate reconstruction fluxes for negative wind case
! we need only one since ZFNEG2 = ZFPOS2
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFNEG1, ZFNEG2, ZFNEG3
!
! smoothness indicators for positive wind case
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBPOS1, ZBPOS2, ZBPOS3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBNEG1, ZBNEG2, ZBNEG3
!
! WENO weights
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: ZOMP1, ZOMP2, ZOMP3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: ZOMN1, ZOMN2, ZOMN3
!
! standard weights
!
REAL, PARAMETER :: ZGAMMA1 = 1./10.
REAL, PARAMETER :: ZGAMMA2 = 3./5.
REAL, PARAMETER :: ZGAMMA3 = 3./10.
REAL, PARAMETER :: ZGAMMA1_PRIM = 1./3.
REAL, PARAMETER :: ZGAMMA2_PRIM = 2./3.
!
REAL, PARAMETER :: ZEPS = 1.0E-15
!
!-------------------------------------------------------------------------------
!
!*       0.3.     COMPUTES THE DOMAIN DIMENSIONS
!                 ------------------------------
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
!-----------------------------------------------------------------------------
!
!*       0.4.   INITIALIZE THE FIELD 
!               ---------------------
!
PR(:,:,:) = 0.0
!
ZFPOS1 = 0.0
ZFPOS2 = 0.0
ZFPOS3 = 0.0
ZFNEG1 = 0.0
ZFNEG2 = 0.0
ZFNEG3 = 0.0
ZBPOS1 = 0.0
ZBPOS2 = 0.0
ZBPOS3 = 0.0
ZBNEG1 = 0.0
ZBNEG2 = 0.0
ZBNEG3 = 0.0
ZOMP1  = 0.0
ZOMP2  = 0.0
ZOMP3  = 0.0
ZOMN1  = 0.0
ZOMN2  = 0.0
ZOMN3  = 0.0 
!
!------------------------------------------------------------------------------
!
SELECT CASE ( HLBCX(1) ) ! X direction LBC type: (1) for left side
!
!*       1.1    CYCLIC CASE IN THE X DIRECTION:
!
CASE ('CYCL')          ! In that case one must have HLBCX(1) == HLBCX(2)
!
  IF(NHALO == 1) THEN
    IW=IIB
    IE=IIE
  ELSE
    CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT,IRESP)
    WRITE(ILUOUT,*) 'ERROR : 3rd order advection in CYCLic case '
    WRITE(ILUOUT,*) 'cannot be used with NHALO=2'
    CALL ABORT
    STOP
  END IF  
!
! r: many left cells in regard to 'i-1' cell for each stencil
! 
! intermediate fluxes at the mass point on Ugrid u(i-1/2,j,k)=((i-1)+1/2,j,k) 
! for positive wind case (left to the right)
! (r=2 for the first stencil ZFPOS1, r=1 for the second ZFPOS2 and
!  r=0 for the last ZFPOS3)
!
  ZFPOS1(IW+2:IE,:,:) = 1./6 * (2.0*PSRC(IW-1:IE-3,:,:) - 7.0*PSRC(IW:IE-2,:,:) + &
                        11.0*PSRC(IW+1:IE-1,:,:))
  ZFPOS1(IW+1,   :,:) = 1./6 * (2.0*TPHALO2%WEST(:,:)   - 7.0*PSRC(IW-1,   :,:) + &
                        11.0*PSRC(IW,       :,:))
  ZFPOS1(IE+1,:,:) = 0.5 * (3.0*PSRC(IE,  :,:) - PSRC(IE-1,:,:))
  ZFPOS1(IW,  :,:) = 0.5 * (3.0*PSRC(IW-1,:,:) - TPHALO2%WEST(:,:) )
  ZFPOS1(IW-1,:,:) = - 999.
!
!
  ZFPOS2(IW+1:IE,:,:) = 1./6 * (-1.0*PSRC(IW-1:IE-2,:,:) + 5.0*PSRC(IW:IE-1,:,:) + &
                        2.0*PSRC(IW+1:IE,:,:))
  ZFPOS2(IE+1,:,:) = 0.5 * (PSRC(IE+1,:,:)    + PSRC(IE,  :,:)) 
  ZFPOS2(IW,  :,:) = 0.5 * (PSRC(IW-1,:,:)    + PSRC(IW,  :,:))
  ZFPOS2(IW-1,:,:) = 0.5 * (TPHALO2%WEST(:,:) + PSRC(IW-1,:,:))
!
!
  ZFPOS3(IW+1:IE,:,:) = 1./6 * (2.0*PSRC(IW:IE-1,:,:) + 5.0*PSRC(IW+1:IE,:,:) - &
                        PSRC(IW+2:IE+1,:,:))
!
! r: many left cells in regard to 'i' cell for each stencil
!
! intermediate fluxes at the mass point on Ugrid u(i-1/2,j,k) for negative wind
! case (R. to the L.)
! (r=2 for the third stencil ZFNEG3=ZFPOS2, r=1 for the second ZFNEG2=ZFPOS3 
!  and r=0 for the first ZFNEG1)
!
  ZFNEG1(IW+1:IE-1,:,:) = 1./6 * (11.0*PSRC(IW+1:IE-1,:,:) - 7.0*PSRC(IW+2:IE,:,:)&
                          + 2.0*PSRC(IW+3:IE+1,:,:))
  ZFNEG1(IE,       :,:) = 1./6 * (11.0*PSRC(IE,       :,:) - 7.0*PSRC(IE+1,   :,:)&
                          + 2.0*TPHALO2%EAST(:,:))
  ZFNEG1(IW,  :,:) = 0.5 * (3.0*PSRC(IW,  :,:) - PSRC(IW+1,   :,:))
  ZFNEG1(IW-1,:,:) = 0.5 * (3.0*PSRC(IW-1,:,:) - PSRC(IW,     :,:))
  ZFNEG1(IE+1,:,:) = 0.5 * (3.0*PSRC(IE+1,:,:) - TPHALO2%EAST(:,:))
!
!
  ZFNEG2(IW+1:IE,:,:) = 1./6 * (2.0*PSRC(IW:IE-1,:,:) + 5.0*PSRC(IW+1:IE,:,:) - &
                        PSRC(IW+2:IE+1,:,:))
  ZFNEG2(IW,  :,:) = 0.5 * (PSRC(IW,  :,:) + PSRC(IW-1,   :,:))
  ZFNEG2(IW-1,:,:) = 0.5 * (PSRC(IW-1,:,:) + TPHALO2%WEST(:,:))
  ZFNEG2(IE+1,:,:) = 0.5 * (PSRC(IE+1,:,:) + PSRC(IE,     :,:))
! 
!
  ZFNEG3(IW+1:IE,:,:) = 1./6 * (-1.0*PSRC(IW-1:IE-2,:,:) + 5.0*PSRC(IW:IE-1,:,:) + &
                        2.0*PSRC(IW+1:IE,:,:))
!
! smoothness indicators for positive wind case
!
  ZBPOS1(IW+2:IE,:,:) = 13./12 * (PSRC(IW-1:IE-3,:,:) - 2.0*PSRC(IW:IE-2,:,:) + &
                        PSRC(IW+1:IE-1,:,:))**2 + &
                        1./4 * (PSRC(IW-1:IE-3,:,:) - 4.0*PSRC(IW:IE-2,:,:) + &
                        3.0*PSRC(IW+1:IE-1,:,:))**2
  ZBPOS1(IW+1,   :,:) = 13./12 * (TPHALO2%WEST(:,:) - 2.0*PSRC(IW-1,:,:) + &
                        PSRC(IW,:,:))**2 + &
                        1./4 * (TPHALO2%WEST(:,:) - 4.0*PSRC(IW-1,:,:) + &
                        3.0*PSRC(IW,:,:))**2
  ZBPOS1(IE+1,:,:) = (PSRC(IE,  :,:) - PSRC(IE-1,:,:))**2
  ZBPOS1(IW,  :,:) = (PSRC(IW-1,:,:) - TPHALO2%WEST(:,:))**2
  ZBPOS1(IW-1,:,:) = - 999.
!
!
  ZBPOS2(IW+1:IE,:,:) = 13./12 * (PSRC(IW-1:IE-2,:,:) - 2.0*PSRC(IW:IE-1,:,:) + &
                        PSRC(IW+1:IE,:,:))**2 + &
                        1./4 * (PSRC(IW-1:IE-2,:,:) - PSRC(IW+1:IE,:,:))**2
  ZBPOS2(IE+1,:,:) = (PSRC(IE+1,:,:) - PSRC(IE,:,:))**2
  ZBPOS2(IW,  :,:) = (PSRC(IW,  :,:) - PSRC(IW-1,:,:))**2
  ZBPOS2(IW-1,:,:) = (PSRC(IW-1,:,:) - TPHALO2%WEST(:,:))**2
!
!
  ZBPOS3(IW+1:IE,:,:) = 13./12 * (PSRC(IW:IE-1,:,:) - 2.0*PSRC(IW+1:IE,:,:) + &
                        PSRC(IW+2:IE+1,:,:))**2 + &
                        1./4 * ( 3.0*PSRC(IW:IE-1,:,:) - 4.0*PSRC(IW+1:IE,:,:) + &
                        PSRC(IW+2:IE+1,:,:))**2
!
! smoothness indicators for negative wind case
!       
  ZBNEG1(IW+1:IE-1,:,:) = 13./12 * (PSRC(IW+1:IE-1,:,:) - 2.0*PSRC(IW+2:IE,:,:) + &
                          PSRC(IW+3:IE+1,:,:))**2 + &
                          1./4 * ( 3.0*PSRC(IW+1:IE-1,:,:) - 4.0*PSRC(IW+2:IE,:,:)&
                          + PSRC(IW+3:IE+1,:,:))**2
  ZBNEG1(IE,       :,:) = 13./12 * (PSRC(IE,:,:) - 2.0*PSRC(IE+1,:,:) + &
                          TPHALO2%EAST(:,:))**2 + &
                          1./4 * ( 3.0*PSRC(IE,:,:) - 4.0*PSRC(IE+1,:,:) + &
                          TPHALO2%EAST(:,:))**2
  ZBNEG1(IW,  :,:) = (PSRC(IW,  :,:) - PSRC(IW+1,:,:))**2
  ZBNEG1(IW-1,:,:) = (PSRC(IW-1,:,:) - PSRC(IW,  :,:))**2
  ZBNEG1(IE+1,:,:) = (PSRC(IE+1,:,:) - TPHALO2%EAST(:,:))**2
!
!
  ZBNEG2(IW+1:IE,:,:) = 13./12 * (PSRC(IW:IE-1,:,:) - 2.0*PSRC(IW+1:IE,:,:) + &
                        PSRC(IW+2:IE+1,:,:))**2 + &
                        1./4 * (PSRC(IW:IE-1,:,:) - PSRC(IW+2:IE+1,:,:))**2
  ZBNEG2(IW,  :,:) = (PSRC(IW-1,:,:) - PSRC(IW,  :,:))**2
  ZBNEG2(IE+1,:,:) = (PSRC(IE,  :,:) - PSRC(IE+1,:,:))**2
  ZBNEG2(IW-1,:,:) = (TPHALO2%WEST(:,:) - PSRC(IW-1,:,:))**2
!
!
  ZBNEG3(IW+1:IE,:,:) = 13./12 * (PSRC(IW-1:IE-2,:,:) - 2.0*PSRC(IW:IE-1,:,:) + &
                        PSRC(IW+1:IE,:,:))**2 + &
                        1./4 * ( PSRC(IW-1:IE-2,:,:) - 4.0*PSRC(IW:IE-1,:,:) + &
                        3.0*PSRC(IW+1:IE,:,:))**2
!
! WENO weights
!
  ZOMP1(IW+1:IE,:,:) = ZGAMMA1 / (ZEPS + ZBPOS1(IW+1:IE,:,:))**2
  ZOMP2(IW+1:IE,:,:) = ZGAMMA2 / (ZEPS + ZBPOS2(IW+1:IE,:,:))**2
  ZOMP3(IW+1:IE,:,:) = ZGAMMA3 / (ZEPS + ZBPOS3(IW+1:IE,:,:))**2
  ZOMN1(IW+1:IE,:,:) = ZGAMMA1 / (ZEPS + ZBNEG1(IW+1:IE,:,:))**2
  ZOMN2(IW+1:IE,:,:) = ZGAMMA2 / (ZEPS + ZBNEG2(IW+1:IE,:,:))**2
  ZOMN3(IW+1:IE,:,:) = ZGAMMA3 / (ZEPS + ZBNEG3(IW+1:IE,:,:))**2
!
  ZOMP1(IW,  :,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IW,  :,:))**2
  ZOMP2(IW,  :,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IW,  :,:))**2
  ZOMN1(IW,  :,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IW,  :,:))**2
  ZOMN2(IW,  :,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IW,  :,:))**2
  ZOMP1(IW-1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IW-1,:,:))**2
  ZOMP2(IW-1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IW-1,:,:))**2
  ZOMN1(IW-1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IW-1,:,:))**2
  ZOMN2(IW-1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IW-1,:,:))**2
  ZOMP1(IE+1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IE+1,:,:))**2
  ZOMP2(IE+1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IE+1,:,:))**2
  ZOMN1(IE+1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IE+1,:,:))**2
  ZOMN2(IE+1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IE+1,:,:))**2 
!
! WENO fluxes (5th order)
!
  PR(IW+1:IE,:,:) = (ZOMP2(IW+1:IE,:,:)/(ZOMP1(IW+1:IE,:,:)+ZOMP2(IW+1:IE,:,:)+&
                    ZOMP3(IW+1:IE,:,:)) * ZFPOS2(IW+1:IE,:,:) +                &
                    ZOMP1(IW+1:IE,:,:)/(ZOMP1(IW+1:IE,:,:)+ZOMP2(IW+1:IE,:,:) +&
                    ZOMP3(IW+1:IE,:,:)) * ZFPOS1(IW+1:IE,:,:) +                &
                    ZOMP3(IW+1:IE,:,:)/(ZOMP1(IW+1:IE,:,:)+ZOMP2(IW+1:IE,:,:)+ &
                    ZOMP3(IW+1:IE,:,:)) *                                      &
                    ZFPOS3(IW+1:IE,:,:)) * (0.5+SIGN(0.5,PRUCT(IW+1:IE,:,:)))  &
                  + (ZOMN2(IW+1:IE,:,:)/(ZOMN1(IW+1:IE,:,:)+ZOMN2(IW+1:IE,:,:)+&
                     ZOMN3(IW+1:IE,:,:)) *                                     &
                     ZFNEG2(IW+1:IE,:,:)                                       &
                   + ZOMN1(IW+1:IE,:,:)/(ZOMN1(IW+1:IE,:,:)+ZOMN2(IW+1:IE,:,:)+&
                     ZOMN3(IW+1:IE,:,:)) * ZFNEG1(IW+1:IE,:,:)                 &
                   + ZOMN3(IW+1:IE,:,:)/(ZOMN1(IW+1:IE,:,:)+ZOMN2(IW+1:IE,:,:)+&
                     ZOMN3(IW+1:IE,:,:)) * ZFNEG3(IW+1:IE,:,:))                &
                   * (0.5-SIGN(0.5,PRUCT(IW+1:IE,:,:)))
!
! WENO fluxes (3rd order)
!
  PR(IW,:,:) = (ZOMP2(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)) * ZFPOS2(IW,:,:)    &
              + ZOMP1(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)) * ZFPOS1(IW,:,:)) * &
              (0.5+SIGN(0.5,PRUCT(IW,:,:)))                                     &
             + (ZOMN2(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)) * ZFNEG2(IW,:,:)    &
              + ZOMN1(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)) * ZFNEG1(IW,:,:)) * &
              (0.5-SIGN(0.5,PRUCT(IW,:,:)))
!
  PR(IW-1,:,:) = (ZOMP2(IW-1,:,:)/(ZOMP1(IW-1,:,:)+ZOMP2(IW-1,:,:)) *        &
     ZFPOS2(IW-1,:,:)                                                        &
     + ZOMP1(IW-1,:,:)/(ZOMP1(IW-1,:,:)+ZOMP2(IW-1,:,:)) * ZFPOS1(IW-1,:,:)) &
     * (0.5+SIGN(0.5,PRUCT(IW-1,:,:)))                                       &
     + (ZOMN2(IW-1,:,:)/(ZOMN1(IW-1,:,:)+ZOMN2(IW-1,:,:)) * ZFNEG2(IW-1,:,:) &
     + ZOMN1(IW-1,:,:)/(ZOMN1(IW-1,:,:)+ZOMN2(IW-1,:,:)) * ZFNEG1(IW-1,:,:)) &
     * (0.5-SIGN(0.5,PRUCT(IW-1,:,:)))
!
  PR(IE+1,:,:) = (ZOMP2(IE+1,:,:)/(ZOMP1(IE+1,:,:)+ZOMP2(IE+1,:,:)) *        &
     ZFPOS2(IE+1,:,:) +                                                      &
     ZOMP1(IE+1,:,:)/(ZOMP1(IE+1,:,:)+ZOMP2(IE+1,:,:)) * ZFPOS1(IE+1,:,:))   &
     * (0.5+SIGN(0.5,PRUCT(IE+1,:,:)))                                       &
     + (ZOMN2(IE+1,:,:)/(ZOMN1(IE+1,:,:)+ZOMN2(IE+1,:,:)) * ZFNEG2(IE+1,:,:)+&
     ZOMN1(IE+1,:,:)/(ZOMN1(IE+1,:,:)+ZOMN2(IE+1,:,:)) * ZFNEG1(IE+1,:,:))   &
     * (0.5-SIGN(0.5,PRUCT(IE+1,:,:)))
!
!
!       OPEN, WALL, NEST CASE IN THE X DIRECTION
!
CASE ('OPEN','WALL','NEST')
!
  IW=IIB
  IE=IIE
!
!  LATERAL BOUNDARY CONDITIONS
!  AT THE PHYSICAL BORDER: USE A FIRST ORDER UPSTREAM WENO SCHEME AT THE POINTS: IW, IE+1 /AND/ A THIRD ORDER WENO SCHEME AT THE POINTS: IW+1, IE
!  AT THE PROC. BORDER: A THIRD ORDER UPSTREAM WENO SCHEME AT THE POINTS: IW, IE+1  /AND/ A FIFTH ORDER WENO SCHEME AT THE POINTS: IW+1, IE
!
!
!   PHYSICAL BORDER (WEST)
!
  IF(LWEST_ll()) THEN
!
!   FIRST ORDER UPSTREAM WENO SCHEME
!
    PR(IW,:,:) = PSRC(IW-1,:,:) * (0.5+SIGN(0.5,PRUCT(IW,:,:))) + &
                 PSRC(IW,:,:) * (0.5-SIGN(0.5,PRUCT(IW,:,:)))
!
!   THIRD ORDER UPSTREAM WENO SCHEME
!
    ZFPOS1(IW+1,:,:) = 0.5 * (3.0*PSRC(IW,:,:) - PSRC(IW-1,:,:))
    ZFPOS2(IW+1,:,:) = 0.5 * (PSRC(IW,    :,:) + PSRC(IW+1,:,:))
    ZBPOS1(IW+1,:,:) = (PSRC(IW,  :,:) - PSRC(IW-1,:,:))**2
    ZBPOS2(IW+1,:,:) = (PSRC(IW+1,:,:) - PSRC(IW,  :,:))**2
!
    ZFNEG1(IW+1,:,:) = 0.5 * (3.0*PSRC(IW+1,:,:) - PSRC(IW+2,:,:))
    ZFNEG2(IW+1,:,:) = 0.5 * (PSRC(IW+1,    :,:) + PSRC(IW,  :,:))
    ZBNEG1(IW+1,:,:) = (PSRC(IW+1,:,:) - PSRC(IW+2,:,:))**2
    ZBNEG2(IW+1,:,:) = (PSRC(IW,  :,:) - PSRC(IW+1,:,:))**2
!
    ZOMP1(IW+1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IW+1,:,:))**2
    ZOMP2(IW+1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IW+1,:,:))**2
    ZOMN1(IW+1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IW+1,:,:))**2
    ZOMN2(IW+1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IW+1,:,:))**2
!
    PR(IW+1,:,:) = (ZOMP2(IW+1,:,:)/(ZOMP1(IW+1,:,:)+ZOMP2(IW+1,:,:)) *         & 
      ZFPOS2(IW+1,:,:)                                                          & 
      + ZOMP1(IW+1,:,:)/(ZOMP1(IW+1,:,:)+ZOMP2(IW+1,:,:)) * ZFPOS1(IW+1,:,:)) * &
      (0.5+SIGN(0.5,PRUCT(IW+1,:,:)))                                           &
      + (ZOMN2(IW+1,:,:)/(ZOMN1(IW+1,:,:)+ZOMN2(IW+1,:,:)) * ZFNEG2(IW+1,:,:)   &
      + ZOMN1(IW+1,:,:)/(ZOMN1(IW+1,:,:)+ZOMN2(IW+1,:,:)) * ZFNEG1(IW+1,:,:)) * &
      (0.5-SIGN(0.5,PRUCT(IW+1,:,:)))
!
! PROC. BORDER (WEST) 
!
  ELSEIF (NHALO == 1) THEN
!
!   THIRD ORDER UPSTREAM WENO SCHEME
!
    ZFPOS1(IW,:,:) = 0.5 * (3.0*PSRC(IW-1,:,:) - TPHALO2%WEST(:,:))
    ZFPOS2(IW,:,:) = 0.5 * (PSRC(IW-1,    :,:)    + PSRC(IW,:,:))
    ZBPOS1(IW,:,:) = (PSRC(IW-1,:,:) - TPHALO2%WEST(:,:))**2
    ZBPOS2(IW,:,:) = (PSRC(IW,  :,:) - PSRC(IW-1,:,:))**2
!
    ZFNEG1(IW,:,:) = 0.5 * (3.0*PSRC(IW,:,:) - PSRC(IW+1,:,:))
    ZFNEG2(IW,:,:) = 0.5 * (PSRC(IW,    :,:) + PSRC(IW-1,:,:))
    ZBNEG1(IW,:,:) = (PSRC(IW,  :,:) - PSRC(IW+1,:,:))**2
    ZBNEG2(IW,:,:) = (PSRC(IW-1,:,:) - PSRC(IW,  :,:))**2
!
    ZOMP1(IW,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IW,:,:))**2
    ZOMP2(IW,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IW,:,:))**2
    ZOMN1(IW,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IW,:,:))**2
    ZOMN2(IW,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IW,:,:))**2
!
    PR(IW,:,:) = (ZOMP2(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)) * ZFPOS2(IW,:,:) &
                + ZOMP1(IW,:,:)/(ZOMP1(IW,:,:)+ZOMP2(IW,:,:)) * ZFPOS1(IW,:,:))&
                                               * (0.5+SIGN(0.5,PRUCT(IW,:,:))) &
               + (ZOMN2(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)) * ZFNEG2(IW,:,:) &
                + ZOMN1(IW,:,:)/(ZOMN1(IW,:,:)+ZOMN2(IW,:,:)) * ZFNEG1(IW,:,:))&
                                               * (0.5-SIGN(0.5,PRUCT(IW,:,:)))
!
!   FIFTH ORDER UPSTREAM WENO SCHEME
!
    ZFPOS1(IW+1,:,:) = 1./6. *(2.0*TPHALO2%WEST(:,:)-7.0*PSRC(IW-1,:,:)+ &
                       11.0*PSRC(IW, :,:))
    ZFPOS2(IW+1,:,:) = 1./6. *(-PSRC(IW-1,  :,:)+ 5.0*PSRC(IW,  :,:)+    &
                       2.0*PSRC(IW+1,:,:))
    ZFPOS3(IW+1,:,:) = 1./6. *(2.0*PSRC(IW,  :,:)+5.0*PSRC(IW+1,:,:)-    &
                       PSRC(IW+2,    :,:))
!
    ZBPOS1(IW+1,:,:) = 13./12. *(TPHALO2%WEST(:,:)-2.0*PSRC(IW-1,:,:)+   &
                       PSRC(IW,:,:))**2 &
                       + 1./4. *(TPHALO2%WEST(:,:)-4.0*PSRC(IW-1,:,:)+   &
                       3.0*PSRC(IW,:,:))**2    
    ZBPOS2(IW+1,:,:) = 13./12. *(PSRC(IW-1,:,:)   -2.0*PSRC(IW,:,:)+     &
                       PSRC(IW+1,:,:))**2 &
                       + 1./4. *(PSRC(IW-1,:,:) - PSRC(IW+1,:,:))**2
    ZBPOS3(IW+1,:,:) = 13./12. *(PSRC(IW,:,:) - 2.0*PSRC(IW+1,:,:) +     &
                       PSRC(IW+2,:,:))**2 &
                       + 1./4. *(3.0*PSRC(IW,:,:) - 4.0*PSRC(IW+1,:,:) + &
                       PSRC(IW+2,:,:))**2
!
    ZFNEG1(IW+1,:,:) = 1./6 * (11.0*PSRC(IW+1,:,:) - 7.0*PSRC(IW+2,:,:) + &
                       2.0*PSRC(IW+3,:,:))
    ZFNEG2(IW+1,:,:) = 1./6 * (2.0*PSRC(IW, :,:) + 5.0*PSRC(IW+1,:,:) -   &
                       PSRC(IW+2,    :,:))
    ZFNEG3(IW+1,:,:) = 1./6 * (-PSRC(IW-1  ,:,:) + 5.0*PSRC(IW,  :,:) +   &
                       2.0*PSRC(IW+1,:,:))
!
    ZBNEG1(IW+1,:,:) = 13./12 * (PSRC(IW+1,:,:) - 2.0*PSRC(IW+2,:,:) +    &
                       PSRC(IW+3,:,:))**2 &
                       + 1./4 * (3.0*PSRC(IW+1,:,:) - 4.0*PSRC(IW+2,:,:) +&
                       PSRC(IW+3,:,:))**2
    ZBNEG2(IW+1,:,:) = 13./12 * (PSRC(IW,:,:) - 2.0*PSRC(IW+1,:,:) +      &
                       PSRC(IW+2,:,:))**2 &
                       + 1./4 * (PSRC(IW,:,:) - PSRC(IW+2,:,:))**2
    ZBNEG3(IW+1,:,:) = 13./12 * (PSRC(IW-1,:,:) - 2.0*PSRC(IW,:,:) +      &
                       PSRC(IW+1,:,:))**2 &
                       + 1./4 * (PSRC(IW-1,:,:) - 4.0*PSRC(IW,:,:) +      &
                       3.0*PSRC(IW+1,:,:))**2
!
    ZOMP1(IW+1,:,:) = ZGAMMA1 / (ZEPS + ZBPOS1(IW+1,:,:))**2
    ZOMP2(IW+1,:,:) = ZGAMMA2 / (ZEPS + ZBPOS2(IW+1,:,:))**2
    ZOMP3(IW+1,:,:) = ZGAMMA3 / (ZEPS + ZBPOS3(IW+1,:,:))**2
    ZOMN1(IW+1,:,:) = ZGAMMA1 / (ZEPS + ZBNEG1(IW+1,:,:))**2
    ZOMN2(IW+1,:,:) = ZGAMMA2 / (ZEPS + ZBNEG2(IW+1,:,:))**2
    ZOMN3(IW+1,:,:) = ZGAMMA3 / (ZEPS + ZBNEG3(IW+1,:,:))**2
!
      PR(IW+1,:,:) = (ZOMP2(IW+1,:,:)/(ZOMP1(IW+1,:,:)+ZOMP2(IW+1,:,:)+ &
                     ZOMP3(IW+1,:,:)) * ZFPOS2(IW+1,:,:)   &
                   + ZOMP1(IW+1,:,:)/(ZOMP1(IW+1,:,:)+ZOMP2(IW+1,:,:)+ &
                     ZOMP3(IW+1,:,:)) * ZFPOS1(IW+1,:,:)    &
                   + ZOMP3(IW+1,:,:)/(ZOMP1(IW+1,:,:)+ZOMP2(IW+1,:,:)+ &
                     ZOMP3(IW+1,:,:)) * ZFPOS3(IW+1,:,:)) * &
                   (0.5+SIGN(0.5,PRUCT(IW+1,:,:)))                     &
                  + (ZOMN2(IW+1,:,:)/(ZOMN1(IW+1,:,:)+ZOMN2(IW+1,:,:)+ &
                     ZOMN3(IW+1,:,:)) * ZFNEG2(IW+1,:,:)    &
                   + ZOMN1(IW+1,:,:)/(ZOMN1(IW+1,:,:)+ZOMN2(IW+1,:,:)+ &
                     ZOMN3(IW+1,:,:)) * ZFNEG1(IW+1,:,:)    &
                   + ZOMN3(IW+1,:,:)/(ZOMN1(IW+1,:,:)+ZOMN2(IW+1,:,:)+ &
                     ZOMN3(IW+1,:,:)) * ZFNEG3(IW+1,:,:)) * &
                   (0.5-SIGN(0.5,PRUCT(IW+1,:,:)))
!
  ENDIF
!
! PHYSICAL BORDER (EAST)
!
  IF(LEAST_ll()) THEN
    PR(IE+1,:,:) = PSRC(IE,:,:) * (0.5+SIGN(0.5,PRUCT(IE+1,:,:))) + &
                   PSRC(IE+1,:,:) * (0.5-SIGN(0.5,PRUCT(IE+1,:,:)))
!
    ZFPOS1(IE,:,:) = 0.5 * (3.0*PSRC(IE-1,:,:) - PSRC(IE-2,:,:))
    ZFPOS2(IE,:,:) = 0.5 * (PSRC(IE,      :,:) + PSRC(IE-1,:,:))
    ZBPOS1(IE,:,:) = (PSRC(IE-1,:,:) - PSRC(IE-2,:,:))**2
    ZBPOS2(IE,:,:) = (PSRC(IE,  :,:) - PSRC(IE-1,:,:))**2
!
    ZFNEG1(IE,:,:) = 0.5 * (3.0*PSRC(IE,:,:) - PSRC(IE+1,:,:))
    ZFNEG2(IE,:,:) = 0.5 * (PSRC(IE,    :,:) + PSRC(IE-1,:,:))
    ZBNEG1(IE,:,:) = (PSRC(IE,  :,:) - PSRC(IE+1,:,:))**2
    ZBNEG2(IE,:,:) = (PSRC(IE-1,:,:) - PSRC(IE,  :,:))**2
!
    ZOMP1(IE,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IE,:,:))**2
    ZOMP2(IE,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IE,:,:))**2
    ZOMN1(IE,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IE,:,:))**2
    ZOMN2(IE,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IE,:,:))**2
!
    PR(IE,:,:) = (ZOMP2(IE,:,:)/(ZOMP1(IE,:,:)+ZOMP2(IE,:,:)) * ZFPOS2(IE,:,:) + &
                  ZOMP1(IE,:,:)/(ZOMP1(IE,:,:)+ZOMP2(IE,:,:)) * ZFPOS1(IE,:,:)) *&
                  (0.5+SIGN(0.5,PRUCT(IE,:,:))) &
               + (ZOMN2(IE,:,:)/(ZOMN1(IE,:,:)+ZOMN2(IE,:,:)) * ZFNEG2(IE,:,:) + &
                  ZOMN1(IE,:,:)/(ZOMN1(IE,:,:)+ZOMN2(IE,:,:)) * ZFNEG1(IE,:,:)) *&
                  (0.5-SIGN(0.5,PRUCT(IE,:,:)))
!
! PROC. BORDER (EAST) 
!
  ELSEIF(NHALO == 1) THEN
    ZFPOS1(IE+1,:,:) = 0.5 * (3.0*PSRC(IE,:,:) - PSRC(IE-1,:,:))
    ZFPOS2(IE+1,:,:) = 0.5 * (PSRC(IE+1,  :,:) + PSRC(IE,  :,:))
    ZBPOS1(IE+1,:,:) = (PSRC(IE,  :,:) - PSRC(IE-1,:,:))**2
    ZBPOS2(IE+1,:,:) = (PSRC(IE+1,:,:) - PSRC(IE,  :,:))**2
!
    ZFNEG1(IE+1,:,:) = 0.5 * (3.0*PSRC(IE+1,:,:) - TPHALO2%EAST(:,:))
    ZFNEG2(IE+1,:,:) = 0.5 * (PSRC(IE+1,    :,:) + PSRC(IE,     :,:))
    ZBNEG1(IE+1,:,:) = (PSRC(IE+1,:,:) - TPHALO2%EAST(:,:))**2
    ZBNEG2(IE+1,:,:) = (PSRC(IE,  :,:) - PSRC(IE+1,   :,:))**2
!
    ZOMP1(IE+1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(IE+1,:,:))**2
    ZOMP2(IE+1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(IE+1,:,:))**2
    ZOMN1(IE+1,:,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(IE+1,:,:))**2
    ZOMN2(IE+1,:,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(IE+1,:,:))**2
!
    PR(IE+1,:,:) = (ZOMP2(IE+1,:,:)/(ZOMP1(IE+1,:,:)+ZOMP2(IE+1,:,:)) * &
                    ZFPOS2(IE+1,:,:) +                                  &
                  ZOMP1(IE+1,:,:)/(ZOMP1(IE+1,:,:)+ZOMP2(IE+1,:,:)) *   &
                  ZFPOS1(IE+1,:,:)) * (0.5+SIGN(0.5,PRUCT(IE+1,:,:)))   &
               + (ZOMN2(IE+1,:,:)/(ZOMN1(IE+1,:,:)+ZOMN2(IE+1,:,:)) *   &
                ZFNEG2(IE+1,:,:) +                                      &
                  ZOMN1(IE+1,:,:)/(ZOMN1(IE+1,:,:)+ZOMN2(IE+1,:,:)) *   &
                  ZFNEG1(IE+1,:,:)) * (0.5-SIGN(0.5,PRUCT(IE+1,:,:)))
!
     ZFPOS1(IE,:,:) = 1./6 * (2.0*PSRC(IE-3,:,:) - 7.0*PSRC(IE-2,:,:) + &
                      11.0*PSRC(IE-1,:,:))
     ZFPOS2(IE,:,:) = 1./6 * (-1.0*PSRC(IE-2,:,:) + 5.0*PSRC(IE-1,:,:) + &
                      2.0*PSRC(IE,:,:))
     ZFPOS3(IE,:,:) = 1./6 * (2.0*PSRC(IE-1,:,:) + 5.0*PSRC(IE,:,:) -    &
                      PSRC(IE+1,:,:))
!
     ZBPOS1(IE,:,:) = 13./12 * (PSRC(IE-3,:,:) - 2.0*PSRC(IE-2,:,:) + &
                      PSRC(IE-1,:,:))**2 + 1./4 * (PSRC(IE-3,:,:) &
                      - 4.0*PSRC(IE-2,:,:) + 3.0*PSRC(IE-1,:,:))**2
     ZBPOS2(IE,:,:) = 13./12 * (PSRC(IE-2,:,:) - 2.0*PSRC(IE-1,:,:) + &
                      PSRC(IE,:,:))**2 + 1./4 * &
                     (PSRC(IE-2,:,:) - PSRC(IE,:,:))**2
     ZBPOS3(IE,:,:) = 13./12 * (PSRC(IE-1,:,:) - 2.0*PSRC(IE,:,:) + &
                      PSRC(IE+1,:,:))**2 + 1./4 * &
                     ( 3.0*PSRC(IE-1,:,:) - 4.0*PSRC(IE,:,:) + PSRC(IE+1,:,:))**2
!
     ZFNEG1(IE,:,:) = 1./6 * (11.0*PSRC(IE,  :,:) - 7.0*PSRC(IE+1,:,:) + &
                      2.0*TPHALO2%EAST(:,:))
     ZFNEG2(IE,:,:) = 1./6 * (2.0*PSRC(IE-1, :,:) + 5.0*PSRC(IE,  :,:) - &
                      PSRC(IE+1,:,:))
     ZFNEG3(IE,:,:) = 1./6 * (-1.0*PSRC(IE-2,:,:) + 5.0*PSRC(IE-1,:,:) + &
                      2.0*PSRC(IE,:,:))
!
     ZBNEG1(IE,:,:) = 13./12 * (PSRC(IE,:,:) - 2.0*PSRC(IE+1,:,:) +      &
       TPHALO2%EAST(:,:))**2 + 1./4 * &
       ( 3.0*PSRC(IE,:,:) - 4.0*PSRC(IE+1,:,:) + TPHALO2%EAST(:,:))**2
     ZBNEG2(IE,:,:) = 13./12 * (PSRC(IE-1,:,:) - 2.0*PSRC(IE,:,:) +      &
        PSRC(IE+1,:,:))**2 + 1./4 * &
        (PSRC(IE-1,:,:) - PSRC(IE+1,:,:))**2
     ZBNEG3(IE,:,:) = 13./12 * (PSRC(IE-2,:,:) - 2.0*PSRC(IE-1,:,:) +    &
        PSRC(IE,:,:))**2 + 1./4 * &
                      ( PSRC(IE-2,:,:) - 4.0*PSRC(IE-1,:,:) +            &
                      3.0*PSRC(IE,:,:))**2
!
     ZOMP1(IE,:,:) = ZGAMMA1 / (ZEPS + ZBPOS1(IE,:,:))**2
     ZOMP2(IE,:,:) = ZGAMMA2 / (ZEPS + ZBPOS2(IE,:,:))**2
     ZOMP3(IE,:,:) = ZGAMMA3 / (ZEPS + ZBPOS3(IE,:,:))**2
     ZOMN1(IE,:,:) = ZGAMMA1 / (ZEPS + ZBNEG1(IE,:,:))**2
     ZOMN2(IE,:,:) = ZGAMMA2 / (ZEPS + ZBNEG2(IE,:,:))**2
     ZOMN3(IE,:,:) = ZGAMMA3 / (ZEPS + ZBNEG3(IE,:,:))**2
!
       PR(IE,:,:) = (ZOMP2(IE,:,:)/(ZOMP1(IE,:,:)+ZOMP2(IE,:,:)+ &
                   ZOMP3(IE,:,:)) * ZFPOS2(IE,:,:)    &
                   + ZOMP1(IE,:,:)/(ZOMP1(IE,:,:)+ZOMP2(IE,:,:)+ &
                   ZOMP3(IE,:,:)) * ZFPOS1(IE,:,:)    &
                   + ZOMP3(IE,:,:)/(ZOMP1(IE,:,:)+ZOMP2(IE,:,:)+ &
                   ZOMP3(IE,:,:)) * ZFPOS3(IE,:,:)) * &
                   (0.5+SIGN(0.5,PRUCT(IE,:,:)))                 &
                  + (ZOMN2(IE,:,:)/(ZOMN1(IE,:,:)+ZOMN2(IE,:,:)+ &
                   ZOMN3(IE,:,:)) * ZFNEG2(IE,:,:)    &
                   + ZOMN1(IE,:,:)/(ZOMN1(IE,:,:)+ZOMN2(IE,:,:)+ &
                   ZOMN3(IE,:,:)) * ZFNEG1(IE,:,:)    &
                   + ZOMN3(IE,:,:)/(ZOMN1(IE,:,:)+ZOMN2(IE,:,:)+ &
                   ZOMN3(IE,:,:)) * ZFNEG3(IE,:,:)) * &
                   (0.5-SIGN(0.5,PRUCT(IE,:,:)))
!
  ENDIF
!
!        USE A FIFTH ORDER UPSTREAM WENO SCHEME ELSEWHERE (IW+2 --> IE-1)
!
  ZFPOS1(IW+2:IE-1,:,:) = 1./6 * (2.0*PSRC(IW-1:IE-4,:,:) - &
          7.0*PSRC(IW:IE-3,  :,:) + 11.0*PSRC(IW+1:IE-2,:,:))
  ZFPOS2(IW+2:IE-1,:,:) = 1./6 * (-1.0*PSRC(IW:IE-3, :,:) + &
          5.0*PSRC(IW+1:IE-2,:,:) + 2.0*PSRC(IW+2:IE-1, :,:))
  ZFPOS3(IW+2:IE-1,:,:) = 1./6 * (2.0*PSRC(IW+1:IE-2,:,:) + &
          5.0*PSRC(IW+2:IE-1,:,:) - PSRC(IW+3:IE,       :,:))
!
  ZBPOS1(IW+2:IE-1,:,:) = 13./12 * (PSRC(IW-1:IE-4,:,:) -   &
          2.0*PSRC(IW:IE-3,:,:) + PSRC(IW+1:IE-2,:,:))**2 + &
          1./4 * (PSRC(IW-1:IE-4,:,:) - 4.0*PSRC(IW:IE-3,:,:) + &
          3.0*PSRC(IW+1:IE-2,:,:))**2
  ZBPOS2(IW+2:IE-1,:,:) = 13./12 * (PSRC(IW:IE-3,:,:) -     &
          2.0*PSRC(IW+1:IE-2,:,:) + PSRC(IW+2:IE-1,:,:))**2 + &
          1./4 * (PSRC(IW:IE-3,:,:) - PSRC(IW+2:IE-1,:,:))**2
  ZBPOS3(IW+2:IE-1,:,:) = 13./12 * (PSRC(IW+1:IE-2,:,:) -   &
          2.0*PSRC(IW+2:IE-1,:,:) + PSRC(IW+3:IE,:,:))**2 + &
          1./4 * (3.0*PSRC(IW+1:IE-2,:,:) - 4.0*PSRC(IW+2:IE-1,:,:) + &
          PSRC(IW+3:IE,:,:))**2
!
  ZFNEG1(IW+2:IE-1,:,:) = 1./6 * (11.0*PSRC(IW+2:IE-1,:,:) - &
          7.0*PSRC(IW+3:IE,:,:) + 2.0*PSRC(IW+4:IE+1,:,:))
  ZFNEG2(IW+2:IE-1,:,:) = 1./6 * (2.0*PSRC(IW+1:IE-2,:,:) +  &
          5.0*PSRC(IW+2:IE-1,:,:) - PSRC(IW+3:IE,:,:))
  ZFNEG3(IW+2:IE-1,:,:) = 1./6 * (-1.0*PSRC(IW:IE-3,:,:) +   &
          5.0*PSRC(IW+1:IE-2,:,:) + 2.0*PSRC(IW+2:IE-1,:,:))
!
  ZBNEG1(IW+2:IE-1,:,:) = 13./12 * (PSRC(IW+2:IE-1,:,:) -    &
          2.0*PSRC(IW+3:IE,:,:) + PSRC(IW+4:IE+1,:,:))**2 + &
          1./4 * ( 3.0*PSRC(IW+2:IE-1,:,:) - 4.0*PSRC(IW+3:IE,:,:) + &
          PSRC(IW+4:IE+1,:,:))**2
  ZBNEG2(IW+2:IE-1,:,:) = 13./12 * (PSRC(IW+1:IE-2,:,:) - &
          2.0*PSRC(IW+2:IE-1,:,:) + PSRC(IW+3:IE,:,:))**2 + &
          1./4 * (PSRC(IW+1:IE-2,:,:) - PSRC(IW+3:IE,:,:))**2
  ZBNEG3(IW+2:IE-1,:,:) = 13./12 * (PSRC(IW:IE-3,:,:) - &
          2.0*PSRC(IW+1:IE-2,:,:) + PSRC(IW+2:IE-1,:,:))**2 + &
          1./4 * ( PSRC(IW:IE-3,:,:) - 4.0*PSRC(IW+1:IE-2,:,:) + &
          3.0*PSRC(IW+2:IE-1,:,:))**2
!
  ZOMP1(IW+2:IE-1,:,:) = ZGAMMA1 / (ZEPS + ZBPOS1(IW+2:IE-1,:,:))**2
  ZOMP2(IW+2:IE-1,:,:) = ZGAMMA2 / (ZEPS + ZBPOS2(IW+2:IE-1,:,:))**2
  ZOMP3(IW+2:IE-1,:,:) = ZGAMMA3 / (ZEPS + ZBPOS3(IW+2:IE-1,:,:))**2
  ZOMN1(IW+2:IE-1,:,:) = ZGAMMA1 / (ZEPS + ZBNEG1(IW+2:IE-1,:,:))**2
  ZOMN2(IW+2:IE-1,:,:) = ZGAMMA2 / (ZEPS + ZBNEG2(IW+2:IE-1,:,:))**2
  ZOMN3(IW+2:IE-1,:,:) = ZGAMMA3 / (ZEPS + ZBNEG3(IW+2:IE-1,:,:))**2
!
    PR(IW+2:IE-1,:,:) = (ZOMP2(IW+2:IE-1,:,:)/(ZOMP1(IW+2:IE-1,:,:)+   &
      ZOMP2(IW+2:IE-1,:,:)+                                            &
      ZOMP3(IW+2:IE-1,:,:)) * ZFPOS2(IW+2:IE-1,:,:) +                  &
      ZOMP1(IW+2:IE-1,:,:)/(ZOMP1(IW+2:IE-1,:,:)+ZOMP2(IW+2:IE-1,:,:)+ &
      ZOMP3(IW+2:IE-1,:,:)) * ZFPOS1(IW+2:IE-1,:,:)  +                 &
      ZOMP3(IW+2:IE-1,:,:)/(ZOMP1(IW+2:IE-1,:,:)+ZOMP2(IW+2:IE-1,:,:)+ &
      ZOMP3(IW+2:IE-1,:,:)) * ZFPOS3(IW+2:IE-1,:,:)) *                 &
      (0.5+SIGN(0.5,PRUCT(IW+2:IE-1,:,:))) +                           &
      (ZOMN2(IW+2:IE-1,:,:)/(ZOMN1(IW+2:IE-1,:,:)+ZOMN2(IW+2:IE-1,:,:)+&
      ZOMN3(IW+2:IE-1,:,:)) * ZFNEG2(IW+2:IE-1,:,:)  +                 &
      ZOMN1(IW+2:IE-1,:,:)/(ZOMN1(IW+2:IE-1,:,:)+ZOMN2(IW+2:IE-1,:,:)+ &
      ZOMN3(IW+2:IE-1,:,:)) * ZFNEG1(IW+2:IE-1,:,:) +                  &
      ZOMN3(IW+2:IE-1,:,:)/(ZOMN1(IW+2:IE-1,:,:)+ZOMN2(IW+2:IE-1,:,:)+ &
      ZOMN3(IW+2:IE-1,:,:)) * ZFNEG3(IW+2:IE-1,:,:)) *                 &
      (0.5-SIGN(0.5,PRUCT(IW+2:IE-1,:,:)))
!
END SELECT
!
PR = PR * PRUCT
!
END SUBROUTINE ADVEC_WENO_K_3_MX
!
!-------------------------------------------------------------------------------
!
!     ########################################################################
      SUBROUTINE ADVEC_WENO_K_3_MY(HLBCY,PSRC, PRVCT, PR, TPHALO2)
!     ########################################################################
!!
!!****  Computes PRVCT * PUT (or PRVCT * PWT). Upstream fluxes of U (or W) 
!!      variables in Y direction.  
!!      Input PUT is on U Grid 'ie' (i,j,k) based on UGRID reference
!!      Output PR is on mass Grid 'ie' (i,j-1/2,k) based on UGRID reference 
!!
!!    AUTHOR
!!    ------
!!    F. Visentin   *CNRS/LA*                 
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
USE MODE_ll
USE MODD_LUNIT
USE MODD_CONF
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY  ! Y direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on U grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRVCT ! contrav. comp. on MASS GRID
!
! output source term
!
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PR
TYPE(HALO2_ll), OPTIONAL, POINTER :: TPHALO2      ! halo2 for the field at t
!
!*       0.2   Declarations of local variables :
!
INTEGER :: IIB,IJB    ! Begining useful area in x,y,z directions
INTEGER :: IIE,IJE    ! End useful area in x,y,z directions
INTEGER::  IS,IN      ! Coordinate of third order diffusion area
!
INTEGER:: ILUOUT,IRESP   ! for prints
!
!
! intermediate reconstruction fluxes for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFPOS1, ZFPOS2, ZFPOS3
!
! intermediate reconstruction fluxes for negative wind case
! we need only one since ZFNEG2 = ZFPOS2
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFNEG1, ZFNEG2, ZFNEG3
!
! smoothness indicators for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBPOS1, ZBPOS2, ZBPOS3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBNEG1, ZBNEG2, ZBNEG3
!
! WENO weights 
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZOMP1, ZOMP2, ZOMP3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZOMN1, ZOMN2, ZOMN3
!
! standard weights
!
REAL, PARAMETER :: ZGAMMA1 = 1./10.
REAL, PARAMETER :: ZGAMMA2 = 3./5.
REAL, PARAMETER :: ZGAMMA3 = 3./10.
REAL, PARAMETER :: ZGAMMA1_PRIM = 1./3.
REAL, PARAMETER :: ZGAMMA2_PRIM = 2./3.
!
REAL, PARAMETER :: ZEPS = 1.0E-15
!
!-----------------------------------------------------------------------------
!
!*       0.3.     COMPUTES THE DOMAIN DIMENSIONS
!                 ------------------------------
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
!---------------------------------------------------------------------------
!
!*       0.4.   INITIALIZE THE FIELD 
!               ---------------------
!
PR(:,:,:) = 0.0
!
ZFPOS1 = 0.0
ZFPOS2 = 0.0
ZFPOS3 = 0.0
ZFNEG1 = 0.0
ZFNEG2 = 0.0
ZFNEG3 = 0.0
ZBPOS1 = 0.0
ZBPOS2 = 0.0
ZBPOS3 = 0.0
ZBNEG1 = 0.0
ZBNEG2 = 0.0
ZBNEG3 = 0.0
ZOMP1  = 0.0
ZOMP2  = 0.0
ZOMP3  = 0.0
ZOMN1  = 0.0
ZOMN2  = 0.0
ZOMN3  = 0.0 
!
!-------------------------------------------------------------------------------
!
SELECT CASE ( HLBCY(1) ) ! 
!
!*       1.1    CYCLIC CASE IN THE Y DIRECTION:
!
CASE ('CYCL')          ! In that case one must have HLBCY(1) == HLBCY(2)
!
  IF(NHALO == 1) THEN
    IS=IJB
    IN=IJE
  ELSE
      CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT,IRESP)
      WRITE(ILUOUT,*) 'ERROR : 4th order advection in CYCLic case '
      WRITE(ILUOUT,*) 'cannot be used with NHALO=2'
      CALL ABORT
      STOP
  END IF
!
! Same explanation than for the subroutine ADVEC_WENO_K_3_MX
!
! intermediate fluxes for positive wind case
!
  ZFPOS1(:,IS+2:IN,:) = 1./6 * (2.0*PSRC(:,IS-1:IN-3,:) - &
   7.0*PSRC(:,IS:IN-2,:) + 11.0*PSRC(:,IS+1:IN-1,:))
  ZFPOS1(:,IS+1,   :) = 1./6 * (2.0*TPHALO2%SOUTH(:,:)  - &
   7.0*PSRC(:,IS-1,   :) + 11.0*PSRC(:,IS,       :))
  ZFPOS1(:,IN+1,:) = 0.5 * (3.0*PSRC(:,IN,:) - PSRC(:,IN-1,:))
  ZFPOS1(:,IS,       :) = 0.5 * (3.0*PSRC(:,IS-1, :) - TPHALO2%SOUTH(:,:))
  ZFPOS1(:,IS-1,     :) = - 999.
!
!
  ZFPOS2(:,IS+1:IN,:) = 1./6 * (-1.0*PSRC(:,IS-1:IN-2,:) + &
   5.0*PSRC(:,IS:IN-1,:) + 2.0*PSRC(:,IS+1:IN,:))
  ZFPOS2(:,IN+1,:) = 0.5 * (PSRC(:,IN,  :) + PSRC(:,IN+1,:))
  ZFPOS2(:,IS,  :) = 0.5 * (PSRC(:,IS-1,:) + PSRC(:,IS,  :))
  ZFPOS2(:,IS-1,:) = 0.5 * (TPHALO2%SOUTH(:,:) + PSRC(:,IS-1,:))
!
!
  ZFPOS3(:,IS+1:IN,:) = 1./6 * (2.0*PSRC(:,IS:IN-1,:) + &
   5.0*PSRC(:,IS+1:IN,:) - 1.0*PSRC(:,IS+2:IN+1,:))
!
! intermediate flux for negative wind case
!
  ZFNEG1(:,IS+1:IN-1,:) = 1./6 * (11.0*PSRC(:,IS+1:IN-1,:) - &
   7.0*PSRC(:,IS+2:IN,:) + 2.0*PSRC(:,IS+3:IN+1,:)) 
  ZFNEG1(:,IN,       :) = 1./6 * (11.0*PSRC(:,IN,       :) - &
   7.0*PSRC(:,IN+1,   :) + 2.0*TPHALO2%NORTH(:,:))
  ZFNEG1(:,IS,  :) = 0.5 * (3.0*PSRC(:,IS,  :) - PSRC(:,IS+1,:))
  ZFNEG1(:,IS-1,:) = 0.5 * (3.0*PSRC(:,IS-1,:) - PSRC(:,IS,  :))
  ZFNEG1(:,IN+1,:) = 0.5 * (3.0*PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))
!
!
  ZFNEG2(:,IS+1:IN,:) = 1./6 * (2.0*PSRC(:,IS:IN-1,:) + &
   5.0*PSRC(:,IS+1:IN,:) - 1.0*PSRC(:,IS+2:IN+1,:))
  ZFNEG2(:,IN+1,:) = 0.5 * (PSRC(:,IN+1,:) + PSRC(:,IN,  :))
  ZFNEG2(:,IS,  :) = 0.5 * (PSRC(:,IS,  :) + PSRC(:,IS-1,:))
  ZFNEG2(:,IS-1,:) = 0.5 * (PSRC(:,IS-1,:) + TPHALO2%SOUTH(:,:))
! 
!
  ZFNEG3(:,IS+1:IN,:) = 1./6 * (-1.0*PSRC(:,IS-1:IN-2,:) + &
   5.0*PSRC(:,IS:IN-1,:) + 2.0*PSRC(:,IS+1:IN,:))
!
! smoothness indicators for positive wind case
!
  ZBPOS1(:,IS+2:IN,:) = 13./12 * (PSRC(:,IS-1:IN-3,:) - 2.0*PSRC(:,IS:IN-2,:) + &
   PSRC(:,IS+1:IN-1,:))**2 + 1./4 * (PSRC(:,IS-1:IN-3,:) - 4.0*PSRC(:,IS:IN-2,:) +&
   3.0*PSRC(:,IS+1:IN-1,:))**2
  ZBPOS1(:,IS+1,:) = 13./12 * (TPHALO2%SOUTH(:,:) - 2.0*PSRC(:,IS-1,:) + &
   PSRC(:,IS,:))**2 + &
   1./4 * (TPHALO2%SOUTH(:,:) - 4.0*PSRC(:,IS-1,:) + 3.0*PSRC(:,IS,:))**2
  ZBPOS1(:,IN+1,:) = (PSRC(:,IN,  :) - PSRC(:,IN-1,:))**2
  ZBPOS1(:,IS,  :) = (PSRC(:,IS-1,:) - TPHALO2%SOUTH(:,:))**2
  ZBPOS1(:,IS-1,:) = - 999. 
!
  ZBPOS2(:,IS+1:IN,:) = 13./12 * (PSRC(:,IS-1:IN-2,:) - 2.0*PSRC(:,IS:IN-1,:) + &
   PSRC(:,IS+1:IN,:))**2 + 1./4 * (PSRC(:,IS-1:IN-2,:) - PSRC(:,IS+1:IN,:))**2
  ZBPOS2(:,IN+1,:) = (PSRC(:,IN+1,:) - PSRC(:,IN,  :))**2
  ZBPOS2(:,IS-1,:) = (PSRC(:,IS-1,:) - TPHALO2%SOUTH(:,:))**2
  ZBPOS2(:,IS,  :) = (PSRC(:,IS,  :) - PSRC(:,IS-1,:))**2
!
!
  ZBPOS3(:,IS+1:IN,:) = 13./12 * (PSRC(:,IS:IN-1,:) - 2.0*PSRC(:,IS+1:IN,:) + &
   PSRC(:,IS+2:IN+1,:))**2 + 1./4 * ( 3.0*PSRC(:,IS:IN-1,:) - 4.0*PSRC(:,IS+1:IN,:) + PSRC(:,IS+2:IN+1,:))**2
!
! smoothness indicators for negative wind case
!
  ZBNEG1(:,IS+1:IN-1,:) = 13./12 * (PSRC(:,IS+1:IN-1,:) - 2.0*PSRC(:,IS+2:IN,:) + &
   PSRC(:,IS+3:IN+1,:))**2 + 1./4 * ( 3.0*PSRC(:,IS+1:IN-1,:) -                   &
   4.0*PSRC(:,IS+2:IN,:) + PSRC(:,IS+3:IN+1,:))**2 
  ZBNEG1(:,IN,       :) = 13./12 * (PSRC(:,IN,:) - 2.0*PSRC(:,IN+1,:) +           &
   TPHALO2%NORTH(:,:))**2 + &
   1./4 * ( 3.0*PSRC(:,IN,:) - 4.0*PSRC(:,IN+1,:) + TPHALO2%NORTH(:,:))**2
  ZBNEG1(:,IS-1,:) = (PSRC(:,IS-1,:) - PSRC(:,IS,:))**2
  ZBNEG1(:,IS,  :) = (PSRC(:,IS,  :) - PSRC(:,IS+1,:))**2
  ZBNEG1(:,IN+1,:) = (PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))**2
!
!
  ZBNEG2(:,IS+1:IN,:) = 13./12 * (PSRC(:,IS:IN-1,:) - 2.0*PSRC(:,IS+1:IN,:) + &
   PSRC(:,IS+2:IN+1,:))**2 + &
   1./4 * (PSRC(:,IS:IN-1,:) - PSRC(:,IS+2:IN+1,:))**2
  ZBNEG2(:,IN+1,:) = (PSRC(:,IN  ,:) - PSRC(:,IN+1,:))**2
  ZBNEG2(:,IS,  :) = (PSRC(:,IS-1,:) - PSRC(:,IS,  :))**2
  ZBNEG2(:,IS-1,:) = (TPHALO2%SOUTH(:,:) - PSRC(:,IS-1,:))**2
!
!
  ZBNEG3(:,IS+1:IN,:) = 13./12 * (PSRC(:,IS-1:IN-2,:) - 2.0*PSRC(:,IS:IN-1,:) + &
   PSRC(:,IS+1:IN,:))**2 + &
   1./4 * ( PSRC(:,IS-1:IN-2,:) - 4.0*PSRC(:,IS:IN-1,:) + 3.0*PSRC(:,IS+1:IN,:))**2 
!
! WENO weights
!
  ZOMP1(:,IS+1:IN,:) = ZGAMMA1 / (ZEPS + ZBPOS1(:,IS+1:IN,:))**2
  ZOMP2(:,IS+1:IN,:) = ZGAMMA2 / (ZEPS + ZBPOS2(:,IS+1:IN,:))**2
  ZOMP3(:,IS+1:IN,:) = ZGAMMA3 / (ZEPS + ZBPOS3(:,IS+1:IN,:))**2
  ZOMN1(:,IS+1:IN,:) = ZGAMMA1 / (ZEPS + ZBNEG1(:,IS+1:IN,:))**2
  ZOMN2(:,IS+1:IN,:) = ZGAMMA2 / (ZEPS + ZBNEG2(:,IS+1:IN,:))**2
  ZOMN3(:,IS+1:IN,:) = ZGAMMA3 / (ZEPS + ZBNEG3(:,IS+1:IN,:))**2
!
  ZOMP1(:,IN+1,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IN+1,:))**2
  ZOMP2(:,IN+1,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IN+1,:))**2
  ZOMN1(:,IN+1,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IN+1,:))**2
  ZOMN2(:,IN+1,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IN+1,:))**2
  ZOMP1(:,IS,  :) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IS,  :))**2
  ZOMP2(:,IS,  :) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IS,  :))**2
  ZOMN1(:,IS,  :) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IS,  :))**2
  ZOMN2(:,IS,  :) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IS,  :))**2
  ZOMP1(:,IS-1,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IS-1,:))**2
  ZOMP2(:,IS-1,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IS-1,:))**2
  ZOMN1(:,IS-1,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IS-1,:))**2
  ZOMN2(:,IS-1,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IS-1,:))**2
!
! WENO fluxes (5th order)
!
  PR(:,IS+1:IN,:) = (ZOMP2(:,IS+1:IN,:)/(ZOMP1(:,IS+1:IN,:)+ZOMP2(:,IS+1:IN,:)+&
                     ZOMP3(:,IS+1:IN,:)) * ZFPOS2(:,IS+1:IN,:)                 &
                   + ZOMP1(:,IS+1:IN,:)/(ZOMP1(:,IS+1:IN,:)+ZOMP2(:,IS+1:IN,:)+&
                     ZOMP3(:,IS+1:IN,:)) * ZFPOS1(:,IS+1:IN,:)                 &
                   + ZOMP3(:,IS+1:IN,:)/(ZOMP1(:,IS+1:IN,:)+ZOMP2(:,IS+1:IN,:)+&
                     ZOMP3(:,IS+1:IN,:)) * ZFPOS3(:,IS+1:IN,:)) *              &
                   (0.5+SIGN(0.5,PRVCT(:,IS+1:IN,:)))                          &
                  + (ZOMN2(:,IS+1:IN,:)/(ZOMN1(:,IS+1:IN,:)+ZOMN2(:,IS+1:IN,:)+&
                     ZOMN3(:,IS+1:IN,:)) * ZFNEG2(:,IS+1:IN,:)                 &
                   + ZOMN1(:,IS+1:IN,:)/(ZOMN1(:,IS+1:IN,:)+ZOMN2(:,IS+1:IN,:)+&
                     ZOMN3(:,IS+1:IN,:)) * ZFNEG1(:,IS+1:IN,:)                 &
                   + ZOMN3(:,IS+1:IN,:)/(ZOMN1(:,IS+1:IN,:)+ZOMN2(:,IS+1:IN,:)+&
                     ZOMN3(:,IS+1:IN,:)) * ZFNEG3(:,IS+1:IN,:)) *              &
                   (0.5-SIGN(0.5,PRVCT(:,IS+1:IN,:)))
!
! WENO fluxes (3rd order)
!
  PR(:,IS-1,:) =  (ZOMP2(:,IS-1,:)/(ZOMP1(:,IS-1,:)+ZOMP2(:,IS-1,:)) * &
                  ZFPOS2(:,IS-1,:)                                     &
                 + ZOMP1(:,IS-1,:)/(ZOMP1(:,IS-1,:)+ZOMP2(:,IS-1,:)) * &
                  ZFPOS1(:,IS-1,:)) * (0.5+SIGN(0.5,PRVCT(:,IS-1,:)))  &
                + (ZOMN2(:,IS-1,:)/(ZOMN1(:,IS-1,:)+ZOMN2(:,IS-1,:)) * &
                  ZFNEG2(:,IS-1,:)                                     &
                 + ZOMN1(:,IS-1,:)/(ZOMN1(:,IS-1,:)+ZOMN2(:,IS-1,:)) * &
                  ZFNEG1(:,IS-1,:)) * (0.5-SIGN(0.5,PRVCT(:,IS-1,:)))
!
  PR(:,IS,  :) =  (ZOMP2(:,IS,  :)/(ZOMP1(:,IS,  :)+ZOMP2(:,IS,  :)) * &
                  ZFPOS2(:,IS,  :)                                     &
                 + ZOMP1(:,IS,  :)/(ZOMP1(:,IS,  :)+ZOMP2(:,IS,  :)) * &
                  ZFPOS1(:,IS,  :)) * (0.5+SIGN(0.5,PRVCT(:,IS,  :)))  &
                + (ZOMN2(:,IS,  :)/(ZOMN1(:,IS,  :)+ZOMN2(:,IS,  :)) * &
                 ZFNEG2(:,IS,  :)                                      &
                 + ZOMN1(:,IS,  :)/(ZOMN1(:,IS,  :)+ZOMN2(:,IS,  :)) * &
                  ZFNEG1(:,IS,  :)) * (0.5-SIGN(0.5,PRVCT(:,IS,  :)))
!
  PR(:,IN+1,:) =  (ZOMP2(:,IN+1,:)/(ZOMP1(:,IN+1,:)+ZOMP2(:,IN+1,:)) * &
                  ZFPOS2(:,IN+1,:)                                     &
                 + ZOMP1(:,IN+1,:)/(ZOMP1(:,IN+1,:)+ZOMP2(:,IN+1,:)) * &
                  ZFPOS1(:,IN+1,:)) * (0.5+SIGN(0.5,PRVCT(:,IN+1,:)))  &
                + (ZOMN2(:,IN+1,:)/(ZOMN1(:,IN+1,:)+ZOMN2(:,IN+1,:)) * &
                  ZFNEG2(:,IN+1,:)                                     &
                 + ZOMN1(:,IN+1,:)/(ZOMN1(:,IN+1,:)+ZOMN2(:,IN+1,:)) * &
                  ZFNEG1(:,IN+1,:)) * (0.5-SIGN(0.5,PRVCT(:,IN+1,:)))
!
!
!       OPEN, WALL, NEST CASE IN THE Y DIRECTION
!
CASE ('OPEN','WALL','NEST')
!
  IS=IJB
  IN=IJE
!
  IF(LSOUTH_ll()) THEN
    PR(:,IS,:) = PSRC(:,IS-1,:) * (0.5+SIGN(0.5,PRVCT(:,IS,:))) + &
                 PSRC(:,IS,:) * (0.5-SIGN(0.5,PRVCT(:,IS,:)))
!
    ZFPOS1(:,IS+1,:) = 0.5 * (3.0*PSRC(:,IS,:) - PSRC(:,IS-1,:))
    ZFPOS2(:,IS+1,:) = 0.5 * (PSRC(:,IS,    :) + PSRC(:,IS+1,:))
    ZBPOS1(:,IS+1,:) = (PSRC(:,IS,  :) - PSRC(:,IS-1,:))**2
    ZBPOS2(:,IS+1,:) = (PSRC(:,IS+1,:) - PSRC(:,IS,  :))**2
!
    ZFNEG1(:,IS+1,:) = 0.5 * (3.0*PSRC(:,IS+1,:) - PSRC(:,IS+2,:))
    ZFNEG2(:,IS+1,:) = 0.5 * (PSRC(:,IS+1,    :) + PSRC(:,IS,:))
    ZBNEG1(:,IS+1,:) = (PSRC(:,IS+1,:) - PSRC(:,IS+2,:))**2
    ZBNEG2(:,IS+1,:) = (PSRC(:,IS,  :) - PSRC(:,IS+1,:))**2
!
    ZOMP1(:,IS+1,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IS+1,:))**2
    ZOMP2(:,IS+1,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IS+1,:))**2
    ZOMN1(:,IS+1,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IS+1,:))**2
    ZOMN2(:,IS+1,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IS+1,:))**2
!
    PR(:,IS+1,  :) =  (ZOMP2(:,IS+1,:)/(ZOMP1(:,IS+1,:)+ZOMP2(:,IS+1,:)) * &
                   ZFPOS2(:,IS+1,:)                                        &
                 + ZOMP1(:,IS+1,:)/(ZOMP1(:,IS+1,:)+ZOMP2(:,IS+1,:)) *     & 
                   ZFPOS1(:,IS+1,:)) * (0.5+SIGN(0.5,PRVCT(:,IS+1,:)))     &
                + (ZOMN2(:,IS+1,:)/(ZOMN1(:,IS+1,:)+ZOMN2(:,IS+1,:)) *     &
                   ZFNEG2(:,IS+1,:)                                        &
                 + ZOMN1(:,IS+1,:)/(ZOMN1(:,IS+1,:)+ZOMN2(:,IS+1,:)) *     &
                   ZFNEG1(:,IS+1,:)) * (0.5-SIGN(0.5,PRVCT(:,IS+1,:)))
!
  ELSEIF(NHALO == 1) THEN
    ZFPOS1(:,IS,:) = 0.5 * (3.0*PSRC(:,IS-1, :) - TPHALO2%SOUTH(:,:))
    ZFPOS2(:,IS,:) = 0.5 * (PSRC(:,IS-1,     :) + PSRC(:,IS,:))
    ZBPOS1(:,IS,:) = (PSRC(:,IS-1,:) - TPHALO2%SOUTH(:,:))**2
    ZBPOS2(:,IS,:) = (PSRC(:,IS,  :) - PSRC(:,IS-1,:))**2
!
    ZFNEG1(:,IS,:) = 0.5 * (3.0*PSRC(:,IS,  :) - PSRC(:,IS+1,:))
    ZFNEG2(:,IS,:) = 0.5 * (PSRC(:,IS,      :) + PSRC(:,IS-1,:))
    ZBNEG1(:,IS,:) = (PSRC(:,IS,  :) - PSRC(:,IS+1,:))**2
    ZBNEG2(:,IS,:) = (PSRC(:,IS-1,:) - PSRC(:,IS,  :))**2
!
    ZOMP1(:,IS,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IS,:))**2
    ZOMP2(:,IS,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IS,:))**2
    ZOMN1(:,IS,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IS,:))**2
    ZOMN2(:,IS,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IS,:))**2
!
    PR(:,IS,:) =  (ZOMP2(:,IS,:)/(ZOMP1(:,IS,:)+ZOMP2(:,IS,:)) * ZFPOS2(:,IS,:)   &
                 + ZOMP1(:,IS,:)/(ZOMP1(:,IS,:)+ZOMP2(:,IS,:)) * ZFPOS1(:,IS,:)) *&
                 (0.5+SIGN(0.5,PRVCT(:,IS,:)))  &
                + (ZOMN2(:,IS,:)/(ZOMN1(:,IS,:)+ZOMN2(:,IS,:)) * ZFNEG2(:,IS,:)   &
                 + ZOMN1(:,IS,:)/(ZOMN1(:,IS,:)+ZOMN2(:,IS,:)) * ZFNEG1(:,IS,:)) *&
                 (0.5-SIGN(0.5,PRVCT(:,IS,:)))
!
    ZFPOS1(:,IS+1,:) = 1./6 * (2.0*TPHALO2%SOUTH(:,:) - 7.0*PSRC(:,IS-1,:) + &
                       11.0*PSRC(:,IS,:))
    ZFPOS2(:,IS+1,:) = 1./6 * (-1.0*PSRC(:,IS-1,:) + 5.0*PSRC(:,IS,:) +      &
                       2.0*PSRC(:,IS+1,:))
    ZFPOS3(:,IS+1,:) = 1./6 * (2.0*PSRC(:,IS,:) + 5.0*PSRC(:,IS+1,:) -       &
                       1.0*PSRC(:,IS+2,:))
!
    ZBPOS1(:,IS+1,:) = 13./12 * (TPHALO2%SOUTH(:,:) - 2.0*PSRC(:,IS-1,:) +   &
                       PSRC(:,IS,:))**2 + &
     1./4 * (TPHALO2%SOUTH(:,:) - 4.0*PSRC(:,IS-1,:) + 3.0*PSRC(:,IS,:))**2
    ZBPOS2(:,IS+1,:) = 13./12 * (PSRC(:,IS-1,:) - 2.0*PSRC(:,IS,:) +         &
     PSRC(:,IS+1,:))**2 +     &
     1./4 * (PSRC(:,IS-1,:) - PSRC(:,IS+1,:))**2
    ZBPOS3(:,IS+1,:) = 13./12 * (PSRC(:,IS,:) - 2.0*PSRC(:,IS+1,:) +         &
     PSRC(:,IS+2,:))**2 +     &
     1./4 * ( 3.0*PSRC(:,IS,:) - 4.0*PSRC(:,IS+1,:) + PSRC(:,IS+2,:))**2
!
    ZFNEG1(:,IS+1,:) = 1./6 * (11.0*PSRC(:,IS+1,:) - 7.0*PSRC(:,IS+2,:) +    &
     2.0*PSRC(:,IS+3,:))
    ZFNEG2(:,IS+1,:) = 1./6 * (2.0*PSRC(:,IS,:) + 5.0*PSRC(:,IS+1,:) -       &
     1.0*PSRC(:,IS+2,:))
    ZFNEG3(:,IS+1,:) = 1./6 * (-1.0*PSRC(:,IS-1,:) + 5.0*PSRC(:,IS,:) +      &
     2.0*PSRC(:,IS+1,:))
!
    ZBNEG1(:,IS+1,:) = 13./12 * (PSRC(:,IS+1,:) - 2.0*PSRC(:,IS+2,:) +       &
     PSRC(:,IS+3,:))**2 +   &
     1./4 * ( 3.0*PSRC(:,IS+1,:) - 4.0*PSRC(:,IS+2,:) + PSRC(:,IS+3,:))**2
    ZBNEG2(:,IS+1,:) = 13./12 * (PSRC(:,IS,:) - 2.0*PSRC(:,IS+1,:) +         &
     PSRC(:,IS+2,:))**2 +     &
     1./4 * (PSRC(:,IS,:) - PSRC(:,IS+2,:))**2
    ZBNEG3(:,IS+1,:) = 13./12 * (PSRC(:,IS-1,:) - 2.0*PSRC(:,IS,:) +         &
     PSRC(:,IS+1,:))**2 +     &
     1./4 * ( PSRC(:,IS-1,:) - 4.0*PSRC(:,IS,:) + 3.0*PSRC(:,IS+1,:))**2
!
    ZOMP1(:,IS+1,:) = ZGAMMA1 / (ZEPS + ZBPOS1(:,IS+1,:))**2
    ZOMP2(:,IS+1,:) = ZGAMMA2 / (ZEPS + ZBPOS2(:,IS+1,:))**2
    ZOMP3(:,IS+1,:) = ZGAMMA3 / (ZEPS + ZBPOS3(:,IS+1,:))**2
    ZOMN1(:,IS+1,:) = ZGAMMA1 / (ZEPS + ZBNEG1(:,IS+1,:))**2
    ZOMN2(:,IS+1,:) = ZGAMMA2 / (ZEPS + ZBNEG2(:,IS+1,:))**2
    ZOMN3(:,IS+1,:) = ZGAMMA3 / (ZEPS + ZBNEG3(:,IS+1,:))**2
!
    PR(:,IS+1,:) = (ZOMP2(:,IS+1,:)/(ZOMP1(:,IS+1,:)+ZOMP2(:,IS+1,:)+ &
                   ZOMP3(:,IS+1,:)) * ZFPOS2(:,IS+1,:)    &
                   + ZOMP1(:,IS+1,:)/(ZOMP1(:,IS+1,:)+ZOMP2(:,IS+1,:)+ &
                   ZOMP3(:,IS+1,:)) * ZFPOS1(:,IS+1,:)   &
                   + ZOMP3(:,IS+1,:)/(ZOMP1(:,IS+1,:)+ZOMP2(:,IS+1,:)+ &
                   ZOMP3(:,IS+1,:)) * ZFPOS3(:,IS+1,:)) *&
                   (0.5+SIGN(0.5,PRVCT(:,IS+1,:)))                     &
                  + (ZOMN2(:,IS+1,:)/(ZOMN1(:,IS+1,:)+ZOMN2(:,IS+1,:)+ &
                   ZOMN3(:,IS+1,:)) * ZFNEG2(:,IS+1,:)   &
                   + ZOMN1(:,IS+1,:)/(ZOMN1(:,IS+1,:)+ZOMN2(:,IS+1,:)+ &
                   ZOMN3(:,IS+1,:)) * ZFNEG1(:,IS+1,:)   &
                   + ZOMN3(:,IS+1,:)/(ZOMN1(:,IS+1,:)+ZOMN2(:,IS+1,:)+ &
                   ZOMN3(:,IS+1,:)) * ZFNEG3(:,IS+1,:)) *&
                   (0.5-SIGN(0.5,PRVCT(:,IS+1,:)))
!
  ENDIF
!
  IF(LNORTH_ll()) THEN
    PR(:,IN+1,:) = PSRC(:,IN,:) * (0.5+SIGN(0.5,PRVCT(:,IN+1,:))) + &
                   PSRC(:,IN+1,:) * (0.5-SIGN(0.5,PRVCT(:,IN+1,:)))
!
    ZFPOS1(:,IN,:) = 0.5 * (3.0*PSRC(:,IN-1,:) - PSRC(:,IN-2,:))
    ZFPOS2(:,IN,:) = 0.5 * (PSRC(:,IN-1,    :) + PSRC(:,IN,  :))
    ZBPOS1(:,IN,:) = (PSRC(:,IN-1,:) - PSRC(:,IN-2,:))**2
    ZBPOS2(:,IN,:) = (PSRC(:,IN,  :) - PSRC(:,IN-1,:))**2
!
    ZFNEG1(:,IN,:) = 0.5 * (3.0*PSRC(:,IN,:) - PSRC(:,IN+1,:)) 
    ZFNEG2(:,IN,:) = 0.5 * (PSRC(:,IN,    :) + PSRC(:,IN-1,:))
    ZBNEG1(:,IN,:) = (PSRC(:,IN,  :) - PSRC(:,IN+1,:))**2
    ZBNEG2(:,IN,:) = (PSRC(:,IN-1,:) - PSRC(:,IN,  :))**2   
!
    ZOMP1(:,IN,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IN,:))**2
    ZOMP2(:,IN,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IN,:))**2
    ZOMN1(:,IN,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IN,:))**2
    ZOMN2(:,IN,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IN,:))**2
!
    PR(:,IN,:) =  (ZOMP2(:,IN,:)/(ZOMP1(:,IN,:)+ZOMP2(:,IN,:)) * ZFPOS2(:,IN,:)   &
                 + ZOMP1(:,IN,:)/(ZOMP1(:,IN,:)+ZOMP2(:,IN,:)) * ZFPOS1(:,IN,:)) *&
                 (0.5+SIGN(0.5,PRVCT(:,IN,:)))   &
                + (ZOMN2(:,IN,:)/(ZOMN1(:,IN,:)+ZOMN2(:,IN,:)) * ZFNEG2(:,IN,:)   &
                 + ZOMN1(:,IN,:)/(ZOMN1(:,IN,:)+ZOMN2(:,IN,:)) * ZFNEG1(:,IN,:)) *&
                 (0.5-SIGN(0.5,PRVCT(:,IN,:)))
!
  ELSEIF(NHALO == 1) THEN
    ZFPOS1(:,IN+1,:) = 0.5 * (3.0*PSRC(:,IN,:) - PSRC(:,IN-1,:))
    ZFPOS2(:,IN+1,:) = 0.5 * (PSRC(:,IN,    :) + PSRC(:,IN+1,:))
    ZBPOS1(:,IN+1,:) = (PSRC(:,IN,  :) - PSRC(:,IN-1,:))**2
    ZBPOS2(:,IN+1,:) = (PSRC(:,IN+1,:) - PSRC(:,IN,  :))**2
!
    ZFNEG1(:,IN+1,:) = 0.5 * (3.0*PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))
    ZFNEG2(:,IN+1,:) = 0.5 * (PSRC(:,IN+1,    :) + PSRC(:,IN,  :))
    ZBNEG1(:,IN+1,:) = (PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))**2
    ZBNEG2(:,IN+1,:) = (PSRC(:,IN  ,:) - PSRC(:,IN+1,:))**2
!
    ZOMP1(:,IN+1,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IN+1,:))**2
    ZOMP2(:,IN+1,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IN+1,:))**2
    ZOMN1(:,IN+1,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IN+1,:))**2
    ZOMN2(:,IN+1,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IN+1,:))**2
!
    PR(:,IN+1,:) =  (ZOMP2(:,IN+1,:)/(ZOMP1(:,IN+1,:)+ZOMP2(:,IN+1,:))*&
                   ZFPOS2(:,IN+1,:)                                    &
                 + ZOMP1(:,IN+1,:)/(ZOMP1(:,IN+1,:)+ZOMP2(:,IN+1,:)) * &
                   ZFPOS1(:,IN+1,:)) * (0.5+SIGN(0.5,PRVCT(:,IN+1,:))) &
                + (ZOMN2(:,IN+1,:)/(ZOMN1(:,IN+1,:)+ZOMN2(:,IN+1,:)) * &
                   ZFNEG2(:,IN+1,:)                                    &
                 + ZOMN1(:,IN+1,:)/(ZOMN1(:,IN+1,:)+ZOMN2(:,IN+1,:)) * &
                   ZFNEG1(:,IN+1,:)) * (0.5-SIGN(0.5,PRVCT(:,IN+1,:)))
!
    ZFPOS1(:,IN,:) = 1./6 * (2.0*PSRC(:,IN-3,:) - 7.0*PSRC(:,IN-2,:) + &
     11.0*PSRC(:,IN-1,:))
    ZFPOS2(:,IN,:) = 1./6 * (-1.0*PSRC(:,IN-2,:) + 5.0*PSRC(:,IN-1,:) + &
     2.0*PSRC(:,IN,:))
    ZFPOS3(:,IN,:) = 1./6 * (2.0*PSRC(:,IN-1,:) + 5.0*PSRC(:,IN,:) - &
     1.0*PSRC(:,IN+1,:))
!
    ZBPOS1(:,IN,:) = 13./12 * (PSRC(:,IN-3,:) - 2.0*PSRC(:,IN-2,:) + &
    PSRC(:,IN-1,:))**2 + &
     1./4 * (PSRC(:,IN-3,:) - 4.0*PSRC(:,IN-2,:) + 3.0*PSRC(:,IN-1,:))**2
    ZBPOS2(:,IN,:) = 13./12 * (PSRC(:,IN-2,:) - 2.0*PSRC(:,IN-1,:) + &
    PSRC(:,IN,:))**2 + &
     1./4 * (PSRC(:,IN-2,:) - PSRC(:,IN,:))**2
    ZBPOS3(:,IN,:) = 13./12 * (PSRC(:,IN-1,:) - 2.0*PSRC(:,IN,:) + &
    PSRC(:,IN+1,:))**2 + &
     1./4 * ( 3.0*PSRC(:,IN-1,:) - 4.0*PSRC(:,IN,:) + PSRC(:,IN+1,:))**2
!
    ZFNEG1(:,IN,:) = 1./6 * (11.0*PSRC(:,IN,:) - 7.0*PSRC(:,IN+1,:) + &
    2.0*TPHALO2%NORTH(:,:))
    ZFNEG2(:,IN,:) = 1./6 * (2.0*PSRC(:,IN-1,:) + 5.0*PSRC(:,IN,:) - &
    1.0*PSRC(:,IN+1,:))
    ZFNEG3(:,IN,:) = 1./6 * (-1.0*PSRC(:,IN-2,:) + 5.0*PSRC(:,IN-1,:) + &
    2.0*PSRC(:,IN,:))
!
    ZBNEG1(:,IN,:) = 13./12 * (PSRC(:,IN,:) - 2.0*PSRC(:,IN+1,:) + &
    TPHALO2%NORTH(:,:))**2 + &
     1./4 * ( 3.0*PSRC(:,IN,:) - 4.0*PSRC(:,IN+1,:) + TPHALO2%NORTH(:,:))**2
    ZBNEG2(:,IN,:) = 13./12 * (PSRC(:,IN-1,:) - 2.0*PSRC(:,IN,:) + &
    PSRC(:,IN+1,:))**2 + &
     1./4 * (PSRC(:,IN-1,:) - PSRC(:,IN+1,:))**2
    ZBNEG3(:,IN,:) = 13./12 * (PSRC(:,IN-2,:) - 2.0*PSRC(:,IN-1,:) +&
    PSRC(:,IN,:))**2 + &
     1./4 * ( PSRC(:,IN-2,:) - 4.0*PSRC(:,IN-1,:) + 3.0*PSRC(:,IN,:))**2
!
    ZOMP1(:,IN,:) = ZGAMMA1 / (ZEPS + ZBPOS1(:,IN,:))**2
    ZOMP2(:,IN,:) = ZGAMMA2 / (ZEPS + ZBPOS2(:,IN,:))**2
    ZOMP3(:,IN,:) = ZGAMMA3 / (ZEPS + ZBPOS3(:,IN,:))**2
    ZOMN1(:,IN,:) = ZGAMMA1 / (ZEPS + ZBNEG1(:,IN,:))**2
    ZOMN2(:,IN,:) = ZGAMMA2 / (ZEPS + ZBNEG2(:,IN,:))**2
    ZOMN3(:,IN,:) = ZGAMMA3 / (ZEPS + ZBNEG3(:,IN,:))**2
!
    PR(:,IN,:) = (ZOMP2(:,IN,:)/(ZOMP1(:,IN,:)+ZOMP2(:,IN,:)+ZOMP3(:,IN,:)) * &
    ZFPOS2(:,IN,:)     &
    + ZOMP1(:,IN,:)/(ZOMP1(:,IN,:)+ZOMP2(:,IN,:)+ZOMP3(:,IN,:)) * ZFPOS1(:,IN,:)  &
    + ZOMP3(:,IN,:)/(ZOMP1(:,IN,:)+ZOMP2(:,IN,:)+ZOMP3(:,IN,:)) * ZFPOS3(:,IN,:)) &
    * (0.5+SIGN(0.5,PRVCT(:,IN,:))) &
    + (ZOMN2(:,IN,:)/(ZOMN1(:,IN,:)+ZOMN2(:,IN,:)+ZOMN3(:,IN,:)) * ZFNEG2(:,IN,:)  &
    + ZOMN1(:,IN,:)/(ZOMN1(:,IN,:)+ZOMN2(:,IN,:)+ZOMN3(:,IN,:)) * ZFNEG1(:,IN,:)  &
    + ZOMN3(:,IN,:)/(ZOMN1(:,IN,:)+ZOMN2(:,IN,:)+ZOMN3(:,IN,:)) * ZFNEG3(:,IN,:)) &
    * (0.5-SIGN(0.5,PRVCT(:,IN,:)))
!
  ENDIF
!
!        USE A FIFTH ORDER UPSTREAM WENO SCHEME ELSEWHERE (IS+2 --> IN-1)
!
  ZFPOS1(:,IS+2:IN-1,:) = 1./6 * (2.0*PSRC(:,IS-1:IN-4,:) - &
  7.0*PSRC(:,IS:IN-3,  :) + 11.0*PSRC(:,IS+1:IN-2,:))
  ZFPOS2(:,IS+2:IN-1,:) = 1./6 * (-1.0*PSRC(:,IS:IN-3, :) + &
  5.0*PSRC(:,IS+1:IN-2,:) + 2.0*PSRC(:,IS+2:IN-1, :))
  ZFPOS3(:,IS+2:IN-1,:) = 1./6 * (2.0*PSRC(:,IS+1:IN-2,:) + &
  5.0*PSRC(:,IS+2:IN-1,:) - 1.0*PSRC(:,IS+3:IN,   :))
!
  ZBPOS1(:,IS+2:IN-1,:) = 13./12 * (PSRC(:,IS-1:IN-4,:) - &
  2.0*PSRC(:,IS:IN-3,:) + PSRC(:,IS+1:IN-2,:))**2 + &
   1./4 * (PSRC(:,IS-1:IN-4,:) - 4.0*PSRC(:,IS:IN-3,:) + &
   3.0*PSRC(:,IS+1:IN-2,:))**2
  ZBPOS2(:,IS+2:IN-1,:) = 13./12 * (PSRC(:,IS:IN-3,:) - &
  2.0*PSRC(:,IS+1:IN-2,:) + PSRC(:,IS+2:IN-1,:))**2 + &
   1./4 * (PSRC(:,IS:IN-3,:) - PSRC(:,IS+2:IN-1,:))**2
  ZBPOS3(:,IS+2:IN-1,:) = 13./12 * (PSRC(:,IS+1:IN-2,:) - &
  2.0*PSRC(:,IS+2:IN-1,:) + PSRC(:,IS+3:IN,:))**2 + &
   1./4 * ( 3.0*PSRC(:,IS+1:IN-2,:) - 4.0*PSRC(:,IS+2:IN-1,:) +&
   PSRC(:,IS+3:IN,:))**2
!
  ZFNEG1(:,IS+2:IN-1,:) = 1./6 * (11.0*PSRC(:,IS+2:IN-1,:) - &
  7.0*PSRC(:,IS+3:IN,:) + 2.0*PSRC(:,IS+4:IN+1,:))
  ZFNEG2(:,IS+2:IN-1,:) = 1./6 * (2.0*PSRC(:,IS+1:IN-2,:) + &
  5.0*PSRC(:,IS+2:IN-1,:) - 1.0*PSRC(:,IS+3:IN,:))
  ZFNEG3(:,IS+2:IN-1,:) = 1./6 * (-1.0*PSRC(:,IS:IN-3,:) + &
  5.0*PSRC(:,IS+1:IN-2,:) + 2.0*PSRC(:,IS+2:IN-1,:))
!
  ZBNEG1(:,IS+2:IN-1,:) = 13./12 * (PSRC(:,IS+2:IN-1,:) - &
  2.0*PSRC(:,IS+3:IN,:) + PSRC(:,IS+4:IN+1,:))**2 + &
   1./4 * ( 3.0*PSRC(:,IS+2:IN-1,:) - 4.0*PSRC(:,IS+3:IN,:) + &
   PSRC(:,IS+4:IN+1,:))**2
  ZBNEG2(:,IS+2:IN-1,:) = 13./12 * (PSRC(:,IS+1:IN-2,:) - &
  2.0*PSRC(:,IS+2:IN-1,:) + PSRC(:,IS+3:IN,:))**2 + &
  1./4 * (PSRC(:,IS+1:IN-2,:) - PSRC(:,IS+3:IN,:))**2
  ZBNEG3(:,IS+2:IN-1,:) = 13./12 * (PSRC(:,IS:IN-3,:) - &
  2.0*PSRC(:,IS+1:IN-2,:) + PSRC(:,IS+2:IN-1,:))**2 + &
  1./4 * ( PSRC(:,IS:IN-3,:) - 4.0*PSRC(:,IS+1:IN-2,:) + &
  3.0*PSRC(:,IS+2:IN-1,:))**2
!
  ZOMP1(:,IS+2:IN-1,:) = ZGAMMA1 / (ZEPS + ZBPOS1(:,IS+2:IN-1,:))**2
  ZOMP2(:,IS+2:IN-1,:) = ZGAMMA2 / (ZEPS + ZBPOS2(:,IS+2:IN-1,:))**2
  ZOMP3(:,IS+2:IN-1,:) = ZGAMMA3 / (ZEPS + ZBPOS3(:,IS+2:IN-1,:))**2
  ZOMN1(:,IS+2:IN-1,:) = ZGAMMA1 / (ZEPS + ZBNEG1(:,IS+2:IN-1,:))**2
  ZOMN2(:,IS+2:IN-1,:) = ZGAMMA2 / (ZEPS + ZBNEG2(:,IS+2:IN-1,:))**2
  ZOMN3(:,IS+2:IN-1,:) = ZGAMMA3 / (ZEPS + ZBNEG3(:,IS+2:IN-1,:))**2
!
    PR(:,IS+2:IN-1,:) = (ZOMP2(:,IS+2:IN-1,:)/(ZOMP1(:,IS+2:IN-1,:)+&
     ZOMP2(:,IS+2:IN-1,:)+ZOMP3(:,IS+2:IN-1,:)) * ZFPOS2(:,IS+2:IN-1,:)  &
     + ZOMP1(:,IS+2:IN-1,:)/(ZOMP1(:,IS+2:IN-1,:)+&
     ZOMP2(:,IS+2:IN-1,:)+ZOMP3(:,IS+2:IN-1,:)) * ZFPOS1(:,IS+2:IN-1,:)  &
     + ZOMP3(:,IS+2:IN-1,:)/(ZOMP1(:,IS+2:IN-1,:)+ZOMP2(:,IS+2:IN-1,:)+ &
     ZOMP3(:,IS+2:IN-1,:)) * ZFPOS3(:,IS+2:IN-1,:)) &
     * (0.5+SIGN(0.5,PRVCT(:,IS+2:IN-1,:))) &
     + (ZOMN2(:,IS+2:IN-1,:)/(ZOMN1(:,IS+2:IN-1,:)+ZOMN2(:,IS+2:IN-1,:)+ &
     ZOMN3(:,IS+2:IN-1,:)) * ZFNEG2(:,IS+2:IN-1,:)                 &
     + ZOMN1(:,IS+2:IN-1,:)/(ZOMN1(:,IS+2:IN-1,:)+ZOMN2(:,IS+2:IN-1,:)+ &
     ZOMN3(:,IS+2:IN-1,:)) * ZFNEG1(:,IS+2:IN-1,:)                     &
     + ZOMN3(:,IS+2:IN-1,:)/(ZOMN1(:,IS+2:IN-1,:)+ZOMN2(:,IS+2:IN-1,:)+&
     ZOMN3(:,IS+2:IN-1,:)) * ZFNEG3(:,IS+2:IN-1,:)) &
     * (0.5-SIGN(0.5,PRVCT(:,IS+2:IN-1,:)))
!
END SELECT
!
PR = PR * PRVCT
!
END SUBROUTINE ADVEC_WENO_K_3_MY
!
!-------------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE ADVEC_WENO_K_3_VY(HLBCY, PSRC, PRVCT, PR, TPHALO2)
!     #############################################################
!!
!!**** Computes PRVCT * PVT. Upstream fluxes of V in Y direction.  
!!     Input PVT is on V Grid 'ie' (i,j,k) based on VGRID reference
!!     Output PR is on mass Grid 'ie' (i,j+1/2,k) based on VGRID reference
!!
!!    AUTHOR
!!    ------
!!    F. Visentin   *CNRS/LA*
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
USE MODE_ll
USE MODD_LUNIT
USE MODD_CONF
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY  ! Y direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on U grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRVCT ! contrav. comp. on MASS GRID
!
! output source term
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PR
TYPE(HALO2_ll), OPTIONAL, POINTER :: TPHALO2      ! halo2 for the field at t
!
!*       0.2   Declarations of local variables :
!
INTEGER :: IIB,IJB    ! Begining useful area in x,y,z directions
INTEGER :: IIE,IJE    ! End useful area in x,y,z directions
INTEGER::  IS,IN   ! Coordinate of third order diffusion area
!
INTEGER:: ILUOUT,IRESP   ! for prints
!
! intermediate reconstruction fluxes for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFPOS1, ZFPOS2, ZFPOS3
!
! intermediate reconstruction fluxes for negative wind case
! we need only one since ZFNEG2 = ZFPOS2
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFNEG1, ZFNEG2, ZFNEG3
!
! smoothness indicators for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBPOS1, ZBPOS2, ZBPOS3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBNEG1, ZBNEG2, ZBNEG3
!
! WENO weights
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: ZOMP1, ZOMP2, ZOMP3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: ZOMN1, ZOMN2, ZOMN3
!
! standard weights
!
REAL, PARAMETER :: ZGAMMA1 = 1./10.
REAL, PARAMETER :: ZGAMMA2 = 3./5.
REAL, PARAMETER :: ZGAMMA3 = 3./10.
REAL, PARAMETER :: ZGAMMA1_PRIM = 1./3.
REAL, PARAMETER :: ZGAMMA2_PRIM = 2./3.
!
    REAL, PARAMETER :: ZEPS = 1.0E-15
!
!----------------------------------------------------------------------------
!
!*       0.3.     COMPUTES THE DOMAIN DIMENSIONS
!                 ------------------------------
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
!--------------------------------------------------------------------------
!
!*       0.4.   INITIALIZE THE FIELD 
!               ---------------------
!
PR(:,:,:) = 0.0
!
ZFPOS1 = 0.0
ZFPOS2 = 0.0
ZFPOS3 = 0.0
ZFNEG1 = 0.0
ZFNEG2 = 0.0
ZFNEG3 = 0.0
ZBPOS1 = 0.0
ZBPOS2 = 0.0
ZBPOS3 = 0.0
ZBNEG1 = 0.0
ZBNEG2 = 0.0
ZBNEG3 = 0.0
ZOMP1  = 0.0
ZOMP2  = 0.0
ZOMP3  = 0.0
ZOMN1  = 0.0
ZOMN2  = 0.0
ZOMN3  = 0.0
!
!-------------------------------------------------------------------------------
!
SELECT CASE ( HLBCY(1) ) ! Y direction LBC type: (1) for left side
!
!*       1.1    CYCLIC CASE IN THE Y DIRECTION:
!
CASE ('CYCL')          ! In that case one must have HLBCX(1) == HLBCX(2)
!
  IF(NHALO == 1) THEN
    IS=IJB
    IN=IJE
  ELSE
      CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT,IRESP)
      WRITE(ILUOUT,*) 'ERROR : 4th order advection in CYCLic case '
      WRITE(ILUOUT,*) 'cannot be used with NHALO=2'
      CALL ABORT
      STOP
  END IF
!
! Same explanation than for the subroutine ADVEC_WENO_K_3_UX
!
! intermediate fluxes for positive wind case
!
  ZFPOS1(:,IS+1:IN-1,:) = 1./6 * (2.0*PSRC(:,IS-1:IN-3,:) - 7.0*PSRC(:,IS:IN-2,:) +&
  11.0*PSRC(:,IS+1:IN-1,:))
  ZFPOS1(:,IS,       :) = 1./6 * (2.0*TPHALO2%SOUTH(:,:)  - 7.0*PSRC(:,IS-1,   :) +&
  11.0*PSRC(:,IS,       :))
  ZFPOS1(:,IS-1,:) = 0.5 * (3.0*PSRC(:,IS-1,:) - TPHALO2%SOUTH(:,:))
  ZFPOS1(:,IN,  :) = 0.5 * (3.0*PSRC(:,IN,  :) - PSRC(:,IN-1,:))
  ZFPOS1(:,IN+1,:) = 0.5 * (3.0*PSRC(:,IN+1,:) - PSRC(:,IN,  :))
!
!  
  ZFPOS2(:,IS:IN-1,:) = 1./6 * (-1.0*PSRC(:,IS-1:IN-2,:) + 5.0*PSRC(:,IS:IN-1,:) +&
  2.0*PSRC(:,IS+1:IN,:))
  ZFPOS2(:,IS-1,:) = 0.5 * (PSRC(:,IS-1,:) + PSRC(:,IS,  :))
  ZFPOS2(:,IN,  :) = 0.5 * (PSRC(:,IN,  :) + PSRC(:,IN+1,:)) 
  ZFPOS2(:,IN+1,:) = 0.5 * (PSRC(:,IN+1,:) + TPHALO2%NORTH(:,:))
!
!
  ZFPOS3(:,IS:IN-1,:) = 1./6 * (2.0*PSRC(:,IS:IN-1,:) + 5.0*PSRC(:,IS+1:IN,:) - &
  1.0*PSRC(:,IS+2:IN+1,:))
!
! intermediate flux for negative wind case
!
  ZFNEG1(:,IS:IN-2,:) = 1./6 * (11.0*PSRC(:,IS+1:IN-1,:) - 7.0*PSRC(:,IS+2:IN,:) +&
  2.0*PSRC(:,IS+3:IN+1,:))
  ZFNEG1(:,IN-1,   :) = 1./6 * (11.0*PSRC(:,IN,       :) - 7.0*PSRC(:,IN+1,   :) +&
  2.0*TPHALO2%NORTH(:,:))
  ZFNEG1(:,IS-1,:) = 0.5 * (3.0*PSRC(:,IS,:) - PSRC(:,IS+1,:))
  ZFNEG1(:,IN+1,:) = - 999.
  ZFNEG1(:,IN,  :) = 0.5 * (3.0*PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))
!
!
  ZFNEG2(:,IS:IN-1,:) = 1./6 * (2.0*PSRC(:,IS:IN-1,:) + 5.0*PSRC(:,IS+1:IN,:) - &
  1.0*PSRC(:,IS+2:IN+1,:))
  ZFNEG2(:,IS-1,:) = 0.5 * (PSRC(:,IS-1,:) + PSRC(:,IS,  :))
  ZFNEG2(:,IN,  :) = 0.5 * (PSRC(:,IN,  :) + PSRC(:,IN+1,:))
  ZFNEG2(:,IN+1,:) = 0.5 * (PSRC(:,IN+1,:) + TPHALO2%NORTH(:,:))
!
!
  ZFNEG3(:,IS:IN-1,:) = 1./6 * (-1.0*PSRC(:,IS-1:IN-2,:) + 5.0*PSRC(:,IS:IN-1,:) + &
  2.0*PSRC(:,IS+1:IN,:))
!
! smoothness indicators for positive wind case
!
  ZBPOS1(:,IS+1:IN-1,:) = 13./12 * (PSRC(:,IS-1:IN-3,:) - 2.0*PSRC(:,IS:IN-2,:) +&
  PSRC(:,IS+1:IN-1,:))**2 + &
   1./4 * (PSRC(:,IS-1:IN-3,:) - 4.0*PSRC(:,IS:IN-2,:) + 3.0*PSRC(:,IS+1:IN-1,:))**2
  ZBPOS1(:,IS,       :) = 13./12 * (TPHALO2%SOUTH(:,:) - 2.0*PSRC(:,IS-1,:) +&
  PSRC(:,IS,:))**2 + &
   1./4 * (TPHALO2%SOUTH(:,:) - 4.0*PSRC(:,IS-1,:) + 3.0*PSRC(:,IS,:))**2
  ZBPOS1(:,IN+1,:) = (PSRC(:,IN+1,:) - PSRC(:,IN,  :))**2
  ZBPOS1(:,IN,  :) = (PSRC(:,IN,  :) - PSRC(:,IN-1,:))**2
  ZBPOS1(:,IS-1,:) = (PSRC(:,IS-1,:) - TPHALO2%SOUTH(:,:))**2
!
!
  ZBPOS2(:,IS:IN-1,:) = 13./12 * (PSRC(:,IS-1:IN-2,:) - 2.0*PSRC(:,IS:IN-1,:) + &
  PSRC(:,IS+1:IN,:))**2 + &
   1./4 * (PSRC(:,IS-1:IN-2,:) - PSRC(:,IS+1:IN,:))**2
  ZBPOS2(:,IS-1,:) = (PSRC(:,IS,  :) - PSRC(:,IS-1,:))**2
  ZBPOS2(:,IN,  :) = (PSRC(:,IN+1,:) - PSRC(:,IN,  :))**2
  ZBPOS2(:,IN+1,:) = (TPHALO2%NORTH(:,:) - PSRC(:,IN+1,:))**2
!
  ZBPOS3(:,IS:IN-1,:) = 13./12 * (PSRC(:,IS:IN-1,:) - 2.0*PSRC(:,IS+1:IN,:) + &
  PSRC(:,IS+2:IN+1,:))**2 + &
   1./4 * ( 3.0*PSRC(:,IS:IN-1,:) - 4.0*PSRC(:,IS+1:IN,:) + PSRC(:,IS+2:IN+1,:))**2
!
! smoothness indicators for negative wind case
!
  ZBNEG1(:,IS:IN-2,:) = 13./12 * (PSRC(:,IS+1:IN-1,:) - 2.0*PSRC(:,IS+2:IN,:) + &
  PSRC(:,IS+3:IN+1,:))**2 + &
   1./4 * ( 3.0*PSRC(:,IS+1:IN-1,:) - 4.0*PSRC(:,IS+2:IN,:) + &
   PSRC(:,IS+3:IN+1,:))**2
  ZBNEG1(:,IN-1,:) = 13./12 * (PSRC(:,IN,:) - 2.0*PSRC(:,IN+1,:) + &
  TPHALO2%NORTH(:,:))**2 + &
   1./4 * ( 3.0*PSRC(:,IN,:) - 4.0*PSRC(:,IN+1,:) + TPHALO2%NORTH(:,:))**2
  ZBNEG1(:,IS-1,:) = (PSRC(:,IS,:) - PSRC(:,IS+1,:))**2
  ZBNEG1(:,IN+1,:) = - 999.
  ZBNEG1(:,IN,  :) = (PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))**2
!
  ZBNEG2(:,IS:IN-1,:) = 13./12 * (PSRC(:,IS:IN-1,:) - 2.0*PSRC(:,IS+1:IN,:) + &
  PSRC(:,IS+2:IN+1,:))**2 + &
   1./4 * (PSRC(:,IS:IN-1,:) - PSRC(:,IS+2:IN+1,:))**2
  ZBNEG2(:,IS-1,:) = (PSRC(:,IS-1,:) - PSRC(:,IS  ,:))**2
  ZBNEG2(:,IN,  :) = (PSRC(:,IN,  :) - PSRC(:,IN+1,:))**2
  ZBNEG2(:,IN+1,:) = (PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))**2 
!
!
  ZBNEG3(:,IS:IN-1,:) = 13./12 * (PSRC(:,IS-1:IN-2,:) - 2.0*PSRC(:,IS:IN-1,:) + &
  PSRC(:,IS+1:IN,:))**2 + &
   1./4 * ( PSRC(:,IS-1:IN-2,:) - 4.0*PSRC(:,IS:IN-1,:) + 3.0*PSRC(:,IS+1:IN,:))**2
!
! WENO weights
!
  ZOMP1(:,IS:IN-1,:) = ZGAMMA1 / (ZEPS + ZBPOS1(:,IS:IN-1,:))**2
  ZOMP2(:,IS:IN-1,:) = ZGAMMA2 / (ZEPS + ZBPOS2(:,IS:IN-1,:))**2
  ZOMP3(:,IS:IN-1,:) = ZGAMMA3 / (ZEPS + ZBPOS3(:,IS:IN-1,:))**2
  ZOMN1(:,IS:IN-1,:) = ZGAMMA1 / (ZEPS + ZBNEG1(:,IS:IN-1,:))**2
  ZOMN2(:,IS:IN-1,:) = ZGAMMA2 / (ZEPS + ZBNEG2(:,IS:IN-1,:))**2
  ZOMN3(:,IS:IN-1,:) = ZGAMMA3 / (ZEPS + ZBNEG3(:,IS:IN-1,:))**2
!
  ZOMP1(:,IS-1,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IS-1,:))**2
  ZOMP1(:,IN,  :) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IN,  :))**2
  ZOMP1(:,IN+1,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IN+1,:))**2
  ZOMN1(:,IS-1,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IS-1,:))**2
  ZOMN1(:,IN,  :) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IN,  :))**2
  ZOMN1(:,IN+1,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IN+1,:))**2
  ZOMP2(:,IS-1,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IS-1,:))**2
  ZOMP2(:,IN,  :) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IN,  :))**2
  ZOMP2(:,IN+1,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IN+1,:))**2
  ZOMN2(:,IS-1,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IS-1,:))**2
  ZOMN2(:,IN,  :) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IN,  :))**2
  ZOMN2(:,IN+1,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IN+1,:))**2
!
! WENO fluxes (5th order)
!
  PR(:,IS:IN-1,:) = (ZOMP2(:,IS:IN-1,:)/(ZOMP1(:,IS:IN-1,:)+ZOMP2(:,IS:IN-1,:)+&
  ZOMP3(:,IS:IN-1,:)) * ZFPOS2(:,IS:IN-1,:)                                    &
                   + ZOMP1(:,IS:IN-1,:)/(ZOMP1(:,IS:IN-1,:)+ZOMP2(:,IS:IN-1,:)+&
                   ZOMP3(:,IS:IN-1,:)) * ZFPOS1(:,IS:IN-1,:)                   &
                   + ZOMP3(:,IS:IN-1,:)/(ZOMP1(:,IS:IN-1,:)+ZOMP2(:,IS:IN-1,:)+&
                   ZOMP3(:,IS:IN-1,:)) * ZFPOS3(:,IS:IN-1,:))&
                   * (0.5+SIGN(0.5,PRVCT(:,IS:IN-1,:))) &
                  + (ZOMN2(:,IS:IN-1,:)/(ZOMN1(:,IS:IN-1,:)+ZOMN2(:,IS:IN-1,:)+&
                  ZOMN3(:,IS:IN-1,:)) * ZFNEG2(:,IS:IN-1,:)                  &
                   + ZOMN1(:,IS:IN-1,:)/(ZOMN1(:,IS:IN-1,:)+ZOMN2(:,IS:IN-1,:)+&
                   ZOMN3(:,IS:IN-1,:)) * ZFNEG1(:,IS:IN-1,:)                   &
                   + ZOMN3(:,IS:IN-1,:)/(ZOMN1(:,IS:IN-1,:)+ZOMN2(:,IS:IN-1,:)+&
                   ZOMN3(:,IS:IN-1,:)) * ZFNEG3(:,IS:IN-1,:))&
                   * (0.5-SIGN(0.5,PRVCT(:,IS:IN-1,:)))
!       
  PR(:,IS-1,:) = (ZOMP2(:,IS-1,:)/(ZOMP1(:,IS-1,:)+ZOMP2(:,IS-1,:)) * &
                  ZFPOS2(:,IS-1,:)                                    &
                + ZOMP1(:,IS-1,:)/(ZOMP1(:,IS-1,:)+ZOMP2(:,IS-1,:)) * &
                  ZFPOS1(:,IS-1,:)) * (0.5+SIGN(0.5,PRVCT(:,IS-1,:))) &
               + (ZOMN2(:,IS-1,:)/(ZOMN1(:,IS-1,:)+ZOMN2(:,IS-1,:)) * &
                  ZFNEG2(:,IS-1,:)                                    &
                + ZOMN1(:,IS-1,:)/(ZOMN1(:,IS-1,:)+ZOMN2(:,IS-1,:)) * &
                ZFNEG1(:,IS-1,:)) * (0.5-SIGN(0.5,PRVCT(:,IS-1,:)))
!
  PR(:,IN,  :) = (ZOMP2(:,IN,  :)/(ZOMP1(:,IN,  :)+ZOMP2(:,IN,  :)) * &
                  ZFPOS2(:,IN,  :)                                    &
                + ZOMP1(:,IN,  :)/(ZOMP1(:,IN,  :)+ZOMP2(:,IN,  :)) * &
                  ZFPOS1(:,IN,  :)) * (0.5+SIGN(0.5,PRVCT(:,IN,  :))) &
               + (ZOMN2(:,IN,  :)/(ZOMN1(:,IN,  :)+ZOMN2(:,IN,  :)) * &
                  ZFNEG2(:,IN,  :)                                    &
                + ZOMN1(:,IN,  :)/(ZOMN1(:,IN,  :)+ZOMN2(:,IN,  :)) * &
                ZFNEG1(:,IN,  :)) * (0.5-SIGN(0.5,PRVCT(:,IN,  :)))
!
  PR(:,IN+1,:) = (ZOMP2(:,IN+1,:)/(ZOMP1(:,IN+1,:)+ZOMP2(:,IN+1,:)) * &
                  ZFPOS2(:,IN+1,:)                                    &
                + ZOMP1(:,IN+1,:)/(ZOMP1(:,IN+1,:)+ZOMP2(:,IN+1,:)) * &
                  ZFPOS1(:,IN+1,:)) * (0.5+SIGN(0.5,PRVCT(:,IN+1,:))) &
               + (ZOMN2(:,IN+1,:)/(ZOMN1(:,IN+1,:)+ZOMN2(:,IN+1,:)) * &
                  ZFNEG2(:,IN+1,:)                                    &
                + ZOMN1(:,IN+1,:)/(ZOMN1(:,IN+1,:)+ZOMN2(:,IN+1,:)) * &
                  ZFNEG1(:,IN+1,:)) * (0.5-SIGN(0.5,PRVCT(:,IN+1,:)))
!
!
!       OPEN, WALL, NEST CASE IN THE Y DIRECTION
!
CASE ('OPEN','WALL','NEST')
!
  IS=IJB
  IN=IJE
!
!       USE A FIRST ORDER UPSTREAM SCHEME AT THE PHYSICAL BORDER
!
  IF(LSOUTH_ll()) THEN
    PR(:,IS-1,:) = PSRC(:,IS-1,:) * (0.5+SIGN(0.5,PRVCT(:,IS-1,:))) + &
                   PSRC(:,IS,:) * (0.5-SIGN(0.5,PRVCT(:,IS-1,:)))
!
    ZFPOS1(:,IS,:) = 0.5 * (3.0*PSRC(:,IS,:) - PSRC(:,IS-1,:))
    ZFPOS2(:,IS,:) = 0.5 * (PSRC(:,IS,    :) + PSRC(:,IS+1,:))
    ZBPOS1(:,IS,:) = (PSRC(:,IS,  :) - PSRC(:,IS-1,:))**2
    ZBPOS2(:,IS,:) = (PSRC(:,IS+1,:) - PSRC(:,IS,  :))**2
!
    ZFNEG1(:,IS,:) = 0.5 * (3.0*PSRC(:,IS+1,:) - PSRC(:,IS+2,:))
    ZFNEG2(:,IS,:) = 0.5 * (PSRC(:,IS,      :) + PSRC(:,IS+1,:))
    ZBNEG1(:,IS,:) = (PSRC(:,IS+1,:) - PSRC(:,IS+2,:))**2
    ZBNEG2(:,IS,:) = (PSRC(:,IS,  :) - PSRC(:,IS+1,:))**2
!
    ZOMP1(:,IS,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IS,:))**2
    ZOMP2(:,IS,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IS,:))**2
    ZOMN1(:,IS,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IS,:))**2
    ZOMN2(:,IS,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IS,:))**2
!
      PR(:,IS,:) = (ZOMP2(:,IS,:)/(ZOMP1(:,IS,:)+ZOMP2(:,IS,:)) * ZFPOS2(:,IS,:) &
                + ZOMP1(:,IS,:)/(ZOMP1(:,IS,:)+ZOMP2(:,IS,:)) * ZFPOS1(:,IS,:)) *&
                (0.5+SIGN(0.5,PRVCT(:,IS,:))) &
               + (ZOMN2(:,IS,:)/(ZOMN1(:,IS,:)+ZOMN2(:,IS,:)) * ZFNEG2(:,IS,:)   &
                + ZOMN1(:,IS,:)/(ZOMN1(:,IS,:)+ZOMN2(:,IS,:)) * ZFNEG1(:,IS,:)) *&
                (0.5-SIGN(0.5,PRVCT(:,IS,:)))
!
  ELSEIF(NHALO == 1) THEN
    ZFPOS1(:,IS-1,:) = 0.5 * (3.0*PSRC(:,IS-1,:) - TPHALO2%SOUTH(:,:))
    ZFPOS2(:,IS-1,:) = 0.5 * (PSRC(:,IS-1,    :) + PSRC(:,IS,:))
    ZBPOS1(:,IS-1,:) = (PSRC(:,IS-1,:) - TPHALO2%SOUTH(:,:))**2
    ZBPOS2(:,IS-1,:) = (PSRC(:,IS,  :) - PSRC(:,IS-1,:))**2
!
    ZFNEG1(:,IS-1,:) = 0.5 * (3.0*PSRC(:,IS,:) - PSRC(:,IS+1,:))
    ZFNEG2(:,IS-1,:) = 0.5 * (PSRC(:,IS-1,  :) + PSRC(:,IS,  :))
    ZBNEG1(:,IS-1,:) = (PSRC(:,IS,  :) - PSRC(:,IS+1,:))**2
    ZBNEG2(:,IS-1,:) = (PSRC(:,IS-1,:) - PSRC(:,IS,  :))**2
!
    ZOMP1(:,IS-1,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IS-1,:))**2
    ZOMN1(:,IS-1,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IS-1,:))**2
    ZOMP2(:,IS-1,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IS-1,:))**2
    ZOMN2(:,IS-1,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IS-1,:))**2
!
    PR(:,IS-1,:) = (ZOMP2(:,IS-1,:)/(ZOMP1(:,IS-1,:)+ZOMP2(:,IS-1,:)) * &
                  ZFPOS2(:,IS-1,:)                                  &
                + ZOMP1(:,IS-1,:)/(ZOMP1(:,IS-1,:)+ZOMP2(:,IS-1,:)) * &
                  ZFPOS1(:,IS-1,:)) * (0.5+SIGN(0.5,PRVCT(:,IS-1,:))) &
               + (ZOMN2(:,IS-1,:)/(ZOMN1(:,IS-1,:)+ZOMN2(:,IS-1,:)) * &
                  ZFNEG2(:,IS-1,:)                                    &
                + ZOMN1(:,IS-1,:)/(ZOMN1(:,IS-1,:)+ZOMN2(:,IS-1,:)) * &
                  ZFNEG1(:,IS-1,:)) * (0.5-SIGN(0.5,PRVCT(:,IS-1,:)))
!
    ZFPOS1(:,IS,:) = 1./6 * (2.0*TPHALO2%SOUTH(:,:) - 7.0*PSRC(:,IS-1,:) + &
                     11.0*PSRC(:,IS,:))
    ZFPOS2(:,IS,:) = 1./6 * (-1.0*PSRC(:,IS-1,:) + 5.0*PSRC(:,IS,  :) + &
                     2.0*PSRC(:,IS+1,:))
    ZFPOS3(:,IS,:) = 1./6 * (2.0*PSRC(:,IS,   :) + 5.0*PSRC(:,IS+1,:) - &
                     1.0*PSRC(:,IS+2,:))
!
    ZFNEG1(:,IS,:) = 1./6 * (11.0*PSRC(:,IS+1,:) - 7.0*PSRC(:,IS+2,:) + &
                     2.0*PSRC(:,IS+3,:))
    ZFNEG2(:,IS,:) = 1./6 * (2.0*PSRC(:,IS,   :) + 5.0*PSRC(:,IS+1,:) - &
                     1.0*PSRC(:,IS+2,:))
    ZFNEG3(:,IS,:) = 1./6 * (-1.0*PSRC(:,IS-1,:) + 5.0*PSRC(:,IS,  :) + &
                     2.0*PSRC(:,IS+1,:))
!
    ZBPOS1(:,IS,:) = 13./12 * (TPHALO2%SOUTH(:,:) - 2.0*PSRC(:,IS-1,:) + &
                     PSRC(:,IS,:))**2 + &
     1./4 * (TPHALO2%SOUTH(:,:) - 4.0*PSRC(:,IS-1,:) + 3.0*PSRC(:,IS,:))**2
    ZBPOS2(:,IS,:) = 13./12 * (PSRC(:,IS-1,:) - 2.0*PSRC(:,IS,:) + &
                     PSRC(:,IS+1,:))**2 + &
     1./4 * (PSRC(:,IS-1,:) - PSRC(:,IS+1,:))**2
    ZBPOS3(:,IS,:) = 13./12 * (PSRC(:,IS,:) - 2.0*PSRC(:,IS+1,:) + &
     PSRC(:,IS+2,:))**2 + &
     1./4 * ( 3.0*PSRC(:,IS,:) - 4.0*PSRC(:,IS+1,:) + PSRC(:,IS+2,:))**2
!
    ZBNEG1(:,IS,:) = 13./12 * (PSRC(:,IS+1,:) - 2.0*PSRC(:,IS+2,:) + &
     PSRC(:,IS+3,:))**2 + &
     1./4 * ( 3.0*PSRC(:,IS+1,:) - 4.0*PSRC(:,IS+2,:) + PSRC(:,IS+3,:))**2
    ZBNEG2(:,IS,:) = 13./12 * (PSRC(:,IS,:) - 2.0*PSRC(:,IS+1,:) + &
     PSRC(:,IS+2,:))**2 + &
     1./4 * (PSRC(:,IS,:) - PSRC(:,IS+2,:))**2
    ZBNEG3(:,IS,:) = 13./12 * (PSRC(:,IS-1,:) - 2.0*PSRC(:,IS,:) + &
     PSRC(:,IS+1,:))**2 + &
     1./4 * ( PSRC(:,IS-1,:) - 4.0*PSRC(:,IS,:) + 3.0*PSRC(:,IS+1,:))**2
!
    ZOMP1(:,IS,:) = ZGAMMA1 / (ZEPS + ZBPOS1(:,IS,:))**2
    ZOMP2(:,IS,:) = ZGAMMA2 / (ZEPS + ZBPOS2(:,IS,:))**2
    ZOMP3(:,IS,:) = ZGAMMA3 / (ZEPS + ZBPOS3(:,IS,:))**2
    ZOMN1(:,IS,:) = ZGAMMA1 / (ZEPS + ZBNEG1(:,IS,:))**2
    ZOMN2(:,IS,:) = ZGAMMA2 / (ZEPS + ZBNEG2(:,IS,:))**2
    ZOMN3(:,IS,:) = ZGAMMA3 / (ZEPS + ZBNEG3(:,IS,:))**2
!
    PR(:,IS,:) = (ZOMP2(:,IS,:)/(ZOMP1(:,IS,:)+ZOMP2(:,IS,:)+ZOMP3(:,IS,:)) * &
     ZFPOS2(:,IS,:)                                  &
     + ZOMP1(:,IS,:)/(ZOMP1(:,IS,:)+ZOMP2(:,IS,:)+ZOMP3(:,IS,:)) * ZFPOS1(:,IS,:)&
     + ZOMP3(:,IS,:)/(ZOMP1(:,IS,:)+ZOMP2(:,IS,:)+ZOMP3(:,IS,:)) * ZFPOS3(:,IS,:))&
     * (0.5+SIGN(0.5,PRVCT(:,IS,:))) &
     + (ZOMN2(:,IS,:)/(ZOMN1(:,IS,:)+ZOMN2(:,IS,:)+ZOMN3(:,IS,:)) * ZFNEG2(:,IS,:)&
     + ZOMN1(:,IS,:)/(ZOMN1(:,IS,:)+ZOMN2(:,IS,:)+ZOMN3(:,IS,:)) * ZFNEG1(:,IS,:)&
     + ZOMN3(:,IS,:)/(ZOMN1(:,IS,:)+ZOMN2(:,IS,:)+ZOMN3(:,IS,:)) * ZFNEG3(:,IS,:))&
     * (0.5-SIGN(0.5,PRVCT(:,IS,:)))
!       
  ENDIF
!
  IF(LNORTH_ll()) THEN
    PR(:,IN,:) = PSRC(:,IN,:) * (0.5+SIGN(0.5,PRVCT(:,IN,:))) + PSRC(:,IN+1,:) *&
     (0.5-SIGN(0.5,PRVCT(:,IN,:)))
!
    ZFPOS1(:,IN-1,:) = 0.5 * (3.0*PSRC(:,IN-1,:) - PSRC(:,IN-2,:))
    ZFPOS2(:,IN-1,:) = 0.5 * (PSRC(:,IN-1,    :) + PSRC(:,IN,  :))
    ZBPOS1(:,IN-1,:) = (PSRC(:,IN-1,:) - PSRC(:,IN-2,:))**2
    ZBPOS2(:,IN-1,:) = (PSRC(:,IN,  :) - PSRC(:,IN-1,:))**2
!
    ZFNEG1(:,IN-1,:) = 0.5 * (3.0*PSRC(:,IN,:) - PSRC(:,IN+1,:))
    ZFNEG2(:,IN-1,:) = 0.5 * (PSRC(:,IN-1,  :) + PSRC(:,IN,  :))
    ZBNEG1(:,IN-1,:) = (PSRC(:,IN,:) - PSRC(:,IN+1,:))**2
    ZBNEG2(:,IN-1,:) = (PSRC(:,IN-1,:) - PSRC(:,IN,:))**2
!
    ZOMP1(:,IN-1,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IN-1,:))**2
    ZOMN1(:,IN-1,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IN-1,:))**2
    ZOMP2(:,IN-1,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IN-1,:))**2
    ZOMN2(:,IN-1,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IN-1,:))**2
!
      PR(:,IN-1,:) = (ZOMP2(:,IN-1,:)/(ZOMP1(:,IN-1,:)+ZOMP2(:,IN-1,:)) * &
                ZFPOS2(:,IN-1,:)                                   &
                + ZOMP1(:,IN-1,:)/(ZOMP1(:,IN-1,:)+ZOMP2(:,IN-1,:)) * &
                ZFPOS1(:,IN-1,:)) * (0.5+SIGN(0.5,PRVCT(:,IN-1,:)))    &
               + (ZOMN2(:,IN-1,:)/(ZOMN1(:,IN-1,:)+ZOMN2(:,IN-1,:)) *  &
                ZFNEG2(:,IN-1,:)                                       &
                + ZOMN1(:,IN-1,:)/(ZOMN1(:,IN-1,:)+ZOMN2(:,IN-1,:)) *  &
                ZFNEG1(:,IN-1,:)) * (0.5-SIGN(0.5,PRVCT(:,IN-1,:)))
!
  ELSEIF(NHALO == 1) THEN
    ZFPOS1(:,IN,:) = 0.5 * (3.0*PSRC(:,IN,:) - PSRC(:,IN-1,:))
    ZFPOS2(:,IN,:) = 0.5 * (PSRC(:,IN,    :) + PSRC(:,IN+1,:))
    ZBPOS1(:,IN,:) = (PSRC(:,IN,  :) - PSRC(:,IN-1,:))**2
    ZBPOS2(:,IN,:) = (PSRC(:,IN+1,:) - PSRC(:,IN,  :))**2
!
    ZFNEG1(:,IN,:) = 0.5 * (3.0*PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))
    ZFNEG2(:,IN,:) = 0.5 * (PSRC(:,IN,      :) + PSRC(:,IN+1,:))
    ZBNEG1(:,IN,:) = (PSRC(:,IN+1,:) - TPHALO2%NORTH(:,:))**2
    ZBNEG2(:,IN,:) = (PSRC(:,IN,  :) - PSRC(:,IN+1,:))**2
 !
    ZOMP1(:,IN,:) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,IN,:))**2
    ZOMN1(:,IN,:) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,IN,:))**2
    ZOMP2(:,IN,:) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,IN,:))**2
    ZOMN2(:,IN,:) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,IN,:))**2
!
      PR(:,IN,:) = (ZOMP2(:,IN,:)/(ZOMP1(:,IN,:)+ZOMP2(:,IN,:)) * ZFPOS2(:,IN,:) &
                + ZOMP1(:,IN,:)/(ZOMP1(:,IN,:)+ZOMP2(:,IN,:)) * ZFPOS1(:,IN,:)) *&
                (0.5+SIGN(0.5,PRVCT(:,IN,:))) &
               + (ZOMN2(:,IN,:)/(ZOMN1(:,IN,:)+ZOMN2(:,IN,:)) * ZFNEG2(:,IN,:)   &
                + ZOMN1(:,IN,:)/(ZOMN1(:,IN,:)+ZOMN2(:,IN,:)) * ZFNEG1(:,IN,:)) *&
                (0.5-SIGN(0.5,PRVCT(:,IN,:)))
!
    ZFPOS1(:,IN-1,:) = 1./6 * (2.0*PSRC(:,IN-3,:) - 7.0*PSRC(:,IN-2,:) + &
                       11.0*PSRC(:,IN-1,:))
    ZFPOS2(:,IN-1,:) = 1./6 * (-1.0*PSRC(:,IN-2,:) + 5.0*PSRC(:,IN-1,:) + &
                       2.0*PSRC(:,IN,:))
    ZFPOS3(:,IN-1,:) = 1./6 * (2.0*PSRC(:,IN-1,:) + 5.0*PSRC(:,IN,:) - &
                       1.0*PSRC(:,IN+1,:))
!
    ZFNEG1(:,IN-1,:) = 1./6 * (11.0*PSRC(:,IN,  :) - 7.0*PSRC(:,IN+1,:) + &
                       2.0*TPHALO2%NORTH(:,:))
    ZFNEG2(:,IN-1,:) = 1./6 * (2.0*PSRC(:,IN-1, :) + 5.0*PSRC(:,IN,  :) - &
                       1.0*PSRC(:,IN+1,:))
    ZFNEG3(:,IN-1,:) = 1./6 * (-1.0*PSRC(:,IN-2,:) + 5.0*PSRC(:,IN-1,:) + &
                       2.0*PSRC(:,IN,  :))
!
    ZBPOS1(:,IN-1,:) = 13./12 * (PSRC(:,IN-3,:) - 2.0*PSRC(:,IN-2,:) + &
                       PSRC(:,IN-1,:))**2 + &
     1./4 * (PSRC(:,IN-3,:) - 4.0*PSRC(:,IN-2,:) + 3.0*PSRC(:,IN-1,:))**2
    ZBPOS2(:,IN-1,:) = 13./12 * (PSRC(:,IN-2,:) - 2.0*PSRC(:,IN-1,:) + &
                       PSRC(:,IN,:))**2 + &
     1./4 * (PSRC(:,IN-2,:) - PSRC(:,IN,:))**2
    ZBPOS3(:,IN-1,:) = 13./12 * (PSRC(:,IN-1,:) - 2.0*PSRC(:,IN,:) + &
    PSRC(:,IN+1,:))**2 + &
     1./4 * ( 3.0*PSRC(:,IN-1,:) - 4.0*PSRC(:,IN,:) + PSRC(:,IN+1,:))**2
!
    ZBNEG1(:,IN-1,:) = 13./12 * (PSRC(:,IN,:) - 2.0*PSRC(:,IN+1,:) + &
     TPHALO2%NORTH(:,:))**2 + &
     1./4 * ( 3.0*PSRC(:,IN,:) - 4.0*PSRC(:,IN+1,:) + TPHALO2%NORTH(:,:))**2
    ZBNEG2(:,IN-1,:) = 13./12 * (PSRC(:,IN-1,:) - 2.0*PSRC(:,IN,:) + &
     PSRC(:,IN+1,:))**2 + &
     1./4 * (PSRC(:,IN-1,:) - PSRC(:,IN+1,:))**2
    ZBNEG3(:,IN-1,:) = 13./12 * (PSRC(:,IN-2,:) - 2.0*PSRC(:,IN-1,:) + &
     PSRC(:,IN,:))**2 + &
     1./4 * ( PSRC(:,IN-2,:) - 4.0*PSRC(:,IN-1,:) + 3.0*PSRC(:,IN,:))**2
!
    ZOMP1(:,IN-1,:) = ZGAMMA1 / (ZEPS + ZBPOS1(:,IN-1,:))**2
    ZOMP2(:,IN-1,:) = ZGAMMA2 / (ZEPS + ZBPOS2(:,IN-1,:))**2
    ZOMP3(:,IN-1,:) = ZGAMMA3 / (ZEPS + ZBPOS3(:,IN-1,:))**2
    ZOMN1(:,IN-1,:) = ZGAMMA1 / (ZEPS + ZBNEG1(:,IN-1,:))**2
    ZOMN2(:,IN-1,:) = ZGAMMA2 / (ZEPS + ZBNEG2(:,IN-1,:))**2
    ZOMN3(:,IN-1,:) = ZGAMMA3 / (ZEPS + ZBNEG3(:,IN-1,:))**2
!
    PR(:,IN-1,:) = (ZOMP2(:,IN-1,:)/(ZOMP1(:,IN-1,:)+ZOMP2(:,IN-1,:)+ &
      ZOMP3(:,IN-1,:)) * ZFPOS2(:,IN-1,:)                             &
                   + ZOMP1(:,IN-1,:)/(ZOMP1(:,IN-1,:)+ZOMP2(:,IN-1,:)+&
                   ZOMP3(:,IN-1,:)) * ZFPOS1(:,IN-1,:)                &
                   + ZOMP3(:,IN-1,:)/(ZOMP1(:,IN-1,:)+ZOMP2(:,IN-1,:)+&
                   ZOMP3(:,IN-1,:)) * ZFPOS3(:,IN-1,:))               &
                   * (0.5+SIGN(0.5,PRVCT(:,IN-1,:)))                  &
                  + (ZOMN2(:,IN-1,:)/(ZOMN1(:,IN-1,:)+ZOMN2(:,IN-1,:)+&
                  ZOMN3(:,IN-1,:)) * ZFNEG2(:,IN-1,:)                 &
                   + ZOMN1(:,IN-1,:)/(ZOMN1(:,IN-1,:)+ZOMN2(:,IN-1,:)+&
                   ZOMN3(:,IN-1,:)) * ZFNEG1(:,IN-1,:)                &
                   + ZOMN3(:,IN-1,:)/(ZOMN1(:,IN-1,:)+ZOMN2(:,IN-1,:)+&
                   ZOMN3(:,IN-1,:)) * ZFNEG3(:,IN-1,:))               &
                   * (0.5-SIGN(0.5,PRVCT(:,IN-1,:)))
!       
  ENDIF
!
  ZFPOS1(:,IS+1:IN-2,:) = 1./6 * (2.0*PSRC(:,IS-1:IN-4,:) - 7.0*PSRC(:,IS:IN-3,:) +&
   11.0*PSRC(:,IS+1:IN-2,:))
  ZFPOS2(:,IS+1:IN-2,:) = 1./6 * (-1.0*PSRC(:,IS:IN-3,:) + 5.0*PSRC(:,IS+1:IN-2,:)+&
   2.0*PSRC(:,IS+2:IN-1,:))
  ZFPOS3(:,IS+1:IN-2,:) = 1./6 * (2.0*PSRC(:,IS+1:IN-2,:) + 5.0*PSRC(:,IS+2:IN-1,:)&
  - 1.0*PSRC(:,IS+3:IN,:))
!
  ZBPOS1(:,IS+1:IN-2,:) = 13./12 * (PSRC(:,IS-1:IN-4,:) - 2.0*PSRC(:,IS:IN-3,:) + &
   PSRC(:,IS+1:IN-2,:))**2 + &
   1./4 * (PSRC(:,IS-1:IN-4,:) - 4.0*PSRC(:,IS:IN-3,:) + 3.0*PSRC(:,IS+1:IN-2,:))**2
  ZBPOS2(:,IS+1:IN-2,:) = 13./12 * (PSRC(:,IS:IN-3,:) - 2.0*PSRC(:,IS+1:IN-2,:) + &
   PSRC(:,IS+2:IN-1,:))**2 + &
   1./4 * (PSRC(:,IS:IN-3,:) - PSRC(:,IS+2:IN-1,:))**2
  ZBPOS3(:,IS+1:IN-2,:) = 13./12 * (PSRC(:,IS+1:IN-2,:) - 2.0*PSRC(:,IS+2:IN-1,:) +&
   PSRC(:,IS+3:IN,:))**2 + &
   1./4 * ( 3.0*PSRC(:,IS+1:IN-2,:) - 4.0*PSRC(:,IS+2:IN-1,:) + &
   PSRC(:,IS+3:IN,:))**2
!
  ZFNEG1(:,IS+1:IN-2,:) = 1./6 * (11.0*PSRC(:,IS+2:IN-1,:) - &
   7.0*PSRC(:,IS+3:IN,:) + 2.0*PSRC(:,IS+4:IN+1,:))
  ZFNEG2(:,IS+1:IN-2,:) = 1./6 * (2.0*PSRC(:,IS+1:IN-2,:) + &
   5.0*PSRC(:,IS+2:IN-1,:) - 1.0*PSRC(:,IS+3:IN,:))
  ZFNEG3(:,IS+1:IN-2,:) = 1./6 * (-1.0*PSRC(:,IS:IN-3,:) + &
   5.0*PSRC(:,IS+1:IN-2,:) + 2.0*PSRC(:,IS+2:IN-1,:))
!
  ZBNEG1(:,IS+1:IN-2,:) = 13./12 * (PSRC(:,IS+2:IN-1,:) - &
   2.0*PSRC(:,IS+3:IN,:) + PSRC(:,IS+4:IN+1,:))**2 + &
   1./4 * ( 3.0*PSRC(:,IS+2:IN-1,:) - 4.0*PSRC(:,IS+3:IN,:) + &
   PSRC(:,IS+4:IN+1,:))**2
  ZBNEG2(:,IS+1:IN-2,:) = 13./12 * (PSRC(:,IS+1:IN-2,:) - &
   2.0*PSRC(:,IS+2:IN-1,:) + PSRC(:,IS+3:IN,:))**2 + &
   1./4 * (PSRC(:,IS+1:IN-2,:) - PSRC(:,IS+3:IN,:))**2
  ZBNEG3(:,IS+1:IN-2,:) = 13./12 * (PSRC(:,IS:IN-3,:) - &
   2.0*PSRC(:,IS+1:IN-2,:) + PSRC(:,IS+2:IN-1,:))**2 + &
   1./4 * ( PSRC(:,IS:IN-3,:) - 4.0*PSRC(:,IS+1:IN-2,:) + &
   3.0*PSRC(:,IS+2:IN-1,:))**2
!
  ZOMP1(:,IS+1:IN-2,:) = ZGAMMA1 / (ZEPS + ZBPOS1(:,IS+1:IN-2,:))**2
  ZOMP2(:,IS+1:IN-2,:) = ZGAMMA2 / (ZEPS + ZBPOS2(:,IS+1:IN-2,:))**2
  ZOMP3(:,IS+1:IN-2,:) = ZGAMMA3 / (ZEPS + ZBPOS3(:,IS+1:IN-2,:))**2
  ZOMN1(:,IS+1:IN-2,:) = ZGAMMA1 / (ZEPS + ZBNEG1(:,IS+1:IN-2,:))**2
  ZOMN2(:,IS+1:IN-2,:) = ZGAMMA2 / (ZEPS + ZBNEG2(:,IS+1:IN-2,:))**2
  ZOMN3(:,IS+1:IN-2,:) = ZGAMMA3 / (ZEPS + ZBNEG3(:,IS+1:IN-2,:))**2
!
  PR(:,IS+1:IN-2,:) = (ZOMP2(:,IS+1:IN-2,:)/(ZOMP1(:,IS+1:IN-2,:)+     &
   ZOMP2(:,IS+1:IN-2,:)+ZOMP3(:,IS+1:IN-2,:)) * ZFPOS2(:,IS+1:IN-2,:)  &
   + ZOMP1(:,IS+1:IN-2,:)/(ZOMP1(:,IS+1:IN-2,:)+                       &
   ZOMP2(:,IS+1:IN-2,:)+ZOMP3(:,IS+1:IN-2,:)) * ZFPOS1(:,IS+1:IN-2,:)  &
   + ZOMP3(:,IS+1:IN-2,:)/(ZOMP1(:,IS+1:IN-2,:)+ZOMP2(:,IS+1:IN-2,:)+  &
   ZOMP3(:,IS+1:IN-2,:)) * ZFPOS3(:,IS+1:IN-2,:))                      &
   * (0.5+SIGN(0.5,PRVCT(:,IS+1:IN-2,:)))                              &
   + (ZOMN2(:,IS+1:IN-2,:)/(ZOMN1(:,IS+1:IN-2,:)+ZOMN2(:,IS+1:IN-2,:)+ &
   ZOMN3(:,IS+1:IN-2,:)) * ZFNEG2(:,IS+1:IN-2,:)                       &
   + ZOMN1(:,IS+1:IN-2,:)/(ZOMN1(:,IS+1:IN-2,:)+ZOMN2(:,IS+1:IN-2,:)+  &
   ZOMN3(:,IS+1:IN-2,:)) * ZFNEG1(:,IS+1:IN-2,:)                       &
   + ZOMN3(:,IS+1:IN-2,:)/(ZOMN1(:,IS+1:IN-2,:)+ZOMN2(:,IS+1:IN-2,:)+  &
   ZOMN3(:,IS+1:IN-2,:)) * ZFNEG3(:,IS+1:IN-2,:))                      &
   * (0.5-SIGN(0.5,PRVCT(:,IS+1:IN-2,:)))
!
END SELECT
!
PR = PR * PRVCT
!
END SUBROUTINE ADVEC_WENO_K_3_VY
!
!-------------------------------------------------------------------------------
!
!     ############################################
      FUNCTION WENO_K_3_WZ(PSRC, PRWCT) RESULT(PR)
!     ############################################
!!
!!* Computes PRWCT * PWT. Upstream fluxes of W in Z direction.  
!!  Input PWT is on W Grid 'ie' (i,j,k) based on WGRID reference
!!  Output PR is on mass Grid 'ie' (i,j,k+1/2) based on WGRID reference
!!
!!    AUTHOR
!!    ------
!!    F. Visentin   *CNRS/LA*
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
USE MODE_ll
USE MODD_CONF
USE MODD_PARAMETERS,ONLY: JPVEXT
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX  ! X direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on W grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRWCT ! contrav. comp. on MASS GRID
!
! output source term
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: PR
!
!*       0.2   Declarations of local variables :
!
INTEGER :: IB    ! Begining useful area in x,y,z directions
INTEGER :: IT    ! End useful area in x,y,z directions
!
! WENO-related variables:
!
! intermediate reconstruction fluxes for positive wind case
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFPOS1, ZFPOS2, ZFPOS3
!
! intermediate reconstruction fluxes for negative wind case
! we need only one since ZFNEG2 = ZFPOS2
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFNEG1, ZFNEG2, ZFNEG3
!
! smoothness indicators for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBPOS1, ZBPOS2, ZBPOS3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBNEG1, ZBNEG2, ZBNEG3
!
! WENO weights
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZOMP1, ZOMP2, ZOMP3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZOMN1, ZOMN2, ZOMN3
!
! standard weights
!
REAL, PARAMETER :: ZGAMMA1 = 1./10.
REAL, PARAMETER :: ZGAMMA2 = 3./5.
REAL, PARAMETER :: ZGAMMA3 = 3./10.
REAL, PARAMETER :: ZGAMMA1_PRIM = 1./3.
REAL, PARAMETER :: ZGAMMA2_PRIM = 2./3.
!
REAL, PARAMETER :: ZEPS = 1.0E-15
!
!-------------------------------------------------------------------------------
!
!*       0.3.     COMPUTES THE DOMAIN DIMENSIONS
!                 ------------------------------
!
IB = 1 + JPVEXT
IT = SIZE(PSRC,3) - JPVEXT
!
PR(:,:,:) = 0.0
!
ZFPOS1 = 0.0
ZFPOS2 = 0.0
ZFPOS3 = 0.0
ZFNEG1 = 0.0
ZFNEG2 = 0.0
ZFNEG3 = 0.0
ZBPOS1 = 0.0
ZBPOS2 = 0.0
ZBPOS3 = 0.0
ZBNEG1 = 0.0
ZBNEG2 = 0.0
ZBNEG3 = 0.0
ZOMP1  = 0.0
ZOMP2  = 0.0
ZOMP3  = 0.0
ZOMN1  = 0.0
ZOMN2  = 0.0
ZOMN3  = 0.0 
! 
! r: many left cells in regard to 'k' cell for each stencil
!
! intermediate fluxes at the mass point on Wgrid u(i,j,k+1/2) for positive wind
! case (left to the right)
! (r=2 for the first stencil ZFPOS1, r=1 for the second ZFPOS2 and 
!  r=0 for the last ZFPOS3)
! 
ZFPOS1(:,:,IB+1:IT-2) = 1./6 * (2.0*PSRC(:,:,IB-1:IT-4) - 7.0*PSRC(:,:,IB:IT-3) + &
 11.0*PSRC(:,:,IB+1:IT-2))
ZFPOS1(:,:,IB) = 0.5 * (3.0*PSRC(:,:,IB) - PSRC(:,:,IB-1))
ZFPOS1(:,:,IT-1) = 0.5 * (3.0*PSRC(:,:,IT-1) - PSRC(:,:,IT-2))
!
!
ZFPOS2(:,:,IB+1:IT-2) = 1./6 * (-1.0*PSRC(:,:,IB:IT-3) + 5.0*PSRC(:,:,IB+1:IT-2) +&
 2.0*PSRC(:,:,IB+2:IT-1))
ZFPOS2(:,:,IB) = 0.5 * (PSRC(:,:,IB) + PSRC(:,:,IB+1))
ZFPOS2(:,:,IT-1) = 0.5 * (PSRC(:,:,IT) + PSRC(:,:,IT+1))
!
ZFPOS3(:,:,IB+1:IT-2) = 1./6 * (2.0*PSRC(:,:,IB+1:IT-2) + 5.0*PSRC(:,:,IB+2:IT-1) -&
 1.0*PSRC(:,:,IB+3:IT))
!
! r: many left cells in regard to 'k+1' cell for each stencil
! 
! intermediate flux at the mass point on Wgrid (i,j,k+1/2)=(i,j,(k+1)-1/2) 
! for negative wind case (right to the left)
! (r=2 for the last stencil ZFNEG3=ZFPOS2, r=1 for the second ZFNEG2=ZFPOS3 
!  and r=0 for the first ZFNEG1)  
!
ZFNEG1(:,:,IB+1:IT-2) = 1./6 * (11.0*PSRC(:,:,IB+2:IT-1) - 7.0*PSRC(:,:,IB+3:IT) +&
 2.0*PSRC(:,:,IB+4:IT+1))
ZFNEG1(:,:,IT-1) = 0.5 * (3.0*PSRC(:,:,IT) - PSRC(:,:,IT+1))
ZFNEG1(:,:,IB) = 0.5 * (3.0*PSRC(:,:,IB+1) - PSRC(:,:,IB+2))
!
!
ZFNEG2(:,:,IB+1:IT-2) = 1./6 * (2.0*PSRC(:,:,IB+1:IT-2) + 5.0*PSRC(:,:,IB+2:IT-1) -&
 1.0*PSRC(:,:,IB+3:IT))
ZFNEG2(:,:,IB) = 0.5 * (PSRC(:,:,IB) + PSRC(:,:,IB+1))
ZFNEG2(:,:,IT-1) = 0.5 * (PSRC(:,:,IT-1) + PSRC(:,:,IT))
!
!
ZFNEG3(:,:,IB+1:IT-2) = 1./6 * (-1.0*PSRC(:,:,IB:IT-3) + 5.0*PSRC(:,:,IB+1:IT-2) + &
 2.0*PSRC(:,:,IB+2:IT-1))
!
! smoothness indicators for positive wind case
!
ZBPOS1(:,:,IB+1:IT-2) = 13./12 * (PSRC(:,:,IB-1:IT-4) - 2.0*PSRC(:,:,IB:IT-3) + &
 PSRC(:,:,IB+1:IT-2))**2 + &
 1./4 * (PSRC(:,:,IB-1:IT-4) - 4.0*PSRC(:,:,IB:IT-3) + 3.0*PSRC(:,:,IB+1:IT-2))**2
ZBPOS1(:,:,IB) = (PSRC(:,:,IB) - PSRC(:,:,IB-1))**2
ZBPOS1(:,:,IT-1) = (PSRC(:,:,IT-1) - PSRC(:,:,IT-2))**2
!
!
ZBPOS2(:,:,IB+1:IT-2) = 13./12 * (PSRC(:,:,IB:IT-3) - 2.0*PSRC(:,:,IB+1:IT-2) + &
 PSRC(:,:,IB+2:IT-1))**2 + &
 1./4 * (PSRC(:,:,IB:IT-3) - PSRC(:,:,IB+2:IT-1))**2
ZBPOS2(:,:,IB) = (PSRC(:,:,IB+1) - PSRC(:,:,IB))**2
ZBPOS2(:,:,IT-1) = (PSRC(:,:,IT) - PSRC(:,:,IT-1))**2
!
!
ZBPOS3(:,:,IB+1:IT-2) = 13./12 * (PSRC(:,:,IB+1:IT-2) - 2.0*PSRC(:,:,IB+2:IT-1) + &
 PSRC(:,:,IB+3:IT))**2 + &
 1./4 * ( 3.0*PSRC(:,:,IB+1:IT-2) - 4.0*PSRC(:,:,IB+2:IT-1) + PSRC(:,:,IB+3:IT))**2
!
! smoothness indicators for negative wind case
!
ZBNEG1(:,:,IB+1:IT-2) = 13./12 * (PSRC(:,:,IB+2:IT-1) - 2.0*PSRC(:,:,IB+3:IT) + &
 PSRC(:,:,IB+4:IT+1))**2 + &
 1./4 * ( 3.0*PSRC(:,:,IB+2:IT-1) - 4.0*PSRC(:,:,IB+3:IT) + PSRC(:,:,IB+4:IT+1))**2
ZBNEG1(:,:,IB) = (PSRC(:,:,IB+1) - PSRC(:,:,IB+2))**2
ZBNEG1(:,:,IT-1) = (PSRC(:,:,IT) - PSRC(:,:,IT+1))**2
!
ZBNEG2(:,:,IB+1:IT-2) = 13./12 * (PSRC(:,:,IB+1:IT-2) - 2.0*PSRC(:,:,IB+2:IT-1) + &
 PSRC(:,:,IB+3:IT))**2 + &
 1./4 * (PSRC(:,:,IB+1:IT-2) - PSRC(:,:,IB+3:IT))**2
ZBNEG2(:,:,IB) = (PSRC(:,:,IB) - PSRC(:,:,IB+1))**2
ZBNEG2(:,:,IT-1) = (PSRC(:,:,IT-1) - PSRC(:,:,IT))**2
!
!
ZBNEG3(:,:,IB+1:IT-2) = 13./12 * (PSRC(:,:,IB:IT-3) - 2.0*PSRC(:,:,IB+1:IT-2) + &
 PSRC(:,:,IB+2:IT-1))**2 + &
 1./4 * ( PSRC(:,:,IB:IT-3) - 4.0*PSRC(:,:,IB+1:IT-2) + 3.0*PSRC(:,:,IB+2:IT-1))**2
!
! WENO weights
!
ZOMP1(:,:,IB+1:IT-2) = ZGAMMA1 / (ZEPS + ZBPOS1(:,:,IB+1:IT-2))**2
ZOMP2(:,:,IB+1:IT-2) = ZGAMMA2 / (ZEPS + ZBPOS2(:,:,IB+1:IT-2))**2
ZOMP3(:,:,IB+1:IT-2) = ZGAMMA3 / (ZEPS + ZBPOS3(:,:,IB+1:IT-2))**2
ZOMN1(:,:,IB+1:IT-2) = ZGAMMA1 / (ZEPS + ZBNEG1(:,:,IB+1:IT-2))**2
ZOMN2(:,:,IB+1:IT-2) = ZGAMMA2 / (ZEPS + ZBNEG2(:,:,IB+1:IT-2))**2
ZOMN3(:,:,IB+1:IT-2) = ZGAMMA3 / (ZEPS + ZBNEG3(:,:,IB+1:IT-2))**2
!
ZOMP1(:,:,  IB) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,:,  IB))**2
ZOMP2(:,:,  IB) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,:,  IB))**2
ZOMN1(:,:,  IB) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,:,  IB))**2
ZOMN2(:,:,  IB) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,:,  IB))**2
ZOMP1(:,:,IT-1) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,:,IT-1))**2
ZOMP2(:,:,IT-1) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,:,IT-1))**2
ZOMN1(:,:,IT-1) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,:,IT-1))**2
ZOMN2(:,:,IT-1) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,:,IT-1))**2
!
! WENO fluxes (5th order)
!
PR(:,:,IB+1:IT-2) = (ZOMP2(:,:,IB+1:IT-2)/(ZOMP1(:,:,IB+1:IT-2)+&
 ZOMP2(:,:,IB+1:IT-2)+ZOMP3(:,:,IB+1:IT-2)) * ZFPOS2(:,:,IB+1:IT-2) &
 + ZOMP1(:,:,IB+1:IT-2)/(ZOMP1(:,:,IB+1:IT-2)+ZOMP2(:,:,IB+1:IT-2)+ &
 ZOMP3(:,:,IB+1:IT-2)) * ZFPOS1(:,:,IB+1:IT-2)                      &
 + ZOMP3(:,:,IB+1:IT-2)/(ZOMP1(:,:,IB+1:IT-2)+ZOMP2(:,:,IB+1:IT-2)+ &
 ZOMP3(:,:,IB+1:IT-2)) * ZFPOS3(:,:,IB+1:IT-2))                     &
 * (0.5+SIGN(0.5,PRWCT(:,:,IB+1:IT-2)))                             &
 + (ZOMN2(:,:,IB+1:IT-2)/(ZOMN1(:,:,IB+1:IT-2)+ZOMN2(:,:,IB+1:IT-2)+&
 ZOMN3(:,:,IB+1:IT-2)) * ZFNEG2(:,:,IB+1:IT-2)                      &
 + ZOMN1(:,:,IB+1:IT-2)/(ZOMN1(:,:,IB+1:IT-2)+ZOMN2(:,:,IB+1:IT-2)+ &
 ZOMN3(:,:,IB+1:IT-2)) * ZFNEG1(:,:,IB+1:IT-2)                      &
 + ZOMN3(:,:,IB+1:IT-2)/(ZOMN1(:,:,IB+1:IT-2)+ZOMN2(:,:,IB+1:IT-2)+ &
 ZOMN3(:,:,IB+1:IT-2)) * ZFNEG3(:,:,IB+1:IT-2))                     &
 * (0.5-SIGN(0.5,PRWCT(:,:,IB+1:IT-2)))
!
! WENO fluxes (3rd order)
!
PR(:,:,IB) = (ZOMP2(:,:,IB)/(ZOMP1(:,:,IB)+ZOMP2(:,:,IB)) * ZFPOS2(:,:,IB)     &
            + ZOMP1(:,:,IB)/(ZOMP1(:,:,IB)+ZOMP2(:,:,IB)) * ZFPOS1(:,:,IB)) *  &
            (0.5+SIGN(0.5,PRWCT(:,:,IB) ))                                     &
           + (ZOMN2(:,:,IB)/(ZOMN1(:,:,IB)+ZOMN2(:,:,IB)) * ZFNEG2(:,:,IB)     &
            + ZOMN1(:,:,IB)/(ZOMN1(:,:,IB)+ZOMN2(:,:,IB)) * ZFNEG1(:,:,IB)) *  &
            (0.5-SIGN(0.5,PRWCT(:,:,IB) ))
!
PR(:,:,IT-1) = (ZOMP2(:,:,IT-1)/(ZOMP1(:,:,IT-1)+ZOMP2(:,:,IT-1)) * &
                ZFPOS2(:,:,IT-1)                                    &
              + ZOMP1(:,:,IT-1)/(ZOMP1(:,:,IT-1)+ZOMP2(:,:,IT-1)) * &
                ZFPOS1(:,:,IT-1)) * (0.5+SIGN(0.5,PRWCT(:,:,IT-1) ))&
             + (ZOMN2(:,:,IT-1)/(ZOMN1(:,:,IT-1)+ZOMN2(:,:,IT-1)) * &
                ZFNEG2(:,:,IT-1)                                    &
              + ZOMN1(:,:,IT-1)/(ZOMN1(:,:,IT-1)+ZOMN2(:,:,IT-1)) * &
                ZFNEG1(:,:,IT-1)) * (0.5-SIGN(0.5,PRWCT(:,:,IT-1) ))
!
PR(:,:,IB-1) =  PSRC(:,:,IB-1) * (0.5+SIGN(0.5,PRWCT(:,:,IB-1) )) &
              + PSRC(:,:,IB  ) * (0.5-SIGN(0.5,PRWCT(:,:,IB-1) ))
!
PR(:,:,IT) = PSRC(:,:,IT  ) * (0.5+SIGN(0.5,PRWCT(:,:,IT) )) &
           + PSRC(:,:,IT+1) * (0.5-SIGN(0.5,PRWCT(:,:,IT) ))
!
PR(:,:,IT+1) = -999.
!
PR = PR * PRWCT
!
END FUNCTION WENO_K_3_WZ
!
!-----------------------------------------------------------------------------
!
!     ########################################################################
      FUNCTION WENO_K_3_MZ(PSRC, PRWCT) RESULT(PR)
!     ########################################################################
!!
!!* Computes PRWCT * PUT (or PRWCT * PVT). Upstream fluxes of U (or V) 
!!  variables in Z direction.  
!!  Input PUT is on U Grid 'ie' (i,j,k) based on UGRID reference                
!!  Output PR is on mass Grid 'ie' (i,j,k-1/2) based on UGRID reference
!!
!!    AUTHOR
!!    ------
!!    F. Visentin   *CNRS/LA*
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
USE MODE_ll
USE MODD_CONF
USE MODD_PARAMETERS,ONLY: JPVEXT
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX  ! X direction LBC type
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PSRC  ! variable on MASS grid at t
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PRWCT ! contrav. comp. on W grid
!
! output source term
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)) :: PR
!
!*       0.2   Declarations of local variables :
!
INTEGER :: IB    ! Begining useful area in x,y,z directions
INTEGER :: IT    ! End useful area in x,y,z directions
!
! WENO-related variables:
!
! intermediate reconstruction fluxes for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFPOS1, ZFPOS2, ZFPOS3
!
! intermediate reconstruction fluxes for negative wind case
! we need only one since ZFNEG2 = ZFPOS2
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZFNEG1, ZFNEG2, ZFNEG3
!
! smoothness indicators for positive wind case
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBPOS1, ZBPOS2, ZBPOS3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZBNEG1, ZBNEG2, ZBNEG3
!
! WENO weights
!
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZOMP1, ZOMP2, ZOMP3
REAL, DIMENSION(SIZE(PSRC,1),SIZE(PSRC,2),SIZE(PSRC,3)):: ZOMN1, ZOMN2, ZOMN3
!
! standard weights
!
REAL, PARAMETER :: ZGAMMA1 = 1./10.
REAL, PARAMETER :: ZGAMMA2 = 3./5.
REAL, PARAMETER :: ZGAMMA3 = 3./10.
REAL, PARAMETER :: ZGAMMA1_PRIM = 1./3.
REAL, PARAMETER :: ZGAMMA2_PRIM = 2./3.
!
REAL, PARAMETER :: ZEPS = 1.0E-15
!
!-------------------------------------------------------------------------------
!
!*       0.3.     COMPUTES THE DOMAIN DIMENSIONS
!                 ------------------------------
!
IB = 1 + JPVEXT
IT = SIZE(PSRC,3) - JPVEXT
!
PR(:,:,:) = 0.0
!
ZFPOS1 = 0.0
ZFPOS2 = 0.0
ZFPOS3 = 0.0
ZFNEG1 = 0.0
ZFNEG2 = 0.0 
ZFNEG3 = 0.0
ZBPOS1 = 0.0
ZBPOS2 = 0.0
ZBPOS3 = 0.0
ZBNEG1 = 0.0
ZBNEG2 = 0.0
ZBNEG3 = 0.0 
ZOMP1  = 0.0
ZOMP2  = 0.0
ZOMP3  = 0.0
ZOMN1  = 0.0
ZOMN2  = 0.0
ZOMN3  = 0.0
!
! r: many left cells in regard to 'k-1' cell for each stencil
! 
! intermediate fluxes at the mass point on Wgrid u(i,j,k-1/2)=(i,j,(k-1)-1/2) 
! for positive wind case (left to the right)
! (r=2 for the first stencil ZFPOS1, r=1 for the second ZFPOS2 and 
! r=0 for the last ZFPOS3)
!
ZFPOS1(:,:,IB+2:IT-1) = 1./6 * (2.0*PSRC(:,:,IB-1:IT-4) - 7.0*PSRC(:,:,IB:IT-3) + &
 11.0*PSRC(:,:,IB+1:IT-2))
ZFPOS1(:,:,IB+1) = 0.5 * (3.0*PSRC(:,:,  IB) - PSRC(:,:,IB-1))
ZFPOS1(:,:,  IT) = 0.5 * (3.0*PSRC(:,:,IT-1) - PSRC(:,:,IT-2))
!
!
ZFPOS2(:,:,IB+2:IT-1) = 1./6 * (-1.0*PSRC(:,:,IB:IT-3) + 5.0*PSRC(:,:,IB+1:IT-2) + &
 2.0*PSRC(:,:,IB+2:IT-1))
ZFPOS2(:,:,IB+1) = 0.5 * (PSRC(:,:,  IB) + PSRC(:,:,IB+1))
ZFPOS2(:,:,  IT) = 0.5 * (PSRC(:,:,IT-1) + PSRC(:,:,  IT))
!
!
ZFPOS3(:,:,IB+2:IT-1) = 1./6 * (2.0*PSRC(:,:,IB+1:IT-2) + 5.0*PSRC(:,:,IB+2:IT-1) -&
 1.0*PSRC(:,:,IB+3:IT)) 
!
! r: many left cells in regard to 'k' cell for each stencil
!
! intermediate fluxes at the mass point on Ugrid u(i,j,k-1/2) for negative wind
! case (R. to the L.)
! (r=2 for the third stencil ZFNEG3=ZFPOS2, r=1 for the second ZFNEG2=ZFPOS3 
!  and r=0 for the first ZFNEG1)
!
ZFNEG1(:,:,IB+2:IT-1) = 1./6 * (11.0*PSRC(:,:,IB+2:IT-1) - 7.0*PSRC(:,:,IB+3:IT) + &
 2.0*PSRC(:,:,IB+4:IT+1))
ZFNEG1(:,:,IB+1) = 0.5 * (3.0*PSRC(:,:,IB+1) - PSRC(:,:,IB+2))
ZFNEG1(:,:,  IT) = 0.5 * (3.0*PSRC(:,:,  IT) - PSRC(:,:,IT+1))
!
ZFNEG2(:,:,IB+2:IT-1) = 1./6 * (2.0*PSRC(:,:,IB+1:IT-2) + 5.0*PSRC(:,:,IB+2:IT-1) -&
 1.0*PSRC(:,:,IB+3:IT))
ZFNEG2(:,:,IB+1) = 0.5 * (PSRC(:,:,  IB) + PSRC(:,:,IB+1))
ZFNEG2(:,:,  IT) = 0.5 * (PSRC(:,:,IT-1) + PSRC(:,:,  IT))
!
!
ZFNEG3(:,:,IB+2:IT-1) = 1./6 * (-1.0*PSRC(:,:,IB:IT-3) + 5.0*PSRC(:,:,IB+1:IT-2) + &
 2.0*PSRC(:,:,IB+2:IT-1))
!
! smoothness indicators for positive wind case
!
ZBPOS1(:,:,IB+2:IT-1) =  13./12 * (PSRC(:,:,IB-1:IT-4) - 2.0*PSRC(:,:,IB:IT-3) + &
 PSRC(:,:,IB+1:IT-2))**2 + &
 1./4 * (PSRC(:,:,IB-1:IT-4) - 4.0*PSRC(:,:,IB:IT-3) + 3.0*PSRC(:,:,IB+1:IT-2))**2
ZBPOS1(:,:,IB+1) = (PSRC(:,:,  IB) - PSRC(:,:,IB-1))**2
ZBPOS1(:,:,  IT) = (PSRC(:,:,IT-1) - PSRC(:,:,IT-2))**2
!
!
ZBPOS2(:,:,IB+2:IT-1) = 13./12 * (PSRC(:,:,IB:IT-3) - 2.0*PSRC(:,:,IB+1:IT-2) + &
 PSRC(:,:,IB+2:IT-1))**2 + &
 1./4 * (PSRC(:,:,IB:IT-3) - PSRC(:,:,IB+2:IT-1))**2
ZBPOS2(:,:,IB+1) = (PSRC(:,:,IB+1) - PSRC(:,:,  IB))**2
ZBPOS2(:,:,  IT) = (PSRC(:,:,  IT) - PSRC(:,:,IT-1))**2
!
!
ZBPOS3(:,:,IB+2:IT-1) = 13./12 * (PSRC(:,:,IB+1:IT-2) - 2.0*PSRC(:,:,IB+2:IT-1) + &
 PSRC(:,:,IB+3:IT))**2 + &
 1./4 * ( 3.0*PSRC(:,:,IB+1:IT-2) - 4.0*PSRC(:,:,IB+2:IT-1) + PSRC(:,:,IB+3:IT))**2
!
! smoothness indicators for negative wind case
!
ZBNEG1(:,:,IB+2:IT-1) = 13./12 * (PSRC(:,:,IB+2:IT-1) - 2.0*PSRC(:,:,IB+3:IT) + &
 PSRC(:,:,IB+4:IT+1))**2 + &
 1./4 * ( 3.0*PSRC(:,:,IB+2:IT-1) - 4.0*PSRC(:,:,IB+3:IT) + PSRC(:,:,IB+4:IT+1))**2
ZBNEG1(:,:,IB+1) = (PSRC(:,:,IB+1) - PSRC(:,:,IB+2))**2
ZBNEG1(:,:,  IT) = (PSRC(:,:,  IT) - PSRC(:,:,IT+1))**2
!
ZBNEG2(:,:,IB+2:IT-1) = 13./12 * (PSRC(:,:,IB+1:IT-2) - 2.0*PSRC(:,:,IB+2:IT-1) + &
 PSRC(:,:,IB+3:IT))**2 + &
 1./4 * (PSRC(:,:,IB+1:IT-2) - PSRC(:,:,IB+3:IT))**2
ZBNEG2(:,:,IB+1) = (PSRC(:,:,  IB) - PSRC(:,:,IB+1))**2
ZBNEG2(:,:,  IT) = (PSRC(:,:,IT-1) - PSRC(:,:,  IT))**2
!
!
ZBNEG3(:,:,IB+2:IT-1) = 13./12 * (PSRC(:,:,IB:IT-3) - 2.0*PSRC(:,:,IB+1:IT-2) + &
 PSRC(:,:,IB+2:IT-1))**2 + &
 1./4 * ( PSRC(:,:,IB:IT-3) - 4.0*PSRC(:,:,IB+1:IT-2) + 3.0*PSRC(:,:,IB+2:IT-1))**2
!
! WENO weights
!
ZOMP1(:,:,IB+2:IT-1) = ZGAMMA1 / (ZEPS + ZBPOS1(:,:,IB+2:IT-1))**2
ZOMP2(:,:,IB+2:IT-1) = ZGAMMA2 / (ZEPS + ZBPOS2(:,:,IB+2:IT-1))**2
ZOMP3(:,:,IB+2:IT-1) = ZGAMMA3 / (ZEPS + ZBPOS3(:,:,IB+2:IT-1))**2
ZOMN1(:,:,IB+2:IT-1) = ZGAMMA1 / (ZEPS + ZBNEG1(:,:,IB+2:IT-1))**2
ZOMN2(:,:,IB+2:IT-1) = ZGAMMA2 / (ZEPS + ZBNEG2(:,:,IB+2:IT-1))**2
ZOMN3(:,:,IB+2:IT-1) = ZGAMMA3 / (ZEPS + ZBNEG3(:,:,IB+2:IT-1))**2
!
ZOMP1(:,:,IB+1) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,:,IB+1))**2
ZOMP2(:,:,IB+1) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,:,IB+1))**2
ZOMN1(:,:,IB+1) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,:,IB+1))**2
ZOMN2(:,:,IB+1) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,:,IB+1))**2
ZOMP1(:,:,  IT) = ZGAMMA1_PRIM / (ZEPS + ZBPOS1(:,:,  IT))**2
ZOMP2(:,:,  IT) = ZGAMMA2_PRIM / (ZEPS + ZBPOS2(:,:,  IT))**2
ZOMN1(:,:,  IT) = ZGAMMA1_PRIM / (ZEPS + ZBNEG1(:,:,  IT))**2
ZOMN2(:,:,  IT) = ZGAMMA2_PRIM / (ZEPS + ZBNEG2(:,:,  IT))**2 
!
PR(:,:,IB+2:IT-1) = (ZOMP1(:,:,IB+2:IT-1)/(ZOMP1(:,:,IB+2:IT-1)+ &
                    ZOMP2(:,:,IB+2:IT-1)+ZOMP3(:,:,IB+2:IT-1)) * &
                    ZFPOS1(:,:,IB+2:IT-1)                        &
                   + ZOMP2(:,:,IB+2:IT-1)/(ZOMP1(:,:,IB+2:IT-1)+ &
                   ZOMP2(:,:,IB+2:IT-1)+ZOMP3(:,:,IB+2:IT-1)) *  &
                   ZFPOS2(:,:,IB+2:IT-1)                         &
                   + ZOMP3(:,:,IB+2:IT-1)/(ZOMP1(:,:,IB+2:IT-1)+ &
                   ZOMP2(:,:,IB+2:IT-1)+ZOMP3(:,:,IB+2:IT-1)) *  &
                   ZFPOS3(:,:,IB+2:IT-1))                        &
                   * (0.5+SIGN(0.5,PRWCT(:,:,IB+2:IT-1)))        &
                  + (ZOMN1(:,:,IB+2:IT-1)/(ZOMN1(:,:,IB+2:IT-1)+ &
                   ZOMN2(:,:,IB+2:IT-1)+ZOMN3(:,:,IB+2:IT-1)) *  &
                   ZFNEG1(:,:,IB+2:IT-1)                         &
                   + ZOMN2(:,:,IB+2:IT-1)/(ZOMN1(:,:,IB+2:IT-1)+ &
                   ZOMN2(:,:,IB+2:IT-1)+ZOMN3(:,:,IB+2:IT-1)) *  &
                   ZFNEG2(:,:,IB+2:IT-1)                         &
                   + ZOMN3(:,:,IB+2:IT-1)/(ZOMN1(:,:,IB+2:IT-1)+ &
                   ZOMN2(:,:,IB+2:IT-1)+ZOMN3(:,:,IB+2:IT-1)) *  &
                   ZFNEG3(:,:,IB+2:IT-1))                        &
                   * (0.5-SIGN(0.5,PRWCT(:,:,IB+2:IT-1) )) 
!
PR(:,:,IB+1) = (ZOMP2(:,:,IB+1)/(ZOMP1(:,:,IB+1)+ZOMP2(:,:,IB+1)) * &
                ZFPOS2(:,:,IB+1)                                    &
              + ZOMP1(:,:,IB+1)/(ZOMP1(:,:,IB+1)+ZOMP2(:,:,IB+1)) * &
                ZFPOS1(:,:,IB+1)) * (0.5+SIGN(0.5,PRWCT(:,:,IB+1) ))&
             + (ZOMN2(:,:,IB+1)/(ZOMN1(:,:,IB+1)+ZOMN2(:,:,IB+1)) * &
                ZFNEG2(:,:,IB+1)                                    &
              + ZOMN1(:,:,IB+1)/(ZOMN1(:,:,IB+1)+ZOMN2(:,:,IB+1)) * &
                ZFNEG1(:,:,IB+1)) * (0.5-SIGN(0.5,PRWCT(:,:,IB+1) ))
!
PR(:,:,IT) = (ZOMP2(:,:,IT)/(ZOMP1(:,:,IT)+ZOMP2(:,:,IT)) * ZFPOS2(:,:,IT)     &
            + ZOMP1(:,:,IT)/(ZOMP1(:,:,IT)+ZOMP2(:,:,IT)) * ZFPOS1(:,:,IT)) *  &
            (0.5+SIGN(0.5,PRWCT(:,:,IT) ))                                     &
           + (ZOMN2(:,:,IT)/(ZOMN1(:,:,IT)+ZOMN2(:,:,IT)) * ZFNEG2(:,:,IT)     &
            + ZOMN1(:,:,IT)/(ZOMN1(:,:,IT)+ZOMN2(:,:,IT)) * ZFNEG1(:,:,IT)) *  &
            (0.5-SIGN(0.5,PRWCT(:,:,IT) ))
!
PR(:,:,IB) = PSRC(:,:,IB-1) * (0.5+SIGN(0.5,PRWCT(:,:,IB) )) &
           + PSRC(:,:,IB  ) * (0.5-SIGN(0.5,PRWCT(:,:,IB) ))
!
PR(:,:,IT+1) = PSRC(:,:,IT  ) * (0.5+SIGN(0.5,PRWCT(:,:,IT+1) )) &
             + PSRC(:,:,IT+1) * (0.5-SIGN(0.5,PRWCT(:,:,IT+1) ))
!
!PR(:,:,IB-1) = - 999.
!
PR = PR * PRWCT
!
END FUNCTION WENO_K_3_MZ
