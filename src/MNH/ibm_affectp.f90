!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!    #######################
MODULE MODI_IBM_AFFECTP  
  !    ####################### 
  !
  INTERFACE
     !
     SUBROUTINE IBM_AFFECTP(PVAR,KIBM_LAYER,PRADIUS,PPOWERS,&
          HIBM_MODE_INTE1,HIBM_MODE_INTE3,&
          HIBM_TYPE_BOUND,HIBM_MODE_BOUND,&
          HIBM_FORC_BOUND,PIBM_FORC_BOUND,PXMU,PDIV)
       !
       REAL, DIMENSION(:,:,:) , INTENT(INOUT) :: PVAR
       INTEGER                , INTENT(IN)    :: KIBM_LAYER
       REAL                   , INTENT(IN)    :: PRADIUS
       REAL                   , INTENT(IN)    :: PPOWERS
       CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_MODE_INTE1
       CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_MODE_INTE3
       CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_TYPE_BOUND
       CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_MODE_BOUND  
       CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_FORC_BOUND
       REAL                   , INTENT(IN)    :: PIBM_FORC_BOUND
       REAL, DIMENSION(:,:,:) , INTENT(IN)    :: PXMU
       REAL, DIMENSION(:,:,:) , INTENT(IN)    :: PDIV
       !
     END SUBROUTINE IBM_AFFECTP
     !
  END INTERFACE
  !
END MODULE MODI_IBM_AFFECTP
!
!     ########################################################
SUBROUTINE IBM_AFFECTP(PVAR,KIBM_LAYER,PRADIUS,PPOWERS,&
     HIBM_MODE_INTE1,HIBM_MODE_INTE3,&
     HIBM_TYPE_BOUND,HIBM_MODE_BOUND,&
     HIBM_FORC_BOUND,PIBM_FORC_BOUND,PXMU,PDIV)
  !     ########################################################
  !
  !
  !****  IBM_AFFECTP computes the variable PVAR on desired ghost points :
  !                 - the P type of the ghost/image 
  !                 - the 3D interpolation mode (HIBM_MODE_INTE3)
  !                 - the 1D interpolation mode (HIBM_MODE_INTE1)
  !                 - the boundary condition    (HIBM_TYPE_BOUND)  
  !                 - the symmetry character    (HIBM_MODE_BOUND)    
  !                   
  !                
  !    PURPOSE
  !    -------
  !****  Ghosts (resp. Images) locations are stored in KIBM_STOR_GHOST (resp. KIBM_STOR_IMAGE).
  !      Solutions are computed in regard of the symmetry character of the solution:
  !                                      HIBM_MODE_BOUND='SYME' (Symmetrical)
  !                                      HIBM_MODE_BOUND='ASYM' (Anti-symmetrical)
  !      The ghost value is depending on the variable value at the interface:
  !                                      HIBM_TYPE_BOUND="NULL" (00 value)
  !                                      HIBM_TYPE_BOUND="FREE" (I1 value)
  !                                      HIBM_TYPE_BOUND="LINE" (linear evolution, only IMAGE2 type)
  !                                      HIBM_TYPE_BOUND="LOGA" (logarithmic evol, only IMAGE2 type)
  !      Three 3D interpolations exists  HIBM_MODE_INTE3 = "IDW" (Inverse  Distance Weighting)
  !                                      HIBM_MODE_INTE3 = "MDW" (Modified Distance Weighting)
  !                                      HIBM_MODE_INTE3 = "CLI" (Trilinear Lagrange interp. )
  !      Three 1D interpolations exists  HIBM_MODE_INTE1 = "CL1" (Lagrange Polynomials - 1 points - MIRROR)
  !                                      HIBM_MODE_INTE1 = "CL2" (Lagrange Polynomials - 2 points - IMAGE1)
  !                                      HIBM_MODE_INTE1 = "CL3" (Lagrange Polynomials - 3 points - IMAGE2)

  !    METHOD
  !    ------
  !      - loop on ghosts
  !      - functions storage
  !      - computations of the location of the corners cell containing MIRROR/IMAGE1/IMAGE2
  !      - 3D interpolation (IDW, MDW, CLI)  to obtain the MIRROR/IMAGE1/IMAGE2 values
  !      - computation of the value at the interface 
  !      - 1D interpolation (CLI1,CLI2,CLI3) to obtain the GHOSTS values
  !      - Affectation
  !
  !    EXTERNAL
  !    --------
  !      SUBROUTINE ?
  !
  !    IMPLICIT ARGUMENTS
  !    ------------------
  !       MODD_?   
  !
  !    REFERENCE
  !    ---------
  !
  !    AUTHOR
  !    ------
  !      Franck Auguste (CERFACS-AE)
  !
  !    MODIFICATIONS
  !    -------------
  !      Original         01/01/2019
  !
  !------------------------------------------------------------------------------
  !       
  !**** 0. DECLARATIONS
  !     ---------------
  ! module
  USE MODE_POS
  USE MODE_ll
  USE MODE_IO
  !
  ! declaration
  USE MODD_IBM_PARAM_n
  !
  ! interface
  USE MODD_REF_n, ONLY: XRHODJ,XRHODREF,XEXNREF
  USE MODI_IBM_VALUECORN
  USE MODI_IBM_LOCATCORN
  USE MODI_IBM_3DINT
  USE MODI_IBM_1DINT
  USE MODI_IBM_0DINT
  USE MODD_CST
  USE MODD_CTURB
  USE MODD_RADIATIONS_n
  USE MODD_DYN_n
  USE MODD_FIELD_n
  USE MODD_GRID_n, ONLY: XXHAT,XYHAT
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  REAL, DIMENSION(:,:,:) , INTENT(INOUT) :: PVAR            ! interpolated variable
  INTEGER                , INTENT(IN)    :: KIBM_LAYER      ! layer number
  REAL                   , INTENT(IN)    :: PRADIUS         ! Radius for MDW 
  REAL                   , INTENT(IN)    :: PPOWERS         ! Power for IDW/MDW
  CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_MODE_INTE1 ! interpolation 1D (normal)
  CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_MODE_INTE3 ! interpolation 3D (isotropic)
  CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_TYPE_BOUND ! imposed variable at the interface 
  CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_MODE_BOUND ! symm.-antisymm. solution
  CHARACTER(LEN=3)       , INTENT(IN)    :: HIBM_FORC_BOUND ! Neu,Dir,Rob CL
  REAL                   , INTENT(IN)    :: PIBM_FORC_BOUND
  REAL, DIMENSION(:,:,:) , INTENT(IN)    :: PXMU
  REAL, DIMENSION(:,:,:) , INTENT(IN)    :: PDIV
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER                              :: JI,JJ,JK,JL,JM,JMM,JN,JNN,JI2,JJ2,JK2 ! loop index
  INTEGER, DIMENSION(:)  , ALLOCATABLE :: I_INDEX_CORN                          ! reference corner index 
  INTEGER                              :: I_GHOST_NUMB                          ! ghost number per layer
  REAL   , DIMENSION(:,:), ALLOCATABLE :: Z_LOCAT_CORN,Z_LOCAT_IMAG             ! corners coordinates
  REAL   , DIMENSION(:)  , ALLOCATABLE :: Z_TESTS_CORN                          ! interface distance dependence 
  REAL   , DIMENSION(:)  , ALLOCATABLE :: Z_VALUE_CORN                          ! value variables at corners
  REAL   , DIMENSION(:)  , ALLOCATABLE :: Z_VALUE_IMAG                          ! value at mirror/image1/image2 
  REAL   , DIMENSION(:)  , ALLOCATABLE :: Z_LOCAT_BOUN,Z_LOCAT_GHOS             ! location of bound and ghost
  CHARACTER(LEN=3)                     :: Y_TYPE_BOUND                          ! imposed variable at the interface 
  CHARACTER(LEN=3)                     :: Y_MODE_BOUND                          ! symm.-antisymm. solution
  REAL                                 :: Z_VALUE_GHOS,Z_DELTA_IMAG
  REAL                                 :: Z_FORC_BOUND,ZIBM_VISC,ZIBM_DIVK,ZSURF
  REAL                                 :: ZIBM_HALO
  !
  !------------------------------------------------------------------------------
  !
  !       0.3  Allocation
  !
  ALLOCATE(I_INDEX_CORN(3))
  ALLOCATE(Z_LOCAT_CORN(8,3))
  ALLOCATE(Z_VALUE_CORN(8))
  ALLOCATE(Z_TESTS_CORN(8))
  ALLOCATE(Z_LOCAT_IMAG(3,3))
  ALLOCATE(Z_VALUE_IMAG(4))
  ALLOCATE(Z_LOCAT_BOUN(3))
  ALLOCATE(Z_LOCAT_GHOS(3))
  !
  !----------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  I_INDEX_CORN(:)  = 0
  Z_LOCAT_CORN(:,:)= 0.
  Z_VALUE_CORN(:)  = 0.
  Z_TESTS_CORN(:)  = 0.
  Z_LOCAT_IMAG(:,:)= 0.
  Z_VALUE_IMAG(:)  = 0.
  Z_LOCAT_GHOS(:)  = 0.
  Z_LOCAT_BOUN(:)  = 0.
  Y_TYPE_BOUND = HIBM_TYPE_BOUND
  Y_MODE_BOUND = HIBM_MODE_BOUND
  Z_FORC_BOUND = PIBM_FORC_BOUND
  !
  !**** 2. EXECUTIONS
  !     -------------
  DO JMM=1,KIBM_LAYER
     !
     ! searching number of ghosts 
     JM = size(NIBM_GHOST_P,1)
     JI = 0
     JJ = 0
     JK = 0
     DO WHILE ((JI==0.and.JJ==0.and.JK==0).and.JM>0)
        JI = NIBM_GHOST_P(JM,JMM,1,1)
        JJ = NIBM_GHOST_P(JM,JMM,1,2)
        JK = NIBM_GHOST_P(JM,JMM,1,3)
        IF (JI==0.and.JJ==0.and.JK==0) JM = JM - 1
     ENDDO
     I_GHOST_NUMB = JM
     !
     ! Loop on each P Ghosts 
     IF (I_GHOST_NUMB<=0) GO TO 666
     DO JM = 1,I_GHOST_NUMB
        !
        ! ghost index/ls
        JI = NIBM_GHOST_P(JM,JMM,1,1)
        JJ = NIBM_GHOST_P(JM,JMM,1,2)
        JK = NIBM_GHOST_P(JM,JMM,1,3)
        IF (JI==0.or.JJ==0.or.JK==0) GO TO 777
        Z_LOCAT_GHOS(:) =  XIBM_GHOST_P(JM,JMM,1,:)
        Z_LOCAT_BOUN(:) = 2.0*XIBM_IMAGE_P(JM,JMM,1,1,:)-1.0*XIBM_IMAGE_P(JM,JMM,1,2,:)
        ZIBM_HALO=1.
        !
        ! === IMAGE1/IMAGE2 computation ===
        !
        DO JN = 1,3
           !
           Z_LOCAT_IMAG(JN,:)= XIBM_IMAGE_P(JM,JMM,1  ,JN,:)
           Z_DELTA_IMAG      = ((XXHAT(JI+1)-XXHAT(JI))*(XYHAT(JJ+1)-XYHAT(JJ)))**0.5       
           I_INDEX_CORN(:)   = NIBM_IMAGE_P(JM,JMM,1,1,JN,:)
           IF (I_INDEX_CORN(1)==0.AND.JN==2) ZIBM_HALO=0.
           IF (I_INDEX_CORN(2)==0.AND.JN==2) ZIBM_HALO=0.
           Z_LOCAT_CORN(:,:) = IBM_LOCATCORN(I_INDEX_CORN,1)
           Z_TESTS_CORN(:)   = XIBM_TESTI_P(JM,JMM,1,1,JN,:)
           Z_VALUE_CORN(:)   = IBM_VALUECORN(PVAR,I_INDEX_CORN)
           Z_VALUE_IMAG(JN)  = IBM_3DINT(JN,Z_VALUE_IMAG,Z_LOCAT_BOUN,Z_TESTS_CORN,&
                Z_LOCAT_CORN,Z_VALUE_CORN,Z_LOCAT_IMAG(JN,:),&
                HIBM_MODE_INTE3,PRADIUS,PPOWERS)
           !
        ENDDO
        !
        ZIBM_VISC = PXMU(JI,JJ,JK)
        ZIBM_DIVK = PDIV(JI,JJ,JK)
        !
        JN = 4
        Z_VALUE_IMAG(JN)  = IBM_0DINT(Z_DELTA_IMAG,Z_VALUE_IMAG,HIBM_TYPE_BOUND,HIBM_FORC_BOUND,Z_FORC_BOUND,ZIBM_VISC,ZIBM_DIVK)
        !
        ! === GHOST computation ===
        !
        ! functions storage
        Z_LOCAT_IMAG(1,3) = ((XIBM_GHOST_P(JM,JMM,1,1)-Z_LOCAT_BOUN(1))**2.+&
             (XIBM_GHOST_P(JM,JMM,1,2)-Z_LOCAT_BOUN(2))**2.+&
             (XIBM_GHOST_P(JM,JMM,1,3)-Z_LOCAT_BOUN(3))**2.)**0.5

        IF ((Z_LOCAT_IMAG(1,3)>Z_DELTA_IMAG).AND.ZIBM_HALO>0.5) THEN
           Z_LOCAT_IMAG(1,1) = ((XIBM_IMAGE_P(JM,JMM,1,1,1)-Z_LOCAT_BOUN(1))**2.+&
                (XIBM_IMAGE_P(JM,JMM,1,1,2)-Z_LOCAT_BOUN(2))**2.+&
                (XIBM_IMAGE_P(JM,JMM,1,1,3)-Z_LOCAT_BOUN(3))**2.)**0.5
           Z_LOCAT_IMAG(1,2) = ((XIBM_IMAGE_P(JM,JMM,1,2,1)-Z_LOCAT_BOUN(1))**2.+&
                (XIBM_IMAGE_P(JM,JMM,1,2,2)-Z_LOCAT_BOUN(2))**2.+&
                (XIBM_IMAGE_P(JM,JMM,1,2,3)-Z_LOCAT_BOUN(3))**2.)**0.5
        ELSE
           Z_LOCAT_IMAG(1,1) = ((XIBM_IMAGE_P(JM,JMM,1,3,1)-Z_LOCAT_BOUN(1))**2.+&
                (XIBM_IMAGE_P(JM,JMM,1,3,2)-Z_LOCAT_BOUN(2))**2.+&
                (XIBM_IMAGE_P(JM,JMM,1,3,3)-Z_LOCAT_BOUN(3))**2.)**0.5
           Z_LOCAT_IMAG(1,2) = ((XIBM_IMAGE_P(JM,JMM,1,1,1)-Z_LOCAT_BOUN(1))**2.+&
                (XIBM_IMAGE_P(JM,JMM,1,1,2)-Z_LOCAT_BOUN(2))**2.+&
                (XIBM_IMAGE_P(JM,JMM,1,1,3)-Z_LOCAT_BOUN(3))**2.)**0.5
           Z_VALUE_IMAG(2) = Z_VALUE_IMAG(1)
           Z_VALUE_IMAG(1) = Z_VALUE_IMAG(3)
        ENDIF
        !
        Z_VALUE_GHOS    = IBM_1DINT(Z_LOCAT_IMAG(1,:),Z_VALUE_IMAG,HIBM_MODE_INTE1)
        !
        JN = 3
        I_INDEX_CORN(:)   = NIBM_IMAGE_P(JM,JMM,1,1,JN,:)
        Z_VALUE_CORN(:)   = IBM_VALUECORN(XIBM_LS(:,:,:,1),I_INDEX_CORN)
        Z_LOCAT_CORN(:,:) = IBM_LOCATCORN(I_INDEX_CORN,1)
        DO JL=1,8
           IF (JL==1) THEN
              JI2 = I_INDEX_CORN(1)
              JJ2 = I_INDEX_CORN(2)
              JK2 = I_INDEX_CORN(3)
           ENDIF
           IF (JL==2) THEN
              JI2 = I_INDEX_CORN(1)+1
              JJ2 = I_INDEX_CORN(2)
              JK2 = I_INDEX_CORN(3)
           ENDIF
           IF (JL==3) THEN
              JI2 = I_INDEX_CORN(1)
              JJ2 = I_INDEX_CORN(2)+1
              JK2 = I_INDEX_CORN(3)
           ENDIF
           IF (JL==4) THEN
              JI2 = I_INDEX_CORN(1)+1
              JJ2 = I_INDEX_CORN(2)+1
              JK2 = I_INDEX_CORN(3)
           ENDIF
           IF (JL==5) THEN
              JI2 = I_INDEX_CORN(1)
              JJ2 = I_INDEX_CORN(2)
              JK2 = I_INDEX_CORN(3)+1
           ENDIF
           IF (JL==6) THEN
              JI2 = I_INDEX_CORN(1)+1
              JJ2 = I_INDEX_CORN(2)
              JK2 = I_INDEX_CORN(3)+1
           ENDIF
           IF (JL==7) THEN
              JI2 = I_INDEX_CORN(1)
              JJ2 = I_INDEX_CORN(2)+1
              JK2 = I_INDEX_CORN(3)+1
           ENDIF
           IF (JL==8) THEN
              JI2 = I_INDEX_CORN(1)+1
              JJ2 = I_INDEX_CORN(2)+1
              JK2 = I_INDEX_CORN(3)+1
           ENDIF
           ZSURF = ((Z_LOCAT_CORN(JL,1)-Z_LOCAT_BOUN(1))**2.+ &
                (Z_LOCAT_CORN(JL,2)-Z_LOCAT_BOUN(2))**2.+ &
                (Z_LOCAT_CORN(JL,3)-Z_LOCAT_BOUN(3))**2.)**0.5/(Z_DELTA_IMAG/2.)
           IF ((ZSURF<1.).AND.(Z_VALUE_CORN(JL).LT.(XIBM_EPSI)).AND.((PVAR(JI2,JJ2,JK2)-Z_VALUE_IMAG(3))*(PVAR(JI2,JJ2,JK2)- & 
                Z_VALUE_IMAG(4)).GT.XIBM_EPSI)) THEN
              PVAR(JI2,JJ2,JK2) = 0.5*PVAR(JI2,JJ2,JK2)+0.5*(Z_VALUE_IMAG(4)-(Z_VALUE_IMAG(3)-Z_VALUE_IMAG(4))* & 
                   Z_VALUE_CORN(JL)/(Z_DELTA_IMAG/2.))
           ENDIF
        ENDDO
        !
        IF (Y_MODE_BOUND=='SYM') PVAR(JI,JJ,JK) = +Z_VALUE_GHOS
        IF (Y_MODE_BOUND=='ASY') PVAR(JI,JJ,JK) = -Z_VALUE_GHOS + 2.*Z_VALUE_IMAG(4)
        IF (Y_MODE_BOUND=='CST') PVAR(JI,JJ,JK) =  Z_VALUE_IMAG(4)
        !
777     CONTINUE
     ENDDO

  ENDDO
  !
666 CONTINUE
  !
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  !
  DEALLOCATE(I_INDEX_CORN)
  DEALLOCATE(Z_LOCAT_CORN)
  DEALLOCATE(Z_VALUE_CORN)
  DEALLOCATE(Z_LOCAT_IMAG)
  DEALLOCATE(Z_VALUE_IMAG)
  DEALLOCATE(Z_LOCAT_BOUN)
  DEALLOCATE(Z_LOCAT_GHOS)
  DEALLOCATE(Z_TESTS_CORN)
  ! 
  RETURN
  !
END SUBROUTINE IBM_AFFECTP
