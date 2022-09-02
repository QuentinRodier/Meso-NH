!MNH_LIC Copyright 2019-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!    #########################
MODULE MODI_IBM_SMOOTH_LS  
  !    ######################### 
  !
  INTERFACE
     !
     SUBROUTINE IBM_SMOOTH_LS(KIBM_SMOOTH,PIBM_SMOOTH,PPHI)
       !
       INTEGER                  ,INTENT(IN)    :: KIBM_SMOOTH
       REAL                     ,INTENT(IN)    :: PIBM_SMOOTH 
       REAL, DIMENSION(:,:,:,:) ,INTENT(INOUT) :: PPHI        
       !
     END SUBROUTINE IBM_SMOOTH_LS
     !
  END INTERFACE
  !
END MODULE MODI_IBM_SMOOTH_LS
!
!     ######################################################
SUBROUTINE IBM_SMOOTH_LS(KIBM_SMOOTH,PIBM_SMOOTH,PPHI)
  !     ######################################################
  !
  !
  !****  IBM_SMOOTH_LS is a smoothing method for LS function
  !                
  !    PURPOSE
  !    -------
  !****  The purpose of this routine is to smooth VF/LS functions
  !      in order to improve computations of characteristics surface     
  !      (be careful with singularities and corners) 
  !
  !    METHOD
  !    ------
  !****  Iterative systems
  !      - value at mass node weighted by values at neighboring flux nodes
  !      - value at flux node weighted by values at neighboring mass nodes
  !    
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
  USE MODD_IBM_LSF   
  USE MODD_PARAMETERS, ONLY: JPVEXT,JPHEXT   
  USE MODD_GRID_n,      ONLY: XDXHAT, XDYHAT
  USE MODD_METRICS_n, ONLY: XDXX,XDYY,XDZZ,XDZX,XDZY
  USE MODD_ARGSLIST_ll, ONLY: LIST_ll
  USE MODD_VAR_ll, ONLY: IP
  !
  ! interface
  USE MODI_SHUMAN
  USE MODI_GRADIENT_M
  USE MODI_GRADIENT_U
  USE MODI_GRADIENT_V
  USE MODI_GRADIENT_W
  USE MODI_GRADIENT_UV
  USE MODI_GRADIENT_VW
  USE MODI_GRADIENT_UW
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  INTEGER                  , INTENT(IN)    :: KIBM_SMOOTH ! Smooth levels
  REAL                     , INTENT(IN)    :: PIBM_SMOOTH ! Smooth weighting 
  REAL, DIMENSION(:,:,:,:) , INTENT(INOUT) :: PPHI        ! LS functions
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER                :: IIB,IJB,IKB,IIE,IJE,IKE
  INTEGER                :: IIU,IJU,IKU             ! domain size
  INTEGER                :: JI,JJ,JK,JL,JM          ! loop index
  INTEGER                :: ILISPT_NUMB             ! number of smooth iteration  
  REAL                   :: ILISPT_FACT             ! smooth factor
  TYPE(LIST_ll), POINTER :: TZFIELDS_ll             ! list of fields to exchange
  INTEGER                                :: IINFO_ll
  REAL                                   :: ILISPT_FACTU,ILISPT_FACTV 
  REAL                                   :: ILISPT_FACTW,ILISPT_FACTP
  REAL                                   :: ZPE,ZPW,ZPB,ZPT,ZPN,ZPS
  REAL                                   :: ZREF,ZREF3
  REAL,DIMENSION(:,:,:,:), ALLOCATABLE   :: ZTEMP
  REAL,DIMENSION(:,:,:,:,:), ALLOCATABLE :: Z_NORM_TEMP0
  REAL,DIMENSION(:,:,:)  , ALLOCATABLE   :: Z_NORM_TEMP1
  !
  !------------------------------------------------------------------------------
  !
  !       0.3  Allocation
  !
  !
  IIU=SIZE(PPHI,1)
  IJU=SIZE(PPHI,2)
  IKU=SIZE(PPHI,3)
  !
  CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
  !
  IKE = IKU - JPVEXT
  IKB =   1 + JPVEXT
  ZREF =(1.e-2)*( XDXHAT(IIB) * XDYHAT(IJB) )**0.5
  ZREF3=        ( XDXHAT(IIB) * XDYHAT(IJB) )**0.5
  !
  ! Boundary symmetry
  !
  PPHI(:,:,1,5) = 2.*PPHI(:,:,2,5)-PPHI(:,:,3,5)
  PPHI(:,:,1,3) = 2.*PPHI(:,:,2,3)-PPHI(:,:,3,3)
  PPHI(:,:,1,2) = 2.*PPHI(:,:,2,2)-PPHI(:,:,3,2)
  PPHI(:,:,1,1) = 2.*PPHI(:,:,2,1)-PPHI(:,:,3,1)
  WHERE (PPHI(:,:,2,5).GT.XIBM_EPSI)  PPHI(:,:,1,5) = PPHI(:,:,2,5)
  WHERE (PPHI(:,:,2,3).GT.XIBM_EPSI)  PPHI(:,:,1,3) = PPHI(:,:,2,3)
  WHERE (PPHI(:,:,2,2).GT.XIBM_EPSI)  PPHI(:,:,1,2) = PPHI(:,:,2,2)
  WHERE (PPHI(:,:,2,1).GT.XIBM_EPSI)  PPHI(:,:,1,1) = PPHI(:,:,2,1)
  PPHI(:,:,2,6) =   (PPHI(:,:,2,2)+PPHI(:,:,1,2))/2.
  PPHI(:,:,2,7) =   (PPHI(:,:,2,3)+PPHI(:,:,1,3))/2.
  PPHI(:,:,2,4) =   (PPHI(:,:,2,1)+PPHI(:,:,1,1))/2.
  PPHI(:,:,1,6) = 2.*PPHI(:,:,2,6)-PPHI(:,:,3,6)
  PPHI(:,:,1,7) = 2.*PPHI(:,:,2,7)-PPHI(:,:,3,7)
  PPHI(:,:,1,4) = 2.*PPHI(:,:,2,4)-PPHI(:,:,3,4)
  WHERE (PPHI(:,:,2,6).GT.XIBM_EPSI)  PPHI(:,:,1,6) = PPHI(:,:,2,6)
  WHERE (PPHI(:,:,2,7).GT.XIBM_EPSI)  PPHI(:,:,1,7) = PPHI(:,:,2,7)
  WHERE (PPHI(:,:,2,4).GT.XIBM_EPSI)  PPHI(:,:,1,4) = PPHI(:,:,2,4)
  !
  PPHI(:,:,IKU,5) = 2.*PPHI(:,:,IKU-1,5)-PPHI(:,:,IKU-2,5)
  PPHI(:,:,IKU,3) = 2.*PPHI(:,:,IKU-1,3)-PPHI(:,:,IKU-2,3)
  PPHI(:,:,IKU,2) = 2.*PPHI(:,:,IKU-1,2)-PPHI(:,:,IKU-2,2)
  PPHI(:,:,IKU,1) = 2.*PPHI(:,:,IKU-1,1)-PPHI(:,:,IKU-2,1)
  WHERE (PPHI(:,:,IKU-1,5).GT.XIBM_EPSI)  PPHI(:,:,IKU,5) = PPHI(:,:,IKU-1,5)
  WHERE (PPHI(:,:,IKU-1,3).GT.XIBM_EPSI)  PPHI(:,:,IKU,3) = PPHI(:,:,IKU-1,3)
  WHERE (PPHI(:,:,IKU-1,2).GT.XIBM_EPSI)  PPHI(:,:,IKU,2) = PPHI(:,:,IKU-1,2)
  WHERE (PPHI(:,:,IKU-1,1).GT.XIBM_EPSI)  PPHI(:,:,IKU,1) = PPHI(:,:,IKU-1,1)
  PPHI(:,:,IKU,6) =   (PPHI(:,:,IKU-1,2)+PPHI(:,:,IKU,2))/2.
  PPHI(:,:,IKU,7) =   (PPHI(:,:,IKU-1,3)+PPHI(:,:,IKU,3))/2.
  PPHI(:,:,IKU,4) =   (PPHI(:,:,IKU-1,1)+PPHI(:,:,IKU,1))/2.
  WHERE (PPHI(:,:,IKU-1,6).GT.XIBM_EPSI)  PPHI(:,:,IKU,6) = PPHI(:,:,IKU-1,6)
  WHERE (PPHI(:,:,IKU-1,7).GT.XIBM_EPSI)  PPHI(:,:,IKU,7) = PPHI(:,:,IKU-1,7)
  WHERE (PPHI(:,:,IKU-1,4).GT.XIBM_EPSI)  PPHI(:,:,IKU,4) = PPHI(:,:,IKU-1,4)
  !
  DO JL=1,7
     !
     IF (LWEST_ll ()) PPHI(2  ,:,:,JL) = PPHI(    3,:,:,JL)
     IF (LEAST_ll ()) PPHI(IIU-1,:,:,JL) = PPHI(IIU-2,:,:,JL)
     IF (LSOUTH_ll()) PPHI(:,2  ,:,JL) = PPHI(:,    3,:,JL)
     IF (LNORTH_ll()) PPHI(:,IJU-1,:,JL) = PPHI(:,IJU-2,:,JL)
     IF (LWEST_ll ()) PPHI(1  ,:,:,JL) = PPHI(    2,:,:,JL)
     IF (LEAST_ll ()) PPHI(IIU,:,:,JL) = PPHI(IIU-1,:,:,JL)
     IF (LSOUTH_ll()) PPHI(:,1  ,:,JL) = PPHI(:,    2,:,JL)
     IF (LNORTH_ll()) PPHI(:,IJU,:,JL) = PPHI(:,IJU-1,:,JL)
     !
     IF(LWEST_ll()) THEN
        PPHI(IIB-1,IJB:IJE,IKB-1,JL)=PPHI(IIB-1,IJB:IJE,IKB,JL)
        PPHI(IIB-1,IJB:IJE,IKE+1,JL)=PPHI(IIB-1,IJB:IJE,IKE,JL)
     END IF
     !
     IF (LEAST_ll()) THEN
        PPHI(IIE+1,IJB:IJE,IKB-1,JL)=PPHI(IIE+1,IJB:IJE,IKB,JL)
        PPHI(IIE+1,IJB:IJE,IKE+1,JL)=PPHI(IIE+1,IJB:IJE,IKE,JL)
     END IF
     !
     IF (LSOUTH_ll()) THEN
        PPHI(IIB:IIE,IJB-1,IKB-1,JL)=PPHI(IIB:IIE,IJB-1,IKB,JL)
        PPHI(IIB:IIE,IJB-1,IKE+1,JL)=PPHI(IIB:IIE,IJB-1,IKE,JL)
     END IF
     !
     IF (LNORTH_ll()) THEN
        PPHI(IIB:IIE,IJE+1,IKB-1,JL)=PPHI(IIB:IIE,IJE+1,IKB,JL)
        PPHI(IIB:IIE,IJE+1,IKE+1,JL)=PPHI(IIB:IIE,IJE+1,IKE,JL)
     END IF
     !
     WHERE (ABS(PPHI(:,:,:,JL)).LT.(ZREF-2.*XIBM_EPSI)) PPHI(:,:,:,JL) = ZREF-XIBM_EPSI
     !
  ENDDO
  !
  NULLIFY(TZFIELDS_ll)
  !
  DO JL=1,7
     CALL ADD3DFIELD_ll(TZFIELDS_ll,PPHI(:,:,:,JL),'IBM_SMOOTH_LS::PPHI')
  ENDDO
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
  !
  IF (KIBM_SMOOTH==0) RETURN
  !
  ALLOCATE(ZTEMP(IIU,IJU,IKU,7))
  ALLOCATE(Z_NORM_TEMP0(IIU,IJU,IKU,3,7),Z_NORM_TEMP1(IIU,IJU,IKU))
  !
  !------------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  ! LISPT_NUMB correspond to the number of iteration
  ! LISPT_FACT correspond to correspond to the weight between mass/flux nodes
  !
  ILISPT_NUMB=KIBM_SMOOTH
  ILISPT_FACT=PIBM_SMOOTH
  !
  IF (IP==1) WRITE(*,*) 'NIBM_SMOOTH,XIBM_SMOOTH' , ILISPT_NUMB,ILISPT_FACT
  !
  !**** 2. EXECUTIONS
  !     -------------
  ! 
  ! Techniques to compute with an accurate precision 
  ! the normal vector to the interface, the local curvature
  !
  DO JL = 1,ILISPT_NUMB
     !
     Z_NORM_TEMP0(:,:,:,:,:)=1.
     !
     IF (MOD(JL,2)==0.AND.JL>3) THEN
        NULLIFY(TZFIELDS_ll)
        DO JM=1,4
           IF (JM==1) THEN
              Z_NORM_TEMP0(:,:,:,1,JM) = -GX_U_M(PPHI(:,:,:,2),XDXX,XDZZ,XDZX)
              Z_NORM_TEMP0(:,:,:,2,JM) = -GY_V_M(PPHI(:,:,:,3),XDYY,XDZZ,XDZY)
              Z_NORM_TEMP0(:,:,:,3,JM) = -GZ_W_M(PPHI(:,:,:,4),XDZZ) 
           ENDIF
           IF (JM==2) THEN
              Z_NORM_TEMP0(:,:,:,1,JM) = -GX_M_U(1,IKU,1,PPHI(:,:,:,1),XDXX,XDZZ,XDZX) 
              Z_NORM_TEMP0(:,:,:,2,JM) = -GY_UV_U(PPHI(:,:,:,5),XDYY,XDZZ,XDZY)
              Z_NORM_TEMP0(:,:,:,3,JM) = -GZ_UW_U(PPHI(:,:,:,6),XDZZ)
           ENDIF
           IF (JM==3) THEN
              Z_NORM_TEMP0(:,:,:,1,JM) = -GX_UV_V(PPHI(:,:,:,5),XDXX,XDZZ,XDZX)
              Z_NORM_TEMP0(:,:,:,2,JM) = - GY_M_V(1,IKU,1,PPHI(:,:,:,1),XDYY,XDZZ,XDZY)
              Z_NORM_TEMP0(:,:,:,3,JM) = -GZ_VW_V(PPHI(:,:,:,7),XDZZ)
           ENDIF
           IF (JM==4) THEN
              Z_NORM_TEMP0(:,:,:,1,JM) = -GX_UW_W(PPHI(:,:,:,6),XDXX,XDZZ,XDZX)
              Z_NORM_TEMP0(:,:,:,2,JM) = -GY_VW_W(PPHI(:,:,:,7),XDYY,XDZZ,XDZY)
              Z_NORM_TEMP0(:,:,:,3,JM) = - GZ_M_W(1,IKU,1,PPHI(:,:,:,1),XDZZ)           
           ENDIF
           Z_NORM_TEMP1(:,:,:)=(ABS(Z_NORM_TEMP0(:,:,:,1,JM))+&
                ABS(Z_NORM_TEMP0(:,:,:,2,JM))+&
                ABS(Z_NORM_TEMP0(:,:,:,3,JM)))
           WHERE (Z_NORM_TEMP1(:,:,:).GT.XIBM_EPSI)
              Z_NORM_TEMP0(:,:,:,1,JM)=3.*(1.-ABS(Z_NORM_TEMP0(:,:,:,1,JM))/Z_NORM_TEMP1(:,:,:))/2. 
              Z_NORM_TEMP0(:,:,:,2,JM)=3.*(1.-ABS(Z_NORM_TEMP0(:,:,:,2,JM))/Z_NORM_TEMP1(:,:,:))/2.
              Z_NORM_TEMP0(:,:,:,3,JM)=3.*(1.-ABS(Z_NORM_TEMP0(:,:,:,3,JM))/Z_NORM_TEMP1(:,:,:))/2.
           ELSEWHERE
              Z_NORM_TEMP0(:,:,:,1,JM)=1.
              Z_NORM_TEMP0(:,:,:,2,JM)=1.
              Z_NORM_TEMP0(:,:,:,3,JM)=1.
           ENDWHERE
           CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMP0(:,:,:,1,JM),'IBM_SMOOTH_LS::Z_NORM_TEMP0')
           CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMP0(:,:,:,2,JM),'IBM_SMOOTH_LS::Z_NORM_TEMP0')
           CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMP0(:,:,:,3,JM),'IBM_SMOOTH_LS::Z_NORM_TEMP0')
           IF (JM==4) THEN
              CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
              CALL CLEANLIST_ll(TZFIELDS_ll)
           ENDIF
        ENDDO
        NULLIFY(TZFIELDS_ll)
        DO JM=5,7
           IF (JM==5) THEN
              Z_NORM_TEMP0(:,:,:,1,JM) = (MXM(Z_NORM_TEMP0(:,:,:,1,3))+MYM(Z_NORM_TEMP0(:,:,:,1,2)))/2.
              Z_NORM_TEMP0(:,:,:,2,JM) = (MXM(Z_NORM_TEMP0(:,:,:,2,3))+MYM(Z_NORM_TEMP0(:,:,:,2,2)))/2.
              Z_NORM_TEMP0(:,:,:,3,JM) = (MXM(Z_NORM_TEMP0(:,:,:,3,3))+MYM(Z_NORM_TEMP0(:,:,:,3,2)))/2.
           ENDIF
           IF (JM==6) THEN
              Z_NORM_TEMP0(:,:,:,1,JM) = (MXM(Z_NORM_TEMP0(:,:,:,1,4))+MZM(Z_NORM_TEMP0(:,:,:,1,2)))/2.
              Z_NORM_TEMP0(:,:,:,2,JM) = (MXM(Z_NORM_TEMP0(:,:,:,2,4))+MZM(Z_NORM_TEMP0(:,:,:,2,2)))/2.
              Z_NORM_TEMP0(:,:,:,3,JM) = (MXM(Z_NORM_TEMP0(:,:,:,3,4))+MZM(Z_NORM_TEMP0(:,:,:,3,2)))/2.
           ENDIF
           IF (JM==7) THEN
              Z_NORM_TEMP0(:,:,:,1,JM) = (MYM(Z_NORM_TEMP0(:,:,:,1,4))+MZM(Z_NORM_TEMP0(:,:,:,1,3)))/2.
              Z_NORM_TEMP0(:,:,:,2,JM) = (MYM(Z_NORM_TEMP0(:,:,:,2,4))+MZM(Z_NORM_TEMP0(:,:,:,2,3)))/2.
              Z_NORM_TEMP0(:,:,:,3,JM) = (MYM(Z_NORM_TEMP0(:,:,:,3,4))+MZM(Z_NORM_TEMP0(:,:,:,3,3)))/2.
           ENDIF
           Z_NORM_TEMP1(:,:,:)=(ABS(Z_NORM_TEMP0(:,:,:,1,JM))+&
                ABS(Z_NORM_TEMP0(:,:,:,2,JM))+&
                ABS(Z_NORM_TEMP0(:,:,:,3,JM)))
           WHERE (Z_NORM_TEMP1(:,:,:).GT.XIBM_EPSI)
              Z_NORM_TEMP0(:,:,:,1,JM)=3.*(1.-ABS(Z_NORM_TEMP0(:,:,:,1,JM))/Z_NORM_TEMP1(:,:,:))/2.
              Z_NORM_TEMP0(:,:,:,2,JM)=3.*(1.-ABS(Z_NORM_TEMP0(:,:,:,2,JM))/Z_NORM_TEMP1(:,:,:))/2.
              Z_NORM_TEMP0(:,:,:,3,JM)=3.*(1.-ABS(Z_NORM_TEMP0(:,:,:,3,JM))/Z_NORM_TEMP1(:,:,:))/2.
           ELSEWHERE
              Z_NORM_TEMP0(:,:,:,1,JM)=1.
              Z_NORM_TEMP0(:,:,:,2,JM)=1.
              Z_NORM_TEMP0(:,:,:,3,JM)=1.
           ENDWHERE
           CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMP0(:,:,:,1,JM),'IBM_SMOOTH_LS::Z_NORM_TEMP0')
           CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMP0(:,:,:,2,JM),'IBM_SMOOTH_LS::Z_NORM_TEMP0')
           CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMP0(:,:,:,3,JM),'IBM_SMOOTH_LS::Z_NORM_TEMP0')
           IF (JM==7) THEN
              CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
              CALL CLEANLIST_ll(TZFIELDS_ll)
           ENDIF
        ENDDO
        !
     ENDIF
     !
     ZTEMP=PPHI
     !
     DO JK=1,IKU-1
        DO JJ=1,IJU-1
           DO JI=2,IIU
              !
              ILISPT_FACTU = 1.-0.5*exp(-abs(PPHI(JI,JJ,JK,2))/ILISPT_FACT)
              !
              ZPW  = PPHI(JI-1,JJ  ,JK  ,1)*Z_NORM_TEMP0(JI,JJ,JK,1,2)
              ZPE  = PPHI(JI  ,JJ  ,JK  ,1)*Z_NORM_TEMP0(JI,JJ,JK,1,2)
              ZPB  = PPHI(JI  ,JJ  ,JK  ,5)*Z_NORM_TEMP0(JI,JJ,JK,2,2) 
              ZPT  = PPHI(JI  ,JJ+1,JK  ,5)*Z_NORM_TEMP0(JI,JJ,JK,2,2)
              ZPS  = PPHI(JI  ,JJ  ,JK  ,6)*Z_NORM_TEMP0(JI,JJ,JK,3,2)
              ZPN  = PPHI(JI  ,JJ  ,JK+1,6)*Z_NORM_TEMP0(JI,JJ,JK,3,2)
              !
              ZTEMP(JI,JJ,JK,2) = (0.+ILISPT_FACTU)*PPHI(JI,JJ,JK,2)+ &
                   (1.-ILISPT_FACTU)*(ZPE+ZPW+ZPB+ZPT+ZPN+ZPS)/6.
              !
           ENDDO
        ENDDO
     ENDDO
     !
     DO JK=1,IKU-1
        DO JJ=2,IJU
           DO JI=1,IIU-1
              !
              ILISPT_FACTV = 1.-0.5*exp(-abs(PPHI(JI,JJ,JK,3))/ILISPT_FACT)
              !
              ZPS  = PPHI(JI  ,JJ  ,JK  ,5)*Z_NORM_TEMP0(JI,JJ,JK,1,3)
              ZPN  = PPHI(JI+1,JJ  ,JK  ,5)*Z_NORM_TEMP0(JI,JJ,JK,1,3)
              ZPW  = PPHI(JI  ,JJ  ,JK  ,1)*Z_NORM_TEMP0(JI,JJ,JK,2,3)
              ZPE  = PPHI(JI  ,JJ-1,JK  ,1)*Z_NORM_TEMP0(JI,JJ,JK,2,3)
              ZPB  = PPHI(JI  ,JJ  ,JK  ,7)*Z_NORM_TEMP0(JI,JJ,JK,3,3)
              ZPT  = PPHI(JI  ,JJ  ,JK+1,7)*Z_NORM_TEMP0(JI,JJ,JK,3,3)
              !
              ZTEMP(JI,JJ,JK,3) = (0.+ILISPT_FACTV)*PPHI(JI,JJ,JK,3)+ &
                   (1.-ILISPT_FACTV)*(ZPE+ZPW+ZPB+ZPT+ZPN+ZPS)/6.
              !
           ENDDO
        ENDDO
     ENDDO
     !
     DO JK=2,IKU
        DO JJ=1,IJU-1
           DO JI=1,IIU-1
              !
              ILISPT_FACTW = 1.-0.5*exp(-abs(PPHI(JI,JJ,JK,4))/ILISPT_FACT)
              !
              ZPB  = PPHI(JI  ,JJ  ,JK  ,6)*Z_NORM_TEMP0(JI,JJ,JK,1,4)
              ZPT  = PPHI(JI+1,JJ  ,JK  ,6)*Z_NORM_TEMP0(JI,JJ,JK,1,4)
              ZPW  = PPHI(JI  ,JJ  ,JK  ,7)*Z_NORM_TEMP0(JI,JJ,JK,2,4)
              ZPE  = PPHI(JI  ,JJ+1,JK  ,7)*Z_NORM_TEMP0(JI,JJ,JK,2,4)
              ZPS  = PPHI(JI  ,JJ  ,JK  ,1)*Z_NORM_TEMP0(JI,JJ,JK,3,4)
              ZPN  = PPHI(JI  ,JJ  ,JK-1,1)*Z_NORM_TEMP0(JI,JJ,JK,3,4)
              !
              ZTEMP(JI,JJ,JK,4) = (0.+ILISPT_FACTW)*PPHI(JI,JJ,JK,4)+ &
                   (1.-ILISPT_FACTW)*(ZPE+ZPW+ZPB+ZPT+ZPN+ZPS)/6.
              !
           ENDDO
        ENDDO
     ENDDO
     !
     DO JK=2,IKU-1
        DO JJ=2,IJU-1
           DO JI=2,IIU-1
              !
              ILISPT_FACTP = 1.-0.5*exp(-abs(PPHI(JI,JJ,JK,1))/ILISPT_FACT)
              !
              ZPB  = PPHI(JI  ,JJ  ,JK  ,2)*Z_NORM_TEMP0(JI,JJ,JK,1,1)
              ZPT  = PPHI(JI+1,JJ  ,JK  ,2)*Z_NORM_TEMP0(JI,JJ,JK,1,1)
              ZPW  = PPHI(JI  ,JJ  ,JK  ,3)*Z_NORM_TEMP0(JI,JJ,JK,2,1)
              ZPE  = PPHI(JI  ,JJ+1,JK  ,3)*Z_NORM_TEMP0(JI,JJ,JK,2,1)
              ZPS  = PPHI(JI  ,JJ  ,JK  ,4)*Z_NORM_TEMP0(JI,JJ,JK,3,1)
              ZPN  = PPHI(JI  ,JJ  ,JK+1,4)*Z_NORM_TEMP0(JI,JJ,JK,3,1)
              !
              ZTEMP(JI,JJ,JK,1) = (0.+ILISPT_FACTP)*PPHI(JI,JJ,JK,1)+ &
                   (1.-ILISPT_FACTP)*(ZPE+ZPW+ZPB+ZPT+ZPN+ZPS)/6.
              !
           ENDDO
        ENDDO
     ENDDO
     !
     DO JK=1,IKU-1
        DO JJ=2,IJU
           DO JI=2,IIU
              !
              ILISPT_FACTP = 1.-0.5*exp(-abs(PPHI(JI,JJ,JK,5))/ILISPT_FACT)
              !
              ZPW  = PPHI(JI  ,JJ  ,JK  ,3)*Z_NORM_TEMP0(JI,JJ,JK,1,5)
              ZPE  = PPHI(JI-1,JJ  ,JK  ,3)*Z_NORM_TEMP0(JI,JJ,JK,1,5)
              ZPB  = PPHI(JI  ,JJ  ,JK  ,2)*Z_NORM_TEMP0(JI,JJ,JK,2,5) 
              ZPT  = PPHI(JI  ,JJ-1,JK  ,2)*Z_NORM_TEMP0(JI,JJ,JK,2,5)
              ZPS  = (PPHI(JI  ,JJ  ,JK  ,4)+PPHI(JI-1,JJ-1,JK  ,4)+PPHI(JI-1,JJ  ,JK  ,4)+PPHI(JI  ,JJ-1,JK  ,4))* &
                   0.25*Z_NORM_TEMP0(JI,JJ,JK,3,5)
              ZPN  = (PPHI(JI  ,JJ  ,JK+1,4)+PPHI(JI-1,JJ-1,JK+1,4)+PPHI(JI-1,JJ  ,JK+1,4)+PPHI(JI  ,JJ-1,JK+1,4))* &
                   0.25*Z_NORM_TEMP0(JI,JJ,JK,3,5)
              !
              ZTEMP(JI,JJ,JK,5) = (0.+ILISPT_FACTP)*PPHI(JI,JJ,JK,5)+ &
                   (1.-ILISPT_FACTP)*(ZPE+ZPW+ZPB+ZPT+ZPS+ZPN)/6.
              !
           ENDDO
        ENDDO
     ENDDO
     !
     DO JK=2,IKU
        DO JJ=1,IJU-1
           DO JI=2,IIU
              !
              ILISPT_FACTP = 1.-0.5*exp(-abs(PPHI(JI,JJ,JK,6))/ILISPT_FACT)
              !
              ZPW  = PPHI(JI-1,JJ  ,JK  ,4)*Z_NORM_TEMP0(JI,JJ,JK,1,6)
              ZPE  = PPHI(JI  ,JJ  ,JK  ,4)*Z_NORM_TEMP0(JI,JJ,JK,1,6)
              ZPS  = (PPHI(JI  ,JJ  ,JK  ,3)+PPHI(JI-1,JJ  ,JK-1,3)+PPHI(JI-1,JJ  ,JK  ,3)+PPHI(JI  ,JJ  ,JK-1,3))* &
                   0.25*Z_NORM_TEMP0(JI,JJ,JK,2,6)
              ZPN  = (PPHI(JI  ,JJ+1,JK  ,3)+PPHI(JI-1,JJ+1,JK-1,3)+PPHI(JI-1,JJ+1,JK  ,3)+PPHI(JI  ,JJ+1,JK-1,3))* &
                   0.25*Z_NORM_TEMP0(JI,JJ,JK,2,6)
              ZPB  = PPHI(JI  ,JJ  ,JK  ,2)*Z_NORM_TEMP0(JI,JJ,JK,3,6) 
              ZPT  = PPHI(JI  ,JJ  ,JK-1,2)*Z_NORM_TEMP0(JI,JJ,JK,3,6)
              !
              ZTEMP(JI,JJ,JK,6) = (0.+ILISPT_FACTP)*PPHI(JI,JJ,JK,6)+ &
                   (1.-ILISPT_FACTP)*(ZPE+ZPW+ZPB+ZPT+ZPS+ZPN)/6.
              !
           ENDDO
        ENDDO
     ENDDO
     !
     DO JK=2,IKU
        DO JJ=2,IJU
           DO JI=1,IIU-1
              !
              ILISPT_FACTP = 1.-0.5*exp(-abs(PPHI(JI,JJ,JK,7))/ILISPT_FACT)
              !
              ZPW  = (PPHI(JI  ,JJ  ,JK  ,2)+PPHI(JI  ,JJ-1,JK-1,2)+PPHI(JI  ,JJ-1,JK  ,2)+PPHI(JI  ,JJ  ,JK-1,2))* &
                   0.25*Z_NORM_TEMP0(JI,JJ,JK,1,7)
              ZPE  = (PPHI(JI+1,JJ  ,JK  ,2)+PPHI(JI+1,JJ-1,JK-1,2)+PPHI(JI+1,JJ-1,JK  ,2)+PPHI(JI+1,JJ  ,JK-1,2))* &
                   0.25*Z_NORM_TEMP0(JI,JJ,JK,1,7)
              ZPB  = PPHI(JI  ,JJ  ,JK  ,4)*Z_NORM_TEMP0(JI,JJ,JK,2,7) 
              ZPT  = PPHI(JI  ,JJ-1,JK  ,4)*Z_NORM_TEMP0(JI,JJ,JK,2,7)
              ZPS  = PPHI(JI  ,JJ  ,JK  ,3)*Z_NORM_TEMP0(JI,JJ,JK,3,7)
              ZPN  = PPHI(JI  ,JJ  ,JK-1,3)*Z_NORM_TEMP0(JI,JJ,JK,3,7)
              !
              ZTEMP(JI,JJ,JK,7) = (0.+ILISPT_FACTP)*PPHI(JI,JJ,JK,7)+ &
                   (1.-ILISPT_FACTP)*(ZPB+ZPT+ZPN+ZPS+ZPW+ZPE)/6.
              !
           ENDDO
        ENDDO
     ENDDO
     !
     IF (JL>4) THEN
        WHERE ((PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).LT.PPHI(IIB  :IIE  ,IJB:IJE,IKB:IKE,2)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).LT.PPHI(IIB+1:IIE+1,IJB:IJE,IKB:IKE,2)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).LT.PPHI(IIB:IIE,IJB  :IJE  ,IKB:IKE,3)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).LT.PPHI(IIB:IIE,IJB+1:IJE+1,IKB:IKE,3)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).LT.PPHI(IIB:IIE,IJB:IJE,IKB  :IKE  ,4)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).LT.PPHI(IIB:IIE,IJB:IJE,IKB+1:IKE+1,4)).AND.&
             PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).LT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,IKB:IKE,1)   =PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).LT.PPHI(IIB  :IIE  ,IJB:IJE,IKB:IKE,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).LT.PPHI(IIB-1:IIE-1,IJB:IJE,IKB:IKE,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).LT.PPHI(IIB:IIE,IJB  :IJE  ,IKB:IKE,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).LT.PPHI(IIB:IIE,IJB+1:IJE+1,IKB:IKE,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).LT.PPHI(IIB:IIE,IJB:IJE,IKB  :IKE  ,6)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).LT.PPHI(IIB:IIE,IJB:IJE,IKB+1:IKE+1,6)).AND.&
             PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).LT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,IKB:IKE,2)   =PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).LT.PPHI(IIB  :IIE  ,IJB:IJE,IKB:IKE,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).LT.PPHI(IIB+1:IIE+1,IJB:IJE,IKB:IKE,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).LT.PPHI(IIB:IIE,IJB  :IJE  ,IKB:IKE,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).LT.PPHI(IIB:IIE,IJB-1:IJE-1,IKB:IKE,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).LT.PPHI(IIB:IIE,IJB:IJE,IKB  :IKE  ,7)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).LT.PPHI(IIB:IIE,IJB:IJE,IKB+1:IKE+1,7)).AND.&
             PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).LT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,IKB:IKE,3)   =PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).LT.PPHI(IIB  :IIE  ,IJB:IJE,IKB:IKE,6)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).LT.PPHI(IIB+1:IIE+1,IJB:IJE,IKB:IKE,6)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).LT.PPHI(IIB:IIE,IJB  :IJE  ,IKB:IKE,7)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).LT.PPHI(IIB:IIE,IJB+1:IJE+1,IKB:IKE,7)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).LT.PPHI(IIB:IIE,IJB:IJE,IKB  :IKE  ,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).LT.PPHI(IIB:IIE,IJB:IJE,IKB-1:IKE-1,1)).AND.&
             PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).LT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,IKB:IKE,4)   =PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).GT.PPHI(IIB  :IIE  ,IJB:IJE,IKB:IKE,2)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).GT.PPHI(IIB+1:IIE+1,IJB:IJE,IKB:IKE,2)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).GT.PPHI(IIB:IIE,IJB  :IJE  ,IKB:IKE,3)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).GT.PPHI(IIB:IIE,IJB+1:IJE+1,IKB:IKE,3)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).GT.PPHI(IIB:IIE,IJB:IJE,IKB  :IKE  ,4)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).GT.PPHI(IIB:IIE,IJB:IJE,IKB+1:IKE+1,4)).AND.&
             PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1).GT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,IKB:IKE,1)   =PPHI(IIB:IIE,IJB:IJE,IKB:IKE,1)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).GT.PPHI(IIB  :IIE  ,IJB:IJE,IKB:IKE,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).GT.PPHI(IIB-1:IIE-1,IJB:IJE,IKB:IKE,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).GT.PPHI(IIB:IIE,IJB  :IJE  ,IKB:IKE,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).GT.PPHI(IIB:IIE,IJB+1:IJE+1,IKB:IKE,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).GT.PPHI(IIB:IIE,IJB:IJE,IKB  :IKE  ,6)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).GT.PPHI(IIB:IIE,IJB:IJE,IKB+1:IKE+1,6)).AND.&
             PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2).GT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,IKB:IKE,2)   =PPHI(IIB:IIE,IJB:IJE,IKB:IKE,2)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).GT.PPHI(IIB  :IIE  ,IJB:IJE,IKB:IKE,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).GT.PPHI(IIB+1:IIE+1,IJB:IJE,IKB:IKE,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).GT.PPHI(IIB:IIE,IJB  :IJE  ,IKB:IKE,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).GT.PPHI(IIB:IIE,IJB-1:IJE-1,IKB:IKE,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).GT.PPHI(IIB:IIE,IJB:IJE,IKB  :IKE  ,7)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).GT.PPHI(IIB:IIE,IJB:IJE,IKB+1:IKE+1,7)).AND.&
             PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3).GT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,IKB:IKE,3)   =PPHI(IIB:IIE,IJB:IJE,IKB:IKE,3)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).GT.PPHI(IIB  :IIE  ,IJB:IJE,IKB:IKE,6)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).GT.PPHI(IIB+1:IIE+1,IJB:IJE,IKB:IKE,6)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).GT.PPHI(IIB:IIE,IJB  :IJE  ,IKB:IKE,7)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).GT.PPHI(IIB:IIE,IJB+1:IJE+1,IKB:IKE,7)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).GT.PPHI(IIB:IIE,IJB:IJE,IKB  :IKE  ,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).GT.PPHI(IIB:IIE,IJB:IJE,IKB-1:IKE-1,1)).AND.&
             PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4).GT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,IKB:IKE,4)   =PPHI(IIB:IIE,IJB:IJE,IKB:IKE,4)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,2,5).GT.PPHI(IIB  :IIE  ,IJB:IJE,2,3)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,5).GT.PPHI(IIB-1:IIE-1,IJB:IJE,2,3)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,5).GT.PPHI(IIB:IIE,IJB  :IJE  ,2,2)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,5).GT.PPHI(IIB:IIE,IJB-1:IJE-1,2,2)).AND.PPHI(IIB:IIE,IJB:IJE,2,5).GT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,2,5)   =PPHI(IIB:IIE,IJB:IJE,2,5)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,2,2).GT.PPHI(IIB  :IIE  ,IJB:IJE,2,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,2).GT.PPHI(IIB-1:IIE-1,IJB:IJE,2,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,2).GT.PPHI(IIB:IIE,IJB  :IJE  ,2,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,2).GT.PPHI(IIB:IIE,IJB+1:IJE+1,2,5)).AND.PPHI(IIB:IIE,IJB:IJE,2,2).GT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,2,2)   =PPHI(IIB:IIE,IJB:IJE,2,2)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,2,3).GT.PPHI(IIB  :IIE  ,IJB:IJE,2,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,3).GT.PPHI(IIB+1:IIE+1,IJB:IJE,2,5)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,3).GT.PPHI(IIB:IIE,IJB  :IJE  ,2,1)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,3).GT.PPHI(IIB:IIE,IJB-1:IJE-1,2,1)).AND.PPHI(IIB:IIE,IJB:IJE,2,3).GT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,2,3)   =PPHI(IIB:IIE,IJB:IJE,2,3)
        ENDWHERE
        WHERE ((PPHI(IIB:IIE,IJB:IJE,2,1).GT.PPHI(IIB  :IIE  ,IJB:IJE,2,2)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,1).GT.PPHI(IIB+1:IIE+1,IJB:IJE,2,2)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,1).GT.PPHI(IIB:IIE,IJB  :IJE  ,2,3)).AND.&
             (PPHI(IIB:IIE,IJB:IJE,2,1).GT.PPHI(IIB:IIE,IJB+1:IJE+1,2,3)).AND.PPHI(IIB:IIE,IJB:IJE,2,1).GT.XIBM_EPSI)
           ZTEMP(IIB:IIE,IJB:IJE,2,1)   =PPHI(IIB:IIE,IJB:IJE,2,1)
        ENDWHERE
     ENDIF
     !
     ZTEMP(:,:,1,5) = 2.*ZTEMP(:,:,2,5)-ZTEMP(:,:,3,5)
     ZTEMP(:,:,1,3) = 2.*ZTEMP(:,:,2,3)-ZTEMP(:,:,3,3)
     ZTEMP(:,:,1,2) = 2.*ZTEMP(:,:,2,2)-ZTEMP(:,:,3,2)
     ZTEMP(:,:,1,1) = 2.*ZTEMP(:,:,2,1)-ZTEMP(:,:,3,1)
     WHERE (ZTEMP(:,:,2,5).GT.XIBM_EPSI)  ZTEMP(:,:,1,5) = ZTEMP(:,:,2,5)
     WHERE (ZTEMP(:,:,2,3).GT.XIBM_EPSI)  ZTEMP(:,:,1,3) = ZTEMP(:,:,2,3)
     WHERE (ZTEMP(:,:,2,2).GT.XIBM_EPSI)  ZTEMP(:,:,1,2) = ZTEMP(:,:,2,2)
     WHERE (ZTEMP(:,:,2,1).GT.XIBM_EPSI)  ZTEMP(:,:,1,1) = ZTEMP(:,:,2,1)
     ZTEMP(:,:,2,6) =   (ZTEMP(:,:,2,2)+ZTEMP(:,:,1,2))/2.
     ZTEMP(:,:,2,7) =   (ZTEMP(:,:,2,3)+ZTEMP(:,:,1,3))/2.
     ZTEMP(:,:,2,4) =   (ZTEMP(:,:,2,1)+ZTEMP(:,:,1,1))/2.
     ZTEMP(:,:,1,6) = 2.*ZTEMP(:,:,2,6)-ZTEMP(:,:,3,6)
     ZTEMP(:,:,1,7) = 2.*ZTEMP(:,:,2,7)-ZTEMP(:,:,3,7)
     ZTEMP(:,:,1,4) = 2.*ZTEMP(:,:,2,4)-ZTEMP(:,:,3,4)
     WHERE (ZTEMP(:,:,2,6).GT.XIBM_EPSI)  ZTEMP(:,:,1,6) = ZTEMP(:,:,2,6)
     WHERE (ZTEMP(:,:,2,7).GT.XIBM_EPSI)  ZTEMP(:,:,1,7) = ZTEMP(:,:,2,7)
     WHERE (ZTEMP(:,:,2,4).GT.XIBM_EPSI)  ZTEMP(:,:,1,4) = ZTEMP(:,:,2,4)
     ZTEMP(:,:,IKU,5) = 2.*ZTEMP(:,:,IKU-1,5)-ZTEMP(:,:,IKU-2,5)
     ZTEMP(:,:,IKU,3) = 2.*ZTEMP(:,:,IKU-1,3)-ZTEMP(:,:,IKU-2,3)
     ZTEMP(:,:,IKU,2) = 2.*ZTEMP(:,:,IKU-1,2)-ZTEMP(:,:,IKU-2,2)
     ZTEMP(:,:,IKU,1) = 2.*ZTEMP(:,:,IKU-1,1)-ZTEMP(:,:,IKU-2,1)
     WHERE (ZTEMP(:,:,IKU-1,5).GT.XIBM_EPSI)  ZTEMP(:,:,IKU,5) = ZTEMP(:,:,IKU-1,5)
     WHERE (ZTEMP(:,:,IKU-1,3).GT.XIBM_EPSI)  ZTEMP(:,:,IKU,3) = ZTEMP(:,:,IKU-1,3)
     WHERE (ZTEMP(:,:,IKU-1,2).GT.XIBM_EPSI)  ZTEMP(:,:,IKU,2) = ZTEMP(:,:,IKU-1,2)
     WHERE (ZTEMP(:,:,IKU-1,1).GT.XIBM_EPSI)  ZTEMP(:,:,IKU,1) = ZTEMP(:,:,IKU-1,1)
     ZTEMP(:,:,IKU,6) =   (ZTEMP(:,:,IKU-1,2)+ZTEMP(:,:,IKU,2))/2.
     ZTEMP(:,:,IKU,7) =   (ZTEMP(:,:,IKU-1,3)+ZTEMP(:,:,IKU,3))/2.
     ZTEMP(:,:,IKU,4) =   (ZTEMP(:,:,IKU-1,1)+ZTEMP(:,:,IKU,1))/2.
     WHERE (ZTEMP(:,:,IKU-1,6).GT.XIBM_EPSI)  ZTEMP(:,:,IKU,6) = ZTEMP(:,:,IKU-1,6)
     WHERE (ZTEMP(:,:,IKU-1,7).GT.XIBM_EPSI)  ZTEMP(:,:,IKU,7) = ZTEMP(:,:,IKU-1,7)
     WHERE (ZTEMP(:,:,IKU-1,4).GT.XIBM_EPSI)  ZTEMP(:,:,IKU,4) = ZTEMP(:,:,IKU-1,4)
     !
     WHERE (ABS(ZTEMP(:,:,:,:)).LT.(ZREF-2.*XIBM_EPSI)) ZTEMP(:,:,:,:) = ZREF-XIBM_EPSI
     !
     NULLIFY(TZFIELDS_ll)
     DO JM=1,7
        ! Boundary symmetry
        IF (LWEST_ll ()) ZTEMP(2  ,:,:,JM) = ZTEMP(    3,:,:,JM)
        IF (LEAST_ll ()) ZTEMP(IIU-1,:,:,JM) = ZTEMP(IIU-2,:,:,JM)
        IF (LSOUTH_ll()) ZTEMP(:,2  ,:,JM) = ZTEMP(:,    3,:,JM)
        IF (LNORTH_ll()) ZTEMP(:,IJU-1,:,JM) = ZTEMP(:,IJU-2,:,JM)
        IF (LWEST_ll ()) ZTEMP(1  ,:,:,JM) = ZTEMP(    2,:,:,JM)
        IF (LEAST_ll ()) ZTEMP(IIU,:,:,JM) = ZTEMP(IIU-1,:,:,JM)
        IF (LSOUTH_ll()) ZTEMP(:,1  ,:,JM) = ZTEMP(:,    2,:,JM)
        IF (LNORTH_ll()) ZTEMP(:,IJU,:,JM) = ZTEMP(:,IJU-1,:,JM)
        !
        IF(LWEST_ll()) THEN
           ZTEMP(IIB-1,IJB:IJE,IKB-1,JM)=ZTEMP(IIB-1,IJB:IJE,IKB,JM)
           ZTEMP(IIB-1,IJB:IJE,IKE+1,JM)=ZTEMP(IIB-1,IJB:IJE,IKE,JM)
        END IF
        !
        IF (LEAST_ll()) THEN
           ZTEMP(IIE+1,IJB:IJE,IKB-1,JM)=ZTEMP(IIE+1,IJB:IJE,IKB,JM)
           ZTEMP(IIE+1,IJB:IJE,IKE+1,JM)=ZTEMP(IIE+1,IJB:IJE,IKE,JM)
        END IF
        !
        IF (LSOUTH_ll()) THEN
           ZTEMP(IIB:IIE,IJB-1,IKB-1,JM)=ZTEMP(IIB:IIE,IJB-1,IKB,JM)
           ZTEMP(IIB:IIE,IJB-1,IKE+1,JM)=ZTEMP(IIB:IIE,IJB-1,IKE,JM)
        END IF
        !
        IF (LNORTH_ll()) THEN
           ZTEMP(IIB:IIE,IJE+1,IKB-1,JM)=ZTEMP(IIB:IIE,IJE+1,IKB,JM)
           ZTEMP(IIB:IIE,IJE+1,IKE+1,JM)=ZTEMP(IIB:IIE,IJE+1,IKE,JM)
        END IF
        CALL ADD3DFIELD_ll(TZFIELDS_ll,ZTEMP(:,:,:,JM),'IBM_SMOOTH_LS::ZTEMP')
     ENDDO
     CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
     CALL CLEANLIST_ll(TZFIELDS_ll)
     !
     PPHI = ZTEMP
     !
  ENDDO
  !
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  ! 
  DEALLOCATE(ZTEMP,Z_NORM_TEMP0,Z_NORM_TEMP1)
  RETURN
  !
END SUBROUTINE IBM_SMOOTH_LS
