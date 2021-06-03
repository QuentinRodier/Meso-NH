!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!       ####################
MODULE MODI_IBM_INIT
  !       ####################
  !
  INTERFACE
     !
     SUBROUTINE IBM_INIT(PIBM_LS)
       !
       REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PIBM_LS
       !
     END SUBROUTINE IBM_INIT
     !
  END INTERFACE
  !
END MODULE MODI_IBM_INIT
!
!       ############################
SUBROUTINE IBM_INIT(PIBM_LS)
  !       ############################
  !
  !****     *IBM_INIT*  - routine to initialize the immersed boundary method 
  !
  !      PURPOSE
  !      -------
  !         The purpose of this routine is to initialize the IBM variables 
  !         that are stored in module MODD_IBM_PARAM_n
  !
  !      METHOD
  !      ------
  !        The constants are set to their numerical values
  !
  !      EXTERNAL
  !      --------
  !        NONE
  !
  !      IMPLICIT ARGUMENTS
  !      ------------------
  !
  !      REFERENCE
  !      ---------
  !
  !      AUTHOR
  !      ------
  !        Franck Auguste       * CERFACS(AE) *
  !
  !      MODIFICATIONS
  !      -------------
  !        Original          01/01/2019
  !
  !------------------------------------------------------------------------------
  !       
  !**** 0. DECLARATIONS
  !     ---------------
  !
  ! module
  USE MODE_POS
  USE MODE_ll
  USE MODE_IO
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll
  !
  ! declaration
  USE MODD_SUB_MODEL_n, ONLY: XT_IBM_DETE
  USE MODD_IBM_PARAM_n
  USE MODD_FIELD_n
  USE MODD_PARAMETERS, ONLY: JPVEXT,JPHEXT
  USE MODD_GRID
  USE MODD_GRID_n
  USE MODD_CST         
  USE MODD_METRICS_n, ONLY: XDXX,XDYY,XDZZ,XDZX,XDZY
  USE MODD_VAR_ll, ONLY: IP
  USE MODD_CONF
  USE MODD_REF_n
  USE MODN_PARAM_n
  !
  ! interface
  USE MODI_IBM_DETECT
  USE MODI_SECOND_MNH
  USE MODI_SHUMAN
  USE MODI_IBM_VOLUME
  USE MODI_GRADIENT_M
  !
  IMPLICIT NONE
  !
  !-----------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PIBM_LS
  !
  !-----------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  REAL                   :: ZTIME1,ZTIME2
  INTEGER                :: IINFO_ll
  TYPE(LIST_ll), POINTER :: TZFIELDS_ll
  INTEGER                :: IIE,IIB,IJB,IJE,IKE,IKB,JN,IIU,IJU,IKU
  !
  !-----------------------------------------------------------------------------
  !
  !****  0.  ALLOCATION
  !     ---------------
  !
  CALL GET_DIM_EXT_ll('B',IIU,IJU)
  IKU=SIZE(XZZ,3)
  IIB=1+JPHEXT
  IIE=IIU-JPHEXT
  IJB=1+JPHEXT
  IJE=IJU-JPHEXT
  IKB=1+JPVEXT
  IKE=IKU-JPVEXT
  !
  ALLOCATE(XIBM_CURV(SIZE(PIBM_LS,1),SIZE(PIBM_LS,2),SIZE(PIBM_LS,3)  )) ; XIBM_CURV = 0.
  ALLOCATE(XIBM_SU  (SIZE(PIBM_LS,1),SIZE(PIBM_LS,2),SIZE(PIBM_LS,3),3)) ; XIBM_SU   = 0.
  IF (LIBM_TROUBLE) THEN
     ALLOCATE(XIBM_SUTR(SIZE(PIBM_LS,1),SIZE(PIBM_LS,2),SIZE(PIBM_LS,3),4)) ; XIBM_SUTR = 1.
  ENDIF
  !
  !------------------------------------------------------------------------------
  !       
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  ZTIME1=0.
  ZTIME2=0.
  XT_IBM_DETE = 0.
  CALL SECOND_MNH(ZTIME1)
  !
  !-------------------------------------------------------------------------------
  !
  !**** 2. EXECUTIONS
  !     -------------
  !
  !=== Level Set function
  JN=1
  PIBM_LS(:,:,IKB-1,JN)=2*PIBM_LS(:,:,IKB,JN)-PIBM_LS(:,:,IKB+1,JN)
  PIBM_LS(:,:,IKE+1,JN)=2*PIBM_LS(:,:,IKE,JN)-PIBM_LS(:,:,IKE-1,JN)
  IF (LWEST_ll ()) THEN
     PIBM_LS(IIB  ,:,:,JN) = PIBM_LS(    IIB+1,:,:,JN)
     PIBM_LS(IIB-1,:,:,JN) = PIBM_LS(    IIB  ,:,:,JN)
  ENDIF
  IF (LEAST_ll ()) THEN
     PIBM_LS(IIE  ,:,:,JN) = PIBM_LS(    IIE-1,:,:,JN)
     PIBM_LS(IIE+1,:,:,JN) = PIBM_LS(    IIE  ,:,:,JN)
  ENDIF
  IF (LSOUTH_ll()) THEN
     PIBM_LS(:,IJB  ,:,JN) = PIBM_LS(:,    IJB+1,:,JN)
     PIBM_LS(:,IJB-1,:,JN) = PIBM_LS(:,    IJB  ,:,JN)
  ENDIF
  IF (LNORTH_ll()) THEN
     PIBM_LS(:,IJE  ,:,JN) = PIBM_LS(:,    IJE-1,:,JN)
     PIBM_LS(:,IJE+1,:,JN) = PIBM_LS(:,    IJE  ,:,JN)
  ENDIF
  !
  PIBM_LS(:,:,:,2)=MXM(PIBM_LS(:,:,:,1))
  PIBM_LS(:,:,:,3)=MYM(PIBM_LS(:,:,:,1))
  PIBM_LS(:,:,:,4)=MZM(PIBM_LS(:,:,:,1))
  !
  NULLIFY(TZFIELDS_ll)
  DO JN=2,4
     PIBM_LS(:,:,IKB-1,JN)=2*PIBM_LS(:,:,IKB,JN)-PIBM_LS(:,:,IKB+1,JN)
     PIBM_LS(:,:,IKE+1,JN)=2*PIBM_LS(:,:,IKE,JN)-PIBM_LS(:,:,IKE-1,JN)
     IF (LWEST_ll ()) THEN
        PIBM_LS(IIB  ,:,:,JN) = PIBM_LS(    IIB+1,:,:,JN)
        PIBM_LS(IIB-1,:,:,JN) = PIBM_LS(    IIB  ,:,:,JN)
     ENDIF
     IF (LEAST_ll ()) THEN
        PIBM_LS(IIE  ,:,:,JN) = PIBM_LS(    IIE-1,:,:,JN)
        PIBM_LS(IIE+1,:,:,JN) = PIBM_LS(    IIE  ,:,:,JN)
     ENDIF
     IF (LSOUTH_ll()) THEN
        PIBM_LS(:,IJB  ,:,JN) = PIBM_LS(:,    IJB+1,:,JN)
        PIBM_LS(:,IJB-1,:,JN) = PIBM_LS(:,    IJB  ,:,JN)
     ENDIF
     IF (LNORTH_ll()) THEN
        PIBM_LS(:,IJE  ,:,JN) = PIBM_LS(:,    IJE-1,:,JN)
        PIBM_LS(:,IJE+1,:,JN) = PIBM_LS(:,    IJE  ,:,JN)
     ENDIF
  ENDDO
  WHERE (ABS(PIBM_LS(:,:,:,:)).LT.XIBM_EPSI) PIBM_LS(:,:,:,:)=2.*XIBM_EPSI
  DO JN=1,4
     CALL ADD3DFIELD_ll(TZFIELDS_ll,PIBM_LS(:,:,:,JN),'IBM_INIT::PIBM_LS')
  ENDDO
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
  !
  !=== detection ghost/images
  IF (IP==1) WRITE(*,*)'*IBM* detection ghost/images detection'
  CALL IBM_DETECT(PIBM_LS) 
  IF (LIBM_TROUBLE) THEN
     NULLIFY(TZFIELDS_ll)
     DO JN=1,4
        CALL ADD3DFIELD_ll(TZFIELDS_ll,XIBM_SUTR(:,:,:,JN),'IBM_INIT::XIBM_SUTR')
     ENDDO
     CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
     CALL CLEANLIST_ll(TZFIELDS_ll)
  ENDIF
  !
  !=== detection surface/volumes
  IF (IP==1) WRITE(*,*)'*IBM* surface/volumes detection'
  CALL IBM_VOLUME(PIBM_LS,XIBM_SU) 
  NULLIFY(TZFIELDS_ll)
  DO JN=1,3
     CALL ADD3DFIELD_ll(TZFIELDS_ll,XIBM_SU(:,:,:,JN),'IBM_INIT::XIBM_SU')
  ENDDO
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
  !
  !=== 
  CALL SECOND_MNH(ZTIME2)
  XT_IBM_DETE=ZTIME2-ZTIME1
  IF (IP==1) WRITE(*,*)'*IBM* End initialization in ',XT_IBM_DETE,' s'
  !
  RETURN
  !
END SUBROUTINE IBM_INIT
