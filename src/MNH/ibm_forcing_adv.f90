!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!      ###########################
MODULE MODI_IBM_FORCING_ADV
  !    ###########################
  !
  INTERFACE
     !
     SUBROUTINE IBM_FORCING_ADV(PRUS,PRVS,PRWS)
       !
       REAL, DIMENSION(:,:,:) , INTENT(INOUT) :: PRUS,PRVS,PRWS
       !
     END SUBROUTINE IBM_FORCING_ADV
     !
  END INTERFACE
  !
END MODULE MODI_IBM_FORCING_ADV
!
!
!       ##########################################
SUBROUTINE IBM_FORCING_ADV(PRUS,PRVS,PRWS)
  !       ##########################################
  !
  !!****     *IBM_FORCING_ADV*  - routine to force all desired fields in the RK
  !!
  !!      PURPOSE
  !!      -------
  !         The purpose of this routine is to compute variables in the virtual
  !         embedded solid region in regard of variables computed in the real
  !         fluid region
  !
  !!      METHOD
  !!      ------
  !!
  !!      EXTERNAL
  !!      --------
  !!        NONE
  !!
  !!      IMPLICIT ARGUMENTS
  !!      ------------------
  !!
  !!      REFERENCE
  !!      ---------
  !!
  !!      AUTHOR
  !!      ------
  !!        Franck Auguste       * CERFACS(AE) *
  !!
  !!      MODIFICATIONS
  !!      -------------
  !!        Original          01/01/2019
  !!
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
  USE MODD_CST
  USE MODD_FIELD_n
  USE MODD_REF
  USE MODD_REF_n, ONLY: XRHODJ,XRHODREF,XTHVREF,XEXNREF,XRVREF
  USE MODD_PARAMETERS, ONLY: JPVEXT,JPHEXT
  USE MODD_IBM_PARAM_n
  USE MODD_LBC_n
  USE MODD_CONF
  USE MODD_CONF_n
  USE MODD_NSV
  USE MODD_CTURB
  USE MODD_PARAM_n
  USE MODD_DYN_n, ONLY: XTSTEP
  !
  ! interface
  USE MODI_IBM_AFFECTV
  USE MODI_IBM_AFFECTP
  USE MODI_SHUMAN
  !
  IMPLICIT NONE
  !
  !-----------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments                                
  REAL, DIMENSION(:,:,:) , INTENT(INOUT) :: PRUS,PRVS,PRWS
  !
  !-----------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  REAL, DIMENSION(:,:,:)   , ALLOCATABLE :: ZTMP,ZXMU,ZDIV,ZTKE
  REAL, DIMENSION(:,:,:,:) , ALLOCATABLE :: ZTMU,ZTRY
  INTEGER                                :: IIU,IJU,IKU,IKB,IKE
  TYPE(LIST_ll), POINTER                 :: TZFIELDS_ll   
  INTEGER                                :: IINFO_ll
  !
  !-----------------------------------------------------------------------------
  !
  !**** 0. ALLOCATIONS
  !     --------------
  !
  IIU = SIZE(PRUS,1)
  IJU = SIZE(PRVS,2)
  IKU = SIZE(PRWS,3)
  ALLOCATE(ZTMU(IIU,IJU,IKU,3),ZTMP(IIU,IJU,IKU),ZTRY(IIU,IJU,IKU,3), &
       ZXMU(IIU,IJU,IKU),ZDIV(IIU,IJU,IKU),ZTKE(IIU,IJU,IKU))
  !
  ZTMU=0.
  ZXMU=0.
  ZDIV=0.
  ZTMP=0.
  ZTRY=0.
  !
  IKB =   1 + JPVEXT
  IKE = IKU - JPVEXT
  !
  !-----------------------------------------------------------------------------
  !       
  !**** 1. PRELIMINARIES
  !     ----------------
  WHERE (XIBM_LS(:,:,:,2).GT.-XIBM_EPSI) PRUS(:,:,:) = XIBM_EPSI
  WHERE (XIBM_LS(:,:,:,3).GT.-XIBM_EPSI) PRVS(:,:,:) = XIBM_EPSI
  WHERE (XIBM_LS(:,:,:,4).GT.-XIBM_EPSI) PRWS(:,:,:) = XIBM_EPSI
  !
  !**** 2. EXECUTIONS
  !     ------------- 
  ZTMU(:,:,:,1) = PRUS(:,:,:)/MXM(XRHODREF)
  ZTMU(:,:,:,2) = PRVS(:,:,:)/MYM(XRHODREF)
  ZTMU(:,:,:,3) = PRWS(:,:,:)/MZM(XRHODREF)
  !
  ZTMP(:,:,:) = PRUS(:,:,:)/MXM(XRHODREF)
  ZXMU(:,:,:) = MXM(XIBM_XMUT(:,:,:))
  ZDIV(:,:,:) = MXM(XIBM_CURV(:,:,:))
  CALL IBM_AFFECTV(ZTMP,ZTMU,ZTRY,'U',NIBM_LAYER_V,CIBM_MODE_INTE3_V,&
       CIBM_FORC_BOUNR_V,XIBM_RADIUS_V,XIBM_POWERS_V,&
       CIBM_MODE_INTE1NV,CIBM_TYPE_BOUNN_V,CIBM_MODE_BOUNN_V,CIBM_FORC_BOUNN_V ,XIBM_FORC_BOUNN_V,&
       CIBM_MODE_INTE1TV,CIBM_TYPE_BOUNT_V,CIBM_MODE_BOUNT_V,CIBM_FORC_BOUNT_V ,XIBM_FORC_BOUNT_V,&
       CIBM_MODE_INTE1CV,CIBM_TYPE_BOUNC_V,CIBM_MODE_BOUNC_V,CIBM_FORC_BOUNC_V ,XIBM_FORC_BOUNC_V,ZXMU,ZDIV)
  PRUS(:,:,:) = ZTMP(:,:,:)*MXM(XRHODREF)
  !
  ZTMP(:,:,:) = PRVS(:,:,:)/MYM(XRHODREF)
  ZXMU(:,:,:) = MYM(XIBM_XMUT(:,:,:))
  ZDIV(:,:,:) = MYM(XIBM_CURV(:,:,:))
  CALL IBM_AFFECTV(ZTMP,ZTMU,ZTRY,'V',NIBM_LAYER_V,CIBM_MODE_INTE3_V,&
       CIBM_FORC_BOUNR_V,XIBM_RADIUS_V,XIBM_POWERS_V,&
       CIBM_MODE_INTE1NV,CIBM_TYPE_BOUNN_V,CIBM_MODE_BOUNN_V,CIBM_FORC_BOUNN_V ,XIBM_FORC_BOUNN_V,&
       CIBM_MODE_INTE1TV,CIBM_TYPE_BOUNT_V,CIBM_MODE_BOUNT_V,CIBM_FORC_BOUNT_V ,XIBM_FORC_BOUNT_V,&
       CIBM_MODE_INTE1CV,CIBM_TYPE_BOUNC_V,CIBM_MODE_BOUNC_V,CIBM_FORC_BOUNC_V ,XIBM_FORC_BOUNC_V,ZXMU,ZDIV)
  PRVS(:,:,:) = ZTMP(:,:,:)*MYM(XRHODREF)
  !
  ZTMP(:,:,:) = PRWS(:,:,:)/MZM(XRHODREF)
  ZXMU(:,:,:) = MZM(XIBM_XMUT(:,:,:))
  ZDIV(:,:,:) = MZM(XIBM_CURV(:,:,:))
  CALL IBM_AFFECTV(ZTMP,ZTMU,ZTRY,'W',NIBM_LAYER_V,CIBM_MODE_INTE3_V,&
       CIBM_FORC_BOUNR_V,XIBM_RADIUS_V,XIBM_POWERS_V,&
       CIBM_MODE_INTE1NV,CIBM_TYPE_BOUNN_V,CIBM_MODE_BOUNN_V,CIBM_FORC_BOUNN_V ,XIBM_FORC_BOUNN_V,&
       CIBM_MODE_INTE1TV,CIBM_TYPE_BOUNT_V,CIBM_MODE_BOUNT_V,CIBM_FORC_BOUNT_V ,XIBM_FORC_BOUNT_V,&
       CIBM_MODE_INTE1CV,CIBM_TYPE_BOUNC_V,CIBM_MODE_BOUNC_V,CIBM_FORC_BOUNC_V ,XIBM_FORC_BOUNC_V,ZXMU,ZDIV)
  PRWS(:,:,:) = ZTMP(:,:,:)*MZM(XRHODREF)
  !
  !**** 3. COMMUNICATIONS 
  !     -----------------
  !
  NULLIFY(TZFIELDS_ll)
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PRUS(:,:,:),'IBM_FORCING_ADV::PRUS')
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PRVS(:,:,:),'IBM_FORCING_ADV::PRVS')
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PRWS(:,:,:),'IBM_FORCING_ADV::PRWS')
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
  !
  !**** 4. DEALLOCATIONS
  !     ----------------
  !
  DEALLOCATE(ZTMP,ZTMU,ZTRY,ZXMU,ZDIV,ZTKE)
  !
  RETURN
  ! 
END SUBROUTINE IBM_FORCING_ADV
