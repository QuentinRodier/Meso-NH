!MNH_LIC Copyright 2019-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!       #######################
MODULE MODI_IBM_INIT_LS
  !       #######################
  !
  INTERFACE
     !
     SUBROUTINE IBM_INIT_LS(PPHI)
       !
       REAL, DIMENSION(:,:,:,:),INTENT(INOUT) :: PPHI 
       !
     END SUBROUTINE IBM_INIT_LS
     !
  END INTERFACE
  !
END MODULE MODI_IBM_INIT_LS
!
!       ############################
SUBROUTINE IBM_INIT_LS(PPHI)
  !       ############################
  !
  !****     *IBM_INIT_LS*  - routine to initialize the Level-Set function for IBM
  !
  !      PURPOSE
  !      -------
  !         The purpose is to compute the LSF at mass/velocity/vorticity nodes 
  !                        to store only its value at the mass node
  !
  !      METHOD
  !      ------
  !        A preparation is done by IBM_PREP depending on the type of topography
  !        A smoothing technique done by IBM_SMOOTH is applied if necessary
  !
  !      EXTERNAL
  !      --------
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
  USE MODE_MSG
  USE MODE_ll
  USE MODE_IO
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll
  !
  ! declaration,      
  USE MODD_SUB_MODEL_n, ONLY: XT_IBM_PREP
  USE MODD_IBM_PARAM_n, ONLY: XIBM_EPSI,XIBM_IEPS
  USE MODD_IBM_LSF, ONLY: LIBM_LSF,CIBM_TYPE,NIBM_SMOOTH,XIBM_SMOOTH
  USE MODD_VAR_ll, ONLY: IP
  USE MODD_GRID_n, ONLY: XXHAT,XYHAT,XZHAT,XZZ
  USE MODD_PARAMETERS, ONLY: XUNDEF,JPHEXT,JPVEXT
  !
  ! interface
  USE MODI_IBM_PREP_LS
  USE MODI_IBM_SMOOTH_LS
  USE MODI_SECOND_MNH
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PPHI
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  REAL,DIMENSION(:,:,:,:), ALLOCATABLE :: ZPHI            ! temporary LSF
  INTEGER :: JJ,JI,JK,IIE,IIB,IJB,IJE,IKE,IKB,IIU,IJU,IKU ! loop index
  REAL    :: ZTIME1,ZTIME2                                ! computation times
  !
  !------------------------------------------------------------------------------
  !
  !**** 0. ALLOCATIONS
  !     --------------
  !
  ALLOCATE(ZPHI(SIZE(PPHI,1),SIZE(PPHI,2),SIZE(PPHI,3),7))
  ZPHI = -XIBM_IEPS
  !
  CALL GET_DIM_EXT_ll('B',IIU,IJU)
  IKU=SIZE(XZZ,3)
  IIB=1+JPHEXT
  IIE=IIU-JPHEXT
  IJB=1+JPHEXT
  IJE=IJU-JPHEXT
  IKB=1+JPVEXT
  IKE=IKU-JPVEXT
  ZTIME1=0.
  ZTIME2=0.
  XT_IBM_PREP = 0.
  !
  !------------------------------------------------------------------------------
  !       
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  IF (IP==1) THEN
     !
     IF (CIBM_TYPE == 'GENE'.OR.CIBM_TYPE == 'IDEA'.OR.CIBM_TYPE =='REAL'   &
          .OR.CIBM_TYPE == 'GEID'.OR.CIBM_TYPE == 'IDRE') THEN
        !
        WRITE(*,*) '****************************'
        WRITE(*,*) '**** BEGIN LSF BUILDING ****'
        WRITE(*,*) '****************************'
        !
     ELSE
        !
        WRITE(*,*) '*****************************'
        WRITE(*,*) '******** LIBM = TRUE ********'
        WRITE(*,*) '*** CIBM_TYPE IS REQUIRED ***'
        WRITE(*,*) '******** = GENE/IDEA ********'
        WRITE(*,*) '**** (stopped execution) ****'
        WRITE(*,*) '*****************************'
        !
        CALL PRINT_MSG(NVERB_FATAL,'GEN','IBM_INIT_LS','with IBM, CIBM_TYPE is REQUIRED')        
        !
     ENDIF
  ENDIF
  !
  !-------------------------------------------------------------------------------
  !
  !**** 2. EXECUTIONS
  !     -------------
  CALL SECOND_MNH(ZTIME1)
  !  
  ! LSF initialization
  CALL IBM_PREP_LS(LIBM_LSF,CIBM_TYPE,ZPHI)
  !
  ! LSF smoothing   
  IF (XIBM_SMOOTH/=0.) THEN
     IF (XIBM_SMOOTH==XUNDEF) THEN
        XIBM_SMOOTH=XIBM_EPSI
        NIBM_SMOOTH=1
     ENDIF
     IF (IP==1) WRITE(*,*)'*IBM* Smoothing is applied on LSF'
     CALL IBM_SMOOTH_LS(NIBM_SMOOTH,XIBM_SMOOTH,ZPHI)
  ELSE
     IF (IP==1) WRITE(*,*)'*IBM* No smoothing is applied on LSF'
  ENDIF
  !
  ! LSF storage
  PPHI(:,:,:,1:4)=ZPHI(:,:,:,1:4)
  !
  !-------------------------------------------------------------------------------
  !
  !**** X. DEALLOCATIONS
  !     ----------------
  ! 
  DEALLOCATE(ZPHI)
  CALL SECOND_MNH(ZTIME2)
  XT_IBM_PREP=ZTIME2-ZTIME1
  !
  IF (IP==1) THEN  
     WRITE(*,*) '*IBM* Time to build LSF (s):', XT_IBM_PREP
     WRITE(*,*) '**************************'
     WRITE(*,*) '**** END LSF BUILDING ****'
     WRITE(*,*) '**************************'
  ENDIF
  !
  RETURN
  !
END SUBROUTINE IBM_INIT_LS
