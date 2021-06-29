!MNH_LIC Copyright 2019-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!    #######################
MODULE MODI_IBM_IDEALRP  
  !    ####################### 
  !
  INTERFACE
     !
     SUBROUTINE IBM_IDEALRP(KNUMB_OBS,PIBM_XYZ,PPHI)
       ! 
       INTEGER                  ,INTENT(IN)    :: KNUMB_OBS                                       
       REAL, DIMENSION(:,:)     ,INTENT(IN)    :: PIBM_XYZ                                                                 
       REAL, DIMENSION(:,:,:,:) ,INTENT(INOUT) :: PPHI                                     
       !
     END SUBROUTINE IBM_IDEALRP
     !
  END INTERFACE
  !
END MODULE MODI_IBM_IDEALRP
!
!     ###############################################
SUBROUTINE IBM_IDEALRP(KNUMB_OBS,PIBM_XYZ,PPHI)
  !     ###############################################
  !
  !
  !****  IBM_IDEALRP compute LS function for parallelepipedic objects       
  !                
  !    PURPOSE
  !    -------
  !****  The purpose of this routine is to estimate the  
  !      levetset function for many parallelepipedic objects.
  !      I_NUMB_ITER is a parameter controlling the fine resolution of
  !      each surface
  !
  !    METHOD
  !    ------
  !****  Use of a smooth Heaviside function and a characteristic numerical interface thickness
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
  !
  ! module
  USE MODE_POS
  USE MODE_ll
  USE MODE_IO
  USE MODE_GATHER_ll
  !
  ! declaration
  USE MODD_IBM_PARAM_n
  USE MODD_DIM_n, ONLY: NIMAX,NJMAX,NKMAX
  USE MODD_GRID_n, ONLY: XXHAT,XYHAT,XZHAT,XZZ
  USE MODD_PARAMETERS, ONLY: JPVEXT,JPHEXT
  !
  ! interface
  USE MODI_SHUMAN
  USE MODI_IBM_INTERPOS
  USE MODI_IBM_INTERPOS2
  !
  IMPLICIT NONE
  !
  !       0.1  declarations of arguments
  !    
  INTEGER                  ,INTENT(IN)    :: KNUMB_OBS ! obstacle number
  REAL, DIMENSION(:,:)     ,INTENT(IN)    :: PIBM_XYZ  ! array for interface initialization
  REAL, DIMENSION(:,:,:,:) ,INTENT(INOUT) :: PPHI      ! LS functions
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER                             :: JI,JJ,JK,JN,JM,IIU,IJU,IKU,IIU_ll,IJU_ll  ! loop index
  INTEGER                             :: JI_MIN,JI_MAX,JJ_MIN,JJ_MAX,JK_MIN,JK_MAX
  REAL                                :: ZDELTX,ZDELTY,ZDELTZ
  REAL, ALLOCATABLE                   :: ZTEST_XMIN,ZTEST_XMAX                     ! saving positions
  REAL, ALLOCATABLE                   :: ZTEST_YMIN,ZTEST_YMAX
  REAL, ALLOCATABLE                   :: ZTEST_ZMIN,ZTEST_ZMAX
  REAL, ALLOCATABLE                   :: ZPOSI_XYZ0,ZPOSI_XYZ1,ZPOSI_XYZ2 
  REAL, ALLOCATABLE                   :: ZDIST_SUR0,ZDIST_SUR1,ZDIST_SUR2          ! saving distances
  REAL, ALLOCATABLE                   :: ZDIST_SUR3,ZDIST_SUR4,ZDIST_SUR5
  REAL, ALLOCATABLE                   :: ZDIST_SUR6,ZDIST_REF0                            
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZXHATM,ZYHATM,ZZHATM                      ! mesh location (mass nodes)
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZXHATC,ZYHATC,ZZHATC                      ! mesh location (cell nodes)
  REAL, DIMENSION(:)    , ALLOCATABLE :: ZXHAT_ll,ZYHAT_ll
  CHARACTER(LEN=1)                    :: YPOS
  INTEGER                             :: NRESP
  !
  !------------------------------------------------------------------------------
  !   
  !       0.3 allocation
  !
  IIU = SIZE(PPHI,1)
  IJU = SIZE(PPHI,2)
  IKU = SIZE(PPHI,3)
  !
  JI_MIN = 1 + JPHEXT
  JI_MAX = IIU - JPHEXT
  JJ_MIN = 1 + JPHEXT
  JJ_MAX = IJU - JPHEXT
  JK_MIN = 1 + JPVEXT
  JK_MAX = IKU - JPVEXT
  !
  ALLOCATE(ZXHATM(IIU  ,IJU  ,IKU  ))
  ALLOCATE(ZYHATM(IIU  ,IJU  ,IKU  ))
  ALLOCATE(ZZHATM(IIU  ,IJU  ,IKU  ))
  ALLOCATE(ZXHATC(IIU+1,IJU+1,IKU+1))
  ALLOCATE(ZYHATC(IIU+1,IJU+1,IKU+1))
  ALLOCATE(ZZHATC(IIU+1,IJU+1,IKU+1))
  ALLOCATE(ZTEST_XMIN,ZTEST_XMAX,ZTEST_YMIN,ZTEST_YMAX,ZTEST_ZMIN,ZTEST_ZMAX)
  ALLOCATE(ZPOSI_XYZ0,ZPOSI_XYZ1,ZPOSI_XYZ2)
  ALLOCATE(ZDIST_SUR0,ZDIST_SUR1,ZDIST_SUR2,ZDIST_SUR3,ZDIST_SUR4,ZDIST_SUR5,ZDIST_SUR6,ZDIST_REF0)
  !
  !-------------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  ! 
  CALL GET_GLOBALDIMS_ll(IIU_ll,IJU_ll)
  ALLOCATE(ZXHAT_ll(IIU_ll+ 2 * JPHEXT))
  ALLOCATE(ZYHAT_ll(IJU_ll+ 2 * JPHEXT))
  CALL GATHERALL_FIELD_ll('XX',XXHAT,ZXHAT_ll,NRESP)
  CALL GATHERALL_FIELD_ll('YY',XYHAT,ZYHAT_ll,NRESP)
  ZDELTX = abs((PIBM_XYZ(KNUMB_OBS,1)-PIBM_XYZ(KNUMB_OBS,2))/ & 
       ((ZXHAT_ll(IIU_ll+2)-ZXHAT_ll(2))/(IIU_ll*1.)))
  ZDELTY = abs((PIBM_XYZ(KNUMB_OBS,3)-PIBM_XYZ(KNUMB_OBS,4))/ & 
       ((ZYHAT_ll(IJU_ll+2)-ZYHAT_ll(2))/(IJU_ll*1.)))  
  ZDELTZ = abs((PIBM_XYZ(KNUMB_OBS,5)-PIBM_XYZ(KNUMB_OBS,6))/ &
       ((XZHAT(IKU)-XZHAT(2))/(IKU*1.-2.)))
  !      
  !-------------------------------------------------------------------------------
  !
  !**** 2. EXECUTIONS
  !     -------------
  !
  DO JM=1,7
     !
     IF (JM==1) THEN
        YPOS = 'P'
        JI_MAX = IIU - JPHEXT
        JJ_MAX = IJU - JPHEXT
        JK_MAX = IKU - JPVEXT
     ENDIF
     IF (JM==2) THEN
        YPOS = 'U'
        JI_MAX = IIU - JPHEXT + 1
        JJ_MAX = IJU - JPHEXT 
        JK_MAX = IKU - JPVEXT 
     ENDIF
     IF (JM==3) THEN
        YPOS = 'V'
        JI_MAX = IIU - JPHEXT 
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT 
     ENDIF
     IF (JM==4) THEN
        YPOS = 'W'
        JI_MAX = IIU - JPHEXT 
        JJ_MAX = IJU - JPHEXT 
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     IF (JM==5) THEN
        YPOS = 'A'
        JI_MAX = IIU - JPHEXT + 1
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT 
     ENDIF
     IF (JM==6) THEN
        YPOS = 'B'
        JI_MAX = IIU - JPHEXT + 1
        JJ_MAX = IJU - JPHEXT 
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     IF (JM==7) THEN
        YPOS = 'C'
        JI_MAX = IIU - JPHEXT 
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     CALL IBM_INTERPOS(ZXHATM,ZYHATM,ZZHATM,YPOS)
     CALL IBM_INTERPOS2(ZXHATM,ZYHATM,ZZHATM,ZXHATC,ZYHATC,ZZHATC)
     DO JK = JK_MIN,JK_MAX
        DO JJ = JJ_MIN,JJ_MAX
           DO JI = JI_MIN,JI_MAX
              !
              ! LS function
              ZTEST_XMIN = PIBM_XYZ(KNUMB_OBS,1)
              ZTEST_XMAX = PIBM_XYZ(KNUMB_OBS,2)
              ZTEST_YMIN = PIBM_XYZ(KNUMB_OBS,3)
              ZTEST_YMAX = PIBM_XYZ(KNUMB_OBS,4)
              ZTEST_ZMIN = PIBM_XYZ(KNUMB_OBS,5)
              ZTEST_ZMAX = PIBM_XYZ(KNUMB_OBS,6)
              !
              ZPOSI_XYZ0 = ZTEST_XMIN
              ZDIST_SUR1 = XIBM_IEPS
              ZPOSI_XYZ1 = max(ZTEST_YMIN,ZYHATM(JI,JJ,JK))
              ZPOSI_XYZ1 = min(ZTEST_YMAX,ZPOSI_XYZ1)
              ZPOSI_XYZ2 = max(ZTEST_ZMIN,ZZHATM(JI,JJ,JK))
              ZPOSI_XYZ2 = min(ZTEST_ZMAX,ZPOSI_XYZ2)
              ZDIST_SUR0 = ((ZPOSI_XYZ0-ZXHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ1-ZYHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ2-ZZHATM(JI,JJ,JK))**2.)**0.5
              ZDIST_SUR1 = min(ZDIST_SUR0,ZDIST_SUR1)
              !
              ZPOSI_XYZ0 = ZTEST_XMAX
              ZDIST_SUR2 = XIBM_IEPS
              ZPOSI_XYZ1 = max(ZTEST_YMIN,ZYHATM(JI,JJ,JK))
              ZPOSI_XYZ1 = min(ZTEST_YMAX,ZPOSI_XYZ1)
              ZPOSI_XYZ2 = max(ZTEST_ZMIN,ZZHATM(JI,JJ,JK))
              ZPOSI_XYZ2 = min(ZTEST_ZMAX,ZPOSI_XYZ2)
              ZDIST_SUR0 = ((ZPOSI_XYZ0-ZXHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ1-ZYHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ2-ZZHATM(JI,JJ,JK))**2.)**0.5
              ZDIST_SUR2 = min(ZDIST_SUR0,ZDIST_SUR2)
              !
              ZPOSI_XYZ0 = ZTEST_YMIN
              ZDIST_SUR3 = XIBM_IEPS
              ZPOSI_XYZ1 = max(ZTEST_XMIN,ZXHATM(JI,JJ,JK))
              ZPOSI_XYZ1 = min(ZTEST_XMAX,ZPOSI_XYZ1)
              ZPOSI_XYZ2 = max(ZTEST_ZMIN,ZZHATM(JI,JJ,JK))
              ZPOSI_XYZ2 = min(ZTEST_ZMAX,ZPOSI_XYZ2)
              ZDIST_SUR0 = ((ZPOSI_XYZ1-ZXHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ0-ZYHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ2-ZZHATM(JI,JJ,JK))**2.)**0.5
              ZDIST_SUR3 = min(ZDIST_SUR0,ZDIST_SUR3)
              !
              ZPOSI_XYZ0 = ZTEST_YMAX
              ZDIST_SUR4 = XIBM_IEPS
              ZPOSI_XYZ1 = max(ZTEST_XMIN,ZXHATM(JI,JJ,JK))
              ZPOSI_XYZ1 = min(ZTEST_XMAX,ZPOSI_XYZ1)
              ZPOSI_XYZ2 = max(ZTEST_ZMIN,ZZHATM(JI,JJ,JK))
              ZPOSI_XYZ2 = min(ZTEST_ZMAX,ZPOSI_XYZ2)
              ZDIST_SUR0 = ((ZPOSI_XYZ1-ZXHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ0-ZYHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ2-ZZHATM(JI,JJ,JK))**2.)**0.5
              ZDIST_SUR4 = min(ZDIST_SUR0,ZDIST_SUR4)
              !
              ZPOSI_XYZ0 = ZTEST_ZMIN
              ZDIST_SUR5 = XIBM_IEPS
              ZPOSI_XYZ1 = max(ZTEST_XMIN,ZXHATM(JI,JJ,JK))
              ZPOSI_XYZ1 = min(ZTEST_XMAX,ZPOSI_XYZ1)
              ZPOSI_XYZ2 = max(ZTEST_YMIN,ZYHATM(JI,JJ,JK))
              ZPOSI_XYZ2 = min(ZTEST_YMAX,ZPOSI_XYZ2)
              ZDIST_SUR0 = ((ZPOSI_XYZ1-ZXHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ2-ZYHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ0-ZZHATM(JI,JJ,JK))**2.)**0.5
              ZDIST_SUR5 = min(ZDIST_SUR0,ZDIST_SUR5)
              !
              ZPOSI_XYZ0 = ZTEST_ZMAX
              ZDIST_SUR6 = XIBM_IEPS
              ZPOSI_XYZ1 = max(ZTEST_XMIN,ZXHATM(JI,JJ,JK))
              ZPOSI_XYZ1 = min(ZTEST_XMAX,ZPOSI_XYZ1)
              ZPOSI_XYZ2 = max(ZTEST_YMIN,ZYHATM(JI,JJ,JK))
              ZPOSI_XYZ2 = min(ZTEST_YMAX,ZPOSI_XYZ2)
              ZDIST_SUR0 = ((ZPOSI_XYZ1-ZXHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ2-ZYHATM(JI,JJ,JK))**2. + &
                   (ZPOSI_XYZ0-ZZHATM(JI,JJ,JK))**2.)**0.5
              ZDIST_SUR6 = min(ZDIST_SUR0,ZDIST_SUR6)
              !
              IF ((ZXHATM(JI,JJ,JK) .gt. ZTEST_XMIN.and.ZXHATM(JI,JJ,JK) .lt. ZTEST_XMAX).and. &
                   (ZYHATM(JI,JJ,JK) .gt. ZTEST_YMIN.and.ZYHATM(JI,JJ,JK) .lt. ZTEST_YMAX).and. &
                   (ZZHATM(JI,JJ,JK) .gt. ZTEST_ZMIN.and.ZZHATM(JI,JJ,JK) .lt. ZTEST_ZMAX)) then
                 ZDIST_REF0 = +min(ZDIST_SUR1,ZDIST_SUR2,ZDIST_SUR3,ZDIST_SUR4,ZDIST_SUR5,ZDIST_SUR6)
              ELSE
                 ZDIST_REF0 = -min(ZDIST_SUR1,ZDIST_SUR2,ZDIST_SUR3,ZDIST_SUR4,ZDIST_SUR5,ZDIST_SUR6)
              ENDIF
              !
              IF (PPHI(JI,JJ,JK,JM) .lt. ZDIST_REF0) PPHI(JI,JJ,JK,JM) = ZDIST_REF0
              !
           ENDDO
        ENDDO
     ENDDO
  ENDDO
  !
  !-------------------------------------------------------------------------------
  !
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  !
  DEALLOCATE(ZXHATC,ZYHATC,ZZHATC)
  DEALLOCATE(ZXHAT_ll,ZYHAT_ll)
  DEALLOCATE(ZTEST_XMIN,ZTEST_XMAX,ZTEST_YMIN,ZTEST_YMAX,ZTEST_ZMIN,ZTEST_ZMAX)
  DEALLOCATE(ZPOSI_XYZ0,ZPOSI_XYZ1,ZPOSI_XYZ2)
  DEALLOCATE(ZDIST_SUR0,ZDIST_SUR1,ZDIST_SUR2,ZDIST_SUR3,ZDIST_SUR4,ZDIST_SUR5,ZDIST_SUR6,ZDIST_REF0)
  !
  RETURN
  !
END SUBROUTINE IBM_IDEALRP
