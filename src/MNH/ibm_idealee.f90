!MNH_LIC Copyright 2019-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!    #######################
MODULE MODI_IBM_IDEALEE  
  !    ####################### 
  !
  INTERFACE
     !
     SUBROUTINE IBM_IDEALEE(KNUMB_OBS,PIBM_XYZ,PPHI)
       !
       INTEGER                  ,INTENT(IN)    :: KNUMB_OBS
       REAL, DIMENSION(:,:)     ,INTENT(IN)    :: PIBM_XYZ
       REAL, DIMENSION(:,:,:,:) ,INTENT(INOUT) :: PPHI
       !
     END SUBROUTINE IBM_IDEALEE
     !
  END INTERFACE
  !
END MODULE MODI_IBM_IDEALEE
!
!     ###############################################
SUBROUTINE IBM_IDEALEE(KNUMB_OBS,PIBM_XYZ,PPHI)
  !     ###############################################
  !
  !
  !****  IBM_IDEALEE computes LS function for ellipsoidal objects       
  !                
  !    PURPOSE
  !    -------
  !****  The purpose of this routine is to estimate the 
  !      levetset function for many ellipsoidal objects.

  !    METHOD
  !    ------
  !****  Use of a analytic solution and approximation in truncated cell
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
  USE MODE_ll
  USE MODE_IO
  !
  ! declaration
  USE MODD_IBM_PARAM_n
  USE MODD_DIM_n, ONLY: NIMAX,NJMAX,NKMAX
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
  REAL, DIMENSION(:,:)     ,INTENT(IN)    :: PIBM_XYZ  ! interface location                    
  REAL, DIMENSION(:,:,:,:) ,INTENT(INOUT) :: PPHI      ! LS function
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER :: JI,JJ,JK,JN,JM                                               ! loop index
  INTEGER :: JI_MIN,JI_MAX,JJ_MIN,JJ_MAX,JK_MIN,JK_MAX,IIU,IJU,IKU        ! loop boundaries
  REAL, ALLOCATABLE                   :: ZPOSI_AXEX,ZPOSI_AXEY,ZPOSI_AXEZ ! saving positions/distances
  REAL, ALLOCATABLE                   :: ZDIST_AXEX,ZDIST_AXEY,ZDIST_AXEZ
  REAL, ALLOCATABLE                   :: ZCOEFA,ZCOEFB,ZDIST_REF0         ! solid volume and cell volume
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZXHATM,ZYHATM,ZZHATM             ! mesh location (mass nodes)
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZXHATC,ZYHATC,ZZHATC             ! mesh location (cell nodes)
  CHARACTER(LEN=1)                    :: YPOS
  !
  !-------------------------------------------------------------------------------  
  !
  !       0.3 allocation
  !
  IIU = SIZE(PPHI,1)
  IJU = SIZE(PPHI,2)
  IKU = SIZE(PPHI,3)
  !
  ALLOCATE(ZPOSI_AXEX,ZPOSI_AXEY,ZPOSI_AXEZ,        &                 
       ZDIST_AXEX,ZDIST_AXEY,ZDIST_AXEZ,        &
       ZCOEFA,ZCOEFB,ZDIST_REF0)
  !
  ALLOCATE(ZXHATC(IIU+1,IJU+1,IKU+1))
  ALLOCATE(ZXHATM(IIU  ,IJU  ,IKU  ))
  ALLOCATE(ZYHATC(IIU+1,IJU+1,IKU+1))
  ALLOCATE(ZYHATM(IIU  ,IJU  ,IKU  ))
  ALLOCATE(ZZHATC(IIU+1,IJU+1,IKU+1))
  ALLOCATE(ZZHATM(IIU  ,IJU  ,IKU  ))
  !
  JI_MIN = 1 + JPHEXT
  JI_MAX = IIU - JPHEXT
  JJ_MIN = 1 + JPHEXT
  JJ_MAX = IJU - JPHEXT
  JK_MIN = 1 + JPVEXT
  JK_MAX = IKU - JPVEXT
  !
  !-------------------------------------------------------------------------------
  !
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  ZDIST_AXEX = PIBM_XYZ(KNUMB_OBS,2) 
  ZDIST_AXEY = PIBM_XYZ(KNUMB_OBS,4) 
  ZDIST_AXEZ = PIBM_XYZ(KNUMB_OBS,6)
  ZPOSI_AXEX = PIBM_XYZ(KNUMB_OBS,1) 
  ZPOSI_AXEY = PIBM_XYZ(KNUMB_OBS,3) 
  ZPOSI_AXEZ = PIBM_XYZ(KNUMB_OBS,5)
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
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     IF (JM==3) THEN
        YPOS = 'V'
        JI_MAX = IIU - JPHEXT + 1
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     IF (JM==4) THEN
        YPOS = 'W'
        JI_MAX = IIU - JPHEXT + 1
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     IF (JM==5) THEN
        YPOS = 'A'
        JI_MAX = IIU - JPHEXT + 1
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     IF (JM==6) THEN
        YPOS = 'B'
        JI_MAX = IIU - JPHEXT + 1
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     IF (JM==7) THEN
        YPOS = 'C'
        JI_MAX = IIU - JPHEXT + 1
        JJ_MAX = IJU - JPHEXT + 1
        JK_MAX = IKU - JPVEXT + 1
     ENDIF
     !
     CALL IBM_INTERPOS(ZXHATM,ZYHATM,ZZHATM,YPOS)
     CALL IBM_INTERPOS2(ZXHATM,ZYHATM,ZZHATM,ZXHATC,ZYHATC,ZZHATC)
     !
     DO JK = JK_MIN,JK_MAX
        DO JJ = JJ_MIN,JJ_MAX
           DO JI = JI_MIN,JI_MAX
              !
              ! LS function
              !
              IF ((ZDIST_AXEX.gt.XIBM_EPSI).and.&
                   (ZDIST_AXEY.gt.XIBM_EPSI).and.&
                   (ZDIST_AXEZ.gt.XIBM_EPSI)) THEN
                 !
                 ZCOEFA = max(ZDIST_AXEX,ZDIST_AXEY,ZDIST_AXEZ)      
                 ZCOEFB = sqrt(((ZXHATM(JI,JJ,JK)-ZPOSI_AXEX)*ZCOEFA/ZDIST_AXEX)**2.+&
                      ((ZYHATM(JI,JJ,JK)-ZPOSI_AXEY)*ZCOEFA/ZDIST_AXEY)**2.+& 
                      ((ZZHATM(JI,JJ,JK)-ZPOSI_AXEZ)*ZCOEFA/ZDIST_AXEZ)**2.)
                 !
              ENDIF
              !
              IF ((ZDIST_AXEX.lt.XIBM_EPSI).and.&
                   (ZDIST_AXEY.gt.XIBM_EPSI).and.&
                   (ZDIST_AXEZ.gt.XIBM_EPSI)) THEN
                 !
                 ZCOEFA = max(ZDIST_AXEY,ZDIST_AXEZ)
                 ZCOEFB = sqrt(((ZYHATM(JI,JJ,JK)-ZPOSI_AXEY)*ZCOEFA/ZDIST_AXEY)**2.+&
                      ((ZZHATM(JI,JJ,JK)-ZPOSI_AXEZ)*ZCOEFA/ZDIST_AXEZ)**2.)
                 !
              ENDIF
              !
              IF ((ZDIST_AXEX.gt.XIBM_EPSI).and.&
                   (ZDIST_AXEY.lt.XIBM_EPSI).and.&
                   (ZDIST_AXEZ.gt.XIBM_EPSI)) THEN
                 !
                 ZCOEFA = max(ZDIST_AXEX,ZDIST_AXEZ)
                 ZCOEFB =sqrt(((ZXHATM(JI,JJ,JK)-ZPOSI_AXEX)*ZCOEFA/ZDIST_AXEX)**2.+&
                      ((ZZHATM(JI,JJ,JK)-ZPOSI_AXEZ)*ZCOEFA/ZDIST_AXEZ)**2.)
                 !
              ENDIF
              !
              IF ((ZDIST_AXEX.gt.XIBM_EPSI).and.&
                   (ZDIST_AXEY.gt.XIBM_EPSI).and.&
                   (ZDIST_AXEZ.lt.XIBM_EPSI)) THEN
                 !
                 ZCOEFA = max(ZDIST_AXEX,ZDIST_AXEY)
                 ZCOEFB = sqrt(((ZXHATM(JI,JJ,JK)-ZPOSI_AXEX)*ZCOEFA/ZDIST_AXEX)**2.+&
                      ((ZYHATM(JI,JJ,JK)-ZPOSI_AXEY)*ZCOEFA/ZDIST_AXEY)**2.)
                 !
              ENDIF
              !
              ZDIST_REF0 = ZCOEFA-ZCOEFB
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
  DEALLOCATE(ZXHATM,ZYHATM,ZZHATM)
  DEALLOCATE(ZXHATC,ZYHATC,ZZHATC)
  DEALLOCATE(ZPOSI_AXEX,ZPOSI_AXEY,ZPOSI_AXEZ,ZDIST_AXEX,ZDIST_AXEY,ZDIST_AXEZ,ZCOEFA,ZCOEFB,ZDIST_REF0)
  !
  RETURN
  !
  !------------------------------------------------------------------------------
END SUBROUTINE IBM_IDEALEE
