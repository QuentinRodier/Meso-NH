!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE NUDGING_ISBA_SOIL(IO, K, NK, NP, NPE, DM, NDM, KYEAR, KMONTH, KDAY, PTSTEP)
!     #####################################################################
!
!!****  *NUDGING_ISBA_SOIL*  
!!
!!    PURPOSE
!!    -------
!!   
!!    Compute the nudging of soil's variables
!!
!!    METHOD
!!    -------
!
!     Step 1
!     Compute the nudging value
!     
!     Step 2
!     Apply the nudging on the soil's variable
!!    ------
!
!!      
!!    AUTHOR
!!    ------
!!    J. Colin           * Meteo-France *
!!
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 08/16
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_K_t, ISBA_NK_t, ISBA_NP_t,  &
                                  ISBA_NPE_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t, DIAG_MISC_ISBA_NP_t
!
USE MODD_SURF_PAR,     ONLY : XUNDEF, NUNDEF
USE MODD_ISBA_PAR,     ONLY : XWGMIN
USE MODD_CSTS,         ONLY : XRHOLW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t),      INTENT(INOUT) :: IO
TYPE(ISBA_K_t),            INTENT(INOUT) :: K
TYPE(ISBA_NK_t),           INTENT(INOUT) :: NK
TYPE(ISBA_NP_t),           INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),          INTENT(INOUT) :: NPE
TYPE(DIAG_MISC_ISBA_t),    INTENT(INOUT) :: DM
TYPE(DIAG_MISC_ISBA_NP_t), INTENT(INOUT) :: NDM
!
INTEGER,                INTENT(IN)    :: KYEAR   ! Current year
INTEGER,                INTENT(IN)    :: KMONTH  ! Current month
INTEGER,                INTENT(IN)    :: KDAY    ! Current day
REAL,                   INTENT(IN)    :: PTSTEP  ! Time step
!                                                      
!*      0.2    declarations of local variables 
!
TYPE(ISBA_K_t),         POINTER :: KK
TYPE(ISBA_P_t),         POINTER :: PK
TYPE(ISBA_PE_t),        POINTER :: PEK
TYPE(DIAG_MISC_ISBA_t), POINTER :: DMK
!
REAL, DIMENSION(SIZE(K%XWSAT,1),IO%NGROUND_LAYER,IO%NPATCH) :: ZNUDG_WGTOT ! Nudging value
REAL, DIMENSION(SIZE(K%XWSAT,1),IO%NGROUND_LAYER,IO%NPATCH) :: ZWG0
REAL, DIMENSION(SIZE(K%XWSAT,1),IO%NGROUND_LAYER,IO%NPATCH) :: ZWGI0
!
REAL, DIMENSION(SIZE(K%XWSAT,1)) :: ZMASK
!
INTEGER :: JI, JL, JP, IMASK          ! Loop indexes
INTEGER :: INI, INL, INP, IDEPTH      ! Loop bounds
REAL    :: ZNUDG_INC, ZDT
!
REAL    :: ZDAT,ZNDAT
INTEGER :: IMTH1,IMTH2,IMTH3
INTEGER :: INDAYS                ! number of days in KMONTH
REAL    :: ZEXCESSF, ZWORK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('NUDGING_ISBA_SOIL',0,ZHOOK_HANDLE)
!
!        0.     Initialisations 
!               ---------------
!
ZDT = PTSTEP/IO%XTRELAX_WG
!
INI = SIZE(K%XWSAT,1)
INL = IO%NGROUND_LAYER
INP = IO%NPATCH
!
ZNUDG_WGTOT(:,:,:)=XUNDEF
!
DO JP = 1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)  
   DO JL = 1,INL
      DO JI = 1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZWG0 (IMASK,JL,JP) = PEK%XWG (JI,JL)
         ZWGI0(IMASK,JL,JP) = PEK%XWGI(JI,JL)
      ENDDO
   ENDDO
ENDDO
!
IF(IO%LNUDG_WG_MASK)THEN
  ZMASK(:)=K%XNUDG_MASK(:)
ELSE
  ZMASK(:)=1.0
ENDIF
!
!-----------------------------------------------------------------------------
!
!        1. Compute the interpoled nudging value 
!        -----------------------------
!
IF (IO%CNUDG_WG=='DAY') THEN   
   ! 
   !  Daily values 
   ! 
   DO JP=1,INP
      KK => NK%AL(JP)
      PK => NP%AL(JP)
      DO JL=1,INL
         DO JI = 1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZNUDG_WGTOT(IMASK,JL,JP) = MIN(MAX(XWGMIN,PK%XNUDG_WGTOT(JI,JL,KDAY)),KK%XWSAT(JI,JL))
         END DO
     ENDDO
   ENDDO
   ! 
ELSE 
   !
   !  Interpolation with three months number of days in the current month
   !
   SELECT CASE (KMONTH)
     CASE(4,6,9,11)
         INDAYS=30
     CASE(1,3,5,7:8,10,12)
         INDAYS=31
     CASE(2)
         IF(((MOD(KYEAR,4)==0).AND.(MOD(KYEAR,100)/=0)).OR.(MOD(KYEAR,400)==0))THEN
           INDAYS=29
         ELSE
           INDAYS=28
         ENDIF
   END SELECT
   !
   !  Interpolation using previous, current and following month
   !
   ZDAT = REAL(KDAY)
   ZNDAT= REAL(INDAYS)
   !
   IMTH1=1
   IMTH2=2
   IMTH3=3
   !
   DO JP=1,INP
      KK => NK%AL(JP)
      PK => NP%AL(JP)
      DO JL=1,INL
         CALL INTERPOL(PK%NSIZE_P,PK%NR_P(:),PK%XNUDG_WGTOT(:,JL,:),ZNUDG_WGTOT(:,JL,JP))
      ENDDO
   END DO
   !
ENDIF
!
!--------------------------------------------------------------------------------
!
!      2. Compute the water nudging  
!      ----------------------------
!
IF (IO%CISBA/='DIF') THEN 
   !
   !  2-L Force restore case
   !  Nudge ice and liquid water in the first two layers 
   !
   IDEPTH=2
   CALL NUDGING_WG_LAYER(IDEPTH)
   !
   !  3-L Force restore case
   !  Nudge liquid water in the third layer 
   !
   IF (IO%CISBA=='3-L') THEN 
      DO JP=1,INP
         KK => NK%AL(JP)
         PK => NP%AL(JP)
         PEK => NPE%AL(JP)
         IF (PK%NSIZE_P>0 )THEN
            DO JI=1,PK%NSIZE_P
               IMASK = PK%NR_P(JI)
               IF (PEK%XWG(JI,3)/=XUNDEF) THEN
                  ZNUDG_INC     = MAX(0.,ZNUDG_WGTOT(IMASK,3,JP)) - PEK%XWG(JI,3)
                  PEK%XWG(JI,3) = PEK%XWG(JI,3) + ZNUDG_INC * ZDT * ZMASK(IMASK) * IO%XNUDG_Z_WG(3)
                  PEK%XWG(JI,3) = MIN(MAX(XWGMIN,PEK%XWG(JI,3)),KK%XWSAT(JI,3))! Check limits
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDIF
   !
ELSE
   !
   ! Isba-dif case
   !
   CALL NUDGING_WG_LAYER(INL)
   !
   DO JP=1,INP       
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      IF (PK%NSIZE_P>0 )THEN
         DO JL=1,INL 
            DO JI=1,PK%NSIZE_P
               IDEPTH = PK%NWG_LAYER(JI)
               IF ((JL>IDEPTH.OR.IDEPTH==NUNDEF).AND.(PEK%XWG(JI,JL)/=XUNDEF)) THEN
                  PEK%XWGI(JI,JL) = XUNDEF
               ENDIF
               IF ((JL>IDEPTH.OR.IDEPTH==NUNDEF).AND.(PEK%XWGI(JI,JL)/=XUNDEF)) THEN
                  PEK%XWGI(JI,JL) = XUNDEF
               ENDIF
            ENDDO
         ENDDO
      ENDIF
   ENDDO
   !
ENDIF
!
!--------------------------------------------------------------------------------
!
!      3. Compute Nudging Increment (kg/m2)  
!      ------------------------------------
! 
DM%XNUDGINCSM (:  ) = 0.0
DM%XNUDGINCSML(:,:) = 0.0
!
DO JP = 1,INP
   DMK => NDM%AL(JP)   
   DMK%XNUDGINCSM (:  )=0.0
   DMK%XNUDGINCSML(:,:)=0.0
ENDDO
!
!
IF (IO%CISBA/='DIF') THEN 
   !
   ! Force restore case
   !   
   DO JP=1,IO%NPATCH
      !
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DMK => NDM%AL(JP)   
      !
      DO JI=1,PK%NSIZE_P
         !
         IMASK = PK%NR_P(JI)
         !
         IF(PEK%XWG(JI,1)/=XUNDEF)THEN
           DMK%XNUDGINCSML(JI,1)=(PEK%XWG(JI,1)-ZWG0(IMASK,1,JP)+PEK%XWGI(JI,1)-ZWGI0(IMASK,1,JP))*PK%XDG(JI,1)*XRHOLW
         ENDIF
         !
         IF(PEK%XWG(JI,2)/=XUNDEF)THEN
           DMK%XNUDGINCSML(JI,2)=(PEK%XWG(JI,2)-ZWG0(IMASK,2,JP)+PEK%XWGI(JI,2)-ZWGI0(IMASK,2,JP)) &
                                *(PK%XDG(JI,2)-PK%XDG(JI,1))*XRHOLW
         ENDIF
         !
         DMK%XNUDGINCSM(JI)=DMK%XNUDGINCSML(JI,1)+DMK%XNUDGINCSML(JI,2)
         !
      ENDDO
      !
   ENDDO
   !
   IF (IO%CISBA=='3-L') THEN 
      DO JP=1,INP
         PK => NP%AL(JP)
         PEK => NPE%AL(JP)
         DMK => NDM%AL(JP)   
         DO JI=1,INI
            IMASK = PK%NR_P(JI)
            IF(PEK%XWG(JI,3)/=XUNDEF) THEN
              DMK%XNUDGINCSML(JI,3)=(PEK%XWG(JI,3)-ZWG0(IMASK,3,JP))*(PK%XDG(JI,3)-PK%XDG(JI,2))*XRHOLW
            ENDIF
            DMK%XNUDGINCSM(JI)=DMK%XNUDGINCSM(JI)+DMK%XNUDGINCSML(JI,3)
         ENDDO
      ENDDO
   ENDIF
   !
ELSE
   !
   ! Isba-dif case
   !
   DO JP=1,IO%NPATCH
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DMK => NDM%AL(JP)   
      IF (PK%NSIZE_P>0 )THEN
         DO JL=1,INL 
            DO JI=1,PK%NSIZE_P
               IMASK  = PK%NR_P(JI)
               IDEPTH = PK%NWG_LAYER(JI)
               IF ((JL<=IDEPTH).AND.(IDEPTH/=NUNDEF).AND.(PEK%XWG(JI,JL)/=XUNDEF)) THEN
                  DMK%XNUDGINCSML(JI,JL)=(PEK%XWG(JI,JL)-ZWG0(IMASK,JL,JP)+PEK%XWGI(JI,JL)-ZWGI0(IMASK,JL,JP))*PK%XDZG(JI,JL)*XRHOLW
               ENDIF
               DMK%XNUDGINCSM(JI)=DMK%XNUDGINCSM(JI)+DMK%XNUDGINCSML(JI,JL)
            ENDDO
         ENDDO
      ENDIF
   ENDDO
   !
ENDIF
!
!--------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('NUDGING_ISBA_SOIL',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------------------------
!
SUBROUTINE INTERPOL(KNPI,KMASK,PNUDG_WG_MTH,PNUDG_WG)
!
USE MODI_INTERPOL_QUADRA
!
IMPLICIT NONE
!
INTEGER,               INTENT(IN)  :: KNPI
INTEGER, DIMENSION(:), INTENT(IN)  :: KMASK
REAL, DIMENSION(:,:),  INTENT(IN)  :: PNUDG_WG_MTH
!
REAL, DIMENSION(:),    INTENT(OUT) :: PNUDG_WG
!
REAL, DIMENSION(KNPI)              :: ZNUDG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('NUDGING_ISBA_SOIL:INTERPOL',0,ZHOOK_HANDLE)
!
ZNUDG(:) = 0.0
!
CALL INTERPOL_QUADRA(ZDAT,ZNDAT,PNUDG_WG_MTH(:,IMTH1),PNUDG_WG_MTH(:,IMTH2),PNUDG_WG_MTH(:,IMTH3),ZNUDG(:))
!
DO JI = 1,KNPI
   IMASK = KMASK(JI)
   PNUDG_WG(KMASK) = MIN(MAX(XWGMIN,ZNUDG(JI)),K%XWSAT(IMASK,JL))
ENDDO
!
IF (LHOOK) CALL DR_HOOK('NUDGING_ISBA_SOIL:INTERPOL',1,ZHOOK_HANDLE)
!
END SUBROUTINE INTERPOL
!
!--------------------------------------------------------------------------------
!
SUBROUTINE NUDGING_WG_LAYER(KNL)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KNL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('NUDGING_ISBA_SOIL:NUDGING_WG_LAYER',0,ZHOOK_HANDLE)
!
DO JP=1,INP
   !
   KK => NK%AL(JP)
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   !
   IF (PK%NSIZE_P>0)THEN
      !
      DO JL=1,KNL 
         !
         DO JI=1,PK%NSIZE_P
            !
            IMASK = PK%NR_P(JI)
            !
            IF (PEK%XWG(JI,JL)/=XUNDEF) THEN 
               !
               ! Liquid water content
               !
               ZWORK          = ZWG0(IMASK,JL,JP)/(ZWG0(IMASK,JL,JP)+ZWGI0(IMASK,JL,JP))
               ZNUDG_INC      = MAX(0.,ZWORK*ZNUDG_WGTOT(IMASK,JL,JP)) - PEK%XWG(JI,JL)
               PEK%XWG(JI,JL) = PEK%XWG(JI,JL) + ZNUDG_INC * ZDT * ZMASK(IMASK) * IO%XNUDG_Z_WG(JL)
               !
               ! Ice content
               !
               ZWORK           = ZWGI0(IMASK,JL,JP)/(ZWG0(IMASK,JL,JP)+ZWGI0(IMASK,JL,JP))
               ZNUDG_INC       = MAX(0.,ZWORK*ZNUDG_WGTOT(IMASK,JL,JP)) - PEK%XWGI(JI,JL)
               PEK%XWGI(JI,JL) = PEK%XWGI(JI,JL) + ZNUDG_INC * ZDT * ZMASK(IMASK) * IO%XNUDG_Z_WG(JL)
               !
               ! Check limits (see hydro_soil.F90)
               !
               ! Ice Minimum
               ! 1)  PEK%XWGI >= 0.0 (It should be the case)
               !
               PEK%XWGI(JI,JL) = MAX(0.0,PEK%XWGI(JI,JL))
               !
               ! 2)  For computational purposes, if PEK%XWGI<1.E-10.PSTEP, PEK%XWGI=0. 
               !
               IF (PEK%XWGI(JI,JL) < 1.0E-10 * PTSTEP) THEN
                   PEK%XWG (JI,JL) = PEK%XWG(JI,JL)+PEK%XWGI(JI,JL)
                   PEK%XWGI(JI,JL) = 0.0
               ENDIF
               !
               ! Ice Maximum
               ! PEK%XWGI<=(XWSAT-XWGMIN). If not, melt the ice and add that quantity to the liquid water content
               !
               ZEXCESSF        = MIN(0.0, KK%XWSAT(JI,JL) - XWGMIN - PEK%XWGI(JI,JL)) ! Ice to melt
               PEK%XWGI(JI,JL) = PEK%XWGI(JI,JL) - ZEXCESSF ! Minus the melted ice
               PEK%XWG (JI,JL) = PEK%XWG (JI,JL) + ZEXCESSF ! Plus the melted ice
               !
               ! Water Maximum
               ! PEK%XWG <= (XWSAT - PEK%XWGI). If not, equals the value
               !
               PEK%XWG(JI,JL) = MIN(PEK%XWG(JI,JL),KK%XWSAT(JI,JL)-PEK%XWGI(JI,JL))
               !
               ! Water Minimum PEK%XWG >= XWGMIN?
               PEK%XWG(JI,JL) = MAX(XWGMIN,PEK%XWG(JI,JL))
               !
            ENDIF
            !
         ENDDO
         !
      ENDDO
      !
   ENDIF
   !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('NUDGING_ISBA_SOIL:NUDGING_WG_LAYER',1,ZHOOK_HANDLE)
!
END SUBROUTINE NUDGING_WG_LAYER
!
!--------------------------------------------------------------------------------
!
END SUBROUTINE NUDGING_ISBA_SOIL
