!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CARBON_DYNAMIC(IO, KK, PK, PEK, KLUOUT, PTSTEP, PDBIO, PDIFBIO, PDCRYO, PDIFCRYO)
!   ###############################################################
!!****  *CARBON DYNAMIC*
!!
!!    PURPOSE
!!    -------
!!
!!    Soil Carbon vertical dynamic based on Morel et al. (2019). 
!!            
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      Morel et al. (2019) JAMES
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme       * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/06/20
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODD_CSTS,         ONLY : XSIYEA, XTT
USE MODD_SOILCARB_PAR, ONLY : XADVREF
!
USE MODE_HYDRO_DIF
!
USE YOMHOOK,  ONLY : LHOOK,   DR_HOOK
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*       0.1 input
!
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
!
INTEGER, INTENT(IN)                   :: KLUOUT
REAL, INTENT(IN)                      :: PTSTEP      ! time step

REAL, DIMENSION(:),   INTENT(IN)      :: PDBIO       ! bioturbation limit (m)
REAL, DIMENSION(:,:), INTENT(IN)      :: PDIFBIO     ! bioturbation diffusivity (m2/s)
REAL, DIMENSION(:),   INTENT(IN)      :: PDCRYO      ! cryoturbation limit (m)
REAL, DIMENSION(:,:), INTENT(IN)      :: PDIFCRYO    ! cryoturbation diffusivity (m2/s)
!
!-------------------------------------------------------------------------------
!
!*       0.3 local parameters
!
REAL, PARAMETER                       :: ZBUDLIM = 1.0E-10
!
REAL, PARAMETER                       :: ZWGHT = 0.5  ! time scheme weight for calculating flux.
!                                                       varies from 0 (explicit time scheme)
!                                                       to 1 (backward difference implicit)
!                                                       Default is 1/2 (Crank-Nicholson)
!
!-------------------------------------------------------------------------------
!
!*       0.4 local variable
!
INTEGER, DIMENSION(SIZE(PK%XDZG,1))                                          :: ILWORK
!
REAL, DIMENSION(SIZE(PK%XDZG,1))                                             :: ZBUD
!
REAL, DIMENSION(SIZE(PK%XDZG,1))                                             :: ZOLD_SOILCARB, ZOLD_LITTER
REAL, DIMENSION(SIZE(PK%XDZG,1))                                             :: ZNEW_SOILCARB, ZNEW_LITTER
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))                             :: ZA_BIO, ZB_BIO
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))                             :: ZA_CRYO, ZB_CRYO
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))                             :: ZAMTRX, ZBMTRX, ZCMTRX
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))                             :: ZALPHA, ZBETA, ZGAMMA
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2),SIZE(PEK%XSOILDIF_CARB,  3)) :: ZFRC_SOILCARB
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2),SIZE(PEK%XSOILDIF_LITTER,3)) :: ZFRC_LITTER
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2),SIZE(PEK%XSOILDIF_CARB,  3)) :: ZDENS_SOILCARB ! soil carbon density (gC m-3)
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2),SIZE(PEK%XSOILDIF_LITTER,3)) :: ZDENS_LITTER   ! soil litter density (gC m-3)
!
INTEGER                                        :: INI, INL, INC, INLIT, IDEPTH
INTEGER                                        :: JI, JL, JK
LOGICAL                                        :: LMASK
!
REAL                                           :: ZCSTADVC   !Advection constant (m/s)
REAL                                           :: ZSPIN, ZDT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!--------------------------------------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_DYNAMIC',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!*       1. Initialisations
!-------------------------------------------------------------------------------
!
!*       1.1 dimensions
!
INI=SIZE(PEK%XSOILDIF_CARB,1)
INL=SIZE(PEK%XSOILDIF_CARB,2)
INC=SIZE(PEK%XSOILDIF_CARB,3)
INLIT=SIZE(PEK%XSOILDIF_LITTER,3)
!
ZSPIN = REAL(IO%NSPINS)
ZDT   = (ZSPIN*PTSTEP)
!
!*       1.2 Initialize local variables
!
ILWORK(:) = 0
!
ZAMTRX(:,:) = 0.0
ZBMTRX(:,:) = 0.0
ZCMTRX(:,:) = 0.0
!
ZALPHA(:,:) = 0.0
ZBETA (:,:) = 0.0
ZGAMMA(:,:) = 0.0
!
ZA_BIO (:,:) = 0.0
ZB_BIO (:,:) = 0.0
ZA_CRYO(:,:) = 0.0
ZB_CRYO(:,:) = 0.0
!
ZFRC_SOILCARB(:,:,:) = XUNDEF
ZFRC_LITTER  (:,:,:) = XUNDEF
!
!*       1.3 Initialize soil carbon for final budget
!
ZOLD_SOILCARB(:)=0.0
DO JK=1,INC
   DO JL=1,INL
      DO JI=1,INI
         IDEPTH=PK%NWG_LAYER(JI)
         IF(JL<=IDEPTH)THEN
            ZOLD_SOILCARB(JI)=ZOLD_SOILCARB(JI)+PEK%XSOILDIF_CARB(JI,JL,JK)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
ZOLD_LITTER(:)=0.0
DO JK=1,INLIT
   DO JL=1,INL
      DO JI=1,INI
         IDEPTH=PK%NWG_LAYER(JI)
         IF(JL<=IDEPTH)THEN
           ZOLD_LITTER(JI)=ZOLD_LITTER(JI)+PEK%XSOILDIF_LITTER(JI,JL,JK)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
!*       1.4 Initialize soil carbon density
!
ZDENS_SOILCARB(:,:,1) = PEK%XSOILDIF_CARB(:,:,1)/PK%XDZG(:,:)
ZDENS_SOILCARB(:,:,2) = PEK%XSOILDIF_CARB(:,:,2)/PK%XDZG(:,:)
ZDENS_SOILCARB(:,:,3) = PEK%XSOILDIF_CARB(:,:,3)/PK%XDZG(:,:)
!
ZDENS_LITTER(:,:,1) = PEK%XSOILDIF_LITTER(:,:,1)/PK%XDZG(:,:)
ZDENS_LITTER(:,:,2) = PEK%XSOILDIF_LITTER(:,:,2)/PK%XDZG(:,:)
!
!-------------------------------------------------------------------------------
! 2. Initialize constant for carbon dyn
!-------------------------------------------------------------------------------
!
!m/an -> m/s
IF(IO%LADVECT_SOC)THEN
  ZCSTADVC = XADVREF/XSIYEA
ELSE
  ZCSTADVC = 0.0
ENDIF
!
!-------------------------------------------------------------------------------
! 3. Bioturbation matrix profile 
!-------------------------------------------------------------------------------
!
IF(IO%LBIOTURB)THEN
!
! Bioturbation only over PDBIO
  ILWORK(:) = 0
  DO JL=1,INL
     DO JI=1,INI  
        IF(PK%XDG(JI,JL)<=PDBIO(JI))THEN
           ILWORK(JI)=JL
        ENDIF
     ENDDO
  ENDDO
!
! Upper BC
  WHERE(ILWORK(:)>1)
     ZA_BIO(:,1)=0.0
     ZB_BIO(:,1)=1.0
  ENDWHERE
!
! Interior Grid
  DO JL=2,INL-1
     DO JI=1,INI
        IDEPTH=ILWORK(JI)
        IF(IDEPTH>2.AND.JL<IDEPTH)THEN             
          ZA_BIO(JI,JL) = 1.0
          ZB_BIO(JI,JL) = 1.0        
        ENDIF
     ENDDO
  ENDDO
!
! Lower BC
  DO JI=1,INI
     IDEPTH=ILWORK(JI)
     IF(IDEPTH>1)THEN             
       ZA_BIO(JI,IDEPTH) = 1.0
       ZB_BIO(JI,IDEPTH) = 0.0
     ENDIF
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
! 4. Cryoturbation matrix profile 
!-------------------------------------------------------------------------------
!
IF(IO%LCRYOTURB)THEN
!
! Cryoturbation only over PDCRYO
  ILWORK(:) = 0
  DO JL=1,INL
     DO JI=1,INI  
        IF(PK%XDG(JI,JL)<=PDCRYO(JI))THEN
           ILWORK(JI)=JL
        ENDIF
     ENDDO
  ENDDO
!
! Upper BC
  WHERE(ILWORK(:)>1)
        ZA_CRYO(:,1)=0.0
        ZB_CRYO(:,1)=1.0
  ENDWHERE
!
! Interior Grid
  DO JL=2,INL-1
     DO JI=1,INI
        IDEPTH=ILWORK(JI)
        IF(IDEPTH>2.AND.JL<IDEPTH)THEN             
          ZA_CRYO(JI,JL) = 1.0
          ZB_CRYO(JI,JL) = 1.0        
        ENDIF
     ENDDO
  ENDDO
!
! Lower BC
  DO JI=1,INI
     IDEPTH=ILWORK(JI)
     IF(IDEPTH>1)THEN             
        ZA_CRYO(JI,IDEPTH) = 1.0
        ZB_CRYO(JI,IDEPTH) = 0.0
     ENDIF
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
! 4. Carbon dynamic solver
!-------------------------------------------------------------------------------
!
!*       4.1 Upper BC
!
ZALPHA(:,1) = 0.0
ZBETA (:,1) = (ZB_CRYO(:,1)*PDIFCRYO(:,1)+ZB_BIO(:,1)*PDIFBIO(:,1))/PK%XDZDIF(:,1)
!
ZAMTRX(:,1) = 0.0
ZBMTRX(:,1) = PK%XDZG(:,1)/ZDT+ZWGHT*(ZBETA(:,1)+ZCSTADVC)
ZCMTRX(:,1) = -ZWGHT*ZBETA(:,1)
!
DO JK=1,INLIT
   DO JI=1,INI
      ZFRC_LITTER(JI,1,JK) = ZDENS_LITTER(JI,1,JK)*(PK%XDZG(JI,1)/ZDT-(1.0-ZWGHT)*(ZBETA(JI,1)+ZCSTADVC)) &
                           + ZDENS_LITTER(JI,2,JK)*(1.0-ZWGHT)*ZBETA(JI,1)
   ENDDO
ENDDO
!
DO JK=1,INC
   DO JI=1,INI
      ZFRC_SOILCARB(JI,1,JK)  = ZDENS_SOILCARB(JI,1,JK)*(PK%XDZG(JI,1)/ZDT-(1.0-ZWGHT)*(ZBETA(JI,1)+ZCSTADVC)) &
                              + ZDENS_SOILCARB(JI,2,JK)*(1.0-ZWGHT)*ZBETA(JI,1)
   ENDDO
ENDDO
!
!*       4.2 Interior Grid : Other sub-surface layers
!
DO JL=2,INL-1
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<IDEPTH)THEN
!              
        ZALPHA(JI,JL) = (ZA_CRYO(JI,JL)*PDIFCRYO(JI,JL-1)+ZA_BIO(JI,JL)*PDIFBIO(JI,JL-1))/PK%XDZDIF(JI,JL-1)
        ZBETA (JI,JL) = (ZB_CRYO(JI,JL)*PDIFCRYO(JI,JL  )+ZB_BIO(JI,JL)*PDIFBIO(JI,JL  ))/PK%XDZDIF(JI,JL  )
        ZGAMMA(JI,JL) = ZALPHA(JI,JL)+ZBETA(JI,JL)
!
        ZAMTRX(JI,JL) = -ZWGHT*(ZALPHA(JI,JL)+ZCSTADVC)
        ZBMTRX(JI,JL) = PK%XDZG(JI,JL)/ZDT+ZWGHT*(ZGAMMA(JI,JL)+ZCSTADVC)
        ZCMTRX(JI,JL) = -ZWGHT*ZBETA(JI,JL)
!        
      ENDIF
   ENDDO
ENDDO
!
DO JK=1,INLIT
   DO JL=2,INL-1
      DO JI=1,INI
         IDEPTH=PK%NWG_LAYER(JI)
         IF(JL<IDEPTH)THEN
           ZFRC_LITTER(JI,JL,JK) = ZDENS_LITTER(JI,JL-1,JK)*(1.0-ZWGHT)*(ZALPHA(JI,JL)+ZCSTADVC)                      &
                                 + ZDENS_LITTER(JI,JL  ,JK)*(PK%XDZG(JI,JL)/ZDT-(1.0-ZWGHT)*(ZGAMMA(JI,JL)+ZCSTADVC)) &
                                 + ZDENS_LITTER(JI,JL+1,JK)*(1.0-ZWGHT)*ZBETA(JI,JL)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JK=1,INC
   DO JL=2,INL-1
      DO JI=1,INI
         IDEPTH=PK%NWG_LAYER(JI)
         IF(JL<IDEPTH)THEN
           ZFRC_SOILCARB(JI,JL,JK) = ZDENS_SOILCARB(JI,JL-1,JK)*(1.0-ZWGHT)*(ZALPHA(JI,JL)+ZCSTADVC)                      &
                                   + ZDENS_SOILCARB(JI,JL  ,JK)*(PK%XDZG(JI,JL)/ZDT-(1.0-ZWGHT)*(ZGAMMA(JI,JL)+ZCSTADVC)) &
                                   + ZDENS_SOILCARB(JI,JL+1,JK)*(1.0-ZWGHT)*ZBETA (JI,JL)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
!*       4.3 Lower BC : last layer
!
DO JI=1,INI
   IDEPTH=PK%NWG_LAYER(JI)
   ZALPHA(JI,IDEPTH) = (ZA_CRYO(JI,IDEPTH)*PDIFCRYO(JI,IDEPTH-1)+ZA_BIO(JI,IDEPTH)*PDIFBIO(JI,IDEPTH-1))/PK%XDZDIF(JI,IDEPTH-1)
   ZBETA (JI,IDEPTH) = 0.0
!
   ZAMTRX(JI,IDEPTH) = -ZWGHT*(ZALPHA(JI,IDEPTH)+ZCSTADVC)
   ZBMTRX(JI,IDEPTH) = PK%XDZG(JI,IDEPTH)/ZDT+ZWGHT*ZALPHA(JI,IDEPTH)
   ZCMTRX(JI,IDEPTH) = 0.0
ENDDO
!
DO JK=1,INLIT
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      ZFRC_LITTER(JI,IDEPTH,JK) = ZDENS_LITTER(JI,IDEPTH-1,JK)*(1.0-ZWGHT)*(ZALPHA(JI,IDEPTH)+ZCSTADVC)           &
                                + ZDENS_LITTER(JI,IDEPTH  ,JK)*(PK%XDZG(JI,IDEPTH)/ZDT-(1.0-ZWGHT)*ZALPHA(JI,IDEPTH))
   ENDDO

ENDDO
!
DO JK=1,INC
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      ZFRC_SOILCARB(JI,IDEPTH,JK) = ZDENS_SOILCARB(JI,IDEPTH-1,JK)*(1.0-ZWGHT)*(ZALPHA(JI,IDEPTH)+ZCSTADVC)           &
                                  + ZDENS_SOILCARB(JI,IDEPTH  ,JK)*(PK%XDZG(JI,IDEPTH)/ZDT-(1.0-ZWGHT)*ZALPHA(JI,IDEPTH))
   ENDDO    
ENDDO
!
!*       4.4 Solver
!
CALL TRIDIAG_DIF(ZAMTRX(:,:),ZBMTRX(:,:),ZCMTRX(:,:),ZFRC_LITTER(:,:,1),PK%NWG_LAYER(:),INL,ZDENS_LITTER(:,:,1))
CALL TRIDIAG_DIF(ZAMTRX(:,:),ZBMTRX(:,:),ZCMTRX(:,:),ZFRC_LITTER(:,:,2),PK%NWG_LAYER(:),INL,ZDENS_LITTER(:,:,2))
!
CALL TRIDIAG_DIF(ZAMTRX(:,:),ZBMTRX(:,:),ZCMTRX(:,:),ZFRC_SOILCARB(:,:,1),PK%NWG_LAYER(:),INL,ZDENS_SOILCARB(:,:,1))
CALL TRIDIAG_DIF(ZAMTRX(:,:),ZBMTRX(:,:),ZCMTRX(:,:),ZFRC_SOILCARB(:,:,2),PK%NWG_LAYER(:),INL,ZDENS_SOILCARB(:,:,2))
CALL TRIDIAG_DIF(ZAMTRX(:,:),ZBMTRX(:,:),ZCMTRX(:,:),ZFRC_SOILCARB(:,:,3),PK%NWG_LAYER(:),INL,ZDENS_SOILCARB(:,:,3))
!
ZNEW_SOILCARB(:)=0.0
DO JK=1,INC
   DO JL=1,INL
      DO JI=1,INI
         IDEPTH=PK%NWG_LAYER(JI)
         IF(JL<=IDEPTH)THEN
            PEK%XSOILDIF_CARB(JI,JL,JK)=ZDENS_SOILCARB(JI,JL,JK)*PK%XDZG(JI,JL)
            ZNEW_SOILCARB(JI)=ZNEW_SOILCARB(JI)+PEK%XSOILDIF_CARB(JI,JL,JK)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
ZNEW_LITTER(:)=0.0
DO JK=1,INLIT
   DO JL=1,INL
      DO JI=1,INI
         IDEPTH=PK%NWG_LAYER(JI)
         IF(JL<=IDEPTH)THEN
           PEK%XSOILDIF_LITTER(JI,JL,JK)=ZDENS_LITTER(JI,JL,JK)*PK%XDZG(JI,JL)
           ZNEW_LITTER(JI)=ZNEW_LITTER(JI)+PEK%XSOILDIF_LITTER(JI,JL,JK)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
! 5. Final budget
!-------------------------------------------------------------------------------
!
ZBUD(:) = ZNEW_LITTER(:)+ZNEW_SOILCARB(:)-ZOLD_LITTER(:)-ZOLD_SOILCARB(:)
!
DO JI=1,INI
   IF(ABS(ZBUD(JI))>ZBUDLIM)THEN
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!'
      WRITE(KLUOUT,*) 'Problem carbon dynamic'
      WRITE(KLUOUT,*) 'Bilan (kg/m2) = ',ZBUD(JI)
      WRITE(KLUOUT,*) 'Grid-cell = ',JI
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!'
      CALL ABOR1_SFX('CARBON_DYNAMIC: NO CONSERVATION SOIL CARBON DYNAMIC')
   ENDIF
ENDDO
!
!--------------------------------------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_DYNAMIC',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------------------------------------
!
END SUBROUTINE CARBON_DYNAMIC
