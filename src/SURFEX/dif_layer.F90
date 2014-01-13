!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!#############################################################
SUBROUTINE DIF_LAYER(KLU, KGROUND_LAYER, KPATCH, KSIZE_NATURE_P, &
                     PPATCH, PDG, PDROOT, PDG2, PROOTFRAC,       &
                     KWG_LAYER, PDZG, PDZDIF, PSOILWGHT,         &
                     PRUNOFFD, KLAYER_HORT, KLAYER_DUN           )  
!#############################################################
!
!!****  *DIF_LAYER_n* - routine to initialize dif numbers of layers
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!    S. Faroux
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2012!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SGH_PAR,        ONLY : XHORT_DEPTH
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KGROUND_LAYER
INTEGER, INTENT(IN) :: KPATCH
INTEGER, DIMENSION(:),  INTENT(IN) :: KSIZE_NATURE_P
REAL, DIMENSION(:,:),   INTENT(IN) :: PPATCH
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDG
REAL, DIMENSION(:,:),   INTENT(IN) :: PDROOT
REAL, DIMENSION(:,:),   INTENT(IN) :: PDG2
REAL, DIMENSION(:,:,:), INTENT(IN) :: PROOTFRAC
INTEGER, DIMENSION(:,:),INTENT(IN) :: KWG_LAYER
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDZG
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDZDIF
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSOILWGHT
REAL, DIMENSION(:,:),   INTENT(OUT) :: PRUNOFFD
INTEGER, INTENT(OUT) :: KLAYER_HORT
INTEGER, INTENT(OUT) :: KLAYER_DUN
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KLU) :: ZWORK
INTEGER, DIMENSION(KLU,KPATCH) :: IWORK
INTEGER :: JLAYER, JPATCH, JILU, IDEPTH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('DIF_LAYER',0,ZHOOK_HANDLE)
!
DO JLAYER = 1, KGROUND_LAYER
  IF (ANY((PROOTFRAC(:,JLAYER,:)<0. .OR. PROOTFRAC(:,JLAYER,:)>1.) .AND. PPATCH(:,:).NE.0.)) &
    CALL ABOR1_SFX('DIF_LAYER: WITH CISBA=DIF ROOTFRAC MUST BE DEFINED')
ENDDO
!
PDZG     (:,:,:) = XUNDEF
PDZDIF   (:,:,:) = XUNDEF
PSOILWGHT(:,:,:) = 0.0
!
DO JPATCH=1,KPATCH
!
  IF (KSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
!
!*   soil layers thicknesses
  PDZG(:,1,JPATCH) = PDG(:,1,JPATCH)
  DO JLAYER=2,KGROUND_LAYER
    DO JILU=1,KLU
      PDZG(JILU,JLAYER,JPATCH) = PDG(JILU,JLAYER,JPATCH) - PDG(JILU,JLAYER-1,JPATCH)
    ENDDO
  ENDDO
!
!*   distance between consecuative layer mid-points
  DO JLAYER=1,KGROUND_LAYER
    DO JILU=1,KLU
      IF(JLAYER<KGROUND_LAYER)THEN
        PDZDIF(JILU,JLAYER,JPATCH)=0.5*(PDZG(JILU,JLAYER,JPATCH)+PDZG(JILU,JLAYER+1,JPATCH))
      ELSE
        PDZDIF(JILU,JLAYER,JPATCH)=0.5*PDZG(JILU,JLAYER,JPATCH) 
      ENDIF
    ENDDO
  ENDDO 
! 
ENDDO
!
! Horton runoff parameter
!
IWORK(:,:) = KWG_LAYER(:,:)
!
DO JPATCH=1,KPATCH
!  
  IF( KSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
!
  DO JILU=1,KLU
    IDEPTH = KWG_LAYER(JILU,JPATCH)
    IF (IDEPTH==NUNDEF) IDEPTH = KGROUND_LAYER
    DO JLAYER=1,IDEPTH-1
      IF(PDG(JILU,JLAYER,JPATCH)<XHORT_DEPTH) IWORK(JILU,JPATCH)=JLAYER+1
    ENDDO
  ENDDO
!
END DO
!  
KLAYER_HORT=MAXVAL(IWORK(:,:),IWORK(:,:)/=NUNDEF)
!  
! Dunne runoff parameter
!
IWORK(:,:)=KWG_LAYER(:,:)
!
DO JPATCH=1,KPATCH
!  
  IF (KSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
!
  DO JILU=1,KLU
    IF(PPATCH(JILU,JPATCH)>0.0)THEN 
      IDEPTH = KWG_LAYER(JILU,JPATCH)    
      IF(PDROOT(JILU,JPATCH)>0.0.AND.PDROOT(JILU,JPATCH)/=XUNDEF)THEN
        PRUNOFFD(JILU,JPATCH) = PDG(JILU,1,JPATCH)
        DO JLAYER=1,IDEPTH-1
          IF(PROOTFRAC(JILU,JLAYER,JPATCH)<0.90)THEN
            PRUNOFFD(JILU,JPATCH) = PDG(JILU,JLAYER+1,JPATCH)
          ENDIF
        ENDDO
      ELSE
        PRUNOFFD(JILU,JPATCH) = MIN(0.6,PDG2(JILU,JPATCH))
      ENDIF
    ENDIF
  ENDDO
!
  ZWORK(:) = 0.0
  DO JLAYER=1,KGROUND_LAYER
    DO JILU=1,KLU
      IF(PPATCH(JILU,JPATCH)>0.0)THEN
        IDEPTH=KWG_LAYER(JILU,JPATCH)
        IF(JLAYER<=IDEPTH)THEN
          ZWORK    (JILU              ) = ZWORK(JILU) + PDZG(JILU,JLAYER,JPATCH)  
          PSOILWGHT(JILU,JLAYER,JPATCH) = MIN(PDZG(JILU,JLAYER,JPATCH), &
                                          MAX(0.0,PRUNOFFD(JILU,JPATCH)-ZWORK(JILU)+PDZG(JILU,JLAYER,JPATCH)))
        ENDIF
        IF(PDG(JILU,JLAYER,JPATCH)<PRUNOFFD(JILU,JPATCH))THEN
          IWORK(JILU,JPATCH)=JLAYER+1
        ENDIF
      ENDIF
    ENDDO
  ENDDO
!  
END DO
!
KLAYER_DUN=MAXVAL(IWORK(:,:),IWORK(:,:)/=NUNDEF)
!
IF (LHOOK) CALL DR_HOOK('DIF_LAYER',1,ZHOOK_HANDLE)
!
END SUBROUTINE DIF_LAYER
