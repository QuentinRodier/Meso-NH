!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE COMPUT_COLD_LAYERS_THICK(PDG,PTG,PPLT,PALT,PFLT)
!     ###############################################################################
!
!!****  *COMPUT_COLD_LAYERS_THICK* - additional diagnostics for ISBA
!!
!!    PURPOSE
!!    -------
!! Comput active layer (ALT) and frozen layer (FLT) thicknesses 
!! using linear interpolation between two nodes :
!!       ALT = depth to zero centigrade isotherm in permafrost
!!       FLT = depth to zero centigrade isotherm in non-permafrost
!!       PLT = Permafrost layer thicknesses
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2014
!!
!!------------------------------------------------------------------
!
USE MODD_CSTS,     ONLY : XTT
USE MODD_SURF_PAR, ONLY : NUNDEF, XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:),  INTENT(IN)  :: PDG           ! soil layer depth
REAL, DIMENSION(:,:),  INTENT(IN)  :: PTG           ! soil temperature
REAL, DIMENSION(:),    INTENT(OUT) :: PPLT          ! permafrost thickness
REAL, DIMENSION(:),    INTENT(OUT) :: PALT          ! active layer thickness
REAL, DIMENSION(:),    INTENT(OUT) :: PFLT          ! frozen layer thickness
!
!*      0.2    declarations of local variables
!
REAL,    DIMENSION(SIZE(PDG,1),SIZE(PDG,2)) :: ZNODE
INTEGER, DIMENSION(SIZE(PDG,1))             :: IUP_ALT, IDOWN_ALT
INTEGER, DIMENSION(SIZE(PDG,1))             :: IUP_FLT, IDOWN_FLT
!
REAL    :: ZTT
REAL    :: ZTG_UP, ZTG_DOWN
REAL    :: ZUP, ZDOWN
REAL    :: ZALT, ZFLT, ZSLOPE
!
INTEGER :: JI, JL, INI, INL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COMPUT_COLD_LAYERS_THICK',0,ZHOOK_HANDLE)
!
INI=SIZE(PDG,1)
INL=SIZE(PDG,2)
!
IF(INL<3)THEN
  PPLT(:) = XUNDEF
  PALT(:) = XUNDEF
  PFLT(:) = XUNDEF
  IF (LHOOK) CALL DR_HOOK('COMPUT_COLD_LAYERS_THICK',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
ZTT=XTT+0.01
!
IUP_ALT  (:)=0
IDOWN_ALT(:)=0
IUP_FLT  (:)=0
IDOWN_FLT(:)=0
!
!Surface soil layer
!
ZNODE(:,1)=0.5*PDG(:,1)
WHERE(PTG(:,1)>ZTT.AND.PTG(:,2)<=ZTT.AND.PTG(:,3)<=ZTT)
      IUP_ALT  (:)=1
      IDOWN_ALT(:)=2
ENDWHERE
WHERE(PTG(:,1)<ZTT.AND.PTG(:,2)>=ZTT.AND.PTG(:,3)>=ZTT)
      IUP_FLT  (:)=1
      IDOWN_FLT(:)=2
ENDWHERE
!
!Middle soil layer
!
DO JL=2,INL-1
   DO JI=1,INI
      ZNODE(JI,JL)=0.5*(PDG(JI,JL)+PDG(JI,JL-1))
      IF(PTG(JI,JL-1)>ZTT.AND.PTG(JI,JL)>ZTT.AND.PTG(JI,JL+1)<=ZTT)THEN
        IUP_ALT  (JI)=JL
        IDOWN_ALT(JI)=JL+1
      ENDIF
      IF(PTG(JI,JL-1)<ZTT.AND.PTG(JI,JL)<ZTT.AND.PTG(JI,JL+1)>=ZTT)THEN
        IUP_FLT  (JI)=JL
        IDOWN_FLT(JI)=JL+1
      ENDIF      
   ENDDO
ENDDO
!
!Last soil layer
!
ZNODE(:,INL)=0.5*(PDG(:,INL)+PDG(:,INL-1))
!
PALT(:)=0.0
WHERE(PTG(:,INL)>ZTT)
     IDOWN_ALT(:)=NUNDEF
     PPLT     (:)=0.0
ELSEWHERE
     PPLT(:)=PDG(:,INL)
ENDWHERE
!
PFLT(:)=0.0
WHERE(PTG(:,INL)<ZTT)IDOWN_FLT(:)=NUNDEF
!
DO JI=1,INI
!
   IF(IDOWN_ALT(JI)>0.AND.IDOWN_ALT(JI)<=INL)THEN
     ZTG_UP    = PTG  (JI,IUP_ALT  (JI))
     ZTG_DOWN  = PTG  (JI,IDOWN_ALT(JI))
     ZUP       = ZNODE(JI,IUP_ALT  (JI))
     ZDOWN     = ZNODE(JI,IDOWN_ALT(JI))
     ZSLOPE    = (ZUP-ZDOWN)/(ZTG_UP-ZTG_DOWN)
     PALT(JI)  = ZDOWN+(ZTT-ZTG_DOWN)*ZSLOPE
     PPLT(JI)  = PDG(JI,INL)-PALT(JI)
   ENDIF
!
   IF(IDOWN_FLT(JI)>0.AND.IDOWN_FLT(JI)<=INL)THEN
     ZTG_UP    = PTG  (JI,IUP_FLT  (JI))
     ZTG_DOWN  = PTG  (JI,IDOWN_FLT(JI))
     ZUP       = ZNODE(JI,IUP_FLT  (JI))
     ZDOWN     = ZNODE(JI,IDOWN_FLT(JI))
     ZSLOPE    = (ZUP-ZDOWN)/(ZTG_UP-ZTG_DOWN)
     PFLT(JI)  = ZDOWN+(ZTT-ZTG_DOWN)*ZSLOPE
   ENDIF
!
ENDDO
!
WHERE(PPLT(:)>0.0)
     PFLT(:)=PDG(:,INL)
ENDWHERE
!
WHERE(PPLT(:)==0.0)
     PALT(:)=PDG(:,INL)
ENDWHERE
!
!
IF (LHOOK) CALL DR_HOOK('COMPUT_COLD_LAYERS_THICK',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COMPUT_COLD_LAYERS_THICK

