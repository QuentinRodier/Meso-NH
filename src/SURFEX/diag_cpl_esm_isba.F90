!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_CPL_ESM_ISBA (I, &
                                    PTSTEP,PCPL_DRAIN,PCPL_RUNOFF,PCPL_EFLOOD, &
                                     PCPL_PFLOOD,PCPL_IFLOOD,PCPL_ICEFLUX         )  
!     #####################################################################
!
!!****  *DIAG_CPL_ESM_ISBA*  
!!
!!    PURPOSE
!!    -------
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      B. Decharme    01/16 : Bug with flood budget and add cpl keys
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODN_SFX_OASIS, ONLY : XTSTEP_CPL_LAND
!
USE MODD_SFX_OASIS,  ONLY : LCPL_FLOOD, LCPL_GW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
REAL, INTENT(IN)                   :: PTSTEP
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_DRAIN
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_RUNOFF
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_EFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_PFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_IFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_ICEFLUX
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PCPL_DRAIN,1),SIZE(PCPL_DRAIN,2)) :: ZCPL_DRAIN
REAL, DIMENSION(SIZE(PCPL_DRAIN,1),SIZE(PCPL_DRAIN,2)) :: ZCPL_RECHARGE
!
REAL, DIMENSION(SIZE(I%XPATCH,1)) :: ZSUMPATCH
REAL, DIMENSION(SIZE(I%XPATCH,1)) :: ZBUDGET
!
INTEGER :: INI,INP
INTEGER :: JI, JPATCH ! tile loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_ISBA',0,ZHOOK_HANDLE)
!
!* Initialization
!  --------------
!
INI=SIZE(I%XPATCH,1)
INP=SIZE(I%XPATCH,2)
!
ZSUMPATCH(:) = 0.0
DO JPATCH=1,INP
  DO JI=1,INI
     ZSUMPATCH(JI) = ZSUMPATCH(JI) + I%XPATCH(JI,JPATCH)
  ENDDO
ENDDO
!
ZCPL_RECHARGE(:,:) = 0.0
!
IF(I%CISBA/='DIF')THEN
! prevent small negatives values with ISBA-FR
  ZCPL_DRAIN(:,:)=MAX(0.0,PCPL_DRAIN(:,:))
ELSE
  ZCPL_DRAIN(:,:)=PCPL_DRAIN(:,:)
ENDIF
!
!* groundwater case
!  ----------------
!
IF(LCPL_GW.AND.I%LWTD)THEN
  DO JPATCH=1,INP
    DO JI=1,INI
      IF(I%XGW(JI)>0.0.AND.ZSUMPATCH(JI)>0.0)THEN
        ZCPL_RECHARGE(JI,JPATCH) = PCPL_DRAIN(JI,JPATCH)
        ZCPL_DRAIN   (JI,JPATCH) = 0.0
      ENDIF
    ENDDO
  ENDDO
ENDIF
!
!* update ISBA - RRM coupling variable (kg/m2)
!  -------------------------------------------
!
!kg/m²
DO JPATCH=1,INP
  DO JI=1,INI
!  
     IF(ZSUMPATCH(JI)>0.0)THEN
       I%XCPL_DRAIN (JI) = I%XCPL_DRAIN (JI) + PTSTEP * ZCPL_DRAIN (JI,JPATCH) * I%XPATCH(JI,JPATCH)/ZSUMPATCH(JI) 
       I%XCPL_RUNOFF(JI) = I%XCPL_RUNOFF(JI) + PTSTEP * PCPL_RUNOFF(JI,JPATCH) * I%XPATCH(JI,JPATCH)/ZSUMPATCH(JI) 
     ENDIF
!
     IF(I%LGLACIER.AND.ZSUMPATCH(JI)>0.0)THEN
        I%XCPL_ICEFLUX(JI) = I%XCPL_ICEFLUX(JI) + PTSTEP * PCPL_ICEFLUX(JI,JPATCH) * I%XPATCH(JI,JPATCH)/ZSUMPATCH(JI)
     ENDIF
!
     IF(LCPL_GW.AND.I%LWTD.AND.ZSUMPATCH(JI)>0.0)THEN
        I%XCPL_RECHARGE(JI) = I%XCPL_RECHARGE(JI) + PTSTEP * ZCPL_RECHARGE(JI,JPATCH) * I%XPATCH(JI,JPATCH)/ZSUMPATCH(JI)
     ENDIF
!   
     IF(LCPL_FLOOD.AND.I%LFLOOD.AND.ZSUMPATCH(JI)>0.0)THEN
        I%XCPL_EFLOOD  (JI) = I%XCPL_EFLOOD  (JI) + PTSTEP * PCPL_EFLOOD  (JI,JPATCH)*I%XPATCH(JI,JPATCH)/ZSUMPATCH(JI)
        I%XCPL_PFLOOD  (JI) = I%XCPL_PFLOOD  (JI) + PTSTEP * PCPL_PFLOOD  (JI,JPATCH)*I%XPATCH(JI,JPATCH)/ZSUMPATCH(JI)
        I%XCPL_IFLOOD  (JI) = I%XCPL_IFLOOD  (JI) + PTSTEP * PCPL_IFLOOD  (JI,JPATCH)*I%XPATCH(JI,JPATCH)/ZSUMPATCH(JI)
     ENDIF
!    
  ENDDO
ENDDO
!
!* update ISBA Floodplains variable for mass conservation (kg/m2)
!  --------------------------------------------------------------
!
IF(LCPL_FLOOD.AND.I%LFLOOD)THEN
  ZBUDGET(:)=(I%XPIFLOOD(:)*XTSTEP_CPL_LAND)+I%XCPL_PFLOOD(:)-I%XCPL_IFLOOD(:)-I%XCPL_EFLOOD(:)
  WHERE(ZBUDGET (:)<=0.0)
        I%XPIFLOOD(:)=0.0
        I%XFFLOOD (:)=0.0
  ENDWHERE
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_ISBA',1,ZHOOK_HANDLE)
!
END SUBROUTINE DIAG_CPL_ESM_ISBA
