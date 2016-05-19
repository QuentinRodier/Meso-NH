!     #########
SUBROUTINE READ_NAMELISTS_IDEAL(HPROGRAM)
!     #######################################################
!
!---------------------------    
!
USE MODN_IDEAL_FLUX, ONLY : NFORCF, NFORCT, XTIMEF, XTIMET, XSFTH, XSFTQ, XSFCO2, &
                            CUSTARTYPE, XUSTAR, XZ0, XALB, XEMIS, XTSRAD
!
USE MODI_DEFAULT_IDEAL_FLUX
USE MODI_READ_IDEAL_FLUX_CONF
!
!--------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------
!                  
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_IDEAL',0,ZHOOK_HANDLE)
 CALL DEFAULT_IDEAL_FLUX(NFORCF, NFORCT, XTIMEF, XTIMET, XSFTH, XSFTQ, XSFCO2, &
                        CUSTARTYPE, XUSTAR, XZ0, XALB, XEMIS, XTSRAD)
!
 CALL READ_IDEAL_FLUX_CONF(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_IDEAL',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------
!
END SUBROUTINE READ_NAMELISTS_IDEAL
