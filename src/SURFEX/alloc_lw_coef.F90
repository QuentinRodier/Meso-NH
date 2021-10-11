SUBROUTINE ALLOC_LW_COEF(LW,KI)

USE MODD_LW_COEF, ONLY : LW_COEF_t

USE MODD_SURF_PAR, ONLY : XUNDEF

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE(LW_COEF_t), INTENT(INOUT) :: LW
INTEGER,           INTENT(IN) :: KI     ! number of points

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ALLOC_LW_COEF',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------)
!
ALLOCATE(LW%XLW_WA_TO_WB (KI)) ! L.W. interactions wall->opposite wall
ALLOCATE(LW%XLW_WA_TO_R  (KI)) ! L.W. interactions wall->road for road balance 
ALLOCATE(LW%XLW_WB_TO_R  (KI)) ! L.W. interactions wall->road for road balance 
ALLOCATE(LW%XLW_WA_TO_NR (KI)) ! L.W. interactions wall->snow for snow balance
ALLOCATE(LW%XLW_WB_TO_NR (KI)) ! L.W. interactions wall->snow for snow balance
ALLOCATE(LW%XLW_WA_TO_G  (KI)) ! L.W. interactions wall->GARDEN areas for garden balance
ALLOCATE(LW%XLW_WB_TO_G  (KI)) ! L.W. interactions wall->GARDEN areas for garden balance
ALLOCATE(LW%XLW_WA_TO_WIN(KI)) ! L.W. interactions wall->win for window balance
ALLOCATE(LW%XLW_WB_TO_WIN(KI)) ! L.W. interactions wall->win for window balance
ALLOCATE(LW%XLW_WA_TO_HV  (KI)) ! L.W. interactions wall->high vegetation areas for high veg balance
ALLOCATE(LW%XLW_WB_TO_HV  (KI)) ! L.W. interactions wall->high vegetation areas for high veg balance
ALLOCATE(LW%XLW_WA_TO_S   (KI)) ! L.W. interactions wall->sky for sky balance
ALLOCATE(LW%XLW_WB_TO_S   (KI)) ! L.W. interactions wall->sky for sky balance
!
ALLOCATE(LW%XLW_R_TO_WA (KI)) ! L.W. interactions road->wall for wall balance
ALLOCATE(LW%XLW_R_TO_WB (KI)) ! L.W. interactions road->wall for wall balance
ALLOCATE(LW%XLW_R_TO_WIN  (KI)) ! L.W. interactions road->win for win balance
ALLOCATE(LW%XLW_R_TO_HV   (KI)) ! L.W. interactions road->high vegetation for high veg balance
ALLOCATE(LW%XLW_R_TO_S    (KI)) ! L.W. interactions road->sky for sky balance
!
!
ALLOCATE(LW%XLW_G_TO_WA (KI)) ! L.W. interactions GARDEN areas->wall for wall balance
ALLOCATE(LW%XLW_G_TO_WB (KI)) ! L.W. interactions GARDEN areas->wall for wall balance
ALLOCATE(LW%XLW_G_TO_WIN  (KI)) ! L.W. interactions GARDEN areas->road for window balance
ALLOCATE(LW%XLW_G_TO_HV   (KI)) ! L.W. interactions GARDEN areas->high vegetation for high veg balance
ALLOCATE(LW%XLW_G_TO_S    (KI)) ! L.W. interactions GARDEN areas->sky for sky balance
!
ALLOCATE(LW%XLW_S_TO_WA (KI)) ! L.W. interactions sky->wall for wall balance
ALLOCATE(LW%XLW_S_TO_WB (KI)) ! L.W. interactions sky->wall for wall balance
ALLOCATE(LW%XLW_S_TO_R  (KI)) ! L.W. interactions sky->road for raod balance
ALLOCATE(LW%XLW_S_TO_NR (KI)) ! L.W. interactions sky->snow for snow balance 
ALLOCATE(LW%XLW_S_TO_G  (KI)) ! L.W. interactions sky->GARDEN areas for garden balance
ALLOCATE(LW%XLW_S_TO_WIN (KI)) ! L.W. interactions sky->win for window balance
ALLOCATE(LW%XLW_S_TO_HV  (KI)) ! L.W. interactions sky->high vegetation areas for high veg balance
!
ALLOCATE(LW%XLW_WIN_TO_WA (KI)) ! L.W. interactions win->wall for wall balance
ALLOCATE(LW%XLW_WIN_TO_WB (KI)) ! L.W. interactions win->wall for wall balance
ALLOCATE(LW%XLW_WIN_TO_R  (KI)) ! L.W. interactions win->road for road balance
ALLOCATE(LW%XLW_WIN_TO_NR (KI)) ! L.W. interactions win->GARDEN areas for snow balance
ALLOCATE(LW%XLW_WIN_TO_G  (KI)) ! L.W. interactions win->GARDEN areas for garden balance
ALLOCATE(LW%XLW_WIN_TO_HV (KI)) ! L.W. interactions win->high vegetation areas for high veg balance
ALLOCATE(LW%XLW_WIN_TO_S  (KI)) ! L.W. interactions win->sky for sky balance
!
!
ALLOCATE(LW%XLW_NR_TO_WA  (KI)) ! L.W. interactions snow(road)->wall for wall balance
ALLOCATE(LW%XLW_NR_TO_WB  (KI)) ! L.W. interactions snow(road)->wall for wall balance
ALLOCATE(LW%XLW_NR_TO_WIN (KI)) ! L.W. interactions snow(road)->WIN areas for window balance
ALLOCATE(LW%XLW_NR_TO_HV  (KI)) ! L.W. interactions snow(road)->high vegetation for high veg balance
ALLOCATE(LW%XLW_NR_TO_S   (KI)) ! L.W. interactions snow(road)->sky for sky balance
!
ALLOCATE(LW%XLW_HV_TO_WA  (KI)) ! L.W. interaction high veg -> wall for wall balance
ALLOCATE(LW%XLW_HV_TO_WB  (KI)) ! L.W. interaction high veg -> wall for wall balance
ALLOCATE(LW%XLW_HV_TO_WIN (KI)) ! L.W. interaction high veg -> wall for wall balance
ALLOCATE(LW%XLW_HV_TO_G   (KI)) ! L.W. interaction high veg -> garden for garden balance
ALLOCATE(LW%XLW_HV_TO_R   (KI)) ! L.W. interaction high veg -> road for road balance 
ALLOCATE(LW%XLW_HV_TO_NR  (KI)) ! L.W. interaction high veg -> snow for road balance 
ALLOCATE(LW%XLW_HV_TO_S   (KI)) ! L.W. interaction high veg -> sky for sky balance 
!
LW%XLW_WA_TO_WB  = XUNDEF ! L.W. interactions wall->opposite wall
LW%XLW_WA_TO_R   = XUNDEF ! L.W. interactions wall->road for road balance 
LW%XLW_WB_TO_R   = XUNDEF ! L.W. interactions wall->road for road balance 
LW%XLW_WA_TO_NR  = XUNDEF ! L.W. interactions wall->snow for snow balance
LW%XLW_WB_TO_NR  = XUNDEF ! L.W. interactions wall->snow for snow balance
LW%XLW_WA_TO_G   = XUNDEF ! L.W. interactions wall->GARDEN areas for garden balance
LW%XLW_WB_TO_G   = XUNDEF ! L.W. interactions wall->GARDEN areas for garden balance
LW%XLW_WA_TO_WIN = XUNDEF ! L.W. interactions wall->win for window balance
LW%XLW_WB_TO_WIN = XUNDEF ! L.W. interactions wall->win for window balance
LW%XLW_WA_TO_HV   = XUNDEF ! L.W. interactions wall->high vegetation areas for high veg balance
LW%XLW_WB_TO_HV   = XUNDEF ! L.W. interactions wall->high vegetation areas for high veg balance
LW%XLW_WA_TO_S    = XUNDEF ! L.W. interactions wall->sky for sky balance
LW%XLW_WB_TO_S    = XUNDEF ! L.W. interactions wall->sky for sky balance
!
LW%XLW_R_TO_WA  = XUNDEF ! L.W. interactions road->wall for wall balance
LW%XLW_R_TO_WB  = XUNDEF ! L.W. interactions road->wall for wall balance
LW%XLW_R_TO_WIN   = XUNDEF ! L.W. interactions road->win for win balance
LW%XLW_R_TO_HV    = XUNDEF ! L.W. interactions road->high vegetation for high veg balance
LW%XLW_R_TO_S     = XUNDEF ! L.W. interactions road->sky for sky balance
!
!
LW%XLW_G_TO_WA  = XUNDEF ! L.W. interactions GARDEN areas->wall for wall balance
LW%XLW_G_TO_WB  = XUNDEF ! L.W. interactions GARDEN areas->wall for wall balance
LW%XLW_G_TO_WIN   = XUNDEF ! L.W. interactions GARDEN areas->road for window balance
LW%XLW_G_TO_HV    = XUNDEF ! L.W. interactions GARDEN areas->high vegetation for high veg balance
LW%XLW_G_TO_S     = XUNDEF ! L.W. interactions GARDEN areas->sky for sky balance
!
LW%XLW_S_TO_WA  = XUNDEF ! L.W. interactions sky->wall for wall balance
LW%XLW_S_TO_WB  = XUNDEF ! L.W. interactions sky->wall for wall balance
LW%XLW_S_TO_R   = XUNDEF ! L.W. interactions sky->road for raod balance
LW%XLW_S_TO_NR  = XUNDEF ! L.W. interactions sky->snow for snow balance 
LW%XLW_S_TO_G   = XUNDEF ! L.W. interactions sky->GARDEN areas for garden balance
LW%XLW_S_TO_WIN  = XUNDEF ! L.W. interactions sky->win for window balance
LW%XLW_S_TO_HV   = XUNDEF ! L.W. interactions sky->high vegetation areas for high veg balance
!
LW%XLW_WIN_TO_WA  = XUNDEF ! L.W. interactions win->wall for wall balance
LW%XLW_WIN_TO_WB  = XUNDEF ! L.W. interactions win->wall for wall balance
LW%XLW_WIN_TO_R   = XUNDEF ! L.W. interactions win->road for road balance
LW%XLW_WIN_TO_NR  = XUNDEF ! L.W. interactions win->GARDEN areas for snow balance
LW%XLW_WIN_TO_G   = XUNDEF ! L.W. interactions win->GARDEN areas for garden balance
LW%XLW_WIN_TO_HV  = XUNDEF ! L.W. interactions win->high vegetation areas for high veg balance
LW%XLW_WIN_TO_S   = XUNDEF ! L.W. interactions win->sky for sky balance
!
!
LW%XLW_NR_TO_WA   = XUNDEF ! L.W. interactions snow(road)->wall for wall balance
LW%XLW_NR_TO_WB   = XUNDEF ! L.W. interactions snow(road)->wall for wall balance
LW%XLW_NR_TO_WIN  = XUNDEF ! L.W. interactions snow(road)->WIN areas for window balance
LW%XLW_NR_TO_HV   = XUNDEF ! L.W. interactions snow(road)->high vegetation for high veg balance
LW%XLW_NR_TO_S    = XUNDEF ! L.W. interactions snow(road)->sky for sky balance
!
LW%XLW_HV_TO_WA   = XUNDEF ! L.W. interaction high veg -> wall for wall balance
LW%XLW_HV_TO_WB   = XUNDEF ! L.W. interaction high veg -> wall for wall balance
LW%XLW_HV_TO_WIN  = XUNDEF ! L.W. interaction high veg -> wall for wall balance
LW%XLW_HV_TO_G    = XUNDEF ! L.W. interaction high veg -> garden for garden balance
LW%XLW_HV_TO_R    = XUNDEF ! L.W. interaction high veg -> road for road balance 
LW%XLW_HV_TO_NR   = XUNDEF ! L.W. interaction high veg -> snow for road balance 
LW%XLW_HV_TO_S    = XUNDEF ! L.W. interaction high veg -> sky for sky balance 
!
IF (LHOOK) CALL DR_HOOK('ALLOC_LW_COEF',1,ZHOOK_HANDLE)

END SUBROUTINE
