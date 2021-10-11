SUBROUTINE DEALLOC_LW_COEF(LW)

USE MODD_LW_COEF, ONLY : LW_COEF_t

USE MODD_SURF_PAR, ONLY : XUNDEF

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE(LW_COEF_t), INTENT(INOUT) :: LW

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ALLOC_LW_COEF',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------)
!
DEALLOCATE(LW%XLW_WA_TO_WB) ! L.W. interactions wall->opposite wall
DEALLOCATE(LW%XLW_WA_TO_R  ) ! L.W. interactions wall->road for road balance 
DEALLOCATE(LW%XLW_WB_TO_R  ) ! L.W. interactions wall->road for road balance 
DEALLOCATE(LW%XLW_WA_TO_NR ) ! L.W. interactions wall->snow for snow balance
DEALLOCATE(LW%XLW_WB_TO_NR ) ! L.W. interactions wall->snow for snow balance
DEALLOCATE(LW%XLW_WA_TO_G  ) ! L.W. interactions wall->GARDEN areas for garden balance
DEALLOCATE(LW%XLW_WB_TO_G  ) ! L.W. interactions wall->GARDEN areas for garden balance
DEALLOCATE(LW%XLW_WA_TO_WIN) ! L.W. interactions wall->win for window balance
DEALLOCATE(LW%XLW_WB_TO_WIN) ! L.W. interactions wall->win for window balance
DEALLOCATE(LW%XLW_WA_TO_HV  ) ! L.W. interactions wall->high vegetation areas for high veg balance
DEALLOCATE(LW%XLW_WB_TO_HV  ) ! L.W. interactions wall->high vegetation areas for high veg balance
DEALLOCATE(LW%XLW_WA_TO_S   ) ! L.W. interactions wall->sky for sky balance
DEALLOCATE(LW%XLW_WB_TO_S   ) ! L.W. interactions wall->sky for sky balance
!
DEALLOCATE(LW%XLW_R_TO_WA ) ! L.W. interactions road->wall for wall balance
DEALLOCATE(LW%XLW_R_TO_WB ) ! L.W. interactions road->wall for wall balance
DEALLOCATE(LW%XLW_R_TO_WIN  ) ! L.W. interactions road->win for win balance
DEALLOCATE(LW%XLW_R_TO_HV   ) ! L.W. interactions road->high vegetation for high veg balance
DEALLOCATE(LW%XLW_R_TO_S    ) ! L.W. interactions road->sky for sky balance
!
!
DEALLOCATE(LW%XLW_G_TO_WA ) ! L.W. interactions GARDEN areas->wall for wall balance
DEALLOCATE(LW%XLW_G_TO_WB ) ! L.W. interactions GARDEN areas->wall for wall balance
DEALLOCATE(LW%XLW_G_TO_WIN  ) ! L.W. interactions GARDEN areas->road for window balance
DEALLOCATE(LW%XLW_G_TO_HV   ) ! L.W. interactions GARDEN areas->high vegetation for high veg balance
DEALLOCATE(LW%XLW_G_TO_S    ) ! L.W. interactions GARDEN areas->sky for sky balance
!
DEALLOCATE(LW%XLW_S_TO_WA ) ! L.W. interactions sky->wall for wall balance
DEALLOCATE(LW%XLW_S_TO_WB ) ! L.W. interactions sky->wall for wall balance
DEALLOCATE(LW%XLW_S_TO_R  ) ! L.W. interactions sky->road for raod balance
DEALLOCATE(LW%XLW_S_TO_NR ) ! L.W. interactions sky->snow for snow balance 
DEALLOCATE(LW%XLW_S_TO_G  ) ! L.W. interactions sky->GARDEN areas for garden balance
DEALLOCATE(LW%XLW_S_TO_WIN ) ! L.W. interactions sky->win for window balance
DEALLOCATE(LW%XLW_S_TO_HV  ) ! L.W. interactions sky->high vegetation areas for high veg balance
!
DEALLOCATE(LW%XLW_WIN_TO_WA ) ! L.W. interactions win->wall for wall balance
DEALLOCATE(LW%XLW_WIN_TO_WB ) ! L.W. interactions win->wall for wall balance
DEALLOCATE(LW%XLW_WIN_TO_R  ) ! L.W. interactions win->road for road balance
DEALLOCATE(LW%XLW_WIN_TO_NR ) ! L.W. interactions win->GARDEN areas for snow balance
DEALLOCATE(LW%XLW_WIN_TO_G  ) ! L.W. interactions win->GARDEN areas for garden balance
DEALLOCATE(LW%XLW_WIN_TO_HV ) ! L.W. interactions win->high vegetation areas for high veg balance
DEALLOCATE(LW%XLW_WIN_TO_S  ) ! L.W. interactions win->sky for sky balance
!
!
DEALLOCATE(LW%XLW_NR_TO_WA  ) ! L.W. interactions snow(road)->wall for wall balance
DEALLOCATE(LW%XLW_NR_TO_WB  ) ! L.W. interactions snow(road)->wall for wall balance
DEALLOCATE(LW%XLW_NR_TO_WIN ) ! L.W. interactions snow(road)->WIN areas for window balance
DEALLOCATE(LW%XLW_NR_TO_HV  ) ! L.W. interactions snow(road)->high vegetation for high veg balance
DEALLOCATE(LW%XLW_NR_TO_S   ) ! L.W. interactions snow(road)->sky for sky balance
!
DEALLOCATE(LW%XLW_HV_TO_WA  ) ! L.W. interaction high veg -> wall for wall balance
DEALLOCATE(LW%XLW_HV_TO_WB  ) ! L.W. interaction high veg -> wall for wall balance
DEALLOCATE(LW%XLW_HV_TO_WIN ) ! L.W. interaction high veg -> wall for wall balance
DEALLOCATE(LW%XLW_HV_TO_G   ) ! L.W. interaction high veg -> garden for garden balance
DEALLOCATE(LW%XLW_HV_TO_R   ) ! L.W. interaction high veg -> road for road balance 
DEALLOCATE(LW%XLW_HV_TO_NR  ) ! L.W. interaction high veg -> snow for road balance 
DEALLOCATE(LW%XLW_HV_TO_S   ) ! L.W. interaction high veg -> sky for sky balance 
!

IF (LHOOK) CALL DR_HOOK('ALLOC_LW_COEF',1,ZHOOK_HANDLE)

END SUBROUTINE
