!     #########
    SUBROUTINE ALLOC_DIAG_TEB_GREENROOF(KLU,KLAYER_GR,KSW)
!   ##########################################################################
!
USE MODD_TEB_GREENROOF_n
USE MODD_DIAG_TEB_GREENROOF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KLAYER_GR
INTEGER, INTENT(IN) :: KSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Diagnostic variables:
!
IF (LHOOK) CALL DR_HOOK('ALLOC_DIAG_TEB_GREENROOF',0,ZHOOK_HANDLE)
ALLOCATE(XRI                     (KLU                     )) 
ALLOCATE(XCD                     (KLU                     )) 
ALLOCATE(XCH                     (KLU                     )) 
ALLOCATE(XRN                     (KLU                     )) 
ALLOCATE(XH                      (KLU                     )) 
ALLOCATE(XGFLUX                  (KLU                     )) 
ALLOCATE(XQS                     (KLU                     )) 
!
ALLOCATE(XLEI                    (KLU                     )) 
ALLOCATE(XLEG                    (KLU                     )) 
ALLOCATE(XLEGI                   (KLU                     )) 
ALLOCATE(XLEV                    (KLU                     )) 
ALLOCATE(XLES                    (KLU                     )) 
ALLOCATE(XLER                    (KLU                     )) 
ALLOCATE(XLETR                   (KLU                     )) 
ALLOCATE(XEVAP                   (KLU                     )) 
ALLOCATE(XDRAIN                  (KLU                     )) 
ALLOCATE(XRUNOFF                 (KLU                     )) 
ALLOCATE(XHORT                   (KLU                     )) 
ALLOCATE(XDRIP                   (KLU                     )) 
ALLOCATE(XRRVEG                  (KLU                     )) 
ALLOCATE(XMELT                   (KLU                     )) 
!
ALLOCATE(XCG                     (KLU                     )) 
ALLOCATE(XC1                     (KLU                     )) 
ALLOCATE(XC2                     (KLU                     )) 
ALLOCATE(XWGEQ                   (KLU                     )) 
ALLOCATE(XCT                     (KLU                     )) 
ALLOCATE(XRS                     (KLU                     )) 
ALLOCATE(XCDN                    (KLU                     )) 
ALLOCATE(XHU                     (KLU                     )) 
ALLOCATE(XHUG                    (KLU                     )) 
ALLOCATE(XRESTORE                (KLU                     )) 
ALLOCATE(XUSTAR                  (KLU                     )) 
ALLOCATE(XIACAN                  (KLU,3                   )) 
!
ALLOCATE(XSNOWTEMP               (KLU,TSNOW%NLAYER        )) 
ALLOCATE(XSNOWLIQ                (KLU,TSNOW%NLAYER        )) 
ALLOCATE(XSNOWDZ                 (KLU,TSNOW%NLAYER        )) 
ALLOCATE(XSNOWHMASS              (KLU                     )) 
ALLOCATE(XMELTADV                (KLU                     )) 
!
ALLOCATE(XHV                     (KLU                     ))
ALLOCATE(XALBT                   (KLU                     )) 
ALLOCATE(XEMIST                  (KLU                     )) 
IF (LHOOK) CALL DR_HOOK('ALLOC_DIAG_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE ALLOC_DIAG_TEB_GREENROOF
