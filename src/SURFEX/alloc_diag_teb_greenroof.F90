!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOC_DIAG_TEB_GREENROOF (DGGR, TGR, &
                                         KLU,KLAYER_GR,KSW)
!   ##########################################################################
!
!
!
!
USE MODD_DIAG_TEB_GREENROOF_n, ONLY : DIAG_TEB_GREENROOF_t
USE MODD_TEB_GREENROOF_n, ONLY : TEB_GREENROOF_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(DIAG_TEB_GREENROOF_t), INTENT(INOUT) :: DGGR
TYPE(TEB_GREENROOF_t), INTENT(INOUT) :: TGR
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
ALLOCATE(DGGR%XRI                     (KLU                     )) 
ALLOCATE(DGGR%XCD                     (KLU                     )) 
ALLOCATE(DGGR%XCH                     (KLU                     )) 
ALLOCATE(DGGR%XRN                     (KLU                     )) 
ALLOCATE(DGGR%XH                      (KLU                     )) 
ALLOCATE(DGGR%XGFLUX                  (KLU                     )) 
ALLOCATE(DGGR%XQS                     (KLU                     )) 
!
ALLOCATE(DGGR%XLEI                    (KLU                     )) 
ALLOCATE(DGGR%XLEG                    (KLU                     )) 
ALLOCATE(DGGR%XLEGI                   (KLU                     )) 
ALLOCATE(DGGR%XLEV                    (KLU                     )) 
ALLOCATE(DGGR%XLES                    (KLU                     )) 
ALLOCATE(DGGR%XLER                    (KLU                     )) 
ALLOCATE(DGGR%XLETR                   (KLU                     )) 
ALLOCATE(DGGR%XEVAP                   (KLU                     )) 
ALLOCATE(DGGR%XDRAIN                  (KLU                     )) 
ALLOCATE(DGGR%XRUNOFF                 (KLU                     )) 
ALLOCATE(DGGR%XHORT                   (KLU                     )) 
ALLOCATE(DGGR%XDRIP                   (KLU                     )) 
ALLOCATE(DGGR%XRRVEG                  (KLU                     )) 
ALLOCATE(DGGR%XMELT                   (KLU                     )) 
!
ALLOCATE(DGGR%XCG                     (KLU                     )) 
ALLOCATE(DGGR%XC1                     (KLU                     )) 
ALLOCATE(DGGR%XC2                     (KLU                     )) 
ALLOCATE(DGGR%XWGEQ                   (KLU                     )) 
ALLOCATE(DGGR%XCT                     (KLU                     )) 
ALLOCATE(DGGR%XRS                     (KLU                     )) 
ALLOCATE(DGGR%XCDN                    (KLU                     )) 
ALLOCATE(DGGR%XHU                     (KLU                     )) 
ALLOCATE(DGGR%XHUG                    (KLU                     )) 
ALLOCATE(DGGR%XRESTORE                (KLU                     )) 
ALLOCATE(DGGR%XUSTAR                  (KLU                     )) 
ALLOCATE(DGGR%XIACAN                  (KLU,3                   )) 
!
ALLOCATE(DGGR%XSNOWTEMP               (KLU,TGR%CUR%TSNOW%NLAYER        )) 
ALLOCATE(DGGR%XSNOWLIQ                (KLU,TGR%CUR%TSNOW%NLAYER        )) 
ALLOCATE(DGGR%XSNOWDZ                 (KLU,TGR%CUR%TSNOW%NLAYER        )) 
ALLOCATE(DGGR%XSNOWHMASS              (KLU                     )) 
ALLOCATE(DGGR%XMELTADV                (KLU                     )) 
!
ALLOCATE(DGGR%XHV                     (KLU                     ))
ALLOCATE(DGGR%XALBT                   (KLU                     )) 
ALLOCATE(DGGR%XEMIST                  (KLU                     )) 
IF (LHOOK) CALL DR_HOOK('ALLOC_DIAG_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE ALLOC_DIAG_TEB_GREENROOF
