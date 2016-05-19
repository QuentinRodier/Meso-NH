!     #########
      SUBROUTINE DEFAULT_PREP_TEB_GREENROOF
!     ###########################
!
!!****  *DEFAULT_PREP_TEB_GREENROOF* - routine to set default values for the configuration for ISBA fields preparation
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!    Based on "default_prep_teb_greenroof"
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
!!    A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PREP_TEB_GREENROOF, ONLY : CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,       &
                                    CFILE_HUG, CTYPE_HUG,          &
                                    CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP,   &
                                    XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                  &
                                    XHUGI_SURF,XHUGI_ROOT, XHUGI_DEEP,                &
                                    CFILE_TG, CTYPE_TG,                               &
                                    CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,      &
                                    XTG_SURF, XTG_ROOT, XTG_DEEP,                     &
                                    XWR_DEF  

USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_TEB_GREENROOF',0,ZHOOK_HANDLE)
CFILE_ISBA      = '                          '
CTYPE           = 'GRIB  '
CFILEPGD_ISBA   = '                          '
CTYPEPGD        = 'GRIB  '
CFILE_HUG       = '                          '
CTYPE_HUG       = '      '
CFILE_TG        = '                          '
CTYPE_TG        = '      '
!
CFILE_HUG_SURF = '                          '
CFILE_HUG_ROOT = '                          '
CFILE_HUG_DEEP = '                          '
CFILE_TG_SURF  = '                          '
CFILE_TG_ROOT  = '                          '
CFILE_TG_DEEP  = '                          '
!
XHUG_SURF = XUNDEF
XHUG_ROOT = XUNDEF
XHUG_DEEP = XUNDEF
XHUGI_SURF= 0.
XHUGI_ROOT= 0.
XHUGI_DEEP= 0.
XTG_SURF  = XUNDEF
XTG_ROOT  = XUNDEF
XTG_DEEP  = XUNDEF
!
XWR_DEF   = 0.
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_PREP_TEB_GREENROOF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_PREP_TEB_GREENROOF
