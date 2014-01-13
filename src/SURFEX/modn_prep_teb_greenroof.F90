!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_TEB_GREENROOF
!     ##################
!
!!****  *MODN_PREP_TEB_GREENROOF* - declaration of namelist NAM_PREP_TEB_GREENROOF
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify the namelist
!       NAM_PREP_TEB_GREENROOF
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!    A. Lemonsu & C. de Munck    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2011                   
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PREP_TEB_GREENROOF,  ONLY : CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,     &
                                     CFILE_HUG, CTYPE_HUG,        &
                                     CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP, &
                                     XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                 &
                                     XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,              &
                                     CFILE_TG, CTYPE_TG,                             &
                                     CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,    &
                                     XTG_SURF, XTG_ROOT, XTG_DEEP   

!
IMPLICIT NONE
!
NAMELIST/NAM_PREP_TEB_GREENROOF/CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,     &
                                CFILE_HUG, CTYPE_HUG,        &
                                CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP, &
                                XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                &
                                XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,             &
                                CFILE_TG, CTYPE_TG,                              &
                                CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,    &
                                XTG_SURF, XTG_ROOT, XTG_DEEP   
!
END MODULE MODN_PREP_TEB_GREENROOF
