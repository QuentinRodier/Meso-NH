!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_TEB_GARDEN
!     ##################
!
!!****  *MODN_PREP_TEB_GARDEN* - declaration of namelist NAM_PREP_TEB_GARDEN
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist
!       NAM_PREP_TEB_GARDEN
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
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004                   
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PREP_TEB_GARDEN,  ONLY : CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,        &
                                     CFILE_HUG, CTYPE_HUG,                           &
                                     CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP, &
                                     XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                &
                                     XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,             &
                                     CFILE_TG, CTYPE_TG,                             &
                                     CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,    &
                                     XTG_SURF, XTG_ROOT, XTG_DEEP   

!
IMPLICIT NONE
!
NAMELIST/NAM_PREP_TEB_GARDEN/CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,        &
                                CFILE_HUG, CTYPE_HUG,                           &
                                CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP, &
                                XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                &
                                XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,             &
                                CFILE_TG, CTYPE_TG,                              &
                                CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,    &
                                XTG_SURF, XTG_ROOT, XTG_DEEP   
!
END MODULE MODN_PREP_TEB_GARDEN
