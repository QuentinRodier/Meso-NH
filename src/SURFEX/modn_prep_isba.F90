!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODN_PREP_ISBA
!     ##################
!
!!****  *MODN_PREP_ISBA* - declaration of namelist NAM_PREP_ISBA
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_PREP_ISBA
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
USE MODD_PREP_ISBA,  ONLY : CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,       &
                              CFILE_HUG, CTYPE_HUG,                           &
                              CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP, &
                              XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                 &
                              XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,              &
                              CFILE_TG, CTYPE_TG,                             &
                              CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,    &
                              XTG_SURF, XTG_ROOT, XTG_DEEP  

!
IMPLICIT NONE
INTEGER           :: NYEAR        ! YEAR for surface
INTEGER           :: NMONTH       ! MONTH for surface
INTEGER           :: NDAY         ! DAY for surface
REAL              :: XTIME        ! TIME for surface
LOGICAL           :: LISBA_CANOPY !flag to use air layers inside the canopy
!
NAMELIST/NAM_PREP_ISBA/CFILE_ISBA, CTYPE, CFILEPGD_ISBA, CTYPEPGD,       &
                         CFILE_HUG, CTYPE_HUG,                           &
                         CFILE_HUG_SURF, CFILE_HUG_ROOT, CFILE_HUG_DEEP, &
                         XHUG_SURF, XHUG_ROOT, XHUG_DEEP,                &
                         XHUGI_SURF, XHUGI_ROOT, XHUGI_DEEP,             &
                         CFILE_TG, CTYPE_TG,                              &
                         CFILE_TG_SURF, CFILE_TG_ROOT, CFILE_TG_DEEP,    &
                         XTG_SURF, XTG_ROOT, XTG_DEEP,                   &
                         NYEAR, NMONTH, NDAY, XTIME, LISBA_CANOPY  
!
END MODULE MODN_PREP_ISBA
