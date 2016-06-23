!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 newsrc 2006/05/23 16:20:13
!-----------------------------------------------------------------
!######################## 
MODULE MODI_SPAWN_SURF
!########################
!
INTERFACE
!
      SUBROUTINE SPAWN_SURF (HINIFILE, HINIFILEPGD, OSPAWN_SURF)
!
CHARACTER (LEN=*),     INTENT(IN) :: HINIFILE     ! Input file
CHARACTER (LEN=*),     INTENT(IN) :: HINIFILEPGD
LOGICAL,               INTENT(IN) :: OSPAWN_SURF  ! flag to spawn surface fields
!
END SUBROUTINE SPAWN_SURF
!
END INTERFACE
!
END MODULE MODI_SPAWN_SURF
!
!
!     #######################################################################
      SUBROUTINE SPAWN_SURF (HINIFILE, HINIFILEPGD, OSPAWN_SURF)
!     #######################################################################
!
!!****  *SPAWN_SURF * - subroutine to  call spawning of surface fields
!!
!!    PURPOSE
!!    -------
!!
!!
!!
!!**  METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!
!! 
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson       * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     01/2004  
!!  06/2016     (G.Delautier) phasage surfex 8
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_LUNIT,        ONLY : CPGDFILE, COUTFMFILE
USE MODD_LUNIT_n,      ONLY : CLUOUT
USE MODD_PARAM_n,      ONLY : CSURF
USE MODD_NESTING,      ONLY : CMY_NAME, CDAD_NAME
USE MODD_CONF,         ONLY : NVERB
USE MODD_GRID_n,       ONLY : XZS
USE MODD_TIME_n,       ONLY : TDTCUR
USE MODD_IO_SURF_MNH,  ONLY : COUTFILE
!
USE MODI_WRITE_HGRIDn
USE MODI_MNHPUT_ZS_n
!
USE MODE_ll
USE MODE_FMWRIT
USE MODE_FM
USE MODE_IO_ll
 !JUAN REALZ
USE MODE_MODELN_HANDLER
 !JUAN REALZ
USE MODI_ZOOM_PGD_SURF_ATM
USE MODI_WRITE_PGD_SURF_ATM_N
USE MODI_WRITE_SURF_ATM_N
USE MODI_INIT_PGD_SURF_ATM
USE MODI_PREP_SURF_ATM
USE MODI_WRITE_DIAG_SURF_ATM_N
USE MODD_MNH_SURFEX_n
!
#ifdef MNH_NCWRIT
USE MODN_NCOUT
USE MODE_UTIL
#endif
!
IMPLICIT NONE
!
!*       0.1.1  Declarations of global variables not declared in the modules :
!
!*       0.1.2  Declarations of dummy arguments :
!
CHARACTER (LEN=*),     INTENT(IN) :: HINIFILE     ! Input file
CHARACTER (LEN=*),     INTENT(IN) :: HINIFILEPGD
LOGICAL,               INTENT(IN) :: OSPAWN_SURF  ! flag to spawn surface fields
!
!*       0.1.3  Declarations of local variables :
!
INTEGER :: IRESP    ! Return codes in FM routines
INTEGER :: ILUOUT   ! Logical unit number for the output listing 
INTEGER :: IINFO_ll
!  
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       7.      Surface variables :
!
IF (CSURF=='EXTE') THEN
  IF (OSPAWN_SURF) THEN
    CPGDFILE   = CMY_NAME(2)
    COUTFMFILE = CMY_NAME(2)
    COUTFILE   = CMY_NAME(2)
    !* spawn PGD fields
    CALL GOTO_SURFEX(1)
    !JUAN REALZ
    !CALL GO_TOMODEL_ll(1,IINFO_ll)
    !CALL GOTO_MODEL(1)
    !JUAN REALZ
    CALL ZOOM_PGD_SURF_ATM(YSURF_CUR,'MESONH',HINIFILEPGD,'MESONH',CPGDFILE,'MESONH')
    CALL MNHPUT_ZS_n
    !* writing of physiographic fields in the file
#ifdef MNH_NCWRIT
    NC_WRITE=LNETCDF
    NC_FILE='pgd'
    CALL WRITE_PGD_SURF_ATM_n(YSURF_CUR,'MESONH')
    IF ( LNETCDF ) THEN
      DEF_NC=.FALSE.
      CALL WRITE_PGD_SURF_ATM_n(YSURF_CUR,'MESONH')
      DEF_NC=.TRUE.
      NC_WRITE = .FALSE.
   END IF
#else 
    CALL WRITE_PGD_SURF_ATM_n(YSURF_CUR,'MESONH')
#endif
    !* rereading of physiographic fields and definition of prognostic fields
    CALL INIT_PGD_SURF_ATM(YSURF_CUR,'MESONH','PRE',HINIFILE,'MESONH',      &
                           TDTCUR%TDATE%YEAR, TDTCUR%TDATE%MONTH, &
                           TDTCUR%TDATE%DAY, TDTCUR%TIME          )
    CALL PREP_SURF_ATM(YSURF_CUR,'MESONH',HINIFILE,'MESONH',HINIFILEPGD,'MESONH')
    !* writing of all surface fields
#ifdef MNH_NCWRIT
    NC_WRITE=LNETCDF
    NC_FILE='sf2'
    CALL WRITE_SURF_ATM_n(YSURF_CUR,'MESONH','ALL',.FALSE.)
    CALL WRITE_DIAG_SURF_ATM_n(YSURF_CUR,'MESONH','ALL')
    IF ( LNETCDF ) THEN
      DEF_NC=.FALSE.
      CALL WRITE_SURF_ATM_n(YSURF_CUR,'MESONH','ALL',.FALSE.)
      CALL WRITE_DIAG_SURF_ATM_n(YSURF_CUR,'MESONH','ALL')
      DEF_NC=.TRUE.
      NC_WRITE = .FALSE.
   END IF
#else
    CALL WRITE_SURF_ATM_n(YSURF_CUR,'MESONH','ALL',.FALSE.)
    CALL WRITE_DIAG_SURF_ATM_n(YSURF_CUR,'MESONH','ALL')
#endif    
    !
  ELSE
    CSURF='EXRM'
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SPAWN_SURF
