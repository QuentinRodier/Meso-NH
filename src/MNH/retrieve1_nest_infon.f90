!MNH_LIC Copyright 1996-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ################################
      MODULE MODI_RETRIEVE1_NEST_INFO_n
!     ################################
!
INTERFACE 
!
      SUBROUTINE RETRIEVE1_NEST_INFO_n(KDAD,KMI,KXOR,KYOR,KXSIZE,KYSIZE,KDXRATIO,KDYRATIO)
!
INTEGER,INTENT(IN)  :: KDAD     ! index of father model 
INTEGER,INTENT(IN)  :: KMI      ! index of son model 
INTEGER,INTENT(OUT) :: KXOR     ! position of pgd model origine points
INTEGER,INTENT(OUT) :: KYOR     ! according to CINIFILE domain
INTEGER,INTENT(OUT) :: KXSIZE   ! number of grid meshes in CINIFILE to be
INTEGER,INTENT(OUT) :: KYSIZE   ! covered by the pgd domain
INTEGER,INTENT(OUT) :: KDXRATIO ! resolution ratio between CINIFILE grid
INTEGER,INTENT(OUT) :: KDYRATIO ! and pgd grid
!
END SUBROUTINE RETRIEVE1_NEST_INFO_n
!
END INTERFACE
!
END MODULE MODI_RETRIEVE1_NEST_INFO_n
!
!
!
!     ################################################################################
      SUBROUTINE RETRIEVE1_NEST_INFO_n(KDAD,KMI,KXOR,KYOR,KXSIZE,KYSIZE,KDXRATIO,KDYRATIO)
!     ################################################################################
!
!!****  *RETRIEVE1_NEST_INFO_n* - routine to test coherence between grid of model 1
!!                            and grid given for spawned grid of model _n.
!!                            retrieve the namelist information for SPAWNING
!!
!!    PURPOSE
!!    -------
!!
!!    WARNING: this routine uses MODD_PGDGRID and MODD_PGDDIM as working modules
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      Module MODD_PGDGRID : contains domain definition
!!        XPGDLATOR
!!        XPGDLONOR
!!        XPGDXHAT
!!        XPGDYHAT
!!      Module MODD_PGDDIM : contains domain size
!!        NPGDIMAX
!!        NPGDJMAX
!!        XLATORI
!!        XLONORI
!!      Module MODD_PGDGRID_n :
!!        XXHAT
!!        XYHAT 
!!      Module MODD_PGDDIM_n :
!!        NIMAX, NJMAX
!!      Module MODD_PARAMETERS :
!!        JPHEXT
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        26/09/96
!!      Modification    30/07/97 (Masson) group MODI_RETRIEVE2_NEST_INFOn
!!      Modification    04/05/00 (Jabouille) test on CPROGRAM to fill working modules
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_GRID, ONLY: XLONORI,XLATORI
USE MODD_GRID_n
USE MODD_DIM_n
USE MODD_PGDGRID
USE MODD_PGDDIM
USE MODD_CONF
!
use mode_msg
USE MODE_MODELN_HANDLER 
!
USE MODI_RETRIEVE2_NEST_INFO_n
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
INTEGER,INTENT(IN)  :: KDAD     ! index of father model 
INTEGER,INTENT(IN)  :: KMI      ! index of son model 
INTEGER,INTENT(OUT) :: KXOR     ! position of pgd model origine points
INTEGER,INTENT(OUT) :: KYOR     ! according to CINIFILE domain
INTEGER,INTENT(OUT) :: KXSIZE   ! number of grid meshes in CINIFILE to be
INTEGER,INTENT(OUT) :: KYSIZE   ! covered by the pgd domain
INTEGER,INTENT(OUT) :: KDXRATIO ! resolution ratio between CINIFILE grid
INTEGER,INTENT(OUT) :: KDYRATIO ! and pgd grid

INTEGER :: IMI
!
!
!*       0.2   declarations of local variables
!
!-------------------------------------------------------------------------------
!
!
IF ( KMI <= KDAD ) call Print_msg( NVERB_FATAL, 'GEN', 'RETRIEVE1_NEST_INFO_n', 'KMI<=KDAD' )
!
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(KDAD)   
!
! Current model is now KDAD
!
IF ( CPROGRAM /= 'SPAWN ' ) THEN
  XPGDLATOR=XLATORI
  XPGDLONOR=XLONORI
  NPGDIMAX =NIMAX
  NPGDJMAX =NJMAX
  IF (ALLOCATED(XPGDXHAT)) DEALLOCATE(XPGDXHAT)
  IF (ALLOCATED(XPGDYHAT)) DEALLOCATE(XPGDYHAT)
  ALLOCATE(XPGDXHAT(SIZE(XXHAT)))
  ALLOCATE(XPGDYHAT(SIZE(XYHAT)))
  XPGDXHAT(:)=XXHAT(:)
  XPGDYHAT(:)=XYHAT(:)
ELSE
  NPGDIMAX =NIMAX
  NPGDJMAX =NJMAX
ENDIF
!
CALL RETRIEVE2_NEST_INFO_n(KMI,KDAD,KXOR,KYOR,KXSIZE,KYSIZE,KDXRATIO,KDYRATIO)
!
IF ( CPROGRAM /= 'SPAWN ' ) THEN
  XXHAT(:)=XPGDXHAT(:)
  XYHAT(:)=XPGDYHAT(:)
ENDIF
!-------------------------------------------------------------------------------
!
CALL GOTO_MODEL(IMI)
!
END SUBROUTINE RETRIEVE1_NEST_INFO_n
