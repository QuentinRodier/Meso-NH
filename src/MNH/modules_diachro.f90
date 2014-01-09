!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 diachro 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #########################
      MODULE MODI_MENU_DIACHRO
!     #########################
!
INTERFACE
!
SUBROUTINE MENU_DIACHRO(HFILEDIA,HLUOUTDIA,HGROUP)
CHARACTER(LEN=*) :: HGROUP
CHARACTER(LEN=*) :: HFILEDIA,HLUOUTDIA
END SUBROUTINE MENU_DIACHRO
!
END INTERFACE
!
END MODULE MODI_MENU_DIACHRO
!-----------------------------------------------------------------
!     #####################################
      MODULE MODI_WRITE_LFIFMN_FORDIACHRO_n
!     #####################################
!
INTERFACE
!
SUBROUTINE WRITE_LFIFMN_FORDIACHRO_n(HFMFILE)
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
END SUBROUTINE WRITE_LFIFMN_FORDIACHRO_n
!
END INTERFACE
!
END MODULE MODI_WRITE_LFIFMN_FORDIACHRO_n
!     #########################
      MODULE MODI_WRITE_DIACHRO
!     #########################
!
INTERFACE
!
SUBROUTINE WRITE_DIACHRO(HFILEDIA,HLUOUTDIA,HGROUP,HTYPE,KGRID, &
                   PDATIME,PVAR,PTRAJT,HTITRE,HUNITE,HCOMMENT,  &
                   OICP, OJCP, OKCP, KIL, KIH, KJL, KJH, KKL, KKH, &
			       PTRAJX,PTRAJY,PTRAJZ,PMASK)
CHARACTER(LEN=*)              :: HFILEDIA,HLUOUTDIA
CHARACTER(LEN=*)              :: HGROUP, HTYPE
CHARACTER(LEN=*),DIMENSION(:) :: HTITRE, HUNITE, HCOMMENT

INTEGER,DIMENSION(:)  :: KGRID
INTEGER,OPTIONAL      :: KIL, KIH
INTEGER,OPTIONAL      :: KJL, KJH
INTEGER,OPTIONAL      :: KKL, KKH
LOGICAL,OPTIONAL      :: OICP, OJCP, OKCP
REAL,DIMENSION(:,:,:,:,:,:),OPTIONAL  :: PMASK
REAL,DIMENSION(:,:)             :: PDATIME
REAL,DIMENSION(:,:,:,:,:,:)     :: PVAR
REAL,DIMENSION(:,:)             :: PTRAJT
REAL,DIMENSION(:,:,:),OPTIONAL  :: PTRAJX
REAL,DIMENSION(:,:,:),OPTIONAL  :: PTRAJY
REAL,DIMENSION(:,:,:),OPTIONAL  :: PTRAJZ

END SUBROUTINE WRITE_DIACHRO
!
END INTERFACE
!
END MODULE MODI_WRITE_DIACHRO
