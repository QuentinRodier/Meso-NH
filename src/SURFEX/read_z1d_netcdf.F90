!SFX_LIC Copyright 2014-2019 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_Z1D_NETCDF
!     ##############################################################
!
!!**** *READ_Z1D_NETCDF* reads the vertical grid in a netcdf file
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    C. Lebeaupin Brossier Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    11/2014
!!      initialisation of NOCKMAX,XZHOC
!  P. Wautelet 19/09/2019: correct support of 64bit integers (MNH_INT=8)
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
use modd_netcdf_sfx, only : IDCDF_KIND
USE MODD_OCEAN_GRID
USE MODD_SURF_PAR, ONLY : NFILENAMELGTMAX, NUNDEF
USE MODD_PREP_SEAFLUX, ONLY : CFILE_SEAFLX,CTYPE_SEAFLX
USE MODE_READ_NETCDF_MERCATOR
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
CHARACTER (LEN=NFILENAMELGTMAX) :: YFILENAME
CHARACTER (LEN=28)  :: YNCVARNAME
INTEGER(kind=IDCDF_KIND) :: JDIMENSION
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
                                           ! and ZLON arrays
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_Z1D_NETCDF',0,ZHOOK_HANDLE)
!
NOCKMAX=-NUNDEF
!
!*    1.      Read the netcdf arrays dimensions
!             ---------------------------------
IF (CTYPE_SEAFLX=="NETCDF") THEN
  YFILENAME=TRIM(CFILE_SEAFLX)
  YNCVARNAME="depth"
  CALL READ_DIM_CDF(YFILENAME,YNCVARNAME,JDIMENSION)
  NOCKMAX=JDIMENSION-1
  ALLOCATE(XZHOC(0:NOCKMAX))
  XZHOC(0)=0.
!
!*    2.      Read the array in the netcdf file
!             ---------------------------------
  CALL READ_Z1D_CDF(YFILENAME,YNCVARNAME,XZHOC(0:NOCKMAX))
  IF (XZHOC(1)>0) XZHOC(:)=-XZHOC(:)
!  WRITE(0,*) 'Oceanic vertical grid readed in netcdf file'
!  WRITE(0,*) 'Number of level',NOCKMAX+1
!  WRITE(0,*) 'Depth of vertical level',XZHOC(:)
!
!----------------------------------------------------------------------------
ELSE
  WRITE(*,*) 'ERROR IN READ_Z1D_NETCF: ', YFILENAME, ' HAS NOT A NETCDF TYPE'
  WRITE(*,*) 'CHECK CTYPE_SEAFLX IN NAM_PREP_SEAFLUX'
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_Z1D_NETCDF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_Z1D_NETCDF
