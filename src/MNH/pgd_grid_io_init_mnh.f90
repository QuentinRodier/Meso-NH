!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:18
!-----------------------------------------------------------------
!     ###############################
      SUBROUTINE PGD_GRID_IO_INIT_MNH
!     ###############################
!!
!!    PURPOSE
!!    -------
!!
!!    Initializes parallel routines for further I/O
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original      01/2004
!!    10/10/2011  J.Escobar call INI_PARAZ_ll
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_ll
USE MODE_FM
USE MODD_PARAMETERS, ONLY : JPHEXT, JPVEXT, JPMODELMAX
USE MODD_CONF,       ONLY : CPROGRAM, L1D, L2D, LPACK
!
!JUANZ
USE MODE_SPLITTINGZ_ll
!JUANZ
!
USE MODI_GET_SURF_GRID_DIM_N
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IINFO_ll ! return code of // routines
INTEGER :: IIMAX    ! number of points in X direction
INTEGER :: IJMAX    ! number of points in Y direction
INTEGER :: ILUOUT   ! output listing logical unit
!
LOGICAL :: GRECT           ! true when grid is rectangular
CHARACTER(LEN=10) :: YGRID ! grid type
!
!------------------------------------------------------------------------------
!
CALL GET_SURF_GRID_DIM_n(YGRID,GRECT,IIMAX,IJMAX)
!
!
IF (YGRID/='CONF PROJ ' .AND. YGRID/='CARTESIAN') THEN
  CALL GET_LUOUT('MESONH',ILUOUT)
  WRITE(ILUOUT,*) "Error, grid type GRID=",YGRID, &
                  " is not supported by MESONH"
END IF
!------------------------------------------------------------------------------
!
IF (CPROGRAM=='IDEAL ' .OR. CPROGRAM=='SPAWN ') RETURN
!
L1D=(IIMAX==1).AND.(IJMAX==1)
L2D=(IIMAX/=1).AND.(IJMAX==1)
LPACK=L1D.OR.L2D
CALL SET_FMPACK_ll(L1D,L2D,LPACK)
CALL SET_JP_ll(JPMODELMAX,JPHEXT,JPVEXT,JPHEXT)
CALL SET_DAD0_ll()
CALL SET_DIM_ll(IIMAX, IJMAX, 1)
CALL SET_LBX_ll('OPEN',1)
CALL SET_LBY_ll('OPEN', 1)
CALL SET_XRATIO_ll(1, 1)
CALL SET_YRATIO_ll(1, 1)
CALL SET_XOR_ll(1, 1)
CALL SET_XEND_ll(IIMAX+2*JPHEXT, 1)
CALL SET_YOR_ll(1, 1)
CALL SET_YEND_ll(IJMAX+2*JPHEXT, 1)
CALL SET_DAD_ll(0, 1)
!JUANZ CALL INI_PARA_ll(IINFO_ll)
CALL INI_PARAZ_ll(IINFO_ll)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_GRID_IO_INIT_MNH
