!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
  SUBROUTINE GET_NB_PROCIO_WRITE_MNH( KNB_PROCIO, KRESP )
!
!!****  *GET_NB_PROCIO_WRITE_MNH* - gets the number of processes used for Output of file MODD_IO_SURF_MNH::COUTFILE
!!                        
!!
!!    PURPOSE
!!    -------
!!      call GET_NB_PROCIO_WRITE_MNH from SURFEX to get the number of processes used 
!!      for Output of file MODD_IO_SURF_MNH::COUTFILE in MESO-NH (defined by user in namelist)
!!
!!**  METHOD
!!    ------
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
!!	M. Moge   *LA - UPS*  08/01/2016	
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
USE MODE_FD_ll,        ONLY : GETFD,JPFINL,FD_ll
USE MODD_IO_SURF_MNH,  ONLY : COUT, COUTFILE
!
IMPLICIT NONE
!
!*      0.    DECLARATIONS
!             ------------
!
!*      0.1   Declarations of arguments
!
!CHARACTER(LEN=*), INTENT(IN)  :: HFILEM  ! FM-file name
INTEGER,          INTENT(OUT) :: KNB_PROCIO    ! number of processes used for IO
INTEGER,          INTENT(OUT) :: KRESP   ! return-code 
!
!*      0.2   Declarations of local variables
!
!----------------------------------------------------------------
CHARACTER(LEN=JPFINL)        :: YFNLFI
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
INTEGER                      :: ILUPRI
!
!*      1. get the number of processes used for IO
!
IRESP = 0
YFNLFI=TRIM(ADJUSTL(COUTFILE))//'.lfi'
!
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  KNB_PROCIO = TZFD%nb_procio
ELSE
  IRESP = -61
END IF
!----------------------------------------------------------------
IF (IRESP.NE.0) THEN
  CALL FMLOOK_ll(COUT,COUT,ILUPRI,IRESP)
  WRITE (ILUPRI,*) ' exit from GET_NB_PROCIO_WRITE_MNH with RESP:',IRESP
  WRITE (ILUPRI,*) '   | COUTFILE = ',COUTFILE
END IF
KRESP = IRESP
!
  END SUBROUTINE GET_NB_PROCIO_WRITE_MNH