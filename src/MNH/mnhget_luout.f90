!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 surfex 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #############################
      MODULE MODI_MNHGET_LUOUT
!     #############################
INTERFACE
      SUBROUTINE MNHGET_LUOUT(HPROGRAM,KLUOUT)
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling GROUND
INTEGER,           INTENT(OUT) :: KLUOUT   ! Logical unit of output listing
!
END SUBROUTINE MNHGET_LUOUT
!
END INTERFACE
END MODULE MODI_MNHGET_LUOUT
!
!     #######################################################
      SUBROUTINE MNHGET_LUOUT(HPROGRAM,KLUOUT)
!     #######################################################
!
!!****  *MNHGET_LUOUT* - get output listing logical unit
!!
!!    PURPOSE
!!    -------
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
USE MODD_CONF, ONLY : CPROGRAM
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling GROUND
INTEGER,           INTENT(OUT) :: KLUOUT   ! Logical unit of output listing
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    ! at the open of the file in LFI  routines 
INTEGER           :: IMI            ! model index
!
CHARACTER(LEN=16) :: YLUOUT         ! output listing file name
!-------------------------------------------------------------------------------
!
!
SELECT CASE (CPROGRAM)
  CASE ('REAL  ','PGD   ','NESPGD')
    YLUOUT = 'OUTPUT_LISTING0'
  CASE ('IDEAL ')
    YLUOUT = 'OUTPUT_LISTING1'
  CASE ('MESONH','DIAG  ','SPAWN ')
    CALL GET_MODEL_NUMBER_ll  (IMI)
    WRITE(YLUOUT,FMT='(A14,I1,A1)')  'OUTPUT_LISTING',IMI,' '
  CASE DEFAULT
    YLUOUT = 'OUTPUT_LISTING0'
END SELECT
!
CALL FMLOOK_ll(YLUOUT,YLUOUT,KLUOUT,IRESP)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHGET_LUOUT
