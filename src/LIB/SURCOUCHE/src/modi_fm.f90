!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------

MODULE MODI_FM_ll
!
INTERFACE 
!
SUBROUTINE SET_FMPACK_ll(O1D,O2D,OPACK)
LOGICAL, INTENT(IN) :: O1D,O2D,OPACK
END SUBROUTINE SET_FMPACK_ll

SUBROUTINE FMATTR_ll(HFILEM,HFIPRI,KNUMBR,KRESP)
CHARACTER(LEN=*), INTENT(IN)  :: HFILEM
CHARACTER(LEN=*), INTENT(IN)  :: HFIPRI
INTEGER,          INTENT(OUT) :: KNUMBR
INTEGER,          INTENT(OUT) :: KRESP
END SUBROUTINE FMATTR_ll

SUBROUTINE FMLOOK_ll(HFILEM,HFIPRI,KNUMBR,KRESP)
CHARACTER(LEN=*), INTENT(IN)  :: HFILEM
CHARACTER(LEN=*), INTENT(IN)  :: HFIPRI
INTEGER,          INTENT(OUT) :: KNUMBR
INTEGER,          INTENT(OUT) :: KRESP
END SUBROUTINE FMLOOK_ll

SUBROUTINE FMOPEN_ll(HFILEM,HACTION,HFIPRI,KNPRAR,KFTYPE,KVERB,KNINAR&
     & ,KRESP,OPARALLELIO)
CHARACTER(LEN=*),INTENT(IN) ::HFILEM  ! name of the file.
CHARACTER(LEN=*),INTENT(IN) ::HACTION ! Action upon the file
                                      ! 'READ' or 'WRITE'
CHARACTER(LEN=*),INTENT(IN) ::HFIPRI  ! file for prints in FM.
INTEGER,         INTENT(IN) ::KNPRAR  ! number of predicted
                                      ! articles  (not vital).
INTEGER,         INTENT(IN) ::KFTYPE  ! type of FM-file.
INTEGER,         INTENT(IN) ::KVERB   ! level of verbose.
INTEGER,         INTENT(OUT)::KNINAR  ! number of articles initially present in the file.
INTEGER,         INTENT(OUT)::KRESP   ! return-code if a problem araised.
LOGICAL,         INTENT(IN),  OPTIONAL :: OPARALLELIO
END SUBROUTINE FMOPEN_ll

SUBROUTINE FMCLOS_ll(HFILEM,HSTATU,HFIPRI,KRESP,OPARALLELIO)
CHARACTER(LEN=*),     INTENT(IN) ::HFILEM  ! file name
CHARACTER(LEN=*),     INTENT(IN) ::HSTATU  ! status for the closed file
CHARACTER(LEN=*),     INTENT(IN) ::HFIPRI  ! file for prints in FM
INTEGER,              INTENT(OUT)::KRESP   ! return-code if problems araised
LOGICAL,              INTENT(IN),  OPTIONAL :: OPARALLELIO
END SUBROUTINE FMCLOS_ll
!
END INTERFACE
END MODULE MODI_FM_ll
