!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! NEC0 masdev4_7 2007/06/16 01:41:59
!-----------------------------------------------------------------
!     ################
      MODULE MODD_OUT_n
!     ################
!
!!****  *MODD_OUT$n* - declaration of informations on the instants for the 
!!      outputs
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to declare the variables
!     describing the instants for the outputs realized by one nested model.         
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of Meso-NH documentation (module MODD_OUTn)
!!          
!!    AUTHOR
!!    ------
!!	J.Stein      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/10/94                      
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX, JPOUTMAX
IMPLICIT NONE

TYPE OUT_t
!
!JUAN
  INTEGER, DIMENSION(:), POINTER   :: NOUT_TIMES => NULL() ! list of the values of the temporal
!JUAN
               !  index in the temporal model loop where fields outputs on 
               !  FM-files are realized by model n
  INTEGER             :: NOUT_NUMB ! number of outputs perform by model n
!
!
END TYPE OUT_t

TYPE(OUT_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: OUT_MODEL
LOGICAL    , DIMENSION(JPMODELMAX),         SAVE :: OUT_FIRST_CALL = .TRUE.

INTEGER, DIMENSION(:), POINTER :: NOUT_TIMES=>NULL()
INTEGER, POINTER :: NOUT_NUMB=>NULL()

CONTAINS

SUBROUTINE OUT_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
!JUAN
IF (OUT_FIRST_CALL(KTO)) THEN
ALLOCATE (OUT_MODEL(KTO)%NOUT_TIMES(JPOUTMAX))
OUT_FIRST_CALL(KTO) = .FALSE.
ENDIF
!JUAN

! Save current state for allocated arrays
!
! Current model is set to model KTO
NOUT_TIMES=>OUT_MODEL(KTO)%NOUT_TIMES
NOUT_NUMB=>OUT_MODEL(KTO)%NOUT_NUMB

END SUBROUTINE OUT_GOTO_MODEL

END MODULE MODD_OUT_n
