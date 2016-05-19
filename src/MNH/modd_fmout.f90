!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #################
      MODULE MODD_FMOUT
!     #################
!
!!****  *MODD_FMOUT* - declaration of informations on the instants for the 
!!      outputs of all models
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to declare the instants for the outputs 
!!     realized by all models.         
!       Introduced to facilitate the output FM-file managment in case of nesting
!     
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_PARAMETERS  :
!!         JPMODELMAX : Maximum allowed  number of nested models
!!
!!    REFERENCE
!!    ---------
!!      Book2 of Meso-NH documentation (module MODD_FMOUT)
!!          
!!    AUTHOR
!!    ------
!!	J.P. Lafore      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/07/96                     
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS
!
IMPLICIT NONE
!
REAL,SAVE, DIMENSION(JPMODELMAX,JPOUTMAX)  ::   XFMOUT    ! XFMOUT(m,i) array of 
! the increments in seconds from the beginning of the segment to the
! instant where the i-th fields output on FM-files is realized by model "m"
!
!
END MODULE MODD_FMOUT
