!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #################
      MODULE MODN_FMOUT
!     #################
!
!!****  *MODN_FMOUT* - declaration of namelist NAM_FMOUT
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify the namelist  NAM_FMOUT
!     which concerns the instants for the outputs realized by all models.         
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_FMOUT : contains declaration of the variables describing 
!!                          the instants for the outputs
!!
!!         XFMOUT :  XFMOUT(m,i) array of increments in seconds from 
!!                 the beginning of the segment to the instant where the i-th
!!                 fields output on FM-files is realized by model "m"
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
USE MODD_FMOUT
!
IMPLICIT NONE
!
NAMELIST/NAM_FMOUT/XFMOUT
!
END MODULE MODN_FMOUT
