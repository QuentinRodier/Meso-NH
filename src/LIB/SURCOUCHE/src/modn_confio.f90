!     ##################
      MODULE MODN_CONFIO
!     ##################
!
!!****  *MODN_CONFIO* - declaration of namelist NAM_CONFIO
!!
!!    PURPOSE
!!    -------
!     Define I/O configuration variables that can be set with the NAM_CONFIO namelist
!!    /!\ These variables must be transmitted to the SURCOUCHE library via the
!!    SET_CONFIO_ll subroutine before the FIRST call to FMOPEN_ll.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      D.Gazen L.A.
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    31/03/2014    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
LOGICAL,SAVE :: LCDF4    = .FALSE. ! TRUE : enable NetCDF4 Input/Output
LOGICAL,SAVE :: LLFIOUT  = .FALSE. ! TRUE : add LFI output when NetCDF4 I/O is enabled (debug)
LOGICAL,SAVE :: LLFIREAD = .FALSE. ! TRUE : enable LFI reading (disable NetCDF4 reading)
                                   !        when NetCDF4 I/O is enabled (debug)
!
NAMELIST/NAM_CONFIO/ LCDF4, LLFIOUT, LLFIREAD
!
END MODULE MODN_CONFIO

