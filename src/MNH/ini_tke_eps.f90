!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2006/07/19 10:33:46
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_INI_TKE_EPS
!     #######################
INTERFACE
!
   SUBROUTINE INI_TKE_EPS(HGETTKET,PTHVREF,PZZ, &
                          PUT,PVT,PTHT,                  &
                          PTKET,TPINITHALO3D_ll    )
!
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
CHARACTER (LEN=*),               INTENT(IN)  :: HGETTKET
             ! character string indicating whether TKE must be 
             ! initialized or not
REAL, DIMENSION(:,:,:),        INTENT(IN)   :: PTHVREF ! virtual potential
                                                       ! temperature
REAL, DIMENSION(:,:,:),        INTENT(IN)   :: PZZ     ! physical height for
                                                       ! w-point
REAL, DIMENSION(:,:,:),        INTENT(INOUT):: PUT     ! x-component of wind
REAL, DIMENSION(:,:,:),        INTENT(INOUT):: PVT     ! y-component of wind
REAL, DIMENSION(:,:,:),        INTENT(INOUT):: PTHT    ! potential temperature
REAL, DIMENSION(:,:,:),        INTENT(INOUT):: PTKET ! TKE fields 
TYPE(LIST_ll), POINTER                      :: TPINITHALO3D_ll ! pointer for the list of fields
                                      !  which must be communicated in INIT
!
END SUBROUTINE INI_TKE_EPS
!
END INTERFACE
!
END MODULE MODI_INI_TKE_EPS
!
!  ###################################################################
   SUBROUTINE INI_TKE_EPS(HGETTKET,PTHVREF,PZZ, &
                          PUT,PVT,PTHT,                  &
                          PTKET,TPINITHALO3D_ll    )
!  ###################################################################
!
!
!! ****  *INI_TKE* initializes by a 1D stationarized TKE equation the 
!!                 values of TKE. A positivity control is made. The 
!!                 dissipation of TKE is set to its minimum value.
!!
!!  PURPOSE
!!  -------
!      The purpose of this routine is to initialize the values of the 
!   turbulence kinetic energy. The dissipation is intialized to its minimum
!   value.
!
!!** METHOD
!!   ------
!!      A diagnostic 1D equation for the TKE is used. The transport terms 
!!   are neglected. 
!!
!!   EXTERNAL
!!   --------
!!      DZF ,MXF, MYF, MZM  : Shuman operators
!!      ADD3DFIELD_ll       : add a field to 3D-list 
!!
!!   IMPLICIT ARGUMENTS
!!   ------------------
!!     MODD_CST   : XG, XRV, XRD
!!     MODD_CTURB : XLINI, XTKEMIN, XCED, XCMFS
!!     MODD_PARAMETERS: JPVEXT
!!
!!   REFERENCE
!!   ---------
!!     Book 2 of Documentation (routine INI_TKE)
!!     Book 1 of Documentation (Chapter Turbulence)
!!
!!   AUTHOR
!!   ------
!!     Joan Cuxart          * INM and Meteo-France *
!!
!!   MODIFICATIONS
!!   -------------
!!     Original             Jan 19, 1995
!!                          Feb 13, 1995 (J. Cuxart) add EPS initialization
!!                          March 25, 1995 (J. Stein)add PZZ in the arguments 
!!                          to compute a real gradient and allow RESTA conf.                
!!                          Aug 10, 1998 (N. Asencio) add parallel code
!!                          May 2006  Remove KEPS
!! -------------------------------------------------------------------------
!
!*          0. DECLARATIONS
!              ------------
!
USE MODD_CTURB      ! XLINI, XCED, XCMFS, XTKEMIN
USE MODD_CST        ! XG, XRD,  XRV
USE MODD_PARAMETERS ! JPVEXT
!
USE MODI_SHUMAN     ! DZF, MXF, MYF, MZM
USE MODE_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
!
IMPLICIT NONE
!
!*          0.1. declarations of arguments
!
CHARACTER (LEN=*),               INTENT(IN)  :: HGETTKET
             ! character string indicating whether TKE must be 
             ! initialized or not
REAL, DIMENSION(:,:,:),        INTENT(IN)   :: PTHVREF ! virtual potential
                                                       ! temperature
REAL, DIMENSION(:,:,:),        INTENT(IN)   :: PZZ     ! physical height for
                                                       ! w-point
REAL, DIMENSION(:,:,:),        INTENT(INOUT):: PUT     ! x-component of wind
REAL, DIMENSION(:,:,:),        INTENT(INOUT):: PVT     ! y-component of wind
REAL, DIMENSION(:,:,:),        INTENT(INOUT):: PTHT    ! potential temperature
REAL, DIMENSION(:,:,:),        INTENT(INOUT):: PTKET ! TKE field 
TYPE(LIST_ll), POINTER                      :: TPINITHALO3D_ll ! pointer for the list of fields
                                      !  which must be communicated in INIT
!
!*       0.2    Declaration of local variables
!
INTEGER ::         IKB,IKE,IKU! index value for the first and last inner
                              ! mass points
INTEGER ::         JKK        ! vertical loop index
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZDELTZ ! vertical
                                                               ! increment
!
! ---------------------------------------------------------------------
! 
! 
IKB=1+JPVEXT
IKE=SIZE(PTHT,3)-JPVEXT
IKU=SIZE(PTHT,3)
!
!*       1.     TKE DETERMINATION
!               -----------------
!
DO JKK=IKB-1,IKE
  ZDELTZ(:,:,JKK) = PZZ(:,:,JKK+1)-PZZ(:,:,JKK)
END DO
ZDELTZ(:,:,IKE+1) = ZDELTZ(:,:,IKE)
!
IF (HGETTKET == 'INIT' ) THEN
!  instant t
  PTHT(:,:,IKB-1) = PTHT(:,:,IKB)
  PUT(:,:,IKB-1)  = PUT(:,:,IKB)
  PVT(:,:,IKB-1)  = PVT(:,:,IKB)
  !
  PTHT(:,:,IKE+1) = PTHT(:,:,IKE)
  PUT(:,:,IKE+1)  = PUT(:,:,IKE)
  PVT(:,:,IKE+1)  = PVT(:,:,IKE)
  !
  ! determines TKE
  PTKET(:,:,:)=(XLINI**2/XCED)*(  &
                  XCMFS*( DZF(1,IKU,1,MXF(MZM(1,IKU,1,PUT)))**2                  &
                         +DZF(1,IKU,1,MYF(MZM(1,IKU,1,PVT)))**2) / ZDELTZ        &
                 -(XG/PTHVREF)*XCSHF*DZF(1,IKU,1,MZM(1,IKU,1,PTHT))              &
                               ) / ZDELTZ
  ! positivity control
  WHERE (PTKET < XTKEMIN) PTKET=XTKEMIN
  !
  !
  ! Add PTKET to TPINITHALO3D_ll list of fields updated at the
  ! end of initialization
  CALL ADD3DFIELD_ll (TPINITHALO3D_ll,PTKET)
END IF
!
!
END SUBROUTINE INI_TKE_EPS
