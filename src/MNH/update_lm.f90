!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!     ###################
      MODULE MODI_UPDATE_LM
!     ###################
INTERFACE
!
SUBROUTINE UPDATE_LM(HLBCX,HLBCY,PLM,PLEPS)
!
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X boundary type
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y boundary type
!
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PLM   ! mixing length
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PLEPS ! dissipative length
!
END SUBROUTINE UPDATE_LM
!
END INTERFACE
!
END MODULE MODI_UPDATE_LM
!
!
!
!     #################################################################
      SUBROUTINE UPDATE_LM(HLBCX,HLBCY,PLM,PLEPS)
!     #################################################################
!
!!****  *UPDATE_LM* - routine to set external points for mixing length
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------   
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!        
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine UPDATE_LM)
!!
!!    AUTHOR
!!    ------
!!	V. Masson        * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    april 2006
!!       V.Masson : Exchange of East and North sides
!!   J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!         
USE MODD_CONF
USE MODD_PARAMETERS
!
USE MODE_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
!
IMPLICIT NONE
!
!
!*       0.1   declarations of arguments
!
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X boundary type
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y boundary type
!
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PLM   ! mixing length
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PLEPS ! dissipative length
!
!*       0.2   declarations of local variables
!
INTEGER             :: IIB      ! First physical index in x direction
INTEGER             :: IJB      ! First physical index in y direction
INTEGER             :: IIE      ! last  physical index in x direction
INTEGER             :: IJE      ! last  physical index in y direction
INTEGER             :: JI       ! loop index
!
TYPE(LIST_ll), POINTER :: TZLM_ll   ! list of fields to exchange
INTEGER                :: IINFO_ll       ! return code of parallel routine
!
!-------------------------------------------------------------------------------
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS :
!              ----------------------------
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
NULLIFY(TZLM_ll)
!
!-------------------------------------------------------------------------------
!
!*       2.  UPDATE HALOs :
!            -------------
!
!
!!$IF(NHALO == 1) THEN
  CALL ADD3DFIELD_ll(TZLM_ll,PLM)
  CALL ADD3DFIELD_ll(TZLM_ll,PLEPS)
  CALL UPDATE_HALO_ll(TZLM_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZLM_ll)
!!$END IF
!
!-------------------------------------------------------------------------------
!
!*       3.  UPDATE EXTERNAL POINTS OF GLOBAL DOMAIN:
!            ---------------------------------------
!
IF ( HLBCX(1) /= "CYCL" .AND. LWEST_ll()) THEN
  PLM  (IIB-1,:,:) = PLM  (IIB,:,:)
  PLEPS(IIB-1,:,:) = PLEPS(IIB,:,:)
END IF
IF ( HLBCX(2) /= "CYCL" .AND. LEAST_ll()) THEN
  PLM  (IIE+1,:,:) = PLM  (IIE,:,:)
  PLEPS(IIE+1,:,:) = PLEPS(IIE,:,:)
END IF
IF ( HLBCY(1) /= "CYCL" .AND. LSOUTH_ll()) THEN
  DO JI=1,SIZE(PLM,1)
    PLM  (JI,IJB-1,:) = PLM  (JI,IJB,:)
    PLEPS(JI,IJB-1,:) = PLEPS(JI,IJB,:)
  END DO
END IF
IF ( HLBCY(2) /= "CYCL" .AND. LNORTH_ll()) THEN
  DO JI=1,SIZE(PLM,1)
    PLM  (JI,IJE+1,:) = PLM  (JI,IJE,:)
    PLEPS(JI,IJE+1,:) = PLEPS(JI,IJE,:)
  END DO
END IF
!-----------------------------------------------------------------------------
END SUBROUTINE UPDATE_LM
