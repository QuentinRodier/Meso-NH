!MNH_LIC Copyright 1995-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!    ########################
     MODULE MODI_INI_RADIATIONS_AGG   
!    ########################
!
INTERFACE

    SUBROUTINE INI_RADIATIONS_AGG (KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG)
INTEGER, INTENT(IN)  :: KRAD_AGG      ! number of aggregated points
INTEGER, INTENT(OUT) :: KI_RAD_AGG    ! reformatted X array size
INTEGER, INTENT(OUT) :: KJ_RAD_AGG    ! reformatted Y array size
INTEGER, INTENT(OUT) :: KIOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, INTENT(OUT) :: KJOR_RAD_AGG  ! index of first point of packed array according to current domain
END SUBROUTINE INI_RADIATIONS_AGG

END INTERFACE

END MODULE MODI_INI_RADIATIONS_AGG
!
!   ############################################################################
    SUBROUTINE INI_RADIATIONS_AGG (KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG)
!   ############################################################################
!
!!****  *INI_RADIATIONS_AGG * - routine to call the SW and LW radiation calculations
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to aggregate columns of the temperature, water vapor
!!    liquid water, cloud fraction, ozone profiles for the ECMWF radiation
!!    calculations. There is a great number of available radiative fluxes in
!!    the output, but only the potential temperature radiative tendency and the
!!    SW and LW surface fluxes are provided in the output of the routine.
!!
!!**  METHOD
!!    ------
!!
!!    All columns are aggregated according to NRAD_AGG * NRAD_AGG points. 
!!    if NRAD_AGG = 1 : no aggregation  (default)
!!    if NRAD_AGG = 2 ; aggregation on  4 points, bottom/left is 1st (modulo 2) physical point 
!!    of the whole domain (not processor domain)
!!    if NRAD_AGG = 3 ; aggregation on  9 points, bottom/left is 1st  (modulo 3) physical point 
!!    of the whole domain (not processor domain) ; NHALO must be at least equal to 2
!!    if NRAD_AGG = 4 ; aggregation on 16 points, bottom/left is 1st   (modulo 4) physical point 
!!    of the whole domain (not processor domain) ; NHALO must be at least equal to 3
!!    if NRAD_AGG = 5 ; aggregation on 25 points, bottom/left is 1st  (modulo 5) physical point 
!!    of the whole domain (not processor domain) ; NHALO must be at least equal to 4
!!    
!!
!!    EXTERNAL
!!    --------
!!      Subroutine INI_RADIATIONS_AGG
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	V. Masson        * CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/23
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE PARKIND1,         ONLY: JPRB
!
USE MODD_CST
USE MODD_LUNIT_n,     ONLY : TLUOUT
USE MODD_CONF,        ONLY : NHALO
USE MODD_LBC_n,       ONLY : CLBCX, CLBCY
USE MODD_PARAMETERS,  ONLY : JPHEXT
!
USE MODE_ll
USE MODE_MSG
USE MODE_MODELN_HANDLER
USE MODD_VAR_ll, ONLY : IP
!
!  
IMPLICIT NONE
!
!*       0.1   DECLARATIONS OF DUMMY ARGUMENTS :
!
INTEGER, INTENT(IN)  :: KRAD_AGG      ! number of aggregated points
INTEGER, INTENT(OUT) :: KI_RAD_AGG    ! reformatted X array size
INTEGER, INTENT(OUT) :: KJ_RAD_AGG    ! reformatted Y array size
INTEGER, INTENT(OUT) :: KIOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, INTENT(OUT) :: KJOR_RAD_AGG  ! index of first point of packed array according to current domain
!
!
!*       0.2   DECLARATIONS OF LOCAL VARIABLES
!
!
INTEGER :: IIB           ! I index value of the first inner mass point (current domain)
INTEGER :: IJB           ! J index value of the first inner mass point
INTEGER :: IIE           ! I index value of the last inner mass point
INTEGER :: IJE           ! J index value of the last inner mass point
INTEGER :: IIB_ll        ! I index value of the first inner mass point (whole   domain)
INTEGER :: IJB_ll        ! J index value of the first inner mass point
INTEGER :: IIE_ll        ! I index value of the last inner mass point
INTEGER :: IJE_ll        ! J index value of the last inner mass point
INTEGER :: IIMAX_ll      ! I index size of the whole physical domain
INTEGER :: IJMAX_ll      ! J index size of the whole physical domain
!
INTEGER :: IIOR_RAD_AGG_ll       ! index of first point of packed array according to whole domain
INTEGER :: IJOR_RAD_AGG_ll       ! index of first point of packed array according to whole domain
!
INTEGER :: IIOR_ll      ! index of first point in the processor relative to the whole domain
INTEGER :: IJOR_ll      ! index of first point in the processor relative to the whole domain
!
INTEGER :: ILUOUT       ! Logical unit 
INTEGER :: IMI
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
!
!*       1.    CHECK OF COHERENCE
!              ------------------
!
IMI = GET_CURRENT_MODEL_INDEX()

ILUOUT = TLUOUT%NLU
!
IF (KRAD_AGG > NHALO+1) THEN
  WRITE(ILUOUT,*) ' +------------------------------------------------------+'
  WRITE(ILUOUT,*) ' [ Error in Radiation columns aggregation               |'
  WRITE(ILUOUT,*) ' [ NRAD_AGG = ',KRAD_AGG,'                                       |'
  WRITE(ILUOUT,*) ' [ NHALO    = ',NHALO,'                                       |'
  WRITE(ILUOUT,*) ' [ NRAD_AGG must be smaller than or equal to NHALO+1    |'
  WRITE(ILUOUT,*) ' +------------------------------------------------------+'
  !
  CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_RADIATIONS_AGG','Incoherence between NRAD_AGG and NHALO' )
END IF
!
!*       2.    COMPUTE DIMENSIONS OF ARRAYS AND OTHER INDICES
!              ----------------------------------------------
!
! full arrays
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)  ! this definition must be coherent with
                                      ! the one used in ini_radiations routine
!
! index of first physical point of the whole domain, relative to the whole domain
!
! Physical global domain bounds
!
CALL GET_GLOBALDIMS_ll ( IIMAX_ll,IJMAX_ll)
IF (CLBCX(1)=='CYCL') THEN
  IIB_ll = 1 + NHALO
  IIE_ll = IIMAX_ll + NHALO
ELSE
  IIB_ll = 1 + JPHEXT
  IIE_ll = IIMAX_ll + JPHEXT
END IF

IF (CLBCX(2)=='CYCL') THEN
  IJB_ll = 1 + NHALO
  IJE_ll = IJMAX_ll + NHALO
ELSE
  IJB_ll = 1 + JPHEXT
  IJE_ll = IJMAX_ll + JPHEXT
END IF
!
! index of first point in the modelessor relative to the whole domain (includes non-physical moints)
!
CALL GET_OR_ll('B',IIOR_ll,IJOR_ll)
!
! index of the first PACKED radiative column domain in the current processor relative to the whole domain
! There must be an entire number of packed domain between first global physical point and the packed domain in this processor
IIOR_RAD_AGG_ll = KRAD_AGG * ((IIOR_ll-1) / KRAD_AGG) + IIB_ll
IJOR_RAD_AGG_ll = KRAD_AGG * ((IJOR_ll-1) / KRAD_AGG) + IJB_ll
!
!
! index of the first PACKED radiative column domain in the current processor relative to the current processor domain
! It can be lower than first physical point index (i.e. inside the HALO points)
KIOR_RAD_AGG = IIOR_RAD_AGG_ll - IIOR_ll + 1
KJOR_RAD_AGG = IJOR_RAD_AGG_ll - IJOR_ll + 1
!
!
! removes packed columns that are entirely included in the HALO (no need to compute them on this processor)
!
KIOR_RAD_AGG = KIOR_RAD_AGG + ((IIB-KIOR_RAD_AGG)/KRAD_AGG) * KRAD_AGG
KJOR_RAD_AGG = KJOR_RAD_AGG + ((IJB-KJOR_RAD_AGG)/KRAD_AGG) * KRAD_AGG
!
!
! Number of PACKED radiative subdomains inside current processor domain
KI_RAD_AGG = (IIE - KIOR_RAD_AGG) / KRAD_AGG + 1
KJ_RAD_AGG = (IJE - KJOR_RAD_AGG) / KRAD_AGG + 1
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_RADIATIONS_AGG

