!MNH_LIC Copyright 2023-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!    ########################
     MODULE MODI_INI_RADIATIONS_AGG   
!    ########################
!
INTERFACE

    SUBROUTINE INI_RADIATIONS_AGG (KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG,KRAD_AGG_FLAG)
INTEGER, INTENT(IN)  :: KRAD_AGG      ! number of aggregated points
INTEGER, INTENT(OUT) :: KI_RAD_AGG    ! reformatted X array size
INTEGER, INTENT(OUT) :: KJ_RAD_AGG    ! reformatted Y array size
INTEGER, INTENT(OUT) :: KIOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, INTENT(OUT) :: KJOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KRAD_AGG_FLAG  ! flag to know if aggregated column is computed in this processor or another one
END SUBROUTINE INI_RADIATIONS_AGG

END INTERFACE

END MODULE MODI_INI_RADIATIONS_AGG
!
!   ############################################################################
    SUBROUTINE INI_RADIATIONS_AGG (KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG,KRAD_AGG_FLAG)
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
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KRAD_AGG_FLAG  ! flag to know if aggregated column is computed in this processor or another one
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

LOGICAL :: LREMOVE_SOUTH, LREMOVE_NORTH, LREMOVE_EAST, LREMOVE_WEST ! flags to not keep packed column in current processor
INTEGER :: ISOUTH, INORTH, IEAST, IWEST ! inner limits of packed colums on each side of the processor (in local coordinate)
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
!-------------------------------------------------------------------------------
!
! Number of PACKED radiative subdomains inside current processor domain
KI_RAD_AGG = (IIE - KIOR_RAD_AGG) / KRAD_AGG + 1
KJ_RAD_AGG = (IJE - KJOR_RAD_AGG) / KRAD_AGG + 1
!
!-------------------------------------------------------------------------------
!
! REMOVES Aggregated columns that are duplicated in several processors
!
! Checks if the middle of the packed column is in the current processor
! 
LREMOVE_SOUTH = .FALSE.
LREMOVE_NORTH = .FALSE.
LREMOVE_EAST  = .FALSE.
LREMOVE_WEST  = .FALSE.
!
KRAD_AGG_FLAG(:,:) = 0.
!
! inner limits of packed colums on each side of the processor (in local coordinate)
IWEST  = KIOR_RAD_AGG +                KRAD_AGG-1
IEAST  = KIOR_RAD_AGG + (KI_RAD_AGG-1)*KRAD_AGG
ISOUTH = KJOR_RAD_AGG +                KRAD_AGG-1
INORTH = KJOR_RAD_AGG + (KJ_RAD_AGG-1)*KRAD_AGG
!
! Eastern side of processor (if NOT of whole domain) (checks on last X index of aggregated columns)
IF (IEAST+KRAD_AGG/2 > IIE .AND. .NOT. (LEAST_ll() .AND. CLBCX(2)=='OPEN') ) THEN
   KRAD_AGG_FLAG(IEAST:IIE,:) = 3  ! points located there will take values computed by processor towards east
   LREMOVE_EAST = .TRUE.
END IF
! Western side of processor (if NOT of whole domain) (checks on first X index of aggregated columns)
IF (KIOR_RAD_AGG+KRAD_AGG/2 < IIB  .AND. .NOT. (LWEST_ll() .AND. CLBCX(1)=='OPEN') ) THEN
   KRAD_AGG_FLAG(IIB:IWEST,:) = 1  ! points located there will take values computed by processor towards west
   LREMOVE_WEST = .TRUE.
END IF
! Northern side of processor (if NOT of whole domain) (checks on last Y index of aggregated columns)
IF (INORTH+KRAD_AGG/2 > IJE .AND. .NOT. (LNORTH_ll() .AND. CLBCY(2)=='OPEN') ) THEN
   KRAD_AGG_FLAG(:,INORTH:IJE) = 4  ! points located there will take values computed by processor towards north
   LREMOVE_NORTH= .TRUE.
END IF
! Southern side of processor (if NOT of whole domain) (checks on first X index of aggregated columns)
IF (KJOR_RAD_AGG+KRAD_AGG/2 < IJB  .AND. .NOT. (LSOUTH_ll() .AND. CLBCY(1)=='OPEN') ) THEN
   KRAD_AGG_FLAG(:,IJB:ISOUTH) = 2  ! points located there will take values computed by processor towards south
   LREMOVE_SOUTH= .TRUE.
END IF
!
! North-Eastern corner of processor (if NOT of whole domain)
IF (       IEAST+KRAD_AGG/2 > IIE .AND.  INORTH+KRAD_AGG/2 > IJE  ) THEN
   IF (.NOT. (LEAST_ll()  .AND. CLBCX(2)=='OPEN') .AND. .NOT. (LNORTH_ll() .AND. CLBCY(2)=='OPEN') ) THEN
     KRAD_AGG_FLAG(IEAST:IIE,INORTH:IJE) = 7  
     ! points located there will take values computed by processor towards NE
     LREMOVE_EAST  = .TRUE.
     LREMOVE_NORTH = .TRUE.
   ELSE IF ( (LEAST_ll()  .AND. CLBCX(2)=='OPEN') .AND. .NOT. (LNORTH_ll() .AND. CLBCY(2)=='OPEN') ) THEN
     KRAD_AGG_FLAG(:,INORTH:IJE) = 4  ! points located there will take values computed by processor towards north
     LREMOVE_NORTH = .TRUE.
   ELSE IF ( (LNORTH_ll()  .AND. CLBCY(2)=='OPEN') .AND. .NOT. (LEAST_ll()  .AND. CLBCX(2)=='OPEN') ) THEN
     KRAD_AGG_FLAG(IEAST:IIE,:) = 3  ! points located there will take values computed by processor towards east
     LREMOVE_EAST = .TRUE.
   END IF
END IF
!
! North-Western corner of processor (if NOT of whole domain)
IF (       KIOR_RAD_AGG+KRAD_AGG/2 < IIB .AND.  INORTH+KRAD_AGG/2 > IJE  ) THEN
   IF (.NOT. (LWEST_ll()  .AND. CLBCX(1)=='OPEN') .AND. .NOT. (LNORTH_ll() .AND. CLBCY(2)=='OPEN')) THEN
     KRAD_AGG_FLAG(IIB:IWEST,INORTH:IJE) = 8  ! points located there will take values computed by processor towards NW
     LREMOVE_WEST  = .TRUE.
     LREMOVE_NORTH = .TRUE.
   ELSE IF ( (LWEST_ll()  .AND. CLBCX(1)=='OPEN') .AND. .NOT. (LNORTH_ll() .AND. CLBCY(2)=='OPEN')) THEN
     KRAD_AGG_FLAG(IIB:IWEST,INORTH:IJE) = 4  ! points located there will take values computed by processor towards north
     LREMOVE_NORTH = .TRUE.
   ELSE IF ( (LNORTH_ll()  .AND. CLBCY(2)=='OPEN') .AND. .NOT. (LWEST_ll()  .AND. CLBCX(1)=='OPEN')) THEN
     KRAD_AGG_FLAG(IIB:IWEST,INORTH:IJE) = 1  ! points located there will take values computed by processor towards west
     LREMOVE_WEST = .TRUE.
   END IF
END IF
!
! South-Western corner of processor (if NOT of whole domain)
IF (       KIOR_RAD_AGG+KRAD_AGG/2 < IIB .AND.  KJOR_RAD_AGG+KRAD_AGG/2 < IJB ) THEN
   IF (.NOT. (LWEST_ll()  .AND. CLBCX(1)=='OPEN') .AND. .NOT. (LSOUTH_ll() .AND. CLBCY(1)=='OPEN') ) THEN
     KRAD_AGG_FLAG(IIB:IWEST,IIB:ISOUTH) = 5  ! points located there will take values computed by processor towards SW
     LREMOVE_WEST  = .TRUE.
     LREMOVE_SOUTH = .TRUE.
   ELSE IF ( (LWEST_ll()  .AND. CLBCX(1)=='OPEN')  .AND. .NOT. (LSOUTH_ll() .AND. CLBCY(1)=='OPEN') ) THEN
     KRAD_AGG_FLAG(IIB:IWEST,IIB:ISOUTH) = 2  ! points located there will take values computed by processor towards south
     LREMOVE_SOUTH = .TRUE.
   ELSE IF ( (LSOUTH_ll()  .AND. CLBCY(2)=='OPEN') .AND. .NOT. (LWEST_ll()  .AND. CLBCX(1)=='OPEN') ) THEN
     KRAD_AGG_FLAG(IIB:IWEST,IIB:ISOUTH) = 1  ! points located there will take values computed by processor towards west
     LREMOVE_WEST = .TRUE.
   END IF
END IF
!
! South-Eastern corner of processor (if NOT of whole domain)
IF (       IEAST+KRAD_AGG/2 > IIE .AND.  KJOR_RAD_AGG+KRAD_AGG/2 < IJB ) THEN
   IF (.NOT. (LEAST_ll()  .AND. CLBCX(1)=='OPEN') .AND. .NOT. (LSOUTH_ll() .AND. CLBCY(1)=='OPEN') ) THEN
     KRAD_AGG_FLAG(IEAST:IIE,IIB:ISOUTH) = 6  ! points located there will take values computed by processor towards SW
     LREMOVE_EAST  = .TRUE.
     LREMOVE_SOUTH = .TRUE.
   ELSE IF ( (LEAST_ll()  .AND. CLBCX(1)=='OPEN')  .AND. .NOT. (LSOUTH_ll() .AND. CLBCY(1)=='OPEN') ) THEN
     KRAD_AGG_FLAG(IEAST:IIE,IIB:ISOUTH) = 2  ! points located there will take values computed by processor towards south
     LREMOVE_SOUTH = .TRUE.
   ELSE IF ( (LSOUTH_ll()  .AND. CLBCY(2)=='OPEN') .AND. .NOT. (LEAST_ll()  .AND. CLBCX(1)=='OPEN') ) THEN
     KRAD_AGG_FLAG(IEAST:IIE,IIB:ISOUTH) = 3  ! points located there will take values computed by processor towards west
     LREMOVE_EAST = .TRUE.
   END IF
END IF
!
! removes from current processor the column that was partially (and majoritaly) in the other processor
!
IF (LREMOVE_EAST) KI_RAD_AGG = KI_RAD_AGG -1 
IF (LREMOVE_WEST) THEN
   KIOR_RAD_AGG = KIOR_RAD_AGG + KRAD_AGG
   KI_RAD_AGG = KI_RAD_AGG -1
END IF
IF (LREMOVE_NORTH) KJ_RAD_AGG = KJ_RAD_AGG -1
IF (LREMOVE_SOUTH) THEN
   KJOR_RAD_AGG = KJOR_RAD_AGG + KRAD_AGG
   KJ_RAD_AGG = KJ_RAD_AGG -1
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_RADIATIONS_AGG

