!      #######################################
       MODULE MODI_SET_CONC_LIMA
!      #######################################
!
INTERFACE
!
      SUBROUTINE SET_CONC_LIMA (HLUOUT,HGETCLOUD,PRHODREF,PRT,PSVT)
!
CHARACTER (LEN=*),         INTENT(IN) :: HLUOUT     ! name of the output-listing
CHARACTER (LEN=4),         INTENT(IN) :: HGETCLOUD  ! Get indicator
REAL, DIMENSION(:,:,:),    INTENT(IN) :: PRHODREF   ! Reference density
!
REAL, DIMENSION(:,:,:,:),  INTENT(INOUT) :: PRT     ! microphysical mixing ratios
!
REAL,  DIMENSION(:,:,:,:), INTENT(INOUT):: PSVT     ! microphys. concentrations
!
!
END SUBROUTINE SET_CONC_LIMA
!
END INTERFACE
!
END MODULE MODI_SET_CONC_LIMA
!
!     ###########################################################################
      SUBROUTINE SET_CONC_LIMA (HLUOUT,HGETCLOUD,PRHODREF,PRT,PSVT)
!     ###########################################################################
!
!!****  *SET_CONC_LIMA * - initialize droplet, raindrop and ice
!!                   concentration for a RESTArt simulation of the LIMA scheme
!!
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to initialize cloud droplet and rain drop
!!    concentrations when the cloud droplet and rain drop mixing ratios are
!!    only available (generally from a previous run using the Kessler scheme).
!!      This routine is used to initialize the droplet/drop concentrations
!!    using the r_c and r_r of a previous REVE or KESS run but also to compute
!!    the LB tendencies in ONE_WAY$n in case of grid-nesting when the optional
!!    argument PTIME is set (a LIMA run embedded in a KESS or REVE run).
!!
!!**  METHOD
!!    ------
!!      The method assumes a Csk law for the activation of aerososl with "s"
!!    the supersaturation (here 0.05 % is chosen). A Marshall-Palmer law with
!!    N_o=10**(-7) m**(-4) is assumed for the rain drop concentration.
!!      The initialization of the PSVT is straightforward for the cloud droplets
!!    while N_r=N_0/Lambda_r with Rho*r_r=Pi*Rho_w*N_0/(Lambda_r**4) is used for
!!    the rain drops. The HGETCLOUD test is used to discriminate between the
!!    'REVE' and 'KESS' options for CCLOUD in the previous run (from which
!!     PRT was calculated).
!!
!!    EXTERNAL
!!    --------
!!      None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_RAIN_C2R2_DESCR, ONLY : XRTMIN, XCTMIN
!!      Module MODD_RAIN_C2R2_KHKO_PARAM, ONLY : XCONCC_INI, XCONCR_PARAM_INI
!!      Module MODD_CONF,            ONLY : NVERB
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation ( routine SET_CONC_RAIN_C2R2 )
!!
!!    AUTHOR
!!    ------
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!      P. Jabouille     * CNRM/GMME *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/11/00
!!                        2014 G.Delautier : remplace MODD_RAIN_C2R2_PARAM par MODD_RAIN_C2R2_KHKO_PARAM
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAM_LIMA,      ONLY : XRTMIN, XCTMIN, LCOLD, LWARM
USE MODD_PARAM_LIMA_COLD, ONLY : XAI, XBI
USE MODD_NSV,             ONLY : NSV_LIMA_NC, NSV_LIMA_NR, NSV_LIMA_CCN_ACTI, NSV_LIMA_NI, NSV_LIMA_IFN_NUCL
USE MODD_CST,             ONLY : XPI, XRHOLW, XRHOLI
USE MODD_CONF,            ONLY : NVERB
!
USE MODE_FM
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=*),         INTENT(IN) :: HLUOUT     ! name of the output-listing
CHARACTER (LEN=4),         INTENT(IN) :: HGETCLOUD  ! Get indicator
REAL, DIMENSION(:,:,:),    INTENT(IN) :: PRHODREF   ! Reference density
!
REAL, DIMENSION(:,:,:,:),  INTENT(INOUT) :: PRT     ! microphysical mixing ratios
!
REAL,  DIMENSION(:,:,:,:), INTENT(INOUT):: PSVT     ! microphys. concentrations
!
!
!*       0.2   Declarations of local variables :
!
INTEGER    :: IRESP   ! Return code of FM routines
INTEGER    :: ILUOUT  ! Logical unit number of output-listing
REAL       :: ZCONCC, ZCONCR, ZCONCI
!
!-------------------------------------------------------------------------------
!*       1.    RETRIEVE LOGICAL UNIT NUMBER
!              ----------------------------
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
!
!*       2.    INITIALIZATION
!              --------------
!
IF (LWARM) THEN
!
!  droplets
!
   ZCONCC = 300 ! droplet concentration set at 300 cm-3
   WHERE ( PRT(:,:,:,2) > XRTMIN(2) )
      PSVT(:,:,:,NSV_LIMA_NC) = ZCONCC
      PSVT(:,:,:,NSV_LIMA_CCN_ACTI) = ZCONCC
   END WHERE
   WHERE ( PRT(:,:,:,2) <= XRTMIN(2) )
      PRT(:,:,:,2)  = 0.0
      PSVT(:,:,:,NSV_LIMA_NC) = 0.0
      PSVT(:,:,:,NSV_LIMA_CCN_ACTI) = 0.0
   END WHERE
   IF( NVERB >= 5 ) THEN
      WRITE (UNIT=ILUOUT,FMT=*) "!INI_MODEL$n: The droplet concentration has "
      WRITE (UNIT=ILUOUT,FMT=*) "been roughly initialised"
   END IF
!
!  drops
!
   ZCONCR = (1.E7)**3/(XPI*XRHOLW) ! cf XCONCR_PARAM_INI in ini_rain_c2r2.f90
   IF (HGETCLOUD == 'INI1') THEN ! init from REVE scheme
      PSVT(:,:,:,NSV_LIMA_NR) = 0.0
   ELSE ! init from KESS, ICE3...
      WHERE ( PRT(:,:,:,3) > XRTMIN(3) )
         PSVT(:,:,:,NSV_LIMA_NR) = MAX( SQRT(SQRT(PRHODREF(:,:,:)*PRT(:,:,:,3) &
              *ZCONCR)),XCTMIN(3) )
      END WHERE
      WHERE ( PRT(:,:,:,3) <= XRTMIN(3) )
         PRT(:,:,:,3)  = 0.0
         PSVT(:,:,:,NSV_LIMA_NR) = 0.0
      END WHERE
      IF( NVERB >= 5 ) THEN
         WRITE (UNIT=ILUOUT,FMT=*) "!INI_MODEL$n: The raindrop concentration has "
         WRITE (UNIT=ILUOUT,FMT=*) "been roughly initialised"
      END IF
   END IF
!
ENDIF
!
IF (LCOLD) THEN
!
! ice crystals
!
   ZCONCI = 100.E3 ! maximum ice concentration set at 100/L
   WHERE ( PRT(:,:,:,4) > XRTMIN(4) )
      PSVT(:,:,:,NSV_LIMA_NI) = MIN( PRHODREF(:,:,:) /                                     &
           ( XRHOLI * XAI*(10.E-06)**XBI * PRT(:,:,:,4) ), &
           ZCONCI )
      PSVT(:,:,:,NSV_LIMA_IFN_NUCL) = PSVT(:,:,:,NSV_LIMA_NI)
   END WHERE
   WHERE ( PRT(:,:,:,4) <= XRTMIN(4) )
      PRT(:,:,:,4)  = 0.0
      PSVT(:,:,:,NSV_LIMA_NI) = 0.0
      PSVT(:,:,:,NSV_LIMA_IFN_NUCL) = 0.0
   END WHERE
   IF( NVERB >= 5 ) THEN
      WRITE (UNIT=ILUOUT,FMT=*) "!INI_MODEL$n: The cloud ice concentration has "
      WRITE (UNIT=ILUOUT,FMT=*) "been roughly initialised"
   END IF
!
END IF
!
END SUBROUTINE SET_CONC_LIMA
