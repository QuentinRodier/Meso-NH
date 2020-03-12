!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###########################
      MODULE MODI_ADVECTION_METSV
!     ###########################
!
INTERFACE
      SUBROUTINE ADVECTION_METSV (TPFILE, OCLOSE_OUT,HUVW_ADV_SCHEME,          &
                            HMET_ADV_SCHEME,HSV_ADV_SCHEME, HCLOUD, KSPLIT,    &
                            OSPLIT_CFL, PSPLIT_CFL, OCFL_WRIT,                 &
                            HLBCX, HLBCY, KRR, KSV, TPDTCUR, PTSTEP,           &
                            PUT, PVT, PWT, PTHT, PRT, PTKET, PSVT, PPABST,     &
                            PTHVREF, PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,     &
                            PRTHS, PRRS, PRTKES, PRSVS,                        &
                            PRTHS_CLD, PRRS_CLD, PRSVS_CLD, PRTKES_ADV         )
!
USE MODD_IO_ll,     ONLY: TFILEDATA
USE MODD_TYPE_DATE, ONLY: DATE_TIME
!
LOGICAL,                INTENT(IN)   ::  OCLOSE_OUT   ! switch for syncronous
                                                      ! file opening
TYPE(TFILEDATA),        INTENT(IN)   ::  TPFILE       ! Output file
CHARACTER(LEN=6),       INTENT(IN)   :: HMET_ADV_SCHEME, & ! Control of the 
                                        HSV_ADV_SCHEME, &  ! scheme applied 
                                        HUVW_ADV_SCHEME
CHARACTER (LEN=4),      INTENT(IN)   :: HCLOUD      ! Kind of cloud parameterization                                
!
INTEGER,                INTENT(INOUT):: KSPLIT       ! Number of time splitting
                                                     ! for PPM advection
LOGICAL,                INTENT(IN)   :: OSPLIT_CFL   ! flag to automatically chose number of iterations
REAL,                   INTENT(IN)   :: PSPLIT_CFL   ! maximum CFL to automatically chose number of iterations
LOGICAL,                INTENT(IN)   :: OCFL_WRIT    ! flag to write CFL fields in output files            
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
INTEGER,                  INTENT(IN)    :: KRR  ! Number of moist variables
INTEGER,                  INTENT(IN)    :: KSV  ! Number of Scalar Variables
!
TYPE (DATE_TIME),         INTENT(IN)    :: TPDTCUR ! current date and time
REAL,                     INTENT(IN)    :: PTSTEP
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT , PVT  , PWT
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET, PRHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST                 
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT , PSVT
                                                  ! Variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHVREF   ! Virtual Temperature
                                          ! of the reference state
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ,PDZX,PDZY
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS, PRTKES
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS , PRSVS
                                                  ! Sources terms 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRTHS_CLD
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRRS_CLD,PRSVS_CLD
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PRTKES_ADV  ! Advection TKE source term 
!
END SUBROUTINE ADVECTION_METSV
!
END INTERFACE
!
END MODULE MODI_ADVECTION_METSV
!     ##########################################################################
      SUBROUTINE ADVECTION_METSV (TPFILE, OCLOSE_OUT,HUVW_ADV_SCHEME,          &
                            HMET_ADV_SCHEME,HSV_ADV_SCHEME, HCLOUD, KSPLIT,    &
                            OSPLIT_CFL, PSPLIT_CFL, OCFL_WRIT,                 &
                            HLBCX, HLBCY, KRR, KSV, TPDTCUR, PTSTEP,           &
                            PUT, PVT, PWT, PTHT, PRT, PTKET, PSVT, PPABST,     &
                            PTHVREF, PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,     &
                            PRTHS, PRRS, PRTKES, PRSVS,                        &
                            PRTHS_CLD, PRRS_CLD, PRSVS_CLD, PRTKES_ADV         )
!     ##########################################################################
!
!!****  *ADVECTION_METSV * - routine to call the specialized advection routines
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to control the advection routines.
!!    For that, it is first necessary to compute the metric coefficients
!!    and the contravariant components of the momentum.
!!
!!**  METHOD
!!    ------
!!      Once the scheme is selected, it is applied to the following group of
!!    variables: METeorologicals (temperature, water substances, TKE,
!!    dissipation TKE) and Scalar Variables. It is possible to select different
!!    advection schemes for each group of variables.
!!
!!    EXTERNAL
!!    --------
!!      CONTRAV              : computes the contravariant components.
!!      ADVECUVW             : computes the advection terms for momentum.
!!      ADVECSCALAR          : computes the advection terms for scalar fields.
!!      ADD3DFIELD_ll        : add a field to 3D-list
!!      ADVEC_4TH_ORDER      : 4th order advection scheme
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE
!!
!!    REFERENCE
!!    ---------
!!      Book1 and book2 ( routine ADVECTION )
!!
!!    AUTHOR
!!    ------
!!	J.-P. Pinty      * Laboratoire d'Aerologie*
!!	J.-P. Lafore     * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/07/94 
!!                  01/04/95 (Ph. Hereil J. Nicolau) add the model number
!!                  23/10/95 (J. Vila and JP Lafore) advection schemes scalar
!!                  16/01/97 (JP Pinty)              change presentation 
!!                  30/04/98 (J. Stein P Jabouille)  extrapolation for the cyclic
!!                                                   case and parallelisation
!!                  24/06/99 (P Jabouille)           case of NHALO>1
!!                  25/10/05 (JP Pinty)              4th order scheme
!!                  24/04/06 (C.Lac)                 Split scalar and passive
!!                                                   tracer routines
!!                  08/06    (T.Maric)               PPM scheme
!!                  04/2011  (V.Masson & C. Lac)     splits the routine and add time splitting
!!                  04/2014  (C.Lac)                 adaptation of time
!!                                                   splitting for L1D and L2D
!!                  09/2014  (G.Delautier)              close OUTPUT_LISTING before STOP
!!                  04/2015  (J.Escobar) remove/commente some NHALO=1 test
!!                  J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!                  J.Escobar : 01/10/2015 : add computation of CFL for L1D case
!!                  04/2016  (C.Lac)       : correction of negativity for KHKO
!!                  10/2016  (C.Lac) Correction on the flag for Strang splitting
!!                                  to insure reproducibility between START and RESTA
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!                  07/2017  (V. Vionnet)  : add advection of 2D variables at
!!                                      the surface for the blowing snow scheme
!!                  03/2020 (B.Vie) : LIMA negativity checks after turbulence, advection and 
!!                                    microphysics budgets 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BUDGET
USE MODD_CST 
USE MODD_CTURB,     ONLY: XTKEMIN
USE MODD_CONF,      ONLY: LNEUTRAL,NHALO,L1D, L2D
USE MODD_IO_ll,     ONLY: TFILEDATA
USE MODD_LUNIT_n,   ONLY: TLUOUT
USE MODD_PARAM_n
USE MODD_TYPE_DATE, ONLY: DATE_TIME
USE MODD_BLOWSNOW
USE MODD_BLOWSNOW_n
USE MODD_PARAMETERS
!
USE MODD_NSV
USE MODD_PARAM_LIMA
!
USE MODE_FIELD,     ONLY: TFIELDDATA, TYPEREAL
USE MODE_FMWRIT
USE MODE_IO_ll
USE MODE_ll
USE MODE_MSG
!
USE MODI_ADV_BOUNDARIES
USE MODI_BUDGET
USE MODI_CONTRAV
USE MODI_GET_HALO
USE MODI_PPM_RHODJ
USE MODI_PPM_MET
USE MODI_PPM_SCALAR
!
!-------------------------------------------------------------------------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
LOGICAL,                INTENT(IN)   ::  OCLOSE_OUT   ! switch for synchronous
                                                      ! file opening
TYPE(TFILEDATA),        INTENT(IN)   ::  TPFILE       ! Output file
CHARACTER(LEN=6),       INTENT(IN)   :: HMET_ADV_SCHEME, & ! Control of the 
                                        HSV_ADV_SCHEME, &  ! scheme applied 
                                        HUVW_ADV_SCHEME
CHARACTER (LEN=4),      INTENT(IN)   :: HCLOUD      ! Kind of cloud parameterization                                
!
INTEGER,                INTENT(INOUT):: KSPLIT       ! Number of time splitting
                                                     ! for PPM advection
LOGICAL,                INTENT(IN)   :: OSPLIT_CFL   ! flag to automatically chose number of iterations
REAL,                   INTENT(IN)   :: PSPLIT_CFL   ! maximum CFL to automatically chose number of iterations
LOGICAL,                INTENT(IN)   :: OCFL_WRIT    ! flag to write CFL fields in output files            
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
INTEGER,                  INTENT(IN)    :: KRR  ! Number of moist variables
INTEGER,                  INTENT(IN)    :: KSV  ! Number of Scalar Variables
!
TYPE (DATE_TIME),         INTENT(IN)    :: TPDTCUR ! current date and time
REAL,                     INTENT(IN)    :: PTSTEP
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT , PVT  , PWT
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT, PTKET, PRHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST                 
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT , PSVT
                                                  ! Variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHVREF   ! Virtual Temperature
                                          ! of the reference state
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ,PDZX,PDZY
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS, PRTKES
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS , PRSVS
                                                  ! Sources terms 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRTHS_CLD
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRRS_CLD, PRSVS_CLD
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PRTKES_ADV  ! Advection TKE source term 
!
!
!*       0.2   declarations of local variables
!
!
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRUCPPM
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRVCPPM
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRWCPPM
                                                  ! contravariant
                                                  ! components
                                                  ! of momentum
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZCFLU
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZCFLV
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZCFLW
!                                                 ! CFL numbers on each direction
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZCFL
!                                                 ! CFL number
!
REAL :: ZCFLU_MAX, ZCFLV_MAX, ZCFLW_MAX, ZCFL_MAX ! maximum CFL numbers
!
REAL, DIMENSION(SIZE(PTHT,1), SIZE(PTHT,2), SIZE(PTHT,3) ) :: ZTH
REAL, DIMENSION(SIZE(PTKET,1),SIZE(PTKET,2),SIZE(PTKET,3)) :: ZTKE
REAL, DIMENSION(SIZE(PTHT,1), SIZE(PTHT,2), SIZE(PTHT,3) ) :: ZRTHS_OTHER
REAL, DIMENSION(SIZE(PTKET,1),SIZE(PTKET,2),SIZE(PTKET,3)) :: ZRTKES_OTHER
REAL, DIMENSION(SIZE(PTHT,1), SIZE(PTHT,2), SIZE(PTHT,3) ) :: ZRTHS_PPM
REAL, DIMENSION(SIZE(PTKET,1),SIZE(PTKET,2),SIZE(PTKET,3)) :: ZRTKES_PPM
REAL, DIMENSION(SIZE(PRT,1), SIZE(PRT,2), SIZE(PRT,3), SIZE(PRT,4) ) :: ZR
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),SIZE(PSVT,4)) :: ZSV
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3), NBLOWSNOW_2D) :: ZSNWC
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3), NBLOWSNOW_2D) :: ZSNWC_INIT
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3), NBLOWSNOW_2D) :: ZRSNWCS
! Guess at the sub time step
REAL, DIMENSION(SIZE(PRT,1), SIZE(PRT,2), SIZE(PRT,3), SIZE(PRT,4) ) :: ZRRS_OTHER
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),SIZE(PSVT,4)) :: ZRSVS_OTHER
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),NBLOWSNOW_2D) ::  ZRSNWCS_OTHER
! Tendencies since the beginning of the time step
REAL, DIMENSION(SIZE(PRT,1), SIZE(PRT,2), SIZE(PRT,3), SIZE(PRT,4) ) :: ZRRS_PPM
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),SIZE(PSVT,4)) :: ZRSVS_PPM
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),NBLOWSNOW_2D) :: ZRSNWCS_PPM
! Guess at the end of the sub time step
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRHOX1,ZRHOX2
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRHOY1,ZRHOY2
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRHOZ1,ZRHOZ2
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)):: ZT,ZEXN,ZLV,ZLS,ZCPH,ZCOR
! Temporary advected rhodj for PPM routines
!
INTEGER :: JS,JR,JSV,JSPL, JI, JJ  ! Loop index
REAL    :: ZTSTEP_PPM ! Sub Time step 
LOGICAL :: GTKE
!
INTEGER                     :: IINFO_ll    ! return code of parallel routine
TYPE(LIST_ll), POINTER      :: TZFIELDS0_ll ! list of fields to exchange
TYPE(LIST_ll), POINTER      :: TZFIELDS1_ll ! list of fields to exchange
!
!
INTEGER             :: IRESP        ! Return code of FM routines
INTEGER             :: ILUOUT       ! logical unit
INTEGER             :: ISPLIT_PPM   ! temporal time splitting 
INTEGER             :: IIB, IIE, IJB, IJE,IKB,IKE
TYPE(TFIELDDATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
!*       0.     INITIALIZATION                        
!	        --------------
!
ILUOUT = TLUOUT%NLU
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
IKB=1+JPVEXT
IKE=SIZE(PSVT,3) - JPVEXT

!
GTKE=(SIZE(PTKET)/=0)
!
!
IF(LBLOWSNOW) THEN    ! Put 2D Canopy blowing snow variables into a 3D array for advection
  ZSNWC_INIT = 0.
  ZRSNWCS = 0.

  DO JSV=1,(NBLOWSNOW_2D)
     ZSNWC_INIT(:,:,IKB,JSV) = XSNWCANO(:,:,JSV)
     ZRSNWCS(:,:,IKB,JSV)    = XRSNWCANOS(:,:,JSV)
  END DO
ENDIF
!
!
!-------------------------------------------------------------------------------
!
!*       2.     COMPUTES THE CONTRAVARIANT COMPONENTS (FOR PPM ONLY)
!	        --------------------------------------
!
!*       2.1 computes contravariant components
!
IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN
 CALL CONTRAV (HLBCX,HLBCY,PUT,PVT,PWT,PDXX,PDYY,PDZZ,PDZX,PDZY,ZRUCPPM,ZRVCPPM,ZRWCPPM,2)
ELSE
 CALL CONTRAV (HLBCX,HLBCY,PUT,PVT,PWT,PDXX,PDYY,PDZZ,PDZX,PDZY,ZRUCPPM,ZRVCPPM,ZRWCPPM,4)
END IF
!
!
!*       2.2 computes CFL numbers
!

IF (.NOT. L1D) THEN
  ZCFLU = 0.0 ; ZCFLV = 0.0 ;  ZCFLW = 0.0
  ZCFLU(IIB:IIE,IJB:IJE,:) = ABS(ZRUCPPM(IIB:IIE,IJB:IJE,:) * PTSTEP)
  ZCFLV(IIB:IIE,IJB:IJE,:) = ABS(ZRVCPPM(IIB:IIE,IJB:IJE,:) * PTSTEP)
  ZCFLW(IIB:IIE,IJB:IJE,:) = ABS(ZRWCPPM(IIB:IIE,IJB:IJE,:) * PTSTEP)
  IF (.NOT. L2D) THEN
    ZCFL  = SQRT(ZCFLU**2+ZCFLV**2+ZCFLW**2)
  ELSE
    ZCFL  = SQRT(ZCFLU**2+ZCFLW**2)
  END IF
ELSE
   ZCFLU = 0.0 ; ZCFLV = 0.0 ;  ZCFLW = 0.0 
   ZCFLW(IIB:IIE,IJB:IJE,:) = ABS(ZRWCPPM(IIB:IIE,IJB:IJE,:) * PTSTEP)
   ZCFL = SQRT(ZCFLW**2)
END IF
!
!* prints in the file the 3D Courant numbers (one should flag this)
!
IF (OCLOSE_OUT .AND. OCFL_WRIT .AND. (.NOT. L1D)) THEN
    TZFIELD%CMNHNAME   = 'CFLU'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'CFLU'
    TZFIELD%CUNITS     = '1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = 'X_Y_Z_CFLU'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZCFLU)
!
  IF (.NOT. L2D) THEN
    TZFIELD%CMNHNAME   = 'CFLV'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'CFLV'
    TZFIELD%CUNITS     = '1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = 'X_Y_Z_CFLV'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZCFLV)
  END IF
!
    TZFIELD%CMNHNAME   = 'CFLW'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'CFLW'
    TZFIELD%CUNITS     = '1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = 'X_Y_Z_CFLW'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZCFLW)
!
    TZFIELD%CMNHNAME   = 'CFL'
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = 'CFL'
    TZFIELD%CUNITS     = '1'
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = 'X_Y_Z_CFL'
    TZFIELD%NGRID      = 1
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 3
    TZFIELD%LTIMEDEP   = .TRUE.
    CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZCFL)
END IF
!
!* prints in the output file the maximum CFL
!
ZCFLU_MAX = MAX_ll(ZCFLU,IINFO_ll)
ZCFLV_MAX = MAX_ll(ZCFLV,IINFO_ll)
ZCFLW_MAX = MAX_ll(ZCFLW,IINFO_ll)
ZCFL_MAX  = MAX_ll(ZCFL,IINFO_ll)
!
WRITE(ILUOUT,FMT='(A24,F10.2,A5,F10.2,A5,F10.2,A9,F10.2)') &
                'Max. CFL number for U : ',ZCFLU_MAX,  &
                '  V : ',ZCFLV_MAX,'  W : ', ZCFLW_MAX,&
                'global : ',ZCFL_MAX
!
!
!*       2.3 updates time step splitting loop
!
IF (OSPLIT_CFL .AND. (.NOT.L1D)  ) THEN
!
 ISPLIT_PPM = INT(ZCFL_MAX/PSPLIT_CFL)+1
 IF ( KSPLIT /= ISPLIT_PPM )                                    &
 WRITE(ILUOUT,FMT='(A37,I2,A4,I2,A11)')                         &
                  'PPM  time spliting loop changed from ',      &
                  KSPLIT,' to ',ISPLIT_PPM, ' iterations'
!
 KSPLIT =     ISPLIT_PPM                      
!
END IF
! ---------------------------------------------------------------
IF (( (ZCFLU_MAX>=3.) .AND. (.NOT.L1D) ) .OR. &
    ( (ZCFLV_MAX>=3.) .AND. (.NOT.L1D) .AND. (.NOT.L2D) ) .OR. &
    ( (ZCFLW_MAX>=8.) .AND. (.NOT.L1D) ) ) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) ' +---------------------------------------------------+'
  WRITE(ILUOUT,*) ' |                   MODEL ERROR                     |'
  WRITE(ILUOUT,*) ' +---------------------------------------------------+'
  WRITE(ILUOUT,*) ' |                                                   |'
  WRITE(ILUOUT,*) ' |       The model wind speed becomes too high       |'
  WRITE(ILUOUT,*) ' |                                                   |'
  IF ( ZCFLU_MAX>=3. .OR. ZCFLV_MAX>=3. ) &
  WRITE(ILUOUT,*) ' |    The  horizontal CFL value reaches 3. or more   |'
  IF ( ZCFLW_MAX>=8.                    ) &
  WRITE(ILUOUT,*) ' |    The  vertical   CFL value reaches 8. or more   |'
  WRITE(ILUOUT,*) ' |                                                   |'
  WRITE(ILUOUT,*) ' |    This can be due either to :                    |'
  WRITE(ILUOUT,*) ' |     - a numerical explosion of the model          |'
  WRITE(ILUOUT,*) ' |     - or a too high wind speed for an             |'
  WRITE(ILUOUT,*) ' |       acceptable accuracy of the advection        |'
  WRITE(ILUOUT,*) ' |                                                   |'
  WRITE(ILUOUT,*) ' |        Please decrease your time-step             |'
  WRITE(ILUOUT,*) ' |                                                   |'
  WRITE(ILUOUT,*) ' +---------------------------------------------------+'
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) ' +---------------------------------------------------+'
  WRITE(ILUOUT,*) ' |                   MODEL STOPS                     |'
  WRITE(ILUOUT,*) ' +---------------------------------------------------+'
  CALL PRINT_MSG(NVERB_FATAL,'GEN','ADVECTION_METSV','')
END IF
!
!
ZTSTEP_PPM = PTSTEP / REAL(KSPLIT)
!
!
!*      2.4 normalized contravariant components for splitted PPM time-step
!
ZRUCPPM = ZRUCPPM*ZTSTEP_PPM
ZRVCPPM = ZRVCPPM*ZTSTEP_PPM
ZRWCPPM = ZRWCPPM*ZTSTEP_PPM
!
!
!-------------------------------------------------------------------------------
!
!
!*       3.     COMPUTES THE TENDENCIES SINCE THE BEGINNING OF THE TIME STEP
!	        ------------------------------------------------------------
!
!* This represent the effects of all OTHER processes
!  Clouds    related processes from previous time-step are     taken into account in PRTHS_CLD
!  Advection related processes from previous time-step will be taken into account in ZRTHS_PPM
!
ZRTHS_OTHER = PRTHS - PTHT * PRHODJ / PTSTEP
IF (GTKE) ZRTKES_OTHER = PRTKES - PTKET * PRHODJ / PTSTEP                      
DO JR = 1, KRR
 ZRRS_OTHER(:,:,:,JR) = PRRS(:,:,:,JR) - PRT(:,:,:,JR) * PRHODJ(:,:,:) / PTSTEP
END DO
DO JSV = 1, KSV
 ZRSVS_OTHER(:,:,:,JSV) = PRSVS(:,:,:,JSV) - PSVT(:,:,:,JSV) * PRHODJ / PTSTEP
END DO
IF(LBLOWSNOW) THEN
   DO JSV = 1, (NBLOWSNOW_2D)        
     ZRSNWCS_OTHER(:,:,:,JSV) = ZRSNWCS(:,:,:,JSV) - ZSNWC_INIT(:,:,:,JSV) * PRHODJ / PTSTEP
   END DO   
ENDIF
!
! Top and bottom Boundaries 
!
CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZRTHS_OTHER)
IF (GTKE) CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZRTKES_OTHER)
DO JR = 1, KRR
  CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZRRS_OTHER(:,:,:,JR))
END DO
DO JSV = 1, KSV
  CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZRSVS_OTHER(:,:,:,JSV))
END DO
IF(LBLOWSNOW) THEN
  DO JSV = 1, (NBLOWSNOW_2D)        
    CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZRSNWCS_OTHER(:,:,:,JSV))
  END DO 
END IF
!
! Exchanges on processors
!
NULLIFY(TZFIELDS0_ll)
!!$IF(NHALO == 1) THEN
  CALL ADD3DFIELD_ll(TZFIELDS0_ll, ZRTHS_OTHER)
  IF (GTKE) CALL ADD3DFIELD_ll(TZFIELDS0_ll, ZRTKES_OTHER)
  DO JR=1,KRR
    CALL ADD3DFIELD_ll(TZFIELDS0_ll, ZRRS_OTHER(:,:,:,JR))
  END DO
  DO JSV=1,KSV
    CALL ADD3DFIELD_ll(TZFIELDS0_ll, ZRSVS_OTHER(:,:,:,JSV))
  END DO
  IF(LBLOWSNOW) THEN
    DO JSV = 1, (NBLOWSNOW_2D)  
      CALL ADD3DFIELD_ll(TZFIELDS0_ll, ZRSNWCS_OTHER(:,:,:,JSV))
    END DO
  END IF
  CALL UPDATE_HALO_ll(TZFIELDS0_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS0_ll)
!!$END IF
!
!

!-------------------------------------------------------------------------------
!
!*       4.     CALLS THE PPM ADVECTION INSIDE A TIME SPLITTING         
!	        --------------------------------------
!
CALL PPM_RHODJ(HLBCX,HLBCY, ZRUCPPM, ZRVCPPM, ZRWCPPM,              &
               ZTSTEP_PPM, PRHODJ, ZRHOX1, ZRHOX2, ZRHOY1, ZRHOY2,  &
               ZRHOZ1, ZRHOZ2                                       )
!
!* values of the fields at the beginning of the time splitting loop
ZTH   = PTHT
ZTKE   = PTKET
IF (KRR /=0 ) ZR    = PRT
IF (KSV /=0 ) ZSV   = PSVT
IF(LBLOWSNOW) THEN
    DO JSV = 1, (NBLOWSNOW_2D)
        ZSNWC(:,:,:,JSV) = ZRSNWCS(:,:,:,JSV)* PTSTEP/ PRHODJ
        CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZSNWC(:,:,:,JSV))
    END DO
    ZSNWC_INIT=ZSNWC
ENDIF
!
IF (GTKE)    PRTKES_ADV(:,:,:)  = 0.              
!
!* time splitting loop
DO JSPL=1,KSPLIT
!
   !ZRTHS_PPM(:,:,:)   = 0.
   !ZRTKES_PPM(:,:,:)   = 0.
   !IF (KRR /=0) ZRRS_PPM(:,:,:,:)   = 0.
   !IF (KSV /=0) ZRSVS_PPM(:,:,:,:)   = 0.
!
   IF (LNEUTRAL) ZTH=ZTH-PTHVREF  !* To be removed with the new PPM scheme ?
   CALL PPM_MET (HLBCX,HLBCY, KRR, TPDTCUR,ZRUCPPM, ZRVCPPM, ZRWCPPM, PTSTEP,ZTSTEP_PPM, &
              PRHODJ,  ZRHOX1, ZRHOX2, ZRHOY1, ZRHOY2,  ZRHOZ1, ZRHOZ2,                  &
              ZTH, ZTKE, ZR, ZRTHS_PPM, ZRTKES_PPM, ZRRS_PPM, HMET_ADV_SCHEME)
   IF (LNEUTRAL) ZTH=ZTH+PTHVREF  !* To be removed with the new PPM scheme ?
!
   CALL PPM_SCALAR (HLBCX,HLBCY, KSV, TPDTCUR, ZRUCPPM, ZRVCPPM, ZRWCPPM, PTSTEP,     &
                 ZTSTEP_PPM, PRHODJ, ZRHOX1, ZRHOX2, ZRHOY1, ZRHOY2,  ZRHOZ1, ZRHOZ2, &
                 ZSV, ZRSVS_PPM, HSV_ADV_SCHEME                                       )
!
! Tendencies of PPM
!
   PRTHS(:,:,:)                      = PRTHS     (:,:,:)   + ZRTHS_PPM (:,:,:)   / KSPLIT
   IF (GTKE)     PRTKES_ADV(:,:,:)   = PRTKES_ADV(:,:,:)   + ZRTKES_PPM(:,:,:)   / KSPLIT
   IF (KRR /=0)  PRRS      (:,:,:,:) = PRRS      (:,:,:,:) + ZRRS_PPM  (:,:,:,:) / KSPLIT
   IF (KSV /=0 ) PRSVS     (:,:,:,:) = PRSVS     (:,:,:,:) + ZRSVS_PPM (:,:,:,:) / KSPLIT
!
   IF (JSPL<KSPLIT) THEN
!
!  Guesses of the field inside the time splitting loop
!
   ZTH = ZTH + ( ZRTHS_PPM(:,:,:) + ZRTHS_OTHER(:,:,:) + PRTHS_CLD(:,:,:)) * &
           ZTSTEP_PPM / PRHODJ(:,:,:)
   IF (GTKE) ZTKE = ZTKE + ( ZRTKES_PPM(:,:,:) + ZRTKES_OTHER(:,:,:) ) * ZTSTEP_PPM / PRHODJ(:,:,:)
   DO JR = 1, KRR
    ZR(:,:,:,JR) = ZR(:,:,:,JR) + ( ZRRS_PPM(:,:,:,JR) + ZRRS_OTHER(:,:,:,JR) + PRRS_CLD(:,:,:,JR) ) &
                    * ZTSTEP_PPM / PRHODJ(:,:,:)
   END DO
   DO JSV = 1, KSV
    ZSV(:,:,:,JSV) = ZSV(:,:,:,JSV) + ( ZRSVS_PPM(:,:,:,JSV) + ZRSVS_OTHER(:,:,:,JSV) +  &
                     PRSVS_CLD(:,:,:,JSV) ) * ZTSTEP_PPM / PRHODJ(:,:,:)
   END DO
!
! Top and bottom Boundaries and LBC for the guesses
!
   CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZTH, PTHT )    
    IF (GTKE) CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZTKE, PTKET)
   DO JR = 1, KRR
     CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZR(:,:,:,JR), PRT(:,:,:,JR))
   END DO
   DO JSV = 1, KSV
     CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZSV(:,:,:,JSV), PSVT(:,:,:,JSV))
   END DO

   IF(LBLOWSNOW) THEN ! Advection of Canopy mass at the 1st atmospheric level
      ZRSNWCS_PPM(:,:,:,:) = 0.
   !

      CALL PPM_SCALAR (HLBCX,HLBCY, NBLOWSNOW_2D, TPDTCUR, ZRUCPPM, ZRVCPPM, ZRWCPPM,PTSTEP,    &
                 ZTSTEP_PPM, PRHODJ, ZRHOX1, ZRHOX2, ZRHOY1, ZRHOY2,  ZRHOZ1, ZRHOZ2,          &
                 ZSNWC, ZRSNWCS_PPM, HSV_ADV_SCHEME)


! Tendencies of PPM
      ZRSNWCS(:,:,:,:)        =    ZRSNWCS(:,:,:,:)  + ZRSNWCS_PPM (:,:,:,:)   / KSPLIT
!  Guesses of the field inside the time splitting loop 
      DO JSV = 1, ( NBLOWSNOW_2D)
          ZSNWC(:,:,:,JSV) = ZSNWC(:,:,:,JSV) + ZRSNWCS_PPM(:,:,:,JSV)*ZTSTEP_PPM/ PRHODJ(:,:,:)  
      END DO

! Top and bottom Boundaries and LBC for the guesses      
      DO JSV = 1, (NBLOWSNOW_2D)
          CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZSNWC(:,:,:,JSV), ZSNWC_INIT(:,:,:,JSV))
      END DO
   END IF   
!
!  Exchanges fields between processors
!
   NULLIFY(TZFIELDS1_ll)
!!$   IF(NHALO == 1) THEN
    CALL ADD3DFIELD_ll(TZFIELDS1_ll, ZTH)
    IF (GTKE) CALL ADD3DFIELD_ll(TZFIELDS1_ll, ZTKE)
    DO JR=1,KRR
      CALL ADD3DFIELD_ll(TZFIELDS1_ll, ZR(:,:,:,JR))
    END DO
    DO JSV=1,KSV
      CALL ADD3DFIELD_ll(TZFIELDS1_ll, ZSV(:,:,:,JSV))
    END DO
    IF(LBLOWSNOW) THEN
       DO JSV=1,(NBLOWSNOW_2D)
         CALL ADD3DFIELD_ll(TZFIELDS1_ll, ZSNWC(:,:,:,JSV))
       END DO
    END IF
    CALL UPDATE_HALO_ll(TZFIELDS1_ll,IINFO_ll)
    CALL CLEANLIST_ll(TZFIELDS1_ll)
!!$   END IF
   END IF
!
END DO
!
!-------------------------------------------------------------------------------
!
!  TKE special case: advection is the last process for TKE
!
! TKE must be greater than its minimum value
! (previously done in tke_eps_sources)
!
IF (GTKE) THEN
   PRTKES(:,:,:)  = PRTKES(:,:,:) + PRTKES_ADV(:,:,:)
   PRTKES(:,:,:) = MAX (PRTKES(:,:,:) , XTKEMIN * PRHODJ(:,:,:) / PTSTEP )
END IF
!
!
!-------------------------------------------------------------------------------
! Update tendency for cano variables : from 3D to 2D
! 
IF(LBLOWSNOW) THEN

    DO JSV=1,(NBLOWSNOW_2D) 
       DO JI=1,SIZE(PSVT,1)
         DO JJ=1,SIZE(PSVT,2)
             XRSNWCANOS(JI,JJ,JSV) = SUM(ZRSNWCS(JI,JJ,IKB:IKE,JSV))
         END DO
       END DO
    END DO
IF(LWEST_ll())  XRSNWCANOS(IIB,:,:)  = ZRSNWCS(IIB,:,IKB,:)
IF(LEAST_ll())  XRSNWCANOS(IIE,:,:)  = ZRSNWCS(IIE,:,IKB,:)
IF(LSOUTH_ll()) XRSNWCANOS(:,IJB,:)  = ZRSNWCS(:,IJB,IKB,:)
IF(LNORTH_ll()) XRSNWCANOS(:,IJE,:)  = ZRSNWCS(:,IJE,IKB,:)

END IF
!-------------------------------------------------------------------------------
!
!*       5.     BUDGETS                                                 
!	        -------
!
IF (LBUDGET_TH)  CALL BUDGET (PRTHS,4,'ADV_BU_RTH')
IF (LBUDGET_TKE) CALL BUDGET (PRTKES,5,'ADV_BU_RTKE')
IF (KRR>=1.AND.LBUDGET_RV) CALL BUDGET (PRRS(:,:,:,1),6,'ADV_BU_RRV') 
IF (KRR>=2.AND.LBUDGET_RC) CALL BUDGET (PRRS(:,:,:,2),7,'ADV_BU_RRC') 
IF (KRR>=3.AND.LBUDGET_RR) CALL BUDGET (PRRS(:,:,:,3),8,'ADV_BU_RRR') 
IF (KRR>=4.AND.LBUDGET_RI) CALL BUDGET (PRRS(:,:,:,4),9,'ADV_BU_RRI') 
IF (KRR>=5.AND.LBUDGET_RS) CALL BUDGET (PRRS(:,:,:,5),10,'ADV_BU_RRS') 
IF (KRR>=6.AND.LBUDGET_RG) CALL BUDGET (PRRS(:,:,:,6),11,'ADV_BU_RRG') 
IF (KRR>=7.AND.LBUDGET_RH) CALL BUDGET (PRRS(:,:,:,7),12,'ADV_BU_RRH')
DO JSV=1,KSV
  IF (LBUDGET_SV) CALL BUDGET (PRSVS(:,:,:,JSV),JSV+12,'ADV_BU_RSV')
END DO
!
SELECT CASE ( HCLOUD )
  CASE('ICE3','ICE4')
     ZEXN(:,:,:)= (PPABST(:,:,:)/XP00)**(XRD/XCPD)
     ZT(:,:,:)= PTHT(:,:,:)*ZEXN(:,:,:)
     ZLV(:,:,:)=XLVTT +(XCPV-XCL) *(ZT(:,:,:)-XTT)
     ZLS(:,:,:)=XLSTT +(XCPV-XCI) *(ZT(:,:,:)-XTT)
     ZCPH(:,:,:)=XCPD +XCPV*PRT(:,:,:,1)
    WHERE (PRRS(:,:,:,4) < 0.)
      PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,4)
      PRTHS(:,:,:) = PRTHS(:,:,:) - PRRS(:,:,:,4) * ZLS(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRRS(:,:,:,4) = 0.
    END WHERE
!
!   cloud
    WHERE (PRRS(:,:,:,2) < 0.)
      PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,2)
      PRTHS(:,:,:) = PRTHS(:,:,:) - PRRS(:,:,:,2) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRRS(:,:,:,2) = 0.
    END WHERE
!
! if rc or ri are positive, we can correct negative rv
!   cloud
    WHERE ((PRRS(:,:,:,1) <0.) .AND. (PRRS(:,:,:,2)> 0.) )
      PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,2)
      PRTHS(:,:,:) = PRTHS(:,:,:) - PRRS(:,:,:,2) * ZLV(:,:,:) /  &
           ZCPH(:,:,:) / ZEXN(:,:,:)
      PRRS(:,:,:,2) = 0.
    END WHERE
!   ice
    IF(KRR > 3) THEN
      WHERE ((PRRS(:,:,:,1) < 0.).AND.(PRRS(:,:,:,4) > 0.))
        ZCOR(:,:,:)=MIN(-PRRS(:,:,:,1),PRRS(:,:,:,4))
        PRRS(:,:,:,1) = PRRS(:,:,:,1) + ZCOR(:,:,:)
        PRTHS(:,:,:) = PRTHS(:,:,:) - ZCOR(:,:,:) * ZLS(:,:,:) /  &
             ZCPH(:,:,:) / ZEXN(:,:,:)
        PRRS(:,:,:,4) = PRRS(:,:,:,4) -ZCOR(:,:,:)
      END WHERE
    END IF
!
  CASE('C2R2','KHKO')
     ZEXN(:,:,:)= (PPABST(:,:,:)/XP00)**(XRD/XCPD)
     ZT(:,:,:)= PTHT(:,:,:)*ZEXN(:,:,:)
     ZLV(:,:,:)=XLVTT +(XCPV-XCL) *(ZT(:,:,:)-XTT)
     ZLS(:,:,:)=XLSTT +(XCPV-XCI) *(ZT(:,:,:)-XTT)
     ZCPH(:,:,:)=XCPD +XCPV*PRT(:,:,:,1)
!  CALL GET_HALO(PRRS(:,:,:,2))
!  CALL GET_HALO(PRSVS(:,:,:,2))
!  CALL GET_HALO(PRSVS(:,:,:,3))
     WHERE (PRRS(:,:,:,2) < 0. .OR. PRSVS(:,:,:,2) < 0.)
        PRSVS(:,:,:,1) = 0.0
     END WHERE
     DO JSV = 2, 3
        WHERE (PRRS(:,:,:,JSV) < 0. .OR. PRSVS(:,:,:,JSV) < 0.)
           PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,JSV)
           PRTHS(:,:,:) = PRTHS(:,:,:) - PRRS(:,:,:,JSV) * ZLV(:,:,:) /  &
                ZCPH(:,:,:) / ZEXN(:,:,:)
           PRRS(:,:,:,JSV)  = 0.0
           PRSVS(:,:,:,JSV) = 0.0
        END WHERE
     END DO
     !
   CASE('LIMA')   
     ZEXN(:,:,:)= (PPABST(:,:,:)/XP00)**(XRD/XCPD)
     ZT(:,:,:)= PTHT(:,:,:)*ZEXN(:,:,:)
     ZLV(:,:,:)=XLVTT +(XCPV-XCL) *(ZT(:,:,:)-XTT)
     ZLS(:,:,:)=XLSTT +(XCPV-XCI) *(ZT(:,:,:)-XTT)
     ZCPH(:,:,:)=XCPD +XCPV*PRT(:,:,:,1)
! Correction where rc<0 or Nc<0
      IF (LWARM) THEN
         WHERE (PRRS(:,:,:,2) < XRTMIN(2)/PTSTEP .OR. PRSVS(:,:,:,NSV_LIMA_NC) < XCTMIN(2)/PTSTEP)
            PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,2)
            PRTHS(:,:,:) = PRTHS(:,:,:) - PRRS(:,:,:,2) * ZLV(:,:,:) /  &
                 ZCPH(:,:,:) / ZEXN(:,:,:)
            PRRS(:,:,:,2)  = 0.0
            PRSVS(:,:,:,NSV_LIMA_NC) = 0.0
         END WHERE
      END IF
! Correction where rr<0 or Nr<0
      IF (LWARM .AND. LRAIN) THEN
         WHERE (PRRS(:,:,:,3) < XRTMIN(3)/PTSTEP .OR. PRSVS(:,:,:,NSV_LIMA_NR) < XCTMIN(3)/PTSTEP)
            PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,3)
            PRTHS(:,:,:) = PRTHS(:,:,:) - PRRS(:,:,:,3) * ZLV(:,:,:) /  &
                 ZCPH(:,:,:) / ZEXN(:,:,:)
            PRRS(:,:,:,3)  = 0.0
            PRSVS(:,:,:,NSV_LIMA_NR) = 0.0
         END WHERE
      END IF
! Correction where ri<0 or Ni<0
      IF (LCOLD) THEN
         WHERE (PRRS(:,:,:,4) < XRTMIN(4)/PTSTEP .OR. PRSVS(:,:,:,NSV_LIMA_NI) < XCTMIN(4)/PTSTEP)
            PRRS(:,:,:,1) = PRRS(:,:,:,1) + PRRS(:,:,:,4)
            PRTHS(:,:,:) = PRTHS(:,:,:) - PRRS(:,:,:,4) * ZLS(:,:,:) /  &
                 ZCPH(:,:,:) / ZEXN(:,:,:)
            PRRS(:,:,:,4)  = 0.0
            PRSVS(:,:,:,NSV_LIMA_NI) = 0.0
         END WHERE
      END IF
!
      PRSVS(:,:,:,:) = MAX( 0.0,PRSVS(:,:,:,:) )
      PRRS(:,:,:,:) = MAX( 0.0,PRRS(:,:,:,:) )
!
END SELECT
!
IF ((HCLOUD == 'ICE3') .OR. (HCLOUD == 'ICE4') .OR. (HCLOUD == 'KHKO') .OR. (HCLOUD == 'C2R2') .OR. (HCLOUD == 'LIMA') ) THEN
  IF (LBUDGET_TH) CALL BUDGET (PRTHS(:,:,:) , 4,'NEADV_BU_RTH')
  IF (LBUDGET_RV) CALL BUDGET (PRRS(:,:,:,1), 6,'NEADV_BU_RRV')
  IF (LBUDGET_RC) CALL BUDGET (PRRS(:,:,:,2), 7,'NEADV_BU_RRC')
  IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:,3), 8,'NEADV_BU_RRR')
  IF (LBUDGET_RI) CALL BUDGET (PRRS(:,:,:,4), 9,'NEADV_BU_RRI')
  IF (LBUDGET_RS) CALL BUDGET (PRRS(:,:,:,5),10,'NEADV_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (PRRS(:,:,:,6),11,'NEADV_BU_RRG')
  IF (LBUDGET_RH) CALL BUDGET (PRRS(:,:,:,7),12,'NEADV_BU_RRH')
END IF
IF (LBUDGET_SV .AND. (HCLOUD == 'LIMA')) THEN
   IF (LWARM) CALL BUDGET (PRSVS(:,:,:,NSV_LIMA_NC),12+NSV_LIMA_NC,'NEADV_BU_RSV')
   IF (LWARM.AND.LRAIN) CALL BUDGET (PRSVS(:,:,:,NSV_LIMA_NR),12+NSV_LIMA_NR,'NEADV_BU_RSV')
   IF (LCOLD) CALL BUDGET (PRSVS(:,:,:,NSV_LIMA_NI),12+NSV_LIMA_NI,'NEADV_BU_RSV')
   IF (NMOD_CCN.GE.1) THEN
      DO JI=1, NMOD_CCN
         CALL BUDGET ( PRSVS(:,:,:,NSV_LIMA_CCN_FREE+JI-1),12+NSV_LIMA_CCN_FREE+JI-1,'NEADV_BU_RSV') 
      END DO
   END IF
   IF (NMOD_IFN.GE.1) THEN
      DO JI=1, NMOD_IFN
         CALL BUDGET ( PRSVS(:,:,:,NSV_LIMA_IFN_FREE+JI-1),12+NSV_LIMA_IFN_FREE+JI-1,'NEADV_BU_RSV') 
      END DO
   END IF
END IF

!-------------------------------------------------------------------------------
!
END SUBROUTINE ADVECTION_METSV
