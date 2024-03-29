!MNH_LIC Copyright 2003-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_INI_RADIATIONS
!     ##########################
!
INTERFACE
!
    SUBROUTINE INI_RADIATIONS(TPINIFILE,OINIRAD,TPDTCUR,TPDTEXP,       &
         PZZ,PDXX,PDYY,                                                &
         PSINDEL,PCOSDEL,PTSIDER,PCORSOL,PSLOPANG,PSLOPAZI,            &
         PDTHRAD,PDIRFLASWD,PSCAFLASWD,                                &
         PFLALWD,PDIRSRFSWD,KCLEARCOL_TM1,                             &
         PZENITH, PAZIM, TPDTRAD_FULL,TPDTRAD_CLONLY,TPINITHALO2D_ll,  &
         PRADEFF,PSWU,PSWD,PLWU,PLWD,PDTHRADSW,PDTHRADLW,              &
         KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG,     &
         KRAD_AGG_FLAG                                                 )
!
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
USE MODD_IO,       ONLY : TFILEDATA
USE MODD_TYPE_DATE
!
TYPE(TFILEDATA),        INTENT(IN)  :: TPINIFILE ! Initial file
LOGICAL,                INTENT(IN)  :: OINIRAD   ! switch to initialize or read
                                                 ! the radiation informations
TYPE (DATE_TIME),       INTENT(IN) :: TPDTCUR    ! Current date and time
TYPE (DATE_TIME),       INTENT(IN) :: TPDTEXP    ! Current date and time
                                                 ! Ajout PP
REAL, DIMENSION(:,:,:), INTENT(IN) :: PZZ        ! height z
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDXX,PDYY  ! metric coefficients
REAL,                   INTENT(OUT):: PSINDEL    ! sine   of the solar declination angle
REAL,                   INTENT(OUT):: PCOSDEL    ! cosine of the solar declination angle
REAL,                   INTENT(OUT):: PTSIDER    ! sideral decimal time correction
REAL,                   INTENT(OUT):: PCORSOL    ! daily solar constant correction
REAL, DIMENSION(:,:),   INTENT(OUT):: PSLOPANG   ! slope angle
REAL, DIMENSION(:,:),   INTENT(OUT):: PSLOPAZI   ! azimuthal slope angle
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDTHRAD    ! radiative tendency
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDIRFLASWD ! Direct    Short Wave flux at the flat surface
REAL, DIMENSION(:,:,:), INTENT(OUT):: PSCAFLASWD ! Scaterred Short Wave flux at the flat surface
REAL, DIMENSION(:,:),   INTENT(OUT):: PFLALWD    ! Long Wave flux at the surface
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDIRSRFSWD ! Direct    Short Wave flux at the surface
INTEGER, DIMENSION(:,:),INTENT(OUT):: KCLEARCOL_TM1 ! trace of cloud col at the previous
                                                    ! radiation step
!
REAL, DIMENSION(:,:), INTENT(INOUT):: PZENITH        ! Solar zenithal angle
REAL, DIMENSION(:,:), INTENT(INOUT):: PAZIM          ! Solar azimuthal angle
TYPE (DATE_TIME),       INTENT(OUT):: TPDTRAD_FULL   ! date and time of the 
                                                     ! last full radiation call
TYPE (DATE_TIME),       INTENT(OUT):: TPDTRAD_CLONLY ! date and time of the 
                         ! last radiation call only for the cloudy verticals
TYPE(LIST_ll), POINTER             :: TPINITHALO2D_ll! pointer for the list of fields
                                                     !  which must be communicated in INIT
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PSWU ! upward SW Flux 
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PSWD ! downward SW Flux 
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PLWU ! upward LW Flux 
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PLWD ! downward LW Flux 
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PDTHRADSW !  dthrad sw
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PDTHRADLW !  dthrad lw
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PRADEFF ! effective radius
!
!
INTEGER, INTENT(IN)  :: KRAD_AGG      ! number of aggregated points
INTEGER, INTENT(OUT) :: KI_RAD_AGG    ! reformatted X array size
INTEGER, INTENT(OUT) :: KJ_RAD_AGG    ! reformatted Y array size
INTEGER, INTENT(OUT) :: KIOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, INTENT(OUT) :: KJOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KRAD_AGG_FLAG  ! flag to know if aggregated column is computed in this processor or another one

END SUBROUTINE INI_RADIATIONS
!
END INTERFACE
!
END MODULE MODI_INI_RADIATIONS
!
!
!   ####################################################################
    SUBROUTINE INI_RADIATIONS(TPINIFILE,OINIRAD,TPDTCUR,TPDTEXP,       &
         PZZ,PDXX,PDYY,                                                &
         PSINDEL,PCOSDEL,PTSIDER,PCORSOL,PSLOPANG,PSLOPAZI,            &
         PDTHRAD,PDIRFLASWD,PSCAFLASWD,                                &
         PFLALWD,PDIRSRFSWD,KCLEARCOL_TM1,                             &
         PZENITH, PAZIM, TPDTRAD_FULL,TPDTRAD_CLONLY,TPINITHALO2D_ll,  &
         PRADEFF,PSWU,PSWD,PLWU,PLWD,PDTHRADSW,PDTHRADLW,              &
         KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG,     &
         KRAD_AGG_FLAG                                                 )
!   ####################################################################
!
!!****  *INI_RADIATION_TIME * - initialisation for radiation scheme in the MesoNH framework
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!! 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!  	V. Masson        * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2003  split of ini_radiations. externalization of ISBA
!!      O.Thouron   06/2008  Add diagnostics
!!      P. Peyrille, M. Tomasini 06/2012:  if LFIX_DAT=T, TDTCUR is replaced by
!!                               TDTEXP to  have a perpetual day ie. the diurnal cycle is retained 
!!                               but the day stays the same during the whole run 
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 14/02/2019: remove CLUOUT/CLUOUT0 and associated variables
!  P. Wautelet 26/04/2019: replace non-standard FLOAT function by REAL function
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!       V. Masson 03/01/2024: aggregation of columns for radiation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!MESO-NH modules
!
USE MODD_ARGSLIST_ll,   ONLY: LIST_ll
USE MODD_CST,           ONLY: XPI
USE MODD_CONF,          ONLY: LFLAT, L2D
USE MODD_IO,            ONLY: TFILEDATA
USE MODD_LES
USE MODD_PARAMETERS,    ONLY: JPVEXT
USE MODD_PARAM_RAD_n,   ONLY: LFIX_DAT
USE MODD_TYPE_DATE
!
USE MODE_IO_FIELD_READ, only: IO_Field_read
USE MODE_ll
!
USE MODI_SHUMAN
USE MODI_INI_RADIATIONS_AGG
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
TYPE(TFILEDATA),        INTENT(IN)  :: TPINIFILE ! Initial file
LOGICAL,                INTENT(IN)  :: OINIRAD   ! switch to initialize or read
                                                 ! the radiation informations
TYPE (DATE_TIME),       INTENT(IN) :: TPDTCUR    ! Current date and time
TYPE (DATE_TIME),       INTENT(IN) :: TPDTEXP    ! Current date and time
                                                 ! Ajout PP
REAL, DIMENSION(:,:,:), INTENT(IN) :: PZZ        ! height z
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDXX,PDYY  ! metric coefficients
REAL,                   INTENT(OUT):: PSINDEL    ! sine   of the solar declination angle
REAL,                   INTENT(OUT):: PCOSDEL    ! cosine of the solar declination angle
REAL,                   INTENT(OUT):: PTSIDER    ! sideral decimal time correction
REAL,                   INTENT(OUT):: PCORSOL    ! daily solar constant correction
REAL, DIMENSION(:,:),   INTENT(OUT):: PSLOPANG   ! slope angle
REAL, DIMENSION(:,:),   INTENT(OUT):: PSLOPAZI   ! azimuthal slope angle
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDTHRAD    ! radiative tendency
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDIRFLASWD ! Direct    Short Wave flux at the flat surface
REAL, DIMENSION(:,:,:), INTENT(OUT):: PSCAFLASWD ! Scaterred Short Wave flux at the flat surface
REAL, DIMENSION(:,:),   INTENT(OUT):: PFLALWD    ! Long Wave flux at the surface
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDIRSRFSWD ! Direct    Short Wave flux at the surface
INTEGER, DIMENSION(:,:),INTENT(OUT):: KCLEARCOL_TM1 ! trace of cloud col at the previous
                                                    ! radiation step
!
REAL, DIMENSION(:,:), INTENT(INOUT):: PZENITH        ! Solar zenithal angle
REAL, DIMENSION(:,:), INTENT(INOUT):: PAZIM          ! Solar azimuthal angle
TYPE (DATE_TIME),       INTENT(OUT):: TPDTRAD_FULL   ! date and time of the 
                                                     ! last full radiation call
TYPE (DATE_TIME),       INTENT(OUT):: TPDTRAD_CLONLY ! date and time of the 
TYPE(LIST_ll), POINTER             :: TPINITHALO2D_ll! pointer for the list of fields
                                                     !  which must be communicated in INIT
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PSWU ! upward SW Flux 
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PSWD ! downward SW Flux 
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PLWU ! upward LW Flux 
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PLWD ! downward LW Flux 
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PDTHRADSW !  dthrad sw
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PDTHRADLW !  dthrad lw
REAL, DIMENSION(:,:,:),     INTENT(OUT) :: PRADEFF ! effective radius
!
INTEGER, INTENT(IN)  :: KRAD_AGG      ! number of aggregated points
INTEGER, INTENT(OUT) :: KI_RAD_AGG    ! reformatted X array size
INTEGER, INTENT(OUT) :: KJ_RAD_AGG    ! reformatted Y array size
INTEGER, INTENT(OUT) :: KIOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, INTENT(OUT) :: KJOR_RAD_AGG  ! index of first point of packed array according to current domain
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KRAD_AGG_FLAG  ! flag to know if aggregated column is computed in this processor or another one
!
!*       0.2   declarations of local variables
!
INTEGER, DIMENSION(0:11) :: IBIS, INOBIS ! Cumulative number of days per month
                                         ! for bissextile and regular years
REAL :: ZDATE         ! Julian day of the year
REAL :: ZAD           ! Angular Julian day of the year
REAL :: ZDECSOL       ! Daily solar declination angle 
REAL :: ZA1, ZA2      ! Ancillary variables
!
INTEGER :: JI         ! loop counter
INTEGER :: IIB        ! I index value of the first inner mass point
INTEGER :: IJB        ! J index value of the first inner mass point
INTEGER :: IKB        ! K index value of the first inner mass point
INTEGER :: IIE        ! I index value of the last inner mass point
INTEGER :: IJE        ! J index value of the last inner mass point

REAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),1) :: ZSLOPX, ZSLOPY ! Terrain slopes in
                                                             ! x and y directions
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB = 1 + JPVEXT
!
!*       1.    COMPUTES THE DAY OF THE YEAR
!              ----------------------------
!
INOBIS(:) = (/0,31,59,90,120,151,181,212,243,273,304,334/)
IBIS(0) = INOBIS(0)
DO JI=1,11
  IBIS(JI) = INOBIS(JI)+1
END DO
IF ( LFIX_DAT ) THEN   ! Ajout PP 
   IF( MOD(TPDTEXP%nyear,4).EQ.0 ) THEN
    ZDATE = REAL(TPDTEXP%nday +   IBIS(TPDTEXP%nmonth-1)) - 1
     ZAD = 2.0*XPI*ZDATE/366.0
   ELSE
     ZDATE = REAL(TPDTEXP%nday + INOBIS(TPDTEXP%nmonth-1)) - 1
     ZAD = 2.0*XPI*ZDATE/365.0
   END IF
ELSE
   IF( MOD(TPDTCUR%nyear,4).EQ.0 ) THEN
     ZDATE = REAL(TPDTCUR%nday +   IBIS(TPDTCUR%nmonth-1)) - 1
     ZAD = 2.0*XPI*ZDATE/366.0
   ELSE
     ZDATE = REAL(TPDTCUR%nday + INOBIS(TPDTCUR%nmonth-1)) - 1
     ZAD = 2.0*XPI*ZDATE/365.0
   END IF
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.     COMPUTES THE SOLAR DECLINATION ANGLE
!	        ------------------------------------
!
ZDECSOL = 0.006918-0.399912*COS(ZAD)   +0.070257*SIN(ZAD)    &
         -0.006758*COS(2.*ZAD)+0.000907*SIN(2.*ZAD) &
         -0.002697*COS(3.*ZAD)+0.00148 *SIN(3.*ZAD)
PSINDEL = SIN(ZDECSOL)
PCOSDEL = COS(ZDECSOL)
!
!-------------------------------------------------------------------------------
!
!*       3.     COMPUTES THE SIDERAL HOUR CORRECTION
!	        ------------------------------------
!
ZA1 = (1.00554*ZDATE- 6.28306)*(XPI/180.0)
ZA2 = (1.93946*ZDATE+23.35089)*(XPI/180.0)
PTSIDER = (7.67825*SIN(ZA1)+10.09176*SIN(ZA2)) / 60.0
!
!-------------------------------------------------------------------------------
!
!*       4.     COMPUTES THE DAILY SOLAR CONSTANT CORRECTION
!	        --------------------------------------------
!
PCORSOL = 1.00011+0.034221*COS(ZAD)   +0.001280*SIN(ZAD)    &
                 +0.000719*COS(2.*ZAD)+0.000077*SIN(2.*ZAD)
!
!-------------------------------------------------------------------------------
!
!*       5.     COMPUTES THE SLOPE ANGLE AND THE AZIMUTHAL SLOPE ANGLE
!	        ------------------------------------------------------
!
!
IF(LFLAT) THEN
  PSLOPANG(:,:) = 0.0
  PSLOPAZI(:,:) = -0.5*XPI 
ELSE 
  !  . local computation
  ZSLOPX(:,:,:) = MXF( DXM(PZZ(:,:,IKB:IKB))/PDXX(:,:,IKB:IKB) )
  !
  IF(L2D) THEN
    PSLOPANG(:,:) = ATAN(ABS(ZSLOPX(:,:,1)))
    PSLOPAZI(:,:) = -0.5*XPI
  ELSE
    !    . local computation
    ZSLOPY(:,:,:) = MYF( DYM(PZZ(:,:,IKB:IKB))/PDYY(:,:,IKB:IKB) ) 
    PSLOPANG(:,:) = ATAN(SQRT(ZSLOPX(:,:,1)**2+ZSLOPY(:,:,1)**2))
    PSLOPAZI(:,:) = 0.0
    PSLOPAZI(:,:) = - 0.5*XPI +                       &
         ATAN2( ZSLOPY(:,:,1), ZSLOPX(:,:,1) + SIGN(1.E-30,ZSLOPX(:,:,1)) )
  END IF
END IF
!
!   5.2 Update halo of PSLOPANG and PSLOPAZI at the end of ini_modeln
!
CALL ADD2DFIELD_ll ( TPINITHALO2D_ll, PSLOPANG, 'INI_RADIATIONS::PSLOPANG' )
CALL ADD2DFIELD_ll ( TPINITHALO2D_ll, PSLOPAZI, 'INI_RADIATIONS::PSLOPAZI' )
!
!-------------------------------------------------------------------------------
!
!*        9.    INITIALIZE TIME FOR THE RADIATION CALL
!	            --------------------------------------
!
PSWU(:,:,:)   = 0.
PSWD(:,:,:)   = 0.
PLWU(:,:,:)   = 0.
PLWD(:,:,:)   = 0.
PDTHRADSW(:,:,:)   = 0.
PDTHRADLW(:,:,:)   = 0.
PRADEFF(:,:,:)   = 0.
!
IF ( OINIRAD ) THEN
  IF (LFIX_DAT ) THEN                      ! Ajout PP
     TPDTRAD_FULL   = TPDTEXP             ! Ajout PP
     TPDTRAD_CLONLY = TPDTEXP             ! Ajout PP
  ELSE                                    ! Ajout PP
     TPDTRAD_FULL     = TPDTCUR
     TPDTRAD_CLONLY   = TPDTCUR
  END IF
  PDTHRAD(:,:,:)   = 0.
  PDIRFLASWD(:,:,:)= 0.
  PSCAFLASWD(:,:,:)= 0.
  PFLALWD(:,:)     = 0.
  PDIRSRFSWD(:,:,:)= 0.
  KCLEARCOL_TM1    = 0
ELSE
  CALL IO_Field_read(TPINIFILE,'DTRAD_FULL',  TPDTRAD_FULL)
  CALL IO_Field_read(TPINIFILE,'DTRAD_CLLY',  TPDTRAD_CLONLY)
  CALL IO_Field_read(TPINIFILE,'DTHRAD',      PDTHRAD)
  CALL IO_Field_read(TPINIFILE,'FLALWD',      PFLALWD)
  CALL IO_Field_read(TPINIFILE,'DIRFLASWD',   PDIRFLASWD)
  CALL IO_Field_read(TPINIFILE,'SCAFLASWD',   PSCAFLASWD)
  CALL IO_Field_read(TPINIFILE,'DIRSRFSWD',   PDIRSRFSWD)
  CALL IO_Field_read(TPINIFILE,'CLEARCOL_TM1',KCLEARCOL_TM1)
  CALL IO_Field_read(TPINIFILE,'ZENITH',      PZENITH)
  CALL IO_Field_read(TPINIFILE,'AZIM',        PAZIM)
END IF
!
!-------------------------------------------------------------------------------
!
!*       10.         INITIALIZE COLUMN AGGREGATION FOR RADIATION CALL
!	            -------------------------------------------------

CALL INI_RADIATIONS_AGG (KRAD_AGG,KI_RAD_AGG,KJ_RAD_AGG,KIOR_RAD_AGG,KJOR_RAD_AGG,KRAD_AGG_FLAG)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_RADIATIONS
