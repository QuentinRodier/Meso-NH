!MNH_LIC Copyright 2000-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ###########################
MODULE MODE_WRITE_AIRCRAFT_BALLOON
!      ###########################

use modd_parameters, only: NCOMMENTLGTMAX, NMNHNAMELGTMAX, NUNITLGTMAX

use mode_msg

implicit none

private

PUBLIC :: AIRCRAFT_BALLOON_FREE_NONLOCAL
public :: WRITE_AIRCRAFT_BALLOON

CHARACTER(LEN=NCOMMENTLGTMAX), DIMENSION(:), ALLOCATABLE :: CCOMMENT ! comment string(
CHARACTER(LEN=NMNHNAMELGTMAX), DIMENSION(:), ALLOCATABLE :: CTITLE   ! title
CHARACTER(LEN=NUNITLGTMAX),    DIMENSION(:), ALLOCATABLE :: CUNIT    ! physical unit

REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: XWORK6   ! contains temporal serie

contains

! ##########################################
SUBROUTINE WRITE_AIRCRAFT_BALLOON(TPDIAFILE)
! ##########################################
!
!
!!****  *WRITE_AIRCRAFT_BALLOON* - write the balloon and aircraft trajectories and records
!!                      in the diachronic file
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!
!!
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/05/2000
!!     10/01/2011 adding IMI, the model number
!!     March, 2013 :  C.Lac : add vertical profiles
!!     July, 2015 (O.Nuissier/F.Duffourg) Add microphysics diagnostic for
!!                                      aircraft, ballon and profiler
!!     Oct 2016 : G.Delautier LIMA
!!     August 2016 (M.Leriche) Add mass concentration of aerosol species
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 29/01/2019: bug: moved an instruction later (to prevent access to a not allocated array)
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 09/10/2020: bugfix: correction on IPROCZ when not LIMA (condition was wrong)
!  P. Wautelet 09/10/2020: Write_diachro: use new datatype tpfields
!  P. Wautelet 03/03/2021: budgets: add tbudiachrometadata type (useful to pass more information to Write_diachro)
!  P. Wautelet 11/03/2021: budgets: remove ptrajx/y/z optional dummy arguments of Write_diachro
!  P. Wautelet 11/03/2021: bugfix: correct name for NSV_LIMA_IMM_NUCL
!  P. Wautelet 04/02/2022: use TSVLIST to manage metadata of scalar variables
!  P. Wautelet    06/2022: reorganize flyers
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_IO,               ONLY: ISP, TFILEDATA
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA), INTENT(IN) :: TPDIAFILE ! file to write
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER :: JI
!
!----------------------------------------------------------------------------

DO JI = 1, NBALLOONS
  ! The balloon data is only available on the process where it is physically located => transfer it if necessary

  ! Send data from owner to writer if necessary
  IF ( ISP == NRANKCUR_BALLOON(JI) .AND. NRANKCUR_BALLOON(JI) /= TPDIAFILE%NMASTER_RANK ) THEN
    CALL TBALLOONS(JI)%TBALLOON%SEND( KTO = TPDIAFILE%NMASTER_RANK, OSEND_SIZE_TO_RECEIVER = .TRUE. )
  END IF

  IF ( ISP == TPDIAFILE%NMASTER_RANK ) THEN
    ! Receive data from owner if not available on the writer process
    IF ( NRANKCUR_BALLOON(JI) /= TPDIAFILE%NMASTER_RANK ) THEN
      IF ( ASSOCIATED( TBALLOONS(JI)%TBALLOON ) ) &
        call Print_msg( NVERB_FATAL, 'GEN', 'WRITE_AIRCRAFT_BALLOON', 'balloon already associated' )
      ALLOCATE( TBALLOONS(JI)%TBALLOON )
      CALL TBALLOONS(JI)%TBALLOON%RECV_ALLOCATE( KFROM = NRANKCUR_BALLOON(JI), ORECV_SIZE_FROM_OWNER = .TRUE. )
    END IF

    ! Write data
    CALL FLYER_DIACHRO( TPDIAFILE, TBALLOONS(JI)%TBALLOON )

    ! Remark: release of memory is done later by a call to AIRCRAFT_BALLOON_FREE_NONLOCAL
    !         This call must be done after the file is closed because flyer data is needed on the
    !         file master process at this last stage (coordinates writing)
  END IF
END DO

DO JI = 1, NAIRCRAFTS
  ! The aircraft data is only available on the process where it is physically located => transfer it if necessary

  ! Send data from owner to writer if necessary
  IF ( ISP == NRANKCUR_AIRCRAFT(JI) .AND. NRANKCUR_AIRCRAFT(JI) /= TPDIAFILE%NMASTER_RANK ) THEN
    CALL TAIRCRAFTS(JI)%TAIRCRAFT%SEND( KTO = TPDIAFILE%NMASTER_RANK, OSEND_SIZE_TO_RECEIVER = .TRUE. )
  END IF

  IF ( ISP == TPDIAFILE%NMASTER_RANK ) THEN
    ! Receive data from owner if not available on the writer process (need to be done only for the first model)
    IF ( NRANKCUR_AIRCRAFT(JI) /= TPDIAFILE%NMASTER_RANK ) THEN
      IF ( ASSOCIATED( TAIRCRAFTS(JI)%TAIRCRAFT ) ) &
        call Print_msg( NVERB_FATAL, 'GEN', 'WRITE_AIRCRAFT_BALLOON', 'aircraft already associated' )
      ALLOCATE( TAIRCRAFTS(JI)%TAIRCRAFT )
      CALL TAIRCRAFTS(JI)%TAIRCRAFT%RECV_ALLOCATE( KFROM = NRANKCUR_AIRCRAFT(JI), ORECV_SIZE_FROM_OWNER = .TRUE. )
    END IF

    ! Write data
    CALL FLYER_DIACHRO( TPDIAFILE, TAIRCRAFTS(JI)%TAIRCRAFT )

    ! Remark: release of memory is done later by a call to AIRCRAFT_BALLOON_FREE_NONLOCAL
    !         This call must be done after the file is closed because flyer data is needed on the
    !         file master process at this last stage (coordinates writing)
  END IF
END DO

END SUBROUTINE WRITE_AIRCRAFT_BALLOON


! ####################################################
SUBROUTINE AIRCRAFT_BALLOON_FREE_NONLOCAL( TPDIAFILE )
! ####################################################

USE MODD_AIRCRAFT_BALLOON,     ONLY: NAIRCRAFTS, NBALLOONS, NRANKCUR_AIRCRAFT, NRANKCUR_BALLOON, TAIRCRAFTS, TBALLOONS
USE MODD_IO,                   ONLY: ISP, TFILEDATA

IMPLICIT NONE

TYPE(TFILEDATA), INTENT(IN) :: TPDIAFILE

INTEGER :: JI

CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'AIRCRAFT_BALLOON_FREE_NONLOCAL', 'called for ' // TRIM(TPDIAFILE%CNAME) )

IF ( ISP == TPDIAFILE%NMASTER_RANK ) THEN
  DO JI = 1, NBALLOONS
    ! Free ballon data if it was not stored on this process
    IF ( NRANKCUR_BALLOON(JI) /= TPDIAFILE%NMASTER_RANK ) THEN
      CALL TBALLOONS(JI)%TBALLOON%DATA_ARRAYS_DEALLOCATE()
      DEALLOCATE( TBALLOONS(JI)%TBALLOON )
    END IF
  END DO

  DO JI = 1, NAIRCRAFTS
    ! Free aircraft data if it was not stored on this process
    IF ( NRANKCUR_AIRCRAFT(JI) /= TPDIAFILE%NMASTER_RANK ) THEN
      CALL TAIRCRAFTS(JI)%TAIRCRAFT%DATA_ARRAYS_DEALLOCATE()
      DEALLOCATE( TAIRCRAFTS(JI)%TAIRCRAFT )
    END IF
  END DO
END IF

END SUBROUTINE AIRCRAFT_BALLOON_FREE_NONLOCAL


! ############################################
SUBROUTINE FLYER_DIACHRO( TPDIAFILE, TPFLYER )
! ############################################

USE MODD_AIRCRAFT_BALLOON
use modd_budget,           only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                                 tbudiachrometadata
USE MODD_CST,              ONLY: XRV
use modd_field,            only: NMNHDIM_LEVEL, NMNHDIM_LEVEL_W, NMNHDIM_FLYER_PROC, NMNHDIM_FLYER_TIME, NMNHDIM_UNUSED, &
                                 tfieldmetadata_base, TYPEREAL
USE MODD_IO,               ONLY: TFILEDATA
USE MODD_NSV,              ONLY: tsvlist, nsv, nsv_aer, nsv_aerbeg, nsv_aerend, nsv_dst, nsv_dstbeg, nsv_dstend, &
                                 nsv_lima_beg, nsv_lima_end
USE MODD_PARAMETERS,       ONLY: XUNDEF
USE MODD_PARAM_n,          ONLY: CCLOUD

USE MODE_AERO_PSD
use mode_aircraft_balloon, only: Aircraft_balloon_longtype_get
USE MODE_DUST_PSD
USE MODE_MODELN_HANDLER,   ONLY: GET_CURRENT_MODEL_INDEX
use mode_write_diachro,    only: Write_diachro


TYPE(TFILEDATA),   INTENT(IN) :: TPDIAFILE ! file to write
CLASS(TFLYERDATA), INTENT(IN) :: TPFLYER
!
!*      0.2  declaration of local variables for diachro
!
REAL, DIMENSION(:,:,:,:),     ALLOCATABLE :: ZSV, ZN0, ZSIG, ZRG
REAL, DIMENSION(:,:,:,:,:),   ALLOCATABLE :: ZPTOTA
REAL, DIMENSION(:,:,:),       ALLOCATABLE :: ZRHO
!
CHARACTER(LEN=NMNHNAMELGTMAX) :: YTITLE
CHARACTER(LEN=NCOMMENTLGTMAX) :: YCOMMENT
CHARACTER(LEN=NUNITLGTMAX)    :: YUNIT
!
INTEGER :: IMI      ! current model index
INTEGER :: IPROC    ! number of variables records
INTEGER :: JPROC    ! loop counter
integer :: jproc_w
INTEGER :: ISTORE
INTEGER :: IPROCZ   ! number of variables records
INTEGER :: IRR      ! number of hydrometeors
INTEGER :: JRR      ! loop counter
INTEGER :: JSV      ! loop counter
INTEGER :: JPT      ! loop counter
INTEGER :: IKU
REAL, DIMENSION(:), ALLOCATABLE :: ZLWC ! Temporary array to store/compute Liquid Water Content at flyer position
type(tbudiachrometadata) :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
!
!----------------------------------------------------------------------------
!
IMI = GET_CURRENT_MODEL_INDEX()

IRR = SIZE( tpflyer%xr, 3 )

IF (TPFLYER%NMODEL==0) RETURN
IF (ALL(TPFLYER%XX==XUNDEF)) RETURN
IF (COUNT(TPFLYER%XX/=XUNDEF)<=1) RETURN
IF ( IMI /= TPFLYER%NMODEL ) RETURN
!
IKU = SIZE(TPFLYER%XRTZ,1) !number of vertical levels
!
IPROC = 9 + IRR + SIZE(TPFLYER%XSV,3) + 2 + SIZE(TPFLYER%XSVW_FLUX,2)
IF ( IRR > 1 ) IPROC = IPROC + 1
IF ( SIZE( TPFLYER%XTKE ) > 0 ) IPROC = IPROC + 1
IPROC = IPROC + 1 ! TKE_DISS
IF ( LORILAM ) IPROC = IPROC + JPMODE * ( 3 + NSOA + NCARB + NSP )
IF ( LDUST ) IPROC = IPROC + NMODE_DST * 3
IF ( SIZE( TPFLYER%XTSRAD ) > 0 ) IPROC = IPROC + 1
!
ISTORE = SIZE( TPFLYER%TFLYER_TIME%TPDATES )
!
!----------------------------------------------------------------------------
!Treat point values
ALLOCATE (XWORK6(1,1,1,ISTORE,1,IPROC))
ALLOCATE (CCOMMENT(IPROC))
ALLOCATE (CTITLE  (IPROC))
ALLOCATE (CUNIT   (IPROC))

jproc = 0

call Add_point( 'ZS', 'orography', 'm', tpflyer%xzs(:) )
!
SELECT TYPE ( TPFLYER )
  CLASS IS ( TAIRCRAFTDATA )
    IF (TPFLYER%LALTDEF) THEN
      call Add_point( 'P', 'pressure', 'Pascal', tpflyer%xp(1,:) )
    ELSE
      call Add_point( 'Z', 'altitude', 'm', tpflyer%xz(:) )
    ENDIF

  CLASS IS ( TBALLOONDATA )
    call Add_point( 'Z', 'altitude', 'm', tpflyer%xz(:) )

END SELECT
!
call Add_point( 'MODEL',    'model on which data was computed', '1', REAL( tpflyer%nmodelhist(:) ) )
call Add_point( 'LON',      'longitude',             'degree', tpflyer%xlon(:) )
call Add_point( 'LAT',      'latitude',              'degree', tpflyer%xlat(:) )
call Add_point( 'ZON_WIND', 'zonal wind',            'm s-1',  tpflyer%xzon(1,:) )
call Add_point( 'MER_WIND', 'meridian wind',         'm s-1',  tpflyer%xmer(1,:) )
call Add_point( 'W',        'air vertical speed',    'm s-1',  tpflyer%xw(1,:)   )
call Add_point( 'Th',       'potential temperature', 'K',      tpflyer%xth(1,:)  )
!
if ( irr >= 1 ) call Add_point( 'Rv', 'water vapor mixing ratio',        'kg kg-1', tpflyer%xr(1,:,1) )
if ( irr >= 2 ) call Add_point( 'Rc', 'liquid cloud water mixing ratio', 'kg kg-1', tpflyer%xr(1,:,2) )
if ( irr >= 3 ) call Add_point( 'Rr', 'Rain water mixing ratio',         'kg kg-1', tpflyer%xr(1,:,3) )
if ( irr >= 4 ) call Add_point( 'Ri', 'Ice cloud water mixing ratio',    'kg kg-1', tpflyer%xr(1,:,4) )
if ( irr >= 5 ) call Add_point( 'Rs', 'Snow mixing ratio',               'kg kg-1', tpflyer%xr(1,:,5) )
if ( irr >= 6 ) call Add_point( 'Rg', 'Graupel mixing ratio',            'kg kg-1', tpflyer%xr(1,:,6) )
if ( irr >= 7 ) call Add_point( 'Rh', 'Hail mixing ratio',               'kg kg-1', tpflyer%xr(1,:,7) )
!
!add cloud liquid water content in g/m3 to compare to measurements from FSSP
!IF (.NOT.(ANY(TPFLYER%XP(:) == 0.))) THEN
IF ( IRR > 1 ) THEN !cloud water is present
  ALLOCATE( ZRHO(1, 1, ISTORE) )
  ALLOCATE( ZLWC(ISTORE) )
  ZRHO(1,1,:) = 0.
  DO JRR = 1, IRR
    ZRHO(1,1,:) = ZRHO(1,1,:) + TPFLYER%XR(1,:,JRR)
  ENDDO
  ZRHO(1,1,:) = TPFLYER%XTH(1,:) * ( 1. + XRV/XRD*TPFLYER%XR(1,:,1) )  &
                                 / ( 1. + ZRHO(1,1,:)               )
  DO JPT=1,ISTORE
    IF ( TPFLYER%NMODELHIST(JPT) > 0 ) THEN !Compute LWC only if flyer is flying
      IF (TPFLYER%XP(1,JPT) == 0.) THEN
        ZRHO(1,1,JPT) = 0.
      ELSE
        ZRHO(1,1,JPT) = TPFLYER%XP(1,JPT) / &
                        (XRD *ZRHO(1,1,JPT) *((TPFLYER%XP(1,JPT)/XP00)**(XRD/XCPD))  )
      ENDIF
      ZLWC(JPT) = TPFLYER%XR(1,JPT,2) * ZRHO(1,1,JPT) * 1.E3
    ELSE
      ZLWC(JPT) = XUNDEF
    END IF
  END DO
  call Add_point( 'LWC', 'cloud liquid water content', 'g m-3', ZLWC(:) )
  DEALLOCATE( ZLWC, ZRHO )
END IF
!
IF (SIZE(TPFLYER%XTKE)>0) call Add_point( 'Tke', 'Turbulent kinetic energy', 'm2 s-2', tpflyer%xtke(1,:) )
!
call Add_point( 'H_FLUX',  'sensible flux', 'W m-2', tpflyer%xthw_flux(:) )
call Add_point( 'LE_FLUX', 'latent flux',   'W m-2', tpflyer%xrcw_flux(:) )
!
DO JSV=1,SIZE(TPFLYER%XSVW_FLUX,2)
  WRITE ( YTITLE, FMT = '( A, I3.3 )' ) 'SV_FLUX', JSV
  call Add_point( Trim( ytitle ), 'scalar flux', 'SVUNIT m s-1', tpflyer%xsvw_flux(:,jsv) )
END DO
call Add_point( 'Tke_Diss', 'TKE dissipation rate', 'm2 s-2', tpflyer%xtke_diss(:) )
!
IF (SIZE(TPFLYER%XSV,3)>=1) THEN
  ! Scalar variables
  DO JSV = 1, NSV
    IF ( TRIM( TSVLIST(JSV)%CUNITS ) == 'ppv' ) THEN
      call Add_point( Trim( tsvlist(jsv)%cmnhname ), '', 'ppb', tpflyer%xsv(1,:,jsv) * 1.e9 ) !*1e9 for conversion ppv->ppb
    ELSE
      call Add_point( Trim( tsvlist(jsv)%cmnhname ), '', Trim( tsvlist(jsv)%cunits ), tpflyer%xsv(1,:,jsv) )
    END IF
  END DO

  IF ((LORILAM).AND. .NOT.(ANY(TPFLYER%XP(1,:) == 0.))) THEN

    ALLOCATE (ZSV(1,1,ISTORE,NSV_AER))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,JPMODE))
    ALLOCATE (ZRG(1,1,ISTORE,JPMODE))
    ALLOCATE (ZSIG(1,1,ISTORE,JPMODE))
    ALLOCATE (ZPTOTA(1,1,ISTORE,NSP+NCARB+NSOA,JPMODE))
    ZSV(1,1,:,1:NSV_AER) = TPFLYER%XSV(1,:,NSV_AERBEG:NSV_AEREND)
    IF (IRR >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,IRR
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPFLYER%XR(1,:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPFLYER%XTH(1,:) * ( 1. + XRV/XRD*TPFLYER%XR(1,:,1) )  &
                                     / ( 1. + ZRHO(1,1,:)               )
    ELSE
      ZRHO(1,1,:) = TPFLYER%XTH(1,:)
    ENDIF
    ZRHO(1,1,:) =  TPFLYER%XP(1,:) / &
                  (XRD *ZRHO(1,1,:) *((TPFLYER%XP(1,:)/XP00)**(XRD/XCPD))  )
    ZSIG = 0.
    ZRG = 0.
    ZN0 = 0.
    ZPTOTA = 0.
    DO JPT=1,ISTORE ! prevent division by zero if ZSV = 0.
      IF (ALL(ZSV(1,1,JPT,:)/=0.)) THEN
        CALL PPP2AERO(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0, PCTOTA=ZPTOTA)
      ENDIF
    ENDDO
    DO JSV=1,JPMODE
      ! mean radius
      WRITE(YTITLE,'(A,I1)')'AERRGA',JSV
      WRITE(YCOMMENT,'(A,I1)')'RG (nb) AERO MODE ',JSV
      call Add_point( Trim( ytitle ), 'um', Trim( ycomment ), ZRG(1,1,:,JSV) )
      ! standard deviation
      WRITE(YTITLE,'(A,I1)')'AERSIGA',JSV
      WRITE(YCOMMENT,'(A,I1)')'SIGMA AERO MODE ',JSV
      call Add_point( Trim( ytitle ), '1', Trim( ycomment ), ZSIG(1,1,:,JSV) )
      ! particles number
      WRITE(YTITLE,'(A,I1)')'AERN0A',JSV
      WRITE(YCOMMENT,'(A,I1)')'N0 AERO MODE ',JSV
      call Add_point( Trim( ytitle ), 'm-3', Trim( ycomment ), ZN0(1,1,:,JSV) )
      ! mass concentration in microg/m3
      ! sulfate
      WRITE(YTITLE,'(A,I1)')'MSO4',JSV
      WRITE(YCOMMENT,'(A,I1)')'MASS SO4 AEROSOL MODE ',JSV
      call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SO4,JSV) )
      ! nitrate
      WRITE(YTITLE,'(A,I1)')'MNO3',JSV
      WRITE(YCOMMENT,'(A,I1)')'MASS NO3 AEROSOL MODE ',JSV
      call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_NO3,JSV) )
      ! amoniac
      WRITE(YTITLE,'(A,I1)')'MNH3',JSV
      WRITE(YCOMMENT,'(A,I1)')'MASS NH3 AEROSOL MODE ',JSV
      call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_NH3,JSV) )
      ! water
      WRITE(YTITLE,'(A,I1)')'MH2O',JSV
      WRITE(YCOMMENT,'(A,I1)')'MASS H2O AEROSOL MODE ',JSV
      call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_H2O,JSV) )
      IF (NSOA .EQ. 10) THEN
        ! SOA1
        WRITE(YTITLE,'(A,I1)')'MSOA1',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA1 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA1,JSV) )
        ! SOA2
        WRITE(YTITLE,'(A,I1)')'MSOA2',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA2 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA2,JSV) )
        ! SOA3
        WRITE(YTITLE,'(A,I1)')'MSOA3',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA3 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA3,JSV) )
        ! SOA4
        WRITE(YTITLE,'(A,I1)')'MSOA4',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA4 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA4,JSV) )
        ! SOA5
        WRITE(YTITLE,'(A,I1)')'MSOA5',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA5 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA5,JSV) )
        ! SOA6
        WRITE(YTITLE,'(A,I1)')'MSOA6',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA6 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA6,JSV) )
        ! SOA7
        WRITE(YTITLE,'(A,I1)')'MSOA7',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA7 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA7,JSV) )
        ! SOA8
        WRITE(YTITLE,'(A,I1)')'MSOA8',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA8 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA8,JSV) )
        ! SOA9
        WRITE(YTITLE,'(A,I1)')'MSOA9',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA9 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA9,JSV) )
        ! SOA10
        WRITE(YTITLE,'(A,I1)')'MSOA10',JSV
        WRITE(YCOMMENT,'(A,I1)')'MASS SOA10 AEROSOL MODE ',JSV
        call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_SOA10,JSV) )
      ENDIF
      ! OC
      WRITE(YTITLE,'(A,I1)')'MOC',JSV
      WRITE(YCOMMENT,'(A,I1)')'MASS OC AEROSOL MODE ',JSV
      call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_OC,JSV) )
      ! BC
      WRITE(YTITLE,'(A,I1)')'MBC',JSV
      WRITE(YCOMMENT,'(A,I1)')'MASS BC AEROSOL MODE ',JSV
      call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_BC,JSV) )
      ! dust
      WRITE(YTITLE,'(A,I1)')'MDUST',JSV
      WRITE(YCOMMENT,'(A,I1)')'MASS DUST AEROSOL MODE ',JSV
      call Add_point( Trim( ytitle ), 'ug m-3', Trim( ycomment ), ZPTOTA(1,1,:,JP_AER_DST,JSV) )
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG,ZPTOTA)
  END IF

  IF ((LDUST).AND. .NOT.(ANY(TPFLYER%XP(1,:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,ISTORE,NSV_DST))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,NMODE_DST))
    ALLOCATE (ZRG(1,1,ISTORE,NMODE_DST))
    ALLOCATE (ZSIG(1,1,ISTORE,NMODE_DST))
    ZSV(1,1,:,1:NSV_DST) = TPFLYER%XSV(1,:,NSV_DSTBEG:NSV_DSTEND)
    IF (IRR >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,IRR
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPFLYER%XR(1,:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPFLYER%XTH(1,:) * ( 1. + XRV/XRD*TPFLYER%XR(1,:,1) )  &
                                     / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPFLYER%XTH(1,:)
    ENDIF
    ZRHO(1,1,:) =  TPFLYER%XP(1,:) / &
                  (XRD *ZRHO(1,1,:) *((TPFLYER%XP(1,:)/XP00)**(XRD/XCPD)) )
    CALL PPP2DUST(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,NMODE_DST
      ! mean radius
      WRITE(YTITLE,'(A,I1)')'DSTRGA',JSV
      WRITE(YCOMMENT,'(A,I1)')'RG (nb) DUST MODE ',JSV
      call Add_point( Trim( ytitle ), 'um', Trim( ycomment ), ZRG(1,1,:,JSV) )
      ! standard deviation
      WRITE(YTITLE,'(A,I1)')'DSTSIGA',JSV
      WRITE(YCOMMENT,'(A,I1)')'SIGMA DUST MODE ',JSV
      call Add_point( Trim( ytitle ), '1', Trim( ycomment ), ZSIG(1,1,:,JSV) )
      ! particles number
      WRITE(YTITLE,'(A,I1)')'DSTN0A',JSV
      WRITE(YCOMMENT,'(A,I1)')'N0 DUST MODE ',JSV
      call Add_point( Trim( ytitle ), 'm-3', Trim( ycomment ), ZN0(1,1,:,JSV) )
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF
ENDIF
!
IF (SIZE(TPFLYER%XTSRAD)>0) THEN
  JPROC = JPROC+1
  CTITLE   (JPROC) = 'Tsrad'
  CUNIT    (JPROC) = 'K'
  CCOMMENT (JPROC) = 'Radiative Surface Temperature'
  XWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XTSRAD(:)
END IF
!
allocate( tzfields( jproc ) )

tzfields(:)%cmnhname  = ctitle(1 : jproc)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = ctitle(1 : jproc)
tzfields(:)%cunits    = cunit(1 : jproc)
tzfields(:)%ccomment  = ccomment(1 : jproc)
tzfields(:)%ngrid     = 0
tzfields(:)%ntype     = TYPEREAL
tzfields(:)%ndims     = 2
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(3) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(4) = NMNHDIM_FLYER_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_FLYER_PROC

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Flyers'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different flyers (aircrafts and balloons)'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .true.
call Aircraft_balloon_longtype_get( tpflyer, tzbudiachro%clevels(NLVL_SUBCATEGORY) )
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = 'Level for the flyers of type: ' // Trim( tzbudiachro%clevels(NLVL_SUBCATEGORY) )

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = Trim( tpflyer%cname )
tzbudiachro%ccomments(NLVL_GROUP)       = 'Values for flyer ' // Trim( tpflyer%cname )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .true.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Point'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Values at position of flyer ' // Trim( tpflyer%cname )

tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Not_time_averaged'
tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are not time averaged'

tzbudiachro%lleveluse(NLVL_NORM)        = .false.
tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

tzbudiachro%lleveluse(NLVL_MASK)        = .false.
tzbudiachro%clevels  (NLVL_MASK)        = ''
tzbudiachro%ccomments(NLVL_MASK)        = ''

tzbudiachro%lmobile    = .true.
!Compression does not make sense here
! tzbudiachro%licompress = NOT SET (default values)
! tzbudiachro%ljcompress = NOT SET (default values)
! tzbudiachro%lkcompress = NOT SET (default values)
tzbudiachro%ltcompress = .false.
tzbudiachro%lnorm      = .false.
!Boundaries in physical domain does not make sense here (but flyer position does)
!These values are not written in the netCDF files
!These values are written in the LFI files
! tzbudiachro%nil        = NOT SET (default values)
! tzbudiachro%nih        = NOT SET (default values)
! tzbudiachro%njl        = NOT SET (default values)
! tzbudiachro%njh        = NOT SET (default values)
! tzbudiachro%nkl        = NOT SET (default values)
! tzbudiachro%nkh        = NOT SET (default values)

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tpflyer%tflyer_time%tpdates, xwork6(:,:,:,:,:,:jproc), &
                    tpflyer = tpflyer                                                                        )

Deallocate( tzfields )
Deallocate( xwork6 )
Deallocate( ccomment )
Deallocate( ctitle )
Deallocate( cunit )

!----------------------------------------------------------------------------
!Treat vertical profiles

IPROCZ = 8 + IRR
IF ( CCLOUD == 'LIMA' )     IPROCZ = IPROCZ + 3
IF ( CCLOUD(1:3) == 'ICE' ) IPROCZ = IPROCZ + 1

ALLOCATE (XWORK6(1,1,IKU,ISTORE,1,IPROCZ))
ALLOCATE (CCOMMENT(IPROCZ))
ALLOCATE (CTITLE (IPROCZ))
ALLOCATE (CUNIT  (IPROCZ))

JPROC = 0

call Add_profile( 'Rt', '1D Total hydrometeor mixing ratio', 'kg kg-1', tpflyer%xrtz(:,:) )

if ( irr >= 1 ) call Add_profile( 'Rv', '1D water vapor mixing ratio',        'kg kg-1', tpflyer%xrz(:,:,1) )
if ( irr >= 2 ) call Add_profile( 'Rc', '1D liquid cloud water mixing ratio', 'kg kg-1', tpflyer%xrz(:,:,2) )
if ( irr >= 3 ) call Add_profile( 'Rr', '1D Rain water mixing ratio',         'kg kg-1', tpflyer%xrz(:,:,3) )
if ( irr >= 4 ) call Add_profile( 'Ri', '1D Ice cloud water mixing ratio',    'kg kg-1', tpflyer%xrz(:,:,4) )
if ( irr >= 5 ) call Add_profile( 'Rs', '1D Snow mixing ratio',               'kg kg-1', tpflyer%xrz(:,:,5) )
if ( irr >= 6 ) call Add_profile( 'Rg', '1D Graupel mixing ratio',            'kg kg-1', tpflyer%xrz(:,:,6) )
if ( irr >= 7 ) call Add_profile( 'Rh', '1D Hail mixing ratio',               'kg kg-1', tpflyer%xrz(:,:,7) )

call Add_profile( 'FF', 'Horizontal wind', 'm s-1', tpflyer%xffz(:,:) )

call Add_profile( 'IWC', 'Ice water content',    'kg m-3', tpflyer%xiwcz(:,:) )
call Add_profile( 'LWC', 'Liquid water content', 'kg m-3', tpflyer%xlwcz(:,:) )

IF ( CCLOUD == 'LIMA' ) THEN
  call Add_profile( 'CCLOUDT', 'liquid cloud concentration', 'kg-1', tpflyer%xccz(:,:) )
  call Add_profile( 'CRAINT',  'Rain concentration',         'kg-1', tpflyer%xcrz(:,:) )
  call Add_profile( 'CICET',   'Ice concentration',          'kg-1', tpflyer%xciz(:,:) )
ELSE IF ( CCLOUD == 'ICE3' .OR. CCLOUD == 'ICE4' ) THEN
  call Add_profile( 'CIT', 'Ice concentration', 'm-3', tpflyer%xciz(:,:) )
END IF

call Add_profile( 'RARE',    '1D cloud radar reflectivity',            'dBZ', tpflyer%xcrare(:,:) )
call Add_profile( 'RAREatt', '1D cloud radar attenuated reflectivity', 'dBZ', tpflyer%xcrare_att(:,:) )

call Add_profile( 'W', '1D vertical velocity', 'm s-1', tpflyer%xwz(:,:) )
!Store position of W in the field list. Useful because it is not computed on the same Arakawa-grid points
jproc_w = jproc
call Add_profile( 'Z', '1D altitude above sea', 'm', tpflyer%xzz(:,:) )

allocate( tzfields( jproc ) )

tzfields(:)%cmnhname  = ctitle(1 : jproc)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = ctitle(1 : jproc)
tzfields(:)%cunits    = cunit(1 : jproc)
tzfields(:)%ccomment  = ccomment(1 : jproc)
tzfields(:)%ngrid     = 0
tzfields(:)%ntype     = TYPEREAL
tzfields(:)%ndims     = 3
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(3) = NMNHDIM_LEVEL
tzfields(jproc_w)%ndimlist(3) = NMNHDIM_LEVEL_W
tzfields(:)%ndimlist(4) = NMNHDIM_FLYER_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_FLYER_PROC

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Flyers'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different flyers (aircrafts and balloons)'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .true.
call Aircraft_balloon_longtype_get( tpflyer, tzbudiachro%clevels(NLVL_SUBCATEGORY) )
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = 'Level for the flyers of type: ' // Trim( tzbudiachro%clevels(NLVL_SUBCATEGORY) )

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = Trim( tpflyer%cname )
tzbudiachro%ccomments(NLVL_GROUP)       = 'Values for flyer ' // Trim( tpflyer%cname )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .true.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Vertical_profile'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Vertical profiles at position of flyer ' // Trim( tpflyer%cname )

tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Not_time_averaged'
tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are not time averaged'

tzbudiachro%lleveluse(NLVL_NORM)        = .false.
tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

tzbudiachro%lleveluse(NLVL_MASK)        = .false.
tzbudiachro%clevels  (NLVL_MASK)        = ''
tzbudiachro%ccomments(NLVL_MASK)        = ''

tzbudiachro%lmobile    = .true.
!Compression does not make sense here
!Keep these values for backward compatibility of LFI files
tzbudiachro%licompress = .true.
tzbudiachro%ljcompress = .true.
tzbudiachro%lkcompress = .false.
tzbudiachro%ltcompress = .false.
tzbudiachro%lnorm      = .false.
!Horizontal boundaries in physical domain does not make sense here (but flyer position does)
!These values are not written in the netCDF files
!These values are written in the LFI files. They are kept for backward compatibility (and not set to default values)
tzbudiachro%nil        = 1
tzbudiachro%nih        = 1
tzbudiachro%njl        = 1
tzbudiachro%njh        = 1
!1->iku includes non-physical levels (IKU=NKMAX+2*JPVEXT)
!This does not conform to documentation (limits are in the physical domain)
!These values are not written in the netCDF files
!These values are written in the LFI files. They are kept for backward compatibility (and not set to default values)
tzbudiachro%nkl        = 1
tzbudiachro%nkh        = iku

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tpflyer%tflyer_time%tpdates, xwork6(:,:,:,:,:,:jproc), &
                    tpflyer = tpflyer                                                                        )

deallocate( tzfields )

DEALLOCATE (XWORK6)
DEALLOCATE (CCOMMENT)
DEALLOCATE (CTITLE  )
DEALLOCATE (CUNIT   )

contains

subroutine Add_profile( htitle, hcomment, hunits, pfield )
! This subroutine is a simple interface to the generic Add_profile.
! This is done this way to reduce the number of arguments passed to Add_profile.
use mode_sensor, only: Sensor_diachro_profile_add

character(len=*),     intent(in) :: htitle
character(len=*),     intent(in) :: hcomment
character(len=*),     intent(in) :: hunits
real, dimension(:,:), intent(in) :: pfield

call Sensor_diachro_profile_add( htitle, hcomment, hunits, pfield, jproc, iprocz, ctitle, ccomment, cunit, xwork6 )

end subroutine Add_profile


subroutine Add_point( htitle, hcomment, hunits, pfield )
! This subroutine is a simple interface to the generic Add_point.
! This is done this way to reduce the number of arguments passed to Add_point.
use mode_sensor, only: Sensor_diachro_point_add

character(len=*),   intent(in) :: htitle
character(len=*),   intent(in) :: hcomment
character(len=*),   intent(in) :: hunits
real, dimension(:), intent(in) :: pfield

call Sensor_diachro_point_add( htitle, hcomment, hunits, pfield, jproc, iproc, ctitle, ccomment, cunit, xwork6 )

end subroutine Add_point

!----------------------------------------------------------------------------
END SUBROUTINE FLYER_DIACHRO
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!

END MODULE MODE_WRITE_AIRCRAFT_BALLOON
