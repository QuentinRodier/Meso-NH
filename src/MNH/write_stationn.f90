!MNH_LIC Copyright 2002-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  P. Tulet    15/02/2002
!
!  Modifications
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 09/10/2020: Write_diachro: use new datatype tpfields
!  P. Wautelet 03/03/2021: budgets: add tbudiachrometadata type (useful to pass more information to Write_diachro)
!  P. Wautelet 04/02/2022: use TSVLIST to manage metadata of scalar variables
!  P. Wautelet    04/2022: restructure stations for better performance, reduce memory usage and correct some problems/bugs
! --------------------------------------------------------------------------
!      ###########################
MODULE MODE_WRITE_STATION_n
!      ###########################

use modd_parameters, only: NCOMMENTLGTMAX, NMNHNAMELGTMAX, NUNITLGTMAX

implicit none

private

public :: STATION_DIACHRO_n

CHARACTER(LEN=NCOMMENTLGTMAX), DIMENSION(:), ALLOCATABLE :: CCOMMENT ! comment string
CHARACTER(LEN=NMNHNAMELGTMAX), DIMENSION(:), ALLOCATABLE :: CTITLE   ! title
CHARACTER(LEN=NUNITLGTMAX),    DIMENSION(:), ALLOCATABLE :: CUNIT    ! physical unit

REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: XWORK6   ! contains temporal serie

contains

! ##################################################
SUBROUTINE STATION_DIACHRO_n( TPDIAFILE, TPSTATION )
! ##################################################

USE MODD_ALLSTATION_n,  ONLY: LDIAG_SURFRAD_STAT
use modd_budget,        only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                              tbudiachrometadata
USE MODD_CONF,          ONLY: LCARTESIAN
USE MODD_CST,           ONLY: XRV
use modd_field,         only: NMNHDIM_STATION_TIME, NMNHDIM_STATION_PROC, NMNHDIM_UNUSED, &
                              tfieldmetadata_base, TYPEREAL
USE MODD_IO,            ONLY: TFILEDATA
USE MODD_NSV,           ONLY: nsv, nsv_aer, nsv_aerbeg, nsv_aerend, &
                              nsv_dst, nsv_dstbeg, nsv_dstend, nsv_slt, nsv_sltbeg, nsv_sltend, &
                              tsvlist
USE MODD_PARAM_n,       ONLY: CRAD, CSURF, CTURB
use modd_station_n,     only: tstations_time
use modd_type_statprof, only: tstationdata

USE MODE_AERO_PSD
USE MODE_DUST_PSD
USE MODE_SALT_PSD
use MODE_WRITE_DIACHRO,  ONLY: Write_diachro

TYPE(TFILEDATA),    INTENT(IN) :: TPDIAFILE ! diachronic file to write
TYPE(TSTATIONDATA), INTENT(IN) :: TPSTATION
!
!*      0.2  declaration of local variables for diachro
!
REAL, DIMENSION(:,:,:,:),     ALLOCATABLE :: ZSV, ZN0, ZSIG, ZRG
REAL, DIMENSION(:,:,:,:,:),   ALLOCATABLE :: ZPTOTA
REAL, DIMENSION(:,:,:),       ALLOCATABLE :: ZRHO
!
CHARACTER(LEN=NCOMMENTLGTMAX)     :: YCOMMENT ! comment string
CHARACTER(LEN=NMNHNAMELGTMAX)     :: YTITLE   ! title
!
!!! do not forget to increment the IPROC value if you add diagnostic !!!
INTEGER :: IPROC    ! number of variables records
!!! do not forget to increment the JPROC value if you add diagnostic !!!
INTEGER :: ISTORE
INTEGER :: JPROC    ! loop counter
INTEGER :: JRR      ! loop counter
INTEGER :: JSV      ! loop counter
real    :: zalt_meas ! True altitude for the station measurements
type(tbudiachrometadata)                             :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
!
!----------------------------------------------------------------------------
!
IPROC = 5 + SIZE(TPSTATION%XR,3) + SIZE(TPSTATION%XSV,3)

IF ( CTURB == 'TKEL' ) IPROC = IPROC + 1
IF (LDIAG_SURFRAD_STAT) THEN
  IF(CSURF=="EXTE") IPROC = IPROC + 10
  IF(CRAD/="NONE")  IPROC = IPROC + 8
  IPROC = IPROC + 1 ! XSFCO2 term
END IF
IF (LORILAM) IPROC = IPROC + JPMODE*(3+NSOA+NCARB+NSP)
IF (LDUST) IPROC = IPROC + NMODE_DST*3
IF (LSALT) IPROC = IPROC + NMODE_SLT*3
IF ( CRAD /= 'NONE' )  IPROC = IPROC + 1

ISTORE = SIZE( TSTATIONS_TIME%TPDATES )

ALLOCATE( XWORK6(1, 1, 1, ISTORE, 1, IPROC) )
ALLOCATE( CCOMMENT(IPROC) )
ALLOCATE( CTITLE  (IPROC) )
ALLOCATE( CUNIT   (IPROC) )
!
JPROC = 0
!
!----------------------------------------------------------------------------
!
call Add_point( 'P',  'Pressure',  'Pa', tpstation%xp(1,:) )

if ( lcartesian ) then
  call Add_point( 'U', 'Axial velocity',       'm s-1', tpstation%xzon(1,:) )
  call Add_point( 'V', 'Transversal velocity', 'm s-1', tpstation%xmer(1,:) )
else
  call Add_point( 'ZON_WIND', 'Zonal wind',      'm s-1', tpstation%xzon(1,:) )
  call Add_point( 'MER_WIND', 'Meridional wind', 'm s-1', tpstation%xmer(1,:) )
end if

call Add_point( 'W',  'Air vertical speed',    'm s-1', tpstation%xw(1,:)  )
call Add_point( 'Th', 'Potential temperature', 'K',     tpstation%xth(1,:) )

if ( ldiag_surfrad_stat ) then
  if ( csurf == "EXTE" ) then
    call Add_point( 'T2m',    '2-m temperature',        'K',       tpstation%xt2m(:)    )
    call Add_point( 'Q2m',    '2-m humidity',           'kg kg-1', tpstation%xq2m(:)    )
    call Add_point( 'HU2m',   '2-m relative humidity',  'percent', tpstation%xhu2m(:)   )
    call Add_point( 'zon10m', '10-m zonal wind',        'm s-1',   tpstation%xzon10m(:) )
    call Add_point( 'mer10m', '10-m meridian wind',     'm s-1',   tpstation%xmer10m(:) )
    call Add_point( 'RN',     'Net radiation',          'W m-2',   tpstation%xrn(:)     )
    call Add_point( 'H',      'Sensible heat flux',     'W m-2',   tpstation%xh(:)      )
    call Add_point( 'LE',     'Total Latent heat flux', 'W m-2',   tpstation%xle(:)     )
    call Add_point( 'G',      'Storage heat flux',      'W m-2',   tpstation%xgflux(:)  )
    call Add_point( 'LEI',    'Solid Latent heat flux', 'W m-2',   tpstation%xlei(:)    )
  end if
  if ( crad /= 'NONE' ) then
    call Add_point( 'SWD',    'Downward short-wave radiation',         'W m-2', tpstation%xswd(:)    )
    call Add_point( 'SWU',    'Upward short-wave radiation',           'W m-2', tpstation%xswu(:)    )
    call Add_point( 'LWD',    'Downward long-wave radiation',          'W m-2', tpstation%xlwd(:)    )
    call Add_point( 'LWU',    'Upward long-wave radiation',            'W m-2', tpstation%xlwu(:)    )
    call Add_point( 'SWDIR',  'Downward direct short-wave radiation',  'W m-2', tpstation%xswdir(:)  )
    call Add_point( 'SWDIFF', 'Downward diffuse short-wave radiation', 'W m-2', tpstation%xswdiff(:) )
    call Add_point( 'DSTAOD', 'Dust aerosol optical depth',            'm',     tpstation%xdstaod(:) )
    call Add_point( 'SLTAOD', 'Salt aerosol optical depth',            'm',     tpstation%xsltaod(:) )
  end if
end if

do jrr = 1, SIZE( tpstation%xr, 3 )
  select case( jrr )
    case (1)
      call Add_point( 'Rv', 'Water vapor mixing ratio',        'kg kg-1', tpstation%xr(1,:,jrr) )
    case (2)
      call Add_point( 'Rc', 'Liquid cloud water mixing ratio', 'kg kg-1', tpstation%xr(1,:,jrr) )
    case (3)
      call Add_point( 'Rr', 'Rain water mixing ratio',         'kg kg-1', tpstation%xr(1,:,jrr) )
    case (4)
      call Add_point( 'Ri', 'Ice cloud water mixing ratio',    'kg kg-1', tpstation%xr(1,:,jrr) )
    case (5)
      call Add_point( 'Rs', 'Snow mixing ratio',               'kg kg-1', tpstation%xr(1,:,jrr) )
    case (6)
      call Add_point( 'Rg', 'Graupel mixing ratio',            'kg kg-1', tpstation%xr(1,:,jrr) )
    case (7)
      call Add_point( 'Rh', 'Hail mixing ratio',               'kg kg-1', tpstation%xr(1,:,jrr) )
  end select
end do

if ( cturb == 'TKEL' ) call Add_point( 'Tke', 'Turbulent kinetic energy', 'm2 s-2', tpstation%xtke(1,:) )

if ( nsv > 0 ) then
  ! Scalar variables
  DO JSV = 1, NSV
    IF ( TRIM( TSVLIST(JSV)%CUNITS ) == 'ppv' ) THEN
      !*1e9 for conversion ppv->ppb
      call Add_point( TRIM( TSVLIST(JSV)%CMNHNAME ), '', 'ppb', TPSTATION%XSV(1,:,JSV) * 1.e9 )
    ELSE
      call Add_point( TRIM( TSVLIST(JSV)%CMNHNAME ), '', TSVLIST(JSV)%CUNITS, TPSTATION%XSV(1,:,JSV) )
    END IF
  END DO

  IF ((LORILAM).AND. .NOT.(ANY(TPSTATION%XP(1,:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,ISTORE,NSV_AER))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,JPMODE))
    ALLOCATE (ZRG(1,1,ISTORE,JPMODE))
    ALLOCATE (ZSIG(1,1,ISTORE,JPMODE))
    ALLOCATE (ZPTOTA(1,1,ISTORE,NSP+NCARB+NSOA,JPMODE))
    ZSV(1,1,:,1:NSV_AER) = TPSTATION%XSV(1,:,NSV_AERBEG:NSV_AEREND)
    IF (SIZE(TPSTATION%XR,3) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,3)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(1,:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(1,:) * ( 1. + XRV/XRD*TPSTATION%XR(1,:,1) )  &
                                       / ( 1. + ZRHO(1,1,:)                 )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(1,:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(1,:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(1,:)/XP00)**(XRD/XCPD)) )

    CALL PPP2AERO(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0,PCTOTA=ZPTOTA)

    DO JSV=1,JPMODE
      ! mean radius
      WRITE(YTITLE,'(A6,I1)')'AERRGA',JSV
      WRITE(YCOMMENT,'(A18,I1)')'RG (nb) AERO MODE ',JSV
      call Add_point( ytitle, ycomment, 'um', ZRG(1,1,:,JSV) )

      ! standard deviation
      WRITE(YTITLE,'(A7,I1)')'AERSIGA',JSV
      WRITE(YCOMMENT,'(A16,I1)')'SIGMA AERO MODE ',JSV
      call Add_point( ytitle, ycomment, '',ZSIG(1,1,:,JSV) )

      ! particles number
      WRITE(YTITLE,'(A6,I1)')'AERN0A',JSV
      WRITE(YCOMMENT,'(A13,I1)')'N0 AERO MODE ',JSV
      call Add_point( ytitle, ycomment, 'm-3', ZN0(1,1,:,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MOC  ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS OC   AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_OC,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MBC  ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS BC   AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_BC,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MDST  ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS DST   AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_DST,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MSO4 ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS SO4  AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SO4,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MNO3 ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS NO3  AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_NO3,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MH2O ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS H2O  AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_H2O,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MNH3 ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS NH3  AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_NH3,JSV) )

      IF ( NSOA == 10 ) THEN
        WRITE(YTITLE,'(A5,I1)')'MSOA1',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA1 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA1,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA2',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA2 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA2,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA3',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA3 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA3,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA4',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA4 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA4,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA5',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA5 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA5,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA6',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA6 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA6,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA7',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA7 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA7,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA8',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA8 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA8,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA9',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA9 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA9,JSV) )

        WRITE(YTITLE,'(A6,I1)')'MSOA10',JSV
        WRITE(CCOMMENT,'(A24,I1)')'MASS SOA10 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA10,JSV) )
      END IF
    END DO

    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF

  IF ((LDUST).AND. .NOT.(ANY(TPSTATION%XP(1,:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,ISTORE,NSV_DST))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,NMODE_DST))
    ALLOCATE (ZRG(1,1,ISTORE,NMODE_DST))
    ALLOCATE (ZSIG(1,1,ISTORE,NMODE_DST))
    ZSV(1,1,:,1:NSV_DST) = TPSTATION%XSV(1,:,NSV_DSTBEG:NSV_DSTEND)
    IF (SIZE(TPSTATION%XR,3) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,3)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(1,:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(1,:) * ( 1. + XRV/XRD*TPSTATION%XR(1,:,1) )  &
                                       / ( 1. + ZRHO(1,1,:)                 )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(1,:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(1,:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(1,:)/XP00)**(XRD/XCPD)) )
    CALL PPP2DUST(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,NMODE_DST
      ! mean radius
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A6,I1)')'DSTRGA',JSV
      CUNIT    (JPROC) = 'um'
      WRITE(CCOMMENT(JPROC),'(A18,I1)')'RG (nb) DUST MODE ',JSV
      XWORK6 (1,1,1,:,1,JPROC) = ZRG(1,1,:,JSV)
      ! standard deviation
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A7,I1)')'DSTSIGA',JSV
      CUNIT    (JPROC) = '  '
      WRITE(CCOMMENT(JPROC),'(A16,I1)')'SIGMA DUST MODE ',JSV
      XWORK6 (1,1,1,:,1,JPROC) = ZSIG(1,1,:,JSV)
      ! particles number
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A6,I1)')'DSTN0A',JSV
      CUNIT    (JPROC) = 'm-3'
      WRITE(CCOMMENT(JPROC),'(A13,I1)')'N0 DUST MODE ',JSV
      XWORK6 (1,1,1,:,1,JPROC) = ZN0(1,1,:,JSV)
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF

  IF ((LSALT).AND. .NOT.(ANY(TPSTATION%XP(1,:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,ISTORE,NSV_SLT))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,NMODE_SLT))
    ALLOCATE (ZRG(1,1,ISTORE,NMODE_SLT))
    ALLOCATE (ZSIG(1,1,ISTORE,NMODE_SLT))
    ZSV(1,1,:,1:NSV_SLT) = TPSTATION%XSV(1,:,NSV_SLTBEG:NSV_SLTEND)
    IF (SIZE(TPSTATION%XR,3) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,3)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(1,:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(1,:) * ( 1. + XRV/XRD*TPSTATION%XR(1,:,1) )  &
                                      / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(1,:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(1,:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(1,:)/XP00)**(XRD/XCPD)) )
    CALL PPP2SALT(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,NMODE_SLT
      ! mean radius
      WRITE(CTITLE(JPROC),'(A6,I1)')'SLTRGA',JSV
      WRITE(CCOMMENT(JPROC),'(A18,I1)')'RG (nb) SALT MODE ',JSV
      call Add_point( ytitle, ycomment, 'um', ZRG(1,1,:,JSV) )

      ! standard deviation
      WRITE(CTITLE(JPROC),'(A7,I1)')'SLTSIGA',JSV
      WRITE(CCOMMENT(JPROC),'(A16,I1)')'SIGMA DUST MODE ',JSV
      call Add_point( ytitle, ycomment, '',ZSIG(1,1,:,JSV) )

      ! particles number
      WRITE(CTITLE(JPROC),'(A6,I1)')'SLTN0A',JSV
      WRITE(CCOMMENT(JPROC),'(A13,I1)')'N0 DUST MODE ',JSV
      call Add_point( ytitle, ycomment, 'm-3', ZN0(1,1,:,JSV) )
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF
end if

if ( crad /= 'NONE' ) call Add_point( 'Tsrad', 'Radiative Surface Temperature', 'K', tpstation%xtsrad(:) )

if ( ldiag_surfrad_stat ) call Add_point( 'SFCO2', 'CO2 Surface Flux', 'mg m-2 s-1', tpstation%xsfco2(:) )
!
!----------------------------------------------------------------------------
!
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
tzfields(:)%ndimlist(4) = NMNHDIM_STATION_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_STATION_PROC

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Stations'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different stations'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = tpstation%cname
tzbudiachro%ccomments(NLVL_GROUP)       = 'Values at position of station ' // Trim( tpstation%cname )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .false.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Point'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Values at position of station ' // Trim( tpstation%cname )

tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Not_time_averaged'
tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are not time averaged'

tzbudiachro%lleveluse(NLVL_NORM)        = .false.
tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

tzbudiachro%lleveluse(NLVL_MASK)        = .false.
tzbudiachro%clevels  (NLVL_MASK)        = ''
tzbudiachro%ccomments(NLVL_MASK)        = ''

tzbudiachro%lmobile    = .false.
!Compression does not make sense here
!Keep these values for backward compatibility of LFI files
tzbudiachro%licompress = .true.
tzbudiachro%ljcompress = .true.
tzbudiachro%lkcompress = .false.
tzbudiachro%ltcompress = .false.
tzbudiachro%lnorm      = .false.
!Boundaries in physical domain does not make sense here
!These values are not written in the netCDF files
!These values are written in the LFI files. Kept for backward compatibility of LFI files
tzbudiachro%nil        = 1
tzbudiachro%nih        = 1
tzbudiachro%njl        = 1
tzbudiachro%njh        = 1
tzbudiachro%nkl        = 1
tzbudiachro%nkh        = 1

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tstations_time%tpdates, xwork6(:,:,:,:,:,:jproc) )

deallocate( tzfields )
Deallocate( xwork6   )
Deallocate( ccomment )
Deallocate( ctitle   )
Deallocate( cunit    )

!----------------------------------------------------------------------------
!Treat position and fix values (not changing during simulation)

IPROC = 6

ALLOCATE( XWORK6(1, 1, 1, 1, 1, IPROC) )
ALLOCATE( CCOMMENT(IPROC) )
ALLOCATE( CTITLE  (IPROC) )
ALLOCATE( CUNIT   (IPROC) )

jproc = 0

if ( lcartesian ) then
  JPROC = JPROC + 1
  CTITLE   (JPROC) = 'X'
  CUNIT    (JPROC) = 'm'
  CCOMMENT (JPROC) = 'X Pos'
  XWORK6 (1,1,1,1,1,JPROC) = TPSTATION%XX_CUR

  JPROC = JPROC + 1
  CTITLE   (JPROC) = 'Y'
  CUNIT    (JPROC) = 'm'
  CCOMMENT (JPROC) = 'Y Pos'
  XWORK6 (1,1,1,1,1,JPROC) = TPSTATION%XY_CUR
else
  JPROC = JPROC + 1
  CTITLE   (JPROC) = 'LON'
  CUNIT    (JPROC) = 'degree'
  CCOMMENT (JPROC) = 'Longitude'
  XWORK6 (1,1,1,1,1,JPROC) = TPSTATION%XLON_CUR

  JPROC = JPROC + 1
  CTITLE   (JPROC) = 'LAT'
  CUNIT    (JPROC) = 'degree'
  CCOMMENT (JPROC) = 'Latitude'
  XWORK6 (1,1,1,1,1,JPROC) = TPSTATION%XLAT_CUR
end if

JPROC = JPROC + 1
CTITLE   (JPROC) = 'Z'
CUNIT    (JPROC) = 'm'
CCOMMENT (JPROC) = 'Altitude'
XWORK6 (1,1,1,1,1,JPROC) = TPSTATION%XZ_CUR

JPROC = JPROC + 1
CTITLE   (JPROC) = 'ZS'
CUNIT    (JPROC) = 'm'
CCOMMENT (JPROC) = 'Orography'
XWORK6 (1,1,1,1,1,JPROC) = TPSTATION%XZS

JPROC = JPROC + 1
CTITLE   (JPROC) = 'Zmeas'
CUNIT    (JPROC) = 'm'
CCOMMENT (JPROC) = 'interpolated altitude used for measurements'
XWORK6 (1,1,1,1,1,JPROC) = TPSTATION%XZMEAS

JPROC = JPROC + 1
CTITLE   (JPROC) = 'K'
CUNIT    (JPROC) = '1'
CCOMMENT (JPROC) = 'vertical model level used for computations'
XWORK6 (1,1,1,1,1,JPROC) = REAL( TPSTATION%NK )

Allocate( tzfields( jproc ) )

tzfields(:)%cmnhname  = ctitle(1 : jproc)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = ctitle(1 : jproc)
tzfields(:)%cunits    = cunit(1 : jproc)
tzfields(:)%ccomment  = ccomment(1 : jproc)
tzfields(:)%ngrid     = 0
tzfields(:)%ntype     = TYPEREAL
tzfields(:)%ndims     = 1
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(3) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(4) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_STATION_PROC

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tstations_time%tpdates, xwork6(:,:,:,:,:,:jproc) )


!Necessary because global variables (private inside module)
Deallocate( xwork6  )
Deallocate (ccomment)
Deallocate (ctitle  )
Deallocate (cunit   )

!----------------------------------------------------------------------------

contains

! ######################################################
subroutine Add_point( htitle, hcomment, hunits, pfield )
! ######################################################
! This subroutine is a simple interface to the generic Add_point.
! This is done this way to reduce the number of arguments passed to Add_point.
use mode_sensor, only: Sensor_diachro_point_add

character(len=*),   intent(in) :: htitle
character(len=*),   intent(in) :: hcomment
character(len=*),   intent(in) :: hunits
real, dimension(:), intent(in) :: pfield

call Sensor_diachro_point_add( htitle, hcomment, hunits, pfield, jproc, iproc, ctitle, ccomment, cunit, xwork6 )

end subroutine Add_point

END SUBROUTINE STATION_DIACHRO_n

END MODULE MODE_WRITE_STATION_n
