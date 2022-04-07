!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ###########################
MODULE MODI_WRITE_STATION_n
!      ###########################
!
INTERFACE
!
      SUBROUTINE WRITE_STATION_n(TPDIAFILE)
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE ! diachronic file to write
!
END SUBROUTINE WRITE_STATION_n
!
END INTERFACE
!
END MODULE MODI_WRITE_STATION_n
!
!     ##########################################
      SUBROUTINE WRITE_STATION_n(TPDIAFILE)
!     ##########################################
!
!
!!****  *WRITE_STATION* - write the balloon and aircraft trajectories and records
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
!!      Pierre TULET             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/02/2002
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 09/10/2020: Write_diachro: use new datatype tpfields
!  P. Wautelet 03/03/2021: budgets: add tbudiachrometadata type (useful to pass more information to Write_diachro)
!  P. Wautelet 04/02/2022: use TSVLIST to manage metadata of scalar variables
!  P. Wautelet 07/04/2022: rewrite types for stations
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_ALLSTATION_n,    ONLY: LDIAG_SURFRAD
use MODD_BUDGET,          ONLY: tbudiachrometadata
USE MODD_CONF,            ONLY: LCARTESIAN
USE MODD_CST,             ONLY: XRV
USE MODD_IO,              ONLY: TFILEDATA
USE MODD_NSV,             ONLY: tsvlist, nsv, nsv_aer, nsv_aerbeg, nsv_aerend, &
                                nsv_dst, nsv_dstbeg, nsv_dstend, nsv_slt, nsv_sltbeg, nsv_sltend
USE MODD_PARAM_n,         ONLY: CRAD, CSURF
USE MODD_PARAMETERS,      ONLY: XUNDEF
USE MODD_STATION_n,       only: NUMBSTAT, TSTATIONS
!
USE MODE_AERO_PSD
USE MODE_DUST_PSD
USE MODE_SALT_PSD
use MODE_WRITE_DIACHRO,   ONLY: Write_diachro
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE ! diachronic file to write
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER     ::  II  ! loop
!
!----------------------------------------------------------------------------
!
DO II = 1, NUMBSTAT
  CALL STATION_DIACHRO_n( TSTATIONS(II) )
ENDDO
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
CONTAINS
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
SUBROUTINE STATION_DIACHRO_n( TPSTATION )

use modd_budget, only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK
use modd_field,  only: NMNHDIM_STATION_TIME, NMNHDIM_STATION_PROC, NMNHDIM_UNUSED, &
                       tfieldmetadata_base, TYPEREAL
use modd_station_n,    only: tstations_time
use modd_type_station, only: tstationdata

TYPE(TSTATIONDATA),   INTENT(IN)       :: TPSTATION
!
!*      0.2  declaration of local variables for diachro
!
REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: ZWORK6 ! contains temporal series
REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: ZW6    ! contains temporal series to write
REAL, DIMENSION(:,:,:,:),     ALLOCATABLE :: ZSV, ZN0, ZSIG, ZRG
REAL, DIMENSION(:,:,:,:,:),     ALLOCATABLE :: ZPTOTA
REAL, DIMENSION(:,:,:),       ALLOCATABLE :: ZRHO
!
INTEGER, DIMENSION(:),            ALLOCATABLE :: IGRID    ! grid indicator
CHARACTER(LEN=  8)                            :: YGROUP   ! group title
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YCOMMENT ! comment string
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YTITLE   ! title
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YUNIT    ! physical unit
!
!!! do not forget to increment the IPROC value if you add diagnostic !!!
INTEGER :: IPROC    ! number of variables records
!!! do not forget to increment the JPROC value if you add diagnostic !!!
INTEGER :: JPROC    ! loop counter
INTEGER :: JRR      ! loop counter
INTEGER :: JSV      ! loop counter
type(tbudiachrometadata)                             :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
!
!----------------------------------------------------------------------------
IF (TPSTATION%XX==XUNDEF) RETURN
IF (TPSTATION%XY==XUNDEF) RETURN
!
IPROC = 8 + SIZE(TPSTATION%XR,2) + SIZE(TPSTATION%XSV,2)

IF (TPSTATION%XX==XUNDEF) IPROC = IPROC + 2
IF (SIZE(TPSTATION%XTKE  )>0) IPROC = IPROC + 1
IF (LDIAG_SURFRAD) THEN
  IF(CSURF=="EXTE") IPROC = IPROC + 10
  IF(CRAD/="NONE")  IPROC = IPROC + 7
END IF
IF (LORILAM) IPROC = IPROC + JPMODE*(3+NSOA+NCARB+NSP)
IF (LDUST) IPROC = IPROC + NMODE_DST*3
IF (LSALT) IPROC = IPROC + NMODE_SLT*3
IF (ANY(TPSTATION%XTSRAD(:)/=XUNDEF))  IPROC = IPROC + 1
IF (ANY(TPSTATION%XSFCO2(:)/=XUNDEF))  IPROC = IPROC + 1
!
ALLOCATE (ZWORK6(1,1,1,SIZE(tstations_time%tpdates),1,IPROC))
ALLOCATE (YCOMMENT(IPROC))
ALLOCATE (YTITLE  (IPROC))
ALLOCATE (YUNIT   (IPROC))
ALLOCATE (IGRID   (IPROC))
!
IGRID  = 1
YGROUP = TPSTATION%CNAME
JPROC = 0
!
!----------------------------------------------------------------------------
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'ZS'
YUNIT    (JPROC) = 'm'
YCOMMENT (JPROC) = 'Orography'
ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XZS
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'P'
YUNIT    (JPROC) = 'Pa'
YCOMMENT (JPROC) = 'Pressure'
ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XP(:)
!
!JPROC = JPROC + 1
!YTITLE   (JPROC) = 'Z'
!YUNIT    (JPROC) = 'm'
!YCOMMENT (JPROC) = 'Z Pos'
!ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XZ
!
IF (LCARTESIAN) THEN
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'X'
  YUNIT    (JPROC) = 'm'
  YCOMMENT (JPROC) = 'X Pos'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XX
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'Y'
  YUNIT    (JPROC) = 'm'
  YCOMMENT (JPROC) = 'Y Pos'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XY
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'U'
  YUNIT    (JPROC) = 'm s-1'
  YCOMMENT (JPROC) = 'Axial velocity'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XZON(:)
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'V'
  YUNIT    (JPROC) = 'm s-1'
  YCOMMENT (JPROC) = 'Transversal velocity'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XMER(:)
ELSE
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'LON'
  YUNIT    (JPROC) = 'degree'
  YCOMMENT (JPROC) = 'Longitude'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XLON
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'LAT'
  YUNIT    (JPROC) = 'degree'
  YCOMMENT (JPROC) = 'Latitude'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XLAT
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'ZON_WIND'
  YUNIT    (JPROC) = 'm s-1'
  YCOMMENT (JPROC) = 'Zonal wind'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XZON(:)
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'MER_WIND'
  YUNIT    (JPROC) = 'm s-1'
  YCOMMENT (JPROC) = 'Meridional wind'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XMER(:)
ENDIF
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'W'
YUNIT    (JPROC) = 'm s-1'
YCOMMENT (JPROC) = 'Air vertical speed'
ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XW(:)
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'Th'
YUNIT    (JPROC) = 'K'
YCOMMENT (JPROC) = 'Potential temperature'
ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XTH(:)
!
IF (LDIAG_SURFRAD) THEN
  IF (CSURF=="EXTE") THEN
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'T2m'
    YUNIT    (JPROC) = 'K'
    YCOMMENT (JPROC) = '2-m temperature'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XT2M(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'Q2m'
    YUNIT    (JPROC) = 'kg kg-1'
    YCOMMENT (JPROC) = '2-m humidity'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XQ2M(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'HU2m'
    YUNIT    (JPROC) = 'percent'
    YCOMMENT (JPROC) = '2-m relative humidity'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XHU2M(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'zon10m'
    YUNIT    (JPROC) = 'm s-1'
    YCOMMENT (JPROC) = '10-m zonal wind'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XZON10M(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'mer10m'
    YUNIT    (JPROC) = 'm s-1'
    YCOMMENT (JPROC) = '10-m meridian wind'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XMER10M(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'RN'
    YUNIT    (JPROC) = 'W m-2'
    YCOMMENT (JPROC) = 'Net radiation'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XRN(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'H'
    YUNIT    (JPROC) = 'W m-2'
    YCOMMENT (JPROC) = 'Sensible heat flux'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XH(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'LE'
    YUNIT    (JPROC) = 'W m-2'
    YCOMMENT (JPROC) = 'Total Latent heat flux'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XLE(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'G'
    YUNIT    (JPROC) = 'W m-2'
    YCOMMENT (JPROC) = 'Storage heat flux'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XGFLUX(:)
    !
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'LEI'
    YUNIT    (JPROC) = 'W m-2'
    YCOMMENT (JPROC) = 'Solid Latent heat flux'
    ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XLEI(:)
  END IF
 IF (CRAD /= 'NONE') THEN
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'SWD'
  YUNIT    (JPROC) = 'W m-2'
  YCOMMENT (JPROC) = 'Downward short-wave radiation'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XSWD(:)
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'SWU'
  YUNIT    (JPROC) = 'W m-2'
  YCOMMENT (JPROC) = 'Upward short-wave radiation'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XSWU(:)
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'LWD'
  YUNIT    (JPROC) = 'W m-2'
  YCOMMENT (JPROC) = 'Downward long-wave radiation'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XLWD(:)
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'LWU'
  YUNIT    (JPROC) = 'W m-2'
  YCOMMENT (JPROC) = 'Upward long-wave radiation'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XLWU(:)
  JPROC = JPROC + 1
  !
  YTITLE   (JPROC) = 'SWDIR'
  YUNIT    (JPROC) = 'W m-2'
  YCOMMENT (JPROC) = 'Downward direct short-wave radiation'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XSWDIR(:)
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'SWDIFF'
  YUNIT    (JPROC) = 'W m-2'
  YCOMMENT (JPROC) = 'Downward diffuse short-wave radiation'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XSWDIFF(:)
  !
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'DSTAOD'
  YUNIT    (JPROC) = 'm'
  YCOMMENT (JPROC) = 'Dust aerosol optical depth'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XDSTAOD(:)
  !
 END IF
ENDIF
!
DO JRR=1,SIZE(TPSTATION%XR,2)
  JPROC = JPROC+1
  YUNIT    (JPROC) = 'kg kg-1'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XR(:,JRR)
  IF (JRR==1) THEN
    YTITLE   (JPROC) = 'Rv'
    YCOMMENT (JPROC) = 'Water vapor mixing ratio'
  ELSE IF (JRR==2) THEN
    YTITLE   (JPROC) = 'Rc'
    YCOMMENT (JPROC) = 'Liquid cloud water mixing ratio'
  ELSE IF (JRR==3) THEN
    YTITLE   (JPROC) = 'Rr'
    YCOMMENT (JPROC) = 'Rain water mixing ratio'
  ELSE IF (JRR==4) THEN
    YTITLE   (JPROC) = 'Ri'
    YCOMMENT (JPROC) = 'Ice cloud water mixing ratio'
  ELSE IF (JRR==5) THEN
    YTITLE   (JPROC) = 'Rs'
    YCOMMENT (JPROC) = 'Snow mixing ratio'
  ELSE IF (JRR==6) THEN
    YTITLE   (JPROC) = 'Rg'
    YCOMMENT (JPROC) = 'Graupel mixing ratio'
  ELSE IF (JRR==7) THEN
    YTITLE   (JPROC) = 'Rh'
    YCOMMENT (JPROC) = 'Hail mixing ratio'
  END IF
END DO
!
IF (SIZE(TPSTATION%XTKE,1)>0) THEN
  JPROC = JPROC+1
  YTITLE   (JPROC) = 'Tke'
  YUNIT    (JPROC) = 'm2 s-2'
  YCOMMENT (JPROC) = 'Turbulent kinetic energy'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XTKE(:)
END IF
!
IF (SIZE(TPSTATION%XSV,2)>=1) THEN
  ! Scalar variables
  DO JSV = 1, NSV
    JPROC = JPROC + 1
    YTITLE(JPROC)   = TRIM( TSVLIST(JSV)%CMNHNAME )
    YCOMMENT(JPROC) = ''
    IF ( TRIM( TSVLIST(JSV)%CUNITS ) == 'ppv' ) THEN
      YUNIT(JPROC)  = 'ppb'
      ZWORK6(1,1,1,:,1,JPROC) = TPSTATION%XSV(:,JSV) * 1.e9 !*1e9 for conversion ppv->ppb
    ELSE
      YUNIT(JPROC)  = TRIM( TSVLIST(JSV)%CUNITS )
      ZWORK6(1,1,1,:,1,JPROC) = TPSTATION%XSV(:,JSV)
    END IF
  END DO

  IF ((LORILAM).AND. .NOT.(ANY(TPSTATION%XP(:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,SIZE(tstations_time%tpdates),NSV_AER))
    ALLOCATE (ZRHO(1,1,SIZE(tstations_time%tpdates)))
    ALLOCATE (ZN0(1,1,SIZE(tstations_time%tpdates),JPMODE))
    ALLOCATE (ZRG(1,1,SIZE(tstations_time%tpdates),JPMODE))
    ALLOCATE (ZSIG(1,1,SIZE(tstations_time%tpdates),JPMODE))
    ALLOCATE (ZPTOTA(1,1,SIZE(tstations_time%tpdates),NSP+NCARB+NSOA,JPMODE))
    ZSV(1,1,:,1:NSV_AER) = TPSTATION%XSV(:,NSV_AERBEG:NSV_AEREND)
    IF (SIZE(TPSTATION%XR,2) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,2)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(:) * ( 1. + XRV/XRD*TPSTATION%XR(:,1) )  &
                                      / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(:)/XP00)**(XRD/XCPD)) )

    CALL PPP2AERO(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0,PCTOTA=ZPTOTA)

    DO JSV=1,JPMODE
      ! mean radius
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A6,I1)')'AERRGA',JSV
      YUNIT    (JPROC) = 'um'
      WRITE(YCOMMENT(JPROC),'(A18,I1)')'RG (nb) AERO MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZRG(1,1,:,JSV)
      ! standard deviation
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A7,I1)')'AERSIGA',JSV
      YUNIT    (JPROC) = '  '
      WRITE(YCOMMENT(JPROC),'(A16,I1)')'SIGMA AERO MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZSIG(1,1,:,JSV)
      ! particles number
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A6,I1)')'AERN0A',JSV
      YUNIT    (JPROC) = 'm-3'
      WRITE(YCOMMENT(JPROC),'(A13,I1)')'N0 AERO MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZN0(1,1,:,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MOC  ',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS OC   AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_OC,JSV)

      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MBC  ',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS BC   AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_BC,JSV)

      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MDST  ',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS DST   AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_DST,JSV)

      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSO4 ',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SO4  AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SO4,JSV)

      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MNO3 ',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS NO3  AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_NO3,JSV)

      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MH2O ',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS H2O  AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_H2O,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MNH3 ',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS NH3  AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_NH3,JSV)
      JPROC = JPROC+1
      IF (NSOA == 10) THEN
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA1',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA1 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA1,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA2',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA2 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA2,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA3',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA3 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA3,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA4',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA4 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA4,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA5',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA5 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA5,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA6',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA6 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA6,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA7',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA7 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA7,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA8',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA8 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA8,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A5,I1)')'MSOA9',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A23,I1)')'MASS SOA9 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA9,JSV)
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A6,I1)')'MSOA10',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT,'(A24,I1)')'MASS SOA10 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC)=ZPTOTA(1,1,:,JP_AER_SOA10,JSV)
      END IF
    ENDDO

    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF

  IF ((LDUST).AND. .NOT.(ANY(TPSTATION%XP(:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,SIZE(tstations_time%tpdates),NSV_DST))
    ALLOCATE (ZRHO(1,1,SIZE(tstations_time%tpdates)))
    ALLOCATE (ZN0(1,1,SIZE(tstations_time%tpdates),NMODE_DST))
    ALLOCATE (ZRG(1,1,SIZE(tstations_time%tpdates),NMODE_DST))
    ALLOCATE (ZSIG(1,1,SIZE(tstations_time%tpdates),NMODE_DST))
    ZSV(1,1,:,1:NSV_DST) = TPSTATION%XSV(:,NSV_DSTBEG:NSV_DSTEND)
    IF (SIZE(TPSTATION%XR,2) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,2)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(:) * ( 1. + XRV/XRD*TPSTATION%XR(:,1) )  &
                                      / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(:)/XP00)**(XRD/XCPD)) )
    CALL PPP2DUST(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,NMODE_DST
      ! mean radius
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A6,I1)')'DSTRGA',JSV
      YUNIT    (JPROC) = 'um'
      WRITE(YCOMMENT(JPROC),'(A18,I1)')'RG (nb) DUST MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZRG(1,1,:,JSV)
      ! standard deviation
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A7,I1)')'DSTSIGA',JSV
      YUNIT    (JPROC) = '  '
      WRITE(YCOMMENT(JPROC),'(A16,I1)')'SIGMA DUST MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZSIG(1,1,:,JSV)
      ! particles number
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A6,I1)')'DSTN0A',JSV
      YUNIT    (JPROC) = 'm-3'
      WRITE(YCOMMENT(JPROC),'(A13,I1)')'N0 DUST MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZN0(1,1,:,JSV)
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF

  IF ((LSALT).AND. .NOT.(ANY(TPSTATION%XP(:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,SIZE(tstations_time%tpdates),NSV_SLT))
    ALLOCATE (ZRHO(1,1,SIZE(tstations_time%tpdates)))
    ALLOCATE (ZN0(1,1,SIZE(tstations_time%tpdates),NMODE_SLT))
    ALLOCATE (ZRG(1,1,SIZE(tstations_time%tpdates),NMODE_SLT))
    ALLOCATE (ZSIG(1,1,SIZE(tstations_time%tpdates),NMODE_SLT))
    ZSV(1,1,:,1:NSV_SLT) = TPSTATION%XSV(:,NSV_SLTBEG:NSV_SLTEND)
    IF (SIZE(TPSTATION%XR,2) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,2)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(:) * ( 1. + XRV/XRD*TPSTATION%XR(:,1) )  &
                                      / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(:)/XP00)**(XRD/XCPD)) )
    CALL PPP2SALT(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,NMODE_SLT
      ! mean radius
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A6,I1)')'SLTRGA',JSV
      YUNIT    (JPROC) = 'um'
      WRITE(YCOMMENT(JPROC),'(A18,I1)')'RG (nb) SALT MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZRG(1,1,:,JSV)
      ! standard deviation
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A7,I1)')'SLTSIGA',JSV
      YUNIT    (JPROC) = '  '
      WRITE(YCOMMENT(JPROC),'(A16,I1)')'SIGMA DUST MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZSIG(1,1,:,JSV)
      ! particles number
      JPROC = JPROC+1
      WRITE(YTITLE(JPROC),'(A6,I1)')'SLTN0A',JSV
      YUNIT    (JPROC) = 'm-3'
      WRITE(YCOMMENT(JPROC),'(A13,I1)')'N0 DUST MODE ',JSV
      ZWORK6 (1,1,1,:,1,JPROC) = ZN0(1,1,:,JSV)
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF
END IF

IF (ANY(TPSTATION%XTSRAD(:)/=XUNDEF)) THEN
  JPROC = JPROC+1
  YTITLE   (JPROC) = 'Tsrad'
  YUNIT    (JPROC) = 'K'
  YCOMMENT (JPROC) = 'Radiative Surface Temperature'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XTSRAD(:)
END IF
!
IF (ANY(TPSTATION%XSFCO2(:)/=XUNDEF)) THEN
  JPROC = JPROC+1
  YTITLE   (JPROC) = 'SFCO2'
  YUNIT    (JPROC) = 'mg m-2 s-1'
  YCOMMENT (JPROC) = 'CO2 Surface Flux'
  ZWORK6 (1,1,1,:,1,JPROC) = TPSTATION%XSFCO2(:)
END IF
!
!----------------------------------------------------------------------------
!
!
ALLOCATE (ZW6(1,1,1,SIZE(tstations_time%tpdates),1,JPROC))
ZW6 = ZWORK6(:,:,:,:,:,:JPROC)
DEALLOCATE(ZWORK6)
!
allocate( tzfields( jproc ) )

tzfields(:)%cmnhname  = ytitle(1 : jproc)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = ytitle(1 : jproc)
tzfields(:)%cunits    = yunit(1 : jproc)
tzfields(:)%ccomment  = ycomment(1 : jproc)
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
tzbudiachro%clevels  (NLVL_GROUP)       = ygroup
tzbudiachro%ccomments(NLVL_GROUP)       = 'Values at position of station ' // Trim( ygroup )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .false.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Point'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Values at position of station ' // Trim( ygroup )

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

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tstations_time%tpdates, zw6 )

deallocate( tzfields )

DEALLOCATE (ZW6)
DEALLOCATE (YCOMMENT)
DEALLOCATE (YTITLE  )
DEALLOCATE (YUNIT   )
DEALLOCATE (IGRID   )
!----------------------------------------------------------------------------
END SUBROUTINE STATION_DIACHRO_n
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
END SUBROUTINE WRITE_STATION_n
