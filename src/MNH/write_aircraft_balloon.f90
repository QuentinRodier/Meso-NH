!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ###########################
MODULE MODI_WRITE_AIRCRAFT_BALLOON
!      ###########################
!
INTERFACE
!
      SUBROUTINE WRITE_AIRCRAFT_BALLOON(TPDIAFILE)
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA), INTENT(IN) :: TPDIAFILE ! file to write
!
END SUBROUTINE WRITE_AIRCRAFT_BALLOON
!
END INTERFACE
!
END MODULE MODI_WRITE_AIRCRAFT_BALLOON
!
!     ##########################################
      SUBROUTINE WRITE_AIRCRAFT_BALLOON(TPDIAFILE)
!     ##########################################
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
USE MODD_CST,             ONLY: XRV
USE MODD_IO,              ONLY: TFILEDATA
USE MODD_PARAMETERS,      ONLY: XUNDEF
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_NSV,             ONLY: tsvlist, nsv, nsv_aer, nsv_aerbeg, nsv_aerend, nsv_dst, nsv_dstbeg, nsv_dstend, &
                                nsv_lima_beg, nsv_lima_end
USE MODD_DIAG_IN_RUN,     ONLY: LDIAG_IN_RUN
!
USE MODE_AERO_PSD
USE MODE_DUST_PSD
USE MODE_MODELN_HANDLER,  ONLY: GET_CURRENT_MODEL_INDEX
use mode_msg
use mode_write_diachro,   only: Write_diachro
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
INTEGER :: IMI    ! current model index
INTEGER :: JI
!
!----------------------------------------------------------------------------
!
IMI=GET_CURRENT_MODEL_INDEX()
!
DO JI = 1, NBALLOONS
  CALL FLYER_DIACHRO( TBALLOONS(JI) )
END DO

DO JI = 1, NAIRCRAFTS
  CALL FLYER_DIACHRO( TAIRCRAFTS(JI) )
END DO
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
CONTAINS
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
SUBROUTINE FLYER_DIACHRO(TPFLYER)

use modd_budget, only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                       tbudiachrometadata
use modd_field,  only: NMNHDIM_LEVEL, NMNHDIM_FLYER_PROC, NMNHDIM_FLYER_TIME, NMNHDIM_UNUSED, &
                       tfieldmetadata_base, TYPEREAL

use modi_aircraft_balloon, only: Aircraft_balloon_longtype_get

CLASS(TFLYERDATA), INTENT(IN)       :: TPFLYER
!
!*      0.2  declaration of local variables for diachro
!
REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: ZWORK6 ! contains temporal serie
REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: ZWORKZ6! contains temporal serie
REAL, DIMENSION(:,:,:,:),     ALLOCATABLE :: ZSV, ZN0, ZSIG, ZRG
REAL, DIMENSION(:,:,:,:,:),   ALLOCATABLE :: ZPTOTA
REAL, DIMENSION(:,:,:),       ALLOCATABLE :: ZRHO
!
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YCOMMENT ! comment string
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YTITLE   ! title
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YUNIT    ! physical unit
!
INTEGER :: IPROC    ! number of variables records
INTEGER :: JPROC    ! loop counter
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YCOMMENTZ! comment string
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YTITLEZ  ! title
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: YUNITZ   ! physical unit
INTEGER :: ISTORE
INTEGER :: IPROCZ   ! number of variables records
INTEGER :: JPROCZ   ! loop counter
INTEGER :: JRR      ! loop counter
INTEGER :: JSV      ! loop counter
INTEGER :: JPT      ! loop counter
INTEGER :: IKU, IK
INTEGER :: JLOOP
type(tbudiachrometadata) :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
!
!----------------------------------------------------------------------------
!
IF (TPFLYER%NMODEL==0) RETURN
IF (ALL(TPFLYER%XX==XUNDEF)) RETURN
IF (COUNT(TPFLYER%XX/=XUNDEF)<=1) RETURN
IF ( IMI /= TPFLYER%NMODEL ) RETURN
!
IKU = SIZE(TPFLYER%XRTZ,2) !number of vertical levels
!
IPROC = 20 + SIZE(TPFLYER%XR,2) + SIZE(TPFLYER%XSV,2) &
       + 2 + SIZE(TPFLYER%XSVW_FLUX,2)
IPROCZ = SIZE(TPFLYER%XRTZ,2)+ SIZE(TPFLYER%XRZ,2)+ SIZE(TPFLYER%XRZ,3)+  SIZE(TPFLYER%XCRARE,2)+ &
         SIZE(TPFLYER%XCRARE_ATT,2)+ SIZE(TPFLYER%XWZ,2) + SIZE(TPFLYER%XFFZ,2)+ &
         SIZE(TPFLYER%XIWCZ,2)+ SIZE(TPFLYER%XLWCZ,2) + SIZE(TPFLYER%XCIZ,2) + &
         SIZE(TPFLYER%XZZ,2)

IF (NSV_LIMA_BEG<=NSV_LIMA_END) IPROCZ= IPROCZ+ SIZE(TPFLYER%XCCZ,2) + SIZE(TPFLYER%XCRZ,2)
IF (SIZE(TPFLYER%XTKE  )>0) IPROC = IPROC + 1
IF (LDIAG_IN_RUN) IPROC = IPROC + 1
IF (LORILAM) IPROC = IPROC + JPMODE*3
IF (LDUST) IPROC = IPROC + NMODE_DST*3
IF (SIZE(TPFLYER%XTSRAD)>0) IPROC = IPROC + 1
!
ISTORE = SIZE( TPFLYER%TFLYER_TIME%TPDATES )

ALLOCATE (ZWORK6(1,1,1,ISTORE,1,IPROC))
ALLOCATE (YCOMMENT(IPROC))
ALLOCATE (YTITLE  (IPROC))
ALLOCATE (YUNIT   (IPROC))
ALLOCATE (ZWORKZ6(1,1,IKU,ISTORE,1,IPROCZ))
ALLOCATE (YCOMMENTZ(IPROCZ))
ALLOCATE (YTITLEZ (IPROCZ))
ALLOCATE (YUNITZ  (IPROCZ))
!
!----------------------------------------------------------------------------
JPROC = 0
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'ZS'
YUNIT    (JPROC) = 'm'
YCOMMENT (JPROC) = 'orography'
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XZS(:)
!
SELECT TYPE ( TPFLYER )
  CLASS IS ( TAIRCRAFTDATA )
    IF (TPFLYER%LALTDEF) THEN
      JPROC = JPROC + 1
      YTITLE   (JPROC) = 'P'
      YUNIT    (JPROC) = 'Pascal'
      YCOMMENT (JPROC) = 'pressure'
      ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XP(:)
    ELSE
      JPROC = JPROC + 1
      YTITLE   (JPROC) = 'Z'
      YUNIT    (JPROC) = 'm'
      YCOMMENT (JPROC) = 'altitude'
      ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XZ(:)
    ENDIF

  CLASS IS ( TBALLOONDATA )
    JPROC = JPROC + 1
    YTITLE   (JPROC) = 'Z'
    YUNIT    (JPROC) = 'm'
    YCOMMENT (JPROC) = 'altitude'
    ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XZ(:)

END SELECT
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'LON'
YUNIT    (JPROC) = 'degree'
YCOMMENT (JPROC) = 'longitude'
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XLON(:)
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'LAT'
YUNIT    (JPROC) = 'degree'
YCOMMENT (JPROC) = 'latitude'
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XLAT(:)
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'ZON_WIND'
YUNIT    (JPROC) = 'm s-1'
YCOMMENT (JPROC) = 'zonal wind'
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XZON(:)
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'MER_WIND'
YUNIT    (JPROC) = 'm s-1'
YCOMMENT (JPROC) = 'meridian wind'
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XMER(:)
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'W'
YUNIT    (JPROC) = 'm s-1'
YCOMMENT (JPROC) = 'air vertical speed' 
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XW(:)
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'Th'
YUNIT    (JPROC) = 'K'
YCOMMENT (JPROC) = 'potential temperature' 
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XTH(:)
!
DO JRR=1,SIZE(TPFLYER%XR,2)
  JPROC = JPROC+1
  YUNIT    (JPROC) = 'kg kg-1'
  ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XR(:,JRR)
  IF (JRR==1) THEN
    YTITLE   (JPROC) = 'Rv'
    YCOMMENT (JPROC) = 'water vapor mixing ratio' 
  ELSE IF (JRR==2) THEN
    YTITLE   (JPROC) = 'Rc'
    YCOMMENT (JPROC) = 'liquid cloud water mixing ratio' 
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
!add cloud liquid water content in g/m3 to compare to measurements from FSSP
!IF (.NOT.(ANY(TPFLYER%XP(:) == 0.))) THEN
ALLOCATE (ZRHO(1,1,ISTORE))
IF (SIZE(TPFLYER%XR,2) >1) THEN !cloud water is present
  ZRHO(1,1,:) = 0.
  DO JRR=1,SIZE(TPFLYER%XR,2)
    ZRHO(1,1,:) = ZRHO(1,1,:) + TPFLYER%XR(:,JRR)
  ENDDO
  ZRHO(1,1,:) = TPFLYER%XTH(:) * ( 1. + XRV/XRD*TPFLYER%XR(:,1) )  &
                                / ( 1. + ZRHO(1,1,:)              )
  DO JPT=1,ISTORE
    IF (TPFLYER%XP(JPT) == 0.) THEN
      ZRHO(1,1,JPT) = 0.
    ELSE
      ZRHO(1,1,JPT) =  TPFLYER%XP(JPT) / &
               (XRD *ZRHO(1,1,JPT) *((TPFLYER%XP(JPT)/XP00)**(XRD/XCPD))  )
    ENDIF
  ENDDO
  JPROC = JPROC + 1
  YTITLE   (JPROC) = 'LWC'
  YUNIT    (JPROC) = 'g m-3'
  YCOMMENT (JPROC) = 'cloud liquid water content'
  ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XR(:,2)*ZRHO(1,1,:)*1.E3
  DEALLOCATE (ZRHO)
ENDIF
!ENDIF
!
IF (SIZE(TPFLYER%XTKE)>0) THEN
  JPROC = JPROC+1
  YTITLE   (JPROC) = 'Tke'
  YUNIT    (JPROC) = 'm2 s-2'
  YCOMMENT (JPROC) = 'Turbulent kinetic energy' 
  ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XTKE(:)
END IF
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'H_FLUX'
YUNIT    (JPROC) = 'W m-2'
YCOMMENT (JPROC) = 'sensible flux' 
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XTHW_FLUX(:)
!
JPROC = JPROC + 1
YTITLE   (JPROC) = 'LE_FLUX'
YUNIT    (JPROC) = 'W m-2'
YCOMMENT (JPROC) = 'latent flux' 
ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XRCW_FLUX(:)
!
DO JSV=1,SIZE(TPFLYER%XSVW_FLUX,2)
  JPROC = JPROC + 1
!PW: titre a modifier pour recuperer nom variables scalaires depuis TSVLIST?
  WRITE ( YTITLE(JPROC), FMT = '( A7, I3.3 )' ) 'SV_FLUX', JSV
  YUNIT    (JPROC) = 'SVUNIT m s-1'
  YCOMMENT (JPROC) = 'scalar flux' 
  ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XSVW_FLUX(:,JSV)
END DO
IF (LDIAG_IN_RUN) THEN
  JPROC = JPROC+1
  YTITLE   (JPROC) = 'Tke_Diss'
  YUNIT    (JPROC) = 'm2 s-2'
  YCOMMENT (JPROC) = 'TKE dissipation rate' 
  ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XTKE_DISS(:)
ENDIF
!
IF (SIZE(TPFLYER%XSV,2)>=1) THEN
  ! Scalar variables
  DO JSV = 1, NSV
    JPROC = JPROC + 1

    YTITLE(JPROC)   = TRIM( TSVLIST(JSV)%CMNHNAME )
    YCOMMENT(JPROC) = ''
    IF ( TRIM( TSVLIST(JSV)%CUNITS ) == 'ppv' ) THEN
      YUNIT(JPROC)  = 'ppb'
      ZWORK6(1,1,1,:,1,JPROC) = TPFLYER%XSV(:,JSV) * 1.e9 !*1e9 for conversion ppv->ppb
    ELSE
      YUNIT(JPROC)  = TRIM( TSVLIST(JSV)%CUNITS )
      ZWORK6(1,1,1,:,1,JPROC) = TPFLYER%XSV(:,JSV)
    END IF
  END DO

  IF ((LORILAM).AND. .NOT.(ANY(TPFLYER%XP(:) == 0.))) THEN

    ALLOCATE (ZSV(1,1,ISTORE,NSV_AER))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,JPMODE))
    ALLOCATE (ZRG(1,1,ISTORE,JPMODE))
    ALLOCATE (ZSIG(1,1,ISTORE,JPMODE))
    ALLOCATE (ZPTOTA(1,1,ISTORE,NSP+NCARB+NSOA,JPMODE))
    ZSV(1,1,:,1:NSV_AER) = TPFLYER%XSV(:,NSV_AERBEG:NSV_AEREND)
    IF (SIZE(TPFLYER%XR,2) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPFLYER%XR,2)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPFLYER%XR(:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPFLYER%XTH(:) * ( 1. + XRV/XRD*TPFLYER%XR(:,1) )  &
                                  / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPFLYER%XTH(:)
    ENDIF
    ZRHO(1,1,:) =  TPFLYER%XP(:) / &
                  (XRD *ZRHO(1,1,:) *((TPFLYER%XP(:)/XP00)**(XRD/XCPD))  )
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
      ! mass concentration in microg/m3
      ! sulfate
      JPROC = JPROC + 1
      WRITE(YTITLE(JPROC),'(A4,I1)')'MSO4',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SO4 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SO4,JSV)
      ! nitrate
      JPROC = JPROC + 1
      WRITE(YTITLE(JPROC),'(A4,I1)')'MNO3',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS NO3 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_NO3,JSV)
      ! amoniac
      JPROC = JPROC + 1
      WRITE(YTITLE(JPROC),'(A4,I1)')'MNH3',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS NH3 AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_NH3,JSV)
      ! water
      JPROC = JPROC + 1
      WRITE(YTITLE(JPROC),'(A4,I1)')'MH2O',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS H2O AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_H2O,JSV)
      IF (NSOA .EQ. 10) THEN
        ! SOA1
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA1',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA1 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA1,JSV)
        ! SOA2
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA2',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA2 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA2,JSV)
        ! SOA3
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA3',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA3 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA3,JSV)
        ! SOA4
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA4',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA4 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA4,JSV)
        ! SOA5
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA5',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA5 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA5,JSV)
        ! SOA6
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA6',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA6 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA6,JSV)
        ! SOA7
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA7',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA7 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA7,JSV)
        ! SOA8
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA8',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA8 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA8,JSV)
        ! SOA9
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA9',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA9 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA9,JSV)
        ! SOA10
        JPROC = JPROC + 1
        WRITE(YTITLE(JPROC),'(A4,I1)')'MSOA10',JSV
        YUNIT    (JPROC) = 'ug m-3'
        WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS SOA10 AEROSOL MODE ',JSV
        ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_SOA10,JSV)
      ENDIF
      ! OC
      JPROC = JPROC + 1
      WRITE(YTITLE(JPROC),'(A4,I1)')'MOC',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS OC AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_OC,JSV)
      ! BC
      JPROC = JPROC + 1
      WRITE(YTITLE(JPROC),'(A4,I1)')'MBC',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS BC AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_BC,JSV)
      ! dust
      JPROC = JPROC + 1
      WRITE(YTITLE(JPROC),'(A4,I1)')'MDUST',JSV
      YUNIT    (JPROC) = 'ug m-3'
      WRITE(YCOMMENT(JPROC),'(A22,I1)')'MASS DUST AEROSOL MODE ',JSV
      ZWORK6(1,1,1,:,1,JPROC) = ZPTOTA(1,1,:,JP_AER_DST,JSV)
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG,ZPTOTA)
  END IF

  IF ((LDUST).AND. .NOT.(ANY(TPFLYER%XP(:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,ISTORE,NSV_DST))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,NMODE_DST))
    ALLOCATE (ZRG(1,1,ISTORE,NMODE_DST))
    ALLOCATE (ZSIG(1,1,ISTORE,NMODE_DST))
    ZSV(1,1,:,1:NSV_DST) = TPFLYER%XSV(:,NSV_DSTBEG:NSV_DSTEND)
    IF (SIZE(TPFLYER%XR,2) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPFLYER%XR,2)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPFLYER%XR(:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPFLYER%XTH(:) * ( 1. + XRV/XRD*TPFLYER%XR(:,1) )  &
                                          / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPFLYER%XTH(:)
    ENDIF
    ZRHO(1,1,:) =  TPFLYER%XP(:) / &
                  (XRD *ZRHO(1,1,:) *((TPFLYER%XP(:)/XP00)**(XRD/XCPD)) )
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
ENDIF
!
IF (SIZE(TPFLYER%XTSRAD)>0) THEN
  JPROC = JPROC+1
  YTITLE   (JPROC) = 'Tsrad'
  YUNIT    (JPROC) = 'K'
  YCOMMENT (JPROC) = 'Radiative Surface Temperature'
  ZWORK6 (1,1,1,:,1,JPROC) = TPFLYER%XTSRAD(:)
END IF
!
DO IK=1, IKU
!
  JPROCZ=0
!
  JPROCZ = JPROCZ + 1
  YTITLEZ  (JPROCZ) = 'Rt'
  YUNITZ   (JPROCZ) = 'kg kg-1'
  YCOMMENTZ(JPROCZ) = '1D Total hydrometeor mixing ratio'
  ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XRTZ(:,IK)
!
  DO JRR=1,SIZE(TPFLYER%XRZ,3)
    JPROCZ = JPROCZ+1
    YUNITZ    (JPROCZ) = 'kg kg-1'
    ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XRZ(:,IK,JRR)
    IF (JRR==1) THEN
      YTITLEZ   (JPROCZ) = 'Rv'
      YCOMMENTZ (JPROCZ) = '1D water vapor mixing ratio' 
    ELSE IF (JRR==2) THEN
      YTITLEZ   (JPROCZ) = 'Rc'
      YCOMMENTZ (JPROCZ) = '1D liquid cloud water mixing ratio' 
    ELSE IF (JRR==3) THEN
      YTITLEZ   (JPROCZ) = 'Rr'
      YCOMMENTZ (JPROCZ) = '1D Rain water mixing ratio' 
    ELSE IF (JRR==4) THEN
      YTITLEZ   (JPROCZ) = 'Ri'
      YCOMMENTZ (JPROCZ) = '1D Ice cloud water mixing ratio' 
    ELSE IF (JRR==5) THEN
      YTITLEZ   (JPROCZ) = 'Rs'
      YCOMMENTZ (JPROCZ) = '1D Snow mixing ratio' 
    ELSE IF (JRR==6) THEN
      YTITLEZ   (JPROCZ) = 'Rg'
      YCOMMENTZ (JPROCZ) = '1D Graupel mixing ratio' 
    ELSE IF (JRR==7) THEN
      YTITLEZ   (JPROCZ) = 'Rh'
      YCOMMENTZ (JPROCZ) = '1D Hail mixing ratio' 
    END IF
  END DO
!
  JPROCZ = JPROCZ + 1
  YTITLEZ  (JPROCZ) = 'FF'
  YUNITZ   (JPROCZ) = 'm s-1'
  YCOMMENTZ(JPROCZ) = 'Horizontal wind'
  ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XFFZ(:,IK)
!
  JPROCZ = JPROCZ + 1
  YTITLEZ  (JPROCZ) = 'IWC'
  YUNITZ   (JPROCZ) = 'kg m-3'
  YCOMMENTZ(JPROCZ) = 'Ice water content'
  ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XIWCZ(:,IK)
!
  JPROCZ = JPROCZ + 1
  YTITLEZ  (JPROCZ) = 'LWC'
  YUNITZ   (JPROCZ) = 'kg m-3'
  YCOMMENTZ(JPROCZ) = 'Liquid water content'
  ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XLWCZ(:,IK)
!
  IF (NSV_LIMA_BEG/=NSV_LIMA_END) THEN
    JPROCZ = JPROCZ + 1
    YTITLEZ  (JPROCZ) = 'CIT'
    YUNITZ   (JPROCZ) = 'm-3'
    YCOMMENTZ(JPROCZ) = 'Ice concentration'
    ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XCIZ(:,IK)
  ELSE
    JPROCZ = JPROCZ + 1
    YTITLEZ  (JPROCZ) = 'CCLOUDT'
    YUNITZ   (JPROCZ) = 'kg-1'
    YCOMMENTZ(JPROCZ) = 'liquid cloud concentration'
    ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XCCZ(:,IK)
!
    JPROCZ = JPROCZ + 1
    YTITLEZ  (JPROCZ) = 'CRAINT'
    YUNITZ   (JPROCZ) = 'kg-1'
    YCOMMENTZ(JPROCZ) = 'Rain concentration'
    ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XCRZ(:,IK)
!
    JPROCZ = JPROCZ + 1
    YTITLEZ  (JPROCZ) = 'CICET'
    YUNITZ   (JPROCZ) = 'kg-1'
    YCOMMENTZ(JPROCZ) = 'Ice concentration'
    ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XCIZ(:,IK)
 ENDIF
!
  JPROCZ = JPROCZ + 1
  YTITLEZ  (JPROCZ) = 'RARE'
  YUNITZ   (JPROCZ) = 'dBZ'
  YCOMMENTZ(JPROCZ) = '1D cloud radar reflectivity'
  ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XCRARE(:,IK)
  JPROCZ = JPROCZ + 1
  YTITLEZ  (JPROCZ) = 'RAREatt'
  YUNITZ   (JPROCZ) = 'dBZ'
  YCOMMENTZ(JPROCZ) = '1D cloud radar attenuated reflectivity' 
  ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XCRARE_ATT(:,IK)
  JPROCZ = JPROCZ + 1
  YTITLEZ  (JPROCZ) = 'W'
  YUNITZ   (JPROCZ) = 'm s-1'
  YCOMMENTZ(JPROCZ) = '1D vertical velocity' 
  ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XWZ(:,IK)
  JPROCZ = JPROCZ + 1
  YTITLEZ  (JPROCZ) = 'Z'
  YUNITZ   (JPROCZ) = 'm'
  YCOMMENTZ(JPROCZ) = '1D altitude above sea'
  ZWORKZ6 (1,1,IK,:,1,JPROCZ) = TPFLYER%XZZ(:,IK)
END DO
!----------------------------------------------------------------------------

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
tzbudiachro%clevels  (NLVL_GROUP)       = Trim( tpflyer%ctitle )
tzbudiachro%ccomments(NLVL_GROUP)       = 'Values for flyer ' // Trim( tpflyer%ctitle )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .true.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Point'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Values at position of flyer ' // Trim( tpflyer%ctitle )

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

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tpflyer%tflyer_time%tpdates, zwork6(:,:,:,:,:,:jproc), &
                    tpflyer = tpflyer                                                                        )

deallocate( tzfields )

allocate( tzfields( jprocz ) )

tzfields(:)%cmnhname  = ytitlez(1 : jprocz)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = ytitlez(1 : jprocz)
tzfields(:)%cunits    = yunitz(1 : jprocz)
tzfields(:)%ccomment  = ycommentz(1 : jprocz)
tzfields(:)%ngrid     = 0
tzfields(:)%ntype     = TYPEREAL
tzfields(:)%ndims     = 3
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(3) = NMNHDIM_LEVEL
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
tzbudiachro%clevels  (NLVL_GROUP)       = Trim( tpflyer%ctitle )
tzbudiachro%ccomments(NLVL_GROUP)       = 'Values for flyer ' // Trim( tpflyer%ctitle )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .true.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Vertical_profile'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Vertical profiles at position of flyer ' // Trim( tpflyer%ctitle )

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

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tpflyer%tflyer_time%tpdates, zworkz6(:,:,:,:,:,:jprocz), &
                    tpflyer = tpflyer                                                                          )

deallocate( tzfields )

DEALLOCATE (ZWORK6)
DEALLOCATE (YCOMMENT)
DEALLOCATE (YTITLE  )
DEALLOCATE (YUNIT   )
DEALLOCATE (ZWORKZ6)
DEALLOCATE (YCOMMENTZ)
DEALLOCATE (YTITLEZ )
DEALLOCATE (YUNITZ  )
!----------------------------------------------------------------------------
END SUBROUTINE FLYER_DIACHRO
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
END SUBROUTINE WRITE_AIRCRAFT_BALLOON
