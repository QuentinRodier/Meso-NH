!MNH_LIC Copyright 2002-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  P. Tulet    15/02/2002
!
!  Modifications
!  G. Delautier      2016: LIMA
!  C. Lac         10/2016: add visibility diagnostics for fog
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  J. Escobar  16/08/2018: From Pierre & Maud , correction use CNAMES(JSV-NSV_CHEMBEG+1)
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 09/10/2020: Write_diachro: use new datatype tpfields
!  P. Wautelet 03/03/2021: budgets: add tbudiachrometadata type (useful to pass more information to Write_diachro)
!  P. Wautelet 11/03/2021: bugfix: correct name for NSV_LIMA_IMM_NUCL
!  P. Wautelet 05/07/2021: reorganisation to store point values correctly (not in vertical profiles)
!  M. Taufour     07/2021: modify RARE for hydrometeors containing ice and add bright band calculation for RARE
!-----------------------------------------------------------------
!      ###########################
MODULE MODE_WRITE_PROFILER_n
!      ###########################
!
implicit none

private

public :: WRITE_PROFILER_n

CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: CCOMMENT ! comment string
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: CTITLE   ! title
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE :: CUNIT    ! physical unit

REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: XWORK6   ! contains temporal serie

contains
!
!#####################################
SUBROUTINE WRITE_PROFILER_n(TPDIAFILE)
!#####################################
!
!
!****  *WRITE_PROFILER* - write the balloon and aircraft trajectories and records
!                      in the diachronic file
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_IO,         ONLY: TFILEDATA
use MODD_PROFILER_n, only: NUMBPROFILER
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
INTEGER :: JI
!
!----------------------------------------------------------------------------
!
DO JI = 1, NUMBPROFILER
  CALL PROFILER_DIACHRO_n( TPDIAFILE, JI )
ENDDO
!
!----------------------------------------------------------------------------
END SUBROUTINE WRITE_PROFILER_n
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
SUBROUTINE PROFILER_DIACHRO_n( TPDIAFILE, KI )

use modd_budget,          only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                                tbudiachrometadata
USE MODD_DIAG_IN_RUN,     only: LDIAG_IN_RUN
USE MODD_DUST,            ONLY: CDUSTNAMES, LDUST, NMODE_DST
USE MODD_CH_AEROSOL,      ONLY: CAERONAMES, LORILAM, JPMODE
USE MODD_CH_M9_n,         ONLY: CNAMES
USE MODD_CST,             ONLY: XRV
USE MODD_ELEC_DESCR,      ONLY: CELECNAMES
use modd_field,           only: NMNHDIM_LEVEL, NMNHDIM_PROFILER_TIME, NMNHDIM_PROFILER_PROC, NMNHDIM_UNUSED, &
                                tfieldmetadata_base, TYPEREAL
USE MODD_ICE_C1R3_DESCR,  ONLY: C1R3NAMES
USE MODD_IO,              ONLY: TFILEDATA
USE MODD_LG,              ONLY: CLGNAMES
USE MODD_NSV
USE MODD_PARAMETERS,      ONLY: XUNDEF
USE MODD_PARAM_LIMA,      ONLY: NINDICE_CCN_IMM,NMOD_CCN,NMOD_IFN,NMOD_IMM
USE MODD_PARAM_LIMA_COLD, ONLY: CLIMA_COLD_NAMES
USE MODD_PARAM_LIMA_WARM, ONLY: CLIMA_WARM_NAMES, CAERO_MASS
USE MODD_PARAM_n,         ONLY: CRAD
USE MODD_PROFILER_n
USE MODD_RADIATIONS_n,    ONLY: NAER
USE MODD_RAIN_C2R2_DESCR, ONLY: C2R2NAMES
USE MODD_SALT,            ONLY: CSALTNAMES, LSALT
USE MODD_TYPE_PROFILER
!
USE MODE_AERO_PSD
USE MODE_DUST_PSD
use mode_write_diachro,   only: Write_diachro
!
TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE ! diachronic file to write
INTEGER,          INTENT(IN) :: KI
!
!*      0.2  declaration of local variables for diachro
!
character(len=2)                                     :: yidx
character(len=100)                                   :: ycomment
character(len=100)                                   :: yname
CHARACTER(LEN=:),                        allocatable :: YGROUP   ! group title
INTEGER                                              :: IKU
INTEGER                                              :: IPROC    ! number of variables records
INTEGER                                              :: JPROC
INTEGER                                              :: JRR      ! loop counter
INTEGER                                              :: JSV      ! loop counter
integer                                              :: ji
integer                                              :: irr !Number of moist variables
REAL, DIMENSION(:,:,:),                  ALLOCATABLE :: ZRHO
REAL, DIMENSION(:,:,:,:),                ALLOCATABLE :: ZSV, ZN0, ZSIG, ZRG
type(tbudiachrometadata)                             :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
!
!----------------------------------------------------------------------------
!
IF (TPROFILER%X(KI)==XUNDEF) RETURN
IF (TPROFILER%Y(KI)==XUNDEF) RETURN
!
IKU = SIZE(TPROFILER%W,2) !Number of vertical levels
!
!IPROC is too large (not a big problem) due to the separation between vertical profiles and point values
IPROC = 25 + SIZE(TPROFILER%R,4) + SIZE(TPROFILER%SV,4)
IF (LDIAG_IN_RUN) IPROC = IPROC + 15
IF (LORILAM) IPROC = IPROC + JPMODE*3
IF (LDUST) IPROC = IPROC + NMODE_DST*3
IF (LDUST .OR. LORILAM .OR. LSALT) IPROC=IPROC+NAER
IF (SIZE(TPROFILER%TKE  )>0) IPROC = IPROC + 1
!
ALLOCATE (XWORK6(1,1,IKU,size(tprofiler%tpdates),1,IPROC))
ALLOCATE (CCOMMENT(IPROC))
ALLOCATE (CTITLE  (IPROC))
ALLOCATE (CUNIT   (IPROC))
!
YGROUP = TPROFILER%NAME(KI)
!
!----------------------------------------------------------------------------
!Treat vertical profiles
jproc = 0

call Add_profile( 'Th',       'Potential temperature',         'K',      tprofiler%th        )
call Add_profile( 'Thv',      'Virtual Potential temperature', 'K',      tprofiler%thv       )
call Add_profile( 'VISI',     'Visibility',                    'km',     tprofiler%visi      )
call Add_profile( 'VISIKUN',  'Visibility Kunkel',             'km',     tprofiler%visikun   )
call Add_profile( 'RARE',     'Radar reflectivity',            'dBZ',    tprofiler%crare     )
call Add_profile( 'RAREatt',  'Radar attenuated reflectivity', 'dBZ',    tprofiler%crare_att )
call Add_profile( 'P',        'Pressure',                      'Pa',     tprofiler%p         )
call Add_profile( 'ALT',      'Altitude',                      'm',      tprofiler%zz        )
call Add_profile( 'ZON_WIND', 'Zonal wind',                    'm s-1',  tprofiler%zon       )
call Add_profile( 'MER_WIND', 'Meridional wind',               'm s-1',  tprofiler%mer       )
call Add_profile( 'FF',       'Wind intensity',                'm s-1',  tprofiler%ff        )
call Add_profile( 'DD',       'Wind direction',                'degree', tprofiler%dd        )
call Add_profile( 'W',        'Air vertical speed',            'm s-1',  tprofiler%w         )

if ( ldiag_in_run ) &
  call Add_profile( 'TKE_DISS', 'TKE dissipation rate', 'm2 s-2', tprofiler% tke_diss )

if ( Size( tprofiler%ciz, 1 ) > 0 ) &
  call Add_profile( 'CIT',      'Ice concentration',    'kg-3',   tprofiler%ciz )

irr = Size( tprofiler%r )
if ( irr >= 1 ) call Add_profile( 'Rv', 'Water vapor mixing ratio',        'kg kg-1', tprofiler%r(:,:,:,1) )
if ( irr >= 2 ) call Add_profile( 'Rc', 'Liquid cloud water mixing ratio', 'kg kg-1', tprofiler%r(:,:,:,2) )
if ( irr >= 3 ) call Add_profile( 'Rr', 'Rain water mixing ratio',         'kg kg-1', tprofiler%r(:,:,:,3) )
if ( irr >= 4 ) call Add_profile( 'Ri', 'Ice cloud water mixing ratio',    'kg kg-1', tprofiler%r(:,:,:,4) )
if ( irr >= 5 ) call Add_profile( 'Rs', 'Snow mixing ratio',               'kg kg-1', tprofiler%r(:,:,:,5) )
if ( irr >= 6 ) call Add_profile( 'Rg', 'Graupel mixing ratio',            'kg kg-1', tprofiler%r(:,:,:,6) )
if ( irr >= 7 ) call Add_profile( 'Rh', 'Hail mixing ratio',               'kg kg-1', tprofiler%r(:,:,:,7) )

call Add_profile( 'Rhod', 'Density of dry air in moist', 'kg m-3', tprofiler%rhod )
if ( Size( tprofiler%tke, 1 ) > 0 ) &
  call Add_profile( 'Tke', 'Turbulent kinetic energy', 'm2 s-2', tprofiler%tke )

if ( Size( tprofiler%sv, 4 ) > 0  ) then
  ! User scalar variables
  do jsv = 1, nsv_user
    Write ( yname, fmt = '( a2, i3.3 )' ) 'Sv', jsv
    call Add_profile( yname, '', 'kg kg-1', tprofiler%sv(:,:,:,jsv) )
  end do
 ! Passive pollutant  scalar variables
  do jsv = nsv_ppbeg, nsv_ppend
    Write ( yname, fmt = '( a2, i3.3 )' ) 'Sv', jsv
    call Add_profile( yname, '', '1', tprofiler%sv(:,:,:,jsv) )
  end do
 ! microphysical C2R2 scheme scalar variables
  do jsv = nsv_ppbeg, nsv_ppend
    call Add_profile( Trim( c2r2names(jsv - nsv_c2r2beg + 1) ), '', 'm-3', tprofiler%sv(:,:,:,jsv) )
  end do
  ! microphysical C3R5 scheme additional scalar variables
  do jsv = nsv_c1r3beg, nsv_c1r3end
    call Add_profile( Trim( c1r3names(jsv - nsv_c1r3beg + 1) ), '', 'm-3', tprofiler%sv(:,:,:,jsv) )
  end do
  ! LIMA variables
  do jsv = nsv_lima_beg, nsv_lima_end
    if ( jsv == nsv_lima_nc ) then
      yname = Trim( clima_warm_names(1) ) // 'T'
    else if ( jsv == nsv_lima_nr ) then
      yname = Trim( clima_warm_names(2) ) // 'T'
    else if ( jsv >= nsv_lima_ccn_free .and. jsv < nsv_lima_ccn_free + nmod_ccn ) then
      Write( yidx, '( i2.2 )' ) jsv - nsv_lima_ccn_free + 1
      yname = Trim( clima_warm_names(3) ) // yidx // 'T'
    else if ( jsv >= nsv_lima_ccn_acti .and. jsv < nsv_lima_ccn_acti + nmod_ccn ) then
      Write( yidx, '( i2.2 )' ) jsv - nsv_lima_ccn_acti + 1
      yname = Trim( clima_warm_names(4) ) // yidx // 'T'
    else if ( jsv == nsv_lima_scavmass ) then
      yname = Trim( caero_mass(1) ) // 'T'
    else if ( jsv == nsv_lima_ni ) then
      yname = Trim( clima_cold_names(1) ) // 'T'
    else if ( jsv >= nsv_lima_ifn_free .and. jsv < nsv_lima_ifn_free + nmod_ifn ) then
      Write( yidx, '( i2.2 )' ) jsv - nsv_lima_ifn_free + 1
      yname = Trim( clima_cold_names(2) ) // yidx // 'T'
    else if ( jsv >= nsv_lima_ifn_nucl .and. jsv < nsv_lima_ifn_nucl + nmod_ifn ) then
      Write( yidx, '( i2.2 )' ) jsv - nsv_lima_ifn_nucl + 1
      yname = Trim( clima_cold_names(3) ) // yidx // 'T'
    else if ( jsv >= nsv_lima_imm_nucl .and. jsv < nsv_lima_imm_nucl + nmod_imm ) then
      write( yidx, '( i2.2 )' ) nindice_ccn_imm(jsv - nsv_lima_imm_nucl + 1)
      yname = Trim( clima_cold_names(4) ) // yidx // 'T'
    else if ( jsv == nsv_lima_hom_haze ) then
      yname = Trim( clima_cold_names(5) ) // 'T'
    else if ( jsv == nsv_lima_spro ) then
      yname = Trim( clima_warm_names(5) ) // 'T'
    end if
    call Add_profile( yname, '', 'kg-1', tprofiler%sv(:,:,:,jsv) )
  end do
  ! electrical scalar variables
  do jsv = nsv_elecbeg, nsv_elecend
    call Add_profile( Trim( celecnames(jsv - nsv_elecbeg + 1) ), '', 'C', tprofiler%sv(:,:,:,jsv) )
  end do
  ! chemical scalar variables
  do jsv = nsv_chembeg, nsv_chemend
    Write( ycomment, '( a5, a3, i3.3 )' ) 'T(s) ', 'SVT', jsv
    call Add_profile( Trim( cnames(jsv - nsv_chembeg + 1) ), ycomment, 'ppb', tprofiler%sv(:,:,:,jsv) * 1.e9 )
  end do
  IF ( LORILAM .AND. .NOT.(ANY(TPROFILER%P(:,:,KI) == 0.)) ) THEN
    ALLOCATE (ZSV (1,iku,size(tprofiler%tpdates),NSV_AER))
    ALLOCATE (ZRHO(1,iku,size(tprofiler%tpdates)))
    ALLOCATE (ZN0 (1,iku,size(tprofiler%tpdates),JPMODE))
    ALLOCATE (ZRG (1,iku,size(tprofiler%tpdates),JPMODE))
    ALLOCATE (ZSIG(1,iku,size(tprofiler%tpdates),JPMODE))
    do ji = 1, iku
      ZSV(1,ji,:,1:NSV_AER) = TPROFILER%SV(:,ji,KI,NSV_AERBEG:NSV_AEREND)
    end do
    IF (SIZE(TPROFILER%R,4) >0) THEN
      ZRHO(1,:,:) = 0.
      do ji = 1, iku
        DO JRR=1,SIZE(TPROFILER%R,4)
          ZRHO(1,ji,:) = ZRHO(1,ji,:) + TPROFILER%R(:,ji,KI,JRR)
        ENDDO
        ZRHO(1,ji,:) = TPROFILER%TH(:,ji,KI) * ( 1. + XRV/XRD*TPROFILER%R(:,ji,KI,1) )  &
                                             / ( 1. + ZRHO(1,ji,:)                )
      end do
    ELSE
      do ji = 1, iku
        ZRHO(1,ji,:) = TPROFILER%TH(:,ji,KI)
      end do
    ENDIF
    do ji = 1, iku
      ZRHO(1,ji,:) =  TPROFILER%P(:,ji,KI) / &
                      (XRD *ZRHO(1,ji,:) *((TPROFILER%P(:,ji,KI)/XP00)**(XRD/XCPD)) )
    end do
    CALL PPP2AERO(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,JPMODE
      ! mean radius
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A6,I1)')'AERRGA',JSV
      CUNIT(JPROC) = 'um'
      WRITE(CCOMMENT(JPROC),'(A18,I1)')'RG (nb) AERO MODE ',JSV
      do ji = 1, iku
        XWORK6(1,1,ji,:,1,JPROC) = ZRG(1,ji,:,JSV)
      end do
      ! standard deviation
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A7,I1)')'AERSIGA',JSV
      CUNIT(JPROC) = '  '
      WRITE(CCOMMENT(JPROC),'(A16,I1)')'SIGMA AERO MODE ',JSV
      do ji = 1, iku
        XWORK6(1,1,ji,:,1,JPROC) = ZSIG(1,ji,:,JSV)
      end do
      ! particles number
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A6,I1)')'AERN0A',JSV
      CUNIT(JPROC) = 'm-3'
      WRITE(CCOMMENT(JPROC),'(A13,I1)')'N0 AERO MODE ',JSV
      do ji = 1, iku
        XWORK6(1,1,ji,:,1,JPROC) = ZN0(1,ji,:,JSV)
      end do
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF
  ! dust scalar variables
  do jsv = nsv_dstbeg, nsv_dstend
    call Add_profile( Trim( cdustnames(jsv - nsv_dstbeg + 1) ), '', 'ppb', tprofiler%sv(:,:,:,jsv) * 1.e9 )
  end do
  IF ((LDUST).AND. .NOT.(ANY(TPROFILER%P(:,:,KI) == 0.))) THEN
    ALLOCATE (ZSV (1,iku,size(tprofiler%tpdates),NSV_DST))
    ALLOCATE (ZRHO(1,iku,size(tprofiler%tpdates)))
    ALLOCATE (ZN0 (1,iku,size(tprofiler%tpdates),NMODE_DST))
    ALLOCATE (ZRG (1,iku,size(tprofiler%tpdates),NMODE_DST))
    ALLOCATE (ZSIG(1,iku,size(tprofiler%tpdates),NMODE_DST))
    do ji = 1, iku
      ZSV(1,ji,:,1:NSV_DST) = TPROFILER%SV(:,ji,KI,NSV_DSTBEG:NSV_DSTEND)
    end do
    IF (SIZE(TPROFILER%R,4) >0) THEN
      ZRHO(1,:,:) = 0.
      do ji = 1, iku
        DO JRR=1,SIZE(TPROFILER%R,4)
          ZRHO(1,ji,:) = ZRHO(1,ji,:) + TPROFILER%R(:,ji,KI,JRR)
        ENDDO
        ZRHO(1,ji,:) = TPROFILER%TH(:,ji,KI) * ( 1. + XRV/XRD*TPROFILER%R(:,ji,KI,1) )  &
                                             / ( 1. + ZRHO(1,ji,:)                )
      end do
    ELSE
      do ji = 1, iku
        ZRHO(1,ji,:) = TPROFILER%TH(:,ji,KI)
      end do
    ENDIF
    do ji = 1, iku
      ZRHO(1,ji,:) =  TPROFILER%P(:,ji,KI) / &
                     (XRD *ZRHO(1,ji,:) *((TPROFILER%P(:,ji,KI)/XP00)**(XRD/XCPD)) )
    end do
    CALL PPP2DUST(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,NMODE_DST
      ! mean radius
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A6,I1)')'DSTRGA',JSV
      CUNIT(JPROC) = 'um'
      WRITE(CCOMMENT(JPROC),'(A18,I1)')'RG (nb) DUST MODE ',JSV
      do ji = 1, iku
        XWORK6 (1,1,ji,:,1,JPROC) = ZRG(1,ji,:,JSV)
      end do
      ! standard deviation
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A7,I1)')'DSTSIGA',JSV
      CUNIT(JPROC) = '  '
      WRITE(CCOMMENT(JPROC),'(A16,I1)')'SIGMA DUST MODE ',JSV
      do ji = 1, iku
        XWORK6 (1,1,ji,:,1,JPROC) = ZSIG(1,ji,:,JSV)
      end do
      ! particles number
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A6,I1)')'DSTN0A',JSV
      CUNIT(JPROC) = 'm-3'
      WRITE(CCOMMENT(JPROC),'(A13,I1)')'N0 DUST MODE ',JSV
      do ji = 1, iku
        XWORK6 (1,1,ji,:,1,JPROC) = ZN0(1,ji,:,JSV)
      end do
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF
  ! sea salt scalar variables
  do jsv = nsv_sltbeg, nsv_sltend
    call Add_profile( Trim( csaltnames(jsv - nsv_sltbeg + 1) ), '', 'ppb', tprofiler%sv(:,:,:,jsv) * 1.e9 )
  end do
  if ( ldust .or. lorilam .or. lsalt ) then
    do jsv = 1, naer
      Write( yname, '( a, i1 )' ) 'AEREXT', jsv
      call Add_profile( yname, 'Aerosol Extinction', '1', tprofiler%aer(:,:,:,jsv) )
    end do
  end if
end if

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
tzfields(:)%ndimlist(4) = NMNHDIM_PROFILER_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_PROFILER_PROC

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Profilers'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different vertical profilers'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = ygroup
tzbudiachro%ccomments(NLVL_GROUP)       = 'Data at position of profiler ' // Trim( ygroup )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .true.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Vertical_profile'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Vertical profiles at position of profiler ' // Trim( ygroup )

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

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tprofiler%tpdates, xwork6(:,:,:,:,:,:jproc) )

Deallocate( tzfields )
Deallocate( xwork6 )

!----------------------------------------------------------------------------
!Treat point values

ALLOCATE (XWORK6(1,1,1,size(tprofiler%tpdates),1,IPROC))

jproc = 0

if ( ldiag_in_run ) then
  call Add_point( 'T2m',    '2-m temperature',               'K',       tprofiler%t2m    )
  call Add_point( 'Q2m',    '2-m humidity',                  'kg kg-1', tprofiler%q2m    )
  call Add_point( 'HU2m',   '2-m relative humidity',         'percent', tprofiler%hu2m   )
  call Add_point( 'zon10m', '10-m zonal wind',               'm s-1',   tprofiler%zon10m )
  call Add_point( 'mer10m', '10-m meridian wind',            'm s-1',   tprofiler%mer10m )
  call Add_point( 'RN',     'Net radiation',                 'W m-2',   tprofiler%rn     )
  call Add_point( 'H',      'Sensible heat flux',            'W m-2',   tprofiler%h      )
  call Add_point( 'LE',     'Total Latent heat flux',        'W m-2',   tprofiler%le     )
  call Add_point( 'G',      'Storage heat flux',             'W m-2',   tprofiler%gflux  )
  if ( crad /= 'NONE' ) then
    call Add_point( 'SWD',  'Downward short-wave radiation', 'W m-2',   tprofiler%swd    )
    call Add_point( 'SWU',  'Upward short-wave radiation',   'W m-2',   tprofiler%swu    )
    call Add_point( 'LWD',  'Downward long-wave radiation',  'W m-2',   tprofiler%lwd    )
    call Add_point( 'LWU',  'Upward long-wave radiation',    'W m-2',   tprofiler%lwu    )
  end if
  call Add_point( 'LEI',    'Solid Latent heat flux',        'W m-2',   tprofiler%lei    )
end if

call Add_point( 'IWV', 'Integrated Water Vapour',   'kg m-2', tprofiler%iwv )
call Add_point( 'ZTD', 'Zenith Tropospheric Delay', 'm',      tprofiler%ztd )
call Add_point( 'ZWD', 'Zenith Wet Delay',          'm',      tprofiler%zwd )
call Add_point( 'ZHD', 'Zenith Hydrostatic Delay',  'm',      tprofiler%zhd )

Allocate( tzfields( jproc ) )

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
tzfields(:)%ndimlist(4) = NMNHDIM_PROFILER_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_PROFILER_PROC

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Profilers'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different vertical profilers'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = ygroup
tzbudiachro%ccomments(NLVL_GROUP)       = 'Data at position of profiler ' // Trim( ygroup )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .true.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Point'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Values at position of profiler ' // Trim( ygroup )

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
!Horizontal boundaries in physical domain does not make sense here (but flyer position does)
!These values are not written in the netCDF files
!These values are written in the LFI files. They are kept for backward compatibility (and not set to default values)
tzbudiachro%nil        = 1
tzbudiachro%nih        = 1
tzbudiachro%njl        = 1
tzbudiachro%njh        = 1
tzbudiachro%nkl        = 1
tzbudiachro%nkh        = 1

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tprofiler%tpdates, xwork6(:,:,:,:,:,:jproc) )

Deallocate( tzfields )


!----------------------------------------------------------------------------
!Treat position

jproc = 0

JPROC = JPROC + 1
CTITLE   (JPROC) = 'LON'
CUNIT    (JPROC) = 'degree'
CCOMMENT (JPROC) = 'Longitude'
XWORK6 (1,1,1,:,1,JPROC) = TPROFILER%LON(KI)

JPROC = JPROC + 1
CTITLE   (JPROC) = 'LAT'
CUNIT    (JPROC) = 'degree'
CCOMMENT (JPROC) = 'Latitude'
XWORK6 (1,1,1,:,1,JPROC) = TPROFILER%LAT(KI)

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
tzfields(:)%ndimlist(6) = NMNHDIM_PROFILER_PROC

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tprofiler%tpdates, xwork6(:,:,:,:,:,:jproc) )


!Necessary because global variables (private inside module)
Deallocate( xwork6  )
Deallocate (ccomment)
Deallocate (ctitle  )
Deallocate (cunit   )


contains


subroutine Add_profile( htitle, hcomment, hunits, pfield )

use mode_msg

character(len=*),       intent(in) :: htitle
character(len=*),       intent(in) :: hcomment
character(len=*),       intent(in) :: hunits
real, dimension(:,:,:), intent(in) :: pfield

integer :: jk

jproc = jproc + 1

if ( jproc > iproc ) call Print_msg( NVERB_FATAL, 'IO', 'Add_profile', 'more profiles than expected' )

ctitle(jproc)   = Trim( htitle)
ccomment(jproc) = Trim( hcomment )
cunit(jproc)    = Trim( hunits )

do jk = 1, iku
  xwork6(1, 1, jk, :, 1, jproc) = pfield(:, jk, ki)
end do

end subroutine Add_profile


subroutine Add_point( htitle, hcomment, hunits, pfield )

use mode_msg

character(len=*),     intent(in) :: htitle
character(len=*),     intent(in) :: hcomment
character(len=*),     intent(in) :: hunits
real, dimension(:,:), intent(in) :: pfield

integer :: jk

jproc = jproc + 1

if ( jproc > iproc ) call Print_msg( NVERB_FATAL, 'IO', 'Add_profile', 'more profiles than expected' )

ctitle(jproc)   = Trim( htitle)
ccomment(jproc) = Trim( hcomment )
cunit(jproc)    = Trim( hunits )

xwork6(1, 1, 1, :, 1, jproc) = pfield(:, ki)

end subroutine Add_point

END SUBROUTINE PROFILER_DIACHRO_n

END MODULE MODE_WRITE_PROFILER_n
