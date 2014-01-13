!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_OROGRAPHY(HPROGRAM,PSEA,PWATER,HFILE,HFILETYPE,OZS)
!     ##############################################################
!
!!**** *PGD_OROGRAPHY* monitor for averaging and interpolations of cover fractions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!    12/2008 E. Martin : add case 'MAX' for choice of orography
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID,       ONLY : NL, CGRID, XGRID_PAR
USE MODD_PGDWORK,        ONLY : XSUMVAL, XSUMVAL2, NSIZE, XSSQO, LSSQO, NSSO
USE MODD_SURF_ATM_n,     ONLY : XZS
USE MODD_SURF_ATM_SSO_n, ONLY : XSSO_STDEV, XAVG_ZS, XSIL_ZS, XMIN_ZS, XMAX_ZS,&
                                  XSSO_ANIS, XSSO_DIR, XSSO_SLOPE,               &
                                  XAOSIP, XAOSIM, XAOSJP, XAOSJM,                &
                                  XHO2IP, XHO2IM, XHO2JP, XHO2JM  
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_NAM_PGD_OROGRAPHY
USE MODI_READ_SURF
USE MODI_TREAT_FIELD
USE MODI_INTERPOL_FIELD
USE MODI_INI_SSOWORK
USE MODI_SSO
USE MODI_SUBSCALE_AOS
USE MODI_GET_SIZE_FULL_n
!
USE MODI_READ_SSO_n
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
#ifdef ASC
USE MODD_IO_SURF_ASC, ONLY : CFILEIN
#endif
#ifdef FA
USE MODD_IO_SURF_FA,  ONLY : CFILEIN_FA
#endif
#ifdef LFI
USE MODD_IO_SURF_LFI, ONLY : CFILEIN_LFI
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM ! program calling
REAL, DIMENSION(:),   INTENT(IN)  :: PSEA     ! sea  fraction
REAL, DIMENSION(:),   INTENT(IN)  :: PWATER   ! lake fraction
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE    ! atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE! atmospheric file type
LOGICAL,              INTENT(IN)  :: OZS      ! .true. if orography is imposed by atm. model
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ILUOUT    ! output listing logical unit
!
LOGICAL, DIMENSION(NL)   :: GSSO        ! mask where SSO are computed
LOGICAL, DIMENSION(NL)   :: GSSO_ANIS   ! mask where SSO anisotropy is computed
LOGICAL, DIMENSION(NL)   :: GZ0EFFI     ! mask where z0  is  computed in subgrid direction x
LOGICAL, DIMENSION(NL)   :: GZ0EFFJ     ! mask where z0  is  computed in subgrid direction y
INTEGER, DIMENSION(NL)   :: IFLAG       ! flag for SSO and z0 fields interpolations
INTEGER                  :: IRESP       ! error code
REAL                     :: ZEPS = 1.E-4! a small number
INTEGER                  :: IDIM_FULL   ! total size of orographic array in atmospheric file
INTEGER                  :: IZS         ! size of orographic array in atmospheric file
!
!*    0.3    Declaration of namelists
!            ------------------------
!
 CHARACTER(LEN=28)        :: YZS         ! file name for orography
 CHARACTER(LEN=6)         :: YFILETYPE   ! data file type
REAL                     :: XUNIF_ZS    ! uniform orography
 CHARACTER(LEN=3)         :: COROGTYPE   ! orogpraphy type 
!                                       ! 'AVG' : average orography
!                                       ! 'SIL' : silhouette orography
!                                       ! 'ENV' : enveloppe orography
REAL                     :: XENV        ! parameter for enveloppe orography:
!                                       ! zs = avg_zs + XENV * SSO_STEDV
LOGICAL                  :: LIMP_ZS     ! Imposed orography from another PGD file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    2.      Reading of namelist
!             -------------------
!
 CALL READ_NAM_PGD_OROGRAPHY(HPROGRAM, YZS, YFILETYPE, XUNIF_ZS, &
                              COROGTYPE, XENV, LIMP_ZS )  
!
!-------------------------------------------------------------------------------
!
!*    3.      Allocations of orographic arrays
!             --------------------------------
!
ALLOCATE(XZS        (NL))
!
ALLOCATE(XAVG_ZS    (NL))
ALLOCATE(XSIL_ZS    (NL))
ALLOCATE(XSSO_STDEV (NL))
ALLOCATE(XMIN_ZS    (NL))
ALLOCATE(XMAX_ZS    (NL))
!
ALLOCATE(XSSO_ANIS  (NL))
ALLOCATE(XSSO_DIR   (NL))
ALLOCATE(XSSO_SLOPE (NL))
!
ALLOCATE(XAOSIP     (NL))
ALLOCATE(XAOSIM     (NL))
ALLOCATE(XAOSJP     (NL))
ALLOCATE(XAOSJM     (NL))
ALLOCATE(XHO2IP     (NL))
ALLOCATE(XHO2IM     (NL))
ALLOCATE(XHO2JP     (NL))
ALLOCATE(XHO2JM     (NL))
!
XZS       (:) = XUNDEF
XAVG_ZS   (:) = XUNDEF
XSIL_ZS   (:) = XUNDEF
XSSO_STDEV(:) = XUNDEF
XMIN_ZS   (:) = 99999.
XMAX_ZS   (:) =-99999. 
!
XSSO_ANIS (:) = XUNDEF
XSSO_DIR  (:) = XUNDEF
XSSO_SLOPE(:) = XUNDEF
!
XAOSIP    (:) = XUNDEF
XAOSIM    (:) = XUNDEF
XAOSJP    (:) = XUNDEF
XAOSJM    (:) = XUNDEF
XHO2IP    (:) = XUNDEF
XHO2IM    (:) = XUNDEF
XHO2JP    (:) = XUNDEF
XHO2JM    (:) = XUNDEF
!-------------------------------------------------------------------------------
!
!*    4.      Allocations of work arrays
!             --------------------------
!
ALLOCATE(NSIZE     (NL))
ALLOCATE(XSUMVAL   (NL))
ALLOCATE(XSUMVAL2  (NL))
!
NSIZE    (:) = 0.
XSUMVAL  (:) = 0.
XSUMVAL2 (:) = 0.
!
 CALL INI_SSOWORK
!
!-------------------------------------------------------------------------------
!
!*    5.      Uniform field is prescribed
!             ---------------------------
!
IF (OZS) THEN
!
!*    5.1     Use of imposed field
!             --------------------
!
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
  CALL READ_SURF(HFILETYPE,'DIM_FULL  ',IDIM_FULL,IRESP)
  CALL GET_SIZE_FULL_n(HPROGRAM,IDIM_FULL,IZS)
  IF (IZS /= NL) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) '* Error in orography preparation                          *'
    WRITE(ILUOUT,*) '* Prescribed orography from atmospheric model does not    *'
    WRITE(ILUOUT,*) '* have the correct number of points                       *'
    WRITE(ILUOUT,*) '* number of points in atmospheric orography: ', IZS
    WRITE(ILUOUT,*) '* number of points in the surface          : ', NL
    WRITE(ILUOUT,*) '***********************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_OROGRAPHY: ATMOSPHERIC PRESCRIBED OROGRAPHY DOES NOT HAVE THE CORRECT NB OF POINTS')
  END IF
  CALL READ_SURF(HFILETYPE,'ZS',XZS(:),IRESP)
  CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  !
  XAVG_ZS(:)    = XZS(:)
  XSIL_ZS(:)    = XZS(:)
  XMIN_ZS(:)    = XZS(:)
  XMAX_ZS(:)    = XZS(:)
  XSSO_STDEV(:) = 0.
  XHO2IP(:)     = 0.
  XHO2IM(:)     = 0.
  XHO2JP(:)     = 0.
  XHO2JM(:)     = 0.
  XAOSIP(:)     = 0.
  XAOSIM(:)     = 0.
  XAOSJP(:)     = 0.
  XAOSJM(:)     = 0.
  XSSO_ANIS(:)  = 0.
  XSSO_DIR(:)   = 0.
  XSSO_SLOPE(:) = 0.

  DEALLOCATE(NSIZE    )
  DEALLOCATE(XSUMVAL  )
  DEALLOCATE(XSUMVAL2 )

  IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',1,ZHOOK_HANDLE)
  RETURN

!
ELSE IF (XUNIF_ZS/=XUNDEF) THEN
!
!*    5.2     Use of the presribed cover fractions
!             ------------------------------------
!
  XZS(:)        = XUNIF_ZS
  XAVG_ZS(:)    = XZS(:)
  XSIL_ZS(:)    = XZS(:)
  XMIN_ZS(:)    = XZS(:)
  XMAX_ZS(:)    = XZS(:)
  XSSO_STDEV(:) = 0.
  XHO2IP(:)     = 0.
  XHO2IM(:)     = 0.
  XHO2JP(:)     = 0.
  XHO2JM(:)     = 0.
  XAOSIP(:)     = 0.
  XAOSIM(:)     = 0.
  XAOSJP(:)     = 0.
  XAOSJM(:)     = 0.
  XSSO_ANIS(:)  = 0.
  XSSO_DIR(:)   = 0.
  XSSO_SLOPE(:) = 0.

  DEALLOCATE(NSIZE    )
  DEALLOCATE(XSUMVAL  )
  DEALLOCATE(XSUMVAL2 )

  IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',1,ZHOOK_HANDLE)
  RETURN
!
!*    5.3     No data
!             -------
!
ELSEIF (LEN_TRIM(YZS)==0) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in orography preparation                          *'
  WRITE(ILUOUT,*) '* There is no prescribed orography and no input file      *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_OROGRAPHY: NO PRESCRIBED OROGRAPHY NOR INPUT FILE')
!  
ELSEIF(LIMP_ZS)THEN !LIMP_ZS (impose topo from input file at the same resolution)
!
  IF(YFILETYPE=='NETCDF')THEN
     CALL ABOR1_SFX('Use another format than netcdf for topo input file with LIMP_ZS')
  ELSE
#ifdef ASC
     CFILEIN     = ADJUSTL(ADJUSTR(YZS)//'.txt')
#endif
#ifdef FA
     CFILEIN_FA  = ADJUSTL(ADJUSTR(YZS)//'.fa')
#endif
#ifdef LFI
     CFILEIN_LFI = ADJUSTL(YZS)
#endif
     CALL INIT_IO_SURF_n(YFILETYPE,'FULL  ','SURF  ','READ ')
  ENDIF     
!   
  CALL READ_SURF(YFILETYPE,'ZS',XZS(:),IRESP) 
  CALL READ_SSO_n(YFILETYPE)
!
  CALL END_IO_SURF_n(YFILETYPE)
!
  DEALLOCATE(NSIZE    )
  DEALLOCATE(XSUMVAL  )
  DEALLOCATE(XSUMVAL2 )
!
  IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',1,ZHOOK_HANDLE)
  RETURN
!
END IF
!
!-------------------------------------------------------------------------------

!
!*    6.      Averages the field
!             ------------------
!
 CALL TREAT_FIELD(HPROGRAM,'SURF  ',YFILETYPE,'A_OROG',YZS,  &
                   'ZS                  '                     )  
!
DEALLOCATE(XSUMVAL  )
DEALLOCATE(XSUMVAL2 )
!
!-------------------------------------------------------------------------------
!
!*    7.      Coherence with land sea mask
!             ----------------------------
!
WHERE (PSEA(:)==1. .AND. NSIZE(:)==0) NSIZE(:) = -1
!
!-------------------------------------------------------------------------------
!
!*    8.      Interpolation if some points are not initialized (no data for these points)
!             ------------------------------------------------
!
! note that if no orography data exists near points that need to be defined,
! these points are probably small isolated islands, and a default value of 1m is assumed.
!
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XAVG_ZS,   'average orography',PDEF=1.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XSIL_ZS,   'silhouette orography',PDEF=1.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XMIN_ZS,   'minimum orography',PDEF=1.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XMAX_ZS,   'maximum orography',PDEF=1.)
!
IFLAG(:) = NSIZE(:)
WHERE (NSIZE(:)==1) IFLAG(:) = 0 ! only 1 data point was not enough for standard deviation
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XSSO_STDEV,'standard deviation of orography',PDEF=0.)
!
!-------------------------------------------------------------------------------
!
!*    9.      Coherence with land sea mask
!             ----------------------------
!
XAVG_ZS   (:) = XAVG_ZS   (:) * (1. - PSEA(:))
XSIL_ZS   (:) = XSIL_ZS   (:) * (1. - PSEA(:))
!
WHERE (PSEA(:)==1.)
  XSSO_STDEV(:) = XUNDEF
END WHERE
!
WHERE (PWATER(:)==1.)
  XSSO_STDEV(:) = 0.
END WHERE
!
WHERE(PSEA(:)>0.)
  XMIN_ZS(:) = 0.
END WHERE
!
WHERE(PSEA(:)==1.)
  XMAX_ZS(:) = 0.
END WHERE
!
!* slightly modifies the orography values when there are by coincidence equal to
!  default value.
!
WHERE (XAVG_ZS==XUNDEF) XAVG_ZS = XAVG_ZS + ZEPS
WHERE (XSIL_ZS==XUNDEF) XSIL_ZS = XSIL_ZS + ZEPS
WHERE (XMIN_ZS==XUNDEF) XMIN_ZS = XMIN_ZS + ZEPS
WHERE (XMAX_ZS==XUNDEF) XMAX_ZS = XMAX_ZS + ZEPS
!
!-------------------------------------------------------------------------------
!
!*   10.      Choice of orography
!             -------------------
!
SELECT CASE (COROGTYPE)
  CASE ('AVG')
    XZS(:) = XAVG_ZS(:)
  CASE ('ENV')
    XZS(:) = XAVG_ZS(:)
    WHERE (PSEA(:)<1.) XZS(:) = XAVG_ZS(:) + XENV * XSSO_STDEV
  CASE ('SIL')
    XZS(:) = XSIL_ZS(:)
  CASE ('MAX')
    XZS(:) = XMAX_ZS(:)
  CASE DEFAULT
    CALL ABOR1_SFX('PGD_OROGRAPHY: OROGRAPHY TYPE NOT SUPPORTED '//COROGTYPE)
END SELECT
!
!-------------------------------------------------------------------------------
!
!*   12.      Subgrid scale orography characteristics
!             ---------------------------------------
!
 CALL SSO(GSSO,GSSO_ANIS,PSEA)
!
IFLAG(:) = NSIZE(:)
WHERE(.NOT. GSSO(:))                 IFLAG(:) = 0
WHERE(PSEA(:)==1. .AND. IFLAG(:)==0) IFLAG(:) = -1
!
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XSSO_DIR,  'subgrid orography direction',PDEF=0.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XSSO_SLOPE,'subgrid orography slope',PDEF=0.)
!
IFLAG(:) = NSIZE(:)
WHERE(.NOT. GSSO_ANIS(:))            IFLAG(:) = 0
WHERE(PSEA(:)==1. .AND. IFLAG(:)==0) IFLAG(:) = -1
!
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XSSO_ANIS, 'subgrid orography anisotropy',PDEF=0.)
!
WHERE (PSEA(:)==1.)
  XSSO_ANIS (:) = XUNDEF
  XSSO_DIR  (:) = XUNDEF
  XSSO_SLOPE(:) = XUNDEF
END WHERE
!
WHERE (PWATER(:)==1.)
  XSSO_ANIS (:) = 1.
  XSSO_DIR  (:) = 0.
  XSSO_SLOPE(:) = 0.
END WHERE
!
!-------------------------------------------------------------------------------
!
!*   13.      Subgrid scale orography roughness
!             ---------------------------------
!
 CALL SUBSCALE_AOS(GZ0EFFI,GZ0EFFJ,PSEA)
!
IFLAG(:) = NSIZE(:)
WHERE(.NOT. GZ0EFFI(:))              IFLAG(:) = 0
WHERE(PSEA(:)==1. .AND. IFLAG(:)==0) IFLAG(:) = -1
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XAOSIP, 'subgrid orography A/S, direction i+',PDEF=0.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XAOSIM, 'subgrid orography A/S, direction i-',PDEF=0.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XHO2IP, 'subgrid orography h/2, direction i+',PDEF=0.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XHO2IM, 'subgrid orography h/2, direction i-',PDEF=0.)
!
IFLAG(:) = NSIZE(:)
WHERE(.NOT. GZ0EFFJ(:))              IFLAG(:) = 0
WHERE(PSEA(:)==1. .AND. IFLAG(:)==0) IFLAG(:) = -1
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XAOSJP, 'subgrid orography A/S, direction j+',PDEF=0.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XAOSJM, 'subgrid orography A/S, direction j-',PDEF=0.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XHO2JP, 'subgrid orography h/2, direction j+',PDEF=0.)
 CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,IFLAG,XHO2JM, 'subgrid orography h/2, direction j-',PDEF=0.)
!
WHERE (PSEA(:)==1.)
  XHO2IP(:) = XUNDEF
  XHO2IM(:) = XUNDEF
  XHO2JP(:) = XUNDEF
  XHO2JM(:) = XUNDEF
  XAOSIP(:) = XUNDEF
  XAOSIM(:) = XUNDEF
  XAOSJP(:) = XUNDEF
  XAOSJM(:) = XUNDEF
END WHERE
!
WHERE (PWATER(:)==1.)
  XHO2IP(:) = 0.
  XHO2IM(:) = 0.
  XHO2JP(:) = 0.
  XHO2JM(:) = 0.
  XAOSIP(:) = 0.
  XAOSIM(:) = 0.
  XAOSJP(:) = 0.
  XAOSJM(:) = 0.
END WHERE
!-------------------------------------------------------------------------------
DEALLOCATE(NSIZE    )
IF (LHOOK) CALL DR_HOOK('PGD_OROGRAPHY',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_OROGRAPHY
