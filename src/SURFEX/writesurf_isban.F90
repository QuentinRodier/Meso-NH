!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_ISBA_n(HPROGRAM,OLAND_USE)
!     #####################################
!
!!****  *WRITESURF_ISBA_n* - writes ISBA prognostic fields
!!                        
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
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      P. LeMoigne 12/2004 : correct dimensionning if more than 10 layers in
!!                            the soil (diffusion version)
!!      B. Decharme  2008    : Floodplains
!!      B. Decharme  01/2009 : Optional Arpege deep soil temperature write
!!      A.L. Gibelin   03/09 : modifications for CENTURY model 
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays 
!!      A.L. Gibelin 06/2009 : Soil carbon variables for CNT option
!!      B. Decharme  07/2011 : land_use semi-prognostic variables
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
!!      B. Decharme  09/2012 : write some key for prep_read_external
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
USE MODD_ISBA_n, ONLY :   NGROUND_LAYER, CISBA, CPHOTO, CRESPSL, CSOC, &
                          NNBIOMASS, NNLITTER, NNSOILCARB, NNLITTLEVS, &
                          XTG, XWG, XWGI, XWR, XLAI, TSNOW, XTSRAD_NAT,&
                          XRESA, XAN, XANFM, XLE, XANDAY, TTIME,       &
                          XRESP_BIOMASS, XBIOMASS, XPATCH, XDG,        &
                          XLITTER, XSOILCARB, XLIGNIN_STRUC, LFLOOD,   &
                          XZ0_FLOOD, LTEMP_ARP, NTEMPLAYER_ARP,        &
                          LGLACIER, XICE_STO, LSPINUPCARBS,            &
                          LSPINUPCARBW, NNBYEARSOLD
!
USE MODD_ASSIM, ONLY : LASSIM, CASSIM
!
USE MODD_CH_ISBA_n,    ONLY : NDSTEQ
USE MODD_DST_n
USE MODD_DST_SURF
!
USE MODI_WRITE_SURF
USE MODI_WRITESURF_GR_SNOW
!
USE MODI_WRITE_SURF_FIELD3D
USE MODI_WRITE_SURF_FIELD2D
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
LOGICAL,           INTENT(IN)  :: OLAND_USE !
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=4 ) :: YLVL
 CHARACTER(LEN=5 ) :: YPATCH
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
 CHARACTER(LEN=25) :: YFORM          ! Writing format
!
INTEGER :: JJ, JLAYER, JP, JNBIOMASS, JNLITTER, JNSOILCARB, JNLITTLEVS  ! loop counter on levels
INTEGER :: IWORK   ! Work integer
INTEGER :: JSV
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*       2.     Prognostic fields:
!               -----------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_ISBA_N',0,ZHOOK_HANDLE)
!* soil temperatures
!
IF(LTEMP_ARP)THEN
  IWORK=NTEMPLAYER_ARP
ELSE
  IWORK=NGROUND_LAYER
ENDIF
!
YRECFM='TG'
YCOMMENT='X_Y_TG'
YCOMMENTUNIT='K'
CALL WRITE_SURF_FIELD3D(HPROGRAM,XTG,1,IWORK,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* soil liquid water contents
!
YRECFM='WG'
YCOMMENT='X_Y_WG'
YCOMMENTUNIT='m3/m3'
CALL WRITE_SURF_FIELD3D(HPROGRAM,XWG,1,NGROUND_LAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* soil ice water contents
!
YRECFM='WGI'
YCOMMENT='X_Y_WGI'
YCOMMENTUNIT='m3/m3'
CALL WRITE_SURF_FIELD3D(HPROGRAM,XWGI,1,NGROUND_LAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* water intercepted on leaves
!
YRECFM='WR'
YCOMMENT='X_Y_WR'
YCOMMENTUNIT='kg/m2'
CALL WRITE_SURF_FIELD2D(HPROGRAM,XWR,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* roughness length of Flood water
!
IF(LFLOOD)THEN
  YRECFM='Z0_FLOOD'
  YCOMMENT='X_Y_Z0_FLOOD'
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XZ0_FLOOD,YRECFM,YCOMMENT,YCOMMENTUNIT)
ENDIF
!
!* Glacier ice storage
!
IF(LGLACIER)THEN
  YRECFM='ICE_STO'
  YCOMMENT='X_Y_ICE_STO'
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XICE_STO,YRECFM,YCOMMENT,YCOMMENTUNIT)
ENDIF
!
!* Leaf Area Index
!
IF (CPHOTO/='NON' .AND. CPHOTO/='AGS' .AND. CPHOTO/='AST') THEN
  !
  IF(LASSIM) THEN
    IF(CASSIM=='PLUS ') THEN
      YRECFM='LAIp'
    ELSEIF(CASSIM=='AVERA') THEN
      YRECFM='LAIa'
    ELSEIF(CASSIM=='2DVAR') THEN
      YRECFM='LAI'
    ENDIF
  ELSE
    YRECFM='LAI'
  ENDIF
  !
  YCOMMENT='X_Y_LAI'
  YCOMMENTUNIT='m2/m2'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XLAI,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
END IF
!
!* snow mantel
!
 CALL WRITESURF_GR_SNOW(HPROGRAM,'VEG','     ',TSNOW)
!
!
!* key and/or field usefull to make an external prep
!
YRECFM = 'GLACIER'
YCOMMENT='LGLACIER key for external prep'
 CALL WRITE_SURF(HPROGRAM,YRECFM,LGLACIER,IRESP,HCOMMENT=YCOMMENT)
!
IF(CISBA=='DIF')THEN
!
  YRECFM = 'SOC'
  YCOMMENT='SOC key for external prep'
  CALL WRITE_SURF(HPROGRAM,YRECFM,CSOC,IRESP,HCOMMENT=YCOMMENT)
!
  IF(CSOC=='SGH')THEN
!   Fraction for each patch
    YRECFM='PATCH'
    YCOMMENT='X_Y_PATCH for external prep with SOC'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XPATCH,YRECFM,YCOMMENT,YCOMMENTUNIT)
  ENDIF
!
ELSE
!
  YRECFM = 'TEMPARP'
  YCOMMENT='LTEMP_ARP key for external prep'
  CALL WRITE_SURF(HPROGRAM,YRECFM,LTEMP_ARP,IRESP,HCOMMENT=YCOMMENT)
!
  IF(LTEMP_ARP)THEN
    YRECFM = 'NTEMPLARP'
    YCOMMENT='NTEMPLAYER_ARP for external prep'
    CALL WRITE_SURF(HPROGRAM,YRECFM,NTEMPLAYER_ARP,IRESP,HCOMMENT=YCOMMENT)
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
!
!* patch averaged radiative temperature (K)
!
YRECFM='TSRAD_NAT'
YCOMMENT='X_TSRAD_NAT (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTSRAD_NAT(:),IRESP,HCOMMENT=YCOMMENT)
!
!* aerodynamical resistance
!
YRECFM='RESA'
YCOMMENT='X_Y_RESA (s/m)'
YCOMMENTUNIT='s/m'
CALL WRITE_SURF_FIELD2D(HPROGRAM,XRESA,YRECFM,YCOMMENT,YCOMMENTUNIT)
!#endif
!
!* Land use variables
!
IF(OLAND_USE)THEN
!     
  YRECFM='OLD_PATCH'
  YCOMMENT='X_Y_OLD_PATCH (-)'
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XPATCH,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='OLD_DG'
  YCOMMENT='X_Y_OLD_DG'
  YCOMMENTUNIT='m'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,XDG,1,NGROUND_LAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
ENDIF
!
!* ISBA-AGS variables
!
IF (CPHOTO/='NON') THEN
  YRECFM='AN'
  YCOMMENT='X_Y_AN'
  YCOMMENTUNIT='kgCO2/kgair m/s'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XAN,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='ANDAY'
  YCOMMENT='X_Y_ANDAY'
  YCOMMENTUNIT='kgCO2/m2/day'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XANDAY,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='ANFM'
  YCOMMENT='X_Y_ANFM'
  YCOMMENTUNIT='kgCO2/kgair m/s'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XANFM,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='LE_AGS'
  YCOMMENT='X_Y_LE_AGS'
  YCOMMENTUNIT='W/m2'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XLE,YRECFM,YCOMMENT,YCOMMENTUNIT)
END IF
!
!
IF (CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
  !
YRECFM='BIOMA'
YCOMMENT='X_Y_BIOMASS'
YCOMMENTUNIT='kgDM/m2'
CALL WRITE_SURF_FIELD3D(HPROGRAM,XBIOMASS,1,NNBIOMASS,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
  !
  YRECFM='RESPI'
  YCOMMENT='X_Y_RESP_BIOMASS'
  YCOMMENTUNIT='kg/m2/s'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,XRESP_BIOMASS,2,NNBIOMASS-2,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
  IF (CPHOTO=='NIT') THEN
    !
    YRECFM='RESPI'
    YCOMMENT='X_Y_RESP_BIOMASS'
    YCOMMENTUNIT='kg/m2/s'
    CALL WRITE_SURF_FIELD3D(HPROGRAM,XRESP_BIOMASS,NNBIOMASS-1,NNBIOMASS,YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
  ENDIF
  !
END IF
!
!* Soil carbon
!
YRECFM = 'RESPSL'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,CRESPSL,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NLITTER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NNLITTER,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NLITTLEVS'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NNLITTLEVS,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NSOILCARB'
YCOMMENT=YRECFM
 CALL WRITE_SURF(HPROGRAM,YRECFM,NNSOILCARB,IRESP,HCOMMENT=YCOMMENT)
!
IF(LSPINUPCARBS.OR.LSPINUPCARBW)THEN
  YRECFM='NBYEARSOLD'
  YCOMMENT='yrs'
  CALL WRITE_SURF(HPROGRAM,YRECFM,NNBYEARSOLD,IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
IF (CRESPSL=='CNT') THEN
  !
  DO JNLITTER=1,NNLITTER
    DO JNLITTLEVS=1,NNLITTLEVS
      YFORM='(A10,I1.1,A1,I1.1)'
      WRITE(YCOMMENT,FMT=YFORM) 'X_Y_LITTER',JNLITTER,' ',JNLITTLEVS
      WRITE(YLVL,'(I1,A1,I1)') JNLITTER,'_',JNLITTLEVS
      YRECFM='LITTER'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      YCOMMENTUNIT='gC/m2'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XLITTER(:,JNLITTER,JNLITTLEVS,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    END DO
  END DO
!
  YRECFM='SOILCARB'
  YCOMMENT='X_Y_SOILCARB'
  YCOMMENTUNIT='gC/m2'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,XSOILCARB,1,NNSOILCARB,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='LIGNIN_STR'
  YCOMMENT='X_Y_LIGNIN_STRUC'
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,XLIGNIN_STRUC,1,NNLITTLEVS,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
ENDIF
!
!
IF (NDSTEQ > 0)THEN
  YRECFM='FLX_DSTM'
  YCOMMENT='X_Y_FLX_DSTM'
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD3D(HPROGRAM,XSFDSTM,1,NDSTMDE,YRECFM,YCOMMENT,YCOMMENTUNIT)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5.  Time
!            ----
!
YRECFM='DTCUR'
YCOMMENT='s'
 CALL WRITE_SURF(HPROGRAM,YRECFM,TTIME,IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_ISBA_n
