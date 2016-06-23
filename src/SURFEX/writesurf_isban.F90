!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_ISBA_n (DGU, U, &
                                    CHI, DST, I, &
                                   HPROGRAM,OLAND_USE)
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
!!      V. Masson   *Meteo France*
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
!!      B. Decharme  04/2013 : Only 2 temperature layer in ISBA-FR
!!      P. Samuelsson 10/2014: MEB
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
!
USE MODD_ASSIM, ONLY : LASSIM, CASSIM, CASSIM_ISBA, NIE, NENS, &
                       XADDTIMECORR, LENS_GEN, NVAR
!
USE MODD_DST_SURF
!
USE MODI_WRITE_SURF
USE MODI_WRITESURF_GR_SNOW
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
!
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(ISBA_t), INTENT(INOUT) :: I
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
 CHARACTER(LEN=3 ) :: YVAR
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write 
 CHARACTER(LEN=25) :: YFORM          ! Writing format
!
INTEGER :: JJ, JLAYER, JP, JNBIOMASS, JNLITTER, JNSOILCARB, JNLITTLEVS  ! loop counter on levels
INTEGER :: IWORK   ! Work integer
INTEGER :: JSV
INTEGER :: ISIZE_LMEB_PATCH
INTEGER :: JVAR
!
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
IF(I%LTEMP_ARP)THEN
  IWORK=I%NTEMPLAYER_ARP
ELSEIF(I%CISBA=='DIF')THEN
  IWORK=I%NGROUND_LAYER
ELSE
  IWORK=2 !Only 2 temperature layer in ISBA-FR
ENDIF
!
YRECFM='TG'
YCOMMENT='X_Y_TG'
YCOMMENTUNIT='K'
 CALL WRITE_SURF_FIELD3D(DGU, U, &
           HPROGRAM,I%XTG,1,IWORK,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* soil liquid water contents
!
YRECFM='WG'
YCOMMENT='X_Y_WG'
YCOMMENTUNIT='m3/m3'
 CALL WRITE_SURF_FIELD3D(DGU, U, &
                HPROGRAM,I%XWG,1,I%NGROUND_LAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* soil ice water contents
!
IF(I%CISBA=='DIF')THEN
  IWORK=I%NGROUND_LAYER
ELSE
  IWORK=2 !Only 2 soil ice layer in ISBA-FR
ENDIF
!
YRECFM='WGI'
YCOMMENT='X_Y_WGI'
YCOMMENTUNIT='m3/m3'
 CALL WRITE_SURF_FIELD3D(DGU, U, &
           HPROGRAM,I%XWGI,1,IWORK,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!
!* water intercepted on leaves
!
YRECFM='WR'
YCOMMENT='X_Y_WR (kg/m2)'
YCOMMENTUNIT='kg/m2'
 CALL WRITE_SURF_FIELD2D(DGU, U, &
        HPROGRAM,I%XWR,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* Glacier ice storage
!
YRECFM = 'GLACIER'
YCOMMENT='LGLACIER key for external prep'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%LGLACIER,IRESP,HCOMMENT=YCOMMENT)
!
IF(I%LGLACIER)THEN
  YRECFM='ICE_STO'
  YCOMMENT='X_Y_ICE_STO (kg/m2)'
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
       HPROGRAM,I%XICE_STO,YRECFM,YCOMMENT,YCOMMENTUNIT)
ENDIF
!
!* Leaf Area Index
!
IF (I%CPHOTO/='NON' .AND. I%CPHOTO/='AGS' .AND. I%CPHOTO/='AST') THEN
  !
  YRECFM='LAI'
  !
  YCOMMENT='X_Y_LAI'
  YCOMMENTUNIT='m2/m2'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
        HPROGRAM,I%XLAI,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
END IF
!
IF ( TRIM(CASSIM_ISBA)=="ENKF" .AND. (LASSIM .OR. NIE/=0) ) THEN
  DO JVAR = 1,NVAR
    IF ( XADDTIMECORR(JVAR)>0. ) THEN
      WRITE(YVAR,'(I3)') JVAR
      YCOMMENT = 'Red_Noise_Enkf'
      YRECFM='RED_NOISE'//ADJUSTL(YVAR(:LEN_TRIM(YVAR)))
      CALL WRITE_SURF(DGU, U, &
                      HPROGRAM,YRECFM,I%XRED_NOISE(:,:,JVAR),IRESP,HCOMMENT=YCOMMENT)
    ENDIF
  ENDDO
ENDIF
!
!* snow mantel
!
 CALL WRITESURF_GR_SNOW(DGU, U, &
                        HPROGRAM,'VEG','     ',I%TSNOW)
!
!
!* key and/or field usefull to make an external prep
!
IF(I%CISBA=='DIF')THEN
!
  YRECFM = 'SOC'
  YCOMMENT='SOC key for external prep'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%LSOC,IRESP,HCOMMENT=YCOMMENT)
!
ELSE
!
  YRECFM = 'TEMPARP'
  YCOMMENT='LTEMP_ARP key for external prep'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%LTEMP_ARP,IRESP,HCOMMENT=YCOMMENT)
!
  IF(I%LTEMP_ARP)THEN
    YRECFM = 'NTEMPLARP'
    YCOMMENT='NTEMPLAYER_ARP for external prep'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NTEMPLAYER_ARP,IRESP,HCOMMENT=YCOMMENT)
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.  MEB Prognostic or Semi-prognostic variables
!            -------------------------------------------
!
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
!
IF (ISIZE_LMEB_PATCH>0) THEN
!
!* water intercepted on canopy vegetation leaves
!
  YRECFM='WRL'
  YCOMMENT='X_Y_WRL'
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
                 HPROGRAM,I%XWRL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* ice on litter
!
  YRECFM='WRLI'
  YCOMMENT='X_Y_WRLI'
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
                 HPROGRAM,I%XWRLI(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* snow intercepted on canopy vegetation leaves
!
  YRECFM='WRVN'
  YCOMMENT='X_Y_WRVN'
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
                 HPROGRAM,I%XWRVN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* canopy vegetation temperature
!
  YRECFM='TV'
  YCOMMENT='X_Y_TV'
  YCOMMENTUNIT='K'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
                 HPROGRAM,I%XTV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* litter temperature
!
  YRECFM='TL'
  YCOMMENT='X_Y_TL'
  YCOMMENTUNIT='K'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
                 HPROGRAM,I%XTL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* vegetation canopy air temperature
!
  YRECFM='TC'
  YCOMMENT='X_Y_TC'
  YCOMMENTUNIT='K'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
                 HPROGRAM,I%XTC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* vegetation canopy air specific humidity
!
  YRECFM='QC'
  YCOMMENT='X_Y_QC'
  YCOMMENTUNIT='kg/kg'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
                 HPROGRAM,I%XQC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
!
!* Fraction for each patch
!
YRECFM='PATCH'
YCOMMENT='fraction for each patch'
YCOMMENTUNIT='-'
 CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,I%XPATCH(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* patch averaged radiative temperature (K)
!
YRECFM='TSRAD_NAT'
YCOMMENT='X_TSRAD_NAT (K)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XTSRAD_NAT(:),IRESP,HCOMMENT=YCOMMENT)
!
!* aerodynamical resistance
!
YRECFM='RESA'
YCOMMENT='X_Y_RESA'
YCOMMENTUNIT='s/m'
 CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,I%XRESA(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!* Land use variables
!
IF(OLAND_USE)THEN
!
  YRECFM='OLD_DG'
  YCOMMENT='X_Y_OLD_DG'
  YCOMMENTUNIT='m'
  CALL WRITE_SURF_FIELD3D(DGU, U, &
         HPROGRAM,I%XDG,1,I%NGROUND_LAYER,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
ENDIF
!
!* ISBA-AGS variables
!
IF (I%CPHOTO/='NON') THEN
 YRECFM='AN'
  YCOMMENT='X_Y_AN'
  YCOMMENTUNIT='kgCO2/kgair m/s'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
           HPROGRAM,I%XAN,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='ANDAY'
  YCOMMENT='X_Y_ANDAY'
  YCOMMENTUNIT='kgCO2/m2/day'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
           HPROGRAM,I%XANDAY,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='ANFM'
  YCOMMENT='X_Y_ANFM'
  YCOMMENTUNIT='kgCO2/kgair m/s'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
           HPROGRAM,I%XANFM,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
  YRECFM='LE_AGS'
  YCOMMENT='X_Y_LE_AGS'
  YCOMMENTUNIT='W/m2'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
           HPROGRAM,I%XLE,YRECFM,YCOMMENT,YCOMMENTUNIT)
END IF
!
!
IF (I%CPHOTO=='NIT' .OR. I%CPHOTO=='NCB') THEN
  !
  YRECFM='BIOMA'
  YCOMMENT='X_Y_BIOMASS'
  YCOMMENTUNIT='kgDM/m2'
  CALL WRITE_SURF_FIELD3D(DGU, U, &
        HPROGRAM,I%XBIOMASS,1,I%NNBIOMASS,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
  YRECFM='RESPI'
  YCOMMENT='X_Y_RESP_BIOMASS'
  YCOMMENTUNIT='kg/m2/s'
  CALL WRITE_SURF_FIELD3D(DGU, U, &
        HPROGRAM,I%XRESP_BIOMASS,2,I%NNBIOMASS,YRECFM,YCOMMENT,YCOMMENTUNIT)
  !
END IF
!
!* Soil carbon
!
YRECFM = 'RESPSL'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%CRESPSL,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NLITTER'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NNLITTER,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NLITTLEVS'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NNLITTLEVS,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='NSOILCARB'
YCOMMENT=YRECFM
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NNSOILCARB,IRESP,HCOMMENT=YCOMMENT)
!
IF(I%LSPINUPCARBS.OR.I%LSPINUPCARBW)THEN
  YRECFM='NBYEARSOLD'
  YCOMMENT='yrs'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%NNBYEARSOLD,IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
IF (I%CRESPSL=='CNT') THEN
  !
  DO JNLITTER=1,I%NNLITTER
    DO JNLITTLEVS=1,I%NNLITTLEVS
      YFORM='(A10,I1.1,A1,I1.1)'
      WRITE(YCOMMENT,FMT=YFORM) 'X_Y_LITTER',JNLITTER,' ',JNLITTLEVS
      WRITE(YLVL,'(I1,A1,I1)') JNLITTER,'_',JNLITTLEVS
      YRECFM='LITTER'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      YCOMMENTUNIT='gC/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
              HPROGRAM,I%XLITTER(:,JNLITTER,JNLITTLEVS,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    END DO
  END DO
  !
  YRECFM='SOILCARB'
  YCOMMENT='X_Y_SOILCARB'
  YCOMMENTUNIT='gC/m2'
  CALL WRITE_SURF_FIELD3D(DGU, U, &
               HPROGRAM,I%XSOILCARB,1,I%NNSOILCARB,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
!
  YRECFM='LIGNIN_STR'
  YCOMMENT='X_Y_LIGNIN_STRUC'
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD3D(DGU, U, &
          HPROGRAM,I%XLIGNIN_STRUC,1,I%NNLITTLEVS,YRECFM,YCOMMENT,YCOMMENTUNIT)
!
ENDIF
!
!
IF (CHI%SVI%NDSTEQ > 0)THEN
  YRECFM='FLX_DSTM'
  YCOMMENT='X_Y_FLX_DSTM'
  YCOMMENTUNIT='kg/m2'
  CALL WRITE_SURF_FIELD3D(DGU, U, &
          HPROGRAM,DST%XSFDSTM,1,NDSTMDE,YRECFM,YCOMMENT,YCOMMENTUNIT)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5.  Time
!            ----
!
YRECFM='DTCUR'
YCOMMENT='s'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%TTIME,IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_ISBA_n
