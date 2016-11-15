!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_ISBA_n (DTCO, I, U, &
                              HPROGRAM)
!     ##################################
!
!!****  *READ_ISBA_n* - routine to initialise ISBA variables
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
!!
!!      READ_SURF for general reading : 08/2003 (S.Malardel)
!!      B. Decharme  2008    : Floodplains
!!      B. Decharme  01/2009 : Optional Arpege deep soil temperature read
!!      A.L. Gibelin   03/09 : modifications for CENTURY model 
!!      A.L. Gibelin    04/2009 : BIOMASS and RESP_BIOMASS arrays 
!!      A.L. Gibelin    06/2009 : Soil carbon variables for CNT option
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
!!      T. Aspelien  08/2013 : Read diagnostics for assimilation
!!      P. Samuelsson   10/2014 : MEB
!!     M.Moge    01/2016  using READ_SURF_FIELD2D/3D for 2D/3D surfex fields reads
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_CO2V_PAR,       ONLY : XANFMINIT, XCONDCTMIN
!                          
USE MODD_ASSIM,          ONLY : LASSIM,CASSIM_ISBA,XAT2M_ISBA,XAHU2M_ISBA,&
                                XAZON10M_ISBA,XAMER10M_ISBA,NIFIC,NVAR, &
                                COBS,NOBSTYPE,CVAR,LPRT,XTPRT,NIVAR,CBIO, &
                                XADDINFL,NENS,XSIGMA,NIE
!                                
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SNOW_PAR,       ONLY : XZ0SN
!
USE MODI_READ_SURF
USE MODI_READ_SURF_FIELD3D
USE MODI_READ_SURF_FIELD2D
!
USE MODI_READ_GR_SNOW
USE MODI_ABOR1_SFX
USE MODI_IO_BUFF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
USE MODE_RANDOM
USE MODE_EKF
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
INTEGER           :: ILU          ! 1D physical dimension
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
!
 CHARACTER(LEN=4)  :: YLVL
!
REAL, DIMENSION(:,:,:),ALLOCATABLE :: ZLAI
REAL, DIMENSION(:,:),ALLOCATABLE  :: ZWORK      ! 2D array to write data in file
REAL, DIMENSION(:), ALLOCATABLE :: ZCOFSWI
!
REAL,DIMENSION(I%NPATCH) :: ZVLAIMIN
REAL :: ZCOEF
!
INTEGER :: IWORK   ! Work integer
!
INTEGER :: JP, JL, JNBIOMASS, JNLITTER, JNSOILCARB, JNLITTLEVS  ! loop counter on layers
INTEGER :: JVAR, JI
!
INTEGER           :: IVERSION       ! surface version
INTEGER           :: IBUGFIX
INTEGER           :: IIVAR
INTEGER           :: IOBS
INTEGER           :: IBSUP
INTEGER           :: ISIZE_LMEB_PATCH
!
LOGICAL :: GKNOWN
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_ISBA_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_NATURE'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'NATURE',ILU)
!
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU,I%NPATCH))
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
IF ( TRIM(CASSIM_ISBA)=="ENKF") THEN
  ALLOCATE(I%XRED_NOISE(ILU,I%NPATCH,NVAR))
  I%XRED_NOISE(:,:,:) = 0.
  ALLOCATE(ZCOFSWI(ILU))
  CALL COFSWI(I%XCLAY(:,1),ZCOFSWI)
ELSE
  ALLOCATE(I%XRED_NOISE(0,0,0))
  ALLOCATE(ZCOFSWI(0))
ENDIF
!
ALLOCATE(I%XTG(ILU,IWORK,I%NPATCH))
I%XTG(:,:,:)=XUNDEF
!
YRECFM='TG'
 CALL READ_SURF_FIELD3D(HPROGRAM,I%XTG,1,IWORK,YRECFM)
!
! Perturb value if requested
IF ( TRIM(CASSIM_ISBA)=="EKF" .AND. LPRT ) THEN
  !
  DO JL=1,IWORK
  ! read in control variable
    IF ( (TRIM(CVAR(NIVAR))=="TG1" .AND. JL==1) .OR. &
         (TRIM(CVAR(NIVAR))=="TG2" .AND. JL==2) ) THEN
      WHERE ( I%XTG(:,JL,:)/=XUNDEF )
        I%XTG(:,JL,:) = I%XTG(:,JL,:) + XTPRT(NIVAR)*I%XTG(:,JL,:)
      ENDWHERE
    ENDIF
  END DO
  !
ELSEIF ( TRIM(CASSIM_ISBA)=="ENKF" .AND. NIE<NENS+1 ) THEN
  !
  CALL MAKE_ENS_ENKF(IWORK,ILU,"TG ",ZCOFSWI,I%XTG,I%XRED_NOISE)
  !
ENDIF
!
!
!* soil liquid and ice water contents
!
ALLOCATE(I%XWG (ILU,I%NGROUND_LAYER,I%NPATCH))
ALLOCATE(I%XWGI(ILU,I%NGROUND_LAYER,I%NPATCH))
!
I%XWG (:,:,:)=XUNDEF
I%XWGI(:,:,:)=XUNDEF
!
YRECFM='WG'
 CALL READ_SURF_FIELD3D(HPROGRAM,I%XWG,1,I%NGROUND_LAYER,YRECFM)
!
! Perturb value if requested
IF ( TRIM(CASSIM_ISBA)=="EKF" .AND. LPRT ) THEN
   !
   DO JL=1,I%NGROUND_LAYER
    ! read in control variable
    IF ( (TRIM(CVAR(NIVAR))=="WG1" .AND. JL==1) .OR. & 
         (TRIM(CVAR(NIVAR))=="WG2" .AND. JL==2) ) THEN
      WHERE ( I%XWG(:,JL,:)/=XUNDEF )
        I%XWG(:,JL,:) = I%XWG(:,JL,:) + XTPRT(NIVAR)*I%XWG(:,JL,:)
      ENDWHERE
    ENDIF
   END DO
   !
ELSEIF ( TRIM(CASSIM_ISBA)=="ENKF" .AND. NIE<NENS+1 ) THEN
  !
  CALL MAKE_ENS_ENKF(IWORK,ILU,"WG ",ZCOFSWI,I%XWG,I%XRED_NOISE)
  !
ENDIF
!
IF(I%CISBA=='DIF')THEN
  IWORK=I%NGROUND_LAYER
ELSE
  IWORK=2 !Only 2 soil ice layer in ISBA-FR
ENDIF
!
YRECFM='WGI'
 CALL READ_SURF_FIELD3D(HPROGRAM,I%XWGI,1,IWORK,YRECFM)
!
!* water intercepted on leaves
!
ALLOCATE(I%XWR(ILU,I%NPATCH))
!
YRECFM = 'WR'
 CALL READ_SURF_FIELD2D(HPROGRAM,I%XWR,YRECFM)
!
!* Leaf Area Index
!
IF (I%CPHOTO=='LAI' .OR. I%CPHOTO=='LST' .OR. I%CPHOTO=='NIT' .OR. I%CPHOTO=='NCB') THEN
  YRECFM = 'LAI'
  CALL READ_SURF_FIELD2D(HPROGRAM,I%XLAI,YRECFM)
  IF ( TRIM(CASSIM_ISBA)=="EKF" .AND. LPRT ) THEN
    !
    ! read in control variable
    IF ( TRIM(CVAR(NIVAR))=="LAI" ) THEN
      WHERE ( I%XLAI(:,:)/=XUNDEF ) 
        I%XLAI(:,:) = I%XLAI(:,:) + XTPRT(NIVAR)*I%XLAI(:,:)
      ENDWHERE
    ENDIF
    !
  ELSEIF ( TRIM(CASSIM_ISBA)=="ENKF" .AND. NIE<NENS+1 ) THEN
    !
    IF (I%NPATCH==12) THEN
      ZVLAIMIN = (/0.3,0.3,0.3,0.3,1.0,1.0,0.3,0.3,0.3,0.3,0.3,0.3/)
    ELSE
      ZVLAIMIN = (/0.3/)
    ENDIF
    !
    ALLOCATE(ZLAI(ILU,1,I%NPATCH))
    ZLAI(:,1,:) = I%XLAI(:,:)
    CALL MAKE_ENS_ENKF(1,ILU,"LAI",ZCOFSWI,ZLAI,I%XRED_NOISE)
    DO JP = 1,I%NPATCH
      I%XLAI(:,JP) = MAX(ZVLAIMIN(JP),ZLAI(:,1,JP))
    ENDDO
    DEALLOCATE(ZLAI)
    !    
  ENDIF  
END IF
!
!* snow mantel
!
 CALL READ_GR_SNOW(&
                   HPROGRAM,'VEG','     ',ILU,I%NPATCH,I%TSNOW  )
!
YRECFM='VERSION'
 CALL READ_SURF(&
                 HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(&
                 HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
IF(I%LGLACIER)THEN
  ALLOCATE(I%XICE_STO(ILU,I%NPATCH))
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM = 'ICE_STO'
    CALL READ_SURF_FIELD2D(HPROGRAM,I%XICE_STO,YRECFM)
  ELSE
    I%XICE_STO(:,:) = 0.0
  ENDIF
ELSE
  ALLOCATE(I%XICE_STO(0,0))
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.  MEB Prognostic or Semi-prognostic variables
!            -------------------------------------------
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
!
IF (ISIZE_LMEB_PATCH>0) THEN
!
!* water intercepted on litter

 ALLOCATE(I%XWRL(ILU,I%NPATCH))
 YRECFM = 'WRL'
 CALL READ_SURF(HPROGRAM,YRECFM,I%XWRL(:,:),IRESP)

 ALLOCATE(I%XWRLI(ILU,I%NPATCH))
 YRECFM = 'WRLI'
 CALL READ_SURF(HPROGRAM,YRECFM,I%XWRLI(:,:),IRESP)
!
!* snow intercepted on vegetation canopy leaves
!
  ALLOCATE(I%XWRVN(ILU,I%NPATCH))
  YRECFM = 'WRVN'
  CALL READ_SURF(&
                 HPROGRAM,YRECFM,I%XWRVN(:,:),IRESP)
!
!* vegetation canopy temperature
!
  ALLOCATE(I%XTV(ILU,I%NPATCH))
  YRECFM = 'TV'
  CALL READ_SURF(HPROGRAM,YRECFM,I%XTV(:,:),IRESP)
!
!* litter temperature
!
  ALLOCATE(I%XTL(ILU,I%NPATCH))
  YRECFM = 'TL'
  CALL READ_SURF(HPROGRAM,YRECFM,I%XTL(:,:),IRESP)
!
!* vegetation canopy air temperature
!
  ALLOCATE(I%XTC(ILU,I%NPATCH))
  YRECFM = 'TC'
  CALL READ_SURF(&
                 HPROGRAM,YRECFM,I%XTC(:,:),IRESP)
!
!* vegetation canopy air specific humidity
!
  ALLOCATE(I%XQC(ILU,I%NPATCH))
  YRECFM = 'QC'
  CALL READ_SURF(&
                 HPROGRAM,YRECFM,I%XQC(:,:),IRESP)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
ALLOCATE(I%XRESA(ILU,I%NPATCH))
ALLOCATE(I%XLE  (ILU,I%NPATCH))
IF (I%CPHOTO/='NON') THEN
  ALLOCATE(I%XANFM  (ILU,I%NPATCH))
  ALLOCATE(I%XAN    (ILU,I%NPATCH))
  ALLOCATE(I%XANDAY (ILU,I%NPATCH))
END IF
!
IF(I%CPHOTO/='NON') THEN
  ALLOCATE(I%XBIOMASS         (ILU,I%NNBIOMASS,I%NPATCH))
  ALLOCATE(I%XRESP_BIOMASS    (ILU,I%NNBIOMASS,I%NPATCH))
END IF
!
!
!* aerodynamical resistance
!
YRECFM = 'RESA'
I%XRESA(:,:) = 100.
 CALL READ_SURF_FIELD2D(HPROGRAM,I%XRESA,YRECFM)
!
!* patch averaged radiative temperature (K)
!
ALLOCATE(I%XTSRAD_NAT(ILU))
IF (IVERSION<6) THEN
  I%XTSRAD_NAT(:)=0.
  DO JP=1,I%NPATCH
    I%XTSRAD_NAT(:)=I%XTSRAD_NAT(:)+I%XTG(:,1,JP)
  ENDDO
  I%XTSRAD_NAT(:)=I%XTSRAD_NAT(:)/I%NPATCH
ELSE
  YRECFM='TSRAD_NAT'
  CALL READ_SURF(&
                 HPROGRAM,YRECFM,I%XTSRAD_NAT(:),IRESP)
ENDIF
!
I%XLE(:,:) = XUNDEF
!
!*       5. ISBA-AGS variables
!
IF (I%CPHOTO/='NON') THEN
  YRECFM = 'AN'
  I%XAN(:,:) = 0.
  CALL READ_SURF_FIELD2D(HPROGRAM,I%XAN,YRECFM)
  !
  YRECFM = 'ANDAY'
  I%XANDAY(:,:) = 0.
  CALL READ_SURF_FIELD2D(HPROGRAM,I%XANDAY,YRECFM)
  !
  YRECFM = 'ANFM'
  I%XANFM(:,:) = XANFMINIT
  CALL READ_SURF_FIELD2D(HPROGRAM,I%XANFM,YRECFM)
  !
  YRECFM = 'LE_AGS'
  I%XLE(:,:) = 0.
  CALL READ_SURF_FIELD2D(HPROGRAM,I%XLE,YRECFM)
END IF
!
IF (I%CPHOTO=='AGS' .OR. I%CPHOTO=='AST') THEN
  !
  I%XBIOMASS(:,:,:) = 0.
  I%XRESP_BIOMASS(:,:,:) = 0.

ELSEIF (I%CPHOTO=='LAI' .OR. I%CPHOTO=='LST') THEN
  !
  I%XBIOMASS(:,1,:) = I%XBSLAI(:,:) * I%XLAI(:,:)
  I%XRESP_BIOMASS(:,:,:) = 0.

ELSEIF (I%CPHOTO=='NIT'.OR.I%CPHOTO=='NCB') THEN
  !
  I%XBIOMASS(:,:,:) = 0.
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM='BIOMA'
  ELSE
    YRECFM='BIOMASS'
  ENDIF
  CALL READ_SURF_FIELD3D(HPROGRAM,I%XBIOMASS,1,I%NNBIOMASS,YRECFM)

  DO JNBIOMASS=1,I%NNBIOMASS
       IF ( TRIM(CASSIM_ISBA)=="EKF" .AND. LPRT ) THEN
      ! read in control variable
      IF ( TRIM(CVAR(NIVAR)) == "LAI" .AND. TRIM(CBIO)==TRIM(YRECFM) ) THEN
        WHERE ( I%XBIOMASS(:,JNBIOMASS,:)/=XUNDEF ) 
          I%XBIOMASS(:,JNBIOMASS,:) = I%XBIOMASS(:,JNBIOMASS,:) + XTPRT(NIVAR)*I%XBIOMASS(:,JNBIOMASS,:)
        ENDWHERE
      ENDIF
    ELSEIF ( TRIM(CASSIM_ISBA)=="ENKF" .AND. NIE<NENS+1 .AND. .NOT.LASSIM ) THEN
      !
      IF ( TRIM(CBIO)==TRIM(YRECFM) ) THEN
        DO JVAR = 1,NVAR
          IF (TRIM(CVAR(JVAR)) == "LAI") THEN
            DO JI = 1,ILU
              DO JP = 1,I%NPATCH
                I%XBIOMASS(JI,JNBIOMASS,JP) = I%XBIOMASS(JI,JNBIOMASS,JP) + XADDINFL(JVAR)*RANDOM_NORMAL()
              ENDDO
            ENDDO
            EXIT
          ENDIF
        ENDDO
      ENDIF
      !      
    ENDIF     
  END DO
!
  IWORK=0
  IF(I%CPHOTO=='NCB'.OR.IVERSION<8)IWORK=2
!
  I%XRESP_BIOMASS(:,:,:) = 0.
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM='RESPI'
  ELSE
    YRECFM='RESP_BIOM'
  ENDIF
  CALL READ_SURF_FIELD3D(HPROGRAM,I%XRESP_BIOMASS,2,I%NNBIOMASS,YRECFM)
  DO JNBIOMASS=2,I%NNBIOMASS-IWORK
    IF ( TRIM(CASSIM_ISBA)=="EKF" .AND. LPRT ) THEN
      ! read in control variable
      IF ( TRIM(CVAR(NIVAR)) == "LAI" .AND. TRIM(CBIO)==TRIM(YRECFM) ) THEN
        WHERE ( I%XRESP_BIOMASS(:,JNBIOMASS,:)/=XUNDEF ) 
          I%XRESP_BIOMASS(:,JNBIOMASS,:) = I%XRESP_BIOMASS(:,JNBIOMASS,:) + &
                                           XTPRT(NIVAR)*I%XRESP_BIOMASS(:,JNBIOMASS,:)
        ENDWHERE
    ELSEIF ( TRIM(CASSIM_ISBA)=="ENKF" .AND. NIE<NENS+1 .AND. .NOT.LASSIM ) THEN
      !
      IF ( TRIM(CBIO)==TRIM(YRECFM) ) THEN
        DO JVAR = 1,NVAR
          IF (TRIM(CVAR(JVAR)) == "LAI") THEN
            DO JI = 1,ILU
              DO JP = 1,I%NPATCH
                I%XRESP_BIOMASS(JI,JNBIOMASS,JP) = I%XRESP_BIOMASS(JI,JNBIOMASS,JP) + &
                                                   XADDINFL(JVAR)*RANDOM_NORMAL()
              ENDDO
            ENDDO
            EXIT
          ENDIF
        ENDDO
      ENDIF
      !  
      ENDIF
    ENDIF      
  END DO
  !
ENDIF
!
DEALLOCATE(ZCOFSWI)
!
!*       6. Soil carbon
!
!
IF (I%CRESPSL=='CNT') THEN
  !
  ALLOCATE(I%XLITTER          (ILU,I%NNLITTER,I%NNLITTLEVS,I%NPATCH))
  ALLOCATE(I%XSOILCARB        (ILU,I%NNSOILCARB,I%NPATCH))
  ALLOCATE(I%XLIGNIN_STRUC    (ILU,I%NNLITTLEVS,I%NPATCH))
  !
  I%XLITTER(:,:,:,:) = 0.
  DO JNLITTER=1,I%NNLITTER
    DO JNLITTLEVS=1,I%NNLITTLEVS
      WRITE(YLVL,'(I1,A1,I1)') JNLITTER,'_',JNLITTLEVS
      YRECFM='LITTER'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      CALL READ_SURF_FIELD2D(HPROGRAM,ZWORK(:,:),YRECFM)
      I%XLITTER(:,JNLITTER,JNLITTLEVS,:)=ZWORK
    END DO
  END DO

  I%XSOILCARB(:,:,:) = 0.
  YRECFM='SOILCARB'
  CALL READ_SURF_FIELD3D(HPROGRAM,I%XSOILCARB,1,I%NNSOILCARB,YRECFM)
!
  I%XLIGNIN_STRUC(:,:,:) = 0.
  YRECFM='LIGNIN_STR'
  CALL READ_SURF_FIELD3D(HPROGRAM,I%XLIGNIN_STRUC,1,I%NNLITTLEVS,YRECFM)
!
ENDIF

IF ( LASSIM ) THEN
  IF ( TRIM(CASSIM_ISBA) == "OI" ) THEN
    IF ( I%NPATCH /= 1 ) CALL ABOR1_SFX ('Reading of diagnostical values for'&
                       & //'assimilation at the moment only works for one patch for OI')          
    ! Diagnostic fields for assimilation
    IF ( .NOT. ALLOCATED(XAT2M_ISBA)) ALLOCATE(XAT2M_ISBA(ILU,1))
    XAT2M_ISBA=XUNDEF
    YRECFM='T2M'
    CALL IO_BUFF(YRECFM,'R',GKNOWN)
    CALL READ_SURF(&
                 HPROGRAM,YRECFM,XAT2M_ISBA(:,1),IRESP)

    IF ( .NOT. ALLOCATED(XAHU2M_ISBA)) ALLOCATE(XAHU2M_ISBA(ILU,1))
    XAHU2M_ISBA=XUNDEF
    YRECFM='HU2M'
    CALL IO_BUFF(YRECFM,'R',GKNOWN)
    CALL READ_SURF(&
                 HPROGRAM,YRECFM,XAHU2M_ISBA(:,1),IRESP)

    IF ( .NOT. ALLOCATED(XAZON10M_ISBA)) ALLOCATE(XAZON10M_ISBA(ILU,1))
    XAZON10M_ISBA=XUNDEF
    YRECFM='ZON10M'
    CALL IO_BUFF(YRECFM,'R',GKNOWN)
    CALL READ_SURF(&
                 HPROGRAM,YRECFM,XAZON10M_ISBA(:,1),IRESP)

    IF ( .NOT. ALLOCATED(XAMER10M_ISBA)) ALLOCATE(XAMER10M_ISBA(ILU,1))
    XAMER10M_ISBA=XUNDEF
    YRECFM='MER10M'
    CALL IO_BUFF(YRECFM,'R',GKNOWN)
    CALL READ_SURF(&
                 HPROGRAM,YRECFM,XAMER10M_ISBA(:,1),IRESP)
  ELSEIF ( NIFIC/=NVAR+2 ) THEN
    ! Diagnostic fields for EKF assimilation ("observations")
    DO IOBS = 1,NOBSTYPE
     SELECT CASE (TRIM(COBS(IOBS)))
       CASE("T2M")
         IF ( .NOT. ALLOCATED(XAT2M_ISBA)) ALLOCATE(XAT2M_ISBA(ILU,1))
         XAT2M_ISBA=XUNDEF
         YRECFM='T2M'
         CALL IO_BUFF(YRECFM,'R',GKNOWN)
         CALL READ_SURF(&
                 HPROGRAM,YRECFM,XAT2M_ISBA(:,1),IRESP)
       CASE("HU2M")
         IF ( .NOT. ALLOCATED(XAHU2M_ISBA)) ALLOCATE(XAHU2M_ISBA(ILU,1))
         XAHU2M_ISBA=XUNDEF
         YRECFM='HU2M'
         CALL IO_BUFF(YRECFM,'R',GKNOWN)
         CALL READ_SURF(&
                 HPROGRAM,YRECFM,XAHU2M_ISBA(:,1),IRESP)
       CASE("WG1")
         ! This is already read above
       CASE("LAI")
         ! This is already read above   
       CASE("SWE")
         ! This is handled independently 
       CASE DEFAULT
         CALL ABOR1_SFX("Mapping of "//TRIM(COBS(IOBS))//" is not defined in READ_ISBA_n!")
     END SELECT
    ENDDO
  ENDIF
ENDIF
!
DEALLOCATE(ZWORK)
!
IF (LHOOK) CALL DR_HOOK('READ_ISBA_N',1,ZHOOK_HANDLE)
!
 CONTAINS
!
SUBROUTINE MAKE_ENS_ENKF(KWORK,KLU,HREC,PCOFSWI,PVAR,PRED_NOISE)
!
USE MODD_ASSIM, ONLY : LENS_GEN, XADDTIMECORR, XADDINFL, XASSIM_WINH
!
USE MODI_ADD_NOISE
USE MODE_RANDOM
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KWORK
INTEGER, INTENT(IN) :: KLU
 CHARACTER(LEN=3), INTENT(IN) :: HREC
REAL, DIMENSION(:), INTENT(IN) :: PCOFSWI
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PVAR
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRED_NOISE
!
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=4) :: YLVL
 CHARACTER(LEN=3) :: YVAR
REAL :: ZWHITE_NOISE, ZVAR0
INTEGER :: JL, JI, JP, IVAR
LOGICAL :: GPASS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('READ_ISBA_N:MAKE_ENS_ENKF',0,ZHOOK_HANDLE)
!
!
DO JL=1,KWORK
  !
  IF (KWORK>1) THEN
    WRITE(YLVL,'(I4)') JL
    YRECFM = TRIM(HREC)//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM = TRIM(HREC)
  ENDIF
  !
  IVAR = 0
  DO JVAR = 1,NVAR
    GPASS = ( TRIM(CVAR(JVAR))==TRIM(YRECFM) )
    IF (GPASS) THEN
      IVAR = JVAR
      EXIT
    ENDIF
  ENDDO
  !
  IF ( GPASS ) THEN
    !
    IF (XADDINFL(IVAR)>0.) THEN
      !
      IF (LASSIM) THEN
        !
        WRITE(YVAR,'(I3)') IVAR
        YRECFM='RED_NOISE'//ADJUSTL(YVAR(:LEN_TRIM(YVAR)))
        CALL READ_SURF(HPROGRAM,YRECFM,PRED_NOISE(:,:,IVAR),IRESP)
        !
      ELSEIF (.NOT.LENS_GEN .AND. XADDTIMECORR(IVAR)>0. ) THEN
        !
        WRITE(YVAR,'(I3)') IVAR
        YRECFM='RED_NOISE'//ADJUSTL(YVAR(:LEN_TRIM(YVAR)))
        CALL READ_SURF(HPROGRAM,YRECFM,PRED_NOISE(:,:,IVAR),IRESP)
        !
        DO JI = 1,KLU
          DO JP = 1,I%NPATCH
            ZWHITE_NOISE = XADDINFL(IVAR)*PCOFSWI(JI)*RANDOM_NORMAL()
            CALL ADD_NOISE(XADDTIMECORR(IVAR),XASSIM_WINH,ZWHITE_NOISE,PRED_NOISE(JI,JP,IVAR))
         ENDDO
         ENDDO
        !
        ZCOEF = XASSIM_WINH/24.
        !
      ELSE
        !
        DO JI = 1,ILU
          DO JP = 1,I%NPATCH 
            PRED_NOISE(JI,JP,IVAR) = XADDINFL(IVAR)*PCOFSWI(JI)*RANDOM_NORMAL()
          ENDDO
        ENDDO
        !
        ZCOEF = 1. 
        !
      ENDIF
      !
      IF (.NOT.LASSIM) THEN
        !
        DO JI = 1,ILU
          DO JP = 1,I%NPATCH
            IF ( PVAR(JI,JL,JP)/=XUNDEF ) THEN
              !
              ZVAR0 = PVAR(JI,JL,JP)
              !
              PVAR(JI,JL,JP) = PVAR(JI,JL,JP) + ZCOEF * PRED_NOISE(JI,JP,IVAR)
              !
              IF (PVAR(JI,JL,JP) < 0.) THEN
                IF (LENS_GEN) THEN
                  PVAR(JI,JL,JP) = ABS(PVAR(JI,JL,JP))
                ELSE
                  PVAR(JI,JL,JP) = ZVAR0
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        !
      ENDIF
      !
    ENDIF
    !
  ENDIF
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('READ_ISBA_N:MAKE_ENS_ENKF',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_ENS_ENKF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_ISBA_n
