!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_ISBA_n(HPROGRAM)
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
!!	V. Masson   *Meteo France*	
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
!!     M.Moge    01/2016  using READ_SURF_FIELD2D/3D for 2D/3D surfex fields reads
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CO2V_PAR,       ONLY : XANFMINIT, XCONDCTMIN
USE MODD_ISBA_n,         ONLY : NGROUND_LAYER, NPATCH, NNBIOMASS,   &
                                  NNLITTER, NNLITTLEVS, NNSOILCARB,   &
                                  CPHOTO, CRESPSL, XTSRAD_NAT,        &
                                  XTG, XWG, XWGI, XWR, XLAI, TSNOW,   &
                                  XRESA, XANFM, XAN, XLE, XANDAY,     &
                                  XBSLAI, XBIOMASS, XRESP_BIOMASS,    &
                                  XLITTER, XSOILCARB, XLIGNIN_STRUC,  &
                                  LFLOOD, XZ0_FLOOD, LTEMP_ARP,       &
                                  NTEMPLAYER_ARP, LGLACIER, XICE_STO  
!                                
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SNOW_PAR,       ONLY : XZ0SN
!
USE MODI_READ_SURF
USE MODI_READ_SURF_FIELD3D
USE MODI_READ_SURF_FIELD2D
!
USE MODI_READ_GR_SNOW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
INTEGER           :: ILU          ! 1D physical dimension
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
 CHARACTER(LEN=4)  :: YLVL
 CHARACTER(LEN=8)  :: YPATCH
!
REAL, DIMENSION(:,:),ALLOCATABLE  :: ZWORK      ! 2D array to write data in file
!
INTEGER :: IWORK   ! Work integer
!
INTEGER :: JP, JL, JNBIOMASS, JNLITTER, JNSOILCARB, JNLITTLEVS  ! loop counter on layers
!
INTEGER           :: IVERSION       ! surface version
INTEGER           :: IBUGFIX
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
 CALL GET_TYPE_DIM_n('NATURE',ILU)
!
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU,NPATCH))
!* soil temperatures
!
IF(LTEMP_ARP)THEN
  IWORK=NTEMPLAYER_ARP
ELSE
  IWORK=NGROUND_LAYER
ENDIF
!
ALLOCATE(XTG(ILU,IWORK,NPATCH))
!
YRECFM='TG'
CALL READ_SURF_FIELD3D(HPROGRAM,XTG,1,IWORK,YRECFM)
!
!
!* soil liquid and ice water contents
!
ALLOCATE(XWG (ILU,NGROUND_LAYER,NPATCH))
ALLOCATE(XWGI(ILU,NGROUND_LAYER,NPATCH))
!
XWG (:,:,:)=XUNDEF
XWGI(:,:,:)=XUNDEF
!
YRECFM='WG'
CALL READ_SURF_FIELD3D(HPROGRAM,XWG,1,NGROUND_LAYER,YRECFM)
!
YRECFM='WGI'
CALL READ_SURF_FIELD3D(HPROGRAM,XWGI,1,NGROUND_LAYER,YRECFM)
!
!* water intercepted on leaves
!
ALLOCATE(XWR(ILU,NPATCH))
!
YRECFM='WR'
CALL READ_SURF_FIELD2D(HPROGRAM,XWR,YRECFM)
!
!* roughness length of Flood water
!
IF(LFLOOD)THEN
  ALLOCATE(XZ0_FLOOD(ILU,NPATCH))
  YRECFM = 'Z0_FLOOD'
  CALL READ_SURF_FIELD2D(HPROGRAM,XZ0_FLOOD,YRECFM)
ENDIF
!
!* Leaf Area Index
!
IF (CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
  YRECFM = 'LAI'
  CALL READ_SURF_FIELD2D(HPROGRAM,XLAI,YRECFM)
END IF
!
!* snow mantel
!
 CALL READ_GR_SNOW(HPROGRAM,'VEG','     ',ILU,NPATCH,TSNOW  )
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
IF(LGLACIER)THEN
  ALLOCATE(XICE_STO(ILU,NPATCH))
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM = 'ICE_STO'
    CALL READ_SURF_FIELD2D(HPROGRAM,XICE_STO,YRECFM)
  ELSE
    XICE_STO(:,:) = 0.0
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
ALLOCATE(XRESA(ILU,NPATCH))
ALLOCATE(XLE  (ILU,NPATCH))
IF (CPHOTO/='NON') THEN
  ALLOCATE(XANFM  (ILU,NPATCH))
  ALLOCATE(XAN    (ILU,NPATCH))
  ALLOCATE(XANDAY (ILU,NPATCH))
END IF
!
IF(CPHOTO/='NON') THEN
  ALLOCATE(XBIOMASS         (ILU,NNBIOMASS,NPATCH))
  ALLOCATE(XRESP_BIOMASS    (ILU,NNBIOMASS,NPATCH))
END IF
!
!
!* aerodynamical resistance
!
YRECFM = 'RESA'
XRESA(:,:) = 100.
CALL READ_SURF_FIELD2D(HPROGRAM,XRESA,YRECFM)
!
!* patch averaged radiative temperature (K)
!
ALLOCATE(XTSRAD_NAT(ILU))
IF (IVERSION<6) THEN
  XTSRAD_NAT(:)=0.
  DO JP=1,NPATCH
    XTSRAD_NAT(:)=XTSRAD_NAT(:)+XTG(:,1,JP)
  ENDDO
  XTSRAD_NAT(:)=XTSRAD_NAT(:)/NPATCH
ELSE
  YRECFM='TSRAD_NAT'
  CALL READ_SURF(HPROGRAM,YRECFM,XTSRAD_NAT(:),IRESP)
ENDIF
!
XLE(:,:) = XUNDEF
!
!*       5. ISBA-AGS variables
!
IF (CPHOTO/='NON') THEN
  YRECFM = 'AN'
  XAN(:,:) = 0.
  CALL READ_SURF_FIELD2D(HPROGRAM,XAN,YRECFM)
  !
  YRECFM = 'ANDAY'
  XANDAY(:,:) = 0.
  CALL READ_SURF_FIELD2D(HPROGRAM,XANDAY,YRECFM)
  !
  YRECFM = 'ANFM'
  XANFM(:,:) = XANFMINIT
  CALL READ_SURF_FIELD2D(HPROGRAM,XANFM,YRECFM)
  !
  YRECFM = 'LE_AGS'
  XLE(:,:) = 0.
  CALL READ_SURF_FIELD2D(HPROGRAM,XLE,YRECFM)
END IF
!
IF (CPHOTO=='AGS' .OR. CPHOTO=='AST') THEN
  !
  XBIOMASS(:,:,:) = 0.
  XRESP_BIOMASS(:,:,:) = 0.

ELSEIF (CPHOTO=='LAI' .OR. CPHOTO=='LST') THEN
  !
  XBIOMASS(:,1,:) = XBSLAI(:,:) * XLAI(:,:)
  XRESP_BIOMASS(:,:,:) = 0.

ELSEIF (CPHOTO=='NIT') THEN
  !
  XBIOMASS(:,:,:) = 0.
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM='BIOMA'
  ELSE
    YRECFM='BIOMASS'
  ENDIF
  CALL READ_SURF_FIELD3D(HPROGRAM,XBIOMASS,1,NNBIOMASS,YRECFM)
  !
  XRESP_BIOMASS(:,:,:) = 0.
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM='RESPI'
  ELSE
    YRECFM='RESP_BIOM'
  ENDIF
  CALL READ_SURF_FIELD3D(HPROGRAM,XRESP_BIOMASS,2,NNBIOMASS,YRECFM)
  !
ELSEIF (CPHOTO=='NCB') THEN
  !
  XBIOMASS(:,:,:) = 0.
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM='BIOMA'
  ELSE
    YRECFM='BIOMASS'
  ENDIF
  CALL READ_SURF_FIELD3D(HPROGRAM,XBIOMASS,1,NNBIOMASS,YRECFM)
  !
  XRESP_BIOMASS(:,:,:) = 0.
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM='RESPI'
  ELSE
    YRECFM='RESP_BIOM'
  ENDIF
  CALL READ_SURF_FIELD3D(HPROGRAM,XRESP_BIOMASS,2,NNBIOMASS-2,YRECFM)
  !
ENDIF
!
!*       6. Soil carbon
!
!
IF (CRESPSL=='CNT') THEN
  ALLOCATE(XLITTER          (ILU,NNLITTER,NNLITTLEVS,NPATCH))
  ALLOCATE(XSOILCARB        (ILU,NNSOILCARB,NPATCH))
  ALLOCATE(XLIGNIN_STRUC    (ILU,NNLITTLEVS,NPATCH))
END IF
!
IF (CRESPSL=='CNT') THEN
  !
  XLITTER(:,:,:,:) = 0.
  DO JNLITTER=1,NNLITTER
    DO JNLITTLEVS=1,NNLITTLEVS
      WRITE(YLVL,'(I1,A1,I1)') JNLITTER,'_',JNLITTLEVS
      YRECFM='LITTER'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      CALL READ_SURF_FIELD2D(HPROGRAM,ZWORK(:,:),YRECFM)
      XLITTER(:,JNLITTER,JNLITTLEVS,:)=ZWORK
    END DO
  END DO

  XSOILCARB(:,:,:) = 0.
  YRECFM='SOILCARB'
  CALL READ_SURF_FIELD3D(HPROGRAM,XSOILCARB,1,NNSOILCARB,YRECFM)
!
  XLIGNIN_STRUC(:,:,:) = 0.
  YRECFM='LIGNIN_STR'
  CALL READ_SURF_FIELD3D(HPROGRAM,XLIGNIN_STRUC,1,NNLITTLEVS,YRECFM)
!
ENDIF
!
!
DEALLOCATE(ZWORK)
IF (LHOOK) CALL DR_HOOK('READ_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_ISBA_n
