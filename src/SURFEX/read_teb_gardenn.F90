!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_TEB_GARDEN_n(HPROGRAM,HPATCH)
!     ##################################
!
!!****  *READ_TEB_GARDEN_n* - routine to initialise ISBA variables
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
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CO2V_PAR,       ONLY : XANFMINIT, XCONDCTMIN
USE MODD_TEB_VEG_n,      ONLY : CPHOTO, CRESPSL, NNBIOMASS
USE MODD_TEB_GARDEN_n,   ONLY : NGROUND_LAYER,               &
                                XTG, XWG, XWGI, XWR, XLAI, TSNOW,   &
                                XRESA, XANFM, XANF, XAN, XLE, XANDAY,&
                                XBSLAI, XBIOMASS, XRESP_BIOMASS  
!                                
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SNOW_PAR,       ONLY : XZ0SN
!
USE MODI_READ_SURF
!
USE MODI_INIT_IO_SURF_n
USE MODI_SET_SURFEX_FILEIN
USE MODI_END_IO_SURF_n
USE MODI_TOWN_PRESENCE
USE MODI_ALLOCATE_GR_SNOW
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
 CHARACTER(LEN=3),  INTENT(IN)  :: HPATCH   ! current TEB patch identificator
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
LOGICAL           :: GTOWN          ! town variables written in the file
INTEGER           :: IVERSION, IBUGFIX
INTEGER           :: ILU            ! 1D physical dimension
INTEGER           :: IRESP          ! Error code after redding
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=4)  :: YLVL
REAL, DIMENSION(:),ALLOCATABLE  :: ZWORK      ! 2D array to write data in file
!
INTEGER :: IWORK   ! Work integer
!
INTEGER :: JLAYER, JNBIOMASS  ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_GARDEN_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n('TOWN  ',ILU)
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU))
!* soil temperatures
!
IWORK=NGROUND_LAYER
!
ALLOCATE(XTG(ILU,IWORK))
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I2)') JLAYER
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF
  YRECFM=ADJUSTL(YRECFM)  
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  XTG(:,JLAYER)=ZWORK
END DO
!
!
!* soil liquid water content
!
ALLOCATE(XWG(ILU,IWORK))
DO JLAYER=1,NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF  
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  XWG(:,JLAYER)=ZWORK
END DO
!
!* soil ice water content
!
ALLOCATE(XWGI(ILU,IWORK))
DO JLAYER=1,NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
! ajouter ici un test pour lire les anciens fichiers
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF  
  YRECFM=ADJUSTL(YRECFM)  
  CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:),IRESP)
  XWGI(:,JLAYER)=ZWORK
END DO
!
!* water intercepted on leaves
!
ALLOCATE(XWR(ILU))
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  YRECFM=HPATCH//'GD_WR'
ELSE
  YRECFM='TWN_WR'
ENDIF
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,XWR(:),IRESP)
!
!* Leaf Area Index (if prognostic)
!
IF (CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_LAI'
  ELSE
    YRECFM='TWN_LAI'
  ENDIF        
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,XLAI(:),IRESP)        
END IF
!
!* snow mantel
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ')
 CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL TOWN_PRESENCE(HPROGRAM,GTOWN)
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP')
 CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','READ ')
!
IF (.NOT. GTOWN) THEN
  TSNOW%SCHEME='1-L'
  CALL ALLOCATE_GR_SNOW(TSNOW,ILU,1)
ELSE
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    CALL READ_GR_SNOW(HPROGRAM,'GD',HPATCH,ILU,1,TSNOW  )
  ELSE
    CALL READ_GR_SNOW(HPROGRAM,'GARD',HPATCH,ILU,1,TSNOW  )
  ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
!* aerodynamical resistance
!
ALLOCATE(XRESA(ILU))
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  YRECFM=HPATCH//'GD_RES'
ELSE
  YRECFM='TWN_RESA'
ENDIF
YRECFM=ADJUSTL(YRECFM)
XRESA(:) = 100.
 CALL READ_SURF(HPROGRAM,YRECFM,XRESA(:),IRESP)
!
ALLOCATE(XLE(ILU))
XLE(:) = XUNDEF
!
!* ISBA-AGS variables
!
IF (CPHOTO/='NON') THEN
  ALLOCATE(XAN   (ILU)) 
  ALLOCATE(XANDAY(ILU)) 
  ALLOCATE(XANFM (ILU))
  ALLOCATE(XANF  (ILU))
  XAN(:)    = 0.
  XANDAY(:) = 0.
  XANFM(:)  = XANFMINIT
  XLE(:)    = 0.
ELSE
  ALLOCATE(XAN   (0)) 
  ALLOCATE(XANDAY(0)) 
  ALLOCATE(XANFM (0))
  ALLOCATE(XANF  (0))
ENDIF
!
IF(CPHOTO/='NON') THEN
  ALLOCATE(XBIOMASS         (ILU,NNBIOMASS))
  ALLOCATE(XRESP_BIOMASS    (ILU,NNBIOMASS))
ELSE
  ALLOCATE(XBIOMASS         (0,0))
  ALLOCATE(XRESP_BIOMASS    (0,0))
END IF
!
IF (CPHOTO=='AGS' .OR. CPHOTO=='AST') THEN
  !
  XBIOMASS(:,:) = 0.
  XRESP_BIOMASS(:,:) = 0.
ELSEIF (CPHOTO=='LAI' .OR. CPHOTO=='LST') THEN
  !
  XBIOMASS(:,1) = XBSLAI(:) * XLAI(:)
  XRESP_BIOMASS(:,:) = 0.
ELSEIF (CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
  !
  XBIOMASS(:,:) = 0.
  DO JNBIOMASS=1,NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
      YRECFM=HPATCH//'GD_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ELSE
      YRECFM='TWN_BIOMASS'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ENDIF
    CALL READ_SURF(HPROGRAM,YRECFM,XBIOMASS(:,JNBIOMASS),IRESP)
  END DO

  XRESP_BIOMASS(:,:) = 0.
  DO JNBIOMASS=2,NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
      YRECFM=HPATCH//'GD_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ELSE
      YRECFM='TWN_RESP_BIOM'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ENDIF    
    CALL READ_SURF(HPROGRAM,YRECFM,XRESP_BIOMASS(:,JNBIOMASS),IRESP)
  END DO
  !
ENDIF
!
DEALLOCATE(ZWORK)
IF (LHOOK) CALL DR_HOOK('READ_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_GARDEN_n
