!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_TEB_GARDEN_n (DTCO, DGU, U, GDM, &
                                    HPROGRAM,HPATCH)
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
!!      V. Masson   *Meteo France*
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
!
USE MODD_CO2V_PAR,       ONLY : XANFMINIT, XCONDCTMIN
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
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
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'TOWN  ',ILU)
!
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU))
!* soil temperatures
!
IWORK=GDM%TGDO%NGROUND_LAYER
!
ALLOCATE(GDM%TGD%CUR%XTG(ILU,IWORK))
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I2)') JLAYER
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF
  YRECFM=ADJUSTL(YRECFM)  
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ZWORK(:),IRESP)
  GDM%TGD%CUR%XTG(:,JLAYER)=ZWORK
END DO
!
!
!* soil liquid water content
!
ALLOCATE(GDM%TGD%CUR%XWG(ILU,IWORK))
DO JLAYER=1,GDM%TGDO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF  
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ZWORK(:),IRESP)
  GDM%TGD%CUR%XWG(:,JLAYER)=ZWORK
END DO
!
!* soil ice water content
!
ALLOCATE(GDM%TGD%CUR%XWGI(ILU,IWORK))
DO JLAYER=1,GDM%TGDO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
! ajouter ici un test pour lire les anciens fichiers
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ELSE
    YRECFM='TWN_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  ENDIF  
  YRECFM=ADJUSTL(YRECFM)  
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ZWORK(:),IRESP)
  GDM%TGD%CUR%XWGI(:,JLAYER)=ZWORK
END DO
!
!* water intercepted on leaves
!
ALLOCATE(GDM%TGD%CUR%XWR(ILU))
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  YRECFM=HPATCH//'GD_WR'
ELSE
  YRECFM='TWN_WR'
ENDIF
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(&
                HPROGRAM,YRECFM,GDM%TGD%CUR%XWR(:),IRESP)
!
!* Leaf Area Index (if prognostic)
!
IF (GDM%TVG%CPHOTO=='LAI' .OR. GDM%TVG%CPHOTO=='LST' .OR. &
                GDM%TVG%CPHOTO=='NIT' .OR. GDM%TVG%CPHOTO=='NCB') THEN
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    YRECFM=HPATCH//'GD_LAI'
  ELSE
    YRECFM='TWN_LAI'
  ENDIF        
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(&
                HPROGRAM,YRECFM,GDM%TGDPE%CUR%XLAI(:),IRESP)        
END IF
!
!* snow mantel
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PGD ')
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'TOWN  ','TEB   ','READ ')
!
 CALL TOWN_PRESENCE(&
                    HPROGRAM,GTOWN)
!
 CALL END_IO_SURF_n(HPROGRAM)
 CALL SET_SURFEX_FILEIN(HPROGRAM,'PREP')
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'TOWN  ','TEB   ','READ ')
!
IF (.NOT. GTOWN) THEN
  GDM%TGD%CUR%TSNOW%SCHEME='1-L'
  CALL ALLOCATE_GR_SNOW(GDM%TGD%CUR%TSNOW,ILU,1)
ELSE
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
    CALL READ_GR_SNOW(&
                      HPROGRAM,'GD',HPATCH,ILU,1,GDM%TGD%CUR%TSNOW  )
  ELSE
    CALL READ_GR_SNOW(&
                      HPROGRAM,'GARD',HPATCH,ILU,1,GDM%TGD%CUR%TSNOW  )
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
ALLOCATE(GDM%TGD%CUR%XRESA(ILU))
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  YRECFM=HPATCH//'GD_RES'
ELSE
  YRECFM='TWN_RESA'
ENDIF
YRECFM=ADJUSTL(YRECFM)
GDM%TGD%CUR%XRESA(:) = 100.
 CALL READ_SURF(&
                HPROGRAM,YRECFM,GDM%TGD%CUR%XRESA(:),IRESP)
!
ALLOCATE(GDM%TGD%CUR%XLE(ILU))
GDM%TGD%CUR%XLE(:) = XUNDEF
!
!* ISBA-AGS variables
!
IF (GDM%TVG%CPHOTO/='NON') THEN
  ALLOCATE(GDM%TGD%CUR%XAN   (ILU)) 
  ALLOCATE(GDM%TGD%CUR%XANDAY(ILU)) 
  ALLOCATE(GDM%TGD%CUR%XANFM (ILU))
  ALLOCATE(GDM%TGDP%XANF  (ILU))
  GDM%TGD%CUR%XAN(:)    = 0.
  GDM%TGD%CUR%XANDAY(:) = 0.
  GDM%TGD%CUR%XANFM(:)  = XANFMINIT
  GDM%TGD%CUR%XLE(:)    = 0.
ELSE
  ALLOCATE(GDM%TGD%CUR%XAN   (0)) 
  ALLOCATE(GDM%TGD%CUR%XANDAY(0)) 
  ALLOCATE(GDM%TGD%CUR%XANFM (0))
  ALLOCATE(GDM%TGDP%XANF  (0))
ENDIF
!
IF(GDM%TVG%CPHOTO/='NON') THEN
  ALLOCATE(GDM%TGD%CUR%XBIOMASS         (ILU,GDM%TVG%NNBIOMASS))
  ALLOCATE(GDM%TGD%CUR%XRESP_BIOMASS    (ILU,GDM%TVG%NNBIOMASS))
ELSE
  ALLOCATE(GDM%TGD%CUR%XBIOMASS         (0,0))
  ALLOCATE(GDM%TGD%CUR%XRESP_BIOMASS    (0,0))
END IF
!
IF (GDM%TVG%CPHOTO=='AGS' .OR. GDM%TVG%CPHOTO=='AST') THEN
  !
  GDM%TGD%CUR%XBIOMASS(:,:) = 0.
  GDM%TGD%CUR%XRESP_BIOMASS(:,:) = 0.
ELSEIF (GDM%TVG%CPHOTO=='LAI' .OR. GDM%TVG%CPHOTO=='LST') THEN
  !
  GDM%TGD%CUR%XBIOMASS(:,1) = GDM%TGDP%XBSLAI(:) * GDM%TGDPE%CUR%XLAI(:)
  GDM%TGD%CUR%XRESP_BIOMASS(:,:) = 0.
ELSEIF (GDM%TVG%CPHOTO=='NIT' .OR. GDM%TVG%CPHOTO=='NCB') THEN
  !
  GDM%TGD%CUR%XBIOMASS(:,:) = 0.
  DO JNBIOMASS=1,GDM%TVG%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
      YRECFM=HPATCH//'GD_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ELSE
      YRECFM='TWN_BIOMASS'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ENDIF
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(&
                HPROGRAM,YRECFM,GDM%TGD%CUR%XBIOMASS(:,JNBIOMASS),IRESP)
  END DO

  GDM%TGD%CUR%XRESP_BIOMASS(:,:) = 0.
  DO JNBIOMASS=2,GDM%TVG%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
      YRECFM=HPATCH//'GD_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ELSE
      YRECFM='TWN_RESP_BIOM'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    ENDIF    
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(&
                HPROGRAM,YRECFM,GDM%TGD%CUR%XRESP_BIOMASS(:,JNBIOMASS),IRESP)
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
