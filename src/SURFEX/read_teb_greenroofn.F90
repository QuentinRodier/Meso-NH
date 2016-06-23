!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_TEB_GREENROOF_n (DTCO, U, TVG, GRM, &
                                       HPROGRAM,HPATCH)
!     ##################################
!
!!****  *READ_TEB_GREENROOF_n* - routine to initialise ISBA variables
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
!!    based on read_teb_greenroofn
!!
!!    AUTHOR
!!    ------
!!      C. de Munck & A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
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
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_CO2V_PAR,          ONLY : XANFMINIT, XCONDCTMIN
!                                
USE MODD_SURF_PAR,          ONLY : XUNDEF
USE MODD_SNOW_PAR,          ONLY : XZ0SN
!
USE MODI_READ_SURF
!
USE MODI_READ_GR_SNOW
!
!
!
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM

!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
 CHARACTER(LEN=3),  INTENT(IN)  :: HPATCH   ! current TEB patch identificator
!
!*       0.2   Declarations of local variables
!              -------------------------------
INTEGER           :: ILU                             ! 1D physical dimension
INTEGER           :: IRESP                           ! Error code after redding
INTEGER           :: IWORK                           ! Work integer
INTEGER           :: JLAYER, JNBIOMASS               ! loop counter on layers
 CHARACTER(LEN=30) :: YRECFM                          ! Name of the article to be read
 CHARACTER(LEN=4)  :: YLVL
REAL, DIMENSION(:),ALLOCATABLE  :: ZWORK             ! 2D array to write data in file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'TOWN  ',ILU)
!
!
!*       2.     Prognostic fields:
!               -----------------
!
ALLOCATE(ZWORK(ILU))
!
!* soil temperatures
!
IWORK = GRM%TGRO%NLAYER_GR
!
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(&
                 HPROGRAM,YRECFM,ZWORK(:),IRESP)
  GRM%TGR%CUR%XTG(:,JLAYER) = ZWORK
END DO
!
!
!* soil liquid water content
!
DO JLAYER=1,GRM%TGRO%NLAYER_GR
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(&
                 HPROGRAM,YRECFM,ZWORK(:),IRESP)
  GRM%TGR%CUR%XWG(:,JLAYER) = ZWORK
END DO
!
!* soil ice water content
!
DO JLAYER=1,GRM%TGRO%NLAYER_GR
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GR_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(&
                 HPROGRAM,YRECFM,ZWORK(:),IRESP)
  GRM%TGR%CUR%XWGI(:,JLAYER) = ZWORK
END DO
!
!* water intercepted on leaves
!
YRECFM=HPATCH//'GR_WR'
YRECFM=ADJUSTL(YRECFM)
 CALL READ_SURF(&
                 HPROGRAM,YRECFM,GRM%TGR%CUR%XWR(:),IRESP)
!
!* Leaf Area Index
!
IF (TVG%CPHOTO=='LAI' .OR. TVG%CPHOTO=='LST' .OR. TVG%CPHOTO=='NIT' .OR. TVG%CPHOTO=='NCB') THEN
  YRECFM = HPATCH//'GR_LAI'
  YRECFM=ADJUSTL(YRECFM)
  CALL READ_SURF(&
                 HPROGRAM,YRECFM,GRM%TGRPE%CUR%XLAI(:),IRESP)
END IF
!
!* snow mantel
!
 CALL READ_GR_SNOW(&
                   HPROGRAM,'GR',HPATCH,ILU,1,GRM%TGR%CUR%TSNOW  )! GROO:GreenROOf 
!
!-------------------------------------------------------------------------------
!
!*       4.  Semi-prognostic variables
!            -------------------------
!
!* aerodynamical resistance
!
YRECFM = HPATCH//'GR_RESA'
YRECFM=ADJUSTL(YRECFM)
GRM%TGR%CUR%XRESA(:) = 100.
 CALL READ_SURF(&
                 HPROGRAM,YRECFM,GRM%TGR%CUR%XRESA(:),IRESP)
!
GRM%TGR%CUR%XLE(:) = XUNDEF
!
!* ISBA-AGS variables
!
IF (TVG%CPHOTO/='NON') THEN
  GRM%TGR%CUR%XAN(:)    = 0.
  GRM%TGR%CUR%XANDAY(:) = 0.
  GRM%TGR%CUR%XANFM(:)  = XANFMINIT
  GRM%TGR%CUR%XLE(:)    = 0.
END IF
!
IF (TVG%CPHOTO=='AGS' .OR. TVG%CPHOTO=='AST') THEN
  GRM%TGR%CUR%XBIOMASS(:,:)      = 0.
  GRM%TGR%CUR%XRESP_BIOMASS(:,:) = 0.
ELSEIF (TVG%CPHOTO=='LAI' .OR. TVG%CPHOTO=='LST') THEN
  GRM%TGR%CUR%XBIOMASS(:,1)      = GRM%TGRP%XBSLAI(:) * GRM%TGRPE%CUR%XLAI(:)
  GRM%TGR%CUR%XRESP_BIOMASS(:,:) = 0.
ELSEIF (TVG%CPHOTO=='NIT') THEN
  GRM%TGR%CUR%XBIOMASS(:,:) = 0.
  DO JNBIOMASS=1,TVG%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GR_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(&
                 HPROGRAM,YRECFM,GRM%TGR%CUR%XBIOMASS(:,JNBIOMASS),IRESP)
  END DO

  GRM%TGR%CUR%XRESP_BIOMASS(:,:) = 0.
  DO JNBIOMASS=2,TVG%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GR_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    CALL READ_SURF(&
                 HPROGRAM,YRECFM,GRM%TGR%CUR%XRESP_BIOMASS(:,JNBIOMASS),IRESP)
  END DO
ENDIF
!
!
DEALLOCATE(ZWORK)
IF (LHOOK) CALL DR_HOOK('READ_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_TEB_GREENROOF_n
