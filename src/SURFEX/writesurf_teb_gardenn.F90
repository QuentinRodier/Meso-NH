!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_TEB_GARDEN_n (DGU, U, GDM, &
                                         HPROGRAM,HPATCH)
!     #####################################
!
!!****  *WRITESURF_TEB_GARDEN_n* - writes ISBA prognostic fields
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
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
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
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
USE MODI_WRITE_SURF
USE MODI_WRITESURF_GR_SNOW
USE MODD_DST_SURF
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
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=3),  INTENT(IN)  :: HPATCH   ! current teb patch
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=14) :: YFORM          ! Writing format
 CHARACTER(LEN=4 ) :: YLVL
!
INTEGER :: JLAYER ! loop counter on soil layers
!
REAL, DIMENSION(:),ALLOCATABLE  :: ZWORK      ! 2D array to write data in file
!
INTEGER :: JNBIOMASS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*       2.     Prognostic fields:
!               -----------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_GARDEN_N',0,ZHOOK_HANDLE)
ALLOCATE(ZWORK(SIZE(GDM%TGD%CUR%XTG,1)))
!* soil temperatures
!
DO JLAYER=1,GDM%TGDO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GD_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A11,I1.1,A4)'
  IF (JLAYER >= 10)  YFORM='(A11,I2.2,A4)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_GD_TG',JLAYER,' (K)'
  ZWORK=GDM%TGD%CUR%XTG(:,JLAYER)
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!* soil liquid water content
!
DO JLAYER=1,GDM%TGDO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GD_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A11,I1.1,A8)'
  IF (JLAYER >= 10)  YFORM='(A11,I2.2,A8)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_GD_WG',JLAYER,' (m3/m3)'
  ZWORK=GDM%TGD%CUR%XWG(:,JLAYER)
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!* soil ice water content
!
DO JLAYER=1,GDM%TGDO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JLAYER
  YRECFM=HPATCH//'GD_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A11,I1.1,A8)'
  IF (JLAYER >= 10)  YFORM='(A11,I2.2,A8)'
  WRITE(YCOMMENT,YFORM) 'X_Y_GD_WGI',JLAYER,' (m3/m3)'
  ZWORK=GDM%TGD%CUR%XWGI(:,JLAYER)
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
DEALLOCATE(ZWORK)
!
!* water intercepted on leaves
!
YRECFM=HPATCH//'GD_WR'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='X_Y_GD_WR (kg/m2)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,GDM%TGD%CUR%XWR(:),IRESP,HCOMMENT=YCOMMENT)
!
!* Leaf Area Index
!
IF (GDM%TVG%CPHOTO/='NON' .AND. GDM%TVG%CPHOTO/='AGS' .AND. GDM%TVG%CPHOTO/='AST') THEN
  YRECFM=HPATCH//'GD_LAI'
  YRECFM=ADJUSTL(YRECFM)
  YCOMMENT='X_Y_GD_LAI (m2/m2)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,GDM%TGDPE%CUR%XLAI(:),IRESP,HCOMMENT=YCOMMENT)
END IF
!
IF (GDM%TVG%CPHOTO=='NIT') THEN
  !
  DO JNBIOMASS=1,GDM%TVG%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GD_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YFORM='(A11,I1.1,A8)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_BIOMASS',JNBIOMASS,' (kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,GDM%TGD%CUR%XBIOMASS(:,JNBIOMASS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !
  DO JNBIOMASS=2,GDM%TVG%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//'GD_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YFORM='(A16,I1.1,A10)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_RESP_BIOMASS',JNBIOMASS,' (kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,GDM%TGD%CUR%XRESP_BIOMASS(:,JNBIOMASS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
END IF
!
!* aerodynamical resistance
!
YRECFM=HPATCH//'GD_RES'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='X_Y_GD_RESA (s/m)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,GDM%TGD%CUR%XRESA(:),IRESP,HCOMMENT=YCOMMENT)
!
!* snow mantel
!
YRECFM='GD'
 CALL WRITESURF_GR_SNOW(DGU, U, &
                        HPROGRAM,YRECFM,HPATCH,GDM%TGD%CUR%TSNOW)
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_GARDEN_n
