!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_TEB_VEG (HSELECT, OSNOWDIMNC, IO, S, PEK, P, HPROGRAM,HSURF,HPATCH)
!     #####################################
!
!!****  *WRITESURF_TEB_VEG* - writes ISBA prognostic fields
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
!!      M. Goret     07/2017 : add writing of respi for the first biomass compartment!* respiration option
!!      M. Goret     08/2017 : add RESPSL option
!!      V. Masson    11/2018 : merges writesurf_teb_garden/greenroof
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_PE_t, ISBA_S_t, ISBA_P_t
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
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT
 LOGICAL, INTENT(IN) :: OSNOWDIMNC
!
TYPE(ISBA_OPTIONS_t), INTENT(IN) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_PE_t), INTENT(IN) :: PEK
TYPE(ISBA_P_t), INTENT(IN) :: P
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=2),  INTENT(IN)  :: HSURF    ! current surface ('GD','GR','GH')
 CHARACTER(LEN=3),  INTENT(IN)  :: HPATCH   ! current teb patch
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(SIZE(PEK%XTG,1)) :: IMASK_P
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=18) :: YFORM          ! Writing format
 CHARACTER(LEN=4 ) :: YLVL
!
INTEGER :: JL, JI ! loop counter on soil layers
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
!
!
IF (HSURF/='GH') THEN
!
ALLOCATE(ZWORK(SIZE(PEK%XTG,1)))
!* soil temperatures
!
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//HSURF//'_TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A4,A2,A3,I1.1,A4)'
  IF (JL >= 10)  YFORM='(A4,A2,A3,I2.2,A4)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_',HSURF,'_TG',JL,' (K)'
  ZWORK=PEK%XTG(:,JL)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!* soil liquid water content
!
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//HSURF//'_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A4,A2,A3,I1.1,A8)'
  IF (JL >= 10)  YFORM='(A4,A2,A3,I2.2,A8)'
  WRITE(YCOMMENT,FMT=YFORM) 'X_Y_',HSURF,'_WG',JL,' (m3/m3)'
  ZWORK=PEK%XWG(:,JL)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
!
!* soil ice water content
!
DO JL=1,IO%NGROUND_LAYER
  WRITE(YLVL,'(I2)') JL
  YRECFM=HPATCH//HSURF//'_WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=ADJUSTL(YRECFM)
  YFORM='(A4,A2,A4,I1.1,A8)'
  IF (JL >= 10)  YFORM='(A4,A2,A4,I2.2,A8)'
  WRITE(YCOMMENT,YFORM) 'X_Y_',HSURF,'_WGI',JL,' (m3/m3)'
  ZWORK=PEK%XWGI(:,JL)
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,ZWORK,IRESP,HCOMMENT=YCOMMENT)
END DO
!
DEALLOCATE(ZWORK)
!
!* Root fraction profile
!
IF (HSURF=='GD') THEN
  DO JL=1,SIZE(P%XROOTFRAC,2)
    WRITE(YLVL,'(I2.2)') JL
    YRECFM='ROOTFRAC_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YCOMMENT='X_Y_'//YRECFM
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,P%XROOTFRAC(:,JL),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
ENDIF
!
!* water intercepted on leaves
!
YRECFM=HPATCH//HSURF//'_WR'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='X_Y_'//HSURF//'_WR (kg/m2)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XWR(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!
!* High vegetation temperature
!
IF (HSURF=='GH') THEN
  YRECFM=HPATCH//HSURF//'_TV'
  YRECFM=ADJUSTL(YRECFM)
  YCOMMENT='X_Y_'//HSURF//'_TV (K)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XTV(:),IRESP,HCOMMENT=YCOMMENT) 
END IF
!
!* Leaf Area Index (Garden, Greenroof, and High vegetation)
!
IF (IO%CPHOTO/='NON' .AND. IO%CPHOTO/='AST') THEN
  YRECFM=HPATCH//HSURF//'_LAI'
  YRECFM=ADJUSTL(YRECFM)
  YCOMMENT='X_Y_'//HSURF//'_LAI (m2/m2)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XLAI(:),IRESP,HCOMMENT=YCOMMENT)
END IF
!
IF (IO%CPHOTO=='NIT') THEN
  !
  DO JNBIOMASS=1,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//HSURF//'_BIOMA'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YFORM='(A11,I1.1,A8)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_BIOMASS',JNBIOMASS,' (kg/m2)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XBIOMASS(:,JNBIOMASS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !
  DO JNBIOMASS=1,IO%NNBIOMASS
    WRITE(YLVL,'(I1)') JNBIOMASS
    YRECFM=HPATCH//HSURF//'_RESPI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=ADJUSTL(YRECFM)
    YFORM='(A16,I1.1,A10)'
    WRITE(YCOMMENT,FMT=YFORM) 'X_Y_RESP_BIOMASS',JNBIOMASS,' (kg/m2/s)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XRESP_BIOMASS(:,JNBIOMASS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
END IF
!
!* respiration option
!
YRECFM=HPATCH//HSURF//'_RESPSL'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT=YRECFM
CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,IO%CRESPSL,IRESP,HCOMMENT=YCOMMENT)
!
!
!* aerodynamical resistance
!
YRECFM=HPATCH//HSURF//'_RES'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='X_Y_'//HSURF//'_RESA (s/m)'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,PEK%XRESA(:),IRESP,HCOMMENT=YCOMMENT)
!
!* snow mantel
!
IF (HSURF/='GH') THEN
YRECFM=HSURF
DO JI = 1,SIZE(IMASK_P)
  IMASK_P(JI) = JI
ENDDO
 CALL WRITESURF_GR_SNOW(OSNOWDIMNC, HSELECT, HPROGRAM, YRECFM, HPATCH,&
                        SIZE(PEK%XTG,1), IMASK_P, 0, PEK%TSNOW, S%XWSN_WR, &
                        S%XRHO_WR, S%XHEA_WR, S%XAGE_WR, S%XSG1_WR, S%XSG2_WR, &
                        S%XHIS_WR, S%XALB_WR, S%XIMP_WR)
!
END IF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_VEG
