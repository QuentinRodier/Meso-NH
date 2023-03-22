!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CARBON_SOILDIF_PROPERTIES(IO, KK, PK, PEK, PCONTROL_TEMP,     &                                  
                                     PCONTROL_MOIST, PCONTROL_LEACH_SOIL,&
                                     PDBIO, PDIFBIO, PDCRYO, PDIFCRYO    )
!   ###############################################################
!!**  CARBON_SOILDIF_PROPERTIES 
!!
!!    PURPOSE
!!    -------
!!    Calculates soil physical and chemical properties for soil carbon scheme
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      Parton et al., Biogeochemestry, 1988
!!      Krinner et al., Global Biochemical Cycles, 2005
!!      Gibelin et al. 2008, AFM
!!      Morel et al. (2019) JAMES
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/06/20
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_CO2V_PAR,     ONLY : XALPHA_DOC
USE MODD_CSTS,         ONLY : XTT, XSIYEA
USE MODD_ISBA_PAR,     ONLY : XWGMIN
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE MODD_SOILCARB_PAR, ONLY : XBIOREF, XCRYOREF, XZBIO, XROOTBIO, &
                              XDBIO, XDCRYO, XDMAX_CRYO, XMAXALT, &
                              XKALT
!
USE MODI_CONTROL_MOIST_FUNC
USE MODI_CONTROL_TEMP_FUNC
!
USE MODI_COMPUT_COLD_LAYERS_THICK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*       0.1 input
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
!
!-------------------------------------------------------------------------------
!
!*       0.1 output
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCONTROL_TEMP        ! temperature control of heterotrophic respiration
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCONTROL_MOIST       ! moisture control of heterotrophic respiration
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCONTROL_LEACH_SOIL  ! leaching transfer function for doc
REAL, DIMENSION(:),   INTENT(OUT)     :: PDBIO                ! bioturbation limit (m)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PDIFBIO              ! bioturbation diffusivity (m2/s)
REAL, DIMENSION(:),   INTENT(OUT)     :: PDCRYO               ! cryoturbation limit (m)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PDIFCRYO             ! cryoturbation diffusivity (m2/s)
!
!-------------------------------------------------------------------------------
!
!*       0.2 local variables
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                  :: ZALT, ZPLT, ZFLT, ZZB, ZALT_REF, ZDMAX 
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2))  :: ZMOIST, ZSAT, ZTEMP, ZWGHT
!
INTEGER         :: INI, INL, INTYPE ! dimensions
!
INTEGER         :: JI, JL, JTYPE, IDEPTH  ! indices
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_SOILDIF_PROPERTIES',0,ZHOOK_HANDLE)
!
!*       1 Initialisations
!        -----------------------------------------------
!
!*       1.1 dimensions
!
INI    = SIZE(PEK%XTG,1)
INL    = SIZE(PEK%XTG,2)
INTYPE = SIZE(PK%XVEGTYPE_PATCH,2)
!
!*       1.2 set output to zero
!
PCONTROL_TEMP      (:,:) = 0.0
PCONTROL_MOIST     (:,:) = 0.0
PCONTROL_LEACH_SOIL(:,:) = 0.0
PDIFBIO            (:,:) = 0.0
PDIFCRYO           (:,:) = 0.0
!
PDBIO (:) = 0.0
PDCRYO(:) = 0.0
!
!*       1.3 init local
!
ZZB         (:) = 0.0
ZALT_REF    (:) = 0.0
ZDMAX       (:) = 0.0
!
ZMOIST(:,:) = 0.0
ZSAT  (:,:) = 0.0
ZTEMP (:,:) = 0.0
ZWGHT (:,:) = 0.0
!
ZPLT(:) = 0.0
ZALT(:) = 0.0
ZFLT(:) = 0.0
IF(IO%LBIOTURB.OR.IO%LCRYOTURB)THEN
  CALL COMPUT_COLD_LAYERS_THICK(PK%XDG,PEK%XTG,ZPLT,ZALT,ZFLT)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    2. Oxic decomposition control functions
!        ------------------------------------
!
DO JL=1,INL
   DO JI=1,INI
      IF(JL<=PK%NWG_LAYER(JI))THEN
        ZMOIST(JI,JL)=MIN(1.0,MAX(0.0,(PEK%XWG(JI,JL)-KK%XWWILT(JI,JL))/(KK%XWFC (JI,JL)-KK%XWWILT(JI,JL))))
        ZSAT  (JI,JL)=MIN(1.0,MAX(0.0,(PEK%XWG(JI,JL)-KK%XWFC  (JI,JL))/(KK%XWSAT(JI,JL)-KK%XWFC  (JI,JL))))
      ENDIF
   ENDDO
ENDDO
!
ZTEMP(:,:) = PEK%XTG(:,:)-XTT
!
DO JL=1,INL
   PCONTROL_TEMP (:,JL) = CONTROL_TEMP_FUNC(ZTEMP(:,JL))
   PCONTROL_MOIST(:,JL) = CONTROL_MOIST_FUNC(ZMOIST(:,JL),ZSAT(:,JL),OSOILGAS=IO%LSOILGAS)
ENDDO
!
DO JL=1,INL
   DO JI=1,INI
      IF(JL>PK%NWG_LAYER(JI))THEN
        PCONTROL_TEMP (JI,JL)=0.0
        PCONTROL_MOIST(JI,JL)=0.0
      ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!
!*    3. Dissolved organic control functions
!        -----------------------------------
!
IF(IO%LCLEACH)THEN
  DO JL=1,INL
     DO JI=1,INI
        IF(JL<=PK%NWG_LAYER(JI))THEN
          PCONTROL_LEACH_SOIL(JI,JL) = XALPHA_DOC * KK%XFSAT(JI)
        ENDIF
     ENDDO
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
!
!
!*    4. Bioturbation diffusivity profile
!        -----------------------------------
!
IF(IO%LBIOTURB)THEN
!
  DO JTYPE=1,INTYPE
     DO JI=1,INI
          ZZB(JI)=ZZB(JI)+XZBIO(JTYPE)*PK%XVEGTYPE_PATCH(JI,JTYPE)
     ENDDO
  ENDDO
!
! Bioturbation limit (m) = 95% of total (root) profile
  DO JI=1,INI
     IDEPTH    = PK%NWG_LAYER(JI)
     PDBIO(JI) = MAX(XDBIO,MIN(-ZZB(JI)*LOG(1.0-XROOTBIO),PK%XDG(JI,IDEPTH)))
  ENDDO
!
! Bioturbation between two nodes (m2/s)
  DO JL=1,INL
     DO JI=1,INI
        IF(JL<=PK%NWG_LAYER(JI).AND.ZFLT(JI)==0.0.AND.PK%XDG(JI,JL)<=PDBIO(JI).AND.ZZB(JI)>0.0)THEN
          PDIFBIO(JI,JL)=(XBIOREF/XSIYEA)*EXP(-PK%XDG(JI,JL)/ZZB(JI))
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!
!*    5. Cryoturbation diffusivity profile
!        ---------------------------------
!
IF(IO%LCRYOTURB)THEN
!
! No bioturbation over permafrost because cryoturbation
  DO JL=1,INL
     DO JI=1,INI
        IF(ZPLT(JI)>0.0)THEN
          PDIFBIO(JI,JL)=0.0
        ENDIF
     ENDDO
  ENDDO
!
  ZALT_REF(:)=MIN(ZALT(:),XMAXALT)
  PDCRYO  (:)=MIN(XKALT*ZALT(:),XDMAX_CRYO)
!
! Cryoturbation between two nodes (m2/s) following Koven et al. (2013)
  DO JL=1,INL
     DO JI=1,INI
        IDEPTH=PK%NWG_LAYER(JI)
        IF(JL<IDEPTH.AND.ZPLT(JI)>0.0.AND.PDCRYO(JI)>=XDCRYO)THEN
          PDIFCRYO(JI,JL) = (XCRYOREF/XSIYEA)*MAX(0.0,1.0-MAX(0.0,PK%XDG(JI,JL)-ZALT_REF(JI))/(PDCRYO(JI)-ZALT_REF(JI)))
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_SOILDIF_PROPERTIES',1,ZHOOK_HANDLE)
!
END SUBROUTINE CARBON_SOILDIF_PROPERTIES
