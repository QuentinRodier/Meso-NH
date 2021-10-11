!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
!
SUBROUTINE EXPLICIT_LONGWAVE (TOP, T, B, LW, DMT, CT, PLW_RAD, PTSRAD_GD, &
     PTS_HVEG, PDN_RD, PDF_RD, PDN_RF, PEMIS_GD, PEMIS_HVEG, HTEST)
  !
  !   ##########################################################################
  !
  !!****  *EXPLICIT_LONGWAVE*  
  !!
  !!    PURPOSE
  !!    -------
  !     Explicit calculation of longwave radiation
  !     
  !!**  METHOD
  !     ------
  !
  !
  !!    EXTERNAL
  !!    --------
  !!
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!      
  !!    REFERENCE
  !!    ---------
  !!
  !!      
  !!    AUTHOR
  !!    ------
  !!
  !!      R. Schoetter          * Meteo-France *
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!    Original    10/2020
  !----------------------------------------------------------------------------------------------
  !
  !*       0.     DECLARATIONS
  !               ------------
  !
  USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
  USE MODD_TEB_n, ONLY : TEB_t
  USE MODD_BEM_n, ONLY : BEM_t
  USE MODD_LW_COEF, ONLY : LW_COEF_t
  USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
  USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
  !
  USE MODD_CSTS, ONLY : XSTEFAN
  USE MODD_SURF_PAR, ONLY : XUNDEF, XSURF_EPSILON
  USE MODD_TEB_PAR, ONLY: XEMIS_WIN_CST
  !
  USE MODI_ABOR1_SFX
  !
  USE YOMHOOK, ONLY : LHOOK, DR_HOOK
  USE PARKIND1, ONLY : JPRB
  !
  IMPLICIT NONE
  !
  !*      0.1    Declarations of arguments
  !
  TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
  TYPE(TEB_t), INTENT(IN) :: T
  TYPE(BEM_t), INTENT(IN) :: B
  TYPE(LW_COEF_t), INTENT(IN) :: LW
  TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
  TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
  !
  CHARACTER(LEN=2), INTENT(IN) :: HTEST        ! Must be equal to 'OK'  
  !
  REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD    ! Atmospheric infrared radiation
  REAL, DIMENSION(:), INTENT(IN) :: PTSRAD_GD  ! Skin temperature of low vegetation
  REAL, DIMENSION(:), INTENT(IN) :: PTS_HVEG   ! Skin temperature of high vegetation
  REAL, DIMENSION(:), INTENT(IN) :: PDN_RD     ! Fraction of snow on road
  REAL, DIMENSION(:), INTENT(IN) :: PDF_RD     ! Fraction of snow-free road
  REAL, DIMENSION(:), INTENT(IN) :: PDN_RF     ! Fraction of snow on roof
  REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GD   ! Emissivity of garden
  REAL, DIMENSION(:), INTENT(IN) :: PEMIS_HVEG ! Emissivity of urban trees
  !
  !*      0.2    Declarations of local variables
  !
  REAL, DIMENSION(SIZE(PLW_RAD)) :: ZT_SKY
  REAL, DIMENSION(SIZE(PLW_RAD)) :: ZLW_W_TO_WIN
  REAL, DIMENSION(SIZE(PLW_RAD)) :: ZTS_WL
  !
  INTEGER :: JI
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !-------------------------------------------------------------------------------
  !
  IF (LHOOK) CALL DR_HOOK('EXPLICIT_LONGWAVE',0,ZHOOK_HANDLE)
  !
  IF (HTEST/='OK') THEN
     CALL ABOR1_SFX('EXPLICIT_LONGWAVE: FATAL ERROR DURING ARGUMENT TRANSFER')
  ENDIF
  !
  ! Sky radiative temperature
  !
  ZT_SKY(:) = (PLW_RAD(:)/XSTEFAN)**0.25
  !
  ! Longwave absorption by the roof
  !
  DMT%XABS_LW_ROOF(:) = T%XEMIS_ROOF(:) * (PLW_RAD(:) - XSTEFAN * T%XT_ROOF(:,1)** 4)
  !
  ! Longwave absorption by the road
  !
  DMT%XABS_LW_ROAD(:) = &
       LW%XLW_S_TO_R  (:) * ( ZT_SKY(:)        - T%XT_ROAD(:,1) ) + &
       LW%XLW_WA_TO_R (:) * ( T%XT_WALL_A(:,1) - T%XT_ROAD(:,1) ) + &
       LW%XLW_WB_TO_R (:) * ( T%XT_WALL_B(:,1) - T%XT_ROAD(:,1) ) + &
       LW%XLW_WIN_TO_R(:) * ( B%XT_WIN1  (:)   - T%XT_ROAD(:,1) ) + &
       LW%XLW_HV_TO_R (:) * ( PTS_HVEG   (:)   - T%XT_ROAD(:,1) ) 
  !
  IF (CT%LCHECK_TEB) THEN
     CT%XLW_WALA_TO_ROAD(:)=T%XROAD(:)*PDF_RD(:)*LW%XLW_WA_TO_R (:)*(T%XT_WALL_A(:,1) - T%XT_ROAD(:,1))
     CT%XLW_WALB_TO_ROAD(:)=T%XROAD(:)*PDF_RD(:)*LW%XLW_WB_TO_R (:)*(T%XT_WALL_B(:,1) - T%XT_ROAD(:,1))
     CT%XLW_WIND_TO_ROAD(:)=T%XROAD(:)*PDF_RD(:)*LW%XLW_WIN_TO_R(:)*(B%XT_WIN1(:)     - T%XT_ROAD(:,1))
     CT%XLW_HV_TO_ROAD  (:)=T%XROAD(:)*PDF_RD(:)*LW%XLW_HV_TO_R (:)*(PTS_HVEG(:)      - T%XT_ROAD(:,1))
  END IF
  !
  ! Longwave absorption by wall A
  !
  DMT%XABS_LW_WALL_A(:) = &
       LW%XLW_S_TO_WA(:)               * ( ZT_SKY(:)          - T%XT_WALL_A(:,1) ) + &
       PDF_RD(:) * LW%XLW_R_TO_WA(:)   * ( T%XT_ROAD(:,1)     - T%XT_WALL_A(:,1) ) + &
       LW%XLW_WA_TO_WB (:)             * ( T%XT_WALL_B(:,1)   - T%XT_WALL_A(:,1) ) + &
       LW%XLW_WIN_TO_WA(:)             * ( B%XT_WIN1(:)       - T%XT_WALL_A(:,1) ) + &
       PDN_RD(:) * LW%XLW_NR_TO_WA(:)  * ( T%TSNOW_ROAD%TS(:) - T%XT_WALL_A(:,1) )
  !
  IF (SIZE(PTSRAD_GD)>0) THEN
     DMT%XABS_LW_WALL_A(:) = DMT%XABS_LW_WALL_A(:) + LW%XLW_G_TO_WA(:) * ( PTSRAD_GD (:) - T%XT_WALL_A(:,1) )
  ENDIF
  !
  IF (SIZE(PTS_HVEG)>0) THEN
     DMT%XABS_LW_WALL_A(:) = DMT%XABS_LW_WALL_A(:) + LW%XLW_HV_TO_WA(:) * ( PTS_HVEG (:) - T%XT_WALL_A(:,1) )
  ENDIF
  !
  IF (CT%LCHECK_TEB) THEN
     !
     CT%XLW_ROAD_TO_WALA(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
          PDF_RD(:)*LW%XLW_R_TO_WA(:)  * ( T%XT_ROAD(:,1)     - T%XT_WALL_A(:,1) )
     CT%XLW_SNOW_TO_WALA(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
          PDN_RD(:)*LW%XLW_NR_TO_WA(:) * ( T%TSNOW_ROAD%TS(:) - T%XT_WALL_A(:,1) )
     CT%XLW_WALB_TO_WALA(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
          LW%XLW_WA_TO_WB(:)           * ( T%XT_WALL_B(:,1)   - T%XT_WALL_A(:,1) )
     CT%XLW_WIND_TO_WALA(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
          LW%XLW_WIN_TO_WA(:)          * ( B%XT_WIN1(:)       - T%XT_WALL_A(:,1) )
     !
     IF (SIZE(PTSRAD_GD)>0) THEN
        CT%XLW_GARD_TO_WALA (:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
             LW%XLW_G_TO_WA(:) * ( PTSRAD_GD(:) - T%XT_WALL_A(:,1) )
     ENDIF
     !
     IF (SIZE(PTS_HVEG)>0) THEN
        CT%XLW_HV_TO_WALA (:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
             LW%XLW_HV_TO_WA(:) * ( PTS_HVEG(:) - T%XT_WALL_A(:,1) )
     ENDIF
     !
  ENDIF
  !
  ! Longwave absorption by wall B
  !
  DMT%XABS_LW_WALL_B(:) = &
       LW%XLW_S_TO_WB(:) *               ( ZT_SKY(:)          - T%XT_WALL_B(:,1) ) + &
       PDF_RD(:) * LW%XLW_R_TO_WB  (:) * ( T%XT_ROAD(:,1)     - T%XT_WALL_B(:,1) ) + &
       LW%XLW_WA_TO_WB (:) *             ( T%XT_WALL_A(:,1)   - T%XT_WALL_B(:,1) ) + &
       LW%XLW_WIN_TO_WB(:) *             ( B%XT_WIN1(:)       - T%XT_WALL_B(:,1) ) + &
       PDN_RD(:) * LW%XLW_NR_TO_WB (:) * ( T%TSNOW_ROAD%TS(:) - T%XT_WALL_B(:,1) )
  !
  IF (SIZE(PTSRAD_GD)>0) THEN
     DMT%XABS_LW_WALL_B(:) = DMT%XABS_LW_WALL_B(:) + LW%XLW_G_TO_WB(:) * ( PTSRAD_GD(:) - T%XT_WALL_B(:,1) )
  ENDIF
  !
  IF (SIZE(PTS_HVEG)>0) THEN
     DMT%XABS_LW_WALL_B(:) = DMT%XABS_LW_WALL_B(:) + LW%XLW_HV_TO_WB(:) * ( PTS_HVEG(:) - T%XT_WALL_B(:,1) )
  ENDIF
  !
  IF (CT%LCHECK_TEB) THEN
     !
     CT%XLW_ROAD_TO_WALB(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
          PDF_RD(:)*LW%XLW_R_TO_WB(:)  * ( T%XT_ROAD(:,1)     - T%XT_WALL_B(:,1) )
     CT%XLW_SNOW_TO_WALB(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
          PDN_RD(:)*LW%XLW_NR_TO_WB(:) * ( T%TSNOW_ROAD%TS(:) - T%XT_WALL_B(:,1) )
     CT%XLW_WALA_TO_WALB(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
          LW%XLW_WA_TO_WB(:)           * ( T%XT_WALL_A(:,1)   - T%XT_WALL_B(:,1) )
     CT%XLW_WIND_TO_WALB(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
          LW%XLW_WIN_TO_WB(:)          * ( B%XT_WIN1(:)       - T%XT_WALL_B(:,1) )
     !
     IF (SIZE(PTSRAD_GD)>0) THEN
        CT%XLW_GARD_TO_WALB(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
             LW%XLW_G_TO_WB(:)         * ( PTSRAD_GD(:) - T%XT_WALL_B(:,1) )
     ENDIF
     !
     IF (SIZE(PTS_HVEG)>0) THEN
        CT%XLW_HV_TO_WALB(:)=0.5*T%XBLD(:)*T%XWALL_O_BLD(:)* &
             LW%XLW_HV_TO_WB(:)          * ( PTS_HVEG(:) - T%XT_WALL_B(:,1) )
     ENDIF
     !
  ENDIF
  !
  ! Longwave absorption by the windows
  !
  ZLW_W_TO_WIN(:) = 0.5 * (LW%XLW_WA_TO_WIN(:) + LW%XLW_WB_TO_WIN(:))
  ZTS_WL(:) = XUNDEF
  !
  WHERE (ZLW_W_TO_WIN(:)>0.)
     ZTS_WL(:) = 0.5 * ( LW%XLW_WA_TO_WIN(:) * T%XT_WALL_A(:,1)   + &
          LW%XLW_WB_TO_WIN(:) * T%XT_WALL_B(:,1) ) / &
          ZLW_W_TO_WIN(:)
  ENDWHERE
  !
  DMT%XABS_LW_WIN(:) = LW%XLW_S_TO_WIN(:)  * ( ZT_SKY (:)         - B%XT_WIN1(:) ) + &
       (1.-PDN_RD(:)) *  LW%XLW_R_TO_WIN(:)  * ( T%XT_ROAD(:,1)     - B%XT_WIN1(:) ) + &
       ZLW_W_TO_WIN(:)     * ( ZTS_WL (:)         - B%XT_WIN1(:) ) + &
       PDN_RD(:) * LW%XLW_NR_TO_WIN(:) * ( T%TSNOW_ROAD%TS(:) - B%XT_WIN1(:) )
  !
  IF (SIZE(PTSRAD_GD)>0) THEN
     DMT%XABS_LW_WIN(:) = DMT%XABS_LW_WIN(:) +  LW%XLW_G_TO_WIN(:) * ( PTSRAD_GD(:) - B%XT_WIN1(:) )
  ENDIF
  !
  IF (SIZE(PTS_HVEG)>0) THEN
     DMT%XABS_LW_WIN(:) = DMT%XABS_LW_WIN(:) +  LW%XLW_HV_TO_WIN(:) * ( PTS_HVEG(:) - B%XT_WIN1(:) )
  ENDIF
  !
  IF (CT%LCHECK_TEB) THEN
     !
     CT%XLW_ROAD_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)*(1.-PDN_RD(:)) * LW%XLW_R_TO_WIN(:)  * ( T%XT_ROAD(:,1)    - B%XT_WIN1(:) )
     CT%XLW_SNOW_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)*PDN_RD(:)      * LW%XLW_NR_TO_WIN(:) * (T%TSNOW_ROAD%TS(:) - B%XT_WIN1(:) )
     CT%XLW_WALL_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)                * ZLW_W_TO_WIN(:)     * (ZTS_WL(:)          - B%XT_WIN1(:) )
     !
     IF (SIZE(PTSRAD_GD)>0) THEN
        CT%XLW_GARD_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)*LW%XLW_G_TO_WIN(:) * ( PTSRAD_GD - B%XT_WIN1(:) )
     ENDIF
     !
     IF (SIZE(PTS_HVEG)>0) THEN
        CT%XLW_HV_TO_WIND (:)=T%XWALL_O_HOR(:)*B%XGR(:)*LW%XLW_HV_TO_WIN(:) * ( PTS_HVEG(:) - B%XT_WIN1(:) )
     ENDIF
     !
  ENDIF
  !
  ! Longwave absorption by the snow on the roofs
  !
  DO JI = 1, SIZE(DMT%XABS_LW_SNOW_ROOF)
     IF (PDN_RF(JI).GT.0.0) THEN
        DMT%XABS_LW_SNOW_ROOF(JI) =  T%TSNOW_ROOF%EMIS(JI) * ( PLW_RAD(JI) - XSTEFAN * T%TSNOW_ROOF%TS(JI)**4 )
     ELSE
        DMT%XABS_LW_SNOW_ROOF(JI) =  XUNDEF
     ENDIF
  ENDDO
  !
  ! Longwave absorption by the snow on the roads
  !
  DO JI = 1, SIZE(DMT%XABS_LW_SNOW_ROAD)
     IF (PDN_RD(JI).GT.0.0) THEN
        DMT%XABS_LW_SNOW_ROAD(JI) = &
               LW%XLW_S_TO_NR(JI)   * ( ZT_SKY(JI)        - T%TSNOW_ROAD%TS(JI) ) &
             + LW%XLW_WA_TO_NR(JI)  * ( T%XT_WALL_A(JI,1) - T%TSNOW_ROAD%TS(JI) ) &
             + LW%XLW_WB_TO_NR(JI)  * ( T%XT_WALL_B(JI,1) - T%TSNOW_ROAD%TS(JI) ) &
             + LW%XLW_WIN_TO_NR(JI) * ( B%XT_WIN1(JI)     - T%TSNOW_ROAD%TS(JI) ) &
             + LW%XLW_HV_TO_NR(JI)  * ( PTS_HVEG(JI)      - T%TSNOW_ROAD%TS(JI) )
     ELSE
        DMT%XABS_LW_SNOW_ROAD(JI) =  XUNDEF
     ENDIF
  ENDDO
  !
  ! Longwave radiation absorbed by urban vegetation
  !
  IF (CT%LCHECK_TEB) THEN
     CT%XLW_SNOW_TO_HV(:)   = 0.
     CT%XLW_ROAD_TO_HV(:)   = 0.
     CT%XLW_WIND_TO_HV(:)   = 0.
     CT%XLW_WALA_TO_HV(:)   = 0.
     CT%XLW_WALB_TO_HV(:)   = 0.
     CT%XLW_GARD_TO_HV(:)   = 0.
     CT%XLW_WIND_TO_GARD(:) = 0.
     CT%XLW_WALA_TO_GARD(:) = 0.
     CT%XLW_WALB_TO_GARD(:) = 0.
     CT%XLW_HV_TO_GARD(:)   = 0.
  END IF
  !
  IF (TOP%LGARDEN) THEN
     !
     DMT%XREC_LW_GARDEN(:) =                                          &
             (LW%XLW_S_TO_G  (:) * (ZT_SKY(:)       - PTSRAD_GD(:))   &
            + LW%XLW_WA_TO_G (:) * (T%XT_WALL_A(:,1)- PTSRAD_GD(:))   &
            + LW%XLW_WB_TO_G (:) * (T%XT_WALL_B(:,1)- PTSRAD_GD(:))   &
            + LW%XLW_WIN_TO_G(:) * (B%XT_WIN1(:)    - PTSRAD_GD(:))   &
            + LW%XLW_HV_TO_G(:)  * (PTS_HVEG(:)     - PTSRAD_GD(:)) ) &
            / PEMIS_GD(:) + XSTEFAN * PTSRAD_GD(:)**4
     !
     DMT%XABS_LW_GARDEN(:) = PEMIS_GD(:) * DMT%XREC_LW_GARDEN(:) - &
                             XSTEFAN * PEMIS_GD(:) * PTSRAD_GD(:)**4 
     !
     IF (CT%LCHECK_TEB) THEN
        CT%XLW_WALA_TO_GARD(:)=T%XGARDEN(:)*LW%XLW_WA_TO_G(:) *(T%XT_WALL_A(:,1)-PTSRAD_GD(:))
        CT%XLW_WALB_TO_GARD(:)=T%XGARDEN(:)*LW%XLW_WB_TO_G(:) *(T%XT_WALL_B(:,1)-PTSRAD_GD(:))
        CT%XLW_WIND_TO_GARD(:)=T%XGARDEN(:)*LW%XLW_WIN_TO_G(:)*(B%XT_WIN1(:)    -PTSRAD_GD(:))
     ENDIF
     !
     IF (TOP%CURBTREE/='NONE') THEN
        !
        DMT%XABS_LW_HVEG(:) =  LW%XLW_S_TO_HV  (:) * (ZT_SKY(:)          - PTS_HVEG(:)) &
                             + LW%XLW_WA_TO_HV (:) * (T%XT_WALL_A(:,1)   - PTS_HVEG(:)) &
                             + LW%XLW_WB_TO_HV (:) * (T%XT_WALL_B(:,1)   - PTS_HVEG(:)) &
                             + LW%XLW_WIN_TO_HV(:) * (B%XT_WIN1(:)       - PTS_HVEG(:)) &
                 + LW%XLW_R_TO_HV  (:) * PDF_RD(:) * (T%XT_ROAD(:,1)     - PTS_HVEG(:)) &
                 + LW%XLW_NR_TO_HV (:) * PDN_RD(:) * (T%TSNOW_ROAD%TS(:) - PTS_HVEG(:)) &
                             + LW%XLW_G_TO_HV  (:) * (PTSRAD_GD(:)       - PTS_HVEG(:))
        !
        DMT%XREC_LW_HVEG(:) = DMT%XABS_LW_HVEG(:) / PEMIS_HVEG(:) + XSTEFAN * PTS_HVEG(:)**4
        !
        IF (CT%LCHECK_TEB) THEN
           CT%XLW_SNOW_TO_HV(:)=T%XFRAC_HVEG(:)*LW%XLW_NR_TO_HV(:) *(T%TSNOW_ROAD%TS(:) - PTS_HVEG(:)) * PDN_RD(:)
           CT%XLW_ROAD_TO_HV(:)=T%XFRAC_HVEG(:)*LW%XLW_R_TO_HV(:)  *(T%XT_ROAD(:,1)     - PTS_HVEG(:)) * PDF_RD(:)
           CT%XLW_WIND_TO_HV(:)=T%XFRAC_HVEG(:)*LW%XLW_WIN_TO_HV(:)*(B%XT_WIN1(:)       - PTS_HVEG(:))
           CT%XLW_WALA_TO_HV(:)=T%XFRAC_HVEG(:)*LW%XLW_WA_TO_HV(:) *(T%XT_WALL_A(:,1)   - PTS_HVEG(:))
           CT%XLW_WALB_TO_HV(:)=T%XFRAC_HVEG(:)*LW%XLW_WB_TO_HV(:) *(T%XT_WALL_B(:,1)   - PTS_HVEG(:))
           CT%XLW_GARD_TO_HV(:)=T%XFRAC_HVEG(:)*LW%XLW_G_TO_HV(:)  *(PTSRAD_GD(:)       - PTS_HVEG(:))

           CT%XLW_HV_TO_GARD(:)=T%XGARDEN(:)   *LW%XLW_HV_TO_G(:)  *(PTS_HVEG(:)        - PTSRAD_GD(:))
        ENDIF
        !
     ELSE
        DMT%XREC_LW_HVEG(:) = 0.
        DMT%XABS_LW_HVEG(:) = 0.
     ENDIF
     !
  ELSE
     DMT%XREC_LW_GARDEN(:) = 0.
     DMT%XABS_LW_GARDEN(:) = 0.
  ENDIF
  !
  IF (LHOOK) CALL DR_HOOK('EXPLICIT_LONGWAVE',1,ZHOOK_HANDLE)
  !
END SUBROUTINE EXPLICIT_LONGWAVE
