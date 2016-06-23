!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
     SUBROUTINE CH_DEP_ISBA        (PUSTAR, PHU, PPSN,           &
                      PVEG, PLAI, PSAND, PCLAY, PRESA,             &
                      PRS, PZ0,  PTA, PPA, PTRAD, PNO, PROCK,      &
                      HSV, PSOILRC_SO2, PSOILRC_O3, PDEP           )  
!###########################################################                      
!!
!!    PURPOSE
!!    -------
!!      
!!    Compute dry deposition velocity for chemical species on nature area    
!!
!!    AUTHOR
!!    ------
!!      P.Tulet      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      20/02/97 
!!      Modification  21/07/00  (Guenais/Tulet) add deposition on town  and
!!                                              vegetation class   
!!      Modification  18/01/01  (Solmon/Tulet) patch dry deposition
!!      Modification  18/07/03  (Tulet) surface externalization
!!      Modification  01/2004   (Tulet Masson) removes patch calculation
!!      Modification  03/2006   (Le Moigne) pb in where test with some
!!                            compilation options
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_PAR
USE MODD_DATA_COVER_PAR
USE MODD_CSTS
USE MODD_CH_ISBA,     ONLY : XRCCLAYSO2, XRCCLAYO3, XRCSANDSO2, XRCSANDO3, &
                             XRCSNOWSO2, XRCSNOWO3, XLANDREXT  
!
USE MODD_CH_SURF
USE MODD_SURF_PAR,   ONLY: XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:),     INTENT(IN)  :: PUSTAR       ! friction velocity
REAL, DIMENSION(:),     INTENT(IN)  :: PHU          ! soil humidity
REAL, DIMENSION(:),     INTENT(IN)  :: PPSN         ! fraction of the grid covered
                                                    ! by snow
REAL, DIMENSION(:),     INTENT(IN)  :: PRS          ! stomatal resistance
REAL, DIMENSION(:),     INTENT(IN)  :: PZ0          ! vegetation roughness length
REAL, DIMENSION(:),     INTENT(IN)  :: PVEG         ! vegetation fraction
REAL, DIMENSION(:),     INTENT(IN)  :: PLAI         ! Leaf area index
REAL, DIMENSION(:,:),   INTENT(IN)  :: PSAND        ! Sand fraction
REAL, DIMENSION(:,:),   INTENT(IN)  :: PCLAY        ! Clay fraction
REAL, DIMENSION(:),     INTENT(IN)  :: PRESA        ! aerodynamical resistance
REAL, DIMENSION(:),     INTENT(IN)  :: PTA          ! air temperature forcing (K)
REAL, DIMENSION(:),     INTENT(IN)  :: PPA          ! surface atmospheric pressure
REAL, DIMENSION(:),     INTENT(IN)  :: PTRAD        ! radiative temperature  (K)
REAL, DIMENSION(:),     INTENT(IN)  :: PSOILRC_SO2  ! bare soil resistance for SO2
REAL, DIMENSION(:),     INTENT(IN)  :: PSOILRC_O3   ! bare soil resistance for O3
REAL, DIMENSION(:,:),   INTENT(OUT) :: PDEP         ! deposition dry velocity (m/s)
REAL, DIMENSION(:),     INTENT(IN)  :: PNO, PROCK   ! fractions of bare soil, rock
 CHARACTER(LEN=6), DIMENSION(:), INTENT(IN)  :: HSV  ! name of chemical
                                                    ! species
!
!*       0.2   Declarations of local variables :
!
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZDIFFMOLVAL
! Molecular diffusivity
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSCMDT 
! Sc(:)hmidt number
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZNATRB 
! nature quasi-laminar  resistances
!
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZHENRYVALCOR
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSTOMRC 
! stomatal surface  resistance
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZMESORC 
! mesophyl  resistance
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZEXTRC   
!  leaf uptake external surface  resistance
!
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSOILRC 
! bare soil surface  resistance
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZNATRC
! nature surface resistances where vegetation is
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSNOWRC 
! snow surface  resistance
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZCLAYRC 
! clay surface  resistance
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSANDRC 
! sand surface  resistance
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZBARERC
! nature surface resistances for bare soils
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZROCKRC
! nature surface resistances for rocks
!
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZRES_VEGTYPE
REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZRES_SNOWTYPE
!
!  final nature resistance by vegtype
REAL, DIMENSION(SIZE(PTRAD,1))      :: ZTYPE1_SAND, ZTYPE1_CLAY, ZTYPE1_SNOW ! Type soil 1
REAL, DIMENSION(SIZE(PTRAD,1))      :: ZUSTAR
REAL, DIMENSION(SIZE(PTRAD,1))      :: ZDIFFMOLH2O
!  final nature resistance
REAL, DIMENSION(SIZE(PTRAD,1))      :: ZLANDEXT
! computed Rext from Wesely tabulations (89)
REAL, DIMENSION(SIZE(PTRAD,1))      :: ZINCRC 
! in-canopy transport  resistance
REAL, DIMENSION(SIZE(PTRAD,1))      :: ZCOEF1, ZCOEF2, ZCOEF3, ZCOEF4, ZCOEF5, ZINV1, ZINV2
REAL, DIMENSION(SIZE(PTRAD,1))      :: ZTCOR
!
REAL, DIMENSION(size(HSV,1)) :: ZVAR1, ZVAR2, ZFACT1
!
REAL :: ZTYPE2_SAND, ZTYPE2_CLAY, ZTYPE2_SNOW ! Type soil 2
!
INTEGER :: JSV, JI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!============================================================================
!
!            Primilary
!            ---------
!Default values
!--------------
!
IF (LHOOK) CALL DR_HOOK('CH_DEP_ISBA',0,ZHOOK_HANDLE)
!
! Default type soil
! TYPE1 = RCCLAY(SO2) = 1000
! TYPE2 = RCCLAY(O3) = 100
IF (XRCCLAYO3.NE.XUNDEF) THEN
  ZTYPE2_CLAY = XRCCLAYO3 
ELSE
  ZTYPE2_CLAY = 100.
ENDIF
!
! TYPE1 = RCSAND(SO2) = 1000
! TYPE2 = RCSAND(O3) = 200
IF (XRCSANDO3.NE.XUNDEF) THEN
  ZTYPE2_SAND = XRCSANDO3
ELSE
  ZTYPE2_SAND = 200.
ENDIF
!
! TYPE1 = RCSNOW(SO2)
! TYPE2 = RCSNOW(O3) 
IF (XRCSNOWO3 /=XUNDEF) THEN
  ZTYPE2_SNOW = XRCSNOWO3 
ELSE
  ZTYPE2_SNOW = 2000.
ENDIF
!
DO JI = 1, SIZE(PVEG)
  !
  IF (XRCCLAYSO2.NE.XUNDEF) THEN
    ZTYPE1_CLAY(JI) = XRCCLAYSO2 
  ELSE
    ZTYPE1_CLAY(JI) = 1000.
  ENDIF
  !
  IF (XRCSANDSO2.NE.XUNDEF) THEN
    ZTYPE1_SAND(JI) = XRCSANDSO2 
  ELSE
    ZTYPE1_SAND(JI) = 1000.
  ENDIF
  !
  IF (XRCSNOWSO2/=XUNDEF) THEN
    ZTYPE1_SNOW(JI) = XRCSNOWSO2
  ELSEIF (PTRAD(JI) > 275.) THEN
    ZTYPE1_SNOW(JI) = 540.
  ELSE
    ZTYPE1_SNOW(JI) = 70. * (275. - PTRAD(JI))
  ENDIF
  !
  !  
  ZUSTAR(JI) = MAX(PUSTAR(JI), 1E-9)
  !
  ZCOEF5(JI) = 1./(XKARMAN*ZUSTAR(JI))
  !
  !        3.2.5 In-canopy transport resistance
  !              ------------------------------
  !
  IF (PVEG(JI) > 0.) THEN
    ZINCRC(JI) = 14. * PLAI(JI) * 4. * PZ0(JI) / ZUSTAR(JI)
  ELSE
    ZINCRC(JI) = 1E-4
  ENDIF
  !
  !
  ! computed Rext from Wesely tabulations (89)
  !
  IF ( XLANDREXT.NE.XUNDEF ) THEN
    ! user value
    ZLANDEXT(JI) = XLANDREXT
  ELSEIF (PLAI(JI) /= XUNDEF) THEN
    ! computed value
    ZLANDEXT(JI) = 6000. -  4000. * TANH( 1.6 * (PLAI(JI) - 1.6) )
  ELSE
    ZLANDEXT(JI) = 9999.
  END IF
  !
  !
  ZCOEF1(JI) = 1./298. - 1./PTA(JI)
  !
  ZDIFFMOLH2O(JI)  = 2.22E-05 + 1.46E-07 * (PTA(JI) * (PPA(JI)/XP00)**(XRD/XCPD) - 273.)  
  ZCOEF2(JI) = PRS(JI) * ZDIFFMOLH2O(JI)
  !
  ZCOEF3(JI) = 1./ZLANDEXT(JI)
  !
  IF ( PTRAD(JI) < 271.) THEN
    ZCOEF4(JI) = 1000. * EXP(-PTRAD(JI) + 269.) 
  ELSE
    ZCOEF4(JI) = 0.
  ENDIF
  !
  ZTCOR(JI) = MIN(2.5E3, ZCOEF4(JI))
  !
  !
  ZINV1(JI) = 1.E-5/PSOILRC_SO2(JI)
  !
  ZINV2(JI) = 1./PSOILRC_O3(JI)
  !
ENDDO
!
!
DO JSV = 1, SIZE(HSV,1)
  !
  ZVAR1(JSV) = XSREALREACTVAL(JSV) / 3000.
  ZVAR2(JSV) = XSREALREACTVAL(JSV) * 100.
  !
  ZFACT1(JSV) = 1.46E-07 * SQRT(18. / XSREALMASSMOLVAL(JSV))
  !
ENDDO
!
!============================================================================
!
DO JSV = 1, SIZE(HSV,1)
  !
  DO JI = 1, SIZE(PTA)
    !
    !       2.0  Quasi-laminar resistance 
    !            ------------------------      
    !
    !         compute molecular diffusivity for each species (Langevin, 1905)
    !         ----------------------------------------------
    !
    ZDIFFMOLVAL(JI,JSV) = 2.22E-05 + (PTA(JI) - 273.0) * ZFACT1(JSV)
    !
    !         computation of Rb for each cover type
    !         -------------------------------------
    !
    ZSCMDT(JI,JSV) = 0.15E-4 / ZDIFFMOLVAL(JI,JSV)
    ZNATRB(JI,JSV) = ((ZSCMDT(JI,JSV)/0.72)**(2./3.)) * ZCOEF5(JI)
    !
    IF (PLAI(JI)/=XUNDEF) ZNATRB(JI,JSV) = 2. * ZNATRB(JI,JSV)
    !
  ENDDO
  !
ENDDO
!
DO JSV = 1, SIZE(HSV,1)
  !
  DO JI = 1, SIZE(PTA)
    !
    !============================================================================
    !
    !       3.0  Surface resistance on NATURE
    !            --------------------------------
    !
    !        3.0.1 Stomatal resistance
    !              -------------------
    !     
    !ZEXTRC_O3(:) = 1./(1./(3.*ZLANDEXT(:) + 1./3000.)) 
    !
    ZHENRYVALCOR(JI,JSV) = XSREALHENRYVAL(JSV,1) * EXP(XSREALHENRYVAL(JSV,2) * ZCOEF1(JI))
    !
    IF (PRS(JI)>0.) THEN
      ! 
      ZSTOMRC(JI,JSV) = ZCOEF2(JI) / ZDIFFMOLVAL(JI,JSV)   
      !
      !        3.2.2 Mesophyl resistance
      !              -------------------
      !
      ZMESORC(JI,JSV) = 1. / ( ZHENRYVALCOR(JI,JSV)/3000. + ZVAR2(JSV) )
      !
    ELSE
      !
      ZSTOMRC(JI,JSV) = 9999.
      !
      ZMESORC(JI,JSV) = 9999.
      !
    ENDIF
    !
    !        3.2.4 External leaf uptake resistance (Wesely, 1989)
    !              -------------------------------
    !
    IF (PHU(JI) >= 1.) THEN ! for dew-wetted surface
      !
      ! compute Rext for any species exept O3
      ! taking acount of (Walmsley, Wesely, 95, technical note, Atm Env vol 30)
      ZEXTRC(JI,JSV) = 1./( ZCOEF3(JI) + 1.0E-7*ZHENRYVALCOR(JI,JSV) + ZVAR1(JSV) )
      !
    ELSEIF ( PRS(JI) > 0. ) THEN
      !
      ZEXTRC(JI,JSV) = ZLANDEXT(JI) / ( 1.0E-5 * ZHENRYVALCOR(JI,JSV) + XSREALREACTVAL(JSV) )
      !
    ELSE
      !
      ZEXTRC (JI,JSV) = 9999. 
      !
    ENDIF
    !
    !    IF ((HSV(JSV)=='O3').OR.(HSV(JSV)=='O_3')) &
    !        ZEXTRC(:,JSV) =  ZEXTRC_O3(:)*ZTCOR(:) 
    !    IF ((HSV(JSV)=='SO2').OR.(HSV(JSV)=='SO_2')) &
    !        ZEXTRC(:,JSV) = ZTCOR(:)* 1./ (1/5000 +1./(3 *ZLANDEXT(:)))
    !
    !         Temperature correction
    !         ----------------------
    !
    ZEXTRC(JI,JSV) = ZEXTRC(JI,JSV) + ZCOEF4(JI)
    !
  ENDDO
  !
ENDDO
!
DO JSV = 1, SIZE(HSV,1)
  !
  DO JI = 1, SIZE(PTA)
    !    
    !        3.2.6 Surface  resistance on soil under veg
    !              ------------------------------------- 
    !
    ZSOILRC(JI,JSV) = 1. / ( ZHENRYVALCOR(JI,JSV)*ZINV1(JI) + XSREALREACTVAL(JSV)*ZINV2(JI) )  
    !
    IF ( ZSTOMRC(JI,JSV)>0. .AND. ZINCRC(JI)>0. .AND. ZEXTRC(JI,JSV)>0. ) THEN
      !
      !          3.2.7 Compute  surface resistance on vegetation
      !                -----------------------------------------
      !
      ZNATRC(JI,JSV) = 1./ &
        ( 1./(ZSTOMRC(JI,JSV)+ZMESORC(JI,JSV)) + 1./(ZINCRC(JI)+ZSOILRC(JI,JSV)) + 1./ZEXTRC(JI,JSV) )  
      !
    ELSE
      ZNATRC(JI,JSV) = 1.E-4 
    ENDIF
    !
    !       3.3  Surface  resistance on NATURE with NO VEG (bare soil, rock, snow) 
    !            -----------------------------------------------------------------
    !
    !          3.3.1 Surface  resistance on clay
    !                ---------------------------
    !
    ZCLAYRC(JI,JSV) = ( 1.E5 * ZTYPE1_CLAY(JI) * ZTYPE2_CLAY ) / &
        ( ZHENRYVALCOR(JI,JSV)*ZTYPE2_CLAY + ZTYPE1_CLAY(JI)*1.E5*XSREALREACTVAL(JSV) )  
    !
    !          3.3.2 Surface  resistance on sand
    !                ---------------------------
    !
    ZSANDRC(JI,JSV) = ( 1.E5 * ZTYPE1_SAND(JI) * ZTYPE2_SAND ) / &
        ( ZHENRYVALCOR(JI,JSV)*ZTYPE2_SAND + ZTYPE1_SAND(JI)*1.E5*XSREALREACTVAL(JSV) )  
    !
    !          3.3.3 Compute surface resistance on bare soil
    !                ---------------------------------------
    !
    ZBARERC(JI,JSV) = 1./ ( PSAND(JI,1)/ZSANDRC(JI,JSV) + (1.-PSAND(JI,1))/ZCLAYRC(JI,JSV) )  
    !
    !          3.3.4 Surface temperature correction 
    !                ------------------------------
    !
    ZBARERC(JI,JSV) = ZBARERC (JI,JSV) + ZTCOR(JI)
    !
    !          3.3.5 Compute surface resistance on ROCK AREA
    !                ---------------------------------------
    !
    ZROCKRC(JI,JSV) = ( 1.E5 * PSOILRC_SO2(JI) * PSOILRC_O3(JI) ) / &
           (ZHENRYVALCOR(JI,JSV)*PSOILRC_O3(JI) + PSOILRC_SO2(JI)*1.E5*XSREALREACTVAL(JSV) )
    !  
    !          3.3.6 Surface temperature correction 
    !                ------------------------------
    !
    ZROCKRC(JI,JSV) = ZROCKRC (JI,JSV) + ZTCOR(JI)
    !
    !       3.4  Surface resistance on snow
    !            ----------------------------------
    !
    !          3.4.1 Compute surface resistance on snow
    !                ----------------------------------
    !
    ZSNOWRC(JI,JSV) = ( 1.E5 * ZTYPE1_SNOW(JI) * ZTYPE2_SNOW ) / &
        ( ZHENRYVALCOR(JI,JSV)*ZTYPE2_SNOW + ZTYPE1_SNOW(JI)*1.E5*XSREALREACTVAL(JSV) )  
    !
    !          3.4.2 Surface temperature correction 
    !                ------------------------------
    !
    ZSNOWRC(JI,JSV) = ZSNOWRC (JI,JSV) + ZTCOR(JI)
    !
    !       3.5  Surface resistance on snow (eternal or explicit)
    !            --------------------------------------------
    !
    ! add rocks into bare soil resistance computation, when present
    IF ( PROCK(JI)>0. ) THEN 
      ZBARERC(JI,JSV) = ( PNO(JI)+PROCK(JI) )/( PNO(JI)/ZBARERC(JI,JSV) + PROCK(JI)/ZROCKRC(JI,JSV) )
    ENDIF
    !
    ! computes resistance due to soil and vegetation
    ZNATRC(JI,JSV) = 1./ ( PVEG(JI)/ZNATRC(JI,JSV) + (1.-PVEG(JI))/ZBARERC(JI,JSV) ) 
    ! 
  ENDDO
  !
ENDDO
!
DO JSV = 1, SIZE(HSV,1)
  !
  DO JI = 1, SIZE(PTA)
    !    
    !---------------------------------------------------------------------
    !
    !       4.0  Compute nature resistance 
    !            --------------------------
    !
    ZRES_VEGTYPE (JI,JSV) = PRESA(JI) + ZNATRB(JI,JSV) + ZNATRC(JI,JSV)
    ZRES_SNOWTYPE(JI,JSV) = PRESA(JI) + ZNATRB(JI,JSV) + ZSNOWRC(JI,JSV)
    !
    PDEP(JI,JSV) = ( 1-PPSN(JI) )/ZRES_VEGTYPE(JI,JSV) + PPSN(JI)/ZRES_SNOWTYPE(JI,JSV)  
    !
  ENDDO
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('CH_DEP_ISBA',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------
!
END SUBROUTINE CH_DEP_ISBA
