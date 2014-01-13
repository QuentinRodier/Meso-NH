!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
     SUBROUTINE CH_DEP_ISBA        (PUSTAR, PHU, PPSN,           &
                      PVEG, PLAI, PSAND, PCLAY, PRESA,             &
                      PRS, PZ0,  PTA, PPA, PTRAD, PNO, PROCK,      &
                      HSV, PSOILRC_SO2, PSOILRC_O3, PDEP           )  
!###########################################################
  !
  !!                   
  !!                       
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
  !!	Modification  21/07/00  (Guenais/Tulet) add deposition on town  and
  !!                                              vegetation class   
  !!	Modification  18/01/01  (Solmon/Tulet) patch dry deposition
  !!	Modification  18/07/03  (Tulet) surface externalization
  !!	Modification  01/2004   (Tulet Masson) removes patch calculation
  !!	Modification  03/2006   (Le Moigne) pb in where test with some
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
  !
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
  !
  !
  !*       0.2   Declarations of local variables :
  !
  REAL             , DIMENSION(SIZE(PTRAD,1))             :: ZLANDEXT
  ! computed Rext from Wesely tabulations (89)
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSCMDT 
  ! Sc(:)hmidt number
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZDIFFMOLVAL
  ! Molecular diffusivity
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZNATRB 
  ! nature quasi-laminar  resistances

  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZMESORC 
  ! mesophyl  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSTOMRC 
  ! stomatal surface  resistance	
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZEXTRC   
  !  leaf uptake external surface  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSOILRC 
  ! bare soil surface  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1)) :: ZINCRC 
  ! in-canopy transport  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSNOWRC 
  ! snow surface  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZCLAYRC 
  ! clay surface  resistance
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZSANDRC 
  ! sand surface  resistance

  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZNATRC
  ! nature surface resistances where vegetation is
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZBARERC
  ! nature surface resistances for bare soils
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZROCKRC
  ! nature surface resistances for rocks

  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZRES_VEGTYPE
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZRES_SNOWTYPE
  !  final nature resistance by vegtype
  REAL             , DIMENSION(SIZE(PTRAD,1),size(HSV,1)) :: ZHENRYVALCOR
  REAL             , DIMENSION(SIZE(PTRAD,1)) :: ZDIFFMOLH2O
  !  final nature resistance
  REAL, DIMENSION(SIZE(PTRAD,1))      :: ZTCOR
  REAL, DIMENSION(SIZE(PTRAD,1))      :: ZTYPE1 ! Type soil 1
  REAL                                :: ZTYPE2 ! Type soil 2
  REAL             , DIMENSION(size(HSV,1)) :: ZVAR1, ZVAR2

  REAL,DIMENSION(SIZE(PUSTAR,1))      ::ZUSTAR
  INTEGER :: JSV, JI
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !============================================================================
  !
  !            Primilary
  !            ---------
  !Default values
  !--------------
  IF (LHOOK) CALL DR_HOOK('CH_DEP_ISBA',0,ZHOOK_HANDLE)
  ZEXTRC(:,:) = 9999. 
  ZINCRC(:) = 1E-4
  ZSOILRC(:,:) = 9999.
  ZNATRC (:,:) = 9999.
  ZBARERC(:,:) = 9999.
  ZSNOWRC(:,:) = 9999.
  ZRES_VEGTYPE(:,:)= 9999.
  ZRES_SNOWTYPE(:,:)= 9999.
  ZNATRB(:,:) = 9999.
  ZUSTAR(:) = MAX(PUSTAR(:), 1E-9)
  !
  DO JI=1,size(HSV,1)
      ZHENRYVALCOR(:,JI) = XSREALHENRYVAL(JI,1) * &
                          EXP(XSREALHENRYVAL(JI,2)* (1./298. - 1./PTA(:)))  
  ENDDO
  !
  ! computed Rext from Wesely tabulations (89)
  !
  IF (XLANDREXT .NE. XUNDEF) THEN
    ! user value
    ZLANDEXT(:) = XLANDREXT
  ELSE
    ! computed value
    WHERE(PLAI(:) /=XUNDEF)
      ZLANDEXT(:) = 6000. -  4000. * TANH(1.6*(PLAI(:) - 1.6))
    ELSEWHERE
      ZLANDEXT(:) = 9999.
    END WHERE
  END IF
  !
  !============================================================================
  !
  !       2.0  Quasi-laminar resistance 
  !            ------------------------      
  !
  !  
  !         compute molecular diffusivity for each species (Langevin, 1905)
  !         ----------------------------------------------
  DO JSV=1,size(HSV,1)
    ZDIFFMOLVAL(:,JSV) = 2.22E-05 + 1.46E-07 * (PTA(:) - 273.0) * &
                                 SQRT(18. / XSREALMASSMOLVAL(JSV))  
    ZSCMDT(:,JSV)=0.15E-4 / ZDIFFMOLVAL(:,JSV)
    ZDIFFMOLH2O(:)  = 2.22E-05 + 1.46E-07 * (PTA(:) * &
     (PPA(:)/XP00)**(XRD/XCPD) -273.)  
  ENDDO
  !
  !         computation of Rb for each cover type
  !         -------------------------------------
  DO JSV=1,size(HSV,1)
    WHERE (PLAI(:) /= XUNDEF) 
      ZNATRB(:,JSV) = 2.* ((ZSCMDT(:,JSV)/0.72)**(2./3.)) &
                  / (XKARMAN*ZUSTAR(:))     
    ELSEWHERE
      ZNATRB(:,JSV) = ((ZSCMDT(:,JSV)/0.72)**(2./3.)) &
                  / (XKARMAN*ZUSTAR(:))  
    END WHERE
  ENDDO
  ! 
  !============================================================================
  !
  !       3.0  Surface resistance on NATURE
  !            --------------------------------
  !
  !        3.0.1 Stomatal resistance
  !              -------------------
  !
  ZNATRC(:,:) = 1.E-4  
          
  !ZEXTRC_O3(:) = 1./(1./(3.*ZLANDEXT(:) + 1./3000.)) 
  DO JSV=1,size(HSV,1)
    ZSTOMRC(:,JSV) = 9999.
    ZVAR1(JSV) = XSREALREACTVAL(JSV)/3000.
    ZVAR2(JSV) = XSREALREACTVAL(JSV)*100.
    WHERE(PRS(:) > 0.) 
      ZSTOMRC(:,JSV) = PRS(:)*ZDIFFMOLH2O(:)&
                       / ZDIFFMOLVAL(:,JSV)   
      !
      !        3.2.2 Mesophyl resistance
      !              -------------------
      !
      ZMESORC(:,JSV) = 1. / (ZHENRYVALCOR(:,JSV)/3000. + ZVAR2(JSV))
      !
      !
      !        3.2.4 External leaf uptake resistance (Wesely, 1989)
      !              -------------------------------
      !
      !
      ZEXTRC(:,JSV) = ZLANDEXT(:) / &
           (1E-5 * ZHENRYVALCOR(:,JSV) + XSREALREACTVAL(JSV))  
    END WHERE

     WHERE(PHU(:) >= 1.) ! for dew-wetted surface
        ! compute Rext for any species exept O3
        ! taking acount of (Walmsley, Wesely, 95, technical note, Atm Env vol 30)

        ZEXTRC(:,JSV) = 1./( 1. / ZLANDEXT(:) + 1.E-7 * ZHENRYVALCOR(:,JSV) + ZVAR1(JSV))
     ENDWHERE

!    IF ((HSV(JSV)=='O3').OR.(HSV(JSV)=='O_3')) &
!        ZEXTRC(:,JSV) =  ZEXTRC_O3(:)*ZTCOR(:) 
!    IF ((HSV(JSV)=='SO2').OR.(HSV(JSV)=='SO_2')) &
!        ZEXTRC(:,JSV) = ZTCOR(:)* 1./ (1/5000 +1./(3 *ZLANDEXT(:)))
    !
    !
    !         Temperature correction
    !         ----------------------
    WHERE(PTRAD(:) < 271.)
      ZEXTRC(:,JSV) = ZEXTRC(:,JSV) +&
            1000 * EXP(-PTRAD(:) + 269.)  
    END WHERE
  ENDDO
  !
  !        3.2.5 In-canopy transport resistance
  !              ------------------------------
  !
  WHERE (PVEG(:) > 0.)
    ZINCRC(:) = 14. * PLAI(:) * 4. * PZ0(:) / ZUSTAR(:)
  END WHERE
  !
  !        3.2.6 Surface  resistance on soil under veg
  !              -------------------------------------
  ! 
  DO JSV=1,size(HSV,1)
    ZSOILRC(:,JSV) = 1. /  &
        (ZHENRYVALCOR(:,JSV) / (1E5 * PSOILRC_SO2) + &
        XSREALREACTVAL(JSV) / PSOILRC_O3)  
    !
    WHERE((ZSTOMRC(:,JSV) > 0.).AND.(ZINCRC(:) > 0.).AND. (ZEXTRC(:,JSV) > 0.))

      !          3.2.7 Compute  surface resistance on vegetation
      !                -----------------------------------------
      !
      ZNATRC(:,JSV) =1./ ( 1. /( ZSTOMRC(:,JSV) &
           + ZMESORC(:,JSV)) + 1. /( ZINCRC(:) + ZSOILRC(:,JSV)) + &
                               1. /( ZEXTRC(:,JSV)))  
    END WHERE
  ENDDO
!
!       3.3  Surface  resistance on NATURE with NO VEG (bare soil, rock, snow) 
!            -----------------------------------------------------------------
!
!          3.3.1 Surface  resistance on clay
!                ---------------------------
! Default type soil
! TYPE1 = RCCLAY(SO2) = 1000
! TYPE2 = RCCLAY(O3) = 100
! 
ZTYPE1(:) = 1000.
ZTYPE2 = 100.
!
!
IF (XRCCLAYSO2.NE.XUNDEF) ZTYPE1(:) = XRCCLAYSO2 
IF (XRCCLAYO3.NE.XUNDEF) ZTYPE2 = XRCCLAYO3 
!
!  
DO JSV=1,size(HSV,1)
  ZCLAYRC(:,JSV) = (1.E5*ZTYPE1(:)*ZTYPE2)/&
       ((ZHENRYVALCOR(:,JSV)*ZTYPE2) +&
       (ZTYPE1(:) * 1.E5 * XSREALREACTVAL(JSV)))  
ENDDO
!
!          3.3.2 Surface  resistance on sand
!                ---------------------------
! Default type soil
! TYPE1 = RCSAND(SO2) = 1000
! TYPE2 = RCSAND(O3) = 200
! 
ZTYPE1(:) = 1000.
ZTYPE2 = 200.
!
!
IF (XRCSANDSO2.NE.XUNDEF) ZTYPE1(:) = XRCSANDSO2 
IF (XRCSANDO3.NE.XUNDEF) ZTYPE2 = XRCSANDO3
!
!  
DO JSV=1,size(HSV,1)
  ZSANDRC(:,JSV) = (1.E5*ZTYPE1(:)*ZTYPE2)/  &
                     (ZHENRYVALCOR(:,JSV)&
                    * ZTYPE2 + ZTYPE1(:) * 1.E5 * XSREALREACTVAL(JSV))  
ENDDO
!
!          3.3.3 Compute surface resistance on bare soil
!                ---------------------------------------
!
DO JSV=1,size(HSV,1)
  ZBARERC(:,JSV) =      1./ (     PSAND(:,1)            /ZSANDRC(:,JSV)  +  &
                                    PCLAY(:,1)            / ZCLAYRC(:,JSV) +  &
                                (1.-PCLAY(:,1)-PSAND(:,1))/ZCLAYRC(:,JSV) )  
ENDDO
!
!          3.3.4 Surface temperature correction 
!                ------------------------------
ZTCOR(:) = 0.
WHERE ( PTRAD(:) < 271.)
  ZTCOR(:) = 1000 * EXP(-PTRAD(:) + 269.)
  ZTCOR(:) = MIN(2.5E3, ZTCOR(:))
END WHERE
DO JSV=1,size(HSV,1)
  ZBARERC(:,JSV) = ZBARERC (:,JSV) + ZTCOR(:)
ENDDO
!
!
!          3.3.5 Compute surface resistance on ROCK AREA
!                ---------------------------------------
!
DO JSV=1,size(HSV,1)
  ZROCKRC(:,JSV) = (1.E5 * PSOILRC_SO2 * PSOILRC_O3(:)) /    &
                     (ZHENRYVALCOR(:,JSV) * PSOILRC_O3(:)     +&
                     PSOILRC_SO2 * 1.E5 * XSREALREACTVAL(JSV))  
ENDDO
!
!          3.3.6 Surface temperature correction 
!                ------------------------------
ZTCOR(:) = 0.
WHERE (PTRAD(:) < 271.)
  ZTCOR(:) = 1000. * EXP(-PTRAD(:) + 269.)
  ZTCOR(:) = MIN(2.5E3, ZTCOR(:))
END WHERE
DO JSV=1,size(HSV,1)
  ZROCKRC(:,JSV) = ZROCKRC (:,JSV) + ZTCOR(:)
ENDDO
! 
!       3.4  Surface resistance on snow
!            ----------------------------------
!
!
!          3.4.1 Compute surface resistance on snow
!                ----------------------------------
!
! Default type soil
! TYPE1 = RCSNOW(SO2)
! TYPE2 = RCSNOW(O3) 
!
WHERE(PTRAD(:) > 275.)
  ZTYPE1(:) = 540.
ELSEWHERE
  ZTYPE1(:) = 70. * (275. - PTRAD(:))
END WHERE
! 
ZTYPE2 = 2000.
!
!
IF (XRCSNOWSO2/=XUNDEF) ZTYPE1(:) = XRCSNOWSO2
IF (XRCSNOWO3 /=XUNDEF) ZTYPE2 = XRCSNOWO3 
!
DO JSV=1,size(HSV,1)
  ZSNOWRC(:,JSV) = (1.E5*ZTYPE1(:)*ZTYPE2)/(ZHENRYVALCOR(:,JSV)&
                  * ZTYPE2 + ZTYPE1(:) * 1.E5 * XSREALREACTVAL(JSV))  
ENDDO
!
!          3.4.2 Surface temperature correction 
!                ------------------------------
!
ZTCOR(:) = 0.
WHERE (PTRAD(:) < 271.)
  ZTCOR(:) = 1000. * EXP(-PTRAD(:)+269.)
  ZTCOR(:) = MIN(2.5E3, ZTCOR(:))
END WHERE
DO JSV=1,size(HSV,1)
  ZSNOWRC(:,JSV) = ZSNOWRC (:,JSV) + ZTCOR(:)
ENDDO
! 
!       3.5  Surface resistance on snow (eternal or explicit)
!            --------------------------------------------
!
DO JSV=1,size(HSV,1)
  ! add rocks into bare soil resistance computation, when present
  WHERE (PROCK(:)>0.) 
    ZBARERC(:,JSV) = (PNO(:)+PROCK(:)) / (PNO(:)/ZBARERC(:,JSV) + PROCK(:)/ZROCKRC(:,JSV))
  END WHERE
  ! computes resistance due to soil and vegetation
  ZNATRC(:,JSV) = 1./ (      PVEG(:)  / ZNATRC (:,JSV)   &
                          +(1.-PVEG(:)) / ZBARERC(:,JSV)  )  
ENDDO
!
!---------------------------------------------------------------------
!
!       4.0  Compute nature resistance 
!            --------------------------
!
PDEP(:,:) = 0.
DO JSV=1,size(HSV,1)
  ZRES_VEGTYPE(:,JSV)  =PRESA(:)+  ZNATRB(:,JSV) + ZNATRC(:,JSV)
  ZRES_SNOWTYPE(:,JSV) =PRESA(:)+  ZNATRB(:,JSV) + ZSNOWRC(:,JSV)
  PDEP(:,JSV)= (1-PPSN(:)) / ZRES_VEGTYPE(:,JSV)  + &
                    PPSN(:) /  ZRES_SNOWTYPE(:,JSV)  
ENDDO
IF (LHOOK) CALL DR_HOOK('CH_DEP_ISBA',1,ZHOOK_HANDLE)
!							       	       
!---------------------------------------------------------------------
!
END SUBROUTINE CH_DEP_ISBA
