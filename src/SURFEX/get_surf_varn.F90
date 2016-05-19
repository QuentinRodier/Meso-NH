!     #########
      SUBROUTINE GET_SURF_VAR_n(HPROGRAM, KI, KS,                              &
                                  PSEA, PWATER, PNATURE, PTOWN,                &
                                  PT2M, PQ2M, PQS, PZ0, PZ0H, PZ0EFF,          &
                                  PZ0_SEA, PZ0_WATER, PZ0_NATURE, PZ0_TOWN,    &
                                  PZ0H_SEA, PZ0H_WATER, PZ0H_NATURE, PZ0H_TOWN,&
                                  PQS_SEA, PQS_WATER, PQS_NATURE, PQS_TOWN,    &
                                  PPSNG, PPSNV, PZS, PSERIES, PTWSNOW,         &
                                  PSSO_STDEV                     )  
!     #######################################################################
!
!!****  *GET_SURF_VAR_n* - gets some surface fields on atmospheric grid
!!
!!    PURPOSE
!!    -------
!!
!!    This program returns some surface variables neede by the atmosphere
!!
!!**  METHOD
!!    ------
!!
!!    Several functions are called in order to initialize surface variables
!!    needed by the atmospheric model. These functions fill the required arrays by
!!    the diagnosed values computed during the run. Since all arrays are optional,
!!    this program may be called with any of the arguments described above.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2006
!       S. Riette   06/2010 PSSO_STDEV and PTWSNOW added
!       B. Decharme 09/2012 Argument added in GET_FLUX_n
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODD_SURF_ATM_n,   ONLY : CWATER
USE MODI_GET_LUOUT
USE MODI_GET_FLUX_n
USE MODI_GET_FRAC_n
USE MODI_GET_Z0_n
USE MODI_GET_QS_n
USE MODI_GET_VAR_SEA_n
USE MODI_GET_VAR_WATER_n
USE MODI_GET_VAR_NATURE_n
USE MODI_GET_VAR_TOWN_n
USE MODI_GET_ZS_n
USE MODI_GET_SERIES_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_GET_SSO_STDEV_n
USE MODI_GET_1D_MASK
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),   INTENT(IN)            :: HPROGRAM    
INTEGER,            INTENT(IN)            :: KI         ! number of points
INTEGER,            INTENT(IN)            :: KS         ! number of points
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PSEA       ! sea fraction
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PWATER     ! water fraction
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PNATURE    ! nature fraction
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PTOWN      ! town fraction
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PT2M       ! Air temperature at 2 meters         (K)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQ2M       ! Air humidity at 2 meters            (kg/kg)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0        ! surface roughness length            (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H       ! surface roughness length for heat   (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0EFF     ! effective roughness length for heat (m)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0_SEA    ! surface roughness length over sea   (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0_WATER  ! surface roughness length over water (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0_NATURE ! surface roughness length over nature(m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0_TOWN   ! surface roughness length over town  (m)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H_SEA    ! surface roughness length for heat over sea   (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H_WATER  ! surface roughness length for heat over water (m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H_NATURE ! surface roughness length for heat over nature(m)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0H_TOWN   ! surface roughness length for heat over town  (m)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS_SEA    ! surface humidity over sea           (kg/kg)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS_WATER  ! surface humidity over water         (kg/kg)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS_NATURE ! surface humidity over nature        (kg/kg)
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PQS_TOWN   ! surface humidity over town          (kg/kg)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PPSNG      ! snow fraction over ground           (-)        
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PPSNV      ! snow fraction over vegetation       (-)
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZS        ! surface orography                   (m)    
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSERIES   ! any surface field for which 
!                                                        ! mesoNH series are required
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PTWSNOW    ! Snow total reservoir
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PSSO_STDEV ! S.S.O. standard deviation           (m)
!
!-------------------------------------------------------------------------------
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI)    :: ZFIELD1, ZFIELD2, ZFIELD3, ZFIELD4, ZFIELD5, ZFIELD6
REAL, DIMENSION(KI)    :: ZFIELD7
REAL, DIMENSION(KI,KS) :: ZSERIES
INTEGER, DIMENSION(KI) :: IMASK
!
INTEGER :: KI_SEA    ! dimension of sea tile
INTEGER :: KI_WATER  ! dimension of water tile
INTEGER :: KI_NATURE ! dimension of nature tile
INTEGER :: KI_TOWN   ! dimension of town tile
!
INTEGER                            :: JI           ! loop index over tiles
INTEGER                            :: ILUOUT       ! unit number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*   0. Logical unit for writing out
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_VAR_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*   1. Fraction of each tile
!
IF (PRESENT(PSEA) .OR. PRESENT(PWATER) .OR. PRESENT(PNATURE) .OR. PRESENT(PTOWN)) THEN
   !
   CALL GET_FRAC_n(HPROGRAM, KI, ZFIELD1, ZFIELD2, ZFIELD3, ZFIELD4)
   !
   IF (PRESENT(PSEA)   ) PSEA    = ZFIELD1
   IF (PRESENT(PWATER) ) PWATER  = ZFIELD2
   IF (PRESENT(PNATURE)) PNATURE = ZFIELD3
   IF (PRESENT(PTOWN)  ) PTOWN   = ZFIELD4
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*   2. Parameters at 2 meters
!
IF ( PRESENT(PT2M) .OR. PRESENT(PQ2M) ) THEN
   !
   CALL GET_FLUX_n(HPROGRAM, KI, ZFIELD1, ZFIELD1, ZFIELD1, ZFIELD1, ZFIELD1, ZFIELD2, &
                                 ZFIELD3, ZFIELD4, ZFIELD4, ZFIELD4, ZFIELD4, ZFIELD4  )
   !
   IF (PRESENT(PT2M)   ) PT2M    = ZFIELD2
   IF (PRESENT(PQ2M)   ) PQ2M    = ZFIELD3
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*   3. Roughness lengths
!
IF ( PRESENT(PZ0) .OR. PRESENT(PZ0H) ) THEN
   !
   CALL GET_Z0_n(HPROGRAM, KI, ZFIELD1, ZFIELD2)
   !
   IF (PRESENT(PZ0)    ) PZ0    = ZFIELD1
   IF (PRESENT(PZ0H)   ) PZ0H   = ZFIELD2
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*   3. Specific humidity
!
IF ( PRESENT(PQS) ) THEN
   !
   CALL GET_QS_n(HPROGRAM, KI, PQS)
   !
END IF
!
!-------------------------------------------------------------------------------
!
!*   4. Surface humidity for each tile (qs is not aggregated)
!
IF ( PRESENT(PQS_SEA) .OR. PRESENT(PZ0_SEA) .OR. PRESENT(PZ0H_SEA) ) THEN
   !
   ! Get parameters over sea tile
   !
   IF ( .NOT.PRESENT(PSEA) ) THEN
      !
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PSEA MISSING')
      !
   ENDIF
   !
   KI_SEA  = COUNT(PSEA    (:) > 0.0)
   !
   IMASK(:)=0
   CALL GET_1D_MASK(KI_SEA, KI, PSEA, IMASK(1:KI_SEA))
   !
   CALL GET_VAR_SEA_n(HPROGRAM, KI_SEA, ZFIELD1(1:KI_SEA), ZFIELD2(1:KI_SEA), ZFIELD3(1:KI_SEA))
   !
   IF(PRESENT(PQS_SEA))THEN
      PQS_SEA    (:) = XUNDEF
      DO JI = 1, KI_SEA
         PQS_SEA(IMASK(JI))  = ZFIELD1(JI)
      END DO
   ENDIF
   !   
   IF(PRESENT(PZ0_SEA))THEN
      PZ0_SEA    (:) = XUNDEF
      DO JI = 1, KI_SEA
         PZ0_SEA(IMASK(JI))  = ZFIELD2(JI)
      END DO
   ENDIF
   !
   IF(PRESENT(PZ0H_SEA))THEN
      PZ0H_SEA   (:) = XUNDEF
      DO JI = 1, KI_SEA
         PZ0H_SEA(IMASK(JI)) = ZFIELD3(JI)
      END DO
   ENDIF
   !
ENDIF
   !
   !-------------------------------------------------------------------------------
   !
IF ( PRESENT(PQS_WATER) .OR. PRESENT(PZ0_WATER) .OR. PRESENT(PZ0H_WATER) ) THEN
   !
   ! Get parameters over water tile
   !
   IF ( .NOT.PRESENT(PWATER) ) THEN
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PWATER MISSING')
   ENDIF
   !
   KI_WATER  = COUNT(PWATER  (:) > 0.0)
   !
   IMASK(:)=0
   CALL GET_1D_MASK(KI_WATER, KI, PWATER, IMASK(1:KI_WATER))
   !
   CALL GET_VAR_WATER_n(HPROGRAM, KI_WATER, CWATER, ZFIELD1(1:KI_WATER), &
                               ZFIELD2(1:KI_WATER), ZFIELD3(1:KI_WATER))
   !
   IF(PRESENT(PQS_WATER))THEN
      PQS_WATER    (:) = XUNDEF
      DO JI = 1, KI_WATER
         PQS_WATER(IMASK(JI))  = ZFIELD1(JI)
      END DO
   ENDIF
   !   
   IF(PRESENT(PZ0_WATER))THEN
      PZ0_WATER    (:) = XUNDEF
      DO JI = 1, KI_WATER
         PZ0_WATER(IMASK(JI))  = ZFIELD2(JI)
      END DO
   ENDIF
   !
   IF(PRESENT(PZ0H_WATER))THEN
      PZ0H_WATER   (:) = XUNDEF
      DO JI = 1, KI_WATER
         PZ0H_WATER(IMASK(JI)) = ZFIELD3(JI)
      END DO
   ENDIF
   !
ENDIF
   !
   !-------------------------------------------------------------------------------
   !
IF ( PRESENT(PQS_NATURE) .OR. PRESENT(PPSNG) .OR. PRESENT(PPSNV) .OR.  PRESENT(PZ0EFF).OR. &
     PRESENT(PTWSNOW) ) THEN
   !
   ! Get parameters over nature tile
   !
   !
   IF ( .NOT.PRESENT(PNATURE) ) THEN
      !
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PNATURE MISSING')
      !
   ENDIF
   !   
   KI_NATURE = COUNT(PNATURE (:) > 0.0)
   !
   IMASK(:)=0
   CALL GET_1D_MASK(KI_NATURE, KI, PNATURE, IMASK(1:KI_NATURE))
   !
   CALL GET_VAR_NATURE_n(HPROGRAM, KI_NATURE, ZFIELD1(1:KI_NATURE), ZFIELD2(1:KI_NATURE), &
                                              ZFIELD3(1:KI_NATURE), ZFIELD4(1:KI_NATURE), &
                        ZFIELD5(1:KI_NATURE), ZFIELD6(1:KI_NATURE), ZFIELD7(1:KI_NATURE))
   !
   IF(PRESENT(PQS_NATURE))THEN
     PQS_NATURE    (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PQS_NATURE(IMASK(JI))  = ZFIELD1(JI)
     END DO
   ENDIF
   !   
   IF(PRESENT(PZ0_NATURE))THEN
     PZ0_NATURE    (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PZ0_NATURE(IMASK(JI))  = ZFIELD5(JI)
     END DO
   ENDIF
   !
   IF(PRESENT(PZ0H_NATURE))THEN
     PZ0H_NATURE   (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PZ0H_NATURE(IMASK(JI)) = ZFIELD6(JI)
     END DO
   ENDIF
   !  
   IF (PRESENT(PPSNG)) THEN
     PPSNG      (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PPSNG     (IMASK(JI)) = ZFIELD2(JI)
     END DO
   ENDIF
   !
   IF (PRESENT(PPSNV)) THEN
     PPSNV      (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PPSNV     (IMASK(JI)) = ZFIELD3(JI)
     END DO
   ENDIF
   !
   IF ( PRESENT(PZ0EFF) ) THEN
     PZ0EFF     (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PZ0EFF    (IMASK(JI)) = ZFIELD4(JI)
     END DO
   ENDIF
   !
   IF(PRESENT(PTWSNOW)) THEN
     PTWSNOW    (:) = XUNDEF
     DO JI = 1, KI_NATURE
       PTWSNOW   (IMASK(JI)) = ZFIELD7(JI)
     ENDDO
   ENDIF
   !
ENDIF
   !
   !-------------------------------------------------------------------------------
   !
IF ( PRESENT(PQS_TOWN) .OR. PRESENT(PZ0_TOWN) .OR. PRESENT(PZ0H_TOWN) ) THEN
   !
   ! Get parameters over town tile
   !
   IF ( .NOT.PRESENT(PTOWN) ) THEN
      !
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PTOWN MISSING')
      !
   ENDIF
   !
   KI_TOWN   = COUNT(PTOWN   (:) > 0.0)
   !
   IMASK(:)=0
   CALL GET_1D_MASK(KI_TOWN, KI, PTOWN, IMASK(1:KI_TOWN))
   !
   CALL GET_VAR_TOWN_n(HPROGRAM, KI_TOWN, ZFIELD1(1:KI_TOWN), ZFIELD2(1:KI_TOWN), ZFIELD3(1:KI_TOWN))
   !
   IF(PRESENT(PQS_TOWN))THEN
      PQS_TOWN    (:) = XUNDEF
      DO JI = 1, KI_TOWN
         PQS_TOWN(IMASK(JI))  = ZFIELD1(JI)
      END DO
   ENDIF
   !   
   IF(PRESENT(PZ0_TOWN))THEN
      PZ0_TOWN    (:) = XUNDEF
      DO JI = 1, KI_TOWN
         PZ0_TOWN(IMASK(JI))  = ZFIELD2(JI)
      END DO
   ENDIF
   !
   IF(PRESENT(PZ0H_TOWN))THEN
      PZ0H_TOWN   (:) = XUNDEF
      DO JI = 1, KI_TOWN
         PZ0H_TOWN(IMASK(JI)) = ZFIELD3(JI)
      END DO
   ENDIF
   !
END IF
!
!*   5. Orography
!
IF (PRESENT(PZS)) THEN
   !
   CALL GET_ZS_n(HPROGRAM, KI, ZFIELD1)
   !
   PZS = ZFIELD1 
   !
END IF
!
!*   6. Series
!
IF (PRESENT(PSERIES)) THEN
   !
   IF ( .NOT.PRESENT(PWATER) ) THEN
      CALL ABOR1_SFX('GET_SURF_VARN: ARGUMENT PWATER REQUIRED FOR WATER SERIES')
   ENDIF        
   !
   IF ( COUNT(PWATER  (:) > 0.0) > 0.0 ) THEN
     !   
     CALL GET_SERIES_n(HPROGRAM, KI, KS, ZSERIES)
     !
     PSERIES = ZSERIES
     !
   ELSE
     PSERIES = XUNDEF
   ENDIF
   !
END IF
!
!*   6. SSO STDEV
!
IF (PRESENT(PSSO_STDEV)) THEN
   !
   CALL GET_SSO_STDEV_n('ASCII ', KI, ZFIELD1)
   !
   PSSO_STDEV = ZFIELD1
   !
END IF
!
IF (LHOOK) CALL DR_HOOK('GET_SURF_VAR_N',1,ZHOOK_HANDLE)
!
!
!==============================================================================
!
END SUBROUTINE GET_SURF_VAR_n
