!     #########################
      SUBROUTINE ECOCLIMAP2_LAI
!     #########################
!
!!**** *ECOCLIMAP2_LAI* initializes cover-field correspondance arrays
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
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
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2008
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_DATA_COVER_n,   ONLY : XDATA_VEGTYPE, NYEAR
USE MODD_DATA_COVER,     ONLY : XDATA_LAI, XDATA_LAI_ALL_YEARS, LCLIM_LAI, &

                                  NECO2_START_YEAR, NECO2_END_YEAR  
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER

!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
INTEGER :: IYEAR           ! year index
INTEGER :: JCOVER,JVEGTYPE ! loop counters on covers and decades
INTEGER :: JYEAR           ! loop counter on years
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1. definition of LAI data
!           ----------------------
!
IF (LHOOK) CALL DR_HOOK('ECOCLIMAP2_LAI',0,ZHOOK_HANDLE)
XDATA_LAI(301:,:,:) = XUNDEF
!
!*       2. if averaged LAI
!           ---------------
!
IF (LCLIM_LAI .OR. NYEAR<NECO2_START_YEAR .OR. NYEAR>NECO2_END_YEAR) THEN
!
  DO JCOVER=301,JPCOVER
    DO JVEGTYPE=1,NVEGTYPE
      IF (XDATA_VEGTYPE(JCOVER,JVEGTYPE).ne.0.) THEN
        XDATA_LAI(JCOVER,:,JVEGTYPE) = 0.
        DO JYEAR=1,5
          XDATA_LAI(JCOVER,:,JVEGTYPE)=XDATA_LAI(JCOVER,:,JVEGTYPE) &
                                        +XDATA_LAI_ALL_YEARS(JCOVER,(JYEAR-1)*36+1:JYEAR*36,JVEGTYPE)/5.  
        END DO
      ENDIF
    END DO
  END DO

!
!*       3. if LAI of a specific year
!           -------------------------
ELSE
!
  IYEAR = NYEAR - NECO2_START_YEAR
  DO JCOVER=301,JPCOVER
    DO JVEGTYPE=1,NVEGTYPE
      IF (XDATA_VEGTYPE(JCOVER,JVEGTYPE).ne.0.) THEN
        XDATA_LAI(JCOVER,:,JVEGTYPE)=XDATA_LAI_ALL_YEARS(JCOVER,IYEAR*36+1:(IYEAR+1)*36,JVEGTYPE)
      ENDIF
    ENDDO
  ENDDO
!
END IF
IF (LHOOK) CALL DR_HOOK('ECOCLIMAP2_LAI',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ECOCLIMAP2_LAI
