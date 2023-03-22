!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_ISBA_NUDGING_n (DTCO, IO, S, NP, U, HPROGRAM)
!     ##################################
!
!!****  *READ_ISBA_NUDGING_n* - routine to initialise ISBA Snow and soil moisture nudging
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/2021
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n,   ONLY : DATA_COVER_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_NP_t, ISBA_P_t, ISBA_PE_t
USE MODD_SURF_ATM_n,     ONLY : SURF_ATM_t
!                                
USE MODD_SURF_PAR,       ONLY : XUNDEF, LEN_HREC
!
USE MODE_READ_SURF_LAYERS
!
USE MODI_READ_SURF
USE MODI_MAKE_CHOICE_ARRAY
USE MODI_PACK_SAME_RANK
!
USE MODI_READ_GR_SNOW
USE MODI_ABOR1_SFX
USE MODI_IO_BUFF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_IO_BUFF_CLEAN
USE MODI_GET_TYPE_DIM_n
USE MODE_RANDOM
USE MODE_EKF
USE MODE_THERMOS
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(ISBA_P_t), POINTER  :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
INTEGER, PARAMETER ::   INMONTH=3
!
CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=LEN_HREC) :: YWORK          ! Work variable
!
REAL, DIMENSION(:,:)  ,ALLOCATABLE :: ZWORK2D  ! 2D array to write data in file
!
CHARACTER(LEN=4) :: YLVL
CHARACTER(LEN=2) :: YDAY, YMTH
!
INTEGER :: ILU      ! 1D physical dimension
INTEGER :: IRESP    ! Error code after redding
INTEGER :: INDAYS   !  Number of days in the months
INTEGER :: IMONTH   ! Current month
!
INTEGER :: JP, JL, JDAY, JMTH   ! loop counter
!
INTEGER :: IVERSION ! surface version
!
LOGICAL :: GDIM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_ISBA_NUDGING_N',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!*       1.     Physical dimension
!               -----------------
!
YRECFM='SIZE_NATURE'
CALL GET_TYPE_DIM_n(DTCO, U, 'NATURE',ILU)
!
YRECFM='VERSION'
CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
GDIM = (IVERSION>=9)
IF (GDIM) CALL READ_SURF(HPROGRAM,'SPLIT_PATCH',GDIM,IRESP)
!
!Compute the number of days in the month (every february is 29 days long)
!
IMONTH=S%TTIME%TDATE%MONTH
SELECT CASE (IMONTH)
  CASE(4,6,9,11)
    INDAYS=30
  CASE(1,3,5,7:8,10,12)
    INDAYS=31
  CASE(2)
    INDAYS=29
END SELECT
!
!-------------------------------------------------------------------------------
ALLOCATE(ZWORK2D(ILU,IO%NPATCH))
!-------------------------------------------------------------------------------
!
!
!*       2.     Snow mass nudging:
!               -----------------
!
IF (IO%LNUDG_SWE) THEN
!  
   DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      ALLOCATE(PK%XNUDG_SWE(PK%NSIZE_P,INDAYS))
      PK%XNUDG_SWE(:,:) = XUNDEF
   ENDDO
!
!  The nudging is applied separately on each patch
!
   DO JDAY=1,INDAYS
      IF(IMONTH==2.AND.JDAY<29)THEN
        WRITE(YDAY,'(I2)')JDAY
        YRECFM='N_SWE_DD'//ADJUSTL(YDAY(:LEN_TRIM(YDAY)))
        CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, YRECFM, ZWORK2D)
        DO JP = 1,IO%NPATCH
           PK => NP%AL(JP)
           CALL PACK_SAME_RANK(PK%NR_P,ZWORK2D(:,JP),PK%XNUDG_SWE(:,JDAY))
        ENDDO
      ELSE
        DO JP = 1,IO%NPATCH
           PK => NP%AL(JP)
           PK%XNUDG_SWE(:,JDAY)=PK%XNUDG_SWE(:,JDAY-1) ! February 28 value for the 29th
        ENDDO
      ENDIF
   ENDDO
!
!  Make sure undefined values are set to XUNDEF
!
   DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      PK%XNUDG_SWE(:,:) = MAX(PK%XNUDG_SWE(:,:),0.0)
      WHERE (PK%XNUDG_SWE(:,:)>=1.E+15)
        PK%XNUDG_SWE(:,:) = XUNDEF
      ENDWHERE
   ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     Soil Moisture Nudging:
!               ---------------------
!
!The nudging is applied separately on each patch
!
IF (IO%CNUDG_WG=='DAY') THEN   
!
!  If the nudging values are daily
!
   DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      ALLOCATE(PK%XNUDG_WGTOT(PK%NSIZE_P,IO%NGROUND_LAYER,INDAYS))
      PK%XNUDG_WGTOT(:,:,:)=XUNDEF
   ENDDO
!
   DO JDAY=1,INDAYS
      IF(IMONTH==2.AND.JDAY<29)THEN
        WRITE(YDAY,'(I2)')JDAY
        YWORK='_DD'//ADJUSTL(YDAY(:LEN_TRIM(YDAY)))
        DO JL=1,IO%NGROUND_LAYER
           WRITE(YLVL,'(I4)') JL
           YRECFM='N_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
           YRECFM=ADJUSTR(YRECFM(:LEN_TRIM(YRECFM)))//ADJUSTL(YWORK(:LEN_TRIM(YWORK)))
           CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, YRECFM, ZWORK2D)
           DO JP = 1,IO%NPATCH
              PK => NP%AL(JP)
              CALL PACK_SAME_RANK(PK%NR_P,ZWORK2D(:,JP),PK%XNUDG_WGTOT(:,JL,JDAY))
           ENDDO
        ENDDO
      ELSE
        DO JP = 1,IO%NPATCH
           PK => NP%AL(JP)
           PK%XNUDG_WGTOT(:,:,JDAY)=PK%XNUDG_WGTOT(:,:,JDAY-1) ! February 28 value for the 29th
        ENDDO
      ENDIF
   ENDDO
!
ELSE
!
!  If the nudging values are montly
!
   DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      ALLOCATE(PK%XNUDG_WGTOT(PK%NSIZE_P,IO%NGROUND_LAYER,INMONTH))
      PK%XNUDG_WGTOT(:,:,:)=XUNDEF
   ENDDO
!
   DO JMTH=1,3
      WRITE(YMTH,'(I2)')JMTH-1
      YWORK='_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
      DO JL=1,IO%NGROUND_LAYER
         WRITE(YLVL,'(I4)') JL
         YRECFM='N_WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
         YRECFM=ADJUSTR(YRECFM(:LEN_TRIM(YRECFM)))//ADJUSTL(YWORK(:LEN_TRIM(YWORK)))
         CALL MAKE_CHOICE_ARRAY(HPROGRAM, IO%NPATCH, GDIM, YRECFM, ZWORK2D)
         DO JP = 1,IO%NPATCH
            PK => NP%AL(JP)
            CALL PACK_SAME_RANK(PK%NR_P,ZWORK2D(:,JP),PK%XNUDG_WGTOT(:,JL,JMTH))
         ENDDO
      ENDDO
   ENDDO
!
ENDIF
!
!Make sure undefined values are set to XUNDEF
!
IF (IO%CNUDG_WG/='DEF') THEN   
   DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      WHERE((PK%XNUDG_WGTOT(:,:,:)>1.00).OR.(PK%XNUDG_WGTOT(:,:,:)<0.00))
             PK%XNUDG_WGTOT(:,:,:)=XUNDEF
      ENDWHERE
   ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
DEALLOCATE(ZWORK2D)
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_ISBA_NUDGING_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_ISBA_NUDGING_n
