!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_TEB_n (DTCO, DGU, U, TM, GDM, GRM, &
                              HPROGRAM,HWRITE)
!     ####################################
!
!!****  *WRITE_TEB_n* - routine to write surface variables in their respective files
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
!!      B. Decharme 07/2011 : Suppress pgd output
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITESURF_TEB_n
USE MODI_WRITESURF_TEB_CONF_n
USE MODI_END_IO_SURF_n
USE MODI_WRITESURF_TEB_CANOPY_n
USE MODI_GOTO_WRAPPER_TEB_PATCH
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JPATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_TEB_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'TOWN  ','TEB   ','WRITE')
!
!*       1.     Selection of surface scheme
!               ---------------------------
!
 CALL WRITESURF_TEB_CONF_n(TM%CHT, TM%DGMTO, TM%DGT, TM%DGUT, TM%T, TM%TOP, &
                           HPROGRAM)
!
DO JPATCH=1,TM%TOP%NTEB_PATCH
  CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                              GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, JPATCH)
  CALL WRITESURF_TEB_n(DGU, U, TM, GDM, GRM, &
                       HPROGRAM,JPATCH,HWRITE)
END DO
!     
 CALL GOTO_WRAPPER_TEB_PATCH(TM%B, TM%DGCT, TM%DGMT, TM%T, &
                              GDM%TGD, GDM%TGDPE, GRM%TGR, GRM%TGRPE, 1)
IF ((.NOT.LNOWRITE_CANOPY).OR.DGU%LSELECT) CALL WRITESURF_TEB_CANOPY_n(DGU, U, &
                                                                       TM%TCP, TM%TOP, &
                                                                       HPROGRAM,HWRITE)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_TEB_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_TEB_n
