!     #########
    SUBROUTINE HEATCAPZ(PSANDZ,PWSATZ,PHCAPSOIL)
!   ###############################################################
!!****  *HEATCAPZ*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates soil thermal conductivity components
!     using sand fraction and model constants in
!     order to calculate the thermal conductivity
!     following the method of Johansen (1975) as recommended
!     by Farouki (1986) parameterized for SVAT schemes
!     following Peters-Lidard et al. 1998 (JAS). This is
!     used in explicit calculation of CG (soil thermal
!     inertia): it is an option. DEFAULT is method of
!     Noilhan and Planton (1989) (see SOIL.F90).
!              
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
!!    Peters-Lidard et al. 1998 (JAS)
!!      
!!    AUTHOR
!!    ------
!!
!!	A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!                  18/02/00    2D for veritcal profiles
!!                  2008/03     P. LeMoigne, ###it thrmconz subroutine
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_ISBA_PAR,   ONLY : XDRYWGHT, XSPHSOIL
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:), INTENT(IN) :: PSANDZ     ! soil sand fraction (-)
REAL,   DIMENSION(:,:), INTENT(IN) :: PWSATZ     ! soil porosity (m3 m-3)
!
REAL,   DIMENSION(:,:), INTENT(OUT):: PHCAPSOIL ! soil heat capacity (J K-1 m-3) 
!
!*      0.2    declarations of local variables
!
REAL,    DIMENSION(SIZE(PSANDZ,1),SIZE(PSANDZ,2)) :: ZGAMMAD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HEATCAPZ',0,ZHOOK_HANDLE)
ZGAMMAD(:,:)   = XUNDEF
PHCAPSOIL(:,:) = XUNDEF
!
!
! Quartz content estimated from sand fraction:
!
WHERE(PSANDZ(:,:)/=XUNDEF)
!
! Note, ZGAMMAD (soil dry density) can be supplied from obs, but
! for mesoscale modeling, we use the following approximation
! from Peters-Lidard et al. 1998:
!
   ZGAMMAD(:,:)   = (1.0-PWSATZ(:,:))*XDRYWGHT
   PHCAPSOIL(:,:) = XSPHSOIL*ZGAMMAD(:,:)
!
END WHERE
IF (LHOOK) CALL DR_HOOK('HEATCAPZ',1,ZHOOK_HANDLE)
!
END SUBROUTINE HEATCAPZ
