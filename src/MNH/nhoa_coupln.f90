!MNH_LIC Copyright 2021-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###################
      MODULE MODI_NHOA_COUPL_n
!     ###################
!
INTERFACE 
!
      SUBROUTINE NHOA_COUPL_n(KDAD,PTSTEP,KMI,KTCOUNT,KKU)
!
INTEGER,          INTENT(IN)    :: KDAD     !  Number of the DAD model
REAL,             INTENT(IN)    :: PTSTEP   !  Time step
INTEGER,          INTENT(IN)    :: KMI      ! model number
INTEGER,          INTENT(IN)    :: KKU
INTEGER,          INTENT(IN)    :: KTCOUNT  !  Temporal loop COUNTer
                                            ! (=1 at the segment beginning)
!
END SUBROUTINE NHOA_COUPL_n
END INTERFACE
END MODULE MODI_NHOA_COUPL_n
!
!     ####################################################################
SUBROUTINE NHOA_COUPL_n(KDAD,PTSTEP,KMI,KTCOUNT,KKU)
!     ####################################################################
!!
!!    PURPOSE
!!    -------
!   To compute the flux at the O-A interface in the auto-coupling O-A LES case
!
!!**  METHOD
!!    ------
!!      
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS: JPHEXT,JPVEXT
!!      Module MODD_FIELD$n : XUT,XVT,XWT,XRT,XTHT,XPABST
!!      Module MODD_REF$n   : XRHODJ, XRVREF,XTHVREF, XRHODREF
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!   JL Redelsperger 03/2021   Version 0 
!!    MODIFICATIONS
!!    -------------
!!-----------------------------------------------------------------------------
!
!*      0.   DECLARATIONS
!            ------------
USE MODE_ll
USE MODE_MODELN_HANDLER
!
USE MODD_PARAMETERS
USE MODD_NESTING
USE MODD_CST
USE MODD_REF_n        ! modules relative to the outer model $n
USE MODD_FIELD_n
USE MODD_CONF
USE MODD_PARAM_n
USE MODD_TURB_n
USE MODD_DYN_n, ONLY : LOCEAN
USE MODD_REF, ONLY: LCOUPLES
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
INTEGER,          INTENT(IN)    :: KDAD     !  Number of the DAD model
REAL,             INTENT(IN)    :: PTSTEP   !  Time step
INTEGER,          INTENT(IN)    :: KMI      ! model number
INTEGER,          INTENT(IN)    :: KKU      ! 
!
!
!*       0.2   declarations of local variables
!
INTEGER                :: IIB,IIE,IJB,IJE,IIU,IJU
INTEGER :: IKE
INTEGER                :: ILBX,ILBY,ILBX2,ILBY2
INTEGER,          INTENT(IN)    :: KTCOUNT  !  Temporal loop COUNTer
                                            ! (=1 at the segment beginning)
!
INTEGER           :: JRR,JSV          !  Loop index
!
INTEGER :: IINFO_ll, IDIMX, IDIMY
! surface variables: wind, current, Temp
REAL, DIMENSION(:,:), ALLOCATABLE :: ZCOUPUA,ZCOUPVA,ZCOUPTA
REAL, DIMENSION(:,:), ALLOCATABLE :: ZCOUPUO,ZCOUPVO,ZCOUPTO
!surf flux  local work space
REAL, DIMENSION(:,:), ALLOCATABLE :: ZCOUPTFL,ZCOUPUFL,ZCOUPVFL
CHARACTER(LEN=4)                    :: ZINIT_TYPE
!
!---Coupled OA MesoNH----------------------------------------------------------------------------
!*      0.   INITIALISATION
!            --------------
! allocate flux local array
ALLOCATE(ZCOUPTFL(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
ALLOCATE(ZCOUPUFL(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
ALLOCATE(ZCOUPVFL(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
! allocate sfc variable local array
ALLOCATE(ZCOUPUA(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
ALLOCATE(ZCOUPVA(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
ALLOCATE(ZCOUPTA(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
ALLOCATE(ZCOUPUO(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
ALLOCATE(ZCOUPVO(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
ALLOCATE(ZCOUPTO(SIZE(XRHODJ,1),SIZE(XRHODJ,2)))
! values in ocean sfc
IKE=KKU-JPVEXT
ZCOUPUO(:,:)= XUT(:,:,IKE)
ZCOUPVO(:,:)= XVT(:,:,IKE)
ZCOUPTO(:,:)= XTHT(:,:,IKE)
!
! we are going to the atmos model i.e. Model 1
CALL GOTO_MODEL(KDAD)
IIB=1
IIE=IIU
IJB=1
IJE=IJU
!
! compute gradient between ocean & atmosphere
ZCOUPUA(:,:)= XUT(:,:,2)-ZCOUPUO(:,:)
ZCOUPVA(:,:)= XVT(:,:,2)-ZCOUPVO(:,:)
ZCOUPTA(:,:)= XTHT(:,:,2)-ZCOUPTO(:,:)
!
! sfc flux computation  * RHO AIR !!!!
! flux vu atmosp
!
ZCOUPTFL(:,:) = -1.2*1.E-3* SQRT(ZCOUPUA(:,:)**2 +ZCOUPVA(:,:)**2) * ZCOUPTA(:,:)
ZCOUPUFL(:,:) = -1.2*1.E-3* SQRT(ZCOUPUA(:,:)**2 +ZCOUPVA(:,:)**2) * ZCOUPUA(:,:)
ZCOUPVFL(:,:) = -1.2*1.E-3* SQRT(ZCOUPUA(:,:)**2 +ZCOUPVA(:,:)**2) * ZCOUPVA(:,:)
!
XSSUFL_C(:,:,1)= ZCOUPUFL(:,:)
XSSVFL_C(:,:,1)= ZCOUPVFL(:,:)
XSSTFL_C(:,:,1)= ZCOUPTFL(:,:)
!
!
! We are going back in the ocean model  
! same sign & unit at the top of ocean model and the bottom of atmospheric model
!  rho_atmos * (w'u')_atmos = rho_ocean * (u'w')_ocean
!  rho_atmos *Cp_atmos* (u'w')_atmos = rho_ocean * CP_ocean * (u'w')_ocean
!
CALL GOTO_MODEL(KMI)
XSSUFL_C(:,:,1)= ZCOUPUFL(:,:)/XRH00OCEAN
XSSVFL_C(:,:,1)= ZCOUPVFL(:,:)/XRH00OCEAN
XSSTFL_C(:,:,1)= ZCOUPTFL(:,:)*1004./(3900.*XRH00OCEAN)
DEALLOCATE(ZCOUPUA,ZCOUPVA,ZCOUPUO,ZCOUPVO,ZCOUPTA,ZCOUPTO)
DEALLOCATE(ZCOUPTFL)
!
END SUBROUTINE NHOA_COUPL_n

