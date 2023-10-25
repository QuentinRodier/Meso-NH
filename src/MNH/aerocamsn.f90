!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 chimie 2006/06/16 13:28:57
!-----------------------------------------------------------------
!!   ########################
     MODULE MODI_AEROCAMS_n
!!   ########################
!!
INTERFACE
!!
SUBROUTINE AEROCAMS_n(PSV, PRHODREF)
IMPLICIT NONE
REAL,       DIMENSION(:,:,:,:),  INTENT(INOUT) :: PSV
REAL,       DIMENSION(:,:,:),  INTENT(IN) :: PRHODREF
END SUBROUTINE AEROCAMS_n
!!
END INTERFACE
!!
END MODULE MODI_AEROCAMS_n
!!
!!
!!   ############################################################
     SUBROUTINE AEROCAMS_n(PSV, PRHODREF)
!!   ############################################################
!!
!!    PURPOSE
!!    -------
!!    Converti les masses aerosols issues de CMAS (kg/kg) en variables aerosols (ppv)
!!    Realise l'équilibre des moments à partir du sigma et du diametre moyen
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!    
!!
!!    EXTERNAL
!!    --------
!!    None
!!

USE MODE_AERO_PSD
USE MODD_CH_AEROSOL
!!
IMPLICIT NONE
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:,:,:),    INTENT(INOUT) :: PSV
REAL,   DIMENSION(:,:,:),      INTENT(IN) :: PRHODREF
!
!
INTEGER  :: JN
!
! SV conversion from kg.kg-3 --> µg/m3 )

DO JN =1,SIZE(PSV, 4)
 PSV(:,:,:,JN) = PSV(:,:,:,JN) * 1E9 / PRHODREF(:,:,:)
 !print*, CAERONAMES(JN),' =',MINVAL(PSV(:,:,:,JN)), MAXVAL(PSV(:,:,:,JN))
ENDDO
PSV(:,:,:,JP_CH_BCi) = MAX(PSV(:,:,:,JP_CH_BCi), 1E-4)
PSV(:,:,:,JP_CH_BCj) = MAX(PSV(:,:,:,JP_CH_BCj), 1E-3)
PSV(:,:,:,JP_CH_OCi) = MAX(PSV(:,:,:,JP_CH_OCi), 1E-4)
PSV(:,:,:,JP_CH_OCj) = MAX(PSV(:,:,:,JP_CH_OCj), 1E-3)
PSV(:,:,:,JP_CH_DSTi) = MAX(PSV(:,:,:,JP_CH_DSTi), 1E-4)
PSV(:,:,:,JP_CH_DSTj) = MAX(PSV(:,:,:,JP_CH_DSTj), 1E-3)

! Compute moment from aerosol mass and conversion  SV aerosols variables into ppv

CALL CON2MIX (PSV, PRHODREF)
!
!
END SUBROUTINE AEROCAMS_n
