!!
!!    #####################
      MODULE MODN_CHS_ORILAM
!!    #####################
!!
!!*** *MODN_CHS_ORILAM*
!!
!!    PURPOSE
!!    -------
!       Namelist for surface ORILAM aerosol parameters 
!!
!!**  AUTHOR
!!    ------
!!    P. Tulet      *Laboratoire d'Aerollogie*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 24/01/05
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_CHS_AEROSOL, ONLY: LCH_AERO_FLUX, XEMISSIGI, XEMISSIGJ,  &
                              XEMISRADIUSI, XEMISRADIUSJ, CRGUNIT,&
                              LCO2PM  
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_CHS_ORILAM/   LCH_AERO_FLUX, XEMISSIGI, XEMISSIGJ,   &
                              XEMISRADIUSI, XEMISRADIUSJ,  CRGUNIT,&
                              LCO2PM  

!
END MODULE MODN_CHS_ORILAM
