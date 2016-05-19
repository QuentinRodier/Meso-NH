!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/10/17 13:34:38
!-----------------------------------------------------------------
!     ####################
      MODULE MODD_ELEC_n
!     ####################
!
!!****  *MODD_ELEC$n* - declaration of electric fields
!!
!!    PURPOSE
!!    -------
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!          
!!    AUTHOR
!!    ------
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE ELEC_t
!
  REAL, DIMENSION(:,:,:), POINTER :: XNI_SDRYG=>NULL(), XNI_IDRYG=>NULL(),  &
     XNI_IAGGS=>NULL(),                 & 
     XEFIELDU=>NULL(), & ! The 3 components of the electric field
     XEFIELDV=>NULL(), XEFIELDW=>NULL(), &
     XESOURCEFW=>NULL(),  & ! Fair weather electric charge (C m^-3)
     XIND_RATE=>NULL(), XEW=>NULL(),  & 
     XIONSOURCEFW =>NULL(), & ! Fair weather ionic source
                              !  (ion pairs m-3 s-1) hold constant in time
     XCION_POS_FW =>NULL(), XCION_NEG_FW =>NULL(), &  !Positive and Negative ion mixing ratio
     XMOBIL_POS =>NULL(), XMOBIL_NEG=>NULL() ! m2/V/s
!
!  Parameters for flat lapalcian operator to solve the electric field
!            (see MODD_DYN_n)
  REAL, DIMENSION(:), POINTER :: XRHOM_E =>NULL(), XAF_E =>NULL(), XCF_E =>NULL()
  REAL, DIMENSION(:,:,:), POINTER :: XBFY_E =>NULL()
     
!
END TYPE ELEC_t

TYPE(ELEC_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: ELEC_MODEL

REAL, DIMENSION(:,:,:), POINTER :: XNI_SDRYG=>NULL(), XNI_IDRYG=>NULL(),  &
                 XNI_IAGGS=>NULL(), XEFIELDU=>NULL(),                     &
                 XESOURCEFW=>NULL(), XEFIELDV=>NULL(), XEFIELDW=>NULL(),  &
                 XIND_RATE=>NULL(), XIONSOURCEFW =>NULL(), XEW=>NULL(),   &                
                 XCION_POS_FW =>NULL(), XCION_NEG_FW =>NULL(),            &  
                 XMOBIL_POS =>NULL(), XMOBIL_NEG=>NULL(),  XBFY_E =>NULL()
REAL, DIMENSION(:), POINTER :: XRHOM_E =>NULL(), XAF_E =>NULL(), XCF_E =>NULL()

CONTAINS

SUBROUTINE ELEC_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
ELEC_MODEL(KFROM)%XNI_SDRYG=>XNI_SDRYG
ELEC_MODEL(KFROM)%XNI_IDRYG=>XNI_IDRYG
ELEC_MODEL(KFROM)%XNI_IAGGS=>XNI_IAGGS
ELEC_MODEL(KFROM)%XIND_RATE=>XIND_RATE
ELEC_MODEL(KFROM)%XEW=>XEW
ELEC_MODEL(KFROM)%XEFIELDU=>XEFIELDU
ELEC_MODEL(KFROM)%XEFIELDV=>XEFIELDV
ELEC_MODEL(KFROM)%XEFIELDW=>XEFIELDW
ELEC_MODEL(KFROM)%XESOURCEFW=>XESOURCEFW
ELEC_MODEL(KFROM)%XIONSOURCEFW=>XIONSOURCEFW
ELEC_MODEL(KFROM)%XCION_POS_FW=>XCION_POS_FW
ELEC_MODEL(KFROM)%XCION_NEG_FW=>XCION_NEG_FW
ELEC_MODEL(KFROM)%XMOBIL_POS=>XMOBIL_POS  
ELEC_MODEL(KFROM)%XMOBIL_NEG=>XMOBIL_NEG  
ELEC_MODEL(KFROM)%XBFY_E=>XBFY_E
ELEC_MODEL(KFROM)%XRHOM_E=>XRHOM_E
ELEC_MODEL(KFROM)%XAF_E=>XAF_E
ELEC_MODEL(KFROM)%XCF_E=>XCF_E
!
! Current model is set to model KTO
XNI_SDRYG=>ELEC_MODEL(KTO)%XNI_SDRYG
XNI_IDRYG=>ELEC_MODEL(KTO)%XNI_IDRYG
XNI_IAGGS=>ELEC_MODEL(KTO)%XNI_IAGGS
XIND_RATE=>ELEC_MODEL(KTO)%XIND_RATE
XEW=>ELEC_MODEL(KTO)%XEW
XEFIELDU=>ELEC_MODEL(KTO)%XEFIELDU
XEFIELDV=>ELEC_MODEL(KTO)%XEFIELDV
XEFIELDW=>ELEC_MODEL(KTO)%XEFIELDW
XESOURCEFW=>ELEC_MODEL(KTO)%XESOURCEFW
XIONSOURCEFW=>ELEC_MODEL(KTO)%XIONSOURCEFW
XCION_POS_FW=>ELEC_MODEL(KTO)%XCION_POS_FW
XCION_NEG_FW=>ELEC_MODEL(KTO)%XCION_NEG_FW
XMOBIL_POS=>ELEC_MODEL(KTO)%XMOBIL_POS
XMOBIL_NEG=>ELEC_MODEL(KTO)%XMOBIL_NEG
XBFY_E=>ELEC_MODEL(KTO)%XBFY_E
XRHOM_E=>ELEC_MODEL(KTO)%XRHOM_E
XAF_E=>ELEC_MODEL(KTO)%XAF_E
XCF_E=>ELEC_MODEL(KTO)%XCF_E
END SUBROUTINE ELEC_GOTO_MODEL

END MODULE MODD_ELEC_n
