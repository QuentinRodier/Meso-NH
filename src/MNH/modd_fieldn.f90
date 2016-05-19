!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/06/27 14:17:24
!-----------------------------------------------------------------
!     ###################
      MODULE MODD_FIELD_n
!     ###################
!
!!****  *MODD_FIELD$n* - declaration of prognostic variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     prognostic variables. 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_FIELDn)
!!      Technical Specifications Report of the Meso-NH (chapters 2 and 3)
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       05/05/94                      
!!      Modifications  03/01/95  (Lafore)  To add the dry mass variables Md  
!!                     09/03/95  (Stein)   eliminate R from the progn. var                    
!!                     15/03/95  (Stein)   add EPS variable
!!      Modifications  21/03/95  (Carriere) To add the subgrid condensation 
!!                                           related parameters
!!                     01/03/96  (J. Stein) add the cloud fraction
!!                     10/10/96  (J. Stein) add XSRCM and XSRCT
!!                     11/04/96  (J.-P. Pinty) add the ice concentration
!!                     25/07/97  (J. Stein) Change the variable pressure
!!                     20/05/06  Remove EPS
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE FIELD_t
  REAL, DIMENSION(:,:,:), POINTER :: XUM=>NULL(),XVM=>NULL(),XWM=>NULL()
                                      ! U,V,W  at time t-dt
  REAL, DIMENSION(:,:,:), POINTER :: XUT=>NULL(),XVT=>NULL(),XWT=>NULL()
                                      ! U,V,W  at time t
  REAL, DIMENSION(:,:,:), POINTER :: XRUS=>NULL(),XRVS=>NULL(),XRWS=>NULL()
                                      ! Source of (rho U), (rho V), (rho w) 
  REAL, DIMENSION(:,:,:), POINTER :: XTHM=>NULL()     ! (rho theta) at time t-dt
  REAL, DIMENSION(:,:,:), POINTER :: XTHT=>NULL()     ! (rho theta) at time t
  REAL, DIMENSION(:,:,:), POINTER :: XRTHS=>NULL()    ! Source of (rho theta)
  REAL, DIMENSION(:,:,:), POINTER :: XTKEM=>NULL()    ! Kinetic energy 
                                                     ! at time t-dt
  REAL, DIMENSION(:,:,:), POINTER :: XTKET=>NULL()    ! Kinetic energy
                                                     ! at time t
  REAL, DIMENSION(:,:,:), POINTER :: XRTKES=>NULL()   ! Source of kinetic energy
                                                     ! (rho e)
  REAL, DIMENSION(:,:,:), POINTER :: XPABSM=>NULL()   ! absolute pressure at
                                                     ! time t-dt
  REAL, DIMENSION(:,:,:), POINTER :: XPABST=>NULL()   ! absolute pressure at
                                                     ! time t
  REAL, DIMENSION(:,:,:,:), POINTER :: XRM=>NULL()    ! Moist variables 
                                                     ! at time t-dt
  REAL, DIMENSION(:,:,:,:), POINTER :: XRT=>NULL()    ! Moist variables (rho Rn) 
                                                     ! at time t
  REAL, DIMENSION(:,:,:,:), POINTER :: XRRS=>NULL()   ! Source of Moist variables
                                                     ! (rho Rn) 
  REAL, DIMENSION(:,:,:,:), POINTER :: XSVM=>NULL()   ! Additionnal scalar
                                                     ! variables at time t-dt
  REAL, DIMENSION(:,:,:,:), POINTER :: XSVT=>NULL()   ! Additionnal scalar
                                                     ! variables at time t  
  REAL, DIMENSION(:,:,:,:), POINTER :: XRSVS=>NULL()  ! Source of addi. scalar
                                                     !  variables (rho Sn.) 
  REAL                          ::   XDRYMASST    ! Mass of dry air Md
  REAL                          ::   XDRYMASSS    ! LS sources of Md
  REAL, DIMENSION(:,:,:), POINTER :: XSRC=>NULL()     ! turbulent flux <s'Rc'>
  REAL, DIMENSION(:,:,:), POINTER :: XSIGS=>NULL()    ! =sqrt(<s's'>) for the
                                                     ! Subgrid Condensation
  REAL, DIMENSION(:,:,:), POINTER :: XCLDFR=>NULL()   ! cloud fraction
  REAL, DIMENSION(:,:,:), POINTER :: XSRCM=>NULL()    ! turbulent flux <s'Rc'>
                                                     ! at t- delta t
  REAL, DIMENSION(:,:,:), POINTER :: XSRCT=>NULL()    ! turbulent flux <s'Rc'>
                                                     ! at t
  REAL, DIMENSION(:,:,:), POINTER :: XCIT=>NULL()     ! Pristine ice concentration
!
END TYPE FIELD_t

TYPE(FIELD_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: FIELD_MODEL

REAL, DIMENSION(:,:,:), POINTER :: XUM=>NULL(),XVM=>NULL(),XWM=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XUT=>NULL(),XVT=>NULL(),XWT=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRUS=>NULL(),XRVS=>NULL(),XRWS=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XTHM=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XTHT=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRTHS=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XTKEM=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XTKET=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRTKES=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XPABSM=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XPABST=>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XRM=>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XRT=>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XRRS=>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XSVM=>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XSVT=>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XRSVS=>NULL()
REAL, POINTER :: XDRYMASST=>NULL()
REAL, POINTER :: XDRYMASSS=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XSRC=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XSIGS=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XCLDFR=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XSRCM=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XSRCT=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XCIT=>NULL()

CONTAINS

SUBROUTINE FIELD_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
FIELD_MODEL(KFROM)%XUM=>XUM
FIELD_MODEL(KFROM)%XVM=>XVM
FIELD_MODEL(KFROM)%XWM=>XWM
FIELD_MODEL(KFROM)%XUT=>XUT
FIELD_MODEL(KFROM)%XVT=>XVT
FIELD_MODEL(KFROM)%XWT=>XWT
FIELD_MODEL(KFROM)%XRUS=>XRUS
FIELD_MODEL(KFROM)%XRVS=>XRVS
FIELD_MODEL(KFROM)%XRWS=>XRWS
FIELD_MODEL(KFROM)%XTHM=>XTHM
FIELD_MODEL(KFROM)%XTHT=>XTHT
FIELD_MODEL(KFROM)%XRTHS=>XRTHS
FIELD_MODEL(KFROM)%XTKEM=>XTKEM
FIELD_MODEL(KFROM)%XTKET=>XTKET
FIELD_MODEL(KFROM)%XRTKES=>XRTKES
FIELD_MODEL(KFROM)%XPABSM=>XPABSM
FIELD_MODEL(KFROM)%XPABST=>XPABST
FIELD_MODEL(KFROM)%XRM=>XRM
FIELD_MODEL(KFROM)%XRT=>XRT
FIELD_MODEL(KFROM)%XRRS=>XRRS
FIELD_MODEL(KFROM)%XSVM=>XSVM
FIELD_MODEL(KFROM)%XSVT=>XSVT
FIELD_MODEL(KFROM)%XRSVS=>XRSVS
FIELD_MODEL(KFROM)%XSRC=>XSRC
FIELD_MODEL(KFROM)%XSIGS=>XSIGS
FIELD_MODEL(KFROM)%XCLDFR=>XCLDFR
FIELD_MODEL(KFROM)%XSRCM=>XSRCM
FIELD_MODEL(KFROM)%XSRCT=>XSRCT
FIELD_MODEL(KFROM)%XCIT=>XCIT
!
! Current model is set to model KTO
XUM=>FIELD_MODEL(KTO)%XUM
XVM=>FIELD_MODEL(KTO)%XVM
XWM=>FIELD_MODEL(KTO)%XWM
XUT=>FIELD_MODEL(KTO)%XUT
XVT=>FIELD_MODEL(KTO)%XVT
XWT=>FIELD_MODEL(KTO)%XWT
XRUS=>FIELD_MODEL(KTO)%XRUS
XRVS=>FIELD_MODEL(KTO)%XRVS
XRWS=>FIELD_MODEL(KTO)%XRWS
XTHM=>FIELD_MODEL(KTO)%XTHM
XTHT=>FIELD_MODEL(KTO)%XTHT
XRTHS=>FIELD_MODEL(KTO)%XRTHS
XTKEM=>FIELD_MODEL(KTO)%XTKEM
XTKET=>FIELD_MODEL(KTO)%XTKET
XRTKES=>FIELD_MODEL(KTO)%XRTKES
XPABSM=>FIELD_MODEL(KTO)%XPABSM
XPABST=>FIELD_MODEL(KTO)%XPABST
XRM=>FIELD_MODEL(KTO)%XRM
XRT=>FIELD_MODEL(KTO)%XRT
XRRS=>FIELD_MODEL(KTO)%XRRS
XSVM=>FIELD_MODEL(KTO)%XSVM
XSVT=>FIELD_MODEL(KTO)%XSVT
XRSVS=>FIELD_MODEL(KTO)%XRSVS
XDRYMASST=>FIELD_MODEL(KTO)%XDRYMASST
XDRYMASSS=>FIELD_MODEL(KTO)%XDRYMASSS
XSRC=>FIELD_MODEL(KTO)%XSRC
XSIGS=>FIELD_MODEL(KTO)%XSIGS
XCLDFR=>FIELD_MODEL(KTO)%XCLDFR
XSRCM=>FIELD_MODEL(KTO)%XSRCM
XSRCT=>FIELD_MODEL(KTO)%XSRCT
XCIT=>FIELD_MODEL(KTO)%XCIT

END SUBROUTINE FIELD_GOTO_MODEL

END MODULE MODD_FIELD_n
