!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! NEC0 masdev4_7 2007/06/16 01:41:59
!-----------------------------------------------------------------
!     #################
      MODULE MODD_GET_n
!     #################
!
!!****  *MODD_GET$n* - declaration of variables about getting of variables in
!!                    initialization
!!                     
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the variables 
!     which indicate how and what variables to get in initialization.
!        
!          
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_GETn)     
!!
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        05/05/94  
!!      modification    22/11/94 (J.Stein) add the get indicators for PHIT,PHIM
!!      modification    07/12/94 (J.Stein) add LS fields get indicators  
!!      modification    15/03/95 (J.Stein) remove R from the historical var. 
!!      modification    15/06/95 (J.Stein) add EPS related variables
!!      modification    15/04/96 (J.Stein) add indicator for the cloud fraction
!!      modification    15/04/96 (J.Stein) add indicator for SRCM and T
!!      modification    11/04/96 (J.-P. Pinty) add the CLDFR and ice conc.
!!      modification    25/07/97 (J.Stein) change the pressure variable
!!      modification    25/07/99 (J.Stein) add get indicators for soil, rad and
!!                                         conv schemes
!!      J.-P. Pinty 25/10/00  add get indicator for cloud scheme
!!      V. Masson   01/2004   surface externalization (rm CGETSURF)
!!                  05/2006   Remove EPS and LGETALL
!!      M. Leriche  04/2010   add get indicators for pH in cloud and rain
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX, JPSVMAX
IMPLICIT NONE

TYPE GET_t
!
  CHARACTER (LEN=4) :: CGETUM, CGETVM, CGETWM !  Get indicator for 
                                                 ! U,V,W at time t-dt
  CHARACTER (LEN=4) :: CGETUT, CGETVT, CGETWT !  Get indicator for
                                                 ! U,V,W at time t 
  CHARACTER (LEN=4) :: CGETTHM,CGETTHT        !  Get indicator for theta
                                                 ! at time t-dt and at time t
  CHARACTER (LEN=4) :: CGETPABSM, CGETPABST   !  Get indicator for
                                                 ! the absolute pressure at
                                                 ! time t-dt and t
  CHARACTER (LEN=4)  :: CGETTKEM,CGETTKET     !  Get indicator for TKE
                                                 ! at time t-dt and at time t
  CHARACTER (LEN=4)  :: CGETRVM,CGETRCM,CGETRRM !  Get indicator for  Rv
  CHARACTER (LEN=4)  :: CGETRIM,CGETRSM,CGETRGM !  Rc,Rr,Ri,Rs,Rg,Rh
  CHARACTER (LEN=4)  :: CGETRHM                 ! at time t-dt 
  CHARACTER (LEN=4)  :: CGETRVT,CGETRCT,CGETRRT !  Get indicator for Rv
  CHARACTER (LEN=4)  :: CGETRIT,CGETRST,CGETRGT ! Rc,Rr,Ri,Rs,Rg,Rh
  CHARACTER (LEN=4)  :: CGETRHT                 ! at time t 
!JUAN
  CHARACTER (LEN=4), DIMENSION(:), POINTER :: CGETSVM=>NULL(),CGETSVT=>NULL() !  Get indicator 
                                ! for the Scalar Var. at time t-dt and t
!JUAN
  CHARACTER (LEN=4) :: CGETLSUM, CGETLSVM, CGETLSWM   !  Get indicator for 
                                ! U,V,W for Larger Scales at time t-dt
  CHARACTER (LEN=4) :: CGETLSTHM, CGETLSRVM     !  Get indicator for 
                                ! Theta , Rv for Larger Scales at time t-dt
  CHARACTER (LEN=4)  :: CGETSIGS,CGETSRC      !  Get indicator for SIGS
                                ! and SRC related to the subgrid condensation
  CHARACTER (LEN=4)  :: CGETCLDFR             !  Get indicator for the
                                ! CLouD FRaction
  CHARACTER (LEN=4)  :: CGETSRCM, CGETSRCT    !  Get indicator for SRCM
                                ! and SRCT related to the subgrid condensation
  CHARACTER (LEN=4)  :: CGETCIT               !  Get indicator for the
                                                 ! primary ice concentration
  CHARACTER (LEN=4)  :: CGETCONV              ! Get indicator for the
                                ! use of a convection scheme
  CHARACTER (LEN=4)  :: CGETRAD               ! Get indicator for the
                                ! use of a radiation scheme
  CHARACTER (LEN=4)  :: CGETCLOUD             ! Get indicator for the
                                ! use of a cloud scheme
  CHARACTER (LEN=4)  :: CGETBL_DEPTH          ! Get indicator for the BL depth
  CHARACTER (LEN=4)  :: CGETSBL_DEPTH         ! Get indicator for the SBL depth
  CHARACTER (LEN=4)  :: CGETPHC,CGETPHR       ! Get indicator for the pH values
                                ! in cloud water and rainwater
!
END TYPE GET_t

TYPE(GET_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: GET_MODEL
LOGICAL    , DIMENSION(JPMODELMAX),         SAVE :: GET_FIRST_CALL = .TRUE.

CHARACTER (LEN=4), POINTER :: CGETUM=>NULL(), CGETVM=>NULL(), CGETWM=>NULL()
CHARACTER (LEN=4), POINTER :: CGETUT=>NULL(), CGETVT=>NULL(), CGETWT=>NULL()
CHARACTER (LEN=4), POINTER :: CGETTHM=>NULL(),CGETTHT=>NULL()
CHARACTER (LEN=4), POINTER :: CGETPABSM=>NULL(), CGETPABST=>NULL()
CHARACTER (LEN=4), POINTER :: CGETTKEM=>NULL(),CGETTKET=>NULL()
CHARACTER (LEN=4), POINTER :: CGETRVM=>NULL(),CGETRCM=>NULL(),CGETRRM=>NULL()
CHARACTER (LEN=4), POINTER :: CGETRIM=>NULL(),CGETRSM=>NULL(),CGETRGM=>NULL()
CHARACTER (LEN=4), POINTER :: CGETRHM=>NULL()
CHARACTER (LEN=4), POINTER :: CGETRVT=>NULL(),CGETRCT=>NULL(),CGETRRT=>NULL()
CHARACTER (LEN=4), POINTER :: CGETRIT=>NULL(),CGETRST=>NULL(),CGETRGT=>NULL()
CHARACTER (LEN=4), POINTER :: CGETRHT=>NULL()
CHARACTER (LEN=4), DIMENSION(:), POINTER :: CGETSVM=>NULL(),CGETSVT=>NULL()
CHARACTER (LEN=4), POINTER :: CGETLSUM=>NULL(), CGETLSVM=>NULL(), CGETLSWM=>NULL()
CHARACTER (LEN=4), POINTER :: CGETLSTHM=>NULL(), CGETLSRVM=>NULL()
CHARACTER (LEN=4), POINTER :: CGETSIGS=>NULL(),CGETSRC=>NULL()
CHARACTER (LEN=4), POINTER :: CGETCLDFR=>NULL()
CHARACTER (LEN=4), POINTER :: CGETSRCM=>NULL(), CGETSRCT=>NULL()
CHARACTER (LEN=4), POINTER :: CGETCIT=>NULL()
CHARACTER (LEN=4), POINTER :: CGETCONV=>NULL()
CHARACTER (LEN=4), POINTER :: CGETRAD=>NULL()
CHARACTER (LEN=4), POINTER :: CGETCLOUD=>NULL()
CHARACTER (LEN=4), POINTER :: CGETBL_DEPTH=>NULL()
CHARACTER (LEN=4), POINTER :: CGETSBL_DEPTH=>NULL()
CHARACTER (LEN=4), POINTER :: CGETPHC=>NULL()
CHARACTER (LEN=4), POINTER :: CGETPHR=>NULL()

CONTAINS

SUBROUTINE GET_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
!JUAN
IF (GET_FIRST_CALL(KTO)) THEN
ALLOCATE (GET_MODEL(KTO)%CGETSVM(JPSVMAX))
ALLOCATE (GET_MODEL(KTO)%CGETSVT(JPSVMAX))
GET_FIRST_CALL(KTO) = .FALSE.
ENDIF
!JUAN
!
! Save current state for allocated arrays
!
! Current model is set to model KTO
CGETUM=>GET_MODEL(KTO)%CGETUM
CGETVM=>GET_MODEL(KTO)%CGETVM
CGETWM=>GET_MODEL(KTO)%CGETWM
CGETUT=>GET_MODEL(KTO)%CGETUT
CGETVT=>GET_MODEL(KTO)%CGETVT
CGETWT=>GET_MODEL(KTO)%CGETWT
CGETTHM=>GET_MODEL(KTO)%CGETTHM
CGETTHT=>GET_MODEL(KTO)%CGETTHT
CGETPABSM=>GET_MODEL(KTO)%CGETPABSM
CGETPABST=>GET_MODEL(KTO)%CGETPABST
CGETTKEM=>GET_MODEL(KTO)%CGETTKEM
CGETTKET=>GET_MODEL(KTO)%CGETTKET
CGETRVM=>GET_MODEL(KTO)%CGETRVM
CGETRCM=>GET_MODEL(KTO)%CGETRCM
CGETRRM=>GET_MODEL(KTO)%CGETRRM
CGETRIM=>GET_MODEL(KTO)%CGETRIM
CGETRSM=>GET_MODEL(KTO)%CGETRSM
CGETRGM=>GET_MODEL(KTO)%CGETRGM
CGETRHM=>GET_MODEL(KTO)%CGETRHM
CGETRVT=>GET_MODEL(KTO)%CGETRVT
CGETRCT=>GET_MODEL(KTO)%CGETRCT
CGETRRT=>GET_MODEL(KTO)%CGETRRT
CGETRIT=>GET_MODEL(KTO)%CGETRIT
CGETRST=>GET_MODEL(KTO)%CGETRST
CGETRGT=>GET_MODEL(KTO)%CGETRGT
CGETRHT=>GET_MODEL(KTO)%CGETRHT
CGETSVM=>GET_MODEL(KTO)%CGETSVM
CGETSVT=>GET_MODEL(KTO)%CGETSVT
CGETLSUM=>GET_MODEL(KTO)%CGETLSUM
CGETLSVM=>GET_MODEL(KTO)%CGETLSVM
CGETLSWM=>GET_MODEL(KTO)%CGETLSWM
CGETLSTHM=>GET_MODEL(KTO)%CGETLSTHM
CGETLSRVM=>GET_MODEL(KTO)%CGETLSRVM
CGETSIGS=>GET_MODEL(KTO)%CGETSIGS
CGETSRC=>GET_MODEL(KTO)%CGETSRC
CGETCLDFR=>GET_MODEL(KTO)%CGETCLDFR
CGETSRCM=>GET_MODEL(KTO)%CGETSRCM
CGETSRCT=>GET_MODEL(KTO)%CGETSRCT
CGETCIT=>GET_MODEL(KTO)%CGETCIT
CGETCONV=>GET_MODEL(KTO)%CGETCONV
CGETRAD=>GET_MODEL(KTO)%CGETRAD
CGETCLOUD=>GET_MODEL(KTO)%CGETCLOUD
CGETBL_DEPTH=>GET_MODEL(KTO)%CGETBL_DEPTH
CGETSBL_DEPTH=>GET_MODEL(KTO)%CGETSBL_DEPTH
CGETPHC=>GET_MODEL(KTO)%CGETPHC
CGETPHR=>GET_MODEL(KTO)%CGETPHR

END SUBROUTINE GET_GOTO_MODEL

END MODULE MODD_GET_n
