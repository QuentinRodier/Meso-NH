!!
!!    #####################
      MODULE MODN_SURF_ATM
!!    #####################
!!
!!*** *MODN_DUST*
!!
!!    PURPOSE
!!    -------
!       Namelist for wind threshold
!!
!!**  AUTHOR
!!    ------
!!    P. Le Moigne      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 10/2007
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_SURF_ATM, ONLY : XCISMIN, XVMODMIN, LALDTHRES, &
                            LDRAG_COEF_ARP, LALDZ0H,      &
                            LNOSOF, LRW_PRECIP,           &
                            XEDB, XEDC, XEDD, XEDK,       &
                            XUSURIC, XUSURID, XUSURICL,   &
                            XVCHRNK, XVZ0CM, XDELTA_MAX,  &
                            XRIMAX, LVERTSHIFT,           &
                            LVZIUSTAR0_ARP, LRRGUST_ARP,  &
                            XVZIUSTAR0,XRZHZ0M,           &
                            XRRSCALE, XRRGAMMA,           &
                            XUTILGUST, LCPL_ARP, LQVNPLUS,&
                            CIMPLICIT_WIND
!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_SURF_ATM/XCISMIN, XVMODMIN, LALDTHRES, &
                         LDRAG_COEF_ARP, LALDZ0H,      &
                         LNOSOF, LRW_PRECIP,           &
                         XEDB, XEDC, XEDD, XEDK,       &
                         XUSURIC, XUSURID, XUSURICL,   &
                         XVCHRNK, XVZ0CM, XDELTA_MAX,  &
                         XRIMAX, LVERTSHIFT,           &
                         LVZIUSTAR0_ARP, LRRGUST_ARP,  &
                         XVZIUSTAR0,XRZHZ0M,           &
                         XRRSCALE, XRRGAMMA,           &
                         XUTILGUST, LCPL_ARP, LQVNPLUS,&
                         CIMPLICIT_WIND
!
END MODULE MODN_SURF_ATM
