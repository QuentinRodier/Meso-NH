!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE SUBSCALE_AOS (UG, USS, &
                               OZ0EFFI,OZ0EFFJ,PSEA)
!     #############################################
!
!!*SUBSCALE_AOS  computes the sum of the ratio: (h'-h)/L when  h'/L >h/L  
!!                  the ' is for subgrid scale orography
!!
!!
!!    METHOD
!!    ------
!!    See M.Georgelin and al. July 1994, Monthly Weather Review.
!!   
!!    EXTERNAL
!!    --------
!!
!!    AUTHOR
!!    ------
!!
!!    M. Georgelin      Laboratoire d'Aerologie
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    18/12/95
!!    (V. Masson) 10/03/97 rewrites the routine in doctor norm.
!!                         computations are made even if a only a few subsquares
!!                         contains data points.
!!                         returns to the calling routine the localization of
!!                         the points where the z0 coefficients are available.
!!    (V. Masson) 03/2004  externalization
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_PGDWORK,        ONLY : NSSO, XSSQO, LSSQO
USE MODD_PGD_GRID,       ONLY : NL, CGRID, XGRID_PAR, NGRID_PAR
!
USE MODI_GET_ADJACENT_MESHES
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_MESH_DIM
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
LOGICAL, DIMENSION(:), INTENT(OUT) :: OZ0EFFI! .T. : the z0eff coefficients
!                                            ! are computed at grid point
!                                            ! .F. : not enough sub-grid
!                                            ! information avalaible to
!                                            ! compute the coefficients
LOGICAL, DIMENSION(:), INTENT(OUT) :: OZ0EFFJ! .T. : the z0eff coefficients
!                                            ! are computed at grid point
!                                            ! .F. : not enough sub-grid
!                                            ! information avalaible to
!                                            ! compute the coefficients
REAL,    DIMENSION(:), INTENT(IN)  :: PSEA   ! sea fraction

!
!*    0.2    Declaration of indexes
!            ----------------------
!
!
INTEGER :: JL          ! loop index on grid meshs
INTEGER :: IL          ! grid mesh index of second subgrid point used
INTEGER :: JISS, JJSS  ! loop indexes for subsquares arrays
INTEGER :: JNEXT       ! loop index on subgrid meshes
INTEGER :: INEXT       ! index to add to JISS or JJSS to obtain the following
!                      ! point containing a data in a segment
INTEGER, DIMENSION(NL) :: ILEFT   ! index of left   grid mesh 
INTEGER, DIMENSION(NL) :: IRIGHT  ! index of right  grid mesh 
INTEGER, DIMENSION(NL) :: ITOP    ! index of top    grid mesh 
INTEGER, DIMENSION(NL) :: IBOTTOM ! index of bottom grid mesh
!
!*    0.3    Declaration of counters inside a grid (JL)
!            -----------------------
!
INTEGER :: IHO2COUNTERJP ! number of times where h/2 has been summed for JP coef
INTEGER :: IHO2COUNTERJM ! number of times where h/2 has been summed for JM coef
INTEGER :: IHO2COUNTERIP ! number of times where h/2 has been summed for IP coef
INTEGER :: IHO2COUNTERIM ! number of times where h/2 has been summed for IM coef
INTEGER :: IAOSCOUNTER   ! number of segments where A/S has been summed
INTEGER :: IAOSDIST ! distance between first and last subsquares used in
!                   ! computation of A/S in a subsegment of the grid
LOGICAL :: GFIRST   ! T indicates the first point has been found for this segment.
!
!*    0.4    Declaration of working arrays inside a grid (JL)
!            -----------------------------
!
REAL, DIMENSION(NSSO) :: ZAOSIP ! A/S in each subsegment for IP coef.
REAL, DIMENSION(NSSO) :: ZAOSIM ! A/S in each subsegment for IM coef.
REAL, DIMENSION(NSSO) :: ZAOSJP ! A/S in each subsegment for JP coef.
REAL, DIMENSION(NSSO) :: ZAOSJM ! A/S in each subsegment for JM coef.
REAL :: ZAIP      ! Area in a subsegment for IP coef.
REAL :: ZAIM      ! Area in a subsegment for IM coef.
REAL :: ZAJP      ! Area in a subsegment for JP coef.
REAL :: ZAJM      ! Area in a subsegment for JM coef.
REAL :: ZSUMHO2IP ! sum of h/2 in the grid for IP coef.
REAL :: ZSUMHO2IM ! sum of h/2 in the grid for IM coef.
REAL :: ZSUMHO2JP ! sum of h/2 in the grid for JP coef.
REAL :: ZSUMHO2JM ! sum of h/2 in the grid for JM coef.
REAL :: ZSSAOS    ! A/S between 2 following points along a subsegment
REAL :: ZSLOPE    ! slope between 2 following points along a subsegment
REAL :: ZDXEFF    ! width of a subsquare along I axis
REAL :: ZDYEFF    ! width of a subsquare along J axis
!
!*    0.5    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(NL)   :: ZDX      ! grid mesh size in x direction
REAL, DIMENSION(NL)   :: ZDY      ! grid mesh size in y direction
REAL, DIMENSION(0:NL) :: ZSLOPEIP ! x mean slope
REAL, DIMENSION(0:NL) :: ZSLOPEJP ! y mean slope
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Initializations
!            ---------------
!
!*    1.1    Occurence of computation of the coefficients
!            --------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_AOS',0,ZHOOK_HANDLE)
OZ0EFFI(:)=.FALSE.
OZ0EFFJ(:)=.FALSE.
!
!*    1.2    Grid dimension (meters)
!            -----------------------
!
 CALL GET_MESH_DIM(CGRID,NGRID_PAR,NL,XGRID_PAR,ZDX,ZDY,UG%XMESH_SIZE)
!
!
!*    1.3    Left, top, right and bottom adjacent gris meshes
!            ------------------------------------------------
!
 CALL GET_ADJACENT_MESHES(CGRID,NGRID_PAR,NL,XGRID_PAR,ILEFT,IRIGHT,ITOP,IBOTTOM)
!
!
!*    1.4    Mean slopes between 2 grid meshes
!            -----------
!
ZSLOPEIP(0) = 0.
ZSLOPEJP(0) = 0.
!
DO JL=1,NL
  IF (IRIGHT(JL)/=0 .AND. ILEFT(JL)/=0) THEN
    ZSLOPEIP(JL) =  0.5 * ( USS%XAVG_ZS(IRIGHT(JL)) - USS%XAVG_ZS(JL) ) &
                          / ( 0.5 * (ZDX(IRIGHT(JL)) + ZDX(JL)) ) &
                    + 0.5 * ( USS%XAVG_ZS(JL) - USS%XAVG_ZS(ILEFT (JL)) ) &
                          / ( 0.5 * (ZDX(JL)  + ZDX(ILEFT(JL))) )  
  ELSE
    ZSLOPEIP(JL) = 0.
  END IF
  IF (ITOP(JL)/=0 .AND. IBOTTOM(JL)/=0) THEN
    ZSLOPEJP(JL) =  0.5 * ( USS%XAVG_ZS(ITOP(JL))     - USS%XAVG_ZS(JL) ) &
                          / ( 0.5 * (ZDY(ITOP(JL))     + ZDY(JL)) ) &
                    + 0.5 * ( USS%XAVG_ZS(JL) - USS%XAVG_ZS(IBOTTOM (JL)) ) &
                          / ( 0.5 * (ZDY(JL)  + ZDY(IBOTTOM(JL))) )  
  ELSE
    ZSLOPEJP(JL) = 0.
  END IF
END DO
!
!----------------------------------------------------------------------------
!
!*    2.     Loop on grid points
!            -------------------
!
DO JL=1,NL
!
!*    2.1    No land in grid mesh
!            --------------------
!
    IF (PSEA(JL)==1.) CYCLE
!
!*    2.2    Index Initializations
!            ---------------------
!
    ZDXEFF=ZDX(JL)/FLOAT(NSSO)
    ZDYEFF=ZDY(JL)/FLOAT(NSSO)
!
!----------------------------------------------------------------------------
!
!*    3.     Computations for IP and IM fields
!            ---------------------------------
!
    ZAOSIP(:)=0.
    ZAOSIM(:)=0.
    ZSUMHO2IP=0.
    ZSUMHO2IM=0.
    IHO2COUNTERIP=0
    IHO2COUNTERIM=0
    IAOSCOUNTER=0
!
!*    3.1    loop on jss index (there is no specific computation along j)
!            -----------------
!
    DO JJSS=1,NSSO
!
!*    3.1.1  initializes counters for the A/S subscale segment computation
!
      GFIRST = .TRUE.
      IAOSDIST=0
      ZAIP=0.
      ZAIM=0.
!
!*    3.2    loop on iss index
!            -----------------
!
      DO JISS=1,NSSO
!
!*    3.3    search for two consecutive grid points
!            --------------------------------------
!
!*    3.3.1 first one
!
        IF (.NOT. LSSQO(JISS,JJSS,JL) ) CYCLE
!
!*    3.3.2  second one (up to one grid mesh further)
!
        DO JNEXT=1,NSSO
          IF (JISS+JNEXT>NSSO) THEN
            IL    = IRIGHT(JL)
            INEXT = JISS+JNEXT-NSSO
          ELSE
            IL    = JL
            INEXT = JISS+JNEXT
          END IF
          ! no right point
          IF (IL==0) EXIT
          ! subgrid data found
          IF (LSSQO(INEXT,JJSS,IL)) EXIT
        END DO
!
!*    3.3.3  none found: end of loop along jss
!                        ---------------------
!
        IF (JNEXT>=NSSO+1) EXIT
!
!*    3.3.4  second point outside of the domain: end of loop along iss
!                                                ---------------------
!
        IF (IL==0) EXIT
!
!*    3.4    add terms to sums of A/S and h/2
!            --------------------------------
!
        IF (GFIRST) IAOSCOUNTER=IAOSCOUNTER+1
        GFIRST = .FALSE.
        IAOSDIST   =IAOSDIST+JNEXT
!
!*    3.4.1  mean slope
!
        ZSLOPE=ZSLOPEIP(JL)
!
!*    3.4.2  A/S term
!
        ZSSAOS =  XSSQO(INEXT,JJSS,IL) - XSSQO(JISS,JJSS,JL) &
                  - ZSLOPE * ZDXEFF * JNEXT  
        IF (ZSSAOS>0.) ZAIP=ZAIP+ZSSAOS
        IF (ZSSAOS<0.) ZAIM=ZAIM-ZSSAOS
!
!*    3.4.3  h/2 term
!
        IF (ZSSAOS>0.) THEN
          ZSUMHO2IP = ZSUMHO2IP + 0.5 * ZSSAOS
          IHO2COUNTERIP=IHO2COUNTERIP+1
        END IF
        IF (ZSSAOS<0.) THEN
          ZSUMHO2IM = ZSUMHO2IM - 0.5 * ZSSAOS
          IHO2COUNTERIM=IHO2COUNTERIM+1
        END IF
!
!*    3.5    end of loop on iss index
!            ------------------------
!
      END DO
      IF (IAOSDIST>0) THEN
        ZAOSIP(JJSS)=ZAIP/(ZDXEFF*IAOSDIST)
        ZAOSIM(JJSS)=ZAIM/(ZDXEFF*IAOSDIST)
      END IF
!
!*    3.6    end of loop on jss index
!            ------------------------
!
    END DO
!
!*    3.7    end of IP and IM coefficients
!            -----------------------------
!
    IF (IAOSCOUNTER>0) THEN
      USS%XAOSIP(JL)=SUM(ZAOSIP) / IAOSCOUNTER
      USS%XAOSIM(JL)=SUM(ZAOSIM) / IAOSCOUNTER
      IF (IHO2COUNTERIP>0) THEN
        USS%XHO2IP(JL)=ZSUMHO2IP   / IHO2COUNTERIP
      ELSE
        USS%XHO2IP(JL)=0.
      END IF
      IF (IHO2COUNTERIM>0) THEN
        USS%XHO2IM(JL)=ZSUMHO2IM   / IHO2COUNTERIM
      ELSE
        USS%XHO2IM(JL)=0.
      END IF
      OZ0EFFI(JL)=.TRUE.
    END IF
!
!----------------------------------------------------------------------------
!
!*    4.     Computations for JP and JM fields
!            ---------------------------------
!
    ZAOSJP(:)=0.
    ZAOSJM(:)=0.
    ZSUMHO2JP=0.
    ZSUMHO2JM=0.
    IHO2COUNTERJP=0
    IHO2COUNTERJM=0
    IAOSCOUNTER=0
!
!*    4.1    loop on iss index (there is no specific computation along i)
!            -----------------
!
    DO JISS=1,NSSO
!
!*    4.1.1  initializes counters for the A/S subscale segment computation
!
      GFIRST = .TRUE.
      IAOSDIST=0
      ZAJP=0.
      ZAJM=0.
!
!*    4.2    loop on jss index
!            -----------------
!
      DO JJSS=1,NSSO
!
!*    4.3    search for two consecutive grid points
!            --------------------------------------
!
!*    4.3.1 first one
!
        IF (.NOT. LSSQO(JISS,JJSS,JL) ) CYCLE
!
!*    4.3.2  second one (up to one grid mesh further)
!
        DO JNEXT=1,NSSO
          IF (JJSS+JNEXT>NSSO) THEN
            IL    = ITOP(JL)
            INEXT = JJSS+JNEXT-NSSO
          ELSE
            IL    = JL
            INEXT = JJSS+JNEXT
          END IF
          ! no right point
          IF (IL==0) EXIT
          ! subgrid data found
          IF (LSSQO(JISS,INEXT,IL)) EXIT
        END DO
!
!*    4.3.3  none found: end of loop along jss
!                        ---------------------
!
        IF (JNEXT>=NSSO+1) EXIT
!
!*    4.3.4  second point outside of the domain: end of loop along iss
!                                                ---------------------
!
        IF (IL==0) EXIT
!
!
!*    4.4    add terms to sums of A/S and h/2
!            --------------------------------
!
        IF (GFIRST) IAOSCOUNTER=IAOSCOUNTER+1
        GFIRST = .FALSE.
        IAOSDIST   =IAOSDIST+JNEXT
!
!*    4.4.1  mean slope
!
        ZSLOPE=ZSLOPEJP(JL)
!
!*    4.4.2  A/S term
!
        ZSSAOS =  XSSQO(JISS,INEXT,IL) - XSSQO(JISS,JJSS,JL) &
                  - ZSLOPE * ZDYEFF * JNEXT  
        IF (ZSSAOS>0.) ZAJP=ZAJP+ZSSAOS
        IF (ZSSAOS<0.) ZAJM=ZAJM-ZSSAOS
!
!*    4.4.3  h/2 term
!
        IF (ZSSAOS>0.) THEN
          ZSUMHO2JP = ZSUMHO2JP + 0.5 * ZSSAOS
          IHO2COUNTERJP=IHO2COUNTERJP+1
        END IF
        IF (ZSSAOS<0.) THEN
          ZSUMHO2JM = ZSUMHO2JM - 0.5 * ZSSAOS
          IHO2COUNTERJM=IHO2COUNTERJM+1
        END IF
!
!*    4.5    end of loop on jss index
!            ------------------------
!
      END DO
      IF (IAOSDIST>0) THEN
        ZAOSJP(JISS)=ZAJP/(ZDYEFF*IAOSDIST)
        ZAOSJM(JISS)=ZAJM/(ZDYEFF*IAOSDIST)
      END IF
!
!*    4.6    end of loop on iss index
!            ------------------------
!
    END DO
!
!*    4.7    end of JP and JM coefficients
!            -----------------------------
!
    IF (IAOSCOUNTER>0) THEN
      USS%XAOSJP(JL)=SUM(ZAOSJP) /IAOSCOUNTER
      USS%XAOSJM(JL)=SUM(ZAOSJM) /IAOSCOUNTER
      IF (IHO2COUNTERJP>0) THEN
        USS%XHO2JP(JL)=ZSUMHO2JP   /IHO2COUNTERJP
      ELSE
        USS%XHO2JP(JL)=0.
      END IF
      IF (IHO2COUNTERJM>0) THEN
        USS%XHO2JM(JL)=ZSUMHO2JM   /IHO2COUNTERJM
      ELSE
        USS%XHO2JM(JL)=0.
      END IF
      OZ0EFFJ(JL)=.TRUE.
    END IF
!
!-------------------------------------------------------------------------------
!
!*    5.     Next grid point
!            ---------------
!
END DO
IF (LHOOK) CALL DR_HOOK('SUBSCALE_AOS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SUBSCALE_AOS
