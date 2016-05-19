!     #########
      SUBROUTINE SSO(OSSO,OSSO_ANIS,PSEA)
!     #########################
!
!!*SSO  computes the SSO anisotropy, direction and slope
!!
!!
!!    METHOD
!!    ------
!!    See Lott and Miller, 1997, QJRMS 101-127
!!   
!!    AUTHOR
!!    ------
!!
!!    V.Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    23/05/97
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_GET_MESH_DIM
USE MODI_GET_ADJACENT_MESHES
!
USE MODD_CSTS,           ONLY : XPI
USE MODD_PGDWORK,        ONLY : NSSO, XSSQO, LSSQO
USE MODD_PGD_GRID,       ONLY : NL, CGRID, XGRID_PAR, NGRID_PAR
USE MODD_SURF_ATM_GRID_n, ONLY : XMESH_SIZE
USE MODD_SURF_ATM_SSO_n,  ONLY : XSSO_ANIS, XSSO_DIR, XSSO_SLOPE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
LOGICAL, DIMENSION(:), INTENT(OUT) :: OSSO   ! .T. : the SSO coefficients
!                                            ! are computed at grid point
!                                            ! .F. : not enough sub-grid
!                                            ! information avalaible to
!                                            ! compute the coefficients
LOGICAL, DIMENSION(:), INTENT(OUT) :: OSSO_ANIS ! .T. : the SSO anisotropy
!                                            ! are computed at grid point
!                                            ! .F. : not enough sub-grid
!                                            ! information avalaible to
!                                            ! compute the coefficients
REAL,    DIMENSION(:), INTENT(IN)  :: PSEA   ! sea fraction
!
!*    0.2    Declaration of indexes
!            ----------------------
!
INTEGER :: JL          ! loop index on grid meshs
INTEGER :: IL          ! grid mesh index of second subgrid point used
INTEGER :: JISS, JJSS  ! loop indexes for subsquares arrays
INTEGER :: JNEXT       ! loop index on subgrid meshes
INTEGER :: JPREV       ! loop index on subgrid meshes
INTEGER :: INEXT       ! index to add to JISS or JJSS to obtain the following
!                      ! point containing a data in a segment
INTEGER :: IPREV       ! index to remove from JISS or JJSS to obtain the
!                      ! previous point containing a data in a segment
!
INTEGER :: IMAXI       ! index of the next subsquare with data along I axis,
                       ! or last subsquare inside grid mesh along I axis
INTEGER :: IMAXJ       ! index of the next subsquare with data along J axis,
                       ! or last subsquare inside grid mesh along J axis
INTEGER, DIMENSION(NL) :: ILEFT   ! index of left   grid mesh 
INTEGER, DIMENSION(NL) :: IRIGHT  ! index of right  grid mesh 
INTEGER, DIMENSION(NL) :: ITOP    ! index of top    grid mesh 
INTEGER, DIMENSION(NL) :: IBOTTOM ! index of bottom grid mesh
!
!*    0.3    Declaration of working arrays inside a MESONH grid (JI,JJ)
!            -----------------------------
!
REAL,    DIMENSION(NSSO,NSSO) :: ZDZSDX  ! topographic gradient along x
REAL,    DIMENSION(NSSO,NSSO) :: ZDZSDY  ! topographic gradient along y
LOGICAL, DIMENSION(NSSO,NSSO) :: GDZSDX  ! occurence of gradient along x
LOGICAL, DIMENSION(NSSO,NSSO) :: GDZSDY  ! occurence of gradient along y
REAL :: ZDXEFF    ! width of a subsquare along I axis
REAL :: ZDYEFF    ! width of a subsquare along J axis
LOGICAL :: GBOUND ! .T.: first left (for dzs/dx) or first bottom (for dzs/dy)
                  ! sub-square gradients is being computed
!
!*    0.4    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(NL) :: ZDX        ! grid mesh size in x direction
REAL, DIMENSION(NL) :: ZDY        ! grid mesh size in y direction
REAL, DIMENSION(NL) :: ZHXX       ! topographic gradient correlation tensor: x,x
REAL, DIMENSION(NL) :: ZHXY       ! topographic gradient correlation tensor: x,y
REAL, DIMENSION(NL) :: ZHYY       ! topographic gradient correlation tensor: y,y
REAL, DIMENSION(NL) :: ZK, ZL, ZM ! diagonalised terms of the tensor
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    1.     Initializations
!            ---------------
!
IF (LHOOK) CALL DR_HOOK('SSO',0,ZHOOK_HANDLE)
ZK=0.
ZL=0.
ZM=0.
!
!*    1.1    Occurence of computation of the coefficients
!            --------------------------------------------
!
OSSO     (:)=.FALSE.
OSSO_ANIS(:)=.FALSE.
!
!
!*    1.2    Grid dimension (meters)
!            -----------------------
!
 CALL GET_MESH_DIM(CGRID,NGRID_PAR,NL,XGRID_PAR,ZDX,ZDY,XMESH_SIZE)
!
!
!*    1.3    Left, top, right and bottom adjacent gris meshes
!            ------------------------------------------------
!
 CALL GET_ADJACENT_MESHES(CGRID,NGRID_PAR,NL,XGRID_PAR,ILEFT,IRIGHT,ITOP,IBOTTOM)
!
!----------------------------------------------------------------------------
!
!*    2.     Loop on MESONH grid points
!            --------------------------
!
ZHYY(:) = 0.
ZHXX(:) = 0.
ZHXY(:) = 0.
!
DO JL=1,NL
!
!
!*    2.1    No land in grid mesh
!            --------------------
!
    IF (PSEA(JL)==1.) CYCLE
!
    ZDXEFF=ZDX(JL)/FLOAT(NSSO)
    ZDYEFF=ZDY(JL)/FLOAT(NSSO)
!
!*    2.3    Not enough data for computation
!            -------------------------------
!
! 1st step: removes points where orography data is not present at all.
!
    IF ( COUNT( LSSQO(:,:,JL)) == 0  ) CYCLE
!
!----------------------------------------------------------------------------
!
!*    3.     Computations of the gradients along x
!            -------------------------------------
!
    GDZSDX (:,:)=.FALSE.
    ZDZSDX (:,:)= 0.
!
!*    3.1    loop on jss index (there is no specific computation along j)
!            -----------------
!
    DO JJSS=1,NSSO
!
!* left point mark initialization
!
        GBOUND=.TRUE.
!
!*    3.2    loop on iss index
!            -----------------
!
      DO JISS=1,NSSO
!
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
!*    3.4    dzs/dx term
!            -----------
!
        IMAXI = MIN(JISS+JNEXT-1,NSSO)
!
        ZDZSDX(JISS:IMAXI,JJSS) = ( XSSQO(INEXT,JJSS,IL) - XSSQO(JISS,JJSS,JL)) &
                                    / FLOAT(JNEXT) / ZDXEFF  
!
        GDZSDX(JISS:IMAXI,JJSS) = .TRUE.
!
!
!*    3.5    left data point not on the left of the grid mesh (one more computation)
!            ------------------------------------------------
!
        IF (GBOUND .AND. JISS/=1) THEN
          DO JPREV=1,NSSO
            IF (JISS-JPREV<1) THEN
              IL    = ILEFT(JL)
              IPREV = JISS-JPREV+NSSO
            ELSE
              IL    = JL
              IPREV = JISS-JPREV
            END IF
            ! no left point
             IF (IL==0) EXIT
            ! subgrid data found
            IF (LSSQO(IPREV,JJSS,IL)) EXIT
          END DO

          IF (.NOT. (JPREV>=NSSO+1 .OR. IL==0)) THEN
            ZDZSDX(1:JISS,JJSS) = ( XSSQO(JISS,JJSS,JL) - XSSQO(IPREV,JJSS,IL)) &
                                    / FLOAT(JPREV) / ZDXEFF  
!
            GDZSDX(1:JISS,JJSS) = .TRUE.
          END IF
        END IF
!
!
        GBOUND=.FALSE.
!
!
!*    3.6    end of loop on iss index
!            ------------------------
!
      END DO
!
!*    3.7    end of loop on jss index
!            ------------------------
!
    END DO
!
!----------------------------------------------------------------------------
!
!*    4.     Computations of the gradients along y
!            -------------------------------------
!
    GDZSDY (:,:)=.FALSE.
    ZDZSDY (:,:)= 0.
!
!*    4.1    loop on iss index (there is no specific computation along i)
!            -----------------
!
    DO JISS=1,NSSO
!
!* bottom point mark initialization
!
      GBOUND=.TRUE.
!
!*    4.2    loop on jss index
!            -----------------
!
      DO JJSS=1,NSSO
!
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
          ! no top point
          IF (IL==0) EXIT
          ! subgrid data found
          IF (LSSQO(JISS,INEXT,IL)) EXIT
        END DO
!
!*    4.3.3  none found: end of loop along iss
!                        ---------------------
!
        IF (JNEXT>=NSSO+1) EXIT
!
!*    3.3.4  second point outside of the domain: end of loop along iss
!                                                ---------------------
!
        IF (IL==0) EXIT
!
!*    4.4    dzs/dy term
!            -----------
!
        IMAXJ = MIN(JJSS+JNEXT-1,NSSO)
!
        ZDZSDY(JISS,JJSS:IMAXJ) = ( XSSQO(JISS,INEXT,IL) - XSSQO(JISS,JJSS,JL)) &
                                    / FLOAT(JNEXT) / ZDYEFF 
       !
        GDZSDY(JISS,JJSS:IMAXJ) = .TRUE.
!
!
!*    4.5    bottom data point not on the bottom of the grid mesh (one more computation)
!            ----------------------------------------------------
!
        IF (GBOUND .AND. JJSS/=1) THEN
          DO JPREV=1,NSSO
            IF (JJSS-JPREV<1) THEN
              IL    = IBOTTOM(JL)
              IPREV = JJSS-JPREV+NSSO
            ELSE
              IL    = JL
              IPREV = JJSS-JPREV
            END IF
            ! no left point
             IF (IL==0) EXIT
            ! subgrid data found
            IF (LSSQO(JISS,IPREV,IL)) EXIT
          END DO

          IF (.NOT. (JPREV>=NSSO+1 .OR. IL==0)) THEN
            ZDZSDY(JISS,1:JJSS) = ( XSSQO(JISS,JJSS,JL) - XSSQO(JISS,IPREV,IL)) &
                                    / FLOAT(JPREV) / ZDYEFF 
!
            GDZSDY(JISS,1:JJSS) = .TRUE.
          END IF
        END IF
!
!
        GBOUND=.FALSE.
!
!
!
!*    4.6   end of loop on jss index
!            ------------------------
!
      END DO
!
!*    4.7    end of loop on iss index
!            ------------------------
!
    END DO
!----------------------------------------------------------------------------
!
!*    5.     Computations of tensor terms
!            ----------------------------
!
!
!*    5.1    test to know if term Hxy is computable
!            --------------------------------------
!
!* 2 values are necessary in the grid point to compute anisotropy
!
    IF ( COUNT(GDZSDX(:,:).AND.GDZSDY(:,:)) ==0 ) CYCLE
!
!
!*    5.2    SSO quantities are computable
!            -----------------------------

    OSSO(JL)=.TRUE.
    OSSO_ANIS(JL)=COUNT(GDZSDX(:,:).AND.GDZSDY(:,:))>1
!
!
!*    5.3    term Hxx
!            --------
!
    ZHXX(JL) = SUM(ZDZSDX(:,:)*ZDZSDX(:,:),MASK=GDZSDX(:,:).AND.GDZSDY(:,:))&
                /COUNT(GDZSDX(:,:).AND.GDZSDY(:,:))  
!
!*    5.4    term Hyy
!            --------
!
    ZHYY(JL) = SUM(ZDZSDY(:,:)*ZDZSDY(:,:),MASK=GDZSDX(:,:).AND.GDZSDY(:,:))&
                /COUNT(GDZSDX(:,:).AND.GDZSDY(:,:)) 
!
!*    5.5    term Hxy
!            --------
!
    ZHXY(JL) = SUM(ZDZSDX(:,:)*ZDZSDY(:,:),MASK=GDZSDX(:,:).AND.GDZSDY(:,:))&
                /COUNT(GDZSDX(:,:).AND.GDZSDY(:,:))  
!
!-------------------------------------------------------------------------------
!
!*    6.     Next MESONH grid point
!            ----------------------
!
END DO
!
!-------------------------------------------------------------------------------
!
!*    7.     Diagonalization of the tensor
!            -----------------------------
!
WHERE (OSSO(:))
  ZK(:)=0.5*(ZHXX(:)+ZHYY(:))
  ZL(:)=0.5*(ZHXX(:)-ZHYY(:))
  ZM(:)=     ZHXY(:)
END WHERE
!
!-------------------------------------------------------------------------------
!
!*    8.     S.S.O. characteristics
!            ----------------------
!
!*    8.1    S.S.O. direction of main axis
!            -----------------------------
!
WHERE (OSSO(:) .AND. ZL(:)>1.E-30 )
  XSSO_DIR(:) = 0.5* ATAN(ZM/ZL) * (180./XPI)
END WHERE
!
WHERE (OSSO(:) .AND. ZL(:)<-1.E-30 )
  XSSO_DIR(:) = 0.5* ATAN(ZM/ZL) * (180./XPI) + 90.
END WHERE
!
WHERE (OSSO(:) .AND. ABS(ZL(:))<=1.E-30 )
  XSSO_DIR(:) = 45.
END WHERE
!
WHERE (OSSO(:) .AND. XSSO_DIR(:)>90. )
  XSSO_DIR(:) = XSSO_DIR(:) - 180.
END WHERE
!
!*    8.2    S.S.O. slope
!            ------------
!
WHERE (OSSO(:))
  XSSO_SLOPE(:) = SQRT( ZK+SQRT(ZL*ZL+ZM*ZM) )
END WHERE
!
!*    8.3    S.S.O. anisotropy
!            -----------------
!
WHERE (OSSO_ANIS(:) .AND. (ZK+SQRT(ZL*ZL+ZM*ZM)) >0. )
  XSSO_ANIS(:)=SQRT( MAX(ZK-SQRT(ZL*ZL+ZM*ZM),0.) / (ZK+SQRT(ZL*ZL+ZM*ZM)))
END WHERE
!
WHERE (OSSO_ANIS(:) .AND. (ZK+SQRT(ZL*ZL+ZM*ZM))==0. )
  XSSO_ANIS(:)=1.
END WHERE
IF (LHOOK) CALL DR_HOOK('SSO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SSO
