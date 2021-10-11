!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE FSSO(U, UG, USS)
!     #########################
!
!!*FSSO  computes the Fractional SSO slopes and aspect
!!
!!
!!    METHOD
!!    ------
!!    See Lott and Miller, 1997, QJRMS 101-127
!!    and Senkova et al, 2007
!!   
!!    AUTHOR
!!    ------
!!
!!    A.Mary        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    24/03/2015
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t 
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n,  ONLY : SSO_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XPI
USE MODD_PGDWORK,        ONLY : NFSSO, XFSSQO, NFSSQO, XFLATRAD
USE MODD_PGD_GRID,       ONLY : NL, CGRID
!
USE MODI_GET_MESH_DIM
USE MODI_GET_ADJACENT_MESHES
!
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_READ_AND_SEND_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SSO_t), INTENT(INOUT) :: USS
!
!*    0.2    Declaration of indexes
!            ----------------------
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ILEFT, IRIGHT, ITOP, IBOTTOM
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFSSQO
INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: IFSSQO
!
REAL, DIMENSION(:), ALLOCATABLE :: ZDX, ZDY, ZMESH_SIZE   ! grid mesh size in x/y direction
REAL, DIMENSION(:), ALLOCATABLE :: ZZS, ZJPDIR ! compass (cf. XJPDIR) (rad)
REAL, DIMENSION(:), ALLOCATABLE :: ZSLOPE_SAVE, ZASPECT_SAVE, ZAVG_SLO
REAL, DIMENSION(:,:), ALLOCATABLE :: ZFRAC_DIR, ZSLOPE_DIR
!
INTEGER, DIMENSION(USS%NSECTORS) :: IC    ! counting index
LOGICAL, DIMENSION(0:NFSSO+1,0:NFSSO+1) :: GLSSQO  ! local flag array
REAL,    DIMENSION(0:NFSSO+1,0:NFSSO+1) :: ZSSQO   ! local work array
!
REAL, DIMENSION(USS%NSECTORS,2)  :: ZSECTORS ! aspect sectors delimitation :
                                             ! sector 1 being centered on North,
                                             ! with clockwise sense
!
INTEGER :: IFSSO1       ! = NFSSO + 1
INTEGER :: IFSSO2       ! = NFSSO**2
REAL    :: Z1IFSSO2     ! = 1/NFSSO**2
!
REAL    :: ZM           ! local subscale M derivatives
REAL    :: ZK           ! local subscale K derivatives
REAL    :: ZL           ! local subscale L derivatives
REAL    :: ZD           ! local subscale D derivatives
                        ! cf. Senkova & Rontu 2013
REAL    :: ZDHDX        ! local derivatives
REAL    :: ZDHDY        ! local derivatives
REAL    :: ZDXNFSSO     ! subscale resolution
REAL    :: ZDYNFSSO     ! subscale resolution
REAL    :: ZP,Z0,ZN     ! local shortcuts
REAL    :: ZASPECT      ! local orographic parameters 
REAL    :: ZSLOPE       ! local orographic parameters
!
REAL :: ZSECTOR_ANGLE   ! angle of one sector
!
INTEGER :: IT, IB, IL, IR
!
INTEGER :: JI, JJ, JK     ! indexes
INTEGER :: JL           ! loop index on grid meshs
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    1.     Initializations
!            ---------------
!
IF (LHOOK) CALL DR_HOOK('FSSO',0,ZHOOK_HANDLE)
!
IFSSO1 = NFSSO+1
IFSSO2 = NFSSO*NFSSO
Z1IFSSO2 = 1./IFSSO2
!
! definition of sectors
ZSECTOR_ANGLE = 2.*XPI/USS%NSECTORS
ZSECTORS(1,1) = -ZSECTOR_ANGLE/2.             ! first sector is centered on 0.0 rad
ZSECTORS(1,2) = ZSECTORS(1,1) + ZSECTOR_ANGLE ! then rotate by sector
DO JI=2,USS%NSECTORS
  ZSECTORS(JI,1) = ZSECTORS(JI-1,2)
  ZSECTORS(JI,2) = ZSECTORS(JI,1) + ZSECTOR_ANGLE
ENDDO
!
USS%XAVG_SLO  (:) = 0.
USS%XSLOPE    (:) = 0.
USS%XASPECT   (:) = 0.
USS%XFRAC_DIR (:,:) = 0.
USS%XSLOPE_DIR(:,:) = 0.
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ZZS   (U%NDIM_FULL))
  ALLOCATE(ZJPDIR(U%NDIM_FULL))
  ALLOCATE(ZFSSQO(U%NDIM_FULL,NFSSO,NFSSO))
  ALLOCATE(IFSSQO(U%NDIM_FULL,NFSSO,NFSSO))
ELSE
  ALLOCATE(ZZS   (0))
  ALLOCATE(ZJPDIR(0))
  ALLOCATE(ZFSSQO(0,0,0))
  ALLOCATE(IFSSQO(0,0,0))
ENDIF
!
CALL GATHER_AND_WRITE_MPI(USS%XAVG_ZS,ZZS)
CALL GATHER_AND_WRITE_MPI(UG%XJPDIR,ZJPDIR) 
CALL GATHER_AND_WRITE_MPI(XFSSQO,ZFSSQO)
CALL GATHER_AND_WRITE_MPI(NFSSQO,IFSSQO)
!
IF (NRANK==NPIO) THEN
  !
  ALLOCATE(ZDX(U%NDIM_FULL),ZDY(U%NDIM_FULL))
  ALLOCATE(ZMESH_SIZE(U%NDIM_FULL))
  CALL GET_MESH_DIM(CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,UG%XGRID_FULL_PAR,ZDX,ZDY,ZMESH_SIZE)
  DEALLOCATE(ZMESH_SIZE)

  !
  !
  !*    1.3    Left, top, right and bottom adjacent gris meshes
  !            ------------------------------------------------
  !
  ALLOCATE(ILEFT(U%NDIM_FULL),IRIGHT(U%NDIM_FULL),ITOP(U%NDIM_FULL),IBOTTOM(U%NDIM_FULL))
  CALL GET_ADJACENT_MESHES(CGRID,UG%NGRID_FULL_PAR,U%NDIM_FULL,UG%XGRID_FULL_PAR,&
                           ILEFT,IRIGHT,ITOP,IBOTTOM)
  !
  ALLOCATE(ZSLOPE_SAVE(U%NDIM_FULL),ZASPECT_SAVE(U%NDIM_FULL),ZAVG_SLO(U%NDIM_FULL))
  ALLOCATE(ZFRAC_DIR(U%NDIM_FULL,USS%NSECTORS),ZSLOPE_DIR(U%NDIM_FULL,USS%NSECTORS))
  ZSLOPE_SAVE (:) = 0.
  ZASPECT_SAVE(:) = 0.
  ZAVG_SLO  (:) = 0.
  ZSLOPE_DIR(:,:) = 0.
  ZFRAC_DIR (:,:) = 0.
  !
  ! deg2radians conversions
  ZJPDIR(:) = ZJPDIR(:) * XPI/180.
  !
  !----------------------------------------------------------------------------
  !
  !*    2.     Loop on grid points
  !            -------------------
  !
  DO JL=1,U%NDIM_FULL
    !
    IT = ITOP   (JL)
    IB = IBOTTOM(JL)
    !
    ! 2.1 - Slope and aspect at mesh scale
    ! ------------------------------------
    ! dH/dY
    IF ( IB/=0 .AND. IT/=0 ) THEN
  
      IF ( ZZS(IT)<XUNDEF .AND. ZZS(IB)<XUNDEF .AND. ZZS(JL)<XUNDEF ) THEN
  
        ZDHDY = (ZZS(IT)-ZZS(JL))/(ZDY(IT)+ZDY(JL)) &
              + (ZZS(JL)-ZZS(IB))/(ZDY(IB)+ZDY(JL))
  
      ELSE
  
        ZP = ZZS(IT)
        Z0 = ZZS(JL)
        ZN = ZZS(IB)
  
        IF (ZP==XUNDEF) ZP = 0.
        IF (Z0==XUNDEF) Z0 = 0.
        IF (ZN==XUNDEF) ZN = 0.
  
        ZDHDY = (ZP-Z0)/(ZDY(IT)+ZDY(JL)) + (Z0-ZN)/(ZDY(IB)+ZDY(JL))
  
      ENDIF
  
    ELSEIF ( IT==0 ) THEN
  
      IF (ZZS(IB)<XUNDEF .AND. ZZS(JL)<XUNDEF) THEN
        ZDHDY = 2.*(ZZS(JL)-ZZS(IB))/(ZDY(IB)+ZDY(JL))
      ELSEIF (ZZS(IB)==XUNDEF .AND. ZZS(JL)==XUNDEF) THEN
        ZDHDY = 0.
      ELSEIF (ZZS(IB)==XUNDEF) THEN
        ZDHDY = 2.*(ZZS(JL)-0.)/(ZDY(IB)+ZDY(JL))
      ELSEIF (ZZS(JL)==XUNDEF) THEN
        ZDHDY = 2.*(0.-ZZS(IB))/(ZDY(IB)+ZDY(JL))
      ENDIF
  
    ELSEIF (IB==0) THEN
  
      IF (ZZS(IT)<XUNDEF .AND. ZZS(JL)<XUNDEF) THEN
        ZDHDY = 2.*(ZZS(IT)-ZZS(JL))/(ZDY(IT)+ZDY(JL))
      ELSEIF (ZZS(IT)==XUNDEF .AND. ZZS(JL)==XUNDEF) THEN
        ZDHDY = 0.
      ELSEIF (ZZS(IT)==XUNDEF) THEN
        ZDHDY = 2.*(0.-ZZS(JL))/(ZDY(IT)+ZDY(JL))
      ELSEIF (ZZS(JL)==XUNDEF) THEN
        ZDHDY = 2.*(ZZS(IT)-0.)/(ZDY(IT)+ZDY(JL))
      ENDIF
  
    ENDIF
  
    IL = ILEFT (JL)
    IR = IRIGHT(JL)
    
    ! dH/dX
    IF ( IL/=0 .AND. IR/=0 ) THEN
  
      IF ( ZZS(IR)<XUNDEF .AND. ZZS(IL)<XUNDEF .AND. ZZS(JL)<XUNDEF ) THEN
  
        ZDHDX = (ZZS(IR)-ZZS(JL))/(ZDX(IR)+ZDX(JL)) &
              + (ZZS(JL)-ZZS(IL))/(ZDX(IL)+ZDX(JL))
  
      ELSE
  
        ZP = ZZS(IR)
        Z0 = ZZS(JL)
        ZN = ZZS(IL)
  
        IF (ZP==XUNDEF) ZP = 0.
        IF (Z0==XUNDEF) Z0 = 0.
        IF (ZN==XUNDEF) ZN = 0.
        
        ZDHDX = (ZP-Z0)/(ZDX(IR)+ZDX(JL)) + (Z0-ZN)/(ZDX(IL)+ZDX(JL))
  
      ENDIF
  
    ELSEIF (IL==0) THEN
  
      IF (ZZS(IR)<XUNDEF .AND. ZZS(JL)<XUNDEF) THEN
        ZDHDX = 2.*(ZZS(IR)-ZZS(JL))/(ZDX(IR)+ZDX(JL))
      ELSEIF (ZZS(IR)==XUNDEF .AND. ZZS(JL)==XUNDEF) THEN
        ZDHDX = 0.
      ELSEIF (ZZS(IR)==XUNDEF) THEN
        ZDHDX = 2.*(0.-ZZS(JL))/(ZDX(IR)+ZDX(JL))
      ELSEIF (ZZS(JL)==XUNDEF) THEN
        ZDHDX = 2.*(ZZS(IR)-0.)/(ZDX(IR)+ZDX(JL))
      ENDIF
  
    ELSEIF (IR==0) THEN
  
      IF (ZZS(IL)/=XUNDEF .AND. ZZS(JL)/=XUNDEF) THEN
        ZDHDX = 2.*(ZZS(JL)-ZZS(IL))/(ZDX(IL)+ZDX(JL))
      ELSEIF (ZZS(IL)==XUNDEF .AND. ZZS(JL)==XUNDEF) THEN
        ZDHDX = 0.
      ELSEIF (ZZS(IL)==XUNDEF) THEN
        ZDHDX = 2.*(ZZS(JL)-0.)/(ZDX(IL)+ZDX(JL))
      ELSEIF (ZZS(JL)==XUNDEF) THEN
        ZDHDX = 2.*(0.-ZZS(IL))/(ZDX(IL)+ZDX(JL))
      ENDIF
  
    ENDIF
  
  
    ! local derivatives (cf. Rontu & Sattler 2013 // hirlam.org)
    ZM = ZDHDX * ZDHDY
    ZK = 0.5 * (ZDHDX*ZDHDX + ZDHDY*ZDHDY)
    ZL = 0.5 * (ZDHDX*ZDHDX - ZDHDY*ZDHDY)
    ZD = SQRT(ZL*ZL + ZM*ZM)
  
    ! first compute the direction of maximum gradient w/r to X axis
    IF (ABS(ZM)>1.E-12) THEN
      ZASPECT = ATAN((ZD - ZL)/ZM)
      IF (ZDHDX>0.) THEN
        ZASPECT = ZASPECT + XPI
      ENDIF
    ELSE
      IF (ABS(ZDHDX)<1.E-6) THEN
        IF (ZDHDY>=0.) THEN
          ZASPECT = -XPI/2.
        ELSE
          ZASPECT = XPI/2.
        ENDIF
      ELSE
        IF (ZDHDX>=0.) THEN
          ZASPECT = XPI
        ELSE
          ZASPECT = 0.
        ENDIF
      ENDIF
    ENDIF
  
    ! then:
    ! - (keeping trigonometric sign for angles)
    ! - set North (Y) to 0, instead of X (-XPI/2)
    ! - and rotate to true Geographic North (-ZJPDIR)
    ZASPECT = ZASPECT - XPI/2. - ZJPDIR(JL)
    ! - shift to clockwise sense
    ZASPECT = 2.*XPI - ZASPECT
    ! finally, shift in the expected window
    ZASPECT = MOD(ZASPECT, 2.*XPI)
    IF (ZASPECT<ZSECTORS(1,1)) THEN
      ZASPECT = ZASPECT + 2.*XPI
    ELSEIF (ZASPECT>=ZSECTORS(USS%NSECTORS,2)) THEN
      ZASPECT = ZASPECT - 2.*XPI
    ENDIF
    ! compute slope
    ZSLOPE = ATAN(SQRT(ZK+ZD))
  
   
    ! save
    ZSLOPE_SAVE (JL) = ZSLOPE
    ZASPECT_SAVE(JL) = ZASPECT
  
  
    ! 2.2 - SSO (or not) slope and aspect for orographic radiation (HLORORAD)
    ! -----------------------------------------------------------------------
    IF (ANY(IFSSQO(JL,:,:)==0)) THEN ! we cannot compute subscale => use mesh scale values computed above
  
      DO JK=1,USS%NSECTORS
        IF (ZASPECT>=ZSECTORS(JK,1) .AND. ZASPECT<ZSECTORS(JK,2) .AND. ZSLOPE>XFLATRAD) THEN
          ZFRAC_DIR (JL,JK) = 1.
          ZSLOPE_DIR(JL,JK) = ZSLOPE
          EXIT
        ENDIF
      ENDDO
      ZAVG_SLO(JL) = ZSLOPE
  
    ELSE ! (ALL(NSSQO(:,:,JL)/=0)) => we can compute subscale (NFSSO^2 subboxes)
  
      ! set local SSO neighbouring
      ZSSQO (1:NFSSO,1:NFSSO) = ZFSSQO(JL,:,:)
      GLSSQO(1:NFSSO,1:NFSSO) = IFSSQO(JL,:,:)/=0
      IF (IL/=0) THEN
        ZSSQO (0,1:NFSSO) = ZFSSQO(IL,NFSSO,:)
        GLSSQO(0,1:NFSSO) = IFSSQO(IL,NFSSO,:)/=0
      ELSE
        GLSSQO(0,1:NFSSO) = .FALSE.
      ENDIF
      IF (IR/=0) THEN
        ZSSQO (IFSSO1,1:NFSSO) = ZFSSQO(IR,1,:)
        GLSSQO(IFSSO1,1:NFSSO) = IFSSQO(IR,1,:)/=0
      ELSE
        GLSSQO(IFSSO1,1:NFSSO) = .FALSE.
      ENDIF
      IF (IT/=0) THEN
        ZSSQO (1:NFSSO,IFSSO1) = ZFSSQO(IT,:,1)
        GLSSQO(1:NFSSO,IFSSO1) = IFSSQO(IT,:,1)/=0
      ELSE
        GLSSQO(1:NFSSO,IFSSO1) = .FALSE.
      ENDIF
      IF (IB/=0) THEN
        ZSSQO (1:NFSSO,0) = ZFSSQO(IB,:,NFSSO)
        GLSSQO(1:NFSSO,0) = IFSSQO(IB,:,NFSSO)/=0
      ELSE
        GLSSQO(1:NFSSO,0) = .FALSE.
      ENDIF
  
      IC(:) = 0
      ZDXNFSSO = ZDX(JL)/NFSSO
      ZDYNFSSO = ZDY(JL)/NFSSO
  
      DO JI=1,NFSSO
        DO JJ=1,NFSSO
  
          ! dH/dX
          IF (GLSSQO(JI-1,JJ) .AND. GLSSQO(JI+1,JJ)) THEN
            ZDHDX = (ZSSQO(JI+1,JJ)-ZSSQO(JI-1,JJ))/(2*ZDXNFSSO)
          ELSEIF (GLSSQO(JI-1,JJ)) THEN
            ZDHDX = (ZSSQO(JI,JJ)-ZSSQO(JI-1,JJ))/ZDXNFSSO
          ELSEIF (GLSSQO(JI+1,JJ)) THEN
            ZDHDX = (ZSSQO(JI+1,JJ)-ZSSQO(JI,JJ))/ZDXNFSSO
          ENDIF
          ! dH/dY
          IF (GLSSQO(JI,JJ-1) .AND. GLSSQO(JI,JJ+1)) THEN
            ZDHDY = (ZSSQO(JI,JJ+1)-ZSSQO(JI,JJ-1))/(2*ZDYNFSSO)
          ELSEIF (GLSSQO(JI,JJ-1)) THEN
            ZDHDY = (ZSSQO(JI,JJ)-ZSSQO(JI,JJ-1))/ZDYNFSSO
          ELSEIF (GLSSQO(JI,JJ+1)) THEN
            ZDHDY = (ZSSQO(JI,JJ+1)-ZSSQO(JI,JJ))/ZDYNFSSO
          ENDIF
  
          ! local derivatives (cf. Rontu & Sattler 2013 // hirlam.org)
          ZM = ZDHDX * ZDHDY
          ZK = 0.5 * (ZDHDX*ZDHDX + ZDHDY*ZDHDY)
          ZL = 0.5 * (ZDHDX*ZDHDX - ZDHDY*ZDHDY)
          ZD = SQRT(ZL*ZL + ZM*ZM)
  
          ! first compute the direction of maximum gradient w/r to X axis
          IF (ABS(ZM)>1.E-12) THEN
            ZASPECT = ATAN((ZD - ZL)/ZM)
            IF (ZDHDX>0.) THEN
              ZASPECT = ZASPECT + XPI
            ENDIF
          ELSE
            IF (ABS(ZDHDX)<1.E-6) THEN
              IF (ZDHDY>=0.) THEN
                ZASPECT = -XPI/2.
              ELSE
                ZASPECT = XPI/2.
              ENDIF
            ELSE
              IF (ZDHDX>=0.) THEN
                ZASPECT = XPI
              ELSE
                ZASPECT = 0.
              ENDIF
            ENDIF
          ENDIF
  
          ! then:
          ! - (keeping trigonometric sign for angles)
          ! - set North (Y) to 0, instead of X (-XPI/2)
          ! - and rotate to true Geographic North (-ZJPDIR)
          ZASPECT = ZASPECT - XPI/2. - ZJPDIR(JL)
          ! - shift to clockwise sense
          ZASPECT = 2.*XPI - ZASPECT
          ! finally, shift in the expected window
          ZASPECT = MOD(ZASPECT,2*XPI)
          IF (ZASPECT<ZSECTORS(1,1)) THEN
            ZASPECT = ZASPECT + 2*XPI
          ELSEIF (ZASPECT>=ZSECTORS(USS%NSECTORS,2)) THEN
            ZASPECT = ZASPECT - 2*XPI
          ENDIF
  
          ! compute slope
          ZSLOPE = ATAN(SQRT(ZK+ZD))
          ZAVG_SLO(JL) = ZAVG_SLO(JL) + ZSLOPE
  
          DO JK=1,USS%NSECTORS
            IF (ZASPECT>=ZSECTORS(JK,1) .AND. ZASPECT<ZSECTORS(JK,2) &
                .AND. ZSLOPE>XFLATRAD) THEN
              ZFRAC_DIR (JL,JK) = ZFRAC_DIR (JL,JK) + Z1IFSSO2
              ZSLOPE_DIR(JL,JK) = ZSLOPE_DIR(JL,JK) + ZSLOPE
              IC(JK) = IC(JK) + 1
              EXIT
            ENDIF
          ENDDO
  
        ENDDO
  
      ENDDO
  
      ZAVG_SLO(JL) = ZAVG_SLO(JL) / IFSSO2
  
      DO JK=1,USS%NSECTORS
        IF (IC(JK)>0) THEN
          ZSLOPE_DIR(JL,JK) = ZSLOPE_DIR(JL,JK)/IC(JK)
        ENDIF
      ENDDO
    ENDIF
    !
  ENDDO
  !
  DEALLOCATE(ZDX,ZDY)
  DEALLOCATE(ILEFT,IRIGHT,ITOP,IBOTTOM)
  !
ELSE
  !
  ALLOCATE(ZSLOPE_SAVE(0),ZASPECT_SAVE(0),ZAVG_SLO(0),ZFRAC_DIR(0,0),ZSLOPE_DIR(0,0))
  !
ENDIF
!
DEALLOCATE(ZZS,ZJPDIR,ZFSSQO,IFSSQO)
!
CALL READ_AND_SEND_MPI(ZSLOPE_SAVE,USS%XSLOPE)
CALL READ_AND_SEND_MPI(ZASPECT_SAVE,USS%XASPECT)
CALL READ_AND_SEND_MPI(ZFRAC_DIR,USS%XFRAC_DIR)
CALL READ_AND_SEND_MPI(ZSLOPE_DIR,USS%XSLOPE_DIR)
CALL READ_AND_SEND_MPI(ZAVG_SLO,USS%XAVG_SLO)
!
DEALLOCATE(ZSLOPE_SAVE,ZASPECT_SAVE,ZAVG_SLO,ZFRAC_DIR,ZSLOPE_DIR)
!
IF (LHOOK) CALL DR_HOOK('FSSO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE FSSO
