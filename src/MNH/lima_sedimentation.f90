!MNH_LIC Copyright 2013-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!      ###################################
       MODULE MODI_LIMA_SEDIMENTATION
!      ###################################
!
INTERFACE
      SUBROUTINE LIMA_SEDIMENTATION (HPHASE, KMOMENTS, KID, KSPLITG, PTSTEP, PZZ, PRHODREF,           &
                                     PPABST, PT, PRT_SUM, PCPT, PRS, PCS, PINPR )
!
CHARACTER(1),             INTENT(IN)    :: HPHASE    ! Liquid or solid hydrometeors
INTEGER,                  INTENT(IN)    :: KMOMENTS  ! Number of moments 
INTEGER,                  INTENT(IN)    :: KID       ! Hydrometeor ID
INTEGER,                  INTENT(IN)    :: KSPLITG   !  
REAL,                     INTENT(IN)    :: PTSTEP    ! Time step          
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ       ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF  ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST    ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PT        ! Temperature
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRT_SUM   ! total water mixing ratio
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCPT      ! Cp
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRS       ! m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCS       ! C. source
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPR     ! Instant precip rate
!
END SUBROUTINE LIMA_SEDIMENTATION
END INTERFACE
END MODULE MODI_LIMA_SEDIMENTATION
!
!
!     ######################################################################
      SUBROUTINE LIMA_SEDIMENTATION (HPHASE, KMOMENTS, KID, KSPLITG, PTSTEP, PZZ, PRHODREF,           &
                                     PPABST, PT, PRT_SUM, PCPT, PRS, PCS, PINPR )
!     ######################################################################
!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the sedimentation of any hydrometeor,
!!    also accounting for the transport of heat
!!
!!    METHOD
!!    ------
!!      The sedimentation rates are computed with a time spliting technique: 
!!    an upstream scheme, written as a difference of non-advective fluxes. 
!!    This source term is added to the next coming time step (split-implicit 
!!    process).
!!
!!    AUTHOR
!!    ------
!!      J.-M. Cohard     * Laboratoire d'Aerologie*
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!      S.    Berthet    * Laboratoire d'Aerologie*
!!      B.    Vié        * CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original             15/03/2018
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAM_LIMA_COLD,  ONLY : XLBEXI, XLBI, XDI,                 &
                                  XFSEDRI, XFSEDCI, XFSEDS, XEXSEDS
USE MODD_PARAM_LIMA_MIXED, ONLY : XFSEDG, XEXSEDG, XFSEDH, XEXSEDH
USE MODD_PARAM_LIMA,       ONLY : XCEXVT, XRTMIN, XCTMIN, NSPLITSED, &
                                  XLB, XLBEX, XD, XFSEDR, XFSEDC,    &
                                  XALPHAC, XNUC
USE MODD_CST,              ONLY : XRHOLW, XCL, XCI
USE MODD_PARAMETERS,       ONLY : JPHEXT, JPVEXT
USE MODI_LIMA_FUNCTIONS,   ONLY : COUNTJV
USE MODI_GAMMA,            ONLY : GAMMA_X0D
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER(1),             INTENT(IN)    :: HPHASE    ! Liquid or solid hydrometeors
INTEGER,                  INTENT(IN)    :: KMOMENTS  ! Number of moments 
INTEGER,                  INTENT(IN)    :: KID       ! Hydrometeor ID
INTEGER,                  INTENT(IN)    :: KSPLITG   !  
REAL,                     INTENT(IN)    :: PTSTEP    ! Time step          
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ       ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF  ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST    ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PT        ! Temperature
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRT_SUM   ! total water mixing ratio
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCPT      ! Cp
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRS       ! m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCS       ! C. source
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPR     ! Instant precip rate
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JK, JL, JN                     ! Loop index
INTEGER :: IIB, IIE, IJB, IJE, IKB, IKE   ! Physical domain
INTEGER :: ISEDIM                         ! Case number of sedimentation
!
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) &
                           :: GSEDIM      ! Test where to compute the SED processes
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) &
                           :: ZW,       & ! Work array
                              ZWSEDR,   & ! Sedimentation of MMR
                              ZWSEDC,   & ! Sedimentation of number conc.
                              ZWDT        ! Temperature change
!
REAL, DIMENSION(:), ALLOCATABLE         &
                           :: ZRS,      & ! m.r. source
                              ZCS,      & ! conc. source
                              ZRHODREF, & ! RHO Dry REFerence
                              ZPABST,   & ! Pressure
                              ZT,       & ! Temperature
                              ZZW,      & ! Work array
                              ZZX,      & ! Work array
                              ZZY,      & ! Work array
                              ZLBDA,    & ! Slope parameter
                              ZCC         ! Cunningham corrective term for droplets fall speed
!
INTEGER , DIMENSION(SIZE(PRHODREF)) :: I1,I2,I3 ! Indexes for PACK replacement
!
REAL    :: ZTSPLITG                       ! Small time step for rain sedimentation
REAL    :: ZC                             ! Cpl or Cpi
!
!
!-------------------------------------------------------------------------------
!
! Physical domain
!
IIB=1+JPHEXT
IIE=SIZE(PZZ,1) - JPHEXT
IJB=1+JPHEXT
IJE=SIZE(PZZ,2) - JPHEXT
IKB=1+JPVEXT
IKE=SIZE(PZZ,3) - JPVEXT
!
! Time splitting
!
ZTSPLITG= PTSTEP / FLOAT(NSPLITSED(KID))
!
ZWDT=0.
PINPR(:,:) = 0.
!
PRS(:,:,:) = PRS(:,:,:) * PTSTEP
IF (KMOMENTS==2) PCS(:,:,:) = PCS(:,:,:) * PTSTEP
DO JK = IKB , IKE
   ZW(:,:,JK)=ZTSPLITG/(PZZ(:,:,JK+1)-PZZ(:,:,JK))
END DO
ZW(:,:,IKE+1) = ZW(:,:,IKE)
!
IF (HPHASE=='L') ZC=XCL
IF (HPHASE=='I') ZC=XCI
!
! ################################
! Compute the sedimentation fluxes
! ################################
!
DO JN = 1 ,  NSPLITSED(KID)
  ! Computation only where enough ice, snow, graupel or hail
   GSEDIM(:,:,:) = .FALSE.
   GSEDIM(IIB:IIE,IJB:IJE,IKB:IKE) = PRS(IIB:IIE,IJB:IJE,IKB:IKE)>XRTMIN(KID)
   IF (KMOMENTS==2)  GSEDIM(:,:,:) = GSEDIM(:,:,:) .AND. PCS(:,:,:)>XCTMIN(KID)
   ISEDIM = COUNTJV( GSEDIM(:,:,:),I1(:),I2(:),I3(:))
!
   IF( ISEDIM >= 1 ) THEN
!
      ALLOCATE(ZRHODREF(ISEDIM))
      ALLOCATE(ZPABST(ISEDIM))
      ALLOCATE(ZT(ISEDIM))
      ALLOCATE(ZRS(ISEDIM))
      ALLOCATE(ZCS(ISEDIM))
      ALLOCATE(ZLBDA(ISEDIM)) ; ZLBDA(:) = 1.E10
      ALLOCATE(ZCC(ISEDIM))   ; ZCC(:) = 1.0
      ALLOCATE(ZZW(ISEDIM))   ; ZZW(:) = 0.0
      ALLOCATE(ZZX(ISEDIM))   ; ZZX(:) = 0.0
      ALLOCATE(ZZY(ISEDIM))   ; ZZY(:) = 0.0
!
      DO JL = 1,ISEDIM
         ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
         ZPABST(JL) = PPABST(I1(JL),I2(JL),I3(JL))
         ZT(JL) = PT(I1(JL),I2(JL),I3(JL))
         ZRS(JL) = PRS(I1(JL),I2(JL),I3(JL))
         IF (KMOMENTS==2) ZCS(JL) = PCS(I1(JL),I2(JL),I3(JL))
      END DO
!
      IF (KMOMENTS==1) ZLBDA(:) = XLB(KID) * ( ZRHODREF(:) * ZRS(:) )**XLBEX(KID)
      IF (KMOMENTS==2) ZLBDA(:) = ( XLB(KID)*ZCS(:) / ZRS(:) )**XLBEX(KID)
!
      ZZY(:) = ZRHODREF(:)**(-XCEXVT) * ZLBDA(:)**(-XD(KID))
      ZZW(:) = XFSEDR(KID) * ZRS(:) * ZZY(:) * ZRHODREF(:)
      IF (KMOMENTS==2) ZZX(:) = XFSEDC(KID) * ZCS(:) * ZZY(:) * ZRHODREF(:)

      IF (KID==2) THEN
         ZCC(:) = 0.5*GAMMA_X0D(XNUC+1./XALPHAC)/(GAMMA_X0D(XNUC)*ZLBDA(:))
         ZCC(:) = 1.+1.26*6.6E-8*(101325./ZPABST(:))*(ZT(:)/293.15)/ZCC(:)
         ZZW(:) = ZCC(:) * ZZW(:)
         ZZX(:) = ZCC(:) * ZZX(:)
      END IF

      ZWSEDR(:,:,:) = UNPACK( ZZW(:),MASK=GSEDIM(:,:,:),FIELD=0.0 )
      ZWSEDR(:,:,IKB:IKE) = MIN( ZWSEDR(:,:,IKB:IKE), PRS(:,:,IKB:IKE) * PRHODREF(:,:,IKB:IKE) / ZW(:,:,IKB:IKE) )
      IF (KMOMENTS==2) ZWSEDC(:,:,:) = UNPACK( ZZX(:),MASK=GSEDIM(:,:,:),FIELD=0.0 )
      IF (KMOMENTS==2) ZWSEDC(:,:,IKB:IKE) = MIN( ZWSEDC(:,:,IKB:IKE), PCS(:,:,IKB:IKE) * PRHODREF(:,:,IKB:IKE) / ZW(:,:,IKB:IKE) )
      
      DO JK = IKB , IKE
         PRS(:,:,JK) = PRS(:,:,JK) + ZW(:,:,JK)*    &
              (ZWSEDR(:,:,JK+1)-ZWSEDR(:,:,JK))/PRHODREF(:,:,JK)
         IF (KMOMENTS==2) PCS(:,:,JK) = PCS(:,:,JK) + ZW(:,:,JK)*    &
              (ZWSEDC(:,:,JK+1)-ZWSEDC(:,:,JK))/PRHODREF(:,:,JK)
         ! Heat transport
         PRT_SUM(:,:,JK-1) = PRT_SUM(:,:,JK-1) + ZW(:,:,JK-1)*ZWSEDR(:,:,JK)/PRHODREF(:,:,JK-1)
         PRT_SUM(:,:,JK) = PRT_SUM(:,:,JK) - ZW(:,:,JK)*ZWSEDR(:,:,JK)/PRHODREF(:,:,JK)
         PCPT(:,:,JK-1) = PCPT(:,:,JK-1) + ZC * (ZW(:,:,JK-1)*ZWSEDR(:,:,JK)/PRHODREF(:,:,JK-1))
         PCPT(:,:,JK) = PCPT(:,:,JK) - ZC * (ZW(:,:,JK)*ZWSEDR(:,:,JK)/PRHODREF(:,:,JK))
         ZWDT(:,:,JK) =(PRHODREF(:,:,JK+1)*(1.+PRT_SUM(:,:,JK))*PCPT(:,:,JK)*PT(:,:,JK) + &
              ZW(:,:,JK)*ZWSEDR(:,:,JK+1)*ZC*PT(:,:,JK+1)) / &
              (PRHODREF(:,:,JK+1)*(1.+PRT_SUM(:,:,JK))*PCPT(:,:,JK) + ZW(:,:,JK)*ZWSEDR(:,:,JK+1)*ZC)
         ZWDT(:,:,JK) = ZWDT(:,:,JK) - PT(:,:,JK)
      END DO
      DEALLOCATE(ZRHODREF)
      DEALLOCATE(ZPABST)
      DEALLOCATE(ZT)
      DEALLOCATE(ZRS)
      DEALLOCATE(ZCS)
      DEALLOCATE(ZCC)
      DEALLOCATE(ZLBDA)
      DEALLOCATE(ZZW)
      DEALLOCATE(ZZX)
      DEALLOCATE(ZZY)
      !      
      PINPR(:,:) = PINPR(:,:) + ZWSEDR(:,:,IKB)/XRHOLW/NSPLITSED(KID)                          ! in m/s
      PT(:,:,:) = PT(:,:,:) + ZWDT(:,:,:)
      
   END IF
END DO
!
PRS(:,:,:) = PRS(:,:,:) / PTSTEP
IF (KMOMENTS==2) PCS(:,:,:) = PCS(:,:,:) / PTSTEP
!
END SUBROUTINE LIMA_SEDIMENTATION
!
!-------------------------------------------------------------------------------
