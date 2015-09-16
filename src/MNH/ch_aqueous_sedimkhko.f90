!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!      ################################
       MODULE MODI_CH_AQUEOUS_SEDIMKHKO
!      ################################
!
INTERFACE
      SUBROUTINE CH_AQUEOUS_SEDIMKHKO (PTSTEP, PZZ, PRHODREF, PRHODJ,             &
                                       PRRT, PRRS, PCRT, PCRS, PSVT, PRSVS  )
!
REAL,                     INTENT(IN)    :: PTSTEP  ! Time step          
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRS    ! Rain water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCRT    ! Rain water C. at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCRS    ! Rain water C. source
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT    ! Precip. aq. species at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRSVS   ! Precip. aq. species source
!
END SUBROUTINE CH_AQUEOUS_SEDIMKHKO
END INTERFACE
END MODULE MODI_CH_AQUEOUS_SEDIMKHKO
!
!     #############################################################################
      SUBROUTINE CH_AQUEOUS_SEDIMKHKO (PTSTEP, PZZ, PRHODREF, PRHODJ,             &
                                       PRRT, PRRS, PCRT, PCRS, PSVT, PRSVS  )
!     #############################################################################
!
!!****  * -  compute the explicit microphysical sources 
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the sedimentation of chemical
!!   species in the raindrops for the KHKO cloud microphysical schemes.
!!      The sedimentation rates are computed with a time spliting technique: 
!!    an upstream scheme, written as a difference of non-advective fluxes. 
!!    This source term is added to the next coming time step (split-implicit 
!!    process). see rain_khko.f90
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      None
!!     
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS
!!          JPHEXT       : Horizontal external points number
!!          JPVEXT       : Vertical external points number
!!      Module MODD_CONF :
!!          CCONF configuration of the model for the first time step
!!
!!    REFERENCE
!!    ---------
!!      Book1 of the documentation ( routine CH_AQUEOUS_SEDIMC2R2 )
!!
!!    AUTHOR
!!    ------
!!      M. Leriche & J.P. Pinty      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/11/08
!!    J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CST
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_RAIN_C2R2_DESCR, ONLY : XRTMIN, XCTMIN
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
REAL,                     INTENT(IN)    :: PTSTEP  ! Time step          
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRS    ! Rain water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCRT    ! Rain water C. at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCRS    ! Rain water C. source
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT    ! Precip. aq. species at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRSVS   ! Precip. aq. species source
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JK            ! Vertical loop index for the rain sedimentation 
INTEGER :: JN            ! Temporal loop index for the rain sedimentation
INTEGER :: IIB           !  Define the domain where is 
INTEGER :: IIE           !  the microphysical sources have to be computed
INTEGER :: IJB           ! 
INTEGER :: IJE           !
INTEGER :: IKB           ! 
INTEGER :: IKE           !
!
REAL    :: ZTSPLITR      ! Small time step for rain sedimentation
!
INTEGER :: ISEDIM ! Case number of sedimentation
LOGICAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3)) &
                                :: GSEDIM ! where to compute the SED processes
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZZRRS     ! rain water m.r.source for sedim
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZZCRS     ! rain water C source for sedim
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZRRS  ! Rain water m.r. source phys.tendency (*dt)
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZCRS  ! Rain water C source phys.tendency
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZW     ! work array
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZMVRR, ZVRR ! sedimentation velocity for rain
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZWSED ! sedimentation fluxes
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZRR_SEDIM       ! Drain/Dt sur ZTSPLIT
REAL,    DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3))   &
                                :: ZSV_SEDIM_FACT  ! Cumul des Dsv/DT
REAL, DIMENSION(:), ALLOCATABLE :: ZZZRRS  ! Rain water m.r. source
!
REAL, DIMENSION(:), ALLOCATABLE :: ZRHODREF, & ! RHO Dry REFerence
                                   ZZVRR,    & ! sedimentation velocity for rain
                                   ZZW1  ! Work array
REAL,  SAVE                     :: ZRTMIN, ZCTMIN
!
REAL                            :: ZVTRMAX, ZDZMIN, ZT
LOGICAL, SAVE                   :: GSFIRSTCALL = .TRUE.
INTEGER, SAVE                   :: ISPLITR
!
INTEGER , DIMENSION(SIZE(GSEDIM)) :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                           :: JL       ! and PACK intrinsics
!  
!-------------------------------------------------------------------------------
!
!*       1.     COMPUTE THE LOOP BOUNDS
!   	        -----------------------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB=1+JPVEXT
IKE=SIZE(PZZ,3) - JPVEXT
!
!-------------------------------------------------------------------------------
!
!!*       2.     TRANSFORMATION INTO PHYSICAL TENDENCIES
!               ---------------------------------------
!
ZRRS(:,:,:) = PRRS(:,:,:) / PRHODJ(:,:,:)
ZCRS(:,:,:) = PCRS(:,:,:) / PRHODJ(:,:,:)
!
!-------------------------------------------------------------------------------
!
!*       3.     COMPUTE THE SEDIMENTATION (RS) SOURCE
!	        -------------------------------------
!
!*       3.1    splitting factor for high Courant number C=v_fall*(del_Z/del_T)
!  
firstcall : IF (GSFIRSTCALL) THEN
  GSFIRSTCALL = .FALSE.
  ZVTRMAX = 30.   !cf. ini_rain_c2r2.f90
  ZDZMIN = MINVAL(PZZ(IIB:IIE,IJB:IJE,IKB+1:IKE+1)-PZZ(IIB:IIE,IJB:IJE,IKB:IKE))
  ISPLITR = 1
  SPLIT : DO
    ZT = PTSTEP / FLOAT(ISPLITR)
    IF ( ZT * ZVTRMAX / ZDZMIN .LT. 1.) EXIT SPLIT
    ISPLITR = ISPLITR + 1
  END DO SPLIT
  ZRTMIN = XRTMIN(3) / PTSTEP
  ZCTMIN = XCTMIN(3) / PTSTEP
END IF firstcall
!
!*       3.2    compute the sedimentation velocities for rain
!
ZMVRR(:,:,:) = 0.
ZVRR(:,:,:) = 0.
WHERE (PCRT(:,:,:) > XCTMIN(3) .and. PRRT(:,:,:)>XRTMIN(3) )
  ZMVRR(:,:,:) = ((3. * PRHODREF(:,:,:)*PRRT(:,:,:))/   &
                  (4. * XPI *XRHOLW*PCRT(:,:,:)))**0.333 ! in m
  ZVRR(:,:,:) = 0.012 * 1.0E6 * ZMVRR(:,:,:) - 0.2 ! velocity for mixing ratio
END WHERE
WHERE (ZVRR(:,:,:) .lt. 0.0)
  ZVRR(:,:,:) = 0.0
END WHERE
!
!*       3.3    time splitting loop initialization
!
ZTSPLITR = PTSTEP / FLOAT(ISPLITR)       ! Small time step
!
!*       3.4    compute the fluxes
! 
!  optimization by looking for locations where
!  the precipitating fields are larger than a minimal value only !!!
!
ZZRRS(:,:,:) = 0.0
ZZRRS(:,:,:) = ZRRS(:,:,:) - PRRT(:,:,:) / PTSTEP
ZRRS(:,:,:) = PRRT(:,:,:) / PTSTEP
ZZCRS(:,:,:) = 0.0
ZZCRS(:,:,:) = ZCRS(:,:,:) - PCRT(:,:,:) / PTSTEP
ZCRS(:,:,:) = PCRT(:,:,:) / PTSTEP
ZSV_SEDIM_FACT(:,:,:) = 1.0
DO JN = 1 , ISPLITR
! 
  GSEDIM(:,:,:) = .FALSE.
  GSEDIM(IIB:IIE,IJB:IJE,IKB:IKE) = ZRRS(IIB:IIE,IJB:IJE,IKB:IKE) > ZRTMIN .AND. &
                                    ZCRS(IIB:IIE,IJB:IJE,IKB:IKE) > ZCTMIN
  ISEDIM = COUNTJV( GSEDIM(:,:,:),I1(:),I2(:),I3(:))
  IF( ISEDIM >= 1 ) THEN
    IF( JN==1 ) THEN
      ZRRS(:,:,:) = ZRRS(:,:,:) + ZZRRS(:,:,:) / ISPLITR
      ZRRS(:,:,:) = ZRRS(:,:,:) * PTSTEP
      ZCRS(:,:,:) = ZCRS(:,:,:) + ZZCRS(:,:,:) / ISPLITR
      ZCRS(:,:,:) = ZCRS(:,:,:) * PTSTEP
      ZW(:,:,:) = 0.0
      DO JK = IKB , IKE-1
        ZW(:,:,JK) =ZTSPLITR*2./(PRHODREF(:,:,JK)*(PZZ(:,:,JK+2)-PZZ(:,:,JK)))
      END DO
      ZW(:,:,IKE)  =ZTSPLITR/(PRHODREF(:,:,IKE)*(PZZ(:,:,IKE+1)-PZZ(:,:,IKE)))
    ELSE
      ZRRS(:,:,:) = ZRRS(:,:,:) + ZZRRS(:,:,:) * ZTSPLITR
      ZCRS(:,:,:) = ZCRS(:,:,:) + ZZCRS(:,:,:) * ZTSPLITR
    END IF
    ALLOCATE(ZRHODREF(ISEDIM))
    ALLOCATE(ZZZRRS(ISEDIM))
    ALLOCATE(ZZVRR(ISEDIM))
    DO JL=1,ISEDIM
      ZRHODREF(JL) =  PRHODREF(I1(JL),I2(JL),I3(JL))
      ZZZRRS(JL) = ZRRS(I1(JL),I2(JL),I3(JL))
      ZZVRR(JL) = ZVRR(I1(JL),I2(JL),I3(JL))
    ENDDO
    ALLOCATE(ZZW1(ISEDIM)) ; ZZW1(:) = 0.0
!
!*      for drizzle
!
    ZZW1(:) = ZZVRR(:) * ZZZRRS(:) * ZRHODREF(:)
    ZWSED(:,:,:) = UNPACK( ZZW1(:),MASK=GSEDIM(:,:,:),FIELD=0.0 )
    DO JK = IKB , IKE
      ZRR_SEDIM(:,:,JK) = ZW(:,:,JK)*(ZWSED(:,:,JK+1)-ZWSED(:,:,JK))
    END DO
    ZRRS(:,:,:) = ZRRS(:,:,:) + ZRR_SEDIM(:,:,:)
!
    ZZW1(:) = ZZVRR(:) * ZRHODREF(:)
    ZWSED(:,:,:) = UNPACK( ZZW1(:),MASK=GSEDIM(:,:,:),FIELD=0.0 )
    ZRR_SEDIM(:,:,:) = 0.0
    DO JK = IKB , IKE
      ZRR_SEDIM(:,:,JK) = ZW(:,:,JK)*(ZWSED(:,:,JK+1)-ZWSED(:,:,JK))
    END DO
    DEALLOCATE(ZRHODREF)
    DEALLOCATE(ZZZRRS)
    DEALLOCATE(ZZVRR)
    DEALLOCATE(ZZW1)
    ZSV_SEDIM_FACT(:,:,:) =   ZSV_SEDIM_FACT(:,:,:) * (1.0 + ZRR_SEDIM(:,:,:))
!!                       (1.0 + ZRR_SEDIM(:,:,:)/MAX(ZZRRS(:,:,:),XRTMIN_AQ))
  END IF      
END DO
!
! Apply the rain sedimentation rate to the WR_xxx aqueous species
!
DO JL= 1, SIZE(PRSVS,4)
  PRSVS(:,:,:,JL) = MAX( 0.0,ZSV_SEDIM_FACT(:,:,:)*PRSVS(:,:,:,JL) )
END DO
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
  FUNCTION COUNTJV(LTAB,I1,I2,I3) RESULT(IC)
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
!
LOGICAL, DIMENSION(:,:,:) :: LTAB ! Mask
INTEGER, DIMENSION(:) :: I1,I2,I3 ! Used to replace the COUNT and PACK
INTEGER :: IC
INTEGER :: JI,JJ,JK
!  
!-------------------------------------------------------------------------------
!
IC = 0
DO JK = 1,SIZE(LTAB,3)
  DO JJ = 1,SIZE(LTAB,2)
    DO JI = 1,SIZE(LTAB,1)
      IF( LTAB(JI,JJ,JK) ) THEN
        IC = IC +1
        I1(IC) = JI
        I2(IC) = JJ
        I3(IC) = JK
      END IF
    END DO
  END DO
END DO
!
END FUNCTION COUNTJV
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CH_AQUEOUS_SEDIMKHKO
