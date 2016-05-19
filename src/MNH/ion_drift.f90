!     #####################
      MODULE MODI_ION_DRIFT
!     #####################

INTERFACE
!
      SUBROUTINE ION_DRIFT(PDRIFTP, PDRIFTM, PSVT, PRHODREF, PRHODJ,  &
                           HLBCX, HLBCY, KTCOUNT, PTSTEP, HDRIFT,     &
                           HUVW_ADV_SCHEME)
!
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN)  :: HLBCX,HLBCY
CHARACTER(LEN=3), INTENT(IN)                :: HDRIFT
REAL, DIMENSION(:,:,:),       INTENT(INOUT) :: PDRIFTP, PDRIFTM
REAL, DIMENSION(:,:,:,:),     INTENT(INOUT) :: PSVT
REAL, DIMENSION(:,:,:),       INTENT(IN)    :: PRHODREF, PRHODJ
INTEGER,                  INTENT(IN)   :: KTCOUNT  ! Temporal loop counter
REAL,                     INTENT(IN)   :: PTSTEP
CHARACTER(LEN=6),         INTENT(IN)   :: HUVW_ADV_SCHEME
!
END SUBROUTINE ION_DRIFT
END INTERFACE
END MODULE MODI_ION_DRIFT
!
!     ################################################################ 
      SUBROUTINE ION_DRIFT(PDRIFTP, PDRIFTM, PSVT, PRHODREF, PRHODJ, &
                           HLBCX, HLBCY, KTCOUNT, PTSTEP, HDRIFT,    &
                           HUVW_ADV_SCHEME)
!     ################################################################
!
!!    PURPOSE
!!    -------
!!
!!    Compute the ion drift defined as
!!        -/+ Div ( mu(+/-) N(+/-) E )
!!        where mu is the ion mobility (m2/V.s), N is the ion mixing ratio (1/kg)
!!              and E is the electric field (V/m)
!!           +/- mu(+/-) E  = drift motion
!!
!!    AUTHOR
!!    ------
!!          M. Chong      01/2010
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!         
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_METRICS_n, ONLY : XDXX, XDYY, XDZX, XDZY, XDZZ
USE MODD_NSV,  ONLY: NSV_ELECBEG, NSV_ELECEND
USE MODD_ELEC_n, ONLY : XCION_POS_FW, XCION_NEG_FW, &
                            XMOBIL_POS, XMOBIL_NEG,     &
                            XEFIELDU, XEFIELDV, XEFIELDW
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
!
USE MODE_ll
USE MODE_ELEC_ll
!
USE MODI_SHUMAN
USE MODI_CONTRAV
USE MODI_PPM_SCALAR
USE MODI_GDIV
USE MODI_ION_BOUND4DRIFT
!
IMPLICIT NONE
!
!
!*       0.1   declarations of arguments
!
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN)  :: HLBCX,HLBCY
CHARACTER(LEN=3), INTENT(IN)                :: HDRIFT
REAL, DIMENSION(:,:,:),       INTENT(INOUT) :: PDRIFTP, PDRIFTM
REAL, DIMENSION(:,:,:,:),     INTENT(INOUT) :: PSVT
REAL, DIMENSION(:,:,:),       INTENT(IN)    :: PRHODREF, PRHODJ
INTEGER,                      INTENT(IN)    :: KTCOUNT  ! Temporal loop counter
REAL,                         INTENT(IN)    :: PTSTEP
CHARACTER(LEN=6),             INTENT(IN)    :: HUVW_ADV_SCHEME
!
!
!*       0.2   declarations of local variables
!
INTEGER :: IIB, IIE  ! index of first and last inner mass points along x
INTEGER :: IJB, IJE  ! index of first and last inner mass points along y
INTEGER :: IKB, IKE  ! index of first and last inner mass points along z
INTEGER :: IKU
INTEGER :: IXOR, IYOR  ! origin of the extended subdomain
INTEGER, DIMENSION(3) :: IM_LOC
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3)) :: ZDRIFTX
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3)) :: ZDRIFTY
REAL, DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3)) :: ZDRIFTZ

REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZADVS, ZSVT  !for advection form
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: ZXCT, ZYCT, ZZCT !
CHARACTER (LEN=6)                     :: HSV_ADV_SCHEME
                                                     ! of drift source
REAL :: ZMIN_DRIFT, ZMAX_DRIFT
REAL :: ZMAX__POS, ZMAX__NEG
INTEGER :: IPROC, IPROCMIN, ISV
INTEGER :: IINFO_ll      ! return code of parallel routine
!
TYPE(LIST_ll), POINTER :: TZFIELDS_ll   ! list of fields to exchange
!
NULLIFY(TZFIELDS_ll)
!
!
!------------------------------------------------------------------------
!
CALL MYPROC_ELEC_ll (IPROC)
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS AND OTHER INDICES
!              ----------------------------------------------
!
! beginning and end indexes of the physical subdomain
IIB = 1 + JPHEXT
IIE = SIZE(PSVT,1) - JPHEXT
IJB = 1 + JPHEXT
IJE = SIZE(PSVT,2) - JPHEXT
IKB = 1 + JPVEXT
IKE = SIZE(PSVT,3) - JPVEXT
IKU = SIZE(PSVT,3)
!
!
!-------------------------------------------------------------------------------
!
!*      3.     UPDATE BOUNDARY CONDITION FOR IONS ACCORDING TO THE DRIFT MOTION
!              ----------------------------------------------------------------
!
IF (LWEST_ll() ) THEN
  XEFIELDU(IIB-1,:,:) = XEFIELDU(IIB,:,:)
ENDIF
!
IF (LEAST_ll() ) THEN
  XEFIELDU(IIE+1,:,:) = XEFIELDU(IIE,:,:)
ENDIF
!
IF (LSOUTH_ll()) THEN
  XEFIELDV(:,IJB-1,:) = XEFIELDV(:,IJB,:)
ENDIF   
!
IF (LNORTH_ll()) THEN
  XEFIELDV(:,IJE+1,:) = XEFIELDV(:,IJE,:)
ENDIF   
!
CALL ADD3DFIELD_ll(TZFIELDS_ll,XEFIELDU)
CALL ADD3DFIELD_ll(TZFIELDS_ll,XEFIELDV)
CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
CALL CLEANLIST_ll(TZFIELDS_ll)
!
!  specify lateral boundary ion mixing ratio
CALL ION_BOUND4DRIFT (HLBCX,HLBCY,XEFIELDU,XEFIELDV,PSVT)
!
CALL ADD3DFIELD_ll(TZFIELDS_ll,PSVT(:,:,:,NSV_ELECBEG))
CALL ADD3DFIELD_ll(TZFIELDS_ll,PSVT(:,:,:,NSV_ELECEND))
CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
CALL CLEANLIST_ll(TZFIELDS_ll)
!
! specify upper boundary ion mixing ratio
WHERE (XEFIELDW(:,:,IKE) .GE. 0.)   ! Out(In)flow for positive (negative) ions 
  PSVT (:,:,IKE+1,NSV_ELECBEG) = 2. * PSVT (:,:,IKE,NSV_ELECBEG) -  &
                                      PSVT (:,:,IKE-1,NSV_ELECBEG)
  PSVT (:,:,IKE+1,NSV_ELECEND) = XCION_NEG_FW(:,:,IKE+1)
ELSE WHERE      ! In(Out)flow for positive (negative) ions
  PSVT (:,:,IKE+1,NSV_ELECBEG) = XCION_POS_FW(:,:,IKE+1)
  PSVT (:,:,IKE+1,NSV_ELECEND) = 2.* PSVT (:,:,IKE,NSV_ELECEND) -  &
                                     PSVT (:,:,IKE-1,NSV_ELECEND)
END WHERE  
!
XEFIELDW(:,:,IKB-1) = XEFIELDW(:,:,IKB)
XEFIELDW(:,:,IKE+1) = XEFIELDW(:,:,IKE)
!
!
!*       4.  positive ion source
!
IF (HDRIFT /= 'PPM') THEN   ! Divergence form
! x-component of div term
  ZDRIFTX(:,:,:) = -PSVT(:,:,:,NSV_ELECBEG) * XMOBIL_POS(:,:,:) * XEFIELDU(:,:,:) &
                   * PRHODJ(:,:,:)
! y-component of div term
  ZDRIFTY(:,:,:) = -PSVT(:,:,:,NSV_ELECBEG) * XMOBIL_POS(:,:,:) * XEFIELDV(:,:,:) &
                   * PRHODJ(:,:,:)
! z-component of div term
  ZDRIFTZ(:,:,:) = -PSVT(:,:,:,NSV_ELECBEG) * XMOBIL_POS(:,:,:) * XEFIELDW(:,:,:) &
                   * PRHODJ(:,:,:)
ELSE            ! Advection form
  ZDRIFTX(:,:,:) = XMOBIL_POS(:,:,:) * XEFIELDU(:,:,:) 
  ZDRIFTY(:,:,:) = XMOBIL_POS(:,:,:) * XEFIELDV(:,:,:)
  ZDRIFTZ(:,:,:) = XMOBIL_POS(:,:,:) * XEFIELDW(:,:,:)
ENDIF
!
! Put components at flux sides
ZDRIFTX(:,:,:) = MXM(ZDRIFTX(:,:,:))
ZDRIFTY(:,:,:) = MYM(ZDRIFTY(:,:,:))
ZDRIFTZ(:,:,:) = MZM(1,IKU,1,ZDRIFTZ (:,:,:))
!
IF (LWEST_ll( ))  ZDRIFTX(IIB-1,:,:) = ZDRIFTX(IIB,:,:)
IF (LSOUTH_ll( )) ZDRIFTY(:,IJB-1,:) = ZDRIFTY(:,IJB,:)
ZDRIFTZ(:,:,IKB-1) = ZDRIFTZ(:,:,IKB)
!
IF (HDRIFT /= 'PPM') THEN
  CALL GDIV(HLBCX,HLBCY,XDXX,XDYY,XDZX,XDZY,XDZZ,ZDRIFTX,ZDRIFTY,ZDRIFTZ,PDRIFTP)
ELSE
  ISV = 1
  ALLOCATE (ZADVS(SIZE(PDRIFTP,1),SIZE(PDRIFTP,2),SIZE(PDRIFTP,3), ISV))
  ALLOCATE (ZSVT(SIZE(PDRIFTP,1),SIZE(PDRIFTP,2),SIZE(PDRIFTP,3), ISV))
  ALLOCATE (ZXCT(SIZE(PDRIFTP,1),SIZE(PDRIFTP,2),SIZE(PDRIFTP,3)))
  ALLOCATE (ZYCT(SIZE(PDRIFTP,1),SIZE(PDRIFTP,2),SIZE(PDRIFTP,3)))
  ALLOCATE (ZZCT(SIZE(PDRIFTP,1),SIZE(PDRIFTP,2),SIZE(PDRIFTP,3)))
!
  IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN                                      
    CALL CONTRAV (HLBCX,HLBCY,ZDRIFTX,ZDRIFTY,ZDRIFTZ,XDXX,XDYY,XDZZ,XDZX,XDZY, &
                  ZXCT,ZYCT,ZZCT,2)
  ELSEIF (HUVW_ADV_SCHEME=='CEN4TH') THEN
    CALL CONTRAV (HLBCX,HLBCY,ZDRIFTX,ZDRIFTY,ZDRIFTZ,XDXX,XDYY,XDZZ,XDZX,XDZY, &
                  ZXCT,ZYCT,ZZCT,4)
  ENDIF
!
  ZXCT = ZXCT*PTSTEP
  ZYCT = ZYCT*PTSTEP
  ZZCT = ZZCT*PTSTEP
!
  ZADVS(:,:,:,1) = 0.
  ZSVT(:,:,:,1) = PSVT(:,:,:,NSV_ELECBEG)
  HSV_ADV_SCHEME = 'PPM_01'
!
  CALL PPM_SCALAR(HLBCX,HLBCY,ISV,KTCOUNT,             &
                   ZXCT, ZYCT, ZZCT, PTSTEP, PRHODJ,  &
                   ZSVT, ZADVS, HSV_ADV_SCHEME      )
!
  PDRIFTP(:,:,:) = ZADVS(:,:,:,1)
!
! Additional term: -N . DIV(mu E)
  ZDRIFTX = ZDRIFTX*MXM(PRHODJ)
  ZDRIFTY = ZDRIFTY*MYM(PRHODJ)
  ZDRIFTZ = ZDRIFTZ*MZM(1,IKU,1,PRHODJ)
!
  CALL GDIV(HLBCX,HLBCY,XDXX,XDYY,XDZX,XDZY,XDZZ,ZDRIFTX,ZDRIFTY,ZDRIFTZ,ZXCT)
!
  PDRIFTP(:,:,:) = PDRIFTP(:,:,:)-ZXCT(:,:,:)*PSVT(:,:,:,NSV_ELECBEG)
ENDIF
!
!
!*       4.2.2  negative ion source
!
IF (HDRIFT /= 'PPM') THEN
! x-component of div term
  ZDRIFTX(:,:,:) = PSVT(:,:,:,NSV_ELECEND) * XMOBIL_NEG(:,:,:) * XEFIELDU(:,:,:) &
                   *PRHODJ(:,:,:)
! y-component of div term
  ZDRIFTY(:,:,:) = PSVT(:,:,:,NSV_ELECEND) * XMOBIL_NEG(:,:,:) * XEFIELDV(:,:,:) &
                   * PRHODJ(:,:,:)
! z-component of div term
  ZDRIFTZ(:,:,:) = PSVT(:,:,:,NSV_ELECEND) * XMOBIL_NEG(:,:,:) * XEFIELDW(:,:,:) &
                   * PRHODJ(:,:,:)
ELSE
  ZDRIFTX(:,:,:) = -XMOBIL_NEG(:,:,:) * XEFIELDU(:,:,:)
  ZDRIFTY(:,:,:) = -XMOBIL_NEG(:,:,:) * XEFIELDV(:,:,:)
  ZDRIFTZ(:,:,:) = -XMOBIL_NEG(:,:,:) * XEFIELDW(:,:,:)
ENDIF
!
! Put components at flux sides
ZDRIFTX(:,:,:) = MXM(ZDRIFTX(:,:,:))
ZDRIFTY(:,:,:) = MYM(ZDRIFTY(:,:,:))
ZDRIFTZ(:,:,:) = MZM(1,IKU,1,ZDRIFTZ (:,:,:))
!
IF (LWEST_ll( ))  ZDRIFTX(IIB-1,:,:) = ZDRIFTX(IIB,:,:)
IF (LSOUTH_ll( )) ZDRIFTY(:,IJB-1,:) = ZDRIFTY(:,IJB,:)
ZDRIFTZ(:,:,IKB-1) = ZDRIFTZ(:,:,IKB)
!
IF (HDRIFT /= 'PPM') THEN
  CALL GDIV(HLBCX,HLBCY,XDXX,XDYY,XDZX,XDZY,XDZZ,ZDRIFTX,ZDRIFTY,ZDRIFTZ,PDRIFTM)
ELSE
  IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN                                      
    CALL CONTRAV (HLBCX,HLBCY,ZDRIFTX,ZDRIFTY,ZDRIFTZ,XDXX,XDYY,XDZZ,XDZX,XDZY, &
                  ZXCT,ZYCT,ZZCT,2)
  ELSEIF (HUVW_ADV_SCHEME=='CEN4TH') THEN
    CALL CONTRAV (HLBCX,HLBCY,ZDRIFTX,ZDRIFTY,ZDRIFTZ,XDXX,XDYY,XDZZ,XDZX,XDZY, &
                  ZXCT,ZYCT,ZZCT,4)
  ENDIF
!
  ZXCT = ZXCT * PTSTEP
  ZYCT = ZYCT * PTSTEP
  ZZCT = ZZCT * PTSTEP
!
  ZADVS(:,:,:,1) = 0.
  ZSVT(:,:,:,1) = PSVT(:,:,:,NSV_ELECEND)
  HSV_ADV_SCHEME = 'PPM_01'
!
  CALL PPM_SCALAR(HLBCX,HLBCY,ISV,KTCOUNT,             &
                   ZXCT, ZYCT, ZZCT, PTSTEP, PRHODJ,  &
                   ZSVT, ZADVS, HSV_ADV_SCHEME      )
!
  PDRIFTM(:,:,:) = ZADVS(:,:,:,1)
!
! Additional term  -N . DIV(-mu E)
  ZDRIFTX = ZDRIFTX * MXM(PRHODJ)
  ZDRIFTY = ZDRIFTY * MYM(PRHODJ)
  ZDRIFTZ = ZDRIFTZ * MZM(1,IKU,1,PRHODJ)
!
  CALL GDIV(HLBCX,HLBCY,XDXX,XDYY,XDZX,XDZY,XDZZ,ZDRIFTX,ZDRIFTY,ZDRIFTZ,ZXCT)
!
  PDRIFTM(:,:,:) = PDRIFTM(:,:,:) - ZXCT(:,:,:) * PSVT(:,:,:,NSV_ELECEND)
!
  DEALLOCATE (ZXCT, ZYCT, ZZCT, ZADVS, ZSVT)
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ION_DRIFT
