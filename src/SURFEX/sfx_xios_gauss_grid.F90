SUBROUTINE SFX_XIOS_GAUSS_GRID(KDGLG,KDLON,KRANK,KPROC)
!!
!!
!!     PURPOSE : declare to XIOS a distribution for the gaussian complete grid
!!     --------  and for a gauss latitudes axis
!!
!!
!!     METHOD
!!     ------
!!
!!     Based on number of latitudes and longitudes, and proc number,
!!     compute a rectilinear distribution for a rectilinear grid
!!     having the gaussian latitudes, an declare it to Xios
!!
!!     Some MPI tasks may be at rest, for the sake of the rectilinear 
!!     distribution
!!
!!     Use Surfex function for getting gaussian latitudes
!!
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
!!
!!     REFERENCE
!!     ---------
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    08/2016
!!     D.St-Martin 04/2017 domain size corrections
!!     S.Sénési    04/2017 - Compute the distribution here, rather than relying
!!                           on Arpege MYSETA, MYSETB.
!!                         - Move from MSE to SURFEX  
!!                         - Fix corner latitudes 
!!     S.Sénési    10/2017 - Add latitudes axis declaration
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#ifdef WXIOS
USE MODI_ABOR1_SFX
!
USE XIOS     , ONLY : XIOS_DOMAIN, XIOS_DOMAINGROUP,XIOS_SET_DOMAIN_ATTR, &
                      XIOS_AXIS, XIOS_AXISGROUP,XIOS_SET_AXIS_ATTR,&
                      XIOS_ADD_CHILD, XIOS_GET_HANDLE
!
USE MODE_GRIDTYPE_GAUSS, ONLY : LATITUDES_GAUSS
USE EGGANGLES , ONLY : P_ASIN
USE MODD_CSTS,  ONLY : XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB, JPIM
!
IMPLICIT NONE
!
INTEGER(KIND=JPIM), INTENT(IN) :: KDGLG         ! number of rows of latitudes
INTEGER(KIND=JPIM), INTENT(IN) :: KDLON         ! length of a row of latitude
INTEGER(KIND=JPIM), INTENT(IN) :: KRANK         ! rank of mpi task (from 0)
INTEGER(KIND=JPIM), INTENT(IN) :: KPROC         ! number of procs
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=8), PARAMETER       :: YNAMGRID="complete"
 CHARACTER(LEN=4), PARAMETER       :: YNAMAXIS="glat"
INTEGER                           :: NI,NJ,I,J,IOFF,JOFF,NEXT
INTEGER                           :: NREG_NS,NREG_EW 
INTEGER                           :: NREGS_NS,NREGS_EW 
INTEGER                           :: IRESIDUAL, NX_NS, NX_EW

REAL(KIND=JPRB)                   :: ZINCR
REAL(KIND=JPRB)                   :: FACT        
REAL, ALLOCATABLE, DIMENSION(:)   :: ZDSINLA, ZSINLA, ZWG
REAL, ALLOCATABLE, DIMENSION(:)   :: ZLATI,ZLONG
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZLATIC,ZLONGC
!
TYPE(xios_domaingroup)            :: domaingroup_hdl
TYPE(xios_domain)                 :: domain_hdl
TYPE(xios_axisgroup)              :: axisgroup_hdl
TYPE(xios_axis)                   :: axis_hdl
!
REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_GAUSS_GRID',0,ZHOOK_HANDLE)

!$OMP SINGLE
! Basic XIOS declarations
 CALL XIOS_GET_HANDLE("domain_definition",domaingroup_hdl)
 CALL XIOS_ADD_CHILD(domaingroup_hdl,domain_hdl,YNAMGRID)
 CALL XIOS_SET_DOMAIN_ATTR(YNAMGRID, type="rectilinear", data_dim=2)!, nvertex=2)
 !
 ! Choose a separable NW/SE distribution for the complete grid, based
 ! on grid size and proc number. Method : explore all couples of
 ! NREGS_NS, NREGS_EW which match NREGS_NS * NREGS_EW <= KPROC and
 ! choose the one with minimal residual and minimal
 ! abs(NREGS_EW-2*NREGS_NS) in order to get somewhat 'square' regions
 !
 ! The # of unused procs is less than 1 for kproc < 20.000
IRESIDUAL=KPROC
NREGS_NS=1
NREGS_EW=KPROC
DO J=KPROC-1,2,-1
   DO I=KPROC-1,2,-1
      IF ((I*J <= KPROC) .AND. ((KPROC-I*J) <= IRESIDUAL  )) THEN 
         IF ((ABS(2*I-J) < ABS(2*NREGS_NS-NREGS_EW)) .OR. &
              (IRESIDUAL == KPROC))  THEN
            IRESIDUAL = KPROC -I*J
            NREGS_NS=I
            NREGS_EW=J
         ENDIF
      ENDIF
   ENDDO
ENDDO
!
! Compute region indices (from 1) for current MPI task
!
NREG_NS = 1+ KRANK/NREGS_EW 
NREG_EW = 1 + mod(KRANK,NREGS_EW)
!
! Compute domain size in longitude 
!
NEXT=0
IF (MOD(KDLON,NREGS_EW)==0) THEN
  NI=KDLON/NREGS_EW
  IOFF=(NREG_EW-1)*NI
ELSE
  NI=KDLON/NREGS_EW+1
  IF (NI*(NREGS_EW-1) < KDLON) THEN
    IOFF=(NREG_EW-1)*NI
    IF (NREG_EW==NREGS_EW) NI=MOD(KDLON,NI)
  ELSE
    NI=KDLON/NREGS_EW
    NEXT=MOD(KDLON,NREGS_EW)
    IF (NREG_EW <= NEXT) THEN
      NI=NI+1
      IOFF=(NREG_EW-1)*NI
    ELSE
      IOFF=(NREG_EW-1)*NI+NEXT
    ENDIF
  ENDIF
ENDIF
!
! Compute domain size in latitude 
!
NEXT=0
IF (MOD(KDGLG,NREGS_NS)==0) THEN
  NJ=KDGLG/NREGS_NS
  JOFF=(NREG_NS-1)*NJ
ELSE
  NJ=KDGLG/NREGS_NS+1
  IF (NJ*(NREGS_NS-1) < KDGLG) THEN
    JOFF=(NREG_NS-1)*NJ
    IF (NREG_NS==NREGS_NS) NJ=MOD(KDGLG,NJ)
  ELSE
    NJ=KDGLG/NREGS_NS
    NEXT=MOD(KDGLG,NREGS_NS)
    IF (NREG_NS <= NEXT) THEN
      NJ=NJ+1
      JOFF=(NREG_NS-1)*NJ
    ELSE
      JOFF=(NREG_NS-1)*NJ+NEXT
    ENDIF
  ENDIF
ENDIF
!
! Use dummy values for tasks which are unused (due to rectilinear
! distribution)
!
IF (KRANK >= NREGS_NS*NREGS_EW) THEN
   NI=0
   IOFF=1
   NJ=0
   JOFF=1
ENDIF
!
! Compute evenly spaced longitudes
!
ALLOCATE(ZLONG(NI))
ALLOCATE(ZLONGC(2,NI))
ZINCR=360./KDLON
DO I=1,NI
   ZLONG(I)=(IOFF+I-1)*ZINCR ! Choose to have longitude 0 at grid mesh center
   !ZLONG(I)=(IOFF+I-0.5)*ZINCR
   ZLONGC(1,I)=ZLONG(I) - ZINCR/2.
   ZLONGC(2,I)=ZLONG(I) + ZINCR/2.
ENDDO
!
 CALL XIOS_SET_DOMAIN_ATTR(YNAMGRID, ni_glo=KDLON, ni=NI,ibegin=IOFF)
 CALL XIOS_SET_DOMAIN_ATTR(YNAMGRID, lonvalue_1d=ZLONG(:), bounds_lon_1D=ZLONGC(:,:))
!
! Use Surfex copy of SUGAW (Cy36) for computing Gauss latitudes
!
ALLOCATE(ZDSINLA(KDGLG))
ALLOCATE(ZSINLA(KDGLG))
ALLOCATE(ZWG(KDGLG))
CALL LATITUDES_GAUSS(KDGLG,ZSINLA,ZDSINLA,ZWG)
!
ALLOCATE(ZLATI(0:NJ+1))
ALLOCATE(ZLATIC(2,NJ))
ZINCR=180./KDGLG
!
! There is an Xios limitation, in remapping from gaussian reduced grid
! to the equivalent complete grid (i.e. same latitiudes).  FACT allows
! to alleviate the problem by using latitudes which are slighlty
! pushed toward the equator (while keeping the pole in corners)
! That works with 1.e-12, not for e.g. 1.e-15
!
FACT= 1. - 1.e-12
!
DO J=0,NJ+1
   IF (JOFF+J < 1) THEN
      ZLATI(J)= -180 + P_ASIN(ZSINLA((JOFF+J+1))) * 180. / XPI * FACT
   ELSE IF (JOFF+J > KDGLG ) THEN
      ZLATI(J)= 180 + P_ASIN(ZSINLA((JOFF+J-1))) * 180. / XPI * FACT
   ELSE
      ZLATI(J)=- P_ASIN(ZSINLA((JOFF+J))) * 180. / XPI * FACT
   ENDIF
   ! When wishing to test evenly spaced latitudes :
   ! ZLATI(J)=-90. + (JOFF+J-0.5)*ZINCR
ENDDO
DO J=1,NJ
   ZLATIC(1,J)=(ZLATI(J) + ZLATI(J-1)) /2.
   ZLATIC(2,J)=(ZLATI(J) + ZLATI(J+1)) /2.
ENDDO
!
DEALLOCATE(ZDSINLA, ZSINLA, ZWG)
!
 CALL XIOS_SET_DOMAIN_ATTR(YNAMGRID, nj_glo=KDGLG, nj=NJ, jbegin=JOFF)
 CALL XIOS_SET_DOMAIN_ATTR(YNAMGRID,latvalue_1d=ZLATI(1:NJ),bounds_lat_1D=ZLATIC(:,:))
!
!
! Define and distribute an axis for latitudes
!
!
 CALL XIOS_GET_HANDLE("axis_definition",axisgroup_hdl)
 CALL XIOS_ADD_CHILD(axisgroup_hdl,axis_hdl,YNAMAXIS)
 CALL XIOS_SET_AXIS_ATTR(YNAMAXIS, standard_name="latitude", unit="degrees_north")
 CALL XIOS_SET_AXIS_ATTR(YNAMAXIS, n_glo=KDGLG, begin=JOFF, n=NJ)
 CALL XIOS_SET_AXIS_ATTR(YNAMAXIS, value=ZLATI(1:NJ))
 CALL XIOS_SET_AXIS_ATTR(YNAMAXIS, bounds=ZLATIC(:,:))
!
!
!$OMP END SINGLE
!
DEALLOCATE(ZLATI,ZLONG,ZLATIC,ZLONGC)
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_GAUSS_GRID',1,ZHOOK_HANDLE)
#endif
END SUBROUTINE SFX_XIOS_GAUSS_GRID
