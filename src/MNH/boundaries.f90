!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!#####################
MODULE MODI_BOUNDARIES
!#####################
!
INTERFACE
!
      SUBROUTINE BOUNDARIES (                                       &
            PTSTEP,HLBCX,HLBCY,KRR,KSV,KTCOUNT,                     &
            PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,   &
            PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,   &
            PLBXUS,PLBXVS,PLBXWS,PLBXTHS,PLBXTKES,PLBXRS,PLBXSVS,   &
            PLBYUS,PLBYVS,PLBYWS,PLBYTHS,PLBYTKES,PLBYRS,PLBYSVS,   &
            PRHODJ,                                                 &
            PUT,PVT,PWT,PTHT,PTKET,PRT,PSVT,PSRCT                   )
!
REAL,                  INTENT(IN) :: PTSTEP        ! time step dt
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX,HLBCY   ! X and Y-direc. LBC type
!
INTEGER,               INTENT(IN) :: KRR           ! Number of moist  variables
INTEGER,               INTENT(IN) :: KSV           ! Number of Scalar Variables
INTEGER,               INTENT(IN) :: KTCOUNT       ! Temporal loop COUNTer
                                                   ! (=1 at the segment beginning)
!
! Lateral Boundary fields at time t
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTKEM
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
! temporal derivative of the Lateral Boundary fields
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXUS,PLBXVS,PLBXWS ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTHS              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYUS,PLBYVS,PLBYWS ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTHS              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTKES          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTKES
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBXRS  ,PLBXSVS  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBYRS  ,PLBYSVS  ! in x and y-dir.
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PRHODJ    ! Jacobian * dry density of
                                                  !  the reference state
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PUT,PVT,PWT,PTHT,PTKET,PSRCT
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRT,PSVT
                                                      ! Variables at t
!
END SUBROUTINE BOUNDARIES
!
END INTERFACE
!

END MODULE MODI_BOUNDARIES
!
!
!     ####################################################################
      SUBROUTINE BOUNDARIES (                                       &
            PTSTEP,HLBCX,HLBCY,KRR,KSV,KTCOUNT,                     &
            PLBXUM,PLBXVM,PLBXWM,PLBXTHM,PLBXTKEM,PLBXRM,PLBXSVM,   &
            PLBYUM,PLBYVM,PLBYWM,PLBYTHM,PLBYTKEM,PLBYRM,PLBYSVM,   &
            PLBXUS,PLBXVS,PLBXWS,PLBXTHS,PLBXTKES,PLBXRS,PLBXSVS,   &
            PLBYUS,PLBYVS,PLBYWS,PLBYTHS,PLBYTKES,PLBYRS,PLBYSVS,   &
            PRHODJ,                                                 &
            PUT,PVT,PWT,PTHT,PTKET,PRT,PSVT,PSRCT                   )
!     ####################################################################
!
!!****  *BOUNDARIES* - routine to prepare the Lateral Boundary Conditions for
!!                 all variables at a scalar localization relative to the 
!!                 considered boundary.
!!
!!    PURPOSE
!!    -------
!       Fill up the left and right lateral EXTernal zones, for all prognostic
!       variables, at time t and t-dt, to avoid particular cases close to
!       the Lateral Boundaries in routines computing the evolution terms, in
!       particular in the advection routines.
!
!!**  METHOD
!!    ------
!!      3 different options are proposed: 'WALL' 'CYCL' 'OPEN'
!!                    to define the Boundary Condition type,
!!      though the variables HLBCX and HLBCY (for the X and Y-directions
!!      respectively).
!!       For the 'OPEN' type of LBC, the treatment depends
!!      on the flow configuration: i.e. INFLOW or OUTFLOW conditions.
!!   
!!    EXTERNAL 
!!    --------  
!!    GET_INDICE_ll  : get physical sub-domain bounds
!!    LWEAST_ll,LEAST_ll,LNORTH_ll,LSOUTH_ll : position functions
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------  
!!      Module MODD_PARAMETERS : 
!!        JPHEXT ,JPVEXT 
!!
!!      Module MODD_CONF :
!!        CCONF
!!
!!      Module MODE_UPDATE_NSV :
!!        NSV_CHEM, NSV_CHEMBEG, NSV_CHEMEND
!!
!!      Module MODD_CTURB :
!!        XTKEMIN 
!!
!!    REFERENCE
!!    ---------
!!      Book1 and book2 of documentation (routine BOUNDARIES)
!!
!!    AUTHOR
!!    ------
!!	J.-P. Lafore J. Stein     * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        17/10/94 
!!      Modification    02/11/94  (J.Stein) copy for t-dt at the external points
!!                                          + change the copy formulation
!!      Modification    18/11/94  (J.Stein) bug correction in the normal velocity
!!                                          prescription in the WALL cases
!!      Modification    13/02/95  (Lafore)  to account for the OPEN case and
!!                                          for the LS fields introduction
!!      Modification    03/03/95  (Mallet)  corrections in variables names in 
!!                                          the Y-OPEN case
!!                      16/03/95  (J.Stein) remove R from the historical variables
!!      Modification    31/05/95  (Lafore)  MASTER_DEV2.1 preparation after the
!!                                          LBC tests performed by I. Mallet
!!      Modification    15/03/96  (Richard) bug correction for OPEN CASE: (TOP Y-LBC)
!!                                          Rv case 
!!      Modification    15/03/96  (Shure)   bug correction for SV variable in
!!                                          open x  right case
!!      Modification    24/10/96  (Masson)  initialization of outer points in
!!                                          wall cases for spawning interpolations
!!      Modification    13/03/97  (Lafore)  "surfacic" LS-fields introduction
!!      Modification    10/04/97  (Lafore)  proper treatment of minima for TKE and EPS
!!      Modification    01/09/97  (Masson)  minimum value for water and passive
!!                                          scalars set to zero at instants M,T
!!      Modification    20/10/97  (Lafore)  introduction of DAVI type of lbc
!!                                           suppression of NEST type
!!      Modification    12/11/97  ( Stein ) use the lB fields
!!      Modification    02/06/98  (Lafore)  declaration of local variables (PLBXUM
!!                                          and PLBXWM do'nt have the same size)
!!      Modification    24/08/98   (Jabouille) parallelize the code 
!!      Modification    20/04/99  ( Stein ) use the same conditions for times t
!!                                          and t-dt
!!      Modification    11/04/00  (Mari)    special conditions for chemical variables
!!      Modification    10/01/01  (Tulet)   update for MOCAGE boundary conditions
!!      Modification    22/01/01  (Gazen)   use NSV_CHEM,NSV_CHEMBEG,NSV_CHEMEND variables
!!      Modification    22/06/01(Jabouille) use XSVMIN
!!      Modification    20/11/01(Gazen & Escobar) rewrite GCHBOUNDARY for portability
!!      Modification    14/03/05 (Tulet)    bug : in case of CYCL do not call ch_boundaries
!!      Modification    14/05/05 (Tulet)    add aerosols / dust
!!      Modification    05/06               Suppression of DAVI type of lbc
!!      Modification    05/06               Remove EPS
!!      Modification    12/2010  (Chong)    Add boundary condition for ions
!!                                          (fair weather profiles)
!!      Modification    04/2013  (C.Lac)    Remove instant M               
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!         
USE MODD_PARAMETERS
USE MODD_CTURB
USE MODD_CONF
USE MODD_NSV
USE MODD_CH_MNHC_n,   ONLY : LUSECHEM, LUSECHIC
USE MODD_CH_AEROSOL , ONLY : LORILAM
USE MODD_DUST
USE MODD_SALT,        ONLY : LSALT
USE MODD_PASPOL,      ONLY : LPASPOL
USE MODD_CONDSAMP,    ONLY : LCONDSAMP
USE MODD_ELEC_DESCR             
USE MODD_ELEC_n                 
USE MODD_REF_n    
USE MODD_PARAM_n,     ONLY : CELEC 
!
USE MODE_MODELN_HANDLER
!
USE MODI_ION_BOUNDARIES
USE MODI_CH_BOUNDARIES
!
USE MODE_ll
!
!
IMPLICIT NONE
!
!
!*       0.1   declarations of arguments
!
!
!
!
REAL,                  INTENT(IN) :: PTSTEP        ! time step dt
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX,HLBCY   ! X and Y-direc. LBC type
!
INTEGER,               INTENT(IN) :: KRR           ! Number of moist  variables
INTEGER,               INTENT(IN) :: KSV           ! Number of Scalar Variables
INTEGER,               INTENT(IN) :: KTCOUNT       ! Temporal loop COUNTer
                                                   ! (=1 at the segment beginning)
!
! Lateral Boundary fields at time t
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXUM,PLBXVM,PLBXWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYUM,PLBYVM,PLBYWM ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTHM              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTKEM          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTKEM
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBXRM  ,PLBXSVM  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBYRM  ,PLBYSVM  ! in x and y-dir.
! temporal derivative of the Lateral Boundary fields
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXUS,PLBXVS,PLBXWS ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTHS              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYUS,PLBYVS,PLBYWS ! Wind
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTHS              ! Mass
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBXTKES          ! TKE
REAL, DIMENSION(:,:,:),          INTENT(IN) :: PLBYTKES
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBXRS  ,PLBXSVS  ! Moisture and SV
REAL, DIMENSION(:,:,:,:),        INTENT(IN) :: PLBYRS  ,PLBYSVS  ! in x and y-dir.
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PRHODJ    ! Jacobian * dry density of
                                                  !  the reference state
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PUT,PVT,PWT,PTHT,PTKET,PSRCT
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRT,PSVT
                                                      ! Variables at t
!
!*       0.2   declarations of local variables
!
INTEGER             :: IIB       ! indice I Beginning in x direction
INTEGER             :: IJB       ! indice J Beginning in y direction
INTEGER             :: IKB       ! indice K Beginning in z direction
INTEGER             :: IIE       ! indice I End       in x direction 
INTEGER             :: IJE       ! indice J End       in y direction 
INTEGER             :: IKE       ! indice K End       in z direction 
INTEGER             :: JEXT      ! Loop index for EXTernal points
INTEGER             :: JRR       ! Loop index for RR variables (water)
INTEGER             :: JSV       ! Loop index for Scalar Variables
INTEGER             :: IMI       ! Model Index
REAL                :: ZTSTEP    ! effective time step
INTEGER             :: ILBX,ILBY ! size of LB fields' arrays
LOGICAL, SAVE, DIMENSION(:), ALLOCATABLE :: GCHBOUNDARY, GAERBOUNDARY,&
                    GDSTBOUNDARY, GSLTBOUNDARY, GPPBOUNDARY,          &
                    GCSBOUNDARY, GICBOUNDARY
LOGICAL, SAVE        :: GFIRSTCALL1 = .TRUE.
LOGICAL, SAVE        :: GFIRSTCALL2 = .TRUE.
LOGICAL, SAVE        :: GFIRSTCALL3 = .TRUE.
LOGICAL, SAVE        :: GFIRSTCALL5 = .TRUE. 
LOGICAL, SAVE        :: GFIRSTCALLPP = .TRUE.                         
LOGICAL, SAVE        :: GFIRSTCALLCS = .TRUE.                         
LOGICAL, SAVE        :: GFIRSTCALLIC = .TRUE.                         
!
REAL, DIMENSION(SIZE(PLBXWM,1),SIZE(PLBXWM,2),SIZE(PLBXWM,3)) ::  &
                       ZLBXVT,ZLBXWT,ZLBXTHT
REAL, DIMENSION(SIZE(PLBYWM,1),SIZE(PLBYWM,2),SIZE(PLBYWM,3)) ::  &
                       ZLBYUT,ZLBYWT,ZLBYTHT
REAL, DIMENSION(SIZE(PLBXTKEM,1),SIZE(PLBXTKEM,2),SIZE(PLBXTKEM,3)) ::  &
                       ZLBXTKET
REAL, DIMENSION(SIZE(PLBYTKEM,1),SIZE(PLBYTKEM,2),SIZE(PLBYTKEM,3)) ::  &
                       ZLBYTKET
REAL, DIMENSION(SIZE(PLBXRM,1),SIZE(PLBXRM,2),SIZE(PLBXRM,3),SIZE(PLBXRM,4)) :: &
                       ZLBXRT
REAL, DIMENSION(SIZE(PLBYRM,1),SIZE(PLBYRM,2),SIZE(PLBYRM,3),SIZE(PLBYRM,4)) :: &
                       ZLBYRT
REAL, DIMENSION(SIZE(PLBXSVM,1),SIZE(PLBXSVM,2),SIZE(PLBXSVM,3),SIZE(PLBXSVM,4)) :: &
                       ZLBXSVT
REAL, DIMENSION(SIZE(PLBYSVM,1),SIZE(PLBYSVM,2),SIZE(PLBYSVM,3),SIZE(PLBYSVM,4)) :: &
                       ZLBYSVT
LOGICAL              :: GCHTMP
LOGICAL              :: GPPTMP
LOGICAL              :: GCSTMP
!
!-------------------------------------------------------------------------------
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS AND OTHER INDICES:
!              ----------------------------------------------
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB = 1 + JPVEXT
IKE = SIZE(PUT,3) - JPVEXT
IMI = GET_CURRENT_MODEL_INDEX()
!
!-------------------------------------------------------------------------------
!
!*       2.    UPPER AND LOWER BC FILLING:   
!              ---------------------------
!
!*       2.1    COMPUTE THE FIELD EXTRAPOLATIONS AT THE GROUND
!

!
!          at the instant t
!
IF(SIZE(PUT) /= 0) PUT  (:,:,IKB-1)   = PUT  (:,:,IKB) 
IF(SIZE(PVT) /= 0) PVT  (:,:,IKB-1)   = PVT  (:,:,IKB) 
IF(SIZE(PWT) /= 0) PWT  (:,:,IKB-1)   = PWT  (:,:,IKB)  
IF(SIZE(PTHT) /= 0) PTHT (:,:,IKB-1)  = PTHT (:,:,IKB) 
IF(SIZE(PTKET) /= 0) PTKET(:,:,IKB-1) = PTKET(:,:,IKB)
IF(SIZE(PRT) /= 0)  PRT  (:,:,IKB-1,:)= PRT  (:,:,IKB,:)
IF(SIZE(PSVT)/= 0)  PSVT (:,:,IKB-1,:)= PSVT (:,:,IKB,:)
IF(SIZE(PSRCT) /= 0) PSRCT(:,:,IKB-1)   = PSRCT(:,:,IKB)
!
!
!*       2.2    COMPUTE THE FIELD EXTRAPOLATIONS AT THE TOP
!
!          at the instant t
!
IF(SIZE(PWT) /= 0) PWT  (:,:,IKE+1)   = 0.           
IF(SIZE(PUT) /= 0) PUT  (:,:,IKE+1)   = PUT  (:,:,IKE) 
IF(SIZE(PVT) /= 0) PVT  (:,:,IKE+1)   = PVT  (:,:,IKE)
IF(SIZE(PTHT) /= 0) PTHT (:,:,IKE+1)  = PTHT (:,:,IKE)
IF(SIZE(PTKET) /= 0) PTKET(:,:,IKE+1) = PTKET(:,:,IKE)
IF(SIZE(PRT) /= 0) PRT  (:,:,IKE+1,:) = PRT  (:,:,IKE,:)
IF(SIZE(PSVT)/= 0) PSVT (:,:,IKE+1,:) = PSVT (:,:,IKE,:)
IF(SIZE(PSRCT) /= 0) PSRCT(:,:,IKE+1)   = PSRCT(:,:,IKE)

! specific for positive and negative ions mixing ratios (1/kg)

IF (NSV_ELEC .NE. 0) THEN
!
   IF (SIZE(PWT) /= 0) THEN
     WHERE ( PWT(:,:,IKE+1)  .GE. 0.)    ! Outflow
         PSVT (:,:,IKE+1,NSV_ELECBEG) = 2.*PSVT (:,:,IKE,NSV_ELECBEG) -  &
                                           PSVT (:,:,IKE-1,NSV_ELECBEG)
         PSVT (:,:,IKE+1,NSV_ELECEND) = 2.*PSVT (:,:,IKE,NSV_ELECEND) -  &
                                           PSVT (:,:,IKE-1,NSV_ELECEND)
     ELSE WHERE                         ! Inflow from the top
         PSVT (:,:,IKE+1,NSV_ELECBEG) = XCION_POS_FW(:,:,IKE+1)
         PSVT (:,:,IKE+1,NSV_ELECEND) = XCION_NEG_FW(:,:,IKE+1)
     END WHERE
   ENDIF
!
END IF

!
!
!-------------------------------------------------------------------------------
!
!*       3.    COMPUTE LB FIELDS AT TIME T
!              ---------------------------
!
!
IF ( KTCOUNT == 1) THEN
  ZTSTEP = 0.
ELSE
  ZTSTEP = PTSTEP
END IF
!
!
IF ( SIZE(PLBXTHS,1) /= 0 .AND.                      &
   (     HLBCX(1)=='OPEN' .OR. HLBCX(2)=='OPEN')     ) THEN            
  ZLBXVT(:,:,:) = PLBXVM(:,:,:) + ZTSTEP * PLBXVS(:,:,:)
  ZLBXWT(:,:,:) = PLBXWM(:,:,:) + ZTSTEP * PLBXWS(:,:,:)
  ZLBXTHT(:,:,:) = PLBXTHM(:,:,:) + ZTSTEP * PLBXTHS(:,:,:)
  IF ( SIZE(PTKET,1) /= 0 ) THEN
    ZLBXTKET(:,:,:) = PLBXTKEM(:,:,:) + ZTSTEP * PLBXTKES(:,:,:)
  END IF
  IF ( KRR > 0) THEN
    ZLBXRT(:,:,:,:) = PLBXRM(:,:,:,:) + ZTSTEP * PLBXRS(:,:,:,:)
  END IF
  IF ( KSV > 0) THEN
    ZLBXSVT(:,:,:,:) = PLBXSVM(:,:,:,:) + ZTSTEP * PLBXSVS(:,:,:,:)
  END IF
!
ELSE
!
  ZLBXVT(:,:,:) = PLBXVM(:,:,:)
  ZLBXWT(:,:,:) = PLBXWM(:,:,:)
  ZLBXTHT(:,:,:) = PLBXTHM(:,:,:)
  IF ( SIZE(PTKET,1) /= 0 ) THEN
    ZLBXTKET(:,:,:) = PLBXTKEM(:,:,:) 
  END IF
  IF ( KRR > 0) THEN
    ZLBXRT(:,:,:,:) = PLBXRM(:,:,:,:) 
  END IF
  IF ( KSV > 0) THEN
    ZLBXSVT(:,:,:,:) = PLBXSVM(:,:,:,:) 
  END IF
!
END IF
!
IF ( SIZE(PLBYTHS,1) /= 0 .AND.                       &
     (    HLBCY(1)=='OPEN' .OR. HLBCY(2)=='OPEN'    ))  THEN          
  ZLBYUT(:,:,:) = PLBYUM(:,:,:) + ZTSTEP * PLBYUS(:,:,:)
  ZLBYWT(:,:,:) = PLBYWM(:,:,:) + ZTSTEP * PLBYWS(:,:,:)
  ZLBYTHT(:,:,:) = PLBYTHM(:,:,:) + ZTSTEP * PLBYTHS(:,:,:)
  IF ( SIZE(PTKET,1) /= 0 ) THEN
    ZLBYTKET(:,:,:) = PLBYTKEM(:,:,:) + ZTSTEP * PLBYTKES(:,:,:)
  END IF
  IF ( KRR > 0) THEN
    ZLBYRT(:,:,:,:) = PLBYRM(:,:,:,:) + ZTSTEP * PLBYRS(:,:,:,:)
  END IF
  IF ( KSV > 0) THEN
    ZLBYSVT(:,:,:,:) = PLBYSVM(:,:,:,:) + ZTSTEP * PLBYSVS(:,:,:,:)
  END IF
!
ELSE
!
  ZLBYUT(:,:,:) = PLBYUM(:,:,:)
  ZLBYWT(:,:,:) = PLBYWM(:,:,:)
  ZLBYTHT(:,:,:) = PLBYTHM(:,:,:)
  IF ( SIZE(PTKET,1) /= 0 ) THEN
    ZLBYTKET(:,:,:) = PLBYTKEM(:,:,:) 
  END IF
  IF ( KRR > 0) THEN
    ZLBYRT(:,:,:,:) = PLBYRM(:,:,:,:) 
  END IF
  IF ( KSV > 0) THEN
    ZLBYSVT(:,:,:,:) = PLBYSVM(:,:,:,:) 
  END IF
!
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       4.    LBC FILLING IN THE X DIRECTION (LEFT WEST SIDE):   
!              ------------------------------------------------
IF (LWEST_ll( )) THEN
!
!
SELECT CASE ( HLBCX(1) )  
!
!*       4.1  WALL CASE:  
!             ========= 
!
  CASE ('WALL')
!
    DO JEXT=1,JPHEXT
       IF(SIZE(PUT) /= 0) PUT  (IIB-JEXT,:,:)   = PUT  (IIB       ,:,:)   ! never used during run
       IF(SIZE(PVT) /= 0) PVT  (IIB-JEXT,:,:)   = PVT  (IIB-1+JEXT,:,:)
       IF(SIZE(PWT) /= 0) PWT  (IIB-JEXT,:,:)   = PWT  (IIB-1+JEXT,:,:)
       IF(SIZE(PTHT) /= 0) PTHT(IIB-JEXT,:,:)   = PTHT (IIB-1+JEXT,:,:)
       IF(SIZE(PTKET)/= 0) PTKET(IIB-JEXT,:,:)  = PTKET(IIB-1+JEXT,:,:)
       IF(SIZE(PRT) /= 0) PRT  (IIB-JEXT,:,:,:) = PRT  (IIB-1+JEXT,:,:,:)
       IF(SIZE(PSVT) /= 0) PSVT(IIB-JEXT,:,:,:) = PSVT (IIB-1+JEXT,:,:,:)
       IF(SIZE(PSRCT) /= 0) PSRCT (IIB-JEXT,:,:)   = PSRCT (IIB-1+JEXT,:,:)
!
    END DO
!
    IF(SIZE(PUT) /= 0) PUT(IIB     ,:,:)   = 0.    ! set the normal velocity
!
!
!*       4.2  OPEN CASE:
!             =========
!
  CASE ('OPEN')
!
    IF(SIZE(PUT) /= 0) THEN
      PUT(IIB-JPHEXT,:,:)=0.
      WHERE ( PUT(IIB,:,:) <= 0. )               !  OUTFLOW condition
        PVT  (IIB-1,:,:) = 2.*PVT  (IIB,:,:)   -PVT  (IIB+1,:,:)
        PWT  (IIB-1,:,:) = 2.*PWT  (IIB,:,:)   -PWT  (IIB+1,:,:)
        PTHT (IIB-1,:,:) = 2.*PTHT (IIB,:,:)   -PTHT (IIB+1,:,:)
!
      ELSEWHERE                                   !  INFLOW  condition
        PVT  (IIB-1,:,:) = ZLBXVT   (1,:,:)
        PWT  (IIB-1,:,:) = ZLBXWT   (1,:,:) 
        PTHT (IIB-1,:,:) = ZLBXTHT  (1,:,:)
      ENDWHERE
    ENDIF
!
!
  IF(SIZE(PTKET) /= 0) THEN
    WHERE ( PUT(IIB,:,:) <= 0. )               !  OUTFLOW condition
      PTKET(IIB-1,:,:) = MAX(XTKEMIN, 2.*PTKET(IIB,:,:)-PTKET(IIB+1,:,:))  
    ELSEWHERE                                  !  INFLOW  condition
      PTKET(IIB-1,:,:) = MAX(XTKEMIN,ZLBXTKET(1,:,:))  
    ENDWHERE
  END IF
    !
!                      Case with KRR moist variables 
! 
! 
!
  DO JRR =1 ,KRR
    IF(SIZE(PUT) /= 0) THEN
      WHERE ( PUT(IIB,:,:) <= 0. )         !  OUTFLOW condition
        PRT(IIB-1,:,:,JRR) = MAX(0.,2.*PRT(IIB,:,:,JRR) -PRT(IIB+1,:,:,JRR))
      ELSEWHERE                            !  INFLOW  condition
        PRT(IIB-1,:,:,JRR) = MAX(0.,ZLBXRT(1,:,:,JRR))
      END WHERE
    END IF
    !
  END DO
!
  IF(SIZE(PSRCT) /= 0) PSRCT (IIB-1,:,:)   = PSRCT (IIB,:,:)
!
!                       Case with KSV scalar variables  
  DO JSV=1 ,KSV
    IF(SIZE(PUT) /= 0) THEN
      WHERE ( PUT(IIB,:,:) <= 0. )         !  OUTFLOW condition
        PSVT(IIB-1,:,:,JSV) = MAX(XSVMIN(JSV),2.*PSVT(IIB,:,:,JSV) - &
                                               PSVT(IIB+1,:,:,JSV))
      ELSEWHERE                            !  INFLOW  condition
        PSVT(IIB-1,:,:,JSV) = MAX(XSVMIN(JSV),ZLBXSVT(1,:,:,JSV))
      END WHERE
    END IF
    !
  END DO
!
!
END SELECT
!
END IF
!-------------------------------------------------------------------------------
!
!*       5    LBC FILLING IN THE X DIRECTION (RIGHT EAST SIDE): 
!              ===============--------------------------------
!
IF (LEAST_ll( )) THEN
!
SELECT CASE ( HLBCX(2) ) 
!
!*       5.1  WALL CASE:
!             =========
!
  CASE ('WALL')
!
    DO JEXT=1,JPHEXT
       IF(SIZE(PUT) /= 0) PUT  (IIE+JEXT,:,:)   = PUT  (IIE       ,:,:)   ! never used during run
       IF(SIZE(PVT) /= 0) PVT  (IIE+JEXT,:,:)   = PVT  (IIE+1-JEXT,:,:)
       IF(SIZE(PWT) /= 0) PWT  (IIE+JEXT,:,:)   = PWT  (IIE+1-JEXT,:,:)
       IF(SIZE(PTHT) /= 0) PTHT (IIE+JEXT,:,:)  = PTHT (IIE+1-JEXT,:,:)
       IF(SIZE(PTKET) /= 0) PTKET(IIE+JEXT,:,:) = PTKET(IIE+1-JEXT,:,:)
       IF(SIZE(PRT) /= 0) PRT  (IIE+JEXT,:,:,:) = PRT  (IIE+1-JEXT,:,:,:)
       IF(SIZE(PSVT) /= 0) PSVT(IIE+JEXT,:,:,:) = PSVT (IIE+1-JEXT,:,:,:)
       IF(SIZE(PSRCT) /= 0) PSRCT (IIE+JEXT,:,:)= PSRCT (IIE+1-JEXT,:,:)
!
    END DO
!
    IF(SIZE(PUT) /= 0) PUT(IIE+1   ,:,:)   = 0.     ! set the normal velocity
!
!*       5.2  OPEN CASE:
!             =========
!
  CASE ('OPEN')
!
    ILBX = SIZE(PLBXVM,1)
    IF(SIZE(PUT) /= 0) THEN
      WHERE ( PUT(IIE+1,:,:) >= 0. )               !  OUTFLOW condition
        PVT  (IIE+1,:,:) = 2.*PVT  (IIE,:,:)   -PVT  (IIE-1,:,:)
        PWT  (IIE+1,:,:) = 2.*PWT  (IIE,:,:)   -PWT  (IIE-1,:,:)
        PTHT (IIE+1,:,:) = 2.*PTHT (IIE,:,:)   -PTHT (IIE-1,:,:)
!
      ELSEWHERE                                   !  INFLOW  condition
        PVT  (IIE+1,:,:) = ZLBXVT   (ILBX,:,:)
        PWT  (IIE+1,:,:) = ZLBXWT   (ILBX,:,:) 
        PTHT (IIE+1,:,:) = ZLBXTHT  (ILBX,:,:)
      ENDWHERE
    ENDIF
!
  IF(SIZE(PTKET) /= 0) THEN
    ILBX = SIZE(PLBXTKEM,1)
    WHERE ( PUT(IIE+1,:,:) >= 0. )             !  OUTFLOW condition
      PTKET(IIE+1,:,:) = MAX(XTKEMIN, 2.*PTKET(IIE,:,:)-PTKET(IIE-1,:,:))  
    ELSEWHERE                                  !  INFLOW  condition
      PTKET(IIE+1,:,:) = MAX(XTKEMIN,ZLBXTKET(ILBX,:,:))  
    ENDWHERE
  END IF
    !
!
!                      Case with KRR moist variables 
! 
! 
  DO JRR =1 ,KRR
    ILBX=SIZE(PLBXRM,1)
    !
    IF(SIZE(PUT) /= 0) THEN
      WHERE ( PUT(IIE+1,:,:) >= 0. )       !  OUTFLOW condition
        PRT(IIE+1,:,:,JRR) = MAX(0.,2.*PRT(IIE,:,:,JRR) -PRT(IIE-1,:,:,JRR))
      ELSEWHERE                            !  INFLOW  condition
        PRT(IIE+1,:,:,JRR) = MAX(0.,ZLBXRT(ILBX,:,:,JRR))
      END WHERE
    END IF
    !
  END DO
!
  IF(SIZE(PSRCT) /= 0) PSRCT (IIE+1,:,:)   = PSRCT (IIE,:,:)
!                       Case with KSV scalar variables  
  DO JSV=1 ,KSV
    ILBX=SIZE(PLBXSVM,1)
    IF(SIZE(PUT) /= 0) THEN
      WHERE ( PUT(IIE+1,:,:) >= 0. )       !  OUTFLOW condition
        PSVT(IIE+1,:,:,JSV) = MAX(XSVMIN(JSV),2.*PSVT(IIE,:,:,JSV) - &
                                               PSVT(IIE-1,:,:,JSV))
      ELSEWHERE                            !  INFLOW  condition
        PSVT(IIE+1,:,:,JSV) = MAX(XSVMIN(JSV),ZLBXSVT(ILBX,:,:,JSV))
      END WHERE
    END IF
    !
  END DO
!
!
END SELECT
!
END IF
!-------------------------------------------------------------------------------
!
!*       6.    LBC FILLING IN THE Y DIRECTION (BOTTOM SOUTH SIDE): 
!              ------------------------------
IF (LSOUTH_ll( )) THEN
!
SELECT CASE ( HLBCY(1) )              
!
!*       6.1  WALL CASE:
!             ========= 
!
  CASE ('WALL')
!
    DO JEXT=1,JPHEXT
       IF(SIZE(PUT) /= 0) PUT  (:,IJB-JEXT,:)   = PUT  (:,IJB-1+JEXT,:)
       IF(SIZE(PVT) /= 0) PVT  (:,IJB-JEXT,:)   = PVT  (:,IJB       ,:)   ! never used during run
       IF(SIZE(PWT) /= 0) PWT  (:,IJB-JEXT,:)   = PWT  (:,IJB-1+JEXT,:)
       IF(SIZE(PTHT) /= 0) PTHT (:,IJB-JEXT,:)  = PTHT (:,IJB-1+JEXT,:)
       IF(SIZE(PTKET) /= 0) PTKET(:,IJB-JEXT,:) = PTKET(:,IJB-1+JEXT,:)
       IF(SIZE(PRT) /= 0) PRT  (:,IJB-JEXT,:,:) = PRT  (:,IJB-1+JEXT,:,:)
       IF(SIZE(PSVT) /= 0) PSVT (:,IJB-JEXT,:,:)= PSVT (:,IJB-1+JEXT,:,:)
       IF(SIZE(PSRCT) /= 0) PSRCT(:,IJB-JEXT,:) = PSRCT(:,IJB-1+JEXT,:)
!
    END DO
!
    IF(SIZE(PVT) /= 0) PVT(:,IJB     ,:)   = 0.       ! set the normal velocity
!
!*       6.2  OPEN CASE:
!             =========
!
  CASE ('OPEN')
!
    IF(SIZE(PVT) /= 0) THEN
      PVT(:,IJB-JPHEXT,:)=0.
      WHERE ( PVT(:,IJB,:) <= 0. )               !  OUTFLOW condition
        PUT  (:,IJB-1,:) = 2.*PUT  (:,IJB,:)   -PUT  (:,IJB+1,:)
        PWT  (:,IJB-1,:) = 2.*PWT  (:,IJB,:)   -PWT  (:,IJB+1,:)
        PTHT (:,IJB-1,:) = 2.*PTHT (:,IJB,:)   -PTHT (:,IJB+1,:)
      ELSEWHERE                                   !  INFLOW  condition
        PUT  (:,IJB-1,:) = ZLBYUT   (:,1,:)
        PWT  (:,IJB-1,:) = ZLBYWT   (:,1,:) 
        PTHT (:,IJB-1,:) = ZLBYTHT  (:,1,:)
      ENDWHERE
    ENDIF
!
  IF(SIZE(PTKET) /= 0) THEN
    WHERE ( PVT(:,IJB,:) <= 0. )             !  OUTFLOW condition
      PTKET(:,IJB-1,:) = MAX(XTKEMIN, 2.*PTKET(:,IJB,:)-PTKET(:,IJB+1,:))  
    ELSEWHERE                                !  INFLOW  condition
      PTKET(:,IJB-1,:) = MAX(XTKEMIN,ZLBYTKET(:,1,:))  
    ENDWHERE
  END IF
    !
!
!                      Case with KRR moist variables 
! 
! 
  DO JRR =1 ,KRR
    IF(SIZE(PVT) /= 0) THEN
      WHERE ( PVT(:,IJB,:) <= 0. )         !  OUTFLOW condition
        PRT(:,IJB-1,:,JRR) = MAX(0.,2.*PRT(:,IJB,:,JRR) -PRT(:,IJB+1,:,JRR))
      ELSEWHERE                            !  INFLOW  condition
        PRT(:,IJB-1,:,JRR) = MAX(0.,ZLBYRT(:,1,:,JRR))
      END WHERE
    END IF
    !
  END DO
!
  IF(SIZE(PSRCT) /= 0) PSRCT(:,IJB-1,:)   = PSRCT(:,IJB,:)
!
!                       Case with KSV scalar variables  
!
  DO JSV=1 ,KSV
    IF(SIZE(PVT) /= 0) THEN
      WHERE ( PVT(:,IJB,:) <= 0. )         !  OUTFLOW condition
        PSVT(:,IJB-1,:,JSV) = MAX(XSVMIN(JSV),2.*PSVT(:,IJB,:,JSV) - &
                                               PSVT(:,IJB+1,:,JSV))
      ELSEWHERE                            !  INFLOW  condition
        PSVT(:,IJB-1,:,JSV) = MAX(XSVMIN(JSV),ZLBYSVT(:,1,:,JSV))
      END WHERE
    END IF
    !
  END DO
!
!
END SELECT
!
END IF
!-------------------------------------------------------------------------------
!
!*       7.    LBC FILLING IN THE Y DIRECTION (TOP NORTH SIDE): 
!              ===============
!
IF (LNORTH_ll( )) THEN
!
SELECT CASE ( HLBCY(2) ) 
!
!*       4.3.1  WALL CASE:
!               ========= 
!
  CASE ('WALL')
!
    DO JEXT=1,JPHEXT
       IF(SIZE(PUT) /= 0) PUT  (:,IJE+JEXT,:)   = PUT  (:,IJE+1-JEXT,:)
       IF(SIZE(PVT) /= 0) PVT  (:,IJE+JEXT,:)   = PVT  (:,IJE       ,:)   ! never used during run
       IF(SIZE(PWT) /= 0) PWT  (:,IJE+JEXT,:)   = PWT  (:,IJE+1-JEXT,:)
       IF(SIZE(PTHT) /= 0) PTHT (:,IJE+JEXT,:)  = PTHT (:,IJE+1-JEXT,:)
       IF(SIZE(PTKET) /= 0) PTKET(:,IJE+JEXT,:) = PTKET(:,IJE+1-JEXT,:)
       IF(SIZE(PRT) /= 0) PRT  (:,IJE+JEXT,:,:) = PRT  (:,IJE+1-JEXT,:,:)
       IF(SIZE(PSVT) /= 0) PSVT (:,IJE+JEXT,:,:)= PSVT (:,IJE+1-JEXT,:,:)
       IF(SIZE(PSRCT) /= 0) PSRCT(:,IJE+JEXT,:) = PSRCT(:,IJE+1-JEXT,:)
!
    END DO
!
    IF(SIZE(PVT) /= 0) PVT(:,IJE+1   ,:)   = 0.    ! set the normal velocity
!
!*       4.3.2  OPEN CASE:
!               ========= 
!
  CASE ('OPEN')
!
!
    ILBY=SIZE(PLBYUM,2)
    IF(SIZE(PVT) /= 0) THEN
      WHERE ( PVT(:,IJE+1,:) >= 0. )               !  OUTFLOW condition
        PUT  (:,IJE+1,:) = 2.*PUT  (:,IJE,:)   -PUT  (:,IJE-1,:)
        PWT  (:,IJE+1,:) = 2.*PWT  (:,IJE,:)   -PWT  (:,IJE-1,:)
        PTHT (:,IJE+1,:) = 2.*PTHT (:,IJE,:)   -PTHT (:,IJE-1,:)
      ELSEWHERE                                   !  INFLOW  condition
        PUT  (:,IJE+1,:) = ZLBYUT   (:,ILBY,:)
        PWT  (:,IJE+1,:) = ZLBYWT   (:,ILBY,:) 
        PTHT (:,IJE+1,:) = ZLBYTHT  (:,ILBY,:)
      ENDWHERE
    ENDIF
!
  IF(SIZE(PTKET) /= 0) THEN
    ILBY=SIZE(PLBYTKEM,2)
    WHERE ( PVT(:,IJE+1,:) >= 0. )             !  OUTFLOW condition
      PTKET(:,IJE+1,:) = MAX(XTKEMIN, 2.*PTKET(:,IJE,:)-PTKET(:,IJE-1,:))  
    ELSEWHERE                                  !  INFLOW  condition
      PTKET(:,IJE+1,:) = MAX(XTKEMIN,ZLBYTKET(:,ILBY,:))  
    ENDWHERE
  ENDIF
    !
!                      Case with KRR moist variables 
! 
! 
  DO JRR =1 ,KRR
    ILBY=SIZE(PLBYRM,2)
    !
    IF(SIZE(PVT) /= 0) THEN
      WHERE ( PVT(:,IJE+1,:) >= 0. )         !  OUTFLOW condition
        PRT(:,IJE+1,:,JRR) = MAX(0.,2.*PRT(:,IJE,:,JRR) -PRT(:,IJE-1,:,JRR))
      ELSEWHERE                            !  INFLOW  condition
        PRT(:,IJE+1,:,JRR) = MAX(0.,ZLBYRT(:,ILBY,:,JRR))
      END WHERE
    END IF
    !
  END DO
!
  IF(SIZE(PSRCT) /= 0) PSRCT(:,IJE+1,:)   = PSRCT(:,IJE,:)
!
!                       Case with KSV scalar variables  
  DO JSV=1 ,KSV
    ILBY=SIZE(PLBYSVM,2)
    !
    IF(SIZE(PVT) /= 0) THEN
      WHERE ( PVT(:,IJE+1,:) >= 0. )         !  OUTFLOW condition
        PSVT(:,IJE+1,:,JSV) = MAX(XSVMIN(JSV),2.*PSVT(:,IJE,:,JSV) - &
                                               PSVT(:,IJE-1,:,JSV))
      ELSEWHERE                            !  INFLOW  condition
        PSVT(:,IJE+1,:,JSV) = MAX(XSVMIN(JSV),ZLBYSVT(:,ILBY,:,JSV))
      END WHERE
    END IF
    !
  END DO
!
!
END SELECT
END IF
!
!
IF (LUSECHEM .AND. IMI == 1) THEN
  IF (GFIRSTCALL1) THEN
    ALLOCATE(GCHBOUNDARY(NSV_CHEM))
    GFIRSTCALL1 = .FALSE.
    DO JSV=NSV_CHEMBEG,NSV_CHEMEND
       GCHTMP = .FALSE.
       IF (LWEST_ll().AND.HLBCX(1)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(1,:,:,JSV)==0)
       IF (LEAST_ll().AND.HLBCX(2)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(ILBX,:,:,JSV)==0)
       IF (LSOUTH_ll().AND.HLBCY(1)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,1,:,JSV)==0)
       IF (LNORTH_ll().AND.HLBCY(2)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,ILBY,:,JSV)==0)
       GCHBOUNDARY(JSV-NSV_CHEMBEG+1) = GCHTMP
    ENDDO
  ENDIF

  DO JSV=NSV_CHEMBEG,NSV_CHEMEND
    IF (GCHBOUNDARY(JSV-NSV_CHEMBEG+1))  THEN
      IF (SIZE(PSVT)>0) THEN
        CALL CH_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT(:,:,:,JSV))
      ENDIF
    ENDIF
  ENDDO
ENDIF
!
IF (LUSECHIC .AND. IMI == 1) THEN
  IF (GFIRSTCALLIC) THEN
    ALLOCATE(GICBOUNDARY(NSV_CHIC))
    GFIRSTCALLIC = .FALSE.
    DO JSV=NSV_CHICBEG,NSV_CHICEND
       GCHTMP = .FALSE.
       IF (LWEST_ll().AND.HLBCX(1)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(1,:,:,JSV)==0)
       IF (LEAST_ll().AND.HLBCX(2)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(ILBX,:,:,JSV)==0)
       IF (LSOUTH_ll().AND.HLBCY(1)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,1,:,JSV)==0)
       IF (LNORTH_ll().AND.HLBCY(2)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,ILBY,:,JSV)==0)
       GICBOUNDARY(JSV-NSV_CHICBEG+1) = GCHTMP
    ENDDO
  ENDIF

  DO JSV=NSV_CHICBEG,NSV_CHICEND
    IF (GICBOUNDARY(JSV-NSV_CHICBEG+1))  THEN
      IF (SIZE(PSVT)>0) THEN
        CALL CH_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT(:,:,:,JSV))
      ENDIF
    ENDIF
  ENDDO
ENDIF
IF (LORILAM .AND. IMI == 1) THEN
  IF (GFIRSTCALL2) THEN
    ALLOCATE(GAERBOUNDARY(NSV_AER))
    GFIRSTCALL2 = .FALSE.
    DO JSV=NSV_AERBEG,NSV_AEREND
       GCHTMP = .FALSE.
       IF (LWEST_ll().AND.HLBCX(1)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(1,:,:,JSV)==0)
       IF (LEAST_ll().AND.HLBCX(2)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(ILBX,:,:,JSV)==0)
       IF (LSOUTH_ll().AND.HLBCY(1)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,1,:,JSV)==0)
       IF (LNORTH_ll().AND.HLBCY(2)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,ILBY,:,JSV)==0)
       GAERBOUNDARY(JSV-NSV_AERBEG+1) = GCHTMP
    ENDDO
  ENDIF

  DO JSV=NSV_AERBEG,NSV_AEREND
    IF (GAERBOUNDARY(JSV-NSV_AERBEG+1)) THEN
      IF (SIZE(PSVT)>0) THEN
        CALL CH_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT(:,:,:,JSV))
      ENDIF
    ENDIF
  ENDDO
ENDIF
!
IF (LDUST .AND. IMI == 1) THEN
  IF (GFIRSTCALL3) THEN
    ALLOCATE(GDSTBOUNDARY(NSV_DST))
    GFIRSTCALL3 = .FALSE.
    DO JSV=NSV_DSTBEG,NSV_DSTEND
       GCHTMP = .FALSE.
       IF (LWEST_ll().AND.HLBCX(1)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(1,:,:,JSV)==0)
       IF (LEAST_ll().AND.HLBCX(2)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(ILBX,:,:,JSV)==0)
       IF (LSOUTH_ll().AND.HLBCY(1)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,1,:,JSV)==0)
       IF (LNORTH_ll().AND.HLBCY(2)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,ILBY,:,JSV)==0)
       GDSTBOUNDARY(JSV-NSV_DSTBEG+1) = GCHTMP
    ENDDO
    ENDIF

  DO JSV=NSV_DSTBEG,NSV_DSTEND
    IF (GDSTBOUNDARY(JSV-NSV_DSTBEG+1)) THEN
      IF (SIZE(PSVT)>0) THEN
        CALL CH_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT(:,:,:,JSV))
      ENDIF
    ENDIF
  ENDDO 
ENDIF
!
IF (LSALT .AND. IMI == 1) THEN
  IF (GFIRSTCALL5) THEN
    ALLOCATE(GSLTBOUNDARY(NSV_SLT))
    GFIRSTCALL5 = .FALSE.
    DO JSV=NSV_SLTBEG,NSV_SLTEND
       GCHTMP = .FALSE.
       IF (LWEST_ll().AND.HLBCX(1)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(1,:,:,JSV)==0)
       IF (LEAST_ll().AND.HLBCX(2)=='OPEN')  GCHTMP = GCHTMP .OR. ALL(PLBXSVM(ILBX,:,:,JSV)==0)
       IF (LSOUTH_ll().AND.HLBCY(1)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,1,:,JSV)==0)
       IF (LNORTH_ll().AND.HLBCY(2)=='OPEN') GCHTMP = GCHTMP .OR. ALL(PLBYSVM(:,ILBY,:,JSV)==0)
       GSLTBOUNDARY(JSV-NSV_SLTBEG+1) = GCHTMP
    ENDDO
  ENDIF

  DO JSV=NSV_SLTBEG,NSV_SLTEND
    IF (GSLTBOUNDARY(JSV-NSV_SLTBEG+1)) THEN
      IF (SIZE(PSVT)>0) THEN
        CALL CH_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT(:,:,:,JSV))
      ENDIF
    ENDIF
  ENDDO
ENDIF
!
IF ( LPASPOL .AND. IMI == 1) THEN
  IF (GFIRSTCALLPP) THEN
    ALLOCATE(GPPBOUNDARY(NSV_PP))
    GFIRSTCALLPP = .FALSE.
    DO JSV=NSV_PPBEG,NSV_PPEND
      GPPTMP = .FALSE.
      IF (LWEST_ll().AND.HLBCX(1)=='OPEN') GPPTMP = GPPTMP .OR. ALL(PLBXSVM(1,:,:,JSV)==0)
      IF (LEAST_ll().AND.HLBCX(2)=='OPEN') GPPTMP = GPPTMP .OR. ALL(PLBXSVM(ILBX,:,:,JSV)==0)
      IF (LSOUTH_ll().AND.HLBCY(1)=='OPEN') GPPTMP = GPPTMP .OR. ALL(PLBYSVM(:,1,:,JSV)==0)
      IF (LNORTH_ll().AND.HLBCY(2)=='OPEN') GPPTMP = GPPTMP .OR. ALL(PLBYSVM(:,ILBY,:,JSV)==0)
      GPPBOUNDARY(JSV-NSV_PPBEG+1) = GPPTMP
    ENDDO
  ENDIF

  DO JSV=NSV_PPBEG,NSV_PPEND
    IF (GPPBOUNDARY(JSV-NSV_PPBEG+1)) THEN
      IF (SIZE(PSVT)>0) THEN
       CALL CH_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT(:,:,:,JSV))
      ENDIF
    ENDIF
  ENDDO
ENDIF
!
IF ( LCONDSAMP .AND. IMI == 1) THEN
  IF (GFIRSTCALLCS) THEN
    ALLOCATE(GCSBOUNDARY(NSV_CS))
    GFIRSTCALLCS = .FALSE.
    DO JSV=NSV_CSBEG,NSV_CSEND
      GCSTMP = .FALSE.
      IF (LWEST_ll().AND.HLBCX(1)=='OPEN') GCSTMP = GCSTMP .OR. ALL(PLBXSVM(1,:,:,JSV)==0)
      IF (LEAST_ll().AND.HLBCX(2)=='OPEN') GCSTMP = GCSTMP .OR. ALL(PLBXSVM(ILBX,:,:,JSV)==0)
      IF (LSOUTH_ll().AND.HLBCY(1)=='OPEN') GCSTMP = GCSTMP .OR. ALL(PLBYSVM(:,1,:,JSV)==0)
      IF (LNORTH_ll().AND.HLBCY(2)=='OPEN') GCSTMP = GCSTMP .OR. ALL(PLBYSVM(:,ILBY,:,JSV)==0)
      GCSBOUNDARY(JSV-NSV_CSBEG+1) = GCSTMP
    ENDDO
  ENDIF

  DO JSV=NSV_CSBEG,NSV_CSEND
    IF (GCSBOUNDARY(JSV-NSV_CSBEG+1)) THEN
      IF (SIZE(PSVT)>0) THEN
       CALL CH_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT(:,:,:,JSV))
      ENDIF
    ENDIF
  ENDDO
ENDIF
!
IF ( CELEC /= 'NONE' .AND. IMI == 1) THEN
  CALL ION_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT)
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE BOUNDARIES
