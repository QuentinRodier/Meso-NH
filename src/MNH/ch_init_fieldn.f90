!MNH_LIC Copyright 1995-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!    ########################### 
      MODULE MODI_CH_INIT_FIELD_n
!!    ########################### 
!!
INTERFACE
!!
SUBROUTINE CH_INIT_FIELD_n(KMI, KLUOUT, KVERB)
!!
IMPLICIT NONE
!!
INTEGER, INTENT(IN)  :: KMI      ! model index
INTEGER, INTENT(IN)  :: KLUOUT   ! output listing channel
INTEGER, INTENT(IN)  :: KVERB    ! verbosity level
!!
!!
END SUBROUTINE CH_INIT_FIELD_n
!!
END INTERFACE
!!
END MODULE MODI_CH_INIT_FIELD_n
!!
!!    ##############################################
      SUBROUTINE CH_INIT_FIELD_n(KMI, KLUOUT, KVERB)
!!    ##############################################
!!
!!*** *CH_INIT_FIELD_n*
!!
!!    PURPOSE
!!    -------
!        initialize MesoNH scalar variables
!!
!!**  METHOD
!!    ------
!!       The subroutine CH_FIELD_VALUE_n returns for each grid-point 
!!    (LAT,LON,ZZ) and each species a corresponding initial value, either
!!    in part/part or in molec/cm3. If necessary, that initial value is 
!!    then converted to mixing ratio (part/part).
!!    The variables at time t and t-dt are given identic values.
!!    Presently, there is only a 1D initialization (homogeneous in x-y)
!!    available. For more sophisticated initializations, the subroutine
!!    CH_FIELD_VALUE_n may be modified by the user. The character parameter
!!    CCH_INIT_FIELD_OPT may be used in order to pass user specific information
!!    on to that subroutine. These subroutines have been duplicated in order
!!    to allow future inclusion of model dependant parameters (like an
!!    initialization that depends on variables stored in MODD_FIELD_n)
!!
!!    REFERENCE
!!    ---------
!!    book 2 of MesoNH
!!
!!    AUTHOR
!!    ------
!!    K. Suhre   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original 02/11/95
!!    05/08/96 (K. Suhre) restructured
!!    11/08/98 (N. Asencio) add parallel code
!!    09/03/99 (V. Crassier) speed up 1-D initialization by reducing a 3-D
!!                           loop to a 1-D loop with 10m precision
!!    09/12/99 (K. Suhre) add missing update halo and a fix for MAXVAL pbs.
!!    09/01/01 (P. Tulet) initialize chemical constant (molar mass, henry 
!!                        specific constant and biological reactivity
!!    22/01/01 (D. Gazen) add NSV_CHEMBEG and NSV_CHEMEND indices to handle SV
!!    04/06/07 (M. Leriche & JP Pinty) add pH initialization
!!    20/04/10 (M. Leriche) remove pH initialization to ini_modeln
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!  P. Tulet    20/05/2021: correction for CON to MIX transformation unit (aerosols only)
!  P. Wautelet 07/09/2022: correction for array boundaries + cleaning
!------------------------------------------------------------------------------
!
USE MODD_ARGSLIST_ll, ONLY: LIST_ll  ! for update_halo
USE MODD_CONF,        ONLY: CPROGRAM, L1D, L2D
USE MODD_CH_AERO_n
USE MODD_CH_AEROSOL
USE MODD_CH_M9_n,     ONLY: CNAMES, NEQ
USE MODD_CH_MNHC_n,   ONLY: CCH_SCHEME, LCH_INIT_FIELD
USE MODD_CST,         ONLY: XMD, XAVOGADRO
USE MODD_FIELD_n,     ONLY: XSVT       ! scalar variable at t
USE MODD_GRID_n,      ONLY: XZZ   ! height z
USE MODD_LSFIELD_n,   ONLY: XLBXSVM, XLBYSVM
USE MODD_NSV,         ONLY: NSV_CHEM, NSV_CHEMBEG,NSV_CHEMEND, &
                            NSV_AER, NSV_AERBEG,NSV_AEREND
USE MODD_PARAMETERS,  ONLY: JPVEXT, JPHEXT  ! number of External points
USE MODD_REF_n,       ONLY: XRHODREF ! dry density of ref. state

USE MODE_AERO_PSD,    ONLY: CON2MIX
USE MODE_ARGSLIST_ll, ONLY: ADD4DFIELD_ll, CLEANLIST_ll
USE MODE_EXCHANGE_ll, ONLY: UPDATE_HALO_ll
USE MODE_MSG
USE MODE_TOOLS_ll, ONLY: GET_INDICE_ll, LWEST_ll, LEAST_ll, LNORTH_ll, LSOUTH_ll

USE MODI_CH_AER_EQM_INIT_n
USE MODI_CH_FIELD_VALUE_n ! returns value of chemical species at each grid point
USE MODI_CH_INIT_CONST_n

!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)  :: KMI      ! model index
INTEGER, INTENT(IN)  :: KLUOUT   ! output listing channel
INTEGER, INTENT(IN)  :: KVERB    ! verbosity level
!
!*      0.2    declarations local variables
!
INTEGER :: JI, JJ, JK, JN        ! loop control variables
CHARACTER(LEN=3) :: YUNIT              ! units of returned initial values
                                       ! "CON" = molec./cm3
                                       ! "MIX" = mixing ratio
REAL    :: ZDEN2MOL
        !  ZDEN2MOL = 6.0221367E+23 * 1E-6 / 28.9644E-3
        !  conversion factor density to mol/cm3
        !  n_molec (moelc./cm3):  M = 1E-6*RHO(kg/m3) * XAVOGADRO / XMD

REAL, ALLOCATABLE, DIMENSION(:)   :: ZHEIGHT   !Height lookup table
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZSVINIT   !Species concentration lookup table  
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZSVINITA  !Aerosols species concentration lookup table  

INTEGER             :: ILEVMAX   !Maximum height level
INTEGER             :: JLEV      !Current height level

INTEGER             :: IIU  ! Upper dimension in x direction
INTEGER             :: IJU  ! Upper dimension in y direction
INTEGER             :: IKU  ! Upper dimension in z direction
INTEGER             :: IIB  ! indice I Beginning in x direction
INTEGER             :: IJB  ! indice J Beginning in y direction
INTEGER             :: IKB  ! indice K Beginning in z direction
INTEGER             :: IIE  ! indice I End       in x direction
INTEGER             :: IJE  ! indice J End       in y direction
INTEGER             :: IKE  ! indice K End       in z direction
!
TYPE(LIST_ll), POINTER :: TZFIELDS_ll ! pointer for the list of 3D fields
INTEGER :: IINFO_ll  ! Return code of //routines
INTEGER :: ILBX, ILBY, IRIMX, IRIMY
!
!-------------------------------------------------------------------------------
!
!*       0.    PROLOGUE
!              --------
!
NULLIFY(TZFIELDS_ll)
!
!*       1.    PREPARE INITIALIZATION
!              ----------------------

IF ( ( CORGANIC == "MPMPO" .OR. CORGANIC == "PUN" .OR. CORGANIC == "EQSAM2" ) .AND. &
     ( CCH_SCHEME == "NONE" .OR. CCH_SCHEME == "RELACS" .OR. CCH_SCHEME == "RACM" ) ) THEN
  CMNHMSG(1) = 'no SOA'
  CMNHMSG(2) = 'you want to use SOA gas particle balance'
  CMNHMSG(3) = 'but the scheme need to be CACM or RELACS 2'
  CMNHMSG(4) = 'CORGANIC has been set to NONE'
  CMNHMSG(5) = 'otherwise compile the correct scheme before'
  CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'CH_INIT_FIELD_n' )
  CORGANIC = "NONE"
END IF
!
!*       1.1   compute dimensions of arrays
!
IIU = SIZE(XSVT,1)
IJU = SIZE(XSVT,2)
IKU = SIZE(XSVT,3)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB = 1 + JPVEXT
IKE = IKU - JPVEXT
!
!        1.1.1 find maximum height level
ILEVMAX=INT(MAXVAL(XZZ(:,:,IKE)/10.))+1
ALLOCATE(ZHEIGHT(ILEVMAX))
ALLOCATE(ZSVINIT(ILEVMAX,NEQ))
ALLOCATE(ZSVINITA(ILEVMAX,NSV_AER))
!
!*       1.2   compute conversion factor kg/m3 --> molec/cm3
!
ZDEN2MOL = 1E-6 * XAVOGADRO / XMD
!
!
!-------------------------------------------------------------------------------
!
!*       2.    INITIALIZE T FIELDS AND CONVERT CONC. TO MIXING RATIO
!              -----------------------
!
YUNIT="MIX"

IF (LORILAM) THEN
  IF (.NOT.(ASSOCIATED(XN3D)))      ALLOCATE(XN3D     (IIU,IJU,IKU,JPMODE))
  IF (.NOT.(ASSOCIATED(XRG3D)))     ALLOCATE(XRG3D    (IIU,IJU,IKU,JPMODE))
  IF (.NOT.(ASSOCIATED(XSIG3D)))    ALLOCATE(XSIG3D   (IIU,IJU,IKU,JPMODE))
  IF (.NOT.(ASSOCIATED(XRHOP3D)))   ALLOCATE(XRHOP3D  (IIU,IJU,IKU,JPMODE))
  IF (.NOT.(ASSOCIATED(XM3D)))      ALLOCATE(XM3D     (IIU,IJU,IKU,JPMODE*3))
  IF (.NOT.(ASSOCIATED(XSEDA)))     ALLOCATE(XSEDA    (IIU,IJU,IKU,JPMODE*3))
  IF (.NOT.(ASSOCIATED(XCTOTA3D)))  ALLOCATE(XCTOTA3D (IIU,IJU,IKU,NSP+NCARB+NSOA,JPMODE))
  IF (.NOT.(ASSOCIATED(XVDEPAERO))) ALLOCATE(XVDEPAERO(IIU,IJU,JPIN))
  IF (.NOT.(ALLOCATED(XFAC)))       ALLOCATE(XFAC(NSP+NSOA+NCARB))
  IF (.NOT.(ALLOCATED(XRHOI)))      ALLOCATE(XRHOI(NSP+NSOA+NCARB))
  IF (.NOT.(ASSOCIATED(XFRAC))) THEN
    ALLOCATE(XFRAC(IIU,IJU,IKU,NEQ))
    XFRAC(:,:,:,:) = 0.
  END IF
  IF (.NOT.(ASSOCIATED(XMI))) THEN
    ALLOCATE(XMI(IIU,IJU,IKU,NSP+NCARB+NSOA))
  END IF
  IF (.NOT.(ASSOCIATED(XJNUC)))        ALLOCATE(XJNUC       (IIU,IJU,IKU))
  IF (.NOT.(ASSOCIATED(XJ2RAT)))       ALLOCATE(XJ2RAT      (IIU,IJU,IKU))
  IF (.NOT.(ASSOCIATED(XCONC_MASS)))   ALLOCATE(XCONC_MASS  (IIU,IJU,IKU))
  IF (.NOT.(ASSOCIATED(XCOND_MASS_I))) ALLOCATE(XCOND_MASS_I(IIU,IJU,IKU))
  IF (.NOT.(ASSOCIATED(XCOND_MASS_J))) ALLOCATE(XCOND_MASS_J(IIU,IJU,IKU))
  IF (.NOT.(ASSOCIATED(XNUCL_MASS)))   ALLOCATE(XNUCL_MASS  (IIU,IJU,IKU))

  IF (.NOT.(ASSOCIATED(XMBEG)))   ALLOCATE(XMBEG(IIU,IJU,IKU,JPIN))
  IF (.NOT.(ASSOCIATED(XMINT)))   ALLOCATE(XMINT(IIU,IJU,IKU,JPIN))
  IF (.NOT.(ASSOCIATED(XMEND)))   ALLOCATE(XMEND(IIU,IJU,IKU,JPIN))

  IF (.NOT.(ASSOCIATED(XDMINTRA)))  ALLOCATE(XDMINTRA(IIU,IJU,IKU,JPIN))
  IF (.NOT.(ASSOCIATED(XDMINTER)))  ALLOCATE(XDMINTER(IIU,IJU,IKU,JPIN))
  IF (.NOT.(ASSOCIATED(XDMCOND)))   ALLOCATE(XDMCOND (IIU,IJU,IKU,JPIN))
  IF (.NOT.(ASSOCIATED(XDMNUCL)))   ALLOCATE(XDMNUCL (IIU,IJU,IKU,JPIN))
  IF (.NOT.(ASSOCIATED(XDMMERG)))   ALLOCATE(XDMMERG (IIU,IJU,IKU,JPIN))
  !
  XJNUC(:,:,:)        = 1.0E-7
  XJ2RAT(:,:,:)       = 0.
  XCONC_MASS(:,:,:)   = 0.
  XCOND_MASS_I(:,:,:) = 0.
  XCOND_MASS_J(:,:,:) = 0.
  XNUCL_MASS(:,:,:)   = 0.
  !
  XMBEG(:,:,:,:)      = 0.
  XMINT(:,:,:,:)      = 0.
  XMEND(:,:,:,:)      = 0.
  !
  XDMINTRA(:,:,:,:)     = 0.
  XDMINTER(:,:,:,:)     = 0.
  XDMCOND(:,:,:,:)      = 0.
  XDMNUCL(:,:,:,:)      = 0.
  XDMMERG(:,:,:,:)      = 0.
END IF
!
IF ( LCH_INIT_FIELD .AND. CPROGRAM /= 'DIAG  ' ) THEN
  DO JLEV=1,ILEVMAX
    ZHEIGHT=REAL(JLEV-1)*10.
    DO JN = 1, NEQ
      ZSVINIT(JLEV,JN) = &
           CH_FIELD_VALUE_n(ZHEIGHT(JLEV), "LLZ", &
           CNAMES(JN), YUNIT, KLUOUT, KVERB)
      ! "LLZ" identifies the type of x-y-z values passed on to
      ! CH_FIELD_VALUE_n ("LLZ"=lon-lat-Z)
      ! in future developpements, "IJK" may be used in order
      ! to pass the grid indices rather than coordinates
    END DO
  END DO

  DO JK = IKB, IKE
    DO JJ = IJB, IJE
      DO JI = IIB, IIE
        JLEV=INT(MAX(XZZ(JI,JJ,JK),0.)/10.)+1
        XSVT(JI,JJ,JK,NSV_CHEMBEG:NSV_CHEMEND) = ZSVINIT(JLEV,:)
      END DO
    END DO
  END DO

  DO JN = NSV_CHEMBEG, NSV_CHEMEND
    DO JK = 1, JPVEXT
      XSVT(:,:,IKB-JK,JN) = XSVT(:,:,IKB,JN)
      XSVT(:,:,IKE+JK,JN) = XSVT(:,:,IKE,JN)
    END DO

    IF ( LWEST_ll() )  THEN
      DO JI = 1, JPHEXT
        XSVT(JI,:,:,JN) = XSVT(IIB,:,:,JN)
      END DO
    END IF

    IF ( LEAST_ll() )  THEN
      DO JI = 1, JPHEXT
        XSVT(IIE+JI,:,:,JN) = XSVT(IIE,:,:,JN)
      END DO
    END IF

    IF ( LSOUTH_ll() ) THEN
      DO JJ = 1, JPHEXT
        XSVT(:,JJ,:,JN) = XSVT(:,IJB,:,JN)
      END DO
    END IF

    IF ( LNORTH_ll() ) THEN
      DO JJ = 1, JPHEXT
        XSVT(:,IJE+JJ,:,JN) = XSVT(:,IJE,:,JN)
      END DO
    END IF
  END DO
  !
  IF ( YUNIT == "CON" ) THEN
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'CH_INIT_FIELD_n', 'converting initial values to mixing ratio' )
    DO JN = NSV_CHEMBEG,NSV_CHEMEND
      XSVT(:,:,:,JN) = XSVT(:,:,:,JN)/(XRHODREF(:,:,:)*ZDEN2MOL)
    ENDDO
  ELSE
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'CH_INIT_FIELD_n', 'initial values are used as is (mixing ratio)' )
  ENDIF
!
!
  ORILAM: IF (LORILAM) THEN
    DO JLEV=1,ILEVMAX
      ZHEIGHT=REAL(JLEV-1)*10.
      DO JN = 1, NSV_AER
        ZSVINITA(JLEV,JN) = &
             CH_FIELD_VALUE_n(ZHEIGHT(JLEV), "LLZ", &
             CAERONAMES(JN), YUNIT, KLUOUT, KVERB)
        ! "LLZ" identifies the type of x-y-z values passed on to
        ! CH_FIELD_VALUE_n ("LLZ"=lon-lat-Z)
        ! in future developpements, "IJK" may be used in order
        ! to pass the grid indices rather than coordinates
      END DO
    END DO
    !
    DO JK = IKB, IKE
      DO JJ = IJB, IJE
        DO JI = IIB, IIE
          JLEV=INT(MAX(XZZ(JI,JJ,JK),0.)/10.)+1
          XSVT(JI,JJ,JK,NSV_AERBEG:NSV_AEREND) = ZSVINITA(JLEV,:)
        END DO
      END DO
    END DO

    DO JN = NSV_AERBEG,NSV_AEREND
      DO JK = 1, JPVEXT
        XSVT(:,:,IKB-JK,JN) = XSVT(:,:,IKB,JN)
        XSVT(:,:,IKE+JK,JN) = XSVT(:,:,IKE,JN)
      END DO

      IF ( LWEST_ll() )  THEN
        DO JI = 1, JPHEXT
          XSVT(JI,:,:,JN) = XSVT(IIB,:,:,JN)
        END DO
      END IF

      IF ( LEAST_ll() )  THEN
        DO JI = 1, JPHEXT
          XSVT(IIE+JI,:,:,JN) = XSVT(IIE,:,:,JN)
        END DO
      END IF

      IF ( LSOUTH_ll() ) THEN
        DO JJ = 1, JPHEXT
          XSVT(:,JJ,:,JN) = XSVT(:,IJB,:,JN)
        END DO
      END IF

      IF ( LNORTH_ll() ) THEN
        DO JJ = 1, JPHEXT
          XSVT(:,IJE+JJ,:,JN) = XSVT(:,IJE,:,JN)
        END DO
      END IF
    END DO
    !
    IF ( YUNIT == "CON" ) THEN
      CALL PRINT_MSG( NVERB_INFO, 'GEN', 'CH_INIT_FIELD_n', 'ORILAM: converting initial values ug/m3 to mixing ratio' )
      CALL CON2MIX (XSVT(:,:,:,NSV_AERBEG:NSV_AEREND), XRHODREF)
    ELSE
      CALL PRINT_MSG( NVERB_INFO, 'GEN', 'CH_INIT_FIELD_n', 'ORILAM: initial values are used as is (mixing ratio)' )
    ENDIF
    !
  END IF ORILAM
  !
END IF
!
!
CALL ADD4DFIELD_ll(TZFIELDS_ll, XSVT(:,:,:,NSV_CHEMBEG:NSV_CHEMEND), 'CH_INIT_FIELD_n::XSVT(:,:,:,NSV_CHEMBEG:NSV_CHEMEND)' )
CALL ADD4DFIELD_ll(TZFIELDS_ll, XSVT(:,:,:,NSV_AERBEG:NSV_AEREND),   'CH_INIT_FIELD_n::XSVT(:,:,:,NSV_AERBEG:NSV_AEREND)'   )
!
CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
CALL CLEANLIST_ll(TZFIELDS_ll)
!
!-------------------------------------------------------------------------------
!
!*        3.    INITIALIZE CHEMICAL CONSTANTS
!
CALL CH_INIT_CONST_n(KLUOUT, KVERB)       
!
!-------------------------------------------------------------------------------
!
!*        4.   INITIALIZE AEROSOLS
!              -------------------
!
IF (LORILAM) THEN
  CALL CH_AER_EQM_INIT_n(XSVT(:,:,:,NSV_CHEMBEG:NSV_CHEMEND),&
                         XSVT(:,:,:,NSV_AERBEG:NSV_AEREND),&
                         XM3D,XRHOP3D,XSIG3D,& 
                         XRG3D,XN3D, XRHODREF, XCTOTA3D) 
  DO JN = 1,JPIN
    DO JK = 1, JPVEXT
      XM3D(:,:,IKB-JK,JN) =  XM3D(:,:,IKB,JN)
      XM3D(:,:,IKE+JK,JN) =  XM3D(:,:,IKE,JN)
    END DO
  END DO
  !
  CALL ADD4DFIELD_ll(TZFIELDS_ll, XSVT(:,:,:,NSV_CHEMBEG:NSV_CHEMEND), 'CH_INIT_FIELD_n::XSVT(:,:,:,NSV_CHEMBEG,NSV_CHEMEND)' )
  CALL ADD4DFIELD_ll(TZFIELDS_ll, XSVT(:,:,:,NSV_AERBEG:NSV_AEREND),   'CH_INIT_FIELD_n::XSVT(:,:,:,NSV_AERBEG:NSV_AEREND)'   )
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
END IF
!
!
!-------------------------------------------------------------------------------
!
!*        5.    INITIALIZE LB IN CASE OF LCH_INIT_FIELD
!               ---------------------------------------
!
IF ((LCH_INIT_FIELD).AND.(CPROGRAM/='DIAG  ').AND.(KMI .EQ. 1)) THEN
  ILBX=SIZE(XLBXSVM,1)
  ILBY=SIZE(XLBYSVM,2)
  IRIMX = INT(ILBX/2)
  IRIMY = INT(ILBY/2)
  DO JN = NSV_CHEMBEG,NSV_CHEMEND
    IF(LWEST_ll() .AND. .NOT. L1D) &
      XLBXSVM(1:IRIMX+1,        :,:,JN) = XSVT(1:IRIMX+1,        :,:,JN)
    IF(LEAST_ll() .AND. .NOT. L1D) &
      XLBXSVM(ILBX-IRIMX:ILBX,:,:,JN)   = XSVT(IIU-IRIMX:IIU,    :,:,JN)
    IF(LSOUTH_ll() .AND. .NOT. L1D .AND. .NOT. L2D) &
      XLBYSVM(:,1:IRIMY+1,        :,JN) = XSVT(:,1:IRIMY+1,      :,JN)
    IF(LNORTH_ll() .AND. .NOT. L1D .AND. .NOT. L2D) &
      XLBYSVM(:,ILBY-IRIMY:ILBY,:,JN)   = XSVT(:,IJU-IRIMY:IJU,  :,JN)
  END DO
  IF (LORILAM) THEN
  DO JN = NSV_AERBEG,NSV_AEREND
    IF(LWEST_ll() .AND. .NOT. L1D) &
      XLBXSVM(1:IRIMX+1,        :,:,JN) = XSVT(1:IRIMX+1,        :,:,JN)
    IF(LEAST_ll() .AND. .NOT. L1D) &
      XLBXSVM(ILBX-IRIMX:ILBX,:,:,JN)   = XSVT(IIU-IRIMX:IIU,    :,:,JN)
    IF(LSOUTH_ll() .AND. .NOT. L1D .AND. .NOT. L2D) &
      XLBYSVM(:,1:IRIMY+1,        :,JN) = XSVT(:,1:IRIMY+1,      :,JN)
    IF(LNORTH_ll() .AND. .NOT. L1D .AND. .NOT. L2D) &
      XLBYSVM(:,ILBY-IRIMY:ILBY,:,JN)   = XSVT(:,IJU-IRIMY:IJU,  :,JN)
  END DO
  ENDIF
!
ENDIF
!
!
END SUBROUTINE CH_INIT_FIELD_n
