!-----------------------------------------------------------------
!     #####################
      MODULE MODI_ADVECUVW_RK
!     #####################
!
INTERFACE
      SUBROUTINE ADVECUVW_RK (HUVW_ADV_SCHEME,                         &
                    HTEMP_SCHEME, KWENO_ORDER,                         &
                    HLBCX, HLBCY, PTSTEP,                              &
                    PU, PV, PW,                                        &
                    PUT, PVT, PWT,                                     &
                    PMXM_RHODJ, PMYM_RHODJ, PMZM_RHODJ,                &
                    PRUCT, PRVCT, PRWCT,                               &
                    PRUS_ADV, PRVS_ADV, PRWS_ADV,                      &
                    PRUS_OTHER, PRVS_OTHER, PRWS_OTHER                 )
!
CHARACTER(LEN=6),         INTENT(IN)    :: HUVW_ADV_SCHEME! to the selected
CHARACTER(LEN=4),         INTENT(IN)    :: HTEMP_SCHEME   ! Temporal scheme
!
INTEGER,                  INTENT(IN)    :: KWENO_ORDER   ! Order of the WENO
                                                         ! scheme (3 or 5)
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
REAL,                     INTENT(IN)    :: PTSTEP
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PU , PV  , PW
                                                  ! Variables to advect
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT , PWT
                                                  ! Variables for boundary
                                                  ! conditions
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PMXM_RHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PMYM_RHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PMZM_RHODJ
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRUCT , PRVCT, PRWCT
                                                  ! Contravariant wind components
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PRUS_ADV , PRVS_ADV, PRWS_ADV
                                                  ! Tendency due to advection
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRUS_OTHER , PRVS_OTHER, PRWS_OTHER
!                                                 ! tendencies from other processes
!
END SUBROUTINE ADVECUVW_RK
!
END INTERFACE
!
END MODULE MODI_ADVECUVW_RK
!     ##########################################################################
      SUBROUTINE ADVECUVW_RK (HUVW_ADV_SCHEME,                         &
                    HTEMP_SCHEME, KWENO_ORDER,                         &
                    HLBCX, HLBCY, PTSTEP,                              &
                    PU, PV, PW,                                        &
                    PUT, PVT, PWT,                                     &
                    PMXM_RHODJ, PMYM_RHODJ, PMZM_RHODJ,                &
                    PRUCT, PRVCT, PRWCT,                               &
                    PRUS_ADV, PRVS_ADV, PRWS_ADV,                      &
                    PRUS_OTHER, PRVS_OTHER, PRWS_OTHER                 )
!     ##########################################################################
!
!!****  *ADVECUVW_RK * - routine to call the specialized advection routines for wind
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE
!!
!!    REFERENCE
!!    ---------
!!      Book1 and book2 ( routine ADVECTION )
!!
!!    AUTHOR
!!    ------
!!	J.-P. Pinty      * Laboratoire d'Aerologie*
!!	J.-P. Lafore     * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/07/94 
!!                  01/04/95 (Ph. Hereil J. Nicolau) add the model number
!!                  23/10/95 (J. Vila and JP Lafore) advection schemes scalar
!!                  16/01/97 (JP Pinty)              change presentation 
!!                  30/04/98 (J. Stein P Jabouille)  extrapolation for the cyclic
!!                                                   case and parallelisation
!!                  24/06/99 (P Jabouille)           case of NHALO>1
!!                  25/10/05 (JP Pinty)              4th order scheme
!!                  24/04/06 (C.Lac)                 Split scalar and passive
!!                                                   tracer routines
!!                  08/06    (T.Maric)               PPM scheme
!!                  04/2011  (V. Masson & C. Lac)    splits the routine and adds
!!                                                   time splitting
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
USE MODD_ARGSLIST_ll, ONLY : LIST_ll, HALO2LIST_ll
USE MODD_PARAMETERS,  ONLY : JPVEXT
USE MODD_CONF,        ONLY : NHALO
!
USE MODI_SHUMAN
USE MODI_ADVECUVW_WENO_K
USE MODI_ADV_BOUNDARIES
USE MODI_GET_HALO
!
!-------------------------------------------------------------------------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER(LEN=6),         INTENT(IN)    :: HUVW_ADV_SCHEME! to the selected
CHARACTER(LEN=4),         INTENT(IN)    :: HTEMP_SCHEME   ! Temporal scheme
!
INTEGER,                  INTENT(IN)    :: KWENO_ORDER   ! Order of the WENO
                                                         ! scheme (3 or 5)
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
REAL,                     INTENT(IN)    :: PTSTEP
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PU , PV  , PW
                                                  ! Variables to advect
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT , PWT
                                                  ! Variables for boundary
                                                  ! conditions
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PMXM_RHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PMYM_RHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PMZM_RHODJ
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRUCT , PRVCT, PRWCT
                                                  ! Contravariant wind components
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PRUS_ADV , PRVS_ADV, PRWS_ADV
                                                  ! Tendency due to advection
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRUS_OTHER , PRVS_OTHER, PRWS_OTHER
!                                                 ! tendencies from other processes
!
!
!
!*       0.2   declarations of local variables
!
!
!  
INTEGER             :: IKE       ! indice K End       in z direction
!
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZU, ZV, ZW
! Guesses at the beginning of the RK loop
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZUT, ZVT, ZWT
! Intermediate Guesses inside the RK loop              
!
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZRUS,ZRVS,ZRWS
! Momentum tendencies due to advection
REAL, DIMENSION(:,:), ALLOCATABLE :: ZBUT ! Butcher array coefficients
                                          ! at the RK sub time step
REAL, DIMENSION(:),   ALLOCATABLE :: ZBUTS! Butcher array coefficients
                                          ! at the end of the RK loop

!JUAN
TYPE(LIST_ll), POINTER      :: TZFIELDMT_ll ! list of fields to exchange
TYPE(HALO2LIST_ll), POINTER :: TZHALO2MT_ll ! momentum variables
INTEGER                     :: INBVAR
INTEGER :: IIU, IJU, IKU ! array sizes
!JUAN

! Momentum tendencies due to advection
INTEGER :: ISPL                ! Number of RK splitting loops
INTEGER :: JI, JS              ! Loop index
!
INTEGER                     :: IINFO_ll    ! return code of parallel routine
TYPE(LIST_ll), POINTER      :: TZFIELD_ll  ! list of fields to exchange
TYPE(LIST_ll), POINTER      :: TZFIELDS_ll ! list of fields to exchange
TYPE(LIST_ll), POINTER      :: TZFIELDS0_ll ! list of fields to exchange
TYPE(LIST_ll), POINTER      :: TZFIELDS4_ll ! list of fields to exchange
!
!
REAL          :: XPRECISION
!-------------------------------------------------------------------------------
!
!*       0.     INITIALIZATION                        
!	        --------------
!
IKE = SIZE(PWT,3) - JPVEXT
IIU=SIZE(PUT,1)
IJU=SIZE(PUT,2)
IKU=SIZE(PUT,3)
!
SELECT CASE (HTEMP_SCHEME)
 CASE('RK11')
  ISPL = 1
 CASE('RK21')
  ISPL = 2
 CASE('RK33')
  ISPL = 3
 CASE('RK53')
  ISPL = 5
END SELECT
!
!
ALLOCATE(ZBUT(ISPL-1,ISPL-1))
ALLOCATE(ZBUTS(ISPL))
!
IF (ISPL == 1 ) ZBUTS = (/ 1. /)
IF (ISPL == 2 ) THEN
   ZBUTS     = (/ 0. , 1. /)
   ZBUT(1,1)   = 3./4.
END IF
IF (ISPL == 3 ) THEN
   ZBUTS     = (/ 1./6. , 1./6. , 2./3. /)
   ZBUT(1,1) = 1.
   ZBUT(1,2) = 0.
   ZBUT(2,:) = 1./4.
END IF
IF (ISPL == 5 ) THEN
   ZBUTS     = (/ 1./4. , 0., 0., 0., 3./4. /)
   ZBUT      = 0.
   ZBUT(1,1) = 1./7.
   ZBUT(2,2) = 3./16.
   ZBUT(3,3) = 1./3.
   ZBUT(4,4) = 2./3.
END IF
!
ALLOCATE(ZRUS(SIZE(PUT,1),SIZE(PUT,2),SIZE(PWT,3),ISPL))
ALLOCATE(ZRVS(SIZE(PUT,1),SIZE(PUT,2),SIZE(PWT,3),ISPL))
ALLOCATE(ZRWS(SIZE(PUT,1),SIZE(PUT,2),SIZE(PWT,3),ISPL))
!
PRUS_ADV = 0.
PRVS_ADV = 0.
PRWS_ADV = 0.
!
!-------------------------------------------------------------------------------
!
!*       2.     Wind guess before RK loop
!	        -------------------------
!
ZUT = PU
ZVT = PV
ZWT = PW
!
CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZUT, PUT, 'U' )    
CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZVT, PVT, 'V' )    
CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZWT, PWT, 'W' )
ZWT (:,:,IKE+1 ) = 0.

ZU = PU
ZV = PV
ZW = PW
!
NULLIFY(TZFIELDMT_ll)
IF( NHALO==1 ) THEN      
!
   CALL ADD3DFIELD_ll(TZFIELDMT_ll, ZUT)
   CALL ADD3DFIELD_ll(TZFIELDMT_ll, ZVT)
   CALL ADD3DFIELD_ll(TZFIELDMT_ll, ZWT)
!
   INBVAR = 3
   IF( NHALO==1 ) CALL INIT_HALO2_ll(TZHALO2MT_ll,INBVAR,SIZE(PUT,1),SIZE(PUT,2),SIZE(PWT,3))
!
 END IF
!
ZRUS = 0.
ZRVS = 0.
ZRWS = 0.
!-------------------------------------------------------------------------------
!
!*       3.     BEGINNING of Runge-Kutta loop
!	        -----------------------------
!
 DO JS = 1, ISPL
!
!
      CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZUT, PUT, 'U' )    
      CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZVT, PVT, 'V' )    
      CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZWT, PWT, 'W' )
      ZW (:,:,IKE+1 ) = 0.
     !JUAN
     IF ( NHALO == 1 ) THEN   
        CALL UPDATE_HALO_ll(TZFIELDMT_ll,IINFO_ll)        
        CALL UPDATE_HALO2_ll(TZFIELDMT_ll, TZHALO2MT_ll, IINFO_ll)
     ENDIF
     !JUAN
!
!*       4.     Advection with WENO
!	        -------------------
!
     CALL ADVECUVW_WENO_K (HLBCX, HLBCY, KWENO_ORDER, ZUT, ZVT, ZWT,      &
                          PRUCT, PRVCT, PRWCT,                            &
                          ZRUS(:,:,:,JS), ZRVS(:,:,:,JS), ZRWS(:,:,:,JS), &
                          TZHALO2MT_ll                                    )
!
!
! ==> verifier si c'est utile !
!
     NULLIFY(TZFIELDS4_ll)
     IF(NHALO == 1) THEN
          CALL ADD3DFIELD_ll(TZFIELDS4_ll, ZRUS(:,:,:,JS))
          CALL ADD3DFIELD_ll(TZFIELDS4_ll, ZRVS(:,:,:,JS))
          CALL ADD3DFIELD_ll(TZFIELDS4_ll, ZRWS(:,:,:,JS))
          CALL UPDATE_HALO_ll(TZFIELDS4_ll,IINFO_ll)
          CALL CLEANLIST_ll(TZFIELDS4_ll)
     END IF

    IF ( JS /= ISPL ) THEN
!

     DO JI = 1, JS

!
! Intermediate guesses inside the RK loop
!
        ZUT(:,:,:) = ZU(:,:,:) + ZBUT(JS,JI) *  PTSTEP *  &
         ( ZRUS(:,:,:,JI) + PRUS_OTHER(:,:,:) ) / PMXM_RHODJ
        ZVT(:,:,:) = ZV(:,:,:) + ZBUT(JS,JI) *  PTSTEP *  &
         ( ZRVS(:,:,:,JI) + PRVS_OTHER(:,:,:) ) / PMYM_RHODJ
        ZWT(:,:,:) = ZW(:,:,:) + ZBUT(JS,JI) *  PTSTEP *  &
         ( ZRWS(:,:,:,JI) + PRWS_OTHER(:,:,:) ) / PMZM_RHODJ
!
      END DO
!
    ELSE  
!
! Guesses at the end of the RK loop
!
      DO JI = 1, ISPL
       PRUS_ADV(:,:,:) = PRUS_ADV(:,:,:) + ZBUTS(JI) * ZRUS(:,:,:,JI) 
       PRVS_ADV(:,:,:) = PRVS_ADV(:,:,:) + ZBUTS(JI) * ZRVS(:,:,:,JI) 
       PRWS_ADV(:,:,:) = PRWS_ADV(:,:,:) + ZBUTS(JI) * ZRWS(:,:,:,JI) 
      END DO
!
    END IF
!
! End of the RK loop
 END DO

!
!
DEALLOCATE(ZBUT, ZBUTS, ZRUS, ZRVS, ZRWS)
CALL CLEANLIST_ll(TZFIELDMT_ll)
CALL  DEL_HALO2_ll(TZHALO2MT_ll)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ADVECUVW_RK
