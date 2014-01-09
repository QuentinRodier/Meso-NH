!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 newsrc 2006/05/23 15:39:51
!-----------------------------------------------------------------
!###################
MODULE MODI_SPAWN_ZS
!###################
!
INTERFACE
!
     SUBROUTINE SPAWN_ZS (KXOR,KXEND,KYOR,KYEND,KDXRATIO,KDYRATIO,HLBCX,HLBCY,&
                          HLUOUT,PZS1,PZS2,HFIELD,PZS2_LS                     )
!
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the model 2 domain, relative to model 1
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction Resolution ratio
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between model 2 and model 1
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
CHARACTER(LEN=*),     INTENT(IN)  :: HLUOUT  ! output-listing file
REAL, DIMENSION(:,:), INTENT(IN)  :: PZS1    ! model 1 orography
REAL, DIMENSION(:,:), INTENT(OUT) :: PZS2    ! interpolated orography with iterative correction
CHARACTER(LEN=6),     INTENT(IN)  :: HFIELD ! name of the field to nest
REAL, DIMENSION(:,:), INTENT(OUT),OPTIONAL  :: PZS2_LS ! interpolated orography
!
END SUBROUTINE SPAWN_ZS
!
END INTERFACE
!
END MODULE MODI_SPAWN_ZS
!
!
!     #########################################################################
     SUBROUTINE SPAWN_ZS (KXOR,KXEND,KYOR,KYEND,KDXRATIO,KDYRATIO,HLBCX,HLBCY,&
                          HLUOUT,PZS1,PZS2,HFIELD,PZS2_LS                     )
!     #########################################################################
!
!!****  *SPAWN_ZS * - subroutine to spawn zs field
!!
!!    PURPOSE
!!    -------
!!
!!      This routine defines the information necessary to generate the model 2
!!    grid, consistently with the spawning model 1.
!!      The longitude and latitude of the model 2 origine are computed from
!!    the model 1. Then the grid in the conformal projection and terrain
!!    following coordinates (XHAT,YHAT and ZHAT) and orography, are interpolated
!!    from the model 1 grid and orography knowledge.
!!
!!**  METHOD
!!    ------
!!
!!      The model 2 variables are transmitted by argument (P or K prefixes),
!!    while the ones of model 1 are declared through calls to MODD_...
!!    (X or N prefixes)
!!
!!      For the case where the resolution ratio between models is 1,
!!    the horizontal interpolation becomes a simple equality.
!!      For the general case where resolution ratio is not egal to one,
!!    grid and orography are interpolated as follows:
!!         - linear interpolation for XHAT and YHAT
!!         - identity for ZHAT (no vertical spawning)
!!            2 types of interpolations can be used:
!!                 1. Clark and Farley (JAS 1984) on 9 points
!!                 2. Bikhardt on 16 points
!!
!!    EXTERNAL
!!    --------
!!
!!      FMLOOK        : to recover a logical unit number
!!      Routine BIKHARDT2     : to perform horizontal interpolations
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_PARAMETERS : contains parameters
!!      Module MODD_CONF       : contains models configuration
!!      Module MODD_LUNIT2     : contains unit numbers of model 2 files
!!
!!    REFERENCE
!!    ---------
!!
!!       Book1 of the documentation
!!       PROGRAM SPAWN_ZS (Book2 of the documentation)
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson    * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     12/01/05
!!      Modification    20/05/06 Remove Clark and Farley interpolation
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_PARAMETERS, ONLY : JPHEXT       ! Declarative modules
USE MODD_CONF,       ONLY : NVERB
!
USE MODD_BIKHARDT_n
!
USE MODI_BIKHARDT
USE MODI_ZS_BOUNDARY
!
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the model 2 domain, relative to model 1
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction Resolution ratio
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between model 2 and model 1
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
CHARACTER(LEN=*),     INTENT(IN)  :: HLUOUT  ! output-listing file
REAL, DIMENSION(:,:), INTENT(IN)  :: PZS1    ! model 1 orography
REAL, DIMENSION(:,:), INTENT(OUT) :: PZS2    ! interpolated orography with iterative correction
CHARACTER(LEN=6),     INTENT(IN)  :: HFIELD ! name of the field to nest
REAL, DIMENSION(:,:), INTENT(OUT),OPTIONAL  :: PZS2_LS ! interpolated orography
!
!*       0.2    Declarations of local variables for print on FM file
!
INTEGER :: ILUOUT   ! Logical unit number for the output listing
INTEGER :: IRESP    ! Return codes in FM routines
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZS2_LS ! interpolated orography
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZS1 ! zs of model 1 at iteration n or n+1 
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZS2 ! averaged zs of model 2 at iteration n
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDZS ! difference between PZS1 and ZZS2
!
INTEGER             :: IXMIN, IXMAX ! indices to interpolate the
                                    ! modified orography on model 2
                                    ! domain to model 1 grid
INTEGER             :: JI,JEPSX  ! Loop index in x direction
INTEGER             :: JJ,JEPSY  ! Loop index in y direction
INTEGER             :: JCOUNTER  ! counter for iterative method
REAL                :: ZRELAX             ! relaxation factor
INTEGER             :: JMAXITER = 2000    ! maximum number of iterations
!
INTEGER, DIMENSION(2) :: IZSMAX
INTEGER               :: IMI     ! current model index
!-------------------------------------------------------------------------------
!
!*       1.    PROLOGUE:
!              ---------
!
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(2)
!
!
!*       1.2  recovers logical unit number of output listing
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*       2.    COMPUTATION:
!              ---------
!
!*       2.1   Purely interpolated zs:
!              -----------------------
!
ALLOCATE(ZZS2_LS(SIZE(PZS2,1),SIZE(PZS2,2)))
!
  CALL BIKHARDT (XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                 XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                 KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                 HLBCX,HLBCY,PZS1,ZZS2_LS)
!
!*       4.2   New zs:
!              -------
!
!* we use an iterative method to insure the equality between large-scale
!  orography and the average of fine orography, and to be sure not to have
!  spurious cliffs near the coast (multiplication by xland during the
!  iterative process).
!
IF (KDXRATIO/=1 .OR. KDYRATIO/=1) THEN
!
!* allocations
!
  ALLOCATE(ZZS2(KXEND-KXOR-1,KYEND-KYOR-1))
  ALLOCATE(ZDZS(KXEND-KXOR-1,KYEND-KYOR-1))
  ALLOCATE(ZZS1(SIZE(PZS1,1),SIZE(PZS1,2)))
!
!* constants
!
  ZRELAX=16./13.     ! best relaxation for infinite aspect ratio.
                     ! for dx=2, one should take 32./27. !!!
!
!* initializations of initial state
!
  JCOUNTER=0
  ZZS1(:,:)=PZS1(:,:)
!
!* iterative loop
!
  DO
!
!* interpolation
!
      CALL BIKHARDT (XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                     XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                     KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                     HLBCX,HLBCY,ZZS1,PZS2)
    JCOUNTER=JCOUNTER+1
!
!* if orography is positive, it stays positive
!
    DO JI=1,KXEND-KXOR-1
      DO JJ=1,KYEND-KYOR-1
        IF (PZS1(JI+KXOR,JJ+KYOR)>-1.E-15)                            &
          PZS2((JI-1)*KDXRATIO+1+JPHEXT:JI*KDXRATIO+JPHEXT,             &
               (JJ-1)*KDYRATIO+1+JPHEXT:JJ*KDYRATIO+JPHEXT)  =          &
            MAX( PZS2((JI-1)*KDXRATIO+1+JPHEXT:JI*KDXRATIO+JPHEXT,      &
                      (JJ-1)*KDYRATIO+1+JPHEXT:JJ*KDYRATIO+JPHEXT), 0.)
      END DO
    END DO
!
!* computation of new averaged orography
!
    ZZS2(:,:) = 0.
    DO JI=1,KXEND-KXOR-1
      DO JJ=1,KYEND-KYOR-1
        DO JEPSX = (JI-1)*KDXRATIO+1+JPHEXT, JI*KDXRATIO+JPHEXT
          DO JEPSY = (JJ-1)*KDYRATIO+1+JPHEXT, JJ*KDYRATIO+JPHEXT
            ZZS2(JI,JJ) = ZZS2(JI,JJ) + PZS2(JEPSX,JEPSY)
          END DO
        END DO
      END DO
    END DO
    ZZS2(:,:) = ZZS2(:,:) / (KDXRATIO*KDYRATIO)
    !
    ZDZS(:,:)=PZS1(KXOR+1:KXEND-1,KYOR+1:KYEND-1)-ZZS2(:,:)
!
!* test to end the iterative process
!
    IF (MAXVAL(ABS(ZDZS(:,:)))<1.E-3) EXIT
!
    IF (JCOUNTER>=JMAXITER) THEN
      WRITE(ILUOUT,FMT=*) 'SPAWN_ZS: convergence of ',TRIM(HFIELD), &
                          ' NOT obtained after',JCOUNTER,' iterations'
      WRITE(ILUOUT,FMT=*) TRIM(HFIELD),                             &
         ' is modified to insure egality of large scale and averaged fine field'
      DO JI=1,KXEND-KXOR-1
        DO JJ=1,KYEND-KYOR-1
          DO JEPSX = (JI-1)*KDXRATIO+1+JPHEXT, JI*KDXRATIO+JPHEXT
            DO JEPSY = (JJ-1)*KDYRATIO+1+JPHEXT, JJ*KDYRATIO+JPHEXT
              PZS2(JEPSX,JEPSY) = PZS2(JEPSX,JEPSY) + ZDZS(JI,JJ)
            END DO
          END DO
        END DO
      END DO
      !
      EXIT
    END IF
!
!* prints
!
    IF (NVERB >=7) THEN
      IZSMAX=MAXLOC(ABS(ZDZS(:,:)))
      IF (MOD(JCOUNTER,500)==1) &
        WRITE(ILUOUT,FMT='(A4,1X,A4,1X,A2,1X,A2,1X,A12,1X,A12,1X,A12)')      &
                          'n IT','nDIV','I1','J1','   ZS1','   ZS2','   DZS'
        WRITE(ILUOUT,FMT='(I4,1X,I4,1X,I2,1X,I2,1X,F12.7,1X,F12.7,1X,F12.7)')&
                           JCOUNTER,COUNT(ABS(ZDZS(:,:))>=1.E-3),            &
                           IZSMAX(1)+KXOR,IZSMAX(2)+KYOR,                    &
                           PZS1(KXOR+IZSMAX(1),KYOR+IZSMAX(2)),              &
                           ZZS2(IZSMAX(1),IZSMAX(2)),                        &
                           ZDZS(IZSMAX(1),IZSMAX(2))
    END IF
!
!* correction of coarse orography
!
    ZZS1(KXOR+1:KXEND-1,KYOR+1:KYEND-1) =                             &
         ZZS1(KXOR+1:KXEND-1,KYOR+1:KYEND-1) + ZRELAX * ZDZS(:,:)
    !
    ! extrapolations (X direction)
    !
    IF(KXOR==1 .AND. KXEND==SIZE(PZS1,1) .AND. HLBCX(1)=='CYCL' ) THEN
      ZZS1(KXOR,KYOR+1:KYEND-1) = ZZS1(KXEND-1,KYOR+1:KYEND-1)
      ZZS1(KXEND,KYOR+1:KYEND-1) = ZZS1(KXOR+1,KYOR+1:KYEND-1)
    ELSE
      ZZS1(KXOR,KYOR+1:KYEND-1) =                                       &
        2. * ZZS1(KXOR+1,KYOR+1:KYEND-1)  - ZZS1(KXOR+2,KYOR+1:KYEND-1)
      IF(KXOR>1)                                                        &
      ZZS1(KXOR-1,KYOR+1:KYEND-1) =                                     &
        2. * ZZS1(KXOR  ,KYOR+1:KYEND-1)  - ZZS1(KXOR+1,KYOR+1:KYEND-1)
      ZZS1(KXEND,KYOR+1:KYEND-1) =                                      &
        2. * ZZS1(KXEND-1,KYOR+1:KYEND-1) - ZZS1(KXEND-2,KYOR+1:KYEND-1)
      IF(KXEND<SIZE(PZS1,1))                                             &
      ZZS1(KXEND+1,KYOR+1:KYEND-1) =                                    &
        2. * ZZS1(KXEND  ,KYOR+1:KYEND-1) - ZZS1(KXEND-1,KYOR+1:KYEND-1)
    END IF
    !
    ! extrapolations (Y direction)
    !
    IXMIN=MAX(KXOR-1,1)
    IXMAX=MIN(KXEND+1,SIZE(PZS1,1))
    IF(KYOR==1 .AND. KYEND==SIZE(PZS1,2) .AND. HLBCY(1)=='CYCL' ) THEN
      ZZS1(IXMIN:IXMAX,KYOR)  = ZZS1(IXMIN:IXMAX,KYEND-1)
      ZZS1(IXMIN:IXMAX,KYEND) = ZZS1(IXMIN:IXMAX,KYOR+1)
    ELSE
      ZZS1(IXMIN:IXMAX,KYOR) =                                       &
        2. * ZZS1(IXMIN:IXMAX,KYOR+1)  - ZZS1(IXMIN:IXMAX,KYOR+2)
      IF(KYOR>1)                                                     &
      ZZS1(IXMIN:IXMAX,KYOR-1) =                                     &
        2. * ZZS1(IXMIN:IXMAX,KYOR)    - ZZS1(IXMIN:IXMAX,KYOR+1)
      ZZS1(IXMIN:IXMAX,KYEND) =                                      &
        2. * ZZS1(IXMIN:IXMAX,KYEND-1) - ZZS1(IXMIN:IXMAX,KYEND-2)
      IF(KYEND<SIZE(PZS1,2))                                          &
      ZZS1(IXMIN:IXMAX,KYEND+1) =                                    &
        2. * ZZS1(IXMIN:IXMAX,KYEND)   - ZZS1(IXMIN:IXMAX,KYEND-1)
    END IF
!
!* next iteration
!
  END DO
!
  CALL ZS_BOUNDARY(PZS2,ZZS2_LS)
!
  WRITE(ILUOUT,FMT=*) 'convergence of ',TRIM(HFIELD),' obtained after ', &
                      JCOUNTER,' iterations'
!
  DEALLOCATE(ZZS2)
  DEALLOCATE(ZDZS)
  DEALLOCATE(ZZS1)
END IF
!
IF (PRESENT(PZS2_LS)) PZS2_LS(:,:)=ZZS2_LS(:,:)
DEALLOCATE(ZZS2_LS)
!
CALL GOTO_MODEL(IMI)
!-------------------------------------------------------------------------------
END SUBROUTINE SPAWN_ZS
!
