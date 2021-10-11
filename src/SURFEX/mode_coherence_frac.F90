!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODE_COHERENCE_FRAC
!
!!**** *MODE_COHERENCE_FRAC* check fractions coherence
!!
!!    PURPOSE
!!    -------
!!    check fractions coherence enter via a namelist
!!
!!    METHOD
!!    ------
!!    The following test are made :
!!     - Does the namelist contains values for every fractions ?
!!     - Are all fractions positive ?
!!     - Is fractions sum egal 1 for each mesh ?
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    M. Goret        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    02/2017
!!
!!
!----------------------------------------------------------------------------
!
!
!
!
 CONTAINS
!
!
!#####################################################
!General subroutine
SUBROUTINE COHERENCE_FRAC(HPROGRAM,PFRAC_VALUE, CD_NAME, L_SUM_CHECK)
!
USE MODD_CSTS ,ONLY : XSURF_EPSILON
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
REAL,      DIMENSION(:,:), INTENT(INOUT)  :: PFRAC_VALUE ! fractions values
CHARACTER(LEN=*),          INTENT(IN)     :: CD_NAME     ! fractions name (for messages)
CHARACTER(LEN=6),          INTENT(IN)     :: HPROGRAM    ! Type of program
LOGICAL, OPTIONAL,         INTENT(IN)     :: L_SUM_CHECK ! 1=sum of all fractions is checked
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
REAL, PARAMETER                         :: JPEPSILON=0.01    ! tolerance error for test
INTEGER                                 :: ILUOUT            ! logical unit of output file
INTEGER                                 :: JFRAC             ! loop control variable
REAL,    DIMENSION(:), ALLOCATABLE      :: ZSUM              ! sum of the n fractions
REAL(KIND=JPRB)                         :: ZHOOK_HANDLE   
LOGICAL                                 :: SUM_CHECK         ! 1=sum of all fractions is checked
!
!
!*    1.1    Get output listing file logical unit
!            ------------------------------------
IF (LHOOK) CALL DR_HOOK('COHERENCE_FRAC',0,ZHOOK_HANDLE)
CALL GET_LUOUT(HPROGRAM,ILUOUT)
IF (PRESENT(L_SUM_CHECK)) THEN
    SUM_CHECK=L_SUM_CHECK
ELSE
    SUM_CHECK=.TRUE.
ENDIF
!
!
!*    1.2    Check all fractions are above 0
!            ---------------------------------------
    IF (MINVAL(PFRAC_VALUE).LT.-XSURF_EPSILON) THEN
        WRITE(ILUOUT,*) '---------------------------------------------'
        WRITE(ILUOUT,*) 'Please enter positive values for ',CD_NAME,' fractions'
        WRITE(ILUOUT,*) 'The minimum accepted value is 0.0 '
        WRITE(ILUOUT,*) '---------------------------------------------'
        CALL ABOR1_SFX('COHERENCE_FRAC: Please enter positive values for '//CD_NAME//' fractions')
    ENDIF
!
!
!*    1.3    Check for fraction total
!            -------------------------
 IF (SUM_CHECK) THEN
    ALLOCATE(ZSUM(SIZE(PFRAC_VALUE,2)))
    ZSUM=SUM (PFRAC_VALUE, DIM = 1)
    !
    IF (MAXVAL(ABS(ZSUM-1.0)).GT.JPEPSILON) THEN
      WRITE(ILUOUT,*) '---------------------------------------------'
      WRITE(ILUOUT,*) 'Error in ',CD_NAME,' fractions preparation            '
      WRITE(ILUOUT,*) 'The prescribed fractions do not fit                     '
      WRITE(ILUOUT,*) 'The sum of all ',SIZE(PFRAC_VALUE,1),' fractions must be approximatly equal to 1.'
      WRITE(ILUOUT,*) 'Curently the accepted error is ', JPEPSILON,'.'
      WRITE(ILUOUT,*) 'If needed, this value can be updated in COHERENCE_FRAC'
      WRITE(ILUOUT,*) '---------------------------------------------'
      CALL ABOR1_SFX('COHERENCE_FRAC: SUM OF ALL '//CD_NAME//' FRACTIONS MUST BE 1.')
   ELSE
      !
      ! Renormalisation for small deviations 
      !
      DO JFRAC=1,SIZE(PFRAC_VALUE,1)
         PFRAC_VALUE(JFRAC,:)=PFRAC_VALUE(JFRAC,:)/ZSUM(:)
      ENDDO
      !
      ! The sum of fractions must be exactly 1.0
      !
      ZSUM=SUM (PFRAC_VALUE, DIM = 1)
      !
      IF (MAXVAL(ABS(ZSUM-1.0)).GT.XSURF_EPSILON) THEN
         WRITE(ILUOUT,*) '---------------------------------------------'
         WRITE(ILUOUT,*) 'Error in ',CD_NAME,' fractions preparation   '
         WRITE(ILUOUT,*) '---------------------------------------------'
         CALL ABOR1_SFX('COHERENCE_FRAC: SUM OF ALL '//CD_NAME//' FRACTIONS MUST BE 1.')
      ENDIF
      !        
   ENDIF
 ENDIF
!
IF (LHOOK) CALL DR_HOOK('COHERENCE_FRAC',1,ZHOOK_HANDLE)
!
END SUBROUTINE COHERENCE_FRAC
!#####################################################
!
!
!#####################################################
! subroutine for the particular case of heat fractions
SUBROUTINE COHERENCE_FRAC_HEAT(HPROGRAM,DTB, KDIM)
!
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*    0.1    Declaration of arguments
!            ------------------------
TYPE(DATA_BEM_t),  INTENT(INOUT) :: DTB      ! data
INTEGER,           INTENT(IN)    :: KDIM     ! teb grid dimension
CHARACTER(LEN=6),  INTENT(IN)    :: HPROGRAM ! Type of program
!
!*    0.2    Declaration of local variables
!            ------------------------------
INTEGER, PARAMETER               :: JPFRAC=4    ! number of fractions
REAL,    DIMENSION(JPFRAC,KDIM)  :: ZFRAC_VALUE ! fraction values
INTEGER                          :: ILUOUT      ! logical unit of output file
REAL(KIND=JPRB)                  :: ZHOOK_HANDLE   
!
!
!
!*    1.1    Get output listing file logical unit
!            ------------------------------------
IF (LHOOK) CALL DR_HOOK('COHERENCE_FRAC_HEAT',0,ZHOOK_HANDLE)
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!
!*    1.2    Take default values if not all heating fractions provided 
!            ----------------------------------------------------------
IF     ((.NOT. DTB%LDATA_FRAC_HEAT_ELEC) .OR. (.NOT. DTB%LDATA_FRAC_HEAT_GAS) &
    .OR.(.NOT. DTB%LDATA_FRAC_HEAT_FUEL) .OR. (.NOT. DTB%LDATA_FRAC_HEAT_OTHER)) THEN
   !
   DEALLOCATE(DTB%XPAR_FRAC_HEAT_ELEC)
   DEALLOCATE(DTB%XPAR_FRAC_HEAT_GAS)
   DEALLOCATE(DTB%XPAR_FRAC_HEAT_FUEL)
   DEALLOCATE(DTB%XPAR_FRAC_HEAT_OTHER)!
   !
   DTB%LDATA_FRAC_HEAT_ELEC =.FALSE.
   DTB%LDATA_FRAC_HEAT_GAS  =.FALSE.
   DTB%LDATA_FRAC_HEAT_FUEL =.FALSE.
   DTB%LDATA_FRAC_HEAT_OTHER=.FALSE.
   DTB%LDATA_FRAC_HEAT=.FALSE.
   !
   WRITE(ILUOUT,*) " -------------------------------------------------------------------------- "
   WRITE(ILUOUT,*) "Default heating source fractions are taken as not all fractions are provided"
   WRITE(ILUOUT,*) " "
   WRITE(ILUOUT,*) " -------------------------------------------------------------------------- "
!
!
!*    1.3    Else further coherence tests are made 
!            -------------------------------------
ELSE
   !
   DTB%LDATA_FRAC_HEAT=.TRUE.
   !
   ZFRAC_VALUE(1,:)=DTB%XPAR_FRAC_HEAT_ELEC(:)
   ZFRAC_VALUE(2,:)=DTB%XPAR_FRAC_HEAT_GAS(:)
   ZFRAC_VALUE(3,:)=DTB%XPAR_FRAC_HEAT_FUEL(:)
   ZFRAC_VALUE(4,:)=DTB%XPAR_FRAC_HEAT_OTHER(:)
   !
   CALL COHERENCE_FRAC(HPROGRAM,ZFRAC_VALUE, "heating source")
   !
   DTB%XPAR_FRAC_HEAT_ELEC =ZFRAC_VALUE(1,:)
   DTB%XPAR_FRAC_HEAT_GAS  =ZFRAC_VALUE(2,:)
   DTB%XPAR_FRAC_HEAT_FUEL =ZFRAC_VALUE(3,:)
   DTB%XPAR_FRAC_HEAT_OTHER=ZFRAC_VALUE(4,:)

ENDIF
!
IF (LHOOK) CALL DR_HOOK('COHERENCE_FRAC_HEAT',1,ZHOOK_HANDLE)
!
END SUBROUTINE COHERENCE_FRAC_HEAT
!######################################################
!
!######################################################
!
END MODULE MODE_COHERENCE_FRAC
