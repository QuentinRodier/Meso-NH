!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_FIELD (DTCO, UG, U, USS, &
                            HPROGRAM,HFIELD,HAREA,HFILE,HFILETYPE,PUNIF,PFIELD,OPRESENT)
!     ##############################################################
!
!!**** *PGD_FIELD* monitor for averaging and interpolations of ISBA physiographic fields
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!    09/2010 (E. Kourzeneva):   interpolation of the lake depth 
!!                               is not allowed and not necessary
!!
!!    02/2014 (B. Decharme):     interpolation of the lake depth 
!!                               re-allowed but using the nearest point
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGD_GRID,       ONLY : NL
USE MODD_PGDWORK,        ONLY : XSUMVAL, NSIZE, CATYPE,      &
                                NVALNBR, NVALCOUNT, XVALLIST, JPVALMAX
!
USE MODI_GET_LUOUT
USE MODI_TREAT_FIELD
USE MODI_INTERPOL_FIELD
USE MODI_PACK_SAME_RANK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM  ! Type of program
 CHARACTER(LEN=*),  INTENT(IN) :: HFIELD    ! field name for prints
 CHARACTER(LEN=3),  INTENT(IN) :: HAREA     ! area where field is defined
!                                          ! 'ALL' : everywhere
!                                          ! 'NAT' : on nature
!                                          ! 'TWN' : on town
!                                          ! 'SEA' : on sea
!                                          ! 'WAT' : on inland waters
 CHARACTER(LEN=28), INTENT(IN) :: HFILE     ! data file name
 CHARACTER(LEN=6),  INTENT(IN) :: HFILETYPE ! data file type
REAL,              INTENT(IN) :: PUNIF     ! prescribed uniform value for field
REAL, DIMENSION(:),INTENT(OUT):: PFIELD    ! physiographic field
LOGICAL, OPTIONAL, INTENT(OUT) :: OPRESENT
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: ILUOUT ! output listing logical unit
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
INTEGER                        :: IDIM   !

!
 CHARACTER(LEN=20)   :: YFIELD
 CHARACTER(LEN=6)    :: YMASK
INTEGER             :: INPTS     ! number of points used for interpolation
REAL, DIMENSION(NL) :: ZFIELD    ! physiographic field on full grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_FIELD',0,ZHOOK_HANDLE)
ZFIELD(:) = XUNDEF
IF (PRESENT(OPRESENT)) OPRESENT=.TRUE.
!-------------------------------------------------------------------------------
!
!*    2.      Output listing logical unit
!             ---------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    3.      Read from file
!             --------------
!
IF (LEN_TRIM(HFILE)/=0) THEN
!       
!-------------------------------------------------------------------------------
!
!*    4.      Averages the field
!             ------------------
!
  ALLOCATE(NSIZE     (NL))
  ALLOCATE(XSUMVAL   (NL))
!
  NSIZE    (:) = 0.
  XSUMVAL  (:) = 0.
  INPTS        = 3
!
  IF(HFIELD=="water depth") THEN
    INPTS = 1
  ENDIF  
!
  IF (CATYPE=='MAJ') THEN
    ALLOCATE(NVALNBR  (NL))
    ALLOCATE(NVALCOUNT(NL,JPVALMAX))
    ALLOCATE(XVALLIST (NL,JPVALMAX))
    NVALNBR   = 0
    NVALCOUNT = 0
    XVALLIST  = XUNDEF
    INPTS     = 1
  END IF
!
  YFIELD = '                    '
  YFIELD = HFIELD(1:MIN(LEN(HFIELD),20))
!
  CALL TREAT_FIELD(UG, U, USS, &
                   HPROGRAM,'SURF  ',HFILETYPE,'A_MESH',HFILE,   &
                   YFIELD,ZFIELD,HAREA                           )  
!
!-------------------------------------------------------------------------------
!
!*    4.      Mask for the interpolations
!             ---------------------------
!
  SELECT CASE (HAREA)
    CASE ('LAN')
      WHERE ((U%XTOWN(:)+U%XNATURE(:))==0. .AND. NSIZE(:)==0 ) NSIZE(:) = -1
    CASE ('TWN')
      WHERE (U%XTOWN  (:)==0. .AND. NSIZE(:)==0 ) NSIZE(:) = -1
    CASE ('BLD')
      WHERE (U%XTOWN  (:)==0. .AND. NSIZE(:)==0 ) NSIZE(:) = -1              
    CASE ('NAT')
      WHERE (U%XNATURE(:)==0. .AND. NSIZE(:)==0 ) NSIZE(:) = -1
    CASE ('SEA')
      WHERE (U%XSEA   (:)==0. .AND. NSIZE(:)==0 ) NSIZE(:) = -1
    CASE ('WAT')
      WHERE (U%XWATER (:)==0. .AND. NSIZE(:)==0 ) NSIZE(:) = -1
  END SELECT
!
!-------------------------------------------------------------------------------
!
!*    5.      Interpolation if some points are not initialized (no data for these points)
!             ------------------------------------------------
!
  IF (PUNIF/=XUNDEF) THEN
    CALL INTERPOL_FIELD(UG, U, &
                        HPROGRAM,ILUOUT,NSIZE,ZFIELD(:),HFIELD,PDEF=PUNIF,KNPTS=INPTS)
  ELSE
    CALL INTERPOL_FIELD(UG, U, &
                        HPROGRAM,ILUOUT,NSIZE,ZFIELD(:),HFIELD)
  END IF          
!
  DEALLOCATE(NSIZE    )
  DEALLOCATE(XSUMVAL  )
  IF (CATYPE=='MAJ') THEN
    DEALLOCATE(NVALNBR  )
    DEALLOCATE(NVALCOUNT)
    DEALLOCATE(XVALLIST )
  END IF
!
!-------------------------------------------------------------------------------
!
ELSEIF (PUNIF/=XUNDEF) THEN
!
!*    3.1     Use of the presribed field
!             --------------------------
!
  ZFIELD(:) = PUNIF
!
ELSE
!
  IF (PRESENT(OPRESENT)) THEN
    OPRESENT=.FALSE.
    IF (LHOOK) CALL DR_HOOK('PGD_FIELD',1,ZHOOK_HANDLE)
    RETURN
  ENDIF
!
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in PGD field preparation of field : ', HFIELD
  WRITE(ILUOUT,*) '* There is no prescribed value and no input file          *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_FIELD: NO PRESCRIBED VALUE NOR INPUT FILE FOR '//HFIELD)
!
END IF
!-------------------------------------------------------------------------------
!
!*    6.      Mask for the field
!             ------------------
!
SELECT CASE (HAREA)
  CASE ('LAN')
          YMASK = 'LAND  '
  CASE ('TWN')
          YMASK = 'TOWN  '
    CASE ('BLD')
          YMASK = 'TOWN '              
  CASE ('NAT')
          YMASK = 'NATURE'
  CASE ('SEA')
          YMASK = 'SEA   '
  CASE ('WAT')
          YMASK = 'WATER '
  CASE DEFAULT
          PFIELD(:) = ZFIELD(:)
          IF (LHOOK) CALL DR_HOOK('PGD_FIELD',1,ZHOOK_HANDLE)
          RETURN
END SELECT

 CALL GET_TYPE_DIM_n(DTCO, U, &
                     YMASK,IDIM)
IF (IDIM/=SIZE(PFIELD)) THEN
   WRITE(ILUOUT,*)'Wrong dimension of MASK: ',IDIM,SIZE(PFIELD)
   CALL ABOR1_SFX('PGD_FIELD: WRONG DIMENSION OF MASK')
ENDIF

ALLOCATE(IMASK(IDIM))
ILU=0
 CALL GET_SURF_MASK_n(DTCO, U, &
                      YMASK,IDIM,IMASK,ILU,ILUOUT)
 CALL PACK_SAME_RANK(IMASK,ZFIELD(:),PFIELD(:))
DEALLOCATE(IMASK)
IF (LHOOK) CALL DR_HOOK('PGD_FIELD',1,ZHOOK_HANDLE)

!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_FIELD
