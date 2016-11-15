!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SURF_FIELD3D( HPROGRAM,PFIELD3D,KFIRSTLAYER,KLASTLAYER,HFIELDNAME,HCOMMENT,HDIR)
!     #####################################
!
!!****  *READ_SURF_FIELD3D* - reads surfex field in input file using READ_SURF,
!!                           layer by layer and patch by patch if needed in MESONH
!!                           with Z-parallel IO in MESO-NH, we force surfex to write 2D fields
!!                           because Z-parallel IO are not supported for 3D SURFEX fields.
!!                        
!!
!!    PURPOSE
!!    -------
!!      reads surfex field in output file using WRITE_SURF, layer by layer 
!!      and patch by patch if needed in MESONH
!!      and NB_PROCIO_R > 1
!!      examples of HFIELDNAME : 'TG', 'soil depth from ecoclimap'
!!      with Z-parallel IO in MESO-NH, we force surfex to write 2D fields
!!      because Z-parallel IO are not supported for 3D SURFEX fields.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      M.Moge   *LA - UPS*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/01/2016
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
USE MODI_READ_SURF
#ifdef SFX_MNH
USE MODI_GET_NB_PROCIO_READ_MNH
#endif
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),                 INTENT(IN) :: HPROGRAM     ! calling program
REAL, DIMENSION(:,:,:),        INTENT(INOUT) :: PFIELD3D     ! 3D field to be read
INTEGER,                          INTENT(IN) :: KFIRSTLAYER  ! first layer of PFIELD3D to be read
INTEGER,                          INTENT(IN) :: KLASTLAYER   ! last layer of PFIELD3D to be read
CHARACTER(LEN=LEN_HREC),                INTENT(IN) :: HFIELDNAME   ! name of the field PFIELD3D. Example : 'X_Y_TG'
 CHARACTER(LEN=*), OPTIONAL,     INTENT(OUT) :: HCOMMENT   !comment string
CHARACTER(LEN=1),OPTIONAL,        INTENT(IN) :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(PFIELD3D,1),SIZE(PFIELD3D,3)) :: ZWORK
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
INTEGER           :: ILAYER         ! number of layers in PFIELD3D
INTEGER           :: IPATCH         ! number of patches in PFIELD3D
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=4 ) :: YLVL           ! current level/layer
CHARACTER(LEN=4 ) :: YPATCH         ! current patch
INTEGER           :: INB_PROCIO     ! number of processes used for Z-parallel IO with MESO-NH
!
CHARACTER(LEN=1)   :: YDIR
INTEGER :: JJ, JLAYER ! loop counter on levels
INTEGER :: JPATCH  ! loop counter on patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_FIELD3D',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
ILAYER = SIZE( PFIELD3D, 2 )
IPATCH = SIZE( PFIELD3D, 3 )
!
INB_PROCIO = 1
#ifdef SFX_MNH
IF (HPROGRAM=='MESONH') THEN
  CALL GET_NB_PROCIO_READ_MNH( INB_PROCIO, IRESP )
ENDIF
#endif
!
IF ( INB_PROCIO > 1 ) THEN
!
  DO JLAYER=KFIRSTLAYER,KLASTLAYER
!
    DO JPATCH=1,IPATCH
      WRITE(YLVL,'(I4)') JLAYER
      YRECFM=ADJUSTL(HFIELDNAME(:LEN_TRIM(HFIELDNAME)))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      WRITE(YPATCH,'(I4.4)') JPATCH
      IF ( IPATCH > 1 ) THEN
        YRECFM=ADJUSTL(YRECFM(:LEN_TRIM(YRECFM)))//YPATCH
      ENDIF
      CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:,JPATCH),IRESP,HCOMMENT=YCOMMENT,HDIR=YDIR)
    ENDDO
    PFIELD3D(:,JLAYER,:)=ZWORK
!
  END DO
!
ELSE
!
  DO JLAYER=KFIRSTLAYER,KLASTLAYER
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM=ADJUSTL(HFIELDNAME(:LEN_TRIM(HFIELDNAME)))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    CALL READ_SURF(HPROGRAM,YRECFM,ZWORK(:,:),IRESP,HCOMMENT=YCOMMENT,HDIR=YDIR)
    PFIELD3D(:,JLAYER,:)=ZWORK
  END DO
!
ENDIF
!
IF (PRESENT(HDIR)) HCOMMENT = YCOMMENT
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_FIELD3D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE READ_SURF_FIELD3D
