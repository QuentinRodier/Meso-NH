!MNH_LIC Copyright 1996-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
module mode_write_diachro

implicit none

private

public :: Write_diachro

contains
!     #################################################################
      SUBROUTINE WRITE_DIACHRO(TPDIAFILE,TPLUOUTDIA,HGROUP,HTYPE,     &
      KGRID, tpdates, PVAR,                                           &
      HTITRE,HUNITE,HCOMMENT,OICP,OJCP,OKCP,KIL,KIH,KJL,KJH,KKL,KKH,  &
      PTRAJX,PTRAJY,PTRAJZ  )
!     #################################################################
!
!!****  *WRITE_DIACHRO* - Ecriture d'un enregistrement dans un fichier
!!                        diachronique (de nom de base HGROUP)
!!
!!    PURPOSE
!!    -------
!      
!
!!**  METHOD
!!    ------
!!      En fait pour un groupe donne HGROUP, on ecrit systematiquement
!       plusieurs enregistrements :
!       - 1: HGROUP.TYPE          (type d'informations a enregistrer)
!       - 2: HGROUP.DIM           (dimensions de toutes les matrices a 
!                                  enregistrer)
!       - 3: HGROUP.TITRE         (Nom des processus)
!       - 4: HGROUP.UNITE         (Unites pour chaque processus)
!       - 5: HGROUP.COMMENT       (Champ commentaire pour chaque processus)
!       - 6: HGROUP.TRAJT         (Temps)
!       - 7: HGROUP.PROCx         (Champ traite . 1 enr./ 1 processus)
!       - 8: HGROUP.DATIM         (Les differentes dates du modele)
!       et pour certains types d'informations on enregistre egalement
!       des coordonnees (HGROUP.TRAJX, HGROUP.TRAJY, HGROUP.TRAJZ)
!!
!!    EXTERNAL
!!    --------
!!      None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      J. Duron    * Laboratoire d'Aerologie *
!!
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       08/01/96
!!      Updated   PM
!!      Modification (N. Asencio) 18/06/99  : the two first dimensions of PMASK
!!                   are linked to the horizontal grid, FMWRIT is called with 'XY' argument. 
!!                   In standard configuration of the budgets, the mask is written once 
!!                   outside this routine with FMWRIT call. Its record name is 'MASK_nnnn.MASK'
!!                   So optional PMASK is not used .
!!      Modification (J. Duron)   24/06/99  : add logical GPACK to disable the pack option,
!!                                            add the initialization of the dimensions of
!!                                          MASK array in MASK case with write outside the 
!!                                          routine.
!!      J.Escobar       02/10/2015 modif for JPHEXT(JPVEXT) variable
!!      D.Gazen+ G.Delautier 06/2016 modif for ncl files
!!      P. Wautelet     09/06/2017: name of the variable added to the name of the written field
!!                                  and better comment (true comment + units)
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 13/09/2019: remove never used PMASK optional dummy-argument
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BUDGET
USE MODD_CONF
USE MODD_IO,             ONLY: TFILEDATA
USE MODD_PARAMETERS,     ONLY: JPHEXT
use modd_time,           only: tdtexp, tdtseg
use modd_time_n,         only: tdtmod
use modd_type_date,      only: date_time
!
use mode_datetime,       only: Datetime_distance
USE MODE_FIELD
USE MODE_IO_FIELD_WRITE, only: IO_Field_write, IO_Field_write_box
USE MODE_ll
use mode_menu_diachro,   only: MENU_DIACHRO
!
IMPLICIT NONE
!
!*       0.1   Dummy arguments
!              ---------------
TYPE(TFILEDATA),              INTENT(IN)          :: TPDIAFILE    ! file to write
TYPE(TFILEDATA),              INTENT(IN)          :: TPLUOUTDIA
CHARACTER(LEN=*),             INTENT(IN)          :: HGROUP, HTYPE
INTEGER,DIMENSION(:),         INTENT(IN)          :: KGRID
type(date_time), dimension(:), intent(in)           :: tpdates
REAL,DIMENSION(:,:,:,:,:,:),  INTENT(IN)          :: PVAR
CHARACTER(LEN=*),DIMENSION(:),INTENT(IN)          :: HTITRE, HUNITE, HCOMMENT
LOGICAL,                      INTENT(IN),OPTIONAL :: OICP, OJCP, OKCP
INTEGER,                      INTENT(IN),OPTIONAL :: KIL, KIH
INTEGER,                      INTENT(IN),OPTIONAL :: KJL, KJH
INTEGER,                      INTENT(IN),OPTIONAL :: KKL, KKH
REAL,DIMENSION(:,:,:),        INTENT(IN),OPTIONAL :: PTRAJX
REAL,DIMENSION(:,:,:),        INTENT(IN),OPTIONAL :: PTRAJY
REAL,DIMENSION(:,:,:),        INTENT(IN),OPTIONAL :: PTRAJZ
!
!*       0.1   Local variables
!              ---------------
CHARACTER(LEN=20) :: YCOMMENT
CHARACTER(LEN=3)  :: YJ
INTEGER   ::   ILENG, ILENTITRE, ILENUNITE, ILENCOMMENT
INTEGER   ::   ILUOUTDIA
INTEGER   ::   II, IJ, IK, IT, IN, IP, J, JJ
INTEGER   ::   INTRAJT, IKTRAJX, IKTRAJY, IKTRAJZ
INTEGER   ::   ITTRAJX, ITTRAJY, ITTRAJZ
INTEGER   ::   INTRAJX, INTRAJY, INTRAJZ
INTEGER   ::   IIMASK, IJMASK, IKMASK, ITMASK, INMASK, IPMASK
INTEGER   ::   ICOMPX, ICOMPY, ICOMPZ
INTEGER   ::   IIMAX_ll, IJMAX_ll ! size of the physical global domain
integer   ::   ji
INTEGER,DIMENSION(:),ALLOCATABLE :: ITABCHAR
logical   :: gicp, gjcp, gkcp
LOGICAL   ::   GPACK
real, dimension(:,:), allocatable :: ztimes
real, dimension(:,:), allocatable :: zdatime
TYPE(TFIELDDATA)  :: TZFIELD
!------------------------------------------------------------------------------

if ( present( oicp ) ) then
  gicp = oicp
else
  gicp = .false.
end if

if ( present( ojcp ) ) then
  gjcp = ojcp
else
  gjcp = .false.
end if

if ( present( okcp ) ) then
  gkcp = okcp
else
  gkcp = .false.
end if

GPACK=LPACK
LPACK=.FALSE.
YCOMMENT='NOTHING'
!
ILUOUTDIA = TPLUOUTDIA%NLU
!
II = SIZE(PVAR,1)
IJ = SIZE(PVAR,2)
IF(HTYPE == 'CART' .AND. .NOT. GICP .AND. .NOT. GJCP) THEN
                              !for parallel execution, PVAR is distributed on several proc
  II=KIH-KIL+1
  IJ=KJH-KJL+1
ENDIF
IK = SIZE(PVAR,3)
IT = SIZE(PVAR,4)
IN = SIZE(PVAR,5)
IP = SIZE(PVAR,6)

INTRAJT=SIZE(tpdates)

IKTRAJX=0; IKTRAJY=0; IKTRAJZ=0
ITTRAJX=0; ITTRAJY=0; ITTRAJZ=0
INTRAJX=0; INTRAJY=0; INTRAJZ=0
IF(PRESENT(PTRAJX))THEN
  IKTRAJX=SIZE(PTRAJX,1)
  ITTRAJX=SIZE(PTRAJX,2)
  INTRAJX=SIZE(PTRAJX,3)
ENDIF
IF(PRESENT(PTRAJY))THEN
  IKTRAJY=SIZE(PTRAJY,1)
  ITTRAJY=SIZE(PTRAJY,2)
  INTRAJY=SIZE(PTRAJY,3)
ENDIF
IF(PRESENT(PTRAJZ))THEN
  IKTRAJZ=SIZE(PTRAJZ,1)
  ITTRAJZ=SIZE(PTRAJZ,2)
  INTRAJZ=SIZE(PTRAJZ,3)
ENDIF

IIMASK=0; IJMASK=0; IKMASK=0; ITMASK=0; INMASK=0; IPMASK=0
IF(HTYPE == 'MASK')THEN
!     MASK is written outside this routine but the dimensions must be initialized
!     the mask is defined on the extended domain
  CALL GET_GLOBALDIMS_ll (IIMAX_ll,IJMAX_ll)
  IIMASK=IIMAX_ll + 2 * JPHEXT
  IJMASK=IJMAX_ll + 2 * JPHEXT
  IKMASK=1
  ITMASK=NBUWRNB
  INMASK=NBUMASK
  IPMASK=1
ENDIF

ILENTITRE = LEN(HTITRE)
ILENUNITE = LEN(HUNITE)
ILENCOMMENT = LEN(HCOMMENT)

ICOMPX=0; ICOMPY=0; ICOMPZ=0
IF ( GICP ) THEN
  ICOMPX = 1
ELSE
  ICOMPX = 0
ENDIF
IF ( GJCP ) THEN
  ICOMPY = 1
ELSE
  ICOMPY = 0
ENDIF
IF ( GKCP ) THEN
  ICOMPZ=1
ELSE
  ICOMPZ = 0
ENDIF
!
IF (NVERB>=5) THEN
  WRITE(ILUOUTDIA,*)' WRITE_DIACHRO: ',TRIM(TPDIAFILE%CNAME)//'.lfi'
ENDIF
!
! 1er enregistrement TYPE
!
TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.TYPE'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.TYPE'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = KGRID(1)
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 0
TZFIELD%LTIMEDEP   = .FALSE.
CALL IO_Field_write(TPDIAFILE,TZFIELD,HTYPE)

IF (NVERB>=5) THEN
  WRITE(ILUOUTDIA,*)'  1st record (',TRIM(TZFIELD%CMNHNAME),'): OK'
ENDIF
!
! 2eme  enregistrement DIMENSIONS des MATRICES et LONGUEUR des TABLEAUX de CARACTERES et FLAGS de COMPRESSION sur les DIFFERENTS AXES
!
TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.DIM'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.DIM'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = KGRID(1)
TZFIELD%NTYPE      = TYPEINT
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.
SELECT CASE(HTYPE)
  CASE('CART','MASK','SPXY')
    ILENG = 34
    ALLOCATE(ITABCHAR(ILENG))
    ITABCHAR(1)=ILENTITRE; ITABCHAR(2)=ILENUNITE
    ITABCHAR(3)=ILENCOMMENT; ITABCHAR(4)=II
    ITABCHAR(5)=IJ; ITABCHAR(6)=IK
    ITABCHAR(7)=IT; ITABCHAR(8)=IN
    ITABCHAR(9)=IP; ITABCHAR(10)=KIL
    ITABCHAR(11)=KJL; ITABCHAR(12)=KKL
    ITABCHAR(13)=KIH; ITABCHAR(14)=KJH
    ITABCHAR(15)=KKH; ITABCHAR(16)=ICOMPX
    ITABCHAR(17)=ICOMPY; ITABCHAR(18)=ICOMPZ
    IF(HTYPE == 'MASK')THEN
!     ITABCHAR(10)=1; ITABCHAR(11)=1
!     ITABCHAR(13)=1; ITABCHAR(14)=1
      ITABCHAR(16)=1; ITABCHAR(17)=1
    ENDIF
    ITABCHAR(19)=INTRAJT; ITABCHAR(20)=IKTRAJX
    ITABCHAR(21)=IKTRAJY; ITABCHAR(22)=IKTRAJZ
    ITABCHAR(23)=ITTRAJX; ITABCHAR(24)=ITTRAJY
    ITABCHAR(25)=ITTRAJZ; ITABCHAR(26)=INTRAJX
    ITABCHAR(27)=INTRAJY; ITABCHAR(28)=INTRAJZ
    ITABCHAR(29)=IIMASK; ITABCHAR(30)=IJMASK
    ITABCHAR(31)=IKMASK; ITABCHAR(32)=ITMASK
    ITABCHAR(33)=INMASK; ITABCHAR(34)=IPMASK
    CALL IO_Field_write(TPDIAFILE,TZFIELD,ITABCHAR)
    DEALLOCATE(ITABCHAR)
    IF (NVERB>=5) THEN
      WRITE(ILUOUTDIA,*)' ILENTITRE,ILENUNITE,ILENCOMMENT ',ILENTITRE,ILENUNITE,ILENCOMMENT
    ENDIF
  CASE DEFAULT
    ILENG = 25 
    ALLOCATE(ITABCHAR(ILENG))
    ITABCHAR(1)=ILENTITRE; ITABCHAR(2)=ILENUNITE
    ITABCHAR(3)=ILENCOMMENT; ITABCHAR(4)=II
    ITABCHAR(5)=IJ; ITABCHAR(6)=IK
    ITABCHAR(7)=IT; ITABCHAR(8)=IN
    ITABCHAR(9)=IP
    ITABCHAR(10)=INTRAJT; ITABCHAR(11)=IKTRAJX
    ITABCHAR(12)=IKTRAJY; ITABCHAR(13)=IKTRAJZ
    ITABCHAR(14)=ITTRAJX; ITABCHAR(15)=ITTRAJY
    ITABCHAR(16)=ITTRAJZ; ITABCHAR(17)=INTRAJX
    ITABCHAR(18)=INTRAJY; ITABCHAR(19)=INTRAJZ
    ITABCHAR(20)=IIMASK; ITABCHAR(21)=IJMASK
    ITABCHAR(22)=IKMASK; ITABCHAR(23)=ITMASK
    ITABCHAR(24)=INMASK; ITABCHAR(25)=IPMASK
    CALL IO_Field_write(TPDIAFILE,TZFIELD,ITABCHAR)
    DEALLOCATE(ITABCHAR)
END SELECT
IF (NVERB>=5) THEN
  WRITE(ILUOUTDIA,*)'  2nd record (',TRIM(TZFIELD%CMNHNAME),'): OK'
ENDIF
!
! 3eme enregistrement TITRE
!
TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.TITRE'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.TITRE'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = KGRID(1)
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.
CALL IO_Field_write(TPDIAFILE,TZFIELD,HTITRE(1:IP))

IF (NVERB>=5) THEN
  WRITE(ILUOUTDIA,*)'  3rd record (',TRIM(TZFIELD%CMNHNAME),'): OK'
ENDIF
!
! 4eme enregistrement UNITE
!
TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.UNITE'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.UNITE'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = KGRID(1)
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.
CALL IO_Field_write(TPDIAFILE,TZFIELD,HUNITE(1:IP))

IF (NVERB>=5) THEN
  WRITE(ILUOUTDIA,*)'  4th record (',TRIM(TZFIELD%CMNHNAME),'): OK'
ENDIF
!
! 5eme enregistrement COMMENT
!
TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.COMMENT'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.COMMENT'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = KGRID(1)
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.
CALL IO_Field_write(TPDIAFILE,TZFIELD,HCOMMENT(1:IP))

IF (NVERB>=5) THEN
  WRITE(ILUOUTDIA,*)'  5th record (',TRIM(TZFIELD%CMNHNAME),'): OK'
ENDIF
!
! 6eme enregistrement PVAR
!
! Dans la mesure ou cette matrice risque d'etre tres volumineuse, on ecrira un 
! enregistrement par processus
!!!!!!!!!!!!!!!!  FUJI  compiler directive !!!!!!!!!!
!ocl scalar
!!!!!!!!!!!!!!!!  FUJI  compiler directive !!!!!!!!!!
DO J = 1,IP
  YJ = '   '
  IF(J < 10)WRITE(YJ,'(I1)')J ; YJ = ADJUSTL(YJ)
  IF(J >= 10 .AND. J < 100) THEN 
          WRITE(YJ,'(I2)')J ; YJ = ADJUSTL(YJ)
  ELSE IF(J >= 100 .AND. J < 1000) THEN 
          WRITE(YJ,'(I3)')J
  ENDIF
  IF(HTYPE == 'CART' .AND. .NOT. GICP .AND. .NOT. GJCP) THEN
    TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.PROC'//YJ
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = TRIM(HUNITE(J))
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = TRIM(HTITRE(J))//' - '//TRIM(HCOMMENT(J))//' ('//TRIM(HUNITE(J))//')'
    TZFIELD%NGRID      = KGRID(J)
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 5
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_Field_write_BOX(TPDIAFILE,TZFIELD,'BUDGET',PVAR(:,:,:,:,:,J), &
                            KIL+JPHEXT,KIH+JPHEXT,KJL+JPHEXT,KJH+JPHEXT)
  ELSE
    TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.PROC'//YJ
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = TRIM(HUNITE(J))
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = TRIM(HTITRE(J))//' - '//TRIM(HCOMMENT(J))//' ('//TRIM(HUNITE(J))//')'
    TZFIELD%NGRID      = KGRID(J)
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 5
    TZFIELD%LTIMEDEP   = .FALSE.
    CALL IO_Field_write(TPDIAFILE,TZFIELD,PVAR(:,:,:,:,:,J))
  ENDIF
  IF (NVERB>=5) THEN
    WRITE(ILUOUTDIA,*)J,TRIM(TZFIELD%CMNHNAME)
  ENDIF
ENDDO
IF (NVERB>=5) THEN
  WRITE(ILUOUTDIA,*)'  6th record: OK'
ENDIF
!
! 7eme enregistrement TRAJT
!
TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.TRAJT'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.TRAJT'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = KGRID(1)
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .FALSE.

!Reconstitute old diachro format
allocate( ztimes( size( tpdates ), 1 ) )

do ji=1,size(tpdates)
  call Datetime_distance( tdtexp, tpdates(ji ), ztimes(ji, 1 ) )
end do

call IO_Field_write( tpdiafile, tzfield, ztimes )

deallocate( ztimes )

IF (NVERB>=5) THEN
  WRITE(ILUOUTDIA,*)'  7th record (',TRIM(TZFIELD%CMNHNAME),'): OK'
ENDIF
!
! Dans certains cas
!
!
! 8eme enregistrement TRAJX
!
IF(PRESENT(PTRAJX))THEN
  TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.TRAJX'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.TRAJX'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
  TZFIELD%NGRID      = KGRID(1)
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(TPDIAFILE,TZFIELD,PTRAJX)
ENDIF
!
! 9eme enregistrement TRAJY
!
IF(PRESENT(PTRAJY))THEN
  TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.TRAJY'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.TRAJY'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
  TZFIELD%NGRID      = KGRID(1)
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(TPDIAFILE,TZFIELD,PTRAJY)
ENDIF
!
! 10eme enregistrement TRAJZ
!
IF(PRESENT(PTRAJZ))THEN
  TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.TRAJZ'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.TRAJZ'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
  TZFIELD%NGRID      = KGRID(1)
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(TPDIAFILE,TZFIELD,PTRAJZ)
ENDIF
!
! 11eme enregistrement PDATIME
!
TZFIELD%CMNHNAME   = TRIM(HGROUP)//'.DATIM'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(HGROUP)//'.DATIM'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = KGRID(1)
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .FALSE.

!Reconstitute old diachro format
allocate( zdatime( 16, size(tpdates) ) )

zdatime(1,  : ) = tdtexp%tdate%year
zdatime(2,  : ) = tdtexp%tdate%month
zdatime(3,  : ) = tdtexp%tdate%day
zdatime(4,  : ) = tdtexp%time
zdatime(5,  : ) = tdtseg%tdate%year
zdatime(6,  : ) = tdtseg%tdate%month
zdatime(7,  : ) = tdtseg%tdate%day
zdatime(8,  : ) = tdtseg%time
zdatime(9,  : ) = tdtmod%tdate%year
zdatime(10, : ) = tdtmod%tdate%month
zdatime(11, : ) = tdtmod%tdate%day
zdatime(12, : ) = tdtmod%time
zdatime(13, : ) = tpdates(:)%tdate%year
zdatime(14, : ) = tpdates(:)%tdate%month
zdatime(15, : ) = tpdates(:)%tdate%day
zdatime(16, : ) = tpdates(:)%time

call IO_Field_write( tpdiafile, tzfield, zdatime )

deallocate( zdatime )
!
CALL MENU_DIACHRO(TPDIAFILE,HGROUP)
LPACK=GPACK
!-----------------------------------------------------------------------------
!
!*       2.       EXITS
!                 -----
! 
RETURN
END SUBROUTINE WRITE_DIACHRO

end module mode_write_diachro
