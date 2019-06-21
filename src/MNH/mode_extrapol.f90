!MNH_LIC Copyright 2011-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!-----------------------------------------------------------------
MODULE MODE_EXTRAPOL

  use mode_msg

  implicit none

  private

  public :: EXTRAPOL, EXTRAPOL_ON_PSEUDO_HALO

  character(len=10) :: ydim1, ydim2 !Strings to store dimensions to print error message

  INTERFACE EXTRAPOL

     MODULE PROCEDURE EXTRAPOL3D,EXTRAPOL3DN,EXTRAPOL2D,EXTRAPOL2DN

  END INTERFACE

  INTERFACE EXTRAPOL_ON_PSEUDO_HALO

     MODULE PROCEDURE EXTRAPOL_ON_PSEUDO_HALO3D,EXTRAPOL_ON_PSEUDO_HALO2D

  END INTERFACE

CONTAINS

  SUBROUTINE EXTRAPOL3D(HBORD,PTAB)
    USE MODD_LBC_n
    USE MODE_TOOLS_ll
    !
    IMPLICIT NONE
    !
    !*       0.1   Declarations of arguments
    !
    CHARACTER              :: HBORD
    REAL, DIMENSION(:,:,:) :: PTAB

    !
    !*       0.2   Declarations of local variables
    !
    INTEGER          :: IIB,IJB,IKB    ! Begining useful area  in x,y,z directions
    INTEGER          :: IIE,IJE,IKE    ! End useful area in x,y,z directions
    !
    !-------------------------------------------------------------------------------
    !
    !*       1.     EXTRAPOLE LATERAL BOUNDARY CONDITIONS :
    !               ---------------------------------------
    !
    !RETURN
    CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)

    SELECT  CASE (HBORD)
    CASE ('W')
       IF (LWEST_ll() .AND. CLBCX(1)/='CYCL')  &
            PTAB(IIB-1,:,:) = 2. * PTAB(IIB,:,:) - PTAB(IIB+1,:,:)
    CASE ('E')
       IF (LEAST_ll() .AND. CLBCX(1)/='CYCL')   &
            PTAB(IIE+1,:,:) = 2. * PTAB(IIE,:,:) - PTAB(IIE-1,:,:)
    CASE ('S')
       IF (LSOUTH_ll() .AND. CLBCY(1)/='CYCL') &
            PTAB(:,IJB-1,:) = 2. * PTAB(:,IJB,:) - PTAB(:,IJB+1,:)
    CASE ('N')
       IF (LNORTH_ll() .AND. CLBCY(1)/='CYCL') &
            PTAB(:,IJE+1,:) = 2. * PTAB(:,IJE,:) - PTAB(:,IJE-1,:)
    CASE   DEFAULT
    END SELECT

  END SUBROUTINE EXTRAPOL3D

  SUBROUTINE EXTRAPOL3DN(HBORD,P1,P2,P3,P4,P5,P6 )
    IMPLICIT NONE
    CHARACTER              :: HBORD
    REAL, DIMENSION(:,:,:)            :: P1,P2
    REAL, DIMENSION(:,:,:) , OPTIONAL :: P3,P4,P5,P6

    CALL EXTRAPOL(HBORD,P1)
    CALL EXTRAPOL(HBORD,P2)
    IF (PRESENT(P3)) CALL EXTRAPOL(HBORD,P3)
    IF (PRESENT(P4)) CALL EXTRAPOL(HBORD,P4)
    IF (PRESENT(P5)) CALL EXTRAPOL(HBORD,P5)
    IF (PRESENT(P6)) CALL EXTRAPOL(HBORD,P6)

  END SUBROUTINE EXTRAPOL3DN

  SUBROUTINE EXTRAPOL2D(HBORD,PTAB)
    USE MODD_LBC_n
    USE MODE_TOOLS_ll
    !
    IMPLICIT NONE
    !
    !*       0.1   Declarations of arguments
    !
    CHARACTER              :: HBORD
    REAL, DIMENSION(:,:) :: PTAB

    !
    !*       0.2   Declarations of local variables
    !
    INTEGER          :: IIB,IJB    ! Begining useful area  in x,y,z directions
    INTEGER          :: IIE,IJE    ! End useful area in x,y,z directions
    !
    !-------------------------------------------------------------------------------
    !
    !*       1.     EXTRAPOLE LATERAL BOUNDARY CONDITIONS :
    !               ---------------------------------------
    !
    !RETURN
    CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)

    SELECT  CASE (HBORD)
    CASE ('W')
       IF (LWEST_ll() .AND. CLBCX(1)/='CYCL')  &
            PTAB(IIB-1,:) = 2. * PTAB(IIB,:) - PTAB(IIB+1,:)
    CASE ('E')
       IF (LEAST_ll() .AND. CLBCX(1)/='CYCL')   &
            PTAB(IIE+1,:) = 2. * PTAB(IIE,:) - PTAB(IIE-1,:)
    CASE ('S')
       IF (LSOUTH_ll() .AND. CLBCY(1)/='CYCL') &
            PTAB(:,IJB-1) = 2. * PTAB(:,IJB) - PTAB(:,IJB+1)
    CASE ('N')
       IF (LNORTH_ll() .AND. CLBCY(1)/='CYCL') &
            PTAB(:,IJE+1) = 2. * PTAB(:,IJE) - PTAB(:,IJE-1)
    CASE   DEFAULT
    END SELECT

  END SUBROUTINE EXTRAPOL2D

  SUBROUTINE EXTRAPOL2DN(HBORD,P1,P2,P3,P4,P5,P6 )
    IMPLICIT NONE
    CHARACTER              :: HBORD
    REAL, DIMENSION(:,:)            :: P1,P2
    REAL, DIMENSION(:,:) , OPTIONAL :: P3,P4,P5,P6

    CALL EXTRAPOL(HBORD,P1)
    CALL EXTRAPOL(HBORD,P2)
    IF (PRESENT(P3)) CALL EXTRAPOL(HBORD,P3)
    IF (PRESENT(P4)) CALL EXTRAPOL(HBORD,P4)
    IF (PRESENT(P5)) CALL EXTRAPOL(HBORD,P5)
    IF (PRESENT(P6)) CALL EXTRAPOL(HBORD,P6)

  END SUBROUTINE EXTRAPOL2DN

!     #######################################################################
  SUBROUTINE EXTRAPOL_ON_PSEUDO_HALO3D(PTAB,OCYCLIC_EXTRAPOL)
!     #######################################################################
!
!!****  *EXTRAPOL_ON_PSEUDO_HALO3D * - when using LS_FORCING_ll with a 
!!                child domain defined on the whole father domain (possibly minus 1 point)
!!                we need to extrapolate the field on the child model before doing the interpolation
!!                from the father grid to the child grid
!!
!!    AUTHOR
!!    ------
!!
!!       M.Moge     * LA - CNRS *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    18/02/2015
!!      J.Escobar 2/05/2016 : add STOP in case of problem with decomposition
!-------------------------------------------------------------------------------
    USE MODD_LBC_n
    use mode_argslist_ll,    only: ADD2DFIELD_ll, CLEANLIST_ll
    USE MODE_MODELN_HANDLER
    use mode_nest_ll,        only: GET_CHILD_DIM_ll, GO_TOMODEL_ll
    USE MODE_TOOLS_ll
    USE MODD_PARAMETERS,     ONLY: JPHEXT
    USE MODE_EXCHANGE_ll,    ONLY: UPDATE_HALO_EXTENDED_ll
    !
    IMPLICIT NONE
    !
    !*       0.1   Declarations of arguments
    !
    REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PTAB
    LOGICAL, OPTIONAL, INTENT(IN) :: OCYCLIC_EXTRAPOL   !if true, we consider the cyclic case if necessary, if false, we do the extrapolation even in the cyclic case

    !
    !*       0.2   Declarations of local variables
    !
    character(len=6) :: ynum
    INTEGER          :: IIB,IJB,IKB     ! Begining useful area  in x,y,z directions
    INTEGER          :: IIE,IJE,IKE     ! End useful area in x,y,z directions
    INTEGER          :: IDIMX_C,IDIMY_C ! size of the child domain (in the father grid)
    INTEGER          :: IINFO_ll
    INTEGER          :: II
    TYPE(LIST_ll), POINTER :: TZZSFIELD_ll   ! list of fields to exchange
    LOGICAL :: GCYCLIC_EXTRAPOL
    !
    !
    !-------------------------------------------------------------------------------
    !
    !*       1.     EXTRAPOLATE LATERAL BOUNDARY CONDITIONS :
    !               ---------------------------------------
    !
    IF ( PRESENT(OCYCLIC_EXTRAPOL) ) THEN
      GCYCLIC_EXTRAPOL = OCYCLIC_EXTRAPOL
    ELSE
      GCYCLIC_EXTRAPOL = .TRUE.
    ENDIF
    !
    CALL GOTO_MODEL(1)
    CALL GO_TOMODEL_ll(1, IINFO_ll)
    CALL GET_CHILD_DIM_ll(2, IDIMX_C, IDIMY_C, IINFO_ll)
    CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
    CALL GO_TOMODEL_ll(2, IINFO_ll)
    CALL GOTO_MODEL(2)
    ! if the child domain has the same size as the father domain in X or Y
    ! AND the boundary conditions are CYCLIC in the corresponding direction
    ! we perform an UPDATE_HALO_ll instead of an extrapolation
    IF ( GCYCLIC_EXTRAPOL .AND. ( ((IDIMX_C > IIE - IIB + 1 + 2*JPHEXT) .AND. CLBCX(1)=='CYCL' )&
            .OR. ((IDIMY_C > IJE - IJB + 1 + 2*JPHEXT) .AND. CLBCY(1)=='CYCL') ) ) THEN
      CALL GOTO_MODEL(1)
      CALL GO_TOMODEL_ll(1, IINFO_ll)
      DO II=1,SIZE(PTAB,3)
        NULLIFY(TZZSFIELD_ll)
        write ( ynum, '( I6 )' ) ii
        CALL ADD2DFIELD_ll( TZZSFIELD_ll, PTAB(:,:,II), 'EXTRAPOL_ON_PSEUDO_HALO3D::PTAB:'//trim( adjustl( ynum ) ) )
        CALL UPDATE_HALO_EXTENDED_ll(TZZSFIELD_ll,IINFO_ll)
        CALL CLEANLIST_ll(TZZSFIELD_ll)
      ENDDO
      CALL GO_TOMODEL_ll(2, IINFO_ll)
      CALL GOTO_MODEL(2)
    ENDIF
!
!we take into account the case of a child domain of the size of the father domain minus 1
    IF ( IDIMX_C > IIE - IIB + 1 + 2*JPHEXT ) THEN
      IF ( IDIMX_C == IIE - IIB + 3 + 2*JPHEXT ) THEN !the child domain has the same size as the father domain
        IF ( LWEST_ll() .AND. (CLBCX(1)/='CYCL' .OR. .NOT. GCYCLIC_EXTRAPOL) )  THEN !du cote ouest, on a un point dans le 'pseudo halo' a extrapoler
          PTAB(1,:,:) = 2. * PTAB(2,:,:) - PTAB(3,:,:)
        ENDIF
        IF ( LEAST_ll() .AND. (CLBCX(1)/='CYCL' .OR. .NOT. GCYCLIC_EXTRAPOL) )  THEN !du cote est, on a un point dans le 'pseudo halo' a extrapoler
          PTAB(IDIMX_C,:,:) = 2. * PTAB(IDIMX_C-1,:,:) - PTAB(IDIMX_C-2,:,:)
        ENDIF
      ELSEIF ( IDIMX_C == IIE - IIB + 2 + 2*JPHEXT ) THEN !the child domain has the size of the father domain minus one
        call Print_msg(NVERB_FATAL,'GEN','EXTRAPOL_ON_PSEUDO_HALO3D','case not supported:'// &
                       'the child grid has to be one point larger or one point smaller in X dim')
!        IF ( IIB>1 .AND. LWEST_ll() .AND. CLBCX(1)/='CYCL' )  THEN !du cote ouest, on a un point dans le 'pseudo halo' a extrapoler
!          PTAB(1,:,:) = 2. * PTAB(2,:,:) - PTAB(3,:,:)
!        ELSEIF ( IIB>1 .AND. LWEST_ll() .AND. CLBCX(1)=='CYCL' ) THEN
!          PTAB(1,:,:) = PTAB(IDIMX_C-1,:,:)
!        ENDIF
!        IF ( IIB==1 .AND. LEAST_ll() .AND. CLBCX(1)/='CYCL' )  THEN !du cote est, on a un point dans le 'pseudo halo' a extrapoler
!          PTAB(IDIMX_C,:,:) = 2. * PTAB(IDIMX_C-1,:,:) - PTAB(IDIMX_C-2,:,:)
!        ELSEIF ( IIB==1 .AND. LEAST_ll() .AND. CLBCX(1)=='CYCL' ) THEN
!          PTAB(IDIMX_C,:,:) = PTAB(2,:,:)
!        ENDIF
      ELSE !Error, this should not happen
        write( ydim1, '( I10 )' ) IDIMX_C
        write( ydim2, '( I10 )' ) IIE - IIB + 1 + 2*JPHEXT
        call Print_msg( NVERB_FATAL, 'GEN','EXTRAPOL_ON_PSEUDO_HALO3D', 'wrong dimensions: IDIMX_C='//trim(ydim1)// &
                        ', IIE - IIB + 1 + 2*JPHEXT='//trim(ydim2) )
      ENDIF
    ENDIF
    IF ( IDIMY_C > IJE - IJB + 1 + 2*JPHEXT ) THEN
      IF ( IDIMY_C == IJE - IJB + 3 + 2*JPHEXT ) THEN !the child domain has the same size as the father domain
        IF ( LNORTH_ll() .AND. (CLBCY(1)/='CYCL' .OR. .NOT. GCYCLIC_EXTRAPOL) )  THEN !du cote ouest, on a un point dans le 'pseudo halo' a extrapoler
          PTAB(:,1,:) = 2. * PTAB(:,2,:) - PTAB(:,3,:)
        ENDIF
        IF ( LSOUTH_ll() .AND. (CLBCY(1)/='CYCL' ) .OR. .NOT. GCYCLIC_EXTRAPOL)  THEN !du cote est, on a un point dans le 'pseudo halo' a extrapoler
          PTAB(:,IDIMY_C,:) = 2. * PTAB(:,IDIMY_C-1,:) - PTAB(:,IDIMY_C-2,:)
        ENDIF
      ELSEIF ( IDIMY_C == IJE - IJB + 2 + 2*JPHEXT ) THEN !the child domain has the size of the father domain minus one
        call Print_msg(NVERB_FATAL,'GEN','EXTRAPOL_ON_PSEUDO_HALO3D','case not supported:'// &
                       'the child grid has to be one point larger or one point smaller in Y dim')
!        IF ( IJB>1 .AND. LNORTH_ll() .AND. CLBCY(1)/='CYCL' )  THEN !du cote ouest, on a un point dans le 'pseudo halo' a extrapoler
!          PTAB(:,1,:) = 2. * PTAB(:,2,:) - PTAB(:,3,:)
!        ELSEIF ( IJB>1 .AND. LNORTH_ll() .AND. CLBCY(1)=='CYCL' ) THEN
!          PTAB(:,1,:) = PTAB(:,IDIMY_C-1,:)
!        ENDIF
!        IF ( IJB==1 .AND. LSOUTH_ll() .AND. CLBCY(1)/='CYCL' )  THEN !du cote est, on a un point dans le 'pseudo halo' a extrapoler
!          PTAB(:,IDIMY_C,:) = 2. * PTAB(:,IDIMY_C-1,:) - PTAB(:,IDIMY_C-2,:)
!        ELSEIF ( IJB==1 .AND. LSOUTH_ll() .AND. CLBCY(1)=='CYCL' ) THEN
!          PTAB(:,IDIMY_C,:) = PTAB(:,2,:)
!        ENDIF
      ELSE !Error, this should not happen
        write( ydim1, '( I10 )' ) IDIMX_C
        write( ydim2, '( I10 )' ) IJE - IJB + 1 + 2*JPHEXT
        call Print_msg( NVERB_FATAL, 'GEN','EXTRAPOL_ON_PSEUDO_HALO3D', 'wrong dimensions: IDIMY_C='//trim(ydim1)// &
                        ', IJE - IJB + 1 + 2*JPHEXT='//trim(ydim2) )
      ENDIF
    ENDIF
!
  END SUBROUTINE EXTRAPOL_ON_PSEUDO_HALO3D
  
!     #######################################################################
  SUBROUTINE EXTRAPOL_ON_PSEUDO_HALO2D(PTAB,OCYCLIC_EXTRAPOL)
!     #######################################################################
!
!!****  *EXTRAPOL_ON_PSEUDO_HALO2D * - when using LS_FORCING_ll with a 
!!                child domain defined on the whole father domain (possibly minus 1 point)
!!                we need to extrapolate the field on the child model before doing the interpolation
!!                from the father grid to the child grid
!!
!!    AUTHOR
!!    ------
!!
!!       M.Moge     * LA - CNRS *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    18/02/2015
!!      J.Escobar 2/05/2016 : add STOP in case of problem with decomposition
!-------------------------------------------------------------------------------
    USE MODD_LBC_n
    use mode_argslist_ll,    only: ADD2DFIELD_ll, CLEANLIST_ll
    USE MODE_MODELN_HANDLER
    use mode_nest_ll,        only: GET_CHILD_DIM_ll, GO_TOMODEL_ll
    USE MODE_TOOLS_ll
    USE MODD_PARAMETERS,     ONLY: JPHEXT
    USE MODE_EXCHANGE_ll,    ONLY: UPDATE_HALO_EXTENDED_ll
    !
    IMPLICIT NONE
    !
    !*       0.1   Declarations of arguments
    !
    REAL, DIMENSION(:,:), INTENT(INOUT) :: PTAB
    LOGICAL, OPTIONAL, INTENT(IN) :: OCYCLIC_EXTRAPOL   !if true, we consider the cyclic case if necessary, if false, we do the extrapolation even in the cyclic case

    !
    !*       0.2   Declarations of local variables
    !
    INTEGER          :: IIB,IJB,IKB     ! Begining useful area  in x,y,z directions
    INTEGER          :: IIE,IJE,IKE     ! End useful area in x,y,z directions
    INTEGER          :: IDIMX_C,IDIMY_C ! size of the child domain (in the father grid)
    INTEGER          :: IINFO_ll
    TYPE(LIST_ll), POINTER :: TZZSFIELD_ll   ! list of fields to exchange
    LOGICAL :: GCYCLIC_EXTRAPOL
    !
    !-------------------------------------------------------------------------------
    !
    !*       1.     EXTRAPOLATE LATERAL BOUNDARY CONDITIONS :
    !               ---------------------------------------
    !
    IF ( PRESENT(OCYCLIC_EXTRAPOL) ) THEN
      GCYCLIC_EXTRAPOL = OCYCLIC_EXTRAPOL
    ELSE
      GCYCLIC_EXTRAPOL = .TRUE.
    ENDIF
    !
    CALL GOTO_MODEL(1)
    CALL GO_TOMODEL_ll(1, IINFO_ll)
    CALL GET_CHILD_DIM_ll(2, IDIMX_C, IDIMY_C, IINFO_ll)
    CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
    CALL GO_TOMODEL_ll(2, IINFO_ll)
    CALL GOTO_MODEL(2)
    ! if the child domain has the same size as the father domain in X or Y
    ! AND the boundary conditions are CYCLIC in the corresponding direction
    ! we perform an UPDATE_HALO_ll instead of an extrapolation
    IF ( GCYCLIC_EXTRAPOL .AND. ( ((IDIMX_C > IIE - IIB + 1 + 2*JPHEXT) .AND. CLBCX(1)=='CYCL' )&
            .OR. ((IDIMY_C > IJE - IJB + 1 + 2*JPHEXT) .AND. CLBCY(1)=='CYCL') ) ) THEN
      CALL GOTO_MODEL(1)
      CALL GO_TOMODEL_ll(1, IINFO_ll)
      NULLIFY(TZZSFIELD_ll)
      CALL ADD2DFIELD_ll( TZZSFIELD_ll, PTAB, 'EXTRAPOL_ON_PSEUDO_HALO3D::PTAB' )
      CALL UPDATE_HALO_EXTENDED_ll(TZZSFIELD_ll,IINFO_ll)
      CALL CLEANLIST_ll(TZZSFIELD_ll)
      CALL GO_TOMODEL_ll(2, IINFO_ll)
      CALL GOTO_MODEL(2)
    ENDIF
!    
!we take into account the case of a child domain of the size of the father domain minus 1
    IF ( IDIMX_C > IIE - IIB + 1 + 2*JPHEXT ) THEN
      IF ( IDIMX_C == IIE - IIB + 3 + 2*JPHEXT ) THEN !the child domain has the same size as the father domain
        IF ( LWEST_ll() .AND. (CLBCX(1)/='CYCL' .OR. .NOT. GCYCLIC_EXTRAPOL) )  THEN !du cote ouest, on a un point dans le 'pseudo halo' a extrapoler
          PTAB(1,:) = 2. * PTAB(2,:) - PTAB(3,:)
        ENDIF
        IF ( LEAST_ll() .AND. (CLBCX(1)/='CYCL' .OR. .NOT. GCYCLIC_EXTRAPOL) )  THEN !du cote est, on a un point dans le 'pseudo halo' a extrapoler
          PTAB(IDIMX_C,:) = 2. * PTAB(IDIMX_C-1,:) - PTAB(IDIMX_C-2,:)
        ENDIF
      ELSEIF ( IDIMX_C == IIE - IIB + 2 + 2*JPHEXT ) THEN !the child domain has the size of the father domain minus one
        call Print_msg(NVERB_FATAL,'GEN','EXTRAPOL_ON_PSEUDO_HALO2D','case not supported:'// &
                       'the child grid has to be one point larger or one point smaller in X dim')
!        IF ( IIB>1 .AND. LWEST_ll() .AND. CLBCX(1)/='CYCL' )  THEN !du cote ouest, on a un point dans le 'pseudo halo' a extrapoler
!          PTAB(1,:) = 2. * PTAB(2,:) - PTAB(3,:)
!        ELSEIF ( IIB>1 .AND. LWEST_ll() .AND. CLBCX(1)=='CYCL' ) THEN
!          PTAB(1,:) = PTAB(IDIMX_C-1,:)
!        ENDIF
!        IF ( IIB==1 .AND. LEAST_ll() .AND. CLBCX(1)/='CYCL' )  THEN !du cote est, on a un point dans le 'pseudo halo' a extrapoler
!          PTAB(IDIMX_C,:) = 2. * PTAB(IDIMX_C-1,:) - PTAB(IDIMX_C-2,:)
!        ELSEIF ( IIB==1 .AND. LEAST_ll() .AND. CLBCX(1)=='CYCL' ) THEN
!          PTAB(IDIMX_C,:) = PTAB(2,:)
!        ENDIF
      ELSE !Error, this should not happen
        write( ydim1, '( I10 )' ) IDIMX_C
        write( ydim2, '( I10 )' ) IIE - IIB + 1 + 2*JPHEXT
        call Print_msg( NVERB_FATAL, 'GEN','EXTRAPOL_ON_PSEUDO_HALO2D', 'wrong dimensions: IDIMX_C='//trim(ydim1)// &
                        ', IIE - IIB + 1 + 2*JPHEXT='//trim(ydim2) )
      ENDIF
    ENDIF
    IF ( IDIMY_C > IJE - IJB + 1 + 2*JPHEXT ) THEN
      IF ( IDIMY_C == IJE - IJB + 3 + 2*JPHEXT ) THEN !the child domain has the same size as the father domain
        IF ( LNORTH_ll() .AND. (CLBCY(1)/='CYCL' .OR. .NOT. GCYCLIC_EXTRAPOL) )  THEN !du cote ouest, on a un point dans le 'pseudo halo' a extrapoler
          PTAB(:,1) = 2. * PTAB(:,2) - PTAB(:,3)
!        ELSEIF ( LNORTH_ll() .AND. CLBCY(1)=='CYCL' ) THEN
!          PTAB(:,1) = PTAB(:,IDIMY_C-1)
        ENDIF
        IF ( LSOUTH_ll() .AND. (CLBCY(1)/='CYCL' .OR. .NOT. GCYCLIC_EXTRAPOL) )  THEN !du cote est, on a un point dans le 'pseudo halo' a extrapoler
          PTAB(:,IDIMY_C) = 2. * PTAB(:,IDIMY_C-1) - PTAB(:,IDIMY_C-2)
!        ELSEIF ( LSOUTH_ll() .AND. CLBCY(1)=='CYCL' ) THEN
!          PTAB(:,IDIMY_C) = PTAB(:,2)
        ENDIF
      ELSEIF ( IDIMY_C == IJE - IJB + 2 + 2*JPHEXT ) THEN !the child domain has the size of the father domain minus one
        call Print_msg(NVERB_FATAL,'GEN','EXTRAPOL_ON_PSEUDO_HALO3D','case not supported:'// &
                       'the child grid has to be one point larger or one point smaller in Y dim')
!        IF ( IJB>1 .AND. LNORTH_ll() .AND. CLBCY(1)/='CYCL' )  THEN !du cote ouest, on a un point dans le 'pseudo halo' a extrapoler
!          PTAB(:,1) = 2. * PTAB(:,2) - PTAB(:,3)
!        ELSEIF ( IJB>1 .AND. LNORTH_ll() .AND. CLBCY(1)=='CYCL' ) THEN
!          PTAB(:,1) = PTAB(:,IDIMY_C-1)
!        ENDIF
!        IF ( IJB==1 .AND. LSOUTH_ll() .AND. CLBCY(1)/='CYCL' )  THEN !du cote est, on a un point dans le 'pseudo halo' a extrapoler
!          PTAB(:,IDIMY_C) = 2. * PTAB(:,IDIMY_C-1) - PTAB(:,IDIMY_C-2)
!        ELSEIF ( IJB==1 .AND. LSOUTH_ll() .AND. CLBCY(1)=='CYCL' ) THEN
!          PTAB(:,IDIMY_C) = PTAB(:,2)
!        ENDIF
      ELSE !Error, this should not happen
        write( ydim1, '( I10 )' ) IDIMX_C
        write( ydim2, '( I10 )' ) IJE - IJB + 1 + 2*JPHEXT
        call Print_msg( NVERB_FATAL, 'GEN','EXTRAPOL_ON_PSEUDO_HALO3D', 'wrong dimensions: IDIMY_C='//trim(ydim1)// &
                        ', IJE - IJB + 1 + 2*JPHEXT='//trim(ydim2) )
      ENDIF
    ENDIF
!
  END SUBROUTINE EXTRAPOL_ON_PSEUDO_HALO2D

END MODULE MODE_EXTRAPOL
