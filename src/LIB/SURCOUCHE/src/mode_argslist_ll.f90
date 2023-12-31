!MNH_LIC Copyright 1998-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------

!!    #######################
      MODULE MODE_ARGSLIST_ll
!!    #######################
!
!!****  *MODE_ARGSLIST_ll *-
!
!!    Routines Of The User Interface
!!    ------------------------------
!
!    SUBROUTINES : ADD1DFIELD_ll, ADD2DFIELD_ll, ADD3DFIELD_ll
!                  DEL1DFIELD_ll, DEL2DFIELD_ll, DEL3DFIELD_ll
!                  CLEANLIST_ll, CLEANLIST1D_ll
!
!!    Purpose
!!    -------
!     This module manages a list of fields. Fields may be added
!     (routines ADD1DFIELD_ll, ADD2DFIELD_ll, ADD3DFIELD_ll) or deleted 
!     (routines DEL1DFIELD_ll, DEL2DFIELD_ll, DEL3DFIELD_ll) from
!     the list. The list can then be used in routines where
!     all fields in the list are handled identically, e.g.
!     the distribution of the data or the update of the halos.
!     The list may contain only one type of fields (1D, 2D or 3D).
! 
!!    Reference
!!     ---------
!
!     User interface for the Meso-NH parallel package
!
!!    Authors
!!    -------
!
!     Ph. Kloos                 * CERFACS - CNRM *
!
!!    Implicit Arguments
!!     ------------------
!
!     Module MODD_ARGSLIST_ll
!         LIST_ll : list of 1D/2D/3D fields
!         LIST1D_ll : list of 1D fields
!
!!    Modifications
!!    -------------
!     Original    May 19, 1998
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!  P. Wautelet 07/12/2020: add CLEANLIST1D_ll subroutine
!-------------------------------------------------------------------------------
!
! Implicit arguments
!
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll, LIST1D_ll
!
  CONTAINS
!
!!    #####################################################
      SUBROUTINE ADD1DFIELD_ll(HDIR, TPLIST, PFIELD, HNAME)
!!    #####################################################
!
!!****  *ADD1DFIELD_ll* -
!
!!    Purpose
!!    -------
!     This routine is used to add a 1D field (PFIELD) to a list of 
!     1D fields (TPLIST). 
!
!!    Reference
!!    ---------
!
!     User interface for the Meso-NH parallel package
!
!!    Implicit Arguments
!!    ------------------
!
!     Module MODD_ARGSLIST_ll
!         LIST1D_ll : list of 1D fields
!!    Author
!!    ------
!
!     Ph. Kloos                 * CERFACS - CNRM *
!
!!    Modifications
!!    -------------
!     Original    May 19, 1998
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!*       0.1   declarations of arguments
!
  use mode_msg

  IMPLICIT NONE
!
  CHARACTER(LEN=1), INTENT(IN) :: HDIR ! direction of the field ("X" or "Y")
  TYPE(LIST1D_ll), POINTER     :: TPLIST ! list of fields
  REAL, DIMENSION(:), TARGET   :: PFIELD ! field to be added
                                         ! to the list of fields
  character(len=*), intent(in) :: HNAME ! Name of the field to be added
!
!*       0.2   declarations of local variables
!
  TYPE(LIST1D_ll), POINTER :: TZLIST
!
!-------------------------------------------------------------------------------
!
!*       1.    Test value of HDIR
!
  IF (HDIR /= "X" .AND. HDIR /= "Y") THEN
    call Print_msg( NVERB_FATAL, 'GEN', 'ADD1DFIELD', 'bad HDIR argument ('//HDIR//')' )
  ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.    In case TPLIST has not been already used
!              ----------------------------------------
  IF (.NOT. ASSOCIATED(TPLIST)) THEN ! TPLIST is empty
!
    ALLOCATE(TPLIST)
    TPLIST%ARRAY1D => PFIELD
    NULLIFY(TPLIST%NEXT)
    TPLIST%NCARD = 1
    TPLIST%CDIR = HDIR
    tplist%cname = hname
!
  ELSE 
!
!-------------------------------------------------------------------------------
!
!*       3.    TPLIST already contains fields;
!*             add the PFIELD field at the end of the list
!              -------------------------------------------------
!
    TZLIST => TPLIST
    DO WHILE (ASSOCIATED(TZLIST%NEXT))
      TZLIST => TZLIST%NEXT
    ENDDO
!
    ALLOCATE(TZLIST%NEXT)
    TZLIST => TZLIST%NEXT
    TZLIST%ARRAY1D => PFIELD
    TZLIST%NCARD = 0
    TZLIST%CDIR = HDIR
    tzlist%cname = hname
    NULLIFY(TZLIST%NEXT)
!
    TPLIST%NCARD = TPLIST%NCARD + 1
!
  ENDIF
! 
!-------------------------------------------------------------------------------
!
      END SUBROUTINE ADD1DFIELD_ll
!
!!    ###############################################
      SUBROUTINE ADD2DFIELD_ll(TPLIST, PFIELD, HNAME)
!!    ###############################################
!
!!****  *ADD2DFIELD_ll* -
!
!!    Purpose
!!    -------
!     This routine is used to add a 2D field (PFIELD) to a list of 
!     2D fields (TPLIST). 
! 
!!    Reference
!!    ---------
! 
!     User interface for the Meso-NH parallel package
! 
!!    Implicit Arguments
!!    ------------------
!
!     Module MODD_ARGSLIST :
!        LIST_ll : list of fields
!
!!    Author
!!    ------
!
!     Ph. Kloos                 * CERFACS - CNRM *
!
!!    Modifications
!!    -------------
!     Original    May 19, 1998
! 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!*       0.1   declarations of arguments
!
  IMPLICIT NONE
!
  TYPE(LIST_ll), POINTER       :: TPLIST ! list of fields
  REAL, DIMENSION(:,:), TARGET :: PFIELD ! field to be added
                                         ! to the list of fields
  character(len=*), intent(in) :: HNAME ! Name of the field to be added
!
!*       0.2   declarations of local variables
!
  TYPE(LIST_ll), POINTER :: TZLIST
!
!-------------------------------------------------------------------------------
!
!*       1.    In case TPLIST has not been already used
!              ----------------------------------------
  IF (.NOT. ASSOCIATED(TPLIST)) THEN ! TPLIST is empty
!
    ALLOCATE(TPLIST)
    NULLIFY(TPLIST%ARRAY1D)
    NULLIFY(TPLIST%ARRAY3D)
    TPLIST%ARRAY2D => PFIELD
    NULLIFY(TPLIST%NEXT)
    TPLIST%NCARD = 1
    tplist%cname  = hname
    TPLIST%L1D = .FALSE.
    TPLIST%L2D = .TRUE.
    TPLIST%L3D = .FALSE.
!
  ELSE 
!
!-------------------------------------------------------------------------------
!
!*       2.    TPLIST already contains fields;
!*             add the PFIELD field at the end of the list
!
    TZLIST => TPLIST
    DO WHILE (ASSOCIATED(TZLIST%NEXT))
      TZLIST => TZLIST%NEXT
    ENDDO
!
    ALLOCATE(TZLIST%NEXT)
    TZLIST => TZLIST%NEXT
    NULLIFY(TZLIST%ARRAY1D)
    NULLIFY(TZLIST%ARRAY3D)
    TZLIST%ARRAY2D => PFIELD
    TZLIST%NCARD = 0
    tzlist%cname = hname
    TZLIST%L1D = .FALSE.
    TZLIST%L2D = .TRUE.
    TZLIST%L3D = .FALSE.
    NULLIFY(TZLIST%NEXT)
!
    TPLIST%NCARD = TPLIST%NCARD + 1
!
  ENDIF
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE ADD2DFIELD_ll
!
!!    ###############################################
      SUBROUTINE ADD3DFIELD_ll(TPLIST, PFIELD, HNAME)
!!    ###############################################
!
!!****  *ADD3DFIELD_ll* -
!
!!    Purpose
!!    -------
!     This routine is used to add a 3D field (PFIELD) to a list of 
!     3D fields (TPLIST). 
!
!!    Reference
!!    ---------
!
!     User interface for the Meso-NH parallel package
!
!!    Implicit Arguments
!!    ------------------
!
!     Module MODD_ARGSLIST :
!         LIST_ll : list of fields
!
!!    Author
!!    ------
!
!     Ph. Kloos                 * CERFACS - CNRM *
!
!!    Modifications
!!    -------------
!     Original    May 19, 1998
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!*       0.1   declarations of arguments
!
  IMPLICIT NONE
!
  TYPE(LIST_ll), POINTER         :: TPLIST   ! list of fields
  REAL, DIMENSION(:,:,:), TARGET :: PFIELD   ! field to be added to the list
!                                              of fields
  character(len=*), intent(in) :: HNAME ! Name of the field to be added
!
!*       0.2   declarations of local variables
!
  TYPE(LIST_ll), POINTER :: TZLIST
!
!-------------------------------------------------------------------------------
!
!*       1.    In case TPLIST has not been already used
!              ----------------------------------------
!
  IF (.NOT. ASSOCIATED(TPLIST)) THEN ! TPLIST is empty
!
    ALLOCATE(TPLIST)
    NULLIFY(TPLIST%ARRAY1D)
    NULLIFY(TPLIST%ARRAY2D)
    TPLIST%ARRAY3D => PFIELD
    NULLIFY(TPLIST%NEXT)
    TPLIST%NCARD = 1
    tplist%cname = hname
    TPLIST%L1D = .FALSE.
    TPLIST%L2D = .FALSE.
    TPLIST%L3D = .TRUE.
!
  ELSE 
!
!-------------------------------------------------------------------------------
!
!*       2.    TPLIST already contains fields;
!*             add the PFIELD field at the end of the list
!              -------------------------------------------
!
    TZLIST => TPLIST
    DO WHILE (ASSOCIATED(TZLIST%NEXT))
      TZLIST => TZLIST%NEXT
    ENDDO  
!
    ALLOCATE(TZLIST%NEXT)
    TZLIST => TZLIST%NEXT
    NULLIFY(TZLIST%ARRAY1D)
    NULLIFY(TZLIST%ARRAY2D)
    TZLIST%ARRAY3D => PFIELD
    TZLIST%NCARD = 0
    tzlist%cname = hname
    TZLIST%L1D = .FALSE.
    TZLIST%L2D = .FALSE.
    TZLIST%L3D = .TRUE.
    NULLIFY(TZLIST%NEXT)
!
    TPLIST%NCARD = TPLIST%NCARD + 1
!
  ENDIF
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE ADD3DFIELD_ll



!################################################
subroutine add4dfield_ll( tplist, pfield, hname )
!################################################
!
!!****  *ADD3DFIELD_ll* -
!
!!    Purpose
!!    -------
!     This routine is used to add a 4D field (PFIELD) to a list of
!     4D fields (TPLIST).
!     For the moment, it is split in a series of 3D fields
!
!!    Reference
!!    ---------
!
!     User interface for the Meso-NH parallel package
!
!!    Implicit Arguments
!!    ------------------
!
!     Module MODD_ARGSLIST :
!         LIST_ll : list of fields
!
!!    Author
!!    ------
!
!  P. Wautelet 20/05/2019
!
!!    Modifications
!    -------------
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!*       0.1   declarations of arguments
!
  implicit none
!
  type(list_ll), pointer   :: tplist   ! list of fields
  real, dimension(:,:,:,:),   intent(in) :: pfield   ! field to be added to the list of fields
  character(len=*), intent(in) :: hname ! name of the field to be added
!
!*       0.2   declarations of local variables
!
  character(len=6)              :: ynum
  integer                       :: ji

  do ji = 1, size( pfield, 4 )
    write ( ynum, '( I6.3 )' ) ji
    call add3dfield_ll(tplist, pfield(:,:,:,ji), trim( hname )//':'//trim( adjustl( ynum ) ) )
  end do

end subroutine add4dfield_ll



!!    ###############################################
      SUBROUTINE DEL1DFIELD_ll(TPLIST, PFIELD, KINFO)
!!    ###############################################
!
!!****  *DEL1DFIELD_ll* -
!
!!    Purpose
!!    -------
!     This routine deletes the PFIELD 1D array from the TPLIST list of fields
!
!!    Reference
!!    ---------
!
!     User interface for the Meso-NH parallel package
!
!!    Implicit Arguments
!!    ------------------
!
!     Module MODD_ARGSLIST
!        LIST1D_ll : type for a list of 1D field
!
!!    Author
!!    ------
!
!     Ph. Kloos                 * CERFACS - CNRM *
!     Didier Gazen              * LA *
!     Ronan Guivarch            * CERFACS - ENSEEIHT *
!
!!    Modifications
!!    -------------
!     Original    May 19, 1998
!     Didier Gazen (ajout DEALLOCATE)
!     Ronan Guivarch LIST_ll -> LIST1D_ll 23/02/00
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!*       0.1   declarations of arguments
!
  IMPLICIT NONE
!
  TYPE(LIST1D_ll), POINTER     :: TPLIST ! list of fields
  REAL, DIMENSION(:), TARGET   :: PFIELD ! field to be deleted from the list
                                         ! of fields
  INTEGER, INTENT(OUT)         :: KINFO  ! return status : 
                                         ! 0 if PFIELD has been found 
                                         ! in TPLIST, 1 otherwise.
! 
!*       0.2   declarations of local variables
!
  INTEGER                  :: NCARD ! number of elements in TPLIST
  TYPE(LIST1D_ll), POINTER :: TZCURRENT, TZPREV, TZTEMP 
!
!-------------------------------------------------------------------------------
!
  KINFO=1
!
!*       1.    Remove PFIELD from TPLIST 
!              -------------------------
!
  IF (ASSOCIATED(TPLIST)) THEN
    ! TPLIST is not empty : good
    NULLIFY(TZPREV)
    TZCURRENT => TPLIST
    NCARD = TPLIST%NCARD
    DO WHILE(ASSOCIATED(TZCURRENT))
      ! look for all occurrences of PFIELD in TPLIST
      !
      IF (.NOT. ASSOCIATED(TZCURRENT%ARRAY1D,PFIELD)) THEN
        ! PFIELD not found, try next element
        TZPREV => TZCURRENT
        TZCURRENT => TZCURRENT%NEXT
      ELSE
        ! PFIELD found in TZCURRENT
        TZTEMP=>TZCURRENT     
        ! Delete TZCURRENT from TPLIST
        IF (ASSOCIATED(TZPREV)) THEN
          ! not the first element of TPLIST
          TZPREV%NEXT => TZCURRENT%NEXT
        ELSE
          ! first element of TPLIST
          TPLIST => TZCURRENT%NEXT
        END IF
!
        TZCURRENT   => TZCURRENT%NEXT
        NCARD = NCARD-1
        IF (NCARD>0) TPLIST%NCARD = NCARD
!
        DEALLOCATE(TZTEMP)
        KINFO=0
      END IF
    END DO
  END IF
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE DEL1DFIELD_ll
!
!!    ###############################################
      SUBROUTINE DEL2DFIELD_ll(TPLIST, PFIELD, KINFO)
!!    ###############################################
!
!!****  *DEL2DFIELD_ll* -
!
!!    Purpose
!!    -------
!     This routine deletes the PFIELD 2D array from the TPLIST list of fields
!     
!!    Reference
!!    ---------
!
!     User interface for the Meso-NH parallel package
!
!!    Implicit Arguments
!!    ------------------
!
!     Module MODD_ARGSLIST
!        LIST_ll : type for a list of fields
!
!!    Author
!!    ------
!
!     Ph. Kloos                 * CERFACS - CNRM *
!     Didier Gazen              * LA *
!
!!    Modifications
!!    -------------
!     Original    May 19, 1998
!     Didier Gazen (ajout DEALLOCATE)
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!*       0.1   declarations of arguments
!
  IMPLICIT NONE
!
  TYPE(LIST_ll), POINTER       :: TPLIST ! list of fields
  REAL, DIMENSION(:,:), TARGET :: PFIELD ! field to be deleted from the list
                                         ! of fields
!
  INTEGER, INTENT(OUT)       :: KINFO  ! return status : 0 if PFIELD
                                       ! has been found  in TPLIST, 1 otherwise.
!
!
!*       0.2   declarations of local variables
!
  INTEGER :: NCARD ! number of elements in TPLIST
  TYPE(LIST_ll), POINTER :: TZCURRENT, TZPREV, TZTEMP 
!
!-------------------------------------------------------------------------------
!
  KINFO=1
!
!*       1.    Remove PFIELD from TPLIST 
!              -------------------------
!
  IF (ASSOCIATED(TPLIST)) THEN
    ! TPLIST is not empty : good
    NULLIFY(TZPREV)
    TZCURRENT => TPLIST
    NCARD = TPLIST%NCARD
    DO WHILE(ASSOCIATED(TZCURRENT))
      ! look for all occurrences of PFIELD in TPLIST
      !
      IF (.NOT. ASSOCIATED(TZCURRENT%ARRAY2D,PFIELD)) THEN
        ! PFIELD not found, try next element
        TZPREV => TZCURRENT
        TZCURRENT => TZCURRENT%NEXT
      ELSE
        ! PFIELD found in TZCURRENT
        TZTEMP=>TZCURRENT
        ! Delete TZCURRENT from TPLIST
        IF (ASSOCIATED(TZPREV)) THEN
          ! not the first element of TPLIST
          TZPREV%NEXT => TZCURRENT%NEXT
        ELSE
          ! first element of TPLIST
          TPLIST => TZCURRENT%NEXT
        END IF
!      
        TZCURRENT   => TZCURRENT%NEXT
        NCARD = NCARD-1
        IF (NCARD>0) TPLIST%NCARD = NCARD
!
        DEALLOCATE(TZTEMP)
        KINFO=0
      END IF
    END DO
  END IF
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE DEL2DFIELD_ll
!
!!    ###############################################
      SUBROUTINE DEL3DFIELD_ll(TPLIST, PFIELD, KINFO)
!!    ###############################################
!!
!!    Purpose
!!    -------
!     This routine deletes the PFIELD 3D array from the TPLIST list of fields 
!     
!!    Reference
!!    ---------
!
!     User interface for the Meso-NH parallel package
!
!!    Implicit Arguments
!!    ------------------
!
!     Module MODD_ARGSLIST
!        LIST_ll : type for a list of fields
!
!!    Author
!!    ------
!
!     Ph. Kloos                 * CERFACS - CNRM *
!     Didier Gazen              * LA *
!
!!    Modifications
!!    -------------
!     Original    May 19, 1998
!     Didier Gazen (ajout DEALLOCATE)
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!*       0.1   declarations of arguments
!
  IMPLICIT NONE
!
  TYPE(LIST_ll), POINTER         :: TPLIST ! list of fields
  REAL, DIMENSION(:,:,:), TARGET :: PFIELD ! field to be deleted
                                           ! from the list of fields
  INTEGER, INTENT(OUT)           :: KINFO  ! return status : 
                                           ! 0 if PFIELD has been found 
                                           ! in TPLIST, 1 otherwise
!
!*       0.2   declarations of local variables
!
  INTEGER :: NCARD ! number of elements in TPLIST
  TYPE(LIST_ll), POINTER :: TZCURRENT, TZPREV, TZTEMP 
!
!-------------------------------------------------------------------------------
!
  KINFO=1
!
!*       1.    Remove PFIELD from TPLIST 
!              -------------------------
!
  IF (ASSOCIATED(TPLIST)) THEN
    ! TPLIST is not empty : good
    NULLIFY(TZPREV)
    TZCURRENT => TPLIST
    NCARD = TPLIST%NCARD
    DO WHILE(ASSOCIATED(TZCURRENT))
      ! look for all occurrences of PFIELD in TPLIST
      !
      IF (.NOT. ASSOCIATED(TZCURRENT%ARRAY3D,PFIELD)) THEN
        ! PFIELD not found, try next element
        TZPREV => TZCURRENT
        TZCURRENT => TZCURRENT%NEXT
      ELSE
        ! PFIELD found in TZCURRENT
        TZTEMP=>TZCURRENT
        ! Delete TZCURRENT from TPLIST
        IF (ASSOCIATED(TZPREV)) THEN
          ! not the first element of TPLIST
          TZPREV%NEXT => TZCURRENT%NEXT
        ELSE
          ! first element of TPLIST
          TPLIST => TZCURRENT%NEXT
        END IF
!      
        TZCURRENT   => TZCURRENT%NEXT
        NCARD = NCARD-1
        IF (NCARD>0) TPLIST%NCARD = NCARD
!
        DEALLOCATE(TZTEMP)
        KINFO=0
      END IF
    END DO
  END IF
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE DEL3DFIELD_ll
!
!
!!    ###############################
      SUBROUTINE CLEANLIST_ll(TPLIST)
!!    ###############################
!
!!****  *CLEANLIST_ll* -
!
!!    Purpose
!!    -------
!     This routine frees a TPLIST by deallocating all elements
!     created by the ADDxDFIELD routines. Arrays associated to each
!     element are untouched.
!     
!!    Reference
!!    ---------
!
!     User interface for the Meso-NH parallel package
!
!!    Implicit Arguments
!!    ------------------
!
!     Module MODD_ARGSLIST
!        LIST_ll : type for a list of fields
!
!!    Author
!!    ------
!
!     Didier Gazen              * LA *
!
!!    Modifications
!!    -------------
!     Original    May 19, 1998
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
!*       0.1   declarations of arguments
!
  IMPLICIT NONE 
!
  TYPE(LIST_ll),  POINTER :: TPLIST ! List of fields
!
!*       0.2   declarations of local variables
!
  TYPE(LIST_ll), POINTER :: TZTEMP
!
!------------------------------------------------------------------------------
!
!*       1.    Dealloacte one by one the elements of TPLIST
!              --------------------------------------------
!
  IF (ASSOCIATED(TPLIST)) THEN
    DO WHILE(ASSOCIATED(TPLIST))
      TZTEMP => TPLIST
      TPLIST => TPLIST%NEXT
      DEALLOCATE(TZTEMP)
    END DO
    NULLIFY(TPLIST)
  END IF
!
!------------------------------------------------------------------------------
!
      END SUBROUTINE CLEANLIST_ll
!
!!    ###############################
      SUBROUTINE CLEANLIST1D_ll(TPLIST)
!!    ###############################
  IMPLICIT NONE
!
  TYPE(LIST1D_ll), POINTER :: TPLIST ! List of fields
!
  TYPE(LIST1D_ll), POINTER :: TZTEMP
!
!------------------------------------------------------------------------------
!
!*       1.    Deallocate one by one the elements of TPLIST
!              --------------------------------------------
!
  IF (ASSOCIATED(TPLIST)) THEN
    DO WHILE(ASSOCIATED(TPLIST))
      TZTEMP => TPLIST
      TPLIST => TPLIST%NEXT
      DEALLOCATE(TZTEMP)
    END DO
    NULLIFY(TPLIST)
  END IF
!
!------------------------------------------------------------------------------
!
      END SUBROUTINE CLEANLIST1D_ll

END MODULE MODE_ARGSLIST_ll
