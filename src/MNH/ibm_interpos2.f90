!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!      #########################
MODULE MODI_IBM_INTERPOS2
  !    #########################
  !
  INTERFACE
     !
     SUBROUTINE IBM_INTERPOS2(PXREF,PYREF,PZREF,PXNEW,PYNEW,PZNEW)
       !
       REAL, DIMENSION(:,:,:) , INTENT(IN)    :: PXREF,PYREF,PZREF
       REAL, DIMENSION(:,:,:) , INTENT(INOUT) :: PXNEW,PYNEW,PZNEW
       !
     END SUBROUTINE IBM_INTERPOS2
     !
  END INTERFACE
  !
END MODULE MODI_IBM_INTERPOS2
!
!       #############################################################
SUBROUTINE IBM_INTERPOS2(PXREF,PYREF,PZREF,PXNEW,PYNEW,PZNEW)
  !       #############################################################
  !
  !****     *IBM_INTERPOS2*  - routine to search location of cell corners 
  !
  !      PURPOSE
  !      -------
  !         The purpose of this routine is to compute cell corners for (U,V,W,P)
  !
  !      METHOD
  !      ------
  !
  !      EXTERNAL
  !      --------
  !        NONE
  !
  !      IMPLICIT ARGUMENTS
  !      ------------------
  !
  !      REFERENCE
  !      ---------
  !
  !      AUTHOR
  !      ------
  !        Franck Auguste       * CERFACS(AE) *
  !
  !      MODIFICATIONS
  !      -------------
  !        Original          01/01/2019
  !
  !------------------------------------------------------------------------------
  !       
  !**** 0. DECLARATIONS
  !     ---------------
  !
  ! module
  USE MODE_POS
  USE MODE_ll
  USE MODE_IO
  !
  ! declaration
  USE MODD_IBM_PARAM_n
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll
  !
  ! interface
  !
  IMPLICIT NONE
  !
  !-----------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  REAL, DIMENSION(:,:,:) , INTENT(IN)    :: PXREF,PYREF,PZREF ! node location
  REAL, DIMENSION(:,:,:) , INTENT(INOUT) :: PXNEW,PYNEW,PZNEW ! cell location
  !
  !-----------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  INTEGER                :: IIU,IJU,IKU  ! domain size
  INTEGER                :: JI,JJ,JK     ! loop index
  TYPE(LIST_ll), POINTER :: TZFIELDS_ll  ! list of fields to exchange
  INTEGER                :: IINFO_ll
  !
  !-----------------------------------------------------------------------------
  !
  !       0.3  Allocation
  !
  NULLIFY(TZFIELDS_ll)
  IIU = SIZE(PXREF,1)
  IJU = SIZE(PYREF,2)
  IKU = SIZE(PZREF,3)
  !
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PXREF,'IBM_INTERPOS2::PXREF')
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PYREF,'IBM_INTERPOS2::PYREF')
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PZREF,'IBM_INTERPOS2::PZREF')
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
  !
  !------------------------------------------------------------------------------
  !
  ! along X
  PXNEW(2:IIU,2:IJU,2:IKU) = PXREF(2:IIU-0,2:IJU-0,2:IKU-0) + PXREF(1:IIU-1,2:IJU-0,2:IKU-0) + &
       PXREF(1:IIU-1,1:IJU-1,2:IKU-0) + PXREF(2:IIU-0,1:IJU-1,2:IKU-0) + &
       PXREF(2:IIU-0,2:IJU-0,1:IKU-1) + PXREF(1:IIU-1,2:IJU-0,1:IKU-1) + &
       PXREF(1:IIU-1,1:IJU-1,1:IKU-1) + PXREF(2:IIU-0,1:IJU-1,1:IKU-1)
  PXNEW(2:IIU,2:IJU,2:IKU) = PXNEW(2:IIU,2:IJU,2:IKU) * (1./8.)
  !
  IF (LWEST_ll()) THEN
     PXNEW(1,:,:) = (2.*PXNEW(2,:,:)-PXNEW(3,:,:))
     PXNEW(:,1,:) = (2.*PXNEW(:,2,:)-PXNEW(:,3,:))
     PXNEW(:,:,1) = (2.*PXNEW(:,:,2)-PXNEW(:,:,3))
     PXNEW(1,1,:) = (2.*PXNEW(2,2,:)-PXNEW(3,3,:))
     PXNEW(:,1,1) = (2.*PXNEW(:,2,2)-PXNEW(:,3,3))
     PXNEW(1,:,1) = (2.*PXNEW(2,:,2)-PXNEW(3,:,3))
     PXNEW(1,1,1) = (2.*PXNEW(2,2,2)-PXNEW(3,3,3))
  ENDIF
  !
  IF (LEAST_ll()) THEN
     PXNEW(IIU+1,    :,    :) = (2.*PXNEW(IIU-0,    :,    :)-PXNEW(IIU-1,    :,    :))
     PXNEW(:    ,IJU+1,    :) = (2.*PXNEW(:    ,IJU-0,    :)-PXNEW(:    ,IJU-1,    :))
     PXNEW(:    ,    :,IKU+1) = (2.*PXNEW(:    ,    :,IKU-0)-PXNEW(:    ,    :,IKU-1))
     PXNEW(IIU+1,IJU+1,    :) = (2.*PXNEW(IIU-0,IJU-0,    :)-PXNEW(IIU-1,IJU-1,    :))
     PXNEW(:    ,IJU+1,IKU+1) = (2.*PXNEW(:    ,IJU-0,IKU-0)-PXNEW(:    ,IJU-1,IKU-1))
     PXNEW(IIU+1,    :,IKU+1) = (2.*PXNEW(IIU-0,    :,IKU-0)-PXNEW(IIU-1,    :,IKU-1))
     PXNEW(IIU+1,IJU+1,IKU+1) = (2.*PXNEW(IIU-0,IJU-0,IKU-0)-PXNEW(IIU-1,IJU-1,IKU-1))
  ENDIF
  !
  ! along Y
  PYNEW(2:IIU-0,2:IJU-0,2:IKU-0) = PYREF(2:IIU-0,2:IJU-0,2:IKU-0) + PYREF(1:IIU-1,2:IJU-0,2:IKU-0) + &
       PYREF(1:IIU-1,1:IJU-1,2:IKU-0) + PYREF(2:IIU-0,1:IJU-1,2:IKU-0) + &
       PYREF(2:IIU-0,2:IJU-0,1:IKU-1) + PYREF(1:IIU-1,2:IJU-0,1:IKU-1) + &
       PYREF(1:IIU-1,1:IJU-1,1:IKU-1) + PYREF(2:IIU-0,1:IJU-1,1:IKU-1)
  PYNEW(2:IIU-0,2:IJU-0,2:IKU-0) = PYNEW(2:IIU,2:IJU,2:IKU) * (1./8.)
  !
  IF (LSOUTH_ll()) THEN
     PYNEW(1,:,:) = (2.*PYNEW(2,:,:)-PYNEW(3,:,:))
     PYNEW(:,1,:) = (2.*PYNEW(:,2,:)-PYNEW(:,3,:))
     PYNEW(:,:,1) = (2.*PYNEW(:,:,2)-PYNEW(:,:,3))
     PYNEW(1,1,:) = (2.*PYNEW(2,2,:)-PYNEW(3,3,:))
     PYNEW(:,1,1) = (2.*PYNEW(:,2,2)-PYNEW(:,3,3))
     PYNEW(1,:,1) = (2.*PYNEW(2,:,2)-PYNEW(3,:,3))
     PYNEW(1,1,1) = (2.*PYNEW(2,2,2)-PYNEW(3,3,3))
  ENDIF
  IF (LNORTH_ll()) THEN
     PYNEW(IIU+1,    :,    :) = (2.*PYNEW(IIU-0,    :,    :)-PYNEW(IIU-1,    :,    :))
     PYNEW(:    ,IJU+1,    :) = (2.*PYNEW(:    ,IJU-0,    :)-PYNEW(:    ,IJU-1,    :))
     PYNEW(:    ,    :,IKU+1) = (2.*PYNEW(:    ,    :,IKU-0)-PYNEW(:    ,    :,IKU-1))
     PYNEW(IIU+1,IJU+1,    :) = (2.*PYNEW(IIU-0,IJU-0,    :)-PYNEW(IIU-1,IJU-1,    :))
     PYNEW(:    ,IJU+1,IKU+1) = (2.*PYNEW(:    ,IJU-0,IKU-0)-PYNEW(:    ,IJU-1,IKU-1))
     PYNEW(IIU+1,    :,IKU+1) = (2.*PYNEW(IIU-0,    :,IKU-0)-PYNEW(IIU-1,    :,IKU-1))
     PYNEW(IIU+1,IJU+1,IKU+1) = (2.*PYNEW(IIU-0,IJU-0,IKU-0)-PYNEW(IIU-1,IJU-1,IKU-1))
  ENDIF
  !
  ! along Z 
  PZNEW(2:IIU-0,2:IJU-0,2:IKU-0) = PZREF(2:IIU-0,2:IJU-0,2:IKU-0) + PZREF(1:IIU-1,2:IJU-0,2:IKU-0) + &
       PZREF(1:IIU-1,1:IJU-1,2:IKU-0) + PZREF(2:IIU-0,1:IJU-1,2:IKU-0) + &
       PZREF(2:IIU-0,2:IJU-0,1:IKU-1) + PZREF(1:IIU-1,2:IJU-0,1:IKU-1) + &
       PZREF(1:IIU-1,1:IJU-1,1:IKU-1) + PZREF(2:IIU-0,1:IJU-1,1:IKU-1)
  PZNEW(2:IIU-0,2:IJU-0,2:IKU-0) = PZNEW(2:IIU-0,2:IJU-0,2:IKU-0) * (1./8.)
  PZNEW(1,:,:) = (2.*PZNEW(2,:,:)-PZNEW(3,:,:))
  PZNEW(:,1,:) = (2.*PZNEW(:,2,:)-PZNEW(:,3,:))
  PZNEW(:,:,1) = (2.*PZNEW(:,:,2)-PZNEW(:,:,3))
  PZNEW(1,1,:) = (2.*PZNEW(2,2,:)-PZNEW(3,3,:))
  PZNEW(:,1,1) = (2.*PZNEW(:,2,2)-PZNEW(:,3,3))
  PZNEW(1,:,1) = (2.*PZNEW(2,:,2)-PZNEW(3,:,3))
  PZNEW(1,1,1) = (2.*PZNEW(2,2,2)-PZNEW(3,3,3))
  PZNEW(IIU+1,    :,    :) = (2.*PZNEW(IIU-0,    :,    :)-PZNEW(IIU-1,    :,    :))
  PZNEW(:    ,IJU+1,    :) = (2.*PZNEW(:    ,IJU-0,    :)-PZNEW(:    ,IJU-1,    :))
  PZNEW(:    ,    :,IKU+1) = (2.*PZNEW(:    ,    :,IKU-0)-PZNEW(:    ,    :,IKU-1))
  PZNEW(IIU+1,IJU+1,    :) = (2.*PZNEW(IIU-0,IJU-0,    :)-PZNEW(IIU-1,IJU-1,    :))
  PZNEW(:    ,IJU+1,IKU+1) = (2.*PZNEW(:    ,IJU-0,IKU-0)-PZNEW(:    ,IJU-1,IKU-1))
  PZNEW(IIU+1,    :,IKU+1) = (2.*PZNEW(IIU-0,    :,IKU-0)-PZNEW(IIU-1,    :,IKU-1))
  PZNEW(IIU+1,IJU+1,IKU+1) = (2.*PZNEW(IIU-0,IJU-0,IKU-0)-PZNEW(IIU-1,IJU-1,IKU-1))
  !
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  !
  RETURN
  !
END SUBROUTINE IBM_INTERPOS2
