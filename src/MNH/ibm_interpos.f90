!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!      ########################
MODULE MODI_IBM_INTERPOS
  !    ########################
  !
  INTERFACE
     !
     SUBROUTINE IBM_INTERPOS(PXREF,PYREF,PZREF,HPOS)
       !
       CHARACTER(LEN=1)       , INTENT(IN)    :: HPOS
       REAL, DIMENSION(:,:,:) , INTENT(INOUT) :: PXREF,PYREF,PZREF
       !
     END SUBROUTINE IBM_INTERPOS
     !
  END INTERFACE
  !
END MODULE MODI_IBM_INTERPOS
!
!       ###############################################
SUBROUTINE IBM_INTERPOS(PXREF,PYREF,PZREF,HPOS)
  !       ###############################################
  !
  !****     *IBM_INTERPOS*  - routine to search location of each type of nodes 
  !
  !      PURPOSE
  !      -------
  !         The purpose of this routine is to compute (X,Y,Z) for (U,V,W,P)
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
  USE MODD_GRID_n, ONLY: XXHAT,XYHAT,XZZ
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll
  !
  ! interface
  USE MODI_SHUMAN
  !
  IMPLICIT NONE
  !
  !-----------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  CHARACTER(LEN=1)       , INTENT(IN)    :: HPOS              ! location UVWP
  REAL, DIMENSION(:,:,:) , INTENT(INOUT) :: PXREF,PYREF,PZREF ! variable
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
  IIU = SIZE(PXREF,1)
  IJU = SIZE(PYREF,2)
  IKU = SIZE(PZREF,3)
  !
  !-----------------------------------------------------------------------------
  !       
  IF (HPOS=='P') THEN
     PXREF = MXF(spread(spread(XXHAT(1:IIU),2,IJU),3,IKU))
     PYREF = MYF(spread(spread(XYHAT(1:IJU),1,IIU),3,IKU))
     PZREF = MZF(XZZ)
  ENDIF
  IF (HPOS=='U') THEN
     PXREF = (spread(spread(XXHAT(1:IIU),2,IJU),3,IKU))
     PYREF = MXM(MYF(spread(spread(XYHAT(1:IJU),1,IIU),3,IKU)))
     PZREF = MXM(MZF(XZZ))
  ENDIF
  IF (HPOS=='V') THEN
     PXREF = MYM(MXF(spread(spread(XXHAT(1:IIU),2,IJU),3,IKU)))
     PYREF = (spread(spread(XYHAT(1:IJU),1,IIU),3,IKU))
     PZREF = MYM(MZF(XZZ))
  ENDIF
  IF (HPOS=='W') THEN
     PXREF = MZM(MXF((spread(spread(XXHAT(1:IIU),2,IJU),3,IKU))))
     PYREF = MZM(MYF((spread(spread(XYHAT(1:IJU),1,IIU),3,IKU))))
     PZREF = XZZ
  ENDIF
  IF (HPOS=='A') THEN
     PXREF = (spread(spread(XXHAT(1:IIU),2,IJU),3,IKU))
     PYREF = (spread(spread(XYHAT(1:IJU),1,IIU),3,IKU))
     PZREF = MZF(XZZ)
  ENDIF
  IF (HPOS=='B') THEN
     PXREF = (spread(spread(XXHAT(1:IIU),2,IJU),3,IKU))
     PYREF = MYF(spread(spread(XYHAT(1:IJU),1,IIU),3,IKU))
     PZREF = XZZ
  ENDIF
  IF (HPOS=='C') THEN
     PXREF = MXF(spread(spread(XXHAT(1:IIU),2,IJU),3,IKU))
     PYREF = (spread(spread(XYHAT(1:IJU),1,IIU),3,IKU))
     PZREF = XZZ
  ENDIF
  IF (HPOS=='X') THEN
     PXREF = (spread(spread(XXHAT(1:IIU),2,IJU),3,IKU))
     PYREF = (spread(spread(XYHAT(1:IJU),1,IIU),3,IKU))    
     PZREF = XZZ
  ENDIF
  !
  IF (LWEST_ll( )) THEN
     PXREF(1,  :,  :) = (2.*PXREF(2,    :,    :)-PXREF(3,    :,    :))
     PYREF(1,  :,  :) = (2.*PYREF(2,    :,    :)-PYREF(3,    :,    :))
     PZREF(1,  :,  :) = (2.*PZREF(2,    :,    :)-PZREF(3,    :,    :))
  ENDIF
  IF (LEAST_ll( )) THEN
     PXREF(IIU,  :,  :) = (2.*PXREF(IIU-1,    :,    :)-PXREF(IIU-2,    :,    :))
     PYREF(IIU,  :,  :) = (2.*PYREF(IIU-1,    :,    :)-PYREF(IIU-2,    :,    :))
     PZREF(IIU,  :,  :) = (2.*PZREF(IIU-1,    :,    :)-PZREF(IIU-2,    :,    :))
  ENDIF
  IF (LSOUTH_ll()) THEN
     PXREF(:  ,1,  :) = (2.*PXREF(:    ,2,    :)-PXREF(:    ,3,    :))
     PYREF(:  ,1,  :) = (2.*PYREF(:    ,2,    :)-PYREF(:    ,3,    :))
     PZREF(:  ,1,  :) = (2.*PZREF(:    ,2,    :)-PZREF(:    ,3,    :))
  ENDIF
  IF (LNORTH_ll()) THEN
     PXREF(:  ,IJU,  :) = (2.*PXREF(:    ,IJU-1,    :)-PXREF(:    ,IJU-2,    :))
     PYREF(:  ,IJU,  :) = (2.*PYREF(:    ,IJU-1,    :)-PYREF(:    ,IJU-2,    :))
     PZREF(:  ,IJU,  :) = (2.*PZREF(:    ,IJU-1,    :)-PZREF(:    ,IJU-2,    :))
  ENDIF
  !
  PXREF(:  ,  :,  1) = (2.*PXREF(:    ,    :,    2)-PXREF(:    ,    :,    3))
  PXREF(:  ,  :,IKU) = (2.*PXREF(:    ,    :,IKU-1)-PXREF(:    ,    :,IKU-2))
  PYREF(:  ,  :,  1) = (2.*PYREF(:    ,    :,    2)-PYREF(:    ,    :,    3))
  PYREF(:  ,  :,IKU) = (2.*PYREF(:    ,    :,IKU-1)-PYREF(:    ,    :,IKU-2))
  PZREF(:  ,  :,  1) = (2.*PZREF(:    ,    :,    2)-PZREF(:    ,    :,    3))
  PZREF(:  ,  :,IKU) = (2.*PZREF(:    ,    :,IKU-1)-PZREF(:    ,    :,IKU-2))
  !
  NULLIFY(TZFIELDS_ll)
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PXREF,'IBM_INTERPOS::PXREF')
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PYREF,'IBM_INTERPOS::PYREF')
  CALL ADD3DFIELD_ll(TZFIELDS_ll,PZREF,'IBM_INTERPOS::PZREF')
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
  !
  !
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  !
  RETURN
  !
END SUBROUTINE IBM_INTERPOS
