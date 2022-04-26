!MNH_LIC Copyright 2021-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!       #####################
        MODULE MODI_RECYCLING
!       #####################
!
INTERFACE
!
SUBROUTINE RECYCLING (PFLUCTUNW,PFLUCTVNN,PFLUCTUTN,PFLUCTVTW,PFLUCTWTW,PFLUCTWTN, &
                      PFLUCTUNE,PFLUCTVNS,PFLUCTUTS,PFLUCTVTE,PFLUCTWTE,PFLUCTWTS, &
                      PTCOUNT)

  INTEGER                 ,INTENT(IN)    :: PTCOUNT
  REAL, DIMENSION(:,:)    ,INTENT(INOUT) :: PFLUCTUNW,PFLUCTVTW,PFLUCTVNN,PFLUCTUTN,PFLUCTWTW,PFLUCTWTN
  REAL, DIMENSION(:,:)    ,INTENT(INOUT) :: PFLUCTUNE,PFLUCTVTE,PFLUCTVNS,PFLUCTUTS,PFLUCTWTE,PFLUCTWTS

END SUBROUTINE RECYCLING
!
END INTERFACE
!
END MODULE MODI_RECYCLING
!
!
!
!       ####################################
        SUBROUTINE RECYCLING (PFLUCTUNW,PFLUCTVNN,PFLUCTUTN,PFLUCTVTW,PFLUCTWTW,PFLUCTWTN, &
                              PFLUCTUNE,PFLUCTVNS,PFLUCTUTS,PFLUCTVTE,PFLUCTWTE,PFLUCTWTS, &
                              PTCOUNT)
!       ####################################
!
!!****     *RECYCLING*  - routine initializing and building the velocity fluctuations fields
!!
!!      PURPOSE
!!      -------
!         The purpose of this routine is to initialize and calculate
!         turbulent fluctuations in order to be applied at the domain 
!         boundaries. 
!
!!      METHOD
!!      ------
!!!
!!      EXTERNAL
!!      --------
!!        NONE
!!
!!      IMPLICIT ARGUMENTS
!!      ------------------
!!
!!      REFERENCE
!!      ---------
!!
!!      AUTHOR
!!      ------
!!        Tim Nagel        * Meteo-France*
!!
!!      MODIFICATIONS
!!      -------------
!!        Original          01/02/2021
!!
!------------------------------------------------------------------------------
!       
!**** 0. DECLARATIONS
!     ---------------
!
USE MODD_CONF,           ONLY: CCONF
USE MODD_FIELD_n,        ONLY: XTHT, XUT, XVT, XWT
USE MODD_GRID_n,         ONLY: XXHAT, XYHAT
USE MODD_METRICS_n,      ONLY: XDZZ
USE MODD_PARAMETERS,     ONLY: JPHEXT
USE MODD_RECYCL_PARAM_n

USE MODE_MSG

USE MODI_RECYCL_FLUC

IMPLICIT NONE
!
!------------------------------------------------------------------------------
!
!       0.1  declarations of arguments
INTEGER                  ,INTENT(IN)    :: PTCOUNT  ! temporal loop index of model KMODEL
REAL, DIMENSION(:,:)     ,INTENT(INOUT) :: PFLUCTUNW,PFLUCTVTW,PFLUCTVNN,PFLUCTUTN,PFLUCTWTW,PFLUCTWTN
REAL, DIMENSION(:,:)     ,INTENT(INOUT) :: PFLUCTUNE,PFLUCTVTE,PFLUCTVNS,PFLUCTUTS,PFLUCTWTE,PFLUCTWTS
!
!------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
INTEGER :: PMINW,PMINE,PMINN,PMINS
INTEGER :: JIDIST,JJDIST
REAL    :: Z_DELTX,Z_DELTY
!
!------------------------------------------------------------------------------
!
!       0.3  allocation
PMINW=0
PMINN=0
PMINS=0
PMINE=0

Z_DELTX = XXHAT(2)-XXHAT(1)
Z_DELTY = XYHAT(2)-XYHAT(1)

!------------------------------------------------------------------------------
!       
!**** 1. Recycling distance calculation 
!     ---------------
!
!Moving averaged parameter verification
IF (PTCOUNT==1 .AND. INT(XTMOY)/INT(XTMOYCOUNT) /= INT(XNUMBELT)) THEN
    CMNHMSG(1) = 'XTMOY/XTMOYCOUNT must be equal to XNUMBELT'
    CMNHMSG(2) = 'Please change the above parameters accordingly in NAM_RECYCL_PARAMn'
    CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'RECYCLING' )
END IF

IF(CCONF == "RESTA" .AND. PTCOUNT == 1 ) THEN
  NR_COUNT = NR_COUNT
ELSE
  NR_COUNT = NR_COUNT +1
  !IF (IP==1) WRITE(*,*)'RCOUNT: ', NR_COUNT
ENDIF

  IF (LRECYCLW) THEN
    JIDIST = INT(XDRECYCLW*cos(XARECYCLW)/Z_DELTX)
    JJDIST = INT(XDRECYCLW*sin(XARECYCLW)/Z_DELTY)
    PMINW = 1+JPHEXT+JIDIST
  ENDIF
  IF (LRECYCLN) THEN
    JIDIST = INT(XDRECYCLN*cos(XARECYCLN)/Z_DELTX)
    JJDIST = INT(XDRECYCLN*sin(XARECYCLN)/Z_DELTY)
    PMINN = 1+JPHEXT+JJDIST
  ENDIF
  IF (LRECYCLE) THEN
    JIDIST = INT(XDRECYCLE*cos(XARECYCLE)/Z_DELTX)
    JJDIST = INT(XDRECYCLE*sin(XARECYCLE)/Z_DELTY)
    PMINE = 1+JPHEXT+JIDIST
  ENDIF
  IF (LRECYCLS) THEN
    JIDIST = INT(XDRECYCLS*cos(XARECYCLS)/Z_DELTX)
    JJDIST = INT(XDRECYCLS*sin(XARECYCLS)/Z_DELTY)
    PMINS = 1+JPHEXT+JJDIST!
  ENDIF  

  CALL RECYCL_FLUC (XUT,XVT,XWT,XTHT,XDZZ,NR_COUNT,PTCOUNT,PMINW,PMINN,PMINE,PMINS,&
                      PFLUCTUNW,PFLUCTVNN,PFLUCTUTN,PFLUCTVTW,PFLUCTWTW,PFLUCTWTN, &
                      PFLUCTUNE,PFLUCTVNS,PFLUCTUTS,PFLUCTVTE,PFLUCTWTE,PFLUCTWTS  )

RETURN

END SUBROUTINE RECYCLING

