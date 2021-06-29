!MNH_LIC Copyright 2021-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!#####################
MODULE MODI_ADDFLUCTUATIONS
!#####################
!
INTERFACE
!
      SUBROUTINE ADDFLUCTUATIONS (                   &
            HLBCX,HLBCY,                             &
            PUT,PVT,PWT,PTHT,PTKET,PRT,PSVT,PSRCT,   &
            PFLUCTUTN,PFLUCTVTW,PFLUCTUTS,PFLUCTVTE, &
            PFLUCTWTW,PFLUCTWTN,PFLUCTWTS,PFLUCTWTE  )

CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX,HLBCY   ! X and Y-direc. LBC type
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PFLUCTUTN,PFLUCTVTW,PFLUCTUTS,PFLUCTVTE ! tengential velocity fluctuations
REAL, DIMENSION(:,:),   INTENT(IN) :: PFLUCTWTN,PFLUCTWTW,PFLUCTWTS,PFLUCTWTE ! vertical tengential velocity fluctuations
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PUT,PVT,PWT,PTHT,PTKET,PSRCT
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRT,PSVT
                                                      ! Variables at t
!
END SUBROUTINE ADDFLUCTUATIONS
!
END INTERFACE
!
END MODULE MODI_ADDFLUCTUATIONS
!
!
!     ####################################################################
SUBROUTINE ADDFLUCTUATIONS (                  &
     HLBCX,HLBCY,                             &
     PUT,PVT,PWT,PTHT,PTKET,PRT,PSVT,PSRCT,   &
     PFLUCTUTN,PFLUCTVTW,PFLUCTUTS,PFLUCTVTE, &
     PFLUCTWTW,PFLUCTWTN,PFLUCTWTS,PFLUCTWTE  )
!     ####################################################################
!
!!****  *ADDFLUCTUATIONS* - routine adding the velocity fluctuations to the 
!!                          Lateral Boundary Conditions for turbulence
!!                          recycling purpose.
!!
!!    PURPOSE
!!    -------
!!    EXTERNAL 
!!    --------  
!!    GET_INDICE_ll  : get physical sub-domain bounds
!!    LWEAST_ll,LEAST_ll,LNORTH_ll,LSOUTH_ll : position functions
!!
!!    AUTHORS
!!    ------
!!    T.Nagel, V.Masson  * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        01/02/2021 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!         
USE MODD_PARAMETERS
USE MODD_LBC_n,      ONLY : XPOND
USE MODD_RECYCL_PARAM_n
USE MODE_MODELN_HANDLER
USE MODE_ll
!
IMPLICIT NONE
!
!
!*       0.1   declarations of arguments
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX,HLBCY   ! X and Y-direc. LBC type
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PFLUCTUTN,PFLUCTVTW,PFLUCTUTS,PFLUCTVTE ! tengential velocity fluctuations
REAL, DIMENSION(:,:),   INTENT(IN) :: PFLUCTWTN,PFLUCTWTW,PFLUCTWTS,PFLUCTWTE ! vertical tengential velocity fluctuations
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PUT,PVT,PWT,PTHT,PTKET,PSRCT
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRT,PSVT
                                                      ! Variables at t
!
!*       0.2   declarations of local variables
!
INTEGER             :: IIB       ! indice I Beginning in x direction
INTEGER             :: IJB       ! indice J Beginning in y direction
INTEGER             :: IKB       ! indice K Beginning in z direction
INTEGER             :: IIE       ! indice I End       in x direction 
INTEGER             :: IJE       ! indice J End       in y direction 
INTEGER             :: IKE       ! indice K End       in z direction 
INTEGER             :: JI,JJ
!
!-------------------------------------------------------------------------------
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS AND OTHER INDICES:
!              ----------------------------------------------
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB = 1 + JPVEXT
IKE = SIZE(PUT,3) - JPVEXT
!
!*        2.    ADD FLUCTUATIONS THE X DIRECTION (LEFT WEST SIDE):   
!              ------------------------------------------------
IF (LRECYCLW) THEN
  IF (LWEST_ll( )) THEN
    SELECT CASE ( HLBCX(1) )
    CASE ('OPEN')
      IF(SIZE(PUT) /= 0) THEN
         DO JI=JPHEXT,1,-1
            PUT(JI,:,:)=0.
               WHERE ( PUT(IIB,:,:) > 0. )   !INFLOW  condition
                  PVT  (JI,:,:) = PVT  (JI,:,:)+XRCOEFF*PFLUCTVTW
                  PWT  (JI,:,:) = PWT  (JI,:,:)+XRCOEFF*PFLUCTWTW
               ENDWHERE
         ENDDO
      ENDIF
    END SELECT
  ENDIF
ENDIF
!
!*        3.    ADD FLUCTUATIONS THE X DIRECTION (RIGHT EAST SIDE):   
!              ------------------------------------------------
IF (LRECYCLE) THEN
  IF (LEAST_ll( )) THEN
  SELECT CASE ( HLBCX(2) )
  CASE ('OPEN')
    IF(SIZE(PUT) /= 0) THEN
       DO JI=1,JPHEXT
             WHERE ( PUT(IIE+1,:,:) < 0. )   !INFLOW  condition
                PVT  (IIE+JI,:,:) = PVT  (IIE+JI,:,:)+XRCOEFF*PFLUCTVTE
                PWT  (IIE+JI,:,:) = PWT  (IIE+JI,:,:)+XRCOEFF*PFLUCTWTE
             ENDWHERE
        ENDDO
     ENDIF
   END SELECT
  ENDIF
ENDIF
!
!*        4.    ADD FLUCTUATIONS THE Y DIRECTION (BOTTOM SOUTH SIDE):   
!              ------------------------------------------------
IF (LRECYCLS) THEN
  IF (LSOUTH_ll( )) THEN
  SELECT CASE ( HLBCY(1) )
  CASE ('OPEN')
    IF(SIZE(PVT) /= 0) THEN
       DO JJ=JPHEXT,1,-1
          PVT(:,JJ,:)=0.
             WHERE ( PVT(:,IJB,:) > 0. )   !INFLOW  condition
                PUT  (:,JJ,:) = PUT  (:,JJ,:)+XRCOEFF*PFLUCTUTS
                PWT  (:,JJ,:) = PWT  (:,JJ,:)+XRCOEFF*PFLUCTWTS
             ENDWHERE
        ENDDO
     ENDIF
   END SELECT
  ENDIF
ENDIF
!
!*        5.    ADD FLUCTUATIONS THE Y DIRECTION (TOP NORTH SIDE):   
!              ------------------------------------------------
IF (LRECYCLN) THEN
  IF (LNORTH_ll( )) THEN
     SELECT CASE ( HLBCY(2) )
     CASE ('OPEN')
       IF(SIZE(PVT) /= 0) THEN
         DO JJ=1,JPHEXT
               WHERE ( PVT(:,IJE+1,:) < 0. )   !INFLOW  condition
                  PUT  (:,IJE+JJ,:) = PUT  (:,IJE+JJ,:)+XRCOEFF*PFLUCTUTN
                  PWT  (:,IJE+JJ,:) = PWT  (:,IJE+JJ,:)+XRCOEFF*PFLUCTWTN
               ENDWHERE
         ENDDO
       ENDIF
     END SELECT
    ENDIF
ENDIF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ADDFLUCTUATIONS

