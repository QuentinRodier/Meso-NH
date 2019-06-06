!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ######spl
     MODULE MODI_CONDSAMP
!    ################## 
!
INTERFACE
!
      SUBROUTINE CONDSAMP (PTSTEP, PSFSV, KLUOUT, KVERB)
IMPLICIT NONE
REAL,  INTENT(IN)            ::  PTSTEP  ! Time step
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PSFSV ! surface flux of scalars
INTEGER, INTENT(IN)          :: KLUOUT     ! unit for output listing count
INTEGER, INTENT(IN)          :: KVERB      ! verbosity level
!
END SUBROUTINE CONDSAMP
!
END INTERFACE
!
END MODULE MODI_CONDSAMP
!     ######spl
      SUBROUTINE CONDSAMP (PTSTEP, PSFSV, KLUOUT, KVERB)
!     ############################################################
!
!
!
!!****  *PASPOL* -
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to release tracers for conditional
!!       samplings according to Couvreux et al. (2010)
!
!!**  METHOD
!!    ------
!!    
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      F.Couvreux, C.Lac         * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      F.Brient                  * Tracer emission from the top
!!                                   of the boundary-layer * 05/2019
!!
!! --------------------------------------------------------------------------
!       
!!    EXTERNAL
!!    --------
!!
USE MODD_PARAMETERS , ONLY : JPVEXT
USE MODD_NSV        , ONLY : NSV_CSBEG, NSV_CSEND, NSV_CS
USE MODD_CONF_n     , ONLY : LUSERC
USE MODD_FIELD_n    , ONLY : XSVT, XRT, XRSVS, XTHT
USE MODD_GRID_n     , ONLY : XZHAT
USE MODD_REF_n      , ONLY : XRHODJ
USE MODD_DYN        , ONLY : XTSTEP_MODEL1
USE MODD_CONDSAMP
USE MODE_ll
USE MODD_CST
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL,  INTENT(IN)            ::  PTSTEP  ! Time step
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PSFSV ! surface flux of scalars
INTEGER, INTENT(IN)          :: KLUOUT     ! unit for output listing count
INTEGER, INTENT(IN)          :: KVERB      ! verbosity level
!
!*      0.2    declarations of local variables
!

INTEGER :: IIB,IIE,IJB,IJE, IKB, IKE
INTEGER :: IIU, IJU, IKU                      ! dimensional indexes
INTEGER :: JK,JSV,IBOT,ITOP,IEMIS ! Loop indice
INTEGER :: INTOP,INBOT
REAL    :: ZDT,ZMAXZDT,ZTHVMEAN,ZOFFSET !       For tracer emission
INTEGER :: JRR
INTEGER :: IINFO_ll       ! return code of parallel routine
REAL, DIMENSION(SIZE(XRT,1),SIZE(XRT,2),SIZE(XRT,3)) :: ZRT
REAL, DIMENSION(SIZE(XSVT,1),SIZE(XSVT,2),SIZE(XSVT,3),SIZE(XSVT,4)) :: ZSVT
REAL, DIMENSION(SIZE(XTHT,1),SIZE(XTHT,2),SIZE(XTHT,3)) :: ZSUM,ZTHV
REAL, DIMENSION(:,:,:), ALLOCATABLE  :: ZLVOCPEXNM,ZLSOCPEXNM ! Lv/Cp/EXNREF and Ls/Cp/EXNREF at t-1

!
!--------------------------------------------------------------------------------------
!
!
!*	0. Initialisation
!
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
IKU = SIZE(XRT,3)
IKB = 1 + JPVEXT
IKE = IKU - JPVEXT
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
ZSVT(:,:,:,:) = XSVT(:,:,:,:)
!
!
!
!*	1.  INITIALIZATION OF CONDITIONAL SAMPLING TRACERS
!	    ----------------------------------------------
! on veut initialiser le 1er traceur a la surface tout le temps
! le 2E si cloud 100m en dessous de cloud base
! le 3eme si cloud 100m au dessus de cloud top
!
IBOT=0
ITOP=0
ZMAXZDT=0.
INTOP=0
INBOT=0

IF ( NSV_CS >= 2 ) THEN
 IF ( NFINDTOP==0 .AND. LUSERC .AND.  MAX_ll(XRT(:,:,:,2),IINFO_ll) > 1.E-6 )  THEN
  ! calcul de la base et du sommet des nuages
  ! on ne considere que l'eau liquide car que pour nuages de couche limite
  DO JK=1,IKE
   ZRT(:,:,:) = SPREAD(XRT(:,:,JK,2),3,IKU)
   IF ((MAX_ll(ZRT(:,:,:),IINFO_ll) > 1.E-6).AND.(IBOT == 0)) IBOT=JK
   IF ( MAX_ll(ZRT(:,:,:),IINFO_ll) > 1.E-6) ITOP=JK
  END DO
  IF (KVERB >= 10) THEN
   WRITE(KLUOUT,'(A)') ' '
   WRITE(KLUOUT,'(A,F7.1)') 'Base nuage  : ',XZHAT(IBOT)
   WRITE(KLUOUT,'(A,F7.1)') 'Sommet nuage: ',XZHAT(ITOP)
   WRITE(KLUOUT,'(A,I3.1)') 'JK Base   : ',IBOT
   WRITE(KLUOUT,'(A,I3.1)') 'JK Sommet : ',ITOP
  END IF
  !
 ELSEIF ( NFINDTOP==2 ) THEN
  !  Find the first layer z from the surface where
  !  THV is larger is the average below z + ZTHV (0.25K by default)
  ZTHVMEAN = 0
  ZSUM(:,:,:) = 0.
  ZOFFSET  = XTHVP 
  JRR     = SIZE(XRT,4) 
  DO JK=1,JRR
   ZSUM(:,:,:) = ZSUM(:,:,:)+XRT(:,:,:,JRR)
  ENDDO
  DO JK=1,IKE
    ZTHV(:,:,JK)=XTHT(:,:,JK) * ( 1. + XRV/XRD*XRT(:,:,JK,1) )  &
                           / ( 1. + ZSUM(:,:,JK) )
  END DO
  ZTHVMEAN = SUM(ZTHV(:,:,2))/SIZE(ZTHV(:,:,2)) 
  DO JK=3,IKE
     IF (ITOP == 0) THEN
      ZDT     =  SUM(ZTHV(:,:,JK))/SIZE(ZTHV(:,:,JK))   
      ZTHVMEAN =   (1.0/XZHAT(JK+1))* & 
                  (XZHAT(JK)*ZTHVMEAN + (XZHAT(JK+1)-XZHAT(JK))*ZDT)
      IF (ZDT > ZTHVMEAN + ZOFFSET ) THEN
        ITOP=JK
      ENDIF
    ENDIF
  END DO
  !
 ELSE
  ! BY DEFAULT IF NO CLOUDS
  ! or only when NFINDTOP==1 
  ! Identification of the layer where lies the stronger gradient
  !  of potential temperature
  !  (need to replace by liquid water potential temperature)
  ! No clouds is defined as MAX_ll(XRT(:,:,:,2),IINFO_ll) < 1.E-6
  !
  DO JK=1,IKE
    ! ZDT need to become positive at least once
    ZDT = SUM((XTHT(:,:,JK+1)-XTHT(:,:,JK)))/SIZE(XTHT(:,:,JK))
    ZDT = ZDT/(XZHAT(JK+1)-XZHAT(JK))
    IF ( ZDT > ZMAXZDT ) THEN
      ITOP=JK
      ZMAXZDT=ZDT
    END IF
  END DO
  IF (KVERB >= 10) THEN
   WRITE(KLUOUT,'(A)') ' '
   WRITE(KLUOUT,'(A,F7.1)') 'Sommet BL: ',XZHAT(ITOP)
   WRITE(KLUOUT,'(A,I3.1)') 'JK Sommet BL : ',ITOP
  END IF
 END IF
END IF

DO JSV=NSV_CSBEG, NSV_CSEND
 !
 IF (JSV== NSV_CSBEG ) THEN 
  ! emission en surface
  PSFSV(IIB:IIE,IJB:IJE,JSV) = 1.
 ENDIF

 IF ((JSV == NSV_CSBEG + 1 ).AND.(IBOT > 2)) THEN
    ! emission XHEIGHT_BASE(m) below the base on XDEPTH_BASE(m)
    !
    DO JK=1,IKE
     IF ((XZHAT(JK) > XZHAT(IBOT) - XHEIGHT_BASE - XDEPTH_BASE/2. ).AND. &
         (XZHAT(JK) < XZHAT(IBOT) - XHEIGHT_BASE + XDEPTH_BASE/2. )) THEN
         INBOT = 1
         ZSVT(IIB:IIE,IJB:IJE,JK,JSV) =  &
           XSVT(IIB:IIE,IJB:IJE,JK,JSV)+1.  
     END IF
    END DO
    IF (INBOT == 0) THEN
      IEMIS = IBOT
      IF (LTPLUS) THEN
        IEMIS = IBOT - 1
      END IF
      ZSVT(IIB:IIE,IJB:IJE,IEMIS,JSV) = &
        XSVT(IIB:IIE,IJB:IJE,IEMIS,JSV)+1.
    END IF
 END IF
!    
 IF ((JSV == NSV_CSBEG + 2 ).AND.(ITOP > 2)) THEN
   ! emission XHEIGHT_TOP(m) above the top on XDEPTH_TOP(m)
   !
   DO JK=1,IKE
    IF ((XZHAT(JK) > XZHAT(ITOP) + XHEIGHT_TOP - XDEPTH_TOP/2. ).AND. &
        (XZHAT(JK) < XZHAT(ITOP) + XHEIGHT_TOP + XDEPTH_TOP/2. )) THEN
        INTOP = 1
        ZSVT(IIB:IIE,IJB:IJE,JK,JSV) = &
          XSVT(IIB:IIE,IJB:IJE,JK,JSV)+1. 
    END IF
   END DO
    IF (INTOP == 0) THEN
      IEMIS = ITOP
      IF (LTPLUS .AND.(ITOP < IKE)) THEN
        IEMIS = ITOP + 1
      END IF
      ZSVT(IIB:IIE,IJB:IJE,IEMIS,JSV) = &
        XSVT(IIB:IIE,IJB:IJE,IEMIS,JSV)+1.
    END IF
 END IF
!
END DO            
!
! correction d'eventuelle concentration négative
WHERE (ZSVT(:,:,:,NSV_CSBEG:NSV_CSEND) <0.0) &
       ZSVT(:,:,:,NSV_CSBEG:NSV_CSEND)=0.0
!
!
!  2: Radioactive decrease            
!
DO JSV=NSV_CSBEG, NSV_CSEND
   ZSVT(:,:,:,JSV) = ZSVT(:,:,:,JSV) *         &
           EXP(-1.*XTSTEP_MODEL1/XRADIO(JSV-NSV_CSBEG+1))
   XRSVS(:,:,:,JSV) = XRSVS(:,:,:,JSV) + &
        XRHODJ(:,:,:)*(ZSVT(:,:,:,JSV)-XSVT(:,:,:,JSV))/PTSTEP
END DO
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CONDSAMP
