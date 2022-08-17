!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/set_mask.f90,v $ $Revision: 1.2.2.1.2.1.18.2 $
! MASDEV4_7 budget 2006/09/08 10:35:15
!-----------------------------------------------------------------
!     ###################
      SUBROUTINE SET_MASK
!     ###################
!
!!****SET_MASK** -routine to define the mask 
!!                           
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to test the occurence or not of the
!     different criteria, used to compute the budgets. It also updates the 
!     number of occurence of the different criteria.
!
!!**  METHOD
!!    ------
!!      According to each criterion associated to one zone, the mask is
!!    set to TRUE at each point where the criterion is confirmed, at each 
!!    time step of the model. Finally, The number of occurence of this criteria is 
!!    increased by 1 and stored in the array XBUSURF. 
!!    Caution : The mask is defined on the inner domain.
!!      
!!
!!    EXTERNAL
!!    --------
!!       NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!       Module MODD_BUDGET
!!         LBU_MASK   : logical array mask defining the zones
!!         NBUTIME    : number of the budget step
!!         XBUSURF    : mask tracer array (surface array) 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of MESO-NH documentation (routine BUDGET)
!!
!!
!!    AUTHOR
!!    ------
!!	J. Nicolau       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/02/95
!!      Modification 10/11/97  (P.Jabouille) : computation made only in the inner domain
!!      Modification 18/06/99  (N.Asencio) : //  , computation are performed on the extended
!!                                           domain but logical array mask is initialized
!!                                           to FALSE outside the physical domain
!!                   02/02/2017 (J.Escobar & JPP ) bug for 1 model only <-> remove unneeded FIELD_MODEL%
!---------------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BUDGET
USE MODE_ll
USE MODD_FIELD_n, ONLY : XWT, XRT
!
USE MODD_PRECIP_n,   ONLY : XINPRR
USE MODD_PARAMETERS, ONLY : JPHEXT, JPVEXT
USE MODD_REF_n,      ONLY : XRHODREF
USE MODD_GRID_n,     ONLY : XZZ
USE MODD_CST,        ONLY : XRHOLW
!
IMPLICIT NONE
!  
!  
!*       0.2   Declarations of local variables :
!
INTEGER                    :: IIB,IJB       ! Lower bounds of the physical
                                            ! sub-domain in x and y directions
INTEGER                    :: IIE,IJE       ! Upper bounds of the physical
                                            ! sub-domain in x and y directions
!
INTEGER                    :: IKB, IKE
INTEGER                    :: IIU, IJU       ! Array sizes in i,j directions
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTHIC, ZTHRW, ZTHCW, ZTHSN, ZTHGR 
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTH_LIQ, ZTH_ICE
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDUM
INTEGER :: JK ! loop index
!
!-------------------------------------------------------------------------------
!
!*       1.    COMPUTES THE PHYSICAL SUBDOMAIN BOUNDS
!              ---------------------------------------
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
!*	 2.     DEFINITION OF THE MASK
!               ----------------------
!  initialization to FALSE on the extended subdomain
LBU_MASK(:,:,:)=.FALSE.
!
!  computing on the physical subdomain 
!==============================================================================
! Change the following lines to set the criterion for each of the NBUMASK masks
! 
IKB = 1 + JPVEXT
IKE = SIZE(XRHODREF,3) - JPVEXT
IIU = IIE + JPHEXT
IJU = IJE + JPHEXT
!
ALLOCATE(ZTHIC(IIU,IJU))   ; ZTHIC(:,:)   = 0.0
ALLOCATE(ZTHRW(IIU,IJU))   ; ZTHRW(:,:)   = 0.0
ALLOCATE(ZTHCW(IIU,IJU))   ; ZTHCW(:,:)   = 0.0
ALLOCATE(ZTHSN(IIU,IJU))   ; ZTHSN(:,:)   = 0.0
ALLOCATE(ZTHGR(IIU,IJU))   ; ZTHGR(:,:)   = 0.0
ALLOCATE(ZDUM(IIU,IJU))    ; ZDUM(:,:)    = 0.0
!
DO JK = IKB, IKE
  ZDUM(:,:)  = XRHODREF(:,:,JK) * (XZZ(:,:,JK+1) - XZZ(:,:,JK)) / XRHOLW
  ZTHIC(:,:) = ZTHIC(:,:) + XRT(:,:,JK,4) * ZDUM(:,:)
  ZTHRW(:,:) = ZTHRW(:,:) + XRT(:,:,JK,3) * ZDUM(:,:)
  ZTHCW(:,:) = ZTHCW(:,:) + XRT(:,:,JK,2) * ZDUM(:,:)
  ZTHSN(:,:) = ZTHSN(:,:) + XRT(:,:,JK,5) * ZDUM(:,:)
  ZTHGR(:,:) = ZTHGR(:,:) + XRT(:,:,JK,6) * ZDUM(:,:)
END DO
!
! m --> mm
ZTHIC(:,:) = ZTHIC(:,:) * 1000.
ZTHRW(:,:) = ZTHRW(:,:) * 1000.
ZTHCW(:,:) = ZTHCW(:,:) * 1000.
ZTHSN(:,:) = ZTHSN(:,:) * 1000.
ZTHGR(:,:) = ZTHGR(:,:) * 1000.
!
ALLOCATE(ZTH_LIQ(IIU,IJU))      ; ZTH_LIQ(:,:) = 0.0
ALLOCATE(ZTH_ICE(IIU,IJU))      ; ZTH_ICE(:,:) = 0.0
!
ZTH_LIQ(:,:) = ZTHCW(:,:) + ZTHRW(:,:) 
ZTH_ICE(:,:) = ZTHIC(:,:) + ZTHSN(:,:) + ZTHGR(:,:)
!print*, nbutime, ' - min-max inprr = ', minval(xinprr*3600.), maxval(xinprr*3600.)
!print*, nbutime, ' - min-max zth_liq = ', minval(zth_liq), maxval(zth_liq)
!print*, nbutime, ' - min-max zth_ice = ', minval(zth_ice), maxval(zth_ice)
!
LBU_MASK(IIB:IIE,IJB:IJE,1) = (XINPRR(IIB:IIE,IJB:IJE)*3.6E6) >= 5. 
!LBU_MASK(IIB:IIE,IJB:IJE,2) = .NOT.(LBU_MASK(IIB:IIE,IJB:IJE,1)) .AND. &
!                              (XINPRR(IIB:IIE,IJB:IJE)*3.6E6) >= 0.5    .AND. &
!                              ZTH_LIQ(IIB:IIE,IJB:IJE) >= 0.01  .AND. &
!                              ZTH_ICE(IIB:IIE,IJB:IJE) >= 0.1
LBU_MASK(IIB:IIE,IJB:IJE,2) = .NOT.(LBU_MASK(IIB:IIE,IJB:IJE,1)) .AND. &
                              ((XINPRR(IIB:IIE,IJB:IJE)*3.6E6) >= 0.5 .OR. &
                              ZTH_LIQ(IIB:IIE,IJB:IJE) >= 0.01  .AND. &
                              ZTH_ICE(IIB:IIE,IJB:IJE) >= 0.1)
LBU_MASK(IIB:IIE,IJB:IJE,3) = .NOT.(LBU_MASK(IIB:IIE,IJB:IJE,1)) .AND. &
                              .NOT.(LBU_MASK(IIB:IIE,IJB:IJE,2)) .AND. &
                              ZTH_LIQ(IIB:IIE,IJB:IJE) < 0.01   .AND. &
                              ZTH_ICE(IIB:IIE,IJB:IJE) >= 0.01
!
DEALLOCATE(ZTHIC)
DEALLOCATE(ZTHRW)
DEALLOCATE(ZTHCW)
DEALLOCATE(ZTHSN)
DEALLOCATE(ZTHGR)
DEALLOCATE(ZTH_LIQ)
DEALLOCATE(ZTH_ICE)
DEALLOCATE(ZDUM)
!
!==============================================================================
!
!*	 3.     INCREASE IN SURFACE ARRAY
!               -------------------------
!
WHERE (LBU_MASK(:,:,:))
  NBUSURF(:,:,:,NBUTIME)=NBUSURF(:,:,:,NBUTIME)+1
END WHERE
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SET_MASK
