!MNH_LIC Copyright 1995-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
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
!!    increased by 1 and stored in the array NBUSURF.
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
!!         NBUSURF    : mask tracer array (surface array)
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
USE MODD_FIELD_n , ONLY : XWT , XRT
USE MODD_GRID_n , ONLY : XZZ
USE MODD_REF_n, ONLY : XRHODREF
USE MODD_PRECIP_n, ONLY : XINPRR
!
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
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2)) :: ZIWP, ZMAXW
INTEGER :: II
!-------------------------------------------------------------------------------
!
!*       1.    COMPUTES THE PHYSICAL SUBDOMAIN BOUNDS
!              ---------------------------------------
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
! compute IWP
ZIWP=0.
ZMAXW=0.
DO II = 2,SIZE(XZZ,3)-1
   ZIWP(:,:) = ZIWP(:,:) + XRHODREF(:,:,II)*(XRT(:,:,II,4)+XRT(:,:,II,5)+XRT(:,:,II,6))*(XZZ(:,:,II+1)-XZZ(:,:,II))
   ZMAXW(:,:) = MAX(ZMAXW(:,:),XWT(:,:,II))
END DO
!*	 2.     DEFINITION OF THE MASK
!               ----------------------
!  initialization to FALSE on the extended subdomain
LBU_MASK(:,:,:)=.FALSE.
!
!  computing on the physical subdomain 
!==============================================================================
! Change the following lines to set the criterion for each of the NBUMASK masks
! 
! 1st mask convective areas
LBU_MASK(IIB:IIE,IJB:IJE,1) = ZIWP(IIB:IIE,IJB:IJE)>0.5 .AND. &
                              ( MAXVAL(XWT(IIB:IIE,IJB:IJE,:),3)>2.5 .OR. &
                                XINPRR(IIB:IIE,IJB:IJE)>15./3.6E6 )
!
! 2rd mask stratiform areas                                                                                            
IF (NBUMASK>=2) &                                                                                                     
  LBU_MASK(IIB:IIE,IJB:IJE,2)=ZIWP(IIB:IIE,IJB:IJE)>0.5 .AND. &
                              XINPRR(IIB:IIE,IJB:IJE)>1./3.6E6 .AND. &
                              .NOT. LBU_MASK(IIB:IIE,IJB:IJE,1)                                                          
!
! 3rd mask cirriform areas                                                                                                                     
IF (NBUMASK>=3) &
  LBU_MASK(IIB:IIE,IJB:IJE,3)=ZIWP(IIB:IIE,IJB:IJE)>0.5 .AND. &
                              .NOT. LBU_MASK(IIB:IIE,IJB:IJE,1) .AND. &
                              .NOT. LBU_MASK(IIB:IIE,IJB:IJE,2)
!
!==============================================================================
!
!*	 3.     INCREASE IN SURFACE ARRAY
!               -------------------------
!
WHERE (LBU_MASK(:,:,:))
  NBUSURF(:,:,:,NBUTIME) = NBUSURF(:,:,:,NBUTIME) + 1
END WHERE
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SET_MASK
