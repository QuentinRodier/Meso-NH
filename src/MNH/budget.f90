!MNH_LIC Copyright 1994-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications
!  P. Wautelet 28/01/2020: new subroutines: Budget_store_init, Budget_store_end and Budget_source_id_find in new module mode_budget
!-----------------------------------------------------------------

!#################
module mode_budget
!#################

use modd_budget, only: cbutype, nbutime, tbudgetdata

use modi_cart_compress, only: Cart_compress
use modi_mask_compress, only: Mask_compress

use mode_msg

implicit none

private

public :: Budget_store_init
public :: Budget_store_end


contains

subroutine Budget_store_init( tpbudget, hsource, pvars )
  type(tbudgetdata),      intent(inout) :: tpbudget ! Budget datastructure
  character(len=*),       intent(in)    :: hsource  ! Name of the source term
  real, dimension(:,:,:), intent(in)    :: pvars    ! Current value to be stored

  integer :: iid ! Reference number of the current source term

  call Print_msg( NVERB_DEBUG, 'BUD', 'Budget_store_init', trim( tpbudget%cname )//':'//trim( hsource ) )

  call Budget_source_id_find( tpbudget, hsource, iid )

  if ( tpbudget%ntmpstoresource /= 0 ) then
    call Print_msg( NVERB_ERROR, 'BUD', 'Budget_store_init', 'ntmpstoresource already set (previous call to ' &
                    //'Budget_store_end missing?) for '//trim( tpbudget%cname )//':'//trim( hsource ) )
  end if

  if ( tpbudget%tsources(iid)%ldonotinit ) then
    ! If ldonotinit is set, this subroutine should not be called
    call Print_msg( NVERB_ERROR, 'BUD', 'Budget_store_init', 'should not be called for ' &
                    //trim( tpbudget%cname )//':'//trim( hsource ) )
    return
  end if

  if ( tpbudget%tsources(iid)%lenabled ) then
    if ( tpbudget%ntmpstoresource /= 0 ) then
      call Print_msg( NVERB_ERROR, 'BUD', 'Budget_store_init', 'xtmpstore already used by ' &
                      //trim( tpbudget%tsources(tpbudget%ntmpstoresource)%cmnhname ) )
      return
    end if

    tpbudget%ntmpstoresource = iid

    !Store data into the budget temporary array
    !This value will be subtracted from the next one (in Budget_store_end) to get the evolution of the array between the 2 calls
    if ( cbutype == 'CART' ) then
      tpbudget%xtmpstore(:, :, : ) = Cart_compress( pvars(:, :, : ) )
    else if ( cbutype == 'MASK' ) then
      tpbudget%xtmpstore(:, nbutime, : ) = Mask_compress( pvars(:, :, : ) )
    else
      call Print_msg( NVERB_ERROR, 'BUD', 'Budget_store_init', 'unknown cbutype: '//trim( cbutype ) )
    end if
  end if

end subroutine Budget_store_init


subroutine Budget_store_end( tpbudget, hsource, pvars )
use modd_budget,only:nbusil,NBUSJL,NBUKL
  type(tbudgetdata),      intent(inout) :: tpbudget ! Budget datastructure
  character(len=*),       intent(in) :: hsource     ! Name of the source term
  real, dimension(:,:,:), intent(in) :: pvars       ! Current value to be stored

  integer :: iid    ! Reference number of the current source term
  integer :: igroup ! Number of the group where to store the source term

  call Print_msg( NVERB_DEBUG, 'BUD', 'Budget_store_end', trim( tpbudget%cname )//':'//trim( hsource ) )

  call Budget_source_id_find( tpbudget, hsource, iid )

  if ( tpbudget%tsources(iid )%lenabled ) then
    if ( iid /= tpbudget%ntmpstoresource .and. .not.tpbudget%tsources(iid )%ldonotinit ) then
      if ( tpbudget%ntmpstoresource == 0 ) then
        call Print_msg( NVERB_ERROR, 'BUD', 'Budget_store_end', 'ntmpstoresource not set for ' &
                        //trim( tpbudget%tsources(iid)%cmnhname ) )
      else
        call Print_msg( NVERB_ERROR, 'BUD', 'Budget_store_end', 'xtmpstore used by an other source: '    &
                        //trim( tpbudget%tsources(tpbudget%ntmpstoresource)%cmnhname )//', expected: '   &
                        //trim( tpbudget%tsources(iid)%cmnhname ) )
      end if
    end if

    !Store data into the budget array
    !The values are computed by the difference between the values stored in the temporary array (filled in Budget_store_init)
    !and the current values added to the already stored ones.
    !Except if ldonotinit is true. In that case, overwrite the array.
    igroup = tpbudget%tsources(iid)%ngroup
    if ( cbutype == 'CART' ) then
      if ( tpbudget%tsources(iid )%ldonotinit ) then
        if ( tpbudget%tsources(iid )%loverwrite ) then
          tpbudget%tgroups(igroup )%xdata(:, :, : ) =   Cart_compress( pvars(:, :, : ) )
        else
          tpbudget%tgroups(igroup )%xdata(:, :, : ) =   tpbudget%tgroups(igroup )%xdata(:, :, : ) &
                                                      + Cart_compress( pvars(:, :, : ) )
        end if
      else
        if ( tpbudget%tsources(iid )%loverwrite ) then
          tpbudget%tgroups(igroup )%xdata(:, :, : ) =   Cart_compress( pvars(:, :, : ) )          &
                                                      - tpbudget%xtmpstore(:, :, : )
        else
          tpbudget%tgroups(igroup )%xdata(:, :, : ) =   tpbudget%tgroups(igroup )%xdata(:, :, : ) &
                                                      + Cart_compress( pvars(:, :, : ) )          &
                                                      - tpbudget%xtmpstore(:, :, : )
        end if
      end if
    else if ( cbutype == 'MASK' ) then
      if ( tpbudget%tsources(iid )%ldonotinit ) then
        if ( tpbudget%tsources(iid )%loverwrite ) then
          tpbudget%tgroups(igroup )%xdata(:, nbutime, : ) =   Mask_compress( pvars(:, :, : ) )
        else
          tpbudget%tgroups(igroup )%xdata(:, nbutime, : ) =   tpbudget%tgroups(igroup )%xdata(:, nbutime, : ) &
                                                            + Mask_compress( pvars(:, :, : ) )
        end if
      else
        if ( tpbudget%tsources(iid )%loverwrite ) then
          tpbudget%tgroups(igroup )%xdata(:, nbutime, : ) =   Mask_compress( pvars(:, :, : ) )   &
                                                            - tpbudget%xtmpstore(:, nbutime, : )
        else
          tpbudget%tgroups(igroup )%xdata(:, nbutime, : ) =   tpbudget%tgroups(igroup )%xdata(:, nbutime, : ) &
                                                            + Mask_compress( pvars(:, :, : ) )                &
                                                            - tpbudget%xtmpstore(:, nbutime, : )
        end if
      end if
    else
      call Print_msg( NVERB_ERROR, 'BUD', 'Budget_store_end', 'unknown cbutype: '//trim( cbutype ) )
    end if

    ! Release the budget temporary array
    tpbudget%ntmpstoresource = 0
  end if

end subroutine Budget_store_end


subroutine Budget_source_id_find( tpbudget, hsource, kid )
  type(tbudgetdata), intent(in)  :: tpbudget ! Budget datastructure
  character(len=*),  intent(in)  :: hsource  ! Name of the source term
  integer,           intent(out) :: kid      ! Reference number of the current source term

  integer :: iid
  integer :: ji

  call Print_msg( NVERB_DEBUG, 'BUD', 'Budget_source_id_find', trim( tpbudget%cname )//':'//trim( hsource ) )

  iid = 0
  do ji = 1, tpbudget%nsources
    if ( trim( hsource ) == trim( tpbudget%tsources(ji)%cmnhname ) ) then
      iid = ji
      exit
    end if
  end do

  if ( iid > 0 ) then
    call Print_msg( NVERB_DEBUG, 'BUD', 'Budget_source_id_find', trim( tpbudget%cname )//':'//trim( hsource )//' found' )
  else
    call Print_msg( NVERB_ERROR, 'BUD', 'Budget_source_id_find', trim( tpbudget%cname )//':'//trim( hsource )//' not found' )
  end if

  kid = iid
end subroutine Budget_source_id_find

end module mode_budget


!##################
 MODULE MODI_BUDGET
!##################
!
INTERFACE
!
SUBROUTINE BUDGET(PVARS,KBUDN,HBUVAR)
!
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PVARS    ! Source 
INTEGER               , INTENT(IN) :: KBUDN    ! variable number
CHARACTER (LEN=*)    , INTENT(IN) :: HBUVAR   ! Identifier of the Budget of the
                                               ! variable that is considered 
!
END SUBROUTINE BUDGET
!
END INTERFACE
!
END MODULE MODI_BUDGET
!     #####################################
      SUBROUTINE BUDGET(PVARS,KBUDN,HBUVAR)
!     #####################################
!
!!****  *BUDGET* - routine to call the BUDGET routine. 
!!                           
!!
!!    PURPOSE
!!    -------
!        This routine selects the variable RVAR, the budget of which is 
!     processed in the inner routine BUDGET_CASE.  !
!!**  METHOD
!!    ------
!!       
!!     
!!
!!    EXTERNAL
!!    --------
!!      CART_COMPRESS 
!!      MASK_COMPRESS
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!       Module MODD_BUDGET
!!         XBURU       : budget array of the variable RU
!!         XBURV       : budget array of the variable RV
!!         XBURW       : budget array of the variable RW
!!         XBURTH      : budget array of the variable RTH
!!         XBURTKE     : budget array of the variable RTKE
!!         XBURRV      : budget array of the variable RRV
!!         XBURRC      : budget array of the variable RRC
!!         XBURRR      : budget array of the variable RRR
!!         XBURRI      : budget array of the variable RRI
!!         XBURRS      : budget array of the variable RRS
!!         XBURRG      : budget array of the variable RRG
!!         XBURRH      : budget array of the variable RRH
!!         XBURTKE     : budget array of the variable RTKE
!!         XBURSV(x)   : budget array of the variable RSVx
!!
!!    REFERENCE
!!    ---------
!!      None
!!
!!    AUTHOR
!!    ------
!!  	J. Nicolau       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    30/08/94
!!      J. Stein    26/06/96  add the 'OF','NO' option  
!!      J.-P. Pinty 12/12/96  simplifies the coding
!!      V. Masson   06/10/02  add LES budgets
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!      J.Escobar : 09/07/2019 : for bit reproductiblity use MPPDB_CHECK with PRECISION=0.0 error
!!      
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BUDGET
USE MODD_LUNIT
!USE MODD_CONF_n
USE MODD_CONF, ONLY : LCHECK
USE MODD_NSV,  ONLY : NSV
USE MODD_LES
!
USE MODE_MSG
!
USE MODI_LES_BUDGET
USE MODI_CART_COMPRESS
USE MODI_MASK_COMPRESS
!
USE MODE_MPPDB
!
USE MODI_SECOND_MNH
!
IMPLICIT NONE
!  
!  
!*       0.1   Declarations of arguments :
!
INTEGER               , INTENT(IN) :: KBUDN    ! variable number
REAL, DIMENSION(:,:,:), INTENT(IN) :: PVARS    ! source of the variable 
CHARACTER (LEN=*)     , INTENT(IN) :: HBUVAR   ! Identifier of the Budget of the
                                               ! variable that is considered 
INTEGER  :: IBUSV   ! Index of the SV 
!
INTEGER  :: ILUOUT0 ! Logical unit number for output-listing
INTEGER  :: IRESP   ! Return code of FM-routines
!
REAL     :: ZTIME1  ! CPU time counter
REAL     :: ZTIME2  ! CPU time counter
!
!-------------------------------------------------------------------------------
!
!* Reproductivity checks
!  Warning: requires an adaptation of the makefile in order to run two runs in
!  parallel for comparison
!
IF (LCHECK) THEN
  print*,'BUDGET :',HBUVAR
  CALL MPPDB_CHECK3D(PVARS,HBUVAR,PRECISION)
END IF
!
!
!* call to LES budgets
!
IF (LLES_CALL) CALL LES_BUDGET(PVARS,KBUDN,HBUVAR)
!
!* call to prognostic variables budgets
!
IF (.NOT. LBU_ENABLE) RETURN
!
SELECT CASE (KBUDN)
  CASE ( NBUDGET_U )
    IF (.NOT. LBU_RU) RETURN 
  CASE ( NBUDGET_V )
    IF (.NOT. LBU_RV) RETURN 
  CASE ( NBUDGET_W )
    IF (.NOT. LBU_RW) RETURN
  CASE (NBUDGET_TH)
    IF (.NOT. LBU_RTH) RETURN 
  CASE ( NBUDGET_TKE )
    IF (.NOT. LBU_RTKE) RETURN 
  CASE ( NBUDGET_RV )
    IF (.NOT. LBU_RRV) RETURN 
  CASE ( NBUDGET_RC )
    IF (.NOT. LBU_RRC) RETURN 
  CASE (NBUDGET_RR )
    IF (.NOT. LBU_RRR) RETURN 
  CASE ( NBUDGET_RI )
    IF (.NOT. LBU_RRI) RETURN 
  CASE ( NBUDGET_RS )
    IF (.NOT. LBU_RRS) RETURN 
  CASE ( NBUDGET_RG )
    IF (.NOT. LBU_RRG) RETURN 
  CASE ( NBUDGET_RH )
    IF (.NOT. LBU_RRH) RETURN 
  CASE ( NBUDGET_SV1 : )
    IF (.NOT. LBU_RSV) RETURN 
END SELECT
!
!-------------------------------------------------------------------------------
!
CALL SECOND_MNH(ZTIME1)
!
SELECT CASE (KBUDN)
!
  CASE ( NBUDGET_U )   !            ==>  RU BUDGET
    CALL BUDGET_CASE(XBURU)
!
  CASE ( NBUDGET_V )   !            ==>  RV BUDGET
    CALL BUDGET_CASE(XBURV)
!
  CASE ( NBUDGET_W )   !            ==>  RW BUDGET
    CALL BUDGET_CASE(XBURW)
!
  CASE ( NBUDGET_TH )  !            ==>  RTH BUDGET
    CALL BUDGET_CASE(XBURTH)
!
  CASE ( NBUDGET_TKE ) !            ==>  RTKE BUDGET
    CALL BUDGET_CASE(XBURTKE)
!
  CASE ( NBUDGET_RV )  !            ==>  RRV BUDGET
    CALL BUDGET_CASE(XBURRV)
!
  CASE ( NBUDGET_RC )  !            ==>  RRC BUDGET
    CALL BUDGET_CASE(XBURRC)
!
  CASE ( NBUDGET_RR )  !            ==>  RRR BUDGET
    CALL BUDGET_CASE(XBURRR)
!
  CASE ( NBUDGET_RI )  !            ==>  RRI BUDGET
    CALL BUDGET_CASE(XBURRI)
!
  CASE ( NBUDGET_RS )  !            ==>  RRS BUDGET
    CALL BUDGET_CASE(XBURRS)
!
  CASE ( NBUDGET_RG )  !            ==>  RRG BUDGET
    CALL BUDGET_CASE(XBURRG)
!
  CASE ( NBUDGET_RH )  !            ==>  RRH BUDGET
    CALL BUDGET_CASE(XBURRH)
!
  CASE ( NBUDGET_SV1 : ) !          ==>  RSVx BUDGET
    IBUSV = KBUDN - ( NBUDGET_SV1 - 1 )
    IF( IBUSV <= NSV ) THEN 
      CALL BUDGET_CASE(XBURSV(:,:,:,:,IBUSV))
    ELSE
      ILUOUT0 = TLUOUT0%NLU
      WRITE(UNIT=ILUOUT0,FMT='("BUDGET: SCALAR VARIABLE",I2," IS ABSENT !!")') &
                                IBUSV
      WRITE(UNIT=ILUOUT0,FMT='("CHECK FOR THE CALL BUDGET OF THAT VARIABLE")')
!callabortstop
      CALL PRINT_MSG(NVERB_FATAL,'BUD','BUDGET','')
    END IF
END SELECT
!
CALL SECOND_MNH(ZTIME2)
!
XTIME_BU_PROCESS = XTIME_BU_PROCESS + ZTIME2 - ZTIME1
XTIME_BU = XTIME_BU + ZTIME2 - ZTIME1
!
!----------------------------------------------------------------------
CONTAINS
!----------------------------------------------------------------------
!     ###############################
      SUBROUTINE BUDGET_CASE(PBURVAR)
!     ###############################
!
!!****  *BUDGET_CASE* - routine to call the BUDGET_CASE routine. 
!!                           
!!
!!    PURPOSE
!!    -------
!        This routine chooses the right call to the functions CART_COMPRESS
!     or MASK_COMPRESS (which realize the compression of the source PVARS
!     in the different directions) and achieves in function of HACTION (which
!     determines the operations to be executed) the budget for the variable 
!     corresponding to the number KBUDN. The budget process counter is
!     incremented by NBUINC depending on the number of active processes in the 
!     model.
!
!!**  METHOD
!!    ------
!!       
!!     
!!
!!    EXTERNAL
!!    --------
!!      CART_COMPRESS 
!!      MASK_COMPRESS
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!       Module MODD_BUDGET
!!         CBUACTION   : type of operation
!!         CBUTYPE     : budget type (CART,MASK or NONE)
!!         NBUTIME     : number of the budget step
!!         NBUPROCCTR  : process counter for each budget variable
!!         PBURVAR     : budget array of the variable RVAR
!!
!!    REFERENCE
!!    ---------
!!      None
!!
!!    AUTHOR
!!    ------
!!  	J.-P. Pinty   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/12/96
!!      Modification 24/06/99 N. Asencio  : budget // , the dimensions of the
!!                                          budget arrays are implicit
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
  USE MODI_CART_COMPRESS
  USE MODI_MASK_COMPRESS
!
  IMPLICIT NONE
!  
!  
!*       0.1   Declarations of arguments :
!
 REAL, DIMENSION(:,:,:,:), INTENT(INOUT):: PBURVAR  ! budget of variable RVAR
!
!*       0.2   Declarations of local variables :
  CHARACTER (LEN=99) ::   YBUVAR_ADJUSTED           ! Adjusted string
  CHARACTER (LEN=99) ::   YBUCOMMENT_ADJUSTED       ! Adjusted string
  CHARACTER (LEN=99) ::   YBUVAR                    ! local string
  CHARACTER (LEN=99) ::   YBUCOMMENT                ! local string

  INTEGER            ::   ILEN                      ! Number of non-blank char.
!
!
!*       1.     SECURITY TEST
!               -------------
!
  YBUVAR      =   HBUVAR
  YBUCOMMENT  =   CBUCOMMENT(KBUDN,NBUPROCCTR(KBUDN))
  YBUVAR_ADJUSTED     = ADJUSTR(YBUVAR)
  YBUCOMMENT_ADJUSTED = ADJUSTR(YBUCOMMENT)
  ILEN =  LEN_TRIM( ADJUSTL(YBUVAR))
!
  IF( CBUACTION(KBUDN,NBUCTR_ACTV(KBUDN))/='NO'.AND. &
      CBUACTION(KBUDN,NBUCTR_ACTV(KBUDN))/='OF'.AND. &
      CBUACTION(KBUDN,NBUCTR_ACTV(KBUDN))/='CC'     ) THEN
    IF( YBUVAR_ADJUSTED(100-ILEN:99) /= YBUCOMMENT_ADJUSTED(100-ILEN:99) &
                                             .OR. ILEN==0 ) THEN
      ILUOUT0 = TLUOUT0%NLU
      WRITE(UNIT=ILUOUT0,FMT='("BUDGET: WRONG BUDGET IDENTIFICATION !!")')
      WRITE(UNIT=ILUOUT0,FMT='("BUDGET: PRESENT  VARIABLE: ",I2)') KBUDN
      WRITE(UNIT=ILUOUT0,FMT='("BUDGET: PRESENT  IDENTIFIER: ",A99)') &
                                    YBUVAR_ADJUSTED
      WRITE(UNIT=ILUOUT0,FMT='("BUDGET: EXPECTED IDENTIFIER: ",A99)') &
                            YBUCOMMENT_ADJUSTED
      WRITE(UNIT=ILUOUT0,FMT='("PLEASE CHECK THE CALL BUDGET OF THE VARIABLE")')
      WRITE(UNIT=ILUOUT0,FMT='("AND THE BUDGET PROCESS ORDER IN INI_BUDGET !")')
!callabortstop
      CALL PRINT_MSG(NVERB_FATAL,'BUD','BUDGET','')
    END IF
  END IF
!
! Budget integration in case of successful test
!
  SELECT CASE (CBUTYPE)
!
!*	     2.     "CART" CASE
!               -----------
!
    CASE ('CART')
!
      SELECT CASE (CBUACTION(KBUDN,NBUCTR_ACTV(KBUDN)))
!
!*	     2.1    Budget beginning : initial fields
!               filled in budget tabulars (NBUPROCCTR=1)
!
        CASE('IG')            
          PBURVAR(:,:,:,1)=CART_COMPRESS(PVARS)
!
!*	     2.2    average tendancy filled every time
!               step in budget tabulars (NBUPROCCTR=3)
!            
        CASE('ES')          
          PBURVAR(:,:,:,3)=PBURVAR(:,:,:,3)+CART_COMPRESS(PVARS)/NBUSTEP
!
!*    	 2.3    Cumul of the sources 
!
        CASE('CC')
          PBURVAR(:,:,:,2)=CART_COMPRESS(PVARS)
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
!
!*	     2.4    Difference in order to compute the budget
!                   for the process NBUPROCCTR                 
!
        CASE('DD')
          PBURVAR(:,:,:,NBUPROCCTR(KBUDN))= PBURVAR(:,:,:,NBUPROCCTR(KBUDN)) &
                                          + CART_COMPRESS(PVARS)             &
                                          - PBURVAR(:,:,:,2)          
          NBUPROCCTR(KBUDN)=NBUPROCCTR(KBUDN)+1
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
!
!*	     2.5    Difference in order to compute the budget for the
!               process NBUPROCCTR and Cumul of the sources (NBUPROCCTR=2)
!
        CASE('DC')
          PBURVAR(:,:,:,NBUPROCCTR(KBUDN)) = PBURVAR(:,:,:,NBUPROCCTR(KBUDN))&
                                           + CART_COMPRESS(PVARS)            &
                                           - PBURVAR(:,:,:,2)          
          PBURVAR(:,:,:,2)=CART_COMPRESS(PVARS)
          NBUPROCCTR(KBUDN)=NBUPROCCTR(KBUDN)+1
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
        CASE('NO')
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
        CASE('OF')
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
          RETURN
      END SELECT
!        
!*	     3.    "MASK" CASE
!               -----------
!
    CASE ('MASK')
!
      SELECT CASE (CBUACTION(KBUDN,NBUCTR_ACTV(KBUDN)))            
!
!*	     3.1    Budget beginning : initial fields
!               filled in budget tabulars (NBUPROC=1)
!
        CASE('IG')
          PBURVAR(:,NBUTIME,:,1) = MASK_COMPRESS(PVARS)
!
!*	     3.2    average tendancy filled every time
!                 step in budget tabulars (NBUPROCCTR=3)
!    
        CASE('ES')      
          PBURVAR(:,NBUTIME,:,3) = PBURVAR(:,NBUTIME,:,3)   &
                                 + MASK_COMPRESS(PVARS)/NBUSTEP
!
!*	     3.3    Cumul of the sources 
!
        CASE('CC')
          PBURVAR(:,NBUTIME,:,2)=MASK_COMPRESS(PVARS)
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
!
!*	     3.4    Difference in order to compute the budget
!               for the process NBUPROCCTR                 
!
        CASE('DD')
          PBURVAR(:,NBUTIME,:,NBUPROCCTR(KBUDN))                      &
                             = PBURVAR(:,NBUTIME,:,NBUPROCCTR(KBUDN)) &
                             + MASK_COMPRESS(PVARS)                   &
                             - PBURVAR(:,NBUTIME,:,2)          
          NBUPROCCTR(KBUDN)=NBUPROCCTR(KBUDN)+1
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)              &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
!
!*       3.5    Difference in order to compute the budget for the
!               process NBUPROCCTR and Cumul of the sources (NBUPROCCTR=2)
!
        CASE('DC')
          PBURVAR(:,NBUTIME,:,NBUPROCCTR(KBUDN))                      &
                             = PBURVAR(:,NBUTIME,:,NBUPROCCTR(KBUDN)) &
                                               +MASK_COMPRESS(PVARS)  &
                                               -PBURVAR(:,NBUTIME,:,2)
          PBURVAR(:,NBUTIME,:,2)=MASK_COMPRESS(PVARS)
          NBUPROCCTR(KBUDN)=NBUPROCCTR(KBUDN)+1
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
        CASE('NO')
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
        CASE('OF')
!
! advance the process counter
!
          NBUCTR_ACTV(KBUDN) = NBUCTR_ACTV(KBUDN)             &
                             + NBUINC(KBUDN,NBUCTR_ACTV(KBUDN))
          RETURN
      END SELECT          
  END SELECT
!
  END SUBROUTINE BUDGET_CASE
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE BUDGET
