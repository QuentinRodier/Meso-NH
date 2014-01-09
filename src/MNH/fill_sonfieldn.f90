!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_nest_pgd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_FILL_SONFIELD_n
!     ##########################
!
INTERFACE 
!
      SUBROUTINE FILL_SONFIELD_n(KMI,YFIELD,PNESTFIELD,KLSON)
!
INTEGER ,                 INTENT(IN)     :: KMI    ! son model number
CHARACTER(LEN=6),         INTENT(IN)     :: YFIELD ! name of the field to nest
REAL, DIMENSION(:,:,:,:), INTENT(INOUT)  :: PNESTFIELD
INTEGER,                  INTENT(IN)     :: KLSON  ! rank of son model in PNESTFIELD
!
END SUBROUTINE FILL_SONFIELD_n
END INTERFACE
!
END MODULE MODI_FILL_SONFIELD_n
!
!
!
!     ##################################################
      SUBROUTINE FILL_SONFIELD_n(KMI,YFIELD,PNESTFIELD,KLSON)
!     ##################################################
!
!!****  *FILL_SONFIELD_n* - fill the working array for nesting of pgd files
!!                          with        son model index= _n
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        27/09/96
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_GRID_n
USE MODD_NESTING
USE MODD_PARAMETERS
!
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
INTEGER ,                 INTENT(IN)     :: KMI    ! son model number
CHARACTER(LEN=6),         INTENT(IN)     :: YFIELD ! name of the field to nest
REAL, DIMENSION(:,:,:,:), INTENT(INOUT)  :: PNESTFIELD
INTEGER,                  INTENT(IN)     :: KLSON  ! rank of son model in PNESTFIELD
!
!
!*       0.2   declarations of local variables
!
INTEGER :: IIB1,IIE1,IJB1,IJE1 ! limits of physical domain of KDAD model
INTEGER :: JI1,JJ1             ! loop counters   in domain of KDAD model
!
INTEGER :: JI2INF, JI2SUP      ! limits of a grid mesh of domain of KDAD model
INTEGER :: JJ2INF,JJ2SUP       ! relatively to son domain
INTEGER :: IMI                 ! current model index
INTEGER :: JLAYER              ! loop counter
!-------------------------------------------------------------------------------
!
!*       1.    initializations
!              ---------------
!
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(KMI)
!
!* correct only if JPHEXT = 1
!
IIB1 = NXOR_ALL (KMI)+1
IIE1 = NXEND_ALL(KMI)-1
IJB1 = NYOR_ALL (KMI)+1
IJE1 = NYEND_ALL(KMI)-1
!
DO JLAYER=1,SIZE(PNESTFIELD,4)
  PNESTFIELD(:,:,KLSON,JLAYER) = XUNDEF
END DO
!
!-------------------------------------------------------------------------------
IF (KLSON==1) THEN
!
!*       2.    case KLSON=1 : father itself
!              ----------------------------
!
      SELECT CASE(YFIELD)
        CASE ('ZS    ')
          PNESTFIELD(:,:,KLSON,1) = XZS(:,:)
         CASE ('ZSMT  ')   ! smooth topography for SLEVE coordinate
          PNESTFIELD(:,:,KLSON,1) = XZSMT(:,:)
        CASE DEFAULT
          GOTO 9999 ! end of subroutine
      END SELECT
!
!-------------------------------------------------------------------------------
ELSE
!
!*       3.    case KLSON>1 : one son
!              ----------------------
!
  DO JI1 = IIB1,IIE1
    DO JJ1 = IJB1,IJE1
      JI2INF= (JI1-IIB1)  *NDXRATIO_ALL(KMI)+1+JPHEXT
      JI2SUP= (JI1-IIB1+1)*NDXRATIO_ALL(KMI)  +JPHEXT
      JJ2INF= (JJ1-IJB1)  *NDYRATIO_ALL(KMI)+1+JPHEXT
      JJ2SUP= (JJ1-IJB1+1)*NDYRATIO_ALL(KMI)  +JPHEXT

      SELECT CASE(YFIELD)
         CASE ('ZS    ')
           PNESTFIELD(JI1,JJ1,KLSON,1) = SUM ( XZS(JI2INF:JI2SUP,JJ2INF:JJ2SUP ) )&
                                           / ( NDXRATIO_ALL(KMI)*NDYRATIO_ALL(KMI) )
         CASE ('ZSMT  ')  ! smooth topography for SLEVE coordinate
           PNESTFIELD(JI1,JJ1,KLSON,1) = SUM ( XZSMT(JI2INF:JI2SUP,JJ2INF:JJ2SUP ) )&
                                           / ( NDXRATIO_ALL(KMI)*NDYRATIO_ALL(KMI) )
        CASE DEFAULT
          GOTO 9999 ! end of subroutine
      END SELECT

    END DO
  END DO
!
!-------------------------------------------------------------------------------
END IF
!
9999 CALL GOTO_MODEL(IMI)
!-------------------------------------------------------------------------------
!
END SUBROUTINE FILL_SONFIELD_n
