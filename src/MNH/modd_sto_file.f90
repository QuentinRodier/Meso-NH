!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######spl
MODULE MODD_STO_FILE
!################
CHARACTER (LEN=28), SAVE :: CFILES(100)       ! names of the files to be treated
CHARACTER (LEN=28), SAVE :: CFILES_STA(100)   ! status of these files 'INIT_SV'
                                              ! if a restart of the lagrangian
                                              ! tracers has been performed
INTEGER           , SAVE :: NSTART_SUPP(100)  ! supplementary starts 
                                              ! for the lagrangian trajectories 
!
END MODULE MODD_STO_FILE
