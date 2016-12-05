!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_GRIB_GRID(HGRIB,KLUOUT,HINMODEL,HGRIDTYPE,TPTIME_GRIB)
!     ##########################################################################
!
!!****  *PREP_GRIB_GRID* - reads GRIB grid.
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. Masson (from read_all_data_grib_case)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   06/2003
!!      S. Faroux 01/2011 : to use library GRIB_API instead of GRIBEX (from
!!                          read_all_data_grib_case)
!!                  05/12/2016 (G.Delautier) length of HGRID for grib_api > 1.14
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_TYPE_DATE_SURF
!
USE MODE_READ_GRIB
!
USE MODD_GRID_ROTLATLON
USE MODD_GRID_GAUSS, ONLY : XILA1, XILO1, XILA2, XILO2, NINLA, NINLO, NILEN, LROTPOLE, XCOEF, XLAP, XLOP
USE MODD_GRID_AROME, ONLY : XX, XY, NX, NY, XLAT0, XLON0, XLATOR, XLONOR, XRPK, XBETA
USE MODD_GRID_GRIB,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,       ONLY : XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!* 0.1. Declaration of arguments
!       ------------------------
!
 CHARACTER(LEN=*),  INTENT(IN)   :: HGRIB     ! Grib file name
INTEGER,          INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=6), INTENT(OUT)   :: HINMODEL  ! Grib originating model
 CHARACTER(LEN=10), INTENT(OUT)  :: HGRIDTYPE ! Grid type
TYPE (DATE_TIME)                :: TPTIME_GRIB    ! current date and time

!
!* 0.2 Declaration of local variables
!      ------------------------------
! General purpose variables
INTEGER(KIND=kindOfInt)                            :: IRET          ! Return code from subroutines
!
! Variable involved in the task of reading the grib file
INTEGER(KIND=kindOfInt)                            :: IMISSING
INTEGER(KIND=kindOfInt)                            :: IUNIT
INTEGER(KIND=kindOfInt)                            :: IGRIB
INTEGER                            :: ICENTER       ! number of center
 CHARACTER(LEN=40)                  :: HGRID         ! type of grid
INTEGER                            :: ISCAN, JSCAN
INTEGER                            :: ILENX ! nb points in X
INTEGER                            :: ILENY ! nb points in Y
INTEGER                            :: ITIME
INTEGER                            :: IUNITTIME,IP1
!
! Grib Grid definition variables
INTEGER                            :: JLOOP1        ! Dummy counter
!JUAN
INTEGER(KIND=kindOfInt), DIMENSION(:), ALLOCATABLE :: ININLO_GRIB
!JUAN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_GRIB_GRID',0,ZHOOK_HANDLE)
WRITE (KLUOUT,'(A)') ' -- Grib reader started'
!
! open grib file
 CALL GRIB_OPEN_FILE(IUNIT,HGRIB,'R',IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error opening the grib file '//HGRIB)
END IF
!
 CALL GRIB_NEW_FROM_FILE(IUNIT,IGRIB,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error in reading the grib file')
END IF
!
! close the grib file
 CALL GRIB_CLOSE_FILE(IUNIT)
!
!---------------------------------------------------------------------------------------
!* 2.  Fix originating center
!---------------------------------------------------------------------------------------
!
 CALL GRIB_GET(IGRIB,'centre',ICENTER,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error in reading center')
END IF
!
 CALL GRIB_GET(IGRIB,'typeOfGrid',HGRID,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error in reading type of grid')
END IF
!
SELECT CASE (ICENTER)

  CASE (96)
    WRITE (KLUOUT,'(A)') ' | Grib file from HARMONY'  
    HINMODEL='ALADIN'
    HGRIDTYPE='AROME     '

  CASE (82)
    WRITE (KLUOUT,'(A)') ' | Grib file from HIRLAM'  
    HINMODEL='HIRLAM'
    HGRIDTYPE='ROTLATLON '

  CASE (98)
    WRITE (KLUOUT,'(A)') ' | Grib file from European Center for Medium-range Weather Forecast'
    HINMODEL = 'ECMWF '
    HGRIDTYPE= 'GAUSS     '

  CASE (85)
    SELECT CASE (HGRID)  

      CASE('regular_gg')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Arpege model'
        WRITE (KLUOUT,'(A)') 'but same grid as ECMWF model (unstretched)'
        HINMODEL = 'ARPEGE'
        HGRIDTYPE= 'GAUSS     '

      CASE('reduced_gg')  
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Arpege model'
        WRITE (KLUOUT,'(A)') 'but reduced grid'
        HINMODEL = 'ARPEGE'
        HGRIDTYPE= 'GAUSS     '

      CASE('regular_ll')  
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Mocage model'
        HINMODEL = 'MOCAGE'
        HGRIDTYPE= 'LATLON    '

      CASE('unknown_PLPresent','reduced_stretched_rotated_gg')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Arpege model'
        HINMODEL = 'ARPEGE'
        HGRIDTYPE= 'ROTGAUSS  '

      CASE('lambert')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Aladin france model'
        HINMODEL = 'ALADIN'
        HGRIDTYPE= 'AROME     '

      CASE('mercator')
        WRITE (KLUOUT,'(A)') ' | Grib file from French Weather Service - Aladin reunion model'
        HINMODEL = 'ALADIN'
        HGRIDTYPE= 'MERCATOR  '

    END SELECT

  CASE DEFAULT
    CALL ABOR1_SFX('PREP_GRIB_GRID: GRIB FILE FORMAT NOT SUPPORTED')

END SELECT

!---------------------------------------------------------------------------------------
!* 3. Number of points
!---------------------------------------------------------------------------------------
!
NX    = NUNDEF
NY    = NUNDEF
NINLA = NUNDEF
NILEN = NUNDEF
IF (ALLOCATED(NINLO)) DEALLOCATE(NINLO)
!
SELECT CASE (HGRIDTYPE)

     CASE ('AROME    ','MERCATOR  ')
     ! 3.1 Lambert conformal projection (ALADIN files)
     ! or Mercator projection (ALADIN REUNION files)
       CALL GRIB_GET(IGRIB,'Nj',NY,IRET)
       CALL GRIB_GET(IGRIB,'Ni',NX,IRET)
       NNI= NX * NY
     !
     !
     CASE ('GAUSS    ','ROTGAUSS  ','LATLON    ')
     ! 3.2 Usual or Gaussian lat,lon grid (ECMWF files)
     !
       CALL GRIB_GET(IGRIB,'Nj',NINLA,IRET)
       ALLOCATE (NINLO(NINLA))
       ALLOCATE (ININLO_GRIB(NINLA))
       NILEN = 0
       CALL GRIB_IS_MISSING(IGRIB,'pl',IMISSING,IRET)
       IF (IRET /= 0 .OR. IMISSING==1)  THEN !  regular
         CALL GRIB_GET(IGRIB,'Ni',ININLO_GRIB(1),IRET)  
         ININLO_GRIB(2:NINLA)=ININLO_GRIB(1)
         NILEN=NINLA*ININLO_GRIB(1)
       ELSE !  quasi-regular
         CALL GRIB_GET(IGRIB,'pl',ININLO_GRIB)
         DO JLOOP1=1 ,NINLA
           NILEN = NILEN + ININLO_GRIB(JLOOP1)
         ENDDO
       ENDIF
       NNI = NILEN
       NINLO = ININLO_GRIB !JUAN 
     CASE ('ROTLATLON')
       CALL GRIB_GET(IGRIB,'Nj',NRY,IRET)
       CALL GRIB_GET(IGRIB,'Ni',NRX,IRET) 
       NNI = NRX * NRY

     CASE DEFAULT
       CALL ABOR1_SFX('PREP_GRIB_GRID: GRID PROJECTION NOT SUPPORTED')
     !
END SELECT
!
!---------------------------------------------------------------------------------------
!* 4.  Updates grid information
!---------------------------------------------------------------------------------------
!
XX    = XUNDEF
XY    = XUNDEF
XILA1 = XUNDEF
XILO1 = XUNDEF
XILA2 = XUNDEF
XILO2 = XUNDEF
LROTPOLE = .FALSE.
XCOEF = XUNDEF
XLAP  = XUNDEF
XLOP  = XUNDEF
   
SELECT CASE (HGRIDTYPE)

     CASE ('AROME     ')  
     ! 4.1 Lambert conformal projection (ALADIN files)
     !
       CALL GRIB_GET(IGRIB,'xDirectionGridLength',ILENX)
       CALL GRIB_GET(IGRIB,'yDirectionGridLength',ILENY)  
       XY = (NY-1)*ILENY
       XX = (NX-1)*ILENX

       CALL GRIB_GET(IGRIB,'Latin1InDegrees',XLAT0) 
       CALL GRIB_GET(IGRIB,'LoVInDegrees',XLON0)      
       IF (XLON0 > 180.) XLON0 = XLON0 - 360.

       CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XLATOR)
       CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XLONOR)
       IF (XLONOR > 180.) XLONOR = XLONOR - 360.

       XRPK  = SIN(XLAT0/180.*XPI)
       XBETA = 0.
     !
     CASE ('GAUSS     ','LATLON    ')
       HGRIDTYPE = 'GAUSS     '        
     ! 4.2 Usual or Gaussian lat,lon grid (ECMWF files)
     !     No projection - just stores the grid definition
     !
       CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XILA1)
       CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XILO1)
       CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XILA2)
       CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XILO2)

       LROTPOLE = .FALSE.
     !
     CASE ('ROTLATLON ')
     !
     ! 4.2.5 Rotated lat/lon grid (HIRLAM)
     !
       CALL GRIB_GET(IGRIB,'iScansNegatively',ISCAN)
       CALL GRIB_GET(IGRIB,'jScansNegatively',JSCAN)

       IF (ISCAN+JSCAN == 0 ) THEN
         CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XRILA2)  
         CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XRILO1)
         CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XRILA1)         
         CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XRILO2)
       ELSEIF (ISCAN+JSCAN == 2) THEN
         CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XRILA1)   
         CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XRILO2)
         CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XRILA2)                 
         CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XRILO1)
       ELSEIF (ISCAN == 1) THEN
         CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XRILA2)   
         CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XRILO2)
         CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XRILA1)                 
         CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XRILO1)
       ELSEIF (JSCAN == 1) THEN
         CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XRILA1)   
         CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XRILO1)
         CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XRILA2)                 
         CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XRILO2)         
       ENDIF

       CALL GRIB_GET(IGRIB,'latitudeOfSouthernPoleInDegrees',XRLAP)                 
       CALL GRIB_GET(IGRIB,'longitudeOfSouthernPoleInDegrees',XRLOP)

       CALL GRIB_GET(IGRIB,'iDirectionIncrementInDegrees',XRDX)                 
       CALL GRIB_GET(IGRIB,'jDirectionIncrementInDegrees',XRDY)

       WRITE(KLUOUT,*)'XRILA1,XRILO1',XRILA1,XRILO1
       WRITE(KLUOUT,*)'XRILA2,XRILO2',XRILA2,XRILO2
       WRITE(KLUOUT,*)'XRLAP,XRLOP',XRLAP,XRLOP
       WRITE(KLUOUT,*)'XRDX,XRDY',XRDX,XRDY
     !
     CASE ('ROTGAUSS  ')
     ! 4.3 Stretched lat,lon grid (Arpege files)
     !
       HGRIDTYPE = 'GAUSS     '
       CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XILA1)
       CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XILO1)
       CALL GRIB_GET(IGRIB,'latitudeOfLastGridPointInDegrees',XILA2)
       CALL GRIB_GET(IGRIB,'longitudeOfLastGridPointInDegrees',XILO2)
     
       LROTPOLE = .TRUE.
       CALL GRIB_GET(IGRIB,'stretchingFactor',XCOEF)
       CALL GRIB_GET(IGRIB,'latitudeOfStretchingPoleInDegrees',XLAP)
       CALL GRIB_GET(IGRIB,'longitudeOfStretchingPoleInDegrees',XLOP)
     !
     
     CASE ('MERCATOR  ')  
     ! 4.4 Mercator  projection (ALADIN Reunion files)
     !
       HGRIDTYPE = 'AROME     '     
       CALL GRIB_GET(IGRIB,'Dj',ILENY)     
       CALL GRIB_GET(IGRIB,'Di',ILENX)
       XY = (NY-1)*ILENY
       XX = (NX-1)*ILENX

       CALL GRIB_GET(IGRIB,'LaDInDegrees',XLAT0)       
       XLON0 = 0.

       CALL GRIB_GET(IGRIB,'latitudeOfFirstGridPointInDegrees',XLATOR)
       CALL GRIB_GET(IGRIB,'longitudeOfFirstGridPointInDegrees',XLONOR)
       IF (XLONOR > 180.) XLONOR = XLONOR - 360.

       XRPK  = 0.
       XBETA = 0.

     CASE DEFAULT
       WRITE (KLUOUT,'(A)') 'No such projection implemented in prep_grib_grid ',HGRID
       CALL ABOR1_SFX('PREP_GRIB_GRID: UNKNOWN PROJECTION')
     !
END SELECT
!---------------------------------------------------------------------------------------
!* 2.4 Read date
!---------------------------------------------------------------------------------------
!
WRITE (KLUOUT,'(A)') ' | Reading date'
!
 CALL GRIB_GET(IGRIB,'year',TPTIME_GRIB%TDATE%YEAR,IRET)
 CALL GRIB_GET(IGRIB,'month',TPTIME_GRIB%TDATE%MONTH,IRET)
 CALL GRIB_GET(IGRIB,'day',TPTIME_GRIB%TDATE%DAY,IRET)
 CALL GRIB_GET(IGRIB,'time',ITIME,IRET)
TPTIME_GRIB%TIME=INT(ITIME/100)*3600+(ITIME-INT(ITIME/100)*100)*60
!  
 CALL GRIB_GET(IGRIB,'P1',IP1,IRET)
IF ( IP1>0 ) THEN
  CALL GRIB_GET(IGRIB,'unitOfTimeRange',IUNITTIME,IRET)      
  SELECT CASE (IUNITTIME)       ! Time unit indicator
    CASE (1)                    !hour
      TPTIME_GRIB%TIME   = TPTIME_GRIB%TIME + IP1*3600.
    CASE (0)                    !minute
      TPTIME_GRIB%TIME   = TPTIME_GRIB%TIME + IP1*60.
  END SELECT
ENDIF
!
!---------------------------------------------------------------------------------------
!
 CALL GRIB_RELEASE(IGRIB,IRET)
IF (IRET /= 0) THEN
  CALL ABOR1_SFX('PREP_GRIB_GRID: Error in releasing the grib message memory')
END IF
!
IF (LHOOK) CALL DR_HOOK('PREP_GRIB_GRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE PREP_GRIB_GRID
