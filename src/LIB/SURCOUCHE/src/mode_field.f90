!MNH_LIC Copyright 2016-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Original version:
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
! Modifications:
!  P. Wautelet 29/01/2019: small bug correction (null pointers) in FIELDLIST_GOTO_MODEL if NESPGD or PGD
!  P. Wautelet 01/02/2019: bug correction in case XRT is not associated
!  C. Lac         02/2019: add rain fraction as an output field
!  S. Bielli      02/2019: sea salt: significant sea wave height influences salt emission; 5 salt modes
!  P. Wautelet 06/03/2019: correct ZWS entry
!  P. Wautelet 06/06/2019: bug correction in FIELDLIST_GOTO_MODEL (XLSTHM was overwritten if LUSERV=.FALSE. due to wrong IF block)
!  P. Wautelet 19/06/2019: add Fieldlist_nmodel_resize subroutine + provide KMODEL to INI_FIELD_LIST when known
!  P. Wautelet 23/01/2020: split in modd_field.f90 and mode_field.f90
!  JL Redelsperger 03/2021: add variables for Ocean LES and auto-coupled version
!  P. Wautelet 08/10/2021: add Goto_model_1field + Add_field2list procedures
!-----------------------------------------------------------------
module mode_field

use modd_conf,       only: cprogram
use modd_field
use modd_io,         only: NVERB_DEBUG, NVERB_INFO, NVERB_WARNING, NVERB_ERROR, NVERB_FATAL
use modd_parameters, only: JPMODELMAX

use mode_msg

implicit none

private

public :: Ini_field_list
public :: Find_field_id_from_mnhname
public :: Alloc_field_scalars
public :: Fieldlist_goto_model
public :: Fieldlist_nmodel_resize
public :: Ini_field_scalars

interface Goto_model_1field
  module procedure :: Goto_model_1field_c0d
  module procedure :: Goto_model_1field_c1d
  module procedure :: Goto_model_1field_l0d
  module procedure :: Goto_model_1field_l1d
  module procedure :: Goto_model_1field_n0d
  module procedure :: Goto_model_1field_n1d
  module procedure :: Goto_model_1field_n2d
  module procedure :: Goto_model_1field_n3d
  module procedure :: Goto_model_1field_t0d
  module procedure :: Goto_model_1field_t1d
  module procedure :: Goto_model_1field_x0d
  module procedure :: Goto_model_1field_x1d
  module procedure :: Goto_model_1field_x2d
  module procedure :: Goto_model_1field_x3d
  module procedure :: Goto_model_1field_x4d
  module procedure :: Goto_model_1field_x5d
  module procedure :: Goto_model_1field_x6d
end interface


contains

SUBROUTINE INI_FIELD_LIST(KMODEL)
! Modif
!  J.Escobar 25/04/2018: missing def of FRC
!------------------------------------------------
USE MODD_CONF, ONLY: NMODEL
!
INTEGER,INTENT(IN),OPTIONAL :: KMODEL
!
INTEGER :: IMODEL
CHARACTER(LEN=42) :: YMSG
!
!F90/95: TFIELDLIST(1) = TFIELDDATA('UT','x_wind','m s-1','XY','X_Y_Z_U component of wind',2)
!F2003:
!TFIELDLIST(1) = TFIELDDATA(CMNHNAME='UT',CSTDNAME='x_wind',CUNITS='m s-1',CDIR='XY',&
!                           CCOMMENT='X_Y_Z_U component of wind',NGRID=2)
!
CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_LIST','called')
IF (LFIELDLIST_ISINIT) THEN
  CALL PRINT_MSG(NVERB_ERROR,'GEN','INI_FIELD_LIST','already called')
  RETURN
END IF
LFIELDLIST_ISINIT = .TRUE.
!
IF (PRESENT(KMODEL)) THEN
  IMODEL = KMODEL
ELSE
  !NMODEL is not necessary known here => allocating for max allowed number of models
  !WARNING: if known, the value could change after this subroutine (ie for a restart
  !         with more models) because READ_DESFM_n is called before READ_EXSEG_n
  !Structures can be resized with a call to Fieldlist_nmodel_resize
  IMODEL = JPMODELMAX
END IF
!
IF (IMODEL==0) CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_FIELD_LIST','allocating fields for zero models not allowed')
if ( imodel > JPMODELMAX ) &
  call Print_msg( NVERB_FATAL, 'GEN', 'INI_FIELD_LIST', 'allocating fields for more than JPMODELMAX models not allowed' )
!
WRITE(YMSG,'("allocating fields for up to ",I4," model(s)")') IMODEL
CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_LIST',YMSG)
!
NMODEL_ALLOCATED = IMODEL
!
call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'MNHVERSION',     &
  CSTDNAME   = '',               &
  CLONGNAME  = 'MesoNH version', &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = '',               &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 1,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA(                  &
  CMNHNAME   = 'MASDEV',                          &
  CSTDNAME   = '',                                &
  CLONGNAME  = 'MesoNH version (without bugfix)', &
  CUNITS     = '',                                &
  CDIR       = '--',                              &
  CCOMMENT   = '',                                &
  NGRID      = 0,                                 &
  NTYPE      = TYPEINT,                           &
  NDIMS      = 0,                                 &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA(       &
  CMNHNAME   = 'BUGFIX',               &
  CSTDNAME   = '',                     &
  CLONGNAME  = 'MesoNH bugfix number', &
  CUNITS     = '',                     &
  CDIR       = '--',                   &
  CCOMMENT   = '',                     &
  NGRID      = 0,                      &
  NTYPE      = TYPEINT,                &
  NDIMS      = 0,                      &
  LTIMEDEP   = .FALSE.                 ) )

call Add_field2list( TFIELDDATA(              &
  CMNHNAME   = 'BIBUSER',                     &
  CSTDNAME   = '',                            &
  CLONGNAME  = 'MesoNH: user binary library', &
  CUNITS     = '',                            &
  CDIR       = '--',                          &
  CCOMMENT   = '',                            &
  NGRID      = 0,                             &
  NTYPE      = TYPECHAR,                      &
  NDIMS      = 0,                             &
  LTIMEDEP   = .FALSE.                        ) )

call Add_field2list( TFIELDDATA(               &
  CMNHNAME   = 'VERSION',                      &
  CSTDNAME   = '',                             &
  CLONGNAME  = 'SURFEX version (without BUG)', &
  CUNITS     = '',                             &
  CDIR       = '--',                           &
  CCOMMENT   = '',                             &
  NGRID      = 0,                              &
  NTYPE      = TYPEINT,                        &
  NDIMS      = 0,                              &
  LTIMEDEP   = .FALSE.                         ) )

call Add_field2list( TFIELDDATA(       &
  CMNHNAME   = 'BUG',                  &
  CSTDNAME   = '',                     &
  CLONGNAME  = 'SURFEX bugfix number', &
  CUNITS     = '',                     &
  CDIR       = '--',                   &
  CCOMMENT   = '',                     &
  NGRID      = 0,                      &
  NTYPE      = TYPEINT,                &
  NDIMS      = 0,                      &
  LTIMEDEP   = .FALSE.                 ) )

call Add_field2list( TFIELDDATA(              &
  CMNHNAME   = 'PROGRAM',                     &
  CSTDNAME   = '',                            &
  CLONGNAME  = 'MesoNH family: used program', &
  CUNITS     = '',                            &
  CDIR       = '--',                          &
  CCOMMENT   = '',                            &
  NGRID      = 0,                             &
  NTYPE      = TYPECHAR,                      &
  NDIMS      = 0,                             &
  LTIMEDEP   = .FALSE.                        ) )

call Add_field2list( TFIELDDATA(    &
  CMNHNAME   = 'FILETYPE',          &
  CSTDNAME   = '',                  &
  CLONGNAME  = 'type of this file', &
  CUNITS     = '',                  &
  CDIR       = '--',                &
  CCOMMENT   = '',                  &
  NGRID      = 0,                   &
  NTYPE      = TYPECHAR,            &
  NDIMS      = 0,                   &
  LTIMEDEP   = .FALSE.              ) )

call Add_field2list( TFIELDDATA(          &
  CMNHNAME   = 'MY_NAME',                 &
  CSTDNAME   = '',                        &
  CLONGNAME  = 'filename (no extension)', &
  CUNITS     = '',                        &
  CDIR       = '--',                      &
  CCOMMENT   = '',                        &
  NGRID      = 0,                         &
  NTYPE      = TYPECHAR,                  &
  NDIMS      = 0,                         &
  LTIMEDEP   = .FALSE.                    ) )

call Add_field2list( TFIELDDATA(           &
  CMNHNAME   = 'DAD_NAME',                 &
  CSTDNAME   = '',                         &
  CLONGNAME  = 'filename of the dad file', &
  CUNITS     = '',                         &
  CDIR       = '--',                       &
  CCOMMENT   = '',                         &
  NGRID      = 0,                          &
  NTYPE      = TYPECHAR,                   &
  NDIMS      = 0,                          &
  LTIMEDEP   = .FALSE.                     ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DXRATIO',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DXRATIO',        &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Resolution ratio between this mesh and its father in x-direction', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DYRATIO',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DYRATIO',        &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Resolution ratio between this mesh and its father in y-direction', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'XSIZE',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'XSIZE',          &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Number of model 1 grid points in x-direction in the model 2 physical domain', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'YSIZE',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'YSIZE',          &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Number of model 1 grid points in y-direction in the model 2 physical domain', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'XOR',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'XOR',            &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Horizontal position of this mesh relative to its father', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'YOR',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'YOR',            &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Vertical position of this mesh relative to its father', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'STORAGE_TYPE',   &
  CSTDNAME   = '',               &
  CLONGNAME  = 'STORAGE_TYPE',   &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Storage type for the information written in the FM files', &
  NGRID      = 0,                &
  NTYPE      = TYPECHAR,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'IMAX',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'IMAX',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'x-dimension of the physical domain', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'JMAX',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'JMAX',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'y-dimension of the physical domain', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'KMAX',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'KMAX',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'z-dimension of the physical domain', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'JPHEXT',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'JPHEXT',         &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Number of horizontal external points on each side', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RPK',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RPK',            &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Projection parameter for conformal projection', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LONORI',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LONORI',         &
  CUNITS     = 'degree',         &
  CDIR       = '--',             &
  CCOMMENT   = 'Longitude of the point of coordinates x=0, y=0 for conformal projection', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LATORI',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LATORI',         &
  CUNITS     = 'degree',         &
  CDIR       = '--',             &
  CCOMMENT   = 'Latitude of the point of coordinates x=0, y=0 for conformal projection', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LONOR',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LONOR',          &
  CUNITS     = 'degree',         &
  CDIR       = '--',             &
  CCOMMENT   = 'Longitude of 1st mass point', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LATOR',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LATOR',          &
  CUNITS     = 'degree',         &
  CDIR       = '--',             &
  CCOMMENT   = 'Latitude of 1st mass point', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'THINSHELL',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'THINSHELL',      &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for thinshell approximation', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LAT0',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LAT0',           &
  CUNITS     = 'degree',         &
  CDIR       = '--',             &
  CCOMMENT   = 'Reference latitude for conformal projection', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LON0',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LON0',           &
  CUNITS     = 'degree',         &
  CDIR       = '--',             &
  CCOMMENT   = 'Reference longitude for conformal projection', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'BETA',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'BETA',           &
  CUNITS     = 'degree',         &
  CDIR       = '--',             &
  CCOMMENT   = 'Rotation angle for conformal projection', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'XHAT',           &
!TODO: check stdname
  CSTDNAME   = 'projection_x_coordinate', &
  CLONGNAME  = 'XHAT',           &
  CUNITS     = 'm',              &
  CDIR       = 'XX',             &
  CCOMMENT   = 'Position x in the conformal or cartesian plane', &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 1,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'YHAT',           &
!TODO: check stdname
  CSTDNAME   = 'projection_y_coordinate', &
  CLONGNAME  = 'YHAT',           &
  CUNITS     = 'm',              &
  CDIR       = 'YY',             &
  CCOMMENT   = 'Position y in the conformal or cartesian plane', &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 1,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ZHAT',           &
!TODO: check stdname
  CSTDNAME   = '',               &
  CLONGNAME  = 'ZHAT',           &
  CUNITS     = 'm',              &
  CDIR       = 'ZZ',             &
  CCOMMENT   = 'Height level without orography', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 1,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA(      &
  CMNHNAME   = 'ZTOP',                &
  CSTDNAME   = 'altitude_at_top_of_atmosphere_model', &
  CLONGNAME  = 'ZTOP',                &
  CUNITS     = 'm',                   &
  CDIR       = '--',                  &
  CCOMMENT   = 'Height of top level', &
  NGRID      = 4,                     &
  NTYPE      = TYPEREAL,              &
  NDIMS      = 0,                     &
  LTIMEDEP   = .FALSE.                ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DXHAT',          &
!TODO: check stdname
  CSTDNAME   = '',               &
  CLONGNAME  = 'DXHAT',          &
  CUNITS     = 'm',              &
  CDIR       = 'XX',             &
  CCOMMENT   = 'Horizontal stretching in x', &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 1,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DYHAT',          &
!TODO: check stdname
  CSTDNAME   = '',               &
  CLONGNAME  = 'DYHAT',          &
  CUNITS     = 'm',              &
  CDIR       = 'YY',             &
  CCOMMENT   = 'Horizontal stretching in y', &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 1,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ALT',            &
  CSTDNAME   = 'altitude',       &
  CLONGNAME  = 'ALT',            &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_ALTitude', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DIRCOSXW',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DIRCOSXW',       &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X director cosinus of the normal to the ground surface', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DIRCOSYW',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DIRCOSYW',       &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Y director cosinus of the normal to the ground surface', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DIRCOSZW',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DIRCOSZW',       &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Z director cosinus of the normal to the ground surface', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'COSSLOPE',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'COSSLOPE',       &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'cosinus of the angle between i and the slope vector', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SINSLOPE',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SINSLOPE',       &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'sinus of the angle between i and the slope vector', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'MAP',            &
!TODO: check stdname
  CSTDNAME   = '',               &
  CLONGNAME  = 'MAP',            &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Map factor',     &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'latitude',       &
  CSTDNAME   = 'latitude',       &
  CLONGNAME  = 'latitude',       &
  CUNITS     = 'degrees_north',  &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_latitude at mass point', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'longitude',      &
  CSTDNAME   = 'longitude',      &
  CLONGNAME  = 'longitude',      &
  CUNITS     = 'degrees_east',   &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_longitude at mass point', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA(          &
  CMNHNAME   = 'latitude_u',              &
  CSTDNAME   = 'latitude_at_u_location',  &
  CLONGNAME  = 'latitude at u location',  &
  CUNITS     = 'degrees_north',           &
  CDIR       = 'XY',                      &
  CCOMMENT   = 'X_Y_latitude at u point', &
  NGRID      = 2,                         &
  NTYPE      = TYPEREAL,                  &
  NDIMS      = 2,                         &
  LTIMEDEP   = .FALSE.                    ) )

call Add_field2list( TFIELDDATA(           &
  CMNHNAME   = 'longitude_u',              &
  CSTDNAME   = 'longitude_at_u_location',  &
  CLONGNAME  = 'longitude at u location',  &
  CUNITS     = 'degrees_east',             &
  CDIR       = 'XY',                       &
  CCOMMENT   = 'X_Y_longitude at u point', &
  NGRID      = 2,                          &
  NTYPE      = TYPEREAL,                   &
  NDIMS      = 2,                          &
  LTIMEDEP   = .FALSE.                     ) )

call Add_field2list( TFIELDDATA(          &
  CMNHNAME   = 'latitude_v',              &
  CSTDNAME   = 'latitude_at_v_location',  &
  CLONGNAME  = 'latitude at v location',  &
  CUNITS     = 'degrees_north',           &
  CDIR       = 'XY',                      &
  CCOMMENT   = 'X_Y_latitude at v point', &
  NGRID      = 3,                         &
  NTYPE      = TYPEREAL,                  &
  NDIMS      = 2,                         &
  LTIMEDEP   = .FALSE.                    ) )

call Add_field2list( TFIELDDATA(           &
  CMNHNAME   = 'longitude_v',              &
  CSTDNAME   = 'longitude_at_v_location',  &
  CLONGNAME  = 'longitude at v location',  &
  CUNITS     = 'degrees_east',             &
  CDIR       = 'XY',                       &
  CCOMMENT   = 'X_Y_longitude at v point', &
  NGRID      = 3,                          &
  NTYPE      = TYPEREAL,                   &
  NDIMS      = 2,                          &
  LTIMEDEP   = .FALSE.                     ) )

call Add_field2list( TFIELDDATA(          &
  CMNHNAME   = 'latitude_f',              &
  CSTDNAME   = 'latitude_at_f_location',  &
  CLONGNAME  = 'latitude at f location',  &
  CUNITS     = 'degrees_north',           &
  CDIR       = 'XY',                      &
  CCOMMENT   = 'X_Y_latitude at f point', &
  NGRID      = 5,                         &
  NTYPE      = TYPEREAL,                  &
  NDIMS      = 2,                         &
  LTIMEDEP   = .FALSE.                    ) )

call Add_field2list( TFIELDDATA(           &
  CMNHNAME   = 'longitude_f',              &
  CSTDNAME   = 'longitude_at_f_location',  &
  CLONGNAME  = 'longitude at f location',  &
  CUNITS     = 'degrees_east',             &
  CDIR       = 'XY',                       &
  CCOMMENT   = 'X_Y_longitude at f point', &
  NGRID      = 5,                          &
  NTYPE      = TYPEREAL,                   &
  NDIMS      = 2,                          &
  LTIMEDEP   = .FALSE.                     ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LAT',            &
!   CSTDNAME   = 'latitude',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LAT',            &
  CUNITS     = 'degrees_north',  &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_latitude',   &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LON',            &
!   CSTDNAME   = 'longitude',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LON',            &
  CUNITS     = 'degrees_east',   &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_longitude',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA(   &
  CMNHNAME   = 'ZS',               &
  CSTDNAME   = 'surface_altitude', &
  CLONGNAME  = 'ZS',               &
  CUNITS     = 'm',                &
  CDIR       = 'XY',               &
  CCOMMENT   = 'orography',        &
  NGRID      = 4,                  &
  NTYPE      = TYPEREAL,           &
  NDIMS      = 2,                  &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA(  &
  CMNHNAME   = 'ZWS',             &
  CSTDNAME   = 'sea_surface_wave_significant_height', &
  CLONGNAME  = 'ZWS',             &
  CUNITS     = 'm',               &
  CDIR       = 'XY',              &
  CCOMMENT   = 'sea wave height', &
  NGRID      = 4,                 &
  NTYPE      = TYPEREAL,          &
  NDIMS      = 2,                 &
  LTIMEDEP   = .TRUE.             ) )

call Add_field2list( TFIELDDATA(   &
  CMNHNAME   = 'ZSMT',             &
  CSTDNAME   = '',                 &
  CLONGNAME  = 'ZSMT',             &
  CUNITS     = 'm',                &
  CDIR       = 'XY',               &
  CCOMMENT   = 'smooth orography', &
  NGRID      = 4,                  &
  NTYPE      = TYPEREAL,           &
  NDIMS      = 2,                  &
  LTIMEDEP   = .FALSE.             ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SLEVE',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SLEVE',          &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for SLEVE coordinate', &
  NGRID      = 4,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LEN1',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LEN1',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Decay scale for smooth topography', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LEN2',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LEN2',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Decay scale for small-scale topography deviation', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA(                      &
  CMNHNAME   = 'DTMOD',                               &
  CSTDNAME   = '',                                    &
  CLONGNAME  = 'DTMOD',                               &
  CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S', &
  CDIR       = '--',                                  &
  CCOMMENT   = 'Time and date of model beginning',    &
  NGRID      = 0,                                     &
  NTYPE      = TYPEDATE,                              &
  NDIMS      = 0,                                     &
  LTIMEDEP   = .FALSE.                                ) )

call Add_field2list( TFIELDDATA(                      &
  CMNHNAME   = 'DTCUR',                               &
  CSTDNAME   = 'time',                                &
  CLONGNAME  = 'DTCUR',                               &
  CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S', &
  CDIR       = '--',                                  &
  CCOMMENT   = 'Current time and date',               &
  NGRID      = 0,                                     &
  NTYPE      = TYPEDATE,                              &
  NDIMS      = 0,                                     &
  LTIMEDEP   = .FALSE.                                ) )

call Add_field2list( TFIELDDATA(                            &
  CMNHNAME   = 'DTRAD_FULL',                                &
  CSTDNAME   = '',                                          &
  CLONGNAME  = 'DTRAD_FULL',                                &
  CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S',       &
  CDIR       = '--',                                        &
  CCOMMENT   = 'Time and date of last full radiation call', &
  NGRID      = 0,                                           &
  NTYPE      = TYPEDATE,                                    &
  NDIMS      = 0,                                           &
  LTIMEDEP   = .FALSE.                                      ) )

call Add_field2list( TFIELDDATA(                      &
  CMNHNAME   = 'DTRAD_CLLY',                          &
  CSTDNAME   = '',                                    &
  CLONGNAME  = 'DTRAD_CLLY',                          &
  CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S', &
  CDIR       = '--',                                  &
  CCOMMENT   = 'Time and date of last radiation call for only cloudy verticals', &
  NGRID      = 0,                                     &
  NTYPE      = TYPEDATE,                              &
  NDIMS      = 0,                                     &
  LTIMEDEP   = .FALSE.                                ) )

call Add_field2list( TFIELDDATA(                                 &
  CMNHNAME   = 'DTDCONV',                                        &
  CSTDNAME   = '',                                               &
  CLONGNAME  = 'DTDCONV',                                        &
  CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S',            &
  CDIR       = '--',                                             &
  CCOMMENT   = 'Time and date of the last deep convection call', &
  NGRID      = 0,                                                &
  NTYPE      = TYPEDATE,                                         &
  NDIMS      = 0,                                                &
  LTIMEDEP   = .FALSE.                                           ) )

call Add_field2list( TFIELDDATA(                        &
  CMNHNAME   = 'DTEXP',                                 &
  CSTDNAME   = '',                                      &
  CLONGNAME  = 'DTEXP',                                 &
  CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S',   &
  CDIR       = '--',                                    &
  CCOMMENT   = 'Time and date of experiment beginning', &
  NGRID      = 0,                                       &
  NTYPE      = TYPEDATE,                                &
  NDIMS      = 0,                                       &
  LTIMEDEP   = .FALSE.                                  ) )

call Add_field2list( TFIELDDATA(                      &
  CMNHNAME   = 'DTSEG',                               &
  CSTDNAME   = '',                                    &
  CLONGNAME  = 'DTSEG',                               &
  CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S', &
  CDIR       = '--',                                  &
  CCOMMENT   = 'Time and date of segment beginning',  &
  NGRID      = 0,                                     &
  NTYPE      = TYPEDATE,                              &
  NDIMS      = 0,                                     &
  LTIMEDEP   = .FALSE.                                ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'L1D',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'L1D',            &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for 1D model version', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'L2D',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'L2D',            &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for 2D model version', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PACK',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PACK',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical to compress 1D or 2D FM files', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CARTESIAN',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CARTESIAN',      &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for cartesian geometry', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBOUSS',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBOUSS',         &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for Boussinesq approximation', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LOCEAN',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LOCEAN',         &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for Ocean MesoNH', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LCOUPLES',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LCOUPLES',       &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for coupling O-A LES', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SURF',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SURF',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Kind of surface processes parameterization', &
  NGRID      = 0,                &
  NTYPE      = TYPECHAR,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CPL_AROME',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CPL_AROME',      &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for AROME coupling file', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'COUPLING',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'COUPLING',       &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Logical for coupling file', &
  NGRID      = 0,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA(            &
  CMNHNAME   = 'UT',                        &
  CSTDNAME   = 'x_wind',                    &
  CLONGNAME  = 'UT',                        &
  CUNITS     = 'm s-1',                     &
  CDIR       = 'XY',                        &
  CCOMMENT   = 'X_Y_Z_U component of wind', &
  NGRID      = 2,                           &
  NTYPE      = TYPEREAL,                    &
  NDIMS      = 3,                           &
  LTIMEDEP   = .TRUE.                       ) )

call Add_field2list( TFIELDDATA(            &
  CMNHNAME   = 'VT',                        &
  CSTDNAME   = 'y_wind',                    &
  CLONGNAME  = 'VT',                        &
  CUNITS     = 'm s-1',                     &
  CDIR       = 'XY',                        &
  CCOMMENT   = 'X_Y_Z_V component of wind', &
  NGRID      = 3,                           &
  NTYPE      = TYPEREAL,                    &
  NDIMS      = 3,                           &
  LTIMEDEP   = .TRUE.                       ) )

call Add_field2list( TFIELDDATA(      &
  CMNHNAME   = 'WT',                  &
  CSTDNAME   = 'upward_air_velocity', &
  CLONGNAME  = 'WT',                  &
  CUNITS     = 'm s-1',               &
  CDIR       = 'XY',                  &
  CCOMMENT   = 'X_Y_Z_vertical wind', &
  NGRID      = 4,                     &
  NTYPE      = TYPEREAL,              &
  NDIMS      = 3,                     &
  LTIMEDEP   = .TRUE.                 ) )

call Add_field2list( TFIELDDATA(              &
  CMNHNAME   = 'THT',                         &
  CSTDNAME   = 'air_potential_temperature',   &
  CLONGNAME  = 'THT',                         &
  CUNITS     = 'K',                           &
  CDIR       = 'XY',                          &
  CCOMMENT   = 'X_Y_Z_potential temperature', &
  NGRID      = 1,                             &
  NTYPE      = TYPEREAL,                      &
  NDIMS      = 3,                             &
  LTIMEDEP   = .TRUE.                         ) )

call Add_field2list( TFIELDDATA(            &
  CMNHNAME   = 'UM',                        &
  CSTDNAME   = 'x_wind',                    &
  CLONGNAME  = 'UM',                        &
  CUNITS     = 'm s-1',                     &
  CDIR       = 'XY',                        &
  CCOMMENT   = 'X_Y_Z_U component of wind', &
  NGRID      = 2,                           &
  NTYPE      = TYPEREAL,                    &
  NDIMS      = 3,                           &
  LTIMEDEP   = .TRUE.                       ) )

call Add_field2list( TFIELDDATA(            &
  CMNHNAME   = 'VM',                        &
  CSTDNAME   = 'y_wind',                    &
  CLONGNAME  = 'VM',                        &
  CUNITS     = 'm s-1',                     &
  CDIR       = 'XY',                        &
  CCOMMENT   = 'X_Y_Z_V component of wind', &
  NGRID      = 3,                           &
  NTYPE      = TYPEREAL,                    &
  NDIMS      = 3,                           &
  LTIMEDEP   = .TRUE.                       ) )

call Add_field2list( TFIELDDATA(      &
  CMNHNAME   = 'WM',                  &
  CSTDNAME   = 'upward_air_velocity', &
  CLONGNAME  = 'WM',                  &
  CUNITS     = 'm s-1',               &
  CDIR       = 'XY',                  &
  CCOMMENT   = 'X_Y_Z_vertical wind', &
  NGRID      = 4,                     &
  NTYPE      = TYPEREAL,              &
  NDIMS      = 3,                     &
  LTIMEDEP   = .TRUE.                 ) )

call Add_field2list( TFIELDDATA(            &
  CMNHNAME   = 'DUM',                       &
  CSTDNAME   = 'x_wind',                    &
  CLONGNAME  = 'DUM',                       &
  CUNITS     = 'm s-1',                     &
  CDIR       = 'XY',                        &
  CCOMMENT   = 'X_Y_Z_U component of wind', &
  NGRID      = 2,                           &
  NTYPE      = TYPEREAL,                    &
  NDIMS      = 3,                           &
  LTIMEDEP   = .TRUE.                       ) )

call Add_field2list( TFIELDDATA(            &
  CMNHNAME   = 'DVM',                       &
  CSTDNAME   = 'y_wind',                    &
  CLONGNAME  = 'DVM',                       &
  CUNITS     = 'm s-1',                     &
  CDIR       = 'XY',                        &
  CCOMMENT   = 'X_Y_Z_V component of wind', &
  NGRID      = 3,                           &
  NTYPE      = TYPEREAL,                    &
  NDIMS      = 3,                           &
  LTIMEDEP   = .TRUE.                       ) )

call Add_field2list( TFIELDDATA(      &
  CMNHNAME   = 'DWM',                 &
  CSTDNAME   = 'upward_air_velocity', &
  CLONGNAME  = 'DWM',                 &
  CUNITS     = 'm s-1',               &
  CDIR       = 'XY',                  &
  CCOMMENT   = 'X_Y_Z_vertical wind', &
  NGRID      = 4,                     &
  NTYPE      = TYPEREAL,              &
  NDIMS      = 3,                     &
  LTIMEDEP   = .TRUE.                 ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'TKET',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'TKET',           &
  CUNITS     = 'm2 s-2',         &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Turbulent Kinetic Energy', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'TKEMS',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'TKEMS',          &
  CUNITS     = 'm2 s-3',         &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Turbulent Kinetic Energy adv source', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA(          &
  CMNHNAME   = 'PABST',                   &
  CSTDNAME   = 'air_pressure',            &
  CLONGNAME  = 'PABST',                   &
  CUNITS     = 'Pa',                      &
  CDIR       = 'XY',                      &
  CCOMMENT   = 'X_Y_Z_ABSolute Pressure', &
  NGRID      = 1,                         &
  NTYPE      = TYPEREAL,                  &
  NDIMS      = 3,                         &
  LTIMEDEP   = .TRUE.                     ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PHIT',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PHIT',           &
  CUNITS     = 'Pa',             &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Reduced Pressure Oce/Shallow conv', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RT',             &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RT',             &
  CUNITS     = 'kg kg-1',        &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Moist variables (rho Rn)', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 4,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA(           &
  CMNHNAME   = 'RVT',                      &
!TODO: check stdname
  CSTDNAME   = 'humidity_mixing_ratio',    &
  CLONGNAME  = 'RVT',                      &
  CUNITS     = 'kg kg-1',                  &
  CDIR       = 'XY',                       &
  CCOMMENT   = 'X_Y_Z_Vapor mixing Ratio', &
  NGRID      = 1,                          &
  NTYPE      = TYPEREAL,                   &
  NDIMS      = 3,                          &
  LTIMEDEP   = .TRUE.                      ) )

call Add_field2list( TFIELDDATA(           &
  CMNHNAME   = 'RCT',                      &
  CSTDNAME   = '',                         &
  CLONGNAME  = 'RCT',                      &
  CUNITS     = 'kg kg-1',                  &
  CDIR       = 'XY',                       &
  CCOMMENT   = 'X_Y_Z_Cloud mixing Ratio', &
  NGRID      = 1,                          &
  NTYPE      = TYPEREAL,                   &
  NDIMS      = 3,                          &
  LTIMEDEP   = .TRUE.                      ) )

call Add_field2list( TFIELDDATA(          &
  CMNHNAME   = 'RRT',                     &
  CSTDNAME   = '',                        &
  CLONGNAME  = 'RRT',                     &
  CUNITS     = 'kg kg-1',                 &
  CDIR       = 'XY',                      &
  CCOMMENT   = 'X_Y_Z_Rain mixing Ratio', &
  NGRID      = 1,                         &
  NTYPE      = TYPEREAL,                  &
  NDIMS      = 3,                         &
  LTIMEDEP   = .TRUE.                     ) )

call Add_field2list( TFIELDDATA(         &
  CMNHNAME   = 'RIT',                    &
!TODO: check stdname
  CSTDNAME   = 'cloud_ice_mixing_ratio', &
  CLONGNAME  = 'RIT',                    &
  CUNITS     = 'kg kg-1',                &
  CDIR       = 'XY',                     &
  CCOMMENT   = 'X_Y_Z_Ice mixing Ratio', &
  NGRID      = 1,                        &
  NTYPE      = TYPEREAL,                 &
  NDIMS      = 3,                        &
  LTIMEDEP   = .TRUE.                    ) )

call Add_field2list( TFIELDDATA(          &
  CMNHNAME   = 'RST',                     &
  CSTDNAME   = '',                        &
  CLONGNAME  = 'RST',                     &
  CUNITS     = 'kg kg-1',                 &
  CDIR       = 'XY',                      &
  CCOMMENT   = 'X_Y_Z_Snow mixing Ratio', &
  NGRID      = 1,                         &
  NTYPE      = TYPEREAL,                  &
  NDIMS      = 3,                         &
  LTIMEDEP   = .TRUE.                     ) )

call Add_field2list( TFIELDDATA(             &
  CMNHNAME   = 'RGT',                        &
  CSTDNAME   = '',                           &
  CLONGNAME  = 'RGT',                        &
  CUNITS     = 'kg kg-1',                    &
  CDIR       = 'XY',                         &
  CCOMMENT   = 'X_Y_Z_Graupel mixing Ratio', &
  NGRID      = 1,                            &
  NTYPE      = TYPEREAL,                     &
  NDIMS      = 3,                            &
  LTIMEDEP   = .TRUE.                        ) )

call Add_field2list( TFIELDDATA(          &
  CMNHNAME   = 'RHT',                     &
  CSTDNAME   = '',                        &
  CLONGNAME  = 'RHT',                     &
  CUNITS     = 'kg kg-1',                 &
  CDIR       = 'XY',                      &
  CCOMMENT   = 'X_Y_Z_Hail mixing Ratio', &
  NGRID      = 1,                         &
  NTYPE      = TYPEREAL,                  &
  NDIMS      = 3,                         &
  LTIMEDEP   = .TRUE.                     ) )

call Add_field2list( TFIELDDATA(        &
  CMNHNAME   = 'SUPSATMAX',             &
  CSTDNAME   = '',                      &
  CLONGNAME  = 'SUPSATMAX',             &
  CUNITS     = '',                      &
  CDIR       = 'XY',                    &
  CCOMMENT   = 'X_Y_Z_Supersaturation', &
  NGRID      = 1,                       &
  NTYPE      = TYPEREAL,                &
  NDIMS      = 3,                       &
  LTIMEDEP   = .TRUE.                   ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'NACT',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'NACT',           &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Nact',     &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SSPRO',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SSPRO',          &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Supersaturation', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'NPRO',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'NPRO',           &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Nact',     &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INPAP',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INPAP',          &
  CUNITS     = 'kg m-2 s-1',     &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous Precipitating Aerosol Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ACPAP',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'ACPAP',          &
  CUNITS     = 'kg m-2',         &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_ACcumulated Precipitating Aerosol Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'EFIELDU',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'EFIELDU',        &
  CUNITS     = 'V m-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_EFIELDU',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'EFIELDV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'EFIELDV',        &
  CUNITS     = 'V m-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_EFIELDV',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'EFIELDW',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'EFIELDW',        &
  CUNITS     = 'V m-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_EFIELDW',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'NI_IAGGS',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'NI_IAGGS',       &
  CUNITS     = 'C m-3 s-1',      &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_NI_IAGGS', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'NI_IDRYG',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'NI_IDRYG',       &
  CUNITS     = 'C m-3 s-1',      &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_NI_IDRYG', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'NI_SDRYG',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'NI_SDRYG',       &
  CUNITS     = 'C m-3 s-1',      &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_NI_SDRYG', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INDUC_CG',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INDUC_CG',       &
  CUNITS     = 'C m-3 s-1',      &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_INDUC_CG', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'TRIG_IC',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'TRIG_IC',        &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_FLASH_MAP_TRIG_IC', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'IMPACT_CG',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'IMPACT_CG',      &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_FLASH_MAP_IMPACT_CG', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'AREA_CG',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'AREA_CG',        &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_FLASH_MAP_2DAREA_CG', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'AREA_IC',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'AREA_IC',        &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_FLASH_MAP_2DAREA_IC', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'FLASH_3DCG',     &
  CSTDNAME   = '',               &
  CLONGNAME  = 'FLASH_3DCG',     &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_FLASH_MAP_3DCG', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'FLASH_3DIC',     &
  CSTDNAME   = '',               &
  CLONGNAME  = 'FLASH_3DIC',     &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_FLASH_MAP_3DIC', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PHC',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PHC',            &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'pH in cloud',    &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PHR',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PHR',            &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'pH in rain',     &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LSUM',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LSUM',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Large Scale U component', &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LSVM',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LSVM',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Large Scale V component', &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LSWM',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LSWM',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Large Scale vertical wind', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LSTHM',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LSTHM',          &
  CUNITS     = 'K',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Large Scale potential Temperature', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LSRVM',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LSRVM',          &
  CUNITS     = 'kg kg-1',        &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Large Scale Vapor Mixing Ratio', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RIMX',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RIMX',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Number of points in the lateral absorbing layer in the x direction', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RIMY',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RIMY',           &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Number of points in the lateral absorbing layer in the y direction', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'HORELAX_UVWTH',  &
  CSTDNAME   = '',               &
  CLONGNAME  = 'HORELAX_UVWTH',  &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Switch to activate the HOrizontal RELAXation', &
  NGRID      = 1,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBXUM',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBXUM',          &
  CUNITS     = 'm s-1',          &
!   CDIR       = ''
  CLBTYPE    = 'LBXU',           &
  CCOMMENT   = '2_Y_Z_LBXUM',    &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBXVM',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBXVM',          &
  CUNITS     = 'm s-1',          &
!   CDIR       = ''
  CLBTYPE    = 'LBX',            &
  CCOMMENT   = '2_Y_Z_LBXVM',    &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBXWM',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBXWM',          &
  CUNITS     = 'm s-1',          &
!   CDIR       = ''
  CLBTYPE    = 'LBX',            &
  CCOMMENT   = '2_Y_Z_LBXWM',    &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBYUM',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBYUM',          &
  CUNITS     = 'm s-1',          &
!   CDIR       = ''
  CLBTYPE    = 'LBY',            &
  CCOMMENT   = '2_Y_Z_LBYUM',    &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBYVM',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBYVM',          &
  CUNITS     = 'm s-1',          &
!   CDIR       = ''
  CLBTYPE    = 'LBYV',           &
  CCOMMENT   = '2_Y_Z_LBYVM',    &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBYWM',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBYWM',          &
  CUNITS     = 'm s-1',          &
!   CDIR       = ''
  CLBTYPE    = 'LBY',            &
  CCOMMENT   = '2_Y_Z_LBYWM',    &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBXTHM',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBXTHM',         &
  CUNITS     = 'K',              &
!   CDIR       = ''
  CLBTYPE    = 'LBX',            &
  CCOMMENT   = '2_Y_Z_LBXTHM',   &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBYTHM',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBYTHM',         &
  CUNITS     = 'K',              &
!   CDIR       = ''
  CLBTYPE    = 'LBY',            &
  CCOMMENT   = '2_Y_Z_LBYTHM',   &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'HORELAX_TKE',    &
  CSTDNAME   = '',               &
  CLONGNAME  = 'HORELAX_TKE',    &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Switch to activate the HOrizontal RELAXation', &
  NGRID      = 1,                &
  NTYPE      = TYPELOG,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBXTKEM',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBXTKEM',        &
  CUNITS     = 'm2 s-2',         &
!   CDIR       = ''
  CLBTYPE    = 'LBX',            &
  CCOMMENT   = '2_Y_Z_LBXTKEM',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'LBYTKEM',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'LBYTKEM',        &
  CUNITS     = 'm2 s-2',         &
!   CDIR       = ''
  CLBTYPE    = 'LBY',            &
  CCOMMENT   = '2_Y_Z_LBYTKEM',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DRYMASST',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DRYMASST',       &
  CUNITS     = 'kg',             &
  CDIR       = '--',             &
  CCOMMENT   = 'Total Dry Mass', &
  NGRID      = 0,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'BL_DEPTH',       &
  CSTDNAME   = '',               &
  CLONGNAME  = 'BL_DEPTH',       &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_BL_DEPTH',   &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SBL_DEPTH',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SBL_DEPTH',      &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_BL_SDEPTH',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'WTHVMF',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'WTHVMF',         &
  CUNITS     = 'm K s-1',        &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_WTHVMF',     &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SRCT',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SRCT',           &
  CUNITS     = 'kg kg-2',        &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_normalized 2nd_order moment s_r_c/2Sigma_s2', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SIGS',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SIGS',           &
  CUNITS     = 'kg kg-2',        &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Sigma_s from turbulence scheme', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RHOREFZ',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RHOREFZ',        &
  CUNITS     = 'kg m-3',         &
  CDIR       = 'ZZ',             &
  CCOMMENT   = 'rhodz for reference state without orography', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 1,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'THVREFZ',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'THVREFZ',        &
  CUNITS     = 'K',              &
  CDIR       = 'ZZ',             &
  CCOMMENT   = 'thetavz for reference state without orography', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 1,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'EXNTOP',         &
  CSTDNAME   = 'dimensionless_exner_function', &
  CLONGNAME  = 'EXNTOP',         &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Exner function at model top', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

  IF (TRIM(CPROGRAM) == 'MESONH' .OR. TRIM(CPROGRAM) == 'DIAG' .OR. TRIM(CPROGRAM) == 'LFICDF') THEN

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'US_PRES',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'US_PRES',        &
!TODO: units?
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_US_PRES',  &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'VS_PRES',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'VS_PRES',        &
!TODO: units?
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_VS_PRES',  &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'WS_PRES',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'WS_PRES',        &
!TODO: units?
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_WS_PRES',  &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'THS_CLD',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'THS_CLD',        &
!TODO: units?
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_THS_CLD',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RS_CLD',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RS_CLD',         &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Source of Moist variables', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 4,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RVS_CLD',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RVS_CLD',        &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_RVS_CLD',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RCS_CLD',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RCS_CLD',        &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_RCS_CLD',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RRS_CLD',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RRS_CLD',        &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_RRS_CLD',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RIS_CLD',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RIS_CLD',        &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_RIS_CLD',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RSS_CLD',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RSS_CLD',        &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_RSS_CLD',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RGS_CLD',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RGS_CLD',        &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_RGS_CLD',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RHS_CLD',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RHS_CLD',        &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_RHS_CLD',  &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CLDFR',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CLDFR',          &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_CLouD FRaction', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CIT',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CIT',            &
  CUNITS     = 'm-3',            &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Cloud Ice concentration', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA(      &
  CMNHNAME   = 'RAINFR',              &
  CSTDNAME   = '',                    &
  CLONGNAME  = 'RAINFR',              &
  CUNITS     = '1',                   &
  CDIR       = 'XY',                  &
  CCOMMENT   = 'X_Y_Z_Rain FRaction', &
  NGRID      = 1,                     &
  NTYPE      = TYPEREAL,              &
  NDIMS      = 3,                     &
  LTIMEDEP   = .TRUE.                 ) )
!
END IF ! CPROGRAM=MESONH .OR. DIAG .OR. LFICDF
!
!
call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RHODREF',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RHODREF',        &
  CUNITS     = 'kg m-3',         &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Dry density for reference state with orography', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'THVREF',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'THVREF',         &
  CUNITS     = 'K',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Thetav for reference state with orography', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )
!
!
IF (     TRIM(CPROGRAM) =='MESONH' .OR. TRIM(CPROGRAM) == 'DIAG'  &
    .OR. TRIM(CPROGRAM) == 'LFICDF'.OR. TRIM(CPROGRAM) == 'SPAWN' ) THEN
!
call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DTHRAD',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DTHRAD',         &
  CUNITS     = 'K s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_RADiative heating/cooling rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'FLALWD',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'FLALWD',         &
  CUNITS     = 'W m-2',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Downward Long Waves on FLAT surface', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DIRFLASWD',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DIRFLASWD',      &
  CUNITS     = 'W m-2',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_DIRect Downward Short Waves on FLAT surface', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NSWB ], &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SCAFLASWD',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SCAFLASWD',      &
  CUNITS     = 'W m-2',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_SCAttered Downward Short Waves on FLAT surface', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NSWB ], &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DIRSRFSWD',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DIRSRFSWD',      &
  CUNITS     = 'W m-2',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_DIRect Downward Short Waves', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NSWB ], &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CLEARCOL_TM1',   &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CLEARCOL_TM1',   &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'TRACE OF CLOUD', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ZENITH',         &
  CSTDNAME   = 'zenith_angle',   &
  CLONGNAME  = 'ZENITH',         &
  CUNITS     = 'rad',            &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_ZENITH',     &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'AZIM',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'AZIM',           &
  CUNITS     = 'rad',            &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_AZIMuth',    &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA(    &
  CMNHNAME   = 'DIR_ALB',           &
  CSTDNAME   = '',                  &
  CLONGNAME  = 'DIR_ALB',           &
  CUNITS     = '1',                 &
  CDIR       = 'XY',                &
  CCOMMENT   = 'X_Y_DIRect ALBedo', &
  NGRID      = 1,                   &
  NTYPE      = TYPEREAL,            &
  NDIMS      = 3,                   &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NSWB ], &
  LTIMEDEP   = .TRUE.               ) )

call Add_field2list( TFIELDDATA(       &
  CMNHNAME   = 'SCA_ALB',              &
  CSTDNAME   = '',                     &
  CLONGNAME  = 'SCA_ALB',              &
  CUNITS     = '1',                    &
  CDIR       = 'XY',                   &
  CCOMMENT   = 'X_Y_SCAttered ALBedo', &
  NGRID      = 1,                      &
  NTYPE      = TYPEREAL,               &
  NDIMS      = 3,                      &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NSWB ], &
  LTIMEDEP   = .TRUE.                  ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'EMIS',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'EMIS',           &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_EMISsivity', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_NLWB ], &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'TSRAD',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'TSRAD',          &
  CUNITS     = 'K',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_RADiative Surface Temperature', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )
!
END IF !CPROGRAM=MESONH .OR. DIAG .OR. LFICDF .OR. SPAWN
!
!
IF ( TRIM(CPROGRAM) /= 'PGD' .AND. TRIM(CPROGRAM) /= 'NESPGD' ) THEN
!
call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'COUNTCONV',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'COUNTCONV',      &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_COUNTCONV',  &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DTHCONV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DTHCONV',        &
  CUNITS     = 'K s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_CONVective heating/cooling rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DRVCONV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DRVCONV',        &
  CUNITS     = 's-1',            &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_CONVective R_v tendency', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DRCCONV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DRCCONV',        &
  CUNITS     = 's-1',            &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_CONVective R_c tendency', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DRICONV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DRICONV',        &
  CUNITS     = 's-1',            &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_CONVective R_i tendency', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PRCONV',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PRCONV',         &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_CONVective instantaneous Precipitation Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PACCONV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PACCONV',        &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_CONVective ACcumulated Precipitation rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PRSCONV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PRSCONV',        &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_CONVective instantaneous Precipitation Rate for Snow', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA(    &
  CMNHNAME   = 'DSVCONV',           &
  CSTDNAME   = '',                  &
  CLONGNAME  = 'DSVCONV',           &
  CUNITS     = 's-1',               &
  CDIR       = 'XY',                &
  CCOMMENT   = 'Tracer tendencies', &
  NGRID      = 1,                   &
  NTYPE      = TYPEREAL,            &
  NDIMS      = 4,                   &
  LTIMEDEP   = .TRUE.               ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PRLFLXCONV',     &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PRLFLXCONV',     &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Liquid Precipitation Convective Flux', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PRSFLXCONV',     &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PRSFLXCONV',     &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Solid Precipitation Convective Flux', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'UMFCONV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'UMFCONV',        &
  CUNITS     = 'kg s-1 m-2',     &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Updraft Convective Mass Flux', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'DMFCONV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'DMFCONV',        &
  CUNITS     = 'kg s-1 m-2',     &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Downdraft Convective Mass Flux', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'MFCONV',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'MFCONV',         &
  CUNITS     = 'kg s-1 m-2',     &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Convective Mass Flux', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CAPE',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CAPE',           &
  CUNITS     = 'J kg-1',         &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Convective Available Potentiel Energy', &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CLTOPCONV_LVL',  &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CLTOPCONV_LVL',  &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Convective cloud top level', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CLBASCONV_LVL',  &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CLBASCONV_LVL',  &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'Convective cloud base level', &
  NGRID      = 1,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'IC_RATE',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'IC_RATE',        &
  CUNITS     = 's-1',            &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_IntraCloud lightning Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CG_RATE',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CG_RATE',        &
  CUNITS     = 's-1',            &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_CloudGround lightning Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'IC_TOTAL_NB',    &
  CSTDNAME   = '',               &
  CLONGNAME  = 'IC_TOTAL_NB',    &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_IntraCloud lightning Number', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'CG_TOTAL_NB',    &
  CSTDNAME   = '',               &
  CLONGNAME  = 'CG_TOTAL_NB',    &
  CUNITS     = '1',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_CloudGround lightning Number', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )
!
END IF !CPROGRAM/=PGD , NESPGD
!
!
call Add_field2list( TFIELDDATA(     &
  CMNHNAME   = 'SSO_ANIS',           &
  CSTDNAME   = '',                   &
  CLONGNAME  = 'SSO_ANIS',           &
  CUNITS     = 'm',                  &
  CDIR       = 'XY',                 &
  CCOMMENT   = 'X_Y_SSO_ANISOTROPY', &
  NGRID      = 4,                    &
  NTYPE      = TYPEREAL,             &
  NDIMS      = 2,                    &
  LTIMEDEP   = .FALSE.               ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SSO_SLOPE',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SSO_SLOPE',      &
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_SSO_SLOPE',  &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SSO_DIR',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SSO_DIR',        &
  CUNITS     = 'degree',         &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_SSO_DIR',    &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'AVG_ZS',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'AVG_ZS',         &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_AVG_ZS',     &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SIL_ZS',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SIL_ZS',         &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_SIL_ZS',     &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'MAX_ZS',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'MAX_ZS',         &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_MAX_ZS',     &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'MIN_ZS',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'MIN_ZS',         &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_MIN_ZS',     &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'SSO_STDEV',      &
  CSTDNAME   = '',               &
  CLONGNAME  = 'SSO_STDEV',      &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_SSO_STDEV',  &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INPRC',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INPRC',          &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous Cloud Precipitation Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ACPRC',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'ACPRC',          &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_ACcumulated Cloud Precipitation Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INDEP',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INDEP',          &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous Cloud Deposition Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ACDEP',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'ACDEP',          &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_ACcumulated Cloud Deposition Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INPRR',          &
  CSTDNAME   = 'rainfall_rate',  &
  CLONGNAME  = 'INPRR',          &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous Precipitation Rain Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INPRR3D',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INPRR3D',        &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous 3D Rain Precipitation flux', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'EVAP3D',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'EVAP3D',         &
  CUNITS     = 'kg kg-1 s-1',    &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous 3D Rain Evaporation flux', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA(                          &
  CMNHNAME   = 'ACPRR',                                   &
  CSTDNAME   = 'thickness_of_rainfall_amount',            &
  CLONGNAME  = 'ACPRR',                                   &
  CUNITS     = 'm',                                       &
  CDIR       = 'XY',                                      &
  CCOMMENT   = 'X_Y_ACcumulated Precipitation Rain Rate', &
  NGRID      = 1,                                         &
  NTYPE      = TYPEREAL,                                  &
  NDIMS      = 2,                                         &
  LTIMEDEP   = .TRUE.                                     ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INPRS',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INPRS',          &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous PRecipitation Snow Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ACPRS',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'ACPRS',          &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_ACcumulated PRecipitation Snow Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INPRG',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INPRG',          &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous PRecipitation Graupel Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ACPRG',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'ACPRG',          &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_ACcumulated PRecipitation Graupel Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INPRH',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INPRH',          &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_INstantaneous PRecipitation Hail Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ACPRH',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'ACPRH',          &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_ACcumulated PRecipitation Hail Rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'INPRT',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'INPRT',          &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Total INstantaneaous PRecipitation rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )
!No permanent variable associated to this field

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'ACPRT',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'ACPRT',          &
  CUNITS     = 'm',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Total ACcumulated PRecipitation rate', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .TRUE.            ) )
!No permanent variable associated to this field

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'VT_FLX',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'VT_FLX',         &
  CUNITS     = 'K m s-1',        &
  CDIR       = 'XY',             &
  CCOMMENT   = '',               &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'WT_FLX',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'WT_FLX',         &
  CUNITS     = 'K m s-1',        &
  CDIR       = 'XY',             &
  CCOMMENT   = '',               &
  NGRID      = 4,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RTHS_EDDY_FLUX', &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RTHS_EDDY_FLUX', &
!TODO PW: units?
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = '',               &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'VU_FLX',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'VU_FLX',         &
  CUNITS     = 'm s-2',          &
  CDIR       = 'XY',             &
  CCOMMENT   = '',               &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'RVS_EDDY_FLUX',  &
  CSTDNAME   = '',               &
  CLONGNAME  = 'RVS_EDDY_FLUX',  &
!TODO PW: units?
  CUNITS     = '',               &
  CDIR       = 'XY',             &
  CCOMMENT   = '',               &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .TRUE.            ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'FRC',            &
  CSTDNAME   = '',               &
  CLONGNAME  = 'FRC',            &
  CUNITS     = '',               &
  CDIR       = '--',             &
  CCOMMENT   = 'Number of forcing profiles', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
IF (TRIM(CPROGRAM)=='REAL' .OR. TRIM(CPROGRAM) == 'LFICDF') THEN
!PW: not yet known: IF (LFILTERING) THEN
!
call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'UT15',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'UT15',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_U component of Total wind', &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'VT15',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'VT15',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_V component of Total wind', &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'TEMPTOT',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'TEMPTOT',        &
  CUNITS     = 'K',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_TOTal TEMPerature', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PRESTOT',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PRESTOT',        &
  CUNITS     = 'Pa',             &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_TOTal PRESsure', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'HUMTOT',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'HUMTOT',         &
  CUNITS     = 'kg kg-1',        &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_TOTal specific HUMidity', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'UT16',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'UT16',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_U component of Environmental wind', &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'VT16',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'VT16',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_V component of Environmental wind', &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'TEMPENV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'TEMPENV',        &
  CUNITS     = 'K',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_ENVironmental TEMPerature', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PRESENV',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PRESENV',        &
  CUNITS     = 'Pa',             &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_ENVironmental PRESsure', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 2,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'HUMENV',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'HUMENV',         &
  CUNITS     = 'kg kg-1',        &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_ENVironmental specific HUMidity', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'UT17',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'UT17',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_U component of Basic wind', &
  NGRID      = 2,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'VT17',           &
  CSTDNAME   = '',               &
  CLONGNAME  = 'VT17',           &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_V component of Basic wind', &
  NGRID      = 3,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'TEMPBAS',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'TEMPBAS',        &
  CUNITS     = 'K',              &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_BASic TEMPerature', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'PRESBAS',        &
  CSTDNAME   = '',               &
  CLONGNAME  = 'PRESBAS',        &
  CUNITS     = 'Pa',             &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_BASic PRESsure', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'HUMBAS',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'HUMBAS',         &
  CUNITS     = 'kg kg-1',        &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_BASic specific HUMidity', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'VTDIS',          &
  CSTDNAME   = '',               &
  CLONGNAME  = 'VTDIS',          &
  CUNITS     = 'm s-1',          &
  CDIR       = 'XY',             &
  CCOMMENT   = 'X_Y_Z_Total disturbance tangential wind', &
  NGRID      = 1,                &
  NTYPE      = TYPEREAL,         &
  NDIMS      = 3,                &
  LTIMEDEP   = .FALSE.           ) )
!
!END IF !LFILTERING
END IF !CPROGRAM==REAL .OR. LFICDF
!
call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'NFRCLT',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'NFRCLT',         &
  CUNITS     = '1',              &
  CDIR       = '--',             &
  CCOMMENT   = 'number of sea surface forcings + 1', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )

call Add_field2list( TFIELDDATA( &
  CMNHNAME   = 'NINFRT',         &
  CSTDNAME   = '',               &
  CLONGNAME  = 'NINFRT',         &
  CUNITS     = 's',              &
  CDIR       = '--',             &
  CCOMMENT   = 'Interval in seconds between forcings', &
  NGRID      = 0,                &
  NTYPE      = TYPEINT,          &
  NDIMS      = 0,                &
  LTIMEDEP   = .FALSE.           ) )
!
!
WRITE(YMSG,'("number of used fields=",I4," out of ",I4)') nfields_used-1,MAXFIELDS
CALL PRINT_MSG(NVERB_INFO,'GEN','INI_FIELD_LIST',TRIM(YMSG))
!
#if 0
!
call Add_field2list( TFIELDDATA( &
  CMNHNAME   = '',               &
  CSTDNAME   = '',               &
  CLONGNAME  = '',               &
  CUNITS     = '',               &
  CDIR       = '',               &
  CLBTYPE    = '',               &
  CCOMMENT   = '',               &
  NGRID      = ,                 &
  NTYPE      = ,                 &
  NDIMS      = ,                 &
  LTIMEDEP   = ,                 ) )
#endif
!
END SUBROUTINE INI_FIELD_LIST
!
SUBROUTINE FIND_FIELD_ID_FROM_MNHNAME(HMNHNAME,KID,KRESP,ONOWARNING)
!
CHARACTER(LEN=*),            INTENT(IN) :: HMNHNAME !Name of the field to find
INTEGER,                     INTENT(OUT):: KID      !Index of the field
INTEGER,                     INTENT(OUT):: KRESP    !Return-code 
LOGICAL, OPTIONAL,           INTENT(IN) :: ONOWARNING !If true, do not print warning
!
INTEGER :: IDX,JI
INTEGER :: ICOUNT
INTEGER,SAVE :: IFIRSTGUESS=1 !Store first field to test
CHARACTER(LEN=64) :: YMSG
LOGICAL :: GNOWARNING
!
!PW: TODO: possible optimizations:
! * Classement alphanumerique + index vers 1er champ commencant par caractere
! * Classement dans l'ordre des ecritures + stockage dernier hit + reboucler depuis le debut => DONE
!
IF (.NOT.LFIELDLIST_ISINIT) THEN
  CALL PRINT_MSG(NVERB_FATAL,'GEN','FIND_FIELD_ID_FROM_MNHNAME','TFIELDLIST not yet initialized')
END IF
!
KID = 0
KRESP = 0
ICOUNT = 0
IDX = IFIRSTGUESS
!
IF (PRESENT(ONOWARNING)) THEN
  GNOWARNING = ONOWARNING
ELSE
  GNOWARNING = .FALSE.
END IF
!
DO
  ICOUNT = ICOUNT + 1
  IF (TRIM(TFIELDLIST(IDX)%CMNHNAME)=='') THEN !Last entry
    IDX = 1
  ELSE IF (TRIM(TFIELDLIST(IDX)%CMNHNAME)==TRIM(HMNHNAME)) THEN
    KID = IDX
    EXIT
  ELSE 
    IDX = IDX + 1
    IF (IDX>MAXFIELDS) IDX = 1
  END IF
  IF (IDX == IFIRSTGUESS) EXIT !All entries have been tested
END DO
!
IF (KID==0) THEN
  !Field not found
  KRESP = -1
  IF (.NOT.GNOWARNING) THEN
    CALL PRINT_MSG(NVERB_WARNING,'GEN','FIND_FIELD_ID_FROM_MNHNAME','field '//TRIM(HMNHNAME)//' not known')
  ELSE
    CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIND_FIELD_ID_FROM_MNHNAME','field '//TRIM(HMNHNAME)//' not known (not unexpected)')
  END IF
ELSE
  IFIRSTGUESS = IDX+1
  IF (IFIRSTGUESS>MAXFIELDS) IFIRSTGUESS = 1
  WRITE(YMSG,'( "field ",A16," found after ",I4," attempt(s)" )') TRIM(HMNHNAME),ICOUNT
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIND_FIELD_ID_FROM_MNHNAME',TRIM(YMSG))
END IF
!
END SUBROUTINE FIND_FIELD_ID_FROM_MNHNAME
!
!
SUBROUTINE ALLOC_FIELD_SCALARS
!
USE MODD_DYN_n
USE MODD_PARAM_n
!
CALL PRINT_MSG(NVERB_DEBUG,'GEN','ALLOC_FIELD_SCALARS','called')
!
IF (LFIELDLIST_ISINIT) THEN
  CALL PRINT_MSG(NVERB_FATAL,'GEN','ALLOC_FIELD_SCALARS','TFIELDLIST already initialized')
END IF
!
!
IF (.NOT.ASSOCIATED(NRIMX)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','ALLOC_FIELD_SCALARS',' NRIMX was not associated')
  ALLOCATE(NRIMX)
END IF
IF (.NOT.ASSOCIATED(NRIMY)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','ALLOC_FIELD_SCALARS',' NRIMY was not associated')
  ALLOCATE(NRIMY)
END IF
IF (.NOT.ASSOCIATED(LHORELAX_UVWTH)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','ALLOC_FIELD_SCALARS',' LHORELAX_UVWTH was not associated')
  ALLOCATE(LHORELAX_UVWTH)
END IF
IF (.NOT.ASSOCIATED(LHORELAX_TKE)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','ALLOC_FIELD_SCALARS',' LHORELAX_TKE was not associated')
  ALLOCATE(LHORELAX_TKE)
END IF
IF (.NOT.ASSOCIATED(CSURF)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','ALLOC_FIELD_SCALARS',' CSURF was not associated')
  ALLOCATE(CHARACTER(LEN=4) :: CSURF)
  CSURF = ''
END IF
!
END SUBROUTINE ALLOC_FIELD_SCALARS
!
!
SUBROUTINE INI_FIELD_SCALARS
!
USE MODD_DYN_n
USE MODD_FIELD_n
USE MODD_GRID_n
USE MODD_TIME_n
USE MODD_PARAM_n
!
INTEGER :: IID,IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS','called')
!
IF (.NOT.LFIELDLIST_ISINIT) THEN
  CALL PRINT_MSG(NVERB_FATAL,'GEN','FIND_FIELD_ID_FROM_MNHNAME','TFIELDLIST not yet initialized')
END IF
!
!
IF (.NOT.ASSOCIATED(XZTOP)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' XZTOP was not associated')
  ALLOCATE(XZTOP)
  CALL FIND_FIELD_ID_FROM_MNHNAME('ZTOP',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_X0D(1)%DATA=>XZTOP
END IF
!
IF (.NOT.ASSOCIATED(LSLEVE)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' LSLEVE was not associated')
  ALLOCATE(LSLEVE)
  CALL FIND_FIELD_ID_FROM_MNHNAME('SLEVE',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_L0D(1)%DATA=>LSLEVE
END IF
!
IF (.NOT.ASSOCIATED(XLEN1)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' XLEN1 was not associated')
  ALLOCATE(XLEN1)
  CALL FIND_FIELD_ID_FROM_MNHNAME('LEN1',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_X0D(1)%DATA=>XLEN1
END IF
!
IF (.NOT.ASSOCIATED(XLEN2)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' XLEN2 was not associated')
  ALLOCATE(XLEN2)
  CALL FIND_FIELD_ID_FROM_MNHNAME('LEN2',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_X0D(1)%DATA=>XLEN2
END IF
!
IF (.NOT.ASSOCIATED(TDTMOD)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' TDTMOD was not associated')
  ALLOCATE(TDTMOD)
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTMOD',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_T0D(1)%DATA=>TDTMOD
END IF
!
IF (.NOT.ASSOCIATED(TDTCUR)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' TDTCUR was not associated')
  ALLOCATE(TDTCUR)
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTCUR',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_T0D(1)%DATA=>TDTCUR
END IF
!
IF (.NOT.ASSOCIATED(TDTRAD_FULL)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' TDTRAD_FULL was not associated')
  ALLOCATE(TDTRAD_FULL)
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTRAD_FULL',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_T0D(1)%DATA=>TDTRAD_FULL
END IF
!
IF (.NOT.ASSOCIATED(TDTRAD_CLONLY)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' TDTRAD_CLONLY was not associated')
  ALLOCATE(TDTRAD_CLONLY)
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTRAD_CLLY',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_T0D(1)%DATA=>TDTRAD_CLONLY
END IF
!
IF (.NOT.ASSOCIATED(TDTDCONV)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' TDTDCONV was not associated')
  ALLOCATE(TDTDCONV)
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTDCONV',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_T0D(1)%DATA=>TDTDCONV
END IF
!
IF (.NOT.ASSOCIATED(CSURF)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' CSURF was not associated')
  ALLOCATE(CHARACTER(LEN=4) :: CSURF)
  CSURF = ''
END IF
CALL FIND_FIELD_ID_FROM_MNHNAME('SURF',IID,IRESP)
TFIELDLIST(IID)%TFIELD_C0D(1)%DATA=>CSURF
!
IF (.NOT.ASSOCIATED(XDRYMASST)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' XDRYMASST was not associated')
  ALLOCATE(XDRYMASST)
  CALL FIND_FIELD_ID_FROM_MNHNAME('DRYMASST',IID,IRESP)
  TFIELDLIST(IID)%TFIELD_X0D(1)%DATA=>XDRYMASST
END IF
!
IF (.NOT.ASSOCIATED(NRIMX)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' NRIMX was not associated')
  ALLOCATE(NRIMX)
END IF
!
CALL FIND_FIELD_ID_FROM_MNHNAME('RIMX',IID,IRESP)
TFIELDLIST(IID)%TFIELD_N0D(1)%DATA=>NRIMX
IF (.NOT.ASSOCIATED(NRIMY)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' NRIMY was not associated')
  ALLOCATE(NRIMY)
END IF
!
CALL FIND_FIELD_ID_FROM_MNHNAME('RIMY',IID,IRESP)
TFIELDLIST(IID)%TFIELD_N0D(1)%DATA=>NRIMY
!
IF (.NOT.ASSOCIATED(LHORELAX_UVWTH)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' LHORELAX_UVWTH was not associated')
  ALLOCATE(LHORELAX_UVWTH)
END IF
CALL FIND_FIELD_ID_FROM_MNHNAME('HORELAX_UVWTH',IID,IRESP)
TFIELDLIST(IID)%TFIELD_L0D(1)%DATA=>LHORELAX_UVWTH
!
IF (.NOT.ASSOCIATED(LHORELAX_TKE)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','INI_FIELD_SCALARS',' LHORELAX_TKE was not associated')
  ALLOCATE(LHORELAX_TKE)
END IF
CALL FIND_FIELD_ID_FROM_MNHNAME('HORELAX_TKE',IID,IRESP)
TFIELDLIST(IID)%TFIELD_L0D(1)%DATA=>LHORELAX_TKE
!
END SUBROUTINE INI_FIELD_SCALARS
!
!
SUBROUTINE FIELDLIST_GOTO_MODEL(KFROM, KTO)
!
USE MODD_REF
!
USE MODD_ADV_n
USE MODD_CONF_n
USE MODD_DEEP_CONVECTION_n
USE MODD_DEF_EDDY_FLUX_n
USE MODD_DEF_EDDYUV_FLUX_n
USE MODD_DYN_n
USE MODD_ELEC_n
USE MODD_FIELD_n
USE MODD_GR_FIELD_n
USE MODD_GRID_n
USE MODD_HURR_FIELD_n
USE MODD_LIMA_PRECIP_SCAVENGING_n
USE MODD_LSFIELD_n
USE MODD_OCEANH
USE MODD_PARAM_n
USE MODD_PAST_FIELD_n
USE MODD_CH_PH_n
USE MODD_PRECIP_n
USE MODD_RADIATIONS_n
USE MODD_REF_n
USE MODD_TIME_n
USE MODD_TURB_n
!
INTEGER, INTENT(IN) :: KFROM, KTO
!
!LOGICAL,SAVE :: GFIRST_CALL=.TRUE.
INTEGER :: IID,IID2,IRESP
CHARACTER(LEN=64) :: YMSG
!
WRITE(YMSG,'( I4,"->",I4 )') KFROM,KTO
CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',TRIM(YMSG))
!
! IF (GFIRST_CALL) THEN
!   !This is necessary because the first time this subroutine is called
!   !the TFIELDLIST is not yet initialized.
!   !The use of this subroutine is not useful the first timebecause the
!   !data for the fields has not yet been allocated.
!   GFIRST_CALL = .FALSE.
!   RETURN
! END IF
!
IF (.NOT.LFIELDLIST_ISINIT) THEN
  CALL PRINT_MSG(NVERB_WARNING,'GEN','FIELDLIST_GOTO_MODEL','TFIELDLIST not yet initialized')
  RETURN
END IF
!
if (kfrom > nmodel_allocated .or. kto > nmodel_allocated ) &
  call Print_msg( NVERB_FATAL, 'GEN', 'FIELDLIST_GOTO_MODEL', 'kfrom or kto > nmodel_allocated' )
!
! Initialize some pointers
!
!PW: TODO: check if still necessary as XRHODREFZ and XTHVREFZ are now initialiazed in ini_modeln even for KMI/=1 (29/01/2019)
IF (KFROM == KTO) THEN
  IF ( CPROGRAM/='NESPGD' .AND. CPROGRAM/='PGD' ) THEN
    IF (.NOT.ALLOCATED(XRHODREFZ)) CALL PRINT_MSG(NVERB_FATAL,'GEN','FIELDLIST_GOTO_MODEL','XRHODREFZ not yet allocated')
    CALL FIND_FIELD_ID_FROM_MNHNAME('RHOREFZ',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X1D(KFROM)%DATA=>XRHODREFZ
    !
    IF (.NOT.ALLOCATED(XTHVREFZ)) CALL PRINT_MSG(NVERB_FATAL,'GEN','FIELDLIST_GOTO_MODEL','XTHVREFZ not yet allocated')
    CALL FIND_FIELD_ID_FROM_MNHNAME('THVREFZ',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X1D(KFROM)%DATA=>XTHVREFZ
  END IF
END IF
!
!
!
!
! Save current state for allocated arrays
!
!
!
!
!
! MODD_FIELD_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('ZWS',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XZWS
CALL FIND_FIELD_ID_FROM_MNHNAME('UT',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XUT
CALL FIND_FIELD_ID_FROM_MNHNAME('VT',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XVT
CALL FIND_FIELD_ID_FROM_MNHNAME('WT',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XWT
CALL FIND_FIELD_ID_FROM_MNHNAME('THT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XTHT
CALL FIND_FIELD_ID_FROM_MNHNAME('TKET', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XTKET
CALL FIND_FIELD_ID_FROM_MNHNAME('PABST',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XPABST
CALL FIND_FIELD_ID_FROM_MNHNAME('PHIT', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XPHIT
CALL FIND_FIELD_ID_FROM_MNHNAME('RT',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X4D(KFROM)%DATA => XRT
!
IF (ASSOCIATED(XRT)) THEN
  IF (CONF_MODEL(KFROM)%IDX_RVT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RVT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>XRT(:,:,:,CONF_MODEL(KFROM)%IDX_RVT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RCT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RCT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>XRT(:,:,:,CONF_MODEL(KFROM)%IDX_RCT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RRT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RRT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>XRT(:,:,:,CONF_MODEL(KFROM)%IDX_RRT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RIT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RIT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>XRT(:,:,:,CONF_MODEL(KFROM)%IDX_RIT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RST>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RST',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>XRT(:,:,:,CONF_MODEL(KFROM)%IDX_RST)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RGT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RGT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>XRT(:,:,:,CONF_MODEL(KFROM)%IDX_RGT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RHT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RHT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>XRT(:,:,:,CONF_MODEL(KFROM)%IDX_RHT)
  END IF
ELSE
  IF (CONF_MODEL(KFROM)%IDX_RVT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RVT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>NULL()
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RCT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RCT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>NULL()
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RRT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RRT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>NULL()
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RIT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RIT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>NULL()
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RST>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RST',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>NULL()
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RGT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RGT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>NULL()
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RHT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RHT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA=>NULL()
  END IF

END IF
!
CALL FIND_FIELD_ID_FROM_MNHNAME('SUPSATMAX',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XSUPSAT
CALL FIND_FIELD_ID_FROM_MNHNAME('NACT',     IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XNACT
CALL FIND_FIELD_ID_FROM_MNHNAME('SSPRO',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XSSPRO
CALL FIND_FIELD_ID_FROM_MNHNAME('NPRO',     IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XNPRO
CALL FIND_FIELD_ID_FROM_MNHNAME('SRCT',     IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XSRCT
CALL FIND_FIELD_ID_FROM_MNHNAME('SIGS',     IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XSIGS
!
IF (CPROGRAM == 'MESONH') THEN
  !
  CALL FIND_FIELD_ID_FROM_MNHNAME('US_PRES',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRUS_PRES
  CALL FIND_FIELD_ID_FROM_MNHNAME('VS_PRES',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRVS_PRES
  CALL FIND_FIELD_ID_FROM_MNHNAME('WS_PRES',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRWS_PRES
  CALL FIND_FIELD_ID_FROM_MNHNAME('THS_CLD',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRTHS_CLD
  !
  CALL FIND_FIELD_ID_FROM_MNHNAME('RS_CLD',IID,IRESP); TFIELDLIST(IID)%TFIELD_X4D(KFROM)%DATA => XRRS_CLD
  !
  IF (CONF_MODEL(KFROM)%IDX_RVT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RVS_CLD',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRRS_CLD(:,:,:,CONF_MODEL(KFROM)%IDX_RVT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RCT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RCS_CLD',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRRS_CLD(:,:,:,CONF_MODEL(KFROM)%IDX_RCT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RRT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RRS_CLD',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRRS_CLD(:,:,:,CONF_MODEL(KFROM)%IDX_RRT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RIT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RIS_CLD',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRRS_CLD(:,:,:,CONF_MODEL(KFROM)%IDX_RIT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RST>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RSS_CLD',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRRS_CLD(:,:,:,CONF_MODEL(KFROM)%IDX_RST)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RGT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RGS_CLD',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRRS_CLD(:,:,:,CONF_MODEL(KFROM)%IDX_RGT)
  END IF
  IF (CONF_MODEL(KFROM)%IDX_RHT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RHS_CLD',IID,IRESP)
    TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRRS_CLD(:,:,:,CONF_MODEL(KFROM)%IDX_RHT)
  END IF
  CALL FIND_FIELD_ID_FROM_MNHNAME('CLDFR',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XCLDFR
  CALL FIND_FIELD_ID_FROM_MNHNAME('CIT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XCIT
  CALL FIND_FIELD_ID_FROM_MNHNAME('RAINFR',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRAINFR
  !
END IF
!
! MODD_PAST_FIELD_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('UM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XUM
CALL FIND_FIELD_ID_FROM_MNHNAME('VM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XVM
CALL FIND_FIELD_ID_FROM_MNHNAME('WM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XWM
CALL FIND_FIELD_ID_FROM_MNHNAME('DUM',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDUM
CALL FIND_FIELD_ID_FROM_MNHNAME('DVM',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDVM
CALL FIND_FIELD_ID_FROM_MNHNAME('DWM',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDWM
!
! MODD_LIMA_PRECIP_SCAVENGING_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('INPAP',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XINPAP
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPAP',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XACPAP
!
! MODD_ELEC_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('EFIELDU',IID,IRESP);  TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XEFIELDU
CALL FIND_FIELD_ID_FROM_MNHNAME('EFIELDV',IID,IRESP);  TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XEFIELDV
CALL FIND_FIELD_ID_FROM_MNHNAME('EFIELDW',IID,IRESP);  TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XEFIELDW
CALL FIND_FIELD_ID_FROM_MNHNAME('NI_IAGGS',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XNI_IAGGS
CALL FIND_FIELD_ID_FROM_MNHNAME('NI_IDRYG',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XNI_IDRYG
CALL FIND_FIELD_ID_FROM_MNHNAME('NI_SDRYG',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XNI_SDRYG
CALL FIND_FIELD_ID_FROM_MNHNAME('INDUC_CG',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XIND_RATE
!
! MODD_CH_PH_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('PHC',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XPHC
CALL FIND_FIELD_ID_FROM_MNHNAME('PHR',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XPHR
!
! MODD_LSFIELD_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('LSUM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLSUM
CALL FIND_FIELD_ID_FROM_MNHNAME('LSVM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLSVM
CALL FIND_FIELD_ID_FROM_MNHNAME('LSWM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLSWM
CALL FIND_FIELD_ID_FROM_MNHNAME('LSTHM',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLSTHM
IF (LUSERV) THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('LSRVM',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLSRVM
END IF
CALL FIND_FIELD_ID_FROM_MNHNAME('LBXUM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLBXUM
CALL FIND_FIELD_ID_FROM_MNHNAME('LBXVM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLBXVM
CALL FIND_FIELD_ID_FROM_MNHNAME('LBXWM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLBXWM
CALL FIND_FIELD_ID_FROM_MNHNAME('LBYUM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLBYUM
CALL FIND_FIELD_ID_FROM_MNHNAME('LBYVM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLBYVM
CALL FIND_FIELD_ID_FROM_MNHNAME('LBYWM', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLBYWM
CALL FIND_FIELD_ID_FROM_MNHNAME('LBXTHM',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLBXTHM
CALL FIND_FIELD_ID_FROM_MNHNAME('LBYTHM',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XLBYTHM
!
! MODD_DYN_n variables
!
!***NONE***
!
! MODD_ADV_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('TKEMS',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRTKEMS
!
! MODD_GRID_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('ZS',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XZS
CALL FIND_FIELD_ID_FROM_MNHNAME('ZSMT', IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XZSMT
CALL FIND_FIELD_ID_FROM_MNHNAME('XHAT', IID,IRESP); TFIELDLIST(IID)%TFIELD_X1D(KFROM)%DATA => XXHAT
CALL FIND_FIELD_ID_FROM_MNHNAME('YHAT', IID,IRESP); TFIELDLIST(IID)%TFIELD_X1D(KFROM)%DATA => XYHAT
CALL FIND_FIELD_ID_FROM_MNHNAME('ZHAT', IID,IRESP); TFIELDLIST(IID)%TFIELD_X1D(KFROM)%DATA => XZHAT
CALL FIND_FIELD_ID_FROM_MNHNAME('ZTOP', IID,IRESP); TFIELDLIST(IID)%TFIELD_X0D(KFROM)%DATA => XZTOP
CALL FIND_FIELD_ID_FROM_MNHNAME('DXHAT',IID,IRESP); TFIELDLIST(IID)%TFIELD_X1D(KFROM)%DATA => XDXHAT
CALL FIND_FIELD_ID_FROM_MNHNAME('DYHAT',IID,IRESP); TFIELDLIST(IID)%TFIELD_X1D(KFROM)%DATA => XDYHAT
CALL FIND_FIELD_ID_FROM_MNHNAME('ALT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XZZ
CALL FIND_FIELD_ID_FROM_MNHNAME('DIRCOSXW',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XDIRCOSXW
CALL FIND_FIELD_ID_FROM_MNHNAME('DIRCOSYW',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XDIRCOSYW
CALL FIND_FIELD_ID_FROM_MNHNAME('DIRCOSZW',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XDIRCOSZW
CALL FIND_FIELD_ID_FROM_MNHNAME('COSSLOPE',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XCOSSLOPE
CALL FIND_FIELD_ID_FROM_MNHNAME('SINSLOPE',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XSINSLOPE
CALL FIND_FIELD_ID_FROM_MNHNAME('MAP',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XMAP
CALL FIND_FIELD_ID_FROM_MNHNAME('LAT',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XLAT
CALL FIND_FIELD_ID_FROM_MNHNAME('LON',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XLON
!
! MODD_TIME_n variables
!
!***NONE***
!
! MODD_PARAM_n variables
!
!***NONE***
!
! MODD_TURB_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('BL_DEPTH', IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XBL_DEPTH
CALL FIND_FIELD_ID_FROM_MNHNAME('SBL_DEPTH',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XSBL_DEPTH
CALL FIND_FIELD_ID_FROM_MNHNAME('WTHVMF',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XWTHVMF
!
! MODD_REF_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('RHODREF',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRHODREF
CALL FIND_FIELD_ID_FROM_MNHNAME('THVREF', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XTHVREF
!
! MODD_RADIATIONS_n variables
!
IF (CPROGRAM=='MESONH') THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTHRAD',      IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDTHRAD
  CALL FIND_FIELD_ID_FROM_MNHNAME('FLALWD',      IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XFLALWD
  CALL FIND_FIELD_ID_FROM_MNHNAME('DIRFLASWD',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDIRFLASWD
  CALL FIND_FIELD_ID_FROM_MNHNAME('SCAFLASWD',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XSCAFLASWD
  CALL FIND_FIELD_ID_FROM_MNHNAME('DIRSRFSWD',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDIRSRFSWD
  CALL FIND_FIELD_ID_FROM_MNHNAME('CLEARCOL_TM1',IID,IRESP); TFIELDLIST(IID)%TFIELD_N2D(KFROM)%DATA => NCLEARCOL_TM1
  CALL FIND_FIELD_ID_FROM_MNHNAME('ZENITH',      IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XZENITH
  CALL FIND_FIELD_ID_FROM_MNHNAME('AZIM',        IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XAZIM
  CALL FIND_FIELD_ID_FROM_MNHNAME('DIR_ALB',     IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDIR_ALB
  CALL FIND_FIELD_ID_FROM_MNHNAME('SCA_ALB',     IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XSCA_ALB
  CALL FIND_FIELD_ID_FROM_MNHNAME('EMIS',        IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XEMIS
  CALL FIND_FIELD_ID_FROM_MNHNAME('TSRAD',       IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XTSRAD
END IF
!
! MODD_DEEP_CONVECTION_n variables
!
IF (TRIM(CPROGRAM) /= 'PGD' .AND. TRIM(CPROGRAM) /= 'NESPGD' .AND. TRIM(CPROGRAM) /= 'SPAWN') THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('COUNTCONV',  IID,IRESP); TFIELDLIST(IID)%TFIELD_N2D(KFROM)%DATA => NCOUNTCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTHCONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDTHCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('DRVCONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDRVCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('DRCCONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDRCCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('DRICONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDRICONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRCONV',     IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XPRCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('PACCONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XPACCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRSCONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XPRSCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('DSVCONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X4D(KFROM)%DATA => XDSVCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRLFLXCONV', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XPRLFLXCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRSFLXCONV', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XPRSFLXCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('UMFCONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XUMFCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('DMFCONV',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XDMFCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('MFCONV',     IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XMFCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('CAPE',       IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XCAPE
  CALL FIND_FIELD_ID_FROM_MNHNAME('CLTOPCONV_LVL',IID,IRESP); TFIELDLIST(IID)%TFIELD_N2D(KFROM)%DATA => NCLTOPCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('CLBASCONV_LVL',IID,IRESP); TFIELDLIST(IID)%TFIELD_N2D(KFROM)%DATA => NCLBASCONV
  CALL FIND_FIELD_ID_FROM_MNHNAME('IC_RATE',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XIC_RATE
  CALL FIND_FIELD_ID_FROM_MNHNAME('CG_RATE',    IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XCG_RATE
  CALL FIND_FIELD_ID_FROM_MNHNAME('IC_TOTAL_NB',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XIC_TOTAL_NUMBER
  CALL FIND_FIELD_ID_FROM_MNHNAME('CG_TOTAL_NB',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XCG_TOTAL_NUMBER
END IF
!
! MODD_GR_FIELD_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('SSO_ANIS', IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XSSO_ANISOTROPY
CALL FIND_FIELD_ID_FROM_MNHNAME('SSO_SLOPE',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XSSO_SLOPE
CALL FIND_FIELD_ID_FROM_MNHNAME('SSO_DIR',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XSSO_DIRECTION
CALL FIND_FIELD_ID_FROM_MNHNAME('AVG_ZS',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XAVG_ZS
CALL FIND_FIELD_ID_FROM_MNHNAME('SIL_ZS',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XSIL_ZS
CALL FIND_FIELD_ID_FROM_MNHNAME('MAX_ZS',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XMAX_ZS
CALL FIND_FIELD_ID_FROM_MNHNAME('MIN_ZS',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XMIN_ZS
CALL FIND_FIELD_ID_FROM_MNHNAME('SSO_STDEV',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XSSO_STDEV
!
! MODD_PRECIP_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRC',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XINPRC
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRC',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XACPRC
CALL FIND_FIELD_ID_FROM_MNHNAME('INDEP',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XINDEP
CALL FIND_FIELD_ID_FROM_MNHNAME('ACDEP',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XACDEP
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRR',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XINPRR
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRR3D',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XINPRR3D
CALL FIND_FIELD_ID_FROM_MNHNAME('EVAP3D', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XEVAP3D
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRR',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XACPRR
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRS',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XINPRS
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRS',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XACPRS
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRG',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XINPRG
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRG',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XACPRG
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRH',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XINPRH
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRH',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XACPRH
!
! MODD_DEF_EDDY_FLUX_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('VT_FLX',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA         => XVTH_FLUX_M
CALL FIND_FIELD_ID_FROM_MNHNAME('WT_FLX',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA         => XWTH_FLUX_M
CALL FIND_FIELD_ID_FROM_MNHNAME('RTHS_EDDY_FLUX',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRTHS_EDDY_FLUX
!
! MODD_DEF_EDDYUV_FLUX_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('VU_FLX',IID,IRESP);        TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XVU_FLUX_M
CALL FIND_FIELD_ID_FROM_MNHNAME('RVS_EDDY_FLUX',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XRVS_EDDY_FLUX
!
! MODD_HURR_FIELD_n variables
!
IF (CPROGRAM=='REAL') THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('UT15',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XUTOT
  CALL FIND_FIELD_ID_FROM_MNHNAME('VT15',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XVTOT
  CALL FIND_FIELD_ID_FROM_MNHNAME('TEMPTOT',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XTTOT
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRESTOT',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XPTOT
  CALL FIND_FIELD_ID_FROM_MNHNAME('HUMTOT', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XQTOT
  CALL FIND_FIELD_ID_FROM_MNHNAME('UT16',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XUENV
  CALL FIND_FIELD_ID_FROM_MNHNAME('VT16',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XVENV
  CALL FIND_FIELD_ID_FROM_MNHNAME('TEMPENV',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XTENV
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRESENV',IID,IRESP); TFIELDLIST(IID)%TFIELD_X2D(KFROM)%DATA => XPENV
  CALL FIND_FIELD_ID_FROM_MNHNAME('HUMENV', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XQENV
  CALL FIND_FIELD_ID_FROM_MNHNAME('UT17',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XUBASIC
  CALL FIND_FIELD_ID_FROM_MNHNAME('VT17',   IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XVBASIC
  CALL FIND_FIELD_ID_FROM_MNHNAME('TEMPBAS',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XTBASIC
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRESBAS',IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XPBASIC
  CALL FIND_FIELD_ID_FROM_MNHNAME('HUMBAS', IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XQBASIC
  CALL FIND_FIELD_ID_FROM_MNHNAME('VTDIS',  IID,IRESP); TFIELDLIST(IID)%TFIELD_X3D(KFROM)%DATA => XVTDIS
END IF
!
!
!
!
! Current model is set to model KTO
!
!
!
!
IF( KFROM/=KTO) THEN
!
! MODD_FIELD_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('ZWS',  IID,IRESP); XZWS   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('UT',   IID,IRESP); XUT    => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('VT',   IID,IRESP); XVT    => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('WT',   IID,IRESP); XWT    => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('THT',  IID,IRESP); XTHT   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('TKET', IID,IRESP); XTKET  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('PABST',IID,IRESP); XPABST => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('PHIT', IID,IRESP); XPHIT  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('RT',   IID,IRESP); XRT    => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA
!
IF (ASSOCIATED(XRT)) THEN
  IF (CONF_MODEL(KTO)%IDX_RVT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RVT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RVT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RCT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RCT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RCT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RRT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RRT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RRT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RIT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RIT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RIT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RST>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RST',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RST)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RGT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RGT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RGT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RHT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RHT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RHT)
  END IF
ELSE
  IF (CONF_MODEL(KTO)%IDX_RVT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RVT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => NULL()
  END IF
  IF (CONF_MODEL(KTO)%IDX_RCT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RCT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => NULL()
  END IF
  IF (CONF_MODEL(KTO)%IDX_RRT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RRT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => NULL()
  END IF
  IF (CONF_MODEL(KTO)%IDX_RIT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RIT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => NULL()
  END IF
  IF (CONF_MODEL(KTO)%IDX_RST>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RST',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => NULL()
  END IF
  IF (CONF_MODEL(KTO)%IDX_RGT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RGT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => NULL()
  END IF
  IF (CONF_MODEL(KTO)%IDX_RHT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RHT',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => NULL()
  END IF
END IF
!
CALL FIND_FIELD_ID_FROM_MNHNAME('SUPSATMAX',IID,IRESP); XSUPSAT => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('NACT',     IID,IRESP); XNACT   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SSPRO',    IID,IRESP); XSSPRO  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('NPRO',     IID,IRESP); XNPRO   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SRCT',     IID,IRESP); XSRCT   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SIGS',     IID,IRESP); XSIGS   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
IF (CPROGRAM == 'MESONH') THEN
  !
  CALL FIND_FIELD_ID_FROM_MNHNAME('RS_CLD',IID,IRESP); XRRS_CLD => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA
  !
  IF (CONF_MODEL(KTO)%IDX_RVT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RVS_CLD',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RVT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RCT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RCS_CLD',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RCT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RRT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RRS_CLD',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RRT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RIT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RIS_CLD',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RIT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RST>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RSS_CLD',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RST)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RGT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RGS_CLD',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RGT)
  END IF
  IF (CONF_MODEL(KTO)%IDX_RHT>0) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('RHS_CLD',IID2,IRESP)
    TFIELDLIST(IID2)%TFIELD_X3D(KTO)%DATA => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA(:,:,:,CONF_MODEL(KTO)%IDX_RHT)
  END IF
  !
  CALL FIND_FIELD_ID_FROM_MNHNAME('US_PRES',IID,IRESP); XRUS_PRES => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('VS_PRES',IID,IRESP); XRVS_PRES => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('WS_PRES',IID,IRESP); XRWS_PRES => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('THS_CLD',IID,IRESP); XRTHS_CLD => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('CLDFR',  IID,IRESP); XCLDFR    => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('CIT',    IID,IRESP); XCIT      => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('RAINFR',  IID,IRESP); XRAINFR   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
END IF
!
! MODD_PAST_FIELD_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('UM', IID,IRESP); XUM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('VM', IID,IRESP); XVM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('WM', IID,IRESP); XWM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('DUM',IID,IRESP); XDUM => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('DVM',IID,IRESP); XDVM => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('DWM',IID,IRESP); XDWM => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
! MODD_LIMA_PRECIP_SCAVENGING_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('INPAP',IID,IRESP); XINPAP => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPAP',IID,IRESP); XACPAP => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
!
! MODD_ELEC_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('EFIELDU',IID,IRESP);  XEFIELDU  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('EFIELDV',IID,IRESP);  XEFIELDV  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('EFIELDW',IID,IRESP);  XEFIELDW  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('NI_IAGGS',IID,IRESP); XNI_IAGGS => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('NI_IDRYG',IID,IRESP); XNI_IDRYG => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('NI_SDRYG',IID,IRESP); XNI_SDRYG => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('INDUC_CG',IID,IRESP); XIND_RATE => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
! MODD_CH_PH_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('PHC',IID,IRESP); XPHC => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('PHR',IID,IRESP); XPHR => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
! MODD_LSFIELD_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('LSUM', IID,IRESP); XLSUM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LSVM', IID,IRESP); XLSVM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LSWM', IID,IRESP); XLSWM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LSTHM',IID,IRESP); XLSTHM => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
IF (LUSERV) THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('LSRVM',IID,IRESP); XLSRVM => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
END IF
CALL FIND_FIELD_ID_FROM_MNHNAME('LBXUM', IID,IRESP); XLBXUM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LBXVM', IID,IRESP); XLBXVM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LBXWM', IID,IRESP); XLBXWM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LBYUM', IID,IRESP); XLBYUM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LBYVM', IID,IRESP); XLBYVM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LBYWM', IID,IRESP); XLBYWM  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LBXTHM',IID,IRESP); XLBXTHM => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LBYTHM',IID,IRESP); XLBYTHM => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('DRYMASST',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA)
END IF
XDRYMASST => TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA
!
! MODD_DYN_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('RIMX',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_N0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_N0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_N0D(KTO)%DATA)
END IF
NRIMX => TFIELDLIST(IID)%TFIELD_N0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('RIMY',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_N0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_N0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_N0D(KTO)%DATA)
END IF
NRIMY => TFIELDLIST(IID)%TFIELD_N0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('HORELAX_UVWTH',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA)
END IF
LHORELAX_UVWTH => TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('HORELAX_TKE',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA)
END IF
LHORELAX_TKE => TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA
!
! MODD_ADV_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('TKEMS',IID,IRESP); XRTKEMS=>TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
! MODD_GRID_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('ZS',   IID,IRESP); XZS    => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ZSMT', IID,IRESP); XZSMT  => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('XHAT', IID,IRESP); XXHAT  => TFIELDLIST(IID)%TFIELD_X1D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('YHAT', IID,IRESP); XYHAT  => TFIELDLIST(IID)%TFIELD_X1D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ZHAT', IID,IRESP); XZHAT  => TFIELDLIST(IID)%TFIELD_X1D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('ZTOP', IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA)
END IF
XZTOP  => TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('DXHAT',IID,IRESP); XDXHAT => TFIELDLIST(IID)%TFIELD_X1D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('DYHAT',IID,IRESP); XDYHAT => TFIELDLIST(IID)%TFIELD_X1D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('SLEVE',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA)
END IF
LSLEVE => TFIELDLIST(IID)%TFIELD_L0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('LEN1',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA)
END IF
XLEN1 => TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('LEN2',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA)
END IF
XLEN2 => TFIELDLIST(IID)%TFIELD_X0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('ALT',     IID,IRESP); XZZ       => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('DIRCOSXW',IID,IRESP); XDIRCOSXW => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('DIRCOSYW',IID,IRESP); XDIRCOSYW => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('DIRCOSZW',IID,IRESP); XDIRCOSZW => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('COSSLOPE',IID,IRESP); XCOSSLOPE => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SINSLOPE',IID,IRESP); XSINSLOPE => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('MAP',  IID,IRESP); XMAP => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LAT',  IID,IRESP); XLAT => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('LON',  IID,IRESP); XLON => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
!
! MODD_TIME_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('DTMOD',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)
END IF
TDTMOD => TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('DTCUR',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)
END IF
TDTCUR => TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('DTRAD_FULL',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)
END IF
TDTRAD_FULL => TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('DTRAD_CLLY',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)
END IF
TDTRAD_CLONLY => TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA
!
CALL FIND_FIELD_ID_FROM_MNHNAME('DTDCONV',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA)
END IF
TDTDCONV => TFIELDLIST(IID)%TFIELD_T0D(KTO)%DATA
!
! MODD_PARAM_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('SURF',IID,IRESP)
IF (.NOT.ASSOCIATED(TFIELDLIST(IID)%TFIELD_C0D(KTO)%DATA)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'GEN','FIELDLIST_GOTO_MODEL',&
                 'TFIELDLIST(IID)%TFIELD_C0D(KTO)%DATA was not associated for '//TFIELDLIST(IID)%CMNHNAME)
  ALLOCATE(CHARACTER(LEN=4) :: TFIELDLIST(IID)%TFIELD_C0D(KTO)%DATA)
  TFIELDLIST(IID)%TFIELD_C0D(KTO)%DATA=''
END IF
CSURF => TFIELDLIST(IID)%TFIELD_C0D(KTO)%DATA
!
! MODD_TURB_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('BL_DEPTH', IID,IRESP); XBL_DEPTH  => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SBL_DEPTH',IID,IRESP); XSBL_DEPTH => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('WTHVMF',   IID,IRESP); XWTHVMF    => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
! MODD_REF_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('RHODREF',IID,IRESP); XRHODREF => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('THVREF', IID,IRESP); XTHVREF  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
! MODD_RADIATIONS_n variables
!
IF (CPROGRAM=='MESONH') THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTHRAD',      IID,IRESP); XDTHRAD       => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('FLALWD',      IID,IRESP); XFLALWD       => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DIRFLASWD',   IID,IRESP); XDIRFLASWD    => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('SCAFLASWD',   IID,IRESP); XSCAFLASWD    => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DIRSRFSWD',   IID,IRESP); XDIRSRFSWD    => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('CLEARCOL_TM1',IID,IRESP); NCLEARCOL_TM1 => TFIELDLIST(IID)%TFIELD_N2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('ZENITH',      IID,IRESP); XZENITH       => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('AZIM',        IID,IRESP); XAZIM         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DIR_ALB',     IID,IRESP); XDIR_ALB      => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('SCA_ALB',     IID,IRESP); XSCA_ALB      => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('EMIS',        IID,IRESP); XEMIS         => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('TSRAD',       IID,IRESP); XTSRAD        => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
END IF
!
! MODD_DEEP_CONVECTION_n variables
!
IF (TRIM(CPROGRAM) /= 'PGD' .AND. TRIM(CPROGRAM) /= 'NESPGD' .AND. TRIM(CPROGRAM) /= 'SPAWN') THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('COUNTCONV',  IID,IRESP); NCOUNTCONV       => TFIELDLIST(IID)%TFIELD_N2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DTHCONV',    IID,IRESP); XDTHCONV         => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DRVCONV',    IID,IRESP); XDRVCONV         => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DRCCONV',    IID,IRESP); XDRCCONV         => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DRICONV',    IID,IRESP); XDRICONV         => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRCONV',     IID,IRESP); XPRCONV          => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('PACCONV',    IID,IRESP); XPACCONV         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRSCONV',    IID,IRESP); XPRSCONV         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DSVCONV',    IID,IRESP); XDSVCONV         => TFIELDLIST(IID)%TFIELD_X4D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRLFLXCONV', IID,IRESP); XPRLFLXCONV      => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRSFLXCONV', IID,IRESP); XPRSFLXCONV      => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('UMFCONV',    IID,IRESP); XUMFCONV         => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('DMFCONV',    IID,IRESP); XDMFCONV         => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('MFCONV',     IID,IRESP); XMFCONV          => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('CAPE',       IID,IRESP); XCAPE            => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('CLTOPCONV_LVL',IID,IRESP); NCLTOPCONV     => TFIELDLIST(IID)%TFIELD_N2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('CLBASCONV_LVL',IID,IRESP); NCLBASCONV     => TFIELDLIST(IID)%TFIELD_N2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('IC_RATE',    IID,IRESP); XIC_RATE         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('CG_RATE',    IID,IRESP); XCG_RATE         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('IC_TOTAL_NB',IID,IRESP); XIC_TOTAL_NUMBER => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('CG_TOTAL_NB',IID,IRESP); XCG_TOTAL_NUMBER => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
END IF
!
! MODD_GR_FIELD_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('SSO_ANIS', IID,IRESP); XSSO_ANISOTROPY => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SSO_SLOPE',IID,IRESP); XSSO_SLOPE      => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SSO_DIR',  IID,IRESP); XSSO_DIRECTION  => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('AVG_ZS',   IID,IRESP); XAVG_ZS         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SIL_ZS',   IID,IRESP); XSIL_ZS         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('MAX_ZS',   IID,IRESP); XMAX_ZS         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('MIN_ZS',   IID,IRESP); XMIN_ZS         => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('SSO_STDEV',IID,IRESP); XSSO_STDEV      => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
!
! MODD_PRECIP_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRC',  IID,IRESP); XINPRC   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRC',  IID,IRESP); XACPRC   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('INDEP',  IID,IRESP); XINDEP   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ACDEP',  IID,IRESP); XACDEP   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRR',  IID,IRESP); XINPRR   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRR3D',IID,IRESP); XINPRR3D => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('EVAP3D', IID,IRESP); XEVAP3D  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRR',  IID,IRESP); XACPRR   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRS',  IID,IRESP); XINPRS   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRS',  IID,IRESP); XACPRS   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRG',  IID,IRESP); XINPRG   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRG',  IID,IRESP); XACPRG   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('INPRH',  IID,IRESP); XINPRH   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('ACPRH',  IID,IRESP); XACPRH   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
!
!
! MODD_DEF_EDDY_FLUX_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('VT_FLX',        IID,IRESP); XVTH_FLUX_M     => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('WT_FLX',        IID,IRESP); XWTH_FLUX_M     => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('RTHS_EDDY_FLUX',IID,IRESP); XRTHS_EDDY_FLUX => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
! MODD_DEF_EDDYUV_FLUX_n variables
!
CALL FIND_FIELD_ID_FROM_MNHNAME('VU_FLX',       IID,IRESP); XVU_FLUX_M     => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
CALL FIND_FIELD_ID_FROM_MNHNAME('RVS_EDDY_FLUX',IID,IRESP); XRVS_EDDY_FLUX => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
!
!
! MODD_HURR_FIELD_n variables
!
IF (CPROGRAM=='REAL') THEN
  CALL FIND_FIELD_ID_FROM_MNHNAME('UT15',   IID,IRESP); XUTOT   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('VT15',   IID,IRESP); XVTOT   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('TEMPTOT',IID,IRESP); XTTOT   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRESTOT',IID,IRESP); XPTOT   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('HUMTOT', IID,IRESP); XQTOT   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('UT16',   IID,IRESP); XUENV   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('VT16',   IID,IRESP); XVENV   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('TEMPENV',IID,IRESP); XTENV   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRESENV',IID,IRESP); XPENV   => TFIELDLIST(IID)%TFIELD_X2D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('HUMENV', IID,IRESP); XQENV   => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('UT17',   IID,IRESP); XUBASIC => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('VT17',   IID,IRESP); XVBASIC => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('TEMPBAS',IID,IRESP); XTBASIC => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('PRESBAS',IID,IRESP); XPBASIC => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('HUMBAS', IID,IRESP); XQBASIC => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
  CALL FIND_FIELD_ID_FROM_MNHNAME('VTDIS',  IID,IRESP); XVTDIS  => TFIELDLIST(IID)%TFIELD_X3D(KTO)%DATA
END IF
!
END IF !KFROM/=KTO
!
END SUBROUTINE FIELDLIST_GOTO_MODEL


subroutine Add_field2list( tpfield )

implicit none

type(tfielddata) :: tpfield

CHARACTER(LEN=42) :: YMSG

if ( nfields_used >= MAXFIELDS ) then
  WRITE(YMSG,'( "nfields_used>=MAXFIELDS (",I5,")" )') MAXFIELDS
  CALL PRINT_MSG(NVERB_FATAL,'GEN','INI_FIELD_LIST',TRIM(YMSG))
end if

nfields_used = nfields_used + 1

tfieldlist(nfields_used) = tpfield

end subroutine Add_field2list


subroutine Goto_model_1field_c0d( hname, kfrom, kto, pdata )

implicit none

character(len=*),          intent(in)    :: hname
integer,                   intent(in)    :: kfrom
integer,                   intent(in)    :: kto
character(len=*), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_c0d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_c0d(kfrom)%data => pdata
if ( kfrom /= kto ) then
  if ( .not. Associated( tfieldlist(iid)%tfield_c0d(kto)%data ) ) then
    Allocate( character(len=Len(pdata)) :: tfieldlist(iid)%tfield_c0d(kto)%data )
    tfieldlist(iid)%tfield_c0d(kto)%data(:) = ''
  end if
  pdata => tfieldlist(iid)%tfield_c0d(kto)%data
end if

end subroutine Goto_model_1field_c0d


subroutine Goto_model_1field_c1d( hname, kfrom, kto, pdata )

implicit none

character(len=*),                        intent(in)    :: hname
integer,                                 intent(in)    :: kfrom
integer,                                 intent(in)    :: kto
character(len=*), dimension(:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp
integer :: ji

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_c1d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_c1d(kfrom)%data => pdata
if ( kfrom /= kto ) then
  if ( .not. Associated( tfieldlist(iid)%tfield_c1d(kto)%data ) ) then
    Allocate( character(len=Len(pdata)) :: tfieldlist(iid)%tfield_c1d(kto)%data(Size(pdata)) )
    do ji = 1, Size(pdata)
      tfieldlist(iid)%tfield_c1d(kto)%data(ji) = ''
    end do
  end if
  pdata => tfieldlist(iid)%tfield_c1d(kto)%data
end if

end subroutine Goto_model_1field_c1d


subroutine Goto_model_1field_l0d( hname, kfrom, kto, pdata )

implicit none

character(len=*),          intent(in)    :: hname
integer,                   intent(in)    :: kfrom
integer,                   intent(in)    :: kto
logical,          pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_l0d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_l0d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_l0d(kto)%data

end subroutine Goto_model_1field_l0d


subroutine Goto_model_1field_l1d( hname, kfrom, kto, pdata )

implicit none

character(len=*),               intent(in)    :: hname
integer,                        intent(in)    :: kfrom
integer,                        intent(in)    :: kto
logical, dimension(:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_l1d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_l1d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_l1d(kto)%data

end subroutine Goto_model_1field_l1d


subroutine Goto_model_1field_n0d( hname, kfrom, kto, pdata )

implicit none

character(len=*),          intent(in)    :: hname
integer,                   intent(in)    :: kfrom
integer,                   intent(in)    :: kto
integer,          pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_n0d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_n0d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_n0d(kto)%data

end subroutine Goto_model_1field_n0d


subroutine Goto_model_1field_n1d( hname, kfrom, kto, pdata )

implicit none

character(len=*),               intent(in)    :: hname
integer,                        intent(in)    :: kfrom
integer,                        intent(in)    :: kto
integer, dimension(:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_n1d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_n1d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_n1d(kto)%data

end subroutine Goto_model_1field_n1d


subroutine Goto_model_1field_n2d( hname, kfrom, kto, pdata )

implicit none

character(len=*),                 intent(in)    :: hname
integer,                          intent(in)    :: kfrom
integer,                          intent(in)    :: kto
integer, dimension(:,:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_n2d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_n2d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_n2d(kto)%data

end subroutine Goto_model_1field_n2d


subroutine Goto_model_1field_n3d( hname, kfrom, kto, pdata )

implicit none

character(len=*),                   intent(in)    :: hname
integer,                            intent(in)    :: kfrom
integer,                            intent(in)    :: kto
integer, dimension(:,:,:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_n3d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_n3d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_n3d(kto)%data

end subroutine Goto_model_1field_n3d


subroutine Goto_model_1field_t0d( hname, kfrom, kto, pdata )

use modd_type_date, only: date_time

implicit none

character(len=*),          intent(in)    :: hname
integer,                   intent(in)    :: kfrom
integer,                   intent(in)    :: kto
type(date_time),  pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_t0d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_t0d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_t0d(kto)%data

end subroutine Goto_model_1field_t0d


subroutine Goto_model_1field_t1d( hname, kfrom, kto, pdata )

use modd_type_date, only: date_time

implicit none

character(len=*),                       intent(in)    :: hname
integer,                                intent(in)    :: kfrom
integer,                                intent(in)    :: kto
type(date_time), dimension(:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_t1d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_t1d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_t1d(kto)%data

end subroutine Goto_model_1field_t1d


subroutine Goto_model_1field_x0d( hname, kfrom, kto, pdata )

implicit none

character(len=*),          intent(in)    :: hname
integer,                   intent(in)    :: kfrom
integer,                   intent(in)    :: kto
real,             pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_x0d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_x0d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_x0d(kto)%data

end subroutine Goto_model_1field_x0d


subroutine Goto_model_1field_x1d( hname, kfrom, kto, pdata )

implicit none

character(len=*),               intent(in)    :: hname
integer,                        intent(in)    :: kfrom
integer,                        intent(in)    :: kto
real,    dimension(:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_x1d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_x1d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_x1d(kto)%data

end subroutine Goto_model_1field_x1d


subroutine Goto_model_1field_x2d( hname, kfrom, kto, pdata )

implicit none

character(len=*),                 intent(in)    :: hname
integer,                          intent(in)    :: kfrom
integer,                          intent(in)    :: kto
real,    dimension(:,:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_x2d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_x2d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_x2d(kto)%data

end subroutine Goto_model_1field_x2d


subroutine Goto_model_1field_x3d( hname, kfrom, kto, pdata )

implicit none

character(len=*),                   intent(in)    :: hname
integer,                            intent(in)    :: kfrom
integer,                            intent(in)    :: kto
real,    dimension(:,:,:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_x3d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_x3d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_x3d(kto)%data

end subroutine Goto_model_1field_x3d


subroutine Goto_model_1field_x4d( hname, kfrom, kto, pdata )

implicit none

character(len=*),                     intent(in)    :: hname
integer,                              intent(in)    :: kfrom
integer,                              intent(in)    :: kto
real,    dimension(:,:,:,:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_x4d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_x4d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_x4d(kto)%data

end subroutine Goto_model_1field_x4d


subroutine Goto_model_1field_x5d( hname, kfrom, kto, pdata )

implicit none

character(len=*),                       intent(in)    :: hname
integer,                                intent(in)    :: kfrom
integer,                                intent(in)    :: kto
real,    dimension(:,:,:,:,:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_x5d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_x5d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_x5d(kto)%data

end subroutine Goto_model_1field_x5d


subroutine Goto_model_1field_x6d( hname, kfrom, kto, pdata )

implicit none

character(len=*),                         intent(in)    :: hname
integer,                                  intent(in)    :: kfrom
integer,                                  intent(in)    :: kto
real,    dimension(:,:,:,:,:,:), pointer, intent(inout) :: pdata

integer :: iid
integer :: iresp

call Find_field_id_from_mnhname( hname, iid, iresp )

call Extend_1field_x6d( tfieldlist(iid), Max( kfrom, kto ) )

tfieldlist(iid)%tfield_x6d(kfrom)%data => pdata
if ( kfrom /= kto ) pdata => tfieldlist(iid)%tfield_x6d(kto)%data

end subroutine Goto_model_1field_x6d


subroutine Extend_1field_c0d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_c0d), dimension(:), allocatable :: tzfield_c0d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_c0d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_c0d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_c0d(ji)%data => null()
    end do
  else
    Allocate( tzfield_c0d(ksize) )
    do ji = 1, Size( tpfield%tfield_c0d)
      tzfield_c0d(ji)%data => tpfield%tfield_c0d(ji)%data
    end do
    do ji = Size( tpfield%tfield_c0d) + 1, ksize
      tzfield_c0d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_c0d, to = tpfield%tfield_c0d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_c0d


subroutine Extend_1field_c1d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_c1d), dimension(:), allocatable :: tzfield_c1d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_c1d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_c1d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_c1d(ji)%data => null()
    end do
  else
    Allocate( tzfield_c1d(ksize) )
    do ji = 1, Size( tpfield%tfield_c1d)
      tzfield_c1d(ji)%data => tpfield%tfield_c1d(ji)%data
    end do
    do ji = Size( tpfield%tfield_c1d) + 1, ksize
      tzfield_c1d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_c1d, to = tpfield%tfield_c1d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_c1d


subroutine Extend_1field_l0d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_l0d), dimension(:), allocatable :: tzfield_l0d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_l0d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_l0d(ksize) )
    do ji = 1, ksize
      ! tpfield%tfield_l0d(ji)%data => null()
      Allocate( tpfield%tfield_l0d(ji)%data )
      tpfield%tfield_l0d(ji)%data = .false.
    end do
  else
    Allocate( tzfield_l0d(ksize) )
    do ji = 1, Size( tpfield%tfield_l0d)
      tzfield_l0d(ji)%data => tpfield%tfield_l0d(ji)%data
    end do
    do ji = Size( tpfield%tfield_l0d) + 1, ksize
      ! tzfield_l0d(ji)%data => null()
      Allocate( tzfield_l0d(ji)%data )
      tzfield_l0d(ji)%data = .false.
    end do
    call Move_alloc( from = tzfield_l0d, to = tpfield%tfield_l0d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_l0d


subroutine Extend_1field_l1d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_l1d), dimension(:), allocatable :: tzfield_l1d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_l1d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_l1d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_l1d(ji)%data => null()
    end do
  else
    Allocate( tzfield_l1d(ksize) )
    do ji = 1, Size( tpfield%tfield_l1d)
      tzfield_l1d(ji)%data => tpfield%tfield_l1d(ji)%data
    end do
    do ji = Size( tpfield%tfield_l1d) + 1, ksize
      tzfield_l1d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_l1d, to = tpfield%tfield_l1d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_l1d


subroutine Extend_1field_n0d( tpfield, ksize )

use modd_parameters, only: NUNDEF

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_n0d), dimension(:), allocatable :: tzfield_n0d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_n0d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_n0d(ksize) )
    do ji = 1, ksize
      ! tpfield%tfield_n0d(ji)%data => null()
      Allocate( tpfield%tfield_n0d(ji)%data )
      tpfield%tfield_n0d(ji)%data = NUNDEF
    end do
  else
    Allocate( tzfield_n0d(ksize) )
    do ji = 1, Size( tpfield%tfield_n0d)
      tzfield_n0d(ji)%data => tpfield%tfield_n0d(ji)%data
    end do
    do ji = Size( tpfield%tfield_n0d) + 1, ksize
      ! tzfield_n0d(ji)%data => null()
      Allocate( tzfield_n0d(ji)%data )
      tzfield_n0d(ji)%data = NUNDEF
    end do
    call Move_alloc( from = tzfield_n0d, to = tpfield%tfield_n0d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_n0d


subroutine Extend_1field_n1d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_n1d), dimension(:), allocatable :: tzfield_n1d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_n1d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_n1d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_n1d(ji)%data => null()
    end do
  else
    Allocate( tzfield_n1d(ksize) )
    do ji = 1, Size( tpfield%tfield_n1d)
      tzfield_n1d(ji)%data => tpfield%tfield_n1d(ji)%data
    end do
    do ji = Size( tpfield%tfield_n1d) + 1, ksize
      tzfield_n1d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_n1d, to = tpfield%tfield_n1d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_n1d


subroutine Extend_1field_n2d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_n2d), dimension(:), allocatable :: tzfield_n2d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_n2d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_n2d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_n2d(ji)%data => null()
    end do
  else
    Allocate( tzfield_n2d(ksize) )
    do ji = 1, Size( tpfield%tfield_n2d)
      tzfield_n2d(ji)%data => tpfield%tfield_n2d(ji)%data
    end do
    do ji = Size( tpfield%tfield_n2d) + 1, ksize
      tzfield_n2d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_n2d, to = tpfield%tfield_n2d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_n2d


subroutine Extend_1field_n3d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_n3d), dimension(:), allocatable :: tzfield_n3d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_n3d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_n3d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_n3d(ji)%data => null()
    end do
  else
    Allocate( tzfield_n3d(ksize) )
    do ji = 1, Size( tpfield%tfield_n3d)
      tzfield_n3d(ji)%data => tpfield%tfield_n3d(ji)%data
    end do
    do ji = Size( tpfield%tfield_n3d) + 1, ksize
      tzfield_n3d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_n3d, to = tpfield%tfield_n3d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_n3d


subroutine Extend_1field_t0d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_t0d), dimension(:), allocatable :: tzfield_t0d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_t0d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_t0d(ksize) )
    do ji = 1, ksize
      ! tpfield%tfield_t0d(ji)%data => null()
      Allocate( tpfield%tfield_t0d(ji)%data )
    end do
  else
    Allocate( tzfield_t0d(ksize) )
    do ji = 1, Size( tpfield%tfield_t0d)
      tzfield_t0d(ji)%data => tpfield%tfield_t0d(ji)%data
    end do
    do ji = Size( tpfield%tfield_t0d) + 1, ksize
      ! tzfield_t0d(ji)%data => null()
      Allocate( tzfield_t0d(ji)%data )
    end do
    call Move_alloc( from = tzfield_t0d, to = tpfield%tfield_t0d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_t0d


subroutine Extend_1field_t1d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_t1d), dimension(:), allocatable :: tzfield_t1d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_t1d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_t1d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_t1d(ji)%data => null()
    end do
  else
    Allocate( tzfield_t1d(ksize) )
    do ji = 1, Size( tpfield%tfield_t1d)
      tzfield_t1d(ji)%data => tpfield%tfield_t1d(ji)%data
    end do
    do ji = Size( tpfield%tfield_t1d) + 1, ksize
      tzfield_t1d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_t1d, to = tpfield%tfield_t1d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_t1d


subroutine Extend_1field_x0d( tpfield, ksize )

use modd_parameters, only: XUNDEF

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_x0d), dimension(:), allocatable :: tzfield_x0d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_x0d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_x0d(ksize) )
    do ji = 1, ksize
      ! tpfield%tfield_x0d(ji)%data => null()
      Allocate( tpfield%tfield_x0d(ji)%data )
      tpfield%tfield_x0d(ji)%data = XUNDEF
    end do
  else
    Allocate( tzfield_x0d(ksize) )
    do ji = 1, Size( tpfield%tfield_x0d)
      tzfield_x0d(ji)%data => tpfield%tfield_x0d(ji)%data
    end do
    do ji = Size( tpfield%tfield_x0d) + 1, ksize
      ! tzfield_x0d(ji)%data => null()
      Allocate( tzfield_x0d(ji)%data )
      tzfield_x0d(ji)%data = XUNDEF
    end do
    call Move_alloc( from = tzfield_x0d, to = tpfield%tfield_x0d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_x0d


subroutine Extend_1field_x1d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_x1d), dimension(:), allocatable :: tzfield_x1d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_x1d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_x1d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_x1d(ji)%data => null()
    end do
  else
    Allocate( tzfield_x1d(ksize) )
    do ji = 1, Size( tpfield%tfield_x1d)
      tzfield_x1d(ji)%data => tpfield%tfield_x1d(ji)%data
    end do
    do ji = Size( tpfield%tfield_x1d) + 1, ksize
      tzfield_x1d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_x1d, to = tpfield%tfield_x1d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_x1d


subroutine Extend_1field_x2d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_x2d), dimension(:), allocatable :: tzfield_x2d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_x2d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_x2d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_x2d(ji)%data => null()
    end do
  else
    Allocate( tzfield_x2d(ksize) )
    do ji = 1, Size( tpfield%tfield_x2d)
      tzfield_x2d(ji)%data => tpfield%tfield_x2d(ji)%data
    end do
    do ji = Size( tpfield%tfield_x2d) + 1, ksize
      tzfield_x2d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_x2d, to = tpfield%tfield_x2d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_x2d


subroutine Extend_1field_x3d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_x3d), dimension(:), allocatable :: tzfield_x3d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_x3d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_x3d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_x3d(ji)%data => null()
    end do
  else
    Allocate( tzfield_x3d(ksize) )
    do ji = 1, Size( tpfield%tfield_x3d)
      tzfield_x3d(ji)%data => tpfield%tfield_x3d(ji)%data
    end do
    do ji = Size( tpfield%tfield_x3d) + 1, ksize
      tzfield_x3d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_x3d, to = tpfield%tfield_x3d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_x3d


subroutine Extend_1field_x4d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_x4d), dimension(:), allocatable :: tzfield_x4d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_x4d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_x4d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_x4d(ji)%data => null()
    end do
  else
    Allocate( tzfield_x4d(ksize) )
    do ji = 1, Size( tpfield%tfield_x4d)
      tzfield_x4d(ji)%data => tpfield%tfield_x4d(ji)%data
    end do
    do ji = Size( tpfield%tfield_x4d) + 1, ksize
      tzfield_x4d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_x4d, to = tpfield%tfield_x4d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_x4d


subroutine Extend_1field_x5d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_x5d), dimension(:), allocatable :: tzfield_x5d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_x5d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_x5d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_x5d(ji)%data => null()
    end do
  else
    Allocate( tzfield_x5d(ksize) )
    do ji = 1, Size( tpfield%tfield_x5d)
      tzfield_x5d(ji)%data => tpfield%tfield_x5d(ji)%data
    end do
    do ji = Size( tpfield%tfield_x5d) + 1, ksize
      tzfield_x5d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_x5d, to = tpfield%tfield_x5d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_x5d


subroutine Extend_1field_x6d( tpfield, ksize )

implicit none

type(tfielddata), intent(inout) :: tpfield
integer,          intent(in)    :: ksize

integer :: ji
type(tfieldptr_x6d), dimension(:), allocatable :: tzfield_x6d

if ( tpfield%nmodelmax < 0 ) then
  !nmodelmax is < 0 if the allocation of the field has been done by hand
  !(not using a constructor, default value of nmodelmax)
  !The correct value of nmodelmax is hence computed here
  tpfield%nmodelmax = Size( tpfield%tfield_x6d )
end if

if ( ksize > tpfield%nmodelmax ) then
  if ( tpfield%nmodelmax == 0 ) then
    Allocate( tpfield%tfield_x6d(ksize) )
    do ji = 1, ksize
      tpfield%tfield_x6d(ji)%data => null()
    end do
  else
    Allocate( tzfield_x6d(ksize) )
    do ji = 1, Size( tpfield%tfield_x6d)
      tzfield_x6d(ji)%data => tpfield%tfield_x6d(ji)%data
    end do
    do ji = Size( tpfield%tfield_x6d) + 1, ksize
      tzfield_x6d(ji)%data => null()
    end do
    call Move_alloc( from = tzfield_x6d, to = tpfield%tfield_x6d )
  end if
  tpfield%nmodelmax = ksize
end if

end subroutine Extend_1field_x6d


subroutine Fieldlist_nmodel_resize( kmodelnew )

implicit none

integer, intent(in) :: kmodelnew

character(len=10) :: ymsg
integer           :: imodelmax
integer           :: ji, jj
type(tfieldptr_c0d), dimension(:), allocatable :: tfield_save_c0d
type(tfieldptr_c1d), dimension(:), allocatable :: tfield_save_c1d
type(tfieldptr_l0d), dimension(:), allocatable :: tfield_save_l0d
type(tfieldptr_l1d), dimension(:), allocatable :: tfield_save_l1d
type(tfieldptr_n0d), dimension(:), allocatable :: tfield_save_n0d
type(tfieldptr_n1d), dimension(:), allocatable :: tfield_save_n1d
type(tfieldptr_n2d), dimension(:), allocatable :: tfield_save_n2d
type(tfieldptr_n3d), dimension(:), allocatable :: tfield_save_n3d
type(tfieldptr_x0d), dimension(:), allocatable :: tfield_save_x0d
type(tfieldptr_x1d), dimension(:), allocatable :: tfield_save_x1d
type(tfieldptr_x2d), dimension(:), allocatable :: tfield_save_x2d
type(tfieldptr_x3d), dimension(:), allocatable :: tfield_save_x3d
type(tfieldptr_x4d), dimension(:), allocatable :: tfield_save_x4d
type(tfieldptr_x5d), dimension(:), allocatable :: tfield_save_x5d
type(tfieldptr_x6d), dimension(:), allocatable :: tfield_save_x6d
type(tfieldptr_t0d), dimension(:), allocatable :: tfield_save_t0d

write( ymsg, '( i4,"->",i4 )') nmodel_allocated, kmodelnew
call Print_msg( NVERB_DEBUG, 'GEN', 'Fieldlist_nmodel_resize', trim( ymsg ) )

!Nothing to do
if ( kmodelnew == nmodel_allocated ) return

imodelmax = max( kmodelnew, nmodel_allocated )

allocate( tfield_save_c0d( imodelmax ) )
allocate( tfield_save_c1d( imodelmax ) )
allocate( tfield_save_l0d( imodelmax ) )
allocate( tfield_save_l1d( imodelmax ) )
allocate( tfield_save_n0d( imodelmax ) )
allocate( tfield_save_n1d( imodelmax ) )
allocate( tfield_save_n2d( imodelmax ) )
allocate( tfield_save_n3d( imodelmax ) )
allocate( tfield_save_x0d( imodelmax ) )
allocate( tfield_save_x1d( imodelmax ) )
allocate( tfield_save_x2d( imodelmax ) )
allocate( tfield_save_x3d( imodelmax ) )
allocate( tfield_save_x4d( imodelmax ) )
allocate( tfield_save_x5d( imodelmax ) )
allocate( tfield_save_x6d( imodelmax ) )
allocate( tfield_save_t0d( imodelmax ) )

do ji = 1, imodelmax
  tfield_save_c0d(ji)%data => null()
  tfield_save_c1d(ji)%data => null()
  tfield_save_l0d(ji)%data => null()
  tfield_save_l1d(ji)%data => null()
  tfield_save_n0d(ji)%data => null()
  tfield_save_n1d(ji)%data => null()
  tfield_save_n2d(ji)%data => null()
  tfield_save_n3d(ji)%data => null()
  tfield_save_x0d(ji)%data => null()
  tfield_save_x1d(ji)%data => null()
  tfield_save_x2d(ji)%data => null()
  tfield_save_x3d(ji)%data => null()
  tfield_save_x4d(ji)%data => null()
  tfield_save_x5d(ji)%data => null()
  tfield_save_x6d(ji)%data => null()
  tfield_save_t0d(ji)%data => null()
end do

do ji = 1, size( tfieldlist )
  if ( allocated( tfieldlist(ji)%tfield_c0d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_c0d(jj)%data => tfieldlist(ji)%tfield_c0d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_c0d )
    allocate(   tfieldlist(ji)%tfield_c0d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_c0d(jj)%data => tfield_save_c0d(jj)%data
      tfield_save_c0d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_c0d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_c0d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_c1d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_c1d(jj)%data => tfieldlist(ji)%tfield_c1d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_c1d )
    allocate(   tfieldlist(ji)%tfield_c1d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_c1d(jj)%data => tfield_save_c1d(jj)%data
      tfield_save_c1d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_c1d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_c1d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_l0d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_l0d(jj)%data => tfieldlist(ji)%tfield_l0d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_l0d )
    allocate(   tfieldlist(ji)%tfield_l0d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_l0d(jj)%data => tfield_save_l0d(jj)%data
      tfield_save_l0d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_l0d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_l0d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_l1d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_l1d(jj)%data => tfieldlist(ji)%tfield_l1d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_l1d )
    allocate(   tfieldlist(ji)%tfield_l1d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_l1d(jj)%data => tfield_save_l1d(jj)%data
      tfield_save_l1d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_l1d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_l1d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_n0d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_n0d(jj)%data => tfieldlist(ji)%tfield_n0d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_n0d )
    allocate(   tfieldlist(ji)%tfield_n0d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_n0d(jj)%data => tfield_save_n0d(jj)%data
      tfield_save_n0d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_n0d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_n0d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_n1d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_n1d(jj)%data => tfieldlist(ji)%tfield_n1d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_n1d )
    allocate(   tfieldlist(ji)%tfield_n1d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_n1d(jj)%data => tfield_save_n1d(jj)%data
      tfield_save_n1d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_n1d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_n1d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_n2d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_n2d(jj)%data => tfieldlist(ji)%tfield_n2d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_n2d )
    allocate(   tfieldlist(ji)%tfield_n2d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_n2d(jj)%data => tfield_save_n2d(jj)%data
      tfield_save_n2d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_n2d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_n2d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_n3d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_n3d(jj)%data => tfieldlist(ji)%tfield_n3d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_n3d )
    allocate(   tfieldlist(ji)%tfield_n3d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_n3d(jj)%data => tfield_save_n3d(jj)%data
      tfield_save_n3d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_n3d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_n3d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_x0d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_x0d(jj)%data => tfieldlist(ji)%tfield_x0d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_x0d )
    allocate(   tfieldlist(ji)%tfield_x0d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_x0d(jj)%data => tfield_save_x0d(jj)%data
      tfield_save_x0d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_x0d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_x0d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_x1d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_x1d(jj)%data => tfieldlist(ji)%tfield_x1d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_x1d )
    allocate(   tfieldlist(ji)%tfield_x1d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_x1d(jj)%data => tfield_save_x1d(jj)%data
      tfield_save_x1d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_x1d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_x1d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_x2d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_x2d(jj)%data => tfieldlist(ji)%tfield_x2d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_x2d )
    allocate(   tfieldlist(ji)%tfield_x2d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_x2d(jj)%data => tfield_save_x2d(jj)%data
      tfield_save_x2d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_x2d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_x2d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_x3d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_x3d(jj)%data => tfieldlist(ji)%tfield_x3d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_x3d )
    allocate(   tfieldlist(ji)%tfield_x3d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_x3d(jj)%data => tfield_save_x3d(jj)%data
      tfield_save_x3d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_x3d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_x3d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_x4d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_x4d(jj)%data => tfieldlist(ji)%tfield_x4d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_x4d )
    allocate(   tfieldlist(ji)%tfield_x4d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_x4d(jj)%data => tfield_save_x4d(jj)%data
      tfield_save_x4d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_x4d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_x4d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_x5d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_x5d(jj)%data => tfieldlist(ji)%tfield_x5d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_x5d )
    allocate(   tfieldlist(ji)%tfield_x5d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_x5d(jj)%data => tfield_save_x5d(jj)%data
      tfield_save_x5d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_x5d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_x5d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_x6d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_x6d(jj)%data => tfieldlist(ji)%tfield_x6d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_x6d )
    allocate(   tfieldlist(ji)%tfield_x6d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_x6d(jj)%data => tfield_save_x6d(jj)%data
      tfield_save_x6d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_x6d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_x6d(jj)%data => null()
      end if
    end do
  end if

  if ( allocated( tfieldlist(ji)%tfield_t0d ) ) then
    !Save existing pointers to temporary structure
    do jj = 1, nmodel_allocated
      tfield_save_t0d(jj)%data => tfieldlist(ji)%tfield_t0d(jj)%data
    end do
    !Reallocate
    deallocate( tfieldlist(ji)%tfield_t0d )
    allocate(   tfieldlist(ji)%tfield_t0d(kmodelnew) )
    !Restore pointers
    do jj = 1, kmodelnew
      tfieldlist(ji)%tfield_t0d(jj)%data => tfield_save_t0d(jj)%data
      tfield_save_t0d(jj)%data => null()
    end do
    !Check no used pointers if nmodel is decreazed
    do jj = kmodelnew + 1, nmodel_allocated
      if ( associated(tfield_save_t0d(jj)%data ) ) then
        call Print_msg( NVERB_ERROR, 'GEN', 'Fieldlist_nmodel_resize', 'data loss due to reduction in number of models' )
        tfield_save_t0d(jj)%data => null()
      end if
    end do
  end if

end do

nmodel_allocated = kmodelnew

end subroutine Fieldlist_nmodel_resize

end module mode_field
