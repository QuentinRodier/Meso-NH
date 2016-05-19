!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 chimie 2006/05/18 13:07:25
!-----------------------------------------------------------------
!!    ###########################
MODULE MODI_CH_AER_INTERMIN
!!    ###########################
!!
INTERFACE
!!
  SUBROUTINE CH_AER_INTERMIN(v, w, x, y, z,zout,KVECNPT)
  USE MODD_CH_AEROSOL
  IMPLICIT NONE
  INTEGER :: KVECNPT
  REAL, DIMENSION(:) :: v, w, x, y, z
  REAL, DIMENSION(:,:) :: zout
  END SUBROUTINE CH_AER_INTERMIN
!!
END INTERFACE
!!
END MODULE MODI_CH_AER_INTERMIN
!     ######spl
! Five variable interpolation program 
!
SUBROUTINE CH_AER_INTERMIN(v, w, x, y, z,zout,KVECNPT)
!include 'chimere.h' !! rhi,tempi,zsu,znh,zni
USE MODD_CH_AEROSOL
IMPLICIT NONE

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INPUT
!   v,w,x,y,z : Coodinates
! OUTPUT
!   zout      : interpolation results (array)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER :: KVECNPT
INTEGER, PARAMETER :: nc=22
INTEGER, PARAMETER :: nh=16
INTEGER, PARAMETER :: nt=11
INTEGER, DIMENSION(KVECNPT) :: nv0,nv1,nw0,nw1,nx0,nx1,ny0,ny1,nz0,nz1
INTEGER :: i,IP
REAL :: v0,v1,w0,w1,x0,x1,y0,y1,z0,z1,ans
REAL, DIMENSION(:) :: v, w, x, y, z
REAL :: xv0, xw0, xx0, xy0, xz0
REAL :: xv1, xw1, xx1, xy1, xz1
REAL, DIMENSION(KVECNPT) :: vsav, wsav, xsav, ysav, zsav
REAL, DIMENSION(:,:) :: zout
REAL :: tempimin,tempimax,zsumin,znhmin, znimin
!
vsav=v
wsav=w
ysav=y
xsav=x
zsav=z
!rhimin=MINVAL(rhi(:))
!rhimax=MAXVAL(rhi(:))
tempimin=MINVAL(tempi(:))
tempimax=MAXVAL(tempi(:))
zsumin=MINVAL(zsu(:))
!zsumax=MAXVAL(zsu(:))
znhmin=MINVAL(znh(:))
!znhmax=MAXVAL(znh(:))
znimin=MINVAL(zni(:))
!znimax=MAXVAL(zni(:))
nv0(:)=MIN(nh-1,MAX(1,MAX(INT((v(:)-0.25)/0.05),INT((v(:)-0.67)/0.02))))
nw0(:)=MIN(nt-1,MAX(1,INT((nt-1)*(w(:)-tempimin)/(tempimax-tempimin))+1))
nx0(:)=MIN(nc-1,MAX(1,INT(LOG(MAX(x(:),zsumin)/0.0066)/0.4187)))
ny0(:)=MIN(nc-1,MAX(1,INT(LOG(MAX(y(:),znhmin)/0.0066)/0.4187)))
nz0(:)=MIN(nc-1,MAX(1,INT(LOG(MAX(z(:),znimin)/0.0066)/0.4187)))
!
nv1=nv0+1
nw1=nw0+1
nx1=nx0+1
ny1=ny0+1
nz1=nz0+1

DO i=1,3
  DO IP=1,KVECNPT
    v0=rhi(nv0(IP)) ; v1=rhi(nv1(IP))
    w0=tempi(nw0(IP)) ; w1=tempi(nw1(IP))
    x0=zsu(nx0(IP)) ; x1=zsu(nx1(IP))
    y0=znh(ny0(IP)) ; y1=znh(ny1(IP))
    z0=zni(nz0(IP)) ; z1=zni(nz1(IP))
    xv0 = (v(IP)-v1)/(v0-v1)
    xw0 = (w(IP)-w1)/(w0-w1)
    xx0 = (x(IP)-x1)/(x0-x1)
    xy0 = (y(IP)-y1)/(y0-y1)
    xz0 = (z(IP)-z1)/(z0-z1)
    xv1=1-xv0
    xw1=1-xw0
    xx1=1-xx0
    xy1=1-xy0
    xz1=1-xz0
    !
    ans= 0.0d+00
    !
    ans = ans + zf(nv0(IP),nw0(IP),nx0(IP),ny0(IP),nz0(IP),i)*(xv0)*(xw0)*(xx0)*(xy0)*(xz0)
    ans = ans + zf(nv0(IP),nw0(IP),nx0(IP),ny0(IP),nz1(IP),i)*(xv0)*(xw0)*(xx0)*(xy0)*(xz1)
    ans = ans + zf(nv0(IP),nw0(IP),nx0(IP),ny1(IP),nz0(IP),i)*(xv0)*(xw0)*(xx0)*(xy1)*(xz0)
    ans = ans + zf(nv0(IP),nw0(IP),nx0(IP),ny1(IP),nz1(IP),i)*(xv0)*(xw0)*(xx0)*(xy1)*(xz1)
    ans = ans + zf(nv0(IP),nw0(IP),nx1(IP),ny0(IP),nz0(IP),i)*(xv0)*(xw0)*(xx1)*(xy0)*(xz0)
    ans = ans + zf(nv0(IP),nw0(IP),nx1(IP),ny0(IP),nz1(IP),i)*(xv0)*(xw0)*(xx1)*(xy0)*(xz1)
    ans = ans + zf(nv0(IP),nw0(IP),nx1(IP),ny1(IP),nz0(IP),i)*(xv0)*(xw0)*(xx1)*(xy1)*(xz0)
    ans = ans + zf(nv0(IP),nw0(IP),nx1(IP),ny1(IP),nz1(IP),i)*(xv0)*(xw0)*(xx1)*(xy1)*(xz1)
    ans = ans + zf(nv0(IP),nw1(IP),nx0(IP),ny0(IP),nz0(IP),i)*(xv0)*(xw1)*(xx0)*(xy0)*(xz0)
    ans = ans + zf(nv0(IP),nw1(IP),nx0(IP),ny0(IP),nz1(IP),i)*(xv0)*(xw1)*(xx0)*(xy0)*(xz1) 
    ans = ans + zf(nv0(IP),nw1(IP),nx0(IP),ny1(IP),nz0(IP),i)*(xv0)*(xw1)*(xx0)*(xy1)*(xz0) 
    ans = ans + zf(nv0(IP),nw1(IP),nx0(IP),ny1(IP),nz1(IP),i)*(xv0)*(xw1)*(xx0)*(xy1)*(xz1) 
    ans = ans + zf(nv0(IP),nw1(IP),nx1(IP),ny0(IP),nz0(IP),i)*(xv0)*(xw1)*(xx1)*(xy0)*(xz0) 
    ans = ans + zf(nv0(IP),nw1(IP),nx1(IP),ny0(IP),nz1(IP),i)*(xv0)*(xw1)*(xx1)*(xy0)*(xz1) 
    ans = ans + zf(nv0(IP),nw1(IP),nx1(IP),ny1(IP),nz0(IP),i)*(xv0)*(xw1)*(xx1)*(xy1)*(xz0) 
    ans = ans + zf(nv0(IP),nw1(IP),nx1(IP),ny1(IP),nz1(IP),i)*(xv0)*(xw1)*(xx1)*(xy1)*(xz1) 
    ans = ans + zf(nv1(IP),nw0(IP),nx0(IP),ny0(IP),nz0(IP),i)*(xv1)*(xw0)*(xx0)*(xy0)*(xz0) 
    ans = ans + zf(nv1(IP),nw0(IP),nx0(IP),ny0(IP),nz1(IP),i)*(xv1)*(xw0)*(xx0)*(xy0)*(xz1) 
    ans = ans + zf(nv1(IP),nw0(IP),nx0(IP),ny1(IP),nz0(IP),i)*(xv1)*(xw0)*(xx0)*(xy1)*(xz0) 
    ans = ans + zf(nv1(IP),nw0(IP),nx0(IP),ny1(IP),nz1(IP),i)*(xv1)*(xw0)*(xx0)*(xy1)*(xz1) 
    ans = ans + zf(nv1(IP),nw0(IP),nx1(IP),ny0(IP),nz0(IP),i)*(xv1)*(xw0)*(xx1)*(xy0)*(xz0) 
    ans = ans + zf(nv1(IP),nw0(IP),nx1(IP),ny0(IP),nz1(IP),i)*(xv1)*(xw0)*(xx1)*(xy0)*(xz1) 
    ans = ans + zf(nv1(IP),nw0(IP),nx1(IP),ny1(IP),nz0(IP),i)*(xv1)*(xw0)*(xx1)*(xy1)*(xz0) 
    ans = ans + zf(nv1(IP),nw0(IP),nx1(IP),ny1(IP),nz1(IP),i)*(xv1)*(xw0)*(xx1)*(xy1)*(xz1) 
    ans = ans + zf(nv1(IP),nw1(IP),nx0(IP),ny0(IP),nz0(IP),i)*(xv1)*(xw1)*(xx0)*(xy0)*(xz0) 
    ans = ans + zf(nv1(IP),nw1(IP),nx0(IP),ny0(IP),nz1(IP),i)*(xv1)*(xw1)*(xx0)*(xy0)*(xz1) 
    ans = ans + zf(nv1(IP),nw1(IP),nx0(IP),ny1(IP),nz0(IP),i)*(xv1)*(xw1)*(xx0)*(xy1)*(xz0) 
    ans = ans + zf(nv1(IP),nw1(IP),nx0(IP),ny1(IP),nz1(IP),i)*(xv1)*(xw1)*(xx0)*(xy1)*(xz1) 
    ans = ans + zf(nv1(IP),nw1(IP),nx1(IP),ny0(IP),nz0(IP),i)*(xv1)*(xw1)*(xx1)*(xy0)*(xz0)
    ans = ans + zf(nv1(IP),nw1(IP),nx1(IP),ny0(IP),nz1(IP),i)*(xv1)*(xw1)*(xx1)*(xy0)*(xz1) 
    ans = ans + zf(nv1(IP),nw1(IP),nx1(IP),ny1(IP),nz0(IP),i)*(xv1)*(xw1)*(xx1)*(xy1)*(xz0) 
    ans = ans + zf(nv1(IP),nw1(IP),nx1(IP),ny1(IP),nz1(IP),i)*(xv1)*(xw1)*(xx1)*(xy1)*(xz1) 
    !
    zout(IP,i) = ans
  ENDDO
END DO
!
v=vsav
w=wsav
y=ysav
x=xsav
z=zsav
!
RETURN
END SUBROUTINE CH_AER_INTERMIN
