	subroutine writePolygonShp(fl,nshapes,npols,npoint,nt,np,x,y,nz)
c
c	write spatial data to a polygon shapefile
c
	common /gisborder/ xmin,xmax,ymin,ymax
	real*8 xmin,xmax,ymin,ymax,xminT,xmaxT,yminT,ymaxT
	character*100 path,file,fileparameters,fl*200
	integer*4 idm,it,ir,il,ip,in,ind(npols)       ! idm = integer dummy
	real*8 dm,xx,yy                               ! dm = real*8 dummy
c	
	dimension nt(nshapes),np(nshapes,npols),x(nshapes,npoint),
     *y(nshapes,npoint)
c
	iPointMax = 0
	iPolMax = 0
	open (20, file=trim(fl), status='unknown', action='write', 
     *form='binary', convert='big_endian')
c	write header
	write (20) 9994,idm,idm,idm,idm,idm,idm
	open (20, file=trim(fl), status='old', action='write', 
     *form='binary', convert='little_endian')
	write (20) 1000,5,xminT,yminT,xmaxT,ymaxT,dm,dm,dm,dm
c	write records
	do 100 ir = 1,nz
		open (20, file=trim(fl), status='old', action='write', 
     *		form='binary', convert='big_endian')
		write (20) ir,112   ! ir: Record number, il: Record length
		open (20, file=trim(fl), status='old', action='write', 
     *		form='binary', convert='little_endian')
c		it: Shape type, ip: No. of parts, in: no. of points, ind: Start position of points of each part
		write (20) 5,dm,dm,dm,dm,1,4,0
		do 110 j=1,4
			xx = x(ir,j)
			yy = y(ir,j)
			write (20) xx,yy
  110		continue
  100	continue
	close (20)
	return
	end
c
c
