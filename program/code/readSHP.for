	subroutine readShapeFile(fln1,fln2)
c
c	read shapefile from file name fln
c
	character*200 fln1,fln2,txt
	common /smzAra/ nts(1),nps(1,135),xs(1,610000),ys(1,610000)
c
	write (txt,'(2a)') ' Reading line shapefile ',trim(fln1)
	call screenLogger(-20.,txt)
c	write (6,'(2a)') ' Reading shapefile ',trim(fln1)
	call readLineShp(fln1)
	write (txt,'(2a)') ' Reading polygon shapefile ',trim(fln2)
	call screenLogger(-20.,txt)
c	write (6,'(2a)') ' Reading shapefile ',trim(fln2)
	call readPolygonShp(fln2,1,135,610000,nts,nps,xs,ys,nz)
	return
	end
c
c
c
	subroutine readLineShp(fln)
c
c	reads spatial data of a line shapefile
c
	common /gisborder/ xmin,xmax,ymin,ymax
	real*8 xmin,xmax,ymin,ymax
	character*200 fln
	integer*4 idm,it,ir,il,ip,in,ind(10)          ! idm = integer dummy
	real*8 dm,xx,yy                               ! dm = real*8 dummy
c	
	common /shp/ nt(170000),np(200000),x(1000000),y(1000000),nl
c
	nPartCounter = 1
	nPointCounter = 1
	open (20, file=trim(fln), status='old', action='read', 
     *form='binary', convert='big_endian')
c	read header
	read (20) idm,idm,idm,idm,idm,idm,idm
	open (20, file=trim(fln), status='old', action='read', 
     *form='binary', convert='little_endian')
	read (20) idm,it,xmin,ymin,xmax,ymax,dm,dm,dm,dm
	if (it.ne.3) then
		write (6,'(3a,i)') 'File ',trim(fln),
     *	' is not a line shapefile but type ',it
		return
	endif
c	read records
	do while (.true.)
		open (20, file=trim(fln), status='old', action='read', 
     *		  form='binary', convert='big_endian')
		read (20, end=99) ir,il   ! ir: Record number, il: Record length
		open (20, file=trim(fln), status='old', action='read', 
     *        form='binary', convert='little_endian')
c		it: Shape type, ip: No. of parts, in: no. of points, ind: Start position of points of each part
		read (20) it,dm,dm,dm,dm,ip,in,(ind(i),i=1,ip)
		if (it.ne.3) then
			write (6,'(a,i,2a)') 'Unexpected shape type: ',it,' in file ',
     *							 trim(fln)
			return
		endif
		nt(ir) = nPartCounter       ! set marker to part array np
		ind(ip+1) = in              ! set end marker for this line
		do 110 i=1,ip               ! do for all parts of this line
			np(nPartCounter) = nPointCounter  ! set pointer where new parts start
			do 111 j=ind(i)+1,ind(i+1)        ! read all points of this part
				read (20) xx,yy
				x(nPointCounter) = xx
				y(nPointCounter) = yy
				nPointCounter = nPointCounter + 1
  111			continue
			nPartCounter = nPartCounter + 1
  110		continue
	end do
   99 continue
	nl = ir
	if (.false.) then
		write (6,'(a,i)') 'Number of lines:  ',nl
		write (6,'(a,i)') 'Number of parts:  ',nPartCounter-1
		write (6,'(a,i)') 'Number of points: ',nPointCounter-1
		read (5,*) 
	endif
	close (20)
	return
	end
c
c
c
	subroutine readPolygonShp(fl,nshapes,npols,npoint,nt,np,x,y,nz)
c
c	reads spatial data of a polygon shapefile
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
	open (20, file=trim(fl), status='old', action='read', 
     *form='binary', convert='big_endian')
c	read header
	read (20) idm,idm,idm,idm,idm,idm,idm
	open (20, file=trim(fl), status='old', action='read', 
     *form='binary', convert='little_endian')
	read (20) idm,it,xminT,yminT,xmaxT,ymaxT,dm,dm,dm,dm
	if (it.ne.5) then
		write (6,'(3a,i)') 'File ',trim(fl),
     *	' is not a polygon shapefile but type ',it
	endif
c	read records
	do while (.true.)
	open (20, file=trim(fl), status='old', action='read', 
     *form='binary', convert='big_endian')
	read (20, end=99) ir,il   ! ir: Record number, il: Record length
	nt(ir) = 0
	open (20, file=trim(fl), status='old', action='read', 
     *form='binary', convert='little_endian')
c	it: Shape type, ip: No. of parts, in: no. of points, ind: Start position of points of each part
	read (20) it,dm,dm,dm,dm,ip,in,(ind(i),i=1,ip)
	if (it.ne.5) then
		write (6,'(a,i)') 'Unexpected shape type: ',it
	endif
	ind(ip+1) = in            ! set end marker for this polygon
	do 110 i=1,ip             ! do for all parts of this polygon
	np(ir,i) = ind(i)+1       ! set pointer where new parts start
	do 111 j=ind(i)+1,ind(i+1)
	read (20) xx,yy
	x(ir,j) = xx
	y(ir,j) = yy
	nt(ir) = nt(ir)+1
  111 continue
	iPointMax = max(iPointMax,ind(i+1))
	iPolMax = max(iPolMax,ip)
  110 continue
	np(ir,ip+1) = in          ! set end marker of last part of this polygon
	end do
   99 continue
	nz = ir
	if (.false.) then
		write (6,'(2a)') 'File name:             ',trim(fl)
		write (6,'(a,i)') 'Number of polygons:    ',nz
		write (6,'(a,i)') 'Max. Number of parts:  ',iPolMax
		write (6,'(a,i)') 'Max. number of points: ',iPointMax
		xxmin = 9999999999.
		xxmax = -9999999999.
		yymin = 9999999999.
		yymax = -9999999999.
		do 100 i=1,size(x,dim=1)
		do 100 j=1,size(x,dim=2)
			xxmin = amin1(xxmin,x(i,j))
			xxmax = amax1(xxmax,x(i,j))
			yymin = amin1(yymin,y(i,j))
			yymax = amax1(yymax,y(i,j))
  100		continue
		write (6,'(a,2f15.1)') 'x Min/Max: ',xxmin,xxmax
		write (6,'(a,2f15.1)') 'y Min/Max: ',yymin,yymax
          write (6,'(a)') 'Hit enter to continue.'
          read (5,*)
	endif
	close (20)
	return
	end
c
c
c
	subroutine readSizeOfPolygonShp(fl)
c
c	reads dimensions of a polygon shapefile
c
	common /gisborder/ xmin,xmax,ymin,ymax
	real*8 xmin,xmax,ymin,ymax,xminT,xmaxT,yminT,ymaxT
	character*100 path,file,fileparameters,fl*200
	integer*4 idm,it,ir,il,ip,in,ind(10000)       ! idm = integer dummy
	real*8 dm,xx,yy,xxmin,xxmax,yymin,yymax       ! dm = real*8 dummy
c	
	iPointMax = 0
	iPolMax = 0
	xxmin = 9999999999.
	xxmax = -9999999999.
	yymin = 9999999999.
	yymax = -9999999999.
	open (20, file=trim(fl), status='old', action='read', 
     *form='binary', convert='big_endian')
c	read header
	read (20) idm,idm,idm,idm,idm,idm,idm
	open (20, file=trim(fl), status='old', action='read', 
     *form='binary', convert='little_endian')
	read (20) idm,it,xminT,yminT,xmaxT,ymaxT,dm,dm,dm,dm
	if (it.ne.5) then
		write (6,'(3a,i)') 'File ',trim(fl),
     *	' is not a polygon shapefile but type ',it
	endif
c	read records
	do while (.true.)
	open (20, file=trim(fl), status='old', action='read', 
     *form='binary', convert='big_endian')
	read (20, end=99) ir,il   ! ir: Record number, il: Record length
	open (20, file=trim(fl), status='old', action='read', 
     *form='binary', convert='little_endian')
c	it: Shape type, ip: No. of parts, in: no. of points, ind: Start position of points of each part
	read (20) it,dm,dm,dm,dm,ip,in,(ind(i),i=1,ip)
	if (it.ne.5) then
		write (6,'(a,i)') 'Unexpected shape type: ',it
	endif
	ind(ip+1) = in            ! set end marker for this polygon
	do 110 i=1,ip             ! do for all parts of this polygon
	do 111 j=ind(i)+1,ind(i+1)
	read (20) xx,yy
	xxmin = dmin1(xxmin,xx)
	xxmax = dmax1(xxmax,xx)
	yymin = dmin1(yymin,yy)
	yymax = dmax1(yymax,yy)
  111 continue
	iPointMax = max(iPointMax,ind(i+1))
	iPolMax = max(iPolMax,ip)
  110 continue
	end do
   99 continue
	nz = ir
	write (6,'(a,i)') 'Number of polygons:    ',nz
	write (6,'(a,i)') 'Max. Number of parts:  ',iPolMax
	write (6,'(a,i)') 'Max. number of points: ',iPointMax
	write (6,'(a,2f15.1)') 'x Min/Max: ',xxmin,xxmax
	write (6,'(a,2f15.1)') 'y Min/Max: ',yymin,yymax
	write (6,'(a)') 'Hit enter to continue'
	read (5,*)
	close (20)
	return
	end
c
c
c
	subroutine readPointShp(fln,n,x,y)
c
c	reads spatial data of a point shapefile 
c
	common /gisborder/ xmin,xmax,ymin,ymax
	real*8 xmin,xmax,ymin,ymax
	character*200 fln
	integer*4 idm,it,ir,il,ip,in,ind(10)          ! idm = integer dummy
	real*8 dm,xx,yy                               ! dm = real*8 dummy
c	
	dimension x(n),y(n)
c
	nPointCounter = 1
	open (20, file=trim(fln), status='old', action='read', 
     *form='binary', convert='big_endian')
c	read header
	read (20) idm,idm,idm,idm,idm,idm,idm
	open (20, file=trim(fln), status='old', action='read', 
     *form='binary', convert='little_endian')
	read (20) idm,it,xmin,ymin,xmax,ymax,dm,dm,dm,dm
	if (it.ne.1) then
		write (6,'(3a,i)') 'File ',trim(fln),
     *	' is not a point shapefile but type ',it
		return
	endif
c	read records
	do while (.true.)
		open (20, file=trim(fln), status='old', action='read', 
     *		  form='binary', convert='big_endian')
		read (20, end=99) ir,il   ! ir: Record number, il: Record length
		open (20, file=trim(fln), status='old', action='read', 
     *        form='binary', convert='little_endian')
c		it: Shape type, ip: No. of parts, in: no. of points, ind: Start position of points of each part
		read (20) it
		if (it.ne.1) then
			write (6,'(a,i,2a)') 'Unexpected shape type: ',it,' in file ',
     *							 trim(fln)
			return
		endif
		if (nPointCounter.gt.n) then
			write (6,'(a)') 'More than ',n,' points in shapefile ',
     *         fln,'. Skipped additional points.'
			return
		endif
		read (20) xx,yy
		x(nPointCounter) = xx
		y(nPointCounter) = yy
		nPointCounter = nPointCounter + 1
	end do
   99 continue
	nl = ir
	if (.false.) then
		write (6,'(a,i)') 'Number of points: ',nPointCounter-1
		read (5,*) 
	endif
	close (20)
	return
	end
c
c
c
