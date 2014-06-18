	program rasterizeNC
c
c	Program to develop rastercells across Georgia for the I-75 corridor study
c	Author: Rolf Moeckel, PB Albuquerque, USA
c	Date: 24 September 2012, Santa Fe (NM)
c
	common /finestRaster/ idfr(4096,4096),pp(4096,4096),hh(4096,4096),
     *	emp(4096,4096)
	character txt*200
	logical stopRun
	dimension dm(1000)
c
c	Set maximum socio-economic data per raster cell
	minSize = 5000
c
	call timer(-1,ih,im,is,ihs)
	write (txt,'(a)') 'Rasterize I-75'
	call setScreen(txt)
	call setpal(1)
	call screenLogger(15.,txt)
c
	write (txt,'(a)') 'Reading in data...'
	call screenLogger(-20.,txt)
	call readData
	if (.true.) then
c	rasterize I-75 TAZ
		call defineInitialRaster
		write (txt,'(a)') 'Rasterizing zones to smallest raster cells...'
		call screenLogger(-20.,txt)
		call rasterizeZones
c		call drawSmallestRasterCells
		write (txt,'(a)') 'Disaggregating SE data to smallest raster cells'
		call screenLogger(-20.,txt)
		call allocateSEdataToRasterCells
c		call drawZonesFast
		call drawPolygonMap
		write (txt,'(a)') ' '
		call screenLogger(-20.,txt)
		stopRun = .false.
		do 100 i=1,size(idfr,dim=2)
			call defineNewRaster(minSize,stopRun)
			if (stopRun) goto 101
			if (2**i.lt.1024) then
				write (txt,'(a,i4,a)') 'Current size: ',515 / (2**i),' km'
			else
				write (txt,'(a,f6.1,a)') 'Current size: ',515. / (2**i)*1000.,' m'
			endif
			call screenLogger(0.,txt)
			do 111 it=1,1000
				dm = 0.
  111			continue			
  100		continue
  101		continue
		call saveNewRaster(n)
		call writeUnGenerateFile
		write (txt,'(a,i5,a)') 'Created ',n,' raster cells.'
		call screenLogger(-20.,txt)
	endif
	if (.true.) then
c	split trip tables
		call readNewRaster
		call readTripTables
		call readTazReferences
		call splitTrips(minSize)
	endif
c
	call timer(1,ih,im,is,ihs)
	write (txt,'(4(a,i2),a)') 'Run time: ',ih,':',im,' ',is,'s ',ihs,'ms'
	call screenLogger(-20.,txt)
	write (txt,'(a)') 'Click to exit.'
	call screenLogger(-20.,txt)
	call waitmouse(9,k,ix,iy)
	stop
	end
c
c
c
	subroutine drawZonesFast
c
c	draw I-75 zones on screen
c
	use dflib
	character txt*200
	integer*1 imag(7691204)
	integer*2 ix,iy,jx,jy
	logical od
c
c	open child window
      inquire (1,opened=od)
      if (.not.od) then
		write (txt,'(a,a3,a)') 'Rasterize NCSTM',char(0)
		call child(1,trim(txt))
		call setThisScreen
	endif
	i = focusqq(1)
	if (i.ne.0) then
		write (6,'(a)') 'Window 1 not focused'
		stop
	endif
c	draw to hidden window
 	i = setactiveqq(0)
	if (i.ne.1) then
		write (6,'(a)') 'Window 1 not activated'
		stop
	endif
c	draw map
c	call clear
	call setcolour(0)
	call drawPolygonMap
c	Transfer page to Child Window 1
	ix = 0
	iy = 0
	jx = 1600
	jy = 1200
  	call getimage(ix,iy,jx,jy,imag)
	i = focusqq(1)
	if (i.ne.0) then
	 write (6,'(a)') 'Window 1 not focused'
	 stop
	endif
	call putimage(ix,iy,imag,$gpset)
	return
	end
c
c
c
	subroutine drawPolygonMap
c
c	Draw map with polygons
c
	common /i75shp/ nt(4500),np(4500,60),x(4500,10000),
     *	y(4500,10000),nz
	common /i75zoneSystem/ idbfID(4500),id(4500),ind(12027)
	dimension xx(10000),yy(10000)
	character txt*200
c
	write (txt,'(a)') 'Drawing zones...'
	call screenLogger(-20.,txt)
	call setColorInRGB(150,150,150)
	do 100 ir=1,nz
		if (nt(ir).eq.0) goto 100
		itaz = id(idbfID(ir))
		if (itaz.le.1715.or.itaz.ge.10000) then
			call setColorInRGB(250,150,150)
		else
			call setColorInRGB(150,150,150)
		endif
		do 110 n=1,size(np, dim = 2)
			if (np(ir,n+1).eq.0) goto 100
			do 111 i=1,(np(ir,n+1)-np(ir,n))
				xx(i) = x(ir,i+np(ir,n)-1)
				yy(i) = y(ir,i+np(ir,n)-1)
  111			continue
			call drawPolygon(np(ir,n+1)-(np(ir,n)),xx,yy,1)
  110		continue
  100 continue
	call setColorInRGB(0,0,0)
	return
	end
c
c
c
	subroutine defineInitialRaster
c
c	define first raster cells
c
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
	common /finestRaster/ idfr(4096,4096),pp(4096,4096),hh(4096,4096),
     *	emp(4096,4096)
c
	xl = -733340.
	xr = 954043.
	yb = 11667783.
	yt = 13355166.
	id = 0
	co = 0.
	idr(1) = 1
	cx(1,1) = xl
	cx(1,2) = xr
	cy(1,1) = yb
	cy(1,2) = yt
	iRange(1,1) = 1
	iRange(1,2) = size(idfr,dim=1)
	jRange(1,1) = 1
	jRange(1,2) = size(idfr,dim=2)
	call drawRasterCells
	return
	end
c
c
c
	subroutine rasterizeZones
c
c	put detailed raster cells over zones
c
	common /i75shp/ nt(4500),np(4500,60),x(4500,10000),
     *	y(4500,10000),nz
	common /i75zoneSystem/ idbfID(4500),id(4500),ind(12027)
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
	common /finestRaster/ idfr(4096,4096),pp(4096,4096),hh(4096,4096),
     *	emp(4096,4096)
	dimension xmin(4500),xmax(4500),ymin(4500),ymax(4500)
	character txt*200
c
	if (.false.) then
		open (20,file='data\rasterizedI75.bin',form='binary')
		read (20) idfr
		close (20)
		return
	endif
c	Define range of every zone
	xmin = 9999999.
	xmax = -9999999.
	ymin = 99999999.
	ymax = -9999999.
	do 100 ir=1,nz
		if (nt(ir).eq.0) goto 100
		itaz = id(idbfID(ir))
		if (itaz.gt.1715.and.itaz.lt.10000) goto 100
		do 110 n=1,size(np, dim = 2)
			if (np(ir,n+1).eq.0) goto 100
			do 111 i=1,(np(ir,n+1)-np(ir,n))
				xmin(ir) = amin1(xmin(ir),x(ir,i+np(ir,n)-1))
				xmax(ir) = amax1(xmax(ir),x(ir,i+np(ir,n)-1))
				ymin(ir) = amin1(ymin(ir),y(ir,i+np(ir,n)-1))
				ymax(ir) = amax1(ymax(ir),y(ir,i+np(ir,n)-1))
  111			continue
  110		continue
  100 continue
c	walk through every raster cell and define zone ID (x from cell 1 to cell 2, y from cell 1 to cell 1)
	call setColorInRGB(0,0,255)
	xSize = abs(xr - xl) / (size(idfr,dim=1) * 1.)
	ySize = abs(yt - yb) / (size(idfr,dim=2) * 1.)
	do 200 ix = 1,size(idfr,dim=1)
	do 200 iy = 1,size(idfr,dim=2)
		xcentroid = xl + (ix * xSize) - xSize / 2.
		ycentroid = yb + (iy * ySize) - ySize / 2.
c		call circle(xcentroid,ycentroid,0.02,0)
		do 210 ir=1,nz
			if (nt(ir).eq.0) goto 210
			itaz = id(idbfID(ir))
			if (itaz.gt.1715.and.itaz.lt.10000) goto 210
			do 211 n=1,size(np, dim = 2)-1
				if (np(ir,n+1).eq.0) goto 211
				ncuts = 0
				if (xmin(ir).le.xcentroid.and.xmax(ir).ge.xcentroid.and.
     *		        ymin(ir).le.ycentroid.and.ymax(ir).ge.ycentroid) then
					do 212 i=1,(np(ir,n+1)-np(ir,n))-1
						x1 = x(ir,i+np(ir,n)-1)
						y1 = y(ir,i+np(ir,n)-1)
						x2 = x(ir,i+np(ir,n))
						y2 = y(ir,i+np(ir,n))
						call sll(x1,y1,x2,y2,-1000000.,ycentroid,xcentroid,ycentroid,no,
     *						xs,ys)
						if (no.eq.4) ncuts = ncuts + 1
  212					continue
					x1 = x(ir,np(ir,n+1)-1)
					y1 = y(ir,np(ir,n+1)-1)
					x2 = x(ir,np(ir,n))
					y2 = y(ir,np(ir,n))
					call sll(x1,y1,x2,y2,-1000000.,ycentroid,xcentroid,ycentroid,no,
     *					xs,ys)
					if (no.eq.4) ncuts = ncuts + 1
c					define if polygon lies in this rastercell
					if (mod(ncuts,2).ne.0) then
						idfr(ix,iy) = itaz
						call drawRectangle(xcentroid-xSize/2.,ycentroid-ySize/2.,
     *						xcentroid+xSize/2.,ycentroid+ySize/2.,1)
					endif
				endif
  211			continue
  210		continue
  200 continue
	open (20,file='data\rasterizedI75.bin',form='binary')
	write (20) idfr
	close (20)
	call setColorInRGB(0,0,0)
	return
	end
c
c
c
	subroutine allocateSEdataToRasterCells
c
c	disaggregate socio-economic data to finest raster cell data structure
c
	common /finestRaster/ idfr(4096,4096),pp(4096,4096),hh(4096,4096),
     *	emp(4096,4096)
	common /seData/ itaz(4500),indSE(12027),popZ(4500),hhZ(4500),empZ(4500)
	common /i75zoneSystem/ idbfID(4500),id(4500),ind(12027)
	dimension irasCnt(4500)
c
	irasCnt = 0
	pp = 0.
	hh = 0.
	emp = 0.
	do 100 ix=1,size(idfr,dim=1)
	do 100 iy=1,size(idfr,dim=2)
		if (idfr(ix,iy).eq.0) goto 100
		irasCnt(ind(idfr(ix,iy))) = irasCnt(ind(idfr(ix,iy))) + 1
  100 continue
	do 200 ix=1,size(idfr,dim=1)
	do 200 iy=1,size(idfr,dim=2)
		if (idfr(ix,iy).eq.0) goto 200
		do 210 i=1,size(itaz)
			if (itaz(i).eq.idfr(ix,iy)) goto 211
  210		continue
		write (6,'(a,i7,a)') 'Error. Could not find zone ',idfr(ix,iy),
     *		' in SE data.'
  211		continue
		pp(ix,iy) = (popZ(i)) / (1. * irasCnt(ind(idfr(ix,iy))))
		hh(ix,iy) = (hhZ(i)) / (1. * irasCnt(ind(idfr(ix,iy))))
		emp(ix,iy) = (empZ(i)) / (1. * irasCnt(ind(idfr(ix,iy))))
  200 continue
	return
	end
c
c
c
	subroutine drawSmallestRasterCells
c
c	Draw smallest raster cells that have been defined and color by zone
c
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
	common /finestRaster/ idfr(4096,4096),pp(4096,4096),hh(4096,4096),
     *	emp(4096,4096)
	common /i75zoneSystem/ idbfID(4500),id(4500),ind(12027)
	dimension ir(4500),ig(4500),ib(4500)
c
c	define color by zone
	do 100 i=1,size(ir)
		ir(i) = rand() * 255. + 0.5
		ig(i) = rand() * 255. + 0.5
		ib(i) = rand() * 255. + 0.5
  100 continue
	xSize = abs(xr - xl) / (size(idfr,dim=1) * 1.)
	ySize = abs(yt - yb) / (size(idfr,dim=2) * 1.)
c	walk though every raster cell
	do 200 ix=1,size(idfr,dim=1)
	do 200 iy=1,size(idfr,dim=2)
c		if (pp(ix,iy).eq.0.and.emp(ix,iy).eq.0) goto 200
		if (idfr(ix,iy).eq.0) goto 200
		itaz = idfr(ix,iy)
		index = ind(itaz)
		call setColorInRGB(ir(index),ig(index),ib(index))
		call drawFilledRectangle(xl+(ix-1)*xSize,yb+(iy-1)*ySize,
     *		xl+ix*xSize,yb+iy*ySize,1)
  200 continue
	call setColorInRGB(0,0,0)
	return
	end
c
c
c
	subroutine drawRasterCells
c
c	draw raster cells
c
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
c
	call setColorInRGB(0,0,255)
	do 100 i=1,size(idr)
		if (idr(i).eq.0) goto 100
		call drawRectangle(cx(i,1),cy(i,1),cx(i,2),cy(i,2),1)
  100 continue
	call setColorInRGB(0,0,0)
	return
	end
c
c
c
	subroutine drawNewestRasterCells(n)
c
c	draw raster cells
c
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
c
	do 100 i=n-3,n
		if (idr(i).eq.0) goto 100
		call drawRectangle(cx(i,1),cy(i,1),cx(i,2),cy(i,2),1)
  100 continue
	return
	end
c
c
c
	subroutine defineNewRaster(minSize,stopRun)
c
c	divide raster cells
c
	common /finestRaster/ idfr(4096,4096),pp(4096,4096),hh(4096,4096),
     *	emp(4096,4096)
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
	character txt*200
	logical stopRun,foundNoDisagg
c
c	find how far raster cells have been defined
	do 100 n=1,size(idr)
	 if (idr(n).eq.0) goto 101
  100 continue
  101 continue
	is = n - 1
	if (n.gt.size(idr)-4) then
		write (txt,'(a,i5,a)') 'Maximum number of ',size(idr),
     *		' zones has been reached. Further disaggregation was stopped.'
		call screenLogger(-20.,txt)
		stopRun = .true.
		return
	endif
	foundNoDisagg = .true.
	call setColorInRGB(0,0,255)
	do 200 i=1,is
		if (idr(i).eq.0) goto 200
		hhSum = 0.
		ppSum = 0.
		empSum = 0.
		do 210 ix = iRange(i,1),iRange(i,2)
		do 210 iy = jRange(i,1),jRange(i,2)
			hhSum = hhSum + hh(ix,iy)
			ppSum = ppSum + pp(ix,iy)
			empSum = empSum + emp(ix,iy)
  210		continue
		if (ppSum.gt.minSize.or.empSum.gt.minSize) then
			call splitCellIntoFourCells(i,n)
			foundNoDisagg = .false.
		endif
		if ((iRange(i,2)-iRange(i,1)).eq.1) stopRun = .true.
  200 continue
	if (foundNoDisagg) stopRun = .true.
	call setColorInRGB(0,0,0)
	return
	end
c
c
c
	subroutine splitCellIntoFourCells(i,n)
c
c	split cell i into four cells
c
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
c
c	upper left corner
	cx(n,1) = cx(i,1)
	cx(n,2) = cx(i,1) + (cx(i,2)-cx(i,1)) / 2.
	cy(n,1) = cy(i,1) + (cy(i,2)-cy(i,1)) / 2.
	cy(n,2) = cy(i,2)
	iRange(n,1) = iRange(i,1)
	iRange(n,2) = iRange(i,1) + (iRange(i,2)-iRange(i,1)) / 2 + 1
	jRange(n,1) = jRange(i,1) + (jRange(i,2)-jRange(i,1)) / 2 + 1
	jRange(n,2) = jRange(i,2)
	idr(n) = n
	
c	write(6,'(a,5i5)')'A',n,iRange(n,1),iRange(n,2),jRange(n,1),jRange(n,2)
	
	n = n + 1
c	upper right corner
	cx(n,1) = cx(i,1) + (cx(i,2)-cx(i,1)) / 2.
	cx(n,2) = cx(i,2)
	cy(n,1) = cy(i,1) + (cy(i,2)-cy(i,1)) / 2.
	cy(n,2) = cy(i,2)
	iRange(n,1) = iRange(i,1) + (iRange(i,2)-iRange(i,1)) / 2 + 1
	iRange(n,2) = iRange(i,2)
	jRange(n,1) = jRange(i,1) + (jRange(i,2)-jRange(i,1)) / 2 + 1
	jRange(n,2) = jRange(i,2)
	idr(n) = n
	
c	write(6,'(a,5i5)')'B',n,iRange(n,1),iRange(n,2),jRange(n,1),jRange(n,2)
	
	n = n + 1
c	lower right corner
	cx(n,1) = cx(i,1) + (cx(i,2)-cx(i,1)) / 2.
	cx(n,2) = cx(i,2)
	cy(n,1) = cy(i,1)
	cy(n,2) = cy(i,1) + (cy(i,2)-cy(i,1)) / 2.
	iRange(n,1) = iRange(i,1) + (iRange(i,2)-iRange(i,1)) / 2 + 1
	iRange(n,2) = iRange(i,2)
	jRange(n,1) = jRange(i,1)
	jRange(n,2) = jRange(i,1) + (jRange(i,2)-jRange(i,1)) / 2 + 1
	idr(n) = n
	
c	write(6,'(a,5i5)')'C',n,iRange(n,1),iRange(n,2),jRange(n,1),jRange(n,2)

	n = n + 1
c	lower left corner (assign original id of i to n)
c	cx(i,1) remains unchanged 
	cx(i,2) = cx(i,1) + (cx(i,2)-cx(i,1)) / 2.
c	cy(i,1) remains unchanged
	cy(i,2) = cy(i,1) + (cy(i,2)-cy(i,1)) / 2.
c	iRange(i,1) remains unchanged
	iRange(i,2) = iRange(i,1) + (iRange(i,2) - iRange(i,1)) / 2
c	jRange(i,1) remains unchanged
	jRange(i,2) = jRange(i,1) + (jRange(i,2) - jRange(i,1)) / 2
	
c	write(6,'(a,5i5)')'D',i,iRange(i,1),iRange(i,2),jRange(i,1),jRange(i,2)
	
	call drawNewestRasterCells(n)
	return
	end
c
c
c
	subroutine saveNewRaster(n)
c
c	save revised raster cells
c
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
c
	open (20,file='data\newRasterCellsI75.bin',form='binary')
	write (20) xl,xr,yb,yt,idr,cx,cy,iRange,jRange
	close (20)
	do 100 n=1,size(idr)
		if (idr(n).eq.0) goto 101
  100 continue
  101 continue
	n = n-1
	return
	end
c
c
c
	subroutine readNewRaster
c
c	read revised raster cells
c
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
c
	open (20,file='data\newRasterCellsI75.bin',form='binary')
	read (20) xl,xr,yb,yt,idr,cx,cy,iRange,jRange
	close (20)
	return
	end
c
c
c
	subroutine writeUnGenerateFile
c
c	write ungenerate file to be imported into shapefile via TransCAD
c
	common /raster/ xl,xr,yb,yt,idr(10000),cx(10000,2),cy(10000,2),
     *	iRange(10000,2),jRange(10000,2)
	character*200 txt
c
c	open (30,file='newRasterCellsI75.bin',form='binary')
c	read (30) xl,xr,yb,yt,idr,cx,cy,iRange,jRange
c	close (30)
	open (40,file='data\newRasterCellsI75.lin')
	open (41,file='data\newRasterCellsI75.pts')
	do 100 i=1,size(idr)
		if (idr(i).eq.0) goto 100
		write (40,'(i10)') idr(i)
		write (40,'(f15.6,1x,f15.6)') cx(i,1),cy(i,1)
		write (40,'(f15.6,1x,f15.6)') cx(i,2),cy(i,1)
		write (40,'(f15.6,1x,f15.6)') cx(i,2),cy(i,2)
		write (40,'(f15.6,1x,f15.6)') cx(i,1),cy(i,2)
		write (40,'(f15.6,1x,f15.6)') cx(i,1),cy(i,1)
		write (40,'(a)') 'END'
		write (41,'(i10,2f16.6)') idr(i),(cx(i,1)+(cx(i,2)-cx(i,1))/2.),
     *		(cy(i,1)+(cy(i,2)-cy(i,1))/2.)
  100 continue
	write (40,'(a)') 'END'
	write (41,'(a)') 'END'
	close (40)
	close (41)
	write (txt,'(2a)') 'gen2shp newRasterCellsI75shp polygons < ',
     *'data\newRasterCellsI75.lin'
	i = system(trim(txt))
	return
	end
c
c
c
	subroutine splitTrips(minSize)
c
c	split trips from TAZ system to raster cells
c
	common /trips/ tAuto(2600,2600),tTrucks(2600,2600)
	real*8 trips(2600,2600),getMatrixSum,sAuto,sTrks
	character*200 fileName,txt
c
	beta = -0.17
c	Disaggregate auto trip tables from zones to raster cells
	call readSkim
	call readArcSkim
	call readStraightLineDistance
	sAuto = 0.
	do 100 i=1,size(tAuto,dim=1)
	do 100 j=1,size(tAuto,dim=2)
		trips(i,j) = tAuto(i,j)
		sAuto = sAuto + trips(i,j)
  100 continue
c	write (6,'(a,2f12.2)') 'Autos input ',sAuto
	write (txt,'(i10)') minSize
	write (fileName,'(3a)') 'data\disAutos_',adjustl(trim(txt)),'.csv'
	call disaggregateSingleVehicleType(trips,fileName,beta)
	
	sTrks = 0.
	do 200 i=1,size(tTrucks,dim=1)
	do 200 j=1,size(tTrucks,dim=2)
		trips(i,j) = tTrucks(i,j)
		sTrks = sTrks + tTrucks(i,j)
  200 continue
c	write (6,'(a,2f12.2)') 'Trcks input ',sTrks
	write (fileName,'(3a)') 'data\disTrucks_',adjustl(trim(txt)),'.csv'
	call disaggregateSingleVehicleType(trips,fileName,beta)
	return
	end
c
c
c
	subroutine disaggregateSingleVehicleType(trips,fileName,beta)
c
c	Disaggregate trips(,) from TAZ to raster cells
c
	character*200 fileName,binFileName,txt,txt_nb
	common /rasterAgg/ irstByTaz(4500,28),rstShareOfTaz(4500,28),
     *	itazByRst(12027,24),tazShareOfRst(12027,24),mainTazOfRs(8000)
	common /i75zoneSystem/ idbfID(4500),id(4500),ind(12027)
	common /seData/ itaz(4500),indSE(12027),popZ(4500),hhZ(4500),empZ(4500)
	common /skim/ ids(2000:8000,2000:8000),itm(2000:8000,2000:8000)
	integer*2 ids,itm
	real*8 wght(28,28),wghtSum,trp,dist,sm,smARC,smDrop
	real*8 trips(2600,2600)     ! in GDOT zone system
	real*8 detTrips(4500,4500)  ! in GDOT/ARC zone system
	real*8 rsTrips(10000,10000) ! in Raster cells
c	
	write (binFileName,'(2a)') trim(fileName),'_i75Zones.bin'
	if (.false.) then
		write (txt,'(2a)') 'Disaggregating trips from GDOT to ARC zones'
		call screenLogger(-20.,txt)
		call disaggregateFromGdotToArcZones(trips,detTrips,beta)
		open (50,file=binFileName,form='binary')
		write (50) detTrips
		close (50)
	else
		open (50,file=binFileName,form='binary')
		read (50) detTrips
		close (50)
	endif
	
	sm = 0.
	rsTrips = 0.
	do 100 i=1,size(detTrips,dim=1)
	do 100 j=1,size(detTrips,dim=2)
		sm = sm + detTrips(i,j)
  100 continue
c	write (6,'(a,2f12.2)') '     output ',sm
	write (txt,'(a)') 'Disaggregating trips from TAZ to raster cells...'
	call screenLogger(-20.,txt)
	do 200 i=1,size(itaz)
	do 200 j=1,size(itaz)
		trp = detTrips(i,j)
		if (trp.eq.0) goto 200
		wght = 0.
		do 210 irs=1,size(irstByTaz,dim=2)
			if (irstByTaz(i,irs).eq.0) goto 210
			do 211 jrs = 1,size(irstByTaz,dim=2)
				if (irstByTaz(j,jrs).eq.0) goto 211
				dst = amin1((1.*ids(irstByTaz(i,irs),irstByTaz(j,jrs))), 200.)
				wght(irs,jrs) = dble(100. * rstShareOfTaz(i,irs)) *
     *				dble(100. * rstShareOfTaz(j,jrs)) * exp(dst * beta)
  211			continue
  210		continue
		wghtSum = 0.
		do 220 irs=1,size(irstByTaz,dim=2)
			if (irstByTaz(i,irs).eq.0) goto 220
			do 221 jrs = 1,size(irstByTaz,dim=2)
				if (irstByTaz(j,jrs).eq.0) goto 221
				wghtSum = wghtSum + wght(irs,jrs)
  221			continue
  220		continue
		do 230 irs=1,size(irstByTaz,dim=2)
			if (irstByTaz(i,irs).eq.0) goto 230
			do 231 jrs = 1,size(irstByTaz,dim=2)
				if (irstByTaz(j,jrs).eq.0) goto 231
				rsTrips(irstByTaz(i,irs),irstByTaz(j,jrs)) = 
     *				rsTrips(irstByTaz(i,irs),irstByTaz(j,jrs)) + 
     *				trp * wght(irs,jrs) / wghtSum
  231			continue
  230		continue
  200 continue
c
	slds = 20.  ! Straight Line Distance Speed (on local roads)
	open (40,file=trim(fileName))
	write (40,'(a)') 'orig,dest,trips'
	sm = 0.
	smARC = 0.
	dist = 0.
	smDrop = 0.
	do 300 i=1,size(rsTrips,dim=1)
	do 300 j=1,size(rsTrips,dim=2)
		if (rsTrips(i,j).gt.0) then
			directDist = getStraightLineDist(i,j)
			directTime = directDist / slds * 60.
			if (directDist.gt.0.and.directTime.lt.itm(i,j)) then
				smDrop = smDrop + rsTrips(i,j)
			else
				write (txt,'(2(i5,a),f16.8)') i,',',j,',',rsTrips(i,j)
				call removeSpacesFromLine(txt,txt_nb)
				write (40,'(a)') trim(txt_nb)
				sm = sm + rsTrips(i,j)
				if (mainTazOfRs(i).gt.10000.and.mainTazOfRs(j).gt.10000) then  
					! within ARC area
					smARC = smARC + rsTrips(i,j)
					dist = dist + rsTrips(i,j) * ids(i,j)
				endif
			endif
		endif
  300 continue
c	write (6,'(a,2f12.2)') '     output ',sm
c	write (6,'(a,2f12.2)') 'Ave. trip l ',(dist / smARC)
c	write (6,'(a,f12.2)')  'Dropped trp ',smDrop
	close (40)
c	write (txt,'(2a)') 'Removing blanks from output file ',trim(fileName)
c	call screenLogger(-20.,txt)
c	call removeSpaces(fileName)
	return
	end
c
c
c
	subroutine disaggregateFromGdotToArcZones(trips,detTrips,beta)
c
c	disaggregate trips from GDOT zone system (1-2542) to 
c	I-75 zone system (241-1715 + 2016-2542 + 10001-12072)
c
	real*8 trips(2600,2600)    ! in GDOT zone system
	real*8 detTrips(4500,4500) ! in GDOT/ARC zone system
	! /i75zoneSystem/ is GDOT/ARC zone system
c	common /i75zoneSystem/ idbfID(4500),id(4500),ind(12027)
	! /seData/ is in I-75 zone system
	common /seData/ itaz(4500),indSE(12027),popZ(4500),hhZ(4500),empZ(4500)
	! iGdotArc(GDOT zone, part 1-58) = ARC zone
	common /gdotArcRef/ iGdotArc(240,58)
	common /skimARC/ idis(4600,4600)
	integer*2 idis
	real*8 wghtO(58),wghtD(58),wght(58,58),wghtSum,getMatrixSum,share
	dimension iOrigList(58),iDestList(58)
c
c	disaggregate zones in ARC region from GDOT zone to ARC zones
	detTrips = 0.
	wghtO = 0.
	wghtD = 0.
	do 100 iorig=1,2542  !GDOT zones
		iOrigList = 0
		if (iorig.le.240) then
			do 110 iz = 1,iGetFinalPos(iorig)
				iArcZone = iGdotArc(iorig,iz)
				i75Zone = indSE(iArcZone+10000)
				wghtO(iz) = popZ(i75Zone) + empZ(i75Zone)
				iOrigList(iz) = i75Zone
  110			continue
		else
			i75Zone = indSE(iorig)
			wghtO(1) = 1.
			iOrigList(1) = i75Zone
		endif		
		do 120 idest=1,2542  !GDOT zones
c			if (iorig.eq.idest) goto 120  ! skip GDOT intrazonals
			if (trips(iorig,idest).eq.0) goto 120
			iDestList = 0
			if (idest.le.240) then
				do 121 iz = 1,iGetFinalPos(idest)
					iArcZone = iGdotArc(idest,iz)
					i75Zone = indSE(iArcZone+10000)
					wghtD(iz) = popZ(i75Zone) + empZ(i75Zone)
					iDestList(iz) = i75Zone
  121				continue
			else
				i75Zone = indSE(idest)
				wghtD(1) = 1.
				iDestList(1) = i75Zone
			endif
c			fill in trips
			wghtSum = 0.
			do 122 i=1,size(iOrigList)
			do 122 j=1,size(iDestList)
				izone = iOrigList(i)
				jzone = iDestList(j)
				if (izone.eq.0.or.jzone.eq.0) goto 122
				distance = amin1(1. * idis(izone,jzone), 200.)
				wght(i,j) = wghtO(i) * wghtD(j) * 
     *				exp(beta * distance)
				wghtSum = wghtSum + wght(i,j)
  122			continue
			do 123 i=1,size(iOrigList)
			do 123 j=1,size(iDestList)
				izone = iOrigList(i)
				jzone = iDestList(j)
				if (izone.eq.0.or.jzone.eq.0) goto 123
				detTrips(izone,jzone) = trips(iorig,idest) * wght(i,j) / wghtSum
  123			continue
  120		continue
  100 continue
	return
	end
c
c
c
	real*8 function getMatrixSum(trips,n,i,j)
c
c	sum up matrix
c
	real*8 trips(n,n)
c
	getMatrixSum = 0.
	do 100 k=i,j
	do 100 l=i,j
		getMatrixSum = getMatrixSum + trips(k,l)
  100 continue
	end
c
c
c
	integer function iGetFinalPos(iGdot)
c
	common /gdotArcRef/ iGdotArc(240,58)
c
	do 100 iGetFinalPos=size(iGdotArc,dim=2),1,-1
		if (iGdotArc(iGdot,iGetFinalPos).gt.0) return
  100 continue
	write (6,'(a,i4)') 'Error. Could not find final position for ',iGdot
	end
c
c
c
	real function getStraightLineDist (i,j)
c
c	calculate the straight line distance between two raster cells
c
	common /centroidCoordinates/ ic(5000),icInd(8000),long(5000),lati(5000)
c
	getStraightLineDist = -1.
	factor = 17562.28263  ! factor to convert lati/long into miles
	if (icInd(i).eq.0.or.icInd(j).eq.0) return  ! not a raster cell
	vlg = (long(icInd(i)) - long(icInd(j))) / factor
	vlt = (lati(icInd(i)) - lati(icInd(j))) / factor
	getStraightLineDist = ((vlg)**2 + (vlt)**2)**0.5
	return
	end