	subroutine readData
c
c	read in shapefile with NCSTM zones
c
	character*200 fl
	common /i75shp/ nt(4500),np(4500,60),x(4500,10000),
     *	y(4500,10000),nz
	common /i75zoneSystem/ idbfID(4500),id(4500),ind(12027)
c
	if (.true.) then
		fl = 'shp\disaggTAZ.shp'
c		call readSizeOfPolygonShp(fl)
		call readPolygonShp(fl,size(nt),size(np,dim=2),size(x,dim=2),nt,np,
     *		x,y,nz)
		fl = 'shp\disaggTAZ.dbf'
		call readShpDbfFile(fl)
		open (20,file='data\i75DisaggShapefile.bin',form='binary')
		write (20) nt,np,x,y,nz,idbfID,id,ind
		close (20)
	else
		open (20,file='data\i75DisaggShapefile.bin',form='binary')
		read (20) nt,np,x,y,nz,idbfID,id,ind
		close (20)
	endif
	call readSEdata
	return
	end
c
c
c
	subroutine readShpDbfFile(fileName)
	! Reads .DBF files, lists header and first few records.
	! Clive Page, 2005 July 9
	IMPLICIT NONE
	INTEGER, PARAMETER :: maxcol = 128
	CHARACTER :: colname(maxcol)*11, coltype(maxcol)*1
	INTEGER   :: colwidth(maxcol), coldec(maxcol), coloff(maxcol)
	CHARACTER :: version*1, year*1, month*1, day*1,
     *             ca*4, cwidth, cdec, string*100, flag*1
	INTEGER   :: nrecs, ncols, icol, irec, ioffset, dataoff, cw, k
	INTEGER(kind=selected_int_kind(3)) :: lhead, lenrec  ! =integer*2
c
	character*200 fileName,txt
	common /i75zoneSystem/ idbfID(4500),id(4500),ind(12027)
	dimension idbfTAZ(5000)
	integer*4 id,ind,idbfID,idbfTAZ
	!
c	write (txt,'(2a)') 'Reading zone system from ',trim(fileName)
c	call screenLogger(-20.,txt)
	OPEN(unit=1, file=fileName, status='old', ACCESS='stream')
	READ(1) version, year, month, day, nrecs, lhead, lenrec
	ncols = (lhead - 32)/32
c	WRITE(*, '(a,i4, a,i4, 2("-",i2.2), 3(i6,a))')
c     *	      'Version ', ichar(version),
c     *	      ' Date ', ichar(year)+1900, ichar(month), ichar(day),
c     *	      nrecs, ' rows', ncols, ' columns', lenrec, ' bytes/row'
	!
c	WRITE(67,*)'Col ---Name--- T Width Decimals Offset'
	ioffset = 1
	DO icol = 1,ncols
	   READ(1, POS=32*icol+1) colname(icol), coltype(icol), ca, cwidth,cdec
	   k = INDEX(colname(icol), char(0))
	   if (k.eq.0) k = 11
	   colname(icol)(k:) = " "
	   colwidth(icol) = ichar(cwidth)
	   coldec(icol)   = ichar(cdec)
	   coloff(icol) = ioffset
	   ioffset = ioffset + colwidth(icol)
c	   WRITE(67, '(i3,1x,a,1x,a,2i6,i8)') icol, colname(icol),
c     *	      coltype(icol), colwidth(icol), coldec(icol), coloff(icol)
	END DO

	dataoff = 32 * ncols + 35 - 1
	DO irec = 1,nrecs
	   READ(1, pos=dataoff + (irec-1)*lenrec) flag
	   DO icol = 1,ncols
	      ioffset = dataoff + (irec-1) * lenrec + coloff(icol)
	      cw = colwidth(icol)
	      READ(1, pos=ioffset) string(1:cw)
			if (trim(colname(icol)).eq.'FID_SWM_TA') then
				read(string(1:cw),'(i)') idbfID(irec)
			elseif (trim(colname(icol)).eq.'COMTAZ') then
				read(string(1:cw),'(i)') id(idbfID(irec))
			endif
	   END DO
	END DO
	close (1)
	do 100 irec=1,size(idbfID)
		if (id(irec).eq.0) goto 100
		ind(id(irec)) = irec
  100 continue
	return
	end
c
c
c
	subroutine readSEdata
c
c	read in socio-economic data
c
	character*200 txt,fileName,columnName
	dimension lines(0:4500)
	character*50 lines
	common /seData/ itaz(4500),indSE(12027),popZ(4500),hhZ(4500),empZ(4500)
c
	fileName = 'data\seData_detailedZoneSystem.csv'
c	write (txt,'(2a)') 'Reading socio-economic data from ',trim(fileName)
c	call screenLogger(-20.,txt)
c	call checkfile(fileName,len(fileName))
 	irow = size(lines)-1
	icol = len(lines)
	call readCSVcontent (fileName,irow,icol,lines)
	write (columnName,'(a)') 'TAZ'
	call getCSVint(lines,irow,icol,columnName,len(columnName),itaz)
	write (columnName,'(a)') 'pop'
	call getCSVreal(lines,irow,icol,columnName,len(columnName),popZ)
	write (columnName,'(a)') 'hh'
	call getCSVreal(lines,irow,icol,columnName,len(columnName),hhZ)
	write (columnName,'(a)') 'emp'
	call getCSVreal(lines,irow,icol,columnName,len(columnName),empZ)
	do 100 i=1,size(itaz)
		if (itaz(i).eq.0) goto 100
		indSE(itaz(i)) = i
  100 continue
	return
	end
c
c
c
	subroutine readTripTables
c
c	read auto and truck trip tables
c
	character*200 fileName,columnName,header,line,txt
	common /trips/ tAuto(2600,2600),tTrucks(2600,2600)
c
	write (txt,'(a)') 'Reading auto trip table...'
	call screenLogger(-20.,txt)
	if (.false.) then
		write (fileName,'(a)') 'data\OD_PC_list.csv'
	  call checkIfFileExists(fileName, len(fileName))
		open (10,file=fileName,action='read')
	  read (10,'(a)') header
	  write (columnName,'(a)') 'From'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icolorig)
	  write (columnName,'(a)') 'To'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icoldest)
	  write (columnName,'(a)') 'total'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icoltrps)
		n = 1
		do while (.true.)
			read (10,'(a)',end=99) line
			io = integerValue(line,len(line),icolorig)
			id = integerValue(line,len(line),icoldest)
			trp = realValue(line,len(line),icoltrps)
			tAuto(io,id) = tAuto(io,id) + trp
			n = n + 1
		end do
   99		continue
		close (10)
		write (txt,'(a,i,a)') '  Read ',n,' auto trips'
		call screenLogger(-20.,txt)
		open (30, file = 'data\autoTripTable.bin', form = 'binary')
		write (30) tAuto
		close (30)
	else
		open (30, file = 'data\autoTripTable.bin', form = 'binary')
		read (30) tAuto
		close (30)
	endif
c
	write (txt,'(a)') 'Reading truck trip tables...'
	call screenLogger(-20.,txt)
	if (.false.) then
		write (fileName,'(a)') 'data\Truck_Total_list.csv'
	  call checkIfFileExists(fileName, len(fileName))
		open (10,file=fileName,action='read')
	  read (10,'(a)') header
	  write (columnName,'(a)') 'From'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icolorig)
	  write (columnName,'(a)') 'To'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icoldest)
	  write (columnName,'(a)') 'total'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icoltrps)
		n = 1
		do while (.true.)
			read (10,'(a)',end=199) line
			io = integerValue(line,len(line),icolorig)
			id = integerValue(line,len(line),icoldest)
			trp = realValue(line,len(line),icoltrps)
			tTrucks(io,id) = tTrucks(io,id) + trp
			n = n + 1
		end do
  199		continue
		close (10)
		write (txt,'(a,i,a)') '  Read ',n,' truck trips by commodity'
		call screenLogger(-20.,txt)
c	add intermodal trucks
		write (fileName,'(2a)') 'data\Truck_Intmod_list.csv'
	  call checkIfFileExists(fileName, len(fileName))
		open (10,file=fileName,action='read')
	  read (10,'(a)') header
	  write (columnName,'(a)') 'From'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icolorig)
	  write (columnName,'(a)') 'To'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icoldest)
	  write (columnName,'(a)') 'intmodal'
	  call countColumns(header,len(header),columnName,len(columnName),
     *                  icolCount,icoltrps)
		n = 1
		do while (.true.)
			read (10,'(a)',end=299) line
			io = integerValue(line,len(line),icolorig)
			id = integerValue(line,len(line),icoldest)
			trp = realValue(line,len(line),icoltrps)
			tTrucks(io,id) = tTrucks(io,id) + trp
			n = n + 1
		end do
  299		continue
		close (10)
		write (txt,'(a,i,a)') '  Read ',n,' intermodal truck trips'
		call screenLogger(-20.,txt)
		open (30, file = 'data\trkTripTable.bin', form = 'binary')
		write (30) tTrucks
		close (30)
	else
		open (30, file = 'data\trkTripTable.bin', form = 'binary')
		read (30) tTrucks
		close (30)
	endif
	return
	end
c
c
c
	subroutine readTazReferences
c
c	Read translation from TAZ to raster cells and reference from ARC to GDOT zones
c
	common /seData/ itaz(4500),indSE(12027),popZ(4500),hhZ(4500),empZ(4500)
	dimension lines(0:19385),idrc(19385),itazR(19385),area(19385)
	dimension linesRef(0:2116),iArc(2116),iGdot(2116)
	dimension linesMn(0:5384),mnTaz(5384),irst(5384)
	character lines*45,linesRef*10,linesMn*15
	character*200 fileName,columnName
	common /rasterAgg/ irstByTaz(4500,28),rstShareOfTaz(4500,28),
     *	itazByRst(12027,24),tazShareOfRst(12027,24),mainTazOfRs(8000)
	common /gdotArcRef/ iGdotArc(240,58)
c
	fileName = 'data\rasterTazReference.csv'
c	call checkfile(fileName,len(fileName))
	irow = size(lines)-1
	icol = len(lines)
	call readCSVcontent (fileName,irow,icol,lines)
	write (columnName,'(a)') 'NEWRASTERC'
	call getCSVint(lines,irow,icol,columnName,len(columnName),idrc)
	write (columnName,'(a)') 'COMTAZ'
	call getCSVint(lines,irow,icol,columnName,len(columnName),itazR)
	write (columnName,'(a)') 'F_AREA'
	call getCSVreal(lines,irow,icol,columnName,len(columnName),area)
	do 100 i=1,size(itazR)
		if (itazR(i).eq.0) goto 100
		izone = indSE(itazR(i))
		do 110 j=1,size(irstByTaz,dim=2)
			if (irstByTaz(izone,j).eq.0) goto 111
  110		continue
  111		continue
		irstByTaz(izone,j) = idrc(i)
		rstShareOfTaz(izone,j) = area(i)
		do 120 j=1,size(tazShareOfRst,dim=2)
			if (tazShareOfRst(idrc(i),j).eq.0) goto 121
  120		continue
  121		continue
		itazByRst(idrc(i),j) = itazR(i)
		tazShareOfRst(idrc(i),j) = area(i)
  100	continue
	do 200 i=1,size(irstByTaz,dim=1)
		sm = 0.
		do 210 j=1,size(irstByTaz,dim=2)
			sm = sm + rstShareOfTaz(i,j)
  210		continue
		if (sm.eq.0) goto 200
		do 220 j=1,size(irstByTaz,dim=2)
			rstShareOfTaz(i,j) = rstShareOfTaz(i,j) / sm
  220		continue
  200	continue
	do 300 i=1,size(itazByRst,dim=1)
		sm = 0.
		do 310 j=1,size(itazByRst,dim=2)
			sm = sm + tazShareOfRst(i,j)
  310		continue
		if (sm.eq.0) goto 300
		do 320 j=1,size(itazByRst,dim=2)
			tazShareOfRst(i,j) = tazShareOfRst(i,j) / sm
  320		continue
  300	continue
c	read GDOT/ARC zone reference
	fileName = 'data\arc_gdot_taz_reference.csv'
c	call checkfile(fileName,len(fileName))
	irow = size(linesRef)-1
	icol = len(linesRef)
	call readCSVcontent (fileName,irow,icol,linesRef)
	write (columnName,'(a)') 'ARC'
	call getCSVint(linesRef,irow,icol,columnName,len(columnName),iArc)
	write (columnName,'(a)') 'GDOT'
	call getCSVint(linesRef,irow,icol,columnName,len(columnName),iGdot)
	iGdotArc = 0
	do 400 i=1,size(iArc)
		if (iGdot(i).gt.240) goto 400  ! ignore ARC's external stations
		do 410 j=1,size(iGdotArc,dim=2)
			if (iGdotArc(iGdot(i),j).eq.0) goto 411
  410		continue
  411		continue
		iGdotArc(iGdot(i),j) = iArc(i)
  400 continue
c	fill in pointer from raster cell to main TAZ
	fileName = 'data\mainTAZofRaster.csv'
c	call checkfile(fileName,len(fileName))
	irow = size(linesMn)-1
	icol = len(linesMn)
	call readCSVcontent (fileName,irow,icol,linesMn)
	write (columnName,'(a)') 'RS'
	call getCSVint(linesMn,irow,icol,columnName,len(columnName),irst)
	write (columnName,'(a)') 'TAZ'
	call getCSVint(linesMn,irow,icol,columnName,len(columnName),mnTaz)
	do 500 i=1,size(irst)
		mainTazOfRs(irst(i)) = mnTaz(i)
  500 continue
	return
	end
c
c
c
	subroutine readSkim
c
c	read skim matrix for raster cells in ARC region
c
	character fileName*200,colName*16,header*100,line*100
	common /skim/ ids(2000:8000,2000:8000),itm(2000:8000,2000:8000)
	integer*2 ids,itm
c
	if (.false.) then
		write (fileName,'(a)') 'data\timeAndDistanceSkim.csv'
		open (10,file=fileName,action='read')
		read (10,'(a)') header
		write (colName,'(a)') 'orig'
		call countColumns(header,len(header),colName,len(colName),
     *		icolCount,icolorig)
		write (colName,'(a)') 'dest'
		call countColumns(header,len(header),colName,len(colName),
     *		icolCount,icoldest)
		write (colName,'(a)') 'dist'
		call countColumns(header,len(header),colName,len(colName),
     *		icolCount,icoldist)
		write (colName,'(a)') 'time'
		call countColumns(header,len(header),colName,len(colName),
     *		icolCount,icoltime)
			n = 1
			do while (.true.)
				if (mod(n,1000000).eq.0) write (6,'(a,i3,a)') '   Processing: ',
     *	                          n/1000000,'000000'
				read (10,'(a)',end=99) line
				io = integerValue(line,len(line),icolorig)
				id = integerValue(line,len(line),icoldest)
				dist = realValue(line,len(line),icoldist)
				time = realValue(line,len(line),icoltime)
				ids(io,id) = dist + 0.5
				itm(io,id) = time + 0.5
				n = n + 1
			end do
   99		continue
		close (10)
		write (fileName,'(a)') 'data\timeAndDistanceSkim.bin'
		open (20,file=fileName,form='binary')
		write (20) ids,itm
		close (20)
	else
		write (fileName,'(a)') 'data\skimTime.bin'
		open (21,file=fileName,form='binary')
		read (21) itm
		close (21)
		write (fileName,'(a)') 'data\skimDist.bin'
		open (22,file=fileName,form='binary')
		read (22) ids
		close (22)
	endif
	return
	end
c
c
c
	subroutine readArcSkim
c
c	read ARC skim matrix for ARC TAZ
c
	character fileName*200,colName*16,header*100,line*40
	common /skimARC/ idis(4600,4600)
	integer*2 idis
	common /seData/ itaz(4500),indSE(12027),popZ(4500),hhZ(4500),empZ(4500)
c
	if (.false.) then
		write (fileName,'(a)') 'data\distSkimGdotPlusArc.csv'
		open (10,file=fileName,action='read')
		read (10,'(a)') header
		write (colName,'(a)') 'From'
		call countColumns(header,len(header),colName,len(colName),
     *		icolCount,icolorig)
		write (colName,'(a)') 'To'
		call countColumns(header,len(header),colName,len(colName),
     *		icolCount,icoldest)
		write (colName,'(a)') 'Dist'
		call countColumns(header,len(header),colName,len(colName),
     *		icolCount,icoldist)
			n = 1
			do while (.true.)
				if (mod(n,1000000).eq.0) write (6,'(a,i3,a)') '   Processing: ',
     *	                          n/1000000,'000000'
				read (10,'(a)',end=99) line
				io = integerValue(line,len(line),icolorig)
				id = integerValue(line,len(line),icoldest)
				dist = realValue(line,len(line),icoldist)
				idis(indSE(io),indSE(id)) = dist + .5
				n = n + 1
			end do
   99		continue
		close (10)
		write (fileName,'(a)') 'data\skimDistByGdotArcTaz.bin'
		open (20,file=fileName,form='binary')
		write (20) idis
		close (20)
	else
		write (fileName,'(2a)') 'data\skimDistByGdotArcTaz.bin'
		open (20,file=fileName,form='binary')
		read (20) idis
		close (20)
	endif
	return
	end
c
c
c
	subroutine readStraightLineDistance
c
c	Read centroid coordinates of raster cell 
c
	dimension lines(0:5000)
	character lines*50
	character*200 fileName,columnName
	common /centroidCoordinates/ ic(5000),icInd(8000),long(5000),lati(5000)
c
	fileName = 'data\rasterCellNodeCoordinates.csv'
c	call checkfile(fileName,len(fileName))
	irow = size(lines)-1
	icol = len(lines)
	call readCSVcontent (fileName,irow,icol,lines)
	write (columnName,'(a)') 'centroid'
	call getCSVint(lines,irow,icol,columnName,len(columnName),ic)
	write (columnName,'(a)') 'LONGITUDE'
	call getCSVint(lines,irow,icol,columnName,len(columnName),long)
	write (columnName,'(a)') 'LATITUDE'
	call getCSVint(lines,irow,icol,columnName,len(columnName),lati)
	do 100 i=1,size(ic)
		if (ic(i).eq.0) goto 100
		icInd(ic(i)) = i
  100 continue
	return
	end