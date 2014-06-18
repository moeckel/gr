	subroutine checkfile(fileName,length)
c
c	generates file size summary
c
	character fileName*length
c	check size of file
	call getFileSize(fileName,irowCount,icolCount,icolCharacters)
	write (6,'(3(a,i10,a/))') 
     *'   Found ',irowCount,' valid rows and ',
     *'         ',icolCount,' columns and ',
     *'   up to ',(icolCharacters + 1),' characters in a single row.'
	return
	end
c
c
c
	subroutine getFileSize(fileName,irowCount,icolCountHead,icolCharacters)
c
c	count number of columns
c     17 Sept. 2009, written by Rolf Moeckel (PB NY)
c
	character fileName*100,txt*100000,dummy*1
c
	write (6,'(2a)') ' Analyzing the size of file ',fileName
	call checkIfFileExists(fileName,len(fileName))
	open (10,file=fileName)
	icolCharacters = 0
	read (10,'(a)') txt
	icolCharacters = len_trim(txt)
	call countColumns(trim(txt),len_trim(txt),' ',0,icolCountHead,idm)
	irowCount = 0
	do while (.true.)
	  read (10,'(a)',end=99) txt
	  call countColumns(trim(txt),len_trim(txt),' ',0,icolCount,idummy)
	  if (icolCount.eq.icolCountHead) then
	    irowCount = irowCount + 1
	    icolCharacters = max0(icolCharacters,len_trim(txt))
	  else
		  goto 99
		endif
	end do
   99 continue
	close (10)
	return
	end
c
c
c
	subroutine checkIfFileExists(fileName, ilength)
c
c	check if file exists
c
	character fileName*ilength
	logical fileExists
c
	inquire (file = fileName, EXIST = fileExists)
	if (.not.fileExists) then
		write (6,'(2a/)') ' Fatal error: Cannot find file ', fileName
		read (5,*)
		stop
	end if
	return
	end
c
c
c
	subroutine countColumns(txt,ltxt,colName,lcolName,icolCount,icolNum)
c
c	count number of comma-delineated columns, return icolNum if specific 
c	column colName is requested
c
	character txt*ltxt,colName*lcolName
	logical endOfCol
c
	icolCount = 1
	iquotCount = 0
	icolNum = 0
	k = 0
	do 100 i=1,ltxt
		if (txt(i:i).eq.'"') then
			iquotCount = iquotCount + 1
		endif
		if (mod(iquotCount,2).ne.0) goto 100
		endOfCol = .false.
		if (txt(i:i).eq.',') endOfCol = .true.
		if (txt(i:i).eq.' ') then
			endOfCol = .true.
			do 110 j=i,ltxt
				if (txt(j:j).ne.' ') endOfCol = .false.
  110			continue
		endif
		if (i.eq.len(txt)) endOfCol = .true.
		if (endOfCol) then
			if (txt(i-1:i-1).ne.'"') then
				if (txt(k+1:i-1).eq.colName) icolNum = icolCount
			else
				if (txt(k+2:i-2).eq.colName) icolNum = icolCount
			endif
			icolCount = icolCount + 1
			k = i
		endif
  100 continue
	if (lcolName.gt.0.and.icolNum.eq.0) write (6,'(3a)') ' Error: Could ',
     *					'not find column ',trim(colName)
	return
	end
c
c
c
	subroutine readCSVcontent (fileName,irow,icol,lines)
c
c	opens file and reads content into character arrays
c
	character fileName*100,lines(0:irow)*icol,dummy*1
c
	call checkIfFileExists(fileName,len(fileName))
c	write (6,'(2a)') ' Reading file ',fileName
	open (10,file=fileName)
	read (10,'(a)') lines(0)
	do 100 i=1,irow
	  read (10,'(a)',end=101) lines(i)
  100 continue
  101 continue
      irow = i-1   ! adjust irow if file is shorter than expected
	k = 0
	do while (.true.) 
		read (10,'(a)',end=201) dummy
		k = k + 1
	end do
  201 continue
  	if (k.gt.2) then
  		write (6,'(3a,i8,2a,i8,a)') ' SEVERE ERROR: The file ',
     *  trim(fileName),' has ',k,' rows more than expected . Those',
     *  ' lines were not read. Currently set to read ',irow,
     *  ' lines in this particular file. Adjust code.'
  	endif
	close (10)
	return
	end
c
c
c
	subroutine getCSVint(lines,irow,icol,columnName,k,integerArray)
c
c	read integerArray from table with lines (irow x icol) from columnName*k
c
	dimension integerArray(irow)
	character columnName*k,lines(0:irow)*icol,linePoint*(icol+1)
c
	ipos = index(lines(0), trim(columnName))
	if (ipos.eq.0) then
		write (6,'(3a/)') ' Fatal error: The column ',trim(columnName),
     *                    ' does not exist in file.'
	endif
	call countColumns(lines(0),icol,columnName,k,icolCount,icolNum)
	do 100 i=1,irow
	  integerArray(i) = integerValue(lines(i),icol,icolNum)
  100 continue
	return
	end
c
c
c
	integer function integerValue(txt,ltxt,icolNum)
c
c	returns integer value from txt*ltxt in column icolNum
c
	character txt*ltxt
c
	integerValue = 0
	icolCount = 1
	iquotCount = 0
	k = 0
	do 100 i=1,ltxt
		if (txt(i:i).eq.'"') then
			iquotCount = iquotCount + 1
		endif
		if (mod(iquotCount,2).ne.0) goto 100
		if (txt(i:i).eq.','.or.i.eq.ltxt) then
			icolCount = icolCount + 1
			if (icolCount.eq.icolNum+1) then
				read (txt(k+1:i-1),'(i)') integerValue
				return
			endif
			k = i
		endif
  100 continue
	return
	end
c
c
c
	subroutine getCSVreal(lines,irow,icol,columnName,k,realArray)
c
c	read realArray from table with lines (irow x icol) from columnName*k
c
	dimension realArray(irow)
	character columnName*k,lines(0:irow)*icol
c
	ipos = index(lines(0), trim(columnName))
	if (ipos.eq.0) then
		write (6,'(3a/)') ' Fatal error: The column ',trim(columnName),
     *                    ' does not exist in file.'
		read (5,*)
		stop
	endif
	call countColumns(lines(0),icol,columnName,k,icolCount,icolNum)
	do 100 i=1,irow
	  realArray(i) = realValue(lines(i),icol,icolNum)
  100 continue
	return
	end
c
c
c
	real function realValue(txt,ltxt,icolNum)
c
c	returns real*4 value from txt*ltxt in column icolNum
c
	character txt*ltxt
	logical hasDecP
c
	realValue = 0
	icolCount = 1
	iquotCount = 0
	k = 0
	do 100 i=1,ltxt
		if (txt(i:i).eq.'"') then
			iquotCount = iquotCount + 1
		endif
		if (mod(iquotCount,2).ne.0) goto 100
		if (txt(i:i).eq.','.or.i.eq.ltxt) then
			icolCount = icolCount + 1
			if (icolCount.eq.icolNum+1) then
				hasDecP = .false.
				do 110 n=k+1,i-1
					if (txt(n:n).eq.'.') hasDecP = .true.
  110				continue
				if (hasDecP) then
					read (txt(k+1:i-1),'(f)') realValue
				else
					read (txt(k+1:i-1),'(i)') intValue
					realValue = real(intValue)
				endif
				return
			endif
			k = i
		endif
  100 continue
	return
	end
c
c
c
	subroutine getCSVchar(lines,irow,icol,columnName,k,chaArray,l)
c
c	read chaArray from table with lines (irow x icol) from columnName*k
c
	character columnName*k,lines(0:irow)*icol,chaArray(irow)*l
c
	ipos = index(lines(0), trim(columnName))
	if (ipos.eq.0) then
		write (6,'(3a/)') ' Fatal error: The column ',trim(columnName),
     *                    ' does not exist in file.'
		read (5,*)
		stop
	endif
	call countColumns(lines(0),icol,columnName,k,icolCount,icolNum)
	do 100 i=1,irow
	   call charValue(lines(i),icol,icolNum,chaArray(i),l)
  100 continue
	return
	end
c
c
c
	subroutine charValue(txt,ltxt,icolNum,out,l)
c
c	returns character value from txt*ltxt in column icolNum
c
	character txt*ltxt,out*l
c
	out = ''
	icolCount = 1
	iquotCount = 0
	k = 0
	do 100 i=1,ltxt
		if (txt(i:i).eq.'"') then
			iquotCount = iquotCount + 1
		endif
		if (txt(i:i).eq.',') then
c			Empty field
			out = ''
c			return
		endif
		if (mod(iquotCount,2).ne.0) goto 100
		if (txt(i:i).eq.','.or.i.eq.ltxt) then
			icolCount = icolCount + 1
			if (icolCount.eq.icolNum+1) then
				read (txt(k+1:i-1),'(a)') out
				if (out(1:1).eq.'"') read(txt(k+2:i-2),'(a)') out
				return
			endif
			k = i
		endif
  100 continue
	return
	end
c
c
c
	subroutine getCSVlogical(lines,irow,icol,columnName,k,logicalArray)
c
c	read logicalArray from table with lines (irow x icol) from columnName*k
c
	logical logicalArray(irow),logicalValue
	character columnName*k,lines(0:irow)*icol
c
	ipos = index(lines(0), trim(columnName))
	if (ipos.eq.0) then
		write (6,'(3a/)') ' Fatal error: The column ',trim(columnName),
     *                    ' does not exist in file.'
	endif
	call countColumns(lines(0),icol,columnName,k,icolCount,icolNum)
	do 100 i=1,irow
	  logicalArray(i) = logicalValue(lines(i),icol,icolNum)
  100 continue
	return
	end
c
c
c
	logical function logicalValue(txt,ltxt,icolNum)
c
c	returns logical value from txt*ltxt in column icolNum
c
	character txt*ltxt
c
	icolCount = 1
	iquotCount = 0
	k = 0
	do 100 i=1,ltxt
		if (txt(i:i).eq.'"') then
			iquotCount = iquotCount + 1
		endif
		if (mod(iquotCount,2).ne.0) goto 100
		if (txt(i:i).eq.','.or.i.eq.ltxt) then
			icolCount = icolCount + 1
			if (icolCount.eq.icolNum+1) then
				if (txt(k+1:i-1).eq.'TRUE'.or.txt(k+1:i-1).eq.'T') then
					logicalValue = .true.
				elseif (txt(k+1:i-1).eq.'FALSE'.or.txt(k+1:i-1).eq.'F') then
					logicalValue = .false.
				else
					write (6,'(4a)') 'Invalid value in logical function logicalValue:',
     *					'<',txt(k+1:i-1),'>'
				endif
				read (txt(k+1:i-1),'(a)') 
				return
			endif
			k = i
		endif
  100 continue
	return
	end
c
c
c
	subroutine removeSpacesFromLine (txt,txt_nb)
c
c	remove blanks from txt
c
	character txt*(*),txt_nb*(*)
c
	txt_nb = ''
	ls2 = 0
	do 100 i=1,len_trim(txt)
		if(txt(i:i).ne.' ') then
			ls2 = ls2 + 1
			txt_nb(ls2:ls2) = txt(i:i)
		endif
  100	continue
	return
	end
c
c
c
	subroutine removeSpaces (fileName)
c
c	read in ascii file and write out without blanks
c
	character fileName*200,outFile*200,line*1001,outLine*1000
c
	do 100 i=len(fileName),1,-1
		if (fileName(i:i).eq.'.') then
			write (outFile,'(3a)') fileName(1:i-1),'_nb.',
     *			fileName(i+1:len(fileName)-3)
			goto 101
		endif
  100 continue
  101 continue
c
	open (10,file=trim(fileName),action='read')
	open (20,file=trim(outFile),action='write',form='FORMATTED')
	n = 1
	do while (.true.)
		read (10,'(a)',end=300) line
		outLine = ''
		if (line(len(line):len(line)).ne.' ') then
			write (6,'(3a,i4,a,i8)') 'Error in readCSV.removeBlanks: File ',
     *			trim(fileName),' has more than ',len(line)-1,
     *			' characters in line ',n
			read (5,*)
		endif
		ls2 = 0
		do 200 i=1,len_trim(line)
			if(line(i:i).ne.' ') then
				ls2 = ls2 + 1
				outLine(ls2:ls2) = line(i:i)
			endif
  200		continue
		write (20,'(a)') trim(outLine)
		n = n + 1
	end do
  300 continue
	close (10)
	close (20)
	return
	end