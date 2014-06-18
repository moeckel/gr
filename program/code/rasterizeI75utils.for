	subroutine screenLogger(yVal,txt)	
c
c	write text on screen
c
	common /windowCoordinate/ maxx,maxy,xl,xr,yb,yt
	INTEGER(2)          maxx, maxy
	common /screenPosition/ yPos
	character*(*) txt
c
	if (yVal.gt.0) then
		yPos = yVal
	else
		yPos = yPos - yVal
	endif
	call setwindow(0.,maxx * 1.,0.,maxy * 1.)
c	call drawText(20.,yPos,1,0.,20.,1.2,trim(txt))
	if (yVal.eq.0) then
	call setColorInRGB(255,255,255)
		call drawFilledRectangle(20.,maxy*1.-yPos-34.,200.,maxy*1.-yPos-15.,1)
	call setColorInRGB(0,0,0)
	endif
	call drawText(20.,maxy*1.-yPos,1,0.,20.,1.2,trim(txt))
	call setThisScreen
	return
	end
c
c
c
	subroutine setThisScreen
c
c	set the window coordinates
c
c	all polygons
c	xlv = -11167786.
c	xrv = 4369616.
c	ybv = 9492216.
c	ytv = 20333237.
c	Georgia
	xlv = -1666667.
	xrv =  1000000.
	ybv = 11500000.
	ytv = 13500000.
	call setwindow(xlv,xrv,ybv,ytv)
	return
	end
c
c
c
