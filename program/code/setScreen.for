	subroutine setScreen(title)
c
c	set screen for drawing
c
	use ifqwin
	character*200 title
c
	call quickwin(title)
c	write (6,'(a)') trim(title)
	call graphicsmode( )
	return
      end
c
c
c
	subroutine clearTheScreen
c
c	Clears screen
c
	use ifqwin
c
      call clearscreen($gclearscreen)
	return
	end
c
c
c
	SUBROUTINE graphicsmode( )
c
	USE IFQWIN
	LOGICAL             modestatus
	TYPE (windowconfig) myscreen
	common /windowCoordinate/ maxx,maxy,xl,xr,yb,yt
	INTEGER(2)          maxx, maxy
	!  Set highest resolution graphics mode.
	myscreen.numxpixels=-1
	myscreen.numypixels=-1
	myscreen.numtextcols=-1
	myscreen.numtextrows=-1
c	myscreen.numcolors=-1
	myscreen.fontsize=-1
	myscreen.title = " "C ! blank
	modestatus=SETWINDOWCONFIG(myscreen)
	!  Determine the maximum dimensions.
	modestatus=GETWINDOWCONFIG(myscreen)
	maxx=myscreen.numxpixels - 1
	maxy=myscreen.numypixels - 1
	END SUBROUTINE
c
c
c
	subroutine setwindow(xlv,xrv,ybv,ytv)
c
c	set coordinates of four window corners
c
	common /windowCoordinate/ maxx,maxy,xl,xr,yb,yt
	integer(2) maxx,maxy
	common /monitor/ iy
c
	xl = xlv
	xr = xrv
	yb = ytv   ! replace lower and upper y coordinate to 
	yt = ybv   ! start counting from bottom of screen
	iy = maxy
	return
	end
c
c
c
	FUNCTION newx(xcoord)
	! NEWX - This function finds new x-coordinates.
	common /windowCoordinate/ maxx,maxy,xl,xr,yb,yt
	INTEGER(2) maxx, maxy
	REAL(4) xcoord, tempx
c
	tempx = (xcoord - xl) * (1. * maxx) / (xr-xl)
	newx = tempx + 0.5
	END FUNCTION
c
c
c
	FUNCTION newy(ycoord)
	! NEWY - This function finds new y-coordinates.
	common /windowCoordinate/ maxx,maxy,xl,xr,yb,yt
	INTEGER(2) maxx, maxy
	REAL(4) ycoord, tempy
c
	tempy = (ycoord - yb) * (1. * maxy) / (yt-yb)
	newy = tempy + 0.5
	END FUNCTION
c
c
c
	subroutine drawLine(x1,y1,x2,y2,ltp)
c
c	draw line from x1/y1 to x2/y2 with line type ltp (1: normal, 2: dashes, 3: dots)
c
	use ifqwin
	type (xycoord)  xy
	real*4 x1,y1,x2,y2
c
	integer(2) style
	if (ltp.eq.1) then
		style = Z'FFFF'
	elseif (ltp.eq.2) then
		style = Z'EEEE'
	elseif (ltp.eq.3) then
		style = Z'AAAA'
	endif
	call setlinestyle(style)
	call moveto(int2(newx(x1)),int2(newy(y1)),xy)
	status = lineto(int2(newx(x2)),int2(newy(y2)))
	return
	end
c
c
c
	subroutine drawCurveThroughPoints(x1,y1,x2,y2,ltp)
c
c	draw curved line from x1/y1 to x2/y2 with line type ltp (1: normal, 2: dashes, 3: dots)
c
	use ifport
	use ifqwin
	real*4 x1,y1,x2,y2,xr,yr,r,width
	integer(2) status,xbl,ybl,xbb,ybb,xa1,ya1,xa2,ya2
	logical dir
c
c	calculate the center between x1/y1 and x2/y2
	xc = x1 + (x2-x1)/2
	yc = y1 + (y2-y1)/2
c	choose randomly which direction arc should take
	if (rand() > 0.5) then
		dir = .true.
	else
		dir = .false.
	endif
c	select randomly how wide arc shall be
	width = amax1(1., rand()*10.)
c	calculate the center of the arc by building a line perpendicular to x1/y1 - x2/y2, select direction of arc randomly
	if (dir) then
		xr = xc - (y2-y1) * width
		yr = yc + (x2-x1) * width
	else
		xr = xc + (y2-y1) * width
		yr = yc - (x2-x1) * width
	endif
c	calculate the radius of the arc
	r = ((xr-x1)**2 + (yr-y1)**2)**0.5
c	define outer box coordinates
	xbl = newx(xr+r)
	ybl = newy(yr+r)
	xbb = newx(xr-r)
	ybb = newy(yr-r)
c	define first and second limit of arc
	xa1 = newx(x1)
	ya1 = newy(y1)
	xa2 = newx(x2)
	ya2 = newy(y2)
c	draw arc
	if (dir) then
		status = arc(xbl,ybl,xbb,ybb,xa1,ya1,xa2,ya2)
	else
		status = arc(xbl,ybl,xbb,ybb,xa2,ya2,xa1,ya1)
	endif
	return
	end
c
c
c
	subroutine drawPolygon(n,x,y,ltp)
c
c	draw polygon with n points
c
	dimension x(n),y(n)
c
	do 100 i=1,n-1
		call drawLine(x(i),y(i),x(i+1),y(i+1),ltp)
  100 continue
	call drawLine(x(1),y(1),x(n),y(n),ltp)
	return
	end
c
c
c
	subroutine drawFilledPolygon(n,x,y,ltp)
c
c	draw polygon with n points
c
	use ifqwin
	dimension x(n),y(n)
	integer(2) status
	TYPE (xycoord) poly(n)
	do 100 i=1,n
		poly(i)%xcoord = newx(x(i))
		poly(i)%ycoord = newy(y(i))
  100 continue
	if (ltp.eq.1) then
		status = POLYGON($GFILLINTERIOR , poly, INT2(N))
	else
		status = POLYGON($GBORDER , poly, INT2(N))
	endif
	return
	end
c
c
c
	subroutine drawRectangle(x1,y1,x2,y2,ltp)
c
c	draw rectangle
c
	dimension x(4),y(4)
c
	x(1) = x1
	y(1) = y1
	x(2) = x1
	y(2) = y2
	x(3) = x2
	y(3) = y2
	x(4) = x2
	y(4) = y1
	call drawPolygon(4,x,y,ltp)
	return
	end
c
c
c
	subroutine drawFilledRectangle(x1,y1,x2,y2,ltp)
c
c	draw rectangle
c
	dimension x(4),y(4)
c
	x(1) = x1
	y(1) = y1
	x(2) = x1
	y(2) = y2
	x(3) = x2
	y(3) = y2
	x(4) = x2
	y(4) = y1
	call drawFilledPolygon(4,x,y,ltp)
	return
	end
c
c
c
	subroutine drawText(x,y,ift,al,htt,xpf,txt)
c
c     Draw text (graphics)
c	x   = x coordinate
c	y   = y coordinate
c	ift = font
c
c	Defined fonts:
c	  1  Arial  
c	  2  Arial italics
c	  3  Arial bold
c	  4  Arial bold italics
c
      use dflib
c
	common /window/ wn(4)
      character*(*) txt
      character*20 fstring
	character*3 face(4)
	logical res
c
	data fstring /'t''Arial''hxxxwxxxpxxx'/
	data face /'p  ','pi ','pe ','pei'/
c
      
      ht = htt * 0.85
	if (al.ne.0.) then
	  res = initializefonts()
	  ial = al*10.
     	  call setgtextrotation(ial)
	endif
c	nh = ht/(wn(4)-wn(3))*768.
	nh = ht
	nw = nh*0.33*xpf
	write (fstring(10:12),'(i3.3)') nh
	write (fstring(14:16),'(i3.3)') nw
	write (fstring(17:20),'(a)') face(ift)
 	res = setfont(fstring)
	a = ht*0.8
	x0 = newx(x)-sind(al)*a
	y0 = newy(y)+cosd(al)*a
	call $moveto(x0,y0)
c	call $moveto((1.*newx(x0)),(1.*newy(y0)))
      call outgtext(txt)
	if (al.ne.0.) call setgtextrotation(0)
      return
      end
c
c
c
      subroutine circle(x,y,r,lti)
c
c     Draw a circle
c
	use ifqwin
	integer(2) result,control,x1,x2,y1,y2
c
	if (lti.eq.1) then
		control = $GFILLINTERIOR
	else
		control = $GBORDER
	endif
	x1 = int2(newx(x - r/2.))
	x2 = int2(newx(x + r/2.))
	y1 = int2(newy(y - r/2.))
	y2 = int2(newy(y + r/2.))
	result = ELLIPSE (control,x1,y1,x2,y2)
c      call $elli(x,y,r,r,lti,0)
      return
      end
c
c
c
	subroutine quickwin(title)
c
c	Open Quickwin
c
	use ifqwin
c
	type (windowconfig) wc
	type (qwinfo) qwi
	logical res
	character*200 title
c
	res = setexitqq(qwin$exitnopersist)
c
	wc%numxpixels = 1024
	wc%numypixels = 768
	wc%numtextcols = -1 
	wc%numtextrows = -1 
	wc%title = '    'C
	wc%numcolors = -1 
	wc%fontsize = qwin$extendfont
	wc%extendfontname = "Fixedsys"C
	wc%extendfontsize = #0008000f
	wc%extendfontattributes = #80000000
 	res = setwindowconfig(wc) 
	i = clickmenuqq(qwin$status)
  	call setpalette
	res = setbkcolor(15)
c     Set defined window
 	qwi.x = 0
 	qwi.y = 0
 	qwi.w = 1024
 	qwi.h = 768
      qwi.type = qwin$set
c     Frame window
      result = SETWSIZEQQ (QWIN$FRAMEWINDOW, qwi)
c     Child window
      qwi.type = qwin$max  ! Set max window size of child window
      result = setwsizeqq(0,qwi)
c      res = updatewindow(gethwndqq(0))
      call clearscreen($gclearscreen)
	res = settextcolor(0)
	call setcolour(0)
	call SETLINEWIDTHQQ (0.25)
 	res = initializefonts()
	return
	end
c     
c
c
!      logical(4) function initialsettings()
!c 
!c     Initial setting for frame window
!c
!      use dflib
!c
!	logical*4 bret
!	integer*2 iret
! 	record /qwinfo/ qw
!	external browse,instructions
!c
!	qw%type = qwin$max
! 	iret = setwsizeqq(qwin$framewindow,qw)
!c	bret = appendmenuqq(1,$menuenabled,'File'C,nul)
!c	bret = appendmenuqq(1,$menuenabled,'Print'C,winprint)
!c	bret = appendmenuqq(1,$menuseparator,'sep'C,nul)
!c	bret = appendmenuqq(1,$menuenabled,'Explorer'C,browse)
!c	bret = appendmenuqq(1,$menuseparator,'sep'C,nul)
!c	bret = appendmenuqq(1,$menuenabled,'Exit'C,winexit)
!c	bret = appendmenuqq(2,$menuenabled,'Help'C,nul)
!c	bret = appendmenuqq(2,$menuenabled,'Instructions'C,instructions)
!c	bret = appendmenuqq(2,$menuenabled,'About'C,winabout)
!	initialsettings = .true.
!	return
!	end
!c
!c
!c
!	subroutine browse
!c
!c	Calls Windows explorer
!c
!	use dflib
!	use dfwin
!c
!	integer*2 iret
!	character*20 fn
!	fn = 'zm'
!c
!	iret = winexec('Explorer.exe '//trim(fn)//'.for'C,sw_maximize)
!	end
!c
!c
!c
!	subroutine instructions
!c
!c	Calls help file
!c
!	use dflib
!c
!	integer*2 iret 
!	character*200 szabout
!c
!	szabout = 'Demonstration of \"user created\" menus.\n'C//
!     *          'Options have been provided,\n'C//
!     *          'in the File menu section for opening \n'C//
!     *          'Windows Explorer.\n'C
!	iret = messageboxqq(szabout,'instructions'C,
!     *       mb$iconexclamation.or.mb$ok)
!	end
!c
!c
!c
	subroutine setpal(k)
c
c	set colours
c
	use dflib
	integer*4 ipal(0:15,4)
	integer*2 i
	data ipal /!colorful
	           !0white,1blue,2red,3green,4,5,6,7,8,9,10,11,12,13,14lightgrey,15black
     *           #000000,#FF0000,#0000FF,#00CC33,#0099FF,         
     *           #FF0099,#7459F0,#6145ED,#4931E8,#321EE6,
     *           #0000E0,#FFFFbb,#505050,#707070,#CCCCCC,#FFFFFF,
                 !red
     *           #000000,#FFCCCC,#FFB8B0,#FFA496,#FF907D,
     *           #FA7D66,#F7684F,#F2553D,#EB412A,#E32817,
     *           #DB0000,#FFFFbb,#505050,#707070,#f0f0f0,#FFFFFF,
                 !blue
     *           #000000,#CCCCFF,#BCB3FC,#AB9BFA,#9883F7,
     *           #876EF5,#7459F0,#6145ED,#4931E8,#321EE6,
     *           #0000E0,#FFFFbb,#505050,#707070,#f0f0f0,#FFFFFF,
	           !0white,1red,2orange,3green,4blue,5purple,6,7,8,9,10,11,12,13,14lightgrey,15black
     *           #000000,#FF0000,#ff9900,#00cc00,#0000ff,         
     *           #9900ff,#7459F0,#6145ED,#4931E8,#321EE6,
     *           #0000E0,#FFFFbb,#505050,#707070,#CCCCCC,#FFFFFF/
	do 100 i=0,15
	ipal1 = mod(ipal(i,k),256)
	ipal2 = mod(ipal(i,k),65536)/256
	ipal3 = ipal(i,k)/65536
	ipalr = ipal1*65536+ipal2*256+ipal3
	dummy = remappalettergb(i,ipalr)
  100 continue
	return
	end
c
c
c
	subroutine setColorInRGB(iro,igo,ibo)
c
c	set colours
c
	USE IFQWIN
	INTEGER(2) numfonts
	INTEGER(4) res,color,ipal
	TYPE (xycoord) xy
	character hex*6
c
      ir = iro
      ig = igo
      ib = ibo
      call reasonableColorRange(ir)
      call reasonableColorRange(ig)
      call reasonableColorRange(ib)
	call getHexCode(ir,ig,ib,hex)
	numfonts = INITIALIZEFONTS( )
	read (hex,'(z6)') ipal
	res = SETCOLORRGB(ipal)
      end
c
c
c
      subroutine reasonableColorRange(ic)
c
c     ensure that color range is between 0 and 255
c
      ic = min0(ic,255)
      ic = max0(ic,0)
      return
      end
c
c
c      
	subroutine getHexCode(nr, ng, nb, txt)
c
c	return hex code for color code n (0-255)
c
	character*1 b(16),txt*6
	data b /'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E',
     *			'F'/
c
c	from red to yellow
	nr1 = nr/16. + 1
	nr2 = mod(nr,16) + 1
	ng1 = ng/16. + 1
	ng2 = mod(ng,16) + 1
	nb1 = nb/16. + 1
	nb2 = mod(nb,16) + 1
	
c	note that order is changed from RGB to BGR
	write (txt,'(6a)') b(nb1),b(nb2),b(ng1),b(ng2),b(nr1),b(nr2)
	return
	end
c
c
c
!	subroutine child(n,title)
!c
!c	Open Child Window n
!c
!	use dflib
!	use dfwin
!c
!	type (windowconfig) wc
!	type (qwinfo) qwi
!	logical res
!	character*(*) title
!	common /windowCoordinate/ maxx,maxy,xl,xr,yb,yt
!	integer(2) maxx,maxy
!c
!c 	res = setwindowtext(gethwndqq(qwin$framewindow),' '//title)
!	open (n,file='user')
!	wc%numxpixels = maxx
!	wc%numypixels = maxy
!	wc%numtextcols = -1 
!	wc%numtextrows = -1 
!	wc%title = '   'C
!	wc%numcolors = -1 
!	wc%fontsize = qwin$extendfont
!	wc%extendfontname = "Fixedsys"C
!	wc%extendfontsize = #0008000f
!	wc%extendfontattributes = #80000000
! 	res = setwindowconfig(wc) 
! 	call setpalette
!  	res = setbkcolor(15)
!  	qwi.x = 0
!  	qwi.y = 0
!  	qwi.w = 1024
!  	qwi.h = 768
!  	qwi.type = qwin$set
!  	res = setwsizeqq(n,qwi)
!c      i = getwindowlong(gethwndqq(n),gwl_style)
!c      i = iand(i,not(ws_caption.or.ws_sysmenu))
!c      i = ior(iand(i,not(ws_thickframe)),ws_border)
!c      k = setwindowlong(gethwndqq(n),gwl_style,i)     
!c      i = movewindow(gethwndqq(n),-1,-1,0,0,.true.) 
!      call clearscreen($gclearscreen)
!c      res = updatewindow(gethwndqq(n))
!	call setwin(-.6667,.6667,-.5,.5)
!	res = settextcolor(0)
!	call setcolour(0)
!	res = initializefonts()
!	return
!	end
