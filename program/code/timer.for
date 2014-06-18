	subroutine timer(k,ih,im,is,ihs)
c
c	takes the time  (k negative: start k-th time, k positive: show k-th time)
c
	use ifport
	integer*2 jh,jm,js,jhs
	common /timerres/ it(50),jt(50)
	if (k.lt.0) then
	 call gettim(jh,jm,js,jhs)
	 jt(-k) = jh*360000+jm*6000+js*100+jhs
	elseif (k.gt.0) then
	 call gettim(jh,jm,js,jhs)
	 it(k) = jh*360000+jm*6000+js*100+jhs
	 it(k) = it(k)-jt(k)
	 if (it(k).lt.0) it(k) = 0     ! when date chages it becomes negative
	 ih = it(k)/360000
       im = mod(it(k),360000)/6000
       is = mod(it(k),6000)/100
       ihs = mod(it(k),100)
	endif
	return
	end
