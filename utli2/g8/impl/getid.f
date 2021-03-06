C... Program GETID.F 
C... to get ID for dams/sires.
C... ATH: Breyta ?arf s?u fyrir naut reglulega
C... n? er h?n t.d. <95000
c     parameter(noani = 460084)! conf.Rmain
c     parameter(noani = 472796)! conf.Rmain
!!    parameter(noani = 485026)! conf.Rmain
!     parameter(noani = 497767)! conf.Rmain
!     parameter(noani = 507345)! conf.Rmain
!     parameter(noani = 522378)! conf.Rmain
!     parameter(noani = 532617)! conf.Rmain okt2017
!     parameter(noani = 549098)! conf.Rmain mai2018
!     parameter(noani = 566476)! conf.Rmain jan2019
!     parameter(noani = 582817)! conf.Rmain okt2019
!     parameter(noani = 593041)! conf.Rmain jan2020
!     parameter(noani = 602987)! conf.Rmain jun2020
      parameter(noani = 617610)! conf.Rmain jan2021
      integer cp(23,noani),idam(noani),isire(noani),pl
      integer ndamX,nsireX,nidam1,nisire1,nidam0,nidam00
      integer nisire0,nisire00
      real*8 dam,sire,cpix(noani)
      logical binsok
      call nollmi(cp,23,noani)
      call nolli(idam,noani)
      call nolli(isire,noani)
      call nollr(cpix,noani)
      open(10,file='../conf.Rmain')
      open(20,file='data/ped.txt')
      ncp=noani
      ndamX=0
      nbullX=0
      nsireX=0
      nidam1=0
      nisire1=0
      nidam0=0
      nidam00=0
      nisire0=0
      nisire00=0
c.....Read files..........................
      print *,'Read unit - 10'
      do 199 i=1,ncp
	if(mod(i,1000).eq.0)Print *,'Read: ',i
	read(10,210)(cp(j,i),j=1,23)
	cpix(i)=cp(2,i)*100000000000.d0+
     +          cp(3,i)*10000.d0+cp(4,i)
  199 continue
  210 format(i6,3(i4,i7,i4),i4,i2,i3,i1,i2,i3,7i2)
  220 format(3i7)
  499 format('0Total number animal for which parent was searched: ',i7)
c 498 format('0Found ID (Ok) for: ', i7)
  498 format('0Found ID (Ok) for: ', i8)
  497 format('0Number of zeros from start: ', i7)
  496 format('0Garbage: ',i7)
  495 format('0Not zero but not found: ',i7)
  488 format('0Found ID (Ok) for: ', i7)
  487 format('0Number of zeros from start: ', i7)
  486 format('0Garbage: ',i7)
  485 format('0Not zero but not found: ',i7)
      do 198 i=1,ncp
      	if(mod(i,1000).eq.0)write(*,*)'Working with Animal...',i
c...saekja modr id
        if(cp(8,i).gt.0)then
	  dam=cp(8,i)*100000000000.d0+
     +        cp(9,i)*10000.d0+cp(10,i)
        if(binsok(cpix,ncp,dam,pl))then
          if(cp(1,pl).ge.cp(1,i))then
            print *,i,cp(1,i),cp(1,pl),'...modir'
            ndamX=ndamX+1
            idam(i)=0
          else
            idam(i)=cp(1,pl)
            ndam1=ndam1+1
          endif
        else
          idam(i)=0
          nidam0=nidam0+1
        endif
        else
          nidam00=nidam00+1
          idam(i)=0
        endif
c...saekja fodir id 
	if(cp(5,i).gt.0)then
	  sire=cp(5,i)*100000000000.d0+
     +        cp(6,i)*10000.d0+cp(7,i)
        if(binsok(cpix,ncp,sire,pl))then
          if(cp(1,pl).ge.cp(1,i))then
            print *,i,cp(1,i),cp(1,pl),'...fadir'
            nbullX=nbullX+1
            isire(i)=0
          else
            isire(i)=cp(1,pl)
            nbull1=nbull1+1
          endif
        else
          isire(i)=0
          nisire0=nisire0+1
        endif
        else
          nisire00=nisire00+1
          isire(i)=0
        endif
c...buin ad saekja fodur id
  198 continue

      print 499,ncp
      print 498,ndam1
      print 497,nidam00
      print 496,ndamX
      print 495,nidam0

      print 488,nbull1
      print 487,nisire00
      print 486,nbullX
      print 485,nisire0

      do 777 i=1,ncp
  777   write(20,220)cp(1,i),isire(i),idam(i)

  999 stop
      end


      logical function binsok(a,n,t,pl)
      integer n,pl
      integer min,max,mitt
      real*8 a(n),t
      min=1
      max=n
   10 if(min.lt.max)then
        mitt=(min+max)/2
        if(t.le.a(mitt))then
	  max=mitt
        else
	  min=mitt+1
        endif
        goto 10
      endif

      if(t.eq.a(min))then
	binsok=.true.
	pl=min
      else
	binsok=.false.
      endif

      return
      end
c     include '/home/agust/agusts/assub/nolli.f'
c     include '/home/agust/agusts/assub/nollmi.f'
c     include '/home/agust/agusts/assub/nollr.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nolli.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollmi.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr.f'
      include '/home/elsa/elsaagust/agusts/assub/nolli.f'
      include '/home/elsa/elsaagust/agusts/assub/nollmi.f'
      include '/home/elsa/elsaagust/agusts/assub/nollr.f'
