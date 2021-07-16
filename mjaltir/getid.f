C... Program GETID.F 
C... to get ID for dams/sires.
C... ATH: Breyta òarf síu fyrir naut reglulega
C... nú er hún t.d. <95000
c     parameter(noani = 453604)
!     parameter(noani = 466316)
!     parameter(noani = 478547)
!     parameter(noani = 491304)
!     parameter(noani = 500881)
!     parameter(noani = 515914)
!     parameter(noani = 526154)
!     parameter(noani = 542634)
!     parameter(noani = 560012)
!     parameter(noani = 576353)
!     parameter(noani = 586577)
!     parameter(noani = 596523)
      parameter(noani = 611148)
      integer cp(15,noani),idam(noani),isire(noani),pl
      integer ndamX,nsireX,nidam1,nisire1,nidam0,nidam00
      integer nisire0,nisire00
      real*8 dam,sire,cpix(noani)
      logical binsok
      call nollmi(cp,15,noani)
      call nolli(idam,noani)
      call nolli(isire,noani)
      call nollr(cpix,noani)
      open(10,file='vinnsla/mjalt.Rmain')
      open(20,file='vinnsla/ped.txt')
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
	read(10,210)(cp(j,i),j=1,15)
	cpix(i)=cp(2,i)*100000000000.d0+
     +          cp(3,i)*10000.d0+cp(4,i)
  199 continue
  210 format(i6,3(i4,i7,i4),i2,2i4,i5,i4)
  220 format(3i7)
  499 format('0Total number animal for which parent was searched: ',i7)
! 498 format('0Found ID (Ok) for: ', i7)
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
