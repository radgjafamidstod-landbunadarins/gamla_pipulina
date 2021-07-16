C... Program GETID.F 
C... to get ID for dams/sires.
C     parameter(noani = 466316)
!     parameter(noani = 478547)!aetternin15=Rmain
!     parameter(noani = 491303)!aetternim16=Rmain
!     parameter(noani = 500881)!aetternin16=Rmain
!     parameter(noani = 515916)!aetternin17=Rmain
!     parameter(noani = 515915)!aetternin17=Rmain
!     parameter(noani = 526154)!aetternin17=Rmain
!     parameter(noani = 542633)!aetternin17=Rmain
!     parameter(noani = 560488)!aetternin19=Rmain
!     parameter(noani = 560120)!aetternin19=Rmain !endurgert21/01
!     parameter(noani = 576353)!aetternin19=Rmain okt
!     parameter(noani = 586577)!aetternin19=Rmain jan20
!     parameter(noani = 596523)!aetternin19=Rmain jun20
      parameter(noani = 611146)!aetternin19=Rmain jan21
      integer cp2(noani),cp(10,noani),idam(noani),isire(noani),pl
      real*8 dam,sire,cpix(noani)
      logical binsok,nytt
c     character pth*29
!     character pth*26
      character pth*21
      nytt=.true.
c     pth='/home/agust/skuggi_kyr/mjolk/'
!     pth='/home/LBHI/elsa/Kyr/mjolk/'
      pth='/home/elsa/Kyr/mjolk/'
      nidamX=0
      nisireX=0
      ncp=0
      ndam1=0
      nidam00=0
      nidam0=0
      nisire00=0
      nisire0=0
      nbull1=0
      if(nytt)then
      open(10,file=pth//'vinnsla/Rmain')
      open(20,file=pth//'vinnsla/ped.txt')
      else
      open(10,file='framl/Rmain')
      open(20,file='framl/ped.txt')
      endif
      ncp=noani
c.....Read files..........................
      print *,'Read unit - 10'
      do 199 i=1,ncp
	if(mod(i,1000).eq.0)Print *,'Read: ',i
	read(10,210)(cp(j,i),j=1,10)
	cpix(i)=cp(2,i)*100000000000.d0+cp(3,i)*10000.d0+cp(4,i)
  199 continue
  210 format(i6,3(i4,i7,i4))
  220 format(3i7)
  499 format('0Total number animal for which parent was searched: ',i7)
  498 format('0Found ID (Ok) for: ', i7)
  497 format('0Number of zeros from start: ', i7)
  496 format('0Garbage: ',i7)
  495 format('0Not zero but not found: ',i7)
  488 format('0Found ID (Ok) for: ', i7)
  487 format('0Number of zeros from start: ', i7)
  486 format('0Garbage: ',i7)
  485 format('0Not zero but not found: ',i7)
      do 198 i=1,ncp
c     	if(mod(i,1000).eq.0)write(*,*)'Working with Animal...',i
c...saekja ID modur
        if(cp(8,i).gt.0)then
	  dam=cp(8,i)*100000000000.d0+cp(9,i)*10000.d0+cp(10,i)
        if(binsok(cpix,ncp,dam,pl))then
c         write(*,'(a20,f16.0)')'Modir finnst undir:',cpix(pl)
          if(cp(1,pl).ge.cp(1,i))then
            ndamX=ndamX+1
            idam(i)=0
          write(40,'(a20,i6,i5,i7,i4,i5,i7,i4,i5,i7,i4)')
     +    'Modir skrad rugl:',(cp(jj,i),jj=1,10)
          else
            idam(i)=cp(1,pl)
            ndam1=ndam1+1
c         write(*,*)'Modir finnst og er skrad:',idam(i)
          endif
        else
          idam(i)=0
          nidam0=nidam0+1
          write(41,'(a34,f16.0)')'Modir skrad en fnnst ekki:',dam
        endif
        else
          nidam00=nidam00+1
          idam(i)=0
c         write(*,*)'Modir er skrad 0',(cp(j,i),j=8,10)
        endif
c...saekja ID fodur
	if(cp(5,i).gt.0)then
	  sire=cp(5,i)*100000000000.d0+cp(6,i)*10000.d0+cp(7,i)
c         write(*,'(f16.0)')sire
        if(binsok(cpix,ncp,sire,pl))then
c         write(*,'(a20,f16.0)')'Fadir finnst undir:',cpix(pl)
c     read(*,*)
          if(cp(1,pl).ge.cp(1,i))then
            nbullX=nbullX+1
            isire(i)=0
          write(40,'(a20,i6,i5,i7,i4,i5,i7,i4,i5,i7,i4)')
     +    'Fadir skradur rugl:',(cp(jj,i),jj=1,10)
          else
            isire(i)=cp(1,pl)
            nbull1=nbull1+1
c         write(*,*)'Fadir finnst og er skradur:',isire(i)
          endif
        else
          write(41,'(a34,f16.0)')'Fadir skradur en finnst ekki:',sire
          isire(i)=0
          nisire0=nisire0+1
        endif
        else
c         write(*,*)'Fadir er skradur 0',(cp(j,i),j=5,7)
          nisire00=nisire00+1
          isire(i)=0
        endif
c... end saekja FODUR
  198 continue

      print 499,ncp
      print 498,ndam1
      print 497,nidam00
      print 496,nidamX
      print 495,nidam0

      print 488,nbull1
      print 487,nisire00
      print 486,nisireX
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

