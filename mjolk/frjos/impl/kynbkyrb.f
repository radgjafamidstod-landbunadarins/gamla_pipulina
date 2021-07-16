c-----------------------------------------------------------------------
c     program kynbkyr 
c.......................................................................
c     Description: Gathers information on the evaluations and accuracy
c                  for each animal and builds the files KYNBKYR.SOL
c                  and NAUTSKRA.SOL
c.......................................................................
c     Written mars 1993; okt 1997
c     Agust Sigurdsson, Department of Animal Breeding and genetics. 
c     Swedish University of Agr. Sci., Uppsala.
c-----------------------------------------------------------------------
c     parameter(noani = 453640-36)
!     parameter(noani = 466352-36)
!!    parameter(noani = 478583-36)
!     parameter(noani = 491339-36)
!     parameter(noani = 500917-36)
!     parameter(noani = 515951-36)
!     parameter(noani = 526192-38)
!     parameter(noani = 560158-38)
!     parameter(noani = 576391-38) 
!     parameter(noani = 586615-38) 
!     parameter(noani = 596563-40) 
      parameter(noani = 611186-40) 
      real rai,t(3)
      real*8 kleit,inaut
      integer*4 im(40),pl,acc(6),isol(4),
     + ink(3),bd(6),rusl(22)
      character*8 c8
      character*1 p1
!     character*12 c12,pth*29
!!    character*12 c12,pth*26
      character*12 c12,pth*21
      logical binsok,binsoki
!     pth='/home/agust/skuggi_kyr/mjolk/'
!!    pth='/home/LBHI/elsa/Kyr/mjolk/'
      pth='/home/elsa/Kyr/mjolk/'
      open(12,file=pth//'vinnsla/Rmain',status='old')
      open(20,file=pth//'frjos/vinnsla/rescaleB.sol',status='old')
      open(21,file=pth//'impl/acc/accuracy.sol',status='old')
c     open(22,file=pth//'frjos/vinnsla/kynbkyrB.n14')
c     open(23,file=pth//'frjos/vinnsla/nautskrB.n14')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.m15')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.m15')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.m16')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.m16')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.n16')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.n16')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.m17')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.m17')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.o17')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.m18')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.j19')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.o19')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.j20')
!     open(22,file=pth//'frjos/vinnsla/kynbkyrB.jun20')
      open(22,file=pth//'frjos/vinnsla/kynbkyrB.jan21')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.o17')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.m18')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.j19')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.o19')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.j20')
!     open(23,file=pth//'frjos/vinnsla/nautskrB.jun20')
      open(23,file=pth//'frjos/vinnsla/nautskrB.jan21')
c ----+----+ ...read solutions +----+----+----+
       p1=' '
       c8='        '
       c12='            '
      do 197 i=1,noani
	read(12,212)im
        t(1)=rusl(20)
        t(2)=rusl(21)
        t(3)=rusl(22)
	read(20,793)isol
	read(21,721)acc,rai
c...Ef um naut er ad raeda.....
	if(im(11).eq.1)then
	 inaut=(im(2))*100000000000.d0+im(3)*10000.d0+im(4)
	 write(23,723)inaut,acc(4),acc(6),isol,rai
	else
         write(22,722)im(2),im(4),im(5),c8,im(6),c12,bd,t,isol(4)
	endif
	if(mod(i,1000).eq.0)Print *,'Animal: ',i
  197 continue 
  793 format(4i4)
  722 format(i4,i6,i4,a8,i7,a12,3(i4,i2),3f6.2,i5)
  723 format(f16.0,i7,i6,4i4,f5.2)
  721 format(i7,i4,i5,i7,2i6,f7.2)
  212 format(i6,3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
c 212 format(i5,i2,i1,i6,i5,i5,i2,i3,i6,i2,6i2,3i1,3i4,3i2,i4
c    + ,12i2,3f5.0)
  214 format(i6,i2,i3,a8)

c ----+-----+ ...done +----+----+----+----+----+----+
      print *,'            ...done'
      stop
      end

      logical function binsok(a,n,t,pl)
      integer n,pl,min,max,mitt
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


      logical function binsoki(a,n,t,pl)
      integer n,pl,min,max,mitt,a(n),t
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
	binsoki=.true.
	pl=min
      else
	binsoki=.false.
      endif

      return
      end
