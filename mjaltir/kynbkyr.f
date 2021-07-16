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
c     parameter(noani = 453604)
!     parameter(noani = 466316)
!!    parameter(noani = 478547)
!     parameter(noani = 491304)
!     parameter(noani = 500881)
!     parameter(noani = 515914)
!     parameter(noani = 526154)
!     parameter(noani = 542634)
!     parameter(noani = 576353)
!     parameter(noani = 586577)
!     parameter(noani = 596523)
      parameter(noani = 611148)
      real rai,t(2)
      real*8 kleit,inaut
      integer*4 im(15),pl,acc(6),isol(2),
     + bd(2),rusl(22)
      character*8 c8
      character*1 p1
      character*12 c12
      logical binsok,binsoki
      open(12,file='vinnsla/mjalt.Rmain',status='old')
      open(20,file='rescaled.sol',status='old')
      open(21,file='acc/accuracy.sol',status='old')
c     open(22,file='vinnsla/kynbkyMG.j10',status='new')
!     open(22,file='vinnsla/kynbkyMG.m18',status='new')
!     open(22,file='vinnsla/kynbkyMG.j19',status='new')
!     open(22,file='vinnsla/kynbkyMG.o19',status='new')
!     open(22,file='vinnsla/kynbkyMG.j20',status='new')
!     open(22,file='vinnsla/kynbkyMG.jun20',status='new')
      open(22,file='vinnsla/kynbkyMG.jan21',status='new')
c     open(23,file='vinnsla/nautskMG.n14')
c     open(23,file='vinnsla/nautskMG.m15')
!     open(23,file='vinnsla/nautskMG.n15')
!     open(23,file='vinnsla/nautskMG.m16')
!     open(23,file='vinnsla/nautskMG.n16')
!     open(23,file='vinnsla/nautskMG.m17')
!     open(23,file='vinnsla/nautskMG.o17')
!     open(23,file='vinnsla/nautskMG.m18')
!     open(23,file='vinnsla/nautskMG.j19')
!     open(23,file='vinnsla/nautskMG.o19')
!     open(23,file='vinnsla/nautskMG.j20')
!     open(23,file='vinnsla/nautskMG.jun20')
      open(23,file='vinnsla/nautskMG.jan21')
c ----+----+ ...read solutions +----+----+----+
      p1=' '
      c8='        '
      c12='            '
      do 197 i=1,noani
	read(12,212)im
	read(20,793)isol
	read(21,721)acc,rai
c...Ef um naut er ad raeda.....
	if(im(11).eq.1)then
	 inaut=im(2)*100000000000.d0+im(3)*10000.d0+im(4)
         write(23,723)inaut,acc(4),acc(6),isol,rai
	else
cwrite(22,722)im(2),im(3),im(4),c8,im(6),c12,im(14),im(15),isol
	endif
	if(mod(i,1000).eq.0)Print *,'Animal: ',i
  197 continue 
  793 format(2i4)
  722 format(i4,i6,i4,a8,i7,a12,i3,2f6.0,2i4)
  723 format(f16.0,i7,i6,2i4,f5.2)
  721 format(i7,i4,i5,i7,2i6,f7.2)
  212 format(i6,3(i4,i7,i4),i2,2i4,i5,i4)

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

