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
c     parameter(noani = 460443)
!     parameter(noani = 473396)
!!    parameter(noani = 485745)
!     parameter(noani = 498812)
!     parameter(noani = 508639)
!     parameter(noani = 528849)
!     parameter(noani = 539230) !okt2017
!     parameter(noani = 556303) !mai2018
!     parameter(noani = 573772) !jan2019
!     parameter(noani = 590776) !okt2019
!     parameter(noani = 601418) !jan2020
!     parameter(noani = 611767) !jun2020
      parameter(noani = 626832) !jan2021
      real*8 kleit,inaut
      integer*4 im(15),pl,acc(6),isol(17),
     + bd(5),rusl(22)
      character*8 c8
      character*1 p1
      character*12 c12
      logical binsok,binsoki
      rai=0.d0
      call nollr4(t,8)
      kleit=0.d0
      call nolli(im,9)
      pl=0
      call nolli(acc,6)
      call nolli(isol,8)
      call nolli(bd,3)
      call nolli(rusl,22)
      open(12,file='../conf.Rmain',status='old')
      open(20,file='../rescaled.sol',status='old')
      open(21,file='acc/accuracy.sol',status='old')
c     open(22,file='../kynbkySN.n14',status='new')
c     open(23,file='../nautskSN.n14',status='new')
!     open(22,file='../kynbkySN.m15',status='new')
!     open(23,file='../nautskSN.m15',status='new')
!!    open(22,file='../kynbkySN.n15',status='new')
!!    open(23,file='../nautskSN.n15',status='new')
!     open(22,file='../kynbkySN.m16',status='new')
!     open(23,file='../nautskSN.m16',status='new')
!     open(22,file='../kynbkySN.n16',status='new')
!     open(23,file='../nautskSN.n16',status='new')
!     open(22,file='../kynbkySN.m17',status='new')
!     open(23,file='../nautskSN.m17',status='new')
!     open(22,file='../kynbkySN.o17',status='new')
!     open(23,file='../nautskSN.o17',status='new')
!     open(22,file='../kynbkySN.m18',status='new')
!     open(23,file='../nautskSN.m18',status='new')
!     open(22,file='../kynbkySN.j19',status='new')
!     open(23,file='../nautskSN.j19',status='new')
!     open(22,file='../kynbkySN.o19',status='new')
!     open(23,file='../nautskSN.o19',status='new')
!     open(22,file='../kynbkySN.j20',status='new')
!     open(23,file='../nautskSN.j20',status='new')
!     open(22,file='../kynbkySN.jun20',status='new')
      open(22,file='../kynbkySN.jan21',status='new')
!     open(23,file='../nautskSN.jun20',status='new')
      open(23,file='../nautskSN.jan21',status='new')
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
c inaut=im(2)*100000000000.d0+im(3)*10000.d0+im(4)
	 inaut=im(2)*100000000000.d0+im(3)*10000.d0+im(4)
	 write(23,723)inaut,acc(4),acc(6),isol,rai
	else
c write(22,722)im(2),im(4),im(5),c8,im(6),c12,bd,t,isol
	endif
	if(mod(i,1000).eq.0)Print *,'Animal: ',i
  197 continue 
  793 format(17i4)
  722 format(i4,i6,i4,a8,i7,a12,3i3,8f6.0,8i4)
  723 format(f16.0,i7,i6,17i4,f5.2)
c 723 format(f9.0,i7,i6,17i4,f5.2)
c 721 format(i7,i4,i5,i7,2i6,f7.2)
!vikkadi plass fyrir fixed, komid yfir 10000
  721 format(i7,i5,i5,i7,2i6,f7.2)
  212 format(i6,3(i4,i7,i4),i2,i4,i3,i1,i2)
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
c     include '/home/agust/agusts/assub/nolli.f'
c     include '/home/agust/agusts/assub/nollr4.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nolli.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr4.f'
      include '/home/elsa/elsaagust/agusts/assub/nolli.f'
      include '/home/elsa/elsaagust/agusts/assub/nollr4.f'


