c-----------------------------------------------------------------------
      program rescale 
c     Program to rescale BLUP solutions so that the average solution of 
c     all cows with observation becomes 100, and 1 standard deviation 
c     of EBV becomes equal to X units.
c     mars 1992; sept. 1996
c     agusts@centrum.is <Agust Sigurdsson>
c-----------------------------------------------------------------------
c     parameter(noani = 453604)
!     parameter(noani = 466316)
!!    parameter(noani = 478547)
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
      parameter(no = 90000)
      real*4 p(2),rp,b0(2),var(3),imean,isd
      real*4 sp(2)
      real*4 dp1(no),dp2(no),dp3(no),dp4(no),dp5(no),dp6(no)
      real*4 dp7(no),dp8(no)
      real*4 trt(2),miss
      integer*4 idist(30:170,2),ie,ih,hjord,fix(1),iobs(2),yrbase
      real*4 data(no),ave,adev,sdev,xar,skew,curt,xmin,xmax
      character*1 stand
c...imean=INDEX mean; isd=INDEX SD
      imean=100.
      isd=10.
c     yrbase=2005
!     yrbase=2010
      yrbase=2015 !arid 2018
c...missing trait def.
      miss=-9.
c...two options for standardization
c     	a: use genetic SD (Sigma(G))
c     	b: use SD of base group
      stand='b'
      if(stand.eq.'a')then
c     goto 123
       open(13,file='data/var.txt',status='old')
c...new sigma(g) for standardization
       read(13,*)var
       sp(1)= sqrt(var(1))
       sp(2)= sqrt(var(3))
       close(13)
      endif
      open(53,file='keep/Rimpl41.txt',status='old')
      open(71,file='data/birthy.txt',status='old')
      open(72,file='data/sex.txt',status='old')
c     open(73,file='data/fixed.txt',status='old')
      open(73,file='data/trait.txt',status='old')
      open(42,file='rescaled.sol',status='new')
      open(43,file='rescaled.dis',status='new')
      open(66,file='rescaled.log',status='new')
c ----+----+ ...zero distribution matrix +----+
      do 388 i=30,170
        do 388 j=1,2
  388     idist(i,j)=0
      do 788 i=1,2
        b0(i)=0.
  788 continue
      do jj=1,2
        iobs(jj)=0
      enddo
c...first round through data file, to calculate means and SD
c...vegna gengr
c     do i=1,19
c       read(53,791) p1,p2,p3
c       read(73,*)hjord,fix
c     enddo
      write(*,*)'Renni i gegnum fyrri umferd'
      write(*,*)'...........................'
      do 127 i=1,noani
        if(mod(i,1000).eq.0)print *,'...gripur ',i
        read(53,791) p
        p(1)=-p(1)
        p(2)=-p(2)
        read(71,771)ibirth
        read(72,772)isex
c       read(73,*)hjord,fix
c...use ped.txt as indicator instead of fixed.txt
        read(73,'(2f5.0)')trt
c...calculate mean and SD of cows with observation
c...ONLY FOR COWS BORN 1990
        if(trt(1).ne.miss.and.ibirth.eq.yrbase)then
c       if(trt(1).ne.miss)then
          iobs(1)=iobs(1)+1
          dp1(iobs(1))=p(1)
        endif
        if(trt(2).ne.miss.and.ibirth.eq.yrbase)then
c       if(trt(2).ne.miss)then
          iobs(2)=iobs(2)+1
          dp2(iobs(2))=p(2)
        endif
  127 continue
  771 format(i4)
  772 format(i1)
c...calculate mean and SD for all cows with observation
      write(66,*)'Statistics for cows with observat. and born ',yrbase
      write(66,*)'...................................................'
      write(*,*)'Reikna medaltol o.p.h'
      write(*,*)'.....................'
      call moment(dp1,iobs(1),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p1 ',iobs(1),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p1 ',iobs(1),ave,sdev,skew,curt,xmin,xmax
      b0(1)=ave
      if(stand.eq.'b')sp(1)=sdev
      call moment(dp2,iobs(2),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p2 ',iobs(2),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p2 ',iobs(2),ave,sdev,skew,curt,xmin,xmax
      b0(2)=ave
      if(stand.eq.'b')sp(2)=sdev
  255 format(a4,i6,6f11.5)
c...done
c ----+----+ ...read solutions +----+----+----+
      rewind(53)
      rewind(73)
c...vegna gengr
c     do i=1,19
c       read(53,791) p1,p2,p3
c       read(73,*)hjord,fix
c     enddo
      do 197 i=1,noani
        read(53,791) p
        p(1)=-p(1)
        p(2)=-p(2)
c       read(73,*)hjord,fix
        read(73,'(2f5.0)')trt
        ip1= int(((p(1)-b0(1))/sp(1)*isd)+imean)
        ip2= int(((p(2)-b0(2))/sp(2)*isd)+imean)
c...Reikna heildareinkunn
c      if(fix(1).gt.0)then
       if(trt(1).ne.miss)idist(ip1,1)=idist(ip1,1)+1
       if(trt(2).ne.miss)idist(ip2,2)=idist(ip2,2)+1
        write(42,793)ip1,ip2
      if(mod(i,1000).eq.0)print *,'Animal: ',i
  197 continue
      do 399 j=30,170
  399   write(43,794)j,(idist(j,i),i=1,2)
  791 format(2f14.6)
  792 format(2f14.6)
  793 format(2i4)
  794 format(i4,2i5)
c ----+-----+ ...done +----+----+----+----+----+----+
      print *,'            ...done'
      close(53)
      close(71)
      close(72)
      close(73)
      stop
      end
c     include '/home/agust/agusts/assub/momentII.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/momentII.f'
      include '/home/elsa/elsaagust/agusts/assub/momentII.f'
