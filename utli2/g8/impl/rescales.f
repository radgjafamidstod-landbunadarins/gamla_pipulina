C-----------------------------------------------------------------------
      program rescale 
c     Program to rescale BLUP solutions so that the average solution of 
c     all cows with observation becomes 100, and 1 standard deviation 
c     of EBV becomes equal to X units.
c     mars 1992; sept. 1996
c     agusts@centrum.is <Agust Sigurdsson>
c-----------------------------------------------------------------------
c     parameter(noani = 460084)
!     parameter(noani = 472796)
!!    parameter(noani = 485026)
!     parameter(noani = 497767)
!     parameter(noani = 507345)
!     parameter(noani = 522378)
!     parameter(noani = 532617) !okt2017
!     parameter(noani = 549098) !mai2018
!     parameter(noani = 566476) !jan2019
!     parameter(noani = 566476) !jan2019
!     parameter(noani = 582817) !okt2019
!     parameter(noani = 593041) !jan2020
!     parameter(noani = 602987) !jun2020
      parameter(noani = 617610) !jan2021
      parameter(no = 60000)
      real*4 p(8),rp,b0(8),var(153),imean,isd
      real*4 sp(8)
      real*4 dp1(no),dp2(no),dp3(no),dp4(no),dp5(no),dp6(no)
      real*4 dp7(no),dp8(no)
      real*4 trt(8),miss
      integer*4 idist(30:170,8),ie,ih,hjord,fix(3),iobs(8),yrbase
      real*4 data(no),ave,adev,sdev,xar,skew,curt,xmin,xmax
      character*1 stand
c...........NULLA
      do i=30,170
        do j=1,8
         idist(i,j)=0
        enddo
      enddo
      call nollr4(p,8)
      call nollr4(sp,8)
      call nollr4(b0,8)
      call nollr4(var,153)
      call nollr4(dp1,no)
      call nollr4(dp2,no)
      call nollr4(dp3,no)
      call nollr4(dp4,no)
      call nollr4(dp5,no)
      call nollr4(dp6,no)
      call nollr4(dp7,no)
      call nollr4(dp8,no)
      call nollr4(trt,8)
      call nollr4(data,no)
      call nolli(iobs,3)
      call nolli(fix,8)
c........................................
c...imean=INDEX mean; isd=INDEX SD
      imean=100.
      isd=10.
c     yrbase=2005
c     yrbase=2010
c     yrbase=2009
      yrbase=2008
c...missing trait def.
      miss=-999.
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
       sp(2)= sqrt(var(4))
       sp(3)= sqrt(var(6))
       close(13)
      endif
      open(53,file='keep/Rimpl41.txt',status='old')
      open(71,file='data/birthy.txt',status='old')
      open(72,file='data/sex.txt',status='old')
c     open(73,file='data/fixed.txt',status='old')
      open(73,file='data/trait.txt',status='old')
c     open(42,file='../rescaled.sol',status='new')
c     open(43,file='../rescaled.dis',status='new')
c     open(66,file='../rescaled.log',status='new')
      open(42,file='../rescaled.so_base2008',status='new')
      open(43,file='../rescaled.dis_base2008',status='new')
      open(66,file='../rescaled.log_base2008',status='new')
c ----+----+ ...zero distribution matrix +----+
      do 388 i=30,170
        do 388 j=1,8
  388     idist(i,j)=0
      do 788 i=1,8
        b0(i)=0.
  788 continue
      do jj=1,8
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
        read(53,'(8f14.6)')p
        read(71,771)ibirth
        read(72,772)isex
c       read(73,*)hjord,fix
c...use ped.txt as indicator instead of fixed.txt
        read(73,'(8f5.0)')trt
c...calculate mean and SD of cows with observation
c...ONLY FOR COWS BORN YRBASE
c       if(fix(1).gt.0.and.ibirth.eq.90)then
        if(trt(1).ne.miss.and.ibirth.eq.yrbase)then
c       if(trt(1).ne.miss)then
          iobs(1)=iobs(1)+1
          dp1(iobs(1))=p(1)
        endif
        if(trt(2).ne.miss.and.ibirth.eq.yrbase)then
          iobs(2)=iobs(2)+1
          dp2(iobs(2))=p(2)
        endif
        if(trt(3).ne.miss.and.ibirth.eq.yrbase)then
          iobs(3)=iobs(3)+1
          dp3(iobs(3))=p(3)
        endif
        if(trt(4).ne.miss.and.ibirth.eq.yrbase)then
          iobs(4)=iobs(4)+1
          dp4(iobs(4))=p(4)
        endif
        if(trt(5).ne.miss.and.ibirth.eq.yrbase)then
          iobs(5)=iobs(5)+1
          dp5(iobs(5))=p(5)
        endif
        if(trt(6).ne.miss.and.ibirth.eq.yrbase)then
          iobs(6)=iobs(6)+1
          dp6(iobs(6))=p(6)
        endif
        if(trt(7).ne.miss.and.ibirth.eq.yrbase)then
          iobs(7)=iobs(7)+1
          dp7(iobs(7))=p(7)
        endif
        if(trt(8).ne.miss.and.ibirth.eq.yrbase)then
          iobs(8)=iobs(8)+1
          dp8(iobs(8))=p(8)
        endif
  127 continue
  771 format(i4)
  772 format(i1)
c...calculate mean and SD for all cows with observation
      write(66,*)'Statistics for cows with observat. and born',yrbase
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
      call moment(dp3,iobs(3),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p3 ',iobs(3),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p3 ',iobs(3),ave,sdev,skew,curt,xmin,xmax
      b0(3)=ave
      if(stand.eq.'b')sp(3)=sdev
      call moment(dp4,iobs(4),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p4 ',iobs(4),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p4 ',iobs(4),ave,sdev,skew,curt,xmin,xmax
      b0(4)=ave
      if(stand.eq.'b')sp(4)=sdev
      call moment(dp5,iobs(5),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p5 ',iobs(5),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p5 ',iobs(5),ave,sdev,skew,curt,xmin,xmax
      b0(5)=ave
      if(stand.eq.'b')sp(5)=sdev
      call moment(dp6,iobs(6),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p6 ',iobs(6),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p6 ',iobs(6),ave,sdev,skew,curt,xmin,xmax
      b0(6)=ave
      if(stand.eq.'b')sp(6)=sdev
      call moment(dp7,iobs(7),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p7 ',iobs(7),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p7 ',iobs(7),ave,sdev,skew,curt,xmin,xmax
      b0(7)=ave
      if(stand.eq.'b')sp(7)=sdev
      call moment(dp8,iobs(8),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p8 ',iobs(8),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p8 ',iobs(8),ave,sdev,skew,curt,xmin,xmax
      b0(8)=ave
      if(stand.eq.'b')sp(8)=sdev
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
        read(53,'(8f14.6)')p
c       read(73,*)hjord,fix
        read(73,'(8f5.0)')trt
        ip1= int(((p(1)-b0(1))/sp(1)*isd)+imean)
        ip2= int(((p(2)-b0(2))/sp(2)*isd)+imean)
        ip3= int(((p(3)-b0(3))/sp(3)*isd)+imean)
        ip4= int(((p(4)-b0(4))/sp(4)*isd)+imean)
        ip5= int(((p(5)-b0(5))/sp(5)*isd)+imean)
        ip6= int(((p(6)-b0(6))/sp(6)*isd)+imean)
        ip7= int(((p(7)-b0(7))/sp(7)*isd)+imean)
        ip8= int(((p(8)-b0(8))/sp(8)*isd)+imean)
c...Reikna heildareinkunn
c      if(fix(1).gt.0)then
       if(trt(1).ne.miss)idist(ip1,1)=idist(ip1,1)+1
       if(trt(2).ne.miss)idist(ip2,2)=idist(ip2,2)+1
       if(trt(3).ne.miss)idist(ip3,3)=idist(ip3,3)+1
       if(trt(4).ne.miss)idist(ip4,4)=idist(ip4,4)+1
       if(trt(5).ne.miss)idist(ip5,5)=idist(ip5,5)+1
       if(trt(6).ne.miss)idist(ip6,6)=idist(ip6,6)+1
       if(trt(7).ne.miss)idist(ip7,7)=idist(ip7,7)+1
       if(trt(8).ne.miss)idist(ip8,8)=idist(ip8,8)+1
        write(42,793)ip1,ip2,ip3,ip4,ip5,ip6,ip7,ip8
      if(mod(i,1000).eq.0)print *,'Animal: ',i
  197 continue
      do 399 j=30,170
  399   write(43,794)j,(idist(j,i),i=1,8)
  791 format(8f14.6)
  792 format(8f14.6)
  793 format(8i4)
  794 format(i4,8i5)
c ----+-----+ ...done +----+----+----+----+----+----+
      print *,'            ...done'
      close(53)
      close(71)
      close(72)
      close(73)
      stop
      end
c     include '/home/agust/agusts/assub/momentII.f'
c     include '/home/agust/agusts/assub/nolli.f'
c     include '/home/agust/agusts/assub/nollr4.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/momentII.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nolli.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr4.f'
      include '/home/elsa/elsaagust/agusts/assub/momentII.f'
      include '/home/elsa/elsaagust/agusts/assub/nolli.f'
      include '/home/elsa/elsaagust/agusts/assub/nollr4.f'
