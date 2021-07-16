C...FYRIR BIL MILLI BURDA
c-----------------------------------------------------------------------
      program rescale 
c     Program to rescale BLUP solutions so that the average solution of 
c     all cows with observation becomes 100, and 1 standard deviation 
c     of EBV becomes equal to X units.
c     mars 1992; sept. 1996
c     agusts@centrum.is <Agust Sigurdsson>
c-----------------------------------------------------------------------
c     parameter(nogrp = 36)
c     parameter(nogrp = 38)
      parameter(nogrp = 40)
c     parameter(noani = 453604)
c     parameter(noani = 466316)
!     parameter(noani = 478547)
!     parameter(noani = 491303)
!     parameter(noani = 500881)
c     parameter(noani = 515915)
!     parameter(noani = 526154)
!     parameter(noani = 560120)
!     parameter(noani = 576365)
!     parameter(noani = 576353) !endurgert
!     parameter(noani = 586577) !endurgert
!     parameter(noani = 596523) !endurgert
      parameter(noani = 611146) !endurgert
      parameter(no = 90000)
      real*4 p1,p2,p3,rp,b0(3),var(6),imean,isd
      real*4 sp(3)
      real*4 dp1(no),dp2(no),dp3(no)
      real*4 dp(no),trt(3),miss
      integer*4 idist(10:190,4),ie,ih,hjord,fix(15),iobs(3),yrbase
      real*4 data(no),ave,adev,sdev,xar,skew,curt,xmin,xmax
!     character*1 stand,pth*29
!     character*1 stand,pth*26
      character*1 stand,pth*21
c...........NULLA
      do i=30,170
        do j=1,4
         idist(i,j)=0
        enddo
      enddo
      call nollr4(sp,3)
      call nollr4(b0,3)
      call nollr4(var,6)
      call nollr4(sp,3)
      call nollr4(dp1,no)
      call nollr4(dp2,no)
      call nollr4(dp3,no)
      call nollr4(dp,no)
      call nollr4(trt,3)
      call nolli(fix,15)
      call nolli(iobs,3)
!     pth='/home/agust/skuggi_kyr/mjolk/'
!     pth='/home/LBHI/elsa/Kyr/mjolk/'
      pth='/home/elsa/Kyr/mjolk/'
c...imean=INDEX mean; isd=INDEX SD
      imean=100.
      isd=10.
c     yrbase=2005 !fram till 2014
cgerdi samt samanburd 2015 med 2005 skolun 
c     yrbase=2010 ! fram til 2020
      yrbase=2015 ! fra 2020
c...missing trait def.
      miss=0.
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
      open(71,file=pth//'impl/data/birthy.txt',status='old')
      open(72,file=pth//'impl/data/sex.txt',status='old')
      open(73,file=pth//'impl/data/fixed.txt',status='old')
c     open(73,file=pth//'impl/data/trait.txt',status='old')
      open(42,file=pth//'frjos/vinnsla/rescaleB.sol',status='new')
      open(43,file=pth//'frjos/vinnsla/rescaleB.dis',status='new')
      open(66,file=pth//'frjos/vinnsla/rescaleB.log',status='new')
c ----+----+ ...zero distribution matrix +----+
      do 388 i=30,170
        do 388 j=1,4
  388     idist(i,j)=0
      do 788 i=1,3
        b0(i)=0.
  788 continue
      iobs(1)=0
      iobs(2)=0
      iobs(3)=0
c...first round through data file, to calculate means and SD
c...vegna gengr
      do i=1,nogrp
        read(53,791) p1,p2,p3
        read(73,*)hjord,fix
      enddo
      do 127 i=1,noani
        print *,i
        read(53,791) p1,p2,p3
        p1=-p1
        p2=-p2
        p3=-p3
        read(71,771)ibirth
        read(72,772)isex
        read(73,*)hjord,fix
c...use ped.txt as indicator instead of fixed.txt
c       read(73,*)trt
c...calculate mean and SD of cows with observation
c...ONLY FOR COWS BORN YRBASE
c       if(fix(1).gt.0)then
        if(fix(1).gt.0.and.ibirth.eq.yrbase)then
c       if(trt(1).ne.miss.and.ibirth.eq.90)then
          iobs(1)=iobs(1)+1
          dp1(iobs(1))=p1
          dp(iobs(1))=(p1+p2+p3)/3.
        endif
c       if(fix(2).gt.0)then
        if(fix(2).gt.0.and.ibirth.eq.yrbase)then
c       if(trt(2).ne.miss.and.ibirth.eq.90)then
          iobs(2)=iobs(2)+1
          dp2(iobs(2))=p2
        endif
c       if(fix(3).gt.0)then
        if(fix(3).gt.0.and.ibirth.eq.yrbase)then
c       if(trt(3).ne.miss.and.ibirth.eq.90)then
          iobs(3)=iobs(3)+1
          dp3(iobs(3))=p3
        endif
  127 continue
  771 format(i4)
  772 format(i1)
c...calculate mean and SD for all cows with observation
      write(66,'(a46,i5)')'Statistics for cows with observations and 
     +born',yrbase
      write(66,'(a46)')'............................................
     +.......'
      call moment(dp1,iobs(1),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p1 ',iobs(1),ave,sdev,skew,curt,xmin,xmax
      b0(1)=ave
      if(stand.eq.'b')sp(1)=sdev
      call moment(dp2,iobs(2),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p2 ',iobs(2),ave,sdev,skew,curt,xmin,xmax
      b0(2)=ave
      if(stand.eq.'b')sp(2)=sdev
      call moment(dp3,iobs(3),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p3 ',iobs(3),ave,sdev,skew,curt,xmin,xmax
      b0(3)=ave
      if(stand.eq.'b')sp(3)=sdev
      call moment(dp,iobs(1),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' Ip ',iobs(1),ave,sdev,skew,curt,xmin,xmax
  255 format(a4,i6,6f11.5)
c...done
c ----+----+ ...read solutions +----+----+----+
      print*,'kominn i 2 hring'
      rewind(53)
      rewind(73)
c...vegna gengr
      do i=1,nogrp
        read(53,791) p1,p2,p3
        read(73,*)hjord,fix
      enddo
      do 197 i=1,noani
        read(53,791) p1,p2,p3
        p1=-p1
        p2=-p2
        p3=-p3
        read(73,*)hjord,fix
c       read(73,*)trt
        ip1= int(((p1-b0(1))/sp(1)*isd)+imean)
        ip2= int(((p2-b0(2))/sp(2)*isd)+imean)
        ip3= int(((p3-b0(3))/sp(3)*isd)+imean)
c...Reikna heildareinkunn
      rp=(((p1-b0(1))/sp(1))+((p2-b0(2))/sp(2))+
     +   ((p3-b0(3))/sp(3)))/3.
      ih=int((rp)*isd+imean)
c      if(fix(1).gt.0)then
       if(fix(1).ne.miss)idist(ip1,1)=idist(ip1,1)+1
       if(fix(2).ne.miss)idist(ip2,2)=idist(ip2,2)+1
       if(fix(3).ne.miss)idist(ip3,3)=idist(ip3,3)+1
       if(fix(1).ne.miss.or.fix(2).ne.miss.or.fix(3).ne.miss)
     + idist(ih,4)=idist(ih,4)+1
        write(42,793)ip1,ip2,ip3,ih
      if(mod(i,1000).eq.0)print *,'Animal: ',i
  197 continue
      do 399 j=30,170
  399   write(43,794)j,(idist(j,i),i=1,4)
  791 format(3f14.6)
  792 format(3f14.6)
  793 format(4i4)
  794 format(i4,4i5)
c ----+-----+ ...done +----+----+----+----+----+----+
      print *,'            ...done'
      close(53)
      close(71)
      close(72)
      close(73)
      stop
      end
!     include '/home/agust/agusts/assub/momentII.f'
!     include '/home/agust/agusts/assub/nollr4.f'
!     include '/home/agust/agusts/assub/nolli.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/momentII.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr4.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nolli.f'
      include '/home/elsa/elsaagust/agusts/assub/momentII.f'
      include '/home/elsa/elsaagust/agusts/assub/nollr4.f'
      include '/home/elsa/elsaagust/agusts/assub/nolli.f'
