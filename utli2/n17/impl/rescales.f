c-----------------------------------------------------------------------
      program rescale 
c     Program to rescale BLUP solutions so that the average solution of 
c     all cows with observation becomes 100, and 1 standard deviation 
c     of EBV becomes equal to X units.
c     mars 1992; sept. 1996
c     agusts@centrum.is <Agust Sigurdsson>
c-----------------------------------------------------------------------
c     parameter(noani = 460443)
c     parameter(noani = 473396)
!!    parameter(noani = 485745)
!     parameter(noani = 498812)
!     parameter(noani = 508639)
!     parameter(noani = 528849)
!     parameter(noani = 539230) ! okt 2017
!     parameter(noani = 556303) ! mai 2018
!     parameter(noani = 573772) ! jan 2019
!     parameter(noani = 590776) ! okt 2019
!     parameter(noani = 601418) ! jan2 2020
!     parameter(noani = 611767) ! jun  2020
      parameter(noani = 626832) ! jan  2021
      parameter(no = 60000)
      real*4 p(17),rp,b0(17),var(153),imean,isd
      real*4 sp(17)
      real*4 dp1(no),dp2(no),dp3(no),dp4(no),dp5(no),dp6(no)
      real*4 dp7(no),dp8(no),dp9(no),dp10(no)
      real*4 dp11(no),dp12(no),dp13(no),dp14(no),dp15(no),dp16(no)
      real*4 dp17(no)
      real*4 trt(17),miss
      integer*4 idist(30:170,17),ie,ih,hjord,fix(3),iobs(17),yrbase
      real*4 data(no),ave,adev,sdev,xar,skew,curt,xmin,xmax
      character*1 stand
c...........NULLA
      do i=30,170
        do j=1,17
         idist(i,j)=0
        enddo
      enddo
      call nollr4(p,17)
      call nollr4(sp,17)
      call nollr4(b0,17)
      call nollr4(var,153)
      call nollr4(dp1,no)
      call nollr4(dp2,no)
      call nollr4(dp3,no)
      call nollr4(dp4,no)
      call nollr4(dp5,no)
      call nollr4(dp6,no)
      call nollr4(dp7,no)
      call nollr4(dp8,no)
      call nollr4(dp9,no)
      call nollr4(dp10,no)
      call nollr4(dp11,no)
      call nollr4(dp12,no)
      call nollr4(dp13,no)
      call nollr4(dp14,no)
      call nollr4(dp15,no)
      call nollr4(dp16,no)
      call nollr4(dp17,no)
      call nollr4(trt,17)
      call nollr4(data,no)
      call nolli(iobs,3)
      call nolli(fix,17)
c........................................
c...imean=INDEX mean; isd=INDEX SD
      imean=100.
      isd=10.
c     yrbase=2005
c     yrbase=2010
      yrbase=2015 ! mai2018
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
      open(42,file='../rescaled.sol',status='new')
      open(43,file='../rescaled.dis',status='new')
      open(66,file='../rescaled.log',status='new')
c ----+----+ ...zero distribution matrix +----+
      do 388 i=30,170
        do 388 j=1,17
  388     idist(i,j)=0
      do 788 i=1,17
        b0(i)=0.
  788 continue
      do jj=1,17
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
        read(53,'(17f14.6)')p
        read(71,771)ibirth
        read(72,772)isex
c       read(73,*)hjord,fix
c...use ped.txt as indicator instead of fixed.txt
        read(73,'(17f5.0)')trt
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
        if(trt(9).ne.miss.and.ibirth.eq.yrbase)then
          iobs(9)=iobs(9)+1
          dp9(iobs(9))=p(9)
        endif
        if(trt(10).ne.miss.and.ibirth.eq.yrbase)then
          iobs(10)=iobs(10)+1
          dp10(iobs(10))=p(10)
        endif
        if(trt(11).ne.miss.and.ibirth.eq.yrbase)then
          iobs(11)=iobs(11)+1
          dp11(iobs(11))=p(11)
        endif
        if(trt(12).ne.miss.and.ibirth.eq.yrbase)then
          iobs(12)=iobs(12)+1
          dp12(iobs(12))=p(12)
        endif
        if(trt(13).ne.miss.and.ibirth.eq.yrbase)then
          iobs(13)=iobs(13)+1
          dp13(iobs(13))=p(13)
        endif
        if(trt(14).ne.miss.and.ibirth.eq.yrbase)then
          iobs(14)=iobs(14)+1
          dp14(iobs(14))=p(14)
        endif
        if(trt(15).ne.miss.and.ibirth.eq.yrbase)then
          iobs(15)=iobs(15)+1
          dp15(iobs(15))=p(15)
        endif
        if(trt(16).ne.miss.and.ibirth.eq.yrbase)then
          iobs(16)=iobs(16)+1
          dp16(iobs(16))=p(16)
        endif
        if(trt(17).ne.miss.and.ibirth.eq.yrbase)then
          iobs(17)=iobs(17)+1
          dp17(iobs(17))=p(17)
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
      call moment(dp9,iobs(9),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p9 ',iobs(9),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p9 ',iobs(9),ave,sdev,skew,curt,xmin,xmax
      b0(9)=ave
      if(stand.eq.'b')sp(9)=sdev
      call moment(dp10,iobs(10),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p10 ',iobs(10),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p10 ',iobs(10),ave,sdev,skew,curt,xmin,xmax
      b0(10)=ave
      if(stand.eq.'b')sp(10)=sdev
      call moment(dp11,iobs(11),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p11 ',iobs(11),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p11 ',iobs(11),ave,sdev,skew,curt,xmin,xmax
      b0(11)=ave
      if(stand.eq.'b')sp(11)=sdev
      call moment(dp12,iobs(12),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p12 ',iobs(12),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p12 ',iobs(12),ave,sdev,skew,curt,xmin,xmax
      b0(12)=ave
      if(stand.eq.'b')sp(12)=sdev
      call moment(dp13,iobs(13),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p13 ',iobs(13),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p13 ',iobs(13),ave,sdev,skew,curt,xmin,xmax
      b0(13)=ave
      if(stand.eq.'b')sp(13)=sdev
      call moment(dp14,iobs(14),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p14 ',iobs(14),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p14 ',iobs(14),ave,sdev,skew,curt,xmin,xmax
      b0(14)=ave
      if(stand.eq.'b')sp(14)=sdev
      call moment(dp15,iobs(15),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p15 ',iobs(15),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p15 ',iobs(15),ave,sdev,skew,curt,xmin,xmax
      b0(15)=ave
      if(stand.eq.'b')sp(15)=sdev
      call moment(dp16,iobs(16),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p16 ',iobs(16),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p16 ',iobs(16),ave,sdev,skew,curt,xmin,xmax
      b0(16)=ave
      if(stand.eq.'b')sp(16)=sdev
      call moment(dp17,iobs(17),ave,adev,sdev,xar,skew,curt,xmin,xmax)
      write(66,255)' p17 ',iobs(17),ave,sdev,skew,curt,xmin,xmax
      write(*,255)' p17 ',iobs(17),ave,sdev,skew,curt,xmin,xmax
      b0(17)=ave
      if(stand.eq.'b')sp(17)=sdev
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
        read(53,'(17f14.6)')p
c       read(73,*)hjord,fix
        read(73,'(17f5.0)')trt
        ip1= int(((p(1)-b0(1))/sp(1)*isd)+imean)
        ip2= int(((p(2)-b0(2))/sp(2)*isd)+imean)
        ip3= int(((p(3)-b0(3))/sp(3)*isd)+imean)
        ip4= int(((p(4)-b0(4))/sp(4)*isd)+imean)
        ip5= int(((p(5)-b0(5))/sp(5)*isd)+imean)
        ip6= int(((p(6)-b0(6))/sp(6)*isd)+imean)
        ip7= int(((p(7)-b0(7))/sp(7)*isd)+imean)
        ip8= int(((p(8)-b0(8))/sp(8)*isd)+imean)
        ip9= int(((p(9)-b0(9))/sp(9)*isd)+imean)
        ip10= int(((p(10)-b0(10))/sp(10)*isd)+imean)
        ip11= int(((p(11)-b0(11))/sp(11)*isd)+imean)
        ip12= int(((p(12)-b0(12))/sp(12)*isd)+imean)
        ip13= int(((p(13)-b0(13))/sp(13)*isd)+imean)
        ip14= int(((p(14)-b0(14))/sp(14)*isd)+imean)
        ip15= int(((p(15)-b0(15))/sp(15)*isd)+imean)
        ip16= int(((p(16)-b0(16))/sp(16)*isd)+imean)
        ip17= int(((p(17)-b0(17))/sp(17)*isd)+imean)
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
       if(trt(9).ne.miss)idist(ip9,9)=idist(ip9,9)+1
       if(trt(10).ne.miss)idist(ip10,10)=idist(ip10,10)+1
       if(trt(11).ne.miss)idist(ip11,11)=idist(ip11,11)+1
       if(trt(12).ne.miss)idist(ip12,12)=idist(ip12,12)+1
       if(trt(13).ne.miss)idist(ip13,13)=idist(ip13,13)+1
       if(trt(14).ne.miss)idist(ip14,14)=idist(ip14,14)+1
       if(trt(15).ne.miss)idist(ip15,15)=idist(ip15,15)+1
       if(trt(16).ne.miss)idist(ip16,16)=idist(ip16,16)+1
       if(trt(17).ne.miss)idist(ip17,17)=idist(ip17,17)+1
        write(42,793)ip1,ip2,ip3,ip4,ip5,ip6,ip7,ip8,ip9,ip10
     +,ip11,ip12,ip13,ip14,ip15,ip16,ip17
      if(mod(i,1000).eq.0)print *,'Animal: ',i
  197 continue
      do 399 j=30,170
  399   write(43,794)j,(idist(j,i),i=1,17)
  791 format(17f14.6)
  792 format(17f14.6)
  793 format(17i4)
  794 format(i4,17i5)
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
