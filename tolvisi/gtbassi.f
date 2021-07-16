c...Reiknar tolfradi a Kynbotaeinkunnir 1..15
      program gtrend
      real*4 x1(2014),x2(2014),x3(2014),m1,m2,m3,tr(3)
      real*4 x4(2014),x5(2014),x6(2014)
      real*4 x7(2014),x8(2014),x9(2014)
      real*4 x10(2014),x11(2014),x12(2014)
      real*4 x13(2014),x14(2014),x15(2014)
      real*4 sig(99),a,b,siga,sigb,gt(15),m4,m5,m6,m7,m8,m9,m10,m11,
     + m12,m13,m14,m15,
     + y(2014),chi2,q,r,zg,prob,w1(2014),w2(2014)
      integer np,n1(2014),n2(2014),n3(2014)
      integer n4(2014),n5(2014),n6(2014)
      integer n7(2014),n8(2014),n9(2014)
      integer n10(2014),n11(2014),n12(2014)
      integer n13(2014),n14(2014),n15(2014)
      character*26 tmp,skra(5)*1
c     character tmp2*7,pth*23
      character tmp2*7,pth*20
      character*10 mlina*74,dlina*74,clina*74
      dlina='===========================================================  
     +==============='
      clina='-----------------------------------------------------------  
     +---------------'
      skra(1)='1'
      skra(2)='2'
      skra(3)='3'
      skra(4)='4'
      skra(5)='5'
c     pth='/home/agust/skuggi_kyr/'
      pth='/home/LBHI/elsa/Kyr/'
c     open(11,file=pth//'mjolk/impl/data/trait1.txt')
c     open(13,file=pth//'utlit/n17/conf.Rmain')
c  -------------------------------------------------------------------
      ndata=442421
      do k=1,1
c     open(10,file=pth//'index/lausnir/jun14/bassi.j14')
      open(10,file=pth//'index/lausnir/bassi.n14')
      open(20,file='gtbassi.n14')
c.....Fer i gegnum alla eiginleikaflokka 1..5
      do j=1970,2014
        n1(j)=0
        n2(j)=0
        n3(j)=0
        n4(j)=0
        n5(j)=0
        n6(j)=0
        n7(j)=0
        n8(j)=0
        n9(j)=0
        n10(j)=0
        n11(j)=0
        n12(j)=0
        n13(j)=0
        n14(j)=0
        n15(j)=0
      enddo
      do i=1,ndata
        read(10,110)np,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,
     +m12,m13,m14,m15
c       write(*,111)np,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,
c    +m12,m13,m14,m15
c      if(i.gt.100)stop
c    +m12,m13,m14,m15,m16,m17
c       read(11,*)tr
c       read(13,'(6x,i4,7x,i4,30x,i1)')np,inum,isex
c       if(isex.eq.1.and.inum.lt.50)then
          x1(np)=x1(np)+m1
          n1(np)=n1(np)+1
          x2(np)=x2(np)+m2
          n2(np)=n2(np)+1
          x3(np)=x3(np)+m3
          n3(np)=n3(np)+1
          x4(np)=x4(np)+m4
          n4(np)=n4(np)+1
          x5(np)=x5(np)+m5
          n5(np)=n5(np)+1
          x6(np)=x6(np)+m6
          n6(np)=n6(np)+1
          x7(np)=x7(np)+m7
          n7(np)=n7(np)+1
          x8(np)=x8(np)+m8
          n8(np)=n8(np)+1
          x9(np)=x9(np)+m9
          n9(np)=n9(np)+1
          x10(np)=x10(np)+m10
          n10(np)=n10(np)+1
          x11(np)=x11(np)+m11
          n11(np)=n11(np)+1
          x12(np)=x12(np)+m12
          n12(np)=n12(np)+1
          x13(np)=x13(np)+m13
          n13(np)=n13(np)+1
          x14(np)=x14(np)+m14
          n14(np)=n14(np)+1
          x15(np)=x15(np)+m15
          n15(np)=n15(np)+1
c       endif
      enddo
      np=0
      do 117 i=1970,2014
        x1(i)=x1(i)/n1(i)
        x2(i)=x2(i)/n2(i)
        x3(i)=x3(i)/n3(i)
        x4(i)=x4(i)/n4(i)
        x5(i)=x5(i)/n5(i)
        x6(i)=x6(i)/n6(i)
        x7(i)=x7(i)/n7(i)
        x8(i)=x8(i)/n8(i)
        x9(i)=x9(i)/n9(i)
        x10(i)=x10(i)/n10(i)
        x11(i)=x11(i)/n11(i)
        x12(i)=x12(i)/n12(i)
        x13(i)=x13(i)/n13(i)
        x14(i)=x14(i)/n14(i)
        x15(i)=x15(i)/n15(i)
        write(20,125)i,n1(i),x1(i),n2(i),x2(i),n3(i),x3(i)
     +  ,n4(i),x4(i),n5(i),x5(i)
     +  ,n6(i),x6(i),n7(i),x7(i),n8(i),x8(i),n9(i),x9(i)
     +  ,n10(i),x10(i),n11(i),x11(i),n12(i),x12(i)
     +  ,n13(i),x13(i),n14(i),x14(i),n15(i),x15(i)
        if(i.ge.1980)then
          np=np+1
          x1(np)=x1(i)
          x2(np)=x2(i)
          x3(np)=x3(i)
          x4(np)=x4(i)
          x5(np)=x5(i)
          x6(np)=x6(i)
          x7(np)=x7(i)
          x8(np)=x8(i)
          x9(np)=x9(i)
          x10(np)=x10(i)
          x11(np)=x11(i)
          x12(np)=x12(i)
          x13(np)=x13(i)
          x14(np)=x14(i)
          x15(np)=x15(i)
          y(np)=i
        endif
  117 continue
       mwt=0
       print *,clina
       print *,'Fit for mean BV:'
       print *,clina
       call fit(y,x1,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(1)=b
       write(*,205) '  Trait 1:',np,a,siga,b,sigb
       call fit(y,x2,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(2)=b
       write(*,205) '  Trait 2:',np,a,siga,b,sigb
       call fit(y,x3,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(3)=b
       write(*,205) '  Trait 3:',np,a,siga,b,sigb
       call fit(y,x4,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(4)=b
       write(*,205) '  Trait 4:',np,a,siga,b,sigb
       call fit(y,x5,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(5)=b
       write(*,205) '  Trait 5:',np,a,siga,b,sigb
       call fit(y,x6,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(6)=b
       write(*,205) '  Trait 6:',np,a,siga,b,sigb
       call fit(y,x7,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(7)=b
       write(*,205) '  Trait 7:',np,a,siga,b,sigb
       call fit(y,x8,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(8)=b
       write(*,205) '  Trait 8:',np,a,siga,b,sigb
       call fit(y,x9,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(9)=b
       write(*,205) '  Trait 9:',np,a,siga,b,sigb
       call fit(y,x10,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(10)=b
       write(*,205) '  Trait 10:',np,a,siga,b,sigb
       call fit(y,x11,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(11)=b
       write(*,205) '  Trait 11:',np,a,siga,b,sigb
       call fit(y,x12,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(12)=b
       write(*,205) '  Trait 12:',np,a,siga,b,sigb
       call fit(y,x13,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(13)=b
       write(*,205) '  Trait 13:',np,a,siga,b,sigb
       call fit(y,x14,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(14)=b
       write(*,205) '  Trait 14:',np,a,siga,b,sigb
       call fit(y,x15,np,sig,mwt,a,b,siga,sigb,chi2,q)
       gt(15)=b
       write(*,205) '  Trait 15:',np,a,siga,b,sigb
c      call fit(y,x16,np,sig,mwt,a,b,siga,sigb,chi2,q)
c      gt(16)=b
c      write(*,205) '  Trait 16:',np,a,siga,b,sigb
c      call fit(y,x17,np,sig,mwt,a,b,siga,sigb,chi2,q)
c      gt(17)=b
c      write(*,205) '  Trait 17:',np,a,siga,b,sigb
       print *,clina
c.....Endalykkja fyrir eiginleikaflokka 1..5
        write(20,126)gt
        close(10)
        close(20)
c       rewind(13)
      enddo
  110 format(i4,7x,4x,196x,15f4.0)
  111 format(i4,15f4.0)
c 110 format(i4,7x,4x,49(4x),15i4)
  125 format(i4,15(i6,f6.1))
  126 format(4x,15(6x,f6.1))
c 126 format(12x,15(f15.6,6x))
  205 format(a10,'n:',i6,' a:',f12.4,' siga:',f9.4,' b:',f9.4,
     +' sigb:',f9.4)
      stop
      end
c     include '/home/agust/agusts/assub/sub06.f'
c     include '/home/agust/agusts/assub/sub08.f'
c     include '/home/agust/agusts/assub/sub13.f'
c     include '/home/agust/agusts/assub/sub14.f'
      include '/home/LBHI/elsa/elsaagust/agusts/assub/sub06.f'
      include '/home/LBHI/elsa/elsaagust/agusts/assub/sub08.f'
      include '/home/LBHI/elsa/elsaagust/agusts/assub/sub13.f'
      include '/home/LBHI/elsa/elsaagust/agusts/assub/sub14.f'
