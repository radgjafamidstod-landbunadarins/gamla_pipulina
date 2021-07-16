c...Reiknar medal-kynbotagildi innan ara og adhvarf kynbotagildis
c   a faedingarar fyrir naut
      program gtrend
      real*4 x1(99),x2(99),x3(99),m1,m2,m3,aa(3),gg(3)
      real*4 sig(99),a,b,siga,sigb,
     + y(99),chi2,q,r,zg,prob,w1(99),w2(99)
      integer n1(99),n2(99),n3(99)
      character*26 tmp
      character tmp2*7
      character*10 mlina*72,dlina*72,clina*72
      dlina='===========================================================  
     +============='
      clina='-----------------------------------------------------------  
     +-------------'
      open(10,file='keepF/Rimpl41.txt')
      open(13,file='/home/naut/vinnsla/Rmain')
c  -------------------------------------------------------------------
c...for the phantom parents
c     open(61,file='sep96/impl/keep/Rgroup41.txt.5',status='old')
      do i=1,19
        read(10,*)m1,m2,m3
c       read(61,*)aa
c       do j=1,3
c         gg(j)=gg(j)+aa(j)
c       enddo
      enddo
c     do j=1,3
c       gg(j)=gg(j)/19.
c     enddo
c     close(61)
c     print *,'Medaltol phantom parents:',gg
      ndata=93124
      do 100 i=1,ndata
        if(mod(i,1000).eq.0)print *,'..read',i
        read(10,*)m1,m2,m3
        m1=m1
        m2=m2
        m3=m3
        read(13,'(5x,i2,i1,6x,i5)')np,isex,inum
        if(isex.eq.1.and.inum.lt.50)then
          x1(np)=x1(np)+m1
          n1(np)=n1(np)+1
          x2(np)=x2(np)+m2
          n2(np)=n2(np)+1
          x3(np)=x3(np)+m3
          n3(np)=n3(np)+1
        endif
  100 continue
      np=0
      do 117 i=70,90
        x1(i)=x1(i)/n1(i)
        x2(i)=x2(i)/n2(i)
        x3(i)=x3(i)/n3(i)
        print 125,i,n1(i),x1(i),n2(i),x2(i),n3(i),x3(i)
        if(i.ge.80)then
          np=np+1
          x1(np)=x1(i)
          x2(np)=x2(i)
          x3(np)=x3(i)
          y(np)=i
        endif
  117 continue
       mwt=0
       print *,clina
       print *,'Adhvarf medal-kynbotagildis a faedingarar:80-90'
       print *,clina
       call fit(y,x1,np,sig,mwt,a,b,siga,sigb,chi2,q)
       write(*,205) 'Eiginl. 1:',np,a,siga,b,sigb
       call fit(y,x2,np,sig,mwt,a,b,siga,sigb,chi2,q)
       write(*,205) 'Eiginl. 2:',np,a,siga,b,sigb
       call fit(y,x3,np,sig,mwt,a,b,siga,sigb,chi2,q)
       write(*,205) 'Eiginl. 3:',np,a,siga,b,sigb
       print *,clina
  122 format(3f14.6)
  123 format(i5,i2)
  125 format(2i6,f15.6,i6,f15.6,i6,f15.6)
  126 format(2i6,f6.1,i6,f6.1)
  127 format(2i6,f6.1)
  205 format(a10,'n:',i6,' a:',f9.4,' siga:',f9.4,' b:',f9.4,
     +' sigb:',f9.4)
      stop
      end
      include '/home/agust/assub/sub06.f'
      include '/home/agust/assub/sub08.f'
      include '/home/agust/assub/sub13.f'
      include '/home/agust/assub/sub14.f'
