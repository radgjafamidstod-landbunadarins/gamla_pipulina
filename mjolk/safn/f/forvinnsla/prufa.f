c...Forritid kemur i stad pedrekur.f og rekur inn alla gripi ur
c...aetternisskra, thannig af BLUP keyrslan gefir mat a alla gripi
c... PEDREKUR.F
c... Traces pedigrees
      parameter(norec =169902)
      parameter(noped =453604)
      parameter(nony  =100000)

      integer rp(39,norec),aett(13,noped),ny(13,nony)
      integer ifbu(norec),icbu(norec)
      real*8  rpix(norec), aettix(noped),nyix(nony)
      real*8  dam,sire,ind
      integer pl,null30(29),vixl(6),isire(3,norec),idam(3,norec)
      integer lasth,cherd,ih1,isex(norec)
c     character las*1,clas*1,pth*34
      character las*1,clas*1,pth*31
      logical binsok,nysok,nytt
c     pth='/home/agust/skuggi_kyr/mjolk/safn/'
      pth='/home/LBHI/elsa/Kyr/mjolk/safn/'
c     open(09,file=pth//'Rpedrekur.log')
c     open(10,file=pth//'../vinnsla/Wxx1')
c     open(11,file=pth//'pgree.n14')
      open(11,file=pth//'pgree.m15')
c     open(22,file=pth//'../vinnsla/Wxxp')
c     do i=1,30
c      null30(i)=0
c     enddo
c 210 format(6x,3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
  309 format(i4,i7,i4,i4,i7,i4,i4,i7,i4,2i6,i1,i2)
c 230 format(3(i4,i7,i4),i2,i4,i3,i1,i2,17i2)
c 230 format(3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
c... read files
c...-----------lesa inn aetternisupplysingar----------
      n=0
      iostat=0
      do i=1,noped
c       if(mod(i,50000).eq.0)Print *,'Read: ',i
        read(11,309) ari,bui,radi,ars,bus,rads,ard,bud,radd,t1,t2,t3,t4
        n=n+1
c       if (aett(j,i),j=1,13.gt.9) then
c       print(*,309) (aett(j,i),j=1,13)
        if(iostat.ne.0) then
        print *, 'villa i linu ',n
        stop
        endif
      enddo 
      end
