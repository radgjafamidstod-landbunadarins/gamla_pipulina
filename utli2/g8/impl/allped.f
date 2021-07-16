c...Forritid kemur i stad pedrekur.f og rekur inn alla gripi ur
c...aetternisskra, thannig af BLUP keyrslan gefir mat a alla gripi
c... PEDREKUR.F
c... Traces pedigrees
c     parameter(norec =79817) !conf.Wxx1
c     parameter(noped =453604)
c     parameter(nony  =100000)
!!    parameter(norec =79817)
      parameter(norec =79816)
c     parameter(noped =466316)
!!    parameter(noped =478548)
!     parameter(noped =491304)
!     parameter(noped =500881)
!     parameter(noped =515915)
!     parameter(noped =526154) !pgrees.o17
!     parameter(noped =542634) !pgrees.m18
!     parameter(noped =560012) !pgrees.j19
!     parameter(noped =576353) !pgrees.o19
!     parameter(noped =586577) !pgrees.j20
!     parameter(noped =596523) !pgrees.jun20
      parameter(noped =611149) !pgrees.jan21
      parameter(nony  =120000)

      integer rp(15,norec),aett(13,noped),ny(13,nony)
      integer ifbu(norec),icbu(norec)
      real*8  rpix(norec), aettix(noped),nyix(nony)
      real*8  dam,sire,ind
      integer pl,null30(29),vixl(6),isire(3,norec),idam(3,norec)
      integer lasth,cherd,ih1,isex(norec)
c     character las*1,clas*1,pth*32
!!    character las*1,clas*1,pth*29
      character las*1,clas*1,pth*24
      logical binsok,nysok,nytt
c     pth='/home/agust/skuggi_kyr/utli2/g8/'
!!    pth='/home/LBHI/elsa/Kyr/utli2/g8/'
      pth='/home/elsa/Kyr/utli2/g8/'
      open(09,file=pth//'Rpedrekur.log')
      open(10,file=pth//'conf.Wxx1')
c     open(11,file=pth//'../../mjolk/safn/pgree.n14')
!     open(11,file=pth//'../../mjolk/safn/pgree.m15')
!!    open(11,file=pth//'../../mjolk/safn/pgrees.n15')
!     open(11,file=pth//'../../mjolk/safn/pgrees.m16')
!     open(11,file=pth//'../../mjolk/safn/pgrees.n16')
!     open(11,file=pth//'../../mjolk/safn/pgrees.m17')
!     open(11,file=pth//'../../mjolk/safn/pgrees.o17')
!     open(11,file=pth//'../../mjolk/safn/pgrees.m18')
!     open(11,file=pth//'../../mjolk/safn/pgrees.j19')
!     open(11,file=pth//'../../mjolk/safn/pgreess.o19')
!     open(11,file=pth//'../../mjolk/safn/pgrees.j20')
!     open(11,file=pth//'../../mjolk/safn/pgrees.jun20')
      open(11,file=pth//'../../mjolk/safn/pgrees.jan21')
      open(22,file=pth//'conf.prodped')
      do i=1,30
       null30(i)=0
      enddo
  210 format(i4,i7,i4,i4,i3,i1,i2,i3,7i2)
  309 format(i4,i7,i4,i4,i7,i4,i4,i7,i4,2i6,i1,i2)
  230 format(3(i4,i7,i4),i2,i4,i3,i1,i2,i3,7i2)
c... read files
c...-----------lesa inn aetternisupplysingar----------
      do i=1,noped
        if(mod(i,50000).eq.0)Print *,'Read: ',i
        read(11,309)(aett(j,i),j=1,13)
c...UR HUPPU.IS KEMUR VIXLAD FADIR OG MODIR!!!!!!!
        do jj=1,6
          vixl(jj)=aett(3+jj,i)
        enddo 
        aett(4,i)=vixl(4)
        aett(5,i)=vixl(5)
        aett(6,i)=vixl(6)
        aett(7,i)=vixl(1)
        aett(8,i)=vixl(2)
        aett(9,i)=vixl(3)
        aettix(i)=aett(1,i)*100000000000.d0+aett(2,i)*10000.d0+aett(3,i)
      enddo
      print *,'Read unit - 10'
c...Innihald i gagnaskra
c...Einstaklingsnumer-Ar athugunar-Mjrod-Grod
      do i=1,norec
        if(mod(i,1000).eq.0)Print *,'Read: ',i
        read(10,210)(rp(j,i),j=1,15)
        rpix(i)=rp(1,i)*100000000000.d0+rp(2,i)*10000.d0+rp(3,i)
      enddo
c...Saekja inn fodur og modur og kyn og framleidslubu ur pgree.j09
      do i=1,norec
        if(mod(i,1000).eq.0)Print *,'Rekja inn fodur og modur: ',i
        ind=rpix(i)
        if(binsok(aettix,noped,ind,pl))then
c         write(9,*)'Found in data file'
          isire(1,i)=aett(4,pl)
          isire(2,i)=aett(5,pl)
          isire(3,i)=aett(6,pl)
          idam(1,i)=aett(7,pl)
          idam(2,i)=aett(8,pl)
          idam(3,i)=aett(9,pl)
          isex(i)=aett(12,pl)
          if(aett(11,pl).gt.0)then
            ifbu(i)=aett(11,pl)
          else
            ifbu(i)=aett(10,pl)
          endif   
         else
c           write(9,*)'Not found in pedigree file'
c...Finnst ekki i aetternisskra - faerslu skal eytt
c...sett serstok merking i rp(4,i)=-1 til ad eyda a eftir
c...thegar kemur ad thvi ad skrfa ut i skra
          rp(4,i)=-1
          isire(1,i)=0
          isire(2,i)=0
          isire(3,i)=0
          idam(1,i)=0
          idam(2,i)=0
          idam(3,i)=0
          ifbu(i)=0
         endif
      enddo
c...Her er hagt ad vinna i naestu linum og sleppa codeherd.f - thad ekki gert nu en mjog audvelt
      goto 991
c... sortera bu til ad fa bukoda
c...sort and join 
      call exec('rm vinnsla/mjalt.Wxxx')
      call exec('rm vinnsla/mjalt.Wxxy')
      open(44,file='vinnsla/mjalt.Wxxx')
      do i=1,norec
        write(44,'(2i6)')i,ifbu(i)
      enddo
      close(44)
      call exec('sort +0.6 -0.12 vinnsla/mjalt.Wxxx 
     +           -ovinnsla/mjalt.Wxxy')
      call exec('rm vinnsla/mjalt.Wxxx')
      call exec('sync')
      open(44,file='vinnsla/mjalt.Wxxy')
      open(45,file='vinnsla/mjalt.Wxxx')
      lasth=0
      ih1=0
c.....Read files..........................
      do i=1,norec
        read(44,'(2i6)')irbu,cherd
        if(cherd.gt.lasth)then
          ih1=ih1+1
          lasth=cherd
          print *,'New herd: ',ih1
        endif
        icbu(i)=ih1
        write(45,'(2i6)')irbu,ih1
      enddo
      print *,'Number of herds: ',ih1
c......Format for I/O.....................
      close(44)
      close(45)
      call exec('rm vinnsla/mjalt.Wxxy')
      call exec('sort +0.0 -0.6 vinnsla/mjalt.Wxxx 
     +           -ovinnsla/mjalt.Wxxy')
      call exec('rm vinnsla/mjalt.Wxxx')
      call exec('sync')
      open(44,file='vinnsla/mjalt.Wxxy')
      do i=1,norec
        read(44,'(2i6)')irbu,icbu(i)
      enddo
      close(44)
  991 continue 
c...buid ad fa bukoda..................................
c...Fer i gegnum aetternisskranna og athugar hvort
c...gripur er med i domaskra
c...ef ekki tha skrifad ut i skra
      do i=1,noped
        ind=aettix(i)
	if(mod(i,1000).eq.0)write(*,*)'Vinn med aetternisgrip...',i
	if(binsok(rpix,norec,ind,pl))then
c	   write(9,*)'Found in data file',rpix(i)
	else
          write(22,230)(aett(j,i),j=1,9),aett(12,i),
     +0,0,0,0,0,0,0,0,0,0,0,0
        endif
      enddo
c...BUID AD FARA I GEGNUM AETTERNISSKRA
c...Skrifa ut kyr med gogn
      do i=1,norec
        if(rp(4,i).ne.-1)
     +  write(22,230)(rp(jk,i),jk=1,3),(isire(jl,i),jl=1,3),
c    +  (idam(jm,i),jm=1,3),2,icbu(i),(rp(jn,i),jn=4,6)
     +  (idam(jm,i),jm=1,3),isex(i),(rp(jn,i),jn=4,15)
      enddo
      close(9)
      close(10)
      close(11)
      close(22)
c...Rada safnskra afurda og aetternisgripa
      call exec('rm vinnsla/mjalt.Wxx2')
      call exec('rm vinnsla/mjalt.Wxxy')
      call exec('sort -n +0.0 -0.11 +0.11 -0.15
c    + /home/agust/skuggi_kyr/utli2/g8/conf.prodped
c    + -o/home/agust/skuggi_kyr/utli2/g8/conf.Wxx2')
!!   + /home/LBHI/elsa/Kyr/utli2/g8/conf.prodped
!!   + -o/home/LBHI/elsa/Kyr/utli2/g8/conf.Wxx2')
     + /home/elsa/Kyr/utli2/g8/conf.prodped
     + -o/home/elsa/Kyr/utli2/g8/conf.Wxx2')
      call exec('sync')
c... fin
      stop
      end

      logical function binsok(a,n,t,pl)
      integer n,pl
      integer min,max,mitt
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

      logical function nysok(a,n,n2,t,pl)
      integer pl,n,n2
      real*8 a(n),t
      do 399 i=1,n2
	if(a(i).eq.t)then
	  nysok=.true.
	  pl=i
	  goto 999
	endif
  399 continue
      nysok=.false.
      pl=1
  999 return
      end
