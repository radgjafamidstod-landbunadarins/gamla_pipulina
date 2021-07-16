c...Forritid kemur i stad pedrekur.f og rekur inn alla gripi ur
c...aetternisskra, thannig af BLUP keyrslan gefir mat a alla gripi
c... PEDREKUR.F
c... Traces pedigrees
!     parameter(norec =169902)
!     parameter(noped =453604)
!     parameter(norec =173342) ! Wxx0
!     parameter(noped =466316) ! pgree.m15
!     parameter(norec =175868) ! Wxx0
!     parameter(norec =179349) ! Wxx0
!     parameter(norec =180596) ! Wxx0 sep2016
!     parameter(norec =181292) ! Wxx0 nov2016
      parameter(norec =184499) ! Wxx0 ma12017
!     parameter(noped =478548) ! pgrees.n15
!     parameter(noped =491304) ! pgrees.m16
!     parameter(noped =500881) ! pgrees.n16
      parameter(noped =515916) ! pgrees.m17
      parameter(nony  =300000)

      integer rp(39,norec),aett(13,noped),ny(13,nony)
      integer ifbu(norec),icbu(norec)
      real*8  rpix(norec), aettix(noped),nyix(nony)
      real*8  dam,sire,ind
      integer pl,null30(29),vixl(6),isire(3,norec),idam(3,norec)
      integer lasth,cherd,ih1,isex(norec)
c     character las*1,clas*1,pth*34
!     character las*1,clas*1,pth*31
      character las*1,clas*1,pth*26
      logical binsok,nysok,nytt
c     pth='/home/agust/skuggi_kyr/mjolk/safn/'
!     pth='/home/LBHI/elsa/Kyr/mjolk/safn/'
      pth='/home/elsa/Kyr/mjolk/safn/'
      open(09,file=pth//'Rpedrekur.log')
      open(10,file=pth//'../vinnsla/Wxx1')
!     open(11,file=pth//'pgree.n14')
!     open(11,file=pth//'pgree.m15')
!     open(11,file=pth//'pgrees.n15')
!     open(11,file=pth//'pgrees.m16')
!     open(11,file=pth//'pgrees.n16')
      open(11,file=pth//'pgrees.m17')
      open(22,file=pth//'../vinnsla/Wxxp')
      do i=1,30
       null30(i)=0
      enddo
  210 format(6x,3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
  309 format(i4,i7,i4,i4,i7,i4,i4,i7,i4,2i6,i1,i2)
c 230 format(3(i4,i7,i4,i2,i4,i3,i1,i2,17i2)
  230 format(3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
c... read files
c...-----------lesa inn aetternisupplysingar----------
      do i=1,noped
c       if(mod(i,50000).eq.0)Print *,'Read: ',i
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
        read(10,210)(rp(j,i),j=1,39)
        rpix(i)=rp(1,i)*100000000000.d0+rp(2,i)*10000.d0+rp(3,i)
      enddo
c...Saekja inn fodur og modur og kyn og framleidslubu ur pgree.j09
c     do i=1,norec
c       if(mod(i,1000).eq.0)Print *,'Rekja inn fodur og modur: ',i
c       ind=rpix(i)
c       if(binsok(aettix,noped,ind,pl))then
c         write(9,*)'Found in data file'
c         isire(1,i)=aett(4,pl)
c         isire(2,i)=aett(5,pl)
c         isire(3,i)=aett(6,pl)
c         idam(1,i)=aett(7,pl)
c         idam(2,i)=aett(8,pl)
c         idam(3,i)=aett(9,pl)
c         isex(i)=aett(12,pl)
c         if(aett(11,pl).gt.0)then
c           ifbu(i)=aett(11,pl)
c         else
c           ifbu(i)=aett(10,pl)
c         endif   
c        else
c           write(9,*)'Not found in pedigree file'
c...Finnst ekki i aetternisskra - faerslu skal eytt
c...sett serstok merking i rp(4,i)=-1 til ad eyda a eftir
c...thegar kemur ad thvi ad skrfa ut i skra
c         rp(4,i)=-1
c         isire(1,i)=0
c         isire(2,i)=0
c         isire(3,i)=0
c         idam(1,i)=0
c         idam(2,i)=0
c         idam(3,i)=0
c         ifbu(i)=0
c        endif
c     enddo
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
     +0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        endif
      enddo
c...BUID AD FARA I GEGNUM AETTERNISSKRA
c...Skrifa ut kyr med gogn
      do i=1,norec
        write(22,230)(rp(jk,i),jk=1,39)
      enddo
      close(9)
      close(10)
      close(11)
      close(22)
c...Rada safnskra afurda og aetternisgripa
c     call exec('rm /home/agust/skuggi_kyr/mjolk/vinnsla/Wxx2')
!     call exec('rm /home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx2')
      call exec('rm /home/elsa/Kyr/mjolk/vinnsla/Wxx2')
c     call exec('rm vinnsla/mjalt.Wxxy')
      call exec('sort -n +0.0 -0.11 +0.11 -0.15
c    + /home/agust/skuggi_kyr/mjolk/vinnsla/Wxxp
c    + -o/home/agust/skuggi_kyr/mjolk/vinnsla/Wxx2')
!    + /home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxxp
!    + -o/home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx2')
     + /home/elsa/Kyr/mjolk/vinnsla/Wxxp
     + -o/home/elsa/Kyr/mjolk/vinnsla/Wxx2')
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
