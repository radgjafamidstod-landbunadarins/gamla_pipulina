c... PEDREKUR.F
c... Traces pedigrees
      parameter(norec =51622)
      parameter(noped =330669)
      parameter(nony  =100000)

      integer rp(24,norec),aett(13,noped),ny(13,nony)
      integer ifbu(norec),icbu(norec)
      real*8  rpix(norec), aettix(noped),nyix(nony)
      real*8  dam,sire,ind
      integer pl,null30(29),vixl(6),isire(3,norec),idam(3,norec)
      integer lasth,cherd,ih1
      character las*1,clas*1,pth*33
      logical binsok,nysok,nytt
      pth='/home/LBHI/agust/BASSI/utlit/n17/'
      open(09,file=pth//'Rpedrekur.log')
      open(10,file=pth//'conf.Wxx1')
      open(11,file=pth//'../../mjolk/safn/pgree.a09')
      open(22,file=pth//'conf.prodped')
      do i=1,30
       null30(i)=0
      enddo
  210 format(i4,i7,i4,i4,i3,i1,i2,17i2)
  309 format(i4,i7,i4,i4,i7,i4,i4,i7,i4,2i6,i1,i2)
  230 format(3(i4,i7,i4),i2,i4,i3,i1,i2,17i2)
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
        read(10,210)(rp(j,i),j=1,24)
        rpix(i)=rp(1,i)*100000000000.d0+rp(2,i)*10000.d0+rp(3,i)
      enddo
c...Saekja inn fodur og modur og framleidslubu ur pgree.j09
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
c... main loop 1 starts...
      iny=0
      inyn=0
      inyd=0
      inys=0
      do 189 i=1,norec
	if(mod(i,1000).eq.0)write(*,*)'Working with Cow...',i
	if(idam(1,i).gt.0)then
	  dam=idam(1,i)*100000000000.d0+idam(2,i)*10000.d0+idam(3,i)
	  if(binsok(rpix,norec,dam,pl))then
c	    write(9,*)'Found in data file',rpix(pl)
	  else if(nysok(nyix,nony,iny,dam,pl))then
c	    write(9,*)'Already in list'
	  else if(binsok(aettix,noped,dam,pl))then
	    iny=iny+1
	    inyd=inyd+1
	    do 188 ix=1,13
  188         ny(ix,iny)=aett(ix,pl)
	      nyix(iny)=aett(1,pl)*100000000000.d0+aett(2,pl)*10000.d0+aett(3,pl)
c	      write(9,*)'Dam picked up in PGREE'
	  else
c	    write(9,*)'Dam not found anywhere'
	    infd=infd+1
	    idam(1,i)=0
	    idam(2,i)=0
	    idam(3,i)=0
	  endif
	endif
c... sakja fodur
	if(isire(1,i).gt.0)then
	  sire=isire(1,i)*100000000000.d0+isire(2,i)*10000.d0+isire(3,i)
	 if(nysok(nyix,nony,iny,sire,pl))then
c	   write(9,*)'Sire found in ny'
	 else if(binsok(aettix,noped,sire,pl))then
	    iny=iny+1
	    inys=inys+1
	    do 108 ix=1,13
  108         ny(ix,iny)=aett(ix,pl)
	      nyix(iny)=aett(1,pl)*100000000000.d0+aett(2,pl)*10000.d0+aett(3,pl)
c              write(9,*)'Sire picked up in PGREE'                
	  else
	    write(9,*)'Sire not found anywhere',sire
	    infs=infs+1
	    isire(1,i)=0
	    isire(2,i)=0
	    isire(3,i)=0
	  endif
	endif
  189 continue
c... main loop 1 fin...
      write(9,*)'Total new dams:',inyd
      write(9,*)'Total new bulls:',inys
      write(9,*)'Dams not found:',infd
      write(9,*)'Sires not found:',infs
      write(*,*)'Tracing parents for Datafile finished...'
c... main loop 2 starts...
      do 139 i=1,nony
c...    stokkid ur thessari "loop"
	if(ny(1,i).eq.0)goto 930
	if(mod(i,1000).eq.0)write(*,*)'Working with Animal...',i
	if(ny(7,i).gt.0)then
	  dam=ny(7,i)*100000000000.d0+ny(8,i)*10000.d0+ny(9,i)
c          write(9,*)(ny(j3,i),j3=6,8),dam
	  if(binsok(rpix,norec,dam,pl))then
c	    write(9,*)'Dam found in datafile',rpix(pl)
	  else if(nysok(nyix,nony,iny,dam,pl))then
c	    write(9,*)'Dam already in list'
	  else if(binsok(aettix,noped,dam,pl))then
	    iny=iny+1
	    inyd=inyd+1
	    do 138 ix=1,13
  138         ny(ix,iny)=aett(ix,pl)
	      nyix(iny)=aett(1,pl)*100000000000.d0+aett(2,pl)*10000.d0+aett(3,pl)
c              write(9,*)'Dam picked up in PGREE'
	  else
c	    write(9,*)'Dam not found anywhere'
	    infd=infd+1
	    ny(7,i)=0
	    ny(8,i)=0
	    ny(9,i)=0
	  endif
	endif
c... Hér òarf aî muna aî breyta efri mörkum á nauti òegar fram líîa
	if(ny(4,i).gt.0)then
	  sire=ny(4,i)*100000000000.d0+ny(5,i)*10000.d0+ny(6,i)
c          write(9,*)ny(5,i),sire
	 if(nysok(nyix,nony,iny,sire,pl))then
c	   write(9,*)'Sire already in list'
	 else if(binsok(aettix,noped,sire,pl))then
	    iny=iny+1
	    inys=inys+1
	    do 618 ix=1,13
  618         ny(ix,iny)=aett(ix,pl)
	      nyix(iny)=aett(1,pl)*100000000000.d0+aett(2,pl)*10000.d0+aett(3,pl)
c              write(9,*)'Sire picked up in PGREE'
	  else
c	    write(9,*)'Sire not found anywhere'
	    infs=infs+1
	    ny(4,i)=0
	    ny(5,i)=0
	    ny(6,i)=0
	  endif
	endif
  139 continue
c... main loop 2 fin...
  930 write(9,*)'Jump from main2 with i and iny:',i,iny
      write(9,*)'Total new dams:',inyd
      write(9,*)'Total new bulls:',inys
      write(9,*)'Dams not found:',infd
      write(9,*)'Bulls not found:',infs
c... write to files
      print *,'Tracing parents for list-animals finished...'
      print *,'Writing to files...'
c...Skrifa ut kyr med gogn
      do i=1,norec
        if(rp(4,i).ne.-1)
     +  write(22,230)(rp(jk,i),jk=1,3),(isire(jl,i),jl=1,3),
     +  (idam(jm,i),jm=1,3),2,(rp(jn,i),jn=4,24)
      enddo
c...Skrifa ut aetternisgripi
      do i=1,iny
        write(22,230)(ny(jk,i),jk=1,9),ny(12,i),
     +0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
      enddo
      close(9)
      close(10)
      close(11)
      close(22)
c...Rada safnskra afurda og aetternisgripa
      call exec('sort -n +0.0 -0.11 +0.11 -0.15
     + /home/LBHI/agust/BASSI/utlit/n17/conf.prodped
     + -o/home/LBHI/agust/BASSI/utlit/n17/conf.Wxx2')
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
