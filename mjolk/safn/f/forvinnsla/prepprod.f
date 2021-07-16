c...LEITA ad ATH
c...Program prepprod.f
c...Nytt forrit sem tekur vid afurdagognum fra huppa.is og undirbyr
c...skra sem kallast Wxx0 sem kemur a undan codeherd.f inn i keyrslurnar
c...--------------------------------------------------------------------
c...Forritid les nyja uppfaerda afurdaskra fra huppa.is og:
c...   a.Rennir gognum i gegnum oryggissiu - a ekki ad thurfa i framtid
c...   b.Kodar umhverfisthaetti
c...   c.Skrifar ut skranna Wxx0
c...Ut falla forritin deila.f;Msny1.f;Msny2.f
c...====================================================================
C   Ymiss tekk eru gerd og golludum faerslu safnad i skra ms1.err
C   Faerslur eru skrifadar med einum bokstaf framan vid í ms1.err
C   allt eftir thvi hvada "villa" a i hlut thannig er:
C     a:Vantadi burdardag
C     b:Afurdir minni en 800 kg.
c...EF UM FRAML. AFURDIR ER AD RAEDA VERDUR AD SLEPPA THESSARI SIU
C     c:Burdabil<305
c...
C     f:Oll 3 mjolkurskeidin voru fyllt
C     g:Kyrinni var fargad samkvaemt sidasta mjolkurskeidi
C     j:Ekkert mjolkurskeid var í m6join en samt enginn las
C     d:Afurdir meiri en 14000 kg.
C     e:Faersla var laest í m6join!!
C     h:Sami burdardagur og a sidasta mjolkurskeidi!!
C     i:Burdardagur er fyrr en sidasti burdardagur!!
c     parameter(noani=169902)
c     parameter(noped=453604)
!     parameter(noani=173342)!mai15
!     parameter(noani=175868)!nov15product.n15
!     parameter(noani=179349)!nov16product.n16
!     parameter(noani=179349)!mai16product.n16
! aukakeyrsla sept 2016 tar sem adeins kynbotamat f. afurdir er reiknad
! notud sama aettarskra og i mai2016
!     parameter(noani=180596)!sep16product.n16
!     parameter(noani=181292)!nov16product.n16
!     parameter(noani=184499)!mai17product.m17
!     parameter(noani=186515)!mai17products.o17
!     parameter(noani=191158)!mai17products.m18
!     parameter(noani=195658)!jan19products.j19
!     parameter(noani=199480)!okt19products.o19
!     parameter(noani=202056)!jan20products.j20
!     parameter(noani=204319)!jan20products.jun20
!     parameter(noani=207626)!jan21products.jan21 ! eftir aðtt ut 108
      parameter(noani=207518)!jan21products.jan21
!     parameter(noped=491304)!mai16pgree.m16
!     parameter(noped=491304)!sep16pgree.s16
!     parameter(noped=500881)!nov16pgree.n16
!     parameter(noped=515916)!mai17pgree.m17
!     parameter(noped=526154)!mai17pgrees.o17
!     parameter(noped=542634)!mai17pgrees.m18
!     parameter(noped=560017)!jan19pgrees.j19
!     parameter(noped=560488)!jan19pgrees.mj19
!     parameter(noped=560012)!jan19pgrees.j19 endurgert
!     parameter(noped=576365)!okt19pgrees.o19 
!     parameter(noped=576353)!okt19pgrees.o19 endurgert
!     parameter(noped=586470)!j20pgrees.j20 
!     parameter(noped=596523)!j20pgrees.jun20 
      parameter(noped=611149)!j21pgrees.jan21 
c...nogped er ekki notad var notad a einhverja keyrslu vegna gagnavanda ur huppa.is
      parameter(nogped=243729)
      integer prod(31,noani),pl,aett(13,noped),ifix(12),iaett(10)
      integer codeyear,codeseas,codeage,codeci,xprod(31),vixl(6) 
      integer statfix(12,0:100),leibniz(0:7)
      real*8 ap,cpix(noani),aettix(noped),kusa
      real my,fp,p
!     character c1*2,pth*29,x*1
!     character c1*2,pth*26,x*1
      character c1*2,pth*21,x*1
      character*1 clas(noani)
      integer ibmb,inew
      integer gaett(4),gfman(nogped)
      real*8 gaettix(nogped)
      logical binsok,nytt
!     pth='/home/agust/skuggi_kyr/mjolk/'
!     open(09,file=pth//'safn/pgree.n14',status='old')
!     open(10,file=pth//'safn/product.n14',status='old')
!     open(20,file=pth//'vinnsla/Wxx0')
      pth='/home/elsa/Kyr/mjolk/'
c     open(09,file=pth//'safn/pgree.n14',status='old')
c     open(10,file=pth//'safn/product.n14',status='old')
c     open(09,file=pth//'safn/prufa',status='old')
!     open(09,file=pth//'safn/pgree.m15',status='old')
!     open(09,file=pth//'safn/pgrees.n15',status='old')
!     open(09,file=pth//'safn/pgrees.m16',status='old')
!     open(09,file=pth//'safn/pgrees.n16',status='old')
!     open(09,file=pth//'safn/pgrees.m17',status='old')
!     open(09,file=pth//'safn/pgrees.o17',status='old')
!     open(09,file=pth//'safn/pgrees.m18',status='old')
!     open(09,file=pth//'safn/pgrees.mj19',status='old')
!     open(09,file=pth//'safn/pgrees.j19',status='old') ! endurgert
!     open(09,file=pth//'safn/pgrees.o19',status='old') ! endurgert
!     open(09,file=pth//'safn/pgreess.o19',status='old') ! 
!     open(09,file=pth//'safn/pgrees.j20',status='old') ! 
!     open(09,file=pth//'safn/pgrees.jun20',status='old') ! 
      open(09,file=pth//'safn/pgrees.jan21',status='old') ! 
!     open(10,file=pth//'safn/product.m15',status='old')
!     open(10,file=pth//'safn/product.n15',status='old')
!     open(10,file=pth//'safn/product.m16',status='old')
!     open(10,file=pth//'safn/product.s16',status='old')
!     open(10,file=pth//'safn/product.n16',status='old')
!     open(10,file=pth//'safn/products.m17',status='old')
!     open(10,file=pth//'safn/products.o17',status='old')
!     open(10,file=pth//'safn/products.m18',status='old')
!     open(10,file=pth//'safn/products.j19',status='old')
!     open(10,file=pth//'safn/product.o19',status='old')
!     open(10,file=pth//'safn/products.o19',status='old') !ofundnarkusur i fyrri keyrslu eytt ut
!     open(10,file=pth//'safn/products.j20',status='old') !ofundnarkusur i fyrri keyrslu eytt ut
!     open(10,file=pth//'safn/products.jun20',status='old') !ofundnarkusur i fyrri keyrslu eytt ut
      open(10,file=pth//'safn/products.jan21',status='old') !ofundnarkusur i fyrri keyrslu eytt ut
      open(20,file=pth//'vinnsla/Wxx0')
      open(15,file=pth//'vinnsla/ofundid')
      open(16,file=pth//'vinnsla/fundid')
      inew=0
      ierra=0
      ierrb=0
      ierrc=0
      ierrd=0
      ierre=0
      ierrf=0
      ierrg=0
      ierrh=0
      ierri=0
      ierrj=0
      ierrk=0
      ierrl=0
      do q=0,7
        leibniz(q)=0
      enddo
      do i=1,12
        do j=0,100
          statfix(i,j)=0
        enddo
      enddo
      ncp=noani
      nap=0
c....nytt er false ef um er ad raeda framlengdar afurdir
c     nytt=.false.
      nytt=.true.
c.....lesa inn afurdaupplysingar.......................... 
      icnt1=0
      icnt11=0
      icnt10=0
      icnt2=0
      icnt21=0
      icnt20=0
      icnt3=0
      icnt31=0
      icnt30=0
      do i=1,ncp
        if(mod(i,10000).eq.0)Print *,'Read: ',i
        read(10,339)(prod(j,i),j=1,31),clas(i)
        cpix(i)=prod(1,i)*100000000000.d0+prod(2,i)*10000.d0+
     +          prod(3,i)
c.............................................
c...LEIBNIZ athugun
c     if(prod(14,i).gt.0.0.and.prod(15,i).gt.0.0.and.prod(16,i).gt.0.0)
c    +leibniz(7)=leibniz(7)+1
c     if(prod(14,i).gt.0.0.and.prod(15,i).gt.0.0.and.prod(16,i).eq.0.0)
c    +leibniz(3)=leibniz(3)+1
c     if(prod(14,i).gt.0.0.and.prod(15,i).eq.0.0.and.prod(16,i).eq.0.0)
c    +leibniz(1)=leibniz(1)+1
c     if(prod(14,i).gt.0.0.and.prod(15,i).eq.0.0.and.prod(16,i).gt.0.0)
c    +then
c       leibniz(5)=leibniz(5)+1
c       write(*,*)'Leibniz=5'
c       write(*,339)(prod(j,i),j=1,31),clas(i)
c     endif
c     if(prod(14,i).eq.0.0.and.prod(15,i).gt.0.0.and.prod(16,i).gt.0.0)
c    +leibniz(6)=leibniz(6)+1
c     if(prod(14,i).eq.0.0.and.prod(15,i).gt.0.0.and.prod(16,i).eq.0.0)
c    +leibniz(2)=leibniz(2)+1
c     if(prod(14,i).eq.0.0.and.prod(15,i).eq.0.0.and.prod(16,i).gt.0.0)
c    +leibniz(4)=leibniz(4)+1
c     if(prod(14,i).eq.0.0.and.prod(15,i).eq.0.0.and.prod(16,i).eq.0.0)
c    +then
c       leibniz(0)=leibniz(0)+1
c       write(*,*)'Leibniz=0'
c       write(*,339)(prod(j,i),j=1,31),clas(i)
c     endif
c.............................................
c...spurning med einhverjar siur her
c...SIUR BYRJA
c...1)mjolk 800-17000 kg
c...2)burdardagur skradur
c...3)bil a milli burda >=295 dagar eda forgunarastada>0
c...4)Leyfdar samsetningar=000;100;110;111
c...hnum.gripur-forgun-bd1-bd2-bd3-dmb1-dmb2-dmb3-m1-...pp3-snum-gnum-S-Las
c...i15-i2-3(i4,i2)-3(i4)-9(i5)-6(i4)-i6-i4-i1-a1
c 339 format(i4,i7,i4,i2,3(i4,i2),3i4,9i5,6i4,i6,i4,i1,a1)
c......fyrsta mjolkurskeid
       if(prod(14,i).eq.0)then
          icnt1=icnt1+1
       else
       if(prod(14,i).ge.800.and.prod(14,i).le.17000.and.
c    +prod(5,i).gt.0.and.((prod(11,i).ge.295).or.(prod(4,i).gt.0)))then
     +prod(5,i).gt.0.and.prod(11,i).ge.295)then
         icnt11=icnt11+1    
       else
        clas(i)='1'
        write(66,339)(prod(j,i),j=1,31),clas(i)
c        nullstilla allt fyrir t1,t2,t3
         do j=5,28
           prod(j,i)=0
         enddo
         icnt10=icnt10+1    
       endif 
       endif
c......annad mjolkurskeid
       if(prod(15,i).eq.0)then
          icnt2=icnt2+1
       else
       if(prod(15,i).ge.800.and.prod(15,i).le.17000.and.
     +prod(7,i).gt.0.and.prod(12,i).ge.295)then
         icnt21=icnt21+1    
       else
        clas(i)='2'
        write(66,339)(prod(j,i),j=1,31),clas(i)
c        nullstilla allt fyrir t2,t3
           prod(7,i)=0
           prod(8,i)=0
           prod(9,i)=0
           prod(10,i)=0
           prod(12,i)=0
           prod(13,i)=0
           prod(15,i)=0
           prod(16,i)=0
           prod(18,i)=0
           prod(19,i)=0
           prod(21,i)=0
           prod(22,i)=0
           prod(24,i)=0
           prod(25,i)=0
           prod(27,i)=0
           prod(28,i)=0
         icnt20=icnt20+1    
       endif 
       endif 
c......thridja mjolkurskeid
       if(prod(16,i).eq.0)then
          icnt3=icnt3+1
       else
       if(prod(16,i).ge.800.and.prod(16,i).le.17000.and.
     +prod(9,i).gt.0.and.prod(13,i).ge.295)then
         icnt31=icnt31+1    
       else
        clas(i)='3'
        write(66,339)(prod(j,i),j=1,31),clas(i)
c        nullstilla allt fyrir t3
           prod(9,i)=0
           prod(10,i)=0
           prod(13,i)=0
           prod(16,i)=0
           prod(19,i)=0
           prod(22,i)=0
           prod(25,i)=0
           prod(28,i)=0
         icnt30=icnt30+1    
       endif 
       endif 
c...SIUR ENDA
c.............................................
c...LEIBNIZ athugun
c...ATHUGa ad her er bara tekid a leibniz=5!!!!! Redding haust 2009
      if(prod(14,i).gt.0.0.and.prod(15,i).gt.0.0.and.prod(16,i).gt.0.0)
     +leibniz(7)=leibniz(7)+1
      if(prod(14,i).gt.0.0.and.prod(15,i).gt.0.0.and.prod(16,i).eq.0.0)
     +leibniz(3)=leibniz(3)+1
      if(prod(14,i).gt.0.0.and.prod(15,i).eq.0.0.and.prod(16,i).eq.0.0)
     +leibniz(1)=leibniz(1)+1
      if(prod(14,i).gt.0.0.and.prod(15,i).eq.0.0.and.prod(16,i).gt.0.0)
     +then
        leibniz(5)=leibniz(5)+1
        write(*,*)'Leibniz=5'
        write(*,339)(prod(j,i),j=1,31),clas(i)
c        nullstilla allt fyrir t3
           prod(9,i)=0
           prod(10,i)=0
           prod(13,i)=0
           prod(16,i)=0
           prod(19,i)=0
           prod(22,i)=0
           prod(25,i)=0
           prod(28,i)=0
         icnt30=icnt30+1    
      endif
      if(prod(14,i).eq.0.0.and.prod(15,i).gt.0.0.and.prod(16,i).gt.0.0)
     +leibniz(6)=leibniz(6)+1
      if(prod(14,i).eq.0.0.and.prod(15,i).gt.0.0.and.prod(16,i).eq.0.0)
     +leibniz(2)=leibniz(2)+1
      if(prod(14,i).eq.0.0.and.prod(15,i).eq.0.0.and.prod(16,i).gt.0.0)
     +leibniz(4)=leibniz(4)+1
      if(prod(14,i).eq.0.0.and.prod(15,i).eq.0.0.and.prod(16,i).eq.0.0)
     +then
        leibniz(0)=leibniz(0)+1
        write(*,*)'Leibniz=0'
        write(*,339)(prod(j,i),j=1,31),clas(i)
      endif
c.............................................
c...vegna rangrar upprodunar mjolkurskeida i huppuskra
c       do k=1,31
c         xprod(k)=prod(k,i)
c       enddo
c..........................................................
c       prod(5,i)=xprod(9)
c       prod(6,i)=xprod(10)
c       prod(9,i)=xprod(5)
c       prod(10,i)=xprod(6)
c       prod(11,i)=xprod(13)
c       prod(13,i)=xprod(11)
c       prod(14,i)=xprod(16)
c       prod(16,i)=xprod(14)
c       prod(17,i)=xprod(19)
c       prod(19,i)=xprod(17)
c       prod(20,i)=xprod(22)
c       prod(22,i)=xprod(20)
c       prod(23,i)=xprod(25)
c       prod(25,i)=xprod(23)
c       prod(26,i)=xprod(28)
c       prod(28,i)=xprod(26)
      enddo
c...Yfirlit vegna siuvinnu
      print *,icnt1,icnt11,icnt10
      print *,icnt2,icnt21,icnt20
      print *,icnt3,icnt31,icnt30
      print *,'=====Leibniz yfirlit====='
      do ji=0,7
        print *,ji,leibniz(ji)
      enddo
c     STOP
c...-----------lesa inn gomlu aetternisupplysingar----------
c 308 format(i1,i6,i4,i4,21x,i2) 
c     do i=1,nogped
c       if(mod(i,50000).eq.0)Print *,'Read-gaett: ',i
c       read(08,308)gaett,gfman(i)
c       gaettix(i)=gaett(3)*100000000000.+gaett(1)*10000000000.
c    +         +gaett(2)*10000.+gaett(4)
c     enddo
c...-----------lesa inn aetternisupplysingar----------
      do i=1,noped
        if(i.ge.50000.and.mod(i,1).eq.0)Print *,'Read: ',i
        read(09,309)(aett(j,i),j=1,13)
c       nr=nr+1
c       write(*,*) 'nr = ',nr
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
c.......na i fmanud ur gomlu pgree-skra
c       if(aett(13,i).eq.0)then
c         kusa=aett(1,i)*100000000000.+2*10000000000.
c    +         +aett(11,i)*10000.+aett(3,i)
c         if(binsok(gaettix,nogped,kusa,pl))then
c           aett(13,i)=gfman(pl) 
c         endif
c       endif
      enddo
c...=================vinna med afurdafaerslu===============================
      inn=0
      innei=0
      do i=1,ncp
        kusa=cpix(i)
c         write(*,'(f17.0)')kusa
        iaett(1)=prod(1,i)
        iaett(2)=prod(2,i)
        iaett(3)=prod(3,i)
c...rekja inn aett
        if(mod(i,1000).eq.0)Print *,'...rek inn aettina fyrir ',i
        if(binsok(aettix,noped,kusa,pl))then
          inn=inn+1
          write(16,'(a10,f17.0)')'Finn :',kusa
          iaett(4)=aett(4,pl)
          iaett(5)=aett(5,pl)
          iaett(6)=aett(6,pl)
          iaett(7)=aett(7,pl)
          iaett(8)=aett(8,pl)
          iaett(9)=aett(9,pl)
          iaett(10)=aett(13,pl)
          if(aett(11,pl).gt.0)then
            prod(29,i)=aett(11,pl)
          else
            prod(29,i)=aett(10,pl)
          endif
        else
          innei=innei+1
          write(55,'(a10,f17.0)')'Finn ekki:',kusa
          write(15,'(a10,f17.0)')'Finn ekki:',kusa
          iaett(4)=0
          iaett(5)=0
          iaett(6)=0
          iaett(7)=0
          iaett(8)=0
          iaett(9)=0
          iaett(10)=0
        endif
c...reikna ut umhverfiskoda
c       print *,'...reikna umhverfiskoda'
        do jj=1,12
          ifix(jj)=0
        enddo
        if(prod(14,i).gt.0)then
        ifix(1)=codeyear(prod(5,i),prod(6,i))
        ifix(4)=codeseas(prod(6,i)) 
        ifix(7)=codeage(prod(1,i),iaett(10),prod(5,i),prod(6,i),1) 
        ifix(10)=codeci(prod(11,i),prod(4,i)) 
          if(kusa.lt.198216711010158.)write(66,'(f17.0,4i4)')
     +       kusa,prod(1,i),iaett(10),prod(5,i),prod(6,i)
        endif
        if(prod(15,i).gt.0)then
        ifix(2)=codeyear(prod(7,i),prod(8,i))
        ifix(5)=codeseas(prod(8,i)) 
        ifix(8)=codeage(prod(1,i),iaett(10),prod(7,i),prod(8,i),2) 
        ifix(11)=codeci(prod(12,i),prod(4,i)) 
        endif
        if(prod(16,i).gt.0)then
        ifix(3)=codeyear(prod(9,i),prod(10,i))
        ifix(6)=codeseas(prod(10,i)) 
        ifix(9)=codeage(prod(1,i),iaett(10),prod(9,i),prod(10,i),3) 
        ifix(12)=codeci(prod(13,i),prod(4,i)) 
        endif
        write(67,'(12i3)')ifix
c       do jj=1,12
c         if(ifix(jj).eq.0)then
c           print *,'...stoppa vegna rugls - umhverfiskodi=0'
c           stop
c         endif
c       enddo
        statfix(1,ifix(1))=statfix(1,ifix(1))+1
        statfix(2,ifix(2))=statfix(2,ifix(2))+1
        statfix(3,ifix(3))=statfix(3,ifix(3))+1
        statfix(4,ifix(4))=statfix(4,ifix(4))+1
        statfix(5,ifix(5))=statfix(5,ifix(5))+1
        statfix(6,ifix(6))=statfix(6,ifix(6))+1
        statfix(7,ifix(7))=statfix(7,ifix(7))+1
        statfix(8,ifix(8))=statfix(8,ifix(8))+1
        statfix(9,ifix(9))=statfix(9,ifix(9))+1
        statfix(10,ifix(10))=statfix(10,ifix(10))+1
        statfix(11,ifix(11))=statfix(11,ifix(11))+1
        statfix(12,ifix(12))=statfix(12,ifix(12))+1
c... Write out to new file
        write(20,239)i,(iaett(j),j=1,9),2,
     + prod(29,i),0,(ifix(jj),jj=1,12),(prod(jjj,i),jjj=14,28),
     + (prod(jjj,i),jjj=5,10)
c         write(16,'(a10,f17.0)')'Finn :',kusa
      enddo  
      print *,'Finn     : ',inn
      print *,'Finn ekki: ',innei
      print *,'     alls: ',inn+innei
c.....................................................................
c..............................Format for I/O.....................
c id-ar-kyn-bu-num-s-mar-d-mbu-man-3bd_?-3dm_?,3af_?,h
c 3y,3s,3a,3c,3m,3f,3p,3f%,3p%,las
c...Lysing vegna aettarnisskrar
c...hnum=Huppunumer (nytt numer notad i huppa.is 4-7-4 (far.hbu.grip)
c...sbu=skyrsluhaldsbu, thar sem afurdir eda eiginleikar er maeldir
c...pgree.j09:hnum.gripur-hnum.fadir-hnum.modir-sfabu-sfrbu-kyn-fman
c... 15-15-15-6-6-1-2 eda 4.7.4-4.7.4-4.7.4-6-6-1-2
c...Lysing vegna afurdaskrar
c...product_xxxxxxj09:
c...snum=skyrsluhaldsnumer vid innlestur;gnum=gripanumer vid innlestur
c...S=lesid upp ur safnskra
c...hnum.gripur-forgun-bd1-bd2-bd3-dmb1-dmb2-dmb3-m1-...pp3-snum-gnum-S-Las
c...i15-i2-3(i4,i2)-3(i4)-9(i5)-6(i4)-i6-i4-i1-a1
c...Wxx0:
c...rad-hnum.gripur-hnum.fad-hnum.mod-kyn-sbu-umhkod-..-afurdir
c...i6-3i15-i2,i4,i4-12i2-9(i5)-6(i4)
  339 format(i4,i7,i4,i2,3(i4,i2),3i4,9i5,6i4,i6,i4,i1,a1)
c 239 format(i6,3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
  239 format(i6,3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4,3(i4,i2))
  309 format(i4,i7,i4,i4,i7,i4,i4,i7,i4,2i6,i1,i2)
c.....................................................................
      write(66,444)'   l          Ar               Man      ' 
     +//'         Ald               BMB      '
      write(66,444)'   f    M1    M2    M3    M1    M2    M3'
     +//'    M1    M2    M3    M1    M2    M3'
      do 180 i=0,50
  180   if(statfix(1,i).gt.0.or.statfix(4,i)
     +  .gt.0.or.statfix(7,i).gt.0.or.statfix(10,i).gt.0)
     +  write(66,445)i,(statfix(j,i),j=1,12)
  444 format(a76)
  445 format(i4,12i6)
      close(09)
      close(10)
      close(20)
 9999 stop
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

      integer function codeyear(iar,iman)
      integer iman,iar
      ima=iar*100+iman
      if(ima.lt.198307.and.ima.gt.197900)then
	codeyear=1
      else if(ima.lt.198407)then
	codeyear=2
      else if(ima.lt.198507)then
	codeyear=3
      else if(ima.lt.198607)then
	codeyear=4
      else if(ima.lt.198707)then
	codeyear=5
      else if(ima.lt.198807)then
	codeyear=6
      else if(ima.lt.198907)then
	codeyear=7
      else if(ima.lt.199007)then
	codeyear=8
      else if(ima.lt.199107)then
	codeyear=9
      else if(ima.lt.199207)then
	codeyear=10
      else if(ima.lt.199307)then
	codeyear=11
      else if(ima.lt.199407)then
	codeyear=12
      else if(ima.lt.199507)then
	codeyear=13
      else if(ima.lt.199607)then
	codeyear=14
      else if(ima.lt.199707)then
	codeyear=15
      else if(ima.lt.199807)then
	codeyear=16
      else if(ima.lt.199907)then
	codeyear=17
      else if(ima.lt.200007)then
	codeyear=18
      else if(ima.lt.200107)then
	codeyear=19
      else if(ima.lt.200207)then
        codeyear=20
      else if(ima.lt.200307)then
        codeyear=21
      else if(ima.lt.200407)then
        codeyear=22
      else if(ima.lt.200507)then
        codeyear=23
      else if(ima.lt.200607)then
        codeyear=24
      else if(ima.lt.200707)then
        codeyear=25
      else if(ima.lt.200807)then
        codeyear=26
      else if(ima.lt.200907)then
        codeyear=27
      else if(ima.lt.201007)then
        codeyear=28
      else if(ima.lt.201107)then
        codeyear=29
      else if(ima.lt.201207)then
        codeyear=30
      else if(ima.lt.201307)then
        codeyear=31
      else if(ima.lt.201407)then
        codeyear=32
      else if(ima.lt.201507)then
        codeyear=33
      else if(ima.lt.201607)then
        codeyear=34
      else if(ima.lt.201707)then
        codeyear=35
      else if(ima.lt.201807)then
        codeyear=36
      else if(ima.lt.201907)then
        codeyear=37
      else if(ima.lt.202007)then
        codeyear=38
      else if(ima.lt.202107)then
        codeyear=39 
      else
	codeyear=0
      endif
      return
      end
       
      integer function codeseas(iman) 
      integer iman
      if(iman.lt.7)then
	codeseas=iman+6
      else
	codeseas=iman-6
      endif
      return
      end

      integer function codeage(ifar,ifman,ibar,ibman,iparity) 
      integer ifar,ifman,ibman,ibar,iparity
c.....Ef faedingarmanud vantar er hann settur a juni
      if(ifman.eq.0)ifman=6
      iage=(ibar-ifar)*12+(ibman-ifman)
      if(iparity.eq.1)then
        if(iage.lt.22)then
          codeage=1
        else if(iage.lt.23)then
          codeage=2
        else if(iage.lt.24)then
          codeage=3
        else if(iage.lt.25)then
          codeage=4
        else if(iage.lt.26)then
          codeage=5
        else if(iage.lt.27)then
          codeage=6
        else if(iage.lt.28)then
          codeage=7
        else if(iage.lt.29)then
          codeage=8
        else if(iage.lt.30)then
          codeage=9
        else if(iage.lt.31)then
          codeage=10
        else if(iage.lt.32)then
          codeage=11
        else if(iage.lt.33)then
          codeage=12
        else if(iage.lt.34)then
          codeage=13
        else if(iage.lt.35)then
          codeage=14
        else if(iage.lt.36)then
          codeage=15
        else if(iage.lt.37)then
          codeage=16
        else if(iage.lt.38)then
          codeage=17
        else if(iage.lt.39)then
          codeage=18
        else if(iage.lt.40)then
          codeage=19
        else 
          codeage=20
        endif
      else if(iparity.eq.2)then
	if(iage.lt.35)then
	  codeage=1
	else if(iage.lt.37)then
	  codeage=2
	else if(iage.lt.39)then
	  codeage=3
	else if(iage.lt.41)then
	  codeage=4
	else if(iage.lt.43)then
	  codeage=5
	else if(iage.lt.45)then
	  codeage=6
	else if(iage.lt.47)then
	  codeage=7
	else if(iage.lt.49)then
	  codeage=8
	else if(iage.lt.51)then
	  codeage=9
	else if(iage.lt.53)then
	  codeage=10
	else if(iage.lt.60)then
	  codeage=11
	else 
	  codeage=12
	endif
      else if(iparity.eq.3)then
	if(iage.lt.47)then
	  codeage=1
	else if(iage.lt.49)then
	  codeage=2
	else if(iage.lt.51)then
	  codeage=3
	else if(iage.lt.53)then
	  codeage=4
	else if(iage.lt.55)then
	  codeage=5
	else if(iage.lt.57)then
	  codeage=6
	else if(iage.lt.59)then
	  codeage=7
	else if(iage.lt.61)then
	  codeage=8
	else if(iage.lt.63)then
	  codeage=9
	else if(iage.lt.66)then
	  codeage=10
	else 
	  codeage=11
	endif
      else
	codeage=0
      endif
  999 return
      end

      integer function codeci(ici,idisp) 
      integer ici,idisp
      if(idisp.gt.0.and.ici.eq.0)ici=9999 
      if(ici.lt.321)then
	codeci=1
      else if(ici.lt.331)then
	codeci=2
      else if(ici.lt.341)then
	codeci=3
      else if(ici.lt.351)then
	codeci=4
      else if(ici.lt.361)then
	codeci=5
      else if(ici.lt.371)then
	codeci=6
      else if(ici.lt.381)then
	codeci=7
      else if(ici.lt.391)then
	codeci=8
      else if(ici.lt.401)then
	codeci=9
      else if(ici.lt.411)then
	codeci=10
      else if(ici.lt.421)then
	codeci=11
      else if(ici.lt.431)then
	codeci=12
      else if(ici.lt.441)then
	codeci=13
      else if(ici.lt.451)then
	codeci=14
      else if(ici.lt.461)then
	codeci=15
      else if(ici.lt.471)then
	codeci=16
      else if(ici.lt.481)then
	codeci=17
      else if(ici.lt.491)then
	codeci=18
      else if(ici.lt.501)then
	codeci=19
      else if(ici.lt.550)then
	codeci=20
      else if(ici.lt.990)then
	codeci=22
      else 
	if(idisp.eq.3)then
	  codeci=21
	else if(idisp.eq.1.or.idisp.eq.2.or.idisp.eq.6)then
	  codeci=23
ccc Her tharf naudsynlega ad athuga hvan nyjar forgunarastadur eru t.e. 16-18???
c...her er thetta bara fellt inn sem 18 i stad 15 sem hamark mars 2006
	else if(idisp.ge.4.and.idisp.le.18)then
	  codeci=24
	else
c         codeci=0
	  codeci=21
	endif
      endif
      return
      end

