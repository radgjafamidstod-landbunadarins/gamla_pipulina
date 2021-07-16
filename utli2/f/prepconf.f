c...HER ER BUID AD SETJA DOMSAR 2000 sem SIU
c...Program prepconf.f
c...Nytt forrit sem tekur vid skopulagsgognum fra huppa.is
c...og undirbyr skrar sem kallast conf.old og conf.new
c...--------------------------------------------------------------------
c...Forritid les nyja uppfaerda afurdaskra fra huppa.is og:
c...   a.Rennir gognum i gegnum oryggissiu - a ekki ad thurfa i framtid
c...   b.Kodar umhverfisthaetti
c...   c.Skrifar ut skranna Wxx0
c...Ut falla forritin deila.f;Msny1.f;Msny2.f
c...====================================================================
C   Ymiss tekk eru gerd og golludum faerslu safnad i skra ms1.err
c... Pick up lactation number and age at 1:st birth of cow
c... Gagnasiur
c     parameter(norec = 98301) ! conf.gogn.1411
c     parameter(noani= 453604)
c     parameter(noprod= 169902)
!     parameter(norec = 102480) ! conf.gogn.1505
!!    parameter(norec = 105049) ! conf.gogn.1511s
!     parameter(norec = 107702) ! conf.gogn.1605s
!     parameter(norec = 110827) ! conf.gogn.1611s
!     parameter(norec = 115380) ! conf.gogn.1705s
!     parameter(norec = 116903) ! conf.gogn.1710s
!     parameter(norec = 121368) ! conf.gogn.1805s
!     parameter(norec = 125428) ! conf.gogn.1901s
!     parameter(norec = 130359) ! conf.gogn.1910s
!     parameter(norec = 132349) ! conf.gogn.0120s
!     parameter(norec = 135051) ! conf.gogn.0620s
      parameter(norec = 137947) ! conf.gogn.2101s
!     parameter(noani= 466316)
!!    parameter(noani= 478547)!Rmain undir mjolk/vinnsla
!     parameter(noani= 491303)!Rmain undir mjolk/vinnsla
!     parameter(noani= 500881)!Rmain undir mjolk/vinnsla
!     parameter(noani= 515915)!Rmain undir mjolk/vinnsla
!     parameter(noani= 526154)!Rmain undir mjolk/vinnsla
!     parameter(noani= 542633)!Rmain undir mjolk/vinnsla
!     parameter(noani= 560012)!Rmain undir mjolk/vinnsla
!     parameter(noani= 576353)!Rmain undir mjolk/vinnsla
!     parameter(noani= 586577)!Rmain undir mjolk/vinnsla
!     parameter(noani= 596523)!Rmain undir mjolk/vinnsla
      parameter(noani= 611146)!Rmain undir mjolk/vinnsla
!     parameter(noprod= 173342) !product.m15
!!    parameter(noprod= 175868) !product.n15
!     parameter(noprod= 179349) !product.m16
!     parameter(noprod= 181292) !product.n16
!     parameter(noprod= 184499) !product.m17
!     parameter(noprod= 186515) !products.o17
!     parameter(noprod= 191158) !products.m18
!     parameter(noprod= 195658) !products.j19
!     parameter(noprod= 199480) !products.o19
!     parameter(noprod= 202056) !products.j20
!     parameter(noprod= 204319) !products.jun20
      parameter(noprod= 207518) !products.jan21

      integer prod(40,noani),idom(38),pix(3),pl,ncp,pl2
      real*8 ap,cpix(noani),prix(noprod),ap2
c     character*1 clas(noani),dom(36)*15,pth*29,pt2*34,pt3*37
!     character*1 clas(noani),dom(36)*15,pth*26,pt2*31,pt3*34
      character*1 clas(noani),dom(36)*15,pth*21,pt2*26,pt3*29
      integer cbd(6,noprod),lac,age
      integer stat(2,20),stdo(34,0:20)
      logical binsok,binsok2
c     pth='/home/agust/skuggi_kyr/utli2/'
c     pt2='/home/agust/skuggi_kyr/mjolk/safn/'
c     pt3='/home/agust/skuggi_kyr/mjolk/vinnsla/'
!!    pth='/home/LBHI/elsa/Kyr/utli2/'
!!    pt2='/home/LBHI/elsa/Kyr/mjolk/safn/'
!!    pt3='/home/LBHI/elsa/Kyr/mjolk/vinnsla/'
      pth='/home/elsa/Kyr/utli2/'
      pt2='/home/elsa/Kyr/mjolk/safn/'
      pt3='/home/elsa/Kyr/mjolk/vinnsla/'
      icnt=0
      open(9,file=pth//'safn/confgetlact.log')
c     open(10,file=pth//'safn/conf.gogn.1411')
!     open(10,file=pth//'safn/conf.gogn.1505')
!!    open(10,file=pth//'safn/conf.gogn.1511s')
!     open(10,file=pth//'safn/conf.gogn.1605s')
!     open(10,file=pth//'safn/conf.gogn.1611s')
!     open(10,file=pth//'safn/conf.gogn.1705s')
!     open(10,file=pth//'safn/conf.gogn.1710s')
!     open(10,file=pth//'safn/conf.gogn.1805s')
!     open(10,file=pth//'safn/conf.gogn.1901s')
!     open(10,file=pth//'safn/conf.gogn.1910s')
!     open(10,file=pth//'safn/conf.gogn.0120s')
!     open(10,file=pth//'safn/conf.gogn.0620s')
      open(10,file=pth//'safn/conf.gogn.2101s')
c     open(11,file=pt2//'product.n14',status='old')
!     open(11,file=pt2//'product.m15',status='old')
!!    open(11,file=pt2//'product.n15',status='old')
!     open(11,file=pt2//'product.m16',status='old')
!     open(11,file=pt2//'product.n16',status='old')
!     open(11,file=pt2//'products.m17',status='old')
!     open(11,file=pt2//'products.o17',status='old')
!     open(11,file=pt2//'products.m18',status='old')
!     open(11,file=pt2//'products.j19',status='old')
!     open(11,file=pt2//'products.o19',status='old')
!     open(11,file=pt2//'products.j20',status='old')
!     open(11,file=pt2//'products.jun20',status='old')
      open(11,file=pt2//'products.jan21',status='old')
      open(12,file=pt3//'Rmain',status='old')
      open(21,file=pth//'g8/conf.old')
      open(22,file=pth//'n17/conf.new')
c     call nolli(rp,39)
c     call nollmi(oldc,44,noani)
c     call nollmi(cbd,6,noani)
c     call nollmi(igerfi,3,noani)
c     call nollmi(stat,2,20)
c     call nollr(cpix,noani)
      do k=1,20
        stat(1,k)=0
        stat(2,k)=0
      enddo
      print *,'Byrja........'
      do i=1,34
       do j=0,20
        stdo(i,j)=0
       enddo
      enddo
      print *,'...afram....'

c 210 format(i4,i6,2i2,i3,i4,i7,i3,8i2,i1,17i1,i6,i4,i4)
c 210 format(i4,i7,i4,i4,2i2,i3,8i2,18i1,i6,i4,i1)
  210 format(i4,i7,i4,i4,2i2,i3,1x,8i2,10i1,1x,3i1,1x,3i1,1x,2i1,2x,
     +3i1,i6,i4,i1)
  339 format(i6,3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
  220 format(i4,i6,2i2,i3,i4,i7,i3,8i2,i1,17i1,i6,i4,i4,i1,i2)
c****************************************************************
c id-ar-kyn-bu-num-s-mar-d-mbu-man-3bd_?-3dm_?,3af_?,h
c 3y,3s,3a,3c,3m,3f,3p,3f%,3p%,las
  230 format(i6,i4,i1,i6,i4,i7,i4,i4,i6,i2,3(i4,i2),3i1,3i4,3i2,i4
     + ,12i2,9i5,6i4,a1)
c...Format for datafile: 
	dom( 1)='Faedingarar'
	dom( 2)='Huppunumer'
	dom( 3)='Radnumer'
	dom( 4)='Domsar'
	dom( 5)='Domsmanudur'
	dom( 6)='Domsdagur'
	dom( 7)='Staerd/Bmal'
c...............'Yfirlina'
	dom( 8)='Bolur G'
	dom( 9)='Malir G'
	dom(10)='Faetur G'
	dom(11)='Jugurlag G'
	dom(12)='Jugurfesta G'
	dom(13)='Spenalengd G'
	dom(14)='Spenalag G'
	dom(15)='Mjaltir G'
	dom(16)='Skap G'
	dom(17)='Boldypt N'
	dom(18)='Utlogur N'
	dom(19)='Yfirlina N'
	dom(20)='Malabreidd N'
	dom(21)='Malahalli N'
	dom(22)='Malabratti N'
   	dom(23)='Stada h.hlid N'
   	dom(24)='Stada h.aftan N'
	dom(25)='Klaufahalli N'
c...............'Jafnvaegi'
	dom(26)='Jugurfesta N'
	dom(27)='Jugurband N'
	dom(28)='Jugurdypt N'
c...............'Spenagerd'
	dom(29)='Spenalengd N'
	dom(30)='Spenapykkt N'
	dom(31)='Spenastada N'
c...............'Spenaoddur'
	dom(32)='Mjaltir N'
	dom(33)='Skap N'
c...............'Aukaspenar'
c...............'Haed'
	dom(34)='Skyrsluhaldsnumer'
	dom(35)='Gripanumer'
	dom(36)='Lesid ur safnskra'
      ncp=noani
      ncp2=noprod
c.....Read files.......................... 
c.....lesa inn nyjustu product skra til ad fa burdardaga
      print *,'Lesa inn nyjustu product skra...'
      do i=1,noprod
        if(mod(i,10000).eq.0)Print *,'Read: ',i
        read(11,'(i4,i7,i4,2x,3(i4,i2))')pix,(cbd(j,i),j=1,6)
        prix(i)=pix(1)*100000000000.d0+pix(2)*10000.d0+pix(3)
      enddo
c.....lesa inn nyjustu Rmain skra til ad fa umhverfiskoda
      print *,'Lesa inn nyjustu Rmain skra ur afurdakeyrslu...'
      do i=1,ncp
        if(mod(i,10000).eq.0)Print *,'Read: ',i
        read(12,339)(prod(j,i),j=1,40)
        cpix(i)=prod(2,i)*100000000000.d0+prod(3,i)*10000.d0+
     +          prod(4,i)
      enddo
c...Buid ad lesa inn afurdaskra
c...Her hefst vinna vid skopulagsdoma
      print *,'Vinna hefst vid skopulagsdoma...'
      do i=1,norec
c       read(10,210)(idom(k),k=1,36)
c...Nytt format a conf.gogn.???? skra 2.11.12
        read(10,210)(idom(k),k=1,7),(idom(k),k=8,25),
     +(idom(k),k=26,28),(idom(k),k=29,31),(idom(k),k=32,33),
     +(idom(k),k=34,36)
        if(mod(i,1).eq.0)write(*,210)(idom(k),k=1,36)
c...skrifa ut villuiur
        if(idom(7).lt.150)write(9,*)'Bandmal: ',idom(7),' lina: ',i
        do k=8,14
         if(idom(k).lt.0.or.idom(k).gt.10)write(9,*)dom(k),idom(k),'>',i
        enddo
        if(idom(15).lt.10.or.idom(15).gt.20)
     +  write(9,*)dom(15),idom(15),'>',i
        if(idom(16).lt.0.or.idom(16).gt.5)
     + write(9,*)dom(16),idom(16),'>',i
        do k=17,33
         if(idom(k).lt.0.or.idom(k).gt.9)write(9,*)dom(k),idom(k),'>',i
        enddo
        do k=8,33
          stdo(k,idom(k))=stdo(k,idom(k))+1
        enddo
        if(idom(19).eq.0.and.idom(32).gt.0)print *,i
        if(idom(33).eq.0.and.idom(32).gt.0)print *,i
c...Setja domsmanud a mai ef hann vantar
        if(idom(5).eq.0)idom(5)=5
c...Villusiur enda
c...Finna gripinn i afurdaskra og athuga hvort um er ad raeda 1 mjsk
c...og saekja umhverfiskoda
c... Locate cow in main file
	ap=(idom(1))*100000000000.d0+idom(2)*10000.d0+idom(3)
	ap2=(idom(1))*100000000000.d0+idom(2)*10000.d0+idom(3)
c       if(idom(1).eq.1990.and.idom(2).eq.1331941)
c    +write(*,'(2f20.0)')ap,ap2
	if(binsok(cpix,ncp,ap,pl))then
c       if(idom(1).eq.1990.and.idom(2).eq.1331941)
c    +write(*,'(3f20.0)')ap,ap2,cpix(pl)
c...Saekir aldur vid fyrsta burd ur Rmain skranni
c...ef aldursflokkur=0 tha er hann settur a 6 eda aldur=27 manudir (medaltal)
c         idom(38)=prod(24,pl)
          idom(38)=prod(20,pl)
          if(idom(38).eq.0)then
             idom(38)=6
             print *,'aldursflokkur=0 ',idom(38)
             icnt=icnt+1
          endif
c...mjaltaskeid sett a 1
          idom(37)=1
           if(ap2.eq.199013319410107.)print *,ap,idom(37),idom(38)
c...her kannad hvort um 2 mjaltaskeid er ad raeda
	   if(binsok2(prix,ncp2,ap2,pl2))then
c       if(idom(1).eq.1990.and.idom(2).eq.1331941)
c    +write(*,'(f20.0,3(i4,i2))')prix(pl2),(cbd(j,pl2),j=1,6)
             if(cbd(3,pl2).gt.0)then
c              if(cbd(3,pl2).le.idom(4).and.cbd(4,pl2).le.idom(5))
               if(cbd(3,pl2).le.idom(4))then
                 idom(37)=2
c                idom(38)=prod(21,pl)
               endif
             endif
           endif
        else
c...ef kyrin finnst ekki i afurdaskra pa er mjolkurskeid sett
c...a 1, og aldur vid fyrsta burd a 27 manudi p.e. flokkur 6
          idom(37)=1
          idom(38)=6
        endif
        stat(1,idom(37))=stat(1,idom(37))+1
        stat(2,idom(38))=stat(2,idom(38))+1
c...skrifa i tvaer product skrar old og new domar
c...Athuga ad jugurlag+jugurfesta er sameinad i einn eiginleika
c...og somuleidis spenalengd+spenalag
c...BARA DOMAR FRAMKVAEMDIR 2000 EDA SIDAR: jun 2014
        if(idom(8).gt.0.and.idom(4).ge.2000)then
          write(21,221)idom(1),idom(2),idom(3),idom(4)-1900,idom(37),
     +    idom(38),idom(7),idom(8),idom(9),idom(10),idom(11)+idom(12),
     +    idom(13)+idom(14),idom(15),idom(16)
        endif
c 221 format(i4,i6,i4,i7,i4,i6,i4,i3,i1,i2,i3,7i2)
  221 format(i4,i7,i4,i3,i1,i2,i3,7i2)
  222 format(i4,i7,i4,i3,i1,i2,17i2)
c 222 format(i4,i6,i4,i7,i4,i6,i4,i3,i1,i2,17i2)
c....BARA DOMAR FRAMKVAEMDIR 2000 EDA SIDAR: jun 2014
        if(idom(17).gt.0.and.idom(4).ge.2000)then
          write(22,222)idom(1),idom(2),idom(3),idom(4)-1900,
     +    idom(37),idom(38),(idom(k),k=17,33)
        endif
c       write(20,220)rp
      enddo
      write(*,*)'Alls aldur=0', icnt
      close(10)
      close(20)
      write(9,*)'       Fjoldi i hverjum flokk  '
      write(9,*)'######################################'
      write(9,*)'Flokkur  Mjolkurskeid  Aldur v/1. burd'
      do i=1,20
        write(9,'(i7,i14,i16)')i,(stat(j,i),j=1,2)
      enddo
      write(9,*)'       Dreifing einkunna'
      write(9,*)'######################################'
      do i=9,34
        write(9,*)dom(i)
        do j=0,20
          write(9,'(i7,i10)')j,stdo(i,j)
        enddo
      enddo
      stop
      end
c############################################################
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
      logical function binsok2(a,n,t,pl)
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
	binsok2=.true.
	pl=min
      else
	binsok2=.false.
      endif

      return
      end
c     include '/home/agust/agusts/assub/nolli.f'
c     include '/home/agust/agusts/assub/nollmi.f'
c     include '/home/agust/agusts/assub/nollr.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nolli.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollmi.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr.f'
      include '/home/elsa/elsaagust/agusts/assub/nolli.f'
      include '/home/elsa/elsaagust/agusts/assub/nollmi.f'
      include '/home/elsa/elsaagust/agusts/assub/nollr.f'
