c...jun 2014: Reiknad med utliti breyttum vaegjum
c...jugur, mjaltir og skap bara ut fra linulega n17
c...skrokkur og spenar ut fra gomlu blondu g8 og n17
c...allt skorid vid domaar 2000 og seinna.
c...skrokk og spena tharf ad skoda betur - vaegisstudlar a
c...eiginleikum med optimum annad en 9 og eins hvernig a ad
c...skilgreina skrokkinn ut fra bara N17.
c     program bassi
c.......................................................................
c     Description: Gathers information on the evaluations and accuracy
c                  for each animal and builds the files HUPPA_BV.F02,
c                  containing all breeeding values for all animals, and
c                  HUPPA_AC.F02 containing all accuracy statistics
c.......................................................................
c     Input:       
c     Output:      
c.......................................................................
c     Reference: 
c.......................................................................
c     Written august 2002;modified april 2009
c     Agust Sigurdsson, Farmers Association of Iceland;AUI
c     Ny heildareinkunn September 2019
c-----------------------------------------------------------------------
      parameter(noani =700000)
      character*16 numer
      integer*4 pl,im(4)
      real*8 ianiA(noani),ianiX,ianiM
      real facc
      integer tafla_bv(noani,52),taflaX_bv(17),vigur_bv(15)
      real tafla_ix(noani,15),hei(2)
      integer tafla_ac(noani,16),taflaX_ac(3),vigur_acc(3)
	  logical naut(noani)
c=======================Yfirlit um toflur===============
c......tafla_bv:
c       1..15:Afurdir
c      16..18:Frjosemi
c      19..21:Frumur
c      22..24:Mjolkuruthald
c      25..32:Utlit gamla
c      33..49:Utlit nyja
c          50:Mjaltarod
c          51:Gaedarod
c          52:Ending (bara fyrir naut i rauninni m.v. apr 2009)
c......tafla_ix:
c       1.. 5:Afurdir
c           6:Afurdir (eigin)
c           7:Afurdir (heild)
c           8:Frjosemi
c           9:Frumur
c          10:Skrokkur
c          11:Jugur
c          12:Spenar
c          13:Mjaltir
c          14:Skap
c          15:Heildareinkunn
c......tafla_ac:
c       1.. 3:Afurdir (Fjoldi, fjoldi med gogn, oryggis%)
c       4.. 6:Frumur -do-
c       7.. 9:Utlit gamla -do-
c      10..12:Utlit nyja -do-
c      13..15:Mjaltir/gaedi -do-           
c      16    :Ending fjoldi daetra med gogn          
c===========================================================
      logical binsok
c     character pth1*23
!!    character pth1*20
      character pth1*15
      ianiX=0.
      pl=0
c     pth1='/home/agust/skuggi_kyr/'
      pth1='/home/elsa/Kyr/'
!	  pth1='/home/jonhjalti/gogn/bassaprof/'
!!    pth1='/home/LBHI/elsa/Kyr/'
      open(11,file=pth1//'mjolk/vinnsla/Rmain')
!      open(13,file=pth1//'frumur/vinnsla/Rmain')
      open(14,file=pth1//'utli2/g8/conf.Rmain')
      open(15,file=pth1//'utli2/n17/conf.Rmain')
      open(16,file=pth1//'mjaltir/vinnsla/mjalt.Rmain')
!     open(17,file=pth1//'index/lausnir/nov13/nautskrE.n13') !ending BHB
!     open(17,file=pth1//'index/lausnir/mai2016/nautskrEs.m16') !ending BHB
!     open(17,file=pth1//'index/lausnir/jan2019/nautskrEs.j19') !ending BHB
!     open(17,file=pth1//'index/lausnir/jan2019/nautskrEs.j19') !ending BHB
!     open(17,file=pth1//'index/lausnir/jan2020/nautskrEs.j20') !ending BHB
      open(17,file=pth1//'index/lausnir/jan2021/nautskrEs.j21') !ending BHB
!     open(17,file=pth1//'index/lausnir/jun2020/nautskrEs.jun20') !ending BHB
!      open(21,file=pth1//'mjolk/vinnsla/rescaled.sol')
      open(21,file=pth1//'tdm/afurdir/tdmebv.txt')
c...........breyta etv i rescaled.sol???????
      open(22,file=pth1//'mjolk/frjos/vinnsla/rescaleB.sol')
!      open(23,file=pth1//'frumur/vinnsla/rescaled.sol')
      open(23,file=pth1//'tdm/afurdir/scsebv.txt')
      open(24,file=pth1//'utli2/g8/rescaled.so_base2008') !skolun fra 2008!
      open(25,file=pth1//'utli2/n17/rescaled.sol')
      open(26,file=pth1//'mjaltir/rescaled.sol')
	  open(27,file=pth1//'tdm/afurdir/perebv.txt')
!      open(31,file=pth1//'mjolk/impl/acc/accuracy.sol')
      open(31,file=pth1//'tdm/afurdir/accuracy.sol')
!      open(33,file=pth1//'frumur/acc/accuracy.sol')
      open(33,file=pth1//'tdm/afurdir/accuracy_f.sol')
      open(34,file=pth1//'utli2/g8/impl/acc/accuracy.sol')
      open(35,file=pth1//'utli2/n17/impl/acc/accuracy.sol')
      open(36,file=pth1//'mjaltir/acc/accuracy.sol')
c     open(40,file=pth1//'index/lausnir/huppa_bv.a09',status='new')
c     open(41,file=pth1//'index/lausnir/huppa_ix.a09',status='new')
c     open(42,file=pth1//'index/lausnir/huppa_ac.a09',status='new')
c     open(44,file=pth1//'index/lausnir/bassi.n14',status='new')
c     open(44,file=pth1//'index/lausnir/bassi.m15',status='new')
!!    open(44,file=pth1//'index/lausnir/bassi.n15',status='new')
!     open(44,file=pth1//'index/lausnir/bassi.m16',status='new')
!     open(44,file=pth1//'index/lausnir/bassi.n16',status='new')
!     open(44,file=pth1//'index/lausnir/bassi.o19',status='new')
!     open(44,file=pth1//'index/lausnir/bassi.o19e',status='new')
!     open(44,file=pth1//'index/lausnir/bassi.j20',status='new')
!     open(44,file=pth1//'index/lausnir/bassi.j20e',status='new')
!     open(44,file=pth1//'index/lausnir/bassi.j20ee',status='new') !26022020
!     open(44,file=pth1//'index/lausnir/bassi.j20eee',status='new') !Mars2020
!     open(44,file=pth1//'index/lausnir/bassi.jun20_uniq',status='new') !Jun2020
!     open(44,file=pth1//'index/lausnir/bassi.jun20_auniq',status='new') !Jun2020
!     open(44,file=pth1//'index/lausnir/bassi.sep20',status='new') !Sep2020
!     open(44,file=pth1//'index/lausnir/bassi.jan21',status='new') !Jan2021
!     open(44,file=pth1//'index/lausnir/bassi.jan21NM',status='new') !Jan2021
      open(44,file=pth1//'index/lausnir/bassi.jan21NMb',status='new') !Jan2021
c ----+----+ ...forsnid nautaskraa... +----+----+----+----+
  111 format(i6,i4,i7,i4)
  113 format(i6,i4,i7,i4)
  114 format(i6,i4,i7,i4)
  116 format(i6,i4,i7,i4)
  117 format(i4,i7,i4,2i3)
  121 format(i4,i7,i4,1x,15i4,2f4.0)
  122 format(3i4,f4.0)
  123 format(i4,i7,i4,4i4)
  124 format(8i4)
  125 format(17i4)
  126 format(2i4)
  127 format(i6,i4,i7,36x,3i4)
  131 format(6x,4x,5x,i7,6x,i6,f7.2)
  240 format(a15,48i4)
  241 format(15i4)
c 242 format(15i4)
  242 format(10i4)
  244 format(a15,52i4,15i4,11i4)
c ----+----+ ...nullstilla ----+----+
      print *,'----------Nullstilla----------'
      do j=1,17
        taflaX_bv(j)=0
      enddo
      do j=1,3
        taflaX_ac(j)=0
      enddo
      do i=1,noani
        do j=1,48
          tafla_bv(i,j)=0
        enddo
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c...VEGNA ENDINGAR SEM ER BARA TIL FYRIR NAUT ALLT SETT A 100 (apr 2009)
        tafla_bv(i,52)=100
		naut(i)=.FALSE.
        do j=1,16
          tafla_ac(i,j)=0
        enddo
        do j=1,15
          tafla_ix(i,j)=0.
        enddo
      enddo
c ----+----+ ...les AFURDA-skrar inn i minni ----+----+
      print *,'----------AFURDIR----------'

! Les her Rmain skrana fyrir afurdir ur gamla og kynbotamatid fyrir frjosemi.
!Utskrain midast vid ta gripi sem eru i tessari skra.
	  
      i=1
    1 read(11,111,end=199)im
!    1  read(21,121,end=199)im(2),im(3),im(4),(tafla_bv(i,j),j=1,15),
!     +    (tafla_ix(i,k),k=6,7)
 	   if(mod(i,10000).eq.0)print *,'...les ',i
        ianiA(i)=
     +  im(2)*100000000000.d0+im(3)*10000.d0+im(4)
        read(22,122)(tafla_bv(i,j),j=16,18),tafla_ix(i,8)
        i=i+1
		goto 1
! Les inn ur maelidagalikansskranum.
  199 print *,'Alls lesnir gripir ur AFURDA-skram: ',i-1
      inA=i-1
	  do
		read(31,131,end=198)(vigur_acc(j),j=1,2),facc
        vigur_acc(3)=int((facc*100.)+0.5)
	    read(21,121)im(2),im(3),im(4),(vigur_bv(j),j=1,15),
     +    hei(1),hei(2)

        ianiM=
     +  im(2)*100000000000.d0+im(3)*10000.d0+im(4)
	    if(binsok(ianiA,inA,ianiM,pl))then
		  do j=1,15
		    tafla_bv(pl,j)=vigur_bv(j)
		  enddo
          tafla_ix(pl,6)=hei(1)
          tafla_ix(pl,7)=hei(2)
		  do j=1,3
		    tafla_ac(pl,j)=vigur_acc(j)
		  enddo
		else
!		  print *,'finn ekki grip ',ianiM, 'i gamla kerfi'
		endif
	  enddo
		
		
  198 continue 
      
CCCCCCCCCCCC
c     goto 399
c ----+----+ ...les nautaskra FRUMUTOLU inn i minni ----+----+
      print *,'----------FRUMUR----------'
      i=1
      inMG0=0
    2 read(23,123)im(2),im(3),im(4),(taflaX_bv(l),l=1,4)
        ianiX=im(2)*100000000000.d0+im(3)*10000.d0+im(4)
!        read(23,123)(taflaX_bv(l),l=1,4)
        read(33,131,end=299)(taflaX_ac(j),j=1,2),facc
        taflaX_ac(3)=int((facc*100.)+0.5)
        if(binsok(ianiA,inA,ianiX,pl))then
          tafla_bv(pl,19)=taflaX_bv(1)
          tafla_bv(pl,20)=taflaX_bv(2)
          tafla_bv(pl,21)=taflaX_bv(3)
          tafla_ac(pl,4)=taflaX_ac(1)
          tafla_ac(pl,5)=taflaX_ac(2)
          tafla_ac(pl,6)=taflaX_ac(3)
          tafla_ix(pl,9)=taflaX_bv(4)
        else
c         print *,'...finn ekki grip ',ianiX,' i toflu AFURDA'
          inMG0=inMG0+1
c         stop
        endif
        i=i+1
      goto 2
  299 print *,'Alls lesnir gripir ur FRUMUTOLU-skram: ',i-1
      print *,'                 alls fundust ekki: ',inMG0
      inF=i-1
	  
c ----+----+ ...les mjolkurthol (mjolkuruthald) inn i minni ----+----+
      print *,'----------UTHALD----------'
      i=1
      inMG0=0
    3 read(27,123,end=399)im(2),im(3),im(4),(taflaX_bv(l),l=1,3)
        ianiX=im(2)*100000000000.d0+im(3)*10000.d0+im(4)
!        read(23,123)(taflaX_bv(l),l=1,4)
!        read(33,131,end=399)(taflaX_ac(j),j=1,2),facc
!        taflaX_ac(3)=int((facc*100.)+0.5)
        if(binsok(ianiA,inA,ianiX,pl))then
          tafla_bv(pl,22)=taflaX_bv(1)
          tafla_bv(pl,23)=taflaX_bv(2)
          tafla_bv(pl,24)=taflaX_bv(3)
!          tafla_ac(pl,4)=taflaX_ac(1)
!          tafla_ac(pl,5)=taflaX_ac(2)
!          tafla_ac(pl,6)=taflaX_ac(3)
!          tafla_ix(pl,9)=taflaX_bv(4)
        else
c         print *,'...finn ekki grip ',ianiX,' i toflu AFURDA'
          inMG0=inMG0+1
c         stop
        endif
        i=i+1
      goto 3
  399 print *,'Alls lesnir gripir ur FRUMUTOLU-skram: ',i-1
      print *,'                 alls fundust ekki: ',inMG0
      inF=i-1
	  
c ----+----+ ...les nautaskra SKOPULAG GAMLA inn i minni ----+----+
      print *,'----------UTLIT G----------'
      i=1
      inMG0=0
    4 read(14,114,end=499)im
        ianiX=im(2)*100000000000.d0+im(3)*10000.d0+im(4)
        read(24,124)(taflaX_bv(l),l=1,8)
        read(34,131)(taflaX_ac(j),j=1,2),facc
        taflaX_ac(3)=int((facc*100.)+0.5)
        if(binsok(ianiA,inA,ianiX,pl))then
          do j=1,8
            tafla_bv(pl,j+24)=taflaX_bv(j)
          enddo
          tafla_ac(pl,7)=taflaX_ac(1)
          tafla_ac(pl,8)=taflaX_ac(2)
          tafla_ac(pl,9)=taflaX_ac(3)
        else
c         print *,'...finn ekki grip ',ianiX,' i toflu AFURDA'
          inMG0=inMG0+1
c          stop
        endif
        i=i+1
      goto 4
  499 print *,'Alls lesnir gripir ur SKOPULAG GAMLA-skram: ',i-1
      print *,'                 alls fundust ekki: ',inMG0
      inSG=i-1
c ----+----+ ...les nautaskra SKOPULAG NYJA inn i minni ----+----+
      print *,'----------UTLIT N----------'
      i=1
      inMG0=0
    5 read(15,113,end=599)im
        ianiX=im(2)*100000000000.d0+im(3)*10000.d0+im(4)
        read(25,125)(taflaX_bv(l),l=1,17)
        read(35,131)(taflaX_ac(j),j=1,2),facc
        taflaX_ac(3)=int((facc*100.)+0.5)
        if(binsok(ianiA,inA,ianiX,pl))then
          do j=1,17
            tafla_bv(pl,j+32)=taflaX_bv(j)
          enddo
          tafla_ac(pl,10)=taflaX_ac(1)
          tafla_ac(pl,11)=taflaX_ac(2)
          tafla_ac(pl,12)=taflaX_ac(3)
        else
c         print *,'...finn ekki grip ',ianiX,' i toflu AFURDA'
          inMG0=inMG0+1
c          stop
        endif
        i=i+1
      goto 5
  599 print *,'Alls lesnir gripir ur SKOPULAG NYJA-skram: ',i-1
      print *,'                 alls fundust ekki: ',inMG0
      inSN=i-1
c ----+----+ ...les nautaskra MJALTIR inn i minni ----+----+
      print *,'----------MJALTIR----------'
      i=1
      inMG0=0
    6 read(16,116,end=699)im
        ianiX=im(2)*100000000000.d0+im(3)*10000.d0+im(4)
        read(26,126)(taflaX_bv(l),l=1,2)
        read(36,131)(taflaX_ac(j),j=1,2),facc
        taflaX_ac(3)=int((facc*100.)+0.5)
        if(binsok(ianiA,inA,ianiX,pl))then
          tafla_bv(pl,50)=taflaX_bv(1)
          tafla_bv(pl,51)=taflaX_bv(2)
          tafla_ac(pl,13)=taflaX_ac(1)
          tafla_ac(pl,14)=taflaX_ac(2)
          tafla_ac(pl,15)=taflaX_ac(3)
        else
          print '(a18,f16.0,a9)','...finn ekki grip ',ianiX,' i AFURDA'
          inMG0=inMG0+1
c         stop
        endif
        i=i+1
      goto 6
  699 print *,'Alls lesnir gripir ur MJALTA-skram: ',i-1
      print *,'                 alls fundust ekki: ',inMG0
      inMG=i-1
c ----+----+ ...les nautaskra ENDING inn i minni ----+----+
      print *,'----------ENDING-----------'
      i=1
      inMG0=0
    7 read(17,117,end=799)(im(j),j=1,3),taflaX_bv(1),taflaX_ac(1)
        ianiX=im(1)*100000000000.d0+im(2)*10000.d0+im(3)
        if(binsok(ianiA,inA,ianiX,pl))then
		  naut(pl)=.TRUE.
          tafla_bv(pl,52)=taflaX_bv(1)
          tafla_ac(pl,16)=taflaX_ac(1)
          write(*,'(f16.0,2i4)')ianix,taflaX_bv(1),taflaX_ac(1) 
        else
          print '(a18,f16.0,a9)','...finn ekki grip ',ianiX,' i AFURDA'
          inMG0=inMG0+1
c         stop
        endif
        i=i+1
      goto 7
  799 print *,'Alls lesnir gripir ur ENDINGAR-skram: ',i-1
      print *,'                 alls fundust ekki: ',inMG0
      print *,'                 gripur: ',ianiX
      inE=i-1
c ----+----+  ...reikna millieinkunnnir ----+----+
      do i=1,inA
c...mjolk
      tafla_ix(i,1)=
     +tafla_bv(i,1)*0.5+tafla_bv(i,2)*0.3+tafla_bv(i,3)*0.2
c...fitukg
      tafla_ix(i,2)=
     +tafla_bv(i,4)*0.5+tafla_bv(i,5)*0.3+tafla_bv(i,6)*0.2
c...proteinkg
      tafla_ix(i,3)=
     +tafla_bv(i,7)*0.5+tafla_bv(i,8)*0.3+tafla_bv(i,9)*0.2
c...fitu%
      tafla_ix(i,4)=
     +tafla_bv(i,10)*0.5+tafla_bv(i,11)*0.3+tafla_bv(i,12)*0.2
c...protein%
      tafla_ix(i,5)=
     +tafla_bv(i,13)*0.5+tafla_bv(i,14)*0.3+tafla_bv(i,15)*0.2
c...skrokkur
c......30% Bolur,Malir og Fotstada (Gamla); 10% yfirlina (Nyja)
      if(tafla_bv(i,26).ne.0.and.tafla_bv(i,27).ne.0.and.
     +tafla_bv(i,28).ne.0.and.tafla_bv(i,35).ne.0)then
        tafla_ix(i,10)=
     +  tafla_bv(i,26)*0.3+tafla_bv(i,27)*0.3+tafla_bv(i,28)*0.3+
     +  tafla_bv(i,35)*0.1
      else
        tafla_ix(i,10)=0.d0
      endif

!      jugurinkunn sept 2019
!     jugurfesta 35%
!     jugurband 15%
!     jugurdypt 50%
      if(tafla_bv(i,42).ne.0.and.
     + tafla_bv(i,43).ne.0.and.tafla_bv(i,44).ne.0)then
        tafla_ix(i,11)=
     +  tafla_bv(i,42)*0.35+tafla_bv(i,43)*0.15+
     +  tafla_bv(i,44)*0.5
      else
        tafla_ix(i,11)=0.d0
      endif


! Spenaeinkunn sept 2019
! Spenalengd (styttri) 30%
! Spenaþykkt (thynnri, einkunn snuid) 30%
! Spenastadsetning 40%
      if(tafla_bv(i,45).ne.0.and.
     + tafla_bv(i,46).ne.0.and.tafla_bv(i,47).ne.0)then
        tafla_ix(i,12)=
     +  tafla_bv(i,45)*0.3+
     +  (200-tafla_bv(i,46))*0.3+tafla_bv(i,47)*0.4
      else
        tafla_ix(i,12)=0.d0
      endif
c...mjaltir
c......30% mjaltir (Gamla); 30% mjaltir (Nyja) og 40% mjaltarod
      if(tafla_bv(i,48).ne.0.and.
     +tafla_bv(i,50).ne.0)then
        tafla_ix(i,13)=
     +  tafla_bv(i,48)*0.6+tafla_bv(i,50)*0.4
      else
        tafla_ix(i,13)=0.d0
      endif
c...skap
c......50% skap (Gamla og Nyja)
      if(tafla_bv(i,32).ne.0.and.tafla_bv(i,49).ne.0)then
c       tafla_ix(i,14)=
c    +  tafla_bv(i,32)*0.5+tafla_bv(i,49)*0.5
        tafla_ix(i,14)=tafla_bv(i,49)
      else
        tafla_ix(i,14)=0.d0
      endif
c...Heildareinkunn gomul
c......AFURDIR 65% (breytt i 60% 16/3 99; 55% feb 2003;44% okt 2005)
c......MJALTIR 10% (9% feb 2003; 8% okt 2005)
c......FRUMUR   5% (breytt i 10% 16/3 99;8% feb 2003)
c......GADAROD  4% (fellur ut 16/3 99)
c......JUGUR    4% (8% 16/3 99)
c......Ending   8% (8% feb 2003 - bara naut - allir gripir 100 annars )
c......FRJOSEMI 4% (8% okt 2005)
c......SPENAR   4% (8% okt 2005)
c......SKAP     4% (8% okt 2005)
c......ALLS   100
!Ny heildareinkunn september 2019
! NAUT
! Afurdir 36%
! Frjosemi 10%
! Frumutala 8%
! Jugur 10%
! Spenar 10%
! Mjaltir 8%
! Skap 8%
! Ending 10%


      if(tafla_ix(i,7).ne.0.and.tafla_ix(i,8).ne.0.and.
     +tafla_ix(i,9).ne.0.and.tafla_ix(i,11).ne.0.and.
     +tafla_ix(i,12).ne.0.and.tafla_ix(i,13).ne.0.and.
     +tafla_ix(i,14).ne.0.)then
	   if(naut(i))then
        tafla_ix(i,15)= tafla_ix(i,7)*0.36+   !afurdir
     +                  tafla_ix(i,8)*0.10+   !frjosemi
     +                  tafla_ix(i,9)*0.08+   !frumutala
     +                  tafla_ix(i,11)*0.10+  !jugur
     +                  tafla_ix(i,12)*0.10+  !spenar 
     +                  tafla_ix(i,13)*0.08+  !mjaltir
     +                  tafla_ix(i,14)*0.08+  !skap
     +                  tafla_bv(i,52)*0.10   !ending

       else 
	  
! KYR
! Afurdir 36%
! Frjosemi 11%
! Frumutala 9%
! Jugur 11%
! Spenar 13%
! Mjaltir 10%
! Skap 10%
! Ending 0%

        tafla_ix(i,15)= tafla_ix(i,7)*0.36+   !afurdir
     +                  tafla_ix(i,8)*0.11+   !frjosemi
     +                  tafla_ix(i,9)*0.09+   !frumutala
     +                  tafla_ix(i,11)*0.11+  !jugur
     +                  tafla_ix(i,12)*0.13+  !spenar 
     +                  tafla_ix(i,13)*0.10+  !mjaltir
     +                  tafla_ix(i,14)*0.10+  !skap
     +                  tafla_bv(i,52)*0.0    !ending

	   endif
      else
        tafla_ix(i,15)=0.d0
      endif

      enddo
c ----+-----+ ...done +----+----+----+----+----+----+
      do i=1,inA
        write(numer,'(f16.0)')ianiA(i)
        read(numer,'(a15)')numer

c...Sleppt ad skrifa ut heildarfjolda afkvaema

c....ALLT I EINA SKRA bassi.a09
        write(44,244)numer,(int(tafla_bv(i,j)),j=1,21),
     +(int(tafla_bv(i,j)),j=25,52),
     +(int(tafla_ix(i,j)+0.5),j=1,15),
     +(tafla_ac(i,j),j=2,3),
     +(tafla_ac(i,k),k=5,6),(tafla_ac(i,l),l=8,9),
     +(tafla_ac(i,m),m=11,12),(tafla_ac(i,n),n=14,16),
     +(int(tafla_bv(i,j)),j=22,24)
      enddo
c ----+-----+ ...done +----+----+----+----+----+----+
      print *,'            ...done'
      stop
      end

      logical function binsok(a,n,t,pl)
      integer n,pl,min,max,mitt
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
