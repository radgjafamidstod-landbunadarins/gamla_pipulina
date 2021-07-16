c...LEITA AD Trait1.txt adur en keyrt er!!!!!!
c...Forritid deilir Buahrifum upp i Bua-Arahrif
c...og skrifar ut skrarnar sem filsplit.f og newprg.f gerdu adur
c...check class sizes when splitting all 1 lact in HY effects
c...print statistics
c...create classification table 
c...remember:DELETE RECORDS WHERE THERE ARE NO HERDMATES!!!
      program hysplit
c     parameter(noani=460443) !numver of animals in conf.Rmain
c     parameter(noherds= 892)
c     parameter(noyears= 22) ! add one plus every year
!     parameter(noani=473396)
!     parameter(noherds= 898)
!     parameter(noyears= 23)
!!    parameter(noani=485745)!conf.Rmain
!     parameter(noani=498812)!conf.Rmain
!     parameter(noherds= 904)!codeherd
!     parameter(noyears= 25)! add one every year
!     parameter(noani=508639)!conf.Rmain
!     parameter(noani=528849)!conf.Rmain
!     parameter(noani=539230)!conf.Rmain okt2017
!     parameter(noani=556303)!conf.Rmain mai2018
!     parameter(noani=573772)!conf.Rmain jan2019
!     parameter(noani=590776)!conf.Rmain okt2019
!     parameter(noani=601418)!conf.Rmain jan2020
!     parameter(noani=611767)!conf.Rmain jun2020
      parameter(noani=626832)!conf.Rmain jan2021
!     parameter(noherds= 906)!codeherd
!     parameter(noherds= 917)!codeherd
!     parameter(noherds= 920)!codeherd okt2017
!     parameter(noherds= 924)!codeherd mai2018
!     parameter(noherds= 925)!codeherd jan2019
!     parameter(noherds= 925)!codeherd okt2019
!     parameter(noherds= 926)!codeherd jan2020 
!     parameter(noherds= 926)!codeherd jun2020 
      parameter(noherds= 929)!codeherd jan2021 
!     parameter(noyears= 27)! add one every year 2017
!     parameter(noyears= 28)! add one every year 2018
!     parameter(noyears= 29)! add one every year 2019
!     parameter(noyears= 30)! add one every year 2020
      parameter(noyears= 31)! add one every year 2021
      integer fix(4),dist(noherds,noyears),sex,by
      integer tclass(noyears),itclass(noyears),index(noyears)
      integer x,hy(noani),srt(noani),nid(noani)
      integer hjord(noherds),hj,oic1
      real tr(17)
c     character pth*33
c     pth='/home/agust/skuggi_kyr/utli2/n17/'
!!    character pth*30
!!    pth='/home/LBHI/elsa/Kyr/utli2/n17/'
      character pth*25
      pth='/home/elsa/Kyr/utli2/n17/'
      call nolli(fix,4)
      call nolli(tclass,noyears)
      call nolli(itclass,noyears)
      call nolli(index,noyears)
      call nolli(hy,noani)
      call nolli(srt,noani)
      call nolli(nid,noani)
      call nolli(hjord,noherds)
      call nollmi(dist,noherds,noyears)
c...minimum number of herdmates
      x=2
      open(09,file=pth//'conf.Rmain')
      open(20,file=pth//'conf.Rhysmall.log')
      open(21,file=pth//'conf.Rhysplit.log')
c...athuga hvort rett er ad geyma thessar skrar
      open(25,file=pth//'conf.Rhydis.1st')
      open(30,file=pth//'impl/data/fixed.txt')
c...1:st LACTATION
      write(*,*)'This file contains herd-numbers where there are'
      write(*,*)'fewer than',x,' herdmates'
      write(*,*)'==============================================='
c   1 read(09,'(11x,i6,25x,i4,i3,i1,i2)',end=2)hj,fix
    1 read(09,'(10x,i7,36x,i4,i3,i1,i2)',end=2)hj,fix
c...Her er aukasia vegna einhverra ruglfaerslana ur huppa.is
        if(fix(2).lt.94.or.fix(2).gt.(93+noyears))then
          fix(1)=0
          fix(2)=0
          fix(3)=0
          fix(4)=0
        endif
        if(fix(2).gt.0)then
c         if(fix(2).lt.85)write(*,*)'asklfjaklf',hj,fix(2)
          fix(2)=fix(2)-93
          dist(fix(1),fix(2))=dist(fix(1),fix(2))+1
          hjord(fix(1))=hj
        endif
      goto 1
    2 continue
      do i=1,noherds
        write(25,'(4i3)')(dist(i,j),j=1,noyears)
c...ic1=# HY classes>0 for this herd
c...ic2=# HY classes>0 but less than x for this herd
c...ict=# mates in total for this herd
c...icf=index for first HY class for this herd
c...tclass=temporary vector for numbers in HY-class for this herd
c...itclass=vector with same cells filled as tclass where each cell contains
c...        class level for particular year (i.e. filled cell)
        ic1=0
        ic2=0
        ict=0
        icf=0
        do k=1,noyears
          tclass(k)=0
          itclass(k)=0
          index(k)=0
        enddo
c...count possible HY classes for this herd
c......count HY classes larger than 2 for this herd
        do j=1,noyears
          ict=ict+dist(i,j)
          if(dist(i,j).gt.0)then
            ic1=ic1+1
            itclass(ic1)=j
            index(j)=ic1
          endif
          if(dist(i,j).gt.0.and.dist(i,j).lt.x)ic2=ic2+1
          tclass(j)=dist(i,j)
        enddo
c...if all HY classes are "legal" then put "year" in classify table
        if(ic2.eq.0)then
          ic3=0
          do j=1,noyears
            if(dist(i,j).gt.0)ic3=ic3+1
          enddo
          if(ic3.ne.ic1)then
            print *,'ic3.ne.ic1!!! STOP',ic1,ic3
            stop
          endif
c...actions for HY classes are not "legal" 
        else
            write(21,*)'Before ...'
            write(21,*)'HERD: ',hjord(i),' kode',i,' ic1<>ic2 ',ic1,ic2
            write(21,*)'   dist: ',(dist(i,kk),kk=1,noyears)
            write(21,*)' tclass: ',tclass
            write(21,*)'itclass: ',itclass
            write(21,*)'  index: ',index
c...if total number of mates all years is less than x!
          if(ict.lt.x)then
            print *,'Whoops...1'             
            write(20,*)'Herd',i,' has only',ict,' mates'
            do ik=1,noyears
              index(ik)=1
            enddo
          else             
c...check if # HY classes is more than 1!
            if(ic1.eq.1)then
              print *,'Whoops...2'             
              stop
            endif
c...check if last HY class has enough members
            oic1=ic1
            if(tclass(itclass(ic1)).lt.x)then
              tclass(itclass(ic1-1))=tclass(itclass(ic1-1))+
     +        tclass(itclass(ic1))
              tclass(itclass(ic1))=0
              index(itclass(ic1))=ic1-1
c             itclass(ic1)=0
              ic1=ic1-1
            endif
c...again!!check if last HY class has enough members
            if(tclass(itclass(ic1)).lt.x)then
            if(ic1.eq.1)then
              print *,'Whoops...3'             
              stop
            endif
              print *,'Last HY class is still too small!!!'
              tclass(itclass(ic1-1))=tclass(itclass(ic1-1))+
     +        tclass(itclass(ic1))
              tclass(itclass(ic1))=0
              index(itclass(ic1))=ic1-1
c             itclass(ic1)=0
              ic1=ic1-1
            endif
c...correct the index of old last class
            if(ic1.lt.oic1)then
              do k9=ic1+1,oic1
                index(itclass(k9))=index(itclass(ic1))
              enddo
            endif
            if(tclass(itclass(ic1)).lt.x)then
              print *,'****Last HY class is still too small!!STOP!****'
              stop
            endif
c...fra 1 to # HY classes here
c  k=1..4
c  itclass(1)= 6 itclass(2)=7...
c  index(6)=3...
            do k=1,ic1
c           print *,'if(tclass(itclass(k)).lt.x)then',tclass(itclass(k))
              if(tclass(itclass(k)).lt.x)then
              print *,'ic1=',ic1
              print *,'HY class k=',k,' [actual year ',itclass(k),'] 
     +has less than x members (',tclass(itclass(k)),')'
              print *,'...it has now index:',index(itclass(k))
         print *,'this will be changed to index:',index(itclass(k+1))
c...save old index to check if earlier HY classes have been assigned to 
c...this old one
                ioi=index(itclass(k))
                tclass(itclass(k+1))=tclass(itclass(k+1))+
     +                               tclass(itclass(k))
                index(itclass(k))=index(itclass(k+1))
                tclass(itclass(k))=0
c               itclass(k)=0
              endif 
              if(k.gt.2)then
                do k2=1,ic1
c      print *,'k=',k,' ioi=',ioi,'k2=',k2,' idex',index(itclass(k2))
                  if(index(itclass(k2)).eq.ioi)index(itclass(k2))=
     +            index(itclass(k))
                enddo
              endif
            enddo
            write(21,*)'...after'
            write(21,*)' tclass: ',tclass
            write(21,*)'itclass: ',itclass
            write(21,*)'  index: ',index
c           read(*,*)
          endif
        endif
c...reuse dist for the index
        do ij=1,noyears
          dist(i,ij)=index(ij)
        enddo
      enddo
      rewind(09)
c...code HY effect
c             STOP
        id=0
    3 read(09,'(53x,i4,i3,i1,i2)',end=4)fix
        ihy=0
        id=id+1
c...Her er aukasia vegna einhverra ruglfaerslana ur huppa.is
        if(fix(2).lt.94.or.fix(2).gt.(93+noyears))then
          fix(1)=0
          fix(2)=0
          fix(3)=0
          fix(4)=0
        endif
        if(fix(2).gt.0)then
          fix(2)=fix(2)-93
          ihy=fix(1)*100+dist(fix(1),fix(2))
c         print *,fix(1),fix(2),dist(fix(1),fix(2)),ihy
        endif
        hy(id)=ihy
      goto 3
    4 continue
      if(id.ne.noani)then
        print *,'Wrong # animals 1:st Lactation',id,noani
        STOP
      endif
c...sort vectors 1:st
      n=noani
      do i=1,noani
        srt(i)=hy(i)
        nid(i)=i
      enddo
      call sort2b(n,srt,nid)
c...recode
      ihy=0
      iohy=0
      do i=1,noani
        if(srt(i).gt.iohy)then
          iohy=srt(i)
          ihy=ihy+1
        endif
        srt(i)=ihy
      enddo
      call sort2b(n,nid,srt)
      do i=1,noani
        hy(i)=srt(i)
      enddo
      close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      rewind(09)
      open(21,file=pth//'impl/data/trait.txt')
      open(26,file=pth//'impl/data/birthy.txt')
      open(27,file=pth//'impl/data/sex.txt')
      do i=1,noani
        read(09,'(6x,i4,41x,i2,i4,i3,i1,i2,17f2.0)')
     +  by,sex,fix,tr
c...Her er aukasia vegna einhverra ruglfaerslana ur huppa.is
        if(fix(2).lt.94.or.fix(2).gt.(93+noyears))then
          fix(1)=0
          fix(2)=0
          fix(3)=0
          fix(4)=0
          do kk=1,17
            tr(kk)=0.
          enddo
        endif
        if(fix(2).gt.0)fix(2)=fix(2)-93
c       write(30,'(i4,i6,i1,i2)')fix(1),hy(i),fix(3),fix(4) 
c....old newprg starts
        iath=0
c...her er aukasia vegna bandmals sem er einhver rugltala i sumum tilfellum
c...her er tha breytt testi thannig ad nullad ut ef eiginl 2-8 eru i einhverjum tilfellum
c...null. 
        do j=1,17
          if(tr(j).eq.0)iath=1
        enddo
        if(iath.eq.1)then
          do j=1,17
            tr(j)=-999.
          enddo
          do j=1,4
            fix(j)=0
          enddo
        endif
        write(30,'(i4,i6,i1,i2)')fix(1),hy(i),fix(3),fix(4) 
        write(21,'(17f5.0)')(tr(k),k=1,17)
c...old filsplit.f
        write(26,'(i4)')by
        write(27,'(i1)')sex
c.........old newprg ends
      enddo
      close(09)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      close(26)
      close(27)
      close(30)
      stop
      end
c     include '/home/agust/agusts/assub/sorting.f'
c     include '/home/agust/agusts/assub/nolli.f'
c     include '/home/agust/agusts/assub/nollmi.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/sorting.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nolli.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollmi.f'
      include '/home/elsa/elsaagust/agusts/assub/sorting.f'
      include '/home/elsa/elsaagust/agusts/assub/nolli.f'
      include '/home/elsa/elsaagust/agusts/assub/nollmi.f'
