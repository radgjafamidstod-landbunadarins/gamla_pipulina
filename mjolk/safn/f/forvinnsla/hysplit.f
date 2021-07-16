C...LEITA AD Trait1.txt adur en keyrt er!!!!!!
c...Forritid deilir Buahrifum upp i Bua-Arahrif
c...og skrifar ut skrarnar sem filsplit.f og newprg.f gerdu adur
c...check class sizes when splitting all 1 lact in HY effects
c...print statistics
c...create classification table 
c...remember:DELETE RECORDS WHERE THERE ARE NO HERDMATES!!!
      program hysplit
c     parameter(noani=453604)
!     parameter(noani=466316)mai15
!     parameter(noani=478547)!nov15ped.txt
!     parameter(noani=491303)!mai16ped.txt og sep16.ped
!     parameter(noani=500881)!nov16ped.txt
!     parameter(noani=515916)!mai17ped.txt
!     parameter(noani=515915)!mai17ped.txt
!     parameter(noani=526154)!okt17ped.txt
!     parameter(noani=542633)!mai18ped.txt
!     parameter(noani=560488)!mai+jan19ped.txt
!     parameter(noani=560120)!jan19ped.txt endurgert 21.1
!     parameter(noani=576472)!okt19ped.txt 
!     parameter(noani=576353)!okt19ped.txt endurgert
!     parameter(noani=586577)!jan20ped.txt 
!     parameter(noani=596523)!jun20ped.txt 
      parameter(noani=611146)!jan21ped.txt 
      parameter(notrt=3)
c     parameter(noherds=1342)
!     parameter(noherds=1351)mai15
!     parameter(noherds=1354)!nov15
!     parameter(noherds=1364)!Mai16 og sep2016
!     parameter(noherds=1353)!Nov16
!     parameter(noherds=1359)!mai17
!     parameter(noherds=1364)!okt17
!     parameter(noherds=1368)!mai18
!     parameter(noherds=1369)!jan19
!     parameter(noherds=1370)!okt19 og jan20
      parameter(noherds=1371)!jun20 og jan21
c     parameter(noyears=  33)
!     parameter(noyears=  34)!mai15
!     parameter(noyears=  34)!nov15
!     parameter(noyears=  35)!sep16 og nov16 og mai17
!     parameter(noyears=  36)!okt17 og mai18
!     parameter(noyears=  37)!mai18 og jan19 og okt19
!     parameter(noyears=  38)!jan20 
      parameter(noyears=  39)!jun20 og jan21
      integer fix(15),dist(noherds,noyears),sex,by
      integer tclass(noyears),itclass(noyears),index(noyears)
      integer x,hy(noani,notrt),srt(noani),nid(noani)
      integer hjord(noherds),hj,oic1
      real tr(15)
      logical nytt
c     character pth*29
!     character pth*26
      character pth*21
c     pth='/home/agust/skuggi_kyr/mjolk/'
!     pth='/home/LBHI/elsa/Kyr/mjolk/'
      pth='/home/elsa/Kyr/mjolk/'
c...minimum number of herdmates
      x=3
      nytt=.true.
      if(nytt)then
      open(09,file=pth//'vinnsla/Rmain')
      open(20,file=pth//'vinnsla/Rhysmall.log')
      open(21,file=pth//'vinnsla/Rhysplit.log')
c...athuga hvort rett er ad geyma thessar skrar
      open(25,file=pth//'vinnsla/Rhydis.1st')
      open(26,file=pth//'vinnsla/Rhydis.2nd')
      open(27,file=pth//'vinnsla/Rhydis.3rd')
      open(30,file=pth//'vinnsla/fixed.txt')
      else
      open(09,file='framl/Rmain')
      open(20,file='framl/Rhysmall.log')
      open(21,file='framl/Rhysplit.log')
c...athuga hvort rett er ad geyma thessar skrar
      open(25,file='framl/Rhydis.1st')
      open(26,file='framl/Rhydis.2nd')
      open(27,file='framl/Rhydis.3rd')
      open(30,file='framl/fixed.txt')
      endif
c...1:st LACTATION
      write(20,*)'This file contains herd-numbers where there are'
      write(20,*)'fewer than',x,' herdmates [1:st LACTATION]'
      write(20,*)'==============================================='
c...2000 vandi
  239 format(i6,3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
    1 read(09,'(52x,i6,i4,12i2)',end=2)hj,fix(1),(fix(k),k=4,15)
        fix(2)=fix(1)
        fix(3)=fix(1)
        if(fix(4).gt.0)then
          dist(fix(1),fix(4))=dist(fix(1),fix(4))+1
          hjord(fix(1))=hj
        endif
      goto 1
    2 continue
      do i=1,noherds
        write(25,'(15i3)')(dist(i,j),j=1,noyears)
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
c...2000 Vandi
    3 read(09,'(58x,i4,12i2)',end=4)fix(1),(fix(k),k=4,15)
        fix(2)=fix(1)
        fix(3)=fix(1)
        ihy=0
        id=id+1
        if(fix(4).gt.0)then
          ihy=fix(1)*100+dist(fix(1),fix(4))
c         print *,fix(1),fix(4),dist(fix(1),fix(4)),ihy
        endif
        hy(id,1)=ihy
      goto 3
    4 continue
      if(id.ne.noani)then
        print *,'Wrong # animals 1:st Lactation',id,noani
        STOP
      endif
c...2:st LACTATION
c...zero vectors
      rewind(09)
      do i=1,noherds
        do j=1,noyears
          dist(i,j)=0
          index(j)=0
          tclass(j)=0
          itclass(j)=0
        enddo
      enddo
      write(20,*)'This file contains herd-numbers where there are'
      write(20,*)'fewer than',x,' herdmates [2:nd LACTATION]'
      write(20,*)'==============================================='
c... 2000 vandi
   21 read(09,'(58x,i4,12i2)',end=22)fix(1),(fix(k),k=4,15)
        fix(2)=fix(1)
        fix(3)=fix(1)
        if(fix(5).gt.0)then
          dist(fix(2),fix(5))=dist(fix(2),fix(5))+1
        endif
      goto 21
   22 continue
      do i=1,noherds
        write(26,'(15i3)')(dist(i,j),j=1,noyears)
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
            write(21,*)'HERD: ',i,' ic1<>ic2 ',ic1,ic2
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
c...2000 vandi
   23 read(09,'(58x,i4,12i2)',end=24)fix(1),(fix(k),k=4,15)
        fix(2)=fix(1)
        fix(3)=fix(1)
        ihy=0
        id=id+1
        if(fix(5).gt.0)then
          ihy=fix(2)*100+dist(fix(2),fix(5))
c         print *,fix(1),fix(4),dist(fix(1),fix(4)),ihy
        endif
        hy(id,2)=ihy
      goto 23
   24 continue
      if(id.ne.noani)then
        print *,'Wrong # animals 2:nd Lactation',id,noani
        STOP
      endif
c...3:rd LACTATION
c...zero vectors
      rewind(09)
      do i=1,noherds
        do j=1,noyears
          dist(i,j)=0
          index(j)=0
          tclass(j)=0
          itclass(j)=0
        enddo
      enddo
      write(20,*)'This file contains herd-numbers where there are'
      write(20,*)'fewer than',x,' herdmates [3:rd LACTATION]'
      write(20,*)'==============================================='
c...2000 vandi
   31 read(09,'(58x,i4,12i2)',end=32)fix(1),(fix(k),k=4,15)
        fix(2)=fix(1)
        fix(3)=fix(1)
        if(fix(6).gt.0)then
          dist(fix(3),fix(6))=dist(fix(3),fix(6))+1
        endif
      goto 31
   32 continue
      do i=1,noherds
        write(27,'(15i3)')(dist(i,j),j=1,noyears)
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
            write(21,*)'HERD: ',i,' ic1<>ic2 ',ic1,ic2
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
c...2000 vandi
   33 read(09,'(58x,i4,12i2)',end=34)fix(1),(fix(k),k=4,15)
        fix(2)=fix(1)
        fix(3)=fix(1)
        ihy=0
        id=id+1
        if(fix(6).gt.0)then
          ihy=fix(3)*100+dist(fix(3),fix(6))
c         print *,fix(1),fix(4),dist(fix(1),fix(4)),ihy
        endif
        hy(id,3)=ihy
      goto 33
   34 continue
      if(id.ne.noani)then
        print *,'Wrong # animals 3:rd Lactation',id,noani
        STOP
      endif
c...sort vectors 1:st
      n=noani
      do j=1,notrt
      do i=1,noani
        srt(i)=hy(i,j)
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
        hy(i,j)=srt(i)
      enddo
      enddo
      close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      close(26)
      close(27)
      rewind(09)
      if(nytt)then
      open(21,file=pth//'vinnsla/trait1.txt')
      open(22,file=pth//'vinnsla/trait2.txt')
      open(23,file=pth//'vinnsla/trait3.txt')
      open(24,file=pth//'vinnsla/trait4.txt')
      open(25,file=pth//'vinnsla/trait5.txt')
      open(26,file=pth//'vinnsla/birthy.txt')
      open(27,file=pth//'vinnsla/sex.txt')
      else
      open(21,file='framl/trait1.txt')
      open(22,file='framl/trait2.txt')
      open(23,file='framl/trait3.txt')
      open(24,file='framl/trait4.txt')
      open(25,file='framl/trait5.txt')
      open(26,file='framl/birthy.txt')
      open(27,file='framl/sex.txt')
      endif
      do i=1,noani
c...2000 VANDI
        read(09,'(6x,i4,41x,i1,6x,i4,12i2,3f5.0,6f5.2,6f4.2)')
     +by,sex,fix(1),(fix(k),k=4,15),tr
        fix(2)=fix(1)
        fix(3)=fix(1)
        write(30,'(i4,3i6,12i3)')fix(1),(hy(i,k),k=1,3),(fix(l),l=4,15) 
c....old newprg starts
        do j=1,15
          if(tr(j).eq.0)tr(j)=-999.
        enddo
        write(21,'(3f8.0)')(tr(k),k=1,3)
        write(22,'(3f8.2)')(tr(k),k=4,6)
        write(23,'(3f8.2)')(tr(k),k=7,9)
        write(24,'(3f8.2)')(tr(k),k=10,12)
        write(25,'(3f8.2)')(tr(k),k=13,15)
c...old filsplit.f
c...2000 VANDI
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
!     include '/home/LBHI/elsa/elsaagust/agusts/assub/sorting.f'
      include '/home/elsa/elsaagust/agusts/assub/sorting.f'
