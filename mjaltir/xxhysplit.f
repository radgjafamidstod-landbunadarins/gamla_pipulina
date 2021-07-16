C...LEITA AD Trait1.txt adur en keyrt er!!!!!!
c...Forritid deilir Buahrifum upp i Bua-Arahrif
c...og skrifar ut skrarnar sem filsplit.f og newprg.f gerdu adur
c...check class sizes when splitting all 1 lact in HY effects
c...print statistics
c...create classification table 
c...remember:DELETE RECORDS WHERE THERE ARE NO HERDMATES!!!
      program hysplit
c     parameter(noani=453604)
c     parameter(noherds=1139)
!     parameter(noani=466316)
!     parameter(noherds=1140)
!!    parameter(noani=478547)
!!    parameter(noherds=1143)
!     parameter(noani=491304)
!     parameter(noani=500881)
!     parameter(noani=515914)
!     parameter(noani=526154)
!     parameter(noani=542634)
!     parameter(noani=560012)
!     parameter(noani=576353)
!     parameter(noani=586577)
!     parameter(noani=596523)
      parameter(noani=611148)
!     parameter(noherds=1143)
!     parameter(noherds=1142)
!     parameter(noherds=1145)
!     parameter(noherds=1146)
!     parameter(noherds=1151)
!     parameter(noherds=1150)
!     parameter(noherds=1149)
!     parameter(noherds=1150)
!     parameter(noherds=1151)
      parameter(noherds=1153)
!     parameter(noyears= 34)
!     parameter(noyears= 35)
!     parameter(noyears= 36)
!     parameter(noyears= 37)
!     parameter(noyears= 38)
      parameter(noyears= 40)
      integer fix(2),dist(noherds,noyears),sex,by
      integer tclass(noyears),itclass(noyears),index(noyears)
      integer x,hy(noani),srt(noani),nid(noani)
      integer hjord(noherds),hj
      real tr(2)
      call nolli(fix,2)
      call nollmi(dist,noherds,noyears)
      call nolli(index,noyears)
      call nolli(itclass,noyears)
      call nolli(tclass,noyears)
      call nolli(hy,noani)
      call nolli(srt,noani)
      call nolli(nid,noani)
      call nolli(hjord,2)
      call nollr4(tr,2)
      sex=0
      by=0
      hj=0
      x=0
      open(09,file='vinnsla/mjalt.Rmain')
      open(30,file='data/fixed.txt')
      open(21,file='data/trait.txt')
      open(26,file='data/birthy.txt')
      open(27,file='data/sex.txt')
      do i=1,noani
        read(09,'(6x,i4,41x,i2,8x,f5.0,f4.0)')
     +  by,sex,tr
        if(tr(1).gt.0)then
          fix(1)=1
        else
         fix(1)=0
        endif
        if(tr(2).gt.0)then
          fix(2)=1
        else
         fix(2)=0
        endif
        write(30,'(2i2)')fix(1),fix(2) 
c....old newprg starts
        if(tr(1).eq.0)tr(1)=-9.
        if(tr(2).eq.0)tr(2)=-9.
        write(21,'(2f5.0)')tr(1),tr(2)
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
c     include '/home/agust/agusts/assub/nollr4.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/sorting.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nolli.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollmi.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr4.f'
      include '/home/elsa/elsaagust/agusts/assub/sorting.f'
      include '/home/elsa/elsaagust/agusts/assub/nolli.f'
      include '/home/elsa/elsaagust/agusts/assub/nollmi.f'
      include '/home/elsa/elsaagust/agusts/assub/nollr4.f'
