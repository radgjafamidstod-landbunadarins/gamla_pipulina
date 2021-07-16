c-----------------------------------------------------------------------
      program accurac1
c.......................................................................
c     Description: Prepares three datafiles for the calculation of
c                  approximate accuracy of genetic evaluation under an
c                  animal model.
c.......................................................................
c           Input: fixed.txt
c                  ped.txt
c                  sex.txt
c          Output: accuraci.dat
c                  accuracs.dat
c                  accuracd.dat
c.......................................................................
c      References: Meyer, K. 1989. Approximate Accuracy of Genetic
c                  Evaluations under an Animal Model. Livestock
c                  Production Science, 21, 87-100.
c.......................................................................
c      Written by Agust Sigurdsson, Department of animal breeding and
c                 genetics. Swedish University of Agric. Sci., Uppsala.
c-----------------------------------------------------------------------
!!    parameter(noani = 485026)
!     parameter(noani = 497767)
!     parameter(noani = 507345)
!     parameter(noani = 522378)
!     parameter(noani = 532617) ! okt 2017
!     parameter(noani = 549098) ! mai 2018
!     parameter(noani = 566476) ! jan 2019
!     parameter(noani = 582817) ! okt 2019
!     parameter(noani = 593041) ! jan 2020
!     parameter(noani = 602987) ! jun 2020
      parameter(noani = 617610) ! jan 2021
      parameter(nofix =     3)

      integer*4 ix(noani),s(noani),d(noani),off1(noani),off2(noani),
     +           ps(noani)
      integer*2 fix(noani,nofix),sex(noani),hjord

      open(10,file='../data/fixed.txt',status='old')
      open(11,file='../data/ped.txt',status='old')
      open(12,file='../data/sex.txt',status='old')

      open(20,file='accuraci.dat',status='new')
      open(21,file='accuracs.dat',status='new')
      open(22,file='accuracd.dat',status='new')

      write(*,*)'Reading data...'
      do 199 i=1,noani
        read(10,'(i4,i6,i1,i2)')hjord,(fix(i,j),j=1,nofix)
c       read(10,*)(fix(i,j),j=1,nofix)
        read(11,*)ixxj,s(i),d(i)
        read(12,497)sex(i)
        ix(i)=i
        off1(i)=0
        off2(i)=0
  199 continue
  499 format(i4,12i2)
  498 format(3i7)
  497 format(i1)
      write(*,*)'        ...done'
      write(*,*)'Counting offsprings...'
      do 188 i=1,noani
        if(mod(i,1000).eq.0)then
           print *,'Working with animal:',i
        endif
        if(sex(i).eq.1)then
          do 187 is=1,noani
            if(s(is).eq.ix(i))then
              if(d(is).gt.0)then
                off1(i)=off1(i)+1
              else
                off2(i)=off2(i)+1
              endif
            endif
  187     continue
        else
          do 186 id=1,noani
            if(d(id).eq.ix(i))then
              if(s(id).gt.0)then
                off1(i)=off1(i)+1
              else
                off2(i)=off2(i)+1
              endif
            endif
  186     continue
        endif
  188 continue
      write(*,*)'               ...done'
      write(*,*)'Creating - accuraci.dat'
      do 179 i=1,noani
        if(fix(i,1).gt.0)then
          irec=1
        else
          irec=0
        endif
      write(20,399)irec,fix(i,1),fix(i,2),fix(i,3),
     +              ix(i),off1(i),
     +   off2(i),s(i),d(i),sex(i)
  179 continue
      write(*,*)'                ...done'
      write(*,*)'Creating - accuracs.dat'
      do 169 i=1,noani
  169   ps(i)=s(i)

      call sort2b(noani,ps,ix)

      do 168 i=1,noani
        if(s(ix(i)).gt.0)then
          if(fix(ix(i),1).gt.0)then
            irec=1
          else
            irec=0
          endif
        write(21,399)irec,fix(ix(i),1),fix(ix(i),2),fix(ix(i),3),
     +    ix(i)
     +    ,off1(ix(i)),off2(ix(i)),s(ix(i)),d(ix(i)),sex(ix(i))
        endif
  168 continue
      write(*,*)'                ...done'
      write(*,*)'Creating - accuracd.dat'
      do 159 i=1,noani
        ix(i)=i
        ps(i)=d(i)
  159 continue
      call sort2b(noani,ps,ix)

      do 158 i=1,noani
        if(d(ix(i)).gt.0)then
          if(fix(ix(i),1).gt.0)then
            irec=1
          else
            irec=0
          endif
        write(22,399)irec,fix(ix(i),1),fix(ix(i),2),fix(ix(i),3),
     +   ix(i),off1(ix(i)),
     +   off2(ix(i)),s(ix(i)),d(ix(i)),sex(ix(i))
        endif
  158 continue
      write(*,*)'                ...done'
  399 format(i1,i5,i1,i2,i6,2i4,2i6,i1)
      close(10)
      close(11)
      close(12)
      close(20)
      close(21)
      close(22)
      stop
      end

c     include '/home/agust/agusts/assub/sorting.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/sorting.f'
      include '/home/elsa/elsaagust/agusts/assub/sorting.f'
