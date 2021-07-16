c-----------------------------------------------------------------------
      program accurac2
c.......................................................................
c     Description: Program for computing approximate accuracy of genetic
c                  evaluations under an animal model.
c.......................................................................
c           Input: accuraci.dat - sorted on random effect 1 (animal)
c                  accuracs.dat - sorted on sire (only sire > 0 records)
c                  accuracd.dat - sorted on dam (only dam > 0 records)
c          Output: accuracy.sol - i,ire,ip,not,(nrt(j,i),j=1,2),rai,sdt
c.......................................................................
c      References: Meyer, K. 1989. Approximate Accuracy of Genetic
c                  Evaluations under an Animal Model. Livestock
c                  Production Science, 21, 87-100.
c.......................................................................
c     Written: may 1990 by Thorvaldur Arnason, for Standardbred trotters
c     where the data structure is the same as for blup animal model with
c     indirec approach.
c.......................................................................
c     Modified: marz 1992 by Agust Sigurdsson, Department of animal
c     breeding and genetics. Swedish University of Agric. Sci., Uppsala,
c     to be used for Icelandic cattles with the implicit data structure.
c.......................................................................
c     Variables:
c     nt=number of traits to be run separately
c     nht=number of animals in total
c     nf1=number of classes for fixed effect 1
c     nf2=number of classes for fixed effect 2
c     ire=1 if animal has own record else ire=0
c     ind(nht)=consequtive number of individual
c     sire(nht)=consequtive number of sire
c     dam(nht)=consequtive number of dam
c     ifix1=fixed effect 1
c     ifix2=fixed effect 2  = always 1 in this example
c     no1=number of progeny of the animal with the other parent known
c     no2=number of progeny of the animal with the other parent unknown
c     h2=heritability
c     sdg=genetic SD
c     nrt=record types of offspring
c       1= record present
c       0= no record on animal
c     rui is unadjused predicted accuracy for each animal
c     rai is approximate adjusted accuracy for evaluation of each animal
c     sdt is approximate standard error of the index value, i.e. probab.
c         disribution of true genetic values given an index value.
c.......................................................................
c     Subroutines:
c       nollmi.f = subroutine nollmi - sets integer matrix to zero
c       nollr.f = subroutine nollr - sets real*8 vector to zero
c-----------------------------------------------------------------------
      parameter(    nt =        1)
      parameter( nofix =        1)
c     parameter(   nht =   453604)
!     parameter(   nht =   466316)
!!    parameter(   nht =   478547)
!     parameter(   nht =   491304)
!     parameter(   nht =   500881)
!     parameter(   nht =   515914)
!     parameter(   nht =   542634)
!     parameter(   nht =   560012)
!     parameter(   nht =   576353)
!     parameter(   nht =   586577)
!     parameter(   nht =   596523)
      parameter(   nht =   611148)
      parameter(    nf =        1)
      integer nfi(1),nh(nf,nofix),nrt(2,nht),ifix(nofix)
     + ,lfix(nofix)
      real*8 d0(nht),d1(nht),d2(nht),d3(nht)
      real*8 h2,sdg,d,q,lambda,sum1,sum2,rui,rai,sdp,sdt
      real*8 d2xs,d2xd,d0d,d1d,d0s,d1s
      nfi(1)=nf
      h2=0.20d0
      sdg=10.d0
      open(13,file='accuraci.dat',status='old')
      open(14,file='accuracs.dat',status='old')
      open(15,file='accuracd.dat',status='old')
      open( 4,file='accuracy.sol',status='new')

c ...read file of untransformed records sorted on random effect 1
c    (animal), count number in fixed effect subclasses

      call nollmi(nh,nf,nofix)
      call nollmi(nrt,2,nht)
      call nollr(d3,nht)

c ...count in fixed classes

      write(*,*)'Counting in fixed classes...'
      do 280 i=1,nht
        read(13,103)ire,(ifix(j),j=1,nofix),ind,no1,no2,isire,idam,isex
        if(ire.gt.0)then
          nh(ifix(1),1)=nh(ifix(1),1)+1
        endif
  280 continue
  103 format(i1,i2,i6,2i4,2i6,i1)
      rewind 13
      write(*,*)'                     ...done'

c ...initilize vectors and counters

      lambda=(1.d0-h2)/h2
      n=0                                                                       
      nut=0
      do 731 j=1,nofix
      do 731 i=1,nfi(j)
        write(*,732) nh(i,j)
        if(nh(i,j).eq.0)then
c       write(*,*) '  empty fixed subclass - stop'
c       stop
        write(*,*) '  empty fixed subclass ...',i,j 
        endif
  731 continue
  732  format(i8)

c ...read file sorted on random effect 1 (animal)

      write(*,*)'Working on step 2...'
      do 130 i=1,nht
        read(13,103)ire,(ifix(j),j=1,nofix),ind,no1,no2,isire,idam,isex
c .....chech if parent no > ind
        if(isire.gt.ind)isire=0
        if(idam.gt.ind)idam=0
c .............................
        if(ire.gt.0)then
          d=1.d0
        else
          d=0.d0
        endif
        if(isire.eq.0.and.idam.eq.0)then
          q=1.d0
        elseif(isire.eq.0)then
          q=4.d0/3.d0
        elseif(idam.eq.0)then
          q=4.d0/3.d0
        else
          q=2.d0
        endif

c .....form the diagonal element of eq. (1) p. 90

        not=no1+no2
        sum1=0.d0
        sum2=0.d0
        d0(ind)=d+q*lambda+no1*lambda/2.d0+no2*lambda/3.d0

c .....form part 1 page 91 - adjustment for effective amount of
c      information about fixed classes

        if(ire.gt.0)then
          x1=nh(ifix(1),1)
          d1(ind)=d0(ind)-1.d0/x1
        else
          d1(ind)=d0(ind)
        endif

c .....form part 2 - eq. (4) page 91

c .......if having progeny in the data
        if(not.gt.0)then
c .........is male - read file sorted on sire (sire > 0)
          if(isex.eq.1)then
            do 140 l=1,not
              read(14,103)lre,(lfix(j),j=1,nofix),lnd,lo1,lo2,
     +         lsire,ldam,lsex
c ...........ind should be equal to lsire.
              if(ind.ne.lsire)then
                write(*,755) ind,lsire
  755           format('  ind=',i8,'.ne. lsire=',i8)
                stop
              endif
c ........................................
c ..........record type for offspring
              if(lre.eq.0)nrt(1,ind)=nrt(1,ind)+1
              if(lre.ge.1)nrt(2,ind)=nrt(2,ind)+1
c ...............................................
              if(lre.gt.0)then
                d=1.d0
              else
                d=0.d0
              endif
              if(ldam.eq.0)then
                q=4.d0/3.d0
              else
                q=2.d0
              endif
              d0s=d+q*lambda+lo1*lambda/2.d0+lo2*lambda/3.d0
              if(lre.gt.0)then
                x1=nh(lfix(1),1)
                d1s=d0s-1.d0/x1
              else
                d1s=d0s
              endif
              if(ldam.eq.0)then
                sum1=sum1+1.d0/d1s
              else
                sum2=sum2+1.d0/d1s
              endif
  140       continue
          else
c ......... else female -  read file sorted on dam (dam > 0)
            do 150 l=1,not
              read(15,103) lre,(lfix(j),j=1,nofix),lnd,lo1,lo2,
     +         lsire,ldam,lsex
c ...........ind should be equal to ldam
              if(ind.ne.ldam)then
                write(*,756) ind,ldam
  756           format('  ind=',i8,'.ne. ldam=',i8)
                stop
              endif
c ......................................
c ..........record type for offspring
              if(lre.eq.0)nrt(1,ind)=nrt(1,ind)+1
              if(lre.ge.1)nrt(2,ind)=nrt(2,ind)+1
c ...............................................
              if(lre.gt.0)then
                d=1.d0
              else
                d=0.d0
              endif
              if(lsire.eq.0)then
                q=4.d0/3.d0
              else
                q=2.d0
              endif
              d0d=d+q*lambda+lo1*lambda/2.d0+lo2*lambda/3.d0
              if(lre.gt.0)then
                x1=nh(lfix(1),1)
                d1d=d0d-1.d0/x1
              else
                d1d=d0d
              endif
              if(lsire.eq.0)then
                sum1=sum1+1.d0/d1d
              else
                sum2=sum2+1.d0/d1d
              endif
  150       continue
          endif
          d2(ind)=d1(ind)-lambda*lambda*(4.d0/9.d0*sum1+sum2)
        else
          d2(ind)=d1(ind)
        endif

c .....form part 3 equations (5), (6), (7) and (8) page 92

        if(isire.gt.0.and.idam.gt.0)then
          d2xs=d2(isire)+lambda*lambda/d2(ind)
          d2xd=d2(idam)+lambda*lambda/d2(ind)
          d3(ind)=d2(ind)-lambda*lambda*((d2xs+d2xd-lambda)/(d2xs*d2xd
     +            -1.d0/4.d0*lambda*lambda))
        elseif(isire.gt.0)then
          d2xs=d2(isire)+4.d0/9.d0*lambda*lambda/d2(ind)
          d3(ind)=d2(ind)-4.d0/9.d0*lambda*lambda/d2xs
        elseif(idam.gt.0)then
          d2xd=d2(idam)+4.d0/9.d0*lambda*lambda/d2(ind)
          d3(ind)=d2(ind)-4.d0/9.d0*lambda*lambda/d2xd
        else
          d3(ind)=d2(ind)
        endif
  130 continue
      write(*,*)'             ...done'
      rewind 13

      write(*,*)'Write out solutions...'
c     write(4,201)
c     write(4,202)
c     write(4,203)
      do 180 i=1,nht
        read(13,103) ire,(ifix(j),j=1,nofix),ind,no1,no2,isire,idam,lsex
c .....chech if parent no > ind
        if(isire.gt.ind)isire=0
        if(idam.gt.ind)idam=0
c .............................
        if(ire.gt.0)then
          d=1.d0
        else
          d=0.d0
        endif
        if(isire.eq.0.and.idam.eq.0)then
          ip=0
          q=1.d0
        elseif(isire.eq.0)then
          ip=1
          q=4.d0/3.d0
        elseif(idam.eq.0)then
          ip=1
          q=4.d0/3.d0
        else
          ip=2
          q=2.d0
        endif

c .....form the diagonal element of eq. (1) p. 90

        not=no1+no2
        d0(ind)=d+q*lambda+no1*lambda/2.d0+no2*lambda/3.d0
c .....possible to calculate and write out rui if wanted
c        rui=dsqrt(1.d0-lambda/d0(ind))
c ......................................................
        if(d3(ind).gt.lambda)then
          rai=dsqrt(1.d0-lambda/d3(ind))
        else
          rai=0.d0
        endif
        sdt=(dsqrt(1.d0 - rai*rai))*sdg
        write(4,204) i,ire,ip,not,(nrt(j,i),j=1,2),rai,sdt
  180 continue
  201 format('Animal  Own  No.      Offsprings      Rai    SE ')
  202 format('        Rec  Par   Tot. N/Rec W/Rec             ')
  203 format('------------------------------------------------')
  204 format(i6,i4,i5,i7,2i6,f7.2,f7.0)
      write(*,*)'               ...done'
      close(13)
      close(14)
      close(15)
      close( 4)
      stop
      end

      subroutine nollr(rvect,ilength)
      real*8 rvect(ilength)
      do 199 i=1,ilength
  199   rvect(i)=0.d0
      return
      end

      subroutine nollmi(rmatrx,irow,icol)
      integer*4 rmatrx(irow,icol)
      do 199 i=1,irow
        do 198 j=1,icol
  198     rmatrx(i,j)=0
  199 continue
      return
      end
