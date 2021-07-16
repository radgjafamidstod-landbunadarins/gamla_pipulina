c-----------------------------------------------------------------------
      program impl
c     version 5.0
c     One random effect (+ error), any number of traits, all possible 
c     missing trait combinations, any number of fixed effects, and 
c     different models for each trait
c.......................................................................
c     jan 1992, revised dec. 1995
c     agust@goliat.hfs.slu.se <Agust Sigurdsson>
c-----------------------------------------------------------------------
      include 'param.i'
      parameter(maxta=notrt*maxani)
      parameter(maxaft=maxft+maxta)
c     character*9 cumstr,dumstr
      real*8 rhss(maxta),rhsb(maxft),r(maxcomb,mtrthf),b(maxft),
     +        s(maxta),bun(maxfix,notrt),
     +        os(maxaft),d(maxcomb,maxdblc,mtrthf),o(maxoblc,mtrthf),
     +        adhn1,adhn2,conv,
     +        grhss(maxta),grhsb(maxft),
     +        dfix(maxft),ca(nofill),relax
     +        ,admad,xx,tmp(notrt),gamma,delta,oconv,tmp0
      integer*4 aeq(maxani),seq(maxani),deq(maxani),nbi,ix2(maxani),
     +         ix3(maxani),ka(maxft),kc(nofill),noficl(0:maxfix)
      integer*4 rix(0:norbl),ix1(maxani)
      integer*4 beq(maxfix,notrt,maxani)
      character dird*5
      character*9 cumstr,dumstr,dagtim*25
      logical loadold,pr
c     call cumtim(cumstr)
c     call wallcl(dumstr)
      adhn1=0.d0
      adhn2=1.d0
      conv=1.d0
      oconv=1.d0
      ip=25
      pr=.false.
c....NULLA
      iter=0
      ixmod=0
      do i=0,maxfix
        noficl(i)=0
      enddo
      do i=0,norbl
        rix(i)=0
      enddo
      do i=1,maxfix
       do j=1,notrt
        do k=1,maxani
         beq(i,j,k)=0
        enddo
       enddo
      enddo
      call nolli(aeq,maxani)
      call nolli(seq,maxani)
      call nolli(deq,maxani)
      call nolli(ix1,maxani)
      call nolli(ix2,maxani)
      call nolli(ix3,maxani)
      call nolli(ka,maxft)
      call nolli(kc,nofill)
      call nollr(rhsb,maxft)
      call nollr(b,maxft)
      call nollr(s,maxta)
      call nollr(os,maxaft)
      call nollr(grhss,maxta)
      call nollr(grhsb,maxft)
      call nollr(dfix,maxft)
      call nollr(ca,nofill)
      call nollr(tmp,notrt)
      call nollm(r,maxcomb,mtrthf)
      call nollm(bun,maxfix,notrt)
      call nollm(o,maxoblc,mtrthf)
      call nollr3(d,maxcomb,maxdblc,mtrthf)
c.....................
c  Convergence criteria for fixed part      
      fconv=-4.d0
c  Relaxation factor
       relax=0.4d0   
c      write(6,200)
c      read(5,*)relax
c  200 format('Give relaxation factor: ')
      open(14,file='data/model.txt')
      open(15,file='dump/Winfo.txt')
      open(18,file='dump/Wimpl18.bin',form='unformatted')
      open(19,file='dump/Wimpl19.bin',form='unformatted')
      open(23,file='dump/Wimpl23.bin',form='unformatted')
      open(25,file='dump/Wimpl25.bin',form='unformatted')
      open(28,file='dump/Wimpl28.bin',form='unformatted')
      open(29,file='dump/Wimpl29.bin',form='unformatted')
      open(30,file='dump/Wimpl30.bin',form='unformatted')
      open(40,file='keep/Rimpl40.txt')
      open(41,file='keep/Rimpl41.txt')
      open(55,file='dump/Wimpl55.bin',form='unformatted')
      open(56,file='dump/Wimpl56.bin',form='unformatted')
      open(58,file='dump/Wimpl58.bin',form='unformatted')
      open(59,file='dump/Wimpl59.bin',form='unformatted')
      open(66,file='keep/Rimpl.log')
c     call fdate(dagtim)
c     write(66,*)'Output from program IMPL ',dagtim
      write(66,*)'..................................................'
      read(14,*)noani,nofix,ilold,maxit,minadh,nbi
      do ll=1,nofix
        read(14,*)noficl(ll) 
      enddo
      read(14,*)nocomb
      read(15,*)nolev,noft
      write(66,*)'#fixed levels ',nolev,' #fixed levels total ',noft
      write(*,*)'#fixed levels ',nolev,' #fixed levels total ',noft
      read(15,*)nodbl,noobl
 1114 format(5i7)
c  Should old solutions be loaded ?
      if(ilold.eq.0)then
        loadold=.false.
        write(66,*)'...old solutions NOT loaded'
      else
        loadold=.true.
        write(66,*)'...old solutions LOADED'
      endif
      noft2=noft*(noft+1)/2
      nota=notrt*noani
      notfa=notrt*(nolev+noani)
      noaft=noft+nota
      iunit55=55
      iunit56=56
      iunit58=58
      iunit59=59
c-----------------------------------------------------------------------
c  Step 1: Read animal file, off- and diagonal and sparse fixed blocks
c-----------------------------------------------------------------------
      print *,'Reading animal file...'
      do i=1,noani
        read(30)ix1(i),ix2(i),ix3(i),aeq(i),
     +          ((beq(m,l,i),m=1,nofix),l=1,notrt),seq(i),deq(i)
c  If record is used as an indicator of first eq. of animal
c        aeq(i)=(noani-i)*notrt+1
      enddo
c...read all combinations of inv(R) that occur in data
      do i=1,nocomb
        read(18)(r(i,j),j=1,mtrthf)
c       write(*,*)(r(i,j),j=1,mtrthf)
      enddo
      do i=0,norbl
        read(19)rix(i)
      enddo
      print *,'Storing diagonal-blocks in memory'
      do k=1,nodbl
        do i=1,nocomb
          read(23)(d(i,k,j2),j2=1,mtrthf)
c         do j2=1,mtrthf
c         write(*,*)i,k,j2,d(i,k,j2)
c         enddo
        enddo
      enddo
  402 format(6f10.3)
  901 print *,'Storing offdiagonal-blocks in memory'
      do i=1,noobl
        read(25)(o(i,j),j=1,mtrthf)
c       write(*,'(45f10.4)')(o(i,j),j=1,mtrthf)
      enddo
  401 format(6f10.6)
  902 print *,'Reading and storing fixed blocks and RHS vektors'
      read(58)k
      call ireahf(kc,k,iunit58)
      read(59)k
      call readhf(ca,k,iunit59)
  990 print *,'       ...sparse offdiagonal XRX'
      read(55)nd
      call ireahf(ka,nd,iunit55)
      read(56)nd
      call readhf(dfix,nd,iunit56)
  991 print *,'              ...diagonal of XRX'
        do 291 i=1,noft
          read(28) rhsb(i)
          grhsb(i)=rhsb(i)
          b(i)=0.d0
  291   continue
        do 203 i=1,nota 
          read(29)grhss(i)
          s(i)=0.d0
  203   continue
      print *,'                          ...RHS'
c.............................................................
c     IF INVERSION OF XRX IS PLANNED IT SHOULD BE DONE HERE
c     OBS: THE STORAGE SCHEME FOR XRX HERE IS SPARSE FULLSTORED
c          WHICH HAS TO BE CHANGED TO UPPER HALF-STORED FORMAT
c          IF SUBROUTINE DKMVHF IS TO BE USED OR UPPER HALF
c          SPARSE FORMAT FOR FSPAK
c.............................................................
      if(loadold)then
        print *,'Reading old solutions...'
        do i=1,nolev
          read(40,791)(b(notrt*(i-1)+ii),ii=1,notrt)
        enddo
        do i=1,noani
          read(41,791)(s(notrt*(i-1)+ii),ii=1,notrt)
        enddo
        rewind(40)
        rewind(41)
      endif
      write(66,*)'ITERATION STARTS WITH THE FOLLOWING PARAMETERS'
      write(66,*)'    Maximum number of iterations:',maxit
      write(66,*)'Convergence (10 to the power of):',minadh
      print *,'ITERATION STARTS WITH THE FOLLOWING PARAMETERS'
      print *,'    Maximum number of iterations:',maxit
      print *,'Convergence (10 to the power of):',minadh
c     call cumtim(cumstr)
c     call wallcl(dumstr)
c-----------------------------------------------------------------------
c  ITERATION PHASE STARTS
c-----------------------------------------------------------------------
  809 do while ((iter.lt.maxit).and.(log10(conv).gt.minadh))
c-----------------------------------------------------------------------
         do 288 i=1,nota
  288      os(noft+i)=s(i)
c.......................................................................
c  Special iteration for XRX
c  COULD BE CHANGED TO MULTIPLICATION OF INVERSE(XRX)*RHSb
c.......................................................................
      di=0.d0
      sum=0.d0
      convf3=1.d0
c     print *,'Start fixed iteration'
      do while(convf3.gt.fconv)
         ic=1
         do 239 i=1,noft
  239      os(i)=b(i)
        do 238 i=1,noft
          sum=0.d0
          if(dfix(i).ne.0.d0)then
            di=dfix(i)
          else
            di=1.d0
          endif
          if(ka(i).gt.0)then
            ic2=ic+ka(i)-1
            do 237 j=ic,ic2
              sum=sum+ca(j)*b(kc(j))
  237       continue
          endif
          sum=sum+dfix(i)*b(i)
          ic=ic+ka(i)
          ra=rhsb(i)-sum
          b(i)=b(i)+ra/di
  238   continue
        convf1=0.d0
        convf2=0.d0
        do 279 i=1,noft
          convf1=convf1+(b(i)-os(i))**2
          convf2=convf2+(b(i))**2
  279   continue
        convf3=log10(sqrt(convf1/convf2))
       print *,'Convergence level of fixed part:',convf3
c...................  XRX loop..........................................
      end do
c     goto 781
      if(mod(iter,nbi).eq.0)print *,'Convergence of fixed part:',convf3
c.......................................................................
c  get fresh RHS
      do 269 i=1,noft
  269 rhsb(i)=grhsb(i)
      do 293 i=1,nota
  293 rhss(i)=grhss(i)
c.......................................................................
c  Iteration on animal vector starts beginning with youngest animal
c.......................................................................
c-----------------------------------------------------------------------
      do 219 k=1,noani
c-----------------------------------------------------------------------
c  Step 3a. Adjust the RHS's of the animals parents for each others
c           solutions.
c.......................................................................
      if(pr)print *,'Step 3a'
      if ((seq(k).ne.0).and.(deq(k).ne.0)) then
        do ii=1,notrt
         tmp(ii)=0.d0
         do jj=1,notrt
           tmp(ii)=tmp(ii)+o(ix3(k),ihmssf(ii,jj,notrt))*s(deq(k)+jj-1)
         enddo
         rhss(seq(k)+ii-1)=rhss(seq(k)+ii-1)-(-0.5)*tmp(ii)
         if(pr)print *,rhss(seq(k)+ii-1)
        enddo
        do ii=1,notrt
         tmp(ii)=0.d0
         do jj=1,notrt
           tmp(ii)=tmp(ii)+o(ix3(k),ihmssf(ii,jj,notrt))*s(seq(k)+jj-1)
         enddo
         rhss(deq(k)+ii-1)=rhss(deq(k)+ii-1)-(-0.5)*tmp(ii)
         if(pr)print *,rhss(deq(k)+ii-1)
        enddo
      endif
c.......................................................................
c  Step 3b. Adjust the RHS of animal for it's b effect.
c.......................................................................
      if(pr)print *,'Step 3b'
      if (ix1(k).ne.0) then
        do ll=1,notrt
         tmp(ll)=0.d0
         do kk=1,nofix
          tmp(ll)=tmp(ll)+b(beq(kk,ll,k))
         enddo
        enddo
        do ir=1,notrt
         tmp0=0.d0
         do ic=1,notrt
          tmp0=tmp0+(r(rix(ix1(k)),ihmssf(ir,ic,notrt))*tmp(ic))
         enddo
         rhss(aeq(k)+(ir-1))=rhss(aeq(k)+(ir-1))-tmp0
         if(pr)print *,rhss(aeq(k)+ir-1)
        enddo
      endif
c.......................................................................
c  Step 3c. Adjust the RHS of animal for it's parents solutions.
c.......................................................................
      if(pr)print *,'Step 3c'
      if((seq(k).ne.0).or.(deq(k).ne.0))then
       if((seq(k).ne.0).and.(deq(k).ne.0))then
        do ll=1,notrt
          tmp(ll)=s(seq(k)+(ll-1))+s(deq(k)+(ll-1))
        enddo
       elseif(deq(k).eq.0)then
        do ll=1,notrt
          tmp(ll)=s(seq(k)+(ll-1))
        enddo
       elseif(seq(k).eq.0)then
        do ll=1,notrt
          tmp(ll)=s(deq(k)+(ll-1))
        enddo
       endif
       do ir=1,notrt
         do ic=1,notrt
           rhss(aeq(k)+(ir-1))=rhss(aeq(k)+(ir-1))-
     +      (o(ix3(k),ihmssf(ir,ic,notrt))*tmp(ic))
         enddo
         if(pr)print *,rhss(aeq(k)+ir-1)
       enddo
      endif
c.......................................................................
c   Step 3d. Calculate new solution for the animal by
c            multiplying it's diagonal block with the RHS.
c.......................................................................
      if(pr)print *,'Step 3d'
       do ir=1,notrt
         s(aeq(k)+(ir-1))=0.d0
         do ic=1,notrt
           s(aeq(k)+(ir-1))=s(aeq(k)+(ir-1))+
     +     d(rix(ix1(k)),ix2(k),ihmssf(ir,ic,notrt))*rhss(aeq(k)+(ic-1))
         enddo
         if(pr)print *,s(aeq(k)+ir-1)
       enddo
c.......................................................................
c  Step 3e. Adjust parents RHS's for the new solution.
c.......................................................................
      if(pr)print *,'Step 3e'
      if((seq(k).ne.0).or.(deq(k).ne.0))then
       do ir=1,notrt
         tmp(ir)=0.d0
         do ic=1,notrt
           tmp(ir)=tmp(ir)+
     +     o(ix3(k),ihmssf(ir,ic,notrt))*s(aeq(k)+(ic-1))
         enddo
       enddo
       if(seq(k).ne.0) then
        do ir=1,notrt
          rhss(seq(k)+(ir-1))=rhss(seq(k)+(ir-1))-tmp(ir)
         if(pr)print *,rhss(seq(k)+ir-1)
        enddo
       endif
       if (deq(k).ne.0) then
        do ir=1,notrt
          rhss(deq(k)+(ir-1))=rhss(deq(k)+(ir-1))-tmp(ir)
         if(pr)print *,rhss(deq(k)+ir-1)
        enddo
       endif
      endif
c.......................................................................
c  Step 3f. Adjust the b RHS for the new solution.
c.......................................................................
      if(pr)print *,'Step 3f'
      if (ix1(k).ne.0) then
       do ll=1,nofix
        do ir=1,notrt
         tmp(ir)=0.d0
         do ic=1,notrt
          tmp(ir)=tmp(ir)+
     +            r(rix(ix1(k)),ihmssf(ir,ic,notrt))*s(aeq(k)+(ic-1))
         enddo
         rhsb(beq(ll,ir,k))= rhsb(beq(ll,ir,k))-tmp(ir)
         if(pr)print *,rhsb(beq(ll,ir,k))
        enddo
       enddo
      endif
c------------------ ... animal vector loop -----------------------------
  219 continue
c-----------------------------------------------------------------------
      iter=iter+1
c ----------------------------------------------------------------------
c...do exponential extrapolation and calculate convergence progress
      if(iter.ge.25)then
        adhn1=0.d0
        adhn2=0.d0
        admad=0.d0
  876 format(i5,3f14.9)
        do 278 i=1,nota
c...apply relaxation 
c          s(i)=(s(i)-os(noft+i))*relax+s(i)
          delta=s(i)-os(noft+i)
          adhn1=adhn1+delta**2
          adhn2=adhn2+(s(i))**2
          if(abs(delta).gt.admad)admad=abs(delta)
          os(noft+i)=delta
  278   continue
        conv=dsqrt(adhn1/adhn2)
        gamma=conv/oconv 
        ixmod=mod(iter,ip)
        if(ixmod.eq.0)then
          oconv=conv
          xx=ip
          if(gamma.lt.0.98d0.and.gamma.gt.0.d0)then
            conv=conv/(1.d0-gamma**(1.d0/xx))
            do 778 i=1,nota
              s(i)=s(i)+os(noft+i)/(1.d0-gamma**(1.d0/xx))
  778       continue 
          endif
        endif
      write(66,718)iter,gamma,conv,oconv,admad
      if(mod(iter,nbi).eq.0)print 718,iter,gamma,conv,oconv,admad
      else
      write(66,718)iter,gamma,conv,oconv,admad
      if(mod(iter,nbi).eq.0)print 718,iter,gamma,conv,oconv,admad
      endif
  718 format('It: ',i4,' gamma: ',f9.5,' Cd: ',f9.5,
     + ' Cd(-25): ',f9.5,' AMD: ',f8.5)
c-----------------------------------------------------------------------
      end do
      write(66,'(a20,i4,a17,f10.6,a12,f4.0)')'System converged in '
     +       ,iter,' iterations, Cd: ',conv,' or log(Cd) ',log10(conv)
      print '(a20,i4,a17,f10.6,a12,f4.0)','System converged in '
     +       ,iter,' iterations, Cd: ',conv,' or log(Cd) ',log10(conv)
c-----------------------------------------------------------------------
c     call cumtim(cumstr)
c     call wallcl(dumstr)
c     write(66,*)' CPU time: ', cumstr
c     write(66,*)' Total wall clock time: ', dumstr
c     print *,' CPU time: ', cumstr
c     print *,' Total wall clock time: ', dumstr
c  ...write out solutions
      print *,'Writing out solutions...'
  781   do i=1,nolev
          write(40,791)(b(notrt*(i-1)+ii),ii=1,notrt)
        enddo
        goto 788
c...to create uniq BLUE solutions, !does not work if different
c...levels between traits
c 781   if(nofix.gt.1)then
        if(nofix.gt.1)then
         do i=2,nofix
          l=0
          do j=1,i
           l=l+noficl(j)
          enddo
          l=(l-1)*notrt
          do j=1,notrt
           bun(i,j)=b(l+j)
           bun(1,j)=bun(1,j)+b(l+j)
          enddo
         enddo
c...solution for last level in each fixed effect in bun()
c...write out first fixed effect
         do i=1,noficl(1)
          write(40,791)((b(notrt*(i-1)+ii)+bun(1,ii)),ii=1,notrt)
         enddo
         do i=2,nofix
          m=0
          do j=1,i-1
           m=m+noficl(j)
          enddo
          l=0
          do j=1,i
           l=l+noficl(j)
          enddo
          do j=m+1,l
           write(40,791)((b(notrt*(j-1)+ii)-bun(i,ii)),ii=1,notrt)
          enddo
         enddo
        else
         do i=1,nolev
          write(40,791)(b(notrt*(i-1)+ii),ii=1,notrt)
         enddo
        endif
  788   do i=1,noani
          write(41,791)(s(notrt*(i-1)+ii),ii=1,notrt)
        enddo
c Format for solutions files impl40.sol impl41.sol
  791 format(8f14.6)
  792 format(8f16.1)
c---------------------- ...done ----------------------------------------
c     call fdate(dagtim)
c     write(66,*)'...end of output from program IMPL ',dagtim
      write(66,*)'............................................'
      print *,'            ...done'
  999 stop
      end
C=======================================================================SUB04750
      INTEGER FUNCTION IHMSSF(I,J,N)                                    SUB04760
C=======================================================================SUB04770
      IF(I.LE.J)THEN                                                    SUB04820
      I1=I-1                                                            SUB04830
      IHMSSF=N*I1-I*I1/2+J                                              SUB04840
      ELSE                                                              SUB04850
      J1=J-1                                                            SUB04860
      IHMSSF=N*J1-J*J1/2+I                                              SUB04870
      END IF                                                            SUB04880
      RETURN                                                            SUB04890
      END                                                               SUB04900
C-----------------------------------------------------------------------SUB04920
      INTEGER FUNCTION IHMI(I,N)                                        SUB04930
C-----------------------------------------------------------------------SUB04940
      I1=I-1                                                            SUB04950
      IHMI=N*I1-I*I1/2                                                  SUB04960
      RETURN                                                            SUB04970
      END                                                               SUB04980
C-----------------------------------------------------------------------SUB05000
      INTEGER FUNCTION IHMII(I,N)                                       SUB05010
C-----------------------------------------------------------------------SUB05020
      IHMII=N*(I-1)+I*(3-I)/2                                           SUB05040
      RETURN                                                            SUB05050
      END                                                               SUB05060
C-----------------------------------------------------------------------SUB05080
      INTEGER FUNCTION IHMIJ(I,J,N)                                     SUB05090
C-----------------------------------------------------------------------SUB05100
C     SPECIALISED FORM OF IHMSSF FOR I<J                                SUB05110
      I1=I-1                                                            SUB05120
      IHMIJ=N*I1-I*I1/2+J                                               SUB05130
      RETURN                                                            SUB05140
      END                                                               SUB05150
C-----------------------------------------------------------------------SUB05170
      INTEGER FUNCTION IHMJI(I,J,N)                                     SUB05180
C-----------------------------------------------------------------------SUB05190
C     SPECIALISED FORM OF IHMSSF FOR I>J                                SUB05200
      J1=J-1                                                            SUB05210
      IHMJI=N*J1-J*J1/2+I                                               SUB05220
      RETURN                                                            SUB05230
      END                                                               SUB05240
C-----------------------------------------------------------------------
      SUBROUTINE READHF(C,N,IUNIT)
C-----------------------------------------------------------------------
      DOUBLE PRECISION C(N)
      IF(N.LE.128)THEN
        READ(IUNIT)(C(II),II=1,N)
        RETURN
      ENDIF
      M=N/128
      I1=-127
      I2=0
      DO 2 I=1,M
        I1=I1+128
        I2=I2+128
        READ(IUNIT)(C(II),II=I1,I2)
    2 continue
      I1=I1+128
      READ(IUNIT)(C(II),II=I1,N)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE IREAHF(IC,N,IUNIT)
C-----------------------------------------------------------------------
      DIMENSION IC(N)
      IF(N.LE.256)THEN
        READ(IUNIT)(IC(II),II=1,N)
        RETURN
      ENDIF
      M=N/256
      I1=-255
      I2=0
      DO 2 I=1,M
        I1=I1+256
        I2=I2+256
        READ(IUNIT)(IC(II),II=I1,I2)
    2 continue
      I1=I1+256
      READ(IUNIT)(IC(II),II=I1,N)
      RETURN
      END
c     include '/home/agust/agusts/assub/nolli.f'
c     include '/home/agust/agusts/assub/nollm.f'
c     include '/home/agust/agusts/assub/nollr3.f'
c     include '/home/agust/agusts/assub/nollr.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nolli.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollm.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr3.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr.f'
      include '/home/elsa/elsaagust/agusts/assub/nolli.f'
      include '/home/elsa/elsaagust/agusts/assub/nollm.f'
      include '/home/elsa/elsaagust/agusts/assub/nollr3.f'
      include '/home/elsa/elsaagust/agusts/assub/nollr.f'
