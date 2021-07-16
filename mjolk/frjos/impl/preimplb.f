c...VEGNA BILS MILLI BURDA - NYTIR FULLKOMLEGA AFURDAKEYRSLU
c...OG KEMUR STRAX AD HENNI LOKINNI
c...Trixid er ad bil milli burda er sidasti fixed i fixed.txt og
c...er lesinn inn sem trait
c-----------------------------------------------------------------------
      program preimpl
c     version 5.0
c     One random effect (+ error), any number of traits, all possible 
c     missing trait combinations, any number of fixed effect and 
c     different model for each trait.
c.......................................................................
c     jan 1992, revised dec 1995;sept 1996
c     agusts@centrum.is <Agust Sigurdsson>
c-----------------------------------------------------------------------
      include 'param.i'
      real*8 r0(mtrthf),r(maxcomb,mtrthf),rsub(mtrthf),go(mtrthf)
      real*8 blcag2(maxoblc,mtrthf),g_half(mtrthf),v(notrt),
     +  bt(maxft,notrt),rhsb(maxft),
     +  el(notrt),suba(mtrthf),d(maxft),ca(nofill),
     +  coef,lilli,pros
      real*4 t(notrt,maxani),miss,t0(notrt,maxani)
      real*4 e1(maxani),e2(maxani),di(maxani),blokag(maxdblc,mtrthf)
      integer*4 comb(notrt),leibniz,okcomb(0:norbl),tel(5,3)
      integer*4 a_ind(maxani),iflag(notrt),si(maxani),
     +  da(maxani),aeq,seq,deq,rix(0:norbl)
      integer*4 ix2(maxani),ix3(maxani),kr(nofill),
     +  kc(nofill),ka(maxft),noficl(0:maxfix)
      integer*4 ix1(maxani),fix(maxfix,notrt,maxani),beq(maxfix,notrt)
      integer*4 hjord
c...til bradabirgda
      integer*4 ifixx(maxfix,notrt,0:maxlev),ihf
c     character pth*29,pt2*40
!     character pth*26,pt2*37
      character pth*21,pt2*32
      character*9 cumstr,dumstr,dagtim*25
c     call cumtim(cumstr)
c     call wallcl(dumstr)
c     pth='/home/agust/skuggi_kyr/mjolk/'
c     pt2='/home/agust/skuggi_kyr/mjolk/frjos/impl/'
!     pth='/home/LBHI/elsa/Kyr/mjolk/'
!     pt2='/home/LBHI/elsa/Kyr/mjolk/frjos/impl/'
      pth='/home/elsa/Kyr/mjolk/'
      pt2='/home/elsa/Kyr/mjolk/frjos/impl/'
      lilli=1.0d-20
c.............NULLUN
      do i=1,mtrthf
        r0(i)=0.d0
        rsub(i)=0.d0
        go(i)=0.d0
        suba(i)=0.d0
        g_half(i)=0.d0
        do j=1,maxcomb
          r(j,i)=0.d0
        enddo
        do j=1,maxoblc
          blcag2(j,i)=0.d0
        enddo
      enddo
      do i=1,nofill
       ca(i)=0.d0
       kr(i)=0
       kc(i)=0
      enddo
      coef=0.d0
      pros=0.d0
      do i=1,notrt
       v(i)=0.d0
       el(i)=0.d0
       iflag(i)=0
       comb(i)=0
      enddo
      do i=1,maxft
       do j=1,notrt
       bt(i,j)=0.d0
       enddo
       rhsb(i)=0.d0
       d(i)=0.d0
       ka(i)=0
      enddo
      do i=1,maxani
       do j=1,notrt
        t(j,i)=0.e0
       enddo
       e1(i)=0.e0
       e2(i)=0.e0
       di(i)=0.e0
      si(i)=0
      da(i)=0
      ix2(i)=0
      ix3(i)=0
      a_ind(i)=0
      enddo
      miss=0.e0
      do i=1,maxdblc
       do j=1,mtrthf
        blokag(i,j)=0.e0
       enddo
      enddo
      leibniz=0
      do i=0,norbl
       okcomb(i)=0
       rix(i)=0
      enddo
      aeq=0
      seq=0
      deq=0
      do i=0,maxfix
       noficl(i)=0
      enddo
      do i=1,maxani
       ix1(i)=0
      enddo
      do i=1,maxfix
       do j=1,notrt
        beq(i,j)=0
        do k=1,maxani
          fix(i,j,k)=0
        enddo
        do k=0,maxlev
          ifixx(i,j,k)=0
        enddo
       enddo
      enddo
      hjord=0 
      ihf=0
c...missing value if t=miss
c...HER ER BREYTING V/BIL MILLI BURDA
c     miss=-999.0
      miss= .0
c...HER ER BREYTING V/BIL MILLI BURDA
      open( 9,file=pt2//'data/var.txt',status='old')
      open(10,file=pth//'impl/data/pedgrp.txt',status='old')
      open(11,file=pth//'impl/data/trait.txt',status='old')
      open(12,file=pth//'impl/data/fixed.txt',status='old')
      open(13,file=pth//'impl/keep/di.txt',status='old')
      open(14,file=pt2//'data/model.txt',status='old')
      open(15,file=pt2//'dump/Winfo.txt')
      open(17,file=pt2//'dump/Wimpl17.bin',form='unformatted')
      open(18,file=pt2//'dump/Wimpl18.bin',form='unformatted')
      open(19,file=pt2//'dump/Wimpl19.bin',form='unformatted')
      open(23,file=pt2//'dump/Wimpl23.bin',form='unformatted')
      open(25,file=pt2//'dump/Wimpl25.bin',form='unformatted')
      open(28,file=pt2//'dump/Wimpl28.bin',form='unformatted')
      open(29,file=pt2//'dump/Wimpl29.bin',form='unformatted')
      open(30,file=pt2//'dump/Wimpl30.bin',form='unformatted')
      open(55,file=pt2//'dump/Wimpl55.bin',form='unformatted')
      open(56,file=pt2//'dump/Wimpl56.bin',form='unformatted')
      open(58,file=pt2//'dump/Wimpl58.bin',form='unformatted')
      open(59,file=pt2//'dump/Wimpl59.bin',form='unformatted')
      open(66,file=pt2//'keep/Rpreimpl.log')
c     call fdate(dagtim)
      write(66,*)'Output from program PREIMPL ',dagtim
      write(66,*)'.....................................................'
      read(14,*)noani,nofix 
      write(*,*)'#animals:',noani,' #fixed effects:',nofix 
      write(66,*)'#animals:',noani,' #fixed effects:',nofix 
      nolev=0
      do ll=1,nofix
        read(14,*)noficl(ll) 
        write(*,*)'   #levels for fixed effect ',ll,':',noficl(ll) 
        write(66,*)'   #levels for fixed effect ',ll,':',noficl(ll) 
        nolev=nolev+noficl(ll) 
      enddo
 2297 format(2i7)
      noft=notrt*nolev
      nd=noft
      write(15,*)nolev,noft 
      iunit55=55
      iunit56=56
      iunit58=58
      iunit59=59
c-----------------------------------------------------------------------
c     step 0 (variance)
c-----------------------------------------------------------------------
      read(9,*)go
      write(66,*)'G matrix'
      write(66,*)go
      call dkmvhf(go,v,iflag,notrt)
      write(66,*)'inv(G) matrix'
      write(66,*)go
      read(9,*)r0
      write(66,*)'R matrix'
      write(66,*)r0
c...new stuff
      read(14,*)nocomb
      write(66,*)'Number of possible combinations: ',nocomb
      write(66,*)'All possible inv(R) matrices'
      do l=1,nocomb
        read(14,*)(comb(jj),jj=1,notrt)
        nr=0
        leibniz=0
        do ll=1,notrt
          leibniz=leibniz+comb(ll)*2**(notrt-ll)
          nr=nr+comb(ll)
        enddo
        write(66,*)'Combination bin:',
     +(comb(jj),jj=1,notrt),' dec:',leibniz
        okcomb(leibniz)=1
        rix(leibniz)=l
        ir=0
        call nollr(rsub,nr)
        do i=1,notrt
          if(comb(i).eq.1)then
            ir=ir+1
            ic=ir-1
            do j=i,notrt
              if(comb(j).eq.1)then
                ic=ic+1
                rsub(ihmssf(ir,ic,nr))=r0(ihmssf(i,j,notrt))
              endif
            enddo
          endif
        enddo
c...invert the full rank submatrix of R
        call dkmvhf(rsub,v,iflag,nr)
        write(66,*)rsub
c...put the inverse of Rsub in the large R again
        ir=0
        do i=1,notrt
          if(comb(i).eq.1)then
            ir=ir+1
            ic=ir-1
            do j=i,notrt
              if(comb(j).eq.1)then
               ic=ic+1
               r(rix(leibniz),ihmssf(i,j,notrt))=rsub(ihmssf(ir,ic,nr))
              endif
            enddo
          endif
        enddo
      enddo
      do i=1,mtrthf
        write(17)go(i)
      enddo
      do i=1,nocomb
        write(18)(r(i,j),j=1,mtrthf)
      enddo
      do i=0,norbl
        write(19)rix(i)
      enddo
  295 format(45f10.4)
      rewind(17)
      rewind(18)
c-----------------------------------------------------------------------
c     step 1 (buildrhs)
c-----------------------------------------------------------------------
      print *,'Step 1: Build RHS...'
      do i=1,nofix
        do j=1,notrt
          do k=0,maxlev
            ifixx(i,j,k)=0
          enddo
        enddo
      enddo
      write(66,*)'...missing value = ',miss,' (correct?)'
      print *,'...missing value = ',miss,' (correct?)'
      do i=1,noani
         read(10,*)iixxx,si(i),da(i)
c...HER ER BREYTING V/BIL MILLI BURDA
         read(11,*)(t0(j,i),j=1,notrt)
         read(12,*)hjord,(fix(1,l,i),l=1,notrt),iix,iixx,iixxx
     +,((fix(k,l,i),l=1,notrt),k=2,nofix)
     +,(t(j,i),j=1,notrt)
         if(t0(1,i).eq.-999.)t(1,i)=miss
         if(t0(2,i).eq.-999.)t(2,i)=miss
         if(t0(3,i).eq.-999.)t(3,i)=miss
c...HER ER BREYTING V/BIL MILLI BURDA
c...henda ut flokkum:
c...21: Kyrin helt ekki (disposal reason 3) held thessu inni
c...23: disp 1,2,6
c...24: disp 4,5,7..15
         do ll=1,notrt
           if(t(ll,i).ge.23)t(ll,i)=miss
         enddo
         if(t(1,i).eq.miss)then
           t(2,i)=miss
           t(3,i)=miss
         endif
         if(t(2,i).eq.miss)then
           t(3,i)=miss
         endif
         write(77,'(i5,3f5.0)')i,(t(j,i),j=1,3)
c        fix(1,1,i)=hjord
c        fix(1,2,i)=hjord
c        fix(1,3,i)=hjord
c...check missing value pattern
         leibniz=0
         do ll=1,notrt
           if(t(ll,i).eq.miss)then
             comb(ll)=0
             do mn=1,nofix
              fix(mn,ll,i)=0
             enddo
           else
             comb(ll)=1
           endif
         enddo
         do ll=1,notrt
           leibniz=leibniz+comb(ll)*2**(notrt-ll)
         enddo
c...check for allowed combination
         if(okcomb(leibniz).ne.1)then
           write(66,*)'Unexpected missing trait combination -give up'
           write(66,*)(t(j,i),j=1,notrt)
           print *,'Unexpected missing trait combination -give up'
           print *,(t(j,i),j=1,notrt)
           stop
         endif
         ix1(i)=leibniz
c...check numbers in fixed classes and limits
        do ii=1,nofix
          do j=1,notrt
            ifixx(ii,j,fix(ii,j,i))=ifixx(ii,j,fix(ii,j,i))+1
          enddo
        enddo
        do ll=1,nofix
          do l=1,notrt
           if(fix(ll,l,i).gt.noficl(ll))then
         write(66,*)'Fixed effect ',ll,l,' out of range at record',i
     +,' max allowed ',noficl(ll),' ; request ',fix(ll,l,i)
         print *,'Fixed effect ',ll,l,' out of range at record',i
     +,' max allowed ',noficl(ll),' ; request ',fix(ll,l,i)
             stop
           endif
          enddo
        enddo
      enddo
      do i=1,nofix
       do k=0,noficl(i)
       if(noficl(i).le.12)
     + print *,'fix: ',i,' lev:i ',k,' No. ',(ifixx(i,j,k),j=1,notrt)
       write(66,*)'fix: ',i,' lev:i ',k,' No. ',(ifixx(i,j,k),j=1,notrt)
       enddo
      enddo
c Format for trait.txt
 1999 format(5f6.1)
c Format for fixed.txt
 1998 format(i1)
c Format for ped.txt
 1997 format(3i7)
c-----------+ build xry +-----------
      print *,'              ...XRy'
      do 197 i=1,nolev
         do 197 j=1,notrt
  197      bt(i,j)= 0.d0

      do i=1,noani
        k=0
        if (ix1(i).gt.0) then
         do kk=1,nofix
          k=k+noficl(kk-1)
          do ir=1,notrt
            do ic=1,notrt
              bt(fix(kk,ir,i)+k,ir)=bt(fix(kk,ir,i)+k,ir)+
     +         t(ic,i)*r(rix(ix1(i)),ihmssf(ir,ic,notrt))
            enddo
          enddo
         enddo
        endif
      enddo

      do i=1,nolev
        do j=1,notrt
         rhsb((i-1)*notrt+j)= bt(i,j)
        enddo
      enddo

      do i=1,noft
        write(28) rhsb(i)
      enddo
c-----------+ build Zry +------------
      print *,'              ...ZRy'
      do i=1,noani
        if(ix1(i).eq.0)then
          do ll=1,notrt
            el(ll)=0.d0
          enddo
        else
          do jr=1,notrt
            el(jr)=0.d0
            do ir=1,notrt
              el(jr)=el(jr)+t(ir,i)*r(rix(ix1(i)),ihmssf(ir,jr,notrt))
            enddo
          enddo
        endif
        do ll=1,notrt
          write(29) el(ll)
        enddo
      enddo
c     goto 888
c     call chktim
c-----------------------------------------------------------------------
c     Step 2 (nonzfix)
c----+----+ ... Build the diagonal sparse-stored blocks for each
c............. fixed effect.
      print *,'Step 2: Build the sparse fixed part...'
c     print *,'i',' m',' km',' n',' kn',' l',
c    +' ir',' k',' ic',' fix1','fix2'
      nxrx=0
      do i=1,noani
       if(ix1(i).gt.0)then
        do m=1,nofix
         km=0
         do mm=1,m
          km=km+noficl(mm-1)*notrt
         enddo
         do n=m,nofix
          kn=0
          do nn=1,n
           kn=kn+noficl(nn-1)*notrt
          enddo
          do l=1,notrt
           ir=km+(fix(m,l,i)-1)*notrt+l
           do k=1,notrt
            ic=(fix(n,k,i)-1)*notrt+k+kn
c           print *,i,m,km,n,kn,l,ir,k,ic,fix(m,l,i),fix(n,k,i)
            if(dabs(r(rix(ix1(i)),ihmssf(l,k,notrt))).gt.lilli
     +.and.ir.le.ic)then
c...use hash technique
            call hash(r(rix(ix1(i)),ihmssf(l,k,notrt)),
     +            ir,ic,kr,kc,ca,nofill,ihf)
            nxrx=nxrx+1
            if(mod(nxrx,nofill/10).eq.0)then
             pros=ihf
             pros=pros/nofill*100.d0 
             print *,nxrx,ir,ic,r(rix(ix1(i)),ihmssf(l,k,notrt)),
     +       ' [',pros,' % of hash matrix filled]'
           endif
c..............................
            endif
           enddo
          enddo
         enddo
        enddo
       endif
      enddo
      print *,'...sort the hashed upper-diagonal XRX part'
c...sort the hash vektors
      if(ihf.gt.0)call sort3(nofill,kr,kc,ca)
c...write the non-zero upper-diagonal XRX part to tmp file
      open(20,file=pt2//'dump/xrx.tmp',form='unformatted')
      i99=0
      do i=1,nofill
        if(dabs(ca(i)).ne.0.d0)then
          write(20)kr(i),kc(i),ca(i)
          i99=i99+1
        endif
        kr(i)=0
        kc(i)=0
        ca(i)=0.d0
      enddo
      print *,i99,' elements written to temporary file'
      rewind(20)
c..... Write to vectors..........................
c..... fixed.....
      k=1
      kd=0
      kk=0
      do i=1,nofix
       kk=kk+noficl(i)*notrt
      enddo
      write(66,*)'Order of XRX matrix:',kk
      print *,'Order of XRX matrix:',kk
      print *,'...read temporary file and build sparse vektors'
   30 read(20,end=39)i,j,coef
        if(i.eq.j)then
          d(i)=coef
          kd=kd+1
          goto 30
        else
          if(coef.ne.0.d0)then
            ca(k)=coef
            kr(k)=i
            kc(k)=j
            k=k+1
            ca(k)=coef
            kr(k)=j
            kc(k)=i
            k=k+1
          endif
          goto 30
        endif 
   39 k=k-1
      close(20)
c     call exec('rm /home/naut/HUPPA/mjolk/frjos/impl/dump/xrx.tmp')
c     call exec('rm /home/LBHI/elsa/Kyr/mjolk/frjos/impl/dump/xrx.tmp')
      call exec('rm /home/elsa/Kyr/mjolk/frjos/impl/dump/xrx.tmp')
      write(66,*)
     +'Number of filled cells on diagonal',kd,'; off-diagonal:',k
      print *,'Number of filled cells on diagonal',kd,
     +'; off-diagonal:',k
      print *,'Sort vectors according to coordinates'
      if(k.gt.0)call sort3(k,kr,kc,ca)
      print *,'Count non-zero elements in each row'
      ic=1
      ir=1
      do 246 i=1,nd
        do while (kr(ic).eq.i)
          ka(i)=ka(i)+1
          ic=ic+1
        end do
  246 continue
      print *,'Write to disk...'
c     call chktim
c  Speed up diskwriting by using chunk-writing
c  Default buffer should be checked, here 1024 bytes
      write(55)nd
      write(56)nd
      call iwrthf(ka,nd,iunit55)
      call writhf(d,nd,iunit56)
      write(58)k
      write(59)k
      call iwrthf(kc,k,iunit58)
      call writhf(ca,k,iunit59)
c 999 call chktim
c-----------------------------------------------------------------------
c     Step 3-5  blocks
c.......................................................................

c.....Step 3 starts (di_odi)
      n=noani
      print *,'Step 3-5: Build Diagonal and Off-Diagonal blocks...'
      do i=1,n
        read(13,3999)di(i)
c       if(i.gt.129700)write(*,*)i,di(i)
      enddo
      rewind(13)
 3999 format(f6.3)
      do i=1,n
        e1(i)=0.0
        e2(i)=0.0
      enddo
      do i=1,n
        if((si(i).eq.0).and.(da(i).eq.0))then
          e1(i)=e1(i)+di(i)
          e2(i)=e2(i)+0.0
        else if((si(i).ne.0).and.(da(i).ne.0))then
          if (si(i).lt.da(i)) then
            p= si(i)
            q= da(i)
          else
            p= da(i)
            q= si(i)
          endif
          e1(i)=e1(i)+di(i)
          e1(p)=e1(p)+0.25*di(i)
          e1(q)=e1(q)+0.25*di(i)
          e2(i)=e2(i)-0.50*di(i)
        else if((si(i).eq.0).and.(da(i).ne.0))then
          p=da(i)
          e1(i)=e1(i)+di(i)
          e1(p)=e1(p)+0.25*di(i)
          e2(i)=e2(i)-0.50*di(i)
        else if((da(i).eq.0).and.(si(i).ne.0))then
          p=si(i)
          e1(i)=e1(i)+di(i)
          e1(p)=e1(p)+0.25*di(i)
          e2(i)=e2(i)-0.50*di(i)
        endif
      enddo
c     call chktim
c.....Step 4 starts (block_2)
      old=-9999.0
      ndiff=0
      do 499 i=1,mtrthf
  499   read(17) g_half(i)
      do 498 i=1,noani
  498   a_ind(i)=i
      n=noani
c...sort the di elements by size for all animals, and id accordingly
      call sort2(n,e1,a_ind)
c...just check the limits of # different diagonal elements
      do i=1,noani
         if (e1(i).ne.old) then
            ndiff=ndiff+1
         endif
         old=e1(i)
      enddo
      if(ndiff.le.maxdblc)then
        write(66,*)'Number of different diagonal blocks:',ndiff
        print *,'Number of different diagonal blocks:',ndiff
      else
        write(66,*)'Variable maxdblc too small...stop',ndiff
        print *,'Variable maxdblc too small...stop',ndiff
        stop
      endif
      old=-9999.0
      ndiff=0
c...build all possible inv(A)*inv(G) blocks
      do i=1,noani
         if (e1(i).ne.old) then
            ndiff=ndiff+1
            do j=1,mtrthf
              blokag(ndiff,j)=e1(i)*g_half(j)
            enddo
         endif
         old=e1(i)
c...index 2 keeps information on the running number of di element for animal i
         ix2(i)=ndiff
      enddo
      n=noani
c...now sort by animal id and index 2 accordingly
      call sort2b(n,a_ind,ix2)
      do 493 k=1,ndiff
        do 492 i=1,nocomb
          do i2=1,mtrthf
            suba(i2)=blokag(k,i2)+r(i,i2)
          enddo
          call dkmvhf(suba,v,iflag,notrt)
          write(23)(suba(ll),ll=1,mtrthf)
  492   continue
  493 continue
c     call chktim
c.....Step 5 starts (block_3)
      old=-9999.0
      noff=0
      do 599 i=1,noani
  599   a_ind(i)=i
      call sort2(n,e2,a_ind)
      do 598 i=1,noani
         if (e2(i).ne.old) then
            noff=noff+1
         endif
         old=e2(i)
  598 continue
      if(noff.le.maxoblc)then
        write(66,*)'Different offdiagonal blocks are:',noff
        print *,'Different offdiagonal blocks are:',noff
      else
        write(66,*)'Variable maxoblc is too small...stop',noff
        print *,'Variable maxoblc is too small...stop',noff
        stop
      endif
      noff=0
      old=-9999.0
      do 597 i=1,noani
         if (e2(i).ne.old) then
            noff=noff+1
            do 596 j=1,mtrthf
               blcag2(noff,j)=e2(i)*g_half(j)
  596       continue
         endif
         old=e2(i)
         ix3(i)=noff
  597 continue
      do i=1,noff
        write(25)(blcag2(i,j),j=1,mtrthf)
      enddo
      n=noani
      call sort2b(n,a_ind,ix3)
      write(15,*)ndiff,noff
c     call chktim
c-----------------------------------------------------------------------
c     Step 6 (animfile)
c-----------------------------------------------------------------------
      print *,'Step 6: Build ANIMAL.FILE...'
      do 699 i=noani,1,-1
         aeq=(i-1)*notrt+1
         if (si(i).gt.0) then
            seq=(si(i)-1)*notrt+1
         else
            seq=si(i)
         endif
         if (da(i).gt.0) then
            deq=(da(i)-1)*notrt+1
         else
            deq=da(i)
         endif
         mm=0
         do m=1,nofix
          mm=mm+noficl(m-1)
          do l=1,notrt
           if(fix(m,l,i).gt.0) then
c           print *,m,l,i,noficl(m-1),fix(m,l,i) 
            beq(m,l)=(fix(m,l,i)-1)*notrt+l+mm*notrt
           else
            beq(m,l)=0
           endif
          enddo
         enddo
         write(30)ix1(i),ix2(i),ix3(i),aeq,
     +            ((beq(m,l),m=1,nofix),l=1,notrt),seq,deq
c        write(*,*)ix1(i),ix2(i),ix3(i),aeq,
c    +            ((beq(m,l),m=1,nofix),l=1,notrt),seq,deq
c  If record is used as an indicator of animals 1st rec. aeq should
c  be skipped from the write sentence.
  699 continue
c     call chktim
c     call cumtim(cumstr)
c     write(66,*)' CPU time: ', cumstr
c     call wallcl(dumstr)
c     write(66,*)' Total wall clock time: ', dumstr
c     print *,'               ...done'
c     call fdate(dagtim)
c     write(66,*)'...end of output from program PREIMPL ',dagtim
      write(66,*)'...............................................'
      stop
      end
c     include 'chktim.f'
c=================================================================      SUB03750
      subroutine inoll(nvec,n)                                          SUB03760
c===============================================================        SUB03770
      dimension nvec(n)                                                 sub03780
      do 1 i=1,n                                                        SUB03790
 1    nvec(i)=0                                                         SUB03800
      return                                                            SUB03810
      end                                                               SUB03820
      subroutine noll(b,n)                                              SUB03830
      double precision b(n)                                             SUB03840
      do 1 i=1,n                                                        SUB03850
 1    b(i)=0.d0                                                         SUB03860
      return                                                            SUB03870
      end                                                               SUB03880
c  -------------------------------------------------------------------
c   subroutine for setting a real*8 vector of length n to zero
      subroutine nollr(vec,n)
      real*8 vec(n)
      do 1 i=1,n
    1 vec(i)=0.d0
      return
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
C     SPECIALISED FORM OF IHMSSF FOR I=J                                SUB05030
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
      J1=J-1                                                            SUB05210
      IHMJI=N*J1-J*J1/2+I                                               SUB05220
      RETURN                                                            SUB05230
      END                                                               SUB05240
C=======================================================================
      SUBROUTINE IWRTHF(IC,N,IUNIT)
C=======================================================================
      DIMENSION IC(N)
      IF(N.LE.256)THEN
        WRITE(IUNIT)(IC(II),II=1,N)
        RETURN
      ENDIF
      M=N/256
      I1=-255
      I2=0
      DO 1 I=1,M
      I1=I1+256
      I2=I2+256
 1    WRITE(IUNIT)(IC(II),II=I1,I2)
      I1=I1+256
      WRITE(IUNIT)(IC(II),II=I1,N)
      RETURN
      END
C=======================================================================
      SUBROUTINE WRITHF(C,N,IUNIT)
C=======================================================================
      DOUBLE PRECISION C(N)
      IF(N.LE.128)THEN
        WRITE(IUNIT)(C(II),II=1,N)
        RETURN
      END IF 
      M=N/128
      I1=-127
      I2=0
      DO 1 I=1,M
        I1=I1+128
        I2=I2+128
        WRITE(IUNIT)(C(II),II=I1,I2)
    1 continue
      I1=I1+128
      WRITE(IUNIT)(C(II),II=I1,N)
      RETURN
      END
c-----------------------------------------------------------------
      subroutine sort2(n,ra,rb)
c-----------------------------------------------------------------
*      dimension ra(n),rb(n)
      real ra(n)
      integer*4 rb(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         go to 10
      end
c-----------------------------------------------------------------
      subroutine sort2b(n,ra,rb)
c-----------------------------------------------------------------
*      dimension ra(n),rb(n)
      integer*4 ra(n),rb(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         go to 10
      end
c-----------------------------------------------------------------
      subroutine sort3(n,ra,rb,rc)
c-----------------------------------------------------------------
      real*8 rc(n)
      integer*4 ra(n),rb(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
            rrc=rc(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            rrc=rc(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            rc(ir)=rc(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               rc(1)=rrc
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               rc(i)=rc(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         rc(i)=rrc
         go to 10
      end
c-----------------------------------------------------------------
                 subroutine dkmvhf(a,v,iflag,n)                         sub03980
c-----------------------------------------------------------------
      double precision a(1),v(1),xx,dmax,amax,bmax,zero,dimax           sub04240
      integer iflag(1)                                                  sub04250
      if(n.eq.1)then                                                    sub04270
      xx=a(1)                                                           sub04280
      if(dabs(xx).gt.zero)then                                          sub04290
      a(1)=1.d0/xx                                                      sub04300
      iflag(1)=1                                                        sub04310
      else                                                              sub04320
      a(1)=0.d0                                                         sub04330
      iflag(1)=0                                                        sub04340
      end if                                                            sub04350
      return                                                            sub04360
      end if                                                            sub04370
      n1=n+1                                                            sub04390
      nn=n*n1/2                                                         sub04400
      do 1 i=1,n                                                        sub04410
    1 iflag(i)=0                                                        sub04420
c      set minimum absolute value of diagonal elements for              sub04440
c      non-singularity (machine specific)                              sub04450
       zero=1.d-20                                                      sub04460
c-----------------------------------------------------------------------sub04480
c      start loop over rows/cols                                        sub04490
c-----------------------------------------------------------------------sub04500
      do 8 ii=1,n                                                       sub04510
c      ... find diagonal element with biggest absolute value            sub04530
          dmax=0.d0                                                     sub04540
          amax=0.d0                                                     sub04550
          kk=-n                                                         sub04560
          do 2 i=1,n                                                    sub04570
c       ... check that this row/col has not been processed              sub04580
              if(iflag(i).ne.0)then                                     sub04590
              kk=kk+n1-i                                                sub04600
              else                                                      sub04610
              kk=kk+n1                                                  sub04620
              bmax=dabs(a(kk))                                          sub04630
              if(bmax.gt.amax)then                                      sub04640
             dmax=a(kk)                                                 sub04650
                  amax=bmax                                             sub04660
                  imax=i                                                sub04670
              end if                                                    sub04680
            kk=kk-i                                                     sub04690
             end if                                                     sub04700
    2     continue                                                      sub04710
c      ... check for singularity                                        sub04720
          if(amax.le.zero)go to 11                                      sub04730
c      ... all elements scanned,set flag                                sub04740
          iflag(imax)=ii                                                sub04750
c      ... invert diagonal                                              sub04770
          dimax=1.d0/dmax                                               sub04780
c      ... devide elements in row/col pertaining to the biggest         sub04790
c      diagonal element by dmax                                         sub04800
          il=imax-n                                                     sub04810
          do 3 i=1,imax-1                                               sub04820
              il=il+n1-i                                                sub04830
              xx=a(il)                                                  sub04840
              if(xx.ne.0)a(il)=xx*dimax                                 sub04850
    3     v(i)=xx                                                       sub04860
c      ... new diagonal element                                         sub04870
          il=il+n1-imax                                                 sub04880
          a(il)=-dimax                                                  sub04890
          do 4 i=imax+1,n                                               sub04900
              il=il+1                                                   sub04910
              xx=a(il)                                                  sub04920
              if(xx.ne.0)a(il)=xx*dimax                                 sub04930
    4     v(i)=xx                                                       sub04940
c      ... adjust the other rows/cols :                                 sub04950
c      a(i,j)=a(i,j)-a(i,imax)*a(j,imax)/a(imax,imax)                   sub04960
          ij=0                                                          sub04970
          do 7 i=1,n                                                    sub04980
              if(i.eq.imax)then                                         sub04990
      ij=ij+n1-i                                                        sub05000
      else                                                              sub05010
              xx=v(i)                                                   sub05020
              if(xx.ne.0.d0)then                                        sub05030
                  xx=xx*dimax                                           sub05040
                  do 5 j=i,n                                            sub05050
                      ij=ij+1                                           sub05060
                      if(j.ne.imax)a(ij)=a(ij)-xx*v(j)                  sub05070
    5             continue                                              sub05080
              else                                                      sub05090
    6             ij=ij+n1-i                                            sub05100
              end if                                                    sub05110
      end if                                                            sub05120
    7     continue                                                      sub05130
c      ... repeat until all rows/cols are processed                     sub05150
    8 continue                                                          sub05160
c-----------------------------------------------------------------------sub05180
c      end loop over rows/cols                                          sub05190
c-----------------------------------------------------------------------sub05200
c      ... reverse sign                                                 sub05210
      do 9 i=1,nn                                                       sub05220
    9 a(i)=-a(i)                                                        sub05230
c      ... and that's it                                               sub05240
c     print 10,n                                                        sub05250
   10 format(' full rank matrix inverted, order =',i5)                  sub05260
c      return rank as last element of flag vector                       sub05270
       iflag(n)=n                                                       sub05280
      return                                                            sub05300
c-----------------------------------------------------------------------sub05320
c      matrix not of full rank, return generalised inverse              sub05330
c-----------------------------------------------------------------------sub05340
   11 irank=ii-1                                                        sub05360
      ij=0                                                              sub05370
      do 14 i=1,n                                                       sub05380
          if(iflag(i).eq.0)then                                         sub05390
c      ... set remaining n-ii rows/cols to zero                         sub05400
              do 12 j=i,n                                               sub05410
                  ij=ij+1                                               sub05420
                  a(ij)=0.d0                                            sub05430
   12         continue                                                  sub05440
          else                                                          sub05450
              do 13 j=i,n                                               sub05460
                  ij=ij+1                                               sub05470
                  if(iflag(j).ne.0)then                                 sub05480
c      ... reverse sign for ii-1 rows/cols previously processed         sub05490
                      a(ij)=-a(ij)                                      sub05500
                  else                                                  sub05510
                      a(ij)=0.d0                                        sub05520
                  end if                                                sub05530
   13         continue                                                  sub05540
          end if                                                        sub05550
   14 continue                                                          sub05570
c     write(*,15)n,irank                                                sub05580
c  15 format(' generalised inverse of matrix with order =',i5,          sub05590
c    1  '   and rank =',i5)                                             sub05600
      iflag(n)=irank                                                    sub05610
      return                                                            sub05630
      end                                                               sub05640
c     include '/home/agust/assub/cputim.f'
c     include '/home/agust/assub/cumtim.f'
c     include '/home/agust/assub/wallcl.f'

      subroutine hash(y,j1,k1,ind1,ind2,ind3,m,nr)
      integer j1,k1,m,nr
      real*8 y,ind3(m)
      integer*4 iaddr,ind1(m),ind2(m)
      integer ie,ieq,izer,iaddress,k,j
      data ie/641/
      iaddr=433*j1+53*k1
      iaddress=mod(iabs(iaddr),m)+1
      do 10 k=1,200
        j=iaddress
        if(ind1(iaddress).ne.j1.or.ind2(iaddress).ne.k1)then
          ieq=1
        else
          ieq=0
        endif
        if(ind1(iaddress).ne.0)then
          izer=1
        else
          izer=0
        endif
        if(izer.eq.0.or.ieq.eq.0)then
          if(izer.eq.0)then
            ind1(iaddress)=j1
            ind2(iaddress)=k1
            ind3(iaddress)=y
            nr=nr+1
          else
            ind3(iaddress)=ind3(iaddress)+y
          endif
          return
        endif
        iaddress=mod(iaddress+ie-1,m)+1
   10 continue
      write(*,60)nr,m
   60 format(' hash matrix too small,filled',i8,' out of',i8)
      stop
      end
