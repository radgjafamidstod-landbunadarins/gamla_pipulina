c-----------------------------------------------------------------------
       program inbr
c...IF PHANTOM PARENTS ARE INCLUDED CHECK read(11....
c      Fast calculation of inbreeding coefficients. Ref: Sigurdsson,A.& 
c      Arnason, Th. Acta Agric.Scand. 1995:45,1-10; Henderson,C.R.
c      Biometrics 1976:32,69-83; Quaas,R.L. Biometrics 1976:32:949-953
c.......................................................................
c      oct 1988, revised juli 1992 
c      agusts@centrum.is <Agust Sigurdsson>
c-----------------------------------------------------------------------
      include 'param.i'
      parameter(maxped= 2600)
      real*8 vecV(maxped),vecU(maxped),fcoeff(maxani)
      real*8 mf(2)
      character*9 cumstr,dumstr,dagtim*25
      real tarray(2)
      integer*4 id(maxani),s(maxani),d(maxani),pid(maxped),
     +        ps(maxped),pd(maxped),fs(maxped),fd(maxped)
     +        ,noani,noherd
      integer*2 iflag(maxani)
c.......................................................................
c      call fdate(dagtim)
      write(66,*)'Output from program INBR ',dagtim
      write(66,*)'.....................................................'
      open(10,file='data/model.txt',status='old')
      open(11,file='data/ped.txt',status='old')
      read(10,*)noani
      iun21=21
      istart=1
      open(20,file='keep/f_coeff.bin')
      open(21,file='keep/di.txt')
      open(66,file='keep/inbr.log')
      do i=1,noani
        read(11,1111)iix,s(i),d(i)
c       if(s(i).le.19)s(i)=0
c       if(d(i).le.19)d(i)=0
      enddo
 1111 format(3i7)
 1112 format(i7,i6,i2)
      write(*,*)'Reading pedigree done...'
      write(*,299)noani,istart
  299 format('Calculate F and "di" element for animal ',i6,' to ',i6)
      nped=0
      mp=maxped
      ina=noani
c      call cumtim(cumstr)
c      call wallcl(dumstr)
      do 158 iani=noani,istart,-1
        id(iani)=iani
        if(iflag(iani).eq.0)then
        if(s(iani).eq.0.or.d(iani).eq.0)then
          fcoeff(iani)=1.d0
          iflag(iani)=1
        else
          iani2=iani
          call pedtrace(iani2,id,s,d,pid,ps,pd,nped,mp,ina)
          if(nped.gt.ideep)ideep=nped
          call sort3b(nped,pid,ps,pd)
          call renumb(pid,ps,pd,mp,fs,fd,nped)
          call fcalc(fs,fd,vecV,vecU,mp,nped)
          do 777 ifco=1,nped
            if(iflag(pid(ifco)).eq.0)then
              fcoeff(pid(ifco))=vecU(ifco)
              iflag(pid(ifco))=1
            endif
  777     continue
        endif
        endif
        if(mod(iani,100).eq.0)write(*,*)iani
  158 continue
c      call cumtim(cumstr)
c      call wallcl(dumstr)
c      print *,' CPU time: ', cumstr
c      print *,' Total wall clock time: ', dumstr
c      write(66,*)' CPU time: ', cumstr
c      write(66,*)' Total wall clock time: ', dumstr
      write(*,*)'Calculate F done'
      print *,'Deepest pedigree: ',ideep
      write(66,*)'Deepest pedigree: ',ideep
      do 776 ifco=1,noani
        if(s(ifco).gt.0.and.d(ifco).gt.0)then
          igt0=igt0+1
          mf(1)=mf(1)+fcoeff(ifco)
          if(fcoeff(ifco).gt.1.d0)then
            ifgt0=ifgt0+1
            mf(2)=mf(2)+fcoeff(ifco)
          endif
        endif
        write(20,*)fcoeff(ifco)
  776 continue
      print *,'Number of animals with both parent registrated:',igt0
      print *,'Mean F:',mf(1)/igt0
      print *,'Number of animals with F>0.0:',ifgt0
      print *,'Mean F:',mf(2)/ifgt0
      write(66,*)'Number of animals with both parent registrated:',igt0
      write(66,*)'Mean F:',mf(1)/igt0
      write(66,*)'Number of animals with F>0.0:',ifgt0
      write(66,*)'Mean F:',mf(2)/ifgt0
      call dicalc(fcoeff,s,d,istart,ina,mp,iun21)
 2999 format(f10.8)
      write(*,*)'...done'
c      call fdate(dagtim)
      write(66,*)'...end of output from program INBR ',dagtim
      write(66,*)'.....................................................'
      stop
      end
c-----------------------------------------------------------------------
      subroutine pedtrace(igid,id,s,d,pid,ps,pd,np,mp,noani)
c-----------------------------------------------------------------------
      integer gs,gd,levp,newp,copperf,
     +        pid(mp),ps(mp),pd(mp),
     +        id(noani),s(noani),d(noani)
      nped=np
      copperf=0
      newp=1
      pid(newp)=igid
      ps(newp)=s(igid)
      pd(newp)=d(igid)
      gs=s(igid)
      gd=d(igid)
      levp=newp
      if(gs.eq.0.and.gd.eq.0)copperf=1
  199 if(ps(levp).eq.0.or.inlist(ps(levp),newp,mp,pid).eq.1)goto 190
        igid=ps(levp)
        gs=s(ps(levp))
        gd=d(ps(levp))
        newp=newp+1
        pid(newp)=igid
        ps(newp)=gs
        pd(newp)=gd
  190 if(pd(levp).eq.0.or.inlist(pd(levp),newp,mp,pid).eq.1)goto 189
        igid=pd(levp)
        gs=s(pd(levp))
        gd=d(pd(levp))
        newp=newp+1
        pid(newp)=igid
        ps(newp)=gs
        pd(newp)=gd
  189 levp=levp+1
      if(levp.gt.newp.or.copperf.eq.1)goto 198
      goto 199
  198 np=newp
      return
      end
c-----------------------------------------------------------------------
      function inlist(ichkid,newp,mp,pid)
c-----------------------------------------------------------------------
      integer pid(mp)
      ifound=0
      do 299 i=1,newp
  299   if(pid(i).eq.ichkid)ifound=1
      inlist=ifound
      return
      end
c-----------------------------------------------------------------------
      subroutine renumb(pid,ps,pd,mp,fs,fd,nped)
c-----------------------------------------------------------------------
      integer pid(mp),ps(mp),pd(mp),
     +        fs(mp),fd(mp)
      do 199 i=1,nped
        if(ps(i).eq.0)then
          fs(i)=0
        else
          fs(i)=isubsc(pid,mp,ps(i))
        endif
        if(pd(i).eq.0)then
          fd(i)=0
        else
          fd(i)=isubsc(pid,mp,pd(i))
        endif
  199 continue
      return
      end
c-----------------------------------------------------------------------
      function isubsc(pid,mp,inum)
c-----------------------------------------------------------------------
      integer pid(mp)
      do 199 i=1,inum
        if(pid(i).eq.inum)then
          isubsc=i
          goto 999
        endif
  199 continue
  999 return
c.....isubsc done
      end
c-----------------------------------------------------------------------
      subroutine fcalc(ps,pd,vecV,vecU,mp,np)
c-----------------------------------------------------------------------
      real*8 vecV(mp),vecU(mp),rp,rq
      integer ps(mp),pd(mp)
      nped=np
      do 195 i=1,nped
        vecV(i)=0.d0
        vecU(i)=0.d0
  195 continue
      do 199 i=1,nped
        do 198 j=i,nped
          if(ps(j).eq.0.and.pd(j).eq.0)then
            if(i.eq.j)then
              vecV(j)=1.d0
            else
              vecV(j)=0.d0
            endif
          else if(ps(j).ne.0.and.pd(j).eq.0)then
            if(i.eq.j)then
              vecV(j)=sqrt(1.d0-.25d0*vecU(ps(j)))
            else
              if(ps(j).lt.i)then
                vecV(j)=0.d0
              else
                vecV(j)=0.5d0*vecV(ps(j))
              endif
            endif
          else if(ps(j).eq.0.and.pd(j).ne.0)then
            if(i.eq.j)then
              vecV(j)=sqrt(1.d0-.25d0*vecU(pd(j)))
            else
              if(pd(j).lt.i)then
                vecV(j)=0.d0
              else
                vecV(j)=0.5d0*vecV(pd(j))
              endif
            endif
          else if(ps(j).ne.0.and.pd(j).ne.0)then
            if(i.eq.j)then
              vecV(j)=sqrt(1.d0-.25d0*(vecU(ps(j))+vecU(pd(j))))
            else
              if(ps(j).lt.i)then
                rp=0.d0
              else
                rp=vecV(ps(j))
              endif
              if(pd(j).lt.i)then
                rq=0.d0
              else
                rq=vecV(pd(j))
              endif
              vecV(j)=0.5d0*(rp+rq)
            endif
          endif
          vecU(j)=vecU(j)+vecV(j)**2
  198   continue
  199 continue
      return
      end
c-----------------------------------------------------------------------
      subroutine dicalc(a,s,d,ist,n,m,iun)
c-----------------------------------------------------------------------
      real*8 a(m),di
      integer s(m),d(m)
      do 112 i=ist,n
        if(s(i).gt.0.and.d(i).gt.0)then
          di=1.d0/(.5d0-.25d0*(a(s(i))+a(d(i))-2.d0))
        else if(s(i).eq.0.and.d(i).eq.0)then
          di=1.d0
        else if(s(i).eq.0.and.d(i).gt.0)then
          di=1.d0/(.75d0-.25d0*(a(d(i))-1.d0))
        else if(d(i).eq.0.and.s(i).gt.0)then
          di=1.d0/(.75d0-.25d0*(a(s(i))-1.d0))
        endif
c  Only with 3 decimals to save time in BV estimation programs
        write(iun,255)di
  112 continue
  255 format(f6.3)
      return
      end
c     include '/home/agust/agusts/assub/sorting.f'
!!    include '/home/LBHI/elsa/elsaagust/agusts/assub/sorting.f'
      include '/home/elsa/elsaagust/agusts/assub/sorting.f'
c      include '/home/agust/assub/wallcl.f'
c      include '/home/agust/assub/cumtim.f'
c      include '/home/agust/assub/cputim.f'
