c-----------------------------------------------------------------------
      program impldiv
c...calculate solutions for each animal based on own records only                 
c   a) Reads all previously prepared diagonal blocks and stores in vectors.
c      Elements of ZRy are read in vectors in memory and also all 
c      combinations of the residual covariance matrix R. Finally the animal
c      file is read into memory including pointers to all blocks and 
c      equations. Reads in then b-solutions from the IMPL-run.
c   b) Compute new solutions for u starting with the youngest animal where
c      for each animal the following steps are carried out:
c      1) Adjust the RHS of animal for it's b effect.
c      2) Calculate new solution for the animal by multiplying it's 
c         diagonal block with the RHS.
c      3) Write out solutions.
c.......................................................................
c     sept. 1996
c     agust@goliat.hfs.slu.se <Agust Sigurdsson>
c-----------------------------------------------------------------------
      include 'param.i'
      parameter(maxta=notrt*maxani)
      parameter(maxaft=maxft+maxta)
      character*9 cumstr,dumstr
      real*8 rhss(maxta),r(maxcomb,mtrthf),b(maxft),
     +        s(maxta),d(maxcomb,maxdblc,mtrthf),tmp(notrt),tmp0
      integer*4 aeq,seq,deq,nbi,ix2,ix3,noficl(0:maxfix),rix(0:norbl)
      integer*4 beq(maxfix,notrt),ix1
c     character*9 cumstr,dumstr,dagtim*25
c      call cumtim(cumstr)
c      call wallcl(dumstr)
      open(14,file='data/model.txt')
      open(15,file='dump/Winfo.txt')
      open(18,file='dump/Wimpl18.bin',form='unformatted')
      open(19,file='dump/Wimpl19.bin',form='unformatted')
      open(23,file='dump/Wimpl23.bin',form='unformatted')
      open(29,file='dump/Wimpl29.bin',form='unformatted')
      open(30,file='dump/Wimpl30.bin',form='unformatted')
c...CHANGE
      open(40,file='keep/Rimpl40.txt')
      open(41,file='keep/Rimpl41.txt')
      open(50,file='keep/Rimpl50.txt')
      open(66,file='keep/Rimpldiv.log')
c.........NULLLA
      deq=0
      nbi=0
      ix2=0
      ix3=0
      ix1=0
      seq=0
      aeq=0
      tmp0=0.d0
      ix1=0
      do i=1,maxfix
	do j=1,notrt
	 beq(i,j)=0
	enddo
      enddo
      do i=0,maxfix
        noficl(i)=0
      enddo
      do i=0,norbl
       rix(i)=0
      enddo
      call nollr(rhss,maxta)
      call nollm(r,maxcomb,mtrthf)
      call nollr(b,maxft)
      call nollr(s,maxta)
      call nollr3(d,maxcomb,maxdblc,mtrthf)
      call nollr(tmp,notrt)
c.........................................
c      call fdate(dagtim)
c      write(66,*)'Output from program IMPLDIV ',dagtim
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
      nota=notrt*noani
c...Read inv(R), diagonal blocks, RHS for animal effects and BLUE solutions
      do i=1,nocomb
        read(18)(r(i,j),j=1,mtrthf)
      enddo
      do i=0,norbl
        read(19)rix(i)
      enddo
      do k=1,nodbl
        do i=1,nocomb
          read(23)(d(i,k,j2),j2=1,mtrthf)
        enddo
      enddo
        do 291 i=1,noft
          b(i)=0.d0
  291   continue
        do 203 i=1,nota 
          read(29)rhss(i)
          s(i)=0.d0
  203   continue
        do i=1,nolev
          read(40,791)(b(notrt*(i-1)+ii),ii=1,notrt)
        enddo
c     call cumtim(cumstr)
c     call wallcl(dumstr)
c...Start with youngest animal
      do 219 k=1,noani
        read(30)ix1,ix2,ix3,aeq,((beq(m,l),m=1,nofix),l=1,notrt),seq,deq
c...adjust the RHS of animal for BLUE effects
      if (ix1.ne.0) then
        do ll=1,notrt
         tmp(ll)=0.d0
         do kk=1,nofix
          tmp(ll)=tmp(ll)+b(beq(kk,ll))
         enddo
        enddo
        do ir=1,notrt
         tmp0=0.d0
         do ic=1,notrt
          tmp0=tmp0+(r(rix(ix1),ihmssf(ir,ic,notrt))*tmp(ic))
         enddo
         rhss(aeq+(ir-1))=rhss(aeq+(ir-1))-tmp0
        enddo
      endif
c...calculate u(own) by multiplying diagonal block and adjusted RHS
       do ir=1,notrt
         s(aeq+(ir-1))=0.d0
         do ic=1,notrt
           s(aeq+(ir-1))=s(aeq+(ir-1))+
     +     d(rix(ix1),ix2,ihmssf(ir,ic,notrt))*rhss(aeq+(ic-1))
         enddo
       enddo
  219 continue
c...until oldest animal
c      call cumtim(cumstr)
c      call wallcl(dumstr)
c      write(66,*)' CPU time: ', cumstr
c      write(66,*)' Total wall clock time: ', dumstr
c      print *,' CPU time: ', cumstr
c      print *,' Total wall clock time: ', dumstr
c  ...write out solutions
      print *,'Writing out solutions...'
      do i=1,noani
        write(50,791)(s(notrt*(i-1)+ii),ii=1,notrt)
      enddo
c Format for solution file Rimpl50.txt
  791 format(5f14.6)
c---------------------- ...done ----------------------------------------
c     call fdate(dagtim)
c     write(66,*)'...end of output from program IMPLDIV ',dagtim
      write(66,*)'............................................'
      print *,'            ...done'
  999 stop
      end
      INTEGER FUNCTION IHMSSF(I,J,N)                                    
      IF(I.LE.J)THEN                                                    
      I1=I-1                                                            
      IHMSSF=N*I1-I*I1/2+J                                              
      ELSE                                                              
      J1=J-1                                                            
      IHMSSF=N*J1-J*J1/2+I                                             
      END IF                                                            
      RETURN                                                            
      END                                                               
c      include '/home/agust/assub/cputim.f'
c      include '/home/agust/assub/cumtim.f'
c      include '/home/agust/assub/wallcl.f'
!      include '/home/agust/agusts/assub/nollm.f'
!      include '/home/agust/agusts/assub/nollr.f'
!      include '/home/agust/agusts/assub/nollr3.f'
!!     include '/home/LBHI/elsa/elsaagust/agusts/assub/nollm.f'
!!     include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr.f'
!!     include '/home/LBHI/elsa/elsaagust/agusts/assub/nollr3.f'
       include '/home/elsa/elsaagust/agusts/assub/nollm.f'
       include '/home/elsa/elsaagust/agusts/assub/nollr.f'
       include '/home/elsa/elsaagust/agusts/assub/nollr3.f'
