C Program to code animals in running order
C Main file is sorted on AR-KYN-BU-NUM
      integer iid
c     character cx*149,pth*29
!     character cx*149,pth*26
      character cx*149,pth*21
      logical nytt
c     pth='/home/agust/skuggi_kyr/mjolk/'
!     pth='/home/LBHI/elsa/Kyr/mjolk/'
      pth='/home/elsa/Kyr/mjolk/'
      nytt=.true.
      if(nytt)then
      open(10,file=pth//'vinnsla/Wxx2',status='OLD')
      open(20,file=pth//'vinnsla/Rmain',status='NEW')
      else
      open(10,file='framl/Wxx2',status='OLD')
      open(20,file='framl/Rmain',status='NEW')
      endif
      i=0
c.....Read file.......................... 
  199 read(10,209,end=999)cx
        i=i+1
        if(mod(i,1000).eq.0)Print *,'Read: ',i,cx
        write(20,210)i,cx
      goto 199
  209 format(a149)
  210 format(i6,a149)
  499 format('0Total records read: ',i7)
  999 print 499,i
      close(10)
      close(20)
c      if(nytt)then
c      call exec('rm vinnsla/Wxx2')
c      else
c      call exec('rm framl/Wxx2')
c      endif
      call exec('sync')
      stop
      end
