C Program to code animals in running order
C Main file is sorted on AR-KYN-BU-NUM
      integer iid
c     character cx*74,pth*32
c     pth='/home/agust/skuggi_kyr/utli2/g8/'
!!    character cx*74,pth*29
!!    pth='/home/LBHI/elsa/Kyr/utli2/g8/'
      character cx*74,pth*24
      pth='/home/elsa/Kyr/utli2/g8/'
      open(10,file=pth//'conf.Wxx2',status='OLD')
      open(20,file=pth//'conf.Rmain',status='NEW')
      i=0
c.....Read file.......................... 
  199 read(10,210,end=999)cx
        i=i+1
        if(mod(i,1000).eq.0)Print *,'Read: ',i,cx
        write(20,220)i,cx
      goto 199
  210 format(a74)
  220 format(i6,a74)
  499 format('0Total records read: ',i6)
  999 print 499,i
      close(10)
      close(20)
c     call exec('rm conf.Wxx2')
c     call exec('sync')
      stop
      end
