C Program to code animals in running order
C Main file is sorted on AR-KYN-BU-NUM
      integer iid
      character cx*64
      open(10,file='vinnsla/mjalt.Wxx2',status='OLD')
      open(20,file='vinnsla/mjalt.Rmain',status='NEW')
      i=0
c.....Read file.......................... 
  199 read(10,210,end=999)cx
        i=i+1
        if(mod(i,1000).eq.0)Print *,'Read: ',i,cx
        write(20,220)i,cx
      goto 199
  210 format(a64)
  220 format(i6,a64)
  499 format('0Total records read: ',i6)
  999 print 499,i
      close(10)
      close(20)
c     call exec('rm mjalt.Wxx2')
c     call exec('sync')
      stop
      end
