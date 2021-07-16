c...Forritid les, radar a bu og sameinar nyju safnskrarnar
c   og skrifar ut nyja safnskra med bu-kodum
c...2000 vandi leystur
      integer ny(40)
      logical nytt
      nytt=.true.
c...sort and join 
  239 format(i6,3(i4,i7,i4),i1,i6,i4,12i2,9i5,6i4)
c     goto 99
      print *,'Her1'
c     call exec('rm /home/LBHI/agust/BASSI/mjolk/Wxx1')
      print *,'Her2'
c     call exec('sort +0.52 
      call exec('sort -n +0.10 -0.17 
c    +/home/agust/skuggi_kyr/mjolk/vinnsla/Wxx0
c    + -o/home/agust/skuggi_kyr/mjolk/vinnsla/Wxx1')
!    +/home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx0
!    + -o/home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx1')
     +/home/elsa/Kyr/mjolk/vinnsla/Wxx0
     + -o/home/elsa/Kyr/mjolk/vinnsla/Wxx1')
c     call exec('rm vinnsla/Wproduct.upd vinnsla/Wproducta.add')
c     call exec('sync')
      print *,'Her4'
c     stop
c     goto 9
   99 print *,'Her4'
c.........................................
      if(nytt)then
c     open(10,file='/home/agust/skuggi_kyr/mjolk/vinnsla/Wxx1')
c     open(20,file='/home/agust/skuggi_kyr/mjolk/vinnsla/Wxx2')
!     open(10,file='/home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx1')
!     open(20,file='/home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx2')
      open(10,file='/home/elsa/Kyr/mjolk/vinnsla/Wxx1')
      open(20,file='/home/elsa/Kyr/mjolk/vinnsla/Wxx2')
      else
c     open(10,file='/home/agust/skuggi_kyr/mjolk/framl/Wxx1')
c     open(20,file='/home/agust/skuggi_kyr/mjolk/framl/Wxx2')
!     open(10,file='/home/LBHI/elsa/Kyr/mjolk/framl/Wxx1')
!     open(20,file='/home/LBHI/elsa/Kyr/mjolk/framl/Wxx2')
      open(10,file='/home/elsa/Kyr/mjolk/framl/Wxx1')
      open(20,file='/home/elsa/Kyr/mjolk/framl/Wxx2')
      endif
      lasth=0
      ih1=0
c.....Read files..........................
  123 read(10,239,end=199)ny
c       write(*,221)ny
        if(ny(3).gt.lasth)then
          ih1=ih1+1
          lasth=ny(3)
          print *,'New herd: ',ih1
        endif
c... store informations in correct cells for this cow
          ny(13)=ih1
c... Write out to new file
        write(20,239)ny
      goto 123
  199 print *,'Number of herds: ',ih1
c......Format for I/O.....................
      close(10)
      close(20)
c      goto 9
      if(nytt)then
c     call exec('rm /home/agust/skuggi_kyr/mjolk/vinnsla/Wxx1')
!     call exec('rm /home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx1')
      call exec('rm /home/elsa/Kyr/mjolk/vinnsla/Wxx1')
      call exec('sort -n +0.6 -0.17 +0.17 -0.21
c    + /home/agust/skuggi_kyr/mjolk/vinnsla/Wxx2
c    + -o/home/agust/skuggi_kyr/mjolk/vinnsla/Wxx1')
!    + /home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx2
!    + -o/home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx1')
     + /home/elsa/Kyr/mjolk/vinnsla/Wxx2
     + -o/home/elsa/Kyr/mjolk/vinnsla/Wxx1')
c     call exec('rm /home/agust/skuggi_kyr/mjolk/vinnsla/Wxx2')
!     call exec('rm /home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx2')
      call exec('rm /home/elsa/Kyr/mjolk/vinnsla/Wxx2')
      else
c     call exec('rm /home/agust/skuggi_kyr/mjolk/framl/Wxx1')
!     call exec('rm /home/LBHI/elsa/Kyr/mjolk/framl/Wxx1')
      call exec('rm /home/elsa/Kyr/mjolk/framl/Wxx1')
      call exec('sort -n +0.6 -0.17 +0.17 -0.21
c    + /home/agust/skuggi_kyr/mjolk/framl/Wxx2
c    +  -o/home/agust/skuggi_kyr/mjolk/framl/Wxx1')
!    + /home/LBHI/elsa/Kyr/mjolk/framl/Wxx2
!    +  -o/home/LBHI/elsa/Kyr/mjolk/framl/Wxx1')
     + /home/elsa/Kyr/mjolk/framl/Wxx2
     +  -o/home/elsa/Kyr/mjolk/framl/Wxx1')
c     call exec('rm /home/agust/skuggi_kyr/mjolk/vinnsla/Wxx2')
!     call exec('rm /home/LBHI/elsa/Kyr/mjolk/vinnsla/Wxx2')
      call exec('rm /home/elsa/Kyr/mjolk/vinnsla/Wxx2')
      endif
    9 call exec('sync')
      stop
      end
