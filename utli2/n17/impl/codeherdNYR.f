c....ATH utli2#utlit
c...Forritid les, radar a bu og sameinar nyju safnskrarnar
c   og skrifar ut nyja safnskra med bu-kodum
      integer ny(23)
c...sort and join 
c     call exec('rm /home/agust/skuggi_kyr/utli2/n17/conf.Wxx1')
!     call exec('rm /home/LBHI/elsa/Kyr/utli2/n17/conf.Wxx1')
      call exec('rm /home/elsa/Kyr/utli2/n17/conf.Wxx1')
      call exec('sort -k1.5 
c    +/home/agust/skuggi_kyr/utli2/n17/conf.new
c    + -o /home/agust/skuggi_kyr/utli2/n17/conf.Wxx1')
!!   +/home/LBHI/elsa/Kyr/utli2/n17/conf.new
!!   + -o /home/LBHI/elsa/Kyr/utli2/n17/conf.Wxx1')
     +/home/elsa/Kyr/utli2/n17/conf.new
     + -o /home/elsa/Kyr/utli2/n17/conf.Wxx1')
      call exec('sync')
c.........................................
      open(10,file=
c    +'/home/agust/skuggi_kyr/utli2/n17/conf.Wxx1')
!!   +'/home/LBHI/elsa/Kyr/utli2/n17/conf.Wxx1')
     +'/home/elsa/Kyr/utli2/n17/conf.Wxx1')
      open(20,file=
c    +'/home/agust/skuggi_kyr/utli2/n17/conf.Wxx2')
!!   +'/home/LBHI/elsa/Kyr/utli2/n17/conf.Wxx2')
     +'/home/elsa/Kyr/utli2/n17/conf.Wxx2')
      lasth=0
      ih1=0
c.....Read files..........................
  123 read(10,210,end=199)ny
        if(ny(2).gt.lasth)then
          ih1=ih1+1
          lasth=ny(2)
          print *,'New herd: ',ih1
        endif
c... store informations in correct cells for this cow
c... Write out to new file
        write(20,220)(ny(k),k=1,3),ih1,(ny(l),l=4,23)
      goto 123
  199 print *,'Number of herds: ',ih1
c......Format for I/O.....................
  210 format(i4,i7,i4,i3,i1,i2,17i2)
  220 format(i4,i7,i4,i4,i3,i1,i2,17i2)
      close(10)
      close(20)
c     call exec('rm /home/agust/skuggi_kyr/utli2/n17/conf.Wxx1')
!!    call exec('rm /home/LBHI/elsa/Kyr/utli2/n17/conf.Wxx1')
      call exec('rm /home/elsa/Kyr/utli2/n17/conf.Wxx1')
      call exec('sort -n 
c    +/home/agust/skuggi_kyr/utli2/n17/conf.Wxx2
c    + -o/home/agust/skuggi_kyr/utli2/n17/conf.Wxx1')
!!   +/home/LBHI/elsa/Kyr/utli2/n17/conf.Wxx2
!!   + -o/home/LBHI/elsa/Kyr/utli2/n17/conf.Wxx1')
     +/home/elsa/Kyr/utli2/n17/conf.Wxx2
     + -o/home/elsa/Kyr/utli2/n17/conf.Wxx1')
c     call exec('rm conf.Wxx2')
      call exec('sync')
      stop
      end

