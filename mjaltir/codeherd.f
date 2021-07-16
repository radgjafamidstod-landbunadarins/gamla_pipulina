c...Forritid les, radar a bu og sameinar nyju safnskrarnar
c   og skrifar ut nyja safnskra med bu-kodum
      integer ny(10)
c...sort and join 
      call exec('rm vinnsla/mjalt.Wxx1')
      call exec('sort +0.4 vinnsla/mjaltir.gogn2 -ovinnsla/mjalt.Wxx1')
      call exec('sync')
c.........................................
      open(10,file='vinnsla/mjalt.Wxx1')
      open(20,file='vinnsla/mjalt.Wxx2')
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
        write(20,220)ny(1),2,(ny(k),k=2,7),ih1,(ny(l),l=8,10)
      goto 123
  199 print *,'Number of herds: ',ih1
c......Format for I/O.....................
  210 format(i4,i6,i4,i7,i4,i6,i4,i3,2i1)
  220 format(i4,i1,i6,i4,i7,i4,i6,i4,i4,i3,2i1)
      close(10)
      close(20)
      call exec('rm vinnsla/mjalt.Wxx1')
      call exec('sort vinnsla/mjalt.Wxx2 -ovinnsla/mjalt.Wxx1')
c     call exec('rm mjalt.Wxx2')
      call exec('sync')
      stop
      end

