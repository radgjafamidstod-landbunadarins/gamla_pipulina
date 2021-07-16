c...Program to split new ms-file in ms**a ms**b ms**3
c...mjaltirjan2009 skal vera sorterud a numer og sidan ar konnunar
      character kyr*15,rest*23,lastk*15
c     open(10,file='safn/mjaltirnov2014')
c     open(20,file='safn/mjal1411a')
c     open(21,file='safn/mjal1411b')
!     open(10,file='safn/mjaltirmai2015')
!!    open(10,file='safn/mjaltirnov2015')
!     open(10,file='safn/mjaltirmai2016')
!     open(10,file='safn/mjaltirnov2016')
!     open(10,file='safn/mjaltirmai2017')
!     open(10,file='safn/mjaltirokt2017')
!     open(10,file='safn/mjaltirmai2018')
!     open(10,file='safn/mjaltirjan2019')
!     open(10,file='safn/mjaltirokt2019')
!     open(10,file='safn/mjaltirjan2020')
!     open(10,file='safn/mjaltirjun2020')
      open(10,file='safn/mjaltirjan21')
!     open(20,file='safn/mjal1505a')
!     open(21,file='safn/mjal1505b')
!!    open(20,file='safn/mjal1511a')
!!    open(21,file='safn/mjal1511b')
!     open(20,file='safn/mjal1605a')
!     open(20,file='safn/mjal1611a')
!     open(20,file='safn/mjal1705a')
!     open(20,file='safn/mjal1710a')
!     open(20,file='safn/mjal1805a')
!     open(20,file='safn/mjal1901a')
!     open(20,file='safn/mjal1910a')
!     open(20,file='safn/mjal2001a')
!     open(20,file='safn/mjal2006a')
      open(20,file='safn/mjal2101a')
!     open(21,file='safn/mjal1605b')
!     open(21,file='safn/mjal1611b')
!     open(21,file='safn/mjal1705b')
!     open(21,file='safn/mjal1710b')
!     open(21,file='safn/mjal1805b')
!     open(21,file='safn/mjal1901b')
!     open(21,file='safn/mjal1910b')
!     open(21,file='safn/mjal2001b')
!     open(21,file='safn/mjal2006b')
      open(21,file='safn/mjal2101b')
      lastk='xxxxxxxxxxxxxxx'
    2 read(10,'(a15,a23)',end=9)kyr,rest
        if(kyr.ne.lastk)then
          lastk=kyr
          write(20,'(a15,a23)')kyr,rest
        else
          lastk=kyr
          write(21,'(a15,a23)')kyr,rest
        endif
      goto 2
    9 continue
      close(10)
      close(20)
      close(21)
c     close(22)
      stop
      end

