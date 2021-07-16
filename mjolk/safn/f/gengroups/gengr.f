c-----------------------------------------------------------------------
       program grnum 
c...Allt fyrir 1970 sett a sama "phantom" hop
c     nov, 1992; oct 1996
c     Agust Sigurdsson, Department of Animal Breeding and genetics.
c     Swedish University of Agr. Sci., Uppsala.
c-----------------------------------------------------------------------
      parameter( nogrp = 36)
      integer iar
      integer telgrp(nogrp),grs(9999),grd(9999)
      integer path(4)
      do i=1,nogrp
        telgrp(i)=0
      enddo
c...Sire of bull
      path(1)=8
c...Sire of cow
      path(2)=6
c...Dam of bull
      path(3)=8
c...Dam of cow
      path(4)=5
c..."PHANTOM"-FEDUR
!     do i=1,2014
!     do i=1,2015
!     do i=1,2016
!     do i=1,2017
!     do i=1,2018
      do i=1,2020
        grs(i)=1
      enddo
      grs(1971)=2
      grs(1972)=2
      grs(1973)=3
      grs(1974)=3
      grs(1975)=4
      grs(1976)=4
      grs(1977)=5
      grs(1978)=5
      grs(1979)=6
      grs(1980)=6
      grs(1981)=7
      grs(1982)=7
      grs(1983)=8
      grs(1984)=8
      grs(1985)=9
      grs(1986)=9
      grs(1987)=10
      grs(1988)=10
      grs(1989)=11
      grs(1990)=11
      grs(1991)=12
      grs(1992)=12
      grs(1993)=13
      grs(1994)=13
      grs(1995)=14
      grs(1996)=14
      grs(1997)=15
      grs(1998)=15
      grs(1999)=16
      grs(2000)=16
      grs(2001)=17
      grs(2002)=17
c     do i=2003,2014
c     do i=2003,2015
c     do i=2003,2016
!     do i=2003,2017
c     do i=2003,2018
      do i=2003,2020
        grs(i)=18
      enddo
c..."PHANTOM"-MAEDUR
c     do i=1,2014
!     do i=1,2015
!     do i=1,2016
!     do i=1,2017
!     do i=1,2018
      do i=1,2020
        grd(i)=19
      enddo
      grd(1971)=20
      grd(1972)=20
      grd(1973)=21
      grd(1974)=21
      grd(1975)=22
      grd(1976)=22
      grd(1977)=23
      grd(1978)=23
      grd(1979)=24
      grd(1980)=24
      grd(1981)=25
      grd(1982)=25
      grd(1983)=26
      grd(1984)=26
      grd(1985)=27
      grd(1986)=27
      grd(1987)=28
      grd(1988)=28
      grd(1989)=29
      grd(1990)=29
      grd(1991)=30
      grd(1992)=30
      grd(1993)=31
      grd(1994)=31
      grd(1995)=32
      grd(1996)=32
      grd(1997)=33
      grd(1998)=33
      grd(1999)=34
      grd(2000)=34
      grd(2001)=35
      grd(2002)=35
c     do i=2003,2014
c     do i=2003,2015
c     do i=2003,2016
!     do i=2003,2017
!     do i=2003,2018
      do i=2003,2020
        grd(i)=36
      enddo
      open(10,file=
c    +'/home/agust/skuggi_kyr/mjolk/vinnsla/birthy.txt')
!    +'/home/LBHI/elsa/Kyr/mjolk/vinnsla/birthy.txt')
     +'/home/elsa/Kyr/mjolk/vinnsla/birthy.txt')
      open(11,file=
c    +'/home/agust/skuggi_kyr/mjolk/vinnsla/sex.txt')
!    +'/home/LBHI/elsa/Kyr/mjolk/vinnsla/sex.txt')
     +'/home/elsa/Kyr/mjolk/vinnsla/sex.txt')
      open(12,file=
c    +'/home/agust/skuggi_kyr/mjolk/vinnsla/ped.txt')
!    +'/home/LBHI/elsa/Kyr/mjolk/vinnsla/ped.txt')
     +'/home/elsa/Kyr/mjolk/vinnsla/ped.txt')
      open(20,file=
c    +'/home/agust/skuggi_kyr/mjolk/vinnsla/pedgrp.txt')
!    +'/home/LBHI/elsa/Kyr/mjolk/vinnsla/pedgrp.txt')
     +'/home/elsa/Kyr/mjolk/vinnsla/pedgrp.txt')
      open(21,file=
c    +'/home/agust/skuggi_kyr/mjolk/vinnsla/grnum.log')
!    +'/home/LBHI/elsa/Kyr/mjolk/vinnsla/grnum.log')
     +'/home/elsa/Kyr/mjolk/vinnsla/grnum.log')
c     open(10,file='/home/naut/framl/birthy.txt')
c     open(11,file='/home/naut/framl/sex.txt')
c     open(12,file='/home/naut/framl/impl/data/ped.old')
c     open(20,file='/home/naut/framl/impl/data/pedgrp.txt')
c     open(21,file='/home/naut/framl/grnum.log')
      do 210 i=1,nogrp
  210   write(20,112)i,0,0
  199 read(12,112,end=999) idd,is,id
      read(11,111) isex
      read(10,110) iar
      if(iar.eq.0)iar=1900
      if(is.eq.0)then
        if(isex.eq.1)is=grs(iar-path(1))
        if(isex.eq.2)is=grs(iar-path(2))
        telgrp(is)=telgrp(is)+1
      else
        is=is+nogrp
      endif
      if(id.eq.0)then
c agiskun fæedar fodur eda modur
        if(isex.eq.1)id=grd(iar-path(3))
        if(isex.eq.2)id=grd(iar-path(4))
        telgrp(id)=telgrp(id)+1
      else
        id=id+nogrp
      endif
      write(20,112)idd+nogrp,is,id
      write(*,*)idd+nogrp,is,id
      goto 199
  999 continue
      do 211 i=1,nogrp
  211   write(21,121)i,telgrp(i)
  110 format(i4)
  111 format(i1)
  112 format(3i7)
  121 format(2i7)
      stop
      end
