c... Pick up birth year of cow
c... Ped file read backwards for every rec. to pick up the youngest cow with 
c... each number i.e. the byear is missing (terrible!!)
c....Ath baett vid oldinni feb 2001
      parameter(norec = 62419)
      parameter(noped =330669)

      integer rp(37),n2(9,noped),domsar
      real*8  dam,sire
      integer pl,null43(43)
      character las*1
      logical binsok,nysok

      open(10,file='/home/LBHI/agust/BASSI/mjaltir/safn/mjal0409a')
      open(11,file='/home/LBHI/agust/BASSI/mjolk/safn/pgree.a09')
      open(20,
     +file='/home/LBHI/agust/BASSI/mjaltir/vinnsla/mjaltir.gogn2')

  211 format(i1,i6,i4,i4,i7,i6,i4,i4,i2) 
  210 format(i6,i4,8i1,i5,i4)
  220 format(i4,i6,i4,i7,i4,i6,i4,i3,2i1)
c...Format for datafile: 
c	bu	i6
c	num	i4
c       mjaltir i1
c       lekar   i1
c       seinar  i1
c       selja illa   i1
c       mismjalta     i1
c       skapgallar    i1
c       hefur fengid jugurbolgu   i1
c       gaedarod   i1
c	fadir	i5
c	ar konnunar i4
c... read files
      im0=0
      ig0=0
      inows=0
      print *,'Read unit - 11'
      do 198 i=1,noped
	if(mod(i,1000).eq.0)Print *,'Read: ',i
	read(11,211)(n2(j,i),j=1,9)
  198 continue
      do i=1,norec
        if(i.gt.48000)write(*,*)'Rec lesin:',i
        read(10,210)(rp(k),k=2,13)
	rp(13)=rp(13)-1900
	if(rp(12).gt.0.and.rp(12).ne.99999)then
c....2000 vandi! gildir til 2010
	  if(rp(12).lt.10000)then
	    rp(12)=2000000+rp(12)
	  else
	    rp(12)=1900000+rp(12)
	  endif
	else
	  rp(12)=0
	endif
        if(mod(i,1).eq.0)write(*,*)i
c	if(i.lt.41200)goto 3
c...search in pgree
        domsar=rp(13)
        rp(1)=-9
        do j=noped,1,-1
          if(rp(2).eq.n2(2,j).and.n2(1,j).eq.2.and.n2(4,j).eq.rp(3))then
c    +rp(12).and.(domsar-n2(3,j).lt.7))then
c...puts in the birth-year
            rp(1)=n2(3,j)
c...puts in the correct sire
            if(rp(12).ne.n2(5,j))then
              rp(12)=n2(5,j)
              inows=inows+1
            endif
c...puts in the dam-herd,dam-birthyear and dam-number
            rp(14)=n2(7,j)
            rp(15)=n2(6,j)
            rp(16)=n2(8,j)
c           if(n2(9,j).eq.0)im0=im0+1
c           if(domsar-n2(3,j).gt.6)ig0=ig0+1
c...setja syningarmanud a mai ef hann vantar
c           if(rp(4).eq.0)rp(4)=5
c           rp(5)=domsar
            goto 2
          endif
        enddo
        print *,'Finnst ekki i aettskra',(rp(kk),kk=1,7),n2(5,j),j
        goto 3
    2   write(20,220)(rp(k),k=1,3),rp(12),(rp(l),l=14,16),rp(13),
     +rp(4),rp(11)
    3  enddo 
c     print *,'Alls kyr med manud=0',im0
c     print *,'Alls kyr eldri en 6 ara vid dom ',ig0
c....Rangt skradir her thydir i rauninni ad fodur er skipt ut
c....vegna thess ad nu er numerid xxxxxxx i stad xxxxx
      print *,'Alls fedur rangt skradir ',inows
      stop
      end
