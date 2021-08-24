* R”dunaralgorithmar £r Press, W.H. et al (1989) kafli 8.*
* A. Straight insertion N < 50. {piksrt, piksr2, shell}  *
* B. Heapsort, Quicksort N > 1000.                       *

      subroutine piksrt(n,arr)
      dimension arr(n)
      real arr
      do 12 j=2,n
         a=arr(j)
         do 11 i=j-1,1,-1
            if(arr(i).le.a)go to 10
            arr(i+1)=arr(i)
   11    continue
         i=0
   10    arr(i+1)=a
   12 continue
      return
      end


      subroutine sort(n,ra)
      dimension ra(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
         else
            rra=ra(ir)
            ra(ir)=ra(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))j=j+1
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         go to 10
      end

      subroutine sort2(n,ra,rb)
*      dimension ra(n),rb(n)
      real ra(n)
      integer*4 rb(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         go to 10
      end


      subroutine sort2b(n,ra,rb)
*      dimension ra(n),rb(n)
      integer*4 ra(n),rb(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         go to 10
      end

      subroutine sort3(n,ra,rb,rc)
      real*8 rc(n),rrc
      integer*4 ra(n),rb(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
            rrc=rc(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            rrc=rc(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            rc(ir)=rc(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               rc(1)=rrc
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               rc(i)=rc(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         rc(i)=rrc
         go to 10
      end

      subroutine sort3b(n,ra,rb,rc)
      integer*4 ra(n),rb(n),rc(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
            rrc=rc(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            rrc=rc(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            rc(ir)=rc(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               rc(1)=rrc
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               rc(i)=rc(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         rc(i)=rrc
         go to 10
      end

      subroutine sort3c(n,ra,rb,rc)
      real*8 ra(n)
      integer*4 rb(n),rc(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
            rrc=rc(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            rrc=rc(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            rc(ir)=rc(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               rc(1)=rrc
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               rc(i)=rc(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         rc(i)=rrc
         go to 10
      end

      subroutine sort4c(n,ra,rb,rc,rd)
      real*8 ra(n)
      integer*4 rb(n),rc(n),rd(n)
      l=n/2+1
      ir=n
   10 continue
         if(l.gt.1)then
            l=l-1
            rra=ra(l)
            rrb=rb(l)
            rrc=rc(l)
            rrd=rd(l)
         else
            rra=ra(ir)
            rrb=rb(ir)
            rrc=rc(ir)
            rrd=rd(ir)
            ra(ir)=ra(1)
            rb(ir)=rb(1)
            rc(ir)=rc(1)
            rd(ir)=rd(1)
            ir=ir-1
            if(ir.eq.1)then
               ra(1)=rra
               rb(1)=rrb
               rc(1)=rrc
               rd(1)=rrd
               return
            endif
         endif
         i=l
         j=l+l
   20    if(j.le.ir)then
            if(j.lt.ir)then
               if(ra(j).lt.ra(j+1))then
                 j=j+1
               endif
            endif
            if(rra.lt.ra(j))then
               ra(i)=ra(j)
               rb(i)=rb(j)
               rc(i)=rc(j)
               rd(i)=rd(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
            go to 20
         endif
         ra(i)=rra
         rb(i)=rrb
         rc(i)=rrc
         rd(i)=rrd
         go to 10
      end
