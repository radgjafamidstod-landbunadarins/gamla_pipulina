c  -------------------------------------------------------------------
c   subroutine for setting an integer matrix of order n*m to zero
      subroutine nollmi(a,n,m)
      integer a(n,m)
      do 1 i=1,n
      do 1 j=1,m
    1 a(i,j)=0
      return
      end
