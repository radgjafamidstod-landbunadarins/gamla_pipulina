c  -------------------------------------------------------------------
c   subroutine for setting a vector of length n to zero
      subroutine nolli(nvec,n)
      integer nvec(n)
      do 1 i=1,n
    1 nvec(i)=0
      return
      end
