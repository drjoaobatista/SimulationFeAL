program name
    implicit none
    real, dimension(:), allocatable :: a, b
    allocate(a(2))
    a(1)=5
    b=a
    a(1)=2
    WRITE(*,*) a(1), b(1) 
end program name