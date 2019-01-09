!compilar para pyton :: f2py -c subroutin_ising.f90 -m ising

subroutine IsingBCC(hist,sigma, L, MCx, MCc,J1,J2,t0,p,aglomeradolimite)
    implicit none
    !saidas
    integer, intent(out),dimension(0:Mcc-1,0:2):: hist
!f2py intent(out) :: hist
    !entradas
    integer, intent(in)::L, MCx, MCc, aglomeradolimite ! para que tamanho de agloreado exixte j2
    !f2py intent(in) ::L, MCx, MCc,aglomeradolimite
    integer, intent(inout),dimension(1:2,1:L,1:L,1:L):: sigma
    !f2py , intent(inout):: sigma
    real(8), intent(in)::J2,J1,t0, p   !p=0 sistema puro
!f2py intent(in) :: J2,J1,t0, p

    !variaveis locais
    integer, dimension(1:2,1:L,1:L,1:L):: marcacao
    integer, dimension(1:L)::ant, suc
    integer, dimension(1:2,1:L,1:L,1:L)::bond_i,bond_j,bond_k
    integer ::magnetizacao, eneJ1, eneJ2, spin
    double precision energia
    integer ::passo
    double precision ::w(-8:8,-6:6)
    double precision ::PJ1,PJ2
    real::rando
    intrinsic random_seed, random_number


!------------------------------------------------
    bond_i=0
    bond_j=0
    bond_k=0

    call initRandomSeed()
    call iniciaContorno
  !  call dilui
    call marcaAgrupamento
    call geraLigacao
    call atualiza
    do passo = 1 , MCx
        call metropolis
        call wolff
    end do
    do passo = 1 , MCc
        call metropolis
        call wolff
        call calcularMagEng
        hist(passo,0)=eneJ1
        hist(passo,1)=eneJ2
        hist(passo,2)=magnetizacao
    end do

CONTAINS
    !-----------------------------------------------------------------------------

    subroutine iniciaContorno !ok
       integer :: i
        do i = 1, L
            ant(i) = i - 1
            suc(i) = i + 1
        end do
        ant(1) = L
        suc(L) = 1
    end subroutine iniciaContorno
   !-----------------------------------------------------------------------------

    subroutine  dilui !ok
       integer :: n
       integer    ::  i,j,k,subrede
        n=0
        do while(n<int(2*p*L*L*L))
            call random_number(rando)
            i=int(L*rando)+1
            call random_number(rando)
            j=int(L*rando)+1
            call random_number(rando)
            k=int(L*rando)+1
            call random_number(rando)
            subrede=int(2*rando)+1
            if (abs(sigma(subrede, i,j,k))==1) then
                sigma(subrede, i,j,k)=0
                n=n+1
            end if
        end do
    end subroutine dilui
    !-----------------------------------------------------------------------------

    subroutine wolff
       integer :: i,j,k, subrede
        !sorteia sitio semente
        do
            call random_number(rando)
            i=int(L*rando)+1
            call random_number(rando)
            j=int(L*rando)+1
            call random_number(rando)
            k=int(L*rando)+1
            call random_number(rando)
            subrede=int(2*rando)+1
            if (abs(sigma(subrede, i,j,k))==1) exit
        end do
        spin= sigma(subrede, i,j,k)
        sigma(subrede, i,j,k)=-sigma(subrede, i,j,k)
        call perimetro(subrede, i,j,k)
    end subroutine wolff
       !-----------------------------------------------------------------------------

    recursive subroutine perimetro(subrede, i,j,k)
       integer, intent(in) :: i,j,k, subrede
       integer :: sub
       !Primeiros Vizinhos
        if(subrede==1) then
            sub=2
            if (sigma(sub,i,j,k)==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,i,j,k)=-spin
                    call perimetro(sub,i,j,k)
                end if
            end if
            !
            if (sigma(sub,suc(i),j,k)==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,suc(i),j,k)=-spin
                    call perimetro(sub,suc(i),j,k)
                end if
            end if
            !
            if (sigma(sub,suc(i),suc(j),k)==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,suc(i),suc(j),k)=-spin
                    call perimetro(sub,suc(i),suc(j),k)
                end if
            end if
            !
            if (sigma(sub,i,suc(j),k)==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,i,suc(j),k)=-spin
                    call perimetro(sub,i,suc(j),k)
                end if
            end if
            !
            if (sigma(sub,i,j,suc(k))==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,i,j,suc(k))=-spin
                    call perimetro(sub,i,j,suc(k))
                end if
            end if
            !
            if (sigma(sub,suc(i),j,suc(k))==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,suc(i),j,suc(k))=-spin
                    call perimetro(sub,suc(i),j,suc(k))
                end if
            end if
            !
            if (sigma(sub,suc(i),suc(j),suc(k))==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,suc(i),suc(j),suc(k))=-spin
                    call perimetro(sub,suc(i),suc(j),suc(k))
                end if
            end if
            !
            if (sigma(sub,i,suc(j),suc(k))==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,i,suc(j),suc(k))=-spin
                    call perimetro(sub,i,suc(j),suc(k))
                end if
            end if
        else
            !--------------------------------------------------------------------------
            sub=1
            if (sigma(sub,ant(i),ant(j),k)==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,ant(i),ant(j),k)=-spin
                    call perimetro(sub,ant(i),ant(j),k)
                end if
            end if
            !
            if (sigma(sub,i,ant(j),k)==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,i,ant(j),k)=-spin
                    call perimetro(sub,i,ant(j),k)
                end if
            end if
            !
            if (sigma(sub,i,j,k)==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,i,j,k)=-spin
                    call perimetro(sub,i,j,k)
                end if
            end if
            !
            if (sigma(sub,ant(i),j,k)==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,ant(i),j,k)=-spin
                    call perimetro(sub,ant(i),j,k)
                end if
            end if
            !
            if (sigma(sub,ant(i),ant(j),ant(k))==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,ant(i),ant(j),ant(k))=-spin
                    call perimetro(sub,ant(i),ant(j),ant(k))
                end if
            end if
            !
            if (sigma(sub,i,ant(j),ant(k))==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,i,ant(j),ant(k))=-spin
                    call perimetro(sub,i,ant(j),ant(k))
                end if
            end if
            !
            if (sigma(sub,i,j,ant(k))==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,i,j,ant(k))=-spin
                    call perimetro(sub,i,j,ant(k))
                end if
            end if
            !
            if (sigma(sub,ant(i),j,ant(k)) ==spin)then
                call random_number(rando)
                if (rando<PJ1) then
                    sigma(sub,ant(i),j,ant(k))=-spin
                    call perimetro(sub,ant(i),j,ant(k))
                end if
            end if
        end if
  !      Segundos Vizinhos--------------------------------------------------------------------------
        sub=subrede;
        if (sigma(sub,ant(i),j,k)*Bond_i(sub,ant(i),j,k)==spin)then
            call random_number(rando)
            if (rando<PJ2) then
                sigma(sub,ant(i),j,k)=-spin
                call perimetro(sub,ant(i),j,k)
            end if
        end if
        !
        if (sigma(sub,suc(i),j,k)*Bond_i(sub,i,j,k)==spin)then
            call random_number(rando)
            if (rando<PJ2) then
                sigma(sub,suc(i),j,k)=-spin
                call perimetro(sub,suc(i),j,k)
            end if
        end if
        !
        if (sigma(sub,i,ant(j),k)*Bond_j(sub,i,ant(j),k)==spin)then
            call random_number(rando)
            if (rando<PJ2) then
                sigma(sub,i,ant(j),k)=-spin
                call perimetro(sub,i,ant(j),k)
            end if
        end if
        !
        if (sigma(sub,i,suc(j),k)*Bond_j(sub,i,j,k)==spin)then
            call random_number(rando)
            if (rando<PJ2) then
                sigma(sub,i,suc(j),k)=-spin
                call perimetro(sub,i,suc(j),k)
            end if
        end if
        !
        if (sigma(sub,i,j,ant(k))*Bond_k(sub,i,j,ant(k))==spin)then
            call random_number(rando)
            if (rando<PJ2) then
                sigma(sub,i,j,ant(k))=-spin
                call perimetro(sub,i,j,ant(k))
            end if
        end if
        if (sigma(sub,i,j,suc(k))*Bond_k(sub,i,j,k)==spin)then
            call random_number(rando)
            if (rando<PJ2) then
                sigma(sub,i,j,suc(k))=-spin
                call perimetro(sub,i,j,suc(k))
            end if
        end if
        return
    end subroutine perimetro

    !-----------------------------------------------------------------------------

    subroutine geraLigacao
       integer::  i,j,k
!subrede 1
        do i=1,L
            do j=1,L
                do k=1,L
                    if( isolada(1,i,j,k) )then
                        bond_i(2,i,j,k)=1
                        bond_i(2,i,suc(j),k)=1
                        bond_i(2,i,j,suc(k))=1
                        bond_i(2,i,suc(j),suc(k))=1

                        bond_j(2,i,j,k)=1
                        bond_j(2,suc(i),j,k)=1
                        bond_j(2,i,j,suc(k))=1
                        bond_j(2,suc(i),j,suc(k))=1

                        bond_k(2,i,j,k)= 1
                        bond_k(2,suc(i),j,k)= 1
                        bond_k(2,i,suc(j),k)= 1
                        bond_k(2,suc(i),suc(j),k)= 1
                    end if
                end do
            end do
        end do
!subrede 2
        do i=1,L
            do j=1,L
                do k=1,L
                    if( isolada(2,i,j,k) )then

                        Bond_i(1,ant(i),ant(j),ant(k))=1
                        Bond_i(1,ant(i),ant(j), k)=1
                        Bond_i(1,ant(i), j, k)=1
                        Bond_i(1,ant(i),j, ant(k))=1

                        Bond_j(1,ant(i), ant(j),ant(k))=1
                        Bond_j(1,ant(i), ant(j), k)=1
                        Bond_j(1, i,  ant(j), k)=1
                        Bond_j(1, i,  ant(j),ant(k))=1

                        Bond_k(1,ant(i),ant(j),ant(k))=1
                        Bond_k(1,i,ant(j),ant(k))=1
                        Bond_k(1,i,j,ant(k))=1
                        Bond_k(1,ant(i),j,ant(k))=1
                    end if
                end do
            end do
        end do
    end subroutine geraLigacao
!-----------------------------------------------------------------------------
    function isolada(subrede,i,j,k)
        logical :: isolada
        integer::  i,j,k
        integer ::subrede
            isolada=(marcacao(subrede,i,j,k)<=aglomeradolimite).AND.(sigma(subrede,i,j,k)==0)
    end function  isolada
  !  -----------------------------------------------------------------------------

    subroutine atualiza
        !Variaveis Locais
        double precision:: aux
        integer::  i,j
        do i=-8,8
            do j=-6,6
                aux=(i*J1+j*J2)/t0
                w(i,j)=1
                if (aux >0) w(i,j)=exp(-2*aux)
            end do
        end do
        PJ1= 1- exp(-2*J1/t0)
        PJ2= 1- exp(-2*J2/t0)
    end subroutine atualiza
    !-----------------------------------------------------------------

    subroutine CalcularMagEng
        integer :: i,j,k,NJ1,NJ2
        integer :: soma_nj1
        integer :: soma_nj2
        integer :: somaSigma

        soma_nj1=0
        soma_nj2=0
        somaSigma=0
        do i=1,L
            do j=1,L
                do k=1,L
                    NJ1= sigma(1,i,j,k)*(&
                        sigma(2,i,j,k)+ &
                        sigma(2,suc(i),j,k)+ &
                        sigma(2,suc(i),suc(j),k)+ &
                        sigma(2,i,suc(j),k)+ &
                        sigma(2,i,j,suc(k))+ &
                        sigma(2,suc(i),j,suc(k))+ &
                        sigma(2,suc(i),suc(j),suc(k))+ &
                        sigma(2,i,suc(j),suc(k))   )

                    NJ2= sigma(1,i,j,k)*(&
                        sigma(1,suc(i),j,k)*Bond_i(1,i,j,k)+ &
                        sigma(1,i,suc(j),k)*Bond_j(1,i,j,k)+ &
                        sigma(1,i,j,suc(k))*Bond_k(1,i,j,k) ) +&
                        sigma(2,i,j,k)*( &
                        sigma(2,suc(i),j,k)*Bond_i(2,i,j,k)+ &
                        sigma(2,i,suc(j),k)*Bond_j(2,i,j,k)+ &
                        sigma(2,i,j,suc(k))*Bond_k(2,i,j,k))

                    soma_nj1=NJ1+ soma_nj1
                    soma_nj2=NJ2+ soma_nj2
                end do
            end do
        end do
        energia = -(J1*soma_nj1+ J2*soma_nj2)
        eneJ1=soma_nj1
        eneJ2=soma_nj2
        magnetizacao = abs(sum(sigma(:,:,:,:)))
    end subroutine CalcularMagEng
    !-----------------------------------------------------------------
    subroutine metropolis
        integer i, j, k, NJ1, NJ2
        ! varre subrede=1
        do i=1,L
            do j=1,L
                do k=1,L
                    NJ1= sigma(1,i,j,k)*(&
                        sigma(2,i,j,k)+ &
                        sigma(2,suc(i),j,k)+ &
                        sigma(2,suc(i),suc(j),k)+ &
                        sigma(2,i,suc(j),k)+ &
                        sigma(2,i,j,suc(k))+ &
                        sigma(2,suc(i),j,suc(k))+ &
                        sigma(2,suc(i),suc(j),suc(k))+ &
                        sigma(2,i,suc(j),suc(k))   )

                    NJ2= sigma(1,i,j,k)*(&
                         sigma(1,ant(i),j,k)*Bond_i(1,ant(i),j,k)+ &
                         sigma(1,suc(i),j,k)*Bond_i(1,i,j,k)+ &
                         sigma(1,i,ant(j),k)*Bond_j(1,i,ant(j),k)+ &
                         sigma(1,i,suc(j),k)*Bond_j(1,i,j,k)+ &
                         sigma(1,i,j,ant(k))*Bond_k(1,i,j,ant(k))+ &
                         sigma(1,i,j,suc(k))*Bond_k(1,i,j,k) )
                    call random_number(rando)
                    if (rando<W(NJ1,NJ2)) then
                        sigma(1,i,j,k)=-sigma(1,i,j,k)
                    end if
                end do
            end do
        end do

        ! varre subrede=2
        do i=1,L
            do j=1,L
                do k=1,L
                    NJ1= sigma(2,i,j,k)*( &
                        sigma(1,ant(i),ant(j),k)+ &
                        sigma(1,i,ant(j),k)+ &
                        sigma(1,i,j,k)+ &
                        sigma(1,ant(i),j,k)+ &
                        sigma(1,ant(i),ant(j),ant(k))+ &
                        sigma(1,i,ant(j),ant(k))+ &
                        sigma(1,i,j,ant(k))+ &
                        sigma(1,ant(i),j,ant(k)) )

                    NJ2= sigma(2,i,j,k)*(  &
                        sigma(2,ant(i),j,k)*Bond_i(2,ant(i),j,k)+ &
                        sigma(2,suc(i),j,k)*Bond_i(2,i,j,k)+ &
                        sigma(2,i,ant(j),k)*Bond_j(2,i,ant(j),k)+ &
                        sigma(2,i,suc(j),k)*Bond_j(2,i,j,k)+ &
                        sigma(2,i,j,ant(k))*Bond_k(2,i,j,ant(k))+ &
                        sigma(2,i,j,suc(k))*Bond_k(2,i,j,k))
                    call random_number(rando)
                    if(rando<W(NJ1,NJ2)) sigma(2,i,j,k)=-sigma(2,i,j,k)
                end do
            end do
        end do
    end subroutine metropolis
    !-----------------------------------------------------------------
    subroutine initRandomSeed()
        integer :: i, n, clock
        integer, dimension(:), allocatable :: seed

        call RANDOM_SEED(size = n)
        allocate(seed(n))
        call SYSTEM_CLOCK(COUNT=clock)
        seed = clock + 37 * (/ (i - 1, i = 1, n) /)
        call RANDOM_SEED(PUT = seed)
        deallocate(seed)
    end subroutine initRandomSeed

 !-----------------------------------------------------------------------------

    subroutine marcaAgrupamento
       integer::  i,j,k,n,sub
        marcacao=0
        do i=1,L
            do j=1,L
                do k=1,L
                    if((sigma(1,i,j,k)==0).And.(marcacao(1,i,j,k)==0)) then
                         n=1
                         sub=1
                         marcacao(1,i,j,k)=-1
                         call  agrupamento(sub,i,j,k,n)
                         where (marcacao==-1)
                         marcacao=n
                         end where

                    end if

                    if(( sigma(2,i,j,k)==0).And.(marcacao(2,i,j,k)==0))then
                         n=1
                         sub=2
                         marcacao(2,i,j,k)=-1
                         call  agrupamento(sub,i,j,k,n)
                         where (marcacao==-1)
                         marcacao=n
                         end where

                    end if
                end do
            end do
        end do
   end subroutine marcaAgrupamento


    !-----------------------------------------------------------------------------

 recursive subroutine agrupamento(subrede, i,j,k,n )
       integer, intent(in) :: i,j,k, subrede
       integer, intent(inout) :: n

       integer :: sub
       integer :: aux=1
       !Primeiros Vizinhos
        if(subrede==1) then
            sub=2
            if ((sigma(sub,i,j,k)==0).AND.(marcacao(sub,i,j,k)==0)) then
                  marcacao(sub,i,j,k)=-1
                  n=n+aux
                  call agrupamento(sub,i,j,k,n)
            end if
            !
            if ((sigma(sub,suc(i),j,k)==0).AND.(marcacao(sub,suc(i),j,k)==0)) then
                    marcacao(sub,suc(i),j,k)=-1
                    n=n+aux
                   call agrupamento(sub,suc(i),j,k,n)
            end if
            !
            if ((sigma(sub,suc(i),suc(j),k)==0) .AND. (marcacao(sub,suc(i),suc(j),k)==0)) then
                   marcacao(sub,suc(i),suc(j),k)=-1
                   n=n+aux
                   call agrupamento(sub,suc(i),suc(j),k,n)
            end if
            !
            if ((sigma(sub,i,suc(j),k)==0).AND.(marcacao(sub,i,suc(j),k)==0)) then
                     marcacao(sub,i,suc(j),k)=-1
                     n=n+aux
                     call agrupamento(sub,i,suc(j),k,n)
            end if
            !
            if ((sigma(sub,i,j,suc(k))==0).AND.(marcacao(sub,i,j,suc(k))==0)) then
                     marcacao(sub,i,j,suc(k))=-1
                     n=n+aux
                     call agrupamento(sub,i,j,suc(k),n)
            end if
            !
            if ((sigma(sub,suc(i),j,suc(k))==0).AND.(marcacao(sub,suc(i),j,suc(k))==0)) then
                    marcacao(sub,suc(i),j,suc(k))=-1
                    n=n+aux
                    call agrupamento(sub,suc(i),j,suc(k),n)
            end if
            !
            if ((sigma(sub,suc(i),suc(j),suc(k))==0).AND.(marcacao(sub,suc(i),suc(j),suc(k))==0)) then
                   marcacao(sub,suc(i),suc(j),suc(k))=-1
                   n=n+aux
                   call agrupamento(sub,suc(i),suc(j),suc(k),n)
            end if
            !
            if ((sigma(sub,i,suc(j),suc(k))==0).AND.(marcacao(sub,i,suc(j),suc(k))==0))then
                   marcacao(sub,i,suc(j),suc(k))=-1
                   n=n+aux
                   call agrupamento(sub,i,suc(j),suc(k),n)
            end if
        else
            !--------------------------------------------------------------------------
            sub=1
            if ((sigma(sub,ant(i),ant(j),k)==0).AND.(marcacao(sub,ant(i),ant(j),k)==0))  then
                    marcacao(sub,ant(i),ant(j),k)=-1
                    n=n+aux
                    call agrupamento(sub,ant(i),ant(j),k,n)
            end if
            !
            if ((sigma(sub,i,ant(j),k)==0).AND.(marcacao(sub,i,ant(j),k)==0))then
                    marcacao(sub,i,ant(j),k)=-1
                    n=n+aux
                    call agrupamento(sub,i,ant(j),k,n)
            end if
            !
            if ((sigma(sub,i,j,k)==0).AND.(marcacao(sub,i,j,k)==0))then
                    marcacao(sub,i,j,k)=-1
                    n=n+aux
                    call agrupamento(sub,i,j,k,n)
            end if
            !
            if ((sigma(sub,ant(i),j,k)==0).AND.(marcacao(sub,ant(i),j,k)==0)) then
                    marcacao(sub,ant(i),j,k)=-1
                    n=n+aux
                    call agrupamento(sub,ant(i),j,k,n)
            end if
            !
            if ((sigma(sub,ant(i),ant(j),ant(k))==0).AND.(marcacao(sub,ant(i),ant(j),ant(k))==0))  then
                    marcacao(sub,ant(i),ant(j),ant(k))=-1
                    n=n+aux
                    call agrupamento(sub,ant(i),ant(j),ant(k),n)
            end if
            !
            if ((sigma(sub,i,ant(j),ant(k))==0).AND.(marcacao(sub,i,ant(j),ant(k))==0)) then
                    marcacao(sub,i,ant(j),ant(k))=-1
                    n=n+aux
                    call agrupamento(sub,i,ant(j),ant(k),n)
            end if
            !
            if ((sigma(sub,i,j,ant(k))==0).AND.(marcacao(sub,i,j,ant(k))==0))  then
                    marcacao(sub,i,j,ant(k))=-1
                    n=n+aux
                    call agrupamento(sub,i,j,ant(k),n)
            end if
            !
            if ((sigma(sub,ant(i),j,ant(k)) ==0).AND.(marcacao(sub,ant(i),j,ant(k)) ==0))  then
                    marcacao(sub,ant(i),j,ant(k))=-1
                    n=n+aux
                    call agrupamento(sub,ant(i),j,ant(k),n)
            end if
        end if
       return
    end subroutine agrupamento

end subroutine IsingBCC
