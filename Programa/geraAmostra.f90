!f2py3 -c geraAmostra.f90 -m geraAmostra

subroutine BCCAmostra(energias, Amostra, clusters ,L, A, q)
    implicit none
    save  
    !saidas
    integer, intent(out),dimension(0:2*L*L*L-1):: Amostra
    !f2py3 intent(out) :: Amostra   
    integer, intent(out),dimension(0:2*L*L*L-1):: clusters
    !f2py3 intent(out) :: nmax
    !f2py3 intent(out) :: Amostra   
    real, intent(out),dimension(0:2*L*L*L-1):: energias
    !entradas 
    integer, intent(in):: L! para que tamanho de agloreado exixte j2
    !f2py3 intent(in) ::2*L*L*L,L, cluster
    real(8), intent(in)::q, A   !p=0 puro
    !f2py3 intent(in) :: q, B

    integer, dimension(0:7,0:2*L*L*L-1) :: vizinhos
    integer, dimension(0:2*L*L*L-1):: marcacao
    real , dimension(0:7,0:2*L*L*L-1):: Ligacao,Ligacao2

    real :: rando, T
    integer :: numeroCoordenacao, i
    clusters=0
    numeroCoordenacao=8
    marcacao=0
    T=25
    call init_random_seed()  
    call marcarVizinhos()
    call diluir()
    call marcaLigacao()
    if ( q>0 ) call minimizarEnergia()    
    call ContaAgrupamentos()
    

 CONTAINS
!----------------------------------------------------------------------------- 
subroutine marcarVizinhos !Amostra bcc ok
    !variaveis Locais
        integer,dimension(0:L-1) :: ant, suc
        integer :: i, j, K, site, L2, L3
        L2=L*L
        L3=L*L*L    
        forall (i = 0:L-1)
            ant(i) = i -1
            suc(i) = i + 1
        end forall
        suc(L-1) =0 
        ant(0) = L-1
    
    !primeiros vizinhos
        DO k = 0 , L-1
            DO j = 0 , L-1
                DO i = 0 , L-1
                !subrede Amarela
                    site =             i      + j*L      +  k*L2     !sitio amarelo      
                    vizinhos(0,site) = i      + j*L      +  k*L2    +   L3 !azul ok
                    vizinhos(1,site) = i      + ant(j)*L +  k*L2    +   L3 !roxo ok
                    vizinhos(2,site) = ant(i) + ant(j)*L +  k*L2    +   L3 !vermelho ok
                    vizinhos(3,site) = ant(i) + j*L      +  k*L2    +   L3 !Verde ok
    
                    vizinhos(4,site) = ant(i) + ant(j)*L +  ant(k)*L2  +  L3!Azul ok 
                    vizinhos(5,site) = ant(i) + j*L      +  ant(k)*L2  +  L3!roxo ok
                    vizinhos(6,site) = i      + j*L      +  ant(k)*L2  +  L3!Vermelho  ok
                    vizinhos(7,site) = i      + ant(j)*L +  ant(k)*L2  +  L3!verde ok
                
    
    
                !subrede Vermelha 
                    site             = i      + j*L      +  k*L2     +   L3  !sitio Vermelho
                    vizinhos(0,site) = suc(i) + suc(j)*L +  suc(k)*L2   !azul ok
                    vizinhos(1,site) = suc(i) + j*L      +  suc(k)*L2   !roxo ok
                    vizinhos(2,site) = i      + j*L      +  suc(k)*L2   !vermelho  ok                                                                             
                    vizinhos(3,site) = i      + suc(j)*L +  suc(k)*L2   !verde ok
                    
                    vizinhos(4,site) = i      + j*L      +  k*L2        !Azul  ok
                    vizinhos(5,site) = i      + suc(j)*L +  k*L2        !roxo   ok
                    vizinhos(6,site) = suc(i) + suc(j)*L +  k*L2        !vermelho ok                      
                    vizinhos(7,site) = suc(i) + j*L      +  k*L2        !verde  ok
    
                END DO
            END DO
        END DO
         
    end subroutine marcarVizinhos
  
!----------------------------------------------------------------------------- 
SUBROUTINE init_random_seed()
    INTEGER :: i, n, clock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))
    CALL SYSTEM_CLOCK(COUNT=clock)

    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)

    DEALLOCATE(seed)
END SUBROUTINE  

Subroutine diluir
    !Variaveis Locais
    integer :: numeroVacancias
    integer :: i
    NumeroVacancias = int(2*L*L*L*q)
    Amostra(0:NumeroVacancias-1) = 0
    Amostra(NumeroVacancias:2*L*L*L-1) = 1
    do i=1,100 
        call Shuffle(Amostra)
    end do   
end subroutine diluir

subroutine Shuffle(vetor)  
    !Variaveis mudas 
    integer, dimension(:), intent(inout) :: vetor    
    !Variaveis Locais
    integer :: i, posicao,temp
    do i = size(vetor), 2, -1
        call random_number(rando)
        posicao = int(rando * i) + 1
        temp = vetor(posicao)
        vetor(posicao) = vetor(i)
        vetor(i) = temp
    end do
end subroutine Shuffle
   
!-----------------------------------------------------------------------------

function Energia()
    real:: Energia
    integer :: i
    energia=0
    do i = 0, 2*L*L*L -1
      Energia = Energia - Amostra(i)*( &
        & Amostra(vizinhos(0,i))*ligacao(0,i) + &
        & Amostra(vizinhos(1,i))*ligacao(1,i) + &
        & Amostra(vizinhos(2,i))*ligacao(2,i) + &
        & Amostra(vizinhos(3,i))*ligacao(3,i) + &
        & Amostra(vizinhos(4,i))*ligacao(4,i) + &
        & Amostra(vizinhos(5,i))*ligacao(5,i) + &
        & Amostra(vizinhos(6,i))*ligacao(6,i) + &
        & Amostra(vizinhos(7,i))*ligacao(7,i)    )
    end do 
end function Energia

!-----------------------------------------------------------------------------
subroutine minimizarEnergia()
    implicit none
    INTEGER i, j, cont
    real::E0, Ef
    cont=0
    E0=Energia()
    do while (cont< 2*L*L*L)
        call random_number(rando)
        i = int(rando * 2*L*L*L)
        if(Amostra(i)==0)then
            call random_number(rando)
            j = int(rando * numeroCoordenacao)
            if (Amostra(vizinhos(j,i))==1) then
                Ligacao2=Ligacao
                E0=Energia()
                Amostra(vizinhos(j,i))=0
                Amostra(i)=1
                call marcaLigacao
                EF=Energia()
                call random_number(rando)
                if (rando< exp((EF-E0)/T)) then
                   !WRITE(*,*) cont
                   cont=cont+1
                   energias(cont)=EF
                else
                    Amostra(vizinhos(j,i))=1
                    Amostra(i)=0
                    Ligacao=Ligacao2
                end if          
            end if
        end if
    end do
end subroutine minimizarEnergia

!---------------------------------------------
subroutine marcaLigacao
    integer:: i
    ligacao=1
    do i=0, 2*L*L*L-1
        if (Amostra(i)==0) then
            ligacao(0,vizinhos(0,i))=real(A)
            ligacao(1,vizinhos(1,i))=real(A)
            ligacao(2,vizinhos(2,i))=real(A)
            ligacao(3,vizinhos(3,i))=real(A)
   
            ligacao(0,vizinhos(4 , vizinhos(4,i) ))=real(A)
            ligacao(1,vizinhos(5 , vizinhos(5,i) ))=real(A)
            ligacao(2,vizinhos(6 , vizinhos(6,i) ))=real(A)
            ligacao(3,vizinhos(7 , vizinhos(7,i) ))=real(A)                       
       end if       
     end do
   
     ! apaga exesso de ligações 
    do i=0,2*L*L*L-1
        if(Amostra(i)==0 )then
            ligacao(0,i)=0
            ligacao(1,i)=0
            ligacao(2,i)=0
            ligacao(3,i)=0
            ligacao(0,vizinhos(4, i))=0
            ligacao(1,vizinhos(5, i))=0
            ligacao(2,vizinhos(6, i))=0
            ligacao(3,vizinhos(7, i))=0               
        end if
    end do
    !completar simetria 
    do i=0, 2*L*L*L-1
        ligacao(4,i) = ligacao(0,vizinhos(4,i))
        ligacao(5,i) = ligacao(1,vizinhos(5,i))
        ligacao(6,i) = ligacao(2,vizinhos(6,i)) 
        ligacao(7,i) = ligacao(3,vizinhos(7,i))         
    end do
     
 end subroutine marcaLigacao
!----------------------------------------------------------------------------- s

 function  agrupamento(j)
    integer :: agrupamento 
    integer, intent(in) :: j
    integer i, v, topoDaPilha, num
    integer, dimension(0:2*L*L*L-1) :: pilhaInt
    
    num=1
    topoDaPilha=1
    pilhaInt(topoDaPilha)=j
    do while (topoDaPilha>0)
         i=pilhaInt(topoDaPilha)
         topoDaPilha=topoDaPilha-1
         do v=0 ,  7
            if ((Amostra(vizinhos(v,i))==0).AND.(marcacao(vizinhos(v,i))==0)) then
                marcacao(vizinhos(v,i))=-1
                num=num+1
                topoDaPilha=topoDaPilha+1
                pilhaInt(topoDaPilha)=vizinhos(v,i)
            end if
        end do
    end do
    agrupamento = num 

end  function  agrupamento

!----------------------------------------------------------------------------- 

Subroutine  ContaAgrupamentos
    integer i, j
    j=0        
    do i = 0, 2*L*L*L-1
        if ((Amostra(i)==0).AND.(marcacao(i)==0)) then
            marcacao(i)=-1
            clusters(j)=agrupamento(i)
            j=j+1
        end if
    end do
end  Subroutine  ContaAgrupamentos

!-----------------------------------------------------------------------------
end subroutine BCCAmostra