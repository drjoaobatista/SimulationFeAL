!  Copyright 2014 Joao Batista dos Santos Filho <joao@jbsantosfilho.com>

subroutine BCC(hist, MCx, MCc,L, A,t0,p, Clusterlimite)
    implicit none
    save  
    !saidas
    real(8), intent(out),dimension(0:Mcc-1,0:3):: hist
    !f2py intent(out) :: hist       
    !entradas 
    integer, intent(in)::L, MCx, MCc ! para que tamanho de agloreado exixte j2
    !f2py intent(in) ::L, MCx, MCc

    real(8), intent(in)::A,t0, p   !p=0 sistema puro
    !f2py intent(in) :: A,t0, p
    
    type :: Spin
        double precision, dimension(1:3) :: s
        integer :: s2
    end type Spin
    
    type :: Quantidades
        double precision,dimension(1:3) :: magnetizacao !magnetização instantanea
        double precision :: energia !Energia instantanea
    end type Quantidades

    type :: Configuracao
        integer :: l
        integer :: numeroCoordenacao
        integer :: numeroSitios
        integer :: numeroTermalizacao
        integer :: numeroPassosMC
    end type Configuracao
   
    type :: Disco  !usado para construir a pilha 
         double precision::prod
         integer::site
    end type Disco

!Variaveis Locais
    
    !declaracao de variaveis 
    intrinsic random_seed, random_number
    type(Spin), dimension(:), allocatable :: rede
    type(Disco), dimension(:), allocatable :: pilha
    type(Quantidades) :: quantidadesTermodinamicas
    type(Configuracao) :: sistema 
    integer, dimension(:,:), allocatable :: vizinhos
    real, dimension(:,:), allocatable :: ligacao
    real :: rando
    integer :: passo

    call lerDados()  
    !Abrindo arquivos 
    open(2,file = 'hist.dat')
   
    !inicilização das variaveis 
    allocate(rede(0:sistema%numeroSitios-1))
    allocate(pilha(1:sistema%numeroSitios)) !usa no wollf e precisa comecar do 1
    allocate(vizinhos(0:sistema%numeroCoordenacao-1, 0:sistema%numeroSitios-1))
    allocate(ligacao(0:sistema%numeroCoordenacao-1,0:sistema%numeroSitios-1))
   
    rede=Spin((/1,0,0/),1)
    call init_random_seed()   
   ! call random_seed(PUT=seed)
    call marcarVizinhos
    call diluir !gera amostra 
    call marcaLigacao     
    call inicializar 
    do passo = 0, sistema%numeroTermalizacao-1
        call metropolis
        call wolff
        call superrelaxacao    
    end do 
    !repetições para media   
    do passo = 0, sistema%numeroPassosMC-1
        call metropolis
        call wolff
        call superrelaxacao
        hist(passo,0)=quantidadesTermodinamicas%Energia/sistema%numeroSitios
        hist(passo,1)=quantidadesTermodinamicas%Magnetizacao(1)/sistema%numeroSitios
        hist(passo,2)=quantidadesTermodinamicas%Magnetizacao(2)/sistema%numeroSitios
        hist(passo,3)=quantidadesTermodinamicas%Magnetizacao(3)/sistema%numeroSitios
    
    end do 

    !fim do programa 
    CONTAINS
!-----------------------------------------------------------------------------

    subroutine metropolis
                
        !Variaveis Locais
        type(Spin) :: spinNovo
        type(Spin) :: campoEfetivo
        integer :: i, j
        double precision :: deltaE 
        double precision :: probabilidade
        
        do i=0, sistema%numeroSitios-1
            if (rede(i)%s2==1)then
                SpinNovo = direcao()
                deltaE = 0
                campoEfetivo =campo(i)
                do j = 1, 3  !mudando 2 > 3
                    deltaE = deltaE - (spinNovo%S(j) - rede(i)%S(j))*campoEfetivo%S(j)           
                end do   
                probabilidade = 1.0d0/(1.0d0 + exp(deltaE/T0))
                call random_number(rando)
                if (rando <= probabilidade ) then 
                    !aceita a nova configura��o
                    forall  (j = 1:3)
                        quantidadesTermodinamicas%Magnetizacao(j) = &
                        &quantidadesTermodinamicas%Magnetizacao(j) + (spinNovo%S(j) - rede(i)%S(j))
                    end forall
                    rede(i) = SpinNovo
                    quantidadesTermodinamicas%energia = quantidadesTermodinamicas%energia+deltaE
                    
                end if
            end if    
        end do 
    end Subroutine metropolis 
!-----------------------------------------------------------------------------

    subroutine wolff !ok
        type(Spin) :: direcaoSemente
        type(Spin) :: projecao
        type(Spin) :: projecaoVizinho
        type(Spin) :: campoEfetivo
        double precision :: produto
        double precision :: produtoSemente
        double precision :: produtoVizinho
        double precision :: produtoSitio
        double precision :: probabilidade
        double precision :: deltaE
        integer ::viz, j
        integer :: semente, ponteiro, sitio, sitioVizinho
        real :: JJ
        !inicia ponteiros do cluster 
        ponteiro = 0 !diferente do programa velho porque eu incremento l� na frente 


        !sorteia sitio semente 
        do 
            call random_number(Rando)  
            semente = int(sistema%numeroSitios*Rando)  !baseado no programa velho s�o sei se o gerador diferente pode prejudicar o resultado 
            if (Rede(semente)%S2==1) exit
        end do
       
        direcaoSemente = direcao()
        produto = direcaoSemente%S(1)*rede(semente)%S(1) &
                  & + direcaoSemente%S(2)*rede(semente)%S(2)&
                  & + direcaoSemente%S(3)*rede(semente)%S(3)
        produtoSemente = produto
        
        !calcula proje��o da semente na dire��o semente 
        forall  (j = 1:3)
            projecao%S(j) = direcaoSemente%S(j)*produto
        end forall

        !flipa semente
        forall  (j = 1:3)
            rede(semente)%S(j) = rede(semente)%S(j) - 2.0d0*projecao%S(j)
        end forall
        !Atualiza Energia e magnetização do sistema 
        campoefetivo = campo(semente)
        deltaE = 2.0d0*(projecao%S(1)*campoefetivo%S(1) + projecao%S(2)*campoefetivo%S(2)+projecao%S(3)*campoefetivo%S(3)) 
        quantidadesTermodinamicas%Energia = quantidadesTermodinamicas%Energia  + deltaE
        forall  (j = 1:3)
            quantidadesTermodinamicas%Magnetizacao(j) = quantidadesTermodinamicas%Magnetizacao(j) -2.0d0*Projecao%S(j)
        end forall
        !incrementa TamanhoCluster e cresce a pilha
        ponteiro = ponteiro + 1
        pilha(ponteiro)%site = semente
        pilha(ponteiro)%prod = produtoSemente
        
        !repetições na pilha 
        do while(ponteiro>0)
            !decrementa a pilha
            sitio = pilha(ponteiro)%site
            produtoSitio = pilha(ponteiro)%prod
            ponteiro = ponteiro - 1
            !repetições nos vizinhos 
            do viz = 0, sistema%numeroCoordenacao -1
                sitioVizinho = Vizinhos(viz,sitio) !o primeiro marca o vizinhos e segundo marca o s�tio
                JJ=ligacao(viz,sitio)
                !se o vizinhos do sitio form magnetico continua 
                if (rede(sitioVizinho)%S2==1) then
                    ProdutoVizinho = rede(sitioVizinho)%S(1)*DirecaoSemente%S(1) + &
                                   & rede(sitioVizinho)%S(2)*DirecaoSemente%S(2) + &
                                   & rede(sitioVizinho)%S(3)*DirecaoSemente%S(3)
                    !se o vizinhos est� na mesma dire��o da semente
                    if (ProdutoVizinho*ProdutoSemente>0) then
                        probabilidade=1-exp(-2.0d0*JJ*ProdutoSitio*ProdutoVizinho/T0) 
                        call random_number(Rando)
                        if (Rando<probabilidade) then
                            !aqui pode ser otimizado com forall
                            
                            !calcula proje��o
                            forall  (j = 1:3)
                                ProjecaoVizinho%S(j)=produtoVizinho*DirecaoSemente%S(j)
                            end forall
                            !flipa o spin

                            forall  (j = 1:3)
                                rede(sitioVizinho)%S(j) = rede(sitioVizinho)%S(j) - 2.0d0*projecaoVizinho%S(j)
                            end forall
                            
                            !Atualiza Energia e magnetização do sistema 
                            campoefetivo = campo(sitioVizinho)
                            deltaE = 2.0d0*(ProjecaoVizinho%S(1)*Campoefetivo%S(1) + &
                                          & ProjecaoVizinho%S(2)*campoefetivo%S(2) + &
                                          & ProjecaoVizinho%S(3)*campoefetivo%S(3)) 
                            quantidadesTermodinamicas%energia = quantidadesTermodinamicas%Energia  + deltaE
                            
                            forall  (j = 1:3)
                                quantidadesTermodinamicas%magnetizacao(j) = &
                                &quantidadesTermodinamicas%magnetizacao(j) - 2.0d0*projecaoVizinho%S(j)
                            end forall
                        
                            ponteiro = ponteiro + 1
                            pilha(ponteiro)%site = sitioVizinho
                            pilha(ponteiro)%prod = produtoVizinho
                       end if 
                    end if    
                end if 
            end do  !repetições nos vizinhos 
        end do !repetições na pilha 
    end subroutine wolff
!-----------------------------------------------------------------------------

    Subroutine superrelaxacao !ok
        !variaveis Locais
        integer :: i, j, k
        type(spin) :: campoEfetivo, versorCampoEfetivo, novoSpin, projecao
        double precision :: moduloProjecao, moduloCampoEfetivo
        do i = 0 , sistema%numeroSitios-1
            call random_number(Rando)  
            j = int((sistema%numeroSitios)*Rando) !rever
            if (Rede(j)%S2==1) then
                CampoEfetivo = campo(j)
                moduloCampoEfetivo = sqrt( campoEfetivo%S(1)*campoEfetivo%S(1) + &
                                             & campoEfetivo%S(2)*campoEfetivo%S(2) + &
                                             & campoEfetivo%S(3)*campoEfetivo%S(3))
                if (moduloCampoEfetivo>0) then             
                    forall  (k = 1:3)
                        versorCampoEfetivo%S(k) = campoEfetivo%S(k)/moduloCampoEfetivo
                    end forall
                    
                    moduloProjecao = versorCampoEfetivo%S(1)*Rede(j)%S(1) + &
                                   & versorCampoEfetivo%S(2)*rede(j)%S(2) + &
                                   & versorCampoEfetivo%S(3)*rede(j)%S(3)
                    forall (K=1:3)
                        projecao%S(k) = moduloProjecao*versorCampoEfetivo%S(k)
                        novoSpin%S(k) = - Rede(j)%S(k) + 2*projecao%S(k)
                        quantidadesTermodinamicas%magnetizacao(K) = quantidadesTermodinamicas%magnetizacao(K)&
                                                                  & + novoSpin%S(K) - Rede(j)%S(K)
                        rede(j)%S(k) = novoSpin%S(K)
                    end forall
                end if
            end if
        end do   
   
    End Subroutine Superrelaxacao
 
   
!-----------------------------------------------------------------------------

    subroutine Inicializar
        !Variaveis locais
        integer :: i, j
        type(Spin) :: campoefetivo
            
        quantidadesTermodinamicas%magnetizacao=0 !magnetização instantanea
        quantidadesTermodinamicas%energia=0 !Energia instantanea
        
        do i = 0, sistema%numeroSitios -1
            campoefetivo = campo(i) 
            do j= 1, 3
                quantidadesTermodinamicas%energia = quantidadesTermodinamicas%energia - rede(i)%S(j)*campoefetivo%S(j)  
            end do
        end do 
        
        quantidadesTermodinamicas%energia = quantidadesTermodinamicas%energia/2
        
        forall  (j=1:3)
            quantidadesTermodinamicas%magnetizacao(j) = sum(rede(:)%S(j))
        end forall
    end subroutine inicializar

  !---------------------------------------------
    subroutine marcaLigacao
        integer::  i
        ligacao=1
        do i=0, sistema%numeroSitios-1
            if(rede(i)%s2==0)then
                ligacao(0,vizinhos(0,i))=A
                ligacao(1,vizinhos(1,i))=A
                ligacao(2,vizinhos(2,i))=A
                ligacao(3,vizinhos(3,i))=A
       
                ligacao(0,vizinhos(4 , vizinhos(4,i) ))=A
                ligacao(1,vizinhos(5 , vizinhos(5,i) ))=A
                ligacao(2,vizinhos(6 , vizinhos(6,i) ))=A
                ligacao(3,vizinhos(7 , vizinhos(7,i) ))=A                       
           end if       
         end do
       ! apaga exesso de ligações 
        do i=0,sistema%numeroSitios-1
            if(rede(i)%S2==0 )then
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
        do i=0, sistema%numeroSitios-1
            ligacao(4,i) = ligacao(0,vizinhos(4,i))
            ligacao(5,i) = ligacao(1,vizinhos(5,i))
            ligacao(6,i) = ligacao(2,vizinhos(6,i)) 
            ligacao(7,i) = ligacao(3,vizinhos(7,i))         
        end do
         
     end subroutine marcaLigacao

!-----------------------------------------------------------------------------

subroutine marcarVizinhos !rede bcc ok
!variaveis Locais
    integer,dimension(0:sistema%L-1) :: ant, suc
    integer :: i, j, K, site, L2, L3
    
    L2=sistema%L*sistema%L
    L3=sistema%L*sistema%L*sistema%L    
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

Subroutine diluir
    !Variaveis Locais
    integer :: numeroVacancias
    integer :: i

    NumeroVacancias = int(sistema%numeroSitios*p)
    rede(0:NumeroVacancias-1) = Spin((/0,0,0/),0)
    rede(NumeroVacancias:sistema%numeroSitios-1) = Spin((/1,0,0/),1)
    do i=1,100 
        call Shuffle(rede)
    end do 
   
end subroutine diluir

!-----------------------------------------------------------------------------

subroutine lerDados()
    sistema%numeroCoordenacao=8
    sistema%l = L
    sistema%numeroTermalizacao=MCx
    sistema%numeroPassosMC=Mcc
    sistema%numeroSitios = 2*sistema%L*sistema%L*sistema%L 
end subroutine LerDados
    
!-----------------------------------------------------------------------------

function campo(i)
    !Variaveis mudas 
    type(Spin) :: campo 
    integer, intent(in) :: i 
    !variaveis locais
    integer :: j    
    forall  (j = 1:3)
             campo%S(j) =  rede(vizinhos(0,i))%S(j)*ligacao(0,i) + &
                         & rede(vizinhos(1,i))%S(j)*ligacao(1,i) + &
                         & rede(vizinhos(2,i))%S(j)*ligacao(2,i) + &
                         & rede(vizinhos(3,i))%S(j)*ligacao(3,i) + &
                         & rede(vizinhos(4,i))%S(j)*ligacao(4,i) + &
                         & rede(vizinhos(5,i))%S(j)*ligacao(5,i) + &
                         & rede(vizinhos(6,i))%S(j)*ligacao(6,i) + &
                         & rede(vizinhos(7,i))%S(j)*ligacao(7,i)
    end Forall

end function campo    
 

!-----------------------------------------------------------------------------

function direcao()!Marsaglia(rand)
    !Variaveis mudas 
    type(spin) :: direcao
    
    !variaveis locais
    double precision :: auxiliar1, auxiliar2, auxiliar3, auxiliar4  
    
    do 
        call random_number(rando)
        auxiliar1 = 1- 2*rando
        call random_number(Rando)
        auxiliar2= 1- 2*Rando
        auxiliar3 = auxiliar1*auxiliar1 + auxiliar2*auxiliar2
        if (auxiliar3 <= 1) exit
    end do
    auxiliar4 = sqrt(1 - auxiliar3)
    direcao%S(1) = 2*auxiliar1*auxiliar4
    direcao%S(2) = 2*auxiliar2*auxiliar4
    direcao%S(3) = 1 - 2*auxiliar3
    direcao%S2 = 1
end function direcao !Marsaglia

!-----------------------------------------------------------------------------
!The Knuth shuffle is used to create a random permutation of an array.
subroutine Shuffle(vetor)
    
    !Variaveis mudas 
    type(Spin), dimension(:), intent(inout) :: vetor    
    !Variaveis Locais
    integer :: i, posicao
    type(Spin) :: temp
    
    do i = size(vetor), 2, -1
        call random_number(rando)
        posicao = int(rando * i) + 1
        temp = vetor(posicao)
        vetor(posicao) = vetor(i)
        vetor(i) = temp
    end do
end subroutine Shuffle

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

end subroutine BCC
