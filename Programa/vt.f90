!compilar para pyton :: f2py3 -c vt.f90 -m vt
!#DONE: colocar abs na staggeredMag, deu errado corrigindo colocando ABS na media 
!#DONE: Trocar o nome do parametro staggeredMag staggered magnetization

subroutine  vtBCC(resultado, amostra, L, MCx, MCc, MCt, A, B, q, tini,tfi, npontos)
    implicit none
    !saidas
    real(8), intent(out),dimension(0:npontos-1,0:7):: resultado
    !f2py intent(out) :: resultado       
    real(8), intent(out),dimension(0:2*L*L*L-1, 0:3):: amostra
    !f2py intent(out) :: resultado       
    !entradas 
    integer, intent(in)::L, MCx, MCc, MCt, nPontos!
    !f2py intent(in) : MCt, :L,   MCx, MC, npontos   
    real(8), intent(in):: A, B,tini,tfi, q    !=0 sistema puro
    !f2py intent(in) :: A, B, tin,tfin, q
    type :: Spin
        real, dimension(1:3) :: s !mudei de double precision para real 
        integer :: s2
    end type Spin
    
    type :: Quantidades
        double precision,dimension(1:3) :: magnetizacao !magnetização instantanea
        double precision :: energia !Energia instantanea
        double precision :: energia2 !Energia instantanea
        double precision :: staggeredMag !Energia instantanea
    end type Quantidades

    type :: Configuracao
        integer :: l
        integer :: numeroCoordenacao
        integer :: numeroSitios
        integer :: numeroTermalizacao
        integer :: numeroPassosMC
        integer :: numeroTentativas
        integer :: numeroVacancias
    end type Configuracao
   
    type :: Disco  !usado para construir a pilha 
         double precision::prod
         integer::site
    end type Disco

!Variaveis Locais

    intrinsic random_seed, random_number
    type(Spin), dimension(:), allocatable :: rede
    type(Disco), dimension(:), allocatable :: pilha
    type(Quantidades) :: quantidadesTermodinamicas
    type(Configuracao) :: sistema 
    integer, dimension(:,:), allocatable :: vizinhos
    real, dimension(:,:), allocatable :: ligacao, ligacao2
    integer,  dimension(:), allocatable ::  zeros
    real, dimension(1:3) :: campoEfetivo0
    real :: rando
    integer :: passo, passo2, contt
    real :: tin, dt, t0

    double precision :: somaMag, somaMag2, somaMag4, somaEne, somaEne2, somaEne4,  somaStaggeredMag, somaStaggeredMag2
    double precision  :: mediaMag, mediaMag2, mediaMag4, mediaEne, mediaEne2, mediaEne4, susceptibilidade, cumuM, calor
    double precision  :: mediaStaggeredMag,mediaStaggeredMag2, susStaggeredMag
    double precision  :: aux
!------------------------------------------------
    
    call lerDados()

    !inicilização das variaveis 
    allocate(rede(0:sistema%numeroSitios-1))
    allocate(zeros(0:int(sistema%numeroSitios*q)-1))
    allocate(pilha(1:sistema%numeroSitios)) !usa no wollf e precisa comecar do 1
    allocate(vizinhos(0:sistema%numeroCoordenacao-1, 0:sistema%numeroSitios-1))
    allocate(ligacao(0:sistema%numeroCoordenacao-1, 0:sistema%numeroSitios-1))


    tin=max(tini, 0.1)
    dt=(tfi-tin)/nPontos
    resultado=0
    call init_random_seed()   
    ! call random_seed(PUT=seed)
    call marcarVizinhos
    call diluir !gera amostra 
    call marcaLigacao     
    call inicializar
    t0 = tin
    do contT = 0 , nPontos-1
        call iniciaVariaveis
        !repetiÃ§Ãµes termalizaÃ§Ã£o
        do passo2=0, MCt-1
            do passo = 0, int(MCx/MCt)-1
                call metropolis
                call wolff
                call superrelaxacao
            end do 
            call trocaAlFe    
        end do
    !repetições para media
        do passo2=0, MCt-1
            do passo = 0, int(MCc/MCt)-1
                call metropolis
                call wolff
                call superrelaxacao
                call calcularSoma
            end do 
            call trocaAlFe  
            aux=quantidadesTermodinamicas%staggeredMag/sistema%numeroSitios 
            somaStaggeredMag=somaStaggeredMag + aux
            somaStaggeredMag2=somaStaggeredMag2 + aux*aux
        end do
        call calcularMedia
        t0=t0+dt
        end do
        do i = 0, sistema%numeroSitios-1
            amostra(i,0) = rede(i)s(0)         
            amostra(i,1) = rede(i)s(1)         
            amostra(i,2) = rede(i)s(2)         
            amostra(i,3) = rede(i)s2         
        end do
    
CONTAINS
subroutine calcularMedia
    integer  ::numeroSitios
    numeroSitios = 2*L**3
    mediaMag =  somaMag/MCc
    mediaMag2 = somaMag2/MCc
    mediaMag4 = somaMag4/MCc
    mediaEne =  somaEne/MCc
    mediaEne2=  somaEne2/MCc
    mediaEne4=  somaEne4/MCc
    calor= (MediaEne2-mediaEne*mediaEne)/NumeroSitios/t0/t0
    susceptibilidade= (mediaMag2 - MediaMag*MediaMag)*NumeroSitios/t0
    cumuM=1-mediaMag4/(3*mediaMag2*mediaMag2)  ! rever essa equaÃ§Ã£o soma ou media
    mediaStaggeredMag=somaStaggeredMag/MCt
    mediaStaggeredMag2=somaStaggeredMag2/MCt
    susStaggeredMag= (mediaStaggeredMag2 - mediaStaggeredMag*mediaStaggeredMag)*NumeroSitios/t0
    if (.NOT.isnan(susceptibilidade)) then
         resultado(contT,0)= t0
         resultado(contT,1)= susceptibilidade
         resultado(contT,2)= calor
         resultado(contT,3)= cumuM
         resultado(contT,4)= mediaMag
         resultado(contT,5)= mediaEne
         resultado(contT,6)= abs(mediaStaggeredMag)
         resultado(contT,7)= susStaggeredMag
    end if
end subroutine calcularMedia
!-----------------------------------------------------------------------------
subroutine calcularSoma
    double precision ::mag, mag2, mag4, ene,ene2, ene4
    INTEGER:: i
    mag2=0
    do i = 1, 3
        mag2=mag2 + quantidadesTermodinamicas%magnetizacao(i)*quantidadesTermodinamicas%magnetizacao(i)    
    end do
    mag2=mag2/sistema%numeroSitios/sistema%numeroSitios
    mag=sqrt(mag2)
    mag4=mag2*mag2
    ene =  quantidadesTermodinamicas%energia + quantidadesTermodinamicas%energia2
    ene2 = ene*ene
    ene4 = ene2*ene2
    somaMag=somaMag+mag
    somaMag2=somaMag2+mag2
    somaMag4=somaMag4+mag4
    somaEne=somaEne+ene
    somaEne2=somaEne2+ene2
    somaEne4=somaEne4+ene4
end subroutine calcularSoma

subroutine iniciaVariaveis
    somaMag=0
    somaMag2=0
    somaMag4=0
    somaEne=0
    somaEne2=0
    somaEne4=0
    somaStaggeredMag=0
    somaStaggeredMag2=0
end subroutine iniciaVariaveis

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
    real :: produto
    real :: produtoSemente
    real :: produtoVizinho
    real :: produtoSitio
    real :: probabilidade
    real :: deltaE
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
    integer :: j
    quantidadesTermodinamicas%energia=energia()
    quantidadesTermodinamicas%energia2=energia2()
    forall  (j=1:3)
        quantidadesTermodinamicas%magnetizacao(j) = sum(rede(:)%S(j))
    end forall
    quantidadesTermodinamicas%staggeredMag=2*(sum(rede(0:L*L*L-1)%S2)-sum(rede(L*L*L:2*L*L*L-1)%S2))
end subroutine inicializar

!---------------------------------------------
subroutine marcaLigacao
    integer::  i
    ligacao=1
    do i=0, sistema%numeroSitios-1
        if (rede(i)%s2==0) then
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
    integer :: i,cont
    if ( q==0 ) then
        rede(0:sistema%numeroSitios-1) = Spin((/1,0,0/),1)
    else
        rede(0:sistema%numeroVacancias-1) = Spin((/0,0,0/),0)
        rede(sistema%numeroVacancias : sistema%numeroSitios-1) = Spin((/1,0,0/),1)
        do i=1,100 
            call Shuffle(rede)
        end do 
        cont=0
        do i=0 ,sistema%numeroSitios-1
            if (rede(i)%S2==0 ) then
                zeros(cont)=i 
                cont=cont+1
            end if
        end do
    end if
end subroutine diluir

!-----------------------------------------------------------------------------

subroutine lerDados()
    sistema%numeroCoordenacao=8
    sistema%l = L
    sistema%numeroTermalizacao=MCx
    sistema%numeroPassosMC=Mcc
    sistema%numeroSitios = 2*sistema%L*sistema%L*sistema%L 
    sistema%numeroTentativas=int(sistema%numeroSitios*sistema%numeroCoordenacao*q)
    sistema%numeroVacancias=int(sistema%numeroSitios*q)
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
subroutine trocaAlFe()
    implicit none
    INTEGER i,k, j, cont
    real::E2, E1
    do cont=1, sistema%numeroTentativas
        call random_number(rando)
        k=int(rando*sistema%numeroVacancias)
        i=zeros(k)
        call random_number(rando)
        j = vizinhos(int(rando*sistema%numeroCoordenacao),i)
        if (rede(j)%S2==1) then
            Ligacao2=Ligacao
            rede(i)=rede(j)
            rede(j)=Spin((/0,0,0/),0)
            call marcaLigacao
            E2=Energia2()
            E1=Energia()
            call random_number(rando)
            if (rando< exp(-( E2 + E1 - quantidadesTermodinamicas%energia2 - quantidadesTermodinamicas%energia)/t0)) then
                quantidadesTermodinamicas%energia=E1
                quantidadesTermodinamicas%energia2=E2
               ! quantidadesTermodinamicas%ordemA=ordemA()
                if (i<L*L*L )then 
                    quantidadesTermodinamicas%staggeredMag= quantidadesTermodinamicas%staggeredMag+4
                else
                    quantidadesTermodinamicas%staggeredMag= quantidadesTermodinamicas%staggeredMag-4
                end if
                zeros(k)=j
                !if (quantidadesTermodinamicas%ordemA==0.00) exit
            else
                rede(j)=rede(i)
                rede(i)=Spin((/0,0,0/),0)
                Ligacao=Ligacao2
            end if          
        end if
    end do
end subroutine trocaAlFe

!----------------------------------------------------------------

function Energia()  !termo Magnetico
    real:: Energia
    integer :: i, j
    Energia=0
    do i = 0, sistema%numeroSitios -1
        forall  (j = 1:3)
            campoefetivo0(j) = (rede(vizinhos(0,i))%S(j)*ligacao(0,i) + &
                             & rede(vizinhos(1,i))%S(j)*ligacao(1,i) + &
                             & rede(vizinhos(2,i))%S(j)*ligacao(2,i) + &
                             & rede(vizinhos(3,i))%S(j)*ligacao(3,i) )
        end forall
        do j= 1, 3
            energia = energia - rede(i)%S(j)*campoefetivo0(j)  
        end do
    end do 
end function Energia

!------------------------------------------------------------------------------
function Energia2() !Termo quimico
    real:: Energia2
    integer :: i, j
    Energia2=0
    do i = 0, sistema%numeroSitios -1
        do j= 1, 3
            energia2 = energia2 - rede(i)%s2*(rede(vizinhos(0,i))%s2 + &
                    & rede(vizinhos(1,i))%s2 + &
                    & rede(vizinhos(2,i))%s2 + &
                    & rede(vizinhos(3,i))%s2)
        end do
    end do
    energia2=energia2*B
end function Energia2

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

end subroutine vtBCC