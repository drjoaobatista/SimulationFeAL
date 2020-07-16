!compilar para pyton :: f2py3 -c vt.f90 -m vt
!#DONE: colocar abs na staggeredMag, deu errado corrigindo colocando ABS na media 
!#DONE: Trocar o nome do parametro staggeredMag staggered magnetization

subroutine  vtBCC(resultado, amostra, L, MCx, MCc, MCt, A, B, q, tini,tfi, npontos)
    implicit none
    !saidas
    real(4), intent(out),dimension(0:npontos-1,0:7):: resultado
    !f2py intent(out) :: resultado       
    real(4), intent(out),dimension(0:2*L*L*L-1, 0:2) :: amostra
    !f2py intent(out) :: amostra       
    !entradas 
    integer, intent(in):: L, MCx, MCc, MCt, nPontos
    !f2py intent(in) :: L, MCx, MCc, MCt, npontos   
    real(4), intent(in):: A, B, tini, tfi, q    !q=0 sistema puro
    !f2py intent(in) :: A, B, tini, tfi, q

    type :: Quantidades
        real,dimension(0:2) :: magnetizacao !magnetização instantanea
        real :: energia !Energia instantanea
        real :: energia2 !Energia instantanea
        real :: staggeredMag !Energia instantanea
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
   


!Variaveis Locais

    intrinsic random_seed, random_number
    real, dimension(0:2*L*L*L-1,0:2) :: rede
    integer , dimension(0:2*L*L*L-1) :: rede2
    integer, dimension(0:7, 0:2*L*L*L-1) :: vizinhos
    real, dimension(0:7, 0:2*L*L*L-1):: ligacao, ligacao2

    integer, dimension(0:2*L*L*L-1):: pilhaI
    real, dimension(0:2*L*L*L-1):: pilhaP
    type(Quantidades) :: quantidadesTermodinamicas
    type(Configuracao) :: sistema 
    integer,  dimension(0:int(2*L*L*L*q)-1) ::  zeros
    real, dimension(0:2) :: campoEfetivo0
    real :: rando
    integer :: passo, passo2, contt
    real :: tin, dt, t0

    real  :: somaMag, somaMag2, somaMag4, somaEne, somaEne2, somaEne4,  somaStaggeredMag, somaStaggeredMag2
    real  :: mediaMag, mediaMag2, mediaMag4, mediaEne, mediaEne2, mediaEne4, susceptibilidade, cumuM, calor
    real  :: mediaStaggeredMag, mediaStaggeredMag2, susStaggeredMag
    real  :: aux
!------------------------------------------------
    
    call lerDados()
    !inicilização das variaveis 
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
        amostra = rede         
        

    
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
    real ::mag, mag2, mag4, ene,ene2, ene4
    mag2= sum(quantidadesTermodinamicas%magnetizacao*quantidadesTermodinamicas%magnetizacao)    
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
    real, dimension(0:2) :: spinNovo
    real, dimension(0:2) :: campoEfetivo
    integer :: i, j
    real :: deltaE 
    real :: probabilidade
    
    do i=0, sistema%numeroSitios-1
        if (rede2(i)==1)then
            SpinNovo = direcao()
            deltaE = 0
            campoEfetivo =campo(i)
            do j = 0, 2  
                deltaE = deltaE - (spinNovo(j) - rede(i, j))*campoEfetivo(j)           
            end do   
            probabilidade = 1.00/real(1.0d0 + exp(deltaE/T0))
            call random_number(rando)
            if (rando <= probabilidade ) then 
                !aceita a nova configura��o
                forall  (j = 0:2)
                    quantidadesTermodinamicas%Magnetizacao(j) = &
                    &quantidadesTermodinamicas%Magnetizacao(j) + (spinNovo(j) - rede(i,j))
                end forall
                rede(i,:) = SpinNovo
                quantidadesTermodinamicas%energia = quantidadesTermodinamicas%energia+deltaE   
            end if
        end if    
    end do 
end Subroutine metropolis 
!-----------------------------------------------------------------------------

subroutine wolff !ok
    real, dimension(0:2) :: direcaoSemente
    real, dimension(0:2) :: projecao
    real, dimension(0:2) :: projecaoVizinho
    real, dimension(0:2) :: campoEfetivo  
    real :: produto
    real :: produtoSemente
    real :: produtoVizinho
    real :: produtoSitio
    real :: probabilidade
    real :: deltaE
    integer ::viz
    integer :: semente, ponteiro, sitio, sitioVizinho
    real :: JJ
    !inicia ponteiros do cluster 
    ponteiro = 0 !diferente do programa velho porque eu incremento l� na frente 
    !sorteia sitio semente 
    do 
        call random_number(Rando)  
        semente = int(sistema%numeroSitios*Rando)  !baseado no programa velho s�o sei se o gerador diferente pode prejudicar o resultado 
        if (Rede2(semente)==1) exit
    end do
    direcaoSemente = direcao()
    produto =  sum(direcaoSemente*rede(semente,:))
    produtoSemente = produto
    projecao = direcaoSemente*produto !calcula projeção da semente na direção semente 
    rede(semente,:) = rede(semente,:) - 2.00*projecao       !flipa semente
    campoefetivo = campo(semente) !Atualiza Energia e magnetização do sistema 
    deltaE = 2.00*sum(projecao*campoefetivo)
    quantidadesTermodinamicas%Energia = quantidadesTermodinamicas%Energia  + deltaE
    quantidadesTermodinamicas%Magnetizacao = quantidadesTermodinamicas%Magnetizacao -2.00*Projecao
    
    !incrementa TamanhoCluster e cresce a pilha
    ponteiro = ponteiro + 1
    pilhaI(ponteiro) = semente
    pilhaP(ponteiro) = produtoSemente
    
    !repetições na pilha 
    do while(ponteiro>0)
        !decrementa a pilha
        sitio = pilhaI(ponteiro)
        produtoSitio = pilhaP(ponteiro)
        ponteiro = ponteiro - 1
        !repetições nos vizinhos 
        do viz = 0, sistema%numeroCoordenacao -1
            sitioVizinho = Vizinhos(viz,sitio) !o primeiro marca o vizinhos e segundo marca o s�tio
            !se o vizinhos do sitio form magnetico continua 
            if (rede2(sitioVizinho)==1) then
                ProdutoVizinho = sum(rede(sitioVizinho,:)*DirecaoSemente)
                !se o vizinhos está na mesma direção da semente
                if (ProdutoVizinho*ProdutoSemente>0) then
                    JJ=ligacao(viz,sitio)
                    probabilidade=1-exp(-2.00*JJ*ProdutoSitio*ProdutoVizinho/T0) 
                    call random_number(Rando)
                    if (Rando<probabilidade) then                   
                        ProjecaoVizinho=produtoVizinho*DirecaoSemente !calcula projeção
                        rede(sitioVizinho,:) = rede(sitioVizinho,:) - 2.00*projecaoVizinho !flipa o spin
                        campoefetivo = campo(sitioVizinho)
                        deltaE = 2.00*Sum(ProjecaoVizinho*Campoefetivo)
                        quantidadesTermodinamicas%energia = quantidadesTermodinamicas%Energia  + deltaE !Atualiza Energia e magnetização do sistema 
                        quantidadesTermodinamicas%magnetizacao=quantidadesTermodinamicas%magnetizacao - 2.00*projecaoVizinho
                        ponteiro = ponteiro + 1
                        pilhaI(ponteiro) = sitioVizinho
                        pilhaP(ponteiro) = produtoVizinho
                    end if 
                end if    
            end if 
        end do  !repetições nos vizinhos 
    end do !repetições na pilha 
end subroutine wolff
!-----------------------------------------------------------------------------

Subroutine superrelaxacao !ok
    integer :: i, j
    real, dimension(0:2):: campoEfetivo, versorCampoEfetivo, novoSpin, projecao
    real :: moduloProjecao, moduloCampoEfetivo
    do i = 0 , sistema%numeroSitios-1
        call random_number(Rando)  
        j = int((sistema%numeroSitios)*Rando) !rever
        if (Rede2(j)==1) then
            CampoEfetivo = campo(j)
            moduloCampoEfetivo = sqrt(sum(campoEfetivo*campoEfetivo))             
            versorCampoEfetivo = campoEfetivo/moduloCampoEfetivo
            moduloProjecao = sum(versorCampoEfetivo*Rede(j,:))
            projecao = moduloProjecao*versorCampoEfetivo
            novoSpin = -Rede(j,:) + 2*projecao
            quantidadesTermodinamicas%magnetizacao = quantidadesTermodinamicas%magnetizacao + novoSpin - Rede(j,:)
            rede(j,:) = novoSpin
        end if
    end do   
End Subroutine Superrelaxacao

!-----------------------------------------------------------------------------

subroutine Inicializar
    !Variaveis locais
    integer :: j
    quantidadesTermodinamicas%energia=energia()
    quantidadesTermodinamicas%energia2=energia2()
    forall  (j=0:2)
        quantidadesTermodinamicas%magnetizacao(j) = sum(rede(:,j))
    end forall
    quantidadesTermodinamicas%staggeredMag=2*(sum(rede2(0:L*L*L-1))-sum(rede2(L*L*L:2*L*L*L-1)))
end subroutine inicializar

!---------------------------------------------
subroutine marcaLigacao
    integer::  i
    ligacao=1
    ! do i=0, sistema%numeroSitios-1
    !     if (rede2(i)==0) then
    !         ligacao(0,vizinhos(0,i)) = ligacao(0,vizinhos(0,i)) +1
    !         ligacao(1,vizinhos(1,i)) = ligacao(1,vizinhos(1,i)) +1
    !         ligacao(2,vizinhos(2,i)) = ligacao(2,vizinhos(2,i)) +1
    !         ligacao(3,vizinhos(3,i)) = ligacao(3,vizinhos(3,i)) +1
   
    !         ligacao(0,vizinhos(4 , vizinhos(4,i) )) = ligacao(0,vizinhos(4 , vizinhos(4,i) )) +1
    !         ligacao(1,vizinhos(5 , vizinhos(5,i) )) = ligacao(1,vizinhos(5 , vizinhos(5,i) )) +1
    !         ligacao(2,vizinhos(6 , vizinhos(6,i) )) = ligacao(2,vizinhos(6 , vizinhos(6,i) )) +1
    !         ligacao(3,vizinhos(7 , vizinhos(7,i) )) = ligacao(3,vizinhos(7 , vizinhos(7,i) )) +1
    !    end if       
    !  end do

     do i=0, sistema%numeroSitios-1
        if (rede2(i)==0) then
            ligacao(0,vizinhos(0,i)) = A
            ligacao(1,vizinhos(1,i)) = A
            ligacao(2,vizinhos(2,i)) = A
            ligacao(3,vizinhos(3,i)) = A
   
            ligacao(0,vizinhos(4 , vizinhos(4,i) )) = A
            ligacao(1,vizinhos(5 , vizinhos(5,i) )) = A
            ligacao(2,vizinhos(6 , vizinhos(6,i) )) = A
            ligacao(3,vizinhos(7 , vizinhos(7,i) )) = A
       end if       
     end do
    !apaga exesso de ligações 
    do i=0,sistema%numeroSitios-1
        if(rede2(i)==0 )then
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
    rede(:,0) = 1
    rede(:,1) = 0
    rede(:,2) = 0
    rede2(:) =  1
    if (sistema%numeroVacancias>0 )then
        rede2(0:sistema%numeroVacancias-1) =0
        do i=1,100 
            call Shuffle(rede2)
        end do 
        cont=0
        do i=0, sistema%numeroSitios-1
            if (rede2(i)==0 ) then
                rede(i,0) = 0
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
    real, dimension(0:2) :: campo 
    integer, intent(in) :: i 
    !variaveis locais
    integer :: j    
    forall  (j = 0:2)
          campo(j) =  rede(vizinhos(0,i),j)*ligacao(0,i) + &
                    & rede(vizinhos(1,i),j)*ligacao(1,i) + &
                    & rede(vizinhos(2,i),j)*ligacao(2,i) + &
                    & rede(vizinhos(3,i),j)*ligacao(3,i) + &
                    & rede(vizinhos(4,i),j)*ligacao(4,i) + &
                    & rede(vizinhos(5,i),j)*ligacao(5,i) + &
                     & rede(vizinhos(6,i),j)*ligacao(6,i) + &
                    & rede(vizinhos(7,i),j)*ligacao(7,i)
    end Forall
end function campo    

!-----------------------------------------------------------------------------

function direcao()!Marsaglia(rand)
    !Variaveis mudas 
    real, dimension(0:2) :: direcao
    !variaveis locais
    real :: auxiliar1, auxiliar2, auxiliar3, auxiliar4  
    do 
        call random_number(rando)
        auxiliar1 = 1- 2*rando
        call random_number(Rando)
        auxiliar2= 1- 2*Rando
        auxiliar3 = auxiliar1*auxiliar1 + auxiliar2*auxiliar2
        if (auxiliar3 <= 1) exit
    end do
    auxiliar4 = sqrt(1 - auxiliar3)
    direcao(0) = 2*auxiliar1*auxiliar4
    direcao(1) = 2*auxiliar2*auxiliar4
    direcao(2) = 1 - 2*auxiliar3

end function direcao !Marsaglia

!-----------------------------------------------------------------------------
subroutine trocaAlFe()
    implicit none
    INTEGER i,k, j, cont
    real::DeltaE2, E1
    do cont=1, sistema%numeroTentativas
        call random_number(rando)
        k=int(rando*sistema%numeroVacancias)
        i=zeros(k)
        call random_number(rando)
        j = vizinhos(int(rando*sistema%numeroCoordenacao),i)
        if (rede2(j)==1) then
            Ligacao2=Ligacao
            DeltaE2 =  B*( sum(rede2(vizinhos(:,i))) -sum(rede2(vizinhos(:,j))) -1)
            rede(i,:)=rede(j,:)
            rede2(i)=1
            rede(j,:)=0.0
            rede2(j)=0
            call marcaLigacao
            E1=Energia()
            call random_number(rando)
            if (rando < exp(-( DeltaE2 + E1 - quantidadesTermodinamicas%energia)/t0)) then
                quantidadesTermodinamicas%energia=E1
                quantidadesTermodinamicas%energia2=quantidadesTermodinamicas%energia2 + DeltaE2
                if (i<L*L*L)then 
                    quantidadesTermodinamicas%staggeredMag= quantidadesTermodinamicas%staggeredMag+4
                else
                    quantidadesTermodinamicas%staggeredMag= quantidadesTermodinamicas%staggeredMag-4
                end if
                zeros(k)=j
            else
                rede(j,:)=rede(i, : )
                rede2(j)=1
                rede(i,:)=0
                rede2(i)=0
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
        forall  (j = 0:2)
        campoefetivo0(j) = (rede(vizinhos(0,i),j)*ligacao(0,i) + &
                          & rede(vizinhos(1,i),j)*ligacao(1,i) + &
                          & rede(vizinhos(2,i),j)*ligacao(2,i) + &
                          & rede(vizinhos(3,i),j)*ligacao(3,i) )
        end forall
        do j= 0, 2
            energia = energia - rede(i,j)*campoefetivo0(j)  
        end do
    end do 
end function Energia

!------------------------------------------------------------------------------
function Energia2() !Termo quimico
    real:: Energia2
    integer :: i
    Energia2=0
    do i = 0, sistema%numeroSitios -1
            energia2 = energia2 - rede2(i)*(rede2(vizinhos(0,i)) + &
                                          & rede2(vizinhos(1,i)) + &
                                          & rede2(vizinhos(2,i)) + &
                                          & rede2(vizinhos(3,i)))
    end do
    energia2=energia2*B
end function Energia2

!-----------------------------------------------------------------------------
!The Knuth shuffle is used to create a random permutation of an array.
subroutine Shuffle(vetor)
    !Variaveis mudas 
    INTEGER, dimension(:), intent(inout) :: vetor    
    !Variaveis Locais
    integer :: i, posicao, temp

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