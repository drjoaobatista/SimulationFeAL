!compilar para pyton :: f2py -c Histogram.f90 -m Histogram
!f2py --fcompiler=ifort -c Histogram.f90 -m Histogram
!f2py --fcompiler=intelem -c Histogram.f90 -m Histogram

subroutine Histogram(resultado, hist, L, MCc,J1,J2,t0, tin, tfi, num_pontos)
    implicit none
	!saidas
    real(8), intent(out),dimension(0:num_pontos-1,0:8)::resultado
    !entradas
    integer, intent(in),dimension(0:MCc-1,0:2):: hist
!f2py intent(in) :: hist
    integer, intent(in)::L, MCc, num_pontos
!f2py intent(in) ::L, MCx, MCc  inici
    real(8), intent(in)::J2,J1,t0,tin,tfi!, eMax, eMin     !p=0 sistema
!f2py intent(in) :: J2,J1,t0, tin, tfi, dt

    !variaveis locais
    double precision :: T,dt
    double precision :: delta !Delta Temperatura
    double precision :: argmax,eMax, eMin ! energias máximas e mínimas
    double precision :: energia,m   !energia e magnetizações

    double precision :: ex! = exp(n*(-delta*eSitio-argmax))
    double precision :: m2   = 0.0d0
    double precision :: m4   = 0.0d0
    double precision :: E    = 0.0d0
    double precision :: E2   = 0.0d0
    double precision :: E4   = 0.0d0

    double precision :: soma =0.00d0 !soma dos ex
    double precision :: somaM  = 0.0d0
    double precision :: somaM2  = 0.0d0
    double precision :: somaM4  = 0.0d0
    double precision :: somaE    = 0.0d0
    double precision :: somaE2   = 0.0d0
    double precision :: somaE4   = 0.0d0

    double precision :: Mmed  =  0.0d0!somaMz/soma
    double precision :: M2med  =  0.0d0!somaMx2/soma
    double precision :: M4med  =  0.0d0!somaMx4/soma
    double precision :: Emed    =  0.0d0!somaE/soma
    double precision :: E2med   =  0.0d0!somaE2/soma
    double precision :: E4med   =  0.0d0!somaE2/soma

    integer :: contT
    integer ::n
    integer :: i
    integer ::mag,  EJ1, EJ2
    call maxmin(eMax, eMin, J1, J2)

    n = 2*L*L*L !numeroSitios
    T  = tin
    dt = (tfi -tin)/float(num_pontos)
    do contT=0,num_pontos-1  !repetição na temperatura
        delta = 1.0/T -1.0/T0
        if (delta > 0) then
            argmax = -delta*(eMin)
        else
            argmax = -delta*(eMax)
        end if
        soma = 0
        somaM = 0
        somaM2 = 0
        somaM4 = 0
        somaE  = 0
        somaE2 = 0
        somaE4 = 0
        do i=0, MCc-1
            EJ1=hist(i,0)
            EJ2=hist(i,1)
            mag=hist(i,2)
            energia= -(J1*EJ1+ J2*EJ2)
            ex      = exp(-delta*energia-argmax)
            m= float(mag)/n
            m2= m*m
            m4= m2*m2
            E=energia
            E2=E*E
            E4=E2*E2
            soma   = soma   + ex
            somaM  = somaM + m*ex
            somaM2  = somaM2  + m2*ex
            somaM4  = somaM4  + m4*ex
            somaE    = somaE   + E*ex
            somaE2   = somaE2  + E2*ex
            somaE4   = somaE4  + E4*ex
        end do
        Mmed   = somaM/soma
        M2med  = somaM2/soma
        M4med  = somaM4/soma
        Emed   = somaE/soma
        E2med  = somaE2/soma
        E4med  = somaE4/soma
        resultado(contT,0)= T
        resultado(contT,1)= Mmed !magnetizacao
        resultado(contT,2)= M2med*n !magnetizacao2 +
        resultado(contT,3)= log(M2med*n*n) !logMag2
        resultado(contT,4)= Emed !energia
        resultado(contT,5)= (E2med - (Emed)**2)/(T*T)*n !calorEspecifico
        resultado(contT,6)= (M2med - (Mmed)**2)/T*n !susceptibilidade
        resultado(contT,7)= (1-M4med/(3*M2med**2)) !cumulante
        resultado(contT,8)= (1-E4med/(3*E2med**2))  !cumulanteE
        T = T + dt
    end do  !temperatura
!fim do programa
CONTAINS
    subroutine  maxmin(eMax_, eMin_,J1_, J2_)
       !Variaveis mudas
       double precision, intent(out) :: eMax_, eMin_
       double precision, intent(in) :: J1_, J2_
      !Variaveis Locais
       integer :: i_
       integer :: ej1, ej2
       double precision :: e_
        ej1=hist(0,0)
        ej2=hist(0,1)
        e_=-(J1_*ej1+J2_*ej2)
        eMax_ = e_
        eMin_ = e_
        do i_=1, mcc -1
            ej1=hist(i_,1)
            ej2=hist(i_,1)
            e_=-(J1_*ej1+J2_*ej2)
            if (e_ > eMax_) eMax_ = e
            if (e_ < eMin_) eMin_ = e
        end do
        close(2)
    end subroutine
end subroutine  Histogram
