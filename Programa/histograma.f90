!compilar para pyton :: f2py3 -c analisaHist.f90 -m analisaHist
! print Histogram.__doc__
!f2py --fcompiler=ifort -c Histogram.f90 -m Histogram
!f2py --fcompiler=intelem -c Histogram.f90 -m Histogram
!2020 
subroutine Histogram(resultado, hist, L, MCc,t0, tin, tfi, num_pontos)
    implicit none
	!saidas
    real(8), intent(out),dimension(0:num_pontos-1,0:8)::resultado
    !entradas     
    real(8) , intent(in),dimension(0:MCc-1,0:3):: hist
    !f2py intent(in) :: hist       
    integer, intent(in)::L, MCc, num_pontos  
    !f2py intent(in) ::L, MCx, MCc  inici
    real(8), intent(in)::t0,tin,tfi!, eMax, eMin     !p=0 sistema
    !f2py intent(in) :: t0, tin, tfi
    
   !variaveis        
    double precision :: T , dt 
    double precision :: delta !Delta Temperatura 
    double precision :: eMax,eMin, argmax ! energias máximas e mínimas 
    double precision :: ex! = exp(n*(-delta*eSitio-argmax))  

 
    double precision :: m2!    = mx2+my*my
    double precision :: m4!    = m2*m2
    double precision :: m!     = sqrt(m2)
    double precision :: E    = 0.0d0
    double precision :: E2   = 0.0d0
    double precision :: E4   = 0.0d0
    
    double precision :: soma !soma dos ex
    double precision :: somaM  = 0.0d0
    double precision :: somaM2 = 0.0d0
    double precision :: somaM4 = 0.0d0
    double precision :: somaE    = 0.0d0
    double precision :: somaE2   = 0.0d0
    double precision :: somaE4   = 0.0d0
    
    double precision :: Mmed  = 0.0d0! somaM/soma
    double precision :: M2med =  0.0d0!somaM2/soma
    double precision :: M4med =  0.0d0!somaM4/soma
    double precision :: Emed    =  0.0d0!somaE/soma
    double precision :: E2med   =  0.0d0!somaE2/soma
    double precision :: E4med   =  0.0d0!somaE2/soma
    
    integer :: contT !numero de sitios 
    integer :: n !numero de sitios 
    integer :: i

    call maxmin
    n = 2*L*L*L !numeroSitios
    T  =max(tin, 0.01)
    dt = (tfi -T)/float(num_pontos)  

    do contT=0,num_pontos-1  !repetição na temperatura
       delta = 1.0/T -1.0/T0
       if (delta > 0) then
          argmax = -delta*(eMin)
       else 
          argmax = -delta*(eMax)
       end if       
        soma     = 0.0d0
        somaM    = 0.0d0
        somaM2   = 0.0d0
        somaM4   = 0.0d0
        somaE    = 0.0d0
        somaE2   = 0.0d0
        somaE4   = 0.0d0
        do i=0, MCc-1 
             ex      = exp((-delta*hist(i,0)-argmax)*n) 
             m2      = hist(i,1)**2 +hist(i,2)**2 +hist(i,3)**2
             m4      = m2**2
             m       = sqrt(m2)
             E=hist(i,0)*n
             E2=E**2
             E4=E2**2
             soma   = soma   + ex
             somaM  = somaM + m*ex
             somaM2 = somaM2 + m2*ex
             somaM4 = somaM4 + m4*ex            
             somaE    = somaE   + E*ex
             somaE2   = somaE2  + E2*ex
             somaE4   = somaE4  + E4*ex
          end do 
          Mmed  = somaM/soma
          M2med = somaM2/soma
          M4med = somaM4/soma
          Emed    = somaE/soma
          E2med   = somaE2/soma
          E4med   = somaE4/soma
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
    end do  
!fim do programa 

CONTAINS
   subroutine  maxmin
      !Variaveis Locais
       integer :: i_ 
       double precision :: e_
       e_=hist(0,0)
       eMax = e_ 
       eMin = e_
       do i_=1, mcc-1
          e_=hist(i_,0)
          if (e_ > eMax) eMax = e_
          if (e_ < eMin) eMin = e_
       end do
       return
    end subroutine

 end
