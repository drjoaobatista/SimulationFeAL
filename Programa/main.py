#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from Simulacao import Simulacao
from Enviar import Enviar
from Salvar import Salvar
from Apresentar import Apresentar
from Parametros import Parametros
if __name__ == '__main__':
    enviar = Enviar(nomeRef='teste30-06b')
    descricao="testando o diagrama completo variando t e q lentamente buscando a fase B2 e a ordem desordem"
    #enviar(descricao) #TODO enviar as descrição 
    salvar=Salvar()
    apresentar=Apresentar()
    enviar.observe('fimAmostra', enviar)
    salvar.observe('fimAmostra', salvar)
    apresentar.observe('fimAmostra',apresentar)
    
    entrada={}    
    entrada['tInicio']=4
    entrada['tFinal']=0.1
    entrada['numeroPontos']=40
    entrada["numeroAmostras"]=4
    entrada['tamanhos']=[5,10]
    entrada['relaxacao']=100000
    entrada['mcs']=100000
    entrada['mcsTroca']=2000
    entrada['concentracao']=[0.0] #, 0.1, 0.2, 0.22, 0.23, 0.24, 0.25, 0.3]
    entrada['A']=1.7
    entrada['B']=-1.1   
    parametros=Parametros(entrada=entrada)
    simulacao=Simulacao(parametros=parametros)
    simulacao()



