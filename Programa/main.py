#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from Simulacao import Simulacao
from Enviar import Enviar
from Salvar import Salvar
from Apresentar import Apresentar
from Parametros import Parametros
if __name__ == '__main__':
    enviar = Enviar(nomeRef='20-05-25-variandoJb-variando2')
    salvar=Salvar()
    apresentar=Apresentar()
    enviar.observe('fimAmostra', enviar)
    salvar.observe('fimAmostra', salvar)
    apresentar.observe('fimAmostra',apresentar)
    
    entrada={}
    entrada['tamanhos'] = [10, 15, 20]
    entrada['relaxacaoHistograma'] = 500000
    entrada['mcsHistograma'] = 1000000
    entrada['A'] = 1.7
    entrada['B'] = 1
    entrada['numeroPontos'] = 300 
    entrada['concentracao'] = [0, 0.021, 0.025, 0.04, 0.05,  0.06,  0.07,  0.075, 0.08, 0.09,  0.1,  0.11, 0.12,0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.198, 0.2, 0.220, 0.24, 0.247, 0.265,0.3, 0.35, 0.4, 0.5, 0.6]
    entrada['t0'] = [2.093366366666573, 2.090345747995239, 2.0900984626582266, 2.085356606645227, 2.0821080839655597, 2.076779561284893, 2.0723577052718927, 2.068997705271893, 2.06588980126356, 2.058713993245893, 2.0519842332245593, 2.044522377211559, 2.0359449981426136, 2.0266895238345595, 2.0164861455181726, 2.0047208624408923, 1.9811231984102264, 1.9559303450332268, 1.9108393476692302, 1.8029851669854682, 1.7840417399201833, 1.6943428571427623, 1.586285714285618, 1.5159999999999052, 1.4107428571427596, 1.1781171428570427, 0.9497571428571693, 0.6977142857143112, 0.4146428571428803, 0.16263698516284694]
    entrada['Js'] = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.993685, 0.986299, 0.970285, 0.924996, 0.920172, 0.888, 0.840227, 0.813345, 0.766485, 0.669217, 0.556957, 0.461901, 0.342527, 0.256863]
    #entrada['clusterlimite']=60
    entrada['numeroAmostras']=10
    entrada['raio']=0.12

    parametros=Parametros(entrada=entrada)
    parametros.observe('fimAmostra',parametros.update)
    simulacao=Simulacao(parametros=parametros)
    simulacao()
