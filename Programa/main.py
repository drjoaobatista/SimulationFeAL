#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from Simulacao import Simulacao
from Enviar import Enviar
from Salvar import Salvar
from Apresentar import Apresentar
from Parametros import Parametros
if __name__ == '__main__':
    enviar = Enviar(nomeRef='20-05-1-A17-c')
    salvar=Salvar()
    apresentar=Apresentar()
    enviar.observe('fimAmostra', enviar)
    salvar.observe('fimAmostra', salvar)
    apresentar.observe('fimAmostra',apresentar)
    
    entrada={}
    entrada['tamanhos']=[10]
    entrada['relaxacaoHistograma']=1000000
    entrada['mcsHistograma']=1000000
    entrada['mcsTroca']=500
    entrada['A']=1.7
    entrada['numeroPontos']=50
    
  #  entrada['concentracao'] = [0,  0.06, 0.084, 0.091, 0.096, 0.141, 0.15, 0.164, 0.166, 0.179, 0.189, 0.253, 0.26, 0.28, 0.299, 0.303, 0.315, 0.317, 0.328, 0.347, 0.394, 0.401, 0.426, 0.5, 0.6, 0.7]
  #  entrada['t0'] = [2.053,  2.030, 2.0163, 2.012, 2.012, 1.964, 1.9527, 1.9362, 1.9348, 1.899, 1.9036, 1.6072, 1.6124, 1.51274, 1.3904, 1.38057, 1.3658, 1.34357, 1.2903, 1.2316, 1.082, 1.0464, 0.97956, 0.7824, 0.5280, 0.2181098]
    #entrada['concentracao'] = [0.5, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.397532, 0.45, 0.5, 0.6, 0.7]
    #entrada['t0'] =[2.084628, 2.042768, 1.9992336, 1.8242588, 1.439984, 1.110405569, 0.845314561, 0.599158924, 0.414541673, 0.24412752, 0.0500425835, 0.00743015]
    entrada['concentracao'] = [0, 0.021, 0.025, 0.04, 0.05,  0.06, 0.07,  0.075, 0.08, 0.09,  0.1,  0.11, 0.12,0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.198, 0.2, 0.22]
    entrada['t0']=[2.093, 2.0909936, 2.0889829, 2.0856345, 2.0809486, 2.0749293, 2.0709165, 2.06757246, 2.0635596, 2.0562027, 2.049510, 2.0421492, 2.034788033333, 2.0247516, 2.0160443, 2.005999366667, 1.9952813, 1.980563266667, 1.959804533333, 1.944392, 1.923590433333, 1.9021072]
    #entrada['concentracao'] = [0.2]
    #entrada['t0'] =[1.7663]
    
    entrada['numeroAmostras']=10
    entrada['raio']=0.12

    parametros=Parametros(entrada=entrada)
    parametros.observe('fimAmostra',parametros.update)
    simulacao=Simulacao(parametros=parametros)
    simulacao()
