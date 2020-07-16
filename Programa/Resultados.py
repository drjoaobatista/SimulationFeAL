
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
""" Implementação de um programa de simulação de sistemas diluido génerico.
    Este módulo implementa uma classe secundaria da simulação, que tem por objeto apresentar resultados da simulação. 
"""
 
__author__ = "João Batista dos Santos-Filho"
__copyright__ = "Copyright 2017, by Santos-Filho"
__credits__ = "Todos desenvolvedores de software livre"
__license__ = "GNU General Public License"
__version__ = "0.20.04"
__maintainer__ = "João Batista dos Santos-Filho"
__email__ = "dr@joaobatista.eng.br"
__status__ = "alpha"

#TODO: compara todos arquivos 
#TODO: documentar todos Arquivos
#TODO: Colocar o todo de cada arquivo  
#TODO: revisar os testes 
#TODO: metrificar meu trabalho
#TODO: usar notas da simulacao para salvar no firebasebase
#TODO: criar um sistema de versionamento mais eficiente 
#TODO: criar um pacote python 
#TODO: contar quanto processos faltam 
#TODO: colocar carga da cpu como dado de entrada 
#TODO: usar keywords
#TODO: melhorar a apresentacao da espera dos novos processos print('.', end="") ok
#FIXME: se processador estiver acima do limite o programa rodar 1 processo quando o processo finalizar ele vai travar

from Enviar import Receber
import threading
import os.path
import json
from Observador import Observer, Event
import jscatter as js
from scipy import stats
import numpy as np
import math  


class ObterResultado(Observer, threading.Thread):
    def __init__(self, **kwargs):
        Observer.__init__(self)
        self.path=kwargs.get('path')
        if not self.path:
            self.path='./'
        self.fileNome=kwargs.get('fileNome')
        if not self.fileNome:
            self.fileNome='entrada'

    def __call__(self, dados):
        pass

    def download(self):
        receber=Receber(nomeRef='28-3-2020e')
        receber()
        self.dados=receber.dados
    def plotDiagrama(self, **kargs):
        concentracao=kargs.get("concentracao")
        p=js.grace()
        p.yaxis(label='tc',charsize=1.50)
        p.xaxis(label='q',charsize=1.50)
        x=[]
        y=[]   
        for q in concentracao: 
            for dado in self.dados:
                if dado.get('p')== q:
                    y.append(dado.get('tc'))
                    x.append(q)
        p.plot(x,y,symbol=-1,line=[1,1,''],legend='Q=$q')
    
    def plotDiagramaFiltrado(self, **kargs):
        concentracao=kargs.get("concentracao")
        p=js.grace()
        p.yaxis(label='tc',charsize=1.50)
        p.xaxis(label='q',charsize=1.50)
        for q in concentracao: 
            x=[]
            y=[]   
            for dado in self.dados:
                if dado.get('p')== q:
                    y.append(dado.get('tc'))
                    x.append(q)
            zscore=stats.zscore(np.array(y))
            X=[]
            Y=[]
            for xx,yy,zz in zip(x, y, zscore.tolist()):
                if abs(zz) < 2.5:
                    Y.append(yy)
                    X.append(xx)
            p.plot(X,Y,symbol=-1,line=[1,1,''],legend='Q=$q')

    def plotDiagramaMedio(self, **kargs):
        concentracao=kargs.get("concentracao")
        p=js.grace()
        p.yaxis(label='tc',charsize=1.50)
        p.xaxis(label='q',charsize=1.50)
        xmedia=[]
        ymedia=[]
        yErro=[]
        for q in concentracao: 
            x=[]
            y=[]   
            for dado in self.dados:
                if dado.get('p')== q:
                    y.append(dado.get('tc'))
                    x.append(q)
            zscore=stats.zscore(np.array(y))
            X=[]
            Y=[]
            for xx,yy,zz in zip(x, y, zscore.tolist()):
                if abs(zz) < 2.5:
                    Y.append(yy)
                    X.append(xx)
            ymedia.append(np.average(Y))
            yErro.append(np.std(Y)/(math.sqrt(len(Y))))
            xmedia.append(np.average(X))
        p.plot(xmedia,ymedia,yErro,symbol=-1,line=[1,1,''],legend='Q=$q')

    def plot(self, x='t', y='sus', **kargs):
        p=js.grace()
        p.yaxis(label=y,charsize=1.50)
        p.xaxis(label=x,charsize=1.50)
        for dado in self.dados:
            p.plot(dado.get(x),dado.get(y),symbol=-1,line=[1,1,''],legend='Q=$q')

#----------------------- teste
import os
import unittest
class TesteResultado(unittest.TestCase):
    def test(self):
        concentracao =[0, 0.021, 0.025, 0.04, 0.05,  0.06, 0.07,  0.075, 0.08, 0.09,  0.1,  0.11, 0.12,0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.198, 0.2, 0.22]
        resultado=ObterResultado()
        resultado.download()
      #  resultado.plotDiagrama( concentracao=concentracao)
        resultado.plotDiagramaMedio( concentracao=concentracao)
        resultado.plot(y='sus')

        self.assertEqual(1, 1)
if __name__ == '__main__':
    unittest.main()

