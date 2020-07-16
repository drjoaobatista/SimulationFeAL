
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
""" Implementação de um programa de simulação de sistemas diluido génerico.
    Este módulo implementa um classe principal da simulação, ele execulta a simulação de uma amostras. 
"""
 
__author__ = "João Batista dos Santos-Filho"
__copyright__ = "Copyright 2017, by Santos-Filho"
__credits__ = "Todos desenvolvedores de software livre"
__license__ = "GNU General Public License"
__version__ = "0.20.04"
__maintainer__ = "João Batista dos Santos-Filho"
__email__ = "dr@joaobatista.eng.br"
__status__ = "beta"

#TODO: usar keywords
#FIXME: se processador estiver acima do limite o programa rodar 1 processo quando o processo finalizar ele vai travar
from multiprocessing import Process,Queue
import numpy as np
import geraHist
import analisaHist
import time

class Amostra(Process):
    def __init__(self, fila, entrada):
        Process.__init__(self)
        self.fila=fila
        self.entrada=entrada

    def preparaSaida(self):
        VectResultado = np.array(self.resultado)
        self.saida={}
        self.saida["t"]=VectResultado.transpose()[0] #melhorar isso
        self.saida["mag"]=np.nan_to_num(VectResultado.transpose()[1])
        self.saida["mag2"]=np.nan_to_num(VectResultado.transpose()[2])
        self.saida["logmag2"]=np.nan_to_num(VectResultado.transpose()[3])
        self.saida["energia"]=np.nan_to_num(VectResultado.transpose()[4])
        self.saida["calor"]=np.nan_to_num(VectResultado.transpose()[5])
        self.saida["sus"]=np.nan_to_num(VectResultado.transpose()[6])
        self.saida["cumu"]=np.nan_to_num(VectResultado.transpose()[7])
        self.saida["cumuE"]=np.nan_to_num(VectResultado.transpose()[8])
        self.saida["tc"]=(self.saida["t"])[self.saida["sus"].argmax()]
        self.saida["t"]=(self.saida["t"]).tolist()
        self.saida["mag"]=(self.saida["mag"]).tolist()
        self.saida["mag2"]=(self.saida["mag2"]).tolist()
        self.saida["logmag2"]=(self.saida["logmag2"]).tolist()
        self.saida["energia"]=(self.saida["energia"]).tolist()
        self.saida["calor"]=(self.saida["calor"]).tolist()
        self.saida["sus"]=(self.saida["sus"]).tolist()
        self.saida["cumu"]=(self.saida["cumu"]).tolist()
        self.saida["cumuE"]=(self.saida["cumuE"]).tolist()
        self.saida["duracao"]=time.time() - self.inicio
        self.saida.update(self.entrada)

    def simula(self):
        self.inicio = time.time()
        self.tin=max(self.entrada['t0'] - self.entrada["raio"], 0.01)
        self.tfi=self.entrada['t0'] + self.entrada["raio"]
        hist=geraHist.bcc(  self.entrada['relaxacaoHistograma'],
                            self.entrada['mcsHistograma'],
                            self.entrada['L'],
                            self.entrada['A'],
                            self.entrada['t0'],
                            self.entrada['q'],
                            self.entrada['clusterlimite'])
        self.resultado=analisaHist.histogram(hist,
                                       self.entrada['L'],
                                       self.entrada['t0'],
                                       self.tin,
                                       self.tfi,
                                       self.entrada['numeroPontos'])
        self.preparaSaida()
        return  self.saida
    def run(self):
        self.fila.put(self.simula())


#--------
if __name__ == "__main__":
    import jscatter as js
    import unittest
    from multiprocessing import Queue
    class TesteParametros(unittest.TestCase):
        def test(self):
            entrada={}
            entrada['L']=5
            entrada['relaxacaoHistograma']=100000
            entrada['mcsHistograma']=100000
            entrada['A']=1
            entrada['t0']=2.05
            entrada['numeroPontos']=50
            entrada['q']=0
            entrada['raio']=0.3
            entrada['clusterlimite']=50
            amostras=[]
            fila=Queue()
            for i in range(0,5):
                amostra=Amostra(fila,entrada)
                amostras.append(amostra)
                amostra.start()
            p=js.grace()
            p.yaxis(label='y',charsize=1.50)
            p.xaxis(label='x',charsize=1.50)
            for amostra in amostras:
                amostra.join()
                saida= amostra.fila.get()
                self.assertLessEqual(abs(saida['tc']-2.05), 0.2)
                self.assertEqual(len(saida['t']),entrada['numeroPontos'])
                p.plot(saida.get('t'),saida.get('sus'),symbol=-1,line=[1,1,''],legend='Q=$q')
    unittest.main()
