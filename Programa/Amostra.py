
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
import vt
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
        self.saida["t"]=VectResultado.transpose()[0].tolist() #melhorar isso
        self.saida["sus"]=np.nan_to_num(VectResultado.transpose()[1]).tolist()
        self.saida["calor"]=np.nan_to_num(VectResultado.transpose()[2]).tolist()
        self.saida["cumu"]=np.nan_to_num(VectResultado.transpose()[3]).tolist()
        self.saida["mag"]=np.nan_to_num(VectResultado.transpose()[4]).tolist()
        self.saida["energia"]=np.nan_to_num(VectResultado.transpose()[5]).tolist()
        self.saida["ordemB"]=np.nan_to_num(VectResultado.transpose()[6]).tolist()
        self.saida["susOrdemB"]=np.nan_to_num(VectResultado.transpose()[7]).tolist()
        self.saida["duracao"]=time.time() - self.inicio
        self.saida["tc"]=float(self.tc)
        self.saida.update(self.entrada)

    def encontraTc(self,t='n', sus=None, cumo=None):
        if isinstance(t, str):
            VectResultado = np.array(self.resultado)
            t = VectResultado.transpose()[0]
            sus = np.nan_to_num(VectResultado.transpose()[1])
            self.tc = t[sus.argmax()]
        else:
            self.tc = t[sus.argmax()]

    def simula(self):
        self.inicio = time.time()
        self.resultado, self.cofiguracao = vt.vtbcc( self.entrada['L'],
                                       self.entrada['relaxacao'],
                                       self.entrada['mcs'],
                                       self.entrada['mcsTroca'],
                                       self.entrada['A'],
                                       self.entrada['B'],
                                       self.entrada['q'],
                                       self.entrada['tInicio'],
                                       self.entrada['tFinal'],
                                       self.entrada['numeroPontos'])
        self.encontraTc()
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
            entrada['relaxacao']=5000
            entrada['mcs']=10000
            entrada['mcsTroca']=100
            entrada['A']=1.7
            entrada['B']=-1
            entrada['tInicio']=10
            entrada['tFinal']=0.1
            entrada['numeroPontos']=30
            entrada['q']=0.2
            amostras=[]
            fila=Queue()
            for i in range(0,5):
                amostra=Amostra(fila,entrada)
                amostras.append(amostra)
                amostra.start()
            p=js.grace()
            p2=js.grace()
            p3=js.grace()
            p.yaxis(label='sus',charsize=1.50)
            p.xaxis(label='x',charsize=1.50)
            p2.yaxis(label='calor',charsize=1.50)
            p2.xaxis(label='x',charsize=1.50)
            p3.yaxis(label='ordem',charsize=1.50)
            p3.xaxis(label='x',charsize=1.50)
            for amostra in amostras:
                amostra.join()
                saida= amostra.fila.get()
                #print(saida.get('t'),saida.get('sus'))
                p.plot(saida.get('t'),saida.get('sus'),symbol=-1,line=[1,1,''],legend='Q=$q')
                p2.plot(saida.get('t'),saida.get('calor'),symbol=-1,line=[1,1,''],legend='Q=$q')
                p3.plot(saida.get('t'),saida.get('ordemB'),symbol=-1,line=[1,1,''],legend='Q=$q')
                p3.plot(saida.get('t'),saida.get('susOrdemB'),symbol=-1,line=[1,1,''],legend='Q=$q')
                #self.assertLessEqual(abs(saida['tc']-2.05), 0.2)
                #self.assertEqual(len(saida['t']),entrada['numeroPontos'])
               
    unittest.main()
