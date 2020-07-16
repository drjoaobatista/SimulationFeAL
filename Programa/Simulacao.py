from Amostra import Amostra
from Observador import Event
from multiprocessing import Queue
import multiprocessing
import psutil
import time

class Simulacao(object):
    
    def __init__(self, parametros):
        self.cpu=multiprocessing.cpu_count() #psutil.cpu_count()
        self.parametros=parametros
    
    def __call__(self):
        fila=Queue()
        amostras=[]
        cont=0
        for parametro in self.parametros:
            amostra=Amostra(fila, parametro)
            amostras.append(amostra)
            amostra.start()
            time.sleep(1)
            cpu=psutil.cpu_percent()
            print("cpu %",cpu)
            cont+=1
            while ( cpu>=70) :
                print("... esperando")
                while fila.empty():
                    pass
                saida= fila.get()
                cont-=1
                Event('fimAmostra', saida)
                print(psutil.cpu_percent())

        while (cont >0):
            print("... esperando")
            while fila.empty():
                pass
            saida= fila.get()
            cont-=1
            Event('fimAmostra', saida)
        
#----------------------- teste

if __name__ == '__main__':
    import os
    import unittest
    from Enviar import Enviar
    from Salvar import Salvar
    from Apresentar import Apresentar
    from Parametros import Parametros
    class TesteSimualcao(unittest.TestCase):
        def test(self):
            self.fileNome="teste.json"
            self.enviar=Enviar()
            self.salvar=Salvar(fileNome=self.fileNome)
            self.apresentar=Apresentar()
            self.apresentar.observe('fimAmostra', self.apresentar)
            self.enviar.observe('fimAmostra', self.enviar)
            self.salvar.observe('fimAmostra', self.salvar)
            entrada={}
            entrada['tamanhos']=[10]
            entrada['relaxacaoHistograma']=10000
            entrada['mcsHistograma']=100000
            entrada['A']=1
            entrada['numeroPontos']=50
            entrada['concentracao']=[0, 0.1]
            entrada['t0']=[2.05, 1.9]
            entrada['numeroAmostras']=4
            entrada['raio']=0.2
            entrada['fileNome']=self.fileNome
            parametros=Parametros(entrada=entrada)
            simulacao=Simulacao(parametros)
            simulacao()
            self.assertEqual(1,1)

        def test2(self):
            fileNome="teste.json"
            enviar=Enviar()
            salvar=Salvar()
            apresentar=Apresentar()
            apresentar.observe('fimAmostra', apresentar)
            enviar.observe('fimAmostra', enviar)
            salvar.observe('fimAmostra', salvar)
            parametros=Parametros()
            simulacao=Simulacao(parametros)
            simulacao()
            self.assertEqual(1,1)
    unittest.main()
