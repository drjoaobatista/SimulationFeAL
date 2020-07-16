
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
""" Implementação de um programa de simulação de sistemas diluido génerico.
    Este módulo implementa a classe principal da simulação, gera amostras que são exexultadas em diferentes
    processos, gera eventos passando os resultados das amostras finalizadas, monitora os processadores livres para inicar as amostras. 
"""
 
__author__ = "João Batista dos Santos-Filho"
__copyright__ = "Copyright 2017, by Santos-Filho"
__credits__ = "Todos desenvolvedores de software livre"
__license__ = "GNU General Public License"
__version__ = "0.20.04"
__maintainer__ = "João Batista dos Santos-Filho"
__email__ = "dr@joaobatista.eng.br"
__status__ = "beta"

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

from Amostra import Amostra
from Observador import Event
from multiprocessing import Queue
import multiprocessing
import psutil
import time

class Simulacao(object):
    def __init__(self, **kwargs):
        """ Entradas opcionais: numeroMaxCpuOcupacadas, cpuMaximaOcupacao """
        self.parametros=kwargs.get('parametros')
        if not self.parametros:
            print("erro parâmetros de configuração está ausente")
        self.cpuMaximaOcupacao=kwargs.get('cpuMaximaOcupacao')
        if not self.cpuMaximaOcupacao:
            self.cpuMaximaOcupacao=90
        self.numerocpu=psutil.cpu_count()
        self.numeroMaxCpuOcupacadas=kwargs.get('numeroMaxCpuOcupacadas')
        if not self.numeroMaxCpuOcupacadas:
            self.numeroMaxCpuOcupacadas=self.numerocpu-2
        self.timeSleep=kwargs.get('timeSleep')
        if not self.timeSleep:
            self.timeSleep=1
    
    def __call__(self):
        fila=Queue()
        amostras=[]
        processoIniciado=0
        processoFinalizado=0
        for parametro in self.parametros:
            amostra=Amostra(fila, parametro)
            amostras.append(amostra)
            amostra.start()
            processoIniciado+=1
            print("Iniciado {} processos, finalizado {} processos,  esperando {} processos".format(processoIniciado, processoFinalizado,processoIniciado-processoFinalizado))
            time.sleep(self.timeSleep)
            Vetorcpu=psutil.cpu_percent(interval=1, percpu=True)
            numeroCpuOcupadas=len([1 for x in Vetorcpu if x >self.cpuMaximaOcupacao])
            print(" {} cpu ocupada acima de {}%".format(numeroCpuOcupadas,self.cpuMaximaOcupacao ))
            temp=numeroCpuOcupadas
            while (numeroCpuOcupadas>=self.numeroMaxCpuOcupacadas):
                if numeroCpuOcupadas != temp:
                    print(" {} cpu ocupada acima de {}%".format(numeroCpuOcupadas,self.cpuMaximaOcupacao ))
                    temp=numeroCpuOcupadas
                else:
                    print("|", end="") #FIXME
                if not fila.empty():
                    saida= fila.get()
                    Event('fimAmostra', saida)
                    processoFinalizado+=1
                time.sleep(self.timeSleep)
                Vetorcpu=psutil.cpu_percent(interval=1, percpu=True)
                numeroCpuOcupadas=len([1 for x in Vetorcpu if x >self.cpuMaximaOcupacao])
                  
        while (processoIniciado-processoFinalizado>0):
            print("esperando {} processos,  finalizado {} processos,  ".format(processoIniciado-processoFinalizado, processoFinalizado))
            if not fila.empty():
                saida= fila.get()
                Event('fimAmostra', saida)
                processoFinalizado+=1
            time.sleep(self.timeSleep)
                
        
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
            entrada['clusterlimite']=50
            entrada['fileNome']=self.fileNome
            parametros=Parametros(entrada=entrada)
            simulacao=Simulacao(parametros=parametros)
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
            simulacao=Simulacao(parametros=parametros)
            simulacao()
            self.assertEqual(1,1)
    unittest.main()
