
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

#TODO: documentar todos Arquivos
#TODO: revisar os testes 
#TODO: contar quanto processos faltam 
#TODO: usar keywords
#FIXME: nada

import threading
from Observador import Observer, Event
class Apresentar(Observer, threading.Thread):
    def __init__(self):
        Observer.__init__(self) # Observer's init needs to be called
    def __call__(self, data):
        if data["tc"]:
            print('tc:', data["tc"],'q:', data["q"] )
        else:
            print('tudo:', data)

if __name__ == "__main__":
    import unittest
    class TesteSalvar(unittest.TestCase):
        def test(self):
            apresentar = Apresentar()
            apresentar.observe('Apresentar', apresentar)
            Event('Apresentar',  {"tc":1,"q":1})
            
    unittest.main()
    