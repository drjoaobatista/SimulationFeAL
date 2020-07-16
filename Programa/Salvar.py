
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
""" Implementação de um programa de simulação de sistemas diluido génerico.
    Este módulo implementa a classe salva e recupera configurações do arquivo json. 
"""
 
__author__ = "João Batista dos Santos-Filho"
__copyright__ = "Copyright 2017, by Santos-Filho"
__credits__ = "Todos desenvolvedores de software livre"
__license__ = "GNU General Public License"
__version__ = "0.20.04"
__maintainer__ = "João Batista dos Santos-Filho"
__email__ = "dr@joaobatista.eng.br"
__status__ = "beta"

#TODO: documentar 
#TODO: revisar os testes 
#TODO: usar notas da simulacao para salvar no disco
#TODO: usar keywords

#FIXME: se processador estiver acima do limite o programa rodar 1 processo quando o processo finalizar ele vai travar
import threading
import os.path
import json

from Observador import Observer
class Salvar(Observer, threading.Thread):
    def __init__(self, **kwargs):
        Observer.__init__(self)

        self.fileNome=kwargs.get('fileNome')
        if not self.fileNome:
            self.fileNome='saidas.json'

        self.path=kwargs.get('path')
        if not self.path:
            self.path='./'

    def __call__(self, dados):
        if os.path.exists(self.fileNome):
            with open(self.fileNome, mode='a') as json_file:
                dadosJson=json.dumps(dados)
                json_file.write(dadosJson+'\n')
        else:
            with open(self.fileNome, mode='w') as json_file:
                dadosJson=json.dumps(dados)
                json_file.write(dadosJson+'\n')



#----------------------- teste
if __name__ == '__main__':
    import os
    import unittest
    from Observador import  Event
    class TesteSalvar(unittest.TestCase):
        def test(self):
            self.fileNome='teste.json'
            try:
                os.remove(self.fileNome)
            except:
                pass
            salvar = Salvar(fileNome=self.fileNome)
            salvar.observe('teste', salvar)
            for i in range(5):
                Event('teste',  {"t":i,"t2":i*i,"t3":[i,i*i]})
            with open(self.fileNome, mode='r') as json_file:
                for linha in json_file:
                    a=json.loads(linha)
                    self.assertEqual(a['t']*a['t'], a['t2'])
                    self.assertIn(a['t'], a['t3'])
                    self.assertIn(a['t2'], a['t3'])

        def test2(self):
            self.fileNome='teste.json'
            try:
                os.remove(self.fileNome)
            except:
                pass
            salvar = Salvar(fileNome=self.fileNome)
            salvar.observe('teste', salvar)
            Event('teste',  {"t":"testeOk"})
            with open(self.fileNome, mode='r') as json_file:
                for linha in json_file:
                    a=json.loads(linha)
                    self.assertEqual(a["t"],"testeOk")

    unittest.main()

