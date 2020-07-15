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

if __name__ == '__main__':
    unittest.main()

