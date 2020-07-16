
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
""" Implementação de um programa de simulação de sistemas diluido génerico.
    Este módulo implementa a classe que envia os dados parar o firebase respondendo a eventos, ou chamando a rotina de enviar. 
"""
 
__author__ = "João Batista dos Santos-Filho"
__copyright__ = "Copyright 2017, by Santos-Filho"
__credits__ = "Todos desenvolvedores de software livre"
__license__ = "GNU General Public License"
__version__ = "0.20.04"
__maintainer__ = "João Batista dos Santos-Filho"
__email__ = "dr@joaobatista.eng.br"
__status__ = "beta"
#TODO: implemetar e testar a rotina de contador e soma dos TCs
#TODO: documentar
#TODO: revisar os testes 
#TODO: usar keywords

#FIXME: se processador estiver acima do limite o programa rodar 1 processo quando o processo finalizar ele vai travar
 
import threading
import json
import firebase_admin
from firebase_admin import credentials
from firebase_admin import db
from Observador import Observer, Event
from datetime import date

class Enviar(Observer, threading.Thread):

    def __init__(self, **kwargs):
        Observer.__init__(self)
        self.nomeRef=kwargs.get('nomeRef')
        dataAtual = date.today()
        if not self.nomeRef:
            self.nomeRef="Teste-{}".format(dataAtual)
        self.transaction=kwargs.get('transaction') #padrao false
        self.conectar()

    def fTransation(self, y):
            return lambda x: x + y if x else y
    
    def __call__(self, *args, **kwargs):
        nomeRef=kwargs.get('nomeRef')
        if nomeRef:
            self.setNomeSimulacao(nomeRef=nomeRef)

        if self.transaction:
            def fTransation(y):
                return lambda x: x + y if x else y
            for dados in args:
                f=self.fTransation(dados)
                self.ref.transaction(f)
            return
        else:
            for dados in args:
                dadosJson=json.dumps(dados)
                self.ref.push(dadosJson)

    def reset(self):
        self.ref.set('')

    def setNomeSimulacao(self, nomeRef):
        self.nomeRef=nomeRef
        self.ref = db.reference(self.nomeRef)

    def conectar(self):
        if (not len(firebase_admin._apps)):
            self.jb = {
                "type": "service_account",
                "project_id": "simulacao-d6cf2",
                "private_key_id": "12e7a68a7405dab308dec0318b2d7cd89e4f07ee",
                "private_key": "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDk/vnPjuRwp49G\njwjwKNRziL2abfVsoPzhyuhc/wi0H7hJibNqW8ZqdROtKJmQuVYDWgkVTioOu3o3\nqZQjuLh7Yh8hM6oqdvXfvlq7XD7NxeD8T1RxB4NuEi9YdrMf52debvbjzVtvdkG1\nnUdhZMM/2mXdlRTNFIQvcJXMWdq8vpDkQG1B/U0jFj2VBK75yxAdO8UfPyFdJw4l\nqhJ44dcvMqmvgGHz9OdZJASyont2gmrTo5QDxZTfj+L2LPsmUgoLipKLy5joa+gV\n+2WaBeUVTrFWZ+USAQ5etYwYaaxYqoIrBHzvlMoPnTXjdREzxWDH/WGWjVxmABZA\nh18Wxis/AgMBAAECggEABCvdA57bE70wacHkuHEQbB+74ugTBNa+W+iVCzsWiJ1w\nsHLbMXfARvJlthc0RxKTqxsdkLQ2Edbcz5NMffHvWXBVP4bZ+pHRBrGNVaLFCGll\njSCWMnWpxR7Lc3QysfZGakQmWWe7gijF9lexXeg7om4EX7S9xoenadge5/k9C6+h\n2oGaQwGEk4BmOIgBMm1TjSityKbfw+hLfM/k0Fli+59Pzuj5HKDt4tw4AtiMO6RD\n3ZMVhAS0KZ3ZVEjA4fy/aN2n9ay523pgOcHvi/7jG2KGBd7oJZ1doINs9xsrjItT\nI6ceOlB7+1K1qWjfzfl4cvYLB19VMI+u0orTejQIVQKBgQDzu4nUsCJB+hFVNN4b\nhsU4e3aPEmu6+ov77bkU9gQqLTX7yXUjO1qfm4nRYKP7jXz5JaPwpIp/8nanhRjx\nHVQ5t+8g3D+ju/nDp1gr71Ot5XBmtGhyUF/Wxrd07kHRRSE11WMSoN5as2+6Hrmc\no7a1m1RDGDGplgLDw7301dulGwKBgQDwhY8D/Tasnv25zJ3+qtrLQ3dl8p88Di+e\nOY8BZsfhVETNwMt2AAK1lDnWWt9dPNF1FEGIyt+xCSNjcouNu2kUoEe1sPNC6e+c\nqcFjRC7sYT52ffABOwZW+BdwsdvemgJLLSdGrwZfw4V7Nw3R/fLAB/GPi9kds1K6\nK5sd1zhIrQKBgFIlOub8Fg+2gHD/SHxuh7npoqPFyZlTKCrYjDCMWgr9AdzCoUAm\nmPyCJ4jx0VLO9Pfjv8UzL5q/1U3Qhf99qw1o+KdSlABTMnLGy+jC2KkbUy7KTwl4\nHVT91IcMZhJqAi9qo+F7ZnWbVGSeRmCMrN+4YkXauaI2dOgV6MfD7CtRAoGBAIc0\n/T1n1ZlaUxVkUhi8IPyMs4zqw9q1usz+XEaUaZ5gicEENRvjS6KZbZ3zPYcjTje7\nqE9mhDL4CR25YXvOM3b11vnrPASMjzcJMn5D8QDOdZeUmhuG+7xeAIdIZGoRN2Ld\nQiv47eNdfSbTvwAh+pk6ne9s1SNeEA45vxwaRTiNAoGBAM7coWAGx7P8ejH9eNdw\niKc952w55nuAs2ViSC5nllYpSWQT8wRQID6J8DnwUQr6gB1P8703ua9K8DHaK3fn\nnZZLa4Rjit8OYAkC3yZJmbxbcuBg0NfJ4Hti6Z0r6AXrYCywfQSiWevHT+WeCu19\n7w11XrpGYXnt78QTKV0vLqvR\n-----END PRIVATE KEY-----\n",
                "client_email": "firebase-adminsdk-oqqrd@simulacao-d6cf2.iam.gserviceaccount.com",
                "client_id": "117122346387157642604",
                "auth_uri": "https://accounts.google.com/o/oauth2/auth",
                "token_uri": "https://oauth2.googleapis.com/token",
                "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
                "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/firebase-adminsdk-oqqrd%40simulacao-d6cf2.iam.gserviceaccount.com"
                }
            cred = credentials.Certificate(self.jb)
            firebase_admin.initialize_app(cred, {'databaseURL': 'https://simulacao-d6cf2.firebaseio.com/'})
        self.ref = db.reference(self.nomeRef)


class Receber(Observer, threading.Thread):
    def __init__(self, **kwargs):
        Observer.__init__(self)
        self.nomeRef=kwargs.get('nomeRef')
        if not self.nomeRef:
            self.nomeRef="FeAl"
        self.conectar()

    def __call__(self):
        dadosBrutos= self.ref.get()
        if isinstance(dadosBrutos, dict):
            self.dados=[]
            for key,dadoJson in dadosBrutos.items():
                self.dados.append(json.loads(dadoJson))
        else:
            self.dados=dadosBrutos

    def setNomeSimulacao(self, nomeRef):
        self.nomeRef=nomeRef
        self.ref = db.reference(self.nomeRef)

    def conectar(self):
        if (not len(firebase_admin._apps)):
            self.jb = {
                "type": "service_account",
                "project_id": "simulacao-d6cf2",
                "private_key_id": "12e7a68a7405dab308dec0318b2d7cd89e4f07ee",
                "private_key": "-----BEGIN PRIVATE KEY-----\nMIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDk/vnPjuRwp49G\njwjwKNRziL2abfVsoPzhyuhc/wi0H7hJibNqW8ZqdROtKJmQuVYDWgkVTioOu3o3\nqZQjuLh7Yh8hM6oqdvXfvlq7XD7NxeD8T1RxB4NuEi9YdrMf52debvbjzVtvdkG1\nnUdhZMM/2mXdlRTNFIQvcJXMWdq8vpDkQG1B/U0jFj2VBK75yxAdO8UfPyFdJw4l\nqhJ44dcvMqmvgGHz9OdZJASyont2gmrTo5QDxZTfj+L2LPsmUgoLipKLy5joa+gV\n+2WaBeUVTrFWZ+USAQ5etYwYaaxYqoIrBHzvlMoPnTXjdREzxWDH/WGWjVxmABZA\nh18Wxis/AgMBAAECggEABCvdA57bE70wacHkuHEQbB+74ugTBNa+W+iVCzsWiJ1w\nsHLbMXfARvJlthc0RxKTqxsdkLQ2Edbcz5NMffHvWXBVP4bZ+pHRBrGNVaLFCGll\njSCWMnWpxR7Lc3QysfZGakQmWWe7gijF9lexXeg7om4EX7S9xoenadge5/k9C6+h\n2oGaQwGEk4BmOIgBMm1TjSityKbfw+hLfM/k0Fli+59Pzuj5HKDt4tw4AtiMO6RD\n3ZMVhAS0KZ3ZVEjA4fy/aN2n9ay523pgOcHvi/7jG2KGBd7oJZ1doINs9xsrjItT\nI6ceOlB7+1K1qWjfzfl4cvYLB19VMI+u0orTejQIVQKBgQDzu4nUsCJB+hFVNN4b\nhsU4e3aPEmu6+ov77bkU9gQqLTX7yXUjO1qfm4nRYKP7jXz5JaPwpIp/8nanhRjx\nHVQ5t+8g3D+ju/nDp1gr71Ot5XBmtGhyUF/Wxrd07kHRRSE11WMSoN5as2+6Hrmc\no7a1m1RDGDGplgLDw7301dulGwKBgQDwhY8D/Tasnv25zJ3+qtrLQ3dl8p88Di+e\nOY8BZsfhVETNwMt2AAK1lDnWWt9dPNF1FEGIyt+xCSNjcouNu2kUoEe1sPNC6e+c\nqcFjRC7sYT52ffABOwZW+BdwsdvemgJLLSdGrwZfw4V7Nw3R/fLAB/GPi9kds1K6\nK5sd1zhIrQKBgFIlOub8Fg+2gHD/SHxuh7npoqPFyZlTKCrYjDCMWgr9AdzCoUAm\nmPyCJ4jx0VLO9Pfjv8UzL5q/1U3Qhf99qw1o+KdSlABTMnLGy+jC2KkbUy7KTwl4\nHVT91IcMZhJqAi9qo+F7ZnWbVGSeRmCMrN+4YkXauaI2dOgV6MfD7CtRAoGBAIc0\n/T1n1ZlaUxVkUhi8IPyMs4zqw9q1usz+XEaUaZ5gicEENRvjS6KZbZ3zPYcjTje7\nqE9mhDL4CR25YXvOM3b11vnrPASMjzcJMn5D8QDOdZeUmhuG+7xeAIdIZGoRN2Ld\nQiv47eNdfSbTvwAh+pk6ne9s1SNeEA45vxwaRTiNAoGBAM7coWAGx7P8ejH9eNdw\niKc952w55nuAs2ViSC5nllYpSWQT8wRQID6J8DnwUQr6gB1P8703ua9K8DHaK3fn\nnZZLa4Rjit8OYAkC3yZJmbxbcuBg0NfJ4Hti6Z0r6AXrYCywfQSiWevHT+WeCu19\n7w11XrpGYXnt78QTKV0vLqvR\n-----END PRIVATE KEY-----\n",
                "client_email": "firebase-adminsdk-oqqrd@simulacao-d6cf2.iam.gserviceaccount.com",
                "client_id": "117122346387157642604",
                "auth_uri": "https://accounts.google.com/o/oauth2/auth",
                "token_uri": "https://oauth2.googleapis.com/token",
                "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
                "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/firebase-adminsdk-oqqrd%40simulacao-d6cf2.iam.gserviceaccount.com"
                }
            cred = credentials.Certificate(self.jb)
            firebase_admin.initialize_app(cred, {'databaseURL': 'https://simulacao-d6cf2.firebaseio.com/'})
        self.ref = db.reference(self.nomeRef)





#----------------------- teste
if __name__ == '__main__':
    import os
    import unittest
    class TesteEnviarReceber(unittest.TestCase):
        def test1(self):
            enviar = Enviar()
            enviar.setNomeSimulacao("teste")
            enviar.reset()
            enviar.observe('enviar', enviar)
            Event('enviar',  'teste')
            Event('enviar',  [[0,1,2],"casa"])
            receber=Receber()
            receber.setNomeSimulacao("teste")
            receber()
            self.assertIn('teste', receber.dados)
            self.assertIn([[0,1,2],"casa"], receber.dados)

        def test2(self):
            enviar = Enviar(transaction=True)
            enviar.setNomeSimulacao("teste2")
            enviar.reset()
            enviar.observe('enviar2', enviar)
            Event('enviar2',  1)
            Event('enviar2',  2.5)
            receber=Receber()
            receber.setNomeSimulacao("teste2")
            receber()
            print(receber.dados)
            self.assertEqual(3, 3)
    unittest.main()

