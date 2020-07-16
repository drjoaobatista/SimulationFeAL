import json
import threading
from Observador import Observer
class Parametros(Observer, threading.Thread):
    def __init__(self, **kwargs): 
        Observer.__init__(self)
        self.iterador=kwargs.get('iterador')
        if not self.iterador:
            self.iterador="concentracao"
        self.entrada= kwargs.get('entrada')
        self.index = 0
        self.amostraSimulada=0
        self.fileNome='config.json'
        if self.entrada:
            self.salvarJsonDisco()
        if not self.entrada:
            try:
                self.lerJsonDisco()
            except:
                print('erro na leitura dos parametros')
                self.salvarJsonDisco()
    
    def update(self,  kwargs): #para colocar o ** precisa atualizar muitos
        self.newtc=kwargs.get('tc')
        self.pnew=kwargs.get('q')
        i=(self.entrada['concentracao']).index(self.pnew)
        (self.entrada['t0'])[i]=self.newtc
    
    def salvarJsonDisco(self):
        self.parametrosJson=json.dumps(self.entrada, indent=4, sort_keys=True)
        arquivo = open('config.json','w')
        arquivo.write(self.parametrosJson)
        arquivo.close()

    def lerJsonDisco(self):
        with open(self.fileNome, mode='r') as json_file:
             self.entrada=json.loads(json_file.read())

    def lerParametrosFiribase(self):
        pass


    def __iter__(self):
        chaves=['relaxacaoHistograma','mcsHistograma','A','numeroPontos','raio','mcsTroca']
        config={}
        for chave in chaves:
            config[chave] = self.entrada.get(chave)
        for L in self.entrada["tamanhos"]:
            for self.amostraSimulada in range(self.entrada["numeroAmostras"]):
                for t0, q in zip(self.entrada['t0'], self.entrada['concentracao']):
                    config["L"]=L
                    config["q"]=q
                    config["t0"]=t0
                    yield config





#--------
if __name__ == "__main__":
    import unittest
    from Observador import  Event
    class TesteParametros(unittest.TestCase):
        def test(self):#testando atualizacao de dados por eventos 
            entrada={}
            entrada["numeroAmostras"]=5
            entrada['tamanhos']=[5]
            entrada['relaxacaoHistograma']=100000
            entrada['mcsHistograma']=100000
            entrada['numeroPontos']=50
            entrada['concentracao']=[0, 0.1]
            entrada['t0']=[2.09, 1.8]
            entrada['A']=1
            entrada['raio']=1
            parametros=Parametros(entrada=entrada)
            parametros.observe('teste', parametros.update)
            dados={}
            dados['q']=0.1
            dados['tc']=2
            Event('teste', dados)
            self.assertEqual(2, (parametros.entrada['t0'])[(parametros.entrada['concentracao']).index(0.1)])
           
        def test1(self):
            entrada={}
            entrada["numeroAmostras"]=5
            entrada['tamanhos']=[5]
            entrada['relaxacaoHistograma']=100000
            entrada['mcsHistograma']=100000
            entrada['numeroPontos']=50
            entrada['concentracao']=[0, 0.1]
            entrada['t0']=[2.09, 1.8]
            entrada['A']=1
            entrada['sementeAleatoria']=1
            entrada['raio']=1
            parametros=Parametros(entrada=entrada)
            parametros.salvarJsonDisco()
            a=len(entrada['concentracao'])
            b=entrada["numeroAmostras"]
            cont=0
            for parametro in parametros:
                cont+=1
                self.assertIn(parametro["q"], entrada['concentracao'])
            self.assertEqual(a*b, cont)

        def test2(self):
            entrada={}
            entrada["numeroAmostras"]=55
            entrada['tamanhos']=[5]
            entrada['relaxacaoHistograma']=100000
            entrada['mcsHistograma']=100000

            entrada['numeroPontos']=50
            entrada['concentracao']=[0, 0.05]
            entrada['t0']=[2.09, 1.8]
            entrada['raio']=1
            parametros=Parametros(entrada=entrada)
            a=len(parametros.entrada['concentracao'])
            b=parametros.entrada["numeroAmostras"]
            cont=0
            for parametro in parametros:
                cont+=1
                self.assertIn(parametro["q"], entrada['concentracao'])
            self.assertEqual(a*b, cont)
    unittest.main()
