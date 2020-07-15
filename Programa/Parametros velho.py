import json

class Parametros(object):
    def __init__(self, **kwargs):
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

    def configAmostra(self):
        chaves=['L', 'relaxacao','mcs','relaxacaoHistograma','mcsHistograma','A','numeroPontos','sementeAleatoria','raio']
        config={}
        for chave in chaves:
            config[chave] = self.entrada.get(chave)
        self.amostraSimulada+=1
        if self.amostraSimulada>=self.entrada["numeroAmostras"]:
            self.index += 1
            self.amostraSimulada=0
        config["p"]=(self.entrada['concentracao'])[self.index]
        config["t0"]=(self.entrada['t0'])[self.index]
        return config

    def __iter__(self):
        return self
        
    def __next__(self):
        try:
            result = self.configAmostra()
        except IndexError:
            raise StopIteration
        return result





#--------
if __name__ == "__main__":
    import unittest
    class TesteParametros(unittest.TestCase):
        def test(self):
            entrada={}
            entrada["numeroAmostras"]=5
            entrada['L']=5
            entrada['relaxacaoHistograma']=100000
            entrada['mcsHistograma']=100000
            entrada['tin']=6
            entrada['tfi']=7
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
            cont=1
            for parametro in parametros:
                cont+=1
                self.assertIn(parametro["p"], entrada['concentracao'])
            self.assertEqual(a*b, cont)

        def test2(self):
            entrada={}
            entrada["numeroAmostras"]=55
            entrada['L']=5
            entrada['relaxacao']=1000
            entrada['mcs']=1000
            entrada['relaxacaoHistograma']=100000
            entrada['mcsHistograma']=100000
            entrada['J1']=1
            entrada['J2']=0
            entrada['tin']=6
            entrada['tfi']=7
            entrada['numeroPontos']=50
            entrada['alpha']=50
            entrada['concentracao']=[0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7]
            entrada['aglomeradoLimit']=0
            entrada['sementeAleatoria']=1
            entrada['raio']=1
            parametros=Parametros(iterador='concentracao')
            a=len(parametros.entrada['concentracao'])
            b=parametros.entrada["numeroAmostras"]
            cont=1
            for parametro in parametros:
                cont+=1
                self.assertIn(parametro["p"], entrada['concentracao'])
            self.assertEqual(a*b, cont)
    unittest.main()