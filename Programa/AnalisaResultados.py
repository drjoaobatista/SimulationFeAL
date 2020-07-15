import numpy as np
import threading
import os.path
import json
from Observador import Observer

class Resultados(Observer, threading.Thread):
    def __init__(self):
        Observer.__init__(self)

    def iDeCorte(x):
        '''
        devolve o I minimo para o qual a função é descrecente
        '''
        indice = len(x) - 1
        while True:
            if indice == 0:
                return 0
            deltaX = x[indice] - x[indice - 1]
            if deltaX >= 0:
                return indice
            indice -= 1

    def pico(x):
        '''
        Descrição:
            verifica se o maximo está no meio da curva
        '''
        x=np.array(x)
        i=x.argmax()
        return (i!=0) and  (i!=len(x))

    def encontraTc(**kwargs):
        '''
        Descrição:
            encontra o valor de tc
        '''
        t_=strToList(linha['t'])
        cumo_= strToList(linha['cumo'])
        sus_= strToList(linha['sus'])
        indice = len(t_) - 1
        icorte=iDeCorte(cumo_)
        i=pico(sus_[icorte:])
        if i!=-1:
            return t_[i]
        else:
            return  np.nan

    def plotarBonitas(listasX, listasY, listaControle, titulo, labelx, labely, nome):
        '''
        Plota todas as cuvas de um a lista
        '''
        graficos = GracePlot()
        s1 = Symbol(symbol=symbols.none, fillcolor=colors.red)
        l1 = Line(type=lines.solid)
        curvas=[]
        for i  in range(len(listasX)):
            icorte=iDeCorte(strToList(listaControle[i]))
            Ex=strToList(listasX[i])
            Ey=strToList(listasY[i])
            if pico(Ey[icorte:]):
                curva = Data(x=Ex[icorte:], y=Ey[icorte:], symbol=s1, line=l1)
                curvas.append(curva)
        grafico = graficos[0]
        grafico.plot(curvas)
        grafico.title(titulo)
        grafico.yaxis(label=Label(labely, font=5, charsize=1.5))
        grafico.xaxis(label=Label(labelx, font=5, charsize=1.5))
        graficos.save( './resultado/' + nome + '.agr')

    dados.head()
    Y=dados[dados.p==0.6].sus.values
    X=dados[dados.p==0.6].t.values
    Z=dados[dados.p==0.6].cumuE.values
    titulo, labelx, labely, nome='sus','t','sus','sus'
    plotarBonitas(X,Y,Z, titulo, labelx, labely, nome)
    plotarTodas(X,Y,Z, titulo, labelx, labely, nome)


    concentracao=[0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7]
    for p in concentracao:
        dados[dados.p==0].tc.mean()
        dados[dados.p==0].tc.std()


    dados['tc2'] = dados.apply(encontraTc, axis=1)
    dados[dados.p==0.7].tc.mean()
    dados[dados.p==0.7].tc.std()

    dados[dados.p==0.7].tc2.mean()
    dados[dados.p==0.7].tc2.std()
    dados.head(30)
    dados.apply(lambda entrada: grafico(entrada['t'], x['sus']), axis=1)

    def gracePlot(X, Y):
        '''
        Descrição:
        plotagem rapida xy
        '''
        p = GracePlot()  # A grace session opens
        s1 = Symbol(symbol=symbols.circle, fillcolor=colors.red)
        l1 = Line(type=lines.none)
        d1 = Data(x=X, y=Y, symbol=s1, line=l1)
        g = p[0]
        g.plot(d1)
        g.title('')
        g.yaxis(label=Label('Tc', font=2, charsize=1.5))
        g.xaxis(label=Label('p', font=5, charsize=1.5))
        p.save(diretorio + '/resultado/xy.agr')

    gracePlot(x1,y1)
    tsdf.transform({'A': np.abs, 'B': lambda x: x + 1})

    for x in vetor.iloc[:]
        print x

    dados.iloc[:,5:7].apply(np.mean, axis=1)
    dados.iloc[:,10:12].head(20)
    x = dados[dados['p']==0].iat[1,2]
    x

    b=dados[dados['p']==0.2].loc[:,['tc']]
    c=dados[dados['p']==0].loc[:,['t0']].to_numpy()
    b.plot()
    c.plot()
    df4.applymap(f)

