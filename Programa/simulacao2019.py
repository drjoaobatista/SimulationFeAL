#!/usr/bin/env python 
#*-* coding: utf-8 *-*
# na v2 o programa roda a simulação num largo espectro encontra o tc depois roda o histograma e encontra o tc mais preciso.
# para v3  fazer o processo de encontrar tc com o histrograma ser "interativo" para diferentes amostras. ok
# para v4  salvar valores para diferentes amostras e fazer um histograma das soluções
# para v4.1   - estimativa de tempo
# para v4.2   - melhoria do codigo
# para v4.3   -  gravar dados no disco e continuar de onde parou somente em etapas
# para v4.4 - continuar onde parou amostras colocar simulação em uma pasta
# para v4.5   plotar no grace com diferentes p
# para v4.6   fitar a gausiana
# para v4.7    encontrar o tc do calor específico
# para v5  fitar esse histograma encontrar o tc com maxima precisão
# para v5.? continuar de onde parar
# para v5.1.2 aglomerado limit
# para v5.2 melhor os graficos grace

# para v6 usar Python's multiprocessing
# foi necessar modificar as funcioes para utilizar parametros de entrada e saida
#  v6.1modificar os graficos do ajuste gausiano e salvar os dados do ajuste no caption
# v6.02 funcionando o multprocesos
# Salvar uma arquivo de log
# v6.1 rodar no intel pyton compilar os fortrans com o compilador intel
#List of available Fortran compilers:
#  --fcompiler=gnu    GNU Fortran Compiler (3.3.4)
#  --fcompiler=intel  Intel Fortran Compiler for 32-bit apps (8.0)

# para v7  fazer envontrar o tc para outros tamanhos de redes
# para v71  enjcontrar o tc do cruzamento dos cumulantes de diferentes amostras e fazer o histograma
# para v7.2 criar pacote
# para v7 fitar o histograma para todos os valores encontrados e calcular os expoentes.
# para v8 criar um ambiente gráfico http://www.sourcecode.net.br/2011/11/criando-interfaces-graficas-com-glade-e.html
# a interface pode ser o dialog
# para v11 trabalar distribuido em cluster
# para v12 trabalhar com outros modelos
# usar o firebase
# o programa pyton deve gerencias as amostras e os estados iniciais
#rodar no win

# importa bibliotecas
import ising
import Histogram
import numpy as np
import matplotlib.pyplot as plt
import itertools
from itertools import izip_longest
import pickle #salvar dados na forma de texto
import time
import datetime
from datetime import timedelta
import os #para criar pastas
from GracePlot import *
from scipy.stats import norm
import math
from multiprocessing import Pool
import time
import sys

#=======================================================================================
# subrotinas graficas
#=======================================================================================
def plotarMediaQuantidades(Amostras):
	s1=Symbol(symbol=symbols.none,fillcolor=colors.red)
	l1=Line(type=lines.solid)
	pMag = GracePlot()
	pMag2= GracePlot()
	pLogmag2= GracePlot()
	pEnergia= GracePlot()
	pCalor= GracePlot()
	pSus= GracePlot()
	pCumu= GracePlot()
	pCumuE= GracePlot()
	vMag=[]
	vMag2=[]
	vLogmag2=[]
	vEnergia=[]
	vCalor=[]
	vSus=[]
	vCumu=[]
	vCumuE=[]
	for p in concentracao:
		t=dadosMag=[x[0] for x in amostras if x[9]==p]
		mag=dadosMag=[x[1] for x in amostras if x[9]==p]
		mag2=dadosMag=[x[2] for x in amostras if x[9]==p]
		logmag2=dadosMag=[x[3] for x in amostras if x[9]==p]
		energia=dadosMag=[x[4] for x in amostras if x[9]==p]
		calor=dadosMag=[x[5] for x in amostras if x[9]==p]
		sus=dadosMag=[x[6] for x in amostras if x[9]==p]
		cumu=dadosMag=[x[7] for x in amostras if x[9]==p]
		cumuE=dadosMag=[x[8] for x in amostras if x[9]==p]
		t=sum(t)/len(t)
		mag=sum(mag)/len(mag)
		mag2=sum(mag2)/len(mag2)
		logmag2=sum(logmag2)/len(logmag2)
		energia=sum(energia)/len(energia)
		calor=sum(calor)/len(calor)
		sus=sum(sus)/len(sus)
		cumu=sum(cumu)/len(cumu)
		cumuE=sum(cumuE)/len(cumuE)
		dMag=Data(x=t,y=mag,symbol=s1,line=l1)
		dMag2=Data(x=t,y=mag2,symbol=s1,line=l1)
		dLogmag2=Data(x=t,y=logmag2,symbol=s1,line=l1)
		dEnergia=Data(x=t,y=energia,symbol=s1,line=l1)
		dCalor=Data(x=t,y=calor,symbol=s1,line=l1)
		dSus=Data(x=t,y=sus,symbol=s1,line=l1)
		dCumu=Data(x=t,y=cumu,symbol=s1,line=l1)
		dCumuE=Data(x=t,y=cumuE,symbol=s1,line=l1)
		vMag.append(dMag)
		vMag2.append(dMag2)
		vLogmag2.append(dLogmag2)
		vEnergia.append(dEnergia)
		vCalor.append(dCalor)
		vSus.append(dSus)
		vCumu.append(dCumu)
		vCumuE.append(dCumuE)
	gMag=pMag[0]
	gMag.plot(vMag)
	gMag.title('Magnetizacao x Temperatura')
	gMag.yaxis(label=Label('magnetizacao',font=5,charsize=1.5))
	gMag.xaxis(label=Label('T',font=5,charsize=1.5))
	pMag.save(dir+'/resultado/mag.agr')

	gMag2=pMag2[0]
	gMag2.plot(vMag2)
	gMag2.title('magnetizacao2 x Temperatura')
	gMag2.yaxis(label=Label('mag2',font=5,charsize=1.5))
	gMag2.xaxis(label=Label('T',font=5,charsize=1.5))
	pMag2.save(dir+'/resultado/mag2.agr')

	gLogmag2=pLogmag2[0]
	gLogmag2.plot(vLogmag2)
	gLogmag2.title('logmag2 x Temperatura')
	gLogmag2.yaxis(label=Label('logmag2',font=5,charsize=1.5))
	gLogmag2.xaxis(label=Label('T',font=5,charsize=1.5))
	pLogmag2.save(dir+'/resultado/logmag2.agr')

	gEnergia=pEnergia[0]
	gEnergia.plot(vEnergia)
	gEnergia.title('energia x Temperatura')
	gEnergia.yaxis(label=Label('energia',font=5,charsize=1.5))
	gEnergia.xaxis(label=Label('T',font=5,charsize=1.5))
	pEnergia.save(dir+'/resultado/energia.agr')

	gCalor=pCalor[0]
	gCalor.plot(vCalor)
	gCalor.title('Calor especifico x Temperatura')
	gCalor.yaxis(label=Label('Calor',font=2,charsize=1.5))
	gCalor.xaxis(label=Label('T',font=5,charsize=1.5))
	pCalor.save(dir+'/resultado/calor.agr')

	gSus=pSus[0]
	gSus.plot(vSus)
	gSus.title('Susceptibilidade x Temperatura')
	gSus.yaxis(label=Label('Susceptibilidade',font=5,charsize=1.5))
	gSus.xaxis(label=Label('T',font=5,charsize=1.5))
	pSus.save(dir+'/resultado/sus.agr')

	gCumu=pCumu[0]
	gCumu.plot(vCumu)
	gCumu.title('cumulante da magnetizacao x Temperatura')
	gCumu.yaxis(label=Label('cumulante',font=5,charsize=1.5))
	gCumu.xaxis(label=Label('T',font=5,charsize=1.5))
	pCumu.save(dir+'/resultado/cumu.agr')

	gCumuE=pCumuE[0]
	gCumuE.plot(vCumuE)
	gCumuE.title('cumulante da energia x Temperatura')
	gCumuE.yaxis(label=Label('cumulante',font=5,charsize=1.5))
	gCumuE.xaxis(label=Label('T',font=5,charsize=1.5))
	pCumuE.save(dir+'/resultado/cumue.agr')
	print "plot da quantidades finalizado "


def plotarQuantidades(Amostras):
	s1=Symbol(symbol=symbols.none,fillcolor=colors.red)
	l1=Line(type=lines.solid)
	pMag = GracePlot()
	pMag2= GracePlot()
	pLogmag2= GracePlot()
	pEnergia= GracePlot()
	pCalor= GracePlot()
	pSus= GracePlot()
	pCumu= GracePlot()
	pCumuE= GracePlot()
	vMag=[]
	vMag2=[]
	vLogmag2=[]
	vEnergia=[]
	vCalor=[]
	vSus=[]
	vCumu=[]
	vCumuE=[]
	mag=[]
	magMedia=np.array(mag, float )
	magMedia += np.array()
	for resultado in Amostras:
		t,mag,mag2,logmag2,energia,calor, sus, cumu,cumuE, pp =  resultado
		dMag=Data(x=t,y=mag,symbol=s1,line=l1)
		dMag2=Data(x=t,y=mag2,symbol=s1,line=l1)
		dLogmag2=Data(x=t,y=logmag2,symbol=s1,line=l1)
		dEnergia=Data(x=t,y=energia,symbol=s1,line=l1)
		dCalor=Data(x=t,y=calor,symbol=s1,line=l1)
		dSus=Data(x=t,y=sus,symbol=s1,line=l1)
		dCumu=Data(x=t,y=cumu,symbol=s1,line=l1)
		dCumuE=Data(x=t,y=cumuE,symbol=s1,line=l1)
		vMag.append(dMag)
		vMag2.append(dMag2)
		vLogmag2.append(dLogmag2)
		vEnergia.append(dEnergia)
		vCalor.append(dCalor)
		vSus.append(dSus)
		vCumu.append(dCumu)
		vCumuE.append(dCumuE)

	gMag=pMag[0]
	gMag.plot(vMag)
	gMag.title('Magnetizacao x Temperatura')
	gMag.yaxis(label=Label('magnetizacao',font=5,charsize=1.5))
	gMag.xaxis(label=Label('T',font=5,charsize=1.5))
	pMag.save(dir+'/resultado/mag.agr')

	gMag2=pMag2[0]
	gMag2.plot(vMag2)
	gMag2.title('magnetizacao2 x Temperatura')
	gMag2.yaxis(label=Label('mag2',font=5,charsize=1.5))
	gMag2.xaxis(label=Label('T',font=5,charsize=1.5))
	pMag2.save(dir+'/resultado/mag2.agr')

	gLogmag2=pLogmag2[0]
	gLogmag2.plot(vLogmag2)
	gLogmag2.title('logmag2 x Temperatura')
	gLogmag2.yaxis(label=Label('logmag2',font=5,charsize=1.5))
	gLogmag2.xaxis(label=Label('T',font=5,charsize=1.5))
	pLogmag2.save(dir+'/resultado/logmag2.agr')

	gEnergia=pEnergia[0]
	gEnergia.plot(vEnergia)
	gEnergia.title('energia x Temperatura')
	gEnergia.yaxis(label=Label('energia',font=5,charsize=1.5))
	gEnergia.xaxis(label=Label('T',font=5,charsize=1.5))
	pEnergia.save(dir+'/resultado/energia.agr')

	gCalor=pCalor[0]
	gCalor.plot(vCalor)
	gCalor.title('Calor especifico x Temperatura')
	gCalor.yaxis(label=Label('Calor',font=2,charsize=1.5))
	gCalor.xaxis(label=Label('T',font=5,charsize=1.5))
	pCalor.save(dir+'/resultado/calor.agr')

	gSus=pSus[0]
	gSus.plot(vSus)
	gSus.title('Susceptibilidade x Temperatura')
	gSus.yaxis(label=Label('Susceptibilidade',font=5,charsize=1.5))
	gSus.xaxis(label=Label('T',font=5,charsize=1.5))
	pSus.save(dir+'/resultado/sus.agr')

	gCumu=pCumu[0]
	gCumu.plot(vCumu)
	gCumu.title('cumulante da magnetizacao x Temperatura')
	gCumu.yaxis(label=Label('cumulante',font=5,charsize=1.5))
	gCumu.xaxis(label=Label('T',font=5,charsize=1.5))
	pCumu.save(dir+'/resultado/cumu.agr')

	gCumuE=pCumuE[0]
	gCumuE.plot(vCumuE)
	gCumuE.title('cumulante da energia x Temperatura')
	gCumuE.yaxis(label=Label('cumulante',font=5,charsize=1.5))
	gCumuE.xaxis(label=Label('T',font=5,charsize=1.5))
	pCumuE.save(dir+'/resultado/cumue.agr')
	print "plot da quantidades finalizado "


def graceDiagrama(diagrama):
	p = GracePlot() # A grace session opens
	s1=Symbol(symbol=symbols.circle,fillcolor=colors.red)
	l1=Line(type=lines.none)
	a,b=([x[0] for x in diagrama], [x[1] for x in diagrama])
	d1=Data(x=a,y=b,symbol=s1,line=l1)
	g=p[0]
	g.plot(d1)
	#g.text('test',.51,.51,color=2)
	g.title('Diagrama')
	g.yaxis(label=Label('Tc',font=2,charsize=1.5))
	g.xaxis(label=Label('p',font=5,charsize=1.5))
	p.save(dir+'/resultado/diagrama.agr')

def graceDiagramaNormalizado(diagrama):
	p = GracePlot() # A grace session opens
	s1=Symbol(symbol=symbols.circle,fillcolor=colors.red)
	l1=Line(type=lines.none)
	c=max([x[1] for x in diagrama])
	a,b=([x[0] for x in diagrama], [x[1]/c for x in diagrama])
	d1=Data(x=a,y=b,symbol=s1,line=l1)
	g=p[0]
	g.plot(d1)
	#g.text('test',.51,.51,color=2)
	g.title('Diagrama normalizado')
	g.yaxis(label=Label('Tc',font=2,charsize=1.5))
	g.xaxis(label=Label('p',font=5,charsize=1.5))
	p.save(dir+'/resultado/diagramaN.agr')

def plotarMaximos(amostras):
	print 'iniciando plot amostras '
	pSus = GracePlot()
	pCalor = GracePlot()
	s1=Symbol(symbol=symbols.circle)
	l1=Line(type=lines.none)
	histogramaTcSus=[]
	histogramaSus=[]
	histogramaTcCalor=[]
	histogramaCalor=[]
	for resultado in amostras:
		t,mag,mag2,logmag2,energia,calor, su, cumo,cumuE, p =  resultado
		histogramaTcSus.append((p,t[su.argmax()]))
		histogramaSus.append((p,su.max()))
		histogramaTcCalor.append((p,t[calor.argmax()]))
		histogramaCalor.append((p,calor.max()))
	graficosSus=[]
	graficosCalor=[]
	for p in concentracao:
		dadosTcSus=[x[1] for x in histogramaTcSus if x[0]==p]
		dadosSus=[x[1] for x in histogramaSus if x[0]==p]
		dadosTcCalor=[x[1] for x in histogramaTcCalor if x[0]==p]
		dadosCalor=[x[1] for x in histogramaCalor if x[0]==p]
		dSus=Data(x=dadosTcSus, y=dadosSus,symbol=s1,line=l1)
		graficosSus.append(dSus)
		dCalor=Data(x=dadosTcCalor, y=dadosCalor,symbol=s1,line=l1)
		graficosCalor.append(dCalor)

	gSus=pSus[0]
	gSus.plot(graficosSus)
	gSus.title('distribuicao susceptibilidade maxima')
	gSus.yaxis(label=Label('susceptibilidade',font=2,charsize=1.5))
	gSus.xaxis(label=Label('Tc',font=5,charsize=1.5))
	pSus.save(dir+'/resultado/distribuicaoSus.agr')

	gCalor=pCalor[0]
	gCalor.plot(graficosCalor)
	gCalor.title('Distribuicao calor máximo')
	gCalor.yaxis(label=Label('calor',font=2,charsize=1.5))
	gCalor.xaxis(label=Label('tc',font=5,charsize=1.5))
	pCalor.save(dir+'/resultado/distribuicaoCalor.agr')
	print 'finalizando plot dos máximos '


def graceHistogramaTc(nAmostras):
	plo = GracePlot() # A grace session opens
	s1=Symbol(symbol=symbols.none,fillcolor=3)
	l1=Line(type=lines.none)
	l2=Line(type=lines.solid)
	histogramaTc=[]
	histogramaSu=[]
	for resultado in nAmostras:
		t,mag,mag2,logmag2,energia,calor, su, cumo,cumuE, p =  resultado
		histogramaTc.append((p,t[su.argmax()]))
		histogramaSu.append((p,su.max()))
	graficos=[]
	for p in concentracao:
		dados=[x[1] for x in histogramaTc if x[0]==p]
		hists, bin_edges = np.histogram(dados, 10)
		d1=DataBar(x=bin_edges[:-1],y=hists,symbol=s1,line=l1)
		graficos.append(d1)
		mu, std = norm.fit(dados)
		x = np.linspace(min(bin_edges), max(bin_edges), 100)
		y = norm.pdf(x, mu, std)
		d1=Data(x=x,y=y,symbol=s1,line=l2)
		graficos.append(d1)
	g=plo[0]
	g.plot(graficos)
	#g.text('test',.51,.51,color=2)
	g.title('histograma de TC')
	g.yaxis(label=Label('Tc',font=2,charsize=1.5))
	g.xaxis(label=Label('p',font=5,charsize=1.5))
	plo.save(dir+'/resultado/histograma.agr')


def plotarDiagramaGausianas(diagramas):
	print 'iniciando plot gaussianas'
	pp = GracePlot() # A grace session opens
	s1=Symbol(symbol=symbols.circle,fillcolor=colors.red)
	l1=Line(type=lines.none)
	TcP=[]
	diagramaM=[]
	for p in concentracao:
		TcP=[x[1] for x in diagramas if x[0]==p]
		TcMedio, std = norm.fit(TcP)
		diagramaM.append((p,TcMedio))

	a,b=([x[0] for x in diagrama], [x[1] for x in diagrama])
	d1=Data(x=a,y=b,symbol=s1,line=l1)
	g=pp[0]
	g.plot(d1)
	g.title('Diagrama')
	g.yaxis(label=Label('Tc',font=2,charsize=1.5))
	g.xaxis(label=Label('p',font=5,charsize=1.5))
	pp.save(dir+'/resultado/diagramaMedio.agr')

def plotarDiagramaGausianas2(diagramas):
	print 'iniciando plot gaussianas'
	TcP=[]
	diagramaM=[]
	arquivo2 = open(dir+'/resultado/tcs.dat', 'w')
	linhas=[]
	for p in concentracao:
		TcP=[x[1] for x in diagramas if x[0]==p]
		linha='#%.7f \n' %(p)
		linhas.append(linha)
		for ponto in  TcP:
			linha='%.7f \n' %(ponto)
			linhas.append(linha)
		TcMedio, std = norm.fit(TcP)
		erro= std/math.sqrt(numeroAmostras)
		diagramaM.append((p,TcMedio, erro))
	arquivo2.writelines(linhas)
	arquivo2.close
	cabecalhoAgr = open('cabecalho.agr', 'r')
	cabecalho = cabecalhoAgr.readlines()
	cabecalhoAgr.close()
	arquivo = open(dir+'/resultado/diagramaMedioErro.agr', 'w')
	arquivo.writelines(cabecalho)
	linhas=[]
	for ponto in  diagramaM:
		linha='%.7f %.7f %.7f \n' %(ponto)
		linhas.append(linha)
	arquivo.writelines(linhas)
	arquivo.close

def plotarGausianas(amostras):
	print 'iniciando plot gaussianas '
	pTcSus = GracePlot()
	pSus = GracePlot()
	pTcCalor = GracePlot()
	pCalor = GracePlot()
	s1=Symbol(symbol=symbols.none,fillcolor=3)
	l1=Line(type=lines.solid)

	histogramaTcSus=[]
	histogramaSus=[]
	histogramaTcCalor=[]
	histogramaCalor=[]
	for resultado in amostras:
		t,mag,mag2,logmag2,energia,calor, su, cumo,cumuE, p =  resultado
		histogramaTcSus.append((p,t[su.argmax()]))
		histogramaSus.append((p,su.max()))
		histogramaTcCalor.append((p,t[calor.argmax()]))
		histogramaCalor.append((p,calor.max()))
	graficosTcSus=[]
	graficosSus=[]
	graficosTcCalor=[]
	graficosCalor=[]
	for p in concentracao:
		dadosTcSus=[x[1] for x in histogramaTcSus if x[0]==p]
		dadosSus=[x[1] for x in histogramaSus if x[0]==p]
		dadosTcCalor=[x[1] for x in histogramaTcCalor if x[0]==p]
		dadosCalor=[x[1] for x in histogramaCalor if x[0]==p]

		mu, std = norm.fit(dadosTcSus)
		a=mu - 2*std
		b=mu +2*std
		x = np.linspace(a, b, 100)
		y = norm.pdf(x, mu, std)
		dTcSus=Data(x=x,y=y,symbol=s1,line=l1)
		graficosTcSus.append(dTcSus)

		mu, std = norm.fit(dadosSus)
		a=mu - 2*std
		b=mu +2*std
		x = np.linspace(a, b, 100)
		y = norm.pdf(x, mu, std)
		dSus=Data(x=x,y=y,symbol=s1,line=l1)
		graficosSus.append(dSus)

		mu, std = norm.fit(dadosTcCalor)
		a=mu - 2*std
		b=mu +2*std
		x = np.linspace(a, b, 100)
		y = norm.pdf(x, mu, std)
		dTcCalor=Data(x=x,y=y,symbol=s1,line=l1)
		graficosTcCalor.append(dTcCalor)

		mu, std = norm.fit(dadosCalor)
		a=mu - 2*std
		b=mu +2*std
		x = np.linspace(a, b, 100)
		y = norm.pdf(x, mu, std)
		dCalor=Data(x=x,y=y,symbol=s1,line=l1)
		graficosCalor.append(dCalor)

	gTcSus=pTcSus[0]
	gTcSus.plot(graficosTcSus)
	gTcSus.title('gausiana Tc sus')
	gTcSus.yaxis(label=Label('P(Tc)',font=2,charsize=1.5))
	gTcSus.xaxis(label=Label('Tc',font=5,charsize=1.5))
	pTcSus.save(dir+'/resultado/gassianaTcSus.agr')

	gSus=pSus[0]
	gSus.plot(graficosSus)
	gSus.title('Gaussiana sus')
	gSus.yaxis(label=Label('P(susceptibilidade)',font=2,charsize=1.5))
	gSus.xaxis(label=Label('susceptibilidade',font=5,charsize=1.5))
	pSus.save(dir+'/resultado/gassianaSus.agr')

	gTcCalor=pTcCalor[0]
	gTcCalor.plot(graficosTcCalor)
	gTcCalor.title('gausiana TC calor')
	gTcCalor.yaxis(label=Label('P(Tc)',font=2,charsize=1.5))
	gTcCalor.xaxis(label=Label('Tc',font=5,charsize=1.5))
	pTcCalor.save(dir+'/resultado/gassianaTcCalor.agr')

	gCalor=pCalor[0]
	gCalor.plot(graficosCalor)
	gCalor.title('Gausiana calor')
	gCalor.yaxis(label=Label('Tc',font=2,charsize=1.5))
	gCalor.xaxis(label=Label('p',font=5,charsize=1.5))
	pCalor.save(dir+'/resultado/gassianaCalor.agr')
	print 'finalizando plot gaussianas '

#=======================================================================================
# subrotinas estimativa de tempo para e continuar
#=======================================================================================
def estimarDuracao(parametros):
	relaxacao,mcs,relaxacaoHistograma,mcsHistograma,J1,J2,alpha,tInicio,tFinal,dt,aglomeradoLimit,raio,numeroPontos,numeroAmostras,amostraSimulada,L,concentracao=parametros
	inicio = time.time()
	hist=ising.isingbcc(L,10,10,J1,J2,tInicio,concentracao[0],aglomeradoLimit)
	fim = time.time()
	unidadeTempo=(fim-inicio)/10
	tempo=unidadeTempo*3*6*(relaxacao+mcs)*len(concentracao)
	tempo2=unidadeTempo*((mcsHistograma/mcs)+ (relaxacaoHistograma/relaxacao))/2*(relaxacao+mcs)*len(concentracao)*numeroAmostras
	print "tempo estimado na primeira fase:"
	print timedelta(seconds=tempo),
	print "tempo para para n amostras"
	print timedelta(seconds=tempo2)
	print "tempo total :"
	print timedelta(seconds=(tempo+tempo2))

def tempoRestante(tempoMedio,n,numeroAmostras):
	tempAux=tempoMedio*(numeroAmostras-n)/4
	return timedelta(seconds=(tempAux))


def abrirSimulacao(simulacao):
	parametros, diagrama,diagramas,amostras,tempoMedio=simulacao
	relaxacao,mcs,relaxacaoHistograma,mcsHistograma,J1,J2,alpha,tInicio,tFinal,dt,aglomeradoLimit,raio,numeroPontos,numeroAmostras,amostraSimulada,L,concentracao=parametros
	try:
		os.makedirs(dir)
	except OSError:
		print "diretori principal já existente "
	finally:
		try:
			os.makedirs(dir+'/resultado')
		except OSError:
			print "subdiretorio resultado já existente "
		try:
			arq=open(dir+'/'+'simulacao.dat','r')
			try:
				simulacao=pickle.loads(arq.read())
			except  IOError as e:
				print "I/O error({0}): {1}".format(e.errno, e.strerror)
			except:
				print "erro para transformar o arquivo"
			arq.close
		except IOError as e:
			 print "I/O error({0}): {1}".format(e.errno, e.strerror)
			 print "criando diretório"
		 	 arq=open(dir+'/'+'simulacao.dat','w')
		 	 arq.close
		return simulacao

def salvarSimulacao(simulacao):
	try:
		arq=open(dir+'/'+'simulacao.dat', 'w')
		arq.writelines(pickle.dumps(simulacao))
		arq.close
	except Exception:
		print "erro para salvar"

#=======================================================================================
# subrotinas simular e buscar diagrama
#=======================================================================================

def primeiroDiagrama(parametros):
	relaxacao,mcs,relaxacaoHistograma,mcsHistograma,J1,J2,alpha,tInicio,tFinal,dt,aglomeradoLimit,raio,numeroPontos,numeroAmostras,amostraSimulada,L,concentracao=parametros
	def Localizatc(tInicio,tFinal,n,p,L,J1,J2,suv,tv):
		if n<=0 :
			return (tInicio+tFinal)/2
		else:
			dt=(tFinal - tInicio)/5
			t=tInicio
			while(t <= tFinal):
				hist=ising.isingbcc(L,relaxacao,mcs,J1,J2,t,p,aglomeradoLimit)
				h = np.array(hist)
				sus=h.var(axis=0)
				suv.append(sus[2])
				tv.append(t)
				t += dt
			tc=tv[suv.index(max(suv))]
			return  Localizatc(max(tc-dt,0.02),tc+dt,n-1,p,L,J1,J2,suv,tv)

	for p in concentracao:
		J1=1 - alpha*p
		suv=[]
		tv=[]
		tc=Localizatc(tInicio,tFinal,3,p,L,J1,J2,suv,tv)
		pontos = zip(tv, suv)
		pontos.sort(key=lambda x: x[0])
		tv,suv=zip(*pontos)
		diagrama.append((p,tc))
	diagramaAux=[]
	for ponto in diagrama:
		p,t0 = ponto
		J1=1 - alpha*p
		hist=ising.isingbcc(L,relaxacaoHistograma,mcsHistograma,J1,J2,t0,p,aglomeradoLimit)
		resultado=Histogram.histogram(hist,L,J1,J2,t0,t0-raio,t0+raio, numeroPontos, mcsHistograma)
		VectResultado = np.array(resultado)
		t_=VectResultado.transpose()[0]
		sus_=np.nan_to_num(VectResultado.transpose()[6])
		tc=t_[sus_.argmax()]
		diagramaAux.append((p,tc))
	return diagramaAux

def simulaAmostra(entrada):
	parametros, diagrama, n = entrada
	relaxacao,mcs,relaxacaoHistograma,mcsHistograma,J1,J2,alpha,tInicio,tFinal,dt,aglomeradoLimit,raio,numeroPontos,numeroAmostras,amostraSimulada,L,concentracao=parametros
	diagramaAux=[]
	amostra=[]
	for ponto in diagrama:
		p,t0 = ponto
		J1=1 - alpha*p
		hist=ising.isingbcc(L,relaxacaoHistograma,mcsHistograma,J1,J2,t0,p,aglomeradoLimit)
		resultado=Histogram.histogram(hist,L,J1,J2,t0,t0-raio,t0+raio, numeroPontos, mcsHistograma)
		VectResultado = np.array(resultado)
		t_=VectResultado.transpose()[0] #melhorar isso
		mag_=np.nan_to_num(VectResultado.transpose()[1])
		mag2_=np.nan_to_num(VectResultado.transpose()[2])
		logmag2_=np.nan_to_num(VectResultado.transpose()[3])
		energia_=np.nan_to_num(VectResultado.transpose()[4])
		calor_=np.nan_to_num(VectResultado.transpose()[5])
		sus_=np.nan_to_num(VectResultado.transpose()[6])
		cumo_=np.nan_to_num(VectResultado.transpose()[7])
		cumuE_=np.nan_to_num(VectResultado.transpose()[8])
		amostra.append((t_, mag_, mag2_,logmag2_,energia_,calor_, sus_, cumo_, cumuE_,p))
		tc=t_[sus_.argmax()]
		resultado2=Histogram.histogram(hist,L,J1,J2,t0,tc-raio/200,tc+raio/200, 2*numeroPontos, mcsHistograma)
		VectResultado2 = np.array(resultado2)
		sus_=np.nan_to_num(VectResultado.transpose()[6])
		tc=t_[sus_.argmax()]
		resultado3=Histogram.histogram(hist,L,J1,J2,t0,tc-raio/20000,tc+raio/20000, 2*numeroPontos, mcsHistograma)
		VectResultado2 = np.array(resultado2)
		sus_=np.nan_to_num(VectResultado.transpose()[6])
		tc=t_[sus_.argmax()]
		diagramaAux.append((p,tc))
	saida=(amostra, diagramaAux)
	return saida


#===========================================================================
# Programa principal
#===========================================================================
# Configuração da Simulação
relaxacao=100
mcs=500
relaxacaoHistograma=3*relaxacao
mcsHistograma=10*mcs
J1=1
J2=0.75
alpha=1.25
aglomeradoLimit=60
raio=0.8 #raio do intervalo em torno de tc que o histograma corre
numeroPontos=1000 #número de pontos que vai ter o gráfico gerado com o histograma
numeroAmostras=20
amostraSimulada=-1
L=5
tInicio=0.1
tFinal=8
dt=0.05
concentracao=[0, 0.05, 0.1, 0.15, 0.2,0.25, 0.3, 0.4, 0.5, 0.6, 0.7]
primeiroDiagram=1
dir = './simualacaoJ1=%.1f,J2=%.2f,mcs=%d,rel=%d,l=%d, aglomerado=%d' %(J1,J2,mcs,relaxacao,L,aglomeradoLimit)
parametros=(relaxacao,mcs,relaxacaoHistograma,mcsHistograma,J1,J2,alpha,tInicio,tFinal,dt,aglomeradoLimit,raio,numeroPontos,numeroAmostras,amostraSimulada,L,concentracao)
diagramaOk=[(0), (0.05), (0.1), (0.15), (0.2), (0.25), (0.3), (0.35), (0.4), (0.45), (0.5), (0.55), (0.6), (0.65), (0.7)]
amostras=[]
diagrama=[]
diagramas=[]
tempoMedio=0
simulacao=(parametros, diagrama, diagramas, amostras,tempoMedio)

#inicia simulação
estimarDuracao(parametros)
parametros, diagrama, diagramas, amostras,tempoMedio=abrirSimulacao(simulacao)
relaxacao,mcs,relaxacaoHistograma,mcsHistograma,J1,J2,alpha,tInicio,tFinal,dt,aglomeradoLimit,raio,numeroPontos,numeroAmostras,amostraSimulada,L,concentracao=parametros
if amostraSimulada==-1:
	amostraSimulada=0
	estimarDuracao(parametros)
	if primeiroDiagram==1:
		print "buscado primeiro diagrama..."
		inicio = time.time()
		diagrama = primeiroDiagrama(parametros)
		fim = time.time()
		print "tempo gasto :"
		tempoMedio=(fim-inicio)
		print timedelta(seconds=tempoMedio)
		graceDiagrama(diagrama)
		graceDiagramaNormalizado(diagrama)
		amostraSimulada=0
	else:
		diagrama=diagramaOk
	parametros=(relaxacao,mcs,relaxacaoHistograma,mcsHistograma,J1,J2,alpha,tInicio,tFinal,dt,aglomeradoLimit,raio,numeroPontos,numeroAmostras,amostraSimulada,L,concentracao)
	salvarSimulacao((parametros, diagrama,diagramas, amostras,tempoMedio))
if amostraSimulada==0:
	print "iniciando a simulacao para varias amostras..."
if amostraSimulada>=numeroAmostras:
	print "Simulacao das amostras finalizadas!"
else:
	amostrasimulada=amostraSimulada
	print "continuando simulcao ... tempo esimado de: "
	print tempoRestante(tempoMedio,amostrasimulada, numeroAmostras)
	for n in range(amostrasimulada, numeroAmostras,4): #paralelizar aqui
		inicio = time.time()
		diagramaAux=[]
		entrada=parametros,diagrama,n+1
		if __name__ == '__main__':
			p = Pool(4)
			(amostra1, diagrama1), (amostra2, diagrama2), (amostra3, diagrama3), (amostra4, diagrama4) = p.map(simulaAmostra, [entrada, entrada, entrada, entrada])
			amostras += amostra1+ amostra2+ amostra3 + amostra4
			diagramaMedio= (np.array(diagrama1, float ) + np.array(diagrama2, float )+ np.array(diagrama3, float ) + np.array(diagrama4, float ))/4
			diagramas+=diagrama1 + diagrama2 + diagrama3 + diagrama4
			diagrama=diagramaMedio.tolist()
		amostraSimulada=n+4
		parametros=(relaxacao,mcs,relaxacaoHistograma,mcsHistograma,J1,J2,alpha,tInicio,tFinal,dt,aglomeradoLimit,raio,numeroPontos,numeroAmostras,amostraSimulada,L,concentracao)
		fim = time.time()
		tempoMedio=(fim-inicio)/4
		salvarSimulacao((parametros, diagrama,diagramas, amostras,tempoMedio))
		print "simulada amostras até o número %d" %(amostraSimulada)
		print "tempo esimado para concluir: "
		print tempoRestante(tempoMedio,n, numeroAmostras)

#plotarDiagramaGausianas2(diagramas)
plotarMediaQuantidades(amostras)
#graceDiagrama(diagrama)
#graceDiagramaNormalizado(diagrama)
#plotarGausianas(amostras)
#graceHistogramaTc(amostras)
#plotarMaximos(amostras)
#plotarQuantidades(amostras)
