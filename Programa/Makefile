.PHONY: all help janela_ui.py vt analisaHist geraHist.f90
SHELL=/bin/bash
PYSOURCES = $(wildcard *.py) #todas os arquivos py são armazenados nessa variavel

VENV_NAME?=venv
VENV_BIN=$(shell pwd)/${VENV_NAME}/bin
VENV_ACTIVATE=. ${VENV_BIN}/activate

PYTHON=${VENV_BIN}/python3

all: help

help:
	@echo "make prepare-dev"
	@echo "    Create python virtual environment and install dependencies."
	@echo "make lint"
	@echo "    Run list on project."
	@echo "make test"
	@echo "    Run tests on project."
	@echo "make run"
	@echo "    Run server."
	@echo "make clean"
	@echo "    Remove python artifacts and virtualenv."
	@echo "make build"
	@echo "    Creates debian package."
	@echo "make janela"
	@echo "    cria janela qt."
	@echo "make install"
	@echo "    Installs package in your system."
	

prepare-dev:
	which dpkg-buildpackage || apt install -y debhelper dh-virtualenv dh-systemd lintian

	which python3 || apt install -y python3 python3-pip
	which virtualenv || python3 -m pip install virtualenv
	make venv

venv: $(VENV_NAME)/bin/activate
	$(VENV_NAME)/bin/activate: setup.py
	test -d $(VENV_NAME) || virtualenv -p python3 $(VENV_NAME)
	${PYTHON} -m pip install -U pip setuptools
	${PYTHON} -m pip install -e .[devel]
	touch $(VENV_NAME)/bin/activate

janela: janela.ui
	@echo "criando interface grafica"
	pyuic5 -x janela.ui -o janela_ui.py

buildFortran: vt analisaHist geraHist
	@echo "criando python fortran"

vt: vt.f90
	@echo "criando compilado fortran to python"
	f2py3 -c vt.f90 -m vt 

analisaHist: analisaHist.f90
	@echo "criando compilado fortran to python"
	f2py3 -c analisaHist.f90 -m analisaHist

geraHist: geraHist.f90
	@echo "criando compilado fortran to python"
	f2py3 -c geraHist.f90 -m geraHist

testPy: $(PYSOURCES)
	python3 $^

lint: venv
	${PYTHON} -m pylint --rcfile=pylintrc webapp
	${PYTHON} -m mypy --ignore-missing-imports webapp

test: venv
	${PYTHON} -m pytest -vv tests

run: venv
	${FLASK} run

shell: venv
	${FLASK} shell

clean:
	find . -name '*.pyc' -exec rm --force {} +
	rm -rf $(VENV_NAME) *.eggs *.egg-info dist build docs/_build .cache

build: venv
	dpkg-buildpackage -us -uc -b
	lintian ../webapp_1.0_all.deb

install:
	-dpkg -i ../webapp_1.0_all.deb
	apt install -f
