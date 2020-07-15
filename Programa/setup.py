#!/usr/bin/env python3

from setuptools import setup, find_packages

with open('README.md') as f:
    long_description = ''.join(f.readlines())

setup(
    name='FeAl alloy Simulation Monte Carlo ',
    version='10.08',
    packages=find_packages(exclude=['tests']),
    include_package_data=True,
    
    # metadata to display on PyPI
    author='João Batista Santos-Filho',
    author_email='joao@jbsantosfilho.com',
    description='Monte Carlo Simulation of Fe-AL alloy',
    keywords="hello world example examples",
    long_description=long_description,
    url='jbsantosfilho.com',
    classifiers=[
        'License :: GPL :: Python Software Foundation License'
    ]

    # All versions are fixed just for case. Once in while try to check for new versions.
    install_requires=[
        'multiprocessing',
        'psutil',
        'time',
        'firebase_admin',
    ],

    # Do not use test_require or build_require, because then it's not installed and
    # can be used only by setup.py. We want to use it manually as well.
    # Actually it could be in file like dev-requirements.txt but it's good to have
    # all dependencies close each other.
    extras_require={
        'devel': [
            'mypy==0.620',
            'pylint==2.1.1',
            'pytest==3.7.1',
        ],
    },

    entry_points={
        'console_scripts': [
            'webapp = webapp.cli:main',
        ],
    },

    classifiers=[
        'Framework :: Flask',
        'Intended Audience :: Developers',
        'Development Status :: 1 - Alpha',
        'License :: Other/Proprietary License',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 3',
        'Topic :: Simulation Monte Carlo :: Fe-Al',
    ],
    zip_safe=False,
)
