import sys


from distutils.core import setup, Extension


module1 = Extension('big2table',
                    sources = ['big2table.cpp', 'handtypetable.c'],
                    extra_compile_args=['-std=c++17', '-Wall'])

setup (name = 'Big2Table',
       version = '0.1',
       description = 'Big 2 (the card game) hand rankings, tabulated.',
       ext_modules = [module1])
