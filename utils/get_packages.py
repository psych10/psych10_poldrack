"""
get a list of package installation commands
for all of the R/Rmd files in the repo
"""

import os,glob


files=glob.glob('../notebooks/*/*.Rmd') + \
    glob.glob('../inst/*/*.R')
packages=[]
for file in files:
    with open(file) as f:
        lines=[i.strip().split('(')[1].replace(')','') for i in f.readlines() if i.find('library')==0 or i.find('require')==0]
    packages=packages+lines
packages=list(set(packages))
for p in packages:
    print('install.packages("%s",dependencies=TRUE)'%p)
