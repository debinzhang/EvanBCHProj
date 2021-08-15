#!/usr/bin/python3

import matplotlib as mpl
mpl.use('tkagg')

import matplotlib.pyplot as plt 
import pandas as pd
import seaborn as sns
from scipy.optimize import curve_fit
import numpy as np


def func(x,a,b):
  return b+a*np.log(x)


data = pd.read_csv("PostHarmon_ICV.csv")
data1 = pd.read_csv("PostHarmon_CSF.csv")
# xData = np.array(data["Age"])
# yData = np.array(data["ICV"])
# plt.plot(xData,yData,label = 'Line of Best Fit')
# popt, pcov = curve_fit(func, xData, yData)
# xFit = np.arange(0.0,100.00,1)
# plt.plot(xFit, func(xFit, *popt), 'r', label ='fit params: a=%5.3f, b=%5.3Ff' % tuple(popt))
# # subjects = sns.load_dataset('raw_data')
plt.figure(figsize=(10,8))
# plt.scatter('Age', 'ICV', data = data)
# plt.xlabel('Age', fontsize = 'large')
# plt.ylabel('ICV', fontsize = 'large')
sns.scatterplot(x=data["Age"],y=data["ICV"], hue=data["Dataset"])
plt.figure(figsize=(10,8))
sns.scatterplot(x=data1["Age"],y=data1["CSF"], hue=data1["Dataset"])
# sns.lmplot(x='Age',y='ICV', hue = 'Dataset', data = data, fit_reg = True, order = 1, ci = None, scatter_kws={'alpha':0.3})
# sns.lmplot(x='Age',y='ICV', hue = 'Dataset', data = data, fit_reg = True, order = 1, ci = None, scatter_kws={'alpha':0.3}, col = "Dataset")
# sns.regplot(x=data["Age"],y=data["ICV"])
plt.show()

