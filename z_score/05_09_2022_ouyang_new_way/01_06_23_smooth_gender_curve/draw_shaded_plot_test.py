import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import rcParams
import numpy as np
from scipy.interpolate import make_interp_spline

# set output plot size
a4_dims = (11.7, 8.27)
sns.set()

def draw_shaded_plot(feature):
  print("working on %s" % feature)
  plot_file_path = feature + "_harmo_no_all_plot" + ".csv"
  df = pd.read_csv(plot_file_path)

  fig, ax = plt.subplots(figsize=a4_dims)
  sns_lineplot = sns.lineplot(ax=ax, x='Age', y=feature, data=df.query("Sex != 'Unknown' & Age<=85"), hue='Sex')
  plt.title("harmonized " + feature + " after outlier removal")
  fig_lineplot = sns_lineplot.get_figure()
  fig_lineplot.savefig(feature+'.png')
  plt.clf()
  plt.close()
  return True

def draw_shade_hemisphere_plot(feature):
  print("working on %s" % feature)
  plot_file_path = feature+ "_hemisphere.csv"
  df = pd.read_csv(plot_file_path)

  fig, ax = plt.subplots(figsize=a4_dims)
  sns_lineplot = sns.lineplot(ax=ax, x='Age', y=feature, data=df.query("Sex != 'Unknown' & Age<=85"), hue='hemisphere')
  plt.title("harmonized " + feature + " after outlier removal")
  fig_lineplot = sns_lineplot.get_figure()
  fig_lineplot.savefig(feature+'_hemisphere.png')
  plt.clf()
  plt.close()
  return True

draw_shaded_plot("lh_bankssts_curvind")

a4_dims = (11.7, 8.27)
sns.set()
fig, ax = plt.subplots(figsize=a4_dims)

feature = "lh_bankssts_curvind"
df = pd.read_csv("lh_bankssts_curvind_harmo_no_all.csv")
fig, ax = plt.subplots(figsize=a4_dims)
sns_lineplot = sns.lineplot(ax=ax, x='Age', y=feature, data=df.query("Sex != 'Unknown' & Age<=85"), hue='Sex')
plt.title("harmonized " + feature + " after outlier removal")
fig_lineplot = sns_lineplot.get_figure()
plt.show()





a4_dims = (11.7, 8.27)
sns.set()

data0 = [
        {'Age': 1, 'Value':4, 'Sex': 'Male'},
        {'Age': 2, 'Value':9, 'Sex': 'Female'},
        {'Age': 4.5, 'Value':3, 'Sex': 'Male'},
        {'Age': 4, 'Value':14, 'Sex': 'Female'},
        {'Age': 2.5, 'Value':5, 'Sex': 'Male'},
        {'Age': 3.5, 'Value':4.5, 'Sex': 'Male'},
        {'Age': 2.5, 'Value':19.9, 'Sex': 'Female'},
        {'Age': 1.5, 'Value':6.5, 'Sex': 'Male'},
        {'Age': 4.5, 'Value':14.6, 'Sex': 'Female'},
        {'Age': 5.5, 'Value':15.9, 'Sex': 'Female'},
       ]
df0 = pd.DataFrame(data0)


# process male first
df1 = df0.loc[df0['Sex']=="Male"]
#sort dataframe by "Age". it is required by the make_interp_spline() function
df2 = df1.sort_values('Age')

xnew = np.linspace(df2['Age'].min(), df2['Age'].max(), 300)
gfg = make_interp_spline(df2['Age'], df2['Value'], k=3)
y_new = gfg(xnew)
# create new dataframe
df3 = pd.DataFrame({'Age':xnew, 'Value':y_new})
# add "Sex" column
df3['Sex'] = "Male"


# process female 
df1_1 = df0.loc[df0['Sex']=="Female"]
#sort dataframe by "Age". it is required by the make_interp_spline() function
df2_1 = df1_1.sort_values('Age')

xnew = np.linspace(df2_1['Age'].min(), df2_1['Age'].max(), 300)
gfg = make_interp_spline(df2_1['Age'], df2_1['Value'], k=3)
y_new = gfg(xnew)
# create new dataframe
df3_1 = pd.DataFrame({'Age':xnew, 'Value':y_new})
# add "Sex" column
df3_1['Sex'] = "Female"

# combine the above two dataframes
frames = [df3, df3_1]
df_final = pd.concat(frame3)

sns.set(rc={'figure.figsize':(11.7,8.27)})
#fig, ax = plt.subplots(figsize=a4_dims)
sns_lineplot = sns.lineplot(x='Age', y='Value', data=df_final.query("Sex != 'Unknown' & Age<=85"), hue='Sex')



fig, ax = plt.subplots(figsize=a4_dims)
sns_lineplot = sns.lineplot(ax=ax, x='Age', y='Value', data=df1.query("Sex != 'Unknown' & Age<=85"), hue='Sex')
plt.title("harmonized test data after outlier removal")
fig_lineplot = sns_lineplot.get_figure()
plt.show()

fig_lineplot.savefig(feature+'.png')


x = np.array([1, 2, 3, 4, 5])
y = np.array([4, 9, 1, 3, 5])
xnew = np.linspace(x.min(), x.max(), 300)
gfg = make_interp_spline(x, y, k=3)
y_new = gfg(xnew)
  
plt.plot(xnew, y_new)

