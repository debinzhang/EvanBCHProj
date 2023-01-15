import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import rcParams
import numpy as np
from scipy.interpolate import make_interp_spline

a4_dims = (11.7, 8.27)
sns.set()

def draw_shaded_plot_smooth(feature):
  print("working on %s" % feature)
  plot_file_path = feature + "_harmo_no_all_plot" + ".csv"
  df = pd.read_csv(plot_file_path)
  # we only care the Age, feature, and Sex columns. select them
  #df0 = df[['Age', 'Sex', feature]]
  df0 = df

  # process male first
  df1 = df0.loc[df0['Sex']=="Male"]
  # sort dataframe by "Age". it is required by the make_interp_spline() function
  df2 = df1.sort_values('Age')
  # aggregate rows with the same 'Age' value. make_interp_spline() does not allow duplicate 'x' value
  agg_function = {'Age':'first', 'Sex': 'first', feature: 'mean'}
  df2 = df2.groupby(df2['Age']).aggregate(agg_function)

  xnew = np.linspace(df2['Age'].min(), df2['Age'].max(), 300)
  gfg = make_interp_spline(df2['Age'], df2[feature], k=3)
  y_new = gfg(xnew)
  # create new dataframe
  df3 = pd.DataFrame({'Age':xnew, feature:y_new})
  # add "Sex" column
  df3['Sex'] = "Male"

  # process female 
  df1_1 = df0.loc[df0['Sex']=="Female"]
  #sort dataframe by "Age". it is required by the make_interp_spline() function
  df2_1 = df1_1.sort_values('Age')
  # aggregate rows with the same 'Age' value. make_interp_spline() does not allow duplicate 'x' value
  agg_function = {'Age':'first', 'Sex': 'first', feature: 'mean'}
  df2_1 = df.groupby(df2_1['Age']).aggregate(agg_function)

  xnew = np.linspace(df2_1['Age'].min(), df2_1['Age'].max(), 300)
  #xnew = np.linspace(df2_1['Age'].min(), df2_1['Age'].max(), 20)
  gfg = make_interp_spline(df2_1['Age'], df2_1[feature], k=3)
  y_new = gfg(xnew)
  # create new dataframe
  df3_1 = pd.DataFrame({'Age':xnew, feature:y_new})
  # add "Sex" column
  df3_1['Sex'] = "Female"

  # combine the above two dataframes
  frames = [df3, df3_1]
  df_final = pd.concat(frames)

  #sns.set(rc={'figure.figsize':(11.7,8.27)})
  fig, ax = plt.subplots(figsize=a4_dims)
  sns_lineplot = sns.lineplot(x='Age', y=feature, data=df_final.query("Sex != 'Unknown' & Age<=85"), hue='Sex', ax=ax)
  plt.title("harmonized " + feature + " after outlier removal - test 1")
  fig_lineplot = sns_lineplot.get_figure()
  fig_lineplot.savefig(feature+'.png')
  plt.clf()
  plt.close()
  return True
  #plt.show()


draw_shaded_plot_smooth("lh_bankssts_curvind")