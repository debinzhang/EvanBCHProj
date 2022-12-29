import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt

sns.set()

def draw_plot(feature):

  plot_file_path = feature + "_harmo_no_all_plot" + ".csv"
  df = pd.read_csv(plot_file_path)

  sns_lineplot = sns.lineplot(x='Age', y=feature, data=df.query("Sex != 'Unknown'"), hue='Sex')
  plt.title("harmonized " + feature + "after outlier removal")
  fig_lineplot = sns_lineplot.get_figure()
  fig_lineplot.savefig(feature+'.png')
  plt.clf()
  #plt.show()

  return True

