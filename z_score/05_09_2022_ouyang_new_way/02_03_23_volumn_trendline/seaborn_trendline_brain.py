import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# # create some sample data
# df = pd.DataFrame({'x': [1, 2, 3, 4, 5], 'y': [1, 4, 9, 16, 25]})

# # fit a quadratic regression model
# model = sns.regplot(x='x', y='y', data=df, order=2)

# # add a title to the plot
# model.set(title='Quadratic Regression Model')

# # show the plot
# plt.show()


# Create a sample dataset
# data = pd.DataFrame({'x': [1, 2, 3, 4, 5], 'y': [1, 3, 2, 5, 4]})

# # Create a scatter plot with a curved trendline
# sns.regplot(x='x', y='y', data=data, lowess=True)

# plt.show()





# # Generate random data
# x = np.random.uniform(0, 10, size=100)
# y = np.sin(x) + np.random.normal(0, 0.5, size=100)

# # Create a data frame
# df = pd.DataFrame({'x1': x, 'y1': y})


df = pd.read_csv("BrainSegVolNotVent_harmo_no_all.csv")


# Plot with seaborn
#sns.regplot(x='Age', y='BrainSegVolNotVent', data=df, order=20, ci=None, scatter_kws={'s': 1}, line_kws={"color": "red"})
#sns.lmplot(x='Age', y='BrainSegVolNotVent', data=df.query("Dataset != 'BGSP' & Dataset != 'beijingEn'"), order=20, ci=None, scatter_kws={'s': 1}, line_kws={"color": "red"})
sns.lmplot(x='Age', y='BrainSegVolNotVent', data=df, order=20, ci=None, scatter_kws={'s': 1}, line_kws={"color": "red"})

# Add labels and title
plt.xlabel('Age')
plt.ylabel('BrainSegVolNotVent')
plt.title('Curved Trendline with Seaborn - BrainSegVolNotVent')
plt.show()
