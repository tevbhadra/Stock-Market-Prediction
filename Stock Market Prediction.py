
# coding: utf-8

# In[6]:

from pandas.io import data
from pandas_datareader import data
prices = web.DataReader('BA', 'yahoo', start, end)['Close']


# In[ ]:

prices.head(5)


# In[ ]:

import pandas_datareader.data as web
import pandas as pd
import datetime as dt
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import style

style.use('ggplot')

start = dt.datetime(2016, 4, 1)
end = dt.datetime(2018, 12, 31)

prices = web.DataReader('BA', 'yahoo', start, end)['Open']
returns = prices.pct_change()

last_price = prices[-1]

#Number of Simulations
num_simulations = 10000
num_days = 60

simulation_df = pd.DataFrame()

for x in range(num_simulations):
    count = 0
    daily_vol = returns.std()
    
    price_series = []
    
    price = last_price * (1 + np.random.normal(0, daily_vol))
    price_series.append(price)
    
    for y in range(num_days):
        if count == 251:
            break
        price = price_series[count] * (1 + np.random.normal(0, daily_vol))
        price_series.append(price)
        count += 1
    
    simulation_df[x] = price_series
    
fig = plt.figure()
fig.suptitle('Monte Carlo Simulation: BA')
plt.plot(simulation_df)
plt.axhline(y = last_price, color = 'r', linestyle = '-')
plt.xlabel('Day')
plt.ylabel('Price')
plt.show()


# In[ ]:

simulation_df=simulation_df.iloc[1:]


# In[ ]:

x = np.mean(simulation_df, axis=1)


# In[ ]:

x


# In[ ]:

start = dt.datetime(2018, 11, 27)
end = dt.datetime(2018, 12, 1)

prices = web.DataReader('BA', 'yahoo', start, end)['Open']
y= prices
y


# In[ ]:

y


# In[ ]:

np.corrcoef(x,y)

