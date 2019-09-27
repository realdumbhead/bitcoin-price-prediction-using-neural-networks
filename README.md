# bitcoin-price-prediction-using-neural-networks


https://www.kaggle.com/dumbhead/predicting-bitcoin-price-using-a-neural-network

#Description
I hope to share my attempts at predicting the price of Bitcoin using a neural network. I will be focusing on the 30 minute time frame as larger time frame have less data, and a bigger neural network will often overfit.

A common problem I see in many models is that they often just predict a price that is very close in the previous time period. If look at the distribution of how much the price of Bitcoin fluctuates every 30 minutes, most of the time it is close to zero!

I decided to tackle this issue by treating the task as a multi-class classification problem. Multi-class classification!? How would that work? I plan to predict the percentage change in price instead of predicting the price itself. This will avoid the skews of the highly different prices during different price periods. I can divide the prices changes into chunks. For instance, if I want to use 4 classes, I can divide the percentage price changes into buckets of 1+% decrease, 0-1% decrease, 0-1% increase, and 1+% increase. This way I can make the amount of data in each class relatively equal by having more classes near 0% price change, which does not disincentivise my model from making bigger prediction.

In addition, the price patterns changes. Price pattern that worked in 2014 may not work in 2019. Therefore, I retrain the neural network after a certain amount of time frames.

Because of the high correlation between HLOC, I only choose to use of them. I found the close was the most effective.

#Interpreting the test set charts:
The EV is calculated cumulatively (adding the percentages gains/loss), multiplicatively (what would happen if you trade with your entire balance), and optimalf (similar to Kelly criterion but for multiclass).

The signal strength represents how strongly the NN thinks the price will go up or down before it makes a trade. Also, the graph shows what happens if the NN only shorts or longs. HODL means buying and holding. This does not include fees.