# The American Statistical Association (ASA) DataFest™ 2022

The American Statistical Association (ASA) DataFest™ is a celebration of data in which student teams work around the clock to find and share meaning in a large, rich, and complex dataset.

In the Spring 22 semester, me and four other students participated in this data analysis competition. In  it , we were given a dataset that contained a row for each click each one of the ~500 participants made (200,000 in total). 

The goal of the task was to predict which kids were at risk of developing a future drug addiction based on gaming behaviors. 

The overall game was divided into four minigames, each one was intended to measure a different skill (e.g. learn from mistakes, resist peer pressure, etc.)

We first found a way of getting the time spent in each minigame by each participant, as well as the amount of times a kid failed at each minigame. 

With that information, we used k-means clustering to identify the participants that have the biggest risk of developing a drug addiction. 
