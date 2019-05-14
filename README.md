# Nonparametric Project

This github repo presents the final project of nonparametric project offered at Columbia University.

Course info:
- STATS GR 5222
- Title: Nonparametric Statistics
- Instructor: Zhiliang Ying
- Group II

Member:
- Caihui Xiao cx2225
- Xiaotong Li xl2788
- Xiaotong Lin xl2506
- Yankang Bennie Chen yc3256
- Yiqiao Yin yy2502 

# Intro

Through several parametric and nonparametric statistical methods, such as t-test, correlation tests, and interaction tests, the goal of this study is to determine the significant predictor variable(s) to classify the income level and figure out their relationships, based on the data extracted from the 1994 U.S. Census database. According to U.S. Census Bureau data from 1994, the median household income is around $50,000 per year, so the dataset separate people into two groups, one of which had an annual income over $50,000 and the other had not. In addition, the data set consists of a random sample of 48,842 persons with 8 predictor variables, including age, education, hours/week, gender, race, native country, workclass, and occupation, towards income. The detailed data description will be shown in Table.1 in Part6: Attachment. By taking 0.05 as the significant level in the whole project and through the various test methods, the results show that age, education, gender, and working hours per week are the 4 most important and informative variables that influence
people’s annual income.

<p align="center">
  <img width="800" src="https://github.com/yiqiao-yin/Nonparametric-Project/blob/master/figs/fig0data.PNG">
</p>

# Selected Results

## Correlation

We implement Pearson, Spearman, and Kendall Correlation Test. Since their results are very similar, we take Pearson’s correlation test as an example.

<p align="center">
  <img width="500" src="https://github.com/yiqiao-yin/Nonparametric-Project/blob/master/figs/fig1.PNG">
</p>

From the result shown in the above graph, blue indicates a positive correlation while orange
indicates a negative one. We observe that income is positively correlated with age (correlation=0.23),
hours/week(0.22), the education level(0.33), and gender(0.22), but it is negatively correlated with
race(-0.06). Hence, we can interpret these results as people’s income increases with the increase of
age, working hours per week and his or her education. Particularly, the education level is one of the
most important variables. In addition, as we assign 0 for female and 1 for male, the correlation result
shows that males tend to have a high income. Similarly, we assign 0 to white people, 1 for black
people, and 2 for other races, the results show that white people tend to have a high income.

## Prediction Accuracy

Considering above methodologies, Two-Sample Tests suggest us to excluding workclass because
its distributions are not affected by income. In addition, the Interaction Test suggests us to exclude
native country and adding the below 4 pairs of joint variables. We then apply these selected variables
with Random Forest, which has the best performance on the original data set, and the results are
presented in table below. We can conclude that by only excluding workclass, Random Forest
classifies our dataset the best with prediction accuracy 81.79%. This, indeed, match the results of
Two-Sample Tests and we do not consider workclass for the Correlation Tests as it is a categorical
variable.

<p align="center">
  <img width="800" src="https://github.com/yiqiao-yin/Nonparametric-Project/blob/master/figs/fig2.PNG">
</p>

# Conclusion

Under the situation that the relationships among variables are complex,
nonparametric methods tend to perform better than parametric methods. Regarding this census
dataset, Random Forest outperforms all other classification methods. 

- The distribution of workclass is not affected by the income level, which demonstrates that no
matter a person works at government, his or her income will be that different;
- Correlation test shows that people’s income increases along
with age, working hours per week and their education level
as below graph shows. This is corresponding to the market
rules. In addition, white people and males are more likely to
have higher income;
- Although the joint effect of the pairs of variable detected by
interaction test does not improve the classification accuracy,
it indicates that association between age & education, age
&hour/week, education & hour/week, age & gender, and
hour/week & gender. For example, older people have more
time to educate themselves, so their association is intuitive.
- Contingency tables help us to validate that, indeed, males
with the same education background, are more likely to
receive high income.

<p align="center">
  <img width="300" src="https://github.com/yiqiao-yin/Nonparametric-Project/blob/master/figs/fig3.PNG">
</p>
