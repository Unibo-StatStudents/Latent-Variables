Lab1_latent_var
================
2024-01-04

# Factor analysis on mental ability score tests

The classic Holzinger and Swineford (1939) dataset consists of mental
ability test scores of seventh- and eighth-grade children from two
different schools (Pasteur and Grant-White). In the original dataset
(available in the MBESS package), there are scores for 26 tests.
However, a smaller subset with 9 variables is more widely used in the
literature. Perform first an exploratory factor analysis and then a
confirmatory factor analysis on this data set

As a good proxy, let’s first see the structure of the dataset: the
dataset is contained in the lavaan library

``` r
library(lavaan)
```

    ## This is lavaan 0.6-16
    ## lavaan is FREE software! Please report any bugs.

``` r
data("HolzingerSwineford1939")
head(HolzingerSwineford1939)
```

    ##   id sex ageyr agemo  school grade       x1   x2    x3       x4   x5        x6
    ## 1  1   1    13     1 Pasteur     7 3.333333 7.75 0.375 2.333333 5.75 1.2857143
    ## 2  2   2    13     7 Pasteur     7 5.333333 5.25 2.125 1.666667 3.00 1.2857143
    ## 3  3   2    13     1 Pasteur     7 4.500000 5.25 1.875 1.000000 1.75 0.4285714
    ## 4  4   1    13     2 Pasteur     7 5.333333 7.75 3.000 2.666667 4.50 2.4285714
    ## 5  5   2    12     2 Pasteur     7 4.833333 4.75 0.875 2.666667 4.00 2.5714286
    ## 6  6   2    14     1 Pasteur     7 5.333333 5.00 2.250 1.000000 3.00 0.8571429
    ##         x7   x8       x9
    ## 1 3.391304 5.75 6.361111
    ## 2 3.782609 6.25 7.916667
    ## 3 3.260870 3.90 4.416667
    ## 4 3.000000 5.30 4.861111
    ## 5 3.695652 6.30 5.916667
    ## 6 4.347826 6.65 7.500000

Since we need just the first 9 variables,

``` r
data <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9")]
```

The correlation matrix provides insights into the relationships between
variables and can help to assess whether there are underlying latent
factors that explain the observed correlations.

``` r
matrcor <- cor(data)
heatmap(matrcor)
```

![](lab1_first_part_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> We
can see from the heatmap that could be three latent variables that
describes the grouping structure that the correlation matrix present: in
particular, we can expect that the latent variables will have different
factor loadings for the three group, i.e. $(x_7, x_8)$,
$(x_6, x_5, x_4)$ and $(x_1,x_2,x_3,x_9)$.

Now we are going to estimate a factor model respectively with 1,2, and 3
factors using the maximum likelihood method. We will use the factanal
function, where we specify the formula

``` r
attach(data)
f1<-factanal(~ x1+x2+x3+x4+x5+x6+x7+x8+x9,
              data=data,
             1)
f2<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,
             data=data,
             2) 
f3<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,
             data=data,
             3)
```

``` r
f3
```

    ## 
    ## Call:
    ## factanal(x = ~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, factors = 3,     data = data)
    ## 
    ## Uniquenesses:
    ##    x1    x2    x3    x4    x5    x6    x7    x8    x9 
    ## 0.513 0.749 0.543 0.279 0.243 0.305 0.502 0.469 0.543 
    ## 
    ## Loadings:
    ##    Factor1 Factor2 Factor3
    ## x1  0.277   0.623   0.151 
    ## x2  0.105   0.489         
    ## x3          0.663   0.130 
    ## x4  0.827   0.165         
    ## x5  0.861                 
    ## x6  0.801   0.212         
    ## x7                  0.696 
    ## x8          0.162   0.709 
    ## x9  0.132   0.406   0.524 
    ## 
    ##                Factor1 Factor2 Factor3
    ## SS loadings      2.185   1.343   1.327
    ## Proportion Var   0.243   0.149   0.147
    ## Cumulative Var   0.243   0.392   0.539
    ## 
    ## Test of the hypothesis that 3 factors are sufficient.
    ## The chi square statistic is 22.38 on 12 degrees of freedom.
    ## The p-value is 0.0335

From the model we get some intersting things: most of all we can see the
values of the factor loadings. The value that are small are not printed
by default by the function, to give emphasis on the most significative
ones. We can see that the Factor 1 has high values for the variables
$(x_4,x_5,x_6)$: if we see the description of the dataset all of these
variables are in somehow related to the sphere of grammar comprehension.
Therefore, we can interpret the first latent variable as the variable
which captures the linguistic cognitive area. Then, for the second
factor we have high values for $(x_1,x_2,x_3,x_9)$ which from the
description given in the help is not clear what they actually means, so
I don’t know, better to ask a psychologist. In the end, for the third
factor we have high values for $(x_7, x_8)$ where those variables can be
seen as the speed of capturing what is asked in the question: so, the
third latent variables is the one that is related to the sphere of the
“speed”.

## Goodness of fit

Now that we fitted three models is time to check which among them is the
best one, so, in other words we have to choose the value of $q$, i.e.,
the number of the latent variabels.

We select the best model using the Chi-square test since for this
problem we already find the statistic inside the fitted model. Recall
that, under normality assumption, the null hypotesis is
$H_0:\Sigma = \Lambda\Lambda' +\Psi$

Inside the factor model just created, there is the significance test
under “STATISTIC”, check the ?factanal help.

For the first model

``` r
Chisq_f1 <- round(f1$STATISTIC, 3)
Chisq_f1
```

    ## objective 
    ##   306.558

And for the other two models

``` r
Chisq_f2 <- round(f2$STATISTIC, 3)
Chisq_f3 <- round(f3$STATISTIC, 3)
Chisq_f2
```

    ## objective 
    ##   127.637

``` r
Chisq_f3
```

    ## objective 
    ##    22.377

If we want to compute the degrees of freedom we recall that

$df = \frac{[(p-q)^2-(p+q)]}{2}$

and so, since $p=9$, for the first model the degrees of freddom are
$27$.

Luckily, this value is already in the model fitted and it is sufficient
to recall it in this way

``` r
df_f1 <- f1$dof
df_f1
```

    ## [1] 27

And for the other two models

``` r
df_f2 <- f2$dof
df_f3 <- f3$dof
```

To get the p-values, again, we just need to look at the fitted model

``` r
p_value_df1 <- f1$PVAL
p_value_df2 <- f2$PVAL
p_value_df3 <- f3$PVAL
```

#### Factor Loadings

Now, on the basis of the matrix of the factor loadings, we need to
interpret the factors individuated: to extract the loadings we use the
“loadings” function which requires an object of class “factanal”.
Moreover, since we are interested just on the significant factors we can
put a cutoff value inside the function. Although, small loadings are not
printed, to draw the eye to the pattern of the larger loadings.

``` r
loadings(f1)
```

    ## 
    ## Loadings:
    ##    Factor1
    ## x1 0.438  
    ## x2 0.220  
    ## x3 0.223  
    ## x4 0.848  
    ## x5 0.841  
    ## x6 0.838  
    ## x7 0.180  
    ## x8 0.201  
    ## x9 0.307  
    ## 
    ##                Factor1
    ## SS loadings      2.586
    ## Proportion Var   0.287

``` r
loadings(f2)
```

    ## 
    ## Loadings:
    ##    Factor1 Factor2
    ## x1 0.324   0.471  
    ## x2 0.151   0.268  
    ## x3         0.455  
    ## x4 0.837   0.158  
    ## x5 0.848   0.127  
    ## x6 0.816   0.181  
    ## x7         0.440  
    ## x8         0.607  
    ## x9 0.126   0.725  
    ## 
    ##                Factor1 Factor2
    ## SS loadings      2.245   1.664
    ## Proportion Var   0.249   0.185
    ## Cumulative Var   0.249   0.434

``` r
loadings(f3)
```

    ## 
    ## Loadings:
    ##    Factor1 Factor2 Factor3
    ## x1  0.277   0.623   0.151 
    ## x2  0.105   0.489         
    ## x3          0.663   0.130 
    ## x4  0.827   0.165         
    ## x5  0.861                 
    ## x6  0.801   0.212         
    ## x7                  0.696 
    ## x8          0.162   0.709 
    ## x9  0.132   0.406   0.524 
    ## 
    ##                Factor1 Factor2 Factor3
    ## SS loadings      2.185   1.343   1.327
    ## Proportion Var   0.243   0.149   0.147
    ## Cumulative Var   0.243   0.392   0.539

For example, in the f1 model, the variable x4, x5, x6, have high values,
suggesting that they strongly contribute to Factor1.

In the f2 model, Variables x1, x2, and x9 show stronger relationships
with Factor2. Same for the x8 and x9 in the f3 model. Actually, we
already did this interpretation for the model with three factors before,
after we fitted the model. Would you say something more?

#### Communalities

And now let’s compute now the communalities: recall that the
communalities are defined as $h_i^2 = \sum_{k=1}^{p}\lambda_{ki}^2$

Computing communalities can provide insights into how well the factors
account for the observed variability in the data: in particular, high
communalities suggest that the factors are capturing a substantial
portion of the variance in the observed variables.

So, we just need to sum by rows the squared loadings

``` r
comm_f1 <- rowSums(loadings(f1)^2) 
comm_f2 <- rowSums(loadings(f2)^2)
comm_f3 <- rowSums(loadings(f3)^2)
```

``` r
sum(comm_f1)
```

    ## [1] 2.585581

``` r
sum(comm_f2)
```

    ## [1] 3.908608

``` r
sum(comm_f3)
```

    ## [1] 4.854672

Adding a latent factor to a model, will increase the communalities, like
in the PCA, since the variance explained will be for sure bigger and
bigger as we add latent variables to the model: therefore, just to have
a proof of this, let’s fit the model with 4 latent factors

``` r
f4<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,
             data=data,
             4)
comm_f4 <- rowSums(loadings(f4)^2)
```

Now, to have also a visual understanding of the communalities, let’s
plot them in a bar plot

``` r
# Create a vector of communalities
communalities <- c(sum(comm_f1), sum(comm_f2), sum(comm_f3), sum(comm_f4))

# Factor model labels
factor_models <- c("Factor Model 1", "Factor Model 2", "Factor Model 3", "Factor Model 4")

# Create a bar plot
barplot(communalities, names.arg = factor_models, col = "green", main = "Communalities for Factor Models", ylab = "Communalities", ylim = c(0, max(communalities) + 1))
```

![](lab1_first_part_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
What we see here is that adding an additional factor increase the
communalities, although from the third to the fourth model the increase
is not that big as in the previous ones. This can help to estabilish
that the model with three factors is the best one.

Now, we want to compute the percentage of the variance of the model
explained by each model:

``` r
percVar_f1 <- sum(comm_f1)/9
percVar_f2 <- sum(comm_f2)/9
percVar_f3 <- sum(comm_f3)/9

percVar_f1
```

    ## [1] 0.2872868

``` r
percVar_f2
```

    ## [1] 0.4342898

``` r
percVar_f3
```

    ## [1] 0.5394079

So, what we did in this analysis can be seen in this way: first, check
the correlation matrix of the observed variables and see if there is
some grouping going on from the values inside the matrix (or the
heatmap). Then, fit a latent variable model where the essence is a model
for the correlation matrix of $x$, where it’s defined as
$\Sigma = \Lambda\Lambda' + \Psi$: now, this lambda matrix is crucial,
beacuse it’s the variance of our model with latent variables in some
sense (check the posterior distribution of $y|x$ in the slide). To see
if our analysis was a good analysis, we can check if the observed matrix
(the one computed in the first lines) is similar to the one that we
estimated with the fitted model: this last matrix is called “reproduced
correlation matrix”.

Here we compute the reproduced correlation matrix which is defined as:

$corr = \lambda_{ij}\lambda_{ij}'$

``` r
repcorr_f1 <- loadings(f1)%*%t(loadings(f1))
repcorr_f2 <- loadings(f2)%*%t(loadings(f2))
repcorr_f3 <- loadings(f3)%*%t(loadings(f3))
```

Now, to check the goodness of fit we simply takes the difference of the
matrix, and we can do it since they have the same dimensions: what we
expect is to have a sparse matrix outside the diagonal, since if the
model was well fitted it should give the same $\lambda_{ij}$

Let’s do this for the model with three factors

``` r
corr_check <- heatmap(matrcor - repcorr_f3)
```

![](lab1_first_part_files/figure-gfm/unnamed-chunk-18-1.png)<!-- --> As
we can see from the heatmap, outside the main diagonal the values are
near zero.
