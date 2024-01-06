Ex1_Lab1
================
2024-01-06

The data contain responses given by 410 individuals to four out of seven
items concerning attitude to abortion. A small number of individual
(379) did not answer to some of the questions and this data set contains
only the complete cases.

The dataframe used is contained in the ltm library

``` r
library(ltm)
```

    ## Loading required package: MASS

    ## Loading required package: msm

    ## Loading required package: polycor

``` r
data("Abortion")
head(Abortion)
```

    ##   Item 1 Item 2 Item 3 Item 4
    ## 1      1      1      1      1
    ## 2      1      1      1      1
    ## 3      1      1      1      1
    ## 4      1      1      1      1
    ## 5      1      1      1      1
    ## 6      1      1      1      1

379 individuals answered to the following questions after being asked if
the law should allow abortion under the cirumstances presented under
each item: to check the items see the help page.

With the function descript we can compute descriptive statistics for
dichotomous and polytomous response matrices.

``` r
descr <- descript(Abortion)
```

With \$perc we can have a numeric matrix which contains the percentages
of negative and positive responses for each item:

``` r
descr$perc
```

    ##                0         1      logit
    ## Item 1 0.5620053 0.4379947 -0.2493044
    ## Item 2 0.4063325 0.5936675  0.3791478
    ## Item 3 0.3641161 0.6358839  0.5575432
    ## Item 4 0.3825858 0.6174142  0.4785874

With \$item we have a numeric matrix containing the frequencies for the
total scores

``` r
descr$items
```

    ##        0  1  2  3   4
    ## Freq 103 33 37 65 141

For example, in this case, there are 103 observations that present the
pattern $(0,0,0,0)$ and 33 observations which answered with just a
single $1$ for the four items; there are also 141 observations which
have the pattern $(1,1,1,1)$

Also, we can see the p-values for the pairwise association between the
items

``` r
descr$pw.ass
```

    ##   Item i Item j p.value
    ## 1      1      4  <2e-16
    ## 2      1      3  <2e-16
    ## 3      2      4  <2e-16
    ## 4      1      2  <2e-16
    ## 5      2      3  <2e-16
    ## 6      3      4  <2e-16

Now, we use the function ltm to fit a 2 IRT model to the data. In the
formula we need to provide the response data matrix, which in our case,
is the Abortion dataset; in the right side we specify the latent
variables. In this case we are going to specify a single latent
variable: the IRT.param when set equal to true the coefficients
estimatets for the model are reported under the usual IRT
parametrization, i.e., with the logit function (the logarithm of the
odds).

``` r
irt_model <- ltm(Abortion~z1 , IRT.param = TRUE)
```

In the output of the irt_model we are going to see the coefficients for
the difficulty parameter and for the discrimination parameter: - If
Dffclt is positive, it means the item is relatively easier, and
individuals with lower levels of the latent trait are likely to endorse
it. - If Dffclt is negative, it means the item is relatively harder, and
individuals with higher levels of the latent trait are likely to endorse
it. The discrimnation parameter reflects how well the item discriminates
between individuals with different levels of the latent trait: - if
Dscrmn is high, the item effectively distinguishes between individuals
with different levels of the latent trait. - if Dscrmn is low, the item
may not effectively discriminate between individuals with varying trait
levels.

``` r
irt_model
```

    ## 
    ## Call:
    ## ltm(formula = Abortion ~ z1, IRT.param = TRUE)
    ## 
    ## Coefficients:
    ##         Dffclt  Dscrmn
    ## Item 1   0.170   4.453
    ## Item 2  -0.236   4.323
    ## Item 3  -0.343   5.664
    ## Item 4  -0.316   3.625
    ## 
    ## Log.Lik: -706.337

For example, item 1 is relatively easier, and individuals with lower
levels of the latent trait are likely to endorse it and has moderate
discrimination, indicating it can distinguish between individuals with
different levels of the latent trait.

Among these items, Item 1 with a positive difficulty of 0.170 suggests
that individuals with lower levels of the latent trait (z1) are more
likely to endorse this item, indicating a more positive attitude towards
abortion.

Item 3 with a discrimination parameter of 5.664 has the highest
discriminatory power. This means that Item 3 is the most effective in
distinguishing between individuals with different attitudes towards
abortion.

Item 4 has the lowest discrimination parameter of 3.625. This suggests
that Item 4 is less effective in discriminating between individuals with
different attitudes towards abortion compared to the other items.

We can also set a different reparametrization of the model

``` r
irt_model_rep <- ltm(Abortion~z1 , IRT.param = FALSE)
irt_model_rep
```

    ## 
    ## Call:
    ## ltm(formula = Abortion ~ z1, IRT.param = FALSE)
    ## 
    ## Coefficients:
    ##         (Intercept)     z1
    ## Item 1       -0.756  4.453
    ## Item 2        1.021  4.323
    ## Item 3        1.942  5.664
    ## Item 4        1.147  3.625
    ## 
    ## Log.Lik: -706.337

``` r
summary(irt_model)$coefficients
```

    ##                    value    std.err    z.vals
    ## Dffclt.Item 1  0.1696615 0.06589159  2.574858
    ## Dffclt.Item 2 -0.2362298 0.06184429 -3.819752
    ## Dffclt.Item 3 -0.3428040 0.06713510 -5.106182
    ## Dffclt.Item 4 -0.3164814 0.06537359 -4.841119
    ## Dscrmn.Item 1  4.4531787 1.02447164  4.346805
    ## Dscrmn.Item 2  4.3226282 0.68244979  6.333987
    ## Dscrmn.Item 3  5.6639280 0.99570697  5.688348
    ## Dscrmn.Item 4  3.6253832 0.55700315  6.508730

``` r
irt_model_rep$coefficients
```

    ##        (Intercept)       z1
    ## Item 1  -0.7555329 4.453179
    ## Item 2   1.0211338 4.322628
    ## Item 3   1.9416173 5.663928
    ## Item 4   1.1473662 3.625383

Now, we compute the standardized discriminant parameters which are
defined as

$\alpha_{ji}(st) = \frac{\alpha_{ij}}{\sqrt{1+\sum{\alpha_{ij}^2}}}$

``` r
alpha <- irt_model_rep$coeff[,2]
alpha_st <- alpha/sqrt(1+alpha^2)
alpha_st
```

    ##    Item 1    Item 2    Item 3    Item 4 
    ## 0.9757019 0.9742691 0.9847692 0.9639998

To compute the probability of a positive response of the median
indvidual is sufficient to apply the following equation

$\pi_i(0) = \frac{1}{1 + exp(-\alpha_{i0})}$

where $\alpha_{i0}$ are the difficulty parameters

``` r
diff_par <- irt_model_rep$coeff[,1]
pi_i0 <- 1/(1+exp(-diff_par))
pi_i0
```

    ##    Item 1    Item 2    Item 3    Item 4 
    ## 0.3196169 0.7351934 0.8745297 0.7590295

Another way to compute the probability of positive response of the
median individual is to use coef function

``` r
coef(irt_model_rep,prob=TRUE)
```

    ##        (Intercept)       z1 P(x=1|z=0)
    ## Item 1  -0.7555329 4.453179  0.3196169
    ## Item 2   1.0211338 4.322628  0.7351934
    ## Item 3   1.9416173 5.663928  0.8745297
    ## Item 4   1.1473662 3.625383  0.7590295

### Item characteristic curve

The items characteristic curves are non decreasing monoting functions
denoted as $\pi_i(y) = P(x_i=1|y)$ with $0 \leq \pi_i(y) \leq 1$

To represent them, is sufficient to plot the model as below

``` r
plot(irt_model, legend=TRUE, xlab="Attitude toward abortion")
```

![](ex1_lab2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> The curves
are sigmoid-shaped, reflecting the typical shape of logistic functions.

#### Interpretation

The slope of the curve at any given point represents the discrimination
of the item. A steeper slope indicates higher discrimination, meaning
that the item is better at distinguishing between individuals with
different levels of the latent trait. As we already seen when we
discussed about the coefficients, the item 3 is the steepest one. A
shift in the curve along the x-axis corresponds to a shift in the
difficulty of the item. A curve shifted to the right indicates an
increase in difficulty, while a shift to the left indicates a decrease
in difficulty.

### Goodness of fit

To evaluate the goodness of fit we can use the following measure:

$R = \frac{(O-E)^2}{E}$ where O is the observed frequencies for any
marginal probability and E is the corresponding expected frequency.

With the fitted functionwe can compute the expected frequency of the
different response pattern

``` r
fitted(irt_model)
```

    ##       Item 1 Item 2 Item 3 Item 4         Exp
    ##  [1,]      0      0      0      0 103.4122369
    ##  [2,]      0      0      0      1  15.5202810
    ##  [3,]      0      0      1      0  11.1475316
    ##  [4,]      0      0      1      1  14.3156121
    ##  [5,]      0      1      0      0   8.9141331
    ##  [6,]      0      1      0      1   6.8099834
    ##  [7,]      0      1      1      0  12.0534014
    ##  [8,]      0      1      1      1  41.8185891
    ##  [9,]      1      0      0      0   1.3985312
    ## [10,]      1      0      1      1   7.1931090
    ## [11,]      1      1      0      0   0.8956743
    ## [12,]      1      1      0      1   2.5554663
    ## [13,]      1      1      1      0   7.1052145
    ## [14,]      1      1      1      1 142.7076472

We can see that we have 14 different possible patterns (from the
theoretical point we should expected 16, since we have 4 items and
$2^4=16$). What we need is just the last column

``` r
E <- fitted(irt_model)[,5]
E
```

    ##  [1] 103.4122369  15.5202810  11.1475316  14.3156121   8.9141331   6.8099834
    ##  [7]  12.0534014  41.8185891   1.3985312   7.1931090   0.8956743   2.5554663
    ## [13]   7.1052145 142.7076472

To get the observed frequencies for any marginal probability, it’s
sufficient to see retrieve this value from the model

``` r
irt_model$patterns
```

    ## $X
    ##       [,1] [,2] [,3] [,4]
    ##  [1,]    0    0    0    0
    ##  [2,]    0    0    0    1
    ##  [3,]    0    0    1    0
    ##  [4,]    0    0    1    1
    ##  [5,]    0    1    0    0
    ##  [6,]    0    1    0    1
    ##  [7,]    0    1    1    0
    ##  [8,]    0    1    1    1
    ##  [9,]    1    0    0    0
    ## [10,]    1    0    1    1
    ## [11,]    1    1    0    0
    ## [12,]    1    1    0    1
    ## [13,]    1    1    1    0
    ## [14,]    1    1    1    1
    ## 
    ## $obs
    ##  [1] 103  13  10  21   9   6   7  44   1   6   3   3  12 141

And since we need just the marginal observations

``` r
O <- irt_model$patterns$obs
```

Now, it’s sufficient to apply the formula

``` r
R <- sum((E-O)^2/E)
R
```

    ## [1] 14.70499

We can create a dataframe where we compare the observed pattern and the
expected one

``` r
comparison <- as.data.frame(cbind(irt_model$pattern$X,irt_model$pattern$obs,E))
```

For example, the pattern $(0,0,0,0)$ is observed 103 and the expectation
is 103.41; but, the pattern $(0,1,1,0)$ is observed 7 times and expected
12.05.

To compute a p-value we can use the $\chi^2$ distribution: when $n$ is
large and $p$ is small the test statistic in fact follows a $\chi^2$
distribution with degrees of freedom $\nu = 2^{p}- p(q+1)-1$

``` r
dof <- length(irt_model$patterns$obs) - 2*2 -1
dof
```

    ## [1] 9

With pchisq function we can compute the p-value for the $\chi^2$
distribution

``` r
pval <- 1-pchisq(R,dof)
pval
```

    ## [1] 0.09936469

In this case, the p-value (0.09936469) is greater than the typical
significance level of 0.05. Therefore, you do not have enough evidence
to reject the null hypothesis. The result suggests that, based on the
chi-square test, there is not a significant difference between the
observed and expected response patterns. The model fits reasonably well.

## Margins

Instead of looking at the whole set of response patterns, we can look at
the two and three way margins. What we do is taking the variables two at
a time and compare the the observed and expected two-way margins

``` r
margins(irt_model)
```

    ## 
    ## Call:
    ## ltm(formula = Abortion ~ z1, IRT.param = TRUE)
    ## 
    ## Fit on the Two-Way Margins
    ## 
    ## Response: (0,0)
    ##   Item i Item j Obs    Exp (O-E)^2/E  
    ## 1      1      4 129 135.53      0.31  
    ## 2      2      3 117 121.46      0.16  
    ## 3      2      4 114 117.99      0.13  
    ## 
    ## Response: (1,0)
    ##   Item i Item j Obs   Exp (O-E)^2/E  
    ## 1      1      2   7 11.74      1.92  
    ## 2      1      4  16 11.43      1.83  
    ## 3      3      4  29 32.33      0.34  
    ## 
    ## Response: (0,1)
    ##   Item i Item j Obs   Exp (O-E)^2/E  
    ## 1      3      4  22 26.01      0.62  
    ## 2      1      4  84 78.46      0.39  
    ## 3      1      2  66 69.60      0.19  
    ## 
    ## Response: (1,1)
    ##   Item i Item j Obs    Exp (O-E)^2/E  
    ## 1      1      2 159 153.26      0.21  
    ## 2      3      4 212 206.03      0.17  
    ## 3      1      4 150 153.58      0.08

``` r
margins(irt_model, type="three-way", nprint=2)
```

    ## 
    ## Call:
    ## ltm(formula = Abortion ~ z1, IRT.param = TRUE)
    ## 
    ## Fit on the Three-Way Margins
    ## 
    ## Response: (0,0,0)
    ##   Item i Item j Item k Obs    Exp (O-E)^2/E  
    ## 1      1      2      3 116 118.93      0.07  
    ## 2      1      2      4 113 114.56      0.02  
    ## 
    ## Response: (1,0,0)
    ##   Item i Item j Item k Obs  Exp (O-E)^2/E  
    ## 1      1      2      4   1 3.43      1.72  
    ## 2      1      3      4   4 2.29      1.27  
    ## 
    ## Response: (0,1,0)
    ##   Item i Item j Item k Obs   Exp (O-E)^2/E  
    ## 1      1      3      4  17 23.20      1.66  
    ## 2      1      2      4  16 20.97      1.18  
    ## 
    ## Response: (1,1,0)
    ##   Item i Item j Item k Obs  Exp (O-E)^2/E    
    ## 1      1      2      4  15 8.00      6.12 ***
    ## 2      1      2      3   6 3.45      1.88    
    ## 
    ## Response: (0,0,1)
    ##   Item i Item j Item k Obs   Exp (O-E)^2/E  
    ## 1      1      2      3  31 25.46       1.2  
    ## 2      2      3      4  13 16.65       0.8  
    ## 
    ## Response: (1,0,1)
    ##   Item i Item j Item k Obs  Exp (O-E)^2/E  
    ## 1      1      2      3   6 9.22      1.12  
    ## 2      1      2      4   6 8.32      0.65  
    ## 
    ## Response: (0,1,1)
    ##   Item i Item j Item k Obs   Exp (O-E)^2/E  
    ## 1      2      3      4  27 21.51       1.4  
    ## 2      1      3      4  65 56.13       1.4  
    ## 
    ## Response: (1,1,1)
    ##   Item i Item j Item k Obs    Exp (O-E)^2/E  
    ## 1      1      2      3 153 149.81      0.07  
    ## 2      1      3      4 147 149.90      0.06  
    ## 
    ## '***' denotes a chi-squared residual greater than 3.5

``` r
margins(irt_model, type="three-way", nprint=3)
```

    ## 
    ## Call:
    ## ltm(formula = Abortion ~ z1, IRT.param = TRUE)
    ## 
    ## Fit on the Three-Way Margins
    ## 
    ## Response: (0,0,0)
    ##   Item i Item j Item k Obs    Exp (O-E)^2/E  
    ## 1      1      2      3 116 118.93      0.07  
    ## 2      1      2      4 113 114.56      0.02  
    ## 3      2      3      4 104 104.81      0.01  
    ## 
    ## Response: (1,0,0)
    ##   Item i Item j Item k Obs  Exp (O-E)^2/E  
    ## 1      1      2      4   1 3.43      1.72  
    ## 2      1      3      4   4 2.29      1.27  
    ## 3      1      2      3   1 2.52      0.92  
    ## 
    ## Response: (0,1,0)
    ##   Item i Item j Item k Obs   Exp (O-E)^2/E  
    ## 1      1      3      4  17 23.20      1.66  
    ## 2      1      2      4  16 20.97      1.18  
    ## 3      2      3      4  10 13.17      0.77  
    ## 
    ## Response: (1,1,0)
    ##   Item i Item j Item k Obs  Exp (O-E)^2/E    
    ## 1      1      2      4  15 8.00      6.12 ***
    ## 2      1      2      3   6 3.45      1.88    
    ## 3      1      3      4  12 9.13      0.90    
    ## 
    ## Response: (0,0,1)
    ##   Item i Item j Item k Obs   Exp (O-E)^2/E  
    ## 1      1      2      3  31 25.46      1.20  
    ## 2      2      3      4  13 16.65      0.80  
    ## 3      1      2      4  34 29.84      0.58  
    ## 
    ## Response: (1,0,1)
    ##   Item i Item j Item k Obs  Exp (O-E)^2/E  
    ## 1      1      2      3   6 9.22      1.12  
    ## 2      1      2      4   6 8.32      0.65  
    ## 3      1      3      4   3 3.68      0.13  
    ## 
    ## Response: (0,1,1)
    ##   Item i Item j Item k Obs   Exp (O-E)^2/E  
    ## 1      2      3      4  27 21.51      1.40  
    ## 2      1      3      4  65 56.13      1.40  
    ## 3      1      2      3  51 53.87      0.15  
    ## 
    ## Response: (1,1,1)
    ##   Item i Item j Item k Obs    Exp (O-E)^2/E  
    ## 1      1      2      3 153 149.81      0.07  
    ## 2      1      3      4 147 149.90      0.06  
    ## 3      1      2      4 144 145.26      0.01  
    ## 
    ## '***' denotes a chi-squared residual greater than 3.5

### Factor scores

Factor scores are measures of the posterior distribution $p(z|x)$ where
$z$ is the vector of latent variables and $x$ the vector of manifest
variables: to compute this we use the function factor.scores

``` r
fact_scores <- factor.scores(irt_model, method="EAP")
fact_scores
```

    ## 
    ## Call:
    ## ltm(formula = Abortion ~ z1, IRT.param = TRUE)
    ## 
    ## Scoring Method: Expected A Posteriori
    ## 
    ## Factor-Scores for observed response patterns:
    ##    Item 1 Item 2 Item 3 Item 4 Obs     Exp     z1 se.z1
    ## 1       0      0      0      0 103 103.412 -1.163 0.550
    ## 2       0      0      0      1  13  15.520 -0.643 0.285
    ## 3       0      0      1      0  10  11.148 -0.449 0.337
    ## 4       0      0      1      1  21  14.316 -0.085 0.244
    ## 5       0      1      0      0   9   8.914 -0.585 0.297
    ## 6       0      1      0      1   6   6.810 -0.188 0.310
    ## 7       0      1      1      0   7  12.053 -0.048 0.214
    ## 8       0      1      1      1  44  41.819  0.121 0.280
    ## 9       1      0      0      0   1   1.399 -0.573 0.301
    ## 10      1      0      1      1   6   7.193  0.131 0.288
    ## 11      1      1      0      0   3   0.896 -0.118 0.270
    ## 12      1      1      0      1   3   2.555  0.047 0.216
    ## 13      1      1      1      0  12   7.105  0.199 0.334
    ## 14      1      1      1      1 141 142.708  0.961 0.602

The factor scores are on a continuous scale, and higher values indicate
a more positive attitude towards abortion. For example, the last pattern
that contains only 1’s has the highest value: observations that shows
this pattern are the ones were the attitude over the abortion is the
most positive one.

Another method used is the “Component” one

``` r
Comp <- factor.scores(irt_model, method ="Component")
Comp
```

    ## 
    ## Call:
    ## ltm(formula = Abortion ~ z1, IRT.param = TRUE)
    ## 
    ## Scoring Method: Component
    ## 
    ## Factor-Scores for observed response patterns:
    ##    Item 1 Item 2 Item 3 Item 4 Obs     Exp     z1
    ## 1       0      0      0      0 103 103.412  0.000
    ## 2       0      0      0      1  13  15.520  3.625
    ## 3       0      0      1      0  10  11.148  5.664
    ## 4       0      0      1      1  21  14.316  9.289
    ## 5       0      1      0      0   9   8.914  4.323
    ## 6       0      1      0      1   6   6.810  7.948
    ## 7       0      1      1      0   7  12.053  9.987
    ## 8       0      1      1      1  44  41.819 13.612
    ## 9       1      0      0      0   1   1.399  4.453
    ## 10      1      0      1      1   6   7.193 13.742
    ## 11      1      1      0      0   3   0.896  8.776
    ## 12      1      1      0      1   3   2.555 12.401
    ## 13      1      1      1      0  12   7.105 14.440
    ## 14      1      1      1      1 141 142.708 18.065

Similar interpretation can be done for this method.

### Summarize

Now to summarize, let’s create a dataframe with all of those
information. We want to see for every response pattern: the total score,
i.e. the sum of ones of the pattern; then, let’s see the obs vs expected
and the factor scores with their standard deviation.

``` r
resp.pattern <- fact_scores$score.dat[,1:4]
total.score <- apply(resp.pattern,1,sum)
total.score
```

    ##  [1] 0 1 1 2 1 2 2 3 1 3 2 3 3 4

``` r
cp <- Comp$score.dat[,7]
tab <- cbind(fact_scores$score.dat,cp,total.score)
round(tab[order(total.score),],3)
```

    ##    Item 1 Item 2 Item 3 Item 4 Obs     Exp     z1 se.z1     cp total.score
    ## 1       0      0      0      0 103 103.412 -1.163 0.550  0.000           0
    ## 2       0      0      0      1  13  15.520 -0.643 0.285  3.625           1
    ## 3       0      0      1      0  10  11.148 -0.449 0.337  5.664           1
    ## 5       0      1      0      0   9   8.914 -0.585 0.297  4.323           1
    ## 9       1      0      0      0   1   1.399 -0.573 0.301  4.453           1
    ## 4       0      0      1      1  21  14.316 -0.085 0.244  9.289           2
    ## 6       0      1      0      1   6   6.810 -0.188 0.310  7.948           2
    ## 7       0      1      1      0   7  12.053 -0.048 0.214  9.987           2
    ## 11      1      1      0      0   3   0.896 -0.118 0.270  8.776           2
    ## 8       0      1      1      1  44  41.819  0.121 0.280 13.612           3
    ## 10      1      0      1      1   6   7.193  0.131 0.288 13.742           3
    ## 12      1      1      0      1   3   2.555  0.047 0.216 12.401           3
    ## 13      1      1      1      0  12   7.105  0.199 0.334 14.440           3
    ## 14      1      1      1      1 141 142.708  0.961 0.602 18.065           4

## Response pattern not observed

In the dataset there are two patterns that do not occure,
i.e. $(1,0,0,1)$ and $(1,0,1,0)$: let’s check the estimate of the
abilities of those patterns

``` r
fitted(irt_model, resp.pattern=rbind(c(1,0,0,1),c(1,0,1,0)))
```

    ##      Item 1 Item 2 Item 3 Item 4      Exp
    ## [1,]      1      0      0      1 1.125171
    ## [2,]      1      0      1      0 2.027418

These estimates represent the model’s prediction of the latent trait
value for individuals with the specified response patterns, even though
those patterns were not observed in the actual dataset. It provides an
indication of how the model would expect individuals with these
unobserved response patterns to score on the latent trait.
