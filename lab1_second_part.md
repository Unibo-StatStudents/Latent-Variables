lab1_second_part
================
2024-01-06

\#Rotation In this section we are going to deal with rotations

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

``` r
data <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9")]
```

We fit a model with three latent variables

``` r
f3<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,
             data=data,
             3)
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

Now, let’s explore different rotation methods to enhance the
interpretability of the factor loadings. We are going to use the
“GPArotation” library:

``` r
library(GPArotation)
```

Varimax rotation is a technique that aims to simplify the factor
structure by maximizing the variance of the squared loadings within each
factor. It often results in more interpretable and easily
distinguishable factors

``` r
Varimax(loadings(f3))
```

    ## Orthogonal rotation method varimax converged.
    ## Loadings:
    ##    Factor1 Factor2 Factor3
    ## x1  0.3202  0.1301  0.6066
    ## x2  0.1353 -0.0409  0.4809
    ## x3  0.0795  0.1133  0.6619
    ## x4  0.8379  0.0767  0.1131
    ## x5  0.8667  0.0703  0.0323
    ## x6  0.8151  0.0658  0.1617
    ## x7  0.1019  0.6954 -0.0624
    ## x8  0.0776  0.7036  0.1744
    ## x9  0.1699  0.5106  0.4088
    ## 
    ##                Factor1 Factor2 Factor3
    ## SS loadings      2.290   1.286   1.279
    ## Proportion Var   0.254   0.143   0.142
    ## Cumulative Var   0.254   0.397   0.539

Quartimax rotation is another technique that simplifies the factor
structure by maximizing the variance of the loadings squared across all
factors. It tends to produce factors with fewer variables loading on
each factor.

``` r
quartimax(loadings(f3)) 
```

    ## Orthogonal rotation method Quartimax converged.
    ## Loadings:
    ##    Factor1 Factor2 Factor3
    ## x1   0.353  0.1244  0.5897
    ## x2   0.158 -0.0423  0.4740
    ## x3   0.115  0.1140  0.6566
    ## x4   0.844  0.0557  0.0719
    ## x5   0.869  0.0483 -0.0102
    ## x6   0.823  0.0456  0.1215
    ## x7   0.116  0.6923 -0.0710
    ## x8   0.104  0.7021  0.1666
    ## x9   0.202  0.5078  0.3974
    ## 
    ##                Factor1 Factor2 Factor3
    ## SS loadings      2.373   1.268   1.214
    ## Proportion Var   0.264   0.141   0.135
    ## Cumulative Var   0.264   0.404   0.539

Oblimin rotation is a method that allows for correlations between
factors, providing a more realistic representation of the underlying
structure.

``` r
oblimin(loadings(f3))
```

    ## Oblique rotation method Oblimin Quartimin converged.
    ## Loadings:
    ##    Factor1 Factor2  Factor3
    ## x1  0.1910  0.6020  0.03094
    ## x2  0.0437  0.5054 -0.11662
    ## x3 -0.0695  0.6893  0.02307
    ## x4  0.8405  0.0218  0.00531
    ## x5  0.8882 -0.0674  0.00756
    ## x6  0.8076  0.0775 -0.01093
    ## x7  0.0436 -0.1516  0.72310
    ## x8 -0.0327  0.1042  0.70150
    ## x9  0.0348  0.3661  0.46318
    ## 
    ##                Factor1 Factor2 Factor3
    ## SS loadings      2.237   1.338   1.280
    ## Proportion Var   0.249   0.149   0.142
    ## Cumulative Var   0.249   0.397   0.539
    ## 
    ## Phi:
    ##         Factor1 Factor2 Factor3
    ## Factor1   1.000   0.326   0.216
    ## Factor2   0.326   1.000   0.270
    ## Factor3   0.216   0.270   1.000

Now we are going to compute the factor scores using the Bartlett and the
Thompson method

### Bartlett

The Bartlett method for computing factor scores is based on Bartlett’s
criterion, which aims to minimize the sum of squared differences between
the observed variables and their estimated values using the factor
model: starting from the posterior distribution of $y|x$, we can define
the factor scores as follows

$\hat{f}= \Lambda'\Sigma^{-1}x$ as suggested in the help of factanal.
The equation is sligthly different in the notes and slides, but the
essence is the same.

``` r
f3_bartlett <-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,
             data=data,
             3,
             scores = "Bartlett")
f3_bartlett
```

    ## 
    ## Call:
    ## factanal(x = ~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, factors = 3,     data = data, scores = "Bartlett")
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

``` r
head(f3_bartlett$scores)
```

    ##      Factor1    Factor2    Factor3
    ## 1  0.1723415 -1.1223759  0.1040892
    ## 2 -1.4884456  0.7964855  1.1497504
    ## 3 -1.9859057 -0.0294077 -1.4704859
    ## 4 -0.1060737  1.2970855 -1.2229873
    ## 5 -0.0330453 -0.8590776  0.5779876
    ## 6 -1.8501262  0.5260152  1.6541725

Factor1: Represents the strength of the association of each observation
with the first latent factor. Factor2: Represents the strength of the
association of each observation with the second latent factor. Factor3:
Represents the strength of the association of each observation with the
third latent factor.

A positive score indicates that the observation has a positive
association with the corresponding factor. A negative score indicates a
negative association.

In summary, Larger absolute scores indicate a stronger association
between an observation and a specific latent factor, and the sign
indicates the direction of that association (positive or negative)

### Thompson

Similarly, the Thompson estimate the factor scores are obtained in a
Bayesian framework.

``` r
f3_thompson<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,
             data=data,
             3,
             scores = "regression")
f3_thompson
```

    ## 
    ## Call:
    ## factanal(x = ~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, factors = 3,     data = data, scores = "regression")
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

``` r
head(f3_thompson$scores)
```

    ##       Factor1    Factor2      Factor3
    ## 1  0.08247152 -0.7258616 -0.001591841
    ## 2 -1.21273720  0.5161999  0.822875023
    ## 3 -1.76841308 -0.2476878 -1.088698544
    ## 4 -0.04449707  0.7666871 -0.769782192
    ## 5 -0.06666738 -0.5306562  0.344127073
    ## 6 -1.53029546  0.3499646  1.148005610

We have similar interpretation with the regression method.

### Confirmatory factor analysis

In plain words, confirmatory factor analysis consist in two steps: in
the first step we make prior conjectures/hypotesis about the
relationships between observed variables and the latent factors.

So, for example, by looking the matrix correlation structure we say that
there can be three latent variables for the “visual”, “textual” and
“speed” variables of our dataset.

``` r
hs.model <- "visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed =~ x7 + x8 + x9"
```

Now, we use the cfa function to check if our prior beliefs are confirmed
by the latent factor analysis: here we assess how well the model fits
the observed data using fit indices and other model fit statistics. This
helps us to determine whether the proposed model is consistent with the
observed data.

``` r
fit <- cfa(hs.model, data = data)
summary(fit, fit.measures=TRUE)
```

    ## lavaan 0.6.16 ended normally after 35 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        21
    ## 
    ##   Number of observations                           301
    ## 
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                85.306
    ##   Degrees of freedom                                24
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               918.852
    ##   Degrees of freedom                                36
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.931
    ##   Tucker-Lewis Index (TLI)                       0.896
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -3737.745
    ##   Loglikelihood unrestricted model (H1)      -3695.092
    ##                                                       
    ##   Akaike (AIC)                                7517.490
    ##   Bayesian (BIC)                              7595.339
    ##   Sample-size adjusted Bayesian (SABIC)       7528.739
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.092
    ##   90 Percent confidence interval - lower         0.071
    ##   90 Percent confidence interval - upper         0.114
    ##   P-value H_0: RMSEA <= 0.050                    0.001
    ##   P-value H_0: RMSEA >= 0.080                    0.840
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.065
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     x1                1.000                           
    ##     x2                0.554    0.100    5.554    0.000
    ##     x3                0.729    0.109    6.685    0.000
    ##   textual =~                                          
    ##     x4                1.000                           
    ##     x5                1.113    0.065   17.014    0.000
    ##     x6                0.926    0.055   16.703    0.000
    ##   speed =~                                            
    ##     x7                1.000                           
    ##     x8                1.180    0.165    7.152    0.000
    ##     x9                1.082    0.151    7.155    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textual           0.408    0.074    5.552    0.000
    ##     speed             0.262    0.056    4.660    0.000
    ##   textual ~~                                          
    ##     speed             0.173    0.049    3.518    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .x1                0.549    0.114    4.833    0.000
    ##    .x2                1.134    0.102   11.146    0.000
    ##    .x3                0.844    0.091    9.317    0.000
    ##    .x4                0.371    0.048    7.779    0.000
    ##    .x5                0.446    0.058    7.642    0.000
    ##    .x6                0.356    0.043    8.277    0.000
    ##    .x7                0.799    0.081    9.823    0.000
    ##    .x8                0.488    0.074    6.573    0.000
    ##    .x9                0.566    0.071    8.003    0.000
    ##     visual            0.809    0.145    5.564    0.000
    ##     textual           0.979    0.112    8.737    0.000
    ##     speed             0.384    0.086    4.451    0.000

This fit indices suggests that there is room for improvement: in
particular, if we look at the test statistic Chi-squared is 85.306 with
24 degrees of freedom, yielding a p-value of 0.000. This suggests that
the model significantly deviates from the observed data. Moreover,CFI is
0.931, and TLI is 0.896. Values close to 1 indicate good fit, but these
indices are below 0.95, suggesting potential changes in the model.

### Orthogonalization

Now the latent factors are assumed to be orthogonal, meaning that they
are uncorrelated with each other.

``` r
fit_orthog <- cfa(hs.model, data = data, orthogonal=TRUE)
summary(fit_orthog, fit.measures=TRUE)
```

    ## lavaan 0.6.16 ended normally after 32 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        18
    ## 
    ##   Number of observations                           301
    ## 
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                               153.527
    ##   Degrees of freedom                                27
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               918.852
    ##   Degrees of freedom                                36
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.857
    ##   Tucker-Lewis Index (TLI)                       0.809
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -3771.856
    ##   Loglikelihood unrestricted model (H1)      -3695.092
    ##                                                       
    ##   Akaike (AIC)                                7579.711
    ##   Bayesian (BIC)                              7646.439
    ##   Sample-size adjusted Bayesian (SABIC)       7589.354
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.125
    ##   90 Percent confidence interval - lower         0.106
    ##   90 Percent confidence interval - upper         0.144
    ##   P-value H_0: RMSEA <= 0.050                    0.000
    ##   P-value H_0: RMSEA >= 0.080                    1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.161
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     x1                1.000                           
    ##     x2                0.778    0.141    5.532    0.000
    ##     x3                1.107    0.214    5.173    0.000
    ##   textual =~                                          
    ##     x4                1.000                           
    ##     x5                1.133    0.067   16.906    0.000
    ##     x6                0.924    0.056   16.391    0.000
    ##   speed =~                                            
    ##     x7                1.000                           
    ##     x8                1.225    0.190    6.460    0.000
    ##     x9                0.854    0.121    7.046    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textual           0.000                           
    ##     speed             0.000                           
    ##   textual ~~                                          
    ##     speed             0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .x1                0.835    0.118    7.064    0.000
    ##    .x2                1.065    0.105   10.177    0.000
    ##    .x3                0.633    0.129    4.899    0.000
    ##    .x4                0.382    0.049    7.805    0.000
    ##    .x5                0.416    0.059    7.038    0.000
    ##    .x6                0.369    0.044    8.367    0.000
    ##    .x7                0.746    0.086    8.650    0.000
    ##    .x8                0.366    0.097    3.794    0.000
    ##    .x9                0.696    0.072    9.640    0.000
    ##     visual            0.524    0.130    4.021    0.000
    ##     textual           0.969    0.112    8.640    0.000
    ##     speed             0.437    0.097    4.520    0.000

This model is even worse than the previous one.

Let’s see what happen if we base our prior conjectures on the
correlation matrix

``` r
heatmap(cor(data))
```

![](lab1_second_part_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
Instead of making hypotesis based on the meaning of the variables as we
just did previously, let base our conjectures on the correlation
structure

``` r
cor.model <- " first = ~ x1+ x2 + x3 + x9
               second =~ x4 + x5 + x6
               third =~ x7 + x8"
```

``` r
fit_cor_model <- cfa(cor.model, data=data)
```

    ## Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
    ## variances are negative

``` r
summary(fit_cor_model, fit.measures=TRUE)
```

    ## lavaan 0.6.16 ended normally after 38 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        21
    ## 
    ##   Number of observations                           301
    ## 
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                93.262
    ##   Degrees of freedom                                24
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               918.852
    ##   Degrees of freedom                                36
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.922
    ##   Tucker-Lewis Index (TLI)                       0.882
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -3741.723
    ##   Loglikelihood unrestricted model (H1)      -3695.092
    ##                                                       
    ##   Akaike (AIC)                                7525.446
    ##   Bayesian (BIC)                              7603.295
    ##   Sample-size adjusted Bayesian (SABIC)       7536.695
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.098
    ##   90 Percent confidence interval - lower         0.077
    ##   90 Percent confidence interval - upper         0.119
    ##   P-value H_0: RMSEA <= 0.050                    0.000
    ##   P-value H_0: RMSEA >= 0.080                    0.926
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.069
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   first =~                                            
    ##     x1                1.000                           
    ##     x2                0.598    0.103    5.820    0.000
    ##     x3                0.792    0.106    7.499    0.000
    ##     x9                0.743    0.096    7.734    0.000
    ##   second =~                                           
    ##     x4                1.000                           
    ##     x5                1.117    0.066   17.008    0.000
    ##     x6                0.927    0.056   16.663    0.000
    ##   third =~                                            
    ##     x7                1.000                           
    ##     x8                1.932    0.580    3.333    0.001
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   first ~~                                            
    ##     second            0.373    0.068    5.445    0.000
    ##     third             0.180    0.063    2.828    0.005
    ##   second ~~                                           
    ##     third             0.080    0.040    1.988    0.047
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .x1                0.684    0.089    7.656    0.000
    ##    .x2                1.140    0.101   11.303    0.000
    ##    .x3                0.852    0.086    9.946    0.000
    ##    .x9                0.643    0.067    9.543    0.000
    ##    .x4                0.374    0.048    7.815    0.000
    ##    .x5                0.441    0.058    7.560    0.000
    ##    .x6                0.357    0.043    8.282    0.000
    ##    .x7                0.906    0.107    8.442    0.000
    ##    .x8               -0.012    0.291   -0.042    0.966
    ##     first             0.674    0.118    5.690    0.000
    ##     second            0.977    0.112    8.716    0.000
    ##     third             0.277    0.100    2.782    0.005

Even with this different approach we don’t have the best fit of the
model, since the pvalue and the other statistics doesn’t support this
model.

Probably other considerations can be done, but we are already above what
was asked during the lab.
