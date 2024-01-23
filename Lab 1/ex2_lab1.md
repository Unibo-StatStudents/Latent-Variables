Exercise2_lab1
================
2024-01-06

The data are based on information provided by 713 male or female married
respondents to a survey carried out in 1949. The variables relate to the
respondent, his or his spouse, father, father-in-law, and firstborn son.
The file socmob.txt contains the full correlation matrix.

``` r
socmob <- read.table("~/Desktop/Università/Latent variables/Lab1-Classical factor analysis -20231226/socmob.txt", quote="\"", comment.char="")
```

The 10 variables are coded as follows: X1=Husband’s father’s
occupational status X2=Wife’s father’s occupational status X3=Husband’s
further education X4=Husband’s qualifications X5=Husband’s occupational
status X6=Wife’s further education X7=Wife’s qualifications
X8=Firstborn’s further education X9=Firstborn’s qualifications
X10=Firstborn’s occupational status

``` r
heatmap(as.matrix(socmob))
```

![](ex2_lab1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> From the
heatmap of the correlation matrix we can make some hypotesis: colors
helps to see that there are 4 groups of variables that shows higher
correlation among each others.

Group 1: $(V_1, V_2, V_5,V_10)$ Group 2: $(V_8,V_9)$ Group 3:
$(V_6,V_7)$ Group 4: $(V_3,V_4)$

This consideration leads to say that we can try to fit a model with four
latent variables

``` r
mod_4f <- factanal(covmat = as.matrix(socmob), factors = 4)
mod_4f
```

    ## 
    ## Call:
    ## factanal(factors = 4, covmat = as.matrix(socmob))
    ## 
    ## Uniquenesses:
    ##    V1    V2    V3    V4    V5    V6    V7    V8    V9   V10 
    ## 0.650 0.721 0.592 0.078 0.428 0.005 0.691 0.419 0.525 0.541 
    ## 
    ## Loadings:
    ##       Factor1 Factor2 Factor3 Factor4
    ##  [1,] 0.582                          
    ##  [2,] 0.512                          
    ##  [3,] 0.327   0.474   0.212   0.177  
    ##  [4,]         0.921   0.221   0.137  
    ##  [5,] 0.731   0.160                  
    ##  [6,] 0.148                   0.980  
    ##  [7,]         0.308   0.138   0.435  
    ##  [8,] 0.198   0.126   0.707   0.163  
    ##  [9,]         0.212   0.654          
    ## [10,] 0.494           0.452          
    ## 
    ##                Factor1 Factor2 Factor3 Factor4
    ## SS loadings      1.561   1.272   1.270   1.247
    ## Proportion Var   0.156   0.127   0.127   0.125
    ## Cumulative Var   0.156   0.283   0.410   0.535
    ## 
    ## The degrees of freedom for the model is 11 and the fit was 0.0236

Now that we fitted the model, let’s have a look to the loadings

``` r
mod_4f$loadings
```

    ## 
    ## Loadings:
    ##       Factor1 Factor2 Factor3 Factor4
    ##  [1,] 0.582                          
    ##  [2,] 0.512                          
    ##  [3,] 0.327   0.474   0.212   0.177  
    ##  [4,]         0.921   0.221   0.137  
    ##  [5,] 0.731   0.160                  
    ##  [6,] 0.148                   0.980  
    ##  [7,]         0.308   0.138   0.435  
    ##  [8,] 0.198   0.126   0.707   0.163  
    ##  [9,]         0.212   0.654          
    ## [10,] 0.494           0.452          
    ## 
    ##                Factor1 Factor2 Factor3 Factor4
    ## SS loadings      1.561   1.272   1.270   1.247
    ## Proportion Var   0.156   0.127   0.127   0.125
    ## Cumulative Var   0.156   0.283   0.410   0.535

We can say that the factor 1 has high values for $(V_1, V_2, V_5,V_10)$:
those variables are all related to the occupational status of the
different memebers of the family. Factor 1 -\> Occupational status

In the second factor, we see high values for the variables $V_3, V_4$:
those variables refer to the education and qualification of the husband.
Factor 2 -\> Husband credential

In the third factor, we have high values for the $V_8, V_9$: they are
related to the firstborn education and qualification. Factor 3 -\>
Firstborn credential

In the fourth factor, $V_6, V_7$ present high values related to the wife
qualification and. education. Factor 4 -\> Wife credential

Now that we spotted the latent variables and attached a meaning to them,
we can check the goodness of fit of the model.

## Goodness of fit

Here we use the scree plot as a visual check for the number of factors:
we didn’t do in class so you can skip it, but the rationale is very
similar to the checks on the explained variance and the proportion of
the explained variance that we did in some point.

This is a heuristic method where the eigenvalues represent the amount of
variance explained by each factor. A scree plot can help identify the
point at which eigenvalues level off, suggesting the optimal number of
factors.

``` r
eigenvalues <- eigen(socmob)$values
num_factors <- 4
# Scree plot
plot(1:10, eigenvalues, type = "b", main = "Scree Plot", 
     xlab = "Factor Number", ylab = "Eigenvalue")

# Add a line at 1 to indicate the 'elbow' of the scree plot
abline(h = 1, col = "red", lty = 2)
```

![](ex2_lab1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

We can check models with increasing latent variables to see if
increasing factors leads to a better variance explained: in a sense, we
just did by checking the eigenvalues before.

## Communalities

I create a for loop to fit a latent variable model with increasing
factors (the maximum number of latent variables is 6 because with 10
variables this is the maximum $q$ which factanal can fit)

``` r
model_list <- list()
for(i in 1:6){
  out <- factanal(covmat = as.matrix(socmob), factors = i)
  model_list[[i]] <- out
}
```

Then, we compute the communalities which are defined as the sum of the
squared loadings for each model.

``` r
communalities <- list()

for(i in 1:6){
  comm <- sum(rowSums(loadings(model_list[[i]])^2))
  communalities[[i]] <- comm
}
```

Now, we simply make a barplot where we show the increasing variance
explained by adding more and more variables.

``` r
# Extract numeric communalities from the list
numeric_communalities <- sapply(communalities, function(x) as.numeric(x))

# Factor model labels
factor_models <- c("Factor Model 1", "Factor Model 2", "Factor Model 3", "Factor Model 4", "Factor Model 5", "Factor Model 6")

# Create a bar plot
barplot(numeric_communalities, names.arg = factor_models, col = "green", main = "Communalities for Factor Models", ylab = "Communalities", ylim = c(0, max(numeric_communalities) + 1))
```

![](ex2_lab1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> Something
to notice although happen: when the model includes 6 latent variables,
the communalities decreases with respect to the model with 5 variables.
This is interesting because those models differs from the PCA analysis.
Indeed, in principal component analysis (PCA), increasing the number of
principal components does lead to explaining more variance in the data.
Each successive principal component captures the maximum amount of
remaining variance orthogonal to the previous components.

However, in factor analysis, especially in exploratory factor analysis
(EFA), the situation is a bit different. The communalities represent the
proportion of each variable’s variance that is accounted for by the
common factors. The number of latent factors can influence
communalities, but the relationship is not necessarily linear or
strictly increasing.

When you add more factors, you are allowing the model to capture more
complex patterns and relationships in the data. However, this doesn’t
guarantee that the additional factors will explain more variance in each
individual variable. If the data does not exhibit clear underlying
factor structure or if there are too many factors relative to the
complexity of the data, you might observe fluctuations or decreases in
communalities as you increase the number of factors.

### Final comments on the results

It’s not easy, since we don’t have a complete description of the
dataset, the survey and the background, to assess the meaning to the
factors founded in the exploratory factor analysis: although, we can say
that the most interesting model is the one with four variables. In fact,
we can check the reproduced correlation matrix, which is defined as

$repr_{corr} = \lambda_{ij}\lambda_{ij}'$

``` r
repr_corr_mod4 <- loadings(mod_4f)%*%t(loadings(mod_4f))
heatmap(as.matrix(socmob)-repr_corr_mod4)
```

![](ex2_lab1_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> The matrix,
outside the diagonal, looks very sparse: this means that the correlation
matrix obtained by the fitted model is very similar to the original
correlation matrix.

A sparse matrix indicates that the fitted model is a good model.

To check this claim, we can plot all the others reproduced correlation
matrix and the original matrix to see that the most sparse is the one
choosen.

``` r
for (i in 1:6){
  repr_corr_mat <-loadings(model_list[[i]])%*%t(loadings(model_list[[i]]))
  diff_matrix <- as.matrix(socmob) - repr_corr_mat
  heatmap(diff_matrix, main = paste("Heatmap of Residuals (Model", i, ")"))
}
```

![](ex2_lab1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->![](ex2_lab1_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->![](ex2_lab1_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->![](ex2_lab1_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->![](ex2_lab1_files/figure-gfm/unnamed-chunk-10-5.png)<!-- -->![](ex2_lab1_files/figure-gfm/unnamed-chunk-10-6.png)<!-- -->
