Notebook for chi-square test on observation moment
================

  - [Using 4 categories](#using-4-categories)
  - [Using 2 categories pre and post](#using-2-categories-pre-and-post)

# Using 4 categories

``` r
if(!require(rcompanion)){install.packages("rcompanion")}
```

    ## Loading required package: rcompanion

    ## Warning: package 'rcompanion' was built under R version 3.6.3

``` r
Input =("
Status     Healthy  Diseased
'1'         414       233
'2'         381       266
'3'         321       325
'4'         297       348
")

MatrixAll = as.matrix(read.table(textConnection(Input),
                   header=TRUE,
                   row.names=1))

chisq.test(MatrixAll)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  MatrixAll
    ## X-squared = 53.126, df = 3, p-value = 1.723e-11

``` r
pairwiseNominalIndependence(MatrixAll,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "bonferroni")
```

    ##   Comparison  p.Chisq p.adj.Chisq
    ## 1      1 : 2 6.76e-02    4.06e-01
    ## 2      1 : 3 2.84e-07    1.70e-06
    ## 3      1 : 4 1.31e-10    7.86e-10
    ## 4      2 : 3 1.10e-03    6.60e-03
    ## 5      2 : 4 4.98e-06    2.99e-05
    ## 6      3 : 4 2.10e-01    1.00e+00

# Using 2 categories pre and post

``` r
Input =("
Status     Healthy  Diseased
'Pre'         795       499
'Post'         618       673
")

MatrixPrePost = as.matrix(read.table(textConnection(Input),
                   header=TRUE,
                   row.names=1))

chisq.test(MatrixPrePost)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  MatrixPrePost
    ## X-squared = 47.455, df = 1, p-value = 5.627e-12

``` r
pairwiseNominalIndependence(MatrixPrePost,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "bonferroni")
```

    ##   Comparison  p.Chisq p.adj.Chisq
    ## 1 Pre : Post 5.63e-12    5.63e-12
