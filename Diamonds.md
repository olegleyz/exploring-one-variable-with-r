Exploratory Data Analysis of Dimonds data set
================

Introduction
------------

In this report I'm going to work with the data set, comtaining the prices and other attributes of almost 54,000 diamonds. The data set comes with ggplot2 library.

``` r
library(ggplot2)
dim(diamonds)
```

    ## [1] 53940    10

``` r
str(diamonds)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    53940 obs. of  10 variables:
    ##  $ carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
    ##  $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
    ##  $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
    ##  $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
    ##  $ depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
    ##  $ table  : num  55 61 65 58 58 57 57 55 61 61 ...
    ##  $ price  : int  326 326 327 334 335 336 336 337 337 338 ...
    ##  $ x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
    ##  $ y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
    ##  $ z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...

Notes:
\* There are 53940 observations in the data set, 10 variables, 3 ordered factors.
\* According to <http://www.lumeradiamonds.com/>, the less body color in a white diamond, the more true color it will reflect, and thus the greater its value - D.

Diamonds price histogram
------------------------

The diamond price histogram from the diamonds data set

``` r
qplot(data = diamonds, x = price, binwidth = 250,
      color = I('black'), fill = I('#F79420'),
      xlab = "Diamonds' price in $",
      ylab = "Count of diamonds") + 
  scale_x_continuous(breaks = seq(0,20000, 2000))
```

![](Diamonds_files/figure-markdown_github/pressure-1.png)

Notes:
\* The diamond price distribution is positively skewed, has a peak (mode) around 800$ and long right tail.
\* The median of the distribution is shifted to the left and equal to 1838\(. Mean is 3170\).
\* Min diamond price is 357$ and max is 18690$.

Diamonds price explorations
---------------------------

``` r
#How many diamonds cost less than $500?
NROW(subset(diamonds,price<500))
```

    ## [1] 1729

``` r
#How many diamonds cost less than $250?
NROW(subset(diamonds,price<250))
```

    ## [1] 0

``` r
#How many diamonds cost $15000 and more?
NROW(subset(diamonds,price>=15000))
```

    ## [1] 1656

Exploring the largest peak in the price histogram
-------------------------------------------------

``` r
library(gridExtra)
q <- qplot(data = diamonds, x = price, binwidth = 1,
      color = I('black'), fill = I('#F79420'),
      xlab = "Diamonds' price in $",
      ylab = "Count") 
q2 <- q + scale_x_continuous(breaks = seq(0,3000, 100), limits = c(350,2000))
ggsave('priceHistogram1.png')
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 29750 rows containing non-finite values (stat_bin).

``` r
q3 <- q + scale_x_continuous(breaks = seq(0,3000, 50), limits = c(575,825))
ggsave('priceHistogram2.png')
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 46966 rows containing non-finite values (stat_bin).

``` r
#exploring the price range with no diamonds
q4 <- q + scale_x_continuous(breaks = seq(0,3000, 50), limits = c(1400,1600))
ggsave('priceHistogram3.png')
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 52871 rows containing non-finite values (stat_bin).

``` r
suppressWarnings(grid.arrange(q2,q3,q4,ncol=1))
```

![](Diamonds_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
ggsave('priceHistogram.png')
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 52871 rows containing non-finite values (stat_bin).

Notes:
\* The largest peak of diamonds' price distribution is in the range from 500$ to 850\(. * Most of the diamonds have the price a bit more than 600\). To be clarified further.
\* There is a price range between 1450$ and 1550$ when we don't have any diamond.

Top 3 diamonds' prices:

``` r
head(sort(table(diamonds$price), decreasing = T),n = 3)
```

    ## 
    ## 605 802 625 
    ## 132 127 126

1.  132 diamonds with the price 605$
2.  127 diamonds for 802$
3.  126 diamonds for 625$

Exploring the price range without diamonds

``` r
table(subset(diamonds$price, diamonds$price > 1450 & diamonds$price < 1550))
```

    ## 
    ## 1451 1452 1453 1454 1546 1547 1548 1549 
    ##    4    4    5    3   21    9    7    6

There are no diamonds in price range from 1455$ till 1545$

The histogram of diamond prices broken up by cut
------------------------------------------------

``` r
q + facet_wrap(~cut, scales = "free_y")
```

![](Diamonds_files/figure-markdown_github/unnamed-chunk-5-1.png) \#\#The histogram of price per carat and by cut facet

``` r
qplot(data = diamonds, x = price/carat, binwidth = 0.025,
      xlab = "Price per carat, $",
      ylab = "Counts") +
  facet_wrap(~cut, scales = "free_y") +
  scale_x_log10()
```

![](Diamonds_files/figure-markdown_github/unnamed-chunk-6-1.png)

Notes:
\* there are still the diamonds with the not-ideal cut and huge price
\* there not many "cheap" diamonds with fair cut, but better is cut, more "cheap" diamonds

``` r
by(diamonds$price,diamonds$cut,summary,digits = max(getOption('digits')))
```

    ## diamonds$cut: Fair
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   337.000  2050.250  3282.000  4358.758  5205.500 18574.000 
    ## -------------------------------------------------------- 
    ## diamonds$cut: Good
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   327.000  1145.000  3050.500  3928.864  5028.000 18788.000 
    ## -------------------------------------------------------- 
    ## diamonds$cut: Very Good
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   336.00   912.00  2648.00  3981.76  5372.75 18818.00 
    ## -------------------------------------------------------- 
    ## diamonds$cut: Premium
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   326.000  1046.000  3185.000  4584.258  6296.000 18823.000 
    ## -------------------------------------------------------- 
    ## diamonds$cut: Ideal
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   326.000   878.000  1810.000  3457.542  4678.500 18806.000

Notes:
\* The lowest price have diamonds with Premium and Ideal cuts
\* The highest price have diamonds with Premium cut \* The lowest median price have diamonds with Ideal cut (surprise)

Boxplot by clarity, color and cut
---------------------------------

Visualizing the box plots, I would like to understand how the price per carat depends from the clarity, color and cut. I expect to see that better clarity (IF - good vs. I1 - bad), better color (D - good vs. J - bad) and better cut causes higher price median.

``` r
b1 <- qplot(data = diamonds, x = clarity, y = price/carat,
      xlab = 'Clarity',
      ylab = 'Price',
      geom = 'boxplot',
      fill=clarity) +
  coord_cartesian(ylim=c(350,18700))
ggsave("boxplot-price-clarity.png")
```

    ## Saving 7 x 5 in image

``` r
b2 <- qplot(data = diamonds, x = color, y = price/carat,
      xlab = 'Color',
      ylab = 'Price',
      geom = 'boxplot',
      fill=color) +
  coord_cartesian(ylim=c(350,18700))
ggsave("boxplot-price-color.png")
```

    ## Saving 7 x 5 in image

``` r
b3 <- qplot(data = diamonds, x = cut, y = price/carat,
      xlab = 'Cut',
      ylab = 'Price',
      geom = 'boxplot',
      fill=cut) +
  coord_cartesian(ylim=c(350,18700))
b1
```

![](Diamonds_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
b2
```

![](Diamonds_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
b3
```

![](Diamonds_files/figure-markdown_github/unnamed-chunk-8-3.png)

``` r
ggsave("boxplot-price-cut.png")
```

    ## Saving 7 x 5 in image

Against expectations, looking on price - clarity boxplot, median price for the one of the worst clarity level SI2 is higher than IF clarity. At the same time, I can see that the most expensive diamonds with price per carat higher than 15000$ has just IF - best - clarity level and the maximum price - clarity has relatively high linear correlation.

Let's add color on the same plot and we can see something interesting:

``` r
qplot(data = diamonds, x = clarity, y = price/carat,
      xlab = 'Clarity',
      ylab = 'Price',
      geom = 'boxplot',
      fill=color) +
  coord_cartesian(ylim=c(350,18700))
```

![](Diamonds_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggsave("boxplot-price-clarity-color.png")
```

    ## Saving 7 x 5 in image

The diamonds with the best color and clarity have the highest price per carat. Let's check how the plot will change if we add cut instead of color:

``` r
qplot(data = diamonds, x = clarity, y = price/carat,
      xlab = 'Clarity',
      ylab = 'Price',
      geom = 'boxplot',
      fill=cut)
```

![](Diamonds_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
ggsave("boxplot-price-clarity-cut.png")
```

    ## Saving 7 x 5 in image

The picture is not so obvious for best clarity and color but there is a linear regression of price clarity - cut

Measures of central tendency
----------------------------

``` r
by(diamonds$price,diamonds$color,summary,digits = max(getOption('digits')))
```

    ## diamonds$color: D
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   357.000   911.000  1838.000  3169.954  4213.500 18693.000 
    ## -------------------------------------------------------- 
    ## diamonds$color: E
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   326.000   882.000  1739.000  3076.752  4003.000 18731.000 
    ## -------------------------------------------------------- 
    ## diamonds$color: F
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   342.000   982.000  2343.500  3724.886  4868.250 18791.000 
    ## -------------------------------------------------------- 
    ## diamonds$color: G
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   354.000   931.000  2242.000  3999.136  6048.000 18818.000 
    ## -------------------------------------------------------- 
    ## diamonds$color: H
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   337.000   984.000  3460.000  4486.669  5980.250 18803.000 
    ## -------------------------------------------------------- 
    ## diamonds$color: I
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   334.000  1120.500  3730.000  5091.875  7201.750 18823.000 
    ## -------------------------------------------------------- 
    ## diamonds$color: J
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##   335.000  1860.500  4234.000  5323.818  7695.000 18710.000

-   The price range for the middle 50% of diamonds with color D:
-   first quartile (25%): 911$
-   third quartile (75%): 4213.5$
-   The price range for the middle 50% of diamonds with color J:
-   first quartile (25%): 1860.5$
-   third quartile (75%): 7695$
-   IQR for diamonds with the best color: 3302.50
-   IQR for diamonds with the worst color: 5834.50

Diamond's size exploration
--------------------------

Carat Frequency Polygon

``` r
qplot(data = diamonds, x = carat, y = ..count.., 
      binwidth = 0.01,
      geom = "freqpoly") +
  scale_x_continuous(limits = c(0, 2.2), breaks = seq(0, 2.2, 0.1))
```

    ## Warning: Removed 462 rows containing non-finite values (stat_bin).

    ## Warning: Removed 2 rows containing missing values (geom_path).

![](Diamonds_files/figure-markdown_github/unnamed-chunk-12-1.png)

Most of the diamonds have the size 0.3 carat

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
