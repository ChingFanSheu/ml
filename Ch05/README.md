R在行為科學之應用:第五章
================
鄭中平、許清芳
2023 5月 01

``` r
if(!require("pacman")){
  install.packages("pacman", repos="https://cloud.r-project.org/")  
  library(pacman)
}
pacman::p_load(mlmRev, printr, gridExtra, tidyverse, optimx, lme4)
```

### 資料管理

``` r
#載入mlmRev套件
library(mlmRev)
```

``` r
#載進資料 
data(Chem97)
```

``` r
#檢視套件提供的資訊
?Chem97
```

    Scores on A-level Chemistry in 1997

    Description:

         Scores on the 1997 A-level Chemistry examination in Britain.
         Students are grouped into schools within local education
         authories.  In addition some demographic and pre-test information
         is provided.

    Usage:

         data(Chem97)
         
    Format:

         A data frame with 31022 observations on the following 8 variables.

         lea Local Education Authority - a factor

         school School identifier - a factor

         student Student identifier - a factor

         score Point score on A-level Chemistry in 1997

         gender Student's gender

         age Age in month, centred at 222 months or 18.5 years

         gcsescore Average GCSE score of individual.

         gcsecnt Average GCSE score of individual, centered at mean.

    Details:

         This data set is relatively large with 31,022 individuals in 2,280
         schools.  Note that while this is used, illustratively, to fit
         Normal response models, the distribution of the response is not
         well described by a Normal distribution.

    Source:

         <http://www.bristol.ac.uk/cmm/learning/mmsoftware/data-rev.html>

    References:

         Yang, M., Fielding, A. and Goldstein, H. (2002). Multilevel
         ordinal models for examination grades (submitted to _Statistical
         Modelling_).

    Examples:

         str(Chem97)
         summary(Chem97)
         (fm1 <- lmer(score ~ (1|school) + (1|lea), Chem97))
         (fm2 <- lmer(score ~ gcsecnt + (1|school) + (1|lea), Chem97))

``` r
#看資料結構
Chem97 |> str()
```

    'data.frame':   31022 obs. of  8 variables:
     $ lea      : Factor w/ 131 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
     $ school   : Factor w/ 2410 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
     $ student  : Factor w/ 31022 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
     $ score    : num  4 10 10 10 8 10 6 8 4 10 ...
     $ gender   : Factor w/ 2 levels "M","F": 2 2 2 2 2 2 2 2 2 2 ...
     $ age      : num  3 -3 -4 -2 -1 4 1 4 3 0 ...
     $ gcsescore: num  6.62 7.62 7.25 7.5 6.44 ...
     $ gcsecnt  : num  0.339 1.339 0.964 1.214 0.158 ...

``` r
#程式報表5.1
Chem97 |> head()
```

| lea | school | student | score | gender | age | gcsescore | gcsecnt |
|:----|:-------|:--------|------:|:-------|----:|----------:|--------:|
| 1   | 1      | 1       |     4 | F      |   3 |     6.625 |  0.3393 |
| 1   | 1      | 2       |    10 | F      |  -3 |     7.625 |  1.3393 |
| 1   | 1      | 3       |    10 | F      |  -4 |     7.250 |  0.9643 |
| 1   | 1      | 4       |    10 | F      |  -2 |     7.500 |  1.2143 |
| 1   | 1      | 5       |     8 | F      |  -1 |     6.444 |  0.1583 |
| 1   | 1      | 6       |    10 | F      |   4 |     7.750 |  1.4643 |

``` r
#去掉第六與第八個變項，只將需要的資料留下，並重新命名
#原始文章分析是以 GCSE 預測 Chem，我們預計倒過來做
dta <- Chem97[, -c(6, 8)]
names(dta) <- c("區域", "學校", "學生", "化學", "性別", "總成績")
```

``` r
#程式報表5.1
dta |> head()
```

| 區域 | 學校 | 學生 | 化學 | 性別 | 總成績 |
|:-----|:-----|:-----|-----:|:-----|-------:|
| 1    | 1    | 1    |    4 | F    |  6.625 |
| 1    | 1    | 2    |   10 | F    |  7.625 |
| 1    | 1    | 3    |   10 | F    |  7.250 |
| 1    | 1    | 4    |   10 | F    |  7.500 |
| 1    | 1    | 5    |    8 | F    |  6.444 |
| 1    | 1    | 6    |   10 | F    |  7.750 |

### 描述統計

``` r
#程式報表5.2
dta |> dplyr::select('化學', '總成績') |> sapply(summary)
```

|         |   化學 | 總成績 |
|:--------|-------:|-------:|
| Min.    |  0.000 |  0.000 |
| 1st Qu. |  4.000 |  5.750 |
| Median  |  6.000 |  6.375 |
| Mean    |  5.813 |  6.286 |
| 3rd Qu. |  8.000 |  6.900 |
| Max.    | 10.000 |  8.000 |

``` r
dta |> dplyr::select('化學', '總成績') |> sapply(sd)
```

      化學 總成績 
    3.3192 0.8735 

``` r
#計算各校化學與總成績平均分數，記錄成資料檔，留待後用
dta_m <- aggregate(cbind(化學, 總成績, as.numeric(性別) - 1) ~ 學校, 
                   data = dta, mean)
names(dta_m) <- c('學校', '化學平均', '總成績平均', '女性比率')
```

``` r
#程式報表5.3
dta_m |> head()
```

| 學校 | 化學平均 | 總成績平均 | 女性比率 |
|:-----|---------:|-----------:|---------:|
| 1    |    8.308 |      7.224 |   1.0000 |
| 2    |    8.714 |      7.024 |   0.0000 |
| 3    |    4.667 |      5.310 |   0.6667 |
| 4    |    8.286 |      6.232 |   0.1429 |
| 5    |    5.500 |      6.174 |   0.4375 |
| 6    |    4.714 |      6.705 |   0.7857 |

### 繪圖

``` r
#載進 lattice，準備畫圖
library(lattice)
```

``` r
#以總成績與化學原始分數，繪製直方圖
p1 <- histogram(~ 總成績, data = dta, type = 'density', 
                xlab = '總成績', ylab = '機率密度')
p2 <- histogram(~ 化學, data = dta, type = 'density', 
                xlab = '化學分數', ylab = '機率密度')
```

``` r
#以學校平均分數，繪製直方圖
p3 <- histogram(~ 總成績平均, data = dta_m, type = 'density', 
          xlab = '各校總成績平均', ylab = '機率密度')
p4 <- histogram(~ 化學平均, data = dta_m, type = 'density', 
          xlab = '各校化學平均分數', ylab = '機率密度')
```

``` r
#計算學校標準差，存檔留後用
dta_sd <- aggregate(cbind(化學, 總成績) ~ 學校, data = dta, sd)
names(dta_sd)[2:3] <- c('化學標準差', '總成績標準差')
```

``` r
#以學校標準差，繪製直方圖。
p5 <- histogram(~ 總成績標準差, data = dta_sd, type = 'density',
          xlab = '各校總成績標準差', ylab = '機率密度')
          
p6 <- histogram(~ 化學標準差, data = dta_sd, type = 'density',
          xlab = '各校化學分數標準差', ylab = '機率密度')
```

``` r
#載入gridExtra，把六張放一起
#圖 5.1
library(gridExtra)
grid.arrange(p6, p5, p4, p3, p2, p1, as.table = T)
```

<img src="Ch05_files/figure-gfm/fig5.1-1.png" style="display: block; margin: auto;" />

``` r
#看看化學與總成績相關，以學生層次分數計算是 .662
dta |> dplyr::select('化學', '總成績') |> cor()
```

|        |   化學 | 總成績 |
|:-------|-------:|-------:|
| 化學   | 1.0000 | 0.6622 |
| 總成績 | 0.6622 | 1.0000 |

``` r
#以學校層次分數計算生態相關（ecological correlation），為 .698
##程式報表5.4
dta_m |> dplyr::select(-c('學校')) |> cor() 
```

|            | 化學平均 | 總成績平均 | 女性比率 |
|:-----------|---------:|-----------:|---------:|
| 化學平均   |   1.0000 |     0.6981 |   0.0663 |
| 總成績平均 |   0.6981 |     1.0000 |   0.2452 |
| 女性比率   |   0.0663 |     0.2452 |   1.0000 |

``` r
#畫看兩者間關聯
#圖5.2
plot(dta[, 4], dta[, 6], type = 'n', xlab ='化學分數', ylab = '總成績', asp = 1)
grid()
#學生
points(dta[, 4], dta[, 6], pch = '.', cex = 2)
abline(lm(dta[,6] ~ dta[, 4]))
#學校
points(dta_m[, 2], dta_m[, 3], cex = 0.5, col = 'grey') 
abline(lm(dta_m[,3] ~ dta_m[,2]), col = 'grey')
#對角線
abline(0, 1, lty = 3, col = 'blue')
```

<img src="Ch05_files/figure-gfm/fig5.2-1.png" style="display: block; margin: auto;" />

``` r
#看看各校與各區域以化學預測總分時的截距與斜率
#請注意原始文章的預測變項與解釋變項跟此所作是相反的。
#載入 ggplot2 套件，準備畫圖。
library(ggplot2)
```

``` r
# 記錄下原始配色
#old <- theme_set(theme_bw())
```

``` r
#各校以化學預測總分時的截距與斜率
#圖5.3
ggplot(data = dta, aes(x = 化學, y = 總成績, group = 學校))+
 stat_smooth(method = 'lm', formula = y ~ x, se = F, color = 'lightgray') +
 geom_point(size = 1) +
 stat_smooth(aes(group = 1), method= 'lm', formula = y ~ x, se = F, color = 'black') +
 labs(x = '化學分數', y = '總成績', title = '學校')
```

<img src="Ch05_files/figure-gfm/fig5.3-1.png" style="display: block; margin: auto;" />

``` r
#各區域以化學預測總分時的截距與斜率
#圖5.4
ggplot(data = dta, aes(x = 化學, y = 總成績, group = 區域)) +
 stat_smooth(method = 'lm', formula = y ~ x, se = F, color = 'lightgray') +
 geom_point(size = 1) +
 stat_smooth(aes(group = 1), method = 'lm', formula = y ~ x, se = F, 
             color = 'black') +
 labs(x = '化學分數', y = '總成績' , title = '區域') 
```

<img src="Ch05_files/figure-gfm/fig5.4-1.png" style="display: block; margin: auto;" />

``` r
# 將變項以總分均置中，亦即，減去總平均。
dta <- dta |>
  dplyr::mutate(化學置中 = scale(化學, scale = F))
```

``` r
#選取25個學校與區域 
set.seed(1225)
ns25 <- with(dta, levels(學校)) |> sample(25) 
set.seed(1225)
nr25 <- with(dta, levels(區域)) |> sample(25)
```

``` r
#重看一次各校以化學預測總分時的截距與斜率
#圖5.5
dta |> dplyr::filter(學校 %in% ns25) |>
 ggplot() + 
   aes(x = 化學置中, y = 總成績, color = 性別) +
   geom_point(size = 1) +
   stat_smooth(method = 'lm', formula = y ~ x, se = F) +
   facet_wrap( ~ 學校 )
```

<img src="Ch05_files/figure-gfm/fig5.5-1.png" style="display: block; margin: auto;" />

``` r
#重看一次各區域以化學預測總分時的截距與斜率
#圖5.6
dta |> dplyr::filter(區域 %in% nr25) |>
 ggplot() + 
   aes(x = 化學置中, y = 總成績, color = 性別) +
   geom_point(size = 1) +
   stat_smooth(method = 'lm', formula = y ~ x, se = F) +
   facet_wrap( ~ 區域 )
```

<img src="Ch05_files/figure-gfm/fig5.6-1.png" style="display: block; margin: auto;" />

### 多層次分析

``` r
#載入 lme4 套件，用來分析多層次資料
library(lme4)
```

``` r
#先以完整模型嘗試
#程式報表5.5
m0 <- lme4::lmer(總成績 ~ 化學置中 + 性別 + 化學置中:性別 + 
                 ( 1 | 學校 ) + ( 1 | 區域 ), data = dta )
m0 |> summary()
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: 總成績 ~ 化學置中 + 性別 + 化學置中:性別 + (1 |  
        學校) + (1 | 區域)
       Data: dta

    REML criterion at convergence: 55708

    Scaled residuals: 
       Min     1Q Median     3Q    Max 
    -9.231 -0.567  0.064  0.653  4.079 

    Random effects:
     Groups   Name        Variance Std.Dev.
     學校     (Intercept) 0.0876   0.296   
     區域     (Intercept) 0.0106   0.103   
     Residual             0.3181   0.564   
    Number of obs: 31022, groups:  學校, 2410; 區域, 131

    Fixed effects:
                   Estimate Std. Error t value
    (Intercept)     6.08025    0.01326  458.43
    化學置中        0.16808    0.00141  119.33
    性別F           0.33409    0.00752   44.44
    化學置中:性別F -0.01227    0.00209   -5.87

    Correlation of Fixed Effects:
                   (Intr) 化學置中 性別F 
    化學置中        0.051                
    性別F          -0.265 -0.065         
    化學置中:性別F -0.023 -0.630    0.067

``` r
# 試著去除區域的隨機效果，並看看去除隨機效果是否顯著。
m1 <- update(m0, . ~ . - ( 1 | 區域 ) )
```

``` r
#程式報表5.6
anova(m0, m1)
```

|     | npar |   AIC |   BIC | logLik | deviance | Chisq |  Df | Pr(\>Chisq) |
|:----|-----:|------:|------:|-------:|---------:|------:|----:|------------:|
| m1  |    6 | 55743 | 55793 | -27866 |    55731 |    NA |  NA |          NA |
| m0  |    7 | 55685 | 55744 | -27836 |    55671 | 59.74 |   1 |           0 |

``` r
#去除交互作用項
#程式報表5.7
drop1(m0, test = 'Chisq')
```

|               | npar |   AIC |   LRT | Pr(Chi) |
|:--------------|-----:|------:|------:|--------:|
| <none>        |   NA | 55685 |    NA |      NA |
| 化學置中:性別 |    1 | 55718 | 34.39 |       0 |

``` r
m0 |> summary()
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: 總成績 ~ 化學置中 + 性別 + 化學置中:性別 + (1 |  
        學校) + (1 | 區域)
       Data: dta

    REML criterion at convergence: 55708

    Scaled residuals: 
       Min     1Q Median     3Q    Max 
    -9.231 -0.567  0.064  0.653  4.079 

    Random effects:
     Groups   Name        Variance Std.Dev.
     學校     (Intercept) 0.0876   0.296   
     區域     (Intercept) 0.0106   0.103   
     Residual             0.3181   0.564   
    Number of obs: 31022, groups:  學校, 2410; 區域, 131

    Fixed effects:
                   Estimate Std. Error t value
    (Intercept)     6.08025    0.01326  458.43
    化學置中        0.16808    0.00141  119.33
    性別F           0.33409    0.00752   44.44
    化學置中:性別F -0.01227    0.00209   -5.87

    Correlation of Fixed Effects:
                   (Intr) 化學置中 性別F 
    化學置中        0.051                
    性別F          -0.265 -0.065         
    化學置中:性別F -0.023 -0.630    0.067

### 固定效果

``` r
#載入 coefplot2 套件，繪製固定效果
if (!require("coefplot2")) install.packages("coefplot2",
    repos="http://www.math.mcmaster.ca/bolker/R")
library(coefplot2)
coefplot2(m0)
```

<img src="Ch05_files/figure-gfm/unnamed-chunk-106-1.png" style="display: block; margin: auto;" />

``` r
#抽取變異成分，計算學校與區域可以解釋部分（ICCs）
print(vc <- lme4::VarCorr(m0), comp = 'Variance' )
```

     Groups   Name        Variance
     學校     (Intercept) 0.0876  
     區域     (Intercept) 0.0106  
     Residual             0.3181  

``` r
vc <- as.data.frame(vc)
vc[vc$grp=='學校', 'vcov']/ sum(vc$vcov)
```

    [1] 0.2105

``` r
vc[vc$grp=='區域', 'vcov']/ sum(vc$vcov)
```

    [1] 0.02555

``` r
1 - (vc[vc$grp=='Residual', 'vcov']/ sum(vc$vcov))
```

    [1] 0.236

### 檢驗模型

``` r
#學校與區域層次的隨機效果QQ圖，檢查隨機效果是否呈常態
qq_r21 <- qqmath(~ ranef(m0)$學校, type = c('p', 'g', 'r'), pch = '.',
                xlab = '常態分位數', ylab = '學校隨機效果分位數')
```

``` r
qq_r22 <- qqmath(~ ranef(m0)$區域, type = c('p', 'g', 'r'), pch = '.',
                xlab = '常態分位數', ylab = '區域隨機效果分位數')
```

``` r
#殘差的QQ圖，檢驗殘差是否呈常態
qq_r0 <- qqmath(~ resid(m0, scale = T), type = c('p', 'g', 'r'), pch = '.',
                xlab = '標準化常態分位數', ylab = '標準化殘差')
```

``` r
#預測值對殘差圖
m0_f <-  data.frame(yhat=fitted(m0), zres=resid(m0, scaled=TRUE))
r_m0 <- ggplot(data = m0_f, aes(x = yhat, y = zres)) + 
          geom_point(pch = '.') +
          stat_smooth(method = 'gam', formula = y ~ s(x, bs='cs'),
                      se = FALSE) +
          labs( x = '預測值', y = '標準化殘差')
```

``` r
#把圖放在一起呈現
#圖5.7
library(gridExtra)
grid.arrange(qq_r21, qq_r22, qq_r0, r_m0, nrow = 2, ncol = 2)
```

<img src="Ch05_files/figure-gfm/fig5.7-1.png" style="display: block; margin: auto;" />

### 複雜的多層次模型

``` r
#顯示複雜的多層次模型，包括隨機斜率效果、學校層次的預測變項以及跨層次交互作用
#先要將不同層次資料併在一起
dta <- merge(dta, dta_m, by="學校") 
```

``` r
rslt <- lmer(總成績 ~ 化學置中 + 性別 + 化學置中:性別 + 女性比率 + 女性比率:性別+
              (化學置中  + 性別 | 學校) + (1 | 區域), data=dta)
rslt |> summary()
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: 總成績 ~ 化學置中 + 性別 + 化學置中:性別 + 女性比率 +  
        女性比率:性別 + (化學置中 + 性別 | 學校) +      (1 | 區域)
       Data: dta

    REML criterion at convergence: 55393

    Scaled residuals: 
       Min     1Q Median     3Q    Max 
    -9.350 -0.561  0.068  0.650  4.123 

    Random effects:
     Groups   Name        Variance Std.Dev. Corr       
     學校     (Intercept) 0.098109 0.3132              
              化學置中    0.000548 0.0234   -0.56      
              性別F       0.018070 0.1344   -0.63  0.07
     區域     (Intercept) 0.010623 0.1031              
     Residual             0.310395 0.5571              
    Number of obs: 31022, groups:  學校, 2410; 區域, 131

    Fixed effects:
                   Estimate Std. Error t value
    (Intercept)     6.14025    0.01925  318.97
    化學置中        0.16986    0.00156  109.12
    性別F           0.14631    0.02226    6.57
    女性比率       -0.22288    0.04002   -5.57
    化學置中:性別F -0.01110    0.00216   -5.15
    性別F:女性比率  0.43084    0.04823    8.93

    Correlation of Fixed Effects:
                   (Intr) 化學置中 性別F  女性比率 化學置中:性
    化學置中       -0.095                                     
    性別F          -0.482 -0.004                              
    女性比率       -0.705  0.069    0.645                     
    化學置中:性別F  0.066 -0.609    0.004 -0.102              
    性別F:女性比率  0.494 -0.026   -0.915 -0.797    0.030     
    optimizer (nloptwrap) convergence code: 0 (OK)
    Model failed to converge with max|grad| = 0.002477 (tol = 0.002, component 1)

``` r
# use a different optimization routine
library(optimx)
rslt <- lmer(總成績 ~ 化學置中 + 性別 + 化學置中:性別 + 女性比率 + 女性比率:性別+
                    (化學置中  + 性別 | 學校) + (1 | 區域), data=dta,
                    control = lmerControl(optimizer ='optimx',
                                          optCtrl=list(method='nlminb')))
rslt |> summary()
```

    Linear mixed model fit by REML ['lmerMod']
    Formula: 總成績 ~ 化學置中 + 性別 + 化學置中:性別 + 女性比率 +  
        女性比率:性別 + (化學置中 + 性別 | 學校) +      (1 | 區域)
       Data: dta
    Control: lmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))

    REML criterion at convergence: 55393

    Scaled residuals: 
       Min     1Q Median     3Q    Max 
    -9.350 -0.561  0.068  0.650  4.123 

    Random effects:
     Groups   Name        Variance Std.Dev. Corr       
     學校     (Intercept) 0.098114 0.3132              
              化學置中    0.000548 0.0234   -0.56      
              性別F       0.018077 0.1345   -0.63  0.07
     區域     (Intercept) 0.010619 0.1030              
     Residual             0.310394 0.5571              
    Number of obs: 31022, groups:  學校, 2410; 區域, 131

    Fixed effects:
                   Estimate Std. Error t value
    (Intercept)     6.14025    0.01925  318.98
    化學置中        0.16986    0.00156  109.11
    性別F           0.14632    0.02226    6.57
    女性比率       -0.22288    0.04002   -5.57
    化學置中:性別F -0.01110    0.00216   -5.15
    性別F:女性比率  0.43084    0.04823    8.93

    Correlation of Fixed Effects:
                   (Intr) 化學置中 性別F  女性比率 化學置中:性
    化學置中       -0.095                                     
    性別F          -0.482 -0.004                              
    女性比率       -0.705  0.069    0.645                     
    化學置中:性別F  0.066 -0.609    0.004 -0.102              
    性別F:女性比率  0.494 -0.026   -0.915 -0.797    0.030     

### 結束

顯示演練單元信息

``` r
sessionInfo()
```

    R version 4.2.3 (2023-03-15)
    Platform: aarch64-apple-darwin20 (64-bit)
    Running under: macOS Ventura 13.3.1

    Matrix products: default
    LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib

    locale:
    [1] zh_TW.UTF-8/zh_TW.UTF-8/zh_TW.UTF-8/C/zh_TW.UTF-8/zh_TW.UTF-8

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] optimx_2022-4.30  coefplot2_0.1.3.2 lattice_0.21-8    reshape_0.8.9    
     [5] coda_0.19-4       lubridate_1.9.2   forcats_1.0.0     stringr_1.5.0    
     [9] dplyr_1.1.1       purrr_1.0.1       readr_2.1.4       tidyr_1.3.0      
    [13] tibble_3.2.1      ggplot2_3.4.2     tidyverse_2.0.0   gridExtra_2.3    
    [17] printr_0.3        mlmRev_1.0-8      lme4_1.1-32       Matrix_1.5-4     
    [21] pacman_0.5.1     

    loaded via a namespace (and not attached):
     [1] jsonlite_1.8.4       splines_4.2.3        highr_0.9           
     [4] cellranger_1.1.0     yaml_2.3.7           numDeriv_2016.8-1.1 
     [7] pillar_1.8.1         glue_1.6.2           digest_0.6.30       
    [10] RColorBrewer_1.1-3   minqa_1.2.5          colorspace_2.0-3    
    [13] htmltools_0.5.4      plyr_1.8.8           pkgconfig_2.0.3     
    [16] DiagrammeR_1.0.9     scales_1.2.1         tzdb_0.3.0          
    [19] timechange_0.2.0     mgcv_1.8-42          generics_0.1.3      
    [22] farver_2.1.1         ellipsis_0.3.2       gtsummary_1.7.0     
    [25] withr_2.5.0          cli_3.6.0            magrittr_2.0.3      
    [28] readxl_1.4.2         evaluate_0.18        fansi_1.0.3         
    [31] broom.helpers_1.12.0 nlme_3.1-162         MASS_7.3-58.3       
    [34] textshaping_0.3.6    rsconnect_0.8.27     tools_4.2.3         
    [37] hms_1.1.2            lifecycle_1.0.3      munsell_0.5.0       
    [40] compiler_4.2.3       systemfonts_1.0.4    rlang_1.1.0         
    [43] grid_4.2.3           nloptr_2.0.3         gt_0.8.0            
    [46] rstudioapi_0.14      htmlwidgets_1.5.4    visNetwork_2.1.0    
    [49] labeling_0.4.2       rmarkdown_2.21       boot_1.3-28.1       
    [52] gtable_0.3.1         R6_2.5.1             knitr_1.42          
    [55] fastmap_1.1.0        utf8_1.2.3           ragg_1.2.5          
    [58] stringi_1.7.12       Rcpp_1.0.10          vctrs_0.6.1         
    [61] tidyselect_1.2.0     xfun_0.38           
