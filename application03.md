---
title: "Application Task 3"
author: "Shreya shakya"
date: "10/18/2019"
output: 
  html_document: 
    keep_md: yes
---

### Load packages


```r
library(tidyverse)
library(infer)
library(dplyr)
library(mosaic)
```

### Load data


```r
gss <- read_csv("data/gss2016.csv",
                na = c("", "Dont know", "Don't know",
                       "No answer", "Not applicable", "NA"),
                guess_max = 2867) %>%
  select(harass5, educ, born, polviews, advfront)
```

### Set seed


```r
set.seed(1020)
```

### 2.1: Harrassment at work

**In the GSS dataset, there were 3 types of responses from the people which are Yes, No and Does not apply(i do not have a job/superior/co-worker).**

```r
gss %>%
  group_by(harass5) %>%
  summarize(count=n())
```

```
## # A tibble: 4 x 2
##   harass5                                                 count
##   <chr>                                                   <int>
## 1 Does not apply (i do not have a job/superior/co-worker)    96
## 2 No                                                       1136
## 3 Yes                                                       237
## 4 <NA>                                                     1398
```

**GSS filter dataframe consist of only the data where people had responsed Yes or No for Harassed question.**

```r
gss_filter <- gss %>%
  filter(harass5 %in% c('Yes','No'))
gss_filter
```

```
## # A tibble: 1,373 x 5
##    harass5  educ born  polviews             advfront
##    <chr>   <dbl> <chr> <chr>                <chr>   
##  1 No         16 Yes   Conservative         <NA>    
##  2 No         18 Yes   Slightly liberal     <NA>    
##  3 No         11 No    Slghtly conservative <NA>    
##  4 No         14 Yes   Conservative         <NA>    
##  5 No         12 Yes   <NA>                 <NA>    
##  6 Yes        13 Yes   Moderate             <NA>    
##  7 No         13 No    Conservative         <NA>    
##  8 No         18 Yes   Liberal              <NA>    
##  9 No         11 Yes   Slghtly conservative <NA>    
## 10 No         10 No    Slightly liberal     <NA>    
## # … with 1,363 more rows
```

**Approximately 83% of the people had not been harrased by their superiors or co-workers at work and about 17% people had been harrased.**


```r
gss_filter %>%
  group_by(harass5) %>%
  summarize(count= n()) %>%
  mutate(percentage =   (count / sum(count))*100)
```

```
## # A tibble: 2 x 3
##   harass5 count percentage
##   <chr>   <int>      <dbl>
## 1 No       1136       82.7
## 2 Yes       237       17.3
```

**The bootstrap distribution has been used to see the proportion of Americans who have been harrassed at work. We see a distribution that is roughly symetric.**


```r
# save resulting bootstrap distribution
variable_ci_statistic <- gss_filter %>%
  # specify the variable of interest
  specify(response = harass5, success="Yes") %>%
  # generate 15000 bootstrap samples
  generate(reps = 1500, type = "bootstrap") %>% 
  # calculate the "statistic" of each bootstrap sample
  calculate(stat = "prop") 

ggplot(data = variable_ci_statistic, mapping = aes(x = stat)) +
  geom_histogram(bins=30, fill = "white", color = "black") +
  theme_minimal() +
  labs(title="Bootstrap Distribution of Americans who have been harrassed at work", x="Proportion", y="count")
```

![](application03_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

**We are 95% confident that mostly people who has been harrased are between 0.152 to 0.192 approximately.**


```r
#se = sd(variable_ci_statistic$stat)
#t=qt((1-0.95)/2,df=length(variable_ci_statistic$stat),lower.tail = FALSE)
#interval = median(variable_ci_statistic$stat) + c(-1,1)*t*se

quan <- variable_ci_statistic %>%
  summarize( lower = quantile(stat, probs = .025),
            upper = quantile(stat, probs = .975))

ggplot(data = variable_ci_statistic, mapping = aes(x = stat)) +
  geom_histogram(bins=30, fill = "white", color = "black") +
  theme_minimal()+
  geom_vline(data=quan, aes(xintercept = lower))  +
  geom_vline(data=quan, aes(xintercept = upper))  +
  labs(title="Bootstrap Distribution of Proportion", 
       subtitle = "95% Confidence Interval", x="Proportion", y="count")
```

![](application03_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### 2.2: Liberals vs. Conservatives - Is science research necessary?

*Checking the types of values each variable contains.*

```r
gss %>%
  group_by(advfront) %>%
  summarize(n=n())
```

```
## # A tibble: 5 x 2
##   advfront              n
##   <chr>             <int>
## 1 Agree               759
## 2 Disagree            184
## 3 Strongly agree      400
## 4 Strongly disagree    15
## 5 <NA>               1509
```

```r
gss %>%
  group_by(polviews) %>%
  summarize(n=n())
```

```
## # A tibble: 8 x 2
##   polviews                 n
##   <chr>                <int>
## 1 Conservative           426
## 2 Extremely liberal      136
## 3 Extrmly conservative   120
## 4 Liberal                350
## 5 Moderate              1032
## 6 Slghtly conservative   382
## 7 Slightly liberal       310
## 8 <NA>                   111
```

**Possible response to the question on science research were "Strongly agree" and "Agree" which are mapped to "Yes", and "Disagree" and "Strongly disagree" which are mapped to "No".**

```r
gss<- gss %>%
  mutate(recode_advfront= fct_collapse(gss$advfront,
                         no = c("Strongly disagree","Disagree"),
                         yes = c("Strongly agree","Agree")))
```

**Possible response to the question on political view were "Extremely liberal", "Liberal", and "Slightly liberal", which are mapped to "Liberal", and "Slghtly conservative", "Conservative", and "Extrmly conservative" which are mapped to "Conservative"**


```r
gss <- gss %>%
  mutate(recode_polviews= fct_collapse(gss$polviews,
                         Liberal = c("Extremely liberal","Liberal","Slightly liberal"),
                         Conservative = c("Slghtly conservative","Conservative","Extrmly conservative")))

gss%>%
  group_by(recode_polviews) %>%
  summarize(n=n())
```

```
## Warning: Factor `recode_polviews` contains implicit NA, consider using
## `forcats::fct_explicit_na`
```

```
## # A tibble: 4 x 2
##   recode_polviews     n
##   <fct>           <int>
## 1 Conservative      928
## 2 Liberal           796
## 3 Moderate         1032
## 4 <NA>              111
```

**Following dataframe consist of respondents who self-identified as "liberal" or "conservative" and who responded "yes" or "no" to the science research question**


```r
gss_update <- gss %>%
  filter(recode_polviews %in% c("Liberal","Conservative"), 
         recode_advfront %in% c("yes","no"))
```

**We have already filtered the non Na values. Now, to estimate the difference in proportion of liberals and conservative who think science research is necessary we conduct the following steps. We conduct sampling with replacement for people who are liberal and who are conservative. Then, we find the bootstrap statistic, i.e difference of proportion between them. We specify the response as political views and explanatory variables as science research.**


```r
diff_political <- gss_update %>%
  specify(response = recode_polviews, explanatory = recode_advfront, success ="Liberal") %>%
  generate(reps = 1500, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("yes","no"))
```

**We are 90% confident that the difference between difference in proportion of liberals and conservatives who think science research is necessary and should be supported by the federal government is between 0.075 and 0.226**


```r
#se = sd(diff_political$stat)
#t=qt((1-0.90)/2,df=length(variable_ci_statistic$stat),lower.tail = FALSE)
#interval = median(variable_ci_statistic$stat) + c(-1,1)*t*se

quan <- diff_political %>%
  summarize( lower = quantile(stat, probs = .05),
            upper = quantile(stat, probs = .95))

ggplot(data = diff_political, mapping = aes(x = stat)) +
  geom_histogram(bins=30, fill = "white", color = "black") +
  theme_minimal()+
  geom_vline(data=quan, aes(xintercept = lower))  +
  geom_vline(data=quan, aes(xintercept = upper))  +
  labs(title="Bootstrap Distribution of difference of Proportion", 
       subtitle = "90% Confidence Interval", x="Proportion", y="count")
```

![](application03_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

