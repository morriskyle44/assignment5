Statistical assignment 5
================
\[Kyle Morrs\] \[056527\]
\[24/03/2019\]

In this assignment we will revisit the data on ethnic intermarriage from assignment 2. First we need to read and join the data. I provide the code for this; as usual you'll need to add the file paths.

``` r
library(tidyverse)
Egoalt8 <- read_tsv("C:/Users/morri/Documents/data/UKDA-6614-tab/tab/ukhls_w8/h_egoalt.tab")
Stable <- read_tsv("C:/Users/morri/Documents/data/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
Partners8 <- Egoalt8 %>%
        filter(h_relationship_dv == 1 | h_relationship_dv == 2) %>%
        select(pidp, apidp, h_hidp, h_esex, h_asex, h_relationship_dv) %>%
        # filter out same-sex couples
        filter(h_esex != h_asex) %>%
        # keep only one observation per couple with women as egos
        filter(h_esex == 2)
# Selecting data on ethnicicty
Stable2 <- Stable %>%
        select(pidp, racel_dv)
# Joining the data sets.
JoinedEthn <- Partners8 %>%
        left_join(Stable2, by = "pidp")
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv)
JoinedEthn <- JoinedEthn %>%
        left_join(Stable2, by = c("apidp" = "pidp"))
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv)
rm(Egoalt8, Partners8, Stable, Stable2)
```

We now have a data frame with the information on partners' ethnicity and will work with it.

Recode ethnicity into a factor (10 points)
------------------------------------------

Recode ethnicity of both marital partners into a factor with the following levels (in this order): White British, Irish, Indian, Pakistani, Bangladeshi, Chinese, Black Caribbean, Black African, any other White background, other (includes all other codes). Missing values should be coded as NA.

``` r
JoinedEthn <- JoinedEthn %>%
        mutate(ethnF = case_when(
                                     
    egoRacel_dv == 1 ~ "White British",
    egoRacel_dv == 2 ~ "Irish",
    egoRacel_dv == 9 ~ "Indian",
    egoRacel_dv == 10 ~ "Pakistani",
    egoRacel_dv == 11 ~ "Bangladeshi",
    egoRacel_dv == 12 ~ "Chinese",
    egoRacel_dv == 14 ~ "Black Caribbean",
    egoRacel_dv == 15 ~ "Black African",
    egoRacel_dv == 4 ~ "Other White",
    egoRacel_dv == -9 ~ NA_character_,
     TRUE ~ "Other"))
                                     

    
                 
JoinedEthn <- JoinedEthn %>%
mutate(ethnM = case_when(
    alterRacel_dv == 1 ~ "White British",
    alterRacel_dv == 2 ~ "Irish",
    alterRacel_dv == 9 ~ "Indian",
    alterRacel_dv == 10 ~ "Pakistani",
    alterRacel_dv == 11 ~ "Bangladeshi",
    alterRacel_dv == 12 ~ "Chinese",
    alterRacel_dv == 14 ~ "Black Caribbean",
    alterRacel_dv == 15 ~ "Black African",
    alterRacel_dv == 4 ~ "Other White",
    alterRacel_dv == -9 ~ NA_character_ ,
     TRUE ~ "Other" ))

view(table(JoinedEthn$ethnM))
```

The following code will print a cross-tabulation of ethnicities of wives (in rows) and husbands (in columns). Many couples in the data set are cohabiting, but I will call them wives and husbands for simplicity.

``` r
EthnTable <- table(JoinedEthn$ethnF, JoinedEthn$ethnM)
EthnTable
```

    ##                  
    ##                   Bangladeshi Black African Black Caribbean Chinese Indian
    ##   Bangladeshi             243             0               0       0      0
    ##   Black African             0           159               3       0      2
    ##   Black Caribbean           0             7              68       1      0
    ##   Chinese                   0             1               0      29      2
    ##   Indian                    2             1               3       1    499
    ##   Irish                     0             1               0       0      2
    ##   Other                     1            14              17       3     16
    ##   Other White               1             5               7       1      7
    ##   Pakistani                 1             2               1       0     13
    ##   White British            10            17              27       7     20
    ##                  
    ##                   Irish Other Other White Pakistani White British
    ##   Bangladeshi         0     2           0         2             5
    ##   Black African       0    10           1         2            15
    ##   Black Caribbean     1     7           4         0            34
    ##   Chinese             0     3           5         0            23
    ##   Indian              1    11           4         4            24
    ##   Irish             116     2           4         1            98
    ##   Other               0   169          25         9           161
    ##   Other White         5    32         221         3           221
    ##   Pakistani           0    11           0       424            13
    ##   White British      93   100         145        18          8655

Probabilities of marrying a white British man or woman (20 points)
------------------------------------------------------------------

For women from all ethnic group, calculate the probabilities of being married to a White British man and print them. Then do the same for men from all ethnic groups (i.e. calculate the probabilities of being married to a white British woman). Write one or two sentences interpreting your findings.

``` r
#Var1 = Female



Female_Count = EthnTable %>% 
  rowSums

Husband_Count = EthnTable %>%
  data.frame() %>% 
  filter(Var2 == 'White British')


df_temp = data.frame(ethnicityF =names(Female_Count),Wives = Female_Count, Wife_Married_To_White_British_Husband = Husband_Count$Freq) %>% 
  mutate(Percent = (Wife_Married_To_White_British_Husband / Wives)*100)

df_temp
```

    ##         ethnicityF Wives Wife_Married_To_White_British_Husband   Percent
    ## 1      Bangladeshi   252                                     5  1.984127
    ## 2    Black African   192                                    15  7.812500
    ## 3  Black Caribbean   122                                    34 27.868852
    ## 4          Chinese    63                                    23 36.507937
    ## 5           Indian   550                                    24  4.363636
    ## 6            Irish   224                                    98 43.750000
    ## 7            Other   415                                   161 38.795181
    ## 8      Other White   503                                   221 43.936382
    ## 9        Pakistani   465                                    13  2.795699
    ## 10   White British  9092                                  8655 95.193577

``` r
Male_Count = EthnTable %>%
  colSums()

Wife_Count = EthnTable %>%
  data.frame() %>%
  filter(Var1 == "White British")

  df_temp_2 = data.frame(ethnicityM =names(Male_Count), Husband = Male_Count, Husband_Married_To_White_British_Wife = Wife_Count$Freq) %>% 
mutate(Percent = (Husband_Married_To_White_British_Wife/Husband)*100)

df_temp_2
```

    ##         ethnicityM Husband Husband_Married_To_White_British_Wife   Percent
    ## 1      Bangladeshi     258                                    10  3.875969
    ## 2    Black African     207                                    17  8.212560
    ## 3  Black Caribbean     126                                    27 21.428571
    ## 4          Chinese      42                                     7 16.666667
    ## 5           Indian     561                                    20  3.565062
    ## 6            Irish     216                                    93 43.055556
    ## 7            Other     347                                   100 28.818444
    ## 8      Other White     409                                   145 35.452323
    ## 9        Pakistani     463                                    18  3.887689
    ## 10   White British    9249                                  8655 93.577684

``` r
# There is mixed amount of enodgamy between other races, however, for White British individuals, they have the highest level of ethnic endogamy - by a significant margin. 
```

Odds ratios (20 points)
-----------------------

The probabilities you calculated in the previous question are affected by the ethnic composition of the population. Imagine a population of 200 people, where we have 90 white women, 90 white men, 10 non-white women, and 10 non-white men. Even if mixing is random, white people will have a much higher probability of having a white partner, compared to the probability of non-white people to have a non-white partner. (In other words, white people may get partnered with other white people more often not because they avoid non-White partners, but simply because there are more White partners available.)

To get a measure of ethnic endogamy that is independent of group size, we can calculate odds ratios. The odds for a white woman to have a white partner are calculated as: ω<sub>w</sub> = n<sub>ww</sub> / n<sub>wn</sub>, i.e. the number of white women who partnered a white man divided by the number of white women who partnered a non-White man. The odds for a non-white woman to have a white partner are calculated as: ω<sub>n</sub> = n<sub>nw</sub> / n<sub>nn</sub>, i.e. the number of non-White women who partnered a white man divided by the number of non-White women who partnered a non-White man. The odds ratio is then calculated as θ = ω<sub>w</sub>/ ω<sub>n</sub> = n<sub>ww</sub> n<sub>nn</sub> / (n<sub>wn</sub> n<sub>nw</sub>).

You can read more about how to calculate odds ratios in D.Powers & Y.Xie. (1999). Statistical Methods for Categorical Data Analysis, section 4.2.3. (Avilable here: <https://www.researchgate.net/profile/Nguyen_Trung_Hiep3/post/What_method_of_statistical_analysis_can_I_use_for_categorical_data_analysis/attachment/59d62ca379197b807798af4d/AS%3A347549920710656%401459873766230/download/Statistical+Methods+for+Categorical+Data+Analysis.pdf>)

Let me show how odds ratios work with an example.

``` r
# Let us generate some data from a fictional population of 120 couples.
df <- data.frame(female = c("White", "White", "non-White", "non-White"),
                 male = c("White", "non-White", "White", "non-White"),
                 n = c(85, 5, 5, 10))
# data represented as a table
xtabs(n ~ female + male, df)
```

    ##            male
    ## female      non-White White
    ##   non-White        10     5
    ##   White             5    85

``` r
# the odds ratio here is:

(85 * 10) / (5 * 5)
```

    ## [1] 34

``` r
# 34
```

In the example above, out of 90 white women, 85 married a white man and 5 married a non-White man. Out of 15 non-White women, 10 married a White man and 5 married a non-White man. The probability of a white woman to marry a white man is then 85/90 = 0.94. The probability of racial endogamy for a non-White woman is 5/15 = 0.33. The odds ratio is (85 \* 10) / (5 \* 5) = 34. This number shows that people of the same race are much more likely to marry each other. If mixing was random the odds ratio would have been 1. Note that in a 2x2 table only one odds ratio is meaningful. We can calculate the odds ratio for racial intermarriage by simply taking the reciprocal of 34 (1/34 = 0.03). The advantage of odds ratios is that they are independent of group size (marginal distributions in the 2x2 table) and therefore can provide a useful measure of group endogamy that can be compared across groups.

If we have more than two groups we can calculate the odds ratio separately for each group after collapsing the data to two levels: the group of interest vs. all other groups. Now calculate the odds ratio for ethnic endogamy for the White British (vs. all other groups) in our data.

``` r
df_temp_2 %>% 
  
  mutate(Is_White = ifelse(ethnicityM == 'White British', as.character(ethnicityM),'other')) %>% 
  group_by(Is_White) %>% 
  summarise(Count_Husbands = sum(Husband),
            Count_White_British_Wives = sum(Husband_Married_To_White_British_Wife)) %>% 
  mutate(Count_Non_White_Brit_Wives = Count_Husbands-Count_White_British_Wives) %>% 
  select(1,4,3)
```

    ## # A tibble: 2 x 3
    ##   Is_White      Count_Non_White_Brit_Wives Count_White_British_Wives
    ##   <chr>                              <dbl>                     <int>
    ## 1 other                               2192                       437
    ## 2 White British                        594                      8655

``` r
(2192 * 8655) / (594 * 437)
```

    ## [1] 73.08693

Write a function to calculate odds ratios (20 points)
-----------------------------------------------------

Now write a function that calculates odds ratios. A function must take as an argument a data frame with three variables ("female", "male" and "n") and four observations, and return the odds ratio. For example, for the data frame **df** (see above) the function should return 34 (once you've got your function written check that it actually returns 34 for **df**).

``` r
oddsRatio <- function(df){
  
  CrossTab = xtabs(n ~ female + male, df)
  
  (CrossTab[1,1]* CrossTab[2,2])/(CrossTab[1,2]*CrossTab[2,1])

  
}
oddsRatio(df)
```

    ## [1] 34

Use iteration to calculate multiple odds ratios (30 points)
-----------------------------------------------------------

Now use a for() loop to iterate over the ethnic groups in our data set and calculate the odds ratio for ethnic endogamy for each of them. Use the function you have just written. Print the odds ratios for all the groups in your output document. Write a short interpretation of the results. Which groups are most endogamous?

``` r
for (Ethnicity in as.character(unique(df_temp$ethnicityF))){
  temp = EthnTable %>%
    data.frame() %>% 
    rename(Wife = Var1, Husband = Var2) %>% 
    mutate(Wife = ifelse(Wife == Ethnicity, 1,0),
           Husband = ifelse(Husband == Ethnicity, 1,0)) %>% 
    group_by(Wife, Husband) %>% 
    summarise(n = sum(Freq)) %>% 
    arrange(Wife, Husband) %>% 
    rename(female = Wife, male = Husband)
    
    
  print(Ethnicity)
  print(oddsRatio(temp))
}
```

    ## [1] "Bangladeshi"
    ## [1] 20899.8
    ## [1] "Black African"
    ## [1] 1168.208
    ## [1] "Black Caribbean"
    ## [1] 253.9796
    ## [1] "Chinese"
    ## [1] 774.3394
    ## [1] "Indian"
    ## [1] 1777.904
    ## [1] "Irish"
    ## [1] 124.0985
    ## [1] "Other"
    ## [1] 43.55451
    ## [1] "Other White"
    ## [1] 46.6336
    ## [1] "Pakistani"
    ## [1] 3015.995
    ## [1] "White British"
    ## [1] 73.08693

``` r
# It shows that even though in the earlier question, we showed that White British individuals have a high tendancy to marry other White British, this is not a quality unique to the White British Ethnic group and that it is actually higher in other ethnic groups and that relatively speaking overall the White British actually demonstrate less ethnic endogamy than most other groups (with the exception of "other white"", and "other".
```
