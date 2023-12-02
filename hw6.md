HW6 document
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(modelr)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
set.seed(1)
```

### Problem 1

In the data cleaning code below we create a `city_state` variable,
change `victim_age` to numeric, modifiy victim_race to have categories
white and non-white, with white as the reference category, and create a
`resolution` variable indicating whether the homicide is solved. Lastly,
we filtered out the following cities: Tulsa, AL; Dallas, TX; Phoenix,
AZ; and Kansas City, MO; and we retained only the variables
`city_state`, `resolution`, `victim_age`, `victim_sex`, and
`victim_race`.

``` r
homicide_df = 
  read_csv("homicide-data.csv", na = c("", "NA", "Unknown")) |> 
  mutate(
    city_state = str_c(city, state, sep = ", "),
    victim_age = as.numeric(victim_age),
    resolution = case_when(
      disposition == "Closed without arrest" ~ 0,
      disposition == "Open/No arrest"        ~ 0,
      disposition == "Closed by arrest"      ~ 1)
  ) |> 
  filter(victim_race %in% c("White", "Black")) |> 
  filter(!(city_state %in% c("Tulsa, AL", "Dallas, TX", "Phoenix, AZ", "Kansas City, MO"))) |> 
  select(city_state, resolution, victim_age, victim_sex, victim_race)
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): uid, victim_last, victim_first, victim_race, victim_sex, city, stat...
    ## dbl (4): reported_date, victim_age, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
baltimore_glm = 
  filter(homicide_df, city_state == "Baltimore, MD") |> 
  glm(resolution ~ victim_age + victim_sex + victim_race, family = binomial(), data = _)

baltimore_glm |> 
  broom::tidy() |> 
  mutate(
    OR = exp(estimate), 
    OR_CI_upper = exp(estimate + 1.96 * std.error),
    OR_CI_lower = exp(estimate - 1.96 * std.error)) |> 
  filter(term == "victim_sexMale") |> 
  select(OR, OR_CI_lower, OR_CI_upper) |>
  knitr::kable(digits = 3)
```

|    OR | OR_CI_lower | OR_CI_upper |
|------:|------------:|------------:|
| 0.426 |       0.325 |       0.558 |

The odds ratio is 0.426, with confidence interval of 0.325 and 0.558.

``` r
model_results = 
  homicide_df |> 
  nest(data = -city_state) |> 
  mutate(
    models = map(data, \(df) glm(resolution ~ victim_age + victim_sex + victim_race, 
                             family = binomial(), data = df)),
    tidy_models = map(models, broom::tidy)) |> 
  select(-models, -data) |> 
  unnest(cols = tidy_models) |> 
  mutate(
    OR = exp(estimate), 
    OR_CI_upper = exp(estimate + 1.96 * std.error),
    OR_CI_lower = exp(estimate - 1.96 * std.error)) |> 
  filter(term == "victim_sexMale") |> 
  select(city_state, OR, OR_CI_lower, OR_CI_upper)

model_results |>
  slice(1:5) |> 
  knitr::kable(digits = 3)
```

| city_state      |    OR | OR_CI_lower | OR_CI_upper |
|:----------------|------:|------------:|------------:|
| Albuquerque, NM | 1.767 |       0.831 |       3.761 |
| Atlanta, GA     | 1.000 |       0.684 |       1.463 |
| Baltimore, MD   | 0.426 |       0.325 |       0.558 |
| Baton Rouge, LA | 0.381 |       0.209 |       0.695 |
| Birmingham, AL  | 0.870 |       0.574 |       1.318 |

Below we generate a plot of the estimated ORs and CIs for each city,
ordered by magnitude of the OR from smallest to largest. From this plot
we see that most cities have odds ratios that are smaller than 1,
suggesting that crimes with male victims have smaller odds of resolution
compared to crimes with female victims after adjusting for victim age
and race. This disparity is strongest in New yrok. In roughly half of
these cities, confidence intervals are narrow and do not contain 1,
suggesting a significant difference in resolution rates by sex after
adjustment for victim age and race.

``` r
model_results |> 
  mutate(city_state = fct_reorder(city_state, OR)) |> 
  ggplot(aes(x = city_state, y = OR)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = OR_CI_lower, ymax = OR_CI_upper)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](hw6_files/figure-gfm/q1_plot-1.png)<!-- -->

### Problem 2

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2022-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## using cached file: /Users/ruohanhong/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-12-01 17:45:54.483354 (8.544)

    ## file min/max dates: 1869-01-01 / 2023-11-30

``` r
bootstrap_analysis = weather_df |>
  modelr::bootstrap(n = 5000) |>
  mutate(
    lm_models = map(strap, ~lm(tmax ~ tmin + prcp, data = .x)),
    glance_data = map(lm_models, broom::glance)
  ) |>
  select(glance_data) |>
  unnest(glance_data)

tidy_analysis = weather_df |>
  modelr::bootstrap(n = 5000) |>
  mutate(
    linear_models = map(strap, ~lm(tmax ~ tmin + prcp, data = .x)),
    tidy_data = map(linear_models, broom::tidy)
  ) |>
  select(tidy_data) |>
  unnest(tidy_data) |>
  select(term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate) |>
  unnest()
```

    ## Warning: Values from `estimate` are not uniquely identified; output will contain
    ## list-cols.
    ## • Use `values_fn = list` to suppress this warning.
    ## • Use `values_fn = {summary_fun}` to summarise duplicates.
    ## • Use the following dplyr code to identify duplicates.
    ##   {data} %>%
    ##   dplyr::group_by(term) %>%
    ##   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    ##   dplyr::filter(n > 1L)

    ## Warning: `cols` is now required when using `unnest()`.
    ## ℹ Please use `cols = c(`(Intercept)`, tmin, prcp)`.

``` r
# Bootstrap analysis with retaining 'term' and 'estimate'
tidy_analysis = weather_df |>
  modelr::bootstrap(n = 5000) |>
  mutate(
    linear_models = map(strap, ~lm(tmax ~ tmin + prcp, data = .x)),
    tidy_data = map(linear_models, broom::tidy)
  ) |>
  select(strap, tidy_data) |>
  unnest(tidy_data)

tidy_analysis_adjusted = tidy_analysis |>
  filter(term %in% c("tmin", "prcp")) |>
  group_by(strap) |>
  summarize(
    tmin_coeff = estimate[term == "tmin"],
    prcp_coeff = estimate[term == "prcp"]
  ) |>
  mutate(
    prod_coeff = tmin_coeff * prcp_coeff,
    logarithmic_value = log(prod_coeff)
  ) |>
  filter(!is.nan(logarithmic_value) & !is.infinite(logarithmic_value))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `logarithmic_value = log(prod_coeff)`.
    ## Caused by warning in `log()`:
    ## ! NaNs produced

``` r
tidy_analysis_adjusted = tidy_analysis |>
  filter(term %in% c("tmin", "prcp")) |>
  group_by(strap) |>
  summarize(
    prod_coeff = prod(estimate[term == "tmin"] * estimate[term == "prcp"])
  ) |>
  mutate(
    logarithmic_value = log(prod_coeff)
  ) |>
  filter(!is.nan(logarithmic_value) & !is.infinite(logarithmic_value))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `logarithmic_value = log(prod_coeff)`.
    ## Caused by warning in `log()`:
    ## ! NaNs produced

``` r
r_squared_plot = bootstrap_analysis |>
  ggplot(aes(x = r.squared)) +
  geom_density()

print(r_squared_plot)
```

![](hw6_files/figure-gfm/Plotting%20the%20distribution%20of%20r-squared%20values-1.png)<!-- -->

``` r
log_value_plot_adjusted = tidy_analysis_adjusted |>
  ggplot(aes(x = logarithmic_value)) +
  geom_density()

print(log_value_plot_adjusted)
```

![](hw6_files/figure-gfm/Plotting%20the%20distribution%20of%20the%20logarithmic%20value-1.png)<!-- -->

``` r
ci_r_squared_adjusted = bootstrap_analysis |> 
  summarize(
    lower_bound_r2 = quantile(r.squared, 0.025), 
    upper_bound_r2 = quantile(r.squared, 0.975)
  )

ci_r_squared_adjusted
```

    ## # A tibble: 1 × 2
    ##   lower_bound_r2 upper_bound_r2
    ##            <dbl>          <dbl>
    ## 1          0.889          0.941

``` r
ci_log_value_adjusted = tidy_analysis_adjusted |>
  summarize(
    lower_bound_log = quantile(logarithmic_value, 0.025), 
    upper_bound_log = quantile(logarithmic_value, 0.975)
  )

ci_log_value_adjusted
```

    ## # A tibble: 1 × 2
    ##   lower_bound_log upper_bound_log
    ##             <dbl>           <dbl>
    ## 1           -9.26           -4.60

The 95% confidence interval for r square is (0.889, 0.941). he 95%
confidence interval for log (beta1\*beta2) is (-4.60, -9.26).

### Problem 3

``` r
birthweight_df = read.csv('birthweight.csv', na =c("", "NA", "Unknown"))
```

``` r
birthweight_df = birthweight_df |>
  mutate(
    babysex = recode(babysex, "1" = "male", "2" = "female"),
    frace = recode(frace, "1" = "White", "2" = "Black", "3" = "Asian", 
                        "4" = "Puerto Rico", "8" = "Others", "9" = "Unknown"),
    malform = recode(malform, "0" = "absent", "1" = "present"),
     mrace = recode(mrace,"1" = "White", "2" = "Black", "3" = "Asian", 
                        "4" = "Puerto Rico", "8" = "Others")
         )
```

## Propose a regression model for birthweight

My proposed regression model for birthweight `bwt` is a linear model
that includes a wide range of predictors - both maternal characteristics
and baby’s characteristics. This model is a comprehensive approach,
taking into account various factors that are hypothesized to impact
birthweight.

Selection of Predictors: The predictors in my model encompass several
categories: Baby Characteristics: `babysex`, `blength` Maternal
Characteristics: `delwt` (delivery weight), `mheight` (mother’s height),
`momage` (mother’s age), `parity` (number of live births), `ppbmi`
(pre-pregnancy BMI), `ppwt` (pre-pregnancy weight), `wtgain` (weight
gain during pregnancy) Health and Lifestyle Factors: `malform` (presence
of malformations), `smoken` (smoking habits) Socioeconomic Factors:
`fincome` (family income) Race and Ethnicity: `frace` (father’s race),
`mrace` (mother’s race) Pregnancy Specifics: `gaweeks` (gestational
age), `pnumlbw` (number of low birth weight babies previously),
`pnumsga` (number of small for gestational age babies previously)
Hypothesis Behind Predictor Selection: The chosen predictors are based
on existing research and theories regarding factors that influence
birthweight.

``` r
model = lm(bwt ~ babysex + blength + delwt + fincome + frace + gaweeks + malform + menarche + mheight+ momage + mrace + parity + pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain, 
            data = birthweight_df)

summary(model)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + blength + delwt + fincome + frace + 
    ##     gaweeks + malform + menarche + mheight + momage + mrace + 
    ##     parity + pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain, 
    ##     data = birthweight_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1654.4  -205.7    -6.7   198.6  3693.9 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -5224.3675   760.1179  -6.873 7.18e-12 ***
    ## babysexmale         20.3313     9.6545   2.106  0.03527 *  
    ## blength            115.1962     1.9850  58.032  < 2e-16 ***
    ## delwt                5.3504     0.4541  11.784  < 2e-16 ***
    ## fincome              0.5286     0.2071   2.553  0.01073 *  
    ## fraceBlack          -3.4188    90.9805  -0.038  0.97003    
    ## fraceOthers         50.6525   112.6099   0.450  0.65287    
    ## fracePuerto Rico   -60.5883    90.5582  -0.669  0.50350    
    ## fraceWhite          17.9172    79.9631   0.224  0.82271    
    ## gaweeks             22.8559     1.6557  13.805  < 2e-16 ***
    ## malformpresent      30.0408    81.5045   0.369  0.71246    
    ## menarche            -1.5596     3.3406  -0.467  0.64062    
    ## mheight             20.9248    11.8954   1.759  0.07864 .  
    ## momage               1.1697     1.4104   0.829  0.40697    
    ## mraceBlack         -96.4075    93.4186  -1.032  0.30213    
    ## mracePuerto Rico    53.6760    93.4175   0.575  0.56560    
    ## mraceWhite          51.7697    82.9903   0.624  0.53279    
    ## parity             124.2871    46.7075   2.661  0.00782 ** 
    ## pnumlbw                  NA         NA      NA       NA    
    ## pnumsga                  NA         NA      NA       NA    
    ## ppbmi               21.0744    17.1780   1.227  0.21995    
    ## ppwt                -6.9450     3.0127  -2.305  0.02120 *  
    ## smoken              -6.3579     0.6759  -9.406  < 2e-16 ***
    ## wtgain                   NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 314.5 on 4321 degrees of freedom
    ## Multiple R-squared:  0.6248, Adjusted R-squared:  0.623 
    ## F-statistic: 359.7 on 20 and 4321 DF,  p-value: < 2.2e-16

Several variables emerge as significant. Baby’s length at birth
(`blength`) and gestational age (`gaweeks`) are highly significant,
indicating a strong positive association with birth weight. Notably, the
baby’s sex (`babysexmale`) also shows significance, suggesting a
difference in birth weight between male and female babies. Mother’s
weight at delivery (`delwt`) and family income (`fincome`) positively
correlate with birth weight, though the effect of income is relatively
small. Interestingly, the mother’s pre-pregnancy weight (`ppwt`) is
negatively associated with birth weight, and maternal smoking (`smoken`)
significantly reduces it, highlighting critical health considerations.
Other factors, including race, presence of malformations, and mother’s
age at menarche, don’t show significant impacts in this model.

``` r
birthweight_df = birthweight_df |> 
  add_predictions(model, var = "preds") |> 
  add_residuals(model, var = "resids")
```

    ## Warning in predict.lm(model, data): prediction from rank-deficient fit; attr(*,
    ## "non-estim") has doubtful cases

    ## Warning in predict.lm(model, data): prediction from rank-deficient fit; attr(*,
    ## "non-estim") has doubtful cases

``` r
# Plot of residuals against fitted values
ggplot(birthweight_df, aes(x = preds, y = resids)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Fitted Values", y = "Residuals", title = "Plot of Residuals vs Fitted Values")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](hw6_files/figure-gfm/plot%20my%20model%20residu-1.png)<!-- -->

## Compare models

``` r
model_1 = lm(bwt ~ blength + gaweeks, data = birthweight_df)

summary(model_1)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ blength + gaweeks, data = birthweight_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1709.6  -215.4   -11.4   208.2  4188.8 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -4347.667     97.958  -44.38   <2e-16 ***
    ## blength       128.556      1.990   64.60   <2e-16 ***
    ## gaweeks        27.047      1.718   15.74   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 333.2 on 4339 degrees of freedom
    ## Multiple R-squared:  0.5769, Adjusted R-squared:  0.5767 
    ## F-statistic:  2958 on 2 and 4339 DF,  p-value: < 2.2e-16

``` r
model_2 = lm(bwt ~ bhead + blength + babysex + bhead * blength + bhead * babysex + blength * babysex, data = birthweight_df)

summary(model_2)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ bhead + blength + babysex + bhead * blength + 
    ##     bhead * babysex + blength * babysex, data = birthweight_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1133.8  -189.7    -7.2   178.8  2721.8 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -3508.3263   820.5298  -4.276 1.95e-05 ***
    ## bhead                  66.8435    25.4407   2.627  0.00863 ** 
    ## blength                35.7217    17.4013   2.053  0.04015 *  
    ## babysexmale          -259.9785   197.9105  -1.314  0.18904    
    ## bhead:blength           1.5608     0.5269   2.962  0.00307 ** 
    ## bhead:babysexmale      12.6620     7.0450   1.797  0.07236 .  
    ## blength:babysexmale    -4.2107     4.1691  -1.010  0.31257    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 288.1 on 4335 degrees of freedom
    ## Multiple R-squared:  0.6839, Adjusted R-squared:  0.6835 
    ## F-statistic:  1563 on 6 and 4335 DF,  p-value: < 2.2e-16

``` r
cv_df = birthweight_df |> 
  modelr::crossv_mc(n = 100) |> 
  mutate(
    train_set = map(train, as_tibble),
    test_set = map(test, as_tibble)
  )
```

``` r
cv_results = cv_df |> 
  mutate(
    model_fit = map(train_set, ~lm(bwt ~ babysex + blength + delwt + fincome + frace + gaweeks + malform + menarche + mheight + momage + mrace + parity + pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain, data = .x)),
    model_1_fit = map(train_set, ~lm(bwt ~ blength + gaweeks, data = .x)),
    model_2_fit = map(train_set, ~lm(bwt ~ bhead + blength + babysex + bhead * blength + bhead * babysex + blength * babysex, data = .x))
  ) |>
  mutate(
    rmse_model = map2_dbl(model_fit, test_set, ~modelr::rmse(.x, .y)),
    rmse_model_1 = map2_dbl(model_1_fit, test_set, ~modelr::rmse(.x, .y)),
    rmse_model_2 = map2_dbl(model_2_fit, test_set, ~modelr::rmse(.x, .y))
  )
```

    ## Warning: There were 100 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `rmse_model = map2_dbl(model_fit, test_set, ~modelr::rmse(.x,
    ##   .y))`.
    ## Caused by warning in `predict.lm()`:
    ## ! prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 99 remaining warnings.

``` r
summary_results = cv_results |> 
  select(rmse_model, rmse_model_1, rmse_model_2) |> 
  pivot_longer(
    everything(),
    names_to = "model",
    values_to = "rmse"
  ) |> 
  group_by(model) |> 
  summarize(average_rmse = mean(rmse, na.rm = TRUE))

summary_results
```

    ## # A tibble: 3 × 2
    ##   model        average_rmse
    ##   <chr>               <dbl>
    ## 1 rmse_model           315.
    ## 2 rmse_model_1         333.
    ## 3 rmse_model_2         288.

Based on the comparison, model 1 which is length at birth and
gestational age as predictors has the largest error of 337, following by
my model and model 2. Which that model 2, Head Circumference, Length,
Sex, and All Interactions, is the best model compare to my model and
model_1 because it has the smallest error of 292.
