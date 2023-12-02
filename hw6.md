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
tidy_analysis <- weather_df |>
  modelr::bootstrap(n = 5000) |>
  mutate(
    linear_models = map(strap, ~lm(tmax ~ tmin + prcp, data = .x)),
    tidy_data = map(linear_models, broom::tidy)
  ) |>
  select(strap, tidy_data) |>
  unnest(tidy_data)

tidy_analysis_adjusted <- tidy_analysis |>
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
tidy_analysis_adjusted <- tidy_analysis |>
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
confidence interval for log (beta1\*beta2) is (-1.98, -4.00).
