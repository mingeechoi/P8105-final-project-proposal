Regression analysis
================
Troy Zhou

    ## Warning: package 'purrr' was built under R version 4.2.2

    ## Warning: package 'geepack' was built under R version 4.2.2

## Intro about the data from exploratory analysis

## Regression Analysis

``` r
comp_df <- read.csv("Data/merged.csv")

ex_df<-comp_df %>% filter(is.na(maskwearing))
comp_df<-comp_df %>% filter(!is.na(maskwearing))

exclude_states <- unique(ex_df$state)
states <- unique(comp_df$state)
```

- excluding `pr, vi, as` since they do not have any measurements.

### Model Building

``` r
analysis_df <- comp_df %>% mutate(date=as.Date(date),state=as.factor(state)) %>% arrange(state,date) %>% select(state,date,maskwearing,symptom,admission,everything())

# test <-analysis_df %>% filter(date>=as.Date("2021-06-01") & date<=as.Date("2022-04-01"))

# outcome = "admission" or "symptom"; l_date = lower bound of date written as "YYYY-MM-DD"; u_date = upper bound of date written as "YYYY-MM-DD"
# s_state = selected state; b_nation = model scale "nation" or "state" -wide

shiny_input_model <- function(outcome, l_date, u_date, s_state, b_nation){
  
  if (outcome=="admission"){
    model_formula <- formula(admission ~ maskwearing)
  }else if (outcome=="symptom"){
    model_formula <- formula(symptom ~ maskwearing)
  }
  
  if (b_nation=="state"){
    temp_df<-analysis_df %>% filter(state %in% s_state & date>=as.Date(l_date) & date<=as.Date(u_date))
    out_table <- geeglm(formula=model_formula, data=temp_df, id=state, family=gaussian("identity"),corstr="exchangeable",std.err = "san.se") %>% 
      broom::tidy() %>% knitr::kable(digits = 3) 
  } else if (b_nation=="nation"){
    temp_df<-analysis_df %>% filter(date>=as.Date(l_date) & date<=as.Date(u_date))
    out_table <- geeglm(formula=model_formula, data=temp_df, id=state, family=gaussian("identity"),corstr="exchangeable",std.err = "san.se") %>% 
      broom::tidy() %>% knitr::kable(digits = 3) 
  }

  return(out_table)
}

ex_1<-shiny_input_model(outcome="admission", l_date="2021-06-01", u_date="2022-04-01", s_state="co", b_nation="state")
ex_2<-shiny_input_model(outcome="admission", l_date="2021-06-01", u_date="2022-04-01", s_state=NA, b_nation="nation")
```

- first, this code chunk is building a nationwide model incorporating
  all states’ date excluding the aforementioned 3.

- given exposure `maskwearing` is a continuous variable, we will be
  estimating the mean function of the repeated measure.

- `identity` function is used as the link function

- We assumed that the correlation among distinct observations in the
  same state is the same regardless of time. Therefore, we used
  `exchangeable` as the correlation structure.

- `state` is treated as the `id`.

- the function `shiny_input_model` was written in preparation for the
  shiny app deployment

- If we want to model the association in `co`, the function will retuen
  the following table:

| term        | estimate | std.error |    statistic | p.value |
|:------------|---------:|----------:|-------------:|--------:|
| (Intercept) |   19.746 |         0 | 6.325771e+20 |       0 |
| maskwearing |   62.123 |         0 | 1.070365e+21 |       0 |

- If we want to model the association nationwide, the function will
  retuen the following table:

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    0.962 |     5.120 |     0.035 |   0.851 |
| maskwearing |   81.353 |    14.022 |    33.661 |   0.000 |
