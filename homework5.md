Untitled
================

# Problem 1

``` r
homicides_df = read.csv("data/homicide-data.csv")
```

# Problem 2

Create a tidy dataframe containing data from all participants, including
the subject ID, arm, and observations over time:

Start with a dataframe containing all file names; the list.files
function will help

Iterate over file names and read in data for each subject using
purrr::map and saving the result as a new variable in the dataframe Tidy
the result; manipulate file names to include control arm and subject ID,
make sure weekly observations are “tidy”, and do any other tidying
that’s necessary Make a spaghetti plot showing observations on each
subject over time, and comment on differences between groups.

``` r
all_df = 
  tibble(
    files = list.files(path="data/longitudinal_study"),
    path = str_c("data/longitudinal_study/", files)
  ) %>% 
  mutate(data = map(path, read_csv))%>% 
  unnest(data)
```

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 1 Columns: 8

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tidy_df=
  all_df %>% 
    select(-path) %>% 
    mutate(files=str_replace(files,".csv","")) %>% 
    separate(files,into=c("group","ID"),sep="_") %>% 
    mutate(group=str_replace(group,"con","control")) %>% 
    mutate(group=str_replace(group,"exp","experiment")) %>% 
    pivot_longer(
      week_1:week_8,
      names_to = "week",
      values_to ="outcome"
    ) %>% 
    mutate(week=str_replace(week,"week_",""),files=str_c(group,ID),ID=as.numeric(ID),week=as.numeric(week)) %>% 
    rename(subjects=files)%>% 
    select(subjects,everything())

tidy_df%>% 
  ggplot(aes(x=week,y=outcome)) + geom_line(aes(color=subjects)) + facet_grid(~group)
```

<img src="homework5_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

As shown in the plot,in the control group,the outcome of 10 subjects do
not change a lot after 8 weeks.However,the outcome of 10 subjects in the
experiment group increase in a roughly simple linear way.

## Problem 3

There are two cases to address:

For numeric variables, you should fill in missing values with the mean
of non-missing values For character variables, you should fill in
missing values with “virginica”

Write a function that takes a vector as an argument; replaces missing
values using the rules defined above; and returns the resulting vector.
Apply this function to the columns of iris_with_missing using a map
statement.

``` r
set.seed(10)

iris_with_missing = iris %>% 
    janitor::clean_names() %>% 
    map_df(~replace(.x, sample(1:150, 20), NA)) %>%
    mutate(species = as.character(species))


fill_missing=function(x){
    if(!is.vector(x)){
      stop("argument should be a vector")
    }
    replace_na(x,"virginica")
    replace_na(x,mean(x,na.rm=TRUE))
}


iris_with_missing %>% 
  mutate(replace_na(mean(sepal_length,na.rm=TRUE)))
```

    ## # A tibble: 150 × 6
    ##    sepal_length sepal_width petal_length petal_width species `replace_na(mean(s…
    ##           <dbl>       <dbl>        <dbl>       <dbl> <chr>                 <dbl>
    ##  1          5.1         3.5          1.4         0.2 setosa                 5.82
    ##  2          4.9         3            1.4         0.2 setosa                 5.82
    ##  3          4.7         3.2          1.3         0.2 setosa                 5.82
    ##  4          4.6         3.1          1.5        NA   setosa                 5.82
    ##  5          5           3.6          1.4         0.2 setosa                 5.82
    ##  6          5.4         3.9          1.7         0.4 setosa                 5.82
    ##  7         NA           3.4          1.4         0.3 setosa                 5.82
    ##  8          5           3.4          1.5         0.2 setosa                 5.82
    ##  9          4.4         2.9          1.4         0.2 setosa                 5.82
    ## 10          4.9         3.1         NA           0.1 setosa                 5.82
    ## # … with 140 more rows

``` r
mean(iris_with_missing$sepal_length,na.rm=TRUE)
```

    ## [1] 5.819231

``` r
output=vector("list",length=5)
for (i in 1:5){
output[i]=map(iris_with_missing[i],fill_missing)
}
```

    ## Warning in mean.default(x, na.rm = TRUE): argument is not numeric or logical:
    ## returning NA

``` r
a=c(1,2,3,4,5,NA)
mean(a)
```

    ## [1] NA

``` r
fill_missing(a)
```

    ## [1] 1 2 3 4 5 3
