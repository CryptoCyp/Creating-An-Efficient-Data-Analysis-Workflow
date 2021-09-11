Creating An Efficient Data Analysis Workflow
================
Eriz Tolay
9/11/2021

## Creating An Efficient Data Analysis Workflow

In this project, we will be acting as a data analyst for a company that
sells books for learning programming. A company has produced multiple
books, and each has received many reviews. Our company wants us to check
out the sales data and see if we can extract any useful information from
it.

``` r
data_df <- read_csv("book_reviews.csv")
```

    ## Rows: 2000 Columns: 4

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): book, review, state
    ## dbl (1): price

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

How big is the dataset?

``` r
dim(data_df)
```

    ## [1] 2000    4

What are the column names?

``` r
column_names <- colnames(data_df)
column_names
```

    ## [1] "book"   "review" "state"  "price"

What are the types of each column?

``` r
for (c in colnames(data_df)) {
  print(typeof(data_df[[c]]))
}
```

    ## [1] "character"
    ## [1] "character"
    ## [1] "character"
    ## [1] "double"

What are the unique values in each column?

``` r
for (c in colnames(data_df)) {
  print("Unique values in the column:")
  print(c)
  print(unique(data_df[[c]]))
  print("")
}
```

    ## [1] "Unique values in the column:"
    ## [1] "book"
    ## [1] "R Made Easy"                        "R For Dummies"                     
    ## [3] "Secrets Of R For Advanced Students" "Top 10 Mistakes R Beginners Make"  
    ## [5] "Fundamentals of R For Beginners"   
    ## [1] ""
    ## [1] "Unique values in the column:"
    ## [1] "review"
    ## [1] "Excellent" "Fair"      "Poor"      "Great"     NA          "Good"     
    ## [1] ""
    ## [1] "Unique values in the column:"
    ## [1] "state"
    ## [1] "TX"         "NY"         "FL"         "Texas"      "California"
    ## [6] "Florida"    "CA"         "New York"  
    ## [1] ""
    ## [1] "Unique values in the column:"
    ## [1] "price"
    ## [1] 19.99 15.99 50.00 29.99 39.99
    ## [1] ""

## Data Cleaning

The first issue we will contend with is the issue of missing data.
Missing data is annoying because there’s nothing we can really do with
it. We can’t perform any analysis or calculations with missing data.

In this project we will remove any rows or columns that have missing
data.

``` r
# Filters and add the rows without NA data into a new dataset.
na_cleaned <- data_df %>%
  filter(!(is.na(review)))

dim(na_cleaned)
```

    ## [1] 1794    4

206 rows were removed which means that 206 rows had NA data.

The next thing that we will work on is the state column. The labeling
for each state is inconsistent. For example, California is written as
both “California” and “CA”. Both “California” and “CA” refer to the same
place in the United States, so we will try to clean this up. We need to
choose one of the ways to refer to the state, and stick to that
convention. Making labels/strings more consistent in the data will make
things easier to analyze later on, so we’ll handle this issue now.

``` r
print(unique(na_cleaned[['state']]))
```

    ## [1] "TX"         "NY"         "FL"         "Texas"      "Florida"   
    ## [6] "CA"         "California" "New York"

``` r
na_cleaned <- na_cleaned %>% 
  mutate(
    state = case_when(
      state == "California" ~ "CA",
      state == "New York" ~ "NY",
      state == "Texas" ~ "TX",
      state == "Florida" ~ "FL",
      TRUE ~ state # ignore cases where it's already postal code
    )
  )
```

The first thing we’ll handle in the dataset are the reviews themselves.
You may have noticed in our data exploration that the reviews take the
form of strings, ranging from “Poor” to “Excellent”. Our goal is to
evaluate the ratings of each of the textbooks, but there’s not much we
can do with text versions of the review scores. It would be better if we
were to convert the reviews into a numerical form.

It would also be helpful to have another column that helps us decide if
a score is “high” or not.

``` r
na_cleaned <- na_cleaned %>%
  mutate(
    review_num = case_when(
      review == "Poor" ~ 1,
      review == "Fair" ~ 2,
      review == "Good" ~ 3,
      review == "Great" ~ 4,
      review == "Excellent" ~ 5
    ),
    is_high_review = case_when(
      review_num >= 4 ~ "TRUE",
      review_num < 4 ~ "FALSE",
  )
)
```

## Data Analysis

After all of our data cleaning, now we’re ready to do some analysis of
the data. Our main goal is to figure out which book is the most
profitable. How will we judge what the “most profitable” book is though?
Our dataset represents customer purchases. One way to define “most
profitable” might be to just choose the book that’s purchased the most.
Another way to define it would be to see how much money each book
generates overall.

``` r
na_cleaned %>% 
  group_by(book) %>% 
  summarize(
    purchased = n()
  ) %>% 
  arrange(-purchased)
```

    ## # A tibble: 5 × 2
    ##   book                               purchased
    ##   <chr>                                  <int>
    ## 1 Fundamentals of R For Beginners          366
    ## 2 R For Dummies                            361
    ## 3 Secrets Of R For Advanced Students       360
    ## 4 Top 10 Mistakes R Beginners Make         355
    ## 5 R Made Easy                              352

``` r
revenues <- na_cleaned %>% 
  group_by(book,price) %>% 
  summarize(
    purchased = n()
  ) %>% 
  mutate(
    total_revenue = purchased * price
  ) %>%
  arrange(-total_revenue)
```

    ## `summarise()` has grouped output by 'book'. You can override using the `.groups` argument.

``` r
revenues
```

    ## # A tibble: 5 × 4
    ## # Groups:   book [5]
    ##   book                               price purchased total_revenue
    ##   <chr>                              <dbl>     <int>         <dbl>
    ## 1 Secrets Of R For Advanced Students  50         360        18000 
    ## 2 Fundamentals of R For Beginners     40.0       366        14636.
    ## 3 Top 10 Mistakes R Beginners Make    30.0       355        10646.
    ## 4 R Made Easy                         20.0       352         7036.
    ## 5 R For Dummies                       16.0       361         5772.

## Conclusion

Judging by the total revenue generated, Secrets of R for Advanced
Students is more profitable than the rest.
