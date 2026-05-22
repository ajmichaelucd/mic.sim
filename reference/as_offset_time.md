# Convert Date to Elapsed Time

Convert Date to Elapsed Time

## Usage

``` r
as_offset_time(x, start_date)
```

## Arguments

- x:

  Date

- start_date:

  Initial timepoint, in decimal years

## Examples

``` r
as_offset_time(lubridate::mdy("2/10/2010"), 2010)
#> Error in as_offset_time(lubridate::mdy("2/10/2010"), 2010): could not find function "as_offset_time"
```
