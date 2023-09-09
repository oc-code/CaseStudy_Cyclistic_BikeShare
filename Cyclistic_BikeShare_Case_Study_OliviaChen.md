Cyclistic Bike-Share Case Study
================
Olivia Chen
2023-09-08

## Introduction

Cyclistic is a fictional company, which is a bike-share company in
Chicago. It launched a bike-share program with flexibility of pricing
plans for casual riders and annual members. In this study, we are trying
to understand the differences between casual riders and annual members
to help the team to design a new marketing strategy to maximize the
number of annual memberships.

## Data

The Cyclistic’s monthly trip data were obtained from an open source
created by Motivate International Inc. The combined data set contains
3,158,109 records between January 2023 and July 2023 and 13 variables.

### Set-up

``` r
library(tidyverse)
library(skimr)
library(dplyr)
library(ggmap)
library(ggplot2)
```

### Combine dataset

``` r
setwd("C:/Users/Olivia/Desktop/MSBA 21/R/case study/")
tripdata <- 
  list.files(path = "C:/Users/Olivia/Desktop/MSBA 21/R/case study/", pattern = "*.csv") %>% 
  map_df(~read_csv(.))
```

### Data Understanding and Preprocessing

``` r
summary(tripdata)
```

    ##    ride_id          rideable_type        started_at                    
    ##  Length:3158109     Length:3158109     Min.   :2023-01-01 00:01:58.00  
    ##  Class :character   Class :character   1st Qu.:2023-04-12 16:12:44.00  
    ##  Mode  :character   Mode  :character   Median :2023-05-28 09:06:45.00  
    ##                                        Mean   :2023-05-16 21:37:34.01  
    ##                                        3rd Qu.:2023-06-30 07:41:51.00  
    ##                                        Max.   :2023-07-31 23:59:56.00  
    ##                                                                        
    ##     ended_at                      start_station_name start_station_id  
    ##  Min.   :2023-01-01 00:02:41.00   Length:3158109     Length:3158109    
    ##  1st Qu.:2023-04-12 16:29:23.00   Class :character   Class :character  
    ##  Median :2023-05-28 09:30:04.00   Mode  :character   Mode  :character  
    ##  Mean   :2023-05-16 21:56:02.72                                        
    ##  3rd Qu.:2023-06-30 07:57:28.00                                        
    ##  Max.   :2023-08-12 04:53:41.00                                        
    ##                                                                        
    ##  end_station_name   end_station_id       start_lat       start_lng     
    ##  Length:3158109     Length:3158109     Min.   :41.64   Min.   :-87.92  
    ##  Class :character   Class :character   1st Qu.:41.88   1st Qu.:-87.66  
    ##  Mode  :character   Mode  :character   Median :41.90   Median :-87.64  
    ##                                        Mean   :41.90   Mean   :-87.65  
    ##                                        3rd Qu.:41.93   3rd Qu.:-87.63  
    ##                                        Max.   :42.07   Max.   :-87.52  
    ##                                                                        
    ##     end_lat         end_lng       member_casual     
    ##  Min.   : 0.00   Min.   :-88.16   Length:3158109    
    ##  1st Qu.:41.88   1st Qu.:-87.66   Class :character  
    ##  Median :41.90   Median :-87.64   Mode  :character  
    ##  Mean   :41.90   Mean   :-87.65                     
    ##  3rd Qu.:41.93   3rd Qu.:-87.63                     
    ##  Max.   :42.18   Max.   :  0.00                     
    ##  NA's   :3714    NA's   :3714

``` r
#check null values
colSums(is.na(tripdata))
```

    ##            ride_id      rideable_type         started_at           ended_at 
    ##                  0                  0                  0                  0 
    ## start_station_name   start_station_id   end_station_name     end_station_id 
    ##             480360             480492             511267             511408 
    ##          start_lat          start_lng            end_lat            end_lng 
    ##                  0                  0               3714               3714 
    ##      member_casual 
    ##                  0

``` r
#calculate ops hours
tripdata$ride_length <- with(tripdata, 
                                    difftime(tripdata$ended_at, tripdata$started_at,
                                             units = "hours"))
tripdata$ride_length <- as.numeric(tripdata$ride_length, unites='hours')

#get date of the week (1=Sunday, 7=Saturday)
tripdata$day_of_week <- wday(tripdata$started_at)
#create subset of the data
sub_tripdata <- select(tripdata,
                                c('ride_id','rideable_type','member_casual','ride_length','day_of_week'))

#outliers
ggplot(sub_tripdata) + aes(x='',y=ride_length) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
```

![](Cyclistic_BikeShare_Case_Study_OliviaChen_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#remove inssuficient ride_length
sub_tripdata <- subset(sub_tripdata,ride_length > 0)
```

### Calculate the mean of ride_length and the max ride_length

``` r
summary(sub_tripdata$ride_length)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   0.0003   0.0900   0.1600   0.3080   0.2869 857.6900

The average length of ride is about 18.5 mins (0.308 hours) and the
maximum length of ride is about 14.3 days (857.69 hours).

### Calculate the mode of day of week

``` r
cal_mode <- function(x){which.max(tabulate(x))}
cal_mode(sub_tripdata$day_of_week)
```

    ## [1] 7

Riders are more likely to utilize the bike-share service on Saturday as
number 7 appears the most often in the data set.

``` r
ggplot(data = sub_tripdata) +
  geom_bar(mapping = aes(x = member_casual, fill = rideable_type)) +
  labs(title="Cyclistic Bike-share: Member Casual vs Riderble Type")
```

![](Cyclistic_BikeShare_Case_Study_OliviaChen_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The above bar chart indicates annual members has a stronger demand on
both classic bike and electric bike comparing to casual riders.

``` r
count(sub_tripdata,member_casual)
```

    ## # A tibble: 2 × 2
    ##   member_casual       n
    ##   <chr>           <int>
    ## 1 casual        1159085
    ## 2 member        1998595

The above numbers indicate there is about 63.29% of Cyclistic users are
annual members between January 2023 and July 2023.

``` r
ggplot(data=sub_tripdata) + geom_smooth(mapping=aes(x=ride_length, y=day_of_week), method = "gam")+
  geom_point(mapping=aes(x=ride_length, y=day_of_week,shape=member_casual, color = member_casual )) +
  labs(title="Cyclistic Bike-share: Ride Length vs Day of Week")
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](Cyclistic_BikeShare_Case_Study_OliviaChen_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

According to the above, casual riders, which are represented by red
circle, tend to ride for a long period of time across all days of the
week. While annual members, which are represented by blue triangles,
concentrate in a relatively shorter ride length.

### Create subset of the data for annual members

``` r
annual_members <- filter(sub_tripdata, member_casual=="member")
summary(annual_members$ride_length)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ##  0.000278  0.081111  0.141111  0.206679  0.243889 25.994444

The average ride length for annual members is about 12.4 minutes.

``` r
ggplot(data=annual_members) + geom_smooth(mapping=aes(x=ride_length, y=day_of_week), method = "gam")+
  geom_point(mapping=aes(x=ride_length, y=day_of_week,shape=member_casual )) +
  labs(title="Cyclistic Bike-share: Ride Length vs Day of Week",
       subtitle = "Annual Members Only")
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](Cyclistic_BikeShare_Case_Study_OliviaChen_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Now, let’s focus on the distributions of annual members. According to
the chart above, the distributions of ride length of annual members are
slightly right-skewed among all day of week.

## Recommendations

Due to the higher demand on both classic bikes and electric bikes for
annual members, I would recommend Cyclistic to reserve a few classic
bikes and electric bikes in each station for annual members only. Since
casual rider tend to ride for longer period of time, I would recommend
to increase the price for single-ride passes which will encourage riders
who use share-bike over 24 hours to convert to annual members.

## Next Steps

1)  Utilize the location variables such as start_lat, start_lng,
    end_lat, end_lng to identify some target locations for marketing
    campaign.
2)  Gather user information and consider launch incentive programs to
    maintain customers’ loyalty.
3)  Since the average ride length for annual member is about 12.4
    minutes, Cyclistic should consider investigate on the docking
    stations to ensure there will be enough docking stations near about
    12 minutes radius from the popular/high-demand stations.
