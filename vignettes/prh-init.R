## ----setup, include = FALSE----------------------------------------------
devtools::load_all(".")
library(tidyverse)
library(lubridate)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE,
  cache.lazy = FALSE
)

## ----read_raw------------------------------------------------------------
raw_prh <- init_prh("mn180607-44", "44")

## ------------------------------------------------------------------------
head(raw_prh@rawdata)

## ------------------------------------------------------------------------
# Find backskips
timestamps <- raw_prh@rawdata$datetimeUTC
backskips <- which(lead(timestamps) < timestamps)
backskips

# Find the boundaries around the first backskip
foreskips <- which(difftime(lead(timestamps), timestamps, units = "secs") > 1)
gap_begin <- timestamps[foreskips[1]]
backskip_begin <- timestamps[foreskips[1] + 1]
backskip_end <- timestamps[backskips[1]]
gap_end <- timestamps[backskips[1] + 1]
  
raw_prh@rawdata %>%
  mutate(timestamp_order = seq_along(datetimeUTC)) %>%
  filter(between(datetimeUTC, 
                 gap_begin - seconds(0.5), 
                 gap_end + seconds(1.5))) %>%
  ggplot(aes(timestamp_order, datetimeUTC)) +
  geom_point() +
  annotate("rect", 
           xmin = foreskips[1], xmax = backskips[1] + 1, 
           ymin = gap_begin, ymax = gap_end, 
           fill = "red", alpha = 0.5) +
  annotate("rect", 
           xmin = foreskips[1] + 1, xmax = backskips[1], 
           ymin = backskip_begin, ymax = backskip_end, 
           fill = "blue", alpha = 0.5) +
  scale_y_datetime(date_labels = "%H:%M:%OS1") +
  labs(x = "Timestamp Order",
       y = "Timestamp") +
  theme_classic()

## ------------------------------------------------------------------------
fixbackskip_prh <- fix_backskip(raw_prh)
timestamps <- fixbackskip_prh@rawdata$datetimeUTC
new_gap_end <- which(timestamps == gap_end)
fixbackskip_prh@rawdata %>%
  mutate(timestamp_order = seq_along(datetimeUTC)) %>%
  filter(between(datetimeUTC, 
                 gap_begin - seconds(0.5), 
                 gap_end + seconds(1.5))) %>%
  ggplot(aes(timestamp_order, datetimeUTC)) +
  geom_point() +
  annotate("rect", 
           xmin = foreskips[1], xmax = new_gap_end, 
           ymin = gap_begin, ymax = gap_end, 
           fill = "red", alpha = 0.5) +
  scale_y_datetime(date_labels = "%H:%M:%OS1") +
  labs(x = "Timestamp Order",
       y = "Timestamp") +
  theme_classic()

## ------------------------------------------------------------------------
# Remove raw_prh to open up some memory
rm(raw_prh)

# Find gaps greater than 0.1s
timestamps <- fixbackskip_prh@rawdata$datetimeUTC
gaps <- which(as.numeric(lead(timestamps) - timestamps, units = "secs") > 0.1)
gaps

# Find the boundaries around the first gap
gap_begin <- timestamps[gaps[1]]
gap_end <- timestamps[gaps[1] + 1]

fixbackskip_prh@rawdata %>%
  mutate(timestamp_order = seq_along(datetimeUTC)) %>%
  filter(between(datetimeUTC, 
                 gap_begin - seconds(0.5), 
                 gap_end + seconds(1.5))) %>%
  ggplot(aes(timestamp_order, datetimeUTC)) +
  geom_point() +
  scale_y_datetime(date_labels = "%H:%M:%OS1") +
  labs(x = "Timestamp Order",
       y = "Timestamp") +
  theme_classic()

## ------------------------------------------------------------------------
fixgap_prh <- fix_gap(fixbackskip_prh)

fixgap_prh@rawdata %>%
  mutate(timestamp_order = seq_along(datetimeUTC)) %>%
  filter(between(datetimeUTC, 
                 gap_begin - seconds(0.5), 
                 gap_end + seconds(1.5))) %>%
  ggplot(aes(timestamp_order, datetimeUTC)) +
  geom_point() +
  scale_y_datetime(date_labels = "%H:%M:%OS1") +
  labs(x = "Timestamp Order",
       y = "Timestamp") +
  theme_classic()

## ------------------------------------------------------------------------
# Remove fixbackskip_prh to open up some memory
rm(fixbackskip_prh)

# How big is the raw data?
object.size(fixgap_prh)

# Decimate data
deci_prh <- decimate_prh(fixgap_prh, new_freq = 10)

# How big is the decimated data?
object.size(deci_prh)

## ------------------------------------------------------------------------
# Remove fixgap_prh to open up some memory
rm(fixgap_prh)

# Set the tag on/off times
on <- as.POSIXct("2018-06-07 15:46:18", tz = "UTC")
off <- as.POSIXct("2018-06-07 18:46:38", tz = "UTC")
trim_prh <- trim_data(deci_prh, use_gui = FALSE, tagon = on, tagoff = off)

## ------------------------------------------------------------------------
# Remove deci_prh
rm(deci_prh)

# Where are the gaps in sensor readings?
# See rle help for details on run length encoding
sensor_gaps <- rle(is.na(trim_prh@rawdata$accX))
sensor_gaps
# The second gap (15 records) is the longest, beginning at index 2086
gap_start_ix <- 2086
gap_end_ix <- 2086 + 15
gap_start <- trim_prh@rawdata$datetimeUTC[gap_start_ix]
gap_end <- trim_prh@rawdata$datetimeUTC[gap_end_ix]

# Plot gap
filter(trim_prh@rawdata,
       between(datetimeUTC, gap_start - seconds(2), gap_end + seconds(2))) %>%
  ggplot(aes(datetimeUTC, accX)) + 
  geom_point() +
  scale_x_datetime(date_labels = "%H:%M:%OS1") +
  labs(x = "Timestamp",
       y = "accX") +
  theme_classic()

# Interpolate gaps
prh <- interp_gaps(trim_prh)

# Plot gap after interpolation
filter(prh@rawdata,
       between(datetimeUTC, gap_start - seconds(2), gap_end + seconds(2))) %>%
  ggplot(aes(datetimeUTC, accX)) + 
  geom_point() +
  scale_x_datetime(date_labels = "%H:%M:%OS1") +
  labs(x = "Timestamp",
       y = "accX") +
  theme_classic()

