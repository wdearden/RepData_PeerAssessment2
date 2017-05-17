library(ggplot2)
library(magrittr)
library(tidyverse)
library(lubridate)
library(forcats)
library(scales)
library(ggalt)

storms_readin <- read.csv("repdata%2Fdata%2FStormData.csv.bz2") %>% as_tibble()

storms <- storms_readin %>%
    mutate(year = BGN_DATE %>% mdy_hms() %>% year())

storms <- storms %>%
    select(EVTYPE, year, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP)

storms <- storms %>%
    mutate(PROPDMG =
        case_when(
            .$PROPDMGEXP == "K" ~ .$PROPDMG * 1000,
            .$PROPDMGEXP == "M" ~ .$PROPDMG * 1000000,
            .$PROPDMGEXP == "B" ~ .$PROPDMG * 1000000000,
            TRUE ~ .$PROPDMG
        )
    ) %>%
    select(-PROPDMGEXP)

storms <- storms %>%
    filter(year >= 2007) %>%
    group_by(EVTYPE) %>%
    summarize_at(vars(-year), funs(sum))

storms_fatal <- storms %>%
    arrange(desc(FATALITIES)) %>%
    slice(1:10)

storms_injuries <- storms %>%
    arrange(desc(INJURIES)) %>%
    slice(1:10)

storms_propdmg <- storms %>%
    arrange(desc(PROPDMG)) %>%
    slice(1:10)

