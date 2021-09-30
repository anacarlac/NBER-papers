#Open git
library(usethis)
use_github() 

#Load packages -----------------------------------------------------
library(nberwp)
library(gender)
library(tidyverse)
library(sjmisc)
library(devtools)
#install_github("ropensci/genderdata")
library(gender)

pacman::p_load(readxl, lubridate, tidyverse, ggplot2, hrbrthemes, ggfittext, 
               patchwork, scales, ggtext, ggpubr,ggbump,wesanderson,
               grid, gridtext, biscale, cowplot,sysfonts,ggimage,extrafont,
               systemfonts, showtext, ggbeeswarm,stringi)

# Load data -----------------------------------------------------
authors <- nberwp::authors
paper_authors <- nberwp::paper_authors
paper_programs <- nberwp::paper_programs
papers <- nberwp::papers
programs <- nberwp::programs

# Build gender datatable -----------------------------------------------------
authors2 <- authors %>% 
  mutate(first_name = stri_extract_first(name, regex="\\w+"),
         first_name = str_to_lower(first_name))

authors2$year_min <- 1940
authors2$year_max <- 2003

# Testing methods
gender("adi", method ="ssa")
gender("ana", method ="ssa")

# Adding gender to dataframe
authors_w_gender <- gender_df(authors2, name_col = "first_name", 
          year_col = c("year_min", "year_max"), 
          method="ssa")

authors_w_gender <- unique(authors_w_gender)

# Data preparation --------------------------------------------------------
# ref: https://github.com/JuanmaMN/TidyTuesday/blob/master/2021/September/TidyTuesday_28_9_2021.R

authors_df <- merge(authors2, authors_w_gender, by.x = "first_name", by.y = "name")

authors_df <- authors_df %>% 
  select(author, name, gender)


authors_df2 <- authors_df %>% 
  left_join(paper_authors, by = "author")%>%
  left_join(paper_programs, by = "paper")%>%
  left_join(programs, by = "program") %>%
  left_join(papers, by = "paper") %>% 
  na.omit()

authors_df2_table <- authors_df2 %>% 
  group_by(program_desc,year,gender) %>%  
  summarise(n=n())

