# Tidytuesday week 40 - NBER papers
# Author: Ana Crispim
# @anacaarlac

#Load packages -----------------------------------------------------
library(nberwp)
library(gender)
library(tidyverse)
library(sjmisc)
library(devtools)
library(stringi)
library(sysfonts)
library(showtext)
library(magick)

# Load data -----------------------------------------------------
authors <- nberwp::authors
paper_authors <- nberwp::paper_authors
paper_programs <- nberwp::paper_programs
papers <- nberwp::papers
programs <- nberwp::programs

# Build gender datatable -----------------------------------------------------

# Create column with first names
# ref: https://github.com/JuanmaMN/TidyTuesday/blob/master/2021/September/TidyTuesday_28_9_2021.R
authors2 <- authors %>% 
  mutate(first_name = stri_extract_first(name, regex="\\w+"),
         first_name = str_to_lower(first_name))

# Add columns with years range to use gender_df, which needs the year range to calculate gender probability
authors2$year_min <- 1940
authors2$year_max <- 2003

# Adding gender to dataframe
authors_w_gender <- gender_df(authors2, name_col = "first_name", 
                              year_col = c("year_min", "year_max"), 
                              method="ssa")

authors_w_gender <- unique(authors_w_gender)

# Data preparation --------------------------------------------------------

# Merge dataframes using first_name and name columns
# There 13.259 cases in which gender was defined using gender_df
authors_df <- merge(authors2, authors_w_gender, by.x = "first_name", by.y = "name")

# Select columns to create data table
authors_df <- authors_df %>% 
  select(author, name, gender)

# Add information of programs and papers for each author
# There are 116.131 observations because some authors published more than one paper.
# A few papers were categorised in more than one program description.
authors_df2 <- authors_df %>% 
  left_join(paper_authors, by = "author")%>%
  left_join(paper_programs, by = "paper")%>%
  left_join(programs, by = "program") %>%
  left_join(papers, by = "paper") %>% 
  na.omit()

# Check number of program descriptions. N = 20
frq(authors_df2$program_desc)

# Summarise descriptives according to program description
authors_df2_table <- authors_df2 %>% 
  group_by(program_desc,year,gender) %>%  
  summarise(n=n())

# Fonts -------------------------------------------------------------------
# ref: https://github.com/JuanmaMN/TidyTuesday/blob/master/2021/September/TidyTuesday_28_9_2021.R
extrafont::loadfonts(device = "win", quiet = TRUE)
font_add_google("Quicksand")
font_name <- "Quicksand"
showtext_auto()

# Graph GIF -------------------------------------------------------------------
# Get a list of unique values for years 
years <- 
  authors_df2_table %>%
  pull(year) %>%
  unique(.) %>%
  sort(.)

# Create object with saving path
dir_out <- file.path(getwd(), "plots")
dir.create(dir_out, recursive = TRUE)

# Create function to compute plots for each year with data available
## Create a loop through years
## Subset data 
## Create barplots with number of papers according to gender
for (y in years) {
  
  plot <- 
    authors_df2_table %>% 
    filter(year == y) %>% 
    ggplot(aes(x= program_desc, y = n, group=gender, fill=gender)) +
    geom_bar(stat = "identity", position="dodge", width = 0.6, show.legend=TRUE) + 
    geom_text(aes(label = n), size = 6, vjust = 0.3, hjust = -0.8, position=position_dodge(width=0.9)) +
    scale_fill_brewer(palette = "Blues") +
    coord_flip() +
    labs(y = "",
         x = "",
         title = y,
         subtitle = "Number of NBER papers per program description and gender across years",
         caption = "Source: #TidyTuesday. Author: Ana Crispim (Twitter @anacaarlac)")  +
    scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, by = 200)) +
    theme(plot.caption =  element_text(
      margin = margin(t = 30),
      color = "#22222b", 
      size = 20,
      hjust = 0,
      family = font_name),
      plot.title.position = "plot", 
      plot.caption.position =  "plot", 
      plot.title = element_text(
        color = "#22222b", 
        size = 34,
        face= "bold",
        family = font_name),
      plot.subtitle = element_text(     
        color = "#22222b", 
        size = 25,
        face = "italic",
        family = font_name),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        margin = margin(t = 5),
        family = font_name,
        size = 25,
        color = "#525252"),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#FCF8F8",
                                     color = NA),    # color removes the border,
      axis.ticks.y = element_blank(),
      legend.position = "right",
      legend.background = element_rect(fill="#FCF8F8",
                                       size= 0.1, 
                                       colour = NA),
      legend.text = element_text(colour="black", 
                                 size = 20),
      legend.title = element_text(colour="black",
                                  size = 20)
    ) 
  
  fp <- file.path(dir_out, paste0(y, ".png"))
  
  ggsave(plot = plot, 
         filename = fp,
         height = 5.5,
         width = 6,
         device = "png")
  
}

## List file names and read in
images <- list.files(dir_out, full.names = TRUE)
images_list <- lapply(images, image_read)

## join the images together
images_joined <- image_join(images_list)

## animate at 2 frames per second
images_animated <- image_animate(images_joined, fps = 2)

## save to disk
image_write(image = images_animated,
            path = "NBER gif.gif")
