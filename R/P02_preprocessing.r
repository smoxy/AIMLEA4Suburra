library(httr)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)


#DATA
suburra <- read.csv("../DATA/01_Suburra_data.csv")
suburra_collapsed <- read.csv("../DATA/02_Suburra_data_collapsed.csv")

################################################################################
#####                          Analisi dei dati                            #####
summary(suburra)
summary(suburra_collapsed)

# Personaggi
personaggi <- unique(suburra$character)
personaggi <- sort(personaggi)

suburra %>%
  group_by(character, is_male) %>%
  count(sort = T)

suburra_collapsed %>%
  group_by(character, is_male) %>%
  count(sort = T) 

#numero di battute per personaggio per database
library(ggplot2)
fig <- ggplot(suburra, aes(x = character, fill = is_male)) + 
  geom_col()

fig


library(plotly)

x <- c('normal', 'collapsed')

y <- c(20, 14, 23)
y2 <- c(16,12,27)
text <- c('27% market share', '24% market share', '19% market share')
data <- data.frame(x, y, y2, text)
data <- suburra$character

fig <- data %>% plot_ly()
fig <- fig %>% add_trace(x = ~x, y = ~y, type = 'bar',
                         text = y, textposition = 'auto',
                         marker = list(color = 'rgb(158,202,225)',
                                       line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% add_trace(x = ~x, y = ~y2, type = 'bar',
                         text = y2, textposition = 'auto',
                         marker = list(color = 'rgb(58,200,225)',
                                       line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% layout(title = "January 2013 Sales Report",
                      barmode = 'group',
                      xaxis = list(title = ""),
                      yaxis = list(title = ""))

fig



# Function to convert ticks to time
convert_ticks_to_time <- function(ticks){
    tick_rate = 10000000
    seconds = ticks / tick_rate
    minutes = floor(seconds / 60)
    seconds = seconds %% 60
    milliseconds = floor((seconds - floor(seconds)) * 1000)
    seconds = floor(seconds)
    time = sprintf("%02d:%02d.%03d", minutes, seconds, milliseconds)
    return(time)
}

# 1. Read data and setting up
get_data <- function(fileName = "01_Suburra_data.csv"){
    wd <- getwd()
    df <- read_csv(file.path(wd, 'DATA', fileName))
    return(df)
}

get_badwords <- function(){
    bad_w1 <- content(GET("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/it"), "text")
    bad_w2 <- content(GET("https://raw.githubusercontent.com/napolux/paroleitaliane/master/paroleitaliane/lista_badwords.txt"), "text")
    badwords <- unique(c(str_split(bad_w1, "\n")[[1]], str_split(bad_w2, "\n")[[1]]))
    return(badwords)
}

# 2. lower case all script lines
lower_case <- function(df){
    df <- df %>% mutate(script_line = tolower(script_line))
    write_csv(df, file.path(getwd(), 'DATA', '01_Suburra_data.csv'))
    return(df)
}

# 2.5. set bad words
set_badwords <- function(df){
    badwords <- get_badwords()
    df <- df %>% mutate(bad_words = map_int(str_split(script_line, " "), ~ifelse(any(.x %in% badwords), 1, 0)))
    write_csv(df, file.path(getwd(), 'DATA', '01_Suburra_data.csv'))
    return(df)
}

# 3. collapse script lines of the same character that are consecutive and drops empty rows
collapse_consecutive_lines <- function(df){
    df <- df %>%
        mutate(script_line = if_else(lead(character, default = first(character)) == character & !is.na(character), str_c(script_line, lead(script_line, default = first(script_line)), sep = " "), script_line),
               duration = if_else(lead(character, default = first(character)) == character & !is.na(character), duration + lead(duration, default = first(duration)), duration),
               begin = if_else(lead(character, default = first(character)) == character & !is.na(character), begin, begin)) %>%
        filter(!is.na(script_line))
    write_csv(df, file.path(getwd(), 'DATA', '02_Suburra_data_collapsed.csv'))
    return(df)
}

# 4. remove punctuation
remove_punctuation <- function(fileName){
    df <- get_data(fileName)
    df <- df %>% mutate(script_line = str_replace_all(script_line, "[[:punct:]]", ""))
    write_csv(df, file.path(getwd(), 'DATA', fileName))
    return(df)
}

df <- get_data()
print('lowering case ...')
df <- lower_case(df)
print('setting bad words ...')