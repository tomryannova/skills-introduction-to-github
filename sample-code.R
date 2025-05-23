library(tidyverse)
library(rio) 
population_data_2 <- read_csv(file = "population-by-state.csv")


population_data_2

summarize ( .data = population_data_2, mean_populatin = mean ( Pop) )

# Calculate the mean population of the five largest states
population_data_2 %>%
  filter(rank <=5) %>%
  summarize( mean_population = mean(Pop))


gapminder_10_rows <- read_csv("https://data.rfortherestofus.com/data/gapminder_10_rows.csv")

gapminder_10_rows

ggplot(
  data= gapminder_10_rows,
  mapping = aes (
    x = year,
    y = lifeExp,
    fill = year
  )
) + geom_col() + scale_fill_viridis_c() + theme_minimal()
  geom_bar() 
geom_line() +
geom_point()


ggplot(
  data= gapminder_10_rows,
  mapping = aes (
    x = year,
    y = lifeExp,
    fill = year
  )
) + geom_col() + theme_minimal()

# the following is a json file
dm_perc_cat_hubs <- import("https://data.rfortherestofus.com/dm_perc_cat_hubs.json")
dm_perc_cat_hubs

southwest_2003 <- dm_perc_cat_hubs %>%
  filter(hub == "Southwest") %>%
  filter(year == 2003)
southwest_2003

ggplot(
  data=southwest_2003, 
  aes(
    x=week,
    y=percentage ,
    fill=category
  )
) + geom_col()  + 
  scale_fill_viridis_d(
    option = "rocket"
  ) 

ggplot(
  data=southwest_2003, 
  aes(
    x=week,
    y=percentage ,
    fill=category
  )
) + geom_col()  + 
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1 
  ) 

ggplot(
  data=southwest_2003, 
  aes(
    x=week,
    y=percentage ,
    fill=category
    )
) + geom_col()  + 
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1 
  ) + labs (x = "Week of Year") + 
  scale_x_continuous (
    name = NULL
    ,
    guide = "none"
#    position = "top"
  ) + 
  scale_y_continuous(
    name = NULL   #name of the y axis
    ,   
    labels = NULL  #the labels on the y axis 
    ,
    position = "right"  #this puts the hash marks / labels on the right side of the graph
  )

dm_perc_cat_hubs %>%
  filter( hub %in% c(
    "Northwest",
    "California",
    "Southwest",
    "Northern Plains"
  )) %>%
  ggplot(aes(
    x = week,
    y = percentage,
    fill = category
  )) +
  geom_col() + 
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1 
  ) + 
  scale_x_continuous(
    name = NULL,
    guide = "none"
  ) +
  scale_y_continuous(
    name = NULL,
    labels = NULL,
    position = "right"
  ) + facet_grid(
    rows = vars(year),
        cols = vars(hub)
    ,
    switch = "y"
  ) + theme_light(base_family = "Roboto") +
  theme(
    axis.title = element_text (  # ?? not sure what this does 
      size = 14,
      color = "black"
    )
 , axis.text = element_text (   # ?? not sure what this does 
      family = "Roboto Mono",
      size = 11 
      ),
   axis.line.x = element_blank(),
  axis.line.y = element_line(  # sets the y axis border/line color and thinkness
    color = "black",
    size = .2
  ),
 axis.ticks.y = element_line ( # sets the color/thinkness of the tick marks on the y axis 
   color = "black",
   size = .2
 ),
 axis.ticks.length.y = unit ( 2, "mm" ),  #sets the length of the tick marks on y axis
 legend.position = "top" ,
 legend.title = element_text (   #think this modified "category" to blue/bold ) 
   color = "#2DAADA",
   face = "bold"
 )
 ,
 legend.text = element_text( color = "#2DAADA")  #turns the D0 - D4 to blue/bold
 , strip.text.x = element_text (
    hjust = .5,     # this is the left / right adjust
    face = "plain",  # plain, bold etc of subgroup header text 
    color = "black",     # color of subgroup header text 
    margin = margin ( t = 20, b = 5 )   # this is the background of the subgroup header window
                                        # t is the margin above the text, b is the margin below the text
  ),
 strip.text.y.left = element_text ( # formatting for  y axis grouping 
   angle = 0  ,  # the 'flip' for printing y axis grouping 
   vjust = .5 ,    # vertical adjust of the y axis grouping 
   face = "plain",
   color = "black"
 )
 ,
 strip.background = element_rect (  # borders and backgrounds, both x and y axis
   fill = "transparent",    #. The color of the fill
   color = "transparent"   #. The color of the border line
 )
    
)

vars(year)  
c(1,7:9)45
c(1:5, 10.5, "next")

x <- 1:4
x
c(x)
names(x) <- letters[1:4]
names(x)
as.vector(x)  # no names
dim(x) <- c(2,2)
dim(x) 
x

