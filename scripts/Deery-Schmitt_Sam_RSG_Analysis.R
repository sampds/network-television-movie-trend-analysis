# Author: Sam Deery-Schmitt
# Subject: Working with RSG Media Data
# 12/15 - 12/18, 2021

packrat::init()

# if packrat::init() fails then run the pkg install function and then run packrat::init()

# List the packages to install
pkg <- c("packrat", "dplyr", "htmlwidgets", "rgl", "scatterplot3d", "ggplot2", "grid", "gridExtra", "cowplot")

# Use this function to install packages
install_if_missing <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
}

# Now apply the function on the packages
install_if_missing(pkg)

# data manipulation
require(dplyr)
# interactive 3d plot
require(htmlwidgets)
require(rgl)
# 3d plot
require(scatterplot3d)
# bar graphs with a bunch of dimensions
library(ggplot2)
# was using for side-by-side plotting, actually using cowplot for that instead
# ggplot objects don't work with par(mfrow()) so have to use this, lattice, or something similar
require(grid)
require(gridExtra)
# for side-by-side plots, and extracting the legend and creating a legend object
require(cowplot)

# run if having issues
# unlink("./packrat", recursive = TRUE)
# packrat::clean()
# packrat::restore()

packrat::snapshot()

# run this if getting wrong snapshot
# packrat::snapshot(ignore.stale=TRUE)

# files
"Top_Movie_Ranker_Full_Data.csv"
# download as a text file from 
url <- "https://public.tableau.com/app/profile/shiv8535/viz/ShivMOVIEANALYSISv2/MedianIncomebyFemaleSkew?publish=yes"
# page on far right titled Top_Movie_Ranker
# converted to csv in excel
# quarters: Q4 2016 - Q4 2017
# Targets: P18-49, F18-49, M18-49

movie_data <- read.csv("C:\\Users\\SamDe\\Desktop\\MS\\RSG\\Top_Movie_Ranker_Full_Data.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE)
# str(movie_data)
# 114373 obs of 17 variable

####### NOTE
# I did not use this dataset, but I would like to in the future, it is much more comprehensive
"GENRE_ROI_BY_DAYPART_Full_Data"
# dowload from url, page titled "GENRE ROI BY DAYPART
# converted to csv in excel
# quarters: Q4 2016 - Q4 2017
# Targets: P18-49, F18-49, M18-49
# # roi_data <- read.csv("C:\\Users\\SamDe\\Desktop\\GENRE_ROI_BY_DAYPART_Full_Data.csv",
#                      header = TRUE,
#                      stringsAsFactors = FALSE)

# str(roi_data)
# 160017 obs of 66 variables


############################################################################
# working with move_data, from Top_Movie_Ranker page of tableau workbook
# cleaning

# table(movie_data$Distr..Mdn..Age.)
# View(movie_data)
# there are 49 0s
# remove 0s
movie_data <- movie_data[movie_data$Distr..Mdn..Age. != 0, ]
# table(movie_data$Distr..Mdn..Age.)

View(movie_data)

########################
# function to create an ordered vector that corresponds to alphabetical order of categorical variable
# this is because in ggplot you cannot order by a factor if it's the fill color
# you can only order by a factor if it's on an axis

# I want to eventually control the order of bars on my plot by genre
# and set the x-axis to something else

# i'm making this a function so i can do it with different data sets
# this function takes a dataframe and a vector within that dataframe
# the vector needs to be something you want to create factors out of
# then it will create a new vector within the dataframe called plot.order
# which will have the corresponding numbers for the factor levels
# the default is to use the unique values of the group_to_order_by as the levels
# in alphabetical order

# update 
# I did all this and graphed it and decided the bar graphs look better
# in descending order of value, not with the same order of genres every time

create_ordered_vector <- function(df, group_to_order_by) {
  # create a new vector the same length as the target vector, with identical values
  df$plot.order <- group_to_order_by
  
  # substitute the values with numbers that correspond to alphabetical order
  # need to first make a dataframe that contains the value to assign
  
  value.vec <- c(seq(1, length(unique(group_to_order_by))))
  group.vec <- sort(unique(group_to_order_by))
  assignment.df <- data.frame(group.vec, value.vec)
  
  # iterate over the input dataframe, creating a new vector that contains
  # the level number corresponding to the factor levels of the input vector
  for (i in assignment.df$group.vec) {
    df$plot.order[group_to_order_by == i] <- assignment.df[assignment.df$group.vec == i, 2]
  }
  df$plot.order <- as.numeric(df$plot.order)  
}
movie_data$plot.order <- create_ordered_vector(movie_data, movie_data$GN.Top.Genre)
#######################################################
# back to cleaning

# only look at male and female targets
movie_data_18_49 <- movie_data[movie_data$Target %in% c("M18-49", "F18-49"), ]
View(movie_data_18_49)

colnames(movie_data_18_49)[c(1:2, 7:8, 11)] <- c("Program.Network", "Daypart", "Average.Audience", "Average.Audience%", "Median.Age")
# colnames(movie_data_18_49)

# look at the broader category, P18_49 only
movie_data_P18_49 <- movie_data[movie_data$Target %in% "P18-49", ]
colnames(movie_data_P18_49)[c(1:2, 7:8, 11)] <- c("Program.Network", "Daypart", "Average.Audience", "Average.Audience%", "Median.Age")
# this will be used for a hist/density plot of age

#######################################################
# aggregating

# this loads the dataframe into memory, it makes it so you don't have to call the
# dataframe within functions that take vectors as arguments
# ie instead of df$vector you can just input vector

attach(movie_data_18_49)

agg <- aggregate(cbind(GRPs, Average.Audience) ~ Program.Network + Daypart + Target + Network.Group + GN.Top.Genre, data = movie_data_18_49, FUN =sum)
# View(agg)

# I need to add ages to this, it would basically be the equivalent of doing a JOIN
# taking the median of the medians
# also need to add the order to plot by, the median will just be value since they are all the same
agg.age <- aggregate(cbind(Median.Age, plot.order) ~ Program.Network + Daypart + Target + Network.Group + GN.Top.Genre, data = movie_data_18_49, FUN = median)
View(agg.age)

# make sure dimensions are the same
#dim(agg.age)
# dim(agg)
# add the median ages as a new column
# add the plot order as a new column

agg$Median.Age <- agg.age$Median.Age
agg$plot.order <- agg.age$plot.order


# now to access the dataframe's vectors will have to call the whole dataframe first
detach(movie_data_18_49)

attach(agg)
# colnames(agg)
# summary(agg$Median.Age)
# inital exploratory plot
# plot(Average.Audience, GRPs)
# max(GRPs)

# might be able to find some interesting relationships in 3D

# aggregation just by genre for m/f genre plot, all dayparts
agg.only.genre.grps <- aggregate(cbind(GRPs) ~ Target + GN.Top.Genre, data = movie_data_18_49, FUN =sum)

# aggregation by daypart and grp for box plot
agg.only.daypart.grps <- aggregate(cbind(GRPs) ~ Target + Daypart, data = movie_data_18_49, FUN =sum)
###############################################################
# 3d plots

# colors RSG uses for male/female
female_color<- "#F2BBCF"
male_color <- "#A0CBE8"

# rgb so can easily set alpha
female_rgb <- rgb(242, 187, 207, 255/2, maxColorValue = 255)
male_rgb <- rgb(160, 203, 232, 255/2, maxColorValue = 255)
# these colors are actually kind of hard to see in rgl, will use darker colors

col.vec <- rep("blue", length(Target))
col.vec[Target == "F18-49"] <- "red"
# col.vec
# i changed the color vector so it won't work anymore for this plot
# it is now for the rgl plot
# scatterplot3d(Average.Audience, GRPs, Median.Age, color = col.vec, pch = 16)

# open the window for the rgl plot
open3d() 
# plot 3d interactive object
# each point is a movie \ network pair
# also add 95th percentile planes

plot3d(Average.Audience, GRPs, Median.Age, col = col.vec,  cex = 1)
bg3d(col = "dark gray")
# plane at 95% percentile of average audience
# quantile(Average.Audience, probs = 0.95)
# which is 200
# points to calculate equation are (200,0,18);(200,434.4,18);(200,0,49)
planes3d(a = 13466.4, b = 0, c = 0, d= -2693280, color = "black", alpha = 0.5, shininess = 100, emission = "black")

# plane at 95th percentile of GRPs
# quantile(GRPs, probs = 0.95)
# which is 51.843
# points to calculate equation are (0,51.843, 18); (1498, 51.843, 18); (0,51.843,49)
planes3d(a = 0, b = -46438, c = 0, d= 2407490, color = "black", alpha =0.5, shininess = 100, emission = "black")


# plot same scatterplot with 99th percentile planes, and add point selection
plot3d(Average.Audience, GRPs, Median.Age, col = col.vec,  cex = 1)

# planes for 99th percentiles
# quantile(Average.Audience, probs = 0.99)
# 400
# (400,0,18);(400,434.4,18);(400,0,49)
planes3d(a = 13466.4, b = 0, c = 0, d= -5386560, color = "gray", alpha = 0.5, shininess = 50)

#quantile(GRPs, probs = 0.99)
# 102.8672
# (0,102.8672, 18); (1498, 102.8672, 18); (0,102.8672,49)
planes3d(a = 0, b = -46438, c = 0, d= 4776950, color = "gray", alpha =0.5, shininess = 50, emission = "black")

# this makes it so user can click on points and it will show the Program and Network
# once you quit (middle mouse button) it returns the plotIDs
# they correspond to the row number in the dataframe
# running this will save the output as a vector which can be passed into
# the dataframe to return the movie information

selected_plots <- identify3d(Average.Audience, GRPs, Median.Age, labels = Program.Network, 
                             plot = TRUE, tolerance = 20, 
                             buttons = c("right", "middle"))
Program.Network[selected_plots]

# save the current view of the plot as a png
out.dir <- "C:\\Users\\SamDe\\Desktop\\"
my.rgl.out <- paste0(out.dir, "movie3dvisualization.png")
rgl.snapshot(filename = my.rgl.out)
detach(agg)

#####################################################################
# more aggregating

# i want the highest GRP for:
# each Daypart, Target, GN.Top.Genre, Median.Age

agg_top <- agg %>% group_by(Daypart, Target, Median.Age, GN.Top.Genre, plot.order) %>% slice_max(order_by = GRPs)
agg_top <- as.data.frame(agg_top[order(agg_top$Median.Age, agg_top$Daypart, agg_top$Target) , ])

# unique(agg_top$Median.Age)
# str(agg_top)
# summary(agg_top)
# dim(agg_top)
# View(agg_top)

###################################################################
# density plot
# use movie_data_P18_49 to get the distribution of age
# d.age <- density(movie_data_P18_49$Median.Age)
# plot(d.age, type = "n",
#      main = "Age Distribution",
#      ylab = "Density"
#      , xlab = "")
# polygon(d.age, col = "orange", border = "#cccccc")

# actually i think I will stack two density plots on eachother and make one transparent
# will adjust opacity in illustrator, for now will just use the color as is
female_rgb <- rgb(242, 187, 207, 255, maxColorValue = 255)
male_rgb <- rgb(160, 203, 232, 255, maxColorValue = 255)

d.age.F <- density(movie_data_18_49[movie_data_18_49$Target == "F18-49", ]$Median.Age)
plot(d.age.F, type = "n",
     main = "Age Distribution",
     ylab = "Density"
     , xlab = ""
     , ylim = c(0,0.08))
polygon(d.age.F, col = female_rgb, border = "#cccccc")

d.age.M <- density(movie_data_18_49[movie_data_18_49$Target == "M18-49", ]$Median.Age)
plot(d.age.M, type = "n",
     main = "Age Distribution",
     ylab = "Density"
     , xlab = "")
polygon(d.age.M, col = male_rgb, border = "#cccccc")


################################################################
# bar plot, box plot
# use agg.only.daypart.grps
agg.only.daypart.grps$plot.order <- c(2,2,3,3,1,1,5,5,4,4)

ggplot(agg.only.daypart.grps, aes(x = reorder(Daypart, plot.order), y = GRPs, fill = Target)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Daypart") +
  # geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
  ggtitle ("GRP by Daypart") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("#F2BBCF", "#A0CBE8"))
# i think this will look better as a boxplot
ggplot(agg.only.daypart.grps, aes(x = reorder(Daypart, plot.order), y = GRPs, fill = Target)) +
  geom_boxplot() +
  xlab("Genre") +
  # geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
  ggtitle ("GRP by Daypart") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("#F2BBCF", "#A0CBE8"))

################################################################
# bar plots

# get information for only 18 year olds
age_18 <- agg_top[ agg_top$Median.Age == 18, ]

# I was trying to create a plot that had the movie name as a text label
# for every column
# however, i can't seem to figure out how to align the text correctly
# I can't figure out how to make each block of text rotate around its own axis,
# it just rotates around the center of the plot
# figured out a different way to do it though

# here's what i was trying

# ggplot(age_18, aes(x = Daypart, y = GRPs, fill = GN.Top.Genre)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   # theme(axis.text.x = element_text(angle = 90))
#   geom_text(aes(label = Program.Network), cex = 2)

# messing around with parameters
# text <- ggplot(age_18, aes(x = factor(Daypart), y = GRPs, label = Program.Network)) +
#   # geom_text(aes(label = Program.Network), cex = 3, angle = 90, vjust = "outward")
#   geom_text(aes(vjust = GN.Top.Genre))
# text

# this actually works, but it's just for one daypart

age_18_day <- age_18[age_18$Daypart == "Daytime", ]  


age_18_day$GN.Top.Genre <- factor(age_18_day$GN.Top.Genre, 
                                  levels = unique(age_18_day$GN.Top.Genre))                               
# this is a pretty hacky way to do this but it works
# i want to order the bars by the genre, not by the film title or by the GRPs
# I cannot get it to work, so I am creating a new vector that will have the factor order for each genre as a number
# first create a vector of same length

# colors RSG uses
action_blue <- "#1F77B4"
children_blue <- "#AEC7E8"
comedy_orange <- "#F2620F"
drama_green <- "#2CA02C"
horror_green <- "#3C6E1E"
romance_red <- "#D62728"
romantic_cmdy_pink <- "#FF9896"
science_fiction_purple <- "#9467BD"
suspense_purple <- "#402B51"
rsg_pallete <- c(action_blue, children_blue, comedy_orange, drama_green, 
                 horror_green, romance_red, romantic_cmdy_pink, 
                 science_fiction_purple,suspense_purple)

# create a vector that has a value for fantasy, RSG doesn't have it in their so make one
rsg_pallete_fantasy <- c(rsg_pallete[1:4], "#FFFFFF", rsg_pallete[5:9])

# to make this work on plots that don't have all 10 of these genres, it is necessary to
# create a color vector for each genre
# i could write a function to do it, but for now I'm just going to create a unique vector when necessary


ggplot(age_18_day, aes(x = reorder(Program.Network, -GRPs), y = GRPs, fill = GN.Top.Genre)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Program\\Network") +
  geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
  ggtitle ("18-Year-Old Daytime") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# also going to make a male/female by genre plot
# use agg.only.genre.grps
ggplot(agg.only.genre.grps, aes(x = reorder(GN.Top.Genre, -GRPs), y = GRPs, fill = Target)) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Genre") +
  # geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
  ggtitle ("GRP by Genre and Sex") +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("#F2BBCF", "#A0CBE8"))

max(agg.only.genre.grps$GRPs)

#####################################################
# learned that you can't convert a dictionary value to a dataframe
#####################################################
# library(hash)
# this lets me make dictionaries
# was trying to store the dataframes as values in dictionary with key being the df name
# but that didn't work so nvm
# it's so i could iterate over the dataframes i made by subsetting instead of calling them by name

# this is what i had in the function

# dfs <- hash()
# dfs[["overnight"]] <- overnight
# dfs[["morning"]] <- morning
# dfs[["daytime"]] <- daytime
# dfs[["early_fringe"]] <- early_fringe
# dfs[["prime"]] <- prime
# dfs
# for (key in keys(dfs)){
#   print(dfs[key])
# }

# for (key in keys(dfs)) {
#   converted <- as.data.frame(dfs[key])
# can't convert the value to a dataframe

# hav to just manually put in the dataframe names

####################################################
# function to iterate over age, printing barplots for each daypart
####################################################

# so what I decided to do was design a function that would print a grid of plots
# one bar plot for each daypart
# the input is the age
# eventually I can change it so that it prints n rows of plots, 1 row for each age
# but for now it's just 1 age at at time
# so what this shows is the most popular movie for each genre within each daypart
# as measured by GRPs
# the movie \ network label is on the x-axis
# the male or female label is above the bar
# i made it so no legend will print, and only 1 title

# what to put in here:
# agg_top, dataframe where top GRP is aggregated by each variable (Daypart, Target, Genre, Age)
# output:
# will return 5 barplots, 1 for each daypart, for the age selected

iterate_over_age <- function(age, df, color_vector) {
  # create 5 dataframes, one for each daypart, for the age the user inputs
  temp_agg <- df[df$Median.Age == age, ]
  for (i in temp_agg$Daypart) {
    if (i == "Morning") {
      morning <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Daytime") {
      daytime <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Early Fringe") {
      early_fringe <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Prime") {
      prime <- temp_agg[temp_agg$Daypart == i,]
    } else if (i == "Overnight") {
      overnight <- temp_agg[temp_agg$Daypart == i, ]
    } 
  } 
  # create 5 barplots
  # and 1 legend
  # must use the dataframe you are subsetting from for the legend
  # or else all the genres might not be represented
  
  # can definitely make a function that makes the ggplots
  # just have to make sure the y labels show up on the first one
  # i was iteratively building these plots and making sure it worked first
  
  gg1 <- ggplot(morning, aes(x = reorder(Program.Network, -GRPs), 
                             y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") +
    xlab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age, "morning")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) + 
    scale_fill_manual(values  = color_vector)
  
  gg2 <- ggplot(daytime, aes(x = reorder(Program.Network, -GRPs), 
                             y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age, "daytime")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) + 
    scale_fill_manual(values  = color_vector)
  
  gg3 <- ggplot(early_fringe, aes(x = reorder(Program.Network, -GRPs), 
                                  y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Program\\Network") + 
    # xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age, "early_fringe")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) + 
    scale_fill_manual(values  = color_vector)
  
  gg4 <- ggplot(prime, aes(x = reorder(Program.Network, -GRPs), 
                           y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age, "prime")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) + 
    scale_fill_manual(values  = color_vector)
  
  gg5 <- ggplot(overnight, aes(x = reorder(Program.Network, -GRPs), 
                               y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age, "overnight")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) + 
    scale_fill_manual(values  = color_vector)
  
  # must call the inputted datframe for this to work
  gg6 <- ggplot(temp_agg, aes(x = reorder(Program.Network, -GRPs), 
                              y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age, "overnight")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) + 
    scale_fill_manual(values  = color_vector) 
  # create legend from previous object
  ggleg <- cowplot::get_legend(gg6)
  
  # this will make them all the same height, 6 in a column
  
  plot_grid(gg1, gg2, gg3, gg4, gg5, ggleg, align = "h", ncol = 6, rel_heights = c(1,1,1,1,1,1),
            axis = "b" )
}

# i can't figure out how to fix the issue that the bars are varying widths
# note that ordering by the plot.order variable will make the bars in order of genre
# i think it's aestheticaly better for insights to do descending GRPs instead

# no fantasy or romance in 18
rsg_pallete_18 <- rsg_pallete[-6]
iterate_over_age(18, agg_top, rsg_pallete_18)
iterate_over_age(36, agg_top, rsg_pallete_fantasy)
iterate_over_age(49, agg_top, rsg_pallete_fantasy)
####################################################################################
# higher level view

# i want to break up the groups into typical advertising segments (looked online for the groups)
attach(agg)
group_18_24 <- agg[Median.Age <= 24.5, ]
group_25_34 <- agg[Median.Age > 24.5 & Median.Age <= 34.5, ]
group_35_44 <- agg[Median.Age > 34.5 & Median.Age <= 44.5, ]
group_45_49 <- agg[Median.Age > 45, ]
detach(agg)
# aggregate these groups using same categories as before
# i could write a function for this, but using attach it will be easy
attach(group_18_24)
agg_top <- group_18_24 %>% group_by(Daypart, Target, GN.Top.Genre, plot.order) %>% slice_max(order_by = GRPs)
agg_top <- as.data.frame(agg_top[order(agg_top$Daypart, agg_top$GN.Top.Genre, agg_top$Target) , ])
agg_18_24 <- agg_top
detach(group_18_24)

attach(group_25_34)
agg_top <- group_25_34 %>% group_by(Daypart, Target, GN.Top.Genre, plot.order) %>% slice_max(order_by = GRPs)
agg_top <- as.data.frame(agg_top[order(agg_top$Daypart, agg_top$GN.Top.Genre, agg_top$Target) , ])
agg_25_34 <- agg_top
detach(group_25_34)

attach(group_35_44)
agg_top <- group_35_44 %>% group_by(Daypart, Target, GN.Top.Genre, plot.order) %>% slice_max(order_by = GRPs)
agg_top <- as.data.frame(agg_top[order(agg_top$Daypart, agg_top$GN.Top.Genre, agg_top$Target) , ])
agg_35_44 <- agg_top
detach(group_35_44)

attach(group_45_49)
agg_top <- group_45_49 %>% group_by(Daypart, Target, GN.Top.Genre, plot.order) %>% slice_max(order_by = GRPs)
agg_top <- as.data.frame(agg_top[order(agg_top$Daypart, agg_top$GN.Top.Genre, agg_top$Target) , ])
agg_45_49 <- agg_top
detach(group_45_49)

# i have to slightly modify the function i wrote earlier for graphing
# age string is just for the titles
# added a vector to pass in colors for scale_fill_manual
iterate_over_group <- function(age_string, df, color_vector) {
  # create 5 dataframes, one for each daypart
  # the temp_agg variable is used a lot so i'm just leaving it for now
  # even though it's not actually needed, df would work
  temp_agg <- df
  for (i in temp_agg$Daypart) {
    if (i == "Morning") {
      morning <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Daytime") {
      daytime <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Early Fringe") {
      early_fringe <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Prime") {
      prime <- temp_agg[temp_agg$Daypart == i,]
    } else if (i == "Overnight") {
      overnight <- temp_agg[temp_agg$Daypart == i, ]
    } 
  } 
  # create 5 barplots
  # and 1 legend
  # must use the dataframe you are subsetting from for the legend
  # or else all the genres might not be represented
  
  # can definitely make a function that makes the ggplots
  # just have to make sure the y labels show up on the first one
  # i was iteratively building these plots and making sure it worked first
  
  gg1 <- ggplot(morning, aes(x = reorder(Program.Network, -GRPs), 
                             y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") +
    xlab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age_string, "morning")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg2 <- ggplot(daytime, aes(x = reorder(Program.Network, -GRPs), 
                             y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age_string, "daytime")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg3 <- ggplot(early_fringe, aes(x = reorder(Program.Network, -GRPs), 
                                  y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Program\\Network") + 
    # xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age_string, "early_fringe")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg4 <- ggplot(prime, aes(x = reorder(Program.Network, -GRPs), 
                           y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age_string, "prime")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg5 <- ggplot(overnight, aes(x = reorder(Program.Network, -GRPs), 
                               y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age_string, "overnight")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs)))  +
    scale_fill_manual(values = color_vector)
  
  # must call the inputted datframe for this to work
  gg6 <- ggplot(temp_agg, aes(x = reorder(Program.Network, -GRPs), 
                              y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(age_string, "overnight")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs)))+
    scale_fill_manual(values = color_vector)
  # create legend from previous object
  ggleg <- cowplot::get_legend(gg6)
  
  # this will make them all the same height, 6 in a column
  
  plot_grid(gg1, gg2, gg3, gg4, gg5, ggleg, align = "h", ncol = 6, rel_heights = c(1,1,1,1,1,1),
            axis = "b" )
}


iterate_over_group("18-24",agg_18_24, rsg_pallete_fantasy)
iterate_over_group("25-34",agg_25_34, rsg_pallete_fantasy)
iterate_over_group("35_44",agg_35_44, rsg_pallete_fantasy)
iterate_over_group("45-49",agg_45_49, rsg_pallete_fantasy)

# let's do one more for the entire group
attach(agg)
agg_top <- agg %>% group_by(Daypart, Target, GN.Top.Genre, plot.order) %>% slice_max(order_by = GRPs)
agg_top <- as.data.frame(agg_top[order(agg_top$Daypart, agg_top$GN.Top.Genre, agg_top$Target) , ])
detach(agg)
str(agg_top)
# the group function should work
# need to remove the current order (it's genre)
agg_top <- agg_top[ , -9]
# the function currently does it in alphabetical order, and I want it in a specific order
agg_top$plot.order <- agg_top$Daypart
# just replace each witha number real quick

agg_top$plot.order[agg_top$Daypart == "Morning"] <- 1
agg_top$plot.order[agg_top$Daypart == "Daytime"] <- 2
agg_top$plot.order[agg_top$Daypart == "Early Fringe"] <- 3
agg_top$plot.order[agg_top$Daypart == "Prime"] <- 4
agg_top$plot.order[agg_top$Daypart == "Overnight"] <- 5
agg_top$plot.order <- as.numeric(agg_top$plot.order)
###########################################


sample(1:10, 1)


# 3d plot the top aggregation, put daypart on an axis
attach(agg_top)
col.vec <- rep("blue", length(Target))
col.vec[Target == "F18-49"] <- "red"
# col.vec
# i changed the color vector so it won't work anymore for this plot
# it is now for the rgl plot
# scatterplot3d(Average.Audience, GRPs, Median.Age, color = col.vec, pch = 16)
# open the window for the rgl plot
open3d() 
# plot 3d interactive object
# each point is a movie \ network pair
# also add 95th percentile planes
# unique(plot.order)
# unique(Daypart)

plot3d(Average.Audience, GRPs, plot.order, col = col.vec,  cex = 1)

bg3d(col = "dark gray")
# plane at 95% percentile of average audience
# quantile(Average.Audience, probs = 0.90)
# which is 694
# points to calculate equation are (694,31.5,1);(694,434.4,1);(694,31.5,5)
planes3d(a = 1611.6	, b = 0, c = 0, d= -1118450, color = "yellow", alpha = 0.4, shininess = 100, emission = "yellow")

# plane at 90th percentile of GRPs
# quantile(GRPs, probs = 0.90)
# which is 231.31
# points to calculate equation are (55,231.31, 1); (1498, 231.31, 1); (55,231.31,5)
planes3d(a = 0, b = -5772, c = 0, d= 1335120, color = "yellow", alpha =0.4, shininess = 100, emission = "yellow")

# right click to display name
selected_plots <- identify3d(Average.Audience, GRPs, plot.order, labels = Program.Network, 
                             plot = TRUE, tolerance = 20, 
                             buttons = c("right", "middle"))
Program.Network[selected_plots]

# save the current view of the plot as a png
out.dir <- "C:\\Users\\SamDe\\Desktop\\"
my.rgl.out <- paste0(out.dir, "movie3dvisualization3.png")
rgl.snapshot(filename = my.rgl.out)
detach(agg_top)

################################################################################
# aggregate by network
# this shows us the most high performing network for each target for each daypart
agg.network <- movie_data_18_49
agg.network$Network <- gsub("^.+[\\]", "", agg.network$Program.Network)
agg.network <- aggregate(cbind(GRPs, Average.Audience) ~ Daypart + Target + Network + Network.Group, data = agg.network, FUN =sum)
agg.network <- agg.network[order(agg.network$Daypart, -agg.network$GRPs), ]
agg.network.top <- agg.network %>% group_by(Daypart, Target) %>% slice_max(order_by = GRPs)
agg.network.top <- as.data.frame(agg.network.top[order(-agg.network.top$GRPs, agg.network.top$Daypart) , ])

################################################################################
# agg by network group
# this show us the most high-performing network group for each target for each daypart
agg.network.group <- movie_data_18_49
agg.network.group <- aggregate(cbind(GRPs, Average.Audience) ~ Daypart + Target + Network.Group, data = agg.network.group, FUN =sum)
agg.network.group <- agg.network.group[order(agg.network.group$Daypart, -agg.network.group$GRPs) , ] 
agg.network.group.top <- agg.network.group %>% group_by(Daypart, Target) %>% slice_max(order_by = GRPs)
agg.network.group.top <- as.data.frame(agg.network.group.top[order(-agg.network.group.top$GRPs, agg.network.group.top$Daypart) , ])

################################################################################
# aggregate by genre, network
# most high-performing network for the top 5 genres for each daypart for each target
agg.genre <- movie_data_18_49
agg.genre$Network <- gsub("^.+[\\]", "", agg.genre$Program.Network)
agg.genre <- aggregate(cbind(GRPs, Average.Audience) ~ Daypart + Target + GN.Top.Genre + Network + Network.Group, data = agg.genre, FUN =sum)
agg.genre <- agg.genre[order(agg.genre$Daypart, -agg.genre$GRPs), ]
agg.genre.top <- agg.genre %>% group_by(Daypart, Target, GN.Top.Genre) %>% slice_max(order_by = GRPs)
agg.genre.top5 <- agg.genre.top %>% arrange(desc(GRPs)) %>% group_by(Daypart, Target) %>% slice(1:5)
agg.genre.top5 <- as.data.frame(agg.genre.top5[order(agg.genre.top5$Daypart, -agg.genre.top5$GRPs) , ])

################################################################################
# top network group for top 5 genres for each daypart for each target
agg.genre.network.group <- movie_data_18_49
agg.genre.network.group <- aggregate(cbind(GRPs, Average.Audience) ~ Daypart + Target + GN.Top.Genre + Network.Group, data = agg.genre.network.group, FUN =sum)
agg.genre.network.group <- agg.genre.network.group[order(agg.genre.network.group$Daypart, -agg.genre.network.group$GRPs), ]
agg.genre.network.group.top <- agg.genre.network.group %>% group_by(Daypart, Target, GN.Top.Genre) %>% slice_max(order_by = GRPs)
agg.genre.network.group.top5 <- agg.genre.network.group.top %>% arrange(desc(GRPs)) %>% group_by(Daypart, Target) %>% slice(1:5)
agg.genre.network.group.top5 <- as.data.frame(agg.genre.network.group.top5[order(agg.genre.network.group.top5$Daypart, -agg.genre.network.group.top5$GRPs) , ])

#################################################################################
# rewrite plotting function so it plots Network instead of Program.Network
iterate_over_network <- function(title_string, df, color_vector) {
  # create 5 dataframes, one for each daypart
  # the temp_agg variable is used a lot so i'm just leaving it for now
  # even though it's not actually needed, df would work
  temp_agg <- df
  for (i in temp_agg$Daypart) {
    if (i == "Morning") {
      morning <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Daytime") {
      daytime <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Early Fringe") {
      early_fringe <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Prime") {
      prime <- temp_agg[temp_agg$Daypart == i,]
    } else if (i == "Overnight") {
      overnight <- temp_agg[temp_agg$Daypart == i, ]
    } 
  } 
  # create 5 barplots
  # and 1 legend
  # must use the dataframe you are subsetting from for the legend
  # or else all the genres might not be represented
  
  # can definitely make a function that makes the ggplots
  # just have to make sure the y labels show up on the first one
  # i was iteratively building these plots and making sure it worked first
  
  gg1 <- ggplot(morning, aes(x = reorder(Network, -GRPs), 
                             y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") +
    xlab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "morning")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg2 <- ggplot(daytime, aes(x = reorder(Network, -GRPs), 
                             y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "daytime")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg3 <- ggplot(early_fringe, aes(x = reorder(Network, -GRPs), 
                                  y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Network") + 
    # xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "early fringe")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg4 <- ggplot(prime, aes(x = reorder(Network, -GRPs), 
                           y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "prime")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg5 <- ggplot(overnight, aes(x = reorder(Network, -GRPs), 
                               y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "overnight")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  # must call the inputted datframe for this to work
  gg6 <- ggplot(temp_agg, aes(x = reorder(Network, -GRPs), 
                              y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "overnight")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector) 
  # create legend from previous object
  ggleg <- cowplot::get_legend(gg6)
  
  # this will make them all the same height, 6 in a column
  
  plot_grid(gg1, gg2, gg3, gg4, gg5, ggleg, align = "h", ncol = 6, rel_heights = c(1,1,1,1,1,1),
            axis = "b" )
}


# same for the network group, all I have to do is change Network variable to Network.Group
# adding argument for color vector for scale_fill_manual
iterate_over_network.group <- function(title_string, df, color_vector) {
  # create 5 dataframes, one for each daypart
  # the temp_agg variable is used a lot so i'm just leaving it for now
  # even though it's not actually needed, df would work
  temp_agg <- df
  for (i in temp_agg$Daypart) {
    if (i == "Morning") {
      morning <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Daytime") {
      daytime <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Early Fringe") {
      early_fringe <- temp_agg[temp_agg$Daypart == i, ]
    } else if (i == "Prime") {
      prime <- temp_agg[temp_agg$Daypart == i,]
    } else if (i == "Overnight") {
      overnight <- temp_agg[temp_agg$Daypart == i, ]
    } 
  } 
  # create 5 barplots
  # and 1 legend
  # must use the dataframe you are subsetting from for the legend
  # or else all the genres might not be represented
  
  # can definitely make a function that makes the ggplots
  # just have to make sure the y labels show up on the first one
  # i was iteratively building these plots and making sure it worked first
  
  gg1 <- ggplot(morning, aes(x = reorder(Network.Group, -GRPs), 
                             y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") +
    xlab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "morning")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg2 <- ggplot(daytime, aes(x = reorder(Network.Group, -GRPs), 
                             y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "daytime")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg3 <- ggplot(early_fringe, aes(x = reorder(Network.Group, -GRPs), 
                                  y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Network") + 
    # xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "early_fringe")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg4 <- ggplot(prime, aes(x = reorder(Network.Group, -GRPs), 
                           y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "prime")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  gg5 <- ggplot(overnight, aes(x = reorder(Network.Group, -GRPs), 
                               y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "overnight")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector)
  
  # must call the inputted datframe for this to work
  gg6 <- ggplot(temp_agg, aes(x = reorder(Network.Group, -GRPs), 
                              y = GRPs, fill = GN.Top.Genre)) +
    geom_bar(position = "dodge", stat = "identity") +
    # xlab("Program\\Network") + 
    xlab("") +
    ylab("") +
    geom_text(aes(label = gsub("18-49", "", Target)), vjust = -0.5) +
    ggtitle (paste(title_string, "overnight")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    coord_cartesian(ylim = c(0, 1.2*max(temp_agg$GRPs))) +
    scale_fill_manual(values = color_vector) 
  # create legend from previous object
  ggleg <- cowplot::get_legend(gg6)
  
  # this will make them all the same height, 6 in a column
  
  plot_grid(gg1, gg2, gg3, gg4, gg5, ggleg, align = "h", ncol = 6, rel_heights = c(1,1,1,1,1,1),
            axis = "b" )
}


##################################################################################
# visualize the aggregations I created earlier
# this shows us the most high performing network for each target for each daypart

agg.network.top$plot.order <- agg.network.top$Daypart
# just replace each witha number real quick

agg.network.top$plot.order[agg.network.top$Daypart == "Morning"] <- 1
agg.network.top$plot.order[agg.network.top$Daypart == "Daytime"] <- 2
agg.network.top$plot.order[agg.network.top$Daypart == "Early Fringe"] <- 3
agg.network.top$plot.order[agg.network.top$Daypart == "Prime"] <- 4
agg.network.top$plot.order[agg.network.top$Daypart == "Overnight"] <- 5
agg.network.top$plot.order <- as.numeric(agg.network.top$plot.order)
ggplot(agg.network.top, aes(x = reorder(Daypart, plot.order), y = GRPs, col = Network, cex = Average.Audience, pch = Target)) +
  geom_point() +
  geom_text(aes(label = Network, vjust = -1, hjust = 1), cex = 3, color = "black") +
  ggtitle("Networks with Highest GRPs for Each Target within Each Daypart")
# scale_color_manual(c(, , , ))
# rsg_pallete
# i think this would look better as bar graph
ggplot(agg.network.top, aes(x = reorder(Daypart, -plot.order), y = GRPs, fill = Network)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = gsub("18-49", "", Target)), color = "black") +
  ggtitle("Networks with Highest GRPs for M18-49 and FM18-49 within Each Daypart") +
  coord_flip() +
  scale_fill_manual(values = c("#1F77B4", "#402B51", "#9467BD", "#AEC7E8", "#F2620F", "#F2620F" ))

# this show us the most high-performing network group for each target for each daypart

agg.network.group.top$plot.order <- agg.network.group.top$Daypart
agg.network.group.top$plot.order[agg.network.group.top$Daypart == "Morning"] <- 1
agg.network.group.top$plot.order[agg.network.group.top$Daypart == "Daytime"] <- 2
agg.network.group.top$plot.order[agg.network.group.top$Daypart == "Early Fringe"] <- 3
agg.network.group.top$plot.order[agg.network.group.top$Daypart == "Prime"] <- 4
agg.network.group.top$plot.order[agg.network.group.top$Daypart == "Overnight"] <- 5
agg.network.group.top$plot.order <- as.numeric(agg.network.group.top$plot.order)

ggplot(agg.network.group.top, aes(x = reorder(Daypart, -plot.order), y = GRPs, fill = Network.Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = gsub("18-49", "", Target)), color = "black") +
  ggtitle("Network Groups with Highest GRPs for M18-49 and FM18-49 within Each Daypart") +
  coord_flip() +
  scale_fill_manual(values = c("#1F77B4", "#AEC7E8", "#F2620F", "#2CA02C"))

# most high-performing network for the top 5 genres for each daypart for each target
# using same colors as RSG does for easy comparison
col.vec.n <- rsg_pallete[-7]
iterate_over_network("", agg.genre.top5, col.vec.n)
# most high-performing network group for each genre for each daypart for each target
# using same colors as rsg
col.vec.ng <- rsg_pallete[-6]
iterate_over_network.group("", agg.genre.network.group.top5, col.vec.ng)

###########################################################
# final thoughts

# there is so much more analysis i could do
# there are so many variables in this dataset
# i could definitely make some machine learning models for predicting ROI once I learn what that is
# maybe SVM could be used to classify movies here
# would have to have a binary classifer for "successfull"
# another thing is should do is just look at the title and not the channel
# could also look at network

# income would be good to look at
# and this is only linear data, so being able to see all the touchpoints would be cool
# could write SQL queries to retrieve the information I graphed

# why is it helpful to find the most popular movies?
# these are outliers, and the movies that are popular within a specific narrow population
# like 18 year-olds
# may not be readily apparent

# knowing this data, combined with when that population was most likely to watch them,
# along with when they are most likely to watch movies from that genre or watch movies in general,
# helps optimize scheduling
# this also opens up the opportunity to recommend other networks pick up movies their competitors have success with
# and also helps plan cross-promotion
# star wars with universal studios, disney with harry potter, etc