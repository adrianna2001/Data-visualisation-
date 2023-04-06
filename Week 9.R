# Week 9
# Adrianna Podolak
# 20.03.2023


# 1. The grammar of graphics -------------------------------------------------------------

# ggplot2 follows the principle of the “The Grammar of Graphics” by Wilkinson, Anand, and Grossman (2005). 
# In a nutshell, a graphic such as a scatter plot gets dissected into its components,
# so that we can manipulate each component separately:

# The geometric objects that display our data are called geoms, e.g. geom_point. 
# Each geom has features, which are called aesthetics in ggplot. 
# These features are their position along the x- and y-axis, their shape, size and colour.

# Now the important bit: To visualise our data we map variables of our data to those aesthetics.
# And this is why we want our data tidy, i.e. one variable per column, 
# so that we can easily say which variable we want displayed through which aesthetic.

# In addition, we can determine the coordinate system (in most cases Cartesian, i.e. x- and y-axes,
# but there are others, too), scales (on axes, but also for example when we use colour), and plot annotations.

# Let’s start by looking at some examples. For this we’ll use a puppy-equivalent of a dataset
# called Palmer Penguins.

install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)

# Have a look at the penguins dataframe with your favourite looking-at-datasets function.

data("penguins")
head(penguins)

# The dataframe has a number of different measurements for three penguin species 
# across three different islands.

# Let’s say we want to find out whether there is a correlation between body mass and bill length. 
# A simple scatterplot is a good way to look at correlations between two continuous variables.

# To do this, we need to tell ggplot which data we’re looking at, which geom we’d like to use,
# and which variables we’d like to map to which aesthetics.

# Have a look at the code below and identify the bits that are high-lighted in bold.

ggplot(data = penguins) + # data is penguins 
  geom_point(mapping = aes(x = bill_length_mm, y = body_mass_g)) # geoms are points, aesthetics are x and y axes,
# map using mapping (), variables are bill length and body mass

# So there seems to be a correlation, but there is also what looks like a cluster that is shifted
# towards the bottom right of the graph. Could those be species differences? 
# Let’s check by mapping species to the aesthetic colour of geom_point:

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = body_mass_g, colour = species))

# Does this cluster also correlate with the island the penguins are from? 
# Copy and change the code above to check.

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = body_mass_g, colour = species, shape = island))

# We can add additional layers to our plot by specifying additional geoms.

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_smooth(mapping = aes(x = bill_length_mm, y = body_mass_g))

# There are several things we can do to improve this code and plot. 
# Firstly, we don’t have to repeat the mapping of variables if we use the same ones in different layers. 
# We can pass them to ggplot() which means that they will be inherited by the geoms that follow:

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth()

# Secondly, we already know that our data fall into three species clusters,
# so fitting the curve across all three is probably not a great idea.
# Let’s map species again to colour.

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(colour = species))+
  geom_smooth()
  
# Huh, the curve is still going across all three species. 
# That’s because mappings are only inherited from ggplot(), not between geoms.

# Copy and fix the code above, so that each species has its own fitted curve.

ggplot_species_curve <- ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(colour = species))+
  geom_smooth(mapping = aes(colour = species))


# Once we’re happy with our plot we can assign it to a variable and add other layers later. 
# This way we can save a basic plot and try out different layers or other modifications.

pengu_plot <- ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(aes(colour = species))

pengu_plot + geom_smooth()

# Write code to produce the following plot. 
# Hint: Look at the documentation for geom_smooth to find the arguments you need 
# for a linear model and to remove the confidence intervals.

?geom_smooth

pengu_plot <- ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(mapping = aes(colour = species, shape = island)) +
  geom_smooth(mapping = aes(colour = species), method = lm, se = FALSE )


# 2. Saving plots  --------------------------------------------------------

# We can save our plots to a file using ‘ggsave’. We can either give ggsave a plot variable:

ggsave(plot = pengu_plot, file = "penguin_plot_1.png")

# Or if we don’t pass it a variable it will save the last plot we printed to screen

pengu_plot +
  geom_smooth()

ggsave(plot = pengu_plot, file= "penguin_plot_2.png")

# ggsave gives you a lot of flexibility in how you save your files; 
# there’s too many to go into in detail here, but you can start by changing the dimensions of your plot.

# Look at the documentation for ggsave; 
# save your latest plot with the linear model lines as a 200mm x 300mm png.

?ggsave

ggsave(plot = pengu_plot, filename = 'penguin_plot_3.png', width = 300, height = 200, units = 'mm')


# 3. Continuous vs categorical variables ----------------------------------

# So far we have only mapped continuous variables such as length measurements to our x- and y-positions. 
# We have mapped categorical variables, such as species or island, to shape or colour of our geoms.
# Which geom we need to use depends on the type of variable we’d like to map to the x- and y-coordinates. 
# For example, if we’d like to investigate body_mass for each species, we can use box plots.

ggplot(data = penguins,
       mapping = aes(x = species, y = body_mass_g)) +
  geom_boxplot(mapping = aes(colour = species))

# Notice how mapping species to colour in geom_boxplot() only changes the colour of the lines.

#Change the code, so that it fills the boxes with colour instead of the lines.
# You might have to google how to do that - it’s not obvious from the documentation.

?geom_boxplot
ggplot(data = penguins,
       mapping = aes(x = species, y = body_mass_g)) +
  geom_boxplot(mapping = aes(fill = species)) # fill fills in with colour, colour () colours the lines 


#We often want to determine the order in which we display our data.
# This is where factors come in handy.
#Categorical variables that have a defined and known set of values, for example the three species in penguins,
# can be defined as factors. 
# Factors have levels which are essentially rank positions for each unique value. 
# By default, levels are in alphanumerical order, that’s why the three species appear
# in alphabetical order in the above plot.

# Look at penguins using both head() and str(). Where can you see which variables are factors?
# What additional information does str() show you?

head(penguins) # fators are species, island, sex
str(penguins) # shows additionally how many levels each factor has 
  
# Here is an example where alphabetical order would be annoying:

df_days <-
  data.frame(day = c("Mon", "Tues", "Wed", "Thu"),
             counts = c(3, 8, 10, 5))
df_days$day <- as.factor(df_days$day)
str(df_days)

ggplot(data = df_days, mapping = aes(x = day, y = counts)) +
  geom_col()

# Luckily we can change that very easily:

df_days$day <- factor(df_days$day, levels = c("Mon", "Tues", "Wed", "Thu"))
str(df_days)

ggplot(data = df_days, mapping = aes(x = day, y = counts)) +
  geom_col()

#Let’s put together a few of the things you’ve learnt so far:

# Write the code to reproduce this plot. 
# You’ll have to use the data visualisation cheat sheet to find the correct geom.

penguins$species <- factor(penguins$species, levels = c('Chinstrap', 'Gentoo', 'Adelie'))
str(penguins)

ggplot(data = penguins, mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(mapping = aes(fill = island))


# 4. Statistical transformations ------------------------------------------

# A lot of geoms do statistical transformations, i.e. calculations of counts, means, etc., by default.
# You’ve actually already seen that in the boxplot graphs! 
# Our dataframe does not contain the median, 25th percentile, 75th percentile, 
# etc. for body_mass_g, but the geom calculates that. Here is another example:

ggplot(data = penguins) +
  geom_bar(mapping = aes(x = species)) +
  coord_flip()

# Our dataframe does not contain the counts of penguins for each species. geom_bar() calculates those.

# Have a look at the documentation for geom_bar.
# What is the difference between geom_bar() and geom_col()? Also, what does coord_flip() do?

# geom_bar - makes the height of the bar proportional to the number of cases in each group
# geom_col - the heights of the bars to represent values in the data,
# coord_flip - Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal.

# have a close look at the plots below. What is the difference? 
# Can you find the geom to reproduce these plots and the geom argument to switch between the two? 
# Bonus credit: The bars are slightly transparent. Can you find the argument to change transparency?

?geom_col()
penguins$species <- factor(penguins$species, levels = c('Adelie', 'Chinstrap', 'Gentoo'))
str(penguins)

ggplot(data = penguins) + geom_histogram(mapping = aes(x = flipper_length_mm, fill = species), 
                                         position = 'identity',
                                         alpha = 0.5)

?geom_histogram



# 5. Plotting only a subset of your data: filter() ------------------------


# We often want to plot a subset of our data. 
# For example, you may want to look at penguins of two of the species only.
# This is where the dplyr function filter() comes in. 
# It does what it says on the tin: It filters (or subsets) rows based on a logical evaluation you give it.

penguins %>% filter(!species == "Chinstrap") %>%
  ggplot(mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(colour = species, shape = island))

# Notice in the code above how we switch from the pipe operator %>% to a + sign for adding layers to ggplot(). 
# Mixing those up is quite a common error. We also don’t need to tell ggplot()
# which data to use because we have piped the dataset into ggplot() already.

# filter() is extremely useful together with the function is.na() to get rid of pesky NAs.

# Use is.na(sex) with filter() to reproduce the plot below, so that it only contains penguins where sex is known.

penguins %>% filter(!is.na(sex)) %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(mapping = aes(fill = sex))

# Another very useful function is arrange() which allows you to sort rows by values in one or more columns.
# You will need this for the last exercise of the workshop.
# It’s easy to use, so we’ll leave that to you to figure out when you need it.


# 6. Labels  --------------------------------------------------------------

# So far we have only manipulated the geometric objects and the axes.
# Here we’ll make a start on making our plot prettier by editing the labels (much more on jazzing up plots next week).
# The function to manipulate or add labels is labs().

penguins %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex)) +
  labs(title = "Weight distribution among penguins",
       subtitle = "Plot generated by E. Busch-Nentwich, March 2023",
       x = "Species",
       y = "Weight in g",
       caption = "Data from Palmer Penguins package\nhttps://allisonhorst.github.io/palmerpenguins/"
  )


# Changing the legend title and labels can’t be done within labs(), 
# because the legend is part of scales. Which function we need to use depends on the aesthetics and variables. 
# Here we have mapped a categorical variable (sex) to fill, so the function to use is scale_fill_discrete(). This function also allows you to change the colours, but we’ll talk about colours a lot more over the next couple of weeks.

penguins %>%
  ggplot(mapping = aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex)) +
  labs(title = "Weight distribution among penguins",
       subtitle = "Plot generated by E. Busch-Nentwich, March 2023",
       x = "Species",
       y = "Weight in g",
       caption = "Data from Palmer Penguins package\nhttps://allisonhorst.github.io/palmerpenguins/"
  ) +
  scale_fill_discrete(name = "Sex",
                      labels = c("Female", "Male", "Unknown"),
                      type = c("yellow3", "magenta4", "grey"))

# Generate a new plot from the penguin data with at least two geoms, good labels, and maybe even try out some colours. # For example, you could try and find a geom that allows you to show the individual datapoints on top of boxplots. Go # wild!


# 7. Big challenge --------------------------------------------------------

# Read in the modelling table (“wmr_modelling.txt”) and reproduce the following plot. You’ll have to figure out a way # to order the dataframe by deaths and then convince ggplot to keep the data in that order when plotting (hint: factors # are your friends!)

# read in data 
malaria_data <- read.table('wmr_modelling.txt', header = TRUE, sep='\t')



# filter for year 2020 and arrange according to deaths 
malaria_2020 <- malaria_data %>% filter(year == 2020) %>% arrange(deaths)

#Make countries factors in the order of deaths
deathorder_20 <- malaria_2020$country
malaria_2020$country <- factor(malaria_2020$country, levels = deathorder_20)

#Plot data
ggplot(malaria_2020, aes(x = country, y = deaths)) +
  geom_col() +
  coord_flip() +
  labs(title = "Malaria Deaths in 2020")


