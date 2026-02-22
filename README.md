# R4Epidemiology-Scripts
##Measures of Dispersion
library(dplyr)

# Simulate some data
height_and_weight_20 <- tribble(
  ~id,   ~sex,     ~ht_in, ~wt_lbs,
  "001", "Male",   71,     190,
  "002", "Male",   69,     177,
  "003", "Female", 64,     130,
  "004", "Female", 65,     153,
  "005", NA,       73,     173,
  "006", "Male",   69,     182,
  "007", "Female", 68,     186,
  "008", NA,       73,     185,
  "009", "Female", 71,     157,
  "010", "Male",   66,     155,
  "011", "Male",   71,     213,
  "012", "Female", 69,     151,
  "013", "Female", 66,     147,
  "014", "Female", 68,     196,
  "015", "Male",   75,     212,
  "016", "Female", 69,     19000,
  "017", "Female", 66,     194,
  "018", "Female", 65,     176,
  "019", "Female", 65,     176,
  "020", "Female", 65,     102
)

# Recreate our mode function
mode_val <- function(x) {
  value_counts <- table(x)
  result <- names(value_counts)[value_counts == max(value_counts)]
  if (length(value_counts) == length(result)) {
    result <- NA
  }
  result
}

height_and_weight_20 %>% 
  summarise(
    min_height    = min(ht_in),
    mean_height   = mean(ht_in),
    median_height = median(ht_in),
    mode_height   = mode_val(ht_in) %>% paste(collapse = " , "),
    max_height    = max(ht_in)
  )

#Measures of Dispersion are 3. Range Variance and Standard Deviation.

##Range. Difference between Maximum and Minimum Value in the data.
height_and_weight_20 %>% 
  summarise(
    min_height  = min(ht_in),
    mean_height = mean(ht_in),
    max_height  = max(ht_in),
    range       = max_height - min_height
  )


##Variance. Difference between each persons height and the mean height.
# We use R's var() function
var(c(rep(58, 3), rep(78, 3)))


##Standard Deviation.Simply the square root of the Variance.


##Comparing Distributions.
sim_data <- tibble(
  all_68     = rep(68, 20),
  half_58_78 = c(rep(58, 10), rep(78, 10)),
  even_58_78 = seq(from = 58, to = 78, length.out = 20),
  half_48_88 = c(rep(48, 10), rep(88, 10)),
  even_48_88 = seq(from = 48, to = 88, length.out = 20)
)
sim_data

tibble(
  Column   = names(sim_data),
  Mean     = purrr::map_dbl(sim_data, mean), # The map_dbl() function from the purrr package is used to iterate over each column in the data.
  Variance = purrr::map_dbl(sim_data, var),
  SD       = purrr::map_dbl(sim_data, sd)
)


##Describing the Relationship Between a Continuous Outcome and a Continuous Predictor.
#Outcome Variable. The variable whose value we are attempting to predict, estimate, or determine is the outcome variable
# Predictor Variable. The variable that we think will determine, or at least help us predict, the value of the outcome variable is called the predictor variable.
# Pearson's Correlation Coefficient. Pearson’s Correlation Coefficient is a parametric measure of the linear relationship between two numerical variables

# Load the dplyr package
library(dplyr)
# Load the ggplot2 package
library(ggplot2)

set.seed(123) #set.seed() function is to ensure that I get the same random numbers every time I run the code chunk.
df <- tibble(
  id = 1:20,
  x  = sample(x = 0:100, size = 20, replace = TRUE),
  y  = sample(x = 0:100, size = 20, replace = TRUE) #replace = TRUE option tells R that the same number can be selected more than once.
)
df

#We used the sample() function to create x and y by sampling a number between 0 and 100 at random, 20 times.

ggplot(df, aes(x, y)) +
  geom_point() +
  theme_bw()
# We created a nicer scatter plot with this. each dot corresponds to a person in our data at the point where their x value intersects with their y value.

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_text(aes(label = id), nudge_x = 1.5, nudge_y = 2) + #nudge_x = 1.5 option moves our text (the id number) to the right 1.5 units. The nudge_y = 2 option moves our text 2 units up.
  theme_bw()
#The geom_text() layer to our plot in order to make it clear which person each dot represents..

ggplot(df, aes(x, y)) +
  geom_text(aes(label = id), nudge_x = 1.5, nudge_y = 2) +
  geom_vline(xintercept = 30, col = "red", size = 0.25) +
  geom_hline(yintercept = 71, col = "red", size = 0.25) +
  geom_point() +
  theme_bw()
# Here, the dot representing id 1 is at the intersection of these two lines.

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_text(aes(label = id), nudge_x = 1.5, nudge_y = 2) +
  geom_point(aes(x, y), tibble(x = 100, y = 80), shape = 1, size = 16, col = "red") +
  geom_point(aes(x, y), tibble(x = 90, y = 8), shape = 1, size = 16, col = "blue") +
  theme_bw()

#Pearson's Correlation Coefficient quantifies our results in some way.
cor.test(x = df$x, y = df$y)

#Correlation Intuition.
# Positively correlated data
tibble(
  x = 1:10,
  y = 100:109,
  r = cor(x, y)
) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_text(aes(x = 2.5, y = 107.5, label = paste("r = ", r)), col = "blue") +
  theme_classic()
#This is positively correlated data. Increase in x simultaneously increases y.

df <- tibble(
  x = 1:-8,
  y = 100:91
)
df

df %>% 
  mutate(r = cor(x, y)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_text(aes(x = -6, y = 98, label = paste("r = ", r)), col = "blue") +
  theme_classic()

#In his final exmple, as x increases by 1, y decreases by 1.
tibble(
  x = 1:10,
  y = 100:91,
  r = cor(x, y)
) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_text(aes(x = 7.5, y = 98, label = paste("r = ", r)), col = "blue") +
  theme_classic()


class <- tibble(
  ht_in = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65, 
            64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62, 
            64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66, 
            69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62, 
            61, 69, 66, NA),
  wt_lbs = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146, 
             125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124, 
             186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136, 
             147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212, 
             110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102, 
             163, 141, NA)
)

#Let's use a scatter plot to explore the relationship between height and weight.
ggplot(class, aes(ht_in, wt_lbs)) +
  geom_jitter() +
  theme_classic()
#IN the scatter plot the dots don't align in a perfect slope.

cor.test(class$ht_in, class$wt_lbs)

#How to disable the scientific notation for th p-value.
options(scipen = 999) #We used options(scipen = 999) to display decimal numbers instead of scientific notation.
cor.test(class$ht_in, class$wt_lbs)

#Finally, let us draw line of best fit. This line is known as an Ordinary Least Squares (OLS) regression line.
#All we need to do is add a geom_smooth() layer to our scatter plot with the method argument set to lm
ggplot(class, aes(ht_in, wt_lbs)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  theme_classic()


##Describing the Relationship Between a Continuous Outcome and a Categorical Predictor.
class <- tibble(
  age       = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25, 
                25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23, 
                24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24, 
                27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34, 
                26, 25, 27, NA),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 
                2, 1, 1, 1, NA),
  gender    = c(2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 
                1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 
                1, 1, 2, 1, NA),
  ht_in     = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65, 
                64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62, 
                64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66, 
                69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62, 
                61, 69, 66, NA),
  wt_lbs    = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146, 
                125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124, 
                186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136, 
                147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212, 
                110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102, 
                163, 141, NA),
  bmi       = c(30.99, 18.78, 26.52, 30.54, 22.39, 26.12, 23.69, 20.67, 26.29, 
                25.39, 25.68, 45.15, 21.56, 20.17, 17.38, 20.8, 22.31, 22.75, 
                26.62, 21.43, 19.14, 23.49, 22.66, 32.98, 25.05, 18.31, 29.13, 
                27.07, 20.37, 25.01, 19.69, 25.97, 18.88, 20.07, NA, 26.76, 
                26.97, 25.24, 20.68, 23.72, 24.82, 23.62, 18.65, 24.03, 23.86, 
                10.63, 23.02, 23.72, 20.82, 28.24, NA, 37.55, 18.88, 18.3, 
                19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97, 
                22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat  = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 
                1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1, 
                1, 1, 1, 1, 1, NA)
) %>% 
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender = factor(gender, labels = c("Female", "Male")),
    bmi_3cat = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese"))
  ) %>% 
  print()

#This time we will use dplyr's group_by() function to calculate these statistics within subgroups of interests.
class_summary <- class %>% 
  filter(!is.na(ht_in)) %>% 
  group_by(gender) %>% 
  summarise(
    n                    = n(),
    mean                 = mean(ht_in),
    `standard deviation` = sd(ht_in),
    min                  = min(ht_in),
    max                  = max(ht_in)
  ) %>% 
  print()

#Let us plot the data to get a feel for the relationship between gender and height.
class %>% 
  filter(!is.na(ht_in)) %>% 
  ggplot(aes(x = gender, y = ht_in)) +
  geom_jitter(aes(col = gender), width = 0.20) + #geom_jitter() function plots a point for each student’s height, and then makes slight random adjustments to the location of the points so that they are less likely to overlap
  geom_segment(  #geom_segment() function creates the two horizontal lines at the mean values of height
    aes(x = c(0.75, 1.75), y = mean, xend = c(1.25, 2.25), yend = mean, col = gender), 
    size = 1.5, data = class_summary
  ) +
  scale_x_discrete("Gender") + #changed the x and y axis titles using the scale_x_discrete() and scale_y_continuous() functions.
  scale_y_continuous("Height (Inches)") +
  scale_color_manual(values = c("#BC581A", "#00519B")) + #changed the default ggplot colors to orange and blue using the scale_color_manual() function.
  theme_classic() + #simplified the plot using the theme_classic() function.
  theme(legend.position = "none", axis.text.x = element_text(size = 12)) #theme(legend.position = "none", axis.text.x = element_text(size = 12)) removed the legend and increased the size of the x-axis labels a little bit.


#Multiple Predictors
class_summary <- class %>% 
  filter(!is.na(bmi)) %>% 
  group_by(gender, age_group) %>% 
  summarise(
    n                    = n(),
    mean                 = mean(bmi),
    `standard deviation` = sd(bmi),
    min                  = min(bmi),
    max                  = max(bmi)
  ) %>% 
  print()

class %>% 
  filter(!is.na(bmi)) %>% 
  ggplot(aes(x = age_group, y = bmi)) +
  facet_wrap(vars(gender)) + #The only difference is that we added facet_wrap(vars(gender)) to plot males and females on separate plot panels.
  geom_jitter(aes(col = age_group), width = 0.20) +
  geom_segment(
    aes(x = rep(c(0.75, 1.75), 2), y = mean, xend = rep(c(1.25, 2.25), 2), yend = mean, 
        col = age_group),
    size = 1.5, data = class_summary
  ) +
  scale_x_discrete("Age Group") +
  scale_y_continuous("BMI") +
  scale_color_manual(values = c("#BC581A", "#00519B")) +
  theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(size = 10))


##Describing the Relationship Between a Categorical Outcome and a Categorical Predictor.
class <- tibble(
  age       = c(32, 30, 32, 29, 24, 38, 25, 24, 48, 29, 22, 29, 24, 28, 24, 25, 
                25, 22, 25, 24, 25, 24, 23, 24, 31, 24, 29, 24, 22, 23, 26, 23, 
                24, 25, 24, 33, 27, 25, 26, 26, 26, 26, 26, 27, 24, 43, 25, 24, 
                27, 28, 29, 24, 26, 28, 25, 24, 26, 24, 26, 31, 24, 26, 31, 34, 
                26, 25, 27, NA),
  age_group = c(2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 
                1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 
                2, 1, 1, 1, NA),
  gender    = c(2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 
                1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 
                1, 1, 2, 1, NA),
  ht_in     = c(70, 63, 62, 67, 67, 58, 64, 69, 65, 68, 63, 68, 69, 66, 67, 65, 
                64, 75, 67, 63, 60, 67, 64, 73, 62, 69, 67, 62, 68, 66, 66, 62, 
                64, 68, NA, 68, 70, 68, 68, 66, 71, 61, 62, 64, 64, 63, 67, 66, 
                69, 76, NA, 63, 64, 65, 65, 71, 66, 65, 65, 71, 64, 71, 60, 62, 
                61, 69, 66, NA),
  wt_lbs    = c(216, 106, 145, 195, 143, 125, 138, 140, 158, 167, 145, 297, 146, 
                125, 111, 125, 130, 182, 170, 121, 98, 150, 132, 250, 137, 124, 
                186, 148, 134, 155, 122, 142, 110, 132, 188, 176, 188, 166, 136, 
                147, 178, 125, 102, 140, 139, 60, 147, 147, 141, 232, 186, 212, 
                110, 110, 115, 154, 140, 150, 130, NA, 171, 156, 92, 122, 102, 
                163, 141, NA),
  bmi       = c(30.99, 18.78, 26.52, 30.54, 22.39, 26.12, 23.69, 20.67, 26.29, 
                25.39, 25.68, 45.15, 21.56, 20.17, 17.38, 20.8, 22.31, 22.75, 
                26.62, 21.43, 19.14, 23.49, 22.66, 32.98, 25.05, 18.31, 29.13, 
                27.07, 20.37, 25.01, 19.69, 25.97, 18.88, 20.07, NA, 26.76, 
                26.97, 25.24, 20.68, 23.72, 24.82, 23.62, 18.65, 24.03, 23.86, 
                10.63, 23.02, 23.72, 20.82, 28.24, NA, 37.55, 18.88, 18.3, 
                19.13, 21.48, 22.59, 24.96, 21.63, NA, 29.35, 21.76, 17.97, 
                22.31, 19.27, 24.07, 22.76, NA),
  bmi_3cat  = c(3, 1, 2, 3, 1, 2, 1, 1, 2, 2, 2, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 
                1, 1, 3, 2, 1, 2, 2, 1, 2, 1, 2, 1, 1, NA, 2, 2, 2, 1, 1, 1, 1, 
                1, 1, 1, 1, 1, 1, 1, 2, NA, 3, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1, 
                1, 1, 1, 1, 1, NA),
  genhlth   = c(2, 2, 3, 3, 2, 1, 2, 2, 2, 1, 3, 3, 1, 2, 2, 1, 2, NA, 3, 2, 3, 
                1, 2, 2, 2, 4, 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 3, 3, 2, 1, 3, 3, 
                2, 2, 3, 3, 2, 3, 2, 2, 3, 5, 3, 2, 3, 2, 3, 3, 2, 2, 3, 3, 3, 
                1, 2, 2, 1, 3),
  persdoc   = c(1, 2, 2, 1, 2, 0, 0, 1, 2, 0, 2, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 
                0, 1, 1, 1, 1, 2, 0, 0, 1, 1, 2, 1, 2, 0, 0, 2, 0, 0, 2, 2, 0, 
                NA, 0, 0, 0, 2, 0, 2, NA, 0, 2, 1, 1, 1, 2, 2, 0, 0, 0, 1, 2, 
                1, 1, 0, 0, 0, NA)
) %>% 
  mutate(
    age_group = factor(age_group, labels = c("Younger than 30", "30 and Older")),
    gender    = factor(gender, labels = c("Female", "Male")),
    bmi_3cat  = factor(bmi_3cat, labels = c("Normal", "Overweight", "Obese")),
    genhlth   = factor(genhlth, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    persdoc    = factor(persdoc, labels = c("No", "Yes, only one", "Yes, more than one"))
  ) %>% 
  print()

#Comparing 2 Variables.
df <- filter(class, !is.na(bmi_3cat)) # Drop rows with missing bmi
gmodels::CrossTable(df$persdoc, df$genhlth)
