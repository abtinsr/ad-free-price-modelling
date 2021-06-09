######################################################
##### PACKAGES
######################################################
# Manage dependencies
library(renv)

# Data fetching:
library(httr)
library(jsonlite)
library(RCurl)

#Data organizing:
library(magrittr)
library(stringr)
library(tidyr)
library(dplyr)
library(data.table)
library(reshape2) # Turn columns into rows
library(lubridate) # Turn rubbish dates into proper ones

#Palettes and visualization:
library(ggplot2)
library(ggrepel)
library(viridis)
library(RColorBrewer)
library(wesanderson)
library(ggthemes)
library(directlabels) # Add end labels to lines
library(forcats) # Re-arrange label levels in a grap
library(scales)
library(ggpubr) # Multiple plots in one view

######################################################
##### GET THE CSV
######################################################

# The data was exceedingly complicated to extract from our databases. 
# As such, a couple of queries were written to get data with different 
# attributes and from different databases.

df_subs <-
  read.csv(file = "../ad-free-price-modelling/1_Data/bq_qv_subs_mockup.csv",
           sep = ",",
           na.strings=c("","n/a"))

df_subs_excl_top100 <-
  read.csv(file = "../ad-free-price-modelling/1_Data/bq_qv_subs_excl_top100_mockup.csv",
           sep = ",",
           na.strings=c("","n/a"))

df_activity <- 
  read.csv(file = "../ad-free-price-modelling/1_Data/bq_rc_activity_mockup.csv",
           sep = ",",
           na.strings=c("","n/a"))

######################################################
##### REDISTRIBUTE NULL VALUE REVENUE
######################################################

# We cannot be sure of how revenue attributed to "untracked" users is distributed. 
# A fair assumption would be that it mirrors the revenue distribution of our "tracked" users. 
# So, one way to remain on the conservative side of our estimate is to redistribute this value 
# according to the existing revenue distribution. 

distributeNullRevenue <- function(data, id_name, revenue_column) {
  
  df <- data
  
  # Save NULL revenue.
  na_revenue <- as.integer(df[is.na(df[id_name])][revenue_column]) # CHANGE COLUMN NUMBER IF NEEDED.
  
  # Omit NULL revenue from dataframe.
  df %<>% 
    na.omit()
  
  # Calculate the total of remaining revenue.
  total_cookie_revenue <- sum(df["total_revenue"])
  
  # Calculate shares of remaining revenue per id and distribute NULL revenue based on those shares.
  df %>% 
    mutate(share_of_rev = total_revenue/total_cookie_revenue) %>% 
    mutate(adjusted_total_revenue = round(total_revenue + share_of_rev * na_revenue,0))
}

df_subs <- 
  distributeNullRevenue(data = df_subs,
                        id_name = "splus_id",
                        revenue_column = 4)

df_subs_excl_top100 <- 
  distributeNullRevenue(data = df_subs_excl_top100,
                        id_name = "splus_id",
                        revenue_column = 4)

df_activity <- 
  distributeNullRevenue(data = df_activity,
                        id_name = "rc_cookie",
                        revenue_column = 3)

######################################################
##### ASSIGN USER PERCENTILES BASED ON REVENUE SHARE
######################################################

df_subs %<>% 
  mutate(user_percentile = round(ntile(adjusted_total_revenue, nrow(df_subs)) / nrow(df_subs), 3)) 

df_subs_excl_top100 %<>% 
  mutate(user_percentile = round(ntile(adjusted_total_revenue, nrow(df_subs)) / nrow(df_subs), 3))

######################################################
##### PLOT REVENUE AND IMPRESSIONS INTERRELATION
######################################################

# Calculate ratio of revenue to impressions per id.
calculateRevsToImpsRatio <- function(data) {
  
  df_ratio <- data %>% 
    select(splus_id, impressions, adjusted_total_revenue) %>% 
    group_by(splus_id) %>% 
    
    # Summarise impressions and revenue per id
    summarise(total_imps = sum(impressions),
              total_rev = round(sum(adjusted_total_revenue))) %>% 
    
    # Divide those values on total impressions and revenue
    mutate(share_imps = (total_imps / sum(total_imps)),
           share_rev = (total_rev / sum(total_rev))) %>% 
    
    # Create a revenue-to-impressions ratio per id
    mutate(rev_to_imps_ratio = (share_rev/share_imps)) %>% 
    
    # Arrange values by ratio and calculate cumulative revenue and impressions
    ungroup() %>% 
    arrange(desc(rev_to_imps_ratio)) %>% 
    mutate(acc_share_imps = cumsum(share_imps),
           acc_share_rev = cumsum(share_rev))
  
  return(df_ratio)
}

# Visualise the distribution of cookies based on revenue and impression shares.
plotRevsToImpsRatio <- function(data) {
  
  df <- calculateRevsToImpsRatio(data)
  
  # Plot data
  ggplot(data = df, 
         mapping = aes(x = acc_share_imps,
                       y = acc_share_rev)) + 
    geom_point(alpha = 0.01,
               size = 0.001) + 
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) + 
    labs(title = "Share of impressions and revenue based on 'highest-value' splus_id",
         x = "Accumulated share of impressions",
         y = "Accumulated share of revenue") + 
    theme_clean()
  
}

plotRevsToImpsRatio(data = df_subs)
plotRevsToImpsRatio(data = df_subs_excl_top100)

createIdSegments <- function(data,
                             rev_share_divider = 0.90) {
  
  df <- calculateRevsToImpsRatio(data)
  
  df %<>% 
    mutate(id_type = ifelse(acc_share_rev >= rev_share_divider, 
                            "Low-Value", 
                            "High-Value"))
  
  return(df)
  
}

plotIdSegments <- function(data, 
                           rev_share_divider = 0.90) {
  
  df <- createIdSegments(data, 
                         rev_share_divider)
  
  SEGMENTPLOT <- 
    ggplot(data = df, 
           mapping = aes(x = acc_share_imps,
                         y = acc_share_rev,
                         colour = id_type)) + 
    geom_point(alpha = 0.8,
               size = 1) + 
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) + 
    labs(title = "Share of impressions and revenue based on 'highest-value' IDs",
         caption = "Date range: 1st to 31th August 2020", 
         x = "Accumulated share of impressions",
         y = "Accumulated share of revenue") + 
    theme_clean()
  
  IDCOUNT <-
    ggplot(data = df, 
           mapping = aes(x = id_type)) + 
    geom_bar() +
    scale_y_continuous(labels = comma) +
    labs(title = "Number of IDs considered high and low value, respectively",
         caption = "Date range: 1st to 31th August 2020", 
         x = "ID type",
         y = "ID count") +
    theme_clean()
  
  ggarrange(SEGMENTPLOT, IDCOUNT,
            ncol = 2, nrow = 1)
  
}

# Visualise the distribution of cookies based on a percentile divider.
plotIdSegments(data = df_subs, 
               rev_share_divider = 0.75)

######################################################
##### BOXPLOTS OF REVENUE PER SEGMENT
######################################################
plotBoxesBySegment <- function(data, segment, title, y_label, y_upper_limit = -1) {
  
  # Deal with setting the upper y limit.
  if(y_upper_limit == -1) {
    y_upper_limit <- 
      data %>% 
      ungroup() %>% 
      select(adjusted_total_revenue) %>% 
      max() + 10
  }
  
  ggplot(data = data, 
         mapping = aes(x = !!segment, # Remember to put quo() around this variable!
                       y = adjusted_total_revenue)) + 
    geom_boxplot() + 
    scale_y_continuous(label = comma) + 
    labs(title = title,
         caption = "Date range: 1st to 31th August 2020", 
         x = "Segment",
         y = y_label) + 
    coord_cartesian(ylim=c(0, y_upper_limit)) + 
    theme_calc()
  
}

###### BY SUBSCRIPTION TYPE (PRINT VS DIGITAL) ######
# Filter data
temp <- df_subs %>% 
  filter(ProductCategoryEnd == "Print" | ProductCategoryEnd == "Digital")

# Full plot
plotBoxesBySegment(data = temp,
                   segment = quo(ProductCategoryEnd), # Remember to put quo() here!
                   title = "Revenue per paying subscriber",
                   y_label = "SEK per Serviceplus-ID")

# Zoomed in plot
plotBoxesBySegment(data = temp,
                   segment = quo(ProductCategoryEnd), # Remember to put quo() here!
                   title = "Zoomed in: Revenue per paying subscriber",
                   y_label = "SEK per Serviceplus-ID",
                   y_upper_limit = 50)

###### BY LEVELS OF ACTIVITY ######
# Filter data
temp <- df_activity %>% 
  select(rc_cookie, activity, adjusted_total_revenue) %>% 
  group_by(rc_cookie, activity) %>% 
  summarise(adjusted_total_revenue = sum(adjusted_total_revenue))

# Full plot
plotBoxesBySegment(data = temp,
                   segment = quo(activity), # Remember to put quo() here!
                   title = "Revenue per paying cookie",
                   y_label = "SEK per Reynolds cookie")

# Zoomed in plot
plotBoxesBySegment(data = temp,
                   segment = quo(activity), # Remember to put quo() here!
                   title = "Zoomed in: Revenue per paying cookie",
                   y_label = "SEK per Reynolds cookie",
                   y_upper_limit = 100)


######################################################
##### CALCULATING QUANTILES
######################################################

quantiles <- as.data.frame(quantile(df_subs["adjusted_total_revenue"], 
                                    probs = seq(0, 1, 0.01), 
                                    na.rm = TRUE))

######################################################
##### CALCULATING MEAN, MEDIAN, SUM VALUES AND TALLIES (SANITY CHECK)
######################################################
aggregateValues <- function(data, segment) {
  
  avgs <- data %>% 
    select(!!segment, adjusted_total_revenue) %>% 
    group_by(!!segment) %>% 
    summarise(median_ad_revenue = round(median(adjusted_total_revenue),0), 
              mean_ad_revenue = round(mean(adjusted_total_revenue),0),
              sum_ad_revenue = round(sum(adjusted_total_revenue)))
  
  tly <- data %>% 
    group_by(!!segment) %>% 
    tally()
  
  return(merge(avgs, tly))
}

aggregateValues(data = df_subs, segment = quo(ProductCategoryEnd))
aggregateValues(data = df_activity, segment = quo(activity))

######################################################
##### PRICE MODELLING THE AD FREE EXPERIENCE
######################################################
# Generate key values regarding print and digital users and revenue. 
getKeyValues <- function(data) {
  
  # Get the number of digital users
  nbr_digital_users <- 
    data %>% 
    filter(ProductCategoryEnd == "Digital") %>% 
    tally() %>% 
    as.integer()
  
  # Get the total print revenue
  print_total_revenue <- 
    data %>% 
    filter(ProductCategoryEnd=="Print") %>% 
    summarise(adjusted_total_revenue = sum(adjusted_total_revenue)) %>% 
    as.integer()
  
  # Get the total digital revenue
  digital_total_revenue <- 
    data %>% 
    filter(ProductCategoryEnd=="Digital") %>% 
    summarise(adjusted_total_revenue = sum(adjusted_total_revenue)) %>% 
    as.integer()
  
  # The redistribution of print revenue is probably logically incorrect. 
  # We should redistribute print revenue based on the print distribution, not digital distribution. 
  # Here, we assume similar distribution (which is, incidentally, the case).
  
  # Calculate revenue per digital user by redistributing the print revenue based on the digital revenue distribution
  subs_only_digital <- 
    data %>% 
    filter(ProductCategoryEnd=="Digital") %>% 
    mutate(share_of_rev = adjusted_total_revenue / digital_total_revenue) %>% 
    mutate(adjusted_total_revenue = round(adjusted_total_revenue + share_of_rev * print_total_revenue,0))
  
  # Recalculate percentiles for this group (not necessary?)
  subs_only_digital %<>% 
    mutate(user_percentile = round(ntile(adjusted_total_revenue, nrow(subs_only_digital)) / nrow(subs_only_digital),3))
  
  # Get print users (not necessary?)
  subs_only_print <- 
    data %>% 
    filter(ProductCategoryEnd=="Print")
  
  # Save output values
  lst = list()
  
  lst$nbr_digital_users <- nbr_digital_users
  lst$print_total_revenue <- print_total_revenue
  lst$digital_total_revenue <- digital_total_revenue
  lst$subs_only_digital <- subs_only_digital
  lst$subs_only_print <- subs_only_print
  
  return(lst)
  
}

# Create MODEL 1 and MODEL 2 variants per percentile of user adoption.
createStandardModelVariants <- function(data, percentile) {
  
  KV <- getKeyValues(data)
  
  # MODEL 1 and MODEL 2 are built with this function. Both models assume that 
  # our users sign up for the ad free experience in descending order from those 
  # this the highest ad revenue to those with the lowest. Meaning, our most valuable 
  # ad revenue sources are lost first. This is to give a conservative estimate based
  # on a "worst" scenario - in which we would maximise our loss - and what the break-even
  # price becomes depending on how many percent of our user base signs up. 
  
  # So what is actually different between MODEL 1 and MODEL 2?
  # MODEL 1 is based on all subscriptions, whereas MODEL 2 excludes the top 100 since these are
  # to a large extent B2B rather than B2C users. Orgs should not be eligible for the ad free experience.
  
  #############################################
  # VARIANT 1: Everyone pays (incl. digital + print).
  #############################################
  V1 <- 
    data %>% 
    filter(adjusted_total_revenue >= quantile(adjusted_total_revenue, 1-percentile)) %>% 
    select(adjusted_total_revenue) %>% 
    summarise(mean = round(mean(adjusted_total_revenue),0))
  
  #############################################
  # VARIANT 2: Only digital pays. Assumed that print readers get ad free to the same
  # extent as digital readers - and that their revenue distribution mimics the digital.
  # Break-even is reached by the digital ad free users paying for their print counterparts.
  #############################################
  V2 <- 
    KV$subs_only_digital %>% 
    filter(adjusted_total_revenue >= quantile(adjusted_total_revenue, 1-percentile)) %>% 
    select(adjusted_total_revenue) %>% 
    summarise(mean = round(mean(adjusted_total_revenue),0))
  
  #############################################
  # VARIANT 3: Only digital pays. Assumed that ALL print readers get ad free. Break-even is
  #  reached by the digital ad free users paying for all the digital ad revenue by print users. 
  #############################################
  V3 <- 
    data %>% 
    filter(ProductCategoryEnd == "Digital" & adjusted_total_revenue >= quantile(adjusted_total_revenue, 1-percentile)) %>% 
    select(adjusted_total_revenue) %>% 
    summarise(mean = round(mean(adjusted_total_revenue) + (KV$print_total_revenue/(ifelse(p!=0, KV$nbr_digital_users * p, 1))),0))
  
  
  #############################################
  # SAVE OUTPUT 
  #############################################
  modelVariants = list()
  
  modelVariants$V1 <- V1
  modelVariants$V2 <- V2
  modelVariants$V3 <- V3
  
  return(modelVariants)
  
}

# Create MODEL 3 variants per percentile of user adoption. 
createRandomisedModelVariants <- function(data, percentile) {
  
  KV <- getKeyValues(data)
  
  # MODEL 3 is slightly different from MODEL 1 and MODEL 2. Where 1 and 2 both assume 
  # that our most valuable ad revenue users sign up first, 3 assumes that that there is 
  # some degree of randomness to the sign-ups. For each half percent of users that sign up  
  # from the top users, this model assumes a half percent of users that sign up which are 
  # sampled randomly from the rest of the user pool. This is a less conservative estimate
  # which is likely to give a significantly lower break-even price. 
  
  #############################################
  # VARIANT 1: Everyone pays (incl. digital + print).
  #############################################
  # Calculate the mean of the top half of our percentile. 
  V1_step1 <- 
    data %>% 
    filter(adjusted_total_revenue >= quantile(adjusted_total_revenue, 1 - p/2)) %>% 
    select(adjusted_total_revenue) %>% 
    summarise(mean = round(mean(adjusted_total_revenue),0)) %>% 
    as.integer()
  
  # Recalculate the fraction to get p% of the remaining pool of users when we sample. 
  new_frac = (p/2)/(1-(p/2))
  
  # Calculate the mean of a randomly sampled half of our percentile.
  V1_step2 <- 
    data %>% 
    filter(adjusted_total_revenue < quantile(adjusted_total_revenue, 1 - p/2)) %>% 
    select(adjusted_total_revenue) %>% 
    sample_frac(new_frac) %>% 
    summarise(mean = round(mean(adjusted_total_revenue),0)) %>% 
    as.integer()
  
  # Make sure that the randomly sampled mean is not null. 
  V1_step2 <- ifelse(is.na(V1_step2), 0, V1_step2)
  
  # Calculate the mean of the means to get the break-even price.
  V1 <- round((V1_step1 + V1_step2)/2,0)
  
  #############################################
  # VARIANT 2: Only digital pays. Assumed that print readers get ad free to the same
  # extent as digital readers - and that their revenue distribution mimics the digital.
  # Break-even is reached by the digital ad free users paying for their print counterparts.
  #############################################
  V2_step1 <- 
    KV$subs_only_digital %>% 
    filter(adjusted_total_revenue >= quantile(adjusted_total_revenue, 1 - p/2)) %>% 
    select(adjusted_total_revenue) %>% 
    summarise(mean = round(mean(adjusted_total_revenue),0)) %>% 
    as.integer()
  
  V2_step2 <- 
    KV$subs_only_digital %>% 
    filter(adjusted_total_revenue < quantile(adjusted_total_revenue, 1 - p/2)) %>% 
    select(adjusted_total_revenue) %>% 
    sample_frac(new_frac) %>% 
    summarise(mean = round(mean(adjusted_total_revenue),0)) %>% 
    as.integer()
  
  V2_step2 <- ifelse(is.na(V2_step2), 0, V2_step2) # == TRUE
  
  V2 <- round((V2_step1 + V2_step2)/2,0)
  
  #############################################
  # SAVE OUTPUT 
  #############################################
  modelVariants = list()
  
  modelVariants$V1 <- V1
  modelVariants$V2 <- V2
  
  return(modelVariants)
  
}

# Create dataframe for break-even prices per percentile of user adoption.
scenarios <- data.frame(percentile = integer(),
                        mean = integer())

# Run the price model per percentile. 
for (p in seq(0, 1, 0.01)) {
  
  ############ MODEL 1 VARIANTS ############ 
  M1 <- createStandardModelVariants(data = df_subs, percentile = p)
  
  ############ MODEL 2 VARIANTS ############ 
  M2 <- createStandardModelVariants(data = df_subs_excl_top100, percentile = p)
  
  ############ MODEL 3 VARIANTS ############ 
  M3 <- createRandomisedModelVariants(data = df_subs, percentile = p)
  
  ############ USERS PER PERCENTILE ############ 
  subs <- 
    df_subs %>% 
    filter(adjusted_total_revenue >= quantile(adjusted_total_revenue, 1-p)) %>% 
    tally() %>% 
    as.integer()
  
  ############ JOIN TOGETHER ############ 
  to_add <- data.frame(p, 
                       subs,
                       as.integer(M1$V1["mean"]), 
                       as.integer(M1$V2["mean"]), 
                       as.integer(M1$V3["mean"]),
                       as.integer(M2$V1["mean"]), 
                       as.integer(M2$V2["mean"]), 
                       as.integer(M2$V3["mean"]),
                       M3$V1,
                       M3$V2)
  names(to_add) <- c("Percentile", 
                     "Subscribers",
                     "M1: Everyone pays", 
                     "M1: Digital pays for corresponding print", 
                     "M1: Digital pays for all print",
                     "M2: Everyone pays", 
                     "M2: Digital pays for corresponding print", 
                     "M2: Digital pays for all print",
                     "M3: Everyone pays",
                     "M3: Digital pays for corresponding print") 
  scenarios <- rbind(scenarios, to_add)
  
}

scenarios_melt <- 
  scenarios %>% 
  dplyr::select(-c(Subscribers)) %>% 
  filter(Percentile > 0) %>% 
  melt(id=c("Percentile"))

ggplot(data = scenarios_melt, 
       aes(x = Percentile, 
           y = value, 
           color = variable)) +
  geom_line(size = 1) +
  labs(title = "Modelling of ad free add-on price", 
       x = "% highest ad-revenue users signing up for ad free", 
       y = "Break-even price (SEK)", 
       color = "Models and variants") + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_clean()

######################################################
##### HOW MUCH OF THE REVENUE DOES THE TOP X% MAKE?
######################################################

shareOfRev <- function(data, percentile){
  
  temp <- 
    data %>% 
    filter(adjusted_total_revenue >= quantile(adjusted_total_revenue, percentile)) %>% 
    select(adjusted_total_revenue) %>% 
    summarise(sum = round(sum(adjusted_total_revenue),0)) %>% 
    as.integer()
  
  tot_rev <- 
    data %>% 
    select(adjusted_total_revenue) %>% 
    summarise(sum = round(sum(adjusted_total_revenue),0)) %>% 
    as.integer()
  
  return(round(temp/tot_rev, 2))
}

shareOfRev(data = df_subs, 
           percentile = 0.99)

shareOfRev(data = df_subs, 
           percentile = 0.95)

shareOfRev(data = df_subs, 
           percentile = 0.5)
