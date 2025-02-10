head(MindSetDF)
df <- MindSetDF

# Add 50 to all Liking and Consideration values as the range is -50 to +50 
df$Liking <- df$Liking + 50
df$Consideration <- df$Consideration + 50

# POTENTIAL

# Calculate potential for awareness
potential_awareness <- (100 - mean(df$Awareness)) / 100
potential_awareness

# Calculate potential for liking
potential_liking <- (100 - mean(df$Liking)) / 100
potential_liking

# Calculate potential for consideration
potential_consideration <- (100 - mean(df$Consideration)) / 100
potential_consideration

# Store the potential values in a dataframe
table_potential <- data.frame(
  potential = c("awareness", "consideration", "liking"), 
  value = c(potential_awareness, potential_consideration, potential_liking)
)
table_potential


# STICKINESS

# Awareness
ar1 <- ar(df$Awareness, aic = TRUE)
stick_awareness <- sum(unlist(ar1$ar))

# Consideration
ar2 <- ar(df$Consideration, aic = TRUE)
stick_consideration <- sum(unlist(ar2$ar))

# Liking
ar3 <- ar(df$Liking, aic = TRUE)
stick_liking <- sum(unlist(ar3$ar))

stick_awareness
stick_consideration
stick_liking


# RESPONSIVENESS

# Generate lagged variables for each mindset metric and sales
df$lag_aware <- lag(df$Awareness)
df$lag_aware[1] <- 0
df$lag_consideration <- lag(df$Consideration)
df$lag_consideration[1] <- 0
df$lag_liking <- lag(df$Liking)
df$lag_liking[1] <- 0
df$lag_sales <- lag(df$Sales)
df$lag_sales[1] <- 0

# Lagged marketing variables
df$lag_insta <- lag(df$InstagramAds)
df$lag_insta[1] <- 0
df$lag_tiktok <- lag(df$TikTokAds)
df$lag_tiktok[1] <- 0
df$lag_sea <- lag(df$SEA)
df$lag_sea[1] <- 0
df$lag_pospromotion <- lag(df$PoSPromotions)
df$lag_pospromotion[1] <- 0
df$lag_colabs <- lag(df$InfluencerColabs)
df$lag_colabs[1] <- 0

# Log-linear models
response_aware <- lm(log(df$Awareness + 1) ~ log(df$lag_aware + 1) + 
                       log(df$InstagramAds + 1) + log(df$TikTokAds + 1) + 
                       log(df$SEA + 1) + log(df$PoSPromotions + 1) + 
                       log(df$InfluencerColabs + 1), data = df)

response_consideration <- lm(log(df$Consideration + 1) ~ log(df$lag_consideration + 1) + 
                               log(df$InstagramAds + 1) + log(df$TikTokAds + 1) + 
                               log(df$SEA + 1) + log(df$PoSPromotions + 1) + 
                               log(df$InfluencerColabs + 1), data = df)

response_liking <- lm(log(df$Liking + 1) ~ log(df$lag_liking + 1) + 
                        log(df$InstagramAds + 1) + log(df$TikTokAds + 1) + 
                        log(df$SEA + 1) + log(df$PoSPromotions + 1) + 
                        log(df$InfluencerColabs + 1), data = df)

response_sales <- lm(log(df$Sales + 1) ~ log(df$lag_sales + 1) + 
                       log(df$InstagramAds + 1) + log(df$TikTokAds + 1) + 
                       log(df$SEA + 1) + log(df$PoSPromotions + 1) + 
                       log(df$InfluencerColabs + 1), data = df)

# Summarize results
summary(response_aware)
summary(response_consideration)
summary(response_liking)
summary(response_sales)

table_response_aware<-data.frame(round(response_aware$coefficients,5))
table_response_liking<-data.frame(round(response_liking$coefficients,5))
table_response_consideration<-data.frame(round(response_consideration$coefficients,5))
table_response_sales<-data.frame(round(response_sales$coefficients,5))

table_response_aware
table_response_consideration
table_response_liking
table_response_sales

# CONVERSION

conversion <- lm(log(df$Sales + 1) ~ log(df$lag_sales + 1) + 
                   log(df$Awareness + 1) + log(df$Consideration + 1) + 
                   log(df$Liking + 1), data = df)
summary(conversion)

table_conversion <-data.frame(round(conversion$coefficients,5))
table_conversion

# APPEAL

appeal_mindset_marketVar <- function(mindset, marketing_var_index){
  appeal <- round(table_final[2,mindset]*table_final[marketing_var_index,mindset]*(1/(1-table_final[3,mindset]))*table_final[9,mindset],5)
  return(appeal)
}

appeal_marketVar <- function(marketing_var_index) {
  mindsets <- c("awareness", "consideration", "liking")
  result <- sum(sapply(mindsets, appeal_mindset_marketVar, marketing_var_index=marketing_var_index))
  return(result)
}

# Calculate appeal for each marketing variable
appeal_marketVar(4) # insta
appeal_marketVar(5) # tiktok
appeal_marketVar(6) # sea
appeal_marketVar(7) # pospromotion
appeal_marketVar(8) # colabs


# CORRELATION MATRIX

install.packages("reshape2")  # Install the package


# Load necessary libraries
library(ggplot2)
library(reshape2)       

# Create the correlation matrix
correlation_matrix <- cor(df[, c("Awareness", "Consideration", "Liking", "Sales", 
                                 "InstagramAds", "TikTokAds", "SEA", "PoSPromotions", 
                                 "InfluencerColabs")], use = "complete.obs")

# Melt the correlation matrix for ggplot2
correlation_melted <- melt(correlation_matrix)

# Plot the heatmap using ggplot2
ggplot(data = correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables")



# RESULTS
table_final <- data.frame(
  Item = c("beginning level", "potential", "stickiness", "responsiveness to insta", 
           "responsiveness to tiktok", "responsiveness to SEA", "responsiveness to PoSPromotion", 
           "responsiveness to influencer colabs", "conversion"), 
  awareness = c(round(mean(df$Awareness) / 100, 3), round(potential_awareness, 3), 
                round(stick_awareness, 3), round(response_aware$coefficients[3], 5), 
                round(response_aware$coefficients[4], 5), round(response_aware$coefficients[5], 5), 
                round(response_aware$coefficients[6], 5), round(response_aware$coefficients[7], 5), 
                round(conversion$coefficients[3], 3)), 
  consideration = c(round(mean(df$Consideration) / 100, 3), round(potential_consideration, 3), 
                    round(stick_consideration, 3), round(response_consideration$coefficients[3], 5), 
                    round(response_consideration$coefficients[4], 5), round(response_consideration$coefficients[5], 5), 
                    round(response_consideration$coefficients[6], 5), round(response_consideration$coefficients[7], 5), 
                    round(conversion$coefficients[4], 3)), 
  liking = c(round(mean(df$Liking) / 100, 3), round(potential_liking, 3), 
             round(stick_liking, 3), round(response_liking$coefficients[3], 5), 
             round(response_liking$coefficients[4], 5), round(response_liking$coefficients[5], 5), 
             round(response_liking$coefficients[6], 5), round(response_liking$coefficients[7], 5), 
             round(conversion$coefficients[5], 3))
)
table_final
