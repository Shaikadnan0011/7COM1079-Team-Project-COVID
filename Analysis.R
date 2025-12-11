state    <- read.csv("C:/Users/cases_state.csv", stringsAsFactors = FALSE)

# View first few rows
head(state)

############ 3.1 Visualization for RQ ##################

# Convert date to Date format
state$date <- as.Date(state$date)

# Filter only two states: Selangor and Sabah
two_states <- subset(state, state %in% c("Selangor", "Sabah"))

# MAIN PLOT: Boxplot comparing mean daily cases
boxplot(two_states$cases_new ~ two_states$state,
        main = "Comparison of Daily COVID-19 Cases: Selangor vs Sabah",
        xlab = "State",
        ylab = "Daily New COVID-19 Cases",
        col = c("lightblue", "lightgreen"))

# Add caption under plot
mtext("Figure 1: Boxplot showing distribution of daily COVID-19 cases for Selangor and Sabah.", 
      side = 1, line = 4, cex = 0.8)

# SUPPLEMENTARY PLOT: Histogram for each state
par(mfrow = c(1,2))  # 2 plots side-by-side

hist(two_states$cases_new[two_states$state == "Selangor"],
     main = "Histogram of Daily Cases in Selangor",
     xlab = "Daily New Cases",
     ylab = "Frequency",
     col = "skyblue")

hist(two_states$cases_new[two_states$state == "Sabah"],
     main = "Histogram of Daily Cases in Sabah",
     xlab = "Daily New Cases",
     ylab = "Frequency",
     col = "lightgreen")

# Reset plot window
par(mfrow = c(1,1)
    
################# 3.2 Addtional information of the data ################

# SIMPLE SCATTER PLOT (Daily cases over time)
plot(two_states$date, two_states$cases_new,
     main = "Daily COVID-19 Cases Over Time (Both States)",
     xlab = "Date", ylab = "Daily New Cases",
     col = ifelse(two_states$state == "Selangor", "blue", "red"),
     pch = 16)

legend("topright", legend=c("Selangor", "Sabah"),
       col=c("blue", "red"), pch=16)

# LINE PLOT FOR SELANGOR
selangor <- subset(two_states, state == "Selangor")
plot(selangor$date, selangor$cases_new, type="l",
     main = "Trend of Daily Cases in Selangor",
     xlab = "Date", ylab = "Daily New Cases",
     col = "blue", lwd = 2)

# LINE PLOT FOR SABAH
sabah <- subset(two_states, state == "Sabah")
plot(sabah$date, sabah$cases_new, type="l",
     main = "Trend of Daily Cases in Sabah",
     xlab = "Date", ylab = "Daily New Cases",
     col = "red", lwd = 2)

# SIMPLE BARPLOT (Mean comparison)
means <- tapply(two_states$cases_new, two_states$state, mean)
barplot(means,
        main = "Mean Daily Cases: Selangor vs Sabah",
        ylab = "Mean Daily Cases",
        xlab = "State",
        col = c("lightblue","lightgreen"))


############### 4.1. Statistical test ###############

# Filter the two states again (if needed)
two_states <- subset(state, state %in% c("Selangor", "Sabah"))

# Separate groups
selangor_cases <- two_states$cases_new[two_states$state == "Selangor"]
sabah_cases    <- two_states$cases_new[two_states$state == "Sabah"]

# INDEPENDENT TWO-SAMPLE T-TEST
t_test_result <- t.test(selangor_cases,
                        sabah_cases,
                        alternative = "two.sided",
                        var.equal = FALSE)

# Display output
t_test_result
