
# STEP 1: INSTALL & LOAD LIBRARIES

# Run once if needed
# install.packages(c("tidyverse", "corrplot", "caret", "randomForest",
#                    "cluster", "factoextra", "e1071", "rpart", "rpart.plot",
#                    "ggcorrplot", "vcd"))

library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(cluster)
library(factoextra)
library(e1071)
library(rpart)
library(rpart.plot)
library(ggcorrplot)
library(vcd)

# STEP 2: LOAD DATA

df <- read.csv("student_data2.csv", stringsAsFactors = FALSE)

# Clean column names (replace spaces/special chars with dots)
colnames(df) <- gsub("[^A-Za-z0-9]+", ".", colnames(df))
colnames(df) <- trimws(colnames(df))

# Display cleaned column names
cat("✅ Cleaned Column Names:\n")
print(colnames(df))


# STEP 3: CLEAN GPA COLUMN & REMOVE NAs

gpa_col <- "Current.GPA.CGPA.or.percentage"
df[[gpa_col]] <- gsub("%", "", df[[gpa_col]])
df[[gpa_col]] <- gsub("[^0-9\\.]", "", df[[gpa_col]])
df[[gpa_col]] <- as.numeric(df[[gpa_col]])
df <- df[!is.na(df[[gpa_col]]), ]
summary(df[[gpa_col]])


# STEP 4: REMOVE UNNECESSARY COLUMNS

df <- df %>% select(-c(Roll.no., Name))


# STEP 5: HANDLE MISSING VALUES

colSums(is.na(df))
for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
  } else {
    mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
    df[[col]][is.na(df[[col]])] <- mode_val
  }
}


# STEP 6: ENCODE CATEGORICAL VARIABLES

df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)

# STEP 7: SCALE NUMERIC FEATURES

num_cols <- sapply(df, is.numeric)
df[num_cols] <- scale(df[num_cols])


# STEP 8: CREATE GPA CATEGORY

qtiles <- quantile(df[[gpa_col]], probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
qtiles <- unique(qtiles)

if (length(qtiles) < 4) {
  qtiles <- seq(min(df[[gpa_col]], na.rm = TRUE), max(df[[gpa_col]], na.rm = TRUE), length.out = 4)
}

df$GPA_Category <- cut(df[[gpa_col]], breaks = qtiles,
                       labels = c("Low", "Medium", "High"),
                       include.lowest = TRUE)
df <- df[!is.na(df$GPA_Category), ]

if (any(table(df$GPA_Category) < 3)) {
  median_gpa <- median(df[[gpa_col]], na.rm = TRUE)
  df$GPA_Category <- ifelse(df[[gpa_col]] <= median_gpa, "Low", "High")
  df$GPA_Category <- as.factor(df$GPA_Category)
}

# STEP 9: RENAME COLUMNS TO SIMPLER NAMES

names(df)[grepl("Number.of.hours.studied.per.day", names(df), ignore.case = TRUE)] <- "StudyHours"
names(df)[grepl("Average.sleep.per.night", names(df), ignore.case = TRUE)] <- "SleepHours"
names(df)[grepl("Quality.of.sleep", names(df), ignore.case = TRUE)] <- "SleepQuality"
names(df)[grepl("Average.daily.screen.time", names(df), ignore.case = TRUE)] <- "ScreenTime"
names(df)[grepl("Time.spent.on.social.media", names(df), ignore.case = TRUE)] <- "SocialMedia"
names(df)[grepl("Average.hours.of.leisure", names(df), ignore.case = TRUE)] <- "LeisureTime"
names(df)[grepl("Self.reported.stress.level", names(df), ignore.case = TRUE)] <- "StressLevel"
names(df)[grepl("Attendance", names(df), ignore.case = TRUE)] <- "Attendance"
names(df)[grepl("study.plan", names(df), ignore.case = TRUE)] <- "StudyPlan"
names(df)[grepl("meal.regularity", names(df), ignore.case = TRUE)] <- "MealRegularity"
names(df)[grepl("fall.asleep.in.class", names(df), ignore.case = TRUE)] <- "FallAsleep"
names(df)[grepl("affecting.your.academics", names(df), ignore.case = TRUE)] <- "AcademicIssues"

cat("✅ Renamed Columns:\n")
print(colnames(df))


# STEP 10: CORRELATION ANALYSIS

num_data <- df[, sapply(df, is.numeric)]
corr_matrix <- cor(num_data, use = "pairwise.complete.obs")
ggcorrplot(corr_matrix, lab = TRUE, title = "Correlation Heatmap: Habits vs GPA")


# STEP 11: VISUALIZATION — LIFESTYLE & ACADEMIC PATTERNS


# --- PIE CHARTS ---
# Sleep Duration
sleep_data <- df %>% count(SleepHours)
ggplot(sleep_data, aes(x = "", y = n, fill = SleepHours)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Average Sleep Duration", fill = "Sleep Hours") +
  theme_void()

# Screen Time
screen_data <- df %>% count(ScreenTime)
ggplot(screen_data, aes(x = "", y = n, fill = ScreenTime)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Average Daily Screen Time", fill = "Screen Time (hrs)") +
  theme_void()

# Leisure/Hobby Time
leisure_data <- df %>% count(LeisureTime)
ggplot(leisure_data, aes(x = "", y = n, fill = LeisureTime)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Leisure / Hobby Hours per Day", fill = "Leisure Time") +
  theme_void()

# --- BOX PLOTS ---
ggplot(df, aes(x = SleepQuality, y = df[[gpa_col]], fill = SleepQuality)) +
  geom_boxplot() +
  labs(title = "GPA Distribution by Sleep Quality",
       x = "Sleep Quality", y = "GPA") +
  theme_minimal()

ggplot(df, aes(x = Attendance, y = df[[gpa_col]], fill = Attendance)) +
  geom_boxplot() +
  labs(title = "GPA across Attendance Levels",
       x = "Attendance (%)", y = "GPA") +
  theme_minimal()

# --- SCATTER PLOT: Study Hours vs GPA ---
ggplot(df, aes(x = StudyHours, y = df[[gpa_col]], color = Gender)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Study Hours vs GPA",
       x = "Hours Studied per Day", y = "GPA") +
  theme_minimal()

# --- MOSAIC PLOT ---
df$Pass_Fail <- ifelse(df[[gpa_col]] >= median(df[[gpa_col]], na.rm = TRUE), "Pass", "Fail")
mosaic(~ SleepHours + Pass_Fail, data = df,
       main = "Mosaic Plot: Sleep Hours vs Pass/Fail",
       shade = TRUE, legend = TRUE)

# --- FINAL CORRELATION HEATMAP ---
num_data <- df[, sapply(df, is.numeric)]
corr_matrix <- cor(num_data, use = "pairwise.complete.obs")
ggcorrplot(corr_matrix, lab = TRUE, title = "Final Correlation Heatmap", ggtheme = theme_minimal())


# STEP 12: SAVE CLEANED DATA

write.csv(df, "cleaned_student_data_final.csv", row.names = FALSE)
cat("✅ Cleaned dataset saved successfully as 'cleaned_student_data_final.csv'\n")

