folder_path <- "Coursera-SwiftKey/final/en_US/"

#Question 2
file_path <- "Coursera-SwiftKey/final/en_US/en_US.twitter.txt"
num_lines <- length(readLines(file_path))
print(num_lines)

#3 What is the length of the longest line seen in any of the three en_US data sets? 
file_paths <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")

longest_line <- 0

for (file in file_paths) {
  lines <- readLines(paste0("Coursera-SwiftKey/final/en_US/",file), warn = FALSE)  # Read all lines
  max_length <- max(nchar(lines))  # Get max length in this file
  longest_line <- max(longest_line, max_length)  # Update max overall
  print(paste0(longest_line, " in ", file))
  longest_line <- 0
}



#4 Question 4
#In the en_US twitter data set, if you divide the number of lines 
# where the word "love" (all lowercase) occurs 
# by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
# Define file path
file_path <- paste0(folder_path,"en_US.twitter.txt")

# Open the file
con <- file(file_path, "r")

# Initialize counters
love_count <- 0
hate_count <- 0

# Read file line by line
while (TRUE) {
  line <- readLines(con, n = 1, warn = FALSE)
  if (length(line) == 0) break  # Stop if EOF
  
  # Check for occurrences of "love" and "hate" (case-sensitive)
  if (grepl("\\blove\\b", line, ignore.case = FALSE)) love_count <- love_count + 1
  if (grepl("\\bhate\\b", line, ignore.case = FALSE)) hate_count <- hate_count + 1
}

# Close the file
close(con)

# Compute ratio
ratio <- love_count / hate_count
print(ratio)


#5 The one tweet in the en_US twitter data set that matches the word "biostats" says what?
# Open the file
con <- file(file_path, "r")

# Read all lines (if memory allows)
tweets <- readLines(con, warn = FALSE)
# Function Overview
# grep(pattern, x, ignore.case = FALSE, fixed = FALSE, value = FALSE)
# grep() is used to search for patterns in a vector of character strings.
# It returns the index positions of elements that match the pattern.


# Close the file
close(con)

# Find the tweet that contains "biostats" (case-sensitive)
matching_tweet <- tweets[grep("\\bbiostats\\b", tweets)]

# Print the result
print(matching_tweet)


#6
# How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". 
# (I.e. the line matches those characters exactly.)
file_path <- paste0(folder_path,"en_US.twitter.txt")
# Open the file and read all lines
tweets <- readLines(file_path, warn = FALSE)

# Define the exact phrase
search_phrase <- "A computer once beat me at chess, but it was no match for me at kickboxing"

# Count occurrences of the phrase (partial match)
match_count <- sum(grepl(search_phrase, tweets, fixed = TRUE))

# Print result
print(match_count)
