library(tm)
library(text2vec)
library(stopwords)

dir <- "Coursera-SwiftKey/final/en_US"  # Directory containing the files
filelist <- list.files(dir, full.names = TRUE)  # Get full file paths

# Read all files and combine into one text data variable
text_data <- unlist(lapply(filelist, function(file) {
  readLines(file, warn = FALSE, encoding = "UTF-8")  # Read each file
}))

# Create a Corpus
corpus <- Corpus(VectorSource(text_data))

# Apply Text Transformations
corpus <- tm_map(corpus, content_transformer(tolower))       # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)                  # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                      # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))       # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace)                    # Remove extra whitespace

# Extract processed text
preprocessed_text <- sapply(corpus, as.character, USE.NAMES = FALSE)

# Tokenize text properly
tokens <- word_tokenizer(preprocessed_text)

# Create an iterator over tokens
it <- itoken(tokens, progressbar = FALSE)

# Create a vocabulary
vocab <- create_vocabulary(it)

# Prune vocabulary to remove rare words (adjust the threshold as needed)
vocab <- prune_vocabulary(vocab, term_count_min = 5)  

# Create a vectorizer
vectorizer <- vocab_vectorizer(vocab)

# Create a term-cooccurrence matrix (TCM)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

# Train the GloVe Model
glove <- GlobalVectors$new(
  rank = 100,     # Word vector size
  x_max = 10      # Parameter for weighting function
)

# Train word vectors
word_vectors_main <- glove$fit_transform(tcm, n_iter = 10)

# Obtain final word embeddings by combining context word vectors
word_vectors_context <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_context)

# Display the trained word embeddings
head(word_vectors)


# Function to compute cosine similarity
cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

# Function to find nearest neighbors
find_nearest_neighbors <- function(word_vectors, target_word, top_n = 5) {
  if (!(target_word %in% rownames(word_vectors))) {
    stop("Word not found in vocabulary!")
  }
  
  # Get the vector for the target word
  target_vector <- word_vectors[target_word, , drop = FALSE]
  
  # Compute cosine similarity with all other words
  similarities <- apply(word_vectors, 1, function(vec) cosine_similarity(target_vector, vec))
  
  # Sort words by similarity (descending order)
  nearest_words <- sort(similarities, decreasing = TRUE)[2:(top_n + 1)]
  
  return(data.frame(Word = names(nearest_words), Similarity = nearest_words))
}

# Example: Find 5 words most similar to "dog"
similar_words <- find_nearest_neighbors(word_vectors, "dog", top_n = 5)
print(similar_words)

#########################################################################################
# Function to get the sentence embedding (average of word vectors)
sentence_embedding <- function(sentence, word_vectors) {
  words <- unlist(strsplit(sentence, " "))  # Tokenize sentence
  words <- words[words %in% rownames(word_vectors)]  # Keep words in vocabulary
  if (length(words) == 0) return(NULL)  # If no words exist in embeddings, return NULL
  return(colMeans(word_vectors[words, , drop = FALSE]))  # Average of word vectors
}

# Function to predict the most probable next word
predict_next_word <- function(sentence, candidates, word_vectors) {
  sentence_vec <- sentence_embedding(sentence, word_vectors)
  
  if (is.null(sentence_vec)) stop("No valid words found in vocabulary!")
  
  # Compute cosine similarity with each candidate
  similarities <- sapply(candidates, function(word) {
    if (!(word %in% rownames(word_vectors))) return(NA)  # Skip words not in vocabulary
    cosine_similarity(sentence_vec, word_vectors[word, , drop = FALSE])
  })
  
  # Return the word with highest similarity
  best_word <- names(sort(similarities, decreasing = TRUE, na.last = TRUE))[1]
  return(best_word)
}




#1 Candidate words
candidates <- c("eat", "sleep", "give", "die")

# Predict the best next word
predict_next_word("live and I'd", candidates, word_vectors)

#2 Candidate words
candidates <- c("horticultural", "marital", "financial", "spiritual")

# Predict the best next word
predict_next_word("I asked about dessert and he started telling me about his", candidates, word_vectors)


candidates <- c("decade", "morning", "weekend", "month")
predict_next_word("I'd give anything to see Arctic Monkeys this", candidates, word_vectors)

candidates <- c("happiness", "hunger", "sleepiness", "stress")
predict_next_word("Talking to your mom has the same effect as a hug and helps reduce your", candidates, word_vectors)

candidates <- c("minute", "look", "picture", "walk")
predict_next_word("When you were in Holland you were like 1 inch away from me but you hadn't time to take a", candidates, word_vectors)

candidates <- c("case", "incident", "account", "matter")
predict_next_word("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the", candidates, word_vectors)

candidates <- c("arm", "hand", "toe", "finger")
predict_next_word("I can't deal with unsymmetrical things. I can't even hold an uneven number of bags of groceries in each", candidates, word_vectors)

candidates <- c("middle", "side", "top", "center")
predict_next_word("Every inch of you is perfect from the bottom to the", candidates, word_vectors)

candidates <- c("weekly", "daily", "inside", "outside")
predict_next_word("I’m thankful my childhood was filled with imagination and bruises from playing", candidates, word_vectors)

candidates <- c("novels", "pictures", "stories", "movies")
predict_next_word("I like how the same people are in almost all of Adam Sandler's", candidates, word_vectors)

saveRDS(word_vectors, file = "trained_model.rds")
word_vectors <- readRDS("trained_model.rds")


