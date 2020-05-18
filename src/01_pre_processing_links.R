# Load libraries ----
library(readxl)
library(purrr)
library(readr)

# Load texts ----
#fake_files <- list.files(path = "data/Fake.br-Corpus-master/full_texts/fake")
#fake_files <- paste0("data/Fake.br-Corpus-master/full_texts/fake/", fake_files)
#fake_texts <- lapply(fake_files, read_file)
#head(fake_texts)

# Load meta info ----

dir("data/Fake.br-Corpus-master/full_texts/true-meta-information")

fake_links_path <- "data/Fake.br-Corpus-master/full_texts/fake-meta-information/"
fake_files_links <- list.files(path = fake_links_path)
fake_files_links <- paste0(fake_links_path, fake_files_links)
fake_links <- lapply(fake_files_links, read_file)
head(fake_links)

true_links_path <- "data/Fake.br-Corpus-master/full_texts/true-meta-information/"
true_files_links <- list.files(path = true_links_path)
true_files_links <- paste0(true_links_path, true_files_links)
true_links <- lapply(true_files_links, read_file)
head(true_links)

links <- c(unlist(fake_links), unlist(true_links))
links_list <- map(links, function(x){
  x_split <- strsplit(x, "\r\n")
  x_split[[1]][2]
  })

links <- unlist(links_list)
links_cleaned <- gsub("https://", "", links)
links_cleaned <- gsub("/", " ", links_cleaned)
links_cleaned <- gsub("-", " ", links_cleaned)
links_cleaned

y <- c(rep("Fake", length(fake_links)), rep("True", length(true_links)))

df <- data.frame(links = links_cleaned, y)

# Text mining packages 
# install.packages('tm', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(tm)
library(SnowballC)

# Step 1 - Create a corpus text
corpus = Corpus(VectorSource(df$links))
corpus[[1]][1]

## Step 2 - Conversion to Lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus[[1]][1]

#Step 3 - Removing Punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]][1]

#Step 4 - Removing Stopwords and other words
corpus = tm_map(corpus, removeWords, c("http", stopwords("portuguese")))

corpus = tm_map(corpus, removeNumbers)
corpus[[1]][1]  

# Step 5 - Stemming: reducing the number of inflectional forms of words
corpus = tm_map(corpus, stemDocument)
corpus[[1]][1]  

# Step 6 - Create Document Term Matrix
frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.995) #remove sparse terms
tSparse_nonamed = as.data.frame(as.matrix(sparse)) #convert into data frame

tSparse <- tSparse_nonamed
colnames(tSparse) = make.names(colnames(tSparse_nonamed)) #all the variable names R-friendly

colnames(tSparse)
head(tSparse)

#tSparse <- tSparse[, -grep("X.", colnames(tSparse), fixed = TRUE)]
#dim(tSparse)

saveRDS(tSparse, file = "cache/tSparse_links.rds")
saveRDS(df, file = "cache/dataframe_links.rds")

