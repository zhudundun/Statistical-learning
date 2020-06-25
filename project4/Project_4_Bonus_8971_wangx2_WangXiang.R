#Visual#
library(text2vec)
library(tm)

stop_words = c("a", "an", "and","are","as","am","after","about","also","any",
               "by","be","but","been","because",
               "can","could",
               "do",
               "even",
               "for", "from","film",
               "get",
               "his", "her", "he", "him", "himself", "has", "have","how", "had",
               "i","is", "me", "my", "myself",  "it", "its", "in","if","into",
               "just",
               "more","much","movie","made","make",
               "not","no",
               "our", "ours","other", "ourselves", "of", "one","on","out","or","only",
               
               "people",
               "really",
               
               "she", "so","some","see","story",
               "their", "then","they", "the",  "this", "that","to","there","time","than","tnem",
               "us",
               "very",
               "we",  "was",  "were", "who","with","when","what","which","would","will","way",
               "you", "your", "yours"  )

all = read.table("data.tsv",stringsAsFactors = F,header = T)
splits = read.table("splits.csv", header = T)
train.plot=subset(all, !(new_id %in% splits[,1]))
train.plot$review.clean <- gsub('<.*?>', '', train.plot$review)

train.plot$review.clean <- removeWords(train.plot$review.clean,stop_words)
train.plot$review.clean <- tolower(gsub('[[:punct:]]', '', train.plot$review.clean))
train.plot$review.clean <- removeNumbers(train.plot$review.clean)
prep_fun = tolower
tok_fun = word_tokenizer

it_plot = itoken(train.plot$review.clean, 
                 preprocessor = prep_fun, 
                 tokenizer = tok_fun, 
                 ids = train.plot$new_id, 
                 progressbar = FALSE)
# sel_words=read.table("words.txt", header = TRUE, sep = "", dec = ".")


vocab = create_vocabulary(it_plot,  ngram = c(1L, 2L), stopwords = stop_words)

sub_vocab=prune_vocabulary(vocab, term_count_min = 50, term_count_max = 19560,
                           doc_count_min = 50,
                           doc_count_max = 11404)

vectorizer = vocab_vectorizer(sub_vocab)




# words = colnames(dtm_train1)
# id = order(abs(myp), decreasing=TRUE)[1:3000]
# 
# it_tmp = itoken(tmp,
#                 preprocessor = prep_fun, 
#                 tokenizer = tok_fun)
# 
# myvocab = create_vocabulary(it_tmp, ngram = c(1L, 2L), 
#                             stopwords = stop_words)
# 
# # remove terms not in words[id]
# tmp.term =  gsub(" ","_", tmp)
# myvocab = myvocab[myvocab$term %in% tmp.term, ]
# bigram_vectorizer = vocab_vectorizer(myvocab)
# 

dtm_plot = create_dtm(it_plot, vectorizer)

v <- colSums(as.matrix(dtm_plot),na.rm=TRUE)
d <- data.frame(word = colnames(dtm_plot),freq=v)
d <- subset(d, !(d$word %in% stop_words))


v.size = dim(dtm_plot)[2]
ytrain = train.plot$sentiment
summ = matrix(0, nrow=v.size, ncol=4)
summ[,1] = apply(dtm_plot[ytrain==1, ], 2, mean)
summ[,2] = apply(dtm_plot[ytrain==1, ], 2, var)
summ[,3] = apply(dtm_plot[ytrain==0, ], 2, mean)
summ[,4] = apply(dtm_plot[ytrain==0, ], 2, var)
n1=sum(ytrain); 
n=length(ytrain)
n0= n - n1

myp = (summ[,1] - summ[,3])/
  sqrt(summ[,2]/n1 + summ[,4]/n0)
words = colnames(dtm_plot)

id = order(abs(myp), decreasing=TRUE)[1:3000]
pos.list = words[id[myp[id]>0]]
neg.list = words[id[myp[id]<0]]
pos.list[1:50]
neg.list[1:50]


d.pos <- subset(d, d$word %in% pos.list)
d.neg <- subset(d, d$word %in% neg.list)

# library(dplyr)
# d=d %>% arrange(desc(freq))
library(wordcloud)
wordcloud(words = d.pos$word, freq = d.pos$freq, min.freq = 300,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = d.neg$word, freq = d.neg$freq, min.freq = 300,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#relationship plot#
tcm = create_tcm(it_plot, vectorizer)
tcm.mat = as.matrix(tcm) 
adj.mat = tcm.mat + t(tcm.mat)
z = order(colSums(adj.mat), decreasing = T)
adj.mat = adj.mat[z,z]
adj = adj.mat[200:230,200:230]
#genera terms in the middle, more specific description on th outside#
library(igraph)
cog = graph.adjacency(adj, mode = 'undirected')
cog =  simplify(cog)
cog = delete.vertices(cog, V(cog)[ degree(cog) == 0 ])
plot(cog)





