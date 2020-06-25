library(pROC)
library(text2vec)
library(glmnet)

all = read.table("data.tsv",stringsAsFactors = F,header = T)
splits = read.table("splits.csv", header = T)
s=3
train1=subset(all, !(new_id %in% splits[,1]))
test1=subset(all, (new_id %in% splits[,1]))

train=subset(all, !(new_id %in% splits[,s]))
test=subset(all, (new_id %in% splits[,s]))

# remove HTML tags
train1$review.clean <- gsub('<.*?>', '', train1$review)
test1$review.clean <- gsub('<.*?>', '', test1$review)

train$review.clean <- gsub('<.*?>', '', train$review)
test$review.clean <- gsub('<.*?>', '', test$review)

stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "of", "one", "for", 
               "the", "us", "this")

prep_fun = tolower
tok_fun = word_tokenizer



it_train1 = itoken(train1$review.clean, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun, 
                   ids = train1$new_id, 
                   progressbar = FALSE)

it_train = itoken(train$review.clean, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train$new_id, 
                  progressbar = FALSE)

it_test1 = itoken(test1$review.clean, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = test1$new_id, 
                  progressbar = FALSE)

it_test = itoken(test$review.clean, 
                 preprocessor = prep_fun, 
                 tokenizer = tok_fun, 
                 ids = test$new_id, 
                 progressbar = FALSE)

vocab = create_vocabulary(it_train1,  ngram = c(1L, 2L), stopwords = stop_words)

sub_vocab=prune_vocabulary(vocab, term_count_min = 50, term_count_max = 19560,
                           doc_count_min = 50,
                           doc_count_max = 11404)

vectorizer = vocab_vectorizer(sub_vocab)

dtm_train1 = create_dtm(it_train1, vectorizer)

v.size = dim(dtm_train1)[2]
ytrain1 = train1$sentiment
summ = matrix(0, nrow=v.size, ncol=4)
summ[,1] = apply(dtm_train1[ytrain1==1, ], 2, mean)
summ[,2] = apply(dtm_train1[ytrain1==1, ], 2, var)
summ[,3] = apply(dtm_train1[ytrain1==0, ], 2, mean)
summ[,4] = apply(dtm_train1[ytrain1==0, ], 2, var)
n1=sum(ytrain1); 
n=length(ytrain1)
n0= n - n1

myp = (summ[,1] - summ[,3])/
  sqrt(summ[,2]/n1 + summ[,4]/n0)
words = colnames(dtm_train1)

id = order(abs(myp), decreasing=TRUE)[1:3000]

set.seed(8971)

NFOLDS = 10

tmp = words[id]
   
write.table(tmp, "./myVocab.txt", sep=",",row.names = FALSE)
sel_words=read.table("myVocab.txt", header = TRUE, sep = "", dec = ".")

it_tmp = itoken(tmp,
                preprocessor = prep_fun, 
                tokenizer = tok_fun)

myvocab = create_vocabulary(it_tmp, ngram = c(1L, 2L), 
                            stopwords = stop_words)

# remove terms not in words[id]
tmp.term =  gsub(" ","_", tmp)
myvocab = myvocab[myvocab$term %in% tmp.term, ]
bigram_vectorizer = vocab_vectorizer(myvocab)

dtm_train = create_dtm(it_train, bigram_vectorizer)
dtm_test = create_dtm(it_test, bigram_vectorizer)

mycv = cv.glmnet(x=dtm_train, y=train$sentiment, 
                 family='binomial',type.measure = "auc", 
                 nfolds = NFOLDS, alpha=0)

myfit = glmnet(x=dtm_train, y=train$sentiment, 
               lambda = mycv$lambda.min, family='binomial', alpha=0)
logit_pred = predict(myfit, dtm_test, type = "response")
roc_obj = roc(test$sentiment, as.vector(logit_pred))
pred=cbind(test$new_id,logit_pred)
colnames(pred) <- c("new_id","prob")
write.table(pred, "./mysubmission.txt", sep=",",row.names = FALSE)
