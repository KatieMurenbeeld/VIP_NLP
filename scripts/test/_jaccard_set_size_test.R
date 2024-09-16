library(ggplot2)



# generate random sets of number and vary the size
lst10_1 = sample(1 : 10, size = 10, replace = T) 
lst10_2 = sample(1 : 10, size = 10, replace = T)
lst10_3 = sample(1 : 100, size = 10, replace = T) 
lst10_4 = sample(1 : 100, size = 10, replace = T)
lst10_5 = sample(1 : 1000, size = 10, replace = T) 
lst10_6 = sample(1 : 1000, size = 10, replace = T)

lst100_1 = sample(1 : 10, size = 100, replace = T) 
lst100_2 = sample(1 : 10, size = 100, replace = T)
lst100_3 = sample(1 : 100, size = 100, replace = T) 
lst100_4 = sample(1 : 100, size = 100, replace = T)
lst100_5 = sample(1 : 1000, size = 100, replace = T) 
lst100_6 = sample(1 : 1000, size = 100, replace = T)

lst1000_1 = sample(1 : 10, size = 1000, replace = T) 
lst1000_2 = sample(1 : 10, size = 1000, replace = T)
lst1000_3 = sample(1 : 100, size = 1000, replace = T) 
lst1000_4 = sample(1 : 100, size = 1000, replace = T)
lst1000_5 = sample(1 : 1000, size = 1000, replace = T) 
lst1000_6 = sample(1 : 1000, size = 1000, replace = T)

jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  #return (c(intersection, union, intersection/union)) 
  return (intersection/union)
}

jaccard(lst10_1, lst10_2) # sampled from 1-10 (with replacement), set size = 10
jaccard(lst10_3, lst10_4) # sampled from 1-100 (with replacement), set size = 10
jaccard(lst10_5, lst10_6) # sampled from 1-1000 (with replacement), set size = 10

jaccard(lst100_1, lst100_2) # sampled from 1-10 (with replacement), set size = 100
jaccard(lst100_3, lst100_4) # sampled from 1-100 (with replacement), set size = 100
jaccard(lst100_5, lst100_6) # sampled from 1-1000 (with replacement), set size = 100

jaccard(lst1000_1, lst1000_2) # sampled from 1-10 (with replacement), set size = 1000
jaccard(lst1000_3, lst1000_4) # sampled from 1-100 (with replacement), set size = 1000
jaccard(lst1000_5, lst1000_6) # sampled from 1-1000 (with replacement), set size = 1000

# the best score always occurs where the range of numbers to sample from = the size of the set of numbers
# not the size of the set, but the size of the sample?? what does this mean for the topics?
# we aren't randomly sampling - but we will have a huge number of tokens  

lst10000 = sample(1 : 10000, size = 10000, replace = T) 
lst10000 = sample(1 : 10000, size = 10000, replace = T)
lst100000 = sample(1 : 100000, size = 100000, replace = T) 
lst100000 = sample(1 : 100000, size = 100000, replace = T)

jaccard(lst10000, lst10000)
jaccard(lst100000, lst10000)

sample_size <- c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000)
scores <- list()

for (i in sample_size){
  #print(i)
  score <- jaccard(sample(1:i, size = i, replace = T), sample(1:i, size = i, replace = T))
  scores <- append(scores, score)
}
scores 

jaccard_df <- as.data.frame(cbind(sample_size, scores))

ggplot(data = jaccard_df, mapping = aes(x = as.numeric(sample_size), y = as.numeric(scores))) + 
  geom_point() + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(limits = c(0,1))
  theme_bw()

set_one <- c("wildlife", "bear", "grizzly", "montana", "season", "habitat", "wolves", "food", "wild", "idaho", "state")
set_two <- c("")

