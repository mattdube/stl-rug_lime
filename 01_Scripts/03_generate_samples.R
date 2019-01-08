# Getting some random values to use here

set.seed(seed = 14412)

sample_values <- test_x[sample(1:nrow(test_x),200, replace = FALSE),]

# Create a sample without replacement (i.e. take the ball out and don't put it back in)
sample1 <- sample_values[sample(1:nrow(sample_values), 1, replace = FALSE),]

# Remove the sampled items from the vector of values
sample_values <- sample_values[!(sample_values %in% sample1)]

# Another sample, and another removal
sample2 <- sample_values[sample(1:nrow(sample_values), 1, replace = FALSE),]
sample_values <- sample_values[!(sample_values %in% sample2)]

# Another sample, and another removal
sample3 <- sample_values[sample(1:nrow(sample_values), 1, replace = FALSE),]
sample_values <- sample_values[!(sample_values %in% sample3)]

# Another sample, and another removal
sample4 <- sample_values[sample(1:nrow(sample_values), 1, replace = FALSE),]
sample_values <- sample_values[!(sample_values %in% sample4)]
