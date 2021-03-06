# ---------
# FUNCTIONS
# ---------

# prints a single line cleanly
print_clean <- function(msg){
  cat(sprintf("%s\n", msg))
}

# returns the current time in seconds, rounded to 4 digits
# If a start time is given, returns the elapsed time
my_timer <- function(start){
  if (missing(start))
    start <- 0
  
  round(as.numeric(Sys.time()) - start, 4)
}

# code for interpolating missing data (from a HackerRank exam)
calcMissing <- function(readings) {
  # Write your code here
  results <- matrix(unlist(strsplit(readings, split="\t")), nrow = 2)
  results <- t(results)
  colnames(results) <- c("Date", "Level")
  results <- data.frame(results, stringsAsFactors=FALSE)
  results[,1] <- as.numeric(as.POSIXct(results[,1],format="%m/%d/%Y %H:%M:%S"))
  n <- nrow(results)
  
  bad_indices <- which(results[,2] %in% sprintf("Missing_%s", 1:20))
  good_indices <- setdiff(1:n, bad_indices)
  good_res <- results[good_indices, ]
  hmm <- rep(0, 20)
  for (i in 1:20)
  {
    if (length(good_indices) < 2)
    {
      hmm[i] <- good_res[1, 2]
    }
    else
    {
      bad_index <- bad_indices[i]
      xout <- results[bad_index, 1]
      ind <- 1
      best_dist <- abs(good_res[ind,1]-xout) + abs(good_res[ind+1,1]-xout)
      
      if (length(good_indices) > 2)
      {
        for (j in 2:(length(good_indices)-1))
        {
          new_dist <- abs(good_res[j,1]-xout) + abs(good_res[j+1,1]-xout)
          
          if (new_dist < best_dist)
          {
            ind <- j
            best_dist <- new_dist
          }
        }
      }        
      
      x1 <- good_res[ind, 1]
      x2 <- good_res[ind+1, 1]
      y1 <- as.numeric(good_res[ind, 2])
      y2 <- as.numeric(good_res[ind+1, 2])
      hmm[i] <- (y1-y2)/(x1-x2)*(xout-x2)+y2
    }
  }
  
  cat(paste(hmm, collapse="\n"))
}

# code for a very fast GCF
fast_gcf <- function(a,b){
  while(a) {
    temp <- b %% a
    b <- a
    a <- temp
  }
  
  abs(b)
}

# code for a very fast LCM
fast_lcm <- function(a,b){
  abs(a/fast_gcf(a,b) * b)
}

# creates egyptian fractions: finite sums of unit fractions
egyptianFrac <- function(p,q)
{
  quot <- ceiling(q/p)
  rev_rem <- quot*p - q
  
  if (rev_rem == 0)
    return(sprintf("1/%s", quot))
  
  return(sprintf("1/%s + %s", quot, breakdown(rev_rem, quot*q)))
}

# Backtracking Solution - suppose you have 2n people and need each person to meet with
# another person for 2n-1 weeks such that everyone eventually meets everyone else.
backTrackSolve <- function(mat, rest, matrix_list = list(), i = 1, j = 2){
  if (length(rest) < 1)
    return(c(matrix_list, list(mat)))
  
  n <- nrow(mat)
  
  while (mat[i,j] != 0)
  {
    i <- ifelse(j == n, i+1, i)
    j <- ifelse(j == n, i+1, j+1)
  }
  
  i2 <- ifelse(j == n, i+1, i)
  j2 <- ifelse(j == n, i+2, j+1)
  indices <- which(!duplicated(rest) & !(rest %in% mat[i,]) & !(rest %in% mat[,j]))
  
  for (k in indices)
  {
    mat[i,j] <- rest[k]
    mat[j,i] <- rest[k]
    matrix_list <- backTrackSolve(mat, rest[-k], matrix_list, i2, j2)
    mat[i,j] <- 0
    mat[j,i] <- 0
  }
  
  matrix_list
}

# --------
# EXAMPLES
# --------

# GCF / LCM
answers <- vector(mode="list", length=100)

for (i in 1:100)
{
  a <- sample(1:200, 1)
  b <- sample(1:100, 1)
  
  while (fast_gcf(a,b) < 2)
  {
    a <- sample(1:200, 1)
    b <- sample(1:100, 1)
  }
  
  answers[[i]] <- c(a,b, fast_gcf(a,b), fast_lcm(a,b))
}

for (i in 1:100){
  print(sprintf(
    "Question %s: What is gcf(%s, %s)? What is lcm (%s, %s)?", 
    i, answers[[i]][1], answers[[i]][2], answers[[i]][1], answers[[i]][2]))
}

for (i in 1:100){
  print(sprintf(
    "Answer to Question %s: %s, %s", 
    i, answers[[i]][3], answers[[i]][4]))
}

# Backtracking Example
num_people <- 8
mat <- matrix(0, nrow=num_people, ncol=num_people)

for (i in 1:(num_people-1))
{
  mat[1,i+1] <- i
  mat[i+1,1] <- i
}
remaining <- rep(1:(num_people-1), (num_people-2)/2)

start <- my_timer()
result <- backTrackSolve(mat, remaining)
print(my_timer(start))

# Find the last two nontrailing zeros of 90!
n = 1
for (i in 1:90)
{
  n = i * n
  while (!(n %% 10))
    n = n / 10
  n = n %% 100
}
n

# --------------------
# FROM OTHER LANGUAGES
# --------------------

# MySQL
# SELECT 
# country_name,
# city_name,
# COUNT(city_id) AS 'my_count'
# FROM 
# customer 
# LEFT JOIN city ON customer.city_id = city.id 
# LEFT JOIN country ON city.country_id = country.id
# GROUP BY
# city_id
# HAVING
# my_count > (SELECT COUNT(*) FROM customer) / 
#   (SELECT COUNT(DISTINCT city_id) FROM customer)
# ORDER BY
# country_name ASC
# ;

# C++
# int segment(int x, vector<int> space) {
#   int n = space.size();
#   
#   for (int i = 0; i < x; ++i)
#   {
#     for (int j = i-1; j > -1; --j)
#     {
#       if (space[j] <= space[i])
#         break;
#       space[j] = space[i]; 
#     }
#   }
#   
#   for (int i = x; i < n; ++i)
#   {
#     for (int j = i-1; j > i-x; --j)
#     {
#       if (space[j] <= space[i])
#         break;
#       space[j] = space[i]; 
#     }
#   }
#   
#   int max = space[0];
#   
#   for (int i = 1; i < n-x+1; ++i)
#     if (space[i] > max)
#       max = space[i];
#   
#   return max;
# }

eq_ans <- vector(mode="list", length=100)

for (i in 1:100)
{
  a <- sample(-100:100, 1)
  b <- sample(-1000:1000, 1)
  c <- sample(-100:100, 1)
  d <- sample(-1000:1000, 1)
  
  num <- d-b
  den <- a-c
  gcf <- fast_gcf(num, den)
  sign <- "-"
  if (num/den > 0)
    sign <- ""
  num <- abs(num / gcf)
  den <- abs(den / gcf)
  
  eq_ans[[i]] <- c(a, b, c, d, num, den, sign)
}

for (i in 1:100)
{
  ans <- eq_ans[[i]]
  print_clean(sprintf("%s: (%s)x + (%s) = (%s)x + (%s)", i, 
                      ans[1], ans[2], ans[3], ans[4]))
}
  

for (i in 1:100)
{
  ans <- eq_ans[[i]]
  print_clean(sprintf("%s: x = %s%s/%s", i, ans[7], ans[5], ans[6]))
}