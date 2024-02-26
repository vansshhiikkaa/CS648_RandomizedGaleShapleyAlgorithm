library(tictoc)
library(ggplot2)
# Function to check if woman 'w' prefers man 'm1' over man 'm'
wPrefersM1OverM <- function(prefer, w, m, m1) {

  for (i in 1:N) {

    if (prefer[w, i] == m1)
      return(TRUE)

    if (prefer[w, i] == m)
      return(FALSE)
    return(FALSE)
  }
}

# Function to find stable marriage
stableMarriage <- function(prefer) {
  
  # Initialize all men and women as free
  wPartner <- rep(-1, N)
  mFree <- rep(FALSE, N)
  cnt <- 0
  
  freeman <- N
  
  # While there are free men
  while (freeman > 0) {
    # Picking the first free man (we could pick any)
    m <- which(!mFree)[1]

    # Here m is the picked free man
    while(mFree[m] == FALSE) {

      w <- prefer[m,sample(1:N,1)]
      cnt <- cnt + 1

      if (wPartner[w-N+1] == -1) {
        wPartner[w-N+1] <- m
        mFree[m] <- TRUE
        freeman <- freeman - 1
      } else {

        m1 <- wPartner[w-N+1]

        if (!wPrefersM1OverM(prefer, w, m, m1)) {
          wPartner[w-N+1] <- m
          mFree[m] <- TRUE
          mFree[m1] <- FALSE
        }
      }
    }
  }
  
  # Print the solution
 # cat("Woman   Man\n")
 # for (i in 1:N) {
 #   cat(i+N, "\t", wPartner[i], "\n")
 # }
  return(cnt)
}

N <- 1000
tic()
# Testing
prefer <- matrix(c(sapply(1:N,function(x) sample(N:(2*N-1),N,replace = FALSE)),
                   sapply(1:N,function(x) sample(0:(N-1),N,replace = FALSE))), nrow = 2*N, byrow = TRUE)
toc()
count <- numeric(length=1e3)
for(i in 1:1e3){
  count[i] <- stableMarriage(prefer)
}
plot(count)
abline(h=N*log(N),col = "red")
abline(h=N,col = "blue")

stableMarriage(prefer)


for(i in 1:1e3){
  N <- i
  prefer <- matrix(c(sapply(1:N,function(x) sample(N:(2*N-1),N,replace = FALSE)),
                     sapply(1:N,function(x) sample(0:(N-1),N,replace = FALSE))), nrow = 2*N, byrow = TRUE)
  count[i] <- stableMarriage(prefer)
}
specialcount <- count

df <- data.frame(x=specialcount,y=1:1e3)

ggplot(df, aes(y, x)) +
  geom_point() +
  geom_line(aes(y, y * log(y)), color = "red") +
  xlim(0,1000) +
  ylim(0,10000) +
  theme_bw()

