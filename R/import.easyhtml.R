import.easyhtmlq <- function(filename, ...) {
  if (substr(filename, nchar(filename)-4, nchar(filename)) != ".json") stop("Q method input: the file name provided is not a '.json' filename")
  
dataset <- list()

# read the file
a <- fromJSON(file = filename, ...)

# Q-SORTS
# obtain number of statements and of Q-sorts
nqsorts <- length(a)
nstat <- length(strsplit(a[[1]][["sort"]], "|", fixed=T)[[1]])

# extract Q-sorts
db.temp <- data.frame(NA, ncol=nstat, nrow=nqsorts)
for (i in 1:nqsorts) {
  db.temp[i,c(1:(nstat))] <- as.integer(strsplit(a[[i]][["sort"]], "|", fixed=T)[[1]])
}
colnames(db.temp) <- paste0("s", 1:nstat)
rownames(db.temp) <- names(a)

# extract the data
dataset[[1]] <- t(db.temp)

# ADDITIONAL INFORMATION
# identify all possible column names (not all Q-sorts fill all)
add.vars <- sort(unique(unlist(lapply(a, names))))
add.vars <- add.vars[add.vars != "sort"]

# create data frame 
db.xtr <- data.frame(matrix(NA, ncol=length(add.vars), nrow=length(a)))
colnames(db.xtr) <- add.vars 
rownames(db.xtr) <- names(a) 
# fill data frame
for (i in add.vars) {
  for (n in names(a)) try(db.xtr[n, i] <- a[[n]][[i]], silent=TRUE)
}
# convert time variable
db.xtr$datetime <- strptime(db.xtr$datetime,"%Y-%m-%d %H:%M:%S")

dataset[[2]] <- db.xtr
names(dataset) <- c("qsorts", "suppl.data")



cat("-------------------------------------------------\nThe dataset contains", nqsorts, "Q-sorts and", nstat, "statements\n-------------------------------------------------\nIf this does not correspond to your data,\nplease report to the package maintainer.\n")

return(dataset)
}