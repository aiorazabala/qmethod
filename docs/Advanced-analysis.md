##Advanced analysis

The function `qmethod()` runs the full analysis with the default loadings and with automatic flagging.

To run more advanced analysis and assess the results at each step, you can run the analysis function by function.

#### Manual manipulation of Q-sort loadings and/or manual flagging
The following is sample code to run the analysis function by function. To adapt it to your data, replace `lipset[[1]]` with your dataset, and adjust the value of `factors`.
##### 1. Load your data and the package
```r
data(lipset) # Sample data
library(qmethod)
```
##### 2. Calculate (and manipulate) Q-sort factor loadings
```r
factors <- 3 # The number of factors to extract 
# and rotate
# The following runs Q analysis and extracts the default
# factor loadings only:
mloa <- qmethod(lipset[[1]], 
                nfactors   = factors,
                extraction = "PCA", # Also "centroid"
                rotation   = "varimax", # Also "none"
                forced     = TRUE)$loa
mloa # Inspect the loadings
# Invert the loadings if necessary:
mloa[1] <- -mloa[1] # This example inverts the sign of 
                    # the first factor, replace 1 with 
                    # the number of the factor to invert
```
##### 3. Manual flagging (either a. or b.)
###### 3.a. Calculate automatic flagging and then modify any of the flags
Note: for an easier inspection of flags, see how to print the loadings next to the flags [in Step 8 of the Cookbook](https://github.com/aiorazabala/qmethod/wiki/Cookbook#8-explore-the-factor-loadings).
```r
# Automatic flagging:
mflagged <- qflag(loa = mloa, nstat = 33) 
# Inspect the automatic flags:
mflagged 
# Modify flags:
mflagged["FR9", 3] <- FALSE # This example eliminates
                            # the flag for Q-sort FR9 
                            # in factor 3
```

###### 3.b. Generate a completely new set of flags
```r
# Create a vector of flags for each factor
# ('TRUE' for flagged Q-sorts):
flags1 <- c(FALSE, FALSE,  TRUE,  TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE)
flags2 <- c( TRUE,  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, FALSE)
flags3 <- c(FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,  TRUE, FALSE, FALSE)
# Bind the vectors together:
mflagged <- data.frame(flags1, flags2, flags3)
# Set the Q-sort names (not necessary, but useful):
rownames(mflagged) <- rownames(mloa)
```
##### 4. Run Q analysis with the modified loadings and/or flags
```r
results <- qzscores(lipset[[1]], 
                    nfactors = factors, 
                    forced   = TRUE,
                    flagged  = mflagged, # Modified flags
                    loa      = mloa) # Modified loadings
results # See your results
```

##### 5. Calculate distinguishing and consensus statements
```r
results[[8]] <- qdc(lipset[[1]], 
                    nfactors = factors, 
                    zsc      = results[[5]], 
                    sed      = as.data.frame(results[[7]][[3]]))
names(results)[8] <- "qdc"
results # See your results
```
