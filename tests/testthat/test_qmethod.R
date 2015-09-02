# factor extraction pca ============
context(desc = "factor extraction: principal components")

test_that(
  desc = "loadings are the same as psych::principal",
  code = {
    cor.mat <- cor(x = lipset[[1]], method = "pearson")
    princ.pca.loadings <- principal(r = cor.mat, nfactors = 4, rotate = "none")
    qmethod.results <- qmethod(dataset = lipset[[1]], nfactors = 4, rotation = "none", quietly = TRUE)
    expect_equivalent(object = as.matrix(qmethod.results$loa), expected = unclass(princ.pca.loadings$loadings))
  }
)

# automatic rotation ===================
context(desc = "automatic rotation procedures: varimax")

test_that(
  desc = "loadings are correctly rotated",
  code = {
    cor.mat <- cor(x = lipset[[1]], method = "pearson")
    principal.varimax <- principal(r = cor.mat, nfactors = 3, rotate = "varimax")
    qmethod.varimax <- qmethod(dataset = lipset[[1]], nfactors = 3, rotation = "varimax", quietly = TRUE)
    expect_equivalent(object = as.matrix(qmethod.varimax$loa), expected = unclass(principal.varimax$loadings))
  }
)


# test against old versions ==========================
context(desc = "test against old (validated) version of qmethod")

# Here come the variants under which this is tested, same as the nesting order of for loops in the below
datasets <- list("lipset" = lipset[[1]])  # add more datasets here, if you like
nfactors <- c(2:5)  # more seems dicey for lipset
rotations <- c("none", "varimax")  # only test orthogonal ones
# equimax appears not to work!!
cor.methods <- c("pearson", "spearman", "kendall")  # those are currently all

# Loop over combinations
old <- NULL
for (v in c("old", "new")) {  # loop over versions

  # Set up baseline ============================================================
  if (v == "old") {
    suppressMessages(dev_mode(on = TRUE))  # this makes sure old qmethod is only stored in a special dir
    library(qmethod)  # load some qmethod
    detach(name = "package:qmethod", unload = TRUE, force = TRUE, character.only = TRUE)  # detach it
    install_version(package = "qmethod", version = "1.2.4", repos = "http://cran.us.r-project.org", quiet = TRUE)  # this is before @maxheld83 joined development
    library(qmethod)
    test_that(
      desc = "old version 1.2.4 can be installed",
      code = {
        expect_equivalent(object = unlist(unclass(packageVersion(pkg = "qmethod"))), expected = c(1, 2, 4))
      }
    )
  }

  if (v == "new") {
    detach(name = "package:qmethod", unload = TRUE, force = TRUE, character.only = TRUE)  # detach it
    suppressMessages(dev_mode(on = FALSE))  # ignore special dir for old qmethod (see above)
    library(qmethod)
    test_that(
      desc = "new version is unequal to old version",
      code = {
        expect_true(object = any(unlist(packageVersion(pkg = "qmethod")) != c(1, 2, 4)))
      }
    )
  }

  for (d in names(datasets)) {
    for (nf in nfactors) {
      for (r in rotations) {
        for (c in cor.methods) {
          round.result <- suppressWarnings(qmethod(dataset = datasets[[d]], nfactors = nf, rotation = r, cor.method = c, quietly = TRUE))
          if (v == "old") {
            old[[d]][[as.character(nf)]][[r]][[c]] <- round.result
          }
          if (v == "new") {
            new <- round.result
            baseline <- old[[d]][[as.character(nf)]][[r]][[c]]
            test_that(
              desc = "loadings are the same",
              code = {
                expect_equal(
                  object = new$loa,
                  expected = baseline$loa,
                  info = paste(
                    paste("dataset =", d),
                    paste("nfactor =", nf),
                    paste("rotation =", r),
                    paste("cor.method =", c),
                    sep = ", "
                  )
                )
              }
            )
          }
        }
      }
    }
  }
}


#  Q Method output ===============================================================
context(desc = "qmethod output")

test_that(
  desc = "qmethod cats info under defaults",
  code = {
    expect_output(object = test <- qmethod(dataset = lipset[[1]], nfactors = 3), regexp = "Q-method analysis.")
    rm(test)
  }
)
test_that(
  desc = "qmethod stays quiet when told so",
  code = {
    expect_output(object = test <- qmethod(dataset = lipset[[1]], nfactors = 3), regexp = "")
  }
)
