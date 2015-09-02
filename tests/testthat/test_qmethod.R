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

# notice that this is pretty much a stop-gap measure; preferable would be testing against validated data as per https://github.com/aiorazabala/qmethod/issues/262

# Here come the variants under which this is tested, same as the nesting order of for loops in the below
datasets <- list("lipset" = lipset[[1]])  # add more datasets here, if you like
nfactors <- c(2:5)  # more seems dicey for lipset, 1: https://github.com/aiorazabala/qmethod/issues/270
rotations <- c("none", "varimax", "quartimax")  # only test the orthogonal ones because https://github.com/aiorazabala/qmethod/issues/95
cor.methods <- c("pearson", "spearman", "kendall")  # those are currently all supported

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
            info.msg <- paste(
              paste("dataset =", d),
              paste("nfactor =", nf),
              paste("rotation =", r),
              paste("cor.method =", c),
              sep = ", "
            )
            test_that(
              desc = "datasets are the same",
              code = {
                expect_equal(
                  object = new$dataset,
                  expected = baseline$dataset
                )
              }
            )
            test_that(
              desc = "brief is the same",
              code = {
                expect_equal(
                  object = new$brief$nstat,
                  expected = baseline$brief$nstat
                )
                expect_equal(
                  object = new$brief$nqsorts,
                  expected = baseline$brief$nqsorts
                )
                expect_equal(
                  object = new$brief$distro,
                  expected = baseline$brief$distro
                )
                expect_equal(
                  object = new$brief$rotation,
                  expected = baseline$brief$rotation
                )
                expect_equal(
                  object = new$brief$cor.method,
                  expected = baseline$brief$cor.method
                )
                expect_equal(
                  object = new$brief$flagging,
                  expected = baseline$brief$flagging
                )
              }
            )
            test_that(
              desc = "loadings are the same",
              code = {
                expect_equal(
                  object = new$loa,
                  expected = baseline$loa,
                  info = info.msg
                )
              }
            )
            test_that(
              desc = "flags are the same",
              code = {
                expect_equal(
                  object = new$flagged,
                  expected = baseline$flagged,
                  info = info.msg
                )
              }
            )
            test_that(
              desc = "zsc are the same",
              code = {
                expect_equal(
                  object = new$zsc,
                  expected = baseline$zsc,
                  info = info.msg
                )
              }
            )
            test_that(
              desc = "zsc_n are the same",
              code = {
                expect_equal(
                  object = new$zsc_n,
                  expected = baseline$zsc_n,
                  info = info.msg
                )
              }
            )
            test_that(
              desc = "f_char are the same",
              code = {
                expect_equal(
                  object = new$f_char$characteristics,
                  expected = baseline$f_char$characteristics,
                  info = info.msg
                )
                expect_equal(
                  object = new$f_char$cor_zsc,
                  expected = baseline$f_char$cor_zsc,
                  info = info.msg
                )
                expect_equal(
                  object = new$f_char$sd_dif,
                  expected = baseline$f_char$sd_dif,
                  info = info.msg
                )
              }
            )
            test_that(
              desc = "qdc are the same",
              code = {
                expect_equal(
                  object = new$qdc,
                  expected = baseline$qdc,
                  info = info.msg
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
