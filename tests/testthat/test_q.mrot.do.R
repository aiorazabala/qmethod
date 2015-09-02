# Test manual rotation function
context(desc = "Manual rotation function")

data("lipset")
results.unrotated <- qmethod(dataset = lipset[[1]], nfactors = 3, rotation = "none", forced = TRUE, cor.method = "pearson")
results.varimax <- qmethod(dataset = lipset[[1]], nfactors = 3, rotation = "varimax", forced = TRUE, cor.method = "pearson")
cor.mat <- cor(x = lipset[[1]], method = "pearson")
rot.mat.varimax <- varimax(x = as.matrix(results.unrotated$loa))

test_that(
  desc = "returned rotation matrix is correct for varimax",
  code = {
    expect_equivalent(object = results.varimax$brief$rotmat, expected = rot.mat.varimax$rotmat)
  }
)

test_that(
  desc = "varimax rotation is the same as unrotated loadings matrix multiplied by varimax rotation",
  code = {
    loa <- as.matrix(results.unrotated$loa) %*% rot.mat.varimax$rotmat

    # must be ordered b/c psych does it, too https://github.com/aiorazabala/qmethod/issues/263
    if(ncol(loa) >1) {
      ev.rotated <- diag(t(loa) %*% loa)
      ev.order <- order(ev.rotated,decreasing=TRUE)
      loa <- loa[,ev.order]
    }
    expect_equivalent(object = abs(as.matrix(results.varimax$loa)), expected = abs(loa))
    # abs must go, this is a bug https://github.com/aiorazabala/qmethod/issues/268
  }
)

#test_that(
#  desc = "results object created from a varimax rotation matrix is the same as original varimax results object",
#  code = {
#    results.mrot.varimax <- q.mrot.do(results = results.unrotated, rot.mat = rot.mat.varimax, quietly = TRUE)
#    expect_equivalent(object = results.mrot.varimax, expected = results.varimax)
#  }
#)
