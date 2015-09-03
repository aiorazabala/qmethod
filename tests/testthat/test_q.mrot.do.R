# Test manual rotation function
context(desc = "Manual rotation function")

data("lipset")
results.unrotated <- qmethod(dataset = lipset[[1]], nfactors = 4, rotation = "none", forced = TRUE, cor.method = "pearson", reorder = FALSE, quietly = TRUE)
results.varimax <- qmethod(dataset = lipset[[1]], nfactors = 4, rotation = "varimax", forced = TRUE, cor.method = "pearson", reorder = FALSE, quietly = TRUE)
# using 4 factors here for a change.
# also, do not use 3 nfactors, because that requires flipping
cor.mat <- cor(x = lipset[[1]], method = "pearson")
rot.mat.varimax <- varimax(x = as.matrix(results.unrotated$loa))$rotmat

test_that(
  desc = "returned rotation matrix is correct for varimax",
  code = {
    expect_equivalent(object = results.varimax$brief$rotmat, expected = rot.mat.varimax)
  }
)

test_that(
  desc = "varimax rotation loadings are the same as unrotated loadings matrix multiplied by varimax rotation",
  code = {
    loa <- as.matrix(results.unrotated$loa) %*% rot.mat.varimax
    expect_equivalent(object = abs(as.matrix(results.varimax$loa)), expected = abs(loa))
    #must be abs because there might be flipping https://github.com/aiorazabala/qmethod/issues/268
  }
)

test_that(
  desc = "results object created from a varimax rotation matrix via q.mrot.do is the same as original varimax results object",
  code = {
    results.mrot.varimax <- q.mrot.do(results = results.unrotated, rot.mat = rot.mat.varimax, quietly = TRUE)
    expect_equivalent(
      object = results.mrot.varimax[names(results.mrot.varimax) != "brief"],
      expected = results.varimax[names(results.varimax) != "brief"],
      info = "all elements except brief"
    )  # this only excludes brief
    expect_equivalent(
      object = results.mrot.varimax$brief[!names(results.mrot.varimax$brief) %in% c("date", "info", "rotation")],
      expected = results.varimax$brief[!names(results.varimax$brief) %in% c("date", "info", "rotation")],
      info = "all relevant elements from brief"
    )
    expect_equal(
      object = results.mrot.varimax$brief$rotation,
      expected = "by-hand",
      info = "brief$rotation correct?"
    )
  }
)
