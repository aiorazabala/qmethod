# tests for the wrapper function

context(desc = "factor extraction: principal components")

test_that(
  desc = "loadings are the same as psych::principal",
  code = {
    cor.mat <- cor(x = lipset[[1]], method = "pearson")
    princ.pca.loadings <- principal(r = cor.mat, nfactors = 4, rotate = "none")
    qmethod.results <- qmethod(dataset = lipset[[1]], nfactors = 4, rotation = "none")
    expect_equivalent(object = as.matrix(qmethod.results$loa), expected = unclass(princ.pca.loadings$loadings))
  }
)


context(desc = "automatic rotation procedures: varimax")

test_that(
  desc = "loadings are correctly rotated",
  code = {
    cor.mat <- cor(x = lipset[[1]], method = "pearson")
    principal.varimax <- principal(r = cor.mat, nfactors = 3, rotate = "varimax")
    qmethod.varimax <- qmethod(dataset = lipset[[1]], nfactors = 3, rotation = "varimax")
    expect_equivalent(object = as.matrix(qmethod.varimax$loa), expected = unclass(principal.varimax$loadings))
  }
)
