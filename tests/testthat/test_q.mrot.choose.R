test_that(
  desc = "validated manual rotation against PQMethod 2.3.5",
  code = {
    skip("Interactive rotation cannot be tested programmatically.")
    # because this is an interactive function it cannot be tested programmatically
    # instead, this provides the script with which to test against the (similarly interactive) manual rotation QROT in PQMethod

    # 3 factors, no rotation ===================
    test <- qmethod(dataset = lipset[[1]], nfactors = 3, rotation = "none", quietly = TRUE, reorder = FALSE)
    test$loa  # this checks out against the on-screen loadings on PQMethod

    q.mrot.choose(results = test, plot.type = "base", plot.all = FALSE, file = "3norot.csv")

    rot.mat.3norot <- read.csv(file = "3norot.csv", header = TRUE, row.names = 1)
    rot.mat.3norot <- as.matrix(rot.mat.3norot)

    test.rot <- q.mrot.do(results = test, rot.mat = rot.mat.3norot, quietly = TRUE)
    test.rot$loa



    # 4 factors, varimax ============
    test <- qmethod(dataset = lipset[[1]], nfactors = 4, rotation = "varimax", quietly = TRUE, reorder = FALSE)
    test$loa  # checks out, except order is different
    evs <- apply(X = test$loa, MARGIN = 2, FUN = function(x) sum(x^2))  # confirm that THESE loa have not been re-ordered
    evs  # evs but they are not eigenvalue

    # reordering systematically alone does *not* work
    test$loa[, order(evs, decreasing = TRUE)]  # probably because of rounding errors

    # notice that factors are reordered as follows, first qmethod, then PQMethod
    # RC1 -> 3
    # RC2 -> 2
    # RC3 -> 1
    # RC4 -> 4

    test$loa[, c(3, 2, 1, 4)] # this is how it is in PQMethod

    q.mrot.choose(results = test, plot.type = "base", plot.all = FALSE, file = "4varimax.csv")

    rot.mat.4varimax <- read.csv(file = "4varimax.csv", header = TRUE, row.names = 1)
    rot.mat.4varimax <- as.matrix(rot.mat.4varimax)
    rot.mat.4varimax

    test.rot <- q.mrot.do(results = test, rot.mat = rot.mat.4varimax, quietly = TRUE)
    test.rot$loa[, c(3, 2, 1, 4)]  # again returned as PQMethod does it
  }
)
