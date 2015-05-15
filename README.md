[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/qmethod)](http://cran.r-project.org/web/packages/qmethod)

qmethod
=======
This R package performs the analysis of Q methodology data. See a [visual demo here](https://azabala.shinyapps.io/shinyapps/qmethod.Rmd), and [more details in the wiki](https://github.com/aiorazabala/qmethod/wiki).

You can install the stable version from [CRAN](http://cran.r-project.org/web/packages/qmethod/index.html):

```{r}
install.packages('qmethod')
```

[Q](http://qmethod.org/about) is a methodology to study the distinct perspectives existing within a group on a topic of interest. It is used across social, health, and environmental studies.

**Contributions** to developing the package are **most welcome**, either in the form of **new function** development or as **suggestions**. See the current projects and suggestions in the [list of issues](https://github.com/aiorazabala/qmethod/issues), an please report any bugs or comments for improvement by [adding new ones](https://github.com/aiorazabala/qmethod/issues/new).

Some friendly suggestions for contributing:

1. Log [issues](https://github.com/aiorazabala/qmethod/issues), so that we know what everyone is up to and interested in
  -  assign `backlog` milestone if it's not happening anytime soon
  -  assign oneself as an assignee if one is actively working on it (so as to avoid duplicate efforts)
2. Collaborators: Create forks
  - such that other collaborators can work (and mess up) in their own sandbox
  - but remember to pull in upstream changes from `aiorazabala/qmethod/master` frequently, so as to stay up to date and avoid merge conflicts.
3. Create "feature-branches", keep work on features separate from bugfixes etc.
  - create a branch off of (forked) masters for some feature to be added, say `rotation-visualization` (hehe, I wish)
  - creating "feature-branches" can seem cumbersome, but it pays off with transparent pull requests (see below)
4. Once work is done, put up a pull-request (see #36), which @aiorazabala as the maintainer and creator then accepts after review.
  - **Crucially** pull requests should be put up only once `R CMD check` passes.
  - by accepting, the pull request, some `feature-branch` is then merged into @aiorazabala's `master`
5. Periodically, whenever significant work has been done, @aiorazabala drafts a release from `master`, essentially just marking some point in the history of the package as `x.x.x`, and sends it off to CRAN.

Or, you know, just hack away.
Please, though, be kind enough to squash your own `R CMD check` issues as they arise.
It's very hard to fix very many of them once they pile up, and CRAN must be obeyed :smirk:


