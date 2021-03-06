## Development

`qmethod` is under continued development, and fixes or additions are [released on GitHub](https://github.com/aiorazabala/qmethod/releases) and published on [CRAN](https://cran.r-project.org/web/packages/qmethod/index.html).

To join development, please consider our [guidelines](./Contribute).

You are invited to **beta-test new features**. To do so:

1. Explore the [issues](https://github.com/aiorazabala/qmethod/issues), [milestones](https://github.com/aiorazabala/qmethod/milestones) and open [pull requests](https://github.com/aiorazabala/qmethod/pulls) to find a [**feature branch**](https://guides.github.com/introduction/flow/) you are interested in.
  (Make sure to familiarize yourself with (Git)Hub first.)
2. Using the [`devtools`](https://cran.r-project.org/package=devtools) package, you can install such a feature branch or forked version *directly from GitHub*, using the appropriate `repo` (or `pull`) and `ref` (branch, commit or tag).
  (To learn how to install R, read the [cookbook](./Cookbook)).
  For example, to install Max's current fork, including additional plots, manual rotation etc from PR [97](https://github.com/aiorazabala/qmethod/pull/97) you would run:

  ```r
  install.packages("devtools")  # if you don't have it yet
  library(devtools)
  install_github(repo = "maxheld83/qmethod", ref = "master")
  library(qmethod)
  ```

3. You can now use this feature branch or forked version of `qmethod`.


Please note:

- If you have any suggestions or find any bugs, please report them on our [issue tracker](https://github.com/aiorazabala/qmethod/issues), and refer to the version (and commit) you were using, ideally with a reproducible example of the problem.
- **Do not use these beta versions for mission-critical work or publications. There may be bugs.**
- Remember to stay up to date by frequently re-running the above command.


To return to your previous, "safe" version of `qmethod` from CRAN, run:

```r
remove.packages("qmethod")
install.packages("qmethod")
library("qmethod")
```
