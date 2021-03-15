## Contributing


**Contributions** to developing the package are **most welcome**.
Great to have you here. Some friendly suggestions for contributing:


## Team Members

[`qmethod`](https://github.com/aiorazabala/qmethod) was created by [Aiora Zabala](http://www.landecon.cam.ac.uk/directory/aiora-zabala), with contributions by [Max Held](http://www.maxheld.de) and [Frans Hermans](www.maxheld.de).


## Learn More & Join the Conversation

Communication on developing this package happens right here on **our [GitHub issues](https://github.com/aiorazabala/qmethod/issues)** :octocat: and by email.
If you have any suggestion, you can raise it here as a new issue, send us an email, or both – that way, we can keep everyone in the loop and have a public record.
If you can, *browse the existing issues first* to avoid duplication.

To learn more about the current state of development:

- Browse the [existing issues](https://github.com/aiorazabala/qmethod/issues) :eyes: and join the conversation by commenting on or  [adding new issues](https://github.com/aiorazabala/qmethod/issues/new) :speech_balloon:.
- Read the [package reference manual](http://cran.r-project.org/web/packages/qmethod/qmethod.pdf) :books: to familiarise yourself with how `qmethod` works (can also be found under `/ man/`).
- Read the [`qmethod` wiki](https://github.com/aiorazabala/qmethod/wiki) for some additional documentation :notebook:.


## Beta-test New Features

`qmethod` is under development, and we're keen to have more beta-test new features.

[Here](https://github.com/aiorazabala/qmethod/wiki/Beta-testing) is how.


## Add New Features

There's so much more to do, and we're excited for new additions.

We're roughly following the [GitHub Flow](https://guides.github.com/introduction/flow/) development model:

1. **Read and log [issues](https://github.com/aiorazabala/qmethod/issues)**, so that we know what everyone is up to and interested in.
  - Create an issue :memo: *before* you start to work on *anything* so we can avoid duplicate efforts.
  -  Assign `backlog` milestone if it's not happening anytime soon.
  -  Assign oneself as an assignee if one is actively working on it (so as to avoid duplicate efforts).
2. Collaborators: **Create forks** :fork_and_knife:, such that other collaborators can work (and mess up) in their own sandbox.
  - *Remember to pull in upstream changes* from `aiorazabala/qmethod/master` frequently, so as to stay up to date and minimize merge conflicts ([here's how](https://help.github.com/articles/syncing-a-fork/)).
3. **Create "feature-branches"**
  - Create a branch off of (forked) masters for some feature to be added, say `centroid-extraction` ([still missing](https://github.com/aiorazabala/qmethod/issues/15) :wink:).
  - If you can, keep feature-branches focused on relatively few, *well*, features, and keep them separate from bugfixes.
  - Creating "feature-branches" max seem cumbersome, but it pays off with transparent pull requests (see below).
4. **Put up a pull-request** ([here's how](https://help.github.com/articles/using-pull-requests/))
  - *create the feature-branch early*, and name it with the prefix `WIP` (for work-in-progress) so that other people can provide early feedback and know what's being worked on.
  - Remove the `WIP`-prefix once the work is done.
    At this stage, a feature should be *fully documented* and *tested* (passing `R CMD check` on all platforms).
    Notice that Travis CI included here on GitHub only tests on Linux machines; @maxheld83 can test on OS X and [`win-builder`](http://win-builder.r-project.org/) on Windows, if you do not have these platforms yourself.
  - @aiorazabala as the creator of `qmethod` will then review the changes and accept the pull request if possible. 
  By accepting, the pull request, say `centroid-extraction`, is then merged into @aiorazabala's `master`
5. **Shipping** :ship: : Periodically, whenever significant work has been done, @aiorazabala drafts a release (as per [#121](https://github.com/aiorazabala/qmethod/issues/121)) from `master`, essentially just marking some point in the history of the package as `x.x.x`, and sends it off to CRAN.


## Testing

Testing is an important part of quality software development, especially for scientific software, where users rely on the accuracy and reproducibility of results.

Most essentially, any changes **should pass the tests for submitting packages to CRAN**. This involves running the following in your command line:
`R CMD check --as-cran qmethod_1.4.0.tar.gz`
and running the package through [WinBuilder](http://win-builder.r-project.org/), or
either `R-devel CMD check --as-cran qmethod_1.4.0.tar.gz`
...and getting **no errors** (or solving whatever may arise).
If in the [CRAN package checks for qmethod](https://cran.r-project.org/web/checks/check_results_qmethod.html), your changes induce any change in the 'OK' Status, the maintainer(s) will poke you to fix it.


To learn more about testing, consider Hadley Wickham's [`testthat` package](https://cran.r-project.org/package=testthat) (which is what we're using here) and his book chapter [on testing](http://r-pkgs.had.co.nz/tests.html).

It is advised that new functions and changes *come with* appropriate tests:

- For internal **consistency**. For example, `q.mrot.choose`, the interactive rotation function, should always produce a rotation matrix of `rank == nfactors` (pseudo-code; the number of factors should be the same as the rank of the rotation matrix).
- Where applicable, new functions and changes should test against **old versions** (`>= 1.2.0`) *known* to be validated by Aiora against `PQMethod` (see [Zabala 2014](http://journal.r-project.org/archive/2014-2/zabala.pdf)). 
- Whenever possible, new functions and changes should test against **(published) results** *known* to be true, using publicy available data.

In addition to such tests for new functions, missing tests for old functions [would also be very welcome].

## Help pages

We give a lot of importance to the usability of the package. Whether you've developed a fantastic function won't matter, if it is not easy to use. For this, we believe that **good documentation is essential**. Please make sure that all new functions are well documented in the help pages. Some general points to observe when writing the help:
- Write full sentences (with noun and verb).
- Be consistent in the use of terms, e.g. do not write Q-Methodology and Q methodology in the same page, or CSV and *.csv, but stick to a single form. If in doubt, check the conventions used in the existing help pages, e.g. those for `qmethod()`
- Think that the user might not necessarily be familiar with all the terms: define everything necessary or link to pages that give more details.
- Don't be afraid of repeating: better to be in excess than to miss an explanation.


## Git(Hub)

If you're unfamiliar with Git(Hub), it's probably worth spending some time learning these tools and conventions first.

Here are some great places to start:

- [GitHub help](https://help.github.com), especially [using pull requests](https://help.github.com/articles/using-pull-requests/)
- [GitHub Guides](https://help.github.com/articles/using-pull-requests/), especially [GitHub Flow](https://guides.github.com/introduction/flow) and [Mastering Issues](https://guides.github.com/features/issues)
- [Official Git Documentation](https://git-scm.com/doc)
