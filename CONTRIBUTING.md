
**Contributions** to developing the package are **most welcome**.
Great to have you here.

Some friendly suggestions for contributing:


## Team Members

[`qmethod`](https://github.com/aiorazabala/qmethod) was created by [Aiora Zabala](http://www.landecon.cam.ac.uk/directory/aiora-zabala), and is now maintained by her and [Max Held](http://www.maxheld.de).


## Learn More & Join the Conversation

For now, most communication on developing this package happens right here on **our [GitHub issues](https://github.com/aiorazabala/qmethod/issues)**.
If you have any suggestion, preferably raise it here as a new issue, rather than send us an email – that way, we can keep everyone in the loop and keep a public record.
If you can, *browse the existing issues first* to avoid duplication.

To learn more about the current state of development:

- Browse the [existing issues](https://github.com/aiorazabala/qmethod/issues) and join the conversation by commenting or  [adding new issues](https://github.com/aiorazabala/qmethod/issues/new).
- Read the [package reference manual](http://cran.r-project.org/web/packages/qmethod/qmethod.pdf) to familiarise yourself with how `qmethod` works (can also be found under `/ man/`).
- Read the [`qmethod` wiki](https://github.com/aiorazabala/qmethod/wiki) for some additional documentation.


## Add New Features

There's so much more to do, and we're excited for new additions.

We're roughly following the [GitHub Flow](https://guides.github.com/introduction/flow/) development model:

1. **Read and log [issues](https://github.com/aiorazabala/qmethod/issues)**, so that we know what everyone is up to and interested in.
  - *Create an issue* before you start to work on *anything* so we can avoid duplicate efforts.
  -  Assign `backlog` milestone if it's not happening anytime soon.
  -  Assign oneself as an assignee if one is actively working on it (so as to avoid duplicate efforts).
2. Collaborators: **Create forks**, such that other collaborators can work (and mess up) in their own sandbox.
  - *Remember to pull in upstream changes* from `aiorazabala/qmethod/master` frequently, so as to stay up to date and avoid merge conflicts ([here's how](https://help.github.com/articles/syncing-a-fork/)).
3. **Create "feature-branches"**
  - Create a branch off of (forked) masters for some feature to be added, say `rotation-visualization` (that's done).
  - If you can, keep feature-branches focused on relatively few, well, features, and keep them separate from bugfixes.
  - Creating "feature-branches" can seem cumbersome, but it pays off with transparent pull requests (see below).
4. **Put up a pull-request** ([here's how](https://help.github.com/articles/using-pull-requests/))
  - *create the feature-branch early*, and name it with the prefix `WIP` (for work-in-progress) so that other people can provide early feedback and know what's being worked on.
  - Remove the `WIP`-prefix once the work is done.
    At this stage, a feature should be *fully document* and *tested* (passing `R CMD check` on all platforms).
    Notice that Travis CI included here on GitHub only tests on Linux machines; @maxheld83 can test on OS X and [`win-builder`](http://win-builder.r-project.org/) on Windows, if you do not have these platforms yourself.
  - @aiorazabala as the creator of `qmethod` will then review the changes and accept the pull request if possible. 
  By accepting, the pull request, some `feature-branch` is then merged into @aiorazabala's `master`
5. Periodically, whenever significant work has been done, @aiorazabala drafts a release (as per [#121](https://github.com/aiorazabala/qmethod/issues/121) from `master`, essentially just marking some point in the history of the package as `x.x.x`, and sends it off to CRAN.

We try to abide by [Google's R Coding Style Guide](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml), and would recommend that for additions, too.
