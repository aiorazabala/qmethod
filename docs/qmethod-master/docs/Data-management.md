## Data management

A Q study can quickly involve quite a lot of different kinds of interrelated  data, including a *concourse*, a *Q set* (or sample), a *condition of instruction* as well as the actual *Q sorts*.
This page suggests some **best practices** for *reproducible*, *cumulative* and *systematic* Q research, as developed during the first [keyneson](https://github.com/maxheld83/keyneson) study.

These best practices can be best implemented using the import functions `import.q.concourse`, `build.q.set`, `import.q.sorts`, `import.q.feedback` and the print function `make.cards`, allowing for a **one-stop-shop** development, iteration and administration of a Q study.
However, nothing in the R Package `qmethod` *requires* that you follow these best practices; functions are generic and applicable to a wide range of use cases.

Not all of these practices and facilities will be immediately appropriate for all studies, especially small and ad-hoc studies.
However, as Q methodology grows and consolidates, more researchers may confront similar challenges to which these best practices provide preliminary solutions.

The below suggestions proceed from simpler, basic to more advanced data management suggestions.

> **[TL;DR](https://en.wikipedia.org/wiki/Wikipedia:Too_long;_didn't_read)**: If you'd rather not read a lengthy piece, but look at an example and get started right away, check out [Max Held](http://www.maxheld.de)'s Q study [`keyneson`](https://github.com/maxheld83/keyneson), for which these practices were developed.


## What Makes For Best Practices (in Q Research)?

In spite of the great diversity of approaches to Q methodology, some criteria of good research practice may be universally acceptable, including:

1. **Reproducibility**.
  Some other researcher should be able to precisely track and [reproduce](https://en.wikipedia.org/wiki/Reproducibility) all steps taken during a research project, especially when it involves empirical analysis.
  Aside from a deeper commitment to open science, this can also help everyone avoid small, but consequential mistakes.  
  In Q methodology, reproducibility may imply:
  - that the gathering, sourcing and editing of the *concourse* of items be well documented,
  - that the sampling of a *Q set* from a concourse be documented and justified,
  - that the *condition of instruction* be documented or
  - that the data entry, verification and cleaning of *Q sorts* be (programmatically) documented.
  While concepts of [external validity](https://en.wikipedia.org/wiki/External_validity) or test/retest reliability do not easily apply to a Q study of *subjectivity*, reproducible Q research may also involve a replication of a given Q study, with other people, or at another time.
2. **Cumulativeness**.
  Q studies, as other research, should build on, or be informed by previous work in a systematic way.
  A concourse theory of communication (Stephenson 1978), on which Q methodology is premised, especially, may suggest that any scientific attempt to tap in this multitude of subjective statements should build on past attempts at doing so, and be open to future revisions.  
  In Q methodology, cumulative research may imply:
  - that other researchers, or the wider public get to suggest edits or additions to a *concourse* of statements,
  - that other researchers sample new *Q sets* from an existing concourse, shared and co-developed between several researchers and the public,
  - that the same Q sets are used in different *Q studies*, using a different participant *p set*, or *condition of instruction* or,
  - that Q researchers conduct *meta analyses*, comparing factors extracted from different or same Q sets, but with different people, at different times, and so on.
3. **Systematicity**.
  For Q methodology, systematicity may imply:
  - that a *method for sampling* concourse items into a Q set (structured or unstructured) is documented in a way so that they may be applied to another concourse, or another study,
  - that suggestions for edits or additions to concourse or Q set are (publicly) documented, including justifications for rejected edits or,
  - that the full set of items in the Q set, the concourse and its sources (if applicable), are easily navigable by other researchers, even if this material cannot all be published in established outlets.


## Naming Items

Items in a Q set or concourse may need to be referred to in different ways, depending on the study.


### Full Item Wording or Stimulus

Items themselves may take many forms, including longer or shorter written language, but also other stimulus material such as pictures. 
Conventionally, let us refer to this as the **full item stimulus** or **full item wording**, depending on the stimulus.
An example (from [keyneson](https://github.com/maxheld83/keyneson)) would be:

> Labor is not a commodity.

Full item wordings may best be saved as *individual* text files in one directory.
It is recommended to use flat text files and *not* binary/proprietary word processor files (such as ~~`*.doc`~~), because the former are smaller, more robust, future-proof and easily transferable.
A full item wording file may simply look like this:

```
Labor is not a commodity.
```

The import function `import.q.concourse` included with the R package `qmethod` expects `*.TEX` as a file extension (which stands for the [LaTeX typesetting language](http://www.latex-project.org), but LaTeX markup is strictly optional.
If you wish to use LaTeX formatting, you can just add markup as in a normal LaTeX file, with no preamble or other declarations needed.
For example,

```
Labor is \emph{not} a commodity.
```

would yield

> Labor is *not* a commodity.


### Item Handles

Depending on the length of these items, and the desired output format, researchers may find it cumbersome to always refer to items by their full item wording.
Instead, items can be conveniently assigned an **item handle**, which should be *short* and *meaningful* to the researcher (say, `labor-no-commodity`, for the above example).
Researcher can then use this item handle to:
- identify *files* including the full item wording (`labor-no-commodity.tex`)
- identify *translations* of an item over various languages (`/english/labor-no-commodity.tex`)
- identify different *versions* of an item (crudely as `labor-no-commodity-1.tex`, preferably by a version control program as suggested below).
- identify items quickly during *factor interpretations* or in *visualizations*, as in the following example:

  > Item `labor-no-commodity` is a distinguishing statement for factor 1.

- link item *feedback* from participants (or other people) to an item, as in the following example `*.csv` file:
   
    ```
    handle,feedback
    labor-no-commodity,"I don't know what a commodity is."
    ```

- identify items to be sampled from a concourse into a Q set as in the following example `sample.csv` file:

    ```
    labor-no-commodity,
    growth-trumps-equality,
    ...
    ```


### Item IDs

Another need to refer to items in some shorthand way arises during the administration of a Q study.
To record participant Q sorts, it would often be too cumbersome to refer to items by their full wording.
Instead, researchers will usually enter some short identifier to record a participants Q sort.

In some settings, it may also *not* be advisable to have participants see the above *item handles*, because these *meaningful* snippets may be understood as additional stimulus by participants, and affect their sorts in unintended ways.
(This may be a similar effect to using Q-cards made from different material, or in different colors for different items).

For that reason, a unintelligible identifier, or **ID** may be advisable to refer to items for Q sort administration.

The import functions `import.q.sorts`, `import.q.feedback` and the printing function `make.cards` included in the R package `qmethod` allow for two ways of doing this:

1. Researchers can **manually** enter arbitrary strings to identify items, such as the customary `sta001`.
  In this case, researcher should specify their manual IDs using the `manual.lookup` options in the above functions (see R documentation for details).
  Such manual IDs can either be "hard-coded" in R, or they can be conveniently read in from a `*.csv` file using the `read.csv` function of base R.
  Such an example `ids.csv` file may look like this:
   
    ```
    handle,id
    labor-no-commodity,sta001
    growth-trumps-equality,sta002
    ...
    ```
    
2. Alternatively, researchers can use the above import and print functions to create an automatic **hash** from the full item wording.
  A hash is a cryptographic way to transform much longer pieces of information into short summaries.
  The same full item wording will always produce the same hash (using the same algorithm), but you cannot reconstruct the full item wording from *only* the hash, if you don't know the set of possible statements from which the hash was created.
  The hash value will be some arbitrary string such as `3ed68fde`.  
  Hashing is default behavior for the above functions and is recommended for several reasons:
  1. Manual ID tables are a frequent source of errors
  2. Computers can do this kind of identifying job better than humans
  3. A hash value will *automatically* change if *something* in the full item wording changes, allowing for a highly reliable way to relate recorded Q sorts back to the items used during administration.
    For example, if, at the last minute before Q sort administration

    ```
    Labor is not a commodity.
    ```
    
    is changed to

    ```
    Labor is something that can be bought and sold like everything else on the market.
    ```
    
    the hash value created by `make.cards` and expected by input functions will *automatically* change, thus negating the possibility of confusing one item version for another.
    Using hash values (and proper version control), researchers will always know exactly what variant of an item people saw and sorted.

This is how items created by `make.cards` using an **ID** look like (in this case, a *manual* ID):

![q card printout](https://cloud.githubusercontent.com/assets/5372770/5266097/df2469c2-7a47-11e4-8838-69c702f40241.jpg)

You can easily break out individual cards, with their **ID** on the back, and the full item wording on the front:

![q card breakout](https://cloud.githubusercontent.com/assets/5372770/5266096/df23f618-7a47-11e4-8f41-19c586e56849.jpg)

**Notice** (from the `qmethod` manual):

> Hashed identification has not been widely tested in Q studies and should be used with great care and only for extra convenience.
> When using hash identification, researchers should be careful to record the precise item wordings at the time of hashing for the printed Q cards, preferably with a version control system.
> Researchers should also record the complete Q sorts of participants in an *unhashed* form, such as a picture of the completed sort in full wordings, in case problems with the hashing arise.  
> This function currently only works for Avery Zweckform C32010 templates, designed in `/cardtemplates/AveryZweckformC32010.Rnw`.
> If you would like support for other templates, check out / chip in [here](https://github.com/aiorazabala/qmethod/issues/34).


## Directory Structure

### One Language, One Condition

The simplest directory structure, starting from the root of some Q study, should look like this:

```
├── feedback
│   └── JohnDoe.csv  # these include possible feedback with one line per item
├── qsorts
│   ├── JaneDoe.csv  # these include the full sorts, recorded in raw form
│   └── JohnDoe.csv
└── sample
    ├── concourse
    │   ├── life-with-q.tex  # these include the full item wordings
    │   ├── q-uprising.tex
    │   ├── r-dominance.tex
    │   ├── small-village.tex
    │   └── video.tex
    │   └── ids.csv  # this includes the IDs, if hard entered 
    └── sampling-structure.csv  # this includes a list of items to be sampled into the q-set
```


### Multilingual, Multi-Condition

The import and print functions in `qmethod` also support *multilingual*, and *multi-condition* Q studies.
In this case, the arguments `conditions` and `languages` should be specified when calling the functions.
The functions will then expect these conditions and languages in the directory structure.

With all bells and whistles, taken from the `importexample` data shipped with `qmethod`, a directory should look like this:

```
├── feedback
│   ├── after  # same conditions as specified in function call
│   │   └── JohnDoe.csv
│   └── before
├── qsorts
│   ├── after  # same conditions as specified in function call
│   │   ├── JaneDoe.csv
│   │   └── JohnDoe.csv
│   └── before
│       ├── JaneDoe.csv
│       └── JohnDoe.csv
└── sample
    ├── concourse
    │   ├── english  # same languages as specified in function call
    │   │   ├── life-with-q.tex
    │   │   ├── q-uprising.tex
    │   │   ├── r-dominance.tex
    │   │   ├── small-village.tex
    │   │   └── video.tex
    │   ├── german
    │   │   ├── life-with-q.tex
    │   │   ├── q-uprising.tex
    │   │   ├── r-dominance.tex
    │   │   ├── small-village.tex
    │   │   └── video.tex
    │   └── ids.csv
    └── sampling-structure.csv
```


### File Types

The above directory includes the following different kinds of files:


#### Item Feedback Files

This is where you store **item feedback** received from participants.

The idea of these files is that such item feedback may be instructive in later factor interpretations, during which it can be called programmatically.

- Files named after the participant who provided the feedback (or a pseudonym), such as `JohnDoe.csv`
- Files are `*.CSV`, or comma-separated values files, that can be produced in most spreadsheet editors.
- As per `import.q.feedback` they include columns for the *ID*, *feedback*, and whether said feedback was just a *correction* (such as a typo), which may *not* be of greater interest to the researcher in later analysis.
- The first row includes headers.
- Item feedback is best enclosed in `" "` to allow for commas *within* a piece of feedback.

A file may look like this:

```
item_id,item_feedback,correction
i01,"I don't like Asterix and Obelix",FALSE
i02,"There is a typo here!",TRUE
```


#### Q Sorts Files

This is where you record raw Q-sorts, as prepared by participants.

- Files named after the participant who provided the Q-sort (or a pseudonym), such as `JohnDoe.csv`
- Files are `*.CSV`, or comma-separated values files, that can be produced in most spreadsheet editors.
- As per the default of `import.q.sorts` (`header = TRUE`) they start a header of variable names in the first line.
  Variable names are ignored by `import.q.sorts` but should be the rank orders (`"-3"` etc.) for consistency.
- Columns are rank orders from the Q-sort (say, `-4` to `+4`), rows are (as in Q-sorts) meaningless, and cells include items by their *ID*s.

A very simple file may look like this:

```
"-1","0","1"  # this first line will be interpreted as variable names
,i01,
i02,i03,i04
```


#### Item Files

This is where you save actual full item wordings.

- Files are named according the item handle, such as `life-with-q.tex`.
- Item handles (and file names) should not include any special characters or spaces.
- Items should be saved as `*.TEX`, but need not include LaTeX markup --- just text suffices.
  (see above)

A very simple file may look like this:

```
And life is not easy for the R-legionaries who bother to read the works of Stephenson and Brown, for these posit actual Q logics of inquiry.
```


#### Additional Files

If manual IDs are used (not recommended), that file may also be saved as a `*.csv` to enable others to reproduce it.
Conventions are not important, so as long as the file is correctly read in and modified as expected for input in `import.q.sorts` or `make.cards`.

An ID file may look like this:

```
ID,handle
i01,r-dominance
i02,q-uprising
i03,small-village
i04,life-with-q
i05,video
```


If a Q set is a selected from a concourse using structured sampling, that sampling subset may also be saved as a `*.csv`, to enable others to reproduce it.
Conventions are not important, so as long as the file is correctly read in and modified as expected for input in `build.q.set`.
An sampling structure file may include arbitrary additional columns, but should include the `item handles`.

A sampling file may look like this:

```
handle
life-with-q
q-uprising
r-dominance
small-village
```


### Why All This Fuss?

Maintaining such a directory structure and the below file types has a number of advantages and enables good research practice.

- Keep Them **Raw**.
  To make research *reproducible*, researchers should save data in the *rawest* form possible, doing all data transformation, cleaning and verification programmatically, and in a well-documented way *on top* of raw data.
  This way, other researchers can check for errors in data preparation, and start from the same raw data.
  For Q methodologists, *raw* data may imply:
  - entering Q sorts in the raw form, as prepared by participants, including, if possible, pictures of completed sorts.
  - entering Q sets and concourse items as raw text files, with all combination, sampling and printing done programmatically.
- Keep Them **Nested** (but separate).
  To enable *systematic*, and *cumulative* research, Q methodologists may want to store *concourse*, *Q set* (or *Q sample*) and the actual study with *Q sorts* in separate, nested directories.
  Notice that any *given* Q study or Q sort is always defined by a particular Q set (or Q sample), which in turn, is defined by a particular concourse.
  However, several researchers may *share* the same concourse (but draw a different Q set sample from it), or *share* the same Q set (but use it for a different study).
- Keep Them Under **Version Control**.
  Version control is highly recommended for any Q study, where concourse items and sample change frequently, but such changes need to be well-documented.
  It is particularly important to have a precise snapshot of the concourse at the time of sampling, and the Q set at the time of Q sort administration.
  The [Git](http://git-scm.com) is a free and open source distributed version control used by many researchers around the globe and is well-suited for Q studies.
- Keep Them in **Nested Submodules**.
  Version control and nested directories become very powerful, when combined using `git submodule`.
  [Git submodules](http://git-scm.com/docs/git-submodule) are essentially Git projects *inside* other git projects, where a superproject always includes a pointer to a particular version of a subproject.
  Git submodules can initially appear unintuitive and lead to unexpected results, but if used appropriately, suit a *cumulative* Q research project very well.
  If the `root` (= the Q study, including Q sorts), `sample` (= the Q set), and the `concourse` folder in the above are all independently versioned as nested submodules, any given Q study is defined by a precise pointer to some version of a Q set, which in turn is defined by a precise pointer to some version of a concourse.
  At the same time, *other* researchers can use arbitrarily different combinations of `sample` and `concourse` for their research projects, while maintaining a systematic relationship between different efforts.
- Keep Them **Well Documented**.
  Any given Q study, Q set and concourse should be accompanied by detailed documentation, including, for example:
  - Q study: the condition of instruction, date and time of administration, information about the P set (or participants) etc.
  - Q set (or sample): the logic for a structured sample, or the algorithm for an unstructured sample, some theoretical background, etc.
  - Concourse: the gathering method, sources, etc.
  Popular Git hoster [GitHub](http://www.github.com) offers *Wikis* that can be attached to each repository as a convenient way to store this kind of meta-information.
  Wikis themselves can also be added as submodules, relating any given meta information (say, some version of a sampling structure) to a specific version of a repository (say, a Q set).
- Make them **Open**.
  If you have all of your data in raw form, version controlled and well documented, current collaborative technologies such as [GitHub](http://www.github.com) offer great ways for collaboration:
  - Other researchers can *fork* your Q set or concourse to develop their own, related versions.
  - Other researchers can suggest edits or additions to your Q set or concourse in *pull requests*.
  - Other researchers or the wider public can comment in *issues* on existing items and samples, or suggest new ones.
  In the *permissionless* spirit of Open Source software development, these conventions allow everyone to contribute or comment --- but they do not force the original author to accept any of the changes.


### With Everything, Please

If you're curious, what a Q study with all of these suggestions looks like, check out [Max Held](http://www.maxheld)'s [`keyneson`](https://github.com/maxheld83/keyneson) repository.

A selection of that directory structure looks like this:

```
├── README.md
├── feedback
│   ├── after
│   │   ├── Frank.csv
│   │   ├── Ingrid.csv
│   │   ├── ...
│   │   └── Wolfgang.csv
│   └── before
│       ├── Claus.csv
│       ├── Frank.csv
│       ├── ...
│       └── Susanne.csv
├── keyneson-sample  # this is a git submodule
│   ├── README.md
│   ├── keyneson-concourse   # this is a git submodule
│   │   ├── README.md
│   │   ├── english
│   │   │   ├── ability-2-pay.tex
│   │   │   ├── all-people-own-earth.tex
│   │   │   ├── ...
│   │   │   └── yield-2-capital-norm.tex
│   │   ├── german
│   │   │   ├── ability-2-pay.tex
│   │   │   ├── all-people-own-earth.tex
│   │   │   ├── ...
│   │   │   └── yield-2-capital-norm.tex
│   │   ├── ids.csv
│   │   └── keyneson-concourse.wiki  # this is a git submodule
│   │       └── Home.md
│   ├── keyneson-sample.wiki   # this is a git submodule
│   │   ├── Home.md
│   │   └── sampling-structure.md
│   └── sampling-structure.csv
├── keyneson.wiki  # this is a git submodule
│   ├── Home.md
│   ├── Q-Sort-Form.pdf
│   └── condition-of-instruction-de.md
└── qsorts
    ├── after
    │   ├── Christian.csv
    │   ├── Frank.csv
    │   ├── ...
    │   └── Wolfgang.csv
    └── before
        ├── Christian.csv
        ├── Claus.csv
        ├── ...
        └── Wolfgang.csv
```

