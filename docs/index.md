## _qmethod_, an R package to analyse Q methodology data

Q is a methodology to study the distinct perspectives existing within a group of people, on a topic of interest.
It is used across disciplines. See further about the method in [http://qmethod.org/about](http://qmethod.org/about). 


### Overview

This package performs the analysis of Q methodology data. Data can be imported from a range of formats, and results can be explored and exported in multiple ways. See a [graphical interface](./GUI) with the basic functionality and [an example of what you can do with this package](./Sample-plot).

The package provides all the options for standard Q analysis, such as different extraction methods (principal components analysis and centroid factor extraction), rotation methods (none or varimax), and both forced and non-forced distributions. Manual flagging can be easily run using R code [(see an example)](./Advanced-analysis#a-modify-any-individual-flag). Additional options include different correlation coefficients for the initial correlation matrix (Pearson, Spearman and Kendall) and other mathematical rotations.

A single function runs the full analysis ([`qmethod()`](https://www.rdocumentation.org/packages/qmethod/versions/1.5.5/topics/qmethod)). Each step can also be run separately using the corresponding functions for correlation matrix, automatic flagging, statement scores, distinguishing and consensus statements, and general factor characteristics.

Additional functions are available to import data from raw `.CSV`, ['HTMLQ'](https://github.com/aproxima/htmlq) and 'FlashQ' `.CSV` files, ['PQMethod'](http://schmolck.org/qmethod/) `.DAT` files and ['easy-htmlq'](https://github.com/shawnbanasick/easy-htmlq) `.JSON` files; to [plot](./plot) and summarise Q results; to import raw data from individual and multilingual `.CSV` files; to make printable cards; and to perform [bootstrapping](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0148087).

For full details about what you can do, see the [package reference manual](http://cran.r-project.org/web/packages/qmethod/qmethod.pdf).



### Resources

Here are further links to learn more about the software and about conducting Q methodology (more references in the [package reference manual](http://cran.r-project.org/web/packages/qmethod/qmethod.pdf)):

* A [cookbook of _qmethod_](./Cookbook).
* The [package in CRAN, the R repository](http://cran.r-project.org/web/packages/qmethod/index.html).
* The [package reference manual](http://cran.r-project.org/web/packages/qmethod/qmethod.pdf).
* The latest code of the package is on [this Github site](https://github.com/aiorazabala/qmethod).
* An introduction to the package, usage, and validation in [Zabala (2014)](http://journal.r-project.org/archive/2014-2/zabala.pdf).
* For those not familiar with R, see a [simple graphical visual interface](./GUI) to use either online (without installation) or offline.
* Introduction to Q methodology (slides of a graduate course): [session 1](http://aiorazabala.net/learnQ/Qmethod_AZ_slides_S2.pdf) and [session 2](http://aiorazabala.net/learnQ/Qmethod_AZ_slides_S2.pdf), including a demo of online Q-sorting using [HtmlQ](https://github.com/aproxima/htmlq).
* Recommendations to [report Q studies](https://github.com/aiorazabala/qmethod/wiki/Reporting-a-Q-study), for transparency and to facilitate evidence review and reproducibility.
* Suggested best practices in [data management](data-management) for Q studies.


***

### Development

The package has been created by [Aiora Zabala](http://aiorazabala.net), with contributions from [Max Held](http://www.maxheld.de/) and [Frans Hermans](https://www.researchgate.net/profile/Frans-Hermans-3). 
Contributions are most welcome. To do so, please read the [guidelines](./Contribute), post your suggestions on the [issue tracker](https://github.com/aiorazabala/qmethod/issues), or email the maintainer.

The package is free and open source. It has been thoroughly [tested an validated](http://journal.r-project.org/archive/2014-2/zabala.pdf). If you use it, please [cite it](https://cran.r-project.org/web/packages/qmethod/citation.html) in your work.

Pathway of priority developments, should resources allow:

- [ ] Shiny GUI & usability
[#369](https://github.com/aiorazabala/qmethod/issues/369),
[#366](https://github.com/aiorazabala/qmethod/issues/366),
~[#287](https://github.com/aiorazabala/qmethod/issues/287)~, 
~[#262](https://github.com/aiorazabala/qmethod/issues/262)~, 
[#217](https://github.com/aiorazabala/qmethod/issues/217), 
[#195](https://github.com/aiorazabala/qmethod/issues/195), 
~[#81](https://github.com/aiorazabala/qmethod/issues/81)~, 
[#23](https://github.com/aiorazabala/qmethod/issues/23)
- [ ] Compatibility with online Q-sort collection using [HTMLQ](https://github.com/aproxima/htmlq) and [easy-HTMLQ](https://github.com/shawnbanasick/easy-htmlq) (also FlashQ):
  - [X] Import Q-sorts, [#368](https://github.com/aiorazabala/qmethod/issues/368), 
  - [ ] Import Q-set [#370](https://github.com/aiorazabala/qmethod/issues/370), P-set data (done).

If you find the package and these resources useful, consider [supporting maintenance and further enhancements:](https://www.paypal.com/donate?hosted_button_id=GCMM9PTXPHNT8) [(see some of the previous work)[https://github.com/aiorazabala/qmethod/issues?q=is%3Aissue]]

[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/donate?hosted_button_id=GCMM9PTXPHNT8)
