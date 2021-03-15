## _qmethod_, an R package to analyse Q methodology data

Q is a methodology to study the distinct perspectives existing within a group of people, on a topic of interest.
It is used across disciplines. See further about the method in [http://qmethod.org/about](http://qmethod.org/about). See [an example of what you can do with this package](./Sample-plot).


### Overview

This package performs the analysis of Q methodology data. Options include different extraction methods (principal components analysis and centroid factor extraction) and rotation methods (none, varimax and other mathematical rotations). See a [graphical interface](./GUI).

_(From the package reference manual)_

> The following steps of the analysis correspond to separate functions: automatic flagging of Q-sorts (manual flagging is optional), z-scores and factor scores for statements, distinguishing and consensus statements, and general characteristics of the factors. 
> The function _qmethod_ wraps them all.

> The functions for each step may be used separately for advanced analysis, for example, for manual flagging. 
> The package includes a function to import data from PQMethod software, to export plain text data for interpretation in two flavours, and to print and plot Q method results.


### Resources

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
To contribute, read the [guidelines](./Contribute), post your suggestions on the [issue tracker](https://github.com/aiorazabala/qmethod/issues), or email the maintainer.

The package is free and open source. It has been thoroughly [tested an validated](http://journal.r-project.org/archive/2014-2/zabala.pdf). If you use it, please [cite it](https://cran.r-project.org/web/packages/qmethod/citation.html) in your work.

Pathway of priority developments, should resources allow:

- [X] Implement centroid extraction.
- [X] Enhance help pages/ warnings/ info:
[#307](https://github.com/aiorazabala/qmethod/issues/307) (and close [#95](https://github.com/aiorazabala/qmethod/issues/95)), 
[#269](https://github.com/aiorazabala/qmethod/issues/269)
- [ ] Shiny GUI & usability
[#369](https://github.com/aiorazabala/qmethod/issues/369),
[#366](https://github.com/aiorazabala/qmethod/issues/366),
~[#287](https://github.com/aiorazabala/qmethod/issues/287)~, 
~[#262](https://github.com/aiorazabala/qmethod/issues/262)~, 
[#217](https://github.com/aiorazabala/qmethod/issues/217), 
[#195](https://github.com/aiorazabala/qmethod/issues/195), 
~[#81](https://github.com/aiorazabala/qmethod/issues/81)~, 
[#23](https://github.com/aiorazabala/qmethod/issues/23)
- [X] Dependencies:
~[#347](https://github.com/aiorazabala/qmethod/issues/347)~
- [ ] Compatibility with online Q-sort collection using [HTMLQ](https://github.com/aproxima/htmlq) and [easy-HTMLQ](https://github.com/shawnbanasick/easy-htmlq) (also FlashQ):
  - [X] Import Q-sorts, [#368](https://github.com/aiorazabala/qmethod/issues/368), 
  - [ ] Import Q-set, P-set data [#370](https://github.com/aiorazabala/qmethod/issues/370)

If you find the package and these resources useful, consider [supporting maintenance and further enhancements with a donation:](https://www.paypal.com/donate?hosted_button_id=GCMM9PTXPHNT8)

[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/donate?hosted_button_id=GCMM9PTXPHNT8)
