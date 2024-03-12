# Databrary

Want to help out? Please reach out via email, commenting on an issue, or whatever works for you. We have significant experience mentoring new developers and Haskell developers.

We have [preliminary library
documentation](http://databrary.github.io/databrary/) on our GitHub Pages site.
Haddocks coverage would be another excellent place to start contributing...

A simple [code coverage report](http://databrary.github.io/databrary/) is also
available on our GitHub pages site.

## Why?

Accelerate the pace of sharing and reusing recorded, annotated video, to increase the pace of analysis and discovery in behaviorial science. Sharing restricted videos without burdening researchers heavily requires a sophisticated, yet simple, approach to entering release level information and authorizing networks of users to datasets. It also requires convenient mechanisms for uploading large collections of video files. It requires allowing for a very flexible model of meta-data to allow a large variety of attributes to be attached to a given dataset.

## Demo

<img width="32%" src="http://databrary.github.io/databrary/screenshots/Screen Shot 2018-07-18 at 9.27.55 AM.png"> <img width="32%" src="http://databrary.github.io/databrary/screenshots/Screen Shot 2018-07-18 at 9.28.02 AM.png"> <img width="32%" src="http://databrary.github.io/databrary/screenshots/Screen Shot 2018-07-18 at 9.28.18 AM.png">

<img width="32%" src="http://databrary.github.io/databrary/screenshots/Screen Shot 2018-07-18 at 9.28.48 AM.png"> <img width="32%" src="http://databrary.github.io/databrary/screenshots/Screen Shot 2018-07-18 at 9.29.01 AM.png">

Go for a test drive on http://dev1.databrary.org:8000/. We are excited to have your attention and interest.

1. Login as test@databrary.org / unastan54
2. Click on "Testarosa Tesla", in the upper right corner, and select "Create Volume"
3. Select "No" at the bottom of the page and enter some text in boxes provided
4. Click on the "Enter Data" tab
5. Click on "add folder", enter a test date, e.g. "01/02/2017"
6. Scroll to the top of the page and click the "eye" icon, to switch to view mode

Try browsing around a site with example data here: http://sandbox1.databrary.org:8000/

You can get an understanding of the full scope of features offered in our feature catalog ([pdf](http://databrary.github.io/databrary/Databrary_Feature_Catalog.pdf)). Some pages have changed since this catalog was produced.

## Related Work (Open Source)

- https://github.com/IQSS/dataverse
- https://github.com/DSpace/DSpace
- https://github.com/CenterForOpenScience/osf.io

## Testing, Requirements, Implementation Documentation

|                 | docs                                                                                                                           |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| functional      | [docs](https://github.com/databrary/design/blob/master/overview.md)                                                            |
| internal routes | [<img src="https://img.shields.io/badge/redoc-generated-brightgreen.svg">](http://databrary.github.io/databrary/route-doc/#)   |
| front end       | [<img src="https://img.shields.io/badge/jsdoc-generated-brightgreen.svg">](http://databrary.github.io/databrary/frontend-doc/) |
| back end        | [<img src="https://img.shields.io/badge/haddocks-generated-brightgreen.svg">](http://databrary.github.io/databrary/haddocks/)  |

|                       | results                                                                                                                                                                                                   | coverage                                                                                                                                                       |
| --------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| functional            | [<img src="https://img.shields.io/badge/tests-passed-brightgreen.svg">](http://databrary.github.io/databrary/Report.html)                                                                                 | [<img src="https://img.shields.io/badge/coverage-37%25-yellow.svg">](http://databrary.github.io/databrary/coverage-comparison/katalon-coverage/hpc_index.html) |
| front end integration |                                                                                                                                                                                                           |                                                                                                                                                                |
| front end unit        |                                                                                                                                                                                                           |                                                                                                                                                                |
| back end integration  | [<img src="https://img.shields.io/badge/runscope-passed-brightgreen.svg">](https://www.runscope.com/radar/agq3bvszxaxe/b788db16-d63b-4ea9-ba98-5abc477dd03d/history/dac7e3ce-280d-4ade-90b1-260cb41614bf) | [<img src="https://img.shields.io/badge/coverage-29%25-yellow.svg">](http://databrary.github.io/databrary/coverage-comparison/runscope/hpc_index.html)         |
| back end unit         | [<img src="https://img.shields.io/badge/tests-passed-brightgreen.svg">](https://github.com/databrary/databrary/blob/gh-pages/databrary-1-discovered.log)                                                  | [<img src="https://img.shields.io/badge/coverage-26%25-yellow.svg">](http://databrary.github.io/databrary/coverage/hpc_index.html)                             |

## Upstream - Anticipating and Contributing

Anticipating features and libraries:

- aeson [JSON parsing with accumulative errors](https://github.com/bos/aeson/issues/578)
- smtp-mail [issue](https://github.com/jhickner/smtp-mail/issues/2)
- zip [Blind Archive Support](https://github.com/mrkkrp/zip/issues/20)
- [beam](https://github.com/tathougies/beam)
- [servant-swagger](https://github.com/haskell-servant/servant-swagger)
- hedgehog [State machine testing multiple results](https://github.com/hedgehogqa/haskell-hedgehog/issues/113)
- [ldap-haskell](https://github.com/ezyang/ldap-haskell)

Recent contributions:

- https://git.coop/lwm/tasty-discover/issues/145
- https://github.com/lspitzner/brittany/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+chreekat

## Service and Operations Status

[<img src="https://img.shields.io/uptimerobot/ratio/m780713534-b4d455210e15c1c4ec4dc851.svg">](https://stats.uptimerobot.com/J80YrsnzP)

Nix Cache - [<img src="https://img.shields.io/uptimerobot/status/m780713505-7d8d87840b9cd6213c96b467.svg">](https://stats.uptimerobot.com/K8JYvs7XW)

## Open Source Report Card

We try our best to use open source throughout our source code, our development and our operations. Here is where we stand currently.

Open source tools: ghc, all haskell libraries, nixpkgs, centos, ubuntu, nixos, redoc/swagger, solr/openjdk, postgresql, nginx, vim, emacs, slurm.

Non-open source tools: runscope, katalon, github, jira, confluence, aws, macos, mailtrap, gmail, slack, google drive, skype, sublime.

## License

Copyright (C) 2013-2024 New York University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
