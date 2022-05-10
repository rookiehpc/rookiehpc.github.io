# Welcome Rookie! :D #
This repository contains the source code of the Github Page website meant to replace the [RookieHPC website](https://www.rookiehpc.com) eventually. Although this is a work in progress, in due time the switch will be made and the url www.rookiehpc.com will point to this Github Pages ;)

## Migration to Github Pages ##
There is already a RookieHPC website online and working, so you may wonder why migrating to Github Pages at all? There are four reasons motivating this decision:
1. Keeping the momentum
1. Contribution
1. Open sourcing
1. Free hosting

### 1. Keeping the momentum ###
Since its creation in 2017, the RookieHPC website gathered over 1,300,000 pages views at the time of writing (May 2022). Based on the feedback received by email, it seems to have helped a non-negligible number of students, teachers, researchers and industrials ranging from pure rookies to HPC experts.

That is the good news; it has become popular. The consequence of the good news is that there is an increasing demand for additional content such as missing documentation entries or additional technologies to cover. The problem is that this website always had a fundamental weakness: it has been written from scratch, and maintained ever since, by a single person. Needless to say that the progress made on the website turned out to become fluctuant. By opening its source code through Github, it also opens it to contribution, which hopefully will help keep new content being populated :)

### 2. Contribution ###
Regularly, someone would kindly offer their help by email. Unfortunately, contribution was near to impossible to accommodate as the data was scattered between a SQL database to which only I had credentials, and the files that would need to be updated directly on the server machine via SSH access. By migrating to Github, contribution comes out of the box :D

The website has been entirely rewritten so that all data is accessible directly from within the repository. Pages have been designed such that each page is built dynamically from a `data.json` file. Updating a `data.json` file therefore updates the corresponding page on the Github Page website! :D This is what can become the cornerstone feature of contribution for the RookieHPC website.

In addition, a new feature has been added; you will see at the bottom of pages a link to edit the page. This will open the page editor, which shows the JSON record for this page. Changing the JSON data on the left handside instantly updates the preview on the right handside. This allows to quickly modify the content of a page and see what it looks like, before updating the corresponding `data.json` file and issuing a pull request :)

If you modify say the documentation entry `MPI_Comm_rank` for instance, you will find the corresponding JSON record in `mpi/docs/mpi_comm_rank/data.json`. At the time of writing, only a dozen of MPI entries are covered because this Github Page project is still a prototype for now. Once its design will have been polished, it will be straightforward to port the content from the current online website to this version.

### 3. Open sourcing ###
It was stated on the RookieHPC website that it is exclusively about HPC (that is, no collection of personal data etc...). You will now be able to see it by yourself :)

### 4. Free hosting ###
The hosting plan is costing a couple of hundred US dollars every two years. The challenge is that this is for a single shared server, and scaling to more (& non-shared) servers to support the increasing traffic would logically come at greater expenses. Getting free hosting is always a nice bonus.

## Bad news ##
The main drawback of this migration to Github is the fact that you are no longer able to *anonymously* fill a contact form from the website directly since the technique that was used to achieve it is not compatible with the static-only pages hosted on Github Pages. If anonimity does matter to you, you are welcome to pick a disposable email address easily findable online :)

## Questions ##
### How to contact now? ###
* To raise any concern, bug or ask for new entries: you can now easily do it by creating an "issue" :) (You will also be able to get comments, see the progress made and even contribute to issues raised by others! :D)
* To send a private message: you can either send a message to the [RookieHPC Twitter account](https://twitter.com/rookiehpc) or send an email to the email address available on the [RookieHPC Github profile](https://github.com/rookiehpc).

### How do I contribute? ###
At the time of writing, a few points remain to be clarified, one of which being which license pick. No license is indicated so far, meaning that all copyright is retained by default. However, it is likely the AGPLv3 license is selected, which is the open source license by nature.
