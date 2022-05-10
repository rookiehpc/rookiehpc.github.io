# Welcome to the RookieHPC website v2 #

This repository contains the source code of the Github Page website meant to replace the [RookieHPC website](https://www.rookiehpc.com) eventually for reasons that will be given in minute. Although this is a work in progress, in due time the switch will be made and the url www.rookiehpc.com will point to this [Github Page version](https://rookiehpc.github.io) ;)

## 1) Migration to Github Pages ##

There is already a RookieHPC website online and working, so you may wonder why migrating to Github Pages at all? There are four reasons motivating this decision:

### 1.1) - Keeping the momentum ###
Since its creation in 2017, the RookieHPC website gathered over 1,300,000 pages views at the time of writing (May 2022). As of April 2022, it passed the 100,000 page views per month. Based on the feedback received by email, it seems to have helped a non-negligible number of students, teachers, researchers and industrials ranging from pure rookies to HPC experts.

That is the good news; it has become popular. The consequence of the good news is that there is an increasing demand for additional content such as missing documentation entries or additional technologies to cover. The problem is that this website always had a fundamental weakness: it has been written from scratch, and maintained ever since, by a single person. Needless to say that the progress made on the website turned out to become fluctuant. By opening its source code through Github, it also opens it to contribution, which hopefully will help keep new content flowing in :)

[Go back to top](#welcome-to-the-rookiehpc-website-v2)

### 1.2) Contribution ###
Regularly, someone would kindly offer their help by email. Unfortunately, contribution was close to impossible to accommodate as it required access to an SQL database as well as the server machine via SSH access. This architecture was initially designed for solo development, it did its job but now it needs to adapt to welcome contributions and collaboration. By migrating to Github, contribution comes out of the box :D

#### 1.2.1) How ####
The first step was to remove the dependencies on external sources of data such as the SQL database from which the MPI and OpenMP contents were drawn. TO that end, the RookieHPC website has been entirely rewritten so that all the website data is now within this repository, encompassing exercises, examples, documentation, indexes etc...

Github Pages websites cannot rely on server-side technologies (PhP and so on). Therefore, webpages have been redesigned to be built dynamically using the following structure. Let's consider the MPI documentation entry `MPI_Comm_rank`, you will find a directory `MPI_Comm_rank` inside `/mpi/docs/` with:

- `index.html`: the file containing the generic code calling Javascript functions that will produce the HTML source code of a webpage based on the `data.json` file that will be found in that same folder
- `data.json`: the file containing the specific information regarding `MPI_Comm_rank` as a JSON record
- `example_1.c`: the example 1 in C
- `example_1.f90`: the example 1 in FORTAN-90
- `example_1.f08`: the example 1 in FORTRAN-2008

Updating this `data.json` file therefore updates the corresponding `MPI_Comm_rank` webpage on the Github Page website! :D This is how suggesting edits to the RookieHPC website will now become convenient.

[Go back to top](#welcome-to-the-rookiehpc-website-v2)

#### 1.2.2) Previewing edits ####
<img src="https://github.com/rookiehpc/rookiehpc.github.io/blob/main/images/EditThisPageLink.png" width="300">

During this redesign, a new feature was implemented; you will see at the bottom of any page a link to edit the page (excluding the homepage). This will open the page editor, a minimalist JSON editor showing the content of the `data.json` file of the webpage you are viewing.

Modifying this JSON record on the left handside instantly updates the preview on the right handside. This allows to quickly modify the content of a page and see what it looks like. Once you are happy with the edits, you can submit a pull request for the corresponding `data.json` file with the new JSON record you have just written! :)

If you modify say the documentation entry `MPI_Comm_rank` for instance, you will find the corresponding JSON record in `mpi/docs/mpi_comm_rank/data.json`. 

[Go back to top](#welcome-to-the-rookiehpc-website-v2)

### 1.3) Open sourcing ###
It was stated on the RookieHPC website that it is exclusively about HPC (that is, no collection of personal data etc...). You will now be able to see it by yourself :)

[Go back to top](#welcome-to-the-rookiehpc-website-v2)

### 1.4) Free hosting ###
The hosting plan is costing a couple of hundred US dollars every two years. The challenge is that this is for a single shared server, and scaling to more (& non-shared) servers to support the increasing traffic would logically come at greater expenses. Getting free hosting on Github Pages is always a nice bonus.

[Go back to top](#welcome-to-the-rookiehpc-website-v2)

## 2) Questions ##
### 2.1) How to contact now? ###
* To raise any concern, bug or ask for new entries: you can now easily do it by creating an "issue" :) (You will also be able to get comments, see the progress made and even contribute to issues raised by others! :D)
* To send a private message: you can either send a message to the [RookieHPC Twitter account](https://twitter.com/rookiehpc) or send an email to the email address available on the [RookieHPC Github profile](https://github.com/rookiehpc).

[Go back to top](#welcome-to-the-rookiehpc-website-v2)

### 2.2) How do I contribute? ###
Contribution can be a delicate topic; how to handle copyright, how to properly recognise contributions, acknowledge contributors etc... All of which I need to learn more as this is new to me. At the time of writing, a few points remain to be clarified, one of which being which license pick. No license is indicated so far, meaning that all copyright is retained by default. However, it is likely the AGPLv3 license is selected, which seens to be the open source license by nature.

[Go back to top](#welcome-to-the-rookiehpc-website-v2)

## 3) Bonus ##
While I was at it, a dark theme was also integrated with an automatic detection to match your OS theme. I regret not having implemented that years ago...

[Go back to top](#welcome-to-the-rookiehpc-website-v2)
