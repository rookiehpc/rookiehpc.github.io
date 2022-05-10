# Welcome Rookie! :D #
This repository contains the source code of the Github Page website meant to replace the [RookieHPC website](https://www.rookiehpc.com) eventually. Although this is a work in progress, in due time the switch will be made and the url www.rookiehpc.com will point to this Github Pages ;)

## Migration to Github Pages ##
There is already a RookieHPC website online and working, so you may wonder why migrating to Github Pages at all? There are four reasons motivating this decision:
1. Keeping the momentum
1. Contribution
1. Open sourcing
1. Free hosting

### 1. Keeping the momentum ###
Since its creation in 2017, the RookieHPC website gathered more than 800,000 pages views at the time of writing (October 2021). Based on the feedback received by email, it seems to have helped a non-negligible number of students, professors and researchers.

That was the good news; it is popular. The consequence of the good news is that there is a rather constant demand for more content such as missing documentation entries or additional technologies to cover. The problem is that this website always had a fundamental weakness: it had been written from scratch, and maintained ever since, by a single person. Needless to say that the progress made on the website turned out to become fluctuant. By opening its source code through Github, it also opens it to contribution, which hopefully will help keep new content being populated :)

### 2. Contribution ###
Regularly, someone would kindly offer their help by email. Unfortunately, without getting into details, contribution would have been cumbersome to accommodate. By migrating to Github, contribution comes out of the box :D

In addition, a new feature has been added; you will see at the bottom of pages a link to edit the page. This will open the page editor, which shows the JSON data displayed for this page. Changing the JSON data on the left handside instantly updates the preview on the right handside. This allows to quickly update the content of a page and see what it looks like, before updating the corresponding JSON file and issuing a pull request :)

### 3. Open sourcing ###
It was stated on the RookieHPC website that it is exclusively about HPC (that is, no collection of personal data etc...). You can now see by yourself :)

### 4. Free hosting ###
The hosting plan is costing a couple of hundred US dollars every two years. The challenge is that this is for a single shared server, and more extensive server plans logically cost more.

## Questions ##
### How to contact now? ###
* To ask for a documentation entry to be added: you can now easily do it by creating an "issue" :) (You will be able to get comments, see the progress made and even contribute to issues raised by others! :D)
* To send a private message: you can either send a message to the [RookieHPC Twitter account](https://twitter.com/rookiehpc) or send your email to the email address available on the [RookieHPC Github profile](https://github.com/rookiehpc).

### How do I contribute? ###
At the time of writing, a few points remain to be clarified. Which license use is one of them. No license has been indicated so far, meaning that all copyright is retained. However, it is likely the AGPLv3 license is selected, which is the open source license by nature.

The unique drawback of this migration to Github is the fact that you will no longer be able to anonymously fill a contact form from the website directly since the PhP form that was designed for that purpose is not compatible with the static-only pages hosted on Github Pages.
