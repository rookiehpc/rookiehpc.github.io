<img src="https://progress-bar.dev/33?title=MPI+docs+porting" height="25"> <img src="https://progress-bar.dev/0?title=OpenMP+docs+porting" height="25">

# Welcome to the RookieHPC website v2! :D #
To newcomers, a quick introduction: the RookieHPC website covers major technologies in High-Performance Computing (HPC), providing documentation accompanied with examples, as well as exercises and sometimes tools.

## I) Why a version 2? ##

The current RookieHPC website, which is online at https://www.rookiehpc.com, has been doing its job for years. However, without getting into too much details, behind the scenes its design does not permit to accommodate any external contribution. Although not a real problem in the early days, it has now become one. This is where this website v2 comes in.

The idea being to develop a rethought version of the website, this time designed to be contribution friendly, at last. To do so, the Github Pages service was leveraged; it produces a static website from a repository. In this case, this repository produces the website available at https://rookiehpc.github.io.

Everything that you see on that Github Pages website is a reflection of the source code in this repository. In other words, modifying the repository will update the website accordingly, and that's how we finally have contribution in the RookieHPC project! :D

At the time of writing, this is still a work in progress, although Github Pages version will eventually replace the original website. Concretely, the URL https://www.rookiehpc.com will point to the Github Pages.

## II) Cool, how can I contribute then? ##

To come to that, a short introduction on the website structure is needed. Each page you see on the [Github Pages website](https://rookiehpc.github.io) is built from a `data.json` file containing the page content. Anything related to layout or formatting is handled from the script `/rk.js`. Any modification you will want to bring to a page's content will therefore come down to modifying the corresponding `data.json` file. Contributing then becomes as simple as submitting a pull request for that file and that's it! :D

### II.1) Alright, I see a page I want to modify, how can I do it? ###
<img src="https://github.com/rookiehpc/rookiehpc.github.io/blob/main/images/EditThisPageLink.png" width="300">

At the bottom of every page you will see an "Edit this page" link, clicking on it will open the page editor.

<img src="https://github.com/rookiehpc/rookiehpc.github.io/blob/main/images/LivePreview.png" width="700">

Modifying the JSON record on the left handside instantly updates the preview on the right handside. This allows to quickly modify the content of a page and see what it looks like. Once you are happy with the edits, submit a pull request for the corresponding `data.json` file with the new JSON record you have just written! :)

### II.2) How do I know the JSON attributes that I must set or modify? ###

In the [Wiki](https://github.com/rookiehpc/rookiehpc.github.io/wiki/JSON-structure) of this repository you will find the explanation, along with examples, of the JSON structure used behind the scenes.

### II.3) How do I know the location of the corresponding original data.json file? ###

If you are using the page editor on an existing page, the location of the corresponding `data.json` file is displayed in the blue ruban at the bottom.

### II.4) What if I want to create a new page, such as a new documentation entry for instance? ###

Although not all categories below are implemented yet, this is the structure that will likely be followed for when you create a new `data.json` file:

Page | Repository location | Example
-|-|-
Homepage | `/data.json` | -
Privacy policy | `/privacy_policy/data.json` | -
Technology "T" homepage | `/T/data.json` | `/mpi/data.json`
Technology "T" about page | `/T/about/data.json` | `/mpi/about/data.json`
Technology "T" documentation entry "E" | `/T/docs/E/data.json` | `/mpi/docs/mpi_comm_size/data.json`
Technology "T" tool entry "E" | `/T/tools/E/data.json` | `/mpi/tools/my_super_tool/data.json`
Technology "T" exercise entry "E" | `/T/exercises/E/data.json` | `/mpi/tools/my_super_exercise/data.json`

The example codes are stored verbatim in their corresponding programming language file format. For instance, the example in `C` for `MPI_Comm_size` is located at `/mpi/docs/mpi_comm_size/example_1.c`. The `FORTRAN-90` have the extension `.f90` while their `FORTRAN-2008` counterpart have `.f08`.

## III) How do I notify of a missing documentation entry, a bug and so on? ##

Simply [submit an issue](https://github.com/rookiehpc/rookiehpc.github.io/issues) and it will be picked up when possible :) You will even be able to track its progress!
