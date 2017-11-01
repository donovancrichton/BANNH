# BANNH - A BackPropagating Artificial Neural Network in Haskell

This is the first project I ever wrote in Haskell, after spending a month or so
getting a feel for the language. Developed as part of the assessment for an
advanced topic in Functional Programming at Griffith University, as part of the
requirements for a Bachelor of Information Technology (Hons). This project
served as an introduction to Haskell, to the Functional Programming paradigm,
and to give me significant insight into the workings of simple artificial
neural networks (*nothing like building something from scratch to get a good
idea of how it works!*).

## Documentation
The build will create the written component of the assessment for the project,
however the code is well commented and I've tried to be as thorough as possible
explaining things.

## Dependencies
This project requires the following in order to build:
* GHC 8.02
* Latex
* Python 2.8 (for latex minted package)
* `ad` https://hackage.haskell.org/package/ad
This has only been tested on a linux machine running Ubutnu 16.04 however
there is no reason why it cannot be built and compiled on linux, windows and
mac.

## Outcome
The neural network trains in a reasonable time for small mathematical
functions, unfortunately even on a tiny image classification data set (60 x 1kb
images), the performance is atrocious. This was due in part to my use of the
`ad` library, much faster performance could be obtained by calculating the
derivatives manually. The other part was due to my inexperience with reasoning
about the run-time performance of lazy data structures, resulting in a large
amount of GC pause time.

## Contact
please contact me at donovan.crichton@griffithuni.edu.au if you have any
questions, concerns, or just want to say hi.



