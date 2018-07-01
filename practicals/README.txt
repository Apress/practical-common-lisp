This directory contains the source code for _Practical Common Lisp_. You may use
and redestribute this software the terms of the license in file LICENSE.

The code is designed to be loaded with Another System Definition Facility (ASDF)
and each chapter directory contains its own ASD file. You can either add the
name of each ChapterXX directory to ASDF:*CENTRAL-REGISTRY* or you can create
symlinks to the ASD files in those directories in a directory that is already
named in ASDF:*CENTRAL-REGISTRY*. If you also add this directory to the central
registry or create a symlink to the file practicals.asd, you can then load all
the practicals code by typing:

  (asdf:oos 'asdf:load-op :practicals)

at the REPL. You can also load the code for individual chapters by loading the
system of the same name as the ASD file in each ChapterXX directory.

  ./Chapter03/simple-database.asd
  ./Chapter08/macro-utilities.asd
  ./Chapter09/test-framework.asd
  ./Chapter15/pathnames.asd
  ./Chapter23/spam.asd
  ./Chapter24/binary-data.asd
  ./Chapter25/id3v2.asd
  ./Chapter26/url-function.asd
  ./Chapter27/mp3-database.asd
  ./Chapter28/shoutcast.asd
  ./Chapter29/mp3-browser.asd
  ./Chapter31/html.asd

Thus to load the test framework code from Chapter 9, you'd type:

  (asdf:oos 'asdf:load-op :test-framework)

at the REPL. (Note that Chapter31/ contains the code for both Chapters 30 and
31.)

--Peter Seibel
