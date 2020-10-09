# cs30

This is a practice environment for people who want to learn about
topics in Discrete Mathematics (students). This tool works as a
web-application, so students interact with it in a browser.

### Help wanted! Edit this file...
If you know what this tool is about, perhaps you can add a bit
to the description here.

If you would like to get started with this environment locally
(on your own computer) you can run cs30 in single user mode.
That will set up a simple (non-configurable) web-server and
direct you to use the application from there.

There is also a Canvas mode available through 'LTI'.

## Installation in Single user mode

Prerequisites: stack, and a browser. Please install stack using the
instructions on the Haskell Stack / GHC Downloads / Haskell Platform
websites, and don't try using the .sh script included here.

The install_stack_foreign.sh script is a workaround to try to install
stack in case no sudo rights are available. Even if it work (and this
is not a given) it will lead to some problems with your stack
installation if you use it for other projects.

Installation instructions:

* Run 'stack exec serve --package cs30:serve'

  After a while (first time is slower) you should see the message:

  ```console
  Please direct your browser to http://localhost:3000/
  ```

* Direct your browser to http://localhost:3000/
  
  A note of warning: this single user mode uses no storage. You can
  abort the script at any time by pressing ctrl-c, at which point
  all session-data will be lost. At the time of writing, this only
  includes a history of what questions you answered and how well
  you did on them, which is used to ask you harder questions if you
  did well, or restart on the easier questions if you did poorly.

## Installation with Canvas integration

This isn't really supported, so you may need to reach out to
Sebastiaan to get help here.

Currently, 'make' will test whether it is running on Sebastiaan's
computter. If it is, it will synchronise the repository with an
online server and then build and install the executables there.
The server is running Apache with php and cgi-scripts enabled.
There is a folder that acts as storage. This folder is mentioned in
the file data/dir and may be relative to the cgi-script's path.
(Using an absolute path is probabaly easier to set up, though)

Finally, the data folder contains some cryptographic random data:
'salt', which should be kept secret, and a file with a random
name and random content, which is to be comunicated with Canvas.

In Canvas, a manual custom tool is to be added with the key set
to the filename and the secret set to the content of the file.
To select a specific exercise, like 'Roster', add ?Roster to the url.

## For developers, running and testing

Avoid using the above instructions, use this instead:
  
  ```console
  stack run serve
  ```

This builds everything, which should give you a larger overhead once,
but it will also alert you if you make breaking changes to the cgi part.

To run the test-suite:
  
```console
stack test
```

### Contributing code

Please for the repository, make your changes there,
add your name to the CONTRIBUTORS.md file, and send me a pull.

