# cs30 [![Build Status](https://travis-ci.com/sjcjoosten/cs30.svg?branch=main)](https://travis-ci.com/sjcjoosten/cs30)

## What is this?

This is a practice environment for people who want to learn about
topics in Discrete Mathematics (students). This tool works as a
web-application, so students interact with it in a browser.

### What do the exercises look like?

There is an environment running here: https://cs.dartmouth.edu/~sjc/cs30/
It is fine to check out the existing exercises, but we do not recommend using this for your students:

- It is quite likely that some of the exercises are unpolished and not intended for students (intended for testing only).
  With Canvas integration, you can select which exercises students can see and complete.
- To set up Canvas integration, you'd need to add a key and secret to this environment, for which you need access to the server (which you don't have).
- There are many ways to loose progress:
  As you complete exercises correctly, questions get harder. This means there is a form of progress.
  If you refresh your browser, however, that progress may be lost.
  Even if you don't refresh your browser, the server might decide to clean up the directory where it stored your progress (Canvas integration prevents this). The chance of this happening is small if only a handful of people are using it, but becomes large if an entire class of students is using it.

### What is Canvas integration like?

There are two ways the environment can be embedded: as an external tool or as an assignment.

When the environment is embedded as a link to an external tool, students will see a single exercise which behaves largely the same as:
https://cs.dartmouth.edu/~sjc/cs30/?Roster
except that the possibility of choosing a different exercise is missing.

When the environment is embedded as an assignment, the following behavior occurs:
Students are required to complete a certain number of exercises correctly (usually 10, but this may differ per exercise).
As students complete exercises, they see their progress, e.g. '3/10'.
Upon making a mistake, the progress is reset.
Once the number is reached, students see the message that they completed the assignment, and a score of 100% of the available points will be awarded on Canvas.
If students navigate to the assignment after that, they will be able to continue practicing.
However, they will also continually see that this assignment is completed and no 'regrades' will be submitted to Canvas.

### Does this work with other LMS's, like Moodle?

This has not been tested yet, but we only used the standards that should be compatable with other LMS's.
This means that the most likely answer is no, but it shouldn't be hard to fix either.

## Installation

### In Single user mode

If you wish to develop exercises, or test this project offline, this is how.

Prerequisites: stack, and a browser. Please install stack using the
instructions on the Haskell Stack / GHC Downloads / Haskell Platform
websites, and don't try using the .sh script included here.

Installation instructions:

* Run 'stack run serve'

  After a while (first time is slower) you should see the message:

  ```console
  Please direct your browser to http://localhost:3000/
  ```

* Direct your browser to http://localhost:3000/
  
  A note of warning: this single user mode uses no storage. You can
  abort the script at any time by pressing ctrl-c, at which point
  all session-data will be lost.
  (If you are developing exercises, this is probably the behavior you want.)

### Installation with Canvas (or some other LMS) integration

This hasn't really happened on any LMS other than my own,
so you may need to reach out to me (Sebastiaan) to get help here.

The steps are as follows:

- setup a webserver with PHP and stack installed. This has been tested with Apache but other webservers might work too.
- fill the data directory with the following files:
  * data/salt (contains a random string)
  * data/dir (contains a path to a writable directory on your server)
  * data/chooseYourOwnFileName (filename is a key, contains a secret)
- build a cgi script called cs30.cgi using stack. The salt and dir files are statically built into the cgi script, so these two steps must be done in this order. The file chooseYourOwnFileName will be copied to a directory of stack's choosing and accessible to the application there.
- add the files in static to a webserver, and add the cgi script in the same folder as well
- add the server to Canvas as an external tool. Use chooseYourOwnFileName as key, and its content as value.
  The path to use is where you installed the static files (like https://cs.dartmouth.edu/~sjc/cs30/ )
- add assignments to Canvas and select 'external tool' as mode of submission.
  Use something like https://cs.dartmouth.edu/~sjc/cs30/?Roster as a path.

There is a Makefile that only works for Sebastiaan, so running it won't help you.
If you are interested in hosting this on your own server,
please let me (Sebastiaan) know about your situation (like what kind of server you have).
There are currently 0 (zero) potential users who would like me to make the above process easier.

## For developers / contributors:

If you wish to contribute ideas, please use the issue-tracker.
If you wish to edit this documentation, code, or anything else, please:

- Fork this repository (use the Fork button on Github)
- Make your changes there
- Create a pull-request (there's a button for that on Github too)

### Testing

To run the test-suite:
  
```console
stack test --ta ""
```

If you need to reproduce errors, put the relevant arguments between the "".
For example:

```console
stack test --ta "--quickcheck-replay=569579"
```

### Contributing code

Please for the repository, make your changes there,
add your name to the CONTRIBUTORS.md file, and send me a pull-request.

