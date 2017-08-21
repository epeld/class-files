
# A Common Lisp JVM Class-File Parser

Hi. This project is a Java .class-file disassembler in the making.


## Starting It
Right now you have to start everything manually...

First you need to build it. Look in `package.lisp` to see which dependencies
are needed. Load the dependencies, then load the source files, i.e all the `.lisp-files`
(probably in the order they appear in package.lisp)

Finally, once you've loaded `server.lisp` you can runi `(start-server)` which gives
you a webserver running on `http://localhost:8080/home`. Upload a .jar-file and
browse away!

## Work In Progress
The application currently supports browsing known classes, seeing their methods, fields and byte code.

In the future, it will be possible to generate .dot-graphs from the classes (with graphs of both method calls and class-dependencies), *and* to translate the byte code to something more human-readable.
