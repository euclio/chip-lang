Welcome to ChipLang!
====================

Building the language
---------------------

ChipLang requires Java 7 and Scala 2.10 to run.

To build the language, type

    mkdir bin
	scalac -d bin -cp resources src/chipLang/*.scala

Running the language
--------------------

To run chipLang, type

	scala -cp bin/ChipLang <.cl file>

There are five included example programs in the "example" folder:
* percussion.cl
* control.cl
* prelude.cl
* crystalTower.cl
* marioBros.cl
