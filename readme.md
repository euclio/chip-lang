Welcome to ChipLang!
====================

Building the language
---------------------

ChipLang requires Java 7 and Scala to run.

To build the language, type

	scalac -d bin -cp src/resources src/*.scala src/chipLang/*.scala src/chipLang/syntax/*.scala

Running the language
--------------------

To run chipLang, type

	scala -cp bin ChipLang <.cl file>

There are five included example programs:
* percussion.cl
* control.cl
* prelude.cl
* crystalTower.cl
* marioBros.cl
