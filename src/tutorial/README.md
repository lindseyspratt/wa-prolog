# wa-prolog
WebAssembly-based Prolog.
A Prolog for running in Web browsers with a WebAssembly WAM.

The implementation of wa-prolog is directly based on the book 
"Warren's Abstract Machine: A Tutorial Introduction" by 
Hassan Aït-Kaci (hak@cs.sfu.ca).
The text for this book plus a collection of errata can be found at
https://github.com/a-yiorgos/wambook .

The WebAssembly implementation of the WAM is in src/wam.
There are several versions of the WAM presented in the tutorial,
each one presenting a particular feature of the WAM.
These are represented in this project by src/wam/languageN.wat,
where N varies between 0 and 3e.

Versions 0 through 3 present the core of the WAM.
Versions 3a through 3e include various essential extensions to this 
'core' WAM necessary to support a usable Prolog.
The 3e version include all of the basic features of the WAM including
indexing and cuts.

A certain amount of Javascript is necessary to run this WebAssembly WAM.
The Javascript utilities used by language3X.wat are in 
src/program/utilties.X.wam.js, for X one of 'a', 'b', 'c', 'd', or 'e'.

There are WAM programs (sequences of WAM instructions) for testing defined
in call.X.wam.js and nrev.X.wam.js (where X indicates the corresponding
language3X.wat and utilities.X.wam.js).
The call.X.wam.js programs are based on examples from the tutorial.
The nrev.X.wam.js program implements the nrev/2 Prolog program and
gives a standard (if very rough) performance benchmark in
'LIPS' (Logical Inferences Per Second).

Example HTML files to run simple tests (often examples from the tutorial)
are test_wamN.html with the same variations for as above for N:
0 through 3 and 3a through 3e.
There are tests for running nrev/1, nrev_wamX.html where X is 3a through 3e.
Also  there are tests for running the tutorial 'call/1' example: 
call_wam3d.html and call_wam3e.html.

There is a collection of tests to investigate WebAssembly in src/tutorial/test.
These are evaluated by src/tutorial/test_tutorial.html.
