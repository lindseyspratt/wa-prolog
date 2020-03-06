# wa-prolog
WebAssembly-based Prolog.
A Prolog for running in Web browsers with a WebAssembly WAM.

The implementation of wa-prolog is directly based on the book 
"Warren's Abstract Machine: A Tutorial Introduction" by 
Hassan Aït-Kaci (hak@cs.sfu.ca).
The text for this book plus a collection of errata can be found at
https://github.com/a-yiorgos/wambook .

The directory src/tutorial contains a direct implementation of the WAM
tutorial in WebAssembly and Javascript.

The src/engine directory contains the WAM implementation of WA-Prolog.
It is an extension of the WAM tutorial implementation.