Doc's Notebook Service
================

This is the source code for the "Doc's Notebook" service from [FAUST CTF 2022](https://2022.faustctf.net).

**As it was written for a CTF service, the code is deliberately insecure and contains exploitable bugs. It
is provided for educational purposes only, do not even think about (re-) using it for anything productive!**

The code is released under the ISC License, see LICENSE.txt for details.

----

Doc's notebook is a written in Emacs Lisp.

It features a web editor that exports documents Org-Mode documents as HTML. For an overview of Org-Mode see [orgmode.org](https://orgmode.org/)

Vulnerability
--------
Org mode allows lisp expressions in table formulas. See [exploit/exploit.py](exploit/exploit.py) for an example
