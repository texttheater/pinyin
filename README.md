pinyin
======

Parsing and generating Hanyu Pinyin with SWI-Prolog

Documentation
-------------

Please refer to the module documentation at
http://www.swi-prolog.org/pack/file_details/pinyin/prolog/pinyin.pl

Installation
------------

Assuming that you have SWI-Prolog >= 6.3 with libarchive installed, you can
simply install this pack by entering the following at the SWI-Prolog prompt.

    ?- pack_install(pinyin).

Usage
-----

Then, to use the pinyin module in your program:

    :- use_module(library(pinyin)).
