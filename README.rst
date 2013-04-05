SpiderMan
==========

Convert Lmod's ``spider`` *softwarePage* JSON into HTML pages. 
SpiderMan can also:

* Filter output on category and keywords
* Generate rst
* Generate ``.page``-rst for `gitit <http://gitit.net>`_ (soon)
* Generate MetaDoc software page xml (soon)

Usage
------

::

  $ runhaskell SpiderMan.hs --help
  $ ./runspider.sh /path/to/modulefiles > modules.json
  $ runhaskell SpiderMan.sh modules.json
  $ xdg-open modules/index.html

Disclaimer
~~~~~~~~~~~

This program should probably have been written in Lua or Python. But I needed
to learn Haskell, so it's written in Haskell. Complaints are accepted in
hardcopy only.

