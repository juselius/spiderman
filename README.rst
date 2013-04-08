SpiderMan
==========

Convert Lmod's ``spider -o softwarePage`` JSON into HTML pages. 

Features
~~~~~~~~

* Filter output on category and keywords
* Renders HTML via easily customizable `StringTemplates
  <http://www.stringtemplate.org/>`_
* Generate reStructuredText output
* Generate ``.page`` rst files for `gitit <http://gitit.net>`_ 
* Generate MetaDoc software page xml 

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

