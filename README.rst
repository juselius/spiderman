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

Quick start
------------

Make sure you have Haskell Platform installed:

* On Ubuntu: ``$ sudo apt-get install haskell-platform`` 
* On Fedora: ``$ sudo yum install haskell-platform`` 
* Otherwise go to http://www.haskell.org/platform

Install the prerequisites::

    $ cabal install --only-dependencies

Run SiderMan::

    $ runhaskell SpiderMan.hs --help
    $ ./data/examples/runspider.sh /path/to/modulefiles/{Core,Apps,...} > modules.json
    $ runhaskell SpiderMan.hs modules.json
    $ xdg-open modules/index.html

If you want to nicely formatted pages, you need to install the files in 
data/css, together with the generated output on a web server.

Building 
---------

For better performance and faster startup you might want to compile and
install spiderman (see the cabal documentation for the details)::

    ($ cabal install --only-dependencies)
    $ cabal configure
    $ cabal build
    $ cabal install

Disclaimer
~~~~~~~~~~~

This program should probably have been written in Lua or Python. But I needed
to learn Haskell, so it's written in Haskell. Complaints are accepted in
hardcopy only. Besides, Haskell is a fantastic language.

