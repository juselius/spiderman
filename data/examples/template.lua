-- -*- lua -*-
-- vim:ft=lua:et:ts=4
--
--
local pkg = {}

pkg.name = "foo"
pkg.version = "1.0"
pkg.id = pathJoin(pkg.name, pkg.version)
pkg.prefix = pathJoin(sitePkgRoot, pkg.id)
pkg.display_name = "Foo"
pkg.help = [[ 
Foobar
========
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse tempor
tortor sed nibh auctor varius. Duis convallis tincidunt lorem eget vehicula.
Aenean eu diam felis.
]] -- reStructuredText 

help(pkg.help)
whatis("Name: " .. pkg.display_name)
whatis("Version: " .. pkg.version)
whatis("Category: Application")  -- Application, Development, Library
whatis("Keyword: ")              -- Compiler, Chemistry, ...
whatis("URL: http://www.lipsum.com/")
whatis("License: ")
whatis("Description: ")

setenv("FOO_ROOT", pkg.prefix) 

prepend_path("PATH", pathJoin(pkg.prefix, "bin"))
prepend_path("LD_LIBRARY_PATH", pathJoin(pkg.prefix, "lib"))
append_path("MANPATH", pathJoin(pkg.prefix, "man"))

