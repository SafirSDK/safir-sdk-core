There appear to be two different bugs that cause flushing to fail.

This one is patched by the file.hpp fix in build/patches/boost/file
https://svn.boost.org/trac/boost/ticket/2998

This one we do not provide a patch for ATM, since it does not affect any of the supported boost versions (the ones supported in lars's branch).
https://svn.boost.org/trac/boost/ticket/4590
