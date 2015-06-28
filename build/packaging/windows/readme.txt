Safir SDK Core
==============

Safir SDK Core is a middleware and platform for creation of distributed soft real-time
systems. It is Scalable, Reliable, Portable, and last but not least, it is Open!

Safir SDK Core is based on modern architectural principles and has a solid foundation in
more than 20 years of development of distributed systems at Saab.

Windows Binary Installer
------------------------

Since you are reading this document you have probably just installed Safir SDK Core for
Windows using the binary installer provided by us. This README contains a bit of
information to get you started.

Licensing
---------

Safir SDK Core is available under GNU GPL v3 (GNU General Public License) - a copy of
which can be found in COPYING.txt - or under a commercial license from Saab AB.

The GPL license means that you are free to try out or modify the software to your hearts
content and create your own applications on top of it. But you are not allowed to
distribute the modified software or your applications (which will classify as derivative
works) without releasing your code under the GPL license too.

If you want to distribute proprietary binaries you need to obtain a commercial license
from Saab AB (contact information can be found at http://www.saabgroup.com).

Prerequisites
-------------

To be able to use the SDK that you have just installed there are a few pieces of software
that you need to have installed on your machine.

For developing Safir SDK Core applications you need:

* Python 2.7 or later (https://www.python.org/downloads/)
* CMake 3.0 or later (http://www.cmake.org/download/)
* Microsoft Visual Studio

For the Safir SDK Core Runtime (i.e. just running applications that are built on Safir
SDK Core) you need:

 * Microsoft Visual C++ Redistributable (http://support.microsoft.com/kb/2019667).
 * Microsoft .NET Framework 4 or later

Note that the version of Visual Studio has to match the build of Safir SDK Core that you
install. So, if you're using Visual Studio 2013, make sure you're using the VS2013 build
of Safir SDK Core.

Documentation
-------------

Documentation for Safir SDK Core can be found at http://www.safirsdkcore.com/docs.

Go to http://www.safirsdkcore.com to find out about our Google+ Community, our GitHub and
SourceForge pages and to get in touch with us!

Bundled Software
----------------

The Safir SDK Core windows installer comes with some bundled software, that is used
internally and that is used in our interfaces:

* Boost C++ Libraries (http://www.boost.org) - All headers and a subset of the Boost
  libraries are bundled.
* Qt runtime (http://www.qt.io) - used by dobmake and dobexplorer.
* Ninja (https://github.com/martine/ninja/releases) - Build system that can run in
  parallel.

License terms for these can be found at their respective web sites, but in short there
are no surprises there, nothing that should affect your ability to use Safir SDK Core
under either the GPLv3 license or the commercial license.
