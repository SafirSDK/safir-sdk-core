Problem:
A bug in Boost.Containers can cause double deallocation of a memory block. In particular
this affects our shared memory usage, since that uses Boost.Interprocess which in turn
uses Boost.Containers.

Affected platforms:
All

References:
https://github.com/SafirSDK/safir-sdk-core/issues/196
http://lists.boost.org/Archives/boost/2014/09/216727.php

Fixed in:
1.57

Applies to:
1.53 - 1.56

Comments:
None
