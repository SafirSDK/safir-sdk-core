Problem:
Stuff in offset_ptr gets inlined with certain (newer) gcc versions and with clang++. This causes 
crashes in Release mode.

Affected platforms:
All where gcc or clang++ are used.

References:
http://lists.boost.org/Archives/boost/2011/06/182908.php
http://llvm.org/bugs/show_bug.cgi?id=10119

Fixed in:
1.48

Applies to:
1.35 - 1.47

Comment:
In one of the tickets above the author claims that it is fixed in 1.47, but looking at the code it is not fixed until 1.48.
