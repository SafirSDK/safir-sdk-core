Problem:
atomic_write32 and atomic_read32 in boost interprocess on x86/x86_64 and GCC have no memory barriers.

Affected platforms:
Linux x86/x86_64

References:
http://boost.2283326.n4.nabble.com/interprocess-atomic-write32-td4173289.html
http://boost.2283326.n4.nabble.com/interprocess-atomic-write32-on-GCC-x86-x68-64-lacks-memory-barrier-td4666165.html

Fixed in:
-

Applies to:
All versions.

Comment:
The patch adds sequential consistency to the atomic_write32 and atomic_read32 operations. This might be "too strict", but at least it should be safe.

