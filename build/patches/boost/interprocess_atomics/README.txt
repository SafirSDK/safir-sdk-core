Problem:
The atomic operations in boost interprocess lack compiler memory barriers.
We have only observed the problem with atomic_write32 on Linux x86 and x86_64
(with GCC). However, as one of the references below indicate there may be a
problem with the atomic_read32 as well.

The following operations are considered suspect:
Windows x86/x86_64: atomic_read32
Linux x86/x86_64: atomic_read32 and atomic_write32
Linux arm: atomic_read32 and atomic_write32

References:
http://boost.2283326.n4.nabble.com/interprocess-atomic-write32-td4173289.html
http://boost.2283326.n4.nabble.com/interprocess-atomic-write32-on-GCC-x86-x68-64-lacks-memory-barrier-td4666165.html

Fixed in:
1.57

Applies to:
All versions.

Comment:
The patch adds sequential consistency to the atomic_write32 and atomic_read32
operations. This might be "too strict", but at least it is safe.
