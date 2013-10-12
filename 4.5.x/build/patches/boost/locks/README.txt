Problem:
There is a conflict between Boost.Move and Boost.Thread which causes stuff using boost::upgrade_to_unique_lock to fail compilation.

Affected platforms:
All, probably. Only tested on ubuntu 12.04 (Precise)

References:
http://stackoverflow.com/questions/8259874/boost-1-48-0-upgrade-to-unique-lock-on-linux-has-something-changed-since-1-47-o
https://svn.boost.org/trac/boost/ticket/6141

Fixed in:
1.49

Applies to:
1.48

Comment:
The patch is the suggested patch from the stackoverflow.com post, not the one that the boost authors used in 1.49.
The reason for using this patch is that it is much less intrusive, and appears to solve the immediate problem,
whereas the "official" patch is more convoluted but fixes the problem in a more general way.
