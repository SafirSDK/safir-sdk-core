Problems:
1. Interprocess stuff hangs at startup for versions of boost up to 1.53
2. Access violations in singleton that uses shared memory in 1.54 and later.

Affected platforms:
All Windows versions.

References for problem 1:
https://svn.boost.org/trac/boost/ticket/7936
https://svn.boost.org/trac/boost/ticket/5409
https://svn.boost.org/trac/boost/ticket/4010
among others (search for wmi on boost trac)

References for problem 2:
https://svn.boost.org/trac/boost/ticket/9262

Problem 1 fixed in:
1.48 and 1.49 but the problem reappears in 1.50, to be fixed again in 1.54

Problem 2:
Ticket 9262 claims that this is fixed in 1.56 but testing with 1.56 indicates that under certain circumstances the access violation is still there.

Problem 1 applies to:
1.40 - 1.47 (maybe to versions before 1.40 as well)
1.50 - 1.53

Problem 2 applies to:
1.54 - 


Comment Problem 1:
In one of the tickets above the author claims that it is fixed in 1.47, but looking at the code it is not fixed until 1.48.
The fix in 1.48 and 1.49 is about disabling wmi. In 1.50 there are changes to the wmi implementation and wmi is enabled again.
Unfortunately it also means that the hanging reappears, at least for our scenario with Win7 and SDK Core built for 64 bit. 
This last breakage appears to be fixed in 1.54.

Comment Problem 2:
The fix for Problem 2 is the same as the fix for Problem 1 in 1.50 to 1.53, which is why this README refers to two problems.
