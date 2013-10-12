Problem:
Interprocess stuff hangs at startup

Affected platforms:
All Windows versions.

References:
https://svn.boost.org/trac/boost/ticket/7936
https://svn.boost.org/trac/boost/ticket/5409
https://svn.boost.org/trac/boost/ticket/4010
among others (search for wmi on boost trac)

Fixed in:
1.48 and 1.49 but the problem reappears in 1.50, to be fixed again in 1.54

Applies to:
1.40 - 1.47 (maybe to versions before 1.40 as well)
1.50 - 1.53


Comment:
In one of the tickets above the author claims that it is fixed in 1.47, but looking at the code it is not fixed until 1.48.
The fix in 1.48 and 1.49 is about disabling wmi. In 1.50 there are changes to the wmi implementation and wmi is enabled again.
Unfortunately it also means that the hanging reappears, at least for our scenario with Win7 and SDK Core built for 64 bit. 
This last breakage appears to be fixed in 1.54.
