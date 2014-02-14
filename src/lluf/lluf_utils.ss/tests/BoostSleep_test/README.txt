This is not really a test for any lluf functionality. It is meant to see if we can reproduce a bug in boost::this_thread::sleep.
The bug is that sleep appears to sometimes return immediately instead of sleeping, and sometimes it seems to be uninterruptible.
