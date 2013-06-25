Instructions how to create test cases
--------------------------------------
All tests shall be contained in separate folders.

The name of the folder must be on the format: 
    <TestNumber>.<TestName>[-ExpectedErrorId]
Where:
    TestNumber - just a number, tests will be run in order starting from 1.
    TestName - Name describing what is tested.
    ExpectedErrorId - If the test is supposed to fail, this is the expected value returned by ParseError.ErrorId().

Example: 
    "123.TestEnumTypes" - A test that is supposed to succeed.
    "124.TestMoreEnumTypes-64" - A test that is supposed to generate a ParseError with ErrorId 64.

    
         
