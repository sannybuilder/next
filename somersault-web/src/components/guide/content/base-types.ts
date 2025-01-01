export default `
// Any language has a set of basic types to represent data. In Somersault, there are three basic types: integers, floats, and strings:

// Integers
-1
0
101

// Floats
0.0
-1.123
-100.5

// Strings
"test"
'test'

// Style of quotes ("" vs '') don't affect the final result. Strings are limited to 16 characters and not null-terminated by default. To add a null-terminator use \\0
"test\\0"
'test\\0'

// Also notice the error to the right. This is due to the fact this particular code example is not a valid (complete) script.
`;