export default `
// Expressions are combinations of operators and operands that produce a value

int x = 5
int y = 10
int z = x + y // z = 15

// Operators are symbols that perform operations on operands

int a = 5 + 3 // addition
int b = 5 - 3 // subtraction
int c = 5 * 3 // multiplication
int d = 5 / 3 // division

// Same can be done with float numbers

float e = 5.0 + 3.0 // addition
float f = 5.0 - 3.0 // subtraction
float g = 5.0 * 3.0 // multiplication
float h = 5.0 / 3.0 // division

// Expressions can be combined with parentheses to control the order of operations:

int i = (5 + 3) * 2 // i = 16

// Function arguments are expressions:

wait(1000 * 10)

// Result of expressions can be returned from functions:

function sum(a: int, b: int): int
    return a + b
end
`