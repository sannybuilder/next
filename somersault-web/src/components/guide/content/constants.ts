export default `
// Constants are used to store values that never change. They are declared using the 'const' keyword:

const SECONDS_IN_A_DAY = 24 * 60 * 60
const PI = 3.14159
const GAME_TITLE = "San Andreas"

// Constants can be used in expressions and functions:

function calculate_circle_area(radius: float): float
    return PI * radius * radius
end

// Constants are only available to the code following the declaration:

// wait(delay) // 'delay' const is not available on this line
const delay = 100
wait(delay) // OK

// Constants can be redeclared in different functions:

function one
    const num = 5
end

function two
    const num = 10
end

// Compiler supports constants 'true' and 'false', representing '1' and '0', respectively:

while true
    wait(0)
end

`;
