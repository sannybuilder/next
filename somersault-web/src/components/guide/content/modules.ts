export default `

// CLEO 5 modules are pre-compiled files that export functions. To create a module, define a function and add the 'export' keyword:

export function circle_area(radius: float): float
    return 3.14159 * radius * radius
end


// You can then save this file under any name (e.g. area.mod) and call in a Sanny Builder script:

/*
// Sanny Builder 4 example:
// Learn more: https://docs.sannybuilder.com/language/import-export

import circle_area from "area.mod" // see an example module above
float area = circle_area(5.0)



*/

// Note that unlike other functions, exported functions have limited space for local variables (31 in total, including input arguments):

export function foo
    int vars[31]
    // can't declare more variables here
end

`;