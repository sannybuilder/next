export default `

// CLEO 5 modules are pre-compiled files that export functions. To create a module, define a function and add the 'export' keyword:

export function circle_area(radius: float): float
    return 3.14159 * radius * radius
end


// You can then save this file under any name (e.g. area.s) and call in a Sanny Builder script:

/*
// Sanny Builder 4 example:
// Learn more: https://docs.sannybuilder.com/language/import-export

import circle_area from "area.s" // see an example module above
float area = circle_area(5.0)



*/


`;