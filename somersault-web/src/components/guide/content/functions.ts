export default `
// Functions are the core of the language. Any command provided by the game can be called using the function syntax:

wait(0)
set_time_of_day(12, 34)

// Names of functions and list of arguments can be found in Sanny Builder Library at https://library.sannybuilder.com/

// You can define your own function using Sanny Builder 4 syntax:

function foo(arg1: int, arg2: float): string
end

// Function definitions can be placed anywhere in the code:

set_midnight()

function set_midnight
    set_time_of_day(0, 0)
end

// Functions may return values:

function get_world_center: float, float, float
    return 0.0, 0.0, 0.0
end

// Function's result can be stored in variables or an array, or ignored:

float pos[3] = get_world_center()
float x, y, z = get_world_center()
get_world_center()

`;
