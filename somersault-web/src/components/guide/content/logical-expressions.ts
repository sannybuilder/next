export default `
// Logical expressions are expressions that can be used in conditional statements to control the flow of the program:

int x = 5
int y = 10

if x < y then
    print_help_formatted("x is less than y")
end

if x > y then
    print_help_formatted("x is greater than y")
end

if x == y then
    print_help_formatted("x is equal to y")
end

if x <> y then
    print_help_formatted("x is not equal to y")
end

if x <= y then
    print_help_formatted("x is less than or equal to y")
end

if x >= y then
    print_help_formatted("x is greater than or equal to y")
end

// You can also prefix logical expressions with the NOT operator to negate the result:

if not (x < y) then
    print_help_formatted("x is not less than y")
end


// AND/OR operators can be used to combine multiple logical expressions:
if x > 0 and y > 0 then
    print_help_formatted("a and b are positive")
end

// AND/OR operators can also be used to select between two expressions:

x = x or y // x will be equal to y if x is 0, otherwise it will be equal to x
x = x and y // x will be equal to x if x is 0, otherwise it will be equal to y

`