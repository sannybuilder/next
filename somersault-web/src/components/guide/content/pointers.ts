export default `
// Pointers are variables that store the memory address of another variable. They are used to access and manipulate data in memory:

// pint32 type creates a pointer to a 32-bit value in memory

int y = 5
pint32 x = get_var_pointer(y) // x is a pointer to variable x

// To access the value at the memory address pointed to by a pointer, use the index operator []:

int value = x[0]
x[0] = 10

// Pointer arithmetic can be used to access adjacent memory locations:

int next_value = x[1] // read the value with the offset 4 * 1
x[1] = 20
`;
