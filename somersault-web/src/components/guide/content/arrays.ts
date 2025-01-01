export default `
// Arrays are groups of variables of the same type. They can be declared similar to variables:

int ids[10]

// Here 10 means a number of elements allocated in this array. To access each element use its index:

ids[0] = 1234
ids[9] = 555

// Accessing elements beyond the array size is not allowed. ids[10] will be an error.

// You can initialize the array when declaring it by giving each element an initial value:

float pos[3] = 100.0, -2140.12, -100.0

// You can use array elements just like any variable:

string names[2] = "Joe", 'Jim'
names[1] = names[0]

`;
