export default `
// You can use IF conditions to decide which part of code to execute:

if 1 then
    wait(0)
end

// Else block will be executed when the condition is falsy:

if is_player_playing(0) then
    wait(0)
else
    terminate_this_script()
end

// Conditions can be negated by using NOT keyword in front of them:

if not is_player_playing(0) then
    terminate_this_script()
else
    wait(0)
end

// You can combine multiple checks using AND/OR operators. The conditions are lazily evaluated:

if 1 or is_player_playing(100) then 
    // is_player_playing(100) will not be executed because 1 makes the whole condition TRUE:
    wait(0)
end

if 0 and is_player_playing(100) then 
    // is_player_playing(100) will not be executed because 0 makes the whole condition FALSE:
    wait(0)
end

// You can combine as many conditions as you need using parenthesis:

if (is_player_playing(0) or is_player_playing(1)) and (is_player_playing(2) or is_player_playing(3)) then
    wait(0)
end
`;