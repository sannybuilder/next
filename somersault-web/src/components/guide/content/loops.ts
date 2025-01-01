export default `
// Loops allow executing the code multiple times. Currently the while loop is supported:

int player = 0
while is_player_playing(player)
    add_score(player, 5)
end

// A while loop is executed while the condition is true. The condition can be any valid logical expression:

while is_player_playing(player) and is_button_pressed(0, 4) 
    add_score(player, 5)
end

// Loops support break and continue statements:

while true
    if is_button_pressed(0, 4) then
        break
    else
        wait(0)
        continue
    end
end
`;
