% game board size: 24x24
% top left = (0,0)

% rooms with boundries (name, topleft-x,y, bottomright-x,y) 
room(kitchen, (0,0), (5,5)).
room(ballroom, (8,0), (15,6)).
room(conservatory, (18,0), (23,4)).
room(billiard_room, (18,7), (23,11)).
room(library, (17,13), (23,17)).
room(study, (17,20), (23,23)).
room(dining_room, (0,8), (7,14)).
room(lounge, (0,18), (6,23)).
room(hall, (9,17), (14,23)).
room(center, (10,9), (14, 15)).

% room entrances- x,y coordinates
entrance(kitchen, (4, 5)).
entrance(ballroom, (8, 4)).
entrance(ballroom, (9, 6)).
entrance(ballroom, (14, 6)).
entrance(ballroom, (15, 4)).
entrance(conservatory, (18, 4)).
entrance(billiard_room, (18, 8)).
entrance(billiard_room, (22, 11)).
entrance(library, (17, 15)).
entrance(library, (20, 13)).
entrance(study, (17, 20)).
entrance(hall, (14, 19)).
entrance(hall, (12, 17)).
entrance(hall, (11, 17)).
entrance(lounge, (6, 18)).
entrance(dining_room, (6, 9)).
entrance(dining_room, (7, 12)).


% secret paths
passage(kitchen, study).
passage(study, kitchen).
passage(conservatory, lounge).
passage(lounge, conservatory).

% characters
character(miss_scarlett).
character(colonel_mustard).
character(mrs_white).
character(reverend_green).
character(mrs_peacock).
character(professor_plum).

% weapons
weapon(candlestick).
weapon(dagger).
weapon(lead_pipe).
weapon(revolver).
weapon(rope).
weapon(wrench).

% print board
% make sure position is within 24x24 play space
valid_position((X, Y)) :- X >= 0, Y =< 23, Y >= 0, X =< 23.

% determine if current position in room
in_room(Pos, Room) :-
  room(Room, (X1, Y1), (X2, Y2)),
  Pos = (X, Y),
  X >= X1, X =< X2,
  Y >= Y1, Y =< Y2.

% hallway position if is a valid position and not in a room
is_hallway(Pos) :- valid_position(Pos), \+ in_room(Pos, _).

is_entrace(Pos) :- entrance(_, Pos).

% given position print cell type
print_cell(X, Y) :-
  Pos = (X, Y),
  ( is_entrace(Pos) -> write('E ')
  ; in_room(Pos, kitchen) -> write('K ')
  ; in_room(Pos, ballroom) -> write('Ba')
  ; in_room(Pos, conservatory) -> write('C ')
  ; in_room(Pos, billiard_room) -> write('Bi')
  ; in_room(Pos, library) -> write('Li')
  ; in_room(Pos, study) -> write('S ')
  ; in_room(Pos, hall) -> write('H ')
  ; in_room(Pos, lounge) -> write('Lo')
  ; in_room(Pos, dining_room) -> write('Di')
  ; in_room(Pos, center) -> write('C ')
  ; is_hallway(Pos) -> write('x ')
  ; write('  ')
  ).

print_row(Y) :- forall(between(0, 23, X), print_cell(X, Y)), nl.
print_board :- forall(between(0, 23, Y), print_row(Y)).