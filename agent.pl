:- [gamesetup].

/* Get number of real players */
agent_setup :-
  write('How many real players will be playing (1-6)? '), nl,
  read(NumPlayers),
  (NumPlayers > 0, NumPlayers =< 6 ->
    NumAgents is 6-NumPlayers,
    findall(Character, character(Character,_,_), CharacterList),
    init_agents(NumAgents, CharacterList)
  ;
    write('Incorrect input! Please enter a number between 1 and 6.'), nl,
    agent_setup
  ).


/* initialize agents */
:- dynamic agent/1.
init_agents(0, _). % remaining characters not played by real players will be agents
init_agents(NumberOfPlayers,[Char|Rest]) :-
  write('AI Player: '), write(Char), nl,
  assert(agent(Char)),
  NumPlayers is NumberOfPlayers-1,
  init_agents(NumPlayers, Rest).
