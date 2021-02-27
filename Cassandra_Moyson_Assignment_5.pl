% -----------------------------------------------------------------
% Cassandra Moyson - 0539918 - 1M Computer Science VUB
% This file contains the code that builds the system described in
% assignment 5.
% -----------------------------------------------------------------
%
% ------------------
%      REMARKS
% ------------------
%
% The code given in this file doesn't implements everything the
% assignment asked for. When stacking the boxes, it doesn't take into
% account the optimal order of delivery. So the boxes are stacked in
% such a way that the heavier boxes are never on top of the lighter
% boxes, but they aren't arranged to get the shortest route. The route
% calculated for the boxes is always the same: from position 8 -> 1 (see
% lower for the meaning of positions of the cakes). This still ensures
% that the boxes on the bottom aren't taken out first, but it isn't the
% shortest delivery route per se. I did leave the code I used to try and
% get the shortest delivery route to show the logic I tried.
%

% ----------------------------
% LOADING FILES AND LIBRARIES
% ----------------------------
%
%
% This project uses predicates and facts from other files. These are
% loaded below.

:- ['DCarcs','DCnodes','DCmodel','DCtypes'].


% Load useful built-in libraries
:- use_module([library(dcg/basics),
               library(clpfd),
               library(pairs),
               library(lists),
               library(apply)
              ]).

% Load dialect sicstus
:- expects_dialect( sicstus ).


% -------------------------------
%       DATA STRUCTURE
% -------------------------------

% cake( order_number, type, address_node)
% The address_node was found by using the nearest/3 predicate defined by
% DCModel.pl. The coordinates of the delivery locations were found using
% Google Maps.

:- block cake( -, -, - ).


cake( 1, 1 , 4430 ). % Smithsionian Institution Offices: 38.88698N, 77.02139W
cake( 2, 3 , 4718 ). %Supreme Court of the United States: 38.89134N, 77.00452W
cake( 3, 2 , 4865 ). %Capitol Skyline Hotel: 38.87904N, 77.00970W
cake( 4, 3 , 7036 ). %Ted's Bulletin Restaurant: 38.88332N, 76.99544W
cake( 5, 1 , 4786 ). %The Wharf Marina: 38.87795N, 77.02175W
cake( 6, 2 , 4737 ). %The International Spy Museum: 38.88410N, 77.02546W
cake( 7, 1 , 4764 ). % Wang Accounting Services: 38.88257N, 77.02192W
cake( 8, 2 , 4773 ). % St Dominic Church: 38.88394N, 77.02058W
cake( 9, 3 , 4474 ). % Voice of America Radio Station: 38.88831N, 77.01651W
cake( 10, 2 , 4705 ). % Rayburn House Office Building: 38.88734N, 77.01051W
cake( 11, 3 , 4715 ). % Captitol Hill Club: 38.88675N, 77.00554W
cake( 12, 2, 3882 ). % Founding Farmers DC Restaurant: 38.90105N, 77.04438W


% ----------------------------------
%         CAKE STACKING
% ----------------------------------

% Stacking the cakes must be done in such a way that the heavier
% boxes are at the bottom and the lighter boxes are at the top. The
% trailer is devided into 4 stacks, with each two cakes. Depending on
% where in the trailer the cakes are placed, they get a different number
% assigned.

% Bottom layer:
% ---------
% | 1 | 3 |
% ---------
% | 5 | 7 |
% ---------

% Top layer:
% ---------
% | 2 | 4 |
% ---------
% | 6 | 8 |
% ---------




% constrain_cakes/2 contrains the phyical properties of individual
% cakes.

constrain_cakes( [], [], [] ).
constrain_cakes( [Cake| Cakes], [Type, Stack, Position| Variables], [cake(Cake, Type, AddressNode, Stack, Position)| CakeList] ) :-
    cake(Cake, Type, AddressNode),
    Type in 1 .. 3,
    Stack in 1 .. 4,
    Position in 1 .. 8,
    (Position #= Stack * 2) #\/ (Position #= Stack * 2 - 1),
    constrain_cakes( Cakes, Variables, CakeList ).


% link_cakes/2 contrains the relationships between cakes. Normally, it
% would've been here that I linked the cakes in such a way that the
% cycle would be minimal. I left the code that I would've executed in
% the comments. The way I tried gave me errors called
% "representation_error(int)" that I was not able to solve, but I leave
% it in to show the logic behind it.

link_cakes( [] ).
link_cakes( [_] ).
link_cakes( [cake( _, Type1, _ , Stack1, Position1 ), cake( Cake2, Type2, Address2, Stack2, Position2 )| Cakes] ) :-
    % link_cakes would've had an extra argument that accumulates the distance. The 3 lines below would've been added.
    % when( (nonvar(Address1), nonvar(Address2)), distance(Address1, Address2, Distance)),
    % RoundedDistance is round(Distance * 1000),
    % NewDistance is RoundedDistance + TotalDistance,
    Stack2 #>= Stack1,
    Stack2 #=< Stack1 + 1,
    ( Stack1 #= Stack2 ) #==> (Type1 #>= Type2),
    ( Stack1 #= Stack2 ) #==> (Position1 #= Position2 - 1),
    link_cakes( [cake( Cake2, Type2, Address2, Stack2, Position2)| Cakes]).


% Link functions together

store_cakes( CakeList ):-
    length( CakeList, 8 ),
    length( OrderNumbers, 8 ),
    all_different( OrderNumbers ),
    constrain_cakes( OrderNumbers, Variables, CakeList),
    link_cakes( CakeList ),
    labeling( [ffc], Variables ).
    % To get the smallest distance, I would've use min(Distance).
    %labeling( [ffc, min(Distance)], Variables ).


% ---------------------------------------------
% SHORTEST PATH BETWEEN TWO NODES: A* ALGORITHM
% ---------------------------------------------


% For search, the A* algorithm is used, which uses a weighted graph. The
% nodes of the graph correspond to the nodes given in DCnodes.pl and
% the edges between the nodes will be determined using the
% arc_either_way_and_type/2 predicate. The weights
% on the edges are equal to the distance between the nodes. The distance
% between two nodes will be caluclated using the predicate distance/2
% defined in DCmodel.pl.
%
% A* works with the update function f(n) = g(n) + h(n), or for each node
% applies that A* score = the cost of the path so far + the heuristic
% cost. As the heuristic for each node, I will use the bird's eye view
% distance (calculated by distance/2) between the goal node and that
% node.
%
% This code follows the way the YouTube video in the following link has
% explained the algorithm. (https://www.youtube.com/watch?v=6TsL96NAZCo&)
%
% An extra constraint has been added to this A* search, the route is not
% allowed to go on all types of roads. After looking at the DCtypes.pl
% and the description given in the assignment, I have made the decision
% that bikes are only allowed to go on roads 41 to 48, 60 to 65 and 70
% to 74.

% arc_either_way_and_type/3 checks whether there is an arc between two
% nodes and returns the type of this arc (based on arc_either_way/2 of
% DCmodel.pl).
arc_either_way_and_type( A, B, Type ) :-
	arc( A, B, _, _, Type ).
arc_either_way_and_type( A, B, Type ) :-
	arc( B, A, _, _, Type ).


% heuristic_cost/2 calculates the heuristic cost of a node
heuristic_cost(GoalNode, Node, Heuristic):-
    distance(GoalNode, Node, Heuristic).


% shortest_path/4 returns the shortest Route with cost TotalCost from
% StartNode to GoalNode
shortest_path(StartNode, GoalNode, TotalCost, Route):-
    heuristic_cost(GoalNode, StartNode, HeuristicCost),

    % A* algorithm uses a priority queue of sorts to store nodes and their A* scores and I will use a list for this purpose.
    % After each iteration of the search algorithm the list will be sorted on the lowest A* score to simulated the behaviour of a priority queue.
    % Th queue stores a quadruple for each node:
    % (A* score of node, the node, the current path cost, nodes in the path untill that point).

    % First parameter list represents the queue and now gives the starting situation of the A* algorithm, the rest of the queue is empty.
    % So far, we only have the heuristic cost of the StartNode, the StartNode,
    % the cost of the path is still equal to zero and the visited node is the StartNode.
    % astar_search/4 performs the A* search algorithm.
    astar_search([(HeuristicCost, StartNode, 0 , [StartNode])], GoalNode, TotalCost, Route).


% We reach to end of the algorithm, the the path with lowest cost is now in front.
astar_search([(_ , _,  TotalCost,[GoalNode|Route] )|_], GoalNode, TotalCost, FinalRoute):-
    % The list is build up from GoalNode to StartNode, so it needs to be reversed.
    reverse([GoalNode|Route], FinalRoute).


% Actual search part of the algorithm.
astar_search([LowestAStarScore|OldCPaths], GoalNode, TotalCost, ReverseFinal):-
    % Calculate new A* score for the child nodes of the lowest A* score node
    expand_child_nodes(LowestAStarScore, GoalNode, ChildNodePaths ),
    % Update the queue that hold the A* scores
    append_or_replace( ChildNodePaths, OldCPaths, NewCPaths, [] ),
    % Sort on the lowest A* score, the path with lowest the A* score will be expanded in the next iteration
    sort( NewCPaths , SortedCPaths),
    % Continue the search
    astar_search(SortedCPaths, GoalNode, TotalCost, ReverseFinal).


% expands_child_nodes/2 expands all child nodes of the given node.
expand_child_nodes(( _, A, CurrentPathCost, Visited), GoalNode, ChildNodePaths):-
    % For each child node, their own quadruple will be calculated and put in ChildNodePaths
    findall(% The quadruple
            (AStarScoreNext, Next, NewCurrentPathCost , [ Next |Visited ]),
           (
               % Find the next node and the type of the edge in the possible path
               arc_either_way_and_type(A, Next, Type),
               % Calculate the distance between the previous and next node (weight on edge)
               distance(A, Next, Distance),
               % Check whether the next node has already been visited
               \+ member(Next, Visited),
               % Check if the type of the edge is right
               ((Type in 41 .. 48) #\/ (Type in 60 .. 65)) #\/ (Type in 70 .. 74),
               % Calculate the current cost of the path
               NewCurrentPathCost is CurrentPathCost + Distance,
               % Calculate the heuristic cost of the next node
               heuristic_cost(GoalNode, Next, HeuristicCostNext),
               % Calculate the A* score (A* score = current path cost + heuristic cost)
               AStarScoreNext is HeuristicCostNext + NewCurrentPathCost
            ),
           ChildNodePaths).


% After the child nodes have been expanded, there is
% a list of quadruples for each child node. These
% have to be added to the queue. append_or_replace/2 (using dcg) updates
% the queue that keeps all the quadruplets. There are four cases.
%
% Case 1: the end of the list of quadruplets of the child nodes has
% been reached. The queue has been updated.
append_or_replace([], AStarList) --> [], AStarList.


% Case 2: The child node is not part of the queue and it is appended to
% the queue.
append_or_replace([(AStarScoreChild, Child, PathCost, Path)|RestAStarList], AStarList) -->
    {\+ member((_, Child, _, _), AStarList),
    append([(AStarScoreChild, Child, PathCost, Path)], AStarList, NewAStarList)},
    append_or_replace(RestAStarList, NewAStarList).


% Case 3: The child node is already part of the queue, but the node in
% the queue had a higher A* score. The one in the queue will be replaced
% by the lower A* score.
append_or_replace([(AStarScoreChild, Child, PathCost, Path)|RestAStarList], AStarList) -->
    {member((_, Child, _, _), AStarList),
    select((OldAStarChild, Child, _, _), AStarList, WithoutAStarList),
    AStarScoreChild < OldAStarChild,
    append([(AStarScoreChild, Child, PathCost, Path)], WithoutAStarList, NewAstarList)},
    append_or_replace(RestAStarList, NewAstarList).


% Case 4: The child node is already part of the queue, but the node in
% the queue has a lower A* score. The one in the queue will be left
% alone, higher A* score will be ignored.
append_or_replace([(AStarScoreChild, Child, _, _)|RestAStarList], AStarList) -->
    {member((_, Child, _, _), AStarList),
    select((OldAStarChild, Child, OldPathCost, OldPath), AStarList, WithoutAStarList),
    AStarScoreChild >= OldAStarChild,
    append([(OldAStarChild, Child, OldPathCost, OldPath)], WithoutAStarList, NewAstarList)},
    append_or_replace(RestAStarList, NewAstarList).



% ---------------------------------------------
%               MAKING A CIRCUIT
% ---------------------------------------------
%
% This portion of the code is responsible for making the shortest
% possible curcuit between an starting node and a list of nodes. Two
% nodes in the circuit are connected using the A* algorithm, so the cost
% of the circuit will be minimal for those points.

% remove_head/2 returns the tail of a list
remove_head([_|Tail], Tail).


% find_delivery_route/2 finds a delivery route, given a StartNode and a
% ListOfNodes and gives the Cost and Route.
find_delivery_route(StartNode, ListOfNodes, Cost, Route):-
    % Check if the length of the address nodes list is 8 (for the 8 deliveries)
    length(ListOfNodes, 8),
    % The startnode and the address nodes all have to be different
    all_distinct([StartNode|ListOfNodes]),

    % find_circuit/2 makes a circuit from the start node, through a list of nodes
    % (in the order of the nodes in that list) and back to the start node and calculates the cost and the path.
    find_circuit(StartNode, StartNode, ListOfNodes, Cost, Route).


% End of the address nodes, the list is empty
find_circuit(_, _, [], _, _).

% The end of the address nodes list is reached, a path is made back to
% the startnode so the circuit is complete.
find_circuit(RealStartNode, AStarStartNode, [HeadNode], TotalCostCircuit, CircuitRoute):-
    shortest_path(AStarStartNode, HeadNode, Cost1, Route1),
    shortest_path(HeadNode, RealStartNode, Cost2, Route2),
    TotalCostCircuit is Cost1 + Cost2,
    remove_head(Route2, Route2NoHead),
    append(Route1, Route2NoHead, CircuitRoute).

% Append the nodes with shortest_path and add the cost.
find_circuit(RealStartNode, AStarStartNode, [HeadNode|RestNodes], TotalCostCircuit, CircuitRoute):-
    shortest_path(AStarStartNode, HeadNode, Cost, Route),
    find_circuit(RealStartNode, HeadNode, RestNodes, Cost2, Route2),
    TotalCostCircuit is Cost + Cost2,
    remove_head(Route2, Route2NoHead),
    append(Route, Route2NoHead, CircuitRoute).


% ----------------------------------------
%        LINKING IT ALL TOGETHER
% ----------------------------------------

% Now that we can make a circuit given the address nodes and
% we can check whether the given list is a valid stacking, I link both
% of these predicates together.

% address_from_stack/2 gives the address of a cake
address_from_stack(cake(_, _, A, _, _), A).

% get_address/2 get address nodes of a list of cakes
get_address( [], End ) --> [], End.
get_address( [H|T], AddressList ) --> { address_from_stack(H, N) }, get_address(T, [N|AddressList]).


% give_optimal_delivery_route/4 links the stacking and making a circuit
give_optimal_delivery_route( CakeList, AddressNodes, Cost, Path ) :-
    % Get the stacking of the cakes
    store_cakes( CakeList ),
    % Get the address nodes of the cakes
    get_address( CakeList, [], AddressNodes, [] ),
    % Make a cycle from the bakery node (955) through the address list
    find_delivery_route( 955 , AddressNodes , Cost, Path ).


% ----------------------------------
%     PRINTING THE INSTRUCTIONS
% ----------------------------------


% To print the instructions of the system, I use definite clause
% grammars (DCG). start([],[]) will start the whole system.

start --> {format("Starting the system. Welcome!~n", [])}, calculating.
calculating --> { format("The route is being calculated.~n", [])}, cakestacking.
cakestacking --> {
    give_optimal_delivery_route( CakeList, AddressNodes, Cost, Route ),
    nth1(1, CakeList, Cake1),
    nth1(2, CakeList, Cake2),
    nth1(3, CakeList, Cake3),
    nth1(4, CakeList, Cake4),
    nth1(5, CakeList, Cake5),
    nth1(6, CakeList, Cake6),
    nth1(7, CakeList, Cake7),
    nth1(8, CakeList, Cake8),
    format("-----~n",[]),
    format("The following is the way you should stack the cakes (cake(order number, type of cake, address node, stack number, position)):~n",[]),
    format("Top:     ~w | ~w | ~w | ~w |~n", [Cake2, Cake4, Cake6, Cake8]),
    format("Bottom:  ~w | ~w | ~w | ~w |~n",[Cake1, Cake3, Cake5, Cake7]),
    format("They will be delivered in the following order:~n", []),
    format(" ~w -> ~w -> ~w -> ~w -> ~w -> ~w -> ~w -> ~w ~n",[Cake8, Cake7, Cake6, Cake5, Cake4, Cake3, Cake2, Cake1])

},
    instructions( Cost, AddressNodes, Route).

instructions( Cost, AddressNodes, [Bakery|Route]) --> {
    format("-----~n",[]),
    format("STARTING INSTRUCTIONS~n",[]),
    format("Start from ~d,~n", [Bakery])
    },
    give_instruction( Route, AddressNodes, Cost ).

% give_instruction is used to loop over the path of the circuit.
give_instruction([], [], Cost) --> [], {format("We are back at the bakery. End of delivery!~n", [])}, cost(Cost).
% Clause for when the node is a delivery node
give_instruction([H|T], [HeadAddress|TailAddress], Cost) -->
    {H =:= HeadAddress,
     cake(OrderNumber, Type, HeadAddress),
    format("Go to ~d. You are at the delivery destination of ~w.~n", [HeadAddress, cake(OrderNumber, Type, HeadAddress)])},
    give_instruction( T, TailAddress, Cost).
give_instruction([H|T], AddressNodes, Cost) --> {format("Go to ~d,~n", [H])}, give_instruction(T, AddressNodes, Cost).

cost(Cost) --> {format("-----~n",[]),
    format("TOTAL COST: ~f~n", [Cost])
      }, shutting_down.

shutting_down --> { format("-----~n",[]),
                    format("Shutting down the system. Bye!",[])
                  }.


% ----------------------------------
%   RUNNING THE SYSTEM AND EXAMPLE
% ----------------------------------
%
% The query below starts the system and will print out the instructions.
%
% ?- start([],[]).
%
% Starting the system. Welcome!
% The route is being calculated.
% -----
% The following is the way you should stack the cakes (cake(order number, type of cake, address node, stack number, position)):
% Top: cake(1,1,4430,1,2) | cake(7,1,4764,2,4) | cake(6,2,4737,3,6) | cake(12,2,3882,4,8) |
% Bottom: cake(10,2,4705,1,1) | cake(5,1,4786,2,3) | cake(3,2,4865,3,5) |cake(8,2,4773,4,7) |
% They will be delivered in the following order:
% cake(12,2,3882,4,8) -> cake(8,2,4773,4,7) -> cake(6,2,4737,3,6) ->cake(3,2,4865,3,5) -> cake(7,1,4764,2,4) -> cake(5,1,4786,2,3) ->
% cake(1,1,4430,1,2) -> cake(10,2,4705,1,1)
% -----
% STARTING INSTRUCTIONS
% Start from 955,
% Go to 956,
% Go to 4791,
% Go to 4770,
% Go to 4751,
% Go to 4763,
% Go to 4764,
% Go to 4760,
% Go to 4741,
% Go to 4758,
% Go to 4757,
% Go to 4756,
% Go to 4462,
% Go to 4430,
% Go to 4427,
% Go to 4460,
% Go to 4424,
% Go to 4445,
% Go to 4440,
% Go to 4417,
% Go to 4412,
% Go to 4404,
% Go to 4401,
% Go to 4400,
% Go to 4181,
% Go to 4175,
% Go to 4168,
% Go to 4155,
% Go to 4153,
% Go to 4094,
% Go to 4090,
% Go to 4043,
% Go to 4041,
% Go to 4032,
% Go to 4030,
% Go to 3899,
% Go to 3897,
% Go to 3882. You are at the delivery destination of cake(12,2,3882).
% Go to 3897,
% Go to 3899,
% Go to 4030,
% Go to 4032,
% Go to 4041,
% Go to 4043,
% Go to 4090,
% Go to 4094,
% Go to 4153,
% Go to 4155,
% Go to 4168,
% Go to 4175,
% Go to 4181,
% Go to 4400,
% Go to 4401,
% Go to 4404,
% Go to 4412,
% Go to 4417,
% Go to 4440,
% Go to 4445,
% Go to 4424,
% Go to 4460,
% Go to 4427,
% Go to 4430,
% Go to 4462,
% Go to 4756,
% Go to 4757,
% Go to 4758,
% Go to 4759,
% Go to 4761,
% Go to 4773. You are at the delivery destination of cake(8,2,4773).
% Go to 4761,
% Go to 4741,
% Go to 4738,
% Go to 4736,
% Go to 4737. You are at the delivery destination of cake(6,2,4737).
% Go to 9367,
% Go to 4733,
% Go to 4742,
% Go to 4749,
% Go to 4748,
% Go to 4751,
% Go to 4770,
% Go to 4781,
% Go to 4782,
% Go to 4785,
% Go to 4842,
% Go to 4851,
% Go to 4838,
% Go to 4863,
% Go to 4865. You are at the delivery destination of cake(3,2,4865).
% Go to 4863,
% Go to 4838,
% Go to 4851,
% Go to 4842,
% Go to 4785,
% Go to 4782,
% Go to 4765,
% Go to 4767,
% Go to 4764. You are at the delivery destination of cake(7,1,4764).
% Go to 4763,
% Go to 4751,
% Go to 4769,
% Go to 4744,
% Go to 4786. You are at the delivery destination of cake(5,1,4786).
% Go to 4744,
% Go to 4769,
% Go to 4751,
% Go to 4763,
% Go to 4764,
% Go to 4760,
% Go to 4741,
% Go to 4758,
% Go to 4757,
% Go to 4756,
% Go to 4462,
% Go to 4430. You are at the delivery destination of cake(1,1,4430).
% Go to 4462,
% Go to 4471,
% Go to 4477,
% Go to 4479,
% Go to 4640,
% Go to 4651,
% Go to 4655,
% Go to 4653,
% Go to 4656,
% Go to 4661,
% Go to 4705. You are at the delivery destination of cake(10,2,4705).
% Go to 4661,
% Go to 4656,
% Go to 4653,
% Go to 4655,
% Go to 4651,
% Go to 4640,
% Go to 4479,
% Go to 4477,
% Go to 4478,
% Go to 4699,
% Go to 4775,
% Go to 4777,
% Go to 4772,
% Go to 4774,
% Go to 4779,
% Go to 4767,
% Go to 4765,
% Go to 4782,
% Go to 4781,
% Go to 4770,
% Go to 4791,
% Go to 956,
% Go to 955,
% We are back at the bakery. End of delivery!
% -----
% TOTAL COST: 17.050584
% -----
% Shutting down the system. Bye!


