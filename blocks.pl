%----------------------------------------------------------------------
% Christopher Stickle
% Blocks World
% CSC 366 - Computiational Models of Cognitive Processes
%----------------------------------------------------------------------

%%%%%%%%%%%%%
%%% Model %%%
%%%%%%%%%%%%%

block(ly).
block(sy).
block(lr).
block(sr).
block(lb).
block(sb).
block(lg).
block(sg).

hasColor(ly, yellow).
hasColor(sy, yellow).
hasColor(lr, red).
hasColor(sr, red).
hasColor(lb, blue).
hasColor(sb, blue).
hasColor(lg, green).
hasColor(sg, green).

hasSize(ly, large).
hasSize(sy, small).
hasSize(lb, large).
hasSize(sb, small).
hasSize(lr, large).
hasSize(sr, small).
hasSize(lg, large).
hasSize(sg, small).

makeWorld(Configuration) :- Configuration = 
   [hand(),
    stack([ly,table]), 
    stack([sy,table]),
    stack([sr,table]),
    stack([lr,table]),
    stack([lb,table]),
    stack([sb,table]),
    stack([lg,table]),
    stack([sg,table])].

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitive Actions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

pickUp(Block, OldState, NewState) :- 
    delete(OldState, stack([Block, table]), Intermediate),
    replace(Intermediate, stack([Block|Rest]), stack(Rest), Intermediate2), 
    replace(Intermediate2, hand(), hand(Block), NewState).
    
% Put Block on top of On (another block) in the OldState to generate NewState.
% 1) Put on table.
put(Block, table, OldState, NewState) :- 
    OldState = [hand(Block)|Stacks], NewState = [hand(),stack([Block,table])|Stacks].
% 2) Put on existing block.
put(Block, On, OldState, NewState) :- 
    replace(OldState, stack([On|Rest]), stack([Block,On|Rest]), IS), 
    replace(IS, hand(Block), hand(), NewState).


%%%%%%%%%%%%%%%
%%% Actions %%%
%%%%%%%%%%%%%%%

perform(pickup, Block, OldState, NewState) :- pickUp(Block, OldState, NewState).
perform(put, Block, On, OldState, NewState) :- put(Block, On, OldState, NewState).
perform(stack, Block, On, OldState, NewState) :-
    perform(pickup, Block, OldState, Intermediate),
    perform(put, Block, On, Intermediate, NewState).


%%%%%%%%%%%%%%%%%%
%%% Validation %%%
%%%%%%%%%%%%%%%%%%

% A block is movable if it has nothing on top of it
% or
% A block is movable if its in the hand.
movable(Block, World) :- (World = [Stack | _], Stack = stack([Block | _])) ; member(hand(Block), World), !.
movable(Block, World) :- World = [_ | Rest], movable(Block, Rest).

% Block is movable, add 1.
movable_count([Block|MoreBlocks], State, Count, [Block|MovableBlocks]) :-
    movable(Block, State),
    movable_count(MoreBlocks, State, NewCt, MovableBlocks), Count is NewCt + 1.
% Block is not movable, add 0
movable_count([Block|MoreBlocks], State, Count, MovableBlocks) :-
    \+ movable(Block, State),
    movable_count(MoreBlocks, State, NewCt, MovableBlocks), Count = NewCt.
% Base cases
movable_count([Block], State, 1, [Block]) :-
    movable(Block, State).
movable_count([Block], State, 0, []) :-
    \+ movable(Block, State).

emptyHand(World) :- World = [hand()|_].

% It's legal to perform a pickup if the Block is movable and hand is empty.
validate(pickup, Block, State) :- movable(Block, State), emptyHand(State).
% Error cases
% pickup:
% the block is the table
validate(pickup, table, _) :-
    write("You're not strong enough to do that."),nl,!,fail.
% the hand is not empty
validate(pickup, _, State) :-
    \+ emptyHand(State), write("The hand is not empty."),nl,!,fail.
% the block has something on top of it
validate(pickup, Block, State) :-
    \+ movable(Block, State), write("The block has something on top of it."),nl,!,fail.
% Validate Pickup block from something
validate(pickup, Block, From, State) :-
    validate(pickup, Block, State),
    member(stack([Block, From | _ ]), State).

% Validate put block on something
validate(put, Block, On, State) :-
    member(hand(Block), State),
    movable(Block, State),
    (member(stack([On|_]), State); On = table).

% Validate stack block on something
validate(stack, Block, On, State) :-
    validate(pickup, Block, State),
    (member(stack([On|_]), State); On = table).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Natural Language Generation %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
Initial Sate should display like this:

Hand: Empty
[T][LY]
[T][SY]
[T][SR]
[T][LR]
[T][LB]
[T][SB]
[T][LG]
[T][SG]
*/
drawWorld(Block) :-
    hasColor(Block, Color), hasSize(Block, Size), write('['),
        (Size = large -> write('L');
         Size = small -> write('S')),
        (Color = yellow -> write('Y');
         Color = red -> write('R');
         Color = blue -> write('B');
         Color = green -> write('G')),
        write(']').
drawWorld(table) :- write('['), write('T'), write(']').
drawWorld(stack([H|T])) :- drawStack(stack([H|T])).
drawWorld(hand()) :- write("Hand: "), write('Empty'), nl.
drawWorld(hand(Block)) :- write("Hand: "), drawWorld(Block), nl.
drawWorld([]).
drawWorld([Object]) :- drawWorld(Object).
drawWorld([H|T]) :- drawWorld(H), drawWorld(T).

% draw the stack in reverse order
drawStack(stack([H|T])) :- drawStack(T), drawStack(H), nl.
drawStack([H|T]) :- drawStack(T), drawStack(H).
drawStack([]).
drawStack(table) :- drawWorld(table).
drawStack(Block) :- drawWorld(Block).

%%%%%%%%%%%
%%% NLP %%%
%%%%%%%%%%%
sentence(s(VP)) --> verb_phrase(VP), ['.'].
sentence(s(VP)) --> verb_phrase(VP).
noun_phrase(np(Det,Noun)) --> det(Det), noun(Noun).
noun_phrase(np(Det,AdjP,Noun)) --> det(Det), adj_phrase(AdjP), noun(Noun).
noun_phrase(np(Pron)) --> pron(Pron).
verb_phrase(vp(Verb,NP)) --> verb(Verb), noun_phrase(NP).
verb_phrase(vp(Verb,NP,PP)) --> verb(Verb), noun_phrase(NP), prep_phrase(PP).
prep_phrase(pp(Prep,NP)) --> prep(Prep), noun_phrase(NP).
adj_phrase(adjp(Adj)) --> adj(Adj).
adj_phrase(adjp(Adj,AdjP)) --> adj(Adj), adj_phrase(AdjP).
verb(v(put)) --> [put].
verb(v(pickup)) --> [pickup].
verb(v(stack)) --> [stack].
det(d(the)) --> [the].
adj(a(large)) --> [large].
adj(a(small)) --> [small].
adj(a(green)) --> [green].
adj(a(red)) --> [red].
adj(a(yellow)) --> [yellow].
adj(a(blue)) --> [blue].
noun(n(block)) --> [block].
noun(n(table)) --> [table].
pron(pron(it)) --> [it].
prep(prep(on)) --> [on].
prep(prep(from)) --> [from].

extract_np(s(vp(_,NP)), NP).
extract_np(s(vp(_,NP,pp(_,NP2))),NP,NP2).

extract_v(s(vp(Verb,_)),Verb).
extract_v(s(vp(Verb,_,_)), Verb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reference Resolution %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2-adjective case
resolve_reference(np(d(the), adjp(a(Size), adjp(a(Color))), n(block)), _, Block) :-
    hasSize(Block,Size), hasColor(Block,Color).

% 1-adjective case
resolve_reference(np(d(the), adjp(a(Color)), n(block)), State, Block) :-
    findall(B, hasColor(B, Color), Blocks), % Find all of the blocks with color Color
    movable_count(Blocks, State, _, [Block]). % Get the movable count, but really all
                                                  % we care about is the list is length 1.
resolve_reference(np(d(the), adjp(a(Size)), n(block)), State, Block) :-
    findall(B, hasSize(B, Size), Blocks), % Find all of the blocks with size Size
    movable_count(Blocks, State, _, [Block]). % Get the movable count, but really all
                                                  % we care about is the list is length 1.

% 0-adjective case
resolve_reference(np(d(the), n(block)), State, Block) :-
    member(hand(Block), State).
resolve_reference(np(d(the), n(table)), _, table).

% introduce stacking context
resolve_reference(np(d(the), adjp(a(Color1)), n(block)),
                  np(d(the), adjp(a(Color2)), n(block)), State, Block1, Block2) :-
    hasColor(Block1, Color1), hasColor(Block2, Color2),
    (member(stack([Block1, Block2|_]), State)).


%%%%%%%%%%%%%%%%%
%%% Main Loop %%%
%%%%%%%%%%%%%%%%%

extract_and_resolve(Parse, State, NewState) :-
    extract_v(Parse, Verb),
    extract_np(Parse, NP),
    resolve_reference(NP, State, Block),
    Verb = v(V),
    validate(V, Block, State),
    perform(V, Block, State, NewState).

extract_and_resolve(Parse, State, NewState) :-
    extract_v(Parse, Verb),
    extract_np(Parse, NP1, NP2),
    resolve_reference(NP1, State, Block1),
    resolve_reference(NP2, State, Block2),
    Verb = v(V),
    validate(V, Block1, Block2, State),
    perform(V, Block1, Block2, State, NewState).

extract_and_resolve(Parse, State, NewState) :-
    extract_v(Parse, Verb),
    extract_np(Parse, NP1, NP2),
    resolve_reference(NP1, NP2, State, Block1, Block2),
    Verb = v(V),
    validate(V, Block1, Block2, State),
    perform(V, Block1, State, NewState).

process_input(State) :-
    read_word_list(Input),
    (Input = ['exit'] -> write('Bye!'),nl,abort;
     Input = ['draw'] -> drawWorld(State),nl ,process_input(State);
     ( sentence(Parse, Input, []) -> ( extract_and_resolve(Parse, State, NewState),
       write('Currently...'), nl, nl, drawWorld(NewState), process_input(NewState));
       write('I don\'t understand...'), nl, process_input(State)));
       write("I'm not sure how to do that."), nl, process_input(State).

talktome :- makeWorld(W), write("Currently..."), nl, nl, drawWorld(W), nl, talktome(W).
talktome(W) :- write("What would you like to do next?"), nl, process_input(W).

%%%%%%%%%%%%%%%%%
%%% Utilities %%%
%%%%%%%%%%%%%%%%%

% Beginning of input list is the same as the thing we want to replace.
% Replace it with With in the output list.
replace([Replace|RestIn], Replace, With, [With|RestOut]) :-
    replace(RestIn, Replace, With, RestOut), !.
% Otherwise, just copy the FirstIn to the front of the output list.
replace([FirstIn|RestIn], Replace, With, [FirstIn|RestOut]) :-
    replace(RestIn, Replace, With, RestOut).
% When lists are empty, we're done (Base case).
replace([], _, _, []).

read_word_list(Ws) :-
    read_line_to_codes(user_input, Cs),
    atom_codes(A, Cs),
    tokenize_atom(A, Ws).

%%%%%%%%%%%%%%%
%%% Testing %%%
%%%%%%%%%%%%%%%

/*
extract_and_resolve_test(Parse, State) :-
        extract_v(Parse, Verb),
        extract_np(Parse, NP),
        resolve_reference(NP, State, Block),
        Verb = v(V),
        write(V), write(" "), write(Block).
extract_and_resolve_test(Parse, State) :-
        extract_v(Parse, Verb),
        extract_np(Parse, NP, NP2),
        resolve_reference(NP, State, Block),
        resolve_reference(NP2, State, Block2),
        Verb = v(V),
        write(V), write(" "), write(Block), write(" on "), write(Block2).

testit :- makeWorld(State),
        read_word_list(Input), sentence(Parse, Input, []),
        extract_and_resolve_test(Parse, State).
*/

% sentence(X, [pickup,the,red,block,from,the,green,block],[]).
% sentence(X, [pickup,the,red,block,from,the,green,block],[]), extract_np(X,NP1,NP2).
% sentence(X, [pickup,the,red,block,from,the,green,block],[]), extract_v(X,V).
% sentence(X, [pickup,the,red,block],[]), extract_v(X, V).
% sentence(X, [pickup,the,large,yellow,block],[]), extract_np(X,NP),resolve_reference(NP, Block).
% sentence(X, [pickup,the,small,yellow,block],[]), extract_np(X,NP),resolve_reference(NP, Block).

% makeWorld(X), put(lb, lr, X, Y).
% makeWorld(X), put(lb, table, X, Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Task 4 - extra credit %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

final_state(FinalState) :- FinalState =
    [
     stack([lr,lb,ly,table]),
     stack([sy,sr,lg,table]),
     stack([sg,sb,table])
    ].

% Build stack 1 at a time
% 1. isolate the 1st stack to be built
% 2. create a list of 'Steps' to build that stack from the bottom up
%    we can grab the list in the stack([List]) and revese List to get the steps
% 3. build the stack by performing each step in the list
% 4. repeat for the next stack
% 5. when all stacks are built, handle the hand(X) state, whatever that may be

/*
I worked for ages trying to get this to work as inteded, but I couldn't get it to work.
Specifically handling the hand() or hand(Block) state gave me isues. I tried too many things to
meningfully elaborate here, I would end up with all my world prints being correct exept for the last one.
or they would all be incorrect and blocks would slowly dissapper as the program ran.

In other scenarios I would get the correct output for the hand either empty or full but not the other

I ended up just leaving out the hand() all together in the finalstate declaration just to make sure
everything else ran as intended, which it does. I'm sure I was dancing around an answer but just not seeing it.

Some other thoughts on this prosses:
While this is maybe a resonable start there is conciderable room for improvement.
aside from fixing the hand issues mentioned above;
 1. I never check to see if moves are valid, I just assume they are.
 2. I never check to see if the final state matched the expected final state.
 3. I never check to see if the final state is a valid state.

 all these things humans would do in some cappacity and would be worth while improvments to
 my implemented heuristic.
*/


% reverse a list
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

% use reverse/3 to get the steps needed to build a stack and call buildStack/3 to build it
getStepsAndBuild(stack(List), W, NewW):-
    reverse(List, Steps, []),
    buildStack(Steps, W, NewW).

% Build the stack based on the steps given and draw the world after each step
buildStack([OnThis,PutThis|Rest], W, NewW):-
    perform(stack, PutThis, OnThis, W, IntermediateW),
    drawWorld(IntermediateW),nl,
    (Rest = [] -> NewW = IntermediateW;
     buildStack([PutThis|Rest], IntermediateW, NewW)).

% Build the stacks one at a time untill done
buildStacks([], _ ).
buildStacks([H|T],W):-
     getStepsAndBuild(H, W, IntermediateW),
     buildStacks(T, IntermediateW).

% Convience function
solve :- makeWorld(World), final_state(FinalState),
         buildStacks(FinalState, World),
         write("Finished!").


