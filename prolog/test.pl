% Make a group of tests with begin_tests/end_tests.
% Make a test with test/2.
% Run your tests with run_tests/0.

% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.

% Examples follow; please remove them in your project.

:- use_module(src).
:- use_module(api).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert_to_words tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests('convert_to_words').

test('base case, empty list', [nondet]) :-
    convert_to_words([], []).

test('singleton list', [nondet]) :-
    convert_to_words(
        [json([word=chow, score=41896, numSyllables=1, tags=[n]])],
        [word(chow, 1, [n])]).

test('more complex list', [nondet]) :-
    convert_to_words(
        [json([word=chow      , score=41896 , numSyllables=1 , tags=[n]])  , 
         json([word=pewpew    , score=15697 , numSyllables=2 , tags=[v]])  , 
         json([word=power     , score=34798 , numSyllables=2 , tags=[adj]])  , 
         json([word=chocolate , score=23199 , numSyllables=3 , tags=[n]])  , 
         json([word=doggy     , score=02290 , numSyllables=2 , tags=[n]])] , 
        [word(chow, 1, [n]), word(pewpew, 2, [v]), word(power, 2, [adj]), word(chocolate, 3, [n]), word(doggy, 2, [n])]).

:- end_tests('convert_to_words').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% random_word_syllable tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests('random_word_syllable').

test('base case - empty words to choose from', [nondet]) :-
    random_word_syllable("N/A", [], 2, n),
    random_word_syllable("N/A", [], 0, n).

test('should find a random word with corresponding syllables - 2 syllables', [nondet]) :-
    random_word_syllable(
        Word,
        [
            word(sport, 1, [n]),
            word(matchup, 2, [n]),
            word(upper, 2, [n]),
            word(walker, 2, [n]),
            word(wedge, 1, [n]),
            word(backgammon, 3, [n])
        ],
        2,
        n),
    member(Word, [matchup, upper, walker]).

test('should find a random word with corresponding syllables - 3 syllables', [nondet]) :-
    random_word_syllable(
        Word,
        [
            word(sport, 1, [n]),
            word(matchup, 2, [n]),
            word(upper, 2, [n]),
            word(walker, 2, [n]),
            word(wedge, 1, [n]),
            word(backgammon, 3, [n])
        ],
        3,
        n),
    member(Word, [backgammon]).

test('should find a random word with corresponding syllables and parts of speech', [nondet]) :-
    random_word_syllable(
        Word,
        [
            word(sport, 1, [n]),
            word(matchup, 2, [v]),
            word(upper, 2, [v]),
            word(walker, 2, [n]),
            word(wedge, 1, [n]),
            word(backgammon, 3, [n])
        ],
        2,
        v),
    member(Word, [matchup, upper]).

test('should not fail if no words found with number of syllables', [nondet]) :-
    random_word_syllable(
        "N/A",
        [
            word(sport, 1, [n]),
            word(matchup, 2, [n]),
            word(upper, 2, [n]),
            word(walker, 2, [n]),
            word(wedge, 1, [n]),
            word(backgammon, 3, [n])
        ],
        200,
        n).

:- end_tests('random_word_syllable').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pick_random_word tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests('pick_random_word').

test('base case - empty words to choose from', [nondet]) :-
    pick_random_word("N/A", [], []).

test('Should correctly find word', [nondet]) :-
    TestList = [a,b,c,d,e,f],
    pick_random_word(X, TestList, TestList),
    member(X, TestList).

test('Should choose word from second argument if first empty', [nondet]) :-
    TestList = [a,b,c,d,e,f],
    pick_random_word(X, [], TestList),
    member(X, TestList).

test('fail if word not from List', [fail]) :-
    TestList = [a,b,c,d,e,f],
    pick_random_word(hello, TestList, TestList).

:- end_tests('pick_random_word').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utility predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests('util_predicates').

test('same_syllable should fail', [fail]) :-
    same_syllable(3, word(hi, 2, [n])).

test('same_parts_of_speech should fail', [fail]) :-
    same_parts_of_speech(v, word(hi, 2, [n])).

test('get_value should fail', [fail]) :-
    get_value(word(hi, 2, [n]), hello).

test('same_syllable should be true if same syllable', [nondet]) :-
    same_syllable(2, word(hi, 2, [n])).

test('same_parts_of_speech should be true if same parts of speech', [nondet]) :-
    same_parts_of_speech(n, word(hi, 2, [n])).

test('get_value should get value of word', [nondet]) :-
    get_value(word(hi, 2, [n]), hi).

:- end_tests('util_predicates').

/*
Tests provided to us from project template - keeping here for future reference
*/

% :- begin_tests('member').

% test('base case, singleton list', [nondet]) :-
%     member(1, [1]).

% test('base case, longer list', [nondet]) :-
%     member(x, [x, y, z]).

% test('recursive case', [nondet]) :-
%     member(y, [x, y, z]),
%     member(z, [x, y, z]).

% test('multiple results', [nondet, all(X =@= [x, y, z])]) :-
%     member(X, [x, y, z]).

% test('multiple but not all results', [nondet, all(X =@= [a, c])]) :-
%     member(pair(X, 1), [pair(a, 1), pair(b, 2), pair(c, 1)]).

% test('a failing test', [fail]) :-
%     member(not_there, [x, y, z]).

% test('some more failing tests', [fail]) :-
%     ( member(w, [x, y, z]) ;
%       member(3, []) ;
%       member(3, [1, 8, 2, 9, something_with_3_inside(3), [3]]) ).

% :- end_tests('member').
