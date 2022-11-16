:- module(src,[start/0, print_haiku/1, pick_random_word/3, random_word_syllable/4, same_syllable/2, same_parts_of_speech/2, get_value/2]).
:- use_module(api).
:- use_module(library(readutil)).


%%%%%% START SCREEN %%%%%%

% intro_prompt(S) is true if S is the intro prompt for our app
intro_prompt(" ~~ Paiku ~~.\nYour number one haiku generator!.\nPlease provide 1-5 topics in comma separated form (i.e. soccer,dot,monster)").

% start/0 calls the starting Paiku UI.
start :- intro_prompt(Prompt), writeln(Prompt), read_line_to_string(current_input, Topics), print_haiku(Topics). 

/*
Credits to: https://swish.swi-prolog.org/p/playing_with_wordnet.swinb
*/
% print_haiku/1 prints the haiku from the user specified topics
print_haiku(Topics) :-
    haiku_lines(Topics, Lines),
    format("~w~n~w~n~w~n", Lines).

/*
Credits to: https://swish.swi-prolog.org/p/playing_with_wordnet.swinb
*/
haiku_lines(Topics, [Line1, Line2, Line3]) :-
    get_words(Topics, Words),
    line1(Line1, Words, 5),
    line2(Line2, Words, 7),
    line3(Line3, Words, 5).

% generates a line in a haiku with 5 or 7 number of syllables

line1(Line, Words, 5) :- 
    random_word_syllable(W1, Words, 3, n),
    random_word_syllable(W2, Words, 2, adv),
    atomic_list_concat([W1, W2], ' ', Line).

line2(Line, Words, 7) :- 
    random_word_syllable(W1, Words, 2, v),
    random_word_syllable(W2, Words, 1, adj),
    random_word_syllable(W3, Words, 3, n),
    random_word_syllable(W4, Words, 1, u),
    atomic_list_concat([W1, W2, W3, W4], ' ', Line).

line3(Line, Words, 5) :- 
    random_word_syllable(W1, Words, 1, v),
    random_word_syllable(W2, Words, 2, n),
    random_word_syllable(W3, Words, 2, adj),
    atomic_list_concat([W1, W2, W3], ' ', Line).

same_syllable(N, word(_, N, _)).
same_parts_of_speech(PoS, word(_, _, Type)) :- member(PoS, Type).
get_value(word(Value, _, _), Value).

% Picks a random word from Words that has N syllables and the specified parts of speech
/*
1. Filter words that have N syllables, store in Out1
2. Filter Out1 for words that have the specified PoS, store in Out2
3. Get the value of the words from Out2 (these are words that have N syllables and the PoS)
4. Get the value of the words from Out1 (these are words that have N syllables)
5. Pick a random word.
*/
random_word_syllable(Word, Words, N, PoS) :-
    include(same_syllable(N), Words, Out1),
    include(same_parts_of_speech(PoS), Out1, Out2),
    maplist(get_value, Out2, SamePoSAndSyllableWords),
    maplist(get_value, Out1, SameSyllableWords),
    pick_random_word(Word, SamePoSAndSyllableWords, SameSyllableWords).

% pick_random_word(Word, SamePoSAndSyllableWords, SameSyllableWords) chooses a random Word from SamePoSAndSyllableWords
% if there are no parts of speech words, we choose from SameSyllableWords
pick_random_word("N/A", [], []).
pick_random_word(Word, [], SameSyllableWords) :- random_member(Word, SameSyllableWords).
pick_random_word(Word, SamePoSAndSyllableWords, _) :- random_member(Word, SamePoSAndSyllableWords).
