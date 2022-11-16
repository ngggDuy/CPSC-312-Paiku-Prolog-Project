# Paiku??? (play on prolog and haiku lol idk)

Using the robust pattern-matching and unification strengths of Prolog, our project will generate English haikus that will hopefully make semantic and grammatical sense.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Eric Kuo (58163288)
+ Duy Nguyen (95844189)

## Product Pitch
> _Unification—_  
>  _and backtracking predicates._  
>  _Throughout the seasons._  
<div style="text-align: right">
<sub><a href="https://www.metalevel.at/prolog/introduction">A Prolog Haiku</a> </sub>
</div>

Originating from Japan, a Haiku is a short form of poetry consisting of three phrases or lines, with five syllables in the first line, seven in the second, and five in the third. Traditionally, Haikus carry a theme centered around nature and seasons, however, modern iterations can be about anything. 

Our project, Paiku, will prompt a user for a set of topics surrounding the desired theme of their haiku and will have the ability to generate (and maybe read aloud!) semantically correct haikus.

One of Prolog’s biggest features is unification. As English grammar can be broken down into disparate components: nouns, verbs, adjectives, modifiers, determiners, and particles, unification allows us to carefully order and reconstruct these small units into a structured form that is a haiku. Although Paiku is obviously rudimentary and will not create haikus that have as much nuance as those written by poets, our project will create an opportunity for us to be exposed to:
- How computers can process and represent languages
- How the logic programming paradigm, compared to others, can help create a semantic model for representing haikus.
- How we can expand our semantic model to generate other poetic forms (e.g. nursery rhymes, sonnets, etc.)
- What APIs and packages can we leverage to collect a dataset of words and to accurately simulate poetic devices such as rhymes
- How we can incorporate natural language processing to generate syntactically improved haikus

These are some questions we were curious about when first learning Prolog and by combining our passion for poetry, we think larger audiences would also be curious how feasible this is with Prolog.

## Minimal Viable Project
In our proof-of-concept (POC), we employed various new language elements and concepts learned in class in order to explore the robustness and unique strengths of Prolog. With the basic functionality for Paiku down, we can focus more time on really leveraging the full capabilities of Prolog. We plan on refining the following concepts and incorporating the specified features:

### Expressing Grammar and Definite Clause Grammars (DCGs)

Our current POC parses and filters words by their Parts of Speech (PoS) identifier, but not to its full extent as each line in our haiku has a very limited grammatical structure. As such, we plan on using these identifiers to "place" words into their syntactically correct positions for each line in our haiku. This can be accomplished using [DCGs](https://en.wikipedia.org/wiki/Definite_clause_grammar) and leveraging Prolog's abilities to represent languages/grammar. As a result, we could be able to express more complex grammatical relationships in our haikus.

### APIs and error handling

Our POC gave us an opportunity to explore how HTTP requests are handled in Prolog. We used the [Datamuse API](https://www.datamuse.com/api/) to generate words and parse the metadata from them (e.g. number of syllables, parts of speech).

We also plan on exploring industry standards for error handling in Prolog. An area we want to focus on is handling situations where our API calls fail. Some ideas we want to learn are how to make our predicates more robust. For example, how we can propagate error messages from the API to the user, or perform some behaviour based on the error codes returned from the API.

### Extend parts of speech identifiers. 

The Datamuse API is used primarily for finding words relating to some topic. However, the API does not have much support for other PoS identifiers like prepositions, determiners, particles, and modifiers. In our MVP, we plan on manually adding words from these parts of speech categories so that the grammatical structure of our haiku's can be improved.

### Flashy Features - two features that we think could be fun and educative to explore:

We acknowledge that these features may not be feasible in the timeline we have, but are concepts we are curious about and plan on exploring even outside of CPSC 312.

NLP - some way to improve the accuracy of our Haiku by providing more powerful natural language processing methods that involve AI. This way, we could potentially introduce more complex language and poetic devices such as tonality, word sounds (i.e. plosives, sibilance, etc.), and word connotations.

Text to Speech - a neat fun feature that we could implement to make Paiku more interactive. Generated Haikus could be read aloud via some package or library. 

## Proof of Concept

At a high-level, our proof-of-concept focuses on expressing the 5-7-5 syllable structure of Haikus. We've separated our code into two main modules, [src.pl](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/poc/prolog/src.pl) (focuses on the core logic) and [api.pl](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/poc/prolog/api.pl) (calls Datamuse API). In more detail, it consists of the following:

### 1. A way to outline the basic structure of a haiku
Given a dataset of words and their syllables, our program will generate each line of a haiku while adhering to the number of syllables needed. The basic structure of our haiku is expressed with the predicate [haiku_lines/2](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/poc/prolog/src.pl#L22-L29).

As seen, we create each line separately using the predicates [line1/3, line2/3, line3/3](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/poc/prolog/src.pl#L33-L49) and ensure that the number of syllables are met. 

These predicates rely on [random_word_syllable/4](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/src.pl#L55-L68) to find words matching the desired number of syllables and parts of speech.
- This was also an opportunity for us to explore and take advantage of Prolog's built-in predicates for operating on lists such as [include/3](https://www.swi-prolog.org/pldoc/doc_for?object=include/3) and [maplist/2](https://www.swi-prolog.org/pldoc/man?predicate=maplist/2).

### 2. Gathering a dataset of words and parsing metadata
We leveraged the word-finding capabilities of the [Datamuse API](https://www.datamuse.com/api/) to get our dataset of words. The API endpoint `/words` produces the number of syllables for each word returned and lists out the parts of speech identifiers. An example response from the API is:

```json
// GET request https://api.datamuse.com/words?max=500&md=sp&topics=burger,soccer,basketball
[
    {
        "word": "fire",
        "score": 11546,
        "numSyllables": 2,
        "tags": [
            "n"
        ]
    },
    {
        "word": "van",
        "score": 11519,
        "numSyllables": 1,
        "tags": [
            "n"
        ]
    }
]
```

To get the API working, we used [Prolog's HTTP client libraries](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27)) to send requests and to also convert from JSON to Prolog terms (which was more difficult than anticipated, but ended up being less code and more succinct than we anticipated).

- [get_words/2](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/api.pl#L11-L16) - predicate that our haiku uses to get words
- [convert_to_words/2](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/api.pl#L18-L28) - converts a JSON response from the API to a list of words where each word is a compound term `word(Value, NumSyllables, PoS)`.
- [make_api_call/2](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/api.pl#L30-L48) - sends the HTTP request

### 3. Curating the haiku to some set of topics

A key feature of our proof-of-concept is generating Haiku’s centered around some theme. This is accomplished by using IO to prompt the user for some topics before generating a Haiku, and then passing those topics to the Datamuse API which skews results towards these topics.
- [topics query parameter](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/api.pl#L43)
- [IO - prompting the user for topics](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/src.pl#L12)

### 4. Parts of Speech and expressing basic grammar

The Datamuse API also returns tags which identify the parts of speech for each word. We collect this metadata and can express the basic grammar of our haiku by finding words that match a parts of speech identifier.

- [line1/3, line2/3, line3/3](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/poc/prolog/src.pl#L33-L49) search for specific parts of speech identifiers.
- [random_word_syllable/4](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/src.pl#L65) filters parts of speech

### How this illustrates a key element of our project and why it’s important:

Getting the dataset of words and defining predicates to express the 5-7-5 structure of our Haiku gives us more confidence that the core logic of Paiku works and is feasible to implement. We’ve also added multiple unit tests to make our relations more robust and to catch edge cases. Lastly, we were curious how http client libraries work in Prolog and had fun experimenting trying to convert JSON to prolog.

### How this gives us the confidence to complete our MVP

Right now most of the Haiku’s do not make much semantically, but by having the basic 5-7-5 structure of a Haiku laid out and the necessary data to work with (parts of speech and number of syllables), we can focus more of our time in the MVP on improving the grammar of our Haiku’s.

### Running the code

- `cd` into the `prolog` directory
- run `swipl src.pl`
- enter the query `?- start.`
> Note there is no need for an API key because the Datamuse API is free.
- Remarks:
    - the query `?- start.` will prompt you to enter a 1-5 topics you want for your haiku. Please enter them in comma separated values. Some topics to try can be (without the quotation marks):
        - "soccer,basketball"
        - "autumn,leaves,orange"
        - anything you're curious about!
    - Notice how each line in our haiku adheres to the 5-7-5 syllable structure.
- Some interesting cases to try
    - You can press `;` to see different variations of the haiku generated
    - load `swipl api.pl` and try the query `?- get_words(‘topic1,topic2’, Words).` Replace the first term with a comma separated list of topics you’re interested in. This will call the API and give a list of compound terms, `word(Value, NumSyllables, PoS)`.

### Testing the code
From the `prolog` directory, run `swipl -g run_tests -t halt test.pl`

Some of our main test groups testing core logic of our proof of concept
- [convert_to_words/2 tests](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/test.pl#L17)
- [random_word_syllable/4 tests](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/test.pl#L42)
- [pick_random_word/3 tests](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/test.pl#L112)
- [same_syllable/2, same_parts_of_speech/2, get_value/2 tests](https://github.students.cs.ubc.ca/ngduy28/cpsc-312-project-prolog/blob/e3d3a2959ad35c821d0deaf29684ebbb2209a826/prolog/test.pl#L136) 
