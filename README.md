# regex engine

This program compiles regular expressions to Non Deterministic Finite Automata (NFA) (meaning that the regular expressions are non-backtracking), and then simulates pattern matching.

# Status

This project currently has no users, but it is in a usable state.
It is recommended that you precompile a set of regexes for your own use cases, test them, and verify their performance.

# Usage

This section demonstrates how to use the Custom Regex Engine implemented in Java.
The engine supports tokenization, parsing, AST generation, NFA construction (via Thompson’s algorithm), and simulation.

# Supported Tokens

- ^ Matches the starting position in the line (the match does not include any of the line's characters).

- $ Matches the ending position in the line (the match does not include any of the line's characters).

- . Matches any single character.

- [ ] Matches a single character if that character is listed between the brackets; "-" can be used to specify a range. For example, [xy] matches x or y, and [0-9] matches any decimal digit.

- [^ ] Matches a single character as long as that character is not listed between the brackets.

- + Matches one or more consecutive occurrences of the preceding atom. For example, [0-9]+ matches any positive decimal integer.

- \* Matches zero or more consecutive occurrences of the preceding atom.

- ? Matches zero or one occurrence of the preceding atom. For example, -?[0-9]+ matches a positive or negative decimal integer.

- ( ) Delimits a subexpression; matches anything that matches the pattern between the parentheses.

- | Matches either the pattern before the | or the pattern after the |. For example, abc|def matches either abc or def.

# Lexer

The Lexer is the first stage of the regex engine.
It tokenizes a raw regular expression string into a list of structured tokens (Token objects) that the Parser can process.
Each token contains:

- Its lexeme (actual character or symbol)

- Its index (position in the string)

- Its type (TokenType)

**TokenType Enum**

- **BeginAnchor (`^`)** → Marks the start of the string
- **EndAnchor (`$`)** → Marks the end of the string
- **Dot (`.`)** → Matches any single character
- **Left_Bracket (`[`)** → Start of a character class
- **Right_Bracket (`]`)** → End of a character class
- **Negate (`[^`)** → Negation inside character class
- **Plus (`+`)** → One or more repetitions
- **Asterisk (`*`)** → Zero or more repetitions
- **Pipe (`|`)** → Alternation (OR) operator
- **QuestionMark (`?`)** → Zero or one repetition
- **Left_Parenthesis (`(`)** → Start of a group
- **Right_Parenthesis (`)`)** → End of a group
- **Range (`-`)** → Range operator inside brackets
- **Char (literal)** → Represents literal characters (`a-z`, `A-Z`, `0-9`, etc.)

**createTokenStream(regex)**

```java
public ArrayList<Token> createTokenStream(String regex) {
	boolean inBracket = false;
	ArrayList<Token> tokenStream = new ArrayList<>();

	for(int i = 0; i < regex.length(); i++){
		String lexeme = String.valueOf(regex.charAt(i));
		if(lexeme.equals("[")) inBracket = true;

		Token prevToken = tokenStream.size() == 0 ? null : tokenStream.get(tokenStream.size() -1);
		Token newToken = new Token(lexeme, i, classifyToken(prevToken, lexeme, inBracket));
		tokenStream.add(newToken);

		if(lexeme.equals("]")) inBracket = false;
	}

	return tokenStream;
}

```

**purpose**

This method scans the input regex string character by character
and produces a list of Token objects, each representing a meaningful unit (e.g. ^, [, ], +, a, etc.).
It acts as the first stage of the regex engine — converting plain text into structured tokens.

# Grammar Production Rules for Regex

- **Regex** → `Concat RegexPrime`
- **RegexPrime** → `"|" Concat RegexPrime` | `ε`

---

- **Concat** → `Repeat ConcatPrime`
- **ConcatPrime** → `Repeat ConcatPrime` | `ε`

---

- **Repeat** → `Atom RepeatOp`
- **RepeatOp** → `"*"` | `"+"` | `"?"` | `ε`

---

- **Atom** →
  - `"^"`
  - `"$"`
  - `"."`
  - `Char`
  - `BracketExpr`
  - `"(" Regex ")"`

---

- **BracketExpr** → `"[" BracketItem "]"`
- **BracketItem** → `"^" BracketList` | `BracketList`
- **BracketList** → `BracketElem BracketList` | `BracketElem`
- **BracketElem** → `Char "-" Char` | `Char`

---

# Parser (LL(1) Recursive Descent)

## ParseNodeType Enum

### Non-terminals (Grammar Rules)

- **REGEX**
- **REGEX_PRIME**
- **CONCAT**
- **CONCAT_PRIME**
- **REPEAT**
- **REPEAT_OP**
- **ATOM**
- **BRACKET_EXPR**
- **BRACKET_ITEM**
- **BRACKET_LIST**
- **BRACKET_ELEM**

---

### Terminals (Tokens)

- **BEGIN_ANCHOR (`^`)** &#8594; Start of line
- **END_ANCHOR (`$`)** &#8594; End of line
- **DOT (`.`)** &#8594; Matches any single character
- **CHAR** &#8594; Literal character
- **LEFT_PAREN (`(`)** &#8594; Start of a group
- **RIGHT_PAREN (`)`)** &#8594; End of a group
- **PIPE (`|`)** &#8594; Alternation operator
- **ASTERISK (`*`)** &#8594; Zero or more repetitions
- **PLUS (`+`)** &#8594; One or more repetitions
- **QUESTION (`?`)** &#8594; Zero or one repetition
- **LEFT_BRACKET (`[`)** &#8594; Start of character class
- **RIGHT_BRACKET (`]`)** &#8594; End of character class
- **NEGATE (`[^`)** &#8594; Negation inside character class
- **RANGE (`-`)** &#8594; Range operator inside brackets

## Core Methods

- `parse()` &#8594; Entry point → calls `parseRegex()`
- `parseRegex()` &#8594; Handles alternation (`|`)
- `parseConcat()` &#8594; Handles concatenation
- `parseRepeat()` &#8594; Handles `*`, `+`, `?` operators
- `parseAtom()` &#8594; Handles base units (`^`, `$`, `.`, `Char`, `(Regex)`, `[Expr]`)
- `parseBracketExpression()` &#8594; Handles `[ ... ]` character classes

## LL(1) Algorithm

1. **Lookahead:** `peek()` inspects the next token
2. **Predict:** Chooses the correct production based on current token
3. **Consume:** `move()` advances the lookahead pointer
4. **Recurse:** Calls the corresponding parse function
5. **Error Handling:** Throws `ParserException` if unexpected token found

## Example Usage

```java
Lexer lexer = new Lexer();
ArrayList<Lexer.Token> tokens = lexer.createTokenStream("(a|b)*c");

Parser parser = new Parser(tokens);
Parser.ParseNode root = parser.parse();

System.out.println(parser.serialize(root));
```

**sample output**

```
REGEX
    CONCAT
        REPEAT
            ATOM
                LEFT_PAREN
                REGEX
                    CONCAT
                        REPEAT
                            ATOM
                                CHAR (a)
                        REGEX_PRIME
                            PIPE
                            CONCAT
                                REPEAT
                                    ATOM
                                        CHAR (b)
                RIGHT_PAREN
            REPEAT_OP
                ASTERISK
        CONCAT_PRIME
            REPEAT
                ATOM
                    CHAR (c)
```

## Parser Features

- &check; LL(1) Predictive Parsing (no backtracking)

- &check; Detailed error reporting

- &check; Nested parentheses supported

- &check; Character classes [a-z], negation [^a-z], and ranges handled

- &check; Extensible grammar — easily add new rules

# AST (Abstract syntax tree)

## ASTNodeType Enum

Represents the types of nodes in the Abstract Syntax Tree (AST) for the regex parser.

---

- **BEGIN_ANCHOR (`^`)** &#8594; Marks the start of a string
- **END_ANCHOR (`$`)** &#8594; Marks the end of a string
- **DOT (`.`)** &#8594; Matches any single character
- **CHAR** &#8594; Represents a literal character
- **PIPE (`|`)** &#8594; Alternation (OR) operator
- **ASTERISK (`*`)** &#8594; Zero or more repetitions (Kleene Star)
- **PLUS (`+`)** &#8594; One or more repetitions
- **QUESTION (`?`)** &#8594; Zero or one repetition (optional)
- **BRACKET (`[]`)** &#8594; Character class (e.g. `[a-z]`)
- **CONCAT (`·`)** &#8594; Explicit concatenation operator

## ASTbuilder Class

The `ASTbuilder` class is responsible for **transforming the parsed regular expression tree** (produced by the `Parser`) into a **postfix (Reverse Polish Notation)** representation.  
This postfix form is ideal for **Thompson’s Construction Algorithm**, which builds an NFA (Non-deterministic Finite Automaton) from the regex.

---

## Purpose

- Converts a **parse tree** (top-down recursive structure) into a **linear postfix list** of `ASTNode`s.
- Postfix order ensures operators (`|`, `*`, `+`, `?`, `·`) appear **after** their operands, which simplifies NFA construction.
- Handles **concatenation, alternation, repetition, grouping**, and **character classes** (`[...]`).

---

## Core Responsibilities

1. **Traverse the Parse Tree**

   - Uses **post-order traversal** (visit children before parent).
   - Ensures correct postfix order for all regex operators.

2. **Generate Postfix Sequence**

   - Builds a list of `ASTNode` elements representing the regex in **Reverse Polish Notation (RPN)**.
   - Example:
     ```
     (a|b)*c
     ```
     → Postfix:
     ```
     a b | * c ·
     ```

3. **Handle Complex Grammar Nodes**
   - **Concat / ConcatPrime**: Flatten nested concatenations into a single sequence.
   - **RegexPrime**: Process alternations (`|`).
   - **Repeat / RepeatOp**: Append unary repetition operators (`*`, `+`, `?`).
   - **BracketExpr**: Treat `[a-z0-9]` as a single atomic operand.
   - **Parentheses**: Ignored in postfix since order is already implicit.

---

## Key Components

### `ASTNodeType`

Defines all possible node types in the AST:

- **Operands:** `CHAR`, `DOT`, `BRACKET`, `BEGIN_ANCHOR`, `END_ANCHOR`
- **Unary Operators:** `ASTERISK (*)`, `PLUS (+)`, `QUESTION (?)`
- **Binary Operators:** `CONCAT (·)`, `PIPE (|)`

---

### `ASTNode`

Represents a single node in the AST.  
Each node may contain:

- `lexeme`: String representation (e.g., `"a"`, `"|"`, `"*"`).
- `type`: Enum value (`ASTNodeType`).
- `children`: List of operand nodes (for operators).

---

### `ASTBracketNode`

Special subclass of `ASTNode` for handling character classes (`[...]`):

- Builds a **set of valid characters**.
- Supports **ranges** (e.g., `a-z`) and **negation** (e.g., `[^0-9]`).

---

### `toPostfix(Parser.ParseNode root, ArrayList<Lexer.Token> tokenStream)`

Main entry point.  
Converts a parse tree (`root`) into a list of `ASTNode`s in **postfix order**.

---

### `toPostfixRecursive(Parser.ParseNode node)`

Recursive traversal implementing **post-order DFS**:

- Processes children first.
- Emits operators after operands.

---

### `buildAST(ArrayList<ASTNode> postfix)`

Builds a **tree structure** from a postfix sequence:

- Uses a **stack-based algorithm**:
  - Push operands.
  - Pop operands for operators.
  - Attach as children and push the resulting subtree.

---

### `serializeAST(ASTNode root)`

Pretty-prints the AST structure for debugging:

**sample output**

- ast

```
{ type CONCAT  lexeme : (c) }
	{ type ASTERISK  lexeme : * }
		{ type PIPE  lexeme : | }
			{ type CHAR  lexeme : a }
			{ type CHAR  lexeme : b }
	{ type CHAR  lexeme : c }
```

- postfix

```
a, b, |, *, c, (c)
```

# NFA

## Overview

This class reads the output of the **ASTbuilder** (a postfix list of `ASTNode`s) and applies the six **Thompson construction rules** to incrementally build an NFA.

Each regular expression operator corresponds to one or more structural transformations of smaller NFAs, combined into a larger automaton.

---

## Core Algorithm: Thompson Construction

### Rule 1 – Literal / Operand

Create a simple NFA for a single symbol or character class.

Example:

- For `'a'`: two states `(0)` and `(1)` connected by transition labeled `'a'`
- For wildcard `.` or bracket `[a-z]`, treat similarly with their respective transition symbols.

---

### Rule 2 – Concatenation (·)

Connect two NFAs **A** and **B** sequentially.  
The accept state of **A** is linked via an ε-transition to the start state of **B**.

Resulting NFA:

- `start = A.start`
- `final = B.final`

---

### Rule 3 – Alternation (|)

Construct an NFA that **splits into two branches** (via ε-transitions) for **A** and **B**, and merges them into a single final state.

Resulting NFA:

- `start = new state s`
- `final = new state t`

---

### Rule 4 – Kleene Star (\*)

Create an NFA that allows **zero or more repetitions** of A.
Accepts any number (including zero) of A.

---

### Rule 5 – Plus (+)

Similar to Kleene Star, but requires **at least one occurrence** of A.

Implemented as:

This ensures one repetition before looping.

---

### Rule 6 – Optional (?)

Allows **zero or one occurrence** of A.

Accepts presence or absence of A.

---

## Class Structure

### `NFABuilder`

Main class responsible for applying Thompson's algorithm.

#### Fields

- `ArrayList<ASTbuilder.ASTNode> postfix`  
  Postfix (RPN) representation of the regex produced by the AST builder.

#### Methods

- `NFABuilder(ArrayList<ASTbuilder.ASTNode> postfix)`  
  Constructor initializing with a postfix list.

- `NFA ThompsonMethod()`  
  Core implementation of Thompson's construction.  
  Iterates through postfix tokens and applies appropriate rules using a **stack-based** approach.

---

### `Trans` (Transition Class)

Represents a **transition** between two states.

#### Fields

- `int state_from` – source state
- `int state_to` – target state
- `String trans_symbol` – symbol (character, '.', 'EPS', etc.)

Supports:

- **Epsilon transitions** (`"EPS"`)
- **Character classes** (`[a-z]`)
- **Negations** (`[^0-9]`)

#### Methods

- `boolean matches(char c)`  
  Checks if a character matches the transition condition.
- `HashSet<String> getCharacterSet()`  
  Returns all possible characters for a character class.
- `boolean getIsNegated()`  
  Indicates if the class is negated.

---

### `NFA`

Represents a **Non-deterministic Finite Automaton**.

#### Fields

- `ArrayList<Integer> states` – all states in the automaton
- `ArrayList<Trans> transitions` – list of transitions
- `int final_state` – accepting state index
- `static String EPSILON = "EPS"` – denotes ε-transitions

#### Constructors

- `NFA()` – empty automaton
- `NFA(int size)` – creates NFA with a given number of states
- `NFA(String c)` – creates NFA for a single character or symbol

#### Methods

- `void setStateSize(int size)` – initializes a state set of given size
- `void display()` – prints all transitions for debugging

---

# ReModule

The `ReModule` class provides a **complete end-to-end regex engine** built upon the **Thompson Construction Algorithm**.  
It compiles a regular expression into an **NFA** (Non-deterministic Finite Automaton) and then performs **simulation-based matching** against input strings.

---

## Overview

This class orchestrates the full regex processing pipeline:

1. **Lexical Analysis (Lexer)**

   - Tokenizes the raw regex string into a sequence of tokens.

2. **Parsing (Parser)**

   - Builds a parse tree from the token stream based on a defined grammar.

3. **AST Conversion (ASTBuilder)**

   - Converts the parse tree into **postfix notation** (Reverse Polish Notation).

4. **NFA Construction (NFABuilder)**

   - Uses **Thompson’s Construction** to generate an NFA from the postfix expression.

5. **NFA Simulation**
   - Simulates input matching using **epsilon-closure** and **state transitions**.

---

## Class Fields

| Field        | Type         | Description                              |
| ------------ | ------------ | ---------------------------------------- |
| `lexer`      | `Lexer`      | Tokenizes regex into `Token` objects     |
| `parser`     | `Parser`     | Parses token stream into a parse tree    |
| `astBuilder` | `ASTbuilder` | Converts parse tree into postfix AST     |
| `nfaBuilder` | `NFABuilder` | Builds an NFA using Thompson’s algorithm |

---

## Methods

### `boolean isExactMatch(String regex, String pattern)`

Performs an **exact match** check — the entire input string must match the given regex.

#### Algorithm Steps

1. **Tokenize Regex**  
   Call `Lexer.createTokenStream(regex)` to generate tokens.

2. **Parse Token Stream**  
   Use `Parser.parse()` to build a parse tree.

3. **Convert to Postfix**  
   `ASTbuilder.toPostfixRecursive(root)` converts the parse tree into postfix order.

4. **Build NFA**  
   Use `NFABuilder.ThompsonMethod()` to construct an NFA.

5. **Initialize Epsilon Closure**  
   Start from state `0`, compute its epsilon-closure:

6. **Simulate Transitions**  
   For each character `c` in `pattern`:

- For every state `s` in `currentStates`, check transitions:
  - If `transition.matches(c)` → add `transition.state_to` to `nextStates`
- Compute epsilon-closures for `nextStates` and replace `currentStates`.

7. **Check Acceptance**  
   After consuming all input characters:

- If `nfa.final_state ∈ currentStates`, accept.
- Otherwise, reject.


#### Returns

- `true` → if input **fully matches** the regex
- `false` → otherwise

#### Example

```java
ReModule re = new ReModule();
boolean result = re.isExactMatch("a(b|c)*", "abcc"); // true
```
