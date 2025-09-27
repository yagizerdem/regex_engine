import java.io.BufferedReader;
import java.lang.reflect.Array;
import java.nio.channels.Pipe;
import java.sql.SQLOutput;
import java.util.*;

/**
 *
 Supported Tokens
 ---------------------------
 ^    Matches the starting position in the line (the match does not include any of the line's characters).
 $    Matches the ending position in the line (the match does not include any of the line's characters).
 .    Matches any single character.
 [ ]  Matches a single character if that character is listed between the brackets; "-" can be used to specify a range. For example, [xy] matches x or y, and [0-9] matches any decimal digit.
 [^ ] Matches a single character as long as that character is not listed between the brackets.
 +    Matches one or more consecutive occurrences of the preceding atom. For example, [0-9]+ matches any positive decimal integer.
 *    Matches zero or more consecutive occurrences of the preceding atom.
 ?    Matches zero or one occurrence of the preceding atom. For example, -?[0-9]+ matches a positive or negative decimal integer.
 ( )  Delimits a subexpression; matches anything that matches the pattern between the parentheses.
 |    Matches either the pattern before the | or the pattern after the |. For example, abc|def matches either abc or def.
 */

public class Main {
    public static void main(String[] args) throws  Exception{
        String regex = "([A-Z][a-z]+[0-9]*|foo(bar)?)[^0-9a-zA-Z]*([1-9][0-9]*|x+y*)?.(txt|csv|log)";
        Lexer lexer = new Lexer();
        ArrayList<Lexer.Token> tokenStream = lexer.createTokenStream(regex);
        Parser parser = new Parser();
        parser.setTokenStream(tokenStream);

        Parser.ParseNode root = parser.parse();
        System.out.println(parser.serialize(root));

        ASTbuilder astBuilder = new ASTbuilder();
        ArrayList<ASTbuilder.ASTNode> postfix =  astBuilder.toPostfix(root, tokenStream);

        System.out.println(postfix);

        ASTbuilder.ASTNode rootAst = astBuilder.buildAST(postfix);
        System.out.println(astBuilder.serializeAST(rootAst));


        NFABuilder nfaBuilder = new NFABuilder();
        nfaBuilder.setPostfix(postfix);
        NFABuilder.NFA nfaRoot = nfaBuilder.ThompsonMethod();

        nfaRoot.display();

        // test
        test1();
        test2();
        test3();


    }

    public static class Lexer  {

        public static enum TokenType {
            BeginAnchor, // ^
            EndAnchor, // $
            Dot, // .
            Left_Bracket, // [
            Right_Bracket, // ]
            Negate, // [^ (Negate operator come after left bracket) ]
            Plus, // +
            Asterisk, // *
            Pipe, // |
            QuestionMark, // ?
            Left_Parenthesis, // (
            Right_Parenthesis, // )
            Range, // [ - (Range operator) ]
            Char, // single character a-z A-Z 0-9
        }

        public static class Token {
            public String lexeme;
            public int index;
            public TokenType type;

            public  Token(String lexeme, int index , TokenType type) {
                this.lexeme = lexeme;
                this.index = index;
                this.type = type;
            }
            public Token(){}

            public String getLexeme(){
                return this.lexeme;
            }

            public int getIndex(){
                return this.index;
            }

            public TokenType getType(){
                return this.type;
            }

            public void setLexeme(String lexeme) {
                this.lexeme = lexeme;
            }

            public void setIndex(int index) {
                this.index = index;
            }

            public void setType(TokenType type) {
                this.type = type;
            }

            @Override
            public String toString() {
                return String.format("lexeme : %s , index : %d , token type : %s" ,
                        this.lexeme, this.index, this.type);
            }
        }

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

        // determines token type based on lexeme
        public TokenType classifyToken(Token prevToken, String lexeme, boolean inBracket) {
            if(inBracket) {
                switch (lexeme) {
                    case "^":
                        if(prevToken.type.equals(TokenType.Left_Bracket))
                            return  TokenType.Negate;
                        return TokenType.Char;
                    case "-" :
                        if(prevToken.type.equals(TokenType.Char))
                            return TokenType.Range;
                        return  TokenType.Char;
                    case "[":
                        return TokenType.Left_Bracket;
                    case "]":
                        return  TokenType.Right_Bracket;
                }
            }
            else{
                switch (lexeme) {
                    case "^":
                        return TokenType.BeginAnchor;
                    case "$":
                        return TokenType.EndAnchor;
                    case "?":
                        return TokenType.QuestionMark;
                    case "*":
                        return TokenType.Asterisk;
                    case "+":
                        return TokenType.Plus;
                    case "(":
                        return TokenType.Left_Parenthesis;
                    case ")":
                        return TokenType.Right_Parenthesis;
                    case ".":
                        return TokenType.Dot;
                    case "|":
                        return TokenType.Pipe;
                }
            }

            // default token type
            return  TokenType.Char;
        }

    }

    public static class Parser {
        /**
         * Grammar production rules for regex
         * ------------------------------------
         * Regex       -> Concat RegexPrime
         * RegexPrime  -> "|" Concat RegexPrime | ε
         * <p>
         * Concat      -> Repeat ConcatPrime
         * ConcatPrime -> Repeat ConcatPrime | ε
         * <p>
         * Repeat      -> Atom RepeatOp
         * RepeatOp    -> "*" | "+" | "?" | ε
         * <p>
         * Atom        -> "^"
         * | "$"
         * | "."
         * | Char
         * | BracketExpr
         * | "(" Regex ")"
         * <p>
         * BracketExpr -> "[" BracketItem "]"
         * BracketItem -> "^" BracketList | BracketList
         * BracketList -> BracketElem BracketList | BracketElem
         * BracketElem -> Char "-" Char | Char
         */

        public static enum ParseNodeType {
            // Non-terminals (grammar rules)
            REGEX,
            REGEX_PRIME,
            CONCAT,
            CONCAT_PRIME,
            REPEAT,
            REPEAT_OP,
            ATOM,
            BRACKET_EXPR,
            BRACKET_ITEM,
            BRACKET_LIST,
            BRACKET_ELEM,

            // Terminals (tokens)
            BEGIN_ANCHOR,      // ^
            END_ANCHOR,        // $
            DOT,               // .
            CHAR,              // literal character
            LEFT_PAREN,        // (
            RIGHT_PAREN,       // )
            PIPE,              // |
            ASTERISK,          // *
            PLUS,              // +
            QUESTION,          // ?
            LEFT_BRACKET,      // [
            RIGHT_BRACKET,     // ]
            Negate,            // [ ^
            RANGE              // [1 -(range) 9]
        }
        public static class ParseNode {
            public ArrayList<ParseNode> children = new ArrayList<>();
            public ParseNodeType type ;

            public Lexer.Token token; // only available in leaf nodes

            public ArrayList<ParseNode> getChildren(){
                return this.children;
            }

            public ParseNodeType getType() {
                return this.type;
            }

            public void setType(ParseNodeType type) {
                this.type = type;
            }

            public void setChildren(ArrayList<ParseNode> children) {
                this.children = children;
            }

            public ParseNode(){}

            public ParseNode(ArrayList<ParseNode> children, ParseNodeType type) {
                this.children =children;
                this.type = type;
            }

            public boolean isLeafNode(){
                return  this.children.size() == 0;
            }

            @Override
            public String toString() {
                return toString(0);
            }

            private String toString(int indent) {
                StringBuilder sb = new StringBuilder();

                sb.append("  ".repeat(indent));

                sb.append("NodeType: ").append(this.type);

                if (this.token != null) {
                    sb.append(" [lexeme='").append(this.token.lexeme)
                            .append("', index=").append(this.token.index)
                            .append(", type=").append(this.token.type)
                            .append("]");
                }

                return sb.toString();
            }

        }

        public static class ParserException extends Exception {
            public ParserException(String message) {
                super(message);
            }

            public ParserException(String message, int position) {
                super("Parse error at position " + position + ": " + message);
            }
        }

        public ArrayList<Lexer.Token> tokenStream;

        public int lookAhead;

        public Parser(ArrayList<Lexer.Token> tokenStream) {
            this.tokenStream = tokenStream;
        }

        public Parser(){
            this.tokenStream = new ArrayList<>();
        }

        public ArrayList<Lexer.Token> getTokenStream(){
            return  this.tokenStream;
        }

        public void setTokenStream(ArrayList<Lexer.Token> tokenStream){
            this.tokenStream = tokenStream;
        }

        public Lexer.Token peek() {
            if(this.lookAhead >= this.tokenStream.size()) return  null;
            return  tokenStream.get(this.lookAhead);
        }

        public Lexer.Token move(){
            if(this.lookAhead >= this.tokenStream.size()) return null;
            Lexer.Token token = this.peek();
            this.lookAhead +=1;
            return  token;
        }


        public ParseNode parse() throws ParserException{
            return  this.parseRegex();
        }

        // parse regex
        private ParseNode parseRegex() throws ParserException{
            ParseNode regexNode = new ParseNode();
            regexNode.type = ParseNodeType.REGEX;
            ParseNode concatNode = this.parseConcat();
            ParseNode regexPrimeNode = this.parseRegexPrime();
            if (concatNode == null) {
                Lexer.Token lastToken = lookAhead > 0 ? this.tokenStream.get(lookAhead - 1) : null;
                throw this.createParseException(
                        "Expected Concat expression at start of Regex",
                        lastToken
                );
            }
            regexNode.children.add(concatNode);
            if(regexPrimeNode != null) regexNode.children.add(regexPrimeNode);

            return  regexNode;
        }

        private ParseNode parseConcat() throws ParserException {
            ParseNode concatNode = new ParseNode();
            concatNode.type = ParseNodeType.CONCAT;

            ParseNode repeatNode = this.parseRepeat();
            if (repeatNode == null) {
                Lexer.Token lastToken = lookAhead > 0 ? this.tokenStream.get(lookAhead - 1) : null;
                throw this.createParseException(
                        "Expected Repeat expression at start of Concat",
                        lastToken
                );
            }
            ParseNode concatPrimeNode = this.parseConcatPrime();

            concatNode.children.add(repeatNode);
            if(concatPrimeNode != null) concatNode.children.add(concatPrimeNode);

            return concatNode;
        }

        private ParseNode parseRegexPrime() throws ParserException {
            Lexer.Token firstLook = this.peek();
            if(firstLook == null) return null;

            if(firstLook.lexeme.equals(")") &&
                    firstLook.type.equals(Lexer.TokenType.Right_Parenthesis)){
                return  null;
            }

            ParseNode regexPrimeNode = new ParseNode();
            regexPrimeNode.type = ParseNodeType.REGEX_PRIME;

            if(!(firstLook.type.equals(Lexer.TokenType.Pipe) &&
                    firstLook.lexeme.equals("|"))) {
                return null;
            }

            ParseNode pipeNode = new ParseNode();
            pipeNode.type = ParseNodeType.PIPE;
            pipeNode.token = firstLook;
            regexPrimeNode.children.add(pipeNode);

            move(); // consume "|"

            ParseNode concatNode = this.parseConcat();
            if (concatNode == null) {
                Lexer.Token lastToken = lookAhead > 0 ? this.tokenStream.get(lookAhead - 1) : null;
                throw this.createParseException("Expected Concat after '|'", lastToken);
            }

            ParseNode regexPrimeTail = this.parseRegexPrime();

            if(concatNode != null) regexPrimeNode.children.add(concatNode);
            if(regexPrimeTail != null) regexPrimeNode.children.add(regexPrimeTail);

            return regexPrimeNode;
        }

        private ParseNode parseConcatPrime() throws ParserException {
            Lexer.Token firstLook = this.peek();
            if(firstLook == null) return null;

            if (!(firstLook.type == Lexer.TokenType.Char
                    || firstLook.type == Lexer.TokenType.BeginAnchor
                    || firstLook.type == Lexer.TokenType.EndAnchor
                    || firstLook.type == Lexer.TokenType.Dot
                    || firstLook.type == Lexer.TokenType.Left_Parenthesis
                    || firstLook.type == Lexer.TokenType.Left_Bracket)) {
                return null; // epsilon
            }

            ParseNode concatPrimeNode = new ParseNode();
            concatPrimeNode.type = ParseNodeType.CONCAT_PRIME;

            ParseNode repeatNode = this.parseRepeat();


            if(repeatNode == null) return  null;

            ParseNode concatPrimeNode_ = this.parseConcatPrime();

            if(repeatNode != null) concatPrimeNode.children.add(repeatNode);
            if(concatPrimeNode_ != null) concatPrimeNode.children.add(concatPrimeNode_);

            return concatPrimeNode;
        }

        private ParseNode parseRepeat() throws ParserException {
            Lexer.Token firstLook = this.peek();
            if(firstLook == null) return  null;

            ParseNode repeatNode = new ParseNode();
            repeatNode.type = ParseNodeType.REPEAT;

            ParseNode atomNode = this.parseAtom();
            if (atomNode == null) {
                Lexer.Token lastToken = lookAhead > 0 ? this.tokenStream.get(lookAhead - 1) : null;
                throw this.createParseException("Expected Atom in Repeat", lastToken);
            }
            ParseNode repeatOpNode = this.parseRepeatOp();

            if(atomNode != null) repeatNode.children.add(atomNode);
            if(repeatOpNode != null) repeatNode.children.add(repeatOpNode);

            return repeatNode;
        }

        private ParseNode parseAtom() throws ParserException  {
            Lexer.Token firstLook = this.peek();
            if (firstLook == null) {
                Lexer.Token lastConsumedToken = lookAhead  -1 > 0 ? this.tokenStream.get(lookAhead -1) : null;
                throw this.createParseException(
                        "Unexpected end of input. Expected Atom (one of: ^, $, ., Char, BracketExpr, (Regex))",
                        lastConsumedToken
                );
            }

            ParseNode atomNode = new ParseNode();
            atomNode.type = ParseNodeType.ATOM;

            if(firstLook.lexeme.equals("^") &&
                    firstLook.type.equals(Lexer.TokenType.BeginAnchor)) {
                ParseNode beginAnchorNode = new ParseNode();
                beginAnchorNode.type = ParseNodeType.BEGIN_ANCHOR;
                beginAnchorNode.token = firstLook;

                atomNode.children.add(beginAnchorNode);
                this.move(); // consume ^
                return  atomNode;
            }
            else if(firstLook.lexeme.equals("$") &&
                    firstLook.type.equals(Lexer.TokenType.EndAnchor)) {
                ParseNode endAnchorNode = new ParseNode();
                endAnchorNode.type = ParseNodeType.END_ANCHOR;
                endAnchorNode.token = firstLook;

                atomNode.children.add(endAnchorNode);
                this.move(); // consume $
                return  atomNode;
            }
            else if(firstLook.lexeme.equals(".") &&
                    firstLook.type.equals(Lexer.TokenType.Dot)) {
                ParseNode dotNode = new ParseNode();
                dotNode.type = ParseNodeType.DOT;
                dotNode.token = firstLook;

                atomNode.children.add(dotNode);
                this.move(); // consume .
                return  atomNode;
            }
            else if(firstLook.lexeme.equals("(") &&
                    firstLook.type.equals(Lexer.TokenType.Left_Parenthesis)) {
                ParseNode leftPharenthesisNode = new ParseNode();
                leftPharenthesisNode.type = ParseNodeType.LEFT_PAREN;
                leftPharenthesisNode.token = firstLook;

                atomNode.children.add(leftPharenthesisNode);

                this.move(); // consume (

                ParseNode regexNode = this.parseRegex();

                atomNode.children.add(regexNode);

                Lexer.Token nextToken = this.peek();

                if (nextToken == null ||
                        !")".equals(nextToken.lexeme) ||
                        nextToken.type != Lexer.TokenType.Right_Parenthesis) {

                    Lexer.Token lastConsumedToken = lookAhead > 0 ? this.tokenStream.get(lookAhead - 1) : null;

                    String got = (nextToken == null)
                            ? "end of input"
                            : "'" + nextToken.getLexeme() + "' at index " + nextToken.getIndex();

                    throw this.createParseException(
                            "Mismatched parenthesis: expected ')' but got " + got,
                            lastConsumedToken
                    );
                }

                ParseNode rightPharenthesisNode = new ParseNode();
                rightPharenthesisNode.type = ParseNodeType.RIGHT_PAREN;
                rightPharenthesisNode.token = nextToken;


                this.move(); // consume )

                atomNode.children.add(rightPharenthesisNode);

                return atomNode;
            }

            else if(firstLook.type.equals(Lexer.TokenType.Char)) {
                ParseNode charNode = new ParseNode();
                charNode.type = ParseNodeType.CHAR;
                charNode.token = firstLook;

                atomNode.children.add(charNode);
                this.move(); // consume char
                return  atomNode;
            }
            else if(firstLook.type.equals(Lexer.TokenType.Left_Bracket) &&
                    firstLook.lexeme.equals("[")){
                ParseNode bracketExpression = this.parseBracketExpression();

                if (bracketExpression == null) {
                    Lexer.Token lastConsumedToken = lookAhead > 0 ? this.tokenStream.get(lookAhead - 1) : null;
                    throw this.createParseException(
                            "Expected Atom but got unexpected token",
                            lastConsumedToken
                    );
                }

                atomNode.children.add(bracketExpression);
                return  atomNode;
            }

            else {
                throw this.createParseException(
                        "Expected Atom (one of: ^, $, ., Char, BracketExpr, (Regex)), but found: " + firstLook.lexeme,
                        firstLook
                );
            }
        }

        private ParseNode parseRepeatOp() throws ParserException  {
            Lexer.Token firstLook = this.peek();
            if(firstLook == null) return null;

            ParseNode repeatOpNode = new ParseNode();
            repeatOpNode.type = ParseNodeType.REPEAT_OP;

            if(firstLook.lexeme.equals("*") &&
                    firstLook.type.equals(Lexer.TokenType.Asterisk)) {
                ParseNode asteriskNode = new ParseNode();
                asteriskNode.type = ParseNodeType.ASTERISK;
                asteriskNode.token = firstLook;

                repeatOpNode.children.add(asteriskNode);
                this.move(); // consume *
                return  repeatOpNode;
            }
            if(firstLook.lexeme.equals("+") &&
                    firstLook.type.equals(Lexer.TokenType.Plus)) {
                ParseNode plusNode = new ParseNode();
                plusNode.type = ParseNodeType.PLUS;
                plusNode.token = firstLook;

                repeatOpNode.children.add(plusNode);
                this.move(); // consume +
                return  repeatOpNode;
            }
            if(firstLook.lexeme.equals("?") &&
                    firstLook.type.equals(Lexer.TokenType.QuestionMark)) {
                ParseNode questionNode = new ParseNode();
                questionNode.type = ParseNodeType.QUESTION;
                questionNode.token = firstLook;

                repeatOpNode.children.add(questionNode);
                this.move(); // consume ?
                return  repeatOpNode;
            }

            return  null;
        }

        private ParseNode parseBracketExpression() throws  ParserException {
            Lexer.Token firstLook = this.peek();

            if (firstLook == null) {
                throw this.createParseException("Unexpected end of input, expected '['", null);
            }

            if (!firstLook.type.equals(Lexer.TokenType.Left_Bracket) ||
                    !firstLook.lexeme.equals("[")) {
                throw this.createParseException("Expected '[' at start of BracketExpr", firstLook);
            }

            ParseNode bracketExprNode = new ParseNode();
            bracketExprNode.type = ParseNodeType.BRACKET_EXPR;

            ParseNode leftBracketNode = new ParseNode();
            leftBracketNode.type = ParseNodeType.LEFT_BRACKET;
            leftBracketNode.token = firstLook;
            bracketExprNode.children.add(leftBracketNode);

            this.move(); // consume [

            ParseNode bracketItemNode = this.parseBracketItem();
            if (bracketItemNode == null) {
                throw this.createParseException("Expected BracketItem after '['", this.peek());
            }
            bracketExprNode.children.add(bracketItemNode);

            Lexer.Token nextToken = this.peek();
            if (!nextToken.type.equals(Lexer.TokenType.Right_Bracket) ||
                    !nextToken.lexeme.equals("]")) {
                throw this.createParseException("Expected closing ']' at end of BracketExpr", nextToken);
            }
            ParseNode rightBracketNode = new ParseNode();
            rightBracketNode.type = ParseNodeType.RIGHT_BRACKET;
            rightBracketNode.token = nextToken;
            bracketExprNode.children.add(rightBracketNode);

            this.move(); // consume ]

            return bracketExprNode;
        }

        private ParseNode parseBracketItem() throws ParserException {
            Lexer.Token firstLook = this.peek();
            if (firstLook == null) {
                Lexer.Token lastConsumedToken =
                        (lookAhead - 1 >= 0 && lookAhead - 1 < this.tokenStream.size())
                                ? this.tokenStream.get(lookAhead - 1)
                                : null;

                throw this.createParseException(
                        "Unexpected end of input. Expected character class content inside '[...]' (e.g. ^a, a-z, or list of characters)",
                        lastConsumedToken
                );
            }

            ParseNode bracketItemNode = new ParseNode();
            bracketItemNode.type = ParseNodeType.BRACKET_ITEM;


            if(firstLook.lexeme.equals("^") &&
                    firstLook.type.equals(Lexer.TokenType.Negate)) {
                ParseNode negateNode = new ParseNode();
                negateNode.type = ParseNodeType.Negate;
                negateNode.token = firstLook;
                bracketItemNode.children.add(negateNode);

                this.move(); // consume ^
            }

            ParseNode bracketListNode = this.parseBracketList();
            if (bracketListNode == null) {
                Lexer.Token lastConsumedToken =
                        (lookAhead - 1 >= 0 && lookAhead - 1 < this.tokenStream.size())
                                ? this.tokenStream.get(lookAhead - 1)
                                : null;

                throw this.createParseException(
                        "Expected one or more characters inside '[]' (e.g. a, z, 0-9).",
                        lastConsumedToken
                );
            }

            bracketItemNode.children.add(bracketListNode);

            return bracketItemNode;
        }

        private ParseNode parseBracketList() throws ParserException {
            Lexer.Token firstLook = this.peek();
            if (firstLook == null) {
                Lexer.Token lastConsumedToken =
                        (lookAhead - 1 >= 0 && lookAhead - 1 < this.tokenStream.size())
                                ? this.tokenStream.get(lookAhead - 1)
                                : null;

                throw this.createParseException(
                        "Unexpected end of input. Expected character or range inside '[]' (e.g. a, z, 0-9).",
                        lastConsumedToken
                );
            }
            ParseNode bracketListNode = new ParseNode();
            bracketListNode.type = ParseNodeType.BRACKET_LIST;

            if(firstLook.lexeme.equals("]") && firstLook.type.equals(Lexer.TokenType.Right_Bracket)) {
                return  null;
            }

            ParseNode bracketElementNode = this.parseBracketElement();
            if (bracketElementNode == null) {
                throw this.createParseException(
                        "Expected character or range (e.g. a or a-z) inside '[]'.",
                        firstLook
                );
            }
            ParseNode bracketListNode_ = this.parseBracketList();

            if(bracketElementNode != null) bracketListNode.children.add(bracketElementNode);
            if(bracketListNode_ != null) bracketListNode.children.add(bracketListNode_);

            return bracketListNode;
        }

        private ParseNode parseBracketElement () throws ParserException {
            Lexer.Token firstLook = this.peek();
            ParseNode bracketElementNode = new ParseNode();
            bracketElementNode.type = ParseNodeType.BRACKET_ELEM;

            if (firstLook == null) {
                Lexer.Token lastConsumedToken =
                        (lookAhead - 1 >= 0 && lookAhead - 1 < this.tokenStream.size())
                                ? this.tokenStream.get(lookAhead - 1)
                                : null;
                throw this.createParseException(
                        "Unexpected end of input. Expected character inside '[]'.",
                        lastConsumedToken
                );
            }

            if (!firstLook.type.equals(Lexer.TokenType.Char)) {
                throw this.createParseException(
                        "Expected character inside '[]', but found: " + firstLook.lexeme,
                        firstLook
                );
            }

            ParseNode charNode = new ParseNode();
            charNode.type = ParseNodeType.CHAR;
            charNode.token = firstLook;

            bracketElementNode.children.add(charNode);
            this.move(); // consume char

            firstLook = this.peek();
            if(firstLook.type.equals(Lexer.TokenType.Range) &&
                    firstLook.lexeme.equals("-")) {
                ParseNode rangeNode = new ParseNode();
                rangeNode.type = ParseNodeType.RANGE;
                rangeNode.token = firstLook;

                bracketElementNode.children.add(rangeNode);
                this.move(); // consume range -

                Lexer.Token secondLook = this.peek();

                if (secondLook == null) {
                    throw this.createParseException(
                            "Unexpected end of input. Expected a character after '-' in range expression inside '[]'.",
                            firstLook // '-' tokenı
                    );
                }

                if (!secondLook.type.equals(Lexer.TokenType.Char)) {
                    throw this.createParseException(
                            "Invalid range. Expected a character after '-' in '[]', but found: " + secondLook.lexeme,
                            secondLook
                    );
                }

                ParseNode charNode_ = new ParseNode();
                charNode_.type = ParseNodeType.CHAR;
                charNode_.token = secondLook;

                bracketElementNode.children.add(charNode_);
                this.move(); // consume char
            }


            return  bracketElementNode;
        }


        private String serialize(ParseNode root) {
            return  serializeRecursive(root, 0);
        }

        private String serializeRecursive(ParseNode root, int level) {
            String serialized = "\t".repeat(level) +  root.toString() + "\n";
            for(int i = 0; i < root.children.size(); i++){
                serialized += serializeRecursive(root.children.get(i), level +1);
            }

            return serialized;
        }

        // util methods
        private ParserException createParseException(String message, Lexer.Token token) {
            if (token != null) {
                return new ParserException(
                        "Parse error at index " + token.getIndex() +
                                " (lexeme: '" + token.getLexeme() + "'): " + message
                );
            } else {
                return new ParserException("Parse error: " + message);
            }
        }

    }

    public static class ASTbuilder {

        /**
         * Generates a postfix (Reverse Polish Notation) representation of a regular expression
         * directly from the parse tree.
         *
         * <p>Traversal strategy: post-order (children first, then operator). This allows
         * concatenation, alternation, and repetition operators to be placed correctly
         * after their operands.</p>
         *
         * <h3>Conversion Rules</h3>
         * <ul>
         *   <li><b>CHAR, BEGIN_ANCHOR, END_ANCHOR</b>:
         *       Emit the token lexeme directly (e.g., "a", "^", "$").</li>
         *
         *   <li><b>BRACKET_EXPR</b>:
         *       Treat the entire character class (e.g., "[a-z0-9]") as a single atomic token
         *       in the postfix output.</li>
         *
         *   <li><b>CONCAT</b>:
         *       Recursively generate postfix for both children, then append the concatenation
         *       operator (usually "·"). Note that in the parse tree, explicit concatenation
         *       may be represented across <code>ConcatPrime</code> nodes.</li>
         *
         *   <li><b>REGEX_PRIME (pipe '|')</b>:
         *       Recursively generate postfix for both alternatives, then append "|".</li>
         *
         *   <li><b>REPEAT_OP</b>:
         *       Recursively generate postfix for the child, then append the repetition operator
         *       (*, +, or ?).</li>
         *
         *   <li><b>LEFT_PAREN, RIGHT_PAREN</b>:
         *       Ignore these nodes — parentheses are not needed in postfix form.</li>
         *
         *   <li><b>ConcatPrime, RegexPrime</b>:
         *       These nodes are structural in the parse tree and do not correspond directly
         *       to operators in postfix. Traverse their children but do not emit anything
         *       for the node itself.</li>
         * </ul>
         *
         * <p>By applying these rules recursively, the parse tree can be converted into a
         * valid postfix regex string that is suitable for Thompson’s construction or other
         * NFA/DFA compilation algorithms.</p>
         */

        public static enum ASTNodeType {
            BEGIN_ANCHOR,      // ^
            END_ANCHOR,        // $
            DOT,               // .
            CHAR,              // literal character
            PIPE,              // |
            ASTERISK,          // *
            PLUS,              // +
            QUESTION,          // ?
            BRACKET,      // []
            CONCAT,         // · (explicit concatenation)
        }

        public  static class ASTNode {

            public String lexeme;
            public ASTNodeType type;

            public String getLexeme(){
                return  this.lexeme;
            }

            public void setLexeme(String lexeme){
                this.lexeme = lexeme;
            }

            public ASTNodeType getType(){
                return  this.type;
            }

            public void setType(ASTNodeType type) {
                this.type = type;
            }

            public  ArrayList<ASTNode> children = new ArrayList<>();

            public ASTNode(){}

            public ASTNode(String lexeme, ASTNodeType type) {
                this.lexeme = lexeme;
                this.type = type;
            }

            @Override
            public String toString() {
                return this.lexeme;
            }
        }

        public static class ASTBracketNode extends ASTNode {

            public HashSet<String> getCharacterSet(){
                HashSet<String> set = new HashSet<>();
                for(int i =1 ; i< this.lexeme.length()- 1 ; i++){
                    if(Character.isLetterOrDigit(this.lexeme.charAt(i))) {
                        set.add(String.valueOf(this.lexeme.charAt(i)));
                    }

                    if(String.valueOf(this.lexeme.charAt(i)).equals("-")) {
                        if(i -1 >= 1 && i  + 1 < this.lexeme.length() - 1 && this.lexeme.length() > 0) {
                            Character prevChar = this.lexeme.charAt(i-1);
                            Character nextChar = this.lexeme.charAt(i+1);
                            for(int j = prevChar ; j <= nextChar ; j++){
                                set.add(String.valueOf((char)j));
                            }
                        }
                    }

                }

                return  set;
            }

            public boolean getIsNegated(){
                if(this.lexeme.length() == 0) return  false;
                return  String.valueOf(this.lexeme.charAt(1)).equals("^");
            }

            public ASTBracketNode(){
                super();
                this.type = ASTNodeType.BRACKET;
            }

            public ASTBracketNode(String lexeme) {
                super(lexeme, ASTNodeType.BRACKET);
            }

            @Override
            public String toString() {
                return this.lexeme;
            }
        }

        public ArrayList<ASTNode> toPostfix(Parser.ParseNode root , ArrayList<Lexer.Token> tokenStream) {
            ArrayList<ASTNode> postFix =  this.toPostfixRecursive(root);
            return  postFix;
        }

        private ArrayList<ASTNode> toPostfixRecursive(Parser.ParseNode node) {
            ArrayList<ASTNode> list = new ArrayList<>();

            if(node.type.equals(Parser.ParseNodeType.BEGIN_ANCHOR) ||
                    node.type.equals(Parser.ParseNodeType.END_ANCHOR) ||
                    node.type.equals(Parser.ParseNodeType.CHAR) ||
                    node.type.equals(Parser.ParseNodeType.DOT)) {

                ASTNode newASTNode = mapParseNodeToAstNode(node);
                list.add(newASTNode);
                return list;
            }

            if(node.type.equals(Parser.ParseNodeType.BRACKET_EXPR)){
                ASTBracketNode bracketNode = this.collectBracket(node);
                list.add(bracketNode);
                return  list;
            }

            if (node.type.equals(Parser.ParseNodeType.CONCAT) ||
                    node.type.equals(Parser.ParseNodeType.CONCAT_PRIME)) {

                ArrayList<Parser.ParseNode> operands = new ArrayList<>();
                collectConcatOperands(node, operands);

                for (Parser.ParseNode child : operands) {
                    list.addAll(toPostfixRecursive(child));
                }

                for (int i = 1; i < operands.size(); i++) {
                    ASTNode concatNode = new ASTNode();
                    concatNode.lexeme = "(c)";
                    concatNode.type = ASTNodeType.CONCAT;
                    list.add(concatNode);
                }

                return list;
            }

            if(node.type.equals(Parser.ParseNodeType.REGEX_PRIME)) {
                if (node.children.isEmpty()) return list;
                ASTNode pipeNode = new ASTNode();
                pipeNode.type = ASTNodeType.PIPE;
                pipeNode.lexeme = "|";

                Parser.ParseNode leftChild = node.children.size() > 1 ? node.children.get(1) : null;
                Parser.ParseNode rightChild = node.children.size() > 2 ? node.children.get(2) : null;

                if(leftChild != null) {
                    ArrayList<ASTNode> leftPostOrder = this.toPostfixRecursive(leftChild);
                    list.addAll(leftPostOrder);
                }

                if(rightChild != null) {
                    ArrayList<ASTNode> rightPostOrder = this.toPostfixRecursive(rightChild);
                    list.addAll(rightPostOrder);
                }

                list.add(pipeNode);

                return list;
            }

            if (node.type.equals(Parser.ParseNodeType.REPEAT)) {
                ArrayList<ASTNode> postOrder = new ArrayList<>();

                Parser.ParseNode atomNode = node.children.get(0);
                postOrder.addAll(toPostfixRecursive(atomNode));

                if (node.children.size() > 1) {
                    Parser.ParseNode repeatOpNode = node.children.get(1);
                    if (repeatOpNode.type == Parser.ParseNodeType.REPEAT_OP
                            && !repeatOpNode.children.isEmpty()) {
                        Parser.ParseNode opChild = repeatOpNode.children.get(0);
                        ASTNode opAst = mapParseNodeToAstNode(opChild);
                        postOrder.add(opAst);
                    }
                }

                return postOrder;
            }


            // apply post order traversal
            for(int i = 0 ; i < node.children.size(); i++){
                Parser.ParseNode child = node.children.get(i);
                ArrayList<ASTNode> postOrder = toPostfixRecursive(child);
                list.addAll(postOrder);
            }

            return list;
        }

        private ASTBracketNode collectBracket(Parser.ParseNode root) {
            ASTBracketNode astBracketNode = new ASTBracketNode();
            astBracketNode.lexeme = collectBracketRecursive(root);

            return  astBracketNode;
        }
        // apply in order traverse
        private String collectBracketRecursive(Parser.ParseNode node){
            String lexeme = "";
            if(node.isLeafNode() &&  node.token != null && node.token.lexeme != null) {
                lexeme = node.token.lexeme;
            }
            for(int i = 0 ; i < node.children.size(); i++){
                Parser.ParseNode  child = node.children.get(i);
                lexeme += collectBracket(child);
            }

            return  lexeme;
        }

        public ASTNode buildAST(ArrayList<ASTNode> postfix) {
            Stack<ASTNode> stack = new Stack<>();

            for (ASTNode node : postfix) {
                if (isOperand(node.type)) {
                    stack.push(node);
                }
                else if (isUnary(node.type)) {
                    if (stack.isEmpty())
                        throw new IllegalStateException("Unary operator without operand: " + node.lexeme);
                    ASTNode child = stack.pop();
                    node.children.add(child);
                    stack.push(node);
                }
                else if (isBinary(node.type)) {
                    if (stack.size() < 2)
                        throw new IllegalStateException("Binary operator without two operands: " + node.lexeme);
                    ASTNode right = stack.pop();
                    ASTNode left = stack.pop();
                    node.children.add(left);
                    node.children.add(right);
                    stack.push(node);
                }
                else {
                    throw new IllegalStateException("Unsupported node type in postfix: " + node.type);
                }
            }

            if (stack.size() != 1)
                throw new IllegalStateException("Invalid postfix, remaining stack size: " + stack.size());

            return stack.pop();
        }


        // helper methods
        private ASTNode mapParseNodeToAstNode(Parser.ParseNode parseNode){
            ASTNode astNode =new ASTNode();
            astNode.lexeme = parseNode.token.lexeme;

            astNode.type  = switch (parseNode.type){
                case BEGIN_ANCHOR   -> ASTNodeType.BEGIN_ANCHOR;
                case END_ANCHOR     -> ASTNodeType.END_ANCHOR;
                case DOT            -> ASTNodeType.DOT;
                case CHAR           -> ASTNodeType.CHAR;
                case PIPE           -> ASTNodeType.PIPE;
                case ASTERISK       -> ASTNodeType.ASTERISK;
                case PLUS           -> ASTNodeType.PLUS;
                case QUESTION       -> ASTNodeType.QUESTION;
                default -> throw new IllegalArgumentException("unsupported parse node type");
            };

            return astNode;
        }

        private void collectConcatOperands(Parser.ParseNode node, ArrayList<Parser.ParseNode> out) {
            if (node.type == Parser.ParseNodeType.CONCAT ||
                    node.type == Parser.ParseNodeType.CONCAT_PRIME) {
                for (Parser.ParseNode child : node.children) {
                    collectConcatOperands(child, out);
                }
            } else if (node.type != Parser.ParseNodeType.REGEX_PRIME) {
                out.add(node);
            }
        }

        /**
         * Check if token is operand (leaf node).
         */
        public static boolean isOperand(ASTNodeType type) {
            return switch (type) {
                case CHAR, BRACKET , BEGIN_ANCHOR, END_ANCHOR, DOT -> true;
                default -> false;
            };
        }

        /**
         * Check if token is unary operator (*, +, ?).
         */
        public static boolean isUnary(ASTNodeType type) {
            return switch (type) {
                case ASTERISK , PLUS, QUESTION -> true;
                default -> false;
            };
        }

        /**
         * Check if token is binary operator (concat, |).
         */
        public static boolean isBinary(ASTNodeType type) {
            return switch (type) {
                case CONCAT, PIPE -> true;
                default -> false;
            };
        }

        public String serializeAST(ASTNode root){
            return  this.serializeASTRecursive(root, 0);
        }

        private String serializeASTRecursive(ASTNode node, int level) {
            String serialized = "\t".repeat(level) +  String.format("{ type %s  lexeme : %s }", node.type.toString(), node.lexeme ) + "\n";
            for(int i = 0 ;i < node.children.size(); i++){
                ASTNode child = node.children.get(i);
                serialized += serializeASTRecursive(child, level + 1);
            }
            return  serialized;
        }

    }

    public static class NFABuilder {

        /**
         * Thompson Construction Algorithm — Six Core Rules
         *
         * <p>
         * This class implements the <b>Thompson Construction</b> algorithm,
         * which converts a regular expression (in postfix or AST form) into
         * a Non-deterministic Finite Automaton (NFA). Each regular expression
         * operator is handled by one of six core construction rules.
         * </p>
         *
         * <h2>Six Thompson Rules</h2>
         *
         * <ol>
         *   <li>
         *     <b>Rule 1 – Literal / Operand</b><br>
         *     Create a simple NFA for a single symbol or character class.<br>
         *     <pre>
         *     NFA:
         *       (s) --a--> (t)
         *     </pre>
         *     Example: For 'a', construct two states with a single transition labeled 'a'.<br>
         *     This also applies to wildcard '.' and bracket expressions like [a-z].
         *   </li>
         *
         *   <li>
         *     <b>Rule 2 – Concatenation (·)</b><br>
         *     Connect two NFAs A and B sequentially. The accept state of A
         *     is linked via an ε-transition (epsilon) to the start of B.
         *     <pre>
         *     A.start → ... → A.end -ε→ B.start → ... → B.end
         *     </pre>
         *     The new NFA's start = A.start, end = B.end.
         *   </li>
         *
         *   <li>
         *     <b>Rule 3 – Alternation (|)</b><br>
         *     Build a new NFA that splits into two parallel branches via ε-transitions,
         *     one for A and one for B. Then merge their accept states into a common final state.
         *     <pre>
         *          ε               ε
         *       → (s) ─────▶ A ───▶ │
         *          │               │
         *          └──────▶ B ─────┘
         *                     ε
         *                      ↓
         *                     (t)
         *     </pre>
         *     New start = s, new end = t.
         *   </li>
         *
         *   <li>
         *     <b>Rule 4 – Kleene Star (*)</b><br>
         *     Create a new NFA that allows zero or more repetitions of NFA A.<br>
         *     Add new start and end states, connect them with ε-transitions:
         *     <pre>
         *       s -ε→ A.start
         *       s -ε→ t
         *       A.end -ε→ A.start
         *       A.end -ε→ t
         *     </pre>
         *     Accepts zero or more instances of A.
         *   </li>
         *
         *   <li>
         *     <b>Rule 5 – Plus (+)</b><br>
         *     Similar to Kleene star, but requires at least one occurrence of A.<br>
         *     Implemented as concatenation of A and A*:
         *     <pre>
         *       A+ = A · A*
         *     </pre>
         *     Ensures at least one repetition before looping.
         *   </li>
         *
         *   <li>
         *     <b>Rule 6 – Optional (?)</b><br>
         *     Create a new NFA that allows either A or ε (zero or one occurrence).
         *     <pre>
         *       s -ε→ A.start
         *       s -ε→ t
         *       A.end -ε→ t
         *     </pre>
         *     Accepts presence or absence of A.
         *   </li>
         * </ol>
         *
         * <h3>Notes</h3>
         * <ul>
         *   <li>Each rule returns a new NFA object with its own start and final state.</li>
         *   <li>All new transitions use 'E' or 'ε' to denote epsilon (empty) transitions.</li>
         *   <li>Parentheses '(' and ')' affect grouping but do not produce transitions themselves.</li>
         *   <li>Anchors '^' and '$' are treated as special operands if supported by the grammar.</li>
         * </ul>
         *
         * <h3>Example Workflow</h3>
         * <pre>{@code
         *   // Postfix: "ab|*"
         *   Stack<NFA> stack = new Stack<>();
         *   for (char token : postfix.toCharArray()) {
         *       switch (token) {
         *           case 'a', 'b' -> stack.push(createLiteralNFA(token));
         *           case '.'      -> stack.push(concat(stack.pop(), stack.pop()));
         *           case '|'      -> stack.push(union(stack.pop(), stack.pop()));
         *           case '*'      -> stack.push(kleene(stack.pop()));
         *       }
         *   }
         *   NFA result = stack.pop();
         * }</pre>
         */


        ArrayList<ASTbuilder.ASTNode> postfix;


        public  NFABuilder(ArrayList<ASTbuilder.ASTNode> postfix){
            this.postfix = postfix;
        }
        public  NFABuilder(){
            this.postfix = new ArrayList<>();
        }

        public  ArrayList<ASTbuilder.ASTNode> getPostfix(){
            return  this.postfix;
        }

        public void setPostfix(ArrayList<ASTbuilder.ASTNode> postfix){
            this.postfix = postfix;
        }


        /*
       Trans - object is used as a tuple of 3 items to depict transitions
           (state from, symbol of tranistion path, state to)
   */
        public static class Trans {
            public int state_from, state_to;
            public String trans_symbol;

            // Cache for char class
            private boolean isCharClass;
            private boolean isNegated;
            private HashSet<Character> charSet; // only for positive classes
            // Note: For performance, build once in ctor if it's a class.

            public static final String EPSILON = "EPS";

            public Trans(int v1, int v2, String sym) {
                this.state_from = v1;
                this.state_to   = v2;
                this.trans_symbol = sym;

                if (sym != null && sym.length() >= 2 && sym.charAt(0) == '[' && sym.charAt(sym.length()-1) == ']') {
                    this.isCharClass = true;
                    this.isNegated   = (sym.length() > 2 && sym.charAt(1) == '^');
                    this.charSet     = buildCharSet(sym, isNegated);
                } else {
                    this.isCharClass = false;
                    this.isNegated   = false;
                    this.charSet     = null;
                }
            }

            /** Main matcher for NFA simulation */
            public boolean matches(char c) {
                if (trans_symbol == null) return false;
                if (EPSILON.equals(trans_symbol)) return false;
                if (".".equals(trans_symbol)) return true;

                if (isCharClass) {
                    boolean hit = charSet.contains(c);
                    return isNegated ? !hit : hit;
                }

                // literal
                return trans_symbol.length() == 1 && trans_symbol.charAt(0) == c;
            }

            /** Robust char-class parser: supports ranges, literals, negation */
            private static HashSet<Character> buildCharSet(String sym, boolean negated) {
                HashSet<Character> set = new HashSet<>();
                if (sym == null || sym.length() < 2) return set;

                int i = negated ? 2 : 1;
                int end = sym.length() - 1;

                // Special cases: '-' literal if at start or end (e.g., "[-a]" or "[a-]")
                // and ']' literal if placed first (POSIX style) – you can extend if needed.

                while (i < end) {
                    char a = sym.charAt(i);

                    // Escape handling (optional – only if lexer emits backslashes)
                    if (a == '\\' && i + 1 < end) {
                        set.add(sym.charAt(i + 1));
                        i += 2;
                        continue;
                    }

                    if ((i + 2) < end && sym.charAt(i + 1) == '-') {
                        // Range a-b
                        char b = sym.charAt(i + 2);
                        if (a <= b) { // guard bad ranges
                            for (char ch = a; ch <= b; ch++) set.add(ch);
                        } else {
                            // If reversed (like [z-a]), either ignore or swap
                            for (char ch = b; ch <= a; ch++) set.add(ch);
                        }
                        i += 3;
                    } else {
                        // Single literal char (allow ANY, not just letterOrDigit)
                        set.add(a);
                        i += 1;
                    }
                }
                return set;
            }

            // If you still want these helpers, make them safe:

            /** Safe: works only for char-classes; otherwise returns empty set */
            public HashSet<String> getCharacterSet() {
                HashSet<String> out = new HashSet<>();
                if (!isCharClass || charSet == null) return out;
                for (char ch : charSet) out.add(String.valueOf(ch));
                return out;
            }

            /** Safe negation flag for char-classes */
            public boolean getIsNegated() {
                return isCharClass && isNegated;
            }
        }

        /*
            NFA - serves as the graph that represents the Non-Deterministic
                Finite Automata. Will use this to better combine the states.
        */
        public static class NFA{

            public static String EPSILON = "EPS";
            public ArrayList <Integer> states;
            public ArrayList <Trans> transitions;
            public int final_state;

            public NFA(){
                this.states = new ArrayList <Integer> ();
                this.transitions = new ArrayList <Trans> ();
                this.final_state = 0;
            }
            public NFA(int size){
                this.states = new ArrayList <Integer> ();
                this.transitions = new ArrayList <Trans> ();
                this.final_state = 0;
                this.setStateSize(size);
            }
            public NFA(String c){
                this.states = new ArrayList<Integer> ();
                this.transitions = new ArrayList <Trans> ();
                this.setStateSize(2);
                this.final_state = 1;
                this.transitions.add(new Trans(0, 1, c));
            }

            public void setStateSize(int size){
                for (int i = 0; i < size; i++)
                    this.states.add(i);
            }

            public void display(){
                for (Trans t: transitions){
                    System.out.println("("+ t.state_from +", "+ t.trans_symbol +
                            ", "+ t.state_to +")");
                }
            }
        }


        public NFA ThompsonMethod() {
            Stack<NFA> stack = new Stack<>();


            for(ASTbuilder.ASTNode astNode : this.postfix){

                if(astNode.type.equals(ASTbuilder.ASTNodeType.CHAR) ||
                astNode.type.equals(ASTbuilder.ASTNodeType.BRACKET) ||
                astNode.type.equals(ASTbuilder.ASTNodeType.DOT)) {
                    // start ----(symbol)----> final
                    NFA nfa = new NFA();
                    nfa.states.addAll(Arrays.asList(0, 1));
                    Trans transition = new Trans(0,1, astNode.lexeme);
                    nfa.transitions.add(transition);
                    nfa.final_state = 1;

                    stack.add(nfa);
                }

                else if(astNode.type.equals(ASTbuilder.ASTNodeType.CONCAT)) {
                    // left ex. [State(0) ---(a) ---> State(1)] ---- epsilon ---> rigth ex. [State(2) ----b---> State(3)]
                    NFA right = stack.pop();
                    NFA left = stack.pop();
                    // create concatenation  NFA
                    NFA nfa = new NFA();

                    int leftOffset = left.states.size();
                    nfa.transitions.addAll(left.transitions);
                    nfa.states.addAll(left.states);

                    Trans epsilonTransition = new Trans(left.final_state, leftOffset, NFA.EPSILON);
                    nfa.transitions.add(epsilonTransition);

                    int rightOffset = left.states.size();

                    for(int rightState : right.states) {
                        nfa.states.add(rightState + rightOffset);
                    }

                    for (Trans rt : right.transitions) {
                        nfa.transitions.add(
                                new Trans(
                                        rt.state_from + rightOffset,
                                        rt.state_to + rightOffset,
                                        rt.trans_symbol
                                )
                        );
                    }

                    nfa.final_state = right.final_state + rightOffset;

                    stack.add(nfa);
                }

                else if(astNode.type.equals(ASTbuilder.ASTNodeType.PIPE)) {
                    /**
                     *           |-- Transiton(E)---- left [State(1) ---- Transiton(lexeme) ---- State(2) ]--- Transiton(E)--------|
                     *  State(0) |                                                                                          | -- State(5) final state
                     *           |__ Transition(E) ----- right[State(3) ---- Transition(lexeme) ----- State(4)] --- Transition(E)---|
                     */

                    // union nfa (Pipe) |
                    NFA nfa = new NFA();
                    NFA right = stack.pop();
                    NFA left = stack.pop();

                    nfa.states.add(0);
                    nfa.transitions.add( new Trans(0, 1, NFA.EPSILON));
                    nfa.transitions.add( new Trans(0, left.final_state + 2, NFA.EPSILON));

                    for(int i = 0; i< left.states.size(); i++) {
                        nfa.states.add(left.states.get(i) + 1);
                    }

                    for(Trans t : left.transitions){
                        nfa.transitions.add(new Trans(
                                t.state_from + 1,
                                t.state_to + 1,
                                t.trans_symbol
                        ));
                    }


                    int rightOffset = left.states.size() + 1;

                    for(int i = 0; i< right.states.size(); i++) {
                        nfa.states.add(right.states.get(i) + rightOffset);
                    }

                    for(Trans t : right.transitions){
                        nfa.transitions.add(new Trans(
                                t.state_from + rightOffset,
                                t.state_to + rightOffset,
                                t.trans_symbol
                        ));
                    }

                    nfa.states.add(right.final_state + rightOffset + 1);
                    nfa.final_state = right.final_state + rightOffset + 1;
                    nfa.transitions.add( new Trans(left.final_state + 1, nfa.final_state, NFA.EPSILON ));
                    nfa.transitions.add( new Trans(right.final_state + rightOffset, nfa.final_state, NFA.EPSILON ));

                    stack.add(nfa);
                }

                else if(astNode.type.equals(ASTbuilder.ASTNodeType.ASTERISK)) {
                    /**
                     * --------------------E------------|
                     (0) --ε--> [(1) --a--> (2) ]--ε--> (3)
                                ↑            │
                                |----ε-------┘
                     */

                    NFA nfa = new NFA();
                    nfa.states.add(0);
                    nfa.transitions.add(new Trans(0, 1, NFA.EPSILON));
                    NFA child = stack.pop();
                    int childOfffeset = 1;
                    for(int state : child.states){
                        nfa.states.add(state +childOfffeset);
                    }
                    for(Trans transition : child.transitions){
                        nfa.transitions.add(new Trans(
                                transition.state_from + childOfffeset,
                                transition.state_to + childOfffeset,
                                transition.trans_symbol));
                    }

                    nfa.transitions.add(new Trans(child.final_state + childOfffeset, 1, NFA.EPSILON));
                    nfa.states.add(child.final_state + childOfffeset + 1);
                    nfa.final_state = child.final_state + childOfffeset + 1;
                    nfa.transitions.add(new Trans(child.final_state + childOfffeset, nfa.final_state, NFA.EPSILON));
                    nfa.transitions.add(new Trans(0, nfa.final_state, NFA.EPSILON));
                    stack.add(nfa);
                }

                else if(astNode.type.equals(ASTbuilder.ASTNodeType.PLUS)) {
                    /**
                     (0) --ε--> [(1) --a--> (2) ]--ε--> (3)
                                ↑            │
                                |----ε-------┘
                     */

                    NFA nfa = new NFA();
                    nfa.states.add(0);
                    nfa.transitions.add(new Trans(0, 1, NFA.EPSILON));
                    NFA child = stack.pop();
                    int childOfffeset = 1;
                    for(int state : child.states){
                        nfa.states.add(state +childOfffeset);
                    }
                    for(Trans transition : child.transitions){
                        nfa.transitions.add(new Trans(
                                transition.state_from + childOfffeset,
                                transition.state_to + childOfffeset,
                                transition.trans_symbol));
                    }

                    nfa.transitions.add(new Trans(child.final_state + childOfffeset, 1, NFA.EPSILON));
                    nfa.states.add(child.final_state + childOfffeset + 1);
                    nfa.final_state = child.final_state + childOfffeset + 1;
                    nfa.transitions.add(new Trans(child.final_state + childOfffeset, nfa.final_state, NFA.EPSILON));
                    stack.add(nfa);
                }

                else if(astNode.type.equals(ASTbuilder.ASTNodeType.QUESTION)) {
                    /**
                     *   (0) --ε--> [(1) --a--> (2)] --ε--> (3)
                     *    \________________ε________________/
                     */

                    NFA nfa = new NFA();
                    NFA child = stack.pop();
                    int childOffset = 1;
                    nfa.states.add(0);
                    nfa.transitions.add(new Trans(0, 1 , NFA.EPSILON));

                    for(int state : child.states){
                        nfa.states.add(state + childOffset);
                    }

                    for(Trans t: child.transitions){
                        nfa.transitions.add(new Trans(
                                t.state_from + childOffset,
                                t.state_to + childOffset,
                                t.trans_symbol));
                    }

                    nfa.states.add(child.final_state + childOffset + 1);
                    nfa.final_state = child.final_state + childOffset + 1;
                    nfa.transitions.add(new Trans(child.final_state + childOffset,
                            nfa.final_state,
                            NFA.EPSILON));
                    nfa.transitions.add(new Trans(0,
                            nfa.final_state,
                            NFA.EPSILON));

                    stack.add(nfa);
                }

            }

            return stack.pop();

        }
    }

    public static class ReModule{
        private Lexer lexer;
        private  Parser parser;
        private ASTbuilder astBuilder;
        private NFABuilder nfaBuilder;


        /**
         * Checks whether the given input string matches the provided regular expression
         * using NFA simulation based on the Thompson Construction algorithm.
         *
         * Algorithm Steps:
         * 1. Tokenize the input regex using the Lexer to create a token stream.
         * 2. Parse the token stream into an Abstract Syntax Tree (AST) using the Parser.
         * 3. Convert the AST into postfix notation with the ASTBuilder.
         * 4. Build the NFA using the ThompsonMethod of NFABuilder.
         * 5. Initialize currentStates as the epsilon-closure of the start state (0).
         * 6. For each character 'c' in the input pattern:
         *    - Create an empty nextStates set.
         *    - For every state 's' in currentStates:
         *         * For each transition 't' starting from 's':
         *             - If t.matches(c) is true, add t.state_to into nextStates.
         *    - Replace currentStates with the union of epsilon-closures of all states in nextStates.
         * 7. After consuming all characters, if currentStates contains the NFA's final state,
         *    the pattern is accepted. Otherwise, it is rejected.
         *
         * This method performs exact match checking. For substring match, run the simulation
         * starting from every position in the input string.
         *
         * @param regex   the regular expression to test (e.g. "a(b|c)*")
         * @param pattern the input string (e.g. "abcc")
         * @return true if the input matches the regex exactly, false otherwise
         * @throws Exception if the regex cannot be tokenized or parsed
         */


        public boolean isExactMatch(String regex, String pattern) throws Exception{
            lexer = new Lexer();
            ArrayList<Lexer.Token> tokenStream = lexer.createTokenStream(regex);
            parser = new Parser();
            parser.setTokenStream(tokenStream);
            Parser.ParseNode root = parser.parse();
            astBuilder = new ASTbuilder();
            ArrayList<ASTbuilder.ASTNode> postfix = astBuilder.toPostfixRecursive(root);

            nfaBuilder = new NFABuilder();
            nfaBuilder.setPostfix(postfix);
            NFABuilder.NFA nfa = nfaBuilder.ThompsonMethod();
            // simulate NFA

            // starting states
            Set<Integer> currentStates = EpsilonClosure(nfa, 0);

            for(char c : pattern.toCharArray()){
                HashSet<Integer> nextStates = new HashSet<>();
                for(int state : currentStates){
                    for(NFABuilder.Trans transition : nfa.transitions){
                        if(transition.state_from == state && transition.matches(c)) {
                            nextStates.add(transition.state_to);
                        }
                    }
                }

                currentStates.clear();
                for(int state : nextStates) {
                    currentStates.addAll(EpsilonClosure(nfa, state));
                }
            }

            return currentStates.contains(nfa.final_state);
        }

        public  boolean isSubstringMatch(String regex, String pattern) throws Exception {
            for(int i =0; i < pattern.length(); i++){
                for(int j = i ; j < pattern.length(); j++){
                    String substring = pattern.substring(i, j+1);
                    if(isExactMatch(regex, substring)) return  true;
                }
            }
            return  false;
        }

        private HashSet<Integer> EpsilonClosure(NFABuilder.NFA nfa, int state){
            HashSet<Integer> set = new HashSet<>();
            set.add(state);
            Stack<Integer> stack = new Stack<>();
            stack.add(state);

            while (!stack.isEmpty()) {
                int curState = stack.pop();

                List<Integer> nextStates =  nfa.transitions.stream()
                        .filter(x -> x.state_from == curState && NFABuilder.NFA.EPSILON.equals(x.trans_symbol))
                        .map(x -> x.state_to)
                        .toList();
                for(int s : nextStates) {
                    if(!set.contains(s)) {
                        set.add(s);
                        stack.add(s);
                    }
                }
            }

            return set;
        }

    }

    public static void test1() throws Exception{
        String regex = "[A-Z][a-z]+[0-9]?@(gmail|hotmail|yahoo).com";
        ReModule engine = new ReModule();
        System.out.println("-".repeat(30));
        System.out.println(String.format("regex %s " , regex));
        System.out.println("valid cases (true)");
        System.out.println(engine.isExactMatch(regex, "John@gmail.com")); // should be true
        System.out.println(engine.isExactMatch(regex, "Alice1@hotmail.com"));  // should be true
        System.out.println(engine.isExactMatch(regex, "Mark@yahoo.com"));  // should be true
        System.out.println(engine.isExactMatch(regex, "David9@gmail.com"));  // should be true
        System.out.println("\n");
        System.out.println("invalid cases (false)");
        System.out.println(engine.isExactMatch("[A-Z][a-z]+[0-9]?@(gmail|hotmail|yahoo).com", "john@gmail.com")); // should be false
        System.out.println(engine.isExactMatch("[A-Z][a-z]+[0-9]?@(gmail|hotmail|yahoo).com", "John12@gmail.com"));// should be false
        System.out.println(engine.isExactMatch("[A-Z][a-z]+[0-9]?@(gmail|hotmail|yahoo).com", "John@outlook.com"));// should be false
        System.out.println(engine.isExactMatch("[A-Z][a-z]+[0-9]?@(gmail|hotmail|yahoo).com", "John@gmail.co"));// should be false
        System.out.println(engine.isExactMatch("[A-Z][a-z]+[0-9]?@(gmail|hotmail|yahoo).com", "John@gmail.com.tr"));// should be false
        System.out.println("-".repeat(30));
    }

    public static void test2() throws Exception {
        String regex = "[A-Z][a-z]+[0-9]*_(dev|test|prod)";
        ReModule engine = new ReModule();
        System.out.println("-".repeat(30));
        System.out.println(String.format("regex %s ", regex));
        System.out.println("valid cases (true)");
        System.out.println(engine.isExactMatch(regex, "John_dev"));       // true
        System.out.println(engine.isExactMatch(regex, "Alice1_test"));    // true
        System.out.println(engine.isExactMatch(regex, "Mark22_prod"));    // true
        System.out.println(engine.isExactMatch(regex, "David_prod"));     // true

        System.out.println("\ninvalid cases (false)");
        System.out.println(engine.isExactMatch(regex, "john_dev"));       // false
        System.out.println(engine.isExactMatch(regex, "John_prod_test")); // false
        System.out.println(engine.isExactMatch(regex, "John"));           // false
        System.out.println(engine.isExactMatch(regex, "John_stage"));     // false
        System.out.println("-".repeat(30));
    }

    public static void test3() throws Exception {
        String regex = "[a-zA-Z0-9_]+-[A-Za-z]+-v[0-9]+.(txt|csv|json|xml|log)";
        ReModule engine = new ReModule();
        System.out.println("-".repeat(30));
        System.out.println(String.format("regex %s ", regex));
        System.out.println("valid cases (true)");
        System.out.println(engine.isExactMatch(regex, "dataParser-core-v1.txt"));     // true
        System.out.println(engine.isExactMatch(regex, "UserAuth-module-v3.csv"));     // true
        System.out.println(engine.isExactMatch(regex, "backupSystem-main-v45.log"));  // true
        System.out.println(engine.isExactMatch(regex, "analyzer-beta-v7.xml"));       // true

        System.out.println("\ninvalid cases (false)");
        System.out.println(engine.isExactMatch(regex, "dataparser-core.txt"));        // false
        System.out.println(engine.isExactMatch(regex, "dataParser-v2.pdf"));          // false
        System.out.println(engine.isExactMatch(regex, "core-v01txt"));                // false
        System.out.println(engine.isExactMatch(regex, "core-v1-txt"));                // false
        System.out.println(engine.isExactMatch(regex, "core-module-v.log"));          // false
        System.out.println("-".repeat(30));
    }

}




