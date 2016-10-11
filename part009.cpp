#include <iostream>
#include <string>
#include <sstream>
#include <memory>
#include <cctype>
#include <vector>
#include <map>

/*
 * PASCAL GRAMMAR RULES
 *
 * program: compound_statement DOT
 * compound_statement: BEGIN statement_list END
 * statement_list: statement
 *               | statement SEMI statement_list
 * statement: compound_statement
 *          | assignment_statement
 *          | empty
 * assignment_statement: variable ASSIGN expr
 * empty:
 * expr: term ((PLUS | MINUS) term)*
 * term: factor ((MUL | DIV) factor)*
 * factor: PLUS factor
 *       | MINUS factor
 *       | INTEGER
 *       | LPAREN expr RPAREN
 *       | variable
 * variable: ID
 *
 */

/*
 * EXAMPLE PASCAL PROGRAM
 *
 * BEGIN
 *   BEGIN
 *     number := 2;
 *     a := number;
 *     b := 10 * a + 10 * number / 4;
 *     c := a - - b;
 *   END;
 *   x := 11;
 * END.
 *
 */

struct Tokens {
    enum Type {
        Integer,
        Plus,
        Minus,
        Multiply,
        Divide,
        LParen,
        RParen,
        Assign,
        Semicolon,
        Begin,
        End,
        Dot,
        ID,
        EndOfFile,
    };

    static std::string typeToString(Type t) {
        switch (t) {
        case Integer:
            return "INTEGER";
        case Plus:
            return "PLUS";
        case Minus:
            return "MINUS";
        case Multiply:
            return "MULT";
        case Divide:
            return "DIV";
        case LParen:
            return "LPAREN";
        case RParen:
            return "RPAREN";
        case Assign:
            return "ASSIGN";
        case Semicolon:
            return "SEMI";
        case Begin:
            return "BEGIN";
        case End:
            return "END";
        case Dot:
            return "DOT";
        case ID:
            return "ID";
        case EndOfFile:
            return "EOF";
        }
        return "";
    }
};

struct ReservedKeywords {
    static const std::map<std::string, Tokens::Type> keywordMap;
};

const std::map<std::string, Tokens::Type> ReservedKeywords::keywordMap = {
    {"BEGIN", Tokens::Begin},
    {"END", Tokens::End},
};

template <typename K, typename V>
std::ostream& operator<< (std::ostream& os, const std::map<K, V>& m) {
    os << "{ ";
    auto it = begin(m);
    if (it != end(m)) {
        os << it->first << ": " << it->second;
    }
    while (++it != end(m)) {
        os << ", " << it->first << ": " << it->second;
    }
    return (os << " }");
}

class Token {
public:
    static Token fromReservedKeywordOrId(const std::string& str) {
        auto it = ReservedKeywords::keywordMap.find(str);
        if (it != ReservedKeywords::keywordMap.end()) {
            return Token(it->second);
        }
        return Token(Tokens::ID, str);
    }

public:
    Token():_type(Tokens::EndOfFile) {}
    Token(Tokens::Type type):_type(type) {}
    Token(Tokens::Type type, const std::string& value):_type(type),_value(value) {
    }

    std::string description() const {
        std::stringstream ss;
        ss << "Token(" << Tokens::typeToString(_type) << ", " << _value << ")";

        return ss.str();
    }

    Tokens::Type type() const {
        return _type;
    }

    std::string value() const {
        return _value;
    }

    int valueAsInt() const {
        int ret;
        std::stringstream ss(_value);
        ss >> ret;
        return ret;
    }

    bool isTypeOf(Tokens::Type type) const {
        return _type == type;
    }

    template <typename T, typename... Targs>
    bool isAnyTypeOf(T type, Targs... types) const {
        return isTypeOf(type) || isAnyTypeOf(types...);
    }

    bool isAnyTypeOf(Tokens::Type type) const {
        return isTypeOf(type);
    }

private:
    Tokens::Type _type;
    std::string _value;
};

struct interpreter_result_t {
    int value;
};

std::ostream& operator<< (std::ostream& os, const interpreter_result_t& res) {
    return (os << res.value);
}

class Lexer {
public:
    Lexer(const std::string& text):
        _text(text),_pos(0)
    {
        _currentChar = text[_pos];
        _eof = false;
    }

    Lexer(const Lexer& lexer):
        _text(lexer._text),_pos(0)
    {
        _currentChar = _text[_pos];
        _eof = false;
    }

    void error() {
        throw std::runtime_error("Invalid character");
    }

    void advance() {
        _pos += 1;
        if (_pos > _text.size() - 1) {
            _currentChar = 0;
            _eof = true;
        } else {
            _currentChar = _text[_pos];
        }
    }

    void skipWhitespace() {
        while (!_eof && isspace(_currentChar)) {
            advance();
        }
    }

    std::string integer() {
        std::stringstream ss;

        while (!_eof && isdigit(_currentChar)) {
            ss << _currentChar;
            advance();
        }

        return ss.str();
    }

    char peek() {
        auto peek_pos = _pos + 1;
        if (peek_pos > _text.size() - 1) {
            return 0;
        }
        return _text[peek_pos];
    }

    Token getNextToken() {
        while (!_eof) {
            if (isspace(_currentChar)) {
                skipWhitespace();
                continue;
            }

            if (isalpha(_currentChar)) {
                return _id();
            }

            if (_currentChar == ':' && peek() == '=') {
                advance();
                advance();
                return Token(Tokens::Assign);
            }

            if (_currentChar == ';') {
                advance();
                return Token(Tokens::Semicolon);
            }

            if (_currentChar == '.') {
                advance();
                return Token(Tokens::Dot);
            }

            if (isdigit(_currentChar)) {
                return Token(Tokens::Integer, integer());
            }

            if (_currentChar == '+') {
                advance();
                return Token(Tokens::Plus);
            }

            if (_currentChar == '-') {
                advance();
                return Token(Tokens::Minus);
            }

            if (_currentChar == '*') {
                advance();
                return Token(Tokens::Multiply);
            }

            if (_currentChar == '/') {
                advance();
                return Token(Tokens::Divide);
            }

            if (_currentChar == '(') {
                advance();
                return Token(Tokens::LParen);
            }

            if (_currentChar == ')') {
                advance();
                return Token(Tokens::RParen);
            }



            error();
        }

        return Token(Tokens::EndOfFile);
    }

private:
    Token _id() {
        std::stringstream ss;
        while (!_eof && isalnum(_currentChar)) {
            ss << _currentChar;
            advance();
        }

        return Token::fromReservedKeywordOrId(ss.str());
    }

private:
    std::string _text;
    int _pos;
    Token _currentToken;
    char _currentChar;
    bool _eof;
};

class VisitorNode;
class AST;
class BinOp;
class UnaryOp;
class Num;
class Compound;
class Assign;
class Var;
class NoOp;

class NodeVisitor {
public:
    virtual void visit(const VisitorNode& node) = 0;
    virtual void visit(const BinOp& node) = 0;
    virtual void visit(const UnaryOp& node) = 0;
    virtual void visit(const Num& node) = 0;
    virtual void visit(const Compound& node) = 0;
    virtual void visit(const Assign& node) = 0;
    virtual void visit(const Var& node) = 0;
    virtual void visit(const NoOp& node) = 0;
};

class VisitorNode {
public:
    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
};

class AST: public VisitorNode {
public:
    AST() {}
    virtual ~AST() {}

    virtual std::string description() const {
        return "AST(empty)";
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
};

class BinOp: public AST {
public:
    BinOp(std::unique_ptr<AST> left, Tokens::Type op, std::unique_ptr<AST> right):
        _left(std::move(left)),_op(op),_right(std::move(right)) {
    }

    const AST& getLeft() const {
        return *_left;
    }

    const AST& getRight() const {
        return *_right;
    }

    const Tokens::Type& getOp() const {
        return _op;
    }

    virtual std::string description() const {
        std::stringstream ss;
        ss << "BinOp(" << _left->description() << ", ";
        ss << Tokens::typeToString(_op) << ", ";
        ss << _right->description() << ")";
        return ss.str();
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    std::unique_ptr<AST> _left;
    std::unique_ptr<AST> _right;
    Tokens::Type _op;
};

class UnaryOp: public AST {
public:
    UnaryOp(Tokens::Type op, std::unique_ptr<AST> expr):
        _op(op),_expr(std::move(expr)) {
    }

    Tokens::Type getOp() const {
        return _op;
    }

    const AST& getExpr() const {
        return *_expr;
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }

private:
    Tokens::Type _op;
    std::unique_ptr<AST> _expr;
};

class Num: public AST {
public:
    Num(Token token):_value(token.valueAsInt()) {
    }

    int getValue() const {
        return _value;
    }

    virtual std::string description() const {
        std::stringstream ss;
        ss << "Num(" << _value << ")";
        return ss.str();
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    int _value;
};

class Compound: public AST {
public:
    typedef std::vector<std::unique_ptr<AST> >::const_iterator const_iterator;
public:
    Compound():_children() {}
    Compound(std::vector<std::unique_ptr<AST> > nodes):
        _children(std::move(nodes)) {
    }

    const_iterator begin() const {
        return _children.begin();
    }

    const_iterator cbegin() const {
        return _children.cbegin();
    }

    const_iterator end() const {
        return _children.end();
    }

    const_iterator cend() const {
        return _children.end();
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    std::vector<std::unique_ptr<AST> > _children;
};

class Assign: public AST {
public:
    Assign(std::unique_ptr<Var> left, Tokens::Type op, std::unique_ptr<AST> right):
        _left(std::move(left)),_right(std::move(right)),_op(op) {
    }

    const Var& getLeft() const {
        return *_left;
    }

    const AST& getRight() const {
        return *_right;
    }

    Tokens::Type getOp() const {
        return _op;
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    std::unique_ptr<Var> _left;
    std::unique_ptr<AST> _right;
    Tokens::Type _op;
};

class Var: public AST {
public:
    Var(Token token):_token(token) {}

    Token getToken() const {
        return _token;
    }

    std::string getValue() const {
        return _token.value();
    }

    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
private:
    Token _token;
};

class NoOp: public AST {
    virtual void accept(NodeVisitor& v) const {
        v.visit(*this);
    }
};

class Parser {
public:
    Parser(Lexer lexer):_lexer(lexer) {
        _currentToken = _lexer.getNextToken();
    }

    Parser(const Parser& parser):_lexer(parser._lexer) {
        _currentToken = _lexer.getNextToken();
    }

    void error() {
        throw std::runtime_error("Invalid syntax");
    }

    void eat(Tokens::Type type) {
        if (_currentToken.type() == type) {
            _currentToken = _lexer.getNextToken();
        } else {
            std::cerr << "Expected " << Tokens::typeToString(type) << " but got " << Tokens::typeToString(_currentToken.type()) << std::endl;
            error();
        }
    }

    std::unique_ptr<AST> factor() {
        Token token = _currentToken;
        if (token.type() == Tokens::Plus) {
            eat(Tokens::Plus);
            return std::make_unique<UnaryOp>(token.type(), factor());
        } else if (token.type() == Tokens::Minus) {
            eat(Tokens::Minus);
            return std::make_unique<UnaryOp>(token.type(), factor());
        } else if (token.type() == Tokens::Integer) {
            eat(Tokens::Integer);
            return std::make_unique<Num>(token);
        } else if (token.type() == Tokens::LParen) {
            eat(Tokens::LParen);
            auto node = expr();
            eat(Tokens::RParen);
            return node;
        }
        return variable();
    }

    std::unique_ptr<AST> term() {
        auto node = factor();

        while (_currentToken.isAnyTypeOf(Tokens::Multiply, Tokens::Divide)) {
            Token token = _currentToken;
            if (token.type() == Tokens::Multiply) {
                eat(Tokens::Multiply);
            } else if (token.type() == Tokens::Divide) {
                eat(Tokens::Divide);
            }

            node = std::make_unique<BinOp>(std::move(node), token.type(), std::move(factor()));
        }

        return node;
    }

    std::unique_ptr<AST> expr() {
        auto node = term();

        while (_currentToken.isAnyTypeOf(Tokens::Plus, Tokens::Minus)) {
            Token token = _currentToken;
            if (token.type() == Tokens::Plus) {
                eat(Tokens::Plus);
            } else if (token.type() == Tokens::Minus) {
                eat(Tokens::Minus);
            }
            node = std::make_unique<BinOp>(std::move(node), token.type(), std::move(term()));
        }

        return node;
    }

    std::unique_ptr<AST> program() {
        auto node = compoundStatement();
        eat(Tokens::Dot);
        return node;
    }

    std::unique_ptr<AST> compoundStatement() {
        eat(Tokens::Begin);
        auto nodes = statementList();
        eat(Tokens::End);

        std::unique_ptr<AST> compound = std::make_unique<Compound>(std::move(nodes));
        return compound;
    }

    std::vector<std::unique_ptr<AST> > statementList() {
        std::vector<std::unique_ptr<AST> > results;
        results.push_back(std::move(statement()));

        while (_currentToken.type() == Tokens::Semicolon) {
            eat(Tokens::Semicolon);
            results.push_back(statement());
        }

        if (_currentToken.type() == Tokens::ID) {
            error();
        }

        return results;
    }

    std::unique_ptr<AST> statement() {
        std::unique_ptr<AST> node;
        if (_currentToken.type() == Tokens::Begin) {
            node = compoundStatement();
        } else if (_currentToken.type() == Tokens::ID) {
            node = assignStatement();
        } else {
            node = empty();
        }
        return node;
    }

    std::unique_ptr<AST> assignStatement() {
        auto left = variable();
        auto token = _currentToken;
        eat(Tokens::Assign);
        auto right = expr();
        std::unique_ptr<AST> node = std::make_unique<Assign>(std::move(left), token.type(), std::move(right));

        return node;
    }

    std::unique_ptr<Var> variable() {
        auto node = std::make_unique<Var>(_currentToken);
        eat(Tokens::ID);

        return node;
    }

    std::unique_ptr<AST> parse() {
        auto node = program();
        if (_currentToken.type() != Tokens::EndOfFile) {
            error();
        }
        return node;
    }

    std::unique_ptr<NoOp> empty() {
        return std::make_unique<NoOp>();
    }

private:
    Lexer _lexer;
    Token _currentToken;
};

class Interpreter: public NodeVisitor {
public:
    Interpreter(Parser& parser):_parser(parser) {
    }

    virtual void visit(const VisitorNode& node) {
        throw std::runtime_error("visitor method must be overridden for type");
    }

    virtual void visit(const Num& node) {
        _result.value = node.getValue();
    }

    virtual void visit(const UnaryOp& node) {
        node.getExpr().accept(*this);

        if (node.getOp() == Tokens::Minus) {
            _result.value = -_result.value;
        }
    }

    virtual void visit(const BinOp& node) {
        node.getLeft().accept(*this);
        auto left = _result.value;

        node.getRight().accept(*this);
        auto right = _result.value;

        auto type = node.getOp();
        if (type == Tokens::Plus) {
            _result.value = left + right;
        } else if (type == Tokens::Minus) {
            _result.value = left - right;
        } else if (type == Tokens::Multiply) {
            _result.value = left * right;
        } else if (type == Tokens::Divide) {
            _result.value = left / right;
        }
    }

    virtual void visit(const Compound& node) {
        for (auto it = std::begin(node); it != std::end(node); it++) {
            it->get()->accept(*this);
        }
    }

    virtual void visit(const Assign& node) {
        auto name = node.getLeft().getValue();
        node.getRight().accept(*this);
        _globalScope[name] = _result.value;
    }

    virtual void visit(const Var& node) {
        auto name = node.getValue();
        _result.value = _globalScope[name];
    }

    virtual void visit(const NoOp& node) {
    }

    interpreter_result_t interpret() {
        _result = interpreter_result_t { 0 };
        auto node = _parser.parse();
        node->accept(*this);
        return _result;
    }

    void printGlobalScope() const {
        std::cout << _globalScope << std::endl;
    }

private:
    Parser _parser;
    interpreter_result_t _result;
    std::map<std::string, int> _globalScope;
};

class ReversePolishNotationTranslator: public NodeVisitor {
public:
    ReversePolishNotationTranslator(Parser& parser):_parser(parser) {
    }

    virtual void visit(const Num& node) {
        _ss << node.getValue();
    }

    virtual void visit(const VisitorNode& node) {
        throw std::runtime_error("visitor method must be overridden for type");
    }

    virtual void visit(const UnaryOp& node) {
        node.getExpr().accept(*this);

        _ss << " -1 *";
    }

    virtual void visit(const BinOp& node) {
        node.getLeft().accept(*this);

        _ss << " ";

        node.getRight().accept(*this);

        _ss << " ";

        auto type = node.getOp();
        if (type == Tokens::Plus) {
            _ss << "+";
        } else if (type == Tokens::Minus) {
            _ss << "-";
        } else if (type == Tokens::Multiply) {
            _ss << "*";
        } else if (type == Tokens::Divide) {
            _ss << "/";
        }
    }

    virtual void visit(const Compound& node) {
    }

    virtual void visit(const Assign& node) {
    }

    virtual void visit(const Var& node) {
    }

    virtual void visit(const NoOp& node) {
    }

    std::string translate() {
        _ss.str("");
        _ss.clear();

        auto node = _parser.parse();
        node->accept(*this);

        return _ss.str();
    }

private:
    Parser _parser;
    std::stringstream _ss;
};

class LispTranslator: public NodeVisitor {
public:
    LispTranslator(Parser& parser):_parser(parser),_ss() {
    }

    virtual void visit(const Num& node) {
        _ss << node.getValue();
    }

    virtual void visit(const VisitorNode& node) {
        throw std::runtime_error("visitor method must be overridden for type");
    }

    virtual void visit(const UnaryOp& node) {
        _ss << "(";

        auto type = node.getOp();
        if (type == Tokens::Plus) {
            _ss << "+";
        } else if (type == Tokens::Minus) {
            _ss << "-";
        }

        _ss << " ";

        node.getExpr().accept(*this);

        _ss << ")";
    }

    virtual void visit(const BinOp& node) {
        _ss << "(";

        auto type = node.getOp();
        if (type == Tokens::Plus) {
            _ss << "+";
        } else if (type == Tokens::Minus) {
            _ss << "-";
        } else if (type == Tokens::Multiply) {
            _ss << "*";
        } else if (type == Tokens::Divide) {
            _ss << "/";
        }

        _ss << " ";

        node.getLeft().accept(*this);

        _ss << " ";

        node.getRight().accept(*this);
        _ss << ")";
    }

    virtual void visit(const Compound& node) {
    }

    virtual void visit(const Assign& node) {
    }

    virtual void visit(const Var& node) {
    }

    virtual void visit(const NoOp& node) {
    }

    std::string translate() {
        _ss.str("");
        _ss.clear();

        auto node = _parser.parse();
        node->accept(*this);

        return _ss.str();
    }

private:
    Parser _parser;
    std::stringstream _ss;
};

int main(int argc, char** argv) {

    std::string text = "\
        BEGIN\
            BEGIN\
                number := 2;\
                a := number;\
                b := 10 * a + 10 * number / 4;\
                c := a - - b\
            END;\
            x := 11;\
        END.\
    ";

    Lexer lexer(text);
    Parser parser(lexer);
    Interpreter interpreter(parser);
    interpreter.interpret();
    interpreter.printGlobalScope();

    return 0;
}


