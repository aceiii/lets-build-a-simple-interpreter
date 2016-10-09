#include <iostream>
#include <string>
#include <sstream>
#include <memory>

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
        case EndOfFile:
            return "EOF";
        }
        return "";
    }
};

class Token {
public:
    Token():_type(Tokens::EndOfFile),_value(0) {}
    Token(Tokens::Type type):_type(type),_value(0) {}
    Token(Tokens::Type type, int value):_type(type),_value(value) {}
    Token(Tokens::Type type, const std::string& value):_type(type),_value(0) {
        std::stringstream ss(value);
        ss >> _value;
    }

    std::string description() const {
        std::stringstream ss;
        ss << "Token(" << Tokens::typeToString(_type) << ", " << _value << ")";

        return ss.str();
    }

    Tokens::Type type() const {
        return _type;
    }

    int value() const {
        return _value;
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
    int _value;
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

    int integer() {
        int result;
        std::stringstream ss;

        while (!_eof && isdigit(_currentChar)) {
            ss << _currentChar;
            advance();
        }

        ss >> result;

        return result;
    }

    Token getNextToken() {
        while (!_eof) {
            if (isspace(_currentChar)) {
                skipWhitespace();
                continue;
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

class NodeVisitor {
public:
    virtual void visit(const VisitorNode& node) = 0;
    virtual void visit(const BinOp& node) = 0;
    virtual void visit(const UnaryOp& node) = 0;
    virtual void visit(const Num& node) = 0;
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
    Num(Token token):_value(token.value()) {
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
        std::cerr << "Expected " << Tokens::typeToString(Tokens::Integer) << " or " << Tokens::typeToString(Tokens::LParen) << " but got " << Tokens::typeToString(token.type());
        error();
        return std::make_unique<AST>();
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

    std::unique_ptr<AST> parse() {
        return std::move(expr());
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

    interpreter_result_t interpret() {
        _result = interpreter_result_t { 0 };
        auto node = _parser.parse();
        node->accept(*this);
        return _result;
    }

private:
    Parser _parser;
    interpreter_result_t _result;
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

    while (true) {
        std::cout << "calc>";

        std::string s;
        std::getline(std::cin, s);

        if (std::cin.fail()) {
            break;
        }

        Lexer lexer(s);
        Parser parser(lexer);
        Interpreter interpreter(parser);
        auto result = interpreter.interpret();

        std::cout << result.value << std::endl;

        ReversePolishNotationTranslator translator1(parser);
        LispTranslator translator2(parser);

        std::cout << "RPN: " << translator1.translate() << std::endl;
        std::cout << "LISP: " << translator2.translate() << std::endl;
    }

    return 0;
}


