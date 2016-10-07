#include <iostream>
#include <string>
#include <sstream>

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

    bool isOperator() const {
        return _type == Tokens::Plus || _type == Tokens::Minus || _type == Tokens::Multiply || _type == Tokens::Divide;
    }

    bool isHighPrecedenceOperator() const {
        return _type == Tokens::Multiply || _type == Tokens::Divide;
    }

    bool isLowPrecendenceOperator() const {
        return _type == Tokens::Plus || _type == Tokens::Minus;
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

class Interpreter {
public:
    Interpreter(Lexer lexer):_lexer(lexer) {
        _currentToken = _lexer.getNextToken();
    }

    void error() {
        throw std::runtime_error("Invalid syntax");
    }

    void eat(Tokens::Type type) {
        if (_currentToken.type() == type) {
            _currentToken = _lexer.getNextToken();
        } else {
            std::cerr << "Error: expected " << Tokens::typeToString
                (type) << " but got " << Tokens::typeToString(_currentToken.type()) << std::endl;
            error();
        }
    }

    int factor() {
        Token token = _currentToken;
        if (token.type() == Tokens::Integer) {
            eat(Tokens::Integer);
            return token.value();
        } else if (token.type() == Tokens::LParen) {
            eat(Tokens::LParen);
            interpreter_result_t result = expr();
            eat(Tokens::RParen);
            return result.value;
        }
        error();
        return 0;
    }

    interpreter_result_t term() {
        interpreter_result_t result;
        result.value = factor();

        while (_currentToken.isHighPrecedenceOperator()) {
            Token token = _currentToken;

            if (token.type() == Tokens::Multiply) {
                eat(Tokens::Multiply);
                result.value *= factor();
            } else if (token.type() == Tokens::Divide) {
                eat(Tokens::Divide);
                result.value /= factor();
            }
        }

        return result;
    }

    interpreter_result_t expr() {
        interpreter_result_t result = term();
        while (_currentToken.isLowPrecendenceOperator()) {
            Token token = _currentToken;

            if (token.type() == Tokens::Plus) {
                eat(Tokens::Plus);
                interpreter_result_t right = term();
                result.value += right.value;
            } else if (token.type() == Tokens::Minus) {
                eat(Tokens::Minus);
                interpreter_result_t right = term();
                result.value -= right.value;
            }
        }

        return result;
    }

private:
    Lexer _lexer;
    Token _currentToken;
};

class AST {
public:
    AST() {}
    virtual ~AST() {}

    virtual std::string description() const {
        return "AST(empty)";
    }
};

class BinOp: public AST {
public:
    BinOp(std::unique_ptr<AST> left, Token op, std::unique_ptr<AST> right):
        _left(std::move(left)),_op(op),_right(std::move(right)) {
    }

    const AST& getLeft() const {
        return *_left;
    }

    const AST& getRight() const {
        return *_right;
    }

    const Token& getOp() const {
        return _op;
    }

    virtual std::string description() const {
        std::stringstream ss;
        ss << "BinOp(" << _left->description() << ", ";
        ss << Tokens::typeToString(_op.type()) << ", ";
        ss << _right->description() << ")";
        return ss.str();
    }

private:
    std::unique_ptr<AST> _left;
    std::unique_ptr<AST> _right;
    Token _op;
};

class Num: public AST {
public:
    Num(Token token):_token(token) {
    }

    const Token& getToken() const {
        return _token;
    }

    virtual std::string description() const {
        std::stringstream ss;
        ss << "Num(" << _token.value() << ")";
        return ss.str();
    }

private:
    Token _token;
};

class Parser {
public:
    Parser(Lexer lexer):_lexer(lexer) {
        _currentToken = _lexer.getNextToken();
    }

    void error() {
        throw std::runtime_error("Invalid syntax");
    }

    void eat(Tokens::Type type) {
        if (_currentToken.type() == type) {
            _currentToken = _lexer.getNextToken();
        } else {
           error();
        }
    }

    std::unique_ptr<AST> factor() {
        Token token = _currentToken;
        if (token.type() == Tokens::Integer) {
            eat(Tokens::Integer);
            return std::make_unique<Num>(token);
        } else if (token.type() == Tokens::LParen) {
            eat(Tokens::LParen);
            auto node = expr();
            eat(Tokens::RParen);
            return node;
        }
        error();
        return std::make_unique<AST>();
    }

    std::unique_ptr<AST> term() {
        auto node = factor();

        while (_currentToken.isHighPrecedenceOperator()) {
            Token token = _currentToken;
            if (token.type() == Tokens::Multiply) {
                eat(Tokens::Multiply);
            } else if (token.type() == Tokens::Divide) {
                eat(Tokens::Divide);
            }

            node = std::make_unique<BinOp>(std::move(node), token, std::move(factor()));
        }

        return node;
    }

    std::unique_ptr<AST> expr() {
        auto node = term();

        while (_currentToken.isLowPrecendenceOperator()) {
            Token token = _currentToken;
            if (token.type() == Tokens::Plus) {
                eat(Tokens::Plus);
            } else if (token.type() == Tokens::Minus) {
                eat(Tokens::Minus);
            }
            node = std::make_unique<BinOp>(std::move(node), token, std::move(term()));
        }

        std::cout << node->description() << std::endl;
        return node;
    }

    std::unique_ptr<AST> parse() {
        return std::move(expr());
    }

private:
    Lexer _lexer;
    Token _currentToken;
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
        //Interpreter interpreter(lexer);
        //interpreter_result_t result = interpreter.expr();
        Parser parser(lexer);
        parser.parse();
        //std::cout << result << std::endl;
    }

    return 0;
}


