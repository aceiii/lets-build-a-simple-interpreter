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

class Interpreter {
public:
    Interpreter(const std::string& text):_text(text),_pos(0)
    {
        _currentChar = text[_pos];
        _eof = false;
    }

    Token error() {
        throw std::runtime_error("Error parsing input");
        return Token();
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
        std::stringstream ss;
        while (!_eof && isdigit(_currentChar)) {
            ss << _currentChar;
            advance();
        }

        std::stringstream s2(ss.str());

        int result;
        s2 >> result;

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

            return error();
        }

        return Token(Tokens::EndOfFile);
    }

    void eat(Tokens::Type type) {
        if (_currentToken.type() == type) {
            _currentToken = getNextToken();
        } else {
            std::cerr << "Error: expected " << Tokens::typeToString
                (type) << " but got " << Tokens::typeToString(_currentToken.type()) << std::endl;
            error();
        }
    }

    int term() {
        Token token = _currentToken;
        eat(Tokens::Integer);
        return token.value();
    }

    interpreter_result_t expr() {
        _currentToken = getNextToken();

        interpreter_result_t result;
        result.value = term();

        while (_currentToken.isOperator()) {
            Token token = _currentToken;

            if (token.type() == Tokens::Plus) {
                eat(Tokens::Plus);
                result.value += term();
            } else if (token.type() == Tokens::Minus) {
                eat(Tokens::Minus);
                result.value -= term();
            } else if (token.type() == Tokens::Multiply) {
                eat(Tokens::Multiply);
                result.value *= term();
            } else if (token.type() == Tokens::Divide) {
                eat(Tokens::Divide);
                result.value /= term();
            }
        }

        return result;
    }

private:
    std::string _text;
    int _pos;
    Token _currentToken;
    char _currentChar;
    bool _eof;
};

int main(int argc, char** argv) {

    while (true) {
        std::cout << "calc>";

        std::string s;
        std::getline(std::cin, s);

        if (std::cin.fail()) {
            break;
        }

        Interpreter interpreter(s);
        interpreter_result_t result = interpreter.expr();
        std::cout << result << std::endl;
    }

    return 0;
}


