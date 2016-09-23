#include <iostream>
#include <string>
#include <sstream>


struct Tokens {
    enum Type {
        Integer,
        Plus,
        EndOfFile,
    };

    static std::string typeToString(Type t) {
        switch (t) {
        case Integer:
            return "INTEGER";
        case Plus:
            return "PLUS";
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
    Token(Tokens::Type type, char c):_type(type),_value(0) {
        std::stringstream ss;
        ss << c;

        std::stringstream ss2(ss.str());
        ss2 >> _value;
    }
    Token(Tokens::Type type, const std::string& value):_type(type),_value(0) {
        std::stringstream ss(value);
        ss >> _value;
    }

    std::string getDescription() const {
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
    }

    Token error() {
        throw std::runtime_error("Error parsing input");
        return Token();
    }

    Token getNextToken() {
        std::string text = _text;

        if (_pos > text.size() - 1) {
            return Token(Tokens::EndOfFile, 0);
        }

        char currentChar = text[_pos];

        if (isdigit(currentChar)) {
            _pos += 1;
            return Token(Tokens::Integer, currentChar);
        }

        if (currentChar == '+') {
            _pos += 1;
            return Token(Tokens::Plus);
        }

        return error();
    }

    void eat(Tokens::Type type) {
        if (_currentToken.type() == type) {
            _currentToken = getNextToken();
        } else {
            error();
        }
    }

    interpreter_result_t expr() {
        _currentToken = getNextToken();

        Token left = _currentToken;
        eat(Tokens::Integer);

        Token op = _currentToken;
        eat(Tokens::Plus);

        Token right = _currentToken;
        eat(Tokens::Integer);

        interpreter_result_t result;
        result.value = left.value() + right.value();
        return result;
    }

private:
    std::string _text;
    int _pos;
    Token _currentToken;
};

int main(int argc, char** argv) {

    while (true) {
        std::cout << "calc>";

        std::string s;
        std::cin >> s;

        if (std::cin.fail()) {
            break;
        }

        Interpreter interpreter(s);
        interpreter_result_t result = interpreter.expr();
        std::cout << result << std::endl;
    }

    return 0;
}


