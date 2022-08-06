#include "lexer.h"

#include <algorithm>
#include <charconv>
#include <unordered_map>

using namespace std;

namespace parse {

    bool operator==(const Token& lhs, const Token& rhs) {
        using namespace token_type;

        if (lhs.index() != rhs.index()) {
            return false;
        }
        if (lhs.Is<Char>()) {
            return lhs.As<Char>().value == rhs.As<Char>().value;
        }
        if (lhs.Is<Number>()) {
            return lhs.As<Number>().value == rhs.As<Number>().value;
        }
        if (lhs.Is<String>()) {
            return lhs.As<String>().value == rhs.As<String>().value;
        }
        if (lhs.Is<Id>()) {
            return lhs.As<Id>().value == rhs.As<Id>().value;
        }
        return true;
    }

    bool operator!=(const Token& lhs, const Token& rhs) {
        return !(lhs == rhs);
    }

    std::ostream& operator<<(std::ostream& os, const Token& rhs) {
        using namespace token_type;

#define VALUED_OUTPUT(type) \
    if (auto p = rhs.TryAs<type>()) return os << #type << '{' << p->value << '}';

        VALUED_OUTPUT(Number);
        VALUED_OUTPUT(Id);
        VALUED_OUTPUT(String);
        VALUED_OUTPUT(Char);

#undef VALUED_OUTPUT

#define UNVALUED_OUTPUT(type) \
    if (rhs.Is<type>()) return os << #type;

        UNVALUED_OUTPUT(Class);
        UNVALUED_OUTPUT(Return);
        UNVALUED_OUTPUT(If);
        UNVALUED_OUTPUT(Else);
        UNVALUED_OUTPUT(Def);
        UNVALUED_OUTPUT(Newline);
        UNVALUED_OUTPUT(Print);
        UNVALUED_OUTPUT(Indent);
        UNVALUED_OUTPUT(Dedent);
        UNVALUED_OUTPUT(And);
        UNVALUED_OUTPUT(Or);
        UNVALUED_OUTPUT(Not);
        UNVALUED_OUTPUT(Eq);
        UNVALUED_OUTPUT(NotEq);
        UNVALUED_OUTPUT(LessOrEq);
        UNVALUED_OUTPUT(GreaterOrEq);
        UNVALUED_OUTPUT(None);
        UNVALUED_OUTPUT(True);
        UNVALUED_OUTPUT(False);
        UNVALUED_OUTPUT(Eof);

#undef UNVALUED_OUTPUT

        return os << "Unknown token :("sv;
    }

    Lexer::Lexer(std::istream& in) {
        using namespace parse;
        using namespace token_type;

        std::string inp_line;
        while (getline(in, inp_line)) {

            if (EmptyLine(inp_line)) {
                continue;
            }

            SetIndentLevel(CheckAndCutLine(inp_line));
            std::istringstream istring(inp_line);
            ReadLine(istring);
        }

        SetIndentLevel(0);

        line_.push_back(Eof{});
    }

    const Token& Lexer::CurrentToken() const {
        if (head_ < line_.size()) {
            return line_[head_];
        }
        throw std::logic_error("Not implemented"s);
    }

    Token Lexer::NextToken() {
        if ((head_ + 1) < line_.size()) {
            head_++;
        }
        return CurrentToken();

        
    }
    size_t Lexer::CheckAndCutLine(std::string& in) const {
        size_t result = in.find_first_not_of(' ', 0);
        if (result >= 2) {
            result /= 2;
        }
        in = in.substr(result);
        return result;
    }

    void Lexer::SetIndentLevel(const size_t new_level) {
        using namespace parse::token_type;
        for (size_t i = indent_level_; i < new_level; ++i) {
            line_.push_back(Indent({}));
        }
        for (size_t i = indent_level_; i > new_level; --i) {
            line_.push_back(Dedent({}));
        }
        if (indent_level_ != new_level) {
            indent_level_ = new_level;
        }
    }

    void Lexer::ReadLine(std::istringstream& istring) {
        using namespace parse::token_type;
        char t;
        bool new_line = false;
        while (istring.get(t)) {
            if (t == ' ') {
                continue;
            }
            if (t == '#') {
                istring.ignore(numeric_limits<streamsize>::max(), '\n');
                line_.push_back(Newline{});
                return;
            }

            new_line = true;
            if (isdigit(t)) {
                ReadNumber(istring);
            }
            else if (isprint(t) || isspace(t)) {
                switch (t) {
                case '=': case '!': case '<': case'>': {
                    if (istring.peek() == '=') {
                        if (t == '=') {
                            line_.push_back(Eq({}));
                        }
                        else if (t == '!') {
                            line_.push_back(NotEq({}));
                        }
                        else if (t == '<') {
                            line_.push_back(LessOrEq({}));
                        }
                        else {
                            line_.push_back(GreaterOrEq({}));
                        }
                        istring.get();
                        break;
                    }
                    [[fallthrough]];
                }

                case '*': case '/': case '+': case '-': case '(': case ')':
                case ',': case '.': case ':': case ';': case '\t': case '\n': {

                    line_.push_back(Char{ t });
                    break;
                }
                case '\'': case '\"': {
                    ReadString(istring, t);
                    break;
                }
                default: {
                    istring.unget();
                    ReadId(istring);
                }
                }
            }
        }
        if (new_line) {
            line_.push_back(Newline{});
        }
    }

    void Lexer::ReadId(std::istringstream& istring) {
        using namespace parse::token_type;
        std::string s;
        char c;
        while (istring.get(c)) {
            if ((isspace(c) || c == ' ') || (ispunct(c) && c != '_')) {
                if (ispunct(c) && c != '_') {
                    istring.unget();
                }
                break;
            }
            s += c;
        }
        if (s == "class") {
            line_.push_back(Class({}));
        }
        else if (s == "return") {
            line_.push_back(Return({}));
        }
        else if (s == "if") {
            line_.push_back(If({}));
        }
        else if (s == "else") {
            line_.push_back(Else({}));
        }
        else if (s == "def") {
            line_.push_back(Def({}));
        }
        else if (s == "print") {
            line_.push_back(Print({}));
        }
        else if (s == "or") {
            line_.push_back(Or({}));
        }
        else if (s == "None") {
            line_.push_back(None({}));
        }
        else if (s == "and") {
            line_.push_back(And({}));
        }
        else if (s == "not") {
            line_.push_back(Not({}));
        }
        else if (s == "True") {
            line_.push_back(True({}));
        }
        else if (s == "False") {
            line_.push_back(False({}));
        }
        else {
            line_.push_back(Id({ s }));
        }
    }

    void Lexer::ReadString(std::istringstream& in, const char d) {
        std::string str;
        char ch;
        while (in.get(ch)) {
            if (ch == d) {
                break;
            }
            if (ch == '\\') {
                in.get(ch);
                switch (ch) {
                case 'n':
                    ch = '\n';
                    break;
                case 't':
                    ch = '\t';
                }
                str += ch;
            }
            else {
                str += ch;
            }

        }
        line_.push_back(parse::token_type::String{ str });
    }

    void Lexer::ReadNumber(std::istringstream& in) {
        in.unget();
        int d;
        in >> d;
        line_.push_back(parse::token_type::Number{ d });
    }

    bool Lexer::EmptyLine(const std::string_view line) const {
        if (line.size() == 0) {
            return true;
        }
        size_t p = line.find_first_not_of(' ');
        if(line[p] == '#' || (p > 0 && p == (line.size() - 1))){
            return true;
        }
        return false;
    }

    int Lexer::IndentLevelOfLine(const std::string_view line)const {
        size_t result = line.find_first_not_of(' ', 0);
        if (result >= 2) {
            result /= 2;
        }
        return result;
    }
}  // namespace parse