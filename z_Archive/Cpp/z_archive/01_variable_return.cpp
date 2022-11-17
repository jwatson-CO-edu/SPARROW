#include <stdint.h> // uint
#include <variant>

int main(){

    std::variant<int, float, char> var { 42.0f };

    if (std::holds_alternative<int>(var)) {
        auto int_res = std::get<int>(var); // int&
        // ...
    } else if (std::holds_alternative<float>(var)) {
        auto float_res = std::get<float>(var); // float&
        // ...
    } else {
        auto char_res = std::get<char>(var); // char&
        // ...
    }

    return 0;
}